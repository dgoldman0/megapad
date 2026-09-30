// ============================================================================
// mp64_fma.v — multi-format fused multiply-add unit
// ============================================================================
//
// docs/floating-point.md defines every result; decision D7 of
// docs/megapad-full-float-plan.md defines the unit.  One unit computes one
// binary64 FMA, or two binary32 FMAs in split mode, per beat.  Each result is
// the exact a * b + c rounded once in the output format, with full subnormal
// support, IEEE signed zeros, and canonical NaNs.  The tile engine rounds to
// nearest-even and forms ADD as a * 1 + b, SUB as a * 1 + (-b), and MUL and
// WMUL as a * b + (-0), so every element-wise arithmetic result has one
// rounding.  The scalar FPU (mp64_fpu.v) uses the same lane with its own
// rounding mode and the exception flags.
//
// The unit holds two lanes.  Lane 0 has 53-bit significands and serves the
// binary64 operation or the first binary32 operation; lane 1 has 24-bit
// significands and serves the second binary32 operation.  Partitioning one
// multiplier array between the lanes is an area refinement that does not
// change any result.
//
// This file contains only bounded, combinational integer logic.  It does not
// use real/shortreal or simulator floating-point behavior.

// ============================================================================
// One FMA lane over exact operand descriptors.
// ============================================================================
//
// A finite operand is (-1)**sign * sig * 2**exp with sig an unsigned integer
// that need not be normalized.  NaN, infinity, and zero are carried as class
// bits.  The exact product has 2*SW bits.  Both addends are normalized so
// their leading bit sits at bit W-1 of a W = max(2*SW + 3, 56) bit field,
// the smaller is aligned with a right shift that ORs discarded bits into
// bit 0, and one add or subtract gives the sum.  When the alignment keeps
// every bit the sum is exact.  Otherwise the normalized larger term has zero
// low bits, so the computed sum is odd and the exact sum lies strictly
// between its even neighbours; the alignment is at least four places, so the
// cancellation is at most one bit and the round bit of a 53-bit result sits
// at least one place above bit 0.  The rounding decision is then the exact
// one.  The sum is rounded once at the quantum of the output format.

module mp64_fma_core #(
    parameter SW = 53
) (
    input  wire               a_nan,
    input  wire               a_inf,
    input  wire               a_zero,
    input  wire               a_sign,
    input  wire [SW-1:0]      a_sig,
    input  wire signed [13:0] a_exp,

    input  wire               b_nan,
    input  wire               b_inf,
    input  wire               b_zero,
    input  wire               b_sign,
    input  wire [SW-1:0]      b_sig,
    input  wire signed [13:0] b_exp,

    input  wire               c_nan,
    input  wire               c_inf,
    input  wire               c_zero,
    input  wire               c_sign,
    input  wire [SW-1:0]      c_sig,
    input  wire signed [13:0] c_exp,

    input  wire               out64,   // round to binary64, else binary32
    input  wire [2:0]         rm,      // 0 RNE, 1 RTZ, 2 RDN, 3 RUP, 4 RMM
    output wire [63:0]        result,  // binary32 results use bits [31:0]
    output wire [4:0]         flags    // {NV, DZ, OF, UF, NX}
);
    localparam PW = 2 * SW;
    localparam W  = (PW + 3 > 56) ? PW + 3 : 56;
    localparam RDN = 3'd2;

    function integer msb_index;
        input [W:0] value;
        integer k;
        begin
            msb_index = -1;
            for (k = 0; k <= W; k = k + 1)
                if (value[k])
                    msb_index = k;
        end
    endfunction

    // Shift right, OR-ing every discarded bit into bit 0.
    function [W-1:0] shift_right_jam;
        input [W-1:0] value;
        input integer amount;
        reg   [W-1:0] mask;
        begin
            if (amount <= 0) begin
                shift_right_jam = value;
            end else if (amount >= W) begin
                shift_right_jam = {{(W-1){1'b0}}, |value};
            end else begin
                mask = ~({W{1'b1}} << amount);
                shift_right_jam = (value >> amount) |
                                  {{(W-1){1'b0}}, |(value & mask)};
            end
        end
    endfunction

    reg [63:0]          special;     // NaN, infinity, and zero results
    reg                 use_special;
    reg                 invalid;
    reg                 sum_sign;
    reg [W:0]           sum;
    reg signed [15:0]   sum_lsb_exp;

    wire [63:0] rounded;
    wire [4:0]  round_flags;

    mp64_fp_round #(.MW(W + 1)) u_round (
        .sign     (sum_sign),
        .mag      (sum),
        .lsb_exp  (sum_lsb_exp),
        .sticky_in(1'b0),
        .fmt      (out64 ? 2'd3 : 2'd2),
        .rm       (rm),
        .result   (rounded),
        .flags    (round_flags)
    );

    assign result = use_special ? special : rounded;
    assign flags  = use_special ? {invalid, 4'd0} : round_flags;

    always @(*) begin : fma
        integer prod_lead;
        integer c_lead;
        integer prod_top;
        integer c_top;
        integer large_top;
        integer small_top;
        reg [PW-1:0] prod;
        reg          prod_sign;
        reg          prod_zero;
        reg          prod_inf;
        reg          addend_zero;
        reg          subtract;
        reg          large_is_prod;
        reg [W-1:0]  prod_norm;
        reg [W-1:0]  c_norm;
        reg [W-1:0]  major;
        reg [W-1:0]  minor;
        reg [W-1:0]  aligned;
        reg [63:0]   quiet_nan;
        reg [63:0]   infinity;
        reg [63:0]   sign_bit;

        quiet_nan = out64 ? 64'h7FF8_0000_0000_0000 : 64'h0000_0000_7FC0_0000;
        infinity  = out64 ? 64'h7FF0_0000_0000_0000 : 64'h0000_0000_7F80_0000;
        sign_bit  = out64 ? 64'h8000_0000_0000_0000 : 64'h0000_0000_8000_0000;

        prod        = a_sig * b_sig;
        prod_sign   = a_sign ^ b_sign;
        prod_inf    = a_inf || b_inf;
        prod_zero   = a_zero || b_zero || (prod == {PW{1'b0}});
        addend_zero = c_zero || (c_sig == {SW{1'b0}});
        // 0 * inf is invalid even beside a NaN addend; inf - inf only when
        // no operand is a NaN.
        invalid = (a_inf && b_zero) || (a_zero && b_inf) ||
                  (!a_nan && !b_nan && !c_nan &&
                   prod_inf && c_inf && (prod_sign != c_sign));

        prod_lead = -1;
        c_lead    = -1;
        prod_top  = 0;
        c_top     = 0;
        large_top = 0;
        small_top = 0;
        subtract  = 1'b0;
        large_is_prod = 1'b0;
        prod_norm = {W{1'b0}};
        c_norm    = {W{1'b0}};
        major     = {W{1'b0}};
        minor     = {W{1'b0}};
        aligned   = {W{1'b0}};
        sum       = {(W+1){1'b0}};
        sum_sign  = 1'b0;
        sum_lsb_exp = 16'sd0;
        special   = 64'd0;
        use_special = 1'b1;

        if (a_nan || b_nan || c_nan || invalid) begin
            special = quiet_nan;
        end else if (prod_inf) begin
            special = infinity | (prod_sign ? sign_bit : 64'd0);
        end else if (c_inf) begin
            special = infinity | (c_sign ? sign_bit : 64'd0);
        end else if (prod_zero && addend_zero) begin
            // An exact zero sum of zeros is -0 when both are -0, or under
            // round-down when either is.
            special = (rm == RDN ? (prod_sign || c_sign)
                                 : (prod_sign && c_sign)) ? sign_bit : 64'd0;
        end else begin
            if (!prod_zero) begin
                prod_lead = msb_index({{(W+1-PW){1'b0}}, prod});
                prod_top  = a_exp + b_exp + prod_lead;
                prod_norm = {{(W-PW){1'b0}}, prod} << (W - 1 - prod_lead);
            end
            if (!addend_zero) begin
                c_lead = msb_index({{(W+1-SW){1'b0}}, c_sig});
                c_top  = c_exp + c_lead;
                c_norm = {{(W-SW){1'b0}}, c_sig} << (W - 1 - c_lead);
            end

            if (addend_zero)
                large_is_prod = 1'b1;
            else if (prod_zero)
                large_is_prod = 1'b0;
            else if (prod_top != c_top)
                large_is_prod = prod_top > c_top;
            else
                large_is_prod = prod_norm >= c_norm;

            if (large_is_prod) begin
                major     = prod_norm;
                large_top = prod_top;
                sum_sign  = prod_sign;
                minor     = c_norm;
                small_top = c_top;
            end else begin
                major     = c_norm;
                large_top = c_top;
                sum_sign  = c_sign;
                minor     = prod_norm;
                small_top = prod_top;
            end

            if (!prod_zero && !addend_zero) begin
                aligned  = shift_right_jam(minor, large_top - small_top);
                subtract = prod_sign != c_sign;
            end
            sum = subtract ? ({1'b0, major} - {1'b0, aligned})
                           : ({1'b0, major} + {1'b0, aligned});

            if (sum == {(W+1){1'b0}}) begin
                // Exact cancellation of nonzero terms is +0, or -0 under
                // round-down.
                special = rm == RDN ? sign_bit : 64'd0;
            end else begin
                // Bit 0 of sum has weight 2**(large_top - (W - 1)).
                use_special = 1'b0;
                sum_lsb_exp = large_top - (W - 1);
            end
        end
    end
endmodule

// ============================================================================
// The two-lane unit over raw encodings.
// ============================================================================
//
// in64 selects binary64 operands for lane 0; otherwise lanes 0 and 1 take
// binary32 operands in bits [31:0].  out64 selects binary64 results for both
// lanes and a binary64 addend for lane 0.  Lane 1's addend is always binary32,
// which is exact in binary64, so split-mode WMUL (binary32 operands, binary64
// products) passes -0 there.

module mp64_fma_unit (
    input  wire        in64,
    input  wire        out64,
    input  wire [63:0] a0,
    input  wire [63:0] b0,
    input  wire [63:0] c0,
    input  wire [31:0] a1,
    input  wire [31:0] b1,
    input  wire [31:0] c1,
    output wire [63:0] r0,
    output wire [63:0] r1
);
    // {nan, inf, zero, sign, exp[13:0], sig[52:0]}
    function [70:0] decode64;
        input [63:0] bits;
        reg   [10:0] field;
        reg   [51:0] fraction;
        begin
            field    = bits[62:52];
            fraction = bits[51:0];
            if (field == 11'h7FF)
                decode64 = {fraction != 52'd0, fraction == 52'd0, 1'b0,
                            bits[63], 14'd0, 53'd0};
            else if (field == 11'd0)
                decode64 = {2'b00, fraction == 52'd0, bits[63],
                            -14'sd1074, {1'b0, fraction}};
            else
                decode64 = {3'b000, bits[63],
                            $signed({3'b000, field}) - 14'sd1075,
                            {1'b1, fraction}};
        end
    endfunction

    function [70:0] decode32;
        input [31:0] bits;
        reg   [7:0]  field;
        reg   [22:0] fraction;
        begin
            field    = bits[30:23];
            fraction = bits[22:0];
            if (field == 8'hFF)
                decode32 = {fraction != 23'd0, fraction == 23'd0, 1'b0,
                            bits[31], 14'd0, 53'd0};
            else if (field == 8'd0)
                decode32 = {2'b00, fraction == 23'd0, bits[31],
                            -14'sd149, 29'd0, 1'b0, fraction};
            else
                decode32 = {3'b000, bits[31],
                            $signed({6'b000000, field}) - 14'sd150,
                            29'd0, 1'b1, fraction};
        end
    endfunction

    wire [70:0] da0 = in64 ? decode64(a0) : decode32(a0[31:0]);
    wire [70:0] db0 = in64 ? decode64(b0) : decode32(b0[31:0]);
    wire [70:0] dc0 = out64 ? decode64(c0) : decode32(c0[31:0]);
    wire [70:0] da1 = decode32(a1);
    wire [70:0] db1 = decode32(b1);
    wire [70:0] dc1 = decode32(c1);

    mp64_fma_core #(.SW(53)) u_lane0 (
        .a_nan (da0[70]), .a_inf (da0[69]), .a_zero(da0[68]),
        .a_sign(da0[67]), .a_exp (da0[66:53]), .a_sig (da0[52:0]),
        .b_nan (db0[70]), .b_inf (db0[69]), .b_zero(db0[68]),
        .b_sign(db0[67]), .b_exp (db0[66:53]), .b_sig (db0[52:0]),
        .c_nan (dc0[70]), .c_inf (dc0[69]), .c_zero(dc0[68]),
        .c_sign(dc0[67]), .c_exp (dc0[66:53]), .c_sig (dc0[52:0]),
        .out64 (out64),
        .rm    (3'd0),
        .result(r0),
        .flags ()
    );

    mp64_fma_core #(.SW(24)) u_lane1 (
        .a_nan (da1[70]), .a_inf (da1[69]), .a_zero(da1[68]),
        .a_sign(da1[67]), .a_exp (da1[66:53]), .a_sig (da1[23:0]),
        .b_nan (db1[70]), .b_inf (db1[69]), .b_zero(db1[68]),
        .b_sign(db1[67]), .b_exp (db1[66:53]), .b_sig (db1[23:0]),
        .c_nan (dc1[70]), .c_inf (dc1[69]), .c_zero(dc1[68]),
        .c_sign(dc1[67]), .c_exp (dc1[66:53]), .c_sig (dc1[23:0]),
        .out64 (out64),
        .rm    (3'd0),
        .result(r1),
        .flags ()
    );
endmodule
