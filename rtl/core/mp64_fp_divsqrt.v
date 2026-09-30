// ============================================================================
// mp64_fp_divsqrt.v — correctly rounded divide and square root
// ============================================================================
//
// docs/floating-point.md §6.1, §6.2, and §8.3 define the results.  One unit
// computes one binary16, bfloat16, binary32, or binary64 quotient or square
// root with a restoring digit recurrence that retires two bits per cycle.
// It produces at least p + 2 result bits (14 for both 16-bit formats, 26 for
// binary32, 56 for binary64), which leaves a guard bit and a sticky
// remainder, so mp64_fp_round rounds the result once and exactly in any mode.
//
// `start` samples the raw operands (a / b, or sqrt(b)) and prepares them in
// the same cycle.  The recurrence then runs for bits/2 cycles, after which
// `ready` rises and `result` and `flags` hold the rounded answer until the
// next start.  The number of cycles never depends on the operands; NaN, zero,
// infinity, and divide-by-zero results come from the special-case logic
// while the recurrence runs on don't-care values.
//
// flags is {NV, DZ, OF, UF, NX}.  All logic is bounded integer arithmetic.
// ============================================================================

module mp64_fp_divsqrt (
    input  wire        clk,
    input  wire        rst,
    input  wire        start,
    input  wire        sqrt,       // 0 divide, 1 square root
    input  wire [1:0]  fmt,        // 0 binary16, 1 bfloat16, 2 binary32, 3 binary64
    input  wire [2:0]  rm,
    input  wire [63:0] a,          // dividend (ignored for sqrt)
    input  wire [63:0] b,          // divisor or radicand
    output reg         ready,
    output wire [63:0] result,
    output wire [4:0]  flags
);
    localparam FMT_H = 2'd0;
    localparam FMT_B = 2'd1;
    localparam FMT_S = 2'd2;

    // Result bits per format: even, and at least p + 2.  Both 16-bit
    // formats use 14 so they share one tile cost.
    function [5:0] result_bits;
        input [1:0] format;
        begin
            case (format)
                FMT_H, FMT_B: result_bits = 6'd14;
                FMT_S:        result_bits = 6'd26;
                default:      result_bits = 6'd56;
            endcase
        end
    endfunction

    // {nan, snan, inf, zero, sign, exp[15:0], sig[52:0]} with sig normalized
    // so a finite nonzero value has its leading one at bit 52.
    function [73:0] decode;
        input [1:0]  format;
        input [63:0] bits;
        reg          negative;
        reg   [10:0] field;
        reg   [51:0] fraction;
        reg   [52:0] sig;
        integer      fraction_bits;
        integer      field_max;
        integer      bias;
        integer      exponent;
        integer      lead;
        integer      i;
        begin
            case (format)
                FMT_H: begin
                    negative = bits[15]; field = {6'd0, bits[14:10]};
                    fraction = {42'd0, bits[9:0]};
                    fraction_bits = 10; field_max = 31; bias = 15;
                end
                FMT_B: begin
                    negative = bits[15]; field = {3'd0, bits[14:7]};
                    fraction = {45'd0, bits[6:0]};
                    fraction_bits = 7; field_max = 255; bias = 127;
                end
                FMT_S: begin
                    negative = bits[31]; field = {3'd0, bits[30:23]};
                    fraction = {29'd0, bits[22:0]};
                    fraction_bits = 23; field_max = 255; bias = 127;
                end
                default: begin
                    negative = bits[63]; field = bits[62:52];
                    fraction = bits[51:0];
                    fraction_bits = 52; field_max = 2047; bias = 1023;
                end
            endcase
            if (field == field_max) begin
                decode = {fraction != 52'd0,
                          fraction != 52'd0 && !fraction[fraction_bits - 1],
                          fraction == 52'd0, 1'b0, negative, 16'd0, 53'd0};
            end else if (field == 11'd0 && fraction == 52'd0) begin
                decode = {4'b0001, negative, 16'd0, 53'd0};
            end else begin
                if (field == 11'd0) begin
                    sig = {1'b0, fraction};
                    exponent = 1 - bias - fraction_bits;
                end else begin
                    sig = {1'b0, fraction} | (53'd1 << fraction_bits);
                    exponent = field - bias - fraction_bits;
                end
                lead = 0;
                for (i = 0; i < 53; i = i + 1)
                    if (sig[i])
                        lead = i;
                exponent = exponent - (52 - lead);
                decode = {4'b0000, negative, exponent[15:0], sig << (52 - lead)};
            end
        end
    endfunction

    `define DS_NAN(d)  d[73]
    `define DS_SNAN(d) d[72]
    `define DS_INF(d)  d[71]
    `define DS_ZERO(d) d[70]
    `define DS_SIGN(d) d[69]
    `define DS_EXP(d)  $signed(d[68:53])
    `define DS_SIG(d)  d[52:0]

    // ------------------------------------------------------------------
    // Operation state, sampled at start
    // ------------------------------------------------------------------
    reg        op_sqrt;
    reg [1:0]  op_fmt;
    reg [2:0]  op_rm;
    reg [73:0] da;
    reg [73:0] db;
    reg [5:0]  iterations;

    reg [55:0]  quotient;       // quotient or root bits
    reg [55:0]  partial;        // division remainder, doubled after each bit
    reg [52:0]  divisor;
    reg [111:0] radicand;       // square-root radicand, consumed from the top
    reg [57:0]  root_rem;
    reg signed [15:0] lsb_exp;

    function [56:0] divide_step;
        input [55:0] r;
        input [52:0] d;
        reg          take;
        reg   [55:0] rest;
        begin
            take = r >= {3'd0, d};
            rest = take ? r - {3'd0, d} : r;
            divide_step = {take, rest[54:0], 1'b0};
        end
    endfunction

    function [225:0] root_step;
        input [55:0]  root;
        input [57:0]  rem;
        input [111:0] rad;
        reg   [57:0]  widened;
        reg   [57:0]  trial;
        begin
            widened = {rem[55:0], rad[111:110]};
            trial   = {root, 2'b01};
            if (widened >= trial)
                root_step = {root[54:0], 1'b1, widened - trial, rad[109:0], 2'b00};
            else
                root_step = {root[54:0], 1'b0, widened, rad[109:0], 2'b00};
        end
    endfunction

    always @(posedge clk) begin : recurrence
        reg [73:0]  na;
        reg [73:0]  nb;
        reg [5:0]   bits;
        reg [56:0]  division;
        reg [225:0] rooting;
        reg [55:0]  r;
        reg [55:0]  q;
        reg [55:0]  root;
        reg [57:0]  rem;
        reg [111:0] rad;
        integer     step;
        integer     exponent;
        integer     shift;

        if (rst) begin
            ready      <= 1'b0;
            iterations <= 6'd0;
        end else if (start) begin
            na   = decode(fmt, a);
            nb   = decode(fmt, b);
            bits = result_bits(fmt);
            op_sqrt    <= sqrt;
            op_fmt     <= fmt;
            op_rm      <= rm;
            da         <= na;
            db         <= nb;
            ready      <= 1'b0;
            iterations <= bits >> 1;
            quotient   <= 56'd0;
            if (!sqrt) begin
                partial  <= {3'd0, `DS_SIG(na)};
                divisor  <= `DS_SIG(nb);
                lsb_exp  <= `DS_EXP(na) - `DS_EXP(nb) - (bits - 1);
            end else begin
                // Radicand = sig * 2**shift with an even exponent, placed
                // at the top so the root has exactly `bits` bits.
                exponent = `DS_EXP(nb);
                shift = 2 * bits - 54;
                if ((exponent - shift) % 2 != 0)
                    shift = shift + 1;
                root_rem <= 58'd0;
                lsb_exp  <= (exponent - shift) / 2;
                radicand <= {59'd0, `DS_SIG(nb)} << (shift + 112 - 2 * bits);
            end
        end else if (iterations != 6'd0) begin
            iterations <= iterations - 6'd1;
            if (iterations == 6'd1)
                ready <= 1'b1;
            if (!op_sqrt) begin
                r = partial;
                q = quotient;
                for (step = 0; step < 2; step = step + 1) begin
                    division = divide_step(r, divisor);
                    q = {q[54:0], division[56]};
                    r = division[55:0];
                end
                partial  <= r;
                quotient <= q;
            end else begin
                root = quotient;
                rem  = root_rem;
                rad  = radicand;
                for (step = 0; step < 2; step = step + 1) begin
                    rooting = root_step(root, rem, rad);
                    root = rooting[225:170];
                    rem  = rooting[169:112];
                    rad  = rooting[111:0];
                end
                quotient <= root;
                root_rem <= rem;
                radicand <= rad;
            end
        end
    end

    // ------------------------------------------------------------------
    // Rounding and special cases
    // ------------------------------------------------------------------
    wire [63:0] rounded;
    wire [4:0]  round_flags;

    mp64_fp_round #(.MW(56)) u_round (
        .sign     (op_sqrt ? 1'b0 : `DS_SIGN(da) ^ `DS_SIGN(db)),
        .mag      (quotient),
        .lsb_exp  (lsb_exp),
        .sticky_in(op_sqrt ? root_rem != 58'd0 : partial != 56'd0),
        .fmt      (op_fmt),
        .rm       (op_rm),
        .result   (rounded),
        .flags    (round_flags)
    );

    reg [63:0] special;
    reg [4:0]  special_flags;
    reg        use_special;

    function [63:0] pack_special;
        input [1:0] format;
        input       negative;
        input [1:0] kind;       // 0 zero, 1 infinity, 2 canonical NaN
        begin
            case (format)
                FMT_H:   pack_special = kind == 2'd2 ? 64'h7E00 :
                             {48'd0, negative, kind == 2'd1 ? 15'h7C00 : 15'd0};
                FMT_B:   pack_special = kind == 2'd2 ? 64'h7FC0 :
                             {48'd0, negative, kind == 2'd1 ? 15'h7F80 : 15'd0};
                FMT_S:   pack_special = kind == 2'd2 ? 64'h7FC0_0000 :
                             {32'd0, negative, kind == 2'd1 ? 31'h7F80_0000 : 31'd0};
                default: pack_special = kind == 2'd2 ? 64'h7FF8_0000_0000_0000 :
                             {negative, kind == 2'd1 ? 63'h7FF0_0000_0000_0000 : 63'd0};
            endcase
        end
    endfunction

    always @(*) begin
        use_special   = 1'b1;
        special       = 64'd0;
        special_flags = 5'd0;
        if (op_sqrt) begin
            if (`DS_NAN(db)) begin
                special = pack_special(op_fmt, 1'b0, 2'd2);
                special_flags[4] = `DS_SNAN(db);
            end else if (`DS_ZERO(db)) begin
                special = pack_special(op_fmt, `DS_SIGN(db), 2'd0);
            end else if (`DS_SIGN(db)) begin
                special = pack_special(op_fmt, 1'b0, 2'd2);
                special_flags[4] = 1'b1;
            end else if (`DS_INF(db)) begin
                special = pack_special(op_fmt, 1'b0, 2'd1);
            end else begin
                use_special = 1'b0;
            end
        end else begin
            if (`DS_NAN(da) || `DS_NAN(db)) begin
                special = pack_special(op_fmt, 1'b0, 2'd2);
                special_flags[4] = `DS_SNAN(da) || `DS_SNAN(db);
            end else if ((`DS_INF(da) && `DS_INF(db)) ||
                         (`DS_ZERO(da) && `DS_ZERO(db))) begin
                special = pack_special(op_fmt, 1'b0, 2'd2);
                special_flags[4] = 1'b1;
            end else if (`DS_INF(da)) begin
                special = pack_special(op_fmt, `DS_SIGN(da) ^ `DS_SIGN(db), 2'd1);
            end else if (`DS_INF(db) || `DS_ZERO(da)) begin
                special = pack_special(op_fmt, `DS_SIGN(da) ^ `DS_SIGN(db), 2'd0);
            end else if (`DS_ZERO(db)) begin
                special = pack_special(op_fmt, `DS_SIGN(da) ^ `DS_SIGN(db), 2'd1);
                special_flags[3] = 1'b1;
            end else begin
                use_special = 1'b0;
            end
        end
    end

    assign result = use_special ? special : rounded;
    assign flags  = use_special ? special_flags : round_flags;

    `undef DS_NAN
    `undef DS_SNAN
    `undef DS_INF
    `undef DS_ZERO
    `undef DS_SIGN
    `undef DS_EXP
    `undef DS_SIG
endmodule
