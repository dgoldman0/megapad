// ============================================================================
// mp64_fp16_alu.v — FP16 / BF16 Arithmetic Unit (Combinational)
// ============================================================================
//
// Shared FP ALU for half-precision (IEEE 754 FP16) and bfloat16 (BF16).
// All operations are single-cycle combinational — targeting Nexys A7 at
// ≤50 MHz where this is feasible for 16-bit floating point.
//
// Format selection via `is_bf16` input:
//
//   FP16:  1 sign + 5 exp + 10 mantissa,  bias = 15,  exp_max = 31
//   BF16:  1 sign + 8 exp +  7 mantissa,  bias = 127, exp_max = 255
//
// Operations provided (active-high one-hot select):
//   fp_add      — addition (a + b)
//   fp_sub      — subtraction (a − b)
//   fp_mul      — multiplication (a × b)
//   fp_min      — NaN-propagating minimum
//   fp_max      — NaN-propagating maximum
//   fp_abs      — absolute value (clear sign bit)
//   fp_cmp_lt   — ordered less-than (for reductions)
//   fp_cmp_gt   — ordered greater-than
//
// Outputs:
//   result  — 16-bit result for add/sub/mul/min/max/abs
//   is_nan  — either input is NaN
//   cmp_out — comparison result (1 if true)
//
// Round-to-nearest-even on all arithmetic outputs.
//

`include "mp64_defs.vh"

module mp64_fp16_alu (
    input  wire        is_bf16,    // 0 = FP16, 1 = BF16
    input  wire [15:0] a,          // operand A (raw bits)
    input  wire [15:0] b,          // operand B (raw bits)
    input  wire        op_add,     // a + b
    input  wire        op_sub,     // a - b
    input  wire        op_mul,     // a * b
    input  wire        op_min,     // NaN-propagating min
    input  wire        op_max,     // NaN-propagating max
    input  wire        op_abs,     // |a|
    input  wire        op_cmp_lt,  // a < b  (ordered)
    input  wire        op_cmp_gt,  // a > b  (ordered)

    output reg  [15:0] result,
    output wire        is_nan_a,
    output wire        is_nan_b,
    output reg         cmp_out
);

    // ========================================================================
    // Field extraction (parameterised by format)
    // ========================================================================
    // FP16: exp_w=5, man_w=10, bias=15
    // BF16: exp_w=8, man_w=7,  bias=127

    wire        sign_a, sign_b;
    reg  [7:0]  exp_a,  exp_b;      // widened to 8b always
    reg  [10:0] man_a,  man_b;      // widened to 11b (incl. implicit 1)
    reg  [7:0]  bias;
    reg  [7:0]  exp_max;            // all-ones exponent
    reg  [3:0]  man_width;          // 10 or 7

    assign sign_a = a[15];
    assign sign_b = b[15];

    always @(*) begin
        if (is_bf16) begin
            bias      = 8'd127;
            exp_max   = 8'd255;
            man_width = 4'd7;
            exp_a     = a[14:7];
            exp_b     = b[14:7];
            man_a     = (exp_a == 0) ? {1'b0, a[6:0], 3'b0}   // subnormal, no implicit 1
                                     : {1'b1, a[6:0], 3'b0};   // normal, pad to 11 bits
            man_b     = (exp_b == 0) ? {1'b0, b[6:0], 3'b0}
                                     : {1'b1, b[6:0], 3'b0};
        end else begin
            bias      = 8'd15;
            exp_max   = 8'd31;
            man_width = 4'd10;
            exp_a     = {3'b0, a[14:10]};
            exp_b     = {3'b0, b[14:10]};
            man_a     = (exp_a == 0) ? {1'b0, a[9:0]}
                                     : {1'b1, a[9:0]};
            man_b     = (exp_b == 0) ? {1'b0, b[9:0]}
                                     : {1'b1, b[9:0]};
        end
    end

    // ========================================================================
    // NaN / Inf detection
    // ========================================================================
    wire a_is_inf, b_is_inf;
    wire a_is_zero, b_is_zero;
    reg  a_man_nonzero, b_man_nonzero;

    always @(*) begin
        if (is_bf16) begin
            a_man_nonzero = |a[6:0];
            b_man_nonzero = |b[6:0];
        end else begin
            a_man_nonzero = |a[9:0];
            b_man_nonzero = |b[9:0];
        end
    end

    assign is_nan_a  = (exp_a == exp_max) && a_man_nonzero;
    assign is_nan_b  = (exp_b == exp_max) && b_man_nonzero;
    assign a_is_inf  = (exp_a == exp_max) && !a_man_nonzero;
    assign b_is_inf  = (exp_b == exp_max) && !b_man_nonzero;
    assign a_is_zero = (exp_a == 0) && !a_man_nonzero;
    assign b_is_zero = (exp_b == 0) && !b_man_nonzero;

    // Canonical quiet NaN
    wire [15:0] qnan_fp16 = 16'h7E00;   // exp=0x1F, frac=0x200
    wire [15:0] qnan_bf16 = 16'h7FC0;   // exp=0xFF, frac=0x40
    wire [15:0] qnan = is_bf16 ? qnan_bf16 : qnan_fp16;

    // Positive / negative infinity
    wire [15:0] pinf_fp16 = 16'h7C00;
    wire [15:0] ninf_fp16 = 16'hFC00;
    wire [15:0] pinf_bf16 = 16'h7F80;
    wire [15:0] ninf_bf16 = 16'hFF80;
    wire [15:0] pinf = is_bf16 ? pinf_bf16 : pinf_fp16;
    wire [15:0] ninf = is_bf16 ? ninf_bf16 : ninf_fp16;

    // ========================================================================
    // Addition / Subtraction
    // ========================================================================
    // 1. Effective operation: add or sub based on signs + requested op
    // 2. Align mantissas (shift smaller by exponent difference)
    // 3. Add or subtract aligned mantissas
    // 4. Normalise and round (RNE)

    reg  [15:0] add_result;

    always @(*) begin : fp_add_sub_block
        reg         eff_sub;
        reg         sa, sb_eff;
        reg  [7:0]  ea, eb;
        reg  [12:0] ma, mb;         // 13 bits: guard+round+sticky capable
        reg  [7:0]  exp_diff;
        reg  [7:0]  res_exp;
        reg         res_sign;
        reg  [13:0] sum;             // 14 bits to capture carry from addition
        reg  [12:0] ma_aligned, mb_aligned;
        reg         swap;
        reg  [3:0]  shift;
        integer     i;
        reg  [7:0]  norm_shift;
        reg         round_bit, sticky_bit, guard_bit;
        reg  [10:0] res_man;

        add_result = 16'd0;

        sa = sign_a;
        sb_eff = op_sub ? ~sign_b : sign_b;  // negate b for subtraction
        eff_sub = (sa != sb_eff);

        ea = exp_a; eb = exp_b;
        // Extend mantissa: implicit bit already in man_a[10], pad with guard/round bits
        ma = {man_a, 2'b00};  // 13 bits
        mb = {man_b, 2'b00};

        // Handle special cases
        if (is_nan_a || is_nan_b) begin
            add_result = qnan;
        end
        else if (a_is_inf && b_is_inf) begin
            if (eff_sub)
                add_result = qnan;    // inf - inf = NaN
            else
                add_result = {sa, a[14:0]};
        end
        else if (a_is_inf)
            add_result = {sa, a[14:0]};
        else if (b_is_inf)
            add_result = {sb_eff, b[14:0]};
        else if (a_is_zero && b_is_zero)
            add_result = {sa & sb_eff, 15'd0};
        else if (a_is_zero)
            add_result = {sb_eff, b[14:0]};
        else if (b_is_zero)
            add_result = {sa, a[14:0]};
        else begin
            // Swap so ea >= eb
            swap = 1'b0;
            if (ea < eb || (ea == eb && ma < mb)) begin
                swap = 1'b1;
            end

            if (swap) begin
                ma_aligned = mb;
                mb_aligned = ma;
                res_exp = eb;
                res_sign = sb_eff;
                exp_diff = eb - ea;
            end else begin
                ma_aligned = ma;
                mb_aligned = mb;
                res_exp = ea;
                res_sign = sa;
                exp_diff = ea - eb;
            end

            // Right-shift smaller mantissa (with sticky)
            sticky_bit = 1'b0;
            for (i = 0; i < 8; i = i + 1) begin
                if (i < exp_diff[3:0] && exp_diff < 8'd13) begin
                    sticky_bit = sticky_bit | mb_aligned[0];
                    mb_aligned = {1'b0, mb_aligned[12:1]};
                end
            end
            if (exp_diff >= 8'd13) begin
                sticky_bit = |mb_aligned;
                mb_aligned = 13'd0;
            end

            // Add or subtract
            if (eff_sub) begin
                sum = {1'b0, ma_aligned} - {1'b0, mb_aligned};
                // Normalise left (find leading 1)
                norm_shift = 8'd0;
                if (sum == 0) begin
                    res_exp = 8'd0;
                    res_sign = 1'b0;
                end else begin
                    // Count leading zeros in sum[12:0]
                    for (i = 12; i >= 0; i = i - 1) begin
                        if (sum[i] == 1'b0 && norm_shift == (8'd12 - i[7:0]))
                            norm_shift = norm_shift + 8'd1;
                    end
                    // Shift and adjust exponent
                    if (norm_shift > 0) begin
                        if (norm_shift >= res_exp) begin
                            // Subnormal result
                            sum = sum << (res_exp - 8'd1);
                            res_exp = 8'd0;
                        end else begin
                            sum = sum << norm_shift;
                            res_exp = res_exp - norm_shift;
                        end
                    end
                end
            end else begin
                sum = {1'b0, ma_aligned} + {1'b0, mb_aligned};
                // Normalise right on overflow (carry into bit 13)
                if (sum[13]) begin
                    sticky_bit = sticky_bit | sum[0];
                    sum = sum >> 1;
                    res_exp = res_exp + 8'd1;
                end
            end

            // Round to nearest even
            // sum[12:2] = 11-bit mantissa (with implicit 1 at bit 12 or 11)
            // sum[1] = round bit, sum[0] = combined guard/sticky
            guard_bit = sum[1];
            round_bit = sum[0] | sticky_bit;

            res_man = sum[12:2];

            if (guard_bit && (round_bit || res_man[0])) begin
                res_man = res_man + 11'd1;
                if (res_man == 11'd0) begin
                    // Mantissa overflow from rounding (all-ones wrap)
                    res_man = {1'b1, 10'd0};
                    res_exp = res_exp + 8'd1;
                end
            end

            // Check for overflow → infinity
            if (res_exp >= exp_max) begin
                add_result = res_sign ? ninf : pinf;
            end else begin
                // Pack result
                if (is_bf16)
                    add_result = {res_sign, res_exp[7:0], res_man[9:3]};
                else
                    add_result = {res_sign, res_exp[4:0], res_man[9:0]};
            end
        end
    end

    // ========================================================================
    // Multiplication
    // ========================================================================
    reg [15:0] mul_result_fp;

    always @(*) begin : fp_mul_block
        reg        res_sign;
        reg [8:0]  res_exp_wide;     // 9 bits for overflow check
        reg [21:0] prod;             // 11 × 11 = 22 bits
        reg [10:0] res_man;
        reg        guard, round_s, sticky;
        integer    i;

        mul_result_fp = 16'd0;
        res_sign = sign_a ^ sign_b;

        if (is_nan_a || is_nan_b)
            mul_result_fp = qnan;
        else if ((a_is_inf && b_is_zero) || (b_is_inf && a_is_zero))
            mul_result_fp = qnan;          // 0 × inf = NaN
        else if (a_is_inf || b_is_inf)
            mul_result_fp = res_sign ? ninf : pinf;
        else if (a_is_zero || b_is_zero)
            mul_result_fp = {res_sign, 15'd0};
        else begin
            // Multiply mantissas (11b × 11b → 22b)
            prod = {11'd0, man_a} * {11'd0, man_b};

            // Result exponent (unbias, add, rebias)
            res_exp_wide = {1'b0, exp_a} + {1'b0, exp_b} - {1'b0, bias};

            // Normalise: product is in [1.0, 4.0) — bit 21 set if >= 2.0
            if (prod[21]) begin
                // Shift right by 1
                sticky = prod[0];
                prod = {1'b0, prod[21:1]};
                res_exp_wide = res_exp_wide + 9'd1;
            end else begin
                sticky = 1'b0;
            end

            // Extract mantissa and round bits from prod[20:0]
            // prod[20] = implicit 1, prod[19:10] = mantissa bits
            // For FP16: 10 mantissa bits needed from prod[19:10]
            // For BF16: 7 mantissa bits from prod[19:13], round at [12]
            if (is_bf16) begin
                res_man = prod[20:10];
                guard   = prod[12];
                round_s = prod[11];
                sticky  = sticky | (|prod[10:0]);
                // Re-extract for BF16's 7-bit mantissa
                res_man = prod[20:10];
            end else begin
                res_man = prod[20:10];
                guard   = prod[9];
                round_s = prod[8];
                sticky  = sticky | (|prod[7:0]);
            end

            // Round to nearest even
            if (guard && (round_s || sticky || res_man[0])) begin
                res_man = res_man + 11'd1;
                if (res_man == 11'h400 && !prod[21]) begin
                    // Overflow from rounding
                    res_exp_wide = res_exp_wide + 9'd1;
                    res_man = {1'b1, 10'd0};
                end
            end

            // Underflow → zero
            if (res_exp_wide[8] || res_exp_wide == 9'd0) begin
                // Subnormal handling (simplified: flush to zero)
                mul_result_fp = {res_sign, 15'd0};
            end
            // Overflow → infinity
            else if (res_exp_wide >= {1'b0, exp_max}) begin
                mul_result_fp = res_sign ? ninf : pinf;
            end else begin
                if (is_bf16)
                    mul_result_fp = {res_sign, res_exp_wide[7:0], res_man[9:3]};
                else
                    mul_result_fp = {res_sign, res_exp_wide[4:0], res_man[9:0]};
            end
        end
    end

    // ========================================================================
    // FP Comparison (ordered, NaN → false)
    // ========================================================================
    reg fp_a_lt_b, fp_a_gt_b;

    always @(*) begin : fp_cmp_block
        reg [15:0] abs_a, abs_b;

        fp_a_lt_b = 1'b0;
        fp_a_gt_b = 1'b0;

        abs_a = {1'b0, a[14:0]};
        abs_b = {1'b0, b[14:0]};

        if (is_nan_a || is_nan_b) begin
            fp_a_lt_b = 1'b0;   // NaN comparisons are unordered
            fp_a_gt_b = 1'b0;
        end
        else if (a_is_zero && b_is_zero) begin
            fp_a_lt_b = 1'b0;   // +0 == -0
            fp_a_gt_b = 1'b0;
        end
        else if (sign_a != sign_b) begin
            // Different signs: negative < positive
            fp_a_lt_b = sign_a;
            fp_a_gt_b = sign_b;
        end
        else if (sign_a) begin
            // Both negative: larger magnitude is smaller value
            fp_a_lt_b = (abs_a > abs_b);
            fp_a_gt_b = (abs_a < abs_b);
        end else begin
            // Both positive
            fp_a_lt_b = (abs_a < abs_b);
            fp_a_gt_b = (abs_a > abs_b);
        end
    end

    // ========================================================================
    // MIN / MAX with NaN propagation
    // ========================================================================
    reg [15:0] min_result, max_result;

    always @(*) begin
        if (is_nan_a || is_nan_b) begin
            min_result = qnan;
            max_result = qnan;
        end else begin
            min_result = fp_a_lt_b ? a : b;
            max_result = fp_a_gt_b ? a : b;
        end
    end

    // ========================================================================
    // ABS — just clear sign bit
    // ========================================================================
    wire [15:0] abs_result = {1'b0, a[14:0]};

    // ========================================================================
    // Output mux
    // ========================================================================
    always @(*) begin
        result  = 16'd0;
        cmp_out = 1'b0;

        if (op_add || op_sub)
            result = add_result;
        else if (op_mul)
            result = mul_result_fp;
        else if (op_min)
            result = min_result;
        else if (op_max)
            result = max_result;
        else if (op_abs)
            result = abs_result;

        if (op_cmp_lt)
            cmp_out = fp_a_lt_b;
        else if (op_cmp_gt)
            cmp_out = fp_a_gt_b;
    end

endmodule

// ============================================================================
// mp64_fp32_adder.v — FP32 Adder (for DOT/DOTACC/TRED accumulation)
// ============================================================================
//
// Simplified FP32 adder for reduction accumulation paths.
// Used to sum FP32 products from FP16 multiplies in DOT/DOTACC/SUM.
// Single-cycle combinational.
//

module mp64_fp32_adder (
    input  wire [31:0] a,
    input  wire [31:0] b,
    output reg  [31:0] result
);

    wire        sign_a = a[31];
    wire        sign_b = b[31];
    wire [7:0]  exp_a  = a[30:23];
    wire [7:0]  exp_b  = b[30:23];
    wire [23:0] man_a  = (exp_a == 0) ? {1'b0, a[22:0]} : {1'b1, a[22:0]};
    wire [23:0] man_b  = (exp_b == 0) ? {1'b0, b[22:0]} : {1'b1, b[22:0]};

    wire a_is_nan = (exp_a == 8'hFF) && |a[22:0];
    wire b_is_nan = (exp_b == 8'hFF) && |b[22:0];
    wire a_is_inf = (exp_a == 8'hFF) && ~|a[22:0];
    wire b_is_inf = (exp_b == 8'hFF) && ~|b[22:0];
    wire a_is_zero = (exp_a == 0) && ~|a[22:0];
    wire b_is_zero = (exp_b == 0) && ~|b[22:0];

    always @(*) begin : fp32_add_block
        reg         eff_sub, swap, res_sign;
        reg  [7:0]  ea, eb, res_exp, exp_diff;
        reg  [25:0] ma, mb;            // 24 + 2 guard/round bits
        reg  [26:0] sum;               // 27 bits to capture carry
        reg         sticky;
        integer     i;
        reg  [23:0] res_man;
        reg  [7:0]  norm_shift;

        result = 32'd0;
        eff_sub = (sign_a != sign_b);

        if (a_is_nan || b_is_nan)
            result = 32'h7FC00000;  // quiet NaN
        else if (a_is_inf && b_is_inf) begin
            if (eff_sub) result = 32'h7FC00000;
            else         result = a;
        end
        else if (a_is_inf) result = a;
        else if (b_is_inf) result = b;
        else if (a_is_zero && b_is_zero) result = {sign_a & sign_b, 31'd0};
        else if (a_is_zero) result = b;
        else if (b_is_zero) result = a;
        else begin
            ea = exp_a; eb = exp_b;
            ma = {man_a, 2'b00};
            mb = {man_b, 2'b00};

            swap = 1'b0;
            if (ea < eb || (ea == eb && ma < mb)) swap = 1'b1;

            if (swap) begin
                res_exp = eb; res_sign = sign_b;
                exp_diff = eb - ea;
                // Swap ma, mb
                sum = ma; ma = mb; mb = sum;
            end else begin
                res_exp = ea; res_sign = sign_a;
                exp_diff = ea - eb;
            end

            sticky = 1'b0;
            for (i = 0; i < 26; i = i + 1) begin
                if (i < exp_diff && exp_diff < 8'd26) begin
                    sticky = sticky | mb[0];
                    mb = {1'b0, mb[25:1]};
                end
            end
            if (exp_diff >= 8'd26) begin
                sticky = |mb;
                mb = 26'd0;
            end

            if (eff_sub) begin
                sum = {1'b0, ma} - {1'b0, mb};
                if (sum == 0) begin
                    result = 32'd0;
                end else begin
                    norm_shift = 8'd0;
                    for (i = 25; i >= 0; i = i - 1) begin
                        if (sum[i] == 1'b0 && norm_shift == (8'd25 - i[7:0]))
                            norm_shift = norm_shift + 8'd1;
                    end
                    if (norm_shift >= res_exp) begin
                        sum = sum << (res_exp - 8'd1);
                        res_exp = 8'd0;
                    end else begin
                        sum = sum << norm_shift;
                        res_exp = res_exp - norm_shift;
                    end
                    res_man = sum[25:2];
                    if (sum[1] && (sum[0] || sticky || res_man[0])) begin
                        res_man = res_man + 24'd1;
                        if (res_man == 24'd0) begin
                            res_man = {1'b1, 23'd0};
                            res_exp = res_exp + 8'd1;
                        end
                    end
                    result = {res_sign, res_exp, res_man[22:0]};
                end
            end else begin
                sum = {1'b0, ma} + {1'b0, mb};
                if (sum[26]) begin
                    sticky = sticky | sum[0];
                    sum = sum >> 1;
                    res_exp = res_exp + 8'd1;
                end
                res_man = sum[25:2];
                if (sum[1] && (sum[0] || sticky || res_man[0])) begin
                    res_man = res_man + 24'd1;
                    if (res_man == 24'd0) begin
                        res_man = {1'b1, 23'd0};
                        res_exp = res_exp + 8'd1;
                    end
                end
                if (res_exp >= 8'hFF)
                    result = {res_sign, 8'hFF, 23'd0};
                else
                    result = {res_sign, res_exp, res_man[22:0]};
            end
        end
    end

endmodule

// ============================================================================
// mp64_fp16_to_fp32.v — FP16/BF16 → FP32 converter (combinational)
// ============================================================================

module mp64_fp16_to_fp32 (
    input  wire        is_bf16,
    input  wire [15:0] fp16_in,
    output reg  [31:0] fp32_out
);
    always @(*) begin
        if (is_bf16) begin
            // BF16 → FP32: just shift left by 16 (identical exponent field)
            fp32_out = {fp16_in, 16'd0};
        end else begin
            // FP16 → FP32: rebias exponent 15 → 127
            reg        sign;
            reg [4:0]  exp5;
            reg [9:0]  man10;
            reg [7:0]  exp8;

            sign  = fp16_in[15];
            exp5  = fp16_in[14:10];
            man10 = fp16_in[9:0];

            if (exp5 == 5'd0 && man10 == 10'd0) begin
                fp32_out = {sign, 31'd0};   // ±zero
            end
            else if (exp5 == 5'd0) begin
                // Subnormal FP16 → normal FP32
                // Find leading 1 in man10 and shift
                exp8 = 8'd112;   // 127 - 15
                fp32_out = {sign, 31'd0};
                begin : subnorm_loop
                    integer s;
                    reg [9:0] m;
                    m = man10;
                    for (s = 0; s < 10; s = s + 1) begin
                        if (m[9] == 1'b0) begin
                            m = {m[8:0], 1'b0};
                            exp8 = exp8 - 8'd1;
                        end
                    end
                    fp32_out = {sign, exp8, m[8:0], 14'd0};
                end
            end
            else if (exp5 == 5'd31) begin
                // Inf / NaN
                fp32_out = {sign, 8'hFF, man10, 13'd0};
            end
            else begin
                // Normal: rebias
                exp8 = {3'd0, exp5} + 8'd112;   // 127 - 15 = 112
                fp32_out = {sign, exp8, man10, 13'd0};
            end
        end
    end
endmodule

// ============================================================================
// mp64_fp32_to_fp16.v — FP32 → FP16/BF16 converter (combinational, RNE)
// ============================================================================

module mp64_fp32_to_fp16 (
    input  wire        is_bf16,
    input  wire [31:0] fp32_in,
    output reg  [15:0] fp16_out
);
    wire        sign  = fp32_in[31];
    wire [7:0]  exp8  = fp32_in[30:23];
    wire [22:0] man23 = fp32_in[22:0];

    always @(*) begin
        if (is_bf16) begin
            // FP32 → BF16: truncate lower 16 bits with RNE
            reg round_bit;
            reg sticky;
            reg [15:0] truncated;

            truncated = fp32_in[31:16];
            round_bit = fp32_in[15];
            sticky    = |fp32_in[14:0];

            if (round_bit && (sticky || truncated[0]))
                fp16_out = truncated + 16'd1;
            else
                fp16_out = truncated;
        end else begin
            // FP32 → FP16: rebias 127 → 15
            reg [4:0]  new_exp;
            reg [9:0]  new_man;
            reg        round_bit, sticky;
            reg signed [8:0] exp_diff;

            if (exp8 == 8'hFF) begin
                // Inf/NaN
                if (|man23)
                    fp16_out = {sign, 5'd31, 10'h200};  // qNaN
                else
                    fp16_out = {sign, 5'd31, 10'd0};    // Inf
            end
            else if (exp8 == 0 && man23 == 0)
                fp16_out = {sign, 15'd0};
            else begin
                exp_diff = {1'b0, exp8} - 9'sd112;  // 127 - 15 = 112

                if (exp_diff <= 0) begin
                    // Underflow → zero (simplified)
                    fp16_out = {sign, 15'd0};
                end
                else if (exp_diff >= 9'sd31) begin
                    // Overflow → infinity
                    fp16_out = {sign, 5'd31, 10'd0};
                end
                else begin
                    new_exp = exp_diff[4:0];
                    new_man = man23[22:13];
                    round_bit = man23[12];
                    sticky = |man23[11:0];

                    if (round_bit && (sticky || new_man[0])) begin
                        {new_exp, new_man} = {new_exp, new_man} + 15'd1;
                        if (new_exp == 5'd31)
                            fp16_out = {sign, 5'd31, 10'd0};  // overflow to inf
                        else
                            fp16_out = {sign, new_exp, new_man};
                    end else
                        fp16_out = {sign, new_exp, new_man};
                end
            end
        end
    end
endmodule
