// ============================================================================
// mp64_fp_exact.v — exact mixed-precision helpers for FP16/BF16 accumulation
// ============================================================================
//
// This file contains only bounded, combinational integer logic.  It does not
// use real/shortreal or simulator floating-point behavior.
//
// Exact finite values use this descriptor:
//
//     value = (-1)**sign * significand * 2**exponent
//
// The significand is an unsigned integer.  It need not be normalized.
// Special values are carried by one-hot NAN/INF/ZERO class bits and do not
// consume the finite descriptor.  All public results use round-to-nearest,
// ties-to-even and canonicalize every NaN to 32'h7FC0_0000.
//
// The shared arithmetic core normalizes exact descriptors into 27 bits
// (24 retained significand bits plus guard, round, and sticky), aligns with a
// right-shift-with-jam operation, performs one sign/magnitude add or subtract,
// and rounds exactly once.  Exponent differences are bounded by the descriptor
// width; no exponent-sized vectors or unbounded loops are elaborated.
//
// Public modules:
//
//   mp64_fp16_bf16_exact_product
//       Decode two raw FP16/BF16 operands, classify their exact product,
//       expose its exact descriptor, and independently round that product to
//       binary32.
//
//   mp64_fp32_add_rne
//       Bit-exact binary32 addition with canonical NaNs and full subnormal
//       support.
//
//   mp64_fp32_add_exact_product_rne
//       Add a binary32 accumulator to an exact product descriptor with one
//       final binary32 rounding point.  This is the TACC feedback operation.
//
// Internal modules are intentionally kept in this file so every consumer uses
// the same decoder and final rounding rules.

// ============================================================================
// Binary32 decoder to the exact finite descriptor.
// ============================================================================

module mp64_fp32_exact_decode (
    input  wire [31:0] value,

    output reg         value_nan,
    output reg         value_inf,
    output reg         value_zero,
    output reg         value_sign,
    output reg  [23:0] value_significand,
    output reg signed [10:0] value_exponent
);
    reg [7:0]  exponent_field;
    reg [22:0] fraction_field;

    always @(*) begin
        exponent_field   = value[30:23];
        fraction_field   = value[22:0];
        value_sign       = value[31];
        value_nan        = 1'b0;
        value_inf        = 1'b0;
        value_zero       = 1'b0;
        value_significand = 24'd0;
        value_exponent    = 11'sd0;

        if (exponent_field == 8'hFF) begin
            if (fraction_field != 23'd0)
                value_nan = 1'b1;
            else
                value_inf = 1'b1;
        end else if (exponent_field == 8'd0) begin
            if (fraction_field == 23'd0) begin
                value_zero = 1'b1;
            end else begin
                // Binary32 subnormal:
                //     fraction * 2^-149
                value_significand = {1'b0, fraction_field};
                value_exponent    = -11'sd149;
            end
        end else begin
            // Binary32 normal:
            //     (2^23 + fraction) * 2^(exp_field - 150)
            value_significand = {1'b1, fraction_field};
            value_exponent =
                $signed({3'b000, exponent_field}) - 11'sd150;
        end
    end
endmodule

// ============================================================================
// Add two exact descriptors and round once to binary32.
//
// NAN has precedence over every other class.  Opposite infinities produce the
// canonical NaN.  Under RNE, an exact finite cancellation produces +0; adding
// two exact zeros produces -0 only when both zero signs are negative.
// ============================================================================

module mp64_fp32_exact_add_terms (
    input  wire         a_nan,
    input  wire         a_inf,
    input  wire         a_zero,
    input  wire         a_sign,
    input  wire [23:0]  a_significand,
    input  wire signed [10:0] a_exponent,

    input  wire         b_nan,
    input  wire         b_inf,
    input  wire         b_zero,
    input  wire         b_sign,
    input  wire [23:0]  b_significand,
    input  wire signed [10:0] b_exponent,

    output reg  [31:0]  result
);
    // Shift right while preserving all discarded information in bit zero.
    // The fixed 27-iteration OR is synthesizable and bounds the logic even
    // when the architectural exponent difference is hundreds of places.
    function automatic [26:0] shift_right_jam27;
        input [26:0] value;
        input integer amount;
        integer bit_index;
        reg sticky;
        reg [26:0] shifted;
        begin
            sticky  = 1'b0;
            shifted = 27'd0;
            if (amount <= 0) begin
                shifted = value;
            end else if (amount >= 27) begin
                shifted = 27'd0;
                shifted[0] = |value;
            end else begin
                shifted = value >> amount;
                for (bit_index = 0;
                     bit_index < 27;
                     bit_index = bit_index + 1)
                    if (bit_index < amount)
                        sticky = sticky | value[bit_index];
                shifted[0] = shifted[0] | sticky;
            end
            shift_right_jam27 = shifted;
        end
    endfunction

    always @(*) begin : exact_add_block
        integer bit_index;
        integer a_lead;
        integer b_lead;
        integer work_lead;
        integer a_leading_exponent;
        integer b_leading_exponent;
        integer large_exponent;
        integer work_exponent;
        integer align_shift;
        integer normalize_shift;
        integer available_shift;
        integer underflow_shift;
        integer rounded_exponent;
        reg     a_nonzero;
        reg     b_nonzero;
        reg     a_is_larger;
        reg     large_sign;
        reg     small_sign;
        reg     result_sign;
        reg     exact_cancel;
        reg     round_up;
        reg     work_is_normal;
        reg [26:0] a_normalized;
        reg [26:0] b_normalized;
        reg [26:0] large_significand;
        reg [26:0] small_significand;
        reg [26:0] aligned_small;
        reg [26:0] work_significand;
        reg [27:0] add_significand;
        reg [23:0] retained_significand;
        reg [24:0] rounded_significand;
        reg [7:0]  result_exponent_field;

        result                = 32'd0;
        a_lead                = -1;
        b_lead                = -1;
        work_lead             = -1;
        a_leading_exponent    = -2048;
        b_leading_exponent    = -2048;
        large_exponent        = -2048;
        work_exponent         = -2048;
        align_shift           = 0;
        normalize_shift       = 0;
        available_shift       = 0;
        underflow_shift       = 0;
        rounded_exponent      = 0;
        a_nonzero             = 1'b0;
        b_nonzero             = 1'b0;
        a_is_larger           = 1'b0;
        large_sign            = 1'b0;
        small_sign            = 1'b0;
        result_sign           = 1'b0;
        exact_cancel          = 1'b0;
        round_up              = 1'b0;
        work_is_normal        = 1'b0;
        a_normalized          = 27'd0;
        b_normalized          = 27'd0;
        large_significand     = 27'd0;
        small_significand     = 27'd0;
        aligned_small         = 27'd0;
        work_significand      = 27'd0;
        add_significand       = 28'd0;
        retained_significand  = 24'd0;
        rounded_significand   = 25'd0;
        result_exponent_field = 8'd0;

        // Canonical exceptional-value handling precedes finite arithmetic.
        if (a_nan || b_nan) begin
            result = 32'h7FC0_0000;
        end else if (a_inf && b_inf) begin
            if (a_sign != b_sign)
                result = 32'h7FC0_0000;
            else
                result = {a_sign, 8'hFF, 23'd0};
        end else if (a_inf) begin
            result = {a_sign, 8'hFF, 23'd0};
        end else if (b_inf) begin
            result = {b_sign, 8'hFF, 23'd0};
        end else begin
            a_nonzero = !a_zero && (a_significand != 24'd0);
            b_nonzero = !b_zero && (b_significand != 24'd0);

            // Locate each exact integer significand's leading bit and move it
            // to bit 26.  Bits [2:0] are then available for G/R/S.
            for (bit_index = 0;
                 bit_index < 24;
                 bit_index = bit_index + 1) begin
                if (a_significand[bit_index])
                    a_lead = bit_index;
                if (b_significand[bit_index])
                    b_lead = bit_index;
            end

            if (a_nonzero) begin
                a_normalized =
                    {3'b000, a_significand} << (26 - a_lead);
                a_leading_exponent = $signed(a_exponent) + a_lead;
            end
            if (b_nonzero) begin
                b_normalized =
                    {3'b000, b_significand} << (26 - b_lead);
                b_leading_exponent = $signed(b_exponent) + b_lead;
            end

            if (!a_nonzero && !b_nonzero) begin
                // IEEE RNE zero addition: -0 only when both inputs are -0.
                result = {a_sign & b_sign, 31'd0};
            end else begin
                if (!b_nonzero) begin
                    a_is_larger = 1'b1;
                end else if (!a_nonzero) begin
                    a_is_larger = 1'b0;
                end else if (a_leading_exponent >
                             b_leading_exponent) begin
                    a_is_larger = 1'b1;
                end else if (a_leading_exponent <
                             b_leading_exponent) begin
                    a_is_larger = 1'b0;
                end else begin
                    a_is_larger = a_normalized >= b_normalized;
                end

                if (a_is_larger) begin
                    large_significand = a_normalized;
                    small_significand = b_normalized;
                    large_exponent    = a_leading_exponent;
                    large_sign        = a_sign;
                    small_sign        = b_sign;
                    align_shift       =
                        a_leading_exponent - b_leading_exponent;
                end else begin
                    large_significand = b_normalized;
                    small_significand = a_normalized;
                    large_exponent    = b_leading_exponent;
                    large_sign        = b_sign;
                    small_sign        = a_sign;
                    align_shift       =
                        b_leading_exponent - a_leading_exponent;
                end

                if (!a_nonzero || !b_nonzero) begin
                    aligned_small = 27'd0;
                end else begin
                    aligned_small =
                        shift_right_jam27(
                            small_significand, align_shift);
                end

                result_sign   = large_sign;
                work_exponent = large_exponent;

                if (large_sign == small_sign ||
                    !a_nonzero || !b_nonzero) begin
                    add_significand =
                        {1'b0, large_significand} +
                        {1'b0, aligned_small};
                    if (add_significand[27]) begin
                        // A same-sign carry renormalizes right by one.  The
                        // discarded bit joins the existing sticky bit.
                        work_significand = add_significand[27:1];
                        work_significand[0] =
                            work_significand[0] |
                            add_significand[0];
                        work_exponent = large_exponent + 1;
                    end else begin
                        work_significand = add_significand[26:0];
                    end
                end else if ((a_leading_exponent ==
                              b_leading_exponent) &&
                             (a_normalized == b_normalized)) begin
                    // Exact finite cancellation is always +0 under RNE.
                    exact_cancel = 1'b1;
                    work_significand = 27'd0;
                    result_sign = 1'b0;
                end else begin
                    work_significand =
                        large_significand - aligned_small;
                end

                if (exact_cancel) begin
                    result = 32'd0;
                end else begin
                    // Normalize a subtraction in one bounded leading-zero
                    // shift.  Do not shift past E=-126; below that point the
                    // final value is represented as a binary32 subnormal.
                    for (bit_index = 0;
                         bit_index < 27;
                         bit_index = bit_index + 1)
                        if (work_significand[bit_index])
                            work_lead = bit_index;

                    if ((work_lead >= 0) &&
                        (work_lead < 26) &&
                        (work_exponent > -126)) begin
                        normalize_shift = 26 - work_lead;
                        available_shift = work_exponent + 126;
                        if (normalize_shift > available_shift)
                            normalize_shift = available_shift;
                        work_significand =
                            work_significand << normalize_shift;
                        work_exponent =
                            work_exponent - normalize_shift;
                    end

                    // Convert exact values below the normal range to the
                    // fixed binary32 subnormal quantum, preserving a jam bit
                    // for correct halfway and underflow decisions.
                    if (work_exponent < -126) begin
                        underflow_shift = -126 - work_exponent;
                        work_significand =
                            shift_right_jam27(
                                work_significand, underflow_shift);
                        work_exponent = -126;
                    end

                    if (work_significand == 27'd0) begin
                        result = {result_sign, 31'd0};
                    end else if (work_exponent > 127) begin
                        result = {result_sign, 8'hFF, 23'd0};
                    end else begin
                        retained_significand =
                            work_significand[26:3];
                        round_up =
                            work_significand[2] &&
                            (work_significand[1] ||
                             work_significand[0] ||
                             retained_significand[0]);
                        rounded_significand =
                            {1'b0, retained_significand} +
                            {{24{1'b0}}, round_up};
                        work_is_normal =
                            (work_exponent > -126) ||
                            ((work_exponent == -126) &&
                             work_significand[26]);

                        if (work_is_normal) begin
                            if (rounded_significand[24]) begin
                                rounded_exponent = work_exponent + 1;
                                if (rounded_exponent > 127)
                                    result = {
                                        result_sign, 8'hFF, 23'd0};
                                else begin
                                    result_exponent_field =
                                        rounded_exponent + 127;
                                    result = {
                                        result_sign,
                                        result_exponent_field,
                                        23'd0};
                                end
                            end else begin
                                result_exponent_field =
                                    work_exponent + 127;
                                result = {
                                    result_sign,
                                    result_exponent_field,
                                    rounded_significand[22:0]};
                            end
                        end else if (rounded_significand[23]) begin
                            // Rounding the largest subnormal upward produces
                            // the minimum normal binary32 value.
                            result = {
                                result_sign, 8'd1,
                                rounded_significand[22:0]};
                        end else begin
                            // A nonzero exact result may round to signed zero.
                            result = {
                                result_sign, 8'd0,
                                rounded_significand[22:0]};
                        end
                    end
                end
            end
        end
    end
endmodule

// ============================================================================
// Round one exact finite descriptor to binary32.
//
// Product-only consumers do not need an adder.  Keeping this packer separate
// makes the 32 exact-product lanes structurally incapable of elaborating a
// second feedback-add bank.
// ============================================================================

module mp64_fp32_round_exact (
    input  wire        value_nan,
    input  wire        value_inf,
    input  wire        value_zero,
    input  wire        value_sign,
    input  wire [23:0] value_significand,
    input  wire signed [10:0] value_exponent,
    output reg  [31:0] result
);
    function automatic [26:0] shift_right_jam27;
        input [26:0] value;
        input integer amount;
        integer bit_index;
        reg sticky;
        reg [26:0] shifted;
        begin
            sticky  = 1'b0;
            shifted = 27'd0;
            if (amount <= 0) begin
                shifted = value;
            end else if (amount >= 27) begin
                shifted[0] = |value;
            end else begin
                shifted = value >> amount;
                for (bit_index = 0;
                     bit_index < 27;
                     bit_index = bit_index + 1)
                    if (bit_index < amount)
                        sticky = sticky | value[bit_index];
                shifted[0] = shifted[0] | sticky;
            end
            shift_right_jam27 = shifted;
        end
    endfunction

    always @(*) begin : round_exact_block
        integer bit_index;
        integer leading_bit;
        integer leading_exponent;
        integer underflow_shift;
        integer rounded_exponent;
        reg [26:0] work_significand;
        reg [23:0] retained_significand;
        reg [24:0] rounded_significand;
        reg         round_up;
        reg         work_is_normal;
        reg [7:0]   result_exponent_field;

        result                = 32'd0;
        leading_bit           = -1;
        leading_exponent      = -2048;
        underflow_shift       = 0;
        rounded_exponent      = 0;
        work_significand      = 27'd0;
        retained_significand  = 24'd0;
        rounded_significand   = 25'd0;
        round_up              = 1'b0;
        work_is_normal        = 1'b0;
        result_exponent_field = 8'd0;

        if (value_nan) begin
            result = 32'h7FC0_0000;
        end else if (value_inf) begin
            result = {value_sign, 8'hFF, 23'd0};
        end else if (value_zero || value_significand == 24'd0) begin
            result = {value_sign, 31'd0};
        end else begin
            for (bit_index = 0;
                 bit_index < 24;
                 bit_index = bit_index + 1)
                if (value_significand[bit_index])
                    leading_bit = bit_index;

            work_significand =
                {3'b000, value_significand} << (26 - leading_bit);
            leading_exponent =
                $signed(value_exponent) + leading_bit;

            if (leading_exponent < -126) begin
                underflow_shift = -126 - leading_exponent;
                work_significand =
                    shift_right_jam27(
                        work_significand, underflow_shift);
                leading_exponent = -126;
            end

            if (work_significand == 27'd0) begin
                result = {value_sign, 31'd0};
            end else if (leading_exponent > 127) begin
                result = {value_sign, 8'hFF, 23'd0};
            end else begin
                retained_significand = work_significand[26:3];
                round_up =
                    work_significand[2] &&
                    (work_significand[1] ||
                     work_significand[0] ||
                     retained_significand[0]);
                rounded_significand =
                    {1'b0, retained_significand} +
                    {{24{1'b0}}, round_up};
                work_is_normal =
                    (leading_exponent > -126) ||
                    ((leading_exponent == -126) &&
                     work_significand[26]);

                if (work_is_normal) begin
                    if (rounded_significand[24]) begin
                        rounded_exponent = leading_exponent + 1;
                        if (rounded_exponent > 127)
                            result = {value_sign, 8'hFF, 23'd0};
                        else begin
                            result_exponent_field =
                                rounded_exponent + 127;
                            result = {
                                value_sign,
                                result_exponent_field,
                                23'd0};
                        end
                    end else begin
                        result_exponent_field =
                            leading_exponent + 127;
                        result = {
                            value_sign,
                            result_exponent_field,
                            rounded_significand[22:0]};
                    end
                end else if (rounded_significand[23]) begin
                    result = {
                        value_sign, 8'd1,
                        rounded_significand[22:0]};
                end else begin
                    result = {
                        value_sign, 8'd0,
                        rounded_significand[22:0]};
                end
            end
        end
    end
endmodule

// ============================================================================
// Exact FP16/BF16 product descriptor and independently rounded binary32 view.
//
// is_bf16=0 selects IEEE binary16 (1/5/10).
// is_bf16=1 selects bfloat16     (1/8/7).
//
// Exactly one product_* class output is asserted.  product_significand and
// product_exponent are meaningful only when product_finite is asserted.
// ============================================================================

module mp64_fp16_bf16_exact_product (
    input  wire        is_bf16,
    input  wire [15:0] a,
    input  wire [15:0] b,

    output reg         product_nan,
    output reg         product_inf,
    output reg         product_zero,
    output reg         product_finite,
    output reg         product_sign,
    output reg  [21:0] product_significand,
    output reg signed [10:0] product_exponent,
    output wire [31:0] rounded_fp32
);
    reg        a_nan;
    reg        a_inf;
    reg        a_zero;
    reg        b_nan;
    reg        b_inf;
    reg        b_zero;
    reg [10:0] a_significand;
    reg [10:0] b_significand;
    reg signed [10:0] a_exponent;
    reg signed [10:0] b_exponent;
    reg [7:0]  a_exp8;
    reg [7:0]  b_exp8;
    reg [6:0]  a_frac7;
    reg [6:0]  b_frac7;
    reg [4:0]  a_exp5;
    reg [4:0]  b_exp5;
    reg [9:0]  a_frac10;
    reg [9:0]  b_frac10;

    always @(*) begin
        a_nan               = 1'b0;
        a_inf               = 1'b0;
        a_zero              = 1'b0;
        b_nan               = 1'b0;
        b_inf               = 1'b0;
        b_zero              = 1'b0;
        a_significand       = 11'd0;
        b_significand       = 11'd0;
        a_exponent          = 11'sd0;
        b_exponent          = 11'sd0;
        a_exp8              = a[14:7];
        b_exp8              = b[14:7];
        a_frac7             = a[6:0];
        b_frac7             = b[6:0];
        a_exp5              = a[14:10];
        b_exp5              = b[14:10];
        a_frac10            = a[9:0];
        b_frac10            = b[9:0];
        product_nan         = 1'b0;
        product_inf         = 1'b0;
        product_zero        = 1'b0;
        product_finite      = 1'b0;
        product_sign        = a[15] ^ b[15];
        product_significand = 22'd0;
        product_exponent    = 11'sd0;

        if (is_bf16) begin
            if (a_exp8 == 8'hFF) begin
                if (a_frac7 != 7'd0)
                    a_nan = 1'b1;
                else
                    a_inf = 1'b1;
            end else if (a_exp8 == 8'd0) begin
                if (a_frac7 == 7'd0)
                    a_zero = 1'b1;
                else begin
                    a_significand = {4'd0, a_frac7};
                    a_exponent    = -11'sd133;
                end
            end else begin
                a_significand = {3'd0, 1'b1, a_frac7};
                a_exponent =
                    $signed({3'b000, a_exp8}) - 11'sd134;
            end

            if (b_exp8 == 8'hFF) begin
                if (b_frac7 != 7'd0)
                    b_nan = 1'b1;
                else
                    b_inf = 1'b1;
            end else if (b_exp8 == 8'd0) begin
                if (b_frac7 == 7'd0)
                    b_zero = 1'b1;
                else begin
                    b_significand = {4'd0, b_frac7};
                    b_exponent    = -11'sd133;
                end
            end else begin
                b_significand = {3'd0, 1'b1, b_frac7};
                b_exponent =
                    $signed({3'b000, b_exp8}) - 11'sd134;
            end
        end else begin
            if (a_exp5 == 5'h1F) begin
                if (a_frac10 != 10'd0)
                    a_nan = 1'b1;
                else
                    a_inf = 1'b1;
            end else if (a_exp5 == 5'd0) begin
                if (a_frac10 == 10'd0)
                    a_zero = 1'b1;
                else begin
                    a_significand = {1'b0, a_frac10};
                    a_exponent    = -11'sd24;
                end
            end else begin
                a_significand = {1'b1, a_frac10};
                a_exponent =
                    $signed({6'b000000, a_exp5}) - 11'sd25;
            end

            if (b_exp5 == 5'h1F) begin
                if (b_frac10 != 10'd0)
                    b_nan = 1'b1;
                else
                    b_inf = 1'b1;
            end else if (b_exp5 == 5'd0) begin
                if (b_frac10 == 10'd0)
                    b_zero = 1'b1;
                else begin
                    b_significand = {1'b0, b_frac10};
                    b_exponent    = -11'sd24;
                end
            end else begin
                b_significand = {1'b1, b_frac10};
                b_exponent =
                    $signed({6'b000000, b_exp5}) - 11'sd25;
            end
        end

        if (a_nan || b_nan ||
            ((a_inf && b_zero) || (a_zero && b_inf))) begin
            product_nan = 1'b1;
        end else if (a_inf || b_inf) begin
            product_inf = 1'b1;
        end else if (a_zero || b_zero) begin
            product_zero = 1'b1;
        end else begin
            product_finite      = 1'b1;
            product_significand =
                a_significand * b_significand;
            product_exponent = a_exponent + b_exponent;
        end
    end

    mp64_fp32_round_exact u_round_product (
        .value_nan        (product_nan),
        .value_inf        (product_inf),
        .value_zero       (product_zero),
        .value_sign       (product_sign),
        .value_significand({2'd0, product_significand}),
        .value_exponent   (product_exponent),
        .result           (rounded_fp32)
    );
endmodule

// ============================================================================
// Public binary32 + binary32 RNE adder.
// ============================================================================

// One physical feedback lane.  The second term is selected between an
// ordinary binary32 operand and an exact half-precision product descriptor.
// Tile engines instantiate this module directly so reduction and TACC modes
// share one final-rounding cone rather than relying on synthesis to merge two
// separately elaborated adders.
module mp64_fp32_feedback_rne (
    input  wire        use_exact_product,
    input  wire [31:0] a,
    input  wire [31:0] b,

    input  wire        product_nan,
    input  wire        product_inf,
    input  wire        product_zero,
    input  wire        product_finite,
    input  wire        product_sign,
    input  wire [21:0] product_significand,
    input  wire signed [10:0] product_exponent,

    output wire [31:0] result
);
    wire        a_nan;
    wire        a_inf;
    wire        a_zero;
    wire        a_sign;
    wire [23:0] a_significand;
    wire signed [10:0] a_exponent;
    wire        b_nan;
    wire        b_inf;
    wire        b_zero;
    wire        b_sign;
    wire [23:0] b_significand;
    wire signed [10:0] b_exponent;

    wire selected_b_nan =
        use_exact_product ? product_nan : b_nan;
    wire selected_b_inf =
        use_exact_product ? product_inf : b_inf;
    wire selected_b_zero =
        use_exact_product ?
        (product_zero ||
         !(product_nan || product_inf || product_finite)) :
        b_zero;
    wire selected_b_sign =
        use_exact_product ? product_sign : b_sign;
    wire [23:0] selected_b_significand =
        use_exact_product ?
        (product_finite ? {2'd0, product_significand} : 24'd0) :
        b_significand;
    wire signed [10:0] selected_b_exponent =
        use_exact_product ? product_exponent : b_exponent;

    mp64_fp32_exact_decode u_decode_a (
        .value              (a),
        .value_nan          (a_nan),
        .value_inf          (a_inf),
        .value_zero         (a_zero),
        .value_sign         (a_sign),
        .value_significand  (a_significand),
        .value_exponent     (a_exponent)
    );

    mp64_fp32_exact_decode u_decode_b (
        .value              (b),
        .value_nan          (b_nan),
        .value_inf          (b_inf),
        .value_zero         (b_zero),
        .value_sign         (b_sign),
        .value_significand  (b_significand),
        .value_exponent     (b_exponent)
    );

    mp64_fp32_exact_add_terms u_add (
        .a_nan          (a_nan),
        .a_inf          (a_inf),
        .a_zero         (a_zero),
        .a_sign         (a_sign),
        .a_significand  (a_significand),
        .a_exponent     (a_exponent),
        .b_nan          (selected_b_nan),
        .b_inf          (selected_b_inf),
        .b_zero         (selected_b_zero),
        .b_sign         (selected_b_sign),
        .b_significand  (selected_b_significand),
        .b_exponent     (selected_b_exponent),
        .result         (result)
    );
endmodule

module mp64_fp32_add_rne (
    input  wire [31:0] a,
    input  wire [31:0] b,
    output wire [31:0] result
);
    wire        a_nan;
    wire        a_inf;
    wire        a_zero;
    wire        a_sign;
    wire [23:0] a_significand;
    wire signed [10:0] a_exponent;
    wire        b_nan;
    wire        b_inf;
    wire        b_zero;
    wire        b_sign;
    wire [23:0] b_significand;
    wire signed [10:0] b_exponent;

    mp64_fp32_exact_decode u_decode_a (
        .value              (a),
        .value_nan          (a_nan),
        .value_inf          (a_inf),
        .value_zero         (a_zero),
        .value_sign         (a_sign),
        .value_significand  (a_significand),
        .value_exponent     (a_exponent)
    );

    mp64_fp32_exact_decode u_decode_b (
        .value              (b),
        .value_nan          (b_nan),
        .value_inf          (b_inf),
        .value_zero         (b_zero),
        .value_sign         (b_sign),
        .value_significand  (b_significand),
        .value_exponent     (b_exponent)
    );

    mp64_fp32_exact_add_terms u_add (
        .a_nan          (a_nan),
        .a_inf          (a_inf),
        .a_zero         (a_zero),
        .a_sign         (a_sign),
        .a_significand  (a_significand),
        .a_exponent     (a_exponent),
        .b_nan          (b_nan),
        .b_inf          (b_inf),
        .b_zero         (b_zero),
        .b_sign         (b_sign),
        .b_significand  (b_significand),
        .b_exponent     (b_exponent),
        .result         (result)
    );
endmodule

// ============================================================================
// Public binary32 accumulator + exact FP16/BF16 product feedback operation.
//
// The product descriptor normally comes directly from
// mp64_fp16_bf16_exact_product.  The rounded_fp32 view of that module must not
// be inserted here: doing so would add an architectural rounding point and is
// observably wrong for BF16 underflow/tie cases.
// ============================================================================

module mp64_fp32_add_exact_product_rne (
    input  wire [31:0] accumulator,

    input  wire        product_nan,
    input  wire        product_inf,
    input  wire        product_zero,
    input  wire        product_finite,
    input  wire        product_sign,
    input  wire [21:0] product_significand,
    input  wire signed [10:0] product_exponent,

    output wire [31:0] result
);
    mp64_fp32_feedback_rne u_feedback (
        .use_exact_product  (1'b1),
        .a                  (accumulator),
        .b                  (32'd0),
        .product_nan        (product_nan),
        .product_inf        (product_inf),
        .product_zero       (product_zero),
        .product_finite     (product_finite),
        .product_sign       (product_sign),
        .product_significand(product_significand),
        .product_exponent   (product_exponent),
        .result             (result)
    );
endmodule
