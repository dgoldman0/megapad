// ============================================================================
// mp64_fp_half.v — FP16/BF16 tile-lane arithmetic (combinational)
// ============================================================================
//
// docs/floating-point.md defines every result and shared/ieee_fp.py is the
// executable reference.  Each lane computes the exact result through the
// bit-exact binary32 modules in mp64_fp_exact.v and rounds once to the lane
// format:
//
//   * ADD/SUB/MUL round to binary32 first.  That is exact for products of
//     FP16 operands and correctly rounded otherwise; a second rounding to a
//     format of precision p is then harmless because 24 >= 2p + 2.
//   * FMA/MAC add the exact product to the widened addend with round-to-odd
//     binary32, which rounds correctly to any format with 24 >= p + 2.
//
// Every NaN result is the lane format's canonical quiet NaN, subnormals are
// kept, and MIN/MAX are IEEE 754-2019 minimum/maximum (-0 below +0).
//
// Public modules:
//
//   mp64_fp16_to_fp32      exact FP16/BF16 -> binary32 widening
//   mp64_fp32_to_half_rne  binary32 -> FP16/BF16 with one RNE rounding
//   mp64_fp_half_lane      one FP16/BF16 TALU/TMUL lane

// ============================================================================
// FP16/BF16 -> binary32 (exact; NaN becomes the canonical binary32 NaN)
// ============================================================================

module mp64_fp16_to_fp32 (
    input  wire        is_bf16,
    input  wire [15:0] fp16_in,
    output reg  [31:0] fp32_out
);
    always @(*) begin : widen_block
        integer s;
        reg        sign;
        reg [4:0]  exp5;
        reg [9:0]  man10;
        reg [7:0]  exp8;
        reg [9:0]  m;

        sign  = fp16_in[15];
        exp5  = fp16_in[14:10];
        man10 = fp16_in[9:0];
        exp8  = 8'd0;
        m     = 10'd0;

        if (is_bf16) begin
            if (fp16_in[14:7] == 8'hFF && fp16_in[6:0] != 7'd0)
                fp32_out = 32'h7FC0_0000;
            else
                fp32_out = {fp16_in, 16'd0};
        end else if (exp5 == 5'd0 && man10 == 10'd0) begin
            fp32_out = {sign, 31'd0};
        end else if (exp5 == 5'd0) begin
            // Subnormal FP16 becomes a normal binary32 value.
            exp8 = 8'd112;
            m    = man10;
            for (s = 0; s < 10; s = s + 1) begin
                if (m[9] == 1'b0) begin
                    m    = {m[8:0], 1'b0};
                    exp8 = exp8 - 8'd1;
                end
            end
            fp32_out = {sign, exp8, m[8:0], 14'd0};
        end else if (exp5 == 5'd31) begin
            fp32_out = (man10 != 10'd0) ?
                32'h7FC0_0000 : {sign, 8'hFF, 23'd0};
        end else begin
            fp32_out = {sign, {3'd0, exp5} + 8'd112, man10, 13'd0};
        end
    end
endmodule

// ============================================================================
// binary32 -> FP16/BF16 with one round-to-nearest-even rounding
// ============================================================================

module mp64_fp32_to_half_rne (
    input  wire        is_bf16,
    input  wire [31:0] value,
    output reg  [15:0] result
);
    always @(*) begin : narrow_block
        integer fraction_bits;
        integer precision;
        integer bias;
        integer exponent_limit;
        integer lsb_exponent;
        integer lead;
        integer quantum;
        integer shift;
        integer biased;
        integer bit_index;
        reg        sign;
        reg [7:0]  exponent_field;
        reg [22:0] fraction;
        reg [23:0] significand;
        reg [49:0] scaled;
        reg [23:0] mantissa;
        reg [23:0] hidden;
        reg        round_bit;
        reg        sticky;

        fraction_bits  = is_bf16 ? 7 : 10;
        precision      = fraction_bits + 1;
        bias           = is_bf16 ? 127 : 15;
        exponent_limit = is_bf16 ? 255 : 31;
        sign           = value[31];
        exponent_field = value[30:23];
        fraction       = value[22:0];
        significand    = 24'd0;
        scaled         = 50'd0;
        mantissa       = 24'd0;
        hidden         = 24'd0;
        round_bit      = 1'b0;
        sticky         = 1'b0;
        lead           = -1;
        quantum        = 0;
        shift          = 0;
        biased         = 0;
        lsb_exponent   = 0;
        result         = 16'd0;

        if (exponent_field == 8'hFF) begin
            if (fraction != 23'd0)
                result = is_bf16 ? 16'h7FC0 : 16'h7E00;
            else
                result = {sign, is_bf16 ? 15'h7F80 : 15'h7C00};
        end else if (exponent_field == 8'd0 && fraction == 23'd0) begin
            result = {sign, 15'd0};
        end else begin
            significand  = {exponent_field != 8'd0, fraction};
            if (exponent_field != 8'd0)
                lsb_exponent = exponent_field;
            else
                lsb_exponent = 1;
            lsb_exponent = lsb_exponent - 150;
            for (bit_index = 0; bit_index < 24; bit_index = bit_index + 1)
                if (significand[bit_index])
                    lead = bit_index;
            // The quantum is the exponent of the result's last place.
            quantum = lead + lsb_exponent - (precision - 1);
            if (quantum < 1 - bias - (precision - 1))
                quantum = 1 - bias - (precision - 1);
            shift = quantum - lsb_exponent;
            if (shift >= 26) begin
                // The value is below a quarter of the last place.
                result = {sign, 15'd0};
            end else begin
                scaled    = {significand, 26'd0} >> shift;
                mantissa  = scaled[49:26];
                round_bit = scaled[25];
                sticky    = |scaled[24:0];
                if (round_bit && (sticky || mantissa[0])) begin
                    mantissa = mantissa + 24'd1;
                    if (mantissa == (24'd1 << precision)) begin
                        mantissa = mantissa >> 1;
                        quantum  = quantum + 1;
                    end
                end
                hidden = 24'd1 << (precision - 1);
                if (mantissa >= hidden) begin
                    biased = quantum + (precision - 1) + bias;
                    if (biased >= exponent_limit)
                        result = {sign, is_bf16 ? 15'h7F80 : 15'h7C00};
                    else
                        result = {sign, 15'd0} |
                                 (biased << fraction_bits) |
                                 (mantissa - hidden);
                end else begin
                    result = {sign, 15'd0} | mantissa[15:0];
                end
            end
        end
    end
endmodule

// ============================================================================
// One FP16/BF16 TALU/TMUL lane
// ============================================================================
//
// The exact product of a and b comes from the tile's shared exact-product
// array, so a lane does not elaborate a second multiplier.

module mp64_fp_half_lane (
    input  wire        is_bf16,
    input  wire [2:0]  op,
    input  wire [15:0] a,
    input  wire [15:0] b,
    input  wire [15:0] c,

    input  wire        product_nan,
    input  wire        product_inf,
    input  wire        product_zero,
    input  wire        product_finite,
    input  wire        product_sign,
    input  wire [21:0] product_significand,
    input  wire signed [10:0] product_exponent,
    input  wire [31:0] product_fp32,

    output reg  [15:0] result
);
    localparam [2:0] LANE_ADD = 3'd0;
    localparam [2:0] LANE_SUB = 3'd1;
    localparam [2:0] LANE_MUL = 3'd2;
    localparam [2:0] LANE_FMA = 3'd3;
    localparam [2:0] LANE_MIN = 3'd4;
    localparam [2:0] LANE_MAX = 3'd5;
    localparam [2:0] LANE_ABS = 3'd6;

    wire [31:0] wide_a;
    wire [31:0] wide_b;
    wire [31:0] wide_c;
    wire [31:0] sum32;
    wire [31:0] fma32;
    wire [15:0] narrowed;

    mp64_fp16_to_fp32 u_widen_a (
        .is_bf16(is_bf16), .fp16_in(a), .fp32_out(wide_a));
    mp64_fp16_to_fp32 u_widen_b (
        .is_bf16(is_bf16), .fp16_in(b), .fp32_out(wide_b));
    mp64_fp16_to_fp32 u_widen_c (
        .is_bf16(is_bf16), .fp16_in(c), .fp32_out(wide_c));

    // Subtraction negates the widened operand; a NaN stays a NaN.
    mp64_fp32_add_rne u_add (
        .a(wide_a),
        .b((op == LANE_SUB) ? {~wide_b[31], wide_b[30:0]} : wide_b),
        .result(sum32));

    mp64_fp32_add_exact_product_rto u_fma (
        .accumulator        (wide_c),
        .product_nan        (product_nan),
        .product_inf        (product_inf),
        .product_zero       (product_zero),
        .product_finite     (product_finite),
        .product_sign       (product_sign),
        .product_significand(product_significand),
        .product_exponent   (product_exponent),
        .result             (fma32));

    mp64_fp32_to_half_rne u_narrow (
        .is_bf16(is_bf16),
        .value  ((op == LANE_MUL) ? product_fp32 :
                 (op == LANE_FMA) ? fma32 : sum32),
        .result (narrowed));

    // IEEE 754-2019 ordering of non-NaN encodings: -0 below +0.
    function [15:0] order_key;
        input [15:0] bits;
        begin
            order_key = bits[15] ? ~bits : {1'b1, bits[14:0]};
        end
    endfunction

    wire [15:0] canonical_nan = is_bf16 ? 16'h7FC0 : 16'h7E00;
    wire a_nan = is_bf16 ?
        (a[14:7] == 8'hFF && a[6:0] != 7'd0) :
        (a[14:10] == 5'h1F && a[9:0] != 10'd0);
    wire b_nan = is_bf16 ?
        (b[14:7] == 8'hFF && b[6:0] != 7'd0) :
        (b[14:10] == 5'h1F && b[9:0] != 10'd0);
    wire a_below_b = order_key(a) < order_key(b);

    always @(*) begin
        case (op)
            LANE_MIN:
                result = (a_nan || b_nan) ? canonical_nan :
                         (a_below_b || order_key(a) == order_key(b)) ? a : b;
            LANE_MAX:
                result = (a_nan || b_nan) ? canonical_nan :
                         a_below_b ? b : a;
            LANE_ABS:
                result = {1'b0, a[14:0]};
            default:
                result = narrowed;
        endcase
    end
endmodule
