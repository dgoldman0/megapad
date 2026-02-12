// ============================================================================
// FP stubs — simulation-only placeholders for BIOS boot testing
// ============================================================================
// These return zero for all operations.  Sufficient for BIOS boot where
// tile FP ops are never exercised.

module mp64_fp32_adder (
    input  wire [31:0] a,
    input  wire [31:0] b,
    output wire [31:0] result
);
    assign result = 32'h0;
endmodule

module mp64_fp16_alu (
    input  wire        is_bf16,
    input  wire [15:0] a,
    input  wire [15:0] b,
    input  wire        op_add,
    input  wire        op_sub,
    input  wire        op_mul,
    input  wire        op_min,
    input  wire        op_max,
    input  wire        op_abs,
    input  wire        op_cmp_lt,
    input  wire        op_cmp_gt,
    output reg  [15:0] result,
    output wire        is_nan_a,
    output wire        is_nan_b
);
    assign is_nan_a = 1'b0;
    assign is_nan_b = 1'b0;
    always @(*) result = 16'h0;
endmodule

module mp64_fp16_to_fp32 (
    input  wire        is_bf16,
    input  wire [15:0] fp16_in,
    output wire [31:0] fp32_out
);
    assign fp32_out = 32'h0;
endmodule

module mp64_fp32_to_fp16 (
    input  wire        is_bf16,
    input  wire [31:0] fp32_in,
    output wire [15:0] fp16_out
);
    assign fp16_out = 16'h0;
endmodule
