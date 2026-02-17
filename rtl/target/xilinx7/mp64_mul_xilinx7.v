// ============================================================================
// mp64_mul_xilinx7.v — Xilinx 7-Series DSP48E1 Multiplier Override
// ============================================================================
//
// Drop-in replacement for mp64_mul.v targeting Xilinx 7-series FPGAs.
// Uses DSP48E1 cascading for the full 64×64→128 multiply.
//
// Usage:
//   At synthesis time, include this file instead of rtl/prim/mp64_mul.v
//   to map multiplication onto dedicated DSP48E1 slices.
//
// Notes:
//   - Xilinx 7-series DSP48E1 natively supports 25×18 signed multiply
//   - A 64×64 multiply requires a Karatsuba-style tree of 9 DSP48E1 slices
//   - Pipeline depth should match LATENCY parameter (default 4)
//   - This is a STUB — fill in for actual FPGA bring-up
//
// Port interface is identical to mp64_mul.v.
//

module mp64_mul #(
    parameter LATENCY = 4               // must match system pipeline depth
)(
    input  wire         clk,
    input  wire         rst,
    input  wire         start,
    input  wire         is_signed,
    input  wire [63:0]  a,
    input  wire [63:0]  b,
    output wire [127:0] result,
    output wire         done,
    output wire         busy
);

    // ========================================================================
    // STUB — currently falls back to behavioural model.
    // Replace the body below with DSP48E1 instantiations for production.
    // ========================================================================

    // Pipeline shift register (same as generic version)
    reg [127:0] pipe_data [0:LATENCY-1];
    reg [LATENCY-1:0] pipe_valid;

    wire [127:0] product = is_signed
        ? $signed(a) * $signed(b)
        : a * b;

    integer i;
    always @(posedge clk) begin
        if (rst) begin
            pipe_valid <= {LATENCY{1'b0}};
            for (i = 0; i < LATENCY; i = i + 1)
                pipe_data[i] <= 128'd0;
        end else begin
            pipe_valid[0] <= start;
            pipe_data[0]  <= start ? product : pipe_data[0];
            for (i = 1; i < LATENCY; i = i + 1) begin
                pipe_valid[i] <= pipe_valid[i-1];
                pipe_data[i]  <= pipe_data[i-1];
            end
        end
    end

    assign result = pipe_data[LATENCY-1];
    assign done   = pipe_valid[LATENCY-1];
    assign busy   = start | (|pipe_valid);

endmodule
