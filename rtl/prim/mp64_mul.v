// ============================================================================
// mp64_mul.v — Portable 64×64→128 Multiplier Wrapper
// ============================================================================
//
// Behavioural model of a pipelined 64-bit multiplier.
// Produces a 128-bit result after LATENCY clock cycles.
//
// Interface contract:
//   - Assert `start` for one cycle with operands on `a`, `b`, `is_signed`.
//   - Result is valid when `done` goes high (LATENCY cycles later).
//   - Callers must not issue a new `start` until `done` fires.
//   - `busy` is high from `start` until `done` (inclusive).
//
// Default LATENCY = 4 (matches DSP48E1 fully-pipelined, Kintex-7).
//
// Platform overrides:
//   Replace this file with a target-specific implementation that
//   instantiates vendor DSP primitives but keeps the same port interface.
//   Alternatively, override LATENCY for targets with fewer pipeline
//   stages (e.g. simulation: LATENCY=1).
//
// Coding standard:
//   - Verilog-2001, synchronous reset (active-high), non-blocking assigns
//

module mp64_mul #(
    parameter LATENCY = 4      // output latency in clock cycles
) (
    input  wire         clk,
    input  wire         rst,

    input  wire         start,      // pulse high for 1 cycle
    input  wire         is_signed,  // 1 = signed multiply
    input  wire  [63:0] a,
    input  wire  [63:0] b,

    output wire [127:0] result,
    output wire         done,       // pulse high when result valid
    output wire         busy
);

    // ========================================================================
    // Pipeline shift register
    // ========================================================================
    reg [LATENCY-1:0] pipe_valid;
    reg [127:0]       result_reg;

    wire [127:0] product_signed   = $signed(a) * $signed(b);
    wire [127:0] product_unsigned = a * b;

    always @(posedge clk) begin
        if (rst) begin
            pipe_valid <= {LATENCY{1'b0}};
            result_reg <= 128'd0;
        end else begin
            // Shift valid pipeline
            pipe_valid <= {pipe_valid[LATENCY-2:0], start};

            // Capture product at start (combinational multiply)
            if (start)
                result_reg <= is_signed ? product_signed : product_unsigned;
        end
    end

    assign result = result_reg;
    assign done   = pipe_valid[LATENCY-1];
    assign busy   = start | (|pipe_valid);

endmodule
