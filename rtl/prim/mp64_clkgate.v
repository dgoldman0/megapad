// ============================================================================
// mp64_clkgate.v — Clock Gate Cell (Portable Behavioural)
// ============================================================================
//
// Glitch-free clock gating cell.  When enable is low, clk_out remains
// low and the downstream logic does not see clock edges.
//
// Behavioural implementation:
//   - Latches enable on the negative edge of clk_in (standard ICG pattern)
//   - AND gate produces clk_out = clk_in & enable_latched
//   - No glitches: enable change during clk_in high is held until falling edge
//
// Platform overrides:
//   - ASIC: instantiate foundry ICG cell (e.g. TLATNCAX12)
//   - FPGA: use BUFGCE or keep the AND gate (tools handle it)
//
// Coding standard: Verilog-2001.
//

module mp64_clkgate (
    input  wire clk_in,
    input  wire enable,
    input  wire test_en,    // scan test override (forces clock on)
    output wire clk_out
);

    // Latch enable on falling edge of clk_in (standard ICG)
    reg enable_latched;

    always @(*) begin
        if (!clk_in)
            enable_latched = enable | test_en;
    end

    assign clk_out = clk_in & enable_latched;

endmodule
