// ============================================================================
// mp64_clkgate_asic.v — ASIC Clock Gate Stub
// ============================================================================
// Replace body with integrated clock gate (ICG) standard cell.
// Port interface identical to rtl/prim/mp64_clkgate.v.
//

module mp64_clkgate (
    input  wire clk_in,
    input  wire enable,
    output wire clk_out
);

    // STUB — latch-based ICG (replace with foundry ICG cell)
    reg en_latched;

    always @(*) begin
        if (!clk_in)
            en_latched = enable;
    end

    assign clk_out = clk_in & en_latched;

endmodule
