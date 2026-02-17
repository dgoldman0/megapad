// ============================================================================
// mp64_clkgate_xilinx7.v — Xilinx 7-Series Clock Gate Override
// ============================================================================
//
// Drop-in replacement for rtl/prim/mp64_clkgate.v targeting Xilinx 7-series.
// Uses BUFGCE for glitch-free gated clock distribution.
//

module mp64_clkgate (
    input  wire clk_in,
    input  wire enable,
    output wire clk_out
);

    BUFGCE u_bufgce (
        .I  (clk_in),
        .CE (enable),
        .O  (clk_out)
    );

endmodule
