// ============================================================================
// mp64_pll_xilinx7.v — Xilinx 7-Series MMCM PLL Override
// ============================================================================
//
// Drop-in replacement for rtl/prim/mp64_pll.v targeting Xilinx 7-series.
// Instantiates MMCME2_BASE for clock synthesis.
//
// Usage:
//   At synthesis time, include this file instead of rtl/prim/mp64_pll.v.
//

module mp64_pll #(
    parameter CLK_MUL = 5,
    parameter CLK_DIV = 10,
    parameter REF_MHZ = 200
)(
    input  wire  clk_in,
    input  wire  rst_in,
    output wire  clk_out,
    output reg   locked
);

    wire clkfb, clk_mmcm, mmcm_locked;

    MMCME2_BASE #(
        .CLKFBOUT_MULT_F  (CLK_MUL),
        .CLKOUT0_DIVIDE_F (CLK_DIV),
        .CLKIN1_PERIOD     (1000.0 / REF_MHZ)
    ) u_mmcm (
        .CLKIN1   (clk_in),
        .RST      (rst_in),
        .CLKFBIN  (clkfb),
        .CLKFBOUT (clkfb),
        .CLKOUT0  (clk_mmcm),
        .LOCKED   (mmcm_locked),
        // Unused outputs
        .CLKOUT0B (),
        .CLKOUT1  (), .CLKOUT1B (),
        .CLKOUT2  (), .CLKOUT2B (),
        .CLKOUT3  (), .CLKOUT3B (),
        .CLKOUT4  (), .CLKOUT5  (), .CLKOUT6  (),
        .PWRDWN   (1'b0)
    );

    BUFG u_bufg (.I(clk_mmcm), .O(clk_out));

    always @(posedge clk_in or posedge rst_in)
        if (rst_in) locked <= 1'b0;
        else        locked <= mmcm_locked;

endmodule
