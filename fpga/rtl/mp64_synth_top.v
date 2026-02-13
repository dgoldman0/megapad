// ============================================================================
// mp64_synth_top.v — Synthesis Wrapper for Genesys 2 (Kintex-7 325T)
// ============================================================================
//
// This wrapper handles:
//   - LVDS 200 MHz differential clock → MMCM → 100 MHz sys_clk
//   - Tie-offs for external memory PHY (not yet instantiated)
//   - Tie-offs for NIC PHY (not yet wired to board)
//   - Instantiation of the mp64_soc core
//
// For synthesis-only testing — no board-specific I/O beyond UART/SD/LEDs.
//

`include "mp64_defs.vh"

module mp64_synth_top (
    // Differential 200 MHz oscillator
    input  wire        sys_clk_p,
    input  wire        sys_clk_n,

    // Active-low reset button
    input  wire        sys_rst_n,

    // UART
    input  wire        uart_rxd,
    output wire        uart_txd,

    // SD Card (SPI)
    output wire        sd_sck,
    output wire        sd_mosi,
    input  wire        sd_miso,
    output wire        sd_cs_n,

    // Debug LEDs
    output wire [7:0]  debug_leds
);

    // ========================================================================
    // Clock Generation — MMCM: 200 MHz LVDS → 100 MHz single-ended
    // ========================================================================
    wire clk_200_buf;
    wire clk_100;
    wire mmcm_locked;
    wire mmcm_fb;

    IBUFDS #(
        .DIFF_TERM("TRUE"),
        .IBUF_LOW_PWR("FALSE")
    ) u_ibufds (
        .I  (sys_clk_p),
        .IB (sys_clk_n),
        .O  (clk_200_buf)
    );

    MMCME2_BASE #(
        .CLKIN1_PERIOD  (5.0),     // 200 MHz input
        .CLKFBOUT_MULT_F(5.0),    // VCO = 200 × 5 = 1000 MHz
        .CLKOUT0_DIVIDE_F(10.0),  // 1000 / 10 = 100 MHz
        .STARTUP_WAIT   ("FALSE")
    ) u_mmcm (
        .CLKIN1   (clk_200_buf),
        .CLKFBIN  (mmcm_fb),
        .CLKFBOUT (mmcm_fb),
        .CLKOUT0  (clk_100),
        .LOCKED   (mmcm_locked),
        .PWRDWN   (1'b0),
        .RST      (~sys_rst_n)
    );

    wire sys_clk_int;
    BUFG u_bufg_clk (.I(clk_100), .O(sys_clk_int));

    // Synchronised reset: external button AND MMCM locked
    reg [3:0] rst_shift = 4'b0000;
    wire      rst_n_sync = rst_shift[3];

    always @(posedge sys_clk_int) begin
        rst_shift <= {rst_shift[2:0], (sys_rst_n & mmcm_locked)};
    end

    // ========================================================================
    // Tie-offs for unconnected PHY interfaces
    // ========================================================================

    // External memory PHY — not connected for synthesis test
    wire [63:0] phy_rdata_tied  = 64'd0;
    wire        phy_rvalid_tied = 1'b0;
    wire        phy_ready_tied  = 1'b0;

    // NIC PHY — not connected for synthesis test
    wire        nic_tx_ready_tied = 1'b0;
    wire        nic_rx_valid_tied = 1'b0;
    wire [7:0]  nic_rx_data_tied  = 8'd0;
    wire        nic_link_up_tied  = 1'b0;

    // ========================================================================
    // SoC Instance
    // ========================================================================
    mp64_soc u_soc (
        .sys_clk    (sys_clk_int),
        .sys_rst_n  (rst_n_sync),

        // UART
        .uart_rxd   (uart_rxd),
        .uart_txd   (uart_txd),

        // External memory (tied off)
        .phy_req       (),
        .phy_addr      (),
        .phy_wen       (),
        .phy_wdata     (),
        .phy_burst_len (),
        .phy_rdata     (phy_rdata_tied),
        .phy_rvalid    (phy_rvalid_tied),
        .phy_ready     (phy_ready_tied),

        // SD Card
        .sd_sck     (sd_sck),
        .sd_mosi    (sd_mosi),
        .sd_miso    (sd_miso),
        .sd_cs_n    (sd_cs_n),

        // NIC (tied off)
        .nic_tx_valid (),
        .nic_tx_data  (),
        .nic_tx_ready (nic_tx_ready_tied),
        .nic_rx_valid (nic_rx_valid_tied),
        .nic_rx_data  (nic_rx_data_tied),
        .nic_rx_ready (),
        .nic_link_up  (nic_link_up_tied),

        // Debug LEDs
        .debug_leds (debug_leds)
    );

endmodule
