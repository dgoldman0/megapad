// ============================================================================
// mp64_platform_sim.v — Simulation Platform Wrapper
// ============================================================================
//
// Minimal wrapper for iverilog / Verilator / VCS simulation.
//   - Generates clock internally
//   - Generates reset pulse
//   - Passes clock and synchronised reset to mp64_top
//   - Ties off external PHY and NIC interfaces
//   - Provides UART loopback for basic testing
//
// This is the testbench-facing entry point for simulation.
// Individual unit testbenches may instantiate lower-level modules directly.
//
// Coding standard: Verilog-2001.
//

module mp64_platform_sim #(
    parameter CLOCK_HZ          = 100_000_000,
    parameter CLK_PERIOD_NS     = 10,           // 100 MHz = 10 ns
    parameter RESET_CYCLES      = 16,           // hold reset for N cycles
    parameter NUM_CORES         = 4,
    parameter NUM_CLUSTERS      = 3,
    parameter CORES_PER_CLUSTER = 4
)(
    // No external ports — this is a self-contained simulation wrapper.
    // Testbenches access internal signals via hierarchical references.
    output wire        sim_clk,
    output wire        sim_rst_n,
    output wire [7:0]  sim_debug_leds,
    output wire        sim_uart_txd
);

    `include "mp64_pkg.vh"

    // ========================================================================
    // Clock generation
    // ========================================================================
    reg clk_r = 1'b0;
    always #(CLK_PERIOD_NS / 2) clk_r = ~clk_r;
    assign sim_clk = clk_r;

    // ========================================================================
    // Reset generation — async assert, sync de-assert via mp64_rst_sync
    // ========================================================================
    reg rst_n_async = 1'b0;
    wire rst_n_sync;

    initial begin
        rst_n_async = 1'b0;
        #(CLK_PERIOD_NS * RESET_CYCLES);
        rst_n_async = 1'b1;
    end

    mp64_rst_sync #(
        .SYNC_STAGES(4)
    ) u_rst_sync (
        .clk       (clk_r),
        .rst_n_in  (rst_n_async),
        .rst_n_out (rst_n_sync)
    );

    assign sim_rst_n = rst_n_sync;

    // ========================================================================
    // Tie-offs for unconnected interfaces
    // ========================================================================
    wire [63:0] phy_rdata_tied  = 64'd0;
    wire        phy_rvalid_tied = 1'b0;
    wire        phy_ready_tied  = 1'b0;

    wire        nic_tx_ready_tied = 1'b0;
    wire        nic_rx_valid_tied = 1'b0;
    wire [7:0]  nic_rx_data_tied  = 8'd0;
    wire        nic_link_up_tied  = 1'b0;

    // UART — loopback TX→RX for smoke testing
    wire uart_txd_w;
    assign sim_uart_txd = uart_txd_w;

    // ========================================================================
    // SoC top-level
    // ========================================================================
    mp64_top #(
        .CLOCK_HZ          (CLOCK_HZ),
        .NUM_CORES          (NUM_CORES),
        .NUM_CLUSTERS       (NUM_CLUSTERS),
        .CORES_PER_CLUSTER  (CORES_PER_CLUSTER)
    ) u_top (
        .clk            (clk_r),
        .rst_n          (rst_n_sync),

        // UART (loopback)
        .uart_rxd       (uart_txd_w),
        .uart_txd       (uart_txd_w),

        // SD (unused)
        .sd_sck         (),
        .sd_mosi        (),
        .sd_miso        (1'b1),
        .sd_cs_n        (),

        // External memory (tied off)
        .phy_req        (),
        .phy_addr       (),
        .phy_wen        (),
        .phy_wdata      (),
        .phy_burst_len  (),
        .phy_rdata      (phy_rdata_tied),
        .phy_rvalid     (phy_rvalid_tied),
        .phy_ready      (phy_ready_tied),

        // NIC (tied off)
        .nic_tx_valid   (),
        .nic_tx_data    (),
        .nic_tx_ready   (nic_tx_ready_tied),
        .nic_rx_valid   (nic_rx_valid_tied),
        .nic_rx_data    (nic_rx_data_tied),
        .nic_rx_ready   (),
        .nic_link_up    (nic_link_up_tied),

        // Debug
        .debug_leds     (sim_debug_leds)
    );

endmodule
