// ============================================================================
// mp64_top.v — Megapad-64 Portable Top-Level
// ============================================================================
//
// This is the technology-agnostic top of the design.  It receives a
// clean clock and synchronous reset from the platform wrapper and
// instantiates the SoC.  No vendor primitives appear here or below.
//
// All system-level parameters flow from here down to every sub-module.
//
// Port interface — platform wrappers connect these:
//   clk       — system clock (100 MHz default)
//   rst_n     — synchronous active-low reset (already synchronised)
//   uart_*    — UART pins
//   sd_*      — SPI SD card pins
//   debug_*   — debug outputs (LEDs, etc.)
//
// External memory and NIC PHY interfaces are exposed for the platform
// to connect or tie off.
//
// Coding standard: Verilog-2001, no vendor primitives.
//

module mp64_top #(
    // ====================================================================
    // System parameters (override from platform wrapper)
    // ====================================================================
    parameter CLOCK_HZ          = 100_000_000,  // 100 MHz
    parameter NUM_CORES         = 4,
    parameter NUM_CLUSTERS      = 3,
    parameter CORES_PER_CLUSTER = 4,
    parameter MEM_DEPTH         = 16384,     // per-bank rows (×512 bits)
    parameter MEM_BANKS         = 4,
    parameter ICACHE_LINES      = 256,
    parameter ICACHE_LINE_W     = 128        // bits per I-cache line
)(
    input  wire        clk,
    input  wire        rst_n,

    // === UART ===
    input  wire        uart_rxd,
    output wire        uart_txd,

    // === SD Card (SPI) ===
    output wire        sd_sck,
    output wire        sd_mosi,
    input  wire        sd_miso,
    output wire        sd_cs_n,

    // === External memory PHY ===
    output wire        phy_req,
    output wire [63:0] phy_addr,
    output wire        phy_wen,
    output wire [63:0] phy_wdata,
    output wire [7:0]  phy_burst_len,
    input  wire [63:0] phy_rdata,
    input  wire        phy_rvalid,
    input  wire        phy_ready,

    // === NIC PHY ===
    output wire        nic_tx_valid,
    output wire [7:0]  nic_tx_data,
    input  wire        nic_tx_ready,
    input  wire        nic_rx_valid,
    input  wire [7:0]  nic_rx_data,
    output wire        nic_rx_ready,
    input  wire        nic_link_up,

    // === Debug ===
    output wire [7:0]  debug_leds
);

    `include "mp64_pkg.vh"

    // ========================================================================
    // SoC instance
    // ========================================================================

    mp64_soc #(
        .CLOCK_HZ          (CLOCK_HZ),
        .NUM_CORES          (NUM_CORES),
        .NUM_CLUSTERS       (NUM_CLUSTERS),
        .CORES_PER_CLUSTER  (CORES_PER_CLUSTER),
        .MEM_DEPTH          (MEM_DEPTH)
    ) u_soc (
        .sys_clk       (clk),
        .sys_rst_n     (rst_n),

        // UART
        .uart_rxd      (uart_rxd),
        .uart_txd      (uart_txd),

        // External memory PHY
        .phy_req       (phy_req),
        .phy_addr      (phy_addr),
        .phy_wen       (phy_wen),
        .phy_wdata     (phy_wdata),
        .phy_burst_len (phy_burst_len),
        .phy_rdata     (phy_rdata),
        .phy_rvalid    (phy_rvalid),
        .phy_ready     (phy_ready),

        // SD Card
        .sd_sck        (sd_sck),
        .sd_mosi       (sd_mosi),
        .sd_miso       (sd_miso),
        .sd_cs_n       (sd_cs_n),

        // NIC PHY
        .nic_tx_valid  (nic_tx_valid),
        .nic_tx_data   (nic_tx_data),
        .nic_tx_ready  (nic_tx_ready),
        .nic_rx_valid  (nic_rx_valid),
        .nic_rx_data   (nic_rx_data),
        .nic_rx_ready  (nic_rx_ready),
        .nic_link_up   (nic_link_up),

        // Debug
        .debug_leds    (debug_leds)
    );

endmodule
