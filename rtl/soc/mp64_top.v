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
    parameter ICACHE_LINE_W     = 128,       // bits per I-cache line
    parameter [31:0] DISK_TOTAL_SECTORS = 32'd65536,
    parameter [31:0] SETTINGS_TOTAL_SECTORS = 32'd2048,
    parameter [31:0] SYSTEM_TOTAL_SECTORS = 32'd65536,
    parameter [31:0] USER_TOTAL_SECTORS = 32'd65536
)(
    input  wire        clk,
    input  wire        rst_n,

    // === UART ===
    input  wire        uart_rxd,
    output wire        uart_txd,

    // === SD card (SPI): the boot disk ===
    output wire        sd_sck,
    output wire        sd_mosi,
    input  wire        sd_miso,
    output wire        sd_cs_n,
    input  wire        sd_card_present,
    input  wire        sd_write_protected,

    // === Settings SD card (SPI): the settings disk, always write-protected ===
    output wire        sd1_sck,
    output wire        sd1_mosi,
    input  wire        sd1_miso,
    output wire        sd1_cs_n,
    input  wire        sd1_card_present,

    // === System SD card (SPI): the system disk, always write-protected ===
    output wire        sd2_sck,
    output wire        sd2_mosi,
    input  wire        sd2_miso,
    output wire        sd2_cs_n,
    input  wire        sd2_card_present,

    // === User SD card (SPI): the user disk ===
    output wire        sd3_sck,
    output wire        sd3_mosi,
    input  wire        sd3_miso,
    output wire        sd3_cs_n,
    input  wire        sd3_card_present,
    input  wire        sd3_write_protected,

    // === External memory PHY ===
    output wire        phy_req,
    output wire [63:0] phy_addr,
    output wire        phy_wen,
    output wire [63:0] phy_wdata,
    output wire [7:0]  phy_burst_len,
    output wire        phy_cancel,
    input  wire [63:0] phy_rdata,
    input  wire        phy_rvalid,
    input  wire        phy_ready,
    input  wire        phy_error,
    input  wire        phy_cancel_done,

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
        .MEM_DEPTH          (MEM_DEPTH),
        .DISK_TOTAL_SECTORS (DISK_TOTAL_SECTORS),
        .SETTINGS_TOTAL_SECTORS (SETTINGS_TOTAL_SECTORS),
        .SYSTEM_TOTAL_SECTORS (SYSTEM_TOTAL_SECTORS),
        .USER_TOTAL_SECTORS (USER_TOTAL_SECTORS)
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
        .phy_cancel    (phy_cancel),
        .phy_rdata     (phy_rdata),
        .phy_rvalid    (phy_rvalid),
        .phy_ready     (phy_ready),
        .phy_error     (phy_error),
        .phy_cancel_done(phy_cancel_done),

        // SD Card
        .sd_sck        (sd_sck),
        .sd_mosi       (sd_mosi),
        .sd_miso       (sd_miso),
        .sd_cs_n       (sd_cs_n),
        .sd_card_present      (sd_card_present),
        .sd_write_protected   (sd_write_protected),

        // Settings SD card
        .sd1_sck       (sd1_sck),
        .sd1_mosi      (sd1_mosi),
        .sd1_miso      (sd1_miso),
        .sd1_cs_n      (sd1_cs_n),
        .sd1_card_present (sd1_card_present),

        // System SD card
        .sd2_sck       (sd2_sck),
        .sd2_mosi      (sd2_mosi),
        .sd2_miso      (sd2_miso),
        .sd2_cs_n      (sd2_cs_n),
        .sd2_card_present (sd2_card_present),

        // User SD card
        .sd3_sck       (sd3_sck),
        .sd3_mosi      (sd3_mosi),
        .sd3_miso      (sd3_miso),
        .sd3_cs_n      (sd3_cs_n),
        .sd3_card_present (sd3_card_present),
        .sd3_write_protected (sd3_write_protected),

        // NIC PHY
        .nic_tx_valid  (nic_tx_valid),
        .nic_tx_data   (nic_tx_data),
        .nic_tx_ready  (nic_tx_ready),
        .nic_rx_valid  (nic_rx_valid),
        .nic_rx_data   (nic_rx_data),
        .nic_rx_ready  (nic_rx_ready),
        .nic_link_up   (nic_link_up),

        // Chiplet link: no chiplet is attached here, so the port stays idle.
        .cl_rq_valid   (1'b0),
        .cl_rq_ready   (),
        .cl_rq_addr    (64'd0),
        .cl_rq_wdata   (64'd0),
        .cl_rq_wen     (1'b0),
        .cl_rq_size    (2'd0),
        .cl_rq_len     (8'd0),
        .cl_rq_cancel  (1'b0),
        .cl_rs_valid   (),
        .cl_rs_ready   (1'b0),
        .cl_rs_rdata   (),
        .cl_rs_code    (),
        .cl_rs_last    (),
        .cl_dbt_valid  (),
        .cl_dbt_ready  (1'b0),
        .cl_dbt_src    (),
        .cl_dbr_valid  (1'b0),
        .cl_dbr_ready  (),
        .cl_dbr_dst    (8'd0),
        .cl_sb_present (1'b0),
        .cl_sb_rst_n   (),
        .cl_sb_stop_req(),
        .cl_sb_stopped (1'b0),
        .cl_sb_ready   (1'b0),
        .cl_sb_id_req  (),
        .cl_sb_id_index(),
        .cl_sb_id_ack  (1'b0),
        .cl_sb_id_data (64'd0),
        .cl_sb_boot_addr(),
        .cl_sb_boot_len(),

        // Debug
        .debug_leds    (debug_leds)
    );

endmodule
