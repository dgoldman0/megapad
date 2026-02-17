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
    // SoC instance (Phase 7 — placeholder stub)
    // ========================================================================
    // The full mp64_soc will be built in later phases.  For Phase 1 we
    // verify that the top-level compiles and the parameter interface is
    // correct.  A minimal stub drives safe defaults on all outputs.

    // UART
    assign uart_txd     = 1'b1;   // idle high

    // SD Card
    assign sd_sck       = 1'b0;
    assign sd_mosi      = 1'b0;
    assign sd_cs_n      = 1'b1;   // deselected

    // External memory
    assign phy_req      = 1'b0;
    assign phy_addr     = 64'd0;
    assign phy_wen      = 1'b0;
    assign phy_wdata    = 64'd0;
    assign phy_burst_len= 8'd0;

    // NIC
    assign nic_tx_valid = 1'b0;
    assign nic_tx_data  = 8'd0;
    assign nic_rx_ready = 1'b0;

    // Debug LEDs — show reset state
    reg [7:0] led_r;
    always @(posedge clk) begin
        if (!rst_n)
            led_r <= 8'h00;
        else
            led_r <= 8'hA5;   // heartbeat pattern
    end
    assign debug_leds = led_r;

endmodule
