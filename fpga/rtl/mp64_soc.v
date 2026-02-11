// ============================================================================
// mp64_soc.v — Megapad-64 System-on-Chip Top Level
// ============================================================================
//
// Wires together:
//   CPU → Bus Arbiter → Memory (dual-port BRAM + external)
//                     → MMIO   (UART, Timer, Disk, NIC)
//   Tile Engine → Memory Port A (512-bit fast path)
//              → External Memory (burst path)
//   Disk DMA → Memory write port
//   NIC  DMA → Memory write port
//
// Clock domain: single clock (sys_clk), active-low reset (sys_rst_n).
//

`include "mp64_defs.vh"

module mp64_soc (
    input  wire        sys_clk,
    input  wire        sys_rst_n,

    // === External UART pins ===
    input  wire        uart_rxd,
    output wire        uart_txd,

    // === External memory PHY ===
    output wire        phy_req,
    output wire [23:0] phy_addr,
    output wire        phy_wen,
    output wire [63:0] phy_wdata,
    output wire [3:0]  phy_burst_len,
    input  wire [63:0] phy_rdata,
    input  wire        phy_rvalid,
    input  wire        phy_ready,

    // === SPI-SD pins ===
    output wire        sd_sck,
    output wire        sd_mosi,
    input  wire        sd_miso,
    output wire        sd_cs_n,
    input  wire        sd_present,

    // === NIC PHY pins ===
    output wire        nic_tx_valid,
    output wire [7:0]  nic_tx_data,
    input  wire        nic_tx_ready,
    input  wire        nic_rx_valid,
    input  wire [7:0]  nic_rx_data,
    output wire        nic_rx_ready,

    // === Debug ===
    output wire [7:0]  debug_leds
);

    // ========================================================================
    // Internal wires
    // ========================================================================

    // --- CPU ↔ Bus ---
    wire        cpu_bus_valid;
    wire [63:0] cpu_bus_addr;
    wire [63:0] cpu_bus_wdata;
    wire        cpu_bus_wen;
    wire [1:0]  cpu_bus_size;
    wire [63:0] cpu_bus_rdata;
    wire        cpu_bus_ready;

    // --- Bus ↔ Memory (CPU port B) ---
    wire        mem_b_en;
    wire [63:0] mem_b_addr;
    wire [63:0] mem_b_wdata;
    wire        mem_b_wen;
    wire [1:0]  mem_b_size;
    wire [63:0] mem_b_rdata;
    wire        mem_b_ready;
    wire        mem_b_ext_fwd;
    wire [63:0] mem_b_ext_addr;

    // --- Bus ↔ MMIO ---
    wire        mmio_en;
    wire [11:0] mmio_addr;
    wire [63:0] mmio_wdata;
    wire        mmio_wen;
    wire [63:0] mmio_rdata;
    wire        mmio_ready;

    // --- Tile engine ↔ Memory (Port A) ---
    wire        tile_a_en;
    wire [16:0] tile_a_addr;
    wire [511:0] tile_a_wdata;
    wire        tile_a_wen;
    wire [511:0] tile_a_rdata;

    // --- Tile engine ↔ External memory ---
    wire        tile_ext_req;
    wire [63:0] tile_ext_addr;
    wire        tile_ext_wen;
    wire [511:0] tile_ext_wdata;
    wire [511:0] tile_ext_rdata;
    wire        tile_ext_done;
    wire        tile_ext_busy;

    // --- CPU ↔ External memory ---
    wire        cpu_ext_req;
    wire [63:0] cpu_ext_addr;
    wire        cpu_ext_wen;
    wire [63:0] cpu_ext_wdata;
    wire [63:0] cpu_ext_rdata;
    wire        cpu_ext_done;

    // --- CPU ↔ Tile CSR ---
    wire        csr_wen;
    wire [7:0]  csr_addr;
    wire [63:0] csr_wdata;
    wire [63:0] csr_rdata;

    // --- CPU ↔ Tile MEX dispatch ---
    wire        mex_valid;
    wire [1:0]  mex_ss;
    wire [1:0]  mex_op;
    wire [2:0]  mex_funct;
    wire [63:0] mex_gpr_val;
    wire [7:0]  mex_imm8;
    wire        mex_done;
    wire        mex_busy;

    // --- Interrupts ---
    wire        irq_timer;
    wire        irq_uart;
    wire        irq_nic;

    // --- Disk DMA ---
    wire        disk_dma_en;
    wire [19:0] disk_dma_addr;
    wire [7:0]  disk_dma_wdata;
    wire        disk_dma_wen;
    wire [7:0]  disk_dma_rdata;

    // --- NIC DMA ---
    wire        nic_dma_en;
    wire [19:0] nic_dma_addr;
    wire [7:0]  nic_dma_wdata;
    wire        nic_dma_wen;
    wire [7:0]  nic_dma_rdata;

    // --- MMIO demux ---
    // UART: 0x000–0x0FF, Timer: 0x100–0x1FF, Disk: 0x200–0x2FF
    // SysInfo: 0x300–0x3FF, NIC: 0x400–0x4FF
    wire        uart_en   = mmio_en && (mmio_addr[11:8] == 4'h0);
    wire        timer_en  = mmio_en && (mmio_addr[11:8] == 4'h1);
    wire        disk_en   = mmio_en && (mmio_addr[11:8] == 4'h2);
    wire        sysinfo_en= mmio_en && (mmio_addr[11:8] == 4'h3);
    wire        nic_en    = mmio_en && (mmio_addr[11:8] == 4'h4);

    wire [63:0] uart_rdata;
    wire [63:0] timer_rdata;
    wire [63:0] disk_rdata;
    wire [63:0] nic_rdata;
    wire [63:0] sysinfo_rdata;

    // MMIO read mux
    assign mmio_rdata = uart_en   ? uart_rdata   :
                        timer_en  ? timer_rdata  :
                        disk_en   ? disk_rdata   :
                        nic_en    ? nic_rdata    :
                        sysinfo_en? sysinfo_rdata:
                        64'd0;
    assign mmio_ready = 1'b1;  // all MMIO peripherals are single-cycle

    // SysInfo: read-only system identification
    assign sysinfo_rdata = (mmio_addr[7:0] == 8'h00) ? 64'h4D50_3634_0001_0001 :  // "MP64" v1.1
                           (mmio_addr[7:0] == 8'h08) ? {44'd0, 20'd1048576}     :  // RAM size
                           64'd0;

    // ========================================================================
    // CPU
    // ========================================================================
    mp64_cpu u_cpu (
        .clk        (sys_clk),
        .rst_n      (sys_rst_n),
        .bus_valid  (cpu_bus_valid),
        .bus_addr   (cpu_bus_addr),
        .bus_wdata  (cpu_bus_wdata),
        .bus_wen    (cpu_bus_wen),
        .bus_size   (cpu_bus_size),
        .bus_rdata  (cpu_bus_rdata),
        .bus_ready  (cpu_bus_ready),
        .csr_wen    (csr_wen),
        .csr_addr   (csr_addr),
        .csr_wdata  (csr_wdata),
        .csr_rdata  (csr_rdata),
        .mex_valid  (mex_valid),
        .mex_ss     (mex_ss),
        .mex_op     (mex_op),
        .mex_funct  (mex_funct),
        .mex_gpr_val(mex_gpr_val),
        .mex_imm8   (mex_imm8),
        .mex_done   (mex_done),
        .mex_busy   (mex_busy),
        .irq_timer  (irq_timer),
        .irq_uart   (irq_uart),
        .irq_nic    (irq_nic)
    );

    // ========================================================================
    // Bus Arbiter & Address Decoder
    // ========================================================================
    mp64_bus u_bus (
        .clk        (sys_clk),
        .rst_n      (sys_rst_n),
        .cpu_valid  (cpu_bus_valid),
        .cpu_addr   (cpu_bus_addr),
        .cpu_wdata  (cpu_bus_wdata),
        .cpu_wen    (cpu_bus_wen),
        .cpu_size   (cpu_bus_size),
        .cpu_rdata  (cpu_bus_rdata),
        .cpu_ready  (cpu_bus_ready),
        .mem_en     (mem_b_en),
        .mem_addr   (mem_b_addr),
        .mem_wdata  (mem_b_wdata),
        .mem_wen    (mem_b_wen),
        .mem_size   (mem_b_size),
        .mem_rdata  (mem_b_rdata),
        .mem_ready  (mem_b_ready),
        .mmio_en    (mmio_en),
        .mmio_addr  (mmio_addr),
        .mmio_wdata (mmio_wdata),
        .mmio_wen   (mmio_wen),
        .mmio_rdata (mmio_rdata),
        .mmio_ready (mmio_ready)
    );

    // ========================================================================
    // Dual-Port Memory (1 MiB internal BRAM)
    // ========================================================================
    mp64_memory u_memory (
        .clk          (sys_clk),
        .rst_n        (sys_rst_n),
        // Port A: Tile engine (512-bit)
        .tile_en      (tile_a_en),
        .tile_addr    (tile_a_addr),
        .tile_wdata   (tile_a_wdata),
        .tile_wen     (tile_a_wen),
        .tile_rdata   (tile_a_rdata),
        // Port B: CPU (64-bit)
        .cpu_en       (mem_b_en),
        .cpu_addr     (mem_b_addr),
        .cpu_wdata    (mem_b_wdata),
        .cpu_wen      (mem_b_wen),
        .cpu_size     (mem_b_size),
        .cpu_rdata    (mem_b_rdata),
        .cpu_ready    (mem_b_ready),
        .cpu_ext_fwd  (mem_b_ext_fwd),
        .cpu_ext_addr (mem_b_ext_addr)
    );

    // ========================================================================
    // Tile Engine
    // ========================================================================
    mp64_tile u_tile (
        .clk        (sys_clk),
        .rst_n      (sys_rst_n),
        // CSR interface
        .csr_wen    (csr_wen),
        .csr_addr   (csr_addr),
        .csr_wdata  (csr_wdata),
        .csr_rdata  (csr_rdata),
        // MEX dispatch
        .mex_valid  (mex_valid),
        .mex_ss     (mex_ss),
        .mex_op     (mex_op),
        .mex_funct  (mex_funct),
        .mex_gpr_val(mex_gpr_val),
        .mex_imm8   (mex_imm8),
        .mex_done   (mex_done),
        .mex_busy   (mex_busy),
        // Internal BRAM (Port A)
        .mem_en     (tile_a_en),
        .mem_addr   (tile_a_addr),
        .mem_wdata  (tile_a_wdata),
        .mem_wen    (tile_a_wen),
        .mem_rdata  (tile_a_rdata),
        // External memory path
        .ext_req    (tile_ext_req),
        .ext_addr   (tile_ext_addr),
        .ext_wen    (tile_ext_wen),
        .ext_wdata  (tile_ext_wdata),
        .ext_rdata  (tile_ext_rdata),
        .ext_done   (tile_ext_done)
    );

    // ========================================================================
    // External Memory Controller
    // ========================================================================
    mp64_extmem u_extmem (
        .clk            (sys_clk),
        .rst_n          (sys_rst_n),
        // CPU port
        .cpu_req        (mem_b_ext_fwd),
        .cpu_addr       (mem_b_ext_addr),
        .cpu_wen        (mem_b_wen),
        .cpu_wdata      (mem_b_wdata),
        .cpu_rdata      (cpu_ext_rdata),
        .cpu_done       (cpu_ext_done),
        // Tile burst port
        .tile_req       (tile_ext_req),
        .tile_addr      (tile_ext_addr),
        .tile_wen       (tile_ext_wen),
        .tile_wdata     (tile_ext_wdata),
        .tile_rdata     (tile_ext_rdata),
        .tile_done      (tile_ext_done),
        // PHY interface
        .phy_req        (phy_req),
        .phy_addr       (phy_addr),
        .phy_wen        (phy_wen),
        .phy_wdata      (phy_wdata),
        .phy_burst_len  (phy_burst_len),
        .phy_rdata      (phy_rdata),
        .phy_rvalid     (phy_rvalid),
        .phy_ready      (phy_ready)
    );

    // ========================================================================
    // UART
    // ========================================================================
    mp64_uart u_uart (
        .clk        (sys_clk),
        .rst_n      (sys_rst_n),
        .sel        (uart_en),
        .addr       (mmio_addr[7:0]),
        .wdata      (mmio_wdata),
        .wen        (mmio_wen),
        .rdata      (uart_rdata),
        .rxd        (uart_rxd),
        .txd        (uart_txd),
        .irq        (irq_uart)
    );

    // ========================================================================
    // Timer
    // ========================================================================
    mp64_timer u_timer (
        .clk        (sys_clk),
        .rst_n      (sys_rst_n),
        .sel        (timer_en),
        .addr       (mmio_addr[7:0]),
        .wdata      (mmio_wdata),
        .wen        (mmio_wen),
        .rdata      (timer_rdata),
        .irq        (irq_timer)
    );

    // ========================================================================
    // Disk (SPI-SD)
    // ========================================================================
    mp64_disk u_disk (
        .clk        (sys_clk),
        .rst_n      (sys_rst_n),
        .sel        (disk_en),
        .addr       (mmio_addr[7:0]),
        .wdata      (mmio_wdata),
        .wen        (mmio_wen),
        .rdata      (disk_rdata),
        .sd_sck     (sd_sck),
        .sd_mosi    (sd_mosi),
        .sd_miso    (sd_miso),
        .sd_cs_n    (sd_cs_n),
        .sd_present (sd_present),
        .dma_en     (disk_dma_en),
        .dma_addr   (disk_dma_addr),
        .dma_wdata  (disk_dma_wdata),
        .dma_wen    (disk_dma_wen),
        .dma_rdata  (disk_dma_rdata)
    );

    // ========================================================================
    // NIC
    // ========================================================================
    mp64_nic u_nic (
        .clk        (sys_clk),
        .rst_n      (sys_rst_n),
        .sel        (nic_en),
        .addr       (mmio_addr[7:0]),
        .wdata      (mmio_wdata),
        .wen        (mmio_wen),
        .rdata      (nic_rdata),
        .irq        (irq_nic),
        .tx_valid   (nic_tx_valid),
        .tx_data    (nic_tx_data),
        .tx_ready   (nic_tx_ready),
        .rx_valid   (nic_rx_valid),
        .rx_data    (nic_rx_data),
        .rx_ready   (nic_rx_ready),
        .dma_en     (nic_dma_en),
        .dma_addr   (nic_dma_addr),
        .dma_wdata  (nic_dma_wdata),
        .dma_wen    (nic_dma_wen),
        .dma_rdata  (nic_dma_rdata)
    );

    // ========================================================================
    // DMA Arbitration (Disk + NIC write to memory via Port B side-channel)
    // ========================================================================
    // In this prototype, DMA writes share the CPU port when CPU is idle.
    // A production design would add a third memory port or use a DMA arbiter.
    // For now, DMA is handled within each peripheral's state machine.

    // ========================================================================
    // Debug LEDs — show CPU state
    // ========================================================================
    assign debug_leds = {mex_busy, irq_timer, cpu_bus_valid, 1'b0,
                         cpu_bus_wen, cpu_bus_size};

endmodule
