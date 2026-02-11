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
    output wire [31:0] phy_addr,
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

    // === NIC PHY pins ===
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

    // --- Bus ↔ Memory (CPU port) ---
    wire        mem_req;
    wire [63:0] mem_addr;
    wire [63:0] mem_wdata;
    wire        mem_wen;
    wire [1:0]  mem_size;
    wire [63:0] mem_rdata;
    wire        mem_ack;

    // --- Bus ↔ MMIO ---
    wire        mmio_req;
    wire [11:0] mmio_addr;
    wire [63:0] mmio_wdata_bus;
    wire        mmio_wen;
    wire [1:0]  mmio_size;
    wire [63:0] mmio_rdata_bus;
    wire        mmio_ack;

    // --- Memory → External (forwarded out-of-range CPU accesses) ---
    wire        mem_ext_req;
    wire [63:0] mem_ext_addr;
    wire [63:0] mem_ext_wdata;
    wire        mem_ext_wen;
    wire [1:0]  mem_ext_size;
    wire [63:0] mem_ext_rdata;
    wire        mem_ext_ack;

    // --- Tile engine ↔ Memory (Port A) ---
    wire        tile_mem_req;
    wire [19:0] tile_mem_addr;
    wire [511:0] tile_mem_wdata;
    wire        tile_mem_wen;
    wire [511:0] tile_mem_rdata;
    wire        tile_mem_ack;

    // --- Tile engine ↔ External memory ---
    wire        tile_ext_req;
    wire [63:0] tile_ext_addr;
    wire        tile_ext_wen;
    wire [511:0] tile_ext_wdata;
    wire [511:0] tile_ext_rdata;
    wire        tile_ext_ack;

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
    wire        disk_dma_req;
    wire [63:0] disk_dma_addr;
    wire [7:0]  disk_dma_wdata;
    wire        disk_dma_wen;
    wire [7:0]  disk_dma_rdata;
    wire        disk_dma_ack;

    // --- NIC DMA ---
    wire        nic_dma_req;
    wire [63:0] nic_dma_addr;
    wire [7:0]  nic_dma_wdata;
    wire        nic_dma_wen;
    wire [7:0]  nic_dma_rdata;
    wire        nic_dma_ack;

    // --- Per-peripheral 8-bit read data ---
    wire [7:0]  uart_rdata8;
    wire [7:0]  timer_rdata8;
    wire [7:0]  disk_rdata8;
    wire [7:0]  nic_rdata8;

    // --- Per-peripheral ack (unused, all single-cycle) ---
    wire        uart_ack, timer_ack, disk_ack, nic_ack;

    // --- MMIO demux ---
    // UART: 0x000–0x0FF, Timer: 0x100–0x1FF, Disk: 0x200–0x2FF
    // SysInfo: 0x300–0x3FF, NIC: 0x400–0x4FF
    wire        uart_sel   = mmio_req && (mmio_addr[11:8] == 4'h0);
    wire        timer_sel  = mmio_req && (mmio_addr[11:8] == 4'h1);
    wire        disk_sel   = mmio_req && (mmio_addr[11:8] == 4'h2);
    wire        sysinfo_sel= mmio_req && (mmio_addr[11:8] == 4'h3);
    wire        nic_sel    = mmio_req && (mmio_addr[11:8] == 4'h4);

    // MMIO read mux — peripherals are 8-bit, zero-extend to 64
    wire [63:0] sysinfo_rdata;

    assign mmio_rdata_bus = uart_sel    ? {56'd0, uart_rdata8}  :
                            timer_sel   ? {56'd0, timer_rdata8} :
                            disk_sel    ? {56'd0, disk_rdata8}  :
                            nic_sel     ? {56'd0, nic_rdata8}   :
                            sysinfo_sel ? sysinfo_rdata         :
                            64'd0;
    assign mmio_ack = 1'b1;  // all MMIO peripherals are single-cycle

    // SysInfo: read-only system identification
    assign sysinfo_rdata = (mmio_addr[7:0] == 8'h00) ? 64'h4D50_3634_0001_0001 :  // "MP64" v1.1
                           (mmio_addr[7:0] == 8'h08) ? 64'd1048576               :  // RAM size
                           64'd0;

    // DMA ack stubs (prototype — DMA not yet integrated with memory arbiter)
    assign disk_dma_rdata = 8'd0;
    assign disk_dma_ack   = 1'b1;
    assign nic_dma_rdata  = 8'd0;
    assign nic_dma_ack    = 1'b1;

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
        // CPU master
        .cpu_valid  (cpu_bus_valid),
        .cpu_addr   (cpu_bus_addr),
        .cpu_wdata  (cpu_bus_wdata),
        .cpu_wen    (cpu_bus_wen),
        .cpu_size   (cpu_bus_size),
        .cpu_rdata  (cpu_bus_rdata),
        .cpu_ready  (cpu_bus_ready),
        // Memory port
        .mem_req    (mem_req),
        .mem_addr   (mem_addr),
        .mem_wdata  (mem_wdata),
        .mem_wen    (mem_wen),
        .mem_size   (mem_size),
        .mem_rdata  (mem_rdata),
        .mem_ack    (mem_ack),
        // MMIO port
        .mmio_req   (mmio_req),
        .mmio_addr  (mmio_addr),
        .mmio_wdata (mmio_wdata_bus),
        .mmio_wen   (mmio_wen),
        .mmio_size  (mmio_size),
        .mmio_rdata (mmio_rdata_bus),
        .mmio_ack   (mmio_ack)
    );

    // ========================================================================
    // Dual-Port Memory (1 MiB internal BRAM)
    // ========================================================================
    mp64_memory u_memory (
        .clk        (sys_clk),
        .rst_n      (sys_rst_n),
        // Port A: Tile engine (512-bit)
        .tile_req   (tile_mem_req),
        .tile_addr  (tile_mem_addr),
        .tile_wdata (tile_mem_wdata),
        .tile_wen   (tile_mem_wen),
        .tile_rdata (tile_mem_rdata),
        .tile_ack   (tile_mem_ack),
        // Port B: CPU (64-bit)
        .cpu_req    (mem_req),
        .cpu_addr   (mem_addr),
        .cpu_wdata  (mem_wdata),
        .cpu_wen    (mem_wen),
        .cpu_size   (mem_size),
        .cpu_rdata  (mem_rdata),
        .cpu_ack    (mem_ack),
        // External forwarding (CPU accesses above 1 MiB)
        .ext_req    (mem_ext_req),
        .ext_addr   (mem_ext_addr),
        .ext_wdata  (mem_ext_wdata),
        .ext_wen    (mem_ext_wen),
        .ext_size   (mem_ext_size),
        .ext_rdata  (mem_ext_rdata),
        .ext_ack    (mem_ext_ack)
    );

    // ========================================================================
    // Tile Engine
    // ========================================================================
    mp64_tile u_tile (
        .clk            (sys_clk),
        .rst_n          (sys_rst_n),
        // CSR interface
        .csr_wen        (csr_wen),
        .csr_addr       (csr_addr),
        .csr_wdata      (csr_wdata),
        .csr_rdata      (csr_rdata),
        // MEX dispatch
        .mex_valid      (mex_valid),
        .mex_ss         (mex_ss),
        .mex_op         (mex_op),
        .mex_funct      (mex_funct),
        .mex_gpr_val    (mex_gpr_val),
        .mex_imm8       (mex_imm8),
        .mex_done       (mex_done),
        .mex_busy       (mex_busy),
        // Internal BRAM (Port A)
        .tile_req       (tile_mem_req),
        .tile_addr      (tile_mem_addr),
        .tile_wen       (tile_mem_wen),
        .tile_wdata     (tile_mem_wdata),
        .tile_rdata     (tile_mem_rdata),
        .tile_ack       (tile_mem_ack),
        // External memory path
        .ext_tile_req   (tile_ext_req),
        .ext_tile_addr  (tile_ext_addr),
        .ext_tile_wen   (tile_ext_wen),
        .ext_tile_wdata (tile_ext_wdata),
        .ext_tile_rdata (tile_ext_rdata),
        .ext_tile_ack   (tile_ext_ack)
    );

    // ========================================================================
    // External Memory Controller
    // ========================================================================
    mp64_extmem u_extmem (
        .clk            (sys_clk),
        .rst_n          (sys_rst_n),
        // CPU port (forwarded from memory controller)
        .cpu_req        (mem_ext_req),
        .cpu_addr       (mem_ext_addr),
        .cpu_wen        (mem_ext_wen),
        .cpu_wdata      (mem_ext_wdata),
        .cpu_size       (mem_ext_size),
        .cpu_rdata      (mem_ext_rdata),
        .cpu_ack        (mem_ext_ack),
        // Tile burst port
        .tile_req       (tile_ext_req),
        .tile_addr      (tile_ext_addr),
        .tile_wen       (tile_ext_wen),
        .tile_wdata     (tile_ext_wdata),
        .tile_rdata     (tile_ext_rdata),
        .tile_ack       (tile_ext_ack),
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
        .clk    (sys_clk),
        .rst_n  (sys_rst_n),
        .req    (uart_sel),
        .addr   (mmio_addr[3:0]),
        .wdata  (mmio_wdata_bus[7:0]),
        .wen    (mmio_wen),
        .rdata  (uart_rdata8),
        .ack    (uart_ack),
        .irq    (irq_uart),
        .tx     (uart_txd),
        .rx     (uart_rxd)
    );

    // ========================================================================
    // Timer
    // ========================================================================
    mp64_timer u_timer (
        .clk    (sys_clk),
        .rst_n  (sys_rst_n),
        .req    (timer_sel),
        .addr   (mmio_addr[3:0]),
        .wdata  (mmio_wdata_bus[7:0]),
        .wen    (mmio_wen),
        .rdata  (timer_rdata8),
        .ack    (timer_ack),
        .irq    (irq_timer)
    );

    // ========================================================================
    // Disk (SPI-SD)
    // ========================================================================
    mp64_disk u_disk (
        .clk        (sys_clk),
        .rst_n      (sys_rst_n),
        .req        (disk_sel),
        .addr       (mmio_addr[3:0]),
        .wdata      (mmio_wdata_bus[7:0]),
        .wen        (mmio_wen),
        .rdata      (disk_rdata8),
        .ack        (disk_ack),
        .spi_clk    (sd_sck),
        .spi_mosi   (sd_mosi),
        .spi_miso   (sd_miso),
        .spi_cs_n   (sd_cs_n),
        .dma_req    (disk_dma_req),
        .dma_addr   (disk_dma_addr),
        .dma_wdata  (disk_dma_wdata),
        .dma_wen    (disk_dma_wen),
        .dma_rdata  (disk_dma_rdata),
        .dma_ack    (disk_dma_ack)
    );

    // ========================================================================
    // NIC
    // ========================================================================
    mp64_nic u_nic (
        .clk            (sys_clk),
        .rst_n          (sys_rst_n),
        .req            (nic_sel),
        .addr           (mmio_addr[6:0]),
        .wdata          (mmio_wdata_bus[7:0]),
        .wen            (mmio_wen),
        .rdata          (nic_rdata8),
        .ack            (nic_ack),
        .irq            (irq_nic),
        .phy_tx_valid   (nic_tx_valid),
        .phy_tx_data    (nic_tx_data),
        .phy_tx_ready   (nic_tx_ready),
        .phy_rx_valid   (nic_rx_valid),
        .phy_rx_data    (nic_rx_data),
        .phy_rx_ready   (nic_rx_ready),
        .phy_link_up    (nic_link_up),
        .dma_req        (nic_dma_req),
        .dma_addr       (nic_dma_addr),
        .dma_wdata      (nic_dma_wdata),
        .dma_wen        (nic_dma_wen),
        .dma_rdata      (nic_dma_rdata),
        .dma_ack        (nic_dma_ack)
    );

    // ========================================================================
    // Debug LEDs — show CPU state
    // ========================================================================
    assign debug_leds = {mex_busy, irq_timer, cpu_bus_valid, 1'b0,
                         cpu_bus_wen, cpu_bus_size};

endmodule
