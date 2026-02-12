// ============================================================================
// mp64_soc.v — Megapad-64 Quad-Core System-on-Chip Top Level
// ============================================================================
//
// Instantiates NUM_CORES (4) CPU+Tile pairs sharing:
//   - 1 MiB dual-port BRAM (Port A: tile arbiter, Port B: CPU arbiter)
//   - MMIO peripherals (UART, Timer, Disk, NIC, Mailbox, Spinlocks)
//   - External memory controller
//
// Multi-core arbitration:
//   - CPU Port B access: round-robin via mp64_bus (4 masters → 1 slave)
//   - Tile Port A access: round-robin via tile arbiter (4 tile engines → 1 port)
//   - MMIO: arbitrated through mp64_bus, single-cycle
//
// Boot protocol:
//   - All cores start from address 0x0000 on reset
//   - Core 0 reads CSR_COREID=0, proceeds with BIOS boot
//   - Cores 1–3 read CSR_COREID≠0, enter HALT (WFI) waiting for IPI
//   - Core 0 sets up per-core stacks, writes entry points to mailbox,
//     then sends IPI to wake secondary cores
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
    // Per-core wires (generated for NUM_CORES)
    // ========================================================================

    // --- CPU ↔ Bus (flat-packed for arbiter) ---
    wire [NUM_CORES-1:0]        cpu_bus_valid;
    wire [NUM_CORES*64-1:0]     cpu_bus_addr;
    wire [NUM_CORES*64-1:0]     cpu_bus_wdata;
    wire [NUM_CORES-1:0]        cpu_bus_wen;
    wire [NUM_CORES*2-1:0]      cpu_bus_size;
    wire [NUM_CORES*64-1:0]     cpu_bus_rdata;
    wire [NUM_CORES-1:0]        cpu_bus_ready;

    // --- Per-core tile engine wires (need arbiter for Port A) ---
    wire [NUM_CORES-1:0]        tile_int_req;
    wire [NUM_CORES*20-1:0]     tile_int_addr;
    wire [NUM_CORES-1:0]        tile_int_wen;
    wire [NUM_CORES*512-1:0]    tile_int_wdata;
    wire [NUM_CORES*512-1:0]    tile_int_rdata;
    wire [NUM_CORES-1:0]        tile_int_ack;

    // --- Per-core tile external memory wires ---
    wire [NUM_CORES-1:0]        tile_ext_req;
    wire [NUM_CORES*64-1:0]     tile_ext_addr;
    wire [NUM_CORES-1:0]        tile_ext_wen;
    wire [NUM_CORES*512-1:0]    tile_ext_wdata;
    wire [NUM_CORES*512-1:0]    tile_ext_rdata;
    wire [NUM_CORES-1:0]        tile_ext_ack;

    // --- Per-core CSR + MEX (each core has its own tile engine) ---
    wire [NUM_CORES-1:0]        core_csr_wen;
    wire [NUM_CORES*8-1:0]      core_csr_addr;
    wire [NUM_CORES*64-1:0]     core_csr_wdata;
    wire [NUM_CORES*64-1:0]     core_csr_rdata;

    wire [NUM_CORES-1:0]        core_mex_valid;
    wire [NUM_CORES*2-1:0]      core_mex_ss;
    wire [NUM_CORES*2-1:0]      core_mex_op;
    wire [NUM_CORES*3-1:0]      core_mex_funct;
    wire [NUM_CORES*64-1:0]     core_mex_gpr_val;
    wire [NUM_CORES*8-1:0]      core_mex_imm8;
    wire [NUM_CORES*4-1:0]      core_mex_ext_mod;
    wire [NUM_CORES-1:0]        core_mex_ext_active;
    wire [NUM_CORES-1:0]        core_mex_done;
    wire [NUM_CORES-1:0]        core_mex_busy;

    // --- Per-core IPI ---
    wire [NUM_CORES-1:0]        ipi_lines;

    // --- Per-core CSR-side IPI interface (CPU → mailbox, bypasses bus) ---
    wire [NUM_CORES-1:0]        csr_ipi_wen;
    wire [NUM_CORES*8-1:0]      csr_ipi_addr;
    wire [NUM_CORES*64-1:0]     csr_ipi_wdata;
    wire [NUM_CORES*64-1:0]     csr_ipi_rdata;

    // Per-core tile CSR rdata (before IPI mux)
    wire [NUM_CORES*64-1:0]     tile_csr_rdata;

    // --- Shared bus wires ---
    wire        mem_req;
    wire [63:0] mem_addr;
    wire [63:0] mem_wdata;
    wire        mem_wen;
    wire [1:0]  mem_size;
    wire [63:0] mem_rdata;
    wire        mem_ack;

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

    // --- Tile Port A (shared, arbitrated) ---
    wire        tile_arb_req;
    wire [19:0] tile_arb_addr;
    wire        tile_arb_wen;
    wire [511:0] tile_arb_wdata;
    wire [511:0] tile_arb_rdata;
    wire        tile_arb_ack;

    // --- External tile (arbitrated to single ext port) ---
    wire        tile_ext_arb_req;
    wire [63:0] tile_ext_arb_addr;
    wire        tile_ext_arb_wen;
    wire [511:0] tile_ext_arb_wdata;
    wire [511:0] tile_ext_arb_rdata;
    wire        tile_ext_arb_ack;

    // --- Interrupts (from peripherals) ---
    wire        irq_timer;
    wire        irq_uart;
    wire        irq_nic;

    // --- Per-peripheral 8-bit read data ---
    wire [7:0]  uart_rdata8;
    wire [7:0]  timer_rdata8;
    wire [7:0]  disk_rdata8;
    wire [7:0]  nic_rdata8;
    wire [7:0]  mbox_rdata8;

    wire        uart_ack, timer_ack, disk_ack, nic_ack, mbox_ack;

    // --- Disk & NIC DMA ---
    wire        disk_dma_req;
    wire [63:0] disk_dma_addr;
    wire [7:0]  disk_dma_wdata;
    wire        disk_dma_wen;
    wire [7:0]  disk_dma_rdata;
    wire        disk_dma_ack;

    wire        nic_dma_req;
    wire [63:0] nic_dma_addr;
    wire [7:0]  nic_dma_wdata;
    wire        nic_dma_wen;
    wire [7:0]  nic_dma_rdata;
    wire        nic_dma_ack;

    // ========================================================================
    // MMIO Address Demux
    // ========================================================================
    wire uart_sel    = mmio_req && (mmio_addr[11:8] == 4'h0);
    wire timer_sel   = mmio_req && (mmio_addr[11:8] == 4'h1);
    wire disk_sel    = mmio_req && (mmio_addr[11:8] == 4'h2);
    wire sysinfo_sel = mmio_req && (mmio_addr[11:8] == 4'h3);
    wire nic_sel     = mmio_req && (mmio_addr[11:8] == 4'h4);
    wire mbox_sel    = mmio_req && (mmio_addr[11:8] == 4'h5 ||
                                    mmio_addr[11:8] == 4'h6);  // mailbox + spinlocks

    // MMIO read mux
    wire [63:0] sysinfo_rdata;

    assign mmio_rdata_bus = uart_sel    ? {56'd0, uart_rdata8}  :
                            timer_sel   ? {56'd0, timer_rdata8} :
                            disk_sel    ? {56'd0, disk_rdata8}  :
                            nic_sel     ? {56'd0, nic_rdata8}   :
                            mbox_sel    ? {56'd0, mbox_rdata8}  :
                            sysinfo_sel ? sysinfo_rdata         :
                            64'd0;
    assign mmio_ack = 1'b1;  // all MMIO peripherals are single-cycle

    // SysInfo: now includes core count
    assign sysinfo_rdata = (mmio_addr[7:0] == 8'h00) ? 64'h4D50_3634_0002_0001 :  // "MP64" v2.1 (multi-core)
                           (mmio_addr[7:0] == 8'h08) ? 64'd1048576               :  // RAM size
                           (mmio_addr[7:0] == 8'h10) ? {62'd0, NUM_CORES[1:0]}   :  // Core count
                           64'd0;

    // DMA ack stubs
    assign disk_dma_rdata = 8'd0;
    assign disk_dma_ack   = 1'b1;
    assign nic_dma_rdata  = 8'd0;
    assign nic_dma_ack    = 1'b1;

    // ========================================================================
    // Generate: NUM_CORES × (CPU + Tile Engine)
    // ========================================================================
    genvar c;
    generate
        for (c = 0; c < NUM_CORES; c = c + 1) begin : core

            // ----------------------------------------------------------------
            // CPU core
            // ----------------------------------------------------------------
            mp64_cpu u_cpu (
                .clk        (sys_clk),
                .rst_n      (sys_rst_n),
                .core_id    (c[CORE_ID_BITS-1:0]),

                // Bus master — packed into flat arrays
                .bus_valid  (cpu_bus_valid[c]),
                .bus_addr   (cpu_bus_addr [c*64 +: 64]),
                .bus_wdata  (cpu_bus_wdata[c*64 +: 64]),
                .bus_wen    (cpu_bus_wen  [c]),
                .bus_size   (cpu_bus_size [c*2  +: 2]),
                .bus_rdata  (cpu_bus_rdata[c*64 +: 64]),
                .bus_ready  (cpu_bus_ready[c]),

                // Tile CSR — per-core tile engine (rdata muxed with IPI)
                .csr_wen    (core_csr_wen   [c]),
                .csr_addr   (core_csr_addr  [c*8  +: 8]),
                .csr_wdata  (core_csr_wdata [c*64 +: 64]),
                .csr_rdata  (core_csr_rdata [c*64 +: 64]),

                // MEX dispatch — per-core tile engine
                .mex_valid  (core_mex_valid  [c]),
                .mex_ss     (core_mex_ss     [c*2  +: 2]),
                .mex_op     (core_mex_op     [c*2  +: 2]),
                .mex_funct  (core_mex_funct  [c*3  +: 3]),
                .mex_gpr_val(core_mex_gpr_val[c*64 +: 64]),
                .mex_imm8   (core_mex_imm8   [c*8  +: 8]),
                .mex_ext_mod   (core_mex_ext_mod   [c*4  +: 4]),
                .mex_ext_active(core_mex_ext_active [c]),
                .mex_done   (core_mex_done   [c]),
                .mex_busy   (core_mex_busy   [c]),

                // Interrupts — timer to all, UART/NIC to core 0 only
                .irq_timer  (irq_timer),
                .irq_uart   (c == 0 ? irq_uart : 1'b0),
                .irq_nic    (c == 0 ? irq_nic  : 1'b0),
                .irq_ipi    (ipi_lines[c])
            );

            // ----------------------------------------------------------------
            // CSR-side IPI: intercept CSR writes to 0x22/0x23 and forward
            // to the mailbox's per-core CSR port.
            // ----------------------------------------------------------------
            assign csr_ipi_wen  [c]          = core_csr_wen[c] &&
                                               (core_csr_addr[c*8 +: 8] == CSR_MBOX ||
                                                core_csr_addr[c*8 +: 8] == CSR_IPIACK);
            assign csr_ipi_addr [c*8  +: 8]  = core_csr_addr [c*8  +: 8];
            assign csr_ipi_wdata[c*64 +: 64] = core_csr_wdata[c*64 +: 64];

            // CSR read mux: IPI CSRs come from mailbox, everything else from tile
            assign core_csr_rdata[c*64 +: 64] =
                (core_csr_addr[c*8 +: 8] == CSR_MBOX ||
                 core_csr_addr[c*8 +: 8] == CSR_IPIACK)
                    ? csr_ipi_rdata[c*64 +: 64]
                    : tile_csr_rdata[c*64 +: 64];

            // ----------------------------------------------------------------
            // Tile engine (one per core — private state, shared memory)
            // ----------------------------------------------------------------
            mp64_tile u_tile (
                .clk            (sys_clk),
                .rst_n          (sys_rst_n),

                // CSR interface (from this core's CPU)
                .csr_wen        (core_csr_wen   [c]),
                .csr_addr       (core_csr_addr  [c*8  +: 8]),
                .csr_wdata      (core_csr_wdata [c*64 +: 64]),
                .csr_rdata      (tile_csr_rdata [c*64 +: 64]),

                // MEX dispatch (from this core's CPU)
                .mex_valid      (core_mex_valid  [c]),
                .mex_ss         (core_mex_ss     [c*2  +: 2]),
                .mex_op         (core_mex_op     [c*2  +: 2]),
                .mex_funct      (core_mex_funct  [c*3  +: 3]),
                .mex_gpr_val    (core_mex_gpr_val[c*64 +: 64]),
                .mex_imm8       (core_mex_imm8   [c*8  +: 8]),
                .mex_ext_mod    (core_mex_ext_mod   [c*4  +: 4]),
                .mex_ext_active (core_mex_ext_active [c]),
                .mex_done       (core_mex_done   [c]),
                .mex_busy       (core_mex_busy   [c]),

                // Internal BRAM Port A (goes to tile arbiter)
                .tile_req       (tile_int_req  [c]),
                .tile_addr      (tile_int_addr [c*20 +: 20]),
                .tile_wen       (tile_int_wen  [c]),
                .tile_wdata     (tile_int_wdata[c*512 +: 512]),
                .tile_rdata     (tile_int_rdata[c*512 +: 512]),
                .tile_ack       (tile_int_ack  [c]),

                // External memory path (goes to ext tile arbiter)
                .ext_tile_req   (tile_ext_req  [c]),
                .ext_tile_addr  (tile_ext_addr [c*64 +: 64]),
                .ext_tile_wen   (tile_ext_wen  [c]),
                .ext_tile_wdata (tile_ext_wdata[c*512 +: 512]),
                .ext_tile_rdata (tile_ext_rdata[c*512 +: 512]),
                .ext_tile_ack   (tile_ext_ack  [c])
            );

        end
    endgenerate

    // ========================================================================
    // Tile Port A Arbiter (round-robin among NUM_CORES tile engines)
    // ========================================================================
    // Simple round-robin: only one tile engine accesses BRAM Port A per cycle.
    reg [CORE_ID_BITS-1:0] tile_grant;
    reg [CORE_ID_BITS-1:0] tile_last_grant;
    reg                     tile_busy;

    // Round-robin scan for tile requests
    reg [CORE_ID_BITS-1:0] tile_next;
    reg                     tile_any;

    always @(*) begin
        tile_next = tile_last_grant;
        tile_any  = 1'b0;
        if (tile_int_req[(tile_last_grant + 2'd1) & 2'd3]) begin
            tile_next = (tile_last_grant + 2'd1) & 2'd3;
            tile_any  = 1'b1;
        end else if (tile_int_req[(tile_last_grant + 2'd2) & 2'd3]) begin
            tile_next = (tile_last_grant + 2'd2) & 2'd3;
            tile_any  = 1'b1;
        end else if (tile_int_req[(tile_last_grant + 2'd3) & 2'd3]) begin
            tile_next = (tile_last_grant + 2'd3) & 2'd3;
            tile_any  = 1'b1;
        end else if (tile_int_req[tile_last_grant]) begin
            tile_next = tile_last_grant;
            tile_any  = 1'b1;
        end
    end

    // Tile arbiter state
    assign tile_arb_req   = tile_any && !tile_busy ? 1'b1 :
                            tile_busy              ? 1'b1 : 1'b0;
    assign tile_arb_addr  = tile_int_addr [tile_grant*20 +: 20];
    assign tile_arb_wen   = tile_int_wen  [tile_grant];
    assign tile_arb_wdata = tile_int_wdata[tile_grant*512 +: 512];

    // Route ack + rdata back to winning core only
    genvar ti;
    generate
        for (ti = 0; ti < NUM_CORES; ti = ti + 1) begin : tile_ack_mux
            assign tile_int_rdata[ti*512 +: 512] = tile_arb_rdata;
            assign tile_int_ack[ti] = tile_arb_ack && (tile_grant == ti[CORE_ID_BITS-1:0]);
        end
    endgenerate

    always @(posedge sys_clk or negedge sys_rst_n) begin
        if (!sys_rst_n) begin
            tile_grant      <= {CORE_ID_BITS{1'b0}};
            tile_last_grant <= {CORE_ID_BITS{1'b0}};
            tile_busy       <= 1'b0;
        end else begin
            if (tile_busy) begin
                if (tile_arb_ack) begin
                    tile_busy       <= 1'b0;
                    tile_last_grant <= tile_grant;
                end
            end else if (tile_any) begin
                tile_grant <= tile_next;
                tile_busy  <= 1'b1;
            end
        end
    end

    // ========================================================================
    // Tile External Memory Arbiter (round-robin, mirrors internal arbiter)
    // ========================================================================
    reg [CORE_ID_BITS-1:0] ext_tile_grant;
    reg [CORE_ID_BITS-1:0] ext_tile_last_grant;
    reg                     ext_tile_busy;

    reg [CORE_ID_BITS-1:0] ext_tile_next;
    reg                     ext_tile_any;

    always @(*) begin
        ext_tile_next = ext_tile_last_grant;
        ext_tile_any  = 1'b0;
        if (tile_ext_req[(ext_tile_last_grant + 2'd1) & 2'd3]) begin
            ext_tile_next = (ext_tile_last_grant + 2'd1) & 2'd3;
            ext_tile_any  = 1'b1;
        end else if (tile_ext_req[(ext_tile_last_grant + 2'd2) & 2'd3]) begin
            ext_tile_next = (ext_tile_last_grant + 2'd2) & 2'd3;
            ext_tile_any  = 1'b1;
        end else if (tile_ext_req[(ext_tile_last_grant + 2'd3) & 2'd3]) begin
            ext_tile_next = (ext_tile_last_grant + 2'd3) & 2'd3;
            ext_tile_any  = 1'b1;
        end else if (tile_ext_req[ext_tile_last_grant]) begin
            ext_tile_next = ext_tile_last_grant;
            ext_tile_any  = 1'b1;
        end
    end

    assign tile_ext_arb_req   = ext_tile_any && !ext_tile_busy ? 1'b1 :
                                ext_tile_busy                 ? 1'b1 : 1'b0;
    assign tile_ext_arb_addr  = tile_ext_addr [ext_tile_grant*64 +: 64];
    assign tile_ext_arb_wen   = tile_ext_wen  [ext_tile_grant];
    assign tile_ext_arb_wdata = tile_ext_wdata[ext_tile_grant*512 +: 512];

    generate
        for (ti = 0; ti < NUM_CORES; ti = ti + 1) begin : ext_tile_ack_mux
            assign tile_ext_rdata[ti*512 +: 512] = tile_ext_arb_rdata;
            assign tile_ext_ack[ti] = tile_ext_arb_ack && (ext_tile_grant == ti[CORE_ID_BITS-1:0]);
        end
    endgenerate

    always @(posedge sys_clk or negedge sys_rst_n) begin
        if (!sys_rst_n) begin
            ext_tile_grant      <= {CORE_ID_BITS{1'b0}};
            ext_tile_last_grant <= {CORE_ID_BITS{1'b0}};
            ext_tile_busy       <= 1'b0;
        end else begin
            if (ext_tile_busy) begin
                if (tile_ext_arb_ack) begin
                    ext_tile_busy       <= 1'b0;
                    ext_tile_last_grant <= ext_tile_grant;
                end
            end else if (ext_tile_any) begin
                ext_tile_grant <= ext_tile_next;
                ext_tile_busy  <= 1'b1;
            end
        end
    end

    // ========================================================================
    // Multi-Master Bus Arbiter (4 CPUs → Memory + MMIO)
    // ========================================================================
    mp64_bus u_bus (
        .clk        (sys_clk),
        .rst_n      (sys_rst_n),
        // Multi-core CPU masters (flat-packed)
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
    // Dual-Port Memory (1 MiB shared BRAM)
    // ========================================================================
    mp64_memory u_memory (
        .clk        (sys_clk),
        .rst_n      (sys_rst_n),
        // Port A: Tile engines (512-bit, arbitrated)
        .tile_req   (tile_arb_req),
        .tile_addr  (tile_arb_addr),
        .tile_wdata (tile_arb_wdata),
        .tile_wen   (tile_arb_wen),
        .tile_rdata (tile_arb_rdata),
        .tile_ack   (tile_arb_ack),
        // Port B: CPUs (64-bit, arbitrated by mp64_bus)
        .cpu_req    (mem_req),
        .cpu_addr   (mem_addr),
        .cpu_wdata  (mem_wdata),
        .cpu_wen    (mem_wen),
        .cpu_size   (mem_size),
        .cpu_rdata  (mem_rdata),
        .cpu_ack    (mem_ack),
        // External forwarding
        .ext_req    (mem_ext_req),
        .ext_addr   (mem_ext_addr),
        .ext_wdata  (mem_ext_wdata),
        .ext_wen    (mem_ext_wen),
        .ext_size   (mem_ext_size),
        .ext_rdata  (mem_ext_rdata),
        .ext_ack    (mem_ext_ack)
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
        // Tile burst port (arbitrated)
        .tile_req       (tile_ext_arb_req),
        .tile_addr      (tile_ext_arb_addr),
        .tile_wen       (tile_ext_arb_wen),
        .tile_wdata     (tile_ext_arb_wdata),
        .tile_rdata     (tile_ext_arb_rdata),
        .tile_ack       (tile_ext_arb_ack),
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
    // Inter-Core Mailbox & Hardware Spinlocks
    // ========================================================================
    // The requester_id comes from the bus arbiter's current grant.
    // We expose it as a wire from the bus arbiter.
    // (For simplicity, mailbox reads the grant register directly.)
    wire [CORE_ID_BITS-1:0] bus_grant;
    assign bus_grant = u_bus.grant;

    mp64_mailbox u_mailbox (
        .clk            (sys_clk),
        .rst_n          (sys_rst_n),
        .req            (mbox_sel),
        .addr           (mmio_addr),
        .wdata          (mmio_wdata_bus[7:0]),
        .wen            (mmio_wen),
        .rdata          (mbox_rdata8),
        .ack            (mbox_ack),
        .requester_id   (bus_grant),
        .ipi_out        (ipi_lines),
        // CSR-side IPI (per-core, bypasses bus)
        .csr_ipi_wen    (csr_ipi_wen),
        .csr_ipi_addr   (csr_ipi_addr),
        .csr_ipi_wdata  (csr_ipi_wdata),
        .csr_ipi_rdata  (csr_ipi_rdata)
    );

    // ========================================================================
    // UART (shared — core 0 gets IRQ, all cores can read/write via MMIO)
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
    // Timer (shared — IRQ goes to all cores)
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
    // Debug LEDs — show core 0 state + multi-core activity
    // ========================================================================
    assign debug_leds = {
        |ipi_lines,                   // [7] any IPI active
        |core_mex_busy,               // [6] any tile engine busy
        irq_timer,                    // [5] timer IRQ
        cpu_bus_valid[0],             // [4] core 0 bus active
        cpu_bus_valid[1],             // [3] core 1 bus active
        cpu_bus_valid[2],             // [2] core 2 bus active
        cpu_bus_valid[3],             // [1] core 3 bus active
        1'b1                          // [0] power-on indicator
    };

endmodule
