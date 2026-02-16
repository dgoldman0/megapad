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

    // --- CPU ↔ Bus (flat-packed for arbiter, NUM_BUS_PORTS wide) ---
    // Ports [0..NUM_CORES-1] = full cores, [NUM_CORES..NUM_BUS_PORTS-1] = clusters
    wire [NUM_BUS_PORTS-1:0]        cpu_bus_valid;
    wire [NUM_BUS_PORTS*64-1:0]     cpu_bus_addr;
    wire [NUM_BUS_PORTS*64-1:0]     cpu_bus_wdata;
    wire [NUM_BUS_PORTS-1:0]        cpu_bus_wen;
    wire [NUM_BUS_PORTS*2-1:0]      cpu_bus_size;
    wire [NUM_BUS_PORTS*64-1:0]     cpu_bus_rdata;
    wire [NUM_BUS_PORTS-1:0]        cpu_bus_ready;

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

    // --- Per-core IPI (NUM_ALL_CORES: full + micro) ---
    wire [NUM_ALL_CORES-1:0]        ipi_lines;

    // --- Per-core CSR-side IPI interface (CPU → mailbox, bypasses bus) ---
    wire [NUM_ALL_CORES-1:0]        csr_ipi_wen;
    wire [NUM_ALL_CORES*8-1:0]      csr_ipi_addr;
    wire [NUM_ALL_CORES*64-1:0]     csr_ipi_wdata;
    wire [NUM_ALL_CORES*64-1:0]     csr_ipi_rdata;

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
    wire        irq_aes;
    wire        irq_sha;
    wire        irq_crc;

    // --- Per-peripheral 8-bit read data ---
    wire [7:0]  uart_rdata8;
    wire [7:0]  timer_rdata8;
    wire [7:0]  disk_rdata8;
    wire [7:0]  nic_rdata8;
    wire [7:0]  mbox_rdata8;

    wire        uart_ack, timer_ack, disk_ack, nic_ack, mbox_ack;

    // --- Crypto accelerator read data ---
    wire [63:0] aes_rdata64;
    wire [63:0] sha_rdata64;
    wire [63:0] crc_rdata64;
    wire [63:0] trng_rdata64;
    wire [63:0] field_alu_rdata64;
    wire [63:0] ntt_rdata64;
    wire [63:0] kem_rdata64;
    wire        aes_ack, sha_ack, crc_ack, trng_ack;
    wire        field_alu_ack, ntt_ack, kem_ack;

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

    // --- QoS CSR sideband (any core can write) ---
    reg         qos_csr_wen;
    reg  [7:0]  qos_csr_addr;
    reg  [63:0] qos_csr_wdata;
    wire [63:0] qos_csr_rdata;

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

    // Crypto peripherals at 0x7xx, sub-decoded
    wire aes_sel     = mmio_req && (mmio_addr[11:7] == 5'b01110);       // 0x700-0x77F
    wire sha_sel     = mmio_req && (mmio_addr[11:6] == 6'b011110);      // 0x780-0x7BF
    wire crc_sel     = mmio_req && (mmio_addr[11:5] == 7'b0111110);     // 0x7C0-0x7DF
    wire trng_sel    = mmio_req && (mmio_addr[11:5] == 7'b1000000);     // 0x800-0x81F
    wire field_alu_sel = mmio_req && (mmio_addr[11:6] == 6'b100001);    // 0x840-0x87F
    wire ntt_sel     = mmio_req && (mmio_addr[11:6] == 6'b100011);      // 0x8C0-0x8FF
    wire kem_sel     = mmio_req && (mmio_addr[11:6] == 6'b100100);      // 0x900-0x93F

    // MMIO read mux
    wire [63:0] sysinfo_rdata;

    assign mmio_rdata_bus = uart_sel    ? {56'd0, uart_rdata8}  :
                            timer_sel   ? {56'd0, timer_rdata8} :
                            disk_sel    ? {56'd0, disk_rdata8}  :
                            nic_sel     ? {56'd0, nic_rdata8}   :
                            mbox_sel    ? {56'd0, mbox_rdata8}  :
                            sysinfo_sel ? sysinfo_rdata         :
                            aes_sel     ? aes_rdata64           :
                            sha_sel     ? sha_rdata64           :
                            crc_sel     ? crc_rdata64           :
                            trng_sel    ? trng_rdata64          :
                            field_alu_sel ? field_alu_rdata64    :
                            ntt_sel     ? ntt_rdata64            :
                            kem_sel     ? kem_rdata64            :
                            64'd0;
    assign mmio_ack = 1'b1;  // all MMIO peripherals are single-cycle

    // SysInfo: now includes core count + cluster enable
    reg [NUM_CLUSTERS-1:0] cluster_en;    // per-cluster enable (default: all off)
    always @(posedge sys_clk or negedge sys_rst_n) begin
        if (!sys_rst_n)
            cluster_en <= {NUM_CLUSTERS{1'b0}};
        else if (sysinfo_sel && mmio_wen && mmio_addr[7:0] == 8'h18)
            cluster_en <= mmio_wdata_bus[NUM_CLUSTERS-1:0];
    end

    assign sysinfo_rdata = (mmio_addr[7:0] == 8'h00) ? 64'h4D50_3634_0002_0001 :  // "MP64" v2.1 (multi-core)
                           (mmio_addr[7:0] == 8'h08) ? 64'd1048576               :  // RAM size
                           (mmio_addr[7:0] == 8'h10) ? NUM_ALL_CORES[63:0]      :  // Core count
                           (mmio_addr[7:0] == 8'h18) ? {{(64-NUM_CLUSTERS){1'b0}}, cluster_en} :  // Cluster enable
                           64'd0;

    // DMA ack stubs
    assign disk_dma_rdata = 8'd0;
    assign disk_dma_ack   = 1'b1;
    assign nic_dma_rdata  = 8'd0;
    assign nic_dma_ack    = 1'b1;

    // QoS CSR write mux: any core can write, core 0 priority
    always @(*) begin
        qos_csr_wen   = 1'b0;
        qos_csr_addr  = 8'd0;
        qos_csr_wdata = 64'd0;
        if (core_csr_wen[0] &&
            (core_csr_addr[7:0] == CSR_QOS_WEIGHT ||
             core_csr_addr[7:0] == CSR_QOS_BWLIMIT)) begin
            qos_csr_wen   = 1'b1;
            qos_csr_addr  = core_csr_addr[7:0];
            qos_csr_wdata = core_csr_wdata[63:0];
        end else if (core_csr_wen[1] &&
            (core_csr_addr[1*8 +: 8] == CSR_QOS_WEIGHT ||
             core_csr_addr[1*8 +: 8] == CSR_QOS_BWLIMIT)) begin
            qos_csr_wen   = 1'b1;
            qos_csr_addr  = core_csr_addr[1*8 +: 8];
            qos_csr_wdata = core_csr_wdata[1*64 +: 64];
        end else if (core_csr_wen[2] &&
            (core_csr_addr[2*8 +: 8] == CSR_QOS_WEIGHT ||
             core_csr_addr[2*8 +: 8] == CSR_QOS_BWLIMIT)) begin
            qos_csr_wen   = 1'b1;
            qos_csr_addr  = core_csr_addr[2*8 +: 8];
            qos_csr_wdata = core_csr_wdata[2*64 +: 64];
        end else if (core_csr_wen[3] &&
            (core_csr_addr[3*8 +: 8] == CSR_QOS_WEIGHT ||
             core_csr_addr[3*8 +: 8] == CSR_QOS_BWLIMIT)) begin
            qos_csr_wen   = 1'b1;
            qos_csr_addr  = core_csr_addr[3*8 +: 8];
            qos_csr_wdata = core_csr_wdata[3*64 +: 64];
        end
    end

    // ========================================================================
    // Generate: NUM_CORES × (CPU + I-Cache + Tile Engine)
    // ========================================================================
    genvar c;
    generate
        for (c = 0; c < NUM_CORES; c = c + 1) begin : core

            // ----------------------------------------------------------------
            // Per-core I-Cache wires
            // ----------------------------------------------------------------
            wire [63:0] ic_fetch_addr;
            wire        ic_fetch_req;
            wire [63:0] ic_fetch_data;
            wire        ic_fetch_hit;
            wire        ic_fetch_stall;
            wire        ic_inv_all;
            wire        ic_inv_line;
            wire [63:0] ic_inv_addr;

            // I-cache → bus (refill path)
            wire        ic_bus_valid;
            wire [63:0] ic_bus_addr;
            wire [63:0] ic_bus_rdata;
            wire        ic_bus_ready;

            // CPU data bus (load/store)
            wire        cpu_data_valid;
            wire [63:0] cpu_data_addr;
            wire [63:0] cpu_data_wdata;
            wire        cpu_data_wen;
            wire [1:0]  cpu_data_size;

            // ----------------------------------------------------------------
            // I-cache instance (4 KiB, 16-byte lines, per-core)
            // ----------------------------------------------------------------
            wire [63:0] ic_stat_hits;
            wire [63:0] ic_stat_misses;

            mp64_icache u_icache (
                .clk            (sys_clk),
                .rst_n          (sys_rst_n),
                .fetch_addr     (ic_fetch_addr),
                .fetch_valid    (ic_fetch_req),
                .fetch_data     (ic_fetch_data),
                .fetch_hit      (ic_fetch_hit),
                .fetch_stall    (ic_fetch_stall),
                .inv_all        (ic_inv_all),
                .inv_line       (ic_inv_line),
                .inv_addr       (ic_inv_addr),
                .bus_valid      (ic_bus_valid),
                .bus_addr       (ic_bus_addr),
                .bus_rdata      (ic_bus_rdata),
                .bus_ready      (ic_bus_ready),
                .stat_hits      (ic_stat_hits),
                .stat_misses    (ic_stat_misses)
            );

            // ----------------------------------------------------------------
            // Local bus mux: CPU data path vs I-cache refill
            //
            // CPU data operations (load/store/MMIO) take priority.
            // I-cache refills only happen when the CPU data path is idle.
            // ----------------------------------------------------------------
            assign cpu_bus_valid[c] = cpu_data_valid | ic_bus_valid;
            assign cpu_bus_addr [c*64 +: 64] = cpu_data_valid ? cpu_data_addr
                                                              : ic_bus_addr;
            assign cpu_bus_wdata[c*64 +: 64] = cpu_data_wdata;
            assign cpu_bus_wen  [c]          = cpu_data_valid ? cpu_data_wen
                                                              : 1'b0;
            assign cpu_bus_size [c*2  +: 2]  = cpu_data_valid ? cpu_data_size
                                                              : 2'b11;  // DWORD for refill

            // Route bus responses
            assign ic_bus_rdata = cpu_bus_rdata[c*64 +: 64];
            assign ic_bus_ready = !cpu_data_valid && cpu_bus_ready[c];

            // ----------------------------------------------------------------
            // CPU core
            // ----------------------------------------------------------------
            mp64_cpu u_cpu (
                .clk        (sys_clk),
                .rst_n      (sys_rst_n),
                .core_id    (c[CORE_ID_BITS-1:0]),

                // I-cache interface
                .icache_addr    (ic_fetch_addr),
                .icache_req     (ic_fetch_req),
                .icache_data    (ic_fetch_data),
                .icache_hit     (ic_fetch_hit),
                .icache_stall   (ic_fetch_stall),
                .icache_inv_all (ic_inv_all),
                .icache_inv_line(ic_inv_line),
                .icache_inv_addr(ic_inv_addr),

                // Data bus master — through local mux
                .bus_valid  (cpu_data_valid),
                .bus_addr   (cpu_data_addr),
                .bus_wdata  (cpu_data_wdata),
                .bus_wen    (cpu_data_wen),
                .bus_size   (cpu_data_size),
                .bus_rdata  (cpu_bus_rdata[c*64 +: 64]),
                .bus_ready  (cpu_data_valid && cpu_bus_ready[c]),

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
                .irq_ipi    (ipi_lines[c]),

                // I-Cache statistics
                .icache_stat_hits   (ic_stat_hits),
                .icache_stat_misses (ic_stat_misses),

                // System configuration
                .mem_size_bytes (64'd1048576),  // 1 MiB

                // External flags (active-low on 1802; tie high = inactive)
                .ef_flags   (4'b0000)
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

            // CSR read mux: IPI CSRs come from mailbox, QoS from bus, else tile
            assign core_csr_rdata[c*64 +: 64] =
                (core_csr_addr[c*8 +: 8] == CSR_MBOX ||
                 core_csr_addr[c*8 +: 8] == CSR_IPIACK)
                    ? csr_ipi_rdata[c*64 +: 64]
                : (core_csr_addr[c*8 +: 8] == CSR_QOS_WEIGHT ||
                   core_csr_addr[c*8 +: 8] == CSR_QOS_BWLIMIT)
                    ? qos_csr_rdata
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
    // Generate: NUM_CLUSTERS × Micro-Core Cluster
    // ========================================================================
    // Each cluster occupies one bus port and contains MICRO_PER_CLUSTER cores.
    // Bus ports: [NUM_CORES..NUM_CORES+NUM_CLUSTERS-1] = [4..6]
    // Core IDs:  MICRO_ID_BASE + cluster * MICRO_PER_CLUSTER + local
    //            Cluster 0 → IDs 4-7, Cluster 1 → IDs 8-11, Cluster 2 → IDs 12-15

    // Per-cluster IPI/CSR winding helper wires
    wire [NUM_CLUSTERS*MICRO_PER_CLUSTER-1:0]       cl_csr_ipi_wen;
    wire [NUM_CLUSTERS*MICRO_PER_CLUSTER*8-1:0]     cl_csr_ipi_addr;
    wire [NUM_CLUSTERS*MICRO_PER_CLUSTER*64-1:0]    cl_csr_ipi_wdata;
    wire [NUM_CLUSTERS*MICRO_PER_CLUSTER*64-1:0]    cl_csr_ipi_rdata_flat;
    wire [NUM_CLUSTERS*MICRO_PER_CLUSTER-1:0]       cl_core_csr_wen;
    wire [NUM_CLUSTERS*MICRO_PER_CLUSTER*8-1:0]     cl_core_csr_addr;
    wire [NUM_CLUSTERS*MICRO_PER_CLUSTER*64-1:0]    cl_core_csr_wdata;

    generate
        for (c = 0; c < NUM_CLUSTERS; c = c + 1) begin : cluster

            localparam BUS_PORT = NUM_CORES + c;
            localparam ID_BASE  = MICRO_ID_BASE + c * MICRO_PER_CLUSTER;

            mp64_cluster #(
                .N               (MICRO_PER_CLUSTER),
                .CLUSTER_ID_BASE (ID_BASE)
            ) u_cluster (
                .clk        (sys_clk),
                .rst_n      (sys_rst_n),
                .cluster_en (cluster_en[c]),

                // Bus port → main arbiter port [BUS_PORT]
                .bus_valid  (cpu_bus_valid [BUS_PORT]),
                .bus_addr   (cpu_bus_addr  [BUS_PORT*64 +: 64]),
                .bus_wdata  (cpu_bus_wdata [BUS_PORT*64 +: 64]),
                .bus_wen    (cpu_bus_wen   [BUS_PORT]),
                .bus_size   (cpu_bus_size  [BUS_PORT*2  +: 2]),
                .bus_rdata  (cpu_bus_rdata [BUS_PORT*64 +: 64]),
                .bus_ready  (cpu_bus_ready [BUS_PORT]),

                // Interrupts from SoC
                .irq_timer  ({MICRO_PER_CLUSTER{irq_timer}}),
                .irq_ipi    (ipi_lines[ID_BASE +: MICRO_PER_CLUSTER]),

                // CSR IPI (per micro-core within cluster → mailbox)
                .csr_ipi_wen   (cl_csr_ipi_wen  [c*MICRO_PER_CLUSTER +: MICRO_PER_CLUSTER]),
                .csr_ipi_addr  (cl_csr_ipi_addr [c*MICRO_PER_CLUSTER*8 +: MICRO_PER_CLUSTER*8]),
                .csr_ipi_wdata (cl_csr_ipi_wdata[c*MICRO_PER_CLUSTER*64 +: MICRO_PER_CLUSTER*64]),
                .csr_ipi_rdata (cl_csr_ipi_rdata_flat[c*MICRO_PER_CLUSTER*64 +: MICRO_PER_CLUSTER*64]),

                // Core CSR sideband (for QoS etc.)
                .core_csr_wen  (cl_core_csr_wen  [c*MICRO_PER_CLUSTER +: MICRO_PER_CLUSTER]),
                .core_csr_addr (cl_core_csr_addr [c*MICRO_PER_CLUSTER*8 +: MICRO_PER_CLUSTER*8]),
                .core_csr_wdata(cl_core_csr_wdata[c*MICRO_PER_CLUSTER*64 +: MICRO_PER_CLUSTER*64])
            );

        end
    endgenerate

    // Connect cluster CSR IPI to the wider mailbox arrays
    // Full cores: indices [0..NUM_CORES-1], Cluster micro-cores: [NUM_CORES..NUM_ALL_CORES-1]
    generate
        for (c = 0; c < NUM_MICRO_CORES; c = c + 1) begin : cl_ipi_map
            assign csr_ipi_wen  [MICRO_ID_BASE + c]           = cl_csr_ipi_wen  [c];
            assign csr_ipi_addr [(MICRO_ID_BASE + c)*8 +: 8]  = cl_csr_ipi_addr [c*8 +: 8];
            assign csr_ipi_wdata[(MICRO_ID_BASE + c)*64 +: 64]= cl_csr_ipi_wdata[c*64 +: 64];
            assign cl_csr_ipi_rdata_flat[c*64 +: 64]          = csr_ipi_rdata[(MICRO_ID_BASE + c)*64 +: 64];
        end
    endgenerate

    // ========================================================================
    // Tile Port A Arbiter (round-robin among NUM_CORES tile engines)
    // ========================================================================
    // Simple round-robin: only one tile engine accesses BRAM Port A per cycle.
    localparam TILE_BITS = $clog2(NUM_CORES);   // only full cores have tiles
    reg [TILE_BITS-1:0] tile_grant;
    reg [TILE_BITS-1:0] tile_last_grant;
    reg                 tile_busy;

    // Round-robin scan for tile requests
    reg [TILE_BITS-1:0] tile_next;
    reg                 tile_any;
    integer             tile_rr_i;

    always @(*) begin
        tile_next = tile_last_grant;
        tile_any  = 1'b0;
        for (tile_rr_i = 1; tile_rr_i <= NUM_CORES; tile_rr_i = tile_rr_i + 1) begin
            if (!tile_any &&
                tile_int_req[(tile_last_grant + tile_rr_i[TILE_BITS-1:0]) % NUM_CORES]) begin
                tile_next = (tile_last_grant + tile_rr_i[TILE_BITS-1:0]) % NUM_CORES;
                tile_any  = 1'b1;
            end
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
            assign tile_int_ack[ti] = tile_arb_ack && (tile_grant == ti[TILE_BITS-1:0]);
        end
    endgenerate

    always @(posedge sys_clk or negedge sys_rst_n) begin
        if (!sys_rst_n) begin
            tile_grant      <= {TILE_BITS{1'b0}};
            tile_last_grant <= {TILE_BITS{1'b0}};
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
    reg [TILE_BITS-1:0] ext_tile_grant;
    reg [TILE_BITS-1:0] ext_tile_last_grant;
    reg                 ext_tile_busy;

    reg [TILE_BITS-1:0] ext_tile_next;
    reg                 ext_tile_any;
    integer             ext_tile_rr_i;

    always @(*) begin
        ext_tile_next = ext_tile_last_grant;
        ext_tile_any  = 1'b0;
        for (ext_tile_rr_i = 1; ext_tile_rr_i <= NUM_CORES; ext_tile_rr_i = ext_tile_rr_i + 1) begin
            if (!ext_tile_any &&
                tile_ext_req[(ext_tile_last_grant + ext_tile_rr_i[TILE_BITS-1:0]) % NUM_CORES]) begin
                ext_tile_next = (ext_tile_last_grant + ext_tile_rr_i[TILE_BITS-1:0]) % NUM_CORES;
                ext_tile_any  = 1'b1;
            end
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
            assign tile_ext_ack[ti] = tile_ext_arb_ack && (ext_tile_grant == ti[TILE_BITS-1:0]);
        end
    endgenerate

    always @(posedge sys_clk or negedge sys_rst_n) begin
        if (!sys_rst_n) begin
            ext_tile_grant      <= {TILE_BITS{1'b0}};
            ext_tile_last_grant <= {TILE_BITS{1'b0}};
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
    // Multi-Master Bus Arbiter (NUM_BUS_PORTS masters → Memory + MMIO)
    // ========================================================================
    mp64_bus #(
        .N_PORTS   (NUM_BUS_PORTS),
        .PORT_BITS (BUS_PORT_BITS)
    ) u_bus (
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
        .mmio_ack   (mmio_ack),
        // QoS CSR sideband
        .qos_csr_wen   (qos_csr_wen),
        .qos_csr_addr  (qos_csr_addr),
        .qos_csr_wdata (qos_csr_wdata),
        .qos_csr_rdata (qos_csr_rdata)
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
    wire [BUS_PORT_BITS-1:0] bus_grant;
    assign bus_grant = u_bus.grant;

    mp64_mailbox #(
        .N_CORES (NUM_ALL_CORES),
        .ID_BITS (CORE_ID_BITS)
    ) u_mailbox (
        .clk            (sys_clk),
        .rst_n          (sys_rst_n),
        .req            (mbox_sel),
        .addr           (mmio_addr),
        .wdata          (mmio_wdata_bus[7:0]),
        .wen            (mmio_wen),
        .rdata          (mbox_rdata8),
        .ack            (mbox_ack),
        .requester_id   ({{(CORE_ID_BITS - BUS_PORT_BITS){1'b0}}, bus_grant}),
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
    // AES-256-GCM Accelerator (MMIO 0x700-0x77F)
    // ========================================================================
    mp64_aes u_aes (
        .clk    (sys_clk),
        .rst_n  (sys_rst_n),
        .req    (aes_sel),
        .addr   (mmio_addr[6:0]),
        .wdata  (mmio_wdata_bus),
        .wen    (mmio_wen),
        .rdata  (aes_rdata64),
        .ack    (aes_ack),
        .irq    (irq_aes)
    );

    // ========================================================================
    // SHA-3/SHAKE Accelerator (MMIO 0x780-0x7BF)
    // ========================================================================
    mp64_sha3 u_sha3 (
        .clk    (sys_clk),
        .rst_n  (sys_rst_n),
        .req    (sha_sel),
        .addr   (mmio_addr[5:0]),
        .wdata  (mmio_wdata_bus),
        .wen    (mmio_wen),
        .rdata  (sha_rdata64),
        .ack    (sha_ack),
        .irq    (irq_sha)
    );

    // ========================================================================
    // CRC32/CRC64 Accelerator (MMIO 0x7C0-0x7DF)
    // ========================================================================
    mp64_crc u_crc (
        .clk    (sys_clk),
        .rst_n  (sys_rst_n),
        .req    (crc_sel),
        .addr   (mmio_addr[4:0]),
        .wdata  (mmio_wdata_bus),
        .wen    (mmio_wen),
        .rdata  (crc_rdata64),
        .ack    (crc_ack),
        .irq    (irq_crc)
    );

    // ========================================================================
    // TRNG — True Random Number Generator (MMIO 0x800-0x81F)
    // ========================================================================
    mp64_trng u_trng (
        .clk    (sys_clk),
        .rst_n  (sys_rst_n),
        .req    (trng_sel),
        .addr   (mmio_addr[4:0]),
        .wdata  (mmio_wdata_bus),
        .wen    (mmio_wen),
        .rdata  (trng_rdata64),
        .ack    (trng_ack)
    );

    // ========================================================================
    // Field ALU — GF(2²⁵⁵−19) Coprocessor (MMIO 0x840-0x87F)
    // Supersedes X25519: mode 0 = backward-compatible scalar multiply.
    // Modes 1-7: FADD, FSUB, FMUL, FSQR, FINV, FPOW, MUL_RAW.
    // ========================================================================
    mp64_field_alu u_field_alu (
        .clk    (sys_clk),
        .rst_n  (sys_rst_n),
        .req    (field_alu_sel),
        .addr   (mmio_addr[5:0]),
        .wdata  (mmio_wdata_bus),
        .wen    (mmio_wen),
        .rdata  (field_alu_rdata64),
        .ack    (field_alu_ack)
    );

    // ========================================================================
    // NTT Accelerator — 256-point NTT for lattice crypto (MMIO 0x8C0-0x8FF)
    // ========================================================================
    mp64_ntt u_ntt (
        .clk    (sys_clk),
        .rst_n  (sys_rst_n),
        .req    (ntt_sel),
        .addr   (mmio_addr[5:0]),
        .wdata  (mmio_wdata_bus),
        .wen    (mmio_wen),
        .rdata  (ntt_rdata64),
        .ack    (ntt_ack)
    );

    // ========================================================================
    // ML-KEM-512 Accelerator (MMIO 0x900-0x93F)
    // ========================================================================
    mp64_kem u_kem (
        .clk    (sys_clk),
        .rst_n  (sys_rst_n),
        .req    (kem_sel),
        .addr   (mmio_addr[5:0]),
        .wdata  (mmio_wdata_bus),
        .wen    (mmio_wen),
        .rdata  (kem_rdata64),
        .ack    (kem_ack)
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
