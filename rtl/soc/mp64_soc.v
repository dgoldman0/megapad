// ============================================================================
// mp64_soc.v — Megapad-64 System-on-Chip Integration
// ============================================================================
//
// Wires together all major design modules:
//
//   CPU cores (×4)  ─┐
//   I-caches  (×4)  ─┤
//   Clusters  (×3)  ─┼─→  Bus arbiter  ─┬─→  Memory subsystem ─→ Ext-mem
//                     │                  └─→  MMIO decoder
//                     │
//   BIOS ROM ────────→ (mapped at addr 0x0 via memory initialisation)
//
// MMIO peripherals:  UART, Timer, Disk (SD), Mailbox, NIC,
//                    AES, SHA-3, CRC, TRNG, Field ALU, NTT, KEM
//
// Tile engines: one private engine per full core and one per microcluster
//
// Conventions:
//   - Cores use active-high `rst`; peripherals use active-low `rst_n`.
//   - Bus arbiter presents flat-packed N_PORTS master ports.
//   - MMIO decoder fans 12-bit addresses to peripheral selects.
//
// Coding standard: Verilog-2001, no vendor primitives.
//

`include "mp64_pkg.vh"

module mp64_soc #(
    parameter CLOCK_HZ          = 100_000_000,
    parameter NUM_CORES         = 4,
    parameter NUM_CLUSTERS      = 3,
    parameter CORES_PER_CLUSTER = 4,
    parameter MEM_DEPTH         = 16384,    // per-bank depth (×512-bit rows)
    parameter EXT_MEM_SIZE_PARAM = 0,         // ext bytes; 0 = up to VRAM_BASE
    parameter [31:0] DISK_TOTAL_SECTORS = 32'd8192,
    parameter BIOS_INIT_FILE    = "rom.hex"
)(
    input  wire        sys_clk,
    input  wire        sys_rst_n,

    // === UART ===
    input  wire        uart_rxd,
    output wire        uart_txd,

    // === External Memory PHY ===
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

    // === SD Card (SPI) ===
    output wire        sd_sck,
    output wire        sd_mosi,
    input  wire        sd_miso,
    output wire        sd_cs_n,
    input  wire        sd_card_present,
    input  wire        sd_write_protected,

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

    // ========================================================================
    // Derived constants
    // ========================================================================
    localparam NIC_BUS_PORT  = NUM_CORES + NUM_CLUSTERS;
    localparam DISK_BUS_PORT = NIC_BUS_PORT + 1;
    localparam N_BUS_PORTS   = NUM_CORES + NUM_CLUSTERS + 2;
    localparam PORT_BITS    = $clog2(N_BUS_PORTS);
    // Checkpoint 2 qualifies reflected/raw CRC, SHA3 streaming, and public
    // raw Keccak independently.  The reserved WOTS aperture below is inert,
    // has no requester port, and must not advertise WOTS_CHAIN.
    localparam [63:0] CRYPTO_CAPS = 64'h0000_0000_0000_0007;

    // System-wide reset (active-high for cores, active-low for peripherals)
    wire rst_h = ~sys_rst_n;

    // Named reset seams keep cancellation scope explicit even though the
    // current production top has no independent reset-controller inputs.
    // They synthesize inactive today and give focused verification (or a
    // future controller) one unambiguous place to drive paired full-core and
    // individual microcore reset domains.
    wire [NUM_CORES-1:0] core_domain_reset;
    wire [NUM_CLUSTERS*CORES_PER_CLUSTER-1:0] cluster_micro_reset;
    assign core_domain_reset = {NUM_CORES{1'b0}};
    assign cluster_micro_reset =
        {(NUM_CLUSTERS*CORES_PER_CLUSTER){1'b0}};

    // ========================================================================
    // BIOS ROM  (64-bit × 4096 words = 32 KiB, mapped at addr 0)
    // ========================================================================
    // Not directly in the memory path — the BIOS is loaded into Bank 0
    // SRAM via the INIT_FILE mechanism.  We keep a ROM copy for read-only
    // BIOS access via MMIO if desired, but the primary execution path is
    // through the memory subsystem which holds the code in its SRAM.
    //
    // For synthesis, Bank 0 SRAM can be initialised from the same hex file.
    // For simulation, the testbench or Python model loads BIOS into memory.

    // ========================================================================
    // CPU Cores + I-Caches
    // ========================================================================
    // Each core has:
    //   - I-cache (fetch path, refills via bus)
    //   - Data bus port (load/store → bus arbiter)
    //   - CSR/MEX ports to a private tile engine

    // Per-core wires
    wire [63:0] core_bus_addr  [0:NUM_CORES-1];
    wire [63:0] core_bus_wdata [0:NUM_CORES-1];
    wire [1:0]  core_bus_size  [0:NUM_CORES-1];
    wire        core_bus_valid [0:NUM_CORES-1];
    wire        core_bus_wen   [0:NUM_CORES-1];
    wire        core_bus_port_io [0:NUM_CORES-1];
    wire [63:0] core_bus_rdata [0:NUM_CORES-1];
    wire        core_bus_ready [0:NUM_CORES-1];

    // I-cache → bus (refill path)
    wire [63:0] ic_bus_addr    [0:NUM_CORES-1];
    wire        ic_bus_valid   [0:NUM_CORES-1];
    wire        ic_bus_wen     [0:NUM_CORES-1];
    wire [1:0]  ic_bus_size    [0:NUM_CORES-1];
    wire [63:0] ic_bus_rdata   [0:NUM_CORES-1];
    wire        ic_bus_ready   [0:NUM_CORES-1];

    // CPU ↔ I-cache
    wire [63:0] cpu_icache_addr    [0:NUM_CORES-1];
    wire        cpu_icache_req     [0:NUM_CORES-1];
    wire [63:0] cpu_icache_data    [0:NUM_CORES-1];
    wire        cpu_icache_hit     [0:NUM_CORES-1];
    wire        cpu_icache_stall   [0:NUM_CORES-1];
    wire        cpu_icache_enabled [0:NUM_CORES-1];
    wire        cpu_icache_inv_all [0:NUM_CORES-1];
    wire        cpu_icache_inv_line[0:NUM_CORES-1];
    wire [63:0] cpu_icache_inv_addr[0:NUM_CORES-1];
    wire [6:0]  cpu_icache_inv_size[0:NUM_CORES-1];
    wire        icache_inv_all     [0:NUM_CORES-1];
    wire        icache_inv_line    [0:NUM_CORES-1];
    wire [63:0] icache_inv_addr    [0:NUM_CORES-1];
    wire [6:0]  icache_inv_size    [0:NUM_CORES-1];
    wire [63:0] ic_stat_hits       [0:NUM_CORES-1];
    wire [63:0] ic_stat_misses     [0:NUM_CORES-1];

    // Every full core has a paired private tile writer.  Completed 64-byte
    // stores join only that core's CPU data-port invalidation stream.
    wire        core_tile_icache_inv_line[0:NUM_CORES-1];
    wire [63:0] core_tile_icache_inv_addr[0:NUM_CORES-1];

    // Per-core CSR/MEX path to the paired private tile engine.
    wire        core_csr_wen   [0:NUM_CORES-1];
    wire [7:0]  core_csr_addr  [0:NUM_CORES-1];
    wire [63:0] core_csr_wdata [0:NUM_CORES-1];
    wire [63:0] core_csr_rdata [0:NUM_CORES-1];
    wire [255:0] core_legacy_acc_state[0:NUM_CORES-1];
    wire [3:0]   core_legacy_acc_wen  [0:NUM_CORES-1];
    wire [255:0] core_legacy_acc_wdata[0:NUM_CORES-1];
    wire         core_acc_zero_consumed[0:NUM_CORES-1];

    wire        core_mex_valid     [0:NUM_CORES-1];
    wire [1:0]  core_mex_ss        [0:NUM_CORES-1];
    wire [1:0]  core_mex_op        [0:NUM_CORES-1];
    wire [2:0]  core_mex_funct     [0:NUM_CORES-1];
    wire [7:0]  core_mex_funct_byte[0:NUM_CORES-1];
    wire [63:0] core_mex_gpr_val   [0:NUM_CORES-1];
    wire [7:0]  core_mex_imm8      [0:NUM_CORES-1];
    wire [3:0]  core_mex_ext_mod   [0:NUM_CORES-1];
    wire        core_mex_ext_active[0:NUM_CORES-1];
    wire        core_mex_done      [0:NUM_CORES-1];
    wire        core_mex_busy      [0:NUM_CORES-1];
    wire [2:0]  core_mex_fault     [0:NUM_CORES-1];
    wire [63:0] core_mex_fault_addr[0:NUM_CORES-1];
    wire        core_mex_stall_cycle[0:NUM_CORES-1];
    wire        core_tacc_xfer_stall_cycle[0:NUM_CORES-1];
    wire        core_perf_extmem_word[0:NUM_CORES-1];
    wire [TACC_CALLER_BITS-1:0]
                 core_tile_caller_id[0:NUM_CORES-1];
    wire        core_tile_priv     [0:NUM_CORES-1];
    wire [63:0] core_tile_mpu_base [0:NUM_CORES-1];
    wire [63:0] core_tile_mpu_limit[0:NUM_CORES-1];
    wire        core_tile_mpu_enabled[0:NUM_CORES-1];
    wire        core_tile_allow_cluster_spad[0:NUM_CORES-1];
    wire [63:0] core_tacc_status   [0:NUM_CORES-1];
    wire        core_tacc_ctl_valid[0:NUM_CORES-1];
    wire [63:0] core_tacc_ctl_wdata[0:NUM_CORES-1];
    wire        core_tacc_ctl_done [0:NUM_CORES-1];
    wire [2:0]  core_tacc_ctl_fault[0:NUM_CORES-1];

    wire [63:0] core_tacc_status_raw[0:NUM_CORES-1];
    wire [TACC_EPOCH_BITS-1:0]
                 core_tile_engine_epoch[0:NUM_CORES-1];

    // Interrupts
    wire        irq_uart_w;
    wire        irq_nic_w;
    wire        irq_timer_w;
    wire [NUM_CORES-1:0] ipi_out;

    // NIC byte-DMA master.  It participates in the same memory arbiter as
    // CPUs and clusters so BIOS NET-RECV/NET-SEND reach real system RAM.
    wire        nic_dma_req;
    wire [63:0] nic_dma_addr;
    wire [7:0]  nic_dma_wdata;
    wire        nic_dma_wen;
    wire [7:0]  nic_dma_rdata;
    wire        nic_dma_ack;

    // Disk byte-DMA master uses an independent arbiter port.  Unlike sharing
    // a request mux with NIC, this preserves each device's held-request/ACK
    // ownership when both engines are active.
    wire        disk_dma_req;
    wire [63:0] disk_dma_addr;
    wire [7:0]  disk_dma_wdata;
    wire        disk_dma_wen;
    wire [7:0]  disk_dma_rdata;
    wire        disk_dma_ack;
    wire        disk_dma_err;

    // ---- SysInfo localparams (match emulator devices.py register map) ----
    localparam [63:0] MEM_SIZE_BYTES  = MEM_DEPTH * 512 / 8 * 4;  // 4 banks total
    localparam [63:0] BANK0_SIZE      = MEM_DEPTH * 512 / 8;      // 1 bank (system RAM)
    localparam integer NUM_GLOBAL_CORES =
        NUM_CORES + NUM_CLUSTERS * CORES_PER_CLUSTER;
    localparam [63:0] NUM_ALL_CORES   = NUM_GLOBAL_CORES;
    localparam [63:0] HBW_SIZE_BYTES  = 3 * BANK0_SIZE;           // 3 HBW banks
    localparam [31:0] EXT_MEM_BASE    = MP64_EXT_MEM_BASE;
    // The external allocation window ends where the distinct VRAM window
    // begins.  Clamp an oversized board parameter rather than allowing one
    // DMA request to straddle external RAM and VRAM as if they were one span.
    localparam [63:0] EXT_MEM_REQUESTED_SIZE = (EXT_MEM_SIZE_PARAM != 0)
                                        ? EXT_MEM_SIZE_PARAM
                                        : (MP64_VRAM_BASE_ADDR - EXT_MEM_BASE);
    localparam [63:0] EXT_MEM_MAX_SIZE = MP64_VRAM_BASE_ADDR - EXT_MEM_BASE;
    localparam [63:0] EXT_MEM_SIZE = (EXT_MEM_REQUESTED_SIZE < EXT_MEM_MAX_SIZE)
                                        ? EXT_MEM_REQUESTED_SIZE
                                        : EXT_MEM_MAX_SIZE;
    localparam [63:0] EXT_MEM_LIMIT = {32'd0, EXT_MEM_BASE} + EXT_MEM_SIZE;
    localparam [63:0] VRAM_LIMIT = {32'd0, MP64_VRAM_BASE_ADDR} +
                                    {32'd0, MP64_VRAM_DEFAULT_SIZE};
    localparam [63:0] HBW_LIMIT = {32'd0, MP64_HBW_BASE_ADDR} + HBW_SIZE_BYTES;
    reg [63:0] sysinfo_cluster_en;  // R/W at SysInfo offset 0x18

    genvar ci;
    generate
        for (ci = 0; ci < NUM_CORES; ci = ci + 1) begin : g_core

            mp64_cpu #(
                .CORE_ID_W (MP64_CORE_ID_BITS)
            ) u_cpu (
                .clk             (sys_clk),
                .rst             (rst_h | core_domain_reset[ci]),
                .core_id         (ci[MP64_CORE_ID_BITS-1:0]),

                // I-cache interface
                .icache_addr     (cpu_icache_addr[ci]),
                .icache_req      (cpu_icache_req[ci]),
                .icache_data     (cpu_icache_data[ci]),
                .icache_hit      (cpu_icache_hit[ci]),
                .icache_stall    (cpu_icache_stall[ci]),
                .icache_enabled  (cpu_icache_enabled[ci]),
                .icache_inv_all  (cpu_icache_inv_all[ci]),
                .icache_inv_line (cpu_icache_inv_line[ci]),
                .icache_inv_addr (cpu_icache_inv_addr[ci]),
                .icache_inv_size (cpu_icache_inv_size[ci]),
                .icache_stat_hits  (ic_stat_hits[ci]),
                .icache_stat_misses(ic_stat_misses[ci]),

                // Data bus
                .bus_valid       (core_bus_valid[ci]),
                .bus_addr        (core_bus_addr[ci]),
                .bus_wdata       (core_bus_wdata[ci]),
                .bus_wen         (core_bus_wen[ci]),
                .bus_size        (core_bus_size[ci]),
                .bus_port_io     (core_bus_port_io[ci]),
                .bus_rdata       (core_bus_rdata[ci]),
                .bus_ready       (core_bus_ready[ci]),

                // CSR / MEX (tile engine)
                .csr_wen         (core_csr_wen[ci]),
                .csr_addr        (core_csr_addr[ci]),
                .csr_wdata       (core_csr_wdata[ci]),
                .csr_rdata       (core_csr_rdata[ci]),
                .legacy_acc_state(core_legacy_acc_state[ci]),
                .legacy_acc_wen  (core_legacy_acc_wen[ci]),
                .legacy_acc_wdata(core_legacy_acc_wdata[ci]),
                .mex_valid       (core_mex_valid[ci]),
                .mex_ss          (core_mex_ss[ci]),
                .mex_op          (core_mex_op[ci]),
                .mex_funct       (core_mex_funct[ci]),
                .mex_funct_byte  (core_mex_funct_byte[ci]),
                .mex_gpr_val     (core_mex_gpr_val[ci]),
                .mex_imm8        (core_mex_imm8[ci]),
                .mex_ext_mod     (core_mex_ext_mod[ci]),
                .mex_ext_active  (core_mex_ext_active[ci]),
                .mex_done        (core_mex_done[ci]),
                .mex_busy        (core_mex_busy[ci]),
                .mex_fault       (core_mex_fault[ci]),
                .mex_fault_addr  (core_mex_fault_addr[ci]),
                .mex_stall_cycle (
                    core_mex_stall_cycle[ci] ||
                    core_tacc_xfer_stall_cycle[ci]),
                .perf_extmem_word(core_perf_extmem_word[ci]),
                .tile_caller_id  (core_tile_caller_id[ci]),
                .tile_priv       (core_tile_priv[ci]),
                .tile_mpu_base   (core_tile_mpu_base[ci]),
                .tile_mpu_limit  (core_tile_mpu_limit[ci]),
                .tile_mpu_enabled(core_tile_mpu_enabled[ci]),
                .tile_allow_cluster_spad(
                    core_tile_allow_cluster_spad[ci]),
                .tacc_status     (core_tacc_status[ci]),
                .tacc_ctl_valid  (core_tacc_ctl_valid[ci]),
                .tacc_ctl_wdata  (core_tacc_ctl_wdata[ci]),
                .tacc_ctl_done   (core_tacc_ctl_done[ci]),
                .tacc_ctl_fault  (core_tacc_ctl_fault[ci]),

                // Interrupts
                .irq_timer       (irq_timer_w),
                .irq_uart        (irq_uart_w),
                .irq_nic         (irq_nic_w),
                .irq_ipi         (ipi_out[ci]),
                .irq_bus         (bus_err_w[ci]),

                // Info
                .mem_size_bytes  (MEM_SIZE_BYTES),
                .ef_flags        (4'b0000)
            );

            // MEX execution keeps the CPU data port quiescent until the
            // paired tile engine's final ACK.  If that invariant is ever
            // broken, use the cache's oversize fail-safe to flush without
            // resetting statistics rather than dropping either write.
            wire inv_collision = cpu_icache_inv_line[ci]
                               && core_tile_icache_inv_line[ci];
            assign icache_inv_all[ci] = cpu_icache_inv_all[ci];
            assign icache_inv_line[ci] = cpu_icache_inv_line[ci]
                                       || core_tile_icache_inv_line[ci];
            assign icache_inv_addr[ci] = core_tile_icache_inv_line[ci]
                                       ? core_tile_icache_inv_addr[ci]
                                       : cpu_icache_inv_addr[ci];
            assign icache_inv_size[ci] = inv_collision
                                       ? 7'd65
                                       : core_tile_icache_inv_line[ci]
                                       ? 7'd64
                                       : cpu_icache_inv_size[ci];

            mp64_icache u_icache (
                .clk         (sys_clk),
                .rst         (rst_h | core_domain_reset[ci]),

                // CPU fetch side
                .enabled     (cpu_icache_enabled[ci]),
                .fetch_addr  (cpu_icache_addr[ci]),
                .fetch_valid (cpu_icache_req[ci]),
                .fetch_data  (cpu_icache_data[ci]),
                .fetch_hit   (cpu_icache_hit[ci]),
                .fetch_stall (cpu_icache_stall[ci]),

                // Bus refill side
                .bus_valid   (ic_bus_valid[ci]),
                .bus_addr    (ic_bus_addr[ci]),
                .bus_rdata   (ic_bus_rdata[ci]),
                .bus_ready   (ic_bus_ready[ci]),
                .bus_wen     (ic_bus_wen[ci]),
                .bus_size    (ic_bus_size[ci]),

                // Invalidation
                .inv_all     (icache_inv_all[ci]),
                .inv_line    (icache_inv_line[ci]),
                .inv_addr    (icache_inv_addr[ci]),
                .inv_size    (icache_inv_size[ci]),

                // Stats
                .stat_hits   (ic_stat_hits[ci]),
                .stat_misses (ic_stat_misses[ci])
            );

`ifndef SYNTHESIS
            always @(posedge sys_clk) begin
                if (!rst_h && cpu_icache_inv_line[ci]
                           && core_tile_icache_inv_line[ci])
                    $error("core %0d CPU and tile writes completed in the same cycle",
                           ci);
            end
`endif

        end // g_core
    endgenerate

    // ========================================================================
    // Micro-Core Clusters
    // ========================================================================
    wire        cluster_bus_valid [0:NUM_CLUSTERS-1];
    wire [63:0] cluster_bus_addr  [0:NUM_CLUSTERS-1];
    wire [63:0] cluster_bus_wdata [0:NUM_CLUSTERS-1];
    wire        cluster_bus_wen   [0:NUM_CLUSTERS-1];
    wire [1:0]  cluster_bus_size  [0:NUM_CLUSTERS-1];
    wire        cluster_bus_requester_valid[0:NUM_CLUSTERS-1];
    wire [MP64_CORE_ID_BITS-1:0]
                cluster_bus_requester_id[0:NUM_CLUSTERS-1];
    wire [63:0] cluster_bus_rdata [0:NUM_CLUSTERS-1];
    wire        cluster_bus_ready [0:NUM_CLUSTERS-1];

    // Per-cluster tile memory ports (from shared tile engines)
    wire        cluster_tile_req    [0:NUM_CLUSTERS-1];
    wire [31:0] cluster_tile_addr   [0:NUM_CLUSTERS-1];
    wire        cluster_tile_wen    [0:NUM_CLUSTERS-1];
    wire [511:0]cluster_tile_wdata  [0:NUM_CLUSTERS-1];
    wire [511:0]cluster_tile_rdata  [0:NUM_CLUSTERS-1];
    wire        cluster_tile_ack    [0:NUM_CLUSTERS-1];
    wire        cluster_tile_error  [0:NUM_CLUSTERS-1];
    wire [63:0] cluster_tile_fault_addr[0:NUM_CLUSTERS-1];

    // Per-cluster ext tile memory ports
    wire        cluster_ext_tile_req   [0:NUM_CLUSTERS-1];
    wire [63:0] cluster_ext_tile_addr  [0:NUM_CLUSTERS-1];
    wire        cluster_ext_tile_wen   [0:NUM_CLUSTERS-1];
    wire [511:0]cluster_ext_tile_wdata [0:NUM_CLUSTERS-1];
    wire [511:0]cluster_ext_tile_rdata [0:NUM_CLUSTERS-1];
    wire        cluster_ext_tile_ack   [0:NUM_CLUSTERS-1];
    wire        cluster_ext_tile_error [0:NUM_CLUSTERS-1];
    wire [63:0] cluster_ext_tile_fault_addr[0:NUM_CLUSTERS-1];
    wire        cluster_tile_source_cancel[0:NUM_CLUSTERS-1];

    wire         cluster_tacc_xfer_req [0:NUM_CLUSTERS-1];
    wire         cluster_tacc_xfer_store[0:NUM_CLUSTERS-1];
    wire         cluster_tacc_xfer_ext [0:NUM_CLUSTERS-1];
    wire [63:0]  cluster_tacc_xfer_base[0:NUM_CLUSTERS-1];
    wire [2:0]   cluster_tacc_xfer_format_ew[0:NUM_CLUSTERS-1];
    wire [7:0]   cluster_tacc_xfer_token[0:NUM_CLUSTERS-1];
    wire [2047:0] cluster_tacc_xfer_store_image[0:NUM_CLUSTERS-1];
    wire         cluster_tacc_xfer_cancel[0:NUM_CLUSTERS-1];
    wire         cluster_tacc_xfer_finish[0:NUM_CLUSTERS-1];
    wire         cluster_tacc_xfer_done[0:NUM_CLUSTERS-1];
    wire [7:0]   cluster_tacc_xfer_response_token[0:NUM_CLUSTERS-1];
    wire [2:0]   cluster_tacc_xfer_fault[0:NUM_CLUSTERS-1];
    wire [63:0]  cluster_tacc_xfer_fault_addr[0:NUM_CLUSTERS-1];
    wire [2047:0] cluster_tacc_xfer_load_image[0:NUM_CLUSTERS-1];
    wire         cluster_tacc_xfer_stall_cycle[0:NUM_CLUSTERS-1];

    genvar ki;
    generate
        for (ki = 0; ki < NUM_CLUSTERS; ki = ki + 1) begin : g_cluster
            mp64_cluster #(
                .N             (CORES_PER_CLUSTER),
                .CLUSTER_ID_BASE(NUM_CORES[7:0] +
                                 ki[7:0] * CORES_PER_CLUSTER[7:0]),
                .TACC_BANK0_LIMIT(BANK0_SIZE),
                .TACC_EXT_LIMIT  (EXT_MEM_LIMIT),
                .TACC_VRAM_BASE  ({32'd0, MP64_VRAM_BASE_ADDR}),
                .TACC_VRAM_LIMIT (VRAM_LIMIT),
                .TACC_HBW_LIMIT  (HBW_LIMIT)
            ) u_cluster (
                .clk         (sys_clk),
                .rst         (rst_h),
                .cluster_en  (sysinfo_cluster_en[ki]),
                .tile_engine_reset(1'b0),
                .micro_reset (cluster_micro_reset[
                    ki*CORES_PER_CLUSTER +: CORES_PER_CLUSTER]),

                .bus_valid   (cluster_bus_valid[ki]),
                .bus_addr    (cluster_bus_addr[ki]),
                .bus_wdata   (cluster_bus_wdata[ki]),
                .bus_wen     (cluster_bus_wen[ki]),
                .bus_size    (cluster_bus_size[ki]),
                .bus_requester_valid(
                    cluster_bus_requester_valid[ki]),
                .bus_requester_id(cluster_bus_requester_id[ki]),
                .bus_rdata   (cluster_bus_rdata[ki]),
                .bus_ready   (cluster_bus_ready[ki]),

                .irq_timer   ({CORES_PER_CLUSTER{irq_timer_w}}),
                .irq_ipi     ({CORES_PER_CLUSTER{1'b0}}),
                .ef_flags    (4'b0000),

                // Tile memory ports
                .tile_req    (cluster_tile_req[ki]),
                .tile_addr   (cluster_tile_addr[ki]),
                .tile_wen    (cluster_tile_wen[ki]),
                .tile_wdata  (cluster_tile_wdata[ki]),
                .tile_rdata  (cluster_tile_rdata[ki]),
                .tile_ack    (cluster_tile_ack[ki]),
                .tile_error  (cluster_tile_error[ki]),
                .tile_fault_addr(cluster_tile_fault_addr[ki]),

                .ext_tile_req  (cluster_ext_tile_req[ki]),
                .ext_tile_addr (cluster_ext_tile_addr[ki]),
                .ext_tile_wen  (cluster_ext_tile_wen[ki]),
                .ext_tile_wdata(cluster_ext_tile_wdata[ki]),
                .ext_tile_rdata(cluster_ext_tile_rdata[ki]),
                .ext_tile_ack  (cluster_ext_tile_ack[ki]),
                .ext_tile_error(cluster_ext_tile_error[ki]),
                .ext_tile_fault_addr(
                    cluster_ext_tile_fault_addr[ki]),
                .tile_source_cancel(
                    cluster_tile_source_cancel[ki]),

                .tacc_xfer_req (cluster_tacc_xfer_req[ki]),
                .tacc_xfer_store(cluster_tacc_xfer_store[ki]),
                .tacc_xfer_ext (cluster_tacc_xfer_ext[ki]),
                .tacc_xfer_base(cluster_tacc_xfer_base[ki]),
                .tacc_xfer_format_ew(
                    cluster_tacc_xfer_format_ew[ki]),
                .tacc_xfer_token(cluster_tacc_xfer_token[ki]),
                .tacc_xfer_store_image(
                    cluster_tacc_xfer_store_image[ki]),
                .tacc_xfer_cancel(cluster_tacc_xfer_cancel[ki]),
                .tacc_xfer_finish(cluster_tacc_xfer_finish[ki]),
                .tacc_xfer_done(cluster_tacc_xfer_done[ki]),
                .tacc_xfer_response_token(
                    cluster_tacc_xfer_response_token[ki]),
                .tacc_xfer_fault(cluster_tacc_xfer_fault[ki]),
                .tacc_xfer_fault_addr(
                    cluster_tacc_xfer_fault_addr[ki]),
                .tacc_xfer_load_image(
                    cluster_tacc_xfer_load_image[ki]),
                .tacc_xfer_stall_cycle(
                    cluster_tacc_xfer_stall_cycle[ki])
            );
        end
    endgenerate

    // ========================================================================
    // Bus Arbiter — pack N_BUS_PORTS flat signals
    // ========================================================================
    // Port layout: [0..NUM_CORES-1] = muxed CPU data/I-cache buses
    //              [NUM_CORES..NIC_BUS_PORT-1] = cluster buses
    //              [NIC_BUS_PORT] = NIC byte-DMA master
    //              [DISK_BUS_PORT] = disk byte-DMA master
    //
    // WAIT — I-caches also need bus access for refills.  The bus has
    // N_PORTS master ports.  We need:
    //   - 4 CPU data ports
    //   - 4 I-cache refill ports
    //   - 3 cluster ports
    //   = 11 ports total
    //
    // But mp64_bus default is N_PORTS=4 (or NUM_CORES+NUM_CLUSTERS=7).
    // I-cache refill accesses go through the *same CPU data port* since
    // the CPU stalls while the I-cache refills — the I-cache refill bus
    // and CPU data bus are TIME-MULTIPLEXED, not simultaneous.
    //
    // So we mux each core's data bus vs icache refill onto one port.

    // Per-core muxed bus signals (icache has priority when it's refilling)
    wire [63:0] muxed_addr  [0:NUM_CORES-1];
    wire [63:0] muxed_wdata [0:NUM_CORES-1];
    wire [1:0]  muxed_size  [0:NUM_CORES-1];
    wire        muxed_valid [0:NUM_CORES-1];
    wire        muxed_wen   [0:NUM_CORES-1];
    wire        muxed_port_io [0:NUM_CORES-1];

    genvar mi;
    generate
        for (mi = 0; mi < NUM_CORES; mi = mi + 1) begin : g_bus_mux
            wire bus_resp_ready;
            wire [63:0] bus_resp_rdata;

            mp64_core_bus_mux u_core_bus_mux (
                .clk          (sys_clk),
                .rst_n        (sys_rst_n),
                .core_valid   (core_bus_valid[mi]),
                .core_addr    (core_bus_addr[mi]),
                .core_wdata   (core_bus_wdata[mi]),
                .core_wen     (core_bus_wen[mi]),
                .core_size    (core_bus_size[mi]),
                .core_port_io (core_bus_port_io[mi]),
                .ic_valid     (ic_bus_valid[mi]),
                .ic_addr      (ic_bus_addr[mi]),
                .ic_wen       (ic_bus_wen[mi]),
                .ic_size      (ic_bus_size[mi]),
                .mux_valid    (muxed_valid[mi]),
                .mux_addr     (muxed_addr[mi]),
                .mux_wdata    (muxed_wdata[mi]),
                .mux_wen      (muxed_wen[mi]),
                .mux_size     (muxed_size[mi]),
                .mux_port_io  (muxed_port_io[mi]),
                .bus_rdata    (bus_resp_rdata),
                .bus_ready    (bus_resp_ready),
                .core_rdata   (core_bus_rdata[mi]),
                .core_ready   (core_bus_ready[mi]),
                .ic_rdata     (ic_bus_rdata[mi]),
                .ic_ready     (ic_bus_ready[mi])
            );
        end
    endgenerate

    // Flat-pack all N_BUS_PORTS master signals for the bus arbiter
    wire [N_BUS_PORTS-1:0]    bus_cpu_valid;
    wire [N_BUS_PORTS*64-1:0] bus_cpu_addr;
    wire [N_BUS_PORTS*64-1:0] bus_cpu_wdata;
    wire [N_BUS_PORTS-1:0]    bus_cpu_wen;
    wire [N_BUS_PORTS*2-1:0]  bus_cpu_size;
    wire [N_BUS_PORTS-1:0]    bus_cpu_port_io;
    wire [N_BUS_PORTS-1:0]    bus_cpu_requester_valid;
    wire [N_BUS_PORTS*MP64_CORE_ID_BITS-1:0]
                              bus_cpu_requester_id;
    wire [N_BUS_PORTS*64-1:0] bus_cpu_rdata;
    wire [N_BUS_PORTS-1:0]    bus_cpu_ready;
    wire [N_BUS_PORTS-1:0]    bus_err_w;
    wire [63:0]               nic_dma_bus_rdata;
    wire [63:0]               disk_dma_bus_rdata;

    genvar pi;
    generate
        // Ports [0..NUM_CORES-1]: muxed CPU/I-cache
        for (pi = 0; pi < NUM_CORES; pi = pi + 1) begin : g_pack_core
            assign bus_cpu_valid[pi]           = muxed_valid[pi];
            assign bus_cpu_addr [pi*64 +: 64]  = muxed_addr[pi];
            assign bus_cpu_wdata[pi*64 +: 64]  = muxed_wdata[pi];
            assign bus_cpu_wen  [pi]           = muxed_wen[pi];
            assign bus_cpu_size [pi*2  +: 2]   = muxed_size[pi];
            assign bus_cpu_port_io[pi]         = muxed_port_io[pi];
            assign bus_cpu_requester_valid[pi] = 1'b1;
            assign bus_cpu_requester_id[
                pi*MP64_CORE_ID_BITS +: MP64_CORE_ID_BITS] =
                    pi[MP64_CORE_ID_BITS-1:0];
        end

        // Ports [NUM_CORES..NIC_BUS_PORT-1]: clusters
        for (pi = 0; pi < NUM_CLUSTERS; pi = pi + 1) begin : g_pack_cluster
            localparam P = NUM_CORES + pi;
            assign bus_cpu_valid[P]           = cluster_bus_valid[pi];
            assign bus_cpu_addr [P*64 +: 64]  = cluster_bus_addr[pi];
            assign bus_cpu_wdata[P*64 +: 64]  = cluster_bus_wdata[pi];
            assign bus_cpu_wen  [P]           = cluster_bus_wen[pi];
            assign bus_cpu_size [P*2  +: 2]   = cluster_bus_size[pi];
            assign bus_cpu_port_io[P]          = 1'b0;
            assign bus_cpu_requester_valid[P] =
                cluster_bus_requester_valid[pi];
            assign bus_cpu_requester_id[
                P*MP64_CORE_ID_BITS +: MP64_CORE_ID_BITS] =
                    cluster_bus_requester_id[pi];
        end
    endgenerate

    // Pack the NIC as a normal byte-wide memory master.  mp64_memory expects
    // sub-word write data in the low bits and returns the containing 64-bit
    // word, so reads select the addressed byte lane on acknowledgement.
    assign bus_cpu_valid[NIC_BUS_PORT]                 = nic_dma_req;
    assign bus_cpu_addr [NIC_BUS_PORT*64 +: 64]        = nic_dma_addr;
    assign bus_cpu_wdata[NIC_BUS_PORT*64 +: 64]        = {56'd0, nic_dma_wdata};
    assign bus_cpu_wen  [NIC_BUS_PORT]                 = nic_dma_wen;
    assign bus_cpu_size [NIC_BUS_PORT*2 +: 2]          = BUS_BYTE;
    assign bus_cpu_port_io[NIC_BUS_PORT]               = 1'b0;
    assign bus_cpu_requester_valid[NIC_BUS_PORT]        = 1'b0;
    assign bus_cpu_requester_id[
        NIC_BUS_PORT*MP64_CORE_ID_BITS +: MP64_CORE_ID_BITS] =
            {MP64_CORE_ID_BITS{1'b0}};
    assign nic_dma_bus_rdata = bus_cpu_rdata[NIC_BUS_PORT*64 +: 64];
    assign nic_dma_rdata = nic_dma_bus_rdata[nic_dma_addr[2:0]*8 +: 8];
    assign nic_dma_ack   = bus_cpu_ready[NIC_BUS_PORT];

    // Pack disk DMA identically: byte writes are presented in the low lane for
    // mp64_memory's RMW path, and reads select the addressed lane on response.
    assign bus_cpu_valid[DISK_BUS_PORT]                 = disk_dma_req;
    assign bus_cpu_addr [DISK_BUS_PORT*64 +: 64]        = disk_dma_addr;
    assign bus_cpu_wdata[DISK_BUS_PORT*64 +: 64]        = {56'd0, disk_dma_wdata};
    assign bus_cpu_wen  [DISK_BUS_PORT]                 = disk_dma_wen;
    assign bus_cpu_size [DISK_BUS_PORT*2 +: 2]          = BUS_BYTE;
    assign bus_cpu_port_io[DISK_BUS_PORT]               = 1'b0;
    assign bus_cpu_requester_valid[DISK_BUS_PORT]        = 1'b0;
    assign bus_cpu_requester_id[
        DISK_BUS_PORT*MP64_CORE_ID_BITS +: MP64_CORE_ID_BITS] =
            {MP64_CORE_ID_BITS{1'b0}};
    assign disk_dma_bus_rdata = bus_cpu_rdata[DISK_BUS_PORT*64 +: 64];
    assign disk_dma_rdata = disk_dma_bus_rdata[disk_dma_addr[2:0]*8 +: 8];
    assign disk_dma_ack = bus_cpu_ready[DISK_BUS_PORT];
    assign disk_dma_err = bus_err_w[DISK_BUS_PORT];

    // Unpack bus responses back to cores and clusters
    generate
        for (pi = 0; pi < NUM_CORES; pi = pi + 1) begin : g_unpack_core
            // Route through the mux demux logic above
            assign g_bus_mux[pi].bus_resp_rdata = bus_cpu_rdata[pi*64 +: 64];
            assign g_bus_mux[pi].bus_resp_ready = bus_cpu_ready[pi];
        end
        for (pi = 0; pi < NUM_CLUSTERS; pi = pi + 1) begin : g_unpack_cluster
            localparam P = NUM_CORES + pi;
            assign cluster_bus_rdata[pi] = bus_cpu_rdata[P*64 +: 64];
            assign cluster_bus_ready[pi] = bus_cpu_ready[P];
        end
    endgenerate

    // ---- Bus arbiter instance -----------------------------------------------
    wire        bus_mem_req;
    wire [63:0] bus_mem_addr;
    wire [63:0] bus_mem_wdata;
    wire        bus_mem_wen;
    wire [1:0]  bus_mem_size;
    wire [63:0] bus_mem_rdata;
    wire        bus_mem_ack;

    wire        bus_mmio_req;
    wire [11:0] bus_mmio_addr;
    wire [63:0] bus_mmio_wdata;
    wire        bus_mmio_wen;
    wire [1:0]  bus_mmio_size;
    wire [63:0] bus_mmio_rdata;
    wire        bus_mmio_ack;

    wire        bus_mmio_port_io;
    wire        bus_mmio_requester_valid;
    wire [MP64_CORE_ID_BITS-1:0] bus_mmio_requester_id;

    mp64_bus #(
        .N_PORTS   (N_BUS_PORTS),
        .PORT_BITS (PORT_BITS),
        .REQUESTER_ID_BITS(MP64_CORE_ID_BITS)
    ) u_bus (
        .clk       (sys_clk),
        .rst_n     (sys_rst_n),

        .cpu_valid (bus_cpu_valid),
        .cpu_addr  (bus_cpu_addr),
        .cpu_wdata (bus_cpu_wdata),
        .cpu_wen   (bus_cpu_wen),
        .cpu_size  (bus_cpu_size),
        .cpu_port_io(bus_cpu_port_io),
        .cpu_requester_valid(bus_cpu_requester_valid),
        .cpu_requester_id(bus_cpu_requester_id),
        .cpu_rdata (bus_cpu_rdata),
        .cpu_ready (bus_cpu_ready),

        .mem_req   (bus_mem_req),
        .mem_addr  (bus_mem_addr),
        .mem_wdata (bus_mem_wdata),
        .mem_wen   (bus_mem_wen),
        .mem_size  (bus_mem_size),
        .mem_rdata (bus_mem_rdata),
        .mem_ack   (bus_mem_ack),

        .mmio_req   (bus_mmio_req),
        .mmio_addr  (bus_mmio_addr),
        .mmio_wdata (bus_mmio_wdata),
        .mmio_wen   (bus_mmio_wen),
        .mmio_size  (bus_mmio_size),
        .mmio_port_io(bus_mmio_port_io),
        .mmio_requester_valid(bus_mmio_requester_valid),
        .mmio_requester_id(bus_mmio_requester_id),
        .mmio_rdata (bus_mmio_rdata),
        .mmio_ack   (bus_mmio_ack),

        .qos_csr_wen   (1'b0),
        .qos_csr_addr  (8'd0),
        .qos_csr_wdata (64'd0),
        .qos_csr_rdata (),

        .bus_err    (bus_err_w)
    );

    // ========================================================================
    // Memory Subsystem
    // ========================================================================
    wire        tile_mem_req;
    wire [31:0] tile_mem_addr;
    wire        tile_mem_wen;
    wire [511:0]tile_mem_wdata;
    wire [511:0]tile_mem_rdata;
    wire        tile_mem_ack;
    wire        tile_mem_accept;
    wire        tile_mem_cancel;
    wire        tile_mem_error;
    wire [63:0] tile_mem_fault_addr;

    wire        mem_ext_req;
    wire [63:0] mem_ext_addr;
    wire [63:0] mem_ext_wdata;
    wire        mem_ext_wen;
    wire [1:0]  mem_ext_size;
    wire [63:0] mem_ext_rdata;
    wire        mem_ext_ack;

    mp64_memory #(
        .BANK_DEPTH (MEM_DEPTH)
    ) u_memory (
        .clk       (sys_clk),
        .rst_n     (sys_rst_n),

        // CPU port (from bus arbiter)
        .cpu_req   (bus_mem_req),
        .cpu_addr  (bus_mem_addr),
        .cpu_wdata (bus_mem_wdata),
        .cpu_wen   (bus_mem_wen),
        .cpu_size  (bus_mem_size),
        .cpu_rdata (bus_mem_rdata),
        .cpu_ack   (bus_mem_ack),

        // Tile port (from tile engine)
        .tile_req  (tile_mem_req),
        .tile_addr (tile_mem_addr),
        .tile_wen  (tile_mem_wen),
        .tile_wdata(tile_mem_wdata),
        .tile_accept(tile_mem_accept),
        .tile_rdata(tile_mem_rdata),
        .tile_ack  (tile_mem_ack),
        .tile_error(tile_mem_error),
        .tile_fault_addr(tile_mem_fault_addr),

        // External forward
        .ext_req   (mem_ext_req),
        .ext_addr  (mem_ext_addr),
        .ext_wdata (mem_ext_wdata),
        .ext_wen   (mem_ext_wen),
        .ext_size  (mem_ext_size),
        .ext_rdata (mem_ext_rdata),
        .ext_ack   (mem_ext_ack)
    );

    // ========================================================================
    // External Memory Controller
    // ========================================================================
    // Tile engine ext port (512-bit burst)
    wire        ext_tile_req;
    wire [63:0] ext_tile_addr;
    wire        ext_tile_wen;
    wire [511:0]ext_tile_wdata;
    wire [511:0]ext_tile_rdata;
    wire        ext_tile_ack;
    wire        ext_tile_accept;
    wire        ext_tile_cancel;
    wire        ext_tile_error;
    wire [63:0] ext_tile_fault_addr;
    wire        ext_tile_word_done;

    // PHY interface (internal 32-bit addr + phy_ack)
    wire        extmem_phy_req;
    wire [31:0] extmem_phy_addr;
    wire [63:0] extmem_phy_wdata;
    wire        extmem_phy_wen;
    wire [63:0] extmem_phy_rdata;
    wire        extmem_phy_ack;
    wire        extmem_phy_cancel;
    wire [3:0]  extmem_phy_burst_len;

    mp64_extmem u_extmem (
        .clk       (sys_clk),
        .rst_n     (sys_rst_n),

        // CPU port (forwarded from memory subsystem)
        .cpu_req   (mem_ext_req),
        .cpu_addr  (mem_ext_addr[31:0]),
        .cpu_wdata (mem_ext_wdata),
        .cpu_wen   (mem_ext_wen),
        .cpu_size  (mem_ext_size),
        .cpu_rdata (mem_ext_rdata),
        .cpu_ack   (mem_ext_ack),

        // Tile port
        .tile_req  (ext_tile_req),
        .tile_addr (ext_tile_addr[31:0]),
        .tile_wdata(ext_tile_wdata),
        .tile_wen  (ext_tile_wen),
        .tile_cancel(ext_tile_cancel),
        .tile_accept(ext_tile_accept),
        .tile_rdata(ext_tile_rdata),
        .tile_ack  (ext_tile_ack),
        .tile_error(ext_tile_error),
        .tile_fault_addr(ext_tile_fault_addr),
        .tile_word_done(ext_tile_word_done),

        // PHY
        .phy_req       (extmem_phy_req),
        .phy_addr      (extmem_phy_addr),
        .phy_wdata     (extmem_phy_wdata),
        .phy_wen       (extmem_phy_wen),
        .phy_ready     (phy_ready),
        .phy_rdata     (extmem_phy_rdata),
        .phy_ack       (extmem_phy_ack),
        .phy_error     (phy_error),
        .phy_cancel    (extmem_phy_cancel),
        .phy_cancel_done(phy_cancel_done),
        .phy_burst_len (extmem_phy_burst_len)
    );

    // Adapt internal 32-bit PHY to external 64-bit / rvalid/ready interface
    assign phy_req       = extmem_phy_req;
    assign phy_addr      = {32'd0, extmem_phy_addr};
    assign phy_wen       = extmem_phy_wen;
    assign phy_wdata     = extmem_phy_wdata;
    assign phy_burst_len = {4'd0, extmem_phy_burst_len};
    assign phy_cancel    = extmem_phy_cancel;
    assign extmem_phy_rdata = phy_rdata;
    assign extmem_phy_ack   = phy_rvalid;

    // ========================================================================
    // Tile Memory Port Arbiter
    // ========================================================================
    // Production has seven physical sources: full-core-private engines occupy
    // lanes 0..3 and microcluster-private engines lanes 4..6.  Parameter-
    // reduced verification builds compact only the instantiated requestors.
    localparam integer TILE_SOURCE_COUNT = NUM_CORES + NUM_CLUSTERS;
    localparam integer TILE_OWNER_BITS =
        (TILE_SOURCE_COUNT <= 1) ? 1 : $clog2(TILE_SOURCE_COUNT);
    localparam integer FULL_TILE_SOURCE_COUNT = NUM_CORES;
    localparam integer CLUSTER_TILE_SOURCE_BASE = NUM_CORES;

    wire        core_tile_req      [0:NUM_CORES-1];
    wire [31:0] core_tile_addr     [0:NUM_CORES-1];
    wire        core_tile_wen      [0:NUM_CORES-1];
    wire [511:0]core_tile_wdata    [0:NUM_CORES-1];
    wire        core_ext_tile_req  [0:NUM_CORES-1];
    wire [63:0] core_ext_tile_addr [0:NUM_CORES-1];
    wire        core_ext_tile_wen  [0:NUM_CORES-1];
    wire [511:0]core_ext_tile_wdata[0:NUM_CORES-1];
    wire        core_tile_source_cancel[0:NUM_CORES-1];
    wire        core_tacc_xfer_req [0:NUM_CORES-1];
    wire        core_tacc_xfer_store[0:NUM_CORES-1];
    wire        core_tacc_xfer_ext [0:NUM_CORES-1];
    wire [63:0] core_tacc_xfer_base[0:NUM_CORES-1];
    wire [2:0]  core_tacc_xfer_format_ew[0:NUM_CORES-1];
    wire [7:0]  core_tacc_xfer_token[0:NUM_CORES-1];
    wire [2047:0] core_tacc_xfer_store_image[0:NUM_CORES-1];
    wire        core_tacc_xfer_cancel[0:NUM_CORES-1];
    wire        core_tacc_xfer_finish[0:NUM_CORES-1];
    wire        core_tacc_xfer_done[0:NUM_CORES-1];
    wire [7:0]  core_tacc_xfer_response_token[0:NUM_CORES-1];
    wire [2:0]  core_tacc_xfer_fault[0:NUM_CORES-1];
    wire [63:0] core_tacc_xfer_fault_addr[0:NUM_CORES-1];
    wire [2047:0] core_tacc_xfer_load_image[0:NUM_CORES-1];

    wire [TILE_SOURCE_COUNT-1:0]     tile_src_req_bus;
    wire [TILE_SOURCE_COUNT*32-1:0]  tile_src_addr_bus;
    wire [TILE_SOURCE_COUNT-1:0]     tile_src_wen_bus;
    wire [TILE_SOURCE_COUNT*512-1:0] tile_src_wdata_bus;
    wire [TILE_SOURCE_COUNT-1:0]     ext_tile_src_req_bus;
    wire [TILE_SOURCE_COUNT*64-1:0]  ext_tile_src_addr_bus;
    wire [TILE_SOURCE_COUNT-1:0]     ext_tile_src_wen_bus;
    wire [TILE_SOURCE_COUNT*512-1:0] ext_tile_src_wdata_bus;
    wire [TILE_SOURCE_COUNT-1:0] tile_src_ack;
    wire [TILE_SOURCE_COUNT-1:0] ext_tile_src_ack;
    wire [TILE_SOURCE_COUNT-1:0] tile_src_error;
    wire [TILE_SOURCE_COUNT-1:0] ext_tile_src_error;
    wire [TILE_SOURCE_COUNT*64-1:0] tile_src_fault_addr;
    wire [TILE_SOURCE_COUNT*64-1:0] ext_tile_src_fault_addr;
    wire [TILE_SOURCE_COUNT-1:0] tile_src_cancel;
    wire [TILE_SOURCE_COUNT-1:0] tile_src_cancel_done;
    wire [TILE_SOURCE_COUNT-1:0] tile_src_accept;
    wire [TILE_SOURCE_COUNT-1:0] tile_engine_source_cancel;
    wire [TILE_SOURCE_COUNT-1:0] tacc_stage_req;
    wire [TILE_SOURCE_COUNT-1:0] tacc_stage_store;
    wire [TILE_SOURCE_COUNT-1:0] tacc_stage_ext;
    wire [TILE_SOURCE_COUNT*64-1:0] tacc_stage_base;
    wire [TILE_SOURCE_COUNT*3-1:0] tacc_stage_format_ew;
    wire [TILE_SOURCE_COUNT*8-1:0] tacc_stage_token;
    wire [TILE_SOURCE_COUNT*2048-1:0] tacc_stage_store_image;
    wire [TILE_SOURCE_COUNT-1:0] tacc_stage_cancel;
    wire [TILE_SOURCE_COUNT-1:0] tacc_stage_finish;
    wire [TILE_SOURCE_COUNT-1:0] tacc_stage_done;
    wire [TILE_SOURCE_COUNT-1:0] tacc_stage_stall_cycle;
    wire [TILE_SOURCE_COUNT*8-1:0] tacc_stage_response_token;
    wire [TILE_SOURCE_COUNT*3-1:0] tacc_stage_fault;
    wire [TILE_SOURCE_COUNT*64-1:0] tacc_stage_fault_addr;
    wire [2047:0] tacc_stage_result_image;
    wire [TILE_SOURCE_COUNT-1:0] tacc_beat_req;
    wire        tacc_beat_ext;
    wire [63:0] tacc_beat_addr;
    wire        tacc_beat_wen;
    wire [511:0] tacc_beat_wdata;
    wire [TILE_SOURCE_COUNT-1:0] tacc_port_cancel;
    wire [TILE_SOURCE_COUNT-1:0] tacc_port_ack =
        tile_src_ack | ext_tile_src_ack;
    wire [TILE_SOURCE_COUNT-1:0] tacc_port_error =
        tile_src_error | ext_tile_src_error;
    wire [TILE_SOURCE_COUNT*64-1:0] tacc_port_fault_addr;
    wire [TILE_SOURCE_COUNT-1:0] cluster_disable_cancel;
    wire       tile_write_commit;
    wire [TILE_OWNER_BITS-1:0] tile_write_owner;
    wire       tile_write_ext;
    wire [63:0]tile_write_addr;
    wire       tile_ext_word_owner_valid;
    wire [TILE_OWNER_BITS-1:0] tile_ext_word_owner;

    genvar tai;
    generate
        for (tai = 0; tai < NUM_CORES;
             tai = tai + 1) begin : g_full_tile_arb_lane
            localparam [TILE_OWNER_BITS-1:0] TILE_OWNER = tai;

            assign tile_src_req_bus[tai] =
                tacc_beat_req[tai] ? !tacc_beat_ext :
                core_tile_req[tai];
            assign tile_src_addr_bus[tai*32 +: 32] =
                tacc_beat_req[tai] ? tacc_beat_addr[31:0] :
                core_tile_addr[tai];
            assign tile_src_wen_bus[tai] =
                tacc_beat_req[tai] ? tacc_beat_wen :
                core_tile_wen[tai];
            assign tile_src_wdata_bus[tai*512 +: 512] =
                tacc_beat_req[tai] ? tacc_beat_wdata :
                core_tile_wdata[tai];
            assign ext_tile_src_req_bus[tai] =
                tacc_beat_req[tai] ? tacc_beat_ext :
                core_ext_tile_req[tai];
            assign ext_tile_src_addr_bus[tai*64 +: 64] =
                tacc_beat_req[tai] ? tacc_beat_addr :
                core_ext_tile_addr[tai];
            assign ext_tile_src_wen_bus[tai] =
                tacc_beat_req[tai] ? tacc_beat_wen :
                core_ext_tile_wen[tai];
            assign ext_tile_src_wdata_bus[tai*512 +: 512] =
                tacc_beat_req[tai] ? tacc_beat_wdata :
                core_ext_tile_wdata[tai];

            assign tacc_stage_req[tai] = core_tacc_xfer_req[tai];
            assign tacc_stage_store[tai] = core_tacc_xfer_store[tai];
            assign tacc_stage_ext[tai] = core_tacc_xfer_ext[tai];
            assign tacc_stage_base[tai*64 +: 64] =
                core_tacc_xfer_base[tai];
            assign tacc_stage_format_ew[tai*3 +: 3] =
                core_tacc_xfer_format_ew[tai];
            assign tacc_stage_token[tai*8 +: 8] =
                core_tacc_xfer_token[tai];
            assign tacc_stage_store_image[tai*2048 +: 2048] =
                core_tacc_xfer_store_image[tai];
            assign tacc_stage_cancel[tai] =
                core_tacc_xfer_cancel[tai];
            assign tacc_stage_finish[tai] =
                core_tacc_xfer_finish[tai];
            assign core_tacc_xfer_done[tai] =
                tacc_stage_done[tai];
            assign core_tacc_xfer_response_token[tai] =
                tacc_stage_response_token[tai*8 +: 8];
            assign core_tacc_xfer_fault[tai] =
                tacc_stage_fault[tai*3 +: 3];
            assign core_tacc_xfer_fault_addr[tai] =
                tacc_stage_fault_addr[tai*64 +: 64];
            assign core_tacc_xfer_load_image[tai] =
                tacc_stage_result_image;
            assign core_tacc_xfer_stall_cycle[tai] =
                tacc_stage_stall_cycle[tai];
            assign core_perf_extmem_word[tai] =
                ext_tile_word_done &&
                tile_ext_word_owner_valid &&
                tile_ext_word_owner == TILE_OWNER;
            assign cluster_disable_cancel[tai] = 1'b0;
            assign tile_engine_source_cancel[tai] =
                core_tile_source_cancel[tai];
        end

        for (tai = 0; tai < NUM_CLUSTERS;
             tai = tai + 1) begin : g_cluster_tile_arb_lane
            localparam integer TILE_LANE = CLUSTER_TILE_SOURCE_BASE + tai;
            assign tile_src_req_bus[TILE_LANE] =
                tacc_beat_req[TILE_LANE] ? !tacc_beat_ext :
                cluster_tile_req[tai];
            assign tile_src_addr_bus[TILE_LANE*32 +: 32] =
                tacc_beat_req[TILE_LANE] ? tacc_beat_addr[31:0] :
                cluster_tile_addr[tai];
            assign tile_src_wen_bus[TILE_LANE] =
                tacc_beat_req[TILE_LANE] ? tacc_beat_wen :
                cluster_tile_wen[tai];
            assign tile_src_wdata_bus[TILE_LANE*512 +: 512] =
                tacc_beat_req[TILE_LANE] ? tacc_beat_wdata :
                cluster_tile_wdata[tai];
            assign ext_tile_src_req_bus[TILE_LANE] =
                tacc_beat_req[TILE_LANE] ? tacc_beat_ext :
                cluster_ext_tile_req[tai];
            assign ext_tile_src_addr_bus[TILE_LANE*64 +: 64] =
                tacc_beat_req[TILE_LANE] ? tacc_beat_addr :
                cluster_ext_tile_addr[tai];
            assign ext_tile_src_wen_bus[TILE_LANE] =
                tacc_beat_req[TILE_LANE] ? tacc_beat_wen :
                cluster_ext_tile_wen[tai];
            assign ext_tile_src_wdata_bus[TILE_LANE*512 +: 512] =
                tacc_beat_req[TILE_LANE] ? tacc_beat_wdata :
                cluster_ext_tile_wdata[tai];
            assign cluster_tile_rdata[tai] = tile_mem_rdata;
            assign cluster_tile_ack[tai] = tile_src_ack[TILE_LANE];
            assign cluster_tile_error[tai] =
                tile_src_error[TILE_LANE];
            assign cluster_tile_fault_addr[tai] =
                tile_src_fault_addr[TILE_LANE*64 +: 64];
            assign cluster_ext_tile_rdata[tai] = ext_tile_rdata;
            assign cluster_ext_tile_ack[tai] =
                ext_tile_src_ack[TILE_LANE];
            assign cluster_ext_tile_error[tai] =
                ext_tile_src_error[TILE_LANE];
            assign cluster_ext_tile_fault_addr[tai] =
                ext_tile_src_fault_addr[TILE_LANE*64 +: 64];

            assign tacc_stage_req[TILE_LANE] =
                cluster_tacc_xfer_req[tai];
            assign tacc_stage_store[TILE_LANE] =
                cluster_tacc_xfer_store[tai];
            assign tacc_stage_ext[TILE_LANE] =
                cluster_tacc_xfer_ext[tai];
            assign tacc_stage_base[TILE_LANE*64 +: 64] =
                cluster_tacc_xfer_base[tai];
            assign tacc_stage_format_ew[TILE_LANE*3 +: 3] =
                cluster_tacc_xfer_format_ew[tai];
            assign tacc_stage_token[TILE_LANE*8 +: 8] =
                cluster_tacc_xfer_token[tai];
            assign tacc_stage_store_image[
                TILE_LANE*2048 +: 2048] =
                cluster_tacc_xfer_store_image[tai];
            assign tacc_stage_cancel[TILE_LANE] =
                cluster_tacc_xfer_cancel[tai] ||
                !sysinfo_cluster_en[tai];
            assign tacc_stage_finish[TILE_LANE] =
                cluster_tacc_xfer_finish[tai];
            assign cluster_tacc_xfer_done[tai] =
                tacc_stage_done[TILE_LANE];
            assign cluster_tacc_xfer_response_token[tai] =
                tacc_stage_response_token[TILE_LANE*8 +: 8];
            assign cluster_tacc_xfer_fault[tai] =
                tacc_stage_fault[TILE_LANE*3 +: 3];
            assign cluster_tacc_xfer_fault_addr[tai] =
                tacc_stage_fault_addr[TILE_LANE*64 +: 64];
            assign cluster_tacc_xfer_load_image[tai] =
                tacc_stage_result_image;
            assign cluster_tacc_xfer_stall_cycle[tai] =
                tacc_stage_stall_cycle[TILE_LANE];
            assign cluster_disable_cancel[TILE_LANE] =
                !sysinfo_cluster_en[tai];
            assign tile_engine_source_cancel[TILE_LANE] =
                cluster_tile_source_cancel[tai];
        end

        for (tai = 0; tai < TILE_SOURCE_COUNT;
             tai = tai + 1) begin : g_tacc_fault_mux
            assign tacc_port_fault_addr[tai*64 +: 64] =
                ext_tile_src_ack[tai] ?
                ext_tile_src_fault_addr[tai*64 +: 64] :
                tile_src_fault_addr[tai*64 +: 64];
        end
    endgenerate

    assign tile_src_cancel =
        tacc_port_cancel | cluster_disable_cancel |
        tile_engine_source_cancel;

    mp64_tacc_transfer #(
        .SOURCE_COUNT(TILE_SOURCE_COUNT),
        .OWNER_BITS  (TILE_OWNER_BITS),
        .TOKEN_BITS  (TACC_EPOCH_BITS)
    ) u_tacc_transfer (
        .clk                (sys_clk),
        .rst                (rst_h),
        .req                (tacc_stage_req),
        .req_store          (tacc_stage_store),
        .req_ext            (tacc_stage_ext),
        .req_base           (tacc_stage_base),
        .req_format_ew      (tacc_stage_format_ew),
        .req_token          (tacc_stage_token),
        .req_store_image    (tacc_stage_store_image),
        .req_cancel         (tacc_stage_cancel),
        .finish             (tacc_stage_finish),
        .port_ack           (tacc_port_ack),
        .port_error         (tacc_port_error),
        .port_fault_addr    (tacc_port_fault_addr),
        .tile_rdata         (tile_mem_rdata),
        .ext_rdata          (ext_tile_rdata),
        .port_cancel_done   (tile_src_cancel_done),
        .beat_req           (tacc_beat_req),
        .beat_ext           (tacc_beat_ext),
        .beat_addr          (tacc_beat_addr),
        .beat_wen           (tacc_beat_wen),
        .beat_wdata         (tacc_beat_wdata),
        .port_cancel        (tacc_port_cancel),
        .stall_cycle        (tacc_stage_stall_cycle),
        .done               (tacc_stage_done),
        .response_token     (tacc_stage_response_token),
        .response_fault     (tacc_stage_fault),
        .response_fault_addr(tacc_stage_fault_addr),
        .result_image       (tacc_stage_result_image)
    );

    mp64_tile_port_arbiter #(
        .SOURCE_COUNT(TILE_SOURCE_COUNT),
        .OWNER_BITS  (TILE_OWNER_BITS)
    ) u_tile_port_arbiter (
        .clk            (sys_clk),
        .rst            (rst_h),
        .src_tile_req   (tile_src_req_bus),
        .src_tile_addr  (tile_src_addr_bus),
        .src_tile_wen   (tile_src_wen_bus),
        .src_tile_wdata (tile_src_wdata_bus),
        .src_tile_ack   (tile_src_ack),
        .src_tile_error (tile_src_error),
        .src_tile_fault_addr(tile_src_fault_addr),
        .src_ext_req    (ext_tile_src_req_bus),
        .src_ext_addr   (ext_tile_src_addr_bus),
        .src_ext_wen    (ext_tile_src_wen_bus),
        .src_ext_wdata  (ext_tile_src_wdata_bus),
        .src_ext_ack    (ext_tile_src_ack),
        .src_ext_error  (ext_tile_src_error),
        .src_ext_fault_addr(ext_tile_src_fault_addr),
        .src_cancel     (tile_src_cancel),
        .src_accept     (tile_src_accept),
        .src_cancel_done(tile_src_cancel_done),
        .tile_req       (tile_mem_req),
        .tile_addr      (tile_mem_addr),
        .tile_wen       (tile_mem_wen),
        .tile_wdata     (tile_mem_wdata),
        .tile_accept    (tile_mem_accept),
        .tile_ack       (tile_mem_ack),
        .tile_error     (tile_mem_error),
        .tile_fault_addr(tile_mem_fault_addr),
        .tile_cancel    (tile_mem_cancel),
        .ext_req        (ext_tile_req),
        .ext_addr       (ext_tile_addr),
        .ext_wen        (ext_tile_wen),
        .ext_wdata      (ext_tile_wdata),
        .ext_accept     (ext_tile_accept),
        .ext_ack        (ext_tile_ack),
        .ext_error      (ext_tile_error),
        .ext_fault_addr (ext_tile_fault_addr),
        .ext_cancel     (ext_tile_cancel),
        .write_commit   (tile_write_commit),
        .write_owner    (tile_write_owner),
        .write_ext      (tile_write_ext),
        .write_addr     (tile_write_addr),
        .ext_word_owner_valid(tile_ext_word_owner_valid),
        .ext_word_owner (tile_ext_word_owner)
    );

`ifndef SYNTHESIS
    // A tile engine cannot issue ordinary lane traffic while its TACC
    // instruction owns that engine.  This makes every one-cycle stage beat
    // unconditionally capturable in the lane's sole arbiter slot; fail
    // closed if later integration ever breaks that invariant.
    always @(posedge sys_clk) begin
        if (!rst_h && |(tacc_beat_req & ~tile_src_accept))
            $fatal(1, "TACC beat was not captured by its source lane");
    end
`endif

    // Read data is shared physically, but ACK is returned only to the captured
    // owner.  Each full-core write invalidates only its paired private
    // I-cache; cluster tile writes remain explicitly noncoherent to full cores.
    genvar fti;
    generate
        for (fti = 0; fti < NUM_CORES;
             fti = fti + 1) begin : g_full_tile
            localparam [TILE_OWNER_BITS-1:0] TILE_OWNER = fti;
            localparam [TACC_CALLER_BITS-1:0] TACC_CALLER = fti;
            wire status_mine =
                core_tacc_status_raw[fti][TACC_STATUS_BIT_CLAIMED] &&
                core_tacc_status_raw[fti][
                    TACC_STATUS_OWNER_MSB:TACC_STATUS_OWNER_LSB] ==
                core_tile_caller_id[fti];

            assign core_tacc_status[fti] =
                (core_tacc_status_raw[fti] &
                 ~(64'd1 << TACC_STATUS_BIT_MINE)) |
                (status_mine ?
                 (64'd1 << TACC_STATUS_BIT_MINE) : 64'd0);
            assign core_tile_icache_inv_line[fti] =
                tile_write_commit && tile_write_owner == TILE_OWNER;
            assign core_tile_icache_inv_addr[fti] =
                tile_write_ext ? tile_write_addr
                               : {tile_write_addr[63:6], 6'd0};

            mp64_tile #(
                    .TACC_CALLER_BASE (TACC_CALLER),
                    .TACC_CALLER_COUNT(1),
                    .TACC_BANK0_LIMIT (BANK0_SIZE),
                    .TACC_EXT_LIMIT   (EXT_MEM_LIMIT),
                    .TACC_VRAM_BASE   (
                        {32'd0, MP64_VRAM_BASE_ADDR}),
                    .TACC_VRAM_LIMIT  (VRAM_LIMIT),
                    .TACC_HBW_LIMIT   (HBW_LIMIT)
                ) u_tile (
                    .clk       (sys_clk),
                    .rst_n     (sys_rst_n),
                    .engine_reset(core_domain_reset[fti]),
                    .caller_cancel(4'b0000),
                    .caller_epochs({(4*TACC_EPOCH_BITS){1'b0}}),
                    .engine_epoch(core_tile_engine_epoch[fti]),

                    .csr_wen       (core_csr_wen[fti]),
                    .csr_addr      (core_csr_addr[fti]),
                    .csr_wdata     (core_csr_wdata[fti]),
                    .csr_rdata     (core_csr_rdata[fti]),
                    .mex_valid     (core_mex_valid[fti]),
                    .mex_ss        (core_mex_ss[fti]),
                    .mex_op        (core_mex_op[fti]),
                    .mex_funct     (core_mex_funct[fti]),
                    .mex_funct_byte(core_mex_funct_byte[fti]),
                    .mex_gpr_val   (core_mex_gpr_val[fti]),
                    .mex_imm8      (core_mex_imm8[fti]),
                    .mex_ext_mod   (core_mex_ext_mod[fti]),
                    .mex_ext_active(core_mex_ext_active[fti]),
                    .mex_caller_id (core_tile_caller_id[fti]),
                    .mex_priv      (core_tile_priv[fti]),
                    .mex_mpu_base  (core_tile_mpu_base[fti]),
                    .mex_mpu_limit (core_tile_mpu_limit[fti]),
                    .mex_mpu_enabled(core_tile_mpu_enabled[fti]),
                    .mex_allow_cluster_spad(
                        core_tile_allow_cluster_spad[fti]),
                    .mex_engine_epoch(core_tile_engine_epoch[fti]),
                    .mex_caller_epoch({TACC_EPOCH_BITS{1'b0}}),
                    .mex_caller_slot(2'd0),
                    .mex_retire    (1'b1),
                    .mex_done      (core_mex_done[fti]),
                    .mex_busy      (core_mex_busy[fti]),
                    .mex_fault     (core_mex_fault[fti]),
                    .mex_fault_addr(core_mex_fault_addr[fti]),
                    .mex_stall_cycle(core_mex_stall_cycle[fti]),

                    .tacc_status_raw(core_tacc_status_raw[fti]),
                    .tacc_ctl_valid(core_tacc_ctl_valid[fti]),
                    .tacc_ctl_caller_id(core_tile_caller_id[fti]),
                    .tacc_ctl_priv (core_tile_priv[fti]),
                    .tacc_ctl_wdata(core_tacc_ctl_wdata[fti]),
                    .tacc_ctl_done (core_tacc_ctl_done[fti]),
                    .tacc_ctl_fault(core_tacc_ctl_fault[fti]),

                    .tacc_xfer_req (core_tacc_xfer_req[fti]),
                    .tacc_xfer_store(core_tacc_xfer_store[fti]),
                    .tacc_xfer_ext (core_tacc_xfer_ext[fti]),
                    .tacc_xfer_base(core_tacc_xfer_base[fti]),
                    .tacc_xfer_format_ew(
                        core_tacc_xfer_format_ew[fti]),
                    .tacc_xfer_token(core_tacc_xfer_token[fti]),
                    .tacc_xfer_store_image(
                        core_tacc_xfer_store_image[fti]),
                    .tacc_xfer_cancel(core_tacc_xfer_cancel[fti]),
                    .tacc_xfer_finish(core_tacc_xfer_finish[fti]),
                    .tacc_xfer_done(core_tacc_xfer_done[fti]),
                    .tacc_xfer_response_token(
                        core_tacc_xfer_response_token[fti]),
                    .tacc_xfer_fault(core_tacc_xfer_fault[fti]),
                    .tacc_xfer_fault_addr(
                        core_tacc_xfer_fault_addr[fti]),
                    .tacc_xfer_load_image(
                        core_tacc_xfer_load_image[fti]),

                    .legacy_acc_state(core_legacy_acc_state[fti]),
                    .legacy_acc_wen(core_legacy_acc_wen[fti]),
                    .legacy_acc_wdata(core_legacy_acc_wdata[fti]),
                    .cfg_load      (1'b0),
                    .cfg_tmode     (64'd0),
                    .cfg_tctrl     (64'd0),
                    .cfg_tsrc0     (64'd0),
                    .cfg_tsrc1     (64'd0),
                    .cfg_tdst      (64'd0),
                    .cfg_sb        (64'd0),
                    .cfg_sr        (64'd0),
                    .cfg_sc        (64'd0),
                    .cfg_sw        (64'd0),
                    .cfg_tstride_r (64'd0),
                    .cfg_tstride_c (64'd0),
                    .cfg_ttile_h   (64'd0),
                    .cfg_ttile_w   (64'd0),
                    .acc_zero_consumed(
                        core_acc_zero_consumed[fti]),

                    .tile_req      (core_tile_req[fti]),
                    .tile_addr     (core_tile_addr[fti]),
                    .tile_wen      (core_tile_wen[fti]),
                    .tile_wdata    (core_tile_wdata[fti]),
                    .tile_rdata    (tile_mem_rdata),
                    .tile_ack      (tile_src_ack[fti]),
                    .tile_error    (tile_src_error[fti]),
                    .tile_fault_addr(
                        tile_src_fault_addr[fti*64 +: 64]),

                    .ext_tile_req  (core_ext_tile_req[fti]),
                    .ext_tile_addr (core_ext_tile_addr[fti]),
                    .ext_tile_wen  (core_ext_tile_wen[fti]),
                    .ext_tile_wdata(core_ext_tile_wdata[fti]),
                    .ext_tile_rdata(ext_tile_rdata),
                    .ext_tile_ack  (ext_tile_src_ack[fti]),
                    .ext_tile_error(ext_tile_src_error[fti]),
                    .ext_tile_fault_addr(
                        ext_tile_src_fault_addr[fti*64 +: 64]),
                    .tile_source_cancel(
                        core_tile_source_cancel[fti])
            );
        end
    endgenerate

`ifndef SYNTHESIS
    initial begin
        if (NUM_CORES < 1 || NUM_CORES > FULL_TILE_SOURCE_COUNT)
            $fatal(1, "mp64_soc supports one to four full cores");
        if (NUM_CLUSTERS < 1 || NUM_CLUSTERS > 3)
            $fatal(1, "mp64_soc supports one to three microclusters");
    end
`endif

    // ========================================================================
    // ========================================================================
    // Port I/O Bridge — Remap CSR and Combinational Address Translation
    // ========================================================================
    // Remap table: 7 entries (ports 1-7), each holding a 12-bit target
    // MMIO address.  When an OUT/INP bus transaction arrives with the
    // bus_mmio_port_io sideband asserted and the bridge is enabled, the
    // raw MMIO address (which encodes the port number in addr[11:9]) is
    // replaced by the remap entry before peripheral decode.
    //
    // CSR at MMIO 0x880-0x88F (16 bytes):
    //   +0x00  PORT1_REMAP   (bits [11:0] = target MMIO address)
    //   +0x02  PORT2_REMAP
    //   +0x04  PORT3_REMAP
    //   +0x06  PORT4_REMAP
    //   +0x08  PORT5_REMAP
    //   +0x0A  PORT6_REMAP
    //   +0x0C  PORT7_REMAP
    //   +0x0E  BRIDGE_CTRL   (bit 0 = enable)
    // -----------------------------------------------------------------------

    reg [11:0] port_remap_1, port_remap_2, port_remap_3, port_remap_4,
               port_remap_5, port_remap_6, port_remap_7;
    reg        port_bridge_en;

    wire mmio_sel_port_bridge = bus_mmio_req && !bus_mmio_port_io
                              && (bus_mmio_addr[11:4] == 8'h88);  // 0x880-0x88F

    // CSR write
    always @(posedge sys_clk or negedge sys_rst_n) begin
        if (!sys_rst_n) begin
            port_remap_1   <= 12'd0;
            port_remap_2   <= 12'd0;
            port_remap_3   <= 12'd0;
            port_remap_4   <= 12'd0;
            port_remap_5   <= 12'd0;
            port_remap_6   <= 12'd0;
            port_remap_7   <= 12'd0;
            port_bridge_en <= 1'b0;
        end else if (mmio_sel_port_bridge && bus_mmio_wen) begin
            case (bus_mmio_addr[3:1])
                3'd0: port_remap_1   <= bus_mmio_wdata[11:0];
                3'd1: port_remap_2   <= bus_mmio_wdata[11:0];
                3'd2: port_remap_3   <= bus_mmio_wdata[11:0];
                3'd3: port_remap_4   <= bus_mmio_wdata[11:0];
                3'd4: port_remap_5   <= bus_mmio_wdata[11:0];
                3'd5: port_remap_6   <= bus_mmio_wdata[11:0];
                3'd6: port_remap_7   <= bus_mmio_wdata[11:0];
                3'd7: port_bridge_en <= bus_mmio_wdata[0];
                default: ;
            endcase
        end
    end

    // CSR read
    reg [63:0] port_bridge_rdata;
    always @(*) begin
        port_bridge_rdata = 64'd0;
        case (bus_mmio_addr[3:1])
            3'd0: port_bridge_rdata[11:0] = port_remap_1;
            3'd1: port_bridge_rdata[11:0] = port_remap_2;
            3'd2: port_bridge_rdata[11:0] = port_remap_3;
            3'd3: port_bridge_rdata[11:0] = port_remap_4;
            3'd4: port_bridge_rdata[11:0] = port_remap_5;
            3'd5: port_bridge_rdata[11:0] = port_remap_6;
            3'd6: port_bridge_rdata[11:0] = port_remap_7;
            3'd7: port_bridge_rdata[0]    = port_bridge_en;
            default: ;
        endcase
    end

    // Combinational remap: select remap target based on port number
    reg [11:0] port_remap_sel;
    always @(*) begin
        case (bus_mmio_addr[11:9])
            3'd1: port_remap_sel = port_remap_1;
            3'd2: port_remap_sel = port_remap_2;
            3'd3: port_remap_sel = port_remap_3;
            3'd4: port_remap_sel = port_remap_4;
            3'd5: port_remap_sel = port_remap_5;
            3'd6: port_remap_sel = port_remap_6;
            3'd7: port_remap_sel = port_remap_7;
            default: port_remap_sel = 12'd0;
        endcase
    end

    wire [11:0] mmio_addr_eff = (bus_mmio_port_io && port_bridge_en
                                 && |bus_mmio_addr[11:9])
                              ? port_remap_sel : bus_mmio_addr;

    // MMIO Peripheral Decoder
    // ========================================================================
    // The bus arbiter presents a single mmio port with 12-bit address.
    // We decode the upper bits to select peripherals.

    // Peripheral select signals (use remapped address for port I/O)
    wire mmio_sel_uart   = bus_mmio_req && (mmio_addr_eff[11:8] == 4'h0); // 0x000
    wire mmio_sel_timer  = bus_mmio_req && (mmio_addr_eff[11:8] == 4'h1); // 0x100
    wire mmio_sel_disk   = bus_mmio_req && (mmio_addr_eff[11:8] == 4'h2); // 0x200
    wire mmio_sel_nic    = bus_mmio_req && (mmio_addr_eff[11:8] == 4'h4); // 0x400
    wire mmio_sel_mbox   = bus_mmio_req &&
                           ((mmio_addr_eff[11:4] == 8'h50) ||
                            (mmio_addr_eff[11:6] == 6'b011000));
                           // Mailbox 0x500-0x50F, spinlocks 0x600-0x63F.
    wire mmio_sel_aes    = bus_mmio_req && (mmio_addr_eff[11:7] == 5'b01110); // 0x700-0x77F
    wire mmio_sel_sha3   = bus_mmio_req && (mmio_addr_eff[11:7] == 5'b01111)
                                         && (mmio_addr_eff[6:5] != 2'b11);// 0x780-0x7DF (96 bytes)
    wire mmio_sel_trng   = bus_mmio_req && (mmio_addr_eff[11:5] == 7'b1000000);// 0x800-0x81F
    wire mmio_sel_ntt    = bus_mmio_req && (mmio_addr_eff[11:6] == 6'b100011);// 0x8C0-0x8FF
    wire mmio_sel_kem    = bus_mmio_req && (mmio_addr_eff[11:6] == 6'b100100);// 0x900-0x93F
    wire mmio_sel_wots   = bus_mmio_req && (mmio_addr_eff[11:5] == 7'b1000101);// 0x8A0-0x8BF
    wire mmio_sel_rtc    = bus_mmio_req && (mmio_addr_eff[11:5] == 7'b1011000); // 0xB00-0xB1F

    // SysInfo occupies the exact half-open range [0x300, 0x370).  Reject a
    // misaligned or crossing request as one bus access instead of allowing the
    // first byte to alias a valid register.
    wire [3:0] sysinfo_access_bytes = 4'd1 << bus_mmio_size;
    wire [12:0] sysinfo_access_end = {1'b0, mmio_addr_eff}
                                       + sysinfo_access_bytes - 13'd1;
    wire sysinfo_access_aligned =
        (bus_mmio_size == BUS_BYTE) ||
        (bus_mmio_size == BUS_HALF  && mmio_addr_eff[0]   == 1'b0) ||
        (bus_mmio_size == BUS_WORD  && mmio_addr_eff[1:0] == 2'b00) ||
        (bus_mmio_size == BUS_DWORD && mmio_addr_eff[2:0] == 3'b000);
    wire mmio_sel_sysinfo = bus_mmio_req
                            && (mmio_addr_eff >= 12'h300)
                            && (sysinfo_access_end < 13'h370)
                            && sysinfo_access_aligned;

    reg [63:0] sysinfo_write_mask;
    reg [63:0] sysinfo_read_mask;
    always @(*) begin
        case (bus_mmio_size)
            BUS_BYTE:  sysinfo_write_mask = 64'h0000_0000_0000_00FF
                                             << (mmio_addr_eff[2:0] * 8);
            BUS_HALF:  sysinfo_write_mask = 64'h0000_0000_0000_FFFF
                                             << (mmio_addr_eff[2:0] * 8);
            BUS_WORD:  sysinfo_write_mask = 64'h0000_0000_FFFF_FFFF
                                             << (mmio_addr_eff[2:0] * 8);
            default:   sysinfo_write_mask = 64'hFFFF_FFFF_FFFF_FFFF;
        endcase

        case (bus_mmio_size)
            BUS_BYTE:  sysinfo_read_mask = 64'h0000_0000_0000_00FF;
            BUS_HALF:  sysinfo_read_mask = 64'h0000_0000_0000_FFFF;
            BUS_WORD:  sysinfo_read_mask = 64'h0000_0000_FFFF_FFFF;
            default:   sysinfo_read_mask = 64'hFFFF_FFFF_FFFF_FFFF;
        endcase
    end

    // ---- Peripheral instances -----------------------------------------------

    // UART
    wire [7:0]  uart_rdata_raw;
    wire        uart_ack;

    mp64_uart #(
        .CLK_FREQ  (CLOCK_HZ),
        .BAUD_RATE (115200)
    ) u_uart (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_uart),
        .addr  (mmio_addr_eff[3:0]),
        .wdata (bus_mmio_wdata[7:0]),
        .wen   (bus_mmio_wen),
        .rdata (uart_rdata_raw),
        .ack   (uart_ack),
        .irq   (irq_uart_w),
        .tx    (uart_txd),
        .rx    (uart_rxd)
    );

    // Timer
    wire [7:0]  timer_rdata_raw;
    wire        timer_ack;

    mp64_timer u_timer (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_timer),
        .addr  (mmio_addr_eff[3:0]),
        .wdata (bus_mmio_wdata[7:0]),
        .wen   (bus_mmio_wen),
        .rdata (timer_rdata_raw),
        .ack   (timer_ack),
        .irq   (irq_timer_w)
    );

    // Disk (SD card SPI)
    wire [7:0]  disk_rdata_raw;
    wire        disk_ack;

    mp64_disk #(
        .TOTAL_SECTORS  (DISK_TOTAL_SECTORS),
        .DMA_BASE_ADDR  (64'd0),
        .DMA_LIMIT_ADDR (BANK0_SIZE),
        .DMA1_BASE_ADDR ({32'd0, EXT_MEM_BASE}),
        .DMA1_LIMIT_ADDR(EXT_MEM_LIMIT),
        .DMA2_BASE_ADDR ({32'd0, MP64_VRAM_BASE_ADDR}),
        .DMA2_LIMIT_ADDR(VRAM_LIMIT),
        .DMA3_BASE_ADDR ({32'd0, MP64_HBW_BASE_ADDR}),
        .DMA3_LIMIT_ADDR(HBW_LIMIT)
    ) u_disk (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_disk),
        .addr  (mmio_addr_eff[5:0]),
        .wdata (bus_mmio_wdata[7:0]),
        .wen   (bus_mmio_wen),
        .rdata (disk_rdata_raw),
        .ack   (disk_ack),
        .dma_req   (disk_dma_req),
        .dma_addr  (disk_dma_addr),
        .dma_wdata (disk_dma_wdata),
        .dma_wen   (disk_dma_wen),
        .dma_rdata (disk_dma_rdata),
        .dma_ack   (disk_dma_ack),
        .dma_err   (disk_dma_err),
        .card_present          (sd_card_present),
        .card_write_protected  (sd_write_protected),
        .spi_clk   (sd_sck),
        .spi_mosi  (sd_mosi),
        .spi_miso  (sd_miso),
        .spi_cs_n  (sd_cs_n)
    );

    // NIC
    wire [7:0]  nic_rdata_raw;
    wire        nic_ack;

    mp64_nic u_nic (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_nic),
        .addr  (mmio_addr_eff[6:0]),
        .wdata (bus_mmio_wdata[7:0]),
        .wen   (bus_mmio_wen),
        .rdata (nic_rdata_raw),
        .ack   (nic_ack),
        .irq   (irq_nic_w),
        .dma_req   (nic_dma_req),
        .dma_addr  (nic_dma_addr),
        .dma_wdata (nic_dma_wdata),
        .dma_wen   (nic_dma_wen),
        .dma_rdata (nic_dma_rdata),
        .dma_ack   (nic_dma_ack),
        .phy_tx_valid (nic_tx_valid),
        .phy_tx_data  (nic_tx_data),
        .phy_tx_ready (nic_tx_ready),
        .phy_rx_valid (nic_rx_valid),
        .phy_rx_data  (nic_rx_data),
        .phy_rx_ready (nic_rx_ready),
        .phy_link_up  (nic_link_up)
    );

    // Mailbox
    wire [7:0]  mbox_rdata_raw;
    wire        mbox_ack;

    mp64_mailbox #(
        .N_CORES        (NUM_CORES),
        .N_GLOBAL_CORES (NUM_GLOBAL_CORES)
    ) u_mailbox (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_mbox),
        .addr  (mmio_addr_eff[11:0]),
        .wdata (bus_mmio_wdata[7:0]),
        .wen   (bus_mmio_wen),
        .rdata (mbox_rdata_raw),
        .ack   (mbox_ack),
        .requester_valid(bus_mmio_requester_valid),
        .requester_id   (bus_mmio_requester_id),
        .ipi_out      (ipi_out),
        .csr_ipi_wen  ({NUM_CORES{1'b0}}),
        .csr_ipi_addr ({NUM_CORES*8{1'b0}}),
        .csr_ipi_wdata({NUM_CORES*64{1'b0}}),
        .csr_ipi_rdata()
    );

    // ---- Crypto accelerators (64-bit data interfaces) ----------------------

    wire [63:0] aes_rdata;
    wire        aes_ack;
    wire        aes_irq;

    mp64_aes u_aes (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_aes),
        .addr  (mmio_addr_eff[6:0]),
        .wdata (bus_mmio_wdata),
        .wen   (bus_mmio_wen),
        .rdata (aes_rdata),
        .ack   (aes_ack),
        .irq   (aes_irq)
    );

    wire [63:0] sha3_rdata;
    wire        sha3_ack;
    wire        wots_active;

    mp64_sha3 u_sha3 (
        .clk             (sys_clk),
        .rst_n           (sys_rst_n),
        // The front end remains selected while any future WOTS requester
        // owns the shared service so STATUS/ERROR never turn into timeouts.
        .req             (mmio_sel_sha3),
        .addr            (mmio_addr_eff[6:0]),
        .wdata           (bus_mmio_wdata),
        .wen             (bus_mmio_wen),
        .size            (bus_mmio_size),
        .rdata           (sha3_rdata),
        .ack             (sha3_ack),
        .sha3_stream_en  (CRYPTO_CAPS[1]),
        .keccak_f1600_en (CRYPTO_CAPS[2]),

        // Production WOTS integration is checkpoint 3.  The old standalone
        // controller below cannot claim or observe this service.
        .wots_claim      (1'b0),
        .wots_grant      (),
        .wots_owned      (),
        .wots_perm_req   (1'b0),
        .wots_state_in   (1600'd0),
        .wots_state_out  (),
        .wots_perm_busy  (),
        .wots_perm_done  (),
        .wots_release    (1'b0),
        .wots_abort      (1'b0)
    );

    // Checkpoint 2 retires the functional three-pointer WOTS prototype.
    // Preserve its reserved aperture as an inert responder until the checked
    // context/DMA/shared-service controller lands atomically in checkpoint 3.
    // Its capability bit is clear, writes have no effect, and reads are zero.
    wire [63:0] wots_rdata = 64'd0;
    wire        wots_ack = mmio_sel_wots;
    wire        wots_irq = 1'b0;
    assign      wots_active = 1'b0;

    wire [63:0] trng_rdata;
    wire        trng_ack;

    mp64_trng u_trng (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_trng),
        .addr  (mmio_addr_eff[4:0]),
        .wdata (bus_mmio_wdata),
        .wen   (bus_mmio_wen),
        .rdata (trng_rdata),
        .ack   (trng_ack)
    );

    wire [63:0] ntt_rdata;
    wire        ntt_ack;

    mp64_ntt u_ntt (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_ntt),
        .addr  (mmio_addr_eff[5:0]),
        .wdata (bus_mmio_wdata),
        .wen   (bus_mmio_wen),
        .rdata (ntt_rdata),
        .ack   (ntt_ack)
    );

    wire [63:0] kem_rdata;
    wire        kem_ack;

    mp64_kem u_kem (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_kem),
        .addr  (mmio_addr_eff[5:0]),
        .wdata (bus_mmio_wdata),
        .wen   (bus_mmio_wen),
        .rdata (kem_rdata),
        .ack   (kem_ack)
    );

    // RTC
    wire [7:0]  rtc_rdata_raw;
    wire        rtc_ack;
    wire        irq_rtc_w;

    mp64_rtc #(
        .CLOCK_HZ (CLOCK_HZ)
    ) u_rtc (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_rtc),
        .addr  (mmio_addr_eff[4:0]),
        .wdata (bus_mmio_wdata[7:0]),
        .wen   (bus_mmio_wen),
        .rdata (rtc_rdata_raw),
        .ack   (rtc_ack),
        .irq   (irq_rtc_w)
    );

    // ========================================================================
    // SysInfo writable register: cluster enable mask (offset 0x18)
    // ========================================================================
    always @(posedge sys_clk or negedge sys_rst_n) begin
        if (!sys_rst_n)
            sysinfo_cluster_en <= {64{1'b1}};  // all clusters enabled at reset
        else if (mmio_sel_sysinfo && bus_mmio_wen && mmio_addr_eff[6:3] == 4'h3)
            sysinfo_cluster_en <=
                (sysinfo_cluster_en & ~sysinfo_write_mask)
                | ((bus_mmio_wdata << (mmio_addr_eff[2:0] * 8))
                   & sysinfo_write_mask);
    end

    // ========================================================================
    // MMIO Read Data & Ack Mux
    // ========================================================================
    // The bus expects a single mmio_rdata/mmio_ack response.
    // We mux based on which peripheral was selected.

    reg [63:0] mmio_rdata_mux;
    reg        mmio_ack_mux;

    always @(*) begin
        mmio_rdata_mux = 64'd0;
        mmio_ack_mux   = 1'b0;

        // 8-bit peripherals (zero-extend to 64 bits)
        if (mmio_sel_uart)    begin mmio_rdata_mux = {56'd0, uart_rdata_raw};  mmio_ack_mux = uart_ack;  end
        if (mmio_sel_timer)   begin mmio_rdata_mux = {56'd0, timer_rdata_raw}; mmio_ack_mux = timer_ack; end
        if (mmio_sel_disk)    begin mmio_rdata_mux = {56'd0, disk_rdata_raw};  mmio_ack_mux = disk_ack;  end
        if (mmio_sel_nic)     begin mmio_rdata_mux = {56'd0, nic_rdata_raw};   mmio_ack_mux = nic_ack;   end
        if (mmio_sel_mbox)    begin mmio_rdata_mux = {56'd0, mbox_rdata_raw};  mmio_ack_mux = mbox_ack;  end

// SysInfo (64-bit aligned register map, matches emulator devices.py)
        //   0x00  BOARD_ID_VER  — "MP64" + version 2.1
        //   0x08  BANK0_SIZE    — Bank 0 (system RAM) size in bytes
        //   0x10  NUM_CORES     — total core count (full + micro)
        //   0x18  CLUSTER_EN    — per-cluster enable mask (R/W)
        //   0x20  HBW_BASE      — HBW math RAM base address
        //   0x28  HBW_SIZE      — HBW region size in bytes
        //   0x30  INT_MEM_TOTAL — total internal memory in bytes
        //   0x38  EXT_MEM_BASE  — external memory base address
        //   0x40  EXT_MEM_SIZE  — external memory size in bytes
        //   0x48  NUM_FULL      — number of full (major) cores
        //   0x50  VRAM_BASE     — dedicated VRAM base address
        //   0x58  VRAM_SIZE     — dedicated VRAM size in bytes
        //   0x60  CRYPTO_CAPS   — independently qualified capabilities
        //   0x68  NUM_BUS_PORTS — weighted-arbiter requester count
        if (mmio_sel_sysinfo) begin
            case (mmio_addr_eff[6:3])  // 64-bit aligned: offset >> 3
                4'h0: mmio_rdata_mux = 64'h4D50_3634_0002_0001;  // BOARD_ID_VER
                4'h1: mmio_rdata_mux = BANK0_SIZE;               // 0x08
                4'h2: mmio_rdata_mux = NUM_ALL_CORES;            // 0x10
                4'h3: mmio_rdata_mux = sysinfo_cluster_en;       // 0x18 (R/W)
                4'h4: mmio_rdata_mux = {32'd0, MP64_HBW_BASE_ADDR};  // 0x20
                4'h5: mmio_rdata_mux = HBW_SIZE_BYTES;           // 0x28
                4'h6: mmio_rdata_mux = MEM_SIZE_BYTES;           // 0x30
                4'h7: mmio_rdata_mux = {32'd0, EXT_MEM_BASE};   // 0x38
                4'h8: mmio_rdata_mux = EXT_MEM_SIZE;             // 0x40
                4'h9: mmio_rdata_mux = {56'd0, NUM_CORES[7:0]};  // 0x48 NUM_FULL
                4'hA: mmio_rdata_mux = {32'd0, MP64_VRAM_BASE_ADDR};  // 0x50
                4'hB: mmio_rdata_mux = {32'd0, MP64_VRAM_DEFAULT_SIZE}; // 0x58
                4'hC: mmio_rdata_mux = CRYPTO_CAPS;              // 0x60
                4'hD: mmio_rdata_mux = N_BUS_PORTS;              // 0x68
                default: mmio_rdata_mux = 64'd0;
            endcase
            mmio_rdata_mux = (mmio_rdata_mux
                               >> (mmio_addr_eff[2:0] * 8))
                              & sysinfo_read_mask;
            mmio_ack_mux = 1'b1;
        end

        // 64-bit crypto peripherals
        if (mmio_sel_aes)     begin mmio_rdata_mux = aes_rdata;   mmio_ack_mux = aes_ack;   end
        if (mmio_sel_sha3)    begin mmio_rdata_mux = sha3_rdata;  mmio_ack_mux = sha3_ack;  end
        if (mmio_sel_trng)    begin mmio_rdata_mux = trng_rdata;  mmio_ack_mux = trng_ack;  end
        if (mmio_sel_ntt)     begin mmio_rdata_mux = ntt_rdata;   mmio_ack_mux = ntt_ack;   end
        if (mmio_sel_kem)     begin mmio_rdata_mux = kem_rdata;   mmio_ack_mux = kem_ack;   end
        if (mmio_sel_wots)    begin mmio_rdata_mux = wots_rdata;  mmio_ack_mux = wots_ack;  end
        if (mmio_sel_rtc)     begin mmio_rdata_mux = {56'd0, rtc_rdata_raw}; mmio_ack_mux = rtc_ack; end
        if (mmio_sel_port_bridge) begin mmio_rdata_mux = port_bridge_rdata; mmio_ack_mux = 1'b1; end
    end

    assign bus_mmio_rdata = mmio_rdata_mux;
    assign bus_mmio_ack   = mmio_ack_mux;

    // ========================================================================
    // Debug LEDs
    // ========================================================================
    reg [7:0] led_r;
    always @(posedge sys_clk) begin
        if (!sys_rst_n)
            led_r <= 8'h00;
        else
            led_r <= 8'hA5;   // heartbeat pattern (TODO: more useful status)
    end
    assign debug_leds = led_r;

endmodule
