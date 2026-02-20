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
// Tile engine:  CSR path from core 0, 512-bit memory port, ext tile port
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
    input  wire [63:0] phy_rdata,
    input  wire        phy_rvalid,
    input  wire        phy_ready,

    // === SD Card (SPI) ===
    output wire        sd_sck,
    output wire        sd_mosi,
    input  wire        sd_miso,
    output wire        sd_cs_n,

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
    localparam N_BUS_PORTS = NUM_CORES + NUM_CLUSTERS;  // 7 masters
    localparam PORT_BITS   = $clog2(N_BUS_PORTS);

    // System-wide reset (active-high for cores, active-low for peripherals)
    wire rst_h = ~sys_rst_n;

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
    //   - CSR/MEX ports (tile engine, only core 0 wired)

    // Per-core wires
    wire [63:0] core_bus_addr  [0:NUM_CORES-1];
    wire [63:0] core_bus_wdata [0:NUM_CORES-1];
    wire [1:0]  core_bus_size  [0:NUM_CORES-1];
    wire        core_bus_valid [0:NUM_CORES-1];
    wire        core_bus_wen   [0:NUM_CORES-1];
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
    wire        cpu_icache_inv_all [0:NUM_CORES-1];
    wire        cpu_icache_inv_line[0:NUM_CORES-1];
    wire [63:0] cpu_icache_inv_addr[0:NUM_CORES-1];
    wire [63:0] ic_stat_hits       [0:NUM_CORES-1];
    wire [63:0] ic_stat_misses     [0:NUM_CORES-1];

    // CSR/MEX (only core 0 → tile engine)
    wire        core_csr_wen   [0:NUM_CORES-1];
    wire [7:0]  core_csr_addr  [0:NUM_CORES-1];
    wire [63:0] core_csr_wdata [0:NUM_CORES-1];
    wire [63:0] core_csr_rdata [0:NUM_CORES-1];

    wire        core_mex_valid     [0:NUM_CORES-1];
    wire [1:0]  core_mex_ss        [0:NUM_CORES-1];
    wire [1:0]  core_mex_op        [0:NUM_CORES-1];
    wire [2:0]  core_mex_funct     [0:NUM_CORES-1];
    wire [63:0] core_mex_gpr_val   [0:NUM_CORES-1];
    wire [7:0]  core_mex_imm8      [0:NUM_CORES-1];
    wire [3:0]  core_mex_ext_mod   [0:NUM_CORES-1];
    wire        core_mex_ext_active[0:NUM_CORES-1];
    wire        core_mex_done      [0:NUM_CORES-1];
    wire        core_mex_busy      [0:NUM_CORES-1];

    // Interrupts
    wire        irq_uart_w;
    wire        irq_nic_w;
    wire        irq_timer_w;
    wire [NUM_CORES-1:0] ipi_out;

    // Memory size visible to CPU
    localparam [63:0] MEM_SIZE_BYTES = MEM_DEPTH * 512 / 8 * 4;  // 4 banks

    genvar ci;
    generate
        for (ci = 0; ci < NUM_CORES; ci = ci + 1) begin : g_core

            mp64_cpu #(
                .CORE_ID_W (MP64_CORE_ID_BITS)
            ) u_cpu (
                .clk             (sys_clk),
                .rst             (rst_h),
                .core_id         (ci[MP64_CORE_ID_BITS-1:0]),

                // I-cache interface
                .icache_addr     (cpu_icache_addr[ci]),
                .icache_req      (cpu_icache_req[ci]),
                .icache_data     (cpu_icache_data[ci]),
                .icache_hit      (cpu_icache_hit[ci]),
                .icache_stall    (cpu_icache_stall[ci]),
                .icache_inv_all  (cpu_icache_inv_all[ci]),
                .icache_inv_line (cpu_icache_inv_line[ci]),
                .icache_inv_addr (cpu_icache_inv_addr[ci]),
                .icache_stat_hits  (ic_stat_hits[ci]),
                .icache_stat_misses(ic_stat_misses[ci]),

                // Data bus
                .bus_valid       (core_bus_valid[ci]),
                .bus_addr        (core_bus_addr[ci]),
                .bus_wdata       (core_bus_wdata[ci]),
                .bus_wen         (core_bus_wen[ci]),
                .bus_size        (core_bus_size[ci]),
                .bus_rdata       (core_bus_rdata[ci]),
                .bus_ready       (core_bus_ready[ci]),

                // CSR / MEX (tile engine)
                .csr_wen         (core_csr_wen[ci]),
                .csr_addr        (core_csr_addr[ci]),
                .csr_wdata       (core_csr_wdata[ci]),
                .csr_rdata       (core_csr_rdata[ci]),
                .mex_valid       (core_mex_valid[ci]),
                .mex_ss          (core_mex_ss[ci]),
                .mex_op          (core_mex_op[ci]),
                .mex_funct       (core_mex_funct[ci]),
                .mex_gpr_val     (core_mex_gpr_val[ci]),
                .mex_imm8        (core_mex_imm8[ci]),
                .mex_ext_mod     (core_mex_ext_mod[ci]),
                .mex_ext_active  (core_mex_ext_active[ci]),
                .mex_done        (core_mex_done[ci]),
                .mex_busy        (core_mex_busy[ci]),

                // Interrupts
                .irq_timer       (irq_timer_w),
                .irq_uart        (irq_uart_w),
                .irq_nic         (irq_nic_w),
                .irq_ipi         (ipi_out[ci]),

                // Info
                .mem_size_bytes  (MEM_SIZE_BYTES),
                .ef_flags        (4'b0000)
            );

            mp64_icache u_icache (
                .clk         (sys_clk),
                .rst         (rst_h),

                // CPU fetch side
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
                .inv_all     (cpu_icache_inv_all[ci]),
                .inv_line    (cpu_icache_inv_line[ci]),
                .inv_addr    (cpu_icache_inv_addr[ci]),

                // Stats
                .stat_hits   (ic_stat_hits[ci]),
                .stat_misses (ic_stat_misses[ci])
            );

            // Tie off CSR/MEX for cores > 0 (only core 0 drives tile engine)
            if (ci > 0) begin : g_mex_tieoff
                assign core_csr_rdata[ci] = 64'd0;
                assign core_mex_done[ci]  = 1'b1;
                assign core_mex_busy[ci]  = 1'b0;
            end

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
    wire [63:0] cluster_bus_rdata [0:NUM_CLUSTERS-1];
    wire        cluster_bus_ready [0:NUM_CLUSTERS-1];

    genvar ki;
    generate
        for (ki = 0; ki < NUM_CLUSTERS; ki = ki + 1) begin : g_cluster
            mp64_cluster #(
                .N             (CORES_PER_CLUSTER),
                .CLUSTER_ID_BASE(NUM_CORES[7:0] + ki[7:0] * CORES_PER_CLUSTER[7:0])
            ) u_cluster (
                .clk         (sys_clk),
                .rst         (rst_h),
                .cluster_en  (1'b1),

                .bus_valid   (cluster_bus_valid[ki]),
                .bus_addr    (cluster_bus_addr[ki]),
                .bus_wdata   (cluster_bus_wdata[ki]),
                .bus_wen     (cluster_bus_wen[ki]),
                .bus_size    (cluster_bus_size[ki]),
                .bus_rdata   (cluster_bus_rdata[ki]),
                .bus_ready   (cluster_bus_ready[ki]),

                .irq_timer   ({CORES_PER_CLUSTER{irq_timer_w}}),
                .irq_ipi     ({CORES_PER_CLUSTER{1'b0}}),
                .ef_flags    (4'b0000)
            );
        end
    endgenerate

    // ========================================================================
    // Bus Arbiter — pack N_BUS_PORTS flat signals
    // ========================================================================
    // Port layout: [0..NUM_CORES-1] = CPU data buses
    //              [NUM_CORES..N_BUS_PORTS-1] = I-cache refill buses
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

    genvar mi;
    generate
        for (mi = 0; mi < NUM_CORES; mi = mi + 1) begin : g_bus_mux
            // I-cache refill has priority (CPU is stalled during refill)
            assign muxed_valid[mi] = ic_bus_valid[mi] ? 1'b1
                                                      : core_bus_valid[mi];
            assign muxed_addr[mi]  = ic_bus_valid[mi] ? ic_bus_addr[mi]
                                                      : core_bus_addr[mi];
            assign muxed_wdata[mi] = ic_bus_valid[mi] ? 64'd0
                                                      : core_bus_wdata[mi];
            assign muxed_wen[mi]   = ic_bus_valid[mi] ? ic_bus_wen[mi]
                                                      : core_bus_wen[mi];
            assign muxed_size[mi]  = ic_bus_valid[mi] ? ic_bus_size[mi]
                                                      : core_bus_size[mi];

            // Demux response back to CPU or I-cache
            wire bus_resp_ready;
            wire [63:0] bus_resp_rdata;

            // These get assigned from unpacked bus arbiter output below
            assign core_bus_rdata[mi] = bus_resp_rdata;
            assign core_bus_ready[mi] = bus_resp_ready && !ic_bus_valid[mi];
            assign ic_bus_rdata[mi]   = bus_resp_rdata;
            assign ic_bus_ready[mi]   = bus_resp_ready && ic_bus_valid[mi];
        end
    endgenerate

    // Flat-pack all N_BUS_PORTS master signals for the bus arbiter
    wire [N_BUS_PORTS-1:0]    bus_cpu_valid;
    wire [N_BUS_PORTS*64-1:0] bus_cpu_addr;
    wire [N_BUS_PORTS*64-1:0] bus_cpu_wdata;
    wire [N_BUS_PORTS-1:0]    bus_cpu_wen;
    wire [N_BUS_PORTS*2-1:0]  bus_cpu_size;
    wire [N_BUS_PORTS*64-1:0] bus_cpu_rdata;
    wire [N_BUS_PORTS-1:0]    bus_cpu_ready;

    genvar pi;
    generate
        // Ports [0..NUM_CORES-1]: muxed CPU/I-cache
        for (pi = 0; pi < NUM_CORES; pi = pi + 1) begin : g_pack_core
            assign bus_cpu_valid[pi]           = muxed_valid[pi];
            assign bus_cpu_addr [pi*64 +: 64]  = muxed_addr[pi];
            assign bus_cpu_wdata[pi*64 +: 64]  = muxed_wdata[pi];
            assign bus_cpu_wen  [pi]           = muxed_wen[pi];
            assign bus_cpu_size [pi*2  +: 2]   = muxed_size[pi];
        end

        // Ports [NUM_CORES..N_BUS_PORTS-1]: clusters
        for (pi = 0; pi < NUM_CLUSTERS; pi = pi + 1) begin : g_pack_cluster
            localparam P = NUM_CORES + pi;
            assign bus_cpu_valid[P]           = cluster_bus_valid[pi];
            assign bus_cpu_addr [P*64 +: 64]  = cluster_bus_addr[pi];
            assign bus_cpu_wdata[P*64 +: 64]  = cluster_bus_wdata[pi];
            assign bus_cpu_wen  [P]           = cluster_bus_wen[pi];
            assign bus_cpu_size [P*2  +: 2]   = cluster_bus_size[pi];
        end
    endgenerate

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

    mp64_bus #(
        .N_PORTS   (N_BUS_PORTS),
        .PORT_BITS (PORT_BITS)
    ) u_bus (
        .clk       (sys_clk),
        .rst_n     (sys_rst_n),

        .cpu_valid (bus_cpu_valid),
        .cpu_addr  (bus_cpu_addr),
        .cpu_wdata (bus_cpu_wdata),
        .cpu_wen   (bus_cpu_wen),
        .cpu_size  (bus_cpu_size),
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
        .mmio_rdata (bus_mmio_rdata),
        .mmio_ack   (bus_mmio_ack),

        .qos_csr_wen   (1'b0),
        .qos_csr_addr  (8'd0),
        .qos_csr_wdata (64'd0),
        .qos_csr_rdata ()
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

    wire        mem_ext_req;
    wire [63:0] mem_ext_addr;
    wire [63:0] mem_ext_wdata;
    wire        mem_ext_wen;
    /* verilator lint_off UNUSED */
    wire [1:0]  mem_ext_size;   // driven by memory, unused by extmem
    /* verilator lint_on  UNUSED */
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
        .tile_rdata(tile_mem_rdata),
        .tile_ack  (tile_mem_ack),

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

    // PHY interface (internal 32-bit addr + phy_ack)
    wire        extmem_phy_req;
    wire [31:0] extmem_phy_addr;
    wire [63:0] extmem_phy_wdata;
    wire        extmem_phy_wen;
    wire [63:0] extmem_phy_rdata;
    wire        extmem_phy_ack;
    wire [3:0]  extmem_phy_burst_len;

    mp64_extmem u_extmem (
        .clk       (sys_clk),
        .rst_n     (sys_rst_n),

        // CPU port (forwarded from memory subsystem)
        .cpu_req   (mem_ext_req),
        .cpu_addr  (mem_ext_addr[31:0]),
        .cpu_wdata (mem_ext_wdata),
        .cpu_wen   (mem_ext_wen),
        .cpu_rdata (mem_ext_rdata),
        .cpu_ack   (mem_ext_ack),

        // Tile port
        .tile_req  (ext_tile_req),
        .tile_addr (ext_tile_addr[31:0]),
        .tile_wdata(ext_tile_wdata),
        .tile_wen  (ext_tile_wen),
        .tile_rdata(ext_tile_rdata),
        .tile_ack  (ext_tile_ack),

        // PHY
        .phy_req       (extmem_phy_req),
        .phy_addr      (extmem_phy_addr),
        .phy_wdata     (extmem_phy_wdata),
        .phy_wen       (extmem_phy_wen),
        .phy_rdata     (extmem_phy_rdata),
        .phy_ack       (extmem_phy_ack),
        .phy_burst_len (extmem_phy_burst_len)
    );

    // Adapt internal 32-bit PHY to external 64-bit / rvalid/ready interface
    assign phy_req       = extmem_phy_req & phy_ready;  // gate on PHY readiness
    assign phy_addr      = {32'd0, extmem_phy_addr};
    assign phy_wen       = extmem_phy_wen;
    assign phy_wdata     = extmem_phy_wdata;
    assign phy_burst_len = {4'd0, extmem_phy_burst_len};
    assign extmem_phy_rdata = phy_rdata;
    assign extmem_phy_ack   = phy_rvalid;

    // ========================================================================
    // Tile Engine (connected to core 0 CSR/MEX, memory tile port)
    // ========================================================================
    mp64_tile u_tile (
        .clk       (sys_clk),
        .rst_n     (sys_rst_n),

        // CSR/MEX from core 0
        .csr_wen       (core_csr_wen[0]),
        .csr_addr      (core_csr_addr[0]),
        .csr_wdata     (core_csr_wdata[0]),
        .csr_rdata     (core_csr_rdata[0]),
        .mex_valid     (core_mex_valid[0]),
        .mex_ss        (core_mex_ss[0]),
        .mex_op        (core_mex_op[0]),
        .mex_funct     (core_mex_funct[0]),
        .mex_gpr_val   (core_mex_gpr_val[0]),
        .mex_imm8      (core_mex_imm8[0]),
        .mex_ext_mod   (core_mex_ext_mod[0]),
        .mex_ext_active(core_mex_ext_active[0]),
        .mex_done      (core_mex_done[0]),
        .mex_busy      (core_mex_busy[0]),

        // Internal tile memory port → memory subsystem
        .tile_req      (tile_mem_req),
        .tile_addr     (tile_mem_addr),
        .tile_wen      (tile_mem_wen),
        .tile_wdata    (tile_mem_wdata),
        .tile_rdata    (tile_mem_rdata),
        .tile_ack      (tile_mem_ack),

        // External tile port → extmem
        .ext_tile_req  (ext_tile_req),
        .ext_tile_addr (ext_tile_addr),
        .ext_tile_wen  (ext_tile_wen),
        .ext_tile_wdata(ext_tile_wdata),
        .ext_tile_rdata(ext_tile_rdata),
        .ext_tile_ack  (ext_tile_ack)
    );

    // ========================================================================
    // MMIO Peripheral Decoder
    // ========================================================================
    // The bus arbiter presents a single mmio port with 12-bit address.
    // We decode the upper bits to select peripherals.

    // Peripheral select signals
    wire mmio_sel_uart   = bus_mmio_req && (bus_mmio_addr[11:8] == 4'h0); // 0x000
    wire mmio_sel_timer  = bus_mmio_req && (bus_mmio_addr[11:8] == 4'h1); // 0x100
    wire mmio_sel_disk   = bus_mmio_req && (bus_mmio_addr[11:8] == 4'h2); // 0x200
    wire mmio_sel_nic    = bus_mmio_req && (bus_mmio_addr[11:8] == 4'h4); // 0x400
    wire mmio_sel_mbox   = bus_mmio_req && (bus_mmio_addr[11:8] == 4'h5
                                         || bus_mmio_addr[11:8] == 4'h6);// 0x500-0x6FF
    wire mmio_sel_aes    = bus_mmio_req && (bus_mmio_addr[11:7] == 5'b01110); // 0x700-0x77F
    wire mmio_sel_sha3   = bus_mmio_req && (bus_mmio_addr[11:7] == 5'b01111)
                                         && (bus_mmio_addr[6:5] != 2'b11);// 0x780-0x7DF (96 bytes)
    wire mmio_sel_crc    = bus_mmio_req && (bus_mmio_addr[11:6] == 6'b100110);// 0x980-0x9BF
    wire mmio_sel_trng   = bus_mmio_req && (bus_mmio_addr[11:5] == 7'b1000000);// 0x800-0x81F
    wire mmio_sel_field  = bus_mmio_req && (bus_mmio_addr[11:6] == 6'b100001);// 0x840-0x87F
    wire mmio_sel_sha256 = bus_mmio_req && (bus_mmio_addr[11:6] == 6'b100101);// 0x940-0x97F
    wire mmio_sel_ntt    = bus_mmio_req && (bus_mmio_addr[11:6] == 6'b100011);// 0x8C0-0x8FF
    wire mmio_sel_kem    = bus_mmio_req && (bus_mmio_addr[11:6] == 6'b100100);// 0x900-0x93F
    wire mmio_sel_rtc    = bus_mmio_req && (bus_mmio_addr[11:5] == 7'b1011000); // 0xB00-0xB1F

    // SysInfo — read-only system information (0x300)
    wire mmio_sel_sysinfo = bus_mmio_req && (bus_mmio_addr[11:8] == 4'h3);

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
        .addr  (bus_mmio_addr[3:0]),
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
        .addr  (bus_mmio_addr[3:0]),
        .wdata (bus_mmio_wdata[7:0]),
        .wen   (bus_mmio_wen),
        .rdata (timer_rdata_raw),
        .ack   (timer_ack),
        .irq   (irq_timer_w)
    );

    // Disk (SD card SPI)
    wire [7:0]  disk_rdata_raw;
    wire        disk_ack;
    wire        disk_dma_req;
    wire [63:0] disk_dma_addr;
    wire [7:0]  disk_dma_wdata;
    wire        disk_dma_wen;

    mp64_disk u_disk (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_disk),
        .addr  (bus_mmio_addr[3:0]),
        .wdata (bus_mmio_wdata[7:0]),
        .wen   (bus_mmio_wen),
        .rdata (disk_rdata_raw),
        .ack   (disk_ack),
        .dma_req   (disk_dma_req),
        .dma_addr  (disk_dma_addr),
        .dma_wdata (disk_dma_wdata),
        .dma_wen   (disk_dma_wen),
        .dma_rdata (8'd0),      // DMA read-back not wired yet
        .dma_ack   (1'b0),
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
        .addr  (bus_mmio_addr[6:0]),
        .wdata (bus_mmio_wdata[7:0]),
        .wen   (bus_mmio_wen),
        .rdata (nic_rdata_raw),
        .ack   (nic_ack),
        .irq   (irq_nic_w),
        .dma_req   (),
        .dma_addr  (),
        .dma_wdata (),
        .dma_wen   (),
        .dma_rdata (8'd0),
        .dma_ack   (1'b0),
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
        .N_CORES (NUM_CORES)
    ) u_mailbox (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_mbox),
        .addr  (bus_mmio_addr[11:0]),
        .wdata (bus_mmio_wdata[7:0]),
        .wen   (bus_mmio_wen),
        .rdata (mbox_rdata_raw),
        .ack   (mbox_ack),
        .requester_id ({MP64_CORE_ID_BITS{1'b0}}),  // TODO: from arbiter grant
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
        .addr  (bus_mmio_addr[6:0]),
        .wdata (bus_mmio_wdata),
        .wen   (bus_mmio_wen),
        .rdata (aes_rdata),
        .ack   (aes_ack),
        .irq   (aes_irq)
    );

    wire [63:0] sha3_rdata;
    wire        sha3_ack;
    wire        sha3_irq;

    mp64_sha3 u_sha3 (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_sha3),
        .addr  (bus_mmio_addr[6:0]),
        .wdata (bus_mmio_wdata),
        .wen   (bus_mmio_wen),
        .rdata (sha3_rdata),
        .ack   (sha3_ack),
        .irq   (sha3_irq)
    );

    wire [63:0] sha256_rdata;
    wire        sha256_ack;
    wire        sha256_irq;

    mp64_sha256 u_sha256 (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_sha256),
        .addr  (bus_mmio_addr[5:0]),
        .wdata (bus_mmio_wdata),
        .wen   (bus_mmio_wen),
        .rdata (sha256_rdata),
        .ack   (sha256_ack),
        .irq   (sha256_irq)
    );

    wire [63:0] crc_rdata;
    wire        crc_ack;
    wire        crc_irq;

    mp64_crc u_crc (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_crc),
        .addr  (bus_mmio_addr[4:0]),
        .wdata (bus_mmio_wdata),
        .wen   (bus_mmio_wen),
        .rdata (crc_rdata),
        .ack   (crc_ack),
        .irq   (crc_irq)
    );

    wire [63:0] trng_rdata;
    wire        trng_ack;

    mp64_trng u_trng (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_trng),
        .addr  (bus_mmio_addr[4:0]),
        .wdata (bus_mmio_wdata),
        .wen   (bus_mmio_wen),
        .rdata (trng_rdata),
        .ack   (trng_ack)
    );

    wire [63:0] field_rdata;
    wire        field_ack;

    mp64_field_alu u_field_alu (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_field),
        .addr  (bus_mmio_addr[5:0]),
        .wdata (bus_mmio_wdata),
        .wen   (bus_mmio_wen),
        .rdata (field_rdata),
        .ack   (field_ack)
    );

    wire [63:0] ntt_rdata;
    wire        ntt_ack;

    mp64_ntt u_ntt (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_ntt),
        .addr  (bus_mmio_addr[5:0]),
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
        .addr  (bus_mmio_addr[5:0]),
        .wdata (bus_mmio_wdata),
        .wen   (bus_mmio_wen),
        .rdata (kem_rdata),
        .ack   (kem_ack)
    );

    // RTC
    wire [7:0]  rtc_rdata_raw;
    wire        rtc_ack;
    wire        irq_rtc_w;

    mp64_rtc u_rtc (
        .clk   (sys_clk),
        .rst_n (sys_rst_n),
        .req   (mmio_sel_rtc),
        .addr  (bus_mmio_addr[4:0]),
        .wdata (bus_mmio_wdata[7:0]),
        .wen   (bus_mmio_wen),
        .rdata (rtc_rdata_raw),
        .ack   (rtc_ack),
        .irq   (irq_rtc_w)
    );

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

        // SysInfo (read-only)
        if (mmio_sel_sysinfo) begin
            case (bus_mmio_addr[3:0])
                4'h0: mmio_rdata_mux = MEM_SIZE_BYTES;
                4'h1: mmio_rdata_mux = {56'd0, NUM_CORES[7:0]};
                4'h2: mmio_rdata_mux = {56'd0, NUM_CLUSTERS[7:0]};
                4'h3: mmio_rdata_mux = CLOCK_HZ;
                4'h4: mmio_rdata_mux = {56'd0, NUM_CORES[7:0]};   // NUM_FULL
                default: mmio_rdata_mux = 64'd0;
            endcase
            mmio_ack_mux = 1'b1;
        end

        // 64-bit crypto peripherals
        if (mmio_sel_aes)     begin mmio_rdata_mux = aes_rdata;   mmio_ack_mux = aes_ack;   end
        if (mmio_sel_sha3)    begin mmio_rdata_mux = sha3_rdata;  mmio_ack_mux = sha3_ack;  end
        if (mmio_sel_sha256)  begin mmio_rdata_mux = sha256_rdata; mmio_ack_mux = sha256_ack; end
        if (mmio_sel_crc)     begin mmio_rdata_mux = crc_rdata;   mmio_ack_mux = crc_ack;   end
        if (mmio_sel_trng)    begin mmio_rdata_mux = trng_rdata;  mmio_ack_mux = trng_ack;  end
        if (mmio_sel_field)   begin mmio_rdata_mux = field_rdata; mmio_ack_mux = field_ack; end
        if (mmio_sel_ntt)     begin mmio_rdata_mux = ntt_rdata;   mmio_ack_mux = ntt_ack;   end
        if (mmio_sel_kem)     begin mmio_rdata_mux = kem_rdata;   mmio_ack_mux = kem_ack;   end
        if (mmio_sel_rtc)     begin mmio_rdata_mux = {56'd0, rtc_rdata_raw}; mmio_ack_mux = rtc_ack; end
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
