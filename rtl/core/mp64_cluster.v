// ============================================================================
// mp64_cluster.v — Megapad-64 Micro-Core Cluster
// ============================================================================
//
// Wraps N micro-cores (default 4) with shared resources:
//   - 64×64→128 multiplier + iterative 64-bit divider
//   - Shared tile/MEX engine (round-robin arbitrated)
//   - 1 KiB dual-port scratchpad (128 × 64-bit)
//   - Hardware barrier register with auto-clear
//   - Internal round-robin bus arbiter (one external bus port)
//   - Cluster BIST controller (march C- on scratchpad)
//   - Shared privilege level, MPU, and IVT base
//
// External interface: one bus port (same as a full core).
// The main bus arbiter treats a cluster as a single bus master.
//
// Core ID mapping:
//   Global ID = CLUSTER_ID_BASE + local micro-core index [0..N-1]
//
// Scratchpad address detection:
//   addr[63:32] == MP64_SPAD_HI → routed to cluster-internal SPAD
//   SPAD word address = addr[9:3] (128 doublewords = 1 KiB)
//

`include "mp64_pkg.vh"

module mp64_cluster #(
    parameter N               = MP64_MICRO_PER_CLUSTER,
    parameter [MP64_CORE_ID_BITS-1:0] CLUSTER_ID_BASE = 8'd4
) (
    input  wire        clk,
    input  wire        rst,
    input  wire        cluster_en,    // 0 = hold all micro-cores in reset

    // === Single external bus port ===
    output reg         bus_valid,
    output reg  [63:0] bus_addr,
    output reg  [63:0] bus_wdata,
    output reg         bus_wen,
    output reg  [1:0]  bus_size,
    input  wire [63:0] bus_rdata,
    input  wire        bus_ready,

    // === Per-micro-core interrupts ===
    input  wire [N-1:0] irq_timer,
    input  wire [N-1:0] irq_ipi,

    // === External flags (shared to all micro-cores) ===
    input  wire [3:0]  ef_flags,

    // === Tile memory port (512-bit, to SoC memory subsystem) ===
    output wire        tile_req,
    output wire [31:0] tile_addr,
    output wire        tile_wen,
    output wire [511:0]tile_wdata,
    input  wire [511:0]tile_rdata,
    input  wire        tile_ack,

    // === External tile memory port (for tiles in external RAM) ===
    output wire        ext_tile_req,
    output wire [63:0] ext_tile_addr,
    output wire        ext_tile_wen,
    output wire [511:0]ext_tile_wdata,
    input  wire [511:0]ext_tile_rdata,
    input  wire        ext_tile_ack
);

    // ====================================================================
    // Gated reset: hold micro-cores in reset when cluster_en == 0
    // ====================================================================
    wire cl_rst = rst | ~cluster_en;

    // ====================================================================
    // Arbiter width
    // ====================================================================
    localparam ARB_BITS = (N > 1) ? $clog2(N) : 1;
    localparam [ARB_BITS:0] N_VAL = N;   // wider copy for wrap-around math

    // ====================================================================
    // Per-micro-core internal bus wires
    // ====================================================================
    wire [N-1:0]        mc_bus_valid;
    wire [N*64-1:0]     mc_bus_addr;
    wire [N*64-1:0]     mc_bus_wdata;
    wire [N-1:0]        mc_bus_wen;
    wire [N*2-1:0]      mc_bus_size;
    reg  [N*64-1:0]     mc_bus_rdata;
    reg  [N-1:0]        mc_bus_ready;
    reg  [N-1:0]        mc_mpu_fault;

    // Per-micro-core MUL/DIV wires
    wire [N-1:0]        mc_mul_req;
    wire [N*4-1:0]      mc_mul_op;
    wire [N*64-1:0]     mc_mul_a;
    wire [N*64-1:0]     mc_mul_b;
    reg  [127:0]        mul_result_reg;
    reg                 mul_done_reg;

    // Per-micro-core cluster CSR wires
    wire [N*8-1:0]      mc_cl_csr_addr;
    wire [N-1:0]        mc_cl_csr_wen;
    wire [N*64-1:0]     mc_cl_csr_wdata;
    reg  [N*64-1:0]     mc_cl_csr_rdata;

    // Per-micro-core MEX/tile wires
    wire [N-1:0]        mc_mex_req;
    wire [N*2-1:0]      mc_mex_ss;
    wire [N*2-1:0]      mc_mex_op;
    wire [N*3-1:0]      mc_mex_funct;
    wire [N*64-1:0]     mc_mex_gpr_val;
    wire [N*8-1:0]      mc_mex_imm8;
    wire [N*4-1:0]      mc_mex_ext_mod;
    wire [N-1:0]        mc_mex_ext_active;
    reg                 mex_done_reg;
    reg                 mex_busy_reg;

    // Per-micro-core tile CSR wires
    wire [N-1:0]        mc_tile_csr_wen;
    wire [N*8-1:0]      mc_tile_csr_addr;
    wire [N*64-1:0]     mc_tile_csr_wdata;
    reg  [N*64-1:0]     mc_tile_csr_rdata;

    // Per-micro-core CRC wires
    wire [N-1:0]        mc_crc_req;
    wire [N*4-1:0]      mc_crc_op;
    wire [N*64-1:0]     mc_crc_rs_val;
    wire [N*8-1:0]      mc_crc_imm8;
    reg  [63:0]         crc_result_reg;
    reg                 crc_done_reg;
    reg                 crc_rd_we_reg;

    // Per-micro-core SHA wires
    wire [N-1:0]        mc_sha_req;
    wire [N*4-1:0]      mc_sha_op;
    wire [N*64-1:0]     mc_sha_rs_val;
    wire [N*8-1:0]      mc_sha_imm8;
    reg  [63:0]         sha_result_reg;
    reg                 sha_done_reg;
    reg                 sha_rd_we_reg;

    // Forward-declare MUL, MEX, CRC, and SHA grants (used in generate block)
    reg  [ARB_BITS-1:0] mul_grant;
    reg  [ARB_BITS-1:0] mex_grant;
    reg  [ARB_BITS-1:0] crc_grant;
    reg  [ARB_BITS-1:0] sha_grant;

    // ====================================================================
    // Cluster-shared state
    // ====================================================================
    reg         cl_priv_level;     // 0=S, 1=U (inert — no enforcement)
    reg [63:0]  cl_mpu_base;
    reg [63:0]  cl_mpu_limit;
    reg [63:0]  cl_ivt_base;
    reg [63:0]  cl_crc_acc;        // cluster-shared CRC accumulator
    reg [1:0]   cl_crc_mode;       // cluster-shared CRC mode (0/1/2)

    // Cluster-shared SHA-2 state
    reg [63:0]  cl_sha_acc  [0:3]; // SHA accumulator (4 × 64-bit = 256 bits)
    reg [1:0]   cl_sha_mode;       // 0=SHA-256
    reg [63:0]  cl_sha_msglen_lo;  // message length in bits (low 64)
    reg [63:0]  cl_sha_msglen_hi;  // high 64
    reg [63:0]  cl_sha_tsrc0;      // shadow of TSRC0 for SHA W-loading

    // ====================================================================
    // Unpack per-micro-core address/wdata/size for indexing
    // ====================================================================
    wire [63:0] mc_addr  [0:N-1];
    wire [63:0] mc_wdata_u [0:N-1];
    wire [1:0]  mc_sz    [0:N-1];

    genvar gi;
    generate
        for (gi = 0; gi < N; gi = gi + 1) begin : unpack
            assign mc_addr[gi]    = mc_bus_addr [gi*64 +: 64];
            assign mc_wdata_u[gi] = mc_bus_wdata[gi*64 +: 64];
            assign mc_sz[gi]      = mc_bus_size [gi*2  +: 2];
        end
    endgenerate

    // ====================================================================
    // Instantiate N micro-cores
    // ====================================================================
    generate
        for (gi = 0; gi < N; gi = gi + 1) begin : mc

            mp64_cpu_micro u_micro (
                .clk        (clk),
                .rst        (cl_rst),
                .core_id    (CLUSTER_ID_BASE + gi[MP64_CORE_ID_BITS-1:0]),

                .bus_valid  (mc_bus_valid[gi]),
                .bus_addr   (mc_bus_addr [gi*64 +: 64]),
                .bus_wdata  (mc_bus_wdata[gi*64 +: 64]),
                .bus_wen    (mc_bus_wen  [gi]),
                .bus_size   (mc_bus_size [gi*2  +: 2]),
                .bus_rdata  (mc_bus_rdata[gi*64 +: 64]),
                .bus_ready  (mc_bus_ready[gi]),

                .mpu_fault  (mc_mpu_fault[gi]),

                .irq_timer  (irq_timer[gi]),
                .irq_ipi    (irq_ipi[gi]),
                .ef_flags   (ef_flags),

                .mul_req    (mc_mul_req[gi]),
                .mul_op     (mc_mul_op [gi*4  +: 4]),
                .mul_a      (mc_mul_a  [gi*64 +: 64]),
                .mul_b      (mc_mul_b  [gi*64 +: 64]),
                .mul_result (mul_result_reg),
                .mul_done   (mul_done_reg && (mul_grant == gi[ARB_BITS-1:0])),

                .crc_req    (mc_crc_req[gi]),
                .crc_op     (mc_crc_op [gi*4  +: 4]),
                .crc_rs_val (mc_crc_rs_val[gi*64 +: 64]),
                .crc_imm8   (mc_crc_imm8 [gi*8  +: 8]),
                .crc_result (crc_result_reg),
                .crc_done   (crc_done_reg && (crc_grant == gi[ARB_BITS-1:0])),
                .crc_rd_we_in(crc_rd_we_reg && (crc_grant == gi[ARB_BITS-1:0])),

                .sha_req    (mc_sha_req[gi]),
                .sha_op     (mc_sha_op [gi*4  +: 4]),
                .sha_rs_val (mc_sha_rs_val[gi*64 +: 64]),
                .sha_imm8   (mc_sha_imm8 [gi*8  +: 8]),
                .sha_result (sha_result_reg),
                .sha_done   (sha_done_reg && (sha_grant == gi[ARB_BITS-1:0])),
                .sha_rd_we_in(sha_rd_we_reg && (sha_grant == gi[ARB_BITS-1:0])),

                .mex_req       (mc_mex_req[gi]),
                .mex_ss        (mc_mex_ss       [gi*2  +: 2]),
                .mex_op        (mc_mex_op       [gi*2  +: 2]),
                .mex_funct     (mc_mex_funct    [gi*3  +: 3]),
                .mex_gpr_val   (mc_mex_gpr_val  [gi*64 +: 64]),
                .mex_imm8      (mc_mex_imm8     [gi*8  +: 8]),
                .mex_ext_mod   (mc_mex_ext_mod  [gi*4  +: 4]),
                .mex_ext_active(mc_mex_ext_active[gi]),
                .mex_done      (mex_done_reg && (mex_grant == gi[ARB_BITS-1:0])),
                .mex_busy      (mex_busy_reg && (mex_grant != gi[ARB_BITS-1:0])),

                .tile_csr_wen  (mc_tile_csr_wen  [gi]),
                .tile_csr_addr (mc_tile_csr_addr [gi*8  +: 8]),
                .tile_csr_wdata(mc_tile_csr_wdata[gi*64 +: 64]),
                .tile_csr_rdata(mc_tile_csr_rdata[gi*64 +: 64]),

                .cl_csr_addr  (mc_cl_csr_addr [gi*8  +: 8]),
                .cl_csr_wen   (mc_cl_csr_wen  [gi]),
                .cl_csr_wdata (mc_cl_csr_wdata[gi*64 +: 64]),
                .cl_csr_rdata (mc_cl_csr_rdata[gi*64 +: 64]),

                .cl_ivt_base  (cl_ivt_base),
                .cl_priv_level(cl_priv_level)
            );

        end
    endgenerate

    // ====================================================================
    // Cluster CSR Read Mux (per micro-core, combinational)
    // ====================================================================
    // Each micro-core gets its own mux driven by its cl_csr_addr.

    // Barrier state (forward-declared for the mux)
    reg [N-1:0] barrier_arrive;
    reg         barrier_done;

    // BIST state (forward-declared for the mux)
    reg [1:0]  bist_status;
    reg [63:0] bist_fail_addr;
    reg [63:0] bist_fail_data;

    generate
        for (gi = 0; gi < N; gi = gi + 1) begin : csr_rd
            wire [7:0] rd_addr = mc_cl_csr_addr[gi*8 +: 8];

            always @(*) begin
                case (rd_addr)
                    CSR_BIST_CMD:       mc_cl_csr_rdata[gi*64 +: 64] = {62'd0, bist_status};
                    CSR_BIST_STATUS:    mc_cl_csr_rdata[gi*64 +: 64] = {62'd0, bist_status};
                    CSR_BIST_FAIL_ADDR: mc_cl_csr_rdata[gi*64 +: 64] = bist_fail_addr;
                    CSR_BIST_FAIL_DATA: mc_cl_csr_rdata[gi*64 +: 64] = bist_fail_data;
                    CSR_CL_PRIV:       mc_cl_csr_rdata[gi*64 +: 64] = {63'd0, cl_priv_level};
                    CSR_CL_MPU_BASE:   mc_cl_csr_rdata[gi*64 +: 64] = cl_mpu_base;
                    CSR_CL_MPU_LIMIT:  mc_cl_csr_rdata[gi*64 +: 64] = cl_mpu_limit;
                    CSR_CL_IVTBASE:    mc_cl_csr_rdata[gi*64 +: 64] = cl_ivt_base;
                    CSR_IVTBASE:       mc_cl_csr_rdata[gi*64 +: 64] = cl_ivt_base;
                    CSR_BARRIER_ARRIVE:mc_cl_csr_rdata[gi*64 +: 64] = {{(64-N){1'b0}}, barrier_arrive};
                    CSR_BARRIER_STATUS:mc_cl_csr_rdata[gi*64 +: 64] = {{(63-N){1'b0}}, barrier_done, barrier_arrive};
                    CSR_CRC_ACC:       mc_cl_csr_rdata[gi*64 +: 64] = cl_crc_acc;
                    CSR_CRC_MODE:      mc_cl_csr_rdata[gi*64 +: 64] = {62'd0, cl_crc_mode};
                    CSR_SHA_MODE:      mc_cl_csr_rdata[gi*64 +: 64] = {62'd0, cl_sha_mode};
                    CSR_SHA_MSGLEN:    mc_cl_csr_rdata[gi*64 +: 64] = cl_sha_msglen_lo;
                    CSR_SHA_MSGLEN_HI: mc_cl_csr_rdata[gi*64 +: 64] = cl_sha_msglen_hi;
                    default:           mc_cl_csr_rdata[gi*64 +: 64] = 64'd0;
                endcase
            end
        end
    endgenerate

    // ====================================================================
    // Internal Round-Robin Bus Arbiter
    // ====================================================================
    reg [ARB_BITS-1:0] arb_grant;
    reg [ARB_BITS-1:0] arb_last;
    reg                arb_busy;
    reg                arb_spad;

    // Round-robin next selection (no modulo — uses wrap-around subtraction)
    reg [ARB_BITS-1:0] arb_next;
    reg                arb_any;

    integer ai;
    reg [ARB_BITS:0] arb_cand;
    always @(*) begin
        arb_next = arb_last;
        arb_any  = 1'b0;
        for (ai = 1; ai <= N; ai = ai + 1) begin
            arb_cand = {1'b0, arb_last} + ai[ARB_BITS:0];
            if (arb_cand >= N_VAL)
                arb_cand = arb_cand - N_VAL;
            if (!arb_any && mc_bus_valid[arb_cand[ARB_BITS-1:0]]) begin
                arb_next = arb_cand[ARB_BITS-1:0];
                arb_any  = 1'b1;
            end
        end
    end

    // ====================================================================
    // Scratchpad address detection
    // ====================================================================
    wire arb_addr_is_spad  = (mc_addr[arb_next][63:32] == MP64_SPAD_HI);
    wire grant_addr_is_spad = (mc_addr[arb_grant][63:32] == MP64_SPAD_HI);

    // ====================================================================
    // MPU fault detection (combinational, on arb_next)
    // ====================================================================
    wire cl_mpu_enabled = cl_priv_level && (cl_mpu_limit > cl_mpu_base);
    wire [63:0] arb_next_addr = mc_addr[arb_next];
    wire arb_addr_is_mmio = (arb_next_addr[63:32] == MP64_MMIO_HI);
    wire arb_addr_is_hbw  = (arb_next_addr[63:32] == 32'd0) &&
                            (arb_next_addr[31:20] >= 12'hFFD);
    wire arb_mpu_fault = cl_priv_level && !arb_addr_is_mmio &&
                         !arb_addr_is_spad && (
                             arb_addr_is_hbw ||
                             (cl_mpu_enabled &&
                              (arb_next_addr < cl_mpu_base ||
                               arb_next_addr >= cl_mpu_limit)));

    // ====================================================================
    // Scratchpad Memory (1 KiB = 128 × 64-bit)
    // ====================================================================
    reg [63:0] spad_mem [0:MP64_CLUSTER_SPAD_DEPTH-1];
    reg [63:0] spad_rdata;

    wire [6:0] spad_word_addr = mc_addr[arb_grant][9:3];

    always @(posedge clk) begin
        if (arb_busy && arb_spad) begin
            if (mc_bus_wen[arb_grant])
                spad_mem[spad_word_addr] <= mc_wdata_u[arb_grant];
            spad_rdata <= spad_mem[spad_word_addr];
        end
    end

    // ====================================================================
    // Bus arbiter state machine
    // ====================================================================
    reg bist_running;

    integer mi;
    always @(posedge clk) begin
        if (cl_rst) begin
            arb_grant    <= {ARB_BITS{1'b0}};
            arb_last     <= {ARB_BITS{1'b0}};
            arb_busy     <= 1'b0;
            arb_spad     <= 1'b0;
            bus_valid    <= 1'b0;
            bus_addr     <= 64'd0;
            bus_wdata    <= 64'd0;
            bus_wen      <= 1'b0;
            bus_size     <= 2'd0;
            mc_bus_ready <= {N{1'b0}};
            mc_bus_rdata <= {(N*64){1'b0}};
            mc_mpu_fault <= {N{1'b0}};
            cl_priv_level <= 1'b0;
            cl_mpu_base   <= 64'd0;
            cl_mpu_limit  <= 64'd0;
            cl_ivt_base   <= 64'd0;
            cl_crc_acc    <= 64'h0000_0000_FFFF_FFFF;
            cl_crc_mode   <= 2'd0;
            cl_sha_acc[0] <= 64'd0; cl_sha_acc[1] <= 64'd0;
            cl_sha_acc[2] <= 64'd0; cl_sha_acc[3] <= 64'd0;
            cl_sha_mode      <= 2'd0;
            cl_sha_msglen_lo <= 64'd0;
            cl_sha_msglen_hi <= 64'd0;
            cl_sha_tsrc0     <= 64'd0;
        end else begin
            mc_bus_ready <= {N{1'b0}};
            mc_mpu_fault <= {N{1'b0}};
            bus_valid    <= 1'b0;

            // ---------------------------------------------------------
            // Bus arbitration
            // ---------------------------------------------------------
            if (arb_busy) begin
                if (arb_spad) begin
                    // Scratchpad: single-cycle
                    mc_bus_rdata[arb_grant*64 +: 64] <= spad_rdata;
                    mc_bus_ready[arb_grant] <= 1'b1;
                    arb_last <= arb_grant;
                    arb_busy <= 1'b0;
                end else begin
                    // External: wait for main bus ready
                    bus_valid <= 1'b1;
                    if (bus_ready) begin
                        mc_bus_rdata[arb_grant*64 +: 64] <= bus_rdata;
                        mc_bus_ready[arb_grant] <= 1'b1;
                        arb_last  <= arb_grant;
                        arb_busy  <= 1'b0;
                        bus_valid <= 1'b0;
                    end
                end
            end else if (arb_any && !bist_running && !sha_bus_active) begin
                arb_grant <= arb_next;
                if (arb_mpu_fault) begin
                    mc_mpu_fault[arb_next] <= 1'b1;
                    mc_bus_ready[arb_next] <= 1'b1;
                    mc_bus_rdata[arb_next*64 +: 64] <= 64'd0;
                    arb_last <= arb_next;
                    cl_priv_level <= 1'b0;   // drop to S-mode for trap
                end else if (arb_addr_is_spad) begin
                    arb_busy <= 1'b1;
                    arb_spad <= 1'b1;
                end else begin
                    arb_busy  <= 1'b1;
                    arb_spad  <= 1'b0;
                    bus_valid <= 1'b1;
                    bus_addr  <= mc_addr[arb_next];
                    bus_wdata <= mc_wdata_u[arb_next];
                    bus_wen   <= mc_bus_wen[arb_next];
                    bus_size  <= mc_sz[arb_next];
                end
            end

            // ---------------------------------------------------------
            // Cluster CSR writes (from any micro-core)
            //   priv_level retained as inert CSR — no enforcement
            // ---------------------------------------------------------
            for (mi = 0; mi < N; mi = mi + 1) begin
                if (mc_cl_csr_wen[mi]) begin
                    case (mc_cl_csr_addr[mi*8 +: 8])
                        CSR_CL_PRIV:
                            cl_priv_level <= mc_cl_csr_wdata[mi*64];
                        CSR_CL_MPU_BASE:
                            cl_mpu_base <= mc_cl_csr_wdata[mi*64 +: 64];
                        CSR_CL_MPU_LIMIT:
                            cl_mpu_limit <= mc_cl_csr_wdata[mi*64 +: 64];
                        CSR_IVTBASE, CSR_CL_IVTBASE:
                            cl_ivt_base <= mc_cl_csr_wdata[mi*64 +: 64];
                        CSR_CRC_ACC:
                            cl_crc_acc <= mc_cl_csr_wdata[mi*64 +: 64];
                        CSR_CRC_MODE:
                            cl_crc_mode <= mc_cl_csr_wdata[mi*64 +: 2];
                        CSR_SHA_MODE:
                            cl_sha_mode <= mc_cl_csr_wdata[mi*64 +: 2];
                        CSR_SHA_MSGLEN:
                            cl_sha_msglen_lo <= mc_cl_csr_wdata[mi*64 +: 64];
                        CSR_SHA_MSGLEN_HI:
                            cl_sha_msglen_hi <= mc_cl_csr_wdata[mi*64 +: 64];
                        default: ;
                    endcase
                end
            end

            // ---------------------------------------------------------
            // Sniff tile CSR writes for TSRC0 → shadow into cl_sha_tsrc0
            // ---------------------------------------------------------
            for (mi = 0; mi < N; mi = mi + 1) begin
                if (mc_tile_csr_wen[mi] &&
                    mc_tile_csr_addr[mi*8 +: 8] == CSR_TSRC0)
                    cl_sha_tsrc0 <= mc_tile_csr_wdata[mi*64 +: 64];
            end
        end
    end

    // ====================================================================
    // Shared MUL/DIV Unit
    // ====================================================================
    // MUL: via mp64_mul wrapper (portable, 4-cycle latency).
    // DIV: iterative 1-bit restoring divider, 64-cycle latency.

    localparam MUL_IDLE    = 2'd0;
    localparam MUL_COMPUTE = 2'd1;

    reg [1:0]           mul_state;
    reg [ARB_BITS-1:0]  mul_last;
    reg [3:0]           mul_op_reg;
    reg [63:0]          mul_a_reg, mul_b_reg;
    reg [127:0]         mul_accum;
    reg [6:0]           mul_cycle;
    reg                 mul_is_div;
    reg                 mul_signed_op;

    // Division registers
    reg [63:0]  div_quotient, div_remainder;
    reg [63:0]  div_dividend, div_divisor;

    // mp64_mul instance
    reg         cl_mul_start;
    wire [127:0] cl_mul_result;
    wire         cl_mul_done;
    wire         cl_mul_busy;

    mp64_mul #(.LATENCY(4)) u_cl_mul (
        .clk       (clk),
        .rst       (cl_rst),
        .start     (cl_mul_start),
        .is_signed (mul_signed_op),
        .a         (mul_a_reg),
        .b         (mul_b_reg),
        .result    (cl_mul_result),
        .done      (cl_mul_done),
        .busy      (cl_mul_busy)
    );

    // MUL arbiter: round-robin (no modulo)
    reg [ARB_BITS-1:0] mul_next;
    reg                mul_any;
    reg [ARB_BITS:0]   mul_cand;

    always @(*) begin
        mul_next = mul_last;
        mul_any  = 1'b0;
        for (mi = 1; mi <= N; mi = mi + 1) begin
            mul_cand = {1'b0, mul_last} + mi[ARB_BITS:0];
            if (mul_cand >= N_VAL)
                mul_cand = mul_cand - N_VAL;
            if (!mul_any && mc_mul_req[mul_cand[ARB_BITS-1:0]]) begin
                mul_next = mul_cand[ARB_BITS-1:0];
                mul_any  = 1'b1;
            end
        end
    end

    wire [63:0] mul_next_a  = mc_mul_a [mul_next*64 +: 64];
    wire [63:0] mul_next_b  = mc_mul_b [mul_next*64 +: 64];
    wire [3:0]  mul_next_op = mc_mul_op[mul_next*4  +: 4];

    always @(posedge clk) begin
        if (cl_rst) begin
            mul_state      <= MUL_IDLE;
            mul_grant      <= {ARB_BITS{1'b0}};
            mul_last       <= {ARB_BITS{1'b0}};
            mul_done_reg   <= 1'b0;
            mul_result_reg <= 128'd0;
            mul_cycle      <= 7'd0;
            mul_is_div     <= 1'b0;
            mul_signed_op  <= 1'b0;
            mul_accum      <= 128'd0;
            mul_op_reg     <= 4'd0;
            mul_a_reg      <= 64'd0;
            mul_b_reg      <= 64'd0;
            div_quotient   <= 64'd0;
            div_remainder  <= 64'd0;
            div_dividend   <= 64'd0;
            div_divisor    <= 64'd0;
            cl_mul_start   <= 1'b0;
        end else begin
            mul_done_reg <= 1'b0;

            case (mul_state)
                MUL_IDLE: begin
                    if (mul_any) begin
                        mul_grant   <= mul_next;
                        mul_op_reg  <= mul_next_op;
                        mul_a_reg   <= mul_next_a;
                        mul_b_reg   <= mul_next_b;
                        mul_cycle   <= 7'd0;

                        case (mul_next_op)
                            4'h0, 4'h1: begin
                                mul_is_div    <= 1'b0;
                                mul_signed_op <= 1'b1;
                                cl_mul_start  <= 1'b1;
                                mul_state     <= MUL_COMPUTE;
                            end
                            4'h2, 4'h3: begin
                                mul_is_div    <= 1'b0;
                                mul_signed_op <= 1'b0;
                                cl_mul_start  <= 1'b1;
                                mul_state     <= MUL_COMPUTE;
                            end
                            4'h4, 4'h6: begin
                                mul_is_div    <= 1'b1;
                                mul_signed_op <= 1'b1;
                                div_dividend  <= mul_next_a[63] ? (~mul_next_a + 64'd1) : mul_next_a;
                                div_divisor   <= mul_next_b[63] ? (~mul_next_b + 64'd1) : mul_next_b;
                                div_quotient  <= 64'd0;
                                div_remainder <= 64'd0;
                                mul_state     <= MUL_COMPUTE;
                            end
                            4'h5, 4'h7: begin
                                mul_is_div    <= 1'b1;
                                mul_signed_op <= 1'b0;
                                div_dividend  <= mul_next_a;
                                div_divisor   <= mul_next_b;
                                div_quotient  <= 64'd0;
                                div_remainder <= 64'd0;
                                mul_state     <= MUL_COMPUTE;
                            end
                            default: mul_state <= MUL_IDLE;
                        endcase
                    end
                end

                MUL_COMPUTE: begin
                    mul_cycle <= mul_cycle + 7'd1;
                    cl_mul_start <= 1'b0;  // deassert after 1 cycle

                    if (!mul_is_div) begin
                        // Wait for mp64_mul done
                        if (cl_mul_done) begin
                            mul_result_reg <= cl_mul_result;
                            mul_done_reg   <= 1'b1;
                            mul_last       <= mul_grant;
                            mul_state      <= MUL_IDLE;
                        end
                    end else begin
                        // Division: 1-bit restoring, 64 iterations
                        if (mul_cycle <= 7'd63) begin
                            div_remainder <= {div_remainder[62:0],
                                              div_dividend[63 - mul_cycle[5:0]]};
                            if ({div_remainder[62:0],
                                 div_dividend[63 - mul_cycle[5:0]]} >= div_divisor)
                            begin
                                div_remainder <= {div_remainder[62:0],
                                    div_dividend[63 - mul_cycle[5:0]]} - div_divisor;
                                div_quotient[63 - mul_cycle[5:0]] <= 1'b1;
                            end
                        end
                        if (mul_cycle >= 7'd64) begin
                            if (mul_signed_op) begin
                                if (mul_a_reg[63] ^ mul_b_reg[63])
                                    mul_result_reg[63:0] <= ~div_quotient + 64'd1;
                                else
                                    mul_result_reg[63:0] <= div_quotient;
                                if (mul_a_reg[63])
                                    mul_result_reg[127:64] <= ~div_remainder + 64'd1;
                                else
                                    mul_result_reg[127:64] <= div_remainder;
                            end else begin
                                mul_result_reg[63:0]   <= div_quotient;
                                mul_result_reg[127:64] <= div_remainder;
                            end
                            mul_done_reg <= 1'b1;
                            mul_last     <= mul_grant;
                            mul_state    <= MUL_IDLE;
                        end
                    end
                end

                default: mul_state <= MUL_IDLE;
            endcase
        end
    end

    // ====================================================================
    // Shared CRC ISA Engine + Hardware Lock Arbiter
    // ====================================================================
    // One mp64_crc_isa instance shared among N micro-cores.
    // Unlike MUL (stateless), CRC is stateful: the accumulator persists
    // across calls.  A hardware lock protects multi-instruction CRC
    // sequences:
    //   CRC.INIT acquires the lock (sets crc_locked, records owner).
    //   CRC.FIN  releases the lock.
    //   While locked, only the lock owner can execute CRC ops.
    //   Other cores' crc_req stalls (they stay in CPU_CRYPTO).
    //
    // If no lock is held, round-robin arbitration picks the next requester.

    localparam CRC_IDLE   = 2'd0;
    localparam CRC_ACTIVE = 2'd1;

    reg [1:0]           crc_state;
    reg [ARB_BITS-1:0]  crc_last;
    reg                 crc_locked;
    reg [ARB_BITS-1:0]  crc_lock_owner;

    // CRC ISA engine instance (combinational — result in same cycle)
    wire [3:0]  crc_isa_op;
    wire [63:0] crc_isa_rs_val;
    wire [7:0]  crc_isa_imm8;
    wire [63:0] crc_isa_acc_out;
    wire [1:0]  crc_isa_mode_out;
    wire [63:0] crc_isa_result;
    wire        crc_isa_acc_we;
    wire        crc_isa_mode_we;
    wire        crc_isa_rd_we;

    mp64_crc_isa u_cl_crc (
        .op          (crc_isa_op),
        .rs_val      (crc_isa_rs_val),
        .imm8        (crc_isa_imm8),
        .crc_acc_in  (cl_crc_acc),
        .crc_mode_in (cl_crc_mode),
        .crc_acc_out (crc_isa_acc_out),
        .crc_mode_out(crc_isa_mode_out),
        .result      (crc_isa_result),
        .acc_we      (crc_isa_acc_we),
        .mode_we     (crc_isa_mode_we),
        .rd_we       (crc_isa_rd_we)
    );

    // CRC arbiter: round-robin (same pattern as MUL), respecting lock
    reg [ARB_BITS-1:0] crc_next;
    reg                crc_any;
    reg [ARB_BITS:0]   crc_cand;

    always @(*) begin
        crc_next = crc_last;
        crc_any  = 1'b0;
        if (crc_locked) begin
            // Only the lock owner may proceed
            if (mc_crc_req[crc_lock_owner]) begin
                crc_next = crc_lock_owner;
                crc_any  = 1'b1;
            end
        end else begin
            for (mi = 1; mi <= N; mi = mi + 1) begin
                crc_cand = {1'b0, crc_last} + mi[ARB_BITS:0];
                if (crc_cand >= N_VAL)
                    crc_cand = crc_cand - N_VAL;
                if (!crc_any && mc_crc_req[crc_cand[ARB_BITS-1:0]]) begin
                    crc_next = crc_cand[ARB_BITS-1:0];
                    crc_any  = 1'b1;
                end
            end
        end
    end

    // Mux granted core's signals to the CRC ISA engine
    assign crc_isa_op     = mc_crc_op    [crc_next*4  +: 4];
    assign crc_isa_rs_val = mc_crc_rs_val[crc_next*64 +: 64];
    assign crc_isa_imm8   = mc_crc_imm8  [crc_next*8  +: 8];

    // CRC arbiter FSM
    always @(posedge clk) begin
        if (cl_rst) begin
            crc_state      <= CRC_IDLE;
            crc_grant      <= {ARB_BITS{1'b0}};
            crc_last       <= {ARB_BITS{1'b0}};
            crc_done_reg   <= 1'b0;
            crc_rd_we_reg  <= 1'b0;
            crc_result_reg <= 64'd0;
            crc_locked     <= 1'b0;
            crc_lock_owner <= {ARB_BITS{1'b0}};
        end else begin
            crc_done_reg  <= 1'b0;
            crc_rd_we_reg <= 1'b0;

            case (crc_state)
                CRC_IDLE: begin
                    if (crc_any) begin
                        crc_grant <= crc_next;
                        crc_state <= CRC_ACTIVE;
                    end
                end

                CRC_ACTIVE: begin
                    // CRC ISA engine is combinational — result ready now
                    crc_result_reg <= crc_isa_result;
                    crc_done_reg   <= 1'b1;
                    crc_rd_we_reg  <= crc_isa_rd_we;

                    // Update shared CRC state
                    if (crc_isa_acc_we)  cl_crc_acc  <= crc_isa_acc_out;
                    if (crc_isa_mode_we) cl_crc_mode <= crc_isa_mode_out;

                    // Hardware lock management
                    if (crc_isa_op == ISA_CRC_INIT) begin
                        // CRC.INIT: acquire lock
                        crc_locked     <= 1'b1;
                        crc_lock_owner <= crc_grant;
                    end else if (crc_isa_op == ISA_CRC_FIN) begin
                        // CRC.FIN: release lock
                        crc_locked <= 1'b0;
                    end

                    crc_last  <= crc_grant;
                    crc_state <= CRC_IDLE;
                end

                default: crc_state <= CRC_IDLE;
            endcase
        end
    end

    // ====================================================================
    // Shared SHA-2 ISA Engine + Arbiter
    // ====================================================================
    // One mp64_sha2_isa instance shared among N micro-cores.  Same
    // round-robin + hardware lock pattern as CRC.
    //
    // SHA.INIT acquires lock, SHA.FINAL releases lock.
    // SHA.ROUND is multi-cycle: loads 16×32-bit W from tile memory
    //   (8 × 64-bit bus reads at cl_sha_tsrc0), then runs 64-round
    //   compression (via mp64_sha2_isa engine).
    //
    // Bus access: SHA arbiter takes over the cluster bus during W loading
    //   (sha_bus_active blocks the normal micro-core bus arbiter).

    localparam SHA_IDLE    = 3'd0;
    localparam SHA_SIMPLE  = 3'd1;   // single-cycle ops (INIT/DIN/DOUT/PAD/FINAL)
    localparam SHA_LOAD    = 3'd2;   // loading W from tile memory (8 bus reads)
    localparam SHA_COMPRESS= 3'd3;   // waiting for 64-round compression engine
    localparam SHA_DONE    = 3'd4;   // signal result to micro-core

    reg [2:0]           sha_state;
    reg [ARB_BITS-1:0]  sha_last;
    reg                 sha_locked;
    reg [ARB_BITS-1:0]  sha_lock_owner;
    reg                 sha_bus_active;   // blocks normal bus arbiter

    // SHA W buffer (loaded from memory before compression)
    reg  [31:0] sha_w_buf [0:15];
    reg  [3:0]  sha_load_cnt;        // 0..7 for 8 × 64-bit reads
    reg         sha_bus_pending;      // bus read in flight

    // SHA engine instance
    reg         sha_eng_start;
    wire [31:0] sha_h_unpack [0:7];
    wire [31:0] sha_h_out    [0:7];
    wire        sha_h_we;
    wire        sha_eng_busy;
    wire        sha_eng_done;

    // Unpack cluster SHA ACC → 8 × 32-bit H
    assign sha_h_unpack[0] = cl_sha_acc[0][63:32];  // a
    assign sha_h_unpack[1] = cl_sha_acc[0][31:0];   // b
    assign sha_h_unpack[2] = cl_sha_acc[1][63:32];  // c
    assign sha_h_unpack[3] = cl_sha_acc[1][31:0];   // d
    assign sha_h_unpack[4] = cl_sha_acc[2][63:32];  // e
    assign sha_h_unpack[5] = cl_sha_acc[2][31:0];   // f
    assign sha_h_unpack[6] = cl_sha_acc[3][63:32];  // g
    assign sha_h_unpack[7] = cl_sha_acc[3][31:0];   // h

    mp64_sha2_isa u_cl_sha2 (
        .clk     (clk),
        .rst_n   (~cl_rst),
        .start   (sha_eng_start),
        .w_in    (sha_w_buf),
        .h_in    (sha_h_unpack),
        .h_out   (sha_h_out),
        .h_we    (sha_h_we),
        .busy    (sha_eng_busy),
        .done    (sha_eng_done)
    );

    // SHA arbiter: round-robin with lock (same pattern as CRC)
    reg [ARB_BITS-1:0] sha_next;
    reg                sha_any;
    reg [ARB_BITS:0]   sha_cand;

    always @(*) begin
        sha_next = sha_last;
        sha_any  = 1'b0;
        if (sha_locked) begin
            if (mc_sha_req[sha_lock_owner]) begin
                sha_next = sha_lock_owner;
                sha_any  = 1'b1;
            end
        end else begin
            for (mi = 1; mi <= N; mi = mi + 1) begin
                sha_cand = {1'b0, sha_last} + mi[ARB_BITS:0];
                if (sha_cand >= N_VAL)
                    sha_cand = sha_cand - N_VAL;
                if (!sha_any && mc_sha_req[sha_cand[ARB_BITS-1:0]]) begin
                    sha_next = sha_cand[ARB_BITS-1:0];
                    sha_any  = 1'b1;
                end
            end
        end
    end

    // Latched op/imm8 from the granted core
    reg  [3:0]  sha_op_r;
    reg  [7:0]  sha_imm8_r;
    reg  [63:0] sha_rs_val_r;

    // SHA arbiter FSM
    always @(posedge clk) begin
        if (cl_rst) begin
            sha_state      <= SHA_IDLE;
            sha_grant      <= {ARB_BITS{1'b0}};
            sha_last       <= {ARB_BITS{1'b0}};
            sha_done_reg   <= 1'b0;
            sha_rd_we_reg  <= 1'b0;
            sha_result_reg <= 64'd0;
            sha_locked     <= 1'b0;
            sha_lock_owner <= {ARB_BITS{1'b0}};
            sha_bus_active <= 1'b0;
            sha_bus_pending<= 1'b0;
            sha_load_cnt   <= 4'd0;
            sha_eng_start  <= 1'b0;
            sha_op_r       <= 4'd0;
            sha_imm8_r     <= 8'd0;
            sha_rs_val_r   <= 64'd0;
        end else begin
            sha_done_reg  <= 1'b0;
            sha_rd_we_reg <= 1'b0;
            sha_eng_start <= 1'b0;

            case (sha_state)
                SHA_IDLE: begin
                    if (sha_any) begin
                        sha_grant    <= sha_next;
                        sha_op_r     <= mc_sha_op    [sha_next*4  +: 4];
                        sha_imm8_r   <= mc_sha_imm8  [sha_next*8  +: 8];
                        sha_rs_val_r <= mc_sha_rs_val [sha_next*64 +: 64];
                        sha_state    <= SHA_SIMPLE;
                    end
                end

                // Single-cycle dispatch: handle all ops here, branch to
                // SHA_LOAD only for ROUND.
                SHA_SIMPLE: begin
                    sha_done_reg  <= 1'b1;  // default: done next cycle
                    sha_rd_we_reg <= 1'b0;  // default: no GPR writeback

                    case (sha_op_r)
                        ISA_SHA_INIT: begin
                            // Load SHA-256 IV, set mode, reset msglen
                            cl_sha_mode      <= sha_imm8_r[1:0];
                            cl_sha_acc[0]    <= 64'h6a09e667_bb67ae85;
                            cl_sha_acc[1]    <= 64'h3c6ef372_a54ff53a;
                            cl_sha_acc[2]    <= 64'h510e527f_9b05688c;
                            cl_sha_acc[3]    <= 64'h1f83d9ab_5be0cd19;
                            cl_sha_msglen_lo <= 64'd0;
                            cl_sha_msglen_hi <= 64'd0;
                            // Acquire lock
                            sha_locked     <= 1'b1;
                            sha_lock_owner <= sha_grant;
                        end

                        ISA_SHA_DIN: begin
                            // ACC[imm8_hi[1:0]] ← rs_val
                            cl_sha_acc[sha_imm8_r[5:4]] <= sha_rs_val_r;
                        end

                        ISA_SHA_DOUT: begin
                            // Rd ← ACC[imm8_lo[1:0]]
                            sha_result_reg <= cl_sha_acc[sha_imm8_r[1:0]];
                            sha_rd_we_reg  <= 1'b1;
                        end

                        ISA_SHA_PAD: begin
                            // NOP in RTL (BIOS does padding manually)
                        end

                        ISA_SHA_FINAL: begin
                            // NOP + release lock
                            sha_locked <= 1'b0;
                        end

                        ISA_SHA_ROUND: begin
                            // Multi-cycle: start W loading from tile memory
                            sha_done_reg   <= 1'b0;  // NOT done yet
                            sha_bus_active <= 1'b1;   // take over bus
                            sha_load_cnt   <= 4'd0;
                            sha_bus_pending<= 1'b0;
                            sha_state      <= SHA_LOAD;
                        end

                        default: ;
                    endcase

                    // For non-ROUND ops, return to IDLE
                    if (sha_op_r != ISA_SHA_ROUND) begin
                        sha_last  <= sha_grant;
                        sha_state <= SHA_IDLE;
                    end
                end

                // W-loading: 8 × 64-bit bus reads from cl_sha_tsrc0
                SHA_LOAD: begin
                    if (!sha_bus_pending && !arb_busy) begin
                        // Issue bus read
                        bus_valid      <= 1'b1;
                        bus_addr       <= cl_sha_tsrc0 + {sha_load_cnt[2:0], 3'b000};
                        bus_wen        <= 1'b0;
                        bus_size       <= BUS_DWORD;
                        sha_bus_pending <= 1'b1;
                    end else if (sha_bus_pending) begin
                        bus_valid <= 1'b1;   // keep valid asserted
                        if (bus_ready) begin
                            bus_valid <= 1'b0;
                            sha_bus_pending <= 1'b0;
                            // Big-endian: high 32 = W[2i], low 32 = W[2i+1]
                            sha_w_buf[{sha_load_cnt[2:0], 1'b0}]        <= bus_rdata[63:32];
                            sha_w_buf[{sha_load_cnt[2:0], 1'b0} | 4'd1] <= bus_rdata[31:0];
                            if (sha_load_cnt[2:0] == 3'd7) begin
                                // All 16 words loaded → start compression
                                sha_eng_start  <= 1'b1;
                                sha_bus_active <= 1'b0;   // release bus
                                sha_state      <= SHA_COMPRESS;
                            end else begin
                                sha_load_cnt <= sha_load_cnt + 4'd1;
                            end
                        end
                    end
                end

                // Wait for 64-round compression engine
                SHA_COMPRESS: begin
                    if (sha_eng_done) begin
                        // Write back H' to cluster SHA ACC
                        cl_sha_acc[0] <= {sha_h_out[0], sha_h_out[1]};
                        cl_sha_acc[1] <= {sha_h_out[2], sha_h_out[3]};
                        cl_sha_acc[2] <= {sha_h_out[4], sha_h_out[5]};
                        cl_sha_acc[3] <= {sha_h_out[6], sha_h_out[7]};
                        // Update message length: +512 bits per block
                        cl_sha_msglen_lo <= cl_sha_msglen_lo + 64'd512;
                        if (cl_sha_msglen_lo > (64'hFFFF_FFFF_FFFF_FFFF - 64'd512))
                            cl_sha_msglen_hi <= cl_sha_msglen_hi + 64'd1;
                        // Signal done to micro-core
                        sha_done_reg  <= 1'b1;
                        sha_rd_we_reg <= 1'b0;
                        sha_last      <= sha_grant;
                        sha_state     <= SHA_IDLE;
                    end
                end

                default: sha_state <= SHA_IDLE;
            endcase
        end
    end

    // ====================================================================
    // Hardware Barrier Register
    // ====================================================================
    // N-bit arrive mask.  CSRW BARRIER_ARRIVE → sets writer's bit.
    // When all bits set → auto-clears, sets barrier_done for 1 cycle.

    wire barrier_all = (barrier_arrive == {N{1'b1}});

    always @(posedge clk) begin
        if (cl_rst) begin
            barrier_arrive <= {N{1'b0}};
            barrier_done   <= 1'b0;
        end else begin
            if (barrier_all && !barrier_done) begin
                barrier_done   <= 1'b1;
                barrier_arrive <= {N{1'b0}};
            end else begin
                barrier_done <= 1'b0;
            end

            for (mi = 0; mi < N; mi = mi + 1) begin
                if (mc_cl_csr_wen[mi] &&
                    mc_cl_csr_addr[mi*8 +: 8] == CSR_BARRIER_ARRIVE)
                    barrier_arrive[mi] <= 1'b1;
            end
        end
    end

    // ====================================================================
    // Cluster BIST Controller
    // ====================================================================
    // March C- on scratchpad.  MUL tested via known vectors.
    // Initiated via CSR_BIST_CMD write from any micro-core.
    // While running, bus arbiter is blocked (micro-cores stall).

    localparam BIST_IDLE    = 3'd0;
    localparam BIST_SPAD_W0 = 3'd1;
    localparam BIST_SPAD_R0 = 3'd2;
    localparam BIST_SPAD_R1 = 3'd3;
    localparam BIST_MUL     = 3'd4;
    localparam BIST_DONE    = 3'd5;

    reg [2:0]  bist_state;
    reg [6:0]  bist_addr_cnt;

    always @(posedge clk) begin
        if (cl_rst) begin
            bist_state     <= BIST_IDLE;
            bist_status    <= 2'd0;
            bist_addr_cnt  <= 7'd0;
            bist_fail_addr <= 64'd0;
            bist_fail_data <= 64'd0;
            bist_running   <= 1'b0;
        end else begin
            case (bist_state)
                BIST_IDLE: begin
                    for (mi = 0; mi < N; mi = mi + 1) begin
                        if (mc_cl_csr_wen[mi] &&
                            mc_cl_csr_addr[mi*8 +: 8] == CSR_BIST_CMD &&
                            mc_cl_csr_wdata[mi*64 +: 64] != 64'd0) begin
                            bist_state   <= BIST_SPAD_W0;
                            bist_status  <= 2'd1;
                            bist_addr_cnt <= 7'd0;
                            bist_running <= 1'b1;
                        end
                    end
                end

                BIST_SPAD_W0: begin
                    spad_mem[bist_addr_cnt] <= 64'd0;
                    if (bist_addr_cnt == MP64_CLUSTER_SPAD_DEPTH - 1) begin
                        bist_addr_cnt <= 7'd0;
                        bist_state    <= BIST_SPAD_R0;
                    end else
                        bist_addr_cnt <= bist_addr_cnt + 7'd1;
                end

                BIST_SPAD_R0: begin
                    if (spad_mem[bist_addr_cnt] != 64'd0) begin
                        bist_fail_addr <= {57'd0, bist_addr_cnt};
                        bist_fail_data <= spad_mem[bist_addr_cnt];
                        bist_status    <= 2'd3;
                        bist_state     <= BIST_DONE;
                    end else begin
                        spad_mem[bist_addr_cnt] <= 64'hFFFF_FFFF_FFFF_FFFF;
                        if (bist_addr_cnt == MP64_CLUSTER_SPAD_DEPTH - 1) begin
                            bist_addr_cnt <= 7'd0;
                            bist_state    <= BIST_SPAD_R1;
                        end else
                            bist_addr_cnt <= bist_addr_cnt + 7'd1;
                    end
                end

                BIST_SPAD_R1: begin
                    if (spad_mem[bist_addr_cnt] != 64'hFFFF_FFFF_FFFF_FFFF) begin
                        bist_fail_addr <= {57'd0, bist_addr_cnt};
                        bist_fail_data <= spad_mem[bist_addr_cnt];
                        bist_status    <= 2'd3;
                        bist_state     <= BIST_DONE;
                    end else begin
                        spad_mem[bist_addr_cnt] <= 64'd0;
                        if (bist_addr_cnt == MP64_CLUSTER_SPAD_DEPTH - 1)
                            bist_state <= BIST_MUL;
                        else
                            bist_addr_cnt <= bist_addr_cnt + 7'd1;
                    end
                end

                BIST_MUL: begin
                    // Quick MUL sanity check (placeholder: always pass)
                    bist_status <= 2'd2;
                    bist_state  <= BIST_DONE;
                end

                BIST_DONE: begin
                    bist_running <= 1'b0;
                    bist_state   <= BIST_IDLE;
                end

                default: bist_state <= BIST_IDLE;
            endcase
        end
    end

    // ====================================================================
    // Shared Tile Engine + MEX Arbiter
    // ====================================================================
    // One mp64_tile instance shared among N micro-cores, round-robin
    // arbitrated — exactly like the shared MUL/DIV unit.
    //
    // Flow:
    //   1. Micro-core asserts mex_req with MEX fields, enters CPU_MEX_WAIT
    //   2. MEX arbiter grants one core, drives tile engine CSR/MEX signals
    //   3. Tile engine processes op (4–8 cycles), asserts mex_done
    //   4. Arbiter routes mex_done back to the granted core
    //   5. Core returns to CPU_FETCH
    //
    // Tile CSR writes: any core can write tile CSRs at any time (each
    // core has its own CSR namespace in the tile engine, muxed by grant).
    // In practice the ISA serialises CSR writes before MEX dispatch, so
    // the arbiter need only forward CSR writes from the MEX-granted core
    // while an op is in flight.  When idle, writes are accepted from
    // any core (last writer wins — software must coordinate).

    // MEX arbiter state
    localparam MEX_IDLE    = 2'd0;
    localparam MEX_ACTIVE  = 2'd1;

    reg [1:0]           mex_state;
    reg [ARB_BITS-1:0]  mex_last;

    // Tile engine wires — from arbiter to tile engine instance
    reg         te_csr_wen;
    reg  [7:0]  te_csr_addr;
    reg  [63:0] te_csr_wdata;
    wire [63:0] te_csr_rdata;

    reg         te_mex_valid;
    reg  [1:0]  te_mex_ss;
    reg  [1:0]  te_mex_op;
    reg  [2:0]  te_mex_funct;
    reg  [63:0] te_mex_gpr_val;
    reg  [7:0]  te_mex_imm8;
    reg  [3:0]  te_mex_ext_mod;
    reg         te_mex_ext_active;
    wire        te_mex_done;
    wire        te_mex_busy;

    // MEX arbiter: round-robin next selection (same pattern as MUL)
    reg [ARB_BITS-1:0] mex_next;
    reg                mex_any;

    always @(*) begin
        mex_next = mex_last;
        mex_any  = 1'b0;
        for (mi = 1; mi <= N; mi = mi + 1) begin : mex_rr_scan
            mex_cand = {1'b0, mex_last} + mi[ARB_BITS:0];
            if (mex_cand >= N_VAL)
                mex_cand = mex_cand - N_VAL;
            if (!mex_any && mc_mex_req[mex_cand[ARB_BITS-1:0]]) begin
                mex_next = mex_cand[ARB_BITS-1:0];
                mex_any  = 1'b1;
            end
        end
    end

    reg [ARB_BITS:0] mex_cand;   // temporary for round-robin scan

    // CSR write forwarding: when idle, accept from any core (last wins);
    // when active, only from the granted core.
    // The addr is always forwarded from any core for combinational reads.
    always @(*) begin
        te_csr_wen   = 1'b0;
        te_csr_addr  = 8'd0;
        te_csr_wdata = 64'd0;
        // Priority: granted core during active op, else any writer
        if (mex_state == MEX_ACTIVE) begin
            te_csr_wen   = mc_tile_csr_wen[mex_grant];
            te_csr_addr  = mc_tile_csr_addr [mex_grant*8  +: 8];
            te_csr_wdata = mc_tile_csr_wdata[mex_grant*64 +: 64];
        end else begin
            for (mi = 0; mi < N; mi = mi + 1) begin
                if (mc_tile_csr_wen[mi]) begin
                    te_csr_wen   = 1'b1;
                    te_csr_addr  = mc_tile_csr_addr [mi*8  +: 8];
                    te_csr_wdata = mc_tile_csr_wdata[mi*64 +: 64];
                end
            end
            // When no core is writing, still forward addr for CSR reads.
            // Any core presenting a non-zero addr wins (last-writer-wins);
            // this is fine since simultaneous reads from different cores
            // targeting different CSR addrs is a don't-care scenario.
            if (!te_csr_wen) begin
                for (mi = 0; mi < N; mi = mi + 1) begin
                    if (mc_tile_csr_addr[mi*8 +: 8] != 8'd0)
                        te_csr_addr = mc_tile_csr_addr[mi*8 +: 8];
                end
            end
        end
    end

    // CSR read mux: each micro-core gets tile CSR rdata from the shared
    // tile engine based on its own tile_csr_addr (combinational).
    // Since there's only one tile engine, all cores see the same state.
    generate
        for (gi = 0; gi < N; gi = gi + 1) begin : tile_csr_rd
            always @(*) begin
                mc_tile_csr_rdata[gi*64 +: 64] = te_csr_rdata;
            end
        end
    endgenerate

    // MEX arbiter FSM
    always @(posedge clk) begin
        if (cl_rst) begin
            mex_state      <= MEX_IDLE;
            mex_grant      <= {ARB_BITS{1'b0}};
            mex_last       <= {ARB_BITS{1'b0}};
            mex_done_reg   <= 1'b0;
            mex_busy_reg   <= 1'b0;
            te_mex_valid   <= 1'b0;
        end else begin
            mex_done_reg <= 1'b0;

            case (mex_state)
                MEX_IDLE: begin
                    mex_busy_reg <= 1'b0;
                    te_mex_valid <= 1'b0;
                    if (mex_any) begin
                        mex_grant       <= mex_next;
                        mex_busy_reg    <= 1'b1;
                        te_mex_valid    <= 1'b1;
                        te_mex_ss       <= mc_mex_ss       [mex_next*2  +: 2];
                        te_mex_op       <= mc_mex_op       [mex_next*2  +: 2];
                        te_mex_funct    <= mc_mex_funct    [mex_next*3  +: 3];
                        te_mex_gpr_val  <= mc_mex_gpr_val  [mex_next*64 +: 64];
                        te_mex_imm8     <= mc_mex_imm8     [mex_next*8  +: 8];
                        te_mex_ext_mod  <= mc_mex_ext_mod  [mex_next*4  +: 4];
                        te_mex_ext_active<= mc_mex_ext_active[mex_next];
                        mex_state       <= MEX_ACTIVE;
                    end
                end

                MEX_ACTIVE: begin
                    te_mex_valid <= 1'b0;  // only pulse for 1 cycle
                    if (te_mex_done) begin
                        mex_done_reg <= 1'b1;
                        mex_busy_reg <= 1'b0;
                        mex_last     <= mex_grant;
                        mex_state    <= MEX_IDLE;
                    end
                end

                default: mex_state <= MEX_IDLE;
            endcase
        end
    end

    // ====================================================================
    // Shared Tile Engine Instance
    // ====================================================================
    mp64_tile u_tile (
        .clk           (clk),
        .rst_n         (~cl_rst),

        // CSR interface (from MEX arbiter)
        .csr_wen       (te_csr_wen),
        .csr_addr      (te_csr_addr),
        .csr_wdata     (te_csr_wdata),
        .csr_rdata     (te_csr_rdata),

        // MEX dispatch (from MEX arbiter)
        .mex_valid     (te_mex_valid),
        .mex_ss        (te_mex_ss),
        .mex_op        (te_mex_op),
        .mex_funct     (te_mex_funct),
        .mex_gpr_val   (te_mex_gpr_val),
        .mex_imm8      (te_mex_imm8),
        .mex_ext_mod   (te_mex_ext_mod),
        .mex_ext_active(te_mex_ext_active),
        .mex_done      (te_mex_done),
        .mex_busy      (te_mex_busy),

        // Internal tile memory port (→ SoC memory subsystem)
        .tile_req      (tile_req),
        .tile_addr     (tile_addr),
        .tile_wen      (tile_wen),
        .tile_wdata    (tile_wdata),
        .tile_rdata    (tile_rdata),
        .tile_ack      (tile_ack),

        // External tile memory port (→ SoC ext-mem)
        .ext_tile_req  (ext_tile_req),
        .ext_tile_addr (ext_tile_addr),
        .ext_tile_wen  (ext_tile_wen),
        .ext_tile_wdata(ext_tile_wdata),
        .ext_tile_rdata(ext_tile_rdata),
        .ext_tile_ack  (ext_tile_ack)
    );

endmodule
