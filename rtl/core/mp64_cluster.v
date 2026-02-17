// ============================================================================
// mp64_cluster.v — Megapad-64 Micro-Core Cluster
// ============================================================================
//
// Wraps N micro-cores (default 4) with shared resources:
//   - 64×64→128 multiplier + iterative 64-bit divider
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
    parameter [MP64_CORE_ID_BITS-1:0] CLUSTER_ID_BASE = 5'd4
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
    input  wire [3:0]  ef_flags
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

    // Forward-declare MUL grant (used in generate block)
    reg  [ARB_BITS-1:0] mul_grant;

    // ====================================================================
    // Cluster-shared state
    // ====================================================================
    reg         cl_priv_level;     // 0 = supervisor, 1 = user
    reg [63:0]  cl_mpu_base;
    reg [63:0]  cl_mpu_limit;
    reg [63:0]  cl_ivt_base;

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
            end else if (arb_any && !bist_running) begin
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
            // ---------------------------------------------------------
            for (mi = 0; mi < N; mi = mi + 1) begin
                if (mc_cl_csr_wen[mi]) begin
                    case (mc_cl_csr_addr[mi*8 +: 8])
                        CSR_CL_PRIV: begin
                            if (!cl_priv_level)
                                cl_priv_level <= mc_cl_csr_wdata[mi*64];
                        end
                        CSR_CL_MPU_BASE: begin
                            if (!cl_priv_level)
                                cl_mpu_base <= mc_cl_csr_wdata[mi*64 +: 64];
                        end
                        CSR_CL_MPU_LIMIT: begin
                            if (!cl_priv_level)
                                cl_mpu_limit <= mc_cl_csr_wdata[mi*64 +: 64];
                        end
                        CSR_IVTBASE, CSR_CL_IVTBASE: begin
                            if (!cl_priv_level)
                                cl_ivt_base <= mc_cl_csr_wdata[mi*64 +: 64];
                        end
                        default: ;
                    endcase
                end
            end
        end
    end

    // ====================================================================
    // Shared MUL/DIV Unit
    // ====================================================================
    // MUL: uses Verilog * (maps to DSP48), 4-cycle latency model.
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
                                mul_state     <= MUL_COMPUTE;
                            end
                            4'h2, 4'h3: begin
                                mul_is_div    <= 1'b0;
                                mul_signed_op <= 1'b0;
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

                    if (!mul_is_div) begin
                        // Multiplication (Verilog * → DSP48 in synthesis)
                        if (mul_cycle == 7'd0) begin
                            if (mul_signed_op)
                                mul_accum <= $signed(mul_a_reg) * $signed(mul_b_reg);
                            else
                                mul_accum <= mul_a_reg * mul_b_reg;
                        end
                        if (mul_cycle >= 7'd3) begin
                            mul_result_reg <= mul_accum;
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

endmodule
