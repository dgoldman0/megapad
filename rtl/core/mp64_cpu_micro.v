// ============================================================================
// mp64_cpu_micro.v — Megapad-64 Micro-Core CPU
// ============================================================================
//
// Stripped-down CPU core for packing into micro-core clusters.
// ISA-compatible with the major core for the modern instruction set.
//
// Always lives inside a cluster (mp64_cluster).  No standalone mode.
//
// Compared to the major core (mp64_cpu.v):
//   REMOVED — I-cache (fetches byte-by-byte from bus)
//   REMOVED — 1802-heritage D, Q, T registers
//   REMOVED — Family 0x8 MEMALU (D-register ops) → ILLEGAL_OP
//   REMOVED — Family 0x9 IO (port input/output) → ILLEGAL_OP
//   REMOVED — SYS sub-ops: RET/DIS/MARK/SAV/SEQ/REQ → ILLEGAL_OP
//   REMOVED — IMM sub-ops: GLO/GHI/PLO/PHI → ILLEGAL_OP
//   REMOVED — Per-core privilege/MPU (cluster-shared)
//   REMOVED — Per-core BIST (cluster controller handles this)
//   REMOVED — DMA ring CSRs, I-cache CSRs, tile self-test
//   SHARED  — Tile/MEX engine via cluster (round-robin arbitrated)
//   SHARED  — MUL/DIV via cluster (always)
//   SHARED  — IVT base (cluster-level, input wire)
//   SHARED  — Privilege level + MPU (cluster-level)
//   KEPT    — SEP (0xA) and SEX (0xB) — zero-cost, avoids ISA fragmentation
//   REDUCED — Performance counters: cycles, stalls, and tile operations
//
// Area budget (Kintex-7 estimates):
//   ~1,200 FFs / ~800 LUTs / 0 DSP48
//

`include "mp64_pkg.vh"

module mp64_cpu_micro (
    input  wire        clk,
    input  wire        rst,

    // === Core identification (set by cluster per instance) ===
    input  wire [MP64_CORE_ID_BITS-1:0] core_id,

    // === Memory bus master (to cluster arbiter) ===
    output reg         bus_valid,
    output reg  [63:0] bus_addr,
    output reg  [63:0] bus_wdata,
    output reg         bus_wen,
    output reg  [1:0]  bus_size,
    input  wire [63:0] bus_rdata,
    input  wire        bus_ready,

    // === Cluster MPU fault (from cluster arbiter) ===
    input  wire        mpu_fault,

    // === Interrupts ===
    input  wire        irq_timer,
    input  wire        irq_ipi,

    // === External flags (EF1-EF4) ===
    input  wire [3:0]  ef_flags,

    // === Shared MUL/DIV interface (to cluster controller) ===
    output reg         mul_req,
    output reg  [3:0]  mul_op,
    output reg  [63:0] mul_a,
    output reg  [63:0] mul_b,
    input  wire [127:0] mul_result,
    input  wire        mul_done,

    // === Shared CRC interface (to cluster CRC arbiter) ===
    output reg         crc_req,
    output reg  [3:0]  crc_op,
    output reg  [63:0] crc_rs_val,
    output reg  [7:0]  crc_imm8,
    input  wire [63:0] crc_result,
    input  wire        crc_done,
    input  wire        crc_rd_we_in,  // 1 = write crc_result to R[dst_reg]

    // === Shared SHA interface (to cluster SHA arbiter) ===
    output reg         sha_req,
    output reg  [3:0]  sha_op,
    output reg  [63:0] sha_rs_val,
    output reg  [7:0]  sha_imm8,
    input  wire [63:0] sha_result,
    input  wire        sha_done,
    input  wire        sha_rd_we_in,  // 1 = write sha_result to R[dst_reg]

    // === Shared Field ALU ISA interface (to cluster GF arbiter) ===
    output reg         gf_req,
    output reg  [3:0]  gf_op,
    output reg  [63:0] gf_rd_val,     // R[Rd] value for GF.CMOV condition
    output reg  [7:0]  gf_imm8,
    input  wire [255:0] gf_acc_out,    // updated ACC from GF engine
    input  wire        gf_acc_we,
    input  wire [1:0]  gf_prime_sel_out,
    input  wire        gf_prime_sel_we,
    input  wire        gf_flag_z,
    input  wire        gf_flag_z_we,
    input  wire        gf_done,

    // === Shared Tile/MEX interface (to cluster tile arbiter) ===
    output reg         mex_req,       // request tile engine access
    output reg  [1:0]  mex_ss,        // source selector
    output reg  [1:0]  mex_op,        // operation class
    output reg  [2:0]  mex_funct,     // sub-function
    output reg  [7:0]  mex_funct_byte,// complete canonicality-preserving byte
    output reg  [63:0] mex_gpr_val,   // GPR value (broadcast mode)
    output reg  [7:0]  mex_imm8,      // immediate (splat mode)
    output reg  [3:0]  mex_ext_mod,   // EXT prefix modifier
    output reg         mex_ext_active,// EXT prefix active
    input  wire        mex_done,      // tile op complete (from arbiter)
    input  wire        mex_busy,      // tile engine busy (stall)
    input  wire [2:0]  mex_fault,
    input  wire [63:0] mex_fault_addr,
    input  wire        mex_stall_cycle,

    // Caller context sampled by the cluster arbiter with each request.
    output wire [TACC_CALLER_BITS-1:0] tile_caller_id,
    output wire        tile_priv,
    output wire [63:0] tile_mpu_base,
    output wire [63:0] tile_mpu_limit,
    output wire        tile_mpu_enabled,
    output wire        tile_allow_cluster_spad,

    // TACC status/control bypass MEX admission so force can be acknowledged
    // while another caller owns the shared tile engine.
    input  wire [63:0] tacc_status,
    output reg         tacc_ctl_valid,
    output reg  [63:0] tacc_ctl_wdata,
    input  wire        tacc_ctl_done,
    input  wire [2:0]  tacc_ctl_fault,
    output reg         tacc_priv_fault,

    // === Shared Tile CSR interface (to cluster tile engine) ===
    output reg         tile_csr_wen,
    output reg  [7:0]  tile_csr_addr,
    output reg  [63:0] tile_csr_wdata,
    input  wire [63:0] tile_csr_rdata,
    output reg         tile_csr_req,
    input  wire        tile_csr_done,

    // === Cluster CSR interface (to cluster controller) ===
    // Used for: BIST, barrier, cluster priv/MPU/IVT
    output reg  [7:0]  cl_csr_addr,
    output reg         cl_csr_wen,
    output reg  [63:0] cl_csr_wdata,
    input  wire [63:0] cl_csr_rdata,

    // === Cluster-shared state (inputs from cluster) ===
    input  wire [63:0] cl_ivt_base,
    input  wire        cl_priv_level,
    input  wire [63:0] cl_mpu_base,
    input  wire [63:0] cl_mpu_limit
);

    `include "mp64_cpu_funcs.vh"

    // ====================================================================
    // Register file
    // ====================================================================
    reg [63:0] R [0:15];
    reg [3:0]  psel, xsel, spsel;
    reg [7:0]  flags;                 // [S I G P V N C Z]

    wire [63:0] PC = R[psel];
    wire [63:0] SP = R[spsel];
    wire [63:0] RX = R[xsel];

    // Trap / interrupt context
    reg [7:0]  ivec_id;
    reg [63:0] trap_addr;
    reg [63:0] trap_return_pc;

    assign tile_caller_id          = core_id;
    assign tile_priv               = cl_priv_level;
    assign tile_mpu_base           = cl_mpu_base;
    assign tile_mpu_limit          = cl_mpu_limit;
    assign tile_mpu_enabled        = cl_priv_level &&
                                     (cl_mpu_limit > cl_mpu_base);
    assign tile_allow_cluster_spad = 1'b1;

    // Per-microcore performance counters.
    reg [63:0] perf_cycles, perf_stalls, perf_tileops;
    reg        perf_enable;

    // EXT prefix
    reg [3:0]  ext_mod;
    reg        ext_active;

    // ====================================================================
    // Instruction fetch buffer
    // ====================================================================
    // No I-cache — fetches byte-by-byte through the cluster bus arbiter
    reg [7:0]  ibuf [0:10];           // up to 11 bytes (EXT + LDI imm64)
    reg [3:0]  ibuf_len;
    reg [3:0]  ibuf_need;
    reg        fetch_pending;
    reg        skip_fetch_pending;
    reg        skip_has_rex;

    wire [3:0] fam = ibuf[0][7:4];
    wire [3:0] nib = ibuf[0][3:0];

    // ====================================================================
    // CPU FSM
    // ====================================================================
    localparam [4:0] CPU_SKIP_REX    = 5'd23;
    localparam [4:0] CPU_SKIP_CRYPTO = 5'd24;

    reg [4:0]  cpu_state;
    function [7:0] mex_fault_vector;
        input [2:0] fault;
        begin
            case (fault)
                MEX_FAULT_ILLEGAL: mex_fault_vector = IRQX_ILLEGAL_OP;
                MEX_FAULT_ALIGN:   mex_fault_vector = IRQX_ALIGN;
                MEX_FAULT_BUS:     mex_fault_vector = IRQX_BUS;
                MEX_FAULT_PRIV:    mex_fault_vector = IRQX_PRIV;
                default:           mex_fault_vector = IRQX_ILLEGAL_OP;
            endcase
        end
    endfunction

    // ====================================================================
    // ALU instance (one per micro-core — combinational, cheap)
    // ====================================================================
    reg  [3:0]  alu_op;
    reg  [63:0] alu_a, alu_b;
    wire [63:0] alu_result;
    wire [7:0]  alu_flags_out;

    mp64_alu u_alu (
        .op       (alu_op),
        .a        (alu_a),
        .b        (alu_b),
        .flags_in (flags),
        .result   (alu_result),
        .flags_out(alu_flags_out)
    );

    // ====================================================================
    // Bitfield ALU instance (Tier 1 only — micro-core)
    // ====================================================================
    reg  [2:0]  bf_op;
    reg  [63:0] bf_a, bf_b;
    wire [63:0] bf_result;
    wire        bf_flag_z, bf_flag_n;
    reg         bf_active;

    mp64_bitfield #(.ENABLE_TIER2(0)) u_bitfield (
        .op     (bf_op),
        .a      (bf_a),
        .b      (bf_b),
        .imm    (6'd0),         // RORI not available on micro-cores
        .result (bf_result),
        .flag_z (bf_flag_z),
        .flag_n (bf_flag_n)
    );

    // Combinational CSR address — cluster muxes rdata on this
    always @(*) cl_csr_addr = ibuf[1];

    // Caller-private tile CSRs retain their immediate combinational read
    // path.  Shared ACC transactions select a captured address for the
    // entire acknowledged request.
    reg        tile_csr_write;
    reg [7:0]  tile_csr_addr_hold;
    reg [3:0]  tile_csr_dst_reg;
    always @(*) begin
        if (tile_csr_req)
            tile_csr_addr = tile_csr_addr_hold;
        else
            tile_csr_addr = ibuf[1];
    end

    // ====================================================================
    // Interrupt pending
    // ====================================================================
    reg        irq_pending;
    reg [3:0]  irq_vector;

    always @(*) begin
        irq_pending = 1'b0;
        irq_vector  = 4'd0;
        if (flags[6]) begin           // IE
            if      (irq_ipi)   begin irq_pending = 1'b1; irq_vector = IRQX_IPI;          end
            else if (irq_timer) begin irq_pending = 1'b1; irq_vector = {1'b0, IRQ_TIMER}; end
        end
    end

    // ====================================================================
    // Multi-cycle temporaries
    // ====================================================================
    reg [63:0] mem_data;
    reg [3:0]  dst_reg, src_reg;
    reg [63:0] effective_addr;
    reg [3:0]  mem_sub;
    reg [2:0]  post_action;

    // ====================================================================
    // Main FSM
    // ====================================================================
    always @(posedge clk) begin
        if (rst) begin
            cpu_state     <= CPU_FETCH;
            bus_valid     <= 1'b0;
            cl_csr_wen    <= 1'b0;
            tile_csr_wen  <= 1'b0;
            mex_req       <= 1'b0;
            ext_active    <= 1'b0;
            ext_mod       <= 4'd0;
            fetch_pending <= 1'b0;
            skip_fetch_pending <= 1'b0;
            skip_has_rex <= 1'b0;
            ibuf_len      <= 4'd0;
            ibuf_need     <= 4'd1;

            psel  <= 4'd3;
            xsel  <= 4'd2;
            spsel <= 4'd15;
            flags <= 8'h40;           // I=1

            ivec_id   <= 8'd0;
            trap_addr <= 64'd0;
            trap_return_pc <= 64'd0;

            post_action <= POST_NONE;
            mem_sub     <= 4'd0;

            alu_op <= 4'd0;
            alu_a  <= 64'd0;
            alu_b  <= 64'd0;

            bf_op     <= 3'd0;
            bf_a      <= 64'd0;
            bf_b      <= 64'd0;
            bf_active <= 1'b0;

            mul_req <= 1'b0;
            mul_op  <= 4'd0;
            mul_a   <= 64'd0;
            mul_b   <= 64'd0;

            crc_req    <= 1'b0;
            crc_op     <= 4'd0;
            crc_rs_val <= 64'd0;
            crc_imm8   <= 8'd0;

            sha_req    <= 1'b0;
            gf_req     <= 1'b0;
            gf_op      <= 4'd0;
            gf_rd_val  <= 64'd0;
            gf_imm8    <= 8'd0;
            sha_op     <= 4'd0;
            sha_rs_val <= 64'd0;
            sha_imm8   <= 8'd0;

            mex_req        <= 1'b0;
            mex_ss         <= 2'd0;
            mex_op         <= 2'd0;
            mex_funct      <= 3'd0;
            mex_funct_byte <= 8'd0;
            mex_gpr_val    <= 64'd0;
            mex_imm8       <= 8'd0;
            mex_ext_mod    <= 4'd0;
            mex_ext_active <= 1'b0;
            tacc_ctl_valid <= 1'b0;
            tacc_ctl_wdata <= 64'd0;
            tacc_priv_fault <= 1'b0;
            tile_csr_wen   <= 1'b0;
            tile_csr_wdata <= 64'd0;
            tile_csr_req   <= 1'b0;
            tile_csr_write <= 1'b0;
            tile_csr_addr_hold <= 8'd0;
            tile_csr_dst_reg   <= 4'd0;

            cl_csr_wen   <= 1'b0;
            cl_csr_wdata <= 64'd0;

            perf_cycles  <= 64'd0;
            perf_stalls  <= 64'd0;
            perf_tileops <= 64'd0;
            perf_enable  <= 1'b1;

            R[0]  <= 64'd0; R[1]  <= 64'd0; R[2]  <= 64'd0; R[3]  <= 64'd0;
            R[4]  <= 64'd0; R[5]  <= 64'd0; R[6]  <= 64'd0; R[7]  <= 64'd0;
            R[8]  <= 64'd0; R[9]  <= 64'd0; R[10] <= 64'd0; R[11] <= 64'd0;
            R[12] <= 64'd0; R[13] <= 64'd0; R[14] <= 64'd0; R[15] <= 64'd0;

        end else begin
            bus_valid    <= 1'b0;
            cl_csr_wen   <= 1'b0;
            tile_csr_wen <= 1'b0;
            tacc_priv_fault <= 1'b0;

            if (perf_enable) begin
                perf_cycles <= perf_cycles + 64'd1;
                if ((cpu_state == CPU_FETCH_MORE && fetch_pending && !bus_ready) ||
                    (cpu_state == CPU_MEM_READ   && !bus_ready) ||
                    (cpu_state == CPU_MEM_WRITE  && !bus_ready) ||
                    (cpu_state == CPU_MEM_READ2  && !bus_ready) ||
                    (cpu_state == CPU_IRQ_PUSH   && !bus_ready) ||
                    (cpu_state == CPU_IRQ_LOAD   && !bus_ready) ||
                    (cpu_state == CPU_CRYPTO     && !crc_done) ||
                    (cpu_state == CPU_SHA_WAIT   && !sha_done) ||
                    (cpu_state == CPU_GF_WAIT    && !gf_done) ||
                    (cpu_state == CPU_MEX_WAIT   && mex_stall_cycle) ||
                    (cpu_state == CPU_CSR_WAIT   && !tacc_ctl_done) ||
                    (cpu_state == CPU_TILE_CSR_WAIT && !tile_csr_done))
                    perf_stalls <= perf_stalls + 64'd1;
                if (cpu_state == CPU_MEX_WAIT && mex_done &&
                    mex_fault == MEX_FAULT_NONE)
                    perf_tileops <= perf_tileops + 64'd1;
            end

            case (cpu_state)

            // ============================================================
            // FETCH: check for pending interrupts
            // ============================================================
            CPU_FETCH: begin
                if (irq_pending && ibuf_len == 4'd0) begin
                    ivec_id  <= {4'd0, irq_vector};
                    cpu_state <= CPU_IRQ;
                end else begin
                    fetch_pending <= 1'b0;
                    cpu_state     <= CPU_FETCH_MORE;
                end
            end

            // ============================================================
            // FETCH_MORE: read instruction bytes one at a time
            // ============================================================
            CPU_FETCH_MORE: begin
                if (!fetch_pending) begin
                    bus_valid     <= 1'b1;
                    bus_addr      <= R[psel] + {60'd0, ibuf_len};
                    bus_wen       <= 1'b0;
                    bus_size      <= BUS_BYTE;
                    fetch_pending <= 1'b1;
                end else if (!bus_ready) begin
                    bus_valid <= 1'b1;
                end

                if (bus_ready && fetch_pending) begin
                    fetch_pending      <= 1'b0;
                    ibuf[ibuf_len]     <= bus_rdata[7:0];
                    ibuf_len           <= ibuf_len + 4'd1;

                    if (ibuf_len == 4'd0)
                        ibuf_need <= instr_len(bus_rdata[7:0], ext_active);

                    if (ibuf_len == 4'd0) begin
                        if (instr_len(bus_rdata[7:0], ext_active) == 4'd1)
                            cpu_state <= CPU_DECODE;
                    end else if (ibuf_len == 4'd1 && ibuf[0] == 8'hFB &&
                                 crypto_is_bare(bus_rdata[7:0])) begin
                        // EXT.CRYPTO advertises its three-byte maximum from
                        // byte zero. Bare sub-ops complete after byte one.
                        ibuf_need <= 4'd2;
                        cpu_state <= CPU_DECODE;
                    end else if (ibuf_len + 4'd1 >= ibuf_need) begin
                        cpu_state <= CPU_DECODE;
                    end
                end
            end

            // ============================================================
            // DECODE + EXECUTE
            // ============================================================
            CPU_DECODE: begin
                R[psel]     <= R[psel] + {60'd0, ibuf_len};
                ibuf_len    <= 4'd0;
                ibuf_need   <= 4'd1;
                post_action <= POST_NONE;
                bf_active   <= 1'b0;

                // --------------------------------------------------------
                // EXT prefix (0xF)
                // --------------------------------------------------------
                if (fam == FAM_EXT) begin
                    if (nib == EXT_STRING || nib == EXT_DICT) begin
                        // EXT.STRING / EXT.DICT not available on micro-cores → ILLEGAL_OP
                        R[spsel] <= R[spsel] - 64'd8;
                        effective_addr <= R[spsel] - 64'd8;
                        trap_return_pc <= R[psel];
                        mem_data <= {56'd0, flags};
                        flags[6] <= 1'b0;
                        ivec_id  <= IRQX_ILLEGAL_OP;
                        post_action <= POST_IRQ_VEC;
                        bus_size <= BUS_DWORD;
                        cpu_state <= CPU_MEM_WRITE;
                    end else if (nib == EXT_CRYPTO) begin
                        // EXT.CRYPTO — dispatch to cluster-shared engines
                        //   ibuf[1] = sub-op: [7:4]=unit, [3:0]=op
                        //   ibuf[2] = DR or imm8 (3-byte ops only)
                        if (ibuf[1][7:4] == 4'd0 &&
                            ibuf[1][3:0] > ISA_CRC_FINRAW) begin
                            // Reserved CRC sub-ops are fail-closed two-byte
                            // instructions, matching full cores/emulators.
                            R[spsel] <= R[spsel] - 64'd8;
                            effective_addr <= R[spsel] - 64'd8;
                            // Capture the same post-instruction PC scheduled
                            // above. A REX prefix has already advanced R[P],
                            // so the current CRC body's ibuf length is exact
                            // for both prefixed and unprefixed encodings.
                            trap_return_pc <=
                                R[psel] + {60'd0, ibuf_len};
                            mem_data <= {56'd0, flags};
                            flags[6] <= 1'b0;
                            ivec_id  <= IRQX_ILLEGAL_OP;
                            post_action <= POST_IRQ_VEC;
                            bus_size <= BUS_DWORD;
                            ext_active <= 1'b0;
                            cpu_state <= CPU_MEM_WRITE;
                        end else if (ibuf[1][7:4] == 4'd0) begin
                            // CRC unit (0) → cluster CRC arbiter
                            crc_req    <= 1'b1;
                            crc_op     <= ibuf[1][3:0];
                            crc_rs_val <= R[ibuf[2][3:0]];
                            crc_imm8   <= ibuf[2];
                            dst_reg    <= ibuf[2][7:4];
                            ext_active <= 1'b0;
                            cpu_state  <= CPU_CRYPTO;
                        end else if (ibuf[1][7:4] == 4'd1 &&
                                     ibuf[1][3:0] > ISA_SHA_RELEASE) begin
                            // Reserved SHA sub-ops are complete two-byte
                            // instructions and must fail closed.
                            R[spsel] <= R[spsel] - 64'd8;
                            effective_addr <= R[spsel] - 64'd8;
                            trap_return_pc <= R[psel];
                            mem_data <= {56'd0, flags};
                            flags[6] <= 1'b0;
                            ivec_id <= IRQX_ILLEGAL_OP;
                            post_action <= POST_IRQ_VEC;
                            bus_size <= BUS_DWORD;
                            ext_active <= 1'b0;
                            cpu_state <= CPU_MEM_WRITE;
                        end else if (ibuf[1][7:4] == 4'd1) begin
                            // SHA-2 unit (1) → cluster SHA arbiter
                            sha_req    <= 1'b1;
                            sha_op     <= ibuf[1][3:0];
                            sha_rs_val <= R[ibuf[2][3:0]];
                            sha_imm8   <= ibuf[2];
                            dst_reg    <= ibuf[2][7:4];
                            ext_active <= 1'b0;
                            cpu_state  <= CPU_SHA_WAIT;
                        end else begin
                            // Unsupported unit → ILLEGAL_OP
                            R[spsel] <= R[spsel] - 64'd8;
                            effective_addr <= R[spsel] - 64'd8;
                            trap_return_pc <= R[psel];
                            mem_data <= {56'd0, flags};
                            flags[6] <= 1'b0;
                            ivec_id  <= IRQX_ILLEGAL_OP;
                            post_action <= POST_IRQ_VEC;
                            bus_size <= BUS_DWORD;
                            cpu_state <= CPU_MEM_WRITE;
                        end
                    end else begin
                        ext_active <= 1'b1;
                        ext_mod    <= nib;
                        cpu_state  <= CPU_FETCH;
                    end
                end

                // --------------------------------------------------------
                // SYS (0x0)
                // --------------------------------------------------------
                else if (fam == FAM_SYS) begin
                    ext_active <= 1'b0;
                    case (nib)
                        4'h0: cpu_state <= CPU_HALT;           // IDL
                        4'h1: cpu_state <= CPU_FETCH;          // NOP
                        4'h2: cpu_state <= CPU_HALT;           // HALT

                        4'h3: begin // RESET
                            psel <= 4'd3; xsel <= 4'd2; spsel <= 4'd15;
                            flags <= 8'h40;
                            ivec_id <= 8'd0;
                            R[0] <= 64'd0;  R[1] <= 64'd0;  R[2] <= 64'd0;  R[3] <= 64'd0;
                            R[4] <= 64'd0;  R[5] <= 64'd0;  R[6] <= 64'd0;  R[7] <= 64'd0;
                            R[8] <= 64'd0;  R[9] <= 64'd0;  R[10]<= 64'd0;  R[11]<= 64'd0;
                            R[12]<= 64'd0;  R[13]<= 64'd0;  R[14]<= 64'd0;  R[15]<= 64'd0;
                            cpu_state <= CPU_FETCH;
                        end

                        4'h4: begin // RTI
                            effective_addr <= R[spsel];
                            R[spsel]    <= R[spsel] + 64'd8;
                            dst_reg     <= psel;
                            post_action <= POST_RTI_POP2;
                            cpu_state   <= CPU_MEM_READ;
                        end

                        // 1802 heritage: RET/DIS/MARK/SAV/SEQ/REQ → ILLEGAL_OP
                        4'h5, 4'h6, 4'h7, 4'h8, 4'h9, 4'hA: begin
                            R[spsel] <= R[spsel] - 64'd8;
                            effective_addr <= R[spsel] - 64'd8;
                            trap_return_pc <= R[psel];
                            mem_data <= {56'd0, flags};
                            flags[6] <= 1'b0;
                            ivec_id  <= IRQX_ILLEGAL_OP;
                            post_action <= POST_IRQ_VEC;
                            bus_size <= BUS_DWORD;
                            cpu_state <= CPU_MEM_WRITE;
                        end

                        4'hB: begin flags[6] <= 1'b1; cpu_state <= CPU_FETCH; end  // EI
                        4'hC: begin flags[6] <= 1'b0; cpu_state <= CPU_FETCH; end  // DI

                        4'hD: begin // CALL.L Rn
                            R[spsel] <= R[spsel] - 64'd8;
                            effective_addr <= R[spsel] - 64'd8;
                            mem_data <= R[psel] + {60'd0, ibuf_len};
                            R[psel]  <= R[ibuf[1][3:0]];
                            bus_size <= BUS_DWORD;
                            cpu_state <= CPU_MEM_WRITE;
                        end

                        4'hE: begin // RET.L
                            effective_addr <= R[spsel];
                            R[spsel] <= R[spsel] + 64'd8;
                            dst_reg  <= psel;
                            bus_size <= BUS_DWORD;
                            cpu_state <= CPU_MEM_READ;
                        end

                        4'hF: begin // TRAP
                            R[spsel] <= R[spsel] - 64'd8;
                            effective_addr <= R[spsel] - 64'd8;
                            trap_return_pc <= R[psel] + {60'd0, ibuf_len};
                            mem_data <= {56'd0, flags};
                            flags[6] <= 1'b0;
                            ivec_id  <= IRQX_SW_TRAP;
                            post_action <= POST_IRQ_VEC;
                            bus_size <= BUS_DWORD;
                            cpu_state <= CPU_MEM_WRITE;
                        end
                    endcase
                end

                // --------------------------------------------------------
                // INC (0x1)
                // --------------------------------------------------------
                else if (fam == FAM_INC) begin
                    ext_active <= 1'b0;
                    R[nib] <= R[nib] + 64'd1;
                    cpu_state <= CPU_FETCH;
                end

                // --------------------------------------------------------
                // DEC (0x2)
                // --------------------------------------------------------
                else if (fam == FAM_DEC) begin
                    ext_active <= 1'b0;
                    R[nib] <= R[nib] - 64'd1;
                    cpu_state <= CPU_FETCH;
                end

                // --------------------------------------------------------
                // BR (0x3) — short branch / SKIP
                // --------------------------------------------------------
                else if (fam == FAM_BR) begin
                    ext_active <= 1'b0;
                    if (ext_active && ext_mod == EXT_SKIP) begin
                        if (cond_eval(nib, flags, 1'b0, ef_flags)) begin
                            // Need to read byte0 of next instr via bus
                            bus_valid <= 1'b1;
                            bus_addr  <= R[psel] + {60'd0, ibuf_len};
                            bus_wen   <= 1'b0;
                            bus_size  <= BUS_BYTE;
                            skip_fetch_pending <= 1'b1;
                            skip_has_rex <= 1'b0;
                            cpu_state <= CPU_SKIP;
                        end else
                            cpu_state <= CPU_FETCH;
                    end else begin
                        if (cond_eval(nib, flags, 1'b0, ef_flags))
                            R[psel] <= R[psel] + {{56{ibuf[1][7]}}, ibuf[1]}
                                       + {60'd0, ibuf_len};
                        cpu_state <= CPU_FETCH;
                    end
                end

                // --------------------------------------------------------
                // LBR (0x4)
                // --------------------------------------------------------
                else if (fam == FAM_LBR) begin
                    ext_active <= 1'b0;
                    if (cond_eval(nib, flags, 1'b0, ef_flags))
                        R[psel] <= R[psel] + {{48{ibuf[1][7]}}, ibuf[1], ibuf[2]}
                                   + {60'd0, ibuf_len};
                    cpu_state <= CPU_FETCH;
                end

                // --------------------------------------------------------
                // MEM (0x5) — all 16 sub-ops
                // --------------------------------------------------------
                else if (fam == FAM_MEM) begin
                    ext_active <= 1'b0;
                    dst_reg <= ibuf[1][7:4];
                    src_reg <= ibuf[1][3:0];
                    mem_sub <= nib;
                    case (nib)
                        4'h0: begin effective_addr<=R[ibuf[1][3:0]]; bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_READ; end
                        4'h1: begin effective_addr<=R[ibuf[1][3:0]]; bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_READ; end
                        4'h2: begin effective_addr<=R[xsel];         bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_READ; end
                        4'h3: begin effective_addr<=R[xsel];         bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_READ; end
                        4'h4: begin effective_addr<=R[ibuf[1][7:4]]; mem_data<=R[ibuf[1][3:0]]; bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_WRITE; end
                        4'h5: begin effective_addr<=R[xsel]; mem_data<=R[ibuf[1][7:4]]; bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_WRITE; end
                        4'h6: begin effective_addr<=R[ibuf[1][3:0]]; bus_size<=BUS_BYTE;  cpu_state<=CPU_MEM_READ; end
                        4'h7: begin effective_addr<=R[ibuf[1][7:4]]; mem_data<={56'd0,R[ibuf[1][3:0]][7:0]}; bus_size<=BUS_BYTE; cpu_state<=CPU_MEM_WRITE; end
                        4'h8: begin effective_addr<=R[ibuf[1][3:0]]; bus_size<=BUS_HALF;  cpu_state<=CPU_MEM_READ; end
                        4'h9: begin effective_addr<=R[ibuf[1][7:4]]; mem_data<={48'd0,R[ibuf[1][3:0]][15:0]}; bus_size<=BUS_HALF; cpu_state<=CPU_MEM_WRITE; end
                        4'hA: begin effective_addr<=R[ibuf[1][3:0]]; bus_size<=BUS_WORD;  cpu_state<=CPU_MEM_READ; end
                        4'hB: begin effective_addr<=R[ibuf[1][7:4]]; mem_data<={32'd0,R[ibuf[1][3:0]][31:0]}; bus_size<=BUS_WORD; cpu_state<=CPU_MEM_WRITE; end
                        4'hC: begin effective_addr<=R[ibuf[1][3:0]]; bus_size<=BUS_BYTE;  cpu_state<=CPU_MEM_READ; end
                        4'hD: begin effective_addr<=R[ibuf[1][3:0]]; bus_size<=BUS_HALF;  cpu_state<=CPU_MEM_READ; end
                        4'hE: begin effective_addr<=R[ibuf[1][3:0]]; bus_size<=BUS_WORD;  cpu_state<=CPU_MEM_READ; end
                        4'hF: begin
                            effective_addr <= R[ibuf[1][3:0]] + ({{56{ibuf[2][7]}}, ibuf[2]} << 3);
                            bus_size <= BUS_DWORD; cpu_state <= CPU_MEM_READ;
                        end
                    endcase
                end

                // --------------------------------------------------------
                // IMM (0x6) — 16 sub-ops (GLO/GHI/PLO/PHI trap)
                // --------------------------------------------------------
                else if (fam == FAM_IMM) begin
                    dst_reg <= ibuf[1][7:4];
                    if (ext_active && ext_mod == EXT_IMM64) begin
                        R[ibuf[1][7:4]] <= {ibuf[9], ibuf[8], ibuf[7], ibuf[6],
                                             ibuf[5], ibuf[4], ibuf[3], ibuf[2]};
                        ext_active <= 1'b0;
                        cpu_state  <= CPU_FETCH;
                    end else begin
                        ext_active <= 1'b0;
                        case (nib)
                            4'h0: begin R[ibuf[1][7:4]] <= {56'd0, ibuf[2]}; cpu_state <= CPU_FETCH; end
                            4'h1: begin R[ibuf[1][7:4]][63:48] <= {ibuf[3], ibuf[2]}; cpu_state <= CPU_FETCH; end
                            4'h2: begin alu_a<=R[ibuf[1][7:4]]; alu_b<={{56{ibuf[2][7]}},ibuf[2]}; alu_op<=ALU_ADD; cpu_state<=CPU_EXECUTE; end
                            4'h3: begin alu_a<=R[ibuf[1][7:4]]; alu_b<={56'd0,ibuf[2]}; alu_op<=ALU_AND; cpu_state<=CPU_EXECUTE; end
                            4'h4: begin alu_a<=R[ibuf[1][7:4]]; alu_b<={56'd0,ibuf[2]}; alu_op<=ALU_OR;  cpu_state<=CPU_EXECUTE; end
                            4'h5: begin alu_a<=R[ibuf[1][7:4]]; alu_b<={56'd0,ibuf[2]}; alu_op<=ALU_XOR; cpu_state<=CPU_EXECUTE; end
                            4'h6: begin alu_a<=R[ibuf[1][7:4]]; alu_b<={{56{ibuf[2][7]}},ibuf[2]}; alu_op<=ALU_CMP; cpu_state<=CPU_EXECUTE; end
                            4'h7: begin alu_a<=R[ibuf[1][7:4]]; alu_b<={{56{ibuf[2][7]}},ibuf[2]}; alu_op<=ALU_SUB; cpu_state<=CPU_EXECUTE; end
                            4'h8: begin R[ibuf[1][7:4]] <= R[ibuf[1][7:4]] << ibuf[1][3:0]; cpu_state <= CPU_FETCH; end
                            4'h9: begin R[ibuf[1][7:4]] <= R[ibuf[1][7:4]] >> ibuf[1][3:0]; cpu_state <= CPU_FETCH; end
                            4'hA: begin R[ibuf[1][7:4]] <= $signed(R[ibuf[1][7:4]]) >>> ibuf[1][3:0]; cpu_state <= CPU_FETCH; end
                            4'hB: begin
                                if (ibuf[1][3:0] != 0)
                                    R[ibuf[1][7:4]] <= (R[ibuf[1][7:4]] << ibuf[1][3:0])
                                                     | (R[ibuf[1][7:4]] >> (4'd0 - ibuf[1][3:0]));
                                cpu_state <= CPU_FETCH;
                            end
                            // GLO/GHI/PLO/PHI → D-register ops, not on micro-core
                            4'hC, 4'hD, 4'hE, 4'hF: begin
                                R[spsel] <= R[spsel] - 64'd8;
                                effective_addr <= R[spsel] - 64'd8;
                                trap_return_pc <= R[psel];
                                mem_data <= {56'd0, flags};
                                flags[6] <= 1'b0;
                                ivec_id  <= IRQX_ILLEGAL_OP;
                                post_action <= POST_IRQ_VEC;
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_WRITE;
                            end
                        endcase
                    end
                end

                // --------------------------------------------------------
                // ALU (0x7) — all 16 sub-ops
                // --------------------------------------------------------
                else if (fam == FAM_ALU) begin
                    ext_active <= 1'b0;
                    dst_reg <= ibuf[1][7:4];
                    src_reg <= ibuf[1][3:0];
                    alu_a   <= R[ibuf[1][7:4]];
                    alu_b   <= R[ibuf[1][3:0]];
                    case (nib)
                        4'h0: alu_op <= ALU_ADD;  4'h1: alu_op <= ALU_ADC;
                        4'h2: alu_op <= ALU_SUB;  4'h3: alu_op <= ALU_SBB;
                        4'h4: alu_op <= ALU_AND;  4'h5: alu_op <= ALU_OR;
                        4'h6: alu_op <= ALU_XOR;  4'h7: alu_op <= ALU_CMP;
                        4'h8: alu_op <= ALU_MOV;  4'h9: alu_op <= ALU_NOT;
                        4'hA: alu_op <= ALU_NEG;  4'hB: alu_op <= ALU_SHL;
                        4'hC: alu_op <= ALU_SHR;  4'hD: alu_op <= ALU_SAR;
                        4'hE: alu_op <= ALU_ROL;  4'hF: alu_op <= ALU_ROR;
                    endcase
                    cpu_state <= CPU_EXECUTE;
                end

                // --------------------------------------------------------
                // MEMALU (0x8) — stripped → ILLEGAL_OP
                // --------------------------------------------------------
                else if (fam == FAM_MEMALU) begin
                    ext_active <= 1'b0;
                    R[spsel] <= R[spsel] - 64'd8;
                    effective_addr <= R[spsel] - 64'd8;
                    trap_return_pc <= R[psel];
                    mem_data <= {56'd0, flags}; flags[6] <= 1'b0;
                    ivec_id  <= IRQX_ILLEGAL_OP;
                    post_action <= POST_IRQ_VEC;
                    bus_size <= BUS_DWORD; cpu_state <= CPU_MEM_WRITE;
                end

                // --------------------------------------------------------
                // IO (0x9) — stripped → ILLEGAL_OP
                // --------------------------------------------------------
                else if (fam == FAM_IO) begin
                    ext_active <= 1'b0;
                    R[spsel] <= R[spsel] - 64'd8;
                    effective_addr <= R[spsel] - 64'd8;
                    trap_return_pc <= R[psel];
                    mem_data <= {56'd0, flags}; flags[6] <= 1'b0;
                    ivec_id  <= IRQX_ILLEGAL_OP;
                    post_action <= POST_IRQ_VEC;
                    bus_size <= BUS_DWORD; cpu_state <= CPU_MEM_WRITE;
                end

                // --------------------------------------------------------
                // SEP (0xA) — kept on micro-core
                // --------------------------------------------------------
                else if (fam == FAM_SEP) begin
                    ext_active <= 1'b0;
                    psel <= nib;
                    cpu_state <= CPU_FETCH;
                end

                // --------------------------------------------------------
                // SEX (0xB) — kept on micro-core
                // --------------------------------------------------------
                else if (fam == FAM_SEX) begin
                    ext_active <= 1'b0;
                    xsel <= nib;
                    cpu_state <= CPU_FETCH;
                end

                // --------------------------------------------------------
                // MULDIV (0xC) — MUL/DIV via cluster; bitfield Tier 1 local
                // --------------------------------------------------------
                else if (fam == FAM_MULDIV) begin
                    ext_active <= 1'b0;
                    dst_reg <= ibuf[1][7:4];
                    src_reg <= ibuf[1][3:0];
                    if (nib <= 4'h7) begin
                        // MUL/DIV sub-ops 0–7 → cluster shared unit
                        if (nib >= 4'h4 && R[ibuf[1][3:0]] == 64'd0) begin
                            ivec_id   <= IRQX_DIV_ZERO;
                            cpu_state <= CPU_IRQ;
                        end else begin
                            mul_req <= 1'b1;
                            mul_op  <= nib;
                            mul_a   <= R[ibuf[1][7:4]];
                            mul_b   <= R[ibuf[1][3:0]];
                            cpu_state <= CPU_MULDIV;
                        end
                    end else if (nib <= 4'hB) begin
                        // Tier 1 bitfield (0x8–0xB): POPCNT/CLZ/CTZ/BITREV — local
                        bf_op <= nib[2:0];   // 0x8→0, 0x9→1, 0xA→2, 0xB→3
                        bf_a  <= R[ibuf[1][7:4]];
                        bf_b  <= R[ibuf[1][3:0]];
                        bf_active <= 1'b1;
                        cpu_state <= CPU_EXECUTE;
                    end else begin
                        // Tier 2 bitfield (0xC–0xF): BEXT/BDEP/RORI/BSWAP → ILLEGAL_OP
                        ivec_id   <= IRQX_ILLEGAL_OP;
                        cpu_state <= CPU_IRQ;
                    end
                end

                // --------------------------------------------------------
                // CSR (0xD) — reduced set
                // --------------------------------------------------------
                else if (fam == FAM_CSR) begin
                    ext_active <= 1'b0;
                    cpu_state  <= CPU_FETCH;
                    if (nib[3]) begin
                        // CSRW
                        case (ibuf[1])
                            CSR_TACC_STATUS: ;  // read-only
                            CSR_TACC_CTL: begin
                                if (cl_priv_level && R[nib[2:0]][0]) begin
                                    // FORCE_RELEASE is the narrow enforced
                                    // privilege check.  Trap before emitting
                                    // a control request and notify the
                                    // cluster so its shared privilege source
                                    // transitions at the same trap boundary.
                                    tacc_ctl_valid <= 1'b0;
                                    tacc_priv_fault <= 1'b1;
                                    R[spsel] <= R[spsel] - 64'd8;
                                    effective_addr <= R[spsel] - 64'd8;
                                    trap_return_pc <=
                                        R[psel] + {60'd0, ibuf_need};
                                    mem_data <= {56'd0, flags};
                                    flags[6] <= 1'b0;
                                    ivec_id <= IRQX_PRIV;
                                    post_action <= POST_IRQ_VEC;
                                    bus_size <= BUS_DWORD;
                                    cpu_state <= CPU_MEM_WRITE;
                                end else begin
                                    tacc_ctl_valid <= 1'b1;
                                    tacc_ctl_wdata <= R[nib[2:0]];
                                    cpu_state <= CPU_CSR_WAIT;
                                end
                            end
                            CSR_FLAGS:    flags    <= R[nib[2:0]][7:0];
                            CSR_PSEL:     psel     <= R[nib[2:0]][3:0];
                            CSR_XSEL:     xsel     <= R[nib[2:0]][3:0];
                            CSR_SPSEL:    spsel    <= R[nib[2:0]][3:0];
                            CSR_IE:       flags[6] <= R[nib[2:0]][0];
                            CSR_IVEC_ID:  ivec_id  <= R[nib[2:0]][7:0];
                            CSR_PERF_CTRL: begin
                                perf_enable <= R[nib[2:0]][0];
                                if (R[nib[2:0]][1]) begin
                                    perf_cycles <= 64'd0;
                                    perf_stalls <= 64'd0;
                                    perf_tileops <= 64'd0;
                                end
                            end
                            // D/Q/T CSRs: silently ignored (stripped)
                            CSR_D, CSR_DF, CSR_QREG, CSR_TREG: ;
                            // Caller-private tile configuration remains an
                            // immediate write pulse.
                            CSR_TMODE, CSR_TCTRL, CSR_TSRC0, CSR_TSRC1, CSR_TDST,
                            CSR_SB, CSR_SR, CSR_SC, CSR_SW,
                            CSR_TSTRIDE_R, CSR_TSTRIDE_C, CSR_TTILE_H, CSR_TTILE_W: begin
                                tile_csr_wen   <= 1'b1;
                                tile_csr_wdata <= R[nib[2:0]];
                            end
                            // The accumulator is shared engine state.  Hold
                            // the complete transaction until the cluster
                            // acknowledges ownership and completion.
                            CSR_ACC0, CSR_ACC1, CSR_ACC2, CSR_ACC3,
                            CSR_SHA_MODE, CSR_SHA_MSGLEN,
                            CSR_SHA_MSGLEN_HI: begin
                                tile_csr_req       <= 1'b1;
                                tile_csr_write     <= 1'b1;
                                tile_csr_wen       <= 1'b1;
                                tile_csr_addr_hold <= ibuf[1];
                                tile_csr_wdata     <= R[nib[2:0]];
                                cpu_state          <= CPU_TILE_CSR_WAIT;
                            end
                            // Cluster CSRs: forward to cluster controller
                            CSR_BIST_CMD, CSR_BIST_STATUS,
                            CSR_BIST_FAIL_ADDR, CSR_BIST_FAIL_DATA,
                            CSR_CL_PRIV, CSR_CL_MPU_BASE, CSR_CL_MPU_LIMIT,
                            CSR_CL_IVTBASE,
                            CSR_BARRIER_ARRIVE, CSR_BARRIER_STATUS: begin
                                cl_csr_wen   <= 1'b1;
                                cl_csr_wdata <= R[nib[2:0]];
                            end
                            // Shared CRC state is mutated only through the
                            // arbitrated ISA transaction. Raw CSR writes are
                            // deliberately ignored on micro-cores.
                            CSR_CRC_ACC, CSR_CRC_MODE: ;
                            // IVT base is cluster-shared — write goes to cluster
                            CSR_IVTBASE: begin
                                cl_csr_wen   <= 1'b1;
                                cl_csr_wdata <= R[nib[2:0]];
                            end
                            default: ;
                        endcase
                    end else begin
                        // CSRR
                        case (ibuf[1])
                            CSR_TACC_STATUS: R[nib[2:0]] <= tacc_status;
                            CSR_TACC_CTL:    R[nib[2:0]] <= 64'd0;
                            CSR_FLAGS:       R[nib[2:0]] <= {56'd0, flags};
                            CSR_PSEL:        R[nib[2:0]] <= {60'd0, psel};
                            CSR_XSEL:        R[nib[2:0]] <= {60'd0, xsel};
                            CSR_SPSEL:       R[nib[2:0]] <= {60'd0, spsel};
                            CSR_IVTBASE:     R[nib[2:0]] <= cl_ivt_base;
                            CSR_D:           R[nib[2:0]] <= 64'd0;
                            CSR_DF:          R[nib[2:0]] <= 64'd0;
                            CSR_QREG:        R[nib[2:0]] <= 64'd0;
                            CSR_TREG:        R[nib[2:0]] <= 64'd0;
                            CSR_IE:          R[nib[2:0]] <= {63'd0, flags[6]};
                            CSR_PRIV:        R[nib[2:0]] <= {63'd0, cl_priv_level};
                            CSR_COREID:      R[nib[2:0]] <= {{(64-MP64_CORE_ID_BITS){1'b0}}, core_id};
                            CSR_NCORES:      R[nib[2:0]] <= 64'd16;
                            CSR_IVEC_ID:     R[nib[2:0]] <= {56'd0, ivec_id};
                            CSR_TRAP_ADDR:   R[nib[2:0]] <= trap_addr;
                            CSR_MEGAPAD_SZ:  R[nib[2:0]] <= 64'd0;
                            CSR_CPUID:       R[nib[2:0]] <= 64'h4D50_3634_0001_4D43; // "MP64" v1 "MC"
                            CSR_PERF_CYCLES: R[nib[2:0]] <= perf_cycles;
                            CSR_PERF_STALLS: R[nib[2:0]] <= perf_stalls;
                            CSR_PERF_TILEOPS:R[nib[2:0]] <= perf_tileops;
                            CSR_PERF_CTRL:   R[nib[2:0]] <= {63'd0, perf_enable};
                            // Caller-private tile CSR reads remain immediate.
                            CSR_TMODE, CSR_TCTRL, CSR_TSRC0, CSR_TSRC1, CSR_TDST,
                            CSR_SB, CSR_SR, CSR_SC, CSR_SW,
                            CSR_TSTRIDE_R, CSR_TSTRIDE_C, CSR_TTILE_H, CSR_TTILE_W:
                                R[nib[2:0]] <= tile_csr_rdata;
                            // Shared ACC reads use the same acknowledged path
                            // as writes and capture the architectural GPR
                            // destination before entering the wait state.
                            CSR_ACC0, CSR_ACC1, CSR_ACC2, CSR_ACC3,
                            CSR_SHA_MODE, CSR_SHA_MSGLEN,
                            CSR_SHA_MSGLEN_HI: begin
                                tile_csr_req       <= 1'b1;
                                tile_csr_write     <= 1'b0;
                                tile_csr_wen       <= 1'b0;
                                tile_csr_addr_hold <= ibuf[1];
                                tile_csr_wdata     <= 64'd0;
                                tile_csr_dst_reg   <= nib[2:0];
                                cpu_state          <= CPU_TILE_CSR_WAIT;
                            end
                            // Cluster CSR reads: forwarded
                            CSR_BIST_CMD, CSR_BIST_STATUS,
                            CSR_BIST_FAIL_ADDR, CSR_BIST_FAIL_DATA,
                            CSR_CL_PRIV, CSR_CL_MPU_BASE, CSR_CL_MPU_LIMIT,
                            CSR_CL_IVTBASE,
                            CSR_BARRIER_ARRIVE, CSR_BARRIER_STATUS:
                                R[nib[2:0]] <= cl_csr_rdata;
                            // Shared CRC state remains a diagnostic snapshot.
                            CSR_CRC_ACC, CSR_CRC_MODE:
                                R[nib[2:0]] <= cl_csr_rdata;
                            default: R[nib[2:0]] <= 64'd0;
                        endcase
                    end
                end

                // --------------------------------------------------------
                // MEX (0xE) — dispatch to cluster-shared tile engine
                // --------------------------------------------------------
                else if (fam == FAM_MEX) begin
                    mex_funct_byte <= ibuf[1];
                    ext_active     <= 1'b0;
                    if (
                        (ibuf[0][1:0] == MEX_TMUL &&
                         ibuf[1][2:0] == TMUL_TAMAC &&
                         (ibuf[0][3:2] == 2'd2 ||
                          ibuf[1][7:3] != 5'd0)) ||
                        (ibuf[0][1:0] == MEX_TMUL &&
                         ibuf[1][2:0] == 3'd7) ||
                        (ext_active && ext_mod == EXT_ETALU &&
                         ibuf[0][1:0] == MEX_TSYS &&
                         (((ibuf[1][2:0] >= 3'd2) &&
                           (ibuf[1][2:0] <= 3'd6) &&
                           (ibuf[0][3:2] != 2'd0 ||
                            ibuf[1][7:3] != 5'd0)) ||
                          ibuf[1][2:0] == 3'd7))
                    ) begin
                        mex_req <= 1'b0;
                        R[spsel] <= R[spsel] - 64'd8;
                        effective_addr <= R[spsel] - 64'd8;
                        trap_return_pc <= R[psel] + {60'd0, ibuf_len};
                        mem_data <= {56'd0, flags};
                        flags[6] <= 1'b0;
                        ivec_id <= IRQX_ILLEGAL_OP;
                        post_action <= POST_IRQ_VEC;
                        bus_size <= BUS_DWORD;
                        cpu_state <= CPU_MEM_WRITE;
                    end else begin
                        mex_req        <= 1'b1;
                        mex_ss         <= ibuf[0][3:2];
                        mex_op         <= ibuf[0][1:0];
                        mex_funct      <= ibuf[1][2:0];
                        mex_gpr_val    <= (ibuf[0][3:2] == 2'd1) ? R[ibuf[2][3:0]] : 64'd0;
                        mex_imm8       <= ibuf[2];
                        mex_ext_mod    <= ext_mod;
                        mex_ext_active <= ext_active;
                        cpu_state      <= CPU_MEX_WAIT;
                    end
                end

                // --------------------------------------------------------
                // Unknown — skip
                // --------------------------------------------------------
                else begin
                    ext_active <= 1'b0;
                    cpu_state  <= CPU_FETCH;
                end
            end

            // ============================================================
            // EXECUTE: ALU / Bitfield writeback
            // ============================================================
            CPU_EXECUTE: begin
                if (bf_active) begin
                    R[dst_reg] <= bf_result;
                    flags[0]   <= bf_flag_z;   // Z
                    flags[2]   <= bf_flag_n;   // N
                    bf_active  <= 1'b0;
                end else begin
                    if (alu_op != ALU_CMP)
                        R[dst_reg] <= alu_result;
                    flags <= alu_flags_out;
                end
                cpu_state <= CPU_FETCH;
            end

            // ============================================================
            // MEM_READ
            // ============================================================
            CPU_MEM_READ: begin
                bus_addr <= effective_addr;
                bus_wen  <= 1'b0;
                if (bus_ready && mpu_fault) begin
                    bus_valid <= 1'b0;
                    trap_addr <= effective_addr;
                    R[spsel] <= R[spsel] - 64'd8;
                    effective_addr <= R[spsel] - 64'd8;
                    trap_return_pc <= R[psel];
                    mem_data <= {56'd0, flags}; flags[6] <= 1'b0;
                    ivec_id  <= IRQX_PRIV;
                    post_action <= POST_IRQ_VEC;
                    bus_size <= BUS_DWORD; cpu_state <= CPU_MEM_WRITE;
                end else if (bus_ready) begin
                    bus_valid <= 1'b0;
                    case (mem_sub)
                        4'hC: R[dst_reg] <= {{56{bus_rdata[7]}}, bus_rdata[7:0]};
                        4'hD: R[dst_reg] <= {{48{bus_rdata[15]}}, bus_rdata[15:0]};
                        4'hE: R[dst_reg] <= {{32{bus_rdata[31]}}, bus_rdata[31:0]};
                        default: R[dst_reg] <= bus_rdata;
                    endcase

                    if (mem_sub == 4'h1) R[src_reg] <= R[src_reg] + 64'd8;
                    if (mem_sub == 4'h3) R[xsel]    <= R[xsel]    + 64'd8;

                    if (post_action == POST_RTI_POP2) begin
                        effective_addr <= R[spsel];
                        R[spsel]    <= R[spsel] + 64'd8;
                        bus_size    <= BUS_DWORD;
                        post_action <= POST_NONE;
                        cpu_state   <= CPU_MEM_READ2;
                    end else
                        cpu_state <= CPU_FETCH;
                end else begin
                    bus_valid <= 1'b1;
                end
            end

            // ============================================================
            // MEM_READ2: RTI flags pop
            // ============================================================
            CPU_MEM_READ2: begin
                bus_addr <= effective_addr;
                bus_wen  <= 1'b0;
                if (bus_ready) begin
                    bus_valid <= 1'b0;
                    flags <= bus_rdata[7:0];
                    // Note: priv_level restored at cluster level via trap return
                    cpu_state <= CPU_FETCH;
                end else begin
                    bus_valid <= 1'b1;
                end
            end

            // ============================================================
            // MEM_WRITE
            // ============================================================
            CPU_MEM_WRITE: begin
                bus_addr  <= effective_addr;
                bus_wdata <= mem_data;
                bus_wen   <= 1'b1;
                if (bus_ready && mpu_fault) begin
                    bus_valid <= 1'b0;
                    trap_addr <= effective_addr;
                    R[spsel] <= R[spsel] - 64'd8;
                    effective_addr <= R[spsel] - 64'd8;
                    trap_return_pc <= R[psel];
                    mem_data <= {56'd0, flags}; flags[6] <= 1'b0;
                    ivec_id  <= IRQX_PRIV;
                    post_action <= POST_IRQ_VEC;
                    bus_size <= BUS_DWORD;
                    cpu_state <= CPU_MEM_WRITE;
                end else if (bus_ready) begin
                    bus_valid <= 1'b0;
                    if (mem_sub == 4'h5)
                        R[xsel] <= R[xsel] - 64'd8;

                    if (post_action == POST_IRQ_VEC) begin
                        R[spsel] <= R[spsel] - 64'd8;
                        effective_addr <= R[spsel] - 64'd8;
                        mem_data <= trap_return_pc;
                        post_action <= POST_NONE;
                        cpu_state <= CPU_IRQ_PUSH;
                    end else
                        cpu_state <= CPU_FETCH;
                end else begin
                    bus_valid <= 1'b1;
                end
            end

            // ============================================================
            // IRQ_PUSH: push return PC, then load IVT vector
            // ============================================================
            CPU_IRQ_PUSH: begin
                bus_addr  <= effective_addr;
                bus_wdata <= mem_data;
                bus_wen   <= 1'b1;
                bus_size  <= BUS_DWORD;
                if (bus_ready) begin
                    bus_valid <= 1'b0;
                    effective_addr <= cl_ivt_base + {56'd0, ivec_id, 3'b000};
                    bus_size <= BUS_DWORD;
                    cpu_state <= CPU_IRQ_LOAD;
                end else begin
                    bus_valid <= 1'b1;
                end
            end

            // ============================================================
            // IRQ_LOAD: read IVT vector, jump
            // ============================================================
            CPU_IRQ_LOAD: begin
                bus_addr <= effective_addr;
                bus_wen  <= 1'b0;
                bus_size <= BUS_DWORD;
                if (bus_ready) begin
                    bus_valid <= 1'b0;
                    R[psel] <= bus_rdata;
                    cpu_state <= CPU_FETCH;
                end else begin
                    bus_valid <= 1'b1;
                end
            end

            // ============================================================
            // MEX_WAIT: wait for shared cluster tile engine
            // ============================================================
            CPU_MEX_WAIT: begin
                if (mex_done) begin
                    mex_req <= 1'b0;
                    if (mex_fault == MEX_FAULT_NONE) begin
                        cpu_state <= CPU_FETCH;
                    end else begin
                        if (mex_fault == MEX_FAULT_ALIGN ||
                            mex_fault == MEX_FAULT_BUS ||
                            mex_fault == MEX_FAULT_PRIV)
                            trap_addr <= mex_fault_addr;
                        R[spsel] <= R[spsel] - 64'd8;
                        effective_addr <= R[spsel] - 64'd8;
                        trap_return_pc <= R[psel];
                        mem_data <= {56'd0, flags};
                        flags[6] <= 1'b0;
                        ivec_id <= mex_fault_vector(mex_fault);
                        post_action <= POST_IRQ_VEC;
                        bus_size <= BUS_DWORD;
                        cpu_state <= CPU_MEM_WRITE;
                    end
                end else begin
                    // Arbitration and service backpressure must not turn a
                    // single architectural request into a pulse.
                    mex_req <= 1'b1;
                end
            end

            // ============================================================
            // CSR_WAIT: acknowledged TACC control write
            // ============================================================
            CPU_CSR_WAIT: begin
                if (tacc_ctl_done) begin
                    tacc_ctl_valid <= 1'b0;
                    if (tacc_ctl_fault == MEX_FAULT_NONE) begin
                        cpu_state <= CPU_FETCH;
                    end else begin
                        R[spsel] <= R[spsel] - 64'd8;
                        effective_addr <= R[spsel] - 64'd8;
                        trap_return_pc <= R[psel];
                        mem_data <= {56'd0, flags};
                        flags[6] <= 1'b0;
                        ivec_id <= mex_fault_vector(tacc_ctl_fault);
                        post_action <= POST_IRQ_VEC;
                        bus_size <= BUS_DWORD;
                        cpu_state <= CPU_MEM_WRITE;
                    end
                end else begin
                    tacc_ctl_valid <= 1'b1;
                end
            end

            // ============================================================
            // TILE_CSR_WAIT: acknowledged shared accumulator access
            // ============================================================
            CPU_TILE_CSR_WAIT: begin
                if (tile_csr_done) begin
                    tile_csr_req <= 1'b0;
                    tile_csr_wen <= 1'b0;
                    if (!tile_csr_write)
                        R[tile_csr_dst_reg] <= tile_csr_rdata;
                    cpu_state <= CPU_FETCH;
                end else begin
                    tile_csr_req <= 1'b1;
                    tile_csr_wen <= tile_csr_write;
                end
            end

            // ============================================================
            // CRYPTO: wait for shared cluster CRC result
            // ============================================================
            CPU_CRYPTO: begin
                if (crc_done) begin
                    crc_req <= 1'b0;
                    if (crc_rd_we_in)
                        R[dst_reg] <= crc_result;
                    cpu_state <= CPU_FETCH;
                end
            end

            // ============================================================
            // SHA_WAIT: wait for shared cluster SHA-2 result
            // ============================================================
            CPU_SHA_WAIT: begin
                if (sha_done) begin
                    sha_req <= 1'b0;
                    if (sha_rd_we_in)
                        R[dst_reg] <= sha_result;
                    cpu_state <= CPU_FETCH;
                end
            end

            // ============================================================
            // GF_WAIT: wait for field ALU ISA engine
            // ============================================================
            CPU_GF_WAIT: begin
                if (gf_done) begin
                    gf_req <= 1'b0;
                    // ACC writeback handled by cluster (gf_acc_we)
                    // Prime selection writeback
                    // Flag writeback (GF.CEQ → Z flag)
                    if (gf_flag_z_we)
                        flags[0] <= gf_flag_z;
                    cpu_state <= CPU_FETCH;
                end
            end

            // ============================================================
            // MULDIV: wait for shared cluster MUL/DIV result
            // ============================================================
            CPU_MULDIV: begin
                if (mul_done) begin
                    mul_req <= 1'b0;
                    case (mul_op)
                        4'h0, 4'h2: begin // MUL, UMUL (low 64)
                            R[dst_reg] <= mul_result[63:0];
                            flags[0] <= (mul_result[63:0] == 64'd0);
                            flags[2] <= mul_result[63];
                        end
                        4'h1, 4'h3: begin // MULH, UMULH (high 64)
                            R[dst_reg] <= mul_result[127:64];
                            flags[0] <= (mul_result[127:64] == 64'd0);
                            flags[2] <= mul_result[127];
                        end
                        4'h4, 4'h5: begin // DIV, UDIV
                            R[dst_reg] <= mul_result[63:0];   // quotient
                            R[0]       <= mul_result[127:64]; // remainder
                            flags[0] <= (mul_result[63:0] == 64'd0);
                            flags[2] <= mul_result[63];
                        end
                        4'h6, 4'h7: begin // MOD, UMOD
                            R[dst_reg] <= mul_result[127:64]; // remainder
                            flags[0] <= (mul_result[127:64] == 64'd0);
                            flags[2] <= mul_result[127];
                        end
                        default: ;
                    endcase
                    cpu_state <= CPU_FETCH;
                end
            end

            // ============================================================
            // IRQ: save FLAGS first; IRQ_PUSH saves the return PC
            // ============================================================
            CPU_IRQ: begin
                R[spsel] <= R[spsel] - 64'd8;
                effective_addr <= R[spsel] - 64'd8;
                trap_return_pc <= R[psel];
                mem_data <= {56'd0, flags};
                bus_size <= BUS_DWORD;
                flags[6] <= 1'b0;
                post_action <= POST_IRQ_VEC;
                cpu_state <= CPU_MEM_WRITE;
            end

            // ============================================================
            // HALT: wait for interrupt
            // ============================================================
            CPU_HALT: begin
                if (irq_pending) begin
                    ivec_id  <= {4'd0, irq_vector};
                    cpu_state <= CPU_IRQ;
                end
            end

            // ============================================================
            // SKIP: inspect the skipped instruction. EXT.CRYPTO needs its
            // sub-op byte, and a redundant REX prefix contributes one byte.
            // ============================================================
            CPU_SKIP: begin
                if (!skip_fetch_pending) begin
                    bus_valid <= 1'b1;
                    bus_addr  <= R[psel];
                    bus_wen   <= 1'b0;
                    bus_size  <= BUS_BYTE;
                    skip_fetch_pending <= 1'b1;
                end else if (!bus_ready) begin
                    bus_valid <= 1'b1;
                end

                if (bus_ready && skip_fetch_pending) begin
                    skip_fetch_pending <= 1'b0;
                    if (bus_rdata[7:0] >= 8'hF1 &&
                        bus_rdata[7:0] <= 8'hF5) begin
                        skip_has_rex <= 1'b1;
                        cpu_state <= CPU_SKIP_REX;
                    end else if (bus_rdata[7:0] == 8'hFB) begin
                        skip_has_rex <= 1'b0;
                        cpu_state <= CPU_SKIP_CRYPTO;
                    end else begin
                        R[psel] <= R[psel]
                                   + {60'd0,
                                      instr_len(bus_rdata[7:0], 1'b0)};
                        cpu_state <= CPU_FETCH;
                    end
                end
            end

            CPU_SKIP_REX: begin
                if (!skip_fetch_pending) begin
                    bus_valid <= 1'b1;
                    bus_addr  <= R[psel] + 64'd1;
                    bus_wen   <= 1'b0;
                    bus_size  <= BUS_BYTE;
                    skip_fetch_pending <= 1'b1;
                end else if (!bus_ready) begin
                    bus_valid <= 1'b1;
                end

                if (bus_ready && skip_fetch_pending) begin
                    skip_fetch_pending <= 1'b0;
                    if (bus_rdata[7:0] == 8'hFB) begin
                        cpu_state <= CPU_SKIP_CRYPTO;
                    end else begin
                        R[psel] <= R[psel] + 64'd1
                                   + {60'd0,
                                      instr_len(bus_rdata[7:0], 1'b1)};
                        cpu_state <= CPU_FETCH;
                    end
                end
            end

            CPU_SKIP_CRYPTO: begin
                if (!skip_fetch_pending) begin
                    bus_valid <= 1'b1;
                    bus_addr  <= R[psel]
                                 + (skip_has_rex ? 64'd2 : 64'd1);
                    bus_wen   <= 1'b0;
                    bus_size  <= BUS_BYTE;
                    skip_fetch_pending <= 1'b1;
                end else if (!bus_ready) begin
                    bus_valid <= 1'b1;
                end

                if (bus_ready && skip_fetch_pending) begin
                    skip_fetch_pending <= 1'b0;
                    R[psel] <= R[psel]
                               + (skip_has_rex ? 64'd1 : 64'd0)
                               + (crypto_is_bare(bus_rdata[7:0])
                                  ? 64'd2 : 64'd3);
                    cpu_state <= CPU_FETCH;
                end
            end

            endcase
        end
    end

endmodule
