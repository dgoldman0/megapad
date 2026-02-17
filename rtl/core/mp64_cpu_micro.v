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
//   REMOVED — Tile/MEX engine (FAM_MEX → ILLEGAL_OP trap)
//   REMOVED — 1802-heritage D, Q, T registers
//   REMOVED — Family 0x8 MEMALU (D-register ops) → ILLEGAL_OP
//   REMOVED — Family 0x9 IO (port input/output) → ILLEGAL_OP
//   REMOVED — SYS sub-ops: RET/DIS/MARK/SAV/SEQ/REQ → ILLEGAL_OP
//   REMOVED — IMM sub-ops: GLO/GHI/PLO/PHI → ILLEGAL_OP
//   REMOVED — Per-core privilege/MPU (cluster-shared)
//   REMOVED — Per-core BIST (cluster controller handles this)
//   REMOVED — DMA ring CSRs, I-cache CSRs, tile self-test
//   SHARED  — MUL/DIV via cluster (always)
//   SHARED  — IVT base (cluster-level, input wire)
//   SHARED  — Privilege level + MPU (cluster-level)
//   KEPT    — SEP (0xA) and SEX (0xB) — zero-cost, avoids ISA fragmentation
//   REDUCED — Performance counters: cycles only
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

    // === Cluster CSR interface (to cluster controller) ===
    // Used for: BIST, barrier, cluster priv/MPU/IVT
    output reg  [7:0]  cl_csr_addr,
    output reg         cl_csr_wen,
    output reg  [63:0] cl_csr_wdata,
    input  wire [63:0] cl_csr_rdata,

    // === Cluster-shared state (inputs from cluster) ===
    input  wire [63:0] cl_ivt_base,
    input  wire        cl_priv_level
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

    // Performance counter (cycles only)
    reg [63:0] perf_cycles;
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

    wire [3:0] fam = ibuf[0][7:4];
    wire [3:0] nib = ibuf[0][3:0];

    // ====================================================================
    // CPU FSM
    // ====================================================================
    reg [3:0]  cpu_state;

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

    // Combinational CSR address — cluster muxes rdata on this
    always @(*) cl_csr_addr = ibuf[1];

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
            ext_active    <= 1'b0;
            ext_mod       <= 4'd0;
            fetch_pending <= 1'b0;
            ibuf_len      <= 4'd0;
            ibuf_need     <= 4'd1;

            psel  <= 4'd3;
            xsel  <= 4'd2;
            spsel <= 4'd15;
            flags <= 8'h40;           // I=1

            ivec_id   <= 8'd0;
            trap_addr <= 64'd0;

            post_action <= POST_NONE;
            mem_sub     <= 4'd0;

            alu_op <= 4'd0;
            alu_a  <= 64'd0;
            alu_b  <= 64'd0;

            mul_req <= 1'b0;
            mul_op  <= 4'd0;
            mul_a   <= 64'd0;
            mul_b   <= 64'd0;

            cl_csr_wen   <= 1'b0;
            cl_csr_wdata <= 64'd0;

            perf_cycles <= 64'd0;
            perf_enable <= 1'b1;

            R[0]  <= 64'd0; R[1]  <= 64'd0; R[2]  <= 64'd0; R[3]  <= 64'd0;
            R[4]  <= 64'd0; R[5]  <= 64'd0; R[6]  <= 64'd0; R[7]  <= 64'd0;
            R[8]  <= 64'd0; R[9]  <= 64'd0; R[10] <= 64'd0; R[11] <= 64'd0;
            R[12] <= 64'd0; R[13] <= 64'd0; R[14] <= 64'd0; R[15] <= 64'd0;

        end else begin
            bus_valid  <= 1'b0;
            cl_csr_wen <= 1'b0;

            if (perf_enable)
                perf_cycles <= perf_cycles + 64'd1;

            case (cpu_state)

            // ============================================================
            // FETCH: check for pending interrupts
            // ============================================================
            CPU_FETCH: begin
                if (irq_pending && ibuf_len == 4'd0) begin
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

                // --------------------------------------------------------
                // EXT prefix (0xF)
                // --------------------------------------------------------
                if (fam == FAM_EXT) begin
                    ext_active <= 1'b1;
                    ext_mod    <= nib;
                    cpu_state  <= CPU_FETCH;
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
                            mem_data <= R[psel];
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
                            mem_data <= R[psel] + {60'd0, ibuf_len};
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
                                mem_data <= R[psel];
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
                    mem_data <= R[psel]; flags[6] <= 1'b0;
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
                    mem_data <= R[psel]; flags[6] <= 1'b0;
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
                // MULDIV (0xC) — always via cluster shared unit
                // --------------------------------------------------------
                else if (fam == FAM_MULDIV) begin
                    ext_active <= 1'b0;
                    dst_reg <= ibuf[1][7:4];
                    src_reg <= ibuf[1][3:0];
                    // Divide-by-zero check for DIV/UDIV/MOD/UMOD
                    if (nib >= 4'h4 && R[ibuf[1][3:0]] == 64'd0) begin
                        ivec_id   <= IRQX_ILLEGAL_OP;
                        cpu_state <= CPU_IRQ;
                    end else begin
                        mul_req <= 1'b1;
                        mul_op  <= nib;
                        mul_a   <= R[ibuf[1][7:4]];
                        mul_b   <= R[ibuf[1][3:0]];
                        cpu_state <= CPU_MULDIV;
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
                            CSR_FLAGS:    flags    <= R[nib[2:0]][7:0];
                            CSR_PSEL:     psel     <= R[nib[2:0]][3:0];
                            CSR_XSEL:     xsel     <= R[nib[2:0]][3:0];
                            CSR_SPSEL:    spsel    <= R[nib[2:0]][3:0];
                            CSR_IE:       flags[6] <= R[nib[2:0]][0];
                            CSR_IVEC_ID:  ivec_id  <= R[nib[2:0]][7:0];
                            CSR_PERF_CTRL: begin
                                perf_enable <= R[nib[2:0]][0];
                                if (R[nib[2:0]][1])
                                    perf_cycles <= 64'd0;
                            end
                            // D/Q/T CSRs: silently ignored (stripped)
                            CSR_D, CSR_DF, CSR_QREG, CSR_TREG: ;
                            // Cluster CSRs: forward to cluster controller
                            CSR_BIST_CMD, CSR_BIST_STATUS,
                            CSR_BIST_FAIL_ADDR, CSR_BIST_FAIL_DATA,
                            CSR_CL_PRIV, CSR_CL_MPU_BASE, CSR_CL_MPU_LIMIT,
                            CSR_CL_IVTBASE,
                            CSR_BARRIER_ARRIVE, CSR_BARRIER_STATUS: begin
                                cl_csr_wen   <= 1'b1;
                                cl_csr_wdata <= R[nib[2:0]];
                            end
                            // IVT base is cluster-shared — write goes to cluster
                            // cl_csr_addr is ibuf[1]=CSR_IVTBASE; cluster maps it
                            CSR_IVTBASE: begin
                                cl_csr_wen   <= 1'b1;
                                cl_csr_wdata <= R[nib[2:0]];
                            end
                            default: ;
                        endcase
                    end else begin
                        // CSRR
                        case (ibuf[1])
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
                            CSR_PERF_CTRL:   R[nib[2:0]] <= {63'd0, perf_enable};
                            // Cluster CSR reads: forwarded
                            CSR_BIST_CMD, CSR_BIST_STATUS,
                            CSR_BIST_FAIL_ADDR, CSR_BIST_FAIL_DATA,
                            CSR_CL_PRIV, CSR_CL_MPU_BASE, CSR_CL_MPU_LIMIT,
                            CSR_CL_IVTBASE,
                            CSR_BARRIER_ARRIVE, CSR_BARRIER_STATUS:
                                R[nib[2:0]] <= cl_csr_rdata;
                            default: R[nib[2:0]] <= 64'd0;
                        endcase
                    end
                end

                // --------------------------------------------------------
                // MEX (0xE) — not on micro-core → ILLEGAL_OP
                // --------------------------------------------------------
                else if (fam == FAM_MEX) begin
                    ext_active <= 1'b0;
                    R[spsel] <= R[spsel] - 64'd8;
                    effective_addr <= R[spsel] - 64'd8;
                    mem_data <= R[psel]; flags[6] <= 1'b0;
                    ivec_id  <= IRQX_ILLEGAL_OP;
                    post_action <= POST_IRQ_VEC;
                    bus_size <= BUS_DWORD; cpu_state <= CPU_MEM_WRITE;
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
            // EXECUTE: ALU writeback
            // ============================================================
            CPU_EXECUTE: begin
                if (alu_op != ALU_CMP)
                    R[dst_reg] <= alu_result;
                flags <= alu_flags_out;
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
                    mem_data <= R[psel]; flags[6] <= 1'b0;
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
                    mem_data <= R[psel]; flags[6] <= 1'b0;
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
                        mem_data <= {56'd0, flags};
                        post_action <= POST_NONE;
                        cpu_state <= CPU_IRQ_PUSH;
                    end else
                        cpu_state <= CPU_FETCH;
                end else begin
                    bus_valid <= 1'b1;
                end
            end

            // ============================================================
            // IRQ_PUSH: push flags, then load IVT vector
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
            // IRQ: vectored interrupt entry
            // ============================================================
            CPU_IRQ: begin
                R[spsel] <= R[spsel] - 64'd8;
                effective_addr <= R[spsel] - 64'd8;
                mem_data <= R[psel];
                bus_size <= BUS_DWORD;
                flags[6] <= 1'b0;
                ivec_id  <= {4'd0, irq_vector};
                post_action <= POST_IRQ_VEC;
                cpu_state <= CPU_MEM_WRITE;
            end

            // ============================================================
            // HALT: wait for interrupt
            // ============================================================
            CPU_HALT: begin
                if (irq_pending)
                    cpu_state <= CPU_IRQ;
            end

            // ============================================================
            // SKIP: advance PC past the next instruction
            // ============================================================
            CPU_SKIP: begin
                bus_valid <= 1'b1;
                bus_addr  <= R[psel];
                bus_wen   <= 1'b0;
                bus_size  <= BUS_BYTE;
                if (bus_ready) begin
                    R[psel] <= R[psel] + {60'd0, instr_len(bus_rdata[7:0], 1'b0)};
                    cpu_state <= CPU_FETCH;
                end
            end

            endcase
        end
    end

endmodule
