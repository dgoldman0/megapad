// ============================================================================
// mp64_cpu_micro.v — Megapad-64 Micro-Core CPU
// ============================================================================
//
// Stripped-down CPU core for packing into micro-core clusters.
// ISA-compatible with the major core for the base instruction set;
// unsupported families (MULDIV, MEX) trap as illegal opcodes.
//
// Compared to the major core (mp64_cpu.v / mp64_cpu_fsm.v):
//   REMOVED — I-cache interface (fetches byte-by-byte from bus)
//   REMOVED — Tile/MEX engine ports and FAM_MEX dispatch
//   REMOVED — Hardware 64×64→128 multiplier (FAM_MULDIV traps)
//   REMOVED — Memory BIST controller
//   REMOVED — Tile datapath self-test
//   REMOVED — DMA ring-buffer CSRs
//   REMOVED — I-cache CSRs
//   REMOVED — QoS CSRs (managed by cluster controller)
//   REDUCED — Performance counters: 1 (cycles only)
//
// Area budget (Kintex-7 estimates):
//   ~1,800 FFs / ~1,100 LUTs / 0 DSP48
//   vs major core: ~2,450 FFs / ~1,300 LUTs / 16 DSP48
//
// The micro-core is designed to be a slave compute element:
//   - Receives work via IPI from its cluster controller
//   - Executes base ISA (integer, branch, load/store, MEMALU, IO, CSR)
//   - Returns results via shared memory or mailbox
//   - Does NOT independently access peripheral MMIO (cluster mediates)
//
// Bus interface is identical to the major core (valid/ready handshake)
// but at the SoC level micro-cores connect through a cluster arbiter,
// NOT directly to the main bus.  Cluster integration is separate.
//

`include "mp64_defs.vh"

module mp64_cpu_micro (
    input  wire        clk,
    input  wire        rst_n,

    // === Core identification (set per instance by cluster) ===
    input  wire [CORE_ID_BITS-1:0] core_id,

    // === Memory bus master ===
    output reg         bus_valid,
    output reg  [63:0] bus_addr,
    output reg  [63:0] bus_wdata,
    output reg         bus_wen,
    output reg  [1:0]  bus_size,
    input  wire [63:0] bus_rdata,
    input  wire        bus_ready,

    // === Interrupts ===
    input  wire        irq_timer,
    input  wire        irq_uart,
    input  wire        irq_nic,
    input  wire        irq_ipi,

    // === External flags (EF1-EF4) ===
    input  wire [3:0]  ef_flags
);

    // ========================================================================
    // Shared constants and functions
    // ========================================================================
    `include "mp64_cpu_common.vh"

    // ========================================================================
    // Register file
    // ========================================================================
    reg [63:0] R [0:15];

    // Selector CSRs
    reg [3:0]  psel;       // PC register  (default R3)
    reg [3:0]  xsel;       // X register   (default R2)
    reg [3:0]  spsel;      // SP register  (default R15)

    // Flags: [S:7  I:6  G:5  P:4  V:3  N:2  C:1  Z:0]
    reg [7:0]  flags;

    wire       flag_z = flags[0];
    wire       flag_c = flags[1];
    wire       flag_n = flags[2];
    wire       flag_v = flags[3];
    wire       flag_p = flags[4];
    wire       flag_g = flags[5];
    wire       flag_i = flags[6];
    wire       flag_s = flags[7];

    // Convenience aliases
    wire [63:0] PC = R[psel];
    wire [63:0] SP = R[spsel];
    wire [63:0] RX = R[xsel];

    // IVT base address (CSR)
    reg [63:0] ivt_base;

    // 1802-heritage special registers
    reg [7:0]  D;          // 8-bit D accumulator
    reg        Q;          // Q flip-flop
    reg [7:0]  T;          // T register (saved XSEL|PSEL on MARK)

    // Trap / interrupt context
    reg [7:0]  ivec_id;
    reg [63:0] trap_addr;

    // Performance counter (cycles only — micro-core minimal)
    reg [63:0] perf_cycles;
    reg        perf_enable;

    // EXT prefix modifier
    reg [3:0]  ext_mod;
    reg        ext_active;

    // Fetch handshake
    reg        fetch_pending;

    // ========================================================================
    // Instruction fetch buffer
    // ========================================================================
    reg [7:0]  ibuf [0:10];    // up to 11 bytes (EXT + LDI imm64)
    reg [3:0]  ibuf_len;
    reg [3:0]  ibuf_need;

    // Decoded instruction fields
    wire [3:0] fam  = ibuf[0][7:4];
    wire [3:0] nib  = ibuf[0][3:0];

    // ========================================================================
    // CPU state machine
    // ========================================================================
    reg [3:0]  cpu_state;

    // ========================================================================
    // ALU instance (shared combinational module)
    // ========================================================================
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

    // ========================================================================
    // Interrupt pending logic
    // ========================================================================
    reg        irq_pending;
    reg [3:0]  irq_vector;

    always @(*) begin
        irq_pending = 1'b0;
        irq_vector  = 4'd0;
        if (flag_i) begin
            // Priority: IPI > Timer > UART > NIC
            if (irq_ipi) begin
                irq_pending = 1'b1;
                irq_vector  = IRQX_IPI;
            end else if (irq_timer) begin
                irq_pending = 1'b1;
                irq_vector  = {1'b0, IRQ_TIMER};
            end else if (irq_uart) begin
                irq_pending = 1'b1;
                irq_vector  = IRQX_UART;
            end else if (irq_nic) begin
                irq_pending = 1'b1;
                irq_vector  = IRQX_NIC;
            end
        end
    end

    // ========================================================================
    // Temporary registers for multi-cycle operations
    // ========================================================================
    reg [63:0] mem_data;
    reg [3:0]  dst_reg;
    reg [3:0]  src_reg;
    reg [63:0] effective_addr;
    reg [3:0]  mem_sub;

    // MEMALU temp
    reg [7:0]  memalu_byte;
    reg [3:0]  memalu_sub;

    // IO temp
    reg [2:0]  io_port;
    reg        io_is_inp;

    reg [2:0]  post_action;

    // ========================================================================
    // Main state machine
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            // Reset state
            cpu_state     <= CPU_FETCH;
            bus_valid     <= 1'b0;
            ext_active    <= 1'b0;
            ext_mod       <= 4'd0;
            fetch_pending <= 1'b0;
            ibuf_len      <= 4'd0;
            ibuf_need     <= 4'd1;

            // Default register setup
            psel  <= 4'd3;
            xsel  <= 4'd2;
            spsel <= 4'd15;
            flags <= 8'h40;    // I=1

            ivt_base  <= 64'd0;
            ivec_id   <= 8'd0;
            trap_addr <= 64'd0;
            D         <= 8'd0;
            Q         <= 1'b0;
            T         <= 8'd0;

            post_action <= POST_NONE;
            mem_sub     <= 4'd0;
            memalu_sub  <= 4'd0;
            memalu_byte <= 8'd0;
            io_port     <= 3'd0;
            io_is_inp   <= 1'b0;

            alu_op <= 4'd0;
            alu_a  <= 64'd0;
            alu_b  <= 64'd0;

            // Performance counter
            perf_cycles <= 64'd0;
            perf_enable <= 1'b1;

            // Clear register file
            R[0]  <= 64'd0;  R[1]  <= 64'd0;  R[2]  <= 64'd0;  R[3]  <= 64'd0;
            R[4]  <= 64'd0;  R[5]  <= 64'd0;  R[6]  <= 64'd0;  R[7]  <= 64'd0;
            R[8]  <= 64'd0;  R[9]  <= 64'd0;  R[10] <= 64'd0;  R[11] <= 64'd0;
            R[12] <= 64'd0;  R[13] <= 64'd0;  R[14] <= 64'd0;  R[15] <= 64'd0;

        end else begin
            bus_valid <= 1'b0;

            // Cycle counter
            if (perf_enable)
                perf_cycles <= perf_cycles + 64'd1;

            case (cpu_state)

                // ============================================================
                // FETCH: check for pending interrupts, begin byte fetch
                // ============================================================
                CPU_FETCH: begin
                    if (irq_pending && ibuf_len == 0) begin
                        cpu_state <= CPU_IRQ;
                    end else begin
                        fetch_pending <= 1'b0;
                        cpu_state <= CPU_FETCH_MORE;
                    end
                end

                // ============================================================
                // FETCH_MORE: read instruction bytes from memory one at a time
                // ============================================================
                CPU_FETCH_MORE: begin
                    if (!fetch_pending) begin
                        bus_valid <= 1'b1;
                        bus_addr  <= R[psel] + {60'd0, ibuf_len};
                        bus_wen   <= 1'b0;
                        bus_size  <= BUS_BYTE;
                        fetch_pending <= 1'b1;
                    end else if (!bus_ready) begin
                        bus_valid <= 1'b1;
                    end

                    if (bus_ready && fetch_pending) begin
                        fetch_pending <= 1'b0;
                        ibuf[ibuf_len] <= bus_rdata[7:0];
                        ibuf_len <= ibuf_len + 1;

                        if (ibuf_len == 0) begin
                            ibuf_need <= instr_len(bus_rdata[7:0], ext_active);
                        end

                        if (ibuf_len == 0) begin
                            if (instr_len(bus_rdata[7:0], ext_active) == 1)
                                cpu_state <= CPU_DECODE;
                        end else if (ibuf_len + 1 >= ibuf_need) begin
                            cpu_state <= CPU_DECODE;
                        end
                    end
                end

                // ============================================================
                // DECODE + EXECUTE
                // ============================================================
                CPU_DECODE: begin
                    // Advance PC past the instruction
                    R[psel] <= R[psel] + {60'd0, ibuf_len};
                    ibuf_len  <= 4'd0;
                    ibuf_need <= 4'd1;
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
                    // SYS family (0x0)
                    // --------------------------------------------------------
                    else if (fam == FAM_SYS) begin
                        ext_active <= 1'b0;
                        case (nib)
                            4'h0: cpu_state <= CPU_HALT;           // IDL

                            4'h1: cpu_state <= CPU_FETCH;          // NOP

                            4'h2: cpu_state <= CPU_HALT;           // HALT

                            4'h3: begin // RESET
                                psel  <= 4'd3;
                                xsel  <= 4'd2;
                                spsel <= 4'd15;
                                flags <= 8'h40;
                                D <= 8'd0;  Q <= 1'b0;  T <= 8'd0;
                                ivt_base <= 64'd0;
                                ivec_id  <= 8'd0;
                                R[0]  <= 64'd0;  R[1]  <= 64'd0;
                                R[2]  <= 64'd0;  R[3]  <= 64'd0;
                                R[4]  <= 64'd0;  R[5]  <= 64'd0;
                                R[6]  <= 64'd0;  R[7]  <= 64'd0;
                                R[8]  <= 64'd0;  R[9]  <= 64'd0;
                                R[10] <= 64'd0;  R[11] <= 64'd0;
                                R[12] <= 64'd0;  R[13] <= 64'd0;
                                R[14] <= 64'd0;  R[15] <= 64'd0;
                                cpu_state <= CPU_FETCH;
                            end

                            4'h4: begin // RTI
                                effective_addr <= R[spsel];
                                R[spsel] <= R[spsel] + 64'd8;
                                dst_reg     <= psel;
                                post_action <= POST_RTI_POP2;
                                cpu_state   <= CPU_MEM_READ;
                            end

                            4'h5: begin // RET (1802)
                                effective_addr <= R[spsel];
                                R[spsel] <= R[spsel] + 64'd8;
                                dst_reg  <= 4'd0;
                                mem_sub  <= 4'h5;
                                cpu_state <= CPU_MEM_READ;
                            end

                            4'h6: begin // DIS
                                effective_addr <= R[spsel];
                                R[spsel] <= R[spsel] + 64'd8;
                                dst_reg  <= 4'd0;
                                mem_sub  <= 4'h6;
                                cpu_state <= CPU_MEM_READ;
                            end

                            4'h7: begin // MARK
                                T <= {xsel, psel};
                                R[spsel] <= R[spsel] - 64'd8;
                                effective_addr <= R[spsel] - 64'd8;
                                mem_data <= {56'd0, xsel, psel};
                                xsel <= psel;
                                cpu_state <= CPU_MEM_WRITE;
                            end

                            4'h8: begin // SAV
                                effective_addr <= R[xsel];
                                mem_data <= {56'd0, T};
                                bus_size <= BUS_BYTE;
                                cpu_state <= CPU_MEM_WRITE;
                            end

                            4'h9: begin Q <= 1'b1; cpu_state <= CPU_FETCH; end  // SEQ
                            4'hA: begin Q <= 1'b0; cpu_state <= CPU_FETCH; end  // REQ
                            4'hB: begin flags[6] <= 1'b1; cpu_state <= CPU_FETCH; end  // EI
                            4'hC: begin flags[6] <= 1'b0; cpu_state <= CPU_FETCH; end  // DI

                            4'hD: begin // CALL.L Rn
                                R[spsel] <= R[spsel] - 64'd8;
                                effective_addr <= R[spsel] - 64'd8;
                                mem_data <= R[psel] + {60'd0, ibuf_len};
                                R[psel] <= R[ibuf[1][3:0]];
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
                                ivec_id <= 8'd6;
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
                    // BR — short branch / SKIP (0x3)
                    // --------------------------------------------------------
                    else if (fam == FAM_BR) begin
                        ext_active <= 1'b0;
                        if (ext_active && ext_mod == 4'd6) begin
                            // SKIP mode
                            if (cond_eval(nib, flags, Q, ef_flags)) begin
                                bus_valid <= 1'b1;
                                bus_addr  <= R[psel] + {60'd0, ibuf_len};
                                bus_wen   <= 1'b0;
                                bus_size  <= 2'b00;
                                cpu_state <= CPU_SKIP;
                            end else begin
                                cpu_state <= CPU_FETCH;
                            end
                        end else begin
                            if (cond_eval(nib, flags, Q, ef_flags)) begin
                                R[psel] <= R[psel]
                                           + {{56{ibuf[1][7]}}, ibuf[1]}
                                           - {60'd0, ibuf_len};
                            end
                            cpu_state <= CPU_FETCH;
                        end
                    end

                    // --------------------------------------------------------
                    // LBR — long branch (0x4)
                    // --------------------------------------------------------
                    else if (fam == FAM_LBR) begin
                        ext_active <= 1'b0;
                        if (cond_eval(nib, flags, Q, ef_flags)) begin
                            R[psel] <= R[psel]
                                       + {{48{ibuf[1][7]}}, ibuf[1], ibuf[2]}
                                       - {60'd0, ibuf_len};
                        end
                        cpu_state <= CPU_FETCH;
                    end

                    // --------------------------------------------------------
                    // MEM — scalar load/store (0x5)
                    // --------------------------------------------------------
                    else if (fam == FAM_MEM) begin
                        ext_active <= 1'b0;
                        dst_reg <= ibuf[1][7:4];
                        src_reg <= ibuf[1][3:0];
                        mem_sub <= nib;

                        case (nib)
                            4'h0: begin // LDN
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'h1: begin // LDA
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'h2: begin // LDX
                                effective_addr <= R[xsel];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'h3: begin // LDXA
                                effective_addr <= R[xsel];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'h4: begin // STR
                                effective_addr <= R[ibuf[1][7:4]];
                                mem_data <= R[ibuf[1][3:0]];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_WRITE;
                            end
                            4'h5: begin // STXD
                                effective_addr <= R[xsel];
                                mem_data <= R[ibuf[1][7:4]];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_WRITE;
                            end
                            4'h6: begin // LD.B
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_BYTE;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'h7: begin // ST.B
                                effective_addr <= R[ibuf[1][7:4]];
                                mem_data <= {56'd0, R[ibuf[1][3:0]][7:0]};
                                bus_size <= BUS_BYTE;
                                cpu_state <= CPU_MEM_WRITE;
                            end
                            4'h8: begin // LD.H
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_HALF;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'h9: begin // ST.H
                                effective_addr <= R[ibuf[1][7:4]];
                                mem_data <= {48'd0, R[ibuf[1][3:0]][15:0]};
                                bus_size <= BUS_HALF;
                                cpu_state <= CPU_MEM_WRITE;
                            end
                            4'hA: begin // LD.W
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_WORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'hB: begin // ST.W
                                effective_addr <= R[ibuf[1][7:4]];
                                mem_data <= {32'd0, R[ibuf[1][3:0]][31:0]};
                                bus_size <= BUS_WORD;
                                cpu_state <= CPU_MEM_WRITE;
                            end
                            4'hC: begin // LD.SB
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_BYTE;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'hD: begin // LD.SH
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_HALF;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'hE: begin // LD.SW
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_WORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'hF: begin // LD.D Rd, [Rn+off8*8]
                                effective_addr <= R[ibuf[1][3:0]]
                                    + ({{56{ibuf[2][7]}}, ibuf[2]} << 3);
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                        endcase
                    end

                    // --------------------------------------------------------
                    // IMM family (0x6)
                    // --------------------------------------------------------
                    else if (fam == FAM_IMM) begin
                        dst_reg <= ibuf[1][7:4];

                        if (ext_active && ext_mod == 4'd0) begin
                            // EXT.IMM64
                            R[ibuf[1][7:4]] <= {ibuf[9], ibuf[8], ibuf[7], ibuf[6],
                                                 ibuf[5], ibuf[4], ibuf[3], ibuf[2]};
                            ext_active <= 1'b0;
                            cpu_state <= CPU_FETCH;
                        end else begin
                            ext_active <= 1'b0;
                            case (nib)
                                4'h0: begin // LDI
                                    R[ibuf[1][7:4]] <= {56'd0, ibuf[2]};
                                    cpu_state <= CPU_FETCH;
                                end
                                4'h1: begin // LHI
                                    R[ibuf[1][7:4]][63:48] <= {ibuf[3], ibuf[2]};
                                    cpu_state <= CPU_FETCH;
                                end
                                4'h2: begin // ADDI
                                    alu_a  <= R[ibuf[1][7:4]];
                                    alu_b  <= {{56{ibuf[2][7]}}, ibuf[2]};
                                    alu_op <= ALU_ADD;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h3: begin // ANDI
                                    alu_a  <= R[ibuf[1][7:4]];
                                    alu_b  <= {56'd0, ibuf[2]};
                                    alu_op <= ALU_AND;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h4: begin // ORI
                                    alu_a  <= R[ibuf[1][7:4]];
                                    alu_b  <= {56'd0, ibuf[2]};
                                    alu_op <= ALU_OR;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h5: begin // XORI
                                    alu_a  <= R[ibuf[1][7:4]];
                                    alu_b  <= {56'd0, ibuf[2]};
                                    alu_op <= ALU_XOR;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h6: begin // CMPI
                                    alu_a  <= R[ibuf[1][7:4]];
                                    alu_b  <= {{56{ibuf[2][7]}}, ibuf[2]};
                                    alu_op <= ALU_CMP;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h7: begin // SUBI
                                    alu_a  <= R[ibuf[1][7:4]];
                                    alu_b  <= {{56{ibuf[2][7]}}, ibuf[2]};
                                    alu_op <= ALU_SUB;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h8: begin // LSLI
                                    R[ibuf[1][7:4]] <= R[ibuf[1][7:4]]
                                                        << ibuf[1][3:0];
                                    cpu_state <= CPU_FETCH;
                                end
                                4'h9: begin // LSRI
                                    R[ibuf[1][7:4]] <= R[ibuf[1][7:4]]
                                                        >> ibuf[1][3:0];
                                    cpu_state <= CPU_FETCH;
                                end
                                4'hA: begin // ASRI
                                    R[ibuf[1][7:4]] <= $signed(R[ibuf[1][7:4]])
                                                        >>> ibuf[1][3:0];
                                    cpu_state <= CPU_FETCH;
                                end
                                4'hB: begin // ROLI
                                    if (ibuf[1][3:0] != 0)
                                        R[ibuf[1][7:4]] <=
                                            (R[ibuf[1][7:4]] << ibuf[1][3:0]) |
                                            (R[ibuf[1][7:4]] >> (4'd0 - ibuf[1][3:0]));
                                    cpu_state <= CPU_FETCH;
                                end
                                4'hC: begin // GLO
                                    D <= R[ibuf[1][7:4]][7:0];
                                    cpu_state <= CPU_FETCH;
                                end
                                4'hD: begin // GHI
                                    D <= R[ibuf[1][7:4]][15:8];
                                    cpu_state <= CPU_FETCH;
                                end
                                4'hE: begin // PLO
                                    R[ibuf[1][7:4]][7:0] <= D;
                                    cpu_state <= CPU_FETCH;
                                end
                                4'hF: begin // PHI
                                    R[ibuf[1][7:4]][15:8] <= D;
                                    cpu_state <= CPU_FETCH;
                                end
                            endcase
                        end
                    end

                    // --------------------------------------------------------
                    // ALU — register-register (0x7)
                    // --------------------------------------------------------
                    else if (fam == FAM_ALU) begin
                        ext_active <= 1'b0;
                        dst_reg <= ibuf[1][7:4];
                        src_reg <= ibuf[1][3:0];
                        alu_a   <= R[ibuf[1][7:4]];
                        alu_b   <= R[ibuf[1][3:0]];
                        case (nib)
                            4'h0: alu_op <= ALU_ADD;
                            4'h1: alu_op <= ALU_ADC;
                            4'h2: alu_op <= ALU_SUB;
                            4'h3: alu_op <= ALU_SBB;
                            4'h4: alu_op <= ALU_AND;
                            4'h5: alu_op <= ALU_OR;
                            4'h6: alu_op <= ALU_XOR;
                            4'h7: alu_op <= ALU_CMP;
                            4'h8: alu_op <= ALU_MOV;
                            4'h9: alu_op <= ALU_NOT;
                            4'hA: alu_op <= ALU_NEG;
                            4'hB: alu_op <= ALU_SHL;
                            4'hC: alu_op <= ALU_SHR;
                            4'hD: alu_op <= ALU_SAR;
                            4'hE: alu_op <= ALU_ROL;
                            4'hF: alu_op <= ALU_ROR;
                        endcase
                        cpu_state <= CPU_EXECUTE;
                    end

                    // --------------------------------------------------------
                    // MEMALU — 1802 heritage D-register ops (0x8)
                    // --------------------------------------------------------
                    else if (fam == FAM_MEMALU) begin
                        ext_active <= 1'b0;
                        memalu_sub <= nib;

                        case (nib)
                            4'hE: begin // IRX
                                R[xsel] <= R[xsel] + 64'd1;
                                cpu_state <= CPU_FETCH;
                            end
                            4'h6: begin // SHR.D
                                flags[1] <= D[0];
                                D <= {1'b0, D[7:1]};
                                flags[0] <= ({1'b0, D[7:1]} == 8'd0) ? 1'b1 : 1'b0;
                                cpu_state <= CPU_FETCH;
                            end
                            4'hA: begin // SHRC.D
                                begin
                                    reg old_c;
                                    old_c = flags[1];
                                    flags[1] <= D[0];
                                    D <= {old_c, D[7:1]};
                                    flags[0] <= ({old_c, D[7:1]} == 8'd0) ? 1'b1 : 1'b0;
                                end
                                cpu_state <= CPU_FETCH;
                            end
                            4'hC: begin // SHL.D
                                flags[1] <= D[7];
                                D <= {D[6:0], 1'b0};
                                flags[0] <= ({D[6:0], 1'b0} == 8'd0) ? 1'b1 : 1'b0;
                                cpu_state <= CPU_FETCH;
                            end
                            4'hD: begin // SHLC.D
                                begin
                                    reg old_c2;
                                    old_c2 = flags[1];
                                    flags[1] <= D[7];
                                    D <= {D[6:0], old_c2};
                                    flags[0] <= ({D[6:0], old_c2} == 8'd0) ? 1'b1 : 1'b0;
                                end
                                cpu_state <= CPU_FETCH;
                            end
                            default: begin
                                // All others read M(R(X))
                                effective_addr <= R[xsel];
                                bus_size <= BUS_BYTE;
                                cpu_state <= CPU_MEMALU_RD;
                            end
                        endcase
                    end

                    // --------------------------------------------------------
                    // IO — port I/O (0x9)
                    // --------------------------------------------------------
                    else if (fam == FAM_IO) begin
                        ext_active <= 1'b0;
                        if (nib >= 4'd1 && nib <= 4'd7) begin
                            // OUT N
                            io_port   <= nib[2:0];
                            io_is_inp <= 1'b0;
                            effective_addr <= R[xsel];
                            bus_size  <= BUS_BYTE;
                            cpu_state <= CPU_MEMALU_RD;
                            memalu_sub <= 4'hF;
                        end else if (nib >= 4'd9) begin
                            // INP (N-8)
                            io_port   <= nib[2:0];
                            io_is_inp <= 1'b1;
                            effective_addr <= {MMIO_HI, 20'd0, nib[2:0], 9'd0};
                            bus_size  <= BUS_BYTE;
                            cpu_state <= CPU_MEM_READ;
                            dst_reg   <= 4'd0;
                            mem_sub   <= 4'hF;
                        end else begin
                            cpu_state <= CPU_FETCH;
                        end
                    end

                    // --------------------------------------------------------
                    // SEP (0xA)
                    // --------------------------------------------------------
                    else if (fam == FAM_SEP) begin
                        ext_active <= 1'b0;
                        psel <= nib;
                        cpu_state <= CPU_FETCH;
                    end

                    // --------------------------------------------------------
                    // SEX (0xB)
                    // --------------------------------------------------------
                    else if (fam == FAM_SEX) begin
                        ext_active <= 1'b0;
                        xsel <= nib;
                        cpu_state <= CPU_FETCH;
                    end

                    // --------------------------------------------------------
                    // MULDIV (0xC) — NOT SUPPORTED ON MICRO-CORE
                    // --------------------------------------------------------
                    else if (fam == FAM_MULDIV) begin
                        ext_active <= 1'b0;
                        // Trap: illegal opcode
                        trap_addr <= R[psel];
                        ivec_id   <= {5'd0, IRQ_ILLOP};
                        R[spsel]  <= R[spsel] - 64'd8;
                        effective_addr <= R[spsel] - 64'd8;
                        mem_data  <= R[psel] + {60'd0, ibuf_len};
                        flags[6]  <= 1'b0;
                        post_action <= POST_IRQ_VEC;
                        bus_size  <= BUS_DWORD;
                        cpu_state <= CPU_MEM_WRITE;
                    end

                    // --------------------------------------------------------
                    // CSR (0xD) — reduced set (no tile/BIST/DMA/icache)
                    // --------------------------------------------------------
                    else if (fam == FAM_CSR) begin
                        ext_active <= 1'b0;
                        if (nib[3]) begin
                            // CSRW: GPR[nib[2:0]] → CSR[ibuf[1]]
                            case (ibuf[1])
                                CSR_FLAGS:    flags    <= R[nib[2:0]][7:0];
                                CSR_PSEL:     psel     <= R[nib[2:0]][3:0];
                                CSR_XSEL:     xsel     <= R[nib[2:0]][3:0];
                                CSR_SPSEL:    spsel    <= R[nib[2:0]][3:0];
                                CSR_IVTBASE:  ivt_base <= R[nib[2:0]];
                                CSR_D:        D        <= R[nib[2:0]][7:0];
                                CSR_DF:       flags[1] <= R[nib[2:0]][0];
                                CSR_QREG:     Q        <= R[nib[2:0]][0];
                                CSR_TREG:     T        <= R[nib[2:0]][7:0];
                                CSR_IE:       flags[6] <= R[nib[2:0]][0];
                                CSR_IVEC_ID:  ivec_id  <= R[nib[2:0]][7:0];
                                CSR_PERF_CTRL: begin
                                    perf_enable <= R[nib[2:0]][0];
                                    if (R[nib[2:0]][1])
                                        perf_cycles <= 64'd0;
                                end
                                // All other CSRs: silently ignored
                                default: ;
                            endcase
                        end else begin
                            // CSRR: CSR[ibuf[1]] → GPR[nib[2:0]]
                            case (ibuf[1])
                                CSR_FLAGS:       R[nib[2:0]] <= {56'd0, flags};
                                CSR_PSEL:        R[nib[2:0]] <= {60'd0, psel};
                                CSR_XSEL:        R[nib[2:0]] <= {60'd0, xsel};
                                CSR_SPSEL:       R[nib[2:0]] <= {60'd0, spsel};
                                CSR_IVTBASE:     R[nib[2:0]] <= ivt_base;
                                CSR_D:           R[nib[2:0]] <= {56'd0, D};
                                CSR_DF:          R[nib[2:0]] <= {63'd0, flags[1]};
                                CSR_QREG:        R[nib[2:0]] <= {63'd0, Q};
                                CSR_TREG:        R[nib[2:0]] <= {56'd0, T};
                                CSR_IE:          R[nib[2:0]] <= {63'd0, flags[6]};
                                CSR_COREID:      R[nib[2:0]] <= {62'd0, core_id};
                                CSR_NCORES:      R[nib[2:0]] <= {62'd0, NUM_CORES[1:0]};
                                CSR_IVEC_ID:     R[nib[2:0]] <= {56'd0, ivec_id};
                                CSR_TRAP_ADDR:   R[nib[2:0]] <= trap_addr;
                                CSR_MEGAPAD_SZ:  R[nib[2:0]] <= 64'd0;
                                CSR_CPUID:       R[nib[2:0]] <= 64'h4D50_3634_0001_4D43; // "MP64" v1 "MC"
                                CSR_PERF_CYCLES: R[nib[2:0]] <= perf_cycles;
                                CSR_PERF_CTRL:   R[nib[2:0]] <= {63'd0, perf_enable};
                                // Unsupported CSRs read as 0
                                default:         R[nib[2:0]] <= 64'd0;
                            endcase
                        end
                        cpu_state <= CPU_FETCH;
                    end

                    // --------------------------------------------------------
                    // MEX (0xE) — NOT SUPPORTED ON MICRO-CORE
                    // --------------------------------------------------------
                    else if (fam == FAM_MEX) begin
                        ext_active <= 1'b0;
                        // Trap: illegal opcode
                        trap_addr <= R[psel];
                        ivec_id   <= {5'd0, IRQ_ILLOP};
                        R[spsel]  <= R[spsel] - 64'd8;
                        effective_addr <= R[spsel] - 64'd8;
                        mem_data  <= R[psel] + {60'd0, ibuf_len};
                        flags[6]  <= 1'b0;
                        post_action <= POST_IRQ_VEC;
                        bus_size  <= BUS_DWORD;
                        cpu_state <= CPU_MEM_WRITE;
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
                    bus_addr  <= effective_addr;
                    bus_wen   <= 1'b0;
                    if (bus_ready) begin
                        bus_valid <= 1'b0;
                        case (mem_sub)
                            4'hC: R[dst_reg] <= {{56{bus_rdata[7]}}, bus_rdata[7:0]};
                            4'hD: R[dst_reg] <= {{48{bus_rdata[15]}}, bus_rdata[15:0]};
                            4'hE: R[dst_reg] <= {{32{bus_rdata[31]}}, bus_rdata[31:0]};
                            4'hF: begin // INP
                                D <= bus_rdata[7:0];
                                effective_addr <= R[xsel];
                                mem_data <= {56'd0, bus_rdata[7:0]};
                                bus_size <= BUS_BYTE;
                                cpu_state <= CPU_MEM_WRITE;
                            end
                            4'h5: begin // 1802 RET
                                T <= bus_rdata[7:0];
                                xsel <= bus_rdata[7:4];
                                psel <= bus_rdata[3:0];
                                flags[6] <= 1'b1;
                                cpu_state <= CPU_FETCH;
                            end
                            4'h6: begin // DIS
                                T <= bus_rdata[7:0];
                                xsel <= bus_rdata[7:4];
                                psel <= bus_rdata[3:0];
                                flags[6] <= 1'b0;
                                cpu_state <= CPU_FETCH;
                            end
                            default:
                                R[dst_reg] <= bus_rdata;
                        endcase

                        if (mem_sub == 4'h1) R[src_reg] <= R[src_reg] + 64'd8;
                        if (mem_sub == 4'h3) R[xsel]    <= R[xsel]    + 64'd8;

                        if (post_action == POST_RTI_POP2) begin
                            effective_addr <= R[spsel];
                            R[spsel] <= R[spsel] + 64'd8;
                            bus_size <= BUS_DWORD;
                            post_action <= POST_NONE;
                            cpu_state <= CPU_MEM_READ2;
                        end else if (mem_sub != 4'hF && mem_sub != 4'h5
                                     && mem_sub != 4'h6) begin
                            cpu_state <= CPU_FETCH;
                        end
                    end else begin
                        bus_valid <= 1'b1;
                    end
                end

                // ============================================================
                // MEM_READ2: second read (RTI flags pop)
                // ============================================================
                CPU_MEM_READ2: begin
                    bus_addr  <= effective_addr;
                    bus_wen   <= 1'b0;
                    if (bus_ready) begin
                        bus_valid <= 1'b0;
                        flags <= bus_rdata[7:0];
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
                    if (bus_ready) begin
                        // Deassert bus_valid on ack to create 1-cycle gap
                        // before the next bus-active state (IRQ_PUSH etc.)
                        bus_valid <= 1'b0;
                        if (mem_sub == 4'h5)
                            R[xsel] <= R[xsel] - 64'd8;

                        if (post_action == POST_IRQ_VEC) begin
                            R[spsel] <= R[spsel] - 64'd8;
                            effective_addr <= R[spsel] - 64'd8;
                            mem_data <= {56'd0, flags};
                            post_action <= POST_NONE;
                            cpu_state <= CPU_IRQ_PUSH;
                        end else begin
                            cpu_state <= CPU_FETCH;
                        end
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
                        effective_addr <= ivt_base + {56'd0, ivec_id, 3'b000};
                        bus_size <= BUS_DWORD;
                        cpu_state <= CPU_IRQ_LOAD;
                    end else begin
                        bus_valid <= 1'b1;
                    end
                end

                // ============================================================
                // IRQ_LOAD: read handler address from IVT
                // ============================================================
                CPU_IRQ_LOAD: begin
                    bus_addr  <= effective_addr;
                    bus_wen   <= 1'b0;
                    bus_size  <= BUS_DWORD;
                    if (bus_ready) begin
                        bus_valid <= 1'b0;
                        R[psel] <= bus_rdata;
                        cpu_state <= CPU_FETCH;
                    end else begin
                        bus_valid <= 1'b1;
                    end
                end

                // ============================================================
                // MEMALU_RD: read M(R(X)) for MEMALU / IO ops
                // ============================================================
                CPU_MEMALU_RD: begin
                    bus_addr  <= effective_addr;
                    bus_wen   <= 1'b0;
                    bus_size  <= BUS_BYTE;
                    if (bus_ready) begin
                        bus_valid <= 1'b0;
                        memalu_byte <= bus_rdata[7:0];

                        if (memalu_sub == 4'hF) begin
                            // IO OUT
                            D <= bus_rdata[7:0];
                            effective_addr <= {MMIO_HI, 20'd0, io_port, 9'd0};
                            mem_data <= {56'd0, bus_rdata[7:0]};
                            bus_size <= BUS_BYTE;
                            R[xsel] <= R[xsel] + 64'd1;
                            cpu_state <= CPU_MEM_WRITE;
                            mem_sub <= 4'd0;
                        end else begin
                            case (memalu_sub)
                                4'h0: D <= bus_rdata[7:0];          // LDX
                                4'h1: begin                         // OR.X
                                    D <= (bus_rdata[7:0] | D);
                                    flags[0] <= ((bus_rdata[7:0] | D) == 8'd0);
                                end
                                4'h2: begin                         // AND.X
                                    D <= (bus_rdata[7:0] & D);
                                    flags[0] <= ((bus_rdata[7:0] & D) == 8'd0);
                                end
                                4'h3: begin                         // XOR.X
                                    D <= (bus_rdata[7:0] ^ D);
                                    flags[0] <= ((bus_rdata[7:0] ^ D) == 8'd0);
                                end
                                4'h4: begin                         // ADD.X
                                    begin
                                        reg [8:0] sum9;
                                        sum9 = {1'b0, bus_rdata[7:0]} + {1'b0, D};
                                        D <= sum9[7:0];
                                        flags[1] <= sum9[8];
                                        flags[0] <= (sum9[7:0] == 8'd0);
                                    end
                                end
                                4'h5: begin                         // SD.X
                                    begin
                                        reg [8:0] diff9;
                                        diff9 = {1'b0, bus_rdata[7:0]} - {1'b0, D};
                                        D <= diff9[7:0];
                                        flags[1] <= ~diff9[8];
                                        flags[0] <= (diff9[7:0] == 8'd0);
                                    end
                                end
                                4'h7: begin                         // SM.X
                                    begin
                                        reg [8:0] diff9b;
                                        diff9b = {1'b0, D} - {1'b0, bus_rdata[7:0]};
                                        D <= diff9b[7:0];
                                        flags[1] <= ~diff9b[8];
                                        flags[0] <= (diff9b[7:0] == 8'd0);
                                    end
                                end
                                4'h8: begin                         // ADC.X
                                    begin
                                        reg [8:0] sum9c;
                                        sum9c = {1'b0, bus_rdata[7:0]} + {1'b0, D}
                                                + {8'd0, flags[1]};
                                        D <= sum9c[7:0];
                                        flags[1] <= sum9c[8];
                                        flags[0] <= (sum9c[7:0] == 8'd0);
                                    end
                                end
                                4'h9: begin                         // SDB.X
                                    begin
                                        reg [8:0] diff9c;
                                        diff9c = {1'b0, bus_rdata[7:0]} - {1'b0, D}
                                                 - {8'd0, ~flags[1]};
                                        D <= diff9c[7:0];
                                        flags[1] <= ~diff9c[8];
                                        flags[0] <= (diff9c[7:0] == 8'd0);
                                    end
                                end
                                4'hB: begin                         // SMB.X
                                    begin
                                        reg [8:0] diff9d;
                                        diff9d = {1'b0, D} - {1'b0, bus_rdata[7:0]}
                                                 - {8'd0, ~flags[1]};
                                        D <= diff9d[7:0];
                                        flags[1] <= ~diff9d[8];
                                        flags[0] <= (diff9d[7:0] == 8'd0);
                                    end
                                end
                                4'hF: begin                         // LDXA
                                    D <= bus_rdata[7:0];
                                    R[xsel] <= R[xsel] + 64'd1;
                                end
                                default: ;
                            endcase
                            cpu_state <= CPU_FETCH;
                        end
                    end else begin
                        bus_valid <= 1'b1;
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
                    bus_size  <= 2'b00;
                    if (bus_ready) begin
                        R[psel] <= R[psel] + {60'd0, instr_len(bus_rdata[7:0], 1'b0)};
                        cpu_state <= CPU_FETCH;
                    end
                end

            endcase
        end
    end

endmodule
