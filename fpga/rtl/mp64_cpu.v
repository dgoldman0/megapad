// ============================================================================
// mp64_cpu.v — Megapad-64 CPU Core
// ============================================================================
//
// 64-bit CPU with RCA 1802-inspired register architecture and a custom
// extended ISA including hardware multiply/divide and tile engine (MEX).
//
// Key features:
//   - 16 × 64-bit general-purpose registers
//   - Selectable PC (PSEL), data pointer (XSEL), and SP (SPSEL)
//   - Variable-length instruction encoding (1–11 bytes)
//   - Subroutine-threaded Forth model (CALL.L / RET.L)
//   - 8-bit flags: Z, C, N, V, P, G, I (interrupt enable), S (saturation)
//   - Vectored interrupts (8 vectors, IVT base in CSR)
//   - CSR space for tile engine, interrupt, and system config
//
// Pipeline: single-stage (fetch → decode → execute → writeback in one cycle
// for simple instructions; multi-cycle for memory, MEX, and mul/div).
//
// This is a PROTOTYPE for FPGA bring-up.  A pipelined version would follow
// after functional verification.
//

`include "mp64_defs.vh"

module mp64_cpu (
    input  wire        clk,
    input  wire        rst_n,

    // === Core identification (set per instance by SoC) ===
    input  wire [CORE_ID_BITS-1:0] core_id,

    // === Memory bus master ===
    output reg         bus_valid,
    output reg  [63:0] bus_addr,
    output reg  [63:0] bus_wdata,
    output reg         bus_wen,
    output reg  [1:0]  bus_size,
    input  wire [63:0] bus_rdata,
    input  wire        bus_ready,

    // === Tile engine interface ===
    output reg         csr_wen,
    output reg  [7:0]  csr_addr,
    output reg  [63:0] csr_wdata,
    input  wire [63:0] csr_rdata,

    output reg         mex_valid,
    output reg  [1:0]  mex_ss,
    output reg  [1:0]  mex_op,
    output reg  [2:0]  mex_funct,
    output reg  [63:0] mex_gpr_val,
    output reg  [7:0]  mex_imm8,
    input  wire        mex_done,
    input  wire        mex_busy,

    // === Interrupts ===
    input  wire        irq_timer,
    input  wire        irq_uart,
    input  wire        irq_nic,
    input  wire        irq_ipi       // inter-processor interrupt
);

    // ========================================================================
    // Register file
    // ========================================================================
    reg [63:0] R [0:15];

    // Selector CSRs
    reg [3:0]  psel;       // which register is PC (default R3)
    reg [3:0]  xsel;       // which register is X (default R2)
    reg [3:0]  spsel;      // which register is SP (default R15)

    // Flags: [S:7 I:6 G:5 P:4 V:3 N:2 C:1 Z:0]
    reg [7:0]  flags;

    wire       flag_z = flags[0];
    wire       flag_c = flags[1];
    wire       flag_n = flags[2];
    wire       flag_v = flags[3];
    wire       flag_p = flags[4];
    wire       flag_g = flags[5];
    wire       flag_i = flags[6];   // interrupt enable
    wire       flag_s = flags[7];

    // Convenience aliases
    wire [63:0] PC = R[psel];
    wire [63:0] SP = R[spsel];

    // IVT base address (CSR)
    reg [63:0] ivt_base;

    // D register (8-bit, 1802 heritage)
    reg [7:0]  D;

    // EXT prefix modifier
    reg [3:0]  ext_mod;
    reg        ext_active;

    // Fetch handshake — prevents bus_valid re-assertion while ibuf_len is stale
    reg        fetch_pending;

    // ========================================================================
    // Instruction fetch buffer
    // ========================================================================
    // We fetch up to 11 bytes for the longest instruction (EXT + LDI imm64).
    // In this prototype, we fetch one byte at a time via the memory bus.
    // A production version would use an instruction cache / fetch buffer.

    reg [7:0]  ibuf [0:10];    // instruction bytes
    reg [3:0]  ibuf_len;       // bytes fetched so far
    reg [3:0]  ibuf_need;      // bytes needed for this instruction

    // Decoded instruction fields
    wire [3:0] fam  = ibuf[0][7:4];   // family
    wire [3:0] nib  = ibuf[0][3:0];   // sub-selector

    // ========================================================================
    // CPU state machine
    // ========================================================================
    localparam CPU_FETCH     = 4'd0;
    localparam CPU_DECODE    = 4'd1;
    localparam CPU_EXECUTE   = 4'd2;
    localparam CPU_MEM_READ  = 4'd3;
    localparam CPU_MEM_WRITE = 4'd4;
    localparam CPU_MEX_WAIT  = 4'd5;
    localparam CPU_IRQ       = 4'd6;
    localparam CPU_HALT      = 4'd7;
    localparam CPU_FETCH_MORE= 4'd8;
    localparam CPU_MULDIV    = 4'd9;

    reg [3:0]  cpu_state;

    // ========================================================================
    // Interrupt pending logic
    // ========================================================================
    reg        irq_pending;
    reg [2:0]  irq_vector;

    always @(*) begin
        irq_pending = 1'b0;
        irq_vector  = 3'd0;
        if (flag_i) begin
            // Priority: IPI > Timer > UART > NIC
            if (irq_ipi) begin
                irq_pending = 1'b1;
                irq_vector  = IRQ_NMI;    // IPI uses NMI vector (highest hw prio)
            end else if (irq_timer) begin
                irq_pending = 1'b1;
                irq_vector  = IRQ_TIMER;
            end
            // UART and NIC interrupt routing handled by SoC
        end
    end

    // ========================================================================
    // Condition code evaluation (for branches)
    // ========================================================================
    function cond_eval;
        input [3:0] cond;
        input [7:0] f;
        begin
            case (cond)
                4'h0: cond_eval = 1'b1;           // AL (always)
                4'h1: cond_eval = f[0];            // EQ (Z=1)
                4'h2: cond_eval = !f[0];           // NE (Z=0)
                4'h3: cond_eval = f[1];            // CS (C=1)
                4'h4: cond_eval = !f[1];           // CC (C=0)
                4'h5: cond_eval = f[2];            // MI (N=1)
                4'h6: cond_eval = !f[2];           // PL (N=0)
                4'h7: cond_eval = f[3];            // VS (V=1)
                4'h8: cond_eval = !f[3];           // VC (V=0)
                4'h9: cond_eval = f[5];            // GT (G=1)
                4'hA: cond_eval = !f[5];           // LE (G=0)
                4'hF: cond_eval = 1'b0;            // NV (never)
                default: cond_eval = 1'b0;
            endcase
        end
    endfunction

    // ========================================================================
    // ALU
    // ========================================================================
    reg [63:0] alu_a, alu_b, alu_result;
    reg [7:0]  alu_flags_out;
    reg [3:0]  alu_op;

    localparam ALU_ADD = 4'd0;
    localparam ALU_SUB = 4'd1;
    localparam ALU_AND = 4'd2;
    localparam ALU_OR  = 4'd3;
    localparam ALU_XOR = 4'd4;
    localparam ALU_MOV = 4'd5;
    localparam ALU_NOT = 4'd6;
    localparam ALU_NEG = 4'd7;
    localparam ALU_SHL = 4'd8;
    localparam ALU_SHR = 4'd9;
    localparam ALU_SAR = 4'd10;
    localparam ALU_CMP = 4'd11;
    localparam ALU_ADC = 4'd12;
    localparam ALU_SBB = 4'd13;

    reg [64:0] alu_wide;   // 65-bit for carry

    always @(*) begin
        alu_result = 64'd0;
        alu_flags_out = flags;
        alu_wide = 65'd0;

        case (alu_op)
            ALU_ADD: begin
                alu_wide = {1'b0, alu_a} + {1'b0, alu_b};
                alu_result = alu_wide[63:0];
                alu_flags_out[1] = alu_wide[64];  // carry
            end
            ALU_ADC: begin
                alu_wide = {1'b0, alu_a} + {1'b0, alu_b} + {64'd0, flag_c};
                alu_result = alu_wide[63:0];
                alu_flags_out[1] = alu_wide[64];
            end
            ALU_SUB, ALU_CMP: begin
                alu_wide = {1'b0, alu_a} - {1'b0, alu_b};
                alu_result = alu_wide[63:0];
                alu_flags_out[1] = !alu_wide[64];  // borrow
                alu_flags_out[5] = (alu_a > alu_b); // G (unsigned greater)
            end
            ALU_SBB: begin
                alu_wide = {1'b0, alu_a} - {1'b0, alu_b} - {64'd0, flag_c};
                alu_result = alu_wide[63:0];
                alu_flags_out[1] = !alu_wide[64];
            end
            ALU_AND: alu_result = alu_a & alu_b;
            ALU_OR:  alu_result = alu_a | alu_b;
            ALU_XOR: alu_result = alu_a ^ alu_b;
            ALU_MOV: alu_result = alu_b;
            ALU_NOT: alu_result = ~alu_a;
            ALU_NEG: begin
                alu_result = -alu_a;
                alu_flags_out[1] = (alu_a != 64'd0);
            end
            ALU_SHL: alu_result = alu_a << alu_b[5:0];
            ALU_SHR: alu_result = alu_a >> alu_b[5:0];
            ALU_SAR: alu_result = $signed(alu_a) >>> alu_b[5:0];
            default: alu_result = 64'd0;
        endcase

        // Common flags
        alu_flags_out[0] = (alu_result == 64'd0);             // Z
        alu_flags_out[2] = alu_result[63];                    // N
        alu_flags_out[3] = (alu_a[63] ^ alu_b[63]) &         // V (signed overflow)
                           (alu_a[63] ^ alu_result[63]);
    end

    // ========================================================================
    // Main state machine
    // ========================================================================
    //
    // This is a simplified single-issue, multi-cycle implementation.
    // Fetch → Decode → Execute happen in sequence.  Memory operations
    // and MEX operations add extra cycles.
    //

    // Instruction length decoder (how many bytes do we need?)
    function [3:0] instr_len;
        input [7:0] byte0;
        input       has_ext;
        begin
            case (byte0[7:4])
                FAM_SYS:   instr_len = (byte0[3:0] == 4'hD) ? 4'd2  // CALL.L Rn
                                      : (byte0[3:0] == 4'hF) ? 4'd2  // TRAP
                                      : 4'd1;
                FAM_INC:   instr_len = 4'd1;
                FAM_DEC:   instr_len = 4'd1;
                FAM_BR:    instr_len = 4'd2;  // BR cond, offset8
                FAM_LBR:   instr_len = 4'd3;  // LBR cond, offset16
                FAM_MEM:   instr_len = 4'd3;  // MEM Rd, [Rs + off8]
                FAM_IMM:   instr_len = (byte0[3:0] == 4'h1) ? 4'd4   // LHI imm16
                                      : has_ext ? 4'd10               // EXT.IMM64 + LDI Rn + 8B
                                      : 4'd3;                         // LDI/ADDI/etc Rn, imm8
                FAM_ALU:   instr_len = 4'd2;  // ALU Rd, Rs
                FAM_MEMALU:instr_len = 4'd1;
                FAM_IO:    instr_len = 4'd1;
                FAM_SEP:   instr_len = 4'd1;
                FAM_SEX:   instr_len = 4'd1;
                FAM_MULDIV:instr_len = 4'd2;  // MUL Rd, Rs
                FAM_CSR:   instr_len = 4'd3;  // CSR addr, Rn
                FAM_MEX:   instr_len = 4'd3;  // MEX varies, 2-3 bytes
                FAM_EXT:   instr_len = 4'd1;  // prefix only
                default:   instr_len = 4'd1;
            endcase
        end
    endfunction

    // Temporary registers for multi-cycle operations
    reg [63:0] mem_data;
    reg [3:0]  dst_reg;
    reg [3:0]  src_reg;
    reg [63:0] effective_addr;

    // Multiply/divide
    reg [127:0] mul_result;
    reg [63:0]  div_quotient, div_remainder;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            // Reset state
            cpu_state  <= CPU_FETCH;
            bus_valid  <= 1'b0;
            csr_wen    <= 1'b0;
            mex_valid  <= 1'b0;
            ext_active <= 1'b0;
            ext_mod    <= 4'd0;
            fetch_pending <= 1'b0;
            ibuf_len   <= 4'd0;
            ibuf_need  <= 4'd1;  // fetch at least 1 byte

            // Default register setup (matches BIOS expectations)
            psel  <= 4'd3;     // R3 = PC
            xsel  <= 4'd2;     // R2 = X
            spsel <= 4'd15;    // R15 = SP
            flags <= 8'h40;    // I=1 (interrupts enabled), rest clear

            ivt_base <= 64'd0;
            D        <= 8'd0;

            // Clear register file
            R[0]  <= 64'd0;  R[1]  <= 64'd0;  R[2]  <= 64'd0;  R[3]  <= 64'd0;
            R[4]  <= 64'd0;  R[5]  <= 64'd0;  R[6]  <= 64'd0;  R[7]  <= 64'd0;
            R[8]  <= 64'd0;  R[9]  <= 64'd0;  R[10] <= 64'd0;  R[11] <= 64'd0;
            R[12] <= 64'd0;  R[13] <= 64'd0;  R[14] <= 64'd0;  R[15] <= 64'd0;

        end else begin
            bus_valid <= 1'b0;
            csr_wen   <= 1'b0;
            mex_valid <= 1'b0;

            case (cpu_state)

                // ============================================================
                // FETCH: read instruction bytes from memory one at a time
                // ============================================================
                CPU_FETCH: begin
                    // Check for pending interrupt first
                    if (irq_pending && ibuf_len == 0) begin
                        cpu_state <= CPU_IRQ;
                    end else begin
                        bus_valid <= 1'b1;
                        bus_addr  <= R[psel] + {60'd0, ibuf_len};
                        bus_wen   <= 1'b0;
                        bus_size  <= BUS_BYTE;
                        fetch_pending <= 1'b1;
                        cpu_state <= CPU_FETCH_MORE;
                    end
                end

                CPU_FETCH_MORE: begin
                    // Drive bus_valid:
                    //  - New fetch:  assert with updated address
                    //  - Waiting:    keep asserted (multi-core visibility)
                    //  - On response: deassert (let arbiter serve others)
                    if (!fetch_pending) begin
                        bus_valid <= 1'b1;
                        bus_addr  <= R[psel] + {60'd0, ibuf_len};
                        bus_wen   <= 1'b0;
                        bus_size  <= BUS_BYTE;
                        fetch_pending <= 1'b1;
                    end else if (!bus_ready) begin
                        // Keep request visible to arbiter while waiting
                        bus_valid <= 1'b1;
                    end
                    // else: bus_ready && fetch_pending → bus_valid stays 0
                    // (default), giving the arbiter 1 cycle to move on

                    if (bus_ready && fetch_pending) begin
                        fetch_pending <= 1'b0;
                        ibuf[ibuf_len] <= bus_rdata[7:0];
                        ibuf_len <= ibuf_len + 1;

                        // After first byte, determine instruction length
                        if (ibuf_len == 0) begin
                            ibuf_need <= instr_len(bus_rdata[7:0], ext_active);
                        end

                        // Have we fetched enough?
                        // NOTE: for the first byte (ibuf_len==0), ibuf_need is
                        // stale (still holds the reset/previous value), so we
                        // use instr_len() directly instead.
                        if (ibuf_len == 0) begin
                            if (instr_len(bus_rdata[7:0], ext_active) == 1)
                                cpu_state <= CPU_DECODE;
                        end else if (ibuf_len + 1 >= ibuf_need) begin
                            cpu_state <= CPU_DECODE;
                        end
                    end
                end

                // ============================================================
                // DECODE + EXECUTE (combined for single-cycle instructions)
                // ============================================================
                CPU_DECODE: begin
                    // Advance PC past the instruction
                    R[psel] <= R[psel] + {60'd0, ibuf_len};
                    ibuf_len <= 4'd0;
                    ibuf_need <= 4'd1;

                    // --- EXT prefix ---
                    if (fam == FAM_EXT) begin
                        ext_active <= 1'b1;
                        ext_mod    <= nib;
                        cpu_state  <= CPU_FETCH;
                    end

                    // --- SYS family ---
                    else if (fam == FAM_SYS) begin
                        ext_active <= 1'b0;
                        case (nib)
                            4'h0: cpu_state <= CPU_HALT;  // IDL
                            4'h1: cpu_state <= CPU_FETCH;  // NOP
                            4'h2: cpu_state <= CPU_HALT;  // HALT
                            4'hD: begin  // CALL.L Rn
                                // Push return address onto SP stack
                                R[spsel] <= R[spsel] - 64'd8;
                                effective_addr <= R[spsel] - 64'd8;
                                mem_data <= R[psel];  // return addr (already advanced)
                                // Jump to address in Rn
                                R[psel] <= R[ibuf[1][3:0]];
                                cpu_state <= CPU_MEM_WRITE;
                            end
                            4'hE: begin  // RET.L
                                // Pop return address from SP stack
                                effective_addr <= R[spsel];
                                R[spsel] <= R[spsel] + 64'd8;
                                cpu_state <= CPU_MEM_READ;
                                dst_reg <= psel;  // load into PC
                            end
                            4'h4: begin  // RTI
                                // Pop flags + PC from stack
                                effective_addr <= R[spsel];
                                R[spsel] <= R[spsel] + 64'd8;
                                cpu_state <= CPU_MEM_READ;
                                dst_reg <= psel;
                                // TODO: also restore flags
                            end
                            4'hF: begin  // TRAP
                                // Software interrupt
                                // Push PC + flags onto stack, jump to IVT[6]
                                R[spsel] <= R[spsel] - 64'd8;
                                effective_addr <= R[spsel] - 64'd8;
                                mem_data <= R[psel];
                                flags[6] <= 1'b0;  // disable interrupts
                                cpu_state <= CPU_MEM_WRITE;
                                // After push, load IVT vector (simplified)
                            end
                            default: cpu_state <= CPU_FETCH;
                        endcase
                    end

                    // --- INC / DEC ---
                    else if (fam == FAM_INC) begin
                        ext_active <= 1'b0;
                        R[nib] <= R[nib] + 64'd1;
                        cpu_state <= CPU_FETCH;
                    end
                    else if (fam == FAM_DEC) begin
                        ext_active <= 1'b0;
                        R[nib] <= R[nib] - 64'd1;
                        cpu_state <= CPU_FETCH;
                    end

                    // --- BR (short branch, 8-bit signed offset) ---
                    else if (fam == FAM_BR) begin
                        ext_active <= 1'b0;
                        if (cond_eval(nib, flags)) begin
                            R[psel] <= R[psel] + {{56{ibuf[1][7]}}, ibuf[1]}
                                       - {60'd0, ibuf_len};  // relative to instr start
                        end
                        cpu_state <= CPU_FETCH;
                    end

                    // --- LBR (long branch, 16-bit signed offset) ---
                    else if (fam == FAM_LBR) begin
                        ext_active <= 1'b0;
                        if (cond_eval(nib, flags)) begin
                            R[psel] <= R[psel] + {{48{ibuf[1][7]}}, ibuf[1], ibuf[2]}
                                       - {60'd0, ibuf_len};
                        end
                        cpu_state <= CPU_FETCH;
                    end

                    // --- ALU (register-register) ---
                    else if (fam == FAM_ALU) begin
                        ext_active <= 1'b0;
                        dst_reg <= ibuf[1][7:4];
                        src_reg <= ibuf[1][3:0];
                        alu_a <= R[ibuf[1][7:4]];
                        alu_b <= R[ibuf[1][3:0]];
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
                            default: alu_op <= ALU_ADD;
                        endcase
                        cpu_state <= CPU_EXECUTE;
                    end

                    // --- IMM family (LDI, ADDI, etc.) ---
                    else if (fam == FAM_IMM) begin
                        dst_reg <= ibuf[1][7:4];
                        if (ext_active && ext_mod == 4'd0) begin
                            // EXT.IMM64: 64-bit immediate load
                            R[ibuf[1][7:4]] <= {ibuf[9], ibuf[8], ibuf[7], ibuf[6],
                                                 ibuf[5], ibuf[4], ibuf[3], ibuf[2]};
                            ext_active <= 1'b0;
                        end else begin
                            ext_active <= 1'b0;
                            case (nib)
                                4'h0: begin  // LDI Rn, imm8
                                    R[ibuf[1][7:4]] <= {56'd0, ibuf[2]};
                                end
                                4'h1: begin  // LHI Rn, imm16
                                    R[ibuf[1][7:4]] <= {48'd0, ibuf[3], ibuf[2]};
                                end
                                4'h2: begin  // ADDI
                                    alu_a <= R[ibuf[1][7:4]];
                                    alu_b <= {56'd0, ibuf[2]};
                                    alu_op <= ALU_ADD;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h3: begin  // ANDI
                                    alu_a <= R[ibuf[1][7:4]];
                                    alu_b <= {56'd0, ibuf[2]};
                                    alu_op <= ALU_AND;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h4: begin  // ORI
                                    alu_a <= R[ibuf[1][7:4]];
                                    alu_b <= {56'd0, ibuf[2]};
                                    alu_op <= ALU_OR;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h5: begin  // XORI
                                    alu_a <= R[ibuf[1][7:4]];
                                    alu_b <= {56'd0, ibuf[2]};
                                    alu_op <= ALU_XOR;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h6: begin  // CMPI
                                    alu_a <= R[ibuf[1][7:4]];
                                    alu_b <= {56'd0, ibuf[2]};
                                    alu_op <= ALU_CMP;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h7: begin  // SUBI
                                    alu_a <= R[ibuf[1][7:4]];
                                    alu_b <= {56'd0, ibuf[2]};
                                    alu_op <= ALU_SUB;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                default: ; // GLO, GHI, PLO, PHI — 1802 compat
                            endcase
                        end
                        if (nib == 4'h0 || nib == 4'h1 ||
                            (ext_active && ext_mod == 4'd0))
                            cpu_state <= CPU_FETCH;
                    end

                    // --- MEM family ---
                    else if (fam == FAM_MEM) begin
                        ext_active <= 1'b0;
                        dst_reg <= ibuf[1][7:4];
                        src_reg <= ibuf[1][3:0];
                        effective_addr <= R[ibuf[1][3:0]] +
                                          {{56{ibuf[2][7]}}, ibuf[2]};
                        case (nib)
                            4'h4, 4'h7, 4'h9, 4'hB: begin  // STR, ST.B, ST.H, ST.W
                                mem_data <= R[ibuf[1][7:4]];
                                bus_size <= (nib == 4'h7) ? BUS_BYTE :
                                            (nib == 4'h9) ? BUS_HALF :
                                            (nib == 4'hB) ? BUS_WORD : BUS_DWORD;
                                cpu_state <= CPU_MEM_WRITE;
                            end
                            default: begin  // Loads
                                bus_size <= (nib == 4'h6) ? BUS_BYTE :
                                            (nib == 4'h8) ? BUS_HALF :
                                            (nib == 4'hA) ? BUS_WORD : BUS_DWORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                        endcase
                    end

                    // --- SEP / SEX ---
                    else if (fam == FAM_SEP) begin
                        ext_active <= 1'b0;
                        psel <= nib;
                        cpu_state <= CPU_FETCH;
                    end
                    else if (fam == FAM_SEX) begin
                        ext_active <= 1'b0;
                        xsel <= nib;
                        cpu_state <= CPU_FETCH;
                    end

                    // --- IO ---
                    else if (fam == FAM_IO) begin
                        ext_active <= 1'b0;
                        if (nib[3]) begin
                            // INP: read from MMIO, store in D and M(R(X))
                            effective_addr <= {MMIO_HI, 20'd0, nib[2:0], 9'd0};
                            cpu_state <= CPU_MEM_READ;
                            dst_reg <= 4'd0;  // temp — D register path
                        end else if (nib != 4'd0) begin
                            // OUT: write D to MMIO
                            effective_addr <= {MMIO_HI, 20'd0, nib[2:0], 9'd0};
                            mem_data <= {56'd0, D};
                            cpu_state <= CPU_MEM_WRITE;
                        end else begin
                            cpu_state <= CPU_FETCH;  // OUT 0 = IRX (no-op here)
                        end
                    end

                    // --- CSR ---
                    else if (fam == FAM_CSR) begin
                        ext_active <= 1'b0;
                        if (nib[3]) begin
                            // CSRW: write GPR to CSR
                            csr_wen   <= 1'b1;
                            csr_addr  <= ibuf[1];
                            csr_wdata <= R[ibuf[2][3:0]];
                            // Also handle local CSRs
                            case (ibuf[1])
                                CSR_PSEL:    psel     <= ibuf[2][3:0];
                                CSR_XSEL:    xsel     <= ibuf[2][3:0];
                                CSR_SPSEL:   spsel    <= ibuf[2][3:0];
                                CSR_FLAGS:   flags    <= R[ibuf[2][3:0]][7:0];
                                CSR_IVTBASE: ivt_base <= R[ibuf[2][3:0]];
                            endcase
                        end else begin
                            // CSRR: read CSR to GPR
                            case (ibuf[1])
                                CSR_PSEL:    R[ibuf[2][7:4]] <= {60'd0, psel};
                                CSR_XSEL:    R[ibuf[2][7:4]] <= {60'd0, xsel};
                                CSR_SPSEL:   R[ibuf[2][7:4]] <= {60'd0, spsel};
                                CSR_FLAGS:   R[ibuf[2][7:4]] <= {56'd0, flags};
                                CSR_IVTBASE: R[ibuf[2][7:4]] <= ivt_base;
                                CSR_COREID:  R[ibuf[2][7:4]] <= {62'd0, core_id};
                                CSR_NCORES:  R[ibuf[2][7:4]] <= {62'd0, NUM_CORES[1:0]};
                                default:     R[ibuf[2][7:4]] <= csr_rdata;
                            endcase
                        end
                        cpu_state <= CPU_FETCH;
                    end

                    // --- MULDIV ---
                    else if (fam == FAM_MULDIV) begin
                        ext_active <= 1'b0;
                        dst_reg <= ibuf[1][7:4];
                        src_reg <= ibuf[1][3:0];
                        case (nib)
                            4'h0: begin  // MUL (signed lower 64 bits)
                                mul_result <= $signed(R[ibuf[1][7:4]]) *
                                              $signed(R[ibuf[1][3:0]]);
                                R[ibuf[1][7:4]] <= mul_result[63:0];
                            end
                            4'h4: begin  // DIV (signed)
                                if (R[ibuf[1][3:0]] != 64'd0) begin
                                    R[ibuf[1][7:4]] <= $signed(R[ibuf[1][7:4]]) /
                                                        $signed(R[ibuf[1][3:0]]);
                                end
                                // else: div-by-zero trap (simplified)
                            end
                            4'h6: begin  // MOD
                                if (R[ibuf[1][3:0]] != 64'd0) begin
                                    R[ibuf[1][7:4]] <= $signed(R[ibuf[1][7:4]]) %
                                                        $signed(R[ibuf[1][3:0]]);
                                end
                            end
                            default: ;
                        endcase
                        cpu_state <= CPU_FETCH;
                    end

                    // --- MEX (tile engine) ---
                    else if (fam == FAM_MEX) begin
                        ext_active <= 1'b0;
                        mex_valid   <= 1'b1;
                        mex_ss      <= ibuf[0][3:2];
                        mex_op      <= ibuf[0][1:0];
                        mex_funct   <= ibuf[1][2:0];
                        mex_gpr_val <= (ibuf[0][3:2] == 2'd1) ? R[ibuf[2][3:0]] : 64'd0;
                        mex_imm8    <= ibuf[2];
                        cpu_state   <= CPU_MEX_WAIT;
                    end

                    else begin
                        ext_active <= 1'b0;
                        cpu_state  <= CPU_FETCH;  // unknown — skip
                    end
                end

                // ============================================================
                // EXECUTE: ALU writeback
                // ============================================================
                CPU_EXECUTE: begin
                    if (alu_op != ALU_CMP)  // CMP doesn't write result
                        R[dst_reg] <= alu_result;
                    flags <= alu_flags_out;
                    cpu_state <= CPU_FETCH;
                end

                // ============================================================
                // MEM_READ: wait for bus, write result to register
                // ============================================================
                CPU_MEM_READ: begin
                    bus_valid <= 1'b1;
                    bus_addr  <= effective_addr;
                    bus_wen   <= 1'b0;
                    if (bus_ready) begin
                        R[dst_reg] <= bus_rdata;
                        cpu_state  <= CPU_FETCH;
                    end
                end

                // ============================================================
                // MEM_WRITE: wait for bus to accept write
                // ============================================================
                CPU_MEM_WRITE: begin
                    bus_valid <= 1'b1;
                    bus_addr  <= effective_addr;
                    bus_wdata <= mem_data;
                    bus_wen   <= 1'b1;
                    if (bus_ready) begin
                        cpu_state <= CPU_FETCH;
                    end
                end

                // ============================================================
                // MEX_WAIT: wait for tile engine to finish
                // ============================================================
                CPU_MEX_WAIT: begin
                    if (mex_done) begin
                        cpu_state <= CPU_FETCH;
                    end
                end

                // ============================================================
                // IRQ: vectored interrupt entry
                // ============================================================
                CPU_IRQ: begin
                    // Push PC onto stack
                    R[spsel] <= R[spsel] - 64'd8;
                    effective_addr <= R[spsel] - 64'd8;
                    mem_data <= R[psel];
                    flags[6] <= 1'b0;  // disable interrupts
                    // Load vector address from IVT
                    // (simplified: would need two memory ops — push then load)
                    R[psel] <= ivt_base + {58'd0, irq_vector, 3'b000};
                    cpu_state <= CPU_MEM_WRITE;  // push return address
                end

                // ============================================================
                // HALT: wait for interrupt (WFI behaviour)
                // ============================================================
                CPU_HALT: begin
                    // Halted — any interrupt (including IPI) wakes us
                    if (irq_pending) begin
                        cpu_state <= CPU_IRQ;
                    end
                end

            endcase
        end
    end

endmodule
