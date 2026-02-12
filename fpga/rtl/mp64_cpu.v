// ============================================================================
// mp64_cpu.v — Megapad-64 CPU Core  (pipelined with I-cache)
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
//   - 8-bit D accumulator, Q flip-flop, T register (1802 heritage)
//   - Full MEMALU family (16 ops on D via M(R(X)))
//   - Vectored interrupts (8+ vectors, IVT base in CSR)
//   - CSR space for tile engine, interrupt, multicore, and system config
//
// Pipeline: 2-stage decoupled fetch pipeline with I-cache.
//   - IF stage: reads 8 bytes per cycle from I-cache into a 16-byte
//     instruction alignment buffer.  Runs ahead of execute.
//   - DEX stage: decode + execute from the alignment buffer.  Same FSM
//     as the original prototype but consumes pre-fetched bytes.
//   - During multi-cycle execute (MEM, MEX, MULDIV), IF continues
//     prefetching the next instruction bytes.
//   - Branches / SEP / interrupts flush the prefetch buffer.
//
// Performance vs. prototype:
//   - I-cache hit: ~1 cycle per 8 fetched bytes (vs. 8 cycles)
//   - Simple instructions: 1 cycle throughput (fetch is free)
//   - Branches: 1–2 cycle bubble (flush + refill)
//

`include "mp64_defs.vh"

module mp64_cpu (
    input  wire        clk,
    input  wire        rst_n,

    // === Core identification (set per instance by SoC) ===
    input  wire [CORE_ID_BITS-1:0] core_id,

    // === Instruction cache interface (fetch path) ===
    output reg  [63:0] icache_addr,
    output reg         icache_req,
    input  wire [63:0] icache_data,
    input  wire        icache_hit,
    input  wire        icache_stall,
    output reg         icache_inv_all,     // invalidate entire I-cache
    output reg         icache_inv_line,    // invalidate single line
    output reg  [63:0] icache_inv_addr,    // address for line invalidation

    // === Data bus master (load/store/MMIO only) ===
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
    output reg  [3:0]  mex_ext_mod,
    output reg         mex_ext_active,
    input  wire        mex_done,
    input  wire        mex_busy,

    // === Interrupts ===
    input  wire        irq_timer,
    input  wire        irq_uart,
    input  wire        irq_nic,
    input  wire        irq_ipi,       // inter-processor interrupt

    // === External flags (EF1-EF4, directly from pins) ===
    input  wire [3:0]  ef_flags
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
    wire [63:0] RX = R[xsel];      // data pointer R(X)

    // IVT base address (CSR)
    reg [63:0] ivt_base;

    // 1802-heritage special registers
    reg [7:0]  D;          // 8-bit D accumulator
    reg        Q;          // Q flip-flop (output)
    reg [7:0]  T;          // T register (saved XSEL|PSEL on MARK)

    // Trap / interrupt context
    reg [7:0]  ivec_id;    // current interrupt vector ID
    reg [63:0] trap_addr;  // faulting address

    // Performance counters (per-core)
    reg [63:0] perf_cycles;     // total clock cycles
    reg [63:0] perf_stalls;     // stall cycles (waiting for bus/memory)
    reg [63:0] perf_tileops;    // tile engine operations completed
    reg [63:0] perf_extmem;     // external memory beats
    reg        perf_enable;     // counting enabled

    // Memory BIST state (CSR-accessible, stub in RTL — real BIST FSM is separate)
    reg  [1:0] bist_status;     // 0=idle, 1=running, 2=pass, 3=fail
    reg [63:0] bist_fail_addr;  // first failing address
    reg [63:0] bist_fail_data;  // expected vs actual (packed)

    // Tile datapath self-test state
    reg  [1:0] tile_selftest;   // 0=idle, 2=pass, 3=fail
    reg  [7:0] tile_st_detail;  // bitmask of failed sub-tests

    // EXT prefix modifier
    reg [3:0]  ext_mod;
    reg        ext_active;

    // I-cache control CSR state
    reg        icache_enabled;        // I-cache enable (CSR writable)

    // ========================================================================
    // Instruction prefetch buffer (filled from I-cache, 8 bytes at a time)
    // ========================================================================
    reg [7:0]  ibuf [0:15];    // 16-byte instruction alignment buffer
    reg [4:0]  ibuf_len;       // bytes currently in buffer (0–16)
    reg [3:0]  ibuf_need;      // bytes needed for current instruction

    // Fetch PC (shadow — runs ahead of architectural PC during prefetch)
    reg [63:0] fetch_pc;
    reg        fetch_active;   // 1 = fetch unit is running

    // Decoded instruction fields
    wire [3:0] fam  = ibuf[0][7:4];   // family
    wire [3:0] nib  = ibuf[0][3:0];   // sub-selector

    // ========================================================================
    // CPU state machine
    // ========================================================================
    localparam CPU_FETCH      = 4'd0;
    localparam CPU_DECODE     = 4'd1;
    localparam CPU_EXECUTE    = 4'd2;
    localparam CPU_MEM_READ   = 4'd3;
    localparam CPU_MEM_WRITE  = 4'd4;
    localparam CPU_MEX_WAIT   = 4'd5;
    localparam CPU_IRQ        = 4'd6;
    localparam CPU_HALT       = 4'd7;
    // CPU_FETCH_MORE removed — replaced by I-cache prefetch buffer
    localparam CPU_MULDIV     = 4'd9;
    localparam CPU_MEM_READ2  = 4'd10;   // second read (RTI flags pop)
    localparam CPU_IRQ_PUSH   = 4'd11;   // push flags in IRQ sequence
    localparam CPU_IRQ_LOAD   = 4'd12;   // load IVT vector from memory
    localparam CPU_MEMALU_RD  = 4'd13;   // MEMALU: reading M(R(X))
    localparam CPU_MEMALU_WB  = 4'd14;   // MEMALU: write-back (IO OUT read)
    localparam CPU_SKIP       = 4'd15;   // SKIP: fetch next instr byte for length

    reg [3:0]  cpu_state;

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
    // Condition code evaluation (for branches)
    // ========================================================================
    function cond_eval;
        input [3:0] cond;
        input [7:0] f;
        input       q_val;
        input [3:0] ef_val;
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
                4'hB: cond_eval = q_val;           // BQ  (Q=1)
                4'hC: cond_eval = !q_val;          // BNQ (Q=0)
                4'hD: cond_eval = f[7];            // SAT (S=1)
                4'hE: cond_eval = |ef_val;         // EF  (any EF set)
                4'hF: cond_eval = 1'b0;            // NV (never)
                default: cond_eval = 1'b0;
            endcase
        end
    endfunction

    // ========================================================================
    // Parity (even parity of low 8 bits)
    // ========================================================================
    function parity8;
        input [63:0] val;
        reg [7:0] b;
        begin
            b = val[7:0];
            b = b ^ (b >> 4);
            b = b ^ (b >> 2);
            b = b ^ (b >> 1);
            parity8 = ~b[0];   // 1 = even number of 1-bits
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
    localparam ALU_ROL = 4'd14;
    localparam ALU_ROR = 4'd15;

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
                alu_flags_out[3] = (~(alu_a[63] ^ alu_b[63])) &
                                   (alu_a[63] ^ alu_wide[63]);  // V
            end
            ALU_ADC: begin
                alu_wide = {1'b0, alu_a} + {1'b0, alu_b} + {64'd0, flag_c};
                alu_result = alu_wide[63:0];
                alu_flags_out[1] = alu_wide[64];
                alu_flags_out[3] = (~(alu_a[63] ^ alu_b[63])) &
                                   (alu_a[63] ^ alu_wide[63]);
            end
            ALU_SUB, ALU_CMP: begin
                alu_wide = {1'b0, alu_a} - {1'b0, alu_b};
                alu_result = alu_wide[63:0];
                // C=1 means no borrow (a >= b unsigned)
                alu_flags_out[1] = (alu_a >= alu_b) ? 1'b1 : 1'b0;
                alu_flags_out[3] = (alu_a[63] ^ alu_b[63]) &
                                   (alu_a[63] ^ alu_result[63]);  // V
                alu_flags_out[5] = (alu_a > alu_b) ? 1'b1 : 1'b0; // G
            end
            ALU_SBB: begin
                alu_wide = {1'b0, alu_a} - {1'b0, alu_b} - {64'd0, ~flag_c};
                alu_result = alu_wide[63:0];
                alu_flags_out[1] = !alu_wide[64];
            end
            ALU_AND: begin
                alu_result = alu_a & alu_b;
                alu_flags_out[1] = 1'b0;  // logic: clear C
                alu_flags_out[3] = 1'b0;  // logic: clear V
            end
            ALU_OR: begin
                alu_result = alu_a | alu_b;
                alu_flags_out[1] = 1'b0;
                alu_flags_out[3] = 1'b0;
            end
            ALU_XOR: begin
                alu_result = alu_a ^ alu_b;
                alu_flags_out[1] = 1'b0;
                alu_flags_out[3] = 1'b0;
            end
            ALU_MOV: alu_result = alu_b;
            ALU_NOT: begin
                alu_result = ~alu_b;
                alu_flags_out[1] = 1'b0;
                alu_flags_out[3] = 1'b0;
            end
            ALU_NEG: begin
                alu_result = (~alu_b) + 64'd1;
                alu_flags_out[1] = (alu_b != 64'd0) ? 1'b1 : 1'b0;
            end
            ALU_SHL: begin
                alu_result = alu_a << alu_b[5:0];
                // carry = last bit shifted out
                alu_flags_out[1] = (alu_b[5:0] != 0) ?
                    alu_a[64 - alu_b[5:0]] : 1'b0;
            end
            ALU_SHR: begin
                alu_result = alu_a >> alu_b[5:0];
                alu_flags_out[1] = (alu_b[5:0] != 0) ?
                    alu_a[alu_b[5:0] - 1] : 1'b0;
            end
            ALU_SAR: begin
                alu_result = $signed(alu_a) >>> alu_b[5:0];
                alu_flags_out[1] = (alu_b[5:0] != 0) ?
                    alu_a[alu_b[5:0] - 1] : 1'b0;
            end
            ALU_ROL: begin
                if (alu_b[5:0] != 0)
                    alu_result = (alu_a << alu_b[5:0]) |
                                 (alu_a >> (7'd64 - {1'b0, alu_b[5:0]}));
                else
                    alu_result = alu_a;
            end
            ALU_ROR: begin
                if (alu_b[5:0] != 0)
                    alu_result = (alu_a >> alu_b[5:0]) |
                                 (alu_a << (7'd64 - {1'b0, alu_b[5:0]}));
                else
                    alu_result = alu_a;
            end
            default: alu_result = 64'd0;
        endcase

        // Common flags for all ALU ops
        alu_flags_out[0] = (alu_result == 64'd0);             // Z
        alu_flags_out[2] = alu_result[63];                    // N
        alu_flags_out[4] = parity8(alu_result);               // P
    end

    // ========================================================================
    // Instruction length decoder
    // ========================================================================
    function [3:0] instr_len;
        input [7:0] byte0;
        input       has_ext;
        begin
            case (byte0[7:4])
                FAM_SYS:   instr_len = (byte0[3:0] == 4'hD) ? 4'd2  // CALL.L Rn
                                      : 4'd1;
                FAM_INC:   instr_len = 4'd1;
                FAM_DEC:   instr_len = 4'd1;
                FAM_BR:    instr_len = 4'd2; // always 2 bytes even in SKIP mode
                FAM_LBR:   instr_len = 4'd3;
                FAM_MEM:   instr_len = (byte0[3:0] == 4'hF) ? 4'd3  // LD.D+off
                                      : 4'd2;
                FAM_IMM:   instr_len = (byte0[3:0] == 4'h1) ? 4'd4  // LHI imm16
                                      : (has_ext) ? 4'd10            // EXT.IMM64
                                      : (byte0[3:0] >= 4'h8) ? 4'd2 // shifts, GLO/GHI/PLO/PHI
                                      : 4'd3;                        // LDI/ADDI/etc imm8
                FAM_ALU:   instr_len = 4'd2;
                FAM_MEMALU:instr_len = 4'd1;
                FAM_IO:    instr_len = 4'd1;
                FAM_SEP:   instr_len = 4'd1;
                FAM_SEX:   instr_len = 4'd1;
                FAM_MULDIV:instr_len = 4'd2;
                FAM_CSR:   instr_len = 4'd2;  // opcode + CSR addr
                FAM_MEX:   instr_len = (byte0[3:2] == 2'd1) ? 4'd3 : 4'd2;
                FAM_EXT:   instr_len = 4'd1;
                default:   instr_len = 4'd1;
            endcase
        end
    endfunction

    // ========================================================================
    // Temporary registers for multi-cycle operations
    // ========================================================================
    reg [63:0] mem_data;
    reg [3:0]  dst_reg;
    reg [3:0]  src_reg;
    reg [63:0] effective_addr;
    reg [3:0]  mem_sub;         // sub-opcode for MEM writeback

    // Multiply/divide
    reg [127:0] mul_result;

    // MEMALU temp
    reg [7:0]  memalu_byte;     // byte read from M(R(X))
    reg [3:0]  memalu_sub;      // which MEMALU sub-op

    // IO temp
    reg [2:0]  io_port;
    reg        io_is_inp;

    // Post-operation action codes
    localparam POST_NONE     = 3'd0;
    localparam POST_RTI_POP2 = 3'd1;  // after popping PC, pop flags
    localparam POST_IRQ_VEC  = 3'd2;  // after pushing, load IVT vector
    reg [2:0]  post_action;

    // ========================================================================
    // Main state machine
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            // Reset state
            cpu_state  <= CPU_FETCH;
            bus_valid  <= 1'b0;
            csr_wen    <= 1'b0;
            mex_valid  <= 1'b0;
            ext_active <= 1'b0;
            ext_mod    <= 4'd0;
            fetch_active <= 1'b1;
            fetch_pc   <= 64'd0;
            ibuf_len   <= 5'd0;
            ibuf_need  <= 4'd1;

            // I-cache control
            icache_enabled  <= 1'b1;
            icache_inv_all  <= 1'b0;
            icache_inv_line <= 1'b0;
            icache_inv_addr <= 64'd0;
            icache_req      <= 1'b0;

            // Default register setup
            psel  <= 4'd3;
            xsel  <= 4'd2;
            spsel <= 4'd15;
            flags <= 8'h40;    // I=1, rest clear

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

            // Performance counters
            perf_cycles  <= 64'd0;
            perf_stalls  <= 64'd0;
            perf_tileops <= 64'd0;
            perf_extmem  <= 64'd0;
            perf_enable  <= 1'b1;  // enabled by default

            // BIST registers
            bist_status    <= 2'd0;
            bist_fail_addr <= 64'd0;
            bist_fail_data <= 64'd0;

            // Tile self-test registers
            tile_selftest  <= 2'd0;
            tile_st_detail <= 8'd0;

            // Clear register file
            R[0]  <= 64'd0;  R[1]  <= 64'd0;  R[2]  <= 64'd0;  R[3]  <= 64'd0;
            R[4]  <= 64'd0;  R[5]  <= 64'd0;  R[6]  <= 64'd0;  R[7]  <= 64'd0;
            R[8]  <= 64'd0;  R[9]  <= 64'd0;  R[10] <= 64'd0;  R[11] <= 64'd0;
            R[12] <= 64'd0;  R[13] <= 64'd0;  R[14] <= 64'd0;  R[15] <= 64'd0;

        end else begin
            bus_valid <= 1'b0;
            csr_wen   <= 1'b0;
            mex_valid <= 1'b0;

            // Clear one-shot invalidation pulses each cycle
            icache_inv_all  <= 1'b0;
            icache_inv_line <= 1'b0;

            // Performance counter increments (every cycle when enabled)
            if (perf_enable) begin
                perf_cycles <= perf_cycles + 64'd1;
                // Stall: waiting for bus or I-cache miss
                if ((cpu_state == CPU_FETCH   && icache_stall) ||
                    (cpu_state == CPU_MEM_READ  && !bus_ready) ||
                    (cpu_state == CPU_MEM_WRITE && !bus_ready) ||
                    (cpu_state == CPU_MEM_READ2 && !bus_ready) ||
                    (cpu_state == CPU_MEMALU_RD && !bus_ready) ||
                    (cpu_state == CPU_MEMALU_WB && !bus_ready) ||
                    (cpu_state == CPU_IRQ_PUSH  && !bus_ready) ||
                    (cpu_state == CPU_IRQ_LOAD  && !bus_ready))
                    perf_stalls <= perf_stalls + 64'd1;
                // Tile ops: count completed MEX instructions
                if (cpu_state == CPU_MEX_WAIT && mex_done)
                    perf_tileops <= perf_tileops + 64'd1;
            end

            case (cpu_state)

                // ============================================================
                // FETCH: fill instruction buffer from I-cache, then decode
                //
                // The I-cache delivers 8 bytes per hit.  We request from
                // fetch_pc (which tracks the architectural PC when the
                // buffer is empty) and shift data into ibuf.  When ibuf
                // holds enough bytes for the current instruction, we
                // move to DECODE.
                // ============================================================
                CPU_FETCH: begin
                    if (irq_pending && ibuf_len == 5'd0) begin
                        cpu_state <= CPU_IRQ;
                    end else if (ibuf_len >= {1'b0, ibuf_need}) begin
                        // Buffer has a complete instruction — decode it
                        cpu_state <= CPU_DECODE;
                    end else begin
                        // Need more bytes — request from I-cache
                        if (!icache_stall) begin
                            icache_req  <= 1'b1;
                            icache_addr <= fetch_pc;
                        end

                        // I-cache hit: copy up to 8 bytes into ibuf
                        if (icache_hit) begin
                            icache_req <= 1'b0;

                            // Byte offset within the 8-byte fetch word
                            // (low 3 bits of fetch_pc determine alignment)
                            case (fetch_pc[2:0])
                                3'd0: begin
                                    ibuf[ibuf_len+0] <= icache_data[ 7: 0];
                                    ibuf[ibuf_len+1] <= icache_data[15: 8];
                                    ibuf[ibuf_len+2] <= icache_data[23:16];
                                    ibuf[ibuf_len+3] <= icache_data[31:24];
                                    ibuf[ibuf_len+4] <= icache_data[39:32];
                                    ibuf[ibuf_len+5] <= icache_data[47:40];
                                    ibuf[ibuf_len+6] <= icache_data[55:48];
                                    ibuf[ibuf_len+7] <= icache_data[63:56];
                                    ibuf_len <= ibuf_len + 5'd8;
                                    fetch_pc <= (fetch_pc & ~64'd7) + 64'd8;
                                end
                                3'd1: begin
                                    ibuf[ibuf_len+0] <= icache_data[15: 8];
                                    ibuf[ibuf_len+1] <= icache_data[23:16];
                                    ibuf[ibuf_len+2] <= icache_data[31:24];
                                    ibuf[ibuf_len+3] <= icache_data[39:32];
                                    ibuf[ibuf_len+4] <= icache_data[47:40];
                                    ibuf[ibuf_len+5] <= icache_data[55:48];
                                    ibuf[ibuf_len+6] <= icache_data[63:56];
                                    ibuf_len <= ibuf_len + 5'd7;
                                    fetch_pc <= (fetch_pc & ~64'd7) + 64'd8;
                                end
                                3'd2: begin
                                    ibuf[ibuf_len+0] <= icache_data[23:16];
                                    ibuf[ibuf_len+1] <= icache_data[31:24];
                                    ibuf[ibuf_len+2] <= icache_data[39:32];
                                    ibuf[ibuf_len+3] <= icache_data[47:40];
                                    ibuf[ibuf_len+4] <= icache_data[55:48];
                                    ibuf[ibuf_len+5] <= icache_data[63:56];
                                    ibuf_len <= ibuf_len + 5'd6;
                                    fetch_pc <= (fetch_pc & ~64'd7) + 64'd8;
                                end
                                3'd3: begin
                                    ibuf[ibuf_len+0] <= icache_data[31:24];
                                    ibuf[ibuf_len+1] <= icache_data[39:32];
                                    ibuf[ibuf_len+2] <= icache_data[47:40];
                                    ibuf[ibuf_len+3] <= icache_data[55:48];
                                    ibuf[ibuf_len+4] <= icache_data[63:56];
                                    ibuf_len <= ibuf_len + 5'd5;
                                    fetch_pc <= (fetch_pc & ~64'd7) + 64'd8;
                                end
                                3'd4: begin
                                    ibuf[ibuf_len+0] <= icache_data[39:32];
                                    ibuf[ibuf_len+1] <= icache_data[47:40];
                                    ibuf[ibuf_len+2] <= icache_data[55:48];
                                    ibuf[ibuf_len+3] <= icache_data[63:56];
                                    ibuf_len <= ibuf_len + 5'd4;
                                    fetch_pc <= (fetch_pc & ~64'd7) + 64'd8;
                                end
                                3'd5: begin
                                    ibuf[ibuf_len+0] <= icache_data[47:40];
                                    ibuf[ibuf_len+1] <= icache_data[55:48];
                                    ibuf[ibuf_len+2] <= icache_data[63:56];
                                    ibuf_len <= ibuf_len + 5'd3;
                                    fetch_pc <= (fetch_pc & ~64'd7) + 64'd8;
                                end
                                3'd6: begin
                                    ibuf[ibuf_len+0] <= icache_data[55:48];
                                    ibuf[ibuf_len+1] <= icache_data[63:56];
                                    ibuf_len <= ibuf_len + 5'd2;
                                    fetch_pc <= (fetch_pc & ~64'd7) + 64'd8;
                                end
                                3'd7: begin
                                    ibuf[ibuf_len+0] <= icache_data[63:56];
                                    ibuf_len <= ibuf_len + 5'd1;
                                    fetch_pc <= (fetch_pc & ~64'd7) + 64'd8;
                                end
                            endcase

                            // After the first byte arrives, determine instruction length
                            if (ibuf_len == 5'd0) begin
                                // byte0 just loaded — compute length on the fly
                                // (use icache_data aligned by fetch_pc low bits)
                                case (fetch_pc[2:0])
                                    3'd0: ibuf_need <= instr_len(icache_data[ 7:0], ext_active);
                                    3'd1: ibuf_need <= instr_len(icache_data[15:8], ext_active);
                                    3'd2: ibuf_need <= instr_len(icache_data[23:16], ext_active);
                                    3'd3: ibuf_need <= instr_len(icache_data[31:24], ext_active);
                                    3'd4: ibuf_need <= instr_len(icache_data[39:32], ext_active);
                                    3'd5: ibuf_need <= instr_len(icache_data[47:40], ext_active);
                                    3'd6: ibuf_need <= instr_len(icache_data[55:48], ext_active);
                                    3'd7: ibuf_need <= instr_len(icache_data[63:56], ext_active);
                                endcase
                            end
                        end
                    end
                end

                // ============================================================
                // DECODE + EXECUTE
                // ============================================================
                CPU_DECODE: begin
                    // Advance PC past the instruction (by instr length, not buffer size)
                    R[psel] <= R[psel] + {60'd0, ibuf_need};
                    // Flush prefetch buffer — restart fetch from new PC
                    ibuf_len  <= 5'd0;
                    ibuf_need <= 4'd1;
                    fetch_pc  <= R[psel] + {60'd0, ibuf_need};
                    icache_req <= 1'b0;
                    post_action <= POST_NONE;

                    // --------------------------------------------------------
                    // EXT prefix
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
                            4'h0: // IDL — idle until interrupt
                                cpu_state <= CPU_HALT;

                            4'h1: // NOP
                                cpu_state <= CPU_FETCH;

                            4'h2: // HALT
                                cpu_state <= CPU_HALT;

                            4'h3: begin // RESET — soft reset all state
                                psel  <= 4'd3;
                                xsel  <= 4'd2;
                                spsel <= 4'd15;
                                flags <= 8'h40;
                                D <= 8'd0;
                                Q <= 1'b0;
                                T <= 8'd0;
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

                            4'h4: begin // RTI — pop PC, then pop flags
                                effective_addr <= R[spsel];
                                R[spsel] <= R[spsel] + 64'd8;
                                dst_reg <= psel;
                                post_action <= POST_RTI_POP2;
                                cpu_state <= CPU_MEM_READ;
                            end

                            4'h5: begin // RET (1802) — pop T, restore X|P, IE←1
                                effective_addr <= R[spsel];
                                R[spsel] <= R[spsel] + 64'd8;
                                dst_reg <= 4'd0;  // temp: T byte goes to R0 then we unpack
                                mem_sub <= 4'h5;  // marker for 1802 RET
                                cpu_state <= CPU_MEM_READ;
                            end

                            4'h6: begin // DIS — pop T, restore X|P, IE←0
                                effective_addr <= R[spsel];
                                R[spsel] <= R[spsel] + 64'd8;
                                dst_reg <= 4'd0;
                                mem_sub <= 4'hB;  // marker for DIS (avoids LD.B sub=6 collision)
                                cpu_state <= CPU_MEM_READ;
                            end

                            4'h7: begin // MARK — T←(X|P), push T, X←P
                                T <= {xsel, psel};
                                // Push T onto stack
                                R[spsel] <= R[spsel] - 64'd8;
                                effective_addr <= R[spsel] - 64'd8;
                                mem_data <= {56'd0, xsel, psel};
                                xsel <= psel;
                                cpu_state <= CPU_MEM_WRITE;
                            end

                            4'h8: begin // SAV — M(R(X)) ← T
                                effective_addr <= R[xsel];
                                mem_data <= {56'd0, T};
                                bus_size <= BUS_BYTE;
                                cpu_state <= CPU_MEM_WRITE;
                            end

                            4'h9: begin // SEQ — Q ← 1
                                Q <= 1'b1;
                                cpu_state <= CPU_FETCH;
                            end

                            4'hA: begin // REQ — Q ← 0
                                Q <= 1'b0;
                                cpu_state <= CPU_FETCH;
                            end

                            4'hB: begin // EI — enable interrupts
                                flags[6] <= 1'b1;
                                cpu_state <= CPU_FETCH;
                            end

                            4'hC: begin // DI — disable interrupts
                                flags[6] <= 1'b0;
                                cpu_state <= CPU_FETCH;
                            end

                            4'hD: begin // CALL.L Rn — push ret addr, PC←R(n)
                                R[spsel] <= R[spsel] - 64'd8;
                                effective_addr <= R[spsel] - 64'd8;
                                mem_data <= R[psel] + {60'd0, ibuf_need};  // return addr
                                R[psel] <= R[ibuf[1][3:0]];
                                fetch_pc <= R[ibuf[1][3:0]];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_WRITE;
                            end

                            4'hE: begin // RET.L — PC ← pop64()
                                effective_addr <= R[spsel];
                                R[spsel] <= R[spsel] + 64'd8;
                                dst_reg <= psel;
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_READ;
                            end

                            4'hF: begin // TRAP — push flags+PC, IVT[6], IE←0
                                // Step 1: push PC onto stack (advanced past TRAP)
                                R[spsel] <= R[spsel] - 64'd8;
                                effective_addr <= R[spsel] - 64'd8;
                                mem_data <= R[psel] + {60'd0, ibuf_need}; // ret addr
                                flags[6] <= 1'b0;  // disable interrupts
                                ivec_id <= 8'd6;    // IVEC_SW_TRAP
                                post_action <= POST_IRQ_VEC;
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_WRITE;
                            end
                        endcase
                    end

                    // --------------------------------------------------------
                    // INC Rn (0x1)
                    // --------------------------------------------------------
                    else if (fam == FAM_INC) begin
                        ext_active <= 1'b0;
                        R[nib] <= R[nib] + 64'd1;
                        cpu_state <= CPU_FETCH;
                    end

                    // --------------------------------------------------------
                    // DEC Rn (0x2)
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
                            // SKIP mode: if condition met, skip next instr
                            if (cond_eval(nib, flags, Q, ef_flags)) begin
                                // Read next instruction's byte0 via I-cache for length
                                icache_req  <= 1'b1;
                                icache_addr <= R[psel] + {60'd0, ibuf_need};
                                cpu_state   <= CPU_SKIP;
                            end else begin
                                cpu_state <= CPU_FETCH;
                            end
                        end else begin
                            if (cond_eval(nib, flags, Q, ef_flags)) begin
                                R[psel] <= R[psel]
                                           + {{56{ibuf[1][7]}}, ibuf[1]}
                                           + {60'd0, ibuf_need};
                                fetch_pc <= R[psel]
                                            + {{56{ibuf[1][7]}}, ibuf[1]}
                                            + {60'd0, ibuf_need};
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
                                       + {60'd0, ibuf_need};
                            fetch_pc <= R[psel]
                                        + {{48{ibuf[1][7]}}, ibuf[1], ibuf[2]}
                                        + {60'd0, ibuf_need};
                        end
                        cpu_state <= CPU_FETCH;
                    end

                    // --------------------------------------------------------
                    // MEM — scalar load/store (0x5) — full 16 sub-ops
                    // --------------------------------------------------------
                    else if (fam == FAM_MEM) begin
                        ext_active <= 1'b0;
                        dst_reg <= ibuf[1][7:4];
                        src_reg <= ibuf[1][3:0];
                        mem_sub <= nib;

                        case (nib)
                            // --- 64-bit loads ---
                            4'h0: begin // LDN Rd, Rn
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'h1: begin // LDA Rd, Rn (load + advance Rs by 8)
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'h2: begin // LDX Rd (load via R(X))
                                effective_addr <= R[xsel];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'h3: begin // LDXA Rd (load via R(X), advance)
                                effective_addr <= R[xsel];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_READ;
                            end

                            // --- 64-bit store ---
                            4'h4: begin // STR Rn, Rs
                                effective_addr <= R[ibuf[1][7:4]];
                                mem_data <= R[ibuf[1][3:0]];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_WRITE;
                            end
                            4'h5: begin // STXD Rs (store via R(X), dec)
                                effective_addr <= R[xsel];
                                mem_data <= R[ibuf[1][7:4]];
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_WRITE;
                            end

                            // --- Byte load/store ---
                            4'h6: begin // LD.B Rd, Rn (zero-extend)
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_BYTE;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'h7: begin // ST.B Rn, Rs
                                effective_addr <= R[ibuf[1][7:4]];
                                mem_data <= {56'd0, R[ibuf[1][3:0]][7:0]};
                                bus_size <= BUS_BYTE;
                                cpu_state <= CPU_MEM_WRITE;
                            end

                            // --- Half-word load/store ---
                            4'h8: begin // LD.H Rd, Rn (zero-extend)
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_HALF;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'h9: begin // ST.H Rn, Rs
                                effective_addr <= R[ibuf[1][7:4]];
                                mem_data <= {48'd0, R[ibuf[1][3:0]][15:0]};
                                bus_size <= BUS_HALF;
                                cpu_state <= CPU_MEM_WRITE;
                            end

                            // --- Word load/store ---
                            4'hA: begin // LD.W Rd, Rn (zero-extend)
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_WORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'hB: begin // ST.W Rn, Rs
                                effective_addr <= R[ibuf[1][7:4]];
                                mem_data <= {32'd0, R[ibuf[1][3:0]][31:0]};
                                bus_size <= BUS_WORD;
                                cpu_state <= CPU_MEM_WRITE;
                            end

                            // --- Sign-extending loads ---
                            4'hC: begin // LD.SB Rd, Rn (sign-ext byte→64)
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_BYTE;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'hD: begin // LD.SH Rd, Rn (sign-ext half→64)
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_HALF;
                                cpu_state <= CPU_MEM_READ;
                            end
                            4'hE: begin // LD.SW Rd, Rn (sign-ext word→64)
                                effective_addr <= R[ibuf[1][3:0]];
                                bus_size <= BUS_WORD;
                                cpu_state <= CPU_MEM_READ;
                            end

                            // --- Offset load ---
                            4'hF: begin // LD.D Rd, [Rn+off8*8]
                                effective_addr <= R[ibuf[1][3:0]]
                                    + ({{56{ibuf[2][7]}}, ibuf[2]} << 3);
                                bus_size <= BUS_DWORD;
                                cpu_state <= CPU_MEM_READ;
                            end
                        endcase
                    end

                    // --------------------------------------------------------
                    // IMM family (0x6) — full 16 sub-ops
                    // --------------------------------------------------------
                    else if (fam == FAM_IMM) begin
                        dst_reg <= ibuf[1][7:4];

                        if (ext_active && ext_mod == 4'd0) begin
                            // EXT.IMM64: 64-bit immediate load
                            R[ibuf[1][7:4]] <= {ibuf[9], ibuf[8], ibuf[7], ibuf[6],
                                                 ibuf[5], ibuf[4], ibuf[3], ibuf[2]};
                            ext_active <= 1'b0;
                            cpu_state <= CPU_FETCH;
                        end else begin
                            ext_active <= 1'b0;
                            case (nib)
                                4'h0: begin // LDI Rn, imm8
                                    R[ibuf[1][7:4]] <= {56'd0, ibuf[2]};
                                    cpu_state <= CPU_FETCH;
                                end
                                4'h1: begin // LHI Rn, imm16 — set upper 16 bits
                                    R[ibuf[1][7:4]][63:48] <= {ibuf[3], ibuf[2]};
                                    cpu_state <= CPU_FETCH;
                                end
                                4'h2: begin // ADDI Rn, simm8
                                    alu_a <= R[ibuf[1][7:4]];
                                    alu_b <= {{56{ibuf[2][7]}}, ibuf[2]};
                                    alu_op <= ALU_ADD;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h3: begin // ANDI Rn, imm8
                                    alu_a <= R[ibuf[1][7:4]];
                                    alu_b <= {56'd0, ibuf[2]};
                                    alu_op <= ALU_AND;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h4: begin // ORI Rn, imm8
                                    alu_a <= R[ibuf[1][7:4]];
                                    alu_b <= {56'd0, ibuf[2]};
                                    alu_op <= ALU_OR;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h5: begin // XORI Rn, imm8
                                    alu_a <= R[ibuf[1][7:4]];
                                    alu_b <= {56'd0, ibuf[2]};
                                    alu_op <= ALU_XOR;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h6: begin // CMPI Rn, simm8
                                    alu_a <= R[ibuf[1][7:4]];
                                    alu_b <= {{56{ibuf[2][7]}}, ibuf[2]};
                                    alu_op <= ALU_CMP;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h7: begin // SUBI Rn, simm8
                                    alu_a <= R[ibuf[1][7:4]];
                                    alu_b <= {{56{ibuf[2][7]}}, ibuf[2]};
                                    alu_op <= ALU_SUB;
                                    cpu_state <= CPU_EXECUTE;
                                end
                                4'h8: begin // LSLI Rn, imm4
                                    R[ibuf[1][7:4]] <= R[ibuf[1][7:4]]
                                                        << ibuf[1][3:0];
                                    cpu_state <= CPU_FETCH;
                                end
                                4'h9: begin // LSRI Rn, imm4
                                    R[ibuf[1][7:4]] <= R[ibuf[1][7:4]]
                                                        >> ibuf[1][3:0];
                                    cpu_state <= CPU_FETCH;
                                end
                                4'hA: begin // ASRI Rn, imm4
                                    R[ibuf[1][7:4]] <= $signed(R[ibuf[1][7:4]])
                                                        >>> ibuf[1][3:0];
                                    cpu_state <= CPU_FETCH;
                                end
                                4'hB: begin // ROLI Rn, imm4
                                    if (ibuf[1][3:0] != 0)
                                        R[ibuf[1][7:4]] <=
                                            (R[ibuf[1][7:4]] << ibuf[1][3:0]) |
                                            (R[ibuf[1][7:4]] >> (4'd0 - ibuf[1][3:0]));
                                    // else: no-op (rotate by 0)
                                    cpu_state <= CPU_FETCH;
                                end
                                4'hC: begin // GLO Rn — D ← R(N)[7:0]
                                    D <= R[ibuf[1][7:4]][7:0];
                                    cpu_state <= CPU_FETCH;
                                end
                                4'hD: begin // GHI Rn — D ← R(N)[15:8]
                                    D <= R[ibuf[1][7:4]][15:8];
                                    cpu_state <= CPU_FETCH;
                                end
                                4'hE: begin // PLO Rn — R(N)[7:0] ← D
                                    R[ibuf[1][7:4]][7:0] <= D;
                                    cpu_state <= CPU_FETCH;
                                end
                                4'hF: begin // PHI Rn — R(N)[15:8] ← D
                                    R[ibuf[1][7:4]][15:8] <= D;
                                    cpu_state <= CPU_FETCH;
                                end
                            endcase
                        end
                    end

                    // --------------------------------------------------------
                    // ALU — register-register (0x7) — full 16 sub-ops
                    // --------------------------------------------------------
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
                            4'hE: alu_op <= ALU_ROL;
                            4'hF: alu_op <= ALU_ROR;
                        endcase
                        cpu_state <= CPU_EXECUTE;
                    end

                    // --------------------------------------------------------
                    // MEMALU — 1802 heritage D-register ops (0x8) — full 16
                    // --------------------------------------------------------
                    else if (fam == FAM_MEMALU) begin
                        ext_active <= 1'b0;
                        memalu_sub <= nib;

                        case (nib)
                            4'hE: begin // IRX — R(X) += 1 (no memory access)
                                R[xsel] <= R[xsel] + 64'd1;
                                cpu_state <= CPU_FETCH;
                            end
                            4'h6: begin // SHR.D — shift D right, lsb→C
                                flags[1] <= D[0];
                                D <= {1'b0, D[7:1]};
                                flags[0] <= ({1'b0, D[7:1]} == 8'd0) ? 1'b1 : 1'b0;
                                cpu_state <= CPU_FETCH;
                            end
                            4'hA: begin // SHRC.D — shift D right thru carry
                                begin
                                    reg old_c;
                                    old_c = flags[1];
                                    flags[1] <= D[0];
                                    D <= {old_c, D[7:1]};
                                    flags[0] <= ({old_c, D[7:1]} == 8'd0) ? 1'b1 : 1'b0;
                                end
                                cpu_state <= CPU_FETCH;
                            end
                            4'hC: begin // SHL.D — shift D left, msb→C
                                flags[1] <= D[7];
                                D <= {D[6:0], 1'b0};
                                flags[0] <= ({D[6:0], 1'b0} == 8'd0) ? 1'b1 : 1'b0;
                                cpu_state <= CPU_FETCH;
                            end
                            4'hD: begin // SHLC.D — shift D left thru carry
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
                                // All other MEMALU ops need to read M(R(X))
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
                            // OUT N: read M(R(X)), write to MMIO port, R(X)++
                            io_port <= nib[2:0];
                            io_is_inp <= 1'b0;
                            effective_addr <= R[xsel];
                            bus_size <= BUS_BYTE;
                            cpu_state <= CPU_MEMALU_RD; // reuse: read M(R(X))
                            memalu_sub <= 4'hF;  // special marker for IO OUT
                        end else if (nib >= 4'd9) begin
                            // INP (N-8): read from MMIO port → D and M(R(X))
                            io_port <= nib[2:0];
                            io_is_inp <= 1'b1;
                            // Read from MMIO address
                            effective_addr <= {MMIO_HI, 20'd0, nib[2:0], 9'd0};
                            bus_size <= BUS_BYTE;
                            cpu_state <= CPU_MEM_READ;
                            dst_reg <= 4'd0;  // placeholder
                            mem_sub <= 4'hF;  // marker for INP
                        end else begin
                            cpu_state <= CPU_FETCH; // OUT 0 / INP 0 = no-op
                        end
                    end

                    // --------------------------------------------------------
                    // SEP Rn (0xA)
                    // --------------------------------------------------------
                    else if (fam == FAM_SEP) begin
                        ext_active <= 1'b0;
                        psel <= nib;
                        // PC register changed — sync fetch_pc to new PC
                        fetch_pc <= R[nib];
                        cpu_state <= CPU_FETCH;
                    end

                    // --------------------------------------------------------
                    // SEX Rn (0xB)
                    // --------------------------------------------------------
                    else if (fam == FAM_SEX) begin
                        ext_active <= 1'b0;
                        xsel <= nib;
                        cpu_state <= CPU_FETCH;
                    end

                    // --------------------------------------------------------
                    // MULDIV (0xC) — full 8 sub-ops
                    // --------------------------------------------------------
                    else if (fam == FAM_MULDIV) begin
                        ext_active <= 1'b0;
                        dst_reg <= ibuf[1][7:4];
                        src_reg <= ibuf[1][3:0];

                        case (nib)
                            4'h0: begin // MUL (signed low 64)
                                mul_result <= $signed(R[ibuf[1][7:4]]) *
                                              $signed(R[ibuf[1][3:0]]);
                                cpu_state <= CPU_MULDIV;
                            end
                            4'h1: begin // MULH (signed high 64)
                                mul_result <= $signed(R[ibuf[1][7:4]]) *
                                              $signed(R[ibuf[1][3:0]]);
                                mem_sub <= 4'h1;  // marker: take high
                                cpu_state <= CPU_MULDIV;
                            end
                            4'h2: begin // UMUL (unsigned low 64)
                                mul_result <= R[ibuf[1][7:4]] *
                                              R[ibuf[1][3:0]];
                                cpu_state <= CPU_MULDIV;
                            end
                            4'h3: begin // UMULH (unsigned high 64)
                                mul_result <= R[ibuf[1][7:4]] *
                                              R[ibuf[1][3:0]];
                                mem_sub <= 4'h1;
                                cpu_state <= CPU_MULDIV;
                            end
                            4'h4: begin // DIV (signed)
                                if (R[ibuf[1][3:0]] == 64'd0) begin
                                    // divide-by-zero trap
                                    ivec_id <= 8'd4;
                                    cpu_state <= CPU_IRQ;
                                end else begin
                                    R[ibuf[1][7:4]] <=
                                        $signed(R[ibuf[1][7:4]]) /
                                        $signed(R[ibuf[1][3:0]]);
                                    R[0] <=
                                        $signed(R[ibuf[1][7:4]]) %
                                        $signed(R[ibuf[1][3:0]]);
                                    // Update flags
                                    cpu_state <= CPU_MULDIV;
                                end
                            end
                            4'h5: begin // UDIV
                                if (R[ibuf[1][3:0]] == 64'd0) begin
                                    ivec_id <= 8'd4;
                                    cpu_state <= CPU_IRQ;
                                end else begin
                                    R[0] <= R[ibuf[1][7:4]] %
                                            R[ibuf[1][3:0]];
                                    R[ibuf[1][7:4]] <=
                                        R[ibuf[1][7:4]] /
                                        R[ibuf[1][3:0]];
                                    cpu_state <= CPU_MULDIV;
                                end
                            end
                            4'h6: begin // MOD (signed)
                                if (R[ibuf[1][3:0]] == 64'd0) begin
                                    ivec_id <= 8'd4;
                                    cpu_state <= CPU_IRQ;
                                end else begin
                                    R[ibuf[1][7:4]] <=
                                        $signed(R[ibuf[1][7:4]]) %
                                        $signed(R[ibuf[1][3:0]]);
                                    cpu_state <= CPU_MULDIV;
                                end
                            end
                            4'h7: begin // UMOD
                                if (R[ibuf[1][3:0]] == 64'd0) begin
                                    ivec_id <= 8'd4;
                                    cpu_state <= CPU_IRQ;
                                end else begin
                                    R[ibuf[1][7:4]] <=
                                        R[ibuf[1][7:4]] %
                                        R[ibuf[1][3:0]];
                                    cpu_state <= CPU_MULDIV;
                                end
                            end
                            default:
                                cpu_state <= CPU_FETCH;
                        endcase
                    end

                    // --------------------------------------------------------
                    // CSR (0xD)
                    // --------------------------------------------------------
                    else if (fam == FAM_CSR) begin
                        ext_active <= 1'b0;
                        // Encoding: nib[3] = W bit, nib[2:0] = register 0-7
                        if (nib[3]) begin
                            // CSRW: write GPR[nib[2:0]] to CSR[ibuf[1]]
                            csr_wen   <= 1'b1;
                            csr_addr  <= ibuf[1];
                            csr_wdata <= R[nib[2:0]];
                            // Handle local CSRs
                            case (ibuf[1])
                                CSR_FLAGS:   flags    <= R[nib[2:0]][7:0];
                                CSR_PSEL:    psel     <= R[nib[2:0]][3:0];
                                CSR_XSEL:    xsel     <= R[nib[2:0]][3:0];
                                CSR_SPSEL:   spsel    <= R[nib[2:0]][3:0];
                                CSR_IVTBASE: ivt_base <= R[nib[2:0]];
                                CSR_D:       D        <= R[nib[2:0]][7:0];
                                CSR_DF:      flags[1] <= R[nib[2:0]][0];
                                CSR_QREG:    Q        <= R[nib[2:0]][0];
                                CSR_TREG:    T        <= R[nib[2:0]][7:0];
                                CSR_IE:      flags[6] <= R[nib[2:0]][0];
                                CSR_IVEC_ID: ivec_id  <= R[nib[2:0]][7:0];
                                CSR_PERF_CTRL: begin
                                    perf_enable <= R[nib[2:0]][0];
                                    if (R[nib[2:0]][1]) begin  // bit 1 = reset
                                        perf_cycles  <= 64'd0;
                                        perf_stalls  <= 64'd0;
                                        perf_tileops <= 64'd0;
                                        perf_extmem  <= 64'd0;
                                    end
                                end
                                CSR_BIST_CMD: begin
                                    // RTL stub: instant pass (real BIST FSM is external)
                                    if (R[nib[2:0]][1:0] != 2'd0) begin
                                        bist_status    <= 2'd2;  // pass
                                        bist_fail_addr <= 64'd0;
                                        bist_fail_data <= 64'd0;
                                    end
                                end
                                CSR_TILE_SELFTEST: begin
                                    // RTL stub: instant pass
                                    if (R[nib[2:0]][0]) begin
                                        tile_selftest  <= 2'd2;
                                        tile_st_detail <= 8'd0;
                                    end
                                end
                                CSR_ICACHE_CTRL: begin
                                    icache_enabled <= R[nib[2:0]][0];
                                    if (R[nib[2:0]][1])  // bit 1 = invalidate all
                                        icache_inv_all <= 1'b1;
                                end
                                // COREID, NCORES are read-only — ignore writes
                            endcase
                        end else begin
                            // CSRR: read CSR[ibuf[1]] to GPR[nib[2:0]]
                            case (ibuf[1])
                                CSR_FLAGS:      R[nib[2:0]] <= {56'd0, flags};
                                CSR_PSEL:       R[nib[2:0]] <= {60'd0, psel};
                                CSR_XSEL:       R[nib[2:0]] <= {60'd0, xsel};
                                CSR_SPSEL:      R[nib[2:0]] <= {60'd0, spsel};
                                CSR_IVTBASE:    R[nib[2:0]] <= ivt_base;
                                CSR_D:          R[nib[2:0]] <= {56'd0, D};
                                CSR_DF:         R[nib[2:0]] <= {63'd0, flags[1]};
                                CSR_QREG:       R[nib[2:0]] <= {63'd0, Q};
                                CSR_TREG:       R[nib[2:0]] <= {56'd0, T};
                                CSR_IE:         R[nib[2:0]] <= {63'd0, flags[6]};
                                CSR_COREID:     R[nib[2:0]] <= {62'd0, core_id};
                                CSR_NCORES:     R[nib[2:0]] <= {62'd0, NUM_CORES[1:0]};
                                CSR_IVEC_ID:    R[nib[2:0]] <= {56'd0, ivec_id};
                                CSR_TRAP_ADDR:  R[nib[2:0]] <= trap_addr;
                                CSR_MEGAPAD_SZ: R[nib[2:0]] <= 64'd0;
                                CSR_CPUID:      R[nib[2:0]] <= 64'h4D50_3634_0001_0000;
                                CSR_PERF_CYCLES:  R[nib[2:0]] <= perf_cycles;
                                CSR_PERF_STALLS:  R[nib[2:0]] <= perf_stalls;
                                CSR_PERF_TILEOPS: R[nib[2:0]] <= perf_tileops;
                                CSR_PERF_EXTMEM:  R[nib[2:0]] <= perf_extmem;
                                CSR_PERF_CTRL:    R[nib[2:0]] <= {63'd0, perf_enable};
                                CSR_BIST_STATUS:    R[nib[2:0]] <= {62'd0, bist_status};
                                CSR_BIST_FAIL_ADDR: R[nib[2:0]] <= bist_fail_addr;
                                CSR_BIST_FAIL_DATA: R[nib[2:0]] <= bist_fail_data;
                                CSR_TILE_SELFTEST:  R[nib[2:0]] <= {62'd0, tile_selftest};
                                CSR_TILE_ST_DETAIL: R[nib[2:0]] <= {56'd0, tile_st_detail};
                                CSR_ICACHE_CTRL:    R[nib[2:0]] <= {63'd0, icache_enabled};
                                CSR_ICACHE_HITS:    R[nib[2:0]] <= 64'd0;  // stats in I-cache module
                                CSR_ICACHE_MISSES:  R[nib[2:0]] <= 64'd0;  // stats in I-cache module
                                default:        R[nib[2:0]] <= csr_rdata;
                            endcase
                        end
                        cpu_state <= CPU_FETCH;
                    end

                    // --------------------------------------------------------
                    // MEX — tile engine (0xE)
                    // --------------------------------------------------------
                    else if (fam == FAM_MEX) begin
                        mex_valid      <= 1'b1;
                        mex_ss         <= ibuf[0][3:2];
                        mex_op         <= ibuf[0][1:0];
                        mex_funct      <= ibuf[1][2:0];
                        mex_gpr_val    <= (ibuf[0][3:2] == 2'd1) ? R[ibuf[2][3:0]] : 64'd0;
                        mex_imm8       <= ibuf[2];
                        mex_ext_mod    <= ext_mod;
                        mex_ext_active <= ext_active;
                        ext_active     <= 1'b0;
                        cpu_state      <= CPU_MEX_WAIT;
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
                // MEM_READ: wait for bus, handle sign-extension, post-ops
                // ============================================================
                CPU_MEM_READ: begin
                    bus_valid <= 1'b1;
                    bus_addr  <= effective_addr;
                    bus_wen   <= 1'b0;
                    if (bus_ready) begin
                        bus_valid <= 1'b0;  // deassert to prevent spurious re-request
                        // Sign-extending loads
                        case (mem_sub)
                            4'hC: // LD.SB
                                R[dst_reg] <= {{56{bus_rdata[7]}}, bus_rdata[7:0]};
                            4'hD: // LD.SH
                                R[dst_reg] <= {{48{bus_rdata[15]}}, bus_rdata[15:0]};
                            4'hE: // LD.SW
                                R[dst_reg] <= {{32{bus_rdata[31]}}, bus_rdata[31:0]};
                            4'hF: begin // INP — store to D and M(R(X))
                                D <= bus_rdata[7:0];
                                // Write to M(R(X))
                                effective_addr <= R[xsel];
                                mem_data <= {56'd0, bus_rdata[7:0]};
                                bus_size <= BUS_BYTE;
                                cpu_state <= CPU_MEM_WRITE;
                            end
                            4'h5: begin // 1802 RET — unpack T byte
                                T <= bus_rdata[7:0];
                                xsel <= bus_rdata[7:4];
                                psel <= bus_rdata[3:0];
                                flags[6] <= 1'b1; // IE←1
                                // Sync fetch_pc to the new PC register
                                fetch_pc <= R[bus_rdata[3:0]];
                                cpu_state <= CPU_FETCH;
                            end
                            4'hB: begin // DIS — unpack T byte
                                T <= bus_rdata[7:0];
                                xsel <= bus_rdata[7:4];
                                psel <= bus_rdata[3:0];
                                flags[6] <= 1'b0; // IE←0
                                fetch_pc <= R[bus_rdata[3:0]];
                                cpu_state <= CPU_FETCH;
                            end
                            default: begin
                                R[dst_reg] <= bus_rdata;
                                // If writing to the PC register, sync fetch_pc
                                if (dst_reg == psel)
                                    fetch_pc <= bus_rdata;
                            end
                        endcase

                        // Post-increment for LDA (sub 1)
                        if (mem_sub == 4'h1)
                            R[src_reg] <= R[src_reg] + 64'd8;
                        // Post-increment for LDXA (sub 3)
                        if (mem_sub == 4'h3)
                            R[xsel] <= R[xsel] + 64'd8;

                        // Post-action dispatch
                        if (post_action == POST_RTI_POP2) begin
                            // PC was loaded, now pop flags
                            effective_addr <= R[spsel];
                            R[spsel] <= R[spsel] + 64'd8;
                            bus_size <= BUS_DWORD;
                            post_action <= POST_NONE;
                            cpu_state <= CPU_MEM_READ2;
                        end else if (mem_sub != 4'hF && mem_sub != 4'h5
                                     && mem_sub != 4'hB) begin
                            cpu_state <= CPU_FETCH;
                        end
                    end
                end

                // ============================================================
                // MEM_READ2: second memory read (RTI flags pop)
                // ============================================================
                CPU_MEM_READ2: begin
                    bus_valid <= 1'b1;
                    bus_addr  <= effective_addr;
                    bus_wen   <= 1'b0;
                    if (bus_ready) begin
                        bus_valid <= 1'b0;  // deassert to prevent spurious re-request
                        flags <= bus_rdata[7:0];
                        cpu_state <= CPU_FETCH;
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
                        bus_valid <= 1'b0;  // deassert to prevent spurious re-request
                        // Store-to-code: conservatively invalidate I-cache
                        // line containing the written address
                        if (icache_enabled) begin
                            icache_inv_line <= 1'b1;
                            icache_inv_addr <= effective_addr;
                        end

                        // Post-decrement for STXD (sub 5)
                        if (mem_sub == 4'h5)
                            R[xsel] <= R[xsel] - 64'd8;

                        // Post-action: after TRAP/IRQ push PC, push flags
                        if (post_action == POST_IRQ_VEC) begin
                            // Push flags next
                            R[spsel] <= R[spsel] - 64'd8;
                            effective_addr <= R[spsel] - 64'd8;
                            mem_data <= {56'd0, flags};
                            post_action <= POST_NONE;
                            cpu_state <= CPU_IRQ_PUSH;
                        end else begin
                            cpu_state <= CPU_FETCH;
                        end
                    end
                end

                // ============================================================
                // IRQ_PUSH: push flags, then load IVT vector
                // ============================================================
                CPU_IRQ_PUSH: begin
                    bus_valid <= 1'b1;
                    bus_addr  <= effective_addr;
                    bus_wdata <= mem_data;
                    bus_wen   <= 1'b1;
                    bus_size  <= BUS_DWORD;
                    if (bus_ready) begin
                        // Now load IVT vector from memory
                        effective_addr <= ivt_base + {56'd0, ivec_id, 3'b000};
                        bus_size <= BUS_DWORD;
                        cpu_state <= CPU_IRQ_LOAD;
                    end
                end

                // ============================================================
                // IRQ_LOAD: read handler address from IVT, jump there
                // ============================================================
                CPU_IRQ_LOAD: begin
                    bus_valid <= 1'b1;
                    bus_addr  <= effective_addr;
                    bus_wen   <= 1'b0;
                    bus_size  <= BUS_DWORD;
                    if (bus_ready) begin
                        R[psel] <= bus_rdata;
                        fetch_pc <= bus_rdata;
                        cpu_state <= CPU_FETCH;
                    end
                end

                // ============================================================
                // MEMALU_RD: read byte from M(R(X)) for MEMALU/IO ops
                // ============================================================
                CPU_MEMALU_RD: begin
                    bus_valid <= 1'b1;
                    bus_addr  <= effective_addr;
                    bus_wen   <= 1'b0;
                    bus_size  <= BUS_BYTE;
                    if (bus_ready) begin
                        memalu_byte <= bus_rdata[7:0];

                        if (memalu_sub == 4'hF) begin
                            // IO OUT: byte read from M(R(X)), now write to MMIO
                            D <= bus_rdata[7:0];
                            effective_addr <= {MMIO_HI, 20'd0, io_port, 9'd0};
                            mem_data <= {56'd0, bus_rdata[7:0]};
                            bus_size <= BUS_BYTE;
                            R[xsel] <= R[xsel] + 64'd1; // advance R(X)
                            cpu_state <= CPU_MEM_WRITE;
                            mem_sub <= 4'd0; // clear post-dec marker
                        end else begin
                            // Execute MEMALU operation on D and memalu_byte
                            case (memalu_sub)
                                4'h0: begin // LDX — D ← M(R(X))
                                    D <= bus_rdata[7:0];
                                end
                                4'h1: begin // OR.X
                                    D <= (bus_rdata[7:0] | D);
                                    flags[0] <= ((bus_rdata[7:0] | D) == 8'd0);
                                end
                                4'h2: begin // AND.X
                                    D <= (bus_rdata[7:0] & D);
                                    flags[0] <= ((bus_rdata[7:0] & D) == 8'd0);
                                end
                                4'h3: begin // XOR.X
                                    D <= (bus_rdata[7:0] ^ D);
                                    flags[0] <= ((bus_rdata[7:0] ^ D) == 8'd0);
                                end
                                4'h4: begin // ADD.X
                                    begin
                                        reg [8:0] sum9;
                                        sum9 = {1'b0, bus_rdata[7:0]} + {1'b0, D};
                                        D <= sum9[7:0];
                                        flags[1] <= sum9[8]; // carry
                                        flags[0] <= (sum9[7:0] == 8'd0);
                                    end
                                end
                                4'h5: begin // SD.X — D ← M(R(X)) - D
                                    begin
                                        reg [8:0] diff9;
                                        diff9 = {1'b0, bus_rdata[7:0]} - {1'b0, D};
                                        D <= diff9[7:0];
                                        flags[1] <= ~diff9[8]; // 1=no borrow
                                        flags[0] <= (diff9[7:0] == 8'd0);
                                    end
                                end
                                4'h7: begin // SM.X — D ← D - M(R(X))
                                    begin
                                        reg [8:0] diff9b;
                                        diff9b = {1'b0, D} - {1'b0, bus_rdata[7:0]};
                                        D <= diff9b[7:0];
                                        flags[1] <= ~diff9b[8];
                                        flags[0] <= (diff9b[7:0] == 8'd0);
                                    end
                                end
                                4'h8: begin // ADC.X — D ← M + D + C
                                    begin
                                        reg [8:0] sum9c;
                                        sum9c = {1'b0, bus_rdata[7:0]} + {1'b0, D}
                                                + {8'd0, flags[1]};
                                        D <= sum9c[7:0];
                                        flags[1] <= sum9c[8];
                                        flags[0] <= (sum9c[7:0] == 8'd0);
                                    end
                                end
                                4'h9: begin // SDB.X — D ← M - D - !C
                                    begin
                                        reg [8:0] diff9c;
                                        diff9c = {1'b0, bus_rdata[7:0]} - {1'b0, D}
                                                 - {8'd0, ~flags[1]};
                                        D <= diff9c[7:0];
                                        flags[1] <= ~diff9c[8];
                                        flags[0] <= (diff9c[7:0] == 8'd0);
                                    end
                                end
                                4'hB: begin // SMB.X — D ← D - M - !C
                                    begin
                                        reg [8:0] diff9d;
                                        diff9d = {1'b0, D} - {1'b0, bus_rdata[7:0]}
                                                 - {8'd0, ~flags[1]};
                                        D <= diff9d[7:0];
                                        flags[1] <= ~diff9d[8];
                                        flags[0] <= (diff9d[7:0] == 8'd0);
                                    end
                                end
                                4'hF: begin // LDXA — D ← M(R(X)); R(X)++
                                    D <= bus_rdata[7:0];
                                    R[xsel] <= R[xsel] + 64'd1;
                                end
                                default: ;
                            endcase
                            cpu_state <= CPU_FETCH;
                        end
                    end
                end

                // ============================================================
                // MULDIV: writeback for multiply/divide
                // ============================================================
                CPU_MULDIV: begin
                    if (mem_sub == 4'h1) begin
                        // High 64 bits (MULH / UMULH)
                        R[dst_reg] <= mul_result[127:64];
                    end else begin
                        // Low 64 bits (MUL / UMUL) or DIV/MOD result
                        // (DIV/MOD already wrote R[dst_reg] in DECODE)
                        if (nib <= 4'h3)  // Only overwrite for MUL/UMUL
                            R[dst_reg] <= mul_result[63:0];
                    end
                    // Flag updates
                    flags[0] <= (R[dst_reg] == 64'd0); // Z
                    flags[2] <= R[dst_reg][63];         // N
                    mem_sub <= 4'd0;
                    cpu_state <= CPU_FETCH;
                end

                // ============================================================
                // MEX_WAIT: wait for tile engine to finish
                // ============================================================
                CPU_MEX_WAIT: begin
                    if (mex_done)
                        cpu_state <= CPU_FETCH;
                end

                // ============================================================
                // IRQ: vectored interrupt entry (push PC+flags, load IVT)
                // ============================================================
                CPU_IRQ: begin
                    // Push PC onto stack
                    R[spsel] <= R[spsel] - 64'd8;
                    effective_addr <= R[spsel] - 64'd8;
                    mem_data <= R[psel];
                    bus_size <= BUS_DWORD;
                    flags[6] <= 1'b0;  // disable interrupts
                    ivec_id <= {4'd0, irq_vector};
                    post_action <= POST_IRQ_VEC;
                    cpu_state <= CPU_MEM_WRITE;
                end

                // ============================================================
                // HALT: wait for interrupt (WFI behaviour)
                // ============================================================
                CPU_HALT: begin
                    if (irq_pending)
                        cpu_state <= CPU_IRQ;
                end

                // ============================================================
                // SKIP: read next instruction's byte0 via I-cache for length
                // ============================================================
                CPU_SKIP: begin
                    if (!icache_stall) begin
                        icache_req  <= 1'b1;
                        icache_addr <= R[psel];
                    end
                    if (icache_hit) begin
                        icache_req <= 1'b0;
                        // Extract byte0 from the cache line at the byte offset
                        // and advance PC + fetch_pc past the skipped instruction
                        begin : skip_block
                            reg [7:0] skip_byte;
                            reg [3:0] skip_len;
                            case (R[psel][2:0])
                                3'd0: skip_byte = icache_data[ 7: 0];
                                3'd1: skip_byte = icache_data[15: 8];
                                3'd2: skip_byte = icache_data[23:16];
                                3'd3: skip_byte = icache_data[31:24];
                                3'd4: skip_byte = icache_data[39:32];
                                3'd5: skip_byte = icache_data[47:40];
                                3'd6: skip_byte = icache_data[55:48];
                                3'd7: skip_byte = icache_data[63:56];
                            endcase
                            skip_len = instr_len(skip_byte, 1'b0);
                            R[psel]  <= R[psel]  + {59'd0, 1'b0, skip_len};
                            fetch_pc <= R[psel]  + {59'd0, 1'b0, skip_len};
                        end
                        cpu_state <= CPU_FETCH;
                    end
                end

            endcase
        end
    end

endmodule
