// ============================================================================
// mp64_cpu.v — Megapad-64 CPU Core (major / full-featured)
// ============================================================================
//
// 64-bit CPU with RCA 1802-inspired register architecture and extended ISA.
//
// Key features:
//   - 16 × 64-bit GPRs, selectable PC (PSEL), data pointer (XSEL), SP (SPSEL)
//   - Variable-length instructions (1–11 bytes)
//   - 8-bit flags: [S I G P V N C Z]
//   - 8-bit D accumulator, Q flip-flop, T register (1802 heritage)
//   - 16 MEMALU ops on D via M(R(X))
//   - Hardware MUL (via mp64_mul wrapper, 4-cycle), iterative DIV (64-cycle)
//   - Vectored interrupts (IVT in memory), privilege levels, MPU
//   - I-cache interface with prefetch buffer (2-stage pipeline)
//   - Tile/MEX engine dispatch
//   - Performance counters, BIST, DMA ring CSRs
//
// Pipeline: 2-stage decoupled
//   IF:  reads 8 bytes/cycle from I-cache into 16-byte ibuf
//   DEX: decode + execute from ibuf
//
// Coding rules:
//   - Verilog-2001, synchronous reset (active-high), non-blocking assigns
//   - No vendor primitives, no `%` / `/` in synthesisable paths
//     (MUL uses `*` for structural inference; DIV uses iterative or trap)
//   - Portable across Kintex-7, Artix-7, ECP5, simulation
//

`include "mp64_pkg.vh"

module mp64_cpu #(
    parameter CORE_ID_W = MP64_CORE_ID_BITS
) (
    input  wire        clk,
    input  wire        rst,

    // === Core identification ===
    input  wire [CORE_ID_W-1:0] core_id,

    // === I-cache fetch interface ===
    output reg  [63:0] icache_addr,
    output reg         icache_req,
    input  wire [63:0] icache_data,
    input  wire        icache_hit,
    input  wire        icache_stall,
    output reg         icache_inv_all,
    output reg         icache_inv_line,
    output reg  [63:0] icache_inv_addr,

    // === Data bus master ===
    output reg         bus_valid,
    output reg  [63:0] bus_addr,
    output reg  [63:0] bus_wdata,
    output reg         bus_wen,
    output reg  [1:0]  bus_size,
    input  wire [63:0] bus_rdata,
    input  wire        bus_ready,

    // === Tile/MEX engine interface ===
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
    input  wire        irq_ipi,

    // === I-cache statistics (from icache module) ===
    input  wire [63:0] icache_stat_hits,
    input  wire [63:0] icache_stat_misses,

    // === System configuration ===
    input  wire [63:0] mem_size_bytes,

    // === External flags (EF1-EF4) ===
    input  wire [3:0]  ef_flags
);

    `include "mp64_cpu_funcs.vh"

    // ====================================================================
    // Register file
    // ====================================================================
    reg [63:0] R [0:15];
    reg [3:0]  psel, xsel, spsel;     // PC / X / SP selectors
    reg [7:0]  flags;                 // [S I G P V N C Z]

    wire [63:0] PC = R[psel];
    wire [63:0] SP = R[spsel];
    wire [63:0] RX = R[xsel];

    // 1802 heritage
    reg [7:0]  D;                     // 8-bit accumulator
    reg        Q;                     // Q flip-flop
    reg [7:0]  T;                     // saved X|P on MARK

    // IVT / trap
    reg [63:0] ivt_base;
    reg [7:0]  ivec_id;
    reg [63:0] trap_addr;

    // Privilege / MPU
    reg        priv_level;            // 0=supervisor, 1=user
    reg [63:0] mpu_base, mpu_limit;

    // Performance counters
    reg [63:0] perf_cycles, perf_stalls, perf_tileops, perf_extmem;
    reg        perf_enable;

    // BIST
    reg [1:0]  bist_status;
    reg [63:0] bist_fail_addr, bist_fail_data;
    reg [1:0]  bist_pattern;
    reg [2:0]  bist_phase;
    reg [19:0] bist_addr_cnt;
    reg        bist_running;

    // Tile self-test
    reg [1:0]  tile_selftest;
    reg [7:0]  tile_st_detail;
    reg [5:0]  tile_st_cnt;

    // DMA ring
    reg [63:0] dma_ring_base, dma_ring_size, dma_head, dma_tail;
    reg [63:0] dma_status, dma_ctrl;

    // EXT prefix
    reg [3:0]  ext_mod;
    reg        ext_active;

    // I-cache enable
    reg        icache_enabled;

    // ====================================================================
    // Instruction prefetch buffer
    // ====================================================================
    reg [7:0]  ibuf [0:15];
    reg [4:0]  ibuf_len;
    reg [3:0]  ibuf_need;
    reg [63:0] fetch_pc;
    reg        fetch_active;

    wire [3:0] fam = ibuf[0][7:4];
    wire [3:0] nib = ibuf[0][3:0];

    // ====================================================================
    // CPU FSM — extra state for BIST
    // ====================================================================
    localparam CPU_BIST = 4'd8;       // reuse FETCH_MORE slot (BIST is major only)

    reg [3:0] cpu_state;

    // ====================================================================
    // Interrupt pending
    // ====================================================================
    reg        irq_pending;
    reg [3:0]  irq_vector;

    always @(*) begin
        irq_pending = 1'b0;
        irq_vector  = 4'd0;
        if (flags[6]) begin           // I flag
            if      (irq_ipi)   begin irq_pending = 1'b1; irq_vector = IRQX_IPI;             end
            else if (irq_timer) begin irq_pending = 1'b1; irq_vector = {1'b0, IRQ_TIMER};    end
            else if (irq_uart)  begin irq_pending = 1'b1; irq_vector = IRQX_UART;            end
            else if (irq_nic)   begin irq_pending = 1'b1; irq_vector = IRQX_NIC;             end
        end
    end

    // ====================================================================
    // ALU instance
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
    // Multi-cycle temporaries
    // ====================================================================
    reg [63:0] mem_data;
    reg [3:0]  dst_reg, src_reg;
    reg [63:0] effective_addr;
    reg [3:0]  mem_sub;
    reg [127:0] mul_result;
    reg        mul_start_r;
    reg        mul_is_signed_r;
    reg [63:0] mul_op_a, mul_op_b;

    // Iterative divider state
    reg        div_active;
    reg        div_signed_op;
    reg [63:0] div_dividend, div_divisor;
    reg [63:0] div_quotient, div_remainder;
    reg [6:0]  div_cycle;
    reg        div_done_r;
    reg [3:0]  div_op_nib;       // which MULDIV sub-op (4–7)

    // mp64_mul instance
    wire [127:0] mul_u_result;
    wire         mul_u_done;
    wire         mul_u_busy;

    mp64_mul #(.LATENCY(4)) u_mul (
        .clk       (clk),
        .rst       (rst),
        .start     (mul_start_r),
        .is_signed (mul_is_signed_r),
        .a         (mul_op_a),
        .b         (mul_op_b),
        .result    (mul_u_result),
        .done      (mul_u_done),
        .busy      (mul_u_busy)
    );
    reg [7:0]  memalu_byte;
    reg [3:0]  memalu_sub;
    reg [2:0]  io_port;
    reg        io_is_inp;
    reg [2:0]  post_action;

    // MPU check (combinational)
    wire mpu_enabled = priv_level && (mpu_limit > mpu_base);
    wire addr_is_hbw = (effective_addr[63:32] == 32'd0)
                     && (effective_addr[31:20] >= 12'hFFD);
    wire addr_is_mmio = (effective_addr[63:32] == MP64_MMIO_HI);
    wire mpu_fault = priv_level && !addr_is_mmio && (
        addr_is_hbw ||
        (mpu_enabled && (effective_addr < mpu_base || effective_addr >= mpu_limit))
    );

    // ====================================================================
    // Main FSM
    // ====================================================================
    always @(posedge clk) begin
        if (rst) begin
            cpu_state      <= CPU_FETCH;
            bus_valid      <= 1'b0;
            csr_wen        <= 1'b0;
            mex_valid      <= 1'b0;
            ext_active     <= 1'b0;
            ext_mod        <= 4'd0;
            fetch_active   <= 1'b1;
            fetch_pc       <= 64'd0;
            ibuf_len       <= 5'd0;
            ibuf_need      <= 4'd1;

            icache_enabled <= 1'b1;
            icache_inv_all <= 1'b0;
            icache_inv_line<= 1'b0;
            icache_inv_addr<= 64'd0;
            icache_req     <= 1'b0;

            psel   <= 4'd3;
            xsel   <= 4'd2;
            spsel  <= 4'd15;
            flags  <= 8'h40;          // I=1

            ivt_base   <= 64'd0;
            ivec_id    <= 8'd0;
            trap_addr  <= 64'd0;
            priv_level <= 1'b0;
            mpu_base   <= 64'd0;
            mpu_limit  <= 64'd0;
            D          <= 8'd0;
            Q          <= 1'b0;
            T          <= 8'd0;

            post_action <= POST_NONE;
            mem_sub     <= 4'd0;
            memalu_sub  <= 4'd0;
            memalu_byte <= 8'd0;
            io_port     <= 3'd0;
            io_is_inp   <= 1'b0;

            alu_op <= 4'd0;
            alu_a  <= 64'd0;
            alu_b  <= 64'd0;

            mul_result <= 128'd0;
            mul_start_r     <= 1'b0;
            mul_is_signed_r <= 1'b0;
            mul_op_a        <= 64'd0;
            mul_op_b        <= 64'd0;
            div_active   <= 1'b0;
            div_signed_op<= 1'b0;
            div_dividend <= 64'd0;
            div_divisor  <= 64'd0;
            div_quotient <= 64'd0;
            div_remainder<= 64'd0;
            div_cycle    <= 7'd0;
            div_done_r   <= 1'b0;
            div_op_nib   <= 4'd0;

            perf_cycles  <= 64'd0;
            perf_stalls  <= 64'd0;
            perf_tileops <= 64'd0;
            perf_extmem  <= 64'd0;
            perf_enable  <= 1'b1;

            bist_status    <= 2'd0;
            bist_fail_addr <= 64'd0;
            bist_fail_data <= 64'd0;
            bist_pattern   <= 2'd0;
            bist_phase     <= 3'd0;
            bist_addr_cnt  <= 20'd0;
            bist_running   <= 1'b0;

            tile_selftest  <= 2'd0;
            tile_st_detail <= 8'd0;
            tile_st_cnt    <= 6'd0;

            dma_ring_base  <= 64'd0;
            dma_ring_size  <= 64'd0;
            dma_head       <= 64'd0;
            dma_tail       <= 64'd0;
            dma_status     <= 64'd0;
            dma_ctrl       <= 64'd0;

            R[0]  <= 64'd0; R[1]  <= 64'd0; R[2]  <= 64'd0; R[3]  <= 64'd0;
            R[4]  <= 64'd0; R[5]  <= 64'd0; R[6]  <= 64'd0; R[7]  <= 64'd0;
            R[8]  <= 64'd0; R[9]  <= 64'd0; R[10] <= 64'd0; R[11] <= 64'd0;
            R[12] <= 64'd0; R[13] <= 64'd0; R[14] <= 64'd0; R[15] <= 64'd0;

        end else begin
            bus_valid      <= 1'b0;
            csr_wen        <= 1'b0;
            mex_valid      <= 1'b0;
            icache_inv_all <= 1'b0;
            icache_inv_line<= 1'b0;

            // Performance counters
            if (perf_enable) begin
                perf_cycles <= perf_cycles + 64'd1;
                if ((cpu_state == CPU_FETCH   && icache_stall) ||
                    (cpu_state == CPU_MEM_READ  && !bus_ready) ||
                    (cpu_state == CPU_MEM_WRITE && !bus_ready) ||
                    (cpu_state == CPU_MEM_READ2 && !bus_ready) ||
                    (cpu_state == CPU_MEMALU_RD && !bus_ready) ||
                    (cpu_state == CPU_IRQ_PUSH  && !bus_ready) ||
                    (cpu_state == CPU_IRQ_LOAD  && !bus_ready))
                    perf_stalls <= perf_stalls + 64'd1;
                if (cpu_state == CPU_MEX_WAIT && mex_done)
                    perf_tileops <= perf_tileops + 64'd1;
            end

            // Tile self-test countdown
            if (tile_selftest == 2'd1 && tile_st_cnt != 6'd0) begin
                tile_st_cnt <= tile_st_cnt - 6'd1;
                if (tile_st_cnt == 6'd1)
                    tile_selftest <= 2'd2;
            end

            case (cpu_state)

            // ============================================================
            // FETCH: fill ibuf from I-cache
            // ============================================================
            CPU_FETCH: begin
                if (irq_pending && ibuf_len == 5'd0) begin
                    cpu_state <= CPU_IRQ;
                end else if (ibuf_len >= {1'b0, ibuf_need}) begin
                    cpu_state <= CPU_DECODE;
                end else begin
                    if (!icache_stall) begin
                        icache_req  <= 1'b1;
                        icache_addr <= fetch_pc;
                    end
                    if (icache_hit) begin
                        icache_req <= 1'b0;
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

                        // Compute instruction length from byte0
                        if (ibuf_len == 5'd0) begin
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
                R[psel]    <= R[psel] + {60'd0, ibuf_need};
                ibuf_len   <= 5'd0;
                ibuf_need  <= 4'd1;
                fetch_pc   <= R[psel] + {60'd0, ibuf_need};
                icache_req <= 1'b0;
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
                            flags <= 8'h40; priv_level <= 1'b0;
                            D <= 8'd0; Q <= 1'b0; T <= 8'd0;
                            ivt_base <= 64'd0; ivec_id <= 8'd0;
                            R[0] <= 64'd0; R[1] <= 64'd0; R[2] <= 64'd0; R[3] <= 64'd0;
                            R[4] <= 64'd0; R[5] <= 64'd0; R[6] <= 64'd0; R[7] <= 64'd0;
                            R[8] <= 64'd0; R[9] <= 64'd0; R[10]<= 64'd0; R[11]<= 64'd0;
                            R[12]<= 64'd0; R[13]<= 64'd0; R[14]<= 64'd0; R[15]<= 64'd0;
                            cpu_state <= CPU_FETCH;
                        end

                        4'h4: begin // RTI
                            effective_addr <= R[spsel];
                            R[spsel]    <= R[spsel] + 64'd8;
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
                            mem_sub  <= 4'hB;
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
                            mem_data <= R[psel] + {60'd0, ibuf_need};
                            R[psel]  <= R[ibuf[1][3:0]];
                            fetch_pc <= R[ibuf[1][3:0]];
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
                            mem_data <= R[psel] + {60'd0, ibuf_need};
                            flags[6] <= 1'b0;
                            priv_level <= 1'b0;
                            ivec_id <= IRQX_SW_TRAP;
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
                // BR (0x3)
                // --------------------------------------------------------
                else if (fam == FAM_BR) begin
                    ext_active <= 1'b0;
                    if (ext_active && ext_mod == EXT_SKIP) begin
                        if (cond_eval(nib, flags, Q, ef_flags)) begin
                            icache_req  <= 1'b1;
                            icache_addr <= R[psel] + {60'd0, ibuf_need};
                            cpu_state   <= CPU_SKIP;
                        end else
                            cpu_state <= CPU_FETCH;
                    end else begin
                        if (cond_eval(nib, flags, Q, ef_flags)) begin
                            R[psel]  <= R[psel] + {{56{ibuf[1][7]}}, ibuf[1]}
                                        + {60'd0, ibuf_need};
                            fetch_pc <= R[psel] + {{56{ibuf[1][7]}}, ibuf[1]}
                                        + {60'd0, ibuf_need};
                        end
                        cpu_state <= CPU_FETCH;
                    end
                end

                // --------------------------------------------------------
                // LBR (0x4)
                // --------------------------------------------------------
                else if (fam == FAM_LBR) begin
                    ext_active <= 1'b0;
                    if (cond_eval(nib, flags, Q, ef_flags)) begin
                        R[psel]  <= R[psel] + {{48{ibuf[1][7]}}, ibuf[1], ibuf[2]}
                                    + {60'd0, ibuf_need};
                        fetch_pc <= R[psel] + {{48{ibuf[1][7]}}, ibuf[1], ibuf[2]}
                                    + {60'd0, ibuf_need};
                    end
                    cpu_state <= CPU_FETCH;
                end

                // --------------------------------------------------------
                // MEM (0x5) — 16 sub-ops
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
                // IMM (0x6) — 16 sub-ops
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
                            4'hC: begin D <= R[ibuf[1][7:4]][7:0];  cpu_state <= CPU_FETCH; end
                            4'hD: begin D <= R[ibuf[1][7:4]][15:8]; cpu_state <= CPU_FETCH; end
                            4'hE: begin R[ibuf[1][7:4]][7:0]  <= D; cpu_state <= CPU_FETCH; end
                            4'hF: begin R[ibuf[1][7:4]][15:8] <= D; cpu_state <= CPU_FETCH; end
                        endcase
                    end
                end

                // --------------------------------------------------------
                // ALU (0x7) — 16 sub-ops
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
                // MEMALU (0x8) — 1802 heritage D-register ops
                // --------------------------------------------------------
                else if (fam == FAM_MEMALU) begin
                    ext_active <= 1'b0;
                    if (priv_level) begin
                        // Privilege trap
                        R[spsel] <= R[spsel] - 64'd8;
                        effective_addr <= R[spsel] - 64'd8;
                        mem_data <= R[psel]; flags[6] <= 1'b0;
                        priv_level <= 1'b0; ivec_id <= {4'd0, IRQX_PRIV};
                        post_action <= POST_IRQ_VEC;
                        bus_size <= BUS_DWORD; cpu_state <= CPU_MEM_WRITE;
                    end else begin
                        memalu_sub <= nib;
                        case (nib)
                            4'hE: begin R[xsel] <= R[xsel]+64'd1; cpu_state<=CPU_FETCH; end // IRX
                            4'h6: begin // SHR.D
                                flags[1] <= D[0]; D <= {1'b0, D[7:1]};
                                flags[0] <= ({1'b0, D[7:1]} == 8'd0);
                                cpu_state <= CPU_FETCH;
                            end
                            4'hA: begin // SHRC.D
                                flags[1] <= D[0]; D <= {flags[1], D[7:1]};
                                flags[0] <= ({flags[1], D[7:1]} == 8'd0);
                                cpu_state <= CPU_FETCH;
                            end
                            4'hC: begin // SHL.D
                                flags[1] <= D[7]; D <= {D[6:0], 1'b0};
                                flags[0] <= ({D[6:0], 1'b0} == 8'd0);
                                cpu_state <= CPU_FETCH;
                            end
                            4'hD: begin // SHLC.D
                                flags[1] <= D[7]; D <= {D[6:0], flags[1]};
                                flags[0] <= ({D[6:0], flags[1]} == 8'd0);
                                cpu_state <= CPU_FETCH;
                            end
                            default: begin
                                effective_addr <= R[xsel];
                                bus_size <= BUS_BYTE;
                                cpu_state <= CPU_MEMALU_RD;
                            end
                        endcase
                    end
                end

                // --------------------------------------------------------
                // IO (0x9)
                // --------------------------------------------------------
                else if (fam == FAM_IO) begin
                    ext_active <= 1'b0;
                    if (priv_level) begin
                        R[spsel] <= R[spsel]-64'd8; effective_addr <= R[spsel]-64'd8;
                        mem_data <= R[psel]; flags[6] <= 1'b0;
                        priv_level <= 1'b0; ivec_id <= {4'd0, IRQX_PRIV};
                        post_action <= POST_IRQ_VEC;
                        bus_size <= BUS_DWORD; cpu_state <= CPU_MEM_WRITE;
                    end else begin
                        if (nib >= 4'd1 && nib <= 4'd7) begin
                            io_port <= nib[2:0]; io_is_inp <= 1'b0;
                            effective_addr <= R[xsel]; bus_size <= BUS_BYTE;
                            cpu_state <= CPU_MEMALU_RD;
                            memalu_sub <= 4'hF;
                        end else if (nib >= 4'd9) begin
                            io_port <= nib[2:0]; io_is_inp <= 1'b1;
                            effective_addr <= {MP64_MMIO_HI, 20'd0, nib[2:0], 9'd0};
                            bus_size <= BUS_BYTE; cpu_state <= CPU_MEM_READ;
                            dst_reg <= 4'd0; mem_sub <= 4'hF;
                        end else
                            cpu_state <= CPU_FETCH;
                    end
                end

                // --------------------------------------------------------
                // SEP (0xA)
                // --------------------------------------------------------
                else if (fam == FAM_SEP) begin
                    ext_active <= 1'b0;
                    if (priv_level) begin
                        R[spsel]<=R[spsel]-64'd8; effective_addr<=R[spsel]-64'd8;
                        mem_data<=R[psel]; flags[6]<=1'b0; priv_level<=1'b0;
                        ivec_id<={4'd0,IRQX_PRIV}; post_action<=POST_IRQ_VEC;
                        bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_WRITE;
                    end else begin
                        psel <= nib; fetch_pc <= R[nib];
                        cpu_state <= CPU_FETCH;
                    end
                end

                // --------------------------------------------------------
                // SEX (0xB)
                // --------------------------------------------------------
                else if (fam == FAM_SEX) begin
                    ext_active <= 1'b0;
                    if (priv_level) begin
                        R[spsel]<=R[spsel]-64'd8; effective_addr<=R[spsel]-64'd8;
                        mem_data<=R[psel]; flags[6]<=1'b0; priv_level<=1'b0;
                        ivec_id<={4'd0,IRQX_PRIV}; post_action<=POST_IRQ_VEC;
                        bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_WRITE;
                    end else begin
                        xsel <= nib; cpu_state <= CPU_FETCH;
                    end
                end

                // --------------------------------------------------------
                // MULDIV (0xC) — hardware multiply/divide
                // --------------------------------------------------------
                else if (fam == FAM_MULDIV) begin
                    ext_active <= 1'b0;
                    dst_reg <= ibuf[1][7:4];
                    src_reg <= ibuf[1][3:0];
                    div_done_r <= 1'b0;
                    case (nib)
                        4'h0: begin // SMUL low
                            mul_op_a <= R[ibuf[1][7:4]];
                            mul_op_b <= R[ibuf[1][3:0]];
                            mul_is_signed_r <= 1'b1;
                            mul_start_r <= 1'b1;
                            cpu_state <= CPU_MULDIV;
                        end
                        4'h1: begin // SMUL high
                            mul_op_a <= R[ibuf[1][7:4]];
                            mul_op_b <= R[ibuf[1][3:0]];
                            mul_is_signed_r <= 1'b1;
                            mul_start_r <= 1'b1;
                            mem_sub <= 4'h1; cpu_state <= CPU_MULDIV;
                        end
                        4'h2: begin // UMUL low
                            mul_op_a <= R[ibuf[1][7:4]];
                            mul_op_b <= R[ibuf[1][3:0]];
                            mul_is_signed_r <= 1'b0;
                            mul_start_r <= 1'b1;
                            cpu_state <= CPU_MULDIV;
                        end
                        4'h3: begin // UMUL high
                            mul_op_a <= R[ibuf[1][7:4]];
                            mul_op_b <= R[ibuf[1][3:0]];
                            mul_is_signed_r <= 1'b0;
                            mul_start_r <= 1'b1;
                            mem_sub <= 4'h1; cpu_state <= CPU_MULDIV;
                        end
                        4'h4: begin // DIV (signed)
                            if (R[ibuf[1][3:0]] == 64'd0) begin
                                ivec_id <= IRQX_ILLEGAL_OP; cpu_state <= CPU_IRQ;
                            end else begin
                                div_signed_op <= 1'b1;
                                div_dividend  <= R[ibuf[1][7:4]][63]
                                    ? (~R[ibuf[1][7:4]] + 64'd1) : R[ibuf[1][7:4]];
                                div_divisor   <= R[ibuf[1][3:0]][63]
                                    ? (~R[ibuf[1][3:0]] + 64'd1) : R[ibuf[1][3:0]];
                                div_quotient  <= 64'd0;
                                div_remainder <= 64'd0;
                                div_cycle     <= 7'd0;
                                div_active    <= 1'b1;
                                div_op_nib    <= 4'h4;
                                cpu_state     <= CPU_MULDIV;
                            end
                        end
                        4'h5: begin // UDIV
                            if (R[ibuf[1][3:0]] == 64'd0) begin
                                ivec_id <= IRQX_ILLEGAL_OP; cpu_state <= CPU_IRQ;
                            end else begin
                                div_signed_op <= 1'b0;
                                div_dividend  <= R[ibuf[1][7:4]];
                                div_divisor   <= R[ibuf[1][3:0]];
                                div_quotient  <= 64'd0;
                                div_remainder <= 64'd0;
                                div_cycle     <= 7'd0;
                                div_active    <= 1'b1;
                                div_op_nib    <= 4'h5;
                                cpu_state     <= CPU_MULDIV;
                            end
                        end
                        4'h6: begin // MOD (signed)
                            if (R[ibuf[1][3:0]] == 64'd0) begin
                                ivec_id <= IRQX_ILLEGAL_OP; cpu_state <= CPU_IRQ;
                            end else begin
                                div_signed_op <= 1'b1;
                                div_dividend  <= R[ibuf[1][7:4]][63]
                                    ? (~R[ibuf[1][7:4]] + 64'd1) : R[ibuf[1][7:4]];
                                div_divisor   <= R[ibuf[1][3:0]][63]
                                    ? (~R[ibuf[1][3:0]] + 64'd1) : R[ibuf[1][3:0]];
                                div_quotient  <= 64'd0;
                                div_remainder <= 64'd0;
                                div_cycle     <= 7'd0;
                                div_active    <= 1'b1;
                                div_op_nib    <= 4'h6;
                                cpu_state     <= CPU_MULDIV;
                            end
                        end
                        4'h7: begin // UMOD
                            if (R[ibuf[1][3:0]] == 64'd0) begin
                                ivec_id <= IRQX_ILLEGAL_OP; cpu_state <= CPU_IRQ;
                            end else begin
                                div_signed_op <= 1'b0;
                                div_dividend  <= R[ibuf[1][7:4]];
                                div_divisor   <= R[ibuf[1][3:0]];
                                div_quotient  <= 64'd0;
                                div_remainder <= 64'd0;
                                div_cycle     <= 7'd0;
                                div_active    <= 1'b1;
                                div_op_nib    <= 4'h7;
                                cpu_state     <= CPU_MULDIV;
                            end
                        end
                        default: cpu_state <= CPU_FETCH;
                    endcase
                end

                // --------------------------------------------------------
                // CSR (0xD) — full CSR space
                // --------------------------------------------------------
                else if (fam == FAM_CSR) begin
                    ext_active <= 1'b0;
                    cpu_state  <= CPU_FETCH;
                    if (nib[3]) begin
                        // CSRW
                        csr_wen   <= 1'b1;
                        csr_addr  <= ibuf[1];
                        csr_wdata <= R[nib[2:0]];
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
                            CSR_PRIV:     if (!priv_level) priv_level <= R[nib[2:0]][0];
                            CSR_MPU_BASE: if (!priv_level) mpu_base  <= R[nib[2:0]];
                            CSR_MPU_LIMIT:if (!priv_level) mpu_limit <= R[nib[2:0]];
                            CSR_IVEC_ID:  ivec_id  <= R[nib[2:0]][7:0];
                            CSR_PERF_CTRL: begin
                                perf_enable <= R[nib[2:0]][0];
                                if (R[nib[2:0]][1]) begin
                                    perf_cycles <= 64'd0; perf_stalls  <= 64'd0;
                                    perf_tileops<= 64'd0; perf_extmem  <= 64'd0;
                                end
                            end
                            CSR_BIST_CMD: begin
                                if (R[nib[2:0]][1:0] != 2'd0 && bist_status != 2'd1) begin
                                    bist_status  <= 2'd1; bist_fail_addr <= 64'd0;
                                    bist_fail_data <= 64'd0; bist_pattern <= R[nib[2:0]][1:0];
                                    bist_phase <= 3'd0; bist_addr_cnt <= 20'd0;
                                    bist_running <= 1'b1; cpu_state <= CPU_BIST;
                                end
                            end
                            CSR_TILE_SELFTEST: begin
                                if (R[nib[2:0]][0] && tile_selftest != 2'd1) begin
                                    tile_selftest <= 2'd1; tile_st_detail <= 8'd0;
                                    tile_st_cnt <= 6'd32;
                                end
                            end
                            CSR_ICACHE_CTRL: begin
                                icache_enabled <= R[nib[2:0]][0];
                                if (R[nib[2:0]][1]) icache_inv_all <= 1'b1;
                            end
                            CSR_DMA_RING_BASE: dma_ring_base <= R[nib[2:0]];
                            CSR_DMA_RING_SIZE: dma_ring_size <= R[nib[2:0]];
                            CSR_DMA_HEAD:      dma_head      <= R[nib[2:0]];
                            CSR_DMA_TAIL:      dma_tail      <= R[nib[2:0]];
                            CSR_DMA_STATUS:    dma_status    <= R[nib[2:0]];
                            CSR_DMA_CTRL: begin
                                dma_ctrl <= R[nib[2:0]];
                                if (R[nib[2:0]][1]) begin
                                    dma_head <= 64'd0; dma_tail <= 64'd0; dma_status <= 64'd0;
                                end
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
                            CSR_IVTBASE:     R[nib[2:0]] <= ivt_base;
                            CSR_D:           R[nib[2:0]] <= {56'd0, D};
                            CSR_DF:          R[nib[2:0]] <= {63'd0, flags[1]};
                            CSR_QREG:        R[nib[2:0]] <= {63'd0, Q};
                            CSR_TREG:        R[nib[2:0]] <= {56'd0, T};
                            CSR_IE:          R[nib[2:0]] <= {63'd0, flags[6]};
                            CSR_PRIV:        R[nib[2:0]] <= {63'd0, priv_level};
                            CSR_MPU_BASE:    R[nib[2:0]] <= mpu_base;
                            CSR_MPU_LIMIT:   R[nib[2:0]] <= mpu_limit;
                            CSR_COREID:      R[nib[2:0]] <= {{(64-CORE_ID_W){1'b0}}, core_id};
                            CSR_NCORES:      R[nib[2:0]] <= 64'd16;  // updated at SoC level
                            CSR_IVEC_ID:     R[nib[2:0]] <= {56'd0, ivec_id};
                            CSR_TRAP_ADDR:   R[nib[2:0]] <= trap_addr;
                            CSR_MEGAPAD_SZ:  R[nib[2:0]] <= mem_size_bytes;
                            CSR_CPUID:       R[nib[2:0]] <= 64'h4D50_3634_0001_4350; // "MP64" v1 "CP"
                            CSR_PERF_CYCLES: R[nib[2:0]] <= perf_cycles;
                            CSR_PERF_STALLS: R[nib[2:0]] <= perf_stalls;
                            CSR_PERF_TILEOPS:R[nib[2:0]] <= perf_tileops;
                            CSR_PERF_EXTMEM: R[nib[2:0]] <= perf_extmem;
                            CSR_PERF_CTRL:   R[nib[2:0]] <= {63'd0, perf_enable};
                            CSR_BIST_STATUS: R[nib[2:0]] <= {62'd0, bist_status};
                            CSR_BIST_FAIL_ADDR: R[nib[2:0]] <= bist_fail_addr;
                            CSR_BIST_FAIL_DATA: R[nib[2:0]] <= bist_fail_data;
                            CSR_TILE_SELFTEST:  R[nib[2:0]] <= {62'd0, tile_selftest};
                            CSR_TILE_ST_DETAIL: R[nib[2:0]] <= {56'd0, tile_st_detail};
                            CSR_ICACHE_CTRL: R[nib[2:0]] <= {63'd0, icache_enabled};
                            CSR_ICACHE_HITS: R[nib[2:0]] <= icache_stat_hits;
                            CSR_ICACHE_MISSES:R[nib[2:0]] <= icache_stat_misses;
                            CSR_DMA_RING_BASE: R[nib[2:0]] <= dma_ring_base;
                            CSR_DMA_RING_SIZE: R[nib[2:0]] <= dma_ring_size;
                            CSR_DMA_HEAD:    R[nib[2:0]] <= dma_head;
                            CSR_DMA_TAIL:    R[nib[2:0]] <= dma_tail;
                            CSR_DMA_STATUS:  R[nib[2:0]] <= dma_status;
                            CSR_DMA_CTRL:    R[nib[2:0]] <= dma_ctrl;
                            default:         R[nib[2:0]] <= csr_rdata;
                        endcase
                    end
                end

                // --------------------------------------------------------
                // MEX (0xE) — tile engine dispatch
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
            // MEM_READ
            // ============================================================
            CPU_MEM_READ: begin
                if (mpu_fault && post_action == POST_NONE) begin
                    trap_addr <= effective_addr;
                    R[spsel]<=R[spsel]-64'd8; effective_addr<=R[spsel]-64'd8;
                    mem_data<=R[psel]; flags[6]<=1'b0; priv_level<=1'b0;
                    ivec_id<={4'd0,IRQX_PRIV}; post_action<=POST_IRQ_VEC;
                    bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_WRITE;
                end else begin
                    bus_valid <= 1'b1;
                    bus_addr  <= effective_addr;
                    bus_wen   <= 1'b0;
                    if (bus_ready) begin
                        bus_valid <= 1'b0;
                        case (mem_sub)
                            4'hC: R[dst_reg] <= {{56{bus_rdata[7]}}, bus_rdata[7:0]};
                            4'hD: R[dst_reg] <= {{48{bus_rdata[15]}}, bus_rdata[15:0]};
                            4'hE: R[dst_reg] <= {{32{bus_rdata[31]}}, bus_rdata[31:0]};
                            4'hF: begin
                                if (io_is_inp) begin
                                    D <= bus_rdata[7:0];
                                    effective_addr <= R[xsel];
                                    mem_data <= {56'd0, bus_rdata[7:0]};
                                    bus_size <= BUS_BYTE;
                                    cpu_state <= CPU_MEM_WRITE;
                                end else begin
                                    R[dst_reg] <= bus_rdata;
                                    if (dst_reg == psel) fetch_pc <= bus_rdata;
                                end
                            end
                            4'h5: begin // 1802 RET
                                T <= bus_rdata[7:0];
                                xsel <= bus_rdata[7:4]; psel <= bus_rdata[3:0];
                                flags[6] <= 1'b1;
                                fetch_pc <= R[bus_rdata[3:0]];
                                cpu_state <= CPU_FETCH;
                            end
                            4'hB: begin // DIS
                                T <= bus_rdata[7:0];
                                xsel <= bus_rdata[7:4]; psel <= bus_rdata[3:0];
                                flags[6] <= 1'b0;
                                fetch_pc <= R[bus_rdata[3:0]];
                                cpu_state <= CPU_FETCH;
                            end
                            default: begin
                                R[dst_reg] <= bus_rdata;
                                if (dst_reg == psel) fetch_pc <= bus_rdata;
                            end
                        endcase

                        if (mem_sub == 4'h1) R[src_reg] <= R[src_reg] + 64'd8;
                        if (mem_sub == 4'h3) R[xsel]    <= R[xsel]    + 64'd8;

                        if (post_action == POST_RTI_POP2) begin
                            effective_addr <= R[spsel];
                            R[spsel] <= R[spsel] + 64'd8;
                            bus_size <= BUS_DWORD;
                            post_action <= POST_NONE;
                            cpu_state <= CPU_MEM_READ2;
                        end else if ((mem_sub != 4'hF || !io_is_inp)
                                     && mem_sub != 4'h5 && mem_sub != 4'hB) begin
                            cpu_state <= CPU_FETCH;
                        end
                    end
                end
            end

            // ============================================================
            // MEM_READ2: RTI flags pop
            // ============================================================
            CPU_MEM_READ2: begin
                bus_valid <= 1'b1;
                bus_addr  <= effective_addr;
                bus_wen   <= 1'b0;
                if (bus_ready) begin
                    bus_valid <= 1'b0;
                    flags <= bus_rdata[7:0];
                    priv_level <= bus_rdata[8];
                    cpu_state <= CPU_FETCH;
                end
            end

            // ============================================================
            // MEM_WRITE
            // ============================================================
            CPU_MEM_WRITE: begin
                if (mpu_fault && post_action == POST_NONE) begin
                    trap_addr <= effective_addr;
                    R[spsel]<=R[spsel]-64'd8; effective_addr<=R[spsel]-64'd8;
                    mem_data<=R[psel]; flags[6]<=1'b0; priv_level<=1'b0;
                    ivec_id<={4'd0,IRQX_PRIV}; post_action<=POST_IRQ_VEC;
                    bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_WRITE;
                end else begin
                    bus_valid <= 1'b1;
                    bus_addr  <= effective_addr;
                    bus_wdata <= mem_data;
                    bus_wen   <= 1'b1;
                    if (bus_ready) begin
                        bus_valid <= 1'b0;
                        if (icache_enabled) begin
                            icache_inv_line <= 1'b1;
                            icache_inv_addr <= effective_addr;
                        end
                        if (mem_sub == 4'h5) R[xsel] <= R[xsel] - 64'd8;
                        if (post_action == POST_IRQ_VEC) begin
                            R[spsel] <= R[spsel] - 64'd8;
                            effective_addr <= R[spsel] - 64'd8;
                            mem_data <= {55'd0, priv_level, flags};
                            post_action <= POST_NONE;
                            cpu_state <= CPU_IRQ_PUSH;
                        end else
                            cpu_state <= CPU_FETCH;
                    end
                end
            end

            // ============================================================
            // IRQ_PUSH: push flags+priv, then load IVT vector
            // ============================================================
            CPU_IRQ_PUSH: begin
                bus_valid <= 1'b1;
                bus_addr  <= effective_addr;
                bus_wdata <= mem_data;
                bus_wen   <= 1'b1;
                bus_size  <= BUS_DWORD;
                if (bus_ready) begin
                    bus_valid <= 1'b0;
                    effective_addr <= ivt_base + {56'd0, ivec_id, 3'b000};
                    bus_size <= BUS_DWORD;
                    cpu_state <= CPU_IRQ_LOAD;
                end
            end

            // ============================================================
            // IRQ_LOAD: read IVT vector, jump
            // ============================================================
            CPU_IRQ_LOAD: begin
                bus_valid <= 1'b1;
                bus_addr  <= effective_addr;
                bus_wen   <= 1'b0;
                bus_size  <= BUS_DWORD;
                if (bus_ready) begin
                    bus_valid <= 1'b0;
                    R[psel] <= bus_rdata;
                    fetch_pc <= bus_rdata;
                    cpu_state <= CPU_FETCH;
                end
            end

            // ============================================================
            // MEMALU_RD: read M(R(X)) for MEMALU/IO
            // ============================================================
            CPU_MEMALU_RD: begin
                bus_valid <= 1'b1;
                bus_addr  <= effective_addr;
                bus_wen   <= 1'b0;
                bus_size  <= BUS_BYTE;
                if (bus_ready) begin
                    bus_valid <= 1'b0;
                    memalu_byte <= bus_rdata[7:0];
                    if (memalu_sub == 4'hF) begin
                        // IO OUT
                        D <= bus_rdata[7:0];
                        effective_addr <= {MP64_MMIO_HI, 20'd0, io_port, 9'd0};
                        mem_data <= {56'd0, bus_rdata[7:0]};
                        bus_size <= BUS_BYTE;
                        R[xsel] <= R[xsel] + 64'd1;
                        cpu_state <= CPU_MEM_WRITE;
                        mem_sub <= 4'd0;
                    end else begin
                        case (memalu_sub)
                            4'h0: D <= bus_rdata[7:0];                     // LDX
                            4'h1: begin                                    // OR.X
                                D <= bus_rdata[7:0] | D;
                                flags[0] <= ((bus_rdata[7:0] | D) == 8'd0);
                            end
                            4'h2: begin                                    // AND.X
                                D <= bus_rdata[7:0] & D;
                                flags[0] <= ((bus_rdata[7:0] & D) == 8'd0);
                            end
                            4'h3: begin                                    // XOR.X
                                D <= bus_rdata[7:0] ^ D;
                                flags[0] <= ((bus_rdata[7:0] ^ D) == 8'd0);
                            end
                            4'h4: begin                                    // ADD.X
                                {flags[1], D} <= {1'b0, bus_rdata[7:0]} + {1'b0, D};
                                flags[0] <= (({1'b0, bus_rdata[7:0]} + {1'b0, D}) == 9'd0);
                            end
                            4'h5: begin                                    // SD.X
                                {flags[1], D} <= {1'b0, bus_rdata[7:0]} - {1'b0, D};
                                flags[0] <= (({1'b0, bus_rdata[7:0]} - {1'b0, D}) == 9'd0);
                                flags[1] <= ~({1'b0, bus_rdata[7:0]} - {1'b0, D} > 9'h1FF);
                            end
                            4'h7: begin                                    // SM.X
                                {flags[1], D} <= {1'b0, D} - {1'b0, bus_rdata[7:0]};
                                flags[0] <= (({1'b0, D} - {1'b0, bus_rdata[7:0]}) == 9'd0);
                                flags[1] <= ~({1'b0, D} - {1'b0, bus_rdata[7:0]} > 9'h1FF);
                            end
                            4'h8: begin                                    // ADC.X
                                {flags[1], D} <= {1'b0, bus_rdata[7:0]} + {1'b0, D} + {8'd0, flags[1]};
                                flags[0] <= (({1'b0, bus_rdata[7:0]} + {1'b0, D} + {8'd0, flags[1]}) == 9'd0);
                            end
                            4'h9: begin                                    // SDB.X
                                {flags[1], D} <= {1'b0, bus_rdata[7:0]} - {1'b0, D} - {8'd0, ~flags[1]};
                                flags[0] <= (({1'b0, bus_rdata[7:0]} - {1'b0, D} - {8'd0, ~flags[1]}) == 9'd0);
                                flags[1] <= ~({1'b0, bus_rdata[7:0]} - {1'b0, D} - {8'd0, ~flags[1]} > 9'h1FF);
                            end
                            4'hB: begin                                    // SMB.X
                                {flags[1], D} <= {1'b0, D} - {1'b0, bus_rdata[7:0]} - {8'd0, ~flags[1]};
                                flags[0] <= (({1'b0, D} - {1'b0, bus_rdata[7:0]} - {8'd0, ~flags[1]}) == 9'd0);
                                flags[1] <= ~({1'b0, D} - {1'b0, bus_rdata[7:0]} - {8'd0, ~flags[1]} > 9'h1FF);
                            end
                            4'hF: begin D <= bus_rdata[7:0]; R[xsel] <= R[xsel]+64'd1; end // LDXA
                            default: ;
                        endcase
                        cpu_state <= CPU_FETCH;
                    end
                end
            end

            // ============================================================
            // MULDIV: wait for multiplier or divider, then writeback
            // ============================================================
            CPU_MULDIV: begin
                // Deassert start after one cycle
                mul_start_r <= 1'b0;

                // --- Iterative divider: 1-bit restoring, 64 cycles ---
                if (div_active) begin
                    if (div_cycle <= 7'd63) begin
                        div_remainder <= {div_remainder[62:0],
                                         div_dividend[63 - div_cycle[5:0]]};
                        if ({div_remainder[62:0],
                             div_dividend[63 - div_cycle[5:0]]} >= div_divisor)
                        begin
                            div_remainder <= {div_remainder[62:0],
                                div_dividend[63 - div_cycle[5:0]]} - div_divisor;
                            div_quotient[63 - div_cycle[5:0]] <= 1'b1;
                        end
                        div_cycle <= div_cycle + 7'd1;
                    end else begin
                        // Division complete — apply sign correction and write back
                        div_active <= 1'b0;
                        div_done_r <= 1'b1;
                        case (div_op_nib)
                            4'h4: begin // DIV (signed): Rd=quotient, R0=remainder
                                if (div_signed_op && (mul_op_a[63] ^ mul_op_b[63]))
                                    R[dst_reg] <= ~div_quotient + 64'd1;
                                else
                                    R[dst_reg] <= div_quotient;
                                if (div_signed_op && mul_op_a[63])
                                    R[0] <= ~div_remainder + 64'd1;
                                else
                                    R[0] <= div_remainder;
                            end
                            4'h5: begin // UDIV: Rd=quotient, R0=remainder
                                R[dst_reg] <= div_quotient;
                                R[0]       <= div_remainder;
                            end
                            4'h6: begin // MOD (signed): Rd=remainder
                                if (div_signed_op && mul_op_a[63])
                                    R[dst_reg] <= ~div_remainder + 64'd1;
                                else
                                    R[dst_reg] <= div_remainder;
                            end
                            4'h7: begin // UMOD: Rd=remainder
                                R[dst_reg] <= div_remainder;
                            end
                        endcase
                        flags[0] <= (div_quotient == 64'd0);
                        flags[2] <= div_quotient[63];
                        mem_sub  <= 4'd0;
                        cpu_state <= CPU_FETCH;
                    end
                end

                // --- Multiply: wait for mp64_mul done ---
                else if (mul_u_done) begin
                    mul_result <= mul_u_result;
                    if (mem_sub == 4'h1)
                        R[dst_reg] <= mul_u_result[127:64];
                    else
                        R[dst_reg] <= mul_u_result[63:0];
                    flags[0] <= (mul_u_result[63:0] == 64'd0);
                    flags[2] <= mul_u_result[63];
                    mem_sub  <= 4'd0;
                    cpu_state <= CPU_FETCH;
                end
            end

            // ============================================================
            // MEX_WAIT
            // ============================================================
            CPU_MEX_WAIT: begin
                if (mex_done) cpu_state <= CPU_FETCH;
            end

            // ============================================================
            // IRQ: push PC, enter vector
            // ============================================================
            CPU_IRQ: begin
                R[spsel] <= R[spsel] - 64'd8;
                effective_addr <= R[spsel] - 64'd8;
                mem_data <= R[psel];
                bus_size <= BUS_DWORD;
                flags[6] <= 1'b0;
                priv_level <= 1'b0;
                ivec_id <= {4'd0, irq_vector};
                post_action <= POST_IRQ_VEC;
                cpu_state <= CPU_MEM_WRITE;
            end

            // ============================================================
            // HALT: wait for interrupt
            // ============================================================
            CPU_HALT: begin
                if (irq_pending) cpu_state <= CPU_IRQ;
            end

            // ============================================================
            // SKIP: read byte0 of next instr via I-cache, advance PC
            // ============================================================
            CPU_SKIP: begin
                if (!icache_stall) begin
                    icache_req  <= 1'b1;
                    icache_addr <= R[psel];
                end
                if (icache_hit) begin
                    icache_req <= 1'b0;
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

            // ============================================================
            // BIST: memory self-test via bus
            // ============================================================
            CPU_BIST: begin
                begin : bist_block
                    reg [63:0] bist_wdata;
                    reg [63:0] bist_expect;
                    reg [63:0] bist_byte_addr;

                    bist_byte_addr = {41'd0, bist_addr_cnt, 3'b000} + 64'h1000;

                    case (bist_pattern)
                        2'd1: bist_wdata = (bist_phase <= 3'd1) ? 64'd0 : 64'hFFFF_FFFF_FFFF_FFFF;
                        2'd2: bist_wdata = (bist_phase <= 3'd1) ? 64'hAAAA_AAAA_AAAA_AAAA : 64'h5555_5555_5555_5555;
                        2'd3: bist_wdata = bist_byte_addr;
                        default: bist_wdata = 64'd0;
                    endcase
                    bist_expect = bist_wdata;

                    case (bist_phase)
                        3'd0, 3'd2: begin
                            bus_valid <= 1'b1; bus_addr <= bist_byte_addr;
                            bus_wen <= 1'b1; bus_wdata <= bist_wdata;
                            bus_size <= BUS_DWORD;
                            if (bus_ready) begin
                                bus_valid <= 1'b0;
                                if (bist_addr_cnt == 20'd255) begin
                                    bist_addr_cnt <= 20'd0; bist_phase <= bist_phase + 3'd1;
                                end else
                                    bist_addr_cnt <= bist_addr_cnt + 20'd1;
                            end
                        end
                        3'd1, 3'd3: begin
                            bus_valid <= 1'b1; bus_addr <= bist_byte_addr;
                            bus_wen <= 1'b0; bus_size <= BUS_DWORD;
                            if (bus_ready) begin
                                bus_valid <= 1'b0;
                                if (bus_rdata != bist_expect) begin
                                    bist_status <= 2'd3; bist_fail_addr <= bist_byte_addr;
                                    bist_fail_data <= bus_rdata;
                                    bist_running <= 1'b0; cpu_state <= CPU_FETCH;
                                end else if (bist_addr_cnt == 20'd255) begin
                                    bist_addr_cnt <= 20'd0;
                                    if ((bist_pattern == 2'd3 && bist_phase == 3'd1)
                                        || bist_phase == 3'd3) begin
                                        bist_status <= 2'd2; bist_running <= 1'b0;
                                        cpu_state <= CPU_FETCH;
                                    end else
                                        bist_phase <= bist_phase + 3'd1;
                                end else
                                    bist_addr_cnt <= bist_addr_cnt + 20'd1;
                            end
                        end
                        default: begin
                            bist_status <= 2'd2; bist_running <= 1'b0; cpu_state <= CPU_FETCH;
                        end
                    endcase
                end
            end

            endcase
        end
    end

endmodule
