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
    output reg         bus_port_io,  // asserted during OUT/INP memory cycles
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
    input  wire        irq_bus,

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
    reg [63:0] R [0:31];
    reg [4:0]  psel, xsel, spsel;     // PC / X / SP selectors (5-bit for 32 regs)
    reg [7:0]  flags;                 // [S I G P V N C Z]

    wire [63:0] PC = R[psel];
    wire [63:0] SP = R[spsel];
    wire [63:0] RX = R[xsel];

    // 1802 heritage
    reg [7:0]  D;                     // 8-bit accumulator
    reg        Q;                     // Q flip-flop
    reg [15:0] T;                     // saved X|P on MARK (widened for 5-bit selectors)

    // IVT / trap
    reg [63:0] ivt_base;
    reg [7:0]  ivec_id;
    reg [63:0] trap_addr;

    // Privilege / MPU  (priv_level retained as inert CSR — no enforcement)
    reg        priv_level;            // 0=supervisor, 1=user (NOT ENFORCED)
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

    // REX extension bits — active only when ext_active and ext_mod in REX range
    wire rex_s = ext_active & ext_mod[0];   // source high bit
    wire rex_d = ext_active & ext_mod[1];   // dest high bit
    wire rex_n = ext_active & ext_mod[2];   // nibble high bit

    wire [4:0] nib5 = {rex_n, nib};
    wire [4:0] dst5 = {rex_d, ibuf[1][7:4]};
    wire [4:0] src5 = {rex_s, ibuf[1][3:0]};

    // ====================================================================
    // CPU FSM — extra state for BIST
    // ====================================================================
    localparam CPU_BIST = 5'd8;       // reuse FETCH_MORE slot (BIST is major only)

    reg [4:0] cpu_state;

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
            else if (irq_bus)   begin irq_pending = 1'b1; irq_vector = IRQX_BUS;             end
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
    // Bitfield ALU instance (Tier 2 enabled — major core)
    // ====================================================================
    reg  [2:0]  bf_op;
    reg  [63:0] bf_a, bf_b;
    reg  [5:0]  bf_imm;
    wire [63:0] bf_result;
    wire        bf_flag_z, bf_flag_n;
    reg         bf_active;  // set during DECODE, checked in EXECUTE

    mp64_bitfield #(.ENABLE_TIER2(1)) u_bitfield (
        .op     (bf_op),
        .a      (bf_a),
        .b      (bf_b),
        .imm    (bf_imm),
        .result (bf_result),
        .flag_z (bf_flag_z),
        .flag_n (bf_flag_n)
    );

    // ====================================================================
    // CRC ISA instance (per-core, combinational)
    // ====================================================================
    reg  [3:0]  crypto_unit_r, crypto_op_r;
    reg  [4:0]  crypto_rd_r, crypto_rs_r;
    reg  [7:0]  crypto_imm_r;
    reg         crypto_active;
    reg  [63:0] crc_acc;
    reg  [1:0]  crc_mode;

    wire [63:0] crc_acc_out, crc_result;
    wire [1:0]  crc_mode_out;
    wire        crc_acc_we, crc_mode_we, crc_rd_we;

    mp64_crc_isa u_crc_isa (
        .op          (crypto_op_r),
        .rs_val      (R[crypto_rs_r]),
        .imm8        (crypto_imm_r),
        .crc_acc_in  (crc_acc),
        .crc_mode_in (crc_mode),
        .crc_acc_out (crc_acc_out),
        .crc_mode_out(crc_mode_out),
        .result      (crc_result),
        .acc_we      (crc_acc_we),
        .mode_we     (crc_mode_we),
        .rd_we       (crc_rd_we)
    );

    // ====================================================================
    // SHA-2 ISA instance (per-core, 64-cycle sequential)
    // ====================================================================
    reg  [1:0]  sha_mode;        // 0=SHA-256 (only mode in RTL)
    reg  [63:0] sha_msglen_lo;   // total message length, bits (low 64)
    reg  [63:0] sha_msglen_hi;   // high 64

    // Local ACC shadow registers (authoritative copy in CPU)
    reg  [63:0] acc_reg [0:3];   // ACC0..ACC3

    // Local shadow of TSRC0 for SHA.ROUND bus reads
    reg  [63:0] sha_tsrc0;

    // SHA W buffer (loaded from tile memory before compression)
    reg  [31:0] sha_w_buf [0:15];
    reg  [3:0]  sha_load_cnt;   // 0..7 for 8 × 64-bit bus reads

    // Unpack ACC → 8 × 32-bit H for SHA engine
    wire [31:0] sha_h_unpack [0:7];
    assign sha_h_unpack[0] = acc_reg[0][63:32];  // a
    assign sha_h_unpack[1] = acc_reg[0][31:0];   // b
    assign sha_h_unpack[2] = acc_reg[1][63:32];  // c
    assign sha_h_unpack[3] = acc_reg[1][31:0];   // d
    assign sha_h_unpack[4] = acc_reg[2][63:32];  // e
    assign sha_h_unpack[5] = acc_reg[2][31:0];   // f
    assign sha_h_unpack[6] = acc_reg[3][63:32];  // g
    assign sha_h_unpack[7] = acc_reg[3][31:0];   // h

    // SHA engine outputs
    wire [31:0] sha_h_out [0:7];
    wire        sha_h_we;
    wire        sha_busy;
    wire        sha_done;

    reg         sha_start_r;

    mp64_sha2_isa u_sha2_isa (
        .clk     (clk),
        .rst_n   (~rst),
        .start   (sha_start_r),
        .w_in    (sha_w_buf),
        .h_in    (sha_h_unpack),
        .h_out   (sha_h_out),
        .h_we    (sha_h_we),
        .busy    (sha_busy),
        .done    (sha_done)
    );

    // ====================================================================
    // Multi-cycle temporaries
    // ====================================================================
    reg [63:0] mem_data;
    reg [4:0]  dst_reg, src_reg;
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
    reg        io_out_active;
    reg [2:0]  post_action;

    // String engine state
    reg        string_start_r;
    reg [3:0]  string_op_r;
    reg [4:0]  string_dst_reg;
    reg [4:0]  string_src_reg;

    // String engine wires
    wire        str_done;
    wire [63:0] str_out_src, str_out_dst, str_out_len, str_out_result;
    wire        str_flag_z, str_flag_g;
    wire        str_bus_req, str_bus_wr;
    wire [63:0] str_bus_addr, str_bus_wdata;

    mp64_string u_string (
        .clk       (clk),
        .rst       (rst),
        .start     (string_start_r),
        .op        (string_op_r),
        .src_addr  (R[string_src_reg]),
        .dst_addr  (R[string_dst_reg]),
        .length    ((string_op_r == 4'h02) ? R[string_src_reg] : R[0]),
        .fill_byte (D),
        .done      (str_done),
        .out_src   (str_out_src),
        .out_dst   (str_out_dst),
        .out_len   (str_out_len),
        .out_result(str_out_result),
        .flag_z    (str_flag_z),
        .flag_g    (str_flag_g),
        .bus_req   (str_bus_req),
        .bus_wr    (str_bus_wr),
        .bus_addr  (str_bus_addr),
        .bus_wdata (str_bus_wdata),
        .bus_ack   (bus_ready),
        .bus_rdata (bus_rdata)
    );

    // Dict engine state
    reg        dict_start_r;
    reg [3:0]  dict_op_r;
    reg [4:0]  dict_dst_reg;
    reg [4:0]  dict_src_reg;

    // Dict engine wires
    wire        dict_done;
    wire [63:0] dict_xt_out;
    wire        dict_flag_z, dict_flag_v;
    wire        dict_bus_req, dict_bus_wr;
    wire [63:0] dict_bus_addr, dict_bus_wdata;

    mp64_dict u_dict (
        .clk        (clk),
        .rst        (rst),
        .start      (dict_start_r),
        .op         (dict_op_r),
        .name_addr  (R[dict_src_reg]),
        .xt_in      (R[dict_dst_reg]),
        .done       (dict_done),
        .xt_out     (dict_xt_out),
        .flag_z     (dict_flag_z),
        .flag_v     (dict_flag_v),
        .bus_req    (dict_bus_req),
        .bus_wr     (dict_bus_wr),
        .bus_addr   (dict_bus_addr),
        .bus_wdata  (dict_bus_wdata),
        .bus_ack    (bus_ready),
        .bus_rdata  (bus_rdata),
        // Broadcast — tie off for single-core; SoC wires in multi-core
        .bcast_valid(),
        .bcast_hash (),
        .bcast_name_len(),
        .bcast_name (),
        .bcast_xt   (),
        .bcast_ack  (1'b1),
        .snoop_valid(1'b0),
        .snoop_hash (32'd0),
        .snoop_name_len(5'd0),
        .snoop_name (248'd0),
        .snoop_xt   (64'd0),
        .snoop_ack  ()
    );

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

            psel   <= 5'd3;
            xsel   <= 5'd2;
            spsel  <= 5'd15;
            flags  <= 8'h40;          // I=1

            ivt_base   <= 64'd0;
            ivec_id    <= 8'd0;
            trap_addr  <= 64'd0;
            priv_level <= 1'b0;
            mpu_base   <= 64'd0;
            mpu_limit  <= 64'd0;
            D          <= 8'd0;
            Q          <= 1'b0;
            T          <= 16'd0;

            post_action <= POST_NONE;
            mem_sub     <= 4'd0;
            memalu_sub  <= 4'd0;
            memalu_byte <= 8'd0;
            io_port     <= 3'd0;
            io_is_inp   <= 1'b0;
            io_out_active <= 1'b0;
            bus_port_io   <= 1'b0;

            string_start_r <= 1'b0;
            string_op_r    <= 4'd0;
            string_dst_reg <= 5'd0;
            string_src_reg <= 5'd0;

            dict_start_r   <= 1'b0;
            dict_op_r      <= 4'd0;
            dict_dst_reg   <= 5'd0;
            dict_src_reg   <= 5'd0;

            alu_op <= 4'd0;
            alu_a  <= 64'd0;
            alu_b  <= 64'd0;

            bf_op     <= 3'd0;
            bf_a      <= 64'd0;
            bf_b      <= 64'd0;
            bf_imm    <= 6'd0;
            bf_active <= 1'b0;

            crypto_unit_r  <= 4'd0;
            crypto_op_r    <= 4'd0;
            crypto_rd_r    <= 5'd0;
            crypto_rs_r    <= 5'd0;
            crypto_imm_r   <= 8'd0;
            crypto_active  <= 1'b0;
            crc_acc        <= 64'hFFFF_FFFF;
            crc_mode       <= 2'd0;

            acc_reg[0]     <= 64'd0; acc_reg[1] <= 64'd0;
            acc_reg[2]     <= 64'd0; acc_reg[3] <= 64'd0;
            sha_mode       <= 2'd0;
            sha_msglen_lo  <= 64'd0;
            sha_msglen_hi  <= 64'd0;
            sha_tsrc0      <= 64'd0;
            sha_load_cnt   <= 4'd0;
            sha_start_r    <= 1'b0;

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
            R[16] <= 64'd0; R[17] <= 64'd0; R[18] <= 64'd0; R[19] <= 64'd0;
            R[20] <= 64'd0; R[21] <= 64'd0; R[22] <= 64'd0; R[23] <= 64'd0;
            R[24] <= 64'd0; R[25] <= 64'd0; R[26] <= 64'd0; R[27] <= 64'd0;
            R[28] <= 64'd0; R[29] <= 64'd0; R[30] <= 64'd0; R[31] <= 64'd0;

        end else begin
            bus_valid      <= 1'b0;
            bus_port_io    <= 1'b0;
            csr_wen        <= 1'b0;
            mex_valid      <= 1'b0;
            icache_inv_all <= 1'b0;
            icache_inv_line<= 1'b0;
            sha_start_r    <= 1'b0;

            // Performance counters
            if (perf_enable) begin
                perf_cycles <= perf_cycles + 64'd1;
                if ((cpu_state == CPU_FETCH   && icache_stall) ||
                    (cpu_state == CPU_MEM_READ  && !bus_ready) ||
                    (cpu_state == CPU_MEM_WRITE && !bus_ready) ||
                    (cpu_state == CPU_MEM_READ2 && !bus_ready) ||
                    (cpu_state == CPU_MEMALU_RD && !bus_ready) ||
                    (cpu_state == CPU_IRQ_PUSH  && !bus_ready) ||
                    (cpu_state == CPU_IRQ_LOAD  && !bus_ready) ||
                    (cpu_state == CPU_SHA_LOAD  && !bus_ready) ||
                    (cpu_state == CPU_SHA_WAIT  && !sha_done))
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
                bf_active   <= 1'b0;

                // --------------------------------------------------------
                // EXT prefix (0xF)
                // --------------------------------------------------------
                if (fam == FAM_EXT) begin
                    if (nib == EXT_STRING) begin
                        // EXT.STRING — 3-byte self-contained instruction
                        //   ibuf[1] = sub-op (00–04)
                        //   ibuf[2] = [Rd:4][Rs:4]
                        string_op_r    <= ibuf[1][3:0];
                        string_dst_reg <= {rex_d, ibuf[2][7:4]};
                        string_src_reg <= {rex_s, ibuf[2][3:0]};
                        string_start_r <= 1'b1;
                        ext_active     <= 1'b0;
                        cpu_state      <= CPU_STRING;
                    end else if (nib == EXT_DICT) begin
                        // EXT.DICT — 3-byte instruction (2 for DCLR)
                        //   ibuf[1] = sub-op (00–03)
                        //   ibuf[2] = [Rd:4][Rs:4]  (ignored for DCLR)
                        dict_op_r      <= ibuf[1][3:0];
                        dict_dst_reg   <= {rex_d, ibuf[2][7:4]};
                        dict_src_reg   <= {rex_s, ibuf[2][3:0]};
                        dict_start_r   <= 1'b1;
                        ext_active     <= 1'b0;
                        cpu_state      <= CPU_DICT;
                    end else if (nib == EXT_CRYPTO) begin
                        // EXT.CRYPTO — 2 or 3-byte instruction
                        //   ibuf[1] = sub-op: [7:4]=unit, [3:0]=op
                        //   ibuf[2] = DR or imm8 (3-byte ops only)
                        crypto_unit_r  <= ibuf[1][7:4];
                        crypto_op_r    <= ibuf[1][3:0];
                        crypto_rd_r    <= {rex_d, ibuf[2][7:4]};
                        crypto_rs_r    <= {rex_s, ibuf[2][3:0]};
                        crypto_imm_r   <= ibuf[2];
                        crypto_active  <= 1'b1;
                        ext_active     <= 1'b0;
                        cpu_state      <= CPU_EXECUTE;
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
                            psel <= 5'd3; xsel <= 5'd2; spsel <= 5'd15;
                            flags <= 8'h40; priv_level <= 1'b0;
                            D <= 8'd0; Q <= 1'b0; T <= 16'd0;
                            ivt_base <= 64'd0; ivec_id <= 8'd0;
                            R[0] <= 64'd0; R[1] <= 64'd0; R[2] <= 64'd0; R[3] <= 64'd0;
                            R[4] <= 64'd0; R[5] <= 64'd0; R[6] <= 64'd0; R[7] <= 64'd0;
                            R[8] <= 64'd0; R[9] <= 64'd0; R[10]<= 64'd0; R[11]<= 64'd0;
                            R[12]<= 64'd0; R[13]<= 64'd0; R[14]<= 64'd0; R[15]<= 64'd0;
                            R[16]<= 64'd0; R[17]<= 64'd0; R[18]<= 64'd0; R[19]<= 64'd0;
                            R[20]<= 64'd0; R[21]<= 64'd0; R[22]<= 64'd0; R[23]<= 64'd0;
                            R[24]<= 64'd0; R[25]<= 64'd0; R[26]<= 64'd0; R[27]<= 64'd0;
                            R[28]<= 64'd0; R[29]<= 64'd0; R[30]<= 64'd0; R[31]<= 64'd0;
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
                            dst_reg  <= 5'd0;
                            mem_sub  <= 4'h5;
                            cpu_state <= CPU_MEM_READ;
                        end

                        4'h6: begin // DIS
                            effective_addr <= R[spsel];
                            R[spsel] <= R[spsel] + 64'd8;
                            dst_reg  <= 5'd0;
                            mem_sub  <= 4'hB;
                            cpu_state <= CPU_MEM_READ;
                        end

                        4'h7: begin // MARK
                            T <= {3'b0, xsel, 3'b0, psel};
                            R[spsel] <= R[spsel] - 64'd8;
                            effective_addr <= R[spsel] - 64'd8;
                            mem_data <= {48'd0, 3'b0, xsel, 3'b0, psel};
                            xsel <= psel;
                            cpu_state <= CPU_MEM_WRITE;
                        end

                        4'h8: begin // SAV
                            effective_addr <= R[xsel];
                            mem_data <= {48'd0, T};
                            bus_size <= BUS_HALF;
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
                            R[psel]  <= R[src5];
                            fetch_pc <= R[src5];
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
                    R[nib5] <= R[nib5] + 64'd1;
                    cpu_state <= CPU_FETCH;
                end

                // --------------------------------------------------------
                // DEC (0x2)
                // --------------------------------------------------------
                else if (fam == FAM_DEC) begin
                    ext_active <= 1'b0;
                    R[nib5] <= R[nib5] - 64'd1;
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
                    dst_reg <= dst5;
                    src_reg <= src5;
                    mem_sub <= nib;
                    case (nib)
                        4'h0: begin effective_addr<=R[src5]; bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_READ; end
                        4'h1: begin effective_addr<=R[src5]; bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_READ; end
                        4'h2: begin effective_addr<=R[xsel];         bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_READ; end
                        4'h3: begin effective_addr<=R[xsel];         bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_READ; end
                        4'h4: begin effective_addr<=R[dst5]; mem_data<=R[src5]; bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_WRITE; end
                        4'h5: begin effective_addr<=R[xsel]; mem_data<=R[dst5]; bus_size<=BUS_DWORD; cpu_state<=CPU_MEM_WRITE; end
                        4'h6: begin effective_addr<=R[src5]; bus_size<=BUS_BYTE;  cpu_state<=CPU_MEM_READ; end
                        4'h7: begin effective_addr<=R[dst5]; mem_data<={56'd0,R[src5][7:0]}; bus_size<=BUS_BYTE; cpu_state<=CPU_MEM_WRITE; end
                        4'h8: begin effective_addr<=R[src5]; bus_size<=BUS_HALF;  cpu_state<=CPU_MEM_READ; end
                        4'h9: begin effective_addr<=R[dst5]; mem_data<={48'd0,R[src5][15:0]}; bus_size<=BUS_HALF; cpu_state<=CPU_MEM_WRITE; end
                        4'hA: begin effective_addr<=R[src5]; bus_size<=BUS_WORD;  cpu_state<=CPU_MEM_READ; end
                        4'hB: begin effective_addr<=R[dst5]; mem_data<={32'd0,R[src5][31:0]}; bus_size<=BUS_WORD; cpu_state<=CPU_MEM_WRITE; end
                        4'hC: begin effective_addr<=R[src5]; bus_size<=BUS_BYTE;  cpu_state<=CPU_MEM_READ; end
                        4'hD: begin effective_addr<=R[src5]; bus_size<=BUS_HALF;  cpu_state<=CPU_MEM_READ; end
                        4'hE: begin effective_addr<=R[src5]; bus_size<=BUS_WORD;  cpu_state<=CPU_MEM_READ; end
                        4'hF: begin
                            effective_addr <= R[src5] + ({{56{ibuf[2][7]}}, ibuf[2]} << 3);
                            bus_size <= BUS_DWORD; cpu_state <= CPU_MEM_READ;
                        end
                    endcase
                end

                // --------------------------------------------------------
                // IMM (0x6) — 16 sub-ops
                // --------------------------------------------------------
                else if (fam == FAM_IMM) begin
                    dst_reg <= dst5;
                    if (ext_active && ext_mod == EXT_IMM64) begin
                        R[dst5] <= {ibuf[9], ibuf[8], ibuf[7], ibuf[6],
                                             ibuf[5], ibuf[4], ibuf[3], ibuf[2]};
                        ext_active <= 1'b0;
                        cpu_state  <= CPU_FETCH;
                    end else begin
                        ext_active <= 1'b0;
                        case (nib)
                            4'h0: begin R[dst5] <= {56'd0, ibuf[2]}; cpu_state <= CPU_FETCH; end
                            4'h1: begin R[dst5][63:48] <= {ibuf[3], ibuf[2]}; cpu_state <= CPU_FETCH; end
                            4'h2: begin alu_a<=R[dst5]; alu_b<={{56{ibuf[2][7]}},ibuf[2]}; alu_op<=ALU_ADD; cpu_state<=CPU_EXECUTE; end
                            4'h3: begin alu_a<=R[dst5]; alu_b<={56'd0,ibuf[2]}; alu_op<=ALU_AND; cpu_state<=CPU_EXECUTE; end
                            4'h4: begin alu_a<=R[dst5]; alu_b<={56'd0,ibuf[2]}; alu_op<=ALU_OR;  cpu_state<=CPU_EXECUTE; end
                            4'h5: begin alu_a<=R[dst5]; alu_b<={56'd0,ibuf[2]}; alu_op<=ALU_XOR; cpu_state<=CPU_EXECUTE; end
                            4'h6: begin alu_a<=R[dst5]; alu_b<={{56{ibuf[2][7]}},ibuf[2]}; alu_op<=ALU_CMP; cpu_state<=CPU_EXECUTE; end
                            4'h7: begin alu_a<=R[dst5]; alu_b<={{56{ibuf[2][7]}},ibuf[2]}; alu_op<=ALU_SUB; cpu_state<=CPU_EXECUTE; end
                            4'h8: begin R[dst5] <= R[dst5] << ibuf[1][3:0]; cpu_state <= CPU_FETCH; end
                            4'h9: begin R[dst5] <= R[dst5] >> ibuf[1][3:0]; cpu_state <= CPU_FETCH; end
                            4'hA: begin R[dst5] <= $signed(R[dst5]) >>> ibuf[1][3:0]; cpu_state <= CPU_FETCH; end
                            4'hB: begin
                                if (ibuf[1][3:0] != 0)
                                    R[dst5] <= (R[dst5] << ibuf[1][3:0])
                                                     | (R[dst5] >> (4'd0 - ibuf[1][3:0]));
                                cpu_state <= CPU_FETCH;
                            end
                            4'hC: begin D <= R[dst5][7:0];  cpu_state <= CPU_FETCH; end
                            4'hD: begin D <= R[dst5][15:8]; cpu_state <= CPU_FETCH; end
                            4'hE: begin R[dst5][7:0]  <= D; cpu_state <= CPU_FETCH; end
                            4'hF: begin R[dst5][15:8] <= D; cpu_state <= CPU_FETCH; end
                        endcase
                    end
                end

                // --------------------------------------------------------
                // ALU (0x7) — 16 sub-ops
                // --------------------------------------------------------
                else if (fam == FAM_ALU) begin
                    ext_active <= 1'b0;
                    dst_reg <= dst5;
                    src_reg <= src5;
                    alu_a   <= R[dst5];
                    alu_b   <= R[src5];
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
                    memalu_sub <= nib;
                    io_out_active <= 1'b0;
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
                            4'h9: begin // STXI — M(R(X)) ← D, R(X) ← R(X)+1
                                effective_addr <= R[xsel];
                                mem_data <= {56'd0, D};
                                bus_size <= BUS_BYTE;
                                R[xsel] <= R[xsel] + 64'd1;
                                cpu_state <= CPU_MEM_WRITE;
                            end
                            4'hB: begin // STXD.D — M(R(X)) ← D, R(X) ← R(X)-1
                                effective_addr <= R[xsel];
                                mem_data <= {56'd0, D};
                                bus_size <= BUS_BYTE;
                                R[xsel] <= R[xsel] - 64'd1;
                                cpu_state <= CPU_MEM_WRITE;
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

                // --------------------------------------------------------
                // IO (0x9)
                // --------------------------------------------------------
                else if (fam == FAM_IO) begin
                    ext_active <= 1'b0;
                    if (nib >= 4'd1 && nib <= 4'd7) begin
                        io_port <= nib[2:0]; io_is_inp <= 1'b0;
                        effective_addr <= R[xsel]; bus_size <= BUS_BYTE;
                        cpu_state <= CPU_MEMALU_RD;
                        memalu_sub <= 4'hF;
                        io_out_active <= 1'b1;
                    end else if (nib >= 4'd9) begin
                        io_port <= nib[2:0]; io_is_inp <= 1'b1;
                        effective_addr <= {MP64_MMIO_HI, 20'd0, nib[2:0], 9'd0};
                        bus_size <= BUS_BYTE; cpu_state <= CPU_MEM_READ;
                        bus_port_io <= 1'b1;  // sideband for SoC remap
                        dst_reg <= 5'd0; mem_sub <= 4'hF;
                    end else
                        cpu_state <= CPU_FETCH;
                end

                // --------------------------------------------------------
                // SEP (0xA)
                // --------------------------------------------------------
                else if (fam == FAM_SEP) begin
                    ext_active <= 1'b0;
                    psel <= nib5; fetch_pc <= R[nib5];
                    cpu_state <= CPU_FETCH;
                end

                // --------------------------------------------------------
                // SEX (0xB)
                // --------------------------------------------------------
                else if (fam == FAM_SEX) begin
                    ext_active <= 1'b0;
                    xsel <= nib5; cpu_state <= CPU_FETCH;
                end

                // --------------------------------------------------------
                // MULDIV (0xC) — hardware multiply/divide
                // --------------------------------------------------------
                else if (fam == FAM_MULDIV) begin
                    ext_active <= 1'b0;
                    dst_reg <= dst5;
                    src_reg <= src5;
                    div_done_r <= 1'b0;
                    case (nib)
                        4'h0: begin // SMUL low
                            mul_op_a <= R[dst5];
                            mul_op_b <= R[src5];
                            mul_is_signed_r <= 1'b1;
                            mul_start_r <= 1'b1;
                            cpu_state <= CPU_MULDIV;
                        end
                        4'h1: begin // SMUL high
                            mul_op_a <= R[dst5];
                            mul_op_b <= R[src5];
                            mul_is_signed_r <= 1'b1;
                            mul_start_r <= 1'b1;
                            mem_sub <= 4'h1; cpu_state <= CPU_MULDIV;
                        end
                        4'h2: begin // UMUL low
                            mul_op_a <= R[dst5];
                            mul_op_b <= R[src5];
                            mul_is_signed_r <= 1'b0;
                            mul_start_r <= 1'b1;
                            cpu_state <= CPU_MULDIV;
                        end
                        4'h3: begin // UMUL high
                            mul_op_a <= R[dst5];
                            mul_op_b <= R[src5];
                            mul_is_signed_r <= 1'b0;
                            mul_start_r <= 1'b1;
                            mem_sub <= 4'h1; cpu_state <= CPU_MULDIV;
                        end
                        4'h4: begin // DIV (signed)
                            if (R[src5] == 64'd0) begin
                                ivec_id <= IRQX_ILLEGAL_OP; cpu_state <= CPU_IRQ;
                            end else begin
                                div_signed_op <= 1'b1;
                                div_dividend  <= R[dst5][63]
                                    ? (~R[dst5] + 64'd1) : R[dst5];
                                div_divisor   <= R[src5][63]
                                    ? (~R[src5] + 64'd1) : R[src5];
                                div_quotient  <= 64'd0;
                                div_remainder <= 64'd0;
                                div_cycle     <= 7'd0;
                                div_active    <= 1'b1;
                                div_op_nib    <= 4'h4;
                                cpu_state     <= CPU_MULDIV;
                            end
                        end
                        4'h5: begin // UDIV
                            if (R[src5] == 64'd0) begin
                                ivec_id <= IRQX_ILLEGAL_OP; cpu_state <= CPU_IRQ;
                            end else begin
                                div_signed_op <= 1'b0;
                                div_dividend  <= R[dst5];
                                div_divisor   <= R[src5];
                                div_quotient  <= 64'd0;
                                div_remainder <= 64'd0;
                                div_cycle     <= 7'd0;
                                div_active    <= 1'b1;
                                div_op_nib    <= 4'h5;
                                cpu_state     <= CPU_MULDIV;
                            end
                        end
                        4'h6: begin // MOD (signed)
                            if (R[src5] == 64'd0) begin
                                ivec_id <= IRQX_ILLEGAL_OP; cpu_state <= CPU_IRQ;
                            end else begin
                                div_signed_op <= 1'b1;
                                div_dividend  <= R[dst5][63]
                                    ? (~R[dst5] + 64'd1) : R[dst5];
                                div_divisor   <= R[src5][63]
                                    ? (~R[src5] + 64'd1) : R[src5];
                                div_quotient  <= 64'd0;
                                div_remainder <= 64'd0;
                                div_cycle     <= 7'd0;
                                div_active    <= 1'b1;
                                div_op_nib    <= 4'h6;
                                cpu_state     <= CPU_MULDIV;
                            end
                        end
                        4'h7: begin // UMOD
                            if (R[src5] == 64'd0) begin
                                ivec_id <= IRQX_ILLEGAL_OP; cpu_state <= CPU_IRQ;
                            end else begin
                                div_signed_op <= 1'b0;
                                div_dividend  <= R[dst5];
                                div_divisor   <= R[src5];
                                div_quotient  <= 64'd0;
                                div_remainder <= 64'd0;
                                div_cycle     <= 7'd0;
                                div_active    <= 1'b1;
                                div_op_nib    <= 4'h7;
                                cpu_state     <= CPU_MULDIV;
                            end
                        end
                        // ------------------------------------------------
                        // Bitfield ALU — sub-ops 0x8–0xF (single-cycle)
                        // ------------------------------------------------
                        4'h8: begin // POPCNT — D ← popcount(Rs)
                            bf_op <= BF_POPCNT; bf_a <= R[dst5]; bf_b <= R[src5]; bf_imm <= 6'd0;
                            bf_active <= 1'b1; dst_reg <= dst5; cpu_state <= CPU_EXECUTE;
                        end
                        4'h9: begin // CLZ — D ← clz(Rs)
                            bf_op <= BF_CLZ; bf_a <= R[dst5]; bf_b <= R[src5]; bf_imm <= 6'd0;
                            bf_active <= 1'b1; dst_reg <= dst5; cpu_state <= CPU_EXECUTE;
                        end
                        4'hA: begin // CTZ — D ← ctz(Rs)
                            bf_op <= BF_CTZ; bf_a <= R[dst5]; bf_b <= R[src5]; bf_imm <= 6'd0;
                            bf_active <= 1'b1; dst_reg <= dst5; cpu_state <= CPU_EXECUTE;
                        end
                        4'hB: begin // BITREV — D ← bitrev(Rs)
                            bf_op <= BF_BITREV; bf_a <= R[dst5]; bf_b <= R[src5]; bf_imm <= 6'd0;
                            bf_active <= 1'b1; dst_reg <= dst5; cpu_state <= CPU_EXECUTE;
                        end
                        4'hC: begin // BEXT — D ← pext(Rs, Rd)
                            bf_op <= BF_BEXT; bf_a <= R[dst5]; bf_b <= R[src5]; bf_imm <= 6'd0;
                            bf_active <= 1'b1; dst_reg <= dst5; cpu_state <= CPU_EXECUTE;
                        end
                        4'hD: begin // BDEP — D ← pdep(Rd, Rs)
                            bf_op <= BF_BDEP; bf_a <= R[dst5]; bf_b <= R[src5]; bf_imm <= 6'd0;
                            bf_active <= 1'b1; dst_reg <= dst5; cpu_state <= CPU_EXECUTE;
                        end
                        4'hE: begin // RORI — D ← rotr(Rd, imm6)
                            bf_op <= BF_RORI; bf_a <= R[dst5]; bf_b <= 64'd0; bf_imm <= ibuf[2][5:0];
                            bf_active <= 1'b1; dst_reg <= dst5; cpu_state <= CPU_EXECUTE;
                        end
                        4'hF: begin // BSWAP — D ← bswap(Rs)
                            bf_op <= BF_BSWAP; bf_a <= R[dst5]; bf_b <= R[src5]; bf_imm <= 6'd0;
                            bf_active <= 1'b1; dst_reg <= dst5; cpu_state <= CPU_EXECUTE;
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
                            CSR_PSEL:     psel     <= R[nib[2:0]][4:0];
                            CSR_XSEL:     xsel     <= R[nib[2:0]][4:0];
                            CSR_SPSEL:    spsel    <= R[nib[2:0]][4:0];
                            CSR_IVTBASE:  ivt_base <= R[nib[2:0]];
                            CSR_D:        D        <= R[nib[2:0]][7:0];
                            CSR_DF:       flags[1] <= R[nib[2:0]][0];
                            CSR_QREG:     Q        <= R[nib[2:0]][0];
                            CSR_TREG:     T        <= R[nib[2:0]][15:0];
                            CSR_IE:       flags[6] <= R[nib[2:0]][0];
                            CSR_PRIV:     priv_level <= R[nib[2:0]][0];  // inert — no enforcement
                            CSR_MPU_BASE: mpu_base   <= R[nib[2:0]];
                            CSR_MPU_LIMIT:mpu_limit  <= R[nib[2:0]];
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
                            CSR_CRC_ACC:  crc_acc  <= R[nib[2:0]];
                            CSR_CRC_MODE: crc_mode <= R[nib[2:0]][1:0];
                            CSR_ACC0:     acc_reg[0] <= R[nib[2:0]];
                            CSR_ACC1:     acc_reg[1] <= R[nib[2:0]];
                            CSR_ACC2:     acc_reg[2] <= R[nib[2:0]];
                            CSR_ACC3:     acc_reg[3] <= R[nib[2:0]];
                            CSR_TSRC0:    sha_tsrc0  <= R[nib[2:0]];
                            CSR_SHA_MODE: sha_mode   <= R[nib[2:0]][1:0];
                            CSR_SHA_MSGLEN:    sha_msglen_lo <= R[nib[2:0]];
                            CSR_SHA_MSGLEN_HI: sha_msglen_hi <= R[nib[2:0]];
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
                            CSR_PSEL:        R[nib[2:0]] <= {59'd0, psel};
                            CSR_XSEL:        R[nib[2:0]] <= {59'd0, xsel};
                            CSR_SPSEL:       R[nib[2:0]] <= {59'd0, spsel};
                            CSR_IVTBASE:     R[nib[2:0]] <= ivt_base;
                            CSR_D:           R[nib[2:0]] <= {56'd0, D};
                            CSR_DF:          R[nib[2:0]] <= {63'd0, flags[1]};
                            CSR_QREG:        R[nib[2:0]] <= {63'd0, Q};
                            CSR_TREG:        R[nib[2:0]] <= {48'd0, T};
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
                            CSR_CRC_ACC:     R[nib[2:0]] <= crc_acc;
                            CSR_CRC_MODE:    R[nib[2:0]] <= {62'd0, crc_mode};
                            CSR_ACC0:        R[nib[2:0]] <= acc_reg[0];
                            CSR_ACC1:        R[nib[2:0]] <= acc_reg[1];
                            CSR_ACC2:        R[nib[2:0]] <= acc_reg[2];
                            CSR_ACC3:        R[nib[2:0]] <= acc_reg[3];
                            CSR_SHA_MODE:    R[nib[2:0]] <= {62'd0, sha_mode};
                            CSR_SHA_MSGLEN:  R[nib[2:0]] <= sha_msglen_lo;
                            CSR_SHA_MSGLEN_HI: R[nib[2:0]] <= sha_msglen_hi;
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
            // EXECUTE: ALU / Bitfield writeback
            // ============================================================
            CPU_EXECUTE: begin
                if (crypto_active) begin
                    // CRC / crypto writeback (unit 0)
                    if (crypto_unit_r == 4'd0) begin
                        if (crc_acc_we)  crc_acc  <= crc_acc_out;
                        if (crc_mode_we) crc_mode <= crc_mode_out;
                        if (crc_rd_we)   R[crypto_rd_r] <= crc_result;
                        crypto_active <= 1'b0;
                        cpu_state <= CPU_FETCH;
                    end
                    // SHA-2 (unit 1)
                    else if (crypto_unit_r == 4'd1) begin
                        case (crypto_op_r)
                            ISA_SHA_INIT: begin
                                // Load SHA-256 IV into ACC, set mode
                                sha_mode <= crypto_imm_r[1:0];
                                // SHA-256 IV (only mode supported in RTL)
                                acc_reg[0] <= 64'h6a09e667_bb67ae85;
                                acc_reg[1] <= 64'h3c6ef372_a54ff53a;
                                acc_reg[2] <= 64'h510e527f_9b05688c;
                                acc_reg[3] <= 64'h1f83d9ab_5be0cd19;
                                sha_msglen_lo <= 64'd0;
                                sha_msglen_hi <= 64'd0;
                                crypto_active <= 1'b0;
                                cpu_state <= CPU_FETCH;
                            end
                            ISA_SHA_ROUND: begin
                                // Begin W loading from tile memory at TSRC0
                                sha_load_cnt <= 4'd0;
                                cpu_state <= CPU_SHA_LOAD;
                            end
                            ISA_SHA_DIN: begin
                                // ACC[Rd[1:0]] ← Rs
                                acc_reg[crypto_rd_r[1:0]] <= R[crypto_rs_r];
                                crypto_active <= 1'b0;
                                cpu_state <= CPU_FETCH;
                            end
                            ISA_SHA_DOUT: begin
                                // Rd ← ACC[Rs[1:0]]
                                R[crypto_rd_r] <= acc_reg[crypto_rs_r[1:0]];
                                crypto_active <= 1'b0;
                                cpu_state <= CPU_FETCH;
                            end
                            ISA_SHA_PAD, ISA_SHA_FINAL: begin
                                // PAD and FINAL: handled by BIOS in software
                                // (write padding manually, then SHA.ROUND)
                                // RTL treats as NOP
                                crypto_active <= 1'b0;
                                cpu_state <= CPU_FETCH;
                            end
                            default: begin
                                crypto_active <= 1'b0;
                                cpu_state <= CPU_FETCH;
                            end
                        endcase
                    end
                    // Other crypto units — NOP
                    else begin
                        crypto_active <= 1'b0;
                        cpu_state <= CPU_FETCH;
                    end
                end else if (bf_active) begin
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
                                T <= bus_rdata[15:0];
                                xsel <= bus_rdata[12:8]; psel <= bus_rdata[4:0];
                                flags[6] <= 1'b1;
                                fetch_pc <= R[bus_rdata[4:0]];
                                cpu_state <= CPU_FETCH;
                            end
                            4'hB: begin // DIS
                                T <= bus_rdata[15:0];
                                xsel <= bus_rdata[12:8]; psel <= bus_rdata[4:0];
                                flags[6] <= 1'b0;
                                fetch_pc <= R[bus_rdata[4:0]];
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
                    if (io_out_active) begin
                        // IO OUT — write byte to MMIO port address
                        io_out_active <= 1'b0;
                        D <= bus_rdata[7:0];
                        effective_addr <= {MP64_MMIO_HI, 20'd0, io_port, 9'd0};
                        mem_data <= {56'd0, bus_rdata[7:0]};
                        bus_size <= BUS_BYTE;
                        bus_port_io <= 1'b1;  // sideband for SoC remap
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

            // ============================================================
            // STRING: EXT.STRING engine stall — forward bus, await done
            // ============================================================
            CPU_STRING: begin
                string_start_r <= 1'b0;
                // Forward string engine bus to CPU bus outputs
                bus_valid <= str_bus_req;
                bus_addr  <= str_bus_addr;
                bus_wdata <= str_bus_wdata;
                bus_wen   <= str_bus_wr;
                bus_size  <= BUS_BYTE;

                if (str_done) begin
                    // Writeback: Rd
                    R[string_dst_reg] <= str_out_dst;
                    // Writeback: Rs (depends on sub-op)
                    if (string_op_r == 4'h02)        // BFILL: Rs was count
                        R[string_src_reg] <= str_out_len;
                    else if (string_op_r == 4'h04)   // BSRCH: Rs ← offset
                        R[string_src_reg] <= str_out_result;
                    else                              // CMOVE/CMOVE>/BCOMP
                        R[string_src_reg] <= str_out_src;
                    // Writeback: R0 (length) for non-BFILL ops
                    if (string_op_r != 4'h02)
                        R[0] <= str_out_len;
                    // Flags: update Z/G for BCOMP and BSRCH
                    if (string_op_r == 4'h03 || string_op_r == 4'h04) begin
                        flags[0] <= str_flag_z;  // Z
                        flags[5] <= str_flag_g;  // G
                    end
                    cpu_state <= CPU_FETCH;
                end
            end

            // ============================================================
            // SHA_LOAD: Read 8 × 64-bit words from tile mem into sha_w_buf
            // ============================================================
            CPU_SHA_LOAD: begin
                bus_valid <= 1'b1;
                bus_addr  <= sha_tsrc0 + {sha_load_cnt[2:0], 3'b000};
                bus_wen   <= 1'b0;
                bus_size  <= BUS_DWORD;
                if (bus_ready) begin
                    bus_valid <= 1'b0;
                    // Big-endian: high 32 bits = W[2i], low 32 bits = W[2i+1]
                    sha_w_buf[{sha_load_cnt[2:0], 1'b0}]      <= bus_rdata[63:32];
                    sha_w_buf[{sha_load_cnt[2:0], 1'b0} | 4'd1] <= bus_rdata[31:0];
                    if (sha_load_cnt[2:0] == 3'd7) begin
                        // All 16 W words loaded — start compression
                        sha_start_r <= 1'b1;
                        cpu_state   <= CPU_SHA_WAIT;
                    end else begin
                        sha_load_cnt <= sha_load_cnt + 4'd1;
                    end
                end
            end

            // ============================================================
            // SHA_WAIT: Wait for 64-round compression engine to finish
            // ============================================================
            CPU_SHA_WAIT: begin
                if (sha_done) begin
                    // Write back H' to ACC registers
                    acc_reg[0] <= {sha_h_out[0], sha_h_out[1]};
                    acc_reg[1] <= {sha_h_out[2], sha_h_out[3]};
                    acc_reg[2] <= {sha_h_out[4], sha_h_out[5]};
                    acc_reg[3] <= {sha_h_out[6], sha_h_out[7]};
                    // Update message length: +512 bits per block
                    sha_msglen_lo <= sha_msglen_lo + 64'd512;
                    if (sha_msglen_lo > (64'hFFFF_FFFF_FFFF_FFFF - 64'd512))
                        sha_msglen_hi <= sha_msglen_hi + 64'd1;
                    flags[0] <= 1'b0;  // Z=0 (success)
                    crypto_active <= 1'b0;
                    cpu_state <= CPU_FETCH;
                end
            end

            // ============================================================
            // DICT: EXT.DICT engine stall — forward bus, await done
            // ============================================================
            CPU_DICT: begin
                dict_start_r <= 1'b0;
                // Forward dict engine bus to CPU bus outputs
                bus_valid <= dict_bus_req;
                bus_addr  <= dict_bus_addr;
                bus_wdata <= dict_bus_wdata;
                bus_wen   <= dict_bus_wr;
                bus_size  <= BUS_BYTE;

                if (dict_done) begin
                    // Writeback: Rd ← XT result for DFIND
                    if (dict_op_r == 4'h00)
                        R[dict_dst_reg] <= dict_xt_out;
                    // Flags
                    flags[0] <= dict_flag_z;  // Z = found / success
                    flags[3] <= dict_flag_v;  // V = overflow (DINS)
                    cpu_state <= CPU_FETCH;
                end
            end

            endcase
        end
    end

endmodule
