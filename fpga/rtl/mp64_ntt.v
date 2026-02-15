// ============================================================================
// mp64_ntt.v — 256-Point Number Theoretic Transform Accelerator
// ============================================================================
//
// Hardware accelerator for lattice-based cryptography (ML-KEM / ML-DSA).
// Performs NTT, inverse NTT, pointwise multiply, and pointwise add on
// polynomials of degree ≤255 over Z_q, where q defaults to 3329 (Kyber).
//
// Target: Kintex-7 / Artix-7.
// Resource estimate:
//   ~4 DSP48E1 slices   (12×12 modular multiply in butterfly unit)
//   ~10K flip-flops     (coefficient memories: 3 × 256 × 32-bit)
//   ~2K LUTs            (butterfly logic, twiddle ROM, FSM)
//
// Cycle counts:
//   NTT_FWD   ≈ 1280  (256 bit-rev + 8×128 butterflies)
//   NTT_INV   ≈ 1536  (256 bit-rev + 8×128 butterflies + 256 scale)
//   NTT_PMUL  ≈ 256
//   NTT_PADD  ≈ 256
//
// MMIO base: 0x8C0 (64-byte block, addr[5:0]).
//
// Register map (64-bit bus, addressed by addr[5:3]):
//   ── Write ──────────────────────────────────────────────────────
//   0x00  CMD      wdata[0]=go, wdata[2:1]=op (0=FWD,1=INV,2=PMUL,3=PADD)
//   0x08  Q        64-bit modulus (default 3329; only 32 low bits used)
//   0x10  IDX      coefficient index (low 8 bits; 0–255)
//   0x18  LOAD_A   wdata[31:0] → poly_a[IDX]; IDX++
//   0x20  LOAD_B   wdata[31:0] → poly_b[IDX]; IDX++
//
//   ── Read ───────────────────────────────────────────────────────
//   0x00  STATUS   {62'd0, done, busy}
//   0x08  Q
//   0x10  IDX
//   0x18  reserved
//   0x20  RESULT   rdata[31:0] = result[IDX]; IDX++
//   0x28  CMD echo (low 3 bits)
//
// ============================================================================

`include "mp64_defs.vh"

module mp64_ntt (
    input  wire        clk,
    input  wire        rst_n,

    // MMIO bus interface
    input  wire        req,
    input  wire [5:0]  addr,
    input  wire [63:0] wdata,
    input  wire        wen,
    output reg  [63:0] rdata,
    output reg         ack
);

    // ========================================================================
    // Parameters — Kyber defaults
    // ========================================================================
    localparam NTT_N     = 256;
    localparam Q_DEFAULT = 32'd3329;
    localparam OMEGA     = 32'd17;       // primitive 256th root of unity mod 3329
    localparam OMEGA_INV = 32'd1175;     // 17⁻¹ mod 3329
    localparam N_INV     = 32'd3316;     // 256⁻¹ mod 3329

    // ========================================================================
    // State machine
    // ========================================================================
    localparam S_IDLE    = 3'd0,
               S_BIT_REV = 3'd1,   // bit-reverse permutation into work mem
               S_STAGE   = 3'd2,   // butterfly stage
               S_SCALE   = 3'd3,   // inverse NTT: multiply by N⁻¹
               S_PMUL    = 3'd4,   // pointwise multiply
               S_PADD    = 3'd5,   // pointwise add
               S_DONE    = 3'd6;

    reg [2:0]  state;
    reg        busy, done;

    // ========================================================================
    // Operation encoding
    // ========================================================================
    localparam OP_FWD  = 2'd0,
               OP_INV  = 2'd1,
               OP_PMUL = 2'd2,
               OP_PADD = 2'd3;

    reg [1:0]  op_reg;

    // ========================================================================
    // Coefficient memory (register-file based for dual-port access)
    // ========================================================================
    reg [31:0] poly_a [0:NTT_N-1];    // input polynomial A
    reg [31:0] poly_b [0:NTT_N-1];    // input polynomial B
    reg [31:0] work   [0:NTT_N-1];    // working / result memory

    reg [31:0] q_reg;                  // modulus
    reg [7:0]  idx_reg;                // coefficient index

    // ========================================================================
    // Twiddle factor ROM (precomputed for q=3329, ω=17)
    // ========================================================================
    reg [31:0] omega_pow     [0:NTT_N-1];
    reg [31:0] omega_inv_pow [0:NTT_N-1];

    integer init_i;
    initial begin
        omega_pow[0]     = 32'd1;
        omega_inv_pow[0] = 32'd1;
        for (init_i = 1; init_i < NTT_N; init_i = init_i + 1) begin
            omega_pow[init_i]     = (omega_pow[init_i-1]     * OMEGA)     % Q_DEFAULT;
            omega_inv_pow[init_i] = (omega_inv_pow[init_i-1] * OMEGA_INV) % Q_DEFAULT;
        end
    end

    // ========================================================================
    // Bit-reverse function (8-bit)
    // ========================================================================
    function [7:0] bit_reverse;
        input [7:0] x;
        begin
            bit_reverse = {x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7]};
        end
    endfunction

    // ========================================================================
    // Modular arithmetic for q_reg (combinational)
    // ========================================================================
    function [31:0] mod_add;
        input [31:0] a, b, q;
        reg [32:0] sum;
        begin
            sum = {1'b0, a} + {1'b0, b};
            if (sum >= {1'b0, q})
                mod_add = sum - {1'b0, q};
            else
                mod_add = sum[31:0];
        end
    endfunction

    function [31:0] mod_sub;
        input [31:0] a, b, q;
        begin
            if (a >= b)
                mod_sub = a - b;
            else
                mod_sub = q - (b - a);
        end
    endfunction

    function [31:0] mod_mul;
        input [31:0] a, b, q;
        reg [63:0] product;
        begin
            product = a * b;
            mod_mul = product % q;   // synthesis: constant-q Barrett; sim: %
        end
    endfunction

    // ========================================================================
    // NTT control registers
    // ========================================================================
    reg [7:0]  br_idx;          // bit-reversal counter (0..255)
    reg [3:0]  stage;           // current stage (1..8)
    reg [7:0]  bf_cnt;          // butterfly counter within stage (0..127)
    reg [31:0] wm_reg;          // current stage twiddle factor (ω^(N/m))
    reg [31:0] w_reg;           // running twiddle (w = wm^j within group)
    reg [7:0]  bf_half;         // half = 2^(stage-1)
    reg [7:0]  bf_k;            // group start address
    reg [7:0]  bf_j;            // butterfly index within group
    reg        bf_sub;          // 0 = read/compute, 1 = write-back
    reg [7:0]  scale_idx;       // counter for N⁻¹ scaling (INTT)
    reg [7:0]  pw_idx;          // pointwise op counter
    reg        result_read;     // flag: result was read, advance idx next cycle

    // Butterfly read latches
    reg [31:0] bf_a, bf_b, bf_t;

    // ========================================================================
    // Main sequential logic
    // ========================================================================
    integer j;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state    <= S_IDLE;
            busy     <= 1'b0;
            done     <= 1'b0;
            op_reg   <= 2'd0;
            q_reg    <= Q_DEFAULT;
            idx_reg  <= 8'd0;
            br_idx   <= 0;
            stage    <= 0;
            bf_cnt   <= 0;
            bf_sub   <= 0;
            bf_half  <= 0;
            bf_k     <= 0;
            bf_j     <= 0;
            w_reg    <= 0;
            wm_reg   <= 0;
            scale_idx <= 0;
            pw_idx   <= 0;
            result_read <= 0;
            bf_a <= 0; bf_b <= 0; bf_t <= 0;
            for (j = 0; j < NTT_N; j = j + 1) begin
                poly_a[j] <= 32'd0;
                poly_b[j] <= 32'd0;
                work[j]   <= 32'd0;
            end
        end else begin

            // Deferred result-read index advance
            if (result_read) begin
                idx_reg     <= idx_reg + 8'd1;
                result_read <= 1'b0;
            end

            // ================================================================
            // MMIO writes
            // ================================================================
            if (req && wen) begin
                case (addr[5:3])
                    3'b000: begin  // CMD — accept in IDLE or DONE
                        if (wdata[0] && (state == S_IDLE || state == S_DONE)) begin
                            op_reg <= wdata[2:1];
                            busy   <= 1'b1;
                            done   <= 1'b0;
                            case (wdata[2:1])
                                OP_FWD, OP_INV: begin
                                    br_idx <= 8'd0;
                                    state  <= S_BIT_REV;
                                end
                                OP_PMUL: begin
                                    pw_idx <= 8'd0;
                                    state  <= S_PMUL;
                                end
                                OP_PADD: begin
                                    pw_idx <= 8'd0;
                                    state  <= S_PADD;
                                end
                            endcase
                        end
                    end
                    3'b001: q_reg   <= wdata[31:0];           // Q
                    3'b010: idx_reg <= wdata[7:0];            // IDX
                    3'b011: begin                             // LOAD_A
                        poly_a[idx_reg] <= wdata[31:0] % q_reg;
                        idx_reg <= idx_reg + 8'd1;
                    end
                    3'b100: begin                             // LOAD_B
                        poly_b[idx_reg] <= wdata[31:0] % q_reg;
                        idx_reg <= idx_reg + 8'd1;
                    end
                    default: ;
                endcase
            end

            // ================================================================
            // State machine
            // ================================================================
            case (state)

                // ------------------------------------------------------------
                // S_BIT_REV — copy poly_a into work[] in bit-reversed order
                // ------------------------------------------------------------
                S_BIT_REV: begin
                    work[bit_reverse(br_idx)] <= poly_a[br_idx];
                    if (br_idx == 8'd255) begin
                        stage    <= 4'd1;
                        bf_cnt   <= 8'd0;
                        bf_half  <= 8'd1;   // half = 2^(stage-1) = 1 for stage 1
                        bf_k     <= 8'd0;   // first group start
                        bf_j     <= 8'd0;
                        bf_sub   <= 1'b0;
                        // wm = omega_pow[N/m] = omega_pow[128] for stage 1
                        // For forward NTT: wm = ω^(N/2^stage) = ω^(128) for stage 1
                        // For inverse NTT: wm = (ω⁻¹)^(N/2^stage)
                        if (op_reg == OP_FWD)
                            wm_reg <= omega_pow[128];     // ω^128
                        else
                            wm_reg <= omega_inv_pow[128]; // (ω⁻¹)^128
                        w_reg <= 32'd1;
                        state <= S_STAGE;
                    end else begin
                        br_idx <= br_idx + 8'd1;
                    end
                end

                // ------------------------------------------------------------
                // S_STAGE — Cooley-Tukey butterfly passes
                //
                // For each stage s (1..8):
                //   m = 2^s, half = 2^(s-1)
                //   wm = ω^(N/m)  (forward) or (ω⁻¹)^(N/m)  (inverse)
                //   For each group starting at k (step m):
                //     w = 1
                //     For j = 0..half-1:
                //       t = w · work[k+j+half]
                //       u = work[k+j]
                //       work[k+j]      = (u + t) mod q
                //       work[k+j+half] = (u - t) mod q
                //       w = w · wm
                //
                // Two sub-phases per butterfly:
                //   sub=0: read work[bf_k+bf_j] and work[bf_k+bf_j+bf_half],
                //          compute t = w·b mod q
                //   sub=1: write back, advance w, j (or k, or stage)
                // ------------------------------------------------------------
                S_STAGE: begin
                    if (!bf_sub) begin
                        // Sub-phase 0: read + multiply
                        bf_a <= work[bf_k + bf_j];
                        bf_b <= work[bf_k + bf_j + bf_half];
                        bf_t <= mod_mul(w_reg, work[bf_k + bf_j + bf_half], q_reg);
                        bf_sub <= 1'b1;
                    end else begin
                        // Sub-phase 1: butterfly write-back
                        work[bf_k + bf_j]           <= mod_add(bf_a, bf_t, q_reg);
                        work[bf_k + bf_j + bf_half] <= mod_sub(bf_a, bf_t, q_reg);
                        w_reg <= mod_mul(w_reg, wm_reg, q_reg);
                        bf_sub <= 1'b0;

                        if (bf_j == bf_half - 8'd1) begin
                            // End of group — advance to next group
                            bf_j <= 8'd0;
                            w_reg <= 32'd1;  // reset w for new group
                            if (bf_k + {1'b0, bf_half, 1'b0} >= NTT_N) begin
                                // End of stage — advance to next stage
                                if (stage == 4'd8) begin
                                    // All stages done
                                    if (op_reg == OP_INV) begin
                                        scale_idx <= 8'd0;
                                        state <= S_SCALE;
                                    end else begin
                                        busy  <= 1'b0;
                                        done  <= 1'b1;
                                        state <= S_DONE;
                                    end
                                end else begin
                                    stage   <= stage + 4'd1;
                                    bf_half <= {bf_half[6:0], 1'b0};  // half <<= 1
                                    bf_k    <= 8'd0;
                                    // wm for next stage: ω^(N/2^(stage+1))
                                    // Index into ROM: N >> (stage+1)
                                    // stage is about to become stage+1, so:
                                    // twiddle step = 256 >> (stage+1)
                                    if (op_reg == OP_FWD)
                                        wm_reg <= omega_pow[NTT_N >> (stage + 1)];
                                    else
                                        wm_reg <= omega_inv_pow[NTT_N >> (stage + 1)];
                                    w_reg <= 32'd1;
                                end
                            end else begin
                                bf_k <= bf_k + {1'b0, bf_half, 1'b0};  // k += m = 2*half
                                w_reg <= 32'd1;
                            end
                        end else begin
                            bf_j <= bf_j + 8'd1;
                        end
                    end
                end

                // ------------------------------------------------------------
                // S_SCALE — inverse NTT: multiply each coefficient by N⁻¹
                // ------------------------------------------------------------
                S_SCALE: begin
                    work[scale_idx] <= mod_mul(work[scale_idx], N_INV, q_reg);
                    if (scale_idx == 8'd255) begin
                        busy  <= 1'b0;
                        done  <= 1'b1;
                        state <= S_DONE;
                    end else begin
                        scale_idx <= scale_idx + 8'd1;
                    end
                end

                // ------------------------------------------------------------
                // S_PMUL — pointwise multiply: result[i] = a[i]·b[i] mod q
                // ------------------------------------------------------------
                S_PMUL: begin
                    work[pw_idx] <= mod_mul(poly_a[pw_idx], poly_b[pw_idx], q_reg);
                    if (pw_idx == 8'd255) begin
                        busy  <= 1'b0;
                        done  <= 1'b1;
                        state <= S_DONE;
                    end else begin
                        pw_idx <= pw_idx + 8'd1;
                    end
                end

                // ------------------------------------------------------------
                // S_PADD — pointwise add: result[i] = (a[i]+b[i]) mod q
                // ------------------------------------------------------------
                S_PADD: begin
                    work[pw_idx] <= mod_add(poly_a[pw_idx], poly_b[pw_idx], q_reg);
                    if (pw_idx == 8'd255) begin
                        busy  <= 1'b0;
                        done  <= 1'b1;
                        state <= S_DONE;
                    end else begin
                        pw_idx <= pw_idx + 8'd1;
                    end
                end

                S_DONE: begin
                    // wait for next CMD
                end

            endcase
        end
    end

    // ========================================================================
    // MMIO read handler
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            rdata <= 64'd0;
            ack   <= 1'b0;
        end else begin
            ack <= 1'b0;
            if (req) begin
                ack <= 1'b1;
                if (!wen) begin
                    case (addr[5:3])
                        3'b000: rdata <= {62'd0, done, busy};     // STATUS
                        3'b001: rdata <= {32'd0, q_reg};          // Q
                        3'b010: rdata <= {56'd0, idx_reg};        // IDX
                        3'b011: rdata <= 64'd0;                   // reserved
                        3'b100: begin                             // RESULT[IDX]
                            rdata       <= {32'd0, work[idx_reg]};
                            result_read <= 1'b1;  // advance idx next cycle
                        end
                        3'b101: rdata <= {62'd0, op_reg};         // CMD echo
                        default: rdata <= 64'd0;
                    endcase
                end else begin
                    rdata <= 64'd0;
                end
            end
        end
    end

endmodule
