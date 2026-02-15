// ============================================================================
// mp64_x25519.v — X25519 Elliptic Curve Diffie-Hellman Accelerator
// ============================================================================
//
// Hardware accelerator for X25519 scalar multiplication (RFC 7748).
// Computes result = clamp(scalar) * point on Curve25519.
//
// Target: Kintex-7 (Genesys2 board).
// Resource estimate:
//   ~165 DSP48E1 slices  (one shared 256×256 multiplier, operand-muxed)
//   ~4200 flip-flops     (state registers + 10 named 256-bit temporaries)
//   ~3K LUTs             (field_add/sub combinational logic)
//
// Timing: 255 ladder steps × 14 phases + 255 inversion steps × 3 phases
//       ≈ 4335 clock cycles per scalar multiply (43 µs at 100 MHz).
//
// MMIO base: 0x840 (64-byte block, addr[5:0]).
//
// Architecture:
//   - Montgomery ladder over GF(2^255 − 19), constant-time
//   - Exactly one field multiplication per clock cycle (single multiplier)
//   - XOR-accumulated conditional swap per RFC 7748 §5 (no swap-back)
//   - Field add/sub: combinational (256-bit adder + compare-subtract)
//   - Field mul: 256×256 → 512-bit schoolbook, Barrett-style reduction
//     via the identity  x mod p ≡ x_lo + 19·x_hi  (applied twice)
//   - Inversion: Fermat's little theorem, z^(p−2) binary method (255 steps)
//   - Scalar clamping per RFC 7748 §5
//
// Register map (byte offsets from X25519_BASE = 0x840):
//   Write:
//     0x00  SCALAR[0]  — scalar bytes  0..7  (little-endian)
//     0x08  SCALAR[1]  — scalar bytes  8..15
//     0x10  SCALAR[2]  — scalar bytes 16..23
//     0x18  SCALAR[3]  — scalar bytes 24..31
//     0x20  POINT[0]   — u-coordinate bytes  0..7
//     0x28  POINT[1]   — u-coordinate bytes  8..15
//     0x30  POINT[2]   — u-coordinate bytes 16..23
//     0x38  POINT[3]   — u-coordinate bytes 24..31 (bit 255 masked)
//     0x3F  CMD        — write 1 to start computation
//   Read:
//     0x00  STATUS     — bit[0]=busy, bit[1]=done
//     0x08  RESULT[0]  — result bytes  0..7
//     0x10  RESULT[1]  — result bytes  8..15
//     0x18  RESULT[2]  — result bytes 16..23
//     0x20  RESULT[3]  — result bytes 24..31
//
// ============================================================================

`include "mp64_defs.vh"

module mp64_x25519 (
    input  wire        clk,
    input  wire        rst_n,

    // MMIO bus interface
    input  wire        req,
    input  wire [5:0]  addr,       // byte offset within 64-byte block
    input  wire [63:0] wdata,
    input  wire        wen,
    output reg  [63:0] rdata,
    output reg         ack
);

    // ========================================================================
    // Constants
    // ========================================================================
    localparam [255:0] PRIME     = 256'h7FFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFED;
    localparam [255:0] A24       = 256'd121665;   // (486662 - 2) / 4, RFC 7748 §5
    localparam [255:0] P_MINUS_2 = 256'h7FFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFEB;

    // ========================================================================
    // State machine
    // ========================================================================
    localparam S_IDLE       = 3'd0,
               S_CLAMP      = 3'd1,
               S_LADDER     = 3'd2,
               S_FINAL_SWAP = 3'd3,
               S_INVERT     = 3'd4,
               S_FINAL      = 3'd5,
               S_DONE       = 3'd6;

    reg [2:0]  state;
    reg        busy, done;

    // ========================================================================
    // I/O registers
    // ========================================================================
    reg [255:0] scalar_reg;
    reg [255:0] point_reg;       // x_1 (u-coordinate, constant throughout)
    reg [255:0] result_reg;

    // ========================================================================
    // Montgomery ladder state (projective coordinates)
    //   x_2 = X2/Z2,  x_3 = X3/Z3
    // ========================================================================
    reg [255:0] X2, Z2, X3, Z3;
    reg         swap_bit;        // XOR-accumulated swap (RFC 7748 §5)
    reg [7:0]   bit_idx;         // current scalar bit (254 downto 0)

    // ========================================================================
    // Ladder temporaries — one named register per intermediate value.
    // All call sites of field_mul are in mutually exclusive case branches;
    // synthesis shares a single 256×256 multiplier via operand muxing.
    // ========================================================================
    reg [255:0] rA, rB;          // A = X2+Z2,  B = X2−Z2
    reg [255:0] rAA, rBB;        // AA = A²,    BB = B²
    reg [255:0] rE;              // E  = AA − BB
    reg [255:0] rC, rD;          // C  = X3+Z3, D = X3−Z3
    reg [255:0] rDA, rCB;        // DA = D·A,   CB = C·B  (then repurposed)
    reg [255:0] rT;              // scratch: a24·E, then AA+a24·E

    reg [3:0]   ladder_phase;    // 0..13 per ladder bit

    // ========================================================================
    // Inversion (Fermat: z^(p−2), binary square-and-multiply)
    // ========================================================================
    reg [255:0] inv_acc;
    reg [255:0] inv_base;
    reg [8:0]   inv_step;        // 0..254
    reg [1:0]   inv_phase;       // 0=square, 1=cond-mul, 2=advance

    // ========================================================================
    // Field arithmetic (combinational functions)
    // ========================================================================

    // (a + b) mod p
    function [255:0] field_add;
        input [255:0] a, b;
        reg [256:0] sum;
        begin
            sum = {1'b0, a} + {1'b0, b};
            if (sum >= {1'b0, PRIME})
                field_add = sum - {1'b0, PRIME};
            else
                field_add = sum[255:0];
        end
    endfunction

    // (a − b) mod p
    function [255:0] field_sub;
        input [255:0] a, b;
        begin
            if (a >= b)
                field_sub = a - b;
            else
                field_sub = PRIME - (b - a);
        end
    endfunction

    // 512-bit → 256-bit reduction mod p = 2^255 − 19.
    // Uses:  x mod p ≡ x_lo + 19·x_hi  where x = x_hi·2^255 + x_lo.
    // Applied twice; final subtract-if-greater-than-p (at most twice).
    function [255:0] field_reduce;
        input [511:0] x;
        reg [255:0] lo;
        reg [256:0] hi;
        reg [511:0] partial;
        reg [256:0] r;
        begin
            lo = x[254:0];
            hi = x[511:255];
            partial = {1'b0, lo} + hi * 19;
            lo = partial[254:0];
            hi = partial[511:255];
            partial = {1'b0, lo} + hi * 19;
            r = partial[256:0];
            if (r >= {1'b0, PRIME})  r = r - {1'b0, PRIME};
            if (r >= {1'b0, PRIME})  r = r - {1'b0, PRIME};
            field_reduce = r[255:0];
        end
    endfunction

    // (a × b) mod p — single 256×256 schoolbook multiply + reduction.
    // Synthesis note: all call sites are in mutually exclusive case arms,
    // so the synthesiser shares one DSP-based multiplier via operand muxing.
    function [255:0] field_mul;
        input [255:0] a, b;
        begin
            field_mul = field_reduce(a * b);
        end
    endfunction

    // Scalar clamping (RFC 7748 §5):
    //   clear bits 0,1,2 of byte 0;  clear bit 7 of byte 31;  set bit 6 of byte 31.
    function [255:0] clamp;
        input [255:0] s;
        reg [255:0] r;
        begin
            r       = s;
            r[0]    = 1'b0;
            r[1]    = 1'b0;
            r[2]    = 1'b0;
            r[255]  = 1'b0;
            r[254]  = 1'b1;
            clamp   = r;
        end
    endfunction

    // ========================================================================
    // Main sequential logic — MMIO writes + state machine
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            scalar_reg   <= 256'd0;
            point_reg    <= 256'd0;
            result_reg   <= 256'd0;
            state        <= S_IDLE;
            busy         <= 1'b0;
            done         <= 1'b0;
            swap_bit     <= 1'b0;
            bit_idx      <= 8'd0;
            ladder_phase <= 4'd0;
            X2 <= 256'd0; Z2 <= 256'd0;
            X3 <= 256'd0; Z3 <= 256'd0;
            rA <= 256'd0; rB <= 256'd0;
            rAA <= 256'd0; rBB <= 256'd0; rE <= 256'd0;
            rC <= 256'd0; rD <= 256'd0;
            rDA <= 256'd0; rCB <= 256'd0; rT <= 256'd0;
            inv_acc  <= 256'd0;
            inv_base <= 256'd0;
            inv_step <= 9'd0;
            inv_phase <= 2'd0;
        end else begin

            // ---- MMIO writes ----
            if (req && wen) begin
                case (addr[5:3])
                    3'b000: scalar_reg[ 63:  0] <= wdata;  // 0x00
                    3'b001: scalar_reg[127: 64] <= wdata;  // 0x08
                    3'b010: scalar_reg[191:128] <= wdata;  // 0x10
                    3'b011: scalar_reg[255:192] <= wdata;  // 0x18
                    3'b100: point_reg [ 63:  0] <= wdata;  // 0x20
                    3'b101: point_reg [127: 64] <= wdata;  // 0x28
                    3'b110: point_reg [191:128] <= wdata;  // 0x30
                    3'b111: begin
                        if (addr[2:0] == 3'b000)
                            point_reg[255:192] <= wdata;   // 0x38
                        else if (wdata[0] && state == S_IDLE) begin
                            state <= S_CLAMP;
                            busy  <= 1'b1;
                            done  <= 1'b0;
                        end
                    end
                endcase
            end

            // ================================================================
            // State machine
            // ================================================================
            case (state)

                // ------------------------------------------------------------
                // S_CLAMP — apply RFC 7748 clamping, init ladder
                // ------------------------------------------------------------
                S_CLAMP: begin
                    scalar_reg     <= clamp(scalar_reg);
                    point_reg[255] <= 1'b0;        // mask MSB of u
                    X2 <= 256'd1;                  // (X2:Z2) = (1:0) — ∞
                    Z2 <= 256'd0;
                    X3 <= point_reg;               // (X3:Z3) = (u:1)
                    X3[255] <= 1'b0;
                    Z3 <= 256'd1;
                    bit_idx      <= 8'd254;        // start at bit 254
                    swap_bit     <= 1'b0;
                    ladder_phase <= 4'd0;
                    state        <= S_LADDER;
                end

                // ------------------------------------------------------------
                // S_LADDER — Montgomery ladder (RFC 7748 §5)
                //
                // 14 phases per scalar bit, exactly one field_mul per
                // multiply phase (10 multiplies per ladder step):
                //
                //  Phase  0: cswap(swap⊕k_t); swap←k_t; A←X2+Z2; B←X2−Z2
                //  Phase  1: AA ← A²                                [MUL 1]
                //  Phase  2: BB ← B²                                [MUL 2]
                //  Phase  3: E←AA−BB; C←X3+Z3; D←X3−Z3
                //  Phase  4: DA ← D·A                               [MUL 3]
                //  Phase  5: CB ← C·B                               [MUL 4]
                //  Phase  6: X2 ← AA·BB                             [MUL 5]
                //  Phase  7: T  ← a24·E                             [MUL 6]
                //  Phase  8: T←AA+T; DA←DA+CB; CB←DA−CB  (no mul)
                //  Phase  9: X3 ← DA²  (=(DA+CB)²)                 [MUL 7]
                //  Phase 10: CB ← CB²  (=(DA−CB)²)                 [MUL 8]
                //  Phase 11: Z3 ← x₁·CB                            [MUL 9]
                //  Phase 12: Z2 ← E·T                              [MUL 10]
                //  Phase 13: advance bit_idx or → S_FINAL_SWAP
                // ------------------------------------------------------------
                S_LADDER: begin
                    case (ladder_phase)

                        4'd0: begin
                            // RFC 7748 XOR-accumulated conditional swap:
                            //   swap ^= k_t;  cswap(swap, x2, x3);  swap = k_t
                            // Implemented as a mux — constant-time, no branch.
                            if (swap_bit ^ scalar_reg[bit_idx]) begin
                                // Swap: new X2 = old X3, new Z2 = old Z3
                                X2 <= X3;  Z2 <= Z3;
                                X3 <= X2;  Z3 <= Z2;
                                // A, B computed from post-swap X2 (= old X3)
                                rA <= field_add(X3, Z3);
                                rB <= field_sub(X3, Z3);
                            end else begin
                                rA <= field_add(X2, Z2);
                                rB <= field_sub(X2, Z2);
                            end
                            swap_bit <= scalar_reg[bit_idx];
                            ladder_phase <= 4'd1;
                        end

                        4'd1: begin
                            rAA <= field_mul(rA, rA);           // AA = A²
                            ladder_phase <= 4'd2;
                        end

                        4'd2: begin
                            rBB <= field_mul(rB, rB);           // BB = B²
                            ladder_phase <= 4'd3;
                        end

                        4'd3: begin
                            rE <= field_sub(rAA, rBB);          // E = AA − BB
                            rC <= field_add(X3, Z3);            // C = X3 + Z3
                            rD <= field_sub(X3, Z3);            // D = X3 − Z3
                            ladder_phase <= 4'd4;
                        end

                        4'd4: begin
                            rDA <= field_mul(rD, rA);           // DA = D·A
                            ladder_phase <= 4'd5;
                        end

                        4'd5: begin
                            rCB <= field_mul(rC, rB);           // CB = C·B
                            ladder_phase <= 4'd6;
                        end

                        4'd6: begin
                            X2 <= field_mul(rAA, rBB);          // X2 = AA·BB
                            ladder_phase <= 4'd7;
                        end

                        4'd7: begin
                            rT <= field_mul(A24, rE);           // T = a24·E
                            ladder_phase <= 4'd8;
                        end

                        4'd8: begin
                            // Non-blocking: all RHS sampled before any LHS updated
                            rT  <= field_add(rAA, rT);         // T  = AA + a24·E
                            rDA <= field_add(rDA, rCB);         // DA = DA + CB
                            rCB <= field_sub(rDA, rCB);         // CB = DA − CB
                            ladder_phase <= 4'd9;
                        end

                        4'd9: begin
                            X3 <= field_mul(rDA, rDA);          // X3 = (DA+CB)²
                            ladder_phase <= 4'd10;
                        end

                        4'd10: begin
                            rCB <= field_mul(rCB, rCB);         // CB = (DA−CB)²
                            ladder_phase <= 4'd11;
                        end

                        4'd11: begin
                            Z3 <= field_mul(point_reg, rCB);    // Z3 = x₁·(DA−CB)²
                            ladder_phase <= 4'd12;
                        end

                        4'd12: begin
                            Z2 <= field_mul(rE, rT);            // Z2 = E·(AA+a24·E)
                            ladder_phase <= 4'd13;
                        end

                        4'd13: begin
                            if (bit_idx == 8'd0)
                                state <= S_FINAL_SWAP;
                            else begin
                                bit_idx <= bit_idx - 8'd1;
                                ladder_phase <= 4'd0;
                            end
                        end

                        default: ladder_phase <= 4'd0;
                    endcase
                end

                // ------------------------------------------------------------
                // S_FINAL_SWAP — RFC 7748: cswap(swap, x2, x3) after loop
                // ------------------------------------------------------------
                S_FINAL_SWAP: begin
                    if (swap_bit) begin
                        X2 <= X3;  Z2 <= Z3;
                        X3 <= X2;  Z3 <= Z2;
                        inv_base <= Z3;   // post-swap Z2 is current Z3
                    end else begin
                        inv_base <= Z2;
                    end
                    inv_acc   <= 256'd1;
                    inv_step  <= 9'd0;
                    inv_phase <= 2'd0;
                    state     <= S_INVERT;
                end

                // ------------------------------------------------------------
                // S_INVERT — Z2⁻¹ via Fermat: z^(p−2) mod p
                //
                // Binary square-and-multiply, MSB first (bit 254 down to 0).
                // p−2 = 0x7F…FFEB: bit 254 set, bits 8..253 all set,
                //       byte 0 = 0xEB = 11101011₂.
                // 255 steps × (square + conditional multiply) ≈ 505 field muls.
                // ------------------------------------------------------------
                S_INVERT: begin
                    case (inv_phase)
                        2'd0: begin
                            inv_acc <= field_mul(inv_acc, inv_acc);   // square
                            inv_phase <= 2'd1;
                        end
                        2'd1: begin
                            if (P_MINUS_2[254 - inv_step[7:0]])
                                inv_acc <= field_mul(inv_acc, inv_base);
                            inv_phase <= 2'd2;
                        end
                        2'd2: begin
                            if (inv_step == 9'd254)
                                state <= S_FINAL;
                            else begin
                                inv_step  <= inv_step + 9'd1;
                                inv_phase <= 2'd0;
                            end
                        end
                        default: inv_phase <= 2'd0;
                    endcase
                end

                // ------------------------------------------------------------
                // S_FINAL — result = X2 · Z2⁻¹
                // ------------------------------------------------------------
                S_FINAL: begin
                    result_reg <= field_mul(X2, inv_acc);
                    busy <= 1'b0;
                    done <= 1'b1;
                    state <= S_DONE;
                end

                // ------------------------------------------------------------
                // S_DONE — idle until next CMD
                // ------------------------------------------------------------
                S_DONE: begin
                    if (req && wen && addr[5:3] == 3'b111 &&
                        addr[2:0] != 3'b000 && wdata[0]) begin
                        state <= S_CLAMP;
                        busy  <= 1'b1;
                        done  <= 1'b0;
                    end
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
                        3'b000: rdata <= {62'd0, done, busy};
                        3'b001: rdata <= result_reg[ 63:  0];
                        3'b010: rdata <= result_reg[127: 64];
                        3'b011: rdata <= result_reg[191:128];
                        3'b100: rdata <= result_reg[255:192];
                        default: rdata <= 64'd0;
                    endcase
                end else begin
                    rdata <= 64'd0;
                end
            end
        end
    end

endmodule
