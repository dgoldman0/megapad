// ============================================================================
// mp64_bitfield.v — Megapad-64 Bitfield ALU (combinational)
// ============================================================================
//
// Pure combinational 64-bit bitfield operations.  Sub-ops C8–CF of the
// MULDIV family (0xC).  Single-cycle execution — no stalls, no clk/rst.
//
// Operations (3-bit op encoding, mapping from MULDIV sub 0x8–0xF):
//     0 POPCNT   — Population count
//     1 CLZ      — Count leading zeros (0 → 64)
//     2 CTZ      — Count trailing zeros (0 → 64)
//     3 BITREV   — Reverse bit order (pure wiring)
//     4 BEXT     — Bit extract / parallel gather (pext)
//     5 BDEP     — Bit deposit / parallel scatter (pdep)
//     6 RORI     — Rotate right by immediate
//     7 BSWAP    — Byte-swap (pure wiring)
//
// Tiering:
//     Tier 1 (ops 0–3): Available on ALL cores (micro + major)
//                        ~100 LUTs total
//     Tier 2 (ops 4–7): Major cores only.  Micro-cores gate these
//                        out with ENABLE_TIER2 parameter.
//                        ~170 additional LUTs
//
// Coding rules:
//   - Verilog-2001, pure combinational (no clk/rst)
//   - No vendor primitives, no `%` or `/` operators
//   - Portable across Kintex-7, Artix-7, ECP5, simulation
//

module mp64_bitfield #(
    parameter ENABLE_TIER2 = 1   // 0 = micro-core (Tier 1 only)
) (
    input  wire [2:0]  op,       // 0–7 maps to POPCNT..BSWAP
    input  wire [63:0] a,        // R[Rd] (mask for BEXT/BDEP, source for RORI)
    input  wire [63:0] b,        // R[Rs] (source for most ops)
    input  wire [5:0]  imm,      // rotation amount for RORI (ibuf[2][5:0])
    output reg  [63:0] result,
    output wire        flag_z,
    output wire        flag_n
);

    // ====================================================================
    // Common flags — always driven from result
    // ====================================================================
    assign flag_z = (result == 64'd0);
    assign flag_n = result[63];

    // ====================================================================
    // POPCNT — Wallace tree adder (6 stages: 64→32→16→8→4→2→1)
    // ====================================================================
    // Standard parallel bit-count using the sideways add pattern.
    // Synthesis tools recognise this and produce an efficient tree.

    wire [63:0] pc_m1 = 64'h5555_5555_5555_5555;
    wire [63:0] pc_m2 = 64'h3333_3333_3333_3333;
    wire [63:0] pc_m4 = 64'h0F0F_0F0F_0F0F_0F0F;

    reg [63:0] pc_step1, pc_step2, pc_step3;
    reg [6:0]  popcnt_val;

    always @(*) begin
        pc_step1 = b - ((b >> 1) & pc_m1);
        pc_step2 = (pc_step1 & pc_m2) + ((pc_step1 >> 2) & pc_m2);
        pc_step3 = (pc_step2 + (pc_step2 >> 4)) & pc_m4;
        // Horizontal sum of bytes — each byte now holds 0–8
        popcnt_val = pc_step3[6:0]   + pc_step3[14:8]  +
                     pc_step3[22:16] + pc_step3[30:24] +
                     pc_step3[38:32] + pc_step3[46:40] +
                     pc_step3[54:48] + pc_step3[62:56];
    end

    // ====================================================================
    // CLZ — Priority encoder (leading zero count, 0 → 64)
    // ====================================================================
    // Binary search: test upper half, narrow, repeat.

    reg [6:0] clz_val;

    always @(*) begin : clz_block
        reg [63:0] x;
        x = b;
        clz_val = 7'd0;
        if (x[63:32] == 32'd0) begin clz_val = clz_val + 7'd32; x = {x[31:0], 32'd0}; end
        if (x[63:48] == 16'd0) begin clz_val = clz_val + 7'd16; x = {x[47:0], 16'd0}; end
        if (x[63:56] ==  8'd0) begin clz_val = clz_val + 7'd8;  x = {x[55:0],  8'd0}; end
        if (x[63:60] ==  4'd0) begin clz_val = clz_val + 7'd4;  x = {x[59:0],  4'd0}; end
        if (x[63:62] ==  2'd0) begin clz_val = clz_val + 7'd2;  x = {x[61:0],  2'd0}; end
        if (x[63]    ==  1'd0) begin clz_val = clz_val + 7'd1; end
        if (b == 64'd0)        begin clz_val = 7'd64; end
    end

    // ====================================================================
    // CTZ — Trailing zero count (0 → 64)
    // ====================================================================
    // Same binary search, working from LSB up.

    reg [6:0] ctz_val;

    always @(*) begin : ctz_block
        reg [63:0] x;
        x = b;
        ctz_val = 7'd0;
        if (x[31:0] == 32'd0) begin ctz_val = ctz_val + 7'd32; x = {32'd0, x[63:32]}; end
        if (x[15:0] == 16'd0) begin ctz_val = ctz_val + 7'd16; x = {16'd0, x[63:16]}; end
        if (x[7:0]  ==  8'd0) begin ctz_val = ctz_val + 7'd8;  x = { 8'd0, x[63:8]};  end
        if (x[3:0]  ==  4'd0) begin ctz_val = ctz_val + 7'd4;  x = { 4'd0, x[63:4]};  end
        if (x[1:0]  ==  2'd0) begin ctz_val = ctz_val + 7'd2;  x = { 2'd0, x[63:2]};  end
        if (x[0]    ==  1'd0) begin ctz_val = ctz_val + 7'd1; end
        if (b == 64'd0)        begin ctz_val = 7'd64; end
    end

    // ====================================================================
    // BITREV — Reverse bit order (pure wiring, zero logic)
    // ====================================================================
    wire [63:0] bitrev_val;
    genvar gi;
    generate
        for (gi = 0; gi < 64; gi = gi + 1) begin : bitrev_gen
            assign bitrev_val[gi] = b[63 - gi];
        end
    endgenerate

    // ====================================================================
    // BEXT (bit extract / parallel gather) — Tier 2
    // ====================================================================
    // Collect bits from 'a' (R[Rd]) at positions where 'b' (R[Rs]) has 1s,
    // pack them into the low bits of result.
    //   D = pext(Rd, Rs)  — Rs is mask, Rd is source

    reg [63:0] bext_val;
    generate
        if (ENABLE_TIER2) begin : bext_gen
            always @(*) begin : bext_block
                reg [63:0] src;
                reg [63:0] mask;
                reg [6:0]  j;
                integer i;
                src  = a;
                mask = b;
                bext_val = 64'd0;
                j = 7'd0;
                for (i = 0; i < 64; i = i + 1) begin
                    if (mask[i]) begin
                        bext_val[j] = src[i];
                        j = j + 7'd1;
                    end
                end
            end
        end else begin : bext_stub
            always @(a or b) bext_val = 64'd0;
        end
    endgenerate

    // ====================================================================
    // BDEP (bit deposit / parallel scatter) — Tier 2
    // ====================================================================
    // Scatter low bits of 'a' (R[Rd]) to positions where 'b' (R[Rs]) has 1s.
    //   D = pdep(Rd, Rs)  — Rs is mask, Rd is source bits

    reg [63:0] bdep_val;
    generate
        if (ENABLE_TIER2) begin : bdep_gen
            always @(*) begin : bdep_block
                reg [63:0] bits;
                reg [63:0] mask;
                reg [6:0]  j;
                integer i;
                bits = a;
                mask = b;
                bdep_val = 64'd0;
                j = 7'd0;
                for (i = 0; i < 64; i = i + 1) begin
                    if (mask[i]) begin
                        bdep_val[i] = bits[j];
                        j = j + 7'd1;
                    end
                end
            end
        end else begin : bdep_stub
            always @(a or b) bdep_val = 64'd0;
        end
    endgenerate

    // ====================================================================
    // RORI — Rotate right by immediate — Tier 2
    // ====================================================================
    // Barrel shifter: result = (a >> imm) | (a << (64 - imm))
    // When imm == 0, result = a (identity).

    reg [63:0] rori_val;
    generate
        if (ENABLE_TIER2) begin : rori_gen
            always @(*) begin
                if (imm == 6'd0)
                    rori_val = a;
                else
                    rori_val = (a >> imm) | (a << (6'd0 - imm));
            end
        end else begin : rori_stub
            always @(a or imm) rori_val = 64'd0;
        end
    endgenerate

    // ====================================================================
    // BSWAP — Byte-swap (pure wiring, zero logic) — Tier 2
    // ====================================================================
    wire [63:0] bswap_val = { b[7:0],   b[15:8],  b[23:16], b[31:24],
                              b[39:32], b[47:40], b[55:48], b[63:56] };

    // ====================================================================
    // Output mux
    // ====================================================================
    always @(*) begin
        case (op)
            3'd0:    result = {57'd0, popcnt_val};    // POPCNT
            3'd1:    result = {57'd0, clz_val};       // CLZ
            3'd2:    result = {57'd0, ctz_val};       // CTZ
            3'd3:    result = bitrev_val;             // BITREV
            3'd4:    result = bext_val;               // BEXT
            3'd5:    result = bdep_val;               // BDEP
            3'd6:    result = rori_val;               // RORI
            3'd7:    result = bswap_val;              // BSWAP
            default: result = 64'd0;
        endcase
    end

endmodule
