// ============================================================================
// mp64_alu.v — Megapad-64 shared ALU (combinational)
// ============================================================================
//
// Standalone 64-bit ALU with 16 operations.  Shared between major and
// micro CPU cores.  Pure combinational — the caller latches inputs and
// samples outputs on the appropriate clock edge.
//
// Operations (op encoding matches ALU_* localparams):
//     0 ADD     4 XOR     8 SHL    12 ADC
//     1 SUB     5 MOV     9 SHR    13 SBB
//     2 AND     6 NOT    10 SAR    14 ROL
//     3 OR      7 NEG    11 CMP    15 ROR
//
// Flags byte: [S:7  I:6  G:5  P:4  V:3  N:2  C:1  Z:0]
//   - Z, N, P updated for ALL operations (combinational)
//   - C, V, G updated by arithmetic / shift / rotate ops
//   - S, I untouched (pass-through from flags_in)
//

`include "mp64_defs.vh"

module mp64_alu (
    input  wire [3:0]  op,
    input  wire [63:0] a,
    input  wire [63:0] b,
    input  wire [7:0]  flags_in,
    output reg  [63:0] result,
    output reg  [7:0]  flags_out
);

    `include "mp64_cpu_common.vh"

    // Even parity of low 8 bits (1 = even number of 1-bits)
    function parity8;
        input [63:0] val;
        reg [7:0] bv;
        begin
            bv = val[7:0];
            bv = bv ^ (bv >> 4);
            bv = bv ^ (bv >> 2);
            bv = bv ^ (bv >> 1);
            parity8 = ~bv[0];
        end
    endfunction

    reg [64:0] wide;   // 65-bit for carry detection

    always @(*) begin
        result    = 64'd0;
        flags_out = flags_in;
        wide      = 65'd0;

        case (op)
            ALU_ADD: begin
                wide = {1'b0, a} + {1'b0, b};
                result = wide[63:0];
                flags_out[1] = wide[64];                          // C
                flags_out[3] = (~(a[63] ^ b[63])) &
                               (a[63] ^ wide[63]);                // V
            end

            ALU_ADC: begin
                wide = {1'b0, a} + {1'b0, b} + {64'd0, flags_in[1]};
                result = wide[63:0];
                flags_out[1] = wide[64];
                flags_out[3] = (~(a[63] ^ b[63])) &
                               (a[63] ^ wide[63]);
            end

            ALU_SUB, ALU_CMP: begin
                wide = {1'b0, a} - {1'b0, b};
                result = wide[63:0];
                flags_out[1] = (a >= b) ? 1'b1 : 1'b0;           // C (no borrow)
                flags_out[3] = (a[63] ^ b[63]) &
                               (a[63] ^ result[63]);              // V
                flags_out[5] = (a > b)  ? 1'b1 : 1'b0;           // G
            end

            ALU_SBB: begin
                wide = {1'b0, a} - {1'b0, b} - {64'd0, ~flags_in[1]};
                result = wide[63:0];
                flags_out[1] = !wide[64];
            end

            ALU_AND: begin
                result = a & b;
                flags_out[1] = 1'b0;
                flags_out[3] = 1'b0;
            end

            ALU_OR: begin
                result = a | b;
                flags_out[1] = 1'b0;
                flags_out[3] = 1'b0;
            end

            ALU_XOR: begin
                result = a ^ b;
                flags_out[1] = 1'b0;
                flags_out[3] = 1'b0;
            end

            ALU_MOV: result = b;

            ALU_NOT: begin
                result = ~b;
                flags_out[1] = 1'b0;
                flags_out[3] = 1'b0;
            end

            ALU_NEG: begin
                result = (~b) + 64'd1;
                flags_out[1] = (b != 64'd0) ? 1'b1 : 1'b0;
            end

            ALU_SHL: begin
                result = a << b[5:0];
                flags_out[1] = (b[5:0] != 0) ?
                    a[64 - b[5:0]] : 1'b0;
            end

            ALU_SHR: begin
                result = a >> b[5:0];
                flags_out[1] = (b[5:0] != 0) ?
                    a[b[5:0] - 1] : 1'b0;
            end

            ALU_SAR: begin
                result = $signed(a) >>> b[5:0];
                flags_out[1] = (b[5:0] != 0) ?
                    a[b[5:0] - 1] : 1'b0;
            end

            ALU_ROL: begin
                if (b[5:0] != 0)
                    result = (a << b[5:0]) |
                             (a >> (7'd64 - {1'b0, b[5:0]}));
                else
                    result = a;
            end

            ALU_ROR: begin
                if (b[5:0] != 0)
                    result = (a >> b[5:0]) |
                             (a << (7'd64 - {1'b0, b[5:0]}));
                else
                    result = a;
            end

            default: result = 64'd0;
        endcase

        // Common flags for all ALU ops
        flags_out[0] = (result == 64'd0);             // Z
        flags_out[2] = result[63];                     // N
        flags_out[4] = parity8(result);                // P
    end

endmodule
