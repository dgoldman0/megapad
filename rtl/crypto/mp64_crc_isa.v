// ============================================================================
// mp64_crc_isa.v — CRC ISA Datapath
// ============================================================================
//
// Combinational CRC engine for EXT.CRYPTO (FB 0x) ISA instructions.
// Instantiated per full core and once per micro-core cluster. The datapath is
// combinational; a cluster wrapper supplies arbitration and transaction lock.
//
// Sub-ops:
//   0x0 CRC.INIT   — reset accumulator to all-ones
//   0x1 CRC.B      — feed 1 byte (R[s][7:0])
//   0x2 CRC.Q      — feed 8 bytes (R[s][63:0], LE order)
//   0x3 CRC.FIN    — finalize (XOR with mask), store and return result
//   0x4 CRC.MODE   — select parameter tuple (only complete values 0/1/2)
//   0x5 CRC.SEED   — load a mode-width accumulator from R[s]
//
// CRC state: 64-bit accumulator (crc_acc) + 2-bit mode (crc_mode)
// These are CSR-accessible (0x80, 0x81).
//
// Parameter tuples are MSB-first and non-reflected, with all-ones init/xor:
//   Mode 0: CRC-32/BZIP2                poly 0x04C11DB7
//   Mode 1: Castagnoli, non-reflected    poly 0x1EDC6F41
//   Mode 2: CRC-64/WE                    poly 0x42F0E1EBA9EA3693
//

`include "mp64_pkg.vh"

module mp64_crc_isa (
    // Inputs
    input  wire [3:0]  op,          // CRC sub-op [3:0]
    input  wire [63:0] rs_val,      // Source register value
    input  wire [7:0]  imm8,        // Immediate byte (for CRC.MODE)
    input  wire [63:0] crc_acc_in,  // Current CRC accumulator
    input  wire [1:0]  crc_mode_in, // Current CRC mode (0/1/2)

    // Outputs
    output reg  [63:0] crc_acc_out, // Next CRC accumulator
    output reg  [1:0]  crc_mode_out,// Next CRC mode
    output reg  [63:0] result,      // For CRC.B/CRC.Q → Rd; CRC.FIN → Rd
    output reg         acc_we,      // 1 = write crc_acc_out
    output reg         mode_we,     // 1 = write crc_mode_out
    output reg         rd_we        // 1 = write result to Rd
);

    // ========================================================================
    // Polynomial selection
    // ========================================================================
    wire [63:0] poly;
    wire        is_64;

    assign is_64 = (crc_mode_in == 2'd2);
    assign poly  = (crc_mode_in == 2'd1) ? 64'h0000_0000_1EDC_6F41
                 : (crc_mode_in == 2'd2) ? 64'h42F0_E1EB_A9EA_3693
                 :                          64'h0000_0000_04C1_1DB7;

    // ========================================================================
    // CRC byte computation (MSB-first, matching RTL mp64_crc.v)
    // ========================================================================
    function [63:0] crc_byte;
        input [63:0] crc_in;
        input [7:0]  data_byte;
        input [63:0] polynomial;
        input        is_crc64;
        reg [63:0] crc;
        integer i;
        begin
            if (is_crc64) begin
                crc = crc_in ^ {data_byte, 56'd0};
                for (i = 0; i < 8; i = i + 1) begin
                    if (crc[63])
                        crc = {crc[62:0], 1'b0} ^ polynomial;
                    else
                        crc = {crc[62:0], 1'b0};
                end
            end else begin
                crc = crc_in;
                crc[31:24] = crc[31:24] ^ data_byte;
                for (i = 0; i < 8; i = i + 1) begin
                    if (crc[31])
                        crc[31:0] = {crc[30:0], 1'b0} ^ polynomial[31:0];
                    else
                        crc[31:0] = {crc[30:0], 1'b0};
                end
            end
            crc_byte = crc;
        end
    endfunction

    // 8-byte CRC (LE byte order: byte0 = bits[7:0], byte7 = bits[63:56])
    function [63:0] crc_8bytes;
        input [63:0] crc_in;
        input [63:0] data_word;
        input [63:0] polynomial;
        input        is_crc64;
        reg [63:0] crc;
        begin
            // LE order: process low byte first
            crc = crc_byte(crc_in,  data_word[7:0],   polynomial, is_crc64);
            crc = crc_byte(crc,     data_word[15:8],   polynomial, is_crc64);
            crc = crc_byte(crc,     data_word[23:16],  polynomial, is_crc64);
            crc = crc_byte(crc,     data_word[31:24],  polynomial, is_crc64);
            crc = crc_byte(crc,     data_word[39:32],  polynomial, is_crc64);
            crc = crc_byte(crc,     data_word[47:40],  polynomial, is_crc64);
            crc = crc_byte(crc,     data_word[55:48],  polynomial, is_crc64);
            crc_8bytes = crc_byte(crc, data_word[63:56], polynomial, is_crc64);
        end
    endfunction

    // ========================================================================
    // Sub-op decode (purely combinational)
    // ========================================================================
    always @(*) begin
        // Defaults: no writes
        crc_acc_out  = crc_acc_in;
        crc_mode_out = crc_mode_in;
        result       = 64'd0;
        acc_we       = 1'b0;
        mode_we      = 1'b0;
        rd_we        = 1'b0;

        case (op)
            ISA_CRC_INIT: begin
                // CRC.INIT: acc ← all-ones (32 or 64 bit depending on mode)
                crc_acc_out = is_64 ? 64'hFFFF_FFFF_FFFF_FFFF
                                    : 64'h0000_0000_FFFF_FFFF;
                acc_we = 1'b1;
            end

            ISA_CRC_B: begin
                // CRC.B: feed 1 byte from rs_val[7:0]
                crc_acc_out = crc_byte(crc_acc_in, rs_val[7:0], poly, is_64);
                result      = crc_acc_out;
                acc_we      = 1'b1;
                rd_we       = 1'b1;
            end

            ISA_CRC_Q: begin
                // CRC.Q: feed 8 bytes (LE order)
                crc_acc_out = crc_8bytes(crc_acc_in, rs_val, poly, is_64);
                result      = crc_acc_out;
                acc_we      = 1'b1;
                rd_we       = 1'b1;
            end

            ISA_CRC_FIN: begin
                // CRC.FIN atomically publishes the finalized value both to
                // Rd and CRC_ACC.  Micro-core clusters release their shared
                // transaction lock on this operation, so a later CSR write
                // would race the next owner.
                result      = crc_acc_in ^ (is_64 ? 64'hFFFF_FFFF_FFFF_FFFF
                                                  : 64'h0000_0000_FFFF_FFFF);
                crc_acc_out = result;
                acc_we      = 1'b1;
                rd_we       = 1'b1;
            end

            ISA_CRC_MODEX: begin
                // Only the complete values 1 and 2 select alternate
                // algorithms. Every other imm8 value canonicalizes to 0;
                // do not truncate first (for example, 5 is invalid).
                crc_mode_out = (imm8 == 8'd1) ? 2'd1
                             : (imm8 == 8'd2) ? 2'd2
                             :                     2'd0;
                mode_we      = 1'b1;
            end

            ISA_CRC_SEED: begin
                // A 32-bit mode never carries hidden state in the high half.
                crc_acc_out = is_64 ? rs_val : {32'd0, rs_val[31:0]};
                result      = crc_acc_out;
                acc_we      = 1'b1;
                rd_we       = 1'b1;
            end

            default: begin
                // Reserved sub-ops: no-op (CPU traps via ILLEGAL_OP)
            end
        endcase
    end

endmodule
