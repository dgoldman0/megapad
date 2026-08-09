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
//   0x4 CRC.MODE   — select a complete parameter tuple
//   0x5 CRC.SEED   — load a mode-width accumulator from R[s]
//   0x6 CRC.FINRAW — publish the raw accumulator without XOR-out
//
// CRC state: 64-bit accumulator (crc_acc) + 3-bit mode (crc_mode)
// These are CSR-accessible (0x80, 0x81).
//
// Modes 0/1/2 are MSB-first. Modes 4/5/6 are the corresponding LSB-first
// tuples. All modes use an all-ones initial value and XOR-out.
//

`include "mp64_pkg.vh"

module mp64_crc_isa (
    // Inputs
    input  wire [3:0]  op,          // CRC sub-op [3:0]
    input  wire [63:0] rs_val,      // Source register value
    input  wire [7:0]  imm8,        // Immediate byte (for CRC.MODE)
    input  wire [63:0] crc_acc_in,  // Current CRC accumulator
    input  wire [2:0]  crc_mode_in, // Current CRC mode (0/1/2/4/5/6)

    // Outputs
    output reg  [63:0] crc_acc_out, // Next CRC accumulator
    output reg  [2:0]  crc_mode_out,// Next CRC mode
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
    wire        is_reflected;

    assign is_64 = (crc_mode_in == 3'd2) || (crc_mode_in == 3'd6);
    assign is_reflected = crc_mode_in[2];
    assign poly  = (crc_mode_in == 3'd1) ? 64'h0000_0000_1EDC_6F41
                 : (crc_mode_in == 3'd2) ? 64'h42F0_E1EB_A9EA_3693
                 : (crc_mode_in == 3'd4) ? 64'h0000_0000_EDB8_8320
                 : (crc_mode_in == 3'd5) ? 64'h0000_0000_82F6_3B78
                 : (crc_mode_in == 3'd6) ? 64'hC96C_5795_D787_0F42
                 :                          64'h0000_0000_04C1_1DB7;

    // ========================================================================
    // CRC byte computation. Reflected modes XOR into the low byte and shift
    // right; non-reflected modes XOR into the high byte and shift left.
    // ========================================================================
    function [63:0] crc_byte;
        input [63:0] crc_in;
        input [7:0]  data_byte;
        input [63:0] polynomial;
        input        is_crc64;
        input        reflected;
        reg [63:0] crc;
        integer i;
        begin
            if (reflected) begin
                if (is_crc64) begin
                    crc = crc_in ^ {56'd0, data_byte};
                    for (i = 0; i < 8; i = i + 1) begin
                        if (crc[0])
                            crc = {1'b0, crc[63:1]} ^ polynomial;
                        else
                            crc = {1'b0, crc[63:1]};
                    end
                end else begin
                    crc = {32'd0, crc_in[31:0] ^ {24'd0, data_byte}};
                    for (i = 0; i < 8; i = i + 1) begin
                        if (crc[0])
                            crc[31:0] = {1'b0, crc[31:1]}
                                      ^ polynomial[31:0];
                        else
                            crc[31:0] = {1'b0, crc[31:1]};
                    end
                end
            end else if (is_crc64) begin
                crc = crc_in ^ {data_byte, 56'd0};
                for (i = 0; i < 8; i = i + 1) begin
                    if (crc[63])
                        crc = {crc[62:0], 1'b0} ^ polynomial;
                    else
                        crc = {crc[62:0], 1'b0};
                end
            end else begin
                crc = {32'd0, crc_in[31:0]};
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
        input        reflected;
        reg [63:0] crc;
        begin
            // LE order: process low byte first
            crc = crc_byte(crc_in, data_word[7:0], polynomial, is_crc64,
                           reflected);
            crc = crc_byte(crc, data_word[15:8], polynomial, is_crc64,
                           reflected);
            crc = crc_byte(crc, data_word[23:16], polynomial, is_crc64,
                           reflected);
            crc = crc_byte(crc, data_word[31:24], polynomial, is_crc64,
                           reflected);
            crc = crc_byte(crc, data_word[39:32], polynomial, is_crc64,
                           reflected);
            crc = crc_byte(crc, data_word[47:40], polynomial, is_crc64,
                           reflected);
            crc = crc_byte(crc, data_word[55:48], polynomial, is_crc64,
                           reflected);
            crc_8bytes = crc_byte(crc, data_word[63:56], polynomial,
                                  is_crc64, reflected);
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
                crc_acc_out = crc_byte(crc_acc_in, rs_val[7:0], poly, is_64,
                                       is_reflected);
                result      = crc_acc_out;
                acc_we      = 1'b1;
                rd_we       = 1'b1;
            end

            ISA_CRC_Q: begin
                // CRC.Q: feed 8 bytes (LE order)
                crc_acc_out = crc_8bytes(crc_acc_in, rs_val, poly, is_64,
                                         is_reflected);
                result      = crc_acc_out;
                acc_we      = 1'b1;
                rd_we       = 1'b1;
            end

            ISA_CRC_FIN: begin
                // CRC.FIN atomically publishes the finalized value both to
                // Rd and CRC_ACC.  Micro-core clusters release their shared
                // transaction lock on this operation, so a later CSR write
                // would race the next owner.
                result      = is_64
                            ? crc_acc_in ^ 64'hFFFF_FFFF_FFFF_FFFF
                            : {32'd0, crc_acc_in[31:0] ^ 32'hFFFF_FFFF};
                crc_acc_out = result;
                acc_we      = 1'b1;
                rd_we       = 1'b1;
            end

            ISA_CRC_MODEX: begin
                // Validate the complete immediate before narrowing it.
                crc_mode_out = (imm8 == 8'd1) ? 3'd1
                             : (imm8 == 8'd2) ? 3'd2
                             : (imm8 == 8'd4) ? 3'd4
                             : (imm8 == 8'd5) ? 3'd5
                             : (imm8 == 8'd6) ? 3'd6
                             :                     3'd0;
                mode_we      = 1'b1;
            end

            ISA_CRC_SEED: begin
                // A 32-bit mode never carries hidden state in the high half.
                crc_acc_out = is_64 ? rs_val : {32'd0, rs_val[31:0]};
                result      = crc_acc_out;
                acc_we      = 1'b1;
                rd_we       = 1'b1;
            end

            ISA_CRC_FINRAW: begin
                // Publish the mode-width running state without XOR-out or
                // bit reversal. Cluster arbitration releases ownership in
                // the same grant that commits this value.
                result      = is_64 ? crc_acc_in
                                    : {32'd0, crc_acc_in[31:0]};
                crc_acc_out = result;
                acc_we      = 1'b1;
                rd_we       = 1'b1;
            end

            default: begin
                // Reserved sub-ops: no-op (CPU traps via ILLEGAL_OP)
            end
        endcase
    end

endmodule
