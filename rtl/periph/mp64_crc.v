// ============================================================================
// mp64_crc.v — CRC32/CRC64 Accelerator
// ============================================================================
//
// MMIO base: 0x7C0 (32 bytes).
//
// Features:
//   - CRC32  (IEEE 802.3):   poly = 0x04C11DB7
//   - CRC32C (Castagnoli):   poly = 0x1EDC6F41
//   - CRC64  (ECMA-182):     poly = 0x42F0E1EBA9EA3693
//   - 8 bytes per cycle throughput (Sarwate / table-based)
//   - Programmable polynomial, init value
//   - Interrupt on completion
//

`include "mp64_pkg.vh"

module mp64_crc (
    input  wire        clk,
    input  wire        rst_n,

    // MMIO interface
    input  wire        req,
    input  wire [4:0]  addr,       // offset within CRC block
    input  wire [63:0] wdata,
    input  wire        wen,
    output reg  [63:0] rdata,
    output reg         ack,

    // Interrupt
    output reg         irq
);

    // ========================================================================
    // Configuration Registers
    // ========================================================================
    reg [63:0] poly;           // Polynomial (32 or 64 bit)
    reg [63:0] init_val;       // Initial CRC value
    reg [63:0] crc_reg;        // Current CRC accumulator
    reg        crc64_mode;     // 0=CRC32, 1=CRC64
    reg        busy;
    reg        done;
    reg        irq_en;

    // ========================================================================
    // CRC computation — process 8 bytes per cycle using bitwise computation
    // ========================================================================
    // For CRC32: processes 8 bits at a time × 8 bytes = 64 bits/cycle
    // For CRC64: same approach with 64-bit accumulator

    // Combinational CRC step: one byte at a time
    function [63:0] crc_byte;
        input [63:0] crc_in;
        input [7:0]  data_byte;
        input [63:0] polynomial;
        input        is_64;
        reg [63:0] crc;
        integer i;
        begin
            if (is_64) begin
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

    // Process 8 bytes in one cycle (combinational chain)
    function [63:0] crc_8bytes;
        input [63:0] crc_in;
        input [63:0] data_word;
        input [63:0] polynomial;
        input        is_64;
        reg [63:0] crc;
        begin
            crc = crc_byte(crc_in,        data_word[63:56], polynomial, is_64);
            crc = crc_byte(crc,            data_word[55:48], polynomial, is_64);
            crc = crc_byte(crc,            data_word[47:40], polynomial, is_64);
            crc = crc_byte(crc,            data_word[39:32], polynomial, is_64);
            crc = crc_byte(crc,            data_word[31:24], polynomial, is_64);
            crc = crc_byte(crc,            data_word[23:16], polynomial, is_64);
            crc = crc_byte(crc,            data_word[15:8],  polynomial, is_64);
            crc_8bytes = crc_byte(crc,     data_word[7:0],   polynomial, is_64);
        end
    endfunction

    // ========================================================================
    // MMIO Write Handler
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            poly       <= 64'h0000_0000_04C1_1DB7;  // Default: CRC32 IEEE
            init_val   <= 64'h0000_0000_FFFF_FFFF;
            crc_reg    <= 64'h0000_0000_FFFF_FFFF;
            crc64_mode <= 1'b0;
            busy       <= 1'b0;
            done       <= 1'b0;
            irq_en     <= 1'b0;
            irq        <= 1'b0;
        end else begin
            irq <= 1'b0;

            if (req && wen) begin
                case (addr)
                    // POLY register
                    5'h00: poly <= wdata;

                    // INIT register — also resets CRC accumulator
                    5'h08: begin
                        init_val <= wdata;
                        crc_reg  <= wdata;
                        done     <= 1'b0;
                    end

                    // DIN register — feed 8 bytes, compute CRC
                    5'h10: begin
                        crc_reg <= crc_8bytes(crc_reg, wdata, poly, crc64_mode);
                        done    <= 1'b1;
                        if (irq_en) irq <= 1'b1;
                    end

                    // CTRL register
                    5'h18: begin
                        crc64_mode <= wdata[0];
                        irq_en     <= wdata[1];
                        // Bit 2: reset CRC to init
                        if (wdata[2]) begin
                            crc_reg <= init_val;
                            done    <= 1'b0;
                        end
                    end

                    default: ;
                endcase
            end
        end
    end

    // ========================================================================
    // MMIO Read Handler
    // ========================================================================
    always @(posedge clk) begin
        ack <= 1'b0;
        if (req) begin
            ack <= 1'b1;
            if (!wen) begin
                case (addr)
                    5'h00:   rdata <= poly;
                    5'h08:   rdata <= init_val;
                    5'h10:   rdata <= crc_reg;     // RESULT (same addr as DIN)
                    5'h18:   rdata <= {61'd0, crc64_mode, irq_en, done};
                    default: rdata <= 64'd0;
                endcase
            end else begin
                rdata <= 64'd0;
            end
        end
    end

endmodule
