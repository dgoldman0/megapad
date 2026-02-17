// ============================================================================
// mp64_disk.v — Storage Controller (SPI-SD Interface)
// ============================================================================
//
// Translates the MMIO disk register interface into SPI transactions to
// an SD card.  Supports DMA-based sector reads and writes.
//
// The BIOS uses:
//   - DISK-SEC! to set the sector number
//   - DISK-DMA! to set the DMA target address in RAM
//   - DISK-N!   to set the sector count (usually 1)
//   - DISK-READ (CMD=0x01) / DISK-WRITE (CMD=0x02)
//
// DMA writes directly into the internal BRAM via a dedicated port on the
// bus arbiter (not shown here — the SoC top-level wires it).
//

`include "mp64_pkg.vh"

module mp64_disk (
    input  wire        clk,
    input  wire        rst_n,

    // === MMIO register interface ===
    input  wire        req,
    input  wire [3:0]  addr,
    input  wire [7:0]  wdata,
    input  wire        wen,
    output reg  [7:0]  rdata,
    output reg         ack,

    // === DMA interface (to memory bus) ===
    output reg         dma_req,
    output reg  [63:0] dma_addr,
    output reg  [7:0]  dma_wdata,
    output reg         dma_wen,     // 1=write to RAM (disk read), 0=read from RAM
    input  wire [7:0]  dma_rdata,
    input  wire        dma_ack,

    // === SPI pins (directly to SD card) ===
    output wire        spi_clk,
    output reg         spi_mosi,
    input  wire        spi_miso,
    output reg         spi_cs_n     // active low chip select
);

    // ========================================================================
    // Registers
    // ========================================================================
    reg [7:0]  cmd;
    reg [7:0]  status;      // bit0=busy, bit1=error, bit7=present
    reg [31:0] sector;
    reg [63:0] dma_base;
    reg [7:0]  sec_count;

    // ========================================================================
    // SPI clock divider (system_clk / 4 = 25 MHz SPI clock)
    // ========================================================================
    reg [1:0] spi_div;
    wire spi_tick = (spi_div == 2'd0);
    assign spi_clk = spi_div[1];  // divide by 4

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n)
            spi_div <= 2'd0;
        else
            spi_div <= spi_div + 1;
    end

    // ========================================================================
    // SD/SPI state machine (simplified — real SD init is more complex)
    // ========================================================================
    localparam S_IDLE     = 4'd0;
    localparam S_CMD_SEND = 4'd1;
    localparam S_CMD_RESP = 4'd2;
    localparam S_READ_TOK = 4'd3;
    localparam S_READ_DAT = 4'd4;
    localparam S_WRITE_TOK= 4'd5;
    localparam S_WRITE_DAT= 4'd6;
    localparam S_WRITE_RSP= 4'd7;
    localparam S_DMA_WRITE= 4'd8;
    localparam S_DMA_READ = 4'd9;
    localparam S_DONE     = 4'd10;

    reg [3:0]  disk_state;
    reg [9:0]  byte_cnt;         // 0-511 within a sector
    reg [7:0]  sectors_left;
    reg [63:0] cur_dma_addr;
    reg [7:0]  spi_rx_byte;
    reg [7:0]  spi_tx_byte;
    reg [2:0]  spi_bit_cnt;
    reg        spi_byte_done;

    // SPI byte shift register (directly on spi_tick for simplicity)
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            spi_bit_cnt   <= 3'd0;
            spi_byte_done <= 1'b0;
            spi_mosi      <= 1'b1;
        end else if (spi_tick) begin
            spi_byte_done <= 1'b0;
            spi_mosi      <= spi_tx_byte[7 - spi_bit_cnt];
            spi_rx_byte[7 - spi_bit_cnt] <= spi_miso;
            if (spi_bit_cnt == 3'd7) begin
                spi_byte_done <= 1'b1;
                spi_bit_cnt   <= 3'd0;
            end else begin
                spi_bit_cnt <= spi_bit_cnt + 1;
            end
        end
    end

    // ========================================================================
    // Main disk controller FSM
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            disk_state   <= S_IDLE;
            status       <= 8'h80;   // bit7 = present
            spi_cs_n     <= 1'b1;
            dma_req      <= 1'b0;
            sectors_left <= 8'd0;
            cmd          <= 8'd0;
        end else begin
            dma_req <= 1'b0;

            case (disk_state)
                S_IDLE: begin
                    status[0] <= 1'b0;  // not busy
                    if (req && wen && addr == DISK_CMD) begin
                        cmd <= wdata;
                        case (wdata)
                            8'h01: begin // READ
                                status[0]    <= 1'b1;  // busy
                                spi_cs_n     <= 1'b0;
                                sectors_left <= sec_count;
                                cur_dma_addr <= dma_base;
                                byte_cnt     <= 10'd0;
                                disk_state   <= S_READ_DAT;
                            end
                            8'h02: begin // WRITE
                                status[0]    <= 1'b1;
                                spi_cs_n     <= 1'b0;
                                sectors_left <= sec_count;
                                cur_dma_addr <= dma_base;
                                byte_cnt     <= 10'd0;
                                disk_state   <= S_DMA_READ;
                            end
                            8'h03: begin // STATUS query
                                // status already reflects device state
                            end
                        endcase
                    end
                end

                // === READ path: SD → SPI → DMA → RAM ===
                S_READ_DAT: begin
                    if (spi_byte_done) begin
                        // Got a byte from SD — write it to RAM via DMA
                        dma_req      <= 1'b1;
                        dma_addr     <= cur_dma_addr;
                        dma_wdata    <= spi_rx_byte;
                        dma_wen      <= 1'b1;
                        disk_state   <= S_DMA_WRITE;
                    end
                end

                S_DMA_WRITE: begin
                    if (dma_ack) begin
                        cur_dma_addr <= cur_dma_addr + 1;
                        byte_cnt     <= byte_cnt + 1;
                        if (byte_cnt == 10'd511) begin
                            byte_cnt     <= 10'd0;
                            sectors_left <= sectors_left - 1;
                            if (sectors_left == 8'd1) begin
                                disk_state <= S_DONE;
                            end else begin
                                disk_state <= S_READ_DAT;
                            end
                        end else begin
                            disk_state <= S_READ_DAT;
                        end
                    end
                end

                // === WRITE path: RAM → DMA → SPI → SD ===
                S_DMA_READ: begin
                    dma_req  <= 1'b1;
                    dma_addr <= cur_dma_addr;
                    dma_wen  <= 1'b0;
                    if (dma_ack) begin
                        spi_tx_byte  <= dma_rdata;
                        disk_state   <= S_WRITE_DAT;
                    end
                end

                S_WRITE_DAT: begin
                    if (spi_byte_done) begin
                        cur_dma_addr <= cur_dma_addr + 1;
                        byte_cnt     <= byte_cnt + 1;
                        if (byte_cnt == 10'd511) begin
                            byte_cnt     <= 10'd0;
                            sectors_left <= sectors_left - 1;
                            if (sectors_left == 8'd1) begin
                                disk_state <= S_DONE;
                            end else begin
                                disk_state <= S_DMA_READ;
                            end
                        end else begin
                            disk_state <= S_DMA_READ;
                        end
                    end
                end

                S_DONE: begin
                    spi_cs_n   <= 1'b1;
                    status[0]  <= 1'b0;  // not busy
                    disk_state <= S_IDLE;
                end
            endcase
        end
    end

    // ========================================================================
    // Register read/write
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            sector    <= 32'd0;
            dma_base  <= 64'd0;
            sec_count <= 8'd1;
        end else if (req && wen) begin
            case (addr)
                // Sector number (32-bit LE)
                DISK_SECTOR + 0: sector[ 7: 0] <= wdata;
                DISK_SECTOR + 1: sector[15: 8] <= wdata;
                DISK_SECTOR + 2: sector[23:16] <= wdata;
                DISK_SECTOR + 3: sector[31:24] <= wdata;
                // DMA address (64-bit LE)
                DISK_DMA + 0: dma_base[ 7: 0] <= wdata;
                DISK_DMA + 1: dma_base[15: 8] <= wdata;
                DISK_DMA + 2: dma_base[23:16] <= wdata;
                DISK_DMA + 3: dma_base[31:24] <= wdata;
                DISK_DMA + 4: dma_base[39:32] <= wdata;
                DISK_DMA + 5: dma_base[47:40] <= wdata;
                DISK_DMA + 6: dma_base[55:48] <= wdata;
                DISK_DMA + 7: dma_base[63:56] <= wdata;
                // Sector count
                DISK_SECN: sec_count <= wdata;
            endcase
        end
    end

    always @(posedge clk) begin
        ack <= 1'b0;
        if (req) begin
            ack <= 1'b1;
            case (addr)
                DISK_STATUS:    rdata <= status;
                DISK_SECTOR+0:  rdata <= sector[ 7: 0];
                DISK_SECTOR+1:  rdata <= sector[15: 8];
                DISK_SECTOR+2:  rdata <= sector[23:16];
                DISK_SECTOR+3:  rdata <= sector[31:24];
                DISK_SECN:      rdata <= sec_count;
                default:        rdata <= 8'd0;
            endcase
        end
    end

endmodule
