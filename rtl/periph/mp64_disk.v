// ============================================================================
// mp64_disk.v — checked storage controller with SPI-SD backend
// ============================================================================
//
// The software-visible request tuple is programmed through SECTOR, DMA, and
// SECN, then snapshotted by READ or WRITE.  A command owns that immutable tuple
// until COMPLETE advances.  Setup/command writes made while BUSY are rejected
// without changing the in-flight request.  Every accepted terminal command
// advances COMPLETE exactly once and publishes RESULT plus TRANSFERRED.
//
// The SD backend uses real SPI-mode command framing.  It initializes an SD v2
// card (CMD0/CMD8/CMD55+ACMD41/CMD58, and CMD16 for byte-addressed cards), uses
// CMD17/CMD24 for each requested sector, validates read CRC16, checks write
// response/busy completion, and implements FLUSH as a checked CMD13 status
// boundary.  A successful WRITE therefore means that the card accepted the
// data block and released its busy indication; FLUSH promises no stronger
// power-loss behavior than the status actually observed from the card.

`include "mp64_pkg.vh"

module mp64_disk #(
    parameter [31:0] TOTAL_SECTORS = 32'd8192,
    parameter [63:0] DMA_BASE_ADDR = 64'd0,
    parameter [63:0] DMA_LIMIT_ADDR = 64'h0000_0000_0010_0000,
    parameter [63:0] DMA1_BASE_ADDR = 64'd1,
    parameter [63:0] DMA1_LIMIT_ADDR = 64'd0,
    parameter [63:0] DMA2_BASE_ADDR = 64'd1,
    parameter [63:0] DMA2_LIMIT_ADDR = 64'd0,
    parameter [63:0] DMA3_BASE_ADDR = 64'd1,
    parameter [63:0] DMA3_LIMIT_ADDR = 64'd0,
    parameter integer SPI_HALF_PERIOD = 2,
    parameter integer SPI_INIT_HALF_PERIOD = 125,
    parameter integer DMA_TIMEOUT_CYCLES = 1024,
    parameter integer SD_RESPONSE_POLLS = 1024,
    parameter integer SD_DATA_POLLS = 65536,
    parameter integer SD_BUSY_POLLS = 1000000,
    parameter integer SD_INIT_RETRIES = 4096
)(
    input  wire        clk,
    input  wire        rst_n,

    // MMIO register interface
    input  wire        req,
    input  wire [4:0]  addr,
    input  wire [7:0]  wdata,
    input  wire        wen,
    output reg  [7:0]  rdata,
    output reg         ack,

    // Byte-wide DMA interface.  The complete tuple remains stable until ACK.
    output reg         dma_req,
    output reg  [63:0] dma_addr,
    output reg  [7:0]  dma_wdata,
    output reg         dma_wen,
    input  wire [7:0]  dma_rdata,
    input  wire        dma_ack,
    input  wire        dma_err,

    // Card-state sidebands.  Platform wrappers may tie these to fixed defaults
    // when their board exposes no card-detect or write-protect switches.
    input  wire        card_present,
    input  wire        card_write_protected,

    // SPI mode-0 pins
    output wire        spi_clk,
    output reg         spi_mosi,
    input  wire        spi_miso,
    output wire        spi_cs_n
);

    // ------------------------------------------------------------------------
    // Stable software-visible registers
    // ------------------------------------------------------------------------
    reg [31:0] sector;
    reg [63:0] dma_base;
    reg [7:0]  sec_count;
    reg [2:0]  dma_push_ctr;

    reg [7:0]  result;
    reg [31:0] completion;
    reg [31:0] media_generation;
    reg [7:0]  transferred;

    reg busy;
    reg rejected;
    reg result_valid;
    reg media_changed;
    reg externally_visible_effect;

    // READ, WRITE, FLUSH, precise results, COMPLETE, and MEDIA_GEN.
    localparam [7:0] DISK_CAPABILITIES = 8'h3F;

    // Synchronize physical sidebands before they affect command state.
    reg present_meta, present_sync;
    reg protect_meta, protect_sync;
    reg present_prev;
    reg card_initialized;
    reg card_block_addressing;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            present_meta <= 1'b0;
            present_sync <= 1'b0;
            protect_meta <= 1'b0;
            protect_sync <= 1'b0;
        end else begin
            present_meta <= card_present;
            present_sync <= present_meta;
            protect_meta <= card_write_protected;
            protect_sync <= protect_meta;
        end
    end

    wire result_error = result_valid && (result[6:0] != DISK_RES_OK);
    wire [7:0] status_value = {
        present_sync,       // bit 7: media present
        1'b0,               // bit 6: reserved
        protect_sync,       // bit 5: write protected
        media_changed,      // bit 4: attachment generation changed
        result_valid,       // bit 3: RESULT/TRANSFERRED describe a completion
        rejected,           // bit 2: a write was rejected while BUSY
        result_error,       // bit 1: terminal RESULT is nonzero
        busy                // bit 0: accepted command is in flight
    };

    wire [31:0] visible_total = present_sync ? TOTAL_SECTORS : 32'd0;

    // ------------------------------------------------------------------------
    // SPI byte engine (mode 0, clock idle low)
    // ------------------------------------------------------------------------
    reg        spi_start;
    reg        spi_abort;
    reg [7:0]  spi_tx_request;
    reg [7:0]  spi_rx_byte;
    reg        spi_busy;
    reg        spi_done;
    reg        spi_clk_r;
    reg [31:0] spi_div_count;
    reg [2:0]  spi_bit_index;
    reg [7:0]  spi_tx_shift;
    reg [7:0]  spi_rx_shift;
    reg        cs_active;

    assign spi_clk  = spi_clk_r;
    assign spi_cs_n = ~cs_active;
    wire [31:0] spi_half_period = card_initialized ? SPI_HALF_PERIOD :
                                                       SPI_INIT_HALF_PERIOD;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            spi_rx_byte  <= 8'hFF;
            spi_busy     <= 1'b0;
            spi_done     <= 1'b0;
            spi_clk_r    <= 1'b0;
            spi_div_count<= 32'd0;
            spi_bit_index<= 3'd7;
            spi_tx_shift <= 8'hFF;
            spi_rx_shift <= 8'd0;
            spi_mosi     <= 1'b1;
        end else begin
            spi_done <= 1'b0;
            if (spi_abort) begin
                spi_busy      <= 1'b0;
                spi_clk_r     <= 1'b0;
                spi_div_count <= 32'd0;
                spi_mosi      <= 1'b1;
            end else if (spi_start && !spi_busy) begin
                spi_busy      <= 1'b1;
                spi_clk_r     <= 1'b0;
                spi_div_count <= 32'd0;
                spi_bit_index <= 3'd7;
                spi_tx_shift  <= spi_tx_request;
                spi_rx_shift  <= 8'd0;
                spi_mosi      <= spi_tx_request[7];
            end else if (spi_busy) begin
                if (spi_div_count + 32'd1 >= spi_half_period) begin
                    spi_div_count <= 32'd0;
                    if (!spi_clk_r) begin
                        // Rising edge: the card's MISO bit is stable.
                        spi_clk_r <= 1'b1;
                        spi_rx_shift[spi_bit_index] <= spi_miso;
                    end else begin
                        // Falling edge: finish this bit and prepare the next.
                        spi_clk_r <= 1'b0;
                        if (spi_bit_index == 3'd0) begin
                            spi_busy    <= 1'b0;
                            spi_done    <= 1'b1;
                            spi_rx_byte <= {spi_rx_shift[7:1], spi_miso};
                            spi_mosi    <= 1'b1;
                        end else begin
                            spi_bit_index <= spi_bit_index - 3'd1;
                            spi_mosi <= spi_tx_shift[spi_bit_index - 3'd1];
                        end
                    end
                end else begin
                    spi_div_count <= spi_div_count + 32'd1;
                end
            end
        end
    end

    // ------------------------------------------------------------------------
    // Controller helpers
    // ------------------------------------------------------------------------
    function [15:0] crc16_byte;
        input [15:0] crc_in;
        input [7:0]  data_in;
        integer k;
        reg [15:0] c;
        begin
            c = crc_in;
            for (k = 0; k < 8; k = k + 1) begin
                if (c[15] ^ data_in[7-k])
                    c = {c[14:0], 1'b0} ^ 16'h1021;
                else
                    c = {c[14:0], 1'b0};
            end
            crc16_byte = c;
        end
    endfunction

    function [6:0] r1_result;
        input [7:0] r1;
        begin
            if (r1[2])
                r1_result = DISK_RES_UNSUPPORTED;
            else if (r1[6] || r1[5])
                r1_result = DISK_RES_LBA_RANGE;
            else
                r1_result = DISK_RES_MEDIA_FAILURE;
        end
    endfunction

    function [7:0] command_frame_byte;
        input [2:0] index;
        begin
            case (index)
                3'd0: command_frame_byte = 8'h40 | {2'b00, sd_cmd_index};
                3'd1: command_frame_byte = sd_cmd_arg[31:24];
                3'd2: command_frame_byte = sd_cmd_arg[23:16];
                3'd3: command_frame_byte = sd_cmd_arg[15:8];
                3'd4: command_frame_byte = sd_cmd_arg[7:0];
                default: command_frame_byte = sd_cmd_crc;
            endcase
        end
    endfunction

    // ------------------------------------------------------------------------
    // Command state
    // ------------------------------------------------------------------------
    localparam [7:0] ST_IDLE                 = 8'd0;
    localparam [7:0] ST_INIT_CLOCKS          = 8'd1;
    localparam [7:0] ST_INIT_CMD0_START      = 8'd2;
    localparam [7:0] ST_CMD_SEND             = 8'd3;
    localparam [7:0] ST_CMD_RESPONSE         = 8'd4;
    localparam [7:0] ST_INIT_CMD0_CHECK      = 8'd5;
    localparam [7:0] ST_INIT_CMD8_START      = 8'd6;
    localparam [7:0] ST_INIT_CMD8_CHECK      = 8'd7;
    localparam [7:0] ST_INIT_CMD8_EXTRA      = 8'd8;
    localparam [7:0] ST_INIT_CMD55_START     = 8'd9;
    localparam [7:0] ST_INIT_CMD55_CHECK     = 8'd10;
    localparam [7:0] ST_INIT_ACMD41_START    = 8'd11;
    localparam [7:0] ST_INIT_ACMD41_CHECK    = 8'd12;
    localparam [7:0] ST_INIT_CMD58_START     = 8'd13;
    localparam [7:0] ST_INIT_CMD58_CHECK     = 8'd14;
    localparam [7:0] ST_INIT_OCR_EXTRA       = 8'd15;
    localparam [7:0] ST_INIT_CMD16_START     = 8'd16;
    localparam [7:0] ST_INIT_CMD16_CHECK     = 8'd17;
    localparam [7:0] ST_GAP                  = 8'd18;
    localparam [7:0] ST_DISPATCH             = 8'd19;
    localparam [7:0] ST_READ_CMD_START       = 8'd20;
    localparam [7:0] ST_READ_CMD_CHECK       = 8'd21;
    localparam [7:0] ST_READ_TOKEN           = 8'd22;
    localparam [7:0] ST_READ_DATA_SPI        = 8'd23;
    localparam [7:0] ST_READ_DATA_DMA        = 8'd24;
    localparam [7:0] ST_READ_CRC_HI          = 8'd25;
    localparam [7:0] ST_READ_CRC_LO          = 8'd26;
    localparam [7:0] ST_READ_SECTOR_DONE     = 8'd27;
    localparam [7:0] ST_WRITE_CMD_START      = 8'd28;
    localparam [7:0] ST_WRITE_CMD_CHECK      = 8'd29;
    localparam [7:0] ST_WRITE_GAP            = 8'd30;
    localparam [7:0] ST_WRITE_TOKEN          = 8'd31;
    localparam [7:0] ST_WRITE_DATA_DMA       = 8'd32;
    localparam [7:0] ST_WRITE_DATA_SPI       = 8'd33;
    localparam [7:0] ST_WRITE_CRC_HI         = 8'd34;
    localparam [7:0] ST_WRITE_CRC_LO         = 8'd35;
    localparam [7:0] ST_WRITE_RESPONSE       = 8'd36;
    localparam [7:0] ST_WRITE_BUSY           = 8'd37;
    localparam [7:0] ST_WRITE_SECTOR_DONE    = 8'd38;
    localparam [7:0] ST_FLUSH_CMD_START      = 8'd39;
    localparam [7:0] ST_FLUSH_CMD_CHECK      = 8'd40;
    localparam [7:0] ST_FLUSH_STATUS         = 8'd41;

    reg [7:0] state;
    reg [7:0] command_after_response;
    reg [7:0] gap_after_state;
    reg [5:0] sd_cmd_index;
    reg [31:0] sd_cmd_arg;
    reg [7:0] sd_cmd_crc;
    reg [2:0] command_byte_index;
    reg [7:0] last_r1;
    reg [31:0] response_polls;
    reg [31:0] init_retries;
    reg [3:0] init_clock_bytes;
    reg [2:0] extra_index;
    reg [31:0] extra_data;

    reg [7:0] active_cmd;
    reg [31:0] current_lba;
    reg [7:0] active_count;
    reg [63:0] active_dma_cursor;
    reg [9:0] data_byte_index;
    reg [7:0] pending_data;
    reg [15:0] data_crc;
    reg [7:0] read_crc_hi;
    reg [31:0] dma_wait_cycles;
    reg [31:0] data_wait_polls;

    wire [63:0] requested_bytes = {56'd0, sec_count} << 9;
    wire [63:0] requested_last_offset = requested_bytes - 64'd1;
    wire request_address_overflow = (sec_count != 8'd0) &&
        (dma_base > (64'hFFFF_FFFF_FFFF_FFFF - requested_last_offset));
    wire [63:0] requested_last = dma_base + requested_last_offset;
    // A request must be wholly contained in one real physical window.  The
    // additional windows default disabled (limit <= base), preserving simple
    // single-RAM instantiations while allowing the SoC's disjoint map.
    wire request_in_dma0 = (sec_count != 8'd0) &&
        (DMA_LIMIT_ADDR > DMA_BASE_ADDR) &&
        (dma_base >= DMA_BASE_ADDR) && (requested_last < DMA_LIMIT_ADDR);
    wire request_in_dma1 = (sec_count != 8'd0) &&
        (DMA1_LIMIT_ADDR > DMA1_BASE_ADDR) &&
        (dma_base >= DMA1_BASE_ADDR) && (requested_last < DMA1_LIMIT_ADDR);
    wire request_in_dma2 = (sec_count != 8'd0) &&
        (DMA2_LIMIT_ADDR > DMA2_BASE_ADDR) &&
        (dma_base >= DMA2_BASE_ADDR) && (requested_last < DMA2_LIMIT_ADDR);
    wire request_in_dma3 = (sec_count != 8'd0) &&
        (DMA3_LIMIT_ADDR > DMA3_BASE_ADDR) &&
        (dma_base >= DMA3_BASE_ADDR) && (requested_last < DMA3_LIMIT_ADDR);
    wire request_dma_invalid = !request_address_overflow &&
        !(request_in_dma0 || request_in_dma1 ||
          request_in_dma2 || request_in_dma3);
    wire request_lba_invalid = (sec_count != 8'd0) &&
        ((sector >= TOTAL_SECTORS) ||
         ({24'd0, sec_count} > (TOTAL_SECTORS - sector)));

    task launch_sd_command;
        input [5:0] cmd_index;
        input [31:0] cmd_arg;
        input [7:0] cmd_crc;
        input [7:0] after_response;
        begin
            sd_cmd_index <= cmd_index;
            sd_cmd_arg <= cmd_arg;
            sd_cmd_crc <= cmd_crc;
            command_byte_index <= 3'd0;
            command_after_response <= after_response;
            response_polls <= 32'd0;
            cs_active <= 1'b1;
            state <= ST_CMD_SEND;
        end
    endtask

    task schedule_gap;
        input [7:0] after_gap;
        begin
            cs_active <= 1'b0;
            gap_after_state <= after_gap;
            state <= ST_GAP;
        end
    endtask

    task complete_command;
        input [6:0] result_code;
        begin
            result <= {((result_code != DISK_RES_OK) &&
                        ((transferred != 8'd0) || externally_visible_effect)),
                       result_code};
            result_valid <= 1'b1;
            busy <= 1'b0;
            completion <= completion + 32'd1;
            dma_req <= 1'b0;
            cs_active <= 1'b0;
            spi_abort <= 1'b1;
            externally_visible_effect <= 1'b0;
            state <= ST_IDLE;
        end
    endtask

    // The system bus holds REQ until it observes this peripheral's registered
    // ACK.  Accept a side-effecting write only on the first REQ cycle.
    wire mmio_write_accept = req && wen && !ack;

    // ------------------------------------------------------------------------
    // Request, media, DMA, and SD protocol state machine
    // ------------------------------------------------------------------------
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            sector <= 32'd0;
            dma_base <= 64'd0;
            sec_count <= 8'd1;
            dma_push_ctr <= 3'd0;
            result <= 8'd0;
            completion <= 32'd0;
            media_generation <= 32'd0;
            transferred <= 8'd0;
            busy <= 1'b0;
            rejected <= 1'b0;
            result_valid <= 1'b0;
            media_changed <= 1'b0;
            externally_visible_effect <= 1'b0;
            present_prev <= 1'b0;
            state <= ST_IDLE;
            command_after_response <= ST_IDLE;
            gap_after_state <= ST_IDLE;
            sd_cmd_index <= 6'd0;
            sd_cmd_arg <= 32'd0;
            sd_cmd_crc <= 8'h01;
            command_byte_index <= 3'd0;
            last_r1 <= 8'hFF;
            response_polls <= 32'd0;
            init_retries <= 32'd0;
            init_clock_bytes <= 4'd0;
            extra_index <= 3'd0;
            extra_data <= 32'd0;
            card_initialized <= 1'b0;
            card_block_addressing <= 1'b1;
            active_cmd <= 8'd0;
            current_lba <= 32'd0;
            active_count <= 8'd0;
            active_dma_cursor <= 64'd0;
            data_byte_index <= 10'd0;
            pending_data <= 8'd0;
            data_crc <= 16'd0;
            read_crc_hi <= 8'd0;
            dma_wait_cycles <= 32'd0;
            data_wait_polls <= 32'd0;
            dma_req <= 1'b0;
            dma_addr <= 64'd0;
            dma_wdata <= 8'd0;
            dma_wen <= 1'b0;
            spi_start <= 1'b0;
            spi_abort <= 1'b0;
            spi_tx_request <= 8'hFF;
            cs_active <= 1'b0;
        end else begin
            spi_start <= 1'b0;
            spi_abort <= 1'b0;

            // Attachment identity changes invalidate all cached card state.
            if (present_sync != present_prev) begin
                present_prev <= present_sync;
                media_generation <= media_generation + 32'd1;
                media_changed <= 1'b1;
                card_initialized <= 1'b0;
            end

            // Removal and write protection observed during a command are
            // terminal device results, never apparent success.
            if (busy && !present_sync) begin
                complete_command(DISK_RES_MEDIA_REMOVED);
                card_initialized <= 1'b0;
            end else if (busy && (active_cmd == DISK_CMD_WRITE) && protect_sync) begin
                complete_command(DISK_RES_WRITE_PROTECTED);
            end else if (busy && mmio_write_accept && (addr == DISK_CMD) &&
                         (wdata == DISK_CMD_RESET)) begin
                // RESET is the sole command accepted while BUSY.  It terminates
                // the in-flight request once, as RESET_ABORTED.
                rejected <= 1'b0;
                card_initialized <= 1'b0;
                sector <= 32'd0;
                dma_base <= 64'd0;
                sec_count <= 8'd1;
                dma_push_ctr <= 3'd0;
                complete_command(DISK_RES_RESET_ABORTED);
            end else begin
                // MMIO writes either program an idle controller or are rejected
                // as a whole while BUSY.  In-flight snapshot fields never move.
                if (mmio_write_accept) begin
                    if (addr == DISK_STATUS) begin
                        // Sticky notification bits are write-one-to-clear and
                        // can be acknowledged while a command is active.
                        if (wdata[2])
                            rejected <= 1'b0;
                        // A simultaneous attachment transition wins over the
                        // clear so software cannot lose a new generation.
                        if (wdata[4] && (present_sync == present_prev))
                            media_changed <= 1'b0;
                    end else if (busy) begin
                        if ((addr == DISK_CMD) ||
                            ((addr >= DISK_SECTOR) &&
                             (addr <= DISK_DMA_PUSH)))
                            rejected <= 1'b1;
                    end else begin
                        case (addr)
                            DISK_SECTOR + 0: sector[7:0] <= wdata;
                            DISK_SECTOR + 1: sector[15:8] <= wdata;
                            DISK_SECTOR + 2: sector[23:16] <= wdata;
                            DISK_SECTOR + 3: sector[31:24] <= wdata;
                            DISK_DMA + 0: dma_base[7:0] <= wdata;
                            DISK_DMA + 1: dma_base[15:8] <= wdata;
                            DISK_DMA + 2: dma_base[23:16] <= wdata;
                            DISK_DMA + 3: dma_base[31:24] <= wdata;
                            DISK_DMA + 4: dma_base[39:32] <= wdata;
                            DISK_DMA + 5: dma_base[47:40] <= wdata;
                            DISK_DMA + 6: dma_base[55:48] <= wdata;
                            DISK_DMA + 7: dma_base[63:56] <= wdata;
                            DISK_SECN: sec_count <= wdata;
                            DISK_DMA_PUSH: begin
                                case (dma_push_ctr)
                                    3'd0: dma_base[7:0] <= wdata;
                                    3'd1: dma_base[15:8] <= wdata;
                                    3'd2: dma_base[23:16] <= wdata;
                                    3'd3: dma_base[31:24] <= wdata;
                                    3'd4: dma_base[39:32] <= wdata;
                                    3'd5: dma_base[47:40] <= wdata;
                                    3'd6: dma_base[55:48] <= wdata;
                                    3'd7: dma_base[63:56] <= wdata;
                                endcase
                                dma_push_ctr <= dma_push_ctr + 3'd1;
                            end
                            DISK_CMD: begin
                                if (wdata == DISK_CMD_STATUS) begin
                                    // Backward-compatible no-op; STATUS is read
                                    // from its register and does not publish a
                                    // synthetic command completion.
                                end else if (wdata == DISK_CMD_RESET) begin
                                    active_cmd <= wdata;
                                    transferred <= 8'd0;
                                    rejected <= 1'b0;
                                    externally_visible_effect <= 1'b0;
                                    sector <= 32'd0;
                                    dma_base <= 64'd0;
                                    sec_count <= 8'd1;
                                    dma_push_ctr <= 3'd0;
                                    card_initialized <= 1'b0;
                                    result <= 8'd0;
                                    result_valid <= 1'b0;
                                end else if ((wdata == DISK_CMD_READ) ||
                                             (wdata == DISK_CMD_WRITE) ||
                                             (wdata == DISK_CMD_FLUSH)) begin
                                    active_cmd <= wdata;
                                    transferred <= 8'd0;
                                    externally_visible_effect <= 1'b0;
                                    dma_push_ctr <= 3'd0;
                                    result <= 8'd0;
                                    result_valid <= 1'b0;
                                    current_lba <= sector;
                                    active_count <= sec_count;
                                    active_dma_cursor <= dma_base;
                                    if (!present_sync) begin
                                        result <= {1'b0, DISK_RES_NO_MEDIA};
                                        result_valid <= 1'b1;
                                        completion <= completion + 32'd1;
                                    end else if ((wdata != DISK_CMD_FLUSH) &&
                                                 (sec_count == 8'd0)) begin
                                        result <= {1'b0, DISK_RES_INVALID_COUNT};
                                        result_valid <= 1'b1;
                                        completion <= completion + 32'd1;
                                    end else if ((wdata != DISK_CMD_FLUSH) &&
                                                 request_lba_invalid) begin
                                        result <= {1'b0, DISK_RES_LBA_RANGE};
                                        result_valid <= 1'b1;
                                        completion <= completion + 32'd1;
                                    end else if ((wdata != DISK_CMD_FLUSH) &&
                                                 request_address_overflow) begin
                                        result <= {1'b0, DISK_RES_ADDRESS_OVERFLOW};
                                        result_valid <= 1'b1;
                                        completion <= completion + 32'd1;
                                    end else if ((wdata != DISK_CMD_FLUSH) &&
                                                 request_dma_invalid) begin
                                        result <= {1'b0, DISK_RES_DMA_INVALID};
                                        result_valid <= 1'b1;
                                        completion <= completion + 32'd1;
                                    end else if ((wdata == DISK_CMD_WRITE) &&
                                                 protect_sync) begin
                                        result <= {1'b0, DISK_RES_WRITE_PROTECTED};
                                        result_valid <= 1'b1;
                                        completion <= completion + 32'd1;
                                    end else begin
                                        busy <= 1'b1;
                                        if (card_initialized) begin
                                            state <= ST_DISPATCH;
                                        end else begin
                                            init_clock_bytes <= 4'd0;
                                            init_retries <= 32'd0;
                                            cs_active <= 1'b0;
                                            state <= ST_INIT_CLOCKS;
                                        end
                                    end
                                end else begin
                                    active_cmd <= wdata;
                                    transferred <= 8'd0;
                                    externally_visible_effect <= 1'b0;
                                    dma_push_ctr <= 3'd0;
                                    result <= {1'b0, DISK_RES_UNSUPPORTED};
                                    result_valid <= 1'b1;
                                    completion <= completion + 32'd1;
                                end
                            end
                            default: ; // DATA and result registers are read-only
                        endcase
                    end
                end

                if (busy) begin
                    case (state)
                        ST_INIT_CLOCKS: begin
                            cs_active <= 1'b0;
                            if (spi_done) begin
                                if (init_clock_bytes == 4'd9)
                                    state <= ST_INIT_CMD0_START;
                                else
                                    init_clock_bytes <= init_clock_bytes + 4'd1;
                            end else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_INIT_CMD0_START:
                            launch_sd_command(6'd0, 32'd0, 8'h95,
                                              ST_INIT_CMD0_CHECK);

                        ST_CMD_SEND: begin
                            if (spi_done) begin
                                if (command_byte_index == 3'd5) begin
                                    response_polls <= 32'd0;
                                    state <= ST_CMD_RESPONSE;
                                end else begin
                                    command_byte_index <= command_byte_index + 3'd1;
                                end
                            end else if (!spi_busy) begin
                                spi_tx_request <= command_frame_byte(command_byte_index);
                                spi_start <= 1'b1;
                            end
                        end

                        ST_CMD_RESPONSE: begin
                            if (spi_done) begin
                                if (spi_rx_byte == 8'hFF) begin
                                    if (response_polls + 32'd1 >= SD_RESPONSE_POLLS)
                                        complete_command(DISK_RES_TIMEOUT);
                                    else
                                        response_polls <= response_polls + 32'd1;
                                end else begin
                                    last_r1 <= spi_rx_byte;
                                    state <= command_after_response;
                                end
                            end else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_INIT_CMD0_CHECK: begin
                            if (last_r1 == 8'h01)
                                schedule_gap(ST_INIT_CMD8_START);
                            else
                                complete_command(DISK_RES_MEDIA_FAILURE);
                        end

                        ST_INIT_CMD8_START:
                            launch_sd_command(6'd8, 32'h0000_01AA, 8'h87,
                                              ST_INIT_CMD8_CHECK);

                        ST_INIT_CMD8_CHECK: begin
                            if (last_r1 == 8'h01) begin
                                extra_index <= 3'd0;
                                extra_data <= 32'd0;
                                state <= ST_INIT_CMD8_EXTRA;
                            end else if (last_r1[2]) begin
                                // This controller deliberately refuses legacy
                                // cards that cannot report the SD-v2 voltage
                                // and check pattern safely.
                                complete_command(DISK_RES_UNSUPPORTED);
                            end else begin
                                complete_command(DISK_RES_MEDIA_FAILURE);
                            end
                        end

                        ST_INIT_CMD8_EXTRA: begin
                            if (spi_done) begin
                                case (extra_index)
                                    3'd0: extra_data[31:24] <= spi_rx_byte;
                                    3'd1: extra_data[23:16] <= spi_rx_byte;
                                    3'd2: extra_data[15:8] <= spi_rx_byte;
                                    default: extra_data[7:0] <= spi_rx_byte;
                                endcase
                                if (extra_index == 3'd3) begin
                                    if ((extra_data[31:8] == 24'h000001) &&
                                        (spi_rx_byte == 8'hAA))
                                        schedule_gap(ST_INIT_CMD55_START);
                                    else
                                        complete_command(DISK_RES_MEDIA_FAILURE);
                                end else begin
                                    extra_index <= extra_index + 3'd1;
                                end
                            end else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_INIT_CMD55_START:
                            launch_sd_command(6'd55, 32'd0, 8'h01,
                                              ST_INIT_CMD55_CHECK);

                        ST_INIT_CMD55_CHECK: begin
                            if ((last_r1 == 8'h01) || (last_r1 == 8'h00))
                                schedule_gap(ST_INIT_ACMD41_START);
                            else
                                complete_command(r1_result(last_r1));
                        end

                        ST_INIT_ACMD41_START:
                            launch_sd_command(6'd41, 32'h4000_0000, 8'h01,
                                              ST_INIT_ACMD41_CHECK);

                        ST_INIT_ACMD41_CHECK: begin
                            if (last_r1 == 8'h00) begin
                                schedule_gap(ST_INIT_CMD58_START);
                            end else if (last_r1 == 8'h01) begin
                                if (init_retries + 32'd1 >= SD_INIT_RETRIES)
                                    complete_command(DISK_RES_TIMEOUT);
                                else begin
                                    init_retries <= init_retries + 32'd1;
                                    schedule_gap(ST_INIT_CMD55_START);
                                end
                            end else begin
                                complete_command(r1_result(last_r1));
                            end
                        end

                        ST_INIT_CMD58_START:
                            launch_sd_command(6'd58, 32'd0, 8'h01,
                                              ST_INIT_CMD58_CHECK);

                        ST_INIT_CMD58_CHECK: begin
                            if (last_r1 == 8'h00) begin
                                extra_index <= 3'd0;
                                extra_data <= 32'd0;
                                state <= ST_INIT_OCR_EXTRA;
                            end else begin
                                complete_command(r1_result(last_r1));
                            end
                        end

                        ST_INIT_OCR_EXTRA: begin
                            if (spi_done) begin
                                case (extra_index)
                                    3'd0: begin
                                        extra_data[31:24] <= spi_rx_byte;
                                        card_block_addressing <= spi_rx_byte[6];
                                    end
                                    3'd1: extra_data[23:16] <= spi_rx_byte;
                                    3'd2: extra_data[15:8] <= spi_rx_byte;
                                    default: extra_data[7:0] <= spi_rx_byte;
                                endcase
                                if (extra_index == 3'd3) begin
                                    if (card_block_addressing) begin
                                        card_initialized <= 1'b1;
                                        schedule_gap(ST_DISPATCH);
                                    end else begin
                                        schedule_gap(ST_INIT_CMD16_START);
                                    end
                                end else begin
                                    extra_index <= extra_index + 3'd1;
                                end
                            end else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_INIT_CMD16_START:
                            launch_sd_command(6'd16, 32'd512, 8'h01,
                                              ST_INIT_CMD16_CHECK);

                        ST_INIT_CMD16_CHECK: begin
                            if (last_r1 == 8'h00) begin
                                card_initialized <= 1'b1;
                                schedule_gap(ST_DISPATCH);
                            end else begin
                                complete_command(r1_result(last_r1));
                            end
                        end

                        ST_GAP: begin
                            cs_active <= 1'b0;
                            if (spi_done)
                                state <= gap_after_state;
                            else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_DISPATCH: begin
                            case (active_cmd)
                                DISK_CMD_READ:  state <= ST_READ_CMD_START;
                                DISK_CMD_WRITE: state <= ST_WRITE_CMD_START;
                                DISK_CMD_FLUSH: state <= ST_FLUSH_CMD_START;
                                default: complete_command(DISK_RES_INTERNAL);
                            endcase
                        end

                        ST_READ_CMD_START: begin
                            if (!card_block_addressing && |current_lba[31:23])
                                complete_command(DISK_RES_ADDRESS_OVERFLOW);
                            else
                                launch_sd_command(6'd17,
                                    card_block_addressing ? current_lba :
                                                            (current_lba << 9),
                                    8'h01, ST_READ_CMD_CHECK);
                        end

                        ST_READ_CMD_CHECK: begin
                            if (last_r1 == 8'h00) begin
                                data_wait_polls <= 32'd0;
                                state <= ST_READ_TOKEN;
                            end else begin
                                complete_command(r1_result(last_r1));
                            end
                        end

                        ST_READ_TOKEN: begin
                            if (spi_done) begin
                                if (spi_rx_byte == 8'hFE) begin
                                    data_byte_index <= 10'd0;
                                    data_crc <= 16'd0;
                                    state <= ST_READ_DATA_SPI;
                                end else if (spi_rx_byte != 8'hFF) begin
                                    complete_command(DISK_RES_MEDIA_FAILURE);
                                end else if (data_wait_polls + 32'd1 >= SD_DATA_POLLS) begin
                                    complete_command(DISK_RES_TIMEOUT);
                                end else begin
                                    data_wait_polls <= data_wait_polls + 32'd1;
                                end
                            end else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_READ_DATA_SPI: begin
                            if (spi_done) begin
                                pending_data <= spi_rx_byte;
                                data_crc <= crc16_byte(data_crc, spi_rx_byte);
                                dma_req <= 1'b1;
                                dma_addr <= active_dma_cursor;
                                dma_wdata <= spi_rx_byte;
                                dma_wen <= 1'b1;
                                // Once a downstream write is issued it may
                                // commit even if its acknowledgement is lost.
                                externally_visible_effect <= 1'b1;
                                dma_wait_cycles <= 32'd0;
                                state <= ST_READ_DATA_DMA;
                            end else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_READ_DATA_DMA: begin
                            if (dma_ack) begin
                                dma_req <= 1'b0;
                                if (dma_err) begin
                                    complete_command(DISK_RES_DMA_FAILURE);
                                end else begin
                                    externally_visible_effect <= 1'b1;
                                    active_dma_cursor <= active_dma_cursor + 64'd1;
                                    if (data_byte_index == 10'd511)
                                        state <= ST_READ_CRC_HI;
                                    else begin
                                        data_byte_index <= data_byte_index + 10'd1;
                                        state <= ST_READ_DATA_SPI;
                                    end
                                end
                            end else if (dma_wait_cycles + 32'd1 >=
                                         DMA_TIMEOUT_CYCLES) begin
                                dma_req <= 1'b0;
                                complete_command(DISK_RES_DMA_FAILURE);
                            end else begin
                                dma_wait_cycles <= dma_wait_cycles + 32'd1;
                            end
                        end

                        ST_READ_CRC_HI: begin
                            if (spi_done) begin
                                read_crc_hi <= spi_rx_byte;
                                state <= ST_READ_CRC_LO;
                            end else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_READ_CRC_LO: begin
                            if (spi_done) begin
                                if ({read_crc_hi, spi_rx_byte} == data_crc) begin
                                    // CRC acceptance is the whole-sector
                                    // confirmation edge.  Publish progress now
                                    // so reset/removal on the following cycle
                                    // cannot lose a confirmed sector.
                                    transferred <= transferred + 8'd1;
                                    if (transferred + 8'd1 >= active_count) begin
                                        complete_command(DISK_RES_OK);
                                    end else begin
                                        current_lba <= current_lba + 32'd1;
                                        schedule_gap(ST_READ_CMD_START);
                                    end
                                end else begin
                                    complete_command(DISK_RES_MEDIA_FAILURE);
                                end
                            end else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_READ_SECTOR_DONE: begin
                            complete_command(DISK_RES_INTERNAL);
                        end

                        ST_WRITE_CMD_START: begin
                            if (!card_block_addressing && |current_lba[31:23])
                                complete_command(DISK_RES_ADDRESS_OVERFLOW);
                            else
                                launch_sd_command(6'd24,
                                    card_block_addressing ? current_lba :
                                                            (current_lba << 9),
                                    8'h01, ST_WRITE_CMD_CHECK);
                        end

                        ST_WRITE_CMD_CHECK: begin
                            if (last_r1 == 8'h00)
                                state <= ST_WRITE_GAP;
                            else
                                complete_command(r1_result(last_r1));
                        end

                        ST_WRITE_GAP: begin
                            if (spi_done)
                                state <= ST_WRITE_TOKEN;
                            else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_WRITE_TOKEN: begin
                            if (spi_done) begin
                                data_byte_index <= 10'd0;
                                data_crc <= 16'd0;
                                state <= ST_WRITE_DATA_DMA;
                            end else if (!spi_busy) begin
                                spi_tx_request <= 8'hFE;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_WRITE_DATA_DMA: begin
                            if (!dma_req) begin
                                dma_req <= 1'b1;
                                dma_addr <= active_dma_cursor;
                                dma_wdata <= 8'd0;
                                dma_wen <= 1'b0;
                                dma_wait_cycles <= 32'd0;
                            end else if (dma_ack) begin
                                dma_req <= 1'b0;
                                if (dma_err) begin
                                    complete_command(DISK_RES_DMA_FAILURE);
                                end else begin
                                    pending_data <= dma_rdata;
                                    data_crc <= crc16_byte(data_crc, dma_rdata);
                                    state <= ST_WRITE_DATA_SPI;
                                end
                            end else if (dma_wait_cycles + 32'd1 >=
                                         DMA_TIMEOUT_CYCLES) begin
                                dma_req <= 1'b0;
                                complete_command(DISK_RES_DMA_FAILURE);
                            end else begin
                                dma_wait_cycles <= dma_wait_cycles + 32'd1;
                            end
                        end

                        ST_WRITE_DATA_SPI: begin
                            if (spi_done) begin
                                externally_visible_effect <= 1'b1;
                                active_dma_cursor <= active_dma_cursor + 64'd1;
                                if (data_byte_index == 10'd511)
                                    state <= ST_WRITE_CRC_HI;
                                else begin
                                    data_byte_index <= data_byte_index + 10'd1;
                                    state <= ST_WRITE_DATA_DMA;
                                end
                            end else if (!spi_busy) begin
                                spi_tx_request <= pending_data;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_WRITE_CRC_HI: begin
                            if (spi_done)
                                state <= ST_WRITE_CRC_LO;
                            else if (!spi_busy) begin
                                spi_tx_request <= data_crc[15:8];
                                spi_start <= 1'b1;
                            end
                        end

                        ST_WRITE_CRC_LO: begin
                            if (spi_done) begin
                                data_wait_polls <= 32'd0;
                                state <= ST_WRITE_RESPONSE;
                            end else if (!spi_busy) begin
                                spi_tx_request <= data_crc[7:0];
                                spi_start <= 1'b1;
                            end
                        end

                        ST_WRITE_RESPONSE: begin
                            if (spi_done) begin
                                if ((spi_rx_byte & 8'h1F) == 8'h05) begin
                                    data_wait_polls <= 32'd0;
                                    state <= ST_WRITE_BUSY;
                                end else if (spi_rx_byte == 8'hFF) begin
                                    if (data_wait_polls + 32'd1 >= SD_RESPONSE_POLLS)
                                        complete_command(DISK_RES_TIMEOUT);
                                    else
                                        data_wait_polls <= data_wait_polls + 32'd1;
                                end else begin
                                    complete_command(DISK_RES_MEDIA_FAILURE);
                                end
                            end else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_WRITE_BUSY: begin
                            if (spi_done) begin
                                if (spi_rx_byte == 8'hFF) begin
                                    // Card-ready after an accepted data token
                                    // is the WRITE confirmation boundary.
                                    transferred <= transferred + 8'd1;
                                    if (transferred + 8'd1 >= active_count) begin
                                        complete_command(DISK_RES_OK);
                                    end else begin
                                        current_lba <= current_lba + 32'd1;
                                        schedule_gap(ST_WRITE_CMD_START);
                                    end
                                end else if (data_wait_polls + 32'd1 >= SD_BUSY_POLLS)
                                    complete_command(DISK_RES_TIMEOUT);
                                else
                                    data_wait_polls <= data_wait_polls + 32'd1;
                            end else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        ST_WRITE_SECTOR_DONE: begin
                            complete_command(DISK_RES_INTERNAL);
                        end

                        ST_FLUSH_CMD_START:
                            launch_sd_command(6'd13, 32'd0, 8'h01,
                                              ST_FLUSH_CMD_CHECK);

                        ST_FLUSH_CMD_CHECK: begin
                            if (last_r1 == 8'h00)
                                state <= ST_FLUSH_STATUS;
                            else
                                complete_command(DISK_RES_FLUSH_FAILURE);
                        end

                        ST_FLUSH_STATUS: begin
                            if (spi_done) begin
                                if (spi_rx_byte == 8'h00)
                                    complete_command(DISK_RES_OK);
                                else
                                    complete_command(DISK_RES_FLUSH_FAILURE);
                            end else if (!spi_busy) begin
                                spi_tx_request <= 8'hFF;
                                spi_start <= 1'b1;
                            end
                        end

                        default: complete_command(DISK_RES_INTERNAL);
                    endcase
                end
            end
        end
    end

    // ------------------------------------------------------------------------
    // MMIO response.  Undefined/read-only holes return zero deterministically.
    // ------------------------------------------------------------------------
    always @(posedge clk) begin
        ack <= 1'b0;
        if (req) begin
            ack <= 1'b1;
            case (addr)
                DISK_CMD: rdata <= 8'd0;
                DISK_STATUS: rdata <= status_value;
                DISK_SECTOR + 0: rdata <= sector[7:0];
                DISK_SECTOR + 1: rdata <= sector[15:8];
                DISK_SECTOR + 2: rdata <= sector[23:16];
                DISK_SECTOR + 3: rdata <= sector[31:24];
                DISK_DMA + 0: rdata <= dma_base[7:0];
                DISK_DMA + 1: rdata <= dma_base[15:8];
                DISK_DMA + 2: rdata <= dma_base[23:16];
                DISK_DMA + 3: rdata <= dma_base[31:24];
                DISK_DMA + 4: rdata <= dma_base[39:32];
                DISK_DMA + 5: rdata <= dma_base[47:40];
                DISK_DMA + 6: rdata <= dma_base[55:48];
                DISK_DMA + 7: rdata <= dma_base[63:56];
                DISK_SECN: rdata <= sec_count;
                DISK_DATA: rdata <= 8'd0;
                DISK_DMA_PUSH: rdata <= 8'd0;
                DISK_TOTAL + 0: rdata <= visible_total[7:0];
                DISK_TOTAL + 1: rdata <= visible_total[15:8];
                DISK_TOTAL + 2: rdata <= visible_total[23:16];
                DISK_TOTAL + 3: rdata <= visible_total[31:24];
                DISK_RESULT: rdata <= result;
                DISK_COMPLETE + 0: rdata <= completion[7:0];
                DISK_COMPLETE + 1: rdata <= completion[15:8];
                DISK_COMPLETE + 2: rdata <= completion[23:16];
                DISK_COMPLETE + 3: rdata <= completion[31:24];
                DISK_MEDIA_GEN + 0: rdata <= media_generation[7:0];
                DISK_MEDIA_GEN + 1: rdata <= media_generation[15:8];
                DISK_MEDIA_GEN + 2: rdata <= media_generation[23:16];
                DISK_MEDIA_GEN + 3: rdata <= media_generation[31:24];
                DISK_CAPS: rdata <= DISK_CAPABILITIES;
                DISK_TRANSFERRED: rdata <= transferred;
                default: rdata <= 8'd0;
            endcase
        end
    end

endmodule
