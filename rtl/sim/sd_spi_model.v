// ============================================================================
// sd_spi_model.v — deterministic SDHC card model for controller qualification
// ============================================================================
// This is intentionally a protocol model, not a shortcut into the controller.
// It observes six-byte SPI command frames, records their 32-bit arguments, and
// transfers complete CMD17/CMD24 data blocks with CRC16.  Fault inputs let the
// controller tests make protocol failures repeatable.

`timescale 1ns / 1ps

module sd_spi_model #(
    parameter integer TOTAL_SECTORS = 8
)(
    input  wire        spi_clk,
    input  wire        spi_cs_n,
    input  wire        spi_mosi,
    output reg         spi_miso,

    input  wire        fault_silence,
    input  wire        fault_read_crc,
    input  wire [31:0] fault_crc_lba,
    input  wire        fault_write_reject,
    input  wire        fault_hold_busy,
    input  wire        fault_flush,

    output reg  [5:0]  last_command,
    output reg  [31:0] last_argument,
    output integer     read_count,
    output integer     write_count,
    output integer     flush_count
);

    localparam integer MEDIA_BYTES = TOTAL_SECTORS * 512;
    localparam integer FIFO_BYTES  = 32768;

    // Kept public so the testbench can preload and inspect deterministic media.
    reg [7:0] media [0:MEDIA_BYTES-1];
    reg [7:0] write_buffer [0:511];
    reg [7:0] response_fifo [0:FIFO_BYTES-1];

    integer response_head;
    integer response_tail;
    integer i;

    reg card_ready;
    reg selected;
    reg [7:0] tx_byte;
    integer tx_bit;
    reg [7:0] rx_shift;
    integer rx_bit;

    reg [7:0] command_bytes [0:5];
    integer command_byte;

    // 0: command traffic, 1: wait for write token, 2: write payload,
    // 3/4: high/low CRC byte.
    integer write_phase;
    integer write_index;
    reg [31:0] write_lba;
    reg [15:0] write_crc;
    reg [15:0] received_write_crc;

    function [15:0] crc16_byte;
        input [15:0] crc_in;
        input [7:0] data_in;
        integer bit_index;
        reg [15:0] c;
        begin
            c = crc_in;
            for (bit_index = 0; bit_index < 8; bit_index = bit_index + 1) begin
                if (c[15] ^ data_in[7-bit_index])
                    c = {c[14:0], 1'b0} ^ 16'h1021;
                else
                    c = {c[14:0], 1'b0};
            end
            crc16_byte = c;
        end
    endfunction

    task enqueue;
        input [7:0] value;
        begin
            if (response_tail >= FIFO_BYTES) begin
                $display("SD MODEL ERROR: response FIFO overflow");
                $finish;
            end
            response_fifo[response_tail] = value;
            response_tail = response_tail + 1;
        end
    endtask

    task enqueue_read_block;
        input [31:0] lba;
        integer byte_index;
        reg [15:0] crc;
        begin
            enqueue(8'h00); // R1
            enqueue(8'hFF); // legal token latency
            enqueue(8'hFE);
            crc = 16'd0;
            for (byte_index = 0; byte_index < 512; byte_index = byte_index + 1) begin
                enqueue(media[(lba * 512) + byte_index]);
                crc = crc16_byte(crc, media[(lba * 512) + byte_index]);
            end
            if (fault_read_crc && (lba == fault_crc_lba))
                crc = crc ^ 16'h0001;
            enqueue(crc[15:8]);
            enqueue(crc[7:0]);
        end
    endtask

    task handle_command;
        reg [5:0] command;
        reg [31:0] argument;
        begin
            command = command_bytes[0][5:0];
            argument = {command_bytes[1], command_bytes[2],
                        command_bytes[3], command_bytes[4]};
            last_command = command;
            last_argument = argument;

            if (!fault_silence) begin
                case (command)
                    6'd0: begin
                        card_ready = 1'b0;
                        enqueue(8'h01);
                    end
                    6'd8: begin
                        enqueue(8'h01);
                        enqueue(8'h00);
                        enqueue(8'h00);
                        enqueue(8'h01);
                        enqueue(8'hAA);
                    end
                    6'd55: enqueue(card_ready ? 8'h00 : 8'h01);
                    6'd41: begin
                        card_ready = 1'b1;
                        enqueue(8'h00);
                    end
                    6'd58: begin
                        enqueue(8'h00);
                        enqueue(8'hC0); // power-up complete and CCS=1 (SDHC)
                        enqueue(8'hFF);
                        enqueue(8'h80);
                        enqueue(8'h00);
                    end
                    6'd16: enqueue(8'h00);
                    6'd17: begin
                        read_count = read_count + 1;
                        if (argument < TOTAL_SECTORS)
                            enqueue_read_block(argument);
                        else
                            enqueue(8'h20); // R1 address error
                    end
                    6'd24: begin
                        if (argument < TOTAL_SECTORS) begin
                            enqueue(8'h00);
                            write_lba = argument;
                            write_phase = 1;
                        end else begin
                            enqueue(8'h20);
                        end
                    end
                    6'd13: begin
                        flush_count = flush_count + 1;
                        enqueue(8'h00);
                        enqueue(fault_flush ? 8'h01 : 8'h00);
                    end
                    default: enqueue(8'h04); // illegal command
                endcase
            end
        end
    endtask

    task consume_byte;
        input [7:0] value;
        integer commit_index;
        begin
            case (write_phase)
                1: begin
                    if (value == 8'hFE) begin
                        write_phase = 2;
                        write_index = 0;
                        write_crc = 16'd0;
                    end
                end
                2: begin
                    write_buffer[write_index] = value;
                    write_crc = crc16_byte(write_crc, value);
                    if (write_index == 511)
                        write_phase = 3;
                    else
                        write_index = write_index + 1;
                end
                3: begin
                    received_write_crc[15:8] = value;
                    write_phase = 4;
                end
                4: begin
                    received_write_crc[7:0] = value;
                    if (!fault_write_reject &&
                        ({received_write_crc[15:8], value} == write_crc)) begin
                        for (commit_index = 0; commit_index < 512;
                             commit_index = commit_index + 1)
                            media[(write_lba * 512) + commit_index] =
                                write_buffer[commit_index];
                        write_count = write_count + 1;
                        enqueue(8'h05); // data accepted
                        if (fault_hold_busy) begin
                            for (commit_index = 0; commit_index < 256;
                                 commit_index = commit_index + 1)
                                enqueue(8'h00);
                        end else begin
                            enqueue(8'h00);
                            enqueue(8'hFF);
                        end
                    end else begin
                        enqueue(8'h0B); // CRC/write rejected
                    end
                    write_phase = 0;
                end
                default: begin
                    if (command_byte == 0) begin
                        if ((value & 8'hC0) == 8'h40) begin
                            command_bytes[0] = value;
                            command_byte = 1;
                        end
                    end else begin
                        command_bytes[command_byte] = value;
                        if (command_byte == 5) begin
                            command_byte = 0;
                            handle_command();
                        end else begin
                            command_byte = command_byte + 1;
                        end
                    end
                end
            endcase
        end
    endtask

    initial begin
        spi_miso = 1'b1;
        response_head = 0;
        response_tail = 0;
        card_ready = 1'b0;
        selected = 1'b0;
        tx_byte = 8'hFF;
        tx_bit = 7;
        rx_shift = 8'd0;
        rx_bit = 0;
        command_byte = 0;
        write_phase = 0;
        write_index = 0;
        write_lba = 32'd0;
        write_crc = 16'd0;
        received_write_crc = 16'd0;
        last_command = 6'd0;
        last_argument = 32'd0;
        read_count = 0;
        write_count = 0;
        flush_count = 0;
        for (i = 0; i < MEDIA_BYTES; i = i + 1)
            media[i] = (i[7:0] ^ (i / 512));
    end

    // Mode 0: the card changes MISO on falling edges, and the host samples it
    // on rising edges.  selected distinguishes CS assertion from a clock edge.
    always @(negedge spi_clk or posedge spi_cs_n or negedge spi_cs_n) begin
        if (spi_cs_n) begin
            spi_miso = 1'b1;
            selected = 1'b0;
            tx_byte = 8'hFF;
            tx_bit = 7;
        end else if (!selected) begin
            selected = 1'b1;
            spi_miso = 1'b1;
            tx_byte = 8'hFF;
            tx_bit = 7;
        end else if (tx_bit == 0) begin
            if (response_head < response_tail) begin
                tx_byte = response_fifo[response_head];
                response_head = response_head + 1;
            end else begin
                tx_byte = 8'hFF;
            end
            tx_bit = 7;
            spi_miso = tx_byte[7];
        end else begin
            tx_bit = tx_bit - 1;
            spi_miso = tx_byte[tx_bit];
        end
    end

    always @(posedge spi_cs_n) begin
        // A deselection terminates the current response/data transaction.
        // Discard any unread bytes so an aborted command cannot leak into the
        // response stream for the next command.
        response_head = response_tail;
        command_byte = 0;
        rx_bit = 0;
        rx_shift = 8'd0;
        if (write_phase != 0)
            write_phase = 0;
    end

    always @(posedge spi_clk) begin
        reg [7:0] completed_byte;
        if (!spi_cs_n) begin
            completed_byte = {rx_shift[6:0], spi_mosi};
            rx_shift = completed_byte;
            if (rx_bit == 7) begin
                rx_bit = 0;
                consume_byte(completed_byte);
            end else begin
                rx_bit = rx_bit + 1;
            end
        end
    end

endmodule
