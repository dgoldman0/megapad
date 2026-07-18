// ============================================================================
// tb_disk_controller.v — storage contract and SD/DMA protocol qualification
// ============================================================================

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_disk_controller;
    localparam integer TOTAL_SECTORS = 8;
    localparam integer DMA_BYTES = 16384;

    reg clk;
    reg rst_n;
    initial clk = 1'b0;
    always #5 clk = ~clk;

    reg        req;
    reg [5:0]  addr;
    reg [7:0]  wdata;
    reg        wen;
    wire [7:0] rdata;
    wire       ack;

    wire        dma_req;
    wire [63:0] dma_addr;
    wire [7:0]  dma_wdata;
    wire        dma_wen;
    wire [7:0]  dma_rdata;
    wire        dma_ack;
    wire        dma_err;
    reg         dma_stall;
    reg         dma_force_error;
    reg [7:0]   dma_memory [0:DMA_BYTES-1];

    reg card_present;
    reg card_write_protected;
    wire spi_clk;
    wire spi_mosi;
    wire spi_miso;
    wire spi_cs_n;

    reg        fault_silence;
    reg        fault_read_crc;
    reg [31:0] fault_crc_lba;
    reg        fault_write_reject;
    reg        fault_hold_busy;
    reg        fault_flush;
    wire [5:0] card_last_command;
    wire [31:0] card_last_argument;
    wire integer card_read_count;
    wire integer card_write_count;
    wire integer card_flush_count;

    assign dma_rdata = (dma_addr < DMA_BYTES) ?
                       dma_memory[dma_addr[13:0]] : 8'd0;
    assign dma_ack = dma_req && !dma_stall;
    assign dma_err = dma_req && dma_force_error;

    always @(posedge clk) begin
        if (dma_req && dma_ack && dma_wen && !dma_err &&
            (dma_addr < DMA_BYTES))
            dma_memory[dma_addr[13:0]] <= dma_wdata;
    end

    mp64_disk #(
        .TOTAL_SECTORS(TOTAL_SECTORS),
        .DMA_BASE_ADDR(64'd0),
        .DMA_LIMIT_ADDR(DMA_BYTES),
        .DMA1_BASE_ADDR(DMA_BYTES),
        .DMA1_LIMIT_ADDR(DMA_BYTES + 4096),
        .SPI_HALF_PERIOD(1),
        .SPI_INIT_HALF_PERIOD(1),
        .DMA_TIMEOUT_CYCLES(64),
        .SD_RESPONSE_POLLS(64),
        .SD_DATA_POLLS(64),
        .SD_BUSY_POLLS(64),
        .SD_INIT_RETRIES(8)
    ) dut (
        .clk(clk), .rst_n(rst_n),
        .req(req), .addr(addr), .wdata(wdata), .wen(wen),
        .rdata(rdata), .ack(ack),
        .dma_req(dma_req), .dma_addr(dma_addr),
        .dma_wdata(dma_wdata), .dma_wen(dma_wen),
        .dma_rdata(dma_rdata), .dma_ack(dma_ack), .dma_err(dma_err),
        .card_present(card_present),
        .card_write_protected(card_write_protected),
        .spi_clk(spi_clk), .spi_mosi(spi_mosi),
        .spi_miso(spi_miso), .spi_cs_n(spi_cs_n)
    );

    sd_spi_model #(.TOTAL_SECTORS(TOTAL_SECTORS)) card (
        .spi_clk(spi_clk), .spi_cs_n(spi_cs_n),
        .spi_mosi(spi_mosi), .spi_miso(spi_miso),
        .fault_silence(fault_silence),
        .fault_read_crc(fault_read_crc), .fault_crc_lba(fault_crc_lba),
        .fault_write_reject(fault_write_reject),
        .fault_hold_busy(fault_hold_busy), .fault_flush(fault_flush),
        .last_command(card_last_command), .last_argument(card_last_argument),
        .read_count(card_read_count), .write_count(card_write_count),
        .flush_count(card_flush_count)
    );

    integer pass_count;
    integer fail_count;
    integer i;
    integer before_count;
    integer before_dma_count;
    integer dma_ack_count;
    integer boundary_watchdog;
    reg [31:0] expected_completion;
    reg [31:0] before_generation;
    reg [7:0] read_value;
    reg data_ok;
    reg stable_ok;
    reg [63:0] held_addr;
    reg [7:0] held_data;
    reg held_wen;

    always @(posedge clk) begin
        if (dma_req && dma_ack)
            dma_ack_count <= dma_ack_count + 1;
    end

    task check;
        input [8*80-1:0] label;
        input condition;
        begin
            if (condition) begin
                $display("  PASS: %0s", label);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s", label);
                fail_count = fail_count + 1;
            end
        end
    endtask

    task disk_write;
        input [5:0] register_address;
        input [7:0] value;
        begin
            @(negedge clk);
            req = 1'b1;
            addr = register_address;
            wdata = value;
            wen = 1'b1;
            @(negedge clk);
            req = 1'b0;
            wen = 1'b0;
        end
    endtask

    task disk_write_held;
        input [5:0] register_address;
        input [7:0] value;
        input integer hold_cycles;
        integer held_cycle;
        begin
            @(negedge clk);
            req = 1'b1;
            addr = register_address;
            wdata = value;
            wen = 1'b1;
            for (held_cycle = 0; held_cycle < hold_cycles;
                 held_cycle = held_cycle + 1)
                @(negedge clk);
            req = 1'b0;
            wen = 1'b0;
        end
    endtask

    task disk_read;
        input [5:0] register_address;
        output [7:0] value;
        begin
            @(negedge clk);
            req = 1'b1;
            addr = register_address;
            wdata = 8'd0;
            wen = 1'b0;
            @(negedge clk);
            value = rdata;
            req = 1'b0;
        end
    endtask

    task program_request;
        input [31:0] request_lba;
        input [63:0] request_dma;
        input [7:0] request_count;
        integer byte_index;
        begin
            for (byte_index = 0; byte_index < 4; byte_index = byte_index + 1)
                disk_write(DISK_SECTOR + byte_index,
                           request_lba[(byte_index*8) +: 8]);
            for (byte_index = 0; byte_index < 8; byte_index = byte_index + 1)
                disk_write(DISK_DMA + byte_index,
                           request_dma[(byte_index*8) +: 8]);
            disk_write(DISK_SECN, request_count);
        end
    endtask

    task program_expected_generation;
        input [31:0] generation;
        integer byte_index;
        begin
            for (byte_index = 0; byte_index < 4; byte_index = byte_index + 1)
                disk_write(DISK_EXPECTED_MEDIA_GEN + byte_index,
                           generation[(byte_index*8) +: 8]);
        end
    endtask

    task expect_next_completion;
        integer watchdog;
        begin
            expected_completion = expected_completion + 32'd1;
            watchdog = 0;
            while ((dut.completion !== expected_completion) &&
                   (watchdog < 400000)) begin
                @(posedge clk);
                watchdog = watchdog + 1;
            end
            if (watchdog >= 400000) begin
                $display("  FAIL: command completion timed out (wanted %0d, got %0d)",
                         expected_completion, dut.completion);
                fail_count = fail_count + 1;
            end
            repeat (2) @(posedge clk);
        end
    endtask

    task wait_for_dma_request;
        integer watchdog;
        begin
            watchdog = 0;
            while (!dma_req && watchdog < 200000) begin
                @(posedge clk);
                watchdog = watchdog + 1;
            end
            if (watchdog >= 200000) begin
                $display("  FAIL: DMA request never arrived");
                fail_count = fail_count + 1;
            end
        end
    endtask

    initial begin
        pass_count = 0;
        fail_count = 0;
        dma_ack_count = 0;
        expected_completion = 32'd0;
        rst_n = 1'b0;
        req = 1'b0;
        addr = 6'd0;
        wdata = 8'd0;
        wen = 1'b0;
        dma_stall = 1'b0;
        dma_force_error = 1'b0;
        card_present = 1'b1;
        card_write_protected = 1'b0;
        fault_silence = 1'b0;
        fault_read_crc = 1'b0;
        fault_crc_lba = 32'hFFFF_FFFF;
        fault_write_reject = 1'b0;
        fault_hold_busy = 1'b0;
        fault_flush = 1'b0;
        for (i = 0; i < DMA_BYTES; i = i + 1)
            dma_memory[i] = 8'hCC;

        repeat (5) @(posedge clk);
        rst_n = 1'b1;
        repeat (6) @(posedge clk);

        $display("\nDisk contract register map");
        disk_read(DISK_STATUS, read_value);
        check("STATUS publishes present and media-changed bits",
              read_value[7] && read_value[4] && !read_value[0]);
        disk_read(DISK_TOTAL, read_value);
        check("TOTAL reports the attached medium's exact sector count",
              read_value == TOTAL_SECTORS);
        disk_read(DISK_CAPS, read_value);
        check("CAPS advertises guarded generation submission",
              read_value == 8'h7F && read_value[6]);
        disk_read(DISK_TRANSFERRED, read_value);
        check("TRANSFERRED resets to zero", read_value == 8'd0);
        program_expected_generation(32'h7856_3412);
        disk_read(DISK_EXPECTED_MEDIA_GEN + 0, read_value);
        check("EXPECTED_MEDIA_GEN is byte-addressable little-endian setup",
              read_value == 8'h12 &&
              dut.expected_media_generation == 32'h7856_3412);
        disk_read(DISK_GUARDED_CMD, read_value);
        check("GUARDED_CMD reads as zero", read_value == 8'd0);

        before_count = dut.completion;
        disk_write(DISK_CMD, DISK_CMD_STATUS);
        repeat (3) @(posedge clk);
        check("STATUS command remains a no-op", dut.completion == before_count);
        disk_write(DISK_STATUS, 8'h10);
        disk_read(DISK_STATUS, read_value);
        check("STATUS.MEDIA_CHANGED is write-one-to-clear", !read_value[4]);

        before_generation = dut.media_generation;
        @(negedge clk);
        card_present = 1'b0;
        repeat (2) @(posedge clk);
        // This W1C is accepted on the same edge that the synchronized
        // presence transition is published.
        disk_write(DISK_STATUS, 8'h10);
        repeat (2) @(posedge clk);
        check("media transition set dominates same-cycle STATUS W1C",
              dut.media_changed &&
              dut.media_generation == before_generation + 1);
        @(negedge clk);
        card_present = 1'b1;
        repeat (5) @(posedge clk);
        check("media transitions preserve guarded-generation setup",
              dut.expected_media_generation == 32'h7856_3412);
        disk_write(DISK_STATUS, 8'h10);

        $display("\nHeld-REQ bus handshake");
        before_count = dut.completion;
        disk_write_held(DISK_CMD, 8'h77, 4);
        repeat (2) @(posedge clk);
        check("held command REQ is accepted exactly once",
              dut.completion == before_count + 1 &&
              dut.result == DISK_RES_UNSUPPORTED && !dut.rejected);
        expected_completion = dut.completion;
        before_count = dut.completion;
        disk_write_held(DISK_CMD, DISK_CMD_RESET, 4);
        repeat (2) @(posedge clk);
        check("held idle RESET has one side effect and no completion",
              dut.completion == before_count && !dut.result_valid &&
              dut.sector == 0 && dut.dma_base == 0 && dut.sec_count == 1 &&
              dut.dma_push_ctr == 0);
        disk_write_held(DISK_DMA_PUSH, 8'hAB, 4);
        repeat (2) @(posedge clk);
        check("held DMA_PUSH advances and writes exactly once",
              dut.dma_push_ctr == 1 && dut.dma_base[7:0] == 8'hAB &&
              dut.dma_base[15:8] == 0);
        disk_write(DISK_CMD, DISK_CMD_STATUS);
        disk_write_held(DISK_DMA_PUSH, 8'hCD, 4);
        repeat (2) @(posedge clk);
        check("STATUS command preserves DMA_PUSH sequencing",
              dut.dma_push_ctr == 2 && dut.dma_base[15:0] == 16'hCDAB);
        disk_write(DISK_CMD, DISK_CMD_RESET);

        $display("\nGeneration-guarded command submission");
        program_request(32'd0, 64'h100, 8'd1);
        program_expected_generation(dut.media_generation);
        disk_write(DISK_GUARDED_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("matching guarded READ follows the normal transfer path",
              dut.result == DISK_RES_OK && dut.transferred == 1);

        program_request(32'd0, 64'h800, 8'd1);
        program_expected_generation(dut.media_generation - 32'd1);
        before_count = card_write_count;
        before_dma_count = dma_ack_count;
        read_value = card.media[0];
        disk_write(DISK_GUARDED_CMD, DISK_CMD_WRITE);
        expect_next_completion();
        check("stale guarded WRITE fails as MEDIA_REMOVED with zero progress",
              dut.result == DISK_RES_MEDIA_REMOVED && dut.transferred == 0);
        check("stale guarded WRITE performs no DMA or card/media write",
              dma_ack_count == before_dma_count &&
              card_write_count == before_count && card.media[0] == read_value);

        program_request(32'd1, 64'h400, 8'd1);
        before_count = card_read_count;
        before_dma_count = dma_ack_count;
        read_value = dma_memory[16'h400];
        disk_write(DISK_GUARDED_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("stale guarded READ fails without DMA or SD access",
              dut.result == DISK_RES_MEDIA_REMOVED && dut.transferred == 0 &&
              dma_ack_count == before_dma_count &&
              card_read_count == before_count &&
              dma_memory[16'h400] == read_value);

        before_count = card_flush_count;
        disk_write(DISK_GUARDED_CMD, DISK_CMD_FLUSH);
        expect_next_completion();
        check("stale guarded FLUSH never reaches card status protocol",
              dut.result == DISK_RES_MEDIA_REMOVED && dut.transferred == 0 &&
              card_flush_count == before_count);

        program_expected_generation(dut.media_generation);

        $display("\nPreflight validation");
        program_request(32'd0, 64'h100, 8'd0);
        before_count = card_read_count;
        disk_write(DISK_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("zero-sector request is INVALID_COUNT",
              dut.result == DISK_RES_INVALID_COUNT);
        check("invalid count never reaches SD", card_read_count == before_count);

        program_request(TOTAL_SECTORS, 64'h100, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("one-past-end LBA is rejected", dut.result == DISK_RES_LBA_RANGE);

        program_request(32'd0, 64'hFFFF_FFFF_FFFF_FE01, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("UINT64_MAX-510 plus one sector is ADDRESS_OVERFLOW",
              dut.result == DISK_RES_ADDRESS_OVERFLOW);

        program_request(32'd0, 64'hFFFF_FFFF_FFFF_FE00, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("span ending exactly at UINT64_MAX is nonoverflow but unmapped",
              dut.result == DISK_RES_DMA_INVALID);

        program_request(32'd0, 64'h0000_0000_0000_3F00, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("span straddling two adjacent DMA windows is rejected",
              dut.result == DISK_RES_DMA_INVALID);

        program_request(32'd6, 64'h0000_0000_0000_3E00, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("span whose last byte equals window limit-1 is valid",
              dut.result == DISK_RES_OK && dut.transferred == 1);

        $display("\nReal SD read and exact-end LBA");
        program_request(TOTAL_SECTORS-1, 64'h100, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("exact final sector succeeds", dut.result == DISK_RES_OK);
        check("CMD17 carries the programmed SDHC LBA",
              card_last_argument == TOTAL_SECTORS-1);
        check("one completed sector is published", dut.transferred == 8'd1);
        data_ok = 1'b1;
        for (i = 0; i < 512; i = i + 1)
            if (dma_memory[16'h100+i] !== card.media[((TOTAL_SECTORS-1)*512)+i])
                data_ok = 1'b0;
        check("CMD17 payload and CRC commit all 512 DMA bytes", data_ok);
        before_count = dut.completion;
        repeat (20) @(posedge clk);
        check("successful command increments COMPLETE exactly once",
              dut.completion == before_count);

        $display("\nBusy snapshot and held DMA request");
        dma_stall = 1'b1;
        program_request(32'd1, 64'h400, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        wait_for_dma_request();
        held_addr = dma_addr;
        held_data = dma_wdata;
        held_wen = dma_wen;
        stable_ok = 1'b1;
        repeat (6) begin
            @(posedge clk);
            if (!dma_req || dma_addr != held_addr || dma_wdata != held_data ||
                dma_wen != held_wen)
                stable_ok = 1'b0;
        end
        check("DMA request tuple remains stable under backpressure", stable_ok);
        disk_write(DISK_SECTOR, 8'h05);
        disk_write(DISK_EXPECTED_MEDIA_GEN, 8'hA5);
        disk_write(DISK_GUARDED_CMD, DISK_CMD_WRITE);
        disk_write(DISK_CMD, DISK_CMD_WRITE);
        disk_read(DISK_STATUS, read_value);
        check("setup/command writes while BUSY set REJECTED", read_value[2]);
        check("busy writes cannot alter the active snapshot",
              dut.current_lba == 32'd1 && dut.active_dma_cursor == 64'h400 &&
              dut.expected_media_generation == dut.media_generation);
        disk_write(DISK_STATUS, 8'h04);
        disk_read(DISK_STATUS, read_value);
        check("STATUS.REJECTED is write-one-to-clear while BUSY", !read_value[2]);
        disk_write(DISK_DATA, 8'hA5);
        disk_read(DISK_STATUS, read_value);
        check("entire busy setup range +02..+10, including DATA, is rejected",
              read_value[2]);
        disk_write(DISK_STATUS, 8'h04);
        disk_write(DISK_TOTAL, 8'hA5);
        disk_read(DISK_STATUS, read_value);
        check("read-only result range +11..+1F is a busy no-op", !read_value[2]);
        disk_write(DISK_DATA, 8'h5A);
        dma_stall = 1'b0;
        expect_next_completion();
        check("stalled read resumes and succeeds", dut.result == DISK_RES_OK);
        check("REJECTED survives completion of the active command", dut.rejected);

        $display("\nReal SD write and flush");
        for (i = 0; i < 512; i = i + 1)
            dma_memory[16'h800+i] = (8'hA5 ^ i[7:0]);
        program_request(32'd2, 64'h800, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_WRITE);
        expect_next_completion();
        check("CMD24 data block is accepted", dut.result == DISK_RES_OK);
        check("REJECTED remains sticky across a later accepted command",
              dut.rejected);
        disk_write(DISK_STATUS, 8'h04);
        check("REJECTED clears only through STATUS W1C or RESET", !dut.rejected);
        data_ok = 1'b1;
        for (i = 0; i < 512; i = i + 1)
            if (card.media[(2*512)+i] !== (8'hA5 ^ i[7:0]))
                data_ok = 1'b0;
        check("CMD24 wrote the DMA payload to addressed media", data_ok);

        fault_write_reject = 1'b1;
        program_request(32'd5, 64'h800, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_WRITE);
        expect_next_completion();
        check("write-response failure reports PARTIAL external effect",
              dut.result == (8'h80 | DISK_RES_MEDIA_FAILURE) &&
              dut.transferred == 0);
        fault_write_reject = 1'b0;

        disk_write(DISK_CMD, DISK_CMD_FLUSH);
        expect_next_completion();
        check("FLUSH waits for checked CMD13 status", dut.result == DISK_RES_OK &&
              card_last_command == 6'd13 && card_flush_count == 1);

        $display("\nConfirmed-sector abort boundaries");
        program_request(32'd0, 64'h1800, 8'd2);
        disk_write(DISK_CMD, DISK_CMD_READ);
        boundary_watchdog = 0;
        while (dut.busy && dut.transferred != 1 &&
               boundary_watchdog < 300000) begin
            @(posedge clk);
            boundary_watchdog = boundary_watchdog + 1;
        end
        check("first READ sector is published at its CRC confirmation edge",
              dut.busy && dut.transferred == 1);
        disk_write(DISK_CMD, DISK_CMD_RESET);
        expect_next_completion();
        check("RESET after READ confirmation preserves exact whole-sector count",
              dut.result == (8'h80 | DISK_RES_RESET_ABORTED) &&
              dut.transferred == 1);

        for (i = 0; i < 1024; i = i + 1)
            dma_memory[16'h2000+i] = (8'h3C ^ i[7:0]);
        program_request(32'd0, 64'h2000, 8'd2);
        disk_write(DISK_CMD, DISK_CMD_WRITE);
        boundary_watchdog = 0;
        while (dut.busy && dut.transferred != 1 &&
               boundary_watchdog < 300000) begin
            @(posedge clk);
            boundary_watchdog = boundary_watchdog + 1;
        end
        check("first WRITE sector is published at card-ready confirmation",
              dut.busy && dut.transferred == 1);
        card_present = 1'b0;
        expect_next_completion();
        check("removal after WRITE confirmation preserves exact sector count",
              dut.result == (8'h80 | DISK_RES_MEDIA_REMOVED) &&
              dut.transferred == 1);
        card_present = 1'b1;
        repeat (6) @(posedge clk);

        $display("\nMedia and protocol fault results");
        card_write_protected = 1'b1;
        repeat (4) @(posedge clk);
        before_count = card_write_count;
        program_request(32'd3, 64'h800, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_WRITE);
        expect_next_completion();
        check("write protection is a terminal preflight result",
              dut.result == DISK_RES_WRITE_PROTECTED &&
              card_write_count == before_count);
        card_write_protected = 1'b0;
        repeat (4) @(posedge clk);

        fault_read_crc = 1'b1;
        fault_crc_lba = 32'd0;
        program_request(32'd0, 64'hA00, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("bad first-sector CRC reports PARTIAL after read DMA effects",
              dut.result == (8'h80 | DISK_RES_MEDIA_FAILURE) &&
              dut.transferred == 0);

        fault_crc_lba = 32'd4;
        program_request(32'd3, 64'hC00, 8'd2);
        disk_write(DISK_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("multi-sector failure reports PARTIAL and exact progress",
              dut.result == (8'h80 | DISK_RES_MEDIA_FAILURE) &&
              dut.transferred == 8'd1);
        fault_read_crc = 1'b0;

        dma_stall = 1'b1;
        program_request(32'd0, 64'h1000, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        wait_for_dma_request();
        expect_next_completion();
        check("unacknowledged DMA terminates as DMA_FAILURE and drops request",
              dut.result == (8'h80 | DISK_RES_DMA_FAILURE) && !dma_req &&
              dut.transferred == 0);
        dma_stall = 1'b0;

        dma_force_error = 1'b1;
        program_request(32'd0, 64'h1200, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("DMA responder error becomes DMA_FAILURE",
              dut.result == (8'h80 | DISK_RES_DMA_FAILURE) &&
              dut.transferred == 0);
        dma_force_error = 1'b0;

        fault_flush = 1'b1;
        disk_write(DISK_CMD, DISK_CMD_FLUSH);
        expect_next_completion();
        check("nonzero CMD13 status becomes FLUSH_FAILURE",
              dut.result == DISK_RES_FLUSH_FAILURE);
        fault_flush = 1'b0;

        $display("\nRemoval, no-media, and reset abort");
        dma_stall = 1'b1;
        program_request(32'd0, 64'h1400, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        wait_for_dma_request();
        card_present = 1'b0;
        expect_next_completion();
        check("mid-command detach becomes MEDIA_REMOVED",
              dut.result == (8'h80 | DISK_RES_MEDIA_REMOVED) && !dma_req);
        dma_stall = 1'b0;
        repeat (5) @(posedge clk);
        program_request(32'd0, 64'h1400, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        expect_next_completion();
        check("detached media fails immediately as NO_MEDIA",
              dut.result == DISK_RES_NO_MEDIA);

        card_present = 1'b1;
        repeat (6) @(posedge clk);
        dma_stall = 1'b1;
        program_request(32'd0, 64'h1600, 8'd1);
        disk_write(DISK_CMD, DISK_CMD_READ);
        wait_for_dma_request();
        disk_write(DISK_CMD, DISK_CMD_RESET);
        expect_next_completion();
        check("RESET aborts one active command exactly once",
              dut.result == (8'h80 | DISK_RES_RESET_ABORTED) &&
              !dut.busy && !dma_req);
        check("active RESET preserves sticky MEDIA_CHANGED", dut.media_changed);
        check("active RESET restores the programmable request tuple",
              dut.sector == 0 && dut.dma_base == 0 && dut.sec_count == 1 &&
              dut.dma_push_ctr == 0 && dut.expected_media_generation == 0);
        dma_stall = 1'b0;

        before_count = dut.completion;
        disk_write(DISK_CMD, 8'h77);
        expect_next_completion();
        check("unknown opcode is UNSUPPORTED", dut.result == DISK_RES_UNSUPPORTED);
        check("unsupported command increments COMPLETE once",
              dut.completion == before_count + 1);

        before_count = dut.completion;
        disk_write(DISK_CMD, DISK_CMD_RESET);
        repeat (3) @(posedge clk);
        check("idle RESET clears terminal state without completing a command",
              dut.completion == before_count && !dut.result_valid &&
              dut.result == 0 && dut.transferred == 0 && !dut.rejected);
        check("idle RESET also preserves MEDIA_CHANGED", dut.media_changed);
        check("idle RESET restores the programmable request tuple",
              dut.sector == 0 && dut.dma_base == 0 && dut.sec_count == 1 &&
              dut.dma_push_ctr == 0 && dut.expected_media_generation == 0);

        $display("\nDisk controller: %0d PASSED, %0d FAILED", pass_count, fail_count);
        if (fail_count != 0)
            $fatal(1, "disk controller qualification failed");
        $finish;
    end

    initial begin
        #100000000;
        $fatal(1, "TIMEOUT: disk controller qualification");
    end

endmodule
