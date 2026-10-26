// ============================================================================
// tb_disk_pair_dma.v — the system and settings disks on one DMA bus port
// ============================================================================
// Mirrors mp64_soc's storage: two mp64_disk controllers, the second with its
// card write-protected, reach Bank 0 through mp64_byte_dma_arbiter and the
// disk's one bus master port.  Both read at once and their bytes land where
// each asked; the settings disk refuses a write and its card sees none.

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_disk_pair_dma;
    localparam integer TOTAL_SECTORS = 4;
    localparam [63:0] BANK0_LIMIT = 64'h0000_0000_0000_4000;

    reg clk;
    reg rst_n;
    initial clk = 1'b0;
    always #5 clk = ~clk;

    // MMIO request lines, one set per controller.
    reg        req0, req1;
    reg [5:0]  addr0, addr1;
    reg [7:0]  wdata0, wdata1;
    reg        wen0, wen1;
    wire [7:0] rdata0, rdata1;
    wire       ack0, ack1;

    wire        d0_req, d1_req;
    wire [63:0] d0_addr, d1_addr;
    wire [7:0]  d0_wdata, d1_wdata;
    wire        d0_wen, d1_wen;
    wire [7:0]  d0_rdata, d1_rdata;
    wire        d0_ack, d1_ack;
    wire        d0_err, d1_err;

    wire        port_req;
    wire [63:0] port_addr;
    wire [7:0]  port_wdata;
    wire        port_wen;
    wire [7:0]  port_rdata;
    wire        port_ack;
    wire        port_err;

    wire spi0_clk, spi0_cs_n, spi0_mosi, spi0_miso;
    wire spi1_clk, spi1_cs_n, spi1_mosi, spi1_miso;

    wire [5:0]  card0_last_command, card1_last_command;
    wire [31:0] card0_last_argument, card1_last_argument;
    wire integer card0_reads, card0_writes, card0_flushes;
    wire integer card1_reads, card1_writes, card1_flushes;

    mp64_disk #(
        .TOTAL_SECTORS(TOTAL_SECTORS),
        .DMA_BASE_ADDR(64'd0), .DMA_LIMIT_ADDR(BANK0_LIMIT),
        .SPI_HALF_PERIOD(1), .SPI_INIT_HALF_PERIOD(1),
        .DMA_TIMEOUT_CYCLES(1024),
        .SD_RESPONSE_POLLS(64), .SD_DATA_POLLS(64),
        .SD_BUSY_POLLS(64), .SD_INIT_RETRIES(8)
    ) u_disk0 (
        .clk(clk), .rst_n(rst_n),
        .req(req0), .addr(addr0), .wdata(wdata0), .wen(wen0),
        .rdata(rdata0), .ack(ack0),
        .dma_req(d0_req), .dma_addr(d0_addr), .dma_wdata(d0_wdata),
        .dma_wen(d0_wen), .dma_rdata(d0_rdata), .dma_ack(d0_ack),
        .dma_err(d0_err),
        .card_present(1'b1), .card_write_protected(1'b0),
        .spi_clk(spi0_clk), .spi_mosi(spi0_mosi),
        .spi_miso(spi0_miso), .spi_cs_n(spi0_cs_n)
    );

    // The settings disk: its card, while present, is write-protected.
    mp64_disk #(
        .TOTAL_SECTORS(TOTAL_SECTORS),
        .DMA_BASE_ADDR(64'd0), .DMA_LIMIT_ADDR(BANK0_LIMIT),
        .SPI_HALF_PERIOD(1), .SPI_INIT_HALF_PERIOD(1),
        .DMA_TIMEOUT_CYCLES(1024),
        .SD_RESPONSE_POLLS(64), .SD_DATA_POLLS(64),
        .SD_BUSY_POLLS(64), .SD_INIT_RETRIES(8)
    ) u_disk1 (
        .clk(clk), .rst_n(rst_n),
        .req(req1), .addr(addr1), .wdata(wdata1), .wen(wen1),
        .rdata(rdata1), .ack(ack1),
        .dma_req(d1_req), .dma_addr(d1_addr), .dma_wdata(d1_wdata),
        .dma_wen(d1_wen), .dma_rdata(d1_rdata), .dma_ack(d1_ack),
        .dma_err(d1_err),
        .card_present(1'b1), .card_write_protected(1'b1),
        .spi_clk(spi1_clk), .spi_mosi(spi1_mosi),
        .spi_miso(spi1_miso), .spi_cs_n(spi1_cs_n)
    );

    sd_spi_model #(.TOTAL_SECTORS(TOTAL_SECTORS)) u_card0 (
        .spi_clk(spi0_clk), .spi_cs_n(spi0_cs_n),
        .spi_mosi(spi0_mosi), .spi_miso(spi0_miso),
        .fault_silence(1'b0), .fault_read_crc(1'b0),
        .fault_crc_lba(32'hFFFF_FFFF), .fault_write_reject(1'b0),
        .fault_hold_busy(1'b0), .fault_flush(1'b0),
        .last_command(card0_last_command), .last_argument(card0_last_argument),
        .read_count(card0_reads), .write_count(card0_writes),
        .flush_count(card0_flushes)
    );

    sd_spi_model #(.TOTAL_SECTORS(TOTAL_SECTORS)) u_card1 (
        .spi_clk(spi1_clk), .spi_cs_n(spi1_cs_n),
        .spi_mosi(spi1_mosi), .spi_miso(spi1_miso),
        .fault_silence(1'b0), .fault_read_crc(1'b0),
        .fault_crc_lba(32'hFFFF_FFFF), .fault_write_reject(1'b0),
        .fault_hold_busy(1'b0), .fault_flush(1'b0),
        .last_command(card1_last_command), .last_argument(card1_last_argument),
        .read_count(card1_reads), .write_count(card1_writes),
        .flush_count(card1_flushes)
    );

    mp64_byte_dma_arbiter u_arbiter (
        .clk(clk), .rst_n(rst_n),
        .m0_req(d0_req), .m0_addr(d0_addr), .m0_wdata(d0_wdata),
        .m0_wen(d0_wen), .m0_rdata(d0_rdata), .m0_ack(d0_ack), .m0_err(d0_err),
        .m1_req(d1_req), .m1_addr(d1_addr), .m1_wdata(d1_wdata),
        .m1_wen(d1_wen), .m1_rdata(d1_rdata), .m1_ack(d1_ack), .m1_err(d1_err),
        .port_req(port_req), .port_addr(port_addr), .port_wdata(port_wdata),
        .port_wen(port_wen), .port_rdata(port_rdata),
        .port_ack(port_ack), .port_err(port_err)
    );

    // Port 0 is idle; port 1 is the disk master port, packed as mp64_soc
    // packs DISK_BUS_PORT.
    wire [1:0] bus_valid = {port_req, 1'b0};
    wire [127:0] bus_addr = {port_addr, 64'd0};
    wire [127:0] bus_wdata = {{56'd0, port_wdata}, 64'd0};
    wire [1:0] bus_wen = {port_wen, 1'b0};
    wire [3:0] bus_size = {BUS_BYTE, BUS_DWORD};
    wire [1:0] bus_port_io = 2'b00;
    wire [127:0] bus_rdata;
    wire [1:0] bus_ready;
    wire [1:0] bus_err;
    wire [63:0] disk_bus_rdata = bus_rdata[127:64];

    assign port_rdata = disk_bus_rdata[port_addr[2:0]*8 +: 8];
    assign port_ack = bus_ready[1];
    assign port_err = bus_err[1];

    wire        mem_req;
    wire [63:0] mem_addr;
    wire [63:0] mem_wdata;
    wire        mem_wen;
    wire [1:0]  mem_size;
    wire [63:0] mem_rdata;
    wire        mem_ack;
    wire        mmio_req;
    wire [11:0] mmio_addr;
    wire [63:0] mmio_wdata;
    wire        mmio_wen;
    wire [1:0]  mmio_size;
    wire        mmio_port_io;

    mp64_bus #(.N_PORTS(2), .PORT_BITS(1)) u_bus (
        .clk(clk), .rst_n(rst_n),
        .cpu_valid(bus_valid), .cpu_addr(bus_addr),
        .cpu_wdata(bus_wdata), .cpu_wen(bus_wen),
        .cpu_size(bus_size), .cpu_port_io(bus_port_io),
        .cpu_rdata(bus_rdata), .cpu_ready(bus_ready),
        .mem_req(mem_req), .mem_addr(mem_addr), .mem_wdata(mem_wdata),
        .mem_wen(mem_wen), .mem_size(mem_size),
        .mem_rdata(mem_rdata), .mem_ack(mem_ack),
        .mem_resp_code(BUS_RESP_OK),
        .mmio_req(mmio_req), .mmio_addr(mmio_addr),
        .mmio_wdata(mmio_wdata), .mmio_wen(mmio_wen),
        .mmio_size(mmio_size), .mmio_port_io(mmio_port_io),
        .mmio_rdata(64'd0), .mmio_ack(1'b0),
        .qos_csr_wen(1'b0), .qos_csr_addr(8'd0),
        .qos_csr_wdata(64'd0), .qos_csr_rdata(), .bus_err(bus_err)
    );

    mp64_memory #(
        .BANK_DEPTH(256), .ADDR_W_TILE(8), .ADDR_W_CPU(11)
    ) u_memory (
        .clk(clk), .rst_n(rst_n),
        .cpu_req(mem_req), .cpu_addr(mem_addr),
        .cpu_wdata(mem_wdata), .cpu_wen(mem_wen), .cpu_size(mem_size),
        .cpu_rdata(mem_rdata), .cpu_ack(mem_ack),
        .tile_req(1'b0), .tile_addr(32'd0), .tile_wen(1'b0),
        .tile_wdata(512'd0), .tile_accept(), .tile_rdata(), .tile_ack(),
        .tile_error(), .tile_fault_addr(),
        .ext_req(), .ext_addr(), .ext_wdata(),
        .ext_wen(), .ext_size(),
        .ext_rdata(64'd0), .ext_ack(1'b0)
    );

    integer pass_count;
    integer fail_count;
    integer i;
    integer overlap_cycles;
    reg data_ok;
    reg [31:0] completion0;
    reg [31:0] completion1;

    // Cycles in which both controllers asked for the port at once.
    always @(posedge clk) begin
        if (!rst_n)
            overlap_cycles <= 0;
        else if (d0_req && d1_req)
            overlap_cycles <= overlap_cycles + 1;
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
        input unit;
        input [5:0] register_address;
        input [7:0] value;
        begin
            @(negedge clk);
            if (unit) begin
                req1 = 1'b1; addr1 = register_address;
                wdata1 = value; wen1 = 1'b1;
            end else begin
                req0 = 1'b1; addr0 = register_address;
                wdata0 = value; wen0 = 1'b1;
            end
            @(negedge clk);
            req0 = 1'b0; wen0 = 1'b0;
            req1 = 1'b0; wen1 = 1'b0;
        end
    endtask

    task program_request;
        input unit;
        input [31:0] request_lba;
        input [63:0] request_dma;
        integer byte_index;
        begin
            for (byte_index = 0; byte_index < 4; byte_index = byte_index + 1)
                disk_write(unit, DISK_SECTOR + byte_index,
                           request_lba[(byte_index*8) +: 8]);
            for (byte_index = 0; byte_index < 8; byte_index = byte_index + 1)
                disk_write(unit, DISK_DMA + byte_index,
                           request_dma[(byte_index*8) +: 8]);
            disk_write(unit, DISK_SECN, 8'd1);
        end
    endtask

    task wait_both;
        integer watchdog;
        begin
            watchdog = 0;
            while ((u_disk0.completion != completion0 ||
                    u_disk1.completion != completion1) &&
                   watchdog < 500000) begin
                @(posedge clk);
                watchdog = watchdog + 1;
            end
            if (watchdog >= 500000) begin
                $display("  FAIL: disk pair commands timed out");
                fail_count = fail_count + 1;
            end
            repeat (3) @(posedge clk);
        end
    endtask

    initial begin
        pass_count = 0;
        fail_count = 0;
        completion0 = 0;
        completion1 = 0;
        rst_n = 1'b0;
        req0 = 1'b0; addr0 = 6'd0; wdata0 = 8'd0; wen0 = 1'b0;
        req1 = 1'b0; addr1 = 6'd0; wdata1 = 8'd0; wen1 = 1'b0;
        // Each card's sector 0 holds bytes of its own.
        for (i = 0; i < 512; i = i + 1) begin
            u_card0.media[i] = (i % 256) ^ 8'h5A;
            u_card1.media[i] = (i % 256) ^ 8'hA5;
        end
        repeat (5) @(posedge clk);
        rst_n = 1'b1;
        repeat (6) @(posedge clk);

        $display("\nBoth disks read at once through one port");
        program_request(1'b0, 32'd0, 64'h100);
        program_request(1'b1, 32'd0, 64'h400);
        disk_write(1'b0, DISK_CMD, DISK_CMD_READ);
        disk_write(1'b1, DISK_CMD, DISK_CMD_READ);
        completion0 = completion0 + 1;
        completion1 = completion1 + 1;
        wait_both();
        check("the system disk's read completed",
              u_disk0.result == DISK_RES_OK && u_disk0.transferred == 1);
        check("the settings disk's read completed",
              u_disk1.result == DISK_RES_OK && u_disk1.transferred == 1);
        check("both controllers asked for the port at the same time",
              overlap_cycles > 0);
        check("no bus error", bus_err == 2'b00);

        // Write each landing area back to the system disk's card and compare:
        // 0x100 holds card 0's sector, 0x400 card 1's.
        program_request(1'b0, 32'd2, 64'h100);
        disk_write(1'b0, DISK_CMD, DISK_CMD_WRITE);
        completion0 = completion0 + 1;
        wait_both();
        program_request(1'b0, 32'd3, 64'h400);
        disk_write(1'b0, DISK_CMD, DISK_CMD_WRITE);
        completion0 = completion0 + 1;
        wait_both();
        data_ok = 1'b1;
        for (i = 0; i < 512; i = i + 1) begin
            if (u_card0.media[(2*512)+i] !== u_card0.media[i])
                data_ok = 1'b0;
            if (u_card0.media[(3*512)+i] !== u_card1.media[i])
                data_ok = 1'b0;
        end
        check("each disk's bytes landed where it asked", data_ok);

        $display("\nThe settings disk refuses writes");
        program_request(1'b1, 32'd1, 64'h100);
        disk_write(1'b1, DISK_CMD, DISK_CMD_WRITE);
        completion1 = completion1 + 1;
        wait_both();
        check("the settings disk's write ends WRITE_PROTECTED",
              u_disk1.result == DISK_RES_WRITE_PROTECTED
              && u_disk1.transferred == 0);
        check("the settings card received no write", card1_writes == 0);

        $display("\nDisk pair DMA: %0d PASSED, %0d FAILED",
                 pass_count, fail_count);
        if (fail_count != 0)
            $fatal(1, "disk pair DMA qualification failed");
        $finish;
    end

    initial begin
        #100000000;
        $fatal(1, "TIMEOUT: disk pair DMA qualification");
    end

endmodule
