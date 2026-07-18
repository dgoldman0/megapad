// ============================================================================
// tb_disk_bus_dma.v — disk byte DMA through mp64_bus and mp64_memory
// ============================================================================
// Mirrors the dedicated disk master port used by mp64_soc.  The test performs
// unaligned SD round trips through both Bank 0 and the size-aware external PHY
// bridge, then verifies that a bus timeout is surfaced as DMA_FAILURE.

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_disk_bus_dma;
    localparam integer TOTAL_SECTORS = 4;
    localparam [63:0] BANK0_LIMIT = 64'h0000_0000_0000_4000;
    localparam [63:0] EXT_BASE = 64'h0000_0000_0010_0000;
    localparam [63:0] EXT_LIMIT = 64'h0000_0000_0010_1000;

    reg clk;
    reg rst_n;
    initial clk = 1'b0;
    always #5 clk = ~clk;

    reg        disk_req;
    reg [5:0]  disk_addr;
    reg [7:0]  disk_wdata;
    reg        disk_wen;
    wire [7:0] disk_rdata;
    wire       disk_ack;

    wire        disk_dma_req;
    wire [63:0] disk_dma_addr;
    wire [7:0]  disk_dma_wdata;
    wire        disk_dma_wen;
    wire [7:0]  disk_dma_rdata;
    wire        disk_dma_ack;
    wire        disk_dma_err;

    wire spi_clk;
    wire spi_cs_n;
    wire spi_mosi;
    wire spi_miso;

    wire [5:0] card_last_command;
    wire [31:0] card_last_argument;
    wire integer card_read_count;
    wire integer card_write_count;
    wire integer card_flush_count;

    mp64_disk #(
        .TOTAL_SECTORS(TOTAL_SECTORS),
        .DMA_BASE_ADDR(64'd0),
        .DMA_LIMIT_ADDR(BANK0_LIMIT),
        .DMA1_BASE_ADDR(EXT_BASE),
        .DMA1_LIMIT_ADDR(EXT_LIMIT),
        .SPI_HALF_PERIOD(1),
        .SPI_INIT_HALF_PERIOD(1),
        .DMA_TIMEOUT_CYCLES(1024),
        .SD_RESPONSE_POLLS(64),
        .SD_DATA_POLLS(64),
        .SD_BUSY_POLLS(64),
        .SD_INIT_RETRIES(8)
    ) u_disk (
        .clk(clk), .rst_n(rst_n),
        .req(disk_req), .addr(disk_addr),
        .wdata(disk_wdata), .wen(disk_wen),
        .rdata(disk_rdata), .ack(disk_ack),
        .dma_req(disk_dma_req), .dma_addr(disk_dma_addr),
        .dma_wdata(disk_dma_wdata), .dma_wen(disk_dma_wen),
        .dma_rdata(disk_dma_rdata), .dma_ack(disk_dma_ack),
        .dma_err(disk_dma_err),
        .card_present(1'b1), .card_write_protected(1'b0),
        .spi_clk(spi_clk), .spi_mosi(spi_mosi),
        .spi_miso(spi_miso), .spi_cs_n(spi_cs_n)
    );

    sd_spi_model #(.TOTAL_SECTORS(TOTAL_SECTORS)) u_card (
        .spi_clk(spi_clk), .spi_cs_n(spi_cs_n),
        .spi_mosi(spi_mosi), .spi_miso(spi_miso),
        .fault_silence(1'b0), .fault_read_crc(1'b0),
        .fault_crc_lba(32'hFFFF_FFFF), .fault_write_reject(1'b0),
        .fault_hold_busy(1'b0), .fault_flush(1'b0),
        .last_command(card_last_command), .last_argument(card_last_argument),
        .read_count(card_read_count), .write_count(card_write_count),
        .flush_count(card_flush_count)
    );

    // Port 0 is idle in this focused harness; port 1 is the same dedicated
    // byte-wide master packing used by mp64_soc's DISK_BUS_PORT.
    wire [1:0] bus_valid = {disk_dma_req, 1'b0};
    wire [127:0] bus_addr = {disk_dma_addr, 64'd0};
    wire [127:0] bus_wdata = {{56'd0, disk_dma_wdata}, 64'd0};
    wire [1:0] bus_wen = {disk_dma_wen, 1'b0};
    wire [3:0] bus_size = {BUS_BYTE, BUS_DWORD};
    wire [1:0] bus_port_io = 2'b00;
    wire [127:0] bus_rdata;
    wire [1:0] bus_ready;
    wire [1:0] bus_err;
    wire [63:0] disk_bus_rdata = bus_rdata[127:64];

    assign disk_dma_rdata = disk_bus_rdata[disk_dma_addr[2:0]*8 +: 8];
    assign disk_dma_ack = bus_ready[1];
    assign disk_dma_err = bus_err[1];

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
        .mmio_req(mmio_req), .mmio_addr(mmio_addr),
        .mmio_wdata(mmio_wdata), .mmio_wen(mmio_wen),
        .mmio_size(mmio_size), .mmio_port_io(mmio_port_io),
        .mmio_rdata(64'd0), .mmio_ack(1'b0),
        .qos_csr_wen(1'b0), .qos_csr_addr(8'd0),
        .qos_csr_wdata(64'd0), .qos_csr_rdata(), .bus_err(bus_err)
    );

    wire        ext_req;
    wire [63:0] ext_addr;
    wire [63:0] ext_wdata;
    wire        ext_wen;
    wire [1:0]  ext_size;
    wire [63:0] ext_rdata;
    wire        ext_ack;

    mp64_memory #(
        .BANK_DEPTH(256), .ADDR_W_TILE(8), .ADDR_W_CPU(11)
    ) u_memory (
        .clk(clk), .rst_n(rst_n),
        .cpu_req(mem_req), .cpu_addr(mem_addr),
        .cpu_wdata(mem_wdata), .cpu_wen(mem_wen), .cpu_size(mem_size),
        .cpu_rdata(mem_rdata), .cpu_ack(mem_ack),
        .tile_req(1'b0), .tile_addr(32'd0), .tile_wen(1'b0),
        .tile_wdata(512'd0), .tile_rdata(), .tile_ack(),
        .ext_req(ext_req), .ext_addr(ext_addr), .ext_wdata(ext_wdata),
        .ext_wen(ext_wen), .ext_size(ext_size),
        .ext_rdata(ext_rdata), .ext_ack(ext_ack)
    );

    wire        ext_phy_req;
    wire [31:0] ext_phy_addr;
    wire [63:0] ext_phy_wdata;
    wire        ext_phy_wen;
    wire [63:0] ext_phy_rdata;
    wire        ext_phy_ack;
    wire [3:0]  ext_phy_burst_len;
    reg         phy_stall;
    reg [63:0]  phy_words [0:511];
    wire        ext_phy_in_range = (ext_phy_addr >= EXT_BASE[31:0]) &&
                                    (ext_phy_addr < EXT_LIMIT[31:0]);
    wire [8:0]  ext_phy_word_index =
        (ext_phy_addr - EXT_BASE[31:0]) >> 3;

    assign ext_phy_rdata = ext_phy_in_range ?
                           phy_words[ext_phy_word_index] : 64'd0;
    assign ext_phy_ack = ext_phy_req && ext_phy_in_range && !phy_stall;

    always @(posedge clk) begin
        if (ext_phy_req && ext_phy_ack && ext_phy_wen)
            phy_words[ext_phy_word_index] <= ext_phy_wdata;
    end

    mp64_extmem u_extmem (
        .clk(clk), .rst_n(rst_n),
        .cpu_req(ext_req), .cpu_addr(ext_addr[31:0]),
        .cpu_wdata(ext_wdata), .cpu_wen(ext_wen), .cpu_size(ext_size),
        .cpu_rdata(ext_rdata), .cpu_ack(ext_ack),
        .tile_req(1'b0), .tile_addr(32'd0), .tile_wdata(512'd0),
        .tile_wen(1'b0), .tile_rdata(), .tile_ack(),
        .phy_req(ext_phy_req), .phy_addr(ext_phy_addr),
        .phy_wdata(ext_phy_wdata), .phy_wen(ext_phy_wen),
        .phy_rdata(ext_phy_rdata), .phy_ack(ext_phy_ack),
        .phy_burst_len(ext_phy_burst_len)
    );

    integer pass_count;
    integer fail_count;
    integer dma_ack_count;
    integer i;
    reg [31:0] expected_completion;
    reg data_ok;

    always @(posedge clk) begin
        if (disk_dma_req && disk_dma_ack)
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
            disk_req = 1'b1;
            disk_addr = register_address;
            disk_wdata = value;
            disk_wen = 1'b1;
            @(negedge clk);
            disk_req = 1'b0;
            disk_wen = 1'b0;
        end
    endtask

    task program_request;
        input [31:0] request_lba;
        input [63:0] request_dma;
        integer byte_index;
        begin
            for (byte_index = 0; byte_index < 4; byte_index = byte_index + 1)
                disk_write(DISK_SECTOR + byte_index,
                           request_lba[(byte_index*8) +: 8]);
            for (byte_index = 0; byte_index < 8; byte_index = byte_index + 1)
                disk_write(DISK_DMA + byte_index,
                           request_dma[(byte_index*8) +: 8]);
            disk_write(DISK_SECN, 8'd1);
        end
    endtask

    task wait_completion;
        integer watchdog;
        begin
            expected_completion = expected_completion + 1;
            watchdog = 0;
            while (u_disk.completion != expected_completion &&
                   watchdog < 500000) begin
                @(posedge clk);
                watchdog = watchdog + 1;
            end
            if (watchdog >= 500000) begin
                $display("  FAIL: disk/bus command timed out");
                fail_count = fail_count + 1;
            end
            repeat (3) @(posedge clk);
        end
    endtask

    initial begin
        pass_count = 0;
        fail_count = 0;
        dma_ack_count = 0;
        expected_completion = 0;
        rst_n = 1'b0;
        disk_req = 1'b0;
        disk_addr = 6'd0;
        disk_wdata = 8'd0;
        disk_wen = 1'b0;
        phy_stall = 1'b0;
        for (i = 0; i < 512; i = i + 1)
            phy_words[i] = 64'hD6D6_D6D6_D6D6_D6D6;
        repeat (5) @(posedge clk);
        rst_n = 1'b1;
        repeat (6) @(posedge clk);

        $display("\nDisk DMA through bus and Bank 0");
        program_request(32'd1, 64'h105);
        disk_write(DISK_CMD, DISK_CMD_READ);
        wait_completion();
        check("unaligned CMD17 completed through bus RMW writes",
              u_disk.result == DISK_RES_OK && u_disk.transferred == 1);
        check("disk master generated exactly 512 acknowledged write beats",
              dma_ack_count == 512);

        program_request(32'd2, 64'h105);
        disk_write(DISK_CMD, DISK_CMD_WRITE);
        wait_completion();
        check("CMD24 read the same unaligned bytes back through bus lanes",
              u_disk.result == DISK_RES_OK && u_disk.transferred == 1);
        data_ok = 1'b1;
        for (i = 0; i < 512; i = i + 1)
            if (u_card.media[(2*512)+i] !== u_card.media[(1*512)+i])
                data_ok = 1'b0;
        check("Bank 0 round-trip preserved all 512 bytes", data_ok);
        check("read plus write consumed 1024 acknowledged DMA beats",
              dma_ack_count == 1024);
        check("successful Bank 0 path raised no bus error", bus_err == 2'b00);

        $display("\nDisk DMA through external PHY RMW path");
        program_request(32'd0, EXT_BASE + 64'd3);
        disk_write(DISK_CMD, DISK_CMD_READ);
        wait_completion();
        check("external unaligned CMD17 completed through byte RMW",
              u_disk.result == DISK_RES_OK && u_disk.transferred == 1);
        check("external RMW preserved bytes before DMA span",
              phy_words[0][23:0] == 24'hD6D6D6);
        check("external RMW preserved bytes after DMA span",
              phy_words[64][63:24] == 40'hD6D6D6D6D6);

        program_request(32'd3, EXT_BASE + 64'd3);
        disk_write(DISK_CMD, DISK_CMD_WRITE);
        wait_completion();
        data_ok = 1'b1;
        for (i = 0; i < 512; i = i + 1)
            if (u_card.media[(3*512)+i] !== u_card.media[i])
                data_ok = 1'b0;
        check("external PHY round-trip preserved all sector bytes", data_ok);
        check("Bank 0 and external paths produced 2048 DMA acknowledgements",
              dma_ack_count == 2048);

        $display("\nDisk DMA bus failure propagation");
        phy_stall = 1'b1;
        program_request(32'd0, EXT_BASE + 64'h800);
        disk_write(DISK_CMD, DISK_CMD_READ);
        wait_completion();
        check("bus timeout is returned to controller as DMA_FAILURE",
              u_disk.result == (8'h80 | DISK_RES_DMA_FAILURE) &&
              u_disk.transferred == 0 && !disk_dma_req);
        check("external request reached the real memory forwarding path",
              ext_addr == EXT_BASE + 64'h800);
        repeat (10) @(posedge clk);
        check("timed-out bus request cancels memory and PHY wait states",
              !ext_req && !ext_phy_req);

        phy_stall = 1'b0;
        program_request(32'd1, EXT_BASE + 64'hC03);
        disk_write(DISK_CMD, DISK_CMD_READ);
        wait_completion();
        check("external DMA succeeds after timeout cancellation recovery",
              u_disk.result == DISK_RES_OK && u_disk.transferred == 1);

        $display("\nDisk bus DMA: %0d PASSED, %0d FAILED",
                 pass_count, fail_count);
        if (fail_count != 0)
            $fatal(1, "disk bus DMA qualification failed");
        $finish;
    end

    initial begin
        #100000000;
        $fatal(1, "TIMEOUT: disk bus DMA qualification");
    end

endmodule
