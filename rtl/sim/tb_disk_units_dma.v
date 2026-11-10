// ============================================================================
// tb_disk_units_dma.v — every storage unit's disk on one DMA bus port
// ============================================================================
// Mirrors mp64_soc's storage: one mp64_disk controller and one card for each
// storage unit, the settings and system units' cards write-protected, reach
// Bank 0 through mp64_byte_dma_arbiter and the disk's one bus master port.
// Every disk reads at once and its bytes land where its controller asked,
// with no controller waiting for more than one beat from each of the others.
// The protected disks refuse a write and their cards see none; the user disk
// takes a write and a flush.

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_disk_units_dma;
    // The units in unit order: boot, settings, system, user.
    localparam integer UNITS = 4;
    localparam [UNITS-1:0] PROTECTED = 4'b0110;
    localparam integer USER = UNITS - 1;
    // Sector 0 holds each card's own bytes; the boot card takes each unit's
    // landing area back at sector LANDED_LBA + unit.
    localparam integer LANDED_LBA = 4;
    localparam integer TOTAL_SECTORS = LANDED_LBA + UNITS;
    localparam [63:0] BANK0_LIMIT = 64'h0000_0000_0000_4000;
    localparam [63:0] LANDING_BASE = 64'h400;

    reg clk;
    reg rst_n;
    initial clk = 1'b0;
    always #5 clk = ~clk;

    // One MMIO bus; REQ selects the unit's controller, as the SoC's decode
    // does.
    reg  [UNITS-1:0]   req;
    reg  [5:0]         addr;
    reg  [7:0]         wdata;
    reg                wen;
    wire [UNITS*8-1:0] rdata;
    wire [UNITS-1:0]   ack;

    wire [UNITS-1:0]    d_req;
    wire [UNITS*64-1:0] d_addr;
    wire [UNITS*8-1:0]  d_wdata;
    wire [UNITS-1:0]    d_wen;
    wire [7:0]          d_rdata;
    wire [UNITS-1:0]    d_ack;
    wire [UNITS-1:0]    d_err;

    wire        port_req;
    wire [63:0] port_addr;
    wire [7:0]  port_wdata;
    wire        port_wen;
    wire [7:0]  port_rdata;
    wire        port_ack;
    wire        port_err;

    // Each unit's controller state and its card's counters.
    wire [UNITS*32-1:0] completion;
    wire [UNITS*8-1:0]  result;
    wire [UNITS*8-1:0]  transferred;
    wire [UNITS*32-1:0] card_writes;
    wire [UNITS*32-1:0] card_flushes;

    // Byte INDEX of the sector unit UNIT's card holds at LBA 0.
    function [7:0] unit_byte;
        input integer unit;
        input integer index;
        begin
            unit_byte = index[7:0] ^ (8'h5A + 8'h21 * unit[7:0]);
        end
    endfunction

    genvar u;
    generate
        for (u = 0; u < UNITS; u = u + 1) begin : g_unit
            wire spi_clk, spi_cs_n, spi_mosi, spi_miso;
            wire [5:0]  last_command;
            wire [31:0] last_argument;
            wire integer reads, writes, flushes;

            // A protected unit's card, while present, is write-protected.
            mp64_disk #(
                .TOTAL_SECTORS(TOTAL_SECTORS),
                .DMA_BASE_ADDR(64'd0), .DMA_LIMIT_ADDR(BANK0_LIMIT),
                .SPI_HALF_PERIOD(1), .SPI_INIT_HALF_PERIOD(1),
                .DMA_TIMEOUT_CYCLES(1024),
                .SD_RESPONSE_POLLS(64), .SD_DATA_POLLS(64),
                .SD_BUSY_POLLS(64), .SD_INIT_RETRIES(8)
            ) u_disk (
                .clk(clk), .rst_n(rst_n),
                .req(req[u]), .addr(addr), .wdata(wdata), .wen(wen),
                .rdata(rdata[u*8 +: 8]), .ack(ack[u]),
                .dma_req(d_req[u]), .dma_addr(d_addr[u*64 +: 64]),
                .dma_wdata(d_wdata[u*8 +: 8]), .dma_wen(d_wen[u]),
                .dma_rdata(d_rdata), .dma_ack(d_ack[u]), .dma_err(d_err[u]),
                .card_present(1'b1), .card_write_protected(PROTECTED[u]),
                .spi_clk(spi_clk), .spi_mosi(spi_mosi),
                .spi_miso(spi_miso), .spi_cs_n(spi_cs_n)
            );

            sd_spi_model #(.TOTAL_SECTORS(TOTAL_SECTORS)) u_card (
                .spi_clk(spi_clk), .spi_cs_n(spi_cs_n),
                .spi_mosi(spi_mosi), .spi_miso(spi_miso),
                .fault_silence(1'b0), .fault_read_crc(1'b0),
                .fault_crc_lba(32'hFFFF_FFFF), .fault_write_reject(1'b0),
                .fault_hold_busy(1'b0), .fault_flush(1'b0),
                .last_command(last_command), .last_argument(last_argument),
                .read_count(reads), .write_count(writes),
                .flush_count(flushes)
            );

            assign completion[u*32 +: 32]   = u_disk.completion;
            assign result[u*8 +: 8]         = u_disk.result;
            assign transferred[u*8 +: 8]    = u_disk.transferred;
            assign card_writes[u*32 +: 32]  = writes;
            assign card_flushes[u*32 +: 32] = flushes;

            integer k;
            initial begin
                for (k = 0; k < 512; k = k + 1)
                    u_card.media[k] = unit_byte(u, k);
            end
        end
    endgenerate

    mp64_byte_dma_arbiter #(.MASTERS(UNITS)) u_arbiter (
        .clk(clk), .rst_n(rst_n),
        .m_req(d_req), .m_addr(d_addr), .m_wdata(d_wdata), .m_wen(d_wen),
        .m_rdata(d_rdata), .m_ack(d_ack), .m_err(d_err),
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
    integer unit_index;
    integer all_asking_cycles;
    reg data_ok;
    reg [UNITS*32-1:0] expected_completion;

    // Cycles in which every controller asked for the port at once.
    always @(posedge clk) begin
        if (!rst_n)
            all_asking_cycles <= 0;
        else if (&d_req)
            all_asking_cycles <= all_asking_cycles + 1;
    end

    // For each controller, the beats the port gave the others since it began
    // asking; WORST_WAIT is the longest any controller waited.
    integer waiting [0:UNITS-1];
    integer worst_wait;
    integer w;
    always @(posedge clk) begin
        if (!rst_n) begin
            for (w = 0; w < UNITS; w = w + 1)
                waiting[w] <= 0;
            worst_wait <= 0;
        end else begin
            for (w = 0; w < UNITS; w = w + 1) begin
                if (!d_req[w] || d_ack[w] || d_err[w]) begin
                    waiting[w] <= 0;
                end else if (port_ack || port_err) begin
                    waiting[w] <= waiting[w] + 1;
                    if (waiting[w] + 1 > worst_wait)
                        worst_wait <= waiting[w] + 1;
                end
            end
        end
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
        input integer unit;
        input [5:0] register_address;
        input [7:0] value;
        begin
            @(negedge clk);
            req = {UNITS{1'b0}};
            req[unit] = 1'b1;
            addr = register_address;
            wdata = value;
            wen = 1'b1;
            @(negedge clk);
            req = {UNITS{1'b0}};
            wen = 1'b0;
        end
    endtask

    task program_request;
        input integer unit;
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

    // Give UNIT's controller COMMAND and expect one more completion from it.
    task command;
        input integer unit;
        input [7:0] value;
        begin
            disk_write(unit, DISK_CMD, value);
            expected_completion[unit*32 +: 32] =
                expected_completion[unit*32 +: 32] + 1;
        end
    endtask

    // Give every controller COMMAND in the same cycle, so that all of them
    // reach the port together.
    task command_all;
        input [7:0] value;
        integer unit;
        begin
            @(negedge clk);
            req = {UNITS{1'b1}};
            addr = DISK_CMD;
            wdata = value;
            wen = 1'b1;
            @(negedge clk);
            req = {UNITS{1'b0}};
            wen = 1'b0;
            for (unit = 0; unit < UNITS; unit = unit + 1)
                expected_completion[unit*32 +: 32] =
                    expected_completion[unit*32 +: 32] + 1;
        end
    endtask

    task wait_all;
        integer watchdog;
        begin
            watchdog = 0;
            while (completion != expected_completion && watchdog < 1000000) begin
                @(posedge clk);
                watchdog = watchdog + 1;
            end
            if (watchdog >= 1000000) begin
                $display("  FAIL: storage unit commands timed out");
                fail_count = fail_count + 1;
            end
            repeat (3) @(posedge clk);
        end
    endtask

    initial begin
        pass_count = 0;
        fail_count = 0;
        expected_completion = {(UNITS*32){1'b0}};
        rst_n = 1'b0;
        req = {UNITS{1'b0}}; addr = 6'd0; wdata = 8'd0; wen = 1'b0;
        repeat (5) @(posedge clk);
        rst_n = 1'b1;
        repeat (6) @(posedge clk);

        $display("\nEvery disk reads at once through one port");
        for (unit_index = 0; unit_index < UNITS; unit_index = unit_index + 1)
            program_request(unit_index, 32'd0,
                            LANDING_BASE + unit_index * 64'd512);
        command_all(DISK_CMD_READ);
        wait_all();
        data_ok = 1'b1;
        for (unit_index = 0; unit_index < UNITS; unit_index = unit_index + 1)
            if (result[unit_index*8 +: 8] != {1'b0, DISK_RES_OK}
                || transferred[unit_index*8 +: 8] != 8'd1)
                data_ok = 1'b0;
        check("every disk's read completed", data_ok);
        check("every controller asked for the port at the same time",
              all_asking_cycles > 0);
        check("a controller waited while the port served another",
              worst_wait > 0);
        check("no controller waited for more than one beat from each other",
              worst_wait <= UNITS - 1);
        check("no bus error", bus_err == 2'b00);

        // Write each landing area back to the boot disk's card and compare
        // it with the sector its own card holds.
        for (unit_index = 0; unit_index < UNITS; unit_index = unit_index + 1) begin
            program_request(0, LANDED_LBA + unit_index,
                            LANDING_BASE + unit_index * 64'd512);
            command(0, DISK_CMD_WRITE);
            wait_all();
        end
        data_ok = 1'b1;
        for (unit_index = 0; unit_index < UNITS; unit_index = unit_index + 1)
            for (i = 0; i < 512; i = i + 1)
                if (g_unit[0].u_card.media[((LANDED_LBA + unit_index)*512)+i]
                    !== unit_byte(unit_index, i))
                    data_ok = 1'b0;
        check("each disk's bytes landed where its controller asked", data_ok);

        $display("\nThe settings and system disks refuse writes");
        data_ok = 1'b1;
        for (unit_index = 0; unit_index < UNITS; unit_index = unit_index + 1)
            if (PROTECTED[unit_index]) begin
                program_request(unit_index, 32'd1, LANDING_BASE);
                command(unit_index, DISK_CMD_WRITE);
                wait_all();
                if (result[unit_index*8 +: 8] != {1'b0, DISK_RES_WRITE_PROTECTED}
                    || transferred[unit_index*8 +: 8] != 8'd0
                    || card_writes[unit_index*32 +: 32] != 32'd0)
                    data_ok = 1'b0;
            end
        check("each protected disk's write ends WRITE_PROTECTED, its card unwritten",
              data_ok);
        check("the settings and system units are the protected ones",
              PROTECTED == 4'b0110);

        $display("\nThe user disk takes a write and a flush");
        program_request(USER, 32'd1, LANDING_BASE);
        command(USER, DISK_CMD_WRITE);
        wait_all();
        check("the user disk's write completed",
              result[USER*8 +: 8] == {1'b0, DISK_RES_OK}
              && transferred[USER*8 +: 8] == 8'd1
              && card_writes[USER*32 +: 32] == 32'd1);
        data_ok = 1'b1;
        for (i = 0; i < 512; i = i + 1)
            if (g_unit[USER].u_card.media[512+i] !== unit_byte(0, i))
                data_ok = 1'b0;
        check("the user card holds the sector written", data_ok);
        command(USER, DISK_CMD_FLUSH);
        wait_all();
        check("the user disk's flush completed",
              result[USER*8 +: 8] == {1'b0, DISK_RES_OK}
              && card_flushes[USER*32 +: 32] == 32'd1);

        $display("\nStorage units DMA: %0d PASSED, %0d FAILED",
                 pass_count, fail_count);
        if (fail_count != 0)
            $fatal(1, "storage units DMA qualification failed");
        $finish;
    end

    initial begin
        #400000000;
        $fatal(1, "TIMEOUT: storage units DMA qualification");
    end

endmodule
