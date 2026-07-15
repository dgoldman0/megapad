// ============================================================================
// tb_nic_bus_dma.v — NIC byte-DMA integration through bus and system RAM
// ============================================================================
// Exercises the same byte-master packing used by mp64_soc: RX writes through
// mp64_bus into mp64_memory, then TX reads byte lanes back through that path.

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_nic_bus_dma;
    reg clk, rst_n;
    initial clk = 1'b0;
    always #5 clk = ~clk;

    reg        nic_req;
    reg [6:0]  nic_addr;
    reg [7:0]  nic_wdata;
    reg        nic_wen;
    wire [7:0] nic_rdata;
    wire       nic_ack;
    wire       nic_irq;

    wire        nic_dma_req;
    wire [63:0] nic_dma_addr;
    wire [7:0]  nic_dma_wdata;
    wire        nic_dma_wen;
    wire [7:0]  nic_dma_rdata;
    wire        nic_dma_ack;

    wire       phy_tx_valid;
    wire [7:0] phy_tx_data;
    reg        phy_tx_ready;
    reg        phy_rx_valid;
    reg [7:0]  phy_rx_data;
    wire       phy_rx_ready;

    mp64_nic u_nic (
        .clk(clk), .rst_n(rst_n),
        .req(nic_req), .addr(nic_addr), .wdata(nic_wdata), .wen(nic_wen),
        .rdata(nic_rdata), .ack(nic_ack), .irq(nic_irq),
        .dma_req(nic_dma_req), .dma_addr(nic_dma_addr),
        .dma_wdata(nic_dma_wdata), .dma_wen(nic_dma_wen),
        .dma_rdata(nic_dma_rdata), .dma_ack(nic_dma_ack),
        .phy_tx_valid(phy_tx_valid), .phy_tx_data(phy_tx_data),
        .phy_tx_ready(phy_tx_ready),
        .phy_rx_valid(phy_rx_valid), .phy_rx_data(phy_rx_data),
        .phy_rx_ready(phy_rx_ready), .phy_link_up(1'b1)
    );

    // Port 0 is idle; port 1 is the NIC DMA master.
    wire [1:0]   bus_valid = {nic_dma_req, 1'b0};
    wire [127:0] bus_addr = {nic_dma_addr, 64'd0};
    wire [127:0] bus_wdata = {{56'd0, nic_dma_wdata}, 64'd0};
    wire [1:0]   bus_wen = {nic_dma_wen, 1'b0};
    wire [3:0]   bus_size = {BUS_BYTE, BUS_DWORD};
    wire [1:0]   bus_port_io = 2'b00;
    wire [127:0] bus_rdata;
    wire [1:0]   bus_ready;
    wire [63:0]  nic_bus_rdata = bus_rdata[127:64];

    assign nic_dma_rdata = nic_bus_rdata[nic_dma_addr[2:0]*8 +: 8];
    assign nic_dma_ack = bus_ready[1];

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
    wire [1:0]  bus_err;

    mp64_bus #(.N_PORTS(2), .PORT_BITS(1)) u_bus (
        .clk(clk), .rst_n(rst_n),
        .cpu_valid(bus_valid), .cpu_addr(bus_addr), .cpu_wdata(bus_wdata),
        .cpu_wen(bus_wen), .cpu_size(bus_size), .cpu_port_io(bus_port_io),
        .cpu_rdata(bus_rdata), .cpu_ready(bus_ready),
        .mem_req(mem_req), .mem_addr(mem_addr), .mem_wdata(mem_wdata),
        .mem_wen(mem_wen), .mem_size(mem_size),
        .mem_rdata(mem_rdata), .mem_ack(mem_ack),
        .mmio_req(mmio_req), .mmio_addr(mmio_addr), .mmio_wdata(mmio_wdata),
        .mmio_wen(mmio_wen), .mmio_size(mmio_size),
        .mmio_port_io(mmio_port_io), .mmio_rdata(64'd0),
        .mmio_ack(mmio_req),
        .qos_csr_wen(1'b0), .qos_csr_addr(8'd0), .qos_csr_wdata(64'd0),
        .qos_csr_rdata(), .bus_err(bus_err)
    );

    mp64_memory #(.BANK_DEPTH(256), .ADDR_W_TILE(8), .ADDR_W_CPU(11)) u_mem (
        .clk(clk), .rst_n(rst_n),
        .cpu_req(mem_req), .cpu_addr(mem_addr), .cpu_wdata(mem_wdata),
        .cpu_wen(mem_wen), .cpu_size(mem_size),
        .cpu_rdata(mem_rdata), .cpu_ack(mem_ack),
        .tile_req(1'b0), .tile_addr(32'd0), .tile_wen(1'b0),
        .tile_wdata(512'd0), .tile_rdata(), .tile_ack(),
        .ext_req(), .ext_addr(), .ext_wdata(), .ext_wen(), .ext_size(),
        .ext_rdata(64'd0), .ext_ack(1'b0)
    );

    task nic_write(input [6:0] a, input [7:0] d);
    begin
        @(negedge clk);
        nic_req = 1'b1; nic_addr = a; nic_wdata = d; nic_wen = 1'b1;
        @(negedge clk);
        nic_req = 1'b0; nic_wen = 1'b0;
    end
    endtask

    integer pass_count, fail_count, dma_ack_count, tx_count;
    reg [7:0] tx_capture [0:3];

    task check(input [511:0] label, input condition);
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

    always @(posedge clk) begin
        if (nic_dma_req && nic_dma_ack)
            dma_ack_count <= dma_ack_count + 1;
        if (phy_tx_valid && phy_tx_ready && tx_count < 4) begin
            tx_capture[tx_count] <= phy_tx_data;
            tx_count <= tx_count + 1;
        end
    end

    initial begin
        pass_count = 0; fail_count = 0; dma_ack_count = 0; tx_count = 0;
        rst_n = 1'b0;
        nic_req = 1'b0; nic_addr = 7'd0; nic_wdata = 8'd0; nic_wen = 1'b0;
        phy_tx_ready = 1'b1; phy_rx_valid = 1'b0; phy_rx_data = 8'd0;
        repeat (4) @(posedge clk);
        rst_n = 1'b1;
        repeat (3) @(posedge clk);

        // RX DMA base = 0x100.
        nic_write(7'h02, 8'h00);
        nic_write(7'h03, 8'h01);
        @(negedge clk); phy_rx_valid = 1'b1; phy_rx_data = 8'hA1;
        @(negedge clk); phy_rx_data = 8'hB2;
        @(negedge clk); phy_rx_data = 8'hC3;
        @(negedge clk); phy_rx_data = 8'hD4;
        @(negedge clk); phy_rx_valid = 1'b0;
        repeat (3) @(posedge clk);
        nic_write(7'h00, 8'h02);
        wait (u_nic.status[4] === 1'b1);
        wait (u_nic.status[4] === 1'b0);
        repeat (2) @(posedge clk);

        check("RX byte 0 reached system RAM",
              u_mem.g_bank[0].u_sram.mem[4][7:0] === 8'hA1);
        check("RX byte 1 reached system RAM",
              u_mem.g_bank[0].u_sram.mem[4][15:8] === 8'hB2);
        check("RX byte 2 reached system RAM",
              u_mem.g_bank[0].u_sram.mem[4][23:16] === 8'hC3);
        check("RX byte 3 reached system RAM",
              u_mem.g_bank[0].u_sram.mem[4][31:24] === 8'hD4);
        check("RX length published after RAM commit", u_nic.frame_len == 16'd4);

        // Preload bytes at 0x108 and read them back through TX DMA.
        u_mem.g_bank[0].u_sram.mem[4][95:64] = 32'h44332211;
        nic_write(7'h02, 8'h08);
        nic_write(7'h03, 8'h01);
        nic_write(7'h0A, 8'h04);
        nic_write(7'h0B, 8'h00);
        nic_write(7'h00, 8'h01);
        wait (tx_count == 4);
        repeat (3) @(posedge clk);
        check("TX byte 0 came from addressed RAM lane", tx_capture[0] === 8'h11);
        check("TX byte 1 came from addressed RAM lane", tx_capture[1] === 8'h22);
        check("TX byte 2 came from addressed RAM lane", tx_capture[2] === 8'h33);
        check("TX byte 3 came from addressed RAM lane", tx_capture[3] === 8'h44);
        check("all eight DMA beats were acknowledged", dma_ack_count == 8);
        check("DMA bus reported no timeout", bus_err == 2'b00);

        $display("NIC bus DMA: %0d PASSED, %0d FAILED", pass_count, fail_count);
        $finish;
    end

    initial begin
        #500000;
        $display("TIMEOUT: NIC bus DMA integration test");
        $finish;
    end
endmodule
