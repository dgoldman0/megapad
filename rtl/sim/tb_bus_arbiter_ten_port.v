// ============================================================================
// tb_bus_arbiter_ten_port.v — Checkpoint-3 integrated-topology regression
// ============================================================================
//
// The production SoC has ten physical main-bus ports after reserving the WOTS
// requester slot:
//   0..3 full cores, 4..6 clusters, 7 NIC DMA, 8 disk DMA, 9 WOTS DMA.
//
// This bench keeps the existing nine-port generic regression intact and pins
// the new final-port capture, held-request bubble, response classification,
// and work-conserving transition back to the unchanged disk port.

`timescale 1ns/1ps
`include "mp64_pkg.vh"

module tb_bus_arbiter_ten_port;

    localparam N_PORTS   = 10;
    localparam PORT_BITS = 4;
    localparam WOTS_PORT = 9;
    localparam DISK_PORT = 8;

    reg clk;
    reg rst_n;
    initial clk = 1'b0;
    always #5 clk = ~clk;

    reg  [N_PORTS-1:0]      cpu_valid;
    reg  [N_PORTS*64-1:0]   cpu_addr;
    reg  [N_PORTS*64-1:0]   cpu_wdata;
    reg  [N_PORTS-1:0]      cpu_wen;
    reg  [N_PORTS*2-1:0]    cpu_size;
    reg  [N_PORTS-1:0]      cpu_port_io;
    wire [N_PORTS*64-1:0]   cpu_rdata;
    wire [N_PORTS-1:0]      cpu_ready;
    wire [N_PORTS-1:0]      cpu_accept;
    wire [N_PORTS*2-1:0]    cpu_resp_code;
    wire [N_PORTS-1:0]      bus_err;

    wire        mem_req;
    wire [63:0] mem_addr;
    wire [63:0] mem_wdata;
    wire        mem_wen;
    wire [1:0]  mem_size;
    reg  [63:0] mem_rdata;
    reg         mem_ack;

    wire        mmio_req;
    wire [11:0] mmio_addr;
    wire [63:0] mmio_wdata;
    wire        mmio_wen;
    wire [1:0]  mmio_size;
    wire        mmio_port_io;

    integer pass_count;
    integer fail_count;
    integer port_index;

    mp64_bus #(
        .N_PORTS(N_PORTS),
        .PORT_BITS(PORT_BITS),
        .FIXED_WEIGHT1_MASK(10'b10_0000_0000)
    ) dut (
        .clk(clk),
        .rst_n(rst_n),
        .cpu_valid(cpu_valid),
        .cpu_addr(cpu_addr),
        .cpu_wdata(cpu_wdata),
        .cpu_wen(cpu_wen),
        .cpu_size(cpu_size),
        .cpu_port_io(cpu_port_io),
        .cpu_requester_valid({N_PORTS{1'b0}}),
        .cpu_requester_id({N_PORTS*8{1'b0}}),
        .cpu_rdata(cpu_rdata),
        .cpu_ready(cpu_ready),
        .cpu_accept(cpu_accept),
        .cpu_resp_code(cpu_resp_code),
        .mem_req(mem_req),
        .mem_addr(mem_addr),
        .mem_wdata(mem_wdata),
        .mem_wen(mem_wen),
        .mem_size(mem_size),
        .mem_rdata(mem_rdata),
        .mem_ack(mem_ack),
        .mem_resp_code(BUS_RESP_OK),
        .mmio_req(mmio_req),
        .mmio_addr(mmio_addr),
        .mmio_wdata(mmio_wdata),
        .mmio_wen(mmio_wen),
        .mmio_size(mmio_size),
        .mmio_port_io(mmio_port_io),
        .mmio_requester_valid(),
        .mmio_requester_id(),
        .mmio_rdata(64'd0),
        .mmio_ack(1'b0),
        .qos_csr_wen(1'b0),
        .qos_csr_addr(8'd0),
        .qos_csr_wdata(64'd0),
        .qos_csr_rdata(),
        .bus_err(bus_err)
    );

    task check;
        input [8*80-1:0] label;
        input            condition;
        begin
            if (condition) begin
                pass_count = pass_count + 1;
                $display("  [PASS] %0s", label);
            end else begin
                fail_count = fail_count + 1;
                $display("  [FAIL] %0s", label);
            end
        end
    endtask

    task reset_dut;
        begin
            rst_n       = 1'b0;
            cpu_valid   = {N_PORTS{1'b0}};
            cpu_addr    = {(N_PORTS*64){1'b0}};
            cpu_wdata   = {(N_PORTS*64){1'b0}};
            cpu_wen     = {N_PORTS{1'b0}};
            cpu_size    = {(N_PORTS*2){1'b0}};
            cpu_port_io = {N_PORTS{1'b0}};
            mem_rdata   = 64'd0;
            mem_ack     = 1'b0;
            repeat (3) @(negedge clk);
            rst_n = 1'b1;
            @(negedge clk);
        end
    endtask

    initial begin
        pass_count = 0;
        fail_count = 0;

        // Reset credit still starts at port 0 with every physical peer valid.
        reset_dut;
        for (port_index = 0; port_index < N_PORTS;
             port_index = port_index + 1) begin
            cpu_valid[port_index] = 1'b1;
            cpu_addr[port_index*64 +: 64] =
                64'h0000_0000_0000_1000 + port_index * 64'h100;
        end
        #1;
        check("ten-port reset tie exposes capture ACCEPT for port 0",
              cpu_accept == 10'b00_0000_0001);
        @(posedge clk); #1;
        check("ten-port reset tie captures port 0",
              mem_req && mem_addr == 64'h0000_0000_0000_1000);
        @(negedge clk);
        mem_ack = 1'b1;
        @(posedge clk); #1;
        check("port-0 success carries OK response code",
              cpu_ready[0] && cpu_resp_code[1:0] == BUS_RESP_OK);
        @(negedge clk);
        mem_ack = 1'b0;
        cpu_valid = {N_PORTS{1'b0}};

        // The appended WOTS slot is a normal final round-robin peer.
        reset_dut;
        cpu_valid[WOTS_PORT] = 1'b1;
        cpu_addr[WOTS_PORT*64 +: 64] = 64'h0000_0000_0000_9900;
        #1;
        check("appended WOTS request receives exact capture ACCEPT",
              cpu_accept == 10'b10_0000_0000);
        @(posedge clk); #1;
        check("appended WOTS request reaches memory",
              mem_req && mem_addr == 64'h0000_0000_0000_9900);

        @(negedge clk);
        mem_rdata = 64'h1122_3344_5566_7788;
        mem_ack = 1'b1;
        @(posedge clk); #1;
        check("WOTS terminal response returns only to port 9",
              cpu_ready == 10'b10_0000_0000);
        check("WOTS terminal response is classified OK",
              cpu_resp_code[WOTS_PORT*2 +: 2] == BUS_RESP_OK);
        @(negedge clk);
        mem_ack = 1'b0;

        // A legacy held-valid master receives exactly the existing one bubble.
        @(posedge clk); #1;
        check("held port-9 request receives one post-completion bubble",
              !mem_req);
        @(posedge clk); #1;
        check("held port-9 request regrants after one bubble",
              mem_req && mem_addr == 64'h0000_0000_0000_9900);

        // Complete WOTS again, then prove disk kept index 8 and is discovered
        // immediately rather than being displaced by the appended requester.
        @(negedge clk);
        mem_ack = 1'b1;
        @(posedge clk); #1;
        @(negedge clk);
        mem_ack = 1'b0;
        cpu_valid[WOTS_PORT] = 1'b0;
        cpu_valid[DISK_PORT] = 1'b1;
        cpu_addr[DISK_PORT*64 +: 64] = 64'h0000_0000_0000_8800;
        @(posedge clk); #1;
        check("disk remains work-conserving at unchanged port 8",
              mem_req && mem_addr == 64'h0000_0000_0000_8800);

        $display("");
        $display("=== tb_bus_arbiter_ten_port: %0d passed, %0d failed ===",
                 pass_count, fail_count);
        if (fail_count != 0)
            $fatal(1, "ten-port main-bus topology regression");
        $finish;
    end

    initial begin
        #10000;
        $fatal(1, "tb_bus_arbiter_ten_port timeout");
    end

endmodule
