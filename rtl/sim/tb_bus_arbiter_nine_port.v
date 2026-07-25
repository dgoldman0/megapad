// ============================================================================
// tb_bus_arbiter_nine_port.v — Integrated-topology round-robin regression
// ============================================================================
//
// The default SoC has nine physical main-bus ports:
//   0..3 full cores, 4..6 clusters, 7 NIC DMA, 8 disk DMA.
//
// This bench pins the non-power-of-two wrap cases that a four-port arbiter
// cannot expose.  In particular, it proves that the held-valid safeguard
// bubbles exactly once for ports 7/8 and that a lone port-7 request remains
// work-conserving after last_grant=8.
//

`timescale 1ns/1ps

module tb_bus_arbiter_nine_port;

    localparam N_PORTS   = 9;
    localparam PORT_BITS = 4;
    localparam CLK_HALF  = 5;

    reg clk;
    reg rst_n;

    reg  [N_PORTS-1:0]      cpu_valid;
    reg  [N_PORTS*64-1:0]   cpu_addr;
    reg  [N_PORTS*64-1:0]   cpu_wdata;
    reg  [N_PORTS-1:0]      cpu_wen;
    reg  [N_PORTS*2-1:0]    cpu_size;
    reg  [N_PORTS-1:0]      cpu_port_io;
    wire [N_PORTS*64-1:0]   cpu_rdata;
    wire [N_PORTS-1:0]      cpu_ready;
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
    reg  [63:0] mmio_rdata;
    reg         mmio_ack;

    reg         qos_csr_wen;
    reg  [7:0]  qos_csr_addr;
    reg  [63:0] qos_csr_wdata;
    wire [63:0] qos_csr_rdata;

    integer pass_count;
    integer fail_count;
    integer port_index;

    initial clk = 1'b0;
    always #CLK_HALF clk = ~clk;

    mp64_bus #(
        .N_PORTS   (N_PORTS),
        .PORT_BITS (PORT_BITS)
    ) dut (
        .clk           (clk),
        .rst_n         (rst_n),
        .cpu_valid     (cpu_valid),
        .cpu_addr      (cpu_addr),
        .cpu_wdata     (cpu_wdata),
        .cpu_wen       (cpu_wen),
        .cpu_size      (cpu_size),
        .cpu_port_io   (cpu_port_io),
        .cpu_rdata     (cpu_rdata),
        .cpu_ready     (cpu_ready),
        .mem_req       (mem_req),
        .mem_addr      (mem_addr),
        .mem_wdata     (mem_wdata),
        .mem_wen       (mem_wen),
        .mem_size      (mem_size),
        .mem_rdata     (mem_rdata),
        .mem_ack       (mem_ack),
        .mmio_req      (mmio_req),
        .mmio_addr     (mmio_addr),
        .mmio_wdata    (mmio_wdata),
        .mmio_wen      (mmio_wen),
        .mmio_size     (mmio_size),
        .mmio_port_io  (mmio_port_io),
        .mmio_rdata    (mmio_rdata),
        .mmio_ack      (mmio_ack),
        .qos_csr_wen   (qos_csr_wen),
        .qos_csr_addr  (qos_csr_addr),
        .qos_csr_wdata (qos_csr_wdata),
        .qos_csr_rdata (qos_csr_rdata),
        .bus_err       (bus_err)
    );

    task check;
        input [8*72-1:0] label;
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
            rst_n         = 1'b0;
            cpu_valid     = {N_PORTS{1'b0}};
            cpu_addr      = {(N_PORTS*64){1'b0}};
            cpu_wdata     = {(N_PORTS*64){1'b0}};
            cpu_wen       = {N_PORTS{1'b0}};
            cpu_size      = {(N_PORTS*2){1'b0}};
            cpu_port_io   = {N_PORTS{1'b0}};
            mem_rdata     = 64'd0;
            mem_ack       = 1'b0;
            mmio_rdata    = 64'd0;
            mmio_ack      = 1'b0;
            qos_csr_wen   = 1'b0;
            qos_csr_addr  = 8'd0;
            qos_csr_wdata = 64'd0;
            repeat (3) @(negedge clk);
            rst_n = 1'b1;
            @(negedge clk);
        end
    endtask

    initial begin
        pass_count = 0;
        fail_count = 0;

        // Reset credit: all nine peers ready, port 0 wins the first tie.
        reset_dut;
        for (port_index = 0;
             port_index < N_PORTS;
             port_index = port_index + 1) begin
            cpu_valid[port_index] = 1'b1;
            cpu_addr[port_index*64 +: 64] =
                64'h0000_0000_0000_1000 + port_index * 64'h100;
        end
        @(posedge clk); #1;
        check("reset-time all-port tie grants port 0",
              mem_req && mem_addr == 64'h0000_0000_0000_1000);

        // Complete the tie transaction before resetting for sparse cases.
        @(negedge clk);
        mem_ack = 1'b1;
        @(posedge clk); #1;
        @(negedge clk);
        mem_ack = 1'b0;
        cpu_valid = {N_PORTS{1'b0}};

        // Establish last_grant=8 with disk held valid across acknowledgement.
        reset_dut;
        cpu_valid[8] = 1'b1;
        cpu_addr[8*64 +: 64] = 64'h0000_0000_0000_8800;
        @(posedge clk); #1;
        check("lone disk request reaches port 8",
              mem_req && mem_addr == 64'h0000_0000_0000_8800);

        @(negedge clk);
        mem_ack = 1'b1;
        @(posedge clk); #1;
        check("disk completion pulses ready[8]", cpu_ready[8]);
        @(negedge clk);
        mem_ack = 1'b0;

        // served_last suppresses exactly the first same-port IDLE edge.
        @(posedge clk); #1;
        check("held disk request receives one idle bubble", !mem_req);
        @(posedge clk); #1;
        check("held disk request regrants after one bubble",
              mem_req && mem_addr == 64'h0000_0000_0000_8800);

        // Complete disk again, then leave only NIC port 7 eligible.  The old
        // four-bit candidate arithmetic overflowed before reaching this port.
        @(negedge clk);
        mem_ack = 1'b1;
        @(posedge clk); #1;
        @(negedge clk);
        mem_ack = 1'b0;
        cpu_valid[8] = 1'b0;
        cpu_valid[7] = 1'b1;
        cpu_addr[7*64 +: 64] = 64'h0000_0000_0000_7700;
        @(posedge clk); #1;
        check("lone NIC request is work-conserving after port 8",
              mem_req && mem_addr == 64'h0000_0000_0000_7700);

        // Port 7 must also remain discoverable when it is the held last port.
        @(negedge clk);
        mem_ack = 1'b1;
        @(posedge clk); #1;
        check("NIC completion pulses ready[7]", cpu_ready[7]);
        @(negedge clk);
        mem_ack = 1'b0;
        @(posedge clk); #1;
        check("held NIC request receives one idle bubble", !mem_req);
        @(posedge clk); #1;
        check("held NIC request regrants after one bubble",
              mem_req && mem_addr == 64'h0000_0000_0000_7700);

        $display("");
        $display(
            "=== tb_bus_arbiter_nine_port: %0d passed, %0d failed ===",
            pass_count,
            fail_count
        );
        if (fail_count != 0)
            $fatal(1, "nine-port main-bus arbitration regression");
        $finish;
    end

    initial begin
        #10000;
        $fatal(1, "tb_bus_arbiter_nine_port timeout");
    end

endmodule
