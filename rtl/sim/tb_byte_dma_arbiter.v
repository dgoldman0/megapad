// ============================================================================
// tb_byte_dma_arbiter.v — two byte-DMA masters sharing one port
// ============================================================================
// A responder acknowledges each beat after a programmable delay.  The bench
// checks that a granted beat's tuple reaches the port unchanged until it
// completes, that the grant alternates while both masters ask, that a lone
// master is served at once, that ERR reaches only the granted master, and
// that a master withdrawing its request releases the port.

`timescale 1ns / 1ps

module tb_byte_dma_arbiter;
    reg clk;
    reg rst_n;
    initial clk = 1'b0;
    always #5 clk = ~clk;

    reg        m0_req, m1_req;
    reg [63:0] m0_addr, m1_addr;
    reg [7:0]  m0_wdata, m1_wdata;
    reg        m0_wen, m1_wen;
    wire [7:0] m0_rdata, m1_rdata;
    wire       m0_ack, m0_err, m1_ack, m1_err;

    wire        port_req;
    wire [63:0] port_addr;
    wire [7:0]  port_wdata;
    wire        port_wen;
    reg  [7:0]  port_rdata;
    reg         port_ack;
    reg         port_err;

    mp64_byte_dma_arbiter u_arb (
        .clk(clk), .rst_n(rst_n),
        .m0_req(m0_req), .m0_addr(m0_addr), .m0_wdata(m0_wdata),
        .m0_wen(m0_wen), .m0_rdata(m0_rdata), .m0_ack(m0_ack), .m0_err(m0_err),
        .m1_req(m1_req), .m1_addr(m1_addr), .m1_wdata(m1_wdata),
        .m1_wen(m1_wen), .m1_rdata(m1_rdata), .m1_ack(m1_ack), .m1_err(m1_err),
        .port_req(port_req), .port_addr(port_addr), .port_wdata(port_wdata),
        .port_wen(port_wen), .port_rdata(port_rdata),
        .port_ack(port_ack), .port_err(port_err)
    );

    integer pass_count;
    integer fail_count;

    task check(input [511:0] label, input cond);
    begin
        if (cond) begin
            pass_count = pass_count + 1;
        end else begin
            $display("  FAIL: %0s", label);
            fail_count = fail_count + 1;
        end
    end
    endtask

    // Responder: answer the beat on the port after DELAY cycles, with the
    // low byte of its address as read data; FAIL_NEXT answers it with ERR.
    integer delay;
    integer waited;
    reg     fail_next;
    // The tuple the port first presented for the beat in progress.
    reg        seen;
    reg [63:0] seen_addr;
    reg [7:0]  seen_wdata;
    reg        seen_wen;
    reg        tuple_changed;

    always @(posedge clk) begin
        port_ack <= 1'b0;
        port_err <= 1'b0;
        if (!rst_n) begin
            waited <= 0;
            seen <= 1'b0;
            tuple_changed <= 1'b0;
        end else if (port_req && !port_ack && !port_err) begin
            if (!seen) begin
                seen <= 1'b1;
                seen_addr <= port_addr;
                seen_wdata <= port_wdata;
                seen_wen <= port_wen;
            end else if (port_addr != seen_addr || port_wdata != seen_wdata
                         || port_wen != seen_wen) begin
                tuple_changed <= 1'b1;
            end
            if (waited >= delay) begin
                waited <= 0;
                seen <= 1'b0;
                port_rdata <= port_addr[7:0];
                if (fail_next) begin
                    port_err <= 1'b1;
                    fail_next <= 1'b0;
                end else begin
                    port_ack <= 1'b1;
                end
            end else begin
                waited <= waited + 1;
            end
        end else if (!port_req) begin
            waited <= 0;
            seen <= 1'b0;
        end
    end

    // Count completions per master and record their order.
    integer m0_done, m1_done;
    reg [7:0] order [0:15];
    integer order_n;
    always @(posedge clk) begin
        if (!rst_n) begin
            m0_done <= 0;
            m1_done <= 0;
            order_n <= 0;
        end else begin
            if (m0_ack) begin
                m0_done <= m0_done + 1;
                if (order_n < 16) begin
                    order[order_n] <= 8'd0;
                    order_n <= order_n + 1;
                end
            end
            if (m1_ack) begin
                m1_done <= m1_done + 1;
                if (order_n < 16) begin
                    order[order_n] <= 8'd1;
                    order_n <= order_n + 1;
                end
            end
        end
    end

    // Each master holds its tuple until ACK or ERR, then asks again while it
    // has beats left, as mp64_disk does.
    integer m0_left, m1_left;
    always @(posedge clk) begin
        if (rst_n) begin
            if (m0_req && (m0_ack || m0_err)) begin
                m0_left <= m0_left - 1;
                m0_addr <= m0_addr + 1;
                if (m0_left == 1) m0_req <= 1'b0;
            end
            if (m1_req && (m1_ack || m1_err)) begin
                m1_left <= m1_left - 1;
                m1_addr <= m1_addr + 1;
                if (m1_left == 1) m1_req <= 1'b0;
            end
        end
    end

    integer i;
    integer alternations;

    initial begin
        pass_count = 0;
        fail_count = 0;
        delay = 3;
        fail_next = 1'b0;
        m0_req = 1'b0; m1_req = 1'b0;
        m0_addr = 64'h100; m1_addr = 64'h200;
        m0_wdata = 8'hA0; m1_wdata = 8'hB1;
        m0_wen = 1'b1; m1_wen = 1'b0;
        m0_left = 0; m1_left = 0;
        rst_n = 1'b0;
        port_ack = 1'b0; port_err = 1'b0; port_rdata = 8'd0;
        repeat (3) @(posedge clk);
        rst_n = 1'b1;
        @(posedge clk);

        // 1. A lone master is served at once, its tuple unchanged on the port.
        m1_left = 2;
        m1_req = 1'b1;
        #1;
        check("a lone master 1 reaches the port at once",
              port_req === 1'b1 && port_addr == 64'h200 && port_wen === 1'b0);
        wait (m1_req === 1'b0);
        @(posedge clk);
        check("both of master 1's beats completed", m1_done == 2);
        check("no tuple changed under a held beat", tuple_changed === 1'b0);
        check("master 1 read the port's data", m1_rdata == 8'h01);

        // 2. Both ask: beats alternate, master 0 first.
        order_n = 0;
        m0_left = 4; m1_left = 4;
        m0_addr = 64'h100; m1_addr = 64'h200;
        m0_req = 1'b1; m1_req = 1'b1;
        wait (m0_req === 1'b0 && m1_req === 1'b0);
        @(posedge clk);
        check("all eight beats completed", m0_done == 4 && m1_done == 6);
        alternations = 0;
        for (i = 1; i < 8; i = i + 1)
            if (order[i] != order[i - 1]) alternations = alternations + 1;
        check("master 0 went first", order[0] == 8'd0);
        check("the grant alternated on every beat", alternations == 7);
        check("still no tuple changed under a held beat", tuple_changed === 1'b0);

        // 3. ERR reaches only the granted master.
        fail_next = 1'b1;
        m0_left = 1;
        m0_req = 1'b1;
        @(posedge m0_err);
        #1;
        check("ERR reached master 0 and not master 1",
              m0_err === 1'b1 && m1_err === 1'b0 && m1_ack === 1'b0);
        @(posedge clk);

        // 4. A master that withdraws its request releases the port.
        delay = 50;
        m0_left = 5;
        m0_req = 1'b1;
        repeat (4) @(posedge clk);
        m0_req = 1'b0;
        m1_left = 1;
        m1_req = 1'b1;
        repeat (2) @(posedge clk);
        #1;
        check("after master 0 withdrew, master 1 holds the port",
              port_req === 1'b1 && port_addr == m1_addr);
        delay = 0;
        wait (m1_req === 1'b0);
        @(posedge clk);

        $display("tb_byte_dma_arbiter: %0d passed, %0d failed", pass_count, fail_count);
        if (fail_count != 0) $fatal(1, "tb_byte_dma_arbiter failed");
        $finish;
    end
endmodule
