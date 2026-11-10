// ============================================================================
// tb_byte_dma_arbiter.v — byte-DMA masters sharing one port
// ============================================================================
// A responder acknowledges each beat after a programmable delay.  The bench
// checks that a lone master is served at once, that a granted beat's tuple
// reaches the port unchanged until it completes, that the grant goes round
// the masters that ask, in order, so a master joining a busy port waits at
// most one beat, that ERR reaches only the granted master, and that a master
// withdrawing its request releases the port.  MASTERS sets how many masters
// share the port; the Makefile runs the bench at several counts.

`timescale 1ns / 1ps

module tb_byte_dma_arbiter;
    parameter integer MASTERS = 4;
    localparam integer LAST = MASTERS - 1;
    localparam integer ROUNDS = 3;

    reg clk;
    reg rst_n;
    initial clk = 1'b0;
    always #5 clk = ~clk;

    reg  [MASTERS-1:0]    m_req;
    reg  [MASTERS*64-1:0] m_addr;
    reg  [MASTERS*8-1:0]  m_wdata;
    reg  [MASTERS-1:0]    m_wen;
    wire [7:0]            m_rdata;
    wire [MASTERS-1:0]    m_ack;
    wire [MASTERS-1:0]    m_err;

    wire        port_req;
    wire [63:0] port_addr;
    wire [7:0]  port_wdata;
    wire        port_wen;
    reg  [7:0]  port_rdata;
    reg         port_ack;
    reg         port_err;

    mp64_byte_dma_arbiter #(.MASTERS(MASTERS)) u_arb (
        .clk(clk), .rst_n(rst_n),
        .m_req(m_req), .m_addr(m_addr), .m_wdata(m_wdata), .m_wen(m_wen),
        .m_rdata(m_rdata), .m_ack(m_ack), .m_err(m_err),
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

    // Count completions per master, record their order, and note any cycle
    // in which more than one master saw a response.
    integer   done [0:MASTERS-1];
    reg [7:0] order [0:255];
    integer   order_n;
    reg       shared_response;
    integer   mon;
    integer   responses;
    always @(posedge clk) begin
        if (!rst_n) begin
            for (mon = 0; mon < MASTERS; mon = mon + 1)
                done[mon] <= 0;
            order_n <= 0;
            shared_response <= 1'b0;
        end else begin
            responses = 0;
            for (mon = 0; mon < MASTERS; mon = mon + 1) begin
                if (m_ack[mon] || m_err[mon])
                    responses = responses + 1;
                if (m_ack[mon]) begin
                    done[mon] <= done[mon] + 1;
                    if (order_n < 256) begin
                        order[order_n] <= mon[7:0];
                        order_n <= order_n + 1;
                    end
                end
            end
            if (responses > 1)
                shared_response <= 1'b1;
        end
    end

    // Each master holds its tuple until ACK or ERR, then asks again while it
    // has beats left, as mp64_disk does.
    integer left [0:MASTERS-1];
    integer drv;
    always @(posedge clk) begin
        if (rst_n) begin
            for (drv = 0; drv < MASTERS; drv = drv + 1) begin
                if (m_req[drv] && (m_ack[drv] || m_err[drv])) begin
                    left[drv] <= left[drv] - 1;
                    m_addr[drv*64 +: 64] <= m_addr[drv*64 +: 64] + 1;
                    if (left[drv] == 1) m_req[drv] <= 1'b0;
                end
            end
        end
    end

    // Start master M on BEATS beats from its own address range.  The bench
    // changes requests on the falling edge, so the arbiter samples settled
    // values on the rising one, as it would from a controller's registers.
    task start(input integer m, input integer beats);
    begin
        left[m] = beats;
        m_addr[m*64 +: 64] = 64'h100 * (m + 1);
        m_req[m] = 1'b1;
    end
    endtask

    integer i;
    integer base;
    integer done_before;
    reg     in_order;

    initial begin
        pass_count = 0;
        fail_count = 0;
        delay = 3;
        fail_next = 1'b0;
        m_req = {MASTERS{1'b0}};
        for (i = 0; i < MASTERS; i = i + 1) begin
            m_addr[i*64 +: 64] = 64'h100 * (i + 1);
            m_wdata[i*8 +: 8] = 8'hA0 + i;
            m_wen[i] = (i % 2 == 0);
            left[i] = 0;
        end
        rst_n = 1'b0;
        port_ack = 1'b0; port_err = 1'b0; port_rdata = 8'd0;
        repeat (3) @(posedge clk);
        rst_n = 1'b1;
        @(negedge clk);

        // 1. A lone master is served at once, its tuple unchanged on the port.
        start(LAST, 2);
        #1;
        check("a lone last master reaches the port at once",
              port_req === 1'b1 && port_addr == 64'h100 * MASTERS
              && port_wdata == 8'hA0 + LAST && port_wen === m_wen[LAST]);
        wait (m_req[LAST] === 1'b0);
        @(negedge clk);
        check("both of the lone master's beats completed", done[LAST] == 2);
        check("no tuple changed under a held beat", tuple_changed === 1'b0);
        check("the master read the port's data", m_rdata == 8'h01);

        // 2. All ask: the grant goes round the masters in order, master 0
        //    first, so each waits for one beat from each of the others.
        base = order_n;
        for (i = 0; i < MASTERS; i = i + 1)
            start(i, ROUNDS);
        wait (m_req === {MASTERS{1'b0}});
        @(negedge clk);
        in_order = (order_n - base == ROUNDS * MASTERS);
        for (i = 0; i < ROUNDS * MASTERS; i = i + 1)
            if (order[base + i] != (i % MASTERS)) in_order = 1'b0;
        check("with all asking, the grant went round the masters in order",
              in_order);
        check("still no tuple changed under a held beat", tuple_changed === 1'b0);

        // 3. Only the first and the last master ask: the grant alternates
        //    between them and passes over the idle ones.
        base = order_n;
        start(0, ROUNDS);
        start(LAST, ROUNDS);
        wait (m_req === {MASTERS{1'b0}});
        @(negedge clk);
        in_order = (order_n - base == 2 * ROUNDS);
        for (i = 0; i < 2 * ROUNDS; i = i + 1)
            if (order[base + i] != ((i % 2 == 0) ? 0 : LAST)) in_order = 1'b0;
        check("with two asking, the grant alternated between them", in_order);

        // 4. A master that joins a busy port waits at most one more beat.
        start(LAST, 8);
        repeat (9) @(negedge clk);
        done_before = done[LAST];
        start(0, 1);
        @(posedge clk);
        while (!m_ack[0]) @(posedge clk);
        check("the joining master waited for at most one beat",
              done[LAST] - done_before <= 1);
        wait (m_req === {MASTERS{1'b0}});
        @(negedge clk);
        check("the busy master still completed every beat",
              done[LAST] == 2 + ROUNDS + ROUNDS + 8);

        // 5. ERR reaches only the granted master.
        fail_next = 1'b1;
        start(0, 1);
        @(posedge m_err[0]);
        #1;
        check("ERR reached master 0 and no other master",
              m_err == {{(MASTERS-1){1'b0}}, 1'b1} && m_ack == {MASTERS{1'b0}});
        @(posedge clk);
        @(negedge clk);

        // 6. A master that withdraws its request releases the port.
        delay = 50;
        start(0, 5);
        repeat (4) @(negedge clk);
        m_req[0] = 1'b0;
        start(LAST, 1);
        repeat (2) @(posedge clk);
        #1;
        check("after master 0 withdrew, the last master holds the port",
              port_req === 1'b1 && port_addr == m_addr[LAST*64 +: 64]);
        delay = 0;
        wait (m_req[LAST] === 1'b0);
        @(negedge clk);
        check("no response reached two masters at once",
              shared_response === 1'b0);
        check("no tuple changed under any held beat", tuple_changed === 1'b0);

        $display("tb_byte_dma_arbiter (%0d masters): %0d passed, %0d failed",
                 MASTERS, pass_count, fail_count);
        if (fail_count != 0) $fatal(1, "tb_byte_dma_arbiter failed");
        $finish;
    end

    initial begin
        #10000000;
        $fatal(1, "TIMEOUT: tb_byte_dma_arbiter");
    end
endmodule
