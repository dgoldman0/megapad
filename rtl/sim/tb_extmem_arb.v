// ============================================================================
// tb_extmem_arb.v — Testbench for mp64_extmem_arb in front of mp64_extmem
// ============================================================================
//
// Covers: a lone client keeps the controller's own latency; weighted
// round-robin order under contention; the per-epoch bandwidth limit; data
// reaching one memory from every client; a sub-word write staying one access
// while another client waits; the error sideband reaching only its client;
// withdrawal of a granted request; and a held request running exactly once.
//

`timescale 1ns/1ps

// ----------------------------------------------------------------------------
// Behavioural PHY: one word per request, fixed latency, optional error address
// ----------------------------------------------------------------------------
module tb_arb_phy #(
    parameter LAT = 3
)(
    input  wire        clk,
    input  wire        rst_n,
    input  wire        phy_req,
    input  wire [31:0] phy_addr,
    input  wire [63:0] phy_wdata,
    input  wire        phy_wen,
    output wire        phy_ready,
    output reg  [63:0] phy_rdata,
    output reg         phy_ack,
    output reg         phy_error,
    input  wire        phy_cancel,
    output wire        phy_cancel_done,
    input  wire        err_enable,
    input  wire [31:0] err_addr
);
    reg [63:0] mem [0:1023];
    reg        pend;
    reg [7:0]  cnt;
    reg [31:0] p_addr;
    reg [63:0] p_wdata;
    reg        p_wen;

    // Launch log: bit 64 = write, low bits = address.
    reg [32:0] launch_log [0:255];
    integer    launch_n;

    integer i;
    initial begin
        for (i = 0; i < 1024; i = i + 1)
            mem[i] = 64'd0;
        launch_n = 0;
    end

    assign phy_ready       = 1'b1;
    assign phy_cancel_done = phy_cancel;

    always @(posedge clk) begin
        phy_ack   <= 1'b0;
        phy_error <= 1'b0;
        if (!rst_n || phy_cancel) begin
            pend <= 1'b0;
        end else if (phy_req && phy_ready && !pend) begin
            pend    <= 1'b1;
            cnt     <= LAT[7:0];
            p_addr  <= phy_addr;
            p_wdata <= phy_wdata;
            p_wen   <= phy_wen;
            if (launch_n < 256) begin
                launch_log[launch_n] = {phy_wen, phy_addr};
                launch_n = launch_n + 1;
            end
        end else if (pend) begin
            if (cnt == 8'd0) begin
                pend    <= 1'b0;
                phy_ack <= 1'b1;
                if (err_enable && p_addr == err_addr)
                    phy_error <= 1'b1;
                else if (p_wen)
                    mem[p_addr[12:3]] <= p_wdata;
                else
                    phy_rdata <= mem[p_addr[12:3]];
            end else begin
                cnt <= cnt - 8'd1;
            end
        end
    end
endmodule

// ----------------------------------------------------------------------------
// Behavioural client: runs `count` accesses back to back when `run` is high
// ----------------------------------------------------------------------------
module tb_arb_client (
    input  wire        clk,
    input  wire        rst_n,
    input  wire        run,          // level: issue accesses while count > 0
    input  wire [31:0] base_addr,
    input  wire [63:0] base_wdata,
    input  wire        wen,
    input  wire [1:0]  size,
    input  wire [7:0]  count,
    input  wire [7:0]  hold_after,   // cycles to keep REQ high after ACK
    input  wire        addr_step,    // 1: advance the address by 8 per access
    output reg         req,
    output reg  [63:0] addr,
    output reg  [63:0] wdata,
    output wire        c_wen,
    output wire [1:0]  c_size,
    input  wire        ack,
    input  wire        error,
    input  wire [63:0] rdata,
    output reg  [7:0]  done_count,
    output reg  [7:0]  error_count,
    output reg  [63:0] last_rdata,
    output reg  [31:0] last_latency
);
    assign c_wen  = wen;
    assign c_size = size;

    reg [7:0]  hold;
    reg        gapped;
    reg [31:0] lat;

    always @(posedge clk) begin
        if (!rst_n) begin
            req <= 1'b0; addr <= 64'd0; wdata <= 64'd0;
            done_count <= 8'd0; error_count <= 8'd0;
            last_rdata <= 64'd0; last_latency <= 32'd0;
            hold <= 8'd0; gapped <= 1'b1; lat <= 32'd0;
        end else begin
            if (req && !ack && hold == 8'd0)
                lat <= lat + 32'd1;
            if (!run) begin
                req <= 1'b0; gapped <= 1'b1; hold <= 8'd0;
                done_count <= 8'd0; error_count <= 8'd0;
            end else if (req && ack) begin
                done_count   <= done_count + 8'd1;
                if (error)
                    error_count <= error_count + 8'd1;
                last_rdata   <= rdata;
                last_latency <= lat;
                if (hold_after == 8'd0) begin
                    // REQ returns low for one cycle, then the next access
                    // is raised at once, as a client with queued work does.
                    req <= 1'b0; gapped <= 1'b1;
                end else begin
                    hold <= hold_after;
                end
            end else if (req && hold != 8'd0) begin
                hold <= hold - 8'd1;
                if (hold == 8'd1) begin
                    req <= 1'b0; gapped <= 1'b0;
                end
            end else if (!req && !gapped) begin
                gapped <= 1'b1;        // REQ stays low for one full cycle
            end else if (!req && gapped && done_count < count) begin
                req   <= 1'b1;
                lat   <= 32'd0;
                addr  <= {32'd0, base_addr} +
                         (addr_step ? {53'd0, done_count, 3'b000} : 64'd0);
                wdata <= base_wdata + {56'd0, done_count};
            end
        end
    end
endmodule

module tb_extmem_arb;
    localparam CLK_HALF = 5;
    localparam N = 2;

    reg clk, rst_n;
    initial clk = 1'b0;
    always #CLK_HALF clk = ~clk;

    integer pass_count, fail_count, test_num;

    task check;
        input [8*96-1:0] label;
        input            condition;
        begin
            test_num = test_num + 1;
            if (condition) begin
                pass_count = pass_count + 1;
                $display("  [PASS %0d] %0s", test_num, label);
            end else begin
                fail_count = fail_count + 1;
                $display("  [FAIL %0d] %0s", test_num, label);
            end
        end
    endtask

    // ------------------------------------------------------------------
    // Clients
    // ------------------------------------------------------------------
    reg         run0, run1;
    reg  [31:0] base0, base1;
    reg  [63:0] wd0, wd1;
    reg         wen0, wen1;
    reg  [1:0]  size0, size1;
    reg  [7:0]  count0, count1;
    reg  [7:0]  hold0, hold1;
    reg         step0, step1;

    wire [N-1:0]    c_req;
    wire [N*64-1:0] c_addr, c_wdata;
    wire [N-1:0]    c_wen;
    wire [N*2-1:0]  c_size;
    wire [N-1:0]    c_ack, c_error;
    wire [63:0]     c_rdata;
    reg  [N*8-1:0]  c_weight;
    reg  [N*16-1:0] c_bwlimit;

    wire [7:0]  done0, done1, err0, err1;
    wire [63:0] rd0, rd1;
    wire [31:0] lat0, lat1;

    tb_arb_client u_c0 (
        .clk(clk), .rst_n(rst_n), .run(run0), .base_addr(base0),
        .base_wdata(wd0), .wen(wen0), .size(size0), .count(count0),
        .hold_after(hold0), .addr_step(step0),
        .req(c_req[0]), .addr(c_addr[63:0]), .wdata(c_wdata[63:0]),
        .c_wen(c_wen[0]), .c_size(c_size[1:0]),
        .ack(c_ack[0]), .error(c_error[0]), .rdata(c_rdata),
        .done_count(done0), .error_count(err0), .last_rdata(rd0),
        .last_latency(lat0)
    );
    tb_arb_client u_c1 (
        .clk(clk), .rst_n(rst_n), .run(run1), .base_addr(base1),
        .base_wdata(wd1), .wen(wen1), .size(size1), .count(count1),
        .hold_after(hold1), .addr_step(step1),
        .req(c_req[1]), .addr(c_addr[127:64]), .wdata(c_wdata[127:64]),
        .c_wen(c_wen[1]), .c_size(c_size[3:2]),
        .ack(c_ack[1]), .error(c_error[1]), .rdata(c_rdata),
        .done_count(done1), .error_count(err1), .last_rdata(rd1),
        .last_latency(lat1)
    );

    // ------------------------------------------------------------------
    // DUT: arbiter, controller, PHY
    // ------------------------------------------------------------------
    wire        ext_req, ext_wen, ext_ack, ext_error;
    wire [63:0] ext_addr, ext_wdata, ext_rdata;
    wire [1:0]  ext_size;

    mp64_extmem_arb #(.N_CLIENTS(N), .CLIENT_BITS(1)) dut (
        .clk(clk), .rst_n(rst_n),
        .c_req(c_req), .c_addr(c_addr), .c_wdata(c_wdata), .c_wen(c_wen),
        .c_size(c_size), .c_ack(c_ack), .c_error(c_error), .c_rdata(c_rdata),
        .c_weight(c_weight), .c_bwlimit(c_bwlimit),
        .ext_req(ext_req), .ext_addr(ext_addr), .ext_wdata(ext_wdata),
        .ext_wen(ext_wen), .ext_size(ext_size), .ext_rdata(ext_rdata),
        .ext_ack(ext_ack), .ext_error(ext_error)
    );

    wire        phy_req, phy_wen, phy_ready, phy_ack, phy_error;
    wire        phy_cancel, phy_cancel_done;
    wire [31:0] phy_addr;
    wire [63:0] phy_wdata, phy_rdata;
    wire [3:0]  phy_burst_len;
    reg         err_enable;
    reg  [31:0] err_addr;

    mp64_extmem u_ext (
        .clk(clk), .rst_n(rst_n),
        .cpu_req(ext_req), .cpu_addr(ext_addr[31:0]), .cpu_wdata(ext_wdata),
        .cpu_wen(ext_wen), .cpu_size(ext_size), .cpu_rdata(ext_rdata),
        .cpu_ack(ext_ack), .cpu_error(ext_error),
        .tile_req(1'b0), .tile_addr(32'd0), .tile_wdata(512'd0),
        .tile_wen(1'b0), .tile_cancel(1'b0), .tile_accept(), .tile_rdata(),
        .tile_ack(), .tile_error(), .tile_fault_addr(), .tile_word_done(),
        .phy_req(phy_req), .phy_addr(phy_addr), .phy_wdata(phy_wdata),
        .phy_wen(phy_wen), .phy_ready(phy_ready), .phy_rdata(phy_rdata),
        .phy_ack(phy_ack), .phy_error(phy_error), .phy_cancel(phy_cancel),
        .phy_cancel_done(phy_cancel_done), .phy_burst_len(phy_burst_len)
    );

    tb_arb_phy #(.LAT(3)) u_phy (
        .clk(clk), .rst_n(rst_n), .phy_req(phy_req), .phy_addr(phy_addr),
        .phy_wdata(phy_wdata), .phy_wen(phy_wen), .phy_ready(phy_ready),
        .phy_rdata(phy_rdata), .phy_ack(phy_ack), .phy_error(phy_error),
        .phy_cancel(phy_cancel), .phy_cancel_done(phy_cancel_done),
        .err_enable(err_enable), .err_addr(err_addr)
    );

    // ------------------------------------------------------------------
    // Reference: the same controller and PHY with one client wired directly
    // ------------------------------------------------------------------
    reg         ref_run;
    wire        ref_req, ref_wen, ref_ack, ref_error;
    wire [63:0] ref_addr, ref_wdata, ref_rdata;
    wire [1:0]  ref_size;
    wire [7:0]  ref_done, ref_err;
    wire [63:0] ref_rd;
    wire [31:0] ref_lat;

    tb_arb_client u_ref_client (
        .clk(clk), .rst_n(rst_n), .run(ref_run), .base_addr(base0),
        .base_wdata(wd0), .wen(wen0), .size(size0), .count(count0),
        .hold_after(hold0), .addr_step(step0),
        .req(ref_req), .addr(ref_addr), .wdata(ref_wdata),
        .c_wen(ref_wen), .c_size(ref_size),
        .ack(ref_ack), .error(ref_error), .rdata(ref_rdata),
        .done_count(ref_done), .error_count(ref_err), .last_rdata(ref_rd),
        .last_latency(ref_lat)
    );

    wire        rphy_req, rphy_wen, rphy_ready, rphy_ack, rphy_error;
    wire        rphy_cancel, rphy_cancel_done;
    wire [31:0] rphy_addr;
    wire [63:0] rphy_wdata, rphy_rdata;

    mp64_extmem u_ref_ext (
        .clk(clk), .rst_n(rst_n),
        .cpu_req(ref_req), .cpu_addr(ref_addr[31:0]), .cpu_wdata(ref_wdata),
        .cpu_wen(ref_wen), .cpu_size(ref_size), .cpu_rdata(ref_rdata),
        .cpu_ack(ref_ack), .cpu_error(ref_error),
        .tile_req(1'b0), .tile_addr(32'd0), .tile_wdata(512'd0),
        .tile_wen(1'b0), .tile_cancel(1'b0), .tile_accept(), .tile_rdata(),
        .tile_ack(), .tile_error(), .tile_fault_addr(), .tile_word_done(),
        .phy_req(rphy_req), .phy_addr(rphy_addr), .phy_wdata(rphy_wdata),
        .phy_wen(rphy_wen), .phy_ready(rphy_ready), .phy_rdata(rphy_rdata),
        .phy_ack(rphy_ack), .phy_error(rphy_error), .phy_cancel(rphy_cancel),
        .phy_cancel_done(rphy_cancel_done), .phy_burst_len()
    );

    tb_arb_phy #(.LAT(3)) u_ref_phy (
        .clk(clk), .rst_n(rst_n), .phy_req(rphy_req), .phy_addr(rphy_addr),
        .phy_wdata(rphy_wdata), .phy_wen(rphy_wen), .phy_ready(rphy_ready),
        .phy_rdata(rphy_rdata), .phy_ack(rphy_ack), .phy_error(rphy_error),
        .phy_cancel(rphy_cancel), .phy_cancel_done(rphy_cancel_done),
        .err_enable(1'b0), .err_addr(32'd0)
    );

    // ------------------------------------------------------------------
    // Completion order log
    // ------------------------------------------------------------------
    reg     order_log [0:255];
    integer order_n;
    always @(posedge clk) begin
        if (c_ack[0] && order_n < 256) begin
            order_log[order_n] = 1'b0; order_n = order_n + 1;
        end
        if (c_ack[1] && order_n < 256) begin
            order_log[order_n] = 1'b1; order_n = order_n + 1;
        end
    end

    // A single ACK must reach exactly one client.
    reg ack_onehot_ok;
    always @(posedge clk)
        if (c_ack[0] && c_ack[1])
            ack_onehot_ok = 1'b0;

    task idle_clients;
        begin
            run0 <= 1'b0; run1 <= 1'b0; ref_run <= 1'b0;
            hold0 <= 8'd0; hold1 <= 8'd0;
            step0 <= 1'b1; step1 <= 1'b1;
            size0 <= 2'd3; size1 <= 2'd3;
            wen0 <= 1'b0; wen1 <= 1'b0;
            repeat (4) @(posedge clk);
            order_n = 0;
            u_phy.launch_n = 0;
        end
    endtask

    task wait_done;
        input [7:0] n0;
        input [7:0] n1;
        integer guard;
        begin
            guard = 0;
            while ((done0 < n0 || done1 < n1) && guard < 200000) begin
                @(posedge clk);
                guard = guard + 1;
            end
        end
    endtask

    integer k, zeros_run, max_zero_run, ones_run, max_one_run, ones_total;
    integer pattern_ok;
    reg [63:0] word;

    initial begin
        pass_count = 0; fail_count = 0; test_num = 0;
        order_n = 0; ack_onehot_ok = 1'b1;
        rst_n = 1'b0;
        run0 = 0; run1 = 0; ref_run = 0;
        base0 = 0; base1 = 0; wd0 = 0; wd1 = 0; wen0 = 0; wen1 = 0;
        size0 = 2'd3; size1 = 2'd3; count0 = 0; count1 = 0;
        hold0 = 0; hold1 = 0; step0 = 1; step1 = 1;
        c_weight = {8'd1, 8'd1}; c_bwlimit = {16'd0, 16'd0};
        err_enable = 1'b0; err_addr = 32'd0;
        repeat (6) @(posedge clk);
        rst_n = 1'b1;
        repeat (6) @(posedge clk);

        // --------------------------------------------------------------
        $display("\nLone client keeps the controller's latency");
        idle_clients;
        base0 <= 32'h0010_0100; wd0 <= 64'h1111_0000_0000_0000;
        wen0 <= 1'b1; count0 <= 8'd3;
        @(posedge clk);
        run0 <= 1'b1; ref_run <= 1'b1;
        wait_done(8'd3, 8'd0);
        repeat (2) @(posedge clk);
        check("three writes complete through the arbiter", done0 == 8'd3);
        check("the direct reference completes the same three",
              ref_done == 8'd3);
        check("latency through the arbiter equals the direct latency",
              lat0 == ref_lat);
        check("no error is reported", err0 == 8'd0);

        idle_clients;
        wen0 <= 1'b0; count0 <= 8'd1; base0 <= 32'h0010_0108;
        @(posedge clk);
        run0 <= 1'b1; ref_run <= 1'b1;
        wait_done(8'd1, 8'd0);
        repeat (2) @(posedge clk);
        check("a read returns the word written through the arbiter",
              rd0 == 64'h1111_0000_0000_0001);
        check("the read latency also matches the direct latency",
              lat0 == ref_lat);

        // --------------------------------------------------------------
        $display("\nEqual weights alternate under contention");
        idle_clients;
        c_weight = {8'd1, 8'd1};
        base0 <= 32'h0010_0200; base1 <= 32'h0010_0400;
        wd0 <= 64'hA000; wd1 <= 64'hB000; wen0 <= 1'b1; wen1 <= 1'b1;
        count0 <= 8'd8; count1 <= 8'd8;
        @(posedge clk);
        run0 <= 1'b1; run1 <= 1'b1;
        wait_done(8'd8, 8'd8);
        repeat (2) @(posedge clk);
        pattern_ok = 1;
        for (k = 1; k < 14; k = k + 1)
            if (order_log[k] == order_log[k-1])
                pattern_ok = 0;
        check("sixteen accesses complete", order_n == 16);
        check("the two clients alternate while both request",
              pattern_ok == 1);

        // --------------------------------------------------------------
        $display("\nWeight three against weight one");
        idle_clients;
        c_weight = {8'd1, 8'd3};          // client 1 = 1, client 0 = 3
        count0 <= 8'd12; count1 <= 8'd4;
        @(posedge clk);
        run0 <= 1'b1; run1 <= 1'b1;
        wait_done(8'd12, 8'd4);
        repeat (2) @(posedge clk);
        max_zero_run = 0; zeros_run = 0; max_one_run = 0; ones_run = 0;
        ones_total = 0;
        for (k = 0; k < 12; k = k + 1) begin
            if (order_log[k] == 1'b0) begin
                zeros_run = zeros_run + 1; ones_run = 0;
                if (zeros_run > max_zero_run) max_zero_run = zeros_run;
            end else begin
                ones_run = ones_run + 1; zeros_run = 0;
                ones_total = ones_total + 1;
                if (ones_run > max_one_run) max_one_run = ones_run;
            end
        end
        check("client 0 never runs more than three in a row",
              max_zero_run == 3);
        check("client 1 never runs two in a row", max_one_run == 1);
        check("client 1 takes three of the first twelve", ones_total == 3);
        c_weight = {8'd1, 8'd1};

        // --------------------------------------------------------------
        $display("\nBandwidth limit");
        idle_clients;
        // Start at a fresh epoch so the count is exact.
        while (dut.epoch_timer != 16'h0004) @(posedge clk);
        c_bwlimit = {16'd4, 16'd0};       // client 1 limited to four
        count0 <= 8'd0; count1 <= 8'd10; wen1 <= 1'b1;
        @(posedge clk);
        run1 <= 1'b1;
        repeat (2000) @(posedge clk);
        check("the limited client completes exactly its four accesses",
              done1 == 8'd4);
        check("its request is still waiting", c_req[1] == 1'b1);
        // The unlimited client runs meanwhile.
        count0 <= 8'd5; wen0 <= 1'b1;
        @(posedge clk);
        run0 <= 1'b1;
        repeat (400) @(posedge clk);
        check("the unlimited client is served while the other waits",
              done0 == 8'd5 && done1 == 8'd4);
        while (dut.epoch_timer != 16'h0000) @(posedge clk);
        repeat (2000) @(posedge clk);
        check("the limited client resumes in the next epoch",
              done1 == 8'd8);
        c_bwlimit = {16'd0, 16'd0};
        wait_done(8'd5, 8'd10);

        // --------------------------------------------------------------
        $display("\nBoth clients reach the same memory");
        idle_clients;
        base1 <= 32'h0010_0800; wd1 <= 64'hCAFE_F00D_0000_0000;
        wen1 <= 1'b1; count1 <= 8'd1; count0 <= 8'd0;
        @(posedge clk);
        run1 <= 1'b1;
        wait_done(8'd0, 8'd1);
        idle_clients;
        base0 <= 32'h0010_0800; wen0 <= 1'b0; count0 <= 8'd1;
        @(posedge clk);
        run0 <= 1'b1;
        wait_done(8'd1, 8'd0);
        repeat (2) @(posedge clk);
        check("client 0 reads the word client 1 wrote",
              rd0 == 64'hCAFE_F00D_0000_0000);

        // --------------------------------------------------------------
        $display("\nA sub-word write stays one access");
        idle_clients;
        u_phy.mem[10'h120] = 64'h8877_6655_4433_2211;
        // Client 0 writes byte lane 1, client 1 byte lane 5, same word.
        base0 <= 32'h0010_0901; wd0 <= 64'hAA; size0 <= 2'd0; wen0 <= 1'b1;
        base1 <= 32'h0010_0905; wd1 <= 64'hBB; size1 <= 2'd0; wen1 <= 1'b1;
        step0 <= 1'b0; step1 <= 1'b0; count0 <= 8'd1; count1 <= 8'd1;
        @(posedge clk);
        run0 <= 1'b1; run1 <= 1'b1;
        wait_done(8'd1, 8'd1);
        repeat (2) @(posedge clk);
        word = u_phy.mem[10'h120];
        check("both byte writes land and the other lanes keep their data",
              word == 64'h8877_BB55_4433_AA11);
        check("the PHY sees read, write, read, write",
              u_phy.launch_n == 4 &&
              u_phy.launch_log[0][32] == 1'b0 &&
              u_phy.launch_log[1][32] == 1'b1 &&
              u_phy.launch_log[2][32] == 1'b0 &&
              u_phy.launch_log[3][32] == 1'b1);

        // --------------------------------------------------------------
        $display("\nThe error sideband reaches only its client");
        idle_clients;
        err_enable = 1'b1; err_addr = 32'h0010_0A00;
        base0 <= 32'h0010_0A00; wen0 <= 1'b0; count0 <= 8'd1;
        base1 <= 32'h0010_0B00; wen1 <= 1'b0; count1 <= 8'd1;
        @(posedge clk);
        run0 <= 1'b1; run1 <= 1'b1;
        wait_done(8'd1, 8'd1);
        repeat (2) @(posedge clk);
        check("the failing access reports an error to client 0",
              err0 == 8'd1);
        check("the failing access returns zero data", rd0 == 64'd0);
        check("client 1's access reports no error", err1 == 8'd0);
        err_enable = 1'b0;

        // --------------------------------------------------------------
        $display("\nWithdrawal of a granted request");
        idle_clients;
        u_phy.mem[10'h180] = 64'h0123_4567_89AB_CDEF;
        base0 <= 32'h0010_0C00; wen0 <= 1'b0; count0 <= 8'd1;
        base1 <= 32'h0010_0C08; wen1 <= 1'b1; wd1 <= 64'h5555;
        count1 <= 8'd1;
        @(posedge clk);
        run0 <= 1'b1;
        repeat (3) @(posedge clk);      // granted and launched, no ACK yet
        run1 <= 1'b1;
        run0 <= 1'b0;                   // client 0 withdraws
        wait_done(8'd0, 8'd1);
        repeat (2) @(posedge clk);
        check("the withdrawn access delivers no completion",
              done0 == 8'd0);
        check("the other client's access completes afterwards",
              done1 == 8'd1 && u_phy.mem[10'h181] == 64'h5555);
        idle_clients;
        base0 <= 32'h0010_0C00; wen0 <= 1'b0; count0 <= 8'd1;
        @(posedge clk);
        run0 <= 1'b1;
        wait_done(8'd1, 8'd0);
        repeat (2) @(posedge clk);
        check("the client that withdrew reads correctly afterwards",
              rd0 == 64'h0123_4567_89AB_CDEF);

        // --------------------------------------------------------------
        $display("\nA held request runs once");
        idle_clients;
        base0 <= 32'h0010_0D00; wen0 <= 1'b1; wd0 <= 64'h77;
        count0 <= 8'd1; hold0 <= 8'd6;
        @(posedge clk);
        run0 <= 1'b1;
        wait_done(8'd1, 8'd0);
        repeat (12) @(posedge clk);
        check("one access reaches the PHY while REQ stays high",
              u_phy.launch_n == 1);
        hold0 <= 8'd0;

        check("no ACK ever reached two clients at once", ack_onehot_ok);

        $display("\n============================================");
        $display(" tb_extmem_arb: %0d passed, %0d failed",
                 pass_count, fail_count);
        $display("============================================");
        if (fail_count != 0)
            $display("FAIL");
        else
            $display("PASS");
        $finish;
    end

    initial begin
        #200000000;
        $display("TIMEOUT");
        $finish;
    end
endmodule
