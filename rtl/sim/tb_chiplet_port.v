// ============================================================================
// tb_chiplet_port.v — Testbench for mp64_chiplet_port
// ============================================================================
//
// Drives the bridge from three sides: the register block, a behavioural
// chiplet on the link, and stand-ins for the external-memory client port and
// the mailbox.  Covers admission against lent ranges and external memory,
// the refusal record and its attention ring, staged range installation,
// memory errors, cancellation, doorbells in both directions, the sideband,
// and the behaviour of a port whose chiplet returns to reset.
//

`timescale 1ns/1ps

module tb_chiplet_port;
    localparam CLK_HALF = 5;

    // Register indices (offset >> 3).
    localparam [3:0] R_CTRL       = 4'h0;
    localparam [3:0] R_STATUS     = 4'h1;
    localparam [3:0] R_FAULT_ADDR = 4'h2;
    localparam [3:0] R_FAULT_INFO = 4'h3;
    localparam [3:0] R_RANGE_SEL  = 4'h4;
    localparam [3:0] R_RANGE_BASE = 4'h5;
    localparam [3:0] R_RANGE_LIM  = 4'h6;
    localparam [3:0] R_RANGE_RTS  = 4'h7;
    localparam [3:0] R_QOS        = 4'h8;
    localparam [3:0] R_ID_INDEX   = 4'h9;
    localparam [3:0] R_ID_DATA    = 4'hA;
    localparam [3:0] R_BOOT_ADDR  = 4'hB;
    localparam [3:0] R_BOOT_LEN   = 4'hC;

    localparam [1:0] LK_OK        = 2'd0;
    localparam [1:0] LK_REFUSED   = 2'd1;
    localparam [1:0] LK_MEM_ERROR = 2'd2;
    localparam [1:0] LK_CANCELLED = 2'd3;

    localparam [63:0] CTL_ENABLE   = 64'h01;
    localparam [63:0] CTL_RESET    = 64'h02;
    localparam [63:0] CTL_STOP     = 64'h04;
    localparam [63:0] CTL_DOORBELL = 64'h08;
    localparam [63:0] CTL_ATTN     = 64'h10;

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
    // DUT connections
    // ------------------------------------------------------------------
    reg         reg_sel, reg_wen;
    reg  [6:0]  reg_addr;
    reg  [63:0] reg_wdata;
    wire [63:0] reg_rdata;

    reg         lk_rq_valid, lk_rq_wen, lk_rq_cancel;
    reg  [63:0] lk_rq_addr, lk_rq_wdata;
    reg  [1:0]  lk_rq_size;
    reg  [7:0]  lk_rq_len;
    wire        lk_rq_ready;
    wire        lk_rs_valid;
    wire        lk_rs_last;
    reg         lk_rs_ready;
    wire [63:0] lk_rs_rdata;
    wire [1:0]  lk_rs_code;

    wire        lk_dbt_valid;
    reg         lk_dbt_ready;
    wire [7:0]  lk_dbt_src;
    reg         lk_dbr_valid;
    wire        lk_dbr_ready;
    reg  [7:0]  lk_dbr_dst;

    reg         sb_present, sb_stopped, sb_ready, sb_id_ack;
    reg  [63:0] sb_id_data;
    wire        sb_rst_n, sb_stop_req, sb_id_req;
    wire [7:0]  sb_id_index;
    wire [63:0] sb_boot_addr, sb_boot_len;

    wire        m_req, m_wen;
    wire [63:0] m_addr, m_wdata;
    wire [1:0]  m_size;
    reg         m_ack, m_error;
    reg  [63:0] m_rdata;
    wire [7:0]  m_weight;
    wire [15:0] m_bwlimit;

    reg         mb_db_valid;
    reg  [7:0]  mb_db_src;
    wire        mb_take_valid, mb_send_valid;
    wire [7:0]  mb_take_src, mb_send_dst;

    mp64_chiplet_port #(
        .RANGES    (6),
        .RANGE_BITS(3),
        .PORT_ID   (8'd16),
        .EXT_BASE  (64'h0000_0000_0010_0000),
        .EXT_LIMIT (64'h0000_0000_0800_0000)
    ) dut (
        .clk(clk), .rst_n(rst_n),
        .reg_sel(reg_sel), .reg_wen(reg_wen), .reg_idx(reg_addr[6:3]),
        .reg_wdata(reg_wdata), .reg_rdata(reg_rdata),
        .lk_rq_valid(lk_rq_valid), .lk_rq_ready(lk_rq_ready),
        .lk_rq_addr(lk_rq_addr), .lk_rq_wdata(lk_rq_wdata),
        .lk_rq_wen(lk_rq_wen), .lk_rq_size(lk_rq_size),
        .lk_rq_len(lk_rq_len), .lk_rq_cancel(lk_rq_cancel),
        .lk_rs_valid(lk_rs_valid), .lk_rs_ready(lk_rs_ready),
        .lk_rs_rdata(lk_rs_rdata), .lk_rs_code(lk_rs_code),
        .lk_rs_last(lk_rs_last),
        .lk_dbt_valid(lk_dbt_valid), .lk_dbt_ready(lk_dbt_ready),
        .lk_dbt_src(lk_dbt_src),
        .lk_dbr_valid(lk_dbr_valid), .lk_dbr_ready(lk_dbr_ready),
        .lk_dbr_dst(lk_dbr_dst),
        .sb_present(sb_present), .sb_rst_n(sb_rst_n),
        .sb_stop_req(sb_stop_req), .sb_stopped(sb_stopped),
        .sb_ready(sb_ready), .sb_id_req(sb_id_req),
        .sb_id_index(sb_id_index), .sb_id_ack(sb_id_ack),
        .sb_id_data(sb_id_data), .sb_boot_addr(sb_boot_addr),
        .sb_boot_len(sb_boot_len),
        .m_req(m_req), .m_addr(m_addr), .m_wdata(m_wdata), .m_wen(m_wen),
        .m_size(m_size), .m_ack(m_ack), .m_error(m_error),
        .m_rdata(m_rdata), .m_weight(m_weight), .m_bwlimit(m_bwlimit),
        .mb_db_valid(mb_db_valid), .mb_db_src(mb_db_src),
        .mb_take_valid(mb_take_valid), .mb_take_src(mb_take_src),
        .mb_send_valid(mb_send_valid), .mb_send_dst(mb_send_dst)
    );

    // ------------------------------------------------------------------
    // External-memory client stand-in: whole-word store, fixed latency
    // ------------------------------------------------------------------
    reg [63:0] mem [0:4095];
    integer    mem_latency;
    integer    mem_count;        // accesses that reached memory
    reg        mem_fail;         // answer the next access with an error
    integer    mwait;
    integer    mi;
    initial for (mi = 0; mi < 4096; mi = mi + 1) mem[mi] = 64'd0;

    always @(posedge clk) begin
        m_ack   <= 1'b0;
        m_error <= 1'b0;
        if (!rst_n) begin
            mwait <= 0;
        end else if (m_req && !m_ack) begin
            if (mwait == mem_latency) begin
                mwait <= 0;
                m_ack <= 1'b1;
                mem_count = mem_count + 1;
                if (mem_fail) begin
                    m_error <= 1'b1;
                    m_rdata <= 64'd0;
                end else if (m_wen) begin
                    mem[m_addr[14:3]] <= m_wdata;
                end else begin
                    m_rdata <= mem[m_addr[14:3]];
                end
            end else begin
                mwait <= mwait + 1;
            end
        end else begin
            mwait <= 0;
        end
    end

    // ------------------------------------------------------------------
    // Mailbox stand-in: record what the bridge sends and takes
    // ------------------------------------------------------------------
    integer send_count, take_count;
    reg [7:0] last_send_dst, last_take_src;
    always @(posedge clk) begin
        if (mb_send_valid) begin
            send_count = send_count + 1;
            last_send_dst = mb_send_dst;
        end
        if (mb_take_valid) begin
            take_count = take_count + 1;
            last_take_src = mb_take_src;
        end
    end

    // ------------------------------------------------------------------
    // Register and link tasks
    // ------------------------------------------------------------------
    reg [63:0] rd;

    task reg_write;
        input [3:0]  idx;
        input [63:0] data;
        begin
            @(posedge clk);
            reg_sel <= 1'b1; reg_wen <= 1'b1;
            reg_addr <= {idx, 3'b000}; reg_wdata <= data;
            @(posedge clk);
            reg_sel <= 1'b0; reg_wen <= 1'b0;
            #1;                     // let the written register settle
        end
    endtask

    task reg_read;
        input [3:0] idx;
        begin
            @(posedge clk);
            reg_sel <= 1'b1; reg_wen <= 1'b0; reg_addr <= {idx, 3'b000};
            @(posedge clk);
            rd = reg_rdata;
            reg_sel <= 1'b0;
        end
    endtask

    task install_range;
        input [2:0]  idx;
        input [63:0] base;
        input [63:0] limit;
        input [1:0]  rights;
        begin
            reg_write(R_RANGE_SEL, {61'd0, idx});
            reg_write(R_RANGE_BASE, base);
            reg_write(R_RANGE_LIM, limit);
            reg_write(R_RANGE_RTS, {62'd0, rights});
        end
    endtask

    reg [1:0]  rs_code;
    reg [63:0] rs_data;
    integer    guard;

    // One request and its response.  cancel_after: 0 = never; n = raise
    // lk_rq_cancel n cycles after the request is taken.
    task link_access;
        input [63:0] addr;
        input [63:0] wdata;
        input        wen;
        input [1:0]  size;
        input integer cancel_after;
        integer waited;
        begin
            @(posedge clk);
            lk_rq_valid <= 1'b1; lk_rq_addr <= addr; lk_rq_wdata <= wdata;
            lk_rq_wen <= wen; lk_rq_size <= size;
            guard = 0;
            @(posedge clk);
            while (!lk_rq_ready && guard < 2000) begin
                @(posedge clk); guard = guard + 1;
            end
            lk_rq_valid <= 1'b0;
            // cancel_after = n makes the bridge see the cancel in the nth
            // cycle after it took the request.
            lk_rq_cancel <= (cancel_after == 1);
            waited = 0;
            guard = 0;
            while (!lk_rs_valid && guard < 2000) begin
                @(posedge clk);
                waited = waited + 1;
                guard = guard + 1;
                lk_rq_cancel <= (cancel_after != 0 &&
                                 waited + 1 == cancel_after);
            end
            lk_rq_cancel <= 1'b0;
            rs_code = lk_rs_code;
            rs_data = lk_rs_rdata;
            lk_rs_ready <= 1'b1;
            @(posedge clk);
            lk_rs_ready <= 1'b0;
            @(posedge clk);
        end
    endtask

    integer prev_n;

    // ------------------------------------------------------------------
    // Bursts
    // ------------------------------------------------------------------
    reg [63:0] b_data [0:15];
    reg [1:0]  b_code [0:15];
    reg        b_last [0:15];
    integer    b_n;
    reg        b_done;

    // Take responses until the one marked last.  cancel_after n raises
    // lk_rq_cancel while response n is being taken (0 = never).
    task take_responses;
        input integer cancel_after;
        begin
            b_n = 0;
            b_done = 1'b0;
            guard = 0;
            while (!b_done && guard < 4000) begin
                @(posedge clk);
                guard = guard + 1;
                if (lk_rs_valid) begin
                    b_data[b_n] = lk_rs_rdata;
                    b_code[b_n] = lk_rs_code;
                    b_last[b_n] = lk_rs_last;
                    b_done = lk_rs_last;
                    b_n = b_n + 1;
                    lk_rs_ready <= 1'b1;
                    if (cancel_after != 0 && b_n == cancel_after)
                        lk_rq_cancel <= 1'b1;
                    @(posedge clk);
                    lk_rs_ready <= 1'b0;
                end
            end
            lk_rq_cancel <= 1'b0;
            @(posedge clk);
        end
    endtask

    task burst_read;
        input [63:0] addr;
        input [7:0]  len;
        input [1:0]  size;
        input integer cancel_after;
        begin
            @(posedge clk);
            lk_rq_valid <= 1'b1; lk_rq_addr <= addr; lk_rq_wdata <= 64'd0;
            lk_rq_wen <= 1'b0; lk_rq_size <= size; lk_rq_len <= len;
            guard = 0;
            @(posedge clk);
            while (!lk_rq_ready && guard < 2000) begin
                @(posedge clk); guard = guard + 1;
            end
            lk_rq_valid <= 1'b0; lk_rq_len <= 8'd0;
            take_responses(cancel_after);
        end
    endtask

    // Send len + 1 words, base, base + 1, ...  stop_at n withholds word n
    // and raises lk_rq_cancel in its place (0 = send them all).
    task burst_write;
        input [63:0] addr;
        input [7:0]  len;
        input [63:0] base;
        input integer stop_at;
        integer k;
        reg     ended;
        begin
            ended = 1'b0;
            @(posedge clk);
            lk_rq_valid <= 1'b1; lk_rq_addr <= addr; lk_rq_wdata <= base;
            lk_rq_wen <= 1'b1; lk_rq_size <= 2'd3; lk_rq_len <= len;
            guard = 0;
            @(posedge clk);
            while (!lk_rq_ready && guard < 2000) begin
                @(posedge clk); guard = guard + 1;
            end
            lk_rq_len <= 8'd0;
            for (k = 1; k <= len && !ended; k = k + 1) begin
                if (stop_at != 0 && k == stop_at) begin
                    lk_rq_valid <= 1'b0;
                    lk_rq_cancel <= 1'b1;
                    ended = 1'b1;
                end else begin
                    lk_rq_wdata <= base + k;
                    @(posedge clk);
                    while (!lk_rq_ready && !lk_rs_valid && guard < 4000) begin
                        @(posedge clk); guard = guard + 1;
                    end
                    if (lk_rs_valid)
                        ended = 1'b1;       // the bridge ended the burst
                end
            end
            lk_rq_valid <= 1'b0;
            take_responses(0);
        end
    endtask

    initial begin
        pass_count = 0; fail_count = 0; test_num = 0;
        mem_latency = 2; mem_count = 0; mem_fail = 1'b0;
        send_count = 0; take_count = 0;
        rst_n = 1'b0;
        reg_sel = 0; reg_wen = 0; reg_addr = 0; reg_wdata = 0;
        lk_rq_valid = 0; lk_rq_wen = 0; lk_rq_cancel = 0;
        lk_rq_addr = 0; lk_rq_wdata = 0; lk_rq_size = 2'd3; lk_rq_len = 8'd0;
        lk_rs_ready = 0; lk_dbt_ready = 0; lk_dbr_valid = 0; lk_dbr_dst = 0;
        sb_present = 1'b1; sb_stopped = 0; sb_ready = 0;
        sb_id_ack = 0; sb_id_data = 0;
        mb_db_valid = 0; mb_db_src = 0;
        repeat (5) @(posedge clk);
        rst_n = 1'b1;
        repeat (3) @(posedge clk);

        // --------------------------------------------------------------
        $display("\nReset state");
        reg_read(R_CTRL);
        check("the chiplet is held in reset after reset", rd == CTL_RESET);
        check("the sideband reset line is low", sb_rst_n == 1'b0);
        check("the link takes no request while the chiplet is in reset",
              lk_rq_ready == 1'b0);
        reg_read(R_STATUS);
        check("STATUS reports a present chiplet, port ID 16, six ranges",
              rd == {24'd0, 8'd6, 8'd0, 8'd16, 16'h0001});

        // --------------------------------------------------------------
        $display("\nA closed port refuses");
        reg_write(R_CTRL, 64'd0);             // release reset, not enabled
        check("the sideband reset line is released", sb_rst_n == 1'b1);
        link_access(64'h0020_0000, 64'd0, 1'b0, 2'd3, 0);
        check("a request on a closed port is refused",
              rs_code == LK_REFUSED);
        check("nothing reached memory", mem_count == 0);
        reg_read(R_STATUS);
        check("the refusal sets FAULT", rd[4] == 1'b1);
        reg_read(R_FAULT_ADDR);
        check("the refused address is recorded", rd == 64'h0020_0000);
        reg_read(R_FAULT_INFO);
        check("the record names the reason, the size and one refusal",
              rd == {32'd0, 16'd1, 8'd0, 4'd1, 1'b0, 1'b0, 2'd3});
        reg_write(R_STATUS, 64'h10);
        reg_read(R_STATUS);
        check("writing 1 to FAULT clears it", rd[4] == 1'b0);

        // --------------------------------------------------------------
        $display("\nAn open port with nothing lent refuses");
        reg_write(R_CTRL, CTL_ENABLE);
        link_access(64'h0020_0000, 64'd0, 1'b0, 2'd3, 0);
        check("no lent range covers the request", rs_code == LK_REFUSED);
        reg_read(R_FAULT_INFO);
        check("the reason is that nothing is lent", rd[7:4] == 4'd4);
        reg_write(R_STATUS, 64'h10);

        // --------------------------------------------------------------
        $display("\nA range lent for reading");
        mem[12'h000] = 64'h1122_3344_5566_7788;
        install_range(3'd0, 64'h0020_0000, 64'h0020_1000, 2'b01);
        link_access(64'h0020_0000, 64'd0, 1'b0, 2'd3, 0);
        check("a read inside the range completes",
              rs_code == LK_OK && rs_data == 64'h1122_3344_5566_7788);
        prev_n = mem_count;
        link_access(64'h0020_0008, 64'hDEAD, 1'b1, 2'd3, 0);
        check("a write to a read-only range is refused",
              rs_code == LK_REFUSED);
        check("the refused write never reached memory",
              mem_count == prev_n);
        reg_write(R_STATUS, 64'h10);

        // --------------------------------------------------------------
        $display("\nA range lent for reading and writing");
        install_range(3'd1, 64'h0020_2000, 64'h0020_3004, 2'b11);
        link_access(64'h0020_2010, 64'hA5A5_0000_1234_5678, 1'b1, 2'd3, 0);
        check("a write inside the range completes", rs_code == LK_OK);
        link_access(64'h0020_2010, 64'd0, 1'b0, 2'd3, 0);
        check("the word reads back",
              rs_code == LK_OK && rs_data == 64'hA5A5_0000_1234_5678);
        link_access(64'h0020_3000, 64'd0, 1'b0, 2'd2, 0);
        check("the last four bytes of the range are reachable",
              rs_code == LK_OK);
        link_access(64'h0020_3000, 64'd0, 1'b0, 2'd3, 0);
        check("an access that crosses the limit is refused",
              rs_code == LK_REFUSED);
        link_access(64'h0020_1FF8, 64'd0, 1'b0, 2'd3, 0);
        check("an access below the base is refused",
              rs_code == LK_REFUSED);
        reg_write(R_STATUS, 64'h10);
        link_access(64'h0020_2004, 64'd0, 1'b0, 2'd3, 0);
        check("a misaligned access is refused", rs_code == LK_REFUSED);
        reg_read(R_FAULT_INFO);
        check("the reason is misalignment", rd[7:4] == 4'd2);
        reg_write(R_STATUS, 64'h10);

        // --------------------------------------------------------------
        $display("\nLent ranges reach external memory only");
        install_range(3'd2, 64'h0000_0000, 64'h0000_2000, 2'b11);
        prev_n = mem_count;
        link_access(64'h0000_1000, 64'hBAD, 1'b1, 2'd3, 0);
        check("a range over Bank 0 gives no access to Bank 0",
              rs_code == LK_REFUSED && mem_count == prev_n);
        reg_read(R_FAULT_INFO);
        check("the reason is that the address is outside external memory",
              rd[7:4] == 4'd3);
        reg_write(R_STATUS, 64'h10);
        install_range(3'd3, 64'hFFD0_0000, 64'hFFE0_0000, 2'b11);
        link_access(64'hFFD0_0000, 64'd0, 1'b0, 2'd3, 0);
        check("a range over the fast banks gives no access to them",
              rs_code == LK_REFUSED);
        install_range(3'd4, 64'hFFFF_FF00_0000_0000,
                      64'hFFFF_FF00_0000_1000, 2'b11);
        link_access(64'hFFFF_FF00_0000_0500, 64'd0, 1'b0, 2'd3, 0);
        check("a range over the devices gives no access to them",
              rs_code == LK_REFUSED && mem_count == prev_n);
        reg_write(R_STATUS, 64'h10);

        // --------------------------------------------------------------
        $display("\nA range is installed in one step");
        reg_write(R_RANGE_SEL, 64'd1);
        reg_write(R_RANGE_BASE, 64'h0030_0000);
        reg_write(R_RANGE_LIM, 64'h0030_1000);
        reg_read(R_RANGE_BASE);
        check("a staged base leaves the installed base in place",
              rd == 64'h0020_2000);
        link_access(64'h0020_2010, 64'd0, 1'b0, 2'd3, 0);
        check("the installed range still serves requests",
              rs_code == LK_OK);
        link_access(64'h0030_0000, 64'd0, 1'b0, 2'd3, 0);
        check("the staged range serves none", rs_code == LK_REFUSED);
        reg_write(R_STATUS, 64'h10);
        reg_write(R_RANGE_RTS, 64'd3);
        reg_read(R_RANGE_BASE);
        check("writing the rights installs the staged base",
              rd == 64'h0030_0000);
        link_access(64'h0030_0000, 64'd0, 1'b0, 2'd3, 0);
        check("the new range serves requests", rs_code == LK_OK);
        link_access(64'h0020_2010, 64'd0, 1'b0, 2'd3, 0);
        check("the replaced range serves none", rs_code == LK_REFUSED);
        reg_write(R_STATUS, 64'h10);
        reg_write(R_RANGE_SEL, 64'd7);
        reg_write(R_RANGE_BASE, 64'h0040_0000);
        reg_write(R_RANGE_LIM, 64'h0040_1000);
        reg_write(R_RANGE_RTS, 64'd3);
        link_access(64'h0040_0000, 64'd0, 1'b0, 2'd3, 0);
        check("a range index beyond the port's ranges installs nothing",
              rs_code == LK_REFUSED);
        reg_write(R_STATUS, 64'h10);

        // --------------------------------------------------------------
        $display("\nMemory errors and cancellation");
        mem_fail = 1'b1;
        link_access(64'h0030_0008, 64'd0, 1'b0, 2'd3, 0);
        check("a memory error is reported as such",
              rs_code == LK_MEM_ERROR && rs_data == 64'd0);
        mem_fail = 1'b0;
        reg_read(R_STATUS);
        check("a memory error is no refusal", rd[4] == 1'b0);

        mem_latency = 12;
        prev_n = mem_count;
        link_access(64'h0030_0010, 64'h77, 1'b1, 2'd3, 4);
        check("a request cancelled while in memory answers CANCELLED",
              rs_code == LK_CANCELLED);
        check("the access it had started ran to its end",
              mem_count == prev_n + 1);
        prev_n = mem_count;
        link_access(64'h0030_0018, 64'h88, 1'b1, 2'd3, 1);
        check("a request cancelled prev_n it starts answers CANCELLED",
              rs_code == LK_CANCELLED);
        check("and never reaches memory", mem_count == prev_n);
        mem_latency = 2;
        link_access(64'h0030_0000, 64'd0, 1'b0, 2'd3, 0);
        check("the port serves requests after a cancellation",
              rs_code == LK_OK);

        // --------------------------------------------------------------
        $display("\nAttention on refusal");
        reg_write(R_CTRL, CTL_ENABLE | CTL_ATTN | (64'd2 << 8));
        prev_n = send_count;
        link_access(64'h0050_0000, 64'd0, 1'b0, 2'd3, 0);
        check("the refusal rings the attention core once",
              send_count == prev_n + 1 && last_send_dst == 8'd2);
        link_access(64'h0050_0008, 64'd0, 1'b0, 2'd3, 0);
        check("further refusals stay quiet until FAULT is cleared",
              send_count == prev_n + 1);
        reg_read(R_FAULT_INFO);
        check("both refusals are counted", rd[31:16] == 16'd2);
        reg_write(R_STATUS, 64'h10);

        // --------------------------------------------------------------
        $display("\nDoorbells from the chiplet");
        prev_n = send_count;
        @(posedge clk);
        lk_dbr_valid <= 1'b1; lk_dbr_dst <= 8'd1;
        @(posedge clk);
        lk_dbr_valid <= 1'b0;
        @(posedge clk);
        check("a doorbell is dropped while DOORBELL_EN is clear",
              send_count == prev_n);
        reg_write(R_CTRL, CTL_ENABLE | CTL_DOORBELL);
        @(posedge clk);
        lk_dbr_valid <= 1'b1; lk_dbr_dst <= 8'd3;
        @(posedge clk);
        lk_dbr_valid <= 1'b0;
        @(posedge clk);
        check("with DOORBELL_EN set it rings its destination",
              send_count == prev_n + 1 && last_send_dst == 8'd3);

        // --------------------------------------------------------------
        $display("\nDoorbells to the chiplet");
        prev_n = take_count;
        @(posedge clk);
        mb_db_valid <= 1'b1; mb_db_src <= 8'd1;
        repeat (2) @(posedge clk);
        check("a pending doorbell is offered to the chiplet",
              lk_dbt_valid == 1'b1 && lk_dbt_src == 8'd1);
        mb_db_src <= 8'd0;                     // a lower source arrives
        repeat (2) @(posedge clk);
        check("the offered source stays stable until it is taken",
              lk_dbt_valid == 1'b1 && lk_dbt_src == 8'd1);
        lk_dbt_ready <= 1'b1;
        @(posedge clk);
        lk_dbt_ready <= 1'b0;
        mb_db_valid <= 1'b0;
        @(posedge clk);
        check("taking it clears that source in the mailbox",
              take_count == prev_n + 1 && last_take_src == 8'd1);

        // --------------------------------------------------------------
        $display("\nSideband");
        reg_write(R_QOS, {32'd0, 16'd300, 8'd0, 8'd5});
        check("QOS drives the arbiter's weight and limit",
              m_weight == 8'd5 && m_bwlimit == 16'd300);
        reg_write(R_BOOT_ADDR, 64'h0060_0000);
        reg_write(R_BOOT_LEN, 64'h0000_8000);
        check("the boot words reach the sideband",
              sb_boot_addr == 64'h0060_0000 && sb_boot_len == 64'h8000);
        reg_write(R_ID_INDEX, 64'd3);
        repeat (2) @(posedge clk);
        check("an identity read is requested for word 3",
              sb_id_req == 1'b1 && sb_id_index == 8'd3);
        sb_id_data <= 64'h4C4D_3100_0001_0000;
        sb_id_ack <= 1'b1;
        @(posedge clk);
        sb_id_ack <= 1'b0;
        @(posedge clk);
        reg_read(R_STATUS);
        check("ID_DONE reports the finished read", rd[5] == 1'b1);
        reg_read(R_ID_DATA);
        check("ID_DATA holds the word", rd == 64'h4C4D_3100_0001_0000);
        reg_write(R_CTRL, CTL_ENABLE | CTL_DOORBELL | CTL_STOP);
        check("STOP_REQ reaches the sideband", sb_stop_req == 1'b1);
        sb_stopped <= 1'b1; sb_ready <= 1'b1;
        @(posedge clk);
        reg_read(R_STATUS);
        check("STATUS reports STOPPED and READY",
              rd[1] == 1'b1 && rd[2] == 1'b1);

        // --------------------------------------------------------------
        $display("\nA chiplet returned to reset");
        mem_latency = 12;
        @(posedge clk);
        lk_rq_valid <= 1'b1; lk_rq_addr <= 64'h0030_0000;
        lk_rq_wen <= 1'b0; lk_rq_size <= 2'd3;
        @(posedge clk);
        @(posedge clk);
        lk_rq_valid <= 1'b0;
        repeat (3) @(posedge clk);
        reg_write(R_CTRL, CTL_ENABLE | CTL_RESET);
        check("the sideband reset line falls", sb_rst_n == 1'b0);
        repeat (30) @(posedge clk);
        check("the request in flight ends without a response",
              lk_rs_valid == 1'b0);
        reg_read(R_STATUS);
        check("the port is idle again", rd[3] == 1'b0);
        check("the link takes no request", lk_rq_ready == 1'b0);
        mem_latency = 2;
        reg_write(R_CTRL, CTL_ENABLE);
        link_access(64'h0030_0000, 64'd0, 1'b0, 2'd3, 0);
        check("released again, the port serves requests",
              rs_code == LK_OK);

        // --------------------------------------------------------------
        $display("\nBursts");
        // Range 1 is [0x0030_0000, 0x0030_1000), read and write.
        mem[12'h040] = 64'hB000; mem[12'h041] = 64'hB001;
        mem[12'h042] = 64'hB002; mem[12'h043] = 64'hB003;
        prev_n = mem_count;
        burst_read(64'h0030_0200, 8'd3, 2'd3, 0);
        check("a read burst of four words gives four responses",
              b_n == 4 && mem_count == prev_n + 4);
        check("the words arrive in order",
              b_data[0] == 64'hB000 && b_data[1] == 64'hB001 &&
              b_data[2] == 64'hB002 && b_data[3] == 64'hB003);
        check("only the fourth is marked last",
              !b_last[0] && !b_last[1] && !b_last[2] && b_last[3]);
        prev_n = mem_count;
        burst_read(64'h0030_0FF0, 8'd3, 2'd3, 0);
        check("a burst that leaves the range is refused whole, once",
              b_n == 1 && b_code[0] == LK_REFUSED && b_last[0] &&
              mem_count == prev_n);
        reg_read(R_FAULT_INFO);
        check("the record holds the burst's length", rd[15:8] == 8'd3);
        reg_write(R_STATUS, 64'h10);
        burst_read(64'h0030_0200, 8'd1, 2'd2, 0);
        check("a burst of sub-word accesses is refused as misaligned",
              b_n == 1 && b_code[0] == LK_REFUSED);
        reg_read(R_FAULT_INFO);
        check("with that reason", rd[7:4] == 4'd2);
        reg_write(R_STATUS, 64'h10);

        prev_n = mem_count;
        burst_write(64'h0030_0300, 8'd2, 64'hC0DE_0000, 0);
        check("a write burst of three words gives one response, OK",
              b_n == 1 && b_code[0] == LK_OK && b_last[0]);
        check("the three words are in memory",
              mem_count == prev_n + 3 &&
              mem[12'h060] == 64'hC0DE_0000 &&
              mem[12'h061] == 64'hC0DE_0001 &&
              mem[12'h062] == 64'hC0DE_0002);

        prev_n = mem_count;
        burst_read(64'h0030_0200, 8'd3, 2'd3, 1);
        check("a read burst cancelled after its first word ends CANCELLED",
              b_n == 2 && b_code[0] == LK_OK &&
              b_code[1] == LK_CANCELLED && b_last[1]);
        check("no further word is read", mem_count == prev_n + 1);
        prev_n = mem_count;
        burst_write(64'h0030_0400, 8'd3, 64'hFACE_0000, 2);
        check("a write burst cancelled before its third word ends CANCELLED",
              b_n == 1 && b_code[0] == LK_CANCELLED && b_last[0]);
        check("the words already sent are written",
              mem_count == prev_n + 2 && mem[12'h081] == 64'hFACE_0001);
        mem_fail = 1'b1;
        burst_write(64'h0030_0500, 8'd2, 64'h1, 0);
        check("a memory error ends a write burst at its first word",
              b_n == 1 && b_code[0] == LK_MEM_ERROR && b_last[0]);
        mem_fail = 1'b0;
        link_access(64'h0030_0000, 64'd0, 1'b0, 2'd3, 0);
        check("single accesses still work after bursts", rs_code == LK_OK);

        $display("\n============================================");
        $display(" tb_chiplet_port: %0d passed, %0d failed",
                 pass_count, fail_count);
        $display("============================================");
        if (fail_count != 0)
            $display("FAIL");
        else
            $display("PASS");
        $finish;
    end

    initial begin
        #50000000;
        $display("TIMEOUT");
        $finish;
    end
endmodule
