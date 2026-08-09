// ============================================================================
// tb_icache.v — Contract tests for mp64_icache
// ============================================================================
`timescale 1ns / 1ps

`include "mp64_pkg.vh"

module tb_icache;

    reg         clk, rst;
    reg         enabled;
    reg  [63:0] fetch_addr;
    reg         fetch_valid;
    wire [63:0] fetch_data;
    wire        fetch_hit;
    wire        fetch_stall;
    wire        bus_valid;
    wire [63:0] bus_addr;
    reg  [63:0] bus_rdata;
    reg         bus_ready;
    reg         bus_error;
    wire        bus_wen;
    wire [1:0]  bus_size;
    reg         inv_all, inv_line;
    reg  [63:0] inv_addr;
    reg  [6:0]  inv_size;
    wire [63:0] stat_hits, stat_misses;

    mp64_icache uut (
        .clk         (clk),
        .rst         (rst),
        .enabled     (enabled),
        .fetch_addr  (fetch_addr),
        .fetch_valid (fetch_valid),
        .fetch_data  (fetch_data),
        .fetch_hit   (fetch_hit),
        .fetch_stall (fetch_stall),
        .bus_valid   (bus_valid),
        .bus_addr    (bus_addr),
        .bus_rdata   (bus_rdata),
        .bus_ready   (bus_ready),
        .bus_error   (bus_error),
        .bus_wen     (bus_wen),
        .bus_size    (bus_size),
        .inv_all     (inv_all),
        .inv_line    (inv_line),
        .inv_addr    (inv_addr),
        .inv_size    (inv_size),
        .stat_hits   (stat_hits),
        .stat_misses (stat_misses)
    );

    always #5 clk = ~clk;

    integer pass_count, fail_count;

    task assert_eq;
        input [255:0] label;
        input [63:0]  actual, expected;
        begin
            if (actual !== expected) begin
                $display("FAIL [%0s]: got %h, expected %h",
                         label, actual, expected);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task assert_true;
        input [255:0] label;
        input         value;
        begin
            if (value !== 1'b1) begin
                $display("FAIL [%0s]: expected true", label);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task assert_false;
        input [255:0] label;
        input         value;
        begin
            if (value !== 1'b0) begin
                $display("FAIL [%0s]: expected false", label);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task begin_request;
        input [63:0] address;
        begin
            @(negedge clk);
            fetch_addr  = address;
            fetch_valid = 1'b1;
            @(posedge clk);
            #1;
        end
    endtask

    // Retire an immediate cache hit.  begin_request already crossed the
    // accepting edge, so the request must be dropped before another edge.
    task finish_hit;
        begin
            @(negedge clk);
            fetch_valid = 1'b0;
            @(posedge clk);
            #1;
        end
    endtask

    // A refill/bypass response becomes visible after its bus-accept edge.
    // Hold the request through one more edge so the requester consumes it.
    task finish_response;
        begin
            @(posedge clk);
            @(negedge clk);
            fetch_valid = 1'b0;
            @(posedge clk);
            #1;
        end
    endtask

    task complete_refill;
        input [63:0] expected_base;
        input [63:0] beat0_data;
        input [63:0] beat1_data;
        begin
            while (!bus_valid)
                @(posedge clk);

            @(negedge clk);
            assert_eq("refill beat 0 address", bus_addr, expected_base);
            bus_rdata = beat0_data;
            bus_ready = 1'b1;
            @(posedge clk);

            @(negedge clk);
            bus_ready = 1'b0;
            while (!bus_valid)
                @(negedge clk);
            assert_eq("refill beat 1 address", bus_addr,
                      expected_base + 64'd8);
            bus_rdata = beat1_data;
            bus_ready = 1'b1;
            @(posedge clk);

            @(negedge clk);
            bus_ready = 1'b0;
            #1;
        end
    endtask

    task complete_bypass;
        input [63:0] expected_address;
        input [63:0] read_data;
        begin
            while (!bus_valid)
                @(posedge clk);

            @(negedge clk);
            assert_eq("bypass DWORD address", bus_addr, expected_address);
            bus_rdata = read_data;
            bus_ready = 1'b1;
            @(posedge clk);

            @(negedge clk);
            bus_ready = 1'b0;
            #1;
        end
    endtask

    task invalidate_line;
        input [63:0] address;
        begin
            @(negedge clk);
            inv_addr = address;
            inv_size = 7'd1;
            inv_line = 1'b1;
            @(posedge clk);
            @(negedge clk);
            inv_line = 1'b0;
            #1;
        end
    endtask

    task invalidate_all;
        begin
            @(negedge clk);
            inv_all = 1'b1;
            @(posedge clk);
            #1;
            @(negedge clk);
            inv_all = 1'b0;
            @(posedge clk);
            #1;
        end
    endtask

    // Complete a refill while invalidating its line on the final beat.
    task complete_refill_with_final_invalidate;
        input [63:0] expected_base;
        input [63:0] beat0_data;
        input [63:0] beat1_data;
        begin
            while (!bus_valid)
                @(posedge clk);

            @(negedge clk);
            assert_eq("race refill beat 0 address", bus_addr, expected_base);
            bus_rdata = beat0_data;
            bus_ready = 1'b1;
            @(posedge clk);

            @(negedge clk);
            bus_ready = 1'b0;
            while (!bus_valid)
                @(negedge clk);
            assert_eq("race refill beat 1 address", bus_addr,
                      expected_base + 64'd8);
            bus_rdata = beat1_data;
            bus_ready = 1'b1;
            inv_addr  = expected_base;
            inv_size  = 7'd1;
            inv_line  = 1'b1;
            @(posedge clk);

            @(negedge clk);
            bus_ready = 1'b0;
            inv_line  = 1'b0;
            #1;
        end
    endtask

    initial begin
        $dumpfile("tb_icache.vcd");
        $dumpvars(0, tb_icache);

        pass_count  = 0;
        fail_count  = 0;
        clk         = 1'b0;
        rst         = 1'b1;
        enabled     = 1'b1;
        fetch_addr  = 64'd0;
        fetch_valid = 1'b0;
        bus_rdata   = 64'd0;
        bus_ready   = 1'b0;
        bus_error   = 1'b0;
        inv_all     = 1'b0;
        inv_line    = 1'b0;
        inv_addr    = 64'd0;
        inv_size    = 7'd1;

        repeat (3) @(posedge clk);
        @(negedge clk);
        rst = 1'b0;
        @(posedge clk);
        #1;

        assert_false("read-only bus", bus_wen);
        assert_eq("DWORD bus size", {62'd0, bus_size},
                  {62'd0, BUS_DWORD});
        assert_eq("reset hit count", stat_hits, 64'd0);
        assert_eq("reset miss count", stat_misses, 64'd0);

        // ================================================================
        // Test 1: one enabled miss, exactly two aligned refill beats
        // ================================================================
        begin_request(64'h0000_0000_0000_0100);
        assert_false("T1 cold request is not a hit", fetch_hit);
        assert_true("T1 cold request stalls", fetch_stall);
        assert_eq("T1 hit count before refill", stat_hits, 64'd0);
        assert_eq("T1 miss count", stat_misses, 64'd1);

        complete_refill(64'h0000_0000_0000_0100,
                        64'hAAAA_BBBB_CCCC_DDDD,
                        64'h1111_2222_3333_4444);
        assert_true("T1 refill response ready", fetch_hit);
        assert_eq("T1 refill response data", fetch_data,
                  64'hAAAA_BBBB_CCCC_DDDD);
        assert_eq("T1 refill is not also a hit", stat_hits, 64'd0);
        assert_eq("T1 still exactly one miss", stat_misses, 64'd1);
        finish_response;
        assert_eq("T1 response retirement adds no hit", stat_hits, 64'd0);

        // ================================================================
        // Test 2: an actual resident lookup counts exactly one hit
        // ================================================================
        begin_request(64'h0000_0000_0000_0108);
        assert_true("T2 upper half hit", fetch_hit);
        assert_eq("T2 upper half data", fetch_data,
                  64'h1111_2222_3333_4444);
        assert_eq("T2 exact hit count", stat_hits, 64'd1);
        assert_eq("T2 miss count unchanged", stat_misses, 64'd1);
        finish_hit;

        // ================================================================
        // Test 3: line invalidation compares the complete physical tag
        // ================================================================
        invalidate_line(64'h0000_0001_0000_0100);
        begin_request(64'h0000_0000_0000_0100);
        assert_true("T3 different-tag invalidation preserves line", fetch_hit);
        assert_eq("T3 preserved line data", fetch_data,
                  64'hAAAA_BBBB_CCCC_DDDD);
        assert_eq("T3 second exact hit", stat_hits, 64'd2);
        finish_hit;

        invalidate_line(64'h0000_0000_0000_0100);
        begin_request(64'h0000_0000_0000_0100);
        assert_false("T3 matching invalidation removes line", fetch_hit);
        assert_eq("T3 matching invalidation causes miss", stat_misses, 64'd2);
        complete_refill(64'h0000_0000_0000_0100,
                        64'hAAAA_0000_0000_0001,
                        64'hAAAA_0000_0000_0002);
        finish_response;

        // ================================================================
        // Test 4: address bits above the old 20-bit tag cannot alias
        // ================================================================
        begin_request(64'h0000_0001_0000_0100);
        assert_false("T4 high physical address conflicts", fetch_hit);
        assert_eq("T4 high-address miss count", stat_misses, 64'd3);
        complete_refill(64'h0000_0001_0000_0100,
                        64'hBBBB_0000_0000_0001,
                        64'hBBBB_0000_0000_0002);
        assert_eq("T4 high-address response", fetch_data,
                  64'hBBBB_0000_0000_0001);
        finish_response;

        begin_request(64'h0000_0000_0000_0100);
        assert_false("T4 original full tag was evicted", fetch_hit);
        assert_eq("T4 original address misses again", stat_misses, 64'd4);
        complete_refill(64'h0000_0000_0000_0100,
                        64'hCCCC_0000_0000_0001,
                        64'hCCCC_0000_0000_0002);
        finish_response;

        // ================================================================
        // Test 5: invalidate-all clears both state and exact counters
        // ================================================================
        invalidate_all;
        assert_eq("T5 invalidate-all resets hits", stat_hits, 64'd0);
        assert_eq("T5 invalidate-all resets misses", stat_misses, 64'd0);

        begin_request(64'h0000_0000_0000_0100);
        assert_false("T5 invalidate-all removed resident line", fetch_hit);
        assert_eq("T5 post-reset first miss", stat_misses, 64'd1);
        complete_refill(64'h0000_0000_0000_0100,
                        64'hDDDD_0000_0000_0001,
                        64'hDDDD_0000_0000_0002);
        finish_response;

        // ================================================================
        // Test 6: disabled fetch is one aligned uncached DWORD transaction
        // ================================================================
        invalidate_all;
        @(negedge clk);
        enabled = 1'b0;

        begin_request(64'h0000_0000_0000_012C);
        assert_false("T6 bypass waits for bus data", fetch_hit);
        assert_true("T6 bypass stalls until ready", fetch_stall);
        assert_eq("T6 bypass does not count a hit", stat_hits, 64'd0);
        assert_eq("T6 bypass does not count a miss", stat_misses, 64'd0);
        complete_bypass(64'h0000_0000_0000_0128,
                        64'h0123_4567_89AB_CDEF);
        assert_true("T6 bypass response ready", fetch_hit);
        assert_eq("T6 bypass response data", fetch_data,
                  64'h0123_4567_89AB_CDEF);
        assert_eq("T6 bypass response leaves hits zero", stat_hits, 64'd0);
        assert_eq("T6 bypass response leaves misses zero", stat_misses, 64'd0);
        finish_response;

        @(negedge clk);
        enabled = 1'b1;
        begin_request(64'h0000_0000_0000_012C);
        assert_false("T6 bypass did not allocate a line", fetch_hit);
        assert_eq("T6 enabled lookup now counts a miss", stat_misses, 64'd1);
        complete_refill(64'h0000_0000_0000_0120,
                        64'hEEEE_0000_0000_0001,
                        64'hEEEE_0000_0000_0002);
        assert_eq("T6 enabled refill selects upper beat", fetch_data,
                  64'hEEEE_0000_0000_0002);
        finish_response;

        // ================================================================
        // Test 7: matching invalidation wins over final refill publication
        // ================================================================
        invalidate_all;
        begin_request(64'h0000_0000_0000_0340);
        assert_eq("T7 initial refill miss", stat_misses, 64'd1);
        complete_refill_with_final_invalidate(
                        64'h0000_0000_0000_0340,
                        64'hFACE_0000_0000_0001,
                        64'hFACE_0000_0000_0002);
        assert_false("T7 invalidated refill has no response", fetch_hit);

        // Stop the held request before IDLE can retry it.
        fetch_valid = 1'b0;
        @(posedge clk);
        #1;
        begin_request(64'h0000_0000_0000_0340);
        assert_false("T7 invalidated refill was not published", fetch_hit);
        assert_eq("T7 retry is a second miss", stat_misses, 64'd2);
        complete_refill(64'h0000_0000_0000_0340,
                        64'hBEEF_0000_0000_0001,
                        64'hBEEF_0000_0000_0002);
        assert_true("T7 clean retry responds", fetch_hit);
        assert_eq("T7 clean retry data", fetch_data,
                  64'hBEEF_0000_0000_0001);
        assert_eq("T7 misses never become hits", stat_hits, 64'd0);
        finish_response;

        // ================================================================
        // Test 8: an unaligned store span invalidates both touched lines
        // ================================================================
        invalidate_all;
        begin_request(64'h0000_0000_0000_0400);
        complete_refill(64'h0000_0000_0000_0400,
                        64'h4000_0000_0000_0001,
                        64'h4000_0000_0000_0002);
        finish_response;
        begin_request(64'h0000_0000_0000_0410);
        complete_refill(64'h0000_0000_0000_0410,
                        64'h4100_0000_0000_0001,
                        64'h4100_0000_0000_0002);
        finish_response;

        @(negedge clk);
        inv_addr = 64'h0000_0000_0000_040F;
        inv_size = 7'd2;
        inv_line = 1'b1;
        @(posedge clk);
        @(negedge clk);
        inv_line = 1'b0;
        inv_size = 7'd1;

        begin_request(64'h0000_0000_0000_0400);
        assert_false("T8 crossing store invalidates first line", fetch_hit);
        complete_refill(64'h0000_0000_0000_0400,
                        64'h4000_0000_0000_0003,
                        64'h4000_0000_0000_0004);
        finish_response;
        begin_request(64'h0000_0000_0000_0410);
        assert_false("T8 crossing store invalidates second line", fetch_hit);
        complete_refill(64'h0000_0000_0000_0410,
                        64'h4100_0000_0000_0003,
                        64'h4100_0000_0000_0004);
        finish_response;

        // An invalid producer size must fail safe.  The architectural maximum
        // is 64 bytes; a larger span conservatively flushes private lines
        // without using a wrapped line count.
        @(negedge clk);
        inv_addr = 64'h0000_0000_0000_0000;
        inv_size = 7'd65;
        inv_line = 1'b1;
        @(posedge clk);
        @(negedge clk);
        inv_line = 1'b0;
        inv_size = 7'd1;
        begin_request(64'h0000_0000_0000_0400);
        assert_false("T8 oversized span conservatively flushes", fetch_hit);
        complete_refill(64'h0000_0000_0000_0400,
                        64'h4000_0000_0000_0005,
                        64'h4000_0000_0000_0006);
        finish_response;

        // ================================================================
        // Test 9: invalidate-all drains an already offered transaction
        // ================================================================
        begin_request(64'h0000_0000_0000_0500);
        while (!bus_valid)
            @(posedge clk);
        @(negedge clk);
        inv_all = 1'b1;
        @(posedge clk);
        #1;
        assert_true("T9 canceled request remains valid until READY",
                    bus_valid);
        assert_eq("T9 invalidate-all resets hits while draining",
                  stat_hits, 64'd0);
        assert_eq("T9 invalidate-all resets misses while draining",
                  stat_misses, 64'd0);
        @(negedge clk);
        inv_all = 1'b0;
        bus_rdata = 64'h5000_0000_0000_0001;
        bus_ready = 1'b1;
        @(posedge clk);
        @(negedge clk);
        bus_ready = 1'b0;
        fetch_valid = 1'b0;
        #1;
        assert_false("T9 drained request publishes no response", fetch_hit);
        assert_false("T9 drained request releases bus", bus_valid);

        // ================================================================
        // Test 10: an owner-qualified refill error is never cached as an
        // instruction response.  The held fetch retries from the same line
        // and can only become a hit after two successful refill beats.
        // ================================================================
        begin_request(64'h0000_0000_0000_0600);
        while (!bus_valid)
            @(posedge clk);
        @(negedge clk);
        assert_eq("T10 faulting refill address", bus_addr,
                  64'h0000_0000_0000_0600);
        bus_rdata = 64'hDEAD_DEAD_DEAD_DEAD;
        bus_ready = 1'b1;
        bus_error = 1'b1;
        @(posedge clk);
        @(negedge clk);
        bus_ready = 1'b0;
        bus_error = 1'b0;
        #1;
        assert_false("T10 refill error publishes no hit", fetch_hit);
        assert_false("T10 refill error releases failed request", bus_valid);
        @(posedge clk);
        #1;
        complete_refill(64'h0000_0000_0000_0600,
                        64'h6000_0000_0000_0001,
                        64'h6000_0000_0000_0002);
        assert_true("T10 successful retry publishes hit", fetch_hit);
        assert_eq("T10 retry data is not timeout sentinel", fetch_data,
                  64'h6000_0000_0000_0001);
        finish_response;

        // ================================================================
        // Test 11: idle interface has neither hit nor stall
        // ================================================================
        assert_false("T11 no hit without request", fetch_hit);
        assert_false("T11 no stall without request", fetch_stall);

        #20;
        $display("");
        $display("============================================");
        if (fail_count == 0)
            $display(" tb_icache: ALL %0d assertions PASSED", pass_count);
        else
            $display(" tb_icache: %0d PASSED, %0d FAILED",
                     pass_count, fail_count);
        $display("============================================");
        $finish;
    end

endmodule
