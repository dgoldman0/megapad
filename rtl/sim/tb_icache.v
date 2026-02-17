// ============================================================================
// tb_icache.v — Testbench for mp64_icache
// ============================================================================
`timescale 1ns / 1ps

`include "mp64_pkg.vh"

module tb_icache;

    reg         clk, rst;
    reg  [63:0] fetch_addr;
    reg         fetch_valid;
    wire [63:0] fetch_data;
    wire        fetch_hit;
    wire        fetch_stall;
    wire        bus_valid;
    wire [63:0] bus_addr;
    reg  [63:0] bus_rdata;
    reg         bus_ready;
    wire        bus_wen;
    wire [1:0]  bus_size;
    reg         inv_all, inv_line;
    reg  [63:0] inv_addr;
    wire [63:0] stat_hits, stat_misses;

    mp64_icache uut (
        .clk        (clk),
        .rst        (rst),
        .fetch_addr (fetch_addr),
        .fetch_valid(fetch_valid),
        .fetch_data (fetch_data),
        .fetch_hit  (fetch_hit),
        .fetch_stall(fetch_stall),
        .bus_valid  (bus_valid),
        .bus_addr   (bus_addr),
        .bus_rdata  (bus_rdata),
        .bus_ready  (bus_ready),
        .bus_wen    (bus_wen),
        .bus_size   (bus_size),
        .inv_all    (inv_all),
        .inv_line   (inv_line),
        .inv_addr   (inv_addr),
        .stat_hits  (stat_hits),
        .stat_misses(stat_misses)
    );

    // Clock: 10 ns period
    always #5 clk = ~clk;

    integer pass_count, fail_count;

    task assert_eq;
        input [255:0] label;
        input [63:0]  actual, expected;
        begin
            if (actual !== expected) begin
                $display("FAIL [%0s]: got %h, expected %h", label, actual, expected);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task assert_true;
        input [255:0] label;
        input         val;
        begin
            if (val !== 1'b1) begin
                $display("FAIL [%0s]: expected true", label);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task assert_false;
        input [255:0] label;
        input         val;
        begin
            if (val !== 1'b0) begin
                $display("FAIL [%0s]: expected false", label);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    // Helper: complete a 2-beat refill by responding when bus_valid goes high
    task do_refill;
        input [63:0] beat0_data;
        input [63:0] beat1_data;
        begin
            // Wait for first beat request
            while (!bus_valid) @(posedge clk);
            @(negedge clk);
            bus_rdata = beat0_data;
            bus_ready = 1'b1;
            @(posedge clk); // first beat consumed
            @(negedge clk);
            bus_ready = 1'b0;

            // Wait for second beat request
            while (!bus_valid) @(posedge clk);
            @(negedge clk);
            bus_rdata = beat1_data;
            bus_ready = 1'b1;
            @(posedge clk); // second beat consumed
            @(negedge clk);
            bus_ready = 1'b0;
        end
    endtask

    initial begin
        $dumpfile("tb_icache.vcd");
        $dumpvars(0, tb_icache);

        pass_count  = 0;
        fail_count  = 0;
        clk         = 0;
        rst         = 1;
        fetch_addr  = 64'd0;
        fetch_valid = 1'b0;
        bus_rdata   = 64'd0;
        bus_ready   = 1'b0;
        inv_all     = 1'b0;
        inv_line    = 1'b0;
        inv_addr    = 64'd0;

        // Reset for 3 cycles
        repeat (3) @(posedge clk);
        @(negedge clk); rst = 1'b0;
        @(posedge clk);

        // ================================================================
        // Test 1: Cold miss — request addr 0x100, refill with known data
        // ================================================================
        @(negedge clk);
        fetch_addr  = 64'h0000_0000_0000_0100;
        fetch_valid = 1'b1;
        @(posedge clk);
        assert_false("T1: no immediate hit", fetch_hit);
        assert_true("T1: stall on miss", fetch_stall);

        // Complete 2-beat refill
        do_refill(64'hAAAA_BBBB_CCCC_DDDD,   // lo half (bytes 0-7)
                  64'h1111_2222_3333_4444);   // hi half (bytes 8-15)

        // After refill, line should be valid — next cycle should hit
        @(posedge clk);
        assert_true("T1: hit after refill", fetch_hit);
        assert_eq("T1: fetch_data lo", fetch_data, 64'hAAAA_BBBB_CCCC_DDDD);

        // ================================================================
        // Test 2: Hit — same line, upper half (offset[3]=1)
        // ================================================================
        @(negedge clk);
        fetch_addr = 64'h0000_0000_0000_0108;  // same line, hi half
        @(posedge clk);
        assert_true("T2: hit upper half", fetch_hit);
        assert_eq("T2: fetch_data hi", fetch_data, 64'h1111_2222_3333_4444);

        // ================================================================
        // Test 3: Repeated hit on same line
        // ================================================================
        @(negedge clk);
        fetch_addr = 64'h0000_0000_0000_0100;  // back to lo
        @(posedge clk);
        assert_true("T3: repeated hit", fetch_hit);
        assert_eq("T3: data", fetch_data, 64'hAAAA_BBBB_CCCC_DDDD);

        // ================================================================
        // Test 4: Miss at different index (addr 0x200)
        // ================================================================
        @(negedge clk);
        fetch_addr = 64'h0000_0000_0000_0200;
        @(posedge clk);
        assert_false("T4: miss different index", fetch_hit);

        do_refill(64'hDEAD_BEEF_CAFE_BABE,
                  64'hFACE_FEED_C0DE_D00D);
        @(posedge clk);
        assert_true("T4: hit after refill", fetch_hit);
        assert_eq("T4: data", fetch_data, 64'hDEAD_BEEF_CAFE_BABE);

        // ================================================================
        // Test 5: Original line (0x100) still valid
        // ================================================================
        @(negedge clk);
        fetch_addr = 64'h0000_0000_0000_0100;
        @(posedge clk);
        assert_true("T5: line 0x100 still valid", fetch_hit);
        assert_eq("T5: data", fetch_data, 64'hAAAA_BBBB_CCCC_DDDD);

        // ================================================================
        // Test 6: Invalidate single line
        // ================================================================
        @(negedge clk);
        inv_line = 1'b1;
        inv_addr = 64'h0000_0000_0000_0100;  // invalidate line at index for 0x100
        @(posedge clk);
        @(negedge clk);
        inv_line = 1'b0;

        // Now accessing 0x100 should miss
        fetch_addr = 64'h0000_0000_0000_0100;
        @(posedge clk);
        assert_false("T6: miss after inv_line", fetch_hit);

        // But 0x200 should still hit
        @(negedge clk);
        fetch_addr = 64'h0000_0000_0000_0200;

        // Need to clear the pending miss on 0x100 first
        // Actually the FSM will try to refill 0x100 — let it complete
        do_refill(64'h0000_0000_0000_0001, 64'h0000_0000_0000_0002);
        @(posedge clk);

        // Now switch to 0x200
        @(negedge clk);
        fetch_addr = 64'h0000_0000_0000_0200;
        @(posedge clk);
        assert_true("T6: 0x200 still valid", fetch_hit);
        assert_eq("T6: data", fetch_data, 64'hDEAD_BEEF_CAFE_BABE);

        // ================================================================
        // Test 7: Invalidate all
        // ================================================================
        @(negedge clk);
        inv_all = 1'b1;
        @(posedge clk);
        @(negedge clk);
        inv_all = 1'b0;

        // Both 0x100 and 0x200 should miss
        fetch_addr = 64'h0000_0000_0000_0200;
        @(posedge clk);
        assert_false("T7: miss after inv_all", fetch_hit);

        // Complete the refill to get FSM back to IDLE
        do_refill(64'hBBBB, 64'hCCCC);
        @(posedge clk);

        // ================================================================
        // Test 8: Tag conflict — same index, different tag
        //   addr 0x100 → index=0x10, tag=0x00
        //   addr 0x1100 → index=0x10, tag=0x01
        //   They alias to the same cache line
        // ================================================================
        @(negedge clk);
        fetch_addr = 64'h0000_0000_0000_0100;
        @(posedge clk);
        // miss (was invalidated) — refill
        do_refill(64'hAAAA_0000_0000_0001, 64'hAAAA_0000_0000_0002);
        @(posedge clk);
        assert_true("T8a: tag0 hit", fetch_hit);

        // Now access aliased address
        @(negedge clk);
        fetch_addr = 64'h0000_0000_0000_1100;
        @(posedge clk);
        assert_false("T8b: tag conflict miss", fetch_hit);

        // Refill with different data
        do_refill(64'hBBBB_0000_0000_0001, 64'hBBBB_0000_0000_0002);
        @(posedge clk);
        assert_true("T8c: new tag hit", fetch_hit);
        assert_eq("T8c: data", fetch_data, 64'hBBBB_0000_0000_0001);

        // Original should now miss (evicted)
        @(negedge clk);
        fetch_addr = 64'h0000_0000_0000_0100;
        @(posedge clk);
        assert_false("T8d: evicted tag miss", fetch_hit);

        // Complete pending refill
        do_refill(64'hAAAA_0000_0000_0001, 64'hAAAA_0000_0000_0002);
        @(posedge clk);

        // ================================================================
        // Test 9: bus_wen always 0, bus_size always DWORD
        // ================================================================
        assert_false("T9: bus_wen always 0", bus_wen);
        assert_eq("T9: bus_size", {62'd0, bus_size}, {62'd0, BUS_DWORD});

        // ================================================================
        // Test 10: Statistics
        // ================================================================
        // We can't predict exact counts easily, but hits > 0 and misses > 0
        assert_true("T10: hits > 0", stat_hits > 0);
        assert_true("T10: misses > 0", stat_misses > 0);

        // ================================================================
        // Test 11: No fetch_valid → no hit, no stall
        // ================================================================
        @(negedge clk);
        fetch_valid = 1'b0;
        fetch_addr  = 64'h0000_0000_0000_0100;
        @(posedge clk);
        assert_false("T11: no hit when !valid", fetch_hit);
        assert_false("T11: no stall when !valid", fetch_stall);

        // ================================================================
        // Summary
        // ================================================================
        #20;
        $display("");
        $display("============================================");
        if (fail_count == 0)
            $display(" tb_icache: ALL %0d assertions PASSED", pass_count);
        else
            $display(" tb_icache: %0d PASSED, %0d FAILED", pass_count, fail_count);
        $display("============================================");
        $finish;
    end

endmodule
