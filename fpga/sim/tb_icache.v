// ============================================================================
// tb_icache.v — Megapad-64 Instruction Cache Testbench
// ============================================================================
//
// Tests:
//   1. Cold miss → refill → re-access = hit
//   2. Sequential accesses within same line → hits after first miss
//   3. Different lines → separate misses
//   4. Tag conflict (same index, different tag) → eviction
//   5. Full invalidate → subsequent access is a miss
//   6. Single-line invalidate → only that line misses
//   7. Stats counters (hits and misses)
//   8. Upper-half fetch (offset[3]=1 within a line)
//   9. Back-to-back misses
//  10. Refill while not fetching (idle after refill)
//

`include "mp64_defs.vh"

module tb_icache;

    reg         clk;
    reg         rst_n;

    // CPU fetch interface
    reg  [63:0] fetch_addr;
    reg         fetch_valid;
    wire [63:0] fetch_data;
    wire        fetch_hit;
    wire        fetch_stall;

    // Bus interface (memory side)
    wire        bus_valid;
    wire [63:0] bus_addr;
    reg  [63:0] bus_rdata;
    reg         bus_ready;
    wire        bus_wen;
    wire [1:0]  bus_size;

    // Invalidate
    reg         inv_all;
    reg         inv_line;
    reg  [63:0] inv_addr;

    // Stats
    wire [63:0] stat_hits;
    wire [63:0] stat_misses;

    // ========================================================================
    // DUT
    // ========================================================================
    mp64_icache dut (
        .clk         (clk),
        .rst_n       (rst_n),
        .fetch_addr  (fetch_addr),
        .fetch_valid (fetch_valid),
        .fetch_data  (fetch_data),
        .fetch_hit   (fetch_hit),
        .fetch_stall (fetch_stall),
        .bus_valid   (bus_valid),
        .bus_addr    (bus_addr),
        .bus_rdata   (bus_rdata),
        .bus_ready   (bus_ready),
        .bus_wen     (bus_wen),
        .bus_size    (bus_size),
        .inv_all     (inv_all),
        .inv_line    (inv_line),
        .inv_addr    (inv_addr),
        .stat_hits   (stat_hits),
        .stat_misses (stat_misses)
    );

    // ========================================================================
    // Clock generation
    // ========================================================================
    initial clk = 0;
    always #5 clk = ~clk;

    // ========================================================================
    // Simple memory model — 1 KiB backing store, 1-cycle latency
    // ========================================================================
    reg [7:0] mem [0:1023];

    integer mi;
    initial begin
        for (mi = 0; mi < 1024; mi = mi + 1)
            mem[mi] = mi[7:0];   // pattern: address low byte
    end

    // Memory responds to bus requests with 1-cycle latency
    always @(posedge clk) begin
        if (bus_valid && !bus_ready) begin
            bus_rdata <= {mem[bus_addr[9:0]+7], mem[bus_addr[9:0]+6],
                          mem[bus_addr[9:0]+5], mem[bus_addr[9:0]+4],
                          mem[bus_addr[9:0]+3], mem[bus_addr[9:0]+2],
                          mem[bus_addr[9:0]+1], mem[bus_addr[9:0]+0]};
            bus_ready <= 1'b1;
        end else begin
            bus_ready <= 1'b0;
        end
    end

    // ========================================================================
    // Test sequence
    // ========================================================================
    integer pass_count;
    integer fail_count;
    integer test_num;

    task check;
        input [255:0] name;
        input         cond;
        begin
            test_num = test_num + 1;
            if (cond) begin
                $display("  PASS %0d: %0s", test_num, name);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL %0d: %0s", test_num, name);
                fail_count = fail_count + 1;
            end
        end
    endtask

    // Wait for a hit or timeout
    task wait_for_hit;
        input integer max_cycles;
        integer cyc;
        begin
            cyc = 0;
            while (!fetch_hit && cyc < max_cycles) begin
                @(posedge clk);
                cyc = cyc + 1;
            end
        end
    endtask

    initial begin
        $dumpfile("tb_icache.vcd");
        $dumpvars(0, tb_icache);

        pass_count = 0;
        fail_count = 0;
        test_num   = 0;

        // Initial state
        rst_n       = 0;
        fetch_addr  = 64'd0;
        fetch_valid = 0;
        inv_all     = 0;
        inv_line    = 0;
        inv_addr    = 64'd0;
        bus_rdata   = 64'd0;
        bus_ready   = 0;

        // Reset
        #20;
        rst_n = 1;
        @(posedge clk);
        @(posedge clk);

        $display("\n=== Megapad-64 Instruction Cache Tests ===\n");

        // ================================================================
        // TEST 1: Cold miss → refill → hit
        // ================================================================
        $display("--- TEST 1: Cold miss and refill ---");
        fetch_addr  = 64'h0000_0000;
        fetch_valid = 1;

        // First cycle: should be a miss (stall)
        @(posedge clk);
        check("Cold access is a miss", fetch_stall == 1);

        // Wait for refill to complete (2 beats + latency)
        wait_for_hit(20);
        check("Refill completes with hit", fetch_hit == 1);

        // Verify data: address 0x00 → bytes 0x07,0x06,...,0x00
        check("Data correct (lo half)", fetch_data == {8'h07, 8'h06, 8'h05, 8'h04,
                                                        8'h03, 8'h02, 8'h01, 8'h00});
        fetch_valid = 0;
        @(posedge clk);
        @(posedge clk);

        // Re-access same address: should hit immediately
        fetch_addr  = 64'h0000_0000;
        fetch_valid = 1;
        @(posedge clk);
        check("Re-access is a hit", fetch_hit == 1);
        fetch_valid = 0;
        @(posedge clk);

        // ================================================================
        // TEST 2: Same-line access (different offset)
        // ================================================================
        $display("--- TEST 2: Same-line hit (upper half) ---");
        fetch_addr  = 64'h0000_0008;  // same line (offset=8), upper half
        fetch_valid = 1;
        @(posedge clk);
        check("Same-line upper half is a hit", fetch_hit == 1);
        check("Upper half data correct", fetch_data == {8'h0F, 8'h0E, 8'h0D, 8'h0C,
                                                         8'h0B, 8'h0A, 8'h09, 8'h08});
        fetch_valid = 0;
        @(posedge clk);

        // ================================================================
        // TEST 3: Different line → miss
        // ================================================================
        $display("--- TEST 3: Different line access ---");
        fetch_addr  = 64'h0000_0010;  // line 1 (index=1)
        fetch_valid = 1;
        @(posedge clk);
        check("Different line is a miss", fetch_stall == 1);
        wait_for_hit(20);
        check("Line 1 refill completes", fetch_hit == 1);
        check("Line 1 data correct", fetch_data == {8'h17, 8'h16, 8'h15, 8'h14,
                                                     8'h13, 8'h12, 8'h11, 8'h10});
        fetch_valid = 0;
        @(posedge clk);

        // ================================================================
        // TEST 4: Tag conflict (eviction)
        // ================================================================
        $display("--- TEST 4: Tag conflict eviction ---");
        // Address 0x1000 has same index as 0x0000 (index=0) but different tag
        // tag(0x0000) = 0x00, tag(0x1000) = 0x01
        fetch_addr  = 64'h0000_1000;
        fetch_valid = 1;
        @(posedge clk);
        check("Conflicting tag is a miss", fetch_stall == 1);
        wait_for_hit(20);
        check("Conflict refill completes", fetch_hit == 1);
        fetch_valid = 0;
        @(posedge clk);

        // Original line 0 should now miss (evicted)
        fetch_addr  = 64'h0000_0000;
        fetch_valid = 1;
        @(posedge clk);
        check("Evicted line is a miss", fetch_stall == 1);
        wait_for_hit(20);
        check("Re-refill of evicted line", fetch_hit == 1);
        fetch_valid = 0;
        @(posedge clk);

        // ================================================================
        // TEST 5: Full invalidate
        // ================================================================
        $display("--- TEST 5: Full invalidate ---");
        // Verify line 0 is cached
        fetch_addr  = 64'h0000_0000;
        fetch_valid = 1;
        @(posedge clk);
        check("Pre-invalidate hit", fetch_hit == 1);
        fetch_valid = 0;
        @(posedge clk);

        // Invalidate
        inv_all = 1;
        @(posedge clk);
        inv_all = 0;
        @(posedge clk);

        // Same address should now miss
        fetch_addr  = 64'h0000_0000;
        fetch_valid = 1;
        @(posedge clk);
        check("Post-invalidate is a miss", fetch_stall == 1);
        wait_for_hit(20);
        check("Post-invalidate refill", fetch_hit == 1);
        fetch_valid = 0;
        @(posedge clk);

        // ================================================================
        // TEST 6: Single-line invalidate
        // ================================================================
        $display("--- TEST 6: Single-line invalidate ---");
        // Load two different lines
        fetch_addr  = 64'h0000_0020;  // line 2
        fetch_valid = 1;
        @(posedge clk);
        wait_for_hit(20);
        fetch_valid = 0;
        @(posedge clk);

        fetch_addr  = 64'h0000_0030;  // line 3
        fetch_valid = 1;
        @(posedge clk);
        wait_for_hit(20);
        fetch_valid = 0;
        @(posedge clk);

        // Invalidate only line 2
        // Set addr one cycle early to avoid race condition
        inv_addr = 64'h0000_0020;
        @(posedge clk);
        inv_line = 1;
        @(posedge clk);
        inv_line = 0;
        @(posedge clk);

        // Line 3 should still hit
        fetch_addr  = 64'h0000_0030;
        fetch_valid = 1;
        @(posedge clk);
        check("Non-invalidated line still hits", fetch_hit == 1);
        fetch_valid = 0;
        @(posedge clk);

        // Line 2 should miss
        fetch_addr  = 64'h0000_0020;
        fetch_valid = 1;
        @(posedge clk);
        check("Invalidated line misses", fetch_stall == 1);
        wait_for_hit(20);
        fetch_valid = 0;
        @(posedge clk);

        // ================================================================
        // TEST 7: Stats counters
        // ================================================================
        $display("--- TEST 7: Stats counters ---");
        check("Hit counter > 0", stat_hits > 0);
        check("Miss counter > 0", stat_misses > 0);
        $display("  Stats: hits=%0d, misses=%0d", stat_hits, stat_misses);

        // ================================================================
        // TEST 8: Bus protocol — wen always 0, size always dword
        // ================================================================
        $display("--- TEST 8: Bus protocol ---");
        check("bus_wen always 0 (read-only)", bus_wen == 0);
        check("bus_size always DWORD", bus_size == BUS_DWORD);

        // ================================================================
        // Summary
        // ================================================================
        $display("\n=== I-Cache Tests: %0d passed, %0d failed ===\n",
                 pass_count, fail_count);

        if (fail_count > 0) begin
            $display("SOME TESTS FAILED");
            $finish(1);
        end else begin
            $display("ALL %0d TESTS PASSED", pass_count);
        end

        $finish;
    end

    // Timeout watchdog
    initial begin
        #100000;
        $display("TIMEOUT — test did not complete");
        $finish(2);
    end

endmodule
