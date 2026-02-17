// ============================================================================
// tb_trng.v — TRNG Unit Tests
// ============================================================================
//
// Tests:
//   1. STATUS reads 0 immediately after reset (pool not yet seeded)
//   2. STATUS reads 1 after 256+ clocks (pool fully seeded)
//   3. RAND8 returns an 8-bit value (upper 56 bits zero)
//   4. RAND64 returns a 64-bit value
//   5. Two consecutive RAND64 reads produce different values
//   6. SEED register write doesn't crash (functional smoke)
//   7. RAND8 varies after SEED injection
//   8. Multiple RAND8 reads produce non-identical values
//   9. Health monitor: STATUS stays valid under normal operation
//

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_trng;

    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;  // 100 MHz

    integer pass_count = 0;
    integer fail_count = 0;
    integer test_num   = 0;

    // ========================================================================
    // TRNG DUT
    // ========================================================================
    reg         trng_req;
    reg  [4:0]  trng_addr;
    reg  [63:0] trng_wdata;
    reg         trng_wen;
    wire [63:0] trng_rdata;
    wire        trng_ack;

    mp64_trng u_trng (
        .clk(clk), .rst_n(rst_n),
        .req(trng_req), .addr(trng_addr), .wdata(trng_wdata), .wen(trng_wen),
        .rdata(trng_rdata), .ack(trng_ack)
    );

    // ========================================================================
    // Helper tasks
    // ========================================================================
    task trng_write(input [4:0] a, input [63:0] d);
        begin
            @(posedge clk);
            trng_req  <= 1'b1; trng_addr <= a; trng_wdata <= d; trng_wen <= 1'b1;
            @(posedge clk);
            trng_req  <= 1'b0; trng_wen  <= 1'b0;
        end
    endtask

    task trng_read(input [4:0] a, output [63:0] d);
        begin
            @(posedge clk);
            trng_req  <= 1'b1; trng_addr <= a; trng_wen <= 1'b0;
            @(posedge clk);
            trng_req  <= 1'b0;
            @(posedge clk);  // wait for registered rdata to settle
            d = trng_rdata;
        end
    endtask

    task check(input [511:0] label, input [63:0] got, input [63:0] expected);
        begin
            if (got === expected) begin
                $display("  PASS: %0s = %016h", label, got);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s = %016h (expected %016h)", label, got, expected);
                fail_count = fail_count + 1;
            end
        end
    endtask

    task check_ne(input [511:0] label, input [63:0] a, input [63:0] b);
        begin
            if (a !== b) begin
                $display("  PASS: %0s: %016h != %016h", label, a, b);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s: both = %016h (expected different)", label, a);
                fail_count = fail_count + 1;
            end
        end
    endtask

    task check_zero_upper(input [511:0] label, input [63:0] val);
        begin
            if (val[63:8] === 56'd0) begin
                $display("  PASS: %0s = %02h (upper 56 bits zero)", label, val[7:0]);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s = %016h (upper 56 bits NOT zero)", label, val);
                fail_count = fail_count + 1;
            end
        end
    endtask

    // ========================================================================
    // Main test sequence
    // ========================================================================
    reg [63:0] rd, rd2, rd3;
    integer i;

    initial begin
        $dumpfile("tb_trng.vcd");
        $dumpvars(0, tb_trng);

        // Init signals
        rst_n = 0;
        trng_req = 0; trng_addr = 0; trng_wdata = 0; trng_wen = 0;

        // Reset
        repeat(4) @(posedge clk);
        rst_n = 1;
        repeat(2) @(posedge clk);

        // ================================================================
        // Test 1: STATUS = 0 right after reset (pool not fully seeded)
        // ================================================================
        test_num = 1;
        $display("\nTest %0d: STATUS=0 after reset", test_num);
        trng_read(5'h10, rd);
        check("STATUS after reset", rd, 64'd0);

        // ================================================================
        // Test 2: STATUS = 1 after pool is fully seeded
        //   raw_count wraps every 64 cycles, raw_valid fires.
        //   pool_ready = 1 after pool_idx wraps past 3 (4 × 64 = 256 cycles).
        //   Give it 300 cycles to be safe.
        // ================================================================
        test_num = 2;
        $display("\nTest %0d: STATUS=1 after seeding", test_num);
        repeat(300) @(posedge clk);
        trng_read(5'h10, rd);
        check("STATUS after 300 clocks", rd, 64'd1);

        // ================================================================
        // Test 3: RAND8 returns 8-bit value (upper 56 bits = 0)
        // ================================================================
        test_num = 3;
        $display("\nTest %0d: RAND8 upper bits zero", test_num);
        trng_read(5'h00, rd);
        check_zero_upper("RAND8", rd);

        // ================================================================
        // Test 4: RAND64 returns a 64-bit value (non-zero after seeding)
        // ================================================================
        test_num = 4;
        $display("\nTest %0d: RAND64 non-zero", test_num);
        trng_read(5'h08, rd);
        if (rd !== 64'd0) begin
            $display("  PASS: RAND64 = %016h (non-zero)", rd);
            pass_count = pass_count + 1;
        end else begin
            $display("  FAIL: RAND64 = 0 (expected non-zero after seeding)");
            fail_count = fail_count + 1;
        end

        // ================================================================
        // Test 5: Two consecutive RAND64 reads differ
        //   The output is mixed with an incrementing counter, so reads
        //   at different times should yield different values.
        // ================================================================
        test_num = 5;
        $display("\nTest %0d: Two RAND64 reads differ", test_num);
        trng_read(5'h08, rd);
        trng_read(5'h08, rd2);
        check_ne("RAND64 consecutive", rd, rd2);

        // ================================================================
        // Test 6: SEED register write (smoke test — no crash)
        // ================================================================
        test_num = 6;
        $display("\nTest %0d: SEED write smoke", test_num);
        trng_write(5'h18, 64'hDEADBEEFCAFEBABE);
        repeat(4) @(posedge clk);
        // If we get here, it didn't crash
        $display("  PASS: SEED write completed without hang");
        pass_count = pass_count + 1;

        // ================================================================
        // Test 7: RAND8 value changes after SEED injection
        // ================================================================
        test_num = 7;
        $display("\nTest %0d: RAND8 varies after SEED", test_num);
        trng_read(5'h00, rd);
        trng_write(5'h18, 64'h1234567890ABCDEF);
        repeat(4) @(posedge clk);
        trng_read(5'h00, rd2);
        // Values should differ (counter changes between reads too)
        check_ne("RAND8 before/after seed", rd, rd2);

        // ================================================================
        // Test 8: Multiple RAND8 reads are not all identical
        //   Read 8 values; at least 2 must be distinct (overwhelming
        //   probability with a properly working TRNG).
        // ================================================================
        test_num = 8;
        $display("\nTest %0d: RAND8 diversity (8 reads)", test_num);
        begin : rand8_diversity
            reg [7:0] vals [0:7];
            integer distinct;
            integer j;

            for (i = 0; i < 8; i = i + 1) begin
                trng_read(5'h00, rd);
                vals[i] = rd[7:0];
            end

            distinct = 1;
            for (i = 1; i < 8; i = i + 1) begin
                for (j = 0; j < i; j = j + 1) begin
                    if (vals[i] != vals[j])
                        distinct = distinct + 1;
                end
            end

            if (distinct > 1) begin
                $display("  PASS: %0d distinct values among 8 RAND8 reads", distinct);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: All 8 RAND8 reads identical (%02h)", vals[0]);
                fail_count = fail_count + 1;
            end
        end

        // ================================================================
        // Test 9: Health monitor — STATUS remains 1 under normal operation
        //   After many reads, STATUS should still be valid (no health
        //   alarm from the repetition count test).
        // ================================================================
        test_num = 9;
        $display("\nTest %0d: Health OK after sustained reads", test_num);
        // Perform 100 reads to exercise the health monitor
        for (i = 0; i < 100; i = i + 1) begin
            trng_read(5'h08, rd);
        end
        trng_read(5'h10, rd);
        check("STATUS after 100 reads", rd, 64'd1);

        // ================================================================
        // Summary
        // ================================================================
        $display("\n========================================");
        $display("  TRNG Tests: %0d PASSED, %0d FAILED", pass_count, fail_count);
        $display("========================================");
        $finish;
    end

    // Watchdog timer
    initial begin
        #200000;
        $display("TIMEOUT: TestBench exceeded 200us");
        $finish;
    end

endmodule
