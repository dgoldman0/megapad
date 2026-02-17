// ============================================================================
// tb_rst_sync.v — Testbench for mp64_rst_sync
// ============================================================================

`timescale 1ns / 1ps

module tb_rst_sync;

    parameter SYNC_STAGES = 4;

    reg  clk;
    reg  rst_n_in;
    wire rst_n_out;

    integer pass_count = 0;
    integer fail_count = 0;
    integer test_num   = 0;

    mp64_rst_sync #(
        .SYNC_STAGES (SYNC_STAGES)
    ) u_dut (
        .clk       (clk),
        .rst_n_in  (rst_n_in),
        .rst_n_out (rst_n_out)
    );

    initial clk = 0;
    always #5 clk = ~clk;

    task check1;
        input         expected;
        input         actual;
        input [255:0] label;
        begin
            test_num = test_num + 1;
            if (actual === expected) begin
                pass_count = pass_count + 1;
            end else begin
                fail_count = fail_count + 1;
                $display("FAIL [%0d] %0s: expected %b, got %b",
                         test_num, label, expected, actual);
            end
        end
    endtask

    integer i;

    initial begin
        $dumpfile("tb_rst_sync.vcd");
        $dumpvars(0, tb_rst_sync);

        // ------------------------------------------------------------------
        // Test 1: Async assertion — rst_n_out goes low immediately
        // ------------------------------------------------------------------
        rst_n_in = 1'b0;
        #2;  // within same cycle, no clock edge needed
        check1(1'b0, rst_n_out, "async assert immediate");

        // ------------------------------------------------------------------
        // Test 2: Output stays low while input is low
        // ------------------------------------------------------------------
        #50;  // several clock cycles
        check1(1'b0, rst_n_out, "stays low during reset");

        // ------------------------------------------------------------------
        // Test 3: Sync de-assert — takes SYNC_STAGES clock cycles
        // ------------------------------------------------------------------
        // Release reset asynchronously
        rst_n_in = 1'b1;

        // Should still be low for SYNC_STAGES-1 cycles
        for (i = 0; i < SYNC_STAGES - 1; i = i + 1) begin
            @(posedge clk);
            #1;
            check1(1'b0, rst_n_out, "de-assert: still low");
        end

        // After SYNC_STAGES clocks, should go high
        @(posedge clk);
        #1;
        check1(1'b1, rst_n_out, "de-assert: goes high");

        // ------------------------------------------------------------------
        // Test 4: Confirm stable high
        // ------------------------------------------------------------------
        @(posedge clk); #1;
        check1(1'b1, rst_n_out, "stable high 1");
        @(posedge clk); #1;
        check1(1'b1, rst_n_out, "stable high 2");

        // ------------------------------------------------------------------
        // Test 5: Re-assert — immediate async
        // ------------------------------------------------------------------
        // Assert mid-cycle (between clock edges)
        @(posedge clk);
        #3;
        rst_n_in = 1'b0;
        #1;
        check1(1'b0, rst_n_out, "re-assert immediate");

        // ------------------------------------------------------------------
        // Test 6: Short glitch — assert for less than 1 cycle
        // ------------------------------------------------------------------
        #40; rst_n_in = 1'b1;  // release
        // Wait for full de-assertion
        repeat (SYNC_STAGES + 1) @(posedge clk);
        #1;
        check1(1'b1, rst_n_out, "post-glitch: recovered");

        // Apply short glitch
        @(posedge clk);
        #2;
        rst_n_in = 1'b0;
        #3;  // less than half a clock period
        rst_n_in = 1'b1;
        #1;
        // Output should have gone low during glitch
        check1(1'b0, rst_n_out, "glitch: caught");
        // Wait for sync recovery
        repeat (SYNC_STAGES + 1) @(posedge clk);
        #1;
        check1(1'b1, rst_n_out, "glitch: recovered");

        // ------------------------------------------------------------------
        // Summary
        // ------------------------------------------------------------------
        #20;
        $display("========================================");
        $display("tb_rst_sync: %0d passed, %0d failed", pass_count, fail_count);
        $display("========================================");
        if (fail_count > 0) $display("*** FAILURES ***");
        else                $display("ALL TESTS PASSED");
        $finish;
    end

endmodule
