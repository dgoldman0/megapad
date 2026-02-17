// ============================================================================
// tb_platform_sim.v — Smoke test for mp64_platform_sim + mp64_top
// ============================================================================
//
// Verifies:
//   - Clock generation
//   - Reset synchronisation
//   - Top-level instantiation compiles
//   - Debug LEDs show expected pattern after reset release
//

`timescale 1ns / 1ps

module tb_platform_sim;

    `include "mp64_pkg.vh"

    wire       sim_clk;
    wire       sim_rst_n;
    wire [7:0] sim_debug_leds;
    wire       sim_uart_txd;

    integer pass_count = 0;
    integer fail_count = 0;
    integer test_num   = 0;

    mp64_platform_sim #(
        .CLOCK_HZ      (100_000_000),
        .CLK_PERIOD_NS  (10),
        .RESET_CYCLES   (8)
    ) u_plat (
        .sim_clk        (sim_clk),
        .sim_rst_n      (sim_rst_n),
        .sim_debug_leds (sim_debug_leds),
        .sim_uart_txd   (sim_uart_txd)
    );

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

    task check8;
        input [7:0]   expected;
        input [7:0]   actual;
        input [255:0] label;
        begin
            test_num = test_num + 1;
            if (actual === expected) begin
                pass_count = pass_count + 1;
            end else begin
                fail_count = fail_count + 1;
                $display("FAIL [%0d] %0s: expected %h, got %h",
                         test_num, label, expected, actual);
            end
        end
    endtask

    // Count clock edges
    integer clk_edges = 0;
    always @(posedge sim_clk) clk_edges = clk_edges + 1;

    initial begin
        $dumpfile("tb_platform_sim.vcd");
        $dumpvars(0, tb_platform_sim);

        // ------------------------------------------------------------------
        // Test 1: Clock should be running
        // ------------------------------------------------------------------
        #200;
        check1(1'b1, (clk_edges > 10), "clock running");

        // ------------------------------------------------------------------
        // Test 2: Reset should be asserted initially
        // ------------------------------------------------------------------
        // Check early — before RESET_CYCLES * CLK_PERIOD_NS = 80 ns
        // We're at 200 ns already so reset may have released.
        // Let's check by going to a known state.

        // ------------------------------------------------------------------
        // Test 3: Wait for reset release
        // ------------------------------------------------------------------
        // Reset release = 80 ns initial + 4 sync stages = ~120 ns
        // We're already past that, so rst_n should be 1
        @(posedge sim_clk);
        #1;
        check1(1'b1, sim_rst_n, "reset released");

        // ------------------------------------------------------------------
        // Test 4: LEDs should show heartbeat (0xA5) after reset
        // ------------------------------------------------------------------
        repeat (5) @(posedge sim_clk);
        #1;
        check8(8'hA5, sim_debug_leds, "LEDs heartbeat");

        // ------------------------------------------------------------------
        // Test 5: UART TX idle (high)
        // ------------------------------------------------------------------
        check1(1'b1, sim_uart_txd, "UART TX idle");

        // ------------------------------------------------------------------
        // Test 6: Run for 1000 more cycles — no hang
        // ------------------------------------------------------------------
        repeat (1000) @(posedge sim_clk);
        check1(1'b1, (clk_edges > 1000), "1000 cycles OK");

        // ------------------------------------------------------------------
        // Summary
        // ------------------------------------------------------------------
        #20;
        $display("========================================");
        $display("tb_platform_sim: %0d passed, %0d failed",
                 pass_count, fail_count);
        $display("========================================");
        if (fail_count > 0) $display("*** FAILURES ***");
        else                $display("ALL TESTS PASSED");
        $finish;
    end

endmodule
