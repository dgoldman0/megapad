// ============================================================================
// tb_clkgate.v — Testbench for mp64_clkgate
// ============================================================================

`timescale 1ns / 1ps

module tb_clkgate;

    reg  clk_in;
    reg  enable;
    reg  test_en;
    wire clk_out;

    integer pass_count = 0;
    integer fail_count = 0;
    integer test_num   = 0;

    mp64_clkgate u_dut (
        .clk_in  (clk_in),
        .enable  (enable),
        .test_en (test_en),
        .clk_out (clk_out)
    );

    // Free-running clock
    initial clk_in = 0;
    always #5 clk_in = ~clk_in;

    // Count output clock edges
    integer edge_count;
    always @(posedge clk_out) edge_count = edge_count + 1;

    task check_i;
        input integer expected;
        input integer actual;
        input [255:0] label;
        begin
            test_num = test_num + 1;
            if (actual === expected) begin
                pass_count = pass_count + 1;
            end else begin
                fail_count = fail_count + 1;
                $display("FAIL [%0d] %0s: expected %0d, got %0d",
                         test_num, label, expected, actual);
            end
        end
    endtask

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

    initial begin
        $dumpfile("tb_clkgate.vcd");
        $dumpvars(0, tb_clkgate);

        enable = 0; test_en = 0; edge_count = 0;

        // ------------------------------------------------------------------
        // Test 1: Clock disabled — no edges for 5 cycles
        // ------------------------------------------------------------------
        #100;
        check_i(0, edge_count, "disabled: zero edges");

        // ------------------------------------------------------------------
        // Test 2: Enable clock — should see edges
        // ------------------------------------------------------------------
        edge_count = 0;
        // Enable on falling edge of clk_in to avoid glitch
        @(negedge clk_in);
        enable = 1;
        #100;  // 10 full cycles at 100 MHz
        check1(1'b1, (edge_count >= 9), "enabled: edges flowing");

        // ------------------------------------------------------------------
        // Test 3: Disable mid-run
        // ------------------------------------------------------------------
        @(negedge clk_in);
        enable = 0;
        edge_count = 0;
        #100;
        check_i(0, edge_count, "re-disabled: zero edges");

        // ------------------------------------------------------------------
        // Test 4: clk_out should be low when gated
        // ------------------------------------------------------------------
        #2;
        check1(1'b0, clk_out, "gated: clk_out low");

        // ------------------------------------------------------------------
        // Test 5: test_en overrides enable
        // ------------------------------------------------------------------
        edge_count = 0;
        @(negedge clk_in);
        test_en = 1;
        #100;
        check1(1'b1, (edge_count >= 9), "test_en override");

        // Release test_en
        @(negedge clk_in);
        test_en = 0;
        edge_count = 0;
        #100;
        check_i(0, edge_count, "test_en released");

        // ------------------------------------------------------------------
        // Test 6: Glitch-free — enable changes during clk_in=1
        //         should not produce a glitch (latch catches on neg edge)
        // ------------------------------------------------------------------
        // Wait for rising edge, then change enable while high
        @(posedge clk_in);
        #2;  // mid-high
        enable = 1;
        // clk_out should still be 0 because latch hasn't opened yet
        #1;
        check1(1'b0, clk_out, "no-glitch: enable mid-high");
        // After falling edge, latch captures
        @(negedge clk_in);
        #1;
        // clk_out should still be 0 (clk_in is low)
        check1(1'b0, clk_out, "latch captured, clk low");
        // After next rising edge, clk_out goes high
        @(posedge clk_in);
        #1;
        check1(1'b1, clk_out, "next posedge: clk_out high");

        // ------------------------------------------------------------------
        // Summary
        // ------------------------------------------------------------------
        @(negedge clk_in); enable = 0;
        #20;
        $display("========================================");
        $display("tb_clkgate: %0d passed, %0d failed", pass_count, fail_count);
        $display("========================================");
        if (fail_count > 0) $display("*** FAILURES ***");
        else                $display("ALL TESTS PASSED");
        $finish;
    end

endmodule
