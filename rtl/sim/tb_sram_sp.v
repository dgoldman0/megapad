// ============================================================================
// tb_sram_sp.v — Testbench for mp64_sram_sp
// ============================================================================

`timescale 1ns / 1ps

module tb_sram_sp;

    parameter ADDR_W = 4;
    parameter DATA_W = 64;
    parameter DEPTH  = (1 << ADDR_W);

    reg                clk;
    reg                rst_n;
    reg                ce;
    reg                we;
    reg  [ADDR_W-1:0]  addr;
    reg  [DATA_W-1:0]  wdata;
    wire [DATA_W-1:0]  rdata;

    integer pass_count = 0;
    integer fail_count = 0;
    integer test_num   = 0;

    // DUT — no output register
    mp64_sram_sp #(
        .ADDR_W  (ADDR_W),
        .DATA_W  (DATA_W),
        .OUT_REG (0)
    ) u_dut (
        .clk   (clk),
        .rst_n (rst_n),
        .ce    (ce),
        .we    (we),
        .addr  (addr),
        .wdata (wdata),
        .rdata (rdata)
    );

    // DUT2 — with output register
    wire [DATA_W-1:0] rdata2;
    mp64_sram_sp #(
        .ADDR_W  (ADDR_W),
        .DATA_W  (DATA_W),
        .OUT_REG (1)
    ) u_dut2 (
        .clk   (clk),
        .rst_n (rst_n),
        .ce    (ce),
        .we    (we),
        .addr  (addr),
        .wdata (wdata),
        .rdata (rdata2)
    );

    // Clock
    initial clk = 0;
    always #5 clk = ~clk;

    task check;
        input [DATA_W-1:0] expected;
        input [DATA_W-1:0] actual;
        input [255:0]      label;
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

    integer i;

    initial begin
        $dumpfile("tb_sram_sp.vcd");
        $dumpvars(0, tb_sram_sp);

        // Reset
        rst_n = 0; ce = 0; we = 0; addr = 0; wdata = 0;
        #20; rst_n = 1;
        @(negedge clk);

        // ------------------------------------------------------------------
        // Test 1: Write all locations (drive on negedge, sampled on posedge)
        // ------------------------------------------------------------------
        for (i = 0; i < DEPTH; i = i + 1) begin
            @(negedge clk);
            ce = 1; we = 1; addr = i; wdata = 64'hA000_0000_0000_0000 + i;
        end
        @(negedge clk);
        ce = 0; we = 0;

        // ------------------------------------------------------------------
        // Test 2: Read back all locations
        // ------------------------------------------------------------------
        for (i = 0; i < DEPTH; i = i + 1) begin
            @(negedge clk);
            ce = 1; we = 0; addr = i;
            @(posedge clk); #1;  // data available after posedge
            check(64'hA000_0000_0000_0000 + i, rdata,
                  "SP read-back");
        end
        @(negedge clk);
        ce = 0;

        // ------------------------------------------------------------------
        // Test 3: Overwrite and verify
        // ------------------------------------------------------------------
        @(negedge clk);
        ce = 1; we = 1; addr = 5; wdata = 64'hDEAD_BEEF_CAFE_BABE;
        @(negedge clk);
        ce = 1; we = 0; addr = 5;
        @(posedge clk); #1;
        check(64'hDEAD_BEEF_CAFE_BABE, rdata, "SP overwrite");

        // ------------------------------------------------------------------
        // Test 4: CE low — output should hold
        // ------------------------------------------------------------------
        @(negedge clk);
        ce = 0; we = 0; addr = 0;
        @(posedge clk); #1;
        check(64'hDEAD_BEEF_CAFE_BABE, rdata, "SP CE-low hold");

        // ------------------------------------------------------------------
        // Summary
        // ------------------------------------------------------------------
        #20;
        $display("========================================");
        $display("tb_sram_sp: %0d passed, %0d failed", pass_count, fail_count);
        $display("========================================");
        if (fail_count > 0) $display("*** FAILURES ***");
        else                $display("ALL TESTS PASSED");
        $finish;
    end

endmodule
