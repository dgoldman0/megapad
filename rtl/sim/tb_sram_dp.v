// ============================================================================
// tb_sram_dp.v — Testbench for mp64_sram_dp
// ============================================================================

`timescale 1ns / 1ps

module tb_sram_dp;

    // Configure as tile (512b) + CPU (64b) — the real use case
    parameter ADDR_W_A = 4;   // 16 rows of 512 bits
    parameter DATA_W_A = 512;
    parameter ADDR_W_B = 7;   // 128 words of 64 bits (16 rows × 8 words)
    parameter DATA_W_B = 64;
    parameter DEPTH_A  = (1 << ADDR_W_A);

    reg                    clk;
    reg                    rst_n;

    // Port A
    reg                    a_ce, a_we;
    reg  [ADDR_W_A-1:0]   a_addr;
    reg  [DATA_W_A-1:0]   a_wdata;
    wire [DATA_W_A-1:0]   a_rdata;

    // Port B
    reg                    b_ce, b_we;
    reg  [ADDR_W_B-1:0]   b_addr;
    reg  [DATA_W_B-1:0]   b_wdata;
    wire [DATA_W_B-1:0]   b_rdata;

    integer pass_count = 0;
    integer fail_count = 0;
    integer test_num   = 0;

    mp64_sram_dp #(
        .ADDR_W_A  (ADDR_W_A),
        .DATA_W_A  (DATA_W_A),
        .ADDR_W_B  (ADDR_W_B),
        .DATA_W_B  (DATA_W_B),
        .DEPTH_A   (DEPTH_A)
    ) u_dut (
        .clk     (clk),
        .rst_n   (rst_n),
        .a_ce    (a_ce),
        .a_we    (a_we),
        .a_addr  (a_addr),
        .a_wdata (a_wdata),
        .a_rdata (a_rdata),
        .b_ce    (b_ce),
        .b_we    (b_we),
        .b_addr  (b_addr),
        .b_wdata (b_wdata),
        .b_rdata (b_rdata)
    );

    initial clk = 0;
    always #5 clk = ~clk;

    task check64;
        input [63:0]  expected;
        input [63:0]  actual;
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

    task check512;
        input [511:0] expected;
        input [511:0] actual;
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

    reg [511:0] test_row;
    integer i, w;

    initial begin
        $dumpfile("tb_sram_dp.vcd");
        $dumpvars(0, tb_sram_dp);

        rst_n = 0;
        a_ce = 0; a_we = 0; a_addr = 0; a_wdata = 0;
        b_ce = 0; b_we = 0; b_addr = 0; b_wdata = 0;
        #20; rst_n = 1;
        @(negedge clk);

        // ------------------------------------------------------------------
        // Test 1: Write via Port A (wide), read via Port A
        // ------------------------------------------------------------------
        test_row = 512'h0;
        for (w = 0; w < 8; w = w + 1)
            test_row[w*64 +: 64] = 64'hCAFE_0000_0000_0000 + w;

        @(negedge clk);
        a_ce = 1; a_we = 1; a_addr = 4'd3; a_wdata = test_row;
        @(negedge clk);
        a_we = 0; a_addr = 4'd3;
        @(posedge clk); #1;
        a_ce = 0;
        check512(test_row, a_rdata, "DP A write/A read");

        // ------------------------------------------------------------------
        // Test 2: Read same row via Port B (narrow) — each sub-word
        // ------------------------------------------------------------------
        for (w = 0; w < 8; w = w + 1) begin
            @(negedge clk);
            b_ce = 1; b_we = 0; b_addr = {4'd3, w[2:0]};
            @(posedge clk); #1;
            check64(64'hCAFE_0000_0000_0000 + w, b_rdata,
                    "DP B read sub-word");
        end
        @(negedge clk); b_ce = 0;

        // ------------------------------------------------------------------
        // Test 3: Write via Port B (narrow), read via Port A (wide)
        // ------------------------------------------------------------------
        @(negedge clk);
        b_ce = 1; b_we = 1; b_addr = {4'd3, 3'd5};
        b_wdata = 64'hBEEF_DEAD_1234_5678;
        @(negedge clk);
        b_ce = 0; b_we = 0;

        // Read the full row via Port A
        @(negedge clk);
        a_ce = 1; a_we = 0; a_addr = 4'd3;
        @(posedge clk); #1;
        a_ce = 0;
        // Word 5 should be updated, others unchanged
        check64(64'hBEEF_DEAD_1234_5678, a_rdata[5*64 +: 64],
                "DP B write / A read w5");
        check64(64'hCAFE_0000_0000_0004, a_rdata[4*64 +: 64],
                "DP B write / A read w4 intact");

        // ------------------------------------------------------------------
        // Test 4: Write via Port B, read via Port B
        // ------------------------------------------------------------------
        @(negedge clk);
        b_ce = 1; b_we = 1; b_addr = {4'd0, 3'd0};
        b_wdata = 64'h1111_2222_3333_4444;
        @(negedge clk);
        b_we = 0; b_addr = {4'd0, 3'd0};
        @(posedge clk); #1;
        b_ce = 0;
        check64(64'h1111_2222_3333_4444, b_rdata, "DP B write/B read");

        // ------------------------------------------------------------------
        // Test 5: Concurrent A-read and B-read (different rows)
        // ------------------------------------------------------------------
        // First write row 1 via A
        test_row = {8{64'hAAAA_BBBB_CCCC_DDDD}};
        @(negedge clk);
        a_ce = 1; a_we = 1; a_addr = 4'd1; a_wdata = test_row;
        @(negedge clk);
        a_we = 0;
        // Concurrent: A reads row 1, B reads row 0 word 0
        a_ce = 1; a_addr = 4'd1;
        b_ce = 1; b_we = 0; b_addr = {4'd0, 3'd0};
        @(posedge clk); #1;
        a_ce = 0; b_ce = 0;
        check512(test_row, a_rdata, "DP concurrent A-rd row1");
        check64(64'h1111_2222_3333_4444, b_rdata, "DP concurrent B-rd row0");

        // ------------------------------------------------------------------
        // Summary
        // ------------------------------------------------------------------
        #20;
        $display("========================================");
        $display("tb_sram_dp: %0d passed, %0d failed", pass_count, fail_count);
        $display("========================================");
        if (fail_count > 0) $display("*** FAILURES ***");
        else                $display("ALL TESTS PASSED");
        $finish;
    end

endmodule
