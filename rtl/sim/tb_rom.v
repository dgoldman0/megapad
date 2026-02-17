// ============================================================================
// tb_rom.v — Testbench for mp64_rom
// ============================================================================

`timescale 1ns / 1ps

module tb_rom;

    parameter ADDR_W = 4;
    parameter DATA_W = 32;
    parameter DEPTH  = (1 << ADDR_W);

    reg                clk;
    reg                rst_n;
    reg                ce;
    reg  [ADDR_W-1:0]  addr;
    wire [DATA_W-1:0]  rdata;

    integer pass_count = 0;
    integer fail_count = 0;
    integer test_num   = 0;

    mp64_rom #(
        .ADDR_W    (ADDR_W),
        .DATA_W    (DATA_W),
        .INIT_FILE ("test_rom.hex")
    ) u_dut (
        .clk   (clk),
        .rst_n (rst_n),
        .ce    (ce),
        .addr  (addr),
        .rdata (rdata)
    );

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

    // Expected ROM contents
    reg [DATA_W-1:0] expected_data [0:15];
    initial begin
        expected_data[0]  = 32'hDEADBEEF;
        expected_data[1]  = 32'hCAFEBABE;
        expected_data[2]  = 32'h01234567;
        expected_data[3]  = 32'h89ABCDEF;
        expected_data[4]  = 32'h00000000;
        expected_data[5]  = 32'hFFFFFFFF;
        expected_data[6]  = 32'hA5A5A5A5;
        expected_data[7]  = 32'h5A5A5A5A;
        expected_data[8]  = 32'h11111111;
        expected_data[9]  = 32'h22222222;
        expected_data[10] = 32'h33333333;
        expected_data[11] = 32'h44444444;
        expected_data[12] = 32'h55555555;
        expected_data[13] = 32'h66666666;
        expected_data[14] = 32'h77777777;
        expected_data[15] = 32'h88888888;
    end

    integer i;

    initial begin
        $dumpfile("tb_rom.vcd");
        $dumpvars(0, tb_rom);

        rst_n = 0; ce = 0; addr = 0;
        #20; rst_n = 1;
        @(negedge clk);

        // ------------------------------------------------------------------
        // Test 1: Read all ROM locations
        // ------------------------------------------------------------------
        for (i = 0; i < DEPTH; i = i + 1) begin
            @(negedge clk);
            ce = 1; addr = i;
            @(posedge clk); #1;
            check(expected_data[i], rdata, "ROM read");
        end
        @(negedge clk);
        ce = 0;

        // ------------------------------------------------------------------
        // Test 2: Sequential reads without CE gap
        //   Pipelined: addr presented on negedge, result available
        //   after the following posedge.
        // ------------------------------------------------------------------
        @(negedge clk);
        ce = 1; addr = 0;
        // posedge: DUT reads mem[0]
        @(posedge clk); #1;
        check(expected_data[0], rdata, "ROM seq-rd t0");
        @(negedge clk);
        addr = 1;
        // posedge: DUT reads mem[1]
        @(posedge clk); #1;
        check(expected_data[1], rdata, "ROM seq-rd t1");
        @(negedge clk);
        addr = 2;
        // posedge: DUT reads mem[2]
        @(posedge clk); #1;
        check(expected_data[2], rdata, "ROM seq-rd t2");
        @(negedge clk);
        ce = 0;

        // ------------------------------------------------------------------
        // Test 3: CE low — output holds
        // ------------------------------------------------------------------
        @(posedge clk); #1;
        check(expected_data[2], rdata, "ROM CE-low hold");

        // ------------------------------------------------------------------
        // Summary
        // ------------------------------------------------------------------
        #20;
        $display("========================================");
        $display("tb_rom: %0d passed, %0d failed", pass_count, fail_count);
        $display("========================================");
        if (fail_count > 0) $display("*** FAILURES ***");
        else                $display("ALL TESTS PASSED");
        $finish;
    end

endmodule
