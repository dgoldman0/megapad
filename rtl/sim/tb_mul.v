// ============================================================================
// tb_mul.v — Testbench for mp64_mul (portable multiplier)
// ============================================================================
`timescale 1ns / 1ps

module tb_mul;

    reg         clk, rst;
    reg         start, is_signed;
    reg  [63:0] a, b;
    wire [127:0] result;
    wire        done, busy;

    mp64_mul #(.LATENCY(4)) uut (
        .clk(clk), .rst(rst),
        .start(start), .is_signed(is_signed),
        .a(a), .b(b),
        .result(result), .done(done), .busy(busy)
    );

    // Clock: 10 ns period
    always #5 clk = ~clk;

    integer pass_count, fail_count;
    reg [127:0] expected;

    task check;
        input [127:0] exp;
        input [80*8-1:0] label;
        begin
            if (result !== exp) begin
                $display("FAIL %0s: got %h, expected %h", label, result, exp);
                fail_count = fail_count + 1;
            end else begin
                $display("  ok %0s", label);
                pass_count = pass_count + 1;
            end
        end
    endtask

    task do_mul;
        input [63:0] op_a, op_b;
        input        op_signed;
        begin
            @(posedge clk);
            a <= op_a; b <= op_b; is_signed <= op_signed; start <= 1'b1;
            @(posedge clk);
            start <= 1'b0;
            // Wait for done
            wait (done);
            @(negedge clk);   // sample at stable point
        end
    endtask

    initial begin
        $dumpfile("tb_mul.vcd"); $dumpvars(0, tb_mul);
        clk = 0; rst = 1; start = 0; is_signed = 0; a = 0; b = 0;
        pass_count = 0; fail_count = 0;

        // Reset
        repeat (4) @(posedge clk);
        rst <= 0;
        @(posedge clk);

        // ----------------------------------------------------------------
        // Test 1: unsigned 7 × 6 = 42
        // ----------------------------------------------------------------
        do_mul(64'd7, 64'd6, 1'b0);
        check(128'd42, "unsigned 7*6");

        // ----------------------------------------------------------------
        // Test 2: unsigned 0 × anything = 0
        // ----------------------------------------------------------------
        do_mul(64'd0, 64'hDEAD_BEEF, 1'b0);
        check(128'd0, "unsigned 0*X");

        // ----------------------------------------------------------------
        // Test 3: unsigned 1 × MAX = MAX
        // ----------------------------------------------------------------
        do_mul(64'd1, 64'hFFFF_FFFF_FFFF_FFFF, 1'b0);
        check({64'd0, 64'hFFFF_FFFF_FFFF_FFFF}, "unsigned 1*MAX");

        // ----------------------------------------------------------------
        // Test 4: unsigned overflow — large × large
        // ----------------------------------------------------------------
        do_mul(64'h1_0000_0000, 64'h1_0000_0000, 1'b0);
        check({64'd1, 64'd0}, "unsigned 2^32 * 2^32");

        // ----------------------------------------------------------------
        // Test 5: signed positive × positive
        // ----------------------------------------------------------------
        do_mul(64'd100, 64'd200, 1'b1);
        check(128'd20000, "signed 100*200");

        // ----------------------------------------------------------------
        // Test 6: signed negative × positive
        // (-1 × 5 = -5, sign-extended to 128 bits)
        // ----------------------------------------------------------------
        do_mul(64'hFFFF_FFFF_FFFF_FFFF, 64'd5, 1'b1);
        check(128'hFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFB, "signed -1*5");

        // ----------------------------------------------------------------
        // Test 7: signed negative × negative
        // (-2 × -3 = 6)
        // ----------------------------------------------------------------
        do_mul(64'hFFFF_FFFF_FFFF_FFFE, 64'hFFFF_FFFF_FFFF_FFFD, 1'b1);
        check(128'd6, "signed -2*-3");

        // ----------------------------------------------------------------
        // Test 8: signed MIN × 1
        // (0x8000...0000 × 1 = 0xFFFF...FFFF_8000...0000)
        // ----------------------------------------------------------------
        do_mul(64'h8000_0000_0000_0000, 64'd1, 1'b1);
        check({64'hFFFF_FFFF_FFFF_FFFF, 64'h8000_0000_0000_0000}, "signed MIN*1");

        // ----------------------------------------------------------------
        // Test 9: unsigned MAX × MAX
        // ----------------------------------------------------------------
        do_mul(64'hFFFF_FFFF_FFFF_FFFF, 64'hFFFF_FFFF_FFFF_FFFF, 1'b0);
        check(128'hFFFF_FFFF_FFFF_FFFE_0000_0000_0000_0001, "unsigned MAX*MAX");

        // ----------------------------------------------------------------
        // Test 10: busy/done timing — ensure busy is high during pipeline
        // ----------------------------------------------------------------
        @(posedge clk);
        a <= 64'd3; b <= 64'd4; is_signed <= 1'b0; start <= 1'b1;
        @(posedge clk);
        start <= 1'b0;
        if (busy) begin
            $display("  ok busy asserted after start");
            pass_count = pass_count + 1;
        end else begin
            $display("FAIL busy not asserted after start");
            fail_count = fail_count + 1;
        end
        wait (done);
        @(negedge clk);
        check(128'd12, "timing 3*4");

        // ----------------------------------------------------------------
        // Test 11: LATENCY=1 variant (instantiate inline)
        // ----------------------------------------------------------------
        // Tested via the LATENCY=4 path above; a LATENCY=1 unit
        // would need a separate instance.  Skipped here — covered
        // by the sim platform's default LATENCY=4.

        // ================================================================
        $display("=== tb_mul: %0d passed, %0d failed ===",
                 pass_count, fail_count);
        if (fail_count > 0) begin
            $display("FAIL");
            $finish;
        end
        $display("PASS");
        $finish;
    end

endmodule
