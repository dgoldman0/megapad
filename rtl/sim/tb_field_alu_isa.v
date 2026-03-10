// ============================================================================
// tb_field_alu_isa.v — Testbench for mp64_field_alu_isa (Per-Core Field ALU)
// ============================================================================
//
// Tests the ISA-path field ALU engine (14 sub-ops + multi-prime + REDC).
//
//   1. GF.ADD (3+5)=8              (Curve25519)
//   2. GF.SUB (5-3)=2
//   3. GF.SUB wraparound (3-5)=p-2
//   4. GF.MUL (7*11)=77
//   5. GF.SQR (9^2)=81
//   6. GF.INV (2^-1)=(p+1)/2
//   7. GF.POW (2^10)=1024
//   8. GF.MULR (100*200)=20000 lo, 0 hi
//   9. GF.CEQ equal → 1
//  10. GF.CEQ not-equal → 0
//  11. GF.CMOV cond=1 → select a
//  12. GF.CMOV cond=0 → keep old
//  13. GF.MAC accumulate 3*5+7*11=92
//  14. GF.MACR raw accumulate 10*20+30*40=1400
//  15. GF.LDPRIME (latch custom prime)
//  16–18. secp256k1: FADD, FSUB wrap, FMUL
//  19–21. P-256: FADD, FSUB wrap, FMUL
//  22–24. Custom prime (65537): FMUL(100,200)=20000, FMUL overflow
//  25–27. Montgomery REDC: FMUL, FSQR, round-trip
//

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_field_alu_isa;

    reg         clk;
    reg         rst_n;
    initial clk = 0;
    always #5 clk = ~clk;  // 100 MHz

    integer pass_count = 0;
    integer fail_count = 0;
    integer test_num   = 0;

    // DUT signals
    reg          start;
    reg  [3:0]   op;
    reg  [63:0]  rd_val;
    reg  [7:0]   imm8;
    reg  [255:0] acc_in;
    reg  [255:0] tile_b;
    reg  [1:0]   prime_sel;
    reg  [255:0] custom_p;
    reg  [255:0] mont_pinv;

    wire [255:0] acc_out;
    wire         acc_we;
    wire [255:0] tile_dst_out;
    wire         tile_dst_we;
    wire [1:0]   prime_sel_out;
    wire         prime_sel_we;
    wire [255:0] ldprime_p_out;
    wire [255:0] ldprime_pinv_out;
    wire         ldprime_we;
    wire         flag_z;
    wire         flag_z_we;
    wire         busy;
    wire         done;

    mp64_field_alu_isa uut (
        .clk             (clk),
        .rst_n           (rst_n),
        .start           (start),
        .op              (op),
        .rd_val          (rd_val),
        .imm8            (imm8),
        .acc_in          (acc_in),
        .tile_b          (tile_b),
        .prime_sel       (prime_sel),
        .custom_p        (custom_p),
        .mont_pinv       (mont_pinv),
        .acc_out         (acc_out),
        .acc_we          (acc_we),
        .tile_dst_out    (tile_dst_out),
        .tile_dst_we     (tile_dst_we),
        .prime_sel_out   (prime_sel_out),
        .prime_sel_we    (prime_sel_we),
        .ldprime_p_out   (ldprime_p_out),
        .ldprime_pinv_out(ldprime_pinv_out),
        .ldprime_we      (ldprime_we),
        .flag_z          (flag_z),
        .flag_z_we       (flag_z_we),
        .busy            (busy),
        .done            (done)
    );

    // ========================================================================
    // Constants
    // ========================================================================
    localparam [255:0] PRIME_25519 = 256'h7FFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFED;
    localparam [255:0] PRIME_SECP  = 256'hFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFE_FFFFFC2F;
    localparam [255:0] PRIME_P256  = 256'hFFFFFFFF_00000001_00000000_00000000_00000000_FFFFFFFF_FFFFFFFF_FFFFFFFF;

    // ========================================================================
    // Helper tasks
    // ========================================================================

    // Issue a start pulse and wait for done
    task run_op(input [3:0] t_op, input [255:0] t_acc, input [255:0] t_b,
                input [63:0] t_rd_val, input [7:0] t_imm8);
        integer timeout;
        begin
            @(posedge clk);
            start  <= 1'b1;
            op     <= t_op;
            acc_in <= t_acc;
            tile_b <= t_b;
            rd_val <= t_rd_val;
            imm8   <= t_imm8;
            @(posedge clk);
            start  <= 1'b0;
            // Wait for done
            timeout = 0;
            while (!done && timeout < 200000) begin
                @(posedge clk);
                timeout = timeout + 1;
            end
            if (timeout >= 200000) begin
                $display("  TIMEOUT waiting for done on op 0x%1h!", t_op);
                fail_count = fail_count + 1;
            end
        end
    endtask

    // Simple run with no rd_val/imm8
    task run_simple(input [3:0] t_op, input [255:0] t_acc, input [255:0] t_b);
        begin
            run_op(t_op, t_acc, t_b, 64'd0, 8'd0);
        end
    endtask

    // Check acc_out
    task check_acc(input [255:0] expected, input [8*50-1:0] name);
        begin
            test_num = test_num + 1;
            if (acc_out === expected) begin
                $display("  [PASS] Test %0d: %0s", test_num, name);
                pass_count = pass_count + 1;
            end else begin
                $display("  [FAIL] Test %0d: %0s", test_num, name);
                $display("         expected: %064h", expected);
                $display("              got: %064h", acc_out);
                fail_count = fail_count + 1;
            end
        end
    endtask

    // Check tile_dst_out
    task check_dst(input [255:0] expected, input [8*50-1:0] name);
        begin
            test_num = test_num + 1;
            if (tile_dst_out === expected) begin
                $display("  [PASS] Test %0d: %0s", test_num, name);
                pass_count = pass_count + 1;
            end else begin
                $display("  [FAIL] Test %0d: %0s", test_num, name);
                $display("         expected: %064h", expected);
                $display("              got: %064h", tile_dst_out);
                fail_count = fail_count + 1;
            end
        end
    endtask

    // ========================================================================
    // Test sequence
    // ========================================================================
    initial begin
        $dumpfile("tb_field_alu_isa.vcd");
        $dumpvars(0, tb_field_alu_isa);

        start     <= 0;
        op        <= 0;
        acc_in    <= 0;
        tile_b    <= 0;
        rd_val    <= 0;
        imm8      <= 0;
        prime_sel <= 2'd0;
        custom_p  <= 256'd0;
        mont_pinv <= 256'd0;

        rst_n = 0;
        #20;
        rst_n = 1;
        #10;

        $display("=== Field ALU ISA Tests ===");

        // ================================================================
        // Curve25519 basic ops (prime_sel = 0)
        // ================================================================

        // --- Test 1: GF.ADD (3+5) = 8 ---
        run_simple(4'h0, 256'd3, 256'd5);
        check_acc(256'd8, "GF.ADD(3,5)=8");

        // --- Test 2: GF.SUB (5-3) = 2 ---
        run_simple(4'h1, 256'd5, 256'd3);
        check_acc(256'd2, "GF.SUB(5,3)=2");

        // --- Test 3: GF.SUB wraparound (3-5) = p-2 ---
        run_simple(4'h1, 256'd3, 256'd5);
        check_acc(PRIME_25519 - 256'd2, "GF.SUB(3,5)=p-2");

        // --- Test 4: GF.MUL (7*11) = 77 ---
        run_simple(4'h2, 256'd7, 256'd11);
        check_acc(256'd77, "GF.MUL(7,11)=77");

        // --- Test 5: GF.SQR (9^2) = 81 ---
        run_simple(4'h3, 256'd9, 256'd0);
        check_acc(256'd81, "GF.SQR(9)=81");

        // --- Test 6: GF.INV (2^-1) = (p+1)/2 ---
        run_simple(4'h4, 256'd2, 256'd0);
        check_acc((PRIME_25519 + 256'd1) >> 1, "GF.INV(2)=(p+1)/2");

        // --- Test 7: GF.POW (2^10) = 1024 ---
        run_simple(4'h5, 256'd2, 256'd10);
        check_acc(256'd1024, "GF.POW(2,10)=1024");

        // --- Test 8: GF.MULR (100*200) lo=20000, hi=0 ---
        run_simple(4'h6, 256'd100, 256'd200);
        check_acc(256'd20000, "GF.MULR(100,200) LO=20000");
        check_dst(256'd0, "GF.MULR(100,200) HI=0");

        // --- Test 9: GF.CEQ equal → 1 ---
        run_simple(4'hA, 256'd12345, 256'd12345);
        check_acc(256'd1, "GF.CEQ equal -> 1");

        // --- Test 10: GF.CEQ not-equal → 0 ---
        run_simple(4'hA, 256'd12345, 256'd12346);
        check_acc(256'd0, "GF.CEQ not-equal -> 0");

        // --- Test 11: GF.CMOV cond=1 → select tile_b ---
        // First pre-load ACC with 42 via GF.ADD(42,0)
        run_simple(4'h0, 256'd42, 256'd0);
        // CMOV: rd_val=1 (non-zero), tile_b=99
        run_op(4'h9, 256'd42, 256'd99, 64'd1, 8'd0);
        check_acc(256'd99, "GF.CMOV cond=1 -> 99");

        // --- Test 12: GF.CMOV cond=0 → no write ---
        // ACC is still 99 from previous (we check acc_we is 0)
        run_op(4'h9, 256'd99, 256'd77, 64'd0, 8'd0);
        // acc_we should be 0 — acc_out is undefined but acc_we=0 means no write
        test_num = test_num + 1;
        if (acc_we === 1'b0) begin
            $display("  [PASS] Test %0d: GF.CMOV cond=0 -> no acc_we", test_num);
            pass_count = pass_count + 1;
        end else begin
            $display("  [FAIL] Test %0d: GF.CMOV cond=0 -> expected acc_we=0, got=%b", test_num, acc_we);
            fail_count = fail_count + 1;
        end

        // --- Test 13: GF.MAC accumulate: (3*5) + (7*11) = 15 + 77 = 92 ---
        // Zero prev_lo via GF.ADD(0,0)
        run_simple(4'h0, 256'd0, 256'd0);
        // MAC: prev(0) + 3*5 = 15
        run_simple(4'h7, 256'd3, 256'd5);
        // MAC: prev(15) + 7*11 = 92
        run_simple(4'h7, 256'd7, 256'd11);
        check_acc(256'd92, "GF.MAC 3*5+7*11=92");

        // --- Test 14: GF.MACR raw accumulate: 10*20+30*40=1400 ---
        // Zero prev via GF.MULR(0,0)
        run_simple(4'h6, 256'd0, 256'd0);
        // MACR: prev(0) + 10*20 = 200
        run_simple(4'h8, 256'd10, 256'd20);
        // MACR: prev(200) + 30*40 = 1400
        run_simple(4'h8, 256'd30, 256'd40);
        check_acc(256'd1400, "GF.MACR 10*20+30*40=1400");

        // --- Test 15: GF.LDPRIME ---
        run_op(4'hC, 256'd65537, 256'd123456789, 64'd0, 8'd0);
        test_num = test_num + 1;
        if (ldprime_we === 1'b1 && ldprime_p_out === 256'd65537 &&
            ldprime_pinv_out === 256'd123456789) begin
            $display("  [PASS] Test %0d: GF.LDPRIME latched", test_num);
            pass_count = pass_count + 1;
        end else begin
            $display("  [FAIL] Test %0d: GF.LDPRIME", test_num);
            fail_count = fail_count + 1;
        end

        // --- Test 16: GF.PRIME imm8 ---
        run_op(4'hB, 256'd0, 256'd0, 64'd0, 8'd2);
        test_num = test_num + 1;
        if (prime_sel_we === 1'b1 && prime_sel_out === 2'd2) begin
            $display("  [PASS] Test %0d: GF.PRIME imm8=2", test_num);
            pass_count = pass_count + 1;
        end else begin
            $display("  [FAIL] Test %0d: GF.PRIME", test_num);
            fail_count = fail_count + 1;
        end

        // ================================================================
        // secp256k1 tests (prime_sel = 1)
        // ================================================================
        prime_sel <= 2'd1;
        @(posedge clk);

        // --- Test 17: secp FADD (3+5) = 8 ---
        run_simple(4'h0, 256'd3, 256'd5);
        check_acc(256'd8, "SECP FADD(3,5)=8");

        // --- Test 18: secp FSUB wrap (3-5) = p_secp - 2 ---
        run_simple(4'h1, 256'd3, 256'd5);
        check_acc(PRIME_SECP - 256'd2, "SECP FSUB(3,5)=p-2");

        // --- Test 19: secp FMUL (7*11) = 77 ---
        run_simple(4'h2, 256'd7, 256'd11);
        check_acc(256'd77, "SECP FMUL(7,11)=77");

        // ================================================================
        // P-256 tests (prime_sel = 2)
        // ================================================================
        prime_sel <= 2'd2;
        @(posedge clk);

        // --- Test 20: P256 FADD (3+5) = 8 ---
        run_simple(4'h0, 256'd3, 256'd5);
        check_acc(256'd8, "P256 FADD(3,5)=8");

        // --- Test 21: P256 FSUB wrap (3-5) = p_256 - 2 ---
        run_simple(4'h1, 256'd3, 256'd5);
        check_acc(PRIME_P256 - 256'd2, "P256 FSUB(3,5)=p-2");

        // --- Test 22: P256 FMUL (7*11) = 77 ---
        run_simple(4'h2, 256'd7, 256'd11);
        check_acc(256'd77, "P256 FMUL(7,11)=77");

        // ================================================================
        // Custom prime tests (prime_sel = 3, p = 65537)
        // ================================================================
        prime_sel <= 2'd3;
        custom_p  <= 256'd65537;
        mont_pinv <= 256'd0;   // no REDC (pinv=0 → single-cycle path)
        @(posedge clk);

        // --- Test 23: Custom FMUL (100*200) mod 65537 = 20000 ---
        run_simple(4'h2, 256'd100, 256'd200);
        check_acc(256'd20000, "CUSTOM FMUL(100,200)=20000");

        // --- Test 24: Custom FMUL overflow: 300*300=90000 mod 65537=24463 ---
        run_simple(4'h2, 256'd300, 256'd300);
        check_acc(256'd24463, "CUSTOM FMUL(300,300)=24463");

        // --- Test 25: Custom FADD (65536+3) mod 65537 = 2 ---
        run_simple(4'h0, 256'd65536, 256'd3);
        check_acc(256'd2, "CUSTOM FADD(65536,3)=2");

        // ================================================================
        // Montgomery REDC tests (prime_sel=3 with non-zero pinv)
        // Use p=251, pinv as precomputed.
        // R = 2^256, REDC(T) = T * R^(-1) mod p
        // R^(-1) mod 251 = 51.
        // REDC(100*200) = 20000 * 51 mod 251 = 187
        // ================================================================
        prime_sel <= 2'd3;
        custom_p  <= 256'd251;
        mont_pinv <= 256'h34041465FDF5CD0105197F7D734041465FDF5CD0105197F7D734041465FDF5CD;
        @(posedge clk);
        @(posedge clk);

        // --- Test 26: REDC FMUL(100,200) = 187 ---
        run_simple(4'h2, 256'd100, 256'd200);
        check_acc(256'd187, "REDC FMUL(100,200)=187");

        // --- Test 27: REDC FSQR(123) = 15129*51 mod 251 = 5 ---
        run_simple(4'h3, 256'd123, 256'd0);
        check_acc(256'd5, "REDC FSQR(123)=5");

        // --- Test 28: REDC round-trip (Montgomery domain) ---
        // aR = 7*R mod 251 = 197;  bR = 11*R mod 251 = 202
        // MontMul(aR, bR) = 77*R mod 251 = 159
        run_simple(4'h2, 256'd197, 256'd202);
        check_acc(256'd159, "REDC roundtrip MontMul=159");

        // --- Test 29: REDC convert back: REDC(159, 1) = 77 ---
        run_simple(4'h2, 256'd159, 256'd1);
        check_acc(256'd77, "REDC convert back=77");

        // ================================================================
        // Verify Curve25519 still works after prime switching
        // ================================================================
        prime_sel <= 2'd0;
        @(posedge clk);

        run_simple(4'h0, 256'd3, 256'd5);
        check_acc(256'd8, "25519 FADD after prime switch");

        // ================================================================
        // MUL identity: a × 1 = a
        // ================================================================
        run_simple(4'h2,
            256'h1EADBEEF_CAFEBABE_12345678_9ABCDEF0_DEADBEEF_CAFEBABE_12345678_9ABCDEF0,
            256'd1);
        check_acc(
            256'h1EADBEEF_CAFEBABE_12345678_9ABCDEF0_DEADBEEF_CAFEBABE_12345678_9ABCDEF0,
            "GF.MUL(x,1)=x");

        // ================================================================
        // CEQ with flags
        // ================================================================
        run_simple(4'hA, 256'd0, 256'd0);
        test_num = test_num + 1;
        if (flag_z === 1'b1 && flag_z_we === 1'b1) begin
            $display("  [PASS] Test %0d: GF.CEQ 0==0 flag_z=1", test_num);
            pass_count = pass_count + 1;
        end else begin
            $display("  [FAIL] Test %0d: GF.CEQ 0==0 flag_z=%b we=%b", test_num, flag_z, flag_z_we);
            fail_count = fail_count + 1;
        end

        // ================================================================
        $display("");
        $display("=== Results: %0d passed, %0d failed ===", pass_count, fail_count);
        if (fail_count == 0)
            $display("ALL TESTS PASSED");
        else
            $display("SOME TESTS FAILED");
        $finish;
    end

endmodule
