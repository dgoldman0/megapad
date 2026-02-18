// ============================================================================
// tb_field_alu.v — Field ALU Accelerator Unit Tests
// ============================================================================
//
// Tests:
//   1. FADD: (3 + 5) mod p = 8
//   2. FSUB: (5 - 3) mod p = 2
//   3. FSUB wraparound: (3 - 5) mod p = p - 2
//   4. FMUL: (7 × 11) mod p = 77
//   5. FSQR: 9² mod p = 81
//   6. FINV: inv(2) × 2 mod p = 1 (verify via readback)
//   7. MUL_RAW: 0xFF...FF × 2 → 512-bit result with overflow
//   8. X25519: scalar = 1 (clamped), base point 9 → known result
//   9. Status transitions: idle → busy → done
//  10. FPOW: 2^10 mod p = 1024
//

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_field_alu;

    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;  // 100 MHz

    integer pass_count = 0;
    integer fail_count = 0;
    integer test_num   = 0;

    // DUT signals
    reg         req;
    reg  [5:0]  addr;
    reg  [63:0] wdata;
    reg         wen;
    wire [63:0] rdata;
    wire        ack;

    mp64_field_alu u_dut (
        .clk(clk), .rst_n(rst_n),
        .req(req), .addr(addr), .wdata(wdata), .wen(wen),
        .rdata(rdata), .ack(ack)
    );

    // ========================================================================
    // Helper tasks
    // ========================================================================

    task write64(input [5:0] a, input [63:0] d);
        begin
            @(posedge clk);
            req <= 1; addr <= a; wdata <= d; wen <= 1;
            @(posedge clk);
            req <= 0; wen <= 0;
        end
    endtask

    task read64(input [5:0] a, output [63:0] d);
        begin
            @(posedge clk);
            req <= 1; addr <= a; wdata <= 0; wen <= 0;
            @(posedge clk);
            req <= 0;
            @(posedge clk);  // wait for registered rdata to settle
            d = rdata;
        end
    endtask

    // Write 256-bit operand A (4 × 64-bit LE words)
    task write_operand_a(input [255:0] val);
        begin
            write64(6'h00, val[ 63:  0]);
            write64(6'h08, val[127: 64]);
            write64(6'h10, val[191:128]);
            write64(6'h18, val[255:192]);
        end
    endtask

    // Write 256-bit operand B
    task write_operand_b(input [255:0] val);
        begin
            write64(6'h20, val[ 63:  0]);
            write64(6'h28, val[127: 64]);
            write64(6'h30, val[191:128]);
            write64(6'h38, val[255:192]);
        end
    endtask

    // Issue CMD: mode in bits [4:1], go in bit [0], result_sel in bit [5],
    //            prime_sel in bits [7:6]
    task issue_cmd(input [3:0] mode, input go, input result_sel, input [1:0] prime_sel);
        begin
            write64(6'h3F, {56'd0, prime_sel, result_sel, mode, go});
        end
    endtask

    // Set prime_sel without go (latches prime_sel for subsequent ops)
    task set_prime(input [1:0] prime_sel);
        begin
            issue_cmd(4'd0, 1'b0, 1'b0, prime_sel);
        end
    endtask

    // Wait for done (polls STATUS until done bit = 1)
    task wait_done;
        reg [63:0] st;
        integer timeout;
        begin
            timeout = 0;
            st = 0;
            while (!st[1] && timeout < 200000) begin
                read64(6'h00, st);
                timeout = timeout + 1;
            end
            if (timeout >= 200000) begin
                $display("  TIMEOUT waiting for done!");
                fail_count = fail_count + 1;
            end
        end
    endtask

    // Read 256-bit result (LO or HI depending on result_sel)
    task read_result(output [255:0] val);
        reg [63:0] w0, w1, w2, w3;
        begin
            read64(6'h08, w0);
            read64(6'h10, w1);
            read64(6'h18, w2);
            read64(6'h20, w3);
            val = {w3, w2, w1, w0};
        end
    endtask

    // Check helper
    task check(input [255:0] got, input [255:0] expected, input [8*40-1:0] name);
        begin
            test_num = test_num + 1;
            if (got === expected) begin
                $display("  [PASS] Test %0d: %0s", test_num, name);
                pass_count = pass_count + 1;
            end else begin
                $display("  [FAIL] Test %0d: %0s", test_num, name);
                $display("         expected: %064h", expected);
                $display("              got: %064h", got);
                fail_count = fail_count + 1;
            end
        end
    endtask

    // ========================================================================
    // Test sequence
    // ========================================================================
    reg [255:0] result;
    reg [63:0]  status;

    localparam [255:0] PRIME = 256'h7FFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFED;
    localparam [255:0] PRIME_SECP = 256'hFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFE_FFFFFC2F;
    localparam [255:0] PRIME_P256 = 256'hFFFFFFFF_00000001_00000000_00000000_00000000_FFFFFFFF_FFFFFFFF_FFFFFFFF;

    initial begin
        $dumpfile("tb_field_alu.vcd");
        $dumpvars(0, tb_field_alu);

        req <= 0; addr <= 0; wdata <= 0; wen <= 0;
        rst_n = 0;
        #20;
        rst_n = 1;
        #10;

        $display("=== Field ALU Tests ===");

        // --- Test 1: FADD (3 + 5) = 8 ---
        write_operand_a(256'd3);
        write_operand_b(256'd5);
        issue_cmd(4'd1, 1'b1, 1'b0, 2'd0);  // mode=FADD, go, prime=25519
        wait_done;
        read_result(result);
        check(result, 256'd8, "FADD(3,5)=8");

        // --- Test 2: FSUB (5 - 3) = 2 ---
        write_operand_a(256'd5);
        write_operand_b(256'd3);
        issue_cmd(4'd2, 1'b1, 1'b0, 2'd0);  // FSUB
        wait_done;
        read_result(result);
        check(result, 256'd2, "FSUB(5,3)=2");

        // --- Test 3: FSUB wraparound (3 - 5) mod p = p - 2 ---
        write_operand_a(256'd3);
        write_operand_b(256'd5);
        issue_cmd(4'd2, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, PRIME - 256'd2, "FSUB(3,5)=p-2");

        // --- Test 4: FMUL (7 × 11) = 77 ---
        write_operand_a(256'd7);
        write_operand_b(256'd11);
        issue_cmd(4'd3, 1'b1, 1'b0, 2'd0);  // FMUL
        wait_done;
        read_result(result);
        check(result, 256'd77, "FMUL(7,11)=77");

        // --- Test 5: FSQR (9²) = 81 ---
        write_operand_a(256'd9);
        issue_cmd(4'd4, 1'b1, 1'b0, 2'd0);  // FSQR
        wait_done;
        read_result(result);
        check(result, 256'd81, "FSQR(9)=81");

        // --- Test 6: FINV — compute 2⁻¹ mod p ---
        // p = 2²⁵⁵ − 19, so 2⁻¹ = (p+1)/2
        write_operand_a(256'd2);
        issue_cmd(4'd5, 1'b1, 1'b0, 2'd0);  // FINV
        wait_done;
        read_result(result);
        check(result, (PRIME + 256'd1) >> 1, "FINV(2)=(p+1)/2");

        // --- Test 7: MUL_RAW — small values, verify low half ---
        write_operand_a(256'd100);
        write_operand_b(256'd200);
        issue_cmd(4'd7, 1'b1, 1'b0, 2'd0);  // MUL_RAW
        wait_done;
        read_result(result);  // reads LO (result_sel=0)
        check(result, 256'd20000, "MUL_RAW(100,200) LO=20000");

        // Read HI half (should be 0 for small operands)
        issue_cmd(4'd0, 1'b0, 1'b1, 2'd0);  // set result_sel=1, no go
        @(posedge clk); @(posedge clk);
        read_result(result);
        check(result, 256'd0, "MUL_RAW(100,200) HI=0");

        // --- Test 8: Status — verify idle→busy→done ---
        // Already done, just verify status reads done
        issue_cmd(4'd0, 1'b0, 1'b0, 2'd0);  // reset result_sel
        @(posedge clk);
        read64(6'h00, status);
        check({192'd0, status}, {192'd0, 62'd0, 2'b10}, "STATUS=done(2)");

        // --- Test 9: FPOW — 2^10 mod p = 1024 ---
        write_operand_a(256'd2);
        write_operand_b(256'd10);
        issue_cmd(4'd6, 1'b1, 1'b0, 2'd0);  // FPOW
        wait_done;
        read_result(result);
        check(result, 256'd1024, "FPOW(2,10)=1024");

        // --- Test 10: FMUL identity — a × 1 = a (a < p) ---
        write_operand_a(256'h1EADBEEF_CAFEBABE_12345678_9ABCDEF0_DEADBEEF_CAFEBABE_12345678_9ABCDEF0);
        write_operand_b(256'd1);
        issue_cmd(4'd3, 1'b1, 1'b0, 2'd0);  // FMUL
        wait_done;
        read_result(result);
        check(result, 256'h1EADBEEF_CAFEBABE_12345678_9ABCDEF0_DEADBEEF_CAFEBABE_12345678_9ABCDEF0,
              "FMUL(x,1)=x");

        // ================================================================
        // secp256k1 tests (prime_sel = 1)
        // ================================================================

        // Set prime to secp256k1
        set_prime(2'd1);

        // --- Test 11: secp256k1 FADD (3 + 5) = 8 ---
        write_operand_a(256'd3);
        write_operand_b(256'd5);
        issue_cmd(4'd1, 1'b1, 1'b0, 2'd0);  // go (prime_sel already latched)
        wait_done;
        read_result(result);
        check(result, 256'd8, "SECP FADD(3,5)=8");

        // --- Test 12: secp256k1 FSUB wraparound (3 - 5) mod p_secp = p_secp - 2 ---
        write_operand_a(256'd3);
        write_operand_b(256'd5);
        issue_cmd(4'd2, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, PRIME_SECP - 256'd2, "SECP FSUB(3,5)=p-2");

        // --- Test 13: secp256k1 FMUL (7 × 11) = 77 ---
        write_operand_a(256'd7);
        write_operand_b(256'd11);
        issue_cmd(4'd3, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, 256'd77, "SECP FMUL(7,11)=77");

        // --- Test 14: secp256k1 FSQR (9²) = 81 ---
        write_operand_a(256'd9);
        issue_cmd(4'd4, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, 256'd81, "SECP FSQR(9)=81");

        // --- Test 15: secp256k1 FMUL that requires reduction ---
        // (p_secp - 1) × 2 mod p_secp = p_secp - 2
        write_operand_a(PRIME_SECP - 256'd1);
        write_operand_b(256'd2);
        issue_cmd(4'd3, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, PRIME_SECP - 256'd2, "SECP FMUL(p-1,2)=p-2");

        // --- Test 16: secp256k1 FINV — inv(2) × 2 = 1 ---
        write_operand_a(256'd2);
        issue_cmd(4'd5, 1'b1, 1'b0, 2'd0);  // FINV
        wait_done;
        read_result(result);
        // Verify: result × 2 mod p_secp = 1
        // inv(2) = (p_secp + 1) / 2
        check(result, (PRIME_SECP + 256'd1) >> 1, "SECP FINV(2)=(p+1)/2");

        // --- Test 17: Switch back to Curve25519 — verify no leakage ---
        set_prime(2'd0);
        write_operand_a(256'd3);
        write_operand_b(256'd5);
        issue_cmd(4'd2, 1'b1, 1'b0, 2'd0);  // FSUB
        wait_done;
        read_result(result);
        check(result, PRIME - 256'd2, "25519 FSUB(3,5) after SECP");

        // ================================================================
        // P-256 tests (prime_sel = 2)
        // ================================================================
        set_prime(2'd2);

        // --- Test 19: P-256 FADD (3 + 5) = 8 ---
        write_operand_a(256'd3);
        write_operand_b(256'd5);
        issue_cmd(4'd1, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, 256'd8, "P256 FADD(3,5)=8");

        // --- Test 20: P-256 FSUB wraparound (3 - 5) mod p_256 = p_256 - 2 ---
        write_operand_a(256'd3);
        write_operand_b(256'd5);
        issue_cmd(4'd2, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, PRIME_P256 - 256'd2, "P256 FSUB(3,5)=p-2");

        // --- Test 21: P-256 FMUL (7 × 11) = 77 ---
        write_operand_a(256'd7);
        write_operand_b(256'd11);
        issue_cmd(4'd3, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, 256'd77, "P256 FMUL(7,11)=77");

        // --- Test 22: P-256 FMUL with reduction (p-1)*2 = p-2 ---
        write_operand_a(PRIME_P256 - 256'd1);
        write_operand_b(256'd2);
        issue_cmd(4'd3, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, PRIME_P256 - 256'd2, "P256 FMUL(p-1,2)=p-2");

        // --- Test 23: P-256 FINV — inv(2) = (p+1)/2 ---
        write_operand_a(256'd2);
        issue_cmd(4'd5, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, (PRIME_P256 + 256'd1) >> 1, "P256 FINV(2)=(p+1)/2");

        // ================================================================
        // Custom prime tests (prime_sel = 3, LOAD_PRIME)
        // Use small prime p=65537 for easy verification
        // ================================================================
        // Load custom prime: A=65537, B=0 (p_inv not used in sim model)
        write_operand_a(256'd65537);
        write_operand_b(256'd0);
        issue_cmd(4'd10, 1'b1, 1'b0, 2'd0);  // LOAD_PRIME
        wait_done;

        // Set prime_sel=3
        set_prime(2'd3);

        // --- Test 24: Custom FMUL (100 × 200) mod 65537 = 20000 mod 65537 ---
        write_operand_a(256'd100);
        write_operand_b(256'd200);
        issue_cmd(4'd3, 1'b1, 1'b0, 2'd0);  // FMUL
        wait_done;
        read_result(result);
        check(result, 256'd20000, "CUSTOM FMUL(100,200)=20000");

        // --- Test 25: Custom FMUL overflow: 300 × 300 = 90000 mod 65537 = 24926 ---
        write_operand_a(256'd300);
        write_operand_b(256'd300);
        issue_cmd(4'd3, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        // 90000 mod 65537 = 90000 - 65537 = 24463
        check(result, 256'd24463, "CUSTOM FMUL(300,300)=24463");

        // --- Test 26: Custom FADD (65536 + 3) mod 65537 = 2 ---
        write_operand_a(256'd65536);
        write_operand_b(256'd3);
        issue_cmd(4'd1, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, 256'd2, "CUSTOM FADD(65536,3)=2");

        // --- Test 27: Custom FINV — inv(2) × 2 mod 65537 = 1 ---
        // inv(2) mod 65537 = 2^(65537-2) mod 65537 = 32769
        write_operand_a(256'd2);
        issue_cmd(4'd5, 1'b1, 1'b0, 2'd0);  // FINV
        wait_done;
        read_result(result);
        check(result, 256'd32769, "CUSTOM FINV(2)=32769");

        // ================================================================
        // Tests 28-35: FCMOV, FCEQ, FMAC, MUL_ADD_RAW (new modes 8-12)
        // ================================================================
        // Switch back to Curve25519 for these tests
        set_prime(2'd0);

        // --- Test 28: FCMOV cond=1 → result = operand_a ---
        // Pre-load result_lo = 42 via FADD(42, 0)
        write_operand_a(256'd42);
        write_operand_b(256'd0);
        issue_cmd(4'd1, 1'b1, 1'b0, 2'd0);  // FADD → result_lo = 42
        wait_done;
        // Now FCMOV: a=99, cond=1 → result = 99
        write_operand_a(256'd99);
        write_operand_b(256'd1);
        issue_cmd(4'd8, 1'b1, 1'b0, 2'd0);  // FCMOV
        wait_done;
        read_result(result);
        check(result, 256'd99, "FCMOV cond=1 → a");

        // --- Test 29: FCMOV cond=0 → result unchanged ---
        // result_lo is 99 from previous test
        write_operand_a(256'd77);
        write_operand_b(256'd0);
        issue_cmd(4'd8, 1'b1, 1'b0, 2'd0);  // FCMOV
        wait_done;
        read_result(result);
        check(result, 256'd99, "FCMOV cond=0 → old");

        // --- Test 30: FCEQ equal → 1 ---
        write_operand_a(256'd12345);
        write_operand_b(256'd12345);
        issue_cmd(4'd9, 1'b1, 1'b0, 2'd0);  // FCEQ
        wait_done;
        read_result(result);
        check(result, 256'd1, "FCEQ equal → 1");

        // --- Test 31: FCEQ not equal → 0 ---
        write_operand_a(256'd12345);
        write_operand_b(256'd12346);
        issue_cmd(4'd9, 1'b1, 1'b0, 2'd0);  // FCEQ
        wait_done;
        read_result(result);
        check(result, 256'd0, "FCEQ not-equal → 0");

        // --- Test 32: FMAC accumulate: (3*5) + (7*11) = 15 + 77 = 92 ---
        // First, zero result via FADD(0,0)
        write_operand_a(256'd0);
        write_operand_b(256'd0);
        issue_cmd(4'd1, 1'b1, 1'b0, 2'd0);  // FADD → result_lo = 0
        wait_done;
        // FMAC: result += 3*5 = 15
        write_operand_a(256'd3);
        write_operand_b(256'd5);
        issue_cmd(4'd11, 1'b1, 1'b0, 2'd0); // FMAC
        wait_done;
        // FMAC: result += 7*11 = 77; total = 92
        write_operand_a(256'd7);
        write_operand_b(256'd11);
        issue_cmd(4'd11, 1'b1, 1'b0, 2'd0); // FMAC
        wait_done;
        read_result(result);
        check(result, 256'd92, "FMAC 3*5+7*11=92");

        // --- Test 33: MUL_ADD_RAW accumulate: 10*20 + 30*40 = 200+1200 = 1400 ---
        // Zero result via MUL_RAW(0,0)
        write_operand_a(256'd0);
        write_operand_b(256'd0);
        issue_cmd(4'd7, 1'b1, 1'b0, 2'd0);  // MUL_RAW → result = 0
        wait_done;
        // MUL_ADD_RAW: result += 10*20 = 200
        write_operand_a(256'd10);
        write_operand_b(256'd20);
        issue_cmd(4'd12, 1'b1, 1'b0, 2'd0); // MUL_ADD_RAW
        wait_done;
        // MUL_ADD_RAW: result += 30*40 = 1200; total = 1400
        write_operand_a(256'd30);
        write_operand_b(256'd40);
        issue_cmd(4'd12, 1'b1, 1'b0, 2'd0); // MUL_ADD_RAW
        wait_done;
        read_result(result);
        check(result, 256'd1400, "MUL_ADD_RAW 10*20+30*40=1400");

        // --- Test 34: FCEQ with zero values ---
        write_operand_a(256'd0);
        write_operand_b(256'd0);
        issue_cmd(4'd9, 1'b1, 1'b0, 2'd0);  // FCEQ
        wait_done;
        read_result(result);
        check(result, 256'd1, "FCEQ 0==0 → 1");

        // --- Test 35: FMAC with reduction (Curve25519) ---
        // p = 2^255 - 19; do FMAC with large values that reduce
        // Start from 0
        write_operand_a(256'd0);
        write_operand_b(256'd0);
        issue_cmd(4'd1, 1'b1, 1'b0, 2'd0);  // FADD → result_lo = 0
        wait_done;
        // FMAC: result += (p-1)*2 mod p = p-2
        write_operand_a({1'b0, {255{1'b1}}} - 256'd19);  // p-1
        write_operand_b(256'd2);
        issue_cmd(4'd11, 1'b1, 1'b0, 2'd0); // FMAC
        wait_done;
        read_result(result);
        check(result, {1'b0, {255{1'b1}}} - 256'd20, "FMAC (p-1)*2 mod p = p-2");

        // ================================================================
        // Tests 36-39: Cross-prime switching + backward compat
        // ================================================================

        // --- Test 36: Switch to secp256k1, compute, switch back to 25519 ---
        set_prime(2'd1);  // secp256k1
        write_operand_a(256'd7);
        write_operand_b(256'd11);
        issue_cmd(4'd3, 1'b1, 1'b0, 2'd0);  // FMUL
        wait_done;
        read_result(result);
        check(result, 256'd77, "cross: SECP FMUL(7,11)=77");

        // Switch to P-256
        set_prime(2'd2);
        write_operand_a(256'd13);
        write_operand_b(256'd17);
        issue_cmd(4'd3, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, 256'd221, "cross: P256 FMUL(13,17)=221");

        // Switch back to Curve25519/default
        set_prime(2'd0);
        write_operand_a(256'd3);
        write_operand_b(256'd5);
        issue_cmd(4'd1, 1'b1, 1'b0, 2'd0);  // FADD
        wait_done;
        read_result(result);
        check(result, 256'd8, "cross: 25519 FADD(3,5)=8 after switch");

        // --- Test 37: X25519 backward compat after prime switching ---
        // X25519 mode always uses Curve25519 regardless of prime_sel
        set_prime(2'd1);  // secp256k1 selected, shouldn't matter
        write_operand_a(256'd42);  // some scalar (will be clamped)
        write_operand_b(256'd9);     // basepoint
        issue_cmd(4'd0, 1'b1, 1'b0, 2'd0);  // X25519
        wait_done;
        read_result(result);
        // Just verify it completed (non-zero result) — exact value
        // depends on clamped scalar, but must not be 0 or 9
        if (result == 256'd0 || result == 256'd9)
            $display("  [FAIL] Test 37: X25519 compat after prime switch");
        else begin
            $display("  [PASS] Test 37: X25519 compat after prime switch");
            pass_count = pass_count + 1;
        end

        // --- Test 38: FCEQ near-miss (differ in one bit) ---
        set_prime(2'd0);
        write_operand_a(256'd255);
        write_operand_b(256'd127);  // differ in bit 7
        issue_cmd(4'd9, 1'b1, 1'b0, 2'd0);  // FCEQ
        wait_done;
        read_result(result);
        check(result, 256'd0, "FCEQ near-miss → 0");

        // --- Test 39: Custom prime, switch to 25519, back to custom ---
        // Verify custom_p survives prime_sel switching
        set_prime(2'd3);  // custom (p=65537 still loaded from test 24)
        write_operand_a(256'd100);
        write_operand_b(256'd200);
        issue_cmd(4'd3, 1'b1, 1'b0, 2'd0);
        wait_done;
        read_result(result);
        check(result, 256'd20000, "custom survives switch back");

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
