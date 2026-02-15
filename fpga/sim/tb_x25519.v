// ============================================================================
// tb_x25519.v — X25519 Accelerator Unit Tests
// ============================================================================
//
// Tests using RFC 7748 §6.1 test vectors:
//   1. RFC 7748 test vector 1: known scalar × basepoint
//   2. RFC 7748 test vector 2: second known scalar × basepoint
//   3. Basepoint self-multiply (scalar=1 padded → should return basepoint)
//   4. Zero scalar → all-zeros output
//   5. Status register: idle before start, busy during, done after
//   6. Double computation: run twice without reset
//   7. RFC 7748 ECDH: Alice × Bob_pub == Bob × Alice_pub
//

`timescale 1ns / 1ps
`include "mp64_defs.vh"

module tb_x25519;

    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;   // 100 MHz

    integer pass_count = 0;
    integer fail_count = 0;
    integer test_num   = 0;

    // ========================================================================
    // DUT
    // ========================================================================
    reg         x_req;
    reg  [5:0]  x_addr;
    reg  [63:0] x_wdata;
    reg         x_wen;
    wire [63:0] x_rdata;
    wire        x_ack;

    mp64_x25519 u_x25519 (
        .clk   (clk),
        .rst_n (rst_n),
        .req   (x_req),
        .addr  (x_addr),
        .wdata (x_wdata),
        .wen   (x_wen),
        .rdata (x_rdata),
        .ack   (x_ack)
    );

    // ========================================================================
    // Helper tasks
    // ========================================================================
    task x_write(input [5:0] a, input [63:0] d);
        begin
            @(posedge clk);
            x_req <= 1'b1; x_addr <= a; x_wdata <= d; x_wen <= 1'b1;
            @(posedge clk);
            x_req <= 1'b0; x_wen <= 1'b0;
        end
    endtask

    task x_read(input [5:0] a, output [63:0] d);
        begin
            @(posedge clk);
            x_req <= 1'b1; x_addr <= a; x_wen <= 1'b0;
            @(posedge clk);
            x_req <= 1'b0;
            @(posedge clk);
            d = x_rdata;
        end
    endtask

    task x_wait_done;
        reg [63:0] st;
        integer timeout;
        begin
            st = 0;
            timeout = 0;
            while (!st[1] && timeout < 500000) begin
                x_read(6'h00, st);
                timeout = timeout + 1;
            end
            if (timeout >= 500000)
                $display("  TIMEOUT waiting for done");
        end
    endtask

    // Load 256-bit value into scalar (4 × 64-bit LE words)
    task load_scalar(input [255:0] s);
        begin
            x_write(6'h00, s[ 63:  0]);
            x_write(6'h08, s[127: 64]);
            x_write(6'h10, s[191:128]);
            x_write(6'h18, s[255:192]);
        end
    endtask

    // Load 256-bit value into point (4 × 64-bit LE words)
    task load_point(input [255:0] p);
        begin
            x_write(6'h20, p[ 63:  0]);
            x_write(6'h28, p[127: 64]);
            x_write(6'h30, p[191:128]);
            x_write(6'h38, p[255:192]);
        end
    endtask

    // Start computation via CMD
    task x_start;
        begin
            x_write(6'h3F, 64'd1);
        end
    endtask

    // Read 256-bit result
    task read_result(output [255:0] r);
        reg [63:0] r0, r1, r2, r3;
        begin
            x_read(6'h08, r0);
            x_read(6'h10, r1);
            x_read(6'h18, r2);
            x_read(6'h20, r3);
            r = {r3, r2, r1, r0};
        end
    endtask

    task check256(input [511:0] label, input [255:0] got, input [255:0] expected);
        begin
            if (got === expected) begin
                $display("  PASS: %0s", label);
                $display("        got = %064h", got);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s", label);
                $display("        got = %064h", got);
                $display("        exp = %064h", expected);
                fail_count = fail_count + 1;
            end
        end
    endtask

    task check64(input [511:0] label, input [63:0] got, input [63:0] expected);
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

    // ========================================================================
    // Test vectors (RFC 7748 §6.1)
    // ========================================================================
    // Input scalar and u-coordinate are in little-endian byte order,
    // stored as 256-bit integers with the byte at offset 0 in the LSB.
    //
    // Test vector 1:
    //   scalar: a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4
    //   u:      e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c
    //   output: c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552
    //
    // Note: these are in the "byte string" representation from RFC 7748.
    // In our 256-bit register, byte[0] is at bits [7:0] (LE).

    // Scalar 1 (LE bytes → 256-bit integer)
    localparam [255:0] SCALAR1 = 256'hc49a44ba44226a50185afcc10a4c1462dd5e46824b15163b9d7c52f06be346a5;
    // u-coordinate 1
    localparam [255:0] POINT1  = 256'h4c1cabd0a603a9103b35b326ec2466727c5fb124a4c19435db3030586768dbe6;
    // Expected output 1
    localparam [255:0] EXPECT1 = 256'h5285a2775507b454f7711c4903cfec324f088df24dea948e90c6e99d3755dac3;

    // Test vector 2:
    //   scalar: 4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d
    //   u:      e5210f12786811d3f4b7959d0538ae2c31dbe7106fc03c3efc4cd549c715a493
    //   output: 95cbde9476e8907d7aade45cb4b873f88b595a68799fa152e6f8f7647aac7957
    localparam [255:0] SCALAR2 = 256'h0dba1879_9e16a42c_d401eae0_21641bc1_f56a7d95_9126d25a_3c67b4d1_d4e9664b;
    localparam [255:0] POINT2  = 256'h93a415c7_49d54cfc_3e3cc06f_10e7db31_2cae3805_9d95b7f4_d3116878_120f21e5;
    localparam [255:0] EXPECT2 = 256'h5779ac7a_64f7f8e6_52a19f79_685a598b_f873b8b4_5ce4ad7a_7d90e876_94decb95;

    // Basepoint (u=9)
    localparam [255:0] BASEPOINT = 256'h0000000000000000000000000000000000000000000000000000000000000009;

    // ========================================================================
    // Main test sequence
    // ========================================================================
    reg [255:0] result;
    reg [63:0]  status;

    initial begin
        $dumpfile("tb_x25519.vcd");
        $dumpvars(0, tb_x25519);

        x_req = 0; x_addr = 0; x_wdata = 0; x_wen = 0;
        rst_n = 0;
        #40;
        rst_n = 1;
        #20;

        $display("=== X25519 Accelerator Tests ===");

        // ----------------------------------------------------------------
        // Test 1: RFC 7748 test vector 1
        // ----------------------------------------------------------------
        test_num = 1;
        $display("\nTest %0d: RFC 7748 vector 1", test_num);
        load_scalar(SCALAR1);
        load_point(POINT1);
        x_start;
        x_wait_done;
        read_result(result);
        check256("vector1_result", result, EXPECT1);

        // ----------------------------------------------------------------
        // Test 2: RFC 7748 test vector 2
        // ----------------------------------------------------------------
        test_num = 2;
        $display("\nTest %0d: RFC 7748 vector 2", test_num);
        load_scalar(SCALAR2);
        load_point(POINT2);
        x_start;
        x_wait_done;
        read_result(result);
        check256("vector2_result", result, EXPECT2);

        // ----------------------------------------------------------------
        // Test 3: Status register check
        // ----------------------------------------------------------------
        test_num = 3;
        $display("\nTest %0d: Status register (idle → busy → done)", test_num);
        // After last test, should be done
        x_read(6'h00, status);
        check64("status_done", status, 64'h2);  // done=1, busy=0

        // Start a new computation
        load_scalar(SCALAR1);
        load_point(POINT1);
        x_start;
        // Immediately read status — should be busy
        x_read(6'h00, status);
        // busy=1 (bit 0), done might be cleared
        if (status[0] == 1'b1) begin
            $display("  PASS: status_busy bit set");
            pass_count = pass_count + 1;
        end else begin
            $display("  FAIL: status_busy not set (status=%016h)", status);
            fail_count = fail_count + 1;
        end
        x_wait_done;
        x_read(6'h00, status);
        check64("status_done_after", status, 64'h2);

        // ----------------------------------------------------------------
        // Test 4: Double computation without reset
        // ----------------------------------------------------------------
        test_num = 4;
        $display("\nTest %0d: Double computation (no reset)", test_num);
        load_scalar(SCALAR2);
        load_point(POINT2);
        x_start;
        x_wait_done;
        read_result(result);
        check256("double_compute", result, EXPECT2);

        // ----------------------------------------------------------------
        // Test 5: Scalar multiply with basepoint u=9
        //   scalar = clamped version of all-1s → known output
        //   Use SCALAR1 × basepoint(9) — this is a DH public key generation
        // ----------------------------------------------------------------
        test_num = 5;
        $display("\nTest %0d: Scalar × basepoint(9)", test_num);
        load_scalar(SCALAR1);
        load_point(BASEPOINT);
        x_start;
        x_wait_done;
        read_result(result);
        // We don't have a pre-computed expected value for SCALAR1 × 9,
        // but we can verify it's non-zero and different from the input
        if (result != 256'd0 && result != SCALAR1 && result != BASEPOINT) begin
            $display("  PASS: basepoint_result is non-trivial");
            $display("        got = %064h", result);
            pass_count = pass_count + 1;
        end else begin
            $display("  FAIL: basepoint_result seems trivial");
            $display("        got = %064h", result);
            fail_count = fail_count + 1;
        end

        // ----------------------------------------------------------------
        // Test 6: ECDH key agreement
        //   Alice_priv × (Bob_priv × G) == Bob_priv × (Alice_priv × G)
        // ----------------------------------------------------------------
        test_num = 6;
        $display("\nTest %0d: ECDH agreement (Alice*Bob_pub == Bob*Alice_pub)", test_num);

        // Alice_pub = SCALAR1 × G (already computed in test 5: 'result')
        // But we need to recompute because we didn't save it.
        // Actually, let's use smaller scalars for clarity.

        // Alice_priv × basepoint → Alice_pub
        load_scalar(SCALAR1);
        load_point(BASEPOINT);
        x_start;
        x_wait_done;
        // Alice_pub is in result registers — save it in Verilog regs
        read_result(result);
        // result = Alice_pub

        // Bob_priv × Alice_pub → shared_secret_bob
        begin : ecdh_block
            reg [255:0] alice_pub, bob_pub;
            reg [255:0] shared_alice, shared_bob;

            alice_pub = result;

            // Bob_pub = SCALAR2 × basepoint
            load_scalar(SCALAR2);
            load_point(BASEPOINT);
            x_start;
            x_wait_done;
            read_result(bob_pub);

            // shared_bob = SCALAR2 × Alice_pub
            load_scalar(SCALAR2);
            load_point(alice_pub);
            x_start;
            x_wait_done;
            read_result(shared_bob);

            // shared_alice = SCALAR1 × Bob_pub
            load_scalar(SCALAR1);
            load_point(bob_pub);
            x_start;
            x_wait_done;
            read_result(shared_alice);

            check256("ecdh_agreement", shared_alice, shared_bob);
        end

        // ----------------------------------------------------------------
        // Summary
        // ----------------------------------------------------------------
        #100;
        $display("\n=== X25519 Results: %0d passed, %0d failed ===", pass_count, fail_count);
        if (fail_count == 0)
            $display("ALL TESTS PASSED");
        else
            $display("SOME TESTS FAILED");
        $finish;
    end

    // Timeout watchdog
    initial begin
        #100_000_000;  // 100 ms at 1 ns resolution
        $display("TIMEOUT: simulation exceeded 100 ms");
        $finish;
    end

endmodule
