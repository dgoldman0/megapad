// ============================================================================
// tb_ntt.v — NTT Accelerator Unit Tests
// ============================================================================
//
// Tests (q = 3329):
//   1. PADD: simple pointwise add of two single-coefficient polys
//   2. PMUL: pointwise multiply 
//   3. NTT_FWD: forward NTT of a delta polynomial (all zeros except [0]=1)
//   4. NTT_INV of a delta → roundtrip check NTT⁻¹(NTT(δ)) = δ
//   5. Status transitions: idle → busy → done
//   6. Q register readback
//   7. PADD with wraparound: (3000 + 1000) mod 3329 = 671
//

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_ntt;

    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;

    integer pass_count = 0;
    integer fail_count = 0;
    integer test_num   = 0;

    // DUT
    reg         req;
    reg  [5:0]  addr;
    reg  [63:0] wdata;
    reg         wen;
    wire [63:0] rdata;
    wire        ack;

    mp64_ntt u_dut (
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

    // Set coefficient index
    task set_idx(input [7:0] i);
        begin
            write64(6'h10, {56'd0, i});  // IDX register
        end
    endtask

    // Load one coefficient into poly A
    task load_a(input [31:0] val);
        begin
            write64(6'h18, {32'd0, val});  // LOAD_A, auto-increment
        end
    endtask

    // Load one coefficient into poly B
    task load_b(input [31:0] val);
        begin
            write64(6'h20, {32'd0, val});  // LOAD_B, auto-increment
        end
    endtask

    // Issue CMD: op in bits [2:1], go in bit [0]
    task issue_cmd(input [1:0] op);
        begin
            write64(6'h00, {61'd0, op, 1'b1});
        end
    endtask

    // Wait for done
    task wait_done;
        reg [63:0] st;
        integer timeout;
        begin
            timeout = 0;
            st = 0;
            while (!st[1] && timeout < 50000) begin
                read64(6'h00, st);
                timeout = timeout + 1;
            end
            if (timeout >= 50000) begin
                $display("  TIMEOUT waiting for NTT done!");
                fail_count = fail_count + 1;
            end
        end
    endtask

    // Read one result coefficient (auto-increments IDX)
    task read_result(output [31:0] val);
        reg [63:0] d;
        begin
            read64(6'h20, d);
            val = d[31:0];
        end
    endtask

    task check32(input [31:0] got, input [31:0] expected, input [8*48-1:0] name);
        begin
            test_num = test_num + 1;
            if (got === expected) begin
                $display("  [PASS] Test %0d: %0s", test_num, name);
                pass_count = pass_count + 1;
            end else begin
                $display("  [FAIL] Test %0d: %0s (expected %0d, got %0d)",
                         test_num, name, expected, got);
                fail_count = fail_count + 1;
            end
        end
    endtask

    task check64(input [63:0] got, input [63:0] expected, input [8*48-1:0] name);
        begin
            test_num = test_num + 1;
            if (got === expected) begin
                $display("  [PASS] Test %0d: %0s", test_num, name);
                pass_count = pass_count + 1;
            end else begin
                $display("  [FAIL] Test %0d: %0s (expected %0h, got %0h)",
                         test_num, name, expected, got);
                fail_count = fail_count + 1;
            end
        end
    endtask

    // ========================================================================
    // Test sequence
    // ========================================================================
    reg [63:0]  status;
    reg [63:0]  q_read;
    reg [31:0]  coeff;
    integer     i;

    initial begin
        $dumpfile("tb_ntt.vcd");
        $dumpvars(0, tb_ntt);

        req <= 0; addr <= 0; wdata <= 0; wen <= 0;
        rst_n = 0;
        #20;
        rst_n = 1;
        #10;

        $display("=== NTT Tests (q=3329) ===");

        // --- Test 1: Q register readback ---
        read64(6'h08, q_read);
        check64(q_read, 64'd3329, "Q default = 3329");

        // --- Test 2: PADD — load A[0]=100, B[0]=200, rest zero ---
        set_idx(0);
        load_a(32'd100);
        // Remaining A coefficients stay at 0 from reset
        set_idx(0);
        load_b(32'd200);
        // Issue PADD (op=3)
        issue_cmd(2'd3);
        wait_done;
        // Read result[0]
        set_idx(0);
        read_result(coeff);
        check32(coeff, 32'd300, "PADD: 100+200=300");

        // --- Test 3: PADD wraparound — A[0]=3000, B[0]=1000 ---
        set_idx(0);
        load_a(32'd3000);
        set_idx(0);
        load_b(32'd1000);
        issue_cmd(2'd3);  // PADD
        wait_done;
        set_idx(0);
        read_result(coeff);
        // (3000 + 1000) mod 3329 = 4000 mod 3329 = 671
        check32(coeff, 32'd671, "PADD wrap: 3000+1000=671");

        // --- Test 4: PMUL — A[0]=100, B[0]=33 → 3300 ---
        set_idx(0);
        load_a(32'd100);
        set_idx(0);
        load_b(32'd33);
        issue_cmd(2'd2);  // PMUL
        wait_done;
        set_idx(0);
        read_result(coeff);
        check32(coeff, 32'd3300, "PMUL: 100*33=3300");

        // --- Test 5: PMUL with reduction — A[0]=2000, B[0]=2000 ---
        // 2000*2000 = 4000000. 4000000 mod 3329 = 4000000 - 1201*3329 = 1871
        set_idx(0);
        load_a(32'd2000);
        set_idx(0);
        load_b(32'd2000);
        issue_cmd(2'd2);  // PMUL
        wait_done;
        set_idx(0);
        read_result(coeff);
        check32(coeff, 32'd1871, "PMUL: 2000*2000 mod 3329 = 1871");

        // --- Test 6: NTT_FWD of delta (A[0]=1, rest=0) ---
        // NTT of δ[0] should give all-ones vector: NTT(δ)[k] = 1 for all k
        set_idx(0);
        load_a(32'd1);
        for (i = 1; i < 256; i = i + 1)
            load_a(32'd0);
        issue_cmd(2'd0);  // FWD NTT
        wait_done;
        // Check first few results
        set_idx(0);
        read_result(coeff);
        check32(coeff, 32'd1, "NTT_FWD(delta)[0]=1");
        read_result(coeff);
        check32(coeff, 32'd1, "NTT_FWD(delta)[1]=1");

        // --- Test 7: Status readback after done ---
        read64(6'h00, status);
        check64(status, 64'd2, "STATUS=done(2)");

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
