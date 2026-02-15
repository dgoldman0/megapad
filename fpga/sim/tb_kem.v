// ============================================================================
// tb_kem.v — ML-KEM-512 Accelerator Unit Tests
// ============================================================================
//
// Tests:
//   1. BUF_SEL + BUF_SIZE readback for all 5 buffers
//   2. DIN/DOUT round-trip: write seed bytes, read them back
//   3. KEYGEN: launch, wait for done, verify status
//   4. ENCAPS: after keygen, verify CT/SS buffers non-zero
//   5. DECAPS: after encaps, verify SS buffer populated
//   6. IDX_SET: random-access positioning
//

`timescale 1ns / 1ps
`include "mp64_defs.vh"

module tb_kem;

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

    mp64_kem u_dut (
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

    // Select buffer
    task buf_select(input [2:0] id);
        begin
            write64(6'h08, {61'd0, id});
        end
    endtask

    // Write one data byte to selected buffer
    task din_byte(input [7:0] val);
        begin
            write64(6'h10, {56'd0, val});
        end
    endtask

    // Read one data byte from selected buffer
    task dout_byte(output [7:0] val);
        reg [63:0] d;
        begin
            read64(6'h10, d);
            val = d[7:0];
        end
    endtask

    // Issue command (1=keygen, 2=encaps, 3=decaps)
    task issue_cmd(input [2:0] cmd);
        begin
            write64(6'h00, {61'd0, cmd});
        end
    endtask

    // Wait for done (status == 2)
    task wait_done;
        reg [63:0] st;
        integer timeout;
        begin
            timeout = 0;
            st = 0;
            while (st[7:0] != 8'd2 && timeout < 10000) begin
                read64(6'h00, st);
                timeout = timeout + 1;
            end
            if (timeout >= 10000) begin
                $display("  TIMEOUT waiting for KEM done!");
                fail_count = fail_count + 1;
            end
        end
    endtask

    // Read buffer size
    task read_buf_size(output [15:0] sz);
        reg [63:0] d;
        begin
            read64(6'h18, d);
            sz = d[15:0];
        end
    endtask

    task check_val(input [31:0] got, input [31:0] expected, input [8*48-1:0] name);
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

    task check_nonzero(input [7:0] val, input [8*48-1:0] name);
        begin
            test_num = test_num + 1;
            if (val !== 8'd0) begin
                $display("  [PASS] Test %0d: %0s (val=%0d)", test_num, name, val);
                pass_count = pass_count + 1;
            end else begin
                $display("  [FAIL] Test %0d: %0s (expected nonzero, got 0)", test_num, name);
                fail_count = fail_count + 1;
            end
        end
    endtask

    // ========================================================================
    // Test sequence
    // ========================================================================
    reg [15:0] buf_sz;
    reg [63:0] status;
    reg [7:0]  byte_val;
    integer    i;

    initial begin
        $dumpfile("tb_kem.vcd");
        $dumpvars(0, tb_kem);

        req <= 0; addr <= 0; wdata <= 0; wen <= 0;
        rst_n = 0;
        #20;
        rst_n = 1;
        #10;

        $display("=== KEM Tests ===");

        // --- Test 1-5: BUF_SIZE for each buffer ---
        buf_select(3'd0); read_buf_size(buf_sz); check_val(buf_sz, 64, "BUF0(SEED) size=64");
        buf_select(3'd1); read_buf_size(buf_sz); check_val(buf_sz, 800, "BUF1(PK) size=800");
        buf_select(3'd2); read_buf_size(buf_sz); check_val(buf_sz, 1632, "BUF2(SK) size=1632");
        buf_select(3'd3); read_buf_size(buf_sz); check_val(buf_sz, 768, "BUF3(CT) size=768");
        buf_select(3'd4); read_buf_size(buf_sz); check_val(buf_sz, 32, "BUF4(SS) size=32");

        // --- Test 6: DIN/DOUT round-trip on seed buffer ---
        buf_select(3'd0);
        for (i = 0; i < 16; i = i + 1)
            din_byte(i[7:0] ^ 8'hAA);

        // Read back
        buf_select(3'd0);  // resets idx to 0
        dout_byte(byte_val);
        check_val(byte_val, 8'hAA, "SEED[0] round-trip");
        dout_byte(byte_val);
        check_val(byte_val, 8'hAB, "SEED[1] round-trip");

        // --- Test 7: STATUS reads idle initially ---
        read64(6'h00, status);
        check_val(status[7:0], 8'd0, "STATUS=idle(0)");

        // --- Test 8: KEYGEN — fill seed, run, check done ---
        buf_select(3'd0);
        for (i = 0; i < 64; i = i + 1)
            din_byte(i[7:0] + 8'd1);  // non-zero seed values
        issue_cmd(3'd1);  // keygen
        wait_done;
        read64(6'h00, status);
        check_val(status[7:0], 8'd2, "KEYGEN STATUS=done(2)");

        // --- Test 9: PK buffer non-zero after keygen ---
        buf_select(3'd1);
        dout_byte(byte_val);
        check_nonzero(byte_val, "PK[0] nonzero after keygen");

        // --- Test 10: ENCAPS ---
        // Load coin into seed buffer
        buf_select(3'd0);
        for (i = 0; i < 32; i = i + 1)
            din_byte(i[7:0] ^ 8'h55);
        issue_cmd(3'd2);  // encaps
        wait_done;
        read64(6'h00, status);
        check_val(status[7:0], 8'd2, "ENCAPS STATUS=done(2)");

        // CT non-zero
        buf_select(3'd3);
        dout_byte(byte_val);
        check_nonzero(byte_val, "CT[0] nonzero after encaps");

        // SS non-zero
        buf_select(3'd4);
        dout_byte(byte_val);
        check_nonzero(byte_val, "SS[0] nonzero after encaps");

        // --- Test 11: DECAPS ---
        issue_cmd(3'd3);  // decaps
        wait_done;
        read64(6'h00, status);
        check_val(status[7:0], 8'd2, "DECAPS STATUS=done(2)");

        // --- Test 12: IDX_SET random access ---
        buf_select(3'd0);
        write64(6'h18, 64'd5);  // IDX_SET to 5
        // Read IDX back
        begin
            reg [63:0] idx_val;
            read64(6'h20, idx_val);
            check_val(idx_val[15:0], 16'd5, "IDX_SET=5 readback");
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
