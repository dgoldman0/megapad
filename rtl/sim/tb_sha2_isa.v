// ============================================================================
// tb_sha2_isa.v — Testbench for mp64_sha2_isa (SHA-256 compression engine)
// ============================================================================
//
// Verifies the 64-round SHA-256 compression engine using NIST FIPS 180-4
// test vectors.  Tests:
//   1. SHA-256("abc")           — single-block, 3 bytes
//   2. SHA-256("")              — empty message (just padding block)
//   3. SHA-256("abcdbcdecdef..") — NIST two-block vector (448-bit msg)
//   4. Compression idempotency  — same inputs → same outputs
//
// The testbench drives the engine at the compression level:
//   - Pre-pads messages into 512-bit blocks (16 × 32-bit big-endian W words)
//   - Loads FIPS 180-4 SHA-256 IV into h_in[0..7]
//   - Asserts `start`, waits for `done`, checks h_out[0..7]
//
`timescale 1ns / 1ps

`include "mp64_pkg.vh"

module tb_sha2_isa;

    reg         clk;
    reg         rst_n;

    // Command
    reg         start;

    // W inputs
    reg  [31:0] w_in [0:15];

    // Hash state inputs (IV or intermediate)
    reg  [31:0] h_in [0:7];

    // Outputs
    wire [31:0] h_out [0:7];
    wire        h_we;
    wire        busy;
    wire        done;

    mp64_sha2_isa uut (
        .clk    (clk),
        .rst_n  (rst_n),
        .start  (start),
        .w_in   (w_in),
        .h_in   (h_in),
        .h_out  (h_out),
        .h_we   (h_we),
        .busy   (busy),
        .done   (done)
    );

    // Clock: 10 ns period
    always #5 clk = ~clk;

    integer pass_count;
    integer fail_count;
    integer i;

    // SHA-256 IV (FIPS 180-4 §5.3.3)
    task load_iv;
        begin
            h_in[0] = 32'h6a09e667;
            h_in[1] = 32'hbb67ae85;
            h_in[2] = 32'h3c6ef372;
            h_in[3] = 32'ha54ff53a;
            h_in[4] = 32'h510e527f;
            h_in[5] = 32'h9b05688c;
            h_in[6] = 32'h1f83d9ab;
            h_in[7] = 32'h5be0cd19;
        end
    endtask

    // Clear W buffer
    task clear_w;
        integer j;
        begin
            for (j = 0; j < 16; j = j + 1)
                w_in[j] = 32'd0;
        end
    endtask

    // Run one compression (start pulse, wait for done)
    task run_compress;
        begin
            @(posedge clk);
            start <= 1'b1;
            @(posedge clk);
            start <= 1'b0;
            // Wait for done
            while (!done) @(posedge clk);
            @(posedge clk); // one extra cycle for h_out to be latched
        end
    endtask

    // Check 8 × 32-bit hash against expected
    task check_hash;
        input [255:0] label;
        input [31:0]  exp0, exp1, exp2, exp3, exp4, exp5, exp6, exp7;
        begin
            if (h_out[0] !== exp0 || h_out[1] !== exp1 ||
                h_out[2] !== exp2 || h_out[3] !== exp3 ||
                h_out[4] !== exp4 || h_out[5] !== exp5 ||
                h_out[6] !== exp6 || h_out[7] !== exp7) begin
                $display("FAIL [%0s]:", label);
                $display("  got:      %08h %08h %08h %08h %08h %08h %08h %08h",
                         h_out[0], h_out[1], h_out[2], h_out[3],
                         h_out[4], h_out[5], h_out[6], h_out[7]);
                $display("  expected: %08h %08h %08h %08h %08h %08h %08h %08h",
                         exp0, exp1, exp2, exp3, exp4, exp5, exp6, exp7);
                fail_count = fail_count + 1;
            end else begin
                $display("  PASS  %0s", label);
                pass_count = pass_count + 1;
            end
        end
    endtask

    initial begin
        clk   = 0;
        rst_n = 0;
        start = 0;
        pass_count = 0;
        fail_count = 0;
        clear_w;
        load_iv;

        // Reset
        #20;
        rst_n = 1;
        #10;

        $display("=== tb_sha2_isa: SHA-256 compression engine tests ===");

        // ================================================================
        // Test 1: SHA-256("abc")
        //   NIST FIPS 180-4 example B.1
        //   Message: 0x61626380 00000000 ... 00000018  (single 512-bit block)
        //   Expected digest:
        //     ba7816bf 8f01cfea 414140de 5dae2223
        //     b00361a3 96177a9c b410ff61 f20015ad
        // ================================================================
        $display("--- Test 1: SHA-256(\"abc\") ---");
        clear_w;
        load_iv;
        // "abc" = 0x61 0x62 0x63, then 0x80 pad, zeros, length = 24 bits = 0x18
        w_in[0]  = 32'h61626380;
        w_in[1]  = 32'h00000000;
        w_in[2]  = 32'h00000000;
        w_in[3]  = 32'h00000000;
        w_in[4]  = 32'h00000000;
        w_in[5]  = 32'h00000000;
        w_in[6]  = 32'h00000000;
        w_in[7]  = 32'h00000000;
        w_in[8]  = 32'h00000000;
        w_in[9]  = 32'h00000000;
        w_in[10] = 32'h00000000;
        w_in[11] = 32'h00000000;
        w_in[12] = 32'h00000000;
        w_in[13] = 32'h00000000;
        w_in[14] = 32'h00000000;
        w_in[15] = 32'h00000018;

        run_compress;
        check_hash("SHA-256(abc)",
            32'hba7816bf, 32'h8f01cfea, 32'h414140de, 32'h5dae2223,
            32'hb00361a3, 32'h96177a9c, 32'hb410ff61, 32'hf20015ad);

        // ================================================================
        // Test 2: SHA-256("")
        //   Empty message — pad block only
        //   Message: 0x80000000 00000000 ... 00000000  (length = 0 bits)
        //   Expected digest:
        //     e3b0c442 98fc1c14 9afbf4c8 996fb924
        //     27ae41e4 649b934c a495991b 7852b855
        // ================================================================
        $display("--- Test 2: SHA-256(\"\") ---");
        clear_w;
        load_iv;
        w_in[0]  = 32'h80000000;
        // w_in[1..14] = 0 (already cleared)
        w_in[15] = 32'h00000000;  // length = 0 bits

        run_compress;
        check_hash("SHA-256(empty)",
            32'he3b0c442, 32'h98fc1c14, 32'h9afbf4c8, 32'h996fb924,
            32'h27ae41e4, 32'h649b934c, 32'ha495991b, 32'h7852b855);

        // ================================================================
        // Test 3: SHA-256("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
        //   NIST FIPS 180-4 example B.2 — 448-bit message (56 bytes)
        //   Two blocks: first block = 64 bytes of message data,
        //               second block = remaining 0 bytes + padding
        //   Wait — 56 bytes means the first block has all data + padding.
        //   Actually 56 bytes fits in one block: 56 bytes + 0x80 + 7 zeros
        //   = 64 bytes including the 8-byte length field.
        //   NO — 56 bytes = 448 bits.  Padding adds 0x80 (1 byte) + length
        //   (8 bytes) = 9 bytes minimum.  56+9 = 65 > 64, so TWO blocks.
        //
        //   Block 1: bytes 0..63 of the message (first 64 bytes)
        //     But we only have 56 bytes!  So block 1 = first 64 bytes?
        //     No: 56 < 64, so block 1 = 56 message bytes + 0x80 + 7 pad
        //       = 64.  But that leaves no room for the 8-byte length.
        //     So: block 1 = 56 bytes + 0x80 + 0x00*7 (no length yet)
        //         block 2 = 0x00*56 + 8-byte length
        //     Actually: 56 + 1 (0x80) = 57.  Need to reach 64: 7 more zeros.
        //     57 + 7 = 64.  That fills block 1.  Length goes in block 2.
        //
        //   Expected digest:
        //     248d6a61 d20638b8 e5c02693 0c3e6039
        //     a33ce459 64ff2167 f6ecedd4 19db06c1
        // ================================================================
        $display("--- Test 3: SHA-256(448-bit NIST vector) ---");

        // Block 1: "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" + 0x80 + pad
        // ASCII: a=61 b=62 c=63 d=64 e=65 f=66 g=67 h=68 i=69 j=6a k=6b l=6c m=6d n=6e o=6f p=70 q=71
        // "abcd" "bcde" "cdef" "defg" "efgh" "fghi" "ghij" "hijk"
        // "ijkl" "jklm" "klmn" "lmno" "mnop" "nopq" 80 00 ...
        clear_w;
        load_iv;
        w_in[0]  = 32'h61626364;  // "abcd"
        w_in[1]  = 32'h62636465;  // "bcde"
        w_in[2]  = 32'h63646566;  // "cdef"
        w_in[3]  = 32'h64656667;  // "defg"
        w_in[4]  = 32'h65666768;  // "efgh"
        w_in[5]  = 32'h66676869;  // "fghi"
        w_in[6]  = 32'h6768696a;  // "ghij"
        w_in[7]  = 32'h68696a6b;  // "hijk"
        w_in[8]  = 32'h696a6b6c;  // "ijkl"
        w_in[9]  = 32'h6a6b6c6d;  // "jklm"
        w_in[10] = 32'h6b6c6d6e;  // "klmn"
        w_in[11] = 32'h6c6d6e6f;  // "lmno"
        w_in[12] = 32'h6d6e6f70;  // "mnop"
        w_in[13] = 32'h6e6f7071;  // "nopq"
        w_in[14] = 32'h80000000;  // padding: 0x80
        w_in[15] = 32'h00000000;  // zeros (length goes in block 2)

        run_compress;

        // Save intermediate hash for block 2
        // h_in is driven from h_out for second block
        for (i = 0; i < 8; i = i + 1)
            h_in[i] = h_out[i];

        // Block 2: all zeros except length field at the end
        //   Message length = 56 bytes = 448 bits = 0x1C0
        clear_w;
        w_in[14] = 32'h00000000;  // high 32 bits of bit-length
        w_in[15] = 32'h000001c0;  // low 32 bits: 448 decimal = 0x1C0

        run_compress;
        check_hash("SHA-256(448-bit NIST)",
            32'h248d6a61, 32'hd20638b8, 32'he5c02693, 32'h0c3e6039,
            32'ha33ce459, 32'h64ff2167, 32'hf6ecedd4, 32'h19db06c1);

        // ================================================================
        // Test 4: Compression idempotency — running "abc" again should
        //         produce the same digest
        // ================================================================
        $display("--- Test 4: Idempotency ---");
        clear_w;
        load_iv;
        w_in[0]  = 32'h61626380;
        w_in[15] = 32'h00000018;

        run_compress;
        check_hash("SHA-256(abc) repeat",
            32'hba7816bf, 32'h8f01cfea, 32'h414140de, 32'h5dae2223,
            32'hb00361a3, 32'h96177a9c, 32'hb410ff61, 32'hf20015ad);

        // ================================================================
        // Test 5: Busy/done handshake — start during idle, verify busy
        //         goes high immediately, done pulses at the end
        // ================================================================
        $display("--- Test 5: Busy/done handshake ---");
        clear_w;
        load_iv;
        w_in[0]  = 32'h61626380;
        w_in[15] = 32'h00000018;

        @(posedge clk);
        start <= 1'b1;
        @(posedge clk);
        start <= 1'b0;
        // Check busy goes high within 2 cycles
        @(posedge clk);
        if (busy !== 1'b1) begin
            $display("FAIL [busy assertion]: busy=%b expected=1 after start", busy);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end
        // Wait for done
        while (!done) @(posedge clk);
        // After done, busy should drop
        @(posedge clk);
        if (busy !== 1'b0) begin
            $display("FAIL [busy deassert]: busy=%b expected=0 after done", busy);
            fail_count = fail_count + 1;
        end else begin
            $display("  PASS  busy/done handshake");
            pass_count = pass_count + 1;
        end

        // ================================================================
        // Test 6: h_we pulses exactly once on completion
        // ================================================================
        $display("--- Test 6: h_we pulse ---");
        clear_w;
        load_iv;
        w_in[0]  = 32'h80000000;
        w_in[15] = 32'h00000000;

        begin : hwe_count_block
            integer hwe_count;
            hwe_count = 0;
            @(posedge clk);
            start <= 1'b1;
            @(posedge clk);
            start <= 1'b0;
            while (!done) begin
                @(posedge clk);
                if (h_we) hwe_count = hwe_count + 1;
            end
            // Count the done cycle too
            @(posedge clk);
            if (h_we) hwe_count = hwe_count + 1;
            if (hwe_count !== 1) begin
                $display("FAIL [h_we count]: h_we pulsed %0d times, expected 1", hwe_count);
                fail_count = fail_count + 1;
            end else begin
                $display("  PASS  h_we pulses exactly once");
                pass_count = pass_count + 1;
            end
        end

        // ================================================================
        // Summary
        // ================================================================
        $display("");
        $display("=== tb_sha2_isa: %0d passed, %0d failed ===", pass_count, fail_count);
        if (fail_count != 0) begin
            $display("SOME TESTS FAILED");
            $finish;
        end else begin
            $display("ALL TESTS PASSED");
        end
        $finish;
    end

endmodule
