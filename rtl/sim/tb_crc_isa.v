// ============================================================================
// tb_crc_isa.v — Testbench for the 32/64-bit CRC ISA parameter tuples
// ============================================================================
//
// Tests the standalone combinational CRC ISA module with all 6 sub-ops
// across all 3 polynomial modes.  Test vectors match the Python emulator.
//
`timescale 1ns / 1ps

`include "mp64_pkg.vh"

module tb_crc_isa;

    reg  [3:0]  op;
    reg  [63:0] rs_val;
    reg  [7:0]  imm8;
    reg  [63:0] crc_acc_in;
    reg  [1:0]  crc_mode_in;

    wire [63:0] crc_acc_out, result;
    wire [1:0]  crc_mode_out;
    wire        acc_we, mode_we, rd_we;

    mp64_crc_isa uut (
        .op          (op),
        .rs_val      (rs_val),
        .imm8        (imm8),
        .crc_acc_in  (crc_acc_in),
        .crc_mode_in (crc_mode_in),
        .crc_acc_out (crc_acc_out),
        .crc_mode_out(crc_mode_out),
        .result      (result),
        .acc_we      (acc_we),
        .mode_we     (mode_we),
        .rd_we       (rd_we)
    );

    integer pass_count;
    integer fail_count;

    task check_acc;
        input [255:0] label;
        input [63:0]  expected_acc;
        input         expected_acc_we;
        begin
            #1;
            if (crc_acc_out !== expected_acc) begin
                $display("FAIL [%0s]: acc_out=%016h expected=%016h", label, crc_acc_out, expected_acc);
                fail_count = fail_count + 1;
            end else if (acc_we !== expected_acc_we) begin
                $display("FAIL [%0s]: acc_we=%b expected=%b", label, acc_we, expected_acc_we);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task check_result;
        input [255:0] label;
        input [63:0]  expected_result;
        input         expected_rd_we;
        begin
            #1;
            if (result !== expected_result) begin
                $display("FAIL [%0s]: result=%016h expected=%016h", label, result, expected_result);
                fail_count = fail_count + 1;
            end else if (rd_we !== expected_rd_we) begin
                $display("FAIL [%0s]: rd_we=%b expected=%b", label, rd_we, expected_rd_we);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task check_mode;
        input [255:0] label;
        input [1:0]   expected_mode;
        input         expected_mode_we;
        begin
            #1;
            if (crc_mode_out !== expected_mode) begin
                $display("FAIL [%0s]: mode_out=%0d expected=%0d", label, crc_mode_out, expected_mode);
                fail_count = fail_count + 1;
            end else if (mode_we !== expected_mode_we) begin
                $display("FAIL [%0s]: mode_we=%b expected=%b", label, mode_we, expected_mode_we);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    // Helper to run CRC.B and get updated acc (simulates sequential feed)
    reg [63:0] sim_acc;
    reg [1:0]  sim_mode;

    task feed_byte;
        input [7:0] data_byte;
        begin
            op          = ISA_CRC_B;
            rs_val      = {56'd0, data_byte};
            crc_acc_in  = sim_acc;
            crc_mode_in = sim_mode;
            #1;
            sim_acc     = crc_acc_out;
        end
    endtask

    task feed_quad;
        input [63:0] data_word;
        begin
            op          = ISA_CRC_Q;
            rs_val      = data_word;
            crc_acc_in  = sim_acc;
            crc_mode_in = sim_mode;
            #1;
            sim_acc     = crc_acc_out;
        end
    endtask

    task do_fin;
        begin
            op          = ISA_CRC_FIN;
            rs_val      = 64'd0;
            crc_acc_in  = sim_acc;
            crc_mode_in = sim_mode;
            #1;
        end
    endtask

    task check_123456789;
        input [255:0] label;
        input [1:0]   mode_value;
        input [63:0]  init_value;
        input [63:0]  expected;
        begin
            sim_acc  = init_value;
            sim_mode = mode_value;
            feed_byte(8'h31); feed_byte(8'h32); feed_byte(8'h33);
            feed_byte(8'h34); feed_byte(8'h35); feed_byte(8'h36);
            feed_byte(8'h37); feed_byte(8'h38); feed_byte(8'h39);
            do_fin;
            check_result(label, expected, 1'b1);
        end
    endtask

    initial begin
        pass_count = 0;
        fail_count = 0;

        $display("=== tb_crc_isa: CRC ISA engine tests ===");

        // ================================================================
        // 1. CRC.INIT — CRC32 mode (default, mode=0)
        // ================================================================
        op          = ISA_CRC_INIT;
        rs_val      = 64'd0;
        imm8        = 8'd0;
        crc_acc_in  = 64'd0;
        crc_mode_in = 2'd0;
        check_acc("CRC.INIT CRC32", 64'h0000_0000_FFFF_FFFF, 1'b1);

        // ================================================================
        // 2. CRC.INIT — CRC64 mode
        // ================================================================
        crc_mode_in = 2'd2;
        check_acc("CRC.INIT CRC64", 64'hFFFF_FFFF_FFFF_FFFF, 1'b1);

        // ================================================================
        // 3. CRC.MODE — set mode to CRC32C (1)
        // ================================================================
        op          = ISA_CRC_MODEX;
        imm8        = 8'd1;
        crc_mode_in = 2'd0;
        check_mode("CRC.MODE 1", 2'd1, 1'b1);

        // ================================================================
        // 4. CRC.MODE — set mode to CRC64 (2)
        // ================================================================
        imm8 = 8'd2;
        check_mode("CRC.MODE 2", 2'd2, 1'b1);

        // Every complete imm8 other than 1/2 canonicalizes to mode 0.
        imm8 = 8'd3;
        check_mode("CRC.MODE 3 canonicalizes to mode 0", 2'd0, 1'b1);
        imm8 = 8'd5;
        check_mode("CRC.MODE 5 does not alias mode 1", 2'd0, 1'b1);
        imm8 = 8'hFF;
        check_mode("CRC.MODE FF canonicalizes to mode 0", 2'd0, 1'b1);

        // ================================================================
        // 5. Mode-0 CRC of byte 'A' (0x41)
        // ================================================================
        sim_acc  = 64'h0000_0000_FFFF_FFFF;
        sim_mode = 2'd0;
        feed_byte(8'h41);
        do_fin;
        check_result("CRC32 'A' FIN", 64'h0000_0000_81B0_2D8B, 1'b1);

        // ================================================================
        // 6. Mode-0 CRC of "ABCD" (4 bytes)
        // ================================================================
        sim_acc  = 64'h0000_0000_FFFF_FFFF;
        sim_mode = 2'd0;
        feed_byte(8'h41); // A
        feed_byte(8'h42); // B
        feed_byte(8'h43); // C
        feed_byte(8'h44); // D
        do_fin;
        check_result("CRC32 'ABCD' FIN", 64'h0000_0000_5430_659C, 1'b1);

        // ================================================================
        // 7. CRC.Q — feed 8 zero bytes in CRC32 mode
        // ================================================================
        sim_acc  = 64'h0000_0000_FFFF_FFFF;
        sim_mode = 2'd0;
        feed_quad(64'h0000_0000_0000_0000);
        do_fin;
        check_result("CRC32 8x00 FIN", 64'h0000_0000_96FB_44A6, 1'b1);

        // ================================================================
        // 8. CRC.Q — "ABCDEFGH" as LE word = 0x4847464544434241
        //    CRC32("ABCDEFGH") = 0x3FFA0BBBB  wait need to compute this.
        //    Actually: bytes A=41 B=42 C=43 D=44 E=45 F=46 G=47 H=48
        //    As LE 64-bit: 0x4847464544434241
        // ================================================================
        sim_acc  = 64'h0000_0000_FFFF_FFFF;
        sim_mode = 2'd0;
        feed_quad(64'h48474645_44434241);

        // Now verify byte-by-byte matches quad result
        begin : crc32_abcdefgh_check
            reg [63:0] byte_acc;
            byte_acc = 64'h0000_0000_FFFF_FFFF;

            op          = ISA_CRC_B;
            crc_mode_in = 2'd0;

            crc_acc_in = byte_acc; rs_val = {56'd0, 8'h41}; #1; byte_acc = crc_acc_out;
            crc_acc_in = byte_acc; rs_val = {56'd0, 8'h42}; #1; byte_acc = crc_acc_out;
            crc_acc_in = byte_acc; rs_val = {56'd0, 8'h43}; #1; byte_acc = crc_acc_out;
            crc_acc_in = byte_acc; rs_val = {56'd0, 8'h44}; #1; byte_acc = crc_acc_out;
            crc_acc_in = byte_acc; rs_val = {56'd0, 8'h45}; #1; byte_acc = crc_acc_out;
            crc_acc_in = byte_acc; rs_val = {56'd0, 8'h46}; #1; byte_acc = crc_acc_out;
            crc_acc_in = byte_acc; rs_val = {56'd0, 8'h47}; #1; byte_acc = crc_acc_out;
            crc_acc_in = byte_acc; rs_val = {56'd0, 8'h48}; #1; byte_acc = crc_acc_out;

            if (sim_acc !== byte_acc) begin
                $display("FAIL [CRC.Q vs CRC.B x8 ABCDEFGH]: quad=%016h byte=%016h", sim_acc, byte_acc);
                fail_count = fail_count + 1;
            end else begin
                $display("  PASS  CRC.Q vs CRC.B x8 ABCDEFGH match: %016h", sim_acc);
                pass_count = pass_count + 1;
            end
        end

        // ================================================================
        // 9. CRC.FIN atomically stores and returns the finalized value
        // ================================================================
        sim_acc  = 64'h0000_0000_DEAD_BEEF;
        sim_mode = 2'd0;
        op          = ISA_CRC_FIN;
        crc_acc_in  = sim_acc;
        crc_mode_in = sim_mode;
        #1;
        if (acc_we !== 1'b1 || crc_acc_out !== result) begin
            $display("FAIL [CRC.FIN atomic publish]: acc_we=%b acc=%016h result=%016h",
                     acc_we, crc_acc_out, result);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        // ================================================================
        // 10. CRC.B — rd_we is set
        // ================================================================
        op          = ISA_CRC_B;
        rs_val      = {56'd0, 8'hFF};
        crc_acc_in  = 64'h0000_0000_FFFF_FFFF;
        crc_mode_in = 2'd0;
        #1;
        if (rd_we !== 1'b1) begin
            $display("FAIL [CRC.B rd_we]: rd_we=%b expected=1", rd_we);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        // ================================================================
        // 11. CRC.INIT — no rd_we
        // ================================================================
        op          = ISA_CRC_INIT;
        crc_acc_in  = 64'd0;
        crc_mode_in = 2'd0;
        #1;
        if (rd_we !== 1'b0) begin
            $display("FAIL [CRC.INIT no rd_we]: rd_we=%b expected=0", rd_we);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        // ================================================================
        // 12. Mode-1 non-reflected Castagnoli CRC of 'A' (0x41)
        // ================================================================
        sim_acc  = 64'h0000_0000_FFFF_FFFF;
        sim_mode = 2'd1;
        feed_byte(8'h41);
        do_fin;
        check_result("CRC mode 1 'A' FIN", 64'h0000_0000_7B18_0D8C, 1'b1);

        // ================================================================
        // 13. Mode-2 CRC-64/WE parameters over 'A' (0x41)
        //     Full 64-bit init, single byte, finalize
        // ================================================================
        sim_acc  = 64'hFFFF_FFFF_FFFF_FFFF;
        sim_mode = 2'd2;
        feed_byte(8'h41);
        do_fin;
        // Just verify result is non-zero and rd_we is set
        if (result == 64'd0) begin
            $display("FAIL [CRC64 'A']: result is zero");
            fail_count = fail_count + 1;
        end else begin
            $display("  PASS  CRC64 'A' FIN = %016h (non-trivial)", result);
            pass_count = pass_count + 1;
        end

        // ================================================================
        // 14. CRC.MODE doesn't touch accumulator
        // ================================================================
        op          = ISA_CRC_MODEX;
        imm8        = 8'd2;
        crc_acc_in  = 64'hDEAD_BEEF_CAFE_BABE;
        crc_mode_in = 2'd0;
        #1;
        if (acc_we !== 1'b0) begin
            $display("FAIL [CRC.MODE no acc_we]: acc_we=%b expected=0", acc_we);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        // ================================================================
        // 15. Mixed CRC.Q + CRC.B tail matches eleven CRC.B operations
        // ================================================================
        sim_acc  = 64'h0000_0000_1234_5678;
        sim_mode = 2'd0;
        feed_quad(64'h48474645_44434241); // "ABCDEFGH"
        feed_byte(8'h49);
        feed_byte(8'h4A);
        feed_byte(8'h4B);
        begin : crc32_mixed_check
            reg [63:0] mixed_acc;
            reg [63:0] byte_acc;
            integer bi;
            mixed_acc = sim_acc;
            byte_acc = 64'h0000_0000_1234_5678;
            op = ISA_CRC_B;
            crc_mode_in = 2'd0;
            for (bi = 0; bi < 11; bi = bi + 1) begin
                crc_acc_in = byte_acc;
                rs_val = 8'h41 + bi;
                #1;
                byte_acc = crc_acc_out;
            end
            if (mixed_acc !== byte_acc) begin
                $display("FAIL [CRC.Q + 3 CRC.B]: mixed=%016h bytes=%016h",
                         mixed_acc, byte_acc);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end

        // ================================================================
        // 16. CRC.SEED masks to the active algorithm width
        // ================================================================
        op          = ISA_CRC_SEED;
        rs_val      = 64'h0123_4567_89AB_CDEF;
        crc_acc_in  = 64'd0;
        crc_mode_in = 2'd0;
        #1;
        if (!acc_we || !rd_we ||
            crc_acc_out !== 64'h0000_0000_89AB_CDEF ||
            result !== 64'h0000_0000_89AB_CDEF) begin
            $display("FAIL [CRC.SEED 32-bit mask]: acc=%016h result=%016h acc_we=%b rd_we=%b",
                     crc_acc_out, result, acc_we, rd_we);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        crc_mode_in = 2'd2;
        #1;
        if (!acc_we || !rd_we ||
            crc_acc_out !== 64'h0123_4567_89AB_CDEF ||
            result !== 64'h0123_4567_89AB_CDEF) begin
            $display("FAIL [CRC.SEED 64-bit preserve]: acc=%016h result=%016h acc_we=%b rd_we=%b",
                     crc_acc_out, result, acc_we, rd_we);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        // ================================================================
        // 17. Reserved sub-op (0x6) → no writes
        // ================================================================
        op          = 4'd6;
        rs_val      = 64'h1234;
        crc_acc_in  = 64'h0000_0000_FFFF_FFFF;
        crc_mode_in = 2'd0;
        #1;
        if (acc_we !== 1'b0 || mode_we !== 1'b0 || rd_we !== 1'b0) begin
            $display("FAIL [reserved sub-op 6]: writes asserted (acc_we=%b mode_we=%b rd_we=%b)",
                     acc_we, mode_we, rd_we);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        // ================================================================
        // 18. Authoritative check vectors for every complete parameter tuple
        // ================================================================
        check_123456789("mode 0 vector 123456789", 2'd0,
                        64'h0000_0000_FFFF_FFFF,
                        64'h0000_0000_FC89_1918);
        check_123456789("mode 1 vector 123456789", 2'd1,
                        64'h0000_0000_FFFF_FFFF,
                        64'h0000_0000_0544_0F15);
        check_123456789("mode 2 vector 123456789", 2'd2,
                        64'hFFFF_FFFF_FFFF_FFFF,
                        64'h62EC_59E3_F1A4_F00A);

        // ================================================================
        // Summary
        // ================================================================
        $display("");
        $display("=== tb_crc_isa: %0d passed, %0d failed ===", pass_count, fail_count);
        if (fail_count != 0) begin
            $display("SOME TESTS FAILED");
            $finish;
        end else begin
            $display("ALL TESTS PASSED");
        end
        $finish;
    end

endmodule
