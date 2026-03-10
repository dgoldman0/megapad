// ============================================================================
// tb_bitfield.v — Testbench for mp64_bitfield (Bitfield ALU, 8 ops)
// ============================================================================
//
// Tests the standalone combinational bitfield module with both
// Tier 1 (POPCNT/CLZ/CTZ/BITREV) and Tier 2 (BEXT/BDEP/RORI/BSWAP)
// operations.  Test vectors match the emulator's test_bitfield_alu.
//
`timescale 1ns / 1ps

`include "mp64_pkg.vh"

module tb_bitfield;

    reg  [2:0]  op;
    reg  [63:0] a, b;
    reg  [5:0]  imm;
    wire [63:0] result;
    wire        flag_z, flag_n;

    // Full-featured instance (ENABLE_TIER2 = 1)
    mp64_bitfield #(.ENABLE_TIER2(1)) uut (
        .op     (op),
        .a      (a),
        .b      (b),
        .imm    (imm),
        .result (result),
        .flag_z (flag_z),
        .flag_n (flag_n)
    );

    // Tier-1-only instance for tiering verification
    wire [63:0] tier1_result;
    wire        tier1_z, tier1_n;
    mp64_bitfield #(.ENABLE_TIER2(0)) u_tier1 (
        .op     (op),
        .a      (a),
        .b      (b),
        .imm    (6'd0),
        .result (tier1_result),
        .flag_z (tier1_z),
        .flag_n (tier1_n)
    );

    integer pass_count;
    integer fail_count;

    task check;
        input [255:0] label;
        input [63:0]  expected;
        input         expected_z;
        input         expected_n;
        begin
            #1; // let combinational settle
            if (result !== expected) begin
                $display("FAIL [%0s]: result=%h expected=%h", label, result, expected);
                fail_count = fail_count + 1;
            end else if (flag_z !== expected_z) begin
                $display("FAIL [%0s]: Z=%b expected=%b", label, flag_z, expected_z);
                fail_count = fail_count + 1;
            end else if (flag_n !== expected_n) begin
                $display("FAIL [%0s]: N=%b expected=%b", label, flag_n, expected_n);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    // Check that tier1-only instance returns zero for Tier 2 ops
    task check_tier1_stub;
        input [255:0] label;
        begin
            #1;
            if (tier1_result !== 64'd0) begin
                $display("FAIL [%0s]: tier1_result=%h expected=0 (gated)", label, tier1_result);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    initial begin
        $dumpfile("tb_bitfield.vcd");
        $dumpvars(0, tb_bitfield);

        pass_count = 0;
        fail_count = 0;
        a   = 64'd0;
        b   = 64'd0;
        imm = 6'd0;

        // ================================================================
        // 1. POPCNT (op=0)
        // ================================================================
        op = BF_POPCNT; b = 64'hFFFF_FFFF_FFFF_FFFF;
        check("POPCNT allones", 64'd64, 1'b0, 1'b0);

        op = BF_POPCNT; b = 64'd0;
        check("POPCNT zero", 64'd0, 1'b1, 1'b0);

        op = BF_POPCNT; b = 64'hAAAA_AAAA_AAAA_AAAA;
        check("POPCNT alternating", 64'd32, 1'b0, 1'b0);

        op = BF_POPCNT; b = 64'd1;
        check("POPCNT one", 64'd1, 1'b0, 1'b0);

        op = BF_POPCNT; b = 64'h8000_0000_0000_0000;
        check("POPCNT msb", 64'd1, 1'b0, 1'b0);

        op = BF_POPCNT; b = 64'hDEAD_BEEF_CAFE_BABE;
        check("POPCNT deadbeef", 64'd46, 1'b0, 1'b0);

        // ================================================================
        // 2. CLZ (op=1)
        // ================================================================
        op = BF_CLZ; b = 64'd0;
        check("CLZ zero", 64'd64, 1'b0, 1'b0);

        op = BF_CLZ; b = 64'h8000_0000_0000_0000;
        check("CLZ msb", 64'd0, 1'b1, 1'b0);

        op = BF_CLZ; b = 64'd1;
        check("CLZ one", 64'd63, 1'b0, 1'b0);

        op = BF_CLZ; b = 64'h0000_0000_0000_FFFF;
        check("CLZ 16bits", 64'd48, 1'b0, 1'b0);

        op = BF_CLZ; b = 64'h0000_0001_0000_0000;
        check("CLZ bit32", 64'd31, 1'b0, 1'b0);

        op = BF_CLZ; b = 64'hFFFF_FFFF_FFFF_FFFF;
        check("CLZ allones", 64'd0, 1'b1, 1'b0);

        // ================================================================
        // 3. CTZ (op=2)
        // ================================================================
        op = BF_CTZ; b = 64'd0;
        check("CTZ zero", 64'd64, 1'b0, 1'b0);

        op = BF_CTZ; b = 64'd1;
        check("CTZ one", 64'd0, 1'b1, 1'b0);

        op = BF_CTZ; b = 64'h8000_0000_0000_0000;
        check("CTZ msb", 64'd63, 1'b0, 1'b0);

        op = BF_CTZ; b = 64'hFFFF_0000_0000_0000;
        check("CTZ upper48", 64'd48, 1'b0, 1'b0);

        op = BF_CTZ; b = 64'h0000_0000_0010_0000;
        check("CTZ bit20", 64'd20, 1'b0, 1'b0);

        op = BF_CTZ; b = 64'hFFFF_FFFF_FFFF_FFFF;
        check("CTZ allones", 64'd0, 1'b1, 1'b0);

        // ================================================================
        // 4. BITREV (op=3) — pure wiring
        // ================================================================
        op = BF_BITREV; b = 64'h8000_0000_0000_0000;
        check("BITREV msb->lsb", 64'd1, 1'b0, 1'b0);

        op = BF_BITREV; b = 64'd1;
        check("BITREV lsb->msb", 64'h8000_0000_0000_0000, 1'b0, 1'b1);

        op = BF_BITREV; b = 64'd0;
        check("BITREV zero", 64'd0, 1'b1, 1'b0);

        op = BF_BITREV; b = 64'hFFFF_FFFF_FFFF_FFFF;
        check("BITREV allones", 64'hFFFF_FFFF_FFFF_FFFF, 1'b0, 1'b1);

        op = BF_BITREV; b = 64'hF0F0_F0F0_0F0F_0F0F;
        check("BITREV pattern", 64'hF0F0_F0F0_0F0F_0F0F, 1'b0, 1'b1);

        op = BF_BITREV; b = 64'h0000_0000_0000_00FF;
        check("BITREV lowbyte", 64'hFF00_0000_0000_0000, 1'b0, 1'b1);

        // ================================================================
        // 5. BEXT (op=4) — bit extract / parallel gather
        // ================================================================
        // a = source, b = mask — collect bits from a at positions where b has 1s
        op = BF_BEXT; a = 64'hDEAD_BEEF_CAFE_BABE; b = 64'hFF00_0000_0000_0000;
        check("BEXT top byte", 64'hDE, 1'b0, 1'b0);

        op = BF_BEXT; a = 64'hFFFF_FFFF_FFFF_FFFF; b = 64'hAAAA_AAAA_AAAA_AAAA;
        check("BEXT alternating", 64'hFFFF_FFFF, 1'b0, 1'b0);

        op = BF_BEXT; a = 64'h1234_5678_9ABC_DEF0; b = 64'd0;
        check("BEXT zero mask", 64'd0, 1'b1, 1'b0);

        op = BF_BEXT; a = 64'hA5; b = 64'hFF;
        check("BEXT low byte", 64'hA5, 1'b0, 1'b0);

        // ================================================================
        // 6. BDEP (op=5) — bit deposit / parallel scatter
        // ================================================================
        // a = bits (low), b = mask — scatter low bits of a to positions where b has 1s
        op = BF_BDEP; a = 64'hDE; b = 64'hFF00_0000_0000_0000;
        check("BDEP top byte", 64'hDE00_0000_0000_0000, 1'b0, 1'b1);

        op = BF_BDEP; a = 64'hFFFF_FFFF; b = 64'hAAAA_AAAA_AAAA_AAAA;
        check("BDEP alternating", 64'hAAAA_AAAA_AAAA_AAAA, 1'b0, 1'b1);

        op = BF_BDEP; a = 64'h1234; b = 64'd0;
        check("BDEP zero mask", 64'd0, 1'b1, 1'b0);

        // ================================================================
        // 7. RORI (op=6) — rotate right by immediate
        // ================================================================
        op = BF_RORI; a = 64'h8000_0000_0000_0001; imm = 6'd1;
        check("RORI by 1", 64'hC000_0000_0000_0000, 1'b0, 1'b1);
        imm = 6'd0;

        op = BF_RORI; a = 64'hDEAD_BEEF_CAFE_BABE; imm = 6'd0;
        check("RORI by 0 (identity)", 64'hDEAD_BEEF_CAFE_BABE, 1'b0, 1'b1);
        imm = 6'd0;

        op = BF_RORI; a = 64'h0000_0000_0000_0001; imm = 6'd32;
        check("RORI by 32", 64'h0000_0001_0000_0000, 1'b0, 1'b0);
        imm = 6'd0;

        op = BF_RORI; a = 64'h0000_0000_0000_0001; imm = 6'd63;
        check("RORI by 63", 64'd2, 1'b0, 1'b0);
        imm = 6'd0;

        op = BF_RORI; a = 64'hFFFF_FFFF_FFFF_FFFF; imm = 6'd17;
        check("RORI allones", 64'hFFFF_FFFF_FFFF_FFFF, 1'b0, 1'b1);
        imm = 6'd0;

        // Round-trip: RORI(RORI(x, n), 64-n) == x
        op = BF_RORI; a = 64'hCAFE_BABE_DEAD_BEEF; imm = 6'd13;
        #1;
        begin : rori_roundtrip
            reg [63:0] mid;
            mid = result;
            a = mid; imm = 6'd51;   // 64 - 13 = 51
            #1;
            if (result !== 64'hCAFE_BABE_DEAD_BEEF) begin
                $display("FAIL [RORI roundtrip]: result=%h expected=CAFE_BABE_DEAD_BEEF", result);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
        imm = 6'd0;

        // ================================================================
        // 8. BSWAP (op=7) — byte-swap
        // ================================================================
        op = BF_BSWAP; b = 64'h0102_0304_0506_0708;
        check("BSWAP sequence", 64'h0807_0605_0403_0201, 1'b0, 1'b0);

        op = BF_BSWAP; b = 64'd0;
        check("BSWAP zero", 64'd0, 1'b1, 1'b0);

        op = BF_BSWAP; b = 64'hFFFF_FFFF_FFFF_FFFF;
        check("BSWAP allones", 64'hFFFF_FFFF_FFFF_FFFF, 1'b0, 1'b1);

        // Round-trip: BSWAP(BSWAP(x)) == x
        op = BF_BSWAP; b = 64'hDEAD_BEEF_CAFE_BABE;
        #1;
        begin : bswap_roundtrip
            reg [63:0] mid;
            mid = result;
            b = mid;
            #1;
            if (result !== 64'hDEAD_BEEF_CAFE_BABE) begin
                $display("FAIL [BSWAP roundtrip]: result=%h expected=DEAD_BEEF_CAFE_BABE", result);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end

        op = BF_BSWAP; b = 64'hFF00_0000_0000_0000;
        check("BSWAP msb byte", 64'h0000_0000_0000_00FF, 1'b0, 1'b0);

        // ================================================================
        // 9. Tier 1 gating — Tier 2 ops return 0 on ENABLE_TIER2=0
        // ================================================================
        op = BF_BEXT; a = 64'hFFFF_FFFF_FFFF_FFFF; b = 64'hFFFF_FFFF_FFFF_FFFF;
        check_tier1_stub("Tier1 gate: BEXT");

        op = BF_BDEP; a = 64'hFFFF_FFFF_FFFF_FFFF; b = 64'hFFFF_FFFF_FFFF_FFFF;
        check_tier1_stub("Tier1 gate: BDEP");

        op = BF_RORI; a = 64'h1234_5678; imm = 6'd4;
        check_tier1_stub("Tier1 gate: RORI");
        imm = 6'd0;

        // Tier 1 ops should still work on the tier1-only instance
        op = BF_POPCNT; b = 64'hFFFF_FFFF_FFFF_FFFF;
        #1;
        if (tier1_result !== 64'd64) begin
            $display("FAIL [Tier1 POPCNT]: tier1_result=%h expected=40", tier1_result);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        op = BF_CLZ; b = 64'd1;
        #1;
        if (tier1_result !== 64'd63) begin
            $display("FAIL [Tier1 CLZ]: tier1_result=%h expected=3F", tier1_result);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        op = BF_CTZ; b = 64'h8000_0000_0000_0000;
        #1;
        if (tier1_result !== 64'd63) begin
            $display("FAIL [Tier1 CTZ]: tier1_result=%h expected=3F", tier1_result);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        op = BF_BITREV; b = 64'd1;
        #1;
        if (tier1_result !== 64'h8000_0000_0000_0000) begin
            $display("FAIL [Tier1 BITREV]: tier1_result=%h expected=8000000000000000", tier1_result);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        // ================================================================
        // Summary
        // ================================================================
        #10;
        $display("");
        $display("============================================");
        if (fail_count == 0)
            $display(" tb_bitfield: ALL %0d assertions PASSED", pass_count);
        else
            $display(" tb_bitfield: %0d PASSED, %0d FAILED", pass_count, fail_count);
        $display("============================================");
        if (fail_count != 0) $finish;
        $finish;
    end

endmodule
