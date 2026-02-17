// ============================================================================
// tb_alu.v — Testbench for mp64_alu (combinational ALU, 16 ops)
// ============================================================================
`timescale 1ns / 1ps

`include "mp64_pkg.vh"

module tb_alu;

    `include "mp64_cpu_funcs.vh"

    reg  [3:0]  op;
    reg  [63:0] a, b;
    reg  [7:0]  flags_in;
    wire [63:0] result;
    wire [7:0]  flags_out;

    mp64_alu uut (
        .op       (op),
        .a        (a),
        .b        (b),
        .flags_in (flags_in),
        .result   (result),
        .flags_out(flags_out)
    );

    // Convenience: extract individual flags
    wire fz = flags_out[0];
    wire fc = flags_out[1];
    wire fn = flags_out[2];
    wire fv = flags_out[3];
    wire fp = flags_out[4];
    wire fg = flags_out[5];

    integer pass_count;
    integer fail_count;

    task check;
        input [255:0] label;
        input [63:0]  expected_result;
        input [7:0]   expected_flags_mask;  // which flag bits to check
        input [7:0]   expected_flags_val;   // expected values for those bits
        begin
            #1; // let combinational settle
            if (result !== expected_result) begin
                $display("FAIL [%0s]: result=%h expected=%h",
                         label, result, expected_result);
                fail_count = fail_count + 1;
            end else if ((flags_out & expected_flags_mask) !== expected_flags_val) begin
                $display("FAIL [%0s]: flags=%b expected=%b (mask=%b)",
                         label, flags_out, expected_flags_val, expected_flags_mask);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    initial begin
        $dumpfile("tb_alu.vcd");
        $dumpvars(0, tb_alu);

        pass_count = 0;
        fail_count = 0;
        flags_in   = 8'h00;

        // ================================================================
        // 1. ADD
        // ================================================================
        op = ALU_ADD; a = 64'd100; b = 64'd200;
        check("ADD basic", 64'd300,
              8'b00000111, 8'b00000000);  // Z=0, C=0, N=0

        op = ALU_ADD; a = 64'hFFFF_FFFF_FFFF_FFFF; b = 64'd1;
        check("ADD overflow", 64'd0,
              8'b00000011, 8'b00000011);  // Z=1, C=1

        op = ALU_ADD; a = 64'h7FFF_FFFF_FFFF_FFFF; b = 64'd1;
        check("ADD signed overflow", 64'h8000_0000_0000_0000,
              8'b00001110, 8'b00001100);  // V=1, N=1, C=0

        // ================================================================
        // 2. SUB
        // ================================================================
        op = ALU_SUB; a = 64'd300; b = 64'd100;
        check("SUB basic", 64'd200,
              8'b00100011, 8'b00100010);  // Z=0, C=1(no borrow), G=1

        op = ALU_SUB; a = 64'd100; b = 64'd100;
        check("SUB equal", 64'd0,
              8'b00100011, 8'b00000011);  // Z=1, C=1(a>=b), G=0

        op = ALU_SUB; a = 64'd50; b = 64'd100;
        check("SUB borrow", 64'hFFFF_FFFF_FFFF_FFCE,
              8'b00100111, 8'b00000100);  // Z=0, C=0(borrow), N=1, G=0

        // ================================================================
        // 3. AND
        // ================================================================
        op = ALU_AND; a = 64'hFF00_FF00_FF00_FF00; b = 64'h0F0F_0F0F_0F0F_0F0F;
        check("AND basic", 64'h0F00_0F00_0F00_0F00,
              8'b00001011, 8'b00000000);  // C=0, V=0, Z=0

        op = ALU_AND; a = 64'hAAAA; b = 64'h5555;
        check("AND zero", 64'd0,
              8'b00000001, 8'b00000001);  // Z=1

        // ================================================================
        // 4. OR
        // ================================================================
        op = ALU_OR; a = 64'hFF00_0000_0000_0000; b = 64'h00FF_0000_0000_0000;
        check("OR basic", 64'hFFFF_0000_0000_0000,
              8'b00000101, 8'b00000100);  // N=1, Z=0

        // ================================================================
        // 5. XOR
        // ================================================================
        op = ALU_XOR; a = 64'hDEAD_BEEF_CAFE_BABE; b = 64'hDEAD_BEEF_CAFE_BABE;
        check("XOR self", 64'd0,
              8'b00000001, 8'b00000001);  // Z=1

        op = ALU_XOR; a = 64'hFFFF_FFFF_FFFF_FFFF; b = 64'd0;
        check("XOR allones", 64'hFFFF_FFFF_FFFF_FFFF,
              8'b00000101, 8'b00000100);  // N=1, Z=0

        // ================================================================
        // 6. MOV
        // ================================================================
        op = ALU_MOV; a = 64'hDEAD; b = 64'hBEEF;
        check("MOV", 64'hBEEF,
              8'b00000001, 8'b00000000);  // Z=0

        // ================================================================
        // 7. NOT
        // ================================================================
        op = ALU_NOT; a = 64'd0; b = 64'hFFFF_FFFF_FFFF_FFFF;
        check("NOT allones", 64'd0,
              8'b00001011, 8'b00000001);  // Z=1, C=0, V=0

        op = ALU_NOT; a = 64'd0; b = 64'd0;
        check("NOT zero", 64'hFFFF_FFFF_FFFF_FFFF,
              8'b00000001, 8'b00000000);  // Z=0

        // ================================================================
        // 8. NEG
        // ================================================================
        op = ALU_NEG; a = 64'd0; b = 64'd1;
        check("NEG 1", 64'hFFFF_FFFF_FFFF_FFFF,
              8'b00000011, 8'b00000010);  // C=1 (b!=0), Z=0

        op = ALU_NEG; a = 64'd0; b = 64'd0;
        check("NEG 0", 64'd0,
              8'b00000011, 8'b00000001);  // C=0 (b==0), Z=1

        op = ALU_NEG; a = 64'd0; b = 64'h8000_0000_0000_0000;
        check("NEG min", 64'h8000_0000_0000_0000,
              8'b00000110, 8'b00000110);  // C=1, N=1

        // ================================================================
        // 9. SHL
        // ================================================================
        op = ALU_SHL; a = 64'd1; b = 64'd63;
        check("SHL 1<<63", 64'h8000_0000_0000_0000,
              8'b00000101, 8'b00000100);  // N=1

        op = ALU_SHL; a = 64'hFFFF_FFFF_FFFF_FFFF; b = 64'd1;
        check("SHL allones by 1", 64'hFFFF_FFFF_FFFF_FFFE,
              8'b00000010, 8'b00000010);  // C=1 (msb shifted out)

        op = ALU_SHL; a = 64'h0000_0000_0000_0001; b = 64'd0;
        check("SHL by 0", 64'd1,
              8'b00000010, 8'b00000000);  // C=0 (shift by 0)

        // ================================================================
        // 10. SHR
        // ================================================================
        op = ALU_SHR; a = 64'h8000_0000_0000_0000; b = 64'd63;
        check("SHR 63", 64'd1,
              8'b00000001, 8'b00000000);  // Z=0

        op = ALU_SHR; a = 64'hFFFF_FFFF_FFFF_FFFF; b = 64'd1;
        check("SHR allones by 1", 64'h7FFF_FFFF_FFFF_FFFF,
              8'b00000010, 8'b00000010);  // C=1 (lsb shifted out)

        // ================================================================
        // 11. SAR (arithmetic right shift)
        // ================================================================
        op = ALU_SAR; a = 64'h8000_0000_0000_0000; b = 64'd63;
        check("SAR neg>>63", 64'hFFFF_FFFF_FFFF_FFFF,
              8'b00000101, 8'b00000100);  // N=1

        op = ALU_SAR; a = 64'h7FFF_FFFF_FFFF_FFFF; b = 64'd1;
        check("SAR pos>>1", 64'h3FFF_FFFF_FFFF_FFFF,
              8'b00000011, 8'b00000010);  // C=1 (bit 0 was 1)

        // ================================================================
        // 12. CMP (like SUB but result not written, only flags)
        // ================================================================
        op = ALU_CMP; a = 64'd42; b = 64'd42;
        check("CMP equal", 64'd0,
              8'b00100011, 8'b00000011);  // Z=1, C=1(no borrow), G=0

        op = ALU_CMP; a = 64'd100; b = 64'd50;
        check("CMP greater", 64'd50,
              8'b00100011, 8'b00100010);  // Z=0, C=1, G=1

        op = ALU_CMP; a = 64'd10; b = 64'd100;
        check("CMP less", 64'hFFFF_FFFF_FFFF_FFA6,
              8'b00100111, 8'b00000100);  // Z=0, C=0, N=1, G=0

        // ================================================================
        // 13. ADC (add with carry)
        // ================================================================
        flags_in = 8'h02; // C=1
        op = ALU_ADC; a = 64'd10; b = 64'd20;
        check("ADC with carry", 64'd31,
              8'b00000011, 8'b00000000);  // Z=0, C=0

        flags_in = 8'h00; // C=0
        op = ALU_ADC; a = 64'd10; b = 64'd20;
        check("ADC no carry", 64'd30,
              8'b00000011, 8'b00000000);

        flags_in = 8'h02; // C=1
        op = ALU_ADC; a = 64'hFFFF_FFFF_FFFF_FFFF; b = 64'd0;
        check("ADC max+carry", 64'd0,
              8'b00000011, 8'b00000011);  // Z=1, C=1

        // ================================================================
        // 14. SBB (subtract with borrow)
        // ================================================================
        flags_in = 8'h02; // C=1 → no borrow
        op = ALU_SBB; a = 64'd100; b = 64'd50;
        check("SBB no borrow", 64'd50,
              8'b00000010, 8'b00000010);  // C=1

        flags_in = 8'h00; // C=0 → borrow
        op = ALU_SBB; a = 64'd100; b = 64'd50;
        check("SBB with borrow", 64'd49,
              8'b00000010, 8'b00000010);  // C=1

        flags_in = 8'h00; // C=0 → borrow
        op = ALU_SBB; a = 64'd0; b = 64'd0;
        check("SBB 0-0-1", 64'hFFFF_FFFF_FFFF_FFFF,
              8'b00000010, 8'b00000000);  // C=0 (borrow occurred)

        // ================================================================
        // 15. ROL (rotate left)
        // ================================================================
        flags_in = 8'h00;
        op = ALU_ROL; a = 64'h8000_0000_0000_0001; b = 64'd1;
        check("ROL by 1", 64'h0000_0000_0000_0003,
              8'b00000001, 8'b00000000);  // Z=0

        op = ALU_ROL; a = 64'hDEAD_BEEF_CAFE_BABE; b = 64'd0;
        check("ROL by 0", 64'hDEAD_BEEF_CAFE_BABE,
              8'b00000001, 8'b00000000);

        op = ALU_ROL; a = 64'h0000_0000_0000_0001; b = 64'd32;
        check("ROL by 32", 64'h0000_0001_0000_0000,
              8'b00000001, 8'b00000000);

        // ================================================================
        // 16. ROR (rotate right)
        // ================================================================
        op = ALU_ROR; a = 64'h0000_0000_0000_0003; b = 64'd1;
        check("ROR by 1", 64'h8000_0000_0000_0001,
              8'b00000101, 8'b00000100);  // N=1

        op = ALU_ROR; a = 64'hAAAA_AAAA_AAAA_AAAA; b = 64'd0;
        check("ROR by 0", 64'hAAAA_AAAA_AAAA_AAAA,
              8'b00000001, 8'b00000000);

        // ================================================================
        // 17. Flag pass-through (S and I bits)
        // ================================================================
        flags_in = 8'hC0; // S=1, I=1
        op = ALU_ADD; a = 64'd1; b = 64'd2;
        check("S+I pass-through", 64'd3,
              8'b11000000, 8'b11000000);  // S and I preserved

        // ================================================================
        // 18. P flag (parity)
        // ================================================================
        flags_in = 8'h00;
        op = ALU_ADD; a = 64'd0; b = 64'd0;
        check("P flag zero", 64'd0,
              8'b00010001, 8'b00010001);  // Z=1, P=1 (even parity of 0x00)

        op = ALU_ADD; a = 64'd0; b = 64'd1;
        check("P flag odd", 64'd1,
              8'b00010001, 8'b00000000);  // Z=0, P=0 (1 has odd parity)

        op = ALU_ADD; a = 64'd0; b = 64'd3;
        check("P flag 0x03", 64'd3,
              8'b00010000, 8'b00010000);  // P=1 (0x03 = 2 bits = even)

        // ================================================================
        // 19. Edge: 64-bit max values
        // ================================================================
        op = ALU_ADD; a = 64'h8000_0000_0000_0000; b = 64'h8000_0000_0000_0000;
        check("ADD two negmax", 64'd0,
              8'b00001011, 8'b00001011);  // Z=1, C=1, V=1

        op = ALU_SUB; a = 64'h8000_0000_0000_0000; b = 64'd1;
        check("SUB negmax-1", 64'h7FFF_FFFF_FFFF_FFFF,
              8'b00101110, 8'b00101010);  // G=1, V=1, C=1

        // ================================================================
        // Summary
        // ================================================================
        #10;
        $display("");
        $display("============================================");
        if (fail_count == 0)
            $display(" tb_alu: ALL %0d assertions PASSED", pass_count);
        else
            $display(" tb_alu: %0d PASSED, %0d FAILED", pass_count, fail_count);
        $display("============================================");
        if (fail_count != 0) $finish;
        $finish;
    end

endmodule
