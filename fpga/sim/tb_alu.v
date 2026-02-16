// ============================================================================
// tb_alu.v — Unit tests for mp64_alu (shared combinational ALU)
// ============================================================================
//
// Tests all 16 ALU operations with edge cases:
//   - Arithmetic: ADD, ADC, SUB, SBB, CMP, NEG
//   - Logic: AND, OR, XOR, NOT, MOV
//   - Shifts/rotates: SHL, SHR, SAR, ROL, ROR
//   - Flag behaviour: Z, C, N, V, P, G
//

`timescale 1ns / 1ps
`include "mp64_defs.vh"

module tb_alu;

    `include "mp64_cpu_common.vh"

    // DUT signals
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

    // Test infrastructure
    integer pass, fail;

    task check_result;
        input [63:0] expected;
        input [255:0] name;
        begin
            if (result !== expected) begin
                $display("FAIL %0s: result=%h expected=%h", name, result, expected);
                fail = fail + 1;
            end else begin
                pass = pass + 1;
            end
        end
    endtask

    task check_flag;
        input        actual;
        input        expected;
        input [255:0] name;
        begin
            if (actual !== expected) begin
                $display("FAIL %0s: flag=%b expected=%b", name, actual, expected);
                fail = fail + 1;
            end else begin
                pass = pass + 1;
            end
        end
    endtask

    initial begin
        $dumpfile("tb_alu.vcd");
        $dumpvars(0, tb_alu);
        pass = 0;
        fail = 0;
        flags_in = 8'h00;

        // ===================================================================
        // ADD
        // ===================================================================
        op = ALU_ADD; a = 64'd100; b = 64'd50; #1;
        check_result(64'd150, "ADD 100+50");
        check_flag(flags_out[0], 1'b0, "ADD 100+50 Z=0");
        check_flag(flags_out[1], 1'b0, "ADD 100+50 C=0");

        op = ALU_ADD; a = 64'hFFFFFFFFFFFFFFFF; b = 64'd1; #1;
        check_result(64'd0, "ADD max+1 overflow");
        check_flag(flags_out[0], 1'b1, "ADD max+1 Z=1");
        check_flag(flags_out[1], 1'b1, "ADD max+1 C=1");

        op = ALU_ADD; a = 64'h7FFFFFFFFFFFFFFF; b = 64'd1; #1;
        check_result(64'h8000000000000000, "ADD signed overflow");
        check_flag(flags_out[2], 1'b1, "ADD signed overflow N=1");
        check_flag(flags_out[3], 1'b1, "ADD signed overflow V=1");

        // Zero + zero
        op = ALU_ADD; a = 64'd0; b = 64'd0; #1;
        check_result(64'd0, "ADD 0+0");
        check_flag(flags_out[0], 1'b1, "ADD 0+0 Z=1");

        // ===================================================================
        // ADC (add with carry)
        // ===================================================================
        flags_in = 8'h02;  // C=1
        op = ALU_ADC; a = 64'd10; b = 64'd20; #1;
        check_result(64'd31, "ADC 10+20+1");
        flags_in = 8'h00;  // C=0
        op = ALU_ADC; a = 64'd10; b = 64'd20; #1;
        check_result(64'd30, "ADC 10+20+0");

        // ===================================================================
        // SUB
        // ===================================================================
        op = ALU_SUB; a = 64'd100; b = 64'd30; #1;
        check_result(64'd70, "SUB 100-30");
        check_flag(flags_out[1], 1'b1, "SUB 100-30 C=1 (no borrow)");
        check_flag(flags_out[5], 1'b1, "SUB 100-30 G=1");

        op = ALU_SUB; a = 64'd30; b = 64'd100; #1;
        check_flag(flags_out[1], 1'b0, "SUB 30-100 C=0 (borrow)");
        check_flag(flags_out[2], 1'b1, "SUB 30-100 N=1");
        check_flag(flags_out[5], 1'b0, "SUB 30-100 G=0");

        op = ALU_SUB; a = 64'd50; b = 64'd50; #1;
        check_result(64'd0, "SUB 50-50");
        check_flag(flags_out[0], 1'b1, "SUB 50-50 Z=1");

        // ===================================================================
        // CMP (same as SUB but result discarded by caller)
        // ===================================================================
        op = ALU_CMP; a = 64'd100; b = 64'd50; #1;
        check_result(64'd50, "CMP 100,50 result");
        check_flag(flags_out[5], 1'b1, "CMP 100>50 G=1");
        check_flag(flags_out[0], 1'b0, "CMP 100!=50 Z=0");

        // ===================================================================
        // SBB (subtract with borrow)
        // ===================================================================
        flags_in = 8'h02;  // C=1 (no borrow)
        op = ALU_SBB; a = 64'd100; b = 64'd30; #1;
        check_result(64'd70, "SBB 100-30 C=1");
        flags_in = 8'h00;  // C=0 (borrow)
        op = ALU_SBB; a = 64'd100; b = 64'd30; #1;
        check_result(64'd69, "SBB 100-30-1 C=0");

        // ===================================================================
        // AND
        // ===================================================================
        flags_in = 8'h00;
        op = ALU_AND; a = 64'hFF00FF00FF00FF00; b = 64'h0F0F0F0F0F0F0F0F; #1;
        check_result(64'h0F000F000F000F00, "AND pattern");
        check_flag(flags_out[1], 1'b0, "AND clears C");

        // ===================================================================
        // OR
        // ===================================================================
        op = ALU_OR; a = 64'hFF00000000000000; b = 64'h00000000000000FF; #1;
        check_result(64'hFF000000000000FF, "OR pattern");

        // ===================================================================
        // XOR
        // ===================================================================
        op = ALU_XOR; a = 64'hAAAAAAAAAAAAAAAA; b = 64'hAAAAAAAAAAAAAAAA; #1;
        check_result(64'd0, "XOR self = 0");
        check_flag(flags_out[0], 1'b1, "XOR self Z=1");

        op = ALU_XOR; a = 64'hAAAAAAAAAAAAAAAA; b = 64'h5555555555555555; #1;
        check_result(64'hFFFFFFFFFFFFFFFF, "XOR complement");

        // ===================================================================
        // MOV
        // ===================================================================
        op = ALU_MOV; a = 64'd999; b = 64'd42; #1;
        check_result(64'd42, "MOV takes b");

        // ===================================================================
        // NOT
        // ===================================================================
        op = ALU_NOT; b = 64'd0; #1;
        check_result(64'hFFFFFFFFFFFFFFFF, "NOT 0");
        op = ALU_NOT; b = 64'hFFFFFFFFFFFFFFFF; #1;
        check_result(64'd0, "NOT all-ones");

        // ===================================================================
        // NEG
        // ===================================================================
        op = ALU_NEG; b = 64'd1; #1;
        check_result(64'hFFFFFFFFFFFFFFFF, "NEG 1 = -1");
        check_flag(flags_out[1], 1'b1, "NEG 1 C=1");
        op = ALU_NEG; b = 64'd0; #1;
        check_result(64'd0, "NEG 0 = 0");
        check_flag(flags_out[1], 1'b0, "NEG 0 C=0");

        // ===================================================================
        // SHL
        // ===================================================================
        op = ALU_SHL; a = 64'd1; b = 64'd4; #1;
        check_result(64'd16, "SHL 1<<4");
        op = ALU_SHL; a = 64'h8000000000000000; b = 64'd1; #1;
        check_result(64'd0, "SHL msb out");
        check_flag(flags_out[1], 1'b1, "SHL msb out C=1");

        // ===================================================================
        // SHR
        // ===================================================================
        op = ALU_SHR; a = 64'd16; b = 64'd4; #1;
        check_result(64'd1, "SHR 16>>4");
        op = ALU_SHR; a = 64'hFF; b = 64'd1; #1;
        check_result(64'h7F, "SHR 0xFF>>1");
        check_flag(flags_out[1], 1'b1, "SHR 0xFF>>1 C=1");

        // ===================================================================
        // SAR (arithmetic shift right)
        // ===================================================================
        op = ALU_SAR; a = 64'hFFFFFFFFFFFFFF00; b = 64'd8; #1;
        check_result(64'hFFFFFFFFFFFFFFFF, "SAR sign-ext");
        check_flag(flags_out[2], 1'b1, "SAR sign-ext N=1");

        // ===================================================================
        // ROL
        // ===================================================================
        op = ALU_ROL; a = 64'h8000000000000001; b = 64'd1; #1;
        check_result(64'h0000000000000003, "ROL msb wraps");

        op = ALU_ROL; a = 64'd42; b = 64'd0; #1;
        check_result(64'd42, "ROL by 0 = identity");

        // ===================================================================
        // ROR
        // ===================================================================
        op = ALU_ROR; a = 64'h0000000000000003; b = 64'd1; #1;
        check_result(64'h8000000000000001, "ROR lsb wraps");

        op = ALU_ROR; a = 64'd42; b = 64'd0; #1;
        check_result(64'd42, "ROR by 0 = identity");

        // ===================================================================
        // P flag (parity) spot check
        // ===================================================================
        op = ALU_ADD; a = 64'd0; b = 64'd3; flags_in = 8'h00; #1;
        // 3 = 0b11 → 2 ones → even parity → P=1
        check_flag(flags_out[4], 1'b1, "P flag even parity (3)");

        op = ALU_ADD; a = 64'd0; b = 64'd1; flags_in = 8'h00; #1;
        // 1 = 0b01 → 1 one → odd parity → P=0
        check_flag(flags_out[4], 1'b0, "P flag odd parity (1)");

        // ===================================================================
        // Summary
        // ===================================================================
        $display("--------------------------------------------");
        $display("ALU: %0d passed, %0d failed", pass, fail);
        $display("--------------------------------------------");
        if (fail != 0) begin
            $display("FAIL");
            $finish(1);
        end else begin
            $display("ALL PASS");
        end
        $finish;
    end

endmodule
