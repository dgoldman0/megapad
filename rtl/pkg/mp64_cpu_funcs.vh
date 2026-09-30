// ============================================================================
// mp64_cpu_funcs.vh — CPU utility functions (include inside module body)
// ============================================================================
//
// Verilog-2001 requires functions to be declared inside a module body.
// This file is `included after the module header's port declarations,
// AFTER `include "mp64_pkg.vh" has been done at file scope.
//
// Provides:
//   cond_eval()  — condition code evaluator for BR/LBR/SKIP
//   instr_len()  — instruction byte length decoder
//   crypto_is_bare() — identify two-byte EXT.CRYPTO instructions
//   fp_has_t(), fp_uses_dynamic_rm(), fp_op_legal(), fp_extra_cycles()
//     — EXT.FP (FC) length, rounding-mode use, legality, and latency
//

// ========================================================================
// Condition code evaluator
// ========================================================================
//   cond[3:0]:
//     0 AL  always          8 VC  !V
//     1 EQ  Z=1             9 GT  G=1
//     2 NE  Z=0             A LE  G=0
//     3 CS  C=1             B BQ  Q=1
//     4 CC  C=0             C BNQ Q=0
//     5 MI  N=1             D SAT S=1
//     6 PL  N=0             E EF  any EF set
//     7 VS  V=1             F NV  never
//
//   flags byte: [S I G P V N C Z] = bits [7:0]

function cond_eval;
    input [3:0] cond;
    input [7:0] f;        // flags byte
    input       q_val;    // Q register
    input [3:0] ef_val;   // external flags
    begin
        case (cond)
            4'h0: cond_eval = 1'b1;
            4'h1: cond_eval = f[0];
            4'h2: cond_eval = ~f[0];
            4'h3: cond_eval = f[1];
            4'h4: cond_eval = ~f[1];
            4'h5: cond_eval = f[2];
            4'h6: cond_eval = ~f[2];
            4'h7: cond_eval = f[3];
            4'h8: cond_eval = ~f[3];
            4'h9: cond_eval = f[5];
            4'hA: cond_eval = ~f[5];
            4'hB: cond_eval = q_val;
            4'hC: cond_eval = ~q_val;
            4'hD: cond_eval = f[7];
            4'hE: cond_eval = |ef_val;
            4'hF: cond_eval = 1'b0;
            default: cond_eval = 1'b0;
        endcase
    end
endfunction

// A MEX system operation that is not an immediate form may be RROT
// (function 7), which carries a control byte after its function byte and any
// broadcast register byte.  Fetchers and SKIP read the function byte before
// they know the complete length.  EXT.8 (ETALU) makes TSYS function 7 a
// reserved TACC encoding without a control byte.
function mex_may_have_control_byte;
    input [7:0] byte0;
    begin
        mex_may_have_control_byte =
            (byte0[7:4] == 4'hE) && (byte0[1:0] == 2'b11) &&
            (byte0[3:2] != 2'd2);
    end
endfunction

function mex_has_control_byte;
    input [7:0] byte0;
    input [7:0] funct_byte;
    input       etalu_prefix;
    begin
        mex_has_control_byte =
            mex_may_have_control_byte(byte0) && !etalu_prefix &&
            (funct_byte[2:0] == 3'd7);
    end
endfunction

// EXT.CRYPTO is the only other family whose length depends on its second
// byte.
// CPU fetchers initially reserve the three-byte maximum, then use this
// predicate after fetching the sub-op.
function crypto_is_bare;
    input [7:0] sub_op;
    begin
        case (sub_op)
            8'h00,                         // CRC.INIT
            8'h07, 8'h08, 8'h09, 8'h0A,  // reserved CRC ops trap as
            8'h0B, 8'h0C, 8'h0D, 8'h0E,  // complete two-byte encodings
            8'h0F,
            8'h11, 8'h12, 8'h15, 8'h16,  // SHA.ROUND/PAD/FINAL/RELEASE
            8'h17, 8'h18, 8'h19, 8'h1A,  // reserved SHA ops trap as
            8'h1B, 8'h1C, 8'h1D, 8'h1E,  // complete two-byte encodings
            8'h1F,
            8'h20, 8'h21, 8'h22, 8'h23,  // GF.ADD/SUB/MUL/SQR
            8'h24, 8'h25, 8'h26,          // GF.INV/POW/MULR
            8'h27, 8'h28,                 // GF.MAC/MACR
            8'h2A, 8'h2C, 8'h2D:          // GF.CEQ/LDPRIME/X25519
                crypto_is_bare = 1'b1;
            default:
                crypto_is_bare = 1'b0;
        endcase
    end
endfunction

// ========================================================================
// Instruction length decoder
// ========================================================================
//   Returns byte count for the instruction starting with byte0.
//   has_ext = 1 when an active EXT prefix modifier is in effect.

function [3:0] instr_len;
    input [7:0] byte0;
    input       has_ext;
    begin
        case (byte0[7:4])
            FAM_SYS:    instr_len = (byte0[3:0] == 4'hD) ? 4'd2 : 4'd1;
            FAM_INC:    instr_len = 4'd1;
            FAM_DEC:    instr_len = 4'd1;
            FAM_BR:     instr_len = has_ext ? 4'd1 : 4'd2;
            FAM_LBR:    instr_len = 4'd3;
            FAM_MEM:    instr_len = (byte0[3:0] == 4'hF) ? 4'd3 : 4'd2;
            FAM_IMM:    instr_len = (byte0[3:0] == 4'h1) ? 4'd4
                                  : has_ext               ? 4'd10
                                  : (byte0[3:0] >= 4'h8)  ? 4'd2
                                  :                          4'd3;
            FAM_ALU:    instr_len = 4'd2;
            FAM_MEMALU: instr_len = 4'd1;
            FAM_IO:     instr_len = 4'd1;
            FAM_SEP:    instr_len = 4'd1;
            FAM_SEX:    instr_len = 4'd1;
            FAM_MULDIV: instr_len = (byte0[3:0] == 4'hE) ? 4'd3 : 4'd2;  // RORI = 3 bytes
            FAM_CSR:    instr_len = 4'd2;
            FAM_MEX:    instr_len = (byte0[3:2] == 2'd1) ? 4'd3 : 4'd2;
            FAM_EXT:    instr_len = (byte0[3:0] == 4'h9) ? 4'd3  // EXT.STRING
                                  : (byte0[3:0] == 4'hA) ? 4'd3  // EXT.DICT
                                  : (byte0[3:0] == 4'hB) ? 4'd3  // EXT.CRYPTO (max 3)
                                  : (byte0[3:0] == 4'hC) ? 4'd3  // EXT.FP (4 with T)
                                  : 4'd1;
            default:    instr_len = 4'd1;
        endcase
    end
endfunction

// ========================================================================
// EXT.FP (FC) — docs/floating-point.md §8 and §10
// ========================================================================
//   FC op DR [T]: op[7:6] format (0 S, 1 D, 2-3 reserved), op[5:0] operation.

// FMA and FMS carry the T byte, so they are four bytes long.
function fp_has_t;
    input [7:0] op;
    begin
        fp_has_t = (op[5:0] == 6'h07) || (op[5:0] == 6'h08);
    end
endfunction

// Operations that round with FPCSR.RM, decided by op[5:0] alone.
function fp_uses_dynamic_rm;
    input [7:0] op;
    reg   [5:0] c;
    begin
        c = op[5:0];
        fp_uses_dynamic_rm =
            (c <= 6'h04) || (c == 6'h07) || (c == 6'h08) ||
            (c >= 6'h38 && c <= 6'h3B) || (c == 6'h3D) ||
            (c >= 6'h20 && c < 6'h38 && c[2:0] == 3'd7);
    end
endfunction

// Legal encoding and mode; anything else raises IVEC_ILLEGAL_OP.
function fp_op_legal;
    input [7:0] op;
    input [7:0] t;
    input [2:0] rm_dyn;
    reg   [5:0] c;
    begin
        c = op[5:0];
        fp_op_legal =
            !op[7] &&
            ((c <= 6'h08) || (c >= 6'h10 && c <= 6'h14) ||
             (c >= 6'h20 && c <= 6'h3E)) &&
            !(fp_has_t(op) && t[7:5] != 3'd0) &&
            !(c >= 6'h20 && c < 6'h38 &&
              (c[2:0] == 3'd5 || c[2:0] == 3'd6)) &&
            !(fp_uses_dynamic_rm(op) && rm_dyn > 3'd4);
    end
endfunction

// §10 extra cycles: the unit's start-to-done latency.
function [5:0] fp_extra_cycles;
    input [7:0] op;
    reg   [5:0] c;
    begin
        c = op[5:0];
        if (c == 6'h03 || c == 6'h04)
            fp_extra_cycles = op[6] ? 6'd30 : 6'd15;
        else if (c == 6'h05 || c == 6'h06 || (c >= 6'h10 && c <= 6'h14))
            fp_extra_cycles = 6'd1;
        else
            fp_extra_cycles = 6'd3;
    end
endfunction
