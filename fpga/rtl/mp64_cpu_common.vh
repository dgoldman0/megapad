// ============================================================================
// mp64_cpu_common.vh — Shared CPU constants and utility functions
// ============================================================================
//
// Included INSIDE the module body of every CPU variant (major, micro)
// and the standalone ALU module.  Contains:
//   - CPU FSM state encodings
//   - ALU operation encodings
//   - Post-action codes (multi-cycle operations)
//   - Condition code evaluator function
//   - Instruction length decoder function
//
// Depends on mp64_defs.vh being included BEFORE the module declaration
// (FAM_SYS, FAM_INC, … constants must be in scope).
//
// NOTE: No `ifndef guard — this file is included inside multiple module
// bodies and must re-expand in each scope.
//

// ========================================================================
// CPU FSM states (4-bit, shared encoding across all core variants)
// ========================================================================
localparam CPU_FETCH      = 4'd0;
localparam CPU_DECODE     = 4'd1;
localparam CPU_EXECUTE    = 4'd2;
localparam CPU_MEM_READ   = 4'd3;
localparam CPU_MEM_WRITE  = 4'd4;
localparam CPU_MEX_WAIT   = 4'd5;   // major core only
localparam CPU_IRQ        = 4'd6;
localparam CPU_HALT       = 4'd7;
localparam CPU_FETCH_MORE = 4'd8;
localparam CPU_MULDIV     = 4'd9;   // major core only
localparam CPU_MEM_READ2  = 4'd10;  // RTI flags pop
localparam CPU_IRQ_PUSH   = 4'd11;  // push flags in IRQ sequence
localparam CPU_IRQ_LOAD   = 4'd12;  // load IVT vector from memory
localparam CPU_MEMALU_RD  = 4'd13;  // MEMALU: reading M(R(X))
localparam CPU_MEMALU_WB  = 4'd14;  // reserved (unused in current FSMs)
localparam CPU_SKIP       = 4'd15;  // SKIP: fetch next byte for length

// ========================================================================
// ALU operation codes (4-bit, shared between mp64_alu and CPU FSMs)
// ========================================================================
localparam ALU_ADD = 4'd0;
localparam ALU_SUB = 4'd1;
localparam ALU_AND = 4'd2;
localparam ALU_OR  = 4'd3;
localparam ALU_XOR = 4'd4;
localparam ALU_MOV = 4'd5;
localparam ALU_NOT = 4'd6;
localparam ALU_NEG = 4'd7;
localparam ALU_SHL = 4'd8;
localparam ALU_SHR = 4'd9;
localparam ALU_SAR = 4'd10;
localparam ALU_CMP = 4'd11;
localparam ALU_ADC = 4'd12;
localparam ALU_SBB = 4'd13;
localparam ALU_ROL = 4'd14;
localparam ALU_ROR = 4'd15;

// ========================================================================
// Post-action codes for multi-cycle operations
// ========================================================================
localparam POST_NONE     = 3'd0;
localparam POST_RTI_POP2 = 3'd1;   // after popping PC, pop flags
localparam POST_IRQ_VEC  = 3'd2;   // after pushing, load IVT vector

// ========================================================================
// Condition code evaluator (for BR / LBR / SKIP)
// ========================================================================
//   cond[3:0]  encoding:
//     0 AL  always          8 VC  !V
//     1 EQ  Z=1             9 GT  G=1
//     2 NE  Z=0             A LE  G=0
//     3 CS  C=1             B BQ  Q=1
//     4 CC  C=0             C BNQ Q=0
//     5 MI  N=1             D SAT S=1
//     6 PL  N=0             E EF  any EF set
//     7 VS  V=1             F NV  never
//
function cond_eval;
    input [3:0] cond;
    input [7:0] f;
    input       q_val;
    input [3:0] ef_val;
    begin
        case (cond)
            4'h0: cond_eval = 1'b1;           // AL
            4'h1: cond_eval = f[0];            // EQ
            4'h2: cond_eval = !f[0];           // NE
            4'h3: cond_eval = f[1];            // CS
            4'h4: cond_eval = !f[1];           // CC
            4'h5: cond_eval = f[2];            // MI
            4'h6: cond_eval = !f[2];           // PL
            4'h7: cond_eval = f[3];            // VS
            4'h8: cond_eval = !f[3];           // VC
            4'h9: cond_eval = f[5];            // GT
            4'hA: cond_eval = !f[5];           // LE
            4'hB: cond_eval = q_val;           // BQ
            4'hC: cond_eval = !q_val;          // BNQ
            4'hD: cond_eval = f[7];            // SAT
            4'hE: cond_eval = |ef_val;         // EF
            4'hF: cond_eval = 1'b0;            // NV
            default: cond_eval = 1'b0;
        endcase
    end
endfunction

// ========================================================================
// Instruction length decoder
// ========================================================================
//   Returns byte count for the instruction beginning with byte0.
//   has_ext indicates an active EXT prefix modifier.
//
function [3:0] instr_len;
    input [7:0] byte0;
    input       has_ext;
    begin
        case (byte0[7:4])
            FAM_SYS:    instr_len = (byte0[3:0] == 4'hD) ? 4'd2  // CALL.L Rn
                                   : 4'd1;
            FAM_INC:    instr_len = 4'd1;
            FAM_DEC:    instr_len = 4'd1;
            FAM_BR:     instr_len = (has_ext) ? 4'd1 : 4'd2;     // SKIP=1 / BR=2
            FAM_LBR:    instr_len = 4'd3;
            FAM_MEM:    instr_len = (byte0[3:0] == 4'hF) ? 4'd3  // LD.D+off
                                   : 4'd2;
            FAM_IMM:    instr_len = (byte0[3:0] == 4'h1) ? 4'd4  // LHI imm16
                                   : (has_ext) ? 4'd10            // EXT.IMM64
                                   : (byte0[3:0] >= 4'h8) ? 4'd2 // shifts, GLO/GHI/PLO/PHI
                                   : 4'd3;                        // LDI/ADDI/etc
            FAM_ALU:    instr_len = 4'd2;
            FAM_MEMALU: instr_len = 4'd1;
            FAM_IO:     instr_len = 4'd1;
            FAM_SEP:    instr_len = 4'd1;
            FAM_SEX:    instr_len = 4'd1;
            FAM_MULDIV: instr_len = 4'd2;
            FAM_CSR:    instr_len = 4'd2;                         // opcode + CSR addr
            FAM_MEX:    instr_len = (byte0[3:2] == 2'd1) ? 4'd3 : 4'd2;
            FAM_EXT:    instr_len = 4'd1;
            default:    instr_len = 4'd1;
        endcase
    end
endfunction
