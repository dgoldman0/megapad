// ============================================================================
// mp64_fpu.v — scalar floating-point unit for the EXT.FP (FC) engine
// ============================================================================
//
// docs/floating-point.md §8–§10 define every result, flag, and latency.  The
// caller checks the encoding (fp_op_legal in mp64_cpu_funcs.vh) and pulses
// `start` with the operation byte and the Rd, Rs, and Rt values; `done`
// pulses exactly fp_extra_cycles(op) cycles later with the result, the
// raised flags in FPCSR order {NV, DZ, OF, UF, NX}, and for FCMP the FLAGS
// bits {V, N, G, Z}.  A full core owns one unit; a cluster shares one unit
// among its microcores.
//
// FADD, FSUB, FMUL, FMA, and FMS use one lane of the multi-format FMA datapath
// (mp64_fma.v) in the requested rounding mode.  Conversions and FRND round
// through mp64_fp_round.  FDIV and FSQRT use mp64_fp_divsqrt, the restoring
// digit recurrence the tile engine also uses, which retires two bits per
// cycle; its answer is ready before the fixed latency ends, so the latency
// never depends on the operands.
//
// All arithmetic is bounded integer logic; nothing uses real or shortreal.
// ============================================================================

`include "mp64_pkg.vh"

module mp64_fpu (
    input  wire        clk,
    input  wire        rst,
    input  wire        start,
    input  wire [7:0]  op,
    input  wire [63:0] rd_val,
    input  wire [63:0] rs_val,
    input  wire [63:0] rt_val,
    input  wire [2:0]  rm_dyn,     // FPCSR.RM; the caller traps if reserved
    output reg         busy,
    output reg         done,
    output reg  [63:0] result,
    output reg         write_rd,   // 0 for FCMP
    output reg  [4:0]  flags,      // {NV, DZ, OF, UF, NX}
    output reg  [3:0]  cmp         // {V, N, G, Z} for FCMP
);
    `include "mp64_cpu_funcs.vh"

    localparam FMT_H = 2'd0;
    localparam FMT_B = 2'd1;
    localparam FMT_S = 2'd2;
    localparam FMT_D = 2'd3;
    localparam RDN   = 3'd2;

    // ------------------------------------------------------------------
    // Latched operation
    // ------------------------------------------------------------------
    reg [7:0]  op_r;
    reg [63:0] a_r;     // Rd
    reg [63:0] b_r;     // Rs
    reg [63:0] c_r;     // Rt
    reg [2:0]  rm_r;    // effective rounding mode
    reg [5:0]  remaining;

    wire       is64 = op_r[6];
    wire [5:0] code = op_r[5:0];
    wire [1:0] fmt  = is64 ? FMT_D : FMT_S;

    // ------------------------------------------------------------------
    // Operand descriptors
    // ------------------------------------------------------------------
    // {nan, snan, inf, zero, sign, exp[15:0], sig[52:0]}: a finite value is
    // (-1)**sign * sig * 2**exp, and sig is not normalized for subnormals.
    function [73:0] decode;
        input [1:0]  format;
        input [63:0] bits;
        reg          negative;
        reg   [10:0] field;
        reg   [51:0] fraction;
        integer      fraction_bits;
        integer      field_max;
        integer      bias;
        integer      exponent;
        begin
            case (format)
                FMT_H: begin
                    negative = bits[15]; field = {6'd0, bits[14:10]};
                    fraction = {42'd0, bits[9:0]};
                    fraction_bits = 10; field_max = 31; bias = 15;
                end
                FMT_B: begin
                    negative = bits[15]; field = {3'd0, bits[14:7]};
                    fraction = {45'd0, bits[6:0]};
                    fraction_bits = 7; field_max = 255; bias = 127;
                end
                FMT_S: begin
                    negative = bits[31]; field = {3'd0, bits[30:23]};
                    fraction = {29'd0, bits[22:0]};
                    fraction_bits = 23; field_max = 255; bias = 127;
                end
                default: begin
                    negative = bits[63]; field = bits[62:52];
                    fraction = bits[51:0];
                    fraction_bits = 52; field_max = 2047; bias = 1023;
                end
            endcase
            if (field == field_max) begin
                decode = {fraction != 52'd0,
                          fraction != 52'd0 && !fraction[fraction_bits - 1],
                          fraction == 52'd0, 1'b0, negative, 16'd0, 53'd0};
            end else if (field == 11'd0) begin
                exponent = 1 - bias - fraction_bits;
                decode = {3'b000, fraction == 52'd0, negative,
                          exponent[15:0], 1'b0, fraction};
            end else begin
                exponent = field - bias - fraction_bits;
                decode = {4'b0000, negative, exponent[15:0],
                          ({1'b0, fraction} | (53'd1 << fraction_bits))};
            end
        end
    endfunction

    function integer msb53;
        input [52:0] value;
        integer i;
        begin
            msb53 = -1;
            for (i = 0; i < 53; i = i + 1)
                if (value[i])
                    msb53 = i;
        end
    endfunction

    function [63:0] canonical_nan;
        input [1:0] format;
        begin
            case (format)
                FMT_H:   canonical_nan = 64'h7E00;
                FMT_B:   canonical_nan = 64'h7FC0;
                FMT_S:   canonical_nan = 64'h7FC0_0000;
                default: canonical_nan = 64'h7FF8_0000_0000_0000;
            endcase
        end
    endfunction

    function [63:0] infinity_of;
        input [1:0] format;
        input       negative;
        begin
            case (format)
                FMT_H:   infinity_of = {48'd0, negative, 15'h7C00};
                FMT_B:   infinity_of = {48'd0, negative, 15'h7F80};
                FMT_S:   infinity_of = {32'd0, negative, 31'h7F80_0000};
                default: infinity_of = {negative, 63'h7FF0_0000_0000_0000};
            endcase
        end
    endfunction

    function [63:0] zero_of;
        input [1:0] format;
        input       negative;
        begin
            case (format)
                FMT_H, FMT_B: zero_of = {48'd0, negative, 15'd0};
                FMT_S:        zero_of = {32'd0, negative, 31'd0};
                default:      zero_of = {negative, 63'd0};
            endcase
        end
    endfunction

    function round_up;
        input [2:0] mode;
        input       negative;
        input       lsb;
        input       round_bit;
        input       sticky;
        begin
            case (mode)
                3'd0:    round_up = round_bit && (sticky || lsb);
                3'd1:    round_up = 1'b0;
                3'd2:    round_up = negative && (round_bit || sticky);
                3'd3:    round_up = !negative && (round_bit || sticky);
                default: round_up = round_bit;
            endcase
        end
    endfunction

    // Order key: an unsigned integer that orders non-NaN values, -0 < +0.
    function [63:0] order_key;
        input        wide;
        input [63:0] bits;
        begin
            if (wide)
                order_key = bits[63] ? ~bits : (bits | 64'h8000_0000_0000_0000);
            else
                order_key = bits[31] ? {32'd0, ~bits[31:0]}
                                     : {32'd0, bits[31:0] | 32'h8000_0000};
        end
    endfunction

    wire [63:0] fmt_mask = is64 ? {64{1'b1}} : 64'h0000_0000_FFFF_FFFF;

    wire [73:0] da = decode(fmt, a_r);   // Rd
    wire [73:0] db = decode(fmt, b_r);   // Rs
    wire [73:0] dc = decode(fmt, c_r);   // Rt

    `define D_NAN(d)  d[73]
    `define D_SNAN(d) d[72]
    `define D_INF(d)  d[71]
    `define D_ZERO(d) d[70]
    `define D_SIGN(d) d[69]
    `define D_EXP(d)  $signed(d[68:53])
    `define D_SIG(d)  d[52:0]

    // ------------------------------------------------------------------
    // FMA lane: FADD, FSUB, FMUL, FMA, FMS
    // ------------------------------------------------------------------
    reg [73:0] fa, fb, fc;
    always @(*) begin
        // 1.0 and a signed zero as exact descriptors.
        fa = da;
        fb = {5'b00000, 16'd0, 53'd1};
        fc = db;
        case (code)
            6'h01: fc = {db[73:70], ~db[69], db[68:0]};              // Rd - Rs
            6'h02: begin                                             // Rd * Rs
                fb = db;
                fc = {3'b000, 1'b1, rm_r != RDN, 16'd0, 53'd0};
            end
            6'h07: begin fa = db; fb = dc; fc = da; end              // Rs*Rt + Rd
            6'h08: begin                                             // Rd - Rs*Rt
                fa = {db[73:70], ~db[69], db[68:0]};
                fb = dc;
                fc = da;
            end
            default: ;                                               // Rd*1 + Rs
        endcase
    end

    wire [63:0] fma_result;
    wire [4:0]  fma_flags;

    mp64_fma_core #(.SW(53)) u_fma (
        .a_nan (`D_NAN(fa)), .a_inf (`D_INF(fa)), .a_zero(`D_ZERO(fa)),
        .a_sign(`D_SIGN(fa)), .a_exp (fa[66:53]), .a_sig (`D_SIG(fa)),
        .b_nan (`D_NAN(fb)), .b_inf (`D_INF(fb)), .b_zero(`D_ZERO(fb)),
        .b_sign(`D_SIGN(fb)), .b_exp (fb[66:53]), .b_sig (`D_SIG(fb)),
        .c_nan (`D_NAN(fc)), .c_inf (`D_INF(fc)), .c_zero(`D_ZERO(fc)),
        .c_sign(`D_SIGN(fc)), .c_exp (fc[66:53]), .c_sig (`D_SIG(fc)),
        .out64 (is64),
        .rm    (rm_r),
        .result(fma_result),
        .flags (fma_flags)
    );

    // ------------------------------------------------------------------
    // Divide and square root
    // ------------------------------------------------------------------
    wire        start_now = start && !busy;
    wire [2:0]  start_rm  = (op[5:0] >= 6'h20 && op[5:0] < 6'h38 &&
                             op[2:0] != 3'd7) ? op[2:0] : rm_dyn;
    wire        ds_ready;
    wire [63:0] ds_result;
    wire [4:0]  ds_flags;

    mp64_fp_divsqrt u_divsqrt (
        .clk   (clk),
        .rst   (rst),
        .start (start_now && (op[5:0] == 6'h03 || op[5:0] == 6'h04)),
        .sqrt  (op[5:0] == 6'h04),
        .fmt   (op[6] ? FMT_D : FMT_S),
        .rm    (start_rm),
        .a     (rd_val),
        .b     (rs_val),
        .ready (ds_ready),
        .result(ds_result),
        .flags (ds_flags)
    );

    // ------------------------------------------------------------------
    // Conversions, FRND, and recurrence results share one rounder
    // ------------------------------------------------------------------
    reg               rnd_sign;
    reg [63:0]        rnd_mag;
    reg signed [15:0] rnd_lsb_exp;
    reg               rnd_sticky;
    reg [1:0]         rnd_fmt;
    wire [63:0]       rnd_result;
    wire [4:0]        rnd_flags;

    mp64_fp_round #(.MW(64)) u_round (
        .sign     (rnd_sign),
        .mag      (rnd_mag),
        .lsb_exp  (rnd_lsb_exp),
        .sticky_in(rnd_sticky),
        .fmt      (rnd_fmt),
        .rm       (rm_r),
        .result   (rnd_result),
        .flags    (rnd_flags)
    );

    // Round a finite descriptor's value to an integer in rm_r:
    // {rounded magnitude[64:0], inexact}.  Magnitudes of 2**64 and above
    // saturate to all ones in bit 64 so range checks fail.
    function [65:0] to_integer;
        input [73:0] d;
        input [2:0]  mode;
        integer      exponent;
        integer      lead;
        reg   [64:0] magnitude;
        reg   [52:0] below;
        reg          round_bit;
        reg          sticky;
        begin
            exponent  = `D_EXP(d);
            lead      = msb53(`D_SIG(d));
            magnitude = 65'd0;
            round_bit = 1'b0;
            sticky    = 1'b0;
            if (`D_ZERO(d)) begin
                magnitude = 65'd0;
            end else if (exponent >= 0) begin
                if (lead + exponent >= 64)
                    magnitude = {1'b1, 64'd0};
                else
                    magnitude = {12'd0, `D_SIG(d)} << exponent;
            end else if (-exponent > 53) begin
                sticky = 1'b1;
            end else begin
                magnitude = {12'd0, `D_SIG(d)} >> (-exponent);
                round_bit = `D_SIG(d) >> (-exponent - 1);
                below     = ~({53{1'b1}} << (-exponent - 1));
                sticky    = |(`D_SIG(d) & below);
            end
            if (round_up(mode, `D_SIGN(d), magnitude[0], round_bit, sticky))
                magnitude = magnitude + 65'd1;
            to_integer = {magnitude, round_bit || sticky};
        end
    endfunction

    // Conversion formats, and the integer FRND and float-to-int round to.
    reg [1:0] cvt_src_fmt;
    reg [1:0] cvt_dst_fmt;
    always @(*) begin
        case (code)
            6'h3A:   begin cvt_src_fmt = is64 ? FMT_S : FMT_D; cvt_dst_fmt = fmt;   end
            6'h3B:   begin cvt_src_fmt = fmt;                  cvt_dst_fmt = FMT_H; end
            6'h3C:   begin cvt_src_fmt = FMT_H;                cvt_dst_fmt = fmt;   end
            6'h3D:   begin cvt_src_fmt = fmt;                  cvt_dst_fmt = FMT_B; end
            6'h3E:   begin cvt_src_fmt = FMT_B;                cvt_dst_fmt = fmt;   end
            default: begin cvt_src_fmt = fmt;                  cvt_dst_fmt = fmt;   end
        endcase
    end
    wire [73:0] cvt_src  = decode(cvt_src_fmt, b_r);
    wire [65:0] integral = to_integer(db, rm_r);

    // Rounder inputs.  This block never reads the rounder's outputs, so the
    // result selection below cannot wake it again.
    always @(*) begin : round_inputs
        rnd_sign    = 1'b0;
        rnd_mag     = 64'd0;
        rnd_lsb_exp = 16'sd0;
        rnd_sticky  = 1'b0;
        rnd_fmt     = fmt;
        case (code)
            6'h38, 6'h39: begin
                rnd_sign = code == 6'h38 && b_r[63];
                rnd_mag  = rnd_sign ? (~b_r + 64'd1) : b_r;
            end
            6'h3A, 6'h3B, 6'h3C, 6'h3D, 6'h3E: begin
                rnd_fmt     = cvt_dst_fmt;
                rnd_sign    = `D_SIGN(cvt_src);
                rnd_mag     = {11'd0, `D_SIG(cvt_src)};
                rnd_lsb_exp = `D_EXP(cvt_src);
            end
            default: begin
                if (code >= 6'h20 && code < 6'h28) begin
                    rnd_sign = `D_SIGN(db);
                    rnd_mag  = integral[64:1];
                end
            end
        endcase
    end

    // ------------------------------------------------------------------
    // Result selection
    // ------------------------------------------------------------------
    reg [63:0] comb_result;
    reg [4:0]  comb_flags;
    reg [3:0]  comb_cmp;
    reg        comb_write;

    always @(*) begin : select
        reg [64:0] magnitude;
        reg        inexact;
        reg        either_nan;
        reg        equal;
        reg        less;
        reg [63:0] key_a;
        reg [63:0] key_b;
        reg        signed_target;

        comb_result = 64'd0;
        comb_flags  = 5'd0;
        comb_cmp    = 4'd0;
        comb_write  = 1'b1;
        magnitude   = 65'd0;
        inexact     = 1'b0;

        either_nan = `D_NAN(da) || `D_NAN(db);
        equal = !either_nan &&
                ((`D_ZERO(da) && `D_ZERO(db)) ||
                 ((a_r & fmt_mask) == (b_r & fmt_mask)));
        key_a = order_key(is64, a_r);
        key_b = order_key(is64, b_r);
        less  = !either_nan && !equal && key_a < key_b;

        case (code)
            6'h00, 6'h01, 6'h02, 6'h07, 6'h08: begin
                comb_result = fma_result;
                comb_flags  = fma_flags;
                if (`D_SNAN(da) || `D_SNAN(db) ||
                    ((code == 6'h07 || code == 6'h08) && `D_SNAN(dc)))
                    comb_flags[4] = 1'b1;
            end

            6'h03, 6'h04: begin                                     // FDIV, FSQRT
                comb_result = ds_result;
                comb_flags  = ds_flags;
            end

            6'h05, 6'h06: begin                                     // FMIN, FMAX
                if (either_nan)
                    comb_result = canonical_nan(fmt);
                else if (code == 6'h05)
                    comb_result = (key_a <= key_b ? a_r : b_r) & fmt_mask;
                else
                    comb_result = (key_a >= key_b ? a_r : b_r) & fmt_mask;
                comb_flags[4] = `D_SNAN(da) || `D_SNAN(db);
            end

            6'h10, 6'h11, 6'h12, 6'h13: begin                       // compares
                comb_cmp = {either_nan, less, !either_nan && !equal && !less,
                            equal};
                comb_write = code != 6'h10;
                case (code)
                    6'h11: comb_result = equal ? {64{1'b1}} : 64'd0;
                    6'h12: comb_result = less ? {64{1'b1}} : 64'd0;
                    6'h13: comb_result = (less || equal) ? {64{1'b1}} : 64'd0;
                    default: comb_result = 64'd0;
                endcase
                comb_flags[4] = (code == 6'h12 || code == 6'h13)
                    ? either_nan : (`D_SNAN(da) || `D_SNAN(db));
            end

            6'h14: begin                                            // FCLASS
                if (`D_NAN(db))
                    comb_result = `D_SNAN(db) ? 64'd256 : 64'd512;
                else if (`D_INF(db))
                    comb_result = `D_SIGN(db) ? 64'd1 : 64'd128;
                else if (`D_ZERO(db))
                    comb_result = `D_SIGN(db) ? 64'd8 : 64'd16;
                else if ((`D_SIG(db) >> (is64 ? 52 : 23)) == 53'd0)
                    comb_result = `D_SIGN(db) ? 64'd4 : 64'd32;
                else
                    comb_result = `D_SIGN(db) ? 64'd2 : 64'd64;
            end

            6'h38, 6'h39: begin                                     // int -> float
                if (b_r == 64'd0) begin
                    comb_result = 64'd0;
                end else begin
                    comb_result = rnd_result;
                    comb_flags  = rnd_flags;
                end
            end

            6'h3A, 6'h3B, 6'h3C, 6'h3D, 6'h3E: begin                // float -> float
                if (`D_NAN(cvt_src)) begin
                    comb_result = canonical_nan(cvt_dst_fmt);
                    comb_flags[4] = `D_SNAN(cvt_src);
                end else if (`D_INF(cvt_src)) begin
                    comb_result = infinity_of(cvt_dst_fmt, `D_SIGN(cvt_src));
                end else if (`D_ZERO(cvt_src)) begin
                    comb_result = zero_of(cvt_dst_fmt, `D_SIGN(cvt_src));
                end else begin
                    comb_result = rnd_result;
                    comb_flags  = rnd_flags;
                end
            end

            default: begin
                if (code >= 6'h20 && code < 6'h28) begin            // FRND
                    if (`D_NAN(db)) begin
                        comb_result = canonical_nan(fmt);
                        comb_flags[4] = `D_SNAN(db);
                    end else if (`D_INF(db) || `D_ZERO(db) ||
                                 `D_EXP(db) >= 0) begin
                        comb_result = b_r & fmt_mask;
                    end else begin
                        if (integral[65:1] == 65'd0) begin
                            comb_result = zero_of(fmt, `D_SIGN(db));
                        end else begin
                            comb_result = rnd_result;    // exact: no flags
                        end
                    end
                end else begin                               // float -> int
                    signed_target = code < 6'h30;
                    if (`D_NAN(db)) begin
                        comb_result = 64'd0;
                        comb_flags[4] = 1'b1;
                    end else begin
                        if (`D_INF(db)) begin
                            magnitude = {1'b1, 64'd0};
                            inexact   = 1'b0;
                        end else begin
                            magnitude = integral[65:1];
                            inexact   = integral[0];
                        end
                        if (signed_target && !`D_SIGN(db) &&
                            magnitude > 65'h0_7FFF_FFFF_FFFF_FFFF) begin
                            comb_result = 64'h7FFF_FFFF_FFFF_FFFF;
                            comb_flags[4] = 1'b1;
                        end else if (signed_target && `D_SIGN(db) &&
                                     magnitude > 65'h0_8000_0000_0000_0000) begin
                            comb_result = 64'h8000_0000_0000_0000;
                            comb_flags[4] = 1'b1;
                        end else if (!signed_target && !`D_SIGN(db) &&
                                     magnitude[64]) begin
                            comb_result = {64{1'b1}};
                            comb_flags[4] = 1'b1;
                        end else if (!signed_target && `D_SIGN(db) &&
                                     magnitude != 65'd0) begin
                            comb_result = 64'd0;
                            comb_flags[4] = 1'b1;
                        end else begin
                            comb_result = `D_SIGN(db) ? (~magnitude[63:0] + 64'd1)
                                                      : magnitude[63:0];
                            comb_flags[0] = inexact;
                        end
                    end
                end
            end
        endcase
    end

    // ------------------------------------------------------------------
    // Sequencing
    // ------------------------------------------------------------------
    always @(posedge clk) begin : sequencing
        done <= 1'b0;
        if (rst) begin
            busy      <= 1'b0;
            remaining <= 6'd0;
            result    <= 64'd0;
            flags     <= 5'd0;
            cmp       <= 4'd0;
            write_rd  <= 1'b0;
        end else if (start_now) begin
            busy      <= 1'b1;
            op_r      <= op;
            a_r       <= rd_val;
            b_r       <= rs_val;
            c_r       <= rt_val;
            rm_r      <= start_rm;
            remaining <= fp_extra_cycles(op);
        end else if (busy) begin
            remaining <= remaining - 6'd1;
            if (remaining == 6'd1) begin
                busy     <= 1'b0;
                done     <= 1'b1;
                result   <= comb_result;
                flags    <= comb_flags;
                cmp      <= comb_cmp;
                write_rd <= comb_write;
            end
        end
    end

    `undef D_NAN
    `undef D_SNAN
    `undef D_INF
    `undef D_ZERO
    `undef D_SIGN
    `undef D_EXP
    `undef D_SIG
endmodule
