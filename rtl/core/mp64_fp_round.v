// ============================================================================
// mp64_fp_round.v — round an exact magnitude once to an IEEE format
// ============================================================================
//
// docs/floating-point.md §3 defines rounding.  The input is a nonzero
// magnitude mag * 2**lsb_exp.  When sticky_in is set, the true magnitude lies
// strictly between mag and mag + 1 at that weight; a caller that sets it must
// supply at least one bit below the rounding position, as a jammed sum or a
// quotient with a guard bit does.  The result is rounded once in rm to
// binary16, bfloat16, binary32, or binary64, with full subnormal support.
// Tininess is detected after rounding, and UF is raised only when the result
// is tiny and inexact.
//
// flags is {NV, DZ, OF, UF, NX}, the FPCSR bit order; this module raises only
// OF, UF, and NX.  Results narrower than 64 bits are zero-extended.
//
// This file contains only bounded, combinational integer logic.
// ============================================================================

module mp64_fp_round #(
    parameter MW = 64
) (
    input  wire               sign,
    input  wire [MW-1:0]      mag,
    input  wire signed [15:0] lsb_exp,
    input  wire               sticky_in,
    input  wire [1:0]         fmt,       // 0 binary16, 1 bfloat16, 2 binary32, 3 binary64
    input  wire [2:0]         rm,        // 0 RNE, 1 RTZ, 2 RDN, 3 RUP, 4 RMM
    output reg  [63:0]        result,
    output reg  [4:0]         flags
);
    localparam FMT_H = 2'd0;
    localparam FMT_B = 2'd1;
    localparam FMT_S = 2'd2;

    function integer msb_index;
        input [MW-1:0] value;
        integer i;
        begin
            msb_index = -1;
            for (i = 0; i < MW; i = i + 1)
                if (value[i])
                    msb_index = i;
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

    function [63:0] pack;
        input [1:0]  format;
        input        negative;
        input [10:0] biased;
        input [51:0] fraction;
        begin
            case (format)
                FMT_H:   pack = {48'd0, negative, biased[4:0], fraction[9:0]};
                FMT_B:   pack = {48'd0, negative, biased[7:0], fraction[6:0]};
                FMT_S:   pack = {32'd0, negative, biased[7:0], fraction[22:0]};
                default: pack = {negative, biased[10:0], fraction[51:0]};
            endcase
        end
    endfunction

    // Split value at `amount`: {kept, round bit, sticky}, where sticky ORs
    // every lower bit and the caller's sticky.
    function [MW+2:0] split;
        input [MW-1:0] value;
        input          sticky_below;
        input integer  amount;
        reg   [MW-1:0] below;
        begin
            if (amount <= 0) begin
                split = {({1'b0, value} << (-amount)), 1'b0, sticky_below};
            end else if (amount > MW) begin
                split = {{(MW+1){1'b0}}, 1'b0, 1'b1};
            end else begin
                below = ~({MW{1'b1}} << (amount - 1));
                split = {({1'b0, value} >> amount), value[amount - 1],
                         (|(value & below)) || sticky_below};
            end
        end
    endfunction

    always @(*) begin : round
        integer precision;
        integer emin;
        integer bias;
        integer field_max;
        integer lead;
        integer lead_exp;
        integer quantum;
        integer shift;
        integer biased;
        reg [MW:0] kept;
        reg [MW:0] kept_p;
        reg        round_bit;
        reg        sticky;
        reg        round_p;
        reg        sticky_p;
        reg        inexact;
        reg        tiny;
        reg [63:0] largest;
        reg [63:0] infinity;

        case (fmt)
            FMT_H: begin precision = 11; emin = -14;   bias = 15;   field_max = 31;   end
            FMT_B: begin precision = 8;  emin = -126;  bias = 127;  field_max = 255;  end
            FMT_S: begin precision = 24; emin = -126;  bias = 127;  field_max = 255;  end
            default: begin precision = 53; emin = -1022; bias = 1023; field_max = 2047; end
        endcase
        infinity = pack(fmt, sign, field_max[10:0], 52'd0);
        largest  = pack(fmt, sign, field_max[10:0] - 11'd1, {52{1'b1}});

        flags  = 5'd0;
        result = pack(fmt, sign, 11'd0, 52'd0);
        tiny   = 1'b0;

        if (mag != {MW{1'b0}}) begin
            lead     = msb_index(mag);
            lead_exp = lsb_exp + lead;
            quantum  = lead_exp - (precision - 1);
            if (quantum < emin - (precision - 1))
                quantum = emin - (precision - 1);
            shift = quantum - lsb_exp;
            {kept, round_bit, sticky} = split(mag, sticky_in, shift);
            inexact = round_bit || sticky;

            // Tininess after rounding: round to `precision` bits with an
            // unbounded exponent and see whether the result reaches 2**emin.
            if (lead_exp < emin - 1) begin
                tiny = 1'b1;
            end else if (lead_exp == emin - 1) begin
                if (lead - (precision - 1) <= 0) begin
                    tiny = 1'b1;
                end else begin
                    {kept_p, round_p, sticky_p} =
                        split(mag, sticky_in, lead - (precision - 1));
                    tiny = !(kept_p == ({{MW{1'b0}}, 1'b1} << precision) -
                                       {{MW{1'b0}}, 1'b1} &&
                             round_up(rm, sign, kept_p[0], round_p, sticky_p));
                end
            end

            if (round_up(rm, sign, kept[0], round_bit, sticky))
                kept = kept + 1'b1;
            if (kept[precision]) begin
                kept    = kept >> 1;
                quantum = quantum + 1;
            end

            flags[0] = inexact;
            flags[1] = inexact && lead_exp < emin && tiny;
            if (kept[precision - 1]) begin
                biased = quantum + (precision - 1) + bias;
                if (biased >= field_max) begin
                    flags[2] = 1'b1;
                    flags[0] = 1'b1;
                    case (rm)
                        3'd1:    result = largest;
                        3'd2:    result = sign ? infinity : largest;
                        3'd3:    result = sign ? largest : infinity;
                        default: result = infinity;
                    endcase
                end else begin
                    result = pack(fmt, sign, biased[10:0], kept[51:0]);
                end
            end else begin
                result = pack(fmt, sign, 11'd0, kept[51:0]);
            end
        end
    end
endmodule
