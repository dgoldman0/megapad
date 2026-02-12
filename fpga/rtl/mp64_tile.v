// ============================================================================
// mp64_tile.v — Megapad-64 Tile Engine (MEX Unit)
// ============================================================================
//
// The tile engine is the primary compute accelerator.  It performs SIMD
// operations on 64-byte tiles (512 bits), processing up to 64 parallel
// lanes in 8-bit mode, 32 in 16-bit, 16 in 32-bit, or 8 in 64-bit.
//
// Supports: TALU (8 ops), TMUL (6 ops), TRED (8 ops), TSYS (8 ops),
//           EXT.8 extended TALU (VSHR/VSHL/VSEL/VCLZ),
//           signed/unsigned modes, saturating arithmetic.
//

`include "mp64_defs.vh"

module mp64_tile (
    input  wire        clk,
    input  wire        rst_n,

    // === CPU interface (CSR read/write + MEX dispatch) ===
    input  wire        csr_wen,
    input  wire [7:0]  csr_addr,
    input  wire [63:0] csr_wdata,
    output reg  [63:0] csr_rdata,

    input  wire        mex_valid,      // MEX instruction decoded
    input  wire [1:0]  mex_ss,         // source selector
    input  wire [1:0]  mex_op,         // operation class
    input  wire [2:0]  mex_funct,      // sub-function
    input  wire [63:0] mex_gpr_val,    // GPR value (for broadcast mode)
    input  wire [7:0]  mex_imm8,       // immediate (for splat mode)
    input  wire [3:0]  mex_ext_mod,    // EXT prefix modifier
    input  wire        mex_ext_active, // EXT prefix is active
    output reg         mex_done,       // operation complete
    output reg         mex_busy,       // engine busy (stall CPU)

    // === Tile memory port (512-bit, directly to BRAM Port A) ===
    output reg         tile_req,
    output reg  [19:0] tile_addr,
    output reg         tile_wen,
    output reg  [511:0]tile_wdata,
    input  wire [511:0]tile_rdata,
    input  wire        tile_ack,

    // === External memory tile access (for tiles in external RAM) ===
    output reg         ext_tile_req,
    output reg  [63:0] ext_tile_addr,
    output reg         ext_tile_wen,
    output reg  [511:0]ext_tile_wdata,
    input  wire [511:0]ext_tile_rdata,
    input  wire        ext_tile_ack
);

    // ========================================================================
    // CSR registers
    // ========================================================================
    reg [63:0] tsrc0;
    reg [63:0] tsrc1;
    reg [63:0] tdst;
    reg [63:0] tmode;        // bits[2:0]=EW, bit[4]=signed, bit[5]=saturate
    reg [63:0] tctrl;        // bit[0]=accumulate, bit[1]=acc_zero
    reg [63:0] acc [0:3];    // 256-bit accumulator (4 × 64-bit)
    reg [63:0] tile_bank;
    reg [63:0] tile_row;
    reg [63:0] tile_col;
    reg [63:0] tile_stride;

    // CSR read mux
    always @(*) begin
        csr_rdata = 64'd0;
        case (csr_addr)
            CSR_TMODE: csr_rdata = tmode;
            CSR_TCTRL: csr_rdata = tctrl;
            CSR_TSRC0: csr_rdata = tsrc0;
            CSR_TSRC1: csr_rdata = tsrc1;
            CSR_TDST:  csr_rdata = tdst;
            CSR_ACC0:  csr_rdata = acc[0];
            CSR_ACC1:  csr_rdata = acc[1];
            CSR_ACC2:  csr_rdata = acc[2];
            CSR_ACC3:  csr_rdata = acc[3];
            CSR_SB:    csr_rdata = tile_bank;
            CSR_SR:    csr_rdata = tile_row;
            CSR_SC:    csr_rdata = tile_col;
            CSR_SW:    csr_rdata = tile_stride;
            default:   csr_rdata = 64'd0;
        endcase
    end

    // CSR write
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            tsrc0       <= 64'd0;
            tsrc1       <= 64'd0;
            tdst        <= 64'd0;
            tmode       <= 64'd0;
            tctrl       <= 64'd0;
            acc[0]      <= 64'd0;
            acc[1]      <= 64'd0;
            acc[2]      <= 64'd0;
            acc[3]      <= 64'd0;
            tile_bank   <= 64'd0;
            tile_row    <= 64'd0;
            tile_col    <= 64'd0;
            tile_stride <= 64'd0;
        end else if (csr_wen) begin
            case (csr_addr)
                CSR_TMODE: tmode       <= csr_wdata;
                CSR_TCTRL: tctrl       <= csr_wdata;
                CSR_TSRC0: tsrc0       <= csr_wdata;
                CSR_TSRC1: tsrc1       <= csr_wdata;
                CSR_TDST:  tdst        <= csr_wdata;
                CSR_ACC0:  acc[0]      <= csr_wdata;
                CSR_ACC1:  acc[1]      <= csr_wdata;
                CSR_ACC2:  acc[2]      <= csr_wdata;
                CSR_ACC3:  acc[3]      <= csr_wdata;
                CSR_SB:    tile_bank   <= csr_wdata;
                CSR_SR:    tile_row    <= csr_wdata;
                CSR_SC:    tile_col    <= csr_wdata;
                CSR_SW:    tile_stride <= csr_wdata;
                default: ;  // no-op for unrecognized CSR
            endcase
        end
    end

    // ========================================================================
    // Address classification
    // ========================================================================
    wire src0_internal = (tsrc0[63:20] == 44'd0);
    wire src1_internal = (tsrc1[63:20] == 44'd0);
    wire dst_internal  = (tdst[63:20]  == 44'd0);

    // ========================================================================
    // Mode decode
    // ========================================================================
    wire [2:0] mode_ew       = tmode[2:0];
    wire       mode_signed   = tmode[4];
    wire       mode_saturate = tmode[5];

    // ========================================================================
    // State machine
    // ========================================================================
    localparam S_IDLE       = 4'd0;
    localparam S_LOAD_A     = 4'd1;
    localparam S_LOAD_B     = 4'd2;
    localparam S_COMPUTE    = 4'd3;
    localparam S_STORE      = 4'd4;
    localparam S_REDUCE     = 4'd5;
    localparam S_EXT_LOAD_A = 4'd6;
    localparam S_EXT_LOAD_B = 4'd7;
    localparam S_EXT_STORE  = 4'd8;
    localparam S_DONE       = 4'd9;
    localparam S_LOAD_C     = 4'd10;   // load existing TDST for MAC/FMA
    localparam S_STORE2     = 4'd11;   // second tile store for WMUL

    reg [3:0]   state;
    reg [511:0] tile_a;
    reg [511:0] tile_b;
    reg [511:0] tile_c;          // existing TDST (MAC/FMA)
    reg [511:0] result;
    reg [511:0] result2;         // second result tile (WMUL high half)
    reg [1:0]   op_reg;
    reg [2:0]   funct_reg;
    reg [1:0]   ss_reg;
    reg [63:0]  gpr_val_reg;
    reg [7:0]   imm8_reg;
    reg [3:0]   ext_mod_reg;
    reg         ext_active_reg;
    reg         needs_load_c;

    // Source B selection
    reg [511:0] src_b_selected;
    always @(*) begin
        case (ss_reg)
            2'd0: src_b_selected = tile_b;
            2'd1: src_b_selected = {8{gpr_val_reg}};
            2'd2: src_b_selected = {64{imm8_reg}};
            2'd3: src_b_selected = tile_a;      // in-place
            default: src_b_selected = 512'd0;
        endcase
    end

    // ========================================================================
    // Lane ALU — 8-bit (64 lanes)
    // ========================================================================
    reg [511:0] alu_result_8;
    integer lane8;
    always @(*) begin
        alu_result_8 = 512'd0;
        for (lane8 = 0; lane8 < 64; lane8 = lane8 + 1) begin : alu8
            reg [7:0] a8, b8;
            reg [8:0] sum9;
            a8 = tile_a[lane8*8 +: 8];
            b8 = src_b_selected[lane8*8 +: 8];
            sum9 = 9'd0;
            case (funct_reg)
                TALU_ADD: begin
                    if (mode_saturate) begin
                        if (mode_signed) begin
                            sum9 = {a8[7], a8} + {b8[7], b8};
                            if (!sum9[8] && sum9[7]) alu_result_8[lane8*8 +: 8] = 8'h7F;
                            else if (sum9[8] && !sum9[7]) alu_result_8[lane8*8 +: 8] = 8'h80;
                            else alu_result_8[lane8*8 +: 8] = sum9[7:0];
                        end else begin
                            sum9 = {1'b0, a8} + {1'b0, b8};
                            alu_result_8[lane8*8 +: 8] = sum9[8] ? 8'hFF : sum9[7:0];
                        end
                    end else
                        alu_result_8[lane8*8 +: 8] = a8 + b8;
                end
                TALU_SUB: begin
                    if (mode_saturate) begin
                        if (mode_signed) begin
                            sum9 = {a8[7], a8} - {b8[7], b8};
                            if (!sum9[8] && sum9[7]) alu_result_8[lane8*8 +: 8] = 8'h7F;
                            else if (sum9[8] && !sum9[7]) alu_result_8[lane8*8 +: 8] = 8'h80;
                            else alu_result_8[lane8*8 +: 8] = sum9[7:0];
                        end else begin
                            if (a8 < b8) alu_result_8[lane8*8 +: 8] = 8'd0;
                            else alu_result_8[lane8*8 +: 8] = a8 - b8;
                        end
                    end else
                        alu_result_8[lane8*8 +: 8] = a8 - b8;
                end
                TALU_AND: alu_result_8[lane8*8 +: 8] = a8 & b8;
                TALU_OR:  alu_result_8[lane8*8 +: 8] = a8 | b8;
                TALU_XOR: alu_result_8[lane8*8 +: 8] = a8 ^ b8;
                TALU_MIN: begin
                    if (mode_signed) alu_result_8[lane8*8 +: 8] = ($signed(a8) < $signed(b8)) ? a8 : b8;
                    else             alu_result_8[lane8*8 +: 8] = (a8 < b8) ? a8 : b8;
                end
                TALU_MAX: begin
                    if (mode_signed) alu_result_8[lane8*8 +: 8] = ($signed(a8) > $signed(b8)) ? a8 : b8;
                    else             alu_result_8[lane8*8 +: 8] = (a8 > b8) ? a8 : b8;
                end
                TALU_ABS: begin
                    if (mode_signed && a8[7]) alu_result_8[lane8*8 +: 8] = (~a8) + 8'd1;
                    else                      alu_result_8[lane8*8 +: 8] = a8;
                end
                default: alu_result_8[lane8*8 +: 8] = 8'd0;
            endcase
        end
    end

    // ========================================================================
    // Lane ALU — 16-bit (32 lanes)
    // ========================================================================
    reg [511:0] alu_result_16;
    integer lane16;
    always @(*) begin
        alu_result_16 = 512'd0;
        for (lane16 = 0; lane16 < 32; lane16 = lane16 + 1) begin : alu16
            reg [15:0] a16, b16;
            reg [16:0] sum17;
            a16 = tile_a[lane16*16 +: 16];
            b16 = src_b_selected[lane16*16 +: 16];
            sum17 = 17'd0;
            case (funct_reg)
                TALU_ADD: begin
                    if (mode_saturate) begin
                        if (mode_signed) begin
                            sum17 = {a16[15], a16} + {b16[15], b16};
                            if (!sum17[16] && sum17[15]) alu_result_16[lane16*16 +: 16] = 16'h7FFF;
                            else if (sum17[16] && !sum17[15]) alu_result_16[lane16*16 +: 16] = 16'h8000;
                            else alu_result_16[lane16*16 +: 16] = sum17[15:0];
                        end else begin
                            sum17 = {1'b0, a16} + {1'b0, b16};
                            alu_result_16[lane16*16 +: 16] = sum17[16] ? 16'hFFFF : sum17[15:0];
                        end
                    end else
                        alu_result_16[lane16*16 +: 16] = a16 + b16;
                end
                TALU_SUB: begin
                    if (mode_saturate) begin
                        if (mode_signed) begin
                            sum17 = {a16[15], a16} - {b16[15], b16};
                            if (!sum17[16] && sum17[15]) alu_result_16[lane16*16 +: 16] = 16'h7FFF;
                            else if (sum17[16] && !sum17[15]) alu_result_16[lane16*16 +: 16] = 16'h8000;
                            else alu_result_16[lane16*16 +: 16] = sum17[15:0];
                        end else begin
                            if (a16 < b16) alu_result_16[lane16*16 +: 16] = 16'd0;
                            else alu_result_16[lane16*16 +: 16] = a16 - b16;
                        end
                    end else
                        alu_result_16[lane16*16 +: 16] = a16 - b16;
                end
                TALU_AND: alu_result_16[lane16*16 +: 16] = a16 & b16;
                TALU_OR:  alu_result_16[lane16*16 +: 16] = a16 | b16;
                TALU_XOR: alu_result_16[lane16*16 +: 16] = a16 ^ b16;
                TALU_MIN: begin
                    if (mode_signed) alu_result_16[lane16*16 +: 16] = ($signed(a16) < $signed(b16)) ? a16 : b16;
                    else             alu_result_16[lane16*16 +: 16] = (a16 < b16) ? a16 : b16;
                end
                TALU_MAX: begin
                    if (mode_signed) alu_result_16[lane16*16 +: 16] = ($signed(a16) > $signed(b16)) ? a16 : b16;
                    else             alu_result_16[lane16*16 +: 16] = (a16 > b16) ? a16 : b16;
                end
                TALU_ABS: begin
                    if (mode_signed && a16[15]) alu_result_16[lane16*16 +: 16] = (~a16) + 16'd1;
                    else                        alu_result_16[lane16*16 +: 16] = a16;
                end
                default: alu_result_16[lane16*16 +: 16] = 16'd0;
            endcase
        end
    end

    // ========================================================================
    // Lane ALU — 32-bit (16 lanes)
    // ========================================================================
    reg [511:0] alu_result_32;
    integer lane32;
    always @(*) begin
        alu_result_32 = 512'd0;
        for (lane32 = 0; lane32 < 16; lane32 = lane32 + 1) begin : alu32
            reg [31:0] a32, b32;
            reg [32:0] sum33;
            a32 = tile_a[lane32*32 +: 32];
            b32 = src_b_selected[lane32*32 +: 32];
            sum33 = 33'd0;
            case (funct_reg)
                TALU_ADD: begin
                    if (mode_saturate) begin
                        if (mode_signed) begin
                            sum33 = {a32[31], a32} + {b32[31], b32};
                            if (!sum33[32] && sum33[31]) alu_result_32[lane32*32 +: 32] = 32'h7FFFFFFF;
                            else if (sum33[32] && !sum33[31]) alu_result_32[lane32*32 +: 32] = 32'h80000000;
                            else alu_result_32[lane32*32 +: 32] = sum33[31:0];
                        end else begin
                            sum33 = {1'b0, a32} + {1'b0, b32};
                            alu_result_32[lane32*32 +: 32] = sum33[32] ? 32'hFFFFFFFF : sum33[31:0];
                        end
                    end else
                        alu_result_32[lane32*32 +: 32] = a32 + b32;
                end
                TALU_SUB: begin
                    if (mode_saturate) begin
                        if (mode_signed) begin
                            sum33 = {a32[31], a32} - {b32[31], b32};
                            if (!sum33[32] && sum33[31]) alu_result_32[lane32*32 +: 32] = 32'h7FFFFFFF;
                            else if (sum33[32] && !sum33[31]) alu_result_32[lane32*32 +: 32] = 32'h80000000;
                            else alu_result_32[lane32*32 +: 32] = sum33[31:0];
                        end else begin
                            if (a32 < b32) alu_result_32[lane32*32 +: 32] = 32'd0;
                            else alu_result_32[lane32*32 +: 32] = a32 - b32;
                        end
                    end else
                        alu_result_32[lane32*32 +: 32] = a32 - b32;
                end
                TALU_AND: alu_result_32[lane32*32 +: 32] = a32 & b32;
                TALU_OR:  alu_result_32[lane32*32 +: 32] = a32 | b32;
                TALU_XOR: alu_result_32[lane32*32 +: 32] = a32 ^ b32;
                TALU_MIN: begin
                    if (mode_signed) alu_result_32[lane32*32 +: 32] = ($signed(a32) < $signed(b32)) ? a32 : b32;
                    else             alu_result_32[lane32*32 +: 32] = (a32 < b32) ? a32 : b32;
                end
                TALU_MAX: begin
                    if (mode_signed) alu_result_32[lane32*32 +: 32] = ($signed(a32) > $signed(b32)) ? a32 : b32;
                    else             alu_result_32[lane32*32 +: 32] = (a32 > b32) ? a32 : b32;
                end
                TALU_ABS: begin
                    if (mode_signed && a32[31]) alu_result_32[lane32*32 +: 32] = (~a32) + 32'd1;
                    else                        alu_result_32[lane32*32 +: 32] = a32;
                end
                default: alu_result_32[lane32*32 +: 32] = 32'd0;
            endcase
        end
    end

    // ========================================================================
    // Lane ALU — 64-bit (8 lanes)
    // ========================================================================
    reg [511:0] alu_result_64;
    integer lane64;
    always @(*) begin
        alu_result_64 = 512'd0;
        for (lane64 = 0; lane64 < 8; lane64 = lane64 + 1) begin : alu64
            reg [63:0] a64, b64;
            a64 = tile_a[lane64*64 +: 64];
            b64 = src_b_selected[lane64*64 +: 64];
            case (funct_reg)
                TALU_ADD: alu_result_64[lane64*64 +: 64] = a64 + b64;
                TALU_SUB: begin
                    if (mode_saturate && !mode_signed && a64 < b64)
                        alu_result_64[lane64*64 +: 64] = 64'd0;
                    else
                        alu_result_64[lane64*64 +: 64] = a64 - b64;
                end
                TALU_AND: alu_result_64[lane64*64 +: 64] = a64 & b64;
                TALU_OR:  alu_result_64[lane64*64 +: 64] = a64 | b64;
                TALU_XOR: alu_result_64[lane64*64 +: 64] = a64 ^ b64;
                TALU_MIN: begin
                    if (mode_signed) alu_result_64[lane64*64 +: 64] = ($signed(a64) < $signed(b64)) ? a64 : b64;
                    else             alu_result_64[lane64*64 +: 64] = (a64 < b64) ? a64 : b64;
                end
                TALU_MAX: begin
                    if (mode_signed) alu_result_64[lane64*64 +: 64] = ($signed(a64) > $signed(b64)) ? a64 : b64;
                    else             alu_result_64[lane64*64 +: 64] = (a64 > b64) ? a64 : b64;
                end
                TALU_ABS: begin
                    if (mode_signed && a64[63]) alu_result_64[lane64*64 +: 64] = (~a64) + 64'd1;
                    else                        alu_result_64[lane64*64 +: 64] = a64;
                end
                default: alu_result_64[lane64*64 +: 64] = 64'd0;
            endcase
        end
    end

    // ALU result mux
    reg [511:0] alu_result_muxed;
    always @(*) begin
        case (mode_ew[1:0])
            2'd0: alu_result_muxed = alu_result_8;
            2'd1: alu_result_muxed = alu_result_16;
            2'd2: alu_result_muxed = alu_result_32;
            2'd3: alu_result_muxed = alu_result_64;
        endcase
    end

    // ========================================================================
    // TMUL.MUL — lane-wise multiply (truncated to element width)
    // ========================================================================
    reg [511:0] mul_result;
    integer ml;
    always @(*) begin
        mul_result = 512'd0;
        case (mode_ew[1:0])
            2'd0: for (ml = 0; ml < 64; ml = ml + 1) begin : m8
                reg [15:0] p8;
                if (mode_signed) p8 = $signed({{8{tile_a[ml*8+7]}}, tile_a[ml*8 +: 8]})
                                    * $signed({{8{src_b_selected[ml*8+7]}}, src_b_selected[ml*8 +: 8]});
                else             p8 = {8'd0, tile_a[ml*8 +: 8]} * {8'd0, src_b_selected[ml*8 +: 8]};
                mul_result[ml*8 +: 8] = p8[7:0];
            end
            2'd1: for (ml = 0; ml < 32; ml = ml + 1) begin : m16
                reg [31:0] p16;
                if (mode_signed) p16 = $signed({{16{tile_a[ml*16+15]}}, tile_a[ml*16 +: 16]})
                                     * $signed({{16{src_b_selected[ml*16+15]}}, src_b_selected[ml*16 +: 16]});
                else             p16 = {16'd0, tile_a[ml*16 +: 16]} * {16'd0, src_b_selected[ml*16 +: 16]};
                mul_result[ml*16 +: 16] = p16[15:0];
            end
            2'd2: for (ml = 0; ml < 16; ml = ml + 1) begin : m32
                reg [63:0] p32;
                if (mode_signed) p32 = $signed({{32{tile_a[ml*32+31]}}, tile_a[ml*32 +: 32]})
                                     * $signed({{32{src_b_selected[ml*32+31]}}, src_b_selected[ml*32 +: 32]});
                else             p32 = {32'd0, tile_a[ml*32 +: 32]} * {32'd0, src_b_selected[ml*32 +: 32]};
                mul_result[ml*32 +: 32] = p32[31:0];
            end
            2'd3: for (ml = 0; ml < 8; ml = ml + 1) begin : m64
                mul_result[ml*64 +: 64] = tile_a[ml*64 +: 64] * src_b_selected[ml*64 +: 64];
            end
        endcase
    end

    // ========================================================================
    // TMUL.WMUL — widening multiply (result in two tiles)
    // ========================================================================
    reg [511:0] wmul_lo, wmul_hi;
    integer wl;
    always @(*) begin
        wmul_lo = 512'd0;
        wmul_hi = 512'd0;
        case (mode_ew[1:0])
            2'd0: for (wl = 0; wl < 64; wl = wl + 1) begin : w8
                reg [15:0] wp8;
                if (mode_signed) wp8 = $signed({{8{tile_a[wl*8+7]}}, tile_a[wl*8 +: 8]})
                                     * $signed({{8{src_b_selected[wl*8+7]}}, src_b_selected[wl*8 +: 8]});
                else             wp8 = {8'd0, tile_a[wl*8 +: 8]} * {8'd0, src_b_selected[wl*8 +: 8]};
                if (wl < 32) wmul_lo[wl*16 +: 16] = wp8;
                else         wmul_hi[(wl-32)*16 +: 16] = wp8;
            end
            2'd1: for (wl = 0; wl < 32; wl = wl + 1) begin : w16
                reg [31:0] wp16;
                if (mode_signed) wp16 = $signed({{16{tile_a[wl*16+15]}}, tile_a[wl*16 +: 16]})
                                      * $signed({{16{src_b_selected[wl*16+15]}}, src_b_selected[wl*16 +: 16]});
                else             wp16 = {16'd0, tile_a[wl*16 +: 16]} * {16'd0, src_b_selected[wl*16 +: 16]};
                if (wl < 16) wmul_lo[wl*32 +: 32] = wp16;
                else         wmul_hi[(wl-16)*32 +: 32] = wp16;
            end
            2'd2: for (wl = 0; wl < 16; wl = wl + 1) begin : w32
                reg [63:0] wp32;
                if (mode_signed) wp32 = $signed({{32{tile_a[wl*32+31]}}, tile_a[wl*32 +: 32]})
                                      * $signed({{32{src_b_selected[wl*32+31]}}, src_b_selected[wl*32 +: 32]});
                else             wp32 = {32'd0, tile_a[wl*32 +: 32]} * {32'd0, src_b_selected[wl*32 +: 32]};
                if (wl < 8) wmul_lo[wl*64 +: 64] = wp32;
                else        wmul_hi[(wl-8)*64 +: 64] = wp32;
            end
            2'd3: wmul_lo = mul_result;  // can't widen 64→128
        endcase
    end

    // ========================================================================
    // TMUL.MAC / TMUL.FMA — multiply-accumulate in-place (dst += a*b)
    // ========================================================================
    reg [511:0] mac_result;
    integer mcl;
    always @(*) begin
        mac_result = 512'd0;
        case (mode_ew[1:0])
            2'd0: for (mcl = 0; mcl < 64; mcl = mcl + 1) begin : mc8
                reg [15:0] mp8;
                if (mode_signed) mp8 = $signed({{8{tile_a[mcl*8+7]}}, tile_a[mcl*8 +: 8]})
                                     * $signed({{8{src_b_selected[mcl*8+7]}}, src_b_selected[mcl*8 +: 8]});
                else             mp8 = {8'd0, tile_a[mcl*8 +: 8]} * {8'd0, src_b_selected[mcl*8 +: 8]};
                mac_result[mcl*8 +: 8] = tile_c[mcl*8 +: 8] + mp8[7:0];
            end
            2'd1: for (mcl = 0; mcl < 32; mcl = mcl + 1) begin : mc16
                reg [31:0] mp16;
                if (mode_signed) mp16 = $signed({{16{tile_a[mcl*16+15]}}, tile_a[mcl*16 +: 16]})
                                      * $signed({{16{src_b_selected[mcl*16+15]}}, src_b_selected[mcl*16 +: 16]});
                else             mp16 = {16'd0, tile_a[mcl*16 +: 16]} * {16'd0, src_b_selected[mcl*16 +: 16]};
                mac_result[mcl*16 +: 16] = tile_c[mcl*16 +: 16] + mp16[15:0];
            end
            2'd2: for (mcl = 0; mcl < 16; mcl = mcl + 1) begin : mc32
                reg [63:0] mp32;
                if (mode_signed) mp32 = $signed({{32{tile_a[mcl*32+31]}}, tile_a[mcl*32 +: 32]})
                                      * $signed({{32{src_b_selected[mcl*32+31]}}, src_b_selected[mcl*32 +: 32]});
                else             mp32 = {32'd0, tile_a[mcl*32 +: 32]} * {32'd0, src_b_selected[mcl*32 +: 32]};
                mac_result[mcl*32 +: 32] = tile_c[mcl*32 +: 32] + mp32[31:0];
            end
            2'd3: for (mcl = 0; mcl < 8; mcl = mcl + 1) begin : mc64
                mac_result[mcl*64 +: 64] = tile_c[mcl*64 +: 64] + tile_a[mcl*64 +: 64] * src_b_selected[mcl*64 +: 64];
            end
        endcase
    end

    // ========================================================================
    // TMUL.DOT — dot product → accumulator
    // ========================================================================
    reg [63:0] dot_result;
    integer dl;
    always @(*) begin
        dot_result = 64'd0;
        case (mode_ew[1:0])
            2'd0: for (dl = 0; dl < 64; dl = dl + 1) begin : d8
                reg [15:0] dp8;
                if (mode_signed) dp8 = $signed({{8{tile_a[dl*8+7]}}, tile_a[dl*8 +: 8]})
                                     * $signed({{8{src_b_selected[dl*8+7]}}, src_b_selected[dl*8 +: 8]});
                else             dp8 = {8'd0, tile_a[dl*8 +: 8]} * {8'd0, src_b_selected[dl*8 +: 8]};
                dot_result = dot_result + {{48{dp8[15]}}, dp8};
            end
            2'd1: for (dl = 0; dl < 32; dl = dl + 1) begin : d16
                reg [31:0] dp16;
                if (mode_signed) dp16 = $signed({{16{tile_a[dl*16+15]}}, tile_a[dl*16 +: 16]})
                                      * $signed({{16{src_b_selected[dl*16+15]}}, src_b_selected[dl*16 +: 16]});
                else             dp16 = {16'd0, tile_a[dl*16 +: 16]} * {16'd0, src_b_selected[dl*16 +: 16]};
                dot_result = dot_result + {{32{dp16[31]}}, dp16};
            end
            2'd2: for (dl = 0; dl < 16; dl = dl + 1) begin : d32
                reg [63:0] dp32;
                if (mode_signed) dp32 = $signed({{32{tile_a[dl*32+31]}}, tile_a[dl*32 +: 32]})
                                      * $signed({{32{src_b_selected[dl*32+31]}}, src_b_selected[dl*32 +: 32]});
                else             dp32 = {32'd0, tile_a[dl*32 +: 32]} * {32'd0, src_b_selected[dl*32 +: 32]};
                dot_result = dot_result + dp32;
            end
            2'd3: for (dl = 0; dl < 8; dl = dl + 1) begin : d64
                dot_result = dot_result + tile_a[dl*64 +: 64] * src_b_selected[dl*64 +: 64];
            end
        endcase
    end

    // ========================================================================
    // TMUL.DOTACC — 4-way chunked dot product
    // ========================================================================
    reg [63:0] dotacc [0:3];
    integer dal;
    always @(*) begin
        dotacc[0] = 64'd0; dotacc[1] = 64'd0; dotacc[2] = 64'd0; dotacc[3] = 64'd0;
        case (mode_ew[1:0])
            2'd0: for (dal = 0; dal < 64; dal = dal + 1) begin : da8
                reg [15:0] dap8;
                if (mode_signed) dap8 = $signed({{8{tile_a[dal*8+7]}}, tile_a[dal*8 +: 8]})
                                      * $signed({{8{src_b_selected[dal*8+7]}}, src_b_selected[dal*8 +: 8]});
                else             dap8 = {8'd0, tile_a[dal*8 +: 8]} * {8'd0, src_b_selected[dal*8 +: 8]};
                dotacc[dal/16] = dotacc[dal/16] + {{48{dap8[15]}}, dap8};
            end
            2'd1: for (dal = 0; dal < 32; dal = dal + 1) begin : da16
                reg [31:0] dap16;
                if (mode_signed) dap16 = $signed({{16{tile_a[dal*16+15]}}, tile_a[dal*16 +: 16]})
                                       * $signed({{16{src_b_selected[dal*16+15]}}, src_b_selected[dal*16 +: 16]});
                else             dap16 = {16'd0, tile_a[dal*16 +: 16]} * {16'd0, src_b_selected[dal*16 +: 16]};
                dotacc[dal/8] = dotacc[dal/8] + {{32{dap16[31]}}, dap16};
            end
            2'd2: for (dal = 0; dal < 16; dal = dal + 1) begin : da32
                reg [63:0] dap32;
                if (mode_signed) dap32 = $signed({{32{tile_a[dal*32+31]}}, tile_a[dal*32 +: 32]})
                                       * $signed({{32{src_b_selected[dal*32+31]}}, src_b_selected[dal*32 +: 32]});
                else             dap32 = {32'd0, tile_a[dal*32 +: 32]} * {32'd0, src_b_selected[dal*32 +: 32]};
                dotacc[dal/4] = dotacc[dal/4] + dap32;
            end
            2'd3: for (dal = 0; dal < 8; dal = dal + 1) begin : da64
                dotacc[dal/2] = dotacc[dal/2] + tile_a[dal*64 +: 64] * src_b_selected[dal*64 +: 64];
            end
        endcase
    end

    // ========================================================================
    // TRED — reductions (all widths)
    // ========================================================================
    function [3:0] popcnt8;
        input [7:0] v;
        integer pi;
        begin
            popcnt8 = 0;
            for (pi = 0; pi < 8; pi = pi + 1)
                popcnt8 = popcnt8 + {3'd0, v[pi]};
        end
    endfunction

    reg [63:0] red_result, red_idx, red_val;
    integer rl;

    always @(*) begin
        red_result = 64'd0; red_idx = 64'd0; red_val = 64'd0;
        case (mode_ew[1:0])
        // ==== 8-bit ====
        2'd0: case (funct_reg)
            TRED_SUM: for (rl=0; rl<64; rl=rl+1)
                if (mode_signed) red_result = red_result + {{56{tile_a[rl*8+7]}}, tile_a[rl*8 +: 8]};
                else             red_result = red_result + {56'd0, tile_a[rl*8 +: 8]};
            TRED_MIN: begin
                red_result = mode_signed ? {{56{tile_a[7]}}, tile_a[7:0]} : {56'd0, tile_a[7:0]};
                for (rl=1; rl<64; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*8 +: 8]) < $signed(red_result[7:0]))
                            red_result = {{56{tile_a[rl*8+7]}}, tile_a[rl*8 +: 8]};
                    end else if (tile_a[rl*8 +: 8] < red_result[7:0])
                        red_result = {56'd0, tile_a[rl*8 +: 8]};
            end
            TRED_MAX: begin
                red_result = mode_signed ? {{56{tile_a[7]}}, tile_a[7:0]} : {56'd0, tile_a[7:0]};
                for (rl=1; rl<64; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*8 +: 8]) > $signed(red_result[7:0]))
                            red_result = {{56{tile_a[rl*8+7]}}, tile_a[rl*8 +: 8]};
                    end else if (tile_a[rl*8 +: 8] > red_result[7:0])
                        red_result = {56'd0, tile_a[rl*8 +: 8]};
            end
            TRED_POPC: for (rl=0; rl<64; rl=rl+1)
                red_result = red_result + {60'd0, popcnt8(tile_a[rl*8 +: 8])};
            TRED_L1: for (rl=0; rl<64; rl=rl+1)
                if (mode_signed && tile_a[rl*8+7])
                    red_result = red_result + {56'd0, (~tile_a[rl*8 +: 8]) + 8'd1};
                else
                    red_result = red_result + {56'd0, tile_a[rl*8 +: 8]};
            TRED_SUMSQ: for (rl=0; rl<64; rl=rl+1)
                red_result = red_result + ({56'd0, tile_a[rl*8 +: 8]} * {56'd0, tile_a[rl*8 +: 8]});
            TRED_MINIDX: begin
                red_val = mode_signed ? {{56{tile_a[7]}}, tile_a[7:0]} : {56'd0, tile_a[7:0]};
                red_idx = 64'd0;
                for (rl=1; rl<64; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*8 +: 8]) < $signed(red_val[7:0])) begin
                            red_val = {{56{tile_a[rl*8+7]}}, tile_a[rl*8 +: 8]};
                            red_idx = {32'd0, rl};
                        end
                    end else if (tile_a[rl*8 +: 8] < red_val[7:0]) begin
                        red_val = {56'd0, tile_a[rl*8 +: 8]};
                        red_idx = {32'd0, rl};
                    end
            end
            TRED_MAXIDX: begin
                red_val = mode_signed ? {{56{tile_a[7]}}, tile_a[7:0]} : {56'd0, tile_a[7:0]};
                red_idx = 64'd0;
                for (rl=1; rl<64; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*8 +: 8]) > $signed(red_val[7:0])) begin
                            red_val = {{56{tile_a[rl*8+7]}}, tile_a[rl*8 +: 8]};
                            red_idx = {32'd0, rl};
                        end
                    end else if (tile_a[rl*8 +: 8] > red_val[7:0]) begin
                        red_val = {56'd0, tile_a[rl*8 +: 8]};
                        red_idx = {32'd0, rl};
                    end
            end
            default: ;
        endcase
        // ==== 16-bit ====
        2'd1: case (funct_reg)
            TRED_SUM: for (rl=0; rl<32; rl=rl+1)
                if (mode_signed) red_result = red_result + {{48{tile_a[rl*16+15]}}, tile_a[rl*16 +: 16]};
                else             red_result = red_result + {48'd0, tile_a[rl*16 +: 16]};
            TRED_MIN: begin
                red_result = mode_signed ? {{48{tile_a[15]}}, tile_a[15:0]} : {48'd0, tile_a[15:0]};
                for (rl=1; rl<32; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*16 +: 16]) < $signed(red_result[15:0]))
                            red_result = {{48{tile_a[rl*16+15]}}, tile_a[rl*16 +: 16]};
                    end else if (tile_a[rl*16 +: 16] < red_result[15:0])
                        red_result = {48'd0, tile_a[rl*16 +: 16]};
            end
            TRED_MAX: begin
                red_result = mode_signed ? {{48{tile_a[15]}}, tile_a[15:0]} : {48'd0, tile_a[15:0]};
                for (rl=1; rl<32; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*16 +: 16]) > $signed(red_result[15:0]))
                            red_result = {{48{tile_a[rl*16+15]}}, tile_a[rl*16 +: 16]};
                    end else if (tile_a[rl*16 +: 16] > red_result[15:0])
                        red_result = {48'd0, tile_a[rl*16 +: 16]};
            end
            TRED_POPC: for (rl=0; rl<32; rl=rl+1) begin
                red_result = red_result + {60'd0, popcnt8(tile_a[rl*16 +: 8])}
                                        + {60'd0, popcnt8(tile_a[rl*16+8 +: 8])};
            end
            TRED_L1: for (rl=0; rl<32; rl=rl+1)
                if (mode_signed && tile_a[rl*16+15])
                    red_result = red_result + {48'd0, (~tile_a[rl*16 +: 16]) + 16'd1};
                else
                    red_result = red_result + {48'd0, tile_a[rl*16 +: 16]};
            TRED_SUMSQ: for (rl=0; rl<32; rl=rl+1)
                red_result = red_result + ({48'd0, tile_a[rl*16 +: 16]} * {48'd0, tile_a[rl*16 +: 16]});
            TRED_MINIDX: begin
                red_val = mode_signed ? {{48{tile_a[15]}}, tile_a[15:0]} : {48'd0, tile_a[15:0]};
                for (rl=1; rl<32; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*16 +: 16]) < $signed(red_val[15:0])) begin
                            red_val = {{48{tile_a[rl*16+15]}}, tile_a[rl*16 +: 16]};
                            red_idx = {32'd0, rl}; end
                    end else if (tile_a[rl*16 +: 16] < red_val[15:0]) begin
                        red_val = {48'd0, tile_a[rl*16 +: 16]};
                        red_idx = {32'd0, rl}; end
            end
            TRED_MAXIDX: begin
                red_val = mode_signed ? {{48{tile_a[15]}}, tile_a[15:0]} : {48'd0, tile_a[15:0]};
                for (rl=1; rl<32; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*16 +: 16]) > $signed(red_val[15:0])) begin
                            red_val = {{48{tile_a[rl*16+15]}}, tile_a[rl*16 +: 16]};
                            red_idx = {32'd0, rl}; end
                    end else if (tile_a[rl*16 +: 16] > red_val[15:0]) begin
                        red_val = {48'd0, tile_a[rl*16 +: 16]};
                        red_idx = {32'd0, rl}; end
            end
            default: ;
        endcase
        // ==== 32-bit ====
        2'd2: case (funct_reg)
            TRED_SUM: for (rl=0; rl<16; rl=rl+1)
                if (mode_signed) red_result = red_result + {{32{tile_a[rl*32+31]}}, tile_a[rl*32 +: 32]};
                else             red_result = red_result + {32'd0, tile_a[rl*32 +: 32]};
            TRED_MIN: begin
                red_result = mode_signed ? {{32{tile_a[31]}}, tile_a[31:0]} : {32'd0, tile_a[31:0]};
                for (rl=1; rl<16; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*32 +: 32]) < $signed(red_result[31:0]))
                            red_result = {{32{tile_a[rl*32+31]}}, tile_a[rl*32 +: 32]};
                    end else if (tile_a[rl*32 +: 32] < red_result[31:0])
                        red_result = {32'd0, tile_a[rl*32 +: 32]};
            end
            TRED_MAX: begin
                red_result = mode_signed ? {{32{tile_a[31]}}, tile_a[31:0]} : {32'd0, tile_a[31:0]};
                for (rl=1; rl<16; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*32 +: 32]) > $signed(red_result[31:0]))
                            red_result = {{32{tile_a[rl*32+31]}}, tile_a[rl*32 +: 32]};
                    end else if (tile_a[rl*32 +: 32] > red_result[31:0])
                        red_result = {32'd0, tile_a[rl*32 +: 32]};
            end
            TRED_POPC: for (rl=0; rl<16; rl=rl+1) begin : pc32
                integer pb;
                for (pb=0; pb<4; pb=pb+1)
                    red_result = red_result + {60'd0, popcnt8(tile_a[rl*32+pb*8 +: 8])};
            end
            TRED_L1: for (rl=0; rl<16; rl=rl+1)
                if (mode_signed && tile_a[rl*32+31])
                    red_result = red_result + {32'd0, (~tile_a[rl*32 +: 32]) + 32'd1};
                else
                    red_result = red_result + {32'd0, tile_a[rl*32 +: 32]};
            TRED_SUMSQ: for (rl=0; rl<16; rl=rl+1)
                red_result = red_result + ({32'd0, tile_a[rl*32 +: 32]} * {32'd0, tile_a[rl*32 +: 32]});
            TRED_MINIDX: begin
                red_val = mode_signed ? {{32{tile_a[31]}}, tile_a[31:0]} : {32'd0, tile_a[31:0]};
                for (rl=1; rl<16; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*32 +: 32]) < $signed(red_val[31:0])) begin
                            red_val = {{32{tile_a[rl*32+31]}}, tile_a[rl*32 +: 32]};
                            red_idx = {32'd0, rl}; end
                    end else if (tile_a[rl*32 +: 32] < red_val[31:0]) begin
                        red_val = {32'd0, tile_a[rl*32 +: 32]};
                        red_idx = {32'd0, rl}; end
            end
            TRED_MAXIDX: begin
                red_val = mode_signed ? {{32{tile_a[31]}}, tile_a[31:0]} : {32'd0, tile_a[31:0]};
                for (rl=1; rl<16; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*32 +: 32]) > $signed(red_val[31:0])) begin
                            red_val = {{32{tile_a[rl*32+31]}}, tile_a[rl*32 +: 32]};
                            red_idx = {32'd0, rl}; end
                    end else if (tile_a[rl*32 +: 32] > red_val[31:0]) begin
                        red_val = {32'd0, tile_a[rl*32 +: 32]};
                        red_idx = {32'd0, rl}; end
            end
            default: ;
        endcase
        // ==== 64-bit ====
        2'd3: case (funct_reg)
            TRED_SUM: for (rl=0; rl<8; rl=rl+1) red_result = red_result + tile_a[rl*64 +: 64];
            TRED_MIN: begin
                red_result = tile_a[63:0];
                for (rl=1; rl<8; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*64 +: 64]) < $signed(red_result)) red_result = tile_a[rl*64 +: 64];
                    end else if (tile_a[rl*64 +: 64] < red_result) red_result = tile_a[rl*64 +: 64];
            end
            TRED_MAX: begin
                red_result = tile_a[63:0];
                for (rl=1; rl<8; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*64 +: 64]) > $signed(red_result)) red_result = tile_a[rl*64 +: 64];
                    end else if (tile_a[rl*64 +: 64] > red_result) red_result = tile_a[rl*64 +: 64];
            end
            TRED_POPC: for (rl=0; rl<8; rl=rl+1) begin : pc64
                integer pb;
                for (pb=0; pb<8; pb=pb+1)
                    red_result = red_result + {60'd0, popcnt8(tile_a[rl*64+pb*8 +: 8])};
            end
            TRED_L1: for (rl=0; rl<8; rl=rl+1)
                if (mode_signed && tile_a[rl*64+63])
                    red_result = red_result + (~tile_a[rl*64 +: 64]) + 64'd1;
                else
                    red_result = red_result + tile_a[rl*64 +: 64];
            TRED_SUMSQ: for (rl=0; rl<8; rl=rl+1)
                red_result = red_result + tile_a[rl*64 +: 64] * tile_a[rl*64 +: 64];
            TRED_MINIDX: begin
                red_val = tile_a[63:0]; red_idx = 64'd0;
                for (rl=1; rl<8; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*64 +: 64]) < $signed(red_val)) begin
                            red_val = tile_a[rl*64 +: 64]; red_idx = {32'd0, rl}; end
                    end else if (tile_a[rl*64 +: 64] < red_val) begin
                        red_val = tile_a[rl*64 +: 64]; red_idx = {32'd0, rl}; end
            end
            TRED_MAXIDX: begin
                red_val = tile_a[63:0]; red_idx = 64'd0;
                for (rl=1; rl<8; rl=rl+1)
                    if (mode_signed) begin
                        if ($signed(tile_a[rl*64 +: 64]) > $signed(red_val)) begin
                            red_val = tile_a[rl*64 +: 64]; red_idx = {32'd0, rl}; end
                    end else if (tile_a[rl*64 +: 64] > red_val) begin
                        red_val = tile_a[rl*64 +: 64]; red_idx = {32'd0, rl}; end
            end
            default: ;
        endcase
        endcase
    end

    // ========================================================================
    // TSYS helpers
    // ========================================================================

    // Transpose 8×8 bytes
    reg [511:0] trans_result;
    integer tr, tc;
    always @(*) begin
        trans_result = 512'd0;
        for (tr = 0; tr < 8; tr = tr + 1)
            for (tc = 0; tc < 8; tc = tc + 1)
                trans_result[(tc*8+tr)*8 +: 8] = tile_a[(tr*8+tc)*8 +: 8];
    end

    // Shuffle (index-based lane permutation)
    reg [511:0] shuffle_result;
    integer sl;
    always @(*) begin
        shuffle_result = 512'd0;
        case (mode_ew[1:0])
            2'd0: for (sl=0; sl<64; sl=sl+1) begin : shuf8
                shuffle_result[sl*8 +: 8] = tile_a[src_b_selected[sl*8 +: 6]*8 +: 8];
            end
            2'd1: for (sl=0; sl<32; sl=sl+1) begin : shuf16
                shuffle_result[sl*16 +: 16] = tile_a[src_b_selected[sl*16 +: 5]*16 +: 16];
            end
            2'd2: for (sl=0; sl<16; sl=sl+1) begin : shuf32
                shuffle_result[sl*32 +: 32] = tile_a[src_b_selected[sl*32 +: 4]*32 +: 32];
            end
            2'd3: for (sl=0; sl<8; sl=sl+1) begin : shuf64
                shuffle_result[sl*64 +: 64] = tile_a[src_b_selected[sl*64 +: 3]*64 +: 64];
            end
        endcase
    end

    // Pack (narrow to half width)
    reg [511:0] pack_result;
    integer pl;
    always @(*) begin
        pack_result = 512'd0;
        case (mode_ew[1:0])
            2'd0: pack_result = tile_a; // can't narrow below 8-bit
            2'd1: for (pl=0; pl<32; pl=pl+1) begin : pk16
                reg [15:0] pv;
                pv = tile_a[pl*16 +: 16];
                if (mode_saturate) begin
                    if (mode_signed) begin
                        if ($signed(pv) > 16'sd127) pack_result[pl*8 +: 8] = 8'h7F;
                        else if ($signed(pv) < -16'sd128) pack_result[pl*8 +: 8] = 8'h80;
                        else pack_result[pl*8 +: 8] = pv[7:0];
                    end else begin
                        if (pv > 16'd255) pack_result[pl*8 +: 8] = 8'hFF;
                        else pack_result[pl*8 +: 8] = pv[7:0];
                    end
                end else pack_result[pl*8 +: 8] = pv[7:0];
            end
            2'd2: for (pl=0; pl<16; pl=pl+1) begin : pk32
                reg [31:0] pv32;
                pv32 = tile_a[pl*32 +: 32];
                if (mode_saturate) begin
                    if (mode_signed) begin
                        if ($signed(pv32) > 32'sd32767) pack_result[pl*16 +: 16] = 16'h7FFF;
                        else if ($signed(pv32) < -32'sd32768) pack_result[pl*16 +: 16] = 16'h8000;
                        else pack_result[pl*16 +: 16] = pv32[15:0];
                    end else begin
                        if (pv32 > 32'd65535) pack_result[pl*16 +: 16] = 16'hFFFF;
                        else pack_result[pl*16 +: 16] = pv32[15:0];
                    end
                end else pack_result[pl*16 +: 16] = pv32[15:0];
            end
            2'd3: for (pl=0; pl<8; pl=pl+1) begin : pk64
                reg [63:0] pv64;
                pv64 = tile_a[pl*64 +: 64];
                if (mode_saturate) begin
                    if (mode_signed) begin
                        if ($signed(pv64) > 64'sh7FFFFFFF) pack_result[pl*32 +: 32] = 32'h7FFFFFFF;
                        else if ($signed(pv64) < -64'sh80000000) pack_result[pl*32 +: 32] = 32'h80000000;
                        else pack_result[pl*32 +: 32] = pv64[31:0];
                    end else begin
                        if (pv64 > 64'hFFFFFFFF) pack_result[pl*32 +: 32] = 32'hFFFFFFFF;
                        else pack_result[pl*32 +: 32] = pv64[31:0];
                    end
                end else pack_result[pl*32 +: 32] = pv64[31:0];
            end
        endcase
    end

    // Unpack (widen to double width)
    reg [511:0] unpack_result;
    integer ul;
    always @(*) begin
        unpack_result = 512'd0;
        case (mode_ew[1:0])
            2'd0: for (ul=0; ul<32; ul=ul+1) begin : up8
                if (mode_signed) unpack_result[ul*16 +: 16] = {{8{tile_a[ul*8+7]}}, tile_a[ul*8 +: 8]};
                else             unpack_result[ul*16 +: 16] = {8'd0, tile_a[ul*8 +: 8]};
            end
            2'd1: for (ul=0; ul<16; ul=ul+1) begin : up16
                if (mode_signed) unpack_result[ul*32 +: 32] = {{16{tile_a[ul*16+15]}}, tile_a[ul*16 +: 16]};
                else             unpack_result[ul*32 +: 32] = {16'd0, tile_a[ul*16 +: 16]};
            end
            2'd2: for (ul=0; ul<8; ul=ul+1) begin : up32
                if (mode_signed) unpack_result[ul*64 +: 64] = {{32{tile_a[ul*32+31]}}, tile_a[ul*32 +: 32]};
                else             unpack_result[ul*64 +: 64] = {32'd0, tile_a[ul*32 +: 32]};
            end
            2'd3: unpack_result = tile_a; // can't widen 64→128
        endcase
    end

    // ========================================================================
    // EXT.8 Extended TALU — VSHR, VSHL, VSEL, VCLZ
    // ========================================================================
    function [7:0] clz8; input [7:0] v; begin
        casez(v)
            8'b1???????: clz8=0; 8'b01??????: clz8=1; 8'b001?????: clz8=2;
            8'b0001????: clz8=3; 8'b00001???: clz8=4; 8'b000001??: clz8=5;
            8'b0000001?: clz8=6; 8'b00000001: clz8=7; default: clz8=8;
        endcase
    end endfunction

    function [15:0] clz16; input [15:0] v; begin
        if (v[15:8] != 0) clz16 = {8'd0, clz8(v[15:8])};
        else               clz16 = {8'd0, clz8(v[7:0])} + 16'd8;
    end endfunction

    function [31:0] clz32; input [31:0] v; begin
        if (v[31:16] != 0) clz32 = {16'd0, clz16(v[31:16])};
        else                clz32 = 32'd16 + {16'd0, clz16(v[15:0])};
    end endfunction

    function [63:0] clz64; input [63:0] v; begin
        if (v[63:32] != 0) clz64 = {32'd0, clz32(v[63:32])};
        else                clz64 = 64'd32 + {32'd0, clz32(v[31:0])};
    end endfunction

    reg [511:0] ext_talu_result;
    integer el;
    always @(*) begin
        ext_talu_result = 512'd0;
        case (mode_ew[1:0])
            2'd0: for (el=0; el<64; el=el+1) begin : ex8
                reg [7:0] ea8, eb8;
                reg [2:0] sh;
                ea8 = tile_a[el*8 +: 8]; eb8 = src_b_selected[el*8 +: 8]; sh = eb8[2:0];
                case (funct_reg)
                    3'd0: if (mode_signed) ext_talu_result[el*8 +: 8] = $signed(ea8) >>> sh;
                          else             ext_talu_result[el*8 +: 8] = ea8 >> sh;
                    3'd1: ext_talu_result[el*8 +: 8] = ea8 << sh;
                    3'd2: ext_talu_result[el*8 +: 8] = ea8; // VSEL placeholder
                    3'd3: ext_talu_result[el*8 +: 8] = clz8(ea8);
                    default: ;
                endcase
            end
            2'd1: for (el=0; el<32; el=el+1) begin : ex16
                reg [15:0] ea16, eb16; reg [3:0] sh16;
                ea16 = tile_a[el*16 +: 16]; eb16 = src_b_selected[el*16 +: 16]; sh16 = eb16[3:0];
                case (funct_reg)
                    3'd0: if (mode_signed) ext_talu_result[el*16 +: 16] = $signed(ea16) >>> sh16;
                          else             ext_talu_result[el*16 +: 16] = ea16 >> sh16;
                    3'd1: ext_talu_result[el*16 +: 16] = ea16 << sh16;
                    3'd2: ext_talu_result[el*16 +: 16] = ea16;
                    3'd3: ext_talu_result[el*16 +: 16] = clz16(ea16);
                    default: ;
                endcase
            end
            2'd2: for (el=0; el<16; el=el+1) begin : ex32
                reg [31:0] ea32, eb32; reg [4:0] sh32;
                ea32 = tile_a[el*32 +: 32]; eb32 = src_b_selected[el*32 +: 32]; sh32 = eb32[4:0];
                case (funct_reg)
                    3'd0: if (mode_signed) ext_talu_result[el*32 +: 32] = $signed(ea32) >>> sh32;
                          else             ext_talu_result[el*32 +: 32] = ea32 >> sh32;
                    3'd1: ext_talu_result[el*32 +: 32] = ea32 << sh32;
                    3'd2: ext_talu_result[el*32 +: 32] = ea32;
                    3'd3: ext_talu_result[el*32 +: 32] = clz32(ea32);
                    default: ;
                endcase
            end
            2'd3: for (el=0; el<8; el=el+1) begin : ex64
                reg [63:0] ea64, eb64; reg [5:0] sh64;
                ea64 = tile_a[el*64 +: 64]; eb64 = src_b_selected[el*64 +: 64]; sh64 = eb64[5:0];
                case (funct_reg)
                    3'd0: if (mode_signed) ext_talu_result[el*64 +: 64] = $signed(ea64) >>> sh64;
                          else             ext_talu_result[el*64 +: 64] = ea64 >> sh64;
                    3'd1: ext_talu_result[el*64 +: 64] = ea64 << sh64;
                    3'd2: ext_talu_result[el*64 +: 64] = ea64;
                    3'd3: ext_talu_result[el*64 +: 64] = clz64(ea64);
                    default: ;
                endcase
            end
        endcase
    end

    // ========================================================================
    // Main state machine
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state         <= S_IDLE;
            mex_done      <= 1'b0;
            mex_busy      <= 1'b0;
            tile_req      <= 1'b0;
            tile_wen      <= 1'b0;
            tile_a        <= 512'd0;
            tile_b        <= 512'd0;
            tile_c        <= 512'd0;
            result        <= 512'd0;
            result2       <= 512'd0;
            ext_tile_req  <= 1'b0;
            needs_load_c  <= 1'b0;
        end else begin
            mex_done     <= 1'b0;
            tile_req     <= 1'b0;
            tile_wen     <= 1'b0;
            ext_tile_req <= 1'b0;

            case (state)
            S_IDLE: begin
                mex_busy <= 1'b0;
                if (mex_valid) begin
                    op_reg        <= mex_op;
                    funct_reg     <= mex_funct;
                    ss_reg        <= mex_ss;
                    gpr_val_reg   <= mex_gpr_val;
                    imm8_reg      <= mex_imm8;
                    ext_mod_reg   <= mex_ext_mod;
                    ext_active_reg<= mex_ext_active;
                    mex_busy      <= 1'b1;
                    needs_load_c  <= 1'b0;

                    // TSYS.ZERO — write zeros
                    if (mex_op == MEX_TSYS && mex_funct == TSYS_ZERO &&
                        !(mex_ext_active && mex_ext_mod == 4'd8)) begin
                        tile_req  <= 1'b1;
                        tile_addr <= tdst[19:0];
                        tile_wen  <= 1'b1;
                        tile_wdata<= 512'd0;
                        state     <= S_DONE;
                    end
                    // TSYS.TRANS — read TDST
                    else if (mex_op == MEX_TSYS && mex_funct == TSYS_TRANS &&
                             !(mex_ext_active && mex_ext_mod == 4'd8)) begin
                        tile_req  <= 1'b1;
                        tile_addr <= tdst[19:0];
                        state     <= S_LOAD_A;
                    end
                    // TSYS.LOADC — cursor address
                    else if (mex_op == MEX_TSYS && mex_funct == TSYS_LOADC &&
                             !(mex_ext_active && mex_ext_mod == 4'd8)) begin
                        tile_req  <= 1'b1;
                        tile_addr <= (tile_row[19:0] * tile_stride[19:0] + tile_col[19:0]) * 20'd64;
                        state     <= S_LOAD_A;
                    end
                    // TRED — load TSRC0 only
                    else if (mex_op == MEX_TRED) begin
                        if (src0_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= tsrc0[19:0];
                            state     <= S_LOAD_A;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= tsrc0;
                            state         <= S_EXT_LOAD_A;
                        end
                    end
                    // TMUL MAC/FMA — need existing TDST
                    else if (mex_op == MEX_TMUL &&
                             (mex_funct == TMUL_MAC || mex_funct == TMUL_FMA)) begin
                        needs_load_c <= 1'b1;
                        if (src0_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= tsrc0[19:0];
                            state     <= S_LOAD_A;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= tsrc0;
                            state         <= S_EXT_LOAD_A;
                        end
                    end
                    // EXT.8 TSYS — TODO: LOAD2D/STORE2D (NOP for now)
                    else if (mex_op == MEX_TSYS && mex_ext_active && mex_ext_mod == 4'd8) begin
                        state <= S_DONE;
                    end
                    // Everything else: load TSRC0
                    else begin
                        if (src0_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= tsrc0[19:0];
                            state     <= S_LOAD_A;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= tsrc0;
                            state         <= S_EXT_LOAD_A;
                        end
                    end
                end
            end

            S_LOAD_A: begin
                if (tile_ack) begin
                    tile_a <= tile_rdata;
                    if (op_reg == MEX_TRED) begin
                        state <= S_REDUCE;
                    end
                    else if (op_reg == MEX_TSYS) begin
                        // SHUFFLE needs index tile from TSRC1
                        if (funct_reg == TSYS_SHUFFLE) begin
                            if (src1_internal) begin
                                tile_req  <= 1'b1;
                                tile_addr <= tsrc1[19:0];
                                state     <= S_LOAD_B;
                            end else begin
                                ext_tile_req  <= 1'b1;
                                ext_tile_addr <= tsrc1;
                                state         <= S_EXT_LOAD_B;
                            end
                        end else
                            state <= S_COMPUTE;
                    end
                    else if (ss_reg == 2'd0) begin
                        if (src1_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= tsrc1[19:0];
                            state     <= S_LOAD_B;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= tsrc1;
                            state         <= S_EXT_LOAD_B;
                        end
                    end
                    else begin
                        if (needs_load_c) begin
                            tile_req  <= 1'b1;
                            tile_addr <= tdst[19:0];
                            state     <= S_LOAD_C;
                        end else
                            state <= S_COMPUTE;
                    end
                end
            end

            S_LOAD_B: begin
                if (tile_ack) begin
                    tile_b <= tile_rdata;
                    if (needs_load_c) begin
                        tile_req  <= 1'b1;
                        tile_addr <= tdst[19:0];
                        state     <= S_LOAD_C;
                    end else
                        state <= S_COMPUTE;
                end
            end

            S_LOAD_C: begin
                if (tile_ack) begin
                    tile_c <= tile_rdata;
                    state  <= S_COMPUTE;
                end
            end

            S_COMPUTE: begin
                // Select result
                if (ext_active_reg && ext_mod_reg == 4'd8 && op_reg == MEX_TALU)
                    result <= ext_talu_result;
                else if (op_reg == MEX_TALU)
                    result <= alu_result_muxed;
                else if (op_reg == MEX_TMUL) begin
                    case (funct_reg)
                        TMUL_MUL: result <= mul_result;
                        TMUL_WMUL: begin result <= wmul_lo; result2 <= wmul_hi; end
                        TMUL_MAC: result <= mac_result;
                        TMUL_FMA: result <= mac_result;
                        default:  result <= mul_result;
                    endcase
                end
                else if (op_reg == MEX_TSYS) begin
                    case (funct_reg)
                        TSYS_TRANS:   result <= trans_result;
                        TSYS_MOVBANK: result <= tile_a;
                        TSYS_LOADC:   result <= tile_a;
                        TSYS_PACK:    result <= pack_result;
                        TSYS_UNPACK:  result <= unpack_result;
                        TSYS_SHUFFLE: result <= shuffle_result;
                        TSYS_RROT:    result <= tile_a; // TODO: full RROT
                        default:      result <= 512'd0;
                    endcase
                end

                // DOT/DOTACC → accumulator, then done (no tile store)
                if (op_reg == MEX_TMUL && funct_reg == TMUL_DOT) begin
                    if (tctrl[1]) begin
                        acc[0] <= dot_result; acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                    end else if (tctrl[0])
                        acc[0] <= acc[0] + dot_result;
                    else begin
                        acc[0] <= dot_result; acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                    end
                    state <= S_DONE;
                end
                else if (op_reg == MEX_TMUL && funct_reg == TMUL_DOTACC) begin
                    if (tctrl[1]) begin
                        acc[0] <= dotacc[0]; acc[1] <= dotacc[1]; acc[2] <= dotacc[2]; acc[3] <= dotacc[3];
                    end else if (tctrl[0]) begin
                        acc[0] <= acc[0]+dotacc[0]; acc[1] <= acc[1]+dotacc[1];
                        acc[2] <= acc[2]+dotacc[2]; acc[3] <= acc[3]+dotacc[3];
                    end else begin
                        acc[0] <= dotacc[0]; acc[1] <= dotacc[1]; acc[2] <= dotacc[2]; acc[3] <= dotacc[3];
                    end
                    state <= S_DONE;
                end
                else begin
                    state <= S_STORE;
                end
            end

            S_STORE: begin
                if (dst_internal) begin
                    tile_req   <= 1'b1;
                    tile_addr  <= tdst[19:0];
                    tile_wen   <= 1'b1;
                    tile_wdata <= result;
                    if (op_reg == MEX_TMUL && funct_reg == TMUL_WMUL)
                        state <= S_STORE2;
                    else
                        state <= S_DONE;
                end else begin
                    ext_tile_req   <= 1'b1;
                    ext_tile_addr  <= tdst;
                    ext_tile_wen   <= 1'b1;
                    ext_tile_wdata <= result;
                    state          <= S_EXT_STORE;
                end
            end

            S_STORE2: begin
                if (tile_ack) begin
                    tile_req   <= 1'b1;
                    tile_addr  <= tdst[19:0] + 20'd64;
                    tile_wen   <= 1'b1;
                    tile_wdata <= result2;
                    state      <= S_DONE;
                end
            end

            S_REDUCE: begin
                if (funct_reg == TRED_MINIDX || funct_reg == TRED_MAXIDX) begin
                    if (tctrl[1]) begin
                        acc[0] <= red_idx; acc[1] <= red_val; acc[2] <= 64'd0; acc[3] <= 64'd0;
                    end else if (tctrl[0]) begin
                        if (funct_reg == TRED_MINIDX) begin
                            if (mode_signed) begin
                                if ($signed(red_val) < $signed(acc[1])) begin acc[0] <= red_idx; acc[1] <= red_val; end
                            end else begin
                                if (red_val < acc[1]) begin acc[0] <= red_idx; acc[1] <= red_val; end
                            end
                        end else begin
                            if (mode_signed) begin
                                if ($signed(red_val) > $signed(acc[1])) begin acc[0] <= red_idx; acc[1] <= red_val; end
                            end else begin
                                if (red_val > acc[1]) begin acc[0] <= red_idx; acc[1] <= red_val; end
                            end
                        end
                    end else begin
                        acc[0] <= red_idx; acc[1] <= red_val;
                    end
                end else begin
                    if (tctrl[1]) begin
                        acc[0] <= red_result; acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                    end else if (tctrl[0])
                        acc[0] <= acc[0] + red_result;
                    else
                        acc[0] <= red_result;
                end
                state <= S_DONE;
            end

            // External memory paths
            S_EXT_LOAD_A: begin
                if (ext_tile_ack) begin
                    tile_a <= ext_tile_rdata;
                    if (op_reg == MEX_TRED) state <= S_REDUCE;
                    else if (op_reg == MEX_TSYS && funct_reg == TSYS_SHUFFLE) begin
                        if (src1_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= tsrc1[19:0];
                            state     <= S_LOAD_B;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= tsrc1;
                            state         <= S_EXT_LOAD_B;
                        end
                    end
                    else if (ss_reg == 2'd0) begin
                        if (src1_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= tsrc1[19:0];
                            state     <= S_LOAD_B;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= tsrc1;
                            state         <= S_EXT_LOAD_B;
                        end
                    end else begin
                        if (needs_load_c) begin
                            tile_req <= 1'b1; tile_addr <= tdst[19:0]; state <= S_LOAD_C;
                        end else state <= S_COMPUTE;
                    end
                end
            end

            S_EXT_LOAD_B: begin
                if (ext_tile_ack) begin
                    tile_b <= ext_tile_rdata;
                    if (needs_load_c) begin
                        tile_req <= 1'b1; tile_addr <= tdst[19:0]; state <= S_LOAD_C;
                    end else state <= S_COMPUTE;
                end
            end

            S_EXT_STORE: if (ext_tile_ack) state <= S_DONE;

            S_DONE: begin
                mex_done <= 1'b1;
                mex_busy <= 1'b0;
                state    <= S_IDLE;
            end
            default: state <= S_IDLE;
            endcase
        end
    end

endmodule
