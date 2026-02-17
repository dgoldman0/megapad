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

`include "mp64_pkg.vh"

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
    output reg  [31:0] tile_addr,
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
    reg [63:0] tstride_r;    // row stride in bytes (for LOAD2D/STORE2D)
    reg [63:0] tstride_c;    // column stride in bytes (reserved)
    reg [63:0] ttile_h;      // tile height (rows, 1-8)
    reg [63:0] ttile_w;      // tile width (bytes per row, 1-64)

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
            CSR_TSTRIDE_R: csr_rdata = tstride_r;
            CSR_TSTRIDE_C: csr_rdata = tstride_c;
            CSR_TTILE_H:   csr_rdata = ttile_h;
            CSR_TTILE_W:   csr_rdata = ttile_w;
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
            tstride_r   <= 64'd0;
            tstride_c   <= 64'd0;
            ttile_h     <= 64'd8;   // default 8 rows
            ttile_w     <= 64'd8;   // default 8 bytes per row
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
                CSR_TSTRIDE_R: tstride_r <= csr_wdata;
                CSR_TSTRIDE_C: tstride_c <= csr_wdata;
                CSR_TTILE_H:   ttile_h   <= csr_wdata;
                CSR_TTILE_W:   ttile_w   <= csr_wdata;
                default: ;  // no-op for unrecognized CSR
            endcase
        end
    end

    // ========================================================================
    // Address classification — internal = Bank 0 OR HBW banks
    // ========================================================================
    wire src0_bank0 = (tsrc0[63:20] == 44'd0);
    wire src0_hbw   = (tsrc0[63:32] == 32'd0) && (tsrc0[31:20] >= 12'hFFD);
    wire src0_internal = src0_bank0 || src0_hbw;

    wire src1_bank0 = (tsrc1[63:20] == 44'd0);
    wire src1_hbw   = (tsrc1[63:32] == 32'd0) && (tsrc1[31:20] >= 12'hFFD);
    wire src1_internal = src1_bank0 || src1_hbw;

    wire dst_bank0 = (tdst[63:20] == 44'd0);
    wire dst_hbw   = (tdst[63:32] == 32'd0) && (tdst[31:20] >= 12'hFFD);
    wire dst_internal  = dst_bank0 || dst_hbw;

    // ========================================================================
    // Mode decode
    // ========================================================================
    wire [2:0] mode_ew       = tmode[2:0];
    wire       mode_signed   = tmode[4];
    wire       mode_saturate = tmode[5];
    wire       mode_rounding = tmode[6];
    wire       mode_fp       = mode_ew[2];          // EW >= 4 → FP mode
    wire       mode_bf16     = (mode_ew == TMODE_BF16);  // 0 = FP16, 1 = BF16

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
    localparam S_LOAD2D_REQ = 4'd12;   // LOAD2D: issue tile read for row
    localparam S_LOAD2D_WAIT= 4'd13;   // LOAD2D: wait for tile ack
    localparam S_STORE2D_REQ= 4'd14;   // STORE2D: issue tile read (RMW)
    localparam S_STORE2D_WAIT=4'd15;   // STORE2D: wait for read ack, then write

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

    // LOAD2D / STORE2D FSM registers
    reg [63:0]  ld2d_base;       // starting memory address
    reg [63:0]  ld2d_stride;     // effective row stride in bytes
    reg [3:0]   ld2d_row;        // current row counter
    reg [6:0]   ld2d_off;        // byte offset in result tile
    reg [3:0]   ld2d_h;          // number of rows
    reg [6:0]   ld2d_w;          // bytes per row
    reg [63:0]  ld2d_row_addr;   // computed row address (cached)

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
        if (mode_fp)
            alu_result_muxed = fp_alu_result;
        else case (mode_ew[1:0])
            2'd0: alu_result_muxed = alu_result_8;
            2'd1: alu_result_muxed = alu_result_16;
            2'd2: alu_result_muxed = alu_result_32;
            2'd3: alu_result_muxed = alu_result_64;
        endcase
    end

    // ========================================================================
    // FP16/BF16 Lane ALU — 32 lanes × 16-bit (via mp64_fp16_alu)
    // ========================================================================
    reg [511:0] fp_alu_result;
    wire [15:0] fp_alu_out [0:31];
    wire        fp_nan_a   [0:31];
    wire        fp_nan_b   [0:31];

    genvar fpl;
    generate
        for (fpl = 0; fpl < 32; fpl = fpl + 1) begin : fp_alu_lanes
            mp64_fp16_alu u_fp_alu (
                .is_bf16  (mode_bf16),
                .a        (tile_a[fpl*16 +: 16]),
                .b        (src_b_selected[fpl*16 +: 16]),
                .op_add   (funct_reg == TALU_ADD),
                .op_sub   (funct_reg == TALU_SUB),
                .op_mul   (1'b0),
                .op_min   (funct_reg == TALU_MIN),
                .op_max   (funct_reg == TALU_MAX),
                .op_abs   (funct_reg == TALU_ABS),
                .op_cmp_lt(1'b0),
                .op_cmp_gt(1'b0),
                .result   (fp_alu_out[fpl]),
                .is_nan_a (fp_nan_a[fpl]),
                .is_nan_b (fp_nan_b[fpl])
            );
        end
    endgenerate

    // FP ALU result assembly (handles bitwise ops directly)
    integer fp_alu_i;
    always @(*) begin
        fp_alu_result = 512'd0;
        for (fp_alu_i = 0; fp_alu_i < 32; fp_alu_i = fp_alu_i + 1) begin
            case (funct_reg)
                TALU_ADD, TALU_SUB, TALU_MIN, TALU_MAX, TALU_ABS:
                    fp_alu_result[fp_alu_i*16 +: 16] = fp_alu_out[fp_alu_i];
                // Bitwise ops: operate on raw bits (no FP decode needed)
                TALU_AND:
                    fp_alu_result[fp_alu_i*16 +: 16] = tile_a[fp_alu_i*16 +: 16] & src_b_selected[fp_alu_i*16 +: 16];
                TALU_OR:
                    fp_alu_result[fp_alu_i*16 +: 16] = tile_a[fp_alu_i*16 +: 16] | src_b_selected[fp_alu_i*16 +: 16];
                TALU_XOR:
                    fp_alu_result[fp_alu_i*16 +: 16] = tile_a[fp_alu_i*16 +: 16] ^ src_b_selected[fp_alu_i*16 +: 16];
                default:
                    fp_alu_result[fp_alu_i*16 +: 16] = 16'd0;
            endcase
        end
    end

    // ========================================================================
    // FP16/BF16 TMUL.MUL — 32 lanes × 16-bit multiply
    // ========================================================================
    reg  [511:0] fp_mul_result;
    wire [15:0]  fp_mul_out [0:31];

    generate
        for (fpl = 0; fpl < 32; fpl = fpl + 1) begin : fp_mul_lanes
            mp64_fp16_alu u_fp_mul (
                .is_bf16  (mode_bf16),
                .a        (tile_a[fpl*16 +: 16]),
                .b        (src_b_selected[fpl*16 +: 16]),
                .op_add   (1'b0),
                .op_sub   (1'b0),
                .op_mul   (1'b1),
                .op_min   (1'b0),
                .op_max   (1'b0),
                .op_abs   (1'b0),
                .op_cmp_lt(1'b0),
                .op_cmp_gt(1'b0),
                .result   (fp_mul_out[fpl]),
                .is_nan_a (),
                .is_nan_b ()
            );
        end
    endgenerate

    integer fp_ml;
    always @(*) begin
        fp_mul_result = 512'd0;
        for (fp_ml = 0; fp_ml < 32; fp_ml = fp_ml + 1)
            fp_mul_result[fp_ml*16 +: 16] = fp_mul_out[fp_ml];
    end

    // ========================================================================
    // FP16/BF16 TMUL.MAC / FMA — fp_mul × lanes + fp_add with tile_c
    // ========================================================================
    reg  [511:0] fp_mac_result;
    wire [15:0]  fp_mac_out [0:31];

    generate
        for (fpl = 0; fpl < 32; fpl = fpl + 1) begin : fp_mac_lanes
            wire [15:0] fp_prod;
            // Multiply a × b
            mp64_fp16_alu u_fp_mac_mul (
                .is_bf16  (mode_bf16),
                .a        (tile_a[fpl*16 +: 16]),
                .b        (src_b_selected[fpl*16 +: 16]),
                .op_add   (1'b0), .op_sub(1'b0),
                .op_mul   (1'b1),
                .op_min   (1'b0), .op_max(1'b0), .op_abs(1'b0),
                .op_cmp_lt(1'b0), .op_cmp_gt(1'b0),
                .result   (fp_prod),
                .is_nan_a (), .is_nan_b ()
            );
            // Add product + tile_c
            mp64_fp16_alu u_fp_mac_add (
                .is_bf16  (mode_bf16),
                .a        (tile_c[fpl*16 +: 16]),
                .b        (fp_prod),
                .op_add   (1'b1), .op_sub(1'b0),
                .op_mul   (1'b0),
                .op_min   (1'b0), .op_max(1'b0), .op_abs(1'b0),
                .op_cmp_lt(1'b0), .op_cmp_gt(1'b0),
                .result   (fp_mac_out[fpl]),
                .is_nan_a (), .is_nan_b ()
            );
        end
    endgenerate

    integer fp_mac_i;
    always @(*) begin
        fp_mac_result = 512'd0;
        for (fp_mac_i = 0; fp_mac_i < 32; fp_mac_i = fp_mac_i + 1)
            fp_mac_result[fp_mac_i*16 +: 16] = fp_mac_out[fp_mac_i];
    end

    // ========================================================================
    // FP16/BF16 TMUL.WMUL — widening multiply: FP16 × FP16 → FP32
    // ========================================================================
    reg  [511:0] fp_wmul_lo, fp_wmul_hi;
    wire [31:0]  fp_wmul_fp32 [0:31];

    generate
        for (fpl = 0; fpl < 32; fpl = fpl + 1) begin : fp_wmul_lanes
            // Convert inputs to FP32
            wire [31:0] a_fp32, b_fp32;
            mp64_fp16_to_fp32 u_cvt_a (
                .is_bf16(mode_bf16), .fp16_in(tile_a[fpl*16 +: 16]), .fp32_out(a_fp32)
            );
            mp64_fp16_to_fp32 u_cvt_b (
                .is_bf16(mode_bf16), .fp16_in(src_b_selected[fpl*16 +: 16]), .fp32_out(b_fp32)
            );
            // Multiply as FP32 (using product of widened values)
            // Simplified: use FP16 mul then widen result
            // Actually: widen first, then multiply. We need FP32 mul which we
            // don't have, so we do: fp16_mul → fp16 result, then widen to fp32.
            // This matches emulator: _fp_decode(a) * _fp_decode(b) → fp32_to_bits
            // The emulator does the full-precision multiply in float, so we widen
            // inputs then use software mul. For RTL, we widen then just store.
            // Actually the emulator widening is: fa * fb computed at f64, stored as fp32.
            // For RTL correctness, let's compute the FP16 product then widen to FP32.
            wire [15:0] fp_prod_w;
            mp64_fp16_alu u_fp_wmul (
                .is_bf16  (mode_bf16),
                .a        (tile_a[fpl*16 +: 16]),
                .b        (src_b_selected[fpl*16 +: 16]),
                .op_add(1'b0), .op_sub(1'b0), .op_mul(1'b1),
                .op_min(1'b0), .op_max(1'b0), .op_abs(1'b0),
                .op_cmp_lt(1'b0), .op_cmp_gt(1'b0),
                .result(fp_prod_w),
                .is_nan_a(), .is_nan_b()
            );
            mp64_fp16_to_fp32 u_widen_prod (
                .is_bf16(mode_bf16), .fp16_in(fp_prod_w), .fp32_out(fp_wmul_fp32[fpl])
            );
        end
    endgenerate

    integer fp_wl;
    always @(*) begin
        fp_wmul_lo = 512'd0;
        fp_wmul_hi = 512'd0;
        for (fp_wl = 0; fp_wl < 32; fp_wl = fp_wl + 1) begin
            if (fp_wl < 16)
                fp_wmul_lo[fp_wl*32 +: 32] = fp_wmul_fp32[fp_wl];
            else
                fp_wmul_hi[(fp_wl-16)*32 +: 32] = fp_wmul_fp32[fp_wl];
        end
    end

    // ========================================================================
    // FP16/BF16 TMUL.DOT — dot product → FP32 accumulator
    // ========================================================================
    // 32 FP16 multiplies → widen to FP32 → adder tree → FP32 result
    reg [31:0] fp_dot_result;

    // Use FP32 products from wmul path (already computed above)
    // Adder tree: 32 → 16 → 8 → 4 → 2 → 1
    wire [31:0] fp_dot_l1 [0:15];
    wire [31:0] fp_dot_l2 [0:7];
    wire [31:0] fp_dot_l3 [0:3];
    wire [31:0] fp_dot_l4 [0:1];
    wire [31:0] fp_dot_l5;

    generate
        // Level 1: 32→16
        for (fpl = 0; fpl < 16; fpl = fpl + 1) begin : dot_l1
            mp64_fp32_adder u_add_l1 (
                .a(fp_wmul_fp32[fpl*2]), .b(fp_wmul_fp32[fpl*2+1]),
                .result(fp_dot_l1[fpl])
            );
        end
        // Level 2: 16→8
        for (fpl = 0; fpl < 8; fpl = fpl + 1) begin : dot_l2
            mp64_fp32_adder u_add_l2 (
                .a(fp_dot_l1[fpl*2]), .b(fp_dot_l1[fpl*2+1]),
                .result(fp_dot_l2[fpl])
            );
        end
        // Level 3: 8→4
        for (fpl = 0; fpl < 4; fpl = fpl + 1) begin : dot_l3
            mp64_fp32_adder u_add_l3 (
                .a(fp_dot_l2[fpl*2]), .b(fp_dot_l2[fpl*2+1]),
                .result(fp_dot_l3[fpl])
            );
        end
        // Level 4: 4→2
        for (fpl = 0; fpl < 2; fpl = fpl + 1) begin : dot_l4
            mp64_fp32_adder u_add_l4 (
                .a(fp_dot_l3[fpl*2]), .b(fp_dot_l3[fpl*2+1]),
                .result(fp_dot_l4[fpl])
            );
        end
        // Level 5: 2→1
        mp64_fp32_adder u_add_l5 (
            .a(fp_dot_l4[0]), .b(fp_dot_l4[1]),
            .result(fp_dot_l5)
        );
    endgenerate

    always @(*) fp_dot_result = fp_dot_l5;

    // ========================================================================
    // FP16/BF16 TMUL.DOTACC — 4-way chunked dot product → FP32 accumulators
    // ========================================================================
    // 32 lanes / 4 chunks = 8 lanes per chunk
    // Each chunk: 8 FP16 muls → FP32 → adder tree(8→1) → acc[k]
    reg [31:0] fp_dotacc_result [0:3];

    generate
        genvar chunk;
        for (chunk = 0; chunk < 4; chunk = chunk + 1) begin : fp_dotacc_chunks
            wire [31:0] chunk_l1 [0:3];
            wire [31:0] chunk_l2 [0:1];
            wire [31:0] chunk_l3;

            // Level 1: 8→4
            for (fpl = 0; fpl < 4; fpl = fpl + 1) begin : dac_l1
                mp64_fp32_adder u_dac_l1 (
                    .a(fp_wmul_fp32[chunk*8 + fpl*2]),
                    .b(fp_wmul_fp32[chunk*8 + fpl*2 + 1]),
                    .result(chunk_l1[fpl])
                );
            end
            // Level 2: 4→2
            for (fpl = 0; fpl < 2; fpl = fpl + 1) begin : dac_l2
                mp64_fp32_adder u_dac_l2 (
                    .a(chunk_l1[fpl*2]), .b(chunk_l1[fpl*2+1]),
                    .result(chunk_l2[fpl])
                );
            end
            // Level 3: 2→1
            mp64_fp32_adder u_dac_l3 (
                .a(chunk_l2[0]), .b(chunk_l2[1]),
                .result(chunk_l3)
            );
        end
    endgenerate

    always @(*) begin
        fp_dotacc_result[0] = fp_dotacc_chunks[0].chunk_l3;
        fp_dotacc_result[1] = fp_dotacc_chunks[1].chunk_l3;
        fp_dotacc_result[2] = fp_dotacc_chunks[2].chunk_l3;
        fp_dotacc_result[3] = fp_dotacc_chunks[3].chunk_l3;
    end

    // ========================================================================
    // FP16/BF16 TRED — reductions (SUM, MIN, MAX, SUMSQ, MINIDX, MAXIDX)
    // ========================================================================
    // POPC/L1 fall through to integer path (operate on raw bits)
    reg [31:0] fp_red_result;      // FP32 for SUM/SUMSQ, raw for MIN/MAX
    reg [63:0] fp_red_idx;
    reg [31:0] fp_red_val;         // FP32 for MINIDX/MAXIDX value

    // FP SUM: reuse DOT adder tree with b=1.0 — actually simpler:
    //   widen each FP16 lane to FP32, then sum via adder tree
    // We already have fp_wmul_fp32 which is a*b products.
    // For SUM we need just the FP32-widened tile_a values.
    wire [31:0] fp_tile_a_fp32 [0:31];
    generate
        for (fpl = 0; fpl < 32; fpl = fpl + 1) begin : fp_widen_a
            mp64_fp16_to_fp32 u_widen_a (
                .is_bf16(mode_bf16),
                .fp16_in(tile_a[fpl*16 +: 16]),
                .fp32_out(fp_tile_a_fp32[fpl])
            );
        end
    endgenerate

    // FP SUM adder tree (32 FP32 values → 1)
    wire [31:0] fp_sum_l1 [0:15];
    wire [31:0] fp_sum_l2 [0:7];
    wire [31:0] fp_sum_l3 [0:3];
    wire [31:0] fp_sum_l4 [0:1];
    wire [31:0] fp_sum_l5;
    generate
        for (fpl = 0; fpl < 16; fpl = fpl + 1) begin : sum_l1
            mp64_fp32_adder u_sum_l1 (.a(fp_tile_a_fp32[fpl*2]), .b(fp_tile_a_fp32[fpl*2+1]), .result(fp_sum_l1[fpl]));
        end
        for (fpl = 0; fpl < 8; fpl = fpl + 1) begin : sum_l2
            mp64_fp32_adder u_sum_l2 (.a(fp_sum_l1[fpl*2]), .b(fp_sum_l1[fpl*2+1]), .result(fp_sum_l2[fpl]));
        end
        for (fpl = 0; fpl < 4; fpl = fpl + 1) begin : sum_l3
            mp64_fp32_adder u_sum_l3 (.a(fp_sum_l2[fpl*2]), .b(fp_sum_l2[fpl*2+1]), .result(fp_sum_l3[fpl]));
        end
        for (fpl = 0; fpl < 2; fpl = fpl + 1) begin : sum_l4
            mp64_fp32_adder u_sum_l4 (.a(fp_sum_l3[fpl*2]), .b(fp_sum_l3[fpl*2+1]), .result(fp_sum_l4[fpl]));
        end
        mp64_fp32_adder u_sum_l5 (.a(fp_sum_l4[0]), .b(fp_sum_l4[1]), .result(fp_sum_l5));
    endgenerate

    // FP SUMSQ: square each lane (a*a), widen, sum
    wire [31:0] fp_sq_fp32 [0:31];
    generate
        for (fpl = 0; fpl < 32; fpl = fpl + 1) begin : fp_sq_lanes
            wire [15:0] fp_sq_prod;
            mp64_fp16_alu u_fp_sq (
                .is_bf16(mode_bf16),
                .a(tile_a[fpl*16 +: 16]), .b(tile_a[fpl*16 +: 16]),
                .op_add(1'b0), .op_sub(1'b0), .op_mul(1'b1),
                .op_min(1'b0), .op_max(1'b0), .op_abs(1'b0),
                .op_cmp_lt(1'b0), .op_cmp_gt(1'b0),
                .result(fp_sq_prod), .is_nan_a(), .is_nan_b()
            );
            mp64_fp16_to_fp32 u_sq_widen (
                .is_bf16(mode_bf16), .fp16_in(fp_sq_prod), .fp32_out(fp_sq_fp32[fpl])
            );
        end
    endgenerate

    wire [31:0] fp_sumsq_l1 [0:15];
    wire [31:0] fp_sumsq_l2 [0:7];
    wire [31:0] fp_sumsq_l3 [0:3];
    wire [31:0] fp_sumsq_l4 [0:1];
    wire [31:0] fp_sumsq_l5;
    generate
        for (fpl = 0; fpl < 16; fpl = fpl + 1) begin : ssq_l1
            mp64_fp32_adder u_ssq_l1 (.a(fp_sq_fp32[fpl*2]), .b(fp_sq_fp32[fpl*2+1]), .result(fp_sumsq_l1[fpl]));
        end
        for (fpl = 0; fpl < 8; fpl = fpl + 1) begin : ssq_l2
            mp64_fp32_adder u_ssq_l2 (.a(fp_sumsq_l1[fpl*2]), .b(fp_sumsq_l1[fpl*2+1]), .result(fp_sumsq_l2[fpl]));
        end
        for (fpl = 0; fpl < 4; fpl = fpl + 1) begin : ssq_l3
            mp64_fp32_adder u_ssq_l3 (.a(fp_sumsq_l2[fpl*2]), .b(fp_sumsq_l2[fpl*2+1]), .result(fp_sumsq_l3[fpl]));
        end
        for (fpl = 0; fpl < 2; fpl = fpl + 1) begin : ssq_l4
            mp64_fp32_adder u_ssq_l4 (.a(fp_sumsq_l3[fpl*2]), .b(fp_sumsq_l3[fpl*2+1]), .result(fp_sumsq_l4[fpl]));
        end
        mp64_fp32_adder u_ssq_l5 (.a(fp_sumsq_l4[0]), .b(fp_sumsq_l4[1]), .result(fp_sumsq_l5));
    endgenerate

    // FP MIN / MAX / MINIDX / MAXIDX — sequential scan with comparators
    wire fp_cmp_lt [0:31];
    wire fp_cmp_gt [0:31];

    generate
        for (fpl = 0; fpl < 32; fpl = fpl + 1) begin : fp_cmp_lanes
            mp64_fp16_alu u_fp_cmp (
                .is_bf16(mode_bf16),
                .a(tile_a[fpl*16 +: 16]),
                .b(16'd0),  // unused for cmp
                .op_add(1'b0), .op_sub(1'b0), .op_mul(1'b0),
                .op_min(1'b0), .op_max(1'b0), .op_abs(1'b0),
                .op_cmp_lt(1'b0), .op_cmp_gt(1'b0),
                .result(),
                .is_nan_a(fp_cmp_lt[fpl]),  // reuse NaN detect
                .is_nan_b()
            );
        end
    endgenerate

    // Combinational FP min/max/minidx/maxidx reduction
    integer fp_ri;
    always @(*) begin : fp_red_block
        reg [15:0] best_raw;
        reg [31:0] best_fp32;
        reg [63:0] best_idx;
        reg        cur_nan, best_nan;
        reg [31:0] cur_fp32;

        fp_red_result = 32'd0;
        fp_red_idx    = 64'd0;
        fp_red_val    = 32'd0;

        case (funct_reg)
            TRED_SUM: fp_red_result = fp_sum_l5;
            TRED_SUMSQ: fp_red_result = fp_sumsq_l5;
            TRED_MIN, TRED_MINIDX: begin
                best_raw = tile_a[15:0];
                best_idx = 64'd0;
                for (fp_ri = 1; fp_ri < 32; fp_ri = fp_ri + 1) begin : fpmin_loop
                    reg [15:0] cur_raw;
                    reg        cur_is_nan, best_is_nan;
                    reg        cur_lt;
                    // NaN detection inline
                    cur_raw = tile_a[fp_ri*16 +: 16];
                    if (mode_bf16) begin
                        cur_is_nan  = (cur_raw[14:7] == 8'hFF) && |cur_raw[6:0];
                        best_is_nan = (best_raw[14:7] == 8'hFF) && |best_raw[6:0];
                    end else begin
                        cur_is_nan  = (cur_raw[14:10] == 5'h1F) && |cur_raw[9:0];
                        best_is_nan = (best_raw[14:10] == 5'h1F) && |best_raw[9:0];
                    end
                    // Skip NaNs; take first non-NaN or update if cur < best
                    if (!cur_is_nan) begin
                        if (best_is_nan) begin
                            best_raw = cur_raw;
                            best_idx = fp_ri;
                        end else begin
                            // Compare: use magnitude comparison
                            // Both positive: smaller mag = smaller val
                            // Both negative: larger mag = smaller val
                            // Different sign: negative is smaller
                            if (cur_raw[15] != best_raw[15]) begin
                                cur_lt = cur_raw[15]; // cur negative → cur < best
                            end else if (cur_raw[15]) begin
                                cur_lt = (cur_raw[14:0] > best_raw[14:0]); // both neg
                            end else begin
                                cur_lt = (cur_raw[14:0] < best_raw[14:0]); // both pos
                            end
                            if (cur_lt) begin
                                best_raw = cur_raw;
                                best_idx = fp_ri;
                            end
                        end
                    end
                end
                // Convert best to FP32 for storage
                if (mode_bf16)
                    fp_red_val = {best_raw, 16'd0};
                else begin
                    // Inline FP16→FP32 for the winner
                    if (best_raw[14:10] == 5'd0 && best_raw[9:0] == 10'd0)
                        fp_red_val = {best_raw[15], 31'd0};
                    else if (best_raw[14:10] == 5'd31)
                        fp_red_val = {best_raw[15], 8'hFF, best_raw[9:0], 13'd0};
                    else
                        fp_red_val = {best_raw[15], {3'd0, best_raw[14:10]} + 8'd112, best_raw[9:0], 13'd0};
                end
                fp_red_result = fp_red_val;
                fp_red_idx    = best_idx;
            end
            TRED_MAX, TRED_MAXIDX: begin
                best_raw = tile_a[15:0];
                best_idx = 64'd0;
                for (fp_ri = 1; fp_ri < 32; fp_ri = fp_ri + 1) begin : fpmax_loop
                    reg [15:0] cur_raw;
                    reg        cur_is_nan, best_is_nan;
                    reg        cur_gt;
                    cur_raw = tile_a[fp_ri*16 +: 16];
                    if (mode_bf16) begin
                        cur_is_nan  = (cur_raw[14:7] == 8'hFF) && |cur_raw[6:0];
                        best_is_nan = (best_raw[14:7] == 8'hFF) && |best_raw[6:0];
                    end else begin
                        cur_is_nan  = (cur_raw[14:10] == 5'h1F) && |cur_raw[9:0];
                        best_is_nan = (best_raw[14:10] == 5'h1F) && |best_raw[9:0];
                    end
                    if (!cur_is_nan) begin
                        if (best_is_nan) begin
                            best_raw = cur_raw;
                            best_idx = fp_ri;
                        end else begin
                            if (cur_raw[15] != best_raw[15]) begin
                                cur_gt = best_raw[15]; // best negative → cur > best
                            end else if (cur_raw[15]) begin
                                cur_gt = (cur_raw[14:0] < best_raw[14:0]); // both neg
                            end else begin
                                cur_gt = (cur_raw[14:0] > best_raw[14:0]); // both pos
                            end
                            if (cur_gt) begin
                                best_raw = cur_raw;
                                best_idx = fp_ri;
                            end
                        end
                    end
                end
                if (mode_bf16)
                    fp_red_val = {best_raw, 16'd0};
                else begin
                    if (best_raw[14:10] == 5'd0 && best_raw[9:0] == 10'd0)
                        fp_red_val = {best_raw[15], 31'd0};
                    else if (best_raw[14:10] == 5'd31)
                        fp_red_val = {best_raw[15], 8'hFF, best_raw[9:0], 13'd0};
                    else
                        fp_red_val = {best_raw[15], {3'd0, best_raw[14:10]} + 8'd112, best_raw[9:0], 13'd0};
                end
                fp_red_result = fp_red_val;
                fp_red_idx    = best_idx;
            end
            // POPC, L1: fall through to integer path (raw bit operations)
            default: ;
        endcase
    end

    // ========================================================================
    // FP PACK — format conversion (FP16↔BF16), not narrowing
    // ========================================================================
    reg [511:0] fp_pack_result;
    wire [15:0] fp_pack_out [0:31];

    generate
        for (fpl = 0; fpl < 32; fpl = fpl + 1) begin : fp_pack_lanes
            wire [31:0] widened;
            // Widen to FP32 from source format
            mp64_fp16_to_fp32 u_pk_widen (
                .is_bf16(mode_bf16), .fp16_in(tile_a[fpl*16 +: 16]), .fp32_out(widened)
            );
            // Convert FP32 to target format (opposite of source)
            mp64_fp32_to_fp16 u_pk_cvt (
                .is_bf16(!mode_bf16), .fp32_in(widened), .fp16_out(fp_pack_out[fpl])
            );
        end
    endgenerate

    integer fp_pk;
    always @(*) begin
        fp_pack_result = 512'd0;
        for (fp_pk = 0; fp_pk < 32; fp_pk = fp_pk + 1)
            fp_pack_result[fp_pk*16 +: 16] = fp_pack_out[fp_pk];
    end

    // ========================================================================
    // FP UNPACK — widen FP16/BF16 → FP32 (first 16 lanes → 16 × 32-bit)
    // ========================================================================
    reg [511:0] fp_unpack_result;

    integer fp_ul;
    always @(*) begin
        fp_unpack_result = 512'd0;
        for (fp_ul = 0; fp_ul < 16; fp_ul = fp_ul + 1)
            fp_unpack_result[fp_ul*32 +: 32] = fp_tile_a_fp32[fp_ul];
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
    // RROT — Row/Column Rotate or Mirror
    // ========================================================================
    reg [511:0] rrot_result;
    wire [1:0]  rrot_dir    = imm8_reg[1:0];
    wire [2:0]  rrot_amt    = imm8_reg[4:2];
    wire        rrot_mirror = imm8_reg[5];
    integer rr, rc, rrot_src;
    always @(*) begin
        rrot_result = 512'd0;
        case (mode_ew[1:0])
            2'd0: // 8-bit: 8 rows × 8 cols
                for (rr = 0; rr < 8; rr = rr + 1)
                    for (rc = 0; rc < 8; rc = rc + 1) begin
                        if (rrot_mirror) begin
                            if (rrot_dir[0]) rrot_src = (7 - rr) * 8 + rc;
                            else             rrot_src = rr * 8 + (7 - rc);
                        end else begin
                            case (rrot_dir)
                                2'd0: rrot_src = rr * 8 + ((rc + rrot_amt) & 7);
                                2'd1: rrot_src = rr * 8 + ((rc + 8 - rrot_amt) & 7);
                                2'd2: rrot_src = ((rr + rrot_amt) & 7) * 8 + rc;
                                default: rrot_src = ((rr + 8 - rrot_amt) & 7) * 8 + rc;
                            endcase
                        end
                        rrot_result[(rr*8+rc)*8 +: 8] = tile_a[rrot_src*8 +: 8];
                    end
            2'd1: // 16-bit: 4 rows × 8 cols
                for (rr = 0; rr < 4; rr = rr + 1)
                    for (rc = 0; rc < 8; rc = rc + 1) begin
                        if (rrot_mirror) begin
                            if (rrot_dir[0]) rrot_src = (3 - rr) * 8 + rc;
                            else             rrot_src = rr * 8 + (7 - rc);
                        end else begin
                            case (rrot_dir)
                                2'd0: rrot_src = rr * 8 + ((rc + rrot_amt) & 7);
                                2'd1: rrot_src = rr * 8 + ((rc + 8 - rrot_amt) & 7);
                                2'd2: rrot_src = ((rr + rrot_amt) & 3) * 8 + rc;
                                default: rrot_src = ((rr + 4 - rrot_amt) & 3) * 8 + rc;
                            endcase
                        end
                        rrot_result[(rr*8+rc)*16 +: 16] = tile_a[rrot_src*16 +: 16];
                    end
            2'd2: // 32-bit: 4 rows × 4 cols
                for (rr = 0; rr < 4; rr = rr + 1)
                    for (rc = 0; rc < 4; rc = rc + 1) begin
                        if (rrot_mirror) begin
                            if (rrot_dir[0]) rrot_src = (3 - rr) * 4 + rc;
                            else             rrot_src = rr * 4 + (3 - rc);
                        end else begin
                            case (rrot_dir)
                                2'd0: rrot_src = rr * 4 + ((rc + rrot_amt) & 3);
                                2'd1: rrot_src = rr * 4 + ((rc + 4 - rrot_amt) & 3);
                                2'd2: rrot_src = ((rr + rrot_amt) & 3) * 4 + rc;
                                default: rrot_src = ((rr + 4 - rrot_amt) & 3) * 4 + rc;
                            endcase
                        end
                        rrot_result[(rr*4+rc)*32 +: 32] = tile_a[rrot_src*32 +: 32];
                    end
            2'd3: // 64-bit: 2 rows × 4 cols
                for (rr = 0; rr < 2; rr = rr + 1)
                    for (rc = 0; rc < 4; rc = rc + 1) begin
                        if (rrot_mirror) begin
                            if (rrot_dir[0]) rrot_src = (1 - rr) * 4 + rc;
                            else             rrot_src = rr * 4 + (3 - rc);
                        end else begin
                            case (rrot_dir)
                                2'd0: rrot_src = rr * 4 + ((rc + rrot_amt) & 3);
                                2'd1: rrot_src = rr * 4 + ((rc + 4 - rrot_amt) & 3);
                                2'd2: rrot_src = ((rr + rrot_amt) & 1) * 4 + rc;
                                default: rrot_src = ((rr + 2 - rrot_amt) & 1) * 4 + rc;
                            endcase
                        end
                        rrot_result[(rr*4+rc)*64 +: 64] = tile_a[rrot_src*64 +: 64];
                    end
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
                reg [7:0] ea8, eb8, shr8;
                reg [2:0] sh;
                reg       rnd8;
                ea8 = tile_a[el*8 +: 8]; eb8 = src_b_selected[el*8 +: 8]; sh = eb8[2:0];
                rnd8 = (mode_rounding && sh != 0) ? ea8[sh - 3'd1] : 1'b0;
                case (funct_reg)
                    3'd0: begin
                        if (mode_signed) shr8 = $signed(ea8) >>> sh;
                        else             shr8 = ea8 >> sh;
                        ext_talu_result[el*8 +: 8] = shr8 + {7'd0, rnd8};
                    end
                    3'd1: ext_talu_result[el*8 +: 8] = ea8 << sh;
                    3'd2: ext_talu_result[el*8 +: 8] = eb8[7] ? ea8 : 8'd0; // VSEL
                    3'd3: ext_talu_result[el*8 +: 8] = clz8(ea8);
                    default: ;
                endcase
            end
            2'd1: for (el=0; el<32; el=el+1) begin : ex16
                reg [15:0] ea16, eb16, shr16; reg [3:0] sh16;
                reg        rnd16;
                ea16 = tile_a[el*16 +: 16]; eb16 = src_b_selected[el*16 +: 16]; sh16 = eb16[3:0];
                rnd16 = (mode_rounding && sh16 != 0) ? ea16[sh16 - 4'd1] : 1'b0;
                case (funct_reg)
                    3'd0: begin
                        if (mode_signed) shr16 = $signed(ea16) >>> sh16;
                        else             shr16 = ea16 >> sh16;
                        ext_talu_result[el*16 +: 16] = shr16 + {15'd0, rnd16};
                    end
                    3'd1: ext_talu_result[el*16 +: 16] = ea16 << sh16;
                    3'd2: ext_talu_result[el*16 +: 16] = eb16[15] ? ea16 : 16'd0; // VSEL
                    3'd3: ext_talu_result[el*16 +: 16] = clz16(ea16);
                    default: ;
                endcase
            end
            2'd2: for (el=0; el<16; el=el+1) begin : ex32
                reg [31:0] ea32, eb32, shr32; reg [4:0] sh32;
                reg        rnd32;
                ea32 = tile_a[el*32 +: 32]; eb32 = src_b_selected[el*32 +: 32]; sh32 = eb32[4:0];
                rnd32 = (mode_rounding && sh32 != 0) ? ea32[sh32 - 5'd1] : 1'b0;
                case (funct_reg)
                    3'd0: begin
                        if (mode_signed) shr32 = $signed(ea32) >>> sh32;
                        else             shr32 = ea32 >> sh32;
                        ext_talu_result[el*32 +: 32] = shr32 + {31'd0, rnd32};
                    end
                    3'd1: ext_talu_result[el*32 +: 32] = ea32 << sh32;
                    3'd2: ext_talu_result[el*32 +: 32] = eb32[31] ? ea32 : 32'd0; // VSEL
                    3'd3: ext_talu_result[el*32 +: 32] = clz32(ea32);
                    default: ;
                endcase
            end
            2'd3: for (el=0; el<8; el=el+1) begin : ex64
                reg [63:0] ea64, eb64, shr64; reg [5:0] sh64;
                reg        rnd64;
                ea64 = tile_a[el*64 +: 64]; eb64 = src_b_selected[el*64 +: 64]; sh64 = eb64[5:0];
                rnd64 = (mode_rounding && sh64 != 0) ? ea64[sh64 - 6'd1] : 1'b0;
                case (funct_reg)
                    3'd0: begin
                        if (mode_signed) shr64 = $signed(ea64) >>> sh64;
                        else             shr64 = ea64 >> sh64;
                        ext_talu_result[el*64 +: 64] = shr64 + {63'd0, rnd64};
                    end
                    3'd1: ext_talu_result[el*64 +: 64] = ea64 << sh64;
                    3'd2: ext_talu_result[el*64 +: 64] = eb64[63] ? ea64 : 64'd0; // VSEL
                    3'd3: ext_talu_result[el*64 +: 64] = clz64(ea64);
                    default: ;
                endcase
            end
        endcase
    end

    // ========================================================================
    // FP32 ACC_ACC adders (pre-computed for sequential assignment)
    // ========================================================================
    // DOT ACC_ACC: acc[0] + fp_dot_result
    wire [31:0] fp_dot_acc_result;
    mp64_fp32_adder u_dot_acc_add (
        .a(acc[0][31:0]), .b(fp_dot_result), .result(fp_dot_acc_result)
    );

    // DOTACC ACC_ACC: acc[k] + fp_dotacc_result[k]
    wire [31:0] fp_dotacc_acc_result [0:3];
    generate
        for (fpl = 0; fpl < 4; fpl = fpl + 1) begin : dotacc_acc_add
            mp64_fp32_adder u_dac_acc (
                .a(acc[fpl][31:0]), .b(fp_dotacc_result[fpl]),
                .result(fp_dotacc_acc_result[fpl])
            );
        end
    endgenerate

    // TRED SUM/SUMSQ ACC_ACC: acc[0] + fp_red_result
    wire [31:0] fp_red_acc_result;
    mp64_fp32_adder u_red_acc_add (
        .a(acc[0][31:0]), .b(fp_red_result), .result(fp_red_acc_result)
    );

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
                        tile_addr <= tdst[31:0];
                        tile_wen  <= 1'b1;
                        tile_wdata<= 512'd0;
                        state     <= S_DONE;
                    end
                    // TSYS.TRANS — read TDST
                    else if (mex_op == MEX_TSYS && mex_funct == TSYS_TRANS &&
                             !(mex_ext_active && mex_ext_mod == 4'd8)) begin
                        tile_req  <= 1'b1;
                        tile_addr <= tdst[31:0];
                        state     <= S_LOAD_A;
                    end
                    // TSYS.LOADC — cursor address
                    else if (mex_op == MEX_TSYS && mex_funct == TSYS_LOADC &&
                             !(mex_ext_active && mex_ext_mod == 4'd8)) begin
                        tile_req  <= 1'b1;
                        tile_addr <= (tile_row[31:0] * tile_stride[31:0] + tile_col[31:0]) * 32'd64;
                        state     <= S_LOAD_A;
                    end
                    // TRED — load TSRC0 only
                    else if (mex_op == MEX_TRED) begin
                        if (src0_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= tsrc0[31:0];
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
                            tile_addr <= tsrc0[31:0];
                            state     <= S_LOAD_A;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= tsrc0;
                            state         <= S_EXT_LOAD_A;
                        end
                    end
                    // EXT.8 TSYS — LOAD2D / STORE2D
                    else if (mex_op == MEX_TSYS && mex_ext_active && mex_ext_mod == 4'd8) begin
                        // Compute cursor base address
                        ld2d_base   <= (tile_bank * 64'h400000)
                                     + (tile_row * tile_stride + tile_col) * 64;
                        ld2d_row_addr <= (tile_bank * 64'h400000)
                                      + (tile_row * tile_stride + tile_col) * 64;
                        ld2d_stride <= (tstride_r != 0) ? tstride_r : ttile_w;
                        ld2d_h      <= ttile_h[3:0];
                        ld2d_w      <= ttile_w[6:0];
                        ld2d_row    <= 4'd0;
                        ld2d_off    <= 7'd0;
                        result      <= 512'd0;
                        if (mex_funct == ETSYS_LOAD2D)
                            state <= S_LOAD2D_REQ;
                        else if (mex_funct == ETSYS_STORE2D) begin
                            // STORE2D: load source tile first
                            if (src0_internal) begin
                                tile_req  <= 1'b1;
                                tile_addr <= tsrc0[31:0];
                                state     <= S_LOAD_A;
                                // After S_LOAD_A completes, we'll check funct
                                // and redirect to S_STORE2D_REQ
                            end else begin
                                ext_tile_req  <= 1'b1;
                                ext_tile_addr <= tsrc0;
                                state         <= S_EXT_LOAD_A;
                            end
                        end else
                            state <= S_DONE;  // unknown ext TSYS funct
                    end
                    // Everything else: load TSRC0
                    else begin
                        if (src0_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= tsrc0[31:0];
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
                        // STORE2D via EXT.8: tile_a loaded, now scatter
                        if (ext_active_reg && ext_mod_reg == 4'd8)
                            state <= S_STORE2D_REQ;
                        // SHUFFLE needs index tile from TSRC1
                        else if (funct_reg == TSYS_SHUFFLE) begin
                            if (src1_internal) begin
                                tile_req  <= 1'b1;
                                tile_addr <= tsrc1[31:0];
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
                            tile_addr <= tsrc1[31:0];
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
                            tile_addr <= tdst[31:0];
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
                        tile_addr <= tdst[31:0];
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
                    if (mode_fp) begin
                        case (funct_reg)
                            TMUL_MUL: result <= fp_mul_result;
                            TMUL_WMUL: begin result <= fp_wmul_lo; result2 <= fp_wmul_hi; end
                            TMUL_MAC: result <= fp_mac_result;
                            TMUL_FMA: result <= fp_mac_result;
                            default:  result <= fp_mul_result;
                        endcase
                    end else begin
                        case (funct_reg)
                            TMUL_MUL: result <= mul_result;
                            TMUL_WMUL: begin result <= wmul_lo; result2 <= wmul_hi; end
                            TMUL_MAC: result <= mac_result;
                            TMUL_FMA: result <= mac_result;
                            default:  result <= mul_result;
                        endcase
                    end
                end
                else if (op_reg == MEX_TSYS) begin
                    case (funct_reg)
                        TSYS_TRANS:   result <= trans_result;
                        TSYS_MOVBANK: result <= tile_a;
                        TSYS_LOADC:   result <= tile_a;
                        TSYS_PACK:    result <= mode_fp ? fp_pack_result : pack_result;
                        TSYS_UNPACK:  result <= mode_fp ? fp_unpack_result : unpack_result;
                        TSYS_SHUFFLE: result <= shuffle_result;
                        TSYS_RROT:    result <= rrot_result;
                        default:      result <= 512'd0;
                    endcase
                end

                // DOT/DOTACC → accumulator, then done (no tile store)
                if (op_reg == MEX_TMUL && funct_reg == TMUL_DOT) begin
                    if (mode_fp) begin
                        // FP DOT: result is FP32 in low 32 bits of acc[0]
                        if (tctrl[1]) begin
                            acc[0] <= {32'd0, fp_dot_result};
                            acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end else if (tctrl[0]) begin
                            // ACC_ACC: add new FP32 dot to existing FP32 acc
                            // Use an inline FP32 add (can't instantiate in sequential)
                            // Store raw — the emulator does acc[0] = fp32_to_bits(old + new)
                            // For RTL, we need a registered FP32 adder.
                            // Workaround: pre-compute in combinational, select here.
                            acc[0] <= {32'd0, fp_dot_acc_result};
                        end else begin
                            acc[0] <= {32'd0, fp_dot_result};
                            acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end
                    end else begin
                        if (tctrl[1]) begin
                            acc[0] <= dot_result; acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end else if (tctrl[0])
                            acc[0] <= acc[0] + dot_result;
                        else begin
                            acc[0] <= dot_result; acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end
                    end
                    state <= S_DONE;
                end
                else if (op_reg == MEX_TMUL && funct_reg == TMUL_DOTACC) begin
                    if (mode_fp) begin
                        if (tctrl[1]) begin
                            acc[0] <= {32'd0, fp_dotacc_result[0]};
                            acc[1] <= {32'd0, fp_dotacc_result[1]};
                            acc[2] <= {32'd0, fp_dotacc_result[2]};
                            acc[3] <= {32'd0, fp_dotacc_result[3]};
                        end else if (tctrl[0]) begin
                            acc[0] <= {32'd0, fp_dotacc_acc_result[0]};
                            acc[1] <= {32'd0, fp_dotacc_acc_result[1]};
                            acc[2] <= {32'd0, fp_dotacc_acc_result[2]};
                            acc[3] <= {32'd0, fp_dotacc_acc_result[3]};
                        end else begin
                            acc[0] <= {32'd0, fp_dotacc_result[0]};
                            acc[1] <= {32'd0, fp_dotacc_result[1]};
                            acc[2] <= {32'd0, fp_dotacc_result[2]};
                            acc[3] <= {32'd0, fp_dotacc_result[3]};
                        end
                    end else begin
                        if (tctrl[1]) begin
                            acc[0] <= dotacc[0]; acc[1] <= dotacc[1]; acc[2] <= dotacc[2]; acc[3] <= dotacc[3];
                        end else if (tctrl[0]) begin
                            acc[0] <= acc[0]+dotacc[0]; acc[1] <= acc[1]+dotacc[1];
                            acc[2] <= acc[2]+dotacc[2]; acc[3] <= acc[3]+dotacc[3];
                        end else begin
                            acc[0] <= dotacc[0]; acc[1] <= dotacc[1]; acc[2] <= dotacc[2]; acc[3] <= dotacc[3];
                        end
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
                    tile_addr  <= tdst[31:0];
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
                    tile_addr  <= tdst[31:0] + 32'd64;
                    tile_wen   <= 1'b1;
                    tile_wdata <= result2;
                    state      <= S_DONE;
                end
            end

            S_REDUCE: begin
                if (mode_fp && (funct_reg != TRED_POPC) && (funct_reg != TRED_L1)) begin
                    // FP reductions: result is FP32 stored in acc[0][31:0]
                    if (funct_reg == TRED_MINIDX || funct_reg == TRED_MAXIDX) begin
                        if (tctrl[1]) begin
                            acc[0] <= fp_red_idx;
                            acc[1] <= {32'd0, fp_red_val};
                            acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end else if (tctrl[0]) begin
                            // Compare new vs old best
                            if (funct_reg == TRED_MINIDX) begin
                                // If new FP value < old acc[1] FP32 value
                                if (fp_red_val[31] && !acc[1][31])  // new neg, old pos → new < old
                                    begin acc[0] <= fp_red_idx; acc[1] <= {32'd0, fp_red_val}; end
                                else if (!fp_red_val[31] && acc[1][31]) ;  // new pos, old neg → keep old
                                else if (fp_red_val[31]) begin  // both negative
                                    if (fp_red_val[30:0] > acc[1][30:0])
                                        begin acc[0] <= fp_red_idx; acc[1] <= {32'd0, fp_red_val}; end
                                end else begin  // both positive
                                    if (fp_red_val[30:0] < acc[1][30:0])
                                        begin acc[0] <= fp_red_idx; acc[1] <= {32'd0, fp_red_val}; end
                                end
                            end else begin
                                if (!fp_red_val[31] && acc[1][31])
                                    begin acc[0] <= fp_red_idx; acc[1] <= {32'd0, fp_red_val}; end
                                else if (fp_red_val[31] && !acc[1][31]) ;
                                else if (fp_red_val[31]) begin
                                    if (fp_red_val[30:0] < acc[1][30:0])
                                        begin acc[0] <= fp_red_idx; acc[1] <= {32'd0, fp_red_val}; end
                                end else begin
                                    if (fp_red_val[30:0] > acc[1][30:0])
                                        begin acc[0] <= fp_red_idx; acc[1] <= {32'd0, fp_red_val}; end
                                end
                            end
                        end else begin
                            acc[0] <= fp_red_idx;
                            acc[1] <= {32'd0, fp_red_val};
                        end
                    end else begin
                        // SUM, MIN, MAX, SUMSQ → FP32 in acc[0]
                        if (tctrl[1]) begin
                            acc[0] <= {32'd0, fp_red_result};
                            acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end else if (tctrl[0])
                            acc[0] <= {32'd0, fp_red_acc_result};
                        else
                            acc[0] <= {32'd0, fp_red_result};
                    end
                end else begin
                    // Integer reductions (unchanged)
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
                            tile_addr <= tsrc1[31:0];
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
                            tile_addr <= tsrc1[31:0];
                            state     <= S_LOAD_B;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= tsrc1;
                            state         <= S_EXT_LOAD_B;
                        end
                    end else begin
                        if (needs_load_c) begin
                            tile_req <= 1'b1; tile_addr <= tdst[31:0]; state <= S_LOAD_C;
                        end else state <= S_COMPUTE;
                    end
                end
            end

            S_EXT_LOAD_B: begin
                if (ext_tile_ack) begin
                    tile_b <= ext_tile_rdata;
                    if (needs_load_c) begin
                        tile_req <= 1'b1; tile_addr <= tdst[31:0]; state <= S_LOAD_C;
                    end else state <= S_COMPUTE;
                end
            end

            S_EXT_STORE: if (ext_tile_ack) state <= S_DONE;

            // ================================================================
            // LOAD2D: multi-cycle strided gather from tile BRAM → result
            // ================================================================
            S_LOAD2D_REQ: begin
                tile_req  <= 1'b1;
                tile_addr <= ld2d_row_addr[31:0];  // bits[5:0] ignored by BRAM
                state     <= S_LOAD2D_WAIT;
            end

            S_LOAD2D_WAIT: begin
                if (tile_ack) begin : load2d_extract
                    integer bi;
                    reg [6:0] byte_off;
                    byte_off = {1'b0, ld2d_row_addr[5:0]};
                    // Extract ld2d_w bytes from tile_rdata at byte_off
                    // into result at ld2d_off
                    for (bi = 0; bi < 64; bi = bi + 1) begin
                        if (bi[6:0] < ld2d_w &&
                            (ld2d_off + bi[6:0]) < 7'd64 &&
                            (byte_off + bi[6:0]) < 7'd64)
                            result[(ld2d_off + bi[6:0])*8 +: 8] <=
                                tile_rdata[(byte_off + bi[6:0])*8 +: 8];
                    end
                    ld2d_row     <= ld2d_row + 4'd1;
                    ld2d_off     <= ld2d_off + ld2d_w;
                    ld2d_row_addr<= ld2d_row_addr + ld2d_stride;
                    if ((ld2d_row + 4'd1) < ld2d_h &&
                        (ld2d_off + ld2d_w) < 7'd64)
                        state <= S_LOAD2D_REQ;
                    else begin
                        // Write gathered tile to TDST
                        state <= S_STORE;
                    end
                end
            end

            // ================================================================
            // STORE2D: multi-cycle strided scatter from tile_a → BRAM
            //   Read-modify-write: read existing 64B block, merge w bytes,
            //   write back.
            // ================================================================
            S_STORE2D_REQ: begin
                tile_req  <= 1'b1;
                tile_addr <= ld2d_row_addr[31:0];
                state     <= S_STORE2D_WAIT;
            end

            S_STORE2D_WAIT: begin
                if (tile_ack) begin : store2d_merge
                    integer si;
                    reg [6:0] st_byte_off;
                    st_byte_off = {1'b0, ld2d_row_addr[5:0]};
                    // Read-modify-write: start from existing data
                    tile_wdata <= tile_rdata;
                    // Overwrite w bytes from tile_a at ld2d_off
                    for (si = 0; si < 64; si = si + 1) begin
                        if (si[6:0] < ld2d_w &&
                            (ld2d_off + si[6:0]) < 7'd64 &&
                            (st_byte_off + si[6:0]) < 7'd64)
                            tile_wdata[(st_byte_off + si[6:0])*8 +: 8] <=
                                tile_a[(ld2d_off + si[6:0])*8 +: 8];
                    end
                    // Issue write
                    tile_req  <= 1'b1;
                    tile_addr <= ld2d_row_addr[31:0];
                    tile_wen  <= 1'b1;
                    // Advance to next row
                    ld2d_row     <= ld2d_row + 4'd1;
                    ld2d_off     <= ld2d_off + ld2d_w;
                    ld2d_row_addr<= ld2d_row_addr + ld2d_stride;
                    if ((ld2d_row + 4'd1) < ld2d_h &&
                        (ld2d_off + ld2d_w) < 7'd64)
                        state <= S_STORE2D_REQ;
                    else
                        state <= S_DONE;
                end
            end

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
