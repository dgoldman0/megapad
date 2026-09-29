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
// Float formats (docs/floating-point.md): FP16 and BF16 run on 32 parallel
// half-precision lanes.  FP32 and FP64 element-wise arithmetic runs on
// FMA_UNITS multi-format FMA units (mp64_fma.v) over 8 / FMA_UNITS beats;
// the production chip has FMA_UNITS = 2, which is the §10 timing model.
//

`include "mp64_pkg.vh"

module mp64_tile #(
    parameter [TACC_CALLER_BITS-1:0] TACC_CALLER_BASE =
        {TACC_CALLER_BITS{1'b0}},
    parameter integer TACC_CALLER_COUNT = 1,
    parameter [63:0] TACC_BANK0_LIMIT = 64'h0000_0000_0010_0000,
    parameter [63:0] TACC_EXT_LIMIT   = 64'h0000_0000_FF00_0000,
    parameter [63:0] TACC_VRAM_BASE   = 64'h0000_0000_FF00_0000,
    parameter [63:0] TACC_VRAM_LIMIT  = 64'h0000_0000_FF40_0000,
    parameter [63:0] TACC_HBW_LIMIT   = 64'h0000_0001_0000_0000,
    // Multi-format FMA units for FP32/FP64 element-wise arithmetic: 1, 2, 4,
    // or 8.  The architectural timing model is FMA_UNITS = 2.
    parameter integer FMA_UNITS = 2
) (
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
    input  wire [7:0]  mex_funct_byte, // complete encoded function byte
    input  wire [63:0] mex_gpr_val,    // GPR value (for broadcast mode)
    input  wire [7:0]  mex_imm8,       // immediate (for splat mode)
    input  wire [3:0]  mex_ext_mod,    // EXT prefix modifier
    input  wire        mex_ext_active, // EXT prefix is active
    input  wire [4:0]  mex_caller_id,  // absolute architectural caller ID
    input  wire        mex_priv,       // 0 = supervisor, 1 = user
    input  wire [63:0] mex_mpu_base,
    input  wire [63:0] mex_mpu_limit,
    input  wire        mex_mpu_enabled,
    input  wire        mex_allow_cluster_spad,
    input  wire [7:0]  mex_engine_epoch,
    input  wire [7:0]  mex_caller_epoch,
    input  wire [1:0]  mex_caller_slot,
    input  wire        engine_reset,
    input  wire [3:0]  caller_cancel,
    input  wire [31:0] caller_epochs,
    output reg  [7:0]  engine_epoch,
    input  wire        mex_retire,     // receiver accepts terminal response
    output wire        mex_done,       // operation complete
    output wire        mex_zero_valid, // completion also updates FLAGS.Z
    output wire        mex_zero,       // the new FLAGS.Z value
    output wire        mex_busy,       // engine busy (stall CPU)
    output wire [2:0]  mex_fault,
    output wire [63:0] mex_fault_addr,
    output reg         mex_stall_cycle,

    // === TACC CSR sidebands ===
    output wire [63:0] tacc_status_raw,
    input  wire        tacc_ctl_valid,
    input  wire [4:0]  tacc_ctl_caller_id,
    input  wire        tacc_ctl_priv,
    input  wire [63:0] tacc_ctl_wdata,
    output wire        tacc_ctl_done,
    output wire [2:0]  tacc_ctl_fault,

    // === Chip-wide canonical-image transfer stage ===
    output wire         tacc_xfer_req,
    output wire         tacc_xfer_store,
    output wire         tacc_xfer_ext,
    output wire [63:0]  tacc_xfer_base,
    output wire [3:0]   tacc_xfer_format_ew,
    output wire [7:0]   tacc_xfer_token,
    output wire [2047:0] tacc_xfer_store_image,
    output wire         tacc_xfer_cancel,
    output wire         tacc_xfer_finish,
    input  wire         tacc_xfer_done,
    input  wire [7:0]   tacc_xfer_response_token,
    input  wire [2:0]   tacc_xfer_fault,
    input  wire [63:0]  tacc_xfer_fault_addr,
    input  wire [2047:0] tacc_xfer_load_image,

    // === Legacy ACC and caller-private configuration preload ===
    output wire [255:0] legacy_acc_state,
    input  wire [3:0]   legacy_acc_wen,
    input  wire [255:0] legacy_acc_wdata,
    input  wire         cfg_load,
    input  wire [63:0]  cfg_tmode,
    input  wire [63:0]  cfg_tctrl,
    input  wire [63:0]  cfg_tsrc0,
    input  wire [63:0]  cfg_tsrc1,
    input  wire [63:0]  cfg_tdst,
    input  wire [63:0]  cfg_sb,
    input  wire [63:0]  cfg_sr,
    input  wire [63:0]  cfg_sc,
    input  wire [63:0]  cfg_sw,
    input  wire [63:0]  cfg_tstride_r,
    input  wire [63:0]  cfg_tstride_c,
    input  wire [63:0]  cfg_ttile_h,
    input  wire [63:0]  cfg_ttile_w,
    output wire         acc_zero_consumed,

    // === Tile memory port (512-bit, directly to BRAM Port A) ===
    output reg         tile_req,
    output reg  [31:0] tile_addr,
    output reg         tile_wen,
    output reg  [511:0]tile_wdata,
    input  wire [511:0]tile_rdata,
    input  wire        tile_ack,
    input  wire        tile_error,
    input  wire [63:0] tile_fault_addr,

    // === External memory tile access (for tiles in external RAM) ===
    output reg         ext_tile_req,
    output reg  [63:0] ext_tile_addr,
    output reg         ext_tile_wen,
    output reg  [511:0]ext_tile_wdata,
    input  wire [511:0]ext_tile_rdata,
    input  wire        ext_tile_ack,
    input  wire        ext_tile_error,
    input  wire [63:0] ext_tile_fault_addr,

    // Cancel an ordinary source-lane transaction after a TAMAC caller or
    // engine cancellation.  The SoC arbiter drains an accepted target request
    // and may retain one replacement request for the same private engine.
    output reg         tile_source_cancel
);

    // ========================================================================
    // CSR registers
    // ========================================================================
    reg [63:0] tsrc0;
    reg [63:0] tsrc1;
    reg [63:0] tdst;
    reg [63:0] tmode;        // [3:0]=EW, [4]=signed, [5]=saturate, [6]=round
    reg [63:0] tctrl;        // bit[0]=accumulate, bit[1]=acc_zero
    reg        tctrl_accumulate_reg;
    reg        tctrl_acc_zero_reg;
    reg        tctrl_acc_zero_clear;
    reg [63:0] acc [0:3];    // 256-bit accumulator (4 × 64-bit)
    reg [63:0] tile_bank;
    reg [63:0] tile_row;
    reg [63:0] tile_col;
    reg [63:0] tile_stride;
    reg [63:0] tstride_r;    // row stride in bytes (for LOAD2D/STORE2D)
    reg [63:0] tstride_c;    // column stride in bytes (reserved)
    reg [63:0] ttile_h;      // tile height (rows, 1-8)
    reg [63:0] ttile_w;      // tile width (bytes per row, 1-64)

    assign legacy_acc_state = {acc[3], acc[2], acc[1], acc[0]};
    assign acc_zero_consumed = tctrl_acc_zero_clear;

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
            tile_bank   <= 64'd0;
            tile_row    <= 64'd0;
            tile_col    <= 64'd0;
            tile_stride <= 64'd0;
            tstride_r   <= 64'd0;
            tstride_c   <= 64'd0;
            ttile_h     <= 64'd8;   // default 8 rows
            ttile_w     <= 64'd8;   // default 8 bytes per row
        end else if (engine_reset) begin
            tsrc0       <= 64'd0;
            tsrc1       <= 64'd0;
            tdst        <= 64'd0;
            tmode       <= 64'd0;
            tctrl       <= 64'd0;
            tile_bank   <= 64'd0;
            tile_row    <= 64'd0;
            tile_col    <= 64'd0;
            tile_stride <= 64'd0;
            tstride_r   <= 64'd0;
            tstride_c   <= 64'd0;
            ttile_h     <= 64'd8;
            ttile_w     <= 64'd8;
        end else begin
            if (cfg_load) begin
                tmode       <= cfg_tmode & TMODE_WRITE_MASK;
                tctrl       <= cfg_tctrl & TCTRL_WRITE_MASK;
                tsrc0       <= cfg_tsrc0;
                tsrc1       <= cfg_tsrc1;
                tdst        <= cfg_tdst;
                tile_bank   <= cfg_sb;
                tile_row    <= cfg_sr;
                tile_col    <= cfg_sc;
                tile_stride <= cfg_sw;
                tstride_r   <= cfg_tstride_r;
                tstride_c   <= cfg_tstride_c;
                ttile_h     <= cfg_ttile_h;
                ttile_w     <= cfg_ttile_w;
            end else if (csr_wen) begin
                case (csr_addr)
                    CSR_TMODE: tmode       <= csr_wdata & TMODE_WRITE_MASK;
                    CSR_TCTRL: tctrl       <= csr_wdata & TCTRL_WRITE_MASK;
                    CSR_TSRC0: tsrc0       <= csr_wdata;
                    CSR_TSRC1: tsrc1       <= csr_wdata;
                    CSR_TDST:  tdst        <= csr_wdata;
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

            // A same-cycle explicit TCTRL write describes the next operation
            // and therefore wins over clearing the one-shot consumed by the
            // operation that just reached its terminal boundary.
            if (tctrl_acc_zero_clear &&
                !cfg_load && !(csr_wen && csr_addr == CSR_TCTRL))
                tctrl[1] <= 1'b0;
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
    wire [63:0] tdst_second = tdst + 64'd64;
    wire dst2_bank0 = (tdst_second[63:20] == 44'd0);
    wire dst2_hbw   = (tdst_second[63:32] == 32'd0)
                   && (tdst_second[31:20] >= 12'hFFD);
    wire dst2_internal = dst2_bank0 || dst2_hbw;

    // ========================================================================
    // Mode decode — one format descriptor per TMODE.EW code
    // ========================================================================
    // Codes 8-15 are reserved.  FP32 and FP64 are defined formats whose
    // operations land in Phases 4 and 5 of docs/megapad-full-float-plan.md;
    // until then admission fails closed on them.
    function tile_format_is_float;
        input [3:0] ew;
        tile_format_is_float = (ew == TMODE_FP16) || (ew == TMODE_BF16) ||
                               (ew == TMODE_FP32) || (ew == TMODE_FP64);
    endfunction

    // log2 of the lane width in bytes (docs/floating-point.md §2).
    function [1:0] tile_format_lane_log2;
        input [3:0] ew;
        case (ew)
            TMODE_16, TMODE_FP16, TMODE_BF16: tile_format_lane_log2 = 2'd1;
            TMODE_32, TMODE_FP32:             tile_format_lane_log2 = 2'd2;
            TMODE_64, TMODE_FP64:             tile_format_lane_log2 = 2'd3;
            default:                          tile_format_lane_log2 = 2'd0;
        endcase
    endfunction

    // Whether a MEX operation may run in a format (shared/tile_formats.py
    // admits).  funct is the effective function (0 for the immediate form)
    // and ext8 marks the EXT.8 forms.  docs/floating-point.md §5.2 makes
    // PACK, UNPACK, VSHR, VSHL, and VCLZ illegal in float formats and WMUL
    // illegal in FP64; the EXT.8 functions 4-7 land in Phases 6 and 8 of
    // docs/megapad-full-float-plan.md.  EXT.8 VSEL, TCVT, and TCMP also
    // restrict their sources and function-byte bits (§6).
    function tile_op_admitted;
        input [3:0] ew;
        input [1:0] op;
        input [2:0] funct;
        input       ext8;
        input [1:0] ss;
        input [7:0] funct_byte;
        begin
            if (ew > TMODE_FP64)
                tile_op_admitted = 1'b0;
            else if (ext8 && (op == MEX_TALU))
                case (funct)
                    ETALU_VSHR, ETALU_VSHL, ETALU_VCLZ:
                        tile_op_admitted = !tile_format_is_float(ew);
                    ETALU_VSEL:
                        tile_op_admitted = (ss != 2'd3);
                    ETALU_TCVT:
                        tile_op_admitted =
                            (ss == 2'd0) && !funct_byte[3] &&
                            (funct_byte[7:4] <= TMODE_FP64) &&
                            (funct_byte[7:4] != ew) &&
                            (tile_format_is_float(ew) ||
                             tile_format_is_float(funct_byte[7:4]));
                    ETALU_TCMP:
                        tile_op_admitted =
                            (ss != 2'd2) && (funct_byte[7:6] == 2'b00);
                    default:  // TDIV and TSQRT land in Phase 8
                        tile_op_admitted = 1'b0;
                endcase
            else if (ext8 && (op == MEX_TSYS))
                tile_op_admitted = 1'b1;
            else if (!tile_format_is_float(ew))
                tile_op_admitted = 1'b1;
            else case (op)
                MEX_TMUL:
                    tile_op_admitted =
                        (funct != TMUL_WMUL) || (ew != TMODE_FP64);
                MEX_TSYS:
                    tile_op_admitted =
                        (funct != TSYS_PACK) && (funct != TSYS_UNPACK);
                default:
                    tile_op_admitted = 1'b1;
            endcase
        end
    endfunction

    wire [3:0] mode_ew       = tmode[3:0];
    wire       mode_signed   = tmode[4];
    wire       mode_saturate = tmode[5];
    wire       mode_rounding = tmode[6];
    wire       mode_fp       = tile_format_is_float(mode_ew);
    wire       mode_bf16     = (mode_ew == TMODE_BF16);  // 0 = FP16, 1 = BF16
    wire       mode_fp64     = (mode_ew == TMODE_FP64);
    wire       mode_wide_fp  = (mode_ew == TMODE_FP32) || mode_fp64;
    // Every lane-shaped path uses the format's real lane width; FP16 and BF16
    // lanes are 16 bits (docs/floating-point.md §5).
    wire [1:0] lane_ew       = tile_format_lane_log2(mode_ew);

    // ========================================================================
    // State machine
    // ========================================================================
    localparam S_IDLE        = 5'd0;
    localparam S_LOAD_A      = 5'd1;
    localparam S_LOAD_B      = 5'd2;
    localparam S_COMPUTE     = 5'd3;
    localparam S_STORE       = 5'd4;
    localparam S_REDUCE      = 5'd5;
    localparam S_EXT_LOAD_A  = 5'd6;
    localparam S_EXT_LOAD_B  = 5'd7;
    localparam S_EXT_STORE   = 5'd8;
    localparam S_DONE        = 5'd9;
    localparam S_LOAD_C      = 5'd10;  // load existing TDST for MAC/FMA
    localparam S_STORE2      = 5'd11;  // wait first WMUL store, issue second
    localparam S_LOAD2D_REQ  = 5'd12;  // LOAD2D: issue tile read for row
    localparam S_LOAD2D_WAIT = 5'd13;  // LOAD2D: wait for tile ack
    localparam S_STORE2D_REQ = 5'd14;  // STORE2D: issue tile read (RMW)
    localparam S_STORE2D_WAIT= 5'd15;  // STORE2D: wait for read, issue write
    localparam S_STORE_WAIT  = 5'd16;  // wait ordinary internal store
    localparam S_STORE2_WAIT = 5'd17;  // wait final internal WMUL store
    localparam S_EXT_STORE2_WAIT = 5'd18; // wait final external WMUL store
    localparam S_STORE2D_WRITE_WAIT = 5'd19; // wait row write acknowledgement
    localparam S_TACC_WAIT   = 5'd20;  // held lifecycle request / terminal
    localparam S_TAMAC_LOAD_A= 5'd21;  // wait first TAMAC source
    localparam S_TAMAC_LOAD_B= 5'd22;  // wait second TAMAC source
    localparam S_TACC_INT    = 5'd23;  // one 16-lane feedback slice
    localparam S_FMA         = 5'd24;  // one FP32/FP64 FMA beat
    localparam S_TREE        = 5'd25;  // one FP32/FP64 reduction beat
    localparam S_CVT_READ_WAIT  = 5'd26;  // TCVT source tile read
    localparam S_CVT_CONVERT    = 5'd27;  // one sixteen-lane TCVT beat
    localparam S_CVT_WRITE_WAIT = 5'd28;  // TCVT destination tile write
    localparam S_CVT_PAD        = 5'd29;  // idle TCVT beats to 4 + (k - 1)

    reg [4:0]   state;
    reg         mex_done_reg;
    reg [2:0]   z_kind_reg;
    reg         mex_zero_valid_reg;
    reg         mex_zero_reg;
    localparam [2:0] Z_NONE      = 3'd0;  // FLAGS.Z unchanged
    localparam [2:0] Z_ALL_WORDS = 3'd1;  // ACC0-ACC3 all zero
    localparam [2:0] Z_ACC0      = 3'd2;  // ACC0 (an index) zero
    localparam [2:0] Z_FP_ACC0   = 3'd3;  // ACC0 binary32 is +-0
    localparam [2:0] Z_FP_ALL    = 3'd4;  // ACC0-ACC3 binary32 all +-0
    localparam [2:0] Z_FP64_ACC0 = 3'd5;  // ACC0 binary64 is +-0
    localparam [2:0] Z_FP64_ALL  = 3'd6;  // ACC0-ACC3 binary64 all +-0
    reg         mex_busy_reg;
    reg [2:0]   mex_fault_reg;
    reg [63:0]  mex_fault_addr_reg;
    wire        mex_done_internal;
    wire [2:0]  mex_fault_internal;
    reg [511:0] tile_a;
    reg [511:0] tile_b;
    reg [511:0] tile_c;          // existing TDST (MAC/FMA)
    reg [511:0] result;
    reg [511:0] result2;         // second result tile (WMUL high half)
    reg [1:0]   op_reg;
    reg [2:0]   funct_reg;
    reg [7:0]   funct_byte_reg;
    reg [1:0]   ss_reg;
    reg [63:0]  gpr_val_reg;
    reg [7:0]   imm8_reg;
    reg [3:0]   ext_mod_reg;
    reg         ext_active_reg;
    reg         needs_load_c;

    // Captured dispatch context.  Later TACC landings consume the protection
    // and identity fields; the epoch fields are already live so cancellation
    // cannot generate a late completion.
    reg [4:0]   caller_id_reg;
    reg         priv_reg;
    reg [63:0]  mpu_base_reg;
    reg [63:0]  mpu_limit_reg;
    reg         mpu_enabled_reg;
    reg         allow_cluster_spad_reg;
    reg [7:0]   request_engine_epoch_reg;
    reg [7:0]   caller_epoch_reg;
    reg [1:0]   caller_slot_reg;
    reg         engine_reset_seen;

    // TAMAC captures every control that affects routing or arithmetic
    // before source traffic begins.  Existing tile_a/tile_b and result scratch
    // registers retain operands and completed slices; no second TACC bank is
    // instantiated.
    reg [3:0]   tamac_ew_reg;
    reg         tamac_signed_reg;
    reg [3:0]   tamac_beat_reg;
    reg [63:0]  tamac_src_a_addr_reg;
    reg [63:0]  tamac_src_b_addr_reg;
    reg [63:0]  tacc_image_addr_reg;
    reg         tamac_src_b_ext_reg;
    reg         tamac_read_ext_reg;

    wire [7:0] incoming_caller_epoch_now =
        caller_epochs[mex_caller_slot*8 +: 8];
    wire [7:0] active_caller_epoch_now =
        caller_epochs[caller_slot_reg*8 +: 8];
    wire incoming_cancelled =
        (mex_engine_epoch != engine_epoch) ||
        caller_cancel[mex_caller_slot] ||
        (incoming_caller_epoch_now != mex_caller_epoch);
    wire active_cancelled =
        (state != S_IDLE) &&
        ((request_engine_epoch_reg != engine_epoch) ||
         caller_cancel[caller_slot_reg] ||
         (active_caller_epoch_now != caller_epoch_reg));

    wire csr_acc_write =
        csr_wen && ((csr_addr == CSR_ACC0) || (csr_addr == CSR_ACC1) ||
                    (csr_addr == CSR_ACC2) || (csr_addr == CSR_ACC3));
    wire mex_acc_mutation_cycle =
        !active_cancelled &&
        (((state == S_COMPUTE) && (op_reg == MEX_TMUL) &&
          ((funct_reg == TMUL_DOT) || (funct_reg == TMUL_DOTACC))) ||
         (state == S_REDUCE) || (state == S_TREE));

`ifndef SYNTHESIS
    // The cluster/common-ACC admission point must prevent simultaneous
    // architectural writers. Keep the priority deterministic in hardware,
    // but fail loudly in simulation if an integration violates the contract.
    always @(posedge clk) begin
        if (rst_n && !engine_reset) begin
            if ((|legacy_acc_wen) && csr_acc_write)
                $error("mp64_tile: concurrent legacy ACC restore and CSR write");
            if (((|legacy_acc_wen) || csr_acc_write) &&
                mex_acc_mutation_cycle)
                $error("mp64_tile: concurrent external/CSR and MEX ACC mutation");
            if ((mex_fault_internal != MEX_FAULT_NONE) &&
                !mex_done_internal)
                $error("mp64_tile: MEX fault without completion");
            if ((tacc_ctl_fault != MEX_FAULT_NONE) && !tacc_ctl_done)
                $error("mp64_tile: TACC control fault without acknowledgement");
        end
    end
`endif

    // Catch the complete assigned/reserved TACC namespaces using both
    // transports so no malformed variant can fall through to a legacy
    // low-three-bit operation and touch memory or legacy ACC.
    // With an immediate source the function byte is data and the function is
    // forced to zero, so only the exact TAMAC byte reaches the TACC decoder
    // (where it traps as a non-canonical source form).
    wire intercept_tacc_tmul =
        (mex_op == MEX_TMUL) &&
        ((mex_ss == 2'd2) ?
         (mex_funct_byte == 8'h06) :
         ((mex_funct == 3'd6) || (mex_funct == 3'd7) ||
          (mex_funct_byte[2:0] == 3'd6) ||
          (mex_funct_byte[2:0] == 3'd7)));
    wire intercept_tacc_lifecycle =
        (mex_op == MEX_TSYS) && mex_ext_active &&
        (mex_ext_mod == 4'd8) &&
        ((mex_funct >= 3'd2) || (mex_funct_byte[2:0] >= 3'd2));
    wire intercept_tacc_namespace =
        intercept_tacc_tmul || intercept_tacc_lifecycle;

    // A full core presents MEX as a one-cycle pulse.  The request mux therefore
    // presents the live dispatch in S_IDLE and the captured copy in
    // S_TACC_WAIT.  If an accepted FORCE defeats admission, the captured copy
    // remains valid until the state leaf is ready and revalidates it.
    wire tacc_req_from_input =
        (state == S_IDLE) && mex_valid && intercept_tacc_namespace &&
        !incoming_cancelled;
    // Drop held valid as soon as the leaf publishes its terminal pulse.  This
    // gives the leaf's level de-duplicator a clean boundary before a following
    // MEX request.  Cancellation also drops held valid on its terminal edge,
    // allowing the leaf to clear its de-dup latch before an immediately
    // following caller is captured.
    wire tacc_req_from_hold =
        (state == S_TACC_WAIT) && !tacc_req_done && !active_cancelled;
    wire tacc_req_valid = tacc_req_from_input || tacc_req_from_hold;
    wire [1:0] tacc_req_ss =
        tacc_req_from_input ? mex_ss : ss_reg;
    wire [1:0] tacc_req_op =
        tacc_req_from_input ? mex_op : op_reg;
    wire [2:0] tacc_req_funct =
        tacc_req_from_input ? mex_funct : funct_reg;
    wire [7:0] tacc_req_funct_byte =
        tacc_req_from_input ? mex_funct_byte : funct_byte_reg;
    wire [3:0] tacc_req_ext_mod =
        tacc_req_from_input ? mex_ext_mod : ext_mod_reg;
    wire tacc_req_ext_active =
        tacc_req_from_input ? mex_ext_active : ext_active_reg;
    wire [4:0] tacc_req_caller_id =
        tacc_req_from_input ? mex_caller_id : caller_id_reg;
    wire [1:0] tacc_req_caller_slot =
        tacc_req_from_input ? mex_caller_slot : caller_slot_reg;
    wire tacc_req_is_tamac =
        (tacc_req_op == MEX_TMUL) &&
        ((tacc_req_funct == TMUL_TAMAC) ||
         (tacc_req_funct_byte[2:0] == TMUL_TAMAC));
    wire tacc_req_canonical =
        tacc_req_is_tamac ?
            ((tacc_req_op == MEX_TMUL) &&
             (tacc_req_funct == TMUL_TAMAC) &&
             (tacc_req_funct_byte == {5'd0, TMUL_TAMAC}) &&
             (tacc_req_ss != 2'd2) && !tacc_req_ext_active) :
            ((tacc_req_op == MEX_TSYS) && tacc_req_ext_active &&
             (tacc_req_ext_mod == 4'd8) && (tacc_req_ss == 2'd0) &&
             (tacc_req_funct_byte == {5'd0, tacc_req_funct}) &&
             (tacc_req_funct >= ETSYS_TACC_TRY) &&
             (tacc_req_funct <= ETSYS_TACC_RESERVED));
    wire [3:0] tacc_req_format_ew =
        tacc_req_from_input ? mode_ew : tamac_ew_reg;
    wire tacc_req_format_signed =
        tacc_req_from_input ? mode_signed : tamac_signed_reg;
    wire [63:0] tacc_req_image_addr =
        tacc_req_from_input ?
            ((tacc_req_funct == ETSYS_TACC_STORE) ? tdst : tsrc0) :
            tacc_image_addr_reg;
    wire [63:0] tacc_req_tamac_src_a =
        tacc_req_from_input ?
            ((tacc_req_ss == 2'd3) ? tdst : tsrc0) :
            tamac_src_a_addr_reg;
    wire [63:0] tacc_req_tamac_src_b =
        tacc_req_from_input ?
            ((tacc_req_ss == 2'd0) ? tsrc1 : tsrc0) :
            tamac_src_b_addr_reg;
    wire tacc_req_tamac_has_b = tacc_req_ss != 2'd1;
    wire tacc_req_priv =
        tacc_req_from_input ? mex_priv : priv_reg;
    wire [63:0] tacc_req_mpu_base =
        tacc_req_from_input ? mex_mpu_base : mpu_base_reg;
    wire [63:0] tacc_req_mpu_limit =
        tacc_req_from_input ? mex_mpu_limit : mpu_limit_reg;
    wire tacc_req_mpu_enabled =
        tacc_req_from_input ? mex_mpu_enabled : mpu_enabled_reg;
    wire tacc_req_image_operation =
        (tacc_req_funct == ETSYS_TACC_LOAD) ||
        (tacc_req_funct == ETSYS_TACC_STORE);
    localparam [63:0] TACC_MMIO_END =
        64'hFFFF_FF80_0000_0000;
    reg [2:0]  tacc_req_preflight_fault;
    reg [63:0] tacc_req_preflight_fault_addr;
    reg        tacc_req_image_ext;
    reg        tacc_req_image_hbw;
    reg [2:0]  tamac_src_a_preflight_fault;
    reg [63:0] tamac_src_a_preflight_fault_addr;
    reg        tamac_src_a_ext;
    reg        tamac_src_a_hbw;
    reg [2:0]  tamac_src_b_preflight_fault;
    reg [63:0] tamac_src_b_preflight_fault_addr;
    reg        tamac_src_b_ext;
    reg        tamac_src_b_hbw;

    // The task is pure combinational routing policy.  It deliberately does
    // not issue a memory request, so both TAMAC source spans can be checked
    // before the first source beat becomes visible.
    task tacc_preflight_span;
        input  [63:0] address;
        input  [8:0]  span_bytes;
        input         caller_priv;
        input  [63:0] caller_mpu_base;
        input  [63:0] caller_mpu_limit;
        input         caller_mpu_enabled;
        output [2:0]  fault;
        output [63:0] fault_address;
        output        routed_ext;
        output        routed_hbw;
        reg [64:0] span_end;
        begin
            fault         = MEX_FAULT_NONE;
            fault_address = 64'd0;
            routed_ext    = 1'b0;
            routed_hbw    = 1'b0;
            span_end      = {1'b0, address} + span_bytes;

            if (span_end[64]) begin
                fault         = MEX_FAULT_BUS;
                fault_address = 64'd0;
            end else if (address[63:32] == MP64_SPAD_HI) begin
                // The shared tile-memory port has no cluster-local scratchpad
                // route.  Reject the sentinel aperture before any traffic
                // even when the captured caller may use it for scalar loads.
                fault         = MEX_FAULT_BUS;
                fault_address = address;
            end else if ((address < TACC_MMIO_END) &&
                         (span_end[63:0] > MP64_MMIO_BASE)) begin
                fault = MEX_FAULT_BUS;
                fault_address =
                    (address < MP64_MMIO_BASE) ?
                    MP64_MMIO_BASE : address;
            end else if (address < TACC_BANK0_LIMIT) begin
                if (span_end > {1'b0, TACC_BANK0_LIMIT}) begin
                    fault         = MEX_FAULT_BUS;
                    fault_address = TACC_BANK0_LIMIT;
                end
            end else if ((address >= {32'd0, MP64_EXT_MEM_BASE}) &&
                         (address < TACC_EXT_LIMIT)) begin
                routed_ext = 1'b1;
                if (span_end > {1'b0, TACC_EXT_LIMIT}) begin
                    fault         = MEX_FAULT_BUS;
                    fault_address = TACC_EXT_LIMIT;
                end
            end else if ((address >= TACC_VRAM_BASE) &&
                         (address < TACC_VRAM_LIMIT)) begin
                routed_ext = 1'b1;
                if (span_end > {1'b0, TACC_VRAM_LIMIT}) begin
                    fault         = MEX_FAULT_BUS;
                    fault_address = TACC_VRAM_LIMIT;
                end
            end else if ((address >= {32'd0, MP64_HBW_BASE_ADDR}) &&
                         (address < TACC_HBW_LIMIT)) begin
                routed_hbw = 1'b1;
                if (span_end > {1'b0, TACC_HBW_LIMIT}) begin
                    fault         = MEX_FAULT_BUS;
                    fault_address = TACC_HBW_LIMIT;
                end
            end else begin
                fault         = MEX_FAULT_BUS;
                fault_address = address;
            end

            if ((fault == MEX_FAULT_NONE) &&
                caller_priv && routed_hbw) begin
                fault         = MEX_FAULT_PRIV;
                fault_address = address;
            end else if ((fault == MEX_FAULT_NONE) &&
                         caller_priv && caller_mpu_enabled) begin
                if ((address < caller_mpu_base) ||
                    (address >= caller_mpu_limit)) begin
                    fault         = MEX_FAULT_PRIV;
                    fault_address = address;
                end else if (span_end >
                             {1'b0, caller_mpu_limit}) begin
                    fault         = MEX_FAULT_PRIV;
                    fault_address = caller_mpu_limit;
                end
            end
        end
    endtask

    // Image and source preflight are deliberately combinational and complete:
    // no BUSY, stage ownership, or source read is visible until every required
    // byte has one legal route under the captured caller's policy.
    always @(*) begin
        tacc_req_preflight_fault      = MEX_FAULT_NONE;
        tacc_req_preflight_fault_addr = 64'd0;
        tacc_req_image_ext            = 1'b0;
        tacc_req_image_hbw            = 1'b0;

        if (tacc_req_image_operation) begin
            if (tacc_req_image_addr[5:0] != 6'd0) begin
                tacc_req_preflight_fault = MEX_FAULT_ALIGN;
                tacc_req_preflight_fault_addr = tacc_req_image_addr;
            end else begin
                tacc_preflight_span(
                    tacc_req_image_addr, 9'd256, tacc_req_priv,
                    tacc_req_mpu_base, tacc_req_mpu_limit,
                    tacc_req_mpu_enabled,
                    tacc_req_preflight_fault,
                    tacc_req_preflight_fault_addr,
                    tacc_req_image_ext, tacc_req_image_hbw);
            end
        end

        tamac_src_a_preflight_fault      = MEX_FAULT_NONE;
        tamac_src_a_preflight_fault_addr = 64'd0;
        tamac_src_a_ext                  = 1'b0;
        tamac_src_a_hbw                  = 1'b0;
        tamac_src_b_preflight_fault      = MEX_FAULT_NONE;
        tamac_src_b_preflight_fault_addr = 64'd0;
        tamac_src_b_ext                  = 1'b0;
        tamac_src_b_hbw                  = 1'b0;

        if (tacc_req_is_tamac) begin
            // A source row is a physical 512-bit request, not a byte-addressed
            // assembly operation.  Give ALIGN deterministic priority across
            // every required operand, then validate every routed span before
            // tacc_tamac_start can expose the first request.
            if (tacc_req_tamac_src_a[5:0] != 6'd0) begin
                tacc_req_preflight_fault = MEX_FAULT_ALIGN;
                tacc_req_preflight_fault_addr =
                    tacc_req_tamac_src_a;
            end else if (tacc_req_tamac_has_b &&
                         (tacc_req_tamac_src_b[5:0] != 6'd0)) begin
                tacc_req_preflight_fault = MEX_FAULT_ALIGN;
                tacc_req_preflight_fault_addr =
                    tacc_req_tamac_src_b;
            end else begin
                tacc_preflight_span(
                    tacc_req_tamac_src_a, 9'd64, tacc_req_priv,
                    tacc_req_mpu_base, tacc_req_mpu_limit,
                    tacc_req_mpu_enabled,
                    tamac_src_a_preflight_fault,
                    tamac_src_a_preflight_fault_addr,
                    tamac_src_a_ext, tamac_src_a_hbw);
                if (tacc_req_tamac_has_b)
                    tacc_preflight_span(
                        tacc_req_tamac_src_b, 9'd64, tacc_req_priv,
                        tacc_req_mpu_base, tacc_req_mpu_limit,
                        tacc_req_mpu_enabled,
                        tamac_src_b_preflight_fault,
                        tamac_src_b_preflight_fault_addr,
                        tamac_src_b_ext, tamac_src_b_hbw);

                if (tamac_src_a_preflight_fault != MEX_FAULT_NONE) begin
                    tacc_req_preflight_fault =
                        tamac_src_a_preflight_fault;
                    tacc_req_preflight_fault_addr =
                        tamac_src_a_preflight_fault_addr;
                end else if (tacc_req_tamac_has_b &&
                             (tamac_src_b_preflight_fault !=
                              MEX_FAULT_NONE)) begin
                    tacc_req_preflight_fault =
                        tamac_src_b_preflight_fault;
                    tacc_req_preflight_fault_addr =
                        tamac_src_b_preflight_fault_addr;
                end
            end
        end
    end

    wire tacc_req_cancel =
        tacc_req_from_input ? incoming_cancelled : active_cancelled;
    wire tacc_req_ready;
    wire tacc_req_done;
    wire tacc_req_busy;
    wire [2:0] tacc_req_fault;
    wire [63:0] tacc_req_fault_addr;
    wire [2047:0] tacc_bank_state;
    wire tacc_tamac_start;
    wire tamac_terminal;
    wire [2:0] tamac_terminal_fault;
    wire [63:0] tamac_terminal_fault_addr;
    wire [2047:0] tamac_result_image;
    mp64_tacc #(
        .CALLER_BASE (TACC_CALLER_BASE),
        .CALLER_COUNT(TACC_CALLER_COUNT)
    ) u_tacc (
        .clk                   (clk),
        .rst_n                 (rst_n),
        .engine_reset          (engine_reset),
        .req_valid             (tacc_req_valid),
        .req_ready             (tacc_req_ready),
        .req_is_tamac          (tacc_req_is_tamac),
        .req_funct             (tacc_req_funct),
        .req_canonical         (tacc_req_canonical),
        .req_caller_id         (tacc_req_caller_id),
        .req_caller_slot       (tacc_req_caller_slot),
        .req_format_ew         (tacc_req_format_ew),
        .req_format_signed     (tacc_req_format_signed),
        .req_image_addr        (tacc_req_image_addr),
        .req_preflight_fault   (tacc_req_preflight_fault),
        .req_preflight_fault_addr(tacc_req_preflight_fault_addr),
        .req_cancel            (tacc_req_cancel),
        .req_retire            (mex_retire),
        .req_done              (tacc_req_done),
        .req_busy              (tacc_req_busy),
        .req_fault             (tacc_req_fault),
        .req_fault_addr        (tacc_req_fault_addr),
        .tamac_start           (tacc_tamac_start),
        .tamac_done            (tamac_terminal),
        .tamac_fault           (tamac_terminal_fault),
        .tamac_fault_addr      (tamac_terminal_fault_addr),
        .tamac_result_image    (tamac_result_image),
        .xfer_req              (tacc_xfer_req),
        .xfer_store            (tacc_xfer_store),
        .xfer_base             (tacc_xfer_base),
        .xfer_format_ew        (tacc_xfer_format_ew),
        .xfer_token            (tacc_xfer_token),
        .xfer_store_image      (tacc_xfer_store_image),
        .xfer_cancel           (tacc_xfer_cancel),
        .xfer_finish           (tacc_xfer_finish),
        .xfer_done             (tacc_xfer_done),
        .xfer_response_token   (tacc_xfer_response_token),
        .xfer_fault            (tacc_xfer_fault),
        .xfer_fault_addr       (tacc_xfer_fault_addr),
        .xfer_load_image       (tacc_xfer_load_image),
        .force_valid           (tacc_ctl_valid),
        .force_ready           (),
        .force_priv            (tacc_ctl_priv),
        .force_wdata           (tacc_ctl_wdata),
        .force_caller_id       (tacc_ctl_caller_id),
        .force_done            (tacc_ctl_done),
        .force_fault           (tacc_ctl_fault),
        .status_raw            (tacc_status_raw),
        .bank_state            (tacc_bank_state)
    );

    assign tacc_xfer_ext = tacc_req_image_ext;

    assign mex_done_internal = mex_done_reg | tacc_req_done;
    assign mex_fault_internal =
        tacc_req_done ? tacc_req_fault :
        (mex_done_reg ? mex_fault_reg : MEX_FAULT_NONE);

    assign mex_done = mex_done_internal;
    // Accumulator publications update FLAGS.Z at completion; element-wise,
    // system, and TACC operations leave it unchanged.
    assign mex_zero_valid = mex_done_reg && mex_zero_valid_reg;
    assign mex_zero = mex_zero_reg;
    assign mex_busy = mex_busy_reg && !tacc_req_done;
    assign mex_fault = mex_fault_internal;
    assign mex_fault_addr =
        tacc_req_done ? tacc_req_fault_addr : mex_fault_addr_reg;

    // Count only cycles in which the leaf is genuinely blocked on a target
    // acknowledgement.  Dispatch, compute, and fixed completion cycles are
    // useful work and remain part of the architectural base latency.
    always @(*) begin
        mex_stall_cycle = 1'b0;
        if (!engine_reset && !active_cancelled) begin
            case (state)
                S_LOAD_A, S_LOAD_B, S_LOAD_C,
                S_STORE2, S_STORE_WAIT, S_STORE2_WAIT,
                S_LOAD2D_WAIT, S_STORE2D_WAIT,
                S_STORE2D_WRITE_WAIT:
                    mex_stall_cycle = !tile_ack;
                S_EXT_LOAD_A, S_EXT_LOAD_B, S_EXT_STORE,
                S_EXT_STORE2_WAIT:
                    mex_stall_cycle = !ext_tile_ack;
                S_CVT_READ_WAIT, S_CVT_WRITE_WAIT:
                    mex_stall_cycle = cvt_ext ? !ext_tile_ack : !tile_ack;
                S_TAMAC_LOAD_A, S_TAMAC_LOAD_B:
                    mex_stall_cycle =
                        tamac_read_ext_reg ?
                        !ext_tile_ack : !tile_ack;
                S_TACC_WAIT:
                    mex_stall_cycle = !tacc_req_busy && !tacc_req_done;
                default:
                    mex_stall_cycle = 1'b0;
            endcase
        end
    end

    // LOAD2D / STORE2D FSM registers
    reg [63:0]  ld2d_base;       // starting memory address
    reg [63:0]  ld2d_stride;     // effective row stride in bytes
    reg [3:0]   ld2d_row;        // current row counter
    reg [6:0]   ld2d_off;        // byte offset in result tile
    reg [3:0]   ld2d_h;          // number of rows
    reg [6:0]   ld2d_w;          // bytes per row
    reg [63:0]  ld2d_row_addr;   // computed row address (cached)

    // Operand A comes from [TDST] for in-place sources and from [TSRC0]
    // otherwise; system operations keep their own addressing.  An immediate
    // source replaces operand A with the splat after [TSRC0] is read into B
    // (docs/tile-engine.md, "Source Selection Modes").
    wire in_place_source = (mex_ss == 2'd3) && (mex_op != MEX_TSYS);
    wire [63:0] mex_src_a_addr = in_place_source ? tdst : tsrc0;
    wire mex_src_a_internal = in_place_source ? dst_internal : src0_internal;

    // Source B selection
    reg [511:0] src_b_selected;
    reg [511:0] gpr_broadcast;
    wire tamac_datapath_active = state == S_TACC_INT;
    wire [3:0] broadcast_mode_ew =
        tamac_datapath_active ? tamac_ew_reg : mode_ew;
    // Float formats take the unsigned immediate converted exactly to the lane
    // format (docs/floating-point.md §5.1); every value 0-255 is exact.
    function [63:0] fp_from_u8;
        input [3:0] ew;
        input [7:0] value;
        integer lead;
        integer k;
        reg [63:0] exponent;
        reg [63:0] widened;
        begin
            lead = -1;
            for (k = 0; k < 8; k = k + 1)
                if (value[k])
                    lead = k;
            widened = {56'd0, value};
            if (lead < 0) begin
                fp_from_u8 = 64'd0;
            end else case (ew)
                TMODE_BF16: begin
                    exponent   = 127 + lead;
                    fp_from_u8 = (exponent << 7) |
                                 ((widened << (7 - lead)) & 64'h7F);
                end
                TMODE_FP32: begin
                    exponent   = 127 + lead;
                    fp_from_u8 = (exponent << 23) |
                                 ((widened << (23 - lead)) & 64'h7F_FFFF);
                end
                TMODE_FP64: begin
                    exponent   = 1023 + lead;
                    fp_from_u8 = (exponent << 52) |
                                 ((widened << (52 - lead)) &
                                  64'h000F_FFFF_FFFF_FFFF);
                end
                default: begin
                    exponent   = 15 + lead;
                    fp_from_u8 = (exponent << 10) |
                                 ((widened << (10 - lead)) & 64'h3FF);
                end
            endcase
        end
    endfunction

    wire [63:0] fp_imm_lane = fp_from_u8(mode_ew, imm8_reg);
    reg  [511:0] imm_splat;
    always @(*) begin
        if (!mode_fp)
            imm_splat = {64{imm8_reg}};
        else case (lane_ew)
            2'd1:    imm_splat = {32{fp_imm_lane[15:0]}};
            2'd2:    imm_splat = {16{fp_imm_lane[31:0]}};
            default: imm_splat = {8{fp_imm_lane}};
        endcase
    end

    always @(*) begin
        // Broadcast replicates the low lane-width bits of Rn as raw bits.
        case (tile_format_lane_log2(broadcast_mode_ew))
            2'd0: gpr_broadcast = {64{gpr_val_reg[7:0]}};
            2'd1: gpr_broadcast = {32{gpr_val_reg[15:0]}};
            2'd2: gpr_broadcast = {16{gpr_val_reg[31:0]}};
            2'd3: gpr_broadcast = {8{gpr_val_reg[63:0]}};
        endcase

        if (tamac_datapath_active && ss_reg == 2'd3)
            // TAMAC in-place is [TDST] x [TSRC0]; tile_a is source A,
            // while the ordinary SS3 convention would incorrectly reuse it
            // as source B.
            src_b_selected = tile_b;
        else begin
            case (ss_reg)
                2'd0: src_b_selected = tile_b;
                2'd1: src_b_selected = gpr_broadcast;
                2'd2: src_b_selected = tile_b;      // [TSRC0]; A is the splat
                2'd3: src_b_selected = tile_b;      // [TSRC0]; A is [TDST]
                default: src_b_selected = 512'd0;
            endcase
        end
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
        if (mode_wide_fp)
            alu_result_muxed = fp_wide_alu_result;
        else if (mode_fp)
            alu_result_muxed = fp_alu_result;
        else case (lane_ew)
            2'd0: alu_result_muxed = alu_result_8;
            2'd1: alu_result_muxed = alu_result_16;
            2'd2: alu_result_muxed = alu_result_32;
            2'd3: alu_result_muxed = alu_result_64;
        endcase
    end

    // FP16/BF16 lane results (assembled below the exact-product array).
    reg [511:0] fp_alu_result;
    reg [511:0] fp_mul_result;
    reg [511:0] fp_mac_result;
    genvar fpl;

    // ========================================================================
    // Exact FP16/BF16 product array
    // ========================================================================
    //
    // WMUL, DOT/DOTACC, SUMSQ, and floating TAMAC all consume this one
    // 32-lane 11x11-or-8x8 multiplier array.  The exact descriptor remains
    // available beside the independently rounded binary32 WMUL view, so TACC
    // never inserts an intermediate half- or binary32-rounding point.
    reg  [511:0] fp_wmul_lo, fp_wmul_hi;
    wire [31:0]  fp_wmul_fp32 [0:31];
    wire         fp_product_nan [0:31];
    wire         fp_product_inf [0:31];
    wire         fp_product_zero [0:31];
    wire         fp_product_finite [0:31];
    wire         fp_product_sign [0:31];
    wire [21:0]  fp_product_significand [0:31];
    wire signed [10:0] fp_product_exponent [0:31];
    wire fp_product_square_mode =
        (state == S_REDUCE) && (op_reg == MEX_TRED) &&
        (funct_reg == TRED_SUMSQ);
    wire fp_product_is_bf16 =
        tamac_datapath_active ?
        (tamac_ew_reg == TMODE_BF16) : mode_bf16;

    generate
        for (fpl = 0; fpl < 32; fpl = fpl + 1) begin : fp_wmul_lanes
            wire [15:0] product_b =
                fp_product_square_mode ?
                tile_a[fpl*16 +: 16] :
                src_b_selected[fpl*16 +: 16];
            mp64_fp16_bf16_exact_product u_exact_product (
                .is_bf16           (fp_product_is_bf16),
                .a                  (tile_a[fpl*16 +: 16]),
                .b                  (product_b),
                .product_nan        (fp_product_nan[fpl]),
                .product_inf        (fp_product_inf[fpl]),
                .product_zero       (fp_product_zero[fpl]),
                .product_finite     (fp_product_finite[fpl]),
                .product_sign       (fp_product_sign[fpl]),
                .product_significand(fp_product_significand[fpl]),
                .product_exponent   (fp_product_exponent[fpl]),
                .rounded_fp32       (fp_wmul_fp32[fpl])
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
    // FP16/BF16 TALU/TMUL lanes (docs/floating-point.md §5)
    // ========================================================================
    //
    // One IEEE-correct lane per 16-bit element.  Products come from the exact
    // array above; MAC and FMA are the same fused operation with [TDST] as
    // the addend.
    localparam [2:0] FP_LANE_ADD = 3'd0;
    localparam [2:0] FP_LANE_SUB = 3'd1;
    localparam [2:0] FP_LANE_MUL = 3'd2;
    localparam [2:0] FP_LANE_FMA = 3'd3;
    localparam [2:0] FP_LANE_MIN = 3'd4;
    localparam [2:0] FP_LANE_MAX = 3'd5;
    localparam [2:0] FP_LANE_ABS = 3'd6;

    reg [2:0] fp_lane_op;
    always @(*) begin
        if (op_reg == MEX_TMUL)
            fp_lane_op = (funct_reg == TMUL_MAC || funct_reg == TMUL_FMA) ?
                         FP_LANE_FMA : FP_LANE_MUL;
        else case (funct_reg)
            TALU_SUB: fp_lane_op = FP_LANE_SUB;
            TALU_MIN: fp_lane_op = FP_LANE_MIN;
            TALU_MAX: fp_lane_op = FP_LANE_MAX;
            TALU_ABS: fp_lane_op = FP_LANE_ABS;
            default:  fp_lane_op = FP_LANE_ADD;
        endcase
    end

    wire [15:0] fp_lane_out [0:31];
    generate
        for (fpl = 0; fpl < 32; fpl = fpl + 1) begin : fp_half_lanes
            mp64_fp_half_lane u_lane (
                .is_bf16            (mode_bf16),
                .op                 (fp_lane_op),
                .a                  (tile_a[fpl*16 +: 16]),
                .b                  (src_b_selected[fpl*16 +: 16]),
                .c                  (tile_c[fpl*16 +: 16]),
                .product_nan        (fp_product_nan[fpl]),
                .product_inf        (fp_product_inf[fpl]),
                .product_zero       (fp_product_zero[fpl]),
                .product_finite     (fp_product_finite[fpl]),
                .product_sign       (fp_product_sign[fpl]),
                .product_significand(fp_product_significand[fpl]),
                .product_exponent   (fp_product_exponent[fpl]),
                .product_fp32       (fp_wmul_fp32[fpl]),
                .result             (fp_lane_out[fpl])
            );
        end
    endgenerate

    integer fp_lane_i;
    always @(*) begin
        fp_alu_result = 512'd0;
        fp_mul_result = 512'd0;
        fp_mac_result = 512'd0;
        for (fp_lane_i = 0; fp_lane_i < 32; fp_lane_i = fp_lane_i + 1) begin
            fp_mul_result[fp_lane_i*16 +: 16] = fp_lane_out[fp_lane_i];
            fp_mac_result[fp_lane_i*16 +: 16] = fp_lane_out[fp_lane_i];
            case (funct_reg)
                TALU_AND:
                    fp_alu_result[fp_lane_i*16 +: 16] =
                        tile_a[fp_lane_i*16 +: 16] &
                        src_b_selected[fp_lane_i*16 +: 16];
                TALU_OR:
                    fp_alu_result[fp_lane_i*16 +: 16] =
                        tile_a[fp_lane_i*16 +: 16] |
                        src_b_selected[fp_lane_i*16 +: 16];
                TALU_XOR:
                    fp_alu_result[fp_lane_i*16 +: 16] =
                        tile_a[fp_lane_i*16 +: 16] ^
                        src_b_selected[fp_lane_i*16 +: 16];
                default:
                    fp_alu_result[fp_lane_i*16 +: 16] =
                        fp_lane_out[fp_lane_i];
            endcase
        end
    end

    // ========================================================================
    // FP32/FP64 element-wise datapath (docs/floating-point.md §5, §10)
    // ========================================================================
    //
    // TALU MIN, MAX, ABS, AND, OR, and XOR are combinational over every lane.
    // MIN and MAX are IEEE 754-2019 minimum and maximum: a NaN gives the
    // canonical NaN and -0 orders below +0 (§3.8).
    function [63:0] fp_wide_lane_alu;
        input        is64;
        input [2:0]  funct;
        input [63:0] a;
        input [63:0] b;
        reg   [63:0] width_mask;
        reg   [63:0] sign_bit;
        reg   [63:0] magnitude;
        reg   [63:0] infinity;
        reg   [63:0] quiet_nan;
        reg   [63:0] key_a;
        reg   [63:0] key_b;
        reg          any_nan;
        begin
            width_mask = is64 ? 64'hFFFF_FFFF_FFFF_FFFF : 64'h0000_0000_FFFF_FFFF;
            sign_bit   = is64 ? 64'h8000_0000_0000_0000 : 64'h0000_0000_8000_0000;
            magnitude  = width_mask ^ sign_bit;
            infinity   = is64 ? 64'h7FF0_0000_0000_0000 : 64'h0000_0000_7F80_0000;
            quiet_nan  = is64 ? 64'h7FF8_0000_0000_0000 : 64'h0000_0000_7FC0_0000;
            any_nan    = ((a & magnitude) > infinity) ||
                         ((b & magnitude) > infinity);
            // Order keys: negative encodings reverse below positive ones.
            key_a = (a & sign_bit) ? (width_mask ^ a) : (a | sign_bit);
            key_b = (b & sign_bit) ? (width_mask ^ b) : (b | sign_bit);
            case (funct)
                TALU_AND: fp_wide_lane_alu = a & b;
                TALU_OR:  fp_wide_lane_alu = a | b;
                TALU_XOR: fp_wide_lane_alu = a ^ b;
                TALU_MIN: fp_wide_lane_alu =
                    any_nan ? quiet_nan : ((key_a <= key_b) ? a : b);
                TALU_MAX: fp_wide_lane_alu =
                    any_nan ? quiet_nan : ((key_a >= key_b) ? a : b);
                TALU_ABS: fp_wide_lane_alu = a & magnitude;
                default:  fp_wide_lane_alu = 64'd0;  // ADD/SUB use the FMA units
            endcase
        end
    endfunction

    reg [511:0] fp_wide_alu_result;
    reg [63:0]  fp_wide_lane32;
    integer fwl;
    always @(*) begin
        fp_wide_alu_result = 512'd0;
        fp_wide_lane32 = 64'd0;
        if (mode_fp64) begin
            for (fwl = 0; fwl < 8; fwl = fwl + 1)
                fp_wide_alu_result[fwl*64 +: 64] = fp_wide_lane_alu(
                    1'b1, funct_reg, tile_a[fwl*64 +: 64],
                    src_b_selected[fwl*64 +: 64]);
        end else begin
            for (fwl = 0; fwl < 16; fwl = fwl + 1) begin
                fp_wide_lane32 = fp_wide_lane_alu(
                    1'b0, funct_reg, {32'd0, tile_a[fwl*32 +: 32]},
                    {32'd0, src_b_selected[fwl*32 +: 32]});
                fp_wide_alu_result[fwl*32 +: 32] = fp_wide_lane32[31:0];
            end
        end
    end

    // TALU ADD and SUB and TMUL MUL, MAC, FMA, and WMUL run on FMA_UNITS
    // multi-format FMA units over FMA_BEATS beats of one cycle.  A beat gives
    // each unit one binary64 lane, or two adjacent binary32 lanes.  ADD is
    // a * 1 + b, SUB is a * 1 + (-b), MUL and WMUL are a * b + (-0), and MAC
    // and FMA are a * b + [TDST].  WMUL multiplies binary32 lanes into exact
    // binary64 products: lanes 0-7 go to [TDST] and 8-15 to [TDST+64].
    //
    // SUM, L1, SUMSQ, DOT, and DOTACC run the canonical tree (§4.3) on the
    // same units in S_TREE, over binary64 values in tree_v.  The product
    // phase forms the DOT/SUMSQ leaves as WMUL-shaped (FP32) or MUL-shaped
    // (FP64) beats; SUM and L1 leaves are the lanes widened exactly.  Each
    // tree level then takes ceil(nodes / FMA_UNITS) beats of binary64
    // a * 1 + b adds, pairing in lane order, down to one value (four DOTACC
    // chunk values).  A final phase reserves ceil(values / FMA_UNITS) beats
    // for the ACC_ACC adds whether or not ACC_ACC is set (§10).
    localparam integer FMA_BEATS = 8 / FMA_UNITS;
    localparam [1:0] TREE_PRODUCTS = 2'd0;
    localparam [1:0] TREE_LEVELS   = 2'd1;
    localparam [1:0] TREE_ACC      = 2'd2;
    reg [1023:0] tree_v;
    reg [1:0]    tree_phase;
    reg [4:0]    tree_count;       // values in the current level
    reg [2:0]    tree_target;      // 1, or 4 for DOTACC
    reg          tree_accumulate;  // ACC_ACC without ACC_ZERO
    wire tree_add_phase  = (state == S_TREE) && (tree_phase != TREE_PRODUCTS);
    // FP32/FP64 TAMAC: each unit adds one exact product to one binary64
    // accumulator lane per beat (docs/floating-point.md §7).
    wire tamac_fma_phase = (state == S_TACC_INT) &&
        ((tamac_ew_reg == TMODE_FP32) || (tamac_ew_reg == TMODE_FP64));
    wire tamac_fma_fp64  = (tamac_ew_reg == TMODE_FP64);
    wire tree_square     = (op_reg == MEX_TRED);  // SUMSQ squares operand A

`ifndef SYNTHESIS
    initial begin
        if (FMA_UNITS != 1 && FMA_UNITS != 2 &&
            FMA_UNITS != 4 && FMA_UNITS != 8)
            $fatal(1, "mp64_tile: FMA_UNITS must be 1, 2, 4, or 8");
    end
`endif

    reg  [3:0] fma_beat;
    integer    fma_i;
    integer    fma_j;
    wire fma_is_wmul     = (op_reg == MEX_TMUL) && (funct_reg == TMUL_WMUL);
    wire fma_is_talu     = (op_reg == MEX_TALU);
    wire fma_uses_addend = (op_reg == MEX_TMUL) &&
                           ((funct_reg == TMUL_MAC) || (funct_reg == TMUL_FMA));
    wire fma_out64       = mode_fp64 || fma_is_wmul || (state == S_TREE);
    wire fma_op = mode_wide_fp &&
        !(ext_active_reg && (ext_mod_reg == 4'd8)) &&
        ((fma_is_talu &&
          ((funct_reg == TALU_ADD) || (funct_reg == TALU_SUB))) ||
         ((op_reg == MEX_TMUL) &&
          ((funct_reg == TMUL_MUL) || fma_is_wmul || fma_uses_addend)));
    wire fma_negate_b = fma_is_talu && (funct_reg == TALU_SUB);

    wire [64*FMA_UNITS-1:0] fma_r0_bus;
    wire [64*FMA_UNITS-1:0] fma_r1_bus;
    genvar fmu;
    generate
        for (fmu = 0; fmu < FMA_UNITS; fmu = fmu + 1) begin : fma_units
            wire [3:0] lane64 = fma_beat * FMA_UNITS + fmu;
            wire [3:0] lane32 = fma_beat * (2 * FMA_UNITS) + 2 * fmu;

            wire [63:0] a0_lane = mode_fp64 ? tile_a[lane64[2:0]*64 +: 64]
                                            : {32'd0, tile_a[lane32*32 +: 32]};
            wire [63:0] b0_lane = tree_square ? a0_lane :
                mode_fp64 ? src_b_selected[lane64[2:0]*64 +: 64] :
                {32'd0, src_b_selected[lane32*32 +: 32]};
            wire [63:0] c0_lane = mode_fp64 ? tile_c[lane64[2:0]*64 +: 64]
                                            : {32'd0, tile_c[lane32*32 +: 32]};
            wire [31:0] a1_lane = tile_a[(lane32 + 1)*32 +: 32];
            wire [31:0] b1_lane = tree_square ? a1_lane :
                                  src_b_selected[(lane32 + 1)*32 +: 32];
            wire [31:0] c1_lane = tile_c[(lane32 + 1)*32 +: 32];

            wire [63:0] one0      = mode_fp64 ? 64'h3FF0_0000_0000_0000
                                              : 64'h0000_0000_3F80_0000;
            wire [63:0] sign0     = mode_fp64 ? 64'h8000_0000_0000_0000
                                              : 64'h0000_0000_8000_0000;
            wire [63:0] neg_zero0 = fma_out64 ? 64'h8000_0000_0000_0000
                                              : 64'h0000_0000_8000_0000;

            wire [63:0] b0 = fma_is_talu ? one0 : b0_lane;
            wire [63:0] c0 =
                fma_is_talu     ? (fma_negate_b ? (b0_lane ^ sign0) : b0_lane) :
                fma_uses_addend ? c0_lane : neg_zero0;
            wire [31:0] b1 = fma_is_talu ? 32'h3F80_0000 : b1_lane;
            wire [31:0] c1 =
                fma_is_talu     ? (fma_negate_b ? (b1_lane ^ 32'h8000_0000)
                                                : b1_lane) :
                fma_uses_addend ? c1_lane : 32'h8000_0000;

            // Tree node lane64 adds tree_v[2j] and tree_v[2j+1]; the ACC_ACC
            // phase adds ACC[j] and tree_v[j].
            wire [2:0]  node   = lane64[2:0];
            wire [63:0] tree_a = (tree_phase == TREE_ACC) ?
                legacy_acc_state[node[1:0]*64 +: 64] :
                tree_v[(2*node)*64 +: 64];
            wire [63:0] tree_c = (tree_phase == TREE_ACC) ?
                tree_v[node*64 +: 64] :
                tree_v[(2*node + 1)*64 +: 64];

            wire [3:0]  tamac_lane = tamac_beat_reg * FMA_UNITS + fmu;
            wire [63:0] tamac_a = tamac_fma_fp64 ?
                tile_a[tamac_lane[2:0]*64 +: 64] :
                {32'd0, tile_a[tamac_lane*32 +: 32]};
            wire [63:0] tamac_b = tamac_fma_fp64 ?
                src_b_selected[tamac_lane[2:0]*64 +: 64] :
                {32'd0, src_b_selected[tamac_lane*32 +: 32]};
            wire [63:0] tamac_c = tacc_bank_state[tamac_lane*64 +: 64];

            mp64_fma_unit u_fma (
                .in64 (tamac_fma_phase ? tamac_fma_fp64 :
                       tree_add_phase  ? 1'b1 : mode_fp64),
                .out64(tamac_fma_phase || fma_out64),
                .a0   (tamac_fma_phase ? tamac_a :
                       tree_add_phase  ? tree_a : a0_lane),
                .b0   (tamac_fma_phase ? tamac_b :
                       tree_add_phase  ? 64'h3FF0_0000_0000_0000 : b0),
                .c0   (tamac_fma_phase ? tamac_c :
                       tree_add_phase  ? tree_c : c0),
                .a1   (a1_lane),
                .b1   (b1),
                .c1   (c1),
                .r0   (fma_r0_bus[fmu*64 +: 64]),
                .r1   (fma_r1_bus[fmu*64 +: 64])
            );
        end
    endgenerate

    // FP32/FP64 TRED MIN, MAX, MINIDX, and MAXIDX are combinational: the
    // NaN-skipping extreme (-0 below +0, lowest index on ties) widened
    // exactly to binary64, or index 0 and the canonical NaN for an all-NaN
    // tile (§3.8, §4.5).  Under ACC_ACC, MIN and MAX keep a running extreme
    // against ACC0, and MINIDX/MAXIDX replace ACC0/ACC1 only for a strictly
    // better value or a value over an old NaN.
    function fp64_is_nan;
        input [63:0] bits;
        fp64_is_nan = (bits[62:52] == 11'h7FF) && (bits[51:0] != 52'd0);
    endfunction

    function [63:0] fp64_order_key;
        input [63:0] bits;
        fp64_order_key = bits[63] ? ~bits : {1'b1, bits[62:0]};
    endfunction

    // Exact binary32 to binary64 widening; a NaN becomes the canonical NaN.
    function [63:0] fp32_to_fp64;
        input [31:0] bits;
        integer lead;
        integer k;
        reg [10:0] biased;
        reg [51:0] fraction;
        begin
            if (bits[30:23] == 8'hFF) begin
                fp32_to_fp64 = (bits[22:0] != 23'd0) ?
                    64'h7FF8_0000_0000_0000 : {bits[31], 11'h7FF, 52'd0};
            end else if (bits[30:23] == 8'd0) begin
                if (bits[22:0] == 23'd0) begin
                    fp32_to_fp64 = {bits[31], 63'd0};
                end else begin
                    lead = 0;
                    for (k = 0; k < 23; k = k + 1)
                        if (bits[k])
                            lead = k;
                    // bits * 2**-149 = 1.f * 2**(lead - 149)
                    biased   = 874 + lead;
                    fraction = {29'd0, bits[22:0]} << (52 - lead);
                    fp32_to_fp64 = {bits[31], biased, fraction};
                end
            end else begin
                fp32_to_fp64 = {bits[31], {3'd0, bits[30:23]} + 11'd896,
                                bits[22:0], 29'd0};
            end
        end
    endfunction

    reg [63:0] fpw_red_best;
    reg [63:0] fpw_red_idx;
    reg        fpw_red_found;
    integer    fpw_ri;
    always @(*) begin : fpw_red_block
        reg [63:0] candidate;
        reg        better;
        fpw_red_best  = 64'd0;
        fpw_red_idx   = 64'd0;
        fpw_red_found = 1'b0;
        candidate     = 64'd0;
        better        = 1'b0;
        for (fpw_ri = 0; fpw_ri < 16; fpw_ri = fpw_ri + 1) begin
            if (mode_fp64 && fpw_ri < 8)
                candidate = tile_a[fpw_ri*64 +: 64];
            else
                candidate = fp32_to_fp64(tile_a[fpw_ri*32 +: 32]);
            if ((!mode_fp64 || fpw_ri < 8) && !fp64_is_nan(candidate)) begin
                better = !fpw_red_found || (fp_red_largest ?
                    (fp64_order_key(candidate) > fp64_order_key(fpw_red_best)) :
                    (fp64_order_key(candidate) < fp64_order_key(fpw_red_best)));
                if (better) begin
                    fpw_red_best  = candidate;
                    fpw_red_idx   = fpw_ri;
                    fpw_red_found = 1'b1;
                end
            end
        end
    end

    wire [63:0] fpw_red_val = fpw_red_found ?
        fpw_red_best : 64'h7FF8_0000_0000_0000;
    wire fpw_red_beats_acc0 = fp_red_largest ?
        (fp64_order_key(fpw_red_val) > fp64_order_key(acc[0])) :
        (fp64_order_key(fpw_red_val) < fp64_order_key(acc[0]));
    wire [63:0] fpw_red_extreme_acc =
        fp64_is_nan(acc[0]) ?
            (fp64_is_nan(fpw_red_val) ? 64'h7FF8_0000_0000_0000
                                       : fpw_red_val) :
        (fp64_is_nan(fpw_red_val) || !fpw_red_beats_acc0) ?
            acc[0] : fpw_red_val;
    wire fpw_red_index_replaces =
        !fp64_is_nan(fpw_red_val) &&
        (fp64_is_nan(acc[1]) ||
         (fp_red_largest ?
          (fp64_order_key(fpw_red_val) > fp64_order_key(acc[1])) :
          (fp64_order_key(fpw_red_val) < fp64_order_key(acc[1]))));

    // ========================================================================
    // Shared FP32 reduction/TACC feedback bank
    // ========================================================================
    //
    // The sixteen first-level lanes are the only FP32 feedback bank in an
    // engine.  Legacy reductions select ordinary binary32+binary32 mode.
    // Floating TAMAC selects one exact-product group on arithmetic beats one
    // and three.  Beats zero and two are fixed product/staging intervals,
    // preserving the locked four-interval floating schedule.
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

    // SUM and L1 sum widened lanes (L1 after clearing the sign); DOT and
    // SUMSQ sum products rounded once to binary32.
    wire fp_reduction_sum_mode =
        (op_reg == MEX_TRED) &&
        (funct_reg == TRED_SUM || funct_reg == TRED_L1);
    wire fp_reduction_abs_mode =
        (op_reg == MEX_TRED) && (funct_reg == TRED_L1);
    wire [31:0] fp_reduction_leaf [0:31];
    wire [31:0] fp_shared_l1 [0:15];
    wire [31:0] fp_shared_l2 [0:7];
    wire [31:0] fp_shared_l3 [0:3];
    wire [31:0] fp_shared_l4 [0:1];
    wire [31:0] fp_shared_l5;
    reg          fp_tamac_product_nan_stage [0:15];
    reg          fp_tamac_product_inf_stage [0:15];
    reg          fp_tamac_product_zero_stage [0:15];
    reg          fp_tamac_product_finite_stage [0:15];
    reg          fp_tamac_product_sign_stage [0:15];
    reg [21:0]   fp_tamac_product_significand_stage [0:15];
    reg signed [10:0] fp_tamac_product_exponent_stage [0:15];
    integer fp_tamac_stage_lane;
    integer fp_tamac_result_lane;
    wire fp_tamac_feedback_active =
        tamac_datapath_active &&
        ((tamac_ew_reg == TMODE_FP16) ||
         (tamac_ew_reg == TMODE_BF16)) &&
        tamac_beat_reg[0];

    // Even floating beats register one group of exact descriptors; the
    // following odd beat feeds that stable group into the shared RNE bank.
    // This is a real multiplier/feedback timing boundary, not an idle cycle.
    always @(posedge clk) begin
        if (!rst_n || engine_reset) begin
            for (fp_tamac_stage_lane = 0;
                 fp_tamac_stage_lane < 16;
                 fp_tamac_stage_lane = fp_tamac_stage_lane + 1) begin
                fp_tamac_product_nan_stage[
                    fp_tamac_stage_lane] <= 1'b0;
                fp_tamac_product_inf_stage[
                    fp_tamac_stage_lane] <= 1'b0;
                fp_tamac_product_zero_stage[
                    fp_tamac_stage_lane] <= 1'b1;
                fp_tamac_product_finite_stage[
                    fp_tamac_stage_lane] <= 1'b0;
                fp_tamac_product_sign_stage[
                    fp_tamac_stage_lane] <= 1'b0;
                fp_tamac_product_significand_stage[
                    fp_tamac_stage_lane] <= 22'd0;
                fp_tamac_product_exponent_stage[
                    fp_tamac_stage_lane] <= 11'sd0;
            end
        end else if (!active_cancelled &&
                     tamac_datapath_active &&
                     ((tamac_ew_reg == TMODE_FP16) ||
                      (tamac_ew_reg == TMODE_BF16)) &&
                     !tamac_beat_reg[0]) begin
            for (fp_tamac_stage_lane = 0;
                 fp_tamac_stage_lane < 16;
                 fp_tamac_stage_lane = fp_tamac_stage_lane + 1) begin
                if (tamac_beat_reg[1]) begin
                    fp_tamac_product_nan_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_nan[fp_tamac_stage_lane+16];
                    fp_tamac_product_inf_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_inf[fp_tamac_stage_lane+16];
                    fp_tamac_product_zero_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_zero[fp_tamac_stage_lane+16];
                    fp_tamac_product_finite_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_finite[fp_tamac_stage_lane+16];
                    fp_tamac_product_sign_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_sign[fp_tamac_stage_lane+16];
                    fp_tamac_product_significand_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_significand[
                            fp_tamac_stage_lane+16];
                    fp_tamac_product_exponent_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_exponent[
                            fp_tamac_stage_lane+16];
                end else begin
                    fp_tamac_product_nan_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_nan[fp_tamac_stage_lane];
                    fp_tamac_product_inf_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_inf[fp_tamac_stage_lane];
                    fp_tamac_product_zero_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_zero[fp_tamac_stage_lane];
                    fp_tamac_product_finite_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_finite[fp_tamac_stage_lane];
                    fp_tamac_product_sign_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_sign[fp_tamac_stage_lane];
                    fp_tamac_product_significand_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_significand[fp_tamac_stage_lane];
                    fp_tamac_product_exponent_stage[
                        fp_tamac_stage_lane] <=
                        fp_product_exponent[fp_tamac_stage_lane];
                end
            end
        end
    end

    generate
        for (fpl = 0; fpl < 32; fpl = fpl + 1) begin : fp_reduce_leaf_mux
            assign fp_reduction_leaf[fpl] =
                !fp_reduction_sum_mode ? fp_wmul_fp32[fpl] :
                fp_reduction_abs_mode ?
                {1'b0, fp_tile_a_fp32[fpl][30:0]} :
                fp_tile_a_fp32[fpl];
        end

        for (fpl = 0; fpl < 16; fpl = fpl + 1) begin : fp_feedback_bank
            wire [31:0] tamac_accumulator =
                tamac_beat_reg[1] ?
                tacc_bank_state[(fpl+16)*32 +: 32] :
                tacc_bank_state[fpl*32 +: 32];
            mp64_fp32_feedback_rne u_feedback (
                .use_exact_product  (fp_tamac_feedback_active),
                .a                  (
                    fp_tamac_feedback_active ?
                    tamac_accumulator :
                    fp_reduction_leaf[fpl*2]),
                .b                  (fp_reduction_leaf[fpl*2+1]),
                .product_nan        (
                    fp_tamac_product_nan_stage[fpl]),
                .product_inf        (
                    fp_tamac_product_inf_stage[fpl]),
                .product_zero       (
                    fp_tamac_product_zero_stage[fpl]),
                .product_finite     (
                    fp_tamac_product_finite_stage[fpl]),
                .product_sign       (
                    fp_tamac_product_sign_stage[fpl]),
                .product_significand(
                    fp_tamac_product_significand_stage[fpl]),
                .product_exponent   (
                    fp_tamac_product_exponent_stage[fpl]),
                .result             (fp_shared_l1[fpl])
            );
        end

        for (fpl = 0; fpl < 8; fpl = fpl + 1) begin : fp_shared_l2_add
            mp64_fp32_add_rne u_add (
                .a(fp_shared_l1[fpl*2]),
                .b(fp_shared_l1[fpl*2+1]),
                .result(fp_shared_l2[fpl])
            );
        end
        for (fpl = 0; fpl < 4; fpl = fpl + 1) begin : fp_shared_l3_add
            mp64_fp32_add_rne u_add (
                .a(fp_shared_l2[fpl*2]),
                .b(fp_shared_l2[fpl*2+1]),
                .result(fp_shared_l3[fpl])
            );
        end
        for (fpl = 0; fpl < 2; fpl = fpl + 1) begin : fp_shared_l4_add
            mp64_fp32_add_rne u_add (
                .a(fp_shared_l3[fpl*2]),
                .b(fp_shared_l3[fpl*2+1]),
                .result(fp_shared_l4[fpl])
            );
        end
        mp64_fp32_add_rne u_fp_shared_l5_add (
            .a(fp_shared_l4[0]),
            .b(fp_shared_l4[1]),
            .result(fp_shared_l5)
        );
    endgenerate

    wire [31:0] fp_dot_result = fp_shared_l5;
    wire [31:0] fp_dotacc_result [0:3];
    assign fp_dotacc_result[0] = fp_shared_l3[0];
    assign fp_dotacc_result[1] = fp_shared_l3[1];
    assign fp_dotacc_result[2] = fp_shared_l3[2];
    assign fp_dotacc_result[3] = fp_shared_l3[3];
    wire [31:0] fp_sum_l5 = fp_shared_l5;
    wire [31:0] fp_sumsq_l5 = fp_shared_l5;

    // ========================================================================
    // FP16/BF16 TRED — reductions (docs/floating-point.md §4)
    // ========================================================================
    // SUM, L1, and SUMSQ come from the shared tree.  MIN, MAX, MINIDX, and
    // MAXIDX scan for the NaN-skipping extreme (-0 below +0, lowest index on
    // ties); the winner is widened exactly to binary32, and an all-NaN tile
    // gives the canonical binary32 NaN.  POPCNT stays on the integer path.
    reg [31:0] fp_red_result;
    reg [63:0] fp_red_idx;
    reg [15:0] fp_red_best_raw;
    wire [31:0] fp_red_val;
    wire fp_red_largest =
        (funct_reg == TRED_MAX) || (funct_reg == TRED_MAXIDX);

    function [15:0] fp_half_order_key;
        input [15:0] bits;
        begin
            fp_half_order_key = bits[15] ? ~bits : {1'b1, bits[14:0]};
        end
    endfunction

    function [31:0] fp32_order_key;
        input [31:0] bits;
        begin
            fp32_order_key = bits[31] ? ~bits : {1'b1, bits[30:0]};
        end
    endfunction

    function fp32_is_nan;
        input [31:0] bits;
        begin
            fp32_is_nan = (bits[30:23] == 8'hFF) && (bits[22:0] != 23'd0);
        end
    endfunction

    integer fp_ri;
    always @(*) begin : fp_red_block
        reg [15:0] cur_raw;
        reg        cur_is_nan;
        reg        best_is_nan;
        reg        better;

        fp_red_best_raw = tile_a[15:0];
        fp_red_idx      = 64'd0;
        for (fp_ri = 1; fp_ri < 32; fp_ri = fp_ri + 1) begin
            cur_raw = tile_a[fp_ri*16 +: 16];
            if (mode_bf16) begin
                cur_is_nan  = (cur_raw[14:7] == 8'hFF) && |cur_raw[6:0];
                best_is_nan = (fp_red_best_raw[14:7] == 8'hFF) &&
                              |fp_red_best_raw[6:0];
            end else begin
                cur_is_nan  = (cur_raw[14:10] == 5'h1F) && |cur_raw[9:0];
                best_is_nan = (fp_red_best_raw[14:10] == 5'h1F) &&
                              |fp_red_best_raw[9:0];
            end
            better = fp_red_largest ?
                (fp_half_order_key(cur_raw) >
                 fp_half_order_key(fp_red_best_raw)) :
                (fp_half_order_key(cur_raw) <
                 fp_half_order_key(fp_red_best_raw));
            if (!cur_is_nan && (best_is_nan || better)) begin
                fp_red_best_raw = cur_raw;
                fp_red_idx      = fp_ri;
            end
        end
    end

    mp64_fp16_to_fp32 u_red_widen (
        .is_bf16 (mode_bf16),
        .fp16_in (fp_red_best_raw),
        .fp32_out(fp_red_val)
    );

    always @(*) begin
        case (funct_reg)
            TRED_SUM, TRED_L1: fp_red_result = fp_sum_l5;
            TRED_SUMSQ:        fp_red_result = fp_sumsq_l5;
            default:           fp_red_result = fp_red_val;
        endcase
    end

    // ========================================================================
    // TMUL.MUL — lane-wise multiply (truncated to element width)
    // ========================================================================
    reg [511:0] mul_result;
    integer ml;
    always @(*) begin
        mul_result = 512'd0;
        case (lane_ew)
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
    wire [3:0] wmul_mode_ew =
        tamac_datapath_active ? tamac_ew_reg : mode_ew;
    wire wmul_mode_signed =
        tamac_datapath_active ? tamac_signed_reg : mode_signed;
    always @(*) begin
        wmul_lo = 512'd0;
        wmul_hi = 512'd0;
        case (tile_format_lane_log2(wmul_mode_ew))
            2'd0: for (wl = 0; wl < 64; wl = wl + 1) begin : w8
                reg [15:0] wp8;
                if (wmul_mode_signed) wp8 = $signed({{8{tile_a[wl*8+7]}}, tile_a[wl*8 +: 8]})
                                     * $signed({{8{src_b_selected[wl*8+7]}}, src_b_selected[wl*8 +: 8]});
                else             wp8 = {8'd0, tile_a[wl*8 +: 8]} * {8'd0, src_b_selected[wl*8 +: 8]};
                if (wl < 32) wmul_lo[wl*16 +: 16] = wp8;
                else         wmul_hi[(wl-32)*16 +: 16] = wp8;
            end
            2'd1: for (wl = 0; wl < 32; wl = wl + 1) begin : w16
                reg [31:0] wp16;
                if (wmul_mode_signed) wp16 = $signed({{16{tile_a[wl*16+15]}}, tile_a[wl*16 +: 16]})
                                      * $signed({{16{src_b_selected[wl*16+15]}}, src_b_selected[wl*16 +: 16]});
                else             wp16 = {16'd0, tile_a[wl*16 +: 16]} * {16'd0, src_b_selected[wl*16 +: 16]};
                if (wl < 16) wmul_lo[wl*32 +: 32] = wp16;
                else         wmul_hi[(wl-16)*32 +: 32] = wp16;
            end
            2'd2: for (wl = 0; wl < 16; wl = wl + 1) begin : w32
                reg [63:0] wp32;
                if (wmul_mode_signed) wp32 = $signed({{32{tile_a[wl*32+31]}}, tile_a[wl*32 +: 32]})
                                      * $signed({{32{src_b_selected[wl*32+31]}}, src_b_selected[wl*32 +: 32]});
                else             wp32 = {32'd0, tile_a[wl*32 +: 32]} * {32'd0, src_b_selected[wl*32 +: 32]};
                if (wl < 8) wmul_lo[wl*64 +: 64] = wp32;
                else        wmul_hi[(wl-8)*64 +: 64] = wp32;
            end
            2'd3: wmul_lo = mul_result;  // can't widen 64→128
        endcase
    end

    // ========================================================================
    // Integer TAMAC — one structurally explicit 16x64-bit feedback bank
    // ========================================================================
    // WMUL already produces exact 8x8, 16x16, and 32x32 products.  Operand
    // muxes below select one group of sixteen accumulator/product lanes for
    // every beat.  The sole arithmetic operators are the sixteen generated
    // 64-bit additions, so synthesis cannot elaborate a separate adder bank
    // for each element width.  U8 consumes only each sum's low 32 bits.
    reg  [63:0] tamac_feedback_lhs [0:15];
    reg  [63:0] tamac_feedback_rhs [0:15];
    wire [63:0] tamac_feedback_sum [0:15];
    reg [1023:0] tamac_slice_result;
    integer tfo;
    integer tfr;
    integer tamac_lane_index;

    always @(*) begin
        tamac_lane_index = 0;
        for (tfo = 0; tfo < 16; tfo = tfo + 1) begin
            tamac_feedback_lhs[tfo] = 64'd0;
            tamac_feedback_rhs[tfo] = 64'd0;

            case (tamac_ew_reg)
                TMODE_8: begin
                    tamac_lane_index = tamac_beat_reg * 16 + tfo;
                    tamac_feedback_lhs[tfo] =
                        {32'd0,
                         tacc_bank_state[
                             tamac_lane_index*32 +: 32]};
                    if (tamac_lane_index < 32) begin
                        tamac_feedback_rhs[tfo] =
                            tamac_signed_reg ?
                            {{48{wmul_lo[
                                tamac_lane_index*16 + 15]}},
                             wmul_lo[
                                tamac_lane_index*16 +: 16]} :
                            {48'd0,
                             wmul_lo[
                                tamac_lane_index*16 +: 16]};
                    end else begin
                        tamac_feedback_rhs[tfo] =
                            tamac_signed_reg ?
                            {{48{wmul_hi[
                                (tamac_lane_index-32)*16 + 15]}},
                             wmul_hi[
                                (tamac_lane_index-32)*16 +: 16]} :
                            {48'd0,
                             wmul_hi[
                                (tamac_lane_index-32)*16 +: 16]};
                    end
                end

                TMODE_16: begin
                    tamac_lane_index = tamac_beat_reg * 16 + tfo;
                    tamac_feedback_lhs[tfo] =
                        tacc_bank_state[
                            tamac_lane_index*64 +: 64];
                    if (tamac_lane_index < 16) begin
                        tamac_feedback_rhs[tfo] =
                            tamac_signed_reg ?
                            {{32{wmul_lo[
                                tamac_lane_index*32 + 31]}},
                             wmul_lo[
                                tamac_lane_index*32 +: 32]} :
                            {32'd0,
                             wmul_lo[
                                tamac_lane_index*32 +: 32]};
                    end else begin
                        tamac_feedback_rhs[tfo] =
                            tamac_signed_reg ?
                            {{32{wmul_hi[
                                (tamac_lane_index-16)*32 + 31]}},
                             wmul_hi[
                                (tamac_lane_index-16)*32 +: 32]} :
                            {32'd0,
                             wmul_hi[
                                (tamac_lane_index-16)*32 +: 32]};
                    end
                end

                TMODE_32: begin
                    tamac_feedback_lhs[tfo] =
                        tacc_bank_state[tfo*64 +: 64];
                    if (tfo < 8)
                        tamac_feedback_rhs[tfo] =
                            wmul_lo[tfo*64 +: 64];
                    else
                        tamac_feedback_rhs[tfo] =
                            wmul_hi[(tfo-8)*64 +: 64];
                end

                default: begin
                end
            endcase
        end
    end

    genvar tamac_feedback_lane;
    generate
        for (tamac_feedback_lane = 0;
             tamac_feedback_lane < 16;
             tamac_feedback_lane = tamac_feedback_lane + 1) begin
            assign tamac_feedback_sum[tamac_feedback_lane] =
                tamac_feedback_lhs[tamac_feedback_lane] +
                tamac_feedback_rhs[tamac_feedback_lane];
        end
    endgenerate

    always @(*) begin
        tamac_slice_result = 1024'd0;
        for (tfr = 0; tfr < 16; tfr = tfr + 1) begin
            if (tamac_ew_reg == TMODE_8)
                tamac_slice_result[tfr*32 +: 32] =
                    tamac_feedback_sum[tfr][31:0];
            else if ((tamac_ew_reg == TMODE_16) ||
                     (tamac_ew_reg == TMODE_32))
                tamac_slice_result[tfr*64 +: 64] =
                    tamac_feedback_sum[tfr];
        end
    end

    wire tamac_last_beat =
        ((tamac_ew_reg == TMODE_8)  &&
         (tamac_beat_reg == 2'd3)) ||
        ((tamac_ew_reg == TMODE_16) &&
         (tamac_beat_reg == 2'd1)) ||
        ((tamac_ew_reg == TMODE_32) &&
         (tamac_beat_reg == 2'd0)) ||
        (((tamac_ew_reg == TMODE_FP16) ||
          (tamac_ew_reg == TMODE_BF16)) &&
         (tamac_beat_reg == 2'd3)) ||
        // FP32 and FP64 run one binary64 lane per FMA unit per beat.
        ((tamac_ew_reg == TMODE_FP32) &&
         (tamac_beat_reg == 16 / FMA_UNITS - 1)) ||
        ((tamac_ew_reg == TMODE_FP64) &&
         (tamac_beat_reg == 8 / FMA_UNITS - 1));
    wire tamac_source_ack =
        tamac_read_ext_reg ? ext_tile_ack : tile_ack;
    wire tamac_source_error =
        tamac_read_ext_reg ? ext_tile_error : tile_error;
    wire [63:0] tamac_source_fault_addr =
        tamac_read_ext_reg ?
        ext_tile_fault_addr : tile_fault_addr;
    wire tamac_source_wait =
        (state == S_TAMAC_LOAD_A) ||
        (state == S_TAMAC_LOAD_B);

    assign tamac_terminal =
        (tamac_source_wait &&
         tamac_source_ack && tamac_source_error) ||
        ((state == S_TACC_INT) && tamac_last_beat);
    assign tamac_terminal_fault =
        (tamac_source_wait &&
         tamac_source_ack && tamac_source_error) ?
        MEX_FAULT_BUS : MEX_FAULT_NONE;
    assign tamac_terminal_fault_addr =
        (tamac_source_wait &&
         tamac_source_ack && tamac_source_error) ?
        tamac_source_fault_addr : 64'd0;
    assign tamac_result_image =
        (tamac_ew_reg == TMODE_8) ?
            {tile_a, result2, result, tile_c} :
        (tamac_ew_reg == TMODE_16) ?
            {tile_b, tile_a, result2, result} :
        (tamac_ew_reg == TMODE_32) ?
            {1024'd0, result2, result} :
        ((tamac_ew_reg == TMODE_FP16) ||
         (tamac_ew_reg == TMODE_BF16) ||
         (tamac_ew_reg == TMODE_FP32)) ?
            {1024'd0, result2, result} :
        (tamac_ew_reg == TMODE_FP64) ?
            {1536'd0, result} :
            2048'd0;

    // ========================================================================
    // TMUL.MAC / TMUL.FMA — multiply-accumulate in-place (dst += a*b)
    // ========================================================================
    reg [511:0] mac_result;
    integer mcl;
    always @(*) begin
        mac_result = 512'd0;
        case (lane_ew)
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
        case (lane_ew)
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
        case (lane_ew)
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
        case (lane_ew)
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
        case (lane_ew)
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
        case (lane_ew)
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
        case (lane_ew)
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
        case (lane_ew)
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
        case (lane_ew)
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
    // EXT.8 VSEL, TCMP, and TCVT (docs/floating-point.md §6.3-§6.5)
    // ========================================================================
    // Every operation works at the format's real lane width.  VSEL takes its
    // mask M from the old [TDST], loaded as tile_c.

    function [63:0] lane_width_mask;
        input [1:0] log2;
        case (log2)
            2'd0:    lane_width_mask = 64'h0000_0000_0000_00FF;
            2'd1:    lane_width_mask = 64'h0000_0000_0000_FFFF;
            2'd2:    lane_width_mask = 64'h0000_0000_FFFF_FFFF;
            default: lane_width_mask = 64'hFFFF_FFFF_FFFF_FFFF;
        endcase
    endfunction

    function [63:0] float_infinity;
        input [3:0] ew;
        case (ew)
            TMODE_FP16: float_infinity = 64'h0000_0000_0000_7C00;
            TMODE_BF16: float_infinity = 64'h0000_0000_0000_7F80;
            TMODE_FP32: float_infinity = 64'h0000_0000_7F80_0000;
            default:    float_infinity = 64'h7FF0_0000_0000_0000;
        endcase
    endfunction

    // {unordered, less, equal} for two zero-extended lanes of format ew.
    function [2:0] lane_order;
        input [3:0]  ew;
        input        signed_ints;
        input [63:0] a;
        input [63:0] b;
        reg   [63:0] mask;
        reg   [63:0] top;
        reg   [63:0] key_a;
        reg   [63:0] key_b;
        begin
            mask = lane_width_mask(tile_format_lane_log2(ew));
            top  = (mask >> 1) + 64'd1;
            if (tile_format_is_float(ew)) begin
                if (((a & (mask ^ top)) > float_infinity(ew)) ||
                    ((b & (mask ^ top)) > float_infinity(ew)))
                    lane_order = 3'b100;
                else if (((a | b) & (mask ^ top)) == 64'd0)
                    lane_order = 3'b001;  // -0 equals +0
                else begin
                    key_a = (a & top) ? (mask ^ a) : (a | top);
                    key_b = (b & top) ? (mask ^ b) : (b | top);
                    lane_order = {1'b0, key_a < key_b, a == b};
                end
            end else if (signed_ints) begin
                lane_order = {1'b0, (a ^ top) < (b ^ top), a == b};
            end else begin
                lane_order = {1'b0, a < b, a == b};
            end
        end
    endfunction

    function compare_holds;
        input [2:0] predicate;
        input [2:0] order;
        reg greater;
        begin
            greater = !order[2] && !order[1] && !order[0];
            case (predicate)
                3'd0: compare_holds = order[0];
                3'd1: compare_holds = !order[0];
                3'd2: compare_holds = order[1];
                3'd3: compare_holds = order[1] || order[0];
                3'd4: compare_holds = greater;
                3'd5: compare_holds = greater || order[0];
                3'd6: compare_holds = order[2];
                default: compare_holds = !order[2];
            endcase
        end
    endfunction

    reg [511:0] ext_select_result;
    reg [511:0] ext_compare_result;
    integer scl;
    always @(*) begin : ext_select_compare
        reg [63:0] la;
        reg [63:0] lb;
        reg [63:0] lm;
        reg [63:0] mask;
        integer    width;
        integer    lanes;
        ext_select_result  = 512'd0;
        ext_compare_result = 512'd0;
        mask  = lane_width_mask(lane_ew);
        width = 8 << lane_ew;
        lanes = 64 >> lane_ew;
        for (scl = 0; scl < 64; scl = scl + 1) begin
            if (scl < lanes) begin
                la = (tile_a >> (scl * width)) & {448'd0, mask};
                lb = (src_b_selected >> (scl * width)) & {448'd0, mask};
                lm = (tile_c >> (scl * width)) & {448'd0, mask};
                ext_select_result = ext_select_result |
                    ({448'd0, (lm & ((mask >> 1) + 64'd1)) ? la : lb}
                     << (scl * width));
                if (compare_holds(funct_byte_reg[5:3],
                                  lane_order(mode_ew, mode_signed, la, lb)))
                    ext_compare_result = ext_compare_result |
                        ({448'd0, mask} << (scl * width));
            end
        end
    end

    // One TCVT lane: the zero-extended lane x of format src converted to
    // format dst (§6.3).  Float results round to nearest-even once.  Float to
    // integer maps NaN to 0, rounds toward zero or (nearest) to nearest-even,
    // and saturates.  Integer signedness comes from TMODE[4].
    function [63:0] tcvt_lane;
        input [3:0]  src;
        input [3:0]  dst;
        input        signed_ints;
        input        nearest;
        input [63:0] x;
        integer src_ebits;
        integer src_fbits;
        integer p;
        integer emin;
        integer bias;
        integer ebits;
        integer width;
        integer exponent;
        integer lead;
        integer quantum;
        integer shift;
        integer biased;
        integer k;
        reg        negative;
        reg        is_nan;
        reg        is_inf;
        reg [63:0] sig;
        reg [63:0] kept;
        reg [63:0] below;
        reg        round_bit;
        reg        sticky;
        reg        overflow;
        reg [63:0] sign_bit;
        reg [63:0] limit;
        reg [63:0] result;
        begin
            negative = 1'b0;
            is_nan   = 1'b0;
            is_inf   = 1'b0;
            sig      = 64'd0;
            exponent = 0;
            overflow = 1'b0;
            round_bit = 1'b0;
            sticky    = 1'b0;
            result    = 64'd0;
            // Decode the source into (-1)**negative * sig * 2**exponent.
            if (!tile_format_is_float(src)) begin
                width = 8 << src;
                sig = x & lane_width_mask(src[1:0]);
                if (signed_ints && sig[width - 1]) begin
                    negative = 1'b1;
                    sig = (~sig + 64'd1) & lane_width_mask(src[1:0]);
                    if (sig == 64'd0)  // the most negative value
                        sig = 64'd1 << (width - 1);
                end
            end else begin
                case (src)
                    TMODE_FP16: begin src_ebits = 5;  src_fbits = 10; end
                    TMODE_BF16: begin src_ebits = 8;  src_fbits = 7;  end
                    TMODE_FP32: begin src_ebits = 8;  src_fbits = 23; end
                    default:    begin src_ebits = 11; src_fbits = 52; end
                endcase
                width    = 1 + src_ebits + src_fbits;
                negative = x[width - 1];
                k        = (x >> src_fbits) & ((64'd1 << src_ebits) - 1);
                sig      = x & ((64'd1 << src_fbits) - 1);
                if (k == (1 << src_ebits) - 1) begin
                    is_nan = (sig != 64'd0);
                    is_inf = (sig == 64'd0);
                end else if (k == 0) begin
                    exponent = 2 - (1 << (src_ebits - 1)) - src_fbits;
                end else begin
                    sig = sig | (64'd1 << src_fbits);
                    exponent = k - ((1 << (src_ebits - 1)) - 1) - src_fbits;
                end
            end

            lead = -1;
            for (k = 0; k < 64; k = k + 1)
                if (sig[k])
                    lead = k;

            if (tile_format_is_float(dst)) begin
                case (dst)
                    TMODE_FP16: begin p = 11; ebits = 5;  end
                    TMODE_BF16: begin p = 8;  ebits = 8;  end
                    TMODE_FP32: begin p = 24; ebits = 8;  end
                    default:    begin p = 53; ebits = 11; end
                endcase
                bias     = (1 << (ebits - 1)) - 1;
                emin     = 1 - bias;
                sign_bit = 64'd1 << (p + ebits - 1);
                if (is_nan) begin
                    result = float_infinity(dst) | (64'd1 << (p - 2));
                end else if (is_inf) begin
                    result = float_infinity(dst) | (negative ? sign_bit : 64'd0);
                end else if (lead < 0) begin
                    result = negative ? sign_bit : 64'd0;
                end else begin
                    quantum = exponent + lead - (p - 1);
                    if (quantum < emin - (p - 1))
                        quantum = emin - (p - 1);
                    shift = quantum - exponent;
                    if (shift <= 0) begin
                        kept = sig << (-shift);
                    end else if (shift > 64) begin
                        kept   = 64'd0;
                        sticky = 1'b1;
                    end else begin
                        kept      = (shift == 64) ? 64'd0 : (sig >> shift);
                        round_bit = sig[shift - 1];
                        below     = (shift == 1) ? 64'd0 :
                                    ((64'd1 << (shift - 1)) - 64'd1);
                        sticky    = |(sig & below);
                    end
                    if (round_bit && (sticky || kept[0]))
                        kept = kept + 64'd1;
                    if (kept[p]) begin
                        kept    = kept >> 1;
                        quantum = quantum + 1;
                    end
                    if (kept[p - 1]) begin
                        biased = quantum + (p - 1) + bias;
                        if (biased >= (1 << ebits) - 1)
                            result = float_infinity(dst);
                        else
                            result = ({52'd0, biased[11:0]} << (p - 1)) |
                                     (kept & ((64'd1 << (p - 1)) - 1));
                    end else begin
                        result = kept;
                    end
                    if (negative)
                        result = result | sign_bit;
                end
            end else begin
                width = 8 << dst;
                if (is_inf) begin
                    overflow = 1'b1;
                    kept = 64'd0;
                end else if (is_nan || lead < 0) begin
                    kept = 64'd0;
                end else if (exponent >= 0) begin
                    overflow = (lead + exponent >= 64);
                    kept = overflow ? 64'd0 : (sig << exponent);
                end else begin
                    shift = -exponent;
                    kept      = (shift >= 64) ? 64'd0 : (sig >> shift);
                    round_bit = (shift <= 64) ? sig[shift - 1] : 1'b0;
                    below     = (shift == 1) ? 64'd0 :
                                (shift > 64) ? 64'hFFFF_FFFF_FFFF_FFFF :
                                ((64'd1 << (shift - 1)) - 64'd1);
                    sticky    = |(sig & below);
                    if (nearest && round_bit && (sticky || kept[0]))
                        kept = kept + 64'd1;
                end
                if (signed_ints) begin
                    limit = 64'd1 << (width - 1);  // |most negative|
                    if (negative)
                        result = (overflow || kept > limit) ? limit :
                                 (~kept + 64'd1);
                    else
                        result = (overflow || kept >= limit) ?
                                 (limit - 64'd1) : kept;
                end else begin
                    limit = lane_width_mask(dst[1:0]);
                    if (negative)
                        result = 64'd0;
                    else
                        result = (overflow || kept > limit) ? limit : kept;
                end
                result = result & lane_width_mask(dst[1:0]);
            end
            tcvt_lane = result;
        end
    endfunction

    // TCVT schedule: sixteen lane converters, one beat per sixteen lanes of
    // the current tile, then idle beats so the conversion takes exactly
    // 4 + (k - 1) cycles (§10).  Narrowing converts each of k source tiles
    // into its lanes of result and writes once; widening reads one source
    // tile and converts and writes each of k destination tiles in turn.
    reg [3:0]  cvt_target;
    reg [3:0]  cvt_k;
    reg [3:0]  cvt_tile;       // current source (narrowing) or target tile
    reg [1:0]  cvt_beat;       // sixteen-lane group within the tile
    reg [3:0]  cvt_cycles;     // conversion cycles spent
    reg        cvt_ext;        // the pending access uses the external port
    wire [1:0] cvt_src_log2 = lane_ew;
    wire [1:0] cvt_dst_log2 = tile_format_lane_log2(cvt_target);
    wire       cvt_narrow   = cvt_dst_log2 < cvt_src_log2;
    wire       cvt_widen    = cvt_dst_log2 > cvt_src_log2;
    // Lanes converted in the current tile: the source tile's lanes when
    // narrowing or equal, the target tile's lanes when widening.
    wire [6:0] cvt_tile_lanes = cvt_widen ? (7'd64 >> cvt_dst_log2)
                                          : (7'd64 >> cvt_src_log2);
    wire       cvt_last_beat  = ({cvt_beat, 4'd0} + 7'd16) >= cvt_tile_lanes;
    // Narrowing reads the next source tile; every write goes to the current
    // destination tile (TDST for narrowing and equal widths).
    wire [63:0] cvt_next_src_addr = tsrc0 + {cvt_tile + 4'd1, 6'd0};
    wire [63:0] cvt_dst_addr      = tdst + {cvt_widen ? cvt_tile : 4'd0, 6'd0};

    function address_internal;  // Bank 0 or the HBW banks
        input [63:0] address;
        address_internal = (address[63:20] == 44'd0) ||
            ((address[63:32] == 32'd0) && (address[31:20] >= 12'hFFD));
    endfunction

    reg [511:0] cvt_result_next;
    integer cvl;
    always @(*) begin : tcvt_lanes
        integer src_lane;
        integer dst_lane;
        reg [63:0] lane_in;
        reg [63:0] lane_out;
        cvt_result_next = result;
        for (cvl = 0; cvl < 16; cvl = cvl + 1) begin
            if ({cvt_beat, 4'd0} + cvl < cvt_tile_lanes) begin
                if (cvt_widen) begin
                    dst_lane = {cvt_beat, 4'd0} + cvl;
                    src_lane = cvt_tile * cvt_tile_lanes + dst_lane;
                end else begin
                    src_lane = {cvt_beat, 4'd0} + cvl;
                    dst_lane = cvt_tile * cvt_tile_lanes + src_lane;
                end
                lane_in = (tile_a >> (src_lane * (8 << cvt_src_log2))) &
                          {448'd0, lane_width_mask(cvt_src_log2)};
                lane_out = tcvt_lane(mode_ew, cvt_target, mode_signed,
                                     mode_rounding, lane_in);
                cvt_result_next =
                    (cvt_result_next &
                     ~({448'd0, lane_width_mask(cvt_dst_log2)} <<
                       (dst_lane * (8 << cvt_dst_log2)))) |
                    ({448'd0, lane_out} << (dst_lane * (8 << cvt_dst_log2)));
            end
        end
    end

    // ========================================================================
    // FP32 ACC_ACC adders (pre-computed for sequential assignment)
    // ========================================================================
    // DOT ACC_ACC: acc[0] + fp_dot_result
    wire [31:0] fp_dot_acc_result;
    mp64_fp32_add_rne u_dot_acc_add (
        .a(acc[0][31:0]), .b(fp_dot_result), .result(fp_dot_acc_result)
    );

    // DOTACC ACC_ACC: acc[k] + fp_dotacc_result[k]
    wire [31:0] fp_dotacc_acc_result [0:3];
    generate
        for (fpl = 0; fpl < 4; fpl = fpl + 1) begin : dotacc_acc_add
            mp64_fp32_add_rne u_dac_acc (
                .a(acc[fpl][31:0]), .b(fp_dotacc_result[fpl]),
                .result(fp_dotacc_acc_result[fpl])
            );
        end
    endgenerate

    // TRED SUM/SUMSQ/L1 ACC_ACC: acc[0] + fp_red_result
    wire [31:0] fp_red_acc_result;
    mp64_fp32_add_rne u_red_acc_add (
        .a(acc[0][31:0]), .b(fp_red_result), .result(fp_red_acc_result)
    );

    // TRED MIN/MAX ACC_ACC: running NaN-skipping extreme against ACC0; the
    // old value wins ties.  MINIDX/MAXIDX replace ACC0/ACC1 only for a
    // strictly better non-NaN value, or any non-NaN value over an old NaN.
    wire [31:0] fp_acc0_value = acc[0][31:0];
    wire [31:0] fp_acc1_value = acc[1][31:0];
    wire fp_red_beats_acc0 = fp_red_largest ?
        (fp32_order_key(fp_red_result) > fp32_order_key(fp_acc0_value)) :
        (fp32_order_key(fp_red_result) < fp32_order_key(fp_acc0_value));
    wire [31:0] fp_red_extreme_acc =
        fp32_is_nan(fp_acc0_value) ?
            (fp32_is_nan(fp_red_result) ? 32'h7FC0_0000 : fp_red_result) :
        (fp32_is_nan(fp_red_result) || !fp_red_beats_acc0) ?
            fp_acc0_value : fp_red_result;
    wire fp_red_index_replaces =
        !fp32_is_nan(fp_red_val) &&
        (fp32_is_nan(fp_acc1_value) ||
         (fp_red_largest ?
          (fp32_order_key(fp_red_val) > fp32_order_key(fp_acc1_value)) :
          (fp32_order_key(fp_red_val) < fp32_order_key(fp_acc1_value))));

    // ========================================================================
    // Main state machine
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state         <= S_IDLE;
            mex_done_reg  <= 1'b0;
            z_kind_reg    <= Z_NONE;
            mex_zero_valid_reg <= 1'b0;
            mex_zero_reg  <= 1'b0;
            mex_busy_reg  <= 1'b0;
            mex_fault_reg <= MEX_FAULT_NONE;
            mex_fault_addr_reg <= 64'd0;
            tile_req      <= 1'b0;
            tile_wen      <= 1'b0;
            tile_a        <= 512'd0;
            tile_b        <= 512'd0;
            tile_c        <= 512'd0;
            result        <= 512'd0;
            result2       <= 512'd0;
            ext_tile_req  <= 1'b0;
            ext_tile_wen  <= 1'b0;
            tile_source_cancel <= 1'b0;
            needs_load_c  <= 1'b0;
            engine_epoch  <= 8'd0;
            engine_reset_seen <= 1'b0;
            tctrl_accumulate_reg <= 1'b0;
            tctrl_acc_zero_reg   <= 1'b0;
            tctrl_acc_zero_clear <= 1'b0;
            op_reg        <= 2'd0;
            funct_reg     <= 3'd0;
            funct_byte_reg<= 8'd0;
            ss_reg        <= 2'd0;
            gpr_val_reg   <= 64'd0;
            imm8_reg      <= 8'd0;
            ext_mod_reg   <= 4'd0;
            ext_active_reg<= 1'b0;
            caller_id_reg <= 5'd0;
            priv_reg      <= 1'b0;
            mpu_base_reg  <= 64'd0;
            mpu_limit_reg <= 64'd0;
            mpu_enabled_reg <= 1'b0;
            allow_cluster_spad_reg <= 1'b0;
            request_engine_epoch_reg <= 8'd0;
            caller_epoch_reg <= 8'd0;
            caller_slot_reg  <= 2'd0;
            tamac_ew_reg      <= TMODE_8;
            tamac_signed_reg  <= 1'b0;
            tamac_beat_reg    <= 4'd0;
            fma_beat          <= 4'd0;
            tree_v            <= 1024'd0;
            tree_phase        <= TREE_PRODUCTS;
            tree_count        <= 5'd0;
            tree_target       <= 3'd0;
            tree_accumulate   <= 1'b0;
            cvt_target        <= 4'd0;
            cvt_k             <= 4'd1;
            cvt_tile          <= 4'd0;
            cvt_beat          <= 2'd0;
            cvt_cycles        <= 4'd0;
            cvt_ext           <= 1'b0;
            tamac_src_a_addr_reg <= 64'd0;
            tamac_src_b_addr_reg <= 64'd0;
            tacc_image_addr_reg  <= 64'd0;
            tamac_src_b_ext_reg  <= 1'b0;
            tamac_read_ext_reg   <= 1'b0;
            acc[0]        <= 64'd0;
            acc[1]        <= 64'd0;
            acc[2]        <= 64'd0;
            acc[3]        <= 64'd0;
        end else if (engine_reset) begin
            state         <= S_IDLE;
            mex_done_reg  <= 1'b0;
            mex_busy_reg  <= 1'b0;
            mex_fault_reg <= MEX_FAULT_NONE;
            mex_fault_addr_reg <= 64'd0;
            tile_req      <= 1'b0;
            tile_wen      <= 1'b0;
            ext_tile_req  <= 1'b0;
            ext_tile_wen  <= 1'b0;
            tile_source_cancel <= tamac_source_wait;
            needs_load_c  <= 1'b0;
            tctrl_accumulate_reg <= 1'b0;
            tctrl_acc_zero_reg   <= 1'b0;
            tctrl_acc_zero_clear <= 1'b0;
            acc[0]        <= 64'd0;
            acc[1]        <= 64'd0;
            acc[2]        <= 64'd0;
            acc[3]        <= 64'd0;
            if (!engine_reset_seen)
                engine_epoch <= engine_epoch + 8'd1;
            engine_reset_seen <= 1'b1;
        end else begin
            engine_reset_seen <= 1'b0;
            mex_done_reg <= 1'b0;
            tile_req     <= 1'b0;
            tile_wen     <= 1'b0;
            ext_tile_req <= 1'b0;
            ext_tile_wen <= 1'b0;
            tile_source_cancel <= 1'b0;
            tctrl_acc_zero_clear <= 1'b0;

            // ACC has one procedural owner. Cluster context restores and
            // direct CSR writes are lane-masked here; terminal MEX updates
            // later in this block deliberately take priority.
            if (legacy_acc_wen[0])
                acc[0] <= legacy_acc_wdata[0*64 +: 64];
            if (legacy_acc_wen[1])
                acc[1] <= legacy_acc_wdata[1*64 +: 64];
            if (legacy_acc_wen[2])
                acc[2] <= legacy_acc_wdata[2*64 +: 64];
            if (legacy_acc_wen[3])
                acc[3] <= legacy_acc_wdata[3*64 +: 64];
            if (csr_wen) begin
                case (csr_addr)
                    CSR_ACC0: acc[0] <= csr_wdata;
                    CSR_ACC1: acc[1] <= csr_wdata;
                    CSR_ACC2: acc[2] <= csr_wdata;
                    CSR_ACC3: acc[3] <= csr_wdata;
                    default: ;
                endcase
            end

            if (active_cancelled) begin
                // Cancellation is terminal but non-retiring.  A TAMAC source
                // request is canceled one interval later, after its dispatch
                // pulse has been captured by the source arbiter.  The arbiter
                // drains any accepted target response and suppresses its ACK.
                state         <= S_IDLE;
                mex_done_reg  <= 1'b0;
                mex_busy_reg  <= 1'b0;
                mex_fault_reg <= MEX_FAULT_NONE;
                mex_fault_addr_reg <= 64'd0;
                tile_req      <= 1'b0;
                tile_wen      <= 1'b0;
                ext_tile_req  <= 1'b0;
                ext_tile_wen  <= 1'b0;
                tile_source_cancel <= tamac_source_wait;
                needs_load_c  <= 1'b0;
            end else begin
            case (state)
            S_IDLE: begin
                mex_busy_reg <= 1'b0;
                if (!mex_valid) begin
                    mex_fault_reg  <= MEX_FAULT_NONE;
                    mex_fault_addr_reg <= 64'd0;
                end
                if (mex_valid) begin
                    if (incoming_cancelled) begin
                        // A stale request belongs to an execution context
                        // that no longer waits for completion.  Drop it before
                        // any memory, ACC, or completion side effect.
                        state          <= S_IDLE;
                        mex_busy_reg   <= 1'b0;
                        mex_fault_reg  <= MEX_FAULT_NONE;
                        mex_fault_addr_reg <= 64'd0;
                    end else begin
                        op_reg        <= mex_op;
                        funct_reg     <= mex_funct;
                        funct_byte_reg<= mex_funct_byte;
                        ss_reg        <= mex_ss;
                        gpr_val_reg   <= mex_gpr_val;
                        imm8_reg      <= mex_imm8;
                        ext_mod_reg   <= mex_ext_mod;
                        ext_active_reg<= mex_ext_active;
                        caller_id_reg <= mex_caller_id;
                        priv_reg      <= mex_priv;
                        mpu_base_reg  <= mex_mpu_base;
                        mpu_limit_reg <= mex_mpu_limit;
                        mpu_enabled_reg <= mex_mpu_enabled;
                        allow_cluster_spad_reg <= mex_allow_cluster_spad;
                        request_engine_epoch_reg <= mex_engine_epoch;
                        caller_epoch_reg <= mex_caller_epoch;
                        caller_slot_reg  <= mex_caller_slot;
                        tamac_ew_reg      <= mode_ew;
                        tamac_signed_reg  <= mode_signed;
                        tamac_beat_reg    <= 4'd0;
                        tamac_src_a_addr_reg <=
                            (mex_ss == 2'd3) ? tdst : tsrc0;
                        tamac_src_b_addr_reg <=
                            (mex_ss == 2'd0) ? tsrc1 : tsrc0;
                        tacc_image_addr_reg <=
                            (mex_funct == ETSYS_TACC_STORE) ?
                            tdst : tsrc0;
                        tctrl_accumulate_reg <= tctrl[0];
                        tctrl_acc_zero_reg   <= tctrl[1];
                        z_kind_reg    <= Z_NONE;
                        mex_busy_reg  <= 1'b1;
                        mex_fault_reg <= MEX_FAULT_NONE;
                        mex_fault_addr_reg <= 64'd0;
                        needs_load_c  <= 1'b0;

                        if (intercept_tacc_namespace) begin
                            if (tacc_req_is_tamac &&
                                tacc_tamac_start) begin
                                tamac_src_b_ext_reg <=
                                    tamac_src_b_ext;
                                tamac_read_ext_reg <=
                                    tamac_src_a_ext;
                                if (tamac_src_a_ext) begin
                                    ext_tile_req <= 1'b1;
                                    ext_tile_addr <=
                                        tacc_req_tamac_src_a;
                                end else begin
                                    tile_req <= 1'b1;
                                    tile_addr <=
                                        tacc_req_tamac_src_a[31:0];
                                end
                                state <= S_TAMAC_LOAD_A;
                            end else begin
                                // Keep the captured copy live because a
                                // simultaneous FORCE may defer admission or
                                // validation may publish a terminal fault.
                                state <= S_TACC_WAIT;
                            end
                        end
                    // A reserved format, or an operation the format does not
                    // admit, retires as an illegal operation before any
                    // memory, accumulator, or TCTRL side effect.  TCVT is
                    // admitted below, after this check.
                    else if (!tile_op_admitted(
                                 mode_ew, mex_op,
                                 (mex_ss == 2'd2) ? 3'd0 : mex_funct,
                                 mex_ext_active && (mex_ext_mod == 4'd8),
                                 mex_ss, mex_funct_byte)) begin
                        mex_fault_reg <= MEX_FAULT_ILLEGAL;
                        state         <= S_DONE;
                    end
                    // EXT.8 TCVT — convert a multi-tile region (§6.3)
                    else if (mex_ext_active && mex_ext_mod == 4'd8 &&
                             mex_op == MEX_TALU &&
                             mex_funct == ETALU_TCVT) begin
                        cvt_target <= mex_funct_byte[7:4];
                        cvt_k      <= 4'd1 << (
                            (tile_format_lane_log2(mex_funct_byte[7:4]) >
                             tile_format_lane_log2(mode_ew)) ?
                            (tile_format_lane_log2(mex_funct_byte[7:4]) -
                             tile_format_lane_log2(mode_ew)) :
                            (tile_format_lane_log2(mode_ew) -
                             tile_format_lane_log2(mex_funct_byte[7:4])));
                        cvt_tile   <= 4'd0;
                        cvt_beat   <= 2'd0;
                        cvt_cycles <= 4'd0;
                        result     <= 512'd0;
                        cvt_ext    <= !src0_internal;
                        if (src0_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= tsrc0[31:0];
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= tsrc0;
                        end
                        state <= S_CVT_READ_WAIT;
                    end
                    // TSYS.ZERO — write zeros
                    else if (mex_op == MEX_TSYS && mex_funct == TSYS_ZERO &&
                        !(mex_ext_active && mex_ext_mod == 4'd8)) begin
                        if (dst_internal) begin
                            tile_req   <= 1'b1;
                            tile_addr  <= tdst[31:0];
                            tile_wen   <= 1'b1;
                            tile_wdata <= 512'd0;
                            state      <= S_STORE_WAIT;
                        end else begin
                            ext_tile_req   <= 1'b1;
                            ext_tile_addr  <= tdst;
                            ext_tile_wen   <= 1'b1;
                            ext_tile_wdata <= 512'd0;
                            state          <= S_EXT_STORE;
                        end
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
                    // TRED — reduce operand A only
                    else if (mex_op == MEX_TRED) begin
                        if (mex_src_a_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= mex_src_a_addr[31:0];
                            state     <= S_LOAD_A;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= mex_src_a_addr;
                            state         <= S_EXT_LOAD_A;
                        end
                    end
                    // TMUL MAC/FMA — need existing TDST
                    else if (mex_op == MEX_TMUL &&
                             (mex_funct == TMUL_MAC || mex_funct == TMUL_FMA)) begin
                        needs_load_c <= 1'b1;
                        if (mex_src_a_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= mex_src_a_addr[31:0];
                            state     <= S_LOAD_A;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= mex_src_a_addr;
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
                    // Everything else: load operand A.  VSEL also needs the
                    // old [TDST] as its mask (§6.4), loaded like MAC's addend.
                    else begin
                        needs_load_c <= mex_ext_active && (mex_ext_mod == 4'd8) &&
                                        (mex_op == MEX_TALU) &&
                                        (mex_funct == ETALU_VSEL);
                        if (mex_src_a_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= mex_src_a_addr[31:0];
                            state     <= S_LOAD_A;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= mex_src_a_addr;
                            state         <= S_EXT_LOAD_A;
                        end
                    end
                    end
                end
            end

            S_LOAD_A: begin
                if (tile_ack) begin
                    tile_a <= tile_rdata;
                    if (op_reg == MEX_TRED) begin
                        if (ss_reg == 2'd2)
                            tile_a <= imm_splat;
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
                    else if (ss_reg == 2'd0 || ss_reg == 2'd3) begin
                        // Operand B: [TSRC1] tile x tile, [TSRC0] in place.
                        if ((ss_reg == 2'd0) ? src1_internal : src0_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= (ss_reg == 2'd0) ?
                                         tsrc1[31:0] : tsrc0[31:0];
                            state     <= S_LOAD_B;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= (ss_reg == 2'd0) ? tsrc1 : tsrc0;
                            state         <= S_EXT_LOAD_B;
                        end
                    end
                    else begin
                        if (ss_reg == 2'd2) begin
                            tile_a <= imm_splat;
                            tile_b <= tile_rdata;
                        end
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
                    result <= (funct_reg == ETALU_VSEL) ? ext_select_result :
                              (funct_reg == ETALU_TCMP) ? ext_compare_result :
                              ext_talu_result;
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
                        TSYS_PACK:    result <= pack_result;
                        TSYS_UNPACK:  result <= unpack_result;
                        TSYS_SHUFFLE: result <= shuffle_result;
                        TSYS_RROT:    result <= rrot_result;
                        default:      result <= 512'd0;
                    endcase
                end

                // FP32/FP64 DOT and DOTACC run the tree from its products.
                if (mode_wide_fp && op_reg == MEX_TMUL &&
                    (funct_reg == TMUL_DOT || funct_reg == TMUL_DOTACC)) begin
                    fma_beat        <= 4'd0;
                    tree_phase      <= TREE_PRODUCTS;
                    tree_count      <= mode_fp64 ? 5'd8 : 5'd16;
                    tree_target     <= (funct_reg == TMUL_DOTACC) ? 3'd4 : 3'd1;
                    tree_accumulate <= tctrl_accumulate_reg &&
                                       !tctrl_acc_zero_reg;
                    state           <= S_TREE;
                end
                // DOT/DOTACC → accumulator, then done (no tile store)
                else if (op_reg == MEX_TMUL && funct_reg == TMUL_DOT) begin
                    tctrl_acc_zero_clear <= tctrl_acc_zero_reg;
                    z_kind_reg <= mode_fp ? Z_FP_ACC0 : Z_ALL_WORDS;
                    if (mode_fp) begin
                        // FP DOT: result is FP32 in low 32 bits of acc[0]
                        if (tctrl_acc_zero_reg) begin
                            acc[0] <= {32'd0, fp_dot_result};
                            acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end else if (tctrl_accumulate_reg) begin
                            // ACC_ACC adds the tile's tree result to the
                            // binary32 ACC0 with one rounding.
                            acc[0] <= {32'd0, fp_dot_acc_result};
                            acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end else begin
                            acc[0] <= {32'd0, fp_dot_result};
                            acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end
                    end else begin
                        if (tctrl_acc_zero_reg) begin
                            acc[0] <= dot_result; acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end else if (tctrl_accumulate_reg)
                            acc[0] <= acc[0] + dot_result;
                        else begin
                            acc[0] <= dot_result; acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end
                    end
                    state <= S_DONE;
                end
                else if (op_reg == MEX_TMUL && funct_reg == TMUL_DOTACC) begin
                    tctrl_acc_zero_clear <= tctrl_acc_zero_reg;
                    z_kind_reg <= mode_fp ? Z_FP_ALL : Z_ALL_WORDS;
                    if (mode_fp) begin
                        if (tctrl_acc_zero_reg) begin
                            acc[0] <= {32'd0, fp_dotacc_result[0]};
                            acc[1] <= {32'd0, fp_dotacc_result[1]};
                            acc[2] <= {32'd0, fp_dotacc_result[2]};
                            acc[3] <= {32'd0, fp_dotacc_result[3]};
                        end else if (tctrl_accumulate_reg) begin
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
                        if (tctrl_acc_zero_reg) begin
                            acc[0] <= dotacc[0]; acc[1] <= dotacc[1]; acc[2] <= dotacc[2]; acc[3] <= dotacc[3];
                        end else if (tctrl_accumulate_reg) begin
                            acc[0] <= acc[0]+dotacc[0]; acc[1] <= acc[1]+dotacc[1];
                            acc[2] <= acc[2]+dotacc[2]; acc[3] <= acc[3]+dotacc[3];
                        end else begin
                            acc[0] <= dotacc[0]; acc[1] <= dotacc[1]; acc[2] <= dotacc[2]; acc[3] <= dotacc[3];
                        end
                    end
                    state <= S_DONE;
                end
                else if (fma_op) begin
                    fma_beat <= 4'd0;
                    state    <= S_FMA;
                end
                else begin
                    state <= S_STORE;
                end
            end

            // One beat of the FP32/FP64 element-wise datapath: capture each
            // unit's lanes, then store after the last beat.
            S_FMA: begin
                for (fma_i = 0; fma_i < FMA_UNITS; fma_i = fma_i + 1) begin
                    if (mode_fp64) begin
                        fma_j = fma_beat * FMA_UNITS + fma_i;
                        result[fma_j*64 +: 64] <= fma_r0_bus[fma_i*64 +: 64];
                    end else begin
                        fma_j = fma_beat * (2 * FMA_UNITS) + 2 * fma_i;
                        if (!fma_is_wmul) begin
                            result[fma_j*32 +: 32] <=
                                fma_r0_bus[fma_i*64 +: 32];
                            result[(fma_j + 1)*32 +: 32] <=
                                fma_r1_bus[fma_i*64 +: 32];
                        end else if (fma_j < 8) begin
                            result[fma_j*64 +: 64] <=
                                fma_r0_bus[fma_i*64 +: 64];
                            result[(fma_j + 1)*64 +: 64] <=
                                fma_r1_bus[fma_i*64 +: 64];
                        end else begin
                            result2[(fma_j - 8)*64 +: 64] <=
                                fma_r0_bus[fma_i*64 +: 64];
                            result2[(fma_j - 7)*64 +: 64] <=
                                fma_r1_bus[fma_i*64 +: 64];
                        end
                    end
                end
                if (fma_beat == FMA_BEATS - 1)
                    state <= S_STORE;
                else
                    fma_beat <= fma_beat + 4'd1;
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
                        state <= S_STORE_WAIT;
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
                    if (dst2_internal) begin
                        tile_req   <= 1'b1;
                        tile_addr  <= tdst_second[31:0];
                        tile_wen   <= 1'b1;
                        tile_wdata <= result2;
                        state      <= S_STORE2_WAIT;
                    end else begin
                        ext_tile_req   <= 1'b1;
                        ext_tile_addr  <= tdst_second;
                        ext_tile_wen   <= 1'b1;
                        ext_tile_wdata <= result2;
                        state          <= S_EXT_STORE2_WAIT;
                    end
                end
            end

            S_STORE_WAIT: begin
                if (tile_ack)
                    state <= S_DONE;
            end

            S_STORE2_WAIT: begin
                if (tile_ack)
                    state <= S_DONE;
            end

            S_REDUCE: begin
                if (mode_wide_fp && (funct_reg == TRED_SUM ||
                                     funct_reg == TRED_L1 ||
                                     funct_reg == TRED_SUMSQ)) begin
                    // SUM and L1 leaves are the lanes widened exactly;
                    // SUMSQ starts from its products.
                    for (fma_i = 0; fma_i < 16; fma_i = fma_i + 1) begin
                        if (mode_fp64 && fma_i < 8)
                            tree_v[fma_i*64 +: 64] <=
                                (funct_reg == TRED_L1) ?
                                {1'b0, tile_a[fma_i*64 +: 63]} :
                                tile_a[fma_i*64 +: 64];
                        else if (!mode_fp64)
                            tree_v[fma_i*64 +: 64] <= fp32_to_fp64(
                                (funct_reg == TRED_L1) ?
                                {1'b0, tile_a[fma_i*32 +: 31]} :
                                tile_a[fma_i*32 +: 32]);
                    end
                    fma_beat        <= 4'd0;
                    tree_phase      <= (funct_reg == TRED_SUMSQ) ?
                                       TREE_PRODUCTS : TREE_LEVELS;
                    tree_count      <= mode_fp64 ? 5'd8 : 5'd16;
                    tree_target     <= 3'd1;
                    tree_accumulate <= tctrl_accumulate_reg &&
                                       !tctrl_acc_zero_reg;
                    state           <= S_TREE;
                end else if (mode_wide_fp && funct_reg != TRED_POPC) begin
                    // FP32/FP64 MIN, MAX, MINIDX, MAXIDX publish binary64.
                    tctrl_acc_zero_clear <= tctrl_acc_zero_reg;
                    if (funct_reg == TRED_MINIDX ||
                        funct_reg == TRED_MAXIDX) begin
                        z_kind_reg <= Z_ACC0;
                        if (tctrl_acc_zero_reg || !tctrl_accumulate_reg ||
                            fpw_red_index_replaces) begin
                            acc[0] <= fpw_red_found ? fpw_red_idx : 64'd0;
                            acc[1] <= fpw_red_val;
                        end
                    end else begin
                        z_kind_reg <= Z_FP64_ACC0;
                        acc[0] <= (!tctrl_acc_zero_reg && tctrl_accumulate_reg) ?
                                  fpw_red_extreme_acc : fpw_red_val;
                        acc[1] <= 64'd0;
                    end
                    acc[2] <= 64'd0;
                    acc[3] <= 64'd0;
                    state  <= S_DONE;
                end else begin
                tctrl_acc_zero_clear <= tctrl_acc_zero_reg;
                if (funct_reg == TRED_MINIDX || funct_reg == TRED_MAXIDX)
                    z_kind_reg <= Z_ACC0;
                else if (mode_fp && funct_reg != TRED_POPC)
                    z_kind_reg <= Z_FP_ACC0;
                else
                    z_kind_reg <= Z_ALL_WORDS;
                if (mode_fp && (funct_reg != TRED_POPC)) begin
                    // Floating reductions publish binary32 in ACC0
                    // (docs/floating-point.md §4.4-§4.5).
                    if (funct_reg == TRED_MINIDX || funct_reg == TRED_MAXIDX) begin
                        if (tctrl_acc_zero_reg || !tctrl_accumulate_reg ||
                            fp_red_index_replaces) begin
                            acc[0] <= fp_red_idx;
                            acc[1] <= {32'd0, fp_red_val};
                        end
                        acc[2] <= 64'd0; acc[3] <= 64'd0;
                    end else begin
                        if (tctrl_acc_zero_reg)
                            acc[0] <= {32'd0, fp_red_result};
                        else if (tctrl_accumulate_reg)
                            acc[0] <= {32'd0,
                                ((funct_reg == TRED_MIN) ||
                                 (funct_reg == TRED_MAX)) ?
                                fp_red_extreme_acc : fp_red_acc_result};
                        else
                            acc[0] <= {32'd0, fp_red_result};
                        acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                    end
                end else begin
                    // Integer reductions
                    if (funct_reg == TRED_MINIDX || funct_reg == TRED_MAXIDX) begin
                        if (tctrl_acc_zero_reg) begin
                            acc[0] <= red_idx; acc[1] <= red_val; acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end else if (tctrl_accumulate_reg) begin
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
                        if (tctrl_acc_zero_reg) begin
                            acc[0] <= red_result; acc[1] <= 64'd0; acc[2] <= 64'd0; acc[3] <= 64'd0;
                        end else if (tctrl_accumulate_reg) begin
                            // MIN/MAX keep a running extreme against ACC0
                            // (docs/floating-point.md §4.6).
                            if (funct_reg == TRED_MIN)
                                acc[0] <= (mode_signed ?
                                    ($signed(red_result) < $signed(acc[0])) :
                                    (red_result < acc[0])) ?
                                    red_result : acc[0];
                            else if (funct_reg == TRED_MAX)
                                acc[0] <= (mode_signed ?
                                    ($signed(red_result) > $signed(acc[0])) :
                                    (red_result > acc[0])) ?
                                    red_result : acc[0];
                            else
                                acc[0] <= acc[0] + red_result;
                        end else
                            acc[0] <= red_result;
                    end
                end
                state <= S_DONE;
                end
            end

            // TCVT (§6.3): read, convert sixteen lanes per beat, write, then
            // idle until the conversion has taken 4 + (k - 1) cycles.
            S_CVT_READ_WAIT: begin
                if (cvt_ext ? ext_tile_ack : tile_ack) begin
                    tile_a <= cvt_ext ? ext_tile_rdata : tile_rdata;
                    state  <= S_CVT_CONVERT;
                end
            end

            S_CVT_CONVERT: begin
                result     <= cvt_result_next;
                cvt_cycles <= cvt_cycles + 4'd1;
                if (!cvt_last_beat) begin
                    cvt_beat <= cvt_beat + 2'd1;
                end else begin
                    cvt_beat <= 2'd0;
                    if (cvt_narrow && (cvt_tile != cvt_k - 4'd1)) begin
                        cvt_tile <= cvt_tile + 4'd1;
                        cvt_ext  <= !address_internal(cvt_next_src_addr);
                        if (address_internal(cvt_next_src_addr)) begin
                            tile_req  <= 1'b1;
                            tile_addr <= cvt_next_src_addr[31:0];
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= cvt_next_src_addr;
                        end
                        state <= S_CVT_READ_WAIT;
                    end else begin
                        cvt_ext <= !address_internal(cvt_dst_addr);
                        if (address_internal(cvt_dst_addr)) begin
                            tile_req   <= 1'b1;
                            tile_addr  <= cvt_dst_addr[31:0];
                            tile_wen   <= 1'b1;
                            tile_wdata <= cvt_result_next;
                        end else begin
                            ext_tile_req   <= 1'b1;
                            ext_tile_addr  <= cvt_dst_addr;
                            ext_tile_wen   <= 1'b1;
                            ext_tile_wdata <= cvt_result_next;
                        end
                        state <= S_CVT_WRITE_WAIT;
                    end
                end
            end

            S_CVT_WRITE_WAIT: begin
                if (cvt_ext ? ext_tile_ack : tile_ack) begin
                    if (cvt_widen && (cvt_tile != cvt_k - 4'd1)) begin
                        cvt_tile <= cvt_tile + 4'd1;
                        state    <= S_CVT_CONVERT;
                    end else if (cvt_cycles == 4'd3 + cvt_k) begin
                        state <= S_DONE;
                    end else begin
                        state <= S_CVT_PAD;
                    end
                end
            end

            S_CVT_PAD: begin
                cvt_cycles <= cvt_cycles + 4'd1;
                if (cvt_cycles + 4'd1 == 4'd3 + cvt_k)
                    state <= S_DONE;
            end

            // One beat of the FP32/FP64 canonical tree (docs/floating-point.md
            // §4.3): products, then the levels, then the reserved ACC_ACC
            // beats, which also publish the result.
            S_TREE: begin
                case (tree_phase)
                    TREE_PRODUCTS: begin
                        for (fma_i = 0; fma_i < FMA_UNITS; fma_i = fma_i + 1) begin
                            if (mode_fp64) begin
                                fma_j = fma_beat * FMA_UNITS + fma_i;
                                tree_v[fma_j*64 +: 64] <=
                                    fma_r0_bus[fma_i*64 +: 64];
                            end else begin
                                fma_j = fma_beat * (2 * FMA_UNITS) + 2 * fma_i;
                                tree_v[fma_j*64 +: 64] <=
                                    fma_r0_bus[fma_i*64 +: 64];
                                tree_v[(fma_j + 1)*64 +: 64] <=
                                    fma_r1_bus[fma_i*64 +: 64];
                            end
                        end
                        if (fma_beat == FMA_BEATS - 1) begin
                            fma_beat   <= 4'd0;
                            tree_phase <= TREE_LEVELS;
                        end else begin
                            fma_beat <= fma_beat + 4'd1;
                        end
                    end
                    TREE_LEVELS: begin
                        for (fma_i = 0; fma_i < FMA_UNITS; fma_i = fma_i + 1) begin
                            fma_j = fma_beat * FMA_UNITS + fma_i;
                            if (fma_j < tree_count / 2)
                                tree_v[fma_j*64 +: 64] <=
                                    fma_r0_bus[fma_i*64 +: 64];
                        end
                        if ((fma_beat + 1) * FMA_UNITS >= tree_count / 2) begin
                            fma_beat   <= 4'd0;
                            tree_count <= tree_count / 2;
                            if (tree_count / 2 == tree_target)
                                tree_phase <= TREE_ACC;
                        end else begin
                            fma_beat <= fma_beat + 4'd1;
                        end
                    end
                    default: begin  // TREE_ACC
                        for (fma_i = 0; fma_i < FMA_UNITS; fma_i = fma_i + 1) begin
                            fma_j = fma_beat * FMA_UNITS + fma_i;
                            if (tree_accumulate && fma_j < tree_target)
                                tree_v[fma_j*64 +: 64] <=
                                    fma_r0_bus[fma_i*64 +: 64];
                        end
                        if ((fma_beat + 1) * FMA_UNITS >= tree_target) begin
                            // Publish: this beat's sums come from the units,
                            // earlier ones from tree_v.
                            for (fma_j = 0; fma_j < 4; fma_j = fma_j + 1) begin
                                if (fma_j >= tree_target)
                                    acc[fma_j] <= 64'd0;
                                else if (tree_accumulate &&
                                         fma_j >= fma_beat * FMA_UNITS)
                                    acc[fma_j] <= fma_r0_bus[
                                        (fma_j - fma_beat * FMA_UNITS)*64 +: 64];
                                else
                                    acc[fma_j] <= tree_v[fma_j*64 +: 64];
                            end
                            tctrl_acc_zero_clear <= tctrl_acc_zero_reg;
                            z_kind_reg <= (tree_target == 3'd4) ?
                                          Z_FP64_ALL : Z_FP64_ACC0;
                            fma_beat   <= 4'd0;
                            state      <= S_DONE;
                        end else begin
                            fma_beat <= fma_beat + 4'd1;
                        end
                    end
                endcase
            end

            // External memory paths
            S_EXT_LOAD_A: begin
                if (ext_tile_ack) begin
                    tile_a <= ext_tile_rdata;
                    if (op_reg == MEX_TRED) begin
                        if (ss_reg == 2'd2)
                            tile_a <= imm_splat;
                        state <= S_REDUCE;
                    end
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
                    else if (ss_reg == 2'd0 || ss_reg == 2'd3) begin
                        if ((ss_reg == 2'd0) ? src1_internal : src0_internal) begin
                            tile_req  <= 1'b1;
                            tile_addr <= (ss_reg == 2'd0) ?
                                         tsrc1[31:0] : tsrc0[31:0];
                            state     <= S_LOAD_B;
                        end else begin
                            ext_tile_req  <= 1'b1;
                            ext_tile_addr <= (ss_reg == 2'd0) ? tsrc1 : tsrc0;
                            state         <= S_EXT_LOAD_B;
                        end
                    end else begin
                        if (ss_reg == 2'd2) begin
                            tile_a <= imm_splat;
                            tile_b <= ext_tile_rdata;
                        end
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

            S_EXT_STORE: begin
                if (ext_tile_ack) begin
                    if (op_reg == MEX_TMUL && funct_reg == TMUL_WMUL) begin
                        if (dst2_internal) begin
                            tile_req   <= 1'b1;
                            tile_addr  <= tdst_second[31:0];
                            tile_wen   <= 1'b1;
                            tile_wdata <= result2;
                            state      <= S_STORE2_WAIT;
                        end else begin
                            ext_tile_req   <= 1'b1;
                            ext_tile_addr  <= tdst_second;
                            ext_tile_wen   <= 1'b1;
                            ext_tile_wdata <= result2;
                            state          <= S_EXT_STORE2_WAIT;
                        end
                    end else begin
                        state <= S_DONE;
                    end
                end
            end

            S_EXT_STORE2_WAIT: begin
                if (ext_tile_ack)
                    state <= S_DONE;
            end

            // TAMAC reads use the engine's private ordinary source
            // lane.  All required spans were validated before tamac_start, so
            // only acknowledged target errors can terminate these states.
            S_TAMAC_LOAD_A: begin
                if (tamac_source_ack) begin
                    if (tamac_source_error) begin
                        state <= S_TACC_WAIT;
                    end else begin
                        tile_a <= tamac_read_ext_reg ?
                                  ext_tile_rdata : tile_rdata;
                        tamac_beat_reg <= 4'd0;
                        if (ss_reg == 2'd1) begin
                            state <= S_TACC_INT;
                        end else begin
                            tamac_read_ext_reg <=
                                tamac_src_b_ext_reg;
                            if (tamac_src_b_ext_reg) begin
                                ext_tile_req  <= 1'b1;
                                ext_tile_addr <=
                                    tamac_src_b_addr_reg;
                            end else begin
                                tile_req  <= 1'b1;
                                tile_addr <=
                                    tamac_src_b_addr_reg[31:0];
                            end
                            state <= S_TAMAC_LOAD_B;
                        end
                    end
                end
            end

            S_TAMAC_LOAD_B: begin
                if (tamac_source_ack) begin
                    if (tamac_source_error) begin
                        state <= S_TACC_WAIT;
                    end else begin
                        tile_b <= tamac_read_ext_reg ?
                                  ext_tile_rdata : tile_rdata;
                        tamac_beat_reg <= 4'd0;
                        state <= S_TACC_INT;
                    end
                end
            end

            S_TACC_INT: begin
                case (tamac_ew_reg)
                    TMODE_8: begin
                        case (tamac_beat_reg)
                            2'd0: tile_c  <=
                                tamac_slice_result[511:0];
                            2'd1: result  <=
                                tamac_slice_result[511:0];
                            2'd2: result2 <=
                                tamac_slice_result[511:0];
                            2'd3: tile_a  <=
                                tamac_slice_result[511:0];
                            default: begin
                            end
                        endcase
                    end

                    TMODE_16: begin
                        if (tamac_beat_reg == 2'd0) begin
                            result  <= tamac_slice_result[511:0];
                            result2 <= tamac_slice_result[1023:512];
                        end else begin
                            tile_a <= tamac_slice_result[511:0];
                            tile_b <= tamac_slice_result[1023:512];
                        end
                    end

                    TMODE_32: begin
                        result  <= tamac_slice_result[511:0];
                        result2 <= tamac_slice_result[1023:512];
                    end

                    TMODE_FP16, TMODE_BF16: begin
                        // Even beats registered the selected exact product
                        // group. Odd beats consume the shared feedback bank.
                        if (tamac_beat_reg == 2'd1) begin
                            for (fp_tamac_result_lane = 0;
                                 fp_tamac_result_lane < 16;
                                 fp_tamac_result_lane =
                                     fp_tamac_result_lane + 1)
                                result[
                                    fp_tamac_result_lane*32 +: 32] <=
                                    fp_shared_l1[
                                        fp_tamac_result_lane];
                        end else if (tamac_beat_reg == 2'd3) begin
                            for (fp_tamac_result_lane = 0;
                                 fp_tamac_result_lane < 16;
                                 fp_tamac_result_lane =
                                     fp_tamac_result_lane + 1)
                                result2[
                                    fp_tamac_result_lane*32 +: 32] <=
                                    fp_shared_l1[
                                        fp_tamac_result_lane];
                        end
                    end

                    TMODE_FP32, TMODE_FP64: begin
                        // Lane j = beat * FMA_UNITS + unit accumulates in
                        // binary64: lanes 0-7 in result, 8-15 in result2.
                        for (fma_i = 0; fma_i < FMA_UNITS; fma_i = fma_i + 1) begin
                            fma_j = tamac_beat_reg * FMA_UNITS + fma_i;
                            if (fma_j < 8)
                                result[fma_j*64 +: 64] <=
                                    fma_r0_bus[fma_i*64 +: 64];
                            else
                                result2[(fma_j - 8)*64 +: 64] <=
                                    fma_r0_bus[fma_i*64 +: 64];
                        end
                    end

                    default: begin
                    end
                endcase

                if (tamac_last_beat)
                    state <= S_TACC_WAIT;
                else
                    tamac_beat_reg <= tamac_beat_reg + 4'd1;
            end

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
                    state     <= S_STORE2D_WRITE_WAIT;
                end
            end

            S_STORE2D_WRITE_WAIT: begin
                if (tile_ack) begin
                    // Commit row progress only after the read-modify-write
                    // reaches memory.  This prevents the following row read
                    // from being mistaken for the outstanding write response.
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

            S_TACC_WAIT: begin
                if (tacc_tamac_start) begin
                    // A FORCE accepted beside the original dispatch may have
                    // delayed admission.  Reuse the captured request context;
                    // live CPU buses are intentionally ignored here.
                    tamac_src_b_ext_reg <= tamac_src_b_ext;
                    tamac_read_ext_reg  <= tamac_src_a_ext;
                    tamac_beat_reg      <= 4'd0;
                    if (tamac_src_a_ext) begin
                        ext_tile_req  <= 1'b1;
                        ext_tile_addr <= tamac_src_a_addr_reg;
                    end else begin
                        tile_req  <= 1'b1;
                        tile_addr <= tamac_src_a_addr_reg[31:0];
                    end
                    mex_busy_reg <= 1'b1;
                    state <= S_TAMAC_LOAD_A;
                end else if (tacc_req_done && mex_retire) begin
                    mex_busy_reg <= 1'b0;
                    state        <= S_IDLE;
                end else begin
                    mex_busy_reg <= 1'b1;
                    state        <= S_TACC_WAIT;
                end
            end

            S_DONE: begin
                mex_done_reg <= 1'b1;
                mex_busy_reg <= 1'b0;
                mex_zero_valid_reg <= (z_kind_reg != Z_NONE);
                case (z_kind_reg)
                    Z_ACC0:
                        mex_zero_reg <= (acc[0] == 64'd0);
                    Z_FP_ACC0:
                        mex_zero_reg <= (acc[0][30:0] == 31'd0);
                    Z_FP_ALL:
                        mex_zero_reg <= (acc[0][30:0] == 31'd0) &&
                                        (acc[1][30:0] == 31'd0) &&
                                        (acc[2][30:0] == 31'd0) &&
                                        (acc[3][30:0] == 31'd0);
                    Z_FP64_ACC0:
                        mex_zero_reg <= (acc[0][62:0] == 63'd0);
                    Z_FP64_ALL:
                        mex_zero_reg <= (acc[0][62:0] == 63'd0) &&
                                        (acc[1][62:0] == 63'd0) &&
                                        (acc[2][62:0] == 63'd0) &&
                                        (acc[3][62:0] == 63'd0);
                    default:
                        mex_zero_reg <= (acc[0] == 64'd0) &&
                                        (acc[1] == 64'd0) &&
                                        (acc[2] == 64'd0) &&
                                        (acc[3] == 64'd0);
                endcase
                state        <= S_IDLE;
            end
            default: state <= S_IDLE;
            endcase
            end
        end
    end

endmodule
