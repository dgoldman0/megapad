// ============================================================================
// mp64_tile.v — Megapad-64 Tile Engine (MEX Unit)
// ============================================================================
//
// The tile engine is the primary compute accelerator.  It performs SIMD
// operations on 64-byte tiles (512 bits), processing up to 64 parallel
// lanes in 8-bit mode.
//
// Key design: the tile engine has a DEDICATED 512-bit port into the
// internal BRAM, separate from the CPU's 64-bit port.  This means:
//   - Tile loads are 1 cycle (vs 8 cycles if using the CPU port)
//   - CPU and tile engine can access memory simultaneously
//   - Tile operations on internal memory: 3 cycles (load A, load B, compute+store)
//   - Tile operations on external memory: ~24+ cycles (burst reads)
//
// Architecture:
//   ┌──────────────────────────────────────────────┐
//   │              Tile Engine                      │
//   │                                               │
//   │  ┌─────────┐  ┌─────────┐  ┌─────────┐      │
//   │  │ TSRC0   │  │ TSRC1   │  │  TDST   │      │
//   │  │ reg     │  │ reg     │  │  reg    │      │
//   │  └────┬────┘  └────┬────┘  └────┬────┘      │
//   │       │             │            │            │
//   │  ┌────▼────┐  ┌────▼────┐       │            │
//   │  │ Tile A  │  │ Tile B  │       │            │
//   │  │ 512-bit │  │ 512-bit │       │            │
//   │  │ latch   │  │ latch   │       │            │
//   │  └────┬────┘  └────┬────┘       │            │
//   │       │             │            │            │
//   │  ┌────▼─────────────▼────┐       │            │
//   │  │   Lane ALU Array      │       │            │
//   │  │   64× 8-bit   or     │       │            │
//   │  │   32× 16-bit  or     │  ┌────▼────┐      │
//   │  │   16× 32-bit  or     │  │ Result  │      │
//   │  │    8× 64-bit         ├──► 512-bit │      │
//   │  │                      │  │ latch   │      │
//   │  └──────────┬───────────┘  └────┬────┘      │
//   │             │                    │            │
//   │        ┌────▼────┐              │            │
//   │        │ Accum   │              │            │
//   │        │ 256-bit │              │            │
//   │        └─────────┘              │            │
//   │                                  │            │
//   │            512-bit tile bus ◄────┘            │
//   └──────────────────────────────────────────────┘
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
    reg [63:0] tsrc0;        // source tile A address
    reg [63:0] tsrc1;        // source tile B address
    reg [63:0] tdst;         // destination tile address
    reg [63:0] tmode;        // bits[1:0]=width, bit[4]=signed
    reg [63:0] tctrl;        // bit[0]=accumulate, bit[1]=acc_zero
    reg [63:0] acc [0:3];    // 256-bit accumulator (4 × 64-bit)
    reg [63:0] tile_bank;    // SB — tile bank selector
    reg [63:0] tile_row;     // SR — cursor row
    reg [63:0] tile_col;     // SC — cursor col
    reg [63:0] tile_stride;  // SW — cursor stride

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
            endcase
        end
    end

    // ========================================================================
    // Address classification: internal vs external
    // ========================================================================
    wire src0_internal = (tsrc0[63:20] == 44'd0);
    wire src1_internal = (tsrc1[63:20] == 44'd0);
    wire dst_internal  = (tdst[63:20]  == 44'd0);

    // ========================================================================
    // Tile operation state machine
    // ========================================================================
    //
    // For internal memory operations (typical case):
    //   Cycle 1: Load tile A from TSRC0 (Port A read)
    //   Cycle 2: Load tile B from TSRC1 (Port A read)
    //   Cycle 3: Compute + store result to TDST (Port A write)
    //
    // For reductions (TSUM, TMIN, TMAX):
    //   Cycle 1: Load tile A from TSRC0
    //   Cycle 2: Reduce → accumulator
    //
    // For external memory: uses burst interface, much slower (~8× per tile)
    //

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

    reg [3:0]   state;
    reg [511:0] tile_a;          // source tile A latch
    reg [511:0] tile_b;          // source tile B latch
    reg [511:0] result;          // computation result
    reg [1:0]   op_reg;          // latched operation class
    reg [2:0]   funct_reg;       // latched sub-function
    reg [1:0]   ss_reg;          // latched source selector
    reg [63:0]  gpr_val_reg;     // latched GPR value
    reg [7:0]   imm8_reg;        // latched immediate

    // Source B selection: tile from TSRC1, GPR broadcast, or imm8 splat
    reg [511:0] src_b_selected;

    always @(*) begin
        case (ss_reg)
            2'd0: src_b_selected = tile_b;              // TSRC1 tile
            2'd1: begin                                  // GPR broadcast
                src_b_selected = {8{gpr_val_reg}};       // replicate 64-bit value
            end
            2'd2: begin                                  // imm8 splat
                src_b_selected = {64{imm8_reg}};         // replicate 8-bit value
            end
            2'd3: src_b_selected = tile_a;               // in-place (DST=A, SRC0=B)
            default: src_b_selected = 512'd0;
        endcase
    end

    // ========================================================================
    // Lane ALU — parallel computation across all lanes
    // ========================================================================
    //
    // 8-bit mode: 64 parallel lanes
    // 16-bit mode: 32 parallel lanes
    // 32-bit mode: 16 parallel lanes
    // 64-bit mode: 8 parallel lanes
    //
    // We generate all four widths and mux based on tmode.
    //

    wire [1:0] mode_width = tmode[1:0];
    wire       mode_signed = tmode[4];
    wire       mode_saturate = tmode[5];

    // --- 8-bit lane ALU (64 lanes) ---
    reg [511:0] alu_result_8;
    reg [63:0]  red_result_8;    // reduction result
    reg [63:0]  red_idx_8;       // reduction index (for MINIDX/MAXIDX)
    reg [63:0]  red_val_8;       // reduction value (for MINIDX/MAXIDX)
    integer lane8;

    always @(*) begin
        alu_result_8 = 512'd0;
        red_result_8 = 64'd0;
        red_idx_8    = 64'd0;
        red_val_8    = 64'd0;

        // ALU operations
        for (lane8 = 0; lane8 < 64; lane8 = lane8 + 1) begin
            case (funct_reg)
                TALU_ADD: begin
                    if (mode_saturate) begin
                        // Unsigned saturating add: clamp at 255
                        if (({1'b0, tile_a[lane8*8 +: 8]} + {1'b0, src_b_selected[lane8*8 +: 8]}) > 9'd255)
                            alu_result_8[lane8*8 +: 8] = 8'hFF;
                        else
                            alu_result_8[lane8*8 +: 8] =
                                tile_a[lane8*8 +: 8] + src_b_selected[lane8*8 +: 8];
                    end else
                        alu_result_8[lane8*8 +: 8] =
                            tile_a[lane8*8 +: 8] + src_b_selected[lane8*8 +: 8];
                end
                TALU_SUB: begin
                    if (mode_saturate) begin
                        // Unsigned saturating sub: clamp at 0
                        if (tile_a[lane8*8 +: 8] < src_b_selected[lane8*8 +: 8])
                            alu_result_8[lane8*8 +: 8] = 8'h00;
                        else
                            alu_result_8[lane8*8 +: 8] =
                                tile_a[lane8*8 +: 8] - src_b_selected[lane8*8 +: 8];
                    end else
                        alu_result_8[lane8*8 +: 8] =
                            tile_a[lane8*8 +: 8] - src_b_selected[lane8*8 +: 8];
                end
                TALU_AND: alu_result_8[lane8*8 +: 8] =
                    tile_a[lane8*8 +: 8] & src_b_selected[lane8*8 +: 8];
                TALU_OR:  alu_result_8[lane8*8 +: 8] =
                    tile_a[lane8*8 +: 8] | src_b_selected[lane8*8 +: 8];
                TALU_XOR: alu_result_8[lane8*8 +: 8] =
                    tile_a[lane8*8 +: 8] ^ src_b_selected[lane8*8 +: 8];
                TALU_MIN: alu_result_8[lane8*8 +: 8] =
                    (tile_a[lane8*8 +: 8] < src_b_selected[lane8*8 +: 8])
                    ? tile_a[lane8*8 +: 8] : src_b_selected[lane8*8 +: 8];
                TALU_MAX: alu_result_8[lane8*8 +: 8] =
                    (tile_a[lane8*8 +: 8] > src_b_selected[lane8*8 +: 8])
                    ? tile_a[lane8*8 +: 8] : src_b_selected[lane8*8 +: 8];
                default:  alu_result_8[lane8*8 +: 8] = 8'd0;
            endcase
        end

        // Reduction (tree would be better, but this is clear for prototyping)
        case (funct_reg)
            TRED_SUM: begin
                red_result_8 = 64'd0;
                for (lane8 = 0; lane8 < 64; lane8 = lane8 + 1)
                    red_result_8 = red_result_8 + {56'd0, tile_a[lane8*8 +: 8]};
            end
            TRED_MIN: begin
                red_result_8 = {56'd0, tile_a[7:0]};
                for (lane8 = 1; lane8 < 64; lane8 = lane8 + 1)
                    if ({56'd0, tile_a[lane8*8 +: 8]} < red_result_8)
                        red_result_8 = {56'd0, tile_a[lane8*8 +: 8]};
            end
            TRED_MAX: begin
                red_result_8 = {56'd0, tile_a[7:0]};
                for (lane8 = 1; lane8 < 64; lane8 = lane8 + 1)
                    if ({56'd0, tile_a[lane8*8 +: 8]} > red_result_8)
                        red_result_8 = {56'd0, tile_a[lane8*8 +: 8]};
            end
            TRED_SUMSQ: begin
                red_result_8 = 64'd0;
                for (lane8 = 0; lane8 < 64; lane8 = lane8 + 1)
                    red_result_8 = red_result_8 +
                        ({56'd0, tile_a[lane8*8 +: 8]} * {56'd0, tile_a[lane8*8 +: 8]});
            end
            TRED_MINIDX: begin
                red_val_8 = {56'd0, tile_a[7:0]};
                red_idx_8 = 64'd0;
                for (lane8 = 1; lane8 < 64; lane8 = lane8 + 1)
                    if ({56'd0, tile_a[lane8*8 +: 8]} < red_val_8) begin
                        red_val_8 = {56'd0, tile_a[lane8*8 +: 8]};
                        red_idx_8 = lane8[63:0];
                    end
            end
            TRED_MAXIDX: begin
                red_val_8 = {56'd0, tile_a[7:0]};
                red_idx_8 = 64'd0;
                for (lane8 = 1; lane8 < 64; lane8 = lane8 + 1)
                    if ({56'd0, tile_a[lane8*8 +: 8]} > red_val_8) begin
                        red_val_8 = {56'd0, tile_a[lane8*8 +: 8]};
                        red_idx_8 = lane8[63:0];
                    end
            end
            default: red_result_8 = 64'd0;
        endcase
    end

    // ========================================================================
    // Main state machine
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state     <= S_IDLE;
            mex_done  <= 1'b0;
            mex_busy  <= 1'b0;
            tile_req  <= 1'b0;
            tile_wen  <= 1'b0;
            tile_a    <= 512'd0;
            tile_b    <= 512'd0;
            result    <= 512'd0;
            ext_tile_req <= 1'b0;
        end else begin
            mex_done <= 1'b0;
            tile_req <= 1'b0;
            tile_wen <= 1'b0;
            ext_tile_req <= 1'b0;

            case (state)
                // --------------------------------------------------------
                S_IDLE: begin
                    mex_busy <= 1'b0;
                    if (mex_valid) begin
                        // Latch instruction parameters
                        op_reg      <= mex_op;
                        funct_reg   <= mex_funct;
                        ss_reg      <= mex_ss;
                        gpr_val_reg <= mex_gpr_val;
                        imm8_reg    <= mex_imm8;
                        mex_busy    <= 1'b1;

                        // Special case: TSYS.ZERO — just write zeros to TDST
                        if (mex_op == MEX_TSYS && mex_funct == 3'd4) begin
                            tile_req  <= 1'b1;
                            tile_addr <= tdst[19:0];
                            tile_wen  <= 1'b1;
                            tile_wdata<= 512'd0;
                            state     <= S_DONE;
                        end
                        // Reductions only need tile A
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
                        // ALU/MUL need both tile A and tile B
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

                // --------------------------------------------------------
                S_LOAD_A: begin
                    if (tile_ack) begin
                        tile_a <= tile_rdata;
                        // If reduction, go straight to compute
                        if (op_reg == MEX_TRED) begin
                            state <= S_REDUCE;
                        end
                        // If source B is from tile (ss=0), load it
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
                        // Source B is GPR/imm/in-place — skip load B
                        else begin
                            state <= S_COMPUTE;
                        end
                    end
                end

                // --------------------------------------------------------
                S_LOAD_B: begin
                    if (tile_ack) begin
                        tile_b <= tile_rdata;
                        state  <= S_COMPUTE;
                    end
                end

                // --------------------------------------------------------
                S_COMPUTE: begin
                    // ALU result is combinationally ready (from lane ALU)
                    result <= alu_result_8;  // TODO: mux based on mode_width
                    state  <= S_STORE;
                end

                // --------------------------------------------------------
                S_STORE: begin
                    if (dst_internal) begin
                        tile_req   <= 1'b1;
                        tile_addr  <= tdst[19:0];
                        tile_wen   <= 1'b1;
                        tile_wdata <= result;
                        state      <= S_DONE;
                    end else begin
                        ext_tile_req   <= 1'b1;
                        ext_tile_addr  <= tdst;
                        ext_tile_wen   <= 1'b1;
                        ext_tile_wdata <= result;
                        state          <= S_EXT_STORE;
                    end
                end

                // --------------------------------------------------------
                S_REDUCE: begin
                    // Reduction: tile_a is loaded, compute reduction
                    if (funct_reg == TRED_MINIDX || funct_reg == TRED_MAXIDX) begin
                        // MINIDX/MAXIDX: acc[0]=index, acc[1]=value
                        if (tctrl[1]) begin  // ACC_ZERO
                            acc[0] <= red_idx_8;
                            acc[1] <= red_val_8;
                            acc[2] <= 64'd0;
                            acc[3] <= 64'd0;
                        end else if (tctrl[0]) begin  // ACC_ACC — compare with running
                            if (funct_reg == TRED_MINIDX) begin
                                if (red_val_8 < acc[1]) begin
                                    acc[0] <= red_idx_8;
                                    acc[1] <= red_val_8;
                                end
                            end else begin  // MAXIDX
                                if (red_val_8 > acc[1]) begin
                                    acc[0] <= red_idx_8;
                                    acc[1] <= red_val_8;
                                end
                            end
                        end else begin
                            acc[0] <= red_idx_8;
                            acc[1] <= red_val_8;
                        end
                    end else begin
                        // Standard scalar reductions
                        if (tctrl[1]) begin  // ACC_ZERO
                            acc[0] <= red_result_8;
                            acc[1] <= 64'd0;
                            acc[2] <= 64'd0;
                            acc[3] <= 64'd0;
                        end else if (tctrl[0]) begin  // ACC_ACC (accumulate)
                            acc[0] <= acc[0] + red_result_8;
                        end else begin
                            acc[0] <= red_result_8;
                        end
                    end
                    state <= S_DONE;
                end

                // --------------------------------------------------------
                // External memory paths (slower)
                // --------------------------------------------------------
                S_EXT_LOAD_A: begin
                    if (ext_tile_ack) begin
                        tile_a <= ext_tile_rdata;
                        if (op_reg == MEX_TRED) begin
                            state <= S_REDUCE;
                        end else if (ss_reg == 2'd0) begin
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
                            state <= S_COMPUTE;
                        end
                    end
                end

                S_EXT_LOAD_B: begin
                    if (ext_tile_ack) begin
                        tile_b <= ext_tile_rdata;
                        state  <= S_COMPUTE;
                    end
                end

                S_EXT_STORE: begin
                    if (ext_tile_ack) begin
                        state <= S_DONE;
                    end
                end

                // --------------------------------------------------------
                S_DONE: begin
                    mex_done <= 1'b1;
                    mex_busy <= 1'b0;
                    state    <= S_IDLE;
                end
            endcase
        end
    end

endmodule
