// ============================================================================
// mp64_sram_dp_xilinx7.v — Xilinx 7-Series Block RAM Dual-Port Override
// ============================================================================
//
// Drop-in replacement for rtl/prim/mp64_sram_dp.v targeting Xilinx 7-series.
//
// The portable primitive uses a single wide array (e.g. 512-bit rows).
// Yosys cannot infer 512-bit-wide BRAMs and flattens them to flip-flops.
//
// This wrapper decomposes the wide Port A into RATIO narrow BRAM slices
// (each DATA_W_B bits wide) that Yosys can map to RAMB36E1 / RAMB18E1.
//
//   Port A (wide):  reads/writes ALL slices in parallel → DATA_W_A bits
//   Port B (narrow): addresses ONE slice via low-order bits → DATA_W_B bits
//
// Port interface identical to rtl/prim/mp64_sram_dp.v.
//

module mp64_sram_dp #(
    parameter ADDR_W_A  = 14,           // Port A address bits
    parameter DATA_W_A  = 512,          // Port A data width (wide)
    parameter ADDR_W_B  = 17,           // Port B address bits
    parameter DATA_W_B  = 64,           // Port B data width (narrow)
    parameter DEPTH_A   = (1 << ADDR_W_A),
    parameter OUT_REG_A = 0,
    parameter OUT_REG_B = 0,
    parameter INIT_FILE = ""
)(
    input  wire                  clk,
    input  wire                  rst_n,

    // === Port A (wide, e.g. tile engine) ===
    input  wire                  a_ce,
    input  wire                  a_we,
    input  wire [ADDR_W_A-1:0]  a_addr,
    input  wire [DATA_W_A-1:0]  a_wdata,
    output wire [DATA_W_A-1:0]  a_rdata,

    // === Port B (narrow, e.g. CPU) ===
    input  wire                  b_ce,
    input  wire                  b_we,
    input  wire [ADDR_W_B-1:0]  b_addr,
    input  wire [DATA_W_B-1:0]  b_wdata,
    output wire [DATA_W_B-1:0]  b_rdata
);

    // ========================================================================
    // Derived parameters
    // ========================================================================
    localparam RATIO = DATA_W_A / DATA_W_B;     // e.g. 512/64 = 8
    localparam SEL_W = (RATIO > 1) ? $clog2(RATIO) : 1;

    // ========================================================================
    // Decompose into RATIO narrow dual-port BRAMs
    // ========================================================================
    // Each slice: DEPTH_A × DATA_W_B, true dual-port
    //   - Port A side: all slices enabled in parallel (wide read/write)
    //   - Port B side: only the selected slice is enabled (narrow read/write)

    wire [DATA_W_B-1:0] slice_a_rdata [0:RATIO-1];
    wire [DATA_W_B-1:0] slice_b_rdata [0:RATIO-1];

    // Port B slice select and row address
    wire [ADDR_W_A-1:0] b_row_addr;
    wire [SEL_W-1:0]    b_sel;

    generate
        if (RATIO > 1) begin : g_asymmetric
            assign b_row_addr = b_addr[ADDR_W_B-1 -: ADDR_W_A];
            assign b_sel      = b_addr[SEL_W-1:0];
        end else begin : g_symmetric
            assign b_row_addr = b_addr;
            assign b_sel      = 1'b0;
        end
    endgenerate

    genvar si;
    generate
        for (si = 0; si < RATIO; si = si + 1) begin : g_slice

            // Each slice is a narrow true dual-port BRAM
            (* ram_style = "block" *)
            reg [DATA_W_B-1:0] mem [0:DEPTH_A-1];

            // --- Port A (wide side — all slices in parallel) ----------------
            reg [DATA_W_B-1:0] a_rd_r;
            always @(posedge clk) begin
                if (a_ce) begin
                    if (a_we)
                        mem[a_addr] <= a_wdata[si*DATA_W_B +: DATA_W_B];
                    a_rd_r <= mem[a_addr];
                end
            end

            // --- Port B (narrow side — only selected slice) -----------------
            wire b_slice_ce = b_ce && (b_sel == si[SEL_W-1:0]);

            reg [DATA_W_B-1:0] b_rd_r;
            always @(posedge clk) begin
                if (b_slice_ce) begin
                    if (b_we)
                        mem[b_row_addr] <= b_wdata;
                    b_rd_r <= mem[b_row_addr];
                end
            end

            assign slice_a_rdata[si] = a_rd_r;
            assign slice_b_rdata[si] = b_rd_r;

        end
    endgenerate

    // ========================================================================
    // Reassemble Port A wide read data
    // ========================================================================
    reg [DATA_W_A-1:0] a_rdata_cat;
    integer ai;
    always @(*) begin
        for (ai = 0; ai < RATIO; ai = ai + 1)
            a_rdata_cat[ai*DATA_W_B +: DATA_W_B] = slice_a_rdata[ai];
    end

    // ========================================================================
    // Port B read data — mux from the previously-selected slice
    // ========================================================================
    // Capture the slice select for one cycle to align with BRAM output
    reg [SEL_W-1:0] b_sel_q;
    always @(posedge clk) begin
        if (b_ce)
            b_sel_q <= b_sel;
    end

    wire [DATA_W_B-1:0] b_rdata_raw = slice_b_rdata[b_sel_q];

    // ========================================================================
    // Optional output registers
    // ========================================================================
    generate
        if (OUT_REG_A) begin : g_outa
            reg [DATA_W_A-1:0] a_rdata_q;
            always @(posedge clk) begin
                if (!rst_n) a_rdata_q <= {DATA_W_A{1'b0}};
                else        a_rdata_q <= a_rdata_cat;
            end
            assign a_rdata = a_rdata_q;
        end else begin : g_norega
            assign a_rdata = a_rdata_cat;
        end

        if (OUT_REG_B) begin : g_outb
            reg [DATA_W_B-1:0] b_rdata_q;
            always @(posedge clk) begin
                if (!rst_n) b_rdata_q <= {DATA_W_B{1'b0}};
                else        b_rdata_q <= b_rdata_raw;
            end
            assign b_rdata = b_rdata_q;
        end else begin : g_noregb
            assign b_rdata = b_rdata_raw;
        end
    endgenerate

endmodule
