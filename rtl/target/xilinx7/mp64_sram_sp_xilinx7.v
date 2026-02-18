// ============================================================================
// mp64_sram_sp_xilinx7.v — Xilinx 7-Series Block RAM Single-Port Override
// ============================================================================
//
// Drop-in replacement for rtl/prim/mp64_sram_sp.v targeting Xilinx 7-series.
//
// When DATA_W > 72 (the widest native RAMB36E1 port), this wrapper
// decomposes the array into ceil(DATA_W/64) narrow BRAM slices so
// Yosys (and Vivado) can infer block RAMs instead of flattening to FFs.
//
// Port interface identical to mp64_sram_sp.v.
//

module mp64_sram_sp #(
    parameter ADDR_W    = 14,
    parameter DATA_W    = 512,
    parameter DEPTH     = (1 << ADDR_W),
    parameter OUT_REG   = 0,
    parameter INIT_FILE = ""
)(
    input  wire                clk,
    input  wire                rst_n,
    input  wire                ce,
    input  wire                we,
    input  wire [ADDR_W-1:0]   addr,
    input  wire [DATA_W-1:0]   wdata,
    output wire [DATA_W-1:0]   rdata
);

    // ========================================================================
    // Slice parameters
    // ========================================================================
    // Each RAMB36E1 supports up to 72-bit data width (64 data + 8 parity).
    // We split into 64-bit slices; the last slice may be narrower.
    localparam SLICE_W   = 64;
    localparam NUM_SLICE = (DATA_W + SLICE_W - 1) / SLICE_W;

    // ========================================================================
    // Generate BRAM slices — all addressed in parallel
    // ========================================================================
    wire [DATA_W-1:0] rdata_cat;

    genvar si;
    generate
        for (si = 0; si < NUM_SLICE; si = si + 1) begin : g_slice

            // Last slice may be narrower than SLICE_W
            localparam LO  = si * SLICE_W;
            localparam HI  = (LO + SLICE_W > DATA_W) ? DATA_W : LO + SLICE_W;
            localparam S_W = HI - LO;

            (* ram_style = "block" *)
            reg [S_W-1:0] mem [0:DEPTH-1];

            reg [S_W-1:0] rdata_r;

            always @(posedge clk) begin
                if (ce) begin
                    if (we)
                        mem[addr] <= wdata[HI-1:LO];
                    rdata_r <= mem[addr];
                end
            end

            assign rdata_cat[HI-1:LO] = rdata_r;

        end
    endgenerate

    // ========================================================================
    // Optional output register
    // ========================================================================
    generate
        if (OUT_REG) begin : g_outreg
            reg [DATA_W-1:0] rdata_q;
            always @(posedge clk) begin
                if (!rst_n)
                    rdata_q <= {DATA_W{1'b0}};
                else
                    rdata_q <= rdata_cat;
            end
            assign rdata = rdata_q;
        end else begin : g_noreg
            assign rdata = rdata_cat;
        end
    endgenerate

endmodule
