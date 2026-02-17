// ============================================================================
// mp64_sram_dp_xilinx7.v — Xilinx 7-Series Block RAM Dual-Port Override
// ============================================================================
//
// Drop-in replacement for rtl/prim/mp64_sram_dp.v targeting Xilinx 7-series.
// Uses (* ram_style = "block" *) attribute to force BRAM inference.
//

module mp64_sram_dp #(
    parameter ADDR_W    = 14,
    parameter DATA_W    = 64,
    parameter DEPTH     = (1 << ADDR_W),
    parameter OUT_REG   = 0,
    parameter INIT_FILE = ""
)(
    input  wire                clk,
    input  wire                rst_n,

    // Port A (read/write)
    input  wire                ce_a,
    input  wire                we_a,
    input  wire [ADDR_W-1:0]   addr_a,
    input  wire [DATA_W-1:0]   wdata_a,
    output wire [DATA_W-1:0]   rdata_a,

    // Port B (read/write)
    input  wire                ce_b,
    input  wire                we_b,
    input  wire [ADDR_W-1:0]   addr_b,
    input  wire [DATA_W-1:0]   wdata_b,
    output wire [DATA_W-1:0]   rdata_b
);

    (* ram_style = "block" *) reg [DATA_W-1:0] mem [0:DEPTH-1];

    generate
        if (INIT_FILE != "") begin : g_init
            initial $readmemh(INIT_FILE, mem);
        end
    endgenerate

    // Port A
    reg [DATA_W-1:0] rdata_a_r;
    always @(posedge clk) begin
        if (ce_a) begin
            if (we_a) mem[addr_a] <= wdata_a;
            rdata_a_r <= mem[addr_a];
        end
    end

    // Port B
    reg [DATA_W-1:0] rdata_b_r;
    always @(posedge clk) begin
        if (ce_b) begin
            if (we_b) mem[addr_b] <= wdata_b;
            rdata_b_r <= mem[addr_b];
        end
    end

    generate
        if (OUT_REG) begin : g_outreg
            reg [DATA_W-1:0] rdata_a_q, rdata_b_q;
            always @(posedge clk) begin
                if (!rst_n) begin
                    rdata_a_q <= {DATA_W{1'b0}};
                    rdata_b_q <= {DATA_W{1'b0}};
                end else begin
                    rdata_a_q <= rdata_a_r;
                    rdata_b_q <= rdata_b_r;
                end
            end
            assign rdata_a = rdata_a_q;
            assign rdata_b = rdata_b_q;
        end else begin : g_noreg
            assign rdata_a = rdata_a_r;
            assign rdata_b = rdata_b_r;
        end
    endgenerate

endmodule
