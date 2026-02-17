// ============================================================================
// mp64_sram_sp_xilinx7.v — Xilinx 7-Series Block RAM Single-Port Override
// ============================================================================
//
// Drop-in replacement for rtl/prim/mp64_sram_sp.v targeting Xilinx 7-series.
// Uses (* ram_style = "block" *) attribute to force BRAM inference.
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

    (* ram_style = "block" *) reg [DATA_W-1:0] mem [0:DEPTH-1];

    generate
        if (INIT_FILE != "") begin : g_init
            initial $readmemh(INIT_FILE, mem);
        end
    endgenerate

    reg [DATA_W-1:0] rdata_r;

    always @(posedge clk) begin
        if (ce) begin
            if (we)
                mem[addr] <= wdata;
            rdata_r <= mem[addr];
        end
    end

    generate
        if (OUT_REG) begin : g_outreg
            reg [DATA_W-1:0] rdata_q;
            always @(posedge clk) begin
                if (!rst_n)
                    rdata_q <= {DATA_W{1'b0}};
                else
                    rdata_q <= rdata_r;
            end
            assign rdata = rdata_q;
        end else begin : g_noreg
            assign rdata = rdata_r;
        end
    endgenerate

endmodule
