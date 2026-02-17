// ============================================================================
// mp64_sram_sp_asic.v — ASIC Single-Port SRAM Stub
// ============================================================================
// Replace body with foundry SRAM compiler macro.
// Port interface identical to rtl/prim/mp64_sram_sp.v.
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

    // STUB — behavioural fallback (replace with SRAM compiler macro)
    reg [DATA_W-1:0] mem [0:DEPTH-1];

    reg [DATA_W-1:0] rdata_r;
    always @(posedge clk) begin
        if (ce) begin
            if (we) mem[addr] <= wdata;
            rdata_r <= mem[addr];
        end
    end

    assign rdata = rdata_r;

endmodule
