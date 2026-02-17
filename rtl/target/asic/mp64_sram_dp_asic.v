// ============================================================================
// mp64_sram_dp_asic.v — ASIC Dual-Port SRAM Stub
// ============================================================================
// Replace body with foundry dual-port SRAM compiler macro.
// Port interface identical to rtl/prim/mp64_sram_dp.v.
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
    input  wire                ce_a,
    input  wire                we_a,
    input  wire [ADDR_W-1:0]   addr_a,
    input  wire [DATA_W-1:0]   wdata_a,
    output wire [DATA_W-1:0]   rdata_a,
    input  wire                ce_b,
    input  wire                we_b,
    input  wire [ADDR_W-1:0]   addr_b,
    input  wire [DATA_W-1:0]   wdata_b,
    output wire [DATA_W-1:0]   rdata_b
);

    // STUB — behavioural fallback
    reg [DATA_W-1:0] mem [0:DEPTH-1];

    reg [DATA_W-1:0] rdata_a_r, rdata_b_r;
    always @(posedge clk) begin
        if (ce_a) begin
            if (we_a) mem[addr_a] <= wdata_a;
            rdata_a_r <= mem[addr_a];
        end
    end
    always @(posedge clk) begin
        if (ce_b) begin
            if (we_b) mem[addr_b] <= wdata_b;
            rdata_b_r <= mem[addr_b];
        end
    end

    assign rdata_a = rdata_a_r;
    assign rdata_b = rdata_b_r;

endmodule
