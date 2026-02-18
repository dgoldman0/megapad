// ============================================================================
// mp64_sram_dp_asic.v — ASIC Dual-Port SRAM Stub
// ============================================================================
// Replace body with foundry dual-port SRAM compiler macro.
// Port interface identical to rtl/prim/mp64_sram_dp.v (asymmetric widths).
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

    // === Port A (wide) ===
    input  wire                  a_ce,
    input  wire                  a_we,
    input  wire [ADDR_W_A-1:0]  a_addr,
    input  wire [DATA_W_A-1:0]  a_wdata,
    output wire [DATA_W_A-1:0]  a_rdata,

    // === Port B (narrow) ===
    input  wire                  b_ce,
    input  wire                  b_we,
    input  wire [ADDR_W_B-1:0]  b_addr,
    input  wire [DATA_W_B-1:0]  b_wdata,
    output wire [DATA_W_B-1:0]  b_rdata
);

    // STUB — behavioural fallback (identical to portable prim)
    // Replace with foundry SRAM macro instantiation for tapeout.

    localparam RATIO = DATA_W_A / DATA_W_B;
    localparam SEL_W = (RATIO > 1) ? $clog2(RATIO) : 1;

    reg [DATA_W_A-1:0] mem [0:DEPTH_A-1];

    // Port A — wide
    reg [DATA_W_A-1:0] a_rdata_r;
    always @(posedge clk) begin
        if (a_ce) begin
            if (a_we) mem[a_addr] <= a_wdata;
            a_rdata_r <= mem[a_addr];
        end
    end

    // Port B — narrow (sub-word addressing)
    wire [ADDR_W_A-1:0] b_row_addr;
    wire [SEL_W-1:0]    b_sel;

    generate
        if (RATIO > 1) begin : g_asym
            assign b_row_addr = b_addr[ADDR_W_B-1 -: ADDR_W_A];
            assign b_sel      = b_addr[SEL_W-1:0];
        end else begin : g_sym
            assign b_row_addr = b_addr;
            assign b_sel      = 1'b0;
        end
    endgenerate

    reg [DATA_W_B-1:0] b_rdata_r;
    always @(posedge clk) begin
        if (b_ce) begin
            if (b_we)
                mem[b_row_addr][b_sel * DATA_W_B +: DATA_W_B] <= b_wdata;
            b_rdata_r <= mem[b_row_addr][b_sel * DATA_W_B +: DATA_W_B];
        end
    end

    assign a_rdata = a_rdata_r;
    assign b_rdata = b_rdata_r;

endmodule
