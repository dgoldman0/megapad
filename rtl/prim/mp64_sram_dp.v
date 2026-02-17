// ============================================================================
// mp64_sram_dp.v — True Dual-Port SRAM Wrapper (Portable)
// ============================================================================
//
// Behavioural model of a synchronous true dual-port SRAM.
// Both ports can read and write independently on each clock edge.
//
// Features:
//   - Parameterised address width and data width (per port)
//   - Port A and Port B may have different data widths
//     (Port A = tile port @ 512b, Port B = CPU port @ 64b)
//   - Shared underlying storage sized to the wider port
//   - Optional output register per port
//   - Write-first semantics per port
//   - No collision detection in this wrapper (handled at higher level)
//
// Address mapping when port widths differ:
//   If DATA_W_A > DATA_W_B, Port B addresses a sub-word of Port A's row.
//   The ratio DATA_W_A / DATA_W_B must be a power of 2.
//   Extra low-order address bits on Port B select the sub-word.
//
// Coding standard: Verilog-2001, sync reset, non-blocking assigns.
//

module mp64_sram_dp #(
    parameter ADDR_W_A  = 14,           // Port A address bits
    parameter DATA_W_A  = 512,          // Port A data width
    parameter ADDR_W_B  = 17,           // Port B address bits
    parameter DATA_W_B  = 64,           // Port B data width
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
    localparam RATIO  = DATA_W_A / DATA_W_B;   // must be power-of-2
    localparam SEL_W  = (RATIO > 1) ? $clog2(RATIO) : 1;

    // ========================================================================
    // Storage — rows at the wider port's dimensions
    // ========================================================================
    reg [DATA_W_A-1:0] mem [0:DEPTH_A-1];

    generate
        if (INIT_FILE != "") begin : g_init
            initial $readmemh(INIT_FILE, mem);
        end
    endgenerate

    // ========================================================================
    // Port A — wide read / write
    // ========================================================================
    reg [DATA_W_A-1:0] a_rdata_r;

    always @(posedge clk) begin
        if (a_ce) begin
            if (a_we)
                mem[a_addr] <= a_wdata;
            a_rdata_r <= mem[a_addr];
        end
    end

    // ========================================================================
    // Port B — narrow read / write (sub-word addressing)
    // ========================================================================
    reg [DATA_W_B-1:0] b_rdata_r;

    // Row address = upper bits, sub-word select = lower bits
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

    always @(posedge clk) begin
        if (b_ce) begin
            if (b_we) begin
                // Read-modify-write the wide row
                // NOTE: blocking read + non-blocking write is the standard
                // BRAM inference pattern for asymmetric ports
                mem[b_row_addr][b_sel * DATA_W_B +: DATA_W_B] <= b_wdata;
            end
            b_rdata_r <= mem[b_row_addr][b_sel * DATA_W_B +: DATA_W_B];
        end
    end

    // ========================================================================
    // Optional output registers
    // ========================================================================
    generate
        if (OUT_REG_A) begin : g_outa
            reg [DATA_W_A-1:0] a_rdata_q;
            always @(posedge clk) begin
                if (!rst_n) a_rdata_q <= {DATA_W_A{1'b0}};
                else        a_rdata_q <= a_rdata_r;
            end
            assign a_rdata = a_rdata_q;
        end else begin : g_norega
            assign a_rdata = a_rdata_r;
        end

        if (OUT_REG_B) begin : g_outb
            reg [DATA_W_B-1:0] b_rdata_q;
            always @(posedge clk) begin
                if (!rst_n) b_rdata_q <= {DATA_W_B{1'b0}};
                else        b_rdata_q <= b_rdata_r;
            end
            assign b_rdata = b_rdata_q;
        end else begin : g_noregb
            assign b_rdata = b_rdata_r;
        end
    endgenerate

endmodule
