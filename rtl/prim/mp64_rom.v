// ============================================================================
// mp64_rom.v — ROM Wrapper (Portable)
// ============================================================================
//
// Synchronous ROM initialised from a hex file via $readmemh.
// Infers block RAM (read-only) on FPGAs.
//
// Features:
//   - Parameterised address width, data width
//   - Mandatory INIT_FILE — ROM without contents is useless
//   - Optional output register
//   - Single read port, synchronous output
//
// Coding standard: Verilog-2001, sync reset, non-blocking assigns.
//

module mp64_rom #(
    parameter ADDR_W    = 8,
    parameter DATA_W    = 32,
    parameter DEPTH     = (1 << ADDR_W),
    parameter OUT_REG   = 0,
    parameter INIT_FILE = "rom.hex"
)(
    input  wire                clk,
    input  wire                rst_n,

    input  wire                ce,
    input  wire [ADDR_W-1:0]   addr,
    output wire [DATA_W-1:0]   rdata
);

    // ========================================================================
    // Storage
    // ========================================================================
    reg [DATA_W-1:0] mem [0:DEPTH-1];

    initial $readmemh(INIT_FILE, mem);

    // ========================================================================
    // Synchronous read
    // ========================================================================
    reg [DATA_W-1:0] rdata_r;

    always @(posedge clk) begin
        if (ce)
            rdata_r <= mem[addr];
    end

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
                    rdata_q <= rdata_r;
            end
            assign rdata = rdata_q;
        end else begin : g_noreg
            assign rdata = rdata_r;
        end
    endgenerate

endmodule
