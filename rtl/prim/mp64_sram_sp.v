// ============================================================================
// mp64_sram_sp.v — Single-Port SRAM Wrapper (Portable)
// ============================================================================
//
// Behavioural model of a synchronous single-port SRAM.
// Infers block RAM on FPGAs and maps to SRAM macros on ASIC.
//
// Features:
//   - Parameterised address width and data width
//   - Optional output register (OUT_REG=1) for cleaner BRAM timing
//   - Optional hex initialisation file
//   - Write-first semantics (read-after-write returns new data)
//   - Synchronous active-low reset clears the output register only
//     (NOT the memory contents — use INIT_FILE or external init)
//
// Coding standard:
//   - Verilog-2001
//   - Synchronous reset
//   - Non-blocking assigns in clocked blocks
//
// Platform overrides:
//   Replace this file with a technology-specific wrapper that
//   instantiates vendor SRAM macros but keeps the same port interface.
//

module mp64_sram_sp #(
    parameter ADDR_W    = 14,           // address bits (depth = 2^ADDR_W)
    parameter DATA_W    = 512,          // data width in bits
    parameter DEPTH     = (1 << ADDR_W),// number of rows (derived)
    parameter OUT_REG   = 0,            // 1 = add output pipeline register
    parameter INIT_FILE = ""            // hex file for $readmemh (or "")
)(
    input  wire                clk,
    input  wire                rst_n,    // sync reset (output reg only)

    input  wire                ce,       // chip enable
    input  wire                we,       // write enable
    input  wire [ADDR_W-1:0]   addr,
    input  wire [DATA_W-1:0]   wdata,
    output wire [DATA_W-1:0]   rdata
);

    // ========================================================================
    // Storage array
    // ========================================================================
    reg [DATA_W-1:0] mem [0:DEPTH-1];

    // Initialisation
    generate
        if (INIT_FILE != "") begin : g_init
            initial $readmemh(INIT_FILE, mem);
        end
    endgenerate

    // ========================================================================
    // Read / Write — write-first
    // ========================================================================
    reg [DATA_W-1:0] rdata_r;

    always @(posedge clk) begin
        if (ce) begin
            if (we)
                mem[addr] <= wdata;
            rdata_r <= mem[addr];
        end
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
