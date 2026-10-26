// ============================================================================
// mp64_byte_dma_arbiter.v — two byte-DMA masters on one bus master port
// ============================================================================
//
// The machine's two storage controllers, the system disk's and the settings
// disk's, reach memory through the disk's one bus master port.  Each
// controller presents one byte beat at a time and holds its request tuple
// stable until ACK or ERR.  This arbiter passes one controller's beat to the
// port and holds that grant until the beat completes, so a tuple the port
// has seen never changes under it.  Between beats the grant alternates while
// both controllers ask, so neither waits longer than one of the other's beats
// and neither reaches its DMA timeout because of the other.  The port's read
// data reaches both controllers; only the granted one sees ACK or ERR.
//
// Coding standard: Verilog-2001, no vendor primitives.

module mp64_byte_dma_arbiter (
    input  wire        clk,
    input  wire        rst_n,

    // Master 0: the system disk's controller
    input  wire        m0_req,
    input  wire [63:0] m0_addr,
    input  wire [7:0]  m0_wdata,
    input  wire        m0_wen,
    output wire [7:0]  m0_rdata,
    output wire        m0_ack,
    output wire        m0_err,

    // Master 1: the settings disk's controller
    input  wire        m1_req,
    input  wire [63:0] m1_addr,
    input  wire [7:0]  m1_wdata,
    input  wire        m1_wen,
    output wire [7:0]  m1_rdata,
    output wire        m1_ack,
    output wire        m1_err,

    // The shared bus master port
    output wire        port_req,
    output wire [63:0] port_addr,
    output wire [7:0]  port_wdata,
    output wire        port_wen,
    input  wire [7:0]  port_rdata,
    input  wire        port_ack,
    input  wire        port_err
);

    // HELD: a beat is granted and has not completed; OWNER names its master.
    // LAST names the master whose beat completed most recently.
    reg held;
    reg owner;
    reg last;

    // With no beat held, master 1 wins while it asks and either master 0
    // does not, or master 0 had the last beat.
    wire choose = m1_req && (!m0_req || !last);
    wire grant  = held ? owner : choose;

    assign port_req   = grant ? m1_req   : m0_req;
    assign port_addr  = grant ? m1_addr  : m0_addr;
    assign port_wdata = grant ? m1_wdata : m0_wdata;
    assign port_wen   = grant ? m1_wen   : m0_wen;

    assign m0_rdata = port_rdata;
    assign m1_rdata = port_rdata;
    assign m0_ack   = !grant && port_ack;
    assign m0_err   = !grant && port_err;
    assign m1_ack   =  grant && port_ack;
    assign m1_err   =  grant && port_err;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            held  <= 1'b0;
            owner <= 1'b0;
            last  <= 1'b1;
        end else if (port_req && (port_ack || port_err)) begin
            held <= 1'b0;
            last <= grant;
        end else if (held && !port_req) begin
            // A master that withdraws its request gives up the grant.
            held <= 1'b0;
        end else if (!held && port_req) begin
            held  <= 1'b1;
            owner <= grant;
        end
    end

endmodule
