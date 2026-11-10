// ============================================================================
// mp64_byte_dma_arbiter.v — byte-DMA masters on one bus master port
// ============================================================================
//
// The machine's storage controllers, one for each storage unit, reach memory
// through the disk's one bus master port.  Each controller presents one byte
// beat at a time and holds its request tuple stable until ACK or ERR.  This
// arbiter passes one controller's beat to the port and holds that grant until
// the beat completes, so a tuple the port has seen never changes under it.
// Between beats the grant goes to the next master that asks, in cyclic order
// after the one served last.  A master therefore waits for at most one beat
// from each of the others, and none reaches its DMA timeout because of them.
// The port's read data reaches every master; only the granted one sees ACK
// or ERR.
//
// Master M's request tuple is bit M of M_REQ and M_WEN, bits [M*64 +: 64] of
// M_ADDR and bits [M*8 +: 8] of M_WDATA; its response is bit M of M_ACK and
// M_ERR.
//
// Coding standard: Verilog-2001, no vendor primitives.

module mp64_byte_dma_arbiter #(
    parameter integer MASTERS = 2
)(
    input  wire                  clk,
    input  wire                  rst_n,

    // The masters' request tuples and responses
    input  wire [MASTERS-1:0]    m_req,
    input  wire [MASTERS*64-1:0] m_addr,
    input  wire [MASTERS*8-1:0]  m_wdata,
    input  wire [MASTERS-1:0]    m_wen,
    output wire [7:0]            m_rdata,
    output wire [MASTERS-1:0]    m_ack,
    output wire [MASTERS-1:0]    m_err,

    // The shared bus master port
    output wire        port_req,
    output wire [63:0] port_addr,
    output wire [7:0]  port_wdata,
    output wire        port_wen,
    input  wire [7:0]  port_rdata,
    input  wire        port_ack,
    input  wire        port_err
);

    localparam integer INDEX_BITS = (MASTERS <= 1) ? 1 : $clog2(MASTERS);
    localparam integer LAST_MASTER = MASTERS - 1;

    // HELD: a beat is granted and has not completed; OWNER names its master.
    // LAST names the master whose beat completed most recently.
    reg                  held;
    reg [INDEX_BITS-1:0] owner;
    reg [INDEX_BITS-1:0] last;

    // With no beat held, the port goes to the first master that asks, in
    // cyclic order starting after LAST.  The loop visits the masters from the
    // end of that order to its start, so the first one asking is assigned
    // last and wins.  With no master asking, CHOOSE names LAST, whose request
    // is low.
    reg [INDEX_BITS-1:0] choose;
    integer              candidate;
    integer              step;
    always @(*) begin
        choose = last;
        for (step = MASTERS; step >= 1; step = step - 1) begin
            candidate = {{(32-INDEX_BITS){1'b0}}, last} + step;
            if (candidate >= MASTERS)
                candidate = candidate - MASTERS;
            if (m_req[candidate])
                choose = candidate[INDEX_BITS-1:0];
        end
    end

    wire [INDEX_BITS-1:0] grant = held ? owner : choose;

    assign port_req   = m_req[grant];
    assign port_addr  = m_addr[grant*64 +: 64];
    assign port_wdata = m_wdata[grant*8 +: 8];
    assign port_wen   = m_wen[grant];

    // GRANTED has the granted master's bit set.
    reg [MASTERS-1:0] granted;
    integer           master;
    always @(*) begin
        for (master = 0; master < MASTERS; master = master + 1)
            granted[master] = (master[INDEX_BITS-1:0] == grant);
    end

    assign m_rdata = port_rdata;
    assign m_ack   = granted & {MASTERS{port_ack}};
    assign m_err   = granted & {MASTERS{port_err}};

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            held  <= 1'b0;
            owner <= {INDEX_BITS{1'b0}};
            // Master 0 has the first beat after reset.
            last  <= LAST_MASTER[INDEX_BITS-1:0];
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
