// ============================================================================
// mp64_core_bus_mux.v — Stable CPU data/I-cache ownership for one bus port
// ============================================================================
//
// A full core shares one main-bus arbiter port between its data path and its
// private I-cache refill path.  The selected request is captured as soon as it
// is offered and remains the sole owner until the registered response pulse.
// This prevents a refill-gap request from stealing the response to an already
// accepted data transaction (or vice versa).
//
// The request payload is captured as well as the owner.  Consequently reset or
// invalidate cancellation upstream cannot withdraw a request that this bridge
// may already have presented to the arbiter; the response is drained to the
// original owner and ownership is then released.
// ============================================================================

module mp64_core_bus_mux (
    input  wire        clk,
    input  wire        rst_n,

    input  wire        core_valid,
    input  wire [63:0] core_addr,
    input  wire [63:0] core_wdata,
    input  wire        core_wen,
    input  wire [1:0]  core_size,
    input  wire        core_port_io,

    input  wire        ic_valid,
    input  wire [63:0] ic_addr,
    input  wire        ic_wen,
    input  wire [1:0]  ic_size,

    output wire        mux_valid,
    output wire [63:0] mux_addr,
    output wire [63:0] mux_wdata,
    output wire        mux_wen,
    output wire [1:0]  mux_size,
    output wire        mux_port_io,

    input  wire [63:0] bus_rdata,
    input  wire        bus_ready,
    input  wire        bus_error,
    output wire [63:0] core_rdata,
    output wire        core_ready,
    output wire        core_error,
    output wire [63:0] ic_rdata,
    output wire        ic_ready,
    output wire        ic_error,
    output wire [63:0] ic_error_addr
);

    reg        owner_active;
    reg        owner_icache;
    reg [63:0] owner_addr;
    reg [63:0] owner_wdata;
    reg        owner_wen;
    reg [1:0]  owner_size;
    reg        owner_port_io;

    // The I-cache wins only when both sources first contend for an otherwise
    // idle physical port.  Once selected, neither current VALID signal may
    // change transaction ownership.
    wire select_icache = owner_active ? owner_icache : ic_valid;
    wire offered_valid = ic_valid || core_valid;

    assign mux_valid = owner_active ? 1'b1 : offered_valid;
    assign mux_addr = owner_active
                    ? owner_addr
                    : select_icache ? ic_addr : core_addr;
    assign mux_wdata = owner_active
                     ? owner_wdata
                     : select_icache ? 64'd0 : core_wdata;
    assign mux_wen = owner_active
                   ? owner_wen
                   : select_icache ? ic_wen : core_wen;
    assign mux_size = owner_active
                    ? owner_size
                    : select_icache ? ic_size : core_size;
    assign mux_port_io = owner_active
                       ? owner_port_io
                       : select_icache ? 1'b0 : core_port_io;

    assign core_rdata = bus_rdata;
    assign ic_rdata = bus_rdata;
    assign core_ready = bus_ready && owner_active && !owner_icache;
    assign ic_ready = bus_ready && owner_active && owner_icache;
    // Error qualification uses the same captured owner as READY.  A refill
    // timeout therefore cannot become a data exception merely because the
    // CPU presents a data request while the response is returning.
    assign core_error = bus_error && core_ready;
    assign ic_error = bus_error && ic_ready;
    assign ic_error_addr = owner_addr;

    always @(posedge clk) begin
        if (!rst_n) begin
            owner_active  <= 1'b0;
            owner_icache  <= 1'b0;
            owner_addr    <= 64'd0;
            owner_wdata   <= 64'd0;
            owner_wen     <= 1'b0;
            owner_size    <= 2'd0;
            owner_port_io <= 1'b0;
        end else if (owner_active) begin
            if (bus_ready)
                owner_active <= 1'b0;
        end else if (offered_valid) begin
            owner_active <= 1'b1;
            owner_icache <= select_icache;
            if (select_icache) begin
                owner_addr    <= ic_addr;
                owner_wdata   <= 64'd0;
                owner_wen     <= ic_wen;
                owner_size    <= ic_size;
                owner_port_io <= 1'b0;
            end else begin
                owner_addr    <= core_addr;
                owner_wdata   <= core_wdata;
                owner_wen     <= core_wen;
                owner_size    <= core_size;
                owner_port_io <= core_port_io;
            end
        end
    end

endmodule
