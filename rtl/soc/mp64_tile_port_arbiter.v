// ============================================================================
// mp64_tile_port_arbiter.v — Buffered equal-RR tile-memory arbitration
// ============================================================================
//
// Four tile engines share the internal 512-bit memory port and the external
// 512-bit burst port.  Each engine emits a one-cycle request pulse and then
// waits for its response, so the arbiter keeps one pending payload slot per
// engine.  This preserves pulses that arrive while another engine is active or
// that lose simultaneous arbitration.
//
// Pending peers are selected with equal round-robin ordering.  The chosen
// owner, address, direction, and write payload remain captured until the
// selected target acknowledges.  Target REQ is suppressed during the ACK
// pulse: both current memory targets return to their idle state while ACK is
// high and would otherwise accept the held transaction a second time.
// ============================================================================

module mp64_tile_port_arbiter (
    input  wire          clk,
    input  wire          rst,

    // Source 0 is the full core; sources 1..3 are the three clusters.
    // Packed address/data lane N belongs to request bit N.
    input  wire [3:0]    src_tile_req,
    input  wire [127:0]  src_tile_addr,
    input  wire [3:0]    src_tile_wen,
    input  wire [2047:0] src_tile_wdata,
    output wire [3:0]    src_tile_ack,

    input  wire [3:0]    src_ext_req,
    input  wire [255:0]  src_ext_addr,
    input  wire [3:0]    src_ext_wen,
    input  wire [2047:0] src_ext_wdata,
    output wire [3:0]    src_ext_ack,

    // Shared internal tile-memory target.
    output wire          tile_req,
    output wire [31:0]   tile_addr,
    output wire          tile_wen,
    output wire [511:0]  tile_wdata,
    input  wire          tile_ack,

    // Shared external tile-memory target.
    output wire          ext_req,
    output wire [63:0]   ext_addr,
    output wire          ext_wen,
    output wire [511:0]  ext_wdata,
    input  wire          ext_ack,

    // Completed write metadata remains valid for the complete ACK cycle.
    output wire          write_commit,
    output wire [1:0]    write_owner,
    output wire          write_ext,
    output wire [63:0]   write_addr
);

    reg [3:0]   pending;
    reg [3:0]   pending_ext;
    reg [63:0]  pending_addr [0:3];
    reg [3:0]   pending_wen;
    reg [511:0] pending_wdata[0:3];

    reg         active;
    reg [1:0]   active_owner;
    reg         active_ext;
    reg [63:0]  active_addr;
    reg         active_wen;
    reg [511:0] active_wdata;
    reg [1:0]   last_grant;

    reg         next_valid;
    reg [1:0]   next_owner;

    integer i;

    // Scan every pending peer exactly once after the most recently completed
    // owner.  Reset last_grant=3 makes source 0 the first reset-era candidate.
    always @(*) begin
        next_valid = 1'b0;
        next_owner = 2'd0;
        case (last_grant)
        2'd0: begin
            if (pending[1]) begin next_valid = 1'b1; next_owner = 2'd1; end
            else if (pending[2]) begin next_valid = 1'b1; next_owner = 2'd2; end
            else if (pending[3]) begin next_valid = 1'b1; next_owner = 2'd3; end
            else if (pending[0]) begin next_valid = 1'b1; next_owner = 2'd0; end
        end
        2'd1: begin
            if (pending[2]) begin next_valid = 1'b1; next_owner = 2'd2; end
            else if (pending[3]) begin next_valid = 1'b1; next_owner = 2'd3; end
            else if (pending[0]) begin next_valid = 1'b1; next_owner = 2'd0; end
            else if (pending[1]) begin next_valid = 1'b1; next_owner = 2'd1; end
        end
        2'd2: begin
            if (pending[3]) begin next_valid = 1'b1; next_owner = 2'd3; end
            else if (pending[0]) begin next_valid = 1'b1; next_owner = 2'd0; end
            else if (pending[1]) begin next_valid = 1'b1; next_owner = 2'd1; end
            else if (pending[2]) begin next_valid = 1'b1; next_owner = 2'd2; end
        end
        default: begin
            if (pending[0]) begin next_valid = 1'b1; next_owner = 2'd0; end
            else if (pending[1]) begin next_valid = 1'b1; next_owner = 2'd1; end
            else if (pending[2]) begin next_valid = 1'b1; next_owner = 2'd2; end
            else if (pending[3]) begin next_valid = 1'b1; next_owner = 2'd3; end
        end
        endcase
    end

    wire active_ack = active_ext ? ext_ack : tile_ack;
    wire [3:0] active_owner_mask = 4'b0001 << active_owner;

    assign tile_req   = active && !active_ext && !tile_ack;
    assign tile_addr  = active_addr[31:0];
    assign tile_wen   = active_wen;
    assign tile_wdata = active_wdata;

    assign ext_req   = active && active_ext && !ext_ack;
    assign ext_addr  = active_addr;
    assign ext_wen   = active_wen;
    assign ext_wdata = active_wdata;

    assign src_tile_ack = (active && !active_ext && tile_ack)
                        ? active_owner_mask : 4'b0000;
    assign src_ext_ack  = (active && active_ext && ext_ack)
                        ? active_owner_mask : 4'b0000;

    assign write_commit = active && active_wen && active_ack;
    assign write_owner  = active_owner;
    assign write_ext    = active_ext;
    assign write_addr   = active_addr;

    always @(posedge clk) begin
        if (rst) begin
            pending      <= 4'b0000;
            pending_ext  <= 4'b0000;
            pending_wen  <= 4'b0000;
            active       <= 1'b0;
            active_owner <= 2'd0;
            active_ext   <= 1'b0;
            active_addr  <= 64'd0;
            active_wen   <= 1'b0;
            active_wdata <= 512'd0;
            last_grant   <= 2'd3;
            for (i = 0; i < 4; i = i + 1) begin
                pending_addr[i]  <= 64'd0;
                pending_wdata[i] <= 512'd0;
            end
        end else begin
`ifndef SYNTHESIS
            for (i = 0; i < 4; i = i + 1) begin
                if (src_tile_req[i] && src_ext_req[i])
                    $error("tile source %0d asserted internal and external requests together", i);
                if ((src_tile_req[i] || src_ext_req[i])
                        && (pending[i]
                         || (active && active_owner == i[1:0])))
                    $error("tile source %0d issued more than one outstanding request", i);
            end
`endif

            // Each engine has at most one outstanding transaction.  Prefer
            // the internal request if a malformed source raises both modes.
            for (i = 0; i < 4; i = i + 1) begin
                if (!pending[i] && (src_tile_req[i] || src_ext_req[i])) begin
                    pending[i] <= 1'b1;
                    if (src_tile_req[i]) begin
                        pending_ext[i]   <= 1'b0;
                        pending_addr[i]  <=
                            {32'd0, src_tile_addr[i*32 +: 32]};
                        pending_wen[i]   <= src_tile_wen[i];
                        pending_wdata[i] <=
                            src_tile_wdata[i*512 +: 512];
                    end else begin
                        pending_ext[i]   <= 1'b1;
                        pending_addr[i]  <=
                            src_ext_addr[i*64 +: 64];
                        pending_wen[i]   <= src_ext_wen[i];
                        pending_wdata[i] <=
                            src_ext_wdata[i*512 +: 512];
                    end
                end
            end

            if (active) begin
                if (active_ack) begin
                    active     <= 1'b0;
                    last_grant <= active_owner;
                end
            end else if (next_valid) begin
                active       <= 1'b1;
                active_owner <= next_owner;
                active_ext   <= pending_ext[next_owner];
                active_addr  <= pending_addr[next_owner];
                active_wen   <= pending_wen[next_owner];
                active_wdata <= pending_wdata[next_owner];
                case (next_owner)
                2'd0: pending[0] <= 1'b0;
                2'd1: pending[1] <= 1'b0;
                2'd2: pending[2] <= 1'b0;
                default: pending[3] <= 1'b0;
                endcase
            end
        end
    end

endmodule
