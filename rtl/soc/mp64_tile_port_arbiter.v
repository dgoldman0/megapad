// ============================================================================
// mp64_tile_port_arbiter.v — Buffered equal-RR tile-memory arbitration
// ============================================================================
//
// Seven tile engines share the internal 512-bit memory port and the external
// 512-bit burst port.  Sources 0..3 are the four full-core-private engines and
// sources 4..6 are the three microcluster-private engines.  Each engine emits
// a one-cycle request pulse and then waits for its response, so the arbiter
// keeps one pending payload slot per engine.  This preserves pulses that
// arrive while another engine is active or that lose simultaneous arbitration.
//
// Pending peers are selected with equal round-robin ordering.  Once selected,
// REQ and its payload remain stable until the target raises ACCEPT.  REQ then
// drops and the captured owner remains active until the target raises its
// terminal ACK.  ERROR and FAULT_ADDR qualify that ACK and are returned only
// to the captured source.
//
// Cancellation is source-local.  A pending or not-yet-accepted request is
// discarded immediately without advancing round-robin state.  An accepted
// request instead raises the selected target's CANCEL and drains its terminal
// ACK without exposing that stale completion to the source.  The source may
// place one fresh request in its pending slot while that drain is in flight.
// A held SRC_CANCEL is edge-qualified, allowing callers to hold cancellation
// until SRC_CANCEL_DONE without accidentally canceling the fresh request.
// ============================================================================

module mp64_tile_port_arbiter #(
    parameter integer SOURCE_COUNT = 7,
    parameter integer OWNER_BITS   = 3
) (
    input  wire          clk,
    input  wire          rst,

    // Packed address/data lane N belongs to request bit N.
    input  wire [SOURCE_COUNT-1:0]     src_tile_req,
    input  wire [SOURCE_COUNT*32-1:0]  src_tile_addr,
    input  wire [SOURCE_COUNT-1:0]     src_tile_wen,
    input  wire [SOURCE_COUNT*512-1:0] src_tile_wdata,
    output wire [SOURCE_COUNT-1:0]     src_tile_ack,
    output wire [SOURCE_COUNT-1:0]     src_tile_error,
    output reg  [SOURCE_COUNT*64-1:0]  src_tile_fault_addr,

    input  wire [SOURCE_COUNT-1:0]     src_ext_req,
    input  wire [SOURCE_COUNT*64-1:0]  src_ext_addr,
    input  wire [SOURCE_COUNT-1:0]     src_ext_wen,
    input  wire [SOURCE_COUNT*512-1:0] src_ext_wdata,
    output wire [SOURCE_COUNT-1:0]     src_ext_ack,
    output wire [SOURCE_COUNT-1:0]     src_ext_error,
    output reg  [SOURCE_COUNT*64-1:0]  src_ext_fault_addr,

    // SRC_ACCEPT is combinational with the source request pulse and means the
    // payload will be captured on the current rising edge.  Cancellation may
    // be held until the corresponding one-cycle SRC_CANCEL_DONE pulse.
    input  wire [SOURCE_COUNT-1:0]     src_cancel,
    output wire [SOURCE_COUNT-1:0]     src_accept,
    output reg  [SOURCE_COUNT-1:0]     src_cancel_done,

    // Shared internal tile-memory target.
    output wire          tile_req,
    output wire [31:0]   tile_addr,
    output wire          tile_wen,
    output wire [511:0]  tile_wdata,
    input  wire          tile_accept,
    input  wire          tile_ack,
    input  wire          tile_error,
    input  wire [63:0]   tile_fault_addr,
    output wire          tile_cancel,

    // Shared external tile-memory target.
    output wire          ext_req,
    output wire [63:0]   ext_addr,
    output wire          ext_wen,
    output wire [511:0]  ext_wdata,
    input  wire          ext_accept,
    input  wire          ext_ack,
    input  wire          ext_error,
    input  wire [63:0]   ext_fault_addr,
    output wire          ext_cancel,

    // Completed write metadata remains valid for the complete ACK cycle.
    output wire          write_commit,
    output wire [OWNER_BITS-1:0] write_owner,
    output wire          write_ext,
    output wire [63:0]   write_addr,

    // The external controller reports individual 64-bit word completions
    // before the arbiter's 512-bit terminal ACK. Keep the captured source
    // identity visible for exact per-core PERF_EXTMEM attribution.
    output wire          ext_word_owner_valid,
    output wire [OWNER_BITS-1:0] ext_word_owner
);

    reg [SOURCE_COUNT-1:0] pending;
    reg [SOURCE_COUNT-1:0] pending_ext;
    reg [63:0]             pending_addr [0:SOURCE_COUNT-1];
    reg [SOURCE_COUNT-1:0] pending_wen;
    reg [511:0]            pending_wdata[0:SOURCE_COUNT-1];

    reg                  active;
    reg [OWNER_BITS-1:0] active_owner;
    reg                  active_ext;
    reg [63:0]           active_addr;
    reg                  active_wen;
    reg [511:0]          active_wdata;
    reg                  active_issued;
    reg                  active_canceling;
    reg [OWNER_BITS-1:0] last_grant;
    reg [SOURCE_COUNT-1:0] cancel_seen;

    reg                  next_valid;
    reg [OWNER_BITS-1:0] next_owner;
    reg [SOURCE_COUNT-1:0] capture_now;

    integer i;
    integer capture_index;
    integer scan_offset;
    integer scan_index;

    wire [SOURCE_COUNT-1:0] cancel_event =
        src_cancel & ~cancel_seen;

    // A normal source has one outstanding request.  The sole exception is a
    // replacement request captured while its accepted predecessor is being
    // canceled and drained.  A same-cycle cancel may also replace pending or
    // pre-accept work atomically.
    always @(*) begin
        capture_now = {SOURCE_COUNT{1'b0}};
        for (capture_index = 0;
             capture_index < SOURCE_COUNT;
             capture_index = capture_index + 1) begin
            if (src_tile_req[capture_index]
                    || src_ext_req[capture_index]) begin
                if (!pending[capture_index]
                        && !(active
                          && active_owner == capture_index))
                    capture_now[capture_index] = 1'b1;
                else if (!pending[capture_index]
                        && active
                        && active_owner == capture_index
                        && (active_canceling
                          || cancel_event[capture_index]))
                    capture_now[capture_index] = 1'b1;
                else if (pending[capture_index]
                        && !(active
                          && active_owner == capture_index)
                        && cancel_event[capture_index])
                    capture_now[capture_index] = 1'b1;
            end
        end
    end

    assign src_accept = capture_now;

    // Scan every pending peer exactly once after the most recently completed
    // owner.  Reset last_grant=6 makes source 0 the first reset-era candidate.
    // The sum is at most (2*SOURCE_COUNT)-1, so one subtraction implements the
    // deterministic SOURCE_COUNT-1 -> 0 wrap.
    always @(*) begin
        next_valid = 1'b0;
        next_owner = {OWNER_BITS{1'b0}};
        scan_index = 0;
        for (scan_offset = 1;
             scan_offset <= SOURCE_COUNT;
             scan_offset = scan_offset + 1) begin
            scan_index = last_grant + scan_offset;
            if (scan_index >= SOURCE_COUNT)
                scan_index = scan_index - SOURCE_COUNT;
            if (!next_valid && pending[scan_index]) begin
                next_valid = 1'b1;
                next_owner = scan_index[OWNER_BITS-1:0];
            end
        end
    end

    wire active_target_accept =
        active_ext ? ext_accept : tile_accept;
    wire active_target_ack =
        active_ext ? ext_ack : tile_ack;
    wire active_target_error =
        active_ext ? ext_error : tile_error;
    wire [63:0] active_target_fault_addr =
        active_ext ? ext_fault_addr : tile_fault_addr;
    wire [SOURCE_COUNT-1:0] active_owner_mask =
        {{(SOURCE_COUNT-1){1'b0}}, 1'b1} << active_owner;
    wire active_cancel_event =
        active && cancel_event[active_owner];
    wire active_cancel_requested =
        active_canceling || active_cancel_event;
    // ACCEPT may be a registered response to REQ sampled on the preceding
    // edge.  Treat it as issued immediately so a coincident source cancel
    // drains the accepted target transaction instead of releasing ownership.
    wire active_target_accepted =
        active_issued || active_target_accept;
    wire active_terminal =
        active && active_target_accepted && active_target_ack;
    wire deliver_terminal =
        active_terminal && !active_cancel_requested;

    assign tile_req   = active && !active_ext && !active_issued
                      && !active_cancel_requested;
    assign tile_addr  = active_addr[31:0];
    assign tile_wen   = active_wen;
    assign tile_wdata = active_wdata;
    assign tile_cancel = active && !active_ext && active_target_accepted
                       && active_cancel_requested;

    assign ext_req   = active && active_ext && !active_issued
                     && !active_cancel_requested;
    assign ext_addr  = active_addr;
    assign ext_wen   = active_wen;
    assign ext_wdata = active_wdata;
    assign ext_cancel = active && active_ext && active_target_accepted
                      && active_cancel_requested;

    assign src_tile_ack = (deliver_terminal && !active_ext)
                        ? active_owner_mask : {SOURCE_COUNT{1'b0}};
    assign src_ext_ack  = (deliver_terminal && active_ext)
                        ? active_owner_mask : {SOURCE_COUNT{1'b0}};
    assign src_tile_error =
        (deliver_terminal && !active_ext && active_target_error)
        ? active_owner_mask : {SOURCE_COUNT{1'b0}};
    assign src_ext_error =
        (deliver_terminal && active_ext && active_target_error)
        ? active_owner_mask : {SOURCE_COUNT{1'b0}};

    // An accepted write may have changed memory even when its terminal result
    // is an error or is being suppressed by cancellation.  Conservatively
    // invalidate the writer's cache on every accepted write terminal.
    assign write_commit = active_terminal && active_wen;
    assign write_owner  = active_owner;
    assign write_ext    = active_ext;
    assign write_addr   = active_addr;
    assign ext_word_owner_valid =
        active && active_ext && active_target_accepted;
    assign ext_word_owner = active_owner;

    always @(*) begin
        src_tile_fault_addr = {(SOURCE_COUNT*64){1'b0}};
        src_ext_fault_addr  = {(SOURCE_COUNT*64){1'b0}};
        if (deliver_terminal && active_target_error) begin
            if (active_ext)
                src_ext_fault_addr[active_owner*64 +: 64] =
                    active_target_fault_addr;
            else
                src_tile_fault_addr[active_owner*64 +: 64] =
                    active_target_fault_addr;
        end
    end

    always @(posedge clk) begin
        if (rst) begin
            pending      <= {SOURCE_COUNT{1'b0}};
            pending_ext  <= {SOURCE_COUNT{1'b0}};
            pending_wen  <= {SOURCE_COUNT{1'b0}};
            src_cancel_done <= {SOURCE_COUNT{1'b0}};
            cancel_seen  <= {SOURCE_COUNT{1'b0}};
            active       <= 1'b0;
            active_owner <= {OWNER_BITS{1'b0}};
            active_ext   <= 1'b0;
            active_addr  <= 64'd0;
            active_wen   <= 1'b0;
            active_wdata <= 512'd0;
            active_issued <= 1'b0;
            active_canceling <= 1'b0;
            last_grant   <= SOURCE_COUNT - 1;
            for (i = 0; i < SOURCE_COUNT; i = i + 1) begin
                pending_addr[i]  <= 64'd0;
                pending_wdata[i] <= 512'd0;
            end
        end else begin
            src_cancel_done <= {SOURCE_COUNT{1'b0}};
            cancel_seen <= src_cancel;

`ifndef SYNTHESIS
            for (i = 0; i < SOURCE_COUNT; i = i + 1) begin
                if (src_tile_req[i] && src_ext_req[i])
                    $error("tile source %0d asserted internal and external requests together", i);
                if ((src_tile_req[i] || src_ext_req[i])
                        && !capture_now[i])
                    $error("tile source %0d issued more than one outstanding request", i);
            end
            if (tile_ack && !tile_accept && active && !active_ext
                    && !active_issued)
                $error("internal tile target ACKed an unaccepted request");
            if (ext_ack && !ext_accept && active && active_ext
                    && !active_issued)
                $error("external tile target ACKed an unaccepted request");
            if (tile_error && !tile_ack)
                $error("internal tile target ERROR without terminal ACK");
            if (ext_error && !ext_ack)
                $error("external tile target ERROR without terminal ACK");
            if (active && active_target_accept
                    && active_cancel_requested
                    && !(active_ext ? ext_cancel : tile_cancel))
                $error("accepted tile request canceled without target drain");
`endif

            // Cancel pending work without consuming a turn.  A later capture
            // assignment intentionally wins when a fresh pulse replaces the
            // canceled pending request on this same edge.
            for (i = 0; i < SOURCE_COUNT; i = i + 1) begin
                if (cancel_event[i]
                        && !(active && active_owner == i)) begin
                    pending[i] <= 1'b0;
                    src_cancel_done[i] <= 1'b1;
                end
            end

            // Prefer the internal request if a malformed source raises both
            // modes.  SRC_ACCEPT reports this exact capture decision.
            for (i = 0; i < SOURCE_COUNT; i = i + 1) begin
                if (capture_now[i]) begin
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
                if (active_target_accepted && active_cancel_requested) begin
                    // Latch a same-cycle ACCEPT before its pulse disappears.
                    // CANCEL_DONE remains suppressed until the target's
                    // terminal ACK retires the canceled transaction.
                    active_issued    <= 1'b1;
                    active_canceling <= 1'b1;
                    if (active_target_ack) begin
                        active            <= 1'b0;
                        active_issued     <= 1'b0;
                        active_canceling  <= 1'b0;
                        last_grant        <= active_owner;
                        src_cancel_done[active_owner] <= 1'b1;
                    end
                end else if (!active_target_accepted
                        && active_cancel_event) begin
                    active            <= 1'b0;
                    active_canceling  <= 1'b0;
                    src_cancel_done[active_owner] <= 1'b1;
                end else if (active_issued) begin
                    if (active_target_ack) begin
                        active           <= 1'b0;
                        active_issued    <= 1'b0;
                        active_canceling <= 1'b0;
                        last_grant       <= active_owner;
                    end
                end else if (active_target_accept) begin
                    if (active_target_ack) begin
                        active           <= 1'b0;
                        active_issued    <= 1'b0;
                        active_canceling <= 1'b0;
                        last_grant       <= active_owner;
                    end else begin
                        active_issued <= 1'b1;
                    end
                end
            end else if (next_valid) begin
                // The cancellation pass above removes this peer.  Do not
                // momentarily grant canceled pending work or advance RR.
                if (!cancel_event[next_owner]) begin
                    active       <= 1'b1;
                    active_owner <= next_owner;
                    active_ext   <= pending_ext[next_owner];
                    active_addr  <= pending_addr[next_owner];
                    active_wen   <= pending_wen[next_owner];
                    active_wdata <= pending_wdata[next_owner];
                    active_issued <= 1'b0;
                    active_canceling <= 1'b0;
                    pending[next_owner] <= 1'b0;
                end
            end
        end
    end

endmodule
