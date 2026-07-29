// ============================================================================
// mp64_tacc_transfer.v — Chip-wide canonical TACC image staging
// ============================================================================
//
// Exactly one 2,048-bit staging image is shared by the seven physical tile
// engines.  Level-valid requests contend with equal round-robin priority.
// Once admitted, an owner retains the stage across all four 512-bit memory
// beats and through its held terminal response.  The independent tile-memory
// arbiter may therefore interleave ordinary traffic between beats without
// admitting a second TACC image transfer.
//
// Request lane N owns every packed input lane N.  Request payload and the
// eight-bit operation token are captured at admission; live inputs are never
// used to form a response.  A source must drop req before the same level can
// be admitted again.  Cancellation also consumes a waiting request level, so
// a stale request cannot become eligible merely because cancel later drops.
//
// beat_req is a one-cycle, one-hot pulse for each beat.  The downstream
// tile-port arbiter returns a one-hot ACK/error/fault-address response and
// supplies cancel_done after either dropping an unissued beat or draining an
// issued one.  Cancellation wins over an ACK sampled on the same edge.
//
// LOAD data remains private to the stage until all four beats succeed.
// result_image is nonzero/valid only alongside a successful held LOAD done.
// STORE snapshots its source bank at admission.  Modes other than TMODE_8 and
// TMODE_16 always force canonical image bytes 128..255 to zero.
// ============================================================================

`include "mp64_pkg.vh"

module mp64_tacc_transfer #(
    parameter integer SOURCE_COUNT = 7,
    parameter integer OWNER_BITS   = 3,
    parameter integer TOKEN_BITS   = 8
) (
    input  wire                         clk,
    input  wire                         rst,

    // Level-valid transfer requests.
    input  wire [SOURCE_COUNT-1:0]      req,
    input  wire [SOURCE_COUNT-1:0]      req_store,
    input  wire [SOURCE_COUNT-1:0]      req_ext,
    input  wire [SOURCE_COUNT*64-1:0]   req_base,
    input  wire [SOURCE_COUNT*3-1:0]    req_format_ew,
    input  wire [SOURCE_COUNT*TOKEN_BITS-1:0] req_token,
    input  wire [SOURCE_COUNT*2048-1:0] req_store_image,
    input  wire [SOURCE_COUNT-1:0]      req_cancel,

    // The consumer retires one held terminal response with finish.
    input  wire [SOURCE_COUNT-1:0]      finish,

    // Unified response from the seven-source tile-memory arbiter.
    input  wire [SOURCE_COUNT-1:0]      port_ack,
    input  wire [SOURCE_COUNT-1:0]      port_error,
    input  wire [SOURCE_COUNT*64-1:0]   port_fault_addr,
    input  wire [511:0]                 tile_rdata,
    input  wire [511:0]                 ext_rdata,
    input  wire [SOURCE_COUNT-1:0]      port_cancel_done,

    // One beat is injected into exactly one physical engine's source lane.
    output wire [SOURCE_COUNT-1:0]      beat_req,
    output wire                         beat_ext,
    output wire [63:0]                  beat_addr,
    output wire                         beat_wen,
    output wire [511:0]                 beat_wdata,
    output wire [SOURCE_COUNT-1:0]      port_cancel,

    // Held, one-hot terminal response.  Packed lane N belongs to source N.
    output reg  [SOURCE_COUNT-1:0]      done,
    output reg  [SOURCE_COUNT*TOKEN_BITS-1:0] response_token,
    output reg  [SOURCE_COUNT*3-1:0]    response_fault,
    output reg  [SOURCE_COUNT*64-1:0]   response_fault_addr,

    // Broadcast result; meaningful only with a successful LOAD done.
    output wire [2047:0]                result_image
);

    reg [SOURCE_COUNT-1:0] seen;
    reg [OWNER_BITS-1:0]   last_grant;

    reg                    active;
    reg [OWNER_BITS-1:0]   owner;
    reg                    owner_store;
    reg                    owner_ext;
    reg [63:0]             owner_base;
    reg [2:0]              owner_format_ew;
    reg [TOKEN_BITS-1:0]   owner_token;

    // This is the sole chip-wide canonical transfer image.
    reg [2047:0] stage_image;

    reg [1:0] beat_index;
    reg       beat_outstanding;
    reg       cancelling;
    reg       terminal;
    reg       terminal_success;

    reg                    next_valid;
    reg [OWNER_BITS-1:0]   next_owner;
    integer                scan_offset;
    integer                scan_index;
    integer                i;

    wire [SOURCE_COUNT-1:0] eligible =
        req & ~seen & ~req_cancel;

    // Reset last_grant to SOURCE_COUNT-1 so the first complete candidate set
    // is scanned in source order 0..SOURCE_COUNT-1.
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
            if (!next_valid && eligible[scan_index]) begin
                next_valid = 1'b1;
                next_owner = scan_index[OWNER_BITS-1:0];
            end
        end
    end

    wire [SOURCE_COUNT-1:0] owner_mask =
        {{(SOURCE_COUNT-1){1'b0}}, 1'b1} << owner;
    wire owner_cancel = |(req_cancel & owner_mask);
    wire owner_finish = |(finish & owner_mask);
    wire owner_ack = |(port_ack & owner_mask);
    wire owner_error = |(port_error & owner_mask);
    wire owner_cancel_done = |(port_cancel_done & owner_mask);

    // The request pulse is emitted only while there is no outstanding beat.
    // Masking the live cancel in this combinational boundary prevents a beat
    // from being fabricated on the edge that cancellation becomes visible.
    wire issue_beat =
        active && !terminal && !cancelling && !beat_outstanding &&
        !owner_cancel;

    assign beat_req = issue_beat
                    ? owner_mask : {SOURCE_COUNT{1'b0}};
    assign beat_ext = owner_ext;
    assign beat_addr =
        owner_base + {56'd0, beat_index, 6'd0};
    assign beat_wen = owner_store;
    assign beat_wdata = owner_store
                      ? stage_image[beat_index*512 +: 512]
                      : 512'd0;

    // Hold cancel until the arbiter confirms that its captured/issued beat
    // can no longer return a response to this source.
    assign port_cancel =
        active && !terminal && beat_outstanding &&
        (cancelling || owner_cancel)
            ? owner_mask : {SOURCE_COUNT{1'b0}};

    // Partial LOAD assembly is deliberately not observable.  The stage itself
    // becomes the broadcast result only at the atomic terminal boundary.
    assign result_image =
        active && terminal && terminal_success && !owner_store
            ? stage_image : 2048'd0;

    always @(posedge clk) begin
        if (rst) begin
            seen                <= {SOURCE_COUNT{1'b0}};
            last_grant          <= SOURCE_COUNT - 1;
            active              <= 1'b0;
            owner               <= {OWNER_BITS{1'b0}};
            owner_store         <= 1'b0;
            owner_ext           <= 1'b0;
            owner_base          <= 64'd0;
            owner_format_ew     <= 3'd0;
            owner_token         <= {TOKEN_BITS{1'b0}};
            stage_image         <= 2048'd0;
            beat_index          <= 2'd0;
            beat_outstanding    <= 1'b0;
            cancelling          <= 1'b0;
            terminal            <= 1'b0;
            terminal_success    <= 1'b0;
            done                <= {SOURCE_COUNT{1'b0}};
            response_token      <= {(SOURCE_COUNT*TOKEN_BITS){1'b0}};
            response_fault      <= {(SOURCE_COUNT*3){1'b0}};
            response_fault_addr <= {(SOURCE_COUNT*64){1'b0}};
        end else begin
            // A held request is one operation.  A cancelled waiting level is
            // consumed as well, and cannot reappear after cancel deasserts.
            for (i = 0; i < SOURCE_COUNT; i = i + 1) begin
                if (!req[i])
                    seen[i] <= 1'b0;
                else if (req_cancel[i])
                    seen[i] <= 1'b1;
            end

            if (active) begin
                if (terminal) begin
                    // Cancellation at the publication/retirement boundary
                    // suppresses the response just as cancellation in flight
                    // does.  Neither path can admit a replacement this cycle.
                    if (owner_cancel || owner_finish) begin
                        active              <= 1'b0;
                        owner_store         <= 1'b0;
                        owner_ext           <= 1'b0;
                        owner_base          <= 64'd0;
                        owner_format_ew     <= 3'd0;
                        owner_token         <= {TOKEN_BITS{1'b0}};
                        stage_image         <= 2048'd0;
                        beat_index          <= 2'd0;
                        beat_outstanding    <= 1'b0;
                        cancelling          <= 1'b0;
                        terminal            <= 1'b0;
                        terminal_success    <= 1'b0;
                        done                <= {SOURCE_COUNT{1'b0}};
                        response_token      <=
                            {(SOURCE_COUNT*TOKEN_BITS){1'b0}};
                        response_fault      <=
                            {(SOURCE_COUNT*3){1'b0}};
                        response_fault_addr <=
                            {(SOURCE_COUNT*64){1'b0}};
                    end
                end else if (cancelling || owner_cancel) begin
                    // No emitted beat needs a drain.  Otherwise retain the
                    // owner and stage until the arbiter confirms cancellation.
                    done <= {SOURCE_COUNT{1'b0}};
                    if (!beat_outstanding || owner_cancel_done) begin
                        active              <= 1'b0;
                        owner_store         <= 1'b0;
                        owner_ext           <= 1'b0;
                        owner_base          <= 64'd0;
                        owner_format_ew     <= 3'd0;
                        owner_token         <= {TOKEN_BITS{1'b0}};
                        stage_image         <= 2048'd0;
                        beat_index          <= 2'd0;
                        beat_outstanding    <= 1'b0;
                        cancelling          <= 1'b0;
                        terminal            <= 1'b0;
                        terminal_success    <= 1'b0;
                        response_token      <=
                            {(SOURCE_COUNT*TOKEN_BITS){1'b0}};
                        response_fault      <=
                            {(SOURCE_COUNT*3){1'b0}};
                        response_fault_addr <=
                            {(SOURCE_COUNT*64){1'b0}};
                    end else begin
                        cancelling <= 1'b1;
                    end
                end else begin
                    if (issue_beat)
                        beat_outstanding <= 1'b1;

                    if (beat_outstanding && owner_ack) begin
                        beat_outstanding <= 1'b0;

                        if (owner_error) begin
                            terminal         <= 1'b1;
                            terminal_success <= 1'b0;
                            done             <= owner_mask;
                            response_token[
                                owner*TOKEN_BITS +: TOKEN_BITS
                            ] <= owner_token;
                            response_fault[
                                owner*3 +: 3
                            ] <= MEX_FAULT_BUS;
                            response_fault_addr[
                                owner*64 +: 64
                            ] <= port_fault_addr[
                                owner*64 +: 64
                            ];
                        end else begin
                            if (!owner_store) begin
                                if ((owner_format_ew == TMODE_8) ||
                                    (owner_format_ew == TMODE_16) ||
                                    (beat_index < 2'd2))
                                    stage_image[
                                        beat_index*512 +: 512
                                    ] <= owner_ext
                                       ? ext_rdata : tile_rdata;
                            end

                            if (beat_index == 2'd3) begin
                                terminal         <= 1'b1;
                                terminal_success <= 1'b1;
                                done             <= owner_mask;
                                response_token[
                                    owner*TOKEN_BITS +: TOKEN_BITS
                                ] <= owner_token;
                                response_fault[
                                    owner*3 +: 3
                                ] <= MEX_FAULT_NONE;
                                response_fault_addr[
                                    owner*64 +: 64
                                ] <= 64'd0;
                            end else begin
                                beat_index <= beat_index + 2'd1;
                            end
                        end
                    end
                end
            end else if (next_valid) begin
                active          <= 1'b1;
                owner           <= next_owner;
                owner_store     <= req_store[next_owner];
                owner_ext       <= req_ext[next_owner];
                owner_base      <= req_base[next_owner*64 +: 64];
                owner_format_ew <=
                    req_format_ew[next_owner*3 +: 3];
                owner_token     <=
                    req_token[next_owner*TOKEN_BITS +: TOKEN_BITS];
                beat_index       <= 2'd0;
                beat_outstanding <= 1'b0;
                cancelling       <= 1'b0;
                terminal         <= 1'b0;
                terminal_success <= 1'b0;
                done             <= {SOURCE_COUNT{1'b0}};
                response_token   <=
                    {(SOURCE_COUNT*TOKEN_BITS){1'b0}};
                response_fault   <=
                    {(SOURCE_COUNT*3){1'b0}};
                response_fault_addr <=
                    {(SOURCE_COUNT*64){1'b0}};
                seen[next_owner] <= 1'b1;
                last_grant       <= next_owner;

                if (req_store[next_owner]) begin
                    if ((req_format_ew[next_owner*3 +: 3] == TMODE_8) ||
                        (req_format_ew[next_owner*3 +: 3] == TMODE_16))
                        stage_image <=
                            req_store_image[
                                next_owner*2048 +: 2048
                            ];
                    else
                        stage_image <= {
                            1024'd0,
                            req_store_image[
                                next_owner*2048 +: 1024
                            ]
                        };
                end else begin
                    // A LOAD is assembled from an all-zero staging image so
                    // ignored inactive bytes are canonical by construction.
                    stage_image <= 2048'd0;
                end
            end
        end
    end

`ifndef SYNTHESIS
    always @(posedge clk) begin
        if (!rst) begin
            if ((port_ack & (port_ack - 1'b1)) != 0)
                $error("TACC transfer received more than one ACK");
            if ((done & (done - 1'b1)) != 0)
                $error("TACC transfer published more than one done");
            if (|(port_error & ~port_ack))
                $error("TACC transfer received error without ACK");
            if (active && (owner >= SOURCE_COUNT))
                $error("TACC transfer owner is outside SOURCE_COUNT");
        end
    end
`endif

endmodule
