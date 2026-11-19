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
// STORE snapshots its source bank at admission.  TMODE_FP64 forces canonical
// image bytes 64..255 to zero, and every mode other than TMODE_8, TMODE_16
// and TMODE_FP64 bytes 128..255.
//
// A STORE whose image lies in a chiplet port's window arrives with req_link
// and the port's index (docs/chiplet-link.md section 3).  The stage then
// sends no beat: it offers that port's bridge one write of the image's
// active 64-byte units, four, two or one, through the same unit multiplexer,
// with the remaining units of the 256 bytes counted as zeros, and holds the
// image until the bridge reports the chiplet's answer.  Any failure the
// bridge reports ends the STORE with a bus fault at the image base.
// Cancellation is held on the bridge until it confirms that the write can
// no longer complete.
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
    input  wire [SOURCE_COUNT-1:0]      req_link,
    input  wire [SOURCE_COUNT*8-1:0]    req_port,
    input  wire [SOURCE_COUNT*64-1:0]   req_base,
    input  wire [SOURCE_COUNT*4-1:0]    req_format_ew,
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
    // One bit per requesting engine. A cycle is stalled when its held image
    // request makes no terminal or acknowledged-beat progress, including
    // stage acquisition, peer ownership, arbiter capture, and target wait.
    output wire [SOURCE_COUNT-1:0]      stall_cycle,

    // Held, one-hot terminal response.  Packed lane N belongs to source N.
    output reg  [SOURCE_COUNT-1:0]      done,
    output reg  [SOURCE_COUNT*TOKEN_BITS-1:0] response_token,
    output reg  [SOURCE_COUNT*3-1:0]    response_fault,
    output reg  [SOURCE_COUNT*64-1:0]   response_fault_addr,

    // Broadcast result; meaningful only with a successful LOAD done.
    output wire [2047:0]                result_image,

    // A STORE into a chiplet's window: one write to the bridge of port
    // link_port (mp64_chiplet_port.v, its bw_* side).  link_valid and the
    // current unit hold until link_ready.  link_done, link_error and
    // link_cancel_done are the bridge's pulses.
    output wire                         link_valid,
    input  wire                         link_ready,
    output wire [7:0]                   link_port,
    output wire [63:0]                  link_addr,
    output wire [511:0]                 link_data,
    output wire [7:0]                   link_len,
    output wire [7:0]                   link_zeros,
    output wire                         link_cancel,
    input  wire                         link_done,
    input  wire                         link_error,
    input  wire                         link_cancel_done
);

    reg [SOURCE_COUNT-1:0] seen;
    reg [OWNER_BITS-1:0]   last_grant;

    reg                    active;
    reg [OWNER_BITS-1:0]   owner;
    reg                    owner_store;
    reg                    owner_ext;
    reg                    owner_link;   // the image goes to a chiplet port
    reg [7:0]              owner_port;
    reg [63:0]             owner_base;
    reg [3:0]              owner_format_ew;

    // Active 64-byte beats of a TACC image (docs/tile-engine.md): U8 and U16
    // use all four, FP64 one, and every other format two.  Inactive bytes
    // load as ignored input that commits as zero and store as zero.
    function [2:0] active_beats;
        input [3:0] ew;
        case (ew)
            TMODE_8, TMODE_16: active_beats = 3'd4;
            TMODE_FP64:        active_beats = 3'd1;
            default:           active_beats = 3'd2;
        endcase
    endfunction

    function [2047:0] active_mask;
        input [3:0] ew;
        case (ew)
            TMODE_8, TMODE_16: active_mask = {2048{1'b1}};
            TMODE_FP64:        active_mask = {1536'd0, {512{1'b1}}};
            default:           active_mask = {1024'd0, {1024{1'b1}}};
        endcase
    endfunction
    reg [TOKEN_BITS-1:0]   owner_token;

    // This is the sole chip-wide canonical transfer image.
    reg [2047:0] stage_image;

    reg [1:0] beat_index;       // also the link's unit index
    reg       beat_outstanding;
    reg       link_sent;        // the bridge has taken every unit
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
    wire link_unit_go = link_valid && link_ready;
    wire [SOURCE_COUNT-1:0] owner_progress =
        (active && (owner_ack ||
                    (owner_link && (link_unit_go || link_done))))
            ? owner_mask : {SOURCE_COUNT{1'b0}};

    assign stall_cycle =
        req & ~req_cancel & ~done & ~owner_progress;

    // The request pulse is emitted only while there is no outstanding beat.
    // Masking the live cancel in this combinational boundary prevents a beat
    // from being fabricated on the edge that cancellation becomes visible.
    wire issue_beat =
        active && !owner_link && !terminal && !cancelling &&
        !beat_outstanding && !owner_cancel;

    assign beat_req = issue_beat
                    ? owner_mask : {SOURCE_COUNT{1'b0}};
    assign beat_ext = owner_ext;
    assign beat_addr =
        owner_base + {56'd0, beat_index, 6'd0};
    assign beat_wen = owner_store;
    assign beat_wdata = owner_store
                      ? stage_image[beat_index*512 +: 512]
                      : 512'd0;

    // The link write offers units from the same multiplexer.
    wire [2:0] owner_units = active_beats(owner_format_ew);
    assign link_valid =
        active && owner_link && !terminal && !cancelling &&
        !owner_cancel && !link_sent;
    assign link_port  = owner_port;
    assign link_addr  = owner_base;
    assign link_data  = beat_wdata;
    assign link_len   = {5'd0, owner_units} - 8'd1;
    assign link_zeros = 8'd4 - {5'd0, owner_units};
    // Held until the bridge confirms, as port_cancel is for a beat.
    assign link_cancel =
        active && owner_link && !terminal && (cancelling || owner_cancel);

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
            owner_link          <= 1'b0;
            owner_port          <= 8'd0;
            owner_base          <= 64'd0;
            owner_format_ew     <= 4'd0;
            owner_token         <= {TOKEN_BITS{1'b0}};
            stage_image         <= 2048'd0;
            beat_index          <= 2'd0;
            beat_outstanding    <= 1'b0;
            link_sent           <= 1'b0;
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
                        owner_link          <= 1'b0;
                        owner_port          <= 8'd0;
                        owner_base          <= 64'd0;
                        owner_format_ew     <= 4'd0;
                        owner_token         <= {TOKEN_BITS{1'b0}};
                        stage_image         <= 2048'd0;
                        beat_index          <= 2'd0;
                        beat_outstanding    <= 1'b0;
                        link_sent           <= 1'b0;
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
                    if (owner_link ? link_cancel_done
                                   : (!beat_outstanding ||
                                      owner_cancel_done)) begin
                        active              <= 1'b0;
                        owner_store         <= 1'b0;
                        owner_ext           <= 1'b0;
                        owner_link          <= 1'b0;
                        owner_port          <= 8'd0;
                        owner_base          <= 64'd0;
                        owner_format_ew     <= 4'd0;
                        owner_token         <= {TOKEN_BITS{1'b0}};
                        stage_image         <= 2048'd0;
                        beat_index          <= 2'd0;
                        beat_outstanding    <= 1'b0;
                        link_sent           <= 1'b0;
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
                end else if (owner_link) begin
                    if (link_done) begin
                        // The chiplet's answer, or the bridge's own
                        // refusal, timeout or loss of the link.
                        terminal         <= 1'b1;
                        terminal_success <= !link_error;
                        done             <= owner_mask;
                        response_token[
                            owner*TOKEN_BITS +: TOKEN_BITS
                        ] <= owner_token;
                        response_fault[
                            owner*3 +: 3
                        ] <= link_error ? MEX_FAULT_BUS : MEX_FAULT_NONE;
                        response_fault_addr[
                            owner*64 +: 64
                        ] <= link_error ? owner_base : 64'd0;
                    end else if (link_unit_go) begin
                        if ({1'b0, beat_index} == owner_units - 3'd1)
                            link_sent <= 1'b1;
                        else
                            beat_index <= beat_index + 2'd1;
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
                                if ({1'b0, beat_index} <
                                    active_beats(owner_format_ew))
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
                owner_link      <= req_link[next_owner];
                owner_port      <= req_port[next_owner*8 +: 8];
                owner_base      <= req_base[next_owner*64 +: 64];
                owner_format_ew <=
                    req_format_ew[next_owner*4 +: 4];
                owner_token     <=
                    req_token[next_owner*TOKEN_BITS +: TOKEN_BITS];
                beat_index       <= 2'd0;
                beat_outstanding <= 1'b0;
                link_sent        <= 1'b0;
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
                    // Inactive image bytes store as zero.
                    stage_image <=
                        req_store_image[next_owner*2048 +: 2048] &
                        active_mask(req_format_ew[next_owner*4 +: 4]);
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
            if (active && owner_link && !owner_store)
                $error("TACC transfer routed a LOAD to a chiplet port");
            if (link_done && !(active && owner_link && !terminal &&
                               !cancelling))
                $error("TACC transfer received a link answer it did not await");
        end
    end
`endif

endmodule
