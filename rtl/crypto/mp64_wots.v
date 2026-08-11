// ============================================================================
// mp64_wots.v -- checked WOTS chain controller
// ============================================================================
//
// This block implements the production 32-byte, byte-access-only WOTS MMIO
// aperture.  It reads one exact 64-byte Bank-0 context through an ordinary
// read-only main-bus requester and borrows the single SHA/Keccak round service
// for nonzero chains.  It contains no private permutation datapath.
//
// DMA has separate capture and terminal-response handshakes.  CLEAR may
// withdraw an unaccepted beat, but an acceptance on the same edge wins and is
// drained before private state is scrubbed.  Terminal DONE/ERROR/IDLE is not
// published until the staged request is wiped and any shared owner is gone.

`include "mp64_pkg.vh"

module mp64_wots #(
    parameter [63:0] BANK0_SIZE = 64'h0000_0000_0010_0000,
    parameter integer N_BUS_PORTS = 1
)(
    input  wire          clk,
    input  wire          rst_n,

    // Byte-only MMIO front end.  Invalid whole accesses deliberately do not
    // acknowledge, allowing the containing bus to report one target fault.
    input  wire          req,
    input  wire [4:0]    addr,
    input  wire [63:0]   wdata,
    input  wire          wen,
    input  wire [1:0]    size,
    output reg  [63:0]   rdata,
    output reg           ack,
    output wire          active,

    // One-beat-at-a-time read-only main-bus requester.
    output wire          dma_valid,
    output wire [63:0]   dma_addr,
    input  wire          dma_accept,
    input  wire [63:0]   dma_rdata,
    input  wire          dma_resp_valid,
    input  wire [1:0]    dma_resp_code,

    // Exclusive shared Keccak service exported by mp64_sha3.
    output wire          sha_claim,
    input  wire          sha_grant,
    input  wire          sha_owned,
    output wire          sha_perm_req,
    output wire [1599:0] sha_state_in,
    input  wire [1599:0] sha_state_out,
    input  wire          sha_perm_busy,
    input  wire          sha_perm_done,
    output wire          sha_release,
    output wire          sha_abort
);

    // Architectural command/status/error values.
    localparam [7:0] CMD_NOP   = 8'd0;
    localparam [7:0] CMD_GO    = 8'd1;
    localparam [7:0] CMD_CLEAR = 8'd2;

    localparam [1:0] STATUS_IDLE  = 2'd0;
    localparam [1:0] STATUS_BUSY  = 2'd1;
    localparam [1:0] STATUS_DONE  = 2'd2;
    localparam [1:0] STATUS_ERROR = 2'd3;

    localparam [7:0] ERROR_NONE           = 8'd0;
    localparam [7:0] ERROR_COMMAND        = 8'd1;
    localparam [7:0] ERROR_OWNER          = 8'd2;
    localparam [7:0] ERROR_STEPS          = 8'd3;
    localparam [7:0] ERROR_GEOMETRY       = 8'd4;
    localparam [7:0] ERROR_SPAN           = 8'd5;
    localparam [7:0] ERROR_TARGET_FAULT   = 8'd6;
    localparam [7:0] ERROR_MEM_TIMEOUT    = 8'd7;
    localparam [7:0] ERROR_ACCEPT_TIMEOUT = 8'd8;
    localparam [7:0] ERROR_INTERNAL       = 8'd9;

    // Private controller phases.  Architectural status remains BUSY through
    // abort drain, shared-core wipe/release, and private-state cleanup.
    localparam [3:0] ST_IDLE        = 4'd0;
    localparam [3:0] ST_CLAIM       = 4'd1;
    localparam [3:0] ST_CLAIM_WAIT  = 4'd2;
    localparam [3:0] ST_DMA_REQ     = 4'd3;
    localparam [3:0] ST_DMA_WAIT    = 4'd4;
    localparam [3:0] ST_BUILD       = 4'd5;
    localparam [3:0] ST_PERM_REQ    = 4'd6;
    localparam [3:0] ST_PERM_WAIT   = 4'd7;
    localparam [3:0] ST_ABORT_DRAIN = 4'd8;
    localparam [3:0] ST_CLEANUP     = 4'd9;

    localparam [1:0] CLEAN_SUCCESS = 2'd0;
    localparam [1:0] CLEAN_ERROR   = 2'd1;
    localparam [1:0] CLEAN_CLEAR   = 2'd2;

    // Contract-derived bounds.  The safe OTHER_PORTS form leaves an invalid
    // zero-port elaboration diagnosable without creating negative arithmetic.
    localparam [127:0] OTHER_PORTS_WIDE =
        (N_BUS_PORTS >= 1) ? N_BUS_PORTS - 1 : 128'd0;
    localparam [127:0] WOTS_DMA_ACCEPT_CYCLES_WIDE =
        OTHER_PORTS_WIDE * 128'd255 * 128'd258 + 128'd1;
    localparam [127:0] WOTS_DMA_BEAT_CYCLES_WIDE =
        WOTS_DMA_ACCEPT_CYCLES_WIDE + 128'd256;
    localparam [127:0] WOTS_REQUEST_CYCLES_MAX_WIDE =
        128'd64 * WOTS_DMA_BEAT_CYCLES_WIDE +
        128'd15 * 128'd32 + 128'd512;
    localparam [127:0] WOTS_CLEAR_CYCLES_WIDE =
        WOTS_DMA_BEAT_CYCLES_WIDE + 128'd32 + 128'd64;
    localparam [63:0] WOTS_DMA_ACCEPT_CYCLES =
        WOTS_DMA_ACCEPT_CYCLES_WIDE[63:0];
    localparam integer DMA_ACCEPT_COUNTER_BITS =
        (WOTS_DMA_ACCEPT_CYCLES <= 1) ? 1 :
        $clog2(WOTS_DMA_ACCEPT_CYCLES);

    reg [63:0] context_addr_reg;
    reg [7:0]  steps_reg;
    reg [7:0]  start_reg;
    reg [1:0]  status_reg;
    reg [7:0]  error_reg;
    reg [31:0] cycles_reg;
    reg [7:0]  dout [0:15];

    reg [3:0]  state;
    reg [1:0]  cleanup_kind;
    reg [7:0]  pending_error;
    reg         req_seen;

    reg [63:0] active_context_addr;
    reg [7:0]  active_steps;
    reg [7:0]  active_start;
    reg [7:0]  context_stage [0:63];
    reg [127:0] current_node;
    reg [1599:0] perm_state_reg;
    reg [7:0]  result_private [0:15];
    reg [5:0]  dma_index;
    reg [3:0]  chain_index;
    reg [5:0]  service_cycles;
    reg [DMA_ACCEPT_COUNTER_BITS-1:0] dma_accept_count;

    // Every byte register is readable.  Only programming bytes and CMD are
    // writable; all legal transfers are exactly one byte.
    reg access_valid;
    always @(*) begin
        access_valid = 1'b0;
        if (size == BUS_BYTE) begin
            if (wen)
                access_valid = (addr <= 5'h0a);
            else
                access_valid = 1'b1;
        end
    end

    wire mmio_fire = req && !req_seen && access_valid;
    wire clear_command = mmio_fire && wen && addr == 5'h0a &&
                         wdata[7:0] == CMD_CLEAR;

    wire [64:0] context_span_end =
        {1'b0, context_addr_reg} + 65'd64;
    wire context_span_valid = !context_span_end[64] &&
                              context_span_end[63:0] <= BANK0_SIZE;
    wire [8:0] geometry_end =
        {1'b0, start_reg} + {1'b0, steps_reg};

    // Construct exactly one SHAKE256 rate block for the current chain step.
    // State byte numbering matches the little-endian Keccak lane layout.
    reg [1599:0] constructed_state;
    integer build_i;
    always @(*) begin
        constructed_state = 1600'd0;
        for (build_i = 0; build_i < 16; build_i = build_i + 1)
            constructed_state[build_i*8 +: 8] =
                context_stage[build_i];
        for (build_i = 0; build_i < 28; build_i = build_i + 1)
            constructed_state[(16+build_i)*8 +: 8] =
                context_stage[16+build_i];
        constructed_state[44*8 +: 8] = 8'd0;
        constructed_state[45*8 +: 8] = 8'd0;
        constructed_state[46*8 +: 8] = 8'd0;
        constructed_state[47*8 +: 8] =
            active_start + {4'd0, chain_index};
        for (build_i = 0; build_i < 16; build_i = build_i + 1)
            constructed_state[(48+build_i)*8 +: 8] =
                current_node[build_i*8 +: 8];
        constructed_state[64*8 +: 8]  = 8'h1f;
        constructed_state[135*8 +: 8] = 8'h80;
    end

    assign active       = (status_reg == STATUS_BUSY);
    assign dma_valid    = (state == ST_DMA_REQ);
    assign dma_addr     = active_context_addr + {58'd0, dma_index};
    assign sha_claim    = (state == ST_CLAIM);
    assign sha_perm_req = (state == ST_PERM_REQ);
    assign sha_state_in = perm_state_reg;
    assign sha_release  = (state == ST_CLEANUP) &&
                          (cleanup_kind == CLEAN_SUCCESS) && sha_owned;
    assign sha_abort    = ((state == ST_ABORT_DRAIN) ||
                           ((state == ST_CLEANUP) &&
                            (cleanup_kind != CLEAN_SUCCESS))) && sha_owned;

    wire [7:0] dma_byte = dma_rdata[dma_addr[2:0]*8 +: 8];

    integer i;
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            ack                 <= 1'b0;
            rdata               <= 64'd0;
            req_seen            <= 1'b0;
            context_addr_reg    <= 64'd0;
            steps_reg           <= 8'd0;
            start_reg           <= 8'd0;
            status_reg          <= STATUS_IDLE;
            error_reg           <= ERROR_NONE;
            cycles_reg          <= 32'd0;
            state               <= ST_IDLE;
            cleanup_kind        <= CLEAN_CLEAR;
            pending_error       <= ERROR_NONE;
            active_context_addr <= 64'd0;
            active_steps        <= 8'd0;
            active_start        <= 8'd0;
            current_node        <= 128'd0;
            perm_state_reg      <= 1600'd0;
            dma_index           <= 6'd0;
            chain_index         <= 4'd0;
            service_cycles      <= 6'd0;
            dma_accept_count    <= {DMA_ACCEPT_COUNTER_BITS{1'b0}};
            for (i = 0; i < 64; i = i + 1)
                context_stage[i] <= 8'd0;
            for (i = 0; i < 16; i = i + 1) begin
                dout[i]           <= 8'd0;
                result_private[i] <= 8'd0;
            end
        end else begin
            ack   <= 1'b0;
            rdata <= 64'd0;

            if (!req)
                req_seen <= 1'b0;

            if (status_reg == STATUS_BUSY && cycles_reg != 32'hffff_ffff)
                cycles_reg <= cycles_reg + 32'd1;

            // ------------------------------------------------------------
            // Private execution state machine
            // ------------------------------------------------------------
            case (state)
                ST_IDLE: ;

                ST_CLAIM: begin
                    if (clear_command) begin
                        cleanup_kind  <= CLEAN_CLEAR;
                        pending_error <= ERROR_NONE;
                        state         <= ST_CLEANUP;
                    end else begin
                        state <= ST_CLAIM_WAIT;
                    end
                end

                ST_CLAIM_WAIT: begin
                    if (clear_command) begin
                        cleanup_kind  <= CLEAN_CLEAR;
                        pending_error <= ERROR_NONE;
                        state         <= ST_CLEANUP;
                    end else if (sha_grant && sha_owned) begin
                        dma_index        <= 6'd0;
                        dma_accept_count <=
                            {DMA_ACCEPT_COUNTER_BITS{1'b0}};
                        state            <= ST_DMA_REQ;
                    end else begin
                        cleanup_kind  <= CLEAN_ERROR;
                        pending_error <= ERROR_OWNER;
                        state         <= ST_CLEANUP;
                    end
                end

                ST_DMA_REQ: begin
                    // Capture wins over same-edge CLEAR and local timeout.
                    if (dma_accept) begin
                        dma_accept_count <=
                            {DMA_ACCEPT_COUNTER_BITS{1'b0}};
                        state <= clear_command ? ST_ABORT_DRAIN : ST_DMA_WAIT;
                    end else if (clear_command) begin
                        cleanup_kind  <= CLEAN_CLEAR;
                        pending_error <= ERROR_NONE;
                        state         <= ST_CLEANUP;
                    end else if (dma_accept_count ==
                                 WOTS_DMA_ACCEPT_CYCLES - 64'd1) begin
                        cleanup_kind  <= CLEAN_ERROR;
                        pending_error <= ERROR_ACCEPT_TIMEOUT;
                        state         <= ST_CLEANUP;
                    end else begin
                        dma_accept_count <= dma_accept_count + 1'b1;
                    end
                end

                ST_DMA_WAIT: begin
                    // A response and CLEAR on one edge consumes the owed beat
                    // but suppresses both data advance and terminal error.
                    if (dma_resp_valid) begin
                        if (clear_command) begin
                            cleanup_kind  <= CLEAN_CLEAR;
                            pending_error <= ERROR_NONE;
                            state         <= ST_CLEANUP;
                        end else if (dma_resp_code == BUS_RESP_OK) begin
                            context_stage[dma_index] <= dma_byte;
                            if (dma_index == 6'd63) begin
                                if (active_steps == 8'd0) begin
                                    for (i = 0; i < 15; i = i + 1)
                                        result_private[i] <=
                                            context_stage[48+i];
                                    result_private[15] <= dma_byte;
                                    cleanup_kind       <= CLEAN_SUCCESS;
                                    pending_error      <= ERROR_NONE;
                                    state              <= ST_CLEANUP;
                                end else begin
                                    for (i = 0; i < 15; i = i + 1)
                                        current_node[i*8 +: 8] <=
                                            context_stage[48+i];
                                    current_node[15*8 +: 8] <= dma_byte;
                                    chain_index <= 4'd0;
                                    state       <= ST_BUILD;
                                end
                            end else begin
                                dma_index        <= dma_index + 6'd1;
                                dma_accept_count <=
                                    {DMA_ACCEPT_COUNTER_BITS{1'b0}};
                                state            <= ST_DMA_REQ;
                            end
                        end else begin
                            cleanup_kind <= CLEAN_ERROR;
                            if (dma_resp_code == BUS_RESP_TARGET_FAULT)
                                pending_error <= ERROR_TARGET_FAULT;
                            else if (dma_resp_code == BUS_RESP_MEM_TIMEOUT)
                                pending_error <= ERROR_MEM_TIMEOUT;
                            else
                                pending_error <= ERROR_INTERNAL;
                            state <= ST_CLEANUP;
                        end
                    end else if (clear_command) begin
                        state <= ST_ABORT_DRAIN;
                    end
                end

                ST_ABORT_DRAIN: begin
                    if (dma_resp_valid) begin
                        cleanup_kind  <= CLEAN_CLEAR;
                        pending_error <= ERROR_NONE;
                        state         <= ST_CLEANUP;
                    end
                end

                ST_BUILD: begin
                    if (clear_command) begin
                        cleanup_kind  <= CLEAN_CLEAR;
                        pending_error <= ERROR_NONE;
                        state         <= ST_CLEANUP;
                    end else if (!sha_owned) begin
                        cleanup_kind  <= CLEAN_ERROR;
                        pending_error <= ERROR_INTERNAL;
                        state         <= ST_CLEANUP;
                    end else begin
                        perm_state_reg <= constructed_state;
                        service_cycles <= 6'd0;
                        state          <= ST_PERM_REQ;
                    end
                end

                ST_PERM_REQ: begin
                    if (clear_command) begin
                        cleanup_kind  <= CLEAN_CLEAR;
                        pending_error <= ERROR_NONE;
                        state         <= ST_CLEANUP;
                    end else if (!sha_owned || sha_perm_busy) begin
                        cleanup_kind  <= CLEAN_ERROR;
                        pending_error <= ERROR_INTERNAL;
                        state         <= ST_CLEANUP;
                    end else begin
                        service_cycles <= 6'd1;
                        state          <= ST_PERM_WAIT;
                    end
                end

                ST_PERM_WAIT: begin
                    if (clear_command) begin
                        cleanup_kind  <= CLEAN_CLEAR;
                        pending_error <= ERROR_NONE;
                        state         <= ST_CLEANUP;
                    end else if (sha_perm_done) begin
                        service_cycles <= 6'd0;
                        current_node   <= sha_state_out[127:0];
                        if ({1'b0, chain_index} + 5'd1 == active_steps[4:0]) begin
                            for (i = 0; i < 16; i = i + 1)
                                result_private[i] <=
                                    sha_state_out[i*8 +: 8];
                            cleanup_kind  <= CLEAN_SUCCESS;
                            pending_error <= ERROR_NONE;
                            state         <= ST_CLEANUP;
                        end else begin
                            chain_index <= chain_index + 4'd1;
                            state       <= ST_BUILD;
                        end
                    end else if (!sha_owned || service_cycles == 6'd31) begin
                        cleanup_kind  <= CLEAN_ERROR;
                        pending_error <= ERROR_INTERNAL;
                        state         <= ST_CLEANUP;
                    end else begin
                        service_cycles <= service_cycles + 6'd1;
                    end
                end

                ST_CLEANUP: begin
                    // Wipe every nonarchitectural request field before a
                    // terminal state can become visible.
                    active_context_addr <= 64'd0;
                    active_steps        <= 8'd0;
                    active_start        <= 8'd0;
                    current_node        <= 128'd0;
                    perm_state_reg      <= 1600'd0;
                    dma_index           <= 6'd0;
                    chain_index         <= 4'd0;
                    service_cycles      <= 6'd0;
                    dma_accept_count    <=
                        {DMA_ACCEPT_COUNTER_BITS{1'b0}};
                    for (i = 0; i < 64; i = i + 1)
                        context_stage[i] <= 8'd0;

                    if (clear_command) begin
                        cleanup_kind  <= CLEAN_CLEAR;
                        pending_error <= ERROR_NONE;
                        for (i = 0; i < 16; i = i + 1)
                            result_private[i] <= 8'd0;
                        if (!sha_owned) begin
                            context_addr_reg <= 64'd0;
                            steps_reg        <= 8'd0;
                            start_reg        <= 8'd0;
                            status_reg       <= STATUS_IDLE;
                            error_reg        <= ERROR_NONE;
                            for (i = 0; i < 16; i = i + 1)
                                dout[i] <= 8'd0;
                            cleanup_kind <= CLEAN_CLEAR;
                            state        <= ST_IDLE;
                        end
                    end else if (!sha_owned) begin
                        if (cleanup_kind == CLEAN_SUCCESS) begin
                            for (i = 0; i < 16; i = i + 1) begin
                                dout[i]           <= result_private[i];
                                result_private[i] <= 8'd0;
                            end
                            status_reg <= STATUS_DONE;
                            error_reg  <= ERROR_NONE;
                        end else if (cleanup_kind == CLEAN_ERROR) begin
                            for (i = 0; i < 16; i = i + 1) begin
                                dout[i]           <= 8'd0;
                                result_private[i] <= 8'd0;
                            end
                            status_reg <= STATUS_ERROR;
                            error_reg  <= pending_error;
                        end else begin
                            context_addr_reg <= 64'd0;
                            steps_reg        <= 8'd0;
                            start_reg        <= 8'd0;
                            status_reg       <= STATUS_IDLE;
                            error_reg        <= ERROR_NONE;
                            for (i = 0; i < 16; i = i + 1) begin
                                dout[i]           <= 8'd0;
                                result_private[i] <= 8'd0;
                            end
                        end
                        pending_error <= ERROR_NONE;
                        cleanup_kind  <= CLEAN_CLEAR;
                        state         <= ST_IDLE;
                    end
                end

                default: begin
                    cleanup_kind  <= CLEAN_ERROR;
                    pending_error <= ERROR_INTERNAL;
                    state         <= ST_CLEANUP;
                end
            endcase

            // ------------------------------------------------------------
            // MMIO transaction and architectural state
            // ------------------------------------------------------------
            if (req && !req_seen) begin
                req_seen <= 1'b1;
                if (access_valid) begin
                    ack <= 1'b1;
                    if (!wen) begin
                        if (addr <= 5'h07)
                            rdata <= {56'd0,
                                context_addr_reg[addr*8 +: 8]};
                        else if (addr == 5'h08)
                            rdata <= {56'd0, steps_reg};
                        else if (addr == 5'h09)
                            rdata <= {56'd0, start_reg};
                        else if (addr == 5'h0a)
                            rdata <= {62'd0, status_reg};
                        else if (addr == 5'h0b)
                            rdata <= {56'd0, error_reg};
                        else if (addr <= 5'h0f)
                            rdata <= {56'd0,
                                cycles_reg[(addr-5'h0c)*8 +: 8]};
                        else
                            rdata <= {56'd0, dout[addr-5'h10]};
                    end else if (addr <= 5'h09) begin
                        if (status_reg == STATUS_IDLE) begin
                            if (addr <= 5'h07)
                                context_addr_reg[addr*8 +: 8] <= wdata[7:0];
                            else if (addr == 5'h08)
                                steps_reg <= wdata[7:0];
                            else
                                start_reg <= wdata[7:0];
                        end
                    end else begin
                        case (wdata[7:0])
                            CMD_NOP: ;

                            CMD_CLEAR: begin
                                // BUSY cleanup/drain is owned by the FSM.
                                if (status_reg != STATUS_BUSY) begin
                                    context_addr_reg    <= 64'd0;
                                    steps_reg           <= 8'd0;
                                    start_reg           <= 8'd0;
                                    status_reg          <= STATUS_IDLE;
                                    error_reg           <= ERROR_NONE;
                                    active_context_addr <= 64'd0;
                                    active_steps        <= 8'd0;
                                    active_start        <= 8'd0;
                                    current_node        <= 128'd0;
                                    perm_state_reg      <= 1600'd0;
                                    dma_index           <= 6'd0;
                                    chain_index         <= 4'd0;
                                    service_cycles      <= 6'd0;
                                    dma_accept_count    <=
                                        {DMA_ACCEPT_COUNTER_BITS{1'b0}};
                                    pending_error <= ERROR_NONE;
                                    cleanup_kind  <= CLEAN_CLEAR;
                                    state         <= ST_IDLE;
                                    for (i = 0; i < 64; i = i + 1)
                                        context_stage[i] <= 8'd0;
                                    for (i = 0; i < 16; i = i + 1) begin
                                        dout[i]           <= 8'd0;
                                        result_private[i] <= 8'd0;
                                    end
                                end
                            end

                            CMD_GO: begin
                                if (status_reg == STATUS_IDLE) begin
                                    for (i = 0; i < 16; i = i + 1)
                                        dout[i] <= 8'd0;
                                    error_reg  <= ERROR_NONE;
                                    cycles_reg <= 32'd0;

                                    // Ordered validation: steps, geometry,
                                    // complete Bank-0 span, then ownership.
                                    if (steps_reg > 8'd15) begin
                                        status_reg <= STATUS_ERROR;
                                        error_reg  <= ERROR_STEPS;
                                    end else if (start_reg > 8'd15 ||
                                        (steps_reg != 8'd0 &&
                                         geometry_end > 9'd15)) begin
                                        status_reg <= STATUS_ERROR;
                                        error_reg  <= ERROR_GEOMETRY;
                                    end else if (!context_span_valid) begin
                                        status_reg <= STATUS_ERROR;
                                        error_reg  <= ERROR_SPAN;
                                    end else begin
                                        active_context_addr <= context_addr_reg;
                                        active_steps        <= steps_reg;
                                        active_start        <= start_reg;
                                        dma_index           <= 6'd0;
                                        chain_index         <= 4'd0;
                                        dma_accept_count    <=
                                            {DMA_ACCEPT_COUNTER_BITS{1'b0}};
                                        status_reg <= STATUS_BUSY;
                                        state <= (steps_reg == 8'd0) ?
                                                 ST_DMA_REQ : ST_CLAIM;
                                    end
                                end
                            end

                            default: begin
                                if (status_reg == STATUS_IDLE) begin
                                    for (i = 0; i < 16; i = i + 1)
                                        dout[i] <= 8'd0;
                                    status_reg <= STATUS_ERROR;
                                    error_reg  <= ERROR_COMMAND;
                                end
                            end
                        endcase
                    end
                end
            end
        end
    end

`ifndef SYNTHESIS
    initial begin
        if (N_BUS_PORTS < 1)
            $error("mp64_wots: N_BUS_PORTS must be at least one");
        if (|WOTS_DMA_ACCEPT_CYCLES_WIDE[127:64])
            $error("mp64_wots: DMA accept deadline exceeds 64 bits");
        if (|WOTS_REQUEST_CYCLES_MAX_WIDE[127:63])
            $error("mp64_wots: request deadline exceeds signed-safe range");
        if (|WOTS_CLEAR_CYCLES_WIDE[127:63])
            $error("mp64_wots: clear deadline exceeds signed-safe range");
    end
`endif

endmodule
