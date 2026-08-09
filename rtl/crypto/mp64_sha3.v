// ============================================================================
// mp64_sha3.v -- checked SHA3/SHAKE and raw Keccak MMIO front end
// ============================================================================
//
// The front end implements the 96-byte checkpoint-2 register window.  It owns
// policy, padding, output-window construction, and arbitration; the reusable
// mp64_keccak_core below it is the sole 24-round datapath.
//
// MMIO sizes use the common BUS_* encoding.  Byte accesses are accepted at
// byte registers.  Aligned qwords are additionally accepted for DOUT and
// STATE_DATA.  A reserved address, wrong direction, forbidden size,
// misalignment, or crossing access is deliberately left unacknowledged so the
// containing SoC reports one architectural bus fault without partial mutation.
//
// The WOTS service port is a claim/execute/release interface for checkpoint 3.
// While wots_owned is asserted, STATUS is 0x0d and the MMIO front end remains
// responsive, but no MMIO state or command mutation can disturb the owner.

`include "mp64_pkg.vh"

module mp64_sha3 (
    input  wire          clk,
    input  wire          rst_n,

    input  wire          req,
    input  wire [6:0]    addr,
    input  wire [63:0]   wdata,
    input  wire          wen,
    input  wire [1:0]    size,
    output reg  [63:0]   rdata,
    output reg           ack,

    // Independently advertised public paths.
    input  wire          sha3_stream_en,
    input  wire          keccak_f1600_en,

    // Shared-service requester used by the production WOTS controller.
    input  wire          wots_claim,
    output reg           wots_grant,
    output wire          wots_owned,
    input  wire          wots_perm_req,
    input  wire [1599:0] wots_state_in,
    output wire [1599:0] wots_state_out,
    output wire          wots_perm_busy,
    output reg           wots_perm_done,
    input  wire          wots_release,
    input  wire          wots_abort
);

    localparam [1:0] PHASE_IDLE  = 2'd0;
    localparam [1:0] PHASE_BUSY  = 2'd1;
    localparam [1:0] PHASE_DONE  = 2'd2;
    localparam [1:0] PHASE_ERROR = 2'd3;

    localparam [1:0] OWNER_NONE   = 2'd0;
    localparam [1:0] OWNER_SPONGE = 2'd1;
    localparam [1:0] OWNER_RAW    = 2'd2;
    localparam [1:0] OWNER_WOTS   = 2'd3;

    localparam [7:0] ERROR_NONE        = 8'd0;
    localparam [7:0] ERROR_COMMAND     = 8'd1;
    localparam [7:0] ERROR_CONFLICT    = 8'd2;
    localparam [7:0] ERROR_MODE        = 8'd3;
    localparam [7:0] ERROR_STATE_INDEX = 8'd4;
    localparam [7:0] ERROR_SERVICE     = 8'd5;
    localparam [7:0] ERROR_UNAVAILABLE = 8'd6;

    localparam [2:0] OP_NONE      = 3'd0;
    localparam [2:0] OP_AUTO      = 3'd1;
    localparam [2:0] OP_FINAL     = 3'd2;
    localparam [2:0] OP_NEXT_COPY = 3'd3;
    localparam [2:0] OP_NEXT_PERM = 3'd4;
    localparam [2:0] OP_RAW       = 3'd5;

    localparam [1:0] CLEAN_MMIO    = 2'd0;
    localparam [1:0] CLEAN_FAILURE = 2'd1;
    localparam [1:0] CLEAN_WOTS    = 2'd2;

    reg [1:0] owner;
    reg [1:0] phase;
    reg [7:0] error_code;
    reg [1:0] mode;
    reg [7:0] din_ptr;
    reg [8:0] squeeze_pos;
    reg [4:0] state_index;

    reg [7:0] output_window [0:63];
    reg [7:0] crossing_tail [0:63];
    reg [6:0] crossing_count;

    reg [2:0] operation;
    reg [5:0] operation_cycles;
    reg       cleanup_pending;
    reg [1:0] cleanup_reason;
    reg       req_seen;
    reg       wots_claim_seen;

    wire [7:0] rate_bytes = (mode == 2'd0) ? 8'd136 :
                            (mode == 2'd1) ? 8'd72  :
                            (mode == 2'd2) ? 8'd168 : 8'd136;

    // --------------------------------------------------------------------
    // One shared Keccak service
    // --------------------------------------------------------------------
    reg          core_start;
    reg          core_load_start;
    reg [1599:0] core_state_in;
    reg          core_lane_we;
    reg [4:0]    core_lane_index;
    reg [63:0]   core_lane_wdata;
    reg [7:0]    core_lane_wstrb;
    reg          core_clear;

    wire [63:0]   core_lane_rdata;
    wire [1599:0] core_state_out;
    wire          core_busy;
    wire          core_done;
    wire          core_clear_done;

    mp64_keccak_core u_keccak_core (
        .clk         (clk),
        .rst_n       (rst_n),
        .start       (core_start),
        .load_start  (core_load_start),
        .state_in    (core_state_in),
        .lane_we     (core_lane_we),
        .lane_index  (core_lane_index),
        .lane_wdata  (core_lane_wdata),
        .lane_wstrb  (core_lane_wstrb),
        .lane_rdata  (core_lane_rdata),
        .state_out   (core_state_out),
        .clear       (core_clear),
        .clear_done  (core_clear_done),
        .busy        (core_busy),
        .done        (core_done)
    );

    assign wots_owned     = (owner == OWNER_WOTS);
    assign wots_state_out = (owner == OWNER_WOTS) ? core_state_out : 1600'd0;
    assign wots_perm_busy = (owner == OWNER_WOTS) && core_busy;

    // A new WOTS claim wins a same-cycle collision with an MMIO mutation.
    // Reads still retire against the pre-edge state and become 0x0d on the
    // following access.
    wire wots_claim_accept = wots_claim && !wots_claim_seen &&
                             owner == OWNER_NONE && phase == PHASE_IDLE &&
                             !cleanup_pending && !core_busy;

    // --------------------------------------------------------------------
    // Whole-access MMIO preflight
    // --------------------------------------------------------------------
    reg access_valid;
    always @(*) begin
        access_valid = 1'b0;
        if (size == BUS_BYTE) begin
            if (wen) begin
                access_valid = (addr == 7'h00) || (addr == 7'h02) ||
                               (addr == 7'h08) || (addr == 7'h50) ||
                               (addr >= 7'h58 && addr <= 7'h5f);
            end else begin
                access_valid = (addr == 7'h00) || (addr == 7'h01) ||
                               (addr == 7'h02) || (addr == 7'h03) ||
                               (addr == 7'h08) ||
                               (addr >= 7'h10 && addr <= 7'h4f) ||
                               (addr == 7'h50) ||
                               (addr >= 7'h58 && addr <= 7'h5f);
            end
        end else if (size == BUS_DWORD) begin
            if (wen)
                access_valid = (addr == 7'h58);
            else
                access_valid = (addr == 7'h58) ||
                               (addr >= 7'h10 && addr <= 7'h48 &&
                                addr[2:0] == 3'b000);
        end
    end

    // The only MMIO request held rather than retired is the byte immediately
    // following a full rate block.  It is accepted at byte zero after the
    // automatic permutation completes.
    wire hold_auto_din = req && !req_seen && access_valid && wen &&
                         size == BUS_BYTE && addr == 7'h08 &&
                         owner == OWNER_SPONGE && phase == PHASE_BUSY &&
                         operation == OP_AUTO && !wots_claim_accept;

    // Candidate states for operations that must atomically alter bytes before
    // round zero.  Keccak lanes and the memory image are both little endian.
    reg [1599:0] absorbed_state;
    reg [1599:0] padded_state;
    always @(*) begin
        absorbed_state = core_state_out;
        absorbed_state[din_ptr*8 +: 8] =
            core_state_out[din_ptr*8 +: 8] ^ wdata[7:0];

        padded_state = core_state_out;
        padded_state[din_ptr*8 +: 8] =
            padded_state[din_ptr*8 +: 8] ^
            ((mode >= 2'd2) ? 8'h1f : 8'h06);
        padded_state[(rate_bytes-1'b1)*8 +: 8] =
            padded_state[(rate_bytes-1'b1)*8 +: 8] ^ 8'h80;
    end

    integer i;
    integer tail_count;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            ack               <= 1'b0;
            rdata             <= 64'd0;
            req_seen          <= 1'b0;
            wots_claim_seen   <= 1'b0;
            wots_grant        <= 1'b0;
            wots_perm_done    <= 1'b0;

            owner             <= OWNER_NONE;
            phase             <= PHASE_IDLE;
            error_code        <= ERROR_NONE;
            mode              <= 2'd0;
            din_ptr           <= 8'd0;
            squeeze_pos       <= 9'd0;
            state_index       <= 5'd0;
            crossing_count    <= 7'd0;
            operation         <= OP_NONE;
            operation_cycles  <= 6'd0;
            cleanup_pending   <= 1'b0;
            cleanup_reason    <= CLEAN_MMIO;

            core_start        <= 1'b0;
            core_load_start   <= 1'b0;
            core_state_in     <= 1600'd0;
            core_lane_we      <= 1'b0;
            core_lane_index   <= 5'd0;
            core_lane_wdata   <= 64'd0;
            core_lane_wstrb   <= 8'd0;
            core_clear        <= 1'b0;

            for (i = 0; i < 64; i = i + 1) begin
                output_window[i] <= 8'd0;
                crossing_tail[i] <= 8'd0;
            end
        end else begin
            ack             <= 1'b0;
            rdata           <= 64'd0;
            wots_grant      <= 1'b0;
            wots_perm_done  <= 1'b0;
            core_start      <= 1'b0;
            core_load_start <= 1'b0;
            core_state_in   <= 1600'd0;
            core_lane_we    <= 1'b0;
            core_lane_wdata <= 64'd0;
            core_lane_wstrb <= 8'd0;
            core_clear      <= 1'b0;

            if (!req)
                req_seen <= 1'b0;
            if (!wots_claim)
                wots_claim_seen <= 1'b0;

            // ------------------------------------------------------------
            // Completion, watchdog, and ordered cleanup
            // ------------------------------------------------------------
            if (cleanup_pending) begin
                if (core_clear_done) begin
                    cleanup_pending  <= 1'b0;
                    operation        <= OP_NONE;
                    operation_cycles <= 6'd0;
                    din_ptr          <= 8'd0;
                    squeeze_pos      <= 9'd0;
                    state_index      <= 5'd0;
                    crossing_count   <= 7'd0;
                    owner            <= OWNER_NONE;
                    for (i = 0; i < 64; i = i + 1) begin
                        output_window[i] <= 8'd0;
                        crossing_tail[i] <= 8'd0;
                    end

                    if (cleanup_reason == CLEAN_FAILURE) begin
                        phase      <= PHASE_ERROR;
                        error_code <= ERROR_SERVICE;
                    end else begin
                        phase      <= PHASE_IDLE;
                        error_code <= ERROR_NONE;
                    end
                end
            end else if (owner == OWNER_WOTS) begin
                // WOTS owns the service across every chain step, including
                // the idle gaps between permutations.
                if (wots_abort) begin
                    core_clear      <= 1'b1;
                    cleanup_pending <= 1'b1;
                    cleanup_reason  <= CLEAN_WOTS;
                end else if (wots_release && !core_busy) begin
                    core_clear      <= 1'b1;
                    cleanup_pending <= 1'b1;
                    cleanup_reason  <= CLEAN_WOTS;
                end else if (core_done) begin
                    wots_perm_done <= 1'b1;
                end else if (wots_perm_req && !core_busy) begin
                    core_state_in   <= wots_state_in;
                    core_load_start <= 1'b1;
                end
            end else begin
                if (phase == PHASE_BUSY && owner != OWNER_NONE &&
                    operation_cycles == 6'd31) begin
                    core_clear      <= 1'b1;
                    cleanup_pending <= 1'b1;
                    cleanup_reason  <= CLEAN_FAILURE;
                end else if (operation == OP_NEXT_COPY) begin
                    for (i = 0; i < 64; i = i + 1)
                        output_window[i] <=
                            core_state_out[(squeeze_pos+i)*8 +: 8];
                    squeeze_pos      <= squeeze_pos + 9'd64;
                    phase            <= PHASE_DONE;
                    operation        <= OP_NONE;
                    operation_cycles <= 6'd0;
                end else if (core_done) begin
                    case (operation)
                        OP_AUTO: begin
                            phase      <= PHASE_IDLE;
                            operation  <= OP_NONE;
                        end

                        OP_FINAL: begin
                            for (i = 0; i < 64; i = i + 1) begin
                                if (mode == 2'd0 && i >= 32)
                                    output_window[i] <= 8'd0;
                                else
                                    output_window[i] <=
                                        core_state_out[i*8 +: 8];
                            end
                            din_ptr     <= 8'd0;
                            squeeze_pos <= (mode >= 2'd2) ? 9'd64 : 9'd0;
                            phase       <= PHASE_DONE;
                            operation   <= OP_NONE;
                        end

                        OP_NEXT_PERM: begin
                            for (i = 0; i < 64; i = i + 1) begin
                                if (i < crossing_count)
                                    output_window[i] <= crossing_tail[i];
                                else
                                    output_window[i] <=
                                        core_state_out[(i-crossing_count)*8 +: 8];
                            end
                            squeeze_pos <= 9'd64 - crossing_count;
                            phase       <= PHASE_DONE;
                            operation   <= OP_NONE;
                        end

                        OP_RAW: begin
                            phase     <= PHASE_DONE;
                            operation <= OP_NONE;
                        end

                        default: begin
                            // A completion without a matching front-end
                            // operation is an internal protocol failure.
                            core_clear      <= 1'b1;
                            cleanup_pending <= 1'b1;
                            cleanup_reason  <= CLEAN_FAILURE;
                        end
                    endcase
                    operation_cycles <= 6'd0;
                end else if (phase == PHASE_BUSY && owner != OWNER_NONE) begin
                    operation_cycles <= operation_cycles + 6'd1;
                end
            end

            // ------------------------------------------------------------
            // WOTS arbitration.  Claim has priority over a same-cycle MMIO
            // mutation; the MMIO request is still acknowledged and preserved.
            // ------------------------------------------------------------
            if (wots_claim_accept) begin
                wots_claim_seen <= 1'b1;
                wots_grant      <= 1'b1;
                owner           <= OWNER_WOTS;
                phase           <= PHASE_BUSY;
                error_code      <= ERROR_NONE;
                operation       <= OP_NONE;
            end

            // ------------------------------------------------------------
            // MMIO transaction.  Invalid whole accesses never ACK and never
            // mutate architectural device state.
            // ------------------------------------------------------------
            if (req && !req_seen && !hold_auto_din) begin
                req_seen <= 1'b1;
                if (access_valid) begin
                    ack <= 1'b1;

                    if (!wen) begin
                        // -------------------------- reads -----------------
                        if (addr == 7'h01) begin
                            rdata <= {60'd0, owner, phase};
                        end else if (addr == 7'h03) begin
                            rdata <= {56'd0, error_code};
                        end else if (addr == 7'h00 || addr == 7'h08) begin
                            rdata <= 64'd0;
                        end else if (addr == 7'h02) begin
                            rdata <= sha3_stream_en ? {62'd0, mode} : 64'd0;
                        end else if (addr >= 7'h10 && addr <= 7'h4f) begin
                            if (sha3_stream_en && !cleanup_pending &&
                                !wots_claim_accept &&
                                owner == OWNER_SPONGE &&
                                phase == PHASE_DONE) begin
                                if (size == BUS_DWORD) begin
                                    for (i = 0; i < 8; i = i + 1)
                                        rdata[i*8 +: 8] <=
                                            output_window[addr-7'h10+i];
                                end else begin
                                    rdata <= {56'd0,
                                        output_window[addr-7'h10]};
                                end
                            end else if ((!cleanup_pending &&
                                         owner != OWNER_WOTS &&
                                         !wots_claim_accept &&
                                         phase != PHASE_BUSY) &&
                                        sha3_stream_en) begin
                                error_code <= ERROR_CONFLICT;
                                phase      <= PHASE_ERROR;
                            end
                        end else if (addr == 7'h50) begin
                            if (!keccak_f1600_en) begin
                                rdata <= 64'd0;
                            end else if (!cleanup_pending &&
                                !wots_claim_accept &&
                                ((owner == OWNER_NONE && phase == PHASE_IDLE) ||
                                 (owner == OWNER_RAW &&
                                  (phase == PHASE_IDLE ||
                                   phase == PHASE_DONE)))) begin
                                rdata <= {59'd0, state_index};
                            end else if (!cleanup_pending &&
                                         owner != OWNER_WOTS &&
                                         !wots_claim_accept &&
                                         phase != PHASE_BUSY) begin
                                error_code <= ERROR_CONFLICT;
                                phase      <= PHASE_ERROR;
                            end
                        end else begin
                            // STATE_DATA byte or qword.
                            if (!keccak_f1600_en) begin
                                rdata <= 64'd0;
                            end else if (!cleanup_pending &&
                                !wots_claim_accept &&
                                owner == OWNER_RAW &&
                                (phase == PHASE_IDLE ||
                                 phase == PHASE_DONE)) begin
                                if (size == BUS_DWORD)
                                    rdata <= core_state_out[
                                                state_index*64 +: 64];
                                else
                                    rdata <= {56'd0,
                                      core_state_out[
                                        state_index*64 +
                                        (addr-7'h58)*8 +: 8]};
                            end else if (!cleanup_pending &&
                                         owner != OWNER_WOTS &&
                                         !wots_claim_accept &&
                                         phase != PHASE_BUSY) begin
                                error_code <= ERROR_CONFLICT;
                                phase      <= PHASE_ERROR;
                            end
                        end
                    end else if (cleanup_pending ||
                                 owner == OWNER_WOTS ||
                                 wots_claim_accept ||
                                 phase == PHASE_BUSY) begin
                        // Busy/WOTS preservation has first priority.  CLEAR is
                        // accepted during an MMIO-owned BUSY operation only.
                        if (addr == 7'h00 && wdata[7:0] == 8'd7 &&
                            owner != OWNER_WOTS && !wots_claim_accept) begin
                            core_clear      <= 1'b1;
                            cleanup_pending <= 1'b1;
                            cleanup_reason  <= CLEAN_MMIO;
                        end
                    end else begin
                        // -------------------------- writes ----------------
                        case (addr)
                            7'h00: begin
                                // Complete-byte command decode; no aliases.
                                case (wdata[7:0])
                                    8'd1: begin // INIT
                                        if (owner == OWNER_RAW) begin
                                            error_code <= ERROR_CONFLICT;
                                            phase      <= PHASE_ERROR;
                                        end else if (!sha3_stream_en) begin
                                            error_code <= ERROR_UNAVAILABLE;
                                            phase      <= PHASE_ERROR;
                                        end else if (owner == OWNER_NONE &&
                                                     phase == PHASE_IDLE) begin
                                            owner             <= OWNER_SPONGE;
                                            phase             <= PHASE_IDLE;
                                            error_code        <= ERROR_NONE;
                                            din_ptr           <= 8'd0;
                                            squeeze_pos       <= 9'd0;
                                            state_index       <= 5'd0;
                                            crossing_count    <= 7'd0;
                                            operation         <= OP_NONE;
                                            operation_cycles  <= 6'd0;
                                            core_clear        <= 1'b1;
                                            for (i = 0; i < 64;
                                                 i = i + 1) begin
                                                output_window[i] <= 8'd0;
                                                crossing_tail[i] <= 8'd0;
                                            end
                                        end else begin
                                            error_code <= ERROR_CONFLICT;
                                            phase      <= PHASE_ERROR;
                                        end
                                    end

                                    8'd3: begin // FINAL
                                        if (owner == OWNER_RAW) begin
                                            error_code <= ERROR_CONFLICT;
                                            phase      <= PHASE_ERROR;
                                        end else if (!sha3_stream_en) begin
                                            error_code <= ERROR_UNAVAILABLE;
                                            phase      <= PHASE_ERROR;
                                        end else if (owner == OWNER_SPONGE &&
                                                     phase == PHASE_IDLE) begin
                                            core_state_in     <= padded_state;
                                            core_load_start   <= 1'b1;
                                            phase             <= PHASE_BUSY;
                                            operation         <= OP_FINAL;
                                            operation_cycles  <= 6'd0;
                                        end else begin
                                            error_code <= ERROR_CONFLICT;
                                            phase      <= PHASE_ERROR;
                                        end
                                    end

                                    8'd4: begin // NEXT
                                        if (owner == OWNER_RAW) begin
                                            error_code <= ERROR_CONFLICT;
                                            phase      <= PHASE_ERROR;
                                        end else if (!sha3_stream_en) begin
                                            error_code <= ERROR_UNAVAILABLE;
                                            phase      <= PHASE_ERROR;
                                        end else if (owner == OWNER_SPONGE &&
                                                     phase == PHASE_DONE &&
                                                     mode < 2'd2) begin
                                            error_code <= ERROR_MODE;
                                            phase      <= PHASE_ERROR;
                                        end else if (owner == OWNER_SPONGE &&
                                                     phase == PHASE_DONE) begin
                                            phase            <= PHASE_BUSY;
                                            operation_cycles <= 6'd0;
                                            if (squeeze_pos + 9'd64 <=
                                                {1'b0, rate_bytes}) begin
                                                operation <= OP_NEXT_COPY;
                                            end else begin
                                                tail_count = rate_bytes -
                                                             squeeze_pos;
                                                crossing_count <= tail_count;
                                                for (i = 0; i < 64;
                                                     i = i + 1) begin
                                                    if (i < tail_count)
                                                        crossing_tail[i] <=
                                                          core_state_out[
                                                            (squeeze_pos+i)*8
                                                            +: 8];
                                                    else
                                                        crossing_tail[i] <=
                                                            8'd0;
                                                end
                                                core_start <= 1'b1;
                                                operation  <= OP_NEXT_PERM;
                                            end
                                        end else begin
                                            error_code <= ERROR_CONFLICT;
                                            phase      <= PHASE_ERROR;
                                        end
                                    end

                                    8'd6: begin // raw Keccak-f[1600]
                                        if (owner == OWNER_SPONGE) begin
                                            error_code <= ERROR_CONFLICT;
                                            phase      <= PHASE_ERROR;
                                        end else if (!keccak_f1600_en) begin
                                            error_code <= ERROR_UNAVAILABLE;
                                            phase      <= PHASE_ERROR;
                                        end else if (owner == OWNER_NONE &&
                                                     phase == PHASE_IDLE) begin
                                            owner             <= OWNER_RAW;
                                            phase             <= PHASE_BUSY;
                                            error_code        <= ERROR_NONE;
                                            operation         <= OP_RAW;
                                            operation_cycles  <= 6'd0;
                                            core_state_in     <= 1600'd0;
                                            core_load_start   <= 1'b1;
                                        end else if (owner == OWNER_RAW &&
                                            (phase == PHASE_IDLE ||
                                             phase == PHASE_DONE)) begin
                                            phase             <= PHASE_BUSY;
                                            error_code        <= ERROR_NONE;
                                            operation         <= OP_RAW;
                                            operation_cycles  <= 6'd0;
                                            core_start        <= 1'b1;
                                        end else begin
                                            error_code <= ERROR_CONFLICT;
                                            phase      <= PHASE_ERROR;
                                        end
                                    end

                                    8'd7: begin // CLEAR
                                        core_clear      <= 1'b1;
                                        cleanup_pending <= 1'b1;
                                        cleanup_reason  <= CLEAN_MMIO;
                                        operation       <= OP_NONE;
                                        if (owner != OWNER_NONE)
                                            phase <= PHASE_BUSY;
                                    end

                                    default: begin // 0, 2, 5, and 8..255
                                        error_code <= ERROR_COMMAND;
                                        phase      <= PHASE_ERROR;
                                    end
                                endcase
                            end

                            7'h02: begin // CTRL
                                if (owner == OWNER_RAW) begin
                                    error_code <= ERROR_CONFLICT;
                                    phase      <= PHASE_ERROR;
                                end else if (!sha3_stream_en) begin
                                    error_code <= ERROR_UNAVAILABLE;
                                    phase      <= PHASE_ERROR;
                                end else if (owner == OWNER_NONE &&
                                             phase == PHASE_IDLE) begin
                                    if (wdata[7:0] <= 8'd3) begin
                                        mode <= wdata[1:0];
                                    end else begin
                                        error_code <= ERROR_MODE;
                                        phase      <= PHASE_ERROR;
                                    end
                                end else begin
                                    error_code <= ERROR_CONFLICT;
                                    phase      <= PHASE_ERROR;
                                end
                            end

                            7'h08: begin // DIN
                                if (owner == OWNER_RAW) begin
                                    error_code <= ERROR_CONFLICT;
                                    phase      <= PHASE_ERROR;
                                end else if (!sha3_stream_en) begin
                                    error_code <= ERROR_UNAVAILABLE;
                                    phase      <= PHASE_ERROR;
                                end else if (owner == OWNER_SPONGE &&
                                             phase == PHASE_IDLE) begin
                                    if (din_ptr == rate_bytes - 8'd1) begin
                                        core_state_in     <= absorbed_state;
                                        core_load_start   <= 1'b1;
                                        din_ptr           <= 8'd0;
                                        phase             <= PHASE_BUSY;
                                        operation         <= OP_AUTO;
                                        operation_cycles  <= 6'd0;
                                    end else begin
                                        core_lane_index <= din_ptr[7:3];
                                        core_lane_wdata <=
                                          core_state_out[
                                            din_ptr[7:3]*64 +: 64] ^
                                          {8{wdata[7:0]}};
                                        core_lane_wstrb <=
                                          (8'b0000_0001 << din_ptr[2:0]);
                                        core_lane_we <= 1'b1;
                                        din_ptr <= din_ptr + 8'd1;
                                    end
                                end else begin
                                    error_code <= ERROR_CONFLICT;
                                    phase      <= PHASE_ERROR;
                                end
                            end

                            7'h50: begin // STATE_INDEX
                                if (owner == OWNER_SPONGE) begin
                                    error_code <= ERROR_CONFLICT;
                                    phase      <= PHASE_ERROR;
                                end else if (!keccak_f1600_en) begin
                                    error_code <= ERROR_UNAVAILABLE;
                                    phase      <= PHASE_ERROR;
                                end else if ((owner == OWNER_NONE &&
                                             phase == PHASE_IDLE) ||
                                            (owner == OWNER_RAW &&
                                             (phase == PHASE_IDLE ||
                                              phase == PHASE_DONE))) begin
                                    if (wdata[7:0] <= 8'd24)
                                        state_index <= wdata[4:0];
                                    else begin
                                        error_code <= ERROR_STATE_INDEX;
                                        phase      <= PHASE_ERROR;
                                    end
                                end else begin
                                    error_code <= ERROR_CONFLICT;
                                    phase      <= PHASE_ERROR;
                                end
                            end

                            default: begin // STATE_DATA byte or qword
                                if (owner == OWNER_SPONGE) begin
                                    error_code <= ERROR_CONFLICT;
                                    phase      <= PHASE_ERROR;
                                end else if (!keccak_f1600_en) begin
                                    error_code <= ERROR_UNAVAILABLE;
                                    phase      <= PHASE_ERROR;
                                end else if ((owner == OWNER_NONE &&
                                             phase == PHASE_IDLE) ||
                                            (owner == OWNER_RAW &&
                                             phase == PHASE_IDLE)) begin
                                    owner           <= OWNER_RAW;
                                    phase           <= PHASE_IDLE;
                                    error_code      <= ERROR_NONE;
                                    core_lane_index <= state_index;
                                    if (size == BUS_DWORD) begin
                                        core_lane_wdata <= wdata;
                                        core_lane_wstrb <= 8'hff;
                                    end else begin
                                        core_lane_wdata <= {8{wdata[7:0]}};
                                        core_lane_wstrb <=
                                          (8'b0000_0001 << addr[2:0]);
                                    end
                                    core_lane_we <= 1'b1;
                                end else begin
                                    error_code <= ERROR_CONFLICT;
                                    phase      <= PHASE_ERROR;
                                end
                            end
                        endcase
                    end
                end
            end
        end
    end

endmodule
