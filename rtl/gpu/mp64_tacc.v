// ============================================================================
// mp64_tacc.v — Persistent full-width tile accumulator state and lifecycle
// ============================================================================
//
// This leaf owns one physical tile engine's 2,048-bit TACC bank and all
// lifecycle metadata.  TACC_STATUS.MINE is deliberately not formed here:
// status_raw is the physical-engine view and always returns MINE=0.  A private
// full-core connection or a cluster CSR fanout compares OWNER with its reader
// and inserts the caller-relative MINE bit.
//
// Lifecycle requests use the architectural function values in req_funct.
// req_is_tamac distinguishes TMUL.TAMAC from extended-TSYS lifecycle
// functions.  req_canonical is the tile decoder's combined check of the EXT
// namespace, source selector, and complete function byte.
//
// Ordinary lifecycle validation faults complete in their acceptance cycle.
// TAMAC and image-operation validation faults publish one interval later;
// neither path raises physical BUSY.  A valid TRY, CLEAR, or RELEASE is
// admitted and exposes BUSY.  req_done is published during the following
// service interval; state commits on the edge where the receiver samples that
// response.  This keeps cancellation and architectural retirement on one
// boundary even when a cluster registers the leaf response.  req_valid may be
// a pulse, so integration must retain a request for which req_ready is low.
//
// LOAD and STORE use a tokened chip-wide staging interface below.  TAMAC
// arithmetic is performed by the parent tile datapath: tamac_start authorizes
// its first source request, and the complete result remains private there until
// this leaf reaches the architectural retirement edge.
//
// FORCE is an independent, level-valid, de-duplicated control transport.
// An accepted supervisor FORCE defeats idle admission in the same cycle.  If
// work is active, FORCE_PENDING remains set until success, fault, or caller
// cancellation reaches that work's terminal boundary, then the bank is wiped.
//

`include "mp64_pkg.vh"

module mp64_tacc #(
    parameter [4:0] CALLER_BASE = 5'd0,
    parameter integer CALLER_COUNT = 1
) (
    input  wire         clk,
    input  wire         rst_n,
    input  wire         engine_reset,

    // Lifecycle/arithmetic request.  req_funct uses ETSYS_TACC_* for
    // lifecycle requests and TMUL_TAMAC when req_is_tamac is asserted.
    input  wire         req_valid,
    output wire         req_ready,
    input  wire         req_is_tamac,
    input  wire [2:0]   req_funct,
    input  wire         req_canonical,
    input  wire [4:0]   req_caller_id,
    input  wire [1:0]   req_caller_slot,
    input  wire [2:0]   req_format_ew,
    input  wire         req_format_signed,
    input  wire [63:0]  req_image_addr,
    input  wire [2:0]   req_preflight_fault,
    input  wire [63:0]  req_preflight_fault_addr,
    input  wire         req_cancel,
    input  wire         req_retire,
    output reg          req_done,
    output wire         req_busy,
    output reg  [2:0]   req_fault,
    output reg  [63:0]  req_fault_addr,

    // TAMAC datapath handshake.  tamac_start is a combinational
    // admission pulse; tamac_done terminates the admitted operation after all
    // source reads and arithmetic slices.  The result image must remain stable
    // while req_done is held, through the req_retire sampling edge.
    output wire          tamac_start,
    input  wire          tamac_done,
    input  wire [2:0]    tamac_fault,
    input  wire [63:0]   tamac_fault_addr,
    input  wire [2047:0] tamac_result_image,

    // One chip-wide image stage arbitrates these level-held requests across
    // the seven physical engines.  The response remains held by that stage
    // until xfer_finish reaches the architectural retirement edge.
    output wire         xfer_req,
    output wire         xfer_store,
    output wire [63:0]  xfer_base,
    output wire [2:0]   xfer_format_ew,
    output wire [7:0]   xfer_token,
    output wire [2047:0] xfer_store_image,
    output wire         xfer_cancel,
    output wire         xfer_finish,
    input  wire         xfer_done,
    input  wire [7:0]   xfer_response_token,
    input  wire [2:0]   xfer_fault,
    input  wire [63:0]  xfer_fault_addr,
    input  wire [2047:0] xfer_load_image,

    // Independent TACC_CTL transport.  force_priv is 0 for supervisor and
    // 1 for user, matching the CPU privilege encoding.
    input  wire         force_valid,
    output wire         force_ready,
    input  wire         force_priv,
    input  wire [63:0]  force_wdata,
    input  wire [4:0]   force_caller_id,
    output reg          force_done,
    output reg  [2:0]   force_fault,

    // Physical-engine state.  status_raw[1] (MINE) is always zero.
    output wire [63:0]  status_raw,
    output wire [2047:0] bank_state
);

    localparam [5:0] CALLER_LIMIT =
        {1'b0, CALLER_BASE} + CALLER_COUNT;

    reg [2047:0] bank_reg;
    reg [4:0]    owner_reg;
    reg          valid_reg;
    reg          dirty_reg;
    reg [2:0]    format_ew_reg;
    reg          format_signed_reg;
    reg          force_pending_reg;

    reg          active_reg;
    reg          active_is_tamac_reg;
    reg [2:0]    active_funct_reg;
    reg [4:0]    active_caller_id_reg;
    reg [2:0]    active_format_ew_reg;
    reg          active_format_signed_reg;
    reg [63:0]   active_image_addr_reg;
    reg [7:0]    active_token_reg;
    reg [7:0]    operation_generation_reg;

    // TAMAC and image-operation validation faults have a locked two-cycle
    // base latency.  Keep their one deferred interval outside active_reg so
    // they never publish BUSY or acquire a datapath/transfer resource.
    reg          deferred_fault_reg;
    reg [2:0]    deferred_fault_code_reg;
    reg [63:0]   deferred_fault_addr_reg;

    // Held-valid de-duplication prevents a just-completed request from being
    // admitted again before its producer observes completion and drops valid.
    reg          req_seen;
    reg          force_seen;

    function format_is_legal;
        input [2:0] ew;
        begin
            case (ew)
                TMODE_8, TMODE_16, TMODE_32,
                TMODE_FP16, TMODE_BF16:
                    format_is_legal = 1'b1;
                default:
                    format_is_legal = 1'b0;
            endcase
        end
    endfunction

    function format_signed_is_known;
        input [2:0] ew;
        input       signed_mode;
        begin
            case (ew)
                TMODE_FP16, TMODE_BF16:
                    // Integer signedness is ignored for floating formats.
                    format_signed_is_known = 1'b1;
                TMODE_8, TMODE_16, TMODE_32:
                    case (signed_mode)
                        1'b0, 1'b1:
                            format_signed_is_known = 1'b1;
                        default:
                            format_signed_is_known = 1'b0;
                    endcase
                default:
                    format_signed_is_known = 1'b0;
            endcase
        end
    endfunction

    function normalized_signed;
        input [2:0] ew;
        input       signed_mode;
        begin
            case (ew)
                TMODE_FP16, TMODE_BF16:
                    normalized_signed = 1'b0;
                default:
                    normalized_signed = signed_mode;
            endcase
        end
    endfunction

    function tamac_format_is_legal;
        input [2:0] ew;
        begin
            case (ew)
                TMODE_8, TMODE_16, TMODE_32,
                TMODE_FP16, TMODE_BF16:
                    tamac_format_is_legal = 1'b1;
                default:
                    tamac_format_is_legal = 1'b0;
            endcase
        end
    endfunction

    function bit_is_zero;
        input value;
        begin
            case (value)
                1'b0:    bit_is_zero = 1'b1;
                default: bit_is_zero = 1'b0;
            endcase
        end
    endfunction

    function bit_is_one;
        input value;
        begin
            case (value)
                1'b1:    bit_is_one = 1'b1;
                default: bit_is_one = 1'b0;
            endcase
        end
    endfunction

    wire incoming_caller_allowed =
        (req_caller_id != TACC_OWNER_NONE) &&
        ({4'd0, req_caller_slot} < CALLER_COUNT) &&
        ({1'b0, req_caller_id} ==
         ({1'b0, CALLER_BASE} + {4'd0, req_caller_slot})) &&
        ({1'b0, req_caller_id} < CALLER_LIMIT);
    wire incoming_mine =
        owner_reg == req_caller_id;
    wire incoming_format_legal =
        format_is_legal(req_format_ew) &&
        format_signed_is_known(req_format_ew, req_format_signed);
    reg [2:0] incoming_fault;
    reg [63:0] incoming_fault_addr;
    always @(*) begin
        incoming_fault = MEX_FAULT_ILLEGAL;
        incoming_fault_addr = 64'd0;

        if (incoming_caller_allowed) begin
            // case/default makes an unknown canonicality or operation tag
            // fail closed in simulation instead of selecting a valid arm.
            case ({req_canonical, req_is_tamac})
                2'b10: begin
                    case (req_funct)
                        ETSYS_TACC_TRY:
                            incoming_fault = MEX_FAULT_NONE;

                        ETSYS_TACC_CLEAR:
                            if (incoming_mine && incoming_format_legal)
                                incoming_fault = MEX_FAULT_NONE;

                        ETSYS_TACC_LOAD:
                            if (incoming_mine && incoming_format_legal) begin
                                case (req_preflight_fault)
                                    MEX_FAULT_NONE:
                                        incoming_fault = MEX_FAULT_NONE;
                                    MEX_FAULT_ALIGN,
                                    MEX_FAULT_BUS,
                                    MEX_FAULT_PRIV: begin
                                        incoming_fault =
                                            req_preflight_fault;
                                        incoming_fault_addr =
                                            req_preflight_fault_addr;
                                    end
                                    default:
                                        incoming_fault = MEX_FAULT_ILLEGAL;
                                endcase
                            end

                        ETSYS_TACC_STORE:
                            if (incoming_mine && valid_reg &&
                                format_is_legal(format_ew_reg)) begin
                                case (req_preflight_fault)
                                    MEX_FAULT_NONE:
                                        incoming_fault = MEX_FAULT_NONE;
                                    MEX_FAULT_ALIGN,
                                    MEX_FAULT_BUS,
                                    MEX_FAULT_PRIV: begin
                                        incoming_fault =
                                            req_preflight_fault;
                                        incoming_fault_addr =
                                            req_preflight_fault_addr;
                                    end
                                    default:
                                        incoming_fault = MEX_FAULT_ILLEGAL;
                                endcase
                            end

                        ETSYS_TACC_RELEASE:
                            if (incoming_mine)
                                incoming_fault = MEX_FAULT_NONE;

                        default:
                            incoming_fault = MEX_FAULT_ILLEGAL;
                    endcase
                end

                2'b11: begin
                    if ((req_funct == TMUL_TAMAC) &&
                        incoming_mine && valid_reg &&
                        incoming_format_legal &&
                        tamac_format_is_legal(req_format_ew) &&
                        (format_ew_reg == req_format_ew) &&
                        (format_signed_reg ==
                         normalized_signed(req_format_ew,
                                           req_format_signed))) begin
                        case (req_preflight_fault)
                            MEX_FAULT_NONE:
                                incoming_fault = MEX_FAULT_NONE;
                            MEX_FAULT_ALIGN,
                            MEX_FAULT_BUS,
                            MEX_FAULT_PRIV: begin
                                incoming_fault =
                                    req_preflight_fault;
                                incoming_fault_addr =
                                    req_preflight_fault_addr;
                            end
                            default:
                                incoming_fault = MEX_FAULT_ILLEGAL;
                        endcase
                    end
                end

                default:
                    incoming_fault = MEX_FAULT_ILLEGAL;
            endcase
        end
    end

    reg       force_action;
    reg [2:0] force_decoded_fault;
    wire force_caller_allowed =
        (force_caller_id != TACC_OWNER_NONE) &&
        ({1'b0, force_caller_id} >= {1'b0, CALLER_BASE}) &&
        ({1'b0, force_caller_id} < CALLER_LIMIT);
    always @(*) begin
        force_action        = 1'b0;
        force_decoded_fault = MEX_FAULT_NONE;
        case (force_caller_allowed)
            1'b1: begin
                case ({force_priv, force_wdata[0]})
                    2'b00, 2'b10: begin
                        // Bit zero is clear. Reserved write bits are ignored
                        // in either privilege mode.
                        force_action        = 1'b0;
                        force_decoded_fault = MEX_FAULT_NONE;
                    end
                    2'b01: begin
                        force_action        = 1'b1;
                        force_decoded_fault = MEX_FAULT_NONE;
                    end
                    2'b11: begin
                        force_action        = 1'b0;
                        force_decoded_fault = MEX_FAULT_PRIV;
                    end
                    default: begin
                        force_action        = 1'b0;
                        force_decoded_fault = MEX_FAULT_PRIV;
                    end
                endcase
            end
            default: begin
                // A malformed or unknown caller identity must never be able
                // to authorize destructive recovery.
                force_action        = 1'b0;
                force_decoded_fault = MEX_FAULT_ILLEGAL;
            end
        endcase
    end

    wire force_new =
        rst_n && !engine_reset && force_valid && !force_seen;
    wire force_authorized_new = force_new && force_action;
    wire request_not_cancelled = bit_is_zero(req_cancel);
    wire response_retired = bit_is_one(req_retire);
    wire incoming_fault_is_deferred =
        req_is_tamac ||
        (!req_is_tamac &&
         ((req_funct == ETSYS_TACC_LOAD) ||
          (req_funct == ETSYS_TACC_STORE)));
    // The requester may treat a published response as no longer busy, while
    // physical BUSY remains asserted through the response-sampling edge.
    // That prevents sibling status readers from observing an idle gap before
    // the architectural mutation commits.
    assign req_busy = active_reg && !req_done;
    assign force_ready = rst_n && !engine_reset && !force_seen;
    assign req_ready =
        rst_n && !engine_reset &&
        !active_reg && !deferred_fault_reg &&
        !req_done && !force_pending_reg && !req_seen &&
        !force_authorized_new;
    assign tamac_start =
        req_valid && req_ready && req_is_tamac &&
        request_not_cancelled &&
        (incoming_fault == MEX_FAULT_NONE);

    wire active_is_transfer =
        !active_is_tamac_reg &&
        ((active_funct_reg == ETSYS_TACC_LOAD) ||
         (active_funct_reg == ETSYS_TACC_STORE));
    assign xfer_req =
        active_reg && active_is_transfer && !req_done &&
        request_not_cancelled;
    assign xfer_store =
        active_funct_reg == ETSYS_TACC_STORE;
    assign xfer_base = active_image_addr_reg;
    assign xfer_format_ew = active_format_ew_reg;
    assign xfer_token = active_token_reg;
    assign xfer_store_image = bank_reg;
    assign xfer_cancel =
        active_reg && active_is_transfer &&
        (!request_not_cancelled || engine_reset);
    assign xfer_finish =
        active_reg && active_is_transfer && req_done &&
        request_not_cancelled && response_retired;

    assign bank_state = bank_reg;
    assign status_raw = {
        43'd0,
        owner_reg,
        6'd0,
        force_pending_reg,
        (valid_reg ? format_signed_reg : 1'b0),
        (valid_reg ? format_ew_reg : 3'd0),
        active_reg,
        dirty_reg,
        valid_reg,
        1'b0,
        owner_reg != TACC_OWNER_NONE
    };

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            bank_reg                    <= 2048'd0;
            owner_reg                   <= TACC_OWNER_NONE;
            valid_reg                   <= 1'b0;
            dirty_reg                   <= 1'b0;
            format_ew_reg               <= 3'd0;
            format_signed_reg           <= 1'b0;
            force_pending_reg           <= 1'b0;
            active_reg                  <= 1'b0;
            active_is_tamac_reg         <= 1'b0;
            active_funct_reg            <= 3'd0;
            active_caller_id_reg        <= TACC_OWNER_NONE;
            active_format_ew_reg        <= 3'd0;
            active_format_signed_reg    <= 1'b0;
            active_image_addr_reg       <= 64'd0;
            active_token_reg            <= 8'd0;
            operation_generation_reg    <= 8'd0;
            deferred_fault_reg          <= 1'b0;
            deferred_fault_code_reg     <= MEX_FAULT_NONE;
            deferred_fault_addr_reg     <= 64'd0;
            req_seen                    <= 1'b0;
            force_seen                  <= 1'b0;
            req_done                    <= 1'b0;
            req_fault                   <= MEX_FAULT_NONE;
            req_fault_addr              <= 64'd0;
            force_done                  <= 1'b0;
            force_fault                 <= MEX_FAULT_NONE;
        end else if (engine_reset) begin
            bank_reg                    <= 2048'd0;
            owner_reg                   <= TACC_OWNER_NONE;
            valid_reg                   <= 1'b0;
            dirty_reg                   <= 1'b0;
            format_ew_reg               <= 3'd0;
            format_signed_reg           <= 1'b0;
            force_pending_reg           <= 1'b0;
            active_reg                  <= 1'b0;
            active_is_tamac_reg         <= 1'b0;
            active_funct_reg            <= 3'd0;
            active_caller_id_reg        <= TACC_OWNER_NONE;
            active_format_ew_reg        <= 3'd0;
            active_format_signed_reg    <= 1'b0;
            active_image_addr_reg       <= 64'd0;
            active_token_reg            <= 8'd0;
            operation_generation_reg    <= operation_generation_reg + 8'd1;
            deferred_fault_reg          <= 1'b0;
            deferred_fault_code_reg     <= MEX_FAULT_NONE;
            deferred_fault_addr_reg     <= 64'd0;
            // A request held across reset belongs to the canceled execution
            // context.  Mark it seen until its source drops valid.
            req_seen                    <= req_valid;
            force_seen                  <= force_valid;
            req_done                    <= 1'b0;
            req_fault                   <= MEX_FAULT_NONE;
            req_fault_addr              <= 64'd0;
            force_done                  <= 1'b0;
            force_fault                 <= MEX_FAULT_NONE;
        end else begin
            force_done  <= 1'b0;
            force_fault <= MEX_FAULT_NONE;

            if (!req_valid)
                req_seen <= 1'b0;
            if (!force_valid)
                force_seen <= 1'b0;

            // The control transport acknowledges exactly one response per
            // asserted-valid interval, independently of the MEX lifecycle
            // transport.
            if (force_new) begin
                force_seen  <= 1'b1;
                force_done  <= 1'b1;
                force_fault <= force_decoded_fault;

                if (force_action) begin
                    if (active_reg) begin
                        force_pending_reg <= 1'b1;
                    end else begin
                        bank_reg          <= 2048'd0;
                        owner_reg         <= TACC_OWNER_NONE;
                        valid_reg         <= 1'b0;
                        dirty_reg         <= 1'b0;
                        format_ew_reg     <= 3'd0;
                        format_signed_reg <= 1'b0;
                        force_pending_reg <= 1'b0;
                        operation_generation_reg <=
                            operation_generation_reg + 8'd1;
                    end
                end
            end

            if (req_done) begin
                // Hold the response and any staged mutation until the receiver
                // reaches its architectural retirement edge. Cancellation
                // wins if both controls are malformed or asserted together.
                if (!request_not_cancelled) begin
                    req_done  <= 1'b0;
                    req_fault <= MEX_FAULT_NONE;
                    req_fault_addr <= 64'd0;
                    if (active_reg) begin
                        active_reg <= 1'b0;
                        active_is_tamac_reg <= 1'b0;
                        if (force_pending_reg || force_authorized_new) begin
                            bank_reg          <= 2048'd0;
                            owner_reg         <= TACC_OWNER_NONE;
                            valid_reg         <= 1'b0;
                            dirty_reg         <= 1'b0;
                            format_ew_reg     <= 3'd0;
                            format_signed_reg <= 1'b0;
                            force_pending_reg <= 1'b0;
                            operation_generation_reg <=
                                operation_generation_reg + 8'd1;
                        end
                    end
                end else if (response_retired) begin
                    req_done  <= 1'b0;
                    req_fault <= MEX_FAULT_NONE;
                    req_fault_addr <= 64'd0;
                    if (active_reg) begin
                        active_reg <= 1'b0;
                        active_is_tamac_reg <= 1'b0;

                        if (force_pending_reg || force_authorized_new) begin
                            // FORCE is independently acknowledged and survives
                            // cancellation or retirement of the instruction.
                            bank_reg          <= 2048'd0;
                            owner_reg         <= TACC_OWNER_NONE;
                            valid_reg         <= 1'b0;
                            dirty_reg         <= 1'b0;
                            format_ew_reg     <= 3'd0;
                            format_signed_reg <= 1'b0;
                            force_pending_reg <= 1'b0;
                            operation_generation_reg <=
                                operation_generation_reg + 8'd1;
                        end else if (req_fault == MEX_FAULT_NONE &&
                                     active_is_tamac_reg) begin
                            bank_reg  <= tamac_result_image;
                            dirty_reg <= 1'b1;
                        end else if (req_fault == MEX_FAULT_NONE) begin
                            case (active_funct_reg)
                                ETSYS_TACC_TRY: begin
                                    if (owner_reg == TACC_OWNER_NONE)
                                        owner_reg <= active_caller_id_reg;
                                    // Same-owner TRY is idempotent; another
                                    // owner's TRY retires without mutation.
                                end

                                ETSYS_TACC_CLEAR: begin
                                    bank_reg          <= 2048'd0;
                                    valid_reg         <= 1'b1;
                                    dirty_reg         <= 1'b1;
                                    format_ew_reg     <= active_format_ew_reg;
                                    format_signed_reg <=
                                        active_format_signed_reg;
                                end

                                ETSYS_TACC_LOAD: begin
                                    bank_reg          <= xfer_load_image;
                                    valid_reg         <= 1'b1;
                                    dirty_reg         <= 1'b0;
                                    format_ew_reg     <= active_format_ew_reg;
                                    format_signed_reg <=
                                        active_format_signed_reg;
                                end

                                ETSYS_TACC_STORE: begin
                                    // The canonical image is already visible
                                    // in memory. Architectural state changes
                                    // only by clearing DIRTY at retirement.
                                    dirty_reg <= 1'b0;
                                end

                                ETSYS_TACC_RELEASE: begin
                                    bank_reg          <= 2048'd0;
                                    owner_reg         <= TACC_OWNER_NONE;
                                    valid_reg         <= 1'b0;
                                    dirty_reg         <= 1'b0;
                                    format_ew_reg     <= 3'd0;
                                    format_signed_reg <= 1'b0;
                                    operation_generation_reg <=
                                        operation_generation_reg + 8'd1;
                                end

                                default: begin
                                    // req_fault rejected this corrupt tag when
                                    // the response was first published.
                                end
                            endcase
                        end
                    end
                end
            end else if (deferred_fault_reg) begin
                // A canceled execution context receives no late fault.  Normal
                // completion appears after exactly one non-BUSY defer interval
                // and then follows the ordinary retirement handshake.
                deferred_fault_reg      <= 1'b0;
                deferred_fault_code_reg <= MEX_FAULT_NONE;
                deferred_fault_addr_reg <= 64'd0;
                if (!request_not_cancelled) begin
                    req_done       <= 1'b0;
                    req_fault      <= MEX_FAULT_NONE;
                    req_fault_addr <= 64'd0;
                end else begin
                    req_done       <= 1'b1;
                    req_fault      <= deferred_fault_code_reg;
                    req_fault_addr <= deferred_fault_addr_reg;
                end
            end else if (active_reg) begin
                if (!request_not_cancelled) begin
                    // Individual caller cancellation is non-retiring and
                    // preserves shared state.  A previously accepted (or
                    // same-cycle accepted) FORCE still owns the terminal
                    // boundary and therefore wipes after the cancellation.
                    active_reg <= 1'b0;
                    active_is_tamac_reg <= 1'b0;
                    if (force_pending_reg || force_authorized_new) begin
                        bank_reg          <= 2048'd0;
                        owner_reg         <= TACC_OWNER_NONE;
                        valid_reg         <= 1'b0;
                        dirty_reg         <= 1'b0;
                        format_ew_reg     <= 3'd0;
                        format_signed_reg <= 1'b0;
                        force_pending_reg <= 1'b0;
                        operation_generation_reg <=
                            operation_generation_reg + 8'd1;
                    end
                end else begin
                    if (active_is_tamac_reg) begin
                        if (tamac_done) begin
                            // The parent holds its completed scratch image
                            // stable until retirement; no second 2,048-bit
                            // accumulator bank is needed here.
                            req_done       <= 1'b1;
                            req_fault      <= tamac_fault;
                            req_fault_addr <= tamac_fault_addr;
                        end
                    end else begin
                        // Publish the response one interval before its
                        // sampling edge so the outer cluster can suppress a
                        // coincident caller cancellation without rollback.
                        req_done  <= 1'b1;
                        req_fault <= MEX_FAULT_NONE;
                        req_fault_addr <= 64'd0;

                        case (active_funct_reg)
                            ETSYS_TACC_TRY,
                            ETSYS_TACC_CLEAR,
                            ETSYS_TACC_RELEASE: begin
                            end
                            ETSYS_TACC_LOAD,
                            ETSYS_TACC_STORE: begin
                                if (xfer_done &&
                                    xfer_response_token ==
                                        active_token_reg) begin
                                    req_fault      <= xfer_fault;
                                    req_fault_addr <= xfer_fault_addr;
                                end else begin
                                    req_done <= 1'b0;
                                end
                            end
                            default:
                                req_fault <= MEX_FAULT_ILLEGAL;
                        endcase
                    end
                end
            end else if (req_valid && req_ready &&
                         request_not_cancelled) begin
                req_seen <= 1'b1;

                if (incoming_fault != MEX_FAULT_NONE) begin
                    // TAMAC and image operations retain their locked second
                    // validation cycle without entering BUSY. Other lifecycle
                    // validation faults complete on this acceptance edge.
                    if (incoming_fault_is_deferred) begin
                        deferred_fault_reg      <= 1'b1;
                        deferred_fault_code_reg <= incoming_fault;
                        deferred_fault_addr_reg <= incoming_fault_addr;
                        req_done                <= 1'b0;
                        req_fault               <= MEX_FAULT_NONE;
                        req_fault_addr          <= 64'd0;
                    end else begin
                        req_done       <= 1'b1;
                        req_fault      <= incoming_fault;
                        req_fault_addr <= incoming_fault_addr;
                    end
                end else begin
                    active_reg               <= 1'b1;
                    active_is_tamac_reg      <= req_is_tamac;
                    active_funct_reg         <= req_funct;
                    active_caller_id_reg     <= req_caller_id;
                    if (!req_is_tamac &&
                        req_funct == ETSYS_TACC_STORE) begin
                        active_format_ew_reg     <= format_ew_reg;
                        active_format_signed_reg <= format_signed_reg;
                    end else begin
                        active_format_ew_reg     <= req_format_ew;
                        active_format_signed_reg <=
                            normalized_signed(req_format_ew,
                                              req_format_signed);
                    end
                    active_image_addr_reg <= req_image_addr;
                    active_token_reg      <= operation_generation_reg;
                    // Every admitted transfer consumes a generation even
                    // when ownership and format remain unchanged.  The
                    // shared stage drains or retires one tenure completely
                    // before another can be admitted, so an eight-bit wrap
                    // cannot alias a still-live response.
                    if (!req_is_tamac &&
                        ((req_funct == ETSYS_TACC_LOAD) ||
                         (req_funct == ETSYS_TACC_STORE)))
                        operation_generation_reg <=
                            operation_generation_reg + 8'd1;
                end
            end
        end
    end

`ifndef SYNTHESIS
    initial begin
        if (CALLER_COUNT < 1 || CALLER_COUNT > 4)
            $error("mp64_tacc: CALLER_COUNT must be in 1..4");
        if (({1'b0, CALLER_BASE} + CALLER_COUNT) > 6'd31)
            $error("mp64_tacc: caller range must not include OWNER_NONE");
    end

    // Architectural metadata is intentionally redundant enough to make
    // integration corruption fail loudly during focused and SoC simulation.
    always @(posedge clk) begin
        if (rst_n && !engine_reset) begin
            if (active_reg) begin
                if (active_is_tamac_reg) begin
                    if (active_funct_reg != TMUL_TAMAC)
                        $error("mp64_tacc: corrupt TAMAC function tag");
                    if (owner_reg != active_caller_id_reg || !valid_reg)
                        $error("mp64_tacc: unowned or invalid TAMAC active");
                    if (!tamac_format_is_legal(active_format_ew_reg))
                        $error("mp64_tacc: illegal TAMAC format became active");
                    if (format_ew_reg != active_format_ew_reg ||
                        format_signed_reg != active_format_signed_reg)
                        $error("mp64_tacc: mismatched TAMAC format active");
                end else case (active_funct_reg)
                    ETSYS_TACC_TRY: begin
                        if (active_caller_id_reg == TACC_OWNER_NONE ||
                            {1'b0, active_caller_id_reg} <
                                {1'b0, CALLER_BASE} ||
                            {1'b0, active_caller_id_reg} >= CALLER_LIMIT)
                            $error("mp64_tacc: TRY retained invalid caller");
                    end
                    ETSYS_TACC_CLEAR: begin
                        if (owner_reg != active_caller_id_reg)
                            $error("mp64_tacc: nonowner CLEAR became active");
                        if (!format_is_legal(active_format_ew_reg))
                            $error("mp64_tacc: illegal CLEAR format active");
                    end
                    ETSYS_TACC_LOAD: begin
                        if (owner_reg != active_caller_id_reg)
                            $error("mp64_tacc: nonowner LOAD became active");
                        if (!format_is_legal(active_format_ew_reg))
                            $error("mp64_tacc: illegal LOAD format active");
                    end
                    ETSYS_TACC_STORE: begin
                        if (owner_reg != active_caller_id_reg)
                            $error("mp64_tacc: nonowner STORE became active");
                        if (!valid_reg)
                            $error("mp64_tacc: STORE active without valid state");
                        if (!format_is_legal(active_format_ew_reg))
                            $error("mp64_tacc: illegal STORE format active");
                    end
                    ETSYS_TACC_RELEASE:
                        if (owner_reg != active_caller_id_reg)
                            $error("mp64_tacc: nonowner RELEASE became active");
                    default:
                        $error("mp64_tacc: illegal lifecycle tag active");
                endcase
                if (tamac_done && !active_is_tamac_reg)
                    $error("mp64_tacc: TAMAC terminal without TAMAC active");
            end
            if (dirty_reg && !valid_reg)
                $error("mp64_tacc: DIRTY set while state is invalid");
            if (deferred_fault_reg && active_reg)
                $error("mp64_tacc: deferred fault overlaps active operation");
            if (deferred_fault_reg &&
                deferred_fault_code_reg == MEX_FAULT_NONE)
                $error("mp64_tacc: deferred terminal has no fault");
            if (valid_reg && owner_reg == TACC_OWNER_NONE)
                $error("mp64_tacc: valid state has no owner");
            if (!valid_reg &&
                (format_ew_reg != 3'd0 || format_signed_reg != 1'b0))
                $error("mp64_tacc: invalid state retained format metadata");
            if (!valid_reg && bank_reg != 2048'd0)
                $error("mp64_tacc: invalid state retained bank data");
            if (valid_reg && !format_is_legal(format_ew_reg))
                $error("mp64_tacc: valid state retained illegal format");
            if (force_pending_reg && !active_reg)
                $error("mp64_tacc: FORCE_PENDING without active operation");
            if ((req_fault != MEX_FAULT_NONE) && !req_done)
                $error("mp64_tacc: request fault without completion");
            if ((force_fault != MEX_FAULT_NONE) && !force_done)
                $error("mp64_tacc: control fault without acknowledgement");
            if (status_raw[TACC_STATUS_BIT_MINE])
                $error("mp64_tacc: physical raw status exposed MINE");
        end
    end
`endif

endmodule
