// ============================================================================
// mp64_chiplet_port.v — Chiplet port bridge (MegaPad's side of one link)
// ============================================================================
//
// One instance serves one chiplet port.  It is the single check between a
// chiplet and MegaPad's memory:
//
//   - The chiplet asks for memory accesses over the link's request channel.
//     The bridge admits an access only when it lies wholly inside external
//     memory and inside a range lent to this port with the right for it.
//     An admitted access goes to the external-memory client arbiter; a
//     refused one is answered REFUSED, recorded, and reported.
//   - Doorbells cross in both directions between the chiplet and the mailbox.
//   - The sideband holds the chiplet in reset, asks it to stop and flush,
//     reads its identity record, and names the image it starts from.
//   - MegaPad's writes into memory the chiplet lends leave through it: the
//     TACC image stage hands it one write of 64-byte units, which it sends
//     across the link and answers once the chiplet has answered.  The
//     chiplet checks those writes against the ranges it lends; the bridge
//     refuses one itself only while BORROW_EN is clear or the link is down.
//
// Link channels use valid/ready handshakes, so any link layer that delivers
// each message once and in order can carry them.  One request is
// outstanding at a time in each direction.
//
// Request channel (chiplet -> MegaPad):  addr, wdata, wen, size, len.
//   Sizes are the bus sizes (byte, half, word, dword), naturally aligned.
//   Sub-word write data sits in the low bits.  A read returns the aligned
//   64-bit word that contains the address.
//   len is the number of 64-bit words beyond the first: 0 for one access,
//   up to 255 for a burst of consecutive words.  A read burst is one
//   request and len + 1 responses.  A write burst is len + 1 request
//   messages, the first with addr and len and each with its word of data,
//   and one response.  The whole burst is admitted or refused at its start,
//   and each word is its own access at external memory.
// Response channel (MegaPad -> chiplet): rdata, code, last.
//   last marks the final response of a request.  A response other than
//   LK_OK ends a burst and carries last.
//   LK_OK         the access completed
//   LK_REFUSED    outside the lent ranges, misaligned, or the port is closed
//   LK_MEM_ERROR  external memory timed out or reported an error
//   LK_CANCELLED  the chiplet cancelled; a write may or may not have landed
// Cancel: lk_rq_cancel abandons the outstanding request.  The bridge still
//   answers it, with LK_CANCELLED unless its response was already waiting.
//
// Write channel (MegaPad -> chiplet): addr, data, len, zeros.
//   One write covers len + 1 units of 64 bytes from addr, the chiplet's own
//   address, then zeros units that the chiplet fills with zeros.  It is
//   len + 1 messages, each carrying its unit; addr, len and zeros ride on
//   every message, and the chiplet takes them from the first.  The stage
//   names the write by MegaPad's address in the port's window, and the
//   bridge sends that address less WINDOW_BASE.
// Answer channel (chiplet -> MegaPad): code, one of the codes above.
//   A running chiplet answers each write once, OK once every byte it covers
//   can be read by its own accesses.  An answer before the last unit ends
//   the write.  lk_wq_cancel abandons the outstanding write, which the
//   chiplet still answers.  Each phase, a unit waiting to be taken or the
//   answer waiting to come, is bounded to 255 cycles.  On expiry the bridge
//   fails the write to the stage, holds lk_wq_cancel until the answer comes
//   or the link goes down, and refuses further writes meanwhile.
//
// Register block (64-bit registers, offsets within the port's block):
//   0x00 CTRL        [0] ENABLE      admit memory requests
//                    [1] RESET_HOLD  hold the chiplet in reset (reset: 1)
//                    [2] STOP_REQ    ask the chiplet to stop and flush
//                    [3] DOORBELL_EN accept doorbells from the chiplet
//                    [4] ATTN_EN     ring ATTN_CORE when a request is refused
//                    [5] BORROW_EN   send MegaPad's writes to the chiplet
//                    [15:8] ATTN_CORE
//   0x08 STATUS      [0] PRESENT [1] STOPPED [2] READY [3] BUSY
//                    [4] FAULT (write 1 to clear, with the refusal record)
//                    [5] ID_DONE
//                    [6] BORROW_BUSY one of MegaPad's writes is outstanding,
//                        a cancelled one awaiting its answer included
//                    [23:16] this port's ID   [39:32] ranges per port
//   0x10 FAULT_ADDR  address of the first refused request since the clear
//   0x18 FAULT_INFO  [1:0] size [2] wen [7:4] reason [15:8] len
//                    [31:16] refusals
//   0x20 RANGE_SEL   range addressed by the three registers below
//   0x28 RANGE_BASE  staged base (write); installed base (read)
//   0x30 RANGE_LIMIT staged limit, exclusive (write); installed limit (read)
//   0x38 RANGE_RIGHTS [0] read [1] write.  A write installs the staged base
//                    and limit with these rights in one step.
//   0x40 QOS         [7:0] weight  [31:16] accesses per epoch (0 = no limit)
//   0x48 ID_INDEX    write starts a sideband read of that identity word
//   0x50 ID_DATA     the word read
//   0x58 BOOT_ADDR   address of the image the chiplet starts from
//   0x60 BOOT_LEN    its length in bytes
//   0x68 BOOT_ARG    a word given to the chiplet with them: the address of
//                    its control block
//   0x70 WINDOW      read-only: the address at which the chiplet's address 0
//                    appears in MegaPad's address space
//
// Coding standard: Verilog-2001, sync reset, non-blocking assigns.
//

`include "mp64_pkg.vh"

module mp64_chiplet_port #(
    parameter        RANGES     = 8,      // lent ranges this port holds
    parameter        RANGE_BITS = 3,      // ceil(log2(RANGES)), at least 1
    parameter [7:0]  PORT_ID    = 8'd16,  // this port's ID
    parameter [63:0] EXT_BASE   = 64'h0000_0000_0010_0000,
    parameter [63:0] EXT_LIMIT  = 64'h0000_0000_FF00_0000,
    // Base of this port's window in MegaPad's address space.
    parameter [63:0] WINDOW_BASE = 64'h0100_0000_0000_0000
)(
    input  wire        clk,
    input  wire        rst_n,

    // === Register access (selected port's block) ===
    input  wire        reg_sel,
    input  wire        reg_wen,
    input  wire [3:0]  reg_idx,        // register index: offset / 8
    input  wire [63:0] reg_wdata,
    output reg  [63:0] reg_rdata,

    // === Link: memory request channel (chiplet -> MegaPad) ===
    input  wire        lk_rq_valid,
    output wire        lk_rq_ready,
    input  wire [63:0] lk_rq_addr,
    input  wire [63:0] lk_rq_wdata,
    input  wire        lk_rq_wen,
    input  wire [1:0]  lk_rq_size,
    input  wire [7:0]  lk_rq_len,      // words beyond the first
    input  wire        lk_rq_cancel,

    // === Link: memory response channel (MegaPad -> chiplet) ===
    output wire        lk_rs_valid,
    input  wire        lk_rs_ready,
    output reg  [63:0] lk_rs_rdata,
    output reg  [1:0]  lk_rs_code,
    output reg         lk_rs_last,     // final response of the request

    // === Link: doorbell to the chiplet ===
    output wire        lk_dbt_valid,
    input  wire        lk_dbt_ready,
    output reg  [7:0]  lk_dbt_src,

    // === Link: doorbell from the chiplet ===
    input  wire        lk_dbr_valid,
    output wire        lk_dbr_ready,
    input  wire [7:0]  lk_dbr_dst,

    // === Link: MegaPad's write (MegaPad -> chiplet) ===
    output wire         lk_wq_valid,
    input  wire         lk_wq_ready,
    output wire [63:0]  lk_wq_addr,
    output wire [511:0] lk_wq_data,
    output wire [7:0]   lk_wq_len,     // units beyond the first
    output wire [7:0]   lk_wq_zeros,   // units of zeros after the data
    output wire         lk_wq_cancel,

    // === Link: its answer (chiplet -> MegaPad) ===
    input  wire         lk_wa_valid,
    output wire         lk_wa_ready,
    input  wire [1:0]   lk_wa_code,

    // === MegaPad's writer, the TACC image stage ===
    // bw_valid and the current unit hold until bw_ready; addr, len and zeros
    // hold for the whole write.  The stage drops bw_valid once its last unit
    // is taken or the write ends.  bw_done and bw_cancel_done are pulses.
    input  wire         bw_valid,
    output wire         bw_ready,
    input  wire [63:0]  bw_addr,       // MegaPad's address, in this window
    input  wire [511:0] bw_data,
    input  wire [7:0]   bw_len,
    input  wire [7:0]   bw_zeros,
    input  wire         bw_cancel,     // level: abandon the outstanding write
    output reg          bw_done,       // the write ended
    output reg          bw_error,      // with bw_done: it failed
    output reg          bw_cancel_done,

    // === Sideband ===
    input  wire        sb_present,     // a chiplet is attached
    output wire        sb_rst_n,       // low holds the chiplet in reset
    output wire        sb_stop_req,
    input  wire        sb_stopped,
    input  wire        sb_ready,       // the chiplet's runtime is up
    output reg         sb_id_req,
    output reg  [7:0]  sb_id_index,
    input  wire        sb_id_ack,
    input  wire [63:0] sb_id_data,
    output wire [63:0] sb_boot_addr,
    output wire [63:0] sb_boot_len,
    output wire [63:0] sb_boot_arg,

    // === External-memory client port (to mp64_extmem_arb) ===
    output wire        m_req,
    output wire [63:0] m_addr,
    output wire [63:0] m_wdata,
    output wire        m_wen,
    output wire [1:0]  m_size,
    input  wire        m_ack,
    input  wire        m_error,
    input  wire [63:0] m_rdata,
    output wire [7:0]  m_weight,
    output wire [15:0] m_bwlimit,

    // === Mailbox ===
    input  wire        mb_db_valid,    // a doorbell is pending for this port
    input  wire [7:0]  mb_db_src,      // its lowest pending source, as an ID
    output wire        mb_take_valid,  // delivered: clear that pending bit
    output wire [7:0]  mb_take_src,
    output wire        mb_send_valid,  // ring a participant as this port
    output wire [7:0]  mb_send_dst
);

    // Response codes on the link.
    localparam [1:0] LK_OK        = 2'd0;
    localparam [1:0] LK_REFUSED   = 2'd1;
    localparam [1:0] LK_MEM_ERROR = 2'd2;
    localparam [1:0] LK_CANCELLED = 2'd3;

    // Refusal reasons in FAULT_INFO.
    localparam [3:0] WHY_CLOSED     = 4'd1;  // ENABLE is clear
    localparam [3:0] WHY_MISALIGNED = 4'd2;
    localparam [3:0] WHY_NOT_EXT    = 4'd3;  // outside external memory
    localparam [3:0] WHY_NOT_LENT   = 4'd4;  // no lent range grants it

    // Register indices (offset / 8).
    localparam [3:0] R_CTRL       = 4'h0;
    localparam [3:0] R_STATUS     = 4'h1;
    localparam [3:0] R_FAULT_ADDR = 4'h2;
    localparam [3:0] R_FAULT_INFO = 4'h3;
    localparam [3:0] R_RANGE_SEL  = 4'h4;
    localparam [3:0] R_RANGE_BASE = 4'h5;
    localparam [3:0] R_RANGE_LIM  = 4'h6;
    localparam [3:0] R_RANGE_RTS  = 4'h7;
    localparam [3:0] R_QOS        = 4'h8;
    localparam [3:0] R_ID_INDEX   = 4'h9;
    localparam [3:0] R_ID_DATA    = 4'hA;
    localparam [3:0] R_BOOT_ADDR  = 4'hB;
    localparam [3:0] R_BOOT_LEN   = 4'hC;
    localparam [3:0] R_BOOT_ARG   = 4'hD;
    localparam [3:0] R_WINDOW     = 4'hE;

    // ========================================================================
    // Registers
    // ========================================================================
    reg        ctl_enable;
    reg        ctl_reset_hold;
    reg        ctl_stop_req;
    reg        ctl_doorbell_en;
    reg        ctl_attn_en;
    reg        ctl_borrow_en;
    reg [7:0]  ctl_attn_core;

    reg [RANGE_BITS-1:0] range_sel;
    reg [63:0] stage_base;
    reg [63:0] stage_limit;
    reg [63:0] range_base  [0:RANGES-1];
    reg [63:0] range_limit [0:RANGES-1];
    reg [1:0]  range_rights[0:RANGES-1];

    reg [7:0]  qos_weight;
    reg [15:0] qos_bwlimit;
    reg [63:0] boot_addr;
    reg [63:0] boot_len;
    reg [63:0] boot_arg;

    reg        fault;
    reg [63:0] fault_addr;
    reg [1:0]  fault_size;
    reg        fault_wen;
    reg [3:0]  fault_why;
    reg [7:0]  fault_len;
    reg [15:0] refusals;

    reg        id_done;
    reg        id_start;
    reg [63:0] id_data;

    // The link carries traffic only while a chiplet is attached and released
    // from reset; its outputs are ignored otherwise.
    wire link_up = sb_present && !ctl_reset_hold;

    assign sb_rst_n     = rst_n && !ctl_reset_hold;
    assign sb_stop_req  = ctl_stop_req;
    assign sb_boot_addr = boot_addr;
    assign sb_boot_len  = boot_len;
    assign sb_boot_arg  = boot_arg;
    assign m_weight     = qos_weight;
    assign m_bwlimit    = qos_bwlimit;

    // ========================================================================
    // MegaPad's writes into memory the chiplet lends
    // ========================================================================
    // WR_IDLE   no write; a new one is refused here or starts sending
    // WR_SEND   units pass from the stage to the link as the link takes them
    // WR_ANSWER every unit is sent; the chiplet's answer is awaited
    // WR_DRAIN  the write has failed or been cancelled toward the stage;
    //           cancel is held until the chiplet's answer arrives
    localparam [1:0] WR_IDLE   = 2'd0;
    localparam [1:0] WR_SEND   = 2'd1;
    localparam [1:0] WR_ANSWER = 2'd2;
    localparam [1:0] WR_DRAIN  = 2'd3;

    reg [1:0] wr_state;
    reg [7:0] wr_left;        // units after the one now offered
    reg [7:0] wr_timer;       // cycles the chiplet has kept this phase waiting
    reg       wr_answered;    // the stage's write ended; wait for bw_valid low
    reg       wr_cancelled;   // the outstanding write was cancelled
    reg       wr_cancel_seen; // this cancel is confirmed; wait for it to drop

    wire wr_sending  = (wr_state == WR_SEND);
    wire wr_waiting  = wr_sending || (wr_state == WR_ANSWER);
    wire wr_open     = link_up && ctl_borrow_en;
    wire wr_new      = bw_valid && !bw_cancel && !wr_answered &&
                       (wr_state == WR_IDLE || wr_state == WR_DRAIN);
    wire wr_cancel   = bw_cancel && !wr_cancel_seen;
    wire wr_unit_go  = lk_wq_valid && lk_wq_ready;
    wire wr_answer   = lk_wa_valid && lk_wa_ready;
    // Between units the stage may take a cycle to offer the next one; those
    // cycles are the stage's, and the bound counts only the chiplet's.
    wire wr_stage_holds = wr_sending && !bw_valid && !bw_cancel &&
                          !wr_cancelled;
    // A unit taken or an answer given on the 255th cycle still counts.
    wire wr_expired  = (wr_timer == 8'd254) && !wr_unit_go &&
                       !wr_stage_holds;

    assign lk_wq_valid  = link_up && wr_sending && bw_valid &&
                          !bw_cancel && !wr_cancelled;
    assign bw_ready     = lk_wq_valid && lk_wq_ready;
    // The link carries the chiplet's own address.
    assign lk_wq_addr   = bw_addr - WINDOW_BASE;
    assign lk_wq_data   = bw_data;
    assign lk_wq_len    = bw_len;
    assign lk_wq_zeros  = bw_zeros;
    assign lk_wq_cancel = link_up &&
                          ((wr_state == WR_DRAIN) ||
                           (wr_waiting && (bw_cancel || wr_cancelled)));
    assign lk_wa_ready  = link_up && (wr_state != WR_IDLE);

    always @(posedge clk) begin
        if (!rst_n) begin
            wr_state       <= WR_IDLE;
            wr_left        <= 8'd0;
            wr_timer       <= 8'd0;
            wr_answered    <= 1'b0;
            wr_cancelled   <= 1'b0;
            wr_cancel_seen <= 1'b0;
            bw_done        <= 1'b0;
            bw_error       <= 1'b0;
            bw_cancel_done <= 1'b0;
        end else begin
            bw_done        <= 1'b0;
            bw_error       <= 1'b0;
            bw_cancel_done <= 1'b0;
            if (!bw_valid)
                wr_answered <= 1'b0;
            if (!bw_cancel)
                wr_cancel_seen <= 1'b0;

            case (wr_state)
                WR_IDLE: begin
                    wr_timer     <= 8'd0;
                    wr_cancelled <= 1'b0;
                    if (wr_cancel) begin
                        // Nothing of the stage's has left the bridge.
                        bw_cancel_done <= 1'b1;
                        wr_cancel_seen <= 1'b1;
                    end else if (wr_new) begin
                        if (!wr_open) begin
                            // Refused here: nothing crosses the link.
                            bw_done     <= 1'b1;
                            bw_error    <= 1'b1;
                            wr_answered <= 1'b1;
                        end else begin
                            wr_left  <= bw_len;
                            wr_state <= WR_SEND;
                        end
                    end
                end

                WR_SEND, WR_ANSWER: begin
                    if (wr_cancel)
                        wr_cancelled <= 1'b1;

                    if (!link_up || wr_answer || wr_expired) begin
                        // (wr_expired excludes a unit taken this cycle.)
                        // The write ends: answered, lost with a chiplet put
                        // back into reset, or out of time.  A cancelled
                        // write is confirmed rather than answered.
                        if (wr_cancel || wr_cancelled) begin
                            bw_cancel_done <= 1'b1;
                            wr_cancel_seen <= 1'b1;
                        end else begin
                            bw_done  <= 1'b1;
                            bw_error <= !link_up || !wr_answer ||
                                        (lk_wa_code != LK_OK);
                        end
                        wr_answered  <= 1'b1;
                        wr_cancelled <= 1'b0;
                        wr_timer     <= 8'd0;
                        // Out of time, cancel stays on the link until the
                        // chiplet answers.
                        wr_state <= (link_up && !wr_answer) ? WR_DRAIN
                                                            : WR_IDLE;
                    end else if (wr_unit_go) begin
                        wr_timer <= 8'd0;
                        if (wr_left == 8'd0)
                            wr_state <= WR_ANSWER;
                        else
                            wr_left <= wr_left - 8'd1;
                    end else if (!wr_stage_holds) begin
                        wr_timer <= wr_timer + 8'd1;
                    end
                end

                WR_DRAIN: begin
                    if (wr_cancel) begin
                        // The stage's write already ended.
                        bw_cancel_done <= 1'b1;
                        wr_cancel_seen <= 1'b1;
                    end
                    if (!link_up || wr_answer) begin
                        wr_state <= WR_IDLE;
                    end else if (wr_new) begin
                        // Writes are refused until the answer arrives.
                        bw_done     <= 1'b1;
                        bw_error    <= 1'b1;
                        wr_answered <= 1'b1;
                    end
                end

                default: wr_state <= WR_IDLE;
            endcase
        end
    end

    // ========================================================================
    // Request state
    // ========================================================================
    localparam [2:0] ST_IDLE  = 3'd0;
    localparam [2:0] ST_CHECK = 3'd1;
    localparam [2:0] ST_MEM   = 3'd2;
    localparam [2:0] ST_RESP  = 3'd3;
    localparam [2:0] ST_WDATA = 3'd4;   // a write burst awaits its next word

    reg [2:0]  state;
    reg [63:0] rq_addr;        // address of the current word
    reg [63:0] rq_wdata;
    reg        rq_wen;
    reg [1:0]  rq_size;
    reg [7:0]  rq_left;        // words of the burst after the current one
    reg        rq_cancelled;

    assign lk_rq_ready = link_up &&
                         (state == ST_IDLE || state == ST_WDATA);
    assign lk_rs_valid = link_up && (state == ST_RESP);

    assign m_req   = (state == ST_MEM);
    assign m_addr  = rq_addr;
    assign m_wdata = rq_wdata;
    assign m_wen   = rq_wen;
    assign m_size  = rq_size;

    // ========================================================================
    // Admission check on the latched request
    // ========================================================================
    // The check runs once, in ST_CHECK, when rq_left still holds the burst's
    // length, so it covers every word of a burst.  A burst is made of whole
    // 64-bit words.
    wire        rq_burst = (rq_left != 8'd0);
    wire [3:0]  rq_bytes = 4'd1 << rq_size;
    wire [11:0] rq_span  = rq_burst ? {({1'b0, rq_left} + 9'd1), 3'b000}
                                    : {8'd0, rq_bytes};
    wire [64:0] rq_end   = {1'b0, rq_addr} + {53'd0, rq_span};
    wire        rq_aligned =
        (!rq_burst && rq_size == BUS_BYTE) ||
        (!rq_burst && rq_size == BUS_HALF  && rq_addr[0]   == 1'b0) ||
        (!rq_burst && rq_size == BUS_WORD  && rq_addr[1:0] == 2'b00) ||
        (rq_size == BUS_DWORD && rq_addr[2:0] == 3'b000);
    wire        rq_in_ext = (rq_addr >= EXT_BASE) &&
                            (rq_end <= {1'b0, EXT_LIMIT});

    reg     rq_lent;
    integer ci;
    always @(*) begin
        rq_lent = 1'b0;
        for (ci = 0; ci < RANGES; ci = ci + 1)
            if (range_rights[ci][rq_wen] &&
                rq_addr >= range_base[ci] &&
                rq_end <= {1'b0, range_limit[ci]})
                rq_lent = 1'b1;
    end

    wire       rq_admit = ctl_enable && rq_aligned && rq_in_ext && rq_lent;
    wire [3:0] rq_why   = !ctl_enable ? WHY_CLOSED
                        : !rq_aligned ? WHY_MISALIGNED
                        : !rq_in_ext  ? WHY_NOT_EXT
                        :               WHY_NOT_LENT;

    // ========================================================================
    // Doorbells
    // ========================================================================
    // To the chiplet: latch one pending source so the payload stays stable
    // until the chiplet takes it, then clear that bit in the mailbox.
    reg dbt_hold;
    assign lk_dbt_valid  = link_up && dbt_hold;
    assign mb_take_valid = lk_dbt_valid && lk_dbt_ready;
    assign mb_take_src   = lk_dbt_src;

    // From the chiplet, and the bridge's own attention ring.  The attention
    // ring goes first; the chiplet's doorbell waits that one cycle.
    reg attn_pending;
    wire dbr_accept = link_up && lk_dbr_valid && !attn_pending;
    assign lk_dbr_ready  = link_up && !attn_pending;
    assign mb_send_valid = attn_pending || (dbr_accept && ctl_doorbell_en);
    assign mb_send_dst   = attn_pending ? ctl_attn_core : lk_dbr_dst;

    // ========================================================================
    // Sequential logic
    // ========================================================================
    wire reg_write = reg_sel && reg_wen;
    wire refuse_now = (state == ST_CHECK) && link_up && !rq_cancelled &&
                      !lk_rq_cancel && !rq_admit;

    integer ri;
    always @(posedge clk) begin
        if (!rst_n) begin
            ctl_enable      <= 1'b0;
            ctl_reset_hold  <= 1'b1;
            ctl_stop_req    <= 1'b0;
            ctl_doorbell_en <= 1'b0;
            ctl_attn_en     <= 1'b0;
            ctl_borrow_en   <= 1'b0;
            ctl_attn_core   <= 8'd0;
            range_sel       <= {RANGE_BITS{1'b0}};
            stage_base      <= 64'd0;
            stage_limit     <= 64'd0;
            for (ri = 0; ri < RANGES; ri = ri + 1) begin
                range_base[ri]   <= 64'd0;
                range_limit[ri]  <= 64'd0;
                range_rights[ri] <= 2'b00;
            end
            qos_weight   <= 8'd1;
            qos_bwlimit  <= 16'd0;
            boot_addr    <= 64'd0;
            boot_len     <= 64'd0;
            boot_arg     <= 64'd0;
            fault        <= 1'b0;
            fault_addr   <= 64'd0;
            fault_size   <= 2'd0;
            fault_wen    <= 1'b0;
            fault_why    <= 4'd0;
            fault_len    <= 8'd0;
            refusals     <= 16'd0;
            id_done      <= 1'b0;
            id_start     <= 1'b0;
            id_data      <= 64'd0;
            sb_id_req    <= 1'b0;
            sb_id_index  <= 8'd0;
            state        <= ST_IDLE;
            rq_addr      <= 64'd0;
            rq_wdata     <= 64'd0;
            rq_wen       <= 1'b0;
            rq_size      <= BUS_DWORD;
            rq_left      <= 8'd0;
            rq_cancelled <= 1'b0;
            lk_rs_rdata  <= 64'd0;
            lk_rs_code   <= LK_OK;
            lk_rs_last   <= 1'b1;
            dbt_hold     <= 1'b0;
            lk_dbt_src   <= 8'd0;
            attn_pending <= 1'b0;
        end else begin
            // ---------------- Register writes ----------------
            if (reg_write) begin
                case (reg_idx)
                    R_CTRL: begin
                        ctl_enable      <= reg_wdata[0];
                        ctl_reset_hold  <= reg_wdata[1];
                        ctl_stop_req    <= reg_wdata[2];
                        ctl_doorbell_en <= reg_wdata[3];
                        ctl_attn_en     <= reg_wdata[4];
                        ctl_borrow_en   <= reg_wdata[5];
                        ctl_attn_core   <= reg_wdata[15:8];
                    end
                    R_STATUS: begin
                        if (reg_wdata[4]) begin
                            fault    <= 1'b0;
                            refusals <= 16'd0;
                        end
                    end
                    R_RANGE_SEL:  range_sel   <= reg_wdata[RANGE_BITS-1:0];
                    R_RANGE_BASE: stage_base  <= reg_wdata;
                    R_RANGE_LIM:  stage_limit <= reg_wdata;
                    R_RANGE_RTS: begin
                        if ({1'b0, range_sel} < RANGES[RANGE_BITS:0]) begin
                            range_base[range_sel]   <= stage_base;
                            range_limit[range_sel]  <= stage_limit;
                            range_rights[range_sel] <= reg_wdata[1:0];
                        end
                    end
                    R_QOS: begin
                        qos_weight  <= reg_wdata[7:0];
                        qos_bwlimit <= reg_wdata[31:16];
                    end
                    R_ID_INDEX: begin
                        // Restart the sideband read with the new index.
                        sb_id_req   <= 1'b0;
                        sb_id_index <= reg_wdata[7:0];
                        id_done     <= 1'b0;
                        id_start    <= 1'b1;
                    end
                    R_BOOT_ADDR: boot_addr <= reg_wdata;
                    R_BOOT_LEN:  boot_len  <= reg_wdata;
                    R_BOOT_ARG:  boot_arg  <= reg_wdata;
                    default: ;
                endcase
            end

            // ---------------- Sideband identity read ----------------
            if (!(reg_write && reg_idx == R_ID_INDEX)) begin
                if (id_start) begin
                    id_start  <= 1'b0;
                    sb_id_req <= 1'b1;
                end else if (sb_id_req && sb_id_ack) begin
                    sb_id_req <= 1'b0;
                    id_data   <= sb_id_data;
                    id_done   <= 1'b1;
                end
            end

            // ---------------- Memory request ----------------
            case (state)
                ST_IDLE: begin
                    rq_cancelled <= 1'b0;
                    if (lk_rq_valid && lk_rq_ready) begin
                        rq_addr  <= lk_rq_addr;
                        rq_wdata <= lk_rq_wdata;
                        rq_wen   <= lk_rq_wen;
                        rq_size  <= lk_rq_size;
                        rq_left  <= lk_rq_len;
                        state    <= ST_CHECK;
                    end
                end

                ST_CHECK: begin
                    lk_rs_rdata <= 64'd0;
                    lk_rs_last  <= 1'b1;
                    if (rq_cancelled || lk_rq_cancel || !link_up) begin
                        lk_rs_code <= LK_CANCELLED;
                        state      <= ST_RESP;
                    end else if (rq_admit) begin
                        state <= ST_MEM;
                    end else begin
                        lk_rs_code <= LK_REFUSED;
                        state      <= ST_RESP;
                    end
                end

                ST_MEM: begin
                    if (lk_rq_cancel || !link_up)
                        rq_cancelled <= 1'b1;
                    if (m_ack) begin
                        if (rq_cancelled || lk_rq_cancel || !link_up) begin
                            lk_rs_rdata <= 64'd0;
                            lk_rs_code  <= LK_CANCELLED;
                            lk_rs_last  <= 1'b1;
                            state       <= ST_RESP;
                        end else if (m_error) begin
                            lk_rs_rdata <= 64'd0;
                            lk_rs_code  <= LK_MEM_ERROR;
                            lk_rs_last  <= 1'b1;
                            state       <= ST_RESP;
                        end else if (rq_wen && rq_left != 8'd0) begin
                            // A written word of a burst: take the next one.
                            rq_addr <= rq_addr + 64'd8;
                            rq_left <= rq_left - 8'd1;
                            state   <= ST_WDATA;
                        end else begin
                            // A read word, or the last written word.
                            lk_rs_rdata <= rq_wen ? 64'd0 : m_rdata;
                            lk_rs_code  <= LK_OK;
                            lk_rs_last  <= (rq_left == 8'd0);
                            state       <= ST_RESP;
                        end
                    end
                end

                ST_WDATA: begin
                    if (!link_up || lk_rq_cancel) begin
                        lk_rs_rdata <= 64'd0;
                        lk_rs_code  <= LK_CANCELLED;
                        lk_rs_last  <= 1'b1;
                        state       <= ST_RESP;
                    end else if (lk_rq_valid) begin
                        rq_wdata <= lk_rq_wdata;
                        state    <= ST_MEM;
                    end
                end

                ST_RESP: begin
                    if (lk_rq_cancel)
                        rq_cancelled <= 1'b1;
                    // A chiplet that has gone back into reset takes no
                    // response; drop it and reopen.
                    if (!link_up) begin
                        state <= ST_IDLE;
                    end else if (lk_rs_valid && lk_rs_ready) begin
                        if (lk_rs_last) begin
                            state <= ST_IDLE;
                        end else if (rq_cancelled || lk_rq_cancel) begin
                            // A read burst cancelled between its words.
                            lk_rs_rdata <= 64'd0;
                            lk_rs_code  <= LK_CANCELLED;
                            lk_rs_last  <= 1'b1;
                        end else begin
                            rq_addr <= rq_addr + 64'd8;
                            rq_left <= rq_left - 8'd1;
                            state   <= ST_MEM;
                        end
                    end
                end

                default: state <= ST_IDLE;
            endcase

            // ---------------- Refusal record ----------------
            if (refuse_now) begin
                if (refusals != 16'hffff)
                    refusals <= refusals + 16'd1;
                if (!fault) begin
                    fault      <= 1'b1;
                    fault_addr <= rq_addr;
                    fault_size <= rq_size;
                    fault_wen  <= rq_wen;
                    fault_why  <= rq_why;
                    fault_len  <= rq_left;
                    if (ctl_attn_en)
                        attn_pending <= 1'b1;
                end
            end
            if (attn_pending)
                attn_pending <= 1'b0;

            // ---------------- Doorbell to the chiplet ----------------
            if (dbt_hold) begin
                if (!link_up || (lk_dbt_valid && lk_dbt_ready))
                    dbt_hold <= 1'b0;
            end else if (link_up && mb_db_valid) begin
                dbt_hold   <= 1'b1;
                lk_dbt_src <= mb_db_src;
            end
        end
    end

    // ========================================================================
    // Register reads
    // ========================================================================
    wire range_sel_valid = ({1'b0, range_sel} < RANGES[RANGE_BITS:0]);

    always @(*) begin
        reg_rdata = 64'd0;
        case (reg_idx)
            R_CTRL:       reg_rdata = {48'd0, ctl_attn_core, 2'd0,
                                       ctl_borrow_en,
                                       ctl_attn_en, ctl_doorbell_en,
                                       ctl_stop_req, ctl_reset_hold,
                                       ctl_enable};
            R_STATUS:     reg_rdata = {24'd0, RANGES[7:0], 8'd0, PORT_ID,
                                       9'd0, (wr_state != WR_IDLE),
                                       id_done, fault,
                                       (state != ST_IDLE), sb_ready,
                                       sb_stopped, sb_present};
            R_FAULT_ADDR: reg_rdata = fault_addr;
            R_FAULT_INFO: reg_rdata = {32'd0, refusals, fault_len, fault_why,
                                       1'b0, fault_wen, fault_size};
            R_RANGE_SEL:  reg_rdata = {{(64-RANGE_BITS){1'b0}}, range_sel};
            R_RANGE_BASE: reg_rdata = range_sel_valid
                                      ? range_base[range_sel] : 64'd0;
            R_RANGE_LIM:  reg_rdata = range_sel_valid
                                      ? range_limit[range_sel] : 64'd0;
            R_RANGE_RTS:  reg_rdata = range_sel_valid
                                      ? {62'd0, range_rights[range_sel]}
                                      : 64'd0;
            R_QOS:        reg_rdata = {32'd0, qos_bwlimit, 8'd0, qos_weight};
            R_ID_INDEX:   reg_rdata = {56'd0, sb_id_index};
            R_ID_DATA:    reg_rdata = id_data;
            R_BOOT_ADDR:  reg_rdata = boot_addr;
            R_BOOT_LEN:   reg_rdata = boot_len;
            R_BOOT_ARG:   reg_rdata = boot_arg;
            R_WINDOW:     reg_rdata = WINDOW_BASE;
            default:      reg_rdata = 64'd0;
        endcase
    end

endmodule
