// ============================================================================
// mp64_extmem_arb.v — Client arbiter for the external-memory 64-bit port
// ============================================================================
//
// Places several requesters in front of the single-beat 64-bit port of
// mp64_extmem.  Client 0 is the core path, the memory subsystem's forward of
// external addresses.  Every further client is a chiplet port, whose bridge
// has already checked the request against the ranges lent to that chiplet.
//
// Client contract (the contract of mp64_extmem's CPU port):
//   - REQ is held with a stable payload until the ACK pulse.
//   - ACK is one cycle wide.  ERROR accompanies it when the access failed.
//   - A client may withdraw REQ before ACK to abandon the access.
//   - REQ returns low for at least one cycle between two accesses.
//
// One access is in flight at a time, and a sub-word write's read and write
// belong to that one access, so the controller remains the single point that
// orders every access to external memory.
//
// Arbitration follows the main bus arbiter's policy:
//   - Weighted round-robin.  A client keeps the grant for `weight`
//     consecutive accesses while it keeps requesting; weight 0 counts as 1.
//   - A bandwidth limit: completed accesses per epoch of 65,536 cycles.
//     A client that reaches its limit waits for the next epoch.  Limit 0
//     means unlimited.
//
// Timing: an uncontended request reaches the controller in the cycle it is
// raised, and ACK returns to the client in the cycle the controller raises
// it, so the core path keeps the latency it has without the arbiter.  After
// each access the controller's request line is held low for one cycle, which
// lets the controller accept a different client's request next.
//
// Coding standard: Verilog-2001, sync reset, non-blocking assigns.
//

module mp64_extmem_arb #(
    parameter N_CLIENTS   = 2,
    parameter CLIENT_BITS = 1      // ceil(log2(N_CLIENTS)), at least 1
)(
    input  wire                    clk,
    input  wire                    rst_n,

    // === Clients (flat-packed, N_CLIENTS wide) ===
    input  wire [N_CLIENTS-1:0]    c_req,
    input  wire [N_CLIENTS*64-1:0] c_addr,
    input  wire [N_CLIENTS*64-1:0] c_wdata,
    input  wire [N_CLIENTS-1:0]    c_wen,
    input  wire [N_CLIENTS*2-1:0]  c_size,
    output wire [N_CLIENTS-1:0]    c_ack,
    output wire [N_CLIENTS-1:0]    c_error,
    output wire [63:0]             c_rdata,     // shared; valid with c_ack

    // === Per-client QoS ===
    input  wire [N_CLIENTS*8-1:0]  c_weight,    // 0 counts as 1
    input  wire [N_CLIENTS*16-1:0] c_bwlimit,   // 0 = unlimited

    // === mp64_extmem CPU port ===
    output wire                    ext_req,
    output wire [63:0]             ext_addr,
    output wire [63:0]             ext_wdata,
    output wire                    ext_wen,
    output wire [1:0]              ext_size,
    input  wire [63:0]             ext_rdata,
    input  wire                    ext_ack,
    input  wire                    ext_error
);

    // ========================================================================
    // State
    // ========================================================================
    reg                   busy;         // an access is granted and in flight
    reg                   gap;          // one low cycle after each access
    reg [CLIENT_BITS-1:0] grant;        // client of the access in flight
    reg [N_CLIENTS-1:0]   served;       // completed; waits for REQ to drop
    reg [CLIENT_BITS-1:0] last_grant;
    reg [7:0]             weight_remain;
    reg [15:0]            epoch_timer;
    reg [15:0]            bw_cnt [0:N_CLIENTS-1];

    // ========================================================================
    // Eligibility: requesting, not already served, inside its bandwidth
    // ========================================================================
    wire [N_CLIENTS-1:0] eligible;
    genvar ei;
    generate
        for (ei = 0; ei < N_CLIENTS; ei = ei + 1) begin : g_elig
            wire [15:0] limit = c_bwlimit[ei*16 +: 16];
            assign eligible[ei] = c_req[ei] && !served[ei] &&
                                  (limit == 16'd0 || bw_cnt[ei] < limit);
        end
    endgenerate

    // ========================================================================
    // Weighted round-robin choice (iterative scan, no `%` operator)
    // ========================================================================
    reg [CLIENT_BITS-1:0] winner;
    reg                   any_eligible;
    integer               rr_i;
    reg [CLIENT_BITS:0]   rr_wide;
    reg [CLIENT_BITS-1:0] rr_candidate;

    always @(*) begin
        winner       = last_grant;
        any_eligible = 1'b0;
        rr_wide      = {(CLIENT_BITS+1){1'b0}};
        rr_candidate = {CLIENT_BITS{1'b0}};
        if (weight_remain != 8'd0 && eligible[last_grant]) begin
            winner       = last_grant;
            any_eligible = 1'b1;
        end else begin
            for (rr_i = 1; rr_i <= N_CLIENTS; rr_i = rr_i + 1) begin
                rr_wide = {1'b0, last_grant} + rr_i[CLIENT_BITS:0];
                if (rr_wide >= N_CLIENTS[CLIENT_BITS:0])
                    rr_wide = rr_wide - N_CLIENTS[CLIENT_BITS:0];
                rr_candidate = rr_wide[CLIENT_BITS-1:0];
                if (!any_eligible && eligible[rr_candidate]) begin
                    winner       = rr_candidate;
                    any_eligible = 1'b1;
                end
            end
        end
    end

    // ========================================================================
    // Request path: the access in flight, or the uncontended pass-through
    // ========================================================================
    wire                   capture = !busy && !gap && any_eligible;
    wire [CLIENT_BITS-1:0] sel     = busy ? grant : winner;

    assign ext_req   = busy ? c_req[grant] : capture;
    assign ext_addr  = c_addr [sel*64 +: 64];
    assign ext_wdata = c_wdata[sel*64 +: 64];
    assign ext_wen   = c_wen  [sel];
    assign ext_size  = c_size [sel*2 +: 2];

    // ========================================================================
    // Response path
    // ========================================================================
    wire complete = busy && ext_ack;

    assign c_rdata = ext_rdata;
    genvar ri;
    generate
        for (ri = 0; ri < N_CLIENTS; ri = ri + 1) begin : g_resp
            assign c_ack[ri]   = complete && (grant == ri[CLIENT_BITS-1:0]);
            assign c_error[ri] = c_ack[ri] && ext_error;
        end
    endgenerate

    // ========================================================================
    // Sequencing
    // ========================================================================
    wire [7:0] winner_weight_raw = c_weight[winner*8 +: 8];
    wire [7:0] winner_weight     = (winner_weight_raw == 8'd0)
                                   ? 8'd1 : winner_weight_raw;

    integer si;
    always @(posedge clk) begin
        if (!rst_n) begin
            busy          <= 1'b0;
            gap           <= 1'b0;
            grant         <= {CLIENT_BITS{1'b0}};
            served        <= {N_CLIENTS{1'b0}};
            last_grant    <= {CLIENT_BITS{1'b0}};
            weight_remain <= 8'd0;
        end else begin
            // A served client becomes eligible again once its REQ has dropped.
            for (si = 0; si < N_CLIENTS; si = si + 1)
                if (!c_req[si])
                    served[si] <= 1'b0;

            if (busy) begin
                if (ext_ack) begin
                    // Terminal response.  Hold the client off until it lowers
                    // REQ, so one held request runs exactly once.
                    busy <= 1'b0;
                    gap  <= 1'b1;
                    if (c_req[grant])
                        served[grant] <= 1'b1;
                end else if (!c_req[grant]) begin
                    // Withdrawn before the response.  The controller drains
                    // any launched word and returns no ACK for it.
                    busy <= 1'b0;
                    gap  <= 1'b1;
                end
            end else if (gap) begin
                gap <= 1'b0;
            end else if (any_eligible) begin
                busy  <= 1'b1;
                grant <= winner;
                if (winner == last_grant && weight_remain != 8'd0) begin
                    weight_remain <= weight_remain - 8'd1;
                end else begin
                    last_grant    <= winner;
                    weight_remain <= winner_weight - 8'd1;
                end
            end
        end
    end

    // ========================================================================
    // Bandwidth accounting: completed accesses per 65,536-cycle epoch
    // ========================================================================
    // This block is the one sequential owner of bw_cnt[].  A completion on
    // the epoch's last cycle counts into the new epoch.
    integer bi;
    always @(posedge clk) begin
        if (!rst_n) begin
            epoch_timer <= 16'd0;
            for (bi = 0; bi < N_CLIENTS; bi = bi + 1)
                bw_cnt[bi] <= 16'd0;
        end else begin
            epoch_timer <= epoch_timer + 16'd1;
            for (bi = 0; bi < N_CLIENTS; bi = bi + 1) begin
                if (epoch_timer == 16'hffff) begin
                    if (complete && grant == bi[CLIENT_BITS-1:0])
                        bw_cnt[bi] <= 16'd1;
                    else
                        bw_cnt[bi] <= 16'd0;
                end else if (complete && grant == bi[CLIENT_BITS-1:0] &&
                             bw_cnt[bi] != 16'hffff) begin
                    bw_cnt[bi] <= bw_cnt[bi] + 16'd1;
                end
            end
        end
    end

endmodule
