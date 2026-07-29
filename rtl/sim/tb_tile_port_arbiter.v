// ============================================================================
// tb_tile_port_arbiter.v — Buffered tile request/response ownership contract
// ============================================================================

`timescale 1ns / 1ps

module tb_tile_port_arbiter;

    localparam integer SOURCE_COUNT = 7;

    reg clk;
    reg rst;

    reg [SOURCE_COUNT-1:0]     src_tile_req;
    reg [SOURCE_COUNT*32-1:0]  src_tile_addr;
    reg [SOURCE_COUNT-1:0]     src_tile_wen;
    reg [SOURCE_COUNT*512-1:0] src_tile_wdata;
    wire [SOURCE_COUNT-1:0]    src_tile_ack;
    wire [SOURCE_COUNT-1:0]    src_tile_error;
    wire [SOURCE_COUNT*64-1:0] src_tile_fault_addr;

    reg [SOURCE_COUNT-1:0]     src_ext_req;
    reg [SOURCE_COUNT*64-1:0]  src_ext_addr;
    reg [SOURCE_COUNT-1:0]     src_ext_wen;
    reg [SOURCE_COUNT*512-1:0] src_ext_wdata;
    wire [SOURCE_COUNT-1:0]    src_ext_ack;
    wire [SOURCE_COUNT-1:0]    src_ext_error;
    wire [SOURCE_COUNT*64-1:0] src_ext_fault_addr;

    reg [SOURCE_COUNT-1:0]     src_cancel;
    wire [SOURCE_COUNT-1:0]    src_accept;
    wire [SOURCE_COUNT-1:0]    src_cancel_done;

    wire         tile_req;
    wire [31:0]  tile_addr;
    wire         tile_wen;
    wire [511:0] tile_wdata;
    reg          tile_accept;
    reg          tile_ack;
    reg          tile_error;
    reg [63:0]   tile_fault_addr;
    wire         tile_cancel;

    wire         ext_req;
    wire [63:0]  ext_addr;
    wire         ext_wen;
    wire [511:0] ext_wdata;
    reg          ext_accept;
    reg          ext_ack;
    reg          ext_error;
    reg [63:0]   ext_fault_addr;
    wire         ext_cancel;

    wire         write_commit;
    wire [2:0]   write_owner;
    wire         write_ext;
    wire [63:0]  write_addr;
    wire         ext_word_owner_valid;
    wire [2:0]   ext_word_owner;

    integer pass_count;
    integer fail_count;

    mp64_tile_port_arbiter uut (
        .clk            (clk),
        .rst            (rst),
        .src_tile_req   (src_tile_req),
        .src_tile_addr  (src_tile_addr),
        .src_tile_wen   (src_tile_wen),
        .src_tile_wdata (src_tile_wdata),
        .src_tile_ack   (src_tile_ack),
        .src_tile_error (src_tile_error),
        .src_tile_fault_addr(src_tile_fault_addr),
        .src_ext_req    (src_ext_req),
        .src_ext_addr   (src_ext_addr),
        .src_ext_wen    (src_ext_wen),
        .src_ext_wdata  (src_ext_wdata),
        .src_ext_ack    (src_ext_ack),
        .src_ext_error  (src_ext_error),
        .src_ext_fault_addr(src_ext_fault_addr),
        .src_cancel     (src_cancel),
        .src_accept     (src_accept),
        .src_cancel_done(src_cancel_done),
        .tile_req       (tile_req),
        .tile_addr      (tile_addr),
        .tile_wen       (tile_wen),
        .tile_wdata     (tile_wdata),
        .tile_accept    (tile_accept),
        .tile_ack       (tile_ack),
        .tile_error     (tile_error),
        .tile_fault_addr(tile_fault_addr),
        .tile_cancel    (tile_cancel),
        .ext_req        (ext_req),
        .ext_addr       (ext_addr),
        .ext_wen        (ext_wen),
        .ext_wdata      (ext_wdata),
        .ext_accept     (ext_accept),
        .ext_ack        (ext_ack),
        .ext_error      (ext_error),
        .ext_fault_addr (ext_fault_addr),
        .ext_cancel     (ext_cancel),
        .write_commit   (write_commit),
        .write_owner    (write_owner),
        .write_ext      (write_ext),
        .write_addr     (write_addr),
        .ext_word_owner_valid(ext_word_owner_valid),
        .ext_word_owner (ext_word_owner)
    );

    initial clk = 1'b0;
    always #5 clk = ~clk;

    task clock;
    begin
        @(posedge clk);
        #1;
    end
    endtask

    task check;
        input [511:0] label;
        input condition;
    begin
        if (condition) begin
            pass_count = pass_count + 1;
            $display("  PASS: %0s", label);
        end else begin
            fail_count = fail_count + 1;
            $display("  FAIL: %0s", label);
        end
    end
    endtask

    task accept_internal;
    begin
        check("internal transaction has no external word owner",
              !ext_word_owner_valid);
        tile_accept = 1'b1;
        #1;
        check("internal request remains asserted through ACCEPT", tile_req);
        clock;
        tile_accept = 1'b0;
        #1;
        check("internal request drops after ACCEPT", !tile_req);
    end
    endtask

    task complete_internal;
        input [2:0] owner;
    begin
        tile_ack = 1'b1;
        #1;
        check("internal request remains suppressed during ACK", !tile_req);
        check("internal ACK routed only to captured owner",
              src_tile_ack
                  == ({{(SOURCE_COUNT-1){1'b0}}, 1'b1} << owner)
              && src_ext_ack == {SOURCE_COUNT{1'b0}});
        clock;
        tile_ack = 1'b0;
        clock;
    end
    endtask

    task accept_external;
        input [2:0] owner;
    begin
        check("external word owner is invalid before target ACCEPT",
              !ext_word_owner_valid);
        ext_accept = 1'b1;
        #1;
        check("external request remains asserted through ACCEPT", ext_req);
        check("target ACCEPT makes the captured external owner visible",
              ext_word_owner_valid && ext_word_owner == owner);
        clock;
        ext_accept = 1'b0;
        #1;
        check("external request drops after ACCEPT", !ext_req);
        check("accepted external transaction retains its word owner",
              ext_word_owner_valid && ext_word_owner == owner);
    end
    endtask

    task complete_external;
        input [2:0] owner;
        input       expected_write;
    begin
        check("external word owner remains stable before terminal ACK",
              ext_word_owner_valid && ext_word_owner == owner);
        ext_ack = 1'b1;
        #1;
        check("external request suppressed during ACK", !ext_req);
        check("external ACK routed only to captured owner",
              src_ext_ack
                  == ({{(SOURCE_COUNT-1){1'b0}}, 1'b1} << owner)
              && src_tile_ack == {SOURCE_COUNT{1'b0}});
        check("external completion write classification",
              write_commit == expected_write);
        check("external word owner remains valid for the ACK cycle",
              ext_word_owner_valid && ext_word_owner == owner);
        clock;
        check("terminal ACK retires external word ownership",
              !ext_word_owner_valid);
        ext_ack = 1'b0;
        clock;
    end
    endtask

    initial begin
        pass_count = 0;
        fail_count = 0;
        rst = 1'b1;
        src_tile_req = {SOURCE_COUNT{1'b0}};
        src_tile_addr = {(SOURCE_COUNT*32){1'b0}};
        src_tile_wen = {SOURCE_COUNT{1'b0}};
        src_tile_wdata = {(SOURCE_COUNT*512){1'b0}};
        src_ext_req = {SOURCE_COUNT{1'b0}};
        src_ext_addr = {(SOURCE_COUNT*64){1'b0}};
        src_ext_wen = {SOURCE_COUNT{1'b0}};
        src_ext_wdata = {(SOURCE_COUNT*512){1'b0}};
        src_cancel = {SOURCE_COUNT{1'b0}};
        tile_accept = 1'b0;
        tile_ack = 1'b0;
        tile_error = 1'b0;
        tile_fault_addr = 64'd0;
        ext_accept = 1'b0;
        ext_ack = 1'b0;
        ext_error = 1'b0;
        ext_fault_addr = 64'd0;

        repeat (2) clock;
        rst = 1'b0;
        clock;

        // A one-cycle source pulse must survive source payload changes and a
        // delayed target response.
        src_tile_addr[0 +: 32] = 32'h0000_1234;
        src_tile_wen[0] = 1'b1;
        src_tile_wdata[0 +: 512] = {8{64'hCAFE_BABE_DEAD_BEEF}};
        src_tile_req[0] = 1'b1;
        #1;
        check("source capture is explicitly acknowledged",
              src_accept == 7'b0000001);
        clock;
        src_tile_req[0] = 1'b0;
        src_tile_addr[0 +: 32] = 32'hFFFF_FFFF;
        src_tile_wen[0] = 1'b0;
        src_tile_wdata[0 +: 512] = 512'd0;
        clock;

        check("captured one-cycle request remains active", tile_req);
        check("captured internal address remains stable",
              tile_addr == 32'h0000_1234);
        check("captured write direction remains stable", tile_wen);
        check("captured write payload remains stable",
              tile_wdata == {8{64'hCAFE_BABE_DEAD_BEEF}});
        repeat (3) begin
            clock;
            check("delayed internal target retains request", tile_req);
            check("delayed internal target retains payload",
                  tile_addr == 32'h0000_1234
                  && tile_wdata == {8{64'hCAFE_BABE_DEAD_BEEF}});
        end

        accept_internal();
        tile_ack = 1'b1;
        #1;
        check("core-0 internal write emits commit metadata",
              write_commit && write_owner == 3'd0 && !write_ext
              && write_addr == 64'h0000_0000_0000_1234);
        complete_internal(3'd0);

        // After owner 0 completes, seven simultaneous peers must be retained
        // and served exactly once in equal round-robin order 1..6,0.  This
        // covers both full-core owners and cluster owners, including 6 -> 0.
        src_tile_req = {SOURCE_COUNT{1'b1}};
        src_tile_wen = {SOURCE_COUNT{1'b1}};
        src_tile_addr[0*32 +: 32] = 32'h0000_2000;
        src_tile_addr[1*32 +: 32] = 32'h0000_2100;
        src_tile_addr[2*32 +: 32] = 32'h0000_2200;
        src_tile_addr[3*32 +: 32] = 32'h0000_2300;
        src_tile_addr[4*32 +: 32] = 32'h0000_2400;
        src_tile_addr[5*32 +: 32] = 32'h0000_2500;
        src_tile_addr[6*32 +: 32] = 32'h0000_2600;
        src_tile_wdata[0*512 +: 512] = {64{8'h10}};
        src_tile_wdata[1*512 +: 512] = {64{8'h11}};
        src_tile_wdata[2*512 +: 512] = {64{8'h12}};
        src_tile_wdata[3*512 +: 512] = {64{8'h13}};
        src_tile_wdata[4*512 +: 512] = {64{8'h14}};
        src_tile_wdata[5*512 +: 512] = {64{8'h15}};
        src_tile_wdata[6*512 +: 512] = {64{8'h16}};
        #1;
        check("all simultaneous source pulses are captured",
              src_accept == {SOURCE_COUNT{1'b1}});
        clock;
        src_tile_req = {SOURCE_COUNT{1'b0}};
        src_tile_wen = {SOURCE_COUNT{1'b0}};
        src_tile_addr = {(SOURCE_COUNT*32){1'b0}};
        src_tile_wdata = {(SOURCE_COUNT*512){1'b0}};
        clock;

        check("round-robin serves owner 1 after owner 0",
              tile_req && tile_addr == 32'h0000_2100
              && tile_wdata == {64{8'h11}});
        accept_internal();
        complete_internal(3'd1);
        check("round-robin next serves owner 2",
              tile_req && tile_addr == 32'h0000_2200
              && tile_wdata == {64{8'h12}});
        accept_internal();
        complete_internal(3'd2);
        check("round-robin next serves owner 3",
              tile_req && tile_addr == 32'h0000_2300
              && tile_wdata == {64{8'h13}});
        accept_internal();
        complete_internal(3'd3);
        check("round-robin next serves owner 4",
              tile_req && tile_addr == 32'h0000_2400
              && tile_wdata == {64{8'h14}});
        accept_internal();
        complete_internal(3'd4);
        check("round-robin next serves owner 5",
              tile_req && tile_addr == 32'h0000_2500
              && tile_wdata == {64{8'h15}});
        accept_internal();
        complete_internal(3'd5);
        check("round-robin next serves owner 6",
              tile_req && tile_addr == 32'h0000_2600
              && tile_wdata == {64{8'h16}});
        accept_internal();
        complete_internal(3'd6);
        check("round-robin wraps from owner 6 to owner 0",
              tile_req && tile_addr == 32'h0000_2000
              && tile_wdata == {64{8'h10}});
        accept_internal();
        complete_internal(3'd0);

        // Requests arriving while another transfer is active must enter their
        // per-peer pending slots, including an external request.
        src_tile_req[0] = 1'b1;
        src_tile_addr[0 +: 32] = 32'h0000_3000;
        clock;
        src_tile_req[0] = 1'b0;
        clock;
        check("owner 0 transfer active for busy-arrival probe",
              tile_req && tile_addr == 32'h0000_3000);
        accept_internal();

        src_ext_req[5] = 1'b1;
        src_ext_addr[5*64 +: 64] = 64'h1234_5678_9ABC_DEF0;
        src_ext_wen[5] = 1'b0;
        src_ext_wdata[5*512 +: 512] = {64{8'h55}};
        src_tile_req[6] = 1'b1;
        src_tile_addr[6*32 +: 32] = 32'h0000_3600;
        src_tile_wen[6] = 1'b1;
        src_tile_wdata[6*512 +: 512] = {64{8'h66}};
        clock;
        src_ext_req[5] = 1'b0;
        src_ext_addr[5*64 +: 64] = 64'd0;
        src_ext_wen[5] = 1'b0;
        src_ext_wdata[5*512 +: 512] = 512'd0;
        src_tile_req[6] = 1'b0;
        src_tile_addr[6*32 +: 32] = 32'd0;
        src_tile_wen[6] = 1'b0;
        src_tile_wdata[6*512 +: 512] = 512'd0;

        complete_internal(3'd0);
        check("busy-time external pulse is retained",
              ext_req && ext_addr == 64'h1234_5678_9ABC_DEF0
              && !ext_wen && ext_wdata == {64{8'h55}});
        accept_external(3'd5);
        complete_external(3'd5, 1'b0);
        check("busy-time internal pulse follows in RR order",
              tile_req && tile_addr == 32'h0000_3600
              && tile_wen && tile_wdata == {64{8'h66}});
        accept_internal();
        tile_ack = 1'b1;
        #1;
        check("cluster write identifies its captured owner",
              write_commit && write_owner == 3'd6 && !write_ext
              && write_addr == 64'h0000_0000_0000_3600);
        complete_internal(3'd6);

        // External write completion must preserve the full captured address
        // used by the writer-local instruction-cache invalidation path.
        src_ext_req[4] = 1'b1;
        src_ext_addr[4*64 +: 64] = 64'hFEDC_BA98_7654_3210;
        src_ext_wen[4] = 1'b1;
        src_ext_wdata[4*512 +: 512] = {64{8'h44}};
        clock;
        src_ext_req[4] = 1'b0;
        src_ext_addr[4*64 +: 64] = 64'd0;
        src_ext_wen[4] = 1'b0;
        src_ext_wdata[4*512 +: 512] = 512'd0;
        clock;
        check("external write retains captured payload",
              ext_req && ext_wen
              && ext_addr == 64'hFEDC_BA98_7654_3210
              && ext_wdata == {64{8'h44}});
        accept_external(3'd4);
        repeat (3) begin
            clock;
            check("external owner remains stable through target wait",
                  ext_word_owner_valid && ext_word_owner == 3'd4);
        end
        ext_ack = 1'b1;
        #1;
        check("external write emits full commit metadata",
              write_commit && write_owner == 3'd4 && write_ext
              && write_addr == 64'hFEDC_BA98_7654_3210);
        complete_external(3'd4, 1'b1);

        // A pending cancellation completes locally, does not reach either
        // target, and does not consume a round-robin turn.
        src_tile_req[2] = 1'b1;
        src_tile_addr[2*32 +: 32] = 32'h0000_4200;
        clock;
        src_tile_req[2] = 1'b0;
        src_tile_req[3] = 1'b1;
        src_tile_addr[3*32 +: 32] = 32'h0000_4300;
        clock;
        src_tile_req[3] = 1'b0;
        // Owner 2 is active but not accepted; owner 3 is pending.  Cancel the
        // pending peer and ensure the active payload remains untouched.
        src_cancel[3] = 1'b1;
        clock;
        check("pending cancellation completes locally",
              src_cancel_done == 7'b0001000);
        check("pending cancellation never reaches a target",
              !tile_cancel && !ext_cancel);
        check("pending cancellation leaves current owner active",
              tile_req && tile_addr == 32'h0000_4200);
        src_cancel[3] = 1'b0;
        clock;
        accept_internal();
        complete_internal(3'd2);
        repeat (2) clock;
        check("canceled pending request is never issued",
              !tile_req && !ext_req);

        // Cancel before target acceptance.  The target REQ disappears in the
        // cancel cycle and no target CANCEL/drain is needed.
        src_ext_req[1] = 1'b1;
        src_ext_addr[1*64 +: 64] = 64'h0000_0001_0000_0100;
        clock;
        src_ext_req[1] = 1'b0;
        clock;
        check("pre-accept cancel probe reached external target", ext_req);
        src_cancel[1] = 1'b1;
        #1;
        check("pre-accept cancel suppresses target REQ immediately",
              !ext_req && !ext_cancel);
        clock;
        check("pre-accept cancel completes without target response",
              src_cancel_done == 7'b0000010);
        src_cancel[1] = 1'b0;
        clock;
        check("pre-accept canceled work leaves no target request",
              !tile_req && !ext_req);

        // A registered target ACCEPT can become visible in the same cycle as
        // the source cancellation.  That request has already crossed the
        // target boundary: assert target CANCEL immediately, retain ownership,
        // and withhold CANCEL_DONE until the terminal ACK arrives.
        src_ext_req[4] = 1'b1;
        src_ext_addr[4*64 +: 64] = 64'h0000_0002_0000_0400;
        clock;
        src_ext_req[4] = 1'b0;
        clock;
        check("accept-cancel race probe reached external target",
              ext_req && ext_addr == 64'h0000_0002_0000_0400);
        ext_accept = 1'b1;
        src_cancel[4] = 1'b1;
        #1;
        check("coincident ACCEPT and cancel starts target drain",
              ext_cancel && !ext_req && src_cancel_done == 0);
        check("coincident ACCEPT and cancel retains external owner",
              ext_word_owner_valid && ext_word_owner == 3'd4);
        clock;
        ext_accept = 1'b0;
        #1;
        check("accepted cancellation retains ownership before ACK",
              ext_cancel && src_cancel_done == 0);
        check("cancel drain keeps the accepted external owner stable",
              ext_word_owner_valid && ext_word_owner == 3'd4);
        ext_ack = 1'b1;
        #1;
        check("race-drain terminal ACK is suppressed from source",
              src_ext_ack == 0 && src_ext_error == 0);
        check("race-drain ACK cycle still identifies external owner",
              ext_word_owner_valid && ext_word_owner == 3'd4);
        clock;
        check("accept-cancel race completes only on terminal ACK",
              src_cancel_done == 7'b0010000);
        check("race-drain ACK retires external word ownership",
              !ext_word_owner_valid);
        ext_ack = 1'b0;
        src_cancel[4] = 1'b0;
        clock;
        check("accept-cancel race leaves arbiter idle",
              !tile_req && !ext_req);

        // Cancel after acceptance.  Hold cancellation until completion, queue
        // a fresh same-lane pulse during the drain, and present a stale error
        // ACK.  The stale result must be suppressed and the fresh pulse kept.
        src_ext_req[5] = 1'b1;
        src_ext_addr[5*64 +: 64] = 64'h0000_0002_0000_0200;
        src_ext_wen[5] = 1'b1;
        src_ext_wdata[5*512 +: 512] = {64{8'hA5}};
        clock;
        src_ext_req[5] = 1'b0;
        clock;
        accept_external(3'd5);
        src_cancel[5] = 1'b1;
        #1;
        check("post-accept cancel reaches selected target",
              ext_cancel && !tile_cancel && !ext_req);
        clock;
        check("cancel remains asserted while accepted work drains",
              ext_cancel && src_cancel_done == 0);
        check("post-accept cancel preserves external owner identity",
              ext_word_owner_valid && ext_word_owner == 3'd5);

        src_ext_req[5] = 1'b1;
        src_ext_addr[5*64 +: 64] = 64'h0000_0002_0000_0300;
        src_ext_wen[5] = 1'b0;
        #1;
        check("fresh same-lane pulse is accepted during cancel drain",
              src_accept == 7'b0100000);
        clock;
        src_ext_req[5] = 1'b0;
        src_cancel[5] = 1'b0;
        ext_error = 1'b1;
        ext_fault_addr = 64'h0000_0002_0000_0238;
        ext_ack = 1'b1;
        #1;
        check("stale canceled ACK and error are suppressed",
              src_ext_ack == 0 && src_ext_error == 0
              && src_ext_fault_addr == 0);
        check("canceled ACK cycle retains external owner identity",
              ext_word_owner_valid && ext_word_owner == 3'd5);
        check("canceled issued write still emits conservative commit",
              write_commit && write_owner == 3'd5 && write_ext
              && write_addr == 64'h0000_0002_0000_0200);
        clock;
        check("issued cancellation completes only after drain ACK",
              src_cancel_done == 7'b0100000);
        check("canceled terminal ACK retires external word ownership",
              !ext_word_owner_valid);
        ext_ack = 1'b0;
        ext_error = 1'b0;
        ext_fault_addr = 64'd0;
        clock;
        check("fresh pulse survives stale completion",
              ext_req && ext_addr == 64'h0000_0002_0000_0300
              && !ext_wen);
        accept_external(3'd5);
        complete_external(3'd5, 1'b0);

        // A normal target error is routed only to the captured source and
        // carries the exact 64-bit faulting address.
        src_tile_req[6] = 1'b1;
        src_tile_addr[6*32 +: 32] = 32'h0000_5000;
        src_tile_wen[6] = 1'b1;
        clock;
        src_tile_req[6] = 1'b0;
        src_tile_wen[6] = 1'b0;
        clock;
        accept_internal();
        tile_error = 1'b1;
        tile_fault_addr = 64'hABCD_EF01_0000_5038;
        tile_ack = 1'b1;
        #1;
        check("internal error ACK routes only to owner 6",
              src_tile_ack == 7'b1000000
              && src_tile_error == 7'b1000000
              && src_ext_ack == 0 && src_ext_error == 0);
        check("internal error routes exact packed fault address",
              src_tile_fault_addr[6*64 +: 64]
                  == 64'hABCD_EF01_0000_5038
              && src_tile_fault_addr[6*64-1:0] == 0);
        check("errored accepted write remains a conservative commit",
              write_commit && write_owner == 3'd6 && !write_ext
              && write_addr == 64'h0000_0000_0000_5000);
        clock;
        tile_ack = 1'b0;
        tile_error = 1'b0;
        tile_fault_addr = 64'd0;
        clock;

        // A target is permitted to accept and terminally acknowledge in one
        // cycle.  This path is required for immediate invalid-address errors.
        src_ext_req[0] = 1'b1;
        src_ext_addr[0 +: 64] = 64'hFFFF_FFFF_FFFF_FFC0;
        clock;
        src_ext_req[0] = 1'b0;
        clock;
        ext_accept = 1'b1;
        ext_ack = 1'b1;
        ext_error = 1'b1;
        ext_fault_addr = 64'hFFFF_FFFF_FFFF_FFC0;
        #1;
        check("same-cycle ACCEPT plus error ACK is delivered",
              ext_req && src_ext_ack == 7'b0000001
              && src_ext_error == 7'b0000001
              && src_ext_fault_addr[0 +: 64]
                  == 64'hFFFF_FFFF_FFFF_FFC0);
        check("same-cycle external terminal identifies captured owner",
              ext_word_owner_valid && ext_word_owner == 3'd0);
        clock;
        check("same-cycle terminal retires external word ownership",
              !ext_word_owner_valid);
        ext_accept = 1'b0;
        ext_ack = 1'b0;
        ext_error = 1'b0;
        ext_fault_addr = 64'd0;
        clock;

        $display("");
        if (fail_count == 0)
            $display("tb_tile_port_arbiter: ALL %0d assertions PASSED",
                     pass_count);
        else
            $display("tb_tile_port_arbiter: %0d PASSED, %0d FAILED",
                     pass_count, fail_count);

        if (fail_count != 0)
            $finish(1);
        $finish(0);
    end

    initial begin
        #200000;
        $display("tb_tile_port_arbiter: TIMEOUT");
        $finish(1);
    end

endmodule
