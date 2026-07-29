// ============================================================================
// tb_tacc_transfer.v — Shared canonical TACC transfer-stage contract
// ============================================================================

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_tacc_transfer;

    localparam integer SOURCE_COUNT = 7;
    localparam integer TOKEN_BITS   = 8;

    reg clk;
    reg rst;

    reg [SOURCE_COUNT-1:0] req;
    reg [SOURCE_COUNT-1:0] req_store;
    reg [SOURCE_COUNT-1:0] req_ext;
    reg [SOURCE_COUNT*64-1:0] req_base;
    reg [SOURCE_COUNT*3-1:0] req_format_ew;
    reg [SOURCE_COUNT*TOKEN_BITS-1:0] req_token;
    reg [SOURCE_COUNT*2048-1:0] req_store_image;
    reg [SOURCE_COUNT-1:0] req_cancel;
    reg [SOURCE_COUNT-1:0] finish;

    reg [SOURCE_COUNT-1:0] port_ack;
    reg [SOURCE_COUNT-1:0] port_error;
    reg [SOURCE_COUNT*64-1:0] port_fault_addr;
    reg [511:0] tile_rdata;
    reg [511:0] ext_rdata;
    reg [SOURCE_COUNT-1:0] port_cancel_done;

    wire [SOURCE_COUNT-1:0] beat_req;
    wire beat_ext;
    wire [63:0] beat_addr;
    wire beat_wen;
    wire [511:0] beat_wdata;
    wire [SOURCE_COUNT-1:0] port_cancel;
    wire [SOURCE_COUNT-1:0] stall_cycle;

    wire [SOURCE_COUNT-1:0] done;
    wire [SOURCE_COUNT*TOKEN_BITS-1:0] response_token;
    wire [SOURCE_COUNT*3-1:0] response_fault;
    wire [SOURCE_COUNT*64-1:0] response_fault_addr;
    wire [2047:0] result_image;

    reg [2047:0] source_image [0:SOURCE_COUNT-1];
    reg [2047:0] expected_image;
    reg [511:0] load_beat [0:3];

    integer pass_count;
    integer fail_count;
    integer source_index;
    integer beat_number;
    integer byte_index;

    mp64_tacc_transfer uut (
        .clk                 (clk),
        .rst                 (rst),
        .req                 (req),
        .req_store           (req_store),
        .req_ext             (req_ext),
        .req_base            (req_base),
        .req_format_ew       (req_format_ew),
        .req_token           (req_token),
        .req_store_image     (req_store_image),
        .req_cancel          (req_cancel),
        .finish              (finish),
        .port_ack            (port_ack),
        .port_error          (port_error),
        .port_fault_addr     (port_fault_addr),
        .tile_rdata          (tile_rdata),
        .ext_rdata           (ext_rdata),
        .port_cancel_done    (port_cancel_done),
        .beat_req            (beat_req),
        .beat_ext            (beat_ext),
        .beat_addr           (beat_addr),
        .beat_wen            (beat_wen),
        .beat_wdata          (beat_wdata),
        .port_cancel         (port_cancel),
        .stall_cycle         (stall_cycle),
        .done                (done),
        .response_token      (response_token),
        .response_fault      (response_fault),
        .response_fault_addr (response_fault_addr),
        .result_image        (result_image)
    );

    initial clk = 1'b0;
    always #5 clk = ~clk;

    task tick;
    begin
        @(posedge clk);
        #1;
    end
    endtask

    task check;
        input [1023:0] label;
        input          condition;
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

    task set_request;
        input integer source;
        input         store;
        input         external;
        input [63:0]  base;
        input [2:0]   format_ew;
        input [7:0]   token;
        input [2047:0] image;
    begin
        req_store[source] = store;
        req_ext[source] = external;
        req_base[source*64 +: 64] = base;
        req_format_ew[source*3 +: 3] = format_ew;
        req_token[source*TOKEN_BITS +: TOKEN_BITS] = token;
        req_store_image[source*2048 +: 2048] = image;
        req[source] = 1'b1;
    end
    endtask

    task expect_store_beat;
        input integer source;
        input integer index;
        input [63:0]  base;
        input         external;
        input [2047:0] image;
    begin
        check("STORE emits exactly the owner one-hot",
              beat_req == ({{(SOURCE_COUNT-1){1'b0}}, 1'b1} << source));
        check("STORE retains its captured route", beat_ext == external);
        check("STORE emits exact consecutive 64-byte address",
              beat_addr == base + index * 64);
        check("STORE emits write direction", beat_wen);
        check("STORE emits exact staged beat",
              beat_wdata == image[index*512 +: 512]);
    end
    endtask

    task capture_and_ack_store_beat;
        input integer source;
        input integer index;
        input [63:0]  base;
        input         external;
        input [2047:0] image;
    begin
        expect_store_beat(source, index, base, external, image);
        tick;
        check("one-cycle beat pulse drops while response is outstanding",
              beat_req == {SOURCE_COUNT{1'b0}});
        port_ack[source] = 1'b1;
        tick;
        port_ack[source] = 1'b0;
    end
    endtask

    task complete_store;
        input integer source;
        input [63:0] base;
        input         external;
        input [2047:0] image;
        input [7:0] token;
        integer index;
    begin
        req[source] = 1'b0;
        for (index = 0; index < 4; index = index + 1)
            capture_and_ack_store_beat(
                source, index, base, external, image
            );
        check("STORE publishes one held terminal response",
              done == ({{(SOURCE_COUNT-1){1'b0}}, 1'b1} << source));
        check("STORE response returns captured operation token",
              response_token[source*TOKEN_BITS +: TOKEN_BITS] == token);
        check("successful STORE reports no fault",
              response_fault[source*3 +: 3] == MEX_FAULT_NONE &&
              response_fault_addr[source*64 +: 64] == 64'd0);
        check("STORE never publishes a result image",
              result_image == 2048'd0);
        check("terminal STORE emits no fifth beat",
              beat_req == {SOURCE_COUNT{1'b0}});
    end
    endtask

    task finish_and_release;
        input integer source;
    begin
        finish[source] = 1'b1;
        tick;
        finish[source] = 1'b0;
        check("finish releases held response and image",
              done == {SOURCE_COUNT{1'b0}} &&
              result_image == 2048'd0);
        check("finish edge cannot regrant", beat_req == 0);
    end
    endtask

    task expect_load_beat;
        input integer source;
        input integer index;
        input [63:0]  base;
        input         external;
    begin
        check("LOAD emits exactly the owner one-hot",
              beat_req == ({{(SOURCE_COUNT-1){1'b0}}, 1'b1} << source));
        check("LOAD retains its captured route", beat_ext == external);
        check("LOAD emits exact consecutive 64-byte address",
              beat_addr == base + index * 64);
        check("LOAD emits read direction", !beat_wen);
        check("LOAD drives deterministic zero write data",
              beat_wdata == 512'd0);
    end
    endtask

    initial begin
        pass_count = 0;
        fail_count = 0;

        rst = 1'b1;
        req = {SOURCE_COUNT{1'b0}};
        req_store = {SOURCE_COUNT{1'b0}};
        req_ext = {SOURCE_COUNT{1'b0}};
        req_base = {(SOURCE_COUNT*64){1'b0}};
        req_format_ew = {(SOURCE_COUNT*3){1'b0}};
        req_token = {(SOURCE_COUNT*TOKEN_BITS){1'b0}};
        req_store_image = {(SOURCE_COUNT*2048){1'b0}};
        req_cancel = {SOURCE_COUNT{1'b0}};
        finish = {SOURCE_COUNT{1'b0}};
        port_ack = {SOURCE_COUNT{1'b0}};
        port_error = {SOURCE_COUNT{1'b0}};
        port_fault_addr = {(SOURCE_COUNT*64){1'b0}};
        tile_rdata = 512'd0;
        ext_rdata = 512'd0;
        port_cancel_done = {SOURCE_COUNT{1'b0}};
        expected_image = 2048'd0;
        for (source_index = 0;
             source_index < SOURCE_COUNT;
             source_index = source_index + 1) begin
            source_image[source_index] = 2048'd0;
            for (beat_number = 0;
                 beat_number < 4;
                 beat_number = beat_number + 1)
                for (byte_index = 0;
                     byte_index < 64;
                     byte_index = byte_index + 1)
                    source_image[source_index][
                        beat_number*512 + byte_index*8 +: 8
                    ] = source_index * 16 + beat_number + 1;
        end

        repeat (3) tick;
        rst = 1'b0;
        tick;

        check("reset clears all request and response outputs",
              beat_req == 0 && port_cancel == 0 && stall_cycle == 0
              && done == 0);
        check("reset hides and zeroizes the shared image",
              result_image == 2048'd0);

        // ------------------------------------------------------------------
        // Stall accounting is request-local and progress based. A held
        // request stalls while acquiring or waiting on the shared stage.
        // Only its acknowledged-beat cycle is progress; cancellation and the
        // held terminal response are excluded.
        // ------------------------------------------------------------------
        set_request(
            2, 1'b1, 1'b0, 64'h0000_0000_0000_0200,
            TMODE_8, 8'h22, source_image[2]
        );
        #1;
        check("held request stalls while acquiring the shared stage",
              stall_cycle == 7'b0000100);
        tick;
        check("admitted owner stalls while its beat awaits capture",
              beat_req == 7'b0000100 &&
              stall_cycle == 7'b0000100);

        set_request(
            5, 1'b1, 1'b1, 64'h0000_0001_0000_0500,
            TMODE_8, 8'h55, source_image[5]
        );
        #1;
        check("owner and waiting peer each report their own stall",
              stall_cycle == 7'b0100100);
        tick;
        repeat (2) begin
            tick;
            check("target wait remains a stall for owner and peer",
                  beat_req == 0 && stall_cycle == 7'b0100100);
        end

        // Complete all four owner beats while retaining both request levels.
        // Each ACK suppresses only the progressing owner's stall for that
        // cycle; the peer remains blocked by stage ownership.
        for (beat_number = 0;
             beat_number < 4;
             beat_number = beat_number + 1) begin
            port_ack[2] = 1'b1;
            #1;
            check("ACK progress clears only the owner's stall bit",
                  stall_cycle == 7'b0100000);
            tick;
            port_ack[2] = 1'b0;
            #1;
            if (beat_number != 3) begin
                check("held owner stalls again after ACK progress ends",
                      stall_cycle == 7'b0100100);
                check("next owner beat is emitted after successful ACK",
                      beat_req == 7'b0000100);
                tick;
            end
        end
        check("terminal done excludes held owner request from stalls",
              done == 7'b0000100 && stall_cycle == 7'b0100000);

        req[2] = 1'b0;
        finish[2] = 1'b1;
        tick;
        finish[2] = 1'b0;
        check("waiting peer remains stalled when prior response retires",
              done == 0 && stall_cycle == 7'b0100000);
        req_cancel[5] = 1'b1;
        #1;
        check("canceled held request does not report a stall",
              stall_cycle == 0);
        tick;
        req[5] = 1'b0;
        req_cancel[5] = 1'b0;
        tick;

        // Restore reset-era arbitration state for the ordering tests below.
        rst = 1'b1;
        repeat (2) tick;
        rst = 1'b0;
        tick;
        check("stall accounting clears across reset",
              stall_cycle == 0 && done == 0 && beat_req == 0);

        // ------------------------------------------------------------------
        // Seven simultaneous stores prove candidate-set RR order, reset
        // source-0 priority, four-beat tenure, and captured payload routing.
        // ------------------------------------------------------------------
        for (source_index = 0;
             source_index < SOURCE_COUNT;
             source_index = source_index + 1)
            set_request(
                source_index,
                1'b1,
                source_index[0],
                64'h0000_0000_0000_1000 + source_index * 256,
                TMODE_8,
                8'h40 + source_index,
                source_image[source_index]
            );

        tick;
        for (source_index = 0;
             source_index < SOURCE_COUNT;
             source_index = source_index + 1) begin
            check("equal RR admits expected source for full tenure",
                  beat_req ==
                    ({{(SOURCE_COUNT-1){1'b0}}, 1'b1} << source_index));
            complete_store(
                source_index,
                64'h0000_0000_0000_1000 + source_index * 256,
                source_index[0],
                source_image[source_index],
                8'h40 + source_index
            );
            finish_and_release(source_index);
            if (source_index != SOURCE_COUNT - 1)
                tick;
        end

        // ------------------------------------------------------------------
        // A 128-byte-active STORE snapshots the request payload and emits
        // canonical zero padding even if live token/data inputs later change.
        // ------------------------------------------------------------------
        source_image[2] = {
            {128{8'h99}},
            {128{8'h11}}
        };
        expected_image = {
            1024'd0,
            {128{8'h11}}
        };
        set_request(
            2, 1'b1, 1'b1, 64'h0000_0001_0000_2000,
            TMODE_32, 8'hA2, source_image[2]
        );
        tick;
        check("single contender is admitted after cyclic cursor",
              beat_req == 7'b0000100);
        req_token[2*TOKEN_BITS +: TOKEN_BITS] = 8'hEF;
        req_store_image[2*2048 +: 2048] = 2048'd0;
        complete_store(
            2, 64'h0000_0001_0000_2000, 1'b1,
            expected_image, 8'hA2
        );
        check("STORE token is captured rather than read live",
              response_token[2*TOKEN_BITS +: TOKEN_BITS] == 8'hA2);
        finish_and_release(2);

        // ------------------------------------------------------------------
        // Full-width LOAD assembly is invisible until the fourth successful
        // ACK, then remains stable with done until explicit finish.
        // ------------------------------------------------------------------
        load_beat[0] = {64{8'h31}};
        load_beat[1] = {64{8'h42}};
        load_beat[2] = {64{8'h53}};
        load_beat[3] = {64{8'h64}};
        expected_image = {
            load_beat[3], load_beat[2],
            load_beat[1], load_beat[0]
        };
        set_request(
            4, 1'b0, 1'b0, 64'h0000_0000_0000_4000,
            TMODE_16, 8'hB4, 2048'd0
        );
        tick;
        req[4] = 1'b0;
        for (beat_number = 0;
             beat_number < 4;
             beat_number = beat_number + 1) begin
            expect_load_beat(
                4, beat_number, 64'h0000_0000_0000_4000, 1'b0
            );
            check("partial LOAD image is not externally visible",
                  result_image == 2048'd0 && done == 0);
            tick;
            tile_rdata = load_beat[beat_number];
            port_ack[4] = 1'b1;
            tick;
            port_ack[4] = 1'b0;
        end
        check("fourth LOAD ACK atomically publishes complete image",
              done == 7'b0010000 && result_image == expected_image);
        check("LOAD returns exact captured token and no fault",
              response_token[4*TOKEN_BITS +: TOKEN_BITS] == 8'hB4 &&
              response_fault[4*3 +: 3] == MEX_FAULT_NONE);
        tick;
        tick;
        check("LOAD response and image remain held without finish",
              done == 7'b0010000 && result_image == expected_image);
        finish_and_release(4);

        // A 128-byte-active external LOAD still transfers four beats, but its
        // upper two incoming beats are ignored and commit as canonical zeros.
        load_beat[0] = {64{8'h71}};
        load_beat[1] = {64{8'h82}};
        load_beat[2] = {64{8'h93}};
        load_beat[3] = {64{8'hA4}};
        expected_image = {
            1024'd0, load_beat[1], load_beat[0]
        };
        set_request(
            5, 1'b0, 1'b1, 64'h0000_0001_0000_5000,
            TMODE_FP16, 8'hC5, 2048'd0
        );
        tick;
        req[5] = 1'b0;
        for (beat_number = 0;
             beat_number < 4;
             beat_number = beat_number + 1) begin
            expect_load_beat(
                5, beat_number, 64'h0000_0001_0000_5000, 1'b1
            );
            tick;
            ext_rdata = load_beat[beat_number];
            port_ack[5] = 1'b1;
            tick;
            port_ack[5] = 1'b0;
        end
        check("inactive external LOAD bytes commit as zeros",
              done == 7'b0100000 && result_image == expected_image);
        finish_and_release(5);

        // ------------------------------------------------------------------
        // A transport error terminates at the exact beat, routes its fault
        // address and token, exposes no partial LOAD, and emits no later beat.
        // ------------------------------------------------------------------
        set_request(
            6, 1'b0, 1'b1, 64'h0000_0001_0000_6000,
            TMODE_8, 8'hD6, 2048'd0
        );
        tick;
        req[6] = 1'b0;
        expect_load_beat(
            6, 0, 64'h0000_0001_0000_6000, 1'b1
        );
        tick;
        ext_rdata = {64{8'hCC}};
        port_ack[6] = 1'b1;
        tick;
        port_ack[6] = 1'b0;
        expect_load_beat(
            6, 1, 64'h0000_0001_0000_6000, 1'b1
        );
        tick;
        port_fault_addr[6*64 +: 64] =
            64'h0000_0001_0000_6048;
        port_error[6] = 1'b1;
        port_ack[6] = 1'b1;
        tick;
        port_ack[6] = 1'b0;
        port_error[6] = 1'b0;
        check("fault publishes owner response and captured token",
              done == 7'b1000000 &&
              response_token[6*TOKEN_BITS +: TOKEN_BITS] == 8'hD6);
        check("fault code and exact downstream address are routed",
              response_fault[6*3 +: 3] == MEX_FAULT_BUS &&
              response_fault_addr[6*64 +: 64] ==
                64'h0000_0001_0000_6048);
        check("failed LOAD hides partial image and issues no later beat",
              result_image == 2048'd0 && beat_req == 0);
        finish_and_release(6);

        // ------------------------------------------------------------------
        // A cancelled waiting request is consumed.  It cannot become eligible
        // when cancel drops until the request level itself has dropped.
        // ------------------------------------------------------------------
        source_image[0] = {256{8'h20}};
        source_image[1] = {256{8'h21}};
        set_request(
            0, 1'b1, 1'b0, 64'h7000,
            TMODE_8, 8'hE0, source_image[0]
        );
        set_request(
            1, 1'b1, 1'b0, 64'h7100,
            TMODE_8, 8'hE1, source_image[1]
        );
        tick;
        check("source zero owns stage while source one waits",
              beat_req == 7'b0000001);
        req[0] = 1'b0;
        req_cancel[1] = 1'b1;
        // Capture source zero's first beat while cancellation consumes the
        // waiting source-one request.
        expect_store_beat(
            0, 0, 64'h7000, 1'b0, source_image[0]
        );
        tick;
        req_cancel[1] = 1'b0;
        port_ack[0] = 1'b1;
        tick;
        port_ack[0] = 1'b0;
        for (beat_number = 1;
             beat_number < 4;
             beat_number = beat_number + 1)
            capture_and_ack_store_beat(
                0, beat_number, 64'h7000, 1'b0, source_image[0]
            );
        check("first transfer still completes after peer cancellation",
              done == 7'b0000001);
        finish_and_release(0);
        tick;
        check("cancelled held request cannot be granted later",
              beat_req == 0 && done == 0);

        // Dropping and reasserting creates a fresh source-one request.  Cancel
        // it before its first beat is captured; no port drain is required.
        req[1] = 1'b0;
        tick;
        set_request(
            1, 1'b1, 1'b0, 64'h7200,
            TMODE_8, 8'hE2, source_image[1]
        );
        tick;
        check("fresh level after drop is admitted",
              beat_req == 7'b0000010);
        req_cancel[1] = 1'b1;
        #1;
        check("pre-emission cancel suppresses beat immediately",
              beat_req == 0 && port_cancel == 0);
        tick;
        check("pre-emission cancel releases without terminal response",
              done == 0 && port_cancel == 0);
        req[1] = 1'b0;
        req_cancel[1] = 1'b0;
        tick;

        // After a beat pulse is emitted, cancellation is held to the port
        // until cancel_done.  A coincident stale ACK cannot complete work.
        source_image[3] = {256{8'h33}};
        set_request(
            3, 1'b1, 1'b1, 64'h0000_0001_0000_7300,
            TMODE_8, 8'hF3, source_image[3]
        );
        tick;
        req[3] = 1'b0;
        check("drain probe admits source three",
              beat_req == 7'b0001000);
        tick;
        req_cancel[3] = 1'b1;
        #1;
        check("in-flight cancel is routed one-hot to port",
              port_cancel == 7'b0001000 && done == 0);
        tick;
        req_cancel[3] = 1'b0;
        check("latched cancellation remains asserted while draining",
              port_cancel == 7'b0001000);
        port_ack[3] = 1'b1;
        tick;
        port_ack[3] = 1'b0;
        check("ACK during cancel drain cannot publish completion",
              port_cancel == 7'b0001000 && done == 0);
        port_cancel_done[3] = 1'b1;
        tick;
        port_cancel_done[3] = 1'b0;
        check("cancel_done releases stage without terminal response",
              port_cancel == 0 && done == 0 && result_image == 0);

        // A post-release stale ACK is ignored before any fresh replacement
        // request is presented.
        port_ack[3] = 1'b1;
        tick;
        port_ack[3] = 1'b0;
        check("post-cancel stale ACK is ignored", done == 0 && beat_req == 0);

        // Finally, prove a replacement token is returned rather than either
        // the cancelled token or later live input changes.
        set_request(
            3, 1'b1, 1'b0, 64'h7400,
            TMODE_8, 8'h34, source_image[3]
        );
        tick;
        req_token[3*TOKEN_BITS +: TOKEN_BITS] = 8'h99;
        complete_store(
            3, 64'h7400, 1'b0, source_image[3], 8'h34
        );
        check("fresh completion cannot fabricate stale/live token",
              response_token[3*TOKEN_BITS +: TOKEN_BITS] == 8'h34);
        finish_and_release(3);

        if (fail_count == 0) begin
            $display("");
            $display("TACC TRANSFER TESTS PASSED: %0d checks", pass_count);
        end else begin
            $display("");
            $display("TACC TRANSFER TESTS FAILED: %0d passed, %0d failed",
                     pass_count, fail_count);
            $fatal(1);
        end
        $finish;
    end

endmodule
