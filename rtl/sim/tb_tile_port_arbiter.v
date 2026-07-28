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

    reg [SOURCE_COUNT-1:0]     src_ext_req;
    reg [SOURCE_COUNT*64-1:0]  src_ext_addr;
    reg [SOURCE_COUNT-1:0]     src_ext_wen;
    reg [SOURCE_COUNT*512-1:0] src_ext_wdata;
    wire [SOURCE_COUNT-1:0]    src_ext_ack;

    wire         tile_req;
    wire [31:0]  tile_addr;
    wire         tile_wen;
    wire [511:0] tile_wdata;
    reg          tile_ack;

    wire         ext_req;
    wire [63:0]  ext_addr;
    wire         ext_wen;
    wire [511:0] ext_wdata;
    reg          ext_ack;

    wire         write_commit;
    wire [2:0]   write_owner;
    wire         write_ext;
    wire [63:0]  write_addr;

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
        .src_ext_req    (src_ext_req),
        .src_ext_addr   (src_ext_addr),
        .src_ext_wen    (src_ext_wen),
        .src_ext_wdata  (src_ext_wdata),
        .src_ext_ack    (src_ext_ack),
        .tile_req       (tile_req),
        .tile_addr      (tile_addr),
        .tile_wen       (tile_wen),
        .tile_wdata     (tile_wdata),
        .tile_ack       (tile_ack),
        .ext_req        (ext_req),
        .ext_addr       (ext_addr),
        .ext_wen        (ext_wen),
        .ext_wdata      (ext_wdata),
        .ext_ack        (ext_ack),
        .write_commit   (write_commit),
        .write_owner    (write_owner),
        .write_ext      (write_ext),
        .write_addr     (write_addr)
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

    task complete_internal;
        input [2:0] owner;
    begin
        tile_ack = 1'b1;
        #1;
        check("internal request suppressed during ACK", !tile_req);
        check("internal ACK routed only to captured owner",
              src_tile_ack
                  == ({{(SOURCE_COUNT-1){1'b0}}, 1'b1} << owner)
              && src_ext_ack == {SOURCE_COUNT{1'b0}});
        clock;
        tile_ack = 1'b0;
        clock;
    end
    endtask

    task complete_external;
        input [2:0] owner;
        input       expected_write;
    begin
        ext_ack = 1'b1;
        #1;
        check("external request suppressed during ACK", !ext_req);
        check("external ACK routed only to captured owner",
              src_ext_ack
                  == ({{(SOURCE_COUNT-1){1'b0}}, 1'b1} << owner)
              && src_tile_ack == {SOURCE_COUNT{1'b0}});
        check("external completion write classification",
              write_commit == expected_write);
        clock;
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
        tile_ack = 1'b0;
        ext_ack = 1'b0;

        repeat (2) clock;
        rst = 1'b0;
        clock;

        // A one-cycle source pulse must survive source payload changes and a
        // delayed target response.
        src_tile_addr[0 +: 32] = 32'h0000_1234;
        src_tile_wen[0] = 1'b1;
        src_tile_wdata[0 +: 512] = {8{64'hCAFE_BABE_DEAD_BEEF}};
        src_tile_req[0] = 1'b1;
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
        clock;
        src_tile_req = {SOURCE_COUNT{1'b0}};
        src_tile_wen = {SOURCE_COUNT{1'b0}};
        src_tile_addr = {(SOURCE_COUNT*32){1'b0}};
        src_tile_wdata = {(SOURCE_COUNT*512){1'b0}};
        clock;

        check("round-robin serves owner 1 after owner 0",
              tile_req && tile_addr == 32'h0000_2100
              && tile_wdata == {64{8'h11}});
        complete_internal(3'd1);
        check("round-robin next serves owner 2",
              tile_req && tile_addr == 32'h0000_2200
              && tile_wdata == {64{8'h12}});
        complete_internal(3'd2);
        check("round-robin next serves owner 3",
              tile_req && tile_addr == 32'h0000_2300
              && tile_wdata == {64{8'h13}});
        complete_internal(3'd3);
        check("round-robin next serves owner 4",
              tile_req && tile_addr == 32'h0000_2400
              && tile_wdata == {64{8'h14}});
        complete_internal(3'd4);
        check("round-robin next serves owner 5",
              tile_req && tile_addr == 32'h0000_2500
              && tile_wdata == {64{8'h15}});
        complete_internal(3'd5);
        check("round-robin next serves owner 6",
              tile_req && tile_addr == 32'h0000_2600
              && tile_wdata == {64{8'h16}});
        complete_internal(3'd6);
        check("round-robin wraps from owner 6 to owner 0",
              tile_req && tile_addr == 32'h0000_2000
              && tile_wdata == {64{8'h10}});
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
        complete_external(3'd5, 1'b0);
        check("busy-time internal pulse follows in RR order",
              tile_req && tile_addr == 32'h0000_3600
              && tile_wen && tile_wdata == {64{8'h66}});
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
        ext_ack = 1'b1;
        #1;
        check("external write emits full commit metadata",
              write_commit && write_owner == 3'd4 && write_ext
              && write_addr == 64'hFEDC_BA98_7654_3210);
        complete_external(3'd4, 1'b1);

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
