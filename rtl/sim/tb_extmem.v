// ============================================================================
// tb_extmem.v — Focused testbench for mp64_extmem
// ============================================================================
//
// Covers the held REQ/READY launch contract, independent terminal ACK/ERROR,
// eight-word tile serialization, retained payloads, cancellation/draining,
// stale-ACK quarantine, and both 255-cycle per-word timeout phases.
//

`timescale 1ns/1ps

module tb_extmem;
    localparam CLK_HALF = 5;

    reg          clk;
    reg          rst_n;

    reg          cpu_req;
    reg  [31:0]  cpu_addr;
    reg  [63:0]  cpu_wdata;
    reg          cpu_wen;
    reg  [1:0]   cpu_size;
    wire [63:0]  cpu_rdata;
    wire         cpu_ack;

    reg          tile_req;
    reg  [31:0]  tile_addr;
    reg  [511:0] tile_wdata;
    reg          tile_wen;
    reg          tile_cancel;
    wire         tile_accept;
    wire [511:0] tile_rdata;
    wire         tile_ack;
    wire         tile_error;
    wire [63:0]  tile_fault_addr;

    wire         phy_req;
    wire [31:0]  phy_addr;
    wire [63:0]  phy_wdata;
    wire         phy_wen;
    reg          phy_ready;
    reg  [63:0]  phy_rdata;
    reg          phy_ack;
    reg          phy_error;
    wire         phy_cancel;
    reg          phy_cancel_done;
    wire [3:0]   phy_burst_len;

    integer pass_count;
    integer fail_count;
    integer test_num;
    integer phy_launch_count;
    integer tile_accept_count;

    task check;
        input [8*96-1:0] label;
        input            condition;
        begin
            test_num = test_num + 1;
            if (condition) begin
                pass_count = pass_count + 1;
                $display("  [PASS %0d] %0s", test_num, label);
            end else begin
                fail_count = fail_count + 1;
                $display("  [FAIL %0d] %0s", test_num, label);
            end
        end
    endtask

    initial clk = 1'b0;
    always #CLK_HALF clk = ~clk;

    always @(posedge clk) begin
        if (!rst_n) begin
            phy_launch_count = 0;
            tile_accept_count = 0;
        end else begin
            if (phy_req && phy_ready)
                phy_launch_count = phy_launch_count + 1;
            if (tile_accept)
                tile_accept_count = tile_accept_count + 1;
        end
    end

    mp64_extmem dut (
        .clk             (clk),
        .rst_n           (rst_n),
        .cpu_req         (cpu_req),
        .cpu_addr        (cpu_addr),
        .cpu_wdata       (cpu_wdata),
        .cpu_wen         (cpu_wen),
        .cpu_size        (cpu_size),
        .cpu_rdata       (cpu_rdata),
        .cpu_ack         (cpu_ack),
        .tile_req        (tile_req),
        .tile_addr       (tile_addr),
        .tile_wdata      (tile_wdata),
        .tile_wen        (tile_wen),
        .tile_cancel     (tile_cancel),
        .tile_accept     (tile_accept),
        .tile_rdata      (tile_rdata),
        .tile_ack        (tile_ack),
        .tile_error      (tile_error),
        .tile_fault_addr (tile_fault_addr),
        .phy_req         (phy_req),
        .phy_addr        (phy_addr),
        .phy_wdata       (phy_wdata),
        .phy_wen         (phy_wen),
        .phy_ready       (phy_ready),
        .phy_rdata       (phy_rdata),
        .phy_ack         (phy_ack),
        .phy_error       (phy_error),
        .phy_cancel      (phy_cancel),
        .phy_cancel_done (phy_cancel_done),
        .phy_burst_len   (phy_burst_len)
    );

    task clear_inputs;
        begin
            cpu_req     <= 1'b0;
            cpu_addr    <= 32'd0;
            cpu_wdata   <= 64'd0;
            cpu_wen     <= 1'b0;
            cpu_size    <= 2'd3;
            tile_req    <= 1'b0;
            tile_addr   <= 32'd0;
            tile_wdata  <= 512'd0;
            tile_wen    <= 1'b0;
            tile_cancel <= 1'b0;
            phy_ready   <= 1'b0;
            phy_rdata   <= 64'd0;
            phy_ack     <= 1'b0;
            phy_error   <= 1'b0;
            phy_cancel_done <= 1'b1;
        end
    endtask

    task reset;
        begin
            rst_n <= 1'b0;
            clear_inputs;
            repeat (3) @(negedge clk);
            rst_n <= 1'b1;
            // The first active edge consumes the reset flush handshake.
            @(posedge clk); #1;
            @(negedge clk);
            phy_cancel_done <= 1'b0;
        end
    endtask

    task wait_phy_req;
        integer watchdog;
        begin
            watchdog = 0;
            while (!phy_req && watchdog < 300) begin
                @(posedge clk); #1;
                watchdog = watchdog + 1;
            end
            check("PHY request arrived before watchdog", phy_req);
        end
    endtask

    task wait_tile_accept;
        integer watchdog;
        begin
            watchdog = 0;
            while (!tile_accept && watchdog < 20) begin
                @(posedge clk); #1;
                watchdog = watchdog + 1;
            end
            check("tile request received explicit accept", tile_accept);
        end
    endtask

    task launch_current_word;
        begin
            @(negedge clk);
            phy_ready <= 1'b1;
            @(posedge clk); #1;
            check("PHY request drops after ready launch", !phy_req);
            @(negedge clk);
            phy_ready <= 1'b0;
        end
    endtask

    task respond_current_word;
        input [63:0] data;
        input        error;
        begin
            @(negedge clk);
            phy_rdata <= data;
            phy_error <= error;
            phy_ack   <= 1'b1;
            @(posedge clk); #1;
            @(negedge clk);
            phy_ack   <= 1'b0;
            phy_error <= 1'b0;
        end
    endtask

    reg [511:0] expected_image;
    reg [511:0] write_image;
    integer beat;

    initial begin
        $dumpfile("tb_extmem.vcd");
        $dumpvars(0, tb_extmem);

        pass_count = 0;
        fail_count = 0;
        test_num   = 0;
        rst_n      = 1'b0;
        clear_inputs;

        $display("=== tb_extmem ===");

        // --------------------------------------------------------------------
        $display("--- Reset contract ---");
        reset;
        @(posedge clk); #1;
        check("PHY request clear after reset", !phy_req);
        check("reset flush completed before target reopened", !phy_cancel);
        check("CPU ACK clear after reset", !cpu_ack);
        check("tile accept/ACK/error clear after reset",
              !tile_accept && !tile_ack && !tile_error);

        // --------------------------------------------------------------------
        $display("--- CPU read: held launch payload and terminal response ---");
        @(negedge clk);
        cpu_req   <= 1'b1;
        cpu_addr  <= 32'h8000_0103;
        cpu_wen   <= 1'b0;
        cpu_size  <= 2'd0;
        wait_phy_req;
        check("CPU read aligns the physical word",
              phy_addr == 32'h8000_0100);
        check("CPU read is one physical word",
              !phy_wen && phy_burst_len == 4'd1);
        repeat (3) begin
            @(posedge clk); #1;
            check("PHY request and payload hold while ready is low",
                  phy_req && phy_addr == 32'h8000_0100 && !phy_wen);
        end
        launch_current_word;
        check("CPU does not ACK at launch", !cpu_ack);
        respond_current_word(64'hDEAD_BEEF_1234_5678, 1'b0);
        check("CPU read ACKs only on terminal PHY response", cpu_ack);
        check("CPU read returns PHY data",
              cpu_rdata == 64'hDEAD_BEEF_1234_5678);
        repeat (3) begin
            @(posedge clk); #1;
        end
        check("held CPU request is not relaunched", phy_launch_count == 1);
        @(negedge clk);
        cpu_req <= 1'b0;
        @(posedge clk); #1;

        // --------------------------------------------------------------------
        $display("--- CPU subword RMW uses two independent PHY words ---");
        @(negedge clk);
        cpu_req   <= 1'b1;
        cpu_addr  <= 32'h8000_0203;
        cpu_wdata <= 64'h0000_0000_0000_00AA;
        cpu_wen   <= 1'b1;
        cpu_size  <= 2'd0;
        wait_phy_req;
        check("subword write first launches a read",
              !phy_wen && phy_addr == 32'h8000_0200);
        launch_current_word;
        respond_current_word(64'h1122_3344_5566_7788, 1'b0);
        check("RMW read does not complete CPU request", !cpu_ack);
        wait_phy_req;
        check("RMW write is a new aligned launch",
              phy_wen && phy_addr == 32'h8000_0200);
        check("RMW write preserves untouched byte lanes",
              phy_wdata == 64'h1122_3344_AA66_7788);
        launch_current_word;
        respond_current_word(64'd0, 1'b0);
        check("CPU ACK waits for RMW write response", cpu_ack);
        check("RMW performed exactly two launches", phy_launch_count == 3);
        @(negedge clk);
        cpu_req <= 1'b0;
        cpu_wen <= 1'b0;
        @(posedge clk); #1;

        // --------------------------------------------------------------------
        $display("--- Stale high PHY ACK is quarantined before launch ---");
        @(negedge clk);
        phy_ack  <= 1'b1;
        cpu_req  <= 1'b1;
        cpu_addr <= 32'h8000_0300;
        cpu_wen  <= 1'b0;
        repeat (4) begin
            @(posedge clk); #1;
        end
        check("stale ACK cannot launch or complete the new request",
              !phy_req && !cpu_ack && phy_launch_count == 3);
        @(negedge clk);
        phy_ack <= 1'b0;
        wait_phy_req;
        launch_current_word;
        respond_current_word(64'hA55A_5AA5_0102_0304, 1'b0);
        check("request recovers after stale ACK drains",
              cpu_ack && cpu_rdata == 64'hA55A_5AA5_0102_0304);
        @(negedge clk);
        cpu_req <= 1'b0;
        @(posedge clk); #1;

        // --------------------------------------------------------------------
        $display("--- CPU PHY error terminates through documented seam ---");
        @(negedge clk);
        cpu_req  <= 1'b1;
        cpu_addr <= 32'h8000_0400;
        wait_phy_req;
        launch_current_word;
        respond_current_word(64'hFFFF_FFFF_FFFF_FFFF, 1'b1);
        check("CPU PHY error cannot deadlock the CPU bus", cpu_ack);
        check("CPU PHY error returns deterministic zero data", cpu_rdata == 0);
        @(negedge clk);
        cpu_req <= 1'b0;
        @(posedge clk); #1;

        // --------------------------------------------------------------------
        $display("--- Tile read: retained request and eight serialized words ---");
        expected_image = 512'd0;
        for (beat = 0; beat < 8; beat = beat + 1)
            expected_image[beat*64 +: 64] =
                64'hB100_0000_0000_0000 + beat;
        @(negedge clk);
        tile_req  <= 1'b1;
        tile_addr <= 32'hA000_0040;
        tile_wen  <= 1'b0;
        wait_tile_accept;
        @(negedge clk);
        tile_req   <= 1'b0;
        tile_addr  <= 32'hDEAD_DEAD;
        tile_wdata <= {8{64'hBAD0_BAD0_BAD0_BAD0}};

        for (beat = 0; beat < 8; beat = beat + 1) begin
            wait_phy_req;
            check("tile read word address advances by eight bytes",
                  phy_addr == (32'hA000_0040 + beat*8));
            check("tile read launches one-word PHY requests",
                  !phy_wen && phy_burst_len == 4'd1);
            launch_current_word;
            respond_current_word(expected_image[beat*64 +: 64], 1'b0);
            if (beat != 7)
                check("tile ACK waits for all eight words", !tile_ack);
        end
        check("tile read returns one terminal success ACK",
              tile_ack && !tile_error);
        check("tile read assembles little-endian word order",
              tile_rdata == expected_image);
        check("tile request is accepted once", tile_accept_count == 1);
        check("tile read launches exactly eight physical words",
              phy_launch_count == 13);
        @(posedge clk); #1;

        // --------------------------------------------------------------------
        $display("--- Tile write retains all 512 payload bits after accept ---");
        write_image = 512'd0;
        for (beat = 0; beat < 8; beat = beat + 1)
            write_image[beat*64 +: 64] =
                64'hC200_0000_0000_0000 + beat;
        @(negedge clk);
        tile_req   <= 1'b1;
        tile_addr  <= 32'hB000_0080;
        tile_wdata <= write_image;
        tile_wen   <= 1'b1;
        wait_tile_accept;
        @(negedge clk);
        tile_req   <= 1'b0;
        tile_addr  <= 32'd0;
        tile_wdata <= 512'd0;
        tile_wen   <= 1'b0;

        for (beat = 0; beat < 8; beat = beat + 1) begin
            wait_phy_req;
            check("tile write word address advances by eight bytes",
                  phy_addr == (32'hB000_0080 + beat*8));
            check("tile write uses retained word payload",
                  phy_wen &&
                  phy_wdata == write_image[beat*64 +: 64]);
            launch_current_word;
            respond_current_word(64'd0, 1'b0);
        end
        check("tile write returns terminal success", tile_ack && !tile_error);
        check("second tile command receives one accept", tile_accept_count == 2);
        @(posedge clk); #1;

        // --------------------------------------------------------------------
        $display("--- Tile PHY error stops at exact word and keeps prefix ---");
        write_image = {8{64'hD300_D300_D300_D300}};
        @(negedge clk);
        tile_req   <= 1'b1;
        tile_addr  <= 32'hC000_0100;
        tile_wdata <= write_image;
        tile_wen   <= 1'b1;
        wait_tile_accept;
        @(negedge clk);
        tile_req <= 1'b0;

        for (beat = 0; beat < 3; beat = beat + 1) begin
            wait_phy_req;
            check("faulting write exposes expected current word",
                  phy_addr == (32'hC000_0100 + beat*8));
            launch_current_word;
            respond_current_word(64'd0, beat == 2);
        end
        check("PHY error becomes terminal tile error",
              tile_ack && tile_error);
        check("PHY error reports exact physical word address",
              tile_fault_addr == 64'h0000_0000_C000_0110);
        repeat (4) begin
            @(posedge clk); #1;
        end
        check("no word after the fault is launched",
              phy_launch_count == 24);

        // --------------------------------------------------------------------
        $display("--- Tile cancel before launch issues no physical word ---");
        begin : cancel_before_launch
            integer launches_before;
            launches_before = phy_launch_count;
            @(negedge clk);
            tile_req    <= 1'b1;
            tile_addr   <= 32'hD000_0000;
            tile_wen    <= 1'b0;
            tile_cancel <= 1'b0;
            wait_tile_accept;
            @(negedge clk);
            tile_req    <= 1'b0;
            tile_cancel <= 1'b1;
            @(posedge clk); #1;
            check("pre-launch cancel returns a terminal target ACK",
                  tile_ack && !tile_error);
            check("pre-launch cancel never reaches PHY",
                  phy_launch_count == launches_before && !phy_req);
            @(negedge clk);
            tile_cancel <= 1'b0;
            @(posedge clk); #1;
        end

        // --------------------------------------------------------------------
        $display("--- Tile cancel after launch drains current response ---");
        begin : cancel_after_launch
            integer launches_before;
            launches_before = phy_launch_count;
            @(negedge clk);
            tile_req    <= 1'b1;
            tile_addr   <= 32'hD100_0000;
            tile_wen    <= 1'b0;
            tile_cancel <= 1'b0;
            wait_tile_accept;
            @(negedge clk);
            tile_req <= 1'b0;
            wait_phy_req;
            launch_current_word;
            @(negedge clk);
            tile_cancel <= 1'b1;
            @(posedge clk); #1;
            check("post-launch cancel waits for terminal response", !tile_ack);
            @(negedge clk);
            tile_cancel <= 1'b0;
            repeat (3) begin
                @(posedge clk); #1;
            end
            check("canceled accepted word remains in drain state", !tile_ack);
            respond_current_word(64'h1111_2222_3333_4444, 1'b0);
            check("drained canceled word returns target-only ACK",
                  tile_ack && !tile_error);
            repeat (4) begin
                @(posedge clk); #1;
            end
            check("post-launch cancel suppresses all later words",
                  phy_launch_count == launches_before + 1);
        end

        // --------------------------------------------------------------------
        $display("--- Launch timeout is bounded and reports word address ---");
        reset;
        @(negedge clk);
        tile_req  <= 1'b1;
        tile_addr <= 32'hE000_0200;
        tile_wen  <= 1'b0;
        wait_tile_accept;
        @(negedge clk);
        tile_req <= 1'b0;
        begin : launch_timeout_window
            integer cycle;
            reg early_ack;
            early_ack = 1'b0;
            for (cycle = 0; cycle < 254; cycle = cycle + 1) begin
                @(posedge clk); #1;
                if (tile_ack)
                    early_ack = 1'b1;
            end
            check("launch timeout does not fire before cycle 255", !early_ack);
            @(posedge clk); #1;
            check("unaccepted launch times out on cycle 255",
                  tile_ack && tile_error);
            check("launch timeout reports exact first word",
                  tile_fault_addr == 64'h0000_0000_E000_0200);
        end
        @(posedge clk); #1;

        // --------------------------------------------------------------------
        $display("--- Response ACK on cycle 255 wins over timeout ---");
        reset;
        @(negedge clk);
        tile_req  <= 1'b1;
        tile_addr <= 32'hE100_0300;
        tile_wen  <= 1'b0;
        wait_tile_accept;
        @(negedge clk);
        tile_req <= 1'b0;
        wait_phy_req;
        launch_current_word;
        begin : response_deadline_win
            integer cycle;
            reg early_ack;
            early_ack = 1'b0;
            for (cycle = 0; cycle < 254; cycle = cycle + 1) begin
                @(posedge clk); #1;
                if (tile_ack)
                    early_ack = 1'b1;
            end
            check("response timeout waits through cycle 254", !early_ack);
            @(negedge clk);
            phy_rdata <= 64'hF00D_F00D_F00D_F00D;
            phy_ack   <= 1'b1;
            @(posedge clk); #1;
            check("ACK on response cycle 255 beats timeout",
                  !tile_ack && !tile_error &&
                  tile_rdata[63:0] == 64'hF00D_F00D_F00D_F00D);
            @(negedge clk);
            phy_ack     <= 1'b0;
            tile_cancel <= 1'b1;
            @(posedge clk); #1;
            check("deadline-win transfer can cancel before its next word",
                  tile_ack && !tile_error);
            @(negedge clk);
            tile_cancel <= 1'b0;
            @(posedge clk); #1;
        end

        // --------------------------------------------------------------------
        $display("--- Accepted response timeout is exact and terminal ---");
        reset;
        @(negedge clk);
        tile_req  <= 1'b1;
        tile_addr <= 32'hE200_0400;
        tile_wen  <= 1'b0;
        wait_tile_accept;
        @(negedge clk);
        tile_req <= 1'b0;
        wait_phy_req;
        launch_current_word;
        begin : response_timeout_window
            integer cycle;
            reg early_ack;
            early_ack = 1'b0;
            for (cycle = 0; cycle < 254; cycle = cycle + 1) begin
                @(posedge clk); #1;
                if (tile_ack)
                    early_ack = 1'b1;
            end
            check("response timeout does not fire before cycle 255",
                  !early_ack);
            @(posedge clk); #1;
            check("missing response times out on cycle 255",
                  tile_ack && tile_error);
            check("response timeout reports accepted word address",
                  tile_fault_addr == 64'h0000_0000_E200_0400);
            check("accepted-word timeout closes PHY epoch for flush",
                  phy_cancel);
        end

        // --------------------------------------------------------------------
        $display("--- Late timed-out ACK is suppressed before successful reuse ---");
        @(negedge clk);
        tile_req   <= 1'b1;
        tile_addr  <= 32'hE300_0500;
        tile_wen   <= 1'b0;
        phy_rdata  <= 64'hDEAD_DEAD_DEAD_DEAD;
        phy_ack    <= 1'b1;
        repeat (3) begin
            @(posedge clk); #1;
            check("flush barrier ignores late ACK and blocks new accept",
                  phy_cancel && !phy_req && !tile_accept && !tile_ack);
        end

        // DONE is asserted only after the PHY has discarded the timed-out
        // epoch and driven ACK low.  The held replacement may then proceed.
        @(negedge clk);
        phy_ack         <= 1'b0;
        phy_cancel_done <= 1'b1;
        @(posedge clk); #1;
        check("PHY cancel completion reopens the controller",
              !phy_cancel && !tile_accept);
        @(negedge clk);
        phy_cancel_done <= 1'b0;
        @(posedge clk); #1;
        check("held replacement is accepted after flush", tile_accept);
        @(negedge clk);
        tile_req <= 1'b0;
        wait_phy_req;
        check("replacement keeps its own post-flush address",
              phy_addr == 32'hE300_0500);
        launch_current_word;
        respond_current_word(64'hACCE_5510_ACCE_5510, 1'b0);
        check("late ACK cannot contaminate replacement read data",
              tile_rdata[63:0] == 64'hACCE_5510_ACCE_5510);
        tile_cancel <= 1'b1;
        @(posedge clk); #1;
        check("replacement can retire normally after reuse",
              tile_ack && !tile_error);
        @(negedge clk);
        tile_cancel <= 1'b0;
        @(posedge clk); #1;

        // --------------------------------------------------------------------
        $display("--- Reset flushes an accepted PHY epoch before reuse ---");
        @(negedge clk);
        tile_req   <= 1'b1;
        tile_addr  <= 32'hE400_0600;
        tile_wen   <= 1'b0;
        wait_tile_accept;
        @(negedge clk);
        tile_req <= 1'b0;
        wait_phy_req;
        launch_current_word;

        // Reset while the accepted word is awaiting its response.
        @(negedge clk);
        rst_n           <= 1'b0;
        phy_cancel_done <= 1'b0;
        @(posedge clk); #1;
        check("reset asserts PHY cancel and suppresses request",
              phy_cancel && !phy_req);

        // A reset-era tardy response is ignored.  A fresh request held across
        // reset release remains blocked until the explicit flush completion.
        @(negedge clk);
        phy_ack   <= 1'b1;
        phy_rdata <= 64'hBAD0_BAD0_BAD0_BAD0;
        tile_req  <= 1'b1;
        tile_addr <= 32'hE500_0700;
        @(posedge clk); #1;
        check("reset ignores stale response while cancel remains asserted",
              phy_cancel && !tile_accept);
        @(negedge clk);
        rst_n <= 1'b1;
        @(posedge clk); #1;
        check("reset release cannot bypass incomplete PHY flush",
              phy_cancel && !tile_accept && !phy_req);

        @(negedge clk);
        phy_ack         <= 1'b0;
        phy_cancel_done <= 1'b1;
        @(posedge clk); #1;
        check("reset flush completion releases quarantine", !phy_cancel);
        @(negedge clk);
        phy_cancel_done <= 1'b0;
        @(posedge clk); #1;
        check("fresh request is accepted only after reset flush",
              tile_accept);
        @(negedge clk);
        tile_req <= 1'b0;
        wait_phy_req;
        check("post-reset request retains its own address",
              phy_addr == 32'hE500_0700);
        launch_current_word;
        respond_current_word(64'h5151_5252_5353_5454, 1'b0);
        check("post-reset response is not stale reset-era data",
              tile_rdata[63:0] == 64'h5151_5252_5353_5454);
        tile_cancel <= 1'b1;
        @(posedge clk); #1;
        check("post-reset transfer remains usable", tile_ack && !tile_error);
        @(negedge clk);
        tile_cancel <= 1'b0;
        @(posedge clk); #1;

        // --------------------------------------------------------------------
        $display("");
        $display("=== tb_extmem: %0d passed, %0d failed ===",
                 pass_count, fail_count);
        if (fail_count > 0)
            $fatal(1, "tb_extmem failures detected");

        #100;
        $finish;
    end

    initial begin
        #2000000;
        $fatal(1, "tb_extmem timeout");
    end
endmodule
