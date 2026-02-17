// ============================================================================
// tb_extmem.v — Testbench for mp64_extmem
// ============================================================================
//
// Negedge-driven methodology.
//
// Tests:
//   1. Reset clears outputs
//   2. CPU single-beat read
//   3. CPU single-beat write
//   4. Tile 8-beat burst read
//   5. Tile 8-beat burst write
//   6. Tile pre-empts CPU (tile priority)
//

`timescale 1ns/1ps

module tb_extmem;

    // ========================================================================
    // Parameters
    // ========================================================================
    localparam CLK_HALF = 5;

    // ========================================================================
    // Signals
    // ========================================================================
    reg          clk, rst_n;

    reg          cpu_req;
    reg  [31:0]  cpu_addr;
    reg  [63:0]  cpu_wdata;
    reg          cpu_wen;
    wire [63:0]  cpu_rdata;
    wire         cpu_ack;

    reg          tile_req;
    reg  [31:0]  tile_addr;
    reg  [511:0] tile_wdata;
    reg          tile_wen;
    wire [511:0] tile_rdata;
    wire         tile_ack;

    wire         phy_req;
    wire [31:0]  phy_addr;
    wire [63:0]  phy_wdata;
    wire         phy_wen;
    reg  [63:0]  phy_rdata;
    reg          phy_ack;
    wire [3:0]   phy_burst_len;

    // ========================================================================
    // Assertions
    // ========================================================================
    integer pass_count, fail_count, test_num;

    task check;
        input [511:0] label;
        input         cond;
        begin
            test_num = test_num + 1;
            if (cond) begin
                pass_count = pass_count + 1;
                $display("  [PASS %0d] %0s", test_num, label);
            end else begin
                fail_count = fail_count + 1;
                $display("  [FAIL %0d] %0s", test_num, label);
            end
        end
    endtask

    // ========================================================================
    // Clock
    // ========================================================================
    initial clk = 0;
    always #CLK_HALF clk = ~clk;

    // ========================================================================
    // DUT
    // ========================================================================
    mp64_extmem dut (
        .clk           (clk),
        .rst_n         (rst_n),
        .cpu_req       (cpu_req),
        .cpu_addr      (cpu_addr),
        .cpu_wdata     (cpu_wdata),
        .cpu_wen       (cpu_wen),
        .cpu_rdata     (cpu_rdata),
        .cpu_ack       (cpu_ack),
        .tile_req      (tile_req),
        .tile_addr     (tile_addr),
        .tile_wdata    (tile_wdata),
        .tile_wen      (tile_wen),
        .tile_rdata    (tile_rdata),
        .tile_ack      (tile_ack),
        .phy_req       (phy_req),
        .phy_addr      (phy_addr),
        .phy_wdata     (phy_wdata),
        .phy_wen       (phy_wen),
        .phy_rdata     (phy_rdata),
        .phy_ack       (phy_ack),
        .phy_burst_len (phy_burst_len)
    );

    // ========================================================================
    // Helper tasks
    // ========================================================================

    task clear_all;
        begin
            cpu_req    <= 1'b0;
            cpu_addr   <= 32'd0;
            cpu_wdata  <= 64'd0;
            cpu_wen    <= 1'b0;
            tile_req   <= 1'b0;
            tile_addr  <= 32'd0;
            tile_wdata <= 512'd0;
            tile_wen   <= 1'b0;
            phy_rdata  <= 64'd0;
            phy_ack    <= 1'b0;
        end
    endtask

    task reset;
        begin
            rst_n <= 1'b0;
            clear_all;
            @(negedge clk);
            @(negedge clk);
            @(negedge clk);
            rst_n <= 1'b1;
            @(negedge clk);
        end
    endtask

    // Wait for phy_req to go high
    task wait_phy_req;
        integer wd;
        begin
            wd = 0;
            while (!phy_req && wd < 50) begin
                @(posedge clk); #1;
                wd = wd + 1;
            end
        end
    endtask

    // ========================================================================
    // Main test sequence
    // ========================================================================
    initial begin
        $dumpfile("tb_extmem.vcd");
        $dumpvars(0, tb_extmem);

        pass_count = 0;
        fail_count = 0;
        test_num   = 0;

        $display("=== tb_extmem ===");

        // ---- Test 1: Reset ----
        $display("--- Test 1: Reset ---");
        reset;
        @(posedge clk); #1;
        check("phy_req clear after reset", phy_req  == 1'b0);
        check("cpu_ack clear after reset", cpu_ack  == 1'b0);
        check("tile_ack clear after reset",tile_ack == 1'b0);

        // ---- Test 2: CPU single-beat read ----
        $display("--- Test 2: CPU read ---");
        @(negedge clk);
        cpu_req   <= 1'b1;
        cpu_addr  <= 32'h8000_0100;
        cpu_wdata <= 64'd0;
        cpu_wen   <= 1'b0;

        wait_phy_req;
        check("phy_req for CPU read",      phy_req       == 1'b1);
        check("phy_burst_len is 1",        phy_burst_len == 4'd1);
        check("phy_addr correct",          phy_addr      == 32'h8000_0100);
        check("phy_wen is 0",              phy_wen       == 1'b0);

        // PHY responds
        @(negedge clk);
        phy_rdata <= 64'hDEAD_BEEF_1234_5678;
        phy_ack   <= 1'b1;
        @(posedge clk); #1;  // FSM processes phy_ack → cpu_ack=1
        check("cpu_ack asserted",  cpu_ack   == 1'b1);
        check("cpu_rdata correct", cpu_rdata == 64'hDEAD_BEEF_1234_5678);
        // Clear before next posedge to prevent re-entry
        @(negedge clk);
        clear_all;
        @(negedge clk);

        // ---- Test 3: CPU single-beat write ----
        $display("--- Test 3: CPU write ---");
        @(negedge clk);
        cpu_req   <= 1'b1;
        cpu_addr  <= 32'h8000_0200;
        cpu_wdata <= 64'hCAFE_BABE_0000_0001;
        cpu_wen   <= 1'b1;

        wait_phy_req;
        check("phy_req for CPU write", phy_req   == 1'b1);
        check("phy_wen is 1",         phy_wen   == 1'b1);
        check("phy_wdata correct",    phy_wdata == 64'hCAFE_BABE_0000_0001);

        @(negedge clk);
        phy_ack <= 1'b1;
        @(posedge clk); #1;
        check("cpu_ack for write", cpu_ack == 1'b1);
        @(negedge clk);
        clear_all;
        @(negedge clk);

        // ---- Test 4: Tile 8-beat burst read ----
        $display("--- Test 4: Tile burst read ---");
        @(negedge clk);
        tile_req  <= 1'b1;
        tile_addr <= 32'hA000_0000;
        tile_wen  <= 1'b0;

        wait_phy_req;
        check("phy_req for tile burst", phy_req       == 1'b1);
        check("phy_burst_len is 8",     phy_burst_len == 4'd8);

        // Feed 8 beats
        begin : blk_t4
            integer beat;
            for (beat = 0; beat < 8; beat = beat + 1) begin
                @(negedge clk);
                phy_rdata <= {32'hBEAD_0000, beat[15:0], 16'h0000};
                phy_ack   <= 1'b1;
                @(posedge clk); #1;
                @(negedge clk);
                phy_ack   <= 1'b0;
                if (beat == 7) tile_req <= 1'b0;
            end
        end

        // tile_ack was set at last beat's posedge, still valid now
        check("tile_ack after burst", tile_ack == 1'b1);
        check("tile_rdata beat 0", tile_rdata[0*64 +: 64] == {32'hBEAD_0000, 16'd0, 16'h0000});
        check("tile_rdata beat 7", tile_rdata[7*64 +: 64] == {32'hBEAD_0000, 16'd7, 16'h0000});

        @(negedge clk);
        clear_all;
        @(negedge clk);

        // ---- Test 5: Tile 8-beat burst write ----
        $display("--- Test 5: Tile burst write ---");
        begin : blk_t5
            reg [511:0] wpattern;
            integer i;
            wpattern = 512'd0;
            for (i = 0; i < 8; i = i + 1)
                wpattern[i*64 +: 64] = 64'hAA00_0000 + i;

            @(negedge clk);
            tile_req   <= 1'b1;
            tile_addr  <= 32'hB000_0000;
            tile_wdata <= wpattern;
            tile_wen   <= 1'b1;

            wait_phy_req;
            check("tile write phy_req",   phy_req       == 1'b1);
            check("tile write phy_wen",   phy_wen       == 1'b1);
            check("tile write burst len", phy_burst_len == 4'd8);
            check("tile write beat 0",    phy_wdata     == wpattern[0*64 +: 64]);

            // Ack all 8 beats
            begin : blk_t5b
                integer beat;
                for (beat = 0; beat < 8; beat = beat + 1) begin
                    @(negedge clk);
                    phy_ack <= 1'b1;
                    @(posedge clk); #1;
                    @(negedge clk);
                    phy_ack <= 1'b0;
                    if (beat == 7) tile_req <= 1'b0;
                end
            end

            check("tile write ack", tile_ack == 1'b1);
        end
        @(negedge clk);
        clear_all;
        @(negedge clk);

        // ---- Test 6: Tile pre-empts CPU ----
        $display("--- Test 6: Tile priority ---");
        @(negedge clk);
        cpu_req   <= 1'b1;
        cpu_addr  <= 32'h9000_0000;
        cpu_wen   <= 1'b0;
        tile_req  <= 1'b1;
        tile_addr <= 32'hC000_0000;
        tile_wen  <= 1'b0;

        wait_phy_req;
        check("tile gets priority", phy_burst_len == 4'd8);
        check("tile addr on phy",   phy_addr == 32'hC000_0000);

        // Ack the tile burst (8 beats)
        begin : blk_t6b
            integer beat;
            for (beat = 0; beat < 8; beat = beat + 1) begin
                @(negedge clk);
                phy_rdata <= 64'h7000 + beat;
                phy_ack   <= 1'b1;
                @(posedge clk); #1;
                @(negedge clk);
                phy_ack   <= 1'b0;
                if (beat == 7) tile_req <= 1'b0;
            end
        end

        check("tile ack in priority test", tile_ack == 1'b1);

        // Now CPU should get its turn
        wait_phy_req;
        check("CPU served after tile", phy_req == 1'b1 && phy_burst_len == 4'd1);

        @(negedge clk);
        phy_rdata <= 64'hC000_AF1E;
        phy_ack   <= 1'b1;
        @(posedge clk); #1;
        check("CPU ack after tile", cpu_ack == 1'b1);

        @(negedge clk);
        clear_all;
        @(negedge clk);

        // ================================================================
        // Summary
        // ================================================================
        $display("");
        $display("=== tb_extmem: %0d passed, %0d failed ===",
                 pass_count, fail_count);
        if (fail_count > 0)
            $display("*** FAILURES DETECTED ***");

        #100;
        $finish;
    end

    // Timeout
    initial begin
        #500000;
        $display("TIMEOUT");
        $finish;
    end

endmodule
