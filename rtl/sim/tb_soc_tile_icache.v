// ============================================================================
// tb_soc_tile_icache.v — Paired tile-write/I-cache integration contract
// ============================================================================

`timescale 1ns / 1ps

module tb_soc_tile_icache;

    reg sys_clk;
    reg sys_rst_n;

    wire        uart_txd;
    wire        phy_req;
    wire [63:0] phy_addr;
    wire        phy_wen;
    wire [63:0] phy_wdata;
    wire [7:0]  phy_burst_len;
    wire        sd_sck;
    wire        sd_mosi;
    wire        sd_cs_n;
    wire        nic_tx_valid;
    wire [7:0]  nic_tx_data;
    wire        nic_rx_ready;
    wire [7:0]  debug_leds;

    integer pass_count;
    integer fail_count;
    integer guard;
    integer c0_write_accepts;
    integer c1_write_accepts;
    integer cluster_write_accepts;

    mp64_soc #(
        .MEM_DEPTH (16)
    ) u_soc (
        .sys_clk            (sys_clk),
        .sys_rst_n          (sys_rst_n),
        .uart_rxd           (1'b1),
        .uart_txd           (uart_txd),
        .phy_req            (phy_req),
        .phy_addr           (phy_addr),
        .phy_wen            (phy_wen),
        .phy_wdata          (phy_wdata),
        .phy_burst_len      (phy_burst_len),
        .phy_rdata          (64'd0),
        .phy_rvalid         (1'b0),
        .phy_ready          (1'b1),
        .sd_sck             (sd_sck),
        .sd_mosi            (sd_mosi),
        .sd_miso            (1'b1),
        .sd_cs_n            (sd_cs_n),
        .sd_card_present    (1'b1),
        .sd_write_protected (1'b0),
        .nic_tx_valid       (nic_tx_valid),
        .nic_tx_data        (nic_tx_data),
        .nic_tx_ready       (1'b1),
        .nic_rx_valid       (1'b0),
        .nic_rx_data        (8'd0),
        .nic_rx_ready       (nic_rx_ready),
        .nic_link_up        (1'b0),
        .debug_leds         (debug_leds)
    );

    initial sys_clk = 1'b0;
    always #5 sys_clk = ~sys_clk;

    task clock;
    begin
        @(posedge sys_clk);
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

    always @(posedge sys_clk) begin
        if (!sys_rst_n) begin
            c0_write_accepts <= 0;
            c1_write_accepts <= 0;
            cluster_write_accepts <= 0;
        end else if (u_soc.u_memory.tile_start && u_soc.tile_mem_wen) begin
            if (u_soc.tile_mem_addr == 32'h0000_0123)
                c0_write_accepts <= c0_write_accepts + 1;
            else if (u_soc.tile_mem_addr == 32'h0000_0323)
                c1_write_accepts <= c1_write_accepts + 1;
            else if (u_soc.tile_mem_addr == 32'h0000_0223)
                cluster_write_accepts <= cluster_write_accepts + 1;
        end
    end

    initial begin
        pass_count = 0;
        fail_count = 0;
        c0_write_accepts = 0;
        c1_write_accepts = 0;
        cluster_write_accepts = 0;
        sys_rst_n = 1'b0;
        repeat (4) clock;

        // Keep unrelated producers quiescent.  The test injects the exact
        // one-cycle source pulses at the integrated arbiter boundary.
        force u_soc.cpu_icache_req[0] = 1'b0;
        force u_soc.cpu_icache_inv_all[0] = 1'b0;
        force u_soc.cpu_icache_inv_line[0] = 1'b0;
        force u_soc.core_tile_req[0] = 1'b0;
        force u_soc.core_tile_req[1] = 1'b0;
        force u_soc.core_tile_req[2] = 1'b0;
        force u_soc.core_tile_req[3] = 1'b0;
        force u_soc.core_ext_tile_req[0] = 1'b0;
        force u_soc.core_ext_tile_req[1] = 1'b0;
        force u_soc.core_ext_tile_req[2] = 1'b0;
        force u_soc.core_ext_tile_req[3] = 1'b0;
        force u_soc.cluster_tile_req[0] = 1'b0;
        force u_soc.cluster_tile_req[1] = 1'b0;
        force u_soc.cluster_tile_req[2] = 1'b0;
        force u_soc.cluster_ext_tile_req[0] = 1'b0;
        force u_soc.cluster_ext_tile_req[1] = 1'b0;
        force u_soc.cluster_ext_tile_req[2] = 1'b0;

        sys_rst_n = 1'b1;
        repeat (3) clock;

        // Prime the private cache line containing internal tile address 0x123.
        // Internal tile RAM ignores bits [5:0], so completion must invalidate
        // the 64-byte-aligned physical span beginning at 0x100.
        @(negedge sys_clk);
        u_soc.g_core[0].u_icache.valid[8'h10] = 1'b1;
        u_soc.g_core[0].u_icache.tags[8'h10] = 64'h0000_0100;
        #1;
        check("core-0 cache line primed",
              u_soc.g_core[0].u_icache.valid[8'h10]);

        force u_soc.core_tile_addr[0] = 32'h0000_0123;
        force u_soc.core_tile_wen[0] = 1'b1;
        force u_soc.core_tile_wdata[0] = {64{8'hA5}};
        force u_soc.core_tile_req[0] = 1'b1;
        clock;
        force u_soc.core_tile_req[0] = 1'b0;

        guard = 0;
        while (!u_soc.core_tile_icache_inv_line[0]) begin
            clock;
            guard = guard + 1;
            if (guard > 30) begin
                $display("  FAIL: timeout waiting for core-0 tile commit");
                fail_count = fail_count + 1;
                $finish(1);
            end
        end
        check("core-0 tile commit invalidates aligned 64-byte span",
              u_soc.core_tile_icache_inv_addr[0] == 64'h0000_0100
              && u_soc.icache_inv_size[0] == 7'd64);
        clock;
        check("core-0 tile commit clears resident instruction line",
              !u_soc.g_core[0].u_icache.valid[8'h10]);
        repeat (4) clock;
        check("core-0 tile write reaches physical memory exactly once",
              c0_write_accepts == 1);

        // Core 1 has an independent engine and invalidates only its paired
        // private I-cache.  A matching line in core 0 remains noncoherent.
        @(negedge sys_clk);
        u_soc.g_core[0].u_icache.valid[8'h30] = 1'b1;
        u_soc.g_core[0].u_icache.tags[8'h30] = 64'h0000_0300;
        u_soc.g_core[1].u_icache.valid[8'h30] = 1'b1;
        u_soc.g_core[1].u_icache.tags[8'h30] = 64'h0000_0300;
        force u_soc.core_tile_addr[1] = 32'h0000_0323;
        force u_soc.core_tile_wen[1] = 1'b1;
        force u_soc.core_tile_wdata[1] = {64{8'h3C}};
        force u_soc.core_tile_req[1] = 1'b1;
        clock;
        force u_soc.core_tile_req[1] = 1'b0;

        guard = 0;
        while (!u_soc.core_tile_icache_inv_line[1]) begin
            clock;
            guard = guard + 1;
            if (guard > 30) begin
                $display("  FAIL: timeout waiting for core-1 tile commit");
                fail_count = fail_count + 1;
                $finish(1);
            end
        end
        check("core-1 tile commit invalidates only its paired cache",
              u_soc.core_tile_icache_inv_addr[1] == 64'h0000_0300
              && u_soc.icache_inv_size[1] == 7'd64
              && !u_soc.core_tile_icache_inv_line[0]);
        clock;
        check("core-1 tile commit clears its resident instruction line",
              !u_soc.g_core[1].u_icache.valid[8'h30]);
        check("core-1 tile commit preserves core-0 private line",
              u_soc.g_core[0].u_icache.valid[8'h30]);
        repeat (4) clock;
        check("core-1 tile write reaches physical memory exactly once",
              c1_write_accepts == 1);

        // A cluster tile write is deliberately noncoherent to the full-core
        // private cache and therefore must not drive core-0 invalidation.
        @(negedge sys_clk);
        u_soc.g_core[0].u_icache.valid[8'h20] = 1'b1;
        u_soc.g_core[0].u_icache.tags[8'h20] = 64'h0000_0200;
        force u_soc.cluster_tile_addr[0] = 32'h0000_0223;
        force u_soc.cluster_tile_wen[0] = 1'b1;
        force u_soc.cluster_tile_wdata[0] = {64{8'h5A}};
        force u_soc.cluster_tile_req[0] = 1'b1;
        clock;
        force u_soc.cluster_tile_req[0] = 1'b0;

        guard = 0;
        while (!(u_soc.tile_write_commit
                 && u_soc.tile_write_owner == 3'd4)) begin
            clock;
            guard = guard + 1;
            if (guard > 30) begin
                $display("  FAIL: timeout waiting for cluster tile commit");
                fail_count = fail_count + 1;
                $finish(1);
            end
        end
        check("cluster tile commit does not claim core-0 coherence",
              !u_soc.core_tile_icache_inv_line[0]);
        clock;
        check("cluster tile commit preserves core-0 private line",
              u_soc.g_core[0].u_icache.valid[8'h20]);
        repeat (4) clock;
        check("cluster tile write reaches physical memory exactly once",
              cluster_write_accepts == 1);

        $display("");
        if (fail_count == 0)
            $display("tb_soc_tile_icache: ALL %0d assertions PASSED",
                     pass_count);
        else
            $display("tb_soc_tile_icache: %0d PASSED, %0d FAILED",
                     pass_count, fail_count);

        if (fail_count != 0)
            $finish(1);
        $finish(0);
    end

    initial begin
        #300000;
        $display("tb_soc_tile_icache: TIMEOUT");
        $finish(1);
    end

endmodule
