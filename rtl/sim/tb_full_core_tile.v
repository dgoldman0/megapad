// ============================================================================
// tb_full_core_tile.v — Four private full-core tile-engine integration
// ============================================================================

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_full_core_tile;

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
    integer write_commit_count;
    integer done_count0;
    integer done_count1;
    integer done_count2;
    integer done_count3;

    mp64_soc #(
        .MEM_DEPTH(16)
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
            write_commit_count = 0;
            done_count0 = 0;
            done_count1 = 0;
            done_count2 = 0;
            done_count3 = 0;
        end else begin
            if (u_soc.core_mex_done[0])
                done_count0 = done_count0 + 1;
            if (u_soc.core_mex_done[1])
                done_count1 = done_count1 + 1;
            if (u_soc.core_mex_done[2])
                done_count2 = done_count2 + 1;
            if (u_soc.core_mex_done[3])
                done_count3 = done_count3 + 1;

            if (u_soc.tile_write_commit) begin
                if (u_soc.tile_write_owner !== write_commit_count[2:0]) begin
                    $display("  FAIL: commit %0d owned by %0d",
                             write_commit_count,
                             u_soc.tile_write_owner);
                    fail_count = fail_count + 1;
                end
                write_commit_count = write_commit_count + 1;
            end
        end
    end

    initial begin
        pass_count = 0;
        fail_count = 0;
        write_commit_count = 0;
        done_count0 = 0;
        done_count1 = 0;
        done_count2 = 0;
        done_count3 = 0;
        sys_rst_n = 1'b0;

        // Hold CPU fetch and cluster tile producers quiescent.  The focused
        // test drives the four full-core architectural boundaries directly.
        force u_soc.cpu_icache_req[0] = 1'b0;
        force u_soc.cpu_icache_req[1] = 1'b0;
        force u_soc.cpu_icache_req[2] = 1'b0;
        force u_soc.cpu_icache_req[3] = 1'b0;
        force u_soc.cluster_tile_req[0] = 1'b0;
        force u_soc.cluster_tile_req[1] = 1'b0;
        force u_soc.cluster_tile_req[2] = 1'b0;
        force u_soc.cluster_ext_tile_req[0] = 1'b0;
        force u_soc.cluster_ext_tile_req[1] = 1'b0;
        force u_soc.cluster_ext_tile_req[2] = 1'b0;

        force u_soc.core_csr_wen[0] = 1'b0;
        force u_soc.core_csr_wen[1] = 1'b0;
        force u_soc.core_csr_wen[2] = 1'b0;
        force u_soc.core_csr_wen[3] = 1'b0;
        force u_soc.core_legacy_acc_wen[0] = 4'b0000;
        force u_soc.core_legacy_acc_wen[1] = 4'b0000;
        force u_soc.core_legacy_acc_wen[2] = 4'b0000;
        force u_soc.core_legacy_acc_wen[3] = 4'b0000;
        force u_soc.core_legacy_acc_wdata[0] = 256'd0;
        force u_soc.core_legacy_acc_wdata[1] = 256'd0;
        force u_soc.core_legacy_acc_wdata[2] = 256'd0;
        force u_soc.core_legacy_acc_wdata[3] = 256'd0;
        force u_soc.core_mex_valid[0] = 1'b0;
        force u_soc.core_mex_valid[1] = 1'b0;
        force u_soc.core_mex_valid[2] = 1'b0;
        force u_soc.core_mex_valid[3] = 1'b0;

        force u_soc.core_mex_ss[0] = 2'd0;
        force u_soc.core_mex_ss[1] = 2'd0;
        force u_soc.core_mex_ss[2] = 2'd0;
        force u_soc.core_mex_ss[3] = 2'd0;
        force u_soc.core_mex_op[0] = MEX_TSYS;
        force u_soc.core_mex_op[1] = MEX_TSYS;
        force u_soc.core_mex_op[2] = MEX_TSYS;
        force u_soc.core_mex_op[3] = MEX_TSYS;
        force u_soc.core_mex_funct[0] = TSYS_ZERO;
        force u_soc.core_mex_funct[1] = TSYS_ZERO;
        force u_soc.core_mex_funct[2] = TSYS_ZERO;
        force u_soc.core_mex_funct[3] = TSYS_ZERO;
        force u_soc.core_mex_funct_byte[0] = {5'd0, TSYS_ZERO};
        force u_soc.core_mex_funct_byte[1] = {5'd0, TSYS_ZERO};
        force u_soc.core_mex_funct_byte[2] = {5'd0, TSYS_ZERO};
        force u_soc.core_mex_funct_byte[3] = {5'd0, TSYS_ZERO};
        force u_soc.core_mex_gpr_val[0] = 64'd0;
        force u_soc.core_mex_gpr_val[1] = 64'd0;
        force u_soc.core_mex_gpr_val[2] = 64'd0;
        force u_soc.core_mex_gpr_val[3] = 64'd0;
        force u_soc.core_mex_imm8[0] = 8'd0;
        force u_soc.core_mex_imm8[1] = 8'd0;
        force u_soc.core_mex_imm8[2] = 8'd0;
        force u_soc.core_mex_imm8[3] = 8'd0;
        force u_soc.core_mex_ext_mod[0] = 4'd0;
        force u_soc.core_mex_ext_mod[1] = 4'd0;
        force u_soc.core_mex_ext_mod[2] = 4'd0;
        force u_soc.core_mex_ext_mod[3] = 4'd0;
        force u_soc.core_mex_ext_active[0] = 1'b0;
        force u_soc.core_mex_ext_active[1] = 1'b0;
        force u_soc.core_mex_ext_active[2] = 1'b0;
        force u_soc.core_mex_ext_active[3] = 1'b0;

        repeat (4) clock;
        sys_rst_n = 1'b1;
        repeat (3) clock;

        // SysInfo's enable mask is the real cluster reset control, not a
        // decorative register.
        force u_soc.sysinfo_cluster_en = 64'hFFFF_FFFF_FFFF_FFFD;
        clock;
        check("cluster-enable mask resets only the disabled cluster",
              !u_soc.g_cluster[0].u_cluster.cl_rst
              && u_soc.g_cluster[1].u_cluster.cl_rst
              && !u_soc.g_cluster[2].u_cluster.cl_rst);
        force u_soc.sysinfo_cluster_en = 64'hFFFF_FFFF_FFFF_FFFF;
        clock;
        check("re-enabled cluster leaves reset",
              !u_soc.g_cluster[1].u_cluster.cl_rst);

        // Simultaneous CSR writes must remain private to each full-core
        // engine.  Readback occurs through the same per-core CSR wires.
        force u_soc.core_csr_addr[0] = CSR_TSRC0;
        force u_soc.core_csr_addr[1] = CSR_TSRC0;
        force u_soc.core_csr_addr[2] = CSR_TSRC0;
        force u_soc.core_csr_addr[3] = CSR_TSRC0;
        force u_soc.core_csr_wdata[0] = 64'h1000;
        force u_soc.core_csr_wdata[1] = 64'h2000;
        force u_soc.core_csr_wdata[2] = 64'h3000;
        force u_soc.core_csr_wdata[3] = 64'h4000;
        force u_soc.core_csr_wen[0] = 1'b1;
        force u_soc.core_csr_wen[1] = 1'b1;
        force u_soc.core_csr_wen[2] = 1'b1;
        force u_soc.core_csr_wen[3] = 1'b1;
        clock;
        force u_soc.core_csr_wen[0] = 1'b0;
        force u_soc.core_csr_wen[1] = 1'b0;
        force u_soc.core_csr_wen[2] = 1'b0;
        force u_soc.core_csr_wen[3] = 1'b0;
        #1;
        check("core 0 has private tile CSR state",
              u_soc.core_csr_rdata[0] == 64'h1000);
        check("core 1 has private tile CSR state",
              u_soc.core_csr_rdata[1] == 64'h2000);
        check("core 2 has private tile CSR state",
              u_soc.core_csr_rdata[2] == 64'h3000);
        check("core 3 has private tile CSR state",
              u_soc.core_csr_rdata[3] == 64'h4000);

        // The tile bank is the sole persistent legacy ACC.  Seed each
        // physical engine through its direct CSR port and prove that the
        // paired CPU sees that same state without a second CPU-local copy.
        force u_soc.core_csr_addr[0] = CSR_ACC0;
        force u_soc.core_csr_addr[1] = CSR_ACC0;
        force u_soc.core_csr_addr[2] = CSR_ACC0;
        force u_soc.core_csr_addr[3] = CSR_ACC0;
        force u_soc.core_csr_wdata[0] = 64'hA000_0000_0000_0000;
        force u_soc.core_csr_wdata[1] = 64'hA100_0000_0000_0001;
        force u_soc.core_csr_wdata[2] = 64'hA200_0000_0000_0002;
        force u_soc.core_csr_wdata[3] = 64'hA300_0000_0000_0003;
        force u_soc.core_csr_wen[0] = 1'b1;
        force u_soc.core_csr_wen[1] = 1'b1;
        force u_soc.core_csr_wen[2] = 1'b1;
        force u_soc.core_csr_wen[3] = 1'b1;
        clock;
        force u_soc.core_csr_wen[0] = 1'b0;
        force u_soc.core_csr_wen[1] = 1'b0;
        force u_soc.core_csr_wen[2] = 1'b0;
        force u_soc.core_csr_wen[3] = 1'b0;
        #1;
        check("direct tile ACC writes reach all four CPU-facing states",
              u_soc.g_core[0].u_cpu.legacy_acc_state[63:0]
                  == 64'hA000_0000_0000_0000
              && u_soc.g_core[1].u_cpu.legacy_acc_state[63:0]
                  == 64'hA100_0000_0000_0001
              && u_soc.g_core[2].u_cpu.legacy_acc_state[63:0]
                  == 64'hA200_0000_0000_0002
              && u_soc.g_core[3].u_cpu.legacy_acc_state[63:0]
                  == 64'hA300_0000_0000_0003);
        check("simultaneous tile ACC CSR seeds remain engine-private",
              u_soc.core_csr_rdata[0] == 64'hA000_0000_0000_0000
              && u_soc.core_csr_rdata[1] == 64'hA100_0000_0000_0001
              && u_soc.core_csr_rdata[2] == 64'hA200_0000_0000_0002
              && u_soc.core_csr_rdata[3] == 64'hA300_0000_0000_0003);

        // Drive the CPU's legacy/SHA masked writeback boundary.  Each core
        // selects a different lane; exact whole-bank checks prove the mask,
        // the return path, and isolation between all four private engines.
        force u_soc.core_legacy_acc_wdata[0] =
            {64'hF000_0000_0000_0000, 192'd0};
        force u_soc.core_legacy_acc_wdata[1] =
            {64'd0, 64'hF100_0000_0000_0001, 128'd0};
        force u_soc.core_legacy_acc_wdata[2] =
            {128'd0, 64'hF200_0000_0000_0002, 64'd0};
        force u_soc.core_legacy_acc_wdata[3] =
            {192'd0, 64'hF300_0000_0000_0003};
        force u_soc.core_legacy_acc_wen[0] = 4'b1000;
        force u_soc.core_legacy_acc_wen[1] = 4'b0100;
        force u_soc.core_legacy_acc_wen[2] = 4'b0010;
        force u_soc.core_legacy_acc_wen[3] = 4'b0001;
        clock;
        force u_soc.core_legacy_acc_wen[0] = 4'b0000;
        force u_soc.core_legacy_acc_wen[1] = 4'b0000;
        force u_soc.core_legacy_acc_wen[2] = 4'b0000;
        force u_soc.core_legacy_acc_wen[3] = 4'b0000;
        #1;
        check("CPU masked legacy writes update only their paired tile bank",
              u_soc.core_legacy_acc_state[0]
                  == {64'hF000_0000_0000_0000, 128'd0,
                      64'hA000_0000_0000_0000}
              && u_soc.core_legacy_acc_state[1]
                  == {64'd0, 64'hF100_0000_0000_0001, 64'd0,
                      64'hA100_0000_0000_0001}
              && u_soc.core_legacy_acc_state[2]
                  == {128'd0, 64'hF200_0000_0000_0002,
                      64'hA200_0000_0000_0002}
              && u_soc.core_legacy_acc_state[3]
                  == {192'd0, 64'hF300_0000_0000_0003});

        force u_soc.core_csr_addr[0] = CSR_ACC3;
        force u_soc.core_csr_addr[1] = CSR_ACC2;
        force u_soc.core_csr_addr[2] = CSR_ACC1;
        force u_soc.core_csr_addr[3] = CSR_ACC0;
        #1;
        check("CPU masked lanes read back through the same tile CSR banks",
              u_soc.core_csr_rdata[0] == 64'hF000_0000_0000_0000
              && u_soc.core_csr_rdata[1] == 64'hF100_0000_0000_0001
              && u_soc.core_csr_rdata[2] == 64'hF200_0000_0000_0002
              && u_soc.core_csr_rdata[3] == 64'hF300_0000_0000_0003);

        // Give each engine a distinct internal destination.
        force u_soc.core_csr_addr[0] = CSR_TDST;
        force u_soc.core_csr_addr[1] = CSR_TDST;
        force u_soc.core_csr_addr[2] = CSR_TDST;
        force u_soc.core_csr_addr[3] = CSR_TDST;
        force u_soc.core_csr_wdata[0] = 64'h0000;
        force u_soc.core_csr_wdata[1] = 64'h0040;
        force u_soc.core_csr_wdata[2] = 64'h0080;
        force u_soc.core_csr_wdata[3] = 64'h00C0;
        force u_soc.core_csr_wen[0] = 1'b1;
        force u_soc.core_csr_wen[1] = 1'b1;
        force u_soc.core_csr_wen[2] = 1'b1;
        force u_soc.core_csr_wen[3] = 1'b1;
        clock;
        force u_soc.core_csr_wen[0] = 1'b0;
        force u_soc.core_csr_wen[1] = 1'b0;
        force u_soc.core_csr_wen[2] = 1'b0;
        force u_soc.core_csr_wen[3] = 1'b0;

        // All four engines admit work together; only their physical memory
        // writes serialize through the seven-source arbiter.
        force u_soc.core_mex_valid[0] = 1'b1;
        force u_soc.core_mex_valid[1] = 1'b1;
        force u_soc.core_mex_valid[2] = 1'b1;
        force u_soc.core_mex_valid[3] = 1'b1;
        clock;
        force u_soc.core_mex_valid[0] = 1'b0;
        force u_soc.core_mex_valid[1] = 1'b0;
        force u_soc.core_mex_valid[2] = 1'b0;
        force u_soc.core_mex_valid[3] = 1'b0;
        check("all four private engines become busy together",
              u_soc.core_mex_busy[0] && u_soc.core_mex_busy[1]
              && u_soc.core_mex_busy[2] && u_soc.core_mex_busy[3]);

        guard = 0;
        while (done_count0 != 1 || done_count1 != 1
               || done_count2 != 1 || done_count3 != 1) begin
            clock;
            guard = guard + 1;
            if (guard > 100) begin
                $display("  FAIL: timeout waiting for four private engines");
                fail_count = fail_count + 1;
                $fatal(1, "full-core private tile timeout");
            end
        end
        repeat (3) clock;
        check("each full-core operation completes exactly once",
              done_count0 == 1 && done_count1 == 1
              && done_count2 == 1 && done_count3 == 1);
        check("four physical writes complete in 0-1-2-3 RR order",
              write_commit_count == 4);
        check("all full-core engines return idle",
              !u_soc.core_mex_busy[0] && !u_soc.core_mex_busy[1]
              && !u_soc.core_mex_busy[2] && !u_soc.core_mex_busy[3]);

        $display("");
        if (fail_count == 0)
            $display("tb_full_core_tile: ALL %0d assertions PASSED",
                     pass_count);
        else
            $display("tb_full_core_tile: %0d PASSED, %0d FAILED",
                     pass_count, fail_count);

        if (fail_count != 0)
            $fatal(1, "tb_full_core_tile failed");
        $finish(0);
    end

    initial begin
        #500000;
        $fatal(1, "tb_full_core_tile timeout");
    end

endmodule
