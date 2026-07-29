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
    integer tacc_write_commit_count;
    reg     track_rr_commits;
    reg     track_tacc_commits;

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
        .phy_cancel         (),
        .phy_rdata          (64'd0),
        .phy_rvalid         (1'b0),
        .phy_ready          (1'b1),
        .phy_error          (1'b0),
        .phy_cancel_done    (1'b1),
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
            tacc_write_commit_count = 0;
            track_rr_commits = 1'b0;
            track_tacc_commits = 1'b0;
        end else begin
            if (u_soc.core_mex_done[0])
                done_count0 = done_count0 + 1;
            if (u_soc.core_mex_done[1])
                done_count1 = done_count1 + 1;
            if (u_soc.core_mex_done[2])
                done_count2 = done_count2 + 1;
            if (u_soc.core_mex_done[3])
                done_count3 = done_count3 + 1;

            if (u_soc.tile_write_commit && track_rr_commits) begin
                // Core zero's preceding TACC STORE consumed the prior turn,
                // so equal RR fairness resumes at core one and wraps to zero.
                if (u_soc.tile_write_owner !==
                    ((write_commit_count + 1) % 4)) begin
                    $display("  FAIL: commit %0d owned by %0d, expected %0d",
                             write_commit_count,
                             u_soc.tile_write_owner,
                             (write_commit_count + 1) % 4);
                    fail_count = fail_count + 1;
                end
                write_commit_count = write_commit_count + 1;
            end

            if (u_soc.tile_write_commit && track_tacc_commits) begin
                if (u_soc.tile_write_owner !== 3'd0 ||
                    u_soc.tile_write_ext !== 1'b0 ||
                    u_soc.tile_write_addr !==
                        (64'h0000_0000_0000_0100 +
                         tacc_write_commit_count * 64)) begin
                    $display("  FAIL: TACC beat %0d owner=%0d ext=%0d addr=%h",
                             tacc_write_commit_count,
                             u_soc.tile_write_owner,
                             u_soc.tile_write_ext,
                             u_soc.tile_write_addr);
                    fail_count = fail_count + 1;
                end
                tacc_write_commit_count =
                    tacc_write_commit_count + 1;
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
        tacc_write_commit_count = 0;
        track_rr_commits = 1'b0;
        track_tacc_commits = 1'b0;
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

        // All four private full-core TACC domains claim simultaneously.
        // Exact OWNER fields prove that each generated tile received its
        // fixed caller-domain parameter rather than sharing one bank.
        force u_soc.core_mex_funct[0] = ETSYS_TACC_TRY;
        force u_soc.core_mex_funct[1] = ETSYS_TACC_TRY;
        force u_soc.core_mex_funct[2] = ETSYS_TACC_TRY;
        force u_soc.core_mex_funct[3] = ETSYS_TACC_TRY;
        force u_soc.core_mex_funct_byte[0] =
            {5'd0, ETSYS_TACC_TRY};
        force u_soc.core_mex_funct_byte[1] =
            {5'd0, ETSYS_TACC_TRY};
        force u_soc.core_mex_funct_byte[2] =
            {5'd0, ETSYS_TACC_TRY};
        force u_soc.core_mex_funct_byte[3] =
            {5'd0, ETSYS_TACC_TRY};
        force u_soc.core_mex_ext_mod[0] = 4'd8;
        force u_soc.core_mex_ext_mod[1] = 4'd8;
        force u_soc.core_mex_ext_mod[2] = 4'd8;
        force u_soc.core_mex_ext_mod[3] = 4'd8;
        force u_soc.core_mex_ext_active[0] = 1'b1;
        force u_soc.core_mex_ext_active[1] = 1'b1;
        force u_soc.core_mex_ext_active[2] = 1'b1;
        force u_soc.core_mex_ext_active[3] = 1'b1;
        force u_soc.core_mex_valid[0] = 1'b1;
        force u_soc.core_mex_valid[1] = 1'b1;
        force u_soc.core_mex_valid[2] = 1'b1;
        force u_soc.core_mex_valid[3] = 1'b1;
        clock;
        force u_soc.core_mex_valid[0] = 1'b0;
        force u_soc.core_mex_valid[1] = 1'b0;
        force u_soc.core_mex_valid[2] = 1'b0;
        force u_soc.core_mex_valid[3] = 1'b0;
        check("all four private TACC claims publish BUSY together",
              u_soc.core_tacc_status_raw[0][TACC_STATUS_BIT_BUSY]
              && u_soc.core_tacc_status_raw[1][TACC_STATUS_BIT_BUSY]
              && u_soc.core_tacc_status_raw[2][TACC_STATUS_BIT_BUSY]
              && u_soc.core_tacc_status_raw[3][TACC_STATUS_BIT_BUSY]);
        clock;
        clock;
        check("all four private TACC domains claim their fixed callers",
              u_soc.core_tacc_status_raw[0][
                  TACC_STATUS_OWNER_MSB:TACC_STATUS_OWNER_LSB] == 5'd0
              && u_soc.core_tacc_status_raw[1][
                  TACC_STATUS_OWNER_MSB:TACC_STATUS_OWNER_LSB] == 5'd1
              && u_soc.core_tacc_status_raw[2][
                  TACC_STATUS_OWNER_MSB:TACC_STATUS_OWNER_LSB] == 5'd2
              && u_soc.core_tacc_status_raw[3][
                  TACC_STATUS_OWNER_MSB:TACC_STATUS_OWNER_LSB] == 5'd3
              && u_soc.core_tacc_status_raw[0][
                  TACC_STATUS_BIT_CLAIMED]
              && u_soc.core_tacc_status_raw[1][
                  TACC_STATUS_BIT_CLAIMED]
              && u_soc.core_tacc_status_raw[2][
                  TACC_STATUS_BIT_CLAIMED]
              && u_soc.core_tacc_status_raw[3][
                  TACC_STATUS_BIT_CLAIMED]);
        check("full-core TACC status inserts caller-relative MINE",
              u_soc.core_tacc_status[0][TACC_STATUS_BIT_MINE]
              && u_soc.core_tacc_status[1][TACC_STATUS_BIT_MINE]
              && u_soc.core_tacc_status[2][TACC_STATUS_BIT_MINE]
              && u_soc.core_tacc_status[3][TACC_STATUS_BIT_MINE]);

        // Exercise the complete SoC image path while all four full-core
        // domains remain claimed.  Four distinct SRAM rows make beat order
        // and atomic publication observable without bypassing the shared
        // transfer stage or the seven-source tile-port arbiter.
        u_soc.u_memory.g_bank[0].u_sram.mem[0] = {64{8'h11}};
        u_soc.u_memory.g_bank[0].u_sram.mem[1] = {64{8'h22}};
        u_soc.u_memory.g_bank[0].u_sram.mem[2] = {64{8'h33}};
        u_soc.u_memory.g_bank[0].u_sram.mem[3] = {64{8'h44}};

        force u_soc.core_csr_addr[0] = CSR_TSRC0;
        force u_soc.core_csr_wdata[0] = 64'h0000_0000_0000_0000;
        force u_soc.core_csr_wen[0] = 1'b1;
        clock;
        force u_soc.core_csr_wen[0] = 1'b0;
        force u_soc.core_csr_addr[0] = CSR_TMODE;
        force u_soc.core_csr_wdata[0] = {61'd0, TMODE_8};
        force u_soc.core_csr_wen[0] = 1'b1;
        clock;
        force u_soc.core_csr_wen[0] = 1'b0;

        force u_soc.core_mex_funct[0] = ETSYS_TACC_LOAD;
        force u_soc.core_mex_funct_byte[0] =
            {5'd0, ETSYS_TACC_LOAD};
        force u_soc.core_mex_valid[0] = 1'b1;
        clock;
        force u_soc.core_mex_valid[0] = 1'b0;
        check("full-core TACC LOAD enters the shared transfer path",
              u_soc.core_mex_busy[0]);

        guard = 0;
        while (!u_soc.core_mex_done[0]) begin
            clock;
            guard = guard + 1;
            if (guard > 100) begin
                $display("  FAIL: timeout waiting for TACC LOAD");
                fail_count = fail_count + 1;
                $fatal(1, "full-core TACC LOAD timeout");
            end
        end
        check("four-beat TACC LOAD completes without a fault",
              u_soc.core_mex_fault[0] == MEX_FAULT_NONE);
        clock;
        check("TACC LOAD publishes the complete image atomically",
              u_soc.g_full_tile[0].u_tile.tacc_bank_state ==
                  {{64{8'h44}}, {64{8'h33}},
                   {64{8'h22}}, {64{8'h11}}});
        check("successful TACC LOAD leaves valid clean private state",
              u_soc.core_tacc_status_raw[0][TACC_STATUS_BIT_VALID]
              && !u_soc.core_tacc_status_raw[0][TACC_STATUS_BIT_DIRTY]
              && !u_soc.core_tacc_status_raw[1][TACC_STATUS_BIT_VALID]
              && !u_soc.core_tacc_status_raw[2][TACC_STATUS_BIT_VALID]
              && !u_soc.core_tacc_status_raw[3][TACC_STATUS_BIT_VALID]);

        u_soc.u_memory.g_bank[0].u_sram.mem[4] = 512'd0;
        u_soc.u_memory.g_bank[0].u_sram.mem[5] = 512'd0;
        u_soc.u_memory.g_bank[0].u_sram.mem[6] = 512'd0;
        u_soc.u_memory.g_bank[0].u_sram.mem[7] = 512'd0;
        force u_soc.core_csr_addr[0] = CSR_TDST;
        force u_soc.core_csr_wdata[0] = 64'h0000_0000_0000_0100;
        force u_soc.core_csr_wen[0] = 1'b1;
        clock;
        force u_soc.core_csr_wen[0] = 1'b0;

        tacc_write_commit_count = 0;
        track_tacc_commits = 1'b1;
        force u_soc.core_mex_funct[0] = ETSYS_TACC_STORE;
        force u_soc.core_mex_funct_byte[0] =
            {5'd0, ETSYS_TACC_STORE};
        force u_soc.core_mex_valid[0] = 1'b1;
        clock;
        force u_soc.core_mex_valid[0] = 1'b0;
        check("full-core TACC STORE enters the shared transfer path",
              u_soc.core_mex_busy[0]);

        guard = 0;
        while (!u_soc.core_mex_done[0]) begin
            clock;
            guard = guard + 1;
            if (guard > 100) begin
                $display("  FAIL: timeout waiting for TACC STORE");
                fail_count = fail_count + 1;
                $fatal(1, "full-core TACC STORE timeout");
            end
        end
        check("four-beat TACC STORE completes without a fault",
              u_soc.core_mex_fault[0] == MEX_FAULT_NONE);
        clock;
        track_tacc_commits = 1'b0;
        check("TACC STORE commits exactly four ordered SRAM beats",
              tacc_write_commit_count == 4);
        check("TACC STORE writes the canonical image end to end",
              u_soc.u_memory.g_bank[0].u_sram.mem[4] == {64{8'h11}}
              && u_soc.u_memory.g_bank[0].u_sram.mem[5] == {64{8'h22}}
              && u_soc.u_memory.g_bank[0].u_sram.mem[6] == {64{8'h33}}
              && u_soc.u_memory.g_bank[0].u_sram.mem[7] == {64{8'h44}});
        check("successful TACC STORE preserves ownership and clean state",
              u_soc.core_tacc_status_raw[0][TACC_STATUS_BIT_CLAIMED]
              && u_soc.core_tacc_status_raw[0][
                  TACC_STATUS_OWNER_MSB:TACC_STATUS_OWNER_LSB] == 5'd0
              && u_soc.core_tacc_status_raw[0][TACC_STATUS_BIT_VALID]
              && !u_soc.core_tacc_status_raw[0][TACC_STATUS_BIT_DIRTY]);

        // The three shared microcluster engines are distinct TACC domains.
        // Drive their post-arbiter leaf boundaries together, then make a
        // sibling lose TRY without trapping or stealing cluster 0's bank.
        force u_soc.g_cluster[0].u_cluster.te_mex_ss = 2'd0;
        force u_soc.g_cluster[1].u_cluster.te_mex_ss = 2'd0;
        force u_soc.g_cluster[2].u_cluster.te_mex_ss = 2'd0;
        force u_soc.g_cluster[0].u_cluster.te_mex_op = MEX_TSYS;
        force u_soc.g_cluster[1].u_cluster.te_mex_op = MEX_TSYS;
        force u_soc.g_cluster[2].u_cluster.te_mex_op = MEX_TSYS;
        force u_soc.g_cluster[0].u_cluster.te_mex_funct =
            ETSYS_TACC_TRY;
        force u_soc.g_cluster[1].u_cluster.te_mex_funct =
            ETSYS_TACC_TRY;
        force u_soc.g_cluster[2].u_cluster.te_mex_funct =
            ETSYS_TACC_TRY;
        force u_soc.g_cluster[0].u_cluster.te_mex_funct_byte =
            {5'd0, ETSYS_TACC_TRY};
        force u_soc.g_cluster[1].u_cluster.te_mex_funct_byte =
            {5'd0, ETSYS_TACC_TRY};
        force u_soc.g_cluster[2].u_cluster.te_mex_funct_byte =
            {5'd0, ETSYS_TACC_TRY};
        force u_soc.g_cluster[0].u_cluster.te_mex_ext_mod = 4'd8;
        force u_soc.g_cluster[1].u_cluster.te_mex_ext_mod = 4'd8;
        force u_soc.g_cluster[2].u_cluster.te_mex_ext_mod = 4'd8;
        force u_soc.g_cluster[0].u_cluster.te_mex_ext_active = 1'b1;
        force u_soc.g_cluster[1].u_cluster.te_mex_ext_active = 1'b1;
        force u_soc.g_cluster[2].u_cluster.te_mex_ext_active = 1'b1;
        force u_soc.g_cluster[0].u_cluster.te_mex_caller_id = 5'd4;
        force u_soc.g_cluster[1].u_cluster.te_mex_caller_id = 5'd8;
        force u_soc.g_cluster[2].u_cluster.te_mex_caller_id = 5'd12;
        force u_soc.g_cluster[0].u_cluster.te_mex_caller_slot = 2'd0;
        force u_soc.g_cluster[1].u_cluster.te_mex_caller_slot = 2'd0;
        force u_soc.g_cluster[2].u_cluster.te_mex_caller_slot = 2'd0;
        force u_soc.g_cluster[0].u_cluster.te_mex_engine_epoch = 8'd0;
        force u_soc.g_cluster[1].u_cluster.te_mex_engine_epoch = 8'd0;
        force u_soc.g_cluster[2].u_cluster.te_mex_engine_epoch = 8'd0;
        force u_soc.g_cluster[0].u_cluster.te_mex_caller_epoch = 8'd0;
        force u_soc.g_cluster[1].u_cluster.te_mex_caller_epoch = 8'd0;
        force u_soc.g_cluster[2].u_cluster.te_mex_caller_epoch = 8'd0;
        // This topology bench drives the post-arbiter leaf boundary directly;
        // the cluster handshake itself is covered in tb_cluster.
        force u_soc.g_cluster[0].u_cluster.te_mex_retire = 1'b1;
        force u_soc.g_cluster[1].u_cluster.te_mex_retire = 1'b1;
        force u_soc.g_cluster[2].u_cluster.te_mex_retire = 1'b1;
        force u_soc.g_cluster[0].u_cluster.te_mex_valid = 1'b1;
        force u_soc.g_cluster[1].u_cluster.te_mex_valid = 1'b1;
        force u_soc.g_cluster[2].u_cluster.te_mex_valid = 1'b1;
        clock;
        force u_soc.g_cluster[0].u_cluster.te_mex_valid = 1'b0;
        force u_soc.g_cluster[1].u_cluster.te_mex_valid = 1'b0;
        force u_soc.g_cluster[2].u_cluster.te_mex_valid = 1'b0;
        check("all three cluster TACC domains publish BUSY together",
              u_soc.g_cluster[0].u_cluster.te_tacc_status_raw[
                  TACC_STATUS_BIT_BUSY]
              && u_soc.g_cluster[1].u_cluster.te_tacc_status_raw[
                  TACC_STATUS_BIT_BUSY]
              && u_soc.g_cluster[2].u_cluster.te_tacc_status_raw[
                  TACC_STATUS_BIT_BUSY]);
        clock;
        clock;
        check("all seven physical TACC domains retain distinct owners",
              u_soc.g_cluster[0].u_cluster.te_tacc_status_raw[
                  TACC_STATUS_OWNER_MSB:TACC_STATUS_OWNER_LSB] == 5'd4
              && u_soc.g_cluster[1].u_cluster.te_tacc_status_raw[
                  TACC_STATUS_OWNER_MSB:TACC_STATUS_OWNER_LSB] == 5'd8
              && u_soc.g_cluster[2].u_cluster.te_tacc_status_raw[
                  TACC_STATUS_OWNER_MSB:TACC_STATUS_OWNER_LSB] == 5'd12
              && u_soc.core_tacc_status_raw[0][
                  TACC_STATUS_OWNER_MSB:TACC_STATUS_OWNER_LSB] == 5'd0
              && u_soc.core_tacc_status_raw[3][
                  TACC_STATUS_OWNER_MSB:TACC_STATUS_OWNER_LSB] == 5'd3);
        check("cluster status inserts MINE only for the owning sibling",
              u_soc.g_cluster[0].u_cluster.mc_tacc_status[
                  0*64 + TACC_STATUS_BIT_MINE]
              && !u_soc.g_cluster[0].u_cluster.mc_tacc_status[
                  1*64 + TACC_STATUS_BIT_MINE]
              && u_soc.g_cluster[1].u_cluster.mc_tacc_status[
                  0*64 + TACC_STATUS_BIT_MINE]
              && !u_soc.g_cluster[1].u_cluster.mc_tacc_status[
                  1*64 + TACC_STATUS_BIT_MINE]
              && u_soc.g_cluster[2].u_cluster.mc_tacc_status[
                  0*64 + TACC_STATUS_BIT_MINE]
              && !u_soc.g_cluster[2].u_cluster.mc_tacc_status[
                  1*64 + TACC_STATUS_BIT_MINE]);

        force u_soc.g_cluster[0].u_cluster.te_mex_caller_id = 5'd5;
        force u_soc.g_cluster[0].u_cluster.te_mex_caller_slot = 2'd1;
        force u_soc.g_cluster[0].u_cluster.te_mex_valid = 1'b1;
        clock;
        force u_soc.g_cluster[0].u_cluster.te_mex_valid = 1'b0;
        clock;
        check("cluster sibling TRY loses without stealing ownership",
              u_soc.g_cluster[0].u_cluster.te_mex_done
              && u_soc.g_cluster[0].u_cluster.te_mex_fault
                    == MEX_FAULT_NONE
              && u_soc.g_cluster[0].u_cluster.te_tacc_status_raw[
                  TACC_STATUS_OWNER_MSB:TACC_STATUS_OWNER_LSB] == 5'd4);
        clock;

        // Release the four full-core domains before the existing stateless
        // memory-arbitration test, then reset its completion counters.
        force u_soc.core_mex_funct[0] = ETSYS_TACC_RELEASE;
        force u_soc.core_mex_funct[1] = ETSYS_TACC_RELEASE;
        force u_soc.core_mex_funct[2] = ETSYS_TACC_RELEASE;
        force u_soc.core_mex_funct[3] = ETSYS_TACC_RELEASE;
        force u_soc.core_mex_funct_byte[0] =
            {5'd0, ETSYS_TACC_RELEASE};
        force u_soc.core_mex_funct_byte[1] =
            {5'd0, ETSYS_TACC_RELEASE};
        force u_soc.core_mex_funct_byte[2] =
            {5'd0, ETSYS_TACC_RELEASE};
        force u_soc.core_mex_funct_byte[3] =
            {5'd0, ETSYS_TACC_RELEASE};
        force u_soc.core_mex_valid[0] = 1'b1;
        force u_soc.core_mex_valid[1] = 1'b1;
        force u_soc.core_mex_valid[2] = 1'b1;
        force u_soc.core_mex_valid[3] = 1'b1;
        clock;
        force u_soc.core_mex_valid[0] = 1'b0;
        force u_soc.core_mex_valid[1] = 1'b0;
        force u_soc.core_mex_valid[2] = 1'b0;
        force u_soc.core_mex_valid[3] = 1'b0;
        clock;
        clock;
        check("all four private RELEASE operations wipe their domains",
              u_soc.core_tacc_status_raw[0]
                  == {43'd0, TACC_OWNER_NONE, 16'd0}
              && u_soc.core_tacc_status_raw[1]
                  == {43'd0, TACC_OWNER_NONE, 16'd0}
              && u_soc.core_tacc_status_raw[2]
                  == {43'd0, TACC_OWNER_NONE, 16'd0}
              && u_soc.core_tacc_status_raw[3]
                  == {43'd0, TACC_OWNER_NONE, 16'd0});
        done_count0 = 0;
        done_count1 = 0;
        done_count2 = 0;
        done_count3 = 0;

        force u_soc.core_mex_funct[0] = TSYS_ZERO;
        force u_soc.core_mex_funct[1] = TSYS_ZERO;
        force u_soc.core_mex_funct[2] = TSYS_ZERO;
        force u_soc.core_mex_funct[3] = TSYS_ZERO;
        force u_soc.core_mex_funct_byte[0] = {5'd0, TSYS_ZERO};
        force u_soc.core_mex_funct_byte[1] = {5'd0, TSYS_ZERO};
        force u_soc.core_mex_funct_byte[2] = {5'd0, TSYS_ZERO};
        force u_soc.core_mex_funct_byte[3] = {5'd0, TSYS_ZERO};
        force u_soc.core_mex_ext_mod[0] = 4'd0;
        force u_soc.core_mex_ext_mod[1] = 4'd0;
        force u_soc.core_mex_ext_mod[2] = 4'd0;
        force u_soc.core_mex_ext_mod[3] = 4'd0;
        force u_soc.core_mex_ext_active[0] = 1'b0;
        force u_soc.core_mex_ext_active[1] = 1'b0;
        force u_soc.core_mex_ext_active[2] = 1'b0;
        force u_soc.core_mex_ext_active[3] = 1'b0;

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
        track_rr_commits = 1'b1;
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
        check("four physical writes continue in 1-2-3-0 RR order",
              write_commit_count == 4);
        track_rr_commits = 1'b0;
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
