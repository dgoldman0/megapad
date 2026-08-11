// ============================================================================
// tb_soc_wots.v -- integrated SoC WOTS bus/Bank0/shared-Keccak transaction
// ============================================================================
//
// This bench drives only the external MMIO-decoder seam.  The WOTS context is
// fetched by the production controller through the real weighted mp64_bus and
// mp64_memory Bank0 port, and both chain permutations use mp64_sha3's sole
// Keccak core.  No DMA capture, response, or permutation result is faked.

`timescale 1ns/1ps
`include "mp64_pkg.vh"

module tb_soc_wots;

    localparam [63:0] CONTEXT_ADDR = 64'h0000_0000_0000_0100;
    localparam [127:0] EXPECT_START3_STEPS2 =
        128'h40c6609d399ea3a3c3ed5e62d5db0197;

    reg clk;
    reg rst_n;
    initial clk = 1'b0;
    always #5 clk = ~clk;

    initial begin
        rst_n = 1'b0;
        #50;
        rst_n = 1'b1;
    end

    wire        uart_txd;
    wire        sd_sck;
    wire        sd_mosi;
    wire        sd_cs_n;
    wire        phy_req;
    wire [63:0] phy_addr;
    wire        phy_wen;
    wire [63:0] phy_wdata;
    wire [7:0]  phy_burst_len;
    wire        nic_tx_valid;
    wire [7:0]  nic_tx_data;
    wire        nic_rx_ready;
    wire [7:0]  debug_leds;

    mp64_soc #(
        .CLOCK_HZ         (100_000_000),
        .NUM_CORES        (1),
        .NUM_CLUSTERS     (1),
        .CORES_PER_CLUSTER(2),
        .MEM_DEPTH        (256)
    ) u_soc (
        .sys_clk        (clk),
        .sys_rst_n      (rst_n),
        .uart_rxd       (1'b1),
        .uart_txd       (uart_txd),
        .phy_req        (phy_req),
        .phy_addr       (phy_addr),
        .phy_wen        (phy_wen),
        .phy_wdata      (phy_wdata),
        .phy_burst_len  (phy_burst_len),
        .phy_cancel     (),
        .phy_rdata      (64'd0),
        .phy_rvalid     (1'b0),
        .phy_ready      (1'b1),
        .phy_error      (1'b0),
        .phy_cancel_done(1'b1),
        .sd_sck         (sd_sck),
        .sd_mosi        (sd_mosi),
        .sd_miso        (1'b1),
        .sd_cs_n        (sd_cs_n),
        .sd_card_present(1'b0),
        .sd_write_protected(1'b0),
        .nic_tx_valid   (nic_tx_valid),
        .nic_tx_data    (nic_tx_data),
        .nic_tx_ready   (1'b0),
        .nic_rx_valid   (1'b0),
        .nic_rx_data    (8'd0),
        .nic_rx_ready   (nic_rx_ready),
        .nic_link_up    (1'b0),
        .debug_leds     (debug_leds)
    );

    integer pass_count;
    integer fail_count;
    integer accept_count;
    integer response_count;
    integer claim_count;
    integer permutation_count;
    integer release_count;
    integer byte_index;
    integer private_index;
    reg monitor_active;
    reg accept_order_ok;
    reg response_order_ok;
    reg response_data_ok;
    reg response_code_ok;
    reg requester_shape_ok;
    reg saw_shared_owner;
    reg release_level_q;
    reg private_clean;
    reg [7:0] mmio_value;
    reg [127:0] first_result;
    reg [127:0] second_result;
    reg [31:0] cycles_before_clear;

    task check;
        input [8*120-1:0] label;
        input             condition;
        begin
            if (condition) begin
                pass_count = pass_count + 1;
                $display("  [PASS] %0s", label);
            end else begin
                fail_count = fail_count + 1;
                $display("  [FAIL] %0s", label);
            end
        end
    endtask

    function [7:0] context_byte;
        input integer index;
        begin
            if (index < 16)
                context_byte = index[7:0];
            else if (index < 48)
                context_byte = 8'h20 + (index - 16);
            else
                context_byte = 8'h80 + (index - 48);
        end
    endfunction

    task mmio_write_byte;
        input [11:0] address;
        input [7:0]  value;
        begin
            @(negedge clk);
            force u_soc.bus_mmio_addr = address;
            force u_soc.bus_mmio_wdata = {56'd0, value};
            force u_soc.bus_mmio_wen = 1'b1;
            force u_soc.bus_mmio_size = BUS_BYTE;
            force u_soc.bus_mmio_port_io = 1'b0;
            force u_soc.bus_mmio_req = 1'b1;
            @(posedge clk); #1;
            check("integrated WOTS MMIO byte write acknowledges",
                  u_soc.bus_mmio_ack === 1'b1);
            @(negedge clk);
            force u_soc.bus_mmio_req = 1'b0;
            force u_soc.bus_mmio_wen = 1'b0;
            @(posedge clk); #1;
        end
    endtask

    task mmio_read_byte;
        input  [11:0] address;
        output [7:0]  value;
        begin
            @(negedge clk);
            force u_soc.bus_mmio_addr = address;
            force u_soc.bus_mmio_wdata = 64'd0;
            force u_soc.bus_mmio_wen = 1'b0;
            force u_soc.bus_mmio_size = BUS_BYTE;
            force u_soc.bus_mmio_port_io = 1'b0;
            force u_soc.bus_mmio_req = 1'b1;
            @(posedge clk); #1;
            check("integrated WOTS MMIO byte read acknowledges",
                  u_soc.bus_mmio_ack === 1'b1);
            value = u_soc.bus_mmio_rdata[7:0];
            @(negedge clk);
            force u_soc.bus_mmio_req = 1'b0;
            @(posedge clk); #1;
        end
    endtask

    always @(posedge clk) begin
        if (!rst_n) begin
            accept_count      <= 0;
            response_count    <= 0;
            claim_count       <= 0;
            permutation_count <= 0;
            release_count     <= 0;
            accept_order_ok   <= 1'b1;
            response_order_ok <= 1'b1;
            response_data_ok  <= 1'b1;
            response_code_ok  <= 1'b1;
            requester_shape_ok <= 1'b1;
            saw_shared_owner  <= 1'b0;
            release_level_q   <= 1'b0;
        end else if (monitor_active) begin
            if (u_soc.wots_dma_accept) begin
                if (u_soc.wots_dma_addr !==
                        CONTEXT_ADDR + accept_count)
                    accept_order_ok <= 1'b0;
                if (u_soc.bus_cpu_wen[u_soc.WOTS_BUS_PORT] !== 1'b0 ||
                    u_soc.bus_cpu_size[
                        u_soc.WOTS_BUS_PORT*2 +: 2] !== BUS_BYTE ||
                    u_soc.bus_cpu_port_io[u_soc.WOTS_BUS_PORT] !== 1'b0 ||
                    u_soc.bus_cpu_addr[
                        u_soc.WOTS_BUS_PORT*64 +: 64] !==
                            u_soc.wots_dma_addr)
                    requester_shape_ok <= 1'b0;
                accept_count <= accept_count + 1;
            end
            if (u_soc.wots_dma_resp_valid) begin
                if (u_soc.wots_dma_addr !==
                        CONTEXT_ADDR + response_count)
                    response_order_ok <= 1'b0;
                if (u_soc.wots_dma_resp_code !== BUS_RESP_OK)
                    response_code_ok <= 1'b0;
                if (u_soc.wots_dma_rdata[
                        u_soc.wots_dma_addr[2:0]*8 +: 8] !==
                        context_byte(response_count))
                    response_data_ok <= 1'b0;
                response_count <= response_count + 1;
            end
            if (u_soc.wots_sha_grant)
                claim_count <= claim_count + 1;
            if (u_soc.wots_sha_perm_req && u_soc.wots_sha_owned)
                permutation_count <= permutation_count + 1;
            if (u_soc.wots_sha_release && !release_level_q)
                release_count <= release_count + 1;
            release_level_q <= u_soc.wots_sha_release;
            if (u_soc.wots_sha_owned)
                saw_shared_owner <= 1'b1;
        end
    end

    initial begin
        pass_count = 0;
        fail_count = 0;
        monitor_active = 1'b0;
        first_result = 128'd0;
        second_result = 128'd0;

        $display("=== tb_soc_wots: integrated WOTS transaction ===");

        // Only the appended WOTS requester remains live.  The direct MMIO
        // seam below substitutes for a guest driver, not for DMA or memory.
        force u_soc.bus_cpu_valid[3:0] = 4'b0000;
        force u_soc.bus_mmio_req = 1'b0;
        force u_soc.bus_mmio_addr = 12'd0;
        force u_soc.bus_mmio_wdata = 64'd0;
        force u_soc.bus_mmio_wen = 1'b0;
        force u_soc.bus_mmio_size = BUS_BYTE;
        force u_soc.bus_mmio_port_io = 1'b0;

        @(posedge rst_n);
        repeat (4) @(posedge clk);

        check("reduced SoC keeps WOTS on the appended real bus port",
              u_soc.WOTS_BUS_PORT == 4 && u_soc.N_BUS_PORTS == 5 &&
              u_soc.WOTS_FIXED_QOS_MASK == 5'b1_0000);

        // One aligned 64-byte Bank0 context.  In ascending byte order this is
        // seed 00..0f, ADRS 20..3f, and node 80..8f.
        u_soc.u_memory.g_bank[0].u_sram.mem[4] =
            {128'h8f8e8d8c8b8a89888786858483828180,
             128'h3f3e3d3c3b3a39383736353433323130,
             128'h2f2e2d2c2b2a29282726252423222120,
             128'h0f0e0d0c0b0a09080706050403020100};

        for (byte_index = 0; byte_index < 8;
             byte_index = byte_index + 1)
            mmio_write_byte(12'h8a0 + byte_index,
                            CONTEXT_ADDR[byte_index*8 +: 8]);
        mmio_write_byte(12'h8a8, 8'd2);
        mmio_write_byte(12'h8a9, 8'd3);

        monitor_active = 1'b1;
        mmio_write_byte(12'h8aa, 8'd1);

        while (u_soc.u_wots.status_reg !== 2'd2)
            @(posedge clk);
        check("integrated WOTS reaches DONE through the production path",
              u_soc.u_wots.status_reg == 2'd2);
        check("integrated WOTS uses exactly 64 ordered bus captures",
              accept_count == 64 && accept_order_ok);
        check("integrated WOTS receives exactly 64 ordered Bank0 responses",
              response_count == 64 && response_order_ok);
        check("integrated Bank0 responses carry the addressed byte lanes",
              response_data_ok && response_code_ok);
        check("integrated requester remains read-only BUS_BYTE throughout",
              requester_shape_ok);
        check("integrated WOTS claims the sole shared service once",
              saw_shared_owner && claim_count == 1);
        check("integrated WOTS requests exactly two shared permutations",
              permutation_count == 2);
        check("integrated WOTS releases the shared service once",
              release_count == 1 && !u_soc.wots_sha_owned &&
              u_soc.u_sha3.owner == 2'd0);

        mmio_read_byte(12'h8ab, mmio_value);
        check("integrated WOTS terminal ERROR is zero", mmio_value == 8'd0);
        for (byte_index = 0; byte_index < 16;
             byte_index = byte_index + 1) begin
            mmio_read_byte(12'h8b0 + byte_index, mmio_value);
            first_result[byte_index*8 +: 8] = mmio_value;
        end
        check("integrated WOTS DOUT matches the independent SHAKE256 KAT",
              first_result == EXPECT_START3_STEPS2);

        repeat (8) @(posedge clk);
        mmio_read_byte(12'h8aa, mmio_value);
        check("integrated WOTS DONE status remains terminal until CLEAR",
              mmio_value == 8'd2);
        mmio_read_byte(12'h8ab, mmio_value);
        check("integrated WOTS terminal ERROR remains zero until CLEAR",
              mmio_value == 8'd0);
        for (byte_index = 0; byte_index < 16;
             byte_index = byte_index + 1) begin
            mmio_read_byte(12'h8b0 + byte_index, mmio_value);
            second_result[byte_index*8 +: 8] = mmio_value;
        end
        check("integrated WOTS DOUT stays stable until CLEAR",
              second_result == first_result);

        private_clean = 1'b1;
        for (private_index = 0; private_index < 64;
             private_index = private_index + 1)
            if (u_soc.u_wots.context_stage[private_index] !== 8'd0)
                private_clean = 1'b0;
        for (private_index = 0; private_index < 16;
             private_index = private_index + 1)
            if (u_soc.u_wots.result_private[private_index] !== 8'd0)
                private_clean = 1'b0;
        check("integrated terminal publication follows private scrubbing",
              private_clean &&
              u_soc.u_wots.active_context_addr == 64'd0 &&
              u_soc.u_wots.active_steps == 8'd0 &&
              u_soc.u_wots.active_start == 8'd0 &&
              u_soc.u_wots.current_node == 128'd0 &&
              u_soc.u_wots.perm_state_reg == 1600'd0 &&
              u_soc.u_wots.dma_index == 6'd0 &&
              u_soc.u_wots.chain_index == 4'd0 &&
              u_soc.u_wots.service_cycles == 6'd0 &&
              u_soc.u_wots.dma_accept_count == 0);
        check("integrated shared Keccak state is scrubbed after release",
              u_soc.wots_sha_state_out == 1600'd0 &&
              u_soc.u_sha3.u_keccak_core.state_out == 1600'd0);

        cycles_before_clear = u_soc.u_wots.cycles_reg;
        mmio_write_byte(12'h8aa, 8'd2);
        while (u_soc.u_wots.status_reg !== 2'd0)
            @(posedge clk);
        private_clean = 1'b1;
        for (private_index = 0; private_index < 16;
             private_index = private_index + 1)
            if (u_soc.u_wots.dout[private_index] !== 8'd0)
                private_clean = 1'b0;
        check("integrated CLEAR returns WOTS to scrubbed IDLE",
              u_soc.u_wots.status_reg == 2'd0 &&
              u_soc.u_wots.error_reg == 8'd0 &&
              u_soc.u_wots.context_addr_reg == 64'd0 &&
              u_soc.u_wots.steps_reg == 8'd0 &&
              u_soc.u_wots.start_reg == 8'd0 &&
              private_clean &&
              !u_soc.wots_active && !u_soc.wots_dma_valid);
        check("integrated CLEAR retains the request CYCLES diagnostic",
              u_soc.u_wots.cycles_reg == cycles_before_clear);
        check("integrated CLEAR launches no additional DMA or permutation",
              accept_count == 64 && response_count == 64 &&
              permutation_count == 2 && release_count == 1);

        monitor_active = 1'b0;
        release u_soc.bus_cpu_valid[3:0];
        release u_soc.bus_mmio_req;
        release u_soc.bus_mmio_addr;
        release u_soc.bus_mmio_wdata;
        release u_soc.bus_mmio_wen;
        release u_soc.bus_mmio_size;
        release u_soc.bus_mmio_port_io;

        $display("=== Results: %0d passed, %0d failed ===",
                 pass_count, fail_count);
        if (fail_count != 0)
            $fatal(1, "tb_soc_wots failed %0d checks", fail_count);
        $finish;
    end

    // Infrastructure watchdog only.  Architectural request/clear bounds are
    // derived and proved by the controller/firmware deadline tests.
    initial begin
        #100000;
        $fatal(1,
               "tb_soc_wots infrastructure safety window expired");
    end

endmodule
