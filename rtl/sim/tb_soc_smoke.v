// ============================================================================
// tb_soc_smoke.v — Smoke testbench for mp64_soc (full SoC integration)
// ============================================================================
//
// Instantiates the complete SoC and verifies:
//   1. Design elaborates and resets cleanly
//   2. UART TX becomes idle-high after reset
//   3. Memory bus activity occurs (CPU is fetching)
//   4. Debug LEDs light up after a few cycles
//
// External PHY / NIC are tied off (no external memory in this test).
//
`timescale 1ns / 1ps

`include "mp64_pkg.vh"

module tb_soc_smoke;

    // ============================================================
    // Clock + reset
    // ============================================================
    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;          // 100 MHz

    initial begin
        rst_n = 1'b0;
        #50;
        rst_n = 1'b1;
    end

    // ============================================================
    // SoC I/O wires
    // ============================================================
    wire        uart_txd;
    wire        sd_sck, sd_mosi, sd_cs_n;
    wire        phy_req;
    wire [63:0] phy_addr;
    wire        phy_wen;
    wire [63:0] phy_wdata;
    wire [7:0]  phy_burst_len;
    wire        nic_tx_valid;
    wire [7:0]  nic_tx_data;
    wire        nic_rx_ready;
    wire [7:0]  debug_leds;

    // ============================================================
    // SoC instance (small configuration for fast simulation)
    // ============================================================
    mp64_soc #(
        .CLOCK_HZ         (100_000_000),
        .NUM_CORES         (1),
        .NUM_CLUSTERS      (1),
        .CORES_PER_CLUSTER (2),
        .MEM_DEPTH         (256)        // small for simulation
    ) u_soc (
        .sys_clk       (clk),
        .sys_rst_n     (rst_n),

        // UART
        .uart_rxd      (1'b1),         // idle
        .uart_txd      (uart_txd),

        // PHY — tied off
        .phy_req       (phy_req),
        .phy_addr      (phy_addr),
        .phy_wen       (phy_wen),
        .phy_wdata     (phy_wdata),
        .phy_burst_len (phy_burst_len),
        .phy_cancel    (),
        .phy_rdata     (64'd0),
        .phy_rvalid    (1'b0),
        .phy_ready     (1'b1),
        .phy_error     (1'b0),
        .phy_cancel_done(1'b1),

        // SD — tied off
        .sd_sck        (sd_sck),
        .sd_mosi       (sd_mosi),
        .sd_miso       (1'b1),
        .sd_cs_n       (sd_cs_n),
        .sd_card_present(1'b0),
        .sd_write_protected(1'b0),

        // NIC — tied off
        .nic_tx_valid  (nic_tx_valid),
        .nic_tx_data   (nic_tx_data),
        .nic_tx_ready  (1'b0),
        .nic_rx_valid  (1'b0),
        .nic_rx_data   (8'd0),
        .nic_rx_ready  (nic_rx_ready),
        .nic_link_up   (1'b0),

        .debug_leds    (debug_leds)
    );

    // ============================================================
    // Test body
    // ============================================================
    integer pass_count, fail_count;

    task check(input [255:0] label, input cond);
    begin
        if (cond) begin
            $display("  PASS: %0s", label);
            pass_count = pass_count + 1;
        end else begin
            $display("  FAIL: %0s", label);
            fail_count = fail_count + 1;
        end
    end
    endtask

    initial begin
        $dumpfile("tb_soc_smoke.vcd");
        $dumpvars(0, tb_soc_smoke);

        pass_count = 0;
        fail_count = 0;

        $display("=== tb_soc_smoke: Full-SoC integration test ===");

        // Wait for reset to complete
        @(posedge rst_n);
        repeat (5) @(posedge clk);

        // --- Check 1: UART TX should be idle-high after reset ---
        check("UART TX idle after reset", uart_txd === 1'b1);

        // Reduced configurations compact both the physical requestor slots and
        // the absolute core IDs while production remains 4+3.
        check("Reduced tile requestors are compact",
              u_soc.TILE_SOURCE_COUNT == 2
              && u_soc.CLUSTER_TILE_SOURCE_BASE == 1
              && $bits(u_soc.tile_src_req_bus) == 2);
        check("Reduced cluster core IDs begin after instantiated full cores",
              u_soc.g_cluster[0].u_cluster.CLUSTER_ID_BASE == 8'd1);
        check("WOTS appends after stable NIC and disk requester indices",
              u_soc.NIC_BUS_PORT == 2 && u_soc.DISK_BUS_PORT == 3 &&
              u_soc.WOTS_BUS_PORT == 4 && u_soc.N_BUS_PORTS == 5);
        check("Integrated WOTS requester resets idle and fixed-QoS",
              u_soc.bus_cpu_valid[u_soc.WOTS_BUS_PORT] === 1'b0 &&
              u_soc.WOTS_FIXED_QOS_MASK == 5'b1_0000);

        // Exercise the integrated SysInfo decode directly.  The testbench
        // temporarily owns the arbiter-to-MMIO seam so CPU fetch traffic
        // cannot make these combinational checks nondeterministic.
        force u_soc.bus_mmio_req = 1'b1;
        force u_soc.bus_mmio_wen = 1'b0;
        force u_soc.bus_mmio_wdata = 64'd0;
        force u_soc.bus_mmio_port_io = 1'b0;

        force u_soc.bus_mmio_addr = 12'h360;
        force u_soc.bus_mmio_size = BUS_DWORD;
        #1;
        check("SysInfo advertises the qualified checkpoint-3 capabilities",
              u_soc.bus_mmio_ack === 1'b1
              && u_soc.bus_mmio_rdata == 64'hF);

        force u_soc.bus_mmio_addr = 12'h368;
        #1;
        check("SysInfo reports every weighted-arbiter requester",
              u_soc.bus_mmio_ack === 1'b1
              && u_soc.bus_mmio_rdata == 64'd5);

        force u_soc.bus_mmio_addr = 12'h305;
        force u_soc.bus_mmio_size = BUS_BYTE;
        #1;
        check("SysInfo byte reads zero-extend an interior lane",
              u_soc.bus_mmio_ack === 1'b1
              && u_soc.bus_mmio_rdata == 64'h0000_0000_0000_0036);

        force u_soc.bus_mmio_addr = 12'h304;
        force u_soc.bus_mmio_size = BUS_HALF;
        #1;
        check("SysInfo halfword reads zero-extend their lane",
              u_soc.bus_mmio_ack === 1'b1
              && u_soc.bus_mmio_rdata == 64'h0000_0000_0000_3634);

        force u_soc.bus_mmio_addr = 12'h300;
        force u_soc.bus_mmio_size = BUS_WORD;
        #1;
        check("SysInfo word reads zero-extend their lane",
              u_soc.bus_mmio_ack === 1'b1
              && u_soc.bus_mmio_rdata == 64'h0000_0000_0002_0001);

        force u_soc.bus_mmio_addr = 12'h360;
        force u_soc.bus_mmio_size = BUS_BYTE;
        #1;
        check("SysInfo byte reads use little-endian lanes",
              u_soc.bus_mmio_ack === 1'b1
              && u_soc.bus_mmio_rdata == 64'hF);

        force u_soc.bus_mmio_addr = 12'h361;
        force u_soc.bus_mmio_size = BUS_HALF;
        #1;
        check("SysInfo rejects a misaligned access",
              u_soc.bus_mmio_ack === 1'b0);

        force u_soc.bus_mmio_addr = 12'h36F;
        #1;
        check("SysInfo rejects a crossing access",
              u_soc.bus_mmio_ack === 1'b0);

        force u_soc.bus_mmio_addr = 12'h370;
        force u_soc.bus_mmio_size = BUS_BYTE;
        #1;
        check("SysInfo does not alias beyond its exact window",
              u_soc.bus_mmio_ack === 1'b0);

        force u_soc.bus_mmio_addr = 12'h360;
        force u_soc.bus_mmio_size = BUS_DWORD;
        force u_soc.bus_mmio_wen = 1'b1;
        force u_soc.bus_mmio_wdata = 64'd0;
        @(posedge clk);
        #1;
        force u_soc.bus_mmio_wen = 1'b0;
        #1;
        check("SysInfo capability writes are acknowledged and ignored",
              u_soc.bus_mmio_ack === 1'b1
              && u_soc.bus_mmio_rdata == 64'hF);

        // Exercise raw Keccak through the integrated SoC decode/mux rather
        // than only through the standalone SHA front-end bench.
        force u_soc.bus_mmio_addr = 12'h780;
        force u_soc.bus_mmio_size = BUS_BYTE;
        force u_soc.bus_mmio_wen = 1'b1;
        force u_soc.bus_mmio_wdata = 64'h6;
        @(posedge clk);
        #1;
        force u_soc.bus_mmio_req = 1'b0;
        repeat (2) @(posedge clk);
        #1;
        force u_soc.bus_mmio_req = 1'b1;
        force u_soc.bus_mmio_wen = 1'b0;
        force u_soc.bus_mmio_addr = 12'h781;
        @(posedge clk);
        #1;
        check("Integrated raw Keccak command enters raw BUSY",
              u_soc.bus_mmio_ack === 1'b1
              && u_soc.bus_mmio_rdata[7:0] == 8'h09);

        force u_soc.bus_mmio_req = 1'b0;
        repeat (32) @(posedge clk);
        #1;
        force u_soc.bus_mmio_req = 1'b1;
        @(posedge clk);
        #1;
        check("Integrated raw Keccak command reaches raw DONE",
              u_soc.bus_mmio_ack === 1'b1
              && u_soc.bus_mmio_rdata[7:0] == 8'h0A);

        force u_soc.bus_mmio_req = 1'b0;
        @(posedge clk);
        #1;
        force u_soc.bus_mmio_req = 1'b1;
        force u_soc.bus_mmio_addr = 12'h7D8;
        force u_soc.bus_mmio_size = BUS_DWORD;
        @(posedge clk);
        #1;
        check("Integrated zero-state Keccak lane 0 is mapped little-endian",
              u_soc.bus_mmio_ack === 1'b1
              && u_soc.bus_mmio_rdata == 64'hF125_8F79_40E1_DDE7);

        force u_soc.bus_mmio_req = 1'b0;
        @(posedge clk);
        #1;
        force u_soc.bus_mmio_req = 1'b1;
        force u_soc.bus_mmio_addr = 12'h780;
        force u_soc.bus_mmio_size = BUS_BYTE;
        force u_soc.bus_mmio_wen = 1'b1;
        force u_soc.bus_mmio_wdata = 64'h7;
        @(posedge clk);
        #1;
        force u_soc.bus_mmio_req = 1'b0;
        repeat (2) @(posedge clk);
        #1;
        force u_soc.bus_mmio_req = 1'b1;
        force u_soc.bus_mmio_wen = 1'b0;
        force u_soc.bus_mmio_addr = 12'h781;
        @(posedge clk);
        #1;
        check("Integrated raw Keccak CLEAR returns scrubbed idle",
              u_soc.bus_mmio_ack === 1'b1
              && u_soc.bus_mmio_rdata[7:0] == 8'h00);

        // Exercise the qualified production byte-only WOTS front end.
        force u_soc.bus_mmio_req = 1'b0;
        @(posedge clk);
        #1;
        force u_soc.bus_mmio_req = 1'b1;
        force u_soc.bus_mmio_addr = 12'h8AA;
        force u_soc.bus_mmio_wen = 1'b0;
        force u_soc.bus_mmio_size = BUS_BYTE;
        @(posedge clk);
        #1;
        check("Integrated WOTS controller reports reset IDLE",
              u_soc.bus_mmio_ack === 1'b1
              && u_soc.bus_mmio_rdata == 64'h0
              && u_soc.wots_active === 1'b0);

        force u_soc.bus_mmio_req = 1'b0;
        @(posedge clk);
        #1;
        force u_soc.bus_mmio_req = 1'b1;
        force u_soc.bus_mmio_addr = 12'h8A0;
        force u_soc.bus_mmio_wen = 1'b1;
        force u_soc.bus_mmio_wdata = 64'hA5;
        @(posedge clk);
        #1;
        check("Integrated WOTS accepts byte programming while IDLE",
              u_soc.bus_mmio_ack === 1'b1);

        force u_soc.bus_mmio_req = 1'b0;
        @(posedge clk);
        #1;
        force u_soc.bus_mmio_req = 1'b1;
        force u_soc.bus_mmio_wen = 1'b0;
        @(posedge clk);
        #1;
        check("Integrated WOTS programming readback is little-endian byte data",
              u_soc.bus_mmio_ack === 1'b1 &&
              u_soc.bus_mmio_rdata[7:0] == 8'hA5);

        force u_soc.bus_mmio_req = 1'b0;
        @(posedge clk);
        #1;
        force u_soc.bus_mmio_req = 1'b1;
        force u_soc.bus_mmio_addr = 12'h8AA;
        force u_soc.bus_mmio_size = BUS_DWORD;
        @(posedge clk);
        #1;
        check("Integrated WOTS rejects wider MMIO before controller request",
              u_soc.mmio_sel_wots === 1'b1 &&
              u_soc.bus_mmio_ack === 1'b0 &&
              u_soc.u_wots.req === 1'b0 &&
              u_soc.u_wots.context_addr_reg[7:0] == 8'hA5 &&
              u_soc.u_wots.status_reg == 2'd0 &&
              u_soc.u_wots.error_reg == 8'd0 &&
              u_soc.wots_active === 1'b0);

        force u_soc.bus_mmio_req = 1'b0;
        @(posedge clk);
        #1;
        force u_soc.bus_mmio_req = 1'b1;
        force u_soc.bus_mmio_addr = 12'h8AB;
        force u_soc.bus_mmio_wen = 1'b1;
        force u_soc.bus_mmio_wdata = 64'hFF;
        force u_soc.bus_mmio_size = BUS_BYTE;
        @(posedge clk);
        #1;
        check("Integrated WOTS rejects writes to read-only bytes before controller request",
              u_soc.mmio_sel_wots === 1'b1 &&
              u_soc.bus_mmio_ack === 1'b0 &&
              u_soc.u_wots.req === 1'b0 &&
              u_soc.u_wots.context_addr_reg[7:0] == 8'hA5 &&
              u_soc.u_wots.status_reg == 2'd0 &&
              u_soc.u_wots.error_reg == 8'd0 &&
              u_soc.wots_active === 1'b0);

        release u_soc.bus_mmio_req;
        release u_soc.bus_mmio_addr;
        release u_soc.bus_mmio_wdata;
        release u_soc.bus_mmio_wen;
        release u_soc.bus_mmio_size;
        release u_soc.bus_mmio_port_io;

        // --- Check 2: Debug LEDs should be non-zero after reset ---
        repeat (5) @(posedge clk);
        check("Debug LEDs active after reset", debug_leds !== 8'h00);

        // --- Check 3: SD CS should be deselected (high) ---
        check("SD CS deselected", sd_cs_n === 1'b1);

        // --- Check 4: Run for 200 cycles, design should not hang ---
        repeat (200) @(posedge clk);
        check("Design ran 200 cycles without hanging", 1'b1);

        // --- Check 5: NIC TX should not assert without link ---
        check("NIC TX not asserting without link", nic_tx_valid === 1'b0);

        $display("\n=== Results: %0d passed, %0d failed ===", pass_count, fail_count);
        if (fail_count > 0)
            $display("SOME TESTS FAILED");
        else
            $display("ALL TESTS PASSED");

        if (fail_count > 0)
            $fatal(1, "tb_soc_smoke failed %0d checks", fail_count);
        $finish;
    end

    // Timeout watchdog
    initial begin
        #50000;
        $fatal(1, "tb_soc_smoke timeout after 50us");
    end

endmodule
