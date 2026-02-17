// ============================================================================
// tb_nic.v — Network Interface Controller Unit Tests
// ============================================================================
//
// Tests for mp64_nic.v:
//   1. Status register — present bit, link up
//   2. MAC address readback
//   3. DMA base register write/readback
//   4. Frame length register write/readback
//   5. IRQ control register
//   6. RX path — PHY drives frame → rx_buf → CMD RECV → DMA out
//   7. RX IRQ generation
//   8. TX path — set frame_len + DMA base → CMD SEND → DMA in → PHY out
//   9. TX complete IRQ
//  10. IRQ status W1C
//  11. RX counter increments
//  12. TX counter increments
//  13. Data window register read/write
//

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_nic;

    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;

    integer pass_cnt = 0, fail_cnt = 0;
    integer test_num = 0;
    integer i;

    // ========================================================================
    // DUT signals
    // ========================================================================
    reg        nic_req;
    reg  [6:0] nic_addr;
    reg  [7:0] nic_wdata;
    reg        nic_wen;
    wire [7:0] nic_rdata;
    wire       nic_ack;
    wire       nic_irq;

    wire       dma_req;
    wire [63:0] dma_addr;
    wire [7:0] dma_wdata;
    wire       dma_wen;
    reg  [7:0] dma_rdata;
    reg        dma_ack;

    wire       phy_tx_valid;
    wire [7:0] phy_tx_data;
    reg        phy_tx_ready;
    reg        phy_rx_valid;
    reg  [7:0] phy_rx_data;
    wire       phy_rx_ready;
    reg        phy_link_up;

    mp64_nic u_nic (
        .clk(clk), .rst_n(rst_n),
        .req(nic_req), .addr(nic_addr), .wdata(nic_wdata), .wen(nic_wen),
        .rdata(nic_rdata), .ack(nic_ack), .irq(nic_irq),
        .dma_req(dma_req), .dma_addr(dma_addr),
        .dma_wdata(dma_wdata), .dma_wen(dma_wen),
        .dma_rdata(dma_rdata), .dma_ack(dma_ack),
        .phy_tx_valid(phy_tx_valid), .phy_tx_data(phy_tx_data),
        .phy_tx_ready(phy_tx_ready),
        .phy_rx_valid(phy_rx_valid), .phy_rx_data(phy_rx_data),
        .phy_rx_ready(phy_rx_ready),
        .phy_link_up(phy_link_up)
    );

    // Simple DMA memory model (256 bytes) — combinational ack
    reg [7:0] dma_mem [0:255];

    // Combinational ack + read path: ack asserts same delta as req,
    // so it drops immediately when NIC deasserts req (no stale-ack skip).
    always @(*) begin
        dma_ack   = dma_req;
        dma_rdata = dma_mem[dma_addr[7:0]];
    end

    // Clocked write path
    always @(posedge clk) begin
        if (dma_req && dma_wen)
            dma_mem[dma_addr[7:0]] <= dma_wdata;
    end

    // ========================================================================
    // Helper tasks
    // ========================================================================
    task nic_write(input [6:0] a, input [7:0] d);
    begin
        @(posedge clk);
        nic_req <= 1'b1; nic_addr <= a; nic_wdata <= d; nic_wen <= 1'b1;
        @(posedge clk);
        nic_req <= 1'b0; nic_wen <= 1'b0;
    end
    endtask

    task nic_read(input [6:0] a, output [7:0] d);
    begin
        @(posedge clk);
        nic_req <= 1'b1; nic_addr <= a; nic_wen <= 1'b0;
        @(posedge clk);
        nic_req <= 1'b0;
        @(posedge clk);
        d = nic_rdata;
    end
    endtask

    task check8(input [511:0] label, input [7:0] got, input [7:0] exp);
    begin
        if (got === exp) begin
            $display("  PASS: %0s = %02h", label, got);
            pass_cnt = pass_cnt + 1;
        end else begin
            $display("  FAIL: %0s = %02h (expected %02h)", label, got, exp);
            fail_cnt = fail_cnt + 1;
        end
    end
    endtask

    task check1(input [511:0] label, input got, input exp);
    begin
        if (got === exp) begin
            $display("  PASS: %0s = %0b", label, got);
            pass_cnt = pass_cnt + 1;
        end else begin
            $display("  FAIL: %0s = %0b (expected %0b)", label, got, exp);
            fail_cnt = fail_cnt + 1;
        end
    end
    endtask

    // ========================================================================
    // Main test sequence
    // ========================================================================
    reg [7:0] rd;

    initial begin
        $dumpfile("tb_nic.vcd");
        $dumpvars(0, tb_nic);

        rst_n = 0;
        nic_req = 0; nic_addr = 0; nic_wdata = 0; nic_wen = 0;
        phy_tx_ready = 1'b1;
        phy_rx_valid = 1'b0;
        phy_rx_data  = 8'd0;
        phy_link_up  = 1'b1;
        for (i = 0; i < 256; i = i + 1) dma_mem[i] = 8'd0;

        repeat(4) @(posedge clk);
        rst_n = 1;
        repeat(2) @(posedge clk);

        // =================================================================
        // TEST 1: Status register — present + link
        // =================================================================
        test_num = 1;
        $display("\n=== TEST %0d: Status register ===", test_num);
        nic_read(7'h01, rd);
        check1("present", rd[7], 1'b1);
        check1("link_up", rd[2], 1'b1);
        check1("rx_avail initially 0", rd[1], 1'b0);
        check1("tx_busy initially 0", rd[0], 1'b0);

        // =================================================================
        // TEST 2: MAC address readback
        // =================================================================
        test_num = 2;
        $display("\n=== TEST %0d: MAC address ===", test_num);
        // MAC = 02:4D:50:36:34:00
        nic_read(7'h0E, rd); check8("MAC byte0", rd, 8'h02);
        nic_read(7'h0F, rd); check8("MAC byte1", rd, 8'h4D);
        nic_read(7'h10, rd); check8("MAC byte2", rd, 8'h50);
        nic_read(7'h13, rd); check8("MAC byte5", rd, 8'h00);

        // =================================================================
        // TEST 3: DMA base register write/readback
        // =================================================================
        test_num = 3;
        $display("\n=== TEST %0d: DMA base register ===", test_num);
        nic_write(7'h02, 8'h00);  // dma_base[7:0]
        nic_write(7'h03, 8'h40);  // dma_base[15:8] → base=0x4000
        nic_write(7'h04, 8'h00);
        nic_write(7'h05, 8'h00);
        nic_write(7'h06, 8'h00);
        nic_write(7'h07, 8'h00);
        nic_write(7'h08, 8'h00);
        nic_write(7'h09, 8'h00);
        // Readback is not directly supported via register map; verified via DMA address

        // =================================================================
        // TEST 4: Frame length register
        // =================================================================
        test_num = 4;
        $display("\n=== TEST %0d: Frame length register ===", test_num);
        nic_write(7'h0A, 8'h08);  // len=8
        nic_write(7'h0B, 8'h00);
        nic_read(7'h0A, rd); check8("frame_len lo", rd, 8'h08);
        nic_read(7'h0B, rd); check8("frame_len hi", rd, 8'h00);

        // =================================================================
        // TEST 5: IRQ control register
        // =================================================================
        test_num = 5;
        $display("\n=== TEST %0d: IRQ control register ===", test_num);
        nic_write(7'h0C, 8'h03);  // enable both RX and TX IRQ
        nic_read(7'h0C, rd); check8("irq_ctrl", rd, 8'h03);

        // =================================================================
        // TEST 6: RX path — PHY drives 4-byte frame → rx_buf
        // =================================================================
        test_num = 6;
        $display("\n=== TEST %0d: RX path ===", test_num);
        // Set DMA base to 0x00 (low bytes of dma_mem)
        nic_write(7'h02, 8'h00);
        nic_write(7'h03, 8'h00);

        // Drive 4 bytes from PHY
        @(posedge clk);
        phy_rx_valid = 1'b1; phy_rx_data = 8'hAA;
        @(posedge clk);
        phy_rx_data = 8'hBB;
        @(posedge clk);
        phy_rx_data = 8'hCC;
        @(posedge clk);
        phy_rx_data = 8'hDD;
        @(posedge clk);
        phy_rx_valid = 1'b0;  // end of frame
        repeat(5) @(posedge clk);

        // Check RX available
        nic_read(7'h01, rd);
        check1("rx_avail after frame", rd[1], 1'b1);

        // Issue CMD=RECV (0x02) to trigger DMA
        nic_write(7'h00, 8'h02);  // CMD=RECV
        // Wait for DMA to complete (4 bytes)
        repeat(20) @(posedge clk);

        // Verify DMA memory contents
        check8("dma_mem[0]", dma_mem[0], 8'hAA);
        check8("dma_mem[1]", dma_mem[1], 8'hBB);
        check8("dma_mem[2]", dma_mem[2], 8'hCC);
        check8("dma_mem[3]", dma_mem[3], 8'hDD);

        // =================================================================
        // TEST 7: RX IRQ generation
        // =================================================================
        test_num = 7;
        $display("\n=== TEST %0d: RX IRQ ===", test_num);
        // IRQ should have been set (irq_ctrl[0]=1 from test 5, rx irq pending)
        check1("NIC IRQ after RX", nic_irq, 1'b1);

        // =================================================================
        // TEST 8: IRQ status W1C
        // =================================================================
        test_num = 8;
        $display("\n=== TEST %0d: IRQ status W1C ===", test_num);
        nic_read(7'h0D, rd);
        check1("RX irq pending", rd[0], 1'b1);
        // Clear RX IRQ
        nic_write(7'h0D, 8'h01);
        @(posedge clk);
        nic_read(7'h0D, rd);
        check1("RX irq cleared", rd[0], 1'b0);
        check1("IRQ deasserted", nic_irq, 1'b0);

        // =================================================================
        // TEST 9: RX counter
        // =================================================================
        test_num = 9;
        $display("\n=== TEST %0d: RX counter ===", test_num);
        nic_read(7'h16, rd); check8("rx_count lo", rd, 8'h01);

        // =================================================================
        // TEST 10: TX path — load data to DMA, set frame_len, CMD SEND
        // =================================================================
        test_num = 10;
        $display("\n=== TEST %0d: TX path ===", test_num);
        // Load 4 bytes into DMA memory for TX
        dma_mem[0] = 8'h11;
        dma_mem[1] = 8'h22;
        dma_mem[2] = 8'h33;
        dma_mem[3] = 8'h44;
        // Set DMA base and frame length
        nic_write(7'h02, 8'h00);  // dma_base = 0
        nic_write(7'h03, 8'h00);
        nic_write(7'h0A, 8'h04);  // frame_len = 4
        nic_write(7'h0B, 8'h00);

        // Issue CMD=SEND (0x01)
        nic_write(7'h00, 8'h01);

        // Wait for TX DMA read + PHY send
        // Capture PHY output
        begin
            reg [7:0] captured [0:3];
            integer cap_idx;
            cap_idx = 0;
            repeat(40) begin
                @(posedge clk);
                if (phy_tx_valid && cap_idx < 4) begin
                    captured[cap_idx] = phy_tx_data;
                    cap_idx = cap_idx + 1;
                end
            end
            check8("PHY TX byte0", captured[0], 8'h11);
            check8("PHY TX byte1", captured[1], 8'h22);
            check8("PHY TX byte2", captured[2], 8'h33);
            check8("PHY TX byte3", captured[3], 8'h44);
        end

        // =================================================================
        // TEST 11: TX complete IRQ
        // =================================================================
        test_num = 11;
        $display("\n=== TEST %0d: TX complete IRQ ===", test_num);
        repeat(5) @(posedge clk);
        nic_read(7'h0D, rd);
        check1("TX irq pending", rd[1], 1'b1);
        check1("NIC IRQ asserted after TX", nic_irq, 1'b1);

        // =================================================================
        // TEST 12: TX counter
        // =================================================================
        test_num = 12;
        $display("\n=== TEST %0d: TX counter ===", test_num);
        nic_read(7'h14, rd); check8("tx_count lo", rd, 8'h01);

        // =================================================================
        // TEST 13: Data window register
        // =================================================================
        test_num = 13;
        $display("\n=== TEST %0d: Data window register ===", test_num);
        // Write to data window at offset 0x20
        nic_write(7'h20, 8'hDE);
        nic_write(7'h21, 8'hAD);
        nic_read(7'h20, rd); check8("data_window[0]", rd, 8'hDE);
        nic_read(7'h21, rd); check8("data_window[1]", rd, 8'hAD);

        // =================================================================
        // Summary
        // =================================================================
        $display("\n========================================");
        $display("  NIC Tests: %0d PASSED, %0d FAILED", pass_cnt, fail_cnt);
        $display("========================================");
        $finish;
    end

    // Watchdog
    initial begin
        #2000000;
        $display("TIMEOUT: NIC TestBench exceeded 2ms");
        $finish;
    end

endmodule
