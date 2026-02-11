// ============================================================================
// tb_mailbox.v — Inter-Core Mailbox & Hardware Spinlock Tests
// ============================================================================
//
// Tests for mp64_mailbox.v:
//   1. Mailbox write + readback (per-core data registers)
//   2. IPI send: core 0 → core 1 (sets pending, fires ipi_out)
//   3. IPI acknowledge (clears pending, de-asserts ipi_out)
//   4. IPI broadcast: core 0 → all cores
//   5. Multiple senders: core 0 + core 2 → core 3
//   6. Spinlock acquire (free → acquired, returns 0)
//   7. Spinlock contention (held → returns 1 for other core)
//   8. Spinlock release + re-acquire by different core
//   9. Re-entrant spinlock (same core re-acquires → OK)
//  10. Multiple spinlocks independence
//  11. Spinlock owner tracking
//

`timescale 1ns / 1ps
`include "mp64_defs.vh"

module tb_mailbox;

    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;

    // === DUT interface ===
    reg         req;
    reg  [11:0] addr;
    reg  [7:0]  wdata;
    reg         wen;
    wire [7:0]  rdata;
    wire        ack;
    reg  [CORE_ID_BITS-1:0] requester_id;
    wire [NUM_CORES-1:0]     ipi_out;

    mp64_mailbox u_mbox (
        .clk          (clk),
        .rst_n        (rst_n),
        .req          (req),
        .addr         (addr),
        .wdata        (wdata),
        .wen          (wen),
        .rdata        (rdata),
        .ack          (ack),
        .requester_id (requester_id),
        .ipi_out      (ipi_out)
    );

    // ========================================================================
    // Helpers
    // ========================================================================
    integer pass_cnt, fail_cnt;

    task mbox_write;
        input [CORE_ID_BITS-1:0] core;
        input [11:0]             offset;
        input [7:0]              data;
    begin
        @(posedge clk);
        req          <= 1'b1;
        wen          <= 1'b1;
        requester_id <= core;
        addr         <= offset;
        wdata        <= data;
        @(posedge clk);
        req <= 1'b0;
        wen <= 1'b0;
    end
    endtask

    task mbox_read;
        input  [CORE_ID_BITS-1:0] core;
        input  [11:0]              offset;
        output [7:0]               data;
    begin
        @(posedge clk);
        req          <= 1'b1;
        wen          <= 1'b0;
        requester_id <= core;
        addr         <= offset;
        @(posedge clk);
        data = rdata;
        req <= 1'b0;
    end
    endtask

    task check8;
        input [7:0]   got;
        input [7:0]   expected;
        input [255:0] label;
    begin
        if (got === expected) begin
            $display("  PASS: %0s = 0x%h", label, got);
            pass_cnt = pass_cnt + 1;
        end else begin
            $display("  FAIL: %0s = 0x%h  (expected 0x%h)", label, got, expected);
            fail_cnt = fail_cnt + 1;
        end
    end
    endtask

    task check_ipi;
        input [NUM_CORES-1:0] expected;
        input [255:0]         label;
    begin
        if (ipi_out === expected) begin
            $display("  PASS: %0s  ipi_out=4'b%b", label, ipi_out);
            pass_cnt = pass_cnt + 1;
        end else begin
            $display("  FAIL: %0s  ipi_out=4'b%b  (expected 4'b%b)", label, ipi_out, expected);
            fail_cnt = fail_cnt + 1;
        end
    end
    endtask

    task check_true;
        input         cond;
        input [255:0] label;
    begin
        if (cond) begin
            $display("  PASS: %0s", label);
            pass_cnt = pass_cnt + 1;
        end else begin
            $display("  FAIL: %0s", label);
            fail_cnt = fail_cnt + 1;
        end
    end
    endtask

    // Convenience: mailbox MMIO addresses
    // MBOX region: 0x500 + register offset
    // Spinlock region: 0x600 + lock_index*4 + register offset

    // ========================================================================
    // Tests
    // ========================================================================
    reg [7:0] rd;

    initial begin
        $dumpfile("tb_mailbox.vcd");
        $dumpvars(0, tb_mailbox);

        pass_cnt = 0;
        fail_cnt = 0;
        req  = 0;
        wen  = 0;
        requester_id = 2'd0;
        rst_n = 0;
        repeat (5) @(posedge clk);
        rst_n = 1;
        @(posedge clk);

        // ================================================================
        // TEST 1 — Mailbox data write + readback
        // ================================================================
        $display("\n=== TEST 1: Mailbox write + readback ===");

        // Core 0 writes 0x42 to its data low byte
        mbox_write(2'd0, {4'h5, 8'h00}, 8'h42);  // MBOX_BASE + MBOX_DATA_LO
        mbox_read (2'd0, {4'h5, 8'h00}, rd);
        check8(rd, 8'h42, "core0 data_lo");

        // Core 0 writes 0xAB to byte 1
        mbox_write(2'd0, {4'h5, 8'h01}, 8'hAB);
        mbox_read (2'd0, {4'h5, 8'h01}, rd);
        check8(rd, 8'hAB, "core0 data[1]");

        // Core 1 writes different data — independent
        mbox_write(2'd1, {4'h5, 8'h00}, 8'hFF);
        mbox_read (2'd1, {4'h5, 8'h00}, rd);
        check8(rd, 8'hFF, "core1 data_lo");

        // Verify core 0's data still intact
        mbox_read (2'd0, {4'h5, 8'h00}, rd);
        check8(rd, 8'h42, "core0 data_lo still");

        // ================================================================
        // TEST 2 — IPI send: core 0 → core 1
        // ================================================================
        $display("\n=== TEST 2: IPI send core 0 -> core 1 ===");

        // Initially no IPIs pending
        check_ipi(4'b0000, "initial no IPI");

        // Core 0 sends to core 1 (write target=1 to MBOX_SEND)
        mbox_write(2'd0, {4'h5, 4'h0, MBOX_SEND}, 8'h01);  // target core 1
        @(posedge clk);  // let state settle

        // ipi_out[1] should be high now
        check_ipi(4'b0010, "ipi to core1");

        // Core 1 reads status — should see pending from core 0 (bit 0)
        mbox_read(2'd1, {4'h5, 4'h0, MBOX_STATUS}, rd);
        check8(rd & 8'h0F, 8'h01, "core1 pending from core0");

        // ================================================================
        // TEST 3 — IPI acknowledge
        // ================================================================
        $display("\n=== TEST 3: IPI acknowledge ===");

        // Core 1 acks core 0's message
        mbox_write(2'd1, {4'h5, 4'h0, MBOX_ACK}, 8'h00);  // ack from core 0
        @(posedge clk);

        // IPI should clear
        check_ipi(4'b0000, "ipi cleared after ack");

        // Status should be clear
        mbox_read(2'd1, {4'h5, 4'h0, MBOX_STATUS}, rd);
        check8(rd & 8'h0F, 8'h00, "core1 pending clear");

        // ================================================================
        // TEST 4 — IPI broadcast: core 0 → all
        // ================================================================
        $display("\n=== TEST 4: IPI broadcast ===");

        // Core 0 sends to cores 1, 2, 3
        mbox_write(2'd0, {4'h5, 4'h0, MBOX_SEND}, 8'h01);
        mbox_write(2'd0, {4'h5, 4'h0, MBOX_SEND}, 8'h02);
        mbox_write(2'd0, {4'h5, 4'h0, MBOX_SEND}, 8'h03);
        @(posedge clk);

        check_ipi(4'b1110, "broadcast ipi 1,2,3");

        // Clean up: ack all
        mbox_write(2'd1, {4'h5, 4'h0, MBOX_ACK}, 8'h00);
        mbox_write(2'd2, {4'h5, 4'h0, MBOX_ACK}, 8'h00);
        mbox_write(2'd3, {4'h5, 4'h0, MBOX_ACK}, 8'h00);
        @(posedge clk);
        check_ipi(4'b0000, "broadcast ack clear");

        // ================================================================
        // TEST 5 — Multiple senders: core 0 + core 2 → core 3
        // ================================================================
        $display("\n=== TEST 5: Multiple senders to core 3 ===");

        mbox_write(2'd0, {4'h5, 4'h0, MBOX_SEND}, 8'h03);  // core 0 → 3
        mbox_write(2'd2, {4'h5, 4'h0, MBOX_SEND}, 8'h03);  // core 2 → 3
        @(posedge clk);

        check_ipi(4'b1000, "core3 ipi from 0+2");

        // Core 3 checks status: bits 0 and 2 should be set
        mbox_read(2'd3, {4'h5, 4'h0, MBOX_STATUS}, rd);
        check8(rd & 8'h0F, 8'h05, "core3 pending 0+2");

        // Ack core 0 only — IPI should stay (core 2 still pending)
        mbox_write(2'd3, {4'h5, 4'h0, MBOX_ACK}, 8'h00);
        @(posedge clk);
        check_ipi(4'b1000, "core3 ipi still (core2)");

        // Ack core 2 — now clear
        mbox_write(2'd3, {4'h5, 4'h0, MBOX_ACK}, 8'h02);
        @(posedge clk);
        check_ipi(4'b0000, "core3 ipi clear");

        // ================================================================
        // TEST 6 — Spinlock acquire (free)
        // ================================================================
        $display("\n=== TEST 6: Spinlock acquire (free) ===");

        // Core 0 acquires lock 0: read SLOCK_ACQUIRE at SPINLOCK_BASE + 0*4
        mbox_read(2'd0, {4'h6, 8'h00}, rd);  // SPINLOCK_BASE + lock0 + SLOCK_ACQUIRE
        check8(rd, 8'h00, "lock0 acquired by core0");

        @(posedge clk);  // let sequential logic settle

        // ================================================================
        // TEST 7 — Spinlock contention (held by core 0, core 1 tries)
        // ================================================================
        $display("\n=== TEST 7: Spinlock contention ===");

        mbox_read(2'd1, {4'h6, 8'h00}, rd);  // core 1 tries lock 0
        check8(rd, 8'h01, "lock0 busy for core1");

        // ================================================================
        // TEST 8 — Spinlock release + re-acquire
        // ================================================================
        $display("\n=== TEST 8: Spinlock release + re-acquire ===");

        // Core 0 releases lock 0
        mbox_write(2'd0, {4'h6, 8'h01}, 8'h00);  // SPINLOCK_BASE + lock0 + SLOCK_RELEASE
        @(posedge clk);

        // Core 1 should now be able to acquire
        mbox_read(2'd1, {4'h6, 8'h00}, rd);
        check8(rd, 8'h00, "lock0 acquired by core1");

        @(posedge clk);

        // Core 0 should now see it as busy
        mbox_read(2'd0, {4'h6, 8'h00}, rd);
        check8(rd, 8'h01, "lock0 busy for core0");

        // Clean up: core 1 releases
        mbox_write(2'd1, {4'h6, 8'h01}, 8'h00);
        @(posedge clk);

        // ================================================================
        // TEST 9 — Re-entrant spinlock (same core)
        // ================================================================
        $display("\n=== TEST 9: Re-entrant spinlock ===");

        // Core 2 acquires lock 1
        mbox_read(2'd2, {4'h6, 8'h04}, rd);  // lock 1 at offset 1*4
        check8(rd, 8'h00, "lock1 acquired by core2");
        @(posedge clk);

        // Core 2 tries again — should still see 0 (re-entrant OK: we own it)
        mbox_read(2'd2, {4'h6, 8'h04}, rd);
        check8(rd, 8'h00, "lock1 re-entrant core2");

        // Clean up
        mbox_write(2'd2, {4'h6, 8'h05}, 8'h00);
        @(posedge clk);

        // ================================================================
        // TEST 10 — Multiple spinlocks independence
        // ================================================================
        $display("\n=== TEST 10: Multiple spinlocks ===");

        // Core 0 acquires lock 2
        mbox_read(2'd0, {4'h6, 8'h08}, rd);  // lock 2 at offset 2*4
        check8(rd, 8'h00, "lock2 acq core0");
        @(posedge clk);

        // Core 1 acquires lock 3 (different lock, should succeed)
        mbox_read(2'd1, {4'h6, 8'h0C}, rd);  // lock 3 at offset 3*4
        check8(rd, 8'h00, "lock3 acq core1");
        @(posedge clk);

        // Core 1 tries lock 2 (held by core 0 — busy)
        mbox_read(2'd1, {4'h6, 8'h08}, rd);
        check8(rd, 8'h01, "lock2 busy for core1");

        // Core 0 tries lock 3 (held by core 1 — busy)
        mbox_read(2'd0, {4'h6, 8'h0C}, rd);
        check8(rd, 8'h01, "lock3 busy for core0");

        // Clean up
        mbox_write(2'd0, {4'h6, 8'h09}, 8'h00);  // release lock 2
        mbox_write(2'd1, {4'h6, 8'h0D}, 8'h00);  // release lock 3
        @(posedge clk);

        // ================================================================
        // TEST 11 — Owner tracking
        // ================================================================
        $display("\n=== TEST 11: Owner tracking ===");

        // Core 3 acquires lock 5
        mbox_read(2'd3, {4'h6, 8'h14}, rd);  // lock 5 at offset 5*4
        check8(rd, 8'h00, "lock5 acq core3");
        @(posedge clk);

        // Verify internal owner
        check_true(u_mbox.slock_owner[5] == 2'd3, "lock5 owner=3");

        // Core 0 can't release (not owner)
        mbox_write(2'd0, {4'h6, 8'h15}, 8'h00);
        @(posedge clk);
        check_true(u_mbox.slock_locked[5] == 1'b1, "lock5 still held");

        // Core 3 releases
        mbox_write(2'd3, {4'h6, 8'h15}, 8'h00);
        @(posedge clk);
        check_true(u_mbox.slock_locked[5] == 1'b0, "lock5 released by owner");

        // ================================================================
        // SUMMARY
        // ================================================================
        $display("\n========================================");
        $display("  Mailbox Tests: %0d PASSED, %0d FAILED", pass_cnt, fail_cnt);
        $display("========================================\n");

        if (fail_cnt > 0) $finish(1);
        $finish(0);
    end

    // Global timeout
    initial begin
        #500000;
        $display("GLOBAL TIMEOUT");
        $finish(1);
    end

endmodule
