// ============================================================================
// tb_bus_arbiter.v — Multi-Master Bus Arbiter Tests
// ============================================================================
//
// Tests for mp64_bus.v (4-master round-robin arbiter, 3-state FSM):
//   1. Single core memory read/write (no contention)
//   2. Single core MMIO read (2-cycle path)
//   3. Round-robin fairness — all 4 cores requesting simultaneously
//   4. Core starvation check — verify each core eventually gets granted
//   5. Bus stall — two cores write simultaneously, both complete
//   6. Sequential core access — cores take turns
//   7. Mixed MMIO + memory — both complete correctly
//   8. Grant register stability — doesn't change mid-transaction
//

`timescale 1ns / 1ps
`include "mp64_defs.vh"

module tb_bus_arbiter;

    // ========================================================================
    // Clock / Reset
    // ========================================================================
    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;  // 100 MHz

    // ========================================================================
    // CPU master signals (per-core, packed into flat arrays)
    // ========================================================================
    reg  [NUM_CORES-1:0]        cpu_valid;
    reg  [NUM_CORES*64-1:0]     cpu_addr;
    reg  [NUM_CORES*64-1:0]     cpu_wdata;
    reg  [NUM_CORES-1:0]        cpu_wen;
    reg  [NUM_CORES*2-1:0]      cpu_size;
    wire [NUM_CORES*64-1:0]     cpu_rdata;
    wire [NUM_CORES-1:0]        cpu_ready;

    // ========================================================================
    // Memory slave — 1-cycle registered BRAM model (2 KiB)
    // ========================================================================
    wire        mem_req;
    wire [63:0] mem_addr;
    wire [63:0] mem_wdata;
    wire        mem_wen;
    wire [1:0]  mem_size;
    reg  [63:0] mem_rdata;
    reg         mem_ack;

    reg [63:0] mem_store [0:255];  // 256 × 8 bytes = 2 KiB

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            mem_ack   <= 1'b0;
            mem_rdata <= 64'd0;
        end else begin
            mem_ack <= 1'b0;
            if (mem_req) begin
                mem_ack <= 1'b1;
                if (mem_wen)
                    mem_store[mem_addr[10:3]] <= mem_wdata;
                mem_rdata <= mem_store[mem_addr[10:3]];
            end
        end
    end

    // ========================================================================
    // MMIO slave — combinational (returns address as data)
    // ========================================================================
    wire        mmio_req;
    wire [11:0] mmio_addr;
    wire [63:0] mmio_wdata;
    wire        mmio_wen;
    wire [1:0]  mmio_size;
    wire [63:0] mmio_rdata;
    wire        mmio_ack;

    assign mmio_rdata = {52'd0, mmio_addr};
    assign mmio_ack   = 1'b1;

    // ========================================================================
    // DUT
    // ========================================================================
    mp64_bus u_bus (
        .clk       (clk),
        .rst_n     (rst_n),
        .cpu_valid (cpu_valid),
        .cpu_addr  (cpu_addr),
        .cpu_wdata (cpu_wdata),
        .cpu_wen   (cpu_wen),
        .cpu_size  (cpu_size),
        .cpu_rdata (cpu_rdata),
        .cpu_ready (cpu_ready),
        .mem_req   (mem_req),
        .mem_addr  (mem_addr),
        .mem_wdata (mem_wdata),
        .mem_wen   (mem_wen),
        .mem_size  (mem_size),
        .mem_rdata (mem_rdata),
        .mem_ack   (mem_ack),
        .mmio_req  (mmio_req),
        .mmio_addr (mmio_addr),
        .mmio_wdata(mmio_wdata),
        .mmio_wen  (mmio_wen),
        .mmio_size (mmio_size),
        .mmio_rdata(mmio_rdata),
        .mmio_ack  (mmio_ack)
    );

    // ========================================================================
    // Test helpers
    // ========================================================================
    integer pass_cnt, fail_cnt;
    integer i, c;

    task reset_all;
    begin
        rst_n     = 0;
        cpu_valid = {NUM_CORES{1'b0}};
        cpu_addr  = {(NUM_CORES*64){1'b0}};
        cpu_wdata = {(NUM_CORES*64){1'b0}};
        cpu_wen   = {NUM_CORES{1'b0}};
        cpu_size  = {(NUM_CORES*2){1'b0}};
        for (i = 0; i < 256; i = i + 1)
            mem_store[i] = 64'd0;
        repeat (5) @(posedge clk);
        rst_n = 1;
        @(posedge clk);
    end
    endtask

    task set_core_req;
        input [1:0]  core;
        input [63:0] addr;
        input [63:0] wdata;
        input        wen;
        input [1:0]  sz;
    begin
        cpu_valid[core]          = 1'b1;
        cpu_addr [core*64 +: 64] = addr;
        cpu_wdata[core*64 +: 64] = wdata;
        cpu_wen  [core]          = wen;
        cpu_size [core*2  +: 2]  = sz;
    end
    endtask

    task clear_core_req;
        input [1:0] core;
    begin
        cpu_valid[core] = 1'b0;
        cpu_wen  [core] = 1'b0;
    end
    endtask

    // Wait for a specific core to get cpu_ready, up to max_cycles
    task wait_core_ready;
        input [1:0]  core;
        input integer max_cycles;
        integer cyc;
    begin
        cyc = 0;
        while (!cpu_ready[core] && cyc < max_cycles) begin
            @(posedge clk);
            cyc = cyc + 1;
        end
        if (cyc >= max_cycles)
            $display("  TIMEOUT: core %0d not ready after %0d cycles", core, max_cycles);
    end
    endtask

    task check_val;
        input [63:0]  got;
        input [63:0]  expected;
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

    // ========================================================================
    // Test cases
    // ========================================================================
    reg [63:0] rdata_cap;
    reg [3:0]  grant_seen;

    initial begin
        $dumpfile("tb_bus_arbiter.vcd");
        $dumpvars(0, tb_bus_arbiter);

        pass_cnt = 0;
        fail_cnt = 0;

        // ================================================================
        // TEST 1 — Single core memory write + read (no contention)
        //   Addresses must be 8-byte aligned for BUS_DWORD.
        //   0x100 → mem_store[32].
        // ================================================================
        $display("\n=== TEST 1: Single core memory write + read ===");
        reset_all;

        // Write 0xDEAD_BEEF to address 0x100
        set_core_req(2'd0, 64'h100, 64'hDEAD_BEEF, 1'b1, BUS_DWORD);
        @(posedge clk);
        wait_core_ready(2'd0, 20);
        clear_core_req(2'd0);
        @(posedge clk);
        @(posedge clk);

        // Read back from address 0x100
        set_core_req(2'd0, 64'h100, 64'd0, 1'b0, BUS_DWORD);
        @(posedge clk);
        wait_core_ready(2'd0, 20);
        rdata_cap = cpu_rdata[0*64 +: 64];
        clear_core_req(2'd0);
        check_val(rdata_cap, 64'hDEAD_BEEF, "core0 mem readback");
        @(posedge clk);

        // ================================================================
        // TEST 2 — MMIO read (2-cycle arbiter path)
        //   MMIO stub returns {52'd0, mmio_addr}.
        // ================================================================
        $display("\n=== TEST 2: MMIO read ===");
        reset_all;

        set_core_req(2'd0, {MMIO_HI, 32'h0000_0042}, 64'd0, 1'b0, BUS_DWORD);
        @(posedge clk);
        wait_core_ready(2'd0, 10);
        rdata_cap = cpu_rdata[0*64 +: 64];
        clear_core_req(2'd0);
        check_val(rdata_cap, {52'd0, 12'h042}, "core0 mmio read");
        @(posedge clk);

        // ================================================================
        // TEST 3 — Round-robin fairness: all 4 cores request at once
        //   Each core reads from a different 8-byte-aligned address.
        //   mem_store[0..3] pre-loaded with known patterns.
        // ================================================================
        $display("\n=== TEST 3: Round-robin fairness ===");
        reset_all;

        mem_store[0] = 64'hAAAA_0000;
        mem_store[1] = 64'hBBBB_1111;
        mem_store[2] = 64'hCCCC_2222;
        mem_store[3] = 64'hDDDD_3333;

        // Core 0 → addr 0x000 (index 0)
        // Core 1 → addr 0x008 (index 1)
        // Core 2 → addr 0x010 (index 2)
        // Core 3 → addr 0x018 (index 3)
        set_core_req(2'd0, 64'h000, 64'd0, 1'b0, BUS_DWORD);
        set_core_req(2'd1, 64'h008, 64'd0, 1'b0, BUS_DWORD);
        set_core_req(2'd2, 64'h010, 64'd0, 1'b0, BUS_DWORD);
        set_core_req(2'd3, 64'h018, 64'd0, 1'b0, BUS_DWORD);
        @(posedge clk);

        // Wait for all 4 cores to be served
        grant_seen = 4'b0000;
        for (i = 0; i < 60; i = i + 1) begin
            @(posedge clk);
            for (c = 0; c < NUM_CORES; c = c + 1) begin
                if (cpu_ready[c]) begin
                    grant_seen[c] = 1'b1;
                    clear_core_req(c[1:0]);
                end
            end
            if (grant_seen == 4'b1111) i = 60;
        end

        check_true(grant_seen == 4'b1111, "all 4 cores served");
        check_val(cpu_rdata[0*64 +: 64], 64'hAAAA_0000, "core0 rdata");
        check_val(cpu_rdata[1*64 +: 64], 64'hBBBB_1111, "core1 rdata");
        check_val(cpu_rdata[2*64 +: 64], 64'hCCCC_2222, "core2 rdata");
        check_val(cpu_rdata[3*64 +: 64], 64'hDDDD_3333, "core3 rdata");
        @(posedge clk);

        // ================================================================
        // TEST 4 — Starvation check: repeated contention for 20 rounds
        // ================================================================
        $display("\n=== TEST 4: No starvation (20 rounds) ===");
        reset_all;

        begin : starvation_test
            integer core_count [0:3];
            integer round;
            for (c = 0; c < 4; c = c + 1) core_count[c] = 0;

            for (round = 0; round < 20; round = round + 1) begin
                // All cores request memory reads each round
                for (c = 0; c < 4; c = c + 1)
                    set_core_req(c[1:0], 64'd0, 64'd0, 1'b0, BUS_DWORD);
                @(posedge clk);

                grant_seen = 4'b0000;
                for (i = 0; i < 60; i = i + 1) begin
                    @(posedge clk);
                    for (c = 0; c < NUM_CORES; c = c + 1) begin
                        if (cpu_ready[c] && !grant_seen[c]) begin
                            grant_seen[c] = 1'b1;
                            core_count[c] = core_count[c] + 1;
                            clear_core_req(c[1:0]);
                        end
                    end
                    if (grant_seen == 4'b1111) i = 60;
                end
            end

            // Each core should have been served at least 10 times (50%)
            check_true(core_count[0] >= 10, "core0 served >= 10 times");
            check_true(core_count[1] >= 10, "core1 served >= 10 times");
            check_true(core_count[2] >= 10, "core2 served >= 10 times");
            check_true(core_count[3] >= 10, "core3 served >= 10 times");
        end

        // ================================================================
        // TEST 5 — Two cores write simultaneously, both complete
        //   Core 0 writes to 0x100 (index 32), core 1 to 0x108 (index 33).
        //   Read back both to verify.
        // ================================================================
        $display("\n=== TEST 5: Two cores write ===");
        reset_all;

        set_core_req(2'd0, 64'h100, 64'h1111_2222, 1'b1, BUS_DWORD);
        set_core_req(2'd1, 64'h108, 64'h3333_4444, 1'b1, BUS_DWORD);
        @(posedge clk);

        grant_seen = 4'b0000;
        for (i = 0; i < 30; i = i + 1) begin
            @(posedge clk);
            if (cpu_ready[0]) begin grant_seen[0] = 1'b1; clear_core_req(2'd0); end
            if (cpu_ready[1]) begin grant_seen[1] = 1'b1; clear_core_req(2'd1); end
            if (grant_seen[1:0] == 2'b11) i = 30;
        end
        check_true(grant_seen[1:0] == 2'b11, "both writes completed");
        // Drain any in-flight duplicate transactions from re-grant race
        repeat (6) @(posedge clk);

        // Read back via core 2 (avoid re-grant race on cores 0/1)
        set_core_req(2'd2, 64'h100, 64'd0, 1'b0, BUS_DWORD);
        @(posedge clk);
        wait_core_ready(2'd2, 20);
        check_val(cpu_rdata[2*64 +: 64], 64'h1111_2222, "write0 readback");
        clear_core_req(2'd2);
        repeat (6) @(posedge clk);

        set_core_req(2'd3, 64'h108, 64'd0, 1'b0, BUS_DWORD);
        @(posedge clk);
        wait_core_ready(2'd3, 20);
        check_val(cpu_rdata[3*64 +: 64], 64'h3333_4444, "write1 readback");
        clear_core_req(2'd3);
        @(posedge clk);

        // ================================================================
        // TEST 6 — Sequential: cores take turns one at a time
        //   Addresses: 0x080, 0x088, 0x090, 0x098 (indices 16-19)
        // ================================================================
        $display("\n=== TEST 6: Sequential access ===");
        reset_all;

        mem_store[16] = 64'hAAAA;
        mem_store[17] = 64'hBBBB;
        mem_store[18] = 64'hCCCC;
        mem_store[19] = 64'hDDDD;

        for (c = 0; c < 4; c = c + 1) begin
            set_core_req(c[1:0], 64'h080 + c * 8, 64'd0, 1'b0, BUS_DWORD);
            @(posedge clk);
            wait_core_ready(c[1:0], 20);
            clear_core_req(c[1:0]);
            @(posedge clk);
            @(posedge clk);
        end
        check_val(cpu_rdata[0*64 +: 64], 64'hAAAA, "seq core0");
        check_val(cpu_rdata[1*64 +: 64], 64'hBBBB, "seq core1");
        check_val(cpu_rdata[2*64 +: 64], 64'hCCCC, "seq core2");
        check_val(cpu_rdata[3*64 +: 64], 64'hDDDD, "seq core3");

        // ================================================================
        // TEST 7 — Mixed MMIO + memory (simultaneous)
        //   Core 0 does MMIO read (addr 0x0AA), core 1 does memory read.
        // ================================================================
        $display("\n=== TEST 7: Mixed MMIO + memory ===");
        reset_all;
        mem_store[0] = 64'hFEED_FACE;

        set_core_req(2'd0, {MMIO_HI, 32'h0000_00AA}, 64'd0, 1'b0, BUS_DWORD);
        set_core_req(2'd1, 64'h000, 64'd0, 1'b0, BUS_DWORD);
        @(posedge clk);

        grant_seen = 4'b0000;
        for (i = 0; i < 30; i = i + 1) begin
            @(posedge clk);
            if (cpu_ready[0]) begin grant_seen[0] = 1'b1; clear_core_req(2'd0); end
            if (cpu_ready[1]) begin grant_seen[1] = 1'b1; clear_core_req(2'd1); end
            if (grant_seen[1:0] == 2'b11) i = 30;
        end
        check_true(grant_seen[1:0] == 2'b11, "mmio+mem both done");
        check_val(cpu_rdata[0*64 +: 64], {52'd0, 12'h0AA}, "mmio rdata");
        check_val(cpu_rdata[1*64 +: 64], 64'hFEED_FACE, "mem rdata");

        // ================================================================
        // TEST 8 — Grant stability: doesn't change during transaction
        //   Start core 2 memory read, then assert cores 0+3.
        //   Verify grant stays at core 2 until its ack.
        // ================================================================
        $display("\n=== TEST 8: Grant stability ===");
        reset_all;

        set_core_req(2'd2, 64'h000, 64'd0, 1'b0, BUS_DWORD);
        @(posedge clk);
        @(posedge clk);  // arbiter registers request, now in MEM_WAIT

        // Fire cores 0 and 3 while arbiter is busy
        set_core_req(2'd0, 64'h008, 64'd0, 1'b0, BUS_DWORD);
        set_core_req(2'd3, 64'h010, 64'd0, 1'b0, BUS_DWORD);

        // Monitor grant stability
        begin : grant_check
            reg stable;
            stable = 1'b1;
            for (i = 0; i < 20; i = i + 1) begin
                @(posedge clk);
                // While arbiter is not IDLE, grant must stay at 2
                if (u_bus.arb_state != 2'd0 && u_bus.grant != 2'd2)
                    stable = 1'b0;
                if (cpu_ready[2]) begin
                    clear_core_req(2'd2);
                    i = 20;
                end
            end
            check_true(stable, "grant stable during MEM_WAIT");
        end

        // Clean up remaining requests
        for (i = 0; i < 40; i = i + 1) begin
            @(posedge clk);
            for (c = 0; c < 4; c = c + 1)
                if (cpu_ready[c]) clear_core_req(c[1:0]);
        end

        // ================================================================
        // SUMMARY
        // ================================================================
        $display("\n========================================");
        $display("  Bus Arbiter Tests: %0d PASSED, %0d FAILED", pass_cnt, fail_cnt);
        $display("========================================\n");

        if (fail_cnt > 0) $finish(1);
        $finish(0);
    end

    // Global timeout
    initial begin
        #5000000;
        $display("GLOBAL TIMEOUT — aborting");
        $finish(1);
    end

endmodule
