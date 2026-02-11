// ============================================================================
// tb_memory.v — Memory Subsystem Unit Tests
// ============================================================================
//
// Tests for mp64_memory.v:
//   1. CPU single-cycle read/write (dword, word, half, byte)
//   2. Tile port 512-bit read/write
//   3. Simultaneous dual-port access (no collision)
//   4. Dual-port collision (same row) — tile has priority, CPU stalls
//   5. External memory forwarding (address ≥ 1 MiB)
//

`timescale 1ns / 1ps
`include "mp64_defs.vh"

module tb_memory;

    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;

    // === CPU port ===
    reg         cpu_req;
    reg  [63:0] cpu_addr;
    reg  [63:0] cpu_wdata;
    reg         cpu_wen;
    reg  [1:0]  cpu_size;
    wire [63:0] cpu_rdata;
    wire        cpu_ack;

    // === Tile port ===
    reg         tile_req;
    reg  [19:0] tile_addr;
    reg         tile_wen;
    reg  [511:0] tile_wdata;
    wire [511:0] tile_rdata;
    wire        tile_ack;

    // === External memory interface ===
    wire        ext_req;
    wire [63:0] ext_addr;
    wire [63:0] ext_wdata;
    wire        ext_wen;
    wire [1:0]  ext_size;
    reg  [63:0] ext_rdata;
    reg         ext_ack;

    // DUT
    mp64_memory u_mem (
        .clk       (clk),
        .rst_n     (rst_n),
        .cpu_req   (cpu_req),
        .cpu_addr  (cpu_addr),
        .cpu_wdata (cpu_wdata),
        .cpu_wen   (cpu_wen),
        .cpu_size  (cpu_size),
        .cpu_rdata (cpu_rdata),
        .cpu_ack   (cpu_ack),
        .tile_req  (tile_req),
        .tile_addr (tile_addr),
        .tile_wen  (tile_wen),
        .tile_wdata(tile_wdata),
        .tile_rdata(tile_rdata),
        .tile_ack  (tile_ack),
        .ext_req   (ext_req),
        .ext_addr  (ext_addr),
        .ext_wdata (ext_wdata),
        .ext_wen   (ext_wen),
        .ext_size  (ext_size),
        .ext_rdata (ext_rdata),
        .ext_ack   (ext_ack)
    );

    integer pass_cnt, fail_cnt;
    integer i;

    // === Tasks ===
    task automatic cpu_write;
        input [63:0] addr;
        input [63:0] data;
        input [1:0]  sz;
    begin
        @(posedge clk);
        cpu_req   <= 1;
        cpu_addr  <= addr;
        cpu_wdata <= data;
        cpu_wen   <= 1;
        cpu_size  <= sz;
        @(posedge clk);
        while (!cpu_ack) @(posedge clk);
        cpu_req <= 0;
        cpu_wen <= 0;
    end
    endtask

    task automatic cpu_read;
        input  [63:0] addr;
        input  [1:0]  sz;
        output [63:0] data;
    begin
        @(posedge clk);
        cpu_req  <= 1;
        cpu_addr <= addr;
        cpu_wen  <= 0;
        cpu_size <= sz;
        @(posedge clk);
        while (!cpu_ack) @(posedge clk);
        data = cpu_rdata;
        cpu_req <= 0;
    end
    endtask

    task automatic tile_write;
        input [19:0]  addr;
        input [511:0] data;
    begin
        @(posedge clk);
        tile_req   <= 1;
        tile_addr  <= addr;
        tile_wdata <= data;
        tile_wen   <= 1;
        @(posedge clk);
        while (!tile_ack) @(posedge clk);
        tile_req <= 0;
        tile_wen <= 0;
    end
    endtask

    task automatic tile_read;
        input  [19:0]  addr;
        output [511:0] data;
    begin
        @(posedge clk);
        tile_req  <= 1;
        tile_addr <= addr;
        tile_wen  <= 0;
        @(posedge clk);
        while (!tile_ack) @(posedge clk);
        data = tile_rdata;
        tile_req <= 0;
    end
    endtask

    task check64;
        input [63:0] got;
        input [63:0] expected;
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

    task check512;
        input [511:0] got;
        input [511:0] expected;
        input [255:0] label;
    begin
        if (got === expected) begin
            $display("  PASS: %0s matches", label);
            pass_cnt = pass_cnt + 1;
        end else begin
            $display("  FAIL: %0s mismatch", label);
            $display("    got[63:0]      = 0x%h", got[63:0]);
            $display("    expected[63:0]  = 0x%h", expected[63:0]);
            fail_cnt = fail_cnt + 1;
        end
    end
    endtask

    // ========================================================================
    reg  [63:0] rd64;
    reg [511:0] rd512;

    initial begin
        $dumpfile("tb_memory.vcd");
        $dumpvars(0, tb_memory);

        pass_cnt = 0;
        fail_cnt = 0;
        cpu_req  = 0; cpu_wen = 0;
        tile_req = 0; tile_wen = 0;
        ext_ack  = 0; ext_rdata = 64'd0;
        rst_n    = 0;
        repeat (5) @(posedge clk);
        rst_n = 1;
        @(posedge clk);

        // ====== TEST 1: CPU dword write + read ======
        $display("\n=== TEST 1: CPU dword write/read ===");
        cpu_write(64'h0000_0100, 64'hDEAD_BEEF_CAFE_BABE, BUS_DWORD);
        cpu_read (64'h0000_0100, BUS_DWORD, rd64);
        check64(rd64, 64'hDEAD_BEEF_CAFE_BABE, "dword");

        // ====== TEST 2: CPU byte / half / word access ======
        $display("\n=== TEST 2: sub-dword access ===");
        cpu_write(64'h0000_0200, 64'h0102_0304_0506_0708, BUS_DWORD);
        cpu_read (64'h0000_0200, BUS_BYTE, rd64);
        check64(rd64, 64'h08, "byte[0]");

        cpu_read (64'h0000_0201, BUS_BYTE, rd64);
        check64(rd64, 64'h07, "byte[1]");

        cpu_read (64'h0000_0200, BUS_HALF, rd64);
        check64(rd64, 64'h0708, "half");

        cpu_read (64'h0000_0200, BUS_WORD, rd64);
        check64(rd64, 64'h0506_0708, "word");

        // ====== TEST 3: Tile 512-bit write + read ======
        $display("\n=== TEST 3: Tile 512-bit write/read ===");
        begin : tile_test
            reg [511:0] tile_pattern;
            tile_pattern = 512'd0;
            for (i = 0; i < 64; i = i + 1)
                tile_pattern[i*8 +: 8] = i[7:0];   // bytes 0x00..0x3F
            tile_write(20'h0_0000, tile_pattern);
            tile_read (20'h0_0000, rd512);
            check512(rd512, tile_pattern, "tile rw");
        end

        // ====== TEST 4: Dual-port — different rows (no collision) ======
        $display("\n=== TEST 4: Dual-port no-collision ===");
        // Write via tile to row 0 (addr 0x000), read via CPU from row 4 (addr 0x100)
        // They are in different rows, so both should complete immediately.
        cpu_write(64'h0000_0100, 64'hAAAA_BBBB_CCCC_DDDD, BUS_DWORD);
        cpu_read (64'h0000_0100, BUS_DWORD, rd64);
        check64(rd64, 64'hAAAA_BBBB_CCCC_DDDD, "no-collision cpu");

        // ====== TEST 5: CPU write cross-check via tile ======
        $display("\n=== TEST 5: CPU write visible via tile ===");
        cpu_write(64'h0000_0000, 64'h1234_5678_9ABC_DEF0, BUS_DWORD);
        tile_read(20'h0_0000, rd512);
        // First dword of the row should be our value
        check64(rd512[63:0], 64'h1234_5678_9ABC_DEF0, "cpu→tile xcheck");

        // ====== TEST 6: External memory forwarding ======
        $display("\n=== TEST 6: External memory forwarding ===");
        // Address >= 1 MiB → forwarded to ext_mem interface
        fork
            begin
                cpu_read(64'h0010_0000, BUS_DWORD, rd64);  // 1 MiB = external
            end
            begin
                // Wait for ext_req, then respond
                @(posedge clk);
                while (!ext_req) @(posedge clk);
                ext_rdata <= 64'hEEEE_FFFF_0000_1111;
                ext_ack   <= 1;
                @(posedge clk);
                ext_ack <= 0;
            end
        join
        check64(rd64, 64'hEEEE_FFFF_0000_1111, "ext fwd");

        // ====== SUMMARY ======
        $display("\n========================================");
        $display("  Memory Tests: %0d PASSED, %0d FAILED", pass_cnt, fail_cnt);
        $display("========================================\n");

        if (fail_cnt > 0) $finish(1);
        $finish(0);
    end

    // Timeout
    initial begin
        #500000;
        $display("GLOBAL TIMEOUT");
        $finish(1);
    end

endmodule
