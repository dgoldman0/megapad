// ============================================================================
// tb_memory.v — Banked Memory Subsystem Unit Tests
// ============================================================================
//
// Tests for mp64_memory.v with 4-bank architecture:
//   1. CPU dword read/write — Bank 0 (system BRAM)
//   2. CPU sub-dword access (byte/half/word)
//   3. Tile 512-bit read/write — Bank 0 (legacy)
//   4. Dual-port no collision (different banks)
//   5. CPU write → tile read cross-check (Bank 0)
//   6. External memory forwarding (address between banks)
//   7. HBW Bank 1 CPU read/write (0xFFD0_xxxx)
//   8. HBW Bank 2 CPU read/write (0xFFE0_xxxx)
//   9. HBW Bank 3 CPU read/write (0xFFF0_xxxx)
//  10. HBW tile 512-bit read/write (Bank 1)
//  11. Bank isolation (write to one, others unchanged)
//  12. HBW cross-bank tile operations
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

    // === Tile port (32-bit address) ===
    reg         tile_req;
    reg  [31:0] tile_addr;
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
        input [31:0]  addr;
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
        input  [31:0]  addr;
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

        // ====== TEST 1: CPU dword write + read (Bank 0) ======
        $display("\n=== TEST 1: CPU dword write/read (Bank 0) ===");
        cpu_write(64'h0000_0100, 64'hDEAD_BEEF_CAFE_BABE, BUS_DWORD);
        cpu_read (64'h0000_0100, BUS_DWORD, rd64);
        check64(rd64, 64'hDEAD_BEEF_CAFE_BABE, "bank0 dword");

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

        // ====== TEST 3: Tile 512-bit write + read (Bank 0) ======
        $display("\n=== TEST 3: Tile 512-bit write/read (Bank 0) ===");
        begin : tile_test
            reg [511:0] tile_pattern;
            tile_pattern = 512'd0;
            for (i = 0; i < 64; i = i + 1)
                tile_pattern[i*8 +: 8] = i[7:0];
            tile_write(32'h0000_0000, tile_pattern);
            tile_read (32'h0000_0000, rd512);
            check512(rd512, tile_pattern, "bank0 tile rw");
        end

        // ====== TEST 4: Dual-port no collision (different rows) ======
        $display("\n=== TEST 4: Dual-port no-collision ===");
        cpu_write(64'h0000_0100, 64'hAAAA_BBBB_CCCC_DDDD, BUS_DWORD);
        cpu_read (64'h0000_0100, BUS_DWORD, rd64);
        check64(rd64, 64'hAAAA_BBBB_CCCC_DDDD, "no-collision cpu");

        // ====== TEST 5: CPU write cross-check via tile ======
        $display("\n=== TEST 5: CPU write visible via tile ===");
        cpu_write(64'h0000_0000, 64'h1234_5678_9ABC_DEF0, BUS_DWORD);
        tile_read(32'h0000_0000, rd512);
        check64(rd512[63:0], 64'h1234_5678_9ABC_DEF0, "cpu->tile xcheck");

        // ====== TEST 6: External memory forwarding ======
        $display("\n=== TEST 6: External memory forwarding ===");
        // Address 0x0010_0000 is between Bank 0 and HBW — external
        fork
            begin
                cpu_read(64'h0010_0000, BUS_DWORD, rd64);
            end
            begin
                @(posedge clk);
                while (!ext_req) @(posedge clk);
                ext_rdata <= 64'hEEEE_FFFF_0000_1111;
                ext_ack   <= 1;
                @(posedge clk);
                ext_ack <= 0;
            end
        join
        check64(rd64, 64'hEEEE_FFFF_0000_1111, "ext fwd");

        // ====== TEST 7: HBW Bank 1 CPU read/write (0xFFD0_xxxx) ======
        $display("\n=== TEST 7: HBW Bank 1 CPU read/write ===");
        cpu_write(64'h0000_0000_FFD0_0000, 64'hB100_B100_B100_B100, BUS_DWORD);
        cpu_read (64'h0000_0000_FFD0_0000, BUS_DWORD, rd64);
        check64(rd64, 64'hB100_B100_B100_B100, "bank1 dword");

        cpu_write(64'h0000_0000_FFD0_0100, 64'hB1FF_B1FF_B1FF_B1FF, BUS_DWORD);
        cpu_read (64'h0000_0000_FFD0_0100, BUS_DWORD, rd64);
        check64(rd64, 64'hB1FF_B1FF_B1FF_B1FF, "bank1 offset");

        // ====== TEST 8: HBW Bank 2 CPU read/write (0xFFE0_xxxx) ======
        $display("\n=== TEST 8: HBW Bank 2 CPU read/write ===");
        cpu_write(64'h0000_0000_FFE0_0000, 64'hB200_AAAA_BBBB_CCCC, BUS_DWORD);
        cpu_read (64'h0000_0000_FFE0_0000, BUS_DWORD, rd64);
        check64(rd64, 64'hB200_AAAA_BBBB_CCCC, "bank2 dword");

        // ====== TEST 9: HBW Bank 3 CPU read/write (0xFFF0_xxxx) ======
        $display("\n=== TEST 9: HBW Bank 3 CPU read/write ===");
        cpu_write(64'h0000_0000_FFF0_0000, 64'hB300_DEAD_FACE_CAFE, BUS_DWORD);
        cpu_read (64'h0000_0000_FFF0_0000, BUS_DWORD, rd64);
        check64(rd64, 64'hB300_DEAD_FACE_CAFE, "bank3 dword");

        // ====== TEST 10: HBW tile 512-bit read/write (Bank 1) ======
        $display("\n=== TEST 10: HBW tile 512-bit read/write (Bank 1) ===");
        begin : hbw_tile
            reg [511:0] hbw_pattern;
            hbw_pattern = 512'd0;
            for (i = 0; i < 64; i = i + 1)
                hbw_pattern[i*8 +: 8] = (64 + i) & 8'hFF;
            tile_write(32'hFFD0_0040, hbw_pattern);  // offset 0x40 = row 1
            tile_read (32'hFFD0_0040, rd512);
            check512(rd512, hbw_pattern, "bank1 tile rw");
        end

        // ====== TEST 11: Bank isolation ======
        $display("\n=== TEST 11: Bank isolation ===");
        // Write unique values to addr 0x0008 in each bank
        cpu_write(64'h0000_0008, 64'hB0B0_B0B0_B0B0_B0B0, BUS_DWORD);            // Bank 0
        cpu_write(64'h0000_0000_FFD0_0008, 64'hB1B1_B1B1_B1B1_B1B1, BUS_DWORD);  // Bank 1
        cpu_write(64'h0000_0000_FFE0_0008, 64'hB2B2_B2B2_B2B2_B2B2, BUS_DWORD);  // Bank 2
        cpu_write(64'h0000_0000_FFF0_0008, 64'hB3B3_B3B3_B3B3_B3B3, BUS_DWORD);  // Bank 3

        // Read back — each should have its own value
        cpu_read(64'h0000_0008, BUS_DWORD, rd64);
        check64(rd64, 64'hB0B0_B0B0_B0B0_B0B0, "iso bank0");
        cpu_read(64'h0000_0000_FFD0_0008, BUS_DWORD, rd64);
        check64(rd64, 64'hB1B1_B1B1_B1B1_B1B1, "iso bank1");
        cpu_read(64'h0000_0000_FFE0_0008, BUS_DWORD, rd64);
        check64(rd64, 64'hB2B2_B2B2_B2B2_B2B2, "iso bank2");
        cpu_read(64'h0000_0000_FFF0_0008, BUS_DWORD, rd64);
        check64(rd64, 64'hB3B3_B3B3_B3B3_B3B3, "iso bank3");

        // ====== TEST 12: HBW CPU write → tile read cross-check ======
        $display("\n=== TEST 12: HBW CPU→tile cross-check ===");
        cpu_write(64'h0000_0000_FFE0_0000, 64'hCB01_CB02_CB03_CB04, BUS_DWORD);
        tile_read(32'hFFE0_0000, rd512);
        check64(rd512[63:0], 64'hCB01_CB02_CB03_CB04, "hbw cpu->tile");

        // ====== SUMMARY ======
        $display("\n========================================");
        $display("  Memory Tests: %0d PASSED, %0d FAILED", pass_cnt, fail_cnt);
        $display("========================================\n");

        if (fail_cnt > 0) $finish(1);
        $finish(0);
    end

    // Timeout
    initial begin
        #1000000;
        $display("GLOBAL TIMEOUT");
        $finish(1);
    end

endmodule
