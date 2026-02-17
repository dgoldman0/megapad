// ============================================================================
// tb_memory.v — Testbench for mp64_memory
// ============================================================================
//
// Negedge-driven methodology.
//
// Tests:
//   1.  Reset clears acks
//   2.  CPU dword write + read-back (Bank 0)
//   3.  CPU byte write + read-back (sub-word RMW)
//   4.  CPU half write + read-back
//   5.  CPU word write + read-back
//   6.  Tile 512-bit write + read-back (Bank 0)
//   7.  HBW bank 1 CPU write + read
//   8.  HBW bank 2 tile write + read
//   9.  External forward (address outside all banks)
//  10.  Concurrent tile + CPU on different banks
//

`timescale 1ns/1ps

module tb_memory;

    // ========================================================================
    // Parameters
    // ========================================================================
    localparam CLK_HALF   = 5;

    // Use small banks for simulation speed
    localparam BANK_DEPTH = 64;
    localparam ADDR_W_T   = 6;   // log2(64)
    localparam ADDR_W_C   = 9;   // log2(64*8)

    // ========================================================================
    // Signals
    // ========================================================================
    reg          clk, rst_n;

    reg          cpu_req;
    reg  [63:0]  cpu_addr;
    reg  [63:0]  cpu_wdata;
    reg          cpu_wen;
    reg  [1:0]   cpu_size;
    wire [63:0]  cpu_rdata;
    wire         cpu_ack;

    reg          tile_req;
    reg  [31:0]  tile_addr;
    reg          tile_wen;
    reg  [511:0] tile_wdata;
    wire [511:0] tile_rdata;
    wire         tile_ack;

    wire         ext_req;
    wire [63:0]  ext_addr;
    wire [63:0]  ext_wdata;
    wire         ext_wen;
    wire [1:0]   ext_size;
    reg  [63:0]  ext_rdata;
    reg          ext_ack;

    // ========================================================================
    // Assertions
    // ========================================================================
    integer pass_count, fail_count, test_num;

    task check;
        input [511:0] label;
        input         cond;
        begin
            test_num = test_num + 1;
            if (cond) begin
                pass_count = pass_count + 1;
                $display("  [PASS %0d] %0s", test_num, label);
            end else begin
                fail_count = fail_count + 1;
                $display("  [FAIL %0d] %0s", test_num, label);
            end
        end
    endtask

    // ========================================================================
    // Clock
    // ========================================================================
    initial clk = 0;
    always #CLK_HALF clk = ~clk;

    // ========================================================================
    // DUT
    // ========================================================================
    mp64_memory #(
        .BANK_DEPTH  (BANK_DEPTH),
        .ADDR_W_TILE (ADDR_W_T),
        .ADDR_W_CPU  (ADDR_W_C)
    ) dut (
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

    // ========================================================================
    // Helper tasks
    // ========================================================================

    task reset;
        begin
            rst_n      <= 1'b0;
            cpu_req    <= 1'b0;
            cpu_addr   <= 64'd0;
            cpu_wdata  <= 64'd0;
            cpu_wen    <= 1'b0;
            cpu_size   <= 2'd0;
            tile_req   <= 1'b0;
            tile_addr  <= 32'd0;
            tile_wen   <= 1'b0;
            tile_wdata <= 512'd0;
            ext_rdata  <= 64'd0;
            ext_ack    <= 1'b0;
            @(negedge clk);
            @(negedge clk);
            @(negedge clk);
            rst_n <= 1'b1;
            @(negedge clk);
        end
    endtask

    // CPU request (holds until ack)
    task cpu_write;
        input [63:0] addr;
        input [63:0] wdata;
        input [1:0]  sz;
        integer wd;
        begin
            @(negedge clk);
            cpu_req   <= 1'b1;
            cpu_addr  <= addr;
            cpu_wdata <= wdata;
            cpu_wen   <= 1'b1;
            cpu_size  <= sz;
            wd = 0;
            while (!cpu_ack && wd < 50) begin
                @(posedge clk); #1;
                wd = wd + 1;
            end
            @(negedge clk);
            cpu_req <= 1'b0;
            cpu_wen <= 1'b0;
        end
    endtask

    task cpu_read;
        input [63:0] addr;
        input [1:0]  sz;
        output [63:0] rdata;
        integer wd;
        begin
            @(negedge clk);
            cpu_req   <= 1'b1;
            cpu_addr  <= addr;
            cpu_wdata <= 64'd0;
            cpu_wen   <= 1'b0;
            cpu_size  <= sz;
            wd = 0;
            while (!cpu_ack && wd < 50) begin
                @(posedge clk); #1;
                wd = wd + 1;
            end
            rdata = cpu_rdata;
            @(negedge clk);
            cpu_req <= 1'b0;
        end
    endtask

    task tile_write;
        input [31:0]  addr;
        input [511:0] wdata;
        integer wd;
        begin
            @(negedge clk);
            tile_req   <= 1'b1;
            tile_addr  <= addr;
            tile_wdata <= wdata;
            tile_wen   <= 1'b1;
            wd = 0;
            while (!tile_ack && wd < 50) begin
                @(posedge clk); #1;
                wd = wd + 1;
            end
            @(negedge clk);
            tile_req <= 1'b0;
            tile_wen <= 1'b0;
        end
    endtask

    task tile_read;
        input [31:0]   addr;
        output [511:0] rdata;
        integer wd;
        begin
            @(negedge clk);
            tile_req  <= 1'b1;
            tile_addr <= addr;
            tile_wen  <= 1'b0;
            wd = 0;
            while (!tile_ack && wd < 50) begin
                @(posedge clk); #1;
                wd = wd + 1;
            end
            rdata = tile_rdata;
            @(negedge clk);
            tile_req <= 1'b0;
        end
    endtask

    // ========================================================================
    // Bus size constants
    // ========================================================================
    localparam [1:0] SZ_BYTE  = 2'd0;
    localparam [1:0] SZ_HALF  = 2'd1;
    localparam [1:0] SZ_WORD  = 2'd2;
    localparam [1:0] SZ_DWORD = 2'd3;

    // ========================================================================
    // Main test sequence
    // ========================================================================
    reg [63:0]  rd64;
    reg [511:0] rd512;

    initial begin
        $dumpfile("tb_memory.vcd");
        $dumpvars(0, tb_memory);

        pass_count = 0;
        fail_count = 0;
        test_num   = 0;

        $display("=== tb_memory ===");

        // ---- Test 1: Reset ----
        $display("--- Test 1: Reset ---");
        reset;
        @(posedge clk); #1;
        check("cpu_ack clear after reset",  cpu_ack  == 1'b0);
        check("tile_ack clear after reset", tile_ack == 1'b0);
        check("ext_req clear after reset",  ext_req  == 1'b0);

        // ---- Test 2: CPU dword write + read (Bank 0) ----
        $display("--- Test 2: CPU dword write+read ---");
        // Write 64 bits at address 0x40 (row 1, word 0 in tiny bank)
        cpu_write(64'h0000_0000_0000_0040, 64'hAAAA_BBBB_CCCC_DDDD, SZ_DWORD);
        cpu_read(64'h0000_0000_0000_0040, SZ_DWORD, rd64);
        check("dword readback", rd64 == 64'hAAAA_BBBB_CCCC_DDDD);

        // ---- Test 3: CPU byte write + read ----
        $display("--- Test 3: CPU byte RMW ---");
        // First write a known dword, then overwrite one byte
        cpu_write(64'h0000_0000_0000_0080, 64'h0102_0304_0506_0708, SZ_DWORD);
        // Overwrite byte at offset 2 (addr 0x82) with 0xFF
        cpu_write(64'h0000_0000_0000_0082, 64'h00FF, SZ_BYTE);
        cpu_read(64'h0000_0000_0000_0080, SZ_DWORD, rd64);
        check("byte RMW correct", rd64 == 64'h0102_0304_05FF_0708);

        // ---- Test 4: CPU half write ----
        $display("--- Test 4: CPU half RMW ---");
        cpu_write(64'h0000_0000_0000_00C0, 64'hFFFF_FFFF_FFFF_FFFF, SZ_DWORD);
        // Overwrite halfword at offset 4 (addr 0xC4)
        cpu_write(64'h0000_0000_0000_00C4, 64'h1234, SZ_HALF);
        cpu_read(64'h0000_0000_0000_00C0, SZ_DWORD, rd64);
        check("half RMW correct", rd64 == 64'hFFFF_1234_FFFF_FFFF);

        // ---- Test 5: CPU word write ----
        $display("--- Test 5: CPU word RMW ---");
        cpu_write(64'h0000_0000_0000_0100, 64'h0000_0000_0000_0000, SZ_DWORD);
        // Overwrite upper 32 bits (offset 4)
        cpu_write(64'h0000_0000_0000_0104, 64'hDEAD_BEEF, SZ_WORD);
        cpu_read(64'h0000_0000_0000_0100, SZ_DWORD, rd64);
        check("word RMW correct", rd64 == 64'hDEAD_BEEF_0000_0000);

        // ---- Test 6: Tile 512-bit write + read (Bank 0) ----
        $display("--- Test 6: Tile write+read ---");
        begin : blk_t6
            reg [511:0] pattern;
            integer i;
            pattern = 512'd0;
            for (i = 0; i < 8; i = i + 1)
                pattern[i*64 +: 64] = {32'd0, i[31:0]} + 64'hA000;
            tile_write(32'h0000_0000, pattern);
            tile_read(32'h0000_0000, rd512);
            check("tile 512b readback", rd512 == pattern);
        end

        // ---- Test 7: HBW Bank 1 ----
        $display("--- Test 7: HBW Bank 1 ---");
        // Bank 1 base: 0xFFD0_0000
        cpu_write(64'h0000_0000_FFD0_0000, 64'hBEEF_FACE, SZ_DWORD);
        cpu_read(64'h0000_0000_FFD0_0000, SZ_DWORD, rd64);
        check("HBW bank 1 readback", rd64 == 64'hBEEF_FACE);

        // ---- Test 8: HBW Bank 2 tile ----
        $display("--- Test 8: HBW Bank 2 tile ---");
        tile_write(32'hFFE0_0040, {8{64'hCAFE_0000_0000_0001}});
        tile_read(32'hFFE0_0040, rd512);
        check("HBW bank 2 tile readback", rd512 == {8{64'hCAFE_0000_0000_0001}});

        // ---- Test 9: External forward ----
        $display("--- Test 9: External forward ---");
        @(negedge clk);
        cpu_req   <= 1'b1;
        cpu_addr  <= 64'h0000_0000_8000_0000;  // outside all banks
        cpu_wdata <= 64'd0;
        cpu_wen   <= 1'b0;
        cpu_size  <= SZ_DWORD;

        begin : blk_t9
            integer wd;
            wd = 0;
            while (!ext_req && wd < 50) begin
                @(posedge clk); #1;
                wd = wd + 1;
            end
        end
        check("ext_req asserted", ext_req == 1'b1);
        check("ext_addr correct", ext_addr == 64'h0000_0000_8000_0000);

        // Drive ext response and wait for FSM to process it
        @(negedge clk);
        ext_rdata <= 64'hE00E_0A10;
        ext_ack   <= 1'b1;
        // Posedge: FSM processes ext_ack → cpu_ack<=1, cpu_state<=IDLE
        @(posedge clk); #1;
        check("ext rdata returned", cpu_rdata == 64'hE00E_0A10);
        // Clear cpu_req and ext_ack before next posedge to prevent FSM re-entry
        @(negedge clk);
        ext_ack <= 1'b0;
        cpu_req <= 1'b0;
        @(negedge clk);

        // ---- Test 10: Concurrent tile + CPU different banks ----
        $display("--- Test 10: Concurrent access ---");
        // Write to bank 0 via CPU, bank 1 via tile concurrently
        @(negedge clk);
        cpu_req    <= 1'b1;
        cpu_addr   <= 64'h0000_0000_0000_0140;
        cpu_wdata  <= 64'hC00C_0000;
        cpu_wen    <= 1'b1;
        cpu_size   <= SZ_DWORD;
        tile_req   <= 1'b1;
        tile_addr  <= 32'hFFD0_0080;
        tile_wdata <= {8{64'h1111_C0CC}};
        tile_wen   <= 1'b1;

        begin : blk_t10
            integer wd;
            reg cpu_done, tile_done;
            cpu_done  = 0;
            tile_done = 0;
            wd = 0;
            while ((!cpu_done || !tile_done) && wd < 50) begin
                @(posedge clk); #1;
                if (cpu_ack) cpu_done = 1;
                if (tile_ack) tile_done = 1;
                wd = wd + 1;
            end
            check("concurrent CPU ack",  cpu_done  == 1);
            check("concurrent tile ack", tile_done == 1);
        end
        @(negedge clk);
        cpu_req  <= 1'b0;
        cpu_wen  <= 1'b0;
        tile_req <= 1'b0;
        tile_wen <= 1'b0;
        @(negedge clk);

        // Verify both writes stuck
        cpu_read(64'h0000_0000_0000_0140, SZ_DWORD, rd64);
        check("concurrent CPU data", rd64 == 64'hC00C_0000);

        tile_read(32'hFFD0_0080, rd512);
        check("concurrent tile data", rd512 == {8{64'h1111_C0CC}});

        // ================================================================
        // Summary
        // ================================================================
        $display("");
        $display("=== tb_memory: %0d passed, %0d failed ===",
                 pass_count, fail_count);
        if (fail_count > 0)
            $display("*** FAILURES DETECTED ***");

        #100;
        $finish;
    end

    // Timeout
    initial begin
        #500000;
        $display("TIMEOUT");
        $finish;
    end

endmodule
