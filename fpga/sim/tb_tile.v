// ============================================================================
// tb_tile.v — Tile Engine Unit Tests
// ============================================================================
//
// Tests for mp64_tile.v:
//   1. CSR write + readback
//   2. TALU.ADD — two known tiles, verify result
//   3. TRED.SUM — all-0xFF tile, expect 64×255 = 16320
//   4. TRED.MIN / TRED.MAX
//   5. TALU.XOR with imm8 splat
//

`timescale 1ns / 1ps
`include "mp64_defs.vh"

module tb_tile;

    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;

    // === CSR interface ===
    reg         csr_wen;
    reg  [7:0]  csr_addr;
    reg  [63:0] csr_wdata;
    wire [63:0] csr_rdata;

    // === MEX dispatch ===
    reg         mex_valid;
    reg  [1:0]  mex_ss;
    reg  [1:0]  mex_op;
    reg  [2:0]  mex_funct;
    reg  [63:0] mex_gpr_val;
    reg  [7:0]  mex_imm8;
    reg  [3:0]  mex_ext_mod;
    reg         mex_ext_active;
    wire        mex_done;
    wire        mex_busy;

    // === Tile memory port — simple 1-cycle BRAM model ===
    wire        tile_req;
    wire [31:0] tile_addr;
    wire        tile_wen;
    wire [511:0] tile_wdata;
    reg  [511:0] tile_rdata;
    reg          tile_ack;

    // === External tile port (unused — stub) ===
    wire        ext_tile_req;
    wire [63:0] ext_tile_addr;
    wire        ext_tile_wen;
    wire [511:0] ext_tile_wdata;
    reg  [511:0] ext_tile_rdata;
    reg          ext_tile_ack;

    // Simple tile BRAM model (256 tiles × 512 bits = 16 KiB)
    reg [511:0] tile_mem [0:255];

    always @(posedge clk) begin
        tile_ack <= 1'b0;
        if (tile_req) begin
            if (tile_wen)
                tile_mem[tile_addr[13:6]] <= tile_wdata;
            tile_rdata <= tile_mem[tile_addr[13:6]];
            tile_ack   <= 1'b1;
        end
    end

    // DUT
    mp64_tile u_tile (
        .clk           (clk),
        .rst_n         (rst_n),
        .csr_wen       (csr_wen),
        .csr_addr      (csr_addr),
        .csr_wdata     (csr_wdata),
        .csr_rdata     (csr_rdata),
        .mex_valid     (mex_valid),
        .mex_ss        (mex_ss),
        .mex_op        (mex_op),
        .mex_funct     (mex_funct),
        .mex_gpr_val   (mex_gpr_val),
        .mex_imm8      (mex_imm8),
        .mex_ext_mod   (mex_ext_mod),
        .mex_ext_active(mex_ext_active),
        .mex_done      (mex_done),
        .mex_busy      (mex_busy),
        .tile_req      (tile_req),
        .tile_addr     (tile_addr),
        .tile_wen      (tile_wen),
        .tile_wdata    (tile_wdata),
        .tile_rdata    (tile_rdata),
        .tile_ack      (tile_ack),
        .ext_tile_req  (ext_tile_req),
        .ext_tile_addr (ext_tile_addr),
        .ext_tile_wen  (ext_tile_wen),
        .ext_tile_wdata(ext_tile_wdata),
        .ext_tile_rdata(ext_tile_rdata),
        .ext_tile_ack  (ext_tile_ack)
    );

    integer pass_cnt, fail_cnt;
    integer i;

    // === Helper tasks ===

    task csr_write;
        input [7:0]  addr;
        input [63:0] data;
    begin
        @(posedge clk);
        csr_wen   <= 1;
        csr_addr  <= addr;
        csr_wdata <= data;
        @(posedge clk);
        csr_wen <= 0;
    end
    endtask

    task csr_read;
        input  [7:0]  addr;
        output [63:0] data;
    begin
        @(posedge clk);
        csr_addr <= addr;
        @(posedge clk);       // allow combinational mux to settle
        data = csr_rdata;
    end
    endtask

    // Dispatch a MEX instruction and wait for completion
    task mex_dispatch;
        input [1:0] ss;
        input [1:0] op;
        input [2:0] funct;
        input [63:0] gpr;
        input [7:0]  imm;
    begin
        @(posedge clk);
        mex_valid      <= 1;
        mex_ss         <= ss;
        mex_op         <= op;
        mex_funct      <= funct;
        mex_gpr_val    <= gpr;
        mex_imm8       <= imm;
        mex_ext_mod    <= 4'd0;
        mex_ext_active <= 1'b0;
        @(posedge clk);
        mex_valid <= 0;
        // Wait for done
        while (!mex_done) @(posedge clk);
    end
    endtask

    // Dispatch with EXT prefix
    task mex_dispatch_ext;
        input [1:0] ss;
        input [1:0] op;
        input [2:0] funct;
        input [63:0] gpr;
        input [7:0]  imm;
        input [3:0]  ext;
    begin
        @(posedge clk);
        mex_valid      <= 1;
        mex_ss         <= ss;
        mex_op         <= op;
        mex_funct      <= funct;
        mex_gpr_val    <= gpr;
        mex_imm8       <= imm;
        mex_ext_mod    <= ext;
        mex_ext_active <= 1'b1;
        @(posedge clk);
        mex_valid <= 0;
        while (!mex_done) @(posedge clk);
    end
    endtask

    task check64;
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

    task check512;
        input [511:0] got;
        input [511:0] expected;
        input [255:0] label;
    begin
        if (got === expected) begin
            $display("  PASS: %0s", label);
            pass_cnt = pass_cnt + 1;
        end else begin
            $display("  FAIL: %0s", label);
            $display("    got[63:0] = 0x%h  expected[63:0] = 0x%h", got[63:0], expected[63:0]);
            fail_cnt = fail_cnt + 1;
        end
    end
    endtask

    // ========================================================================
    reg [63:0]  rd64;
    reg [511:0] expected_tile;

    initial begin
        $dumpfile("tb_tile.vcd");
        $dumpvars(0, tb_tile);

        pass_cnt  = 0;
        fail_cnt  = 0;
        csr_wen   = 0;
        mex_valid = 0;
        mex_ext_mod    = 4'd0;
        mex_ext_active = 1'b0;
        ext_tile_ack   = 0;
        ext_tile_rdata = 512'd0;
        rst_n = 0;
        repeat (5) @(posedge clk);
        rst_n = 1;
        @(posedge clk);

        // Pre-fill tile memory
        // Tile 0 (addr 0x000): all bytes = 1
        // Tile 1 (addr 0x040): all bytes = 2
        // Tile 2 (addr 0x080): destination (zeroed)
        // Tile 3 (addr 0x0C0): all bytes = 0xFF
        for (i = 0; i < 256; i = i + 1)
            tile_mem[i] = 512'd0;
        tile_mem[0] = {64{8'h01}};   // tile 0: all 0x01
        tile_mem[1] = {64{8'h02}};   // tile 1: all 0x02
        tile_mem[3] = {64{8'hFF}};   // tile 3: all 0xFF

        // ====== TEST 1: CSR write + readback ======
        $display("\n=== TEST 1: CSR write + readback ===");
        csr_write(CSR_TSRC0, 64'h0000_0000_0000_0000);
        csr_write(CSR_TSRC1, 64'h0000_0000_0000_0040);
        csr_write(CSR_TDST,  64'h0000_0000_0000_0080);
        csr_write(CSR_TMODE, 64'd0);  // 8-bit mode, unsigned

        csr_read(CSR_TSRC0, rd64);
        check64(rd64, 64'h0000, "TSRC0");
        csr_read(CSR_TSRC1, rd64);
        check64(rd64, 64'h0040, "TSRC1");
        csr_read(CSR_TDST, rd64);
        check64(rd64, 64'h0080, "TDST");

        // ====== TEST 2: TALU.ADD — tile[0] + tile[1] → tile[2] ======
        $display("\n=== TEST 2: TALU.ADD ===");
        // TSRC0=0x000 (all 0x01), TSRC1=0x040 (all 0x02), TDST=0x080
        // ss=0 (tile B from TSRC1), op=MEX_TALU, funct=TALU_ADD
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd0);
        csr_write(CSR_TCTRL, 64'd0);

        mex_dispatch(2'd0, MEX_TALU, TALU_ADD, 64'd0, 8'd0);

        expected_tile = {64{8'h03}};  // 0x01 + 0x02 = 0x03
        check512(tile_mem[2], expected_tile, "TALU.ADD result");

        // ====== TEST 3: TRED.SUM — tile[3] (all 0xFF), 8-bit mode ======
        // Expected: 64 × 255 = 16320 = 0x3FC0
        $display("\n=== TEST 3: TRED.SUM ===");
        csr_write(CSR_TSRC0, 64'hC0);  // tile 3
        csr_write(CSR_TMODE, 64'd0);
        csr_write(CSR_TCTRL, 64'h02);  // ACC_ZERO = 1

        mex_dispatch(2'd0, MEX_TRED, TRED_SUM, 64'd0, 8'd0);

        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'd16320, "TRED.SUM acc0");

        // ====== TEST 4: TRED.MIN — tile with ascending bytes ======
        $display("\n=== TEST 4: TRED.MIN ===");
        // Put bytes 0x10..0x4F into tile 4
        begin : min_max_setup
            reg [511:0] asc_tile;
            asc_tile = 512'd0;
            for (i = 0; i < 64; i = i + 1)
                asc_tile[i*8 +: 8] = (i + 16) & 8'hFF;  // 0x10..0x4F
            tile_mem[4] = asc_tile;
        end

        csr_write(CSR_TSRC0, 64'h100);  // tile 4 (addr 0x100 = index 4)
        csr_write(CSR_TCTRL, 64'h02);

        mex_dispatch(2'd0, MEX_TRED, TRED_MIN, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'h10, "TRED.MIN acc0");

        // ====== TEST 5: TRED.MAX — same ascending tile ======
        $display("\n=== TEST 5: TRED.MAX ===");
        csr_write(CSR_TCTRL, 64'h02);
        mex_dispatch(2'd0, MEX_TRED, TRED_MAX, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'h4F, "TRED.MAX acc0");

        // ====== TEST 6: TALU.XOR with imm8 splat ======
        // tile[0] XOR 0xAA → each byte 0x01 ^ 0xAA = 0xAB
        $display("\n=== TEST 6: TALU.XOR imm8 splat ===");
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TDST,  64'h80);

        // ss=2 (imm8 splat), op=MEX_TALU, funct=TALU_XOR, imm=0xAA
        mex_dispatch(2'd2, MEX_TALU, TALU_XOR, 64'd0, 8'hAA);

        expected_tile = {64{8'hAB}};
        check512(tile_mem[2], expected_tile, "TALU.XOR splat");

        // ====== TEST 7: TRED.SUM with accumulate mode ======
        // First: SUM of tile[0] (all 0x01) with ACC_ZERO → acc0 = 64
        // Then:  SUM of tile[1] (all 0x02) with ACC (accumulate) → acc0 = 64 + 128 = 192
        $display("\n=== TEST 7: TRED.SUM accumulate ===");
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TCTRL, 64'h02);  // ACC_ZERO
        mex_dispatch(2'd0, MEX_TRED, TRED_SUM, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'd64, "TRED.SUM step1");

        csr_write(CSR_TSRC0, 64'h40);
        csr_write(CSR_TCTRL, 64'h01);  // ACC_ACC (accumulate)
        mex_dispatch(2'd0, MEX_TRED, TRED_SUM, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'd192, "TRED.SUM accum");

        // ====== TEST 8: TALU.ADD 16-bit mode ======
        // tile[0]: all 0x01 → 16-bit lanes each = 0x0101
        // tile[1]: all 0x02 → 16-bit lanes each = 0x0202
        // Expected: 0x0303 per lane
        $display("\n=== TEST 8: TALU.ADD 16-bit ===");
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd1);  // EW=1 → 16-bit
        csr_write(CSR_TCTRL, 64'd0);
        mex_dispatch(2'd0, MEX_TALU, TALU_ADD, 64'd0, 8'd0);
        expected_tile = {32{16'h0303}};
        check512(tile_mem[2], expected_tile, "TALU.ADD 16-bit");

        // ====== TEST 9: TALU.SUB 8-bit ======
        // tile[1] - tile[0] → 0x02 - 0x01 = 0x01
        $display("\n=== TEST 9: TALU.SUB 8-bit ===");
        csr_write(CSR_TSRC0, 64'h40);  // tile 1 as A
        csr_write(CSR_TSRC1, 64'h00);  // tile 0 as B
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd0);
        mex_dispatch(2'd0, MEX_TALU, TALU_SUB, 64'd0, 8'd0);
        expected_tile = {64{8'h01}};
        check512(tile_mem[2], expected_tile, "TALU.SUB result");

        // ====== TEST 10: TALU.ABS 8-bit signed ======
        // Put -5 (0xFB) in every byte of tile 5
        $display("\n=== TEST 10: TALU.ABS signed 8-bit ===");
        tile_mem[5] = {64{8'hFB}};  // -5
        csr_write(CSR_TSRC0, 64'h140);  // tile 5
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'h10);   // signed, 8-bit
        mex_dispatch(2'd3, MEX_TALU, TALU_ABS, 64'd0, 8'd0);  // ss=3 (in-place)
        expected_tile = {64{8'h05}};
        check512(tile_mem[2], expected_tile, "TALU.ABS signed");

        // ====== TEST 11: TMUL.MUL 8-bit ======
        // tile[0] (all 0x01) × tile[1] (all 0x02) = all 0x02
        $display("\n=== TEST 11: TMUL.MUL 8-bit ===");
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd0);
        mex_dispatch(2'd0, MEX_TMUL, TMUL_MUL, 64'd0, 8'd0);
        expected_tile = {64{8'h02}};
        check512(tile_mem[2], expected_tile, "TMUL.MUL result");

        // ====== TEST 12: TMUL.DOT → accumulator ======
        // tile[0] (all 1) · tile[1] (all 2) = 64×(1×2) = 128
        $display("\n=== TEST 12: TMUL.DOT ===");
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TMODE, 64'd0);
        csr_write(CSR_TCTRL, 64'h02);  // ACC_ZERO
        mex_dispatch(2'd0, MEX_TMUL, TMUL_DOT, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'd128, "TMUL.DOT acc0");

        // ====== TEST 13: TSYS.ZERO ======
        $display("\n=== TEST 13: TSYS.ZERO ===");
        tile_mem[6] = {64{8'hAB}};  // fill with garbage
        csr_write(CSR_TDST, 64'h180);  // tile 6
        mex_dispatch(2'd0, MEX_TSYS, TSYS_ZERO, 64'd0, 8'd0);
        check512(tile_mem[6], 512'd0, "TSYS.ZERO");

        // ====== TEST 14: TSYS.MOVBANK ======
        // Copy tile[0] to tile[7]
        $display("\n=== TEST 14: TSYS.MOVBANK ===");
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TDST,  64'h1C0);  // tile 7
        mex_dispatch(2'd0, MEX_TSYS, TSYS_MOVBANK, 64'd0, 8'd0);
        check512(tile_mem[7], {64{8'h01}}, "TSYS.MOVBANK");

        // ====== TEST 15: TSYS.TRANS 8×8 ======
        $display("\n=== TEST 15: TSYS.TRANS ===");
        begin : trans_setup
            reg [511:0] trans_in, trans_exp;
            integer r, col;
            trans_in = 512'd0;
            for (r = 0; r < 8; r = r + 1)
                for (col = 0; col < 8; col = col + 1)
                    trans_in[(r*8+col)*8 +: 8] = r*8 + col;
            tile_mem[8] = trans_in;

            trans_exp = 512'd0;
            for (r = 0; r < 8; r = r + 1)
                for (col = 0; col < 8; col = col + 1)
                    trans_exp[(col*8+r)*8 +: 8] = r*8 + col;

            csr_write(CSR_TDST,  64'h200);  // tile 8 — TRANS reads from TDST
            csr_write(CSR_TMODE, 64'd0);
            mex_dispatch(2'd0, MEX_TSYS, TSYS_TRANS, 64'd0, 8'd0);
            check512(tile_mem[8], trans_exp, "TSYS.TRANS 8x8");
        end

        // ====== TEST 16: TRED.POPCNT 8-bit ======
        // tile[3] = all 0xFF → 64 bytes × 8 bits = 512
        $display("\n=== TEST 16: TRED.POPCNT ===");
        csr_write(CSR_TSRC0, 64'hC0);
        csr_write(CSR_TMODE, 64'd0);
        csr_write(CSR_TCTRL, 64'h02);
        mex_dispatch(2'd0, MEX_TRED, TRED_POPC, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'd512, "TRED.POPCNT");

        // ====== TEST 17: TRED.MINIDX 8-bit ======
        // tile[4] has ascending bytes 0x10..0x4F → min is 0x10 at index 0
        $display("\n=== TEST 17: TRED.MINIDX ===");
        csr_write(CSR_TSRC0, 64'h100);
        csr_write(CSR_TCTRL, 64'h02);
        mex_dispatch(2'd0, MEX_TRED, TRED_MINIDX, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'd0, "TRED.MINIDX idx");
        csr_read(CSR_ACC1, rd64);
        check64(rd64, 64'h10, "TRED.MINIDX val");

        // ====== TEST 18: TRED.MAXIDX 8-bit ======
        $display("\n=== TEST 18: TRED.MAXIDX ===");
        csr_write(CSR_TCTRL, 64'h02);
        mex_dispatch(2'd0, MEX_TRED, TRED_MAXIDX, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'd63, "TRED.MAXIDX idx");
        csr_read(CSR_ACC1, rd64);
        check64(rd64, 64'h4F, "TRED.MAXIDX val");

        // ====== TEST 19: TALU.ADD 32-bit mode ======
        $display("\n=== TEST 19: TALU.ADD 32-bit ===");
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd2);  // 32-bit
        mex_dispatch(2'd0, MEX_TALU, TALU_ADD, 64'd0, 8'd0);
        expected_tile = {16{32'h03030303}};  // 0x01010101 + 0x02020202
        check512(tile_mem[2], expected_tile, "TALU.ADD 32-bit");

        // ====== TEST 20: TALU.ADD 64-bit mode ======
        $display("\n=== TEST 20: TALU.ADD 64-bit ===");
        csr_write(CSR_TMODE, 64'd3);  // 64-bit
        mex_dispatch(2'd0, MEX_TALU, TALU_ADD, 64'd0, 8'd0);
        expected_tile = {8{64'h0303030303030303}};
        check512(tile_mem[2], expected_tile, "TALU.ADD 64-bit");

        // ====== TEST 21: TRED.L1 (Manhattan norm) 8-bit unsigned ======
        // tile[0] all 0x01 → L1 = 64
        $display("\n=== TEST 21: TRED.L1 ===");
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TMODE, 64'd0);
        csr_write(CSR_TCTRL, 64'h02);
        mex_dispatch(2'd0, MEX_TRED, TRED_L1, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'd64, "TRED.L1");

        // ====== TEST 22: TRED.SUMSQ 8-bit ======
        // tile[1] all 0x02 → sum(4) = 64 × 4 = 256
        $display("\n=== TEST 22: TRED.SUMSQ ===");
        csr_write(CSR_TSRC0, 64'h40);
        csr_write(CSR_TCTRL, 64'h02);
        mex_dispatch(2'd0, MEX_TRED, TRED_SUMSQ, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'd256, "TRED.SUMSQ");

        // ====== TEST 23: EXT.8 TALU — VSHL 8-bit ======
        // tile[0] all 0x01, shift left by 3 → 0x08
        $display("\n=== TEST 23: EXT.8 VSHL ===");
        tile_mem[9] = {64{8'h03}};  // shift amount = 3
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h240);  // tile 9 = shift amounts
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd0);
        mex_dispatch_ext(2'd0, MEX_TALU, 3'd1, 64'd0, 8'd0, 4'd8);  // VSHL
        expected_tile = {64{8'h08}};  // 0x01 << 3 = 0x08
        check512(tile_mem[2], expected_tile, "EXT.8 VSHL");

        // ====== TEST 24: TMUL.MAC 8-bit ======
        // MAC: dst = dst + a*b = 0x08 + 0x01*0x02 = 0x0A
        $display("\n=== TEST 24: TMUL.MAC ===");
        csr_write(CSR_TSRC0, 64'h00);  // all 0x01
        csr_write(CSR_TSRC1, 64'h40);  // all 0x02
        csr_write(CSR_TDST,  64'h80);  // tile 2 (currently all 0x08 from VSHL)
        csr_write(CSR_TMODE, 64'd0);
        mex_dispatch(2'd0, MEX_TMUL, TMUL_MAC, 64'd0, 8'd0);
        expected_tile = {64{8'h0A}};  // 0x08 + 0x01*0x02 = 0x0A
        check512(tile_mem[2], expected_tile, "TMUL.MAC");

        // ================================================================
        //  FP16 / BF16 TESTS
        // ================================================================

        // ====== TEST 25: FP16 TALU.ADD — 1.0 + 2.0 = 3.0 ======
        // FP16: 1.0 = 0x3C00, 2.0 = 0x4000, 3.0 = 0x4200
        $display("\n=== TEST 25: FP16 TALU.ADD ===");
        tile_mem[0]  = {32{16'h3C00}};  // 32 lanes of 1.0
        tile_mem[1]  = {32{16'h4000}};  // 32 lanes of 2.0
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd4);    // FP16 mode
        mex_dispatch(2'd0, MEX_TALU, TALU_ADD, 64'd0, 8'd0);
        expected_tile = {32{16'h4200}};  // 32 lanes of 3.0
        check512(tile_mem[2], expected_tile, "FP16 TALU.ADD 1.0+2.0=3.0");

        // ====== TEST 26: FP16 TALU.SUB — 3.0 - 1.0 = 2.0 ======
        $display("\n=== TEST 26: FP16 TALU.SUB ===");
        tile_mem[0]  = {32{16'h4200}};  // 3.0
        tile_mem[1]  = {32{16'h3C00}};  // 1.0
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd4);
        mex_dispatch(2'd0, MEX_TALU, TALU_SUB, 64'd0, 8'd0);
        expected_tile = {32{16'h4000}};  // 2.0
        check512(tile_mem[2], expected_tile, "FP16 TALU.SUB 3.0-1.0=2.0");

        // ====== TEST 27: FP16 TALU.MIN — min(3.0, 1.0) = 1.0 ======
        $display("\n=== TEST 27: FP16 TALU.MIN ===");
        tile_mem[0]  = {32{16'h4200}};  // 3.0
        tile_mem[1]  = {32{16'h3C00}};  // 1.0
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd4);
        mex_dispatch(2'd0, MEX_TALU, TALU_MIN, 64'd0, 8'd0);
        expected_tile = {32{16'h3C00}};  // 1.0
        check512(tile_mem[2], expected_tile, "FP16 TALU.MIN min(3,1)=1");

        // ====== TEST 28: FP16 TALU.MAX — max(3.0, 1.0) = 3.0 ======
        $display("\n=== TEST 28: FP16 TALU.MAX ===");
        csr_write(CSR_TMODE, 64'd4);
        mex_dispatch(2'd0, MEX_TALU, TALU_MAX, 64'd0, 8'd0);
        expected_tile = {32{16'h4200}};  // 3.0
        check512(tile_mem[2], expected_tile, "FP16 TALU.MAX max(3,1)=3");

        // ====== TEST 29: FP16 TALU.ABS — abs(-2.0) = 2.0 ======
        // FP16 -2.0 = 0xC000
        $display("\n=== TEST 29: FP16 TALU.ABS ===");
        tile_mem[0]  = {32{16'hC000}};  // -2.0
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd4);
        mex_dispatch(2'd3, MEX_TALU, TALU_ABS, 64'd0, 8'd0);  // ss=3 in-place
        expected_tile = {32{16'h4000}};  // 2.0
        check512(tile_mem[2], expected_tile, "FP16 TALU.ABS abs(-2)=2");

        // ====== TEST 30: FP16 TMUL.MUL — 2.0 × 3.0 = 6.0 ======
        // FP16: 6.0 = 0x4600
        $display("\n=== TEST 30: FP16 TMUL.MUL ===");
        tile_mem[0]  = {32{16'h4000}};  // 2.0
        tile_mem[1]  = {32{16'h4200}};  // 3.0
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd4);
        mex_dispatch(2'd0, MEX_TMUL, TMUL_MUL, 64'd0, 8'd0);
        expected_tile = {32{16'h4600}};  // 6.0
        check512(tile_mem[2], expected_tile, "FP16 TMUL.MUL 2*3=6");

        // ====== TEST 31: FP16 TMUL.DOT — 32 × (1.0 × 1.0) = 32.0 ======
        // FP32 32.0 = 0x42000000
        $display("\n=== TEST 31: FP16 TMUL.DOT ===");
        tile_mem[0]  = {32{16'h3C00}};  // 1.0
        tile_mem[1]  = {32{16'h3C00}};  // 1.0
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TMODE, 64'd4);
        csr_write(CSR_TCTRL, 64'h02);  // ACC_ZERO
        mex_dispatch(2'd0, MEX_TMUL, TMUL_DOT, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'h0000000042000000, "FP16 DOT 32×(1×1)=32.0");

        // ====== TEST 32: FP16 TRED.SUM — 32 × 2.0 = 64.0 ======
        // FP32 64.0 = 0x42800000
        $display("\n=== TEST 32: FP16 TRED.SUM ===");
        tile_mem[0]  = {32{16'h4000}};  // 2.0
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TMODE, 64'd4);
        csr_write(CSR_TCTRL, 64'h02);  // ACC_ZERO
        mex_dispatch(2'd0, MEX_TRED, TRED_SUM, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        check64(rd64, 64'h0000000042800000, "FP16 TRED.SUM 32×2=64.0");

        // ====== TEST 33: FP16 NaN propagation ======
        // NaN + 1.0 = NaN
        $display("\n=== TEST 33: FP16 NaN propagation ===");
        tile_mem[0]  = {32{16'h7E00}};  // qNaN
        tile_mem[1]  = {32{16'h3C00}};  // 1.0
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd4);
        mex_dispatch(2'd0, MEX_TALU, TALU_ADD, 64'd0, 8'd0);
        expected_tile = {32{16'h7E00}};  // qNaN
        check512(tile_mem[2], expected_tile, "FP16 NaN+1 = NaN");

        // ====== TEST 34: BF16 TALU.ADD — 1.0 + 2.0 = 3.0 ======
        // BF16: 1.0=0x3F80, 2.0=0x4000, 3.0=0x4040
        $display("\n=== TEST 34: BF16 TALU.ADD ===");
        tile_mem[0]  = {32{16'h3F80}};  // bfloat16 1.0
        tile_mem[1]  = {32{16'h4000}};  // bfloat16 2.0
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd5);    // BF16 mode
        mex_dispatch(2'd0, MEX_TALU, TALU_ADD, 64'd0, 8'd0);
        expected_tile = {32{16'h4040}};  // bfloat16 3.0
        check512(tile_mem[2], expected_tile, "BF16 TALU.ADD 1.0+2.0=3.0");

        // ====== TEST 35: BF16 TMUL.MUL — 2.0 × 3.0 = 6.0 ======
        // BF16: 6.0=0x40C0
        $display("\n=== TEST 35: BF16 TMUL.MUL ===");
        tile_mem[0]  = {32{16'h4000}};  // 2.0
        tile_mem[1]  = {32{16'h4040}};  // 3.0
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd5);
        mex_dispatch(2'd0, MEX_TMUL, TMUL_MUL, 64'd0, 8'd0);
        expected_tile = {32{16'h40C0}};  // 6.0
        check512(tile_mem[2], expected_tile, "BF16 TMUL.MUL 2*3=6");

        // ====== TEST 36: FP16 TRED.MIN — min across lanes ======
        $display("\n=== TEST 36: FP16 TRED.MIN ===");
        tile_mem[0] = {32{16'h4200}};  // fill with 3.0
        tile_mem[0][0 +: 16] = 16'h3C00;  // lane 0 = 1.0 (minimum)
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TMODE, 64'd4);
        csr_write(CSR_TCTRL, 64'h02);
        mex_dispatch(2'd0, MEX_TRED, TRED_MIN, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        // FP32 1.0 = 0x3F800000
        check64(rd64, 64'h000000003F800000, "FP16 TRED.MIN = 1.0");

        // ====== TEST 37: FP16 TRED.MAX — max across lanes ======
        $display("\n=== TEST 37: FP16 TRED.MAX ===");
        tile_mem[0] = {32{16'h3C00}};  // fill with 1.0
        tile_mem[0][31*16 +: 16] = 16'h4500;  // lane 31 = 5.0
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TMODE, 64'd4);
        csr_write(CSR_TCTRL, 64'h02);
        mex_dispatch(2'd0, MEX_TRED, TRED_MAX, 64'd0, 8'd0);
        csr_read(CSR_ACC0, rd64);
        // FP32 5.0 = 0x40A00000
        check64(rd64, 64'h0000000040A00000, "FP16 TRED.MAX = 5.0");

        // ====== TEST 38: FP16 TMUL.MAC — c + a*b ======
        // MAC: 1.0 + 2.0*3.0 = 7.0 (FP16 7.0 = 0x4700)
        $display("\n=== TEST 38: FP16 TMUL.MAC ===");
        tile_mem[0]  = {32{16'h4000}};  // a = 2.0
        tile_mem[1]  = {32{16'h4200}};  // b = 3.0
        tile_mem[2]  = {32{16'h3C00}};  // dst (c) = 1.0
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd4);
        mex_dispatch(2'd0, MEX_TMUL, TMUL_MAC, 64'd0, 8'd0);
        expected_tile = {32{16'h4700}};  // 7.0
        check512(tile_mem[2], expected_tile, "FP16 TMUL.MAC 1+2*3=7");

        // ====== TEST 39: FP16 bitwise AND (raw bits) ======
        $display("\n=== TEST 39: FP16 bitwise AND ===");
        tile_mem[0] = {32{16'hFFFF}};
        tile_mem[1] = {32{16'h7C00}};  // +inf mask
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd4);
        mex_dispatch(2'd0, MEX_TALU, TALU_AND, 64'd0, 8'd0);
        expected_tile = {32{16'h7C00}};
        check512(tile_mem[2], expected_tile, "FP16 bitwise AND");

        // ====== TEST 40: VSHR unsigned without rounding ======
        $display("\n=== TEST 40: VSHR unsigned (no rounding) ===");
        tile_mem[0] = {64{8'h07}};   // all 7
        tile_mem[1] = {64{8'h01}};   // shift by 1
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd0);  // 8-bit, unsigned, no rounding
        mex_dispatch_ext(2'd0, MEX_TALU, 3'd0, 64'd0, 8'd0, 4'd8);
        expected_tile = {64{8'h03}};  // 7 >> 1 = 3
        check512(tile_mem[2], expected_tile, "VSHR 7>>1=3 (no round)");

        // ====== TEST 41: VSHR unsigned with rounding ======
        $display("\n=== TEST 41: VSHR unsigned (rounding) ===");
        // tmode[6] = rounding bit  →  tmode = 64'h40 = bit 6 set
        csr_write(CSR_TMODE, 64'h40);
        mex_dispatch_ext(2'd0, MEX_TALU, 3'd0, 64'd0, 8'd0, 4'd8);
        expected_tile = {64{8'h04}};  // (7 + 1) >> 1 = 4 (round half-up)
        check512(tile_mem[2], expected_tile, "VSHR 7>>1=4 (rounded)");

        // ====== TEST 42: VSHR signed with rounding ======
        $display("\n=== TEST 42: VSHR signed (rounding) ===");
        tile_mem[0] = {64{8'hFD}};   // -3 in signed 8-bit
        tile_mem[1] = {64{8'h01}};   // shift by 1
        csr_write(CSR_TMODE, 64'h50);  // bit6=rounding, bit4=signed
        mex_dispatch_ext(2'd0, MEX_TALU, 3'd0, 64'd0, 8'd0, 4'd8);
        // (-3 + 1) >> 1 = -2 >> 1 = -1 = 0xFF
        expected_tile = {64{8'hFF}};
        check512(tile_mem[2], expected_tile, "VSHR -3>>1=-1 (signed+round)");

        // ====== TEST 43: VSHR signed without rounding ======
        $display("\n=== TEST 43: VSHR signed (no rounding) ===");
        csr_write(CSR_TMODE, 64'h10);  // bit4=signed, no rounding
        mex_dispatch_ext(2'd0, MEX_TALU, 3'd0, 64'd0, 8'd0, 4'd8);
        // -3 >>> 1 = -2 = 0xFE (arithmetic right shift)
        expected_tile = {64{8'hFE}};
        check512(tile_mem[2], expected_tile, "VSHR -3>>1=-2 (signed,no round)");

        // ====== TEST 44: RROT row-rotate-left by 1 (8-bit) ======
        $display("\n=== TEST 44: RROT row-left by 1 ===");
        // Set up tile[0] as an identity-like pattern:
        // Row 0: 00 01 02 03 04 05 06 07
        // Row 1: 08 09 0A 0B 0C 0D 0E 0F
        // ... (ascending bytes 0x00..0x3F)
        for (i = 0; i < 64; i = i + 1)
            tile_mem[0][i*8 +: 8] = i[7:0];
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd0);  // 8-bit mode
        // imm8 = ctrl byte: dir=0 (row-left), amt=1 (bits[4:2]=001), mirror=0
        // imm8 = 0b000_001_00 = 0x04
        mex_dispatch(2'd0, MEX_TSYS, TSYS_RROT, 64'd0, 8'h04);
        // After row-left by 1: each row rotated left by 1
        // Row 0: 01 02 03 04 05 06 07 00
        // Row 1: 09 0A 0B 0C 0D 0E 0F 08
        // etc.
        for (i = 0; i < 64; i = i + 1) begin
            expected_tile[i*8 +: 8] = ((i / 8) * 8 + ((i % 8 + 1) % 8));
        end
        check512(tile_mem[2], expected_tile, "RROT row-left by 1");

        // ====== TEST 45: RROT vertical mirror (8-bit) ======
        $display("\n=== TEST 45: RROT vertical mirror ===");
        // Same source tile (ascending 0x00..0x3F)
        // imm8: mirror=1 (bit5), dir[0]=1 (vertical), amt=don't care
        // imm8 = 0b1_000_01 = 0x21
        mex_dispatch(2'd0, MEX_TSYS, TSYS_RROT, 64'd0, 8'h21);
        // Vertical mirror: row 0 ↔ row 7, row 1 ↔ row 6, etc.
        // Row 0 of result = Row 7 of source = 38 39 3A 3B 3C 3D 3E 3F
        // Row 7 of result = Row 0 of source = 00 01 02 03 04 05 06 07
        for (i = 0; i < 64; i = i + 1) begin
            expected_tile[i*8 +: 8] = ((7 - i / 8) * 8 + (i % 8));
        end
        check512(tile_mem[2], expected_tile, "RROT vertical mirror");

        // ====== TEST 46: RROT col-rotate-up by 2 (8-bit) ======
        $display("\n=== TEST 46: RROT col-up by 2 ===");
        // imm8: dir=2 (col-up), amt=2 (bits[4:2]=010), mirror=0
        // imm8 = 0b000_010_10 = 0x0A
        mex_dispatch(2'd0, MEX_TSYS, TSYS_RROT, 64'd0, 8'h0A);
        // Col-up by 2: each column rotated up by 2 rows
        // dst[r][c] = src[(r+2)%8][c]
        for (i = 0; i < 64; i = i + 1) begin
            expected_tile[i*8 +: 8] = (((i / 8 + 2) % 8) * 8 + (i % 8));
        end
        check512(tile_mem[2], expected_tile, "RROT col-up by 2");

        // ====== TEST 47: LOAD2D — strided gather (4×8 from 128B stride) ======
        $display("\n=== TEST 47: LOAD2D strided gather ===");
        // Set up a "framebuffer" in BRAM tiles: 2 tiles wide (128 bytes/row)
        // Tiles 4..7 form a 4-tile = 256-byte region = 2 rows of 128 bytes
        // Tile 4 (addr 0x100): bytes 0x10..0x4F (row 0, cols 0-63)
        // Tile 5 (addr 0x140): bytes 0x50..0x8F (row 0, cols 64-127)
        // Tile 6 (addr 0x180): bytes 0xA0..0xDF (row 1, cols 0-63)
        // Tile 7 (addr 0x1C0): bytes 0xE0..0x1F (row 1, cols 64-127)
        for (i = 0; i < 64; i = i + 1) begin
            tile_mem[4][i*8 +: 8] = (8'h10 + i) & 8'hFF;
            tile_mem[5][i*8 +: 8] = (8'h50 + i) & 8'hFF;
            tile_mem[6][i*8 +: 8] = (8'hA0 + i) & 8'hFF;
            tile_mem[7][i*8 +: 8] = (8'hE0 + i) & 8'hFF;
        end
        // Set cursor: bank=0, row=0, col=0, stride=4 (4 tiles per row = 256 bytes)
        // Cursor address = bank*4M + (row*stride + col) * 64 = 0
        // We want to start at tile 4 = address 0x100
        // row=1, col=0, stride=4: cursor = (1*4+0)*64 = 256 = 0x100
        csr_write(CSR_SB, 64'd0);
        csr_write(CSR_SR, 64'd1);     // row 1
        csr_write(CSR_SC, 64'd0);     // col 0
        csr_write(CSR_SW, 64'd4);     // stride 4 tiles
        // LOAD2D params: stride_r=128 bytes (2 tiles), h=2, w=8
        csr_write(CSR_TSTRIDE_R, 64'd128);
        csr_write(CSR_TTILE_H,   64'd2);
        csr_write(CSR_TTILE_W,   64'd8);
        csr_write(CSR_TDST, 64'h80);  // destination tile 2
        mex_dispatch_ext(2'd0, MEX_TSYS, 3'd0, 64'd0, 8'd0, 4'd8);  // LOAD2D
        // Expected: 2 rows of 8 bytes packed contiguously into tile 2
        // Row 0: tile 4, bytes 0-7 = 0x10 0x11 ... 0x17
        // Row 1: addr 0x100+128=0x180 = tile 6, bytes 0-7 = 0xA0 0xA1 ... 0xA7
        // Result: bytes 0-7: 10..17, bytes 8-15: A0..A7, rest 0
        expected_tile = 512'd0;
        for (i = 0; i < 8; i = i + 1) begin
            expected_tile[i*8 +: 8] = (8'h10 + i);       // row 0
            expected_tile[(8+i)*8 +: 8] = (8'hA0 + i);   // row 1
        end
        check512(tile_mem[2], expected_tile, "LOAD2D 2x8 stride=128");

        // ====== TEST 48: STORE2D — strided scatter ======
        $display("\n=== TEST 48: STORE2D strided scatter ===");
        // Write a known pattern into tile 0 (TSRC0)
        for (i = 0; i < 64; i = i + 1)
            tile_mem[0][i*8 +: 8] = (8'hB0 + i) & 8'hFF;
        csr_write(CSR_TSRC0, 64'h00);  // source tile 0
        // Cursor → tile 12 (addr 0x300), stride_r=64, h=2, w=8
        csr_write(CSR_SR, 64'd0);
        csr_write(CSR_SC, 64'd12);     // col 12
        csr_write(CSR_SW, 64'd16);     // stride 16 tiles (arbitrary, just for address calc)
        // cursor = (0*16 + 12) * 64 = 768 = 0x300 = tile 12
        csr_write(CSR_TSTRIDE_R, 64'd64);   // each row is in consecutive tiles
        csr_write(CSR_TTILE_H,   64'd2);
        csr_write(CSR_TTILE_W,   64'd8);
        // Clear target tiles
        tile_mem[12] = 512'hDEAD;  // non-zero to verify RMW
        tile_mem[13] = 512'hBEEF;
        mex_dispatch_ext(2'd0, MEX_TSYS, 3'd1, 64'd0, 8'd0, 4'd8);  // STORE2D
        // Expected: tile 12 bytes 0-7 = 0xB0..0xB7, rest = 0 (from DEAD fill)
        //           tile 13 bytes 0-7 = 0xB8..0xBF, rest = 0 (from BEEF fill)
        begin
            reg ok;
            ok = 1;
            for (i = 0; i < 8; i = i + 1) begin
                if (tile_mem[12][i*8 +: 8] !== (8'hB0 + i)) ok = 0;
                if (tile_mem[13][i*8 +: 8] !== (8'hB8 + i)) ok = 0;
            end
            if (ok) begin
                $display("  PASS: STORE2D 2x8 stride=64");
                pass_cnt = pass_cnt + 1;
            end else begin
                $display("  FAIL: STORE2D 2x8 stride=64");
                $display("    tile12[63:0]=0x%h", tile_mem[12][63:0]);
                $display("    tile13[63:0]=0x%h", tile_mem[13][63:0]);
                fail_cnt = fail_cnt + 1;
            end
        end

        // ====== TEST 49: VSEL 8-bit — MSB-based conditional select ======
        $display("\n=== TEST 49: VSEL 8-bit ===");
        // tile A (src0) = 0x42 in every lane
        // tile B (src1) = alternating 0x80 (MSB set → select A) and 0x01 (MSB clear → 0)
        tile_mem[0] = {64{8'h42}};
        begin
            for (i = 0; i < 64; i = i + 1)
                tile_mem[1][i*8 +: 8] = (i & 1) ? 8'h80 : 8'h01;
        end
        csr_write(CSR_TSRC0, 64'h00);      // tile 0
        csr_write(CSR_TSRC1, 64'h40);      // tile 1  (addr = 1 * 64 = 64 = 0x40)
        csr_write(CSR_TDST,  64'h80);      // tile 2
        csr_write(CSR_TMODE, 64'd0);       // unsigned, no rounding
        mex_dispatch_ext(2'd0, MEX_TALU, 3'd2, 64'd0, 8'd0, 4'd8);  // VSEL
        begin
            reg ok;
            ok = 1;
            for (i = 0; i < 64; i = i + 1) begin
                if (i & 1) begin
                    // B MSB set → result = A = 0x42
                    if (tile_mem[2][i*8 +: 8] !== 8'h42) ok = 0;
                end else begin
                    // B MSB clear → result = 0
                    if (tile_mem[2][i*8 +: 8] !== 8'h00) ok = 0;
                end
            end
            if (ok) begin
                $display("  PASS: VSEL 8-bit alternating mask");
                pass_cnt = pass_cnt + 1;
            end else begin
                $display("  FAIL: VSEL 8-bit");
                $display("    tile2[63:0]=0x%h", tile_mem[2][63:0]);
                fail_cnt = fail_cnt + 1;
            end
        end

        // ====== TEST 50: VSEL 16-bit ======
        $display("\n=== TEST 50: VSEL 16-bit ===");
        // tile A = 0x1234 in every 16-bit lane
        tile_mem[0] = {32{16'h1234}};
        // tile B: even lanes MSB set (0x8000), odd lanes MSB clear (0x0001)
        begin
            for (i = 0; i < 32; i = i + 1)
                tile_mem[1][i*16 +: 16] = (i & 1) ? 16'h0001 : 16'h8000;
        end
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd1);  // 16-bit element width
        mex_dispatch_ext(2'd0, MEX_TALU, 3'd2, 64'd0, 8'd0, 4'd8);  // VSEL 16-bit
        begin
            reg ok;
            ok = 1;
            for (i = 0; i < 32; i = i + 1) begin
                if (i & 1) begin
                    if (tile_mem[2][i*16 +: 16] !== 16'h0000) ok = 0;
                end else begin
                    if (tile_mem[2][i*16 +: 16] !== 16'h1234) ok = 0;
                end
            end
            if (ok) begin
                $display("  PASS: VSEL 16-bit");
                pass_cnt = pass_cnt + 1;
            end else begin
                $display("  FAIL: VSEL 16-bit");
                fail_cnt = fail_cnt + 1;
            end
        end

        // ====== TEST 51: VCLZ 8-bit — count leading zeros ======
        $display("\n=== TEST 51: VCLZ 8-bit ===");
        // Known values: 0xFF→0, 0x40→1, 0x20→2, 0x10→3, 0x08→4, 0x04→5, 0x01→7, 0x00→8
        tile_mem[0][7:0]   = 8'hFF;  // clz = 0
        tile_mem[0][15:8]  = 8'h40;  // clz = 1
        tile_mem[0][23:16] = 8'h20;  // clz = 2
        tile_mem[0][31:24] = 8'h10;  // clz = 3
        tile_mem[0][39:32] = 8'h08;  // clz = 4
        tile_mem[0][47:40] = 8'h04;  // clz = 5
        tile_mem[0][55:48] = 8'h01;  // clz = 7
        tile_mem[0][63:56] = 8'h00;  // clz = 8
        // Fill rest with 0x01 (clz=7)
        for (i = 8; i < 64; i = i + 1)
            tile_mem[0][i*8 +: 8] = 8'h01;
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h00);  // unused for VCLZ but set anyway
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd0);
        mex_dispatch_ext(2'd0, MEX_TALU, 3'd3, 64'd0, 8'd0, 4'd8);  // VCLZ
        begin
            reg ok;
            ok = 1;
            if (tile_mem[2][7:0]   !== 8'd0) ok = 0;  // clz(0xFF) = 0
            if (tile_mem[2][15:8]  !== 8'd1) ok = 0;  // clz(0x40) = 1
            if (tile_mem[2][23:16] !== 8'd2) ok = 0;  // clz(0x20) = 2
            if (tile_mem[2][31:24] !== 8'd3) ok = 0;  // clz(0x10) = 3
            if (tile_mem[2][39:32] !== 8'd4) ok = 0;  // clz(0x08) = 4
            if (tile_mem[2][47:40] !== 8'd5) ok = 0;  // clz(0x04) = 5
            if (tile_mem[2][55:48] !== 8'd7) ok = 0;  // clz(0x01) = 7
            if (tile_mem[2][63:56] !== 8'd8) ok = 0;  // clz(0x00) = 8
            if (ok) begin
                $display("  PASS: VCLZ 8-bit");
                pass_cnt = pass_cnt + 1;
            end else begin
                $display("  FAIL: VCLZ 8-bit");
                $display("    tile2[63:0]=0x%h", tile_mem[2][63:0]);
                fail_cnt = fail_cnt + 1;
            end
        end

        // ====== TEST 52: VCLZ 32-bit ======
        $display("\n=== TEST 52: VCLZ 32-bit ===");
        tile_mem[0][31:0]    = 32'h0000_0001;  // clz = 31
        tile_mem[0][63:32]   = 32'h8000_0000;  // clz = 0
        tile_mem[0][95:64]   = 32'h0000_0100;  // clz = 23
        tile_mem[0][127:96]  = 32'h0000_0000;  // clz = 32
        // Fill rest with 0x1
        for (i = 4; i < 16; i = i + 1)
            tile_mem[0][i*32 +: 32] = 32'h0000_0001;
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd2);  // 32-bit element width
        mex_dispatch_ext(2'd0, MEX_TALU, 3'd3, 64'd0, 8'd0, 4'd8);  // VCLZ 32-bit
        begin
            reg ok;
            ok = 1;
            if (tile_mem[2][31:0]    !== 32'd31) ok = 0;
            if (tile_mem[2][63:32]   !== 32'd0)  ok = 0;
            if (tile_mem[2][95:64]   !== 32'd23) ok = 0;
            if (tile_mem[2][127:96]  !== 32'd32) ok = 0;
            if (ok) begin
                $display("  PASS: VCLZ 32-bit");
                pass_cnt = pass_cnt + 1;
            end else begin
                $display("  FAIL: VCLZ 32-bit");
                $display("    tile2[127:0]=0x%h", tile_mem[2][127:0]);
                fail_cnt = fail_cnt + 1;
            end
        end

        // ====== TEST 53: VSEL 64-bit — all MSB set ======
        $display("\n=== TEST 53: VSEL 64-bit all-select ===");
        tile_mem[0] = {8{64'hDEAD_BEEF_CAFE_F00D}};
        tile_mem[1] = {8{64'h8000_0000_0000_0001}};  // MSB set → select A
        csr_write(CSR_TSRC0, 64'h00);
        csr_write(CSR_TSRC1, 64'h40);
        csr_write(CSR_TDST,  64'h80);
        csr_write(CSR_TMODE, 64'd3);  // 64-bit element width
        mex_dispatch_ext(2'd0, MEX_TALU, 3'd2, 64'd0, 8'd0, 4'd8);  // VSEL 64-bit
        expected_tile = {8{64'hDEAD_BEEF_CAFE_F00D}};
        check512(tile_mem[2], expected_tile, "VSEL 64-bit all-select");

        // ====== SUMMARY ======
        $display("\n========================================");
        $display("  Tile Tests: %0d PASSED, %0d FAILED", pass_cnt, fail_cnt);
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
