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
    wire [19:0] tile_addr;
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
