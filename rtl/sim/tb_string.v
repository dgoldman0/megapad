// ============================================================================
// tb_string.v — Smoke testbench for EXT.STRING engine (prefix F9)
// ============================================================================
// Exercises all 5 sub-ops of the Forth-Aware String Engine via the major
// CPU core: CMOVE, CMOVE>, BFILL, BCOMP, BSRCH.
//
// Uses the same combinational RAM + I-cache stub as tb_cpu_smoke.
//
`timescale 1ns / 1ps

`include "mp64_pkg.vh"

module tb_string;

    `include "mp64_cpu_funcs.vh"

    // ====================================================================
    // Clock + reset
    // ====================================================================
    reg clk, rst;
    initial clk = 0;
    always #5 clk = ~clk;           // 100 MHz

    // ====================================================================
    // Bus model (4 KiB combinational RAM)
    // ====================================================================
    reg [7:0] mem [0:4095];

    wire        bus_valid;
    wire [63:0] bus_addr;
    wire [63:0] bus_wdata;
    wire        bus_wen;
    wire [1:0]  bus_size;
    reg  [63:0] bus_rdata;
    reg         bus_ready;

    always @(negedge clk) begin
        bus_ready <= 1'b0;
        bus_rdata <= 64'd0;
        if (bus_valid) begin
            bus_ready <= 1'b1;
            if (bus_wen) begin
                case (bus_size)
                    BUS_BYTE:  mem[bus_addr[11:0]] <= bus_wdata[7:0];
                    BUS_HALF: begin
                        mem[bus_addr[11:0]]   <= bus_wdata[15:8];
                        mem[bus_addr[11:0]+1] <= bus_wdata[7:0];
                    end
                    BUS_WORD: begin
                        mem[bus_addr[11:0]]   <= bus_wdata[31:24];
                        mem[bus_addr[11:0]+1] <= bus_wdata[23:16];
                        mem[bus_addr[11:0]+2] <= bus_wdata[15:8];
                        mem[bus_addr[11:0]+3] <= bus_wdata[7:0];
                    end
                    BUS_DWORD: begin
                        mem[bus_addr[11:0]]   <= bus_wdata[63:56];
                        mem[bus_addr[11:0]+1] <= bus_wdata[55:48];
                        mem[bus_addr[11:0]+2] <= bus_wdata[47:40];
                        mem[bus_addr[11:0]+3] <= bus_wdata[39:32];
                        mem[bus_addr[11:0]+4] <= bus_wdata[31:24];
                        mem[bus_addr[11:0]+5] <= bus_wdata[23:16];
                        mem[bus_addr[11:0]+6] <= bus_wdata[15:8];
                        mem[bus_addr[11:0]+7] <= bus_wdata[7:0];
                    end
                endcase
            end else begin
                case (bus_size)
                    BUS_BYTE: bus_rdata <= {56'd0, mem[bus_addr[11:0]]};
                    BUS_HALF: bus_rdata <= {48'd0, mem[bus_addr[11:0]],
                                                   mem[bus_addr[11:0]+1]};
                    BUS_WORD: bus_rdata <= {32'd0, mem[bus_addr[11:0]],
                                                   mem[bus_addr[11:0]+1],
                                                   mem[bus_addr[11:0]+2],
                                                   mem[bus_addr[11:0]+3]};
                    BUS_DWORD: bus_rdata <= {mem[bus_addr[11:0]],
                                             mem[bus_addr[11:0]+1],
                                             mem[bus_addr[11:0]+2],
                                             mem[bus_addr[11:0]+3],
                                             mem[bus_addr[11:0]+4],
                                             mem[bus_addr[11:0]+5],
                                             mem[bus_addr[11:0]+6],
                                             mem[bus_addr[11:0]+7]};
                endcase
            end
        end
    end

    // ====================================================================
    // I-cache stub: 1-cycle hit, reads 8 aligned bytes from mem[]
    // ====================================================================
    wire [63:0] icache_addr;
    wire        icache_req;
    reg  [63:0] icache_data;
    reg         icache_hit;
    wire        icache_inv_all, icache_inv_line;
    wire [63:0] icache_inv_addr;

    wire [11:0] ic_base = {icache_addr[11:3], 3'b000};
    always @(negedge clk) begin
        icache_hit  <= 1'b0;
        icache_data <= 64'd0;
        if (icache_req) begin
            icache_hit <= 1'b1;
            icache_data <= {mem[ic_base+7], mem[ic_base+6],
                            mem[ic_base+5], mem[ic_base+4],
                            mem[ic_base+3], mem[ic_base+2],
                            mem[ic_base+1], mem[ic_base+0]};
        end
    end

    // ====================================================================
    // CPU instance
    // ====================================================================
    wire        csr_wen_w;
    wire [7:0]  csr_addr_w;
    wire [63:0] csr_wdata_w;
    wire        mex_valid_w;
    wire [1:0]  mex_ss_w, mex_op_w;
    wire [2:0]  mex_funct_w;
    wire [63:0] mex_gpr_val_w;
    wire [7:0]  mex_imm8_w;
    wire [3:0]  mex_ext_mod_w;
    wire        mex_ext_active_w;

    mp64_cpu uut (
        .clk       (clk),
        .rst       (rst),
        .core_id   (8'd0),
        .icache_addr    (icache_addr),
        .icache_req     (icache_req),
        .icache_data    (icache_data),
        .icache_hit     (icache_hit),
        .icache_stall   (1'b0),
        .icache_inv_all (icache_inv_all),
        .icache_inv_line(icache_inv_line),
        .icache_inv_addr(icache_inv_addr),
        .bus_valid (bus_valid),
        .bus_addr  (bus_addr),
        .bus_wdata (bus_wdata),
        .bus_wen   (bus_wen),
        .bus_size  (bus_size),
        .bus_rdata (bus_rdata),
        .bus_ready (bus_ready),
        .csr_wen   (csr_wen_w),
        .csr_addr  (csr_addr_w),
        .csr_wdata (csr_wdata_w),
        .csr_rdata (64'd0),
        .mex_valid (mex_valid_w),
        .mex_ss    (mex_ss_w),
        .mex_op    (mex_op_w),
        .mex_funct (mex_funct_w),
        .mex_gpr_val(mex_gpr_val_w),
        .mex_imm8  (mex_imm8_w),
        .mex_ext_mod(mex_ext_mod_w),
        .mex_ext_active(mex_ext_active_w),
        .mex_done  (1'b0),
        .mex_busy  (1'b0),
        .irq_timer (1'b0),
        .irq_uart  (1'b0),
        .irq_nic   (1'b0),
        .irq_ipi   (1'b0),
        .icache_stat_hits  (64'd0),
        .icache_stat_misses(64'd0),
        .mem_size_bytes(64'h0000_1000),
        .ef_flags  (4'b0000)
    );

    // ====================================================================
    // Helpers
    // ====================================================================
    integer pass_count, fail_count;

    task check_reg;
        input [255:0] label;
        input [4:0]   rn;
        input [63:0]  expected;
        begin
            if (uut.R[rn] !== expected) begin
                $display("FAIL [%0s]: R%0d = %h, expected %h", label, rn, uut.R[rn], expected);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task check_flags_z;
        input [255:0] label;
        input         expected_z;
        begin
            if (uut.flags[0] !== expected_z) begin
                $display("FAIL [%0s]: Z=%b, expected=%b", label, uut.flags[0], expected_z);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task check_flags_g;
        input [255:0] label;
        input         expected_g;
        begin
            if (uut.flags[5] !== expected_g) begin
                $display("FAIL [%0s]: G=%b, expected=%b", label, uut.flags[5], expected_g);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task check_mem_byte;
        input [255:0] label;
        input [11:0]  addr;
        input [7:0]   expected;
        begin
            if (mem[addr] !== expected) begin
                $display("FAIL [%0s]: mem[%03h]=%02h, expected=%02h",
                         label, addr, mem[addr], expected);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task run_to_halt;
        integer cyc;
        begin
            for (cyc = 0; cyc < 10000; cyc = cyc + 1) begin
                @(posedge clk);
                if (uut.cpu_state == CPU_HALT) cyc = 10000;
            end
        end
    endtask

    integer i;

    // ====================================================================
    // Tests
    // ====================================================================
    initial begin
        $dumpfile("tb_string.vcd");
        $dumpvars(0, tb_string);

        pass_count = 0;
        fail_count = 0;

        // =================================================================
        // Test 1: CMOVE 4 bytes — forward copy
        //   Source  @ 0x80: AA BB CC DD
        //   Dest    @ 0xC0: (empty)
        //   R5 = 0x80 (src), R4 = 0xC0 (dst), R0 = 4 (len)
        //   F9 00 45 → CMOVE Rd=R4, Rs=R5
        // =================================================================
        $display("--- Test 1: CMOVE 4 bytes ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        // Source data
        mem[12'h080] = 8'hAA;
        mem[12'h081] = 8'hBB;
        mem[12'h082] = 8'hCC;
        mem[12'h083] = 8'hDD;

        // Program at 0x0000 (PC = R3 starts at 0)
        //  00: LDI R5, 0x80   → 60 50 80
        //  03: LDI R4, 0xC0   → 60 40 C0
        //  06: LDI R0, 0x04   → 60 00 04
        //  09: CMOVE R4, R5   → F9 00 45
        //  0C: HALT            → 02
        mem[12'h000] = 8'h60; mem[12'h001] = 8'h50; mem[12'h002] = 8'h80;
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h40; mem[12'h005] = 8'hC0;
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h00; mem[12'h008] = 8'h04;
        mem[12'h009] = 8'hF9; mem[12'h00A] = 8'h00; mem[12'h00B] = 8'h45;
        mem[12'h00C] = 8'h02;

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;

        check_mem_byte("CMOVE: dst[0]", 12'h0C0, 8'hAA);
        check_mem_byte("CMOVE: dst[1]", 12'h0C1, 8'hBB);
        check_mem_byte("CMOVE: dst[2]", 12'h0C2, 8'hCC);
        check_mem_byte("CMOVE: dst[3]", 12'h0C3, 8'hDD);
        check_reg("CMOVE: R4 (dst)", 5'd4, 64'h0C4);
        check_reg("CMOVE: R5 (src)", 5'd5, 64'h084);
        check_reg("CMOVE: R0 (len)", 5'd0, 64'h000);

        // =================================================================
        // Test 2: BCOMP equal — 4-byte compare, all match
        //   Region A @ 0x80: 11 22 33 44
        //   Region B @ 0xC0: 11 22 33 44
        //   R5=0x80 (src), R4=0xC0 (dst), R0=4
        //   F9 03 45 → BCOMP Rd=R4, Rs=R5
        //   Expected: Z=1, G=0
        // =================================================================
        $display("--- Test 2: BCOMP equal ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        // Region A (src)
        mem[12'h080] = 8'h11; mem[12'h081] = 8'h22;
        mem[12'h082] = 8'h33; mem[12'h083] = 8'h44;
        // Region B (dst — compared against)
        mem[12'h0C0] = 8'h11; mem[12'h0C1] = 8'h22;
        mem[12'h0C2] = 8'h33; mem[12'h0C3] = 8'h44;

        mem[12'h000] = 8'h60; mem[12'h001] = 8'h50; mem[12'h002] = 8'h80;  // LDI R5, 0x80
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h40; mem[12'h005] = 8'hC0;  // LDI R4, 0xC0
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h00; mem[12'h008] = 8'h04;  // LDI R0, 4
        mem[12'h009] = 8'hF9; mem[12'h00A] = 8'h03; mem[12'h00B] = 8'h45;  // BCOMP R4, R5
        mem[12'h00C] = 8'h02;                                                // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;

        check_flags_z("BCOMP=: Z", 1'b1);
        check_flags_g("BCOMP=: G", 1'b0);
        check_reg("BCOMP=: R0", 5'd0, 64'h000);

        // =================================================================
        // Test 3: BCOMP unequal — mismatch at index 2
        //   Region A @ 0x80: 11 22 33 44
        //   Region B @ 0xC0: 11 22 55 44   (differ at byte 2: 55 > 33)
        //   Expected: Z=0, G=1 (dst byte > src byte)
        //   R5=0x82, R4=0xC2, R0=2 (remaining)
        // =================================================================
        $display("--- Test 3: BCOMP unequal ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h080] = 8'h11; mem[12'h081] = 8'h22;
        mem[12'h082] = 8'h33; mem[12'h083] = 8'h44;
        mem[12'h0C0] = 8'h11; mem[12'h0C1] = 8'h22;
        mem[12'h0C2] = 8'h55; mem[12'h0C3] = 8'h44;

        mem[12'h000] = 8'h60; mem[12'h001] = 8'h50; mem[12'h002] = 8'h80;
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h40; mem[12'h005] = 8'hC0;
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h00; mem[12'h008] = 8'h04;
        mem[12'h009] = 8'hF9; mem[12'h00A] = 8'h03; mem[12'h00B] = 8'h45;
        mem[12'h00C] = 8'h02;

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;

        check_flags_z("BCOMP!=: Z", 1'b0);
        check_flags_g("BCOMP!=: G", 1'b1);
        check_reg("BCOMP!=: R5 (src ptr)", 5'd5, 64'h082);
        check_reg("BCOMP!=: R4 (dst ptr)", 5'd4, 64'h0C2);
        check_reg("BCOMP!=: R0 (remain)",  5'd0, 64'h002);

        // =================================================================
        // Test 4: Zero-length CMOVE — immediate done, registers unchanged
        //   R0 = 0 after reset (no explicit load needed)
        //   R5 = 0x80, R4 = 0xC0
        // =================================================================
        $display("--- Test 4: Zero-length CMOVE ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h000] = 8'h60; mem[12'h001] = 8'h50; mem[12'h002] = 8'h80;  // LDI R5, 0x80
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h40; mem[12'h005] = 8'hC0;  // LDI R4, 0xC0
        // R0 stays at 0 from reset
        mem[12'h006] = 8'hF9; mem[12'h007] = 8'h00; mem[12'h008] = 8'h45;  // CMOVE R4, R5
        mem[12'h009] = 8'h02;                                                // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;

        check_reg("CMOVE0: R4", 5'd4, 64'h0C0);  // unchanged
        check_reg("CMOVE0: R5", 5'd5, 64'h080);  // unchanged
        check_reg("CMOVE0: R0", 5'd0, 64'h000);  // still 0

        // =================================================================
        // Test 5: BFILL 4 bytes — fill with D=0x42
        //   Use GLO to set D from R1.
        //   R4 = 0xC0 (dst), R5 = 4 (count, since BFILL uses Rs as len)
        //   F9 02 45 → BFILL Rd=R4, Rs=R5
        // =================================================================
        $display("--- Test 5: BFILL 4 bytes ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        //  00: LDI R1, 0x42    → 60 10 42
        //  03: GLO R1           → 6C 10       (D = R1[7:0] = 0x42)
        //  05: LDI R4, 0xC0    → 60 40 C0
        //  08: LDI R5, 0x04    → 60 50 04
        //  0B: BFILL R4, R5    → F9 02 45
        //  0E: HALT             → 02
        mem[12'h000] = 8'h60; mem[12'h001] = 8'h10; mem[12'h002] = 8'h42;
        mem[12'h003] = 8'h6C; mem[12'h004] = 8'h10;
        mem[12'h005] = 8'h60; mem[12'h006] = 8'h40; mem[12'h007] = 8'hC0;
        mem[12'h008] = 8'h60; mem[12'h009] = 8'h50; mem[12'h00A] = 8'h04;
        mem[12'h00B] = 8'hF9; mem[12'h00C] = 8'h02; mem[12'h00D] = 8'h45;
        mem[12'h00E] = 8'h02;

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;

        check_mem_byte("BFILL: dst[0]", 12'h0C0, 8'h42);
        check_mem_byte("BFILL: dst[1]", 12'h0C1, 8'h42);
        check_mem_byte("BFILL: dst[2]", 12'h0C2, 8'h42);
        check_mem_byte("BFILL: dst[3]", 12'h0C3, 8'h42);
        check_reg("BFILL: R4 (dst)", 5'd4, 64'h0C4);
        check_reg("BFILL: R5 (cnt)", 5'd5, 64'h000);  // consumed

        // =================================================================
        // Test 6: BSRCH found — needle 0x42 at index 2
        //   Haystack @ 0x80: 11 22 42 44
        //   D = 0x42 (needle), R4 = 0x80 (haystack=Rd), R0 = 4
        //   F9 04 45 → BSRCH Rd=R4, Rs=R5
        //   Expected: Z=1, R5=2 (offset), R4=0x82, R0=2
        // =================================================================
        $display("--- Test 6: BSRCH found ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h080] = 8'h11; mem[12'h081] = 8'h22;
        mem[12'h082] = 8'h42; mem[12'h083] = 8'h44;

        //  00: LDI R1, 0x42    → 60 10 42
        //  03: GLO R1           → 6C 10       (D = 0x42)
        //  05: LDI R4, 0x80    → 60 40 80
        //  08: LDI R0, 0x04    → 60 00 04
        //  0B: BSRCH R4, R5    → F9 04 45
        //  0E: HALT             → 02
        mem[12'h000] = 8'h60; mem[12'h001] = 8'h10; mem[12'h002] = 8'h42;
        mem[12'h003] = 8'h6C; mem[12'h004] = 8'h10;
        mem[12'h005] = 8'h60; mem[12'h006] = 8'h40; mem[12'h007] = 8'h80;
        mem[12'h008] = 8'h60; mem[12'h009] = 8'h00; mem[12'h00A] = 8'h04;
        mem[12'h00B] = 8'hF9; mem[12'h00C] = 8'h04; mem[12'h00D] = 8'h45;
        mem[12'h00E] = 8'h02;

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;

        check_flags_z("BSRCH+: Z", 1'b1);
        check_reg("BSRCH+: R5 (offset)", 5'd5, 64'h002);
        check_reg("BSRCH+: R4 (scan)",   5'd4, 64'h082);
        check_reg("BSRCH+: R0 (remain)", 5'd0, 64'h002);

        // =================================================================
        // Test 7: BSRCH not found
        //   Haystack @ 0x80: 11 22 33 44 (no 0xFF)
        //   D = 0xFF (needle), R4 = 0x80, R0 = 4
        //   Expected: Z=0, R5=4 (orig_len), R4=0x84, R0=0
        // =================================================================
        $display("--- Test 7: BSRCH not found ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h080] = 8'h11; mem[12'h081] = 8'h22;
        mem[12'h082] = 8'h33; mem[12'h083] = 8'h44;

        //  00: LDI R1, 0xFF    → 60 10 FF
        //  03: GLO R1           → 6C 10       (D = 0xFF)
        //  05: LDI R4, 0x80    → 60 40 80
        //  08: LDI R0, 0x04    → 60 00 04
        //  0B: BSRCH R4, R5    → F9 04 45
        //  0E: HALT             → 02
        mem[12'h000] = 8'h60; mem[12'h001] = 8'h10; mem[12'h002] = 8'hFF;
        mem[12'h003] = 8'h6C; mem[12'h004] = 8'h10;
        mem[12'h005] = 8'h60; mem[12'h006] = 8'h40; mem[12'h007] = 8'h80;
        mem[12'h008] = 8'h60; mem[12'h009] = 8'h00; mem[12'h00A] = 8'h04;
        mem[12'h00B] = 8'hF9; mem[12'h00C] = 8'h04; mem[12'h00D] = 8'h45;
        mem[12'h00E] = 8'h02;

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;

        check_flags_z("BSRCH-: Z", 1'b0);
        check_reg("BSRCH-: R5 (orig_len)", 5'd5, 64'h004);
        check_reg("BSRCH-: R4 (past end)", 5'd4, 64'h084);
        check_reg("BSRCH-: R0 (remain)",   5'd0, 64'h000);

        // =================================================================
        // Test 8: CMOVE> 4 bytes — backward copy
        //   Source  @ 0x80: AA BB CC DD
        //   Dest    @ 0xC0: (empty)
        //   R5=0x80, R4=0xC0, R0=4
        //   F9 01 45 → CMOVE> Rd=R4, Rs=R5
        //   Expected: mem[0xC0..C3]=AA BB CC DD, R4=0xC4, R5=0x84, R0=0
        // =================================================================
        $display("--- Test 8: CMOVE> 4 bytes ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h080] = 8'hAA; mem[12'h081] = 8'hBB;
        mem[12'h082] = 8'hCC; mem[12'h083] = 8'hDD;

        mem[12'h000] = 8'h60; mem[12'h001] = 8'h50; mem[12'h002] = 8'h80;  // LDI R5, 0x80
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h40; mem[12'h005] = 8'hC0;  // LDI R4, 0xC0
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h00; mem[12'h008] = 8'h04;  // LDI R0, 4
        mem[12'h009] = 8'hF9; mem[12'h00A] = 8'h01; mem[12'h00B] = 8'h45;  // CMOVE> R4, R5
        mem[12'h00C] = 8'h02;                                                // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;

        check_mem_byte("CMOVE>: dst[0]", 12'h0C0, 8'hAA);
        check_mem_byte("CMOVE>: dst[1]", 12'h0C1, 8'hBB);
        check_mem_byte("CMOVE>: dst[2]", 12'h0C2, 8'hCC);
        check_mem_byte("CMOVE>: dst[3]", 12'h0C3, 8'hDD);
        check_reg("CMOVE>: R4 (dst)", 5'd4, 64'h0C4);
        check_reg("CMOVE>: R5 (src)", 5'd5, 64'h084);
        check_reg("CMOVE>: R0 (len)", 5'd0, 64'h000);

        // =================================================================
        $display("===========================================");
        if (fail_count == 0)
            $display("tb_string: ALL %0d TESTS PASSED", pass_count);
        else
            $display("tb_string: %0d PASSED, %0d FAILED", pass_count, fail_count);
        $display("===========================================");
        if (fail_count != 0) $stop;
        $finish;
    end

    // Timeout watchdog
    initial begin
        #2000000;
        $display("TIMEOUT: tb_string");
        $finish;
    end

endmodule
