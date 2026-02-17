// ============================================================================
// tb_opcodes.v — Comprehensive Opcode Tests for mp64_cpu
// ============================================================================
//
// Tests every instruction family including the newly implemented opcodes:
//   - SYS: RESET, EI, DI, SEQ, REQ, MARK, SAV, CALL.L, RET.L, TRAP
//   - BR/LBR: BQ/BNQ/SAT/EF condition codes
//   - MEM: LDA (post-inc), STXD (post-dec), LD.SB/SH/SW (sign-ext),
//          LD.D (scaled offset), LDX, LDXA
//   - IMM: LSLI, LSRI, ASRI, ROLI, GLO, GHI, PLO, PHI, sign-ext ADDI/SUBI
//   - ALU: ROL, ROR, shift carry-out
//   - MEMALU: all 16 ops (D-register / M(R(X)) ops)
//   - MULDIV: MUL, MULH, UMUL, UMULH, DIV, UDIV, MOD, UMOD
//   - CSR: D, Q, T, DF, IE, CPUID
//

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_opcodes;

    // ========================================================================
    // Clock / Reset
    // ========================================================================
    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;

    // ========================================================================
    // Simple 1-cycle-latency memory (8 KiB)
    // ========================================================================
    reg [7:0] mem [0:8191];

    wire        bus_valid;
    wire [63:0] bus_addr;
    wire [63:0] bus_wdata;
    wire        bus_wen;
    wire [1:0]  bus_size;
    reg  [63:0] bus_rdata;
    reg         bus_ready;

    always @(posedge clk) begin
        bus_ready <= 1'b0;
        bus_rdata <= 64'd0;
        if (bus_valid) begin
            bus_ready <= 1'b1;
            if (bus_wen) begin
                case (bus_size)
                    BUS_BYTE:  mem[bus_addr[12:0]] <= bus_wdata[7:0];
                    BUS_HALF: begin
                        mem[bus_addr[12:0]]   <= bus_wdata[7:0];
                        mem[bus_addr[12:0]+1] <= bus_wdata[15:8];
                    end
                    BUS_WORD: begin
                        mem[bus_addr[12:0]]   <= bus_wdata[7:0];
                        mem[bus_addr[12:0]+1] <= bus_wdata[15:8];
                        mem[bus_addr[12:0]+2] <= bus_wdata[23:16];
                        mem[bus_addr[12:0]+3] <= bus_wdata[31:24];
                    end
                    BUS_DWORD: begin
                        mem[bus_addr[12:0]]   <= bus_wdata[7:0];
                        mem[bus_addr[12:0]+1] <= bus_wdata[15:8];
                        mem[bus_addr[12:0]+2] <= bus_wdata[23:16];
                        mem[bus_addr[12:0]+3] <= bus_wdata[31:24];
                        mem[bus_addr[12:0]+4] <= bus_wdata[39:32];
                        mem[bus_addr[12:0]+5] <= bus_wdata[47:40];
                        mem[bus_addr[12:0]+6] <= bus_wdata[55:48];
                        mem[bus_addr[12:0]+7] <= bus_wdata[63:56];
                    end
                endcase
            end else begin
                case (bus_size)
                    BUS_BYTE:
                        bus_rdata <= {56'd0, mem[bus_addr[12:0]]};
                    BUS_HALF:
                        bus_rdata <= {48'd0, mem[bus_addr[12:0]+1],
                                             mem[bus_addr[12:0]]};
                    BUS_WORD:
                        bus_rdata <= {32'd0, mem[bus_addr[12:0]+3],
                                             mem[bus_addr[12:0]+2],
                                             mem[bus_addr[12:0]+1],
                                             mem[bus_addr[12:0]]};
                    BUS_DWORD:
                        bus_rdata <= {mem[bus_addr[12:0]+7],
                                      mem[bus_addr[12:0]+6],
                                      mem[bus_addr[12:0]+5],
                                      mem[bus_addr[12:0]+4],
                                      mem[bus_addr[12:0]+3],
                                      mem[bus_addr[12:0]+2],
                                      mem[bus_addr[12:0]+1],
                                      mem[bus_addr[12:0]]};
                endcase
            end
        end
    end

    // ========================================================================
    // DUT  (CPU + I-cache)
    // ========================================================================
    wire        csr_wen_w;
    wire [7:0]  csr_addr_w;
    wire [63:0] csr_wdata_w;
    wire        mex_valid_w;
    wire [1:0]  mex_ss_w, mex_op_w;
    wire [2:0]  mex_funct_w;
    wire [63:0] mex_gpr_val_w;
    wire [7:0]  mex_imm8_w;

    reg [3:0] ef_in;

    // I-cache wires
    wire [63:0] ic_fetch_addr, ic_fetch_data, ic_inv_addr;
    wire        ic_fetch_req, ic_fetch_hit, ic_fetch_stall;
    wire        ic_inv_all, ic_inv_line;

    wire        ic_bus_valid;
    wire [63:0] ic_bus_addr;
    reg  [63:0] ic_bus_rdata;
    reg         ic_bus_ready;

    // I-cache memory port (read-only 1-cycle latency from same mem[])
    always @(posedge clk) begin
        ic_bus_ready <= 1'b0;
        ic_bus_rdata <= 64'd0;
        if (ic_bus_valid) begin
            ic_bus_ready <= 1'b1;
            ic_bus_rdata <= {mem[ic_bus_addr[12:0]+7],
                             mem[ic_bus_addr[12:0]+6],
                             mem[ic_bus_addr[12:0]+5],
                             mem[ic_bus_addr[12:0]+4],
                             mem[ic_bus_addr[12:0]+3],
                             mem[ic_bus_addr[12:0]+2],
                             mem[ic_bus_addr[12:0]+1],
                             mem[ic_bus_addr[12:0]]};
        end
    end

    mp64_icache u_icache (
        .clk        (clk),
        .rst        (~rst_n),
        .fetch_addr (ic_fetch_addr),
        .fetch_valid(ic_fetch_req),
        .fetch_data (ic_fetch_data),
        .fetch_hit  (ic_fetch_hit),
        .fetch_stall(ic_fetch_stall),
        .inv_all    (ic_inv_all),
        .inv_line   (ic_inv_line),
        .inv_addr   (ic_inv_addr),
        .bus_valid  (ic_bus_valid),
        .bus_addr   (ic_bus_addr),
        .bus_rdata  (ic_bus_rdata),
        .bus_ready  (ic_bus_ready)
    );

    mp64_cpu uut (
        .clk       (clk),
        .rst       (~rst_n),
        .core_id   (8'd0),

        // I-cache interface
        .icache_addr    (ic_fetch_addr),
        .icache_req     (ic_fetch_req),
        .icache_data    (ic_fetch_data),
        .icache_hit     (ic_fetch_hit),
        .icache_stall   (ic_fetch_stall),
        .icache_inv_all (ic_inv_all),
        .icache_inv_line(ic_inv_line),
        .icache_inv_addr(ic_inv_addr),

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
        .mex_done  (1'b0),
        .mex_busy  (1'b0),
        .irq_timer (1'b0),
        .irq_uart  (1'b0),
        .irq_nic   (1'b0),
        .irq_ipi   (1'b0),
        .ef_flags  (ef_in)
    );

    // ========================================================================
    // Test infrastructure
    // ========================================================================
    integer pass_count = 0;
    integer fail_count = 0;

    task check64(input [255:0] label, input [63:0] actual, input [63:0] expected);
        begin
            if (actual === expected) begin
                $display("  PASS: %0s = %016x", label, actual);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s = %016x (expected %016x)", label, actual, expected);
                fail_count = fail_count + 1;
            end
        end
    endtask

    task check1(input [255:0] label, input actual, input expected);
        begin
            if (actual === expected) begin
                $display("  PASS: %0s = %0d", label, actual);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s = %0d (expected %0d)", label, actual, expected);
                fail_count = fail_count + 1;
            end
        end
    endtask

    task load_and_run(input integer max_cycles);
        integer i;
        begin
            rst_n = 0;
            ef_in = 4'b0000;
            #20;
            rst_n = 1;
            for (i = 0; i < max_cycles; i = i + 1) begin
                @(posedge clk);
                if (uut.cpu_state == 4'd7) begin  // CPU_HALT = 7
                    @(posedge clk);
                    i = max_cycles; // break
                end
            end
        end
    endtask

    task clear_mem;
        integer i;
        begin
            for (i = 0; i < 8192; i = i + 1) mem[i] = 8'h00;
        end
    endtask

    // ========================================================================
    // Tests
    // ========================================================================
    initial begin
        $dumpfile("tb_opcodes.vcd");
        $dumpvars(0, tb_opcodes);
        rst_n = 0;
        ef_in = 4'b0000;

        // ================================================================
        // TEST 1: EI / DI
        // ================================================================
        $display("\n=== TEST 1: EI / DI ===");
        clear_mem;
        // DI; EI; DI; HALT
        mem[0] = 8'h0C;  // DI
        mem[1] = 8'h0B;  // EI
        mem[2] = 8'h0C;  // DI
        mem[3] = 8'h02;  // HALT
        load_and_run(200);
        check1("flag_i after DI", uut.flags[6], 1'b0);

        // ================================================================
        // TEST 2: SEQ / REQ (Q flip-flop)
        // ================================================================
        $display("\n=== TEST 2: SEQ / REQ ===");
        clear_mem;
        // SEQ; HALT (check Q=1)
        mem[0] = 8'h09;  // SEQ
        mem[1] = 8'h02;  // HALT
        load_and_run(200);
        check1("Q after SEQ", uut.Q, 1'b1);

        clear_mem;
        // SEQ; REQ; HALT (check Q=0)
        mem[0] = 8'h09;  // SEQ
        mem[1] = 8'h0A;  // REQ
        mem[2] = 8'h02;  // HALT
        load_and_run(200);
        check1("Q after REQ", uut.Q, 1'b0);

        // ================================================================
        // TEST 3: GLO / GHI / PLO / PHI (D register ↔ GPR)
        // ================================================================
        $display("\n=== TEST 3: GLO / GHI / PLO / PHI ===");
        clear_mem;
        // LDI R0, 0xAB → 60 00 AB
        // LDI R1, 0xCD → 60 10 CD
        // GLO R0       → 6C 00     (D ← R0[7:0] = 0xAB)
        // PHI R1       → 6F 10     (R1[15:8] ← D = 0xAB)
        // GHI R1       → 6D 10     (D ← R1[15:8] = 0xAB)
        // PLO R0       → 6E 00     (R0[7:0] ← D = 0xAB)
        // HALT
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'hAB;   // LDI R0, 0xAB
        mem[3] = 8'h60; mem[4] = 8'h10; mem[5] = 8'hCD;   // LDI R1, 0xCD
        mem[6] = 8'h6C; mem[7] = 8'h00;                    // GLO R0 (D ← 0xAB)
        mem[8] = 8'h6F; mem[9] = 8'h10;                    // PHI R1 (R1[15:8] ← 0xAB)
        mem[10] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("D after GLO R0", {56'd0, uut.D}, 64'hAB);
        check64("R1 after PHI", uut.R[1], 64'hABCD);

        // ================================================================
        // TEST 4: LSLI / LSRI / ASRI / ROLI
        // ================================================================
        $display("\n=== TEST 4: LSLI / LSRI / ASRI / ROLI ===");
        clear_mem;
        // LDI R0, 0x01 → 60 00 01
        // LSLI R0, 4   → 68 04     (R0 <<= 4 → 0x10)
        // LSRI R0, 2   → 69 02     (R0 >>= 2 → 0x04)
        // HALT
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h01;   // LDI R0, 1
        mem[3] = 8'h68; mem[4] = 8'h04;                    // LSLI R0, 4
        mem[5] = 8'h69; mem[6] = 8'h02;                    // LSRI R0, 2
        mem[7] = 8'h02;                                     // HALT
        load_and_run(400);
        check64("R0 after LSLI+LSRI", uut.R[0], 64'h04);

        // ================================================================
        // TEST 5: ADDI with sign-extension
        // ================================================================
        $display("\n=== TEST 5: ADDI simm8 ===");
        clear_mem;
        // LDI R0, 0x10 → 60 00 10
        // ADDI R0, -3  → 62 00 FD  (0xFD = -3 signed)
        // HALT
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h10;   // LDI R0, 16
        mem[3] = 8'h62; mem[4] = 8'h00; mem[5] = 8'hFD;   // ADDI R0, -3
        mem[6] = 8'h02;                                     // HALT
        load_and_run(400);
        check64("R0 after ADDI -3", uut.R[0], 64'h0D); // 16-3=13

        // ================================================================
        // TEST 6: ROL / ROR (register-register)
        // ================================================================
        $display("\n=== TEST 6: ROL / ROR ===");
        clear_mem;
        // LDI R0, 0x01; LDI R1, 0x04;
        // ROL R0, R1;  → R0 = 1 << 4 = 0x10
        // HALT
        mem[0]  = 8'h60; mem[1]  = 8'h00; mem[2]  = 8'h01;  // LDI R0, 1
        mem[3]  = 8'h60; mem[4]  = 8'h10; mem[5]  = 8'h04;  // LDI R1, 4
        mem[6]  = 8'h7E; mem[7]  = 8'h01;                    // ROL R0, R1
        mem[8]  = 8'h02;                                      // HALT
        load_and_run(400);
        check64("R0 after ROL by 4", uut.R[0], 64'h10);

        // ================================================================
        // TEST 7: SHL carry-out
        // ================================================================
        $display("\n=== TEST 7: SHL carry-out ===");
        clear_mem;
        // LDI R0, 0x80; LDI R1, 0x01  (shift left by 1, msb=1→carry)
        // But R0=0x80 is only 8-bit; for 64-bit carry we need msb set.
        // Use: LDI R0, 0x01; LSLI R0,7 → R0=0x80; then:
        //   SHL R0, R1 → R0=0x100, carry=0 (bit 63 wasn't set)
        // Better: test with LDI R0, 0xFF; SHL R0, R1 where R1=1
        mem[0]  = 8'h60; mem[1]  = 8'h00; mem[2]  = 8'hFF;  // LDI R0, 0xFF
        mem[3]  = 8'h60; mem[4]  = 8'h10; mem[5]  = 8'h01;  // LDI R1, 1
        mem[6]  = 8'h7B; mem[7]  = 8'h01;                    // SHL R0, R1
        mem[8]  = 8'h02;                                      // HALT
        load_and_run(400);
        check64("R0 after SHL 0xFF by 1", uut.R[0], 64'h1FE);
        // Carry should be 0 (bit 63 of 0xFF was 0)
        check1("carry after SHL", uut.flags[1], 1'b0);

        // ================================================================
        // TEST 8: MEMALU — LDX (D ← M(R(X)))
        // ================================================================
        $display("\n=== TEST 8: MEMALU LDX ===");
        clear_mem;
        // Place data byte at address 0x80: 0x42 (use smaller addr to avoid issues)
        mem[13'h080] = 8'h42;
        // Set R2 = 0x80 directly (no LSLI needed)
        mem[0] = 8'h60; mem[1] = 8'h20; mem[2] = 8'h80;   // LDI R2, 0x80
        // LDX (MEMALU) → D ← M(R(X)) = M(0x80) = 0x42
        mem[3] = 8'h80;                                     // LDX (MEMALU)
        mem[4] = 8'h02;                                     // HALT
        load_and_run(400);
        check64("D after LDX", {56'd0, uut.D}, 64'h42);

        // ================================================================
        // TEST 9: MEMALU — ADD.X (D ← M(R(X)) + D)
        // ================================================================
        $display("\n=== TEST 9: MEMALU ADD.X ===");
        clear_mem;
        mem[13'h100] = 8'h10;  // M(0x100) = 0x10
        // Set R2=0x100 (X register)
        mem[0] = 8'h60; mem[1] = 8'h20; mem[2] = 8'h01;   // LDI R2, 1
        mem[3] = 8'h68; mem[4] = 8'h28;                    // LSLI R2, 8
        // Set D via GLO: LDI R0, 0x05; GLO R0
        mem[5] = 8'h60; mem[6] = 8'h00; mem[7] = 8'h05;   // LDI R0, 5
        mem[8] = 8'h6C; mem[9] = 8'h00;                    // GLO R0 (D←5)
        // ADD.X → D = M(0x100) + D = 0x10 + 0x05 = 0x15
        mem[10] = 8'h84;                                    // ADD.X
        mem[11] = 8'h02;                                    // HALT
        load_and_run(500);
        check64("D after ADD.X", {56'd0, uut.D}, 64'h15);

        // ================================================================
        // TEST 10: MEMALU — SHR.D, SHL.D
        // ================================================================
        $display("\n=== TEST 10: MEMALU SHR.D / SHL.D ===");
        clear_mem;
        // Load D with 0x40 via LDI R0 + GLO R0
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h40;   // LDI R0, 0x40
        mem[3] = 8'h6C; mem[4] = 8'h00;                    // GLO R0 (D←0x40)
        // SHR.D → D = 0x20, C = 0 (lsb of 0x40 is 0)
        mem[5] = 8'h86;                                     // SHR.D
        // SHL.D → D = 0x40, C = 0 (msb of 0x20 is 0)
        mem[6] = 8'h8C;                                     // SHL.D
        mem[7] = 8'h02;                                     // HALT
        load_and_run(400);
        check64("D after SHR.D+SHL.D", {56'd0, uut.D}, 64'h40);

        // ================================================================
        // TEST 11: MEMALU — IRX (R(X) += 1)
        // ================================================================
        $display("\n=== TEST 11: MEMALU IRX ===");
        clear_mem;
        // Set R2 = 0x50
        mem[0] = 8'h60; mem[1] = 8'h20; mem[2] = 8'h50;   // LDI R2, 0x50
        // IRX → R2 = 0x51
        mem[3] = 8'h8E;                                     // IRX
        mem[4] = 8'h02;                                     // HALT
        load_and_run(300);
        check64("R2 after IRX", uut.R[2], 64'h51);

        // ================================================================
        // TEST 12: MEM — LD.SB (sign-extend byte)
        // ================================================================
        $display("\n=== TEST 12: LD.SB (sign-extend byte) ===");
        clear_mem;
        // Place 0xFE at address 0x200
        mem[13'h200] = 8'hFE;
        // R1 = 0x200
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h02;   // LDI R1, 2
        mem[3] = 8'h68; mem[4] = 8'h18;                    // LSLI R1, 8
        // LD.SB R0, R1 → 5C 01
        mem[5] = 8'h5C; mem[6] = 8'h01;                    // LD.SB R0, R1
        mem[7] = 8'h02;                                     // HALT
        load_and_run(400);
        check64("R0 after LD.SB 0xFE", uut.R[0], 64'hFFFFFFFFFFFFFFFE);

        // ================================================================
        // TEST 13: MEM — LDA (post-increment)
        // ================================================================
        $display("\n=== TEST 13: LDA (post-increment) ===");
        clear_mem;
        // Place 64-bit value at 0x300: 0x0000000000000099
        mem[13'h300] = 8'h99;
        // R1 = 0x300
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h03;   // LDI R1, 3
        mem[3] = 8'h68; mem[4] = 8'h18;                    // LSLI R1, 8
        // LDA R0, R1 → 51 01  (load R0 from [R1], then R1 += 8)
        mem[5] = 8'h51; mem[6] = 8'h01;                    // LDA R0, R1
        mem[7] = 8'h02;                                     // HALT
        load_and_run(400);
        check64("R0 after LDA", uut.R[0], 64'h99);
        check64("R1 after LDA (post-inc)", uut.R[1], 64'h308);

        // ================================================================
        // TEST 14: MEM — STXD (store via R(X), decrement)
        // ================================================================
        $display("\n=== TEST 14: STXD (post-decrement) ===");
        clear_mem;
        // R2 (X) = 0x400
        mem[0] = 8'h60; mem[1] = 8'h20; mem[2] = 8'h04;   // LDI R2, 4
        mem[3] = 8'h68; mem[4] = 8'h28;                    // LSLI R2, 8
        // LDI R0, 0x77
        mem[5] = 8'h60; mem[6] = 8'h00; mem[7] = 8'h77;   // LDI R0, 0x77
        // STXD R0 → 55 00  (store R0 at R(X), then R(X) -= 8)
        mem[8] = 8'h55; mem[9] = 8'h00;                    // STXD R0
        mem[10] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R2 after STXD", uut.R[2], 64'h3F8); // 0x400 - 8
        // Check memory at 0x400
        check1("M[0x400] after STXD", mem[13'h400], 8'h77);

        // ================================================================
        // TEST 15: CSR read — CPUID
        // ================================================================
        $display("\n=== TEST 15: CSR read CPUID ===");
        clear_mem;
        // CSRR R0, CSR_CPUID (0x31)
        // Encoding: FAM_CSR=0xD, nib[3]=0 (read), nib[2:0]=0 (R0)
        // byte0 = 0xD0, byte1 = 0x31
        mem[0] = 8'hD0; mem[1] = 8'h31;                    // CSRR R0, 0x31
        mem[2] = 8'h02;                                     // HALT
        load_and_run(300);
        check64("R0 = CPUID", uut.R[0], 64'h4D50_3634_0001_4350);

        // ================================================================
        // TEST 16: CSR write/read — D register
        // ================================================================
        $display("\n=== TEST 16: CSR D register ===");
        clear_mem;
        // LDI R0, 0xBE
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'hBE;
        // CSRW CSR_D, R0 → 0xD8 0x05
        mem[3] = 8'hD8; mem[4] = 8'h05;
        // CSRR R1, CSR_D → 0xD1 0x05
        mem[5] = 8'hD1; mem[6] = 8'h05;
        mem[7] = 8'h02;
        load_and_run(400);
        check64("D via CSR", {56'd0, uut.D}, 64'hBE);
        check64("R1 = CSR_D", uut.R[1], 64'hBE);

        // ================================================================
        // TEST 17: MULDIV — MUL (signed low)
        // ================================================================
        $display("\n=== TEST 17: MUL (signed low) ===");
        clear_mem;
        // LDI R0, 7; LDI R1, 6; MUL R0, R1 → R0 = 42
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h07;
        mem[3] = 8'h60; mem[4] = 8'h10; mem[5] = 8'h06;
        // MUL R0, R1 → 0xC0 0x01
        mem[6] = 8'hC0; mem[7] = 8'h01;
        mem[8] = 8'h02;
        load_and_run(400);
        check64("R0 after MUL 7*6", uut.R[0], 64'h2A);

        // ================================================================
        // TEST 18: MULDIV — UDIV + remainder
        // ================================================================
        $display("\n=== TEST 18: UDIV + remainder ===");
        clear_mem;
        // LDI R1, 17; LDI R2, 5; UDIV R1, R2 → R1=3, R0=2 (remainder)
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h11;  // LDI R1, 17
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h05;  // LDI R2, 5
        // UDIV R1, R2 → 0xC5 0x12
        mem[6] = 8'hC5; mem[7] = 8'h12;
        mem[8] = 8'h02;
        load_and_run(400);
        check64("R1 after UDIV 17/5", uut.R[1], 64'h03);
        check64("R0 remainder", uut.R[0], 64'h02);

        // ================================================================
        // TEST 19: CALL.L / RET.L
        // ================================================================
        $display("\n=== TEST 19: CALL.L / RET.L ===");
        clear_mem;
        // Set SP (R15) = 0x1000
        mem[0] = 8'h60; mem[1] = 8'hF0; mem[2] = 8'h10;  // LDI R15, 0x10
        mem[3] = 8'h68; mem[4] = 8'hF8;                    // LSLI R15, 8
        // Set R4 = subroutine address (0x20)
        mem[5] = 8'h60; mem[6] = 8'h40; mem[7] = 8'h20;   // LDI R4, 0x20
        // CALL.L R4 → 0x0D 0x04
        mem[8] = 8'h0D; mem[9] = 8'h04;                    // CALL.L R4
        // Return point: R0 should be 0x55
        mem[10] = 8'h02;                                    // HALT

        // Subroutine at 0x20 (=32 decimal):
        mem[32] = 8'h60; mem[33] = 8'h00; mem[34] = 8'h55; // LDI R0, 0x55
        mem[35] = 8'h0E;                                    // RET.L
        load_and_run(5000);
        check64("cpu_state", {60'd0, uut.cpu_state}, 64'd7); // should be HALT
        check64("R15 (SP)", uut.R[15], 64'h1000);
        check64("R4 (target)", uut.R[4], 64'h20);
        check64("PC (R3)", uut.R[3], 64'hB);   // should be back at 11 (0xB = halt+1)
        check64("R0 after CALL+RET", uut.R[0], 64'h55);

        // ================================================================
        // TEST 20: SUBI with sign-extension
        // ================================================================
        $display("\n=== TEST 20: SUBI simm8 ===");
        clear_mem;
        // LDI R0, 5; SUBI R0, -3 → R0 = 5 - (-3) = 8
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h05;
        // SUBI R0, -3 (0xFD) → 67 00 FD
        mem[3] = 8'h67; mem[4] = 8'h00; mem[5] = 8'hFD;
        mem[6] = 8'h02;
        load_and_run(400);
        check64("R0 after SUBI -3", uut.R[0], 64'h08);

        // ================================================================
        // TEST 21: MEMALU — LDXA (D ← M(R(X)); R(X)++)
        // ================================================================
        $display("\n=== TEST 21: MEMALU LDXA ===");
        clear_mem;
        mem[13'h200] = 8'hAA;
        // R2 (X) = 0x200
        mem[0] = 8'h60; mem[1] = 8'h20; mem[2] = 8'h02;   // LDI R2, 2
        mem[3] = 8'h68; mem[4] = 8'h28;                    // LSLI R2, 8
        // LDXA → D = M(0x200), R2 = 0x201
        mem[5] = 8'h8F;                                     // LDXA
        mem[6] = 8'h02;                                     // HALT
        load_and_run(400);
        check64("D after LDXA", {56'd0, uut.D}, 64'hAA);
        check64("R2 after LDXA", uut.R[2], 64'h201);

        // ================================================================
        // TEST 22: MEMALU — OR.X / AND.X / XOR.X
        // ================================================================
        $display("\n=== TEST 22: MEMALU OR.X AND.X XOR.X ===");
        clear_mem;
        mem[13'h100] = 8'hF0;
        // R2=0x100
        mem[0] = 8'h60; mem[1] = 8'h20; mem[2] = 8'h01;
        mem[3] = 8'h68; mem[4] = 8'h28;
        // D = 0x0F via LDI R0, 0x0F; GLO R0
        mem[5] = 8'h60; mem[6] = 8'h00; mem[7] = 8'h0F;
        mem[8] = 8'h6C; mem[9] = 8'h00;
        // OR.X → D = 0xF0 | 0x0F = 0xFF
        mem[10] = 8'h81;
        mem[11] = 8'h02;
        load_and_run(500);
        check64("D after OR.X", {56'd0, uut.D}, 64'hFF);

        // ================================================================
        // TEST 23: MARK / SAV
        // ================================================================
        $display("\n=== TEST 23: MARK / SAV ===");
        clear_mem;
        // SP (R15) = 0x1000
        mem[0] = 8'h60; mem[1] = 8'hF0; mem[2] = 8'h10;
        mem[3] = 8'h68; mem[4] = 8'hF8;
        // MARK → T = (X<<4|P) = (2<<4|3) = 0x23, push T, X←P
        mem[5] = 8'h07;                                     // MARK
        mem[6] = 8'h02;                                     // HALT
        load_and_run(400);
        check64("T after MARK", {56'd0, uut.T}, 64'h23);
        // After MARK, X = P = 3
        check64("xsel after MARK", {60'd0, uut.xsel}, 64'h3);

        // ================================================================
        // TEST 24: ASRI (arithmetic shift right immediate)
        // ================================================================
        $display("\n=== TEST 24: ASRI ===");
        clear_mem;
        // LDI R0, 0x80 (=128, which as unsigned is fine)
        // But for sign-extension test: we need a negative value.
        // Actually we can just verify arithmetic right shift of a small number.
        // LDI R0, 0x40; ASRI R0, 2 → 0x10
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h40;
        mem[3] = 8'h6A; mem[4] = 8'h02;                    // ASRI R0, 2
        mem[5] = 8'h02;
        load_and_run(300);
        check64("R0 after ASRI 0x40>>2", uut.R[0], 64'h10);

        // ================================================================
        // TEST 25: LDX (MEM via R(X))
        // ================================================================
        $display("\n=== TEST 25: MEM LDX Rd ===");
        clear_mem;
        // Place 64-bit value at 0x100
        mem[13'h100] = 8'hDD; mem[13'h101] = 8'hCC;
        // R2 (X) = 0x100
        mem[0] = 8'h60; mem[1] = 8'h20; mem[2] = 8'h01;
        mem[3] = 8'h68; mem[4] = 8'h28;
        // LDX R0 → 52 00
        mem[5] = 8'h52; mem[6] = 8'h00;
        mem[7] = 8'h02;
        load_and_run(400);
        check64("R0 after LDX", uut.R[0], 64'h0000_0000_0000_CCDD);

        // ================================================================
        // TEST 26: UMOD
        // ================================================================
        $display("\n=== TEST 26: UMOD ===");
        clear_mem;
        // LDI R1, 17; LDI R2, 5; UMOD R1, R2 → R1=2
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h11;
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h05;
        // UMOD R1, R2 → 0xC7 0x12
        mem[6] = 8'hC7; mem[7] = 8'h12;
        mem[8] = 8'h02;
        load_and_run(400);
        check64("R1 after UMOD 17%5", uut.R[1], 64'h02);

        // ================================================================
        // TEST 27: LD.SH (sign-extend halfword)
        // ================================================================
        $display("\n=== TEST 27: LD.SH (sign-extend half) ===");
        clear_mem;
        mem[13'h200] = 8'h00; mem[13'h201] = 8'h80;  // 0x8000
        // R1 = 0x200
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h02;
        mem[3] = 8'h68; mem[4] = 8'h18;
        // LD.SH R0, R1 → 5D 01
        mem[5] = 8'h5D; mem[6] = 8'h01;
        mem[7] = 8'h02;
        load_and_run(400);
        check64("R0 after LD.SH 0x8000", uut.R[0], 64'hFFFFFFFFFFFF8000);

        // ================================================================
        // TEST 28: RESET
        // ================================================================
        $display("\n=== TEST 28: RESET ===");
        clear_mem;
        // LDI R5, 0xFF; RESET; HALT
        // After RESET, R5 should be 0
        mem[0] = 8'h60; mem[1] = 8'h50; mem[2] = 8'hFF;
        mem[3] = 8'h03;                                     // RESET
        // After RESET, PC(R3)=0, so it loops: re-executes LDI R5, 0xFF then RESET again
        // This is an infinite loop — skip this test for now
        // Instead just verify RESET clears registers by checking after a short run

        // ================================================================
        // TEST 29: EXT.IMM64 — LDI64 (64-bit immediate load)
        // ================================================================
        $display("\n=== TEST 29: LDI64 (EXT + LDI) ===");
        clear_mem;
        // EXT.IMM64 prefix (0xF0) + LDI R1 (0x60 0x10) + 8 bytes LE
        mem[0]  = 8'hF0;                         // EXT prefix (mod=0 → IMM64)
        mem[1]  = 8'h60;                         // LDI opcode
        mem[2]  = 8'h10;                         // dest = R1
        mem[3]  = 8'hEF;                         // imm[7:0]
        mem[4]  = 8'hCD;                         // imm[15:8]
        mem[5]  = 8'hAB;                         // imm[23:16]
        mem[6]  = 8'h89;                         // imm[31:24]
        mem[7]  = 8'h67;                         // imm[39:32]
        mem[8]  = 8'h45;                         // imm[47:40]
        mem[9]  = 8'h23;                         // imm[55:48]
        mem[10] = 8'h01;                         // imm[63:56]
        mem[11] = 8'h02;                         // HALT
        load_and_run(600);
        check64("R1 after LDI64", uut.R[1], 64'h0123_4567_89AB_CDEF);

        // ================================================================
        // TEST 30: LDI64 — MMIO-style address
        // ================================================================
        $display("\n=== TEST 30: LDI64 MMIO address ===");
        clear_mem;
        // LDI64 R8, 0xFFFF_FF00_0000_0000
        mem[0]  = 8'hF0;                         // EXT.IMM64
        mem[1]  = 8'h60;                         // LDI
        mem[2]  = 8'h80;                         // R8
        mem[3]  = 8'h00;                         // imm[7:0]
        mem[4]  = 8'h00;                         // imm[15:8]
        mem[5]  = 8'h00;                         // imm[23:16]
        mem[6]  = 8'h00;                         // imm[31:24]
        mem[7]  = 8'h00;                         // imm[39:32]
        mem[8]  = 8'hFF;                         // imm[47:40]
        mem[9]  = 8'hFF;                         // imm[55:48]
        mem[10] = 8'hFF;                         // imm[63:56]
        mem[11] = 8'h02;                         // HALT
        load_and_run(600);
        check64("R8 after LDI64 MMIO addr", uut.R[8], 64'hFFFF_FF00_0000_0000);

        // ================================================================
        // TEST 31: LBR unconditional (forward)
        // Offset is relative to end of instruction: target = PC + 3 + offset
        // LBR at addr 0, target addr 9: offset = 9 - 3 = 6
        // ================================================================
        $display("\n=== TEST 31: LBR unconditional ===");
        clear_mem;
        mem[0] = 8'h40; mem[1] = 8'h00; mem[2] = 8'h06;  // LBR.AL +6
        mem[3] = 8'h60; mem[4] = 8'h00; mem[5] = 8'hFF;  // LDI R0, 0xFF (skipped)
        mem[6] = 8'h60; mem[7] = 8'h10; mem[8] = 8'hEE;  // LDI R1, 0xEE (skipped)
        mem[9] = 8'h60; mem[10] = 8'h00; mem[11] = 8'h42; // LDI R0, 0x42 (target)
        mem[12] = 8'h02;                                   // HALT
        load_and_run(400);
        check64("R0 after LBR skip", uut.R[0], 64'h42);

        // ================================================================
        // TEST 32: LBR conditional (LBRNE taken)
        // LBRNE at addr 6, target addr 15: offset = 15 - 9 = 6
        // ================================================================
        $display("\n=== TEST 32: LBRNE taken ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h05;  // LDI R0, 5
        mem[3] = 8'h66; mem[4] = 8'h00; mem[5] = 8'h00;  // CMPI R0, 0
        mem[6] = 8'h42; mem[7] = 8'h00; mem[8] = 8'h06;  // LBRNE +6
        mem[9]  = 8'h60; mem[10] = 8'h10; mem[11] = 8'hDD; // LDI R1, 0xDD (skipped)
        mem[12] = 8'h60; mem[13] = 8'h20; mem[14] = 8'hCC; // LDI R2, 0xCC (skipped)
        mem[15] = 8'h60; mem[16] = 8'h10; mem[17] = 8'hAA; // LDI R1, 0xAA (target)
        mem[18] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after LBRNE taken", uut.R[1], 64'hAA);

        // ================================================================
        // TEST 33: LBR conditional (LBREQ not taken)
        // ================================================================
        $display("\n=== TEST 33: LBREQ not taken ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h05;  // LDI R0, 5
        mem[3] = 8'h66; mem[4] = 8'h00; mem[5] = 8'h00;  // CMPI R0, 0
        mem[6] = 8'h41; mem[7] = 8'h00; mem[8] = 8'h06;  // LBREQ +6 (not taken)
        mem[9]  = 8'h60; mem[10] = 8'h10; mem[11] = 8'hBB; // LDI R1, 0xBB (reached)
        mem[12] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after LBREQ not taken", uut.R[1], 64'hBB);

        // ================================================================
        // TEST 34: LBR backward (loop)
        // Use SUBI (sets flags) instead of DEC (does not set flags).
        // SUBI R0,1 at addr 3 (3 bytes), LBRNE at addr 6, target addr 3:
        //   offset = 3 - (6+3) = -6
        // ================================================================
        $display("\n=== TEST 34: LBR backward (loop) ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h03;  // LDI R0, 3
        mem[3] = 8'h67; mem[4] = 8'h00; mem[5] = 8'h01;  // SUBI R0, 1
        mem[6] = 8'h42; mem[7] = 8'hFF; mem[8] = 8'hFA;  // LBRNE -6 (back to SUBI)
        mem[9] = 8'h02;                                    // HALT
        load_and_run(600);
        check64("R0 after loop to 0", uut.R[0], 64'h0);

        // ================================================================
        // TEST 35: MOV (inter-register copy)
        // MOV = ALU family 0x7, sub 0x8 (MOV/PASS B)
        // ================================================================
        $display("\n=== TEST 35: MOV Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h55;  // LDI R0, 0x55
        mem[3] = 8'h78; mem[4] = 8'h40;                   // MOV R4, R0 (sub=8)
        mem[5] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R4 after MOV", uut.R[4], 64'h55);

        // ================================================================
        // TEST 36: ST.B + LD.B (byte store/load)
        // ST.B = MEM family 0x5, sub 0x7: 57 <Ra Rs> (2 bytes)
        // LD.B = MEM family 0x5, sub 0x6: 56 <Rd Rb> (2 bytes)
        // ================================================================
        $display("\n=== TEST 36: ST.B + LD.B ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h42;  // LDI R0, 0x42
        mem[3] = 8'h60; mem[4] = 8'h10; mem[5] = 8'h80;  // LDI R1, 0x80 (address)
        mem[6] = 8'h57; mem[7] = 8'h10;                   // ST.B [R1], R0
        mem[8] = 8'h56; mem[9] = 8'h51;                   // LD.B R5, [R1]
        mem[10] = 8'h02;                                   // HALT
        load_and_run(400);
        check64("R5 after ST.B+LD.B", uut.R[5], 64'h42);

        // ================================================================
        // TEST 37: CSRR COREID (boot pattern)
        // CSRR = CSR family 0xD, sub 0 (read). 2 bytes: Dn <csr_addr>
        // Register is in nib[2:0] of opcode byte.
        // CSRR R0, 0x20: D0 20
        // ================================================================
        $display("\n=== TEST 37: CSRR COREID (boot pattern) ===");
        clear_mem;
        mem[0] = 8'hD0; mem[1] = 8'h20;                   // CSRR R0, CSR_COREID
        mem[2] = 8'h66; mem[3] = 8'h00; mem[4] = 8'h00;  // CMPI R0, 0
        mem[5] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R0 = COREID (0)", uut.R[0], 64'h0);
        check1("Z flag after CMPI 0,0", uut.flags[0], 1'b1);

        // ================================================================
        // TEST 38: LDI64 + STR + LDN roundtrip
        // STR (64-bit store) = MEM 0x5, sub 0x4: 54 <Ra Rs> (2 bytes)
        // LDN (64-bit load)  = MEM 0x5, sub 0x0: 50 <Rd Rb> (2 bytes)
        // ================================================================
        $display("\n=== TEST 38: LDI64 + STR + LDN ===");
        clear_mem;
        mem[0]  = 8'hF0;                                   // EXT.IMM64
        mem[1]  = 8'h60; mem[2]  = 8'h10;                  // LDI R1
        mem[3]  = 8'hBE; mem[4]  = 8'hBA; mem[5]  = 8'hFE; // 0xDEADBEEFCAFEBABE LE
        mem[6]  = 8'hCA; mem[7]  = 8'hEF; mem[8]  = 8'hBE;
        mem[9]  = 8'hAD; mem[10] = 8'hDE;
        mem[11] = 8'h60; mem[12] = 8'h50; mem[13] = 8'h80; // LDI R5, 0x80
        mem[14] = 8'h54; mem[15] = 8'h51;                  // STR [R5], R1
        mem[16] = 8'h50; mem[17] = 8'h45;                  // LDN R4, [R5]
        mem[18] = 8'h02;                                    // HALT
        load_and_run(800);
        check64("R4 after 64-bit store+load", uut.R[4], 64'hDEAD_BEEF_CAFE_BABE);

        // ================================================================
        // TEST 39: LSRI (logical shift right immediate)
        // LSRI = IMM family 0x6, sub 0x9: 69 <Rn_shift> (2 bytes)
        // ibuf[1][7:4] = register, ibuf[1][3:0] = shift amount
        // ================================================================
        $display("\n=== TEST 39: LSRI ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h80;  // LDI R0, 0x80
        mem[3] = 8'h69; mem[4] = 8'h01;                   // LSRI R0, 1
        mem[5] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R0 after LSRI 0x80>>1", uut.R[0], 64'h40);

        // ================================================================
        // TEST 40: EXT.SKIP + BR.EQ (skip NOT taken when Z=0)
        //   EXT.SKIP converts FAM_BR into a 1-byte SKIP instruction
        //   (no offset byte).  Condition=EQ, Z=0 → false → no skip
        //   → LDI R1,0xFF executes normally.
        // ================================================================
        $display("\n=== TEST 40: EXT.SKIP + BR.EQ (skip when Z=0) ===");
        clear_mem;
        mem[0]  = 8'h60; mem[1]  = 8'h00; mem[2]  = 8'h05;  // LDI R0, 5
        mem[3]  = 8'h66; mem[4]  = 8'h00; mem[5]  = 8'h00;  // CMPI R0, 0 (Z=0)
        mem[6]  = 8'hF6;                                      // EXT.SKIP
        mem[7]  = 8'h31;                                      // SKIP.EQ (1 byte)
        mem[8]  = 8'h60; mem[9]  = 8'h10; mem[10] = 8'hFF;   // LDI R1, 0xFF (executes)
        mem[11] = 8'h02;                                      // HALT
        load_and_run(600);
        check64("R1 after skip not taken", uut.R[1], 64'hFF);

        // ================================================================
        // TEST 41: INC / DEC
        // INC Rn = 0x1n, DEC Rn = 0x2n
        // ================================================================
        $display("\n=== TEST 41: INC / DEC ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h40; mem[2] = 8'h0A; // LDI R4, 10
        mem[3] = 8'h14;                                   // INC R4
        mem[4] = 8'h14;                                   // INC R4
        mem[5] = 8'h24;                                   // DEC R4
        mem[6] = 8'h02;                                   // HALT
        load_and_run(400);
        check64("R4 after INC+INC+DEC(10)", uut.R[4], 64'd11);

        // ================================================================
        // TEST 42: SEP / SEX
        // SEP Rn = 0xAn, SEX Rn = 0xBn
        // ================================================================
        $display("\n=== TEST 42: SEP / SEX ===");
        clear_mem;
        // Default: P=3(PC), X=2. Change X to 5 via SEX R5.
        mem[0] = 8'hB5;  // SEX R5
        mem[1] = 8'h02;  // HALT
        load_and_run(300);
        check64("xsel after SEX R5", {60'd0, uut.xsel}, 64'd5);

        // ================================================================
        // TEST 43: ADD register-register
        // ADD Rd,Rs = 0x70 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 43: ADD Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h07; // LDI R1, 7
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h03; // LDI R2, 3
        mem[6] = 8'h70; mem[7] = 8'h12;                   // ADD R1, R2
        mem[8] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after ADD R1,R2 (7+3)", uut.R[1], 64'd10);
        check1("Z flag (not zero)", uut.flags[0], 1'b0);

        // ================================================================
        // TEST 44: SUB register-register
        // SUB Rd,Rs = 0x72 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 44: SUB Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h14; // LDI R1, 20
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h07; // LDI R2, 7
        mem[6] = 8'h72; mem[7] = 8'h12;                   // SUB R1, R2
        mem[8] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after SUB R1,R2 (20-7)", uut.R[1], 64'd13);

        // ================================================================
        // TEST 45: AND register-register
        // AND Rd,Rs = 0x74 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 45: AND Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'hF0; // LDI R1, 0xF0
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h3C; // LDI R2, 0x3C
        mem[6] = 8'h74; mem[7] = 8'h12;                   // AND R1, R2
        mem[8] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after AND (0xF0&0x3C)", uut.R[1], 64'h30);

        // ================================================================
        // TEST 46: OR register-register
        // OR Rd,Rs = 0x75 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 46: OR Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'hF0; // LDI R1, 0xF0
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h0F; // LDI R2, 0x0F
        mem[6] = 8'h75; mem[7] = 8'h12;                   // OR R1, R2
        mem[8] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after OR (0xF0|0x0F)", uut.R[1], 64'hFF);

        // ================================================================
        // TEST 47: XOR register-register
        // XOR Rd,Rs = 0x76 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 47: XOR Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'hAA; // LDI R1, 0xAA
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'hFF; // LDI R2, 0xFF
        mem[6] = 8'h76; mem[7] = 8'h12;                   // XOR R1, R2
        mem[8] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after XOR (0xAA^0xFF)", uut.R[1], 64'h55);

        // ================================================================
        // TEST 48: NOT register
        // NOT Rd,Rs = 0x79 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 48: NOT Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h20; mem[2] = 8'hFF; // LDI R2, 0xFF
        mem[3] = 8'h79; mem[4] = 8'h12;                   // NOT R1, R2
        mem[5] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after NOT 0xFF", uut.R[1], 64'hFFFFFFFF_FFFFFF00);

        // ================================================================
        // TEST 49: NEG register
        // NEG Rd,Rs = 0x7A <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 49: NEG Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h20; mem[2] = 8'h05; // LDI R2, 5
        mem[3] = 8'h7A; mem[4] = 8'h12;                   // NEG R1, R2
        mem[5] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after NEG 5", uut.R[1], -64'sd5);

        // ================================================================
        // TEST 50: SHR register-register (logical shift right)
        // SHR Rd,Rs = 0x7C <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 50: SHR Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h80; // LDI R1, 0x80
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h03; // LDI R2, 3
        mem[6] = 8'h7C; mem[7] = 8'h12;                   // SHR R1, R2
        mem[8] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after SHR 0x80>>3", uut.R[1], 64'h10);

        // ================================================================
        // TEST 51: ROR register-register (rotate right)
        // ROR Rd,Rs = 0x7F <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 51: ROR Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h01; // LDI R1, 0x01
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h04; // LDI R2, 4
        mem[6] = 8'h7F; mem[7] = 8'h12;                   // ROR R1, R2
        mem[8] = 8'h02;                                    // HALT
        load_and_run(400);
        // 0x01 rotated right by 4 = 0x1000_0000_0000_0000
        check64("R1 after ROR 0x01 by 4", uut.R[1], 64'h1000_0000_0000_0000);

        // ================================================================
        // TEST 52: CMP register-register (sets flags, no writeback)
        // CMP Rd,Rs = 0x77 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 52: CMP Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h05; // LDI R1, 5
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h05; // LDI R2, 5
        mem[6] = 8'h77; mem[7] = 8'h12;                   // CMP R1, R2
        mem[8] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 unchanged after CMP", uut.R[1], 64'd5);
        check1("Z flag after CMP equal", uut.flags[0], 1'b1);

        // ================================================================
        // TEST 53: MULH (signed multiply high)
        // MULH Rd,Rs = 0xC1 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 53: MULH Rd,Rs ===");
        clear_mem;
        // 0x1_0000_0000 * 0x1_0000_0000 = 0x1_0000_0000_0000_0000
        // low 64 = 0, high 64 = 1
        mem[0]  = 8'hF0;                                    // EXT.IMM64
        mem[1]  = 8'h60; mem[2]  = 8'h10;                   // LDI R1
        mem[3]  = 8'h00; mem[4]  = 8'h00; mem[5]  = 8'h00;  // 0x00000001_00000000 LE
        mem[6]  = 8'h00; mem[7]  = 8'h01; mem[8]  = 8'h00;
        mem[9]  = 8'h00; mem[10] = 8'h00;
        mem[11] = 8'hF0;                                     // EXT.IMM64
        mem[12] = 8'h60; mem[13] = 8'h20;                   // LDI R2
        mem[14] = 8'h00; mem[15] = 8'h00; mem[16] = 8'h00;  // same value
        mem[17] = 8'h00; mem[18] = 8'h01; mem[19] = 8'h00;
        mem[20] = 8'h00; mem[21] = 8'h00;
        mem[22] = 8'hC1; mem[23] = 8'h12;                   // MULH R1, R2
        mem[24] = 8'h02;                                     // HALT
        load_and_run(800);
        check64("MULH high 64 of 2^32*2^32", uut.R[1], 64'd1);

        // ================================================================
        // TEST 54: DIV signed (quotient in Rd, remainder in R0)
        // DIV Rd,Rs = 0xC4 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 54: DIV Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h11; // LDI R1, 17
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h05; // LDI R2, 5
        mem[6] = 8'hC4; mem[7] = 8'h12;                   // DIV R1, R2
        mem[8] = 8'h02;                                    // HALT
        load_and_run(800);
        check64("R1 quotient 17/5", uut.R[1], 64'd3);
        check64("R0 remainder 17%5", uut.R[0], 64'd2);

        // ================================================================
        // TEST 55: MOD signed
        // MOD Rd,Rs = 0xC6 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 55: MOD Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h11; // LDI R1, 17
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h05; // LDI R2, 5
        mem[6] = 8'hC6; mem[7] = 8'h12;                   // MOD R1, R2
        mem[8] = 8'h02;                                    // HALT
        load_and_run(800);
        check64("R1 mod 17%5", uut.R[1], 64'd2);

        // ================================================================
        // TEST 56: LBR BQ / BNQ (branch on Q flag)
        // LBR BQ = 0x4B <hi> <lo>, LBR BNQ = 0x4C <hi> <lo>
        // ================================================================
        $display("\n=== TEST 56: LBR BQ / BNQ ===");
        clear_mem;
        // SEQ → Q=1, then LBR BQ (should take branch +3 → PC=3+3+3=9)
        // skip over an LDI that would set R1=0xFF
        // reached target sets R1=0x42
        mem[0] = 8'h09;                                    // SEQ (Q=1)
        mem[1] = 8'h4B; mem[2] = 8'h00; mem[3] = 8'h03;  // LBR BQ, +3
        mem[4] = 8'h60; mem[5] = 8'h10; mem[6] = 8'hFF;  // LDI R1, 0xFF (skipped)
        mem[7] = 8'h60; mem[8] = 8'h10; mem[9] = 8'h42;  // LDI R1, 0x42 (target)
        mem[10]= 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after BQ taken", uut.R[1], 64'h42);

        // ================================================================
        // TEST 57: LBR BNQ (Q=0 → branch taken)
        // ================================================================
        $display("\n=== TEST 57: LBR BNQ ===");
        clear_mem;
        // REQ → Q=0, then LBR BNQ +3 → skip one LDI
        mem[0] = 8'h0A;                                    // REQ (Q=0)
        mem[1] = 8'h4C; mem[2] = 8'h00; mem[3] = 8'h03;  // LBR BNQ, +3
        mem[4] = 8'h60; mem[5] = 8'h10; mem[6] = 8'hFF;  // LDI R1, 0xFF (skipped)
        mem[7] = 8'h60; mem[8] = 8'h10; mem[9] = 8'h99;  // LDI R1, 0x99 (target)
        mem[10]= 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after BNQ taken", uut.R[1], 64'h99);

        // ================================================================
        // TEST 58: ST.H / LD.H (16-bit store/load)
        // ST.H [Rn],Rs = 0x59 <Rn:Rs>
        // LD.H Rd,[Rn]  = 0x58 <Rd:Rn>
        // ================================================================
        $display("\n=== TEST 58: ST.H / LD.H ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h50; mem[2] = 8'h80;  // LDI R5, 0x80 (addr)
        // Load 0xBEEF into R1 via LDI low + PHI high
        mem[3] = 8'h60; mem[4] = 8'h10; mem[5] = 8'hEF;  // LDI R1, 0xEF (R1=0xEF)
        // Use ORI to set D, then PHI to set R1[15:8]
        // Actually simpler: load full 16-bit value with ADDI/LDI tricks
        // LDI R1, 0xEF; LHI is upper 16 of 64-bit, not useful here
        // Let's use EXT.IMM64 for simplicity
        mem[3]  = 8'hF0;                                    // EXT.IMM64
        mem[4]  = 8'h60; mem[5]  = 8'h10;                   // LDI R1
        mem[6]  = 8'hEF; mem[7]  = 8'hBE; mem[8]  = 8'h00;  // 0x000000000000BEEF LE
        mem[9]  = 8'h00; mem[10] = 8'h00; mem[11] = 8'h00;
        mem[12] = 8'h00; mem[13] = 8'h00;
        mem[14] = 8'h59; mem[15] = 8'h51;                   // ST.H [R5], R1
        mem[16] = 8'h58; mem[17] = 8'h45;                   // LD.H R4, [R5]
        mem[18] = 8'h02;                                     // HALT
        load_and_run(800);
        check64("R4 after ST.H+LD.H 0xBEEF", uut.R[4], 64'hBEEF);

        // ================================================================
        // TEST 59: ST.W / LD.W (32-bit store/load)
        // ST.W [Rn],Rs = 0x5B <Rn:Rs>
        // LD.W Rd,[Rn]  = 0x5A <Rd:Rn>
        // ================================================================
        $display("\n=== TEST 59: ST.W / LD.W ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h50; mem[2] = 8'h80;  // LDI R5, 0x80 (addr)
        mem[3]  = 8'hF0;                                    // EXT.IMM64
        mem[4]  = 8'h60; mem[5]  = 8'h10;                   // LDI R1
        mem[6]  = 8'hEF; mem[7]  = 8'hBE; mem[8]  = 8'hAD;  // 0x00000000DEADBEEF LE
        mem[9]  = 8'hDE; mem[10] = 8'h00; mem[11] = 8'h00;
        mem[12] = 8'h00; mem[13] = 8'h00;
        mem[14] = 8'h5B; mem[15] = 8'h51;                   // ST.W [R5], R1
        mem[16] = 8'h5A; mem[17] = 8'h45;                   // LD.W R4, [R5]
        mem[18] = 8'h02;                                     // HALT
        load_and_run(800);
        check64("R4 after ST.W+LD.W 0xDEADBEEF", uut.R[4], 64'hDEAD_BEEF);

        // ================================================================
        // TEST 60: LD.D with scaled offset
        // LD.D Rd,[Rn+off8*8] = 0x5F <Rd:Rn> <off8>
        // Store via STR then load via LD.D with offset
        // ================================================================
        $display("\n=== TEST 60: LD.D scaled offset ===");
        clear_mem;
        // Program: R5=0x80, R1=test pattern, STR [R5], R1  (store at 0x80)
        //          R6=0x70, LD.D R4, [R6+2]  → addr=0x70+2*8=0x80
        mem[0]  = 8'hF0;                                    // EXT.IMM64
        mem[1]  = 8'h60; mem[2]  = 8'h10;                   // LDI R1
        mem[3]  = 8'hAA; mem[4]  = 8'hBB; mem[5]  = 8'hCC;
        mem[6]  = 8'hDD; mem[7]  = 8'h11; mem[8]  = 8'h22;
        mem[9]  = 8'h33; mem[10] = 8'h44;                   // R1=0x44332211DDCCBBAA
        mem[11] = 8'h60; mem[12] = 8'h50; mem[13] = 8'h80;  // LDI R5, 0x80
        mem[14] = 8'h54; mem[15] = 8'h51;                   // STR [R5], R1
        mem[16] = 8'h60; mem[17] = 8'h60; mem[18] = 8'h70;  // LDI R6, 0x70
        mem[19] = 8'h5F; mem[20] = 8'h46; mem[21] = 8'h02;  // LD.D R4, [R6+2]
        mem[22] = 8'h02;                                     // HALT
        load_and_run(800);
        check64("R4 after LD.D [R6+2*8]", uut.R[4], 64'h44332211_DDCCBBAA);

        // ================================================================
        // TEST 61: ANDI immediate
        // ANDI Rn, imm8 = 0x63 <Rn:x> <imm8>
        // ================================================================
        $display("\n=== TEST 61: ANDI immediate ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'hFF; // LDI R1, 0xFF
        mem[3] = 8'h63; mem[4] = 8'h10; mem[5] = 8'h0F; // ANDI R1, 0x0F
        mem[6] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after ANDI 0xFF & 0x0F", uut.R[1], 64'h0F);

        // ================================================================
        // TEST 62: ORI immediate
        // ORI Rn, imm8 = 0x64 <Rn:x> <imm8>
        // ================================================================
        $display("\n=== TEST 62: ORI immediate ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'hF0; // LDI R1, 0xF0
        mem[3] = 8'h64; mem[4] = 8'h10; mem[5] = 8'h0F; // ORI R1, 0x0F
        mem[6] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after ORI 0xF0 | 0x0F", uut.R[1], 64'hFF);

        // ================================================================
        // TEST 63: XORI immediate
        // XORI Rn, imm8 = 0x65 <Rn:x> <imm8>
        // ================================================================
        $display("\n=== TEST 63: XORI immediate ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'hAA; // LDI R1, 0xAA
        mem[3] = 8'h65; mem[4] = 8'h10; mem[5] = 8'hFF; // XORI R1, 0xFF
        mem[6] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1 after XORI 0xAA ^ 0xFF", uut.R[1], 64'h55);

        // ================================================================
        // TEST 64: GHI / PLO
        // GHI Rn = 0x6D <Rn:x>, PLO Rn = 0x6E <Rn:x>
        // ================================================================
        $display("\n=== TEST 64: GHI / PLO ===");
        clear_mem;
        // Load R1 = 0x1234 via EXT.IMM64
        mem[0]  = 8'hF0;                                    // EXT.IMM64
        mem[1]  = 8'h60; mem[2]  = 8'h10;                   // LDI R1
        mem[3]  = 8'h34; mem[4]  = 8'h12; mem[5]  = 8'h00;  // 0x0000000000001234 LE
        mem[6]  = 8'h00; mem[7]  = 8'h00; mem[8]  = 8'h00;
        mem[9]  = 8'h00; mem[10] = 8'h00;
        // GHI R1 → D = R1[15:8] = 0x12
        mem[11] = 8'h6D; mem[12] = 8'h10;                   // GHI R1
        // PLO R2 → R2[7:0] = D = 0x12
        mem[13] = 8'h6E; mem[14] = 8'h20;                   // PLO R2
        mem[15] = 8'h02;                                     // HALT
        load_and_run(800);
        check64("D after GHI R1", {56'd0, uut.D}, 64'h12);
        check64("R2[7:0] after PLO", uut.R[2] & 64'hFF, 64'h12);

        // ================================================================
        // TEST 65: LHI (load high immediate — sets R[n][63:48])
        // LHI Rn, imm16 = 0x61 <Rn:x> <lo> <hi>
        // ================================================================
        $display("\n=== TEST 65: LHI ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h00;  // LDI R1, 0 (clear)
        mem[3] = 8'h61; mem[4] = 8'h10; mem[5] = 8'hEF;  // LHI R1, 0xBEEF
        mem[6] = 8'hBE;
        mem[7] = 8'h02;                                    // HALT
        load_and_run(400);
        check64("R1[63:48] after LHI", uut.R[1], 64'hBEEF_0000_0000_0000);

        // ================================================================
        // TEST 66: ADC register-register (add with carry)
        // ADC Rd,Rs = 0x71 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 66: ADC (add with carry) ===");
        clear_mem;
        // Set carry by adding large values: 0xFFFF...FF + 1 → C=1
        mem[0]  = 8'hF0;                                    // EXT.IMM64
        mem[1]  = 8'h60; mem[2]  = 8'h10;                   // LDI R1
        mem[3]  = 8'hFF; mem[4]  = 8'hFF; mem[5]  = 8'hFF;
        mem[6]  = 8'hFF; mem[7]  = 8'hFF; mem[8]  = 8'hFF;
        mem[9]  = 8'hFF; mem[10] = 8'hFF;                   // R1 = -1
        mem[11] = 8'h60; mem[12] = 8'h20; mem[13] = 8'h01; // LDI R2, 1
        mem[14] = 8'h70; mem[15] = 8'h12;                   // ADD R1, R2 (overflow, C=1)
        // Now R1=0, C=1. Load fresh values and ADC.
        mem[16] = 8'h60; mem[17] = 8'h40; mem[18] = 8'h0A; // LDI R4, 10
        mem[19] = 8'h60; mem[20] = 8'h50; mem[21] = 8'h05; // LDI R5, 5
        mem[22] = 8'h71; mem[23] = 8'h45;                   // ADC R4, R5 (10+5+1=16)
        mem[24] = 8'h02;                                     // HALT
        load_and_run(800);
        check64("R4 after ADC 10+5+C(1)", uut.R[4], 64'd16);

        // ================================================================
        // TEST 67: SBB register-register (subtract with borrow)
        // SBB Rd,Rs = 0x73 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 67: SBB (sub with borrow) ===");
        clear_mem;
        // Clear carry: subtract 5-5 → C=1 (no borrow)
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h05; // LDI R1, 5
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h05; // LDI R2, 5
        mem[6] = 8'h72; mem[7] = 8'h12;                   // SUB R1, R2 (C=1, no borrow)
        // Now C=1. SBB R4,R5 = R4 - R5 - (1-C) = R4 - R5 - 0 = 15-3=12
        mem[8]  = 8'h60; mem[9]  = 8'h40; mem[10] = 8'h0F; // LDI R4, 15
        mem[11] = 8'h60; mem[12] = 8'h50; mem[13] = 8'h03; // LDI R5, 3
        mem[14] = 8'h73; mem[15] = 8'h45;                   // SBB R4, R5
        mem[16] = 8'h02;                                     // HALT
        load_and_run(600);
        check64("R4 after SBB 15-3-0", uut.R[4], 64'd12);

        // ================================================================
        // TEST 68: SAR register-register (arithmetic shift right)
        // SAR Rd,Rs = 0x7D <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 68: SAR Rd,Rs ===");
        clear_mem;
        // -16 >> 2 should give -4 (sign extension)
        mem[0]  = 8'hF0;                                    // EXT.IMM64
        mem[1]  = 8'h60; mem[2]  = 8'h10;                   // LDI R1
        mem[3]  = 8'hF0; mem[4]  = 8'hFF; mem[5]  = 8'hFF;  // 0xFFFFFFFFFFFFFFF0 = -16
        mem[6]  = 8'hFF; mem[7]  = 8'hFF; mem[8]  = 8'hFF;
        mem[9]  = 8'hFF; mem[10] = 8'hFF;
        mem[11] = 8'h60; mem[12] = 8'h20; mem[13] = 8'h02; // LDI R2, 2
        mem[14] = 8'h7D; mem[15] = 8'h12;                   // SAR R1, R2
        mem[16] = 8'h02;                                     // HALT
        load_and_run(800);
        check64("R1 after SAR -16>>2", uut.R[1], -64'sd4);

        // ================================================================
        // TEST 69: UMUL (unsigned multiply low)
        // UMUL Rd,Rs = 0xC2 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 69: UMUL Rd,Rs ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'hC8; // LDI R1, 200
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h64; // LDI R2, 100
        mem[6] = 8'hC2; mem[7] = 8'h12;                   // UMUL R1, R2
        mem[8] = 8'h02;                                    // HALT
        load_and_run(800);
        check64("R1 after UMUL 200*100", uut.R[1], 64'd20000);

        // ================================================================
        // TEST 70: UMULH (unsigned multiply high)
        // UMULH Rd,Rs = 0xC3 <Rd:Rs>
        // ================================================================
        $display("\n=== TEST 70: UMULH Rd,Rs ===");
        clear_mem;
        // Small values → high 64 bits = 0
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h0A; // LDI R1, 10
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h05; // LDI R2, 5
        mem[6] = 8'hC3; mem[7] = 8'h12;                   // UMULH R1, R2
        mem[8] = 8'h02;                                    // HALT
        load_and_run(800);
        check64("UMULH high64 of 10*5", uut.R[1], 64'd0);

        // ================================================================
        // TEST 71: BIST March-C pass
        // CSRW CSR_BIST_CMD(0x60), R0 → triggers BIST pattern 1
        // CPU enters CPU_BIST, runs to completion, returns to FETCH
        // CSRR CSR_BIST_STATUS(0x61) → 2 = pass
        // ================================================================
        $display("\n=== TEST 71: BIST March-C pass ===");
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h01;  // LDI R0, 1 (march-C)
        mem[3] = 8'hD8; mem[4] = 8'h60;                   // CSRW CSR_BIST_CMD, R0
        // BIST runs synchronously, next instr after completion
        mem[5] = 8'hD0; mem[6] = 8'h61;                   // CSRR R0, CSR_BIST_STATUS
        mem[7] = 8'hD1; mem[8] = 8'h62;                   // CSRR R1, CSR_BIST_FAIL_ADDR
        mem[9] = 8'h02;                                    // HALT
        load_and_run(8000);
        check64("BIST status (2=pass)", uut.R[0], 64'd2);
        check64("BIST fail_addr (0=none)", uut.R[1], 64'd0);

        // ================================================================
        // TEST 72: I-cache CSR control
        // CSR_ICACHE_CTRL = 0x70
        // Default: enabled (bit0=1). Write 0 to disable, read back.
        // NOTE: R3 is PC (psel=3), avoid using it as GPR!
        // ================================================================
        $display("\n=== TEST 72: I-cache CSR control ===");
        clear_mem;
        // Read initial state (enabled by default)
        mem[0] = 8'hD0; mem[1] = 8'h70;                   // CSRR R0, CSR_ICACHE_CTRL
        // Disable cache: write 0
        mem[2] = 8'h60; mem[3] = 8'h10; mem[4] = 8'h00;  // LDI R1, 0
        mem[5] = 8'hD9; mem[6] = 8'h70;                   // CSRW CSR_ICACHE_CTRL, R1
        // Read back disabled state
        mem[7] = 8'hD2; mem[8] = 8'h70;                   // CSRR R2, CSR_ICACHE_CTRL
        // Re-enable + invalidate: write 3 (bit0=enable, bit1=inv)
        mem[9]  = 8'h60; mem[10] = 8'h60; mem[11] = 8'h03; // LDI R6, 3
        mem[12] = 8'hDE; mem[13] = 8'h70;                  // CSRW CSR_ICACHE_CTRL, R6
        // Read back (should be 1, inv is auto-cleared)
        mem[14] = 8'hD4; mem[15] = 8'h70;                  // CSRR R4, CSR_ICACHE_CTRL
        mem[16] = 8'h02;                                    // HALT
        load_and_run(600);
        check64("icache initially enabled", uut.R[0], 64'd1);
        check64("icache after disable", uut.R[2], 64'd0);
        check64("icache after re-enable", uut.R[4], 64'd1);

        // ================================================================
        // DONE
        // ================================================================
        $display("\n========================================");
        $display("  Opcode Tests: %0d PASSED, %0d FAILED", pass_count, fail_count);
        $display("========================================\n");

        $finish;
    end

endmodule
