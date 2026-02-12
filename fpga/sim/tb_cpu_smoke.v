// ============================================================================
// tb_cpu_smoke.v — CPU Core Smoke Tests
// ============================================================================
//
// Runs the CPU core standalone against small test programs loaded into
// a simple 1-cycle-latency memory model.  Verifies fetch/decode/execute:
//   - NOP + HALT
//   - INC / DEC (single-byte)
//   - LDI (3-byte immediate load — tests multi-byte fetch)
//   - ALU register-register (ADD, SUB, AND, OR, XOR, SHL, SHR, CMP)
//   - SEP (PC register switch)
//   - Flag verification (Z, C, N, G)
//
// IMPORTANT: test programs are loaded into memory WHILE the CPU is in
// reset, guaranteeing data is stable before the first fetch.
//

`timescale 1ns / 1ps
`include "mp64_defs.vh"

module tb_cpu_smoke;

    // ========================================================================
    // Clock / Reset
    // ========================================================================
    reg clk, rst_n;

    initial clk = 0;
    always #5 clk = ~clk;  // 100 MHz

    // ========================================================================
    // Simple 1-cycle-latency memory model (8 KiB, byte-addressable)
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
    // DUT — CPU core with tile engine stubs + I-cache
    // ========================================================================
    reg  [63:0] csr_rdata;
    wire        csr_wen;
    wire [7:0]  csr_addr;
    wire [63:0] csr_wdata;
    wire        mex_valid;
    wire [1:0]  mex_ss, mex_op;
    wire [2:0]  mex_funct;
    wire [63:0] mex_gpr_val;
    wire [7:0]  mex_imm8;

    initial csr_rdata = 64'd0;

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
        .rst_n      (rst_n),
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

    mp64_cpu u_cpu (
        .clk        (clk),
        .rst_n      (rst_n),
        .core_id    ({CORE_ID_BITS{1'b0}}),  // core 0 for smoke tests

        // I-cache interface
        .icache_addr    (ic_fetch_addr),
        .icache_req     (ic_fetch_req),
        .icache_data    (ic_fetch_data),
        .icache_hit     (ic_fetch_hit),
        .icache_stall   (ic_fetch_stall),
        .icache_inv_all (ic_inv_all),
        .icache_inv_line(ic_inv_line),
        .icache_inv_addr(ic_inv_addr),

        // Data bus
        .bus_valid  (bus_valid),
        .bus_addr   (bus_addr),
        .bus_wdata  (bus_wdata),
        .bus_wen    (bus_wen),
        .bus_size   (bus_size),
        .bus_rdata  (bus_rdata),
        .bus_ready  (bus_ready),
        .csr_wen    (csr_wen),
        .csr_addr   (csr_addr),
        .csr_wdata  (csr_wdata),
        .csr_rdata  (csr_rdata),
        .mex_valid  (mex_valid),
        .mex_ss     (mex_ss),
        .mex_op     (mex_op),
        .mex_funct  (mex_funct),
        .mex_gpr_val(mex_gpr_val),
        .mex_imm8   (mex_imm8),
        .mex_done   (1'b0),
        .mex_busy   (1'b0),
        .irq_timer  (1'b0),
        .irq_uart   (1'b0),
        .irq_nic    (1'b0),
        .irq_ipi    (1'b0),
        .ef_flags   (4'b0000)
    );

    // ========================================================================
    // Test helpers
    // ========================================================================
    integer pass_cnt, fail_cnt;
    integer i;

    // Assert reset and zero memory — call BEFORE loading the program.
    // After this returns, rst_n is still LOW.  Load your program into mem[],
    // then call release_reset.
    task hold_reset;
    begin
        rst_n = 0;
        for (i = 0; i < 8192; i = i + 1)
            mem[i] = 8'h00;
        repeat (5) @(posedge clk);
    end
    endtask

    // Release reset — call AFTER loading program into mem[].
    task release_reset;
    begin
        rst_n = 1;
        repeat (2) @(posedge clk);  // give CPU 2 cycles to start
    end
    endtask

    task wait_halt;
        input integer max_cycles;
        integer cyc;
    begin
        cyc = 0;
        while (u_cpu.cpu_state != 4'd7 && cyc < max_cycles) begin
            @(posedge clk);
            cyc = cyc + 1;
        end
        if (cyc >= max_cycles)
            $display("  TIMEOUT after %0d cycles  (state=%0d  PC=0x%h)",
                     max_cycles, u_cpu.cpu_state, u_cpu.R[u_cpu.psel]);
    end
    endtask

    task check_reg;
        input [3:0]  rn;
        input [63:0] expected;
    begin
        if (u_cpu.R[rn] === expected) begin
            $display("  PASS: R%0d = 0x%h", rn, u_cpu.R[rn]);
            pass_cnt = pass_cnt + 1;
        end else begin
            $display("  FAIL: R%0d = 0x%h  (expected 0x%h)", rn,
                     u_cpu.R[rn], expected);
            fail_cnt = fail_cnt + 1;
        end
    end
    endtask

    task check_pc;
        input [63:0] expected;
    begin
        if (u_cpu.R[u_cpu.psel] === expected) begin
            $display("  PASS: PC = 0x%h", u_cpu.R[u_cpu.psel]);
            pass_cnt = pass_cnt + 1;
        end else begin
            $display("  FAIL: PC = 0x%h  (expected 0x%h)",
                     u_cpu.R[u_cpu.psel], expected);
            fail_cnt = fail_cnt + 1;
        end
    end
    endtask

    // Flag map: [S:7 I:6 G:5 P:4 V:3 N:2 C:1 Z:0]
    task check_flag;
        input [2:0] bit_pos;
        input       expected;
    begin
        if (u_cpu.flags[bit_pos] === expected) begin
            $display("  PASS: flags[%0d] = %0b", bit_pos, u_cpu.flags[bit_pos]);
            pass_cnt = pass_cnt + 1;
        end else begin
            $display("  FAIL: flags[%0d] = %0b  (expected %0b)", bit_pos,
                     u_cpu.flags[bit_pos], expected);
            fail_cnt = fail_cnt + 1;
        end
    end
    endtask

    // ========================================================================
    // Test cases
    // ========================================================================
    initial begin
        $dumpfile("tb_cpu_smoke.vcd");
        $dumpvars(0, tb_cpu_smoke);

        pass_cnt = 0;
        fail_cnt = 0;

        // ================================================================
        // TEST 1 — NOP + HALT  (single-byte fetch only)
        // ================================================================
        $display("\n=== TEST 1: 15×NOP + HALT ===");
        hold_reset;
        for (i = 0; i < 15; i = i + 1) mem[i] = 8'h01;  // NOP
        mem[15] = 8'h02;                                    // HALT
        release_reset;
        wait_halt(2000);
        check_pc(64'h10);   // 16 bytes → PC=16

        // ================================================================
        // TEST 2 — INC / DEC
        // ================================================================
        $display("\n=== TEST 2: INC R0 ×4, DEC R0 ===");
        hold_reset;
        mem[0] = 8'h10;  // INC R0
        mem[1] = 8'h10;  // INC R0
        mem[2] = 8'h10;  // INC R0
        mem[3] = 8'h10;  // INC R0
        mem[4] = 8'h20;  // DEC R0
        mem[5] = 8'h02;  // HALT
        release_reset;
        wait_halt(2000);
        check_reg(4'd0, 64'd3);

        // ================================================================
        // TEST 3 — LDI (3-byte instruction, tests multi-byte fetch)
        // ================================================================
        $display("\n=== TEST 3: LDI R0, 0x42 ===");
        hold_reset;
        mem[0] = 8'h60;  // FAM_IMM | LDI
        mem[1] = 8'h00;  // ibuf[1][7:4]=R0
        mem[2] = 8'h42;  // imm8
        mem[3] = 8'h02;  // HALT
        release_reset;
        wait_halt(2000);
        check_reg(4'd0, 64'h42);

        // ================================================================
        // TEST 4 — LDI + LDI + ADD
        // ================================================================
        $display("\n=== TEST 4: LDI + LDI + ADD R0,R1 ===");
        hold_reset;
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h0A;  // LDI R0, 10
        mem[3] = 8'h60; mem[4] = 8'h10; mem[5] = 8'h14;  // LDI R1, 20
        mem[6] = 8'h70; mem[7] = 8'h01;                    // ADD R0, R1
        mem[8] = 8'h02;                                     // HALT
        release_reset;
        wait_halt(2000);
        check_reg(4'd0, 64'd30);
        check_reg(4'd1, 64'd20);

        // ================================================================
        // TEST 5 — SUB + zero flag
        // ================================================================
        $display("\n=== TEST 5: SUB, Z flag ===");
        hold_reset;
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h05;  // LDI R0, 5
        mem[3] = 8'h60; mem[4] = 8'h10; mem[5] = 8'h05;  // LDI R1, 5
        mem[6] = 8'h72; mem[7] = 8'h01;                    // SUB R0, R1
        mem[8] = 8'h02;                                     // HALT
        release_reset;
        wait_halt(2000);
        check_reg(4'd0, 64'd0);
        check_flag(3'd0, 1'b1);    // Z = 1

        // ================================================================
        // TEST 6 — CMP (no writeback) + G flag
        // ================================================================
        $display("\n=== TEST 6: CMP, no writeback, G flag ===");
        hold_reset;
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h0A;  // LDI R0, 10
        mem[3] = 8'h60; mem[4] = 8'h10; mem[5] = 8'h05;  // LDI R1, 5
        mem[6] = 8'h77; mem[7] = 8'h01;                    // CMP R0, R1
        mem[8] = 8'h02;                                     // HALT
        release_reset;
        wait_halt(2000);
        check_reg(4'd0, 64'd10);  // unchanged
        check_flag(3'd0, 1'b0);   // Z = 0
        check_flag(3'd5, 1'b1);   // G = 1

        // ================================================================
        // TEST 7 — INC multiple registers
        // ================================================================
        $display("\n=== TEST 7: INC R1×2, INC R5, INC R15 ===");
        hold_reset;
        mem[0] = 8'h11;  // INC R1
        mem[1] = 8'h11;  // INC R1
        mem[2] = 8'h15;  // INC R5
        mem[3] = 8'h1F;  // INC R15
        mem[4] = 8'h02;  // HALT
        release_reset;
        wait_halt(2000);
        check_reg(4'd1,  64'd2);
        check_reg(4'd5,  64'd1);
        check_reg(4'd15, 64'd1);

        // ================================================================
        // TEST 8 — AND / OR / XOR
        // ================================================================
        $display("\n=== TEST 8: AND / OR / XOR ===");
        hold_reset;
        mem[0]  = 8'h60; mem[1]  = 8'h00; mem[2]  = 8'hFF;  // LDI R0, 0xFF
        mem[3]  = 8'h60; mem[4]  = 8'h10; mem[5]  = 8'h0F;  // LDI R1, 0x0F
        mem[6]  = 8'h60; mem[7]  = 8'h20; mem[8]  = 8'hFF;  // LDI R2, 0xFF
        mem[9]  = 8'h60; mem[10] = 8'h40; mem[11] = 8'hFF;  // LDI R4, 0xFF
        mem[12] = 8'h74; mem[13] = 8'h01;                     // AND R0, R1
        mem[14] = 8'h75; mem[15] = 8'h21;                     // OR  R2, R1
        mem[16] = 8'h76; mem[17] = 8'h41;                     // XOR R4, R1
        mem[18] = 8'h02;                                       // HALT
        release_reset;
        wait_halt(2000);
        check_reg(4'd0, 64'h0F);
        check_reg(4'd2, 64'hFF);
        check_reg(4'd4, 64'hF0);

        // ================================================================
        // TEST 9 — SHL / SHR
        // ================================================================
        $display("\n=== TEST 9: SHL / SHR ===");
        hold_reset;
        mem[0] = 8'h60; mem[1] = 8'h00; mem[2] = 8'h01;  // LDI R0, 1
        mem[3] = 8'h60; mem[4] = 8'h10; mem[5] = 8'h04;  // LDI R1, 4
        mem[6] = 8'h7B; mem[7] = 8'h01;                    // SHL R0, R1
        mem[8]  = 8'h60; mem[9]  = 8'h20; mem[10] = 8'h80; // LDI R2, 0x80
        mem[11] = 8'h7C; mem[12] = 8'h21;                   // SHR R2, R1
        mem[13] = 8'h02;                                     // HALT
        release_reset;
        wait_halt(2000);
        check_reg(4'd0, 64'd16);  // 1 << 4
        check_reg(4'd2, 64'd8);   // 0x80 >> 4

        // ================================================================
        // TEST 10 — SEP (switch PC register)
        // ================================================================
        $display("\n=== TEST 10: SEP R4 ===");
        hold_reset;
        mem[0] = 8'h60; mem[1] = 8'h40; mem[2] = 8'h08; // LDI R4, 8
        mem[3] = 8'hA4;                                    // SEP R4
        mem[4] = 8'h02;                                    // (dead — skipped)
        mem[8] = 8'h10;                                    // INC R0
        mem[9] = 8'h02;                                    // HALT
        release_reset;
        wait_halt(2000);
        check_reg(4'd0, 64'd1);  // INC R0 executed at addr 8

        // ================================================================
        // SUMMARY
        // ================================================================
        $display("\n========================================");
        $display("  CPU Smoke Tests: %0d PASSED, %0d FAILED", pass_cnt, fail_cnt);
        $display("========================================\n");

        if (fail_cnt > 0) $finish(1);
        $finish(0);
    end

    // Global timeout watchdog
    initial begin
        #5000000;  // 5 ms
        $display("GLOBAL TIMEOUT — aborting");
        $finish(1);
    end

endmodule
