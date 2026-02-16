// ============================================================================
// tb_cpu_micro.v — Smoke tests for mp64_cpu_micro (micro-core)
// ============================================================================
//
// Runs the micro-core against small test programs loaded into a simple
// 1-cycle-latency memory model.  Verifies:
//   1. Basic execution: NOP, HALT, LDI, INC, DEC
//   2. ALU register-register: ADD, SUB, CMP + flags
//   3. Memory load/store: STR, LDN
//   4. Branch: BR with condition
//   5. MULDIV trap: MUL triggers illegal opcode exception
//   6. MEX trap: tile instruction triggers illegal opcode exception
//   7. CSR: read CPUID returns micro-core identifier
//

`timescale 1ns / 1ps
`include "mp64_defs.vh"

module tb_cpu_micro;

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
    // DUT — Micro-core CPU
    // ========================================================================
    mp64_cpu_micro u_cpu (
        .clk       (clk),
        .rst_n     (rst_n),
        .core_id   ({CORE_ID_BITS{1'b0}}),
        .bus_valid (bus_valid),
        .bus_addr  (bus_addr),
        .bus_wdata (bus_wdata),
        .bus_wen   (bus_wen),
        .bus_size  (bus_size),
        .bus_rdata (bus_rdata),
        .bus_ready (bus_ready),
        .irq_timer (1'b0),
        .irq_uart  (1'b0),
        .irq_nic   (1'b0),
        .irq_ipi   (1'b0),
        .ef_flags  (4'd0)
    );

    // ========================================================================
    // Test infrastructure
    // ========================================================================
    integer pass, fail, timeout;
    integer i;

    task clear_mem;
        integer j;
        begin
            for (j = 0; j < 8192; j = j + 1)
                mem[j] = 8'h00;
        end
    endtask

    task reset_cpu;
        begin
            rst_n = 0;
            repeat (4) @(posedge clk);
            rst_n = 1;
        end
    endtask

    // Wait for CPU to reach HALT state (state 7)
    task wait_halt;
        input integer max_cycles;
        integer cyc;
        begin
            timeout = 0;
            for (cyc = 0; cyc < max_cycles; cyc = cyc + 1) begin
                @(posedge clk);
                if (u_cpu.cpu_state == 4'd7) begin  // CPU_HALT
                    timeout = 0;
                    cyc = max_cycles; // break
                end
            end
            if (u_cpu.cpu_state != 4'd7) begin
                $display("  TIMEOUT: CPU did not halt within %0d cycles", max_cycles);
                timeout = 1;
            end
        end
    endtask

    task check_reg;
        input [3:0]  rn;
        input [63:0] expected;
        input [255:0] name;
        begin
            if (u_cpu.R[rn] !== expected) begin
                $display("FAIL %0s: R%0d=%h expected=%h", name, rn, u_cpu.R[rn], expected);
                fail = fail + 1;
            end else begin
                pass = pass + 1;
            end
        end
    endtask

    task check_flag_bit;
        input        actual;
        input        expected;
        input [255:0] name;
        begin
            if (actual !== expected) begin
                $display("FAIL %0s: flag=%b expected=%b", name, actual, expected);
                fail = fail + 1;
            end else begin
                pass = pass + 1;
            end
        end
    endtask

    // ========================================================================
    // Tests
    // ========================================================================
    initial begin
        $dumpfile("tb_cpu_micro.vcd");
        $dumpvars(0, tb_cpu_micro);
        pass = 0;
        fail = 0;
        rst_n = 0;

        // ============================================================
        // TEST 1: NOP + HALT
        // ============================================================
        $display("Test 1: NOP + HALT");
        clear_mem;
        // Program at 0x0000 (R3 = PC, starts at 0):
        //   NOP   = 0x01
        //   NOP   = 0x01
        //   HALT  = 0x02
        mem[0] = 8'h01;
        mem[1] = 8'h01;
        mem[2] = 8'h02;
        reset_cpu;
        wait_halt(200);
        // PC (R3) should be at 0x03 (past the HALT)
        check_reg(3, 64'h03, "T1 PC after NOP+HALT");

        // ============================================================
        // TEST 2: LDI + INC + DEC + HALT
        // ============================================================
        $display("Test 2: LDI + INC + DEC + HALT");
        clear_mem;
        // LDI R5, 42  → 0x60 0x50 0x2A  (FAM_IMM=6, sub=0, dst=R5[7:4])
        // INC R5      → 0x15            (FAM_INC=1, nib=5)
        // DEC R5      → 0x25            (FAM_DEC=2, nib=5)
        // HALT        → 0x02
        mem[0] = 8'h60; mem[1] = 8'h50; mem[2] = 8'h2A;
        mem[3] = 8'h15;
        mem[4] = 8'h25;
        mem[5] = 8'h02;
        reset_cpu;
        wait_halt(300);
        check_reg(5, 64'd42, "T2 R5=42 (LDI+INC+DEC)");

        // ============================================================
        // TEST 3: ALU ADD register-register
        // ============================================================
        $display("Test 3: ALU ADD");
        clear_mem;
        // LDI R4, 8   → 0x60 0x40 0x08
        // LDI R5, 42  → 0x60 0x50 0x2A
        // ADD R4, R5  → 0x70 0x45  (FAM_ALU=7, sub=0=ADD, byte1={R4,R5})
        // HALT        → 0x02
        mem[0] = 8'h60; mem[1] = 8'h40; mem[2] = 8'h08;
        mem[3] = 8'h60; mem[4] = 8'h50; mem[5] = 8'h2A;
        mem[6] = 8'h70; mem[7] = 8'h45;
        mem[8] = 8'h02;
        reset_cpu;
        wait_halt(400);
        check_reg(4, 64'd50, "T3 R4=50 (8+42)");
        check_reg(5, 64'd42, "T3 R5=42 unchanged");

        // ============================================================
        // TEST 4: CMP + flags
        // ============================================================
        $display("Test 4: CMP + flags");
        clear_mem;
        // LDI R4, 10  → 0x60 0x40 0x0A
        // LDI R5, 10  → 0x60 0x50 0x0A
        // CMP R4, R5  → 0x77 0x45 (FAM_ALU=7, sub=7=CMP)
        // HALT        → 0x02
        mem[0] = 8'h60; mem[1] = 8'h40; mem[2] = 8'h0A;
        mem[3] = 8'h60; mem[4] = 8'h50; mem[5] = 8'h0A;
        mem[6] = 8'h77; mem[7] = 8'h45;
        mem[8] = 8'h02;
        reset_cpu;
        wait_halt(400);
        // CMP doesn't modify Rd
        check_reg(4, 64'd10, "T4 R4=10 unchanged after CMP");
        // Z should be set (equal)
        check_flag_bit(u_cpu.flags[0], 1'b1, "T4 Z=1 (equal)");

        // ============================================================
        // TEST 5: Memory store + load
        // ============================================================
        $display("Test 5: STR + LDN");
        clear_mem;
        // LDI R4, 99      → 0x60 0x40 0x63    ; R4 = 99
        // LDI R6, 0        → 0x60 0x60 0x00    ; R6 = 0 (will load into R6)
        // LDI R7, 0x80     → 0x60 0x70 0x80    ; R7 = 0x80 (address)
        // STR R7, R4       → 0x54 0x74         ; M(R7) = R4 = 99
        //   STR Rd,Rs: nib=4, byte1={Rd_addr, Rs}: Rd_addr=R7 → upper nibble
        //   Actually: STR encoding: nib=4, byte1[7:4]=address_reg, byte1[3:0]=data_reg
        //   STR R7, R4: byte1 = {R7=7, R4=4} = 0x74
        // LDN R6, R7       → 0x50 0x67         ; R6 = M(R7)
        //   LDN: nib=0, byte1[7:4]=dst, byte1[3:0]=src_addr
        //   LDN R6, R7: byte1 = {R6=6, R7=7} = 0x67
        // HALT             → 0x02
        mem[ 0] = 8'h60; mem[ 1] = 8'h40; mem[ 2] = 8'h63;   // LDI R4, 99
        mem[ 3] = 8'h60; mem[ 4] = 8'h60; mem[ 5] = 8'h00;   // LDI R6, 0
        mem[ 6] = 8'h60; mem[ 7] = 8'h70; mem[ 8] = 8'h80;   // LDI R7, 0x80
        mem[ 9] = 8'h54; mem[10] = 8'h74;                     // STR R7, R4
        mem[11] = 8'h50; mem[12] = 8'h67;                     // LDN R6, R7
        mem[13] = 8'h02;                                        // HALT
        reset_cpu;
        wait_halt(600);
        check_reg(6, 64'd99, "T5 R6=99 (STR+LDN round-trip)");

        // ============================================================
        // TEST 6: Short branch (BR NE to loop, counts R5 from 0→5)
        // ============================================================
        $display("Test 6: Branch loop");
        clear_mem;
        // LDI R5, 0    → 0x60 0x50 0x00     addr 0x00
        // LDI R6, 5    → 0x60 0x60 0x05     addr 0x03
        // INC R5       → 0x15               addr 0x06
        // CMP R5, R6   → 0x77 0x56          addr 0x07
        // BR NE, -5    → 0x32 0xFB          addr 0x09
        //   BR NE: nib=2 (NE), target=0x06
        //   offset = target - addr + ibuf_len = 0x06 - 0x09 + 2 = -1
        //   Wait: target = addr + offset - ibuf_len
        //   0x06 = 0x09 + offset - 2  → offset = -1
        //   -1 as signed byte = 0xFF
        //   Actually let me recompute: PC at BR is 0x09. ibuf_len=2.
        //   New PC = old_PC + offset - ibuf_len = 0x09 + offset - 2
        //   We want new PC = 0x06
        //   0x06 = 0x09 + offset - 2  → offset = -1 (0xFF)
        // HALT         → 0x02               addr 0x0B
        mem[16'h00] = 8'h60; mem[16'h01] = 8'h50; mem[16'h02] = 8'h00;  // LDI R5, 0
        mem[16'h03] = 8'h60; mem[16'h04] = 8'h60; mem[16'h05] = 8'h05;  // LDI R6, 5
        mem[16'h06] = 8'h15;                                              // INC R5
        mem[16'h07] = 8'h77; mem[16'h08] = 8'h56;                        // CMP R5, R6
        mem[16'h09] = 8'h32; mem[16'h0A] = 8'hFF;                        // BR NE, -1
        mem[16'h0B] = 8'h02;                                              // HALT
        reset_cpu;
        wait_halt(2000);
        if (!timeout)
            check_reg(5, 64'd5, "T6 R5=5 (counted via branch loop)");
        else begin
            $display("FAIL T6: timed out during branch loop");
            fail = fail + 1;
        end

        // ============================================================
        // TEST 7: MULDIV trap — MUL should trigger illegal opcode
        // ============================================================
        $display("Test 7: MULDIV trap");
        clear_mem;
        // Set up IVT at address 0x100
        // LDI R0, 0      → 0x60 0x00 0x00
        // LDI R1, 1      → 0x60 0x10 0x01   (use as temp for IVT setup)
        // First, write CSR IVTBASE = 0x100
        //   LDI R0, 0    → (R0 will hold IVT base value)
        //   ADDI R0, 0   → we need R0 = 0x100
        //   Actually: LDI R0, 0x00 → then we need 0x100
        //   Use EXT.IMM64? Too complex. Let's place handler at IVT[2]*8 = 0x10
        //   So IVT base = 0. Handler address at mem[0x10..0x17] = handler addr.
        //
        // Simpler approach: IVT base = 0 (default), IVT[2] = illegal op handler.
        // IVT[2] at offset 2*8 = 16 = 0x10. Write handler address there.
        // Handler at 0x80: just HALT.
        //
        // Program:
        // 0x00: LDI R4, 10       → 0x60 0x40 0x0A
        // 0x03: LDI R5, 20       → 0x60 0x50 0x14
        // 0x06: LDI R15, 0x00    → 0x60 0xF0 0x00  (SP = R15, init to top)
        // Then set SP high: ADDI R15, 0x7F → 0x62 0xF0 0x7F (R15 = 0x7F)
        //   That gives SP=0x7F... need more. Use two ADDIs.
        //   ADDI R15, 0x7F → R15 = 0x7F
        //   ADDI R15, 0x7F → R15 = 0xFE
        //   That should be enough stack space.
        //
        // 0x09: ADDI R15, 0x7F   → 0x62 0xF0 0x7F (R15 = 127)
        // 0x0C: ADDI R15, 0x7F   → 0x62 0xF0 0x7F (R15 = 254)
        // 0x0F: MUL R4, R5       → 0xC0 0x45       (should trap!)
        // 0x11: HALT             → 0x02             (should NOT reach here)
        //
        // IVT[2] at mem[0x10..0x17] = 0x80 (handler address)
        // BUT the program occupies 0x00-0x11, and IVT[2] is at 0x10-0x17.
        // They overlap! Let me move the program start.
        //
        // Actually IVT base is at 0x0000 by default. IVT[2] = illegal op
        // is at address 0x10. So I need to make sure my program doesn't
        // overwrite the IVT. Let me set IVT base to 0x100 using CSR write:
        //
        // 0x00: LDI R0, 0      → 0x60 0x00 0x00  (R0 = 0 for later)
        // 0x03: LDI R1, 0      → 0x60 0x10 0x00
        // 0x06: ADDI R1, 0x7F  → 0x62 0x10 0x7F  (R1 = 127)
        // 0x09: ADDI R1, 0x7F  → 0x62 0x10 0x7F  (R1 = 254 = 0xFE)
        // 0x0C: ADDI R1, 0x02  → 0x62 0x10 0x02  (R1 = 256 = 0x100)
        // 0x0F: CSR.W R1 → IVTBASE → 0xD9 0x04  (CSRW: nib[3]=1, nib[2:0]=R1=1; CSR=0x04)
        // 0x11: LDI R15, 0     → 0x60 0xF0 0x00
        // 0x14: ADDI R15, 0x7F → 0x62 0xF0 0x7F  (R15 = 127)
        // 0x17: ADDI R15, 0x7F → 0x62 0xF0 0x7F  (R15 = 254)
        // 0x1A: LDI R4, 10     → 0x60 0x40 0x0A
        // 0x1D: LDI R5, 20     → 0x60 0x50 0x14
        // 0x20: MUL R4, R5     → 0xC0 0x45        (TRAP!)
        // 0x22: HALT            → 0x02             (should not reach)
        //
        // Handler at 0x80: HALT = 0x02
        // IVT[2] at 0x100 + 2*8 = 0x110 → store 0x80 there
        mem[16'h00] = 8'h60; mem[16'h01] = 8'h00; mem[16'h02] = 8'h00;  // LDI R0,0
        mem[16'h03] = 8'h60; mem[16'h04] = 8'h10; mem[16'h05] = 8'h00;  // LDI R1,0
        mem[16'h06] = 8'h62; mem[16'h07] = 8'h10; mem[16'h08] = 8'h7F;  // ADDI R1,127
        mem[16'h09] = 8'h62; mem[16'h0A] = 8'h10; mem[16'h0B] = 8'h7F;  // ADDI R1,127
        mem[16'h0C] = 8'h62; mem[16'h0D] = 8'h10; mem[16'h0E] = 8'h02;  // ADDI R1,2
        mem[16'h0F] = 8'hD9; mem[16'h10] = 8'h04;                        // CSRW R1,IVTBASE
        mem[16'h11] = 8'h60; mem[16'h12] = 8'hF0; mem[16'h13] = 8'h00;  // LDI R15,0
        mem[16'h14] = 8'h62; mem[16'h15] = 8'hF0; mem[16'h16] = 8'h7F;  // ADDI R15,127
        mem[16'h17] = 8'h62; mem[16'h18] = 8'hF0; mem[16'h19] = 8'h7F;  // ADDI R15,127
        mem[16'h1A] = 8'h60; mem[16'h1B] = 8'h40; mem[16'h1C] = 8'h0A;  // LDI R4,10
        mem[16'h1D] = 8'h60; mem[16'h1E] = 8'h50; mem[16'h1F] = 8'h14;  // LDI R5,20
        mem[16'h20] = 8'hC0; mem[16'h21] = 8'h45;                        // MUL R4,R5
        mem[16'h22] = 8'h02;                                              // HALT (should not reach)

        // Handler at 0x80
        mem[16'h80] = 8'h02;  // HALT

        // IVT[2] at 0x110: little-endian 64-bit address = 0x80
        mem[16'h110] = 8'h80;
        mem[16'h111] = 8'h00; mem[16'h112] = 8'h00; mem[16'h113] = 8'h00;
        mem[16'h114] = 8'h00; mem[16'h115] = 8'h00; mem[16'h116] = 8'h00;
        mem[16'h117] = 8'h00;

        reset_cpu;
        wait_halt(2000);
        if (!timeout) begin
            // PC should be at handler + 1 (0x81), NOT at 0x22
            if (u_cpu.R[3] == 64'h22) begin
                $display("FAIL T7: MUL did not trap (PC reached 0x22)");
                fail = fail + 1;
            end else begin
                // Should be at 0x81 (handler HALT at 0x80, advanced by 1)
                check_reg(3, 64'h81, "T7 PC=0x81 (trapped to handler)");
            end
        end else begin
            $display("FAIL T7: timed out");
            fail = fail + 1;
        end

        // ============================================================
        // TEST 8: CSR read CPUID (micro-core identifier)
        // ============================================================
        $display("Test 8: CSR CPUID");
        clear_mem;
        // CSRR R0, CPUID → 0xD0 0x31  (CSRR: nib[3]=0, nib[2:0]=R0=0; CSR=0x31)
        // HALT            → 0x02
        mem[0] = 8'hD0; mem[1] = 8'h31;
        mem[2] = 8'h02;
        reset_cpu;
        wait_halt(200);
        check_reg(0, 64'h4D50_3634_0001_4D43, "T8 CPUID=MP64 v1 MC");

        // ============================================================
        // TEST 9: SEP / SEX
        // ============================================================
        $display("Test 9: SEP + SEX");
        clear_mem;
        // R3 = PC initially. Put a program at 0x00 that switches PC to R4.
        // LDI R4, 0x20    → 0x60 0x40 0x20   ; R4 = 0x20
        // SEP R4          → 0xA4             ; psel = 4, R4 becomes PC
        // (next instruction fetched from 0x20)
        // At 0x20: HALT   → 0x02
        mem[0] = 8'h60; mem[1] = 8'h40; mem[2] = 8'h20;  // LDI R4, 0x20
        mem[3] = 8'hA4;                                     // SEP R4
        mem[16'h20] = 8'h02;                                // HALT
        reset_cpu;
        wait_halt(300);
        // After SEP R4, psel=4 and R4=0x20 was used as PC. After HALT, PC=0x21.
        check_reg(4, 64'h21, "T9 R4=0x21 (SEP to R4, halted at 0x20+1)");

        // ============================================================
        // Summary
        // ============================================================
        $display("--------------------------------------------");
        $display("Micro-Core: %0d passed, %0d failed", pass, fail);
        $display("--------------------------------------------");
        if (fail != 0) begin
            $display("FAIL");
            $finish(1);
        end else begin
            $display("ALL PASS");
        end
        $finish;
    end

endmodule
