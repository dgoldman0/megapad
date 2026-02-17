// ============================================================================
// tb_cpu_smoke.v — Smoke testbench for mp64_cpu (major core)
// ============================================================================
// Provides a simple combinational RAM model and exercises basic instruction
// sequences: NOP, INC, DEC, ALU ops, LDI, branch, SEP/SEX, CSRR CPUID,
// MEM load/store, HALT.
//
// The RAM is 4 KiB, byte-addressable, returns 64-bit values big-endian.
// Instructions are pre-loaded into the RAM at address 0x0000.
//
`timescale 1ns / 1ps

`include "mp64_pkg.vh"

module tb_cpu_smoke;

    `include "mp64_cpu_funcs.vh"

    // ====================================================================
    // Clock + reset
    // ====================================================================
    reg clk, rst;
    initial clk = 0;
    always #5 clk = ~clk;           // 100 MHz

    // ====================================================================
    // Bus model (data bus — simple combinational RAM, 4 KiB)
    // ====================================================================
    reg [7:0] mem [0:4095];

    wire        bus_valid;
    wire [63:0] bus_addr;
    wire [63:0] bus_wdata;
    wire        bus_wen;
    wire [1:0]  bus_size;
    reg  [63:0] bus_rdata;
    reg         bus_ready;

    // 1-cycle response (combinational)
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
            // Little-endian: byte 0 → bits [7:0]
            icache_data <= {mem[ic_base+7], mem[ic_base+6],
                            mem[ic_base+5], mem[ic_base+4],
                            mem[ic_base+3], mem[ic_base+2],
                            mem[ic_base+1], mem[ic_base+0]};
        end
    end

    // ====================================================================
    // CPU instance
    // ====================================================================
    // Tile/MEX stubs
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

        // I-cache interface
        .icache_addr    (icache_addr),
        .icache_req     (icache_req),
        .icache_data    (icache_data),
        .icache_hit     (icache_hit),
        .icache_stall   (1'b0),
        .icache_inv_all (icache_inv_all),
        .icache_inv_line(icache_inv_line),
        .icache_inv_addr(icache_inv_addr),

        // Data bus
        .bus_valid (bus_valid),
        .bus_addr  (bus_addr),
        .bus_wdata (bus_wdata),
        .bus_wen   (bus_wen),
        .bus_size  (bus_size),
        .bus_rdata (bus_rdata),
        .bus_ready (bus_ready),

        // Tile/MEX stubs
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

        // Interrupts
        .irq_timer (1'b0),
        .irq_uart  (1'b0),
        .irq_nic   (1'b0),
        .irq_ipi   (1'b0),

        // I-cache stats
        .icache_stat_hits  (64'd0),
        .icache_stat_misses(64'd0),

        // System config
        .mem_size_bytes(64'h0000_1000),

        // External flags
        .ef_flags  (4'b0000)
    );

    // ====================================================================
    // Helpers
    // ====================================================================
    integer pass_count, fail_count;

    task check_reg;
        input [255:0] label;
        input [3:0]   rn;
        input [63:0]  expected;
        begin
            if (uut.R[rn] !== expected) begin
                $display("FAIL [%0s]: R%0d=%h expected=%h", label, rn, uut.R[rn], expected);
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
                $display("FAIL [%0s]: Z=%b expected=%b", label, uut.flags[0], expected_z);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task wait_state;
        input [3:0] target_state;
        input integer max_cycles;
        integer cyc;
        begin
            for (cyc = 0; cyc < max_cycles; cyc = cyc + 1) begin
                @(posedge clk);
                if (uut.cpu_state == target_state) cyc = max_cycles;
            end
        end
    endtask

    task wait_halt;
        input integer max_cycles;
        begin
            wait_state(CPU_HALT, max_cycles);
        end
    endtask

    task wait_fetch;
        input integer max_cycles;
        begin
            wait_state(CPU_FETCH, max_cycles);
        end
    endtask

    // Run until CPU is in HALT or timeout
    task run_to_halt;
        integer cyc;
        begin
            for (cyc = 0; cyc < 5000; cyc = cyc + 1) begin
                @(posedge clk);
                if (uut.cpu_state == CPU_HALT) cyc = 5000;
            end
        end
    endtask

    integer i;

    // ====================================================================
    // Test program loader
    // ====================================================================
    // Load bytes into mem[] starting at addr.
    task load_byte;
        input [11:0] addr;
        input [7:0]  val;
        begin
            mem[addr] = val;
        end
    endtask

    // ====================================================================
    // Main test
    // ====================================================================
    initial begin
        $dumpfile("tb_cpu_smoke.vcd");
        $dumpvars(0, tb_cpu_smoke);

        pass_count = 0;
        fail_count = 0;

        // Clear memory
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        // -----------------------------------------------------------------
        // Test 1: NOP followed by HALT
        // Program: NOP (0x01), HALT (0x02)
        // After reset, R3=PC=0. NOP increments PC, HALT stops.
        // -----------------------------------------------------------------
        mem[0] = 8'h01;              // NOP
        mem[1] = 8'h02;              // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;
        check_reg("NOP+HALT: PC", 3, 64'd2);

        // -----------------------------------------------------------------
        // Test 2: INC / DEC
        // INC R5, INC R5, INC R5, DEC R5, HALT
        // R5 starts at 0, should end at 2
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0] = 8'h15;              // INC R5
        mem[1] = 8'h15;              // INC R5
        mem[2] = 8'h15;              // INC R5
        mem[3] = 8'h25;              // DEC R5
        mem[4] = 8'h02;              // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;
        check_reg("INC/DEC: R5", 5, 64'd2);
        check_reg("INC/DEC: PC", 3, 64'd5);

        // -----------------------------------------------------------------
        // Test 3: ALU ADD — R4 = R4 + R5
        // LDI R4, 0x0A   →  60 40 0A
        // LDI R5, 0x05   →  60 50 05
        // ADD R4, R5      →  70 45
        // HALT            →  02
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0]  = 8'h60; mem[1]  = 8'h40; mem[2]  = 8'h0A;   // LDI R4, 10
        mem[3]  = 8'h60; mem[4]  = 8'h50; mem[5]  = 8'h05;   // LDI R5, 5
        mem[6]  = 8'h70; mem[7]  = 8'h45;                     // ADD R4, R5
        mem[8]  = 8'h02;                                       // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;
        check_reg("ALU ADD: R4", 4, 64'd15);
        check_reg("ALU ADD: R5", 5, 64'd5);

        // -----------------------------------------------------------------
        // Test 4: SEP / SEX
        // Default: psel=3, xsel=2.
        // SEP 4 → psel=4 (R4 becomes PC).
        // Must pre-load R4 with target address.
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        // R4 must point to the instruction AFTER the SEP
        // Program at 0x0000:
        //   LDI R4, 0x05     → 60 40 05
        //   SEP 4             → A4
        //   HALT              → 02  (skipped — PC is now R4=5)
        // At 0x05:
        //   HALT              → 02

        mem[0]  = 8'h60; mem[1]  = 8'h40; mem[2]  = 8'h05;   // LDI R4, 5
        mem[3]  = 8'hA4;                                       // SEP 4
        mem[4]  = 8'h02;                                       // HALT (dead code)
        mem[5]  = 8'h02;                                       // HALT (reached)

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;
        if (uut.psel !== 4'd4) begin
            $display("FAIL [SEP: psel]: got=%d expected=4", uut.psel);
            fail_count = fail_count + 1;
        end else
            pass_count = pass_count + 1;
        check_reg("SEP: PC=R4", 4, 64'd6);   // HALT at 5, +1 = 6

        // -----------------------------------------------------------------
        // Test 5: MEM Load/Store
        // LDI R4, #0x80       → 60 40 80     (R4 = 128)
        // LDI R5, #0xAB       → 60 50 AB
        // STR R5, [R4]        → 54 54
        // LDR R6, [R4]        → 50 64        (MEM sub 0: LDR Rd,[Rs])
        // HALT                → 02
        // R6 should == 0xAB
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0]  = 8'h60; mem[1]  = 8'h40; mem[2]  = 8'h80;   // LDI R4, 128
        mem[3]  = 8'h60; mem[4]  = 8'h50; mem[5]  = 8'hAB;   // LDI R5, 0xAB
        mem[6]  = 8'h54; mem[7]  = 8'h45;                     // STR.64 [R4],R5
        mem[8]  = 8'h50; mem[9]  = 8'h64;                     // LDR.64 R6,[R4]
        mem[10] = 8'h02;                                       // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;
        check_reg("MEM LDR/STR: R6", 6, 64'hAB);

        // -----------------------------------------------------------------
        // Test 6: CSRR CPUID
        // CSRR R1 ← CPUID (CSR addr 0x10)
        // Encoding: D0 10  (CSRR R0 ← ibuf[1])
        //   Actually nib[3]=0 → CSRR, nib[2:0]=reg
        //   CSRR R1: fam=0xD, nib=1 → D1 10
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0]  = 8'hD1; mem[1]  = 8'h31;                     // CSRR R1, CPUID
        mem[2]  = 8'h02;                                       // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;
        check_reg("CSRR CPUID", 1, 64'h4D50_3634_0001_4350);  // "MP64" v1 "CP"

        // -----------------------------------------------------------------
        // Test 7: Short branch (always)
        // BR ALWAYS, +3 (skip 2 bytes forward from branch end)
        // Encoding: 30 03  (offset=+3)
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        //   00: 30 03         BR.always +3  (PC after BR = 2, +3 = 5)
        //   02: 02            HALT (skipped)
        //   03: 02            HALT (skipped)
        //   04: 02            HALT (skipped)
        //   05: 02            HALT (reached)

        mem[0]  = 8'h30; mem[1]  = 8'h03;                     // BR.always +3
        mem[2]  = 8'h02;
        mem[3]  = 8'h02;
        mem[4]  = 8'h02;
        mem[5]  = 8'h02;                                       // HALT (target)

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;
        check_reg("BR always: PC", 3, 64'd6);  // HALT at 5, PC incremented to 6

        // -----------------------------------------------------------------
        // Test 8: ALU SUB with flags
        // LDI R4, 5 → R4=5
        // LDI R5, 5 → R5=5
        // SUB R4, R5 → R4=0, Z flag set
        // HALT
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0]  = 8'h60; mem[1]  = 8'h40; mem[2]  = 8'h05;   // LDI R4, 5
        mem[3]  = 8'h60; mem[4]  = 8'h50; mem[5]  = 8'h05;   // LDI R5, 5
        mem[6]  = 8'h72; mem[7]  = 8'h45;                     // SUB R4, R5
        mem[8]  = 8'h02;                                       // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;
        check_reg("SUB: R4", 4, 64'd0);
        check_flags_z("SUB: Z flag", 1'b1);

        // =================================================================
        $display("===========================================");
        if (fail_count == 0)
            $display("tb_cpu_smoke: ALL %0d TESTS PASSED", pass_count);
        else
            $display("tb_cpu_smoke: %0d PASSED, %0d FAILED", pass_count, fail_count);
        $display("===========================================");
        $finish;
    end

    // Timeout watchdog
    initial begin
        #500000;
        $display("TIMEOUT: tb_cpu_smoke");
        $finish;
    end

endmodule
