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
    wire        icache_enabled, icache_inv_all, icache_inv_line;
    wire [63:0] icache_inv_addr;
    wire [6:0]  icache_inv_size;

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
    wire [7:0]  mex_funct_byte_w;
    wire [63:0] mex_gpr_val_w;
    wire [7:0]  mex_imm8_w;
    wire [3:0]  mex_ext_mod_w;
    wire        mex_ext_active_w;
    reg         mex_done_r;
    reg         mex_busy_r;
    reg  [2:0]  mex_fault_r;
    reg  [63:0] mex_fault_addr_r;
    reg         mex_stall_cycle_r;
    reg  [2:0]  next_mex_fault;
    reg  [63:0] next_mex_fault_addr;
    reg         mex_ack_enable;
    wire [TACC_CALLER_BITS-1:0] tile_caller_id_w;
    wire        tile_priv_w;
    wire [63:0] tile_mpu_base_w;
    wire [63:0] tile_mpu_limit_w;
    wire        tile_mpu_enabled_w;
    wire        tile_allow_cluster_spad_w;
    reg  [63:0] tacc_status_r;
    wire        tacc_ctl_valid_w;
    wire [63:0] tacc_ctl_wdata_w;
    reg         tacc_ctl_done_r;
    reg  [2:0]  tacc_ctl_fault_r;
    reg  [2:0]  next_tacc_ctl_fault;
    reg         tacc_ctl_ack_enable;

    integer mex_dispatch_count;
    integer tacc_ctl_dispatch_count;
    integer legacy_csr_write_count;
    reg [1:0] captured_mex_ss;
    reg [1:0] captured_mex_op;
    reg [2:0] captured_mex_funct;
    reg [7:0] captured_mex_funct_byte;
    reg [3:0] captured_mex_ext_mod;
    reg       captured_mex_ext_active;

    // One-cycle completion stubs.  A disabled acknowledgement deliberately
    // leaves the CPU in its wait state so held-valid behavior can be checked.
    always @(negedge clk) begin
        mex_done_r <= 1'b0;
        tacc_ctl_done_r <= 1'b0;

        if (mex_valid_w) begin
            mex_dispatch_count = mex_dispatch_count + 1;
            captured_mex_ss = mex_ss_w;
            captured_mex_op = mex_op_w;
            captured_mex_funct = mex_funct_w;
            captured_mex_funct_byte = mex_funct_byte_w;
            captured_mex_ext_mod = mex_ext_mod_w;
            captured_mex_ext_active = mex_ext_active_w;
            if (mex_ack_enable) begin
                mex_fault_r <= next_mex_fault;
                mex_fault_addr_r <= next_mex_fault_addr;
                mex_done_r <= 1'b1;
            end
        end

        if (tacc_ctl_valid_w && tacc_ctl_ack_enable) begin
            tacc_ctl_dispatch_count = tacc_ctl_dispatch_count + 1;
            tacc_ctl_fault_r <= next_tacc_ctl_fault;
            tacc_ctl_done_r <= 1'b1;
        end

        if (csr_wen_w)
            legacy_csr_write_count = legacy_csr_write_count + 1;
    end

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
        .icache_enabled (icache_enabled),
        .icache_inv_all (icache_inv_all),
        .icache_inv_line(icache_inv_line),
        .icache_inv_addr(icache_inv_addr),
        .icache_inv_size(icache_inv_size),

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
        .mex_funct_byte(mex_funct_byte_w),
        .mex_gpr_val(mex_gpr_val_w),
        .mex_imm8  (mex_imm8_w),
        .mex_ext_mod(mex_ext_mod_w),
        .mex_ext_active(mex_ext_active_w),
        .mex_done  (mex_done_r),
        .mex_busy  (mex_busy_r),
        .mex_fault (mex_fault_r),
        .mex_fault_addr(mex_fault_addr_r),
        .mex_stall_cycle(mex_stall_cycle_r),
        .tile_caller_id(tile_caller_id_w),
        .tile_priv (tile_priv_w),
        .tile_mpu_base(tile_mpu_base_w),
        .tile_mpu_limit(tile_mpu_limit_w),
        .tile_mpu_enabled(tile_mpu_enabled_w),
        .tile_allow_cluster_spad(tile_allow_cluster_spad_w),
        .tacc_status(tacc_status_r),
        .tacc_ctl_valid(tacc_ctl_valid_w),
        .tacc_ctl_wdata(tacc_ctl_wdata_w),
        .tacc_ctl_done(tacc_ctl_done_r),
        .tacc_ctl_fault(tacc_ctl_fault_r),

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

    task check_mem_qword_be;
        input [255:0] label;
        input [11:0]  addr;
        input [63:0]  expected;
        reg [63:0] actual;
        begin
            actual = {mem[addr],   mem[addr+1], mem[addr+2], mem[addr+3],
                      mem[addr+4], mem[addr+5], mem[addr+6], mem[addr+7]};
            if (actual !== expected) begin
                $display("FAIL [%0s]: mem[%h]=%h expected=%h",
                         label, addr, actual, expected);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
    end
    endtask

    task check64;
        input [255:0] label;
        input [63:0] got;
        input [63:0] expected;
        begin
            if (got !== expected) begin
                $display("FAIL [%0s]: got=%h expected=%h",
                         label, got, expected);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task clear_mem;
        integer clear_i;
        begin
            for (clear_i = 0; clear_i < 4096; clear_i = clear_i + 1)
                mem[clear_i] = 8'h00;
        end
    endtask

    task reset_cpu;
        begin
            rst = 1'b1;
            repeat (4) @(posedge clk);
            rst = 1'b0;
        end
    endtask

    task install_vector;
        input [7:0] vector;
        input [7:0] target;
        integer vector_base;
        begin
            vector_base = vector * 8;
            mem[vector_base+0] = 8'd0;
            mem[vector_base+1] = 8'd0;
            mem[vector_base+2] = 8'd0;
            mem[vector_base+3] = 8'd0;
            mem[vector_base+4] = 8'd0;
            mem[vector_base+5] = 8'd0;
            mem[vector_base+6] = 8'd0;
            mem[vector_base+7] = target;
        end
    endtask

    task run_mex_fault_case;
        input [2:0] fault;
        input [7:0] expected_vector;
        input [63:0] fault_addr;
        input [63:0] expected_trap_addr;
        begin
            clear_mem;
            mem[0] = 8'h60; mem[1] = 8'hF0; mem[2] = 8'h80;
            mem[3] = 8'hE0; mem[4] = 8'h00;
            mem[5] = 8'h02;
            install_vector(expected_vector, 8'h40);
            mem[8'h40] = 8'h02;

            next_mex_fault = fault;
            next_mex_fault_addr = fault_addr;
            mex_dispatch_count = 0;
            reset_cpu;
            @(negedge clk);
            uut.trap_addr = 64'hCAFE_BABE_DEAD_BEEF;
            run_to_halt;

            check64("MEX fault emitted one request",
                    mex_dispatch_count, 64'd1);
            check64("MEX fault vector",
                    uut.ivec_id, expected_vector);
            check64("MEX fault TRAP_ADDR",
                    uut.trap_addr, expected_trap_addr);
            check_mem_qword_be("MEX fault saved end PC",
                               12'h070, 64'd5);
            check64("faulting MEX does not retire",
                    uut.perf_tileops, 64'd0);
        end
    endtask

    task run_illegal_mex_case;
        input [7:0] raw0;
        input [7:0] raw1;
        input [7:0] raw2;
        input [7:0] raw3;
        input [2:0] raw_len;
        begin
            clear_mem;
            mem[0] = 8'h60; mem[1] = 8'hF0; mem[2] = 8'h80;
            mem[3] = raw0;
            mem[4] = raw1;
            mem[5] = raw2;
            mem[6] = raw3;
            mem[3+raw_len] = 8'h02;
            install_vector(IRQX_ILLEGAL_OP, 8'h40);
            mem[8'h40] = 8'h02;

            next_mex_fault = MEX_FAULT_NONE;
            next_mex_fault_addr = 64'd0;
            mex_dispatch_count = 0;
            reset_cpu;
            @(negedge clk);
            uut.trap_addr = 64'h0123_4567_89AB_CDEF;
            run_to_halt;

            check64("malformed encoding emits no MEX request",
                    mex_dispatch_count, 64'd0);
            check64("malformed encoding vector",
                    uut.ivec_id, IRQX_ILLEGAL_OP);
            check_mem_qword_be("malformed encoding saved complete end PC",
                               12'h070, 64'd3 + raw_len);
            check64("illegal encoding preserves TRAP_ADDR",
                    uut.trap_addr, 64'h0123_4567_89AB_CDEF);
        end
    endtask

    task wait_state;
        input [4:0] target_state;
        input integer max_cycles;
        integer cyc;
        reg reached;
        begin
            reached = 1'b0;
            for (cyc = 0; cyc < max_cycles; cyc = cyc + 1) begin
                @(posedge clk);
                if (uut.cpu_state == target_state) begin
                    reached = 1'b1;
                    cyc = max_cycles;
                end
            end
            if (!reached)
                $fatal(1, "CPU did not reach state %0d within %0d cycles",
                       target_state, max_cycles);
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
        begin
            wait_state(CPU_HALT, 5000);
        end
    endtask

    integer i;
    integer saw_illegal;

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
        mex_done_r = 1'b0;
        mex_busy_r = 1'b0;
        mex_fault_r = MEX_FAULT_NONE;
        mex_fault_addr_r = 64'd0;
        mex_stall_cycle_r = 1'b0;
        next_mex_fault = MEX_FAULT_NONE;
        next_mex_fault_addr = 64'd0;
        mex_ack_enable = 1'b1;
        tacc_status_r = {43'd0, TACC_OWNER_NONE, 16'd0};
        tacc_ctl_done_r = 1'b0;
        tacc_ctl_fault_r = MEX_FAULT_NONE;
        next_tacc_ctl_fault = MEX_FAULT_NONE;
        tacc_ctl_ack_enable = 1'b1;
        mex_dispatch_count = 0;
        tacc_ctl_dispatch_count = 0;
        legacy_csr_write_count = 0;
        captured_mex_ss = 2'd0;
        captured_mex_op = 2'd0;
        captured_mex_funct = 3'd0;
        captured_mex_funct_byte = 8'd0;
        captured_mex_ext_mod = 4'd0;
        captured_mex_ext_active = 1'b0;

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

        // -----------------------------------------------------------------
        // Test 9: bare EXT.CRYPTO length on the full core.
        // CRC.INIT is exactly two bytes; the following INC must not be
        // consumed as a phantom register/immediate byte.
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0] = 8'hFB; mem[1] = 8'h00;  // CRC.INIT (bare, 2 bytes)
        mem[2] = 8'h15;                   // INC R5
        mem[3] = 8'h02;                   // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;
        check_reg("bare CRC.INIT preserves next opcode", 5, 64'd1);

        // -----------------------------------------------------------------
        // Test 10: CRC_MODE CSR validates the complete 64-bit value.
        // Values 5 and 0xFF must not alias modes 1 and 3 after truncation.
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0]  = 8'h60; mem[1]  = 8'h20; mem[2]  = 8'h05; // LDI R2,5
        mem[3]  = 8'hDA; mem[4]  = 8'h81;                  // CSRW CRC_MODE,R2
        mem[5]  = 8'hD5; mem[6]  = 8'h81;                  // CSRR R5,CRC_MODE
        mem[7]  = 8'h60; mem[8]  = 8'h20; mem[9]  = 8'hFF; // LDI R2,FF
        mem[10] = 8'hDA; mem[11] = 8'h81;                  // CSRW CRC_MODE,R2
        mem[12] = 8'hD6; mem[13] = 8'h81;                  // CSRR R6,CRC_MODE
        mem[14] = 8'h02;                                    // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;
        check_reg("CRC_MODE CSR rejects 5", 5, 64'd0);
        check_reg("CRC_MODE CSR rejects FF", 6, 64'd0);

        // -----------------------------------------------------------------
        // Test 11: reserved CRC sub-op 6 is a two-byte illegal instruction.
        // The following INC is neither fetched as an operand nor executed.
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0] = 8'hFB; mem[1] = 8'h06;  // reserved CRC op (2 bytes)
        mem[2] = 8'h15;                   // INC R5 (must not execute)

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        saw_illegal = 0;
        for (i = 0; i < 200; i = i + 1) begin
            @(posedge clk);
            if (uut.ivec_id == IRQX_ILLEGAL_OP) begin
                saw_illegal = 1;
                i = 200;
            end
        end
        if (!saw_illegal) begin
            $display("FAIL [reserved CRC op did not trap]");
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end
        check_reg("reserved CRC op length", 3, 64'd2);
        check_reg("reserved CRC op preserves next opcode", 5, 64'd0);

        // -----------------------------------------------------------------
        // Test 12: TRAP + RTI uses the documented FLAGS/PC stack order.
        // The saved FLAGS word includes IE and sits below the return PC.
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0] = 8'h60; mem[1] = 8'hF0; mem[2] = 8'h80; // LDI R15,0x80
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h01; // LDI R2,1
        mem[6] = 8'hDA; mem[7] = 8'h0A;                  // CSRW PRIV,R2
        mem[8] = 8'h0F;                                  // TRAP
        mem[9] = 8'h02;                                  // HALT after RTI
        mem[16'h30] = 8'h00; mem[16'h31] = 8'h00;
        mem[16'h32] = 8'h00; mem[16'h33] = 8'h00;
        mem[16'h34] = 8'h00; mem[16'h35] = 8'h00;
        mem[16'h36] = 8'h00; mem[16'h37] = 8'h40;          // IVT[6] = 0x40
        mem[16'h40] = 8'h04;                               // RTI

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;
        if (uut.cpu_state !== CPU_HALT) begin
            $display("FAIL [TRAP+RTI did not return to HALT]");
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end
        check_reg("TRAP+RTI: PC resumes", 3, 64'h0A);
        check_reg("TRAP+RTI: SP restored", 15, 64'h80);
        if (uut.flags !== 8'h40) begin
            $display("FAIL [TRAP+RTI: FLAGS]: got=%h expected=40", uut.flags);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end
        if (uut.priv_level !== 1'b1) begin
            $display("FAIL [TRAP+RTI: PRIV]: got=%b expected=1",
                     uut.priv_level);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end
        check_mem_qword_be("TRAP frame return PC", 12'h070, 64'h09);
        check_mem_qword_be("TRAP frame saved FLAGS+PRIV", 12'h078, 64'h140);

        // -----------------------------------------------------------------
        // Test 13: division by zero uses vector 4, not ILLEGAL_OP vector 2.
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0] = 8'h60; mem[1] = 8'hF0; mem[2] = 8'h80; // LDI R15,0x80
        mem[3] = 8'h60; mem[4] = 8'h40; mem[5] = 8'h0A; // LDI R4,10
        mem[6] = 8'hC5; mem[7] = 8'h45;                 // UDIV R4,R5; R5=0
        mem[8] = 8'h02;                                  // must not execute
        mem[16'h20] = 8'h00; mem[16'h21] = 8'h00;
        mem[16'h22] = 8'h00; mem[16'h23] = 8'h00;
        mem[16'h24] = 8'h00; mem[16'h25] = 8'h00;
        mem[16'h26] = 8'h00; mem[16'h27] = 8'h40;        // IVT[4] = 0x40
        mem[16'h40] = 8'h02;                              // DIV_ZERO handler

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        run_to_halt;
        if (uut.cpu_state !== CPU_HALT) begin
            $display("FAIL [DIV_ZERO did not reach handler]");
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end
        check_reg("DIV_ZERO: handler PC", 3, 64'h41);
        if (uut.ivec_id !== IRQX_DIV_ZERO) begin
            $display("FAIL [DIV_ZERO IVEC]: got=%0d expected=%0d",
                     uut.ivec_id, IRQX_DIV_ZERO);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        // -----------------------------------------------------------------
        // Test 14: canonical TACC encodings preserve the complete request.
        // Landing 2.1 stubs successful completion here so this bench tests
        // CPU decode/transport independently of later tile execution.
        // -----------------------------------------------------------------
        clear_mem;
        mem[0] = 8'hE1; mem[1] = 8'h06; // TAMAC tile x tile
        mem[2] = 8'h02;
        next_mex_fault = MEX_FAULT_NONE;
        next_mex_fault_addr = 64'd0;
        mex_dispatch_count = 0;
        reset_cpu;
        run_to_halt;
        check64("canonical TAMAC request count",
                mex_dispatch_count, 64'd1);
        check64("canonical TAMAC selector", captured_mex_ss, 64'd0);
        check64("canonical TAMAC operation", captured_mex_op, MEX_TMUL);
        check64("canonical TAMAC function", captured_mex_funct, TMUL_TAMAC);
        check64("canonical TAMAC raw byte",
                captured_mex_funct_byte, 64'h06);
        check64("full-core caller ID", tile_caller_id_w, 64'd0);
        check64("full core cannot access cluster scratchpad",
                tile_allow_cluster_spad_w, 64'd0);

        clear_mem;
        mem[0] = 8'hF8; mem[1] = 8'hE3; mem[2] = 8'h02;
        mem[3] = 8'h02;
        mex_dispatch_count = 0;
        reset_cpu;
        run_to_halt;
        check64("canonical lifecycle request count",
                mex_dispatch_count, 64'd1);
        check64("canonical lifecycle operation",
                captured_mex_op, MEX_TSYS);
        check64("canonical lifecycle raw byte",
                captured_mex_funct_byte, 64'h02);
        check64("canonical lifecycle EXT modifier",
                captured_mex_ext_mod, 64'd8);
        check64("canonical lifecycle EXT active",
                captured_mex_ext_active, 64'd1);

        // -----------------------------------------------------------------
        // Test 15: malformed/reserved TACC encodings trap after complete
        // decode and never reach the tile request port.
        // -----------------------------------------------------------------
        run_illegal_mex_case(8'hE9, 8'h06, 8'h00, 8'h00, 3'd2);
        run_illegal_mex_case(8'hE1, 8'h26, 8'h00, 8'h00, 3'd2);
        run_illegal_mex_case(8'hE1, 8'h07, 8'h00, 8'h00, 3'd2);
        run_illegal_mex_case(8'hF8, 8'hE7, 8'h02, 8'h00, 3'd4);
        run_illegal_mex_case(8'hF8, 8'hE3, 8'h22, 8'h00, 3'd3);
        run_illegal_mex_case(8'hF8, 8'hE3, 8'h07, 8'h00, 3'd3);

        // -----------------------------------------------------------------
        // Test 16: precise MEX completion faults.
        // -----------------------------------------------------------------
        run_mex_fault_case(MEX_FAULT_ILLEGAL, IRQX_ILLEGAL_OP,
                           64'h1111, 64'hCAFE_BABE_DEAD_BEEF);
        run_mex_fault_case(MEX_FAULT_ALIGN, IRQX_ALIGN,
                           64'h2222, 64'h2222);
        run_mex_fault_case(MEX_FAULT_BUS, IRQX_BUS,
                           64'h3333, 64'h3333);
        run_mex_fault_case(MEX_FAULT_PRIV, IRQX_PRIV,
                           64'h4444, 64'h4444);

        // -----------------------------------------------------------------
        // Test 17: TACC status/control bypass the legacy tile CSR path.
        // -----------------------------------------------------------------
        clear_mem;
        tacc_status_r = 64'h0123_4567_89AB_CDEF;
        mem[0] = 8'hD1; mem[1] = CSR_TACC_STATUS;
        mem[2] = 8'hD2; mem[3] = CSR_TACC_CTL;
        mem[4] = 8'h02;
        reset_cpu;
        run_to_halt;
        check_reg("TACC_STATUS returns dedicated status", 1,
                  64'h0123_4567_89AB_CDEF);
        check_reg("TACC_CTL reads zero", 2, 64'd0);

        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'hAA;
        mem[3] = 8'hD9; mem[4] = CSR_TACC_STATUS;
        mem[5] = 8'h02;
        legacy_csr_write_count = 0;
        tacc_ctl_dispatch_count = 0;
        reset_cpu;
        run_to_halt;
        check64("TACC_STATUS write does not reach legacy CSR",
                legacy_csr_write_count, 64'd0);
        check64("TACC_STATUS write does not reach control",
                tacc_ctl_dispatch_count, 64'd0);

        // The CPU must hold control valid and data until an acknowledgement.
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h01;
        mem[3] = 8'hD9; mem[4] = CSR_TACC_CTL;
        mem[5] = 8'h02;
        tacc_ctl_ack_enable = 1'b0;
        next_tacc_ctl_fault = MEX_FAULT_NONE;
        tacc_ctl_dispatch_count = 0;
        reset_cpu;
        wait_state(CPU_CSR_WAIT, 200);
        check64("TACC_CTL enters acknowledged wait",
                uut.cpu_state, CPU_CSR_WAIT);
        repeat (3) begin
            @(negedge clk);
            check64("TACC_CTL valid remains held",
                    tacc_ctl_valid_w, 64'd1);
            check64("TACC_CTL data remains held",
                    tacc_ctl_wdata_w, 64'd1);
        end
        tacc_ctl_ack_enable = 1'b1;
        run_to_halt;
        check64("TACC_CTL publishes one acknowledged transaction",
                tacc_ctl_dispatch_count, 64'd1);

        // User FORCE_RELEASE traps locally with no sideband transaction.
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h01;
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h01;
        mem[6] = 8'hDA; mem[7] = CSR_PRIV;
        mem[8] = 8'hD9; mem[9] = CSR_TACC_CTL;
        mem[10] = 8'h02;
        install_vector(IRQX_PRIV, 8'h40);
        mem[8'h40] = 8'h02;
        tacc_ctl_dispatch_count = 0;
        reset_cpu;
        @(negedge clk);
        uut.R[15] = 64'h200;
        run_to_halt;
        check64("user FORCE_RELEASE emits no control request",
                tacc_ctl_dispatch_count, 64'd0);
        check64("user FORCE_RELEASE vector", uut.ivec_id, IRQX_PRIV);
        check_mem_qword_be("user FORCE_RELEASE saved end PC",
                           12'h1F0, 64'd10);

        // Reserved control bits are ignored even in user mode.
        clear_mem;
        mem[0] = 8'h60; mem[1] = 8'h10; mem[2] = 8'h02;
        mem[3] = 8'h60; mem[4] = 8'h20; mem[5] = 8'h01;
        mem[6] = 8'hDA; mem[7] = CSR_PRIV;
        mem[8] = 8'hD9; mem[9] = CSR_TACC_CTL;
        mem[10] = 8'h02;
        tacc_ctl_dispatch_count = 0;
        reset_cpu;
        run_to_halt;
        check64("user reserved-only control write is acknowledged",
                tacc_ctl_dispatch_count, 64'd1);
        check64("user reserved-only control write retires",
                uut.cpu_state, CPU_HALT);

        // =================================================================
        $display("===========================================");
        if (fail_count == 0)
            $display("tb_cpu_smoke: ALL %0d TESTS PASSED", pass_count);
        else
            $display("tb_cpu_smoke: %0d PASSED, %0d FAILED", pass_count, fail_count);
        $display("===========================================");
        if (fail_count != 0)
            $fatal(1, "tb_cpu_smoke failed");
        $finish(0);
    end

    // Timeout watchdog
    initial begin
        #500000;
        $display("TIMEOUT: tb_cpu_smoke");
        $fatal(1, "tb_cpu_smoke timeout");
    end

endmodule
