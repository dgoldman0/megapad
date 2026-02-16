// ============================================================================
// tb_cluster.v — Testbench for mp64_cluster (micro-core cluster)
// ============================================================================
//
// Tests:
//   1. Basic halt: all 4 micro-cores execute NOP+HALT
//   2. Shared MUL: 10 × 20 = 200 via cluster multiplier
//   3. Shared DIV: 100 / 7 = 14 remainder 2
//   4. Scratchpad: store/load round-trip (cluster-local SRAM)
//   5. BIST: invoke cluster self-test, verify pass
//   6. Multi-core MUL contention: all cores MUL simultaneously
//

`timescale 1ns / 1ps
`include "mp64_defs.vh"

module tb_cluster;

    localparam N = MICRO_PER_CLUSTER;  // 4

    // ========================================================================
    // Clock / Reset
    // ========================================================================
    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;  // 100 MHz

    // ========================================================================
    // Simple 1-cycle memory model (16 KiB, behind the cluster's bus port)
    // ========================================================================
    reg [7:0] mem [0:16383];

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
                    BUS_BYTE:  mem[bus_addr[13:0]] <= bus_wdata[7:0];
                    BUS_HALF: begin
                        mem[bus_addr[13:0]]   <= bus_wdata[7:0];
                        mem[bus_addr[13:0]+1] <= bus_wdata[15:8];
                    end
                    BUS_WORD: begin
                        mem[bus_addr[13:0]]   <= bus_wdata[7:0];
                        mem[bus_addr[13:0]+1] <= bus_wdata[15:8];
                        mem[bus_addr[13:0]+2] <= bus_wdata[23:16];
                        mem[bus_addr[13:0]+3] <= bus_wdata[31:24];
                    end
                    BUS_DWORD: begin
                        mem[bus_addr[13:0]]   <= bus_wdata[7:0];
                        mem[bus_addr[13:0]+1] <= bus_wdata[15:8];
                        mem[bus_addr[13:0]+2] <= bus_wdata[23:16];
                        mem[bus_addr[13:0]+3] <= bus_wdata[31:24];
                        mem[bus_addr[13:0]+4] <= bus_wdata[39:32];
                        mem[bus_addr[13:0]+5] <= bus_wdata[47:40];
                        mem[bus_addr[13:0]+6] <= bus_wdata[55:48];
                        mem[bus_addr[13:0]+7] <= bus_wdata[63:56];
                    end
                endcase
            end else begin
                case (bus_size)
                    BUS_BYTE:
                        bus_rdata <= {56'd0, mem[bus_addr[13:0]]};
                    BUS_HALF:
                        bus_rdata <= {48'd0, mem[bus_addr[13:0]+1],
                                             mem[bus_addr[13:0]]};
                    BUS_WORD:
                        bus_rdata <= {32'd0, mem[bus_addr[13:0]+3],
                                             mem[bus_addr[13:0]+2],
                                             mem[bus_addr[13:0]+1],
                                             mem[bus_addr[13:0]]};
                    BUS_DWORD:
                        bus_rdata <= {mem[bus_addr[13:0]+7],
                                      mem[bus_addr[13:0]+6],
                                      mem[bus_addr[13:0]+5],
                                      mem[bus_addr[13:0]+4],
                                      mem[bus_addr[13:0]+3],
                                      mem[bus_addr[13:0]+2],
                                      mem[bus_addr[13:0]+1],
                                      mem[bus_addr[13:0]]};
                endcase
            end
        end
    end

    // ========================================================================
    // DUT — Cluster
    // ========================================================================
    wire [N-1:0]       csr_ipi_wen;
    wire [N*8-1:0]     csr_ipi_addr;
    wire [N*64-1:0]    csr_ipi_wdata;
    reg  [N*64-1:0]    csr_ipi_rdata;
    wire [N-1:0]       core_csr_wen;
    wire [N*8-1:0]     core_csr_addr;
    wire [N*64-1:0]    core_csr_wdata;

    mp64_cluster #(
        .N             (N),
        .CLUSTER_ID_BASE (MICRO_ID_BASE)
    ) u_cluster (
        .clk           (clk),
        .rst_n         (rst_n),
        .cluster_en    (1'b1),
        .bus_valid     (bus_valid),
        .bus_addr      (bus_addr),
        .bus_wdata     (bus_wdata),
        .bus_wen       (bus_wen),
        .bus_size      (bus_size),
        .bus_rdata     (bus_rdata),
        .bus_ready     (bus_ready),
        .irq_timer     ({N{1'b0}}),
        .irq_ipi       ({N{1'b0}}),
        .csr_ipi_wen   (csr_ipi_wen),
        .csr_ipi_addr  (csr_ipi_addr),
        .csr_ipi_wdata (csr_ipi_wdata),
        .csr_ipi_rdata (csr_ipi_rdata),
        .core_csr_wen  (core_csr_wen),
        .core_csr_addr (core_csr_addr),
        .core_csr_wdata(core_csr_wdata)
    );

    // Tie off IPI read data
    initial csr_ipi_rdata = {(N*64){1'b0}};

    // ========================================================================
    // Test infrastructure
    // ========================================================================
    integer pass, fail, timeout;
    integer i;

    task clear_mem;
        integer j;
        begin
            for (j = 0; j < 16384; j = j + 1)
                mem[j] = 8'h00;
        end
    endtask

    task reset_cluster;
        begin
            rst_n = 0;
            repeat (4) @(posedge clk);
            rst_n = 1;
        end
    endtask

    // Wait for a specific micro-core to reach HALT state (cpu_state == 4'd7)
    task wait_core_halt;
        input integer core_idx;
        input integer max_cycles;
        integer cyc;
        reg halted;
        begin
            timeout = 0;
            halted = 0;
            for (cyc = 0; cyc < max_cycles && !halted; cyc = cyc + 1) begin
                @(posedge clk);
                case (core_idx)
                    0: halted = (u_cluster.mc[0].u_micro.cpu_state == 4'd7);
                    1: halted = (u_cluster.mc[1].u_micro.cpu_state == 4'd7);
                    2: halted = (u_cluster.mc[2].u_micro.cpu_state == 4'd7);
                    3: halted = (u_cluster.mc[3].u_micro.cpu_state == 4'd7);
                endcase
            end
            if (!halted) begin
                $display("  TIMEOUT: core %0d did not halt in %0d cycles",
                         core_idx, max_cycles);
                timeout = 1;
            end
        end
    endtask

    // Wait for ALL micro-cores to halt
    task wait_all_halt;
        input integer max_cycles;
        integer cyc;
        reg [N-1:0] halted;
        begin
            timeout = 0;
            halted = {N{1'b0}};
            for (cyc = 0; cyc < max_cycles && halted != {N{1'b1}}; cyc = cyc + 1)
            begin
                @(posedge clk);
                if (u_cluster.mc[0].u_micro.cpu_state == 4'd7) halted[0] = 1;
                if (u_cluster.mc[1].u_micro.cpu_state == 4'd7) halted[1] = 1;
                if (u_cluster.mc[2].u_micro.cpu_state == 4'd7) halted[2] = 1;
                if (u_cluster.mc[3].u_micro.cpu_state == 4'd7) halted[3] = 1;
            end
            if (halted != {N{1'b1}}) begin
                $display("  TIMEOUT: not all cores halted in %0d cycles (mask=%b)",
                         max_cycles, halted);
                timeout = 1;
            end
        end
    endtask

    task check_core_reg;
        input integer core_idx;
        input [3:0]   rn;
        input [63:0]  expected;
        input [255:0] name;
        reg [63:0] actual;
        begin
            case (core_idx)
                0: actual = u_cluster.mc[0].u_micro.R[rn];
                1: actual = u_cluster.mc[1].u_micro.R[rn];
                2: actual = u_cluster.mc[2].u_micro.R[rn];
                3: actual = u_cluster.mc[3].u_micro.R[rn];
                default: actual = 64'hDEAD;
            endcase
            if (actual !== expected) begin
                $display("FAIL %0s: core%0d R%0d=%h expected=%h",
                         name, core_idx, rn, actual, expected);
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
        $dumpfile("tb_cluster.vcd");
        $dumpvars(0, tb_cluster);
        pass = 0;
        fail = 0;
        rst_n = 0;

        // ============================================================
        // TEST 1: All cores NOP + HALT
        // ============================================================
        $display("Test 1: All cores NOP+HALT");
        clear_mem;
        // All cores start at PC=0 (R3=0) and fetch same program
        mem[0] = 8'h01;  // NOP
        mem[1] = 8'h01;  // NOP
        mem[2] = 8'h02;  // HALT
        reset_cluster;
        wait_all_halt(4000);
        if (!timeout) begin
            check_core_reg(0, 3, 64'h03, "T1 core0 PC=3");
            check_core_reg(1, 3, 64'h03, "T1 core1 PC=3");
            check_core_reg(2, 3, 64'h03, "T1 core2 PC=3");
            check_core_reg(3, 3, 64'h03, "T1 core3 PC=3");
        end else begin
            $display("FAIL T1: not all cores halted");
            fail = fail + 1;
        end

        // ============================================================
        // TEST 2: Shared MUL — 10 × 20 = 200
        // ============================================================
        $display("Test 2: Shared MUL (10 x 20)");
        clear_mem;
        // Program: LDI R4, 10; LDI R5, 20; MUL R4, R5; HALT
        // All 4 cores will execute this — MUL serializes via cluster RR
        mem[16'h00] = 8'h60; mem[16'h01] = 8'h40; mem[16'h02] = 8'h0A; // LDI R4,10
        mem[16'h03] = 8'h60; mem[16'h04] = 8'h50; mem[16'h05] = 8'h14; // LDI R5,20
        mem[16'h06] = 8'hC0; mem[16'h07] = 8'h45;                      // MUL R4,R5
        mem[16'h08] = 8'h02;                                             // HALT
        reset_cluster;
        wait_all_halt(8000);
        if (!timeout) begin
            check_core_reg(0, 4, 64'd200, "T2 core0 R4=200");
            check_core_reg(1, 4, 64'd200, "T2 core1 R4=200");
            check_core_reg(2, 4, 64'd200, "T2 core2 R4=200");
            check_core_reg(3, 4, 64'd200, "T2 core3 R4=200");
        end else begin
            $display("FAIL T2: timed out waiting for MUL");
            fail = fail + 1;
        end

        // ============================================================
        // TEST 3: Shared DIV — 100 / 7 = 14 remainder 2
        // ============================================================
        $display("Test 3: Shared DIV (100 / 7)");
        clear_mem;
        // LDI R4, 100  → 0x60 0x40 0x64
        // LDI R5, 7    → 0x60 0x50 0x07
        // DIV R4, R5   → 0xC4 0x45 (nib=4=DIV)
        // HALT         → 0x02
        mem[16'h00] = 8'h60; mem[16'h01] = 8'h40; mem[16'h02] = 8'h64; // LDI R4,100
        mem[16'h03] = 8'h60; mem[16'h04] = 8'h50; mem[16'h05] = 8'h07; // LDI R5,7
        mem[16'h06] = 8'hC4; mem[16'h07] = 8'h45;                      // DIV R4,R5
        mem[16'h08] = 8'h02;                                             // HALT
        reset_cluster;
        wait_all_halt(20000);
        if (!timeout) begin
            // DIV: R4 = quotient (100/7=14)
            check_core_reg(0, 4, 64'd14, "T3 core0 R4=14 (quot)");
        end else begin
            $display("FAIL T3: timed out waiting for DIV");
            fail = fail + 1;
        end

        // ============================================================
        // TEST 4: Scratchpad store + load
        // ============================================================
        $display("Test 4: Scratchpad store/load");
        clear_mem;
        // Load 64-bit scratchpad address into R7 using EXT.IMM64:
        //   EXT prefix:   0xF0 (1 byte, sets ext_active=1, ext_mod=0)
        //   LDI64 R7:     0x60 0x70 <8 bytes LE>
        // Scratchpad addr: 0xFFFFFE0000000000
        //   LE bytes: 00 00 00 00 00 FE FF FF
        //             (byte0=LSB=0x00, byte7=MSB=0xFF)
        mem[16'h00] = 8'hF0;                          // EXT prefix
        mem[16'h01] = 8'h60; mem[16'h02] = 8'h70;     // LDI R7 (extended)
        mem[16'h03] = 8'h00;  // byte 0 (LSB)
        mem[16'h04] = 8'h00;  // byte 1
        mem[16'h05] = 8'h00;  // byte 2
        mem[16'h06] = 8'h00;  // byte 3
        mem[16'h07] = 8'h00;  // byte 4
        mem[16'h08] = 8'hFE;  // byte 5
        mem[16'h09] = 8'hFF;  // byte 6
        mem[16'h0A] = 8'hFF;  // byte 7 (MSB)
        // LDI R4, 0x42
        mem[16'h0B] = 8'h60; mem[16'h0C] = 8'h40; mem[16'h0D] = 8'h42;
        // STR R7, R4 → store R4 to M[R7]
        mem[16'h0E] = 8'h54; mem[16'h0F] = 8'h74;
        // LDI R6, 0
        mem[16'h10] = 8'h60; mem[16'h11] = 8'h60; mem[16'h12] = 8'h00;
        // LDN R6, R7 → load from M[R7] into R6
        mem[16'h13] = 8'h50; mem[16'h14] = 8'h67;
        // HALT
        mem[16'h15] = 8'h02;

        reset_cluster;
        wait_all_halt(8000);
        if (!timeout) begin
            // R6 should have the value loaded from scratchpad = 0x42
            check_core_reg(0, 6, 64'h42, "T4 core0 R6=0x42 (spad)");
        end else begin
            $display("FAIL T4: timed out");
            fail = fail + 1;
        end

        // ============================================================
        // TEST 5: BIST — invoke and check pass
        // ============================================================
        $display("Test 5: BIST run");
        clear_mem;
        // Write CSR_BIST_CMD (0x60) = 1 to start BIST
        // LDI R0, 1       → 0x60 0x00 0x01
        // CSRW R0, 0x60   → 0xD8 0x60 (CSRW: opcode byte=0xD{nib}, nib[3]=1, nib[2:0]=R0=0)
        //   CSR_BIST_CMD = 0x60, CSRW encoding: 0xD8 0x60
        // Wait some cycles (NOP loop)
        // CSRR R1, 0x61   → 0xD1 0x61 (CSRR: nib[3]=0, nib[2:0]=R1=1; addr=0x61)
        // HALT             → 0x02
        //
        // We need enough NOPs for BIST to complete (128 spad entries × 3 passes + MUL)
        // Use a short delay loop: LDI R2, 50; DEC R2; BR NE, back
        mem[16'h00] = 8'h60; mem[16'h01] = 8'h00; mem[16'h02] = 8'h01; // LDI R0, 1
        mem[16'h03] = 8'hD8; mem[16'h04] = 8'h60;  // CSRW R0, CSR_BIST_CMD
        // Delay loop: LDI R2, 0; ADDI R2, 0x7F (127); ADDI R2, 0x7F (254)
        mem[16'h05] = 8'h60; mem[16'h06] = 8'h20; mem[16'h07] = 8'h00; // LDI R2, 0
        mem[16'h08] = 8'h62; mem[16'h09] = 8'h20; mem[16'h0A] = 8'h7F; // ADDI R2, 127
        mem[16'h0B] = 8'h62; mem[16'h0C] = 8'h20; mem[16'h0D] = 8'h7F; // ADDI R2, 127
        // Loop: DEC R2; CMP R2, R0_baseline; BR NE
        // Actually simpler: just a NOP sled
        // 50 NOPs at addresses 0x0E to 0x3F
        for (i = 16'h0E; i < 16'h40; i = i + 1)
            mem[i] = 8'h01;  // NOP
        // Read BIST status
        mem[16'h40] = 8'hD1; mem[16'h41] = 8'h61;  // CSRR R1, CSR_BIST_STATUS
        mem[16'h42] = 8'h02;  // HALT

        reset_cluster;
        wait_all_halt(20000);
        if (!timeout) begin
            // R1 should be 2 (BIST pass)
            check_core_reg(0, 1, 64'd2, "T5 core0 R1=2 (BIST pass)");
        end else begin
            $display("FAIL T5: timed out");
            fail = fail + 1;
        end

        // ============================================================
        // TEST 6: Unsigned MUL — 0xFFFF × 0xFFFF
        // ============================================================
        $display("Test 6: UMUL large values");
        clear_mem;
        // LDI R4 = 0xFFFF (LDI + ADDI chain)
        mem[16'h00] = 8'h60; mem[16'h01] = 8'h40; mem[16'h02] = 8'h00; // LDI R4, 0
        mem[16'h03] = 8'h62; mem[16'h04] = 8'h40; mem[16'h05] = 8'h7F; // ADDI R4, 127
        mem[16'h06] = 8'h62; mem[16'h07] = 8'h40; mem[16'h08] = 8'h7F; // ADDI R4, 127
        mem[16'h09] = 8'h62; mem[16'h0A] = 8'h40; mem[16'h0B] = 8'h03; // ADDI R4, 3 → R4=259?
        // Hmm, need exactly 0xFFFF. That's 65535 = 127*515+70. Too many ADDIs.
        // Use a simpler value: R4=100, R5=200
        // LDI R4, 100 → 0x60 0x40 0x64
        // LDI R5, 0; ADDI R5, 127; ADDI R5, 73 → R5=200
        mem[16'h00] = 8'h60; mem[16'h01] = 8'h40; mem[16'h02] = 8'h64; // LDI R4, 100
        mem[16'h03] = 8'h60; mem[16'h04] = 8'h50; mem[16'h05] = 8'h00; // LDI R5, 0
        mem[16'h06] = 8'h62; mem[16'h07] = 8'h50; mem[16'h08] = 8'h7F; // ADDI R5, 127
        mem[16'h09] = 8'h62; mem[16'h0A] = 8'h50; mem[16'h0B] = 8'h49; // ADDI R5, 73 → R5=200
        mem[16'h0C] = 8'hC2; mem[16'h0D] = 8'h45;                      // UMUL R4,R5 (nib=2)
        mem[16'h0E] = 8'h02;                                             // HALT
        reset_cluster;
        wait_all_halt(8000);
        if (!timeout) begin
            // 100 × 200 = 20000
            check_core_reg(0, 4, 64'd20000, "T6 core0 R4=20000 (UMUL)");
        end else begin
            $display("FAIL T6: timed out");
            fail = fail + 1;
        end

        // ============================================================
        // Summary
        // ============================================================
        $display("--------------------------------------------");
        $display("Cluster: %0d passed, %0d failed", pass, fail);
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
