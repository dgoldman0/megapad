// ============================================================================
// tb_cluster.v — Testbench for mp64_cluster (micro-core cluster)
// ============================================================================
// Exercises basic instruction execution through the cluster wrapper:
//   1. NOP + HALT on micro-core 0
//   2. INC on micro-core 0
//   3. Scratchpad write/read via magic address
//   4. MUL via shared multiplier
//   5. Verify all 4 micro-cores reach HALT
//
`timescale 1ns / 1ps

`include "mp64_pkg.vh"

module tb_cluster;

    `include "mp64_cpu_funcs.vh"

    // ====================================================================
    // Clock + reset
    // ====================================================================
    reg clk, rst;
    initial clk = 0;
    always #5 clk = ~clk;

    // ====================================================================
    // Bus model (4 KiB combinational RAM, 1-cycle latency)
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
    // Cluster instance
    // ====================================================================
    localparam N = 4;

    mp64_cluster #(
        .N              (N),
        .CLUSTER_ID_BASE(8'd4)
    ) uut (
        .clk        (clk),
        .rst        (rst),
        .cluster_en (1'b1),

        .bus_valid  (bus_valid),
        .bus_addr   (bus_addr),
        .bus_wdata  (bus_wdata),
        .bus_wen    (bus_wen),
        .bus_size   (bus_size),
        .bus_rdata  (bus_rdata),
        .bus_ready  (bus_ready),

        .irq_timer  ({N{1'b0}}),
        .irq_ipi    ({N{1'b0}}),
        .ef_flags   (4'b0000)
    );

    // ====================================================================
    // Helpers
    // ====================================================================
    integer pass_count, fail_count;

    task check64;
        input [255:0] label;
        input [63:0]  got;
        input [63:0]  expected;
        begin
            if (got !== expected) begin
                $display("FAIL [%0s]: got=%h expected=%h", label, got, expected);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task check_mc0_state;
        input [255:0] label;
        input [3:0]   expected_state;
        begin
            if (uut.mc[0].u_micro.cpu_state !== expected_state) begin
                $display("FAIL [%0s]: mc0 state=%d expected=%d",
                    label, uut.mc[0].u_micro.cpu_state, expected_state);
                fail_count = fail_count + 1;
            end else
                pass_count = pass_count + 1;
        end
    endtask

    task check_mc1_state;
        input [255:0] label;
        input [3:0]   expected_state;
        begin
            if (uut.mc[1].u_micro.cpu_state !== expected_state) begin
                $display("FAIL [%0s]: mc1 state=%d expected=%d",
                    label, uut.mc[1].u_micro.cpu_state, expected_state);
                fail_count = fail_count + 1;
            end else
                pass_count = pass_count + 1;
        end
    endtask

    task check_mc2_state;
        input [255:0] label;
        input [3:0]   expected_state;
        begin
            if (uut.mc[2].u_micro.cpu_state !== expected_state) begin
                $display("FAIL [%0s]: mc2 state=%d expected=%d",
                    label, uut.mc[2].u_micro.cpu_state, expected_state);
                fail_count = fail_count + 1;
            end else
                pass_count = pass_count + 1;
        end
    endtask

    task check_mc3_state;
        input [255:0] label;
        input [3:0]   expected_state;
        begin
            if (uut.mc[3].u_micro.cpu_state !== expected_state) begin
                $display("FAIL [%0s]: mc3 state=%d expected=%d",
                    label, uut.mc[3].u_micro.cpu_state, expected_state);
                fail_count = fail_count + 1;
            end else
                pass_count = pass_count + 1;
        end
    endtask

    // Wait until all N micro-cores are in HALT (or timeout)
    task wait_all_halt;
        input integer max_cycles;
        integer cyc;
        reg all_halted;
        begin
            for (cyc = 0; cyc < max_cycles; cyc = cyc + 1) begin
                @(posedge clk);
                all_halted =
                    (uut.mc[0].u_micro.cpu_state == CPU_HALT) &&
                    (uut.mc[1].u_micro.cpu_state == CPU_HALT) &&
                    (uut.mc[2].u_micro.cpu_state == CPU_HALT) &&
                    (uut.mc[3].u_micro.cpu_state == CPU_HALT);
                if (all_halted) cyc = max_cycles;
            end
        end
    endtask

    task wait_mc0_halt;
        input integer max_cycles;
        integer cyc;
        begin
            for (cyc = 0; cyc < max_cycles; cyc = cyc + 1) begin
                @(posedge clk);
                if (uut.mc[0].u_micro.cpu_state == CPU_HALT) cyc = max_cycles;
            end
        end
    endtask

    integer i;

    // ====================================================================
    // Main tests
    // ====================================================================
    initial begin
        $dumpfile("tb_cluster.vcd");
        $dumpvars(0, tb_cluster);

        pass_count = 0;
        fail_count = 0;

        // Clear memory
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        // -----------------------------------------------------------------
        // Test 1: All micro-cores start at PC (R3) = 0.
        // All execute: HALT (0x02) at address 0.
        // -----------------------------------------------------------------
        mem[0] = 8'h02;              // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        wait_all_halt(2000);
        check_mc0_state("all-halt: mc0", CPU_HALT);
        check_mc1_state("all-halt: mc1", CPU_HALT);
        check_mc2_state("all-halt: mc2", CPU_HALT);
        check_mc3_state("all-halt: mc3", CPU_HALT);

        // -----------------------------------------------------------------
        // Test 2: INC on micro-core 0
        // Since all 4 share the same code, all 4 will execute the same
        // program.  Verify mc0's R5.
        // Program: INC R5, INC R5, HALT
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0] = 8'h15;              // INC R5
        mem[1] = 8'h15;              // INC R5
        mem[2] = 8'h02;              // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        wait_all_halt(5000);
        check64("INC: mc0 R5", uut.mc[0].u_micro.R[5], 64'd2);
        check64("INC: mc1 R5", uut.mc[1].u_micro.R[5], 64'd2);

        // -----------------------------------------------------------------
        // Test 3: ALU ADD through cluster arbiter
        // LDI R4, 7  → 60 40 07
        // LDI R5, 3  → 60 50 03
        // ADD R4,R5  → 70 45
        // HALT       → 02
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0]  = 8'h60; mem[1]  = 8'h40; mem[2]  = 8'h07;
        mem[3]  = 8'h60; mem[4]  = 8'h50; mem[5]  = 8'h03;
        mem[6]  = 8'h70; mem[7]  = 8'h45;
        mem[8]  = 8'h02;

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        wait_all_halt(10000);
        check64("ALU ADD: mc0 R4", uut.mc[0].u_micro.R[4], 64'd10);

        // -----------------------------------------------------------------
        // Test 4: MULDIV through shared multiplier
        // LDI R4, 7    → 60 40 07
        // LDI R5, 6    → 60 50 06
        // MUL R4, R5   → C0 45
        // HALT         → 02
        // R4 should = 42
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0]  = 8'h60; mem[1]  = 8'h40; mem[2]  = 8'h07;   // LDI R4, 7
        mem[3]  = 8'h60; mem[4]  = 8'h50; mem[5]  = 8'h06;   // LDI R5, 6
        mem[6]  = 8'hC0; mem[7]  = 8'h45;                     // MUL R4, R5
        mem[8]  = 8'h02;                                       // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        wait_all_halt(10000);
        check64("MUL: mc0 R4", uut.mc[0].u_micro.R[4], 64'd42);

        // -----------------------------------------------------------------
        // Test 5: CSRR CPUID on micro-core
        // CSRR R1, CSR_CPUID → D1 10
        // HALT → 02
        //  Expect "MP64" v1 "MC"  = 0x4D50_3634_0001_4D43
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0]  = 8'hD1; mem[1]  = 8'h31;                     // CSRR R1, CPUID
        mem[2]  = 8'h02;                                       // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        wait_all_halt(5000);
        check64("CSRR CPUID mc0", uut.mc[0].u_micro.R[1],
                64'h4D50_3634_0001_4D43);

        // -----------------------------------------------------------------
        // Test 6: CSRR COREID (verify unique IDs across micro-cores)
        // CSRR R1, CSR_COREID → D1 0F
        // HALT → 02
        // mc0 → 4, mc1 → 5, mc2 → 6, mc3 → 7
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0]  = 8'hD1; mem[1]  = 8'h20;                     // CSRR R1, COREID
        mem[2]  = 8'h02;                                       // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        wait_all_halt(5000);
        check64("COREID mc0", uut.mc[0].u_micro.R[1], 64'd4);
        check64("COREID mc1", uut.mc[1].u_micro.R[1], 64'd5);
        check64("COREID mc2", uut.mc[2].u_micro.R[1], 64'd6);
        check64("COREID mc3", uut.mc[3].u_micro.R[1], 64'd7);

        // -----------------------------------------------------------------
        // Test 7: SEP on micro-core (kept, not trapped)
        // LDI R4, 5   → 60 40 05
        // SEP 4       → A4
        // HALT(dead)  → 02
        // HALT(target)→ 02   at addr 5
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0]  = 8'h60; mem[1]  = 8'h40; mem[2]  = 8'h05;
        mem[3]  = 8'hA4;
        mem[4]  = 8'h02;
        mem[5]  = 8'h02;

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        wait_mc0_halt(5000);
        if (uut.mc[0].u_micro.psel !== 4'd4) begin
            $display("FAIL [SEP mc0 psel]: got=%d expected=4",
                     uut.mc[0].u_micro.psel);
            fail_count = fail_count + 1;
        end else
            pass_count = pass_count + 1;

        // =================================================================
        $display("===========================================");
        if (fail_count == 0)
            $display("tb_cluster: ALL %0d TESTS PASSED", pass_count);
        else
            $display("tb_cluster: %0d PASSED, %0d FAILED", pass_count, fail_count);
        $display("===========================================");
        $finish;
    end

    // Timeout watchdog
    initial begin
        #2000000;
        $display("TIMEOUT: tb_cluster");
        $finish;
    end

endmodule
