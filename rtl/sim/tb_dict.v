// ============================================================================
// tb_dict.v — Smoke testbench for EXT.DICT engine (prefix FA)
// ============================================================================
// Exercises all 4 sub-ops of the Forth Dictionary Search Engine:
//   DFIND (00), DINS (01), DDEL (02), DCLR (03)
//
// Uses the same combinational RAM + I-cache stub as tb_string.
// Registers are set via LDI instructions (8-bit immediate, addr ≤ 0xFF).
// Data (counted-strings) placed at 0x80+; instructions at 0x00+.
//
`timescale 1ns / 1ps

`include "mp64_pkg.vh"

module tb_dict;

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

    task check_flag_z;
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

    task check_flag_v;
        input [255:0] label;
        input         expected_v;
        begin
            if (uut.flags[3] !== expected_v) begin
                $display("FAIL [%0s]: V=%b, expected=%b", label, uut.flags[3], expected_v);
                fail_count = fail_count + 1;
            end else begin
                pass_count = pass_count + 1;
            end
        end
    endtask

    task run_to_halt;
        integer cyc;
        begin
            for (cyc = 0; cyc < 20000; cyc = cyc + 1) begin
                @(posedge clk);
                if (uut.cpu_state == CPU_HALT) cyc = 20000;
            end
        end
    endtask

    integer i;

    // ====================================================================
    // Encoding notes:
    //   LDI   Rd, imm8  : 60 [Rd:4|0:4] [imm8]    (3 bytes)
    //   DCLR             : FA 03 00                  (3 bytes)
    //   DINS  Rd, Rs     : FA 01 [Rd:4|Rs:4]        (3 bytes)
    //   DFIND Rd, Rs     : FA 00 [Rd:4|Rs:4]        (3 bytes)
    //   DDEL  Rd, Rs     : FA 02 [Rd:4|Rs:4]        (3 bytes)
    //   HALT             : 02                        (1 byte)
    //
    // Data layout (counted-strings):
    //   0x80: "\003DUP"
    //   0x90: "\004SWAP"
    //   0xA0: "\004DROP"
    //   0xB0: "\003FOO"
    //   0xC0: "\024ABCDEFGHIJKLMNOPQRST" (20-byte name)
    //   0xD8-0xFC: single-char names "A"-"E" (for multi-insert)
    // ====================================================================

    initial begin
        $dumpfile("tb_dict.vcd");
        $dumpvars(0, tb_dict);

        pass_count = 0;
        fail_count = 0;

        // =================================================================
        // Test 1: DINS + DFIND hit
        //   Insert "DUP" with XT=0x34, clear Rd, then find it
        // =================================================================
        $display("--- Test 1: DINS + DFIND hit ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        // Data: "DUP" at 0x80
        mem[12'h080] = 8'h03;
        mem[12'h081] = "D"; mem[12'h082] = "U"; mem[12'h083] = "P";

        // Program at 0x00:
        //  00: DCLR            FA 03 00
        //  03: LDI R5, 0x80   60 50 80
        //  06: LDI R4, 0x34   60 40 34
        //  09: DINS R4, R5    FA 01 45
        //  0C: LDI R4, 0x00   60 40 00
        //  0F: DFIND R4, R5   FA 00 45
        //  12: HALT            02
        mem[12'h000] = 8'hFA; mem[12'h001] = 8'h03; mem[12'h002] = 8'h00;
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h50; mem[12'h005] = 8'h80;
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h40; mem[12'h008] = 8'h34;
        mem[12'h009] = 8'hFA; mem[12'h00A] = 8'h01; mem[12'h00B] = 8'h45;
        mem[12'h00C] = 8'h60; mem[12'h00D] = 8'h40; mem[12'h00E] = 8'h00;
        mem[12'h00F] = 8'hFA; mem[12'h010] = 8'h00; mem[12'h011] = 8'h45;
        mem[12'h012] = 8'h02;

        rst = 1'b1;
        repeat(4) @(posedge clk);
        rst = 1'b0;
        run_to_halt;
        check_flag_z("DINS+DFIND hit Z", 1'b1);
        check_reg("DFIND hit XT", 4, 64'h0000_0000_0000_0034);

        // =================================================================
        // Test 2: DFIND miss — insert DUP, search for SWAP
        // =================================================================
        $display("--- Test 2: DFIND miss ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h080] = 8'h03; mem[12'h081] = "D"; mem[12'h082] = "U"; mem[12'h083] = "P";
        mem[12'h090] = 8'h04; mem[12'h091] = "S"; mem[12'h092] = "W";
        mem[12'h093] = "A";   mem[12'h094] = "P";

        //  00: DCLR            FA 03 00
        //  03: LDI R5, 0x80   60 50 80
        //  06: LDI R4, 0x34   60 40 34
        //  09: DINS R4, R5    FA 01 45       (insert DUP)
        //  0C: LDI R6, 0x90   60 60 90
        //  0F: DFIND R4, R6   FA 00 46       (find SWAP → miss)
        //  12: HALT            02
        mem[12'h000] = 8'hFA; mem[12'h001] = 8'h03; mem[12'h002] = 8'h00;
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h50; mem[12'h005] = 8'h80;
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h40; mem[12'h008] = 8'h34;
        mem[12'h009] = 8'hFA; mem[12'h00A] = 8'h01; mem[12'h00B] = 8'h45;
        mem[12'h00C] = 8'h60; mem[12'h00D] = 8'h60; mem[12'h00E] = 8'h90;
        mem[12'h00F] = 8'hFA; mem[12'h010] = 8'h00; mem[12'h011] = 8'h46;
        mem[12'h012] = 8'h02;

        rst = 1'b1;
        repeat(4) @(posedge clk);
        rst = 1'b0;
        run_to_halt;
        check_flag_z("DFIND miss Z", 1'b0);

        // =================================================================
        // Test 3: DINS update — insert DUP twice with different XTs
        // =================================================================
        $display("--- Test 3: DINS update existing ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h080] = 8'h03; mem[12'h081] = "D"; mem[12'h082] = "U"; mem[12'h083] = "P";

        //  00: DCLR            FA 03 00
        //  03: LDI R5, 0x80   60 50 80
        //  06: LDI R4, 0x34   60 40 34
        //  09: DINS R4, R5    FA 01 45       (XT=0x34)
        //  0C: LDI R7, 0x56   60 70 56
        //  0F: DINS R7, R5    FA 01 75       (XT=0x56, update)
        //  12: LDI R4, 0x00   60 40 00
        //  15: DFIND R4, R5   FA 00 45
        //  18: HALT            02
        mem[12'h000] = 8'hFA; mem[12'h001] = 8'h03; mem[12'h002] = 8'h00;
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h50; mem[12'h005] = 8'h80;
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h40; mem[12'h008] = 8'h34;
        mem[12'h009] = 8'hFA; mem[12'h00A] = 8'h01; mem[12'h00B] = 8'h45;
        mem[12'h00C] = 8'h60; mem[12'h00D] = 8'h70; mem[12'h00E] = 8'h56;
        mem[12'h00F] = 8'hFA; mem[12'h010] = 8'h01; mem[12'h011] = 8'h75;
        mem[12'h012] = 8'h60; mem[12'h013] = 8'h40; mem[12'h014] = 8'h00;
        mem[12'h015] = 8'hFA; mem[12'h016] = 8'h00; mem[12'h017] = 8'h45;
        mem[12'h018] = 8'h02;

        rst = 1'b1;
        repeat(4) @(posedge clk);
        rst = 1'b0;
        run_to_halt;
        check_flag_z("DINS update Z", 1'b1);
        check_reg("DFIND update XT", 4, 64'h0000_0000_0000_0056);

        // =================================================================
        // Test 4: DDEL — delete DUP then DFIND misses
        // =================================================================
        $display("--- Test 4: DDEL + DFIND miss ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h080] = 8'h03; mem[12'h081] = "D"; mem[12'h082] = "U"; mem[12'h083] = "P";

        //  00: DCLR            FA 03 00
        //  03: LDI R5, 0x80   60 50 80
        //  06: LDI R4, 0x34   60 40 34
        //  09: DINS R4, R5    FA 01 45
        //  0C: DDEL R0, R5    FA 02 05
        //  0F: DFIND R4, R5   FA 00 45
        //  12: HALT            02
        mem[12'h000] = 8'hFA; mem[12'h001] = 8'h03; mem[12'h002] = 8'h00;
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h50; mem[12'h005] = 8'h80;
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h40; mem[12'h008] = 8'h34;
        mem[12'h009] = 8'hFA; mem[12'h00A] = 8'h01; mem[12'h00B] = 8'h45;
        mem[12'h00C] = 8'hFA; mem[12'h00D] = 8'h02; mem[12'h00E] = 8'h05;
        mem[12'h00F] = 8'hFA; mem[12'h010] = 8'h00; mem[12'h011] = 8'h45;
        mem[12'h012] = 8'h02;

        rst = 1'b1;
        repeat(4) @(posedge clk);
        rst = 1'b0;
        run_to_halt;
        check_flag_z("DDEL then DFIND Z", 1'b0);

        // =================================================================
        // Test 5: DCLR — insert, clear, then DFIND misses
        // =================================================================
        $display("--- Test 5: DCLR + DFIND miss ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h080] = 8'h03; mem[12'h081] = "D"; mem[12'h082] = "U"; mem[12'h083] = "P";

        //  00: DCLR            FA 03 00
        //  03: LDI R5, 0x80   60 50 80
        //  06: LDI R4, 0x34   60 40 34
        //  09: DINS R4, R5    FA 01 45
        //  0C: DCLR            FA 03 00
        //  0F: DFIND R4, R5   FA 00 45
        //  12: HALT            02
        mem[12'h000] = 8'hFA; mem[12'h001] = 8'h03; mem[12'h002] = 8'h00;
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h50; mem[12'h005] = 8'h80;
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h40; mem[12'h008] = 8'h34;
        mem[12'h009] = 8'hFA; mem[12'h00A] = 8'h01; mem[12'h00B] = 8'h45;
        mem[12'h00C] = 8'hFA; mem[12'h00D] = 8'h03; mem[12'h00E] = 8'h00;
        mem[12'h00F] = 8'hFA; mem[12'h010] = 8'h00; mem[12'h011] = 8'h45;
        mem[12'h012] = 8'h02;

        rst = 1'b1;
        repeat(4) @(posedge clk);
        rst = 1'b0;
        run_to_halt;
        check_flag_z("DCLR then DFIND Z", 1'b0);

        // =================================================================
        // Test 6: DDEL specific entry, others remain
        //   Insert DUP and SWAP, delete SWAP, find DUP still present
        // =================================================================
        $display("--- Test 6: DDEL selective ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h080] = 8'h03; mem[12'h081] = "D"; mem[12'h082] = "U"; mem[12'h083] = "P";
        mem[12'h090] = 8'h04; mem[12'h091] = "S"; mem[12'h092] = "W";
        mem[12'h093] = "A";   mem[12'h094] = "P";

        //  00: DCLR            FA 03 00
        //  03: LDI R5, 0x80   60 50 80      (DUP addr)
        //  06: LDI R4, 0x34   60 40 34
        //  09: DINS R4, R5    FA 01 45      (insert DUP, XT=0x34)
        //  0C: LDI R7, 0x90   60 70 90      (SWAP addr)
        //  0F: LDI R6, 0x56   60 60 56
        //  12: DINS R6, R7    FA 01 67      (insert SWAP, XT=0x56)
        //  15: DDEL R0, R7    FA 02 07      (delete SWAP)
        //  18: LDI R4, 0x00   60 40 00
        //  1B: DFIND R4, R5   FA 00 45      (find DUP)
        //  1E: HALT            02
        mem[12'h000] = 8'hFA; mem[12'h001] = 8'h03; mem[12'h002] = 8'h00;
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h50; mem[12'h005] = 8'h80;
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h40; mem[12'h008] = 8'h34;
        mem[12'h009] = 8'hFA; mem[12'h00A] = 8'h01; mem[12'h00B] = 8'h45;
        mem[12'h00C] = 8'h60; mem[12'h00D] = 8'h70; mem[12'h00E] = 8'h90;
        mem[12'h00F] = 8'h60; mem[12'h010] = 8'h60; mem[12'h011] = 8'h56;
        mem[12'h012] = 8'hFA; mem[12'h013] = 8'h01; mem[12'h014] = 8'h67;
        mem[12'h015] = 8'hFA; mem[12'h016] = 8'h02; mem[12'h017] = 8'h07;
        mem[12'h018] = 8'h60; mem[12'h019] = 8'h40; mem[12'h01A] = 8'h00;
        mem[12'h01B] = 8'hFA; mem[12'h01C] = 8'h00; mem[12'h01D] = 8'h45;
        mem[12'h01E] = 8'h02;

        rst = 1'b1;
        repeat(4) @(posedge clk);
        rst = 1'b0;
        run_to_halt;
        check_flag_z("DDEL selective DUP Z", 1'b1);
        check_reg("DDEL selective DUP XT", 4, 64'h0000_0000_0000_0034);

        // =================================================================
        // Test 7: DDEL non-existent — Z=0
        // =================================================================
        $display("--- Test 7: DDEL non-existent ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h0B0] = 8'h03; mem[12'h0B1] = "F"; mem[12'h0B2] = "O"; mem[12'h0B3] = "O";

        //  00: DCLR            FA 03 00
        //  03: LDI R5, 0xB0   60 50 B0
        //  06: DDEL R0, R5    FA 02 05
        //  09: HALT            02
        mem[12'h000] = 8'hFA; mem[12'h001] = 8'h03; mem[12'h002] = 8'h00;
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h50; mem[12'h005] = 8'hB0;
        mem[12'h006] = 8'hFA; mem[12'h007] = 8'h02; mem[12'h008] = 8'h05;
        mem[12'h009] = 8'h02;

        rst = 1'b1;
        repeat(4) @(posedge clk);
        rst = 1'b0;
        run_to_halt;
        check_flag_z("DDEL nonexist Z", 1'b0);

        // =================================================================
        // Test 8: Multi-entry DFIND
        //   Insert DUP, SWAP, DROP — then find DUP
        // =================================================================
        $display("--- Test 8: Multi-entry DFIND ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h080] = 8'h03; mem[12'h081] = "D"; mem[12'h082] = "U"; mem[12'h083] = "P";
        mem[12'h090] = 8'h04; mem[12'h091] = "S"; mem[12'h092] = "W";
        mem[12'h093] = "A";   mem[12'h094] = "P";
        mem[12'h0A0] = 8'h04; mem[12'h0A1] = "D"; mem[12'h0A2] = "R";
        mem[12'h0A3] = "O";   mem[12'h0A4] = "P";

        //  00: DCLR            FA 03 00
        //  03: LDI R5, 0x80   60 50 80
        //  06: LDI R4, 0x34   60 40 34
        //  09: DINS R4, R5    FA 01 45      (DUP, XT=0x34)
        //  0C: LDI R7, 0x90   60 70 90
        //  0F: LDI R6, 0x56   60 60 56
        //  12: DINS R6, R7    FA 01 67      (SWAP, XT=0x56)
        //  15: LDI R9, 0xA0   60 90 A0
        //  18: LDI R8, 0x78   60 80 78
        //  1B: DINS R8, R9    FA 01 89      (DROP, XT=0x78)
        //  1E: LDI R4, 0x00   60 40 00
        //  21: DFIND R4, R5   FA 00 45      (find DUP)
        //  24: HALT            02
        mem[12'h000] = 8'hFA; mem[12'h001] = 8'h03; mem[12'h002] = 8'h00;
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h50; mem[12'h005] = 8'h80;
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h40; mem[12'h008] = 8'h34;
        mem[12'h009] = 8'hFA; mem[12'h00A] = 8'h01; mem[12'h00B] = 8'h45;
        mem[12'h00C] = 8'h60; mem[12'h00D] = 8'h70; mem[12'h00E] = 8'h90;
        mem[12'h00F] = 8'h60; mem[12'h010] = 8'h60; mem[12'h011] = 8'h56;
        mem[12'h012] = 8'hFA; mem[12'h013] = 8'h01; mem[12'h014] = 8'h67;
        mem[12'h015] = 8'h60; mem[12'h016] = 8'h90; mem[12'h017] = 8'hA0;
        mem[12'h018] = 8'h60; mem[12'h019] = 8'h80; mem[12'h01A] = 8'h78;
        mem[12'h01B] = 8'hFA; mem[12'h01C] = 8'h01; mem[12'h01D] = 8'h89;
        mem[12'h01E] = 8'h60; mem[12'h01F] = 8'h40; mem[12'h020] = 8'h00;
        mem[12'h021] = 8'hFA; mem[12'h022] = 8'h00; mem[12'h023] = 8'h45;
        mem[12'h024] = 8'h02;

        rst = 1'b1;
        repeat(4) @(posedge clk);
        rst = 1'b0;
        run_to_halt;
        check_flag_z("Multi DFIND(DUP) Z", 1'b1);
        check_reg("Multi DFIND(DUP) XT", 4, 64'h0000_0000_0000_0034);

        // =================================================================
        // Test 9: Long name (20 bytes)
        // =================================================================
        $display("--- Test 9: Long name ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[12'h0C0] = 8'd20;
        mem[12'h0C1] = "A"; mem[12'h0C2] = "B"; mem[12'h0C3] = "C"; mem[12'h0C4] = "D";
        mem[12'h0C5] = "E"; mem[12'h0C6] = "F"; mem[12'h0C7] = "G"; mem[12'h0C8] = "H";
        mem[12'h0C9] = "I"; mem[12'h0CA] = "J"; mem[12'h0CB] = "K"; mem[12'h0CC] = "L";
        mem[12'h0CD] = "M"; mem[12'h0CE] = "N"; mem[12'h0CF] = "O"; mem[12'h0D0] = "P";
        mem[12'h0D1] = "Q"; mem[12'h0D2] = "R"; mem[12'h0D3] = "S"; mem[12'h0D4] = "T";

        //  00: DCLR            FA 03 00
        //  03: LDI R5, 0xC0   60 50 C0
        //  06: LDI R4, 0xAB   60 40 AB
        //  09: DINS R4, R5    FA 01 45
        //  0C: LDI R4, 0x00   60 40 00
        //  0F: DFIND R4, R5   FA 00 45
        //  12: HALT            02
        mem[12'h000] = 8'hFA; mem[12'h001] = 8'h03; mem[12'h002] = 8'h00;
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h50; mem[12'h005] = 8'hC0;
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h40; mem[12'h008] = 8'hAB;
        mem[12'h009] = 8'hFA; mem[12'h00A] = 8'h01; mem[12'h00B] = 8'h45;
        mem[12'h00C] = 8'h60; mem[12'h00D] = 8'h40; mem[12'h00E] = 8'h00;
        mem[12'h00F] = 8'hFA; mem[12'h010] = 8'h00; mem[12'h011] = 8'h45;
        mem[12'h012] = 8'h02;

        rst = 1'b1;
        repeat(4) @(posedge clk);
        rst = 1'b0;
        run_to_halt;
        check_flag_z("Long name DFIND Z", 1'b1);
        check_reg("Long name DFIND XT", 4, 64'h0000_0000_0000_00AB);

        // =================================================================
        // Test 10: 5 inserts with single-char names — all succeed
        // =================================================================
        $display("--- Test 10: Multi-insert ---");
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        // Single-char counted-strings at 0xD8, 0xE0, 0xE8, 0xF0, 0xF8
        mem[12'h0D8] = 8'h01; mem[12'h0D9] = "A";
        mem[12'h0E0] = 8'h01; mem[12'h0E1] = "B";
        mem[12'h0E8] = 8'h01; mem[12'h0E9] = "C";
        mem[12'h0F0] = 8'h01; mem[12'h0F1] = "D";
        mem[12'h0F8] = 8'h01; mem[12'h0F9] = "E";

        //  00: DCLR            FA 03 00
        //  03: LDI R4, 0xAA   60 40 AA
        //  06: LDI R5, 0xD8   60 50 D8
        //  09: DINS R4, R5    FA 01 45
        //  0C: LDI R6, 0xE0   60 60 E0
        //  0F: DINS R4, R6    FA 01 46
        //  12: LDI R7, 0xE8   60 70 E8
        //  15: DINS R4, R7    FA 01 47
        //  18: LDI R8, 0xF0   60 80 F0
        //  1B: DINS R4, R8    FA 01 48
        //  1E: LDI R9, 0xF8   60 90 F8
        //  21: DINS R4, R9    FA 01 49
        //  24: HALT            02
        mem[12'h000] = 8'hFA; mem[12'h001] = 8'h03; mem[12'h002] = 8'h00;
        mem[12'h003] = 8'h60; mem[12'h004] = 8'h40; mem[12'h005] = 8'hAA;
        mem[12'h006] = 8'h60; mem[12'h007] = 8'h50; mem[12'h008] = 8'hD8;
        mem[12'h009] = 8'hFA; mem[12'h00A] = 8'h01; mem[12'h00B] = 8'h45;
        mem[12'h00C] = 8'h60; mem[12'h00D] = 8'h60; mem[12'h00E] = 8'hE0;
        mem[12'h00F] = 8'hFA; mem[12'h010] = 8'h01; mem[12'h011] = 8'h46;
        mem[12'h012] = 8'h60; mem[12'h013] = 8'h70; mem[12'h014] = 8'hE8;
        mem[12'h015] = 8'hFA; mem[12'h016] = 8'h01; mem[12'h017] = 8'h47;
        mem[12'h018] = 8'h60; mem[12'h019] = 8'h80; mem[12'h01A] = 8'hF0;
        mem[12'h01B] = 8'hFA; mem[12'h01C] = 8'h01; mem[12'h01D] = 8'h48;
        mem[12'h01E] = 8'h60; mem[12'h01F] = 8'h90; mem[12'h020] = 8'hF8;
        mem[12'h021] = 8'hFA; mem[12'h022] = 8'h01; mem[12'h023] = 8'h49;
        mem[12'h024] = 8'h02;

        rst = 1'b1;
        repeat(4) @(posedge clk);
        rst = 1'b0;
        run_to_halt;
        check_flag_z("5 inserts Z", 1'b1);
        check_flag_v("5 inserts V", 1'b0);

        // =================================================================
        // Summary
        // =================================================================
        $display("");
        $display("==========================================");
        $display("  tb_dict: %0d passed, %0d failed", pass_count, fail_count);
        $display("==========================================");
        if (fail_count > 0)
            $display("*** FAILURES DETECTED ***");
        else
            $display("All tests PASSED.");
        $finish;
    end

endmodule
