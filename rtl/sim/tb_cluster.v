// ============================================================================
// tb_cluster.v — Testbench for mp64_cluster (micro-core cluster)
// ============================================================================
// Exercises basic instruction execution through the cluster wrapper:
//   1. NOP + HALT on micro-core 0
//   2. INC on micro-core 0
//   3. Scratchpad write/read via magic address
//   4. MUL via shared multiplier
//   5. Verify all 4 micro-cores reach HALT
//   6. MEX T.ADD via shared tile engine
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
    localparam [7:0] CLUSTER_ID_BASE = 8'd4;
    reg tile_engine_reset;
    reg [N-1:0] micro_reset;

    // Tile memory port model (256 tiles × 512 bits = 16 KiB)
    wire        tile_req;
    wire [31:0] tile_addr;
    wire        tile_wen;
    wire [511:0] tile_wdata;
    reg  [511:0] tile_rdata;
    reg          tile_ack;
    reg  [511:0] tile_mem_model [0:255];

    // External tile port (unused — stub)
    wire        ext_tile_req;
    wire [63:0] ext_tile_addr;
    wire        ext_tile_wen;
    wire [511:0] ext_tile_wdata;

    always @(posedge clk) begin
        tile_ack <= 1'b0;
        if (tile_req) begin
            if (tile_wen)
                tile_mem_model[tile_addr[13:6]] <= tile_wdata;
            tile_rdata <= tile_mem_model[tile_addr[13:6]];
            tile_ack   <= 1'b1;
        end
    end

    mp64_cluster #(
        .N              (N),
        .CLUSTER_ID_BASE(CLUSTER_ID_BASE)
    ) uut (
        .clk        (clk),
        .rst        (rst),
        .cluster_en (1'b1),
        .tile_engine_reset(tile_engine_reset),
        .micro_reset(micro_reset),

        .bus_valid  (bus_valid),
        .bus_addr   (bus_addr),
        .bus_wdata  (bus_wdata),
        .bus_wen    (bus_wen),
        .bus_size   (bus_size),
        .bus_rdata  (bus_rdata),
        .bus_ready  (bus_ready),

        .irq_timer  ({N{1'b0}}),
        .irq_ipi    ({N{1'b0}}),
        .ef_flags   (4'b0000),

        // Tile memory ports
        .tile_req   (tile_req),
        .tile_addr  (tile_addr),
        .tile_wen   (tile_wen),
        .tile_wdata (tile_wdata),
        .tile_rdata (tile_rdata),
        .tile_ack   (tile_ack),
        .tile_error (1'b0),
        .tile_fault_addr(64'd0),

        .ext_tile_req  (ext_tile_req),
        .ext_tile_addr (ext_tile_addr),
        .ext_tile_wen  (ext_tile_wen),
        .ext_tile_wdata(ext_tile_wdata),
        .ext_tile_rdata(512'd0),
        .ext_tile_ack  (1'b0),
        .ext_tile_error(1'b0),
        .ext_tile_fault_addr(64'd0),
        .tile_source_cancel(),
        .tacc_xfer_stall_cycle(1'b0)
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
    reg [N-1:0] tb_crc_req;
    reg [N*4-1:0] tb_crc_op;
    reg [N*64-1:0] tb_crc_rs_val;
    reg [N*8-1:0] tb_crc_imm8;
    reg [N-1:0] tb_crc_csr_wen;
    reg [N*8-1:0] tb_crc_csr_addr;
    reg [N*64-1:0] tb_crc_csr_wdata;
    reg [63:0] tb_crc_result;
    integer tb_crc_seen;
    reg [N-1:0] tb_mex_req;
    reg [N*2-1:0] tb_mex_ss;
    reg [N*2-1:0] tb_mex_op;
    reg [N*3-1:0] tb_mex_funct;
    reg [N*8-1:0] tb_mex_funct_byte;
    reg [N*64-1:0] tb_mex_gpr_val;
    reg [N*8-1:0] tb_mex_imm8;
    reg [N*4-1:0] tb_mex_ext_mod;
    reg [N-1:0] tb_mex_ext_active;
    reg [N*TACC_CALLER_BITS-1:0] tb_tile_caller_id;
    reg [N-1:0] tb_tile_priv;
    reg [N*64-1:0] tb_tile_mpu_base;
    reg [N*64-1:0] tb_tile_mpu_limit;
    reg [N-1:0] tb_tile_mpu_enabled;
    reg [N-1:0] tb_tile_allow_cluster_spad;
    reg [N-1:0] tb_tacc_ctl_valid;
    reg [N*64-1:0] tb_tacc_ctl_wdata;
    reg [N-1:0] tb_sha_req;
    reg [N*4-1:0] tb_sha_op;
    reg [N*64-1:0] tb_sha_rs_val;
    reg [N*8-1:0] tb_sha_imm8;
    reg [N-1:0] tb_tile_csr_req;
    reg [N-1:0] tb_tile_csr_wen;
    reg [N*8-1:0] tb_tile_csr_addr;
    reg [N*64-1:0] tb_tile_csr_wdata;
    integer tb_mex_valid_count;
    integer tb_mex_done_count;
    integer tb_ctl_valid_count;
    integer tb_ctl_done_count;
    integer tb_cancel_done_count;
    integer tb_legacy_done_count;
    integer tb_sha_done_count;
    integer tb_csr_done_count;
    integer tb_tred_done_count;
    integer tb_wait_seen;
    reg [2:0] tb_private_mex_fault;
    reg [1:0] tb_legacy_order [0:3];

    task drive_crc_op;
        input integer core_idx;
        input [3:0] op_value;
        input [63:0] rs_value;
        input [7:0] imm_value;
        output [63:0] op_result;
        integer cyc;
        reg seen;
        begin
            tb_crc_op[core_idx*4 +: 4] = op_value;
            tb_crc_rs_val[core_idx*64 +: 64] = rs_value;
            tb_crc_imm8[core_idx*8 +: 8] = imm_value;
            tb_crc_req[core_idx] = 1'b1;
            seen = 1'b0;
            op_result = 64'd0;
            for (cyc = 0; cyc < 200; cyc = cyc + 1) begin
                @(posedge clk);
                if (uut.crc_done_reg && uut.crc_grant == core_idx) begin
                    op_result = uut.crc_result_reg;
                    seen = 1'b1;
                    cyc = 200;
                end
            end
            if (!seen) begin
                $display("FAIL [CRC arbiter timeout]: core=%0d op=%0d",
                         core_idx, op_value);
                fail_count = fail_count + 1;
            end
            @(negedge clk);
            tb_crc_req[core_idx] = 1'b0;
            repeat (3) @(posedge clk);
        end
    endtask

    task drive_private_tile_csr;
        input integer core_idx;
        input [7:0] csr_addr_value;
        input [63:0] csr_data_value;
        begin
            @(negedge clk);
            tb_tile_csr_req[core_idx] = 1'b0;
            tb_tile_csr_wen[core_idx] = 1'b1;
            tb_tile_csr_addr[core_idx*8 +: 8] = csr_addr_value;
            tb_tile_csr_wdata[core_idx*64 +: 64] = csr_data_value;
            @(negedge clk);
            tb_tile_csr_wen[core_idx] = 1'b0;
    end
    endtask

    task drive_private_mex;
        input integer core_idx;
        input [1:0] source_form;
        input [1:0] operation;
        input [2:0] function_code;
        input [7:0] function_byte;
        input [3:0] ext_modifier;
        input       ext_is_active;
        output [2:0] operation_fault;
        integer cyc;
        reg seen;
        begin
            @(negedge clk);
            tb_mex_ss[core_idx*2 +: 2] = source_form;
            tb_mex_op[core_idx*2 +: 2] = operation;
            tb_mex_funct[core_idx*3 +: 3] = function_code;
            tb_mex_funct_byte[core_idx*8 +: 8] = function_byte;
            tb_mex_ext_mod[core_idx*4 +: 4] = ext_modifier;
            tb_mex_ext_active[core_idx] = ext_is_active;
            tb_mex_req[core_idx] = 1'b1;
            seen = 1'b0;
            operation_fault = MEX_FAULT_NONE;
            for (cyc = 0; cyc < 200; cyc = cyc + 1) begin
                @(negedge clk);
                if (uut.mex_done_reg &&
                    uut.mex_grant == core_idx) begin
                    operation_fault = uut.mex_fault_reg;
                    seen = 1'b1;
                    cyc = 200;
                end
            end
            if (!seen) begin
                $display("FAIL [private MEX timeout]: core=%0d op=%0d funct=%0d",
                         core_idx, operation, function_code);
                fail_count = fail_count + 1;
            end
            tb_mex_req[core_idx] = 1'b0;
            repeat (3) @(negedge clk);
        end
    endtask

    task drive_sha_op;
        input integer core_idx;
        input [3:0] op_value;
        input [63:0] rs_value;
        input [7:0] imm_value;
        integer cyc;
        reg seen;
        begin
            @(negedge clk);
            tb_sha_op[core_idx*4 +: 4] = op_value;
            tb_sha_rs_val[core_idx*64 +: 64] = rs_value;
            tb_sha_imm8[core_idx*8 +: 8] = imm_value;
            tb_sha_req[core_idx] = 1'b1;
            seen = 1'b0;
            for (cyc = 0; cyc < 400; cyc = cyc + 1) begin
                @(negedge clk);
                if (uut.sha_done_reg && uut.sha_grant == core_idx) begin
                    seen = 1'b1;
                    cyc = 400;
                end
            end
            if (!seen) begin
                $display("FAIL [SHA common-domain timeout]: core=%0d op=%0d",
                         core_idx, op_value);
                fail_count = fail_count + 1;
            end
            tb_sha_req[core_idx] = 1'b0;
            repeat (3) @(negedge clk);
        end
    endtask

    // ====================================================================
    // Main tests
    // ====================================================================
    initial begin
        $dumpfile("tb_cluster.vcd");
        $dumpvars(0, tb_cluster);

        pass_count = 0;
        fail_count = 0;
        tile_engine_reset = 1'b0;
        micro_reset = {N{1'b0}};

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

        // -----------------------------------------------------------------
        // Test 8: MEX T.ADD on micro-core 0 via shared tile engine
        //
        // Fill tile_mem_model[0] (addr 0x0000) with all 0x01 = src0
        // Fill tile_mem_model[1] (addr 0x0040) with all 0x02 = src1
        // Set TSRC0  = 0x0000
        //     TSRC1  = 0x0040
        //     TDST   = 0x0080   (tile_mem_model[2])
        //     TMODE  = 0x00     (8-bit unsigned)
        // Execute T.ADD → dst should be all 0x03
        //
        // Instruction encoding (from ISA):
        //   CSRW  CSR_TMODE,  R4  →  D8+r  addr  (D8 = CSRW R0)
        //   CSRW  CSR_TSRC0,  R4  →  DC    16
        //   ...
        //   T.ADD             →  E0 00
        //   HALT              →  02
        //
        // Program:
        //   LDI R4, 0       ; TMODE = 0x00
        //   CSRW 0x14, R4   ; CSR_TMODE = R4
        //   LDI R4, 0       ; TSRC0 address = 0
        //   CSRW 0x16, R4   ; CSR_TSRC0
        //   LDI R4, 64      ; TSRC1 address = 0x0040
        //   CSRW 0x17, R4   ; CSR_TSRC1
        //   LDI R4, 128     ; TDST  address = 0x0080
        //   CSRW 0x18, R4   ; CSR_TDST
        //   T.ADD            ; E0 00
        //   HALT             ; 02
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        // Pre-fill tile memory: tile at addr 0 = all 0x01, tile at addr 64 = all 0x02
        begin : tile_fill
            integer ti;
            for (ti = 0; ti < 256; ti = ti + 1) tile_mem_model[ti] = 512'd0;
            // Tile 0 (offset 0x0000): all bytes = 0x01
            tile_mem_model[0] = {64{8'h01}};
            // Tile 1 (offset 0x0040): all bytes = 0x02
            tile_mem_model[1] = {64{8'h02}};
        end

        // LDI R4, 0
        mem[0]  = 8'h60; mem[1]  = 8'h40; mem[2]  = 8'h00;
        // CSRW CSR_TMODE(0x14), R4
        mem[3]  = 8'hDC; mem[4]  = 8'h14;
        // LDI R4, 0
        mem[5]  = 8'h60; mem[6]  = 8'h40; mem[7]  = 8'h00;
        // CSRW CSR_TSRC0(0x16), R4
        mem[8]  = 8'hDC; mem[9]  = 8'h16;
        // LDI R4, 64
        mem[10] = 8'h60; mem[11] = 8'h40; mem[12] = 8'h40;
        // CSRW CSR_TSRC1(0x17), R4
        mem[13] = 8'hDC; mem[14] = 8'h17;
        // LDI R4, 128
        mem[15] = 8'h60; mem[16] = 8'h40; mem[17] = 8'h80;
        // CSRW CSR_TDST(0x18), R4
        mem[18] = 8'hDC; mem[19] = 8'h18;
        // T.ADD (MEX family 0xE, n=0, funct=0)
        mem[20] = 8'hE0; mem[21] = 8'h00;
        // HALT
        mem[22] = 8'h02;

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        wait_all_halt(50000);
        check_mc0_state("MEX T.ADD: mc0 halted", CPU_HALT);
        // Check that tile_mem_model[2] (dst at 0x0080) = all 0x03
        if (tile_mem_model[2] === {64{8'h03}}) begin
            pass_count = pass_count + 1;
        end else begin
            $display("FAIL [MEX T.ADD dst]: got=%h expected=%h",
                     tile_mem_model[2], {64{8'h03}});
            fail_count = fail_count + 1;
        end

        // -----------------------------------------------------------------
        // Test 9: CSRR tile CSR readback on micro-core 0
        // After test 8, CSR_TDST should still be 128 (0x80).
        //   CSRR R1, CSR_TDST (0x18)  →  D1 18
        //   HALT                       →  02
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0]  = 8'hD1; mem[1]  = 8'h18;    // CSRR R1, CSR_TDST
        mem[2]  = 8'h02;                       // HALT

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        wait_all_halt(5000);
        // Note: after reset the tile CSRs are cleared, so tdst=0
        // Instead verify that micro-core can write+read tile CSRs:
        // Rewrite as: CSRW CSR_TDST=0x42, CSRR R1, CSR_TDST, HALT
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        // LDI R4, 0x42
        mem[0]  = 8'h60; mem[1]  = 8'h40; mem[2]  = 8'h42;
        // CSRW CSR_TDST(0x18), R4
        mem[3]  = 8'hDC; mem[4]  = 8'h18;
        // CSRR R1, CSR_TDST(0x18)
        mem[5]  = 8'hD1; mem[6]  = 8'h18;
        // HALT
        mem[7]  = 8'h02;

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        wait_all_halt(5000);
        check64("tile CSR w/r mc0", uut.mc[0].u_micro.R[1], 64'h42);

        // -----------------------------------------------------------------
        // Test 10: Two micro-cores both issue T.ADD (contention test)
        // mc0 and mc1 run the same T.ADD program but can't both use the
        // shared tile engine simultaneously. The arbiter should serialise.
        // If both complete and halt, the arbiter is working.
        // (We just check both reach HALT — functional correctness was
        //  already verified in test 8.)
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;
        tile_mem_model[0] = {64{8'h05}};
        tile_mem_model[1] = {64{8'h03}};

        // Same program as test 8 (shorter: reuse the TMODE=0 from reset)
        // LDI R4, 0;  CSRW TMODE, R4
        mem[0]  = 8'h60; mem[1]  = 8'h40; mem[2]  = 8'h00;
        mem[3]  = 8'hDC; mem[4]  = 8'h14;
        // LDI R4, 0;  CSRW TSRC0, R4
        mem[5]  = 8'h60; mem[6]  = 8'h40; mem[7]  = 8'h00;
        mem[8]  = 8'hDC; mem[9]  = 8'h16;
        // LDI R4, 64; CSRW TSRC1, R4
        mem[10] = 8'h60; mem[11] = 8'h40; mem[12] = 8'h40;
        mem[13] = 8'hDC; mem[14] = 8'h17;
        // LDI R4, 128; CSRW TDST, R4
        mem[15] = 8'h60; mem[16] = 8'h40; mem[17] = 8'h80;
        mem[18] = 8'hDC; mem[19] = 8'h18;
        // T.ADD
        mem[20] = 8'hE0; mem[21] = 8'h00;
        // HALT
        mem[22] = 8'h02;

        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;

        wait_all_halt(100000);
        check_mc0_state("contention: mc0 halted", CPU_HALT);
        check_mc1_state("contention: mc1 halted", CPU_HALT);
        check_mc2_state("contention: mc2 halted", CPU_HALT);
        check_mc3_state("contention: mc3 halted", CPU_HALT);
        // The destination tile should be 0x05 + 0x03 = 0x08
        // (last core to write wins, but values are the same)
        if (tile_mem_model[2] === {64{8'h08}}) begin
            pass_count = pass_count + 1;
        end else begin
            $display("FAIL [contention T.ADD dst]: got=%h expected=%h",
                     tile_mem_model[2], {64{8'h08}});
            fail_count = fail_count + 1;
        end

        // -----------------------------------------------------------------
        // Test 11: shared CRC transactions remain owner-atomic.
        // Drive the arbiter at its request boundary so instruction-fetch
        // bus timing is not part of this focused lock test.
        // -----------------------------------------------------------------
        for (i = 0; i < 4096; i = i + 1) mem[i] = 8'h00;

        mem[0] = 8'h02;
        rst = 1'b1;
        repeat (4) @(posedge clk);
        rst = 1'b0;
        wait_all_halt(5000);

        tb_crc_req = {N{1'b0}};
        tb_crc_op = {(N*4){1'b0}};
        tb_crc_rs_val = {(N*64){1'b0}};
        tb_crc_imm8 = {(N*8){1'b0}};
        tb_crc_csr_wen = {N{1'b0}};
        tb_crc_csr_addr = {(N*8){1'b0}};
        tb_crc_csr_wdata = {(N*64){1'b0}};
        force uut.mc_crc_req = tb_crc_req;
        force uut.mc_crc_op = tb_crc_op;
        force uut.mc_crc_rs_val = tb_crc_rs_val;
        force uut.mc_crc_imm8 = tb_crc_imm8;
        force uut.mc_cl_csr_wen = tb_crc_csr_wen;
        force uut.mc_cl_csr_addr = tb_crc_csr_addr;
        force uut.mc_cl_csr_wdata = tb_crc_csr_wdata;

        // Two unlocked cores request distinct MODE transactions together.
        // Round-robin starts after crc_last=0, so core 1 must win with its
        // own immediate; core 0 must remain blocked by the new lock.
        tb_crc_op[0 +: 4] = ISA_CRC_MODEX;
        tb_crc_imm8[0 +: 8] = 8'd1;
        tb_crc_op[4 +: 4] = ISA_CRC_MODEX;
        tb_crc_imm8[8 +: 8] = 8'd0;
        tb_crc_req[0] = 1'b1;
        tb_crc_req[1] = 1'b1;
        tb_crc_seen = 0;
        for (i = 0; i < 200; i = i + 1) begin
            @(posedge clk);
            if (uut.crc_done_reg) begin
                if (uut.crc_grant !== 1) begin
                    $display("FAIL [simultaneous CRC.MODE grant]: got=%0d expected=1",
                             uut.crc_grant);
                    fail_count = fail_count + 1;
                end else begin
                    pass_count = pass_count + 1;
                end
                tb_crc_seen = 1;
                i = 200;
            end
        end
        if (!tb_crc_seen) begin
            $display("FAIL [simultaneous CRC.MODE timeout]");
            fail_count = fail_count + 1;
        end
        @(negedge clk);
        tb_crc_req[1] = 1'b0;
        repeat (20) begin
            @(posedge clk);
            if (uut.crc_done_reg && uut.crc_grant == 0) begin
                $display("FAIL [losing simultaneous CRC.MODE interleaved]");
                fail_count = fail_count + 1;
            end
        end
        check64("simultaneous CRC.MODE uses winner immediate",
                {62'd0, uut.cl_crc_mode}, 64'd0);
        check64("simultaneous CRC.MODE locks winner",
                {62'd0, uut.crc_lock_owner}, 64'd1);
        @(negedge clk);
        tb_crc_req[0] = 1'b0;
        repeat (3) @(posedge clk);

        drive_crc_op(1, ISA_CRC_INIT, 64'd0, 8'd0, tb_crc_result);

        // Raw CRC CSR writes are ignored on every micro-core, independent
        // of lock ownership.
        tb_crc_csr_addr[0 +: 8] = CSR_CRC_ACC;
        tb_crc_csr_wdata[0 +: 64] = 64'hDEAD_BEEF_CAFE_BABE;
        tb_crc_csr_wen[0] = 1'b1;
        repeat (2) @(posedge clk);
        @(negedge clk);
        tb_crc_csr_wen[0] = 1'b0;
        check64("CRC non-owner CSR write ignored", uut.cl_crc_acc,
                64'h0000_0000_FFFF_FFFF);

        tb_crc_csr_addr[8 +: 8] = CSR_CRC_ACC;
        tb_crc_csr_wdata[64 +: 64] = 64'h0123_4567_89AB_CDEF;
        tb_crc_csr_wen[1] = 1'b1;
        repeat (2) @(posedge clk);
        @(negedge clk);
        tb_crc_csr_wen[1] = 1'b0;
        check64("CRC owner CSR write ignored", uut.cl_crc_acc,
                64'h0000_0000_FFFF_FFFF);

        drive_crc_op(1, ISA_CRC_SEED, 64'hDEAD_BEEF_FFFF_FFFF,
                     8'd0, tb_crc_result);
        check64("CRC.SEED masks high half in mode 0", uut.cl_crc_acc,
                64'h0000_0000_FFFF_FFFF);

        // A non-owner cannot interleave even a new MODE transaction.
        tb_crc_op[0 +: 4] = ISA_CRC_MODEX;
        tb_crc_imm8[0 +: 8] = 8'd1;
        tb_crc_req[0] = 1'b1;
        repeat (20) begin
            @(posedge clk);
            if (uut.crc_done_reg && uut.crc_grant == 0) begin
                $display("FAIL [CRC non-owner interleaved while locked]");
                fail_count = fail_count + 1;
            end
        end
        @(negedge clk);
        tb_crc_req[0] = 1'b0;

        drive_crc_op(1, ISA_CRC_B, 64'h41, 8'd0, tb_crc_result);
        drive_crc_op(1, ISA_CRC_FIN, 64'd0, 8'd0, tb_crc_result);
        check64("CRC owner 1 finalized result", tb_crc_result,
                64'h0000_0000_81B0_2D8B);
        check64("CRC FIN publishes shared accumulator", uut.cl_crc_acc,
                64'h0000_0000_81B0_2D8B);
        if (uut.crc_locked !== 1'b0) begin
            $display("FAIL [CRC contention lock release]: crc_locked=%b",
                     uut.crc_locked);
            fail_count = fail_count + 1;
        end else begin
            pass_count = pass_count + 1;
        end

        drive_crc_op(0, ISA_CRC_MODEX, 64'd0, 8'd3, tb_crc_result);
        check64("CRC mode 3 canonicalizes to mode 0", {62'd0, uut.cl_crc_mode},
                64'd0);
        drive_crc_op(0, ISA_CRC_MODEX, 64'd0, 8'd5, tb_crc_result);
        check64("CRC mode 5 does not alias mode 1", {62'd0, uut.cl_crc_mode},
                64'd0);
        drive_crc_op(0, ISA_CRC_MODEX, 64'd0, 8'hFF, tb_crc_result);
        check64("CRC mode FF canonicalizes to mode 0", {62'd0, uut.cl_crc_mode},
                64'd0);
        drive_crc_op(0, ISA_CRC_INIT, 64'd0, 8'd0, tb_crc_result);
        drive_crc_op(0, ISA_CRC_SEED, 64'h0000_0000_FFFF_FFFF,
                     8'd0, tb_crc_result);
        drive_crc_op(0, ISA_CRC_B, 64'h41, 8'd0, tb_crc_result);
        drive_crc_op(0, ISA_CRC_FIN, 64'd0, 8'd0, tb_crc_result);
        check64("CRC owner 0 after handoff", tb_crc_result,
                64'h0000_0000_81B0_2D8B);

        release uut.mc_crc_req;
        release uut.mc_crc_op;
        release uut.mc_crc_rs_val;
        release uut.mc_crc_imm8;
        release uut.mc_cl_csr_wen;
        release uut.mc_cl_csr_addr;
        release uut.mc_cl_csr_wdata;

        // -----------------------------------------------------------------
        // Test 12: TACC MEX/control arbitration and caller-relative status.
        // Drive the shared-resource request boundary directly, matching the
        // focused CRC arbiter tests above.
        // -----------------------------------------------------------------
        tb_mex_req = {N{1'b0}};
        tb_mex_ss = {(N*2){1'b0}};
        tb_mex_op = {(N*2){1'b0}};
        tb_mex_funct = {(N*3){1'b0}};
        tb_mex_funct_byte = {(N*8){1'b0}};
        tb_mex_gpr_val = {(N*64){1'b0}};
        tb_mex_imm8 = {(N*8){1'b0}};
        tb_mex_ext_mod = {(N*4){1'b0}};
        tb_mex_ext_active = {N{1'b0}};
        tb_tile_caller_id = {(N*TACC_CALLER_BITS){1'b0}};
        tb_tile_caller_id[0*TACC_CALLER_BITS +: TACC_CALLER_BITS] =
            CLUSTER_ID_BASE + 8'd0;
        tb_tile_caller_id[1*TACC_CALLER_BITS +: TACC_CALLER_BITS] =
            CLUSTER_ID_BASE + 8'd1;
        tb_tile_caller_id[2*TACC_CALLER_BITS +: TACC_CALLER_BITS] =
            CLUSTER_ID_BASE + 8'd2;
        tb_tile_caller_id[3*TACC_CALLER_BITS +: TACC_CALLER_BITS] =
            CLUSTER_ID_BASE + 8'd3;
        tb_tile_priv = {N{1'b0}};
        tb_tile_mpu_base = {(N*64){1'b0}};
        tb_tile_mpu_limit = {(N*64){1'b0}};
        tb_tile_mpu_enabled = {N{1'b0}};
        tb_tile_allow_cluster_spad = {N{1'b1}};
        tb_tacc_ctl_valid = {N{1'b0}};
        tb_tacc_ctl_wdata = {(N*64){1'b0}};
        tb_sha_req = {N{1'b0}};
        tb_sha_op = {(N*4){1'b0}};
        tb_sha_rs_val = {(N*64){1'b0}};
        tb_sha_imm8 = {(N*8){1'b0}};
        tb_tile_csr_req = {N{1'b0}};
        tb_tile_csr_wen = {N{1'b0}};
        tb_tile_csr_addr = {(N*8){1'b0}};
        tb_tile_csr_wdata = {(N*64){1'b0}};

        force uut.mc_mex_req = tb_mex_req;
        force uut.mc_mex_ss = tb_mex_ss;
        force uut.mc_mex_op = tb_mex_op;
        force uut.mc_mex_funct = tb_mex_funct;
        force uut.mc_mex_funct_byte = tb_mex_funct_byte;
        force uut.mc_mex_gpr_val = tb_mex_gpr_val;
        force uut.mc_mex_imm8 = tb_mex_imm8;
        force uut.mc_mex_ext_mod = tb_mex_ext_mod;
        force uut.mc_mex_ext_active = tb_mex_ext_active;
        force uut.mc_tile_caller_id = tb_tile_caller_id;
        force uut.mc_tile_priv = tb_tile_priv;
        force uut.mc_tile_mpu_base = tb_tile_mpu_base;
        force uut.mc_tile_mpu_limit = tb_tile_mpu_limit;
        force uut.mc_tile_mpu_enabled = tb_tile_mpu_enabled;
        force uut.mc_tile_allow_cluster_spad =
            tb_tile_allow_cluster_spad;
        force uut.mc_tacc_ctl_valid = tb_tacc_ctl_valid;
        force uut.mc_tacc_ctl_wdata = tb_tacc_ctl_wdata;
        force uut.mc_sha_req = tb_sha_req;
        force uut.mc_sha_op = tb_sha_op;
        force uut.mc_sha_rs_val = tb_sha_rs_val;
        force uut.mc_sha_imm8 = tb_sha_imm8;
        force uut.mc_tile_csr_req = tb_tile_csr_req;
        force uut.mc_tile_csr_wen = tb_tile_csr_wen;
        force uut.mc_tile_csr_addr = tb_tile_csr_addr;
        force uut.mc_tile_csr_wdata = tb_tile_csr_wdata;

        // A held architectural request produces one dispatch and one routed
        // completion, then remains in WAIT_DROP until the caller withdraws.
        tb_mex_op[2*2 +: 2] = MEX_TMUL;
        tb_mex_funct[2*3 +: 3] = 3'd7;
        tb_mex_funct_byte[2*8 +: 8] = 8'h07;
        tb_mex_valid_count = 0;
        tb_mex_done_count = 0;
        @(negedge clk);
        tb_mex_req[2] = 1'b1;
        repeat (32) begin
            @(negedge clk);
            if (uut.te_mex_valid)
                tb_mex_valid_count = tb_mex_valid_count + 1;
            if (uut.mex_done_reg && uut.mex_grant == 2)
                tb_mex_done_count = tb_mex_done_count + 1;
        end
        check64("held MEX dispatches once",
                tb_mex_valid_count, 64'd1);
        check64("held MEX completes once",
                tb_mex_done_count, 64'd1);
        check64("held MEX remains in WAIT_DROP",
                uut.mex_state, uut.MEX_WAIT_DROP);
        check64("held MEX holds common WAIT_DROP",
                uut.legacy_state, uut.LEGACY_WAIT_DROP);
        check64("MEX fault routes from granted caller",
                uut.mex_fault_reg, MEX_FAULT_ILLEGAL);
        check64("MEX raw function captured",
                uut.te_mex_funct_byte, 64'h07);
        check64("MEX absolute caller captured",
                uut.te_mex_caller_id, CLUSTER_ID_BASE + 8'd2);
        @(negedge clk);
        tb_mex_req[2] = 1'b0;
        repeat (3) @(negedge clk);
        check64("MEX returns idle after request drops",
                uut.mex_state, uut.MEX_IDLE);
        check64("common domain idles after MEX drops",
                uut.legacy_state, uut.LEGACY_IDLE);

        // A caller reset concurrent with first admission must mask both MEX
        // and control requests and advance exactly one caller epoch.
        tb_mex_op[1*2 +: 2] = MEX_TMUL;
        tb_mex_funct[1*3 +: 3] = 3'd7;
        tb_mex_funct_byte[1*8 +: 8] = 8'h07;
        tb_tacc_ctl_wdata[1*64 +: 64] = 64'd1;
        tb_mex_valid_count = 0;
        tb_ctl_valid_count = 0;
        tb_ctl_done_count = 0;
        @(negedge clk);
        tb_mex_req[1] = 1'b1;
        tb_tacc_ctl_valid[1] = 1'b1;
        micro_reset[1] = 1'b1;
        repeat (3) begin
            @(negedge clk);
            if (uut.te_mex_valid)
                tb_mex_valid_count = tb_mex_valid_count + 1;
            if (uut.te_tacc_ctl_valid)
                tb_ctl_valid_count = tb_ctl_valid_count + 1;
            if (uut.tacc_ctl_done_reg)
                tb_ctl_done_count = tb_ctl_done_count + 1;
        end
        check64("same-edge reset masks MEX admission",
                tb_mex_valid_count, 64'd0);
        check64("same-edge reset masks control admission",
                tb_ctl_valid_count, 64'd0);
        check64("same-edge reset prevents control completion",
                tb_ctl_done_count, 64'd0);
        check64("same-edge reset advances caller epoch once",
                uut.tacc_caller_epoch[1], 64'd1);
        @(negedge clk);
        tb_mex_req[1] = 1'b0;
        tb_tacc_ctl_valid[1] = 1'b0;
        micro_reset[1] = 1'b0;
        repeat (3) @(negedge clk);

        // The control sideband must complete independently while another
        // caller owns the active MEX grant.
        tb_mex_op[0 +: 2] = MEX_TMUL;
        tb_mex_funct[0 +: 3] = 3'd7;
        tb_mex_funct_byte[0 +: 8] = 8'h07;
        force uut.te_mex_done = 1'b0;
        @(negedge clk);
        tb_mex_req[0] = 1'b1;
        while (uut.mex_state != uut.MEX_ACTIVE) @(negedge clk);
        tb_tacc_ctl_wdata[1*64 +: 64] = 64'd1;
        tb_tacc_ctl_valid[1] = 1'b1;
        tb_ctl_done_count = 0;
        repeat (12) begin
            @(negedge clk);
            if (uut.tacc_ctl_done_reg && uut.tacc_ctl_grant == 1)
                tb_ctl_done_count = tb_ctl_done_count + 1;
        end
        check64("control completes once while MEX active",
                tb_ctl_done_count, 64'd1);
        check64("control does not disturb active MEX grant",
                uut.mex_state, uut.MEX_ACTIVE);
        check64("control preserves active MEX owner",
                uut.mex_grant, 64'd0);
        check64("control preserves common MEX turn",
                {60'd0, uut.legacy_kind, uut.legacy_grant},
                {60'd0, uut.LEGACY_KIND_MEX, 2'd0});
        @(negedge clk);
        tb_tacc_ctl_valid[1] = 1'b0;
        repeat (3) @(negedge clk);

        // Reset the active caller.  The arbiter and leaf both cancel it and
        // a late leaf completion cannot be routed.
        tb_cancel_done_count = 0;
        @(negedge clk);
        micro_reset[0] = 1'b1;
        repeat (3) begin
            @(negedge clk);
            if (uut.mex_done_reg)
                tb_cancel_done_count = tb_cancel_done_count + 1;
        end
        @(negedge clk);
        tb_mex_req[0] = 1'b0;
        micro_reset[0] = 1'b0;

        // Present a stale completion after the request and reset have both
        // dropped.  Releasing a forced-low leaf would not itself inject one.
        force uut.te_mex_done = 1'b1;
        @(negedge clk);
        if (uut.mex_done_reg)
            tb_cancel_done_count = tb_cancel_done_count + 1;
        force uut.te_mex_done = 1'b0;
        release uut.te_mex_done;
        repeat (2) begin
            @(negedge clk);
            if (uut.mex_done_reg)
                tb_cancel_done_count = tb_cancel_done_count + 1;
        end
        check64("cancelled active MEX has no late completion",
                tb_cancel_done_count, 64'd0);
        check64("cancelled active MEX returns arbiter idle",
                uut.mex_state, uut.MEX_IDLE);
        check64("cancelled active MEX releases common domain",
                uut.legacy_state, uut.LEGACY_IDLE);
        check64("active reset advances caller epoch once",
                uut.tacc_caller_epoch[0], 64'd1);
        repeat (3) @(negedge clk);

        // A fresh caller can use the engine after cancellation.
        tb_mex_op[3*2 +: 2] = MEX_TMUL;
        tb_mex_funct[3*3 +: 3] = 3'd7;
        tb_mex_funct_byte[3*8 +: 8] = 8'h07;
        tb_mex_done_count = 0;
        @(negedge clk);
        tb_mex_req[3] = 1'b1;
        repeat (24) begin
            @(negedge clk);
            if (uut.mex_done_reg && uut.mex_grant == 3)
                tb_mex_done_count = tb_mex_done_count + 1;
        end
        check64("fresh caller completes after cancellation",
                tb_mex_done_count, 64'd1);
        @(negedge clk);
        tb_mex_req[3] = 1'b0;
        repeat (3) @(negedge clk);

        // Only MINE is caller-relative; every physical status field remains
        // identical for all simultaneous readers.
        force uut.te_tacc_status_raw =
            (64'd1 << TACC_STATUS_BIT_CLAIMED) |
            (64'd1 << TACC_STATUS_BIT_VALID) |
            (64'd1 << TACC_STATUS_BIT_BUSY) |
            (64'd5 << TACC_STATUS_OWNER_LSB);
        #1;
        check64("nonowner status clears MINE",
                uut.mc_tacc_status[0*64 +: 64],
                (64'd1 << TACC_STATUS_BIT_CLAIMED) |
                (64'd1 << TACC_STATUS_BIT_VALID) |
                (64'd1 << TACC_STATUS_BIT_BUSY) |
                (64'd5 << TACC_STATUS_OWNER_LSB));
        check64("matching absolute caller status sets MINE",
                uut.mc_tacc_status[1*64 +: 64],
                (64'd1 << TACC_STATUS_BIT_CLAIMED) |
                (64'd1 << TACC_STATUS_BIT_MINE) |
                (64'd1 << TACC_STATUS_BIT_VALID) |
                (64'd1 << TACC_STATUS_BIT_BUSY) |
                (64'd5 << TACC_STATUS_OWNER_LSB));
        release uut.te_tacc_status_raw;

        // -----------------------------------------------------------------
        // Test 13: one caller-round-robin domain covers SHA, MEX, SHA
        // metadata CSR, and legacy ACC CSR requests.  With last=0, four
        // simultaneous callers must complete in order 1,2,3,0 regardless of
        // producer kind.  Held requests still produce exactly one completion.
        // -----------------------------------------------------------------
        @(negedge clk);
        tb_mex_req = {N{1'b0}};
        tb_sha_req = {N{1'b0}};
        tb_tile_csr_req = {N{1'b0}};
        tb_tile_csr_wen = {N{1'b0}};
        tile_engine_reset = 1'b1;
        repeat (2) @(negedge clk);
        tile_engine_reset = 1'b0;
        repeat (2) @(negedge clk);

        tb_sha_op[1*4 +: 4] = ISA_SHA_RELEASE;
        tb_sha_imm8[1*8 +: 8] = 8'd0;

        tb_mex_ss[2*2 +: 2] = 2'd0;
        tb_mex_op[2*2 +: 2] = MEX_TMUL;
        tb_mex_funct[2*3 +: 3] = 3'd7;
        tb_mex_funct_byte[2*8 +: 8] = 8'h07;

        tb_tile_csr_addr[3*8 +: 8] = CSR_SHA_MODE;
        tb_tile_csr_wen[3] = 1'b0;
        tb_tile_csr_addr[0*8 +: 8] = CSR_ACC0;
        tb_tile_csr_wdata[0*64 +: 64] =
            64'hCAFE_BABE_0123_4567;
        tb_tile_csr_wen[0] = 1'b1;

        tb_legacy_done_count = 0;
        @(negedge clk);
        tb_sha_req[1] = 1'b1;
        tb_mex_req[2] = 1'b1;
        tb_tile_csr_req[3] = 1'b1;
        tb_tile_csr_req[0] = 1'b1;
        for (i = 0; i < 400; i = i + 1) begin
            @(negedge clk);
            if (uut.sha_done_reg && uut.sha_grant == 1 &&
                tb_sha_req[1]) begin
                if (tb_legacy_done_count < 4)
                    tb_legacy_order[tb_legacy_done_count] = 2'd1;
                tb_legacy_done_count = tb_legacy_done_count + 1;
                tb_sha_req[1] = 1'b0;
            end
            if (uut.mex_done_reg && uut.mex_grant == 2 &&
                tb_mex_req[2]) begin
                if (tb_legacy_done_count < 4)
                    tb_legacy_order[tb_legacy_done_count] = 2'd2;
                tb_legacy_done_count = tb_legacy_done_count + 1;
                tb_mex_req[2] = 1'b0;
            end
            if (uut.mc_tile_csr_done[3] && tb_tile_csr_req[3]) begin
                if (tb_legacy_done_count < 4)
                    tb_legacy_order[tb_legacy_done_count] = 2'd3;
                tb_legacy_done_count = tb_legacy_done_count + 1;
                tb_tile_csr_req[3] = 1'b0;
            end
            if (uut.mc_tile_csr_done[0] && tb_tile_csr_req[0]) begin
                if (tb_legacy_done_count < 4)
                    tb_legacy_order[tb_legacy_done_count] = 2'd0;
                tb_legacy_done_count = tb_legacy_done_count + 1;
                tb_tile_csr_req[0] = 1'b0;
            end
            if (tb_legacy_done_count == 4)
                i = 400;
        end
        check64("mixed common domain completes four",
                tb_legacy_done_count, 64'd4);
        check64("mixed common RR first caller",
                tb_legacy_order[0], 64'd1);
        check64("mixed common RR second caller",
                tb_legacy_order[1], 64'd2);
        check64("mixed common RR third caller",
                tb_legacy_order[2], 64'd3);
        check64("mixed common RR wraps to zero",
                tb_legacy_order[3], 64'd0);
        repeat (4) @(negedge clk);
        check64("mixed common RR cursor",
                uut.legacy_last, 64'd0);
        check64("SHA metadata CSR shares common domain",
                uut.mc_tile_csr_rdata[3*64 +: 64], 64'd0);
        check64("ACC CSR write reaches tile-owned bank",
                uut.te_legacy_acc_state[0*64 +: 64],
                64'hCAFE_BABE_0123_4567);

        // A different caller reads the same authoritative ACC bank through
        // the acknowledged CSR path.
        tb_tile_csr_addr[2*8 +: 8] = CSR_ACC0;
        tb_tile_csr_wen[2] = 1'b0;
        tb_wait_seen = 0;
        @(negedge clk);
        tb_tile_csr_req[2] = 1'b1;
        for (i = 0; i < 100; i = i + 1) begin
            @(negedge clk);
            if (uut.mc_tile_csr_done[2]) begin
                tb_wait_seen = 1;
                i = 100;
            end
        end
        if (!tb_wait_seen) begin
            $display("FAIL [authoritative ACC CSR read timeout]");
            fail_count = fail_count + 1;
        end
        tb_tile_csr_req[2] = 1'b0;
        repeat (3) @(negedge clk);
        check64("ACC CSR read observes shared bank",
                uut.mc_tile_csr_rdata[2*64 +: 64],
                64'hCAFE_BABE_0123_4567);

        // -----------------------------------------------------------------
        // Test 14: SHA owns the shared digest/legacy-ACC transaction, but does
        // not monopolize stateless MEX service.  Private config is sampled
        // from the granted MEX caller, FINAL retains the lock, RELEASE alone
        // unlocks, and ACC_ZERO clears only the granted caller's TCTRL shadow.
        // -----------------------------------------------------------------
        tile_mem_model[0] = {64{8'h01}};
        tile_mem_model[1] = {64{8'h02}};
        tile_mem_model[2] = 512'd0;
        tile_mem_model[3] = {64{8'h01}};

        drive_private_tile_csr(2, CSR_TMODE, 64'd0);
        drive_private_tile_csr(2, CSR_TSRC0, 64'h0000);
        drive_private_tile_csr(2, CSR_TSRC1, 64'h0040);
        drive_private_tile_csr(2, CSR_TDST, 64'h0080);
        drive_private_tile_csr(3, CSR_TMODE, 64'd0);
        drive_private_tile_csr(3, CSR_TSRC0, 64'h00C0);
        drive_private_tile_csr(3, CSR_TCTRL, 64'h0002);
        drive_private_tile_csr(1, CSR_TCTRL, 64'h0002);
        check64("caller 2 keeps private TSRC0",
                uut.cfg_tsrc0[2], 64'h0000);
        check64("caller 3 keeps private TSRC0",
                uut.cfg_tsrc0[3], 64'h00C0);

        drive_sha_op(0, ISA_SHA_INIT, 64'd0, 8'd0);
        check64("SHA INIT acquires lock",
                {61'd0, uut.sha_locked, uut.sha_lock_owner},
                {61'd0, 1'b1, 2'd0});
        check64("SHA INIT writes authoritative ACC0",
                uut.te_legacy_acc_state[0*64 +: 64],
                64'h6a09e667_bb67ae85);

        // Caller 1 requests a protected ACC write, caller 3 requests a
        // protected reduction, and caller 2 requests stateless T.ADD.
        // Only caller 2 may receive service while caller 0 owns SHA.
        tb_tile_csr_addr[1*8 +: 8] = CSR_ACC0;
        tb_tile_csr_wdata[1*64 +: 64] = 64'hDEAD_BEEF_CAFE_0001;
        tb_tile_csr_wen[1] = 1'b1;

        tb_mex_ss[2*2 +: 2] = 2'd0;
        tb_mex_op[2*2 +: 2] = MEX_TALU;
        tb_mex_funct[2*3 +: 3] = TALU_ADD;
        tb_mex_funct_byte[2*8 +: 8] = {5'd0, TALU_ADD};
        tb_mex_ss[3*2 +: 2] = 2'd0;
        tb_mex_op[3*2 +: 2] = MEX_TRED;
        tb_mex_funct[3*3 +: 3] = TRED_SUM;
        tb_mex_funct_byte[3*8 +: 8] = {5'd0, TRED_SUM};

        tb_mex_done_count = 0;
        tb_csr_done_count = 0;
        tb_tred_done_count = 0;
        @(negedge clk);
        tb_tile_csr_req[1] = 1'b1;
        tb_mex_req[2] = 1'b1;
        tb_mex_req[3] = 1'b1;
        for (i = 0; i < 400; i = i + 1) begin
            @(negedge clk);
            if (uut.mc_tile_csr_done[1])
                tb_csr_done_count = tb_csr_done_count + 1;
            if (uut.mex_done_reg && uut.mex_grant == 3)
                tb_tred_done_count = tb_tred_done_count + 1;
            if (uut.mex_done_reg && uut.mex_grant == 2 &&
                tb_mex_req[2]) begin
                tb_mex_done_count = tb_mex_done_count + 1;
                tb_mex_req[2] = 1'b0;
                i = 400;
            end
        end
        check64("stateless MEX completes under SHA lock",
                tb_mex_done_count, 64'd1);
        repeat (16) begin
            @(negedge clk);
            if (uut.mc_tile_csr_done[1])
                tb_csr_done_count = tb_csr_done_count + 1;
            if (uut.mex_done_reg && uut.mex_grant == 3)
                tb_tred_done_count = tb_tred_done_count + 1;
        end
        check64("SHA lock blocks nonowner ACC CSR",
                tb_csr_done_count, 64'd0);
        check64("SHA lock blocks nonowner TRED",
                tb_tred_done_count, 64'd0);
        check64("SHA lock preserves authoritative ACC",
                uut.te_legacy_acc_state[0*64 +: 64],
                64'h6a09e667_bb67ae85);
        check64("stateless MEX sampled caller 2 config",
                uut.u_tile.tsrc0, 64'h0000);
        if (tile_mem_model[2] === {64{8'h03}})
            pass_count = pass_count + 1;
        else begin
            $display("FAIL [stateless MEX under SHA lock]: got=%h",
                     tile_mem_model[2]);
            fail_count = fail_count + 1;
        end

        // The immediate-source encoding still carries the raw TMUL function
        // into the current leaf.  A low function value of DOT must therefore
        // remain protected rather than bypassing the SHA transaction lock.
        tb_mex_ss[2*2 +: 2] = 2'd2;
        tb_mex_op[2*2 +: 2] = MEX_TMUL;
        tb_mex_funct[2*3 +: 3] = TMUL_DOT;
        tb_mex_funct_byte[2*8 +: 8] = {5'd0, TMUL_DOT};
        tb_mex_done_count = 0;
        @(negedge clk);
        tb_mex_req[2] = 1'b1;
        repeat (16) begin
            @(negedge clk);
            if (uut.mex_done_reg && uut.mex_grant == 2)
                tb_mex_done_count = tb_mex_done_count + 1;
        end
        check64("SHA lock blocks SS2 DOT",
                tb_mex_done_count, 64'd0);
        check64("blocked SS2 DOT preserves ACC",
                uut.te_legacy_acc_state[0*64 +: 64],
                64'h6a09e667_bb67ae85);
        tb_mex_req[2] = 1'b0;
        repeat (3) @(negedge clk);

        drive_sha_op(0, ISA_SHA_FINAL, 64'd0, 8'd0);
        check64("SHA FINAL retains transaction lock",
                {61'd0, uut.sha_locked, uut.sha_lock_owner},
                {61'd0, 1'b1, 2'd0});
        check64("FINAL leaves blocked ACC pending",
                tb_csr_done_count, 64'd0);
        check64("FINAL leaves blocked TRED pending",
                tb_tred_done_count, 64'd0);

        // Release caller 0, then retain both previously blocked requests.
        // RR last=0 after RELEASE, so caller 1's ACC write must precede caller
        // 3's reduction.
        tb_sha_op[0*4 +: 4] = ISA_SHA_RELEASE;
        tb_sha_rs_val[0*64 +: 64] = 64'd0;
        tb_sha_imm8[0*8 +: 8] = 8'd0;
        tb_sha_done_count = 0;
        tb_csr_done_count = 0;
        tb_tred_done_count = 0;
        tb_legacy_done_count = 0;
        @(negedge clk);
        tb_sha_req[0] = 1'b1;
        for (i = 0; i < 600; i = i + 1) begin
            @(negedge clk);
            if (uut.sha_done_reg && uut.sha_grant == 0 &&
                tb_sha_req[0]) begin
                tb_sha_done_count = tb_sha_done_count + 1;
                tb_sha_req[0] = 1'b0;
            end
            if (uut.mc_tile_csr_done[1] && tb_tile_csr_req[1]) begin
                if (tb_legacy_done_count < 4)
                    tb_legacy_order[tb_legacy_done_count] = 2'd1;
                tb_legacy_done_count = tb_legacy_done_count + 1;
                tb_csr_done_count = tb_csr_done_count + 1;
                tb_tile_csr_req[1] = 1'b0;
            end
            if (uut.mex_done_reg && uut.mex_grant == 3 &&
                tb_mex_req[3]) begin
                if (tb_legacy_done_count < 4)
                    tb_legacy_order[tb_legacy_done_count] = 2'd3;
                tb_legacy_done_count = tb_legacy_done_count + 1;
                tb_tred_done_count = tb_tred_done_count + 1;
                tb_mex_req[3] = 1'b0;
            end
            if (tb_sha_done_count == 1 &&
                tb_legacy_done_count == 2)
                i = 600;
        end
        @(negedge clk);
        tb_sha_req[0] = 1'b0;
        tb_tile_csr_req[1] = 1'b0;
        tb_mex_req[3] = 1'b0;
        repeat (5) @(negedge clk);
        check64("SHA RELEASE completes once",
                tb_sha_done_count, 64'd1);
        check64("SHA RELEASE is sole unlock",
                uut.sha_locked, 64'd0);
        check64("released ACC writer completes once",
                tb_csr_done_count, 64'd1);
        check64("released TRED completes once",
                tb_tred_done_count, 64'd1);
        check64("post-release first caller",
                tb_legacy_order[0], 64'd1);
        check64("post-release second caller",
                tb_legacy_order[1], 64'd3);
        check64("TRED updates authoritative ACC",
                uut.te_legacy_acc_state[0*64 +: 64], 64'd64);
        check64("ACC_ZERO clears granted caller only",
                uut.cfg_tctrl[3], 64'd0);
        check64("ACC_ZERO preserves sibling shadow",
                uut.cfg_tctrl[1], 64'd2);
        check64("TRED sampled caller 3 config",
                uut.u_tile.tsrc0, 64'h00C0);

        // -----------------------------------------------------------------
        // Test 15: SHA.ROUND captures the granted caller's private TSRC0.
        // Resetting that caller after the outer bus captures a read must drain
        // and discard its response before a normal microcore bus request can
        // take ownership.
        // -----------------------------------------------------------------
        drive_private_tile_csr(1, CSR_TSRC0, 64'h0100);
        drive_private_tile_csr(2, CSR_TSRC0, 64'h0300);
        mem[12'h380] = 8'h11; mem[12'h381] = 8'h22;
        mem[12'h382] = 8'h33; mem[12'h383] = 8'h44;
        mem[12'h384] = 8'h55; mem[12'h385] = 8'h66;
        mem[12'h386] = 8'h77; mem[12'h387] = 8'h88;
        force uut.bus_ready = 1'b0;
        tb_sha_op[1*4 +: 4] = ISA_SHA_ROUND;
        tb_sha_rs_val[1*64 +: 64] = 64'd0;
        tb_sha_imm8[1*8 +: 8] = 8'd0;
        tb_wait_seen = 0;
        @(negedge clk);
        tb_sha_req[1] = 1'b1;
        for (i = 0; i < 200; i = i + 1) begin
            @(negedge clk);
            if (uut.sha_state == uut.SHA_LOAD &&
                uut.sha_bus_pending) begin
                tb_wait_seen = 1;
                i = 200;
            end
        end
        if (!tb_wait_seen) begin
            $display("FAIL [SHA captured-source load timeout]");
            fail_count = fail_count + 1;
        end
        check64("SHA samples granted private TSRC0",
                uut.sha_tsrc0_reg, 64'h0100);
        check64("SHA load uses captured TSRC0",
                bus_addr, 64'h0100);

        // A sibling can change its own shadow while the load is held without
        // perturbing the captured SHA source.
        drive_private_tile_csr(2, CSR_TSRC0, 64'h0380);
        check64("sibling TSRC0 write remains private",
                uut.cfg_tsrc0[2], 64'h0380);
        check64("active SHA source remains captured",
                uut.sha_tsrc0_reg, 64'h0100);

        tb_cancel_done_count = 0;
        @(negedge clk);
        micro_reset[1] = 1'b1;
        repeat (2) begin
            @(negedge clk);
            if (uut.sha_done_reg)
                tb_cancel_done_count = tb_cancel_done_count + 1;
        end
        tb_sha_req[1] = 1'b0;
        micro_reset[1] = 1'b0;
        repeat (2) begin
            @(negedge clk);
            if (uut.sha_done_reg)
                tb_cancel_done_count = tb_cancel_done_count + 1;
        end
        check64("cancelled SHA enters response drain",
                uut.sha_state, uut.SHA_DRAIN);
        check64("cancelled SHA retains bus ownership",
                {62'd0, uut.sha_bus_active, uut.sha_bus_pending}, 64'd3);
        check64("SHA drain holds captured address",
                bus_addr, 64'h0100);

        // Hold a normal microcore request behind the canceled SHA response.
        // If SHA released the port early, the stale response below would be
        // routed to this request.
        force uut.mc_bus_valid = 4'b0100;
        force uut.mc_bus_addr =
            {64'd0, 64'h0000_0000_0000_0380, 128'd0};
        force uut.mc_bus_wdata = 256'd0;
        force uut.mc_bus_wen = 4'b0000;
        force uut.mc_bus_size = {2'd0, BUS_DWORD, 4'd0};
        repeat (2) @(negedge clk);
        check64("normal bus waits behind SHA drain",
                {62'd0, uut.arb_busy, uut.mc_bus_ready[2]}, 64'd0);
        check64("drain still presents canceled SHA request",
                {63'd0, bus_valid}, 64'd1);

        // Return the already-captured SHA response. It is consumed only as a
        // drain event and cannot acknowledge the waiting normal request.
        force uut.bus_rdata = 64'hDEAD_CAFE_55AA_1234;
        force uut.bus_ready = 1'b1;
        @(posedge clk);
        #1;
        check64("stale SHA response not delivered to microcore",
                {63'd0, uut.mc_bus_ready[2]}, 64'd0);
        @(negedge clk);
        release uut.bus_ready;
        release uut.bus_rdata;
        repeat (2) @(negedge clk);
        check64("cancelled SHA has no late completion",
                tb_cancel_done_count, 64'd0);
        check64("cancelled SHA returns leaf idle",
                uut.sha_state, uut.SHA_IDLE);
        check64("drained SHA releases bus ownership",
                {62'd0, uut.sha_bus_active, uut.sha_bus_pending}, 64'd0);
        check64("cancelled SHA releases common turn",
                uut.legacy_state, uut.LEGACY_IDLE);
        check64("cancelled SHA preserves ACC",
                uut.te_legacy_acc_state[0*64 +: 64], 64'd64);

        // The still-held normal request now acquires the cluster port and
        // receives its own response rather than the discarded SHA data.
        tb_wait_seen = 0;
        for (i = 0; i < 100; i = i + 1) begin
            @(posedge clk);
            #1;
            if (uut.mc_bus_ready[2]) begin
                tb_wait_seen = 1;
                i = 100;
            end
        end
        if (!tb_wait_seen) begin
            $display("FAIL [normal bus request after SHA drain timeout]");
            fail_count = fail_count + 1;
        end
        check64("normal bus gets its own post-drain response",
                uut.mc_bus_rdata[2*64 +: 64],
                64'h1122_3344_5566_7788);
        release uut.mc_bus_valid;
        release uut.mc_bus_addr;
        release uut.mc_bus_wdata;
        release uut.mc_bus_wen;
        release uut.mc_bus_size;
        repeat (2) @(negedge clk);

        // A fresh caller proves cancellation left the common domain usable.
        tb_tile_csr_addr[2*8 +: 8] = CSR_ACC0;
        tb_tile_csr_wen[2] = 1'b0;
        tb_wait_seen = 0;
        @(negedge clk);
        tb_tile_csr_req[2] = 1'b1;
        for (i = 0; i < 100; i = i + 1) begin
            @(negedge clk);
            if (uut.mc_tile_csr_done[2]) begin
                tb_wait_seen = 1;
                i = 100;
            end
        end
        if (!tb_wait_seen) begin
            $display("FAIL [fresh ACC read after SHA cancellation timeout]");
            fail_count = fail_count + 1;
        end
        tb_tile_csr_req[2] = 1'b0;
        repeat (3) @(negedge clk);
        check64("fresh caller reads ACC after cancellation",
                uut.mc_tile_csr_rdata[2*64 +: 64], 64'd64);

        // -----------------------------------------------------------------
        // Test 16: cancellation and mutation share one terminal boundary.
        // Exercise the exact edge after a CSR/SHA write is prepared, restore
        // a MEX accumulator mutation that has already reached the leaf, and
        // prove a canceled compression job cannot complete as fresh work.
        // -----------------------------------------------------------------
        tb_mex_req = {N{1'b0}};
        tb_sha_req = {N{1'b0}};
        tb_tile_csr_req = {N{1'b0}};
        tb_tile_csr_wen = {N{1'b0}};
        tile_engine_reset = 1'b1;
        repeat (2) @(negedge clk);
        tile_engine_reset = 1'b0;
        repeat (2) @(negedge clk);

        // The cluster-shared physical engine executes TAMAC for its owning
        // microcore through the registered dispatch/retirement path.  Use a
        // simple all-lane product here; adversarial arithmetic remains covered
        // by the emulator-generated direct-engine vectors.
        drive_private_tile_csr(0, CSR_TMODE, TMODE_8);
        drive_private_tile_csr(0, CSR_TSRC0, 64'h0000_0000_0000_0100);
        drive_private_tile_csr(0, CSR_TSRC1, 64'h0000_0000_0000_0140);
        tile_mem_model[4] = {64{8'h03}};
        tile_mem_model[5] = {64{8'h05}};

        drive_private_mex(
            0, 2'd0, MEX_TSYS, ETSYS_TACC_TRY,
            {5'd0, ETSYS_TACC_TRY}, 4'd8, 1'b1,
            tb_private_mex_fault);
        check64("cluster owner TRY succeeds",
                tb_private_mex_fault, MEX_FAULT_NONE);
        drive_private_mex(
            0, 2'd0, MEX_TSYS, ETSYS_TACC_CLEAR,
            {5'd0, ETSYS_TACC_CLEAR}, 4'd8, 1'b1,
            tb_private_mex_fault);
        check64("cluster owner CLEAR succeeds",
                tb_private_mex_fault, MEX_FAULT_NONE);
        drive_private_mex(
            0, 2'd0, MEX_TMUL, TMUL_TAMAC,
            {5'd0, TMUL_TAMAC}, 4'd0, 1'b0,
            tb_private_mex_fault);
        check64("cluster owner integer TAMAC succeeds",
                tb_private_mex_fault, MEX_FAULT_NONE);
        check64("cluster TAMAC updates low U8 accumulator lanes",
                uut.u_tile.tacc_bank_state[63:0],
                64'h0000_000F_0000_000F);
        check64("cluster TAMAC updates high U8 accumulator lanes",
                uut.u_tile.tacc_bank_state[2047:1984],
                64'h0000_000F_0000_000F);
        check64("cluster TAMAC leaves shared bank valid and dirty",
                {62'd0,
                 uut.te_tacc_status_raw[TACC_STATUS_BIT_DIRTY],
                 uut.te_tacc_status_raw[TACC_STATUS_BIT_VALID]},
                64'd3);

        // Reformat the same owned physical bank and execute floating TAMAC
        // through the production cluster dispatch path.  Direct-engine
        // fixtures cover adversarial arithmetic; this case proves captured
        // FP mode, four arithmetic intervals, and registered retirement.
        drive_private_tile_csr(0, CSR_TMODE, TMODE_FP16);
        tile_mem_model[4] = {32{16'h3C00}}; // 1.0
        tile_mem_model[5] = {32{16'h4000}}; // 2.0
        drive_private_mex(
            0, 2'd0, MEX_TSYS, ETSYS_TACC_CLEAR,
            {5'd0, ETSYS_TACC_CLEAR}, 4'd8, 1'b1,
            tb_private_mex_fault);
        check64("cluster owner FP16 CLEAR succeeds",
                tb_private_mex_fault, MEX_FAULT_NONE);
        drive_private_mex(
            0, 2'd0, MEX_TMUL, TMUL_TAMAC,
            {5'd0, TMUL_TAMAC}, 4'd0, 1'b0,
            tb_private_mex_fault);
        check64("cluster owner FP16 TAMAC succeeds",
                tb_private_mex_fault, MEX_FAULT_NONE);
        check64("cluster FP16 TAMAC updates lane zero",
                uut.u_tile.tacc_bank_state[31:0],
                32'h4000_0000);
        check64("cluster FP16 TAMAC updates lane thirty-one",
                uut.u_tile.tacc_bank_state[31*32 +: 32],
                32'h4000_0000);
        check64("cluster FP16 TAMAC keeps inactive image half zero",
                uut.u_tile.tacc_bank_state[2047:1984],
                64'd0);

        drive_private_mex(
            0, 2'd0, MEX_TSYS, ETSYS_TACC_RELEASE,
            {5'd0, ETSYS_TACC_RELEASE}, 4'd8, 1'b1,
            tb_private_mex_fault);
        check64("cluster owner RELEASE restores FREE TACC",
                uut.te_tacc_status_raw,
                {43'd0, TACC_OWNER_NONE, 16'd0});

        // Cancel an ACC CSR while its ACTIVE terminal signals are present.
        tb_tile_csr_addr[2*8 +: 8] = CSR_ACC0;
        tb_tile_csr_wdata[2*64 +: 64] =
            64'hC5C5_0000_DEAD_BEEF;
        tb_tile_csr_wen[2] = 1'b1;
        @(negedge clk);
        tb_tile_csr_req[2] = 1'b1;
        tb_wait_seen = 0;
        for (i = 0; i < 100; i = i + 1) begin
            @(negedge clk);
            if (uut.legacy_state == uut.LEGACY_ACTIVE &&
                uut.legacy_kind == uut.LEGACY_KIND_CSR &&
                uut.legacy_grant == 2) begin
                tb_wait_seen = 1;
                i = 100;
            end
        end
        if (!tb_wait_seen) begin
            $display("FAIL [CSR terminal-cancel setup timeout]");
            fail_count = fail_count + 1;
        end
        micro_reset[2] = 1'b1;
        #1;
        check64("reset suppresses terminal CSR done",
                uut.mc_tile_csr_done[2], 64'd0);
        @(posedge clk);
        @(negedge clk);
        tb_tile_csr_req[2] = 1'b0;
        tb_tile_csr_wen[2] = 1'b0;
        micro_reset[2] = 1'b0;
        repeat (3) @(negedge clk);
        check64("cancelled CSR preserves ACC",
                uut.te_legacy_acc_state[0*64 +: 64], 64'd0);
        check64("cancelled CSR does not advance RR",
                uut.legacy_last, 64'd0);

        // Cancel SHA.INIT during SHA_DONE, before the prepared IV write and
        // ownership acquisition reach their common terminal edge.
        tb_sha_op[1*4 +: 4] = ISA_SHA_INIT;
        tb_sha_imm8[1*8 +: 8] = 8'd0;
        @(negedge clk);
        tb_sha_req[1] = 1'b1;
        tb_wait_seen = 0;
        for (i = 0; i < 100; i = i + 1) begin
            @(negedge clk);
            if (uut.sha_state == uut.SHA_DONE &&
                uut.sha_done_reg && uut.sha_grant == 1) begin
                tb_wait_seen = 1;
                i = 100;
            end
        end
        if (!tb_wait_seen) begin
            $display("FAIL [SHA terminal-cancel setup timeout]");
            fail_count = fail_count + 1;
        end
        micro_reset[1] = 1'b1;
        #1;
        check64("reset masks terminal SHA completion",
                uut.mc[1].u_micro.sha_done, 64'd0);
        @(posedge clk);
        @(negedge clk);
        tb_sha_req[1] = 1'b0;
        micro_reset[1] = 1'b0;
        repeat (3) @(negedge clk);
        check64("cancelled SHA INIT preserves ACC",
                uut.te_legacy_acc_state[0*64 +: 64], 64'd0);
        check64("cancelled SHA INIT preserves lock",
                uut.sha_locked, 64'd0);
        check64("cancelled SHA INIT preserves mode",
                uut.cl_sha_mode, 64'd0);
        check64("cancelled SHA INIT does not advance RR",
                uut.legacy_last, 64'd0);

        // Seed ACC, then cancel a reduction only after the tile leaf has
        // entered S_DONE with the mutated accumulator.  The common-domain
        // admission snapshot must be restored on the cancellation edge.
        tb_tile_csr_addr[0*8 +: 8] = CSR_ACC0;
        tb_tile_csr_wdata[0*64 +: 64] =
            64'h55AA_0000_0000_1234;
        tb_tile_csr_wen[0] = 1'b1;
        @(negedge clk);
        tb_tile_csr_req[0] = 1'b1;
        tb_wait_seen = 0;
        for (i = 0; i < 100; i = i + 1) begin
            @(negedge clk);
            if (uut.mc_tile_csr_done[0]) begin
                tb_wait_seen = 1;
                i = 100;
            end
        end
        if (!tb_wait_seen) begin
            $display("FAIL [ACC seed timeout]");
            fail_count = fail_count + 1;
        end
        tb_tile_csr_req[0] = 1'b0;
        tb_tile_csr_wen[0] = 1'b0;
        repeat (3) @(negedge clk);

        tile_mem_model[0] = {64{8'h01}};
        drive_private_tile_csr(2, CSR_TMODE, 64'd0);
        drive_private_tile_csr(2, CSR_TSRC0, 64'h0000);
        drive_private_tile_csr(2, CSR_TCTRL, 64'd0);
        tb_mex_ss[2*2 +: 2] = 2'd0;
        tb_mex_op[2*2 +: 2] = MEX_TRED;
        tb_mex_funct[2*3 +: 3] = TRED_SUM;
        tb_mex_funct_byte[2*8 +: 8] = {5'd0, TRED_SUM};
        @(negedge clk);
        tb_mex_req[2] = 1'b1;
        tb_wait_seen = 0;
        for (i = 0; i < 200; i = i + 1) begin
            @(negedge clk);
            if (uut.u_tile.state == uut.u_tile.S_DONE &&
                uut.te_legacy_acc_state[0*64 +: 64] == 64'd64) begin
                tb_wait_seen = 1;
                i = 200;
            end
        end
        if (!tb_wait_seen) begin
            $display("FAIL [post-mutation MEX cancel setup timeout]");
            fail_count = fail_count + 1;
        end
        micro_reset[2] = 1'b1;
        @(posedge clk);
        @(negedge clk);
        tb_mex_req[2] = 1'b0;
        micro_reset[2] = 1'b0;
        repeat (4) @(negedge clk);
        check64("cancelled MEX restores ACC snapshot",
                uut.te_legacy_acc_state[0*64 +: 64],
                64'h55AA_0000_0000_1234);
        check64("cancelled MEX returns common domain idle",
                uut.legacy_state, uut.LEGACY_IDLE);
        check64("cancelled MEX does not advance RR",
                uut.legacy_last, 64'd0);

        // Cancel a lifecycle instruction after the leaf has published its
        // response but on the edge where this arbiter would retire it.  The
        // leaf must keep the mutation staged until that edge, allowing the
        // caller epoch to suppress both retirement and the TRY claim.
        tb_mex_ss[0*2 +: 2] = 2'd0;
        tb_mex_op[0*2 +: 2] = MEX_TSYS;
        tb_mex_funct[0*3 +: 3] = ETSYS_TACC_TRY;
        tb_mex_funct_byte[0*8 +: 8] = {5'd0, ETSYS_TACC_TRY};
        tb_mex_ext_mod[0*4 +: 4] = 4'd8;
        tb_mex_ext_active[0] = 1'b1;
        tb_cancel_done_count = 0;
        @(negedge clk);
        tb_mex_req[0] = 1'b1;
        tb_wait_seen = 0;
        for (i = 0; i < 100; i = i + 1) begin
            @(negedge clk);
            if (uut.u_tile.u_tacc.req_done &&
                uut.mex_state == uut.MEX_ACTIVE) begin
                tb_wait_seen = 1;
                i = 100;
            end
        end
        if (!tb_wait_seen) begin
            $display("FAIL [post-publication TACC cancel setup timeout]");
            fail_count = fail_count + 1;
        end
        check64("published TACC response retains BUSY before commit",
                {62'd0,
                 uut.te_tacc_status_raw[TACC_STATUS_BIT_BUSY],
                 uut.te_tacc_status_raw[TACC_STATUS_BIT_CLAIMED]},
                64'd2);
        micro_reset[0] = 1'b1;
        @(posedge clk);
        #1;
        if (uut.mex_done_reg)
            tb_cancel_done_count = tb_cancel_done_count + 1;
        @(negedge clk);
        tb_mex_req[0] = 1'b0;
        micro_reset[0] = 1'b0;
        repeat (3) begin
            @(negedge clk);
            if (uut.mex_done_reg)
                tb_cancel_done_count = tb_cancel_done_count + 1;
        end
        check64("sampling-edge TACC cancel has no routed completion",
                tb_cancel_done_count, 64'd0);
        check64("sampling-edge TACC cancel suppresses TRY mutation",
                uut.te_tacc_status_raw,
                {43'd0, TACC_OWNER_NONE, 16'd0});
        check64("cancelled TACC returns both arbitration domains idle",
                {60'd0, uut.legacy_state, uut.mex_state},
                {60'd0, uut.LEGACY_IDLE, uut.MEX_IDLE});
        check64("cancelled TACC does not advance common RR",
                uut.legacy_last, 64'd0);

        // Repeat the claim, this time allowing the cluster to capture the leaf
        // response.  Cancel while registered mex_done is being delivered to
        // the microcore: this is the final retirement window, one edge later
        // than the leaf-publication case above.
        tb_cancel_done_count = 0;
        @(negedge clk);
        tb_mex_req[0] = 1'b1;
        tb_wait_seen = 0;
        for (i = 0; i < 100; i = i + 1) begin
            @(negedge clk);
            if (uut.mex_state == uut.MEX_WAIT_DROP &&
                uut.mex_done_reg &&
                uut.legacy_state == uut.LEGACY_ACTIVE) begin
                tb_wait_seen = 1;
                i = 100;
            end
        end
        if (!tb_wait_seen) begin
            $display("FAIL [registered-delivery TACC cancel setup timeout]");
            fail_count = fail_count + 1;
        end
        check64("registered TACC delivery retains staged state",
                {62'd0,
                 uut.te_tacc_status_raw[TACC_STATUS_BIT_BUSY],
                 uut.te_tacc_status_raw[TACC_STATUS_BIT_CLAIMED]},
                64'd2);
        micro_reset[0] = 1'b1;
        @(posedge clk);
        #1;
        if (uut.mex_done_reg)
            tb_cancel_done_count = tb_cancel_done_count + 1;
        @(negedge clk);
        tb_mex_req[0] = 1'b0;
        micro_reset[0] = 1'b0;
        repeat (3) begin
            @(negedge clk);
            if (uut.mex_done_reg)
                tb_cancel_done_count = tb_cancel_done_count + 1;
        end
        check64("retirement-edge TACC cancel has no late completion",
                tb_cancel_done_count, 64'd0);
        check64("retirement-edge TACC cancel suppresses TRY mutation",
                uut.te_tacc_status_raw,
                {43'd0, TACC_OWNER_NONE, 16'd0});
        check64("retirement-edge cancel releases TACC arbitration",
                {60'd0, uut.legacy_state, uut.mex_state},
                {60'd0, uut.LEGACY_IDLE, uut.MEX_IDLE});
        check64("retirement-edge TACC cancel does not advance RR",
                uut.legacy_last, 64'd0);

        // Cancel a live compression child, then run a fresh ROUND.  The child
        // must reset rather than publishing its stale digest as fresh work.
        drive_private_tile_csr(1, CSR_TSRC0, 64'h0200);
        drive_private_tile_csr(2, CSR_TSRC0, 64'h0300);
        for (i = 0; i < 128; i = i + 1) begin
            mem[12'h200 + i] = i[7:0];
            mem[12'h300 + i] = (8'hFF - i[7:0]);
        end
        tb_sha_op[1*4 +: 4] = ISA_SHA_ROUND;
        @(negedge clk);
        tb_sha_req[1] = 1'b1;
        tb_wait_seen = 0;
        for (i = 0; i < 300; i = i + 1) begin
            @(negedge clk);
            if (uut.sha_state == uut.SHA_COMPRESS &&
                uut.sha_eng_busy) begin
                tb_wait_seen = 1;
                i = 300;
            end
        end
        if (!tb_wait_seen) begin
            $display("FAIL [SHA compression-cancel setup timeout]");
            fail_count = fail_count + 1;
        end
        micro_reset[1] = 1'b1;
        @(posedge clk);
        @(negedge clk);
        tb_sha_req[1] = 1'b0;
        micro_reset[1] = 1'b0;
        repeat (3) @(negedge clk);
        check64("cancel resets SHA compression child",
                uut.sha_eng_busy, 64'd0);
        check64("cancelled compression releases common domain",
                uut.legacy_state, uut.LEGACY_IDLE);

        tb_sha_op[2*4 +: 4] = ISA_SHA_ROUND;
        tb_sha_done_count = 0;
        tb_wait_seen = 0;
        @(negedge clk);
        tb_sha_req[2] = 1'b1;
        for (i = 0; i < 400; i = i + 1) begin
            @(negedge clk);
            if (uut.sha_state == uut.SHA_COMPRESS)
                tb_wait_seen = tb_wait_seen + 1;
            if (uut.sha_done_reg && uut.sha_grant == 2 &&
                tb_sha_req[2]) begin
                tb_sha_done_count = tb_sha_done_count + 1;
                tb_sha_req[2] = 1'b0;
                i = 400;
            end
        end
        check64("fresh ROUND completes once",
                tb_sha_done_count, 64'd1);
        if (tb_wait_seen >= 60)
            pass_count = pass_count + 1;
        else begin
            $display("FAIL [fresh ROUND completed too early]: cycles=%0d",
                     tb_wait_seen);
            fail_count = fail_count + 1;
        end
        repeat (5) @(negedge clk);
        check64("fresh ROUND leaves child idle",
                uut.sha_eng_busy, 64'd0);

        release uut.mc_mex_req;
        release uut.mc_mex_ss;
        release uut.mc_mex_op;
        release uut.mc_mex_funct;
        release uut.mc_mex_funct_byte;
        release uut.mc_mex_gpr_val;
        release uut.mc_mex_imm8;
        release uut.mc_mex_ext_mod;
        release uut.mc_mex_ext_active;
        release uut.mc_tile_caller_id;
        release uut.mc_tile_priv;
        release uut.mc_tile_mpu_base;
        release uut.mc_tile_mpu_limit;
        release uut.mc_tile_mpu_enabled;
        release uut.mc_tile_allow_cluster_spad;
        release uut.mc_tacc_ctl_valid;
        release uut.mc_tacc_ctl_wdata;
        release uut.mc_sha_req;
        release uut.mc_sha_op;
        release uut.mc_sha_rs_val;
        release uut.mc_sha_imm8;
        release uut.mc_tile_csr_req;
        release uut.mc_tile_csr_wen;
        release uut.mc_tile_csr_addr;
        release uut.mc_tile_csr_wdata;

        // =================================================================
        $display("===========================================");
        if (fail_count == 0)
            $display("tb_cluster: ALL %0d TESTS PASSED", pass_count);
        else
            $display("tb_cluster: %0d PASSED, %0d FAILED", pass_count, fail_count);
        $display("===========================================");
        if (fail_count != 0)
            $fatal(1, "tb_cluster failed");
        $finish(0);
    end

    // Timeout watchdog
    initial begin
        #10000000;
        $display("TIMEOUT: tb_cluster");
        $fatal(1, "tb_cluster timeout");
    end

endmodule
