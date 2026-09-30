// ============================================================================
// tb_cpu_fp.v — full-core EXT.FP program against the Python emulator
// ============================================================================
//
// gen_cpu_fp_program.py assembles a program that runs every FC operation
// through the ordinary fetch, decode, FPU, and writeback path, stores each
// result and FPCSR (FLAGS for FCMP) to a buffer, SKIPs over FC instructions,
// and resumes three trapping encodings through an RTI handler.  The expected
// registers, FPCSR, FLAGS, and buffer come from the Python emulator.

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_cpu_fp;

    // ========================================================================
    // Clock / Reset
    // ========================================================================
    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;

    // ========================================================================
    // Simple 1-cycle-latency memory (64 KiB)
    // ========================================================================
    reg [7:0] mem [0:65535];

    wire        bus_valid;
    wire [63:0] bus_addr;
    wire [63:0] bus_wdata;
    wire        bus_wen;
    wire [1:0]  bus_size;
    reg  [63:0] bus_rdata;
    reg         bus_ready;

    // Each request is acknowledged once: a request still held in the cycle
    // that sees its ready is not written again, so the ready cannot leak
    // into the next request (the trap frame's back-to-back pushes).
    always @(posedge clk) begin
        bus_ready <= 1'b0;
        bus_rdata <= 64'd0;
        if (bus_valid && !bus_ready) begin
            bus_ready <= 1'b1;
            if (bus_wen) begin
                case (bus_size)
                    BUS_BYTE:  mem[bus_addr[15:0]] <= bus_wdata[7:0];
                    BUS_HALF: begin
                        mem[bus_addr[15:0]]   <= bus_wdata[7:0];
                        mem[bus_addr[15:0]+1] <= bus_wdata[15:8];
                    end
                    BUS_WORD: begin
                        mem[bus_addr[15:0]]   <= bus_wdata[7:0];
                        mem[bus_addr[15:0]+1] <= bus_wdata[15:8];
                        mem[bus_addr[15:0]+2] <= bus_wdata[23:16];
                        mem[bus_addr[15:0]+3] <= bus_wdata[31:24];
                    end
                    BUS_DWORD: begin
                        mem[bus_addr[15:0]]   <= bus_wdata[7:0];
                        mem[bus_addr[15:0]+1] <= bus_wdata[15:8];
                        mem[bus_addr[15:0]+2] <= bus_wdata[23:16];
                        mem[bus_addr[15:0]+3] <= bus_wdata[31:24];
                        mem[bus_addr[15:0]+4] <= bus_wdata[39:32];
                        mem[bus_addr[15:0]+5] <= bus_wdata[47:40];
                        mem[bus_addr[15:0]+6] <= bus_wdata[55:48];
                        mem[bus_addr[15:0]+7] <= bus_wdata[63:56];
                    end
                endcase
            end else begin
                case (bus_size)
                    BUS_BYTE:
                        bus_rdata <= {56'd0, mem[bus_addr[15:0]]};
                    BUS_HALF:
                        bus_rdata <= {48'd0, mem[bus_addr[15:0]+1],
                                             mem[bus_addr[15:0]]};
                    BUS_WORD:
                        bus_rdata <= {32'd0, mem[bus_addr[15:0]+3],
                                             mem[bus_addr[15:0]+2],
                                             mem[bus_addr[15:0]+1],
                                             mem[bus_addr[15:0]]};
                    BUS_DWORD:
                        bus_rdata <= {mem[bus_addr[15:0]+7],
                                      mem[bus_addr[15:0]+6],
                                      mem[bus_addr[15:0]+5],
                                      mem[bus_addr[15:0]+4],
                                      mem[bus_addr[15:0]+3],
                                      mem[bus_addr[15:0]+2],
                                      mem[bus_addr[15:0]+1],
                                      mem[bus_addr[15:0]]};
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
    wire        ic_enabled, ic_inv_all, ic_inv_line;
    wire [6:0]  ic_inv_size;

    wire        ic_bus_valid;
    wire [63:0] ic_bus_addr;
    reg  [63:0] ic_bus_rdata;
    reg         ic_bus_ready;
    reg         hold_icache_ready;
    reg         irq_timer_tb;

    // I-cache memory port (read-only 1-cycle latency from same mem[])
    always @(posedge clk) begin
        ic_bus_ready <= 1'b0;
        ic_bus_rdata <= 64'd0;
        if (ic_bus_valid && !hold_icache_ready) begin
            ic_bus_ready <= 1'b1;
            ic_bus_rdata <= {mem[ic_bus_addr[15:0]+7],
                             mem[ic_bus_addr[15:0]+6],
                             mem[ic_bus_addr[15:0]+5],
                             mem[ic_bus_addr[15:0]+4],
                             mem[ic_bus_addr[15:0]+3],
                             mem[ic_bus_addr[15:0]+2],
                             mem[ic_bus_addr[15:0]+1],
                             mem[ic_bus_addr[15:0]]};
        end
    end

    mp64_icache u_icache (
        .clk        (clk),
        .rst        (~rst_n),
        .enabled    (ic_enabled),
        .fetch_addr (ic_fetch_addr),
        .fetch_valid(ic_fetch_req),
        .fetch_data (ic_fetch_data),
        .fetch_hit  (ic_fetch_hit),
        .fetch_stall(ic_fetch_stall),
        .inv_all    (ic_inv_all),
        .inv_line   (ic_inv_line),
        .inv_addr   (ic_inv_addr),
        .inv_size   (ic_inv_size),
        .bus_valid  (ic_bus_valid),
        .bus_addr   (ic_bus_addr),
        .bus_rdata  (ic_bus_rdata),
        .bus_ready  (ic_bus_ready),
        .bus_error  (1'b0)
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
        .icache_error   (1'b0),
        .icache_error_addr(64'd0),
        .icache_enabled (ic_enabled),
        .icache_inv_all (ic_inv_all),
        .icache_inv_line(ic_inv_line),
        .icache_inv_addr(ic_inv_addr),
        .icache_inv_size(ic_inv_size),

        .bus_valid (bus_valid),
        .bus_addr  (bus_addr),
        .bus_wdata (bus_wdata),
        .bus_wen   (bus_wen),
        .bus_size  (bus_size),
        .bus_rdata (bus_rdata),
        .bus_ready (bus_ready),
        .bus_error (1'b0),
        .csr_wen   (csr_wen_w),
        .csr_addr  (csr_addr_w),
        .csr_wdata (csr_wdata_w),
        .csr_rdata (64'd0),
        .legacy_acc_state(256'd0),
        .legacy_acc_wen(),
        .legacy_acc_wdata(),
        .mex_valid (mex_valid_w),
        .mex_ss    (mex_ss_w),
        .mex_op    (mex_op_w),
        .mex_funct (mex_funct_w),
        .mex_funct_byte(),
        .mex_gpr_val(mex_gpr_val_w),
        .mex_imm8  (mex_imm8_w),
        .mex_ext_mod(),
        .mex_ext_active(),
        .mex_done  (1'b0),
        .mex_zero_valid(1'b0),
        .mex_zero(1'b0),
        .mex_busy  (1'b0),
        .mex_fault (MEX_FAULT_NONE),
        .mex_fault_addr(64'd0),
        .mex_stall_cycle(1'b0),
        .perf_extmem_word(1'b0),
        .tile_caller_id(),
        .tile_priv (),
        .tile_mpu_base(),
        .tile_mpu_limit(),
        .tile_mpu_enabled(),
        .tile_allow_cluster_spad(),
        .tacc_status(64'h0000_0000_001F_0000),
        .tacc_ctl_valid(),
        .tacc_ctl_wdata(),
        .tacc_ctl_done(1'b1),
        .tacc_ctl_fault(MEX_FAULT_NONE),
        .irq_timer (irq_timer_tb),
        .irq_uart  (1'b0),
        .irq_nic   (1'b0),
        .irq_ipi   (1'b0),
        .ef_flags  (ef_in)
    );

    // ========================================================================
    // Program and expected state
    // ========================================================================
    reg [63:0] expected [0:1023];
    integer i;
    integer fail_count;
    integer checked;
    integer cycles;
    reg [63:0] word;

    initial begin
        fail_count = 0;
        checked = 0;
        rst_n = 0;
        ef_in = 4'b0000;
        hold_icache_ready = 1'b0;
        irq_timer_tb = 1'b0;
        for (i = 0; i < 65536; i = i + 1)
            mem[i] = 8'h00;
        $readmemh("cpu_fp_program.hex", mem);
        $readmemh("cpu_fp_expected.hex", expected);
        #20;
        rst_n = 1;
        cycles = 0;
        while (uut.cpu_state != CPU_HALT && cycles < 400000) begin
            @(posedge clk);
            cycles = cycles + 1;
        end
        if (uut.cpu_state != CPU_HALT) begin
            fail_count = fail_count + 1;
            $display("  FAIL: program did not halt");
        end

        for (i = 0; i < 32; i = i + 1) begin
            checked = checked + 1;
            if (uut.R[i] !== expected[i]) begin
                fail_count = fail_count + 1;
                $display("  FAIL: R%0d = %016x, expected %016x",
                         i, uut.R[i], expected[i]);
            end
        end
        checked = checked + 2;
        if ({55'd0, uut.fpcsr} !== expected[32]) begin
            fail_count = fail_count + 1;
            $display("  FAIL: FPCSR = %03x, expected %03x",
                     uut.fpcsr, expected[32]);
        end
        if ({56'd0, uut.flags} !== expected[33]) begin
            fail_count = fail_count + 1;
            $display("  FAIL: FLAGS = %02x, expected %02x",
                     uut.flags, expected[33]);
        end
        if (expected[34] < 300) begin
            fail_count = fail_count + 1;
            $display("  FAIL: only %0d result words", expected[34]);
        end
        for (i = 0; i < expected[34]; i = i + 1) begin
            word = {mem[16'h8000 + 8*i + 7], mem[16'h8000 + 8*i + 6],
                    mem[16'h8000 + 8*i + 5], mem[16'h8000 + 8*i + 4],
                    mem[16'h8000 + 8*i + 3], mem[16'h8000 + 8*i + 2],
                    mem[16'h8000 + 8*i + 1], mem[16'h8000 + 8*i]};
            checked = checked + 1;
            if (word !== expected[35 + i]) begin
                fail_count = fail_count + 1;
                if (fail_count < 40)
                    $display("  FAIL: result %0d = %016x, expected %016x",
                             i, word, expected[35 + i]);
            end
        end

        $display("CPU FP: %0d checks, %0d failures, %0d cycles",
                 checked, fail_count, cycles);
        if (fail_count != 0)
            $fatal(1, "tb_cpu_fp failed");
        $display("ALL CPU FP CHECKS PASSED");
        $finish;
    end

endmodule
