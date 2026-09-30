`timescale 1ns/1ps

// ============================================================================
// Scalar FPU regression
// ============================================================================
//
// Expected values come from shared/scalar_fp.py through gen_fpu_vectors.py.
// Each row starts one operation and checks the result, the flags, FCMP's
// FLAGS bits, the register-write flag, and the exact start-to-done latency.

module tb_fpu;
    reg         clk;
    reg         rst;
    reg         start;
    reg  [7:0]  op;
    reg  [63:0] rd_val;
    reg  [63:0] rs_val;
    reg  [63:0] rt_val;
    reg  [2:0]  rm_dyn;
    wire        busy;
    wire        done;
    wire [63:0] result;
    wire        write_rd;
    wire [4:0]  flags;
    wire [3:0]  cmp;

    mp64_fpu u_fpu (
        .clk     (clk),
        .rst     (rst),
        .start   (start),
        .op      (op),
        .rd_val  (rd_val),
        .rs_val  (rs_val),
        .rt_val  (rt_val),
        .rm_dyn  (rm_dyn),
        .busy    (busy),
        .done    (done),
        .result  (result),
        .write_rd(write_rd),
        .flags   (flags),
        .cmp     (cmp)
    );

    always #5 clk = ~clk;

    integer vector_fd;
    integer vector_scan;
    integer vector_count;
    integer fail_count;
    integer cycles;
    reg [2047:0] vector_line;
    reg [255:0]  vector_name;
    reg [7:0]    v_op;
    reg [63:0]   v_rd;
    reg [63:0]   v_rs;
    reg [63:0]   v_rt;
    reg [3:0]    v_rm;
    reg [63:0]   v_expected;
    reg [7:0]    v_flags;
    reg [3:0]    v_cmp;
    integer      v_write;
    integer      v_latency;

    initial begin
        clk = 1'b0;
        rst = 1'b1;
        start = 1'b0;
        op = 8'd0;
        rd_val = 64'd0;
        rs_val = 64'd0;
        rt_val = 64'd0;
        rm_dyn = 3'd0;
        fail_count = 0;
        vector_count = 0;
        repeat (2) @(posedge clk);
        rst = 1'b0;

        vector_fd = $fopen("fpu_vectors.vec", "r");
        if (vector_fd == 0)
            $fatal(1, "cannot open fpu_vectors.vec");

        while (!$feof(vector_fd)) begin
            vector_line = {2048{1'b0}};
            vector_scan = $fgets(vector_line, vector_fd);
            vector_scan = $sscanf(
                vector_line, "%s %h %h %h %h %h %h %h %h %d %d",
                vector_name, v_op, v_rd, v_rs, v_rt, v_rm, v_expected,
                v_flags, v_cmp, v_write, v_latency);
            if (vector_scan == 11) begin
                vector_count = vector_count + 1;
                @(negedge clk);
                op     = v_op;
                rd_val = v_rd;
                rs_val = v_rs;
                rt_val = v_rt;
                rm_dyn = v_rm[2:0];
                start  = 1'b1;
                @(posedge clk);      // the unit latches the operation here
                #1 start = 1'b0;
                cycles = 0;
                while (!done && cycles < 64) begin
                    @(posedge clk);
                    #1 cycles = cycles + 1;
                end
                if (cycles != v_latency) begin
                    fail_count = fail_count + 1;
                    $display("  FAIL: %0s latency %0d, expected %0d",
                             vector_name, cycles, v_latency);
                end
                if (write_rd !== v_write[0]) begin
                    fail_count = fail_count + 1;
                    $display("  FAIL: %0s write %0d, expected %0d",
                             vector_name, write_rd, v_write);
                end
                if (v_write != 0 && result !== v_expected) begin
                    fail_count = fail_count + 1;
                    $display("  FAIL: %0s result %016x, expected %016x",
                             vector_name, result, v_expected);
                end
                if (flags !== v_flags[4:0]) begin
                    fail_count = fail_count + 1;
                    $display("  FAIL: %0s flags %02x, expected %02x",
                             vector_name, flags, v_flags);
                end
                if (v_write == 0 && cmp !== v_cmp) begin
                    fail_count = fail_count + 1;
                    $display("  FAIL: %0s cmp %x, expected %x",
                             vector_name, cmp, v_cmp);
                end
            end
        end
        $fclose(vector_fd);

        if (vector_count < 3000) begin
            fail_count = fail_count + 1;
            $display("  FAIL: executed only %0d vectors", vector_count);
        end

        $display("FPU: %0d vectors, %0d failures", vector_count, fail_count);
        if (fail_count != 0)
            $fatal(1, "tb_fpu failed");
        $display("ALL FPU VECTORS PASSED");
        $finish;
    end
endmodule
