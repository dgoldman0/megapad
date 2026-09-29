`timescale 1ns/1ps

// ============================================================================
// Multi-format FMA unit regression
// ============================================================================
//
// Expected values come from shared/ieee_fp.py through gen_fma_vectors.py.
// This bench deliberately does not reproduce the arithmetic.

module tb_fma_unit;
    reg         in64;
    reg         out64;
    reg  [63:0] a0;
    reg  [63:0] b0;
    reg  [63:0] c0;
    reg  [31:0] a1;
    reg  [31:0] b1;
    reg  [31:0] c1;
    wire [63:0] r0;
    wire [63:0] r1;

    integer vector_fd;
    integer vector_scan;
    integer vector_count;
    integer pass_count;
    integer fail_count;
    reg [2047:0] vector_line;
    reg [767:0]  vector_name;
    reg [7:0]    vector_mode;
    reg [63:0]   vector_a0;
    reg [63:0]   vector_b0;
    reg [63:0]   vector_c0;
    reg [31:0]   vector_a1;
    reg [31:0]   vector_b1;
    reg [31:0]   vector_c1;
    reg [63:0]   vector_r0;
    reg [63:0]   vector_r1;

    mp64_fma_unit u_unit (
        .in64 (in64),
        .out64(out64),
        .a0   (a0),
        .b0   (b0),
        .c0   (c0),
        .a1   (a1),
        .b1   (b1),
        .c1   (c1),
        .r0   (r0),
        .r1   (r1)
    );

    initial begin
        pass_count = 0;
        fail_count = 0;
        vector_count = 0;

        vector_fd = $fopen("fma_vectors.vec", "r");
        if (vector_fd == 0)
            $fatal(1, "cannot open fma_vectors.vec");

        while (!$feof(vector_fd)) begin
            vector_line = {2048{1'b0}};
            vector_scan = $fgets(vector_line, vector_fd);
            vector_scan = $sscanf(
                vector_line,
                "%s %c %h %h %h %h %h %h %h %h",
                vector_name,
                vector_mode,
                vector_a0,
                vector_b0,
                vector_c0,
                vector_a1,
                vector_b1,
                vector_c1,
                vector_r0,
                vector_r1);
            if (vector_scan == 10) begin
                vector_count = vector_count + 1;
                in64  = (vector_mode == "d");
                out64 = (vector_mode != "s");
                a0 = vector_a0;
                b0 = vector_b0;
                c0 = vector_c0;
                a1 = vector_a1;
                b1 = vector_b1;
                c1 = vector_c1;
                #1;
                if (r0 !== vector_r0) begin
                    fail_count = fail_count + 1;
                    $display("  FAIL: %0s lane 0 = %016x, expected %016x",
                             vector_name, r0, vector_r0);
                end else begin
                    pass_count = pass_count + 1;
                end
                if (r1 !== vector_r1) begin
                    fail_count = fail_count + 1;
                    $display("  FAIL: %0s lane 1 = %016x, expected %016x",
                             vector_name, r1, vector_r1);
                end else begin
                    pass_count = pass_count + 1;
                end
            end
        end
        $fclose(vector_fd);

        if (vector_count != 1055) begin
            fail_count = fail_count + 1;
            $display("  FAIL: executed %0d vectors, expected 1055",
                     vector_count);
        end

        $display("FMA unit: %0d passed, %0d failed", pass_count, fail_count);
        if (fail_count != 0)
            $fatal(1, "tb_fma_unit failed");
        $display("ALL FMA UNIT VECTORS PASSED");
        $finish;
    end
endmodule
