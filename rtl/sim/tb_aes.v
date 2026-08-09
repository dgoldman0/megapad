// AES regression retained when the former mixed crypto bench was replaced by
// the authoritative SHA3/Keccak contract bench.

`timescale 1ns / 1ps

module tb_aes;
    reg clk;
    reg rst_n;
    always #5 clk = ~clk;

    reg         req;
    reg [6:0]   addr;
    reg [63:0]  wdata;
    reg         wen;
    wire [63:0] rdata;
    wire        ack;
    wire        irq;

    integer pass_count;
    integer fail_count;
    integer cycles;
    reg [63:0] rd;
    reg [127:0] first_output;
    reg [127:0] second_output;

    mp64_aes dut (
        .clk(clk), .rst_n(rst_n), .req(req), .addr(addr),
        .wdata(wdata), .wen(wen), .rdata(rdata), .ack(ack), .irq(irq)
    );

    task check;
        input condition;
        input [8*80-1:0] label;
        begin
            if (condition) begin
                pass_count = pass_count + 1;
                $display("PASS: %0s", label);
            end else begin
                fail_count = fail_count + 1;
                $display("FAIL: %0s", label);
            end
        end
    endtask

    task write_reg;
        input [6:0] a;
        input [63:0] d;
        begin
            @(negedge clk);
            req = 1'b1; addr = a; wdata = d; wen = 1'b1;
            @(posedge clk); #1;
            check(ack, "AES write acknowledged");
            @(negedge clk);
            req = 1'b0; wen = 1'b0;
            @(posedge clk); #1;
        end
    endtask

    task read_reg;
        input [6:0] a;
        output [63:0] value;
        begin
            @(negedge clk);
            req = 1'b1; addr = a; wen = 1'b0;
            @(posedge clk); #1;
            check(ack, "AES read acknowledged");
            value = rdata;
            @(negedge clk);
            req = 1'b0;
            @(posedge clk); #1;
        end
    endtask

    task wait_done;
        begin
            cycles = 0;
            rd = 64'd0;
            while (!rd[1] && cycles < 80) begin
                read_reg(7'h39, rd);
                cycles = cycles + 1;
            end
            check(rd[1] && !rd[0], "AES operation reaches DONE");
        end
    endtask

    task read_block;
        output [127:0] value;
        reg [63:0] word_value;
        begin
            read_reg(7'h50, word_value); value[127:96] = word_value[31:0];
            read_reg(7'h54, word_value); value[95:64]  = word_value[31:0];
            read_reg(7'h58, word_value); value[63:32]  = word_value[31:0];
            read_reg(7'h5c, word_value); value[31:0]   = word_value[31:0];
        end
    endtask

    initial begin
        clk = 1'b0;
        rst_n = 1'b0;
        req = 1'b0;
        addr = 7'd0;
        wdata = 64'd0;
        wen = 1'b0;
        pass_count = 0;
        fail_count = 0;
        repeat (4) @(posedge clk);
        rst_n = 1'b1;
        repeat (2) @(posedge clk);

        write_reg(7'h00, 64'h00010203);
        write_reg(7'h04, 64'h04050607);
        write_reg(7'h08, 64'h08090a0b);
        write_reg(7'h0c, 64'h0c0d0e0f);
        write_reg(7'h10, 64'h10111213);
        write_reg(7'h14, 64'h14151617);
        write_reg(7'h18, 64'h18191a1b);
        write_reg(7'h1c, 64'h1c1d1e1f);
        write_reg(7'h20, 64'd0);
        write_reg(7'h24, 64'd0);
        write_reg(7'h28, 64'd0);
        write_reg(7'h38, 64'd1);
        wait_done();

        write_reg(7'h40, 64'h00112233);
        write_reg(7'h44, 64'h44556677);
        write_reg(7'h48, 64'h8899aabb);
        write_reg(7'h4c, 64'hccddeeff);
        wait_done();
        read_block(first_output);
        check(first_output != 128'd0 &&
              first_output != 128'h00112233445566778899aabbccddeeff,
              "AES-GCM CTR output is nontrivial");

        write_reg(7'h40, 64'hffffffff);
        write_reg(7'h44, 64'hffffffff);
        write_reg(7'h48, 64'hffffffff);
        write_reg(7'h4c, 64'hffffffff);
        wait_done();
        read_block(second_output);
        check(second_output != first_output,
              "different AES plaintext produces different output");

        $display("AES regression: %0d passed, %0d failed",
                 pass_count, fail_count);
        if (fail_count != 0)
            $fatal(1, "AES regression failures");
        $finish;
    end

    initial begin
        #500000;
        $fatal(1, "tb_aes timeout");
    end
endmodule
