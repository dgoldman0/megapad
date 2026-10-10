// ============================================================================
// tb_rtc.v — RTC wall-clock set, validity and single-effect bus accesses
// ============================================================================
//
// The bus model holds REQ until it observes the RTC's registered ACK, as the
// system bus does, so every access spans two clock edges.  A small CLOCK_HZ
// makes one millisecond 20 cycles, so accesses can be aimed at a tick edge.

`timescale 1ns/1ps

module tb_rtc;

    localparam integer CLOCK_HZ = 20_000;
    localparam integer DIV      = CLOCK_HZ / 1000;

    reg        clk = 1'b0;
    reg        rst_n = 1'b0;
    reg        req = 1'b0;
    reg  [4:0] addr = 5'd0;
    reg  [7:0] wdata = 8'd0;
    reg        wen = 1'b0;
    wire [7:0] rdata;
    wire       ack;
    wire       irq;
    wire [63:0] uptime;

    mp64_rtc #(.CLOCK_HZ(CLOCK_HZ)) uut (
        .clk(clk), .rst_n(rst_n),
        .req(req), .addr(addr), .wdata(wdata), .wen(wen),
        .rdata(rdata), .ack(ack), .irq(irq), .uptime(uptime)
    );

    always #5 clk = ~clk;

    integer failures = 0;
    integer checks = 0;

    task check(input [8*48-1:0] name, input [63:0] got, input [63:0] want);
        begin
            checks = checks + 1;
            if (got !== want) begin
                failures = failures + 1;
                $display("FAIL %0s: got %h want %h", name, got, want);
            end
        end
    endtask

    // Start an access just after a clock edge; it is accepted at the next
    // edge, and REQ stays high through the edge that observes ACK.
    task begin_access(input w, input [4:0] a, input [7:0] d);
        begin
            req = 1'b1; wen = w; addr = a; wdata = d;
        end
    endtask

    task finish_access(output [7:0] q);
        begin
            @(posedge clk); #1;          // edge 1: accepted, ACK registered
            q = rdata;
            @(posedge clk); #1;          // edge 2: REQ still high, ACK seen
            req = 1'b0; wen = 1'b0;
        end
    endtask

    task bus_write(input [4:0] a, input [7:0] d);
        reg [7:0] ignored;
        begin
            @(posedge clk); #1;
            begin_access(1'b1, a, d);
            finish_access(ignored);
        end
    endtask

    task bus_read(input [4:0] a, output [7:0] q);
        begin
            @(posedge clk); #1;
            begin_access(1'b0, a, 8'd0);
            finish_access(q);
        end
    endtask

    // Return just after an edge that leaves one_ms_tick high, so an access
    // started now is accepted on the tick edge itself.
    task align_to_tick;
        begin
            @(posedge clk); #1;
            while (uut.ms_prescaler != DIV - 1) begin
                @(posedge clk); #1;
            end
        end
    endtask

    task wait_ms(input integer ms);
        begin
            repeat (ms * DIV) @(posedge clk);
            #1;
        end
    endtask

    reg [7:0]  q;
    reg [63:0] value;
    reg [63:0] earlier;
    integer    i;

    initial begin
        repeat (3) @(posedge clk);
        rst_n = 1'b1;
        #1;

        // -- Reset: the clock runs, and the wall clock is not yet valid --
        bus_read(5'h18, q);
        check("CTRL runs from reset", q, 8'h01);
        bus_read(5'h1D, q);
        check("EPOCH_STAT invalid after reset", q, 8'h00);
        earlier = uut.uptime_ms;
        wait_ms(3);
        check("uptime advances without a CTRL write",
              uut.uptime_ms - earlier >= 3, 1);

        // -- Staging: bytes 0-6 change nothing visible --
        value = 64'h0123_4567_89AB_CDEF;
        earlier = uut.epoch_ms;
        for (i = 0; i < 7; i = i + 1)
            bus_write(5'h08 + i, value[i*8 +: 8]);
        check("staged bytes leave the epoch counting",
              uut.epoch_ms - earlier < 5, 1);
        bus_read(5'h1D, q);
        check("EPOCH_STAT still invalid while staged", q, 8'h00);

        // -- Commit: byte 7 publishes all eight bytes and sets VALID --
        @(posedge clk); #1;
        while (uut.ms_prescaler == DIV - 1) begin
            @(posedge clk); #1;
        end
        begin_access(1'b1, 5'h0F, value[63:56]);
        @(posedge clk); #1;
        check("commit publishes the staged value", uut.epoch_ms, value);
        @(posedge clk); #1;
        req = 1'b0; wen = 1'b0;
        bus_read(5'h1D, q);
        check("EPOCH_STAT valid after a commit", q, 8'h01);

        // -- Commit on a tick edge keeps both the write and the tick --
        value = 64'h0000_0199_0000_0000;
        for (i = 0; i < 7; i = i + 1)
            bus_write(5'h08 + i, value[i*8 +: 8]);
        earlier = uut.uptime_ms;
        align_to_tick;
        begin_access(1'b1, 5'h0F, value[63:56]);
        @(posedge clk); #1;
        check("tick-edge commit counts the tick", uut.epoch_ms, value + 1);
        check("tick-edge commit keeps uptime", uut.uptime_ms, earlier + 1);
        @(posedge clk); #1;
        check("second REQ edge does not commit again",
              uut.epoch_ms, value + 1);
        req = 1'b0; wen = 1'b0;

        // -- A read latches once: the second REQ edge must not re-latch --
        align_to_tick;
        begin_access(1'b0, 5'h08, 8'd0);
        finish_access(q);
        check("epoch latch matches the byte-0 read",
              {56'd0, uut.epoch_latch[7:0]}, {56'd0, q});
        align_to_tick;
        begin_access(1'b0, 5'h00, 8'd0);
        finish_access(q);
        check("uptime latch matches the byte-0 read",
              {56'd0, uut.uptime_latch[7:0]}, {56'd0, q});

        // -- EPOCH_STAT is read-only --
        bus_write(5'h1D, 8'h00);
        bus_read(5'h1D, q);
        check("EPOCH_STAT ignores writes", q, 8'h01);

        // -- Reset clears VALID and the stage --
        rst_n = 1'b0;
        @(posedge clk); #1;
        rst_n = 1'b1;
        bus_read(5'h1D, q);
        check("reset clears VALID", q, 8'h00);
        check("reset clears the stage", {8'd0, uut.epoch_stage}, 64'd0);

        if (failures == 0)
            $display("tb_rtc: %0d checks passed", checks);
        else
            $display("tb_rtc: %0d of %0d checks FAILED", failures, checks);
        $finish;
    end

endmodule
