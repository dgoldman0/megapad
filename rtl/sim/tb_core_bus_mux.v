// ============================================================================
// tb_core_bus_mux.v — CPU data/I-cache response-ownership contract
// ============================================================================

`timescale 1ns / 1ps

module tb_core_bus_mux;

    reg clk;
    reg rst_n;

    reg        core_valid;
    reg [63:0] core_addr;
    reg [63:0] core_wdata;
    reg        core_wen;
    reg [1:0]  core_size;
    reg        core_port_io;

    reg        ic_valid;
    reg [63:0] ic_addr;
    reg        ic_wen;
    reg [1:0]  ic_size;

    wire        mux_valid;
    wire [63:0] mux_addr;
    wire [63:0] mux_wdata;
    wire        mux_wen;
    wire [1:0]  mux_size;
    wire        mux_port_io;

    reg  [63:0] bus_rdata;
    reg         bus_ready;
    wire [63:0] core_rdata;
    wire        core_ready;
    wire [63:0] ic_rdata;
    wire        ic_ready;

    integer pass_count;
    integer fail_count;

    mp64_core_bus_mux uut (
        .clk          (clk),
        .rst_n        (rst_n),
        .core_valid   (core_valid),
        .core_addr    (core_addr),
        .core_wdata   (core_wdata),
        .core_wen     (core_wen),
        .core_size    (core_size),
        .core_port_io (core_port_io),
        .ic_valid     (ic_valid),
        .ic_addr      (ic_addr),
        .ic_wen       (ic_wen),
        .ic_size      (ic_size),
        .mux_valid    (mux_valid),
        .mux_addr     (mux_addr),
        .mux_wdata    (mux_wdata),
        .mux_wen      (mux_wen),
        .mux_size     (mux_size),
        .mux_port_io  (mux_port_io),
        .bus_rdata    (bus_rdata),
        .bus_ready    (bus_ready),
        .core_rdata   (core_rdata),
        .core_ready   (core_ready),
        .ic_rdata     (ic_rdata),
        .ic_ready     (ic_ready)
    );

    initial clk = 1'b0;
    always #5 clk = ~clk;

    task check;
        input [255:0] label;
        input condition;
        begin
            if (condition) begin
                pass_count = pass_count + 1;
                $display("  PASS: %0s", label);
            end else begin
                fail_count = fail_count + 1;
                $display("  FAIL: %0s", label);
            end
        end
    endtask

    task clock;
        begin
            @(posedge clk);
            #1;
        end
    endtask

    initial begin
        pass_count = 0;
        fail_count = 0;
        rst_n = 1'b0;
        core_valid = 1'b0;
        core_addr = 64'd0;
        core_wdata = 64'd0;
        core_wen = 1'b0;
        core_size = 2'd0;
        core_port_io = 1'b0;
        ic_valid = 1'b0;
        ic_addr = 64'd0;
        ic_wen = 1'b0;
        ic_size = 2'd3;
        bus_rdata = 64'd0;
        bus_ready = 1'b0;

        repeat (2) clock;
        rst_n = 1'b1;
        clock;

        // Refill low owns the port and receives only its own response.
        ic_valid = 1'b1;
        ic_addr = 64'h1000;
        clock;
        check("refill-low selected", mux_valid && !mux_wen
              && mux_addr == 64'h1000);
        bus_rdata = 64'h1111_1111_1111_1111;
        bus_ready = 1'b1;
        #1;
        check("refill-low ready routed only to cache",
              ic_ready && !core_ready
              && ic_rdata == 64'h1111_1111_1111_1111);
        clock;
        bus_ready = 1'b0;
        ic_valid = 1'b0;

        // During the refill gap, an IRQ stack store can claim the port.
        core_valid = 1'b1;
        core_addr = 64'h2000;
        core_wdata = 64'hCAFE_BABE_DEAD_BEEF;
        core_wen = 1'b1;
        core_size = 2'd3;
        clock;

        // Refill-high arrives before the delayed store response. Ownership
        // must remain with the already selected store.
        ic_valid = 1'b1;
        ic_addr = 64'h1008;
        #1;
        check("delayed store keeps address ownership",
              mux_valid && mux_wen && mux_addr == 64'h2000);
        check("delayed store keeps payload ownership",
              mux_wdata == 64'hCAFE_BABE_DEAD_BEEF
              && mux_size == 2'd3);

        // Even if the CPU withdraws after presenting the request, the bridge
        // drains the captured transaction to its original owner.
        core_valid = 1'b0;
        bus_rdata = 64'h2222_2222_2222_2222;
        bus_ready = 1'b1;
        #1;
        check("store response cannot be reclassified as refill",
              core_ready && !ic_ready);
        clock;
        bus_ready = 1'b0;

        // The waiting refill-high request now becomes the next owner.
        clock;
        check("refill-high follows completed store",
              mux_valid && !mux_wen && mux_addr == 64'h1008);
        bus_rdata = 64'h3333_3333_3333_3333;
        bus_ready = 1'b1;
        #1;
        check("refill-high response routed only to cache",
              ic_ready && !core_ready
              && ic_rdata == 64'h3333_3333_3333_3333);
        clock;
        bus_ready = 1'b0;
        ic_valid = 1'b0;

        // Simultaneous new requests retain the local cache priority.
        core_valid = 1'b1;
        core_addr = 64'h4000;
        ic_valid = 1'b1;
        ic_addr = 64'h3000;
        clock;
        check("cache wins simultaneous idle-port contention",
              mux_addr == 64'h3000 && !mux_wen);

        $display("");
        if (fail_count == 0)
            $display("tb_core_bus_mux: ALL %0d assertions PASSED",
                     pass_count);
        else
            $display("tb_core_bus_mux: %0d PASSED, %0d FAILED",
                     pass_count, fail_count);
        $finish;
    end

endmodule
