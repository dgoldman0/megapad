// ============================================================================
// tb_bus_arbiter.v — Testbench for mp64_bus
// ============================================================================
//
// Negedge-driven methodology:
//   - Drive inputs on @(negedge clk)
//   - Check outputs on @(posedge clk); #1
//
// Tests:
//   1. Reset clears all outputs
//   2. Single port memory read (request → ack with rdata)
//   3. Single port memory write
//   4. MMIO read (address in MMIO range)
//   5. Round-robin fairness (two ports requesting simultaneously)
//   6. QoS weight CSR write & read-back
//   7. Bandwidth throttling (port throttled after limit hit)
//

`timescale 1ns/1ps

module tb_bus_arbiter;

    // ========================================================================
    // Parameters
    // ========================================================================
    localparam N_PORTS   = 4;
    localparam PORT_BITS = 3;
    localparam CLK_HALF  = 5;  // 100 MHz

    // ========================================================================
    // Signals
    // ========================================================================
    reg  clk, rst_n;

    reg  [N_PORTS-1:0]      cpu_valid;
    reg  [N_PORTS*64-1:0]   cpu_addr;
    reg  [N_PORTS*64-1:0]   cpu_wdata;
    reg  [N_PORTS-1:0]      cpu_wen;
    reg  [N_PORTS*2-1:0]    cpu_size;
    wire [N_PORTS*64-1:0]   cpu_rdata;
    wire [N_PORTS-1:0]      cpu_ready;

    wire        mem_req;
    wire [63:0] mem_addr;
    wire [63:0] mem_wdata;
    wire        mem_wen;
    wire [1:0]  mem_size;
    reg  [63:0] mem_rdata;
    reg         mem_ack;

    wire        mmio_req;
    wire [11:0] mmio_addr;
    wire [63:0] mmio_wdata;
    wire        mmio_wen;
    wire [1:0]  mmio_size;
    reg  [63:0] mmio_rdata;
    reg         mmio_ack;

    reg         qos_csr_wen;
    reg  [7:0]  qos_csr_addr;
    reg  [63:0] qos_csr_wdata;
    wire [63:0] qos_csr_rdata;

    // ========================================================================
    // Assertions
    // ========================================================================
    integer pass_count;
    integer fail_count;
    integer test_num;

    task check;
        input [255:0] label;
        input         cond;
        begin
            test_num = test_num + 1;
            if (cond) begin
                pass_count = pass_count + 1;
                $display("  [PASS %0d] %0s", test_num, label);
            end else begin
                fail_count = fail_count + 1;
                $display("  [FAIL %0d] %0s", test_num, label);
            end
        end
    endtask

    // ========================================================================
    // Clock
    // ========================================================================
    initial clk = 0;
    always #CLK_HALF clk = ~clk;

    // ========================================================================
    // DUT
    // ========================================================================
    mp64_bus #(
        .N_PORTS   (N_PORTS),
        .PORT_BITS (PORT_BITS)
    ) dut (
        .clk           (clk),
        .rst_n         (rst_n),
        .cpu_valid     (cpu_valid),
        .cpu_addr      (cpu_addr),
        .cpu_wdata     (cpu_wdata),
        .cpu_wen       (cpu_wen),
        .cpu_size      (cpu_size),
        .cpu_rdata     (cpu_rdata),
        .cpu_ready     (cpu_ready),
        .mem_req       (mem_req),
        .mem_addr      (mem_addr),
        .mem_wdata     (mem_wdata),
        .mem_wen       (mem_wen),
        .mem_size      (mem_size),
        .mem_rdata     (mem_rdata),
        .mem_ack       (mem_ack),
        .mmio_req      (mmio_req),
        .mmio_addr     (mmio_addr),
        .mmio_wdata    (mmio_wdata),
        .mmio_wen      (mmio_wen),
        .mmio_size     (mmio_size),
        .mmio_rdata    (mmio_rdata),
        .mmio_ack      (mmio_ack),
        .qos_csr_wen   (qos_csr_wen),
        .qos_csr_addr  (qos_csr_addr),
        .qos_csr_wdata (qos_csr_wdata),
        .qos_csr_rdata (qos_csr_rdata)
    );

    // ========================================================================
    // Helper tasks
    // ========================================================================

    task reset;
        begin
            rst_n         <= 1'b0;
            cpu_valid     <= {N_PORTS{1'b0}};
            cpu_addr      <= {(N_PORTS*64){1'b0}};
            cpu_wdata     <= {(N_PORTS*64){1'b0}};
            cpu_wen       <= {N_PORTS{1'b0}};
            cpu_size      <= {(N_PORTS*2){1'b0}};
            mem_rdata     <= 64'd0;
            mem_ack       <= 1'b0;
            mmio_rdata    <= 64'd0;
            mmio_ack      <= 1'b0;
            qos_csr_wen   <= 1'b0;
            qos_csr_addr  <= 8'd0;
            qos_csr_wdata <= 64'd0;
            @(negedge clk);
            @(negedge clk);
            @(negedge clk);
            rst_n <= 1'b1;
            @(negedge clk);
        end
    endtask

    // Drive a request on port `p`
    task drive_req;
        input [PORT_BITS-1:0] p;
        input [63:0]          addr;
        input [63:0]          wdata;
        input                 wen;
        input [1:0]           sz;
        begin
            cpu_valid[p]       <= 1'b1;
            cpu_addr[p*64 +: 64]  <= addr;
            cpu_wdata[p*64 +: 64] <= wdata;
            cpu_wen[p]         <= wen;
            cpu_size[p*2 +: 2] <= sz;
        end
    endtask

    // Clear a port's request
    task clear_req;
        input [PORT_BITS-1:0] p;
        begin
            cpu_valid[p]       <= 1'b0;
            cpu_wen[p]         <= 1'b0;
        end
    endtask

    // Wait for mem_req, supply rdata, ack
    task mem_respond;
        input [63:0] rdata;
        integer watchdog;
        begin
            watchdog = 0;
            while (!mem_req && watchdog < 50) begin
                @(negedge clk);
                watchdog = watchdog + 1;
            end
            @(negedge clk);
            mem_rdata <= rdata;
            mem_ack   <= 1'b1;
            @(negedge clk);
            mem_ack   <= 1'b0;
        end
    endtask

    // Wait for mmio_req, supply rdata, ack
    task mmio_respond;
        input [63:0] rdata;
        integer watchdog;
        begin
            watchdog = 0;
            while (!mmio_req && watchdog < 50) begin
                @(negedge clk);
                watchdog = watchdog + 1;
            end
            @(negedge clk);
            mmio_rdata <= rdata;
            mmio_ack   <= 1'b1;
            @(negedge clk);
            mmio_ack   <= 1'b0;
        end
    endtask

    // Wait for cpu_ready on port p
    task wait_ready;
        input [PORT_BITS-1:0] p;
        integer watchdog;
        begin
            watchdog = 0;
            while (!cpu_ready[p] && watchdog < 100) begin
                @(posedge clk); #1;
                watchdog = watchdog + 1;
            end
        end
    endtask

    // ========================================================================
    // Main test sequence
    // ========================================================================
    initial begin
        $dumpfile("tb_bus_arbiter.vcd");
        $dumpvars(0, tb_bus_arbiter);

        pass_count = 0;
        fail_count = 0;
        test_num   = 0;

        $display("=== tb_bus_arbiter ===");

        // ---- Test 1: Reset ----
        $display("--- Test 1: Reset ---");
        reset;
        @(posedge clk); #1;
        check("mem_req clear after reset",  mem_req  == 1'b0);
        check("mmio_req clear after reset", mmio_req == 1'b0);
        check("cpu_ready clear after reset",cpu_ready == {N_PORTS{1'b0}});

        // ---- Test 2: Single port memory read ----
        $display("--- Test 2: Memory read port 0 ---");
        @(negedge clk);
        drive_req(0, 64'h0000_0100, 64'd0, 1'b0, 2'd3);  // read addr 0x100, dword

        // Wait for arbiter to issue mem_req
        begin : blk_t2
            integer wd;
            wd = 0;
            while (!mem_req && wd < 20) begin
                @(posedge clk); #1;
                wd = wd + 1;
            end
        end
        check("mem_req asserted for read", mem_req == 1'b1);
        check("mem_addr correct",         mem_addr == 64'h0000_0100);
        check("mem_wen is 0 for read",    mem_wen  == 1'b0);

        // Respond from memory
        @(negedge clk);
        mem_rdata <= 64'hDEAD_BEEF_CAFE_BABE;
        mem_ack   <= 1'b1;
        @(negedge clk);
        mem_ack   <= 1'b0;

        // Wait for cpu_ready
        wait_ready(0);
        check("cpu_ready[0] asserted",    cpu_ready[0] == 1'b1);
        check("cpu_rdata correct",        cpu_rdata[0*64 +: 64] == 64'hDEAD_BEEF_CAFE_BABE);

        @(negedge clk);
        clear_req(0);
        @(negedge clk);

        // ---- Test 3: Single port memory write ----
        $display("--- Test 3: Memory write port 1 ---");
        @(negedge clk);
        drive_req(1, 64'h0000_0200, 64'h1234_5678_9ABC_DEF0, 1'b1, 2'd3);

        begin : blk_t3
            integer wd;
            wd = 0;
            while (!mem_req && wd < 20) begin
                @(posedge clk); #1;
                wd = wd + 1;
            end
        end
        check("mem_req for write",    mem_req    == 1'b1);
        check("mem_addr for write",   mem_addr   == 64'h0000_0200);
        check("mem_wen is 1",         mem_wen    == 1'b1);
        check("mem_wdata correct",    mem_wdata  == 64'h1234_5678_9ABC_DEF0);

        @(negedge clk);
        mem_ack <= 1'b1;
        @(negedge clk);
        mem_ack <= 1'b0;

        wait_ready(1);
        check("cpu_ready[1] for write", cpu_ready[1] == 1'b1);

        @(negedge clk);
        clear_req(1);
        @(negedge clk);

        // ---- Test 4: MMIO read ----
        $display("--- Test 4: MMIO read ---");
        @(negedge clk);
        // MMIO address: upper 32 bits = 0xFFFF_FF00, lower 12 bits = UART
        drive_req(0, 64'hFFFF_FF00_0000_0000, 64'd0, 1'b0, 2'd3);

        begin : blk_t4
            integer wd;
            wd = 0;
            while (!mmio_req && wd < 20) begin
                @(posedge clk); #1;
                wd = wd + 1;
            end
        end
        check("mmio_req asserted",       mmio_req  == 1'b1);
        check("mmio_addr is UART",       mmio_addr == 12'h000);
        check("mmio_wen is 0 for read",  mmio_wen  == 1'b0);

        @(negedge clk);
        mmio_rdata <= 64'h0000_0000_0000_0055;  // UART RX = 'U'
        mmio_ack   <= 1'b1;
        @(negedge clk);
        mmio_ack   <= 1'b0;

        wait_ready(0);
        check("mmio rdata correct", cpu_rdata[0*64 +: 64] == 64'h0000_0000_0000_0055);

        @(negedge clk);
        clear_req(0);
        @(negedge clk);

        // ---- Test 5: Round-robin (two ports) ----
        $display("--- Test 5: Round-robin fairness ---");
        @(negedge clk);
        // Both port 0 and port 1 request simultaneously
        drive_req(0, 64'h0000_0300, 64'd0, 1'b0, 2'd3);
        drive_req(1, 64'h0000_0400, 64'd0, 1'b0, 2'd3);

        // First grant should go to port 0 (or 1, depending on last_grant)
        begin : blk_t5a
            integer wd;
            wd = 0;
            while (!mem_req && wd < 20) begin
                @(posedge clk); #1;
                wd = wd + 1;
            end
        end
        check("first mem_req in RR", mem_req == 1'b1);

        // Remember which port was served first
        begin : blk_t5b
            reg first_was_0;
            first_was_0 = (mem_addr == 64'h0000_0300);
            check("one port served first", mem_addr == 64'h0000_0300 ||
                                           mem_addr == 64'h0000_0400);
            // Ack first
            @(negedge clk);
            mem_rdata <= 64'hAAAA;
            mem_ack   <= 1'b1;
            @(negedge clk);
            mem_ack   <= 1'b0;

            // Wait for the served port's ready
            if (first_was_0) begin
                wait_ready(0);
                @(negedge clk);
                clear_req(0);
            end else begin
                wait_ready(1);
                @(negedge clk);
                clear_req(1);
            end

            // Second port should now be served
            begin : blk_t5c
                integer wd2;
                wd2 = 0;
                while (!mem_req && wd2 < 20) begin
                    @(posedge clk); #1;
                    wd2 = wd2 + 1;
                end
            end

            if (first_was_0)
                check("second port served", mem_addr == 64'h0000_0400);
            else
                check("second port served", mem_addr == 64'h0000_0300);

            @(negedge clk);
            mem_rdata <= 64'hBBBB;
            mem_ack   <= 1'b1;
            @(negedge clk);
            mem_ack   <= 1'b0;

            if (first_was_0) begin
                wait_ready(1);
                @(negedge clk);
                clear_req(1);
            end else begin
                wait_ready(0);
                @(negedge clk);
                clear_req(0);
            end
        end
        @(negedge clk);

        // ---- Test 6: QoS weight CSR write & read-back ----
        $display("--- Test 6: QoS CSR ---");
        @(negedge clk);
        qos_csr_wen   <= 1'b1;
        qos_csr_addr  <= 8'h58;  // CSR_QOS_WEIGHT
        qos_csr_wdata <= 64'h04030201;  // port0=1, port1=2, port2=3, port3=4
        @(negedge clk);
        qos_csr_wen   <= 1'b0;
        @(posedge clk); #1;
        // Read back (CSR read is registered, need one more cycle)
        @(posedge clk); #1;
        check("QoS weight readback [7:0]",   qos_csr_rdata[7:0]   == 8'd1);
        check("QoS weight readback [15:8]",  qos_csr_rdata[15:8]  == 8'd2);
        check("QoS weight readback [23:16]", qos_csr_rdata[23:16] == 8'd3);
        check("QoS weight readback [31:24]", qos_csr_rdata[31:24] == 8'd4);

        // ---- Test 7: BW limit throttling ----
        $display("--- Test 7: BW throttling ---");
        // Set port 2 BW limit to 1 (only 1 transaction per epoch)
        @(negedge clk);
        qos_csr_wen   <= 1'b1;
        qos_csr_addr  <= 8'h59;  // CSR_QOS_BWLIMIT
        qos_csr_wdata <= 64'h0000_0001_0000_0000;  // port2 limit=1, others=0
        @(negedge clk);
        qos_csr_wen   <= 1'b0;
        @(negedge clk);

        // First transaction for port 2 — should work
        drive_req(2, 64'h0000_0500, 64'd0, 1'b0, 2'd3);
        begin : blk_t7a
            integer wd;
            wd = 0;
            while (!mem_req && wd < 20) begin
                @(posedge clk); #1;
                wd = wd + 1;
            end
        end
        check("port 2 first req passes", mem_req == 1'b1);
        @(negedge clk);
        mem_rdata <= 64'hCCCC;
        mem_ack   <= 1'b1;
        @(negedge clk);
        mem_ack   <= 1'b0;
        wait_ready(2);
        @(negedge clk);
        clear_req(2);
        @(negedge clk);

        // Second transaction for port 2 — should be throttled
        // Also issue port 3 request to verify port 3 is not throttled
        drive_req(2, 64'h0000_0600, 64'd0, 1'b0, 2'd3);
        drive_req(3, 64'h0000_0700, 64'd0, 1'b0, 2'd3);
        begin : blk_t7b
            integer wd;
            wd = 0;
            while (!mem_req && wd < 20) begin
                @(posedge clk); #1;
                wd = wd + 1;
            end
        end
        // Should serve port 3 since port 2 is throttled
        check("throttled port skipped", mem_addr == 64'h0000_0700);
        @(negedge clk);
        mem_rdata <= 64'hDDDD;
        mem_ack   <= 1'b1;
        @(negedge clk);
        mem_ack   <= 1'b0;
        wait_ready(3);
        @(negedge clk);
        clear_req(2);
        clear_req(3);
        @(negedge clk);

        // ================================================================
        // Summary
        // ================================================================
        $display("");
        $display("=== tb_bus_arbiter: %0d passed, %0d failed ===",
                 pass_count, fail_count);
        if (fail_count > 0)
            $display("*** FAILURES DETECTED ***");

        #100;
        $finish;
    end

    // Timeout watchdog
    initial begin
        #200000;
        $display("TIMEOUT");
        $finish;
    end

endmodule
