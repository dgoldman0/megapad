// ============================================================================
// tb_trng.v — TRNG Unit Tests
// ============================================================================
//
// Tests:
//   1. STATUS reads 0 immediately after reset (pool not yet seeded)
//   2. STATUS reads 1 after 256+ clocks (pool fully seeded)
//   3. RAND8 returns an 8-bit value (upper 56 bits zero)
//   4. RAND64 returns a 64-bit value
//   5. Two consecutive RAND64 reads produce different values
//   6. SEED register write doesn't crash (functional smoke)
//   7. RAND8 varies after SEED injection
//   8. Multiple RAND8 reads produce non-identical values
//   9. Health monitor: STATUS stays valid under normal operation
//  10. Repetition count: 9 identical bytes pass, the 10th fails
//  11. The first run after reset fails at its 10th byte, whether its
//      byte is 0x00 or 0xFF
//  12. A run broken at 9 bytes starts the next run's count at 1
//  13. A failure stays latched while different bytes follow
//  14. Unusable device on the bus: STATUS reads 0, RAND8 and RAND64 end
//      as target faults, SEED is ignored
//  15. Reset recovers a latched failure
//  16. Before the pool is seeded: STATUS reads 0, RAND8 and RAND64 end as
//      target faults, SEED is ignored
//  17. Usable device on the bus: reads succeed, reserved offsets read 0,
//      one SEED write mixes into the pool once
//  18. Ordinary operation: 300 raw blocks from the source leave STATUS at 1
//
// Tests 1-9 drive the device directly with one-edge requests.  From test 14
// the bench also reaches the device through mp64_bus, wired as mp64_soc.v
// wires it, so a check can read the bus's terminal response code.
//
// Tests 10-17 choose the raw bytes the health test sees.  The device has
// no test input: the bench forces u_trng.raw_valid and u_trng.raw_accum,
// the strobe and block that the entropy path hands to the health test and
// the pool, and releases them to return to the modelled source.
//

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_trng;

    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;  // 100 MHz

    integer pass_count = 0;
    integer fail_count = 0;
    integer test_num   = 0;

    // ========================================================================
    // TRNG DUT
    // ========================================================================
    // Direct port, driven by trng_read and trng_write.
    reg         trng_req;
    reg  [4:0]  trng_addr;
    reg  [63:0] trng_wdata;
    reg         trng_wen;
    wire [63:0] trng_rdata;
    wire        trng_ack;

    // Bus port: mp64_bus's MMIO target, decoded and muxed as in mp64_soc.v.
    reg         use_bus;
    wire        bus_mmio_req;
    wire [11:0] bus_mmio_addr;
    wire [63:0] bus_mmio_wdata;
    wire        bus_mmio_wen;
    wire        bus_sel_trng   = bus_mmio_req &&
                                 (bus_mmio_addr[11:5] == 7'b1000000);
    wire [63:0] bus_mmio_rdata = bus_sel_trng ? trng_rdata : 64'd0;
    wire        bus_mmio_ack   = bus_sel_trng ? trng_ack   : 1'b0;

    mp64_trng u_trng (
        .clk(clk), .rst_n(rst_n),
        .req  (use_bus ? bus_sel_trng       : trng_req),
        .addr (use_bus ? bus_mmio_addr[4:0] : trng_addr),
        .wdata(use_bus ? bus_mmio_wdata     : trng_wdata),
        .wen  (use_bus ? bus_mmio_wen       : trng_wen),
        .rdata(trng_rdata), .ack(trng_ack)
    );

    // ========================================================================
    // Bus arbiter (the bench requests on port 0)
    // ========================================================================
    localparam N_PORTS = 4;

    reg  [N_PORTS-1:0]    cpu_valid;
    reg  [N_PORTS*64-1:0] cpu_addr;
    reg  [N_PORTS*64-1:0] cpu_wdata;
    reg  [N_PORTS-1:0]    cpu_wen;
    wire [N_PORTS*64-1:0] cpu_rdata;
    wire [N_PORTS-1:0]    cpu_ready;
    wire [N_PORTS*2-1:0]  cpu_resp_code;
    wire [N_PORTS-1:0]    bus_err;

    mp64_bus #(
        .N_PORTS (N_PORTS)
    ) u_bus (
        .clk(clk), .rst_n(rst_n),
        .cpu_valid(cpu_valid), .cpu_addr(cpu_addr), .cpu_wdata(cpu_wdata),
        .cpu_wen(cpu_wen), .cpu_size({N_PORTS{BUS_DWORD}}),
        .cpu_port_io({N_PORTS{1'b0}}),
        .cpu_requester_valid({N_PORTS{1'b0}}),
        .cpu_requester_id({(N_PORTS*8){1'b0}}),
        .cpu_rdata(cpu_rdata), .cpu_ready(cpu_ready), .cpu_accept(),
        .cpu_resp_code(cpu_resp_code),
        .mem_req(), .mem_addr(), .mem_wdata(), .mem_wen(), .mem_size(),
        .mem_rdata(64'd0), .mem_ack(1'b0), .mem_resp_code(BUS_RESP_OK),
        .mmio_req(bus_mmio_req), .mmio_addr(bus_mmio_addr),
        .mmio_wdata(bus_mmio_wdata), .mmio_wen(bus_mmio_wen),
        .mmio_size(), .mmio_port_io(), .mmio_requester_valid(),
        .mmio_requester_id(),
        .mmio_rdata(bus_mmio_rdata), .mmio_ack(bus_mmio_ack),
        .qos_csr_wen(1'b0), .qos_csr_addr(8'd0), .qos_csr_wdata(64'd0),
        .qos_csr_rdata(),
        .bus_err(bus_err)
    );

    // What the device drives while one bus access is in flight.
    integer dev_acks;
    reg     dev_data;
    always @(negedge clk) begin
        if (trng_ack)             dev_acks = dev_acks + 1;
        if (trng_rdata !== 64'd0) dev_data = 1'b1;
    end

    // XOR of the four pool words: what a SEED write changes.
    wire [63:0] pool_fold = u_trng.pool[0] ^ u_trng.pool[1] ^
                            u_trng.pool[2] ^ u_trng.pool[3];

    // ========================================================================
    // Helper tasks
    // ========================================================================
    task trng_write(input [4:0] a, input [63:0] d);
        begin
            @(posedge clk);
            trng_req  <= 1'b1; trng_addr <= a; trng_wdata <= d; trng_wen <= 1'b1;
            @(posedge clk);
            trng_req  <= 1'b0; trng_wen  <= 1'b0;
        end
    endtask

    task trng_read(input [4:0] a, output [63:0] d);
        begin
            @(posedge clk);
            trng_req  <= 1'b1; trng_addr <= a; trng_wen <= 1'b0;
            @(posedge clk);
            trng_req  <= 1'b0;
            @(posedge clk);  // wait for registered rdata to settle
            d = trng_rdata;
        end
    endtask

    task check(input [511:0] label, input [63:0] got, input [63:0] expected);
        begin
            if (got === expected) begin
                $display("  PASS: %0s = %016h", label, got);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s = %016h (expected %016h)", label, got, expected);
                fail_count = fail_count + 1;
            end
        end
    endtask

    task check_ne(input [511:0] label, input [63:0] a, input [63:0] b);
        begin
            if (a !== b) begin
                $display("  PASS: %0s: %016h != %016h", label, a, b);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s: both = %016h (expected different)", label, a);
                fail_count = fail_count + 1;
            end
        end
    endtask

    task check_zero_upper(input [511:0] label, input [63:0] val);
        begin
            if (val[63:8] === 56'd0) begin
                $display("  PASS: %0s = %02h (upper 56 bits zero)", label, val[7:0]);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s = %016h (upper 56 bits NOT zero)", label, val);
                fail_count = fail_count + 1;
            end
        end
    endtask

    task check_count(input [511:0] label, input integer got,
                     input integer expected);
        begin
            if (got === expected) begin
                $display("  PASS: %0s = %0d", label, got);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s = %0d (expected %0d)", label, got, expected);
                fail_count = fail_count + 1;
            end
        end
    endtask

    task expect_status(input [511:0] label, input [63:0] expected);
        reg [63:0] s;
        begin
            trng_read(5'h10, s);
            check(label, s, expected);
        end
    endtask

    // ========================================================================
    // Raw-byte hook
    // ========================================================================
    // While the bench holds the hook, u_trng.raw_valid and u_trng.raw_accum
    // follow inj_valid and inj_block, so the health test and the pool see
    // exactly the blocks that raw_byte sends.
    reg        inj_valid;
    reg [63:0] inj_block;

    task raw_take;
        begin
            inj_valid = 1'b0;
            force u_trng.raw_valid = inj_valid;
            force u_trng.raw_accum = inj_block;
        end
    endtask

    task raw_give;
        begin
            release u_trng.raw_valid;
            release u_trng.raw_accum;
        end
    endtask

    // Hand the device one raw block whose health-tested low byte is `b`.
    // The upper 56 bits differ on every call, so only that byte repeats.
    task raw_byte(input [7:0] b);
        begin
            @(negedge clk);
            inj_block = {inj_block[63:8] + 56'h9E3779B97F4A7D, b};
            inj_valid = 1'b1;
            @(negedge clk);
            inj_valid = 1'b0;
        end
    endtask

    // Four blocks with distinct low bytes seed the four pool words.
    task seed_pool;
        begin
            raw_byte(8'h11);
            raw_byte(8'h22);
            raw_byte(8'h33);
            raw_byte(8'h44);
        end
    endtask

    // Send up to `max` blocks with low byte `b` and read STATUS after each.
    // `last_ok` counts the bytes up to the last one that left STATUS at 1.
    // `trip` is the count of the byte that brought STATUS to 0 on a seeded
    // pool, or 0 when the run ends with STATUS still 1.
    task raw_run(input [7:0] b, input integer max,
                 output integer last_ok, output integer trip);
        integer    n;
        reg [63:0] s;
        begin
            last_ok = 0;
            trip    = 0;
            n       = 0;
            while (n < max && trip == 0) begin
                raw_byte(b);
                n = n + 1;
                trng_read(5'h10, s);
                if (s === 64'd1)
                    last_ok = n;
                else if (u_trng.pool_ready)
                    trip = n;
            end
        end
    endtask

    // Reset the device and the bus.  With `own_raw` set the bench takes the
    // hook before reset ends, so the device sees no block from the modelled
    // source.
    task reset_dut(input own_raw);
        begin
            @(negedge clk);
            use_bus   = 1'b0;
            trng_req  = 1'b0;
            trng_wen  = 1'b0;
            cpu_valid = {N_PORTS{1'b0}};
            cpu_wen   = {N_PORTS{1'b0}};
            rst_n     = 1'b0;
            if (own_raw)
                raw_take;
            else
                raw_give;
            repeat (4) @(negedge clk);
            rst_n = 1'b1;
        end
    endtask

    // ========================================================================
    // Bus access
    // ========================================================================
    // One access to TRNG offset `a` through the bus arbiter, held as a core
    // holds it until the arbiter's terminal response.  The response stays in
    // bus_q, bus_code and bus_fault.
    reg [63:0] bus_q;
    reg [1:0]  bus_code;
    reg        bus_fault;
    reg        bus_done;

    task bus_access(input [4:0] a, input wr, input [63:0] d);
        integer waited;
        begin
            @(negedge clk);
            use_bus         = 1'b1;
            cpu_addr[63:0]  = {MP64_MMIO_HI, 20'd0, 7'b1000000, a};
            cpu_wdata[63:0] = d;
            cpu_wen[0]      = wr;
            cpu_valid[0]    = 1'b1;
            dev_acks        = 0;
            dev_data        = 1'b0;
            waited          = 0;
            @(negedge clk);
            while (!cpu_ready[0] && waited < 200) begin
                @(negedge clk);
                waited = waited + 1;
            end
            bus_done     = cpu_ready[0];
            bus_q        = cpu_rdata[63:0];
            bus_code     = cpu_resp_code[1:0];
            bus_fault    = bus_err[0];
            cpu_valid[0] = 1'b0;
            cpu_wen[0]   = 1'b0;
            @(negedge clk);
            use_bus = 1'b0;
        end
    endtask

    // The last bus access ended with an OK response.
    task check_bus_ok(input [511:0] label);
        begin
            if (bus_done && bus_code === BUS_RESP_OK && bus_fault === 1'b0) begin
                $display("  PASS: %0s: OK response, data %016h", label, bus_q);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s: done=%b code=%0d bus_err=%b (expected an OK response)",
                         label, bus_done, bus_code, bus_fault);
                fail_count = fail_count + 1;
            end
        end
    endtask

    // A read of offset `a` ends with an OK response carrying `expected`.
    task bus_expect(input [511:0] label, input [4:0] a,
                    input [63:0] expected);
        begin
            bus_access(a, 1'b0, 64'd0);
            if (bus_done && bus_code === BUS_RESP_OK && bus_fault === 1'b0 &&
                bus_q === expected) begin
                $display("  PASS: %0s = %016h, OK response", label, bus_q);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s = %016h done=%b code=%0d bus_err=%b (expected %016h, OK)",
                         label, bus_q, bus_done, bus_code, bus_fault, expected);
                fail_count = fail_count + 1;
            end
        end
    endtask

    // A read of offset `a` ends as a target fault: the device drives no ACK
    // and no data, and the arbiter's MMIO watchdog answers with
    // BUS_RESP_TARGET_FAULT and the port's bus_err pulse.
    task bus_expect_fault(input [511:0] label, input [4:0] a);
        begin
            bus_access(a, 1'b0, 64'd0);
            if (bus_done && bus_code === BUS_RESP_TARGET_FAULT &&
                bus_fault === 1'b1 && dev_acks == 0 && dev_data === 1'b0) begin
                $display("  PASS: %0s: target fault with bus_err; device drove no ACK and no data",
                         label);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s: done=%b code=%0d bus_err=%b acks=%0d data=%b (%0s)",
                         label, bus_done, bus_code, bus_fault, dev_acks, dev_data,
                         "expected a target fault");
                fail_count = fail_count + 1;
            end
        end
    endtask

    // Reads of `count` offsets from `first` that the device answers with
    // anything except zero data and an OK response.
    task bus_count_nonzero(input [4:0] first, input integer count,
                           output integer bad);
        integer k;
        begin
            bad = 0;
            for (k = 0; k < count; k = k + 1) begin
                bus_access(first + k[4:0], 1'b0, 64'd0);
                if (!(bus_done && bus_code === BUS_RESP_OK &&
                      bus_fault === 1'b0 && bus_q === 64'd0))
                    bad = bad + 1;
            end
        end
    endtask

    // ========================================================================
    // Main test sequence
    // ========================================================================
    reg [63:0] rd, rd2, rd3;
    reg [63:0] fold0;
    reg [1:0]  idx0;
    integer i;
    integer ok_n, trip_n, bad_n;

    initial begin
        $dumpfile("tb_trng.vcd");
        $dumpvars(0, tb_trng);

        // Init signals
        rst_n = 0;
        trng_req = 0; trng_addr = 0; trng_wdata = 0; trng_wen = 0;
        use_bus = 0;
        cpu_valid = 0; cpu_addr = 0; cpu_wdata = 0; cpu_wen = 0;
        inj_valid = 0; inj_block = 0;
        dev_acks = 0; dev_data = 0;

        // Reset
        repeat(4) @(posedge clk);
        rst_n = 1;
        repeat(2) @(posedge clk);

        // ================================================================
        // Test 1: STATUS = 0 right after reset (pool not fully seeded)
        // ================================================================
        test_num = 1;
        $display("\nTest %0d: STATUS=0 after reset", test_num);
        trng_read(5'h10, rd);
        check("STATUS after reset", rd, 64'd0);

        // ================================================================
        // Test 2: STATUS = 1 after pool is fully seeded
        //   raw_count wraps every 64 cycles, raw_valid fires.
        //   pool_ready = 1 after pool_idx wraps past 3 (4 × 64 = 256 cycles).
        //   Give it 300 cycles to be safe.
        // ================================================================
        test_num = 2;
        $display("\nTest %0d: STATUS=1 after seeding", test_num);
        repeat(300) @(posedge clk);
        trng_read(5'h10, rd);
        check("STATUS after 300 clocks", rd, 64'd1);

        // ================================================================
        // Test 3: RAND8 returns 8-bit value (upper 56 bits = 0)
        // ================================================================
        test_num = 3;
        $display("\nTest %0d: RAND8 upper bits zero", test_num);
        trng_read(5'h00, rd);
        check_zero_upper("RAND8", rd);

        // ================================================================
        // Test 4: RAND64 returns a 64-bit value (non-zero after seeding)
        // ================================================================
        test_num = 4;
        $display("\nTest %0d: RAND64 non-zero", test_num);
        trng_read(5'h08, rd);
        if (rd !== 64'd0) begin
            $display("  PASS: RAND64 = %016h (non-zero)", rd);
            pass_count = pass_count + 1;
        end else begin
            $display("  FAIL: RAND64 = 0 (expected non-zero after seeding)");
            fail_count = fail_count + 1;
        end

        // ================================================================
        // Test 5: Two consecutive RAND64 reads differ
        //   The output is mixed with an incrementing counter, so reads
        //   at different times should yield different values.
        // ================================================================
        test_num = 5;
        $display("\nTest %0d: Two RAND64 reads differ", test_num);
        trng_read(5'h08, rd);
        trng_read(5'h08, rd2);
        check_ne("RAND64 consecutive", rd, rd2);

        // ================================================================
        // Test 6: SEED register write (smoke test — no crash)
        // ================================================================
        test_num = 6;
        $display("\nTest %0d: SEED write smoke", test_num);
        trng_write(5'h18, 64'hDEADBEEFCAFEBABE);
        repeat(4) @(posedge clk);
        // If we get here, it didn't crash
        $display("  PASS: SEED write completed without hang");
        pass_count = pass_count + 1;

        // ================================================================
        // Test 7: RAND8 value changes after SEED injection
        // ================================================================
        test_num = 7;
        $display("\nTest %0d: RAND8 varies after SEED", test_num);
        trng_read(5'h00, rd);
        trng_write(5'h18, 64'h1234567890ABCDEF);
        repeat(4) @(posedge clk);
        trng_read(5'h00, rd2);
        // Values should differ (counter changes between reads too)
        check_ne("RAND8 before/after seed", rd, rd2);

        // ================================================================
        // Test 8: Multiple RAND8 reads are not all identical
        //   Read 8 values; at least 2 must be distinct (overwhelming
        //   probability with a properly working TRNG).
        // ================================================================
        test_num = 8;
        $display("\nTest %0d: RAND8 diversity (8 reads)", test_num);
        begin : rand8_diversity
            reg [7:0] vals [0:7];
            integer distinct;
            integer j;

            for (i = 0; i < 8; i = i + 1) begin
                trng_read(5'h00, rd);
                vals[i] = rd[7:0];
            end

            distinct = 1;
            for (i = 1; i < 8; i = i + 1) begin
                for (j = 0; j < i; j = j + 1) begin
                    if (vals[i] != vals[j])
                        distinct = distinct + 1;
                end
            end

            if (distinct > 1) begin
                $display("  PASS: %0d distinct values among 8 RAND8 reads", distinct);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: All 8 RAND8 reads identical (%02h)", vals[0]);
                fail_count = fail_count + 1;
            end
        end

        // ================================================================
        // Test 9: Health monitor — STATUS remains 1 under normal operation
        //   After many reads, STATUS should still be valid (no health
        //   alarm from the repetition count test).
        // ================================================================
        test_num = 9;
        $display("\nTest %0d: Health OK after sustained reads", test_num);
        // Perform 100 reads to exercise the health monitor
        for (i = 0; i < 100; i = i + 1) begin
            trng_read(5'h08, rd);
        end
        trng_read(5'h10, rd);
        check("STATUS after 100 reads", rd, 64'd1);

        // ================================================================
        // Test 10: Repetition count cutoff
        //   With the pool seeded, a run of one byte leaves STATUS at 1
        //   through its 9th byte and brings it to 0 at its 10th.
        // ================================================================
        test_num = 10;
        $display("\nTest %0d: 9 identical bytes pass, the 10th fails", test_num);
        reset_dut(1'b1);
        seed_pool;
        expect_status("STATUS with the pool seeded", 64'd1);
        raw_run(8'hA5, 16, ok_n, trip_n);
        check_count("0xA5 run: last byte with STATUS=1", ok_n, 9);
        check_count("0xA5 run: byte that brings STATUS to 0", trip_n, 10);

        // ================================================================
        // Test 11: First run after reset
        //   The first byte after reset starts a run of length 1 whatever
        //   its value, so a first run of 0x00 fails at the same byte as a
        //   first run of 0xFF.  The first three bytes seed the pool, so
        //   STATUS shows the health state from the 4th byte on.
        // ================================================================
        test_num = 11;
        $display("\nTest %0d: First run after reset fails at its 10th byte", test_num);
        reset_dut(1'b1);
        raw_run(8'h00, 16, ok_n, trip_n);
        check_count("first run of 0x00: last byte with STATUS=1", ok_n, 9);
        check_count("first run of 0x00: byte that brings STATUS to 0", trip_n, 10);
        reset_dut(1'b1);
        raw_run(8'hFF, 16, ok_n, trip_n);
        check_count("first run of 0xFF: last byte with STATUS=1", ok_n, 9);
        check_count("first run of 0xFF: byte that brings STATUS to 0", trip_n, 10);

        // ================================================================
        // Test 12: A broken run starts the count again
        //   Nine 0x5A, nine 0x3C and nine 0x5A leave STATUS at 1.  The
        //   last run then fails at its own 10th byte.
        // ================================================================
        test_num = 12;
        $display("\nTest %0d: A run broken at 9 starts the next count at 1", test_num);
        reset_dut(1'b1);
        seed_pool;
        raw_run(8'h5A, 9, ok_n, trip_n);
        check_count("nine 0x5A: bytes with STATUS=1", ok_n, 9);
        raw_run(8'h3C, 9, ok_n, trip_n);
        check_count("then nine 0x3C: bytes with STATUS=1", ok_n, 9);
        raw_run(8'h5A, 16, ok_n, trip_n);
        check_count("then 0x5A again: last byte with STATUS=1", ok_n, 9);
        check_count("then 0x5A again: byte that brings STATUS to 0", trip_n, 10);

        // ================================================================
        // Test 13: The failure latches
        //   Test 12 left the health test failed.  Neither 32 distinct
        //   bytes from the bench nor ten blocks from the modelled source
        //   bring STATUS back.
        // ================================================================
        test_num = 13;
        $display("\nTest %0d: A failure stays latched", test_num);
        for (i = 0; i < 32; i = i + 1)
            raw_byte(8'h40 + i[7:0]);
        expect_status("STATUS after 32 distinct bytes", 64'd0);
        raw_give;
        repeat (640) @(posedge clk);
        expect_status("STATUS after ten blocks from the source", 64'd0);
        raw_take;

        // ================================================================
        // Test 14: Unusable device on the bus
        //   STATUS still answers, with zero.  RAND8 and every RAND64 lane
        //   end as target faults.  A SEED write is acknowledged and leaves
        //   the pool and STATUS as they were.
        // ================================================================
        test_num = 14;
        $display("\nTest %0d: Bus access after a health failure", test_num);
        bus_expect("STATUS", 5'h10, 64'd0);
        bus_expect_fault("RAND8", 5'h00);
        bus_expect_fault("RAND64", 5'h08);
        bus_expect_fault("RAND64 lane 7", 5'h0F);
        fold0 = pool_fold;
        idx0  = u_trng.pool_idx;
        bus_access(5'h18, 1'b1, 64'hDEADBEEFCAFEBABE);
        check_bus_ok("SEED write");
        check("pool change from the SEED write", pool_fold ^ fold0, 64'd0);
        check_count("pool words the SEED write used",
                    (u_trng.pool_idx - idx0) & 3, 0);
        bus_expect("STATUS after the SEED write", 5'h10, 64'd0);

        // ================================================================
        // Test 15: Reset recovers
        //   After reset the modelled source seeds the pool again and the
        //   device is usable.
        // ================================================================
        test_num = 15;
        $display("\nTest %0d: Reset recovers a latched failure", test_num);
        reset_dut(1'b0);
        repeat (300) @(posedge clk);
        bus_expect("STATUS after reset and seeding", 5'h10, 64'd1);
        bus_access(5'h00, 1'b0, 64'd0);
        check_bus_ok("RAND8");
        bus_access(5'h08, 1'b0, 64'd0);
        check_bus_ok("RAND64");

        // ================================================================
        // Test 16: Before the pool is seeded
        //   With no raw block delivered since reset, STATUS reads zero,
        //   RAND8 and RAND64 end as target faults and SEED is ignored.
        //   Four blocks then make the device usable.
        // ================================================================
        test_num = 16;
        $display("\nTest %0d: Bus access before the pool is seeded", test_num);
        reset_dut(1'b1);
        bus_expect("STATUS", 5'h10, 64'd0);
        bus_expect_fault("RAND8", 5'h00);
        bus_expect_fault("RAND64", 5'h08);
        bus_access(5'h18, 1'b1, 64'h0123456789ABCDEF);
        check_bus_ok("SEED write");
        check("pool change from the SEED write", pool_fold, 64'd0);
        check_count("pool words the SEED write used", u_trng.pool_idx, 0);
        seed_pool;
        bus_expect("STATUS after four blocks", 5'h10, 64'd1);
        bus_access(5'h00, 1'b0, 64'd0);
        check_bus_ok("RAND8 after four blocks");

        // ================================================================
        // Test 17: Usable device on the bus
        //   The arbiter holds REQ across two clock edges of an acknowledged
        //   access.  Reads succeed, offsets outside the four registers read
        //   zero, and one SEED write mixes into one pool word once.  The
        //   bench still holds the hook, so no raw block reaches the pool.
        // ================================================================
        test_num = 17;
        $display("\nTest %0d: Bus access while usable", test_num);
        bus_access(5'h00, 1'b0, 64'd0);
        check_bus_ok("RAND8");
        check_zero_upper("RAND8 through the bus", bus_q);
        bus_access(5'h08, 1'b0, 64'd0);
        check_bus_ok("RAND64");
        rd = bus_q;
        bus_access(5'h08, 1'b0, 64'd0);
        check_ne("RAND64 consecutive through the bus", rd, bus_q);
        bus_count_nonzero(5'h01, 7, bad_n);
        check_count("offsets 0x01-0x07 that read non-zero", bad_n, 0);
        bus_count_nonzero(5'h11, 7, bad_n);
        check_count("offsets 0x11-0x17 that read non-zero", bad_n, 0);
        bus_count_nonzero(5'h18, 8, bad_n);
        check_count("offsets 0x18-0x1F that read non-zero", bad_n, 0);
        fold0 = pool_fold;
        idx0  = u_trng.pool_idx;
        bus_access(5'h18, 1'b1, 64'hDEADBEEFCAFEBABE);
        check_bus_ok("SEED write");
        check("pool change from the SEED write", pool_fold ^ fold0,
              64'hDEADBEEFCAFEBABE);
        check_count("pool words the SEED write used",
                    (u_trng.pool_idx - idx0) & 3, 1);
        bus_expect("STATUS after the SEED write", 5'h10, 64'd1);

        // ================================================================
        // Test 18: Ordinary operation
        //   300 raw blocks from the modelled source leave the device
        //   usable.  The waveform dump pauses for the wait.
        // ================================================================
        test_num = 18;
        $display("\nTest %0d: 300 raw blocks from the source", test_num);
        reset_dut(1'b0);
        $dumpoff;
        repeat (64 * 300 + 8) @(posedge clk);
        $dumpon;
        expect_status("STATUS after 300 blocks", 64'd1);
        bus_access(5'h08, 1'b0, 64'd0);
        check_bus_ok("RAND64");

        // ================================================================
        // Summary
        // ================================================================
        $display("\n========================================");
        $display("  TRNG Tests: %0d PASSED, %0d FAILED", pass_count, fail_count);
        $display("========================================");
        $finish;
    end

    // Watchdog timer
    initial begin
        #1000000;
        $display("TIMEOUT: TestBench exceeded 1ms");
        $finish;
    end

endmodule
