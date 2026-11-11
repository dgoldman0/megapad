// ============================================================================
// tb_soc_chiplet.v — A chiplet port inside the full SoC
// ============================================================================
//
// Instantiates mp64_soc with one chiplet port, a behavioural chiplet on its
// link, and a behavioural external-memory PHY.  The cores are held in reset
// and the bench drives the arbiter's MMIO and memory seams itself, so every
// step is deterministic.  Covers:
//   - the register window and the shared registers;
//   - a chiplet reading and writing lent external memory through the
//     bridge, the client arbiter, the controller and the PHY;
//   - refusal of an access outside the lent range, before the PHY;
//   - the core path and the chiplet sharing external memory;
//   - doorbells in both directions through the mailbox, with the IPI line
//     and the external flag;
//   - the sideband: reset, identity and the boot words.
//

`timescale 1ns / 1ps

`include "mp64_pkg.vh"

module tb_soc_chiplet;
    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;

    integer pass_count, fail_count;
    task check(input [8*88-1:0] label, input cond);
        begin
            if (cond) begin
                $display("  PASS: %0s", label);
                pass_count = pass_count + 1;
            end else begin
                $display("  FAIL: %0s", label);
                fail_count = fail_count + 1;
            end
        end
    endtask

    // ------------------------------------------------------------------
    // External-memory PHY: 1 MiB at the external base, fixed latency
    // ------------------------------------------------------------------
    wire        phy_req, phy_wen, phy_cancel;
    wire [63:0] phy_addr, phy_wdata;
    wire [7:0]  phy_burst_len;
    reg  [63:0] phy_rdata;
    reg         phy_rvalid;

    reg [63:0] xmem [0:131071];
    reg        ppend;
    reg [3:0]  pcnt;
    reg [63:0] p_addr, p_wdata;
    reg        p_wen;
    integer    phy_launches;
    integer    xi;
    initial for (xi = 0; xi < 131072; xi = xi + 1) xmem[xi] = 64'd0;

    wire [16:0] p_index = p_addr[19:3];
    always @(posedge clk) begin
        phy_rvalid <= 1'b0;
        if (!rst_n || phy_cancel) begin
            ppend <= 1'b0;
        end else if (phy_req && !ppend) begin
            ppend   <= 1'b1;
            pcnt    <= 4'd2;
            p_addr  <= phy_addr;
            p_wdata <= phy_wdata;
            p_wen   <= phy_wen;
            phy_launches = phy_launches + 1;
        end else if (ppend) begin
            if (pcnt == 4'd0) begin
                ppend      <= 1'b0;
                phy_rvalid <= 1'b1;
                if (p_wen)
                    xmem[p_index] <= p_wdata;
                else
                    phy_rdata <= xmem[p_index];
            end else begin
                pcnt <= pcnt - 4'd1;
            end
        end
    end

    // ------------------------------------------------------------------
    // Chiplet side of the link
    // ------------------------------------------------------------------
    reg         cl_rq_valid, cl_rq_wen, cl_rq_cancel, cl_rs_ready;
    reg  [63:0] cl_rq_addr, cl_rq_wdata;
    reg  [1:0]  cl_rq_size;
    wire        cl_rq_ready, cl_rs_valid;
    wire [63:0] cl_rs_rdata;
    wire [1:0]  cl_rs_code;
    wire        cl_dbt_valid, cl_dbr_ready;
    reg         cl_dbt_ready, cl_dbr_valid;
    wire [7:0]  cl_dbt_src;
    reg  [7:0]  cl_dbr_dst;
    wire        cl_sb_rst_n, cl_sb_stop_req, cl_sb_id_req;
    wire [7:0]  cl_sb_id_index;
    reg         cl_sb_id_ack;
    reg  [63:0] cl_sb_id_data;
    wire [63:0] cl_sb_boot_addr, cl_sb_boot_len;

    // The chiplet's identity record: word n reads "LM1" and n.
    always @(posedge clk) begin
        cl_sb_id_ack <= 1'b0;
        if (cl_sb_id_req && !cl_sb_id_ack) begin
            cl_sb_id_ack  <= 1'b1;
            cl_sb_id_data <= {40'h4C_4D31_0000, 16'd0, cl_sb_id_index};
        end
    end

    // ------------------------------------------------------------------
    // SoC: one full core, one cluster of two, 1 MiB of external memory
    // ------------------------------------------------------------------
    mp64_soc #(
        .CLOCK_HZ          (100_000_000),
        .NUM_CORES         (1),
        .NUM_CLUSTERS      (1),
        .CORES_PER_CLUSTER (2),
        .MEM_DEPTH         (256),
        .EXT_MEM_SIZE_PARAM(32'h0010_0000),
        .NUM_CHIPLET_PORTS (1)
    ) u_soc (
        .sys_clk(clk), .sys_rst_n(rst_n),
        .uart_rxd(1'b1), .uart_txd(),
        .phy_req(phy_req), .phy_addr(phy_addr), .phy_wen(phy_wen),
        .phy_wdata(phy_wdata), .phy_burst_len(phy_burst_len),
        .phy_cancel(phy_cancel), .phy_rdata(phy_rdata),
        .phy_rvalid(phy_rvalid), .phy_ready(1'b1), .phy_error(1'b0),
        .phy_cancel_done(phy_cancel),
        .sd_sck(), .sd_mosi(), .sd_miso(1'b1), .sd_cs_n(),
        .sd_card_present(1'b0), .sd_write_protected(1'b0),
        .sd1_sck(), .sd1_mosi(), .sd1_miso(1'b1), .sd1_cs_n(),
        .sd1_card_present(1'b0),
        .sd2_sck(), .sd2_mosi(), .sd2_miso(1'b1), .sd2_cs_n(),
        .sd2_card_present(1'b0),
        .sd3_sck(), .sd3_mosi(), .sd3_miso(1'b1), .sd3_cs_n(),
        .sd3_card_present(1'b0), .sd3_write_protected(1'b0),
        .nic_tx_valid(), .nic_tx_data(), .nic_tx_ready(1'b0),
        .nic_rx_valid(1'b0), .nic_rx_data(8'd0), .nic_rx_ready(),
        .nic_link_up(1'b0),
        .cl_rq_valid(cl_rq_valid), .cl_rq_ready(cl_rq_ready),
        .cl_rq_addr(cl_rq_addr), .cl_rq_wdata(cl_rq_wdata),
        .cl_rq_wen(cl_rq_wen), .cl_rq_size(cl_rq_size),
        .cl_rq_cancel(cl_rq_cancel),
        .cl_rs_valid(cl_rs_valid), .cl_rs_ready(cl_rs_ready),
        .cl_rs_rdata(cl_rs_rdata), .cl_rs_code(cl_rs_code),
        .cl_dbt_valid(cl_dbt_valid), .cl_dbt_ready(cl_dbt_ready),
        .cl_dbt_src(cl_dbt_src),
        .cl_dbr_valid(cl_dbr_valid), .cl_dbr_ready(cl_dbr_ready),
        .cl_dbr_dst(cl_dbr_dst),
        .cl_sb_present(1'b1), .cl_sb_rst_n(cl_sb_rst_n),
        .cl_sb_stop_req(cl_sb_stop_req), .cl_sb_stopped(1'b0),
        .cl_sb_ready(1'b1), .cl_sb_id_req(cl_sb_id_req),
        .cl_sb_id_index(cl_sb_id_index), .cl_sb_id_ack(cl_sb_id_ack),
        .cl_sb_id_data(cl_sb_id_data), .cl_sb_boot_addr(cl_sb_boot_addr),
        .cl_sb_boot_len(cl_sb_boot_len),
        .debug_leds()
    );

    // ------------------------------------------------------------------
    // The arbiter's MMIO seam, owned by the bench
    // ------------------------------------------------------------------
    reg [63:0] mmio_rd;
    reg        mmio_acked;

    task mmio_access;
        input [11:0] a;
        input [63:0] d;
        input        w;
        input [1:0]  sz;
        begin
            @(negedge clk);
            force u_soc.bus_mmio_req   = 1'b1;
            force u_soc.bus_mmio_wen   = w;
            force u_soc.bus_mmio_addr  = a;
            force u_soc.bus_mmio_wdata = d;
            force u_soc.bus_mmio_size  = sz;
            #1;
            mmio_acked = u_soc.bus_mmio_ack;
            mmio_rd    = u_soc.bus_mmio_rdata;
            @(negedge clk);          // the rising edge between applies a write
            force u_soc.bus_mmio_req = 1'b0;
            force u_soc.bus_mmio_wen = 1'b0;
        end
    endtask

    task reg_wr(input [11:0] a, input [63:0] d);
        mmio_access(a, d, 1'b1, BUS_DWORD);
    endtask
    task reg_rd(input [11:0] a);
        mmio_access(a, 64'd0, 1'b0, BUS_DWORD);
    endtask

    // Chiplet window registers.
    localparam [11:0] W_COUNT = 12'hE00, W_SEL = 12'hE08, W_CORE_QOS = 12'hE10;
    localparam [11:0] P_CTRL = 12'hE80, P_STATUS = 12'hE88;
    localparam [11:0] P_FAULT_INFO = 12'hE98;
    localparam [11:0] P_RSEL = 12'hEA0, P_RBASE = 12'hEA8;
    localparam [11:0] P_RLIM = 12'hEB0, P_RRTS = 12'hEB8;
    localparam [11:0] P_ID_INDEX = 12'hEC8, P_ID_DATA = 12'hED0;
    localparam [11:0] P_BOOT_ADDR = 12'hED8, P_BOOT_LEN = 12'hEE0;
    localparam [11:0] MB_SEND = 12'h508, MB_STATUS = 12'h509, MB_ACK = 12'h50A;

    // ------------------------------------------------------------------
    // Link tasks
    // ------------------------------------------------------------------
    reg [1:0]  rs_code;
    reg [63:0] rs_data;
    integer    guard;

    task link_request;
        input [63:0] addr;
        input [63:0] wdata;
        input        wen;
        begin
            @(posedge clk);
            cl_rq_valid <= 1'b1; cl_rq_addr <= addr; cl_rq_wdata <= wdata;
            cl_rq_wen <= wen; cl_rq_size <= BUS_DWORD;
            guard = 0;
            @(posedge clk);
            while (!cl_rq_ready && guard < 2000) begin
                @(posedge clk); guard = guard + 1;
            end
            cl_rq_valid <= 1'b0;
        end
    endtask

    task link_response;
        begin
            guard = 0;
            while (!cl_rs_valid && guard < 4000) begin
                @(posedge clk); guard = guard + 1;
            end
            rs_code = cl_rs_code;
            rs_data = cl_rs_rdata;
            cl_rs_ready <= 1'b1;
            @(posedge clk);
            cl_rs_ready <= 1'b0;
            @(posedge clk);
        end
    endtask

    task link_access;
        input [63:0] addr;
        input [63:0] wdata;
        input        wen;
        begin
            link_request(addr, wdata, wen);
            link_response;
        end
    endtask

    // One access on the core path, through the arbiter's memory seam.
    reg [63:0] core_rd;
    task core_access;
        input [63:0] addr;
        input [63:0] wdata;
        input        wen;
        begin
            @(negedge clk);
            force u_soc.bus_mem_req   = 1'b1;
            force u_soc.bus_mem_addr  = addr;
            force u_soc.bus_mem_wdata = wdata;
            force u_soc.bus_mem_wen   = wen;
            force u_soc.bus_mem_size  = BUS_DWORD;
            guard = 0;
            @(posedge clk);
            while (!u_soc.bus_mem_ack && guard < 4000) begin
                @(posedge clk); guard = guard + 1;
            end
            core_rd = u_soc.bus_mem_rdata;
            @(negedge clk);
            force u_soc.bus_mem_req = 1'b0;
            repeat (2) @(posedge clk);
        end
    endtask

    integer prev_n;
    localparam [7:0] PORT_ID = 8'd3;      // one full core and two microcores

    initial begin
        pass_count = 0; fail_count = 0; phy_launches = 0;
        rst_n = 1'b0;
        cl_rq_valid = 0; cl_rq_wen = 0; cl_rq_cancel = 0; cl_rs_ready = 0;
        cl_rq_addr = 0; cl_rq_wdata = 0; cl_rq_size = BUS_DWORD;
        cl_dbt_ready = 0; cl_dbr_valid = 0; cl_dbr_dst = 0;
        cl_sb_id_ack = 0; cl_sb_id_data = 0;

        // Hold every core in reset and take the arbiter's seams.
        force u_soc.core_domain_reset   = 1'b1;
        force u_soc.cluster_micro_reset = 2'b11;
        force u_soc.bus_mmio_req     = 1'b0;
        force u_soc.bus_mmio_wen     = 1'b0;
        force u_soc.bus_mmio_port_io = 1'b0;
        force u_soc.bus_mmio_requester_valid = 1'b1;
        force u_soc.bus_mmio_requester_id    = 8'd0;
        force u_soc.bus_mem_req = 1'b0;
        #50;
        rst_n = 1'b1;
        repeat (10) @(posedge clk);

        $display("=== tb_soc_chiplet ===");

        $display("\nThe register window");
        reg_rd(W_COUNT);
        check("PORT_COUNT reports one port, ID 3, eight ranges",
              mmio_acked && mmio_rd == {40'd0, 8'd8, PORT_ID, 8'd1});
        mmio_access(W_COUNT, 64'd0, 1'b0, BUS_WORD);
        check("a narrower access to the window goes unanswered",
              mmio_acked === 1'b0);
        mmio_access(12'hE04, 64'd0, 1'b0, BUS_DWORD);
        check("a misaligned access goes unanswered", mmio_acked === 1'b0);
        reg_wr(W_CORE_QOS, 64'd3);
        reg_rd(W_CORE_QOS);
        check("the core path's weight is writable", mmio_rd == 64'd3);
        reg_rd(P_STATUS);
        check("the port reports its chiplet present and ready",
              mmio_rd[0] && mmio_rd[2] && mmio_rd[23:16] == PORT_ID);
        check("the chiplet is held in reset", cl_sb_rst_n == 1'b0);
        reg_wr(W_SEL, 64'd1);
        reg_rd(P_STATUS);
        check("a port index beyond the ports built reads zero",
              mmio_acked && mmio_rd == 64'd0);
        reg_wr(W_SEL, 64'd0);

        $display("\nSideband before release");
        reg_wr(P_ID_INDEX, 64'd2);
        repeat (4) @(posedge clk);
        reg_rd(P_ID_DATA);
        check("the identity record reads while the chiplet is in reset",
              mmio_rd == {40'h4C_4D31_0000, 16'd0, 8'd2});
        reg_wr(P_BOOT_ADDR, 64'h0010_8000);
        reg_wr(P_BOOT_LEN, 64'h0000_0400);
        check("the boot words reach the chiplet",
              cl_sb_boot_addr == 64'h0010_8000 &&
              cl_sb_boot_len == 64'h400);

        $display("\nLent memory");
        // Lend [0x0010_1000, 0x0010_2000) for reading and writing.
        reg_wr(P_RSEL, 64'd0);
        reg_wr(P_RBASE, 64'h0010_1000);
        reg_wr(P_RLIM, 64'h0010_2000);
        reg_wr(P_RRTS, 64'd3);
        reg_wr(P_CTRL, 64'h09);              // ENABLE, DOORBELL_EN, released
        check("the chiplet leaves reset", cl_sb_rst_n == 1'b1);
        link_access(64'h0010_1008, 64'hFEED_FACE_CAFE_BEEF, 1'b1);
        check("the chiplet writes a lent word", rs_code == 2'd0);
        check("the word is in external memory",
              xmem[17'h00201] == 64'hFEED_FACE_CAFE_BEEF);
        link_access(64'h0010_1008, 64'd0, 1'b0);
        check("the chiplet reads it back",
              rs_code == 2'd0 && rs_data == 64'hFEED_FACE_CAFE_BEEF);
        prev_n = phy_launches;
        link_access(64'h0010_3000, 64'h1, 1'b1);
        check("an access outside the lent range is refused",
              rs_code == 2'd1);
        check("it never reaches the PHY", phy_launches == prev_n);
        reg_rd(P_FAULT_INFO);
        check("the refusal is recorded", mmio_rd[31:16] == 16'd1);
        reg_wr(P_STATUS, 64'h10);

        $display("\nThe core path beside the chiplet");
        core_access(64'h0010_1010, 64'h0123_4567_89AB_CDEF, 1'b1);
        link_access(64'h0010_1010, 64'd0, 1'b0);
        check("the chiplet reads a word a core wrote",
              rs_code == 2'd0 && rs_data == 64'h0123_4567_89AB_CDEF);
        link_access(64'h0010_1018, 64'h1111_2222_3333_4444, 1'b1);
        core_access(64'h0010_1018, 64'd0, 1'b0);
        check("a core reads a word the chiplet wrote",
              core_rd == 64'h1111_2222_3333_4444);
        // Both at once: the chiplet's request is in flight when the core's
        // arrives, and each completes with its own data.
        xmem[17'h00204] = 64'hAAAA_0000_0000_0001;
        xmem[17'h00205] = 64'hBBBB_0000_0000_0002;
        link_request(64'h0010_1020, 64'd0, 1'b0);
        core_access(64'h0010_1028, 64'd0, 1'b0);
        link_response;
        check("with both in flight the core gets its word",
              core_rd == 64'hBBBB_0000_0000_0002);
        check("and the chiplet gets its word",
              rs_code == 2'd0 && rs_data == 64'hAAAA_0000_0000_0001);

        $display("\nDoorbells");
        mmio_access(MB_SEND, {56'd0, PORT_ID}, 1'b1, BUS_BYTE);
        repeat (3) @(posedge clk);
        check("a core's signal reaches the chiplet with the core's ID",
              cl_dbt_valid == 1'b1 && cl_dbt_src == 8'd0);
        check("it raises no IPI and no external flag",
              u_soc.ipi_out == 1'b0 && u_soc.mbox_ef == 1'b0);
        cl_dbt_ready <= 1'b1;
        @(posedge clk);
        cl_dbt_ready <= 1'b0;
        repeat (2) @(posedge clk);
        check("once taken it is offered no more", cl_dbt_valid == 1'b0);

        @(posedge clk);
        cl_dbr_valid <= 1'b1; cl_dbr_dst <= 8'd0;
        @(posedge clk);
        cl_dbr_valid <= 1'b0;
        repeat (2) @(posedge clk);
        check("the chiplet's signal raises core 0's IPI line",
              u_soc.ipi_out == 1'b1);
        check("and its external flag", u_soc.mbox_ef == 1'b1);
        check("the flag reaches the core's flag input",
              u_soc.g_core[0].u_cpu.ef_flags == 4'b0001);
        mmio_access(MB_STATUS, 64'd0, 1'b0, BUS_BYTE);
        check("the core's status shows the port's bit at its ID, 3",
              mmio_rd[7:0] == 8'b0000_1000);
        check("the core's CSR mask shows the same bit",
              u_soc.g_core[0].u_cpu.ipi_pending == 64'h8);
        mmio_access(MB_ACK, {56'd0, PORT_ID}, 1'b1, BUS_BYTE);
        repeat (2) @(posedge clk);
        check("acknowledging the port clears the line and the flag",
              u_soc.ipi_out == 1'b0 && u_soc.mbox_ef == 1'b0);

        $display("\nReturn to reset");
        reg_wr(P_CTRL, 64'h0B);
        check("the chiplet's reset line falls", cl_sb_rst_n == 1'b0);
        check("the link takes no request", cl_rq_ready == 1'b0);

        $display("\n=== Results: %0d passed, %0d failed ===",
                 pass_count, fail_count);
        if (fail_count == 0)
            $display("ALL TESTS PASSED");
        else
            $display("SOME TESTS FAILED");
        $finish;
    end

    initial begin
        #20000000;
        $display("TIMEOUT");
        $finish;
    end
endmodule
