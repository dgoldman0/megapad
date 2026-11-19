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
//   - the sideband: reset, identity and the boot words;
//   - a TACC.STORE whose image lies in the port's window, from core 0's tile
//     engine through the image stage and the bridge to the chiplet, which
//     holds the image before a doorbell rung after the store; and the
//     faults of a closed port, a refusing chiplet, a LOAD from the window
//     and a core's access there.
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
    reg  [7:0]  cl_rq_len;
    wire        cl_rq_ready, cl_rs_valid, cl_rs_last;
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
    wire [63:0] cl_sb_boot_addr, cl_sb_boot_len, cl_sb_boot_arg;

    // MegaPad's writes into the memory the chiplet lends.  The chiplet keeps
    // 16 units of 64 bytes at its own addresses 0x4000-0x43FF.  It answers
    // with t_code: OK after taking every unit and writing the data and the
    // zeros, any other code at once, taking no unit and writing nothing.
    wire         cl_wq_valid, cl_wq_cancel, cl_wa_ready;
    wire [63:0]  cl_wq_addr;
    wire [511:0] cl_wq_data;
    wire [7:0]   cl_wq_len, cl_wq_zeros;
    reg          cl_wa_valid;
    reg  [1:0]   cl_wa_code;
    localparam [63:0] LENT_BASE = 64'h4000;
    reg  [511:0] lent [0:15];
    reg  [1:0]   t_code;
    reg          t_answering;
    integer      t_units, t_wait, t_writes, t_unit_count;
    integer      cyc, answer_cyc, dbt_cyc;
    integer      li, lz;
    wire cl_wq_ready = (t_code == 2'd0) && !t_answering;

    initial begin
        cyc = 0; answer_cyc = 0; dbt_cyc = 0;
        for (li = 0; li < 16; li = li + 1) lent[li] = 512'd0;
    end

    always @(posedge clk) begin
        cyc = cyc + 1;
        if (cl_dbt_valid && dbt_cyc == 0)
            dbt_cyc = cyc;
        if (!rst_n) begin
            t_units = 0; t_answering = 1'b0; t_writes = 0; t_unit_count = 0;
            cl_wa_valid <= 1'b0; cl_wa_code <= 2'd0;
        end else begin
            if (cl_wq_valid && cl_wq_ready) begin
                lent[(cl_wq_addr - LENT_BASE) / 64 + t_units] = cl_wq_data;
                t_units = t_units + 1;
                t_unit_count = t_unit_count + 1;
                if (t_units == cl_wq_len + 1) begin
                    for (lz = 0; lz < cl_wq_zeros; lz = lz + 1)
                        lent[(cl_wq_addr - LENT_BASE) / 64 + t_units + lz] =
                            512'd0;
                    t_answering = 1'b1;
                    t_wait = 3;
                end
            end else if (cl_wq_valid && t_code != 2'd0 && !t_answering) begin
                t_answering = 1'b1;
                t_wait = 0;
            end
            if (t_answering) begin
                if (t_wait > 0) begin
                    t_wait = t_wait - 1;
                end else if (!cl_wa_valid) begin
                    cl_wa_valid <= 1'b1;
                    cl_wa_code  <= t_code;
                end else if (cl_wa_ready) begin
                    cl_wa_valid <= 1'b0;
                    t_answering = 1'b0;
                    t_units = 0;
                    t_writes = t_writes + 1;
                    answer_cyc = cyc;
                end
            end
        end
    end

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
        .cl_rq_len(cl_rq_len), .cl_rq_cancel(cl_rq_cancel),
        .cl_rs_valid(cl_rs_valid), .cl_rs_ready(cl_rs_ready),
        .cl_rs_rdata(cl_rs_rdata), .cl_rs_code(cl_rs_code),
        .cl_rs_last(cl_rs_last),
        .cl_dbt_valid(cl_dbt_valid), .cl_dbt_ready(cl_dbt_ready),
        .cl_dbt_src(cl_dbt_src),
        .cl_dbr_valid(cl_dbr_valid), .cl_dbr_ready(cl_dbr_ready),
        .cl_dbr_dst(cl_dbr_dst),
        .cl_wq_valid(cl_wq_valid), .cl_wq_ready(cl_wq_ready),
        .cl_wq_addr(cl_wq_addr), .cl_wq_data(cl_wq_data),
        .cl_wq_len(cl_wq_len), .cl_wq_zeros(cl_wq_zeros),
        .cl_wq_cancel(cl_wq_cancel),
        .cl_wa_valid(cl_wa_valid), .cl_wa_ready(cl_wa_ready),
        .cl_wa_code(cl_wa_code),
        .cl_sb_present(1'b1), .cl_sb_rst_n(cl_sb_rst_n),
        .cl_sb_stop_req(cl_sb_stop_req), .cl_sb_stopped(1'b0),
        .cl_sb_ready(1'b1), .cl_sb_id_req(cl_sb_id_req),
        .cl_sb_id_index(cl_sb_id_index), .cl_sb_id_ack(cl_sb_id_ack),
        .cl_sb_id_data(cl_sb_id_data), .cl_sb_boot_addr(cl_sb_boot_addr),
        .cl_sb_boot_len(cl_sb_boot_len),
        .cl_sb_boot_arg(cl_sb_boot_arg),
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
    localparam [11:0] P_BOOT_ADDR = 12'hED8, P_BOOT_LEN = 12'hEE0,
                      P_BOOT_ARG = 12'hEE8;
    localparam [11:0] P_WINDOW = 12'hEF0;
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
    reg        core_fault;
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
            core_fault = u_soc.bus_mem_fault;
            @(negedge clk);
            force u_soc.bus_mem_req = 1'b0;
            repeat (2) @(posedge clk);
        end
    endtask

    // Core 0's tile engine, driven at its dispatch and CSR boundary.
    reg [2:0]  mex_f;
    reg [63:0] mex_fa;
    integer    mex_cycles;
    integer    done_cyc;          // cycle count when the operation ended

    task tile_csr(input [7:0] a, input [63:0] d);
        begin
            @(negedge clk);
            force u_soc.core_csr_addr[0]  = a;
            force u_soc.core_csr_wdata[0] = d;
            force u_soc.core_csr_wen[0]   = 1'b1;
            @(negedge clk);
            force u_soc.core_csr_wen[0]   = 1'b0;
        end
    endtask

    // One TACC lifecycle operation; returns its fault and fault address.
    task tacc_op(input [2:0] funct);
        begin
            @(negedge clk);
            force u_soc.core_mex_funct[0]      = funct;
            force u_soc.core_mex_funct_byte[0] = {5'd0, funct};
            force u_soc.core_mex_valid[0]      = 1'b1;
            @(negedge clk);
            force u_soc.core_mex_valid[0]      = 1'b0;
            mex_cycles = 1;
            while (!u_soc.core_mex_done[0] && mex_cycles < 2000) begin
                @(negedge clk); mex_cycles = mex_cycles + 1;
            end
            mex_f  = u_soc.core_mex_fault[0];
            mex_fa = u_soc.core_mex_fault_addr[0];
            done_cyc = cyc;
            @(negedge clk);
        end
    endtask

    function [511:0] unit_of(input [7:0] tag);
        unit_of = {64{tag}};
    endfunction

    integer prev_n;
    localparam [7:0] PORT_ID = 8'd3;      // one full core and two microcores
    localparam [63:0] WINDOW = 64'h0100_0000_0000_0000;

    initial begin
        pass_count = 0; fail_count = 0; phy_launches = 0;
        rst_n = 1'b0;
        cl_rq_valid = 0; cl_rq_wen = 0; cl_rq_cancel = 0; cl_rs_ready = 0;
        cl_rq_addr = 0; cl_rq_wdata = 0; cl_rq_size = BUS_DWORD;
        cl_rq_len = 8'd0;
        cl_dbt_ready = 0; cl_dbr_valid = 0; cl_dbr_dst = 0;
        cl_sb_id_ack = 0; cl_sb_id_data = 0;
        t_code = 2'd0;

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
        check("PORT_COUNT reports one port, ID 3, eight ranges, W = 48",
              mmio_acked &&
              mmio_rd == {32'd0, 8'd48, 8'd8, PORT_ID, 8'd1});
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
        reg_wr(P_BOOT_ARG, 64'h0010_9000);
        check("the boot words reach the chiplet",
              cl_sb_boot_addr == 64'h0010_8000 &&
              cl_sb_boot_len == 64'h400 &&
              cl_sb_boot_arg == 64'h0010_9000);

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

        $display("\nA burst through the SoC");
        xmem[17'h00210] = 64'hD000_0000_0000_0000;
        xmem[17'h00211] = 64'hD000_0000_0000_0001;
        xmem[17'h00212] = 64'hD000_0000_0000_0002;
        cl_rq_len = 8'd2;
        link_request(64'h0010_1080, 64'd0, 1'b0);
        cl_rq_len = 8'd0;
        link_response;
        check("a read burst's first word arrives, more to come",
              rs_code == 2'd0 && rs_data == 64'hD000_0000_0000_0000);
        link_response;
        check("then its second",
              rs_code == 2'd0 && rs_data == 64'hD000_0000_0000_0001);
        link_response;
        check("and its third, marked last by the bridge",
              rs_code == 2'd0 && rs_data == 64'hD000_0000_0000_0002);
        link_access(64'h0010_1008, 64'd0, 1'b0);
        check("a single access follows the burst",
              rs_code == 2'd0 && rs_data == 64'hFEED_FACE_CAFE_BEEF);

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

        // The SoC has one full core and two small cores, IDs 1 and 2.
        @(posedge clk);
        cl_dbr_valid <= 1'b1; cl_dbr_dst <= 8'd2;
        @(posedge clk);
        cl_dbr_valid <= 1'b0;
        repeat (2) @(posedge clk);
        check("the chiplet's signal to a small core raises that core's lines",
              u_soc.ipi_out == 3'b100 && u_soc.mbox_ef == 3'b100);
        check("they reach the small core in its cluster",
              u_soc.g_cluster[0].u_cluster.irq_ipi == 2'b10 &&
              u_soc.g_cluster[0].u_cluster.ef_flags == 8'h10);
        force u_soc.bus_mmio_requester_valid = 1'b1;
        force u_soc.bus_mmio_requester_id    = 8'd2;
        mmio_access(MB_ACK, {56'd0, PORT_ID}, 1'b1, BUS_BYTE);
        release u_soc.bus_mmio_requester_valid;
        release u_soc.bus_mmio_requester_id;
        repeat (2) @(posedge clk);
        check("the small core's acknowledgement clears them",
              u_soc.ipi_out == 3'b000 && u_soc.mbox_ef == 3'b000);


        $display("\nTACC.STORE into memory the chiplet lends");
        reg_rd(P_WINDOW);
        check("WINDOW reads the base of port 0's window",
              mmio_acked && mmio_rd == WINDOW);
        prev_n = phy_launches;
        core_access(WINDOW + LENT_BASE, 64'd0, 1'b0);
        check("a core's load from the window is a bus fault",
              core_fault == 1'b1);
        check("and reaches neither the PHY nor the chiplet",
              phy_launches == prev_n && t_unit_count == 0);

        // Core 0 leaves reset with its fetch held off; the bench drives its
        // tile engine's dispatch and CSR boundary.
        force u_soc.cpu_icache_req[0]      = 1'b0;
        force u_soc.core_mex_valid[0]      = 1'b0;
        force u_soc.core_mex_ss[0]         = 2'd0;
        force u_soc.core_mex_op[0]         = MEX_TSYS;
        force u_soc.core_mex_funct[0]      = ETSYS_TACC_TRY;
        force u_soc.core_mex_funct_byte[0] = {5'd0, ETSYS_TACC_TRY};
        force u_soc.core_mex_ext_mod[0]    = 4'd8;
        force u_soc.core_mex_ext_active[0] = 1'b1;
        force u_soc.core_mex_gpr_val[0]    = 64'd0;
        force u_soc.core_mex_imm8[0]       = 8'd0;
        force u_soc.core_csr_wen[0]        = 1'b0;
        force u_soc.core_legacy_acc_wen[0] = 4'b0000;
        release u_soc.core_domain_reset;
        repeat (4) @(posedge clk);

        u_soc.u_memory.g_bank[0].u_sram.mem[8]  = unit_of(8'hA0);
        u_soc.u_memory.g_bank[0].u_sram.mem[9]  = unit_of(8'hA1);
        u_soc.u_memory.g_bank[0].u_sram.mem[10] = unit_of(8'hA2);
        u_soc.u_memory.g_bank[0].u_sram.mem[11] = unit_of(8'hA3);
        tacc_op(ETSYS_TACC_TRY);
        check("core 0 claims its TACC", mex_f == MEX_FAULT_NONE);
        tile_csr(CSR_TMODE, {60'd0, TMODE_8});
        tile_csr(CSR_TSRC0, 64'h200);
        tacc_op(ETSYS_TACC_LOAD);
        check("and loads an image from Bank 0", mex_f == MEX_FAULT_NONE);

        reg_wr(P_CTRL, 64'h29);              // ENABLE, DOORBELL_EN, BORROW_EN
        reg_rd(P_CTRL);
        check("BORROW_EN reads back", mmio_rd[5] == 1'b1);
        for (li = 0; li < 16; li = li + 1) lent[li] = unit_of(8'hEE);
        prev_n = phy_launches;
        tile_csr(CSR_TDST, WINDOW + LENT_BASE + 64'h40);
        tacc_op(ETSYS_TACC_STORE);
        check("TACC.STORE into the window completes without a fault",
              mex_f == MEX_FAULT_NONE);
        $display("    (dispatch to retirement: %0d cycles, the chiplet taking",
                 mex_cycles);
        $display("     each unit at once and answering 3 cycles after the last)");
        check("the chiplet took four units and answered once",
              t_unit_count == 4 && t_writes == 1);
        check("the chiplet holds the image at its own address 0x4040",
              lent[1] == unit_of(8'hA0) && lent[2] == unit_of(8'hA1) &&
              lent[3] == unit_of(8'hA2) && lent[4] == unit_of(8'hA3));
        check("around it the chiplet's memory is untouched",
              lent[0] == unit_of(8'hEE) && lent[5] == unit_of(8'hEE));
        check("the image never reached external memory",
              phy_launches == prev_n);
        check("the store retired only after the chiplet's answer",
              answer_cyc != 0 && answer_cyc < done_cyc);
        dbt_cyc = 0;
        force u_soc.bus_mmio_requester_valid = 1'b1;
        force u_soc.bus_mmio_requester_id    = 8'd0;
        mmio_access(MB_SEND, {56'd0, PORT_ID}, 1'b1, BUS_BYTE);
        release u_soc.bus_mmio_requester_valid;
        release u_soc.bus_mmio_requester_id;
        repeat (3) @(posedge clk);
        check("a doorbell rung after the store reaches the chiplet",
              cl_dbt_valid == 1'b1 && dbt_cyc != 0);
        check("after the chiplet answered for the data",
              dbt_cyc > answer_cyc);
        cl_dbt_ready <= 1'b1;
        @(posedge clk);
        cl_dbt_ready <= 1'b0;
        repeat (2) @(posedge clk);
        reg_rd(P_STATUS);
        check("BORROW_BUSY is clear once the store retired",
              mmio_rd[6] == 1'b0);

        // An FP64 image is one unit; the chiplet writes three of zeros.
        tile_csr(CSR_TMODE, {60'd0, TMODE_FP64});
        tacc_op(ETSYS_TACC_LOAD);
        check("an FP64 image loads", mex_f == MEX_FAULT_NONE);
        for (li = 0; li < 16; li = li + 1) lent[li] = unit_of(8'hEE);
        prev_n = t_unit_count;
        tile_csr(CSR_TDST, WINDOW + LENT_BASE + 64'h100);
        tacc_op(ETSYS_TACC_STORE);
        check("an FP64 TACC.STORE into the window completes",
              mex_f == MEX_FAULT_NONE);
        $display("    (dispatch to retirement: %0d cycles)", mex_cycles);
        check("one unit crosses the link", t_unit_count == prev_n + 1);
        check("the destination holds the canonical image",
              lent[4] == unit_of(8'hA0) && lent[5] == 512'd0 &&
              lent[6] == 512'd0 && lent[7] == 512'd0 &&
              lent[8] == unit_of(8'hEE));

        // Failures end the store with a bus fault at the image base.
        t_code = 2'd1;
        prev_n = t_unit_count;
        tile_csr(CSR_TDST, WINDOW + LENT_BASE + 64'h200);
        tacc_op(ETSYS_TACC_STORE);
        check("a refusing chiplet makes TACC.STORE a bus fault",
              mex_f == MEX_FAULT_BUS &&
              mex_fa == WINDOW + LENT_BASE + 64'h200);
        check("and its memory is untouched",
              t_unit_count == prev_n && lent[8] == unit_of(8'hEE));
        t_code = 2'd0;
        check("TACC stays valid after the failed store",
              u_soc.core_tacc_status_raw[0][TACC_STATUS_BIT_VALID]);

        reg_wr(P_CTRL, 64'h09);              // BORROW_EN clear
        prev_n = t_writes;
        tacc_op(ETSYS_TACC_STORE);
        check("with BORROW_EN clear TACC.STORE is a bus fault",
              mex_f == MEX_FAULT_BUS &&
              mex_fa == WINDOW + LENT_BASE + 64'h200);
        check("and nothing crosses the link", t_writes == prev_n);
        reg_wr(P_CTRL, 64'h29);

        tile_csr(CSR_TSRC0, WINDOW + LENT_BASE);
        prev_n = t_writes;
        tacc_op(ETSYS_TACC_LOAD);
        check("TACC.LOAD from the window is a bus fault at its base",
              mex_f == MEX_FAULT_BUS && mex_fa == WINDOW + LENT_BASE);
        check("and makes no link traffic", t_writes == prev_n);

        tile_csr(CSR_TDST, WINDOW + 64'h0001_0000_0000_0000);
        tacc_op(ETSYS_TACC_STORE);
        check("a store past the last port's window is a bus fault",
              mex_f == MEX_FAULT_BUS &&
              mex_fa == WINDOW + 64'h0001_0000_0000_0000);
        tile_csr(CSR_TDST, WINDOW + 64'h0000_FFFF_FFFF_FFC0);
        tacc_op(ETSYS_TACC_STORE);
        check("a store running off the window's end faults at that end",
              mex_f == MEX_FAULT_BUS &&
              mex_fa == WINDOW + 64'h0001_0000_0000_0000);
        force u_soc.core_domain_reset = 1'b1;

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
