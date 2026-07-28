// ============================================================================
// tb_tile_write_ack.v — Tile write retirement and final-ACK contract
// ============================================================================

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_tile_write_ack;

    reg clk;
    reg rst_n;

    reg         csr_wen;
    reg [7:0]   csr_addr;
    reg [63:0]  csr_wdata;
    wire [63:0] csr_rdata;

    reg         mex_valid;
    reg [1:0]   mex_ss;
    reg [1:0]   mex_op;
    reg [2:0]   mex_funct;
    reg [63:0]  mex_gpr_val;
    reg [7:0]   mex_imm8;
    reg [3:0]   mex_ext_mod;
    reg         mex_ext_active;
    wire        mex_done;
    wire        mex_busy;

    wire         tile_req;
    wire [31:0]  tile_addr;
    wire         tile_wen;
    wire [511:0] tile_wdata;
    reg [511:0]  tile_rdata;
    reg          tile_ack;

    wire         ext_tile_req;
    wire [63:0]  ext_tile_addr;
    wire         ext_tile_wen;
    wire [511:0] ext_tile_wdata;
    reg [511:0]  ext_tile_rdata;
    reg          ext_tile_ack;

    integer pass_count;
    integer fail_count;
    integer guard;

    // Distinct lower and upper lane groups make swapped, duplicated, zeroed,
    // or otherwise corrupted WMUL halves observable at the write ports.
    localparam [511:0] WMUL_A  = {{32{8'h04}}, {32{8'h02}}};
    localparam [511:0] WMUL_B  = {64{8'h03}};
    localparam [511:0] WMUL_LO = {32{16'h0006}};
    localparam [511:0] WMUL_HI = {32{16'h000C}};

    mp64_tile uut (
        .clk            (clk),
        .rst_n          (rst_n),
        .engine_reset   (1'b0),
        .caller_cancel  (4'd0),
        .caller_epochs  (32'd0),
        .engine_epoch   (),
        .csr_wen        (csr_wen),
        .csr_addr       (csr_addr),
        .csr_wdata      (csr_wdata),
        .csr_rdata      (csr_rdata),
        .mex_valid      (mex_valid),
        .mex_ss         (mex_ss),
        .mex_op         (mex_op),
        .mex_funct      (mex_funct),
        .mex_funct_byte ({5'd0, mex_funct}),
        .mex_gpr_val    (mex_gpr_val),
        .mex_imm8       (mex_imm8),
        .mex_ext_mod    (mex_ext_mod),
        .mex_ext_active (mex_ext_active),
        .mex_caller_id  (5'd0),
        .mex_priv       (1'b0),
        .mex_mpu_base   (64'd0),
        .mex_mpu_limit  (64'd0),
        .mex_mpu_enabled(1'b0),
        .mex_allow_cluster_spad(1'b0),
        .mex_engine_epoch(8'd0),
        .mex_caller_epoch(8'd0),
        .mex_caller_slot(2'd0),
        .mex_done       (mex_done),
        .mex_busy       (mex_busy),
        .mex_fault      (),
        .mex_fault_addr (),
        .mex_stall_cycle(),
        .tacc_status_raw(),
        .tacc_ctl_valid (1'b0),
        .tacc_ctl_caller_id(5'd0),
        .tacc_ctl_priv  (1'b0),
        .tacc_ctl_wdata (64'd0),
        .tacc_ctl_done  (),
        .tacc_ctl_fault (),
        .tile_req       (tile_req),
        .tile_addr      (tile_addr),
        .tile_wen       (tile_wen),
        .tile_wdata     (tile_wdata),
        .tile_rdata     (tile_rdata),
        .tile_ack       (tile_ack),
        .ext_tile_req   (ext_tile_req),
        .ext_tile_addr  (ext_tile_addr),
        .ext_tile_wen   (ext_tile_wen),
        .ext_tile_wdata (ext_tile_wdata),
        .ext_tile_rdata (ext_tile_rdata),
        .ext_tile_ack   (ext_tile_ack)
    );

    initial clk = 1'b0;
    always #5 clk = ~clk;

    task clock;
    begin
        @(posedge clk);
        #1;
    end
    endtask

    task check;
        input [511:0] label;
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

    task csr_write;
        input [7:0]  address;
        input [63:0] value;
    begin
        csr_wen = 1'b1;
        csr_addr = address;
        csr_wdata = value;
        clock;
        csr_wen = 1'b0;
    end
    endtask

    task dispatch;
        input [1:0] ss;
        input [1:0] op;
        input [2:0] funct;
        input       ext_active;
        input [3:0] ext_mod;
    begin
        mex_ss = ss;
        mex_op = op;
        mex_funct = funct;
        mex_gpr_val = 64'd0;
        mex_imm8 = 8'd0;
        mex_ext_active = ext_active;
        mex_ext_mod = ext_mod;
        mex_valid = 1'b1;
        clock;
        mex_valid = 1'b0;
    end
    endtask

    task wait_internal;
        input expected_wen;
        input [31:0] expected_addr;
    begin
        guard = 0;
        while (!(tile_req && tile_wen == expected_wen
                         && tile_addr == expected_addr)) begin
            clock;
            guard = guard + 1;
            if (guard > 40) begin
                $display("  FAIL: timeout waiting for internal request");
                fail_count = fail_count + 1;
                $fatal(1, "timed out waiting for internal tile request");
            end
        end
    end
    endtask

    task wait_external;
        input expected_wen;
        input [63:0] expected_addr;
    begin
        guard = 0;
        while (!(ext_tile_req && ext_tile_wen == expected_wen
                             && ext_tile_addr == expected_addr)) begin
            clock;
            guard = guard + 1;
            if (guard > 40) begin
                $display("  FAIL: timeout waiting for external request");
                fail_count = fail_count + 1;
                $fatal(1, "timed out waiting for external tile request");
            end
        end
    end
    endtask

    task ack_internal;
        input [511:0] read_data;
    begin
        tile_rdata = read_data;
        tile_ack = 1'b1;
        clock;
        tile_ack = 1'b0;
    end
    endtask

    task ack_external;
        input [511:0] read_data;
    begin
        ext_tile_rdata = read_data;
        ext_tile_ack = 1'b1;
        clock;
        ext_tile_ack = 1'b0;
    end
    endtask

    task expect_no_retirement;
        input [511:0] label;
        input integer cycles;
        integer n;
        integer failed;
    begin
        failed = 0;
        for (n = 0; n < cycles; n = n + 1) begin
            clock;
            if (mex_done || !mex_busy) begin
                $display("  FAIL: %0s", label);
                fail_count = fail_count + 1;
                failed = 1;
                n = cycles;
            end else if (tile_req || ext_tile_req) begin
                $display("  FAIL: %0s (request repeated before ACK)", label);
                fail_count = fail_count + 1;
                failed = 1;
                n = cycles;
            end
        end
        if (!failed) begin
            $display("  PASS: %0s", label);
            pass_count = pass_count + 1;
        end
    end
    endtask

    task finish_operation;
        input [511:0] label;
    begin
        guard = 0;
        while (!mex_done) begin
            clock;
            guard = guard + 1;
            if (guard > 20) begin
                $display("  FAIL: %0s", label);
                fail_count = fail_count + 1;
                $fatal(1, "timed out waiting for tile retirement");
            end
        end
        check(label, mex_done && !mex_busy);
        clock;
    end
    endtask

    initial begin
        pass_count = 0;
        fail_count = 0;
        rst_n = 1'b0;
        csr_wen = 1'b0;
        csr_addr = 8'd0;
        csr_wdata = 64'd0;
        mex_valid = 1'b0;
        mex_ss = 2'd0;
        mex_op = 2'd0;
        mex_funct = 3'd0;
        mex_gpr_val = 64'd0;
        mex_imm8 = 8'd0;
        mex_ext_mod = 4'd0;
        mex_ext_active = 1'b0;
        tile_rdata = 512'd0;
        tile_ack = 1'b0;
        ext_tile_rdata = 512'd0;
        ext_tile_ack = 1'b0;

        repeat (3) clock;
        rst_n = 1'b1;
        clock;
        check("external write direction resets low", !ext_tile_wen);

        // TSYS.ZERO must not retire before its internal write completes.
        csr_write(CSR_TDST, 64'h0000_0000_0000_0180);
        dispatch(2'd0, MEX_TSYS, TSYS_ZERO, 1'b0, 4'd0);
        wait_internal(1'b1, 32'h0000_0180);
        check("TSYS.ZERO writes an all-zero tile", tile_wdata == 512'd0);
        expect_no_retirement("TSYS.ZERO waits for its write ACK", 3);
        ack_internal(512'd0);
        finish_operation("TSYS.ZERO retires after write ACK");

        // The same ZERO operation routes an external destination through the
        // external port and waits for its completion.
        csr_write(CSR_TDST, 64'h0000_0000_0010_0000);
        dispatch(2'd0, MEX_TSYS, TSYS_ZERO, 1'b0, 4'd0);
        wait_external(1'b1, 64'h0000_0000_0010_0000);
        check("external TSYS.ZERO writes an all-zero tile",
              ext_tile_wdata == 512'd0);
        expect_no_retirement("external TSYS.ZERO waits for ACK", 3);
        ack_external(512'd0);
        finish_operation("external TSYS.ZERO retires after ACK");
        check("external write direction clears after request", !ext_tile_wen);

        // Ordinary internal compute/store completion is also ACK-bounded.
        csr_write(CSR_TSRC0, 64'h0000);
        csr_write(CSR_TSRC1, 64'h0040);
        csr_write(CSR_TDST,  64'h0080);
        csr_write(CSR_TMODE, 64'd0);
        dispatch(2'd0, MEX_TALU, TALU_ADD, 1'b0, 4'd0);
        wait_internal(1'b0, 32'h0000);
        ack_internal({64{8'h01}});
        wait_internal(1'b0, 32'h0040);
        ack_internal({64{8'h02}});
        wait_internal(1'b1, 32'h0080);
        check("ordinary store carries computed result",
              tile_wdata == {64{8'h03}});
        expect_no_retirement("ordinary store waits for final ACK", 3);
        ack_internal(512'd0);
        finish_operation("ordinary store retires after final ACK");

        // Internal WMUL has two stores; neither the second request nor MEX
        // retirement may precede the corresponding acknowledgement.
        csr_write(CSR_TSRC0, 64'h0000);
        csr_write(CSR_TSRC1, 64'h0040);
        csr_write(CSR_TDST,  64'h0200);
        csr_write(CSR_TMODE, 64'd0);
        dispatch(2'd0, MEX_TMUL, TMUL_WMUL, 1'b0, 4'd0);
        wait_internal(1'b0, 32'h0000);
        ack_internal(WMUL_A);
        wait_internal(1'b0, 32'h0040);
        ack_internal(WMUL_B);
        wait_internal(1'b1, 32'h0200);
        check("WMUL first store carries lower widening half",
              tile_wdata == WMUL_LO);
        expect_no_retirement("WMUL waits for first store ACK", 3);
        check("WMUL second store is not issued early", !tile_req);
        ack_internal(512'd0);
        wait_internal(1'b1, 32'h0240);
        check("WMUL second store carries upper widening half",
              tile_wdata == WMUL_HI);
        expect_no_retirement("WMUL waits for second store ACK", 3);
        ack_internal(512'd0);
        finish_operation("WMUL retires after second store ACK");

        // External WMUL must preserve both halves and wait for the second
        // external burst rather than silently dropping it.
        csr_write(CSR_TSRC0, 64'h0000);
        csr_write(CSR_TSRC1, 64'h0040);
        csr_write(CSR_TDST,  64'h0010_0000);
        dispatch(2'd0, MEX_TMUL, TMUL_WMUL, 1'b0, 4'd0);
        wait_internal(1'b0, 32'h0000);
        ack_internal(WMUL_A);
        wait_internal(1'b0, 32'h0040);
        ack_internal(WMUL_B);
        wait_external(1'b1, 64'h0010_0000);
        check("external WMUL first burst carries lower widening half",
              ext_tile_wdata == WMUL_LO);
        expect_no_retirement("external WMUL waits for first burst ACK", 3);
        ack_external(512'd0);
        wait_external(1'b1, 64'h0010_0040);
        check("external WMUL second burst carries upper widening half",
              ext_tile_wdata == WMUL_HI);
        expect_no_retirement("external WMUL waits for second burst ACK", 3);
        ack_external(512'd0);
        finish_operation("external WMUL retires after both burst ACKs");

        // Route the second WMUL tile from its own address.  The double-width
        // result may cross between memory apertures even though both halves
        // belong to one architectural instruction.
        csr_write(CSR_TSRC0, 64'h0000);
        csr_write(CSR_TSRC1, 64'h0040);
        csr_write(CSR_TDST,  64'h000F_FFC0);
        dispatch(2'd0, MEX_TMUL, TMUL_WMUL, 1'b0, 4'd0);
        wait_internal(1'b0, 32'h0000);
        ack_internal(WMUL_A);
        wait_internal(1'b0, 32'h0040);
        ack_internal(WMUL_B);
        wait_internal(1'b1, 32'h000F_FFC0);
        check("Bank-0 WMUL half carries lower widening result",
              tile_wdata == WMUL_LO);
        ack_internal(512'd0);
        wait_external(1'b1, 64'h0010_0000);
        check("cross-aperture external half carries upper result",
              ext_tile_wdata == WMUL_HI);
        expect_no_retirement(
            "Bank-0/external WMUL waits for cross-aperture second ACK", 2);
        ack_external(512'd0);
        finish_operation(
            "Bank-0/external WMUL retires after cross-aperture ACK");

        csr_write(CSR_TSRC0, 64'h0000);
        csr_write(CSR_TSRC1, 64'h0040);
        csr_write(CSR_TDST,  64'h0000_0000_FFCF_FFC0);
        dispatch(2'd0, MEX_TMUL, TMUL_WMUL, 1'b0, 4'd0);
        wait_internal(1'b0, 32'h0000);
        ack_internal(WMUL_A);
        wait_internal(1'b0, 32'h0040);
        ack_internal(WMUL_B);
        wait_external(1'b1, 64'h0000_0000_FFCF_FFC0);
        check("external WMUL half carries lower widening result",
              ext_tile_wdata == WMUL_LO);
        ack_external(512'd0);
        wait_internal(1'b1, 32'hFFD0_0000);
        check("cross-aperture HBW half carries upper result",
              tile_wdata == WMUL_HI);
        expect_no_retirement(
            "external/HBW WMUL waits for cross-aperture second ACK", 2);
        ack_internal(512'd0);
        finish_operation(
            "external/HBW WMUL retires after cross-aperture ACK");

        // STORE2D must serialize each read-modify-write row.  In particular,
        // the next row read cannot consume the previous row's write ACK.
        csr_write(CSR_TSRC0,    64'h0000);
        csr_write(CSR_SB,       64'd0);
        csr_write(CSR_SR,       64'd0);
        csr_write(CSR_SC,       64'd12);
        csr_write(CSR_SW,       64'd16);
        csr_write(CSR_TSTRIDE_R,64'd64);
        csr_write(CSR_TTILE_H,  64'd2);
        csr_write(CSR_TTILE_W,  64'd8);
        dispatch(2'd0, MEX_TSYS, 3'd1, 1'b1, 4'd8);
        wait_internal(1'b0, 32'h0000);
        ack_internal({64{8'hA5}});
        wait_internal(1'b0, 32'h0300);
        ack_internal(512'd0);
        wait_internal(1'b1, 32'h0300);
        expect_no_retirement("STORE2D waits for first row write ACK", 3);
        check("STORE2D cannot issue next row read early", !tile_req);
        ack_internal(512'd0);
        wait_internal(1'b0, 32'h0340);
        ack_internal(512'd0);
        wait_internal(1'b1, 32'h0340);
        expect_no_retirement("STORE2D waits for final row write ACK", 3);
        ack_internal(512'd0);
        finish_operation("STORE2D retires after final row write ACK");

        $display("");
        if (fail_count == 0)
            $display("tb_tile_write_ack: ALL %0d assertions PASSED",
                     pass_count);
        else
            $display("tb_tile_write_ack: %0d PASSED, %0d FAILED",
                     pass_count, fail_count);

        if (fail_count != 0)
            $fatal(1, "tb_tile_write_ack failed");
        $finish(0);
    end

    initial begin
        #300000;
        $display("tb_tile_write_ack: TIMEOUT");
        $fatal(1, "tb_tile_write_ack timeout");
    end

endmodule
