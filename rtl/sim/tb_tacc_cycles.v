// ============================================================================
// tb_tacc_cycles.v — Tile-leaf TACC lifecycle timing and pulse retention
// ============================================================================

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_tacc_cycles;

    reg clk;
    reg rst_n;
    reg engine_reset;

    reg         mex_valid;
    reg [1:0]   mex_ss;
    reg [1:0]   mex_op;
    reg [2:0]   mex_funct;
    reg [7:0]   mex_funct_byte;
    reg [3:0]   mex_ext_mod;
    reg         mex_ext_active;
    reg [4:0]   mex_caller_id;
    reg         mex_priv;
    reg [63:0]  mex_mpu_base;
    reg [63:0]  mex_mpu_limit;
    reg         mex_mpu_enabled;
    reg [7:0]   mex_engine_epoch;
    reg [7:0]   mex_caller_epoch;
    reg [1:0]   mex_caller_slot;
    reg [3:0]   caller_cancel;
    reg [31:0]  caller_epochs;

    wire        mex_done;
    wire        mex_busy;
    wire [2:0]  mex_fault;
    wire [63:0] mex_fault_addr;
    wire        mex_stall_cycle;
    wire [7:0]  engine_epoch;

    reg         tacc_ctl_valid;
    reg [4:0]   tacc_ctl_caller_id;
    reg         tacc_ctl_priv;
    reg [63:0]  tacc_ctl_wdata;
    wire        tacc_ctl_done;
    wire [2:0]  tacc_ctl_fault;
    wire [63:0] tacc_status_raw;
    wire        tacc_xfer_req;
    wire        tacc_xfer_store;
    wire        tacc_xfer_ext;
    wire [63:0] tacc_xfer_base;
    wire [2:0]  tacc_xfer_format_ew;
    wire [7:0]  tacc_xfer_token;
    wire [2047:0] tacc_xfer_store_image;
    wire        tacc_xfer_cancel;
    wire        tacc_xfer_finish;
    reg         tacc_xfer_done;
    reg [7:0]   tacc_xfer_response_token;
    reg [2:0]   tacc_xfer_fault;
    reg [63:0]  tacc_xfer_fault_addr;
    reg [2047:0] tacc_xfer_load_image;

    reg         csr_wen;
    reg [7:0]   csr_addr;
    reg [63:0]  csr_wdata;
    wire [63:0] csr_rdata;

    wire [255:0] legacy_acc_state;
    wire         acc_zero_consumed;
    wire         tile_req;
    wire         tile_wen;
    wire         ext_tile_req;
    wire         ext_tile_wen;

    integer pass_count;
    integer fail_count;

    mp64_tile #(
        .TACC_CALLER_BASE (5'd4),
        .TACC_CALLER_COUNT(4)
    ) uut (
        .clk                    (clk),
        .rst_n                  (rst_n),
        .csr_wen                (csr_wen),
        .csr_addr               (csr_addr),
        .csr_wdata              (csr_wdata),
        .csr_rdata              (csr_rdata),
        .mex_valid              (mex_valid),
        .mex_ss                 (mex_ss),
        .mex_op                 (mex_op),
        .mex_funct              (mex_funct),
        .mex_funct_byte         (mex_funct_byte),
        .mex_gpr_val            (64'd0),
        .mex_imm8               (8'd0),
        .mex_ext_mod            (mex_ext_mod),
        .mex_ext_active         (mex_ext_active),
        .mex_caller_id          (mex_caller_id),
        .mex_priv               (mex_priv),
        .mex_mpu_base           (mex_mpu_base),
        .mex_mpu_limit          (mex_mpu_limit),
        .mex_mpu_enabled        (mex_mpu_enabled),
        .mex_allow_cluster_spad (1'b0),
        .mex_engine_epoch       (mex_engine_epoch),
        .mex_caller_epoch       (mex_caller_epoch),
        .mex_caller_slot        (mex_caller_slot),
        .engine_reset           (engine_reset),
        .caller_cancel          (caller_cancel),
        .caller_epochs          (caller_epochs),
        .engine_epoch           (engine_epoch),
        .mex_retire             (1'b1),
        .mex_done               (mex_done),
        .mex_busy               (mex_busy),
        .mex_fault              (mex_fault),
        .mex_fault_addr         (mex_fault_addr),
        .mex_stall_cycle        (mex_stall_cycle),
        .tacc_status_raw        (tacc_status_raw),
        .tacc_ctl_valid         (tacc_ctl_valid),
        .tacc_ctl_caller_id     (tacc_ctl_caller_id),
        .tacc_ctl_priv          (tacc_ctl_priv),
        .tacc_ctl_wdata         (tacc_ctl_wdata),
        .tacc_ctl_done          (tacc_ctl_done),
        .tacc_ctl_fault         (tacc_ctl_fault),
        .tacc_xfer_req          (tacc_xfer_req),
        .tacc_xfer_store        (tacc_xfer_store),
        .tacc_xfer_ext          (tacc_xfer_ext),
        .tacc_xfer_base         (tacc_xfer_base),
        .tacc_xfer_format_ew    (tacc_xfer_format_ew),
        .tacc_xfer_token        (tacc_xfer_token),
        .tacc_xfer_store_image  (tacc_xfer_store_image),
        .tacc_xfer_cancel       (tacc_xfer_cancel),
        .tacc_xfer_finish       (tacc_xfer_finish),
        .tacc_xfer_done         (tacc_xfer_done),
        .tacc_xfer_response_token(tacc_xfer_response_token),
        .tacc_xfer_fault        (tacc_xfer_fault),
        .tacc_xfer_fault_addr   (tacc_xfer_fault_addr),
        .tacc_xfer_load_image   (tacc_xfer_load_image),
        .legacy_acc_state       (legacy_acc_state),
        .legacy_acc_wen         (4'd0),
        .legacy_acc_wdata       (256'd0),
        .cfg_load               (1'b0),
        .cfg_tmode              (64'd0),
        .cfg_tctrl              (64'd0),
        .cfg_tsrc0              (64'd0),
        .cfg_tsrc1              (64'd0),
        .cfg_tdst               (64'd0),
        .cfg_sb                 (64'd0),
        .cfg_sr                 (64'd0),
        .cfg_sc                 (64'd0),
        .cfg_sw                 (64'd0),
        .cfg_tstride_r          (64'd0),
        .cfg_tstride_c          (64'd0),
        .cfg_ttile_h            (64'd8),
        .cfg_ttile_w            (64'd8),
        .acc_zero_consumed      (acc_zero_consumed),
        .tile_req               (tile_req),
        .tile_addr              (),
        .tile_wen               (tile_wen),
        .tile_wdata             (),
        .tile_rdata             (512'd0),
        .tile_ack               (1'b0),
        .ext_tile_req           (ext_tile_req),
        .ext_tile_addr          (),
        .ext_tile_wen           (ext_tile_wen),
        .ext_tile_wdata         (),
        .ext_tile_rdata         (512'd0),
        .ext_tile_ack           (1'b0)
    );

    initial clk = 1'b0;
    always #5 clk = ~clk;

    task tick;
    begin
        @(posedge clk);
        #1;
    end
    endtask

    task check;
        input [767:0] label;
        input         condition;
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

    task set_lifecycle;
        input [2:0] funct;
        input [7:0] funct_byte;
    begin
        mex_ss          = 2'd0;
        mex_op          = MEX_TSYS;
        mex_funct       = funct;
        mex_funct_byte  = funct_byte;
        mex_ext_mod     = 4'd8;
        mex_ext_active  = 1'b1;
        mex_caller_id   = 5'd4;
        mex_caller_slot = 2'd0;
    end
    endtask

    task dispatch_success;
        input [2:0] funct;
    begin
        set_lifecycle(funct, {5'd0, funct});
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("admitted lifecycle is busy after capture",
              mex_busy && !mex_done &&
              tacc_status_raw[TACC_STATUS_BIT_BUSY]);
        tick;
        check("admitted lifecycle completes on second base cycle",
              mex_done && !mex_busy && mex_fault == MEX_FAULT_NONE);
        tick;
        check("lifecycle completion pulse drops", !mex_done);
    end
    endtask

    task dispatch_fault;
        input [2:0] funct;
        input [7:0] funct_byte;
    begin
        set_lifecycle(funct, funct_byte);
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("validation fault completes in first base cycle",
              mex_done && !mex_busy && mex_fault == MEX_FAULT_ILLEGAL);
        check("validation fault never publishes TACC BUSY",
              !tacc_status_raw[TACC_STATUS_BIT_BUSY]);
        check("validation fault issues no tile-memory request",
              !tile_req && !ext_tile_req);
        tick;
        check("validation-fault pulse drops", !mex_done);
    end
    endtask

    task write_csr;
        input [7:0]  address;
        input [63:0] value;
    begin
        csr_addr  = address;
        csr_wdata = value;
        csr_wen   = 1'b1;
        tick;
        csr_wen   = 1'b0;
    end
    endtask

    task dispatch_address_fault;
        input [2:0]  expected_fault;
        input [63:0] expected_addr;
    begin
        set_lifecycle(ETSYS_TACC_LOAD,
                      {5'd0, ETSYS_TACC_LOAD});
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("address preflight fault is immediate and nonbusy",
              mex_done && !mex_busy &&
              mex_fault == expected_fault);
        check("address preflight reports the first forbidden byte",
              mex_fault_addr == expected_addr);
        check("address preflight emits no stage or memory request",
              !tacc_xfer_req && !tile_req && !ext_tile_req);
        tick;
    end
    endtask

    initial begin
        pass_count = 0;
        fail_count = 0;

        rst_n = 1'b0;
        engine_reset = 1'b0;
        mex_valid = 1'b0;
        mex_ss = 2'd0;
        mex_op = MEX_TSYS;
        mex_funct = ETSYS_TACC_TRY;
        mex_funct_byte = {5'd0, ETSYS_TACC_TRY};
        mex_ext_mod = 4'd8;
        mex_ext_active = 1'b1;
        mex_caller_id = 5'd4;
        mex_priv = 1'b0;
        mex_mpu_base = 64'd0;
        mex_mpu_limit = 64'd0;
        mex_mpu_enabled = 1'b0;
        mex_engine_epoch = 8'd0;
        mex_caller_epoch = 8'd0;
        mex_caller_slot = 2'd0;
        caller_cancel = 4'd0;
        caller_epochs = 32'd0;
        tacc_ctl_valid = 1'b0;
        tacc_ctl_caller_id = 5'd4;
        tacc_ctl_priv = 1'b0;
        tacc_ctl_wdata = 64'd0;
        tacc_xfer_done = 1'b0;
        tacc_xfer_response_token = 8'd0;
        tacc_xfer_fault = MEX_FAULT_NONE;
        tacc_xfer_fault_addr = 64'd0;
        tacc_xfer_load_image = {256{8'h96}};
        csr_wen = 1'b0;
        csr_addr = 8'd0;
        csr_wdata = 64'd0;

        repeat (3) tick;
        rst_n = 1'b1;
        tick;

        check("reset exposes exact FREE status",
              tacc_status_raw == {43'd0, TACC_OWNER_NONE, 16'd0});

        // Successful lifecycle operations expose BUSY for one interval and
        // retire after exactly two direct leaf cycles.
        dispatch_success(ETSYS_TACC_TRY);
        check("TRY commits owner only at its terminal boundary",
              tacc_status_raw[TACC_STATUS_BIT_CLAIMED] &&
              !tacc_status_raw[TACC_STATUS_BIT_VALID] &&
              tacc_status_raw[TACC_STATUS_OWNER_MSB:
                              TACC_STATUS_OWNER_LSB] == 5'd4);

        // Noncanonical lifecycle and STORE-without-valid-state fail in one
        // cycle and cannot enter legacy memory handling.
        dispatch_fault(ETSYS_TACC_CLEAR, 8'h23);
        dispatch_fault(ETSYS_TACC_STORE,
                       {5'd0, ETSYS_TACC_STORE});

        // A canonical LOAD waits on the chip-wide stage and publishes its
        // complete image only on the following retirement edge.
        set_lifecycle(ETSYS_TACC_LOAD,
                      {5'd0, ETSYS_TACC_LOAD});
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("LOAD capture raises BUSY without ordinary tile traffic",
              mex_busy && !mex_done && tacc_xfer_req &&
              !tile_req && !ext_tile_req);
        check("LOAD stage request carries canonical internal base",
              !tacc_xfer_store && !tacc_xfer_ext &&
              tacc_xfer_base == 64'd0);
        tacc_xfer_response_token = tacc_xfer_token;
        tacc_xfer_done = 1'b1;
        tick;
        tacc_xfer_done = 1'b0;
        check("LOAD stage response reaches MEX terminal",
              mex_done && mex_fault == MEX_FAULT_NONE &&
              tacc_xfer_finish);
        tick;
        check("LOAD retirement publishes VALID clean state",
              tacc_status_raw[TACC_STATUS_BIT_VALID] &&
              !tacc_status_raw[TACC_STATUS_BIT_DIRTY]);

        write_csr(CSR_TSRC0, 64'h0000_0000_0000_0041);
        dispatch_address_fault(MEX_FAULT_ALIGN,
                               64'h0000_0000_0000_0041);
        write_csr(CSR_TSRC0, 64'h0000_0000_FF40_0000);
        dispatch_address_fault(MEX_FAULT_BUS,
                               64'h0000_0000_FF40_0000);
        write_csr(CSR_TSRC0, 64'h0000_0000_FFD0_0000);
        mex_priv = 1'b1;
        dispatch_address_fault(MEX_FAULT_PRIV,
                               64'h0000_0000_FFD0_0000);
        write_csr(CSR_TSRC0, 64'h0000_0000_0010_0000);
        mex_mpu_base = 64'h0000_0000_0010_0000;
        mex_mpu_limit = 64'h0000_0000_0010_0080;
        mex_mpu_enabled = 1'b1;
        dispatch_address_fault(MEX_FAULT_PRIV,
                               64'h0000_0000_0010_0080);
        mex_priv = 1'b0;
        mex_mpu_enabled = 1'b0;
        write_csr(CSR_TSRC0, 64'd0);

        // A canonical TAMAC is intercepted by the state leaf and remains a
        // precise one-cycle illegal operation until its arithmetic landing.
        mex_ss = 2'd0;
        mex_op = MEX_TMUL;
        mex_funct = TMUL_TAMAC;
        mex_funct_byte = {5'd0, TMUL_TAMAC};
        mex_ext_mod = 4'd0;
        mex_ext_active = 1'b0;
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("pre-arithmetic TAMAC fails closed in one cycle",
              mex_done && !mex_busy && mex_fault == MEX_FAULT_ILLEGAL);
        check("pre-arithmetic TAMAC issues no memory request",
              !tile_req && !ext_tile_req);
        tick;

        // Restore lifecycle fields for control/admission collision checks.
        set_lifecycle(ETSYS_TACC_RELEASE,
                      {5'd0, ETSYS_TACC_RELEASE});
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("RELEASE is active before terminal", mex_busy && !mex_done);
        tick;
        check("RELEASE completes normally", mex_done);
        tick;

        // A same-cycle authorized FORCE must displace, not drop, a one-cycle
        // MEX pulse.  The held slot accounts one stall and revalidates later.
        set_lifecycle(ETSYS_TACC_TRY,
                      {5'd0, ETSYS_TACC_TRY});
        tacc_ctl_priv = 1'b0;
        tacc_ctl_wdata = 64'd1;
        mex_valid = 1'b1;
        tacc_ctl_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        tacc_ctl_valid = 1'b0;
        check("same-cycle FORCE is acknowledged", tacc_ctl_done &&
              tacc_ctl_fault == MEX_FAULT_NONE);
        check("same-cycle FORCE retains request without early admission",
              !mex_done && mex_busy &&
              !tacc_status_raw[TACC_STATUS_BIT_BUSY]);
        check("force-displacement wait is an explicit stall",
              mex_stall_cycle);

        // Corrupt the now-deasserted live bus. The retained request must use
        // the payload and identity captured with the original one-cycle pulse.
        mex_funct = ETSYS_TACC_RESERVED;
        mex_funct_byte = {5'd0, ETSYS_TACC_RESERVED};
        mex_caller_id = 5'd5;
        mex_caller_slot = 2'd1;
        tick;
        check("retained request is admitted after FORCE",
              !mex_done && mex_busy &&
              tacc_status_raw[TACC_STATUS_BIT_BUSY]);
        check("admission cycle is useful work, not a stall",
              !mex_stall_cycle);
        tick;
        check("retained one-cycle request eventually completes",
              mex_done && !mex_busy && mex_fault == MEX_FAULT_NONE);
        tick;
        check("retained request reclaims the wiped bank",
              tacc_status_raw[TACC_STATUS_BIT_CLAIMED] &&
              tacc_status_raw[TACC_STATUS_OWNER_MSB:
                              TACC_STATUS_OWNER_LSB] == 5'd4);

        // A user FORCE faults independently but is not an accepted recovery
        // action, so it must not fence a simultaneous lifecycle request.
        set_lifecycle(ETSYS_TACC_TRY,
                      {5'd0, ETSYS_TACC_TRY});
        tacc_ctl_priv = 1'b1;
        tacc_ctl_wdata = 64'd1;
        mex_valid = 1'b1;
        tacc_ctl_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        tacc_ctl_valid = 1'b0;
        check("user FORCE reports its privilege fault with acknowledgement",
              tacc_ctl_done && tacc_ctl_fault == MEX_FAULT_PRIV);
        check("rejected FORCE does not displace lifecycle admission",
              mex_busy && tacc_status_raw[TACC_STATUS_BIT_BUSY] &&
              !mex_stall_cycle);
        tick;
        check("lifecycle still retires beside rejected control",
              mex_done && mex_fault == MEX_FAULT_NONE);
        tick;
        tacc_ctl_priv = 1'b0;

        // Individual caller cancellation terminates without retirement and
        // leaves the already-owned shared bank intact.
        set_lifecycle(ETSYS_TACC_CLEAR,
                      {5'd0, ETSYS_TACC_CLEAR});
        csr_wen = 1'b1;
        csr_addr = CSR_TMODE;
        csr_wdata = TMODE_16 | (64'd1 << TMODE_BIT_SIGNED);
        tick;
        csr_wen = 1'b0;
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("cancel target is admitted", mex_busy &&
              tacc_status_raw[TACC_STATUS_BIT_BUSY]);
        caller_cancel[0] = 1'b1;
        tick;
        caller_cancel[0] = 1'b0;
        check("individual cancel suppresses completion",
              !mex_done && !mex_busy && mex_fault == MEX_FAULT_NONE);
        check("individual cancel preserves shared ownership",
              tacc_status_raw[TACC_STATUS_BIT_CLAIMED] &&
              tacc_status_raw[TACC_STATUS_OWNER_MSB:
                              TACC_STATUS_OWNER_LSB] == 5'd4);

        // The leaf publishes completion one interval before the caller samples
        // it.  Cancel on that sampling edge: CLEAR must not become visible
        // merely because its local response was already high.
        set_lifecycle(ETSYS_TACC_CLEAR,
                      {5'd0, ETSYS_TACC_CLEAR});
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("post-publication cancel target is admitted",
              mex_busy && tacc_status_raw[TACC_STATUS_BIT_BUSY]);
        tick;
        check("response publication retains BUSY and pre-commit state",
              mex_done && !mex_busy &&
              tacc_status_raw[TACC_STATUS_BIT_BUSY] &&
              !tacc_status_raw[TACC_STATUS_BIT_VALID]);
        caller_cancel[0] = 1'b1;
        tick;
        caller_cancel[0] = 1'b0;
        check("sampling-edge cancel suppresses lifecycle completion",
              !mex_done && !mex_busy && mex_fault == MEX_FAULT_NONE);
        check("sampling-edge cancel suppresses CLEAR mutation",
              tacc_status_raw[TACC_STATUS_BIT_CLAIMED] &&
              !tacc_status_raw[TACC_STATUS_BIT_VALID] &&
              !tacc_status_raw[TACC_STATUS_BIT_DIRTY] &&
              tacc_status_raw[TACC_STATUS_OWNER_MSB:
                              TACC_STATUS_OWNER_LSB] == 5'd4);

        // Engine reset is broader: it immediately wipes ownership and cannot
        // produce a late lifecycle completion.
        set_lifecycle(ETSYS_TACC_CLEAR,
                      {5'd0, ETSYS_TACC_CLEAR});
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("engine-reset target is admitted", mex_busy);
        engine_reset = 1'b1;
        tick;
        check("engine reset cancels without a completion",
              !mex_done && !mex_busy);
        check("engine reset restores exact FREE status",
              tacc_status_raw == {43'd0, TACC_OWNER_NONE, 16'd0});
        engine_reset = 1'b0;
        tick;
        check("engine reset produces no late completion", !mex_done);

        check("lifecycle-only bench observed no memory write request",
              !tile_wen && !ext_tile_wen);
        check("lifecycle faults leave fault address neutral",
              mex_fault_addr == 64'd0);

        if (fail_count == 0) begin
            $display("ALL %0d TACC CYCLE TESTS PASSED", pass_count);
            $finish;
        end else begin
            $fatal(1, "%0d TACC cycle checks failed", fail_count);
        end
    end

endmodule
