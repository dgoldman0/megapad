// ============================================================================
// tb_tacc.v — TACC lifecycle, ownership, recovery, and cancellation contract
// ============================================================================

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_tacc;

    reg clk;
    reg rst_n;
    reg engine_reset;

    reg         req_valid;
    wire        req_ready;
    reg         req_is_tamac;
    reg [2:0]   req_funct;
    reg         req_canonical;
    reg [4:0]   req_caller_id;
    reg [1:0]   req_caller_slot;
    reg [2:0]   req_format_ew;
    reg         req_format_signed;
    reg [63:0]  req_image_addr;
    reg [2:0]   req_preflight_fault;
    reg [63:0]  req_preflight_fault_addr;
    reg         req_cancel;
    wire        req_done;
    wire        req_busy;
    wire [2:0]  req_fault;
    wire [63:0] req_fault_addr;
    wire         tamac_start;
    reg          tamac_done;
    reg [2:0]    tamac_fault;
    reg [63:0]   tamac_fault_addr;
    reg [2047:0] tamac_result_image;

    wire         xfer_req;
    wire         xfer_store;
    wire [63:0]  xfer_base;
    wire [2:0]   xfer_format_ew;
    wire [7:0]   xfer_token;
    wire [2047:0] xfer_store_image;
    wire         xfer_cancel;
    wire         xfer_finish;
    reg          xfer_done;
    reg [7:0]    xfer_response_token;
    reg [2:0]    xfer_fault;
    reg [63:0]   xfer_fault_addr;
    reg [2047:0] xfer_load_image;

    reg         force_valid;
    wire        force_ready;
    reg         force_priv;
    reg [63:0]  force_wdata;
    reg [4:0]   force_caller_id;
    wire        force_done;
    wire [2:0]  force_fault;

    wire [63:0]   status_raw;
    wire [2047:0] bank_state;

    integer pass_count;
    integer fail_count;
    reg [63:0] metadata_snapshot;
    reg [2047:0] bank_snapshot;
    reg [7:0] prior_xfer_token;

    mp64_tacc #(
        .CALLER_BASE (5'd4),
        .CALLER_COUNT(4)
    ) uut (
        .clk                     (clk),
        .rst_n                   (rst_n),
        .engine_reset            (engine_reset),
        .req_valid               (req_valid),
        .req_ready               (req_ready),
        .req_is_tamac            (req_is_tamac),
        .req_funct               (req_funct),
        .req_canonical           (req_canonical),
        .req_caller_id           (req_caller_id),
        .req_caller_slot         (req_caller_slot),
        .req_format_ew           (req_format_ew),
        .req_format_signed       (req_format_signed),
        .req_image_addr          (req_image_addr),
        .req_preflight_fault     (req_preflight_fault),
        .req_preflight_fault_addr(req_preflight_fault_addr),
        .req_cancel              (req_cancel),
        .req_retire              (1'b1),
        .req_done                (req_done),
        .req_busy                (req_busy),
        .req_fault               (req_fault),
        .req_fault_addr          (req_fault_addr),
        .tamac_start             (tamac_start),
        .tamac_done              (tamac_done),
        .tamac_fault             (tamac_fault),
        .tamac_fault_addr        (tamac_fault_addr),
        .tamac_result_image      (tamac_result_image),
        .xfer_req                (xfer_req),
        .xfer_store              (xfer_store),
        .xfer_base               (xfer_base),
        .xfer_format_ew          (xfer_format_ew),
        .xfer_token              (xfer_token),
        .xfer_store_image        (xfer_store_image),
        .xfer_cancel             (xfer_cancel),
        .xfer_finish             (xfer_finish),
        .xfer_done               (xfer_done),
        .xfer_response_token     (xfer_response_token),
        .xfer_fault              (xfer_fault),
        .xfer_fault_addr         (xfer_fault_addr),
        .xfer_load_image         (xfer_load_image),
        .force_valid             (force_valid),
        .force_ready             (force_ready),
        .force_priv              (force_priv),
        .force_wdata             (force_wdata),
        .force_caller_id         (force_caller_id),
        .force_done              (force_done),
        .force_fault             (force_fault),
        .status_raw              (status_raw),
        .bank_state              (bank_state)
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

    task set_request;
        input       is_tamac;
        input [2:0] funct;
        input       canonical;
        input [4:0] caller_id;
        input [1:0] caller_slot;
        input [2:0] format_ew;
        input       format_signed;
    begin
        req_is_tamac      = is_tamac;
        req_funct         = funct;
        req_canonical     = canonical;
        req_caller_id     = caller_id;
        req_caller_slot   = caller_slot;
        req_format_ew     = format_ew;
        req_format_signed = format_signed;
    end
    endtask

    // A successfully admitted lifecycle operation must expose BUSY after its
    // admission edge and complete at the next terminal edge.
    task lifecycle_success;
        input [2:0] funct;
        input [4:0] caller_id;
        input [1:0] caller_slot;
        input [2:0] format_ew;
        input       format_signed;
    begin
        set_request(1'b0, funct, 1'b1, caller_id, caller_slot,
                    format_ew, format_signed);
        check("successful lifecycle request is ready", req_ready);
        req_valid = 1'b1;
        tick;
        req_valid = 1'b0;
        check("admitted lifecycle request raises BUSY", req_busy);
        check("admitted lifecycle request does not complete early",
              !req_done && req_fault == MEX_FAULT_NONE);
        check("raw status BUSY follows admitted lifecycle request",
              status_raw[TACC_STATUS_BIT_BUSY]);
        tick;
        check("lifecycle request completes at terminal boundary",
              req_done && !req_busy && req_fault == MEX_FAULT_NONE);
        tick;
        check("lifecycle completion is a one-cycle pulse", !req_done);
    end
    endtask

    // Ordinary lifecycle validation fails on the acceptance edge. TAMAC and
    // image operations retain their locked second base cycle without ever
    // publishing BUSY or mutating state.
    task lifecycle_fault;
        input       is_tamac;
        input [2:0] funct;
        input       canonical;
        input [4:0] caller_id;
        input [1:0] caller_slot;
        input [2:0] format_ew;
        input       format_signed;
        input [2:0] expected_fault;
        reg         deferred_class;
    begin
        deferred_class =
            is_tamac ||
            (!is_tamac &&
             ((funct == ETSYS_TACC_LOAD) ||
              (funct == ETSYS_TACC_STORE)));
        metadata_snapshot = status_raw &
            ~(64'd1 << TACC_STATUS_BIT_BUSY);
        bank_snapshot = bank_state;
        set_request(is_tamac, funct, canonical, caller_id, caller_slot,
                    format_ew, format_signed);
        check("validation-fault request is ready", req_ready);
        req_valid = 1'b1;
        if (is_tamac) begin
            #1;
            check("validation-fault TAMAC never starts its datapath",
                  !tamac_start);
        end
        tick;
        req_valid = 1'b0;
        if (deferred_class) begin
            check("deferred validation fault waits one nonbusy interval",
                  !req_done && !req_busy &&
                  req_fault == MEX_FAULT_NONE);
            tick;
            check("deferred validation fault completes in second base cycle",
                  req_done && req_fault == expected_fault);
        end else begin
            check("validation fault completes in acceptance cycle",
                  req_done && req_fault == expected_fault);
        end
        check("validation fault never raises BUSY", !req_busy);
        check("validation fault preserves physical metadata",
              (status_raw & ~(64'd1 << TACC_STATUS_BIT_BUSY))
                  == metadata_snapshot);
        check("validation fault preserves persistent bank",
              bank_state == bank_snapshot);
        tick;
        check("validation-fault completion is a one-cycle pulse",
              !req_done && req_fault == MEX_FAULT_NONE);
    end
    endtask

    task start_lifecycle;
        input [2:0] funct;
        input [4:0] caller_id;
        input [1:0] caller_slot;
        input [2:0] format_ew;
        input       format_signed;
    begin
        set_request(1'b0, funct, 1'b1, caller_id, caller_slot,
                    format_ew, format_signed);
        check("cancel-target lifecycle request is ready", req_ready);
        req_valid = 1'b1;
        tick;
        req_valid = 1'b0;
        check("cancel-target lifecycle request became busy",
              req_busy && !req_done);
    end
    endtask

    task control_write;
        input       privilege;
        input [63:0] value;
        input [2:0] expected_fault;
    begin
        metadata_snapshot = status_raw;
        bank_snapshot = bank_state;
        force_priv  = privilege;
        force_wdata = value;
        check("control request is ready", force_ready);
        force_valid = 1'b1;
        tick;
        force_valid = 1'b0;
        check("control request is acknowledged once",
              force_done && force_fault == expected_fault);
        if (expected_fault != MEX_FAULT_NONE || !value[0]) begin
            check("non-actioning control write preserves metadata",
                  status_raw == metadata_snapshot);
            check("non-actioning control write preserves bank",
                  bank_state == bank_snapshot);
        end
        tick;
        check("control acknowledgement is a one-cycle pulse", !force_done);
    end
    endtask

    // Continuous fail-closed checks catch malformed output combinations even
    // if a directed check later in the sequence were accidentally weakened.
    always @(negedge clk) begin
        if (rst_n && !engine_reset) begin
            if (status_raw[63:21] !== 43'd0 ||
                status_raw[15:10] !== 6'd0 ||
                status_raw[TACC_STATUS_BIT_MINE] !== 1'b0)
                $fatal(1, "TACC raw status exposed a reserved or MINE bit");
            if (status_raw[TACC_STATUS_BIT_DIRTY] &&
                !status_raw[TACC_STATUS_BIT_VALID])
                $fatal(1, "TACC DIRTY asserted while invalid");
            if (status_raw[TACC_STATUS_BIT_CLAIMED] !=
                (status_raw[TACC_STATUS_OWNER_MSB:
                            TACC_STATUS_OWNER_LSB] != TACC_OWNER_NONE))
                $fatal(1, "TACC CLAIMED/OWNER status mismatch");
            if ((req_fault != MEX_FAULT_NONE) && !req_done)
                $fatal(1, "TACC request fault without completion");
            if ((force_fault != MEX_FAULT_NONE) && !force_done)
                $fatal(1, "TACC control fault without acknowledgement");
        end
    end

    initial begin
        pass_count = 0;
        fail_count = 0;
        prior_xfer_token = 8'd0;

        rst_n = 1'b0;
        engine_reset = 1'b0;
        req_valid = 1'b0;
        req_is_tamac = 1'b0;
        req_funct = ETSYS_TACC_TRY;
        req_canonical = 1'b1;
        req_caller_id = 5'd4;
        req_caller_slot = 2'd0;
        req_format_ew = TMODE_8;
        req_format_signed = 1'b0;
        req_image_addr = 64'h0000_0000_0000_0400;
        req_preflight_fault = MEX_FAULT_NONE;
        req_preflight_fault_addr = 64'd0;
        req_cancel = 1'b0;
        tamac_done = 1'b0;
        tamac_fault = MEX_FAULT_NONE;
        tamac_fault_addr = 64'd0;
        tamac_result_image = 2048'd0;
        xfer_done = 1'b0;
        xfer_response_token = 8'd0;
        xfer_fault = MEX_FAULT_NONE;
        xfer_fault_addr = 64'd0;
        xfer_load_image = 2048'd0;
        force_valid = 1'b0;
        force_priv = 1'b0;
        force_wdata = 64'd0;
        force_caller_id = 5'd4;
        repeat (3) tick;
        rst_n = 1'b1;
        tick;

        check("reset produces exact physical FREE status",
              status_raw == {43'd0, TACC_OWNER_NONE, 16'd0});
        check("reset zeroizes persistent bank", bank_state == 2048'd0);
        check("reset leaves lifecycle and control transports ready",
              req_ready && force_ready);

        // Caller identity is a BASE+slot pair, not either field alone.
        lifecycle_fault(1'b0, ETSYS_TACC_TRY, 1'b1,
                        5'd4, 2'd1, TMODE_8, 1'b0,
                        MEX_FAULT_ILLEGAL);
        lifecycle_fault(1'b0, ETSYS_TACC_TRY, 1'b1,
                        5'd8, 2'd0, TMODE_8, 1'b0,
                        MEX_FAULT_ILLEGAL);

        // FREE, same-owner, and foreign-owner TRY all retire normally.
        lifecycle_success(ETSYS_TACC_TRY, 5'd4, 2'd0,
                          TMODE_8, 1'b0);
        check("FREE TRY establishes ownership but not validity",
              status_raw[TACC_STATUS_BIT_CLAIMED] &&
              !status_raw[TACC_STATUS_BIT_VALID] &&
              status_raw[TACC_STATUS_OWNER_MSB:
                         TACC_STATUS_OWNER_LSB] == 5'd4);
        metadata_snapshot = status_raw;
        lifecycle_success(ETSYS_TACC_TRY, 5'd4, 2'd0,
                          TMODE_8, 1'b0);
        check("same-owner TRY is idempotent", status_raw == metadata_snapshot);
        lifecycle_success(ETSYS_TACC_TRY, 5'd5, 2'd1,
                          TMODE_8, 1'b0);
        check("foreign TRY retires without stealing ownership",
              status_raw == metadata_snapshot);

        // Protected operations fault before BUSY or mutation.
        lifecycle_fault(1'b0, ETSYS_TACC_CLEAR, 1'b1,
                        5'd5, 2'd1, TMODE_8, 1'b0,
                        MEX_FAULT_ILLEGAL);
        lifecycle_fault(1'b0, ETSYS_TACC_CLEAR, 1'b1,
                        5'd4, 2'd0, TMODE_64, 1'b0,
                        MEX_FAULT_ILLEGAL);
        lifecycle_fault(1'b0, ETSYS_TACC_RESERVED, 1'b1,
                        5'd4, 2'd0, TMODE_8, 1'b0,
                        MEX_FAULT_ILLEGAL);
        lifecycle_fault(1'b0, ETSYS_TACC_CLEAR, 1'b0,
                        5'd4, 2'd0, TMODE_8, 1'b0,
                        MEX_FAULT_ILLEGAL);

        lifecycle_success(ETSYS_TACC_CLEAR, 5'd4, 2'd0,
                          TMODE_8, 1'b1);
        check("integer CLEAR establishes valid dirty signed format",
              status_raw[TACC_STATUS_BIT_VALID] &&
              status_raw[TACC_STATUS_BIT_DIRTY] &&
              status_raw[TACC_STATUS_FORMAT_EW_MSB:
                         TACC_STATUS_FORMAT_EW_LSB] == TMODE_8 &&
              status_raw[TACC_STATUS_BIT_FORMAT_SIGNED]);
        check("CLEAR zeroizes the whole persistent bank",
              bank_state == 2048'd0);

        // LOAD retains the old architectural bank until the shared stage's
        // terminal response reaches the explicit retirement edge.
        bank_snapshot = bank_state;
        xfer_load_image = {256{8'h5A}};
        set_request(1'b0, ETSYS_TACC_LOAD, 1'b1,
                    5'd4, 2'd0, TMODE_16, 1'b1);
        req_valid = 1'b1;
        tick;
        req_valid = 1'b0;
        check("LOAD admission exposes one held stage request",
              xfer_req && !xfer_store &&
              xfer_base == req_image_addr &&
              xfer_format_ew == TMODE_16);
        prior_xfer_token = xfer_token;
        check("LOAD admission leaves old bank private",
              bank_state == bank_snapshot);
        xfer_response_token = xfer_token;
        xfer_done = 1'b1;
        tick;
        xfer_done = 1'b0;
        check("LOAD stage completion publishes terminal response",
              req_done && req_fault == MEX_FAULT_NONE && xfer_finish);
        check("LOAD response is atomic before retirement",
              bank_state == bank_snapshot);
        tick;
        check("LOAD retirement publishes the complete image",
              bank_state == {256{8'h5A}});
        check("LOAD retirement latches clean format metadata",
              status_raw[TACC_STATUS_BIT_VALID] &&
              !status_raw[TACC_STATUS_BIT_DIRTY] &&
              status_raw[TACC_STATUS_FORMAT_EW_MSB:
                         TACC_STATUS_FORMAT_EW_LSB] == TMODE_16 &&
              status_raw[TACC_STATUS_BIT_FORMAT_SIGNED]);

        // STORE snapshots the persistent bank but changes only DIRTY, and
        // only on a successful terminal retirement.
        uut.dirty_reg = 1'b1;
        #1;
        set_request(1'b0, ETSYS_TACC_STORE, 1'b1,
                    5'd4, 2'd0, TMODE_64, 1'b0);
        req_valid = 1'b1;
        tick;
        req_valid = 1'b0;
        check("STORE uses the latched format rather than current TMODE",
              xfer_req && xfer_store &&
              xfer_format_ew == TMODE_16 &&
              xfer_store_image == {256{8'h5A}});
        check("back-to-back transfers use distinct operation tokens",
              xfer_token != prior_xfer_token);
        prior_xfer_token = xfer_token;
        xfer_response_token = xfer_token;
        xfer_fault = MEX_FAULT_BUS;
        xfer_fault_addr = 64'h0000_0000_0000_0488;
        xfer_done = 1'b1;
        tick;
        xfer_done = 1'b0;
        check("STORE bus fault reports exact target address",
              req_done && req_fault == MEX_FAULT_BUS &&
              req_fault_addr == 64'h0000_0000_0000_0488);
        tick;
        check("failed STORE preserves bank and preinstruction DIRTY",
              bank_state == {256{8'h5A}} &&
              status_raw[TACC_STATUS_BIT_DIRTY]);

        set_request(1'b0, ETSYS_TACC_STORE, 1'b1,
                    5'd4, 2'd0, TMODE_8, 1'b0);
        req_valid = 1'b1;
        tick;
        req_valid = 1'b0;
        check("faulted transfer token cannot alias fresh STORE",
              xfer_token != prior_xfer_token);
        xfer_response_token = xfer_token;
        xfer_fault = MEX_FAULT_NONE;
        xfer_fault_addr = 64'd0;
        xfer_done = 1'b1;
        tick;
        xfer_done = 1'b0;
        check("successful STORE reaches terminal response", req_done);
        tick;
        check("successful STORE clears DIRTY without changing bank",
              !status_raw[TACC_STATUS_BIT_DIRTY] &&
              bank_state == {256{8'h5A}});

        // TAMAC admission and completion are separate boundaries.  The parent
        // datapath may finish early, but its complete image remains private
        // until this leaf samples architectural retirement.
        set_request(1'b1, TMUL_TAMAC, 1'b1,
                    5'd4, 2'd0, TMODE_16, 1'b1);
        tamac_result_image = {256{8'h3C}};
        req_valid = 1'b1;
        #1;
        check("matching integer TAMAC raises combinational start",
              tamac_start);
        tick;
        req_valid = 1'b0;
        check("admitted TAMAC is busy without early completion",
              req_busy && !req_done);
        bank_snapshot = bank_state;
        tamac_done = 1'b1;
        tick;
        tamac_done = 1'b0;
        check("TAMAC terminal response precedes architectural commit",
              req_done && !req_busy &&
              req_fault == MEX_FAULT_NONE &&
              bank_state == bank_snapshot);
        tick;
        check("retired TAMAC publishes complete dirty image",
              bank_state == {256{8'h3C}} &&
              status_raw[TACC_STATUS_BIT_DIRTY]);

        bank_snapshot = bank_state;
        tamac_result_image = {256{8'hC3}};
        req_valid = 1'b1;
        tick;
        req_valid = 1'b0;
        req_cancel = 1'b1;
        tick;
        req_cancel = 1'b0;
        check("active TAMAC cancellation suppresses response and commit",
              !req_done && !req_busy &&
              bank_state == bank_snapshot);

        tamac_result_image = {256{8'h69}};
        req_valid = 1'b1;
        tick;
        req_valid = 1'b0;
        tamac_fault = MEX_FAULT_BUS;
        tamac_fault_addr = 64'h0000_0000_0010_0040;
        tamac_done = 1'b1;
        tick;
        tamac_done = 1'b0;
        tamac_fault = MEX_FAULT_NONE;
        tamac_fault_addr = 64'd0;
        check("TAMAC source fault reaches terminal with exact address",
              req_done && req_fault == MEX_FAULT_BUS &&
              req_fault_addr == 64'h0000_0000_0010_0040);
        tick;
        check("faulted TAMAC preserves the complete prior image",
              bank_state == bank_snapshot);

        req_preflight_fault = MEX_FAULT_BUS;
        req_preflight_fault_addr = 64'h0000_0000_0010_0080;
        req_valid = 1'b1;
        #1;
        check("preflight-fault TAMAC never starts its datapath",
              !tamac_start);
        tick;
        req_valid = 1'b0;
        check("preflight-fault TAMAC defers without BUSY",
              !req_done && !req_busy &&
              req_fault == MEX_FAULT_NONE);
        tick;
        check("preflight-fault TAMAC completes in second base cycle",
              req_done && !req_busy &&
              req_fault == MEX_FAULT_BUS &&
              req_fault_addr == 64'h0000_0000_0010_0080);
        req_preflight_fault = MEX_FAULT_NONE;
        req_preflight_fault_addr = 64'd0;
        tick;

        lifecycle_fault(1'b1, TMUL_TAMAC, 1'b1,
                        5'd4, 2'd0, TMODE_8, 1'b1,
                        MEX_FAULT_ILLEGAL);
        lifecycle_fault(1'b1, TMUL_TAMAC, 1'b1,
                        5'd4, 2'd0, TMODE_FP16, 1'b0,
                        MEX_FAULT_ILLEGAL);
        lifecycle_fault(1'b1, TMUL_TAMAC, 1'b1,
                        5'd4, 2'd0, TMODE_BF16, 1'b1,
                        MEX_FAULT_ILLEGAL);

        // Test-only deposit keeps every bank bit observable for destructive
        // lifecycle checks below.
        uut.bank_reg = {256{8'hA5}};
        #1;
        check("focused bench seeds every persistent bank byte",
              bank_state == {256{8'hA5}});
        lifecycle_success(ETSYS_TACC_CLEAR, 5'd4, 2'd0,
                          TMODE_FP16, 1'b1);
        check("floating CLEAR ignores and clears signed format bit",
              status_raw[TACC_STATUS_FORMAT_EW_MSB:
                         TACC_STATUS_FORMAT_EW_LSB] == TMODE_FP16 &&
              !status_raw[TACC_STATUS_BIT_FORMAT_SIGNED]);
        check("floating CLEAR zeroizes the seeded whole bank",
              bank_state == 2048'd0);
        uut.dirty_reg = 1'b0;
        #1;
        check("FP16 TAMAC fixture begins from clean valid state",
              !status_raw[TACC_STATUS_BIT_DIRTY]);

        // Floating TAMAC ignores TMODE.SIGNED.  Keep the inactive upper half
        // explicit in these supplied leaf images: the parent datapath
        // canonicalizes it, while this leaf commits the complete image only
        // at architectural retirement.
        set_request(1'b1, TMUL_TAMAC, 1'b1,
                    5'd4, 2'd0, TMODE_FP16, 1'b1);
        tamac_result_image =
            {1024'd0, {16{64'h7FC0_0000_3F80_0000}}};
        req_valid = 1'b1;
        #1;
        check("matching FP16 TAMAC starts with SIGNED set",
              tamac_start);
        tick;
        req_valid = 1'b0;
        check("admitted FP16 TAMAC is busy before completion",
              req_busy && !req_done);
        bank_snapshot = bank_state;
        tamac_done = 1'b1;
        tick;
        tamac_done = 1'b0;
        check("FP16 TAMAC response precedes atomic image commit",
              req_done && !req_busy &&
              req_fault == MEX_FAULT_NONE &&
              bank_state == bank_snapshot &&
              !status_raw[TACC_STATUS_BIT_DIRTY]);
        tick;
        check("retired FP16 TAMAC commits exact canonical dirty image",
              bank_state ==
                  {1024'd0, {16{64'h7FC0_0000_3F80_0000}}} &&
              status_raw[TACC_STATUS_BIT_DIRTY] &&
              status_raw[TACC_STATUS_FORMAT_EW_MSB:
                         TACC_STATUS_FORMAT_EW_LSB] == TMODE_FP16 &&
              !status_raw[TACC_STATUS_BIT_FORMAT_SIGNED]);

        // The opposite SIGNED value is admitted too.  Poison every supplied
        // result byte so any partial or premature publication is observable.
        bank_snapshot = bank_state;
        set_request(1'b1, TMUL_TAMAC, 1'b1,
                    5'd4, 2'd0, TMODE_FP16, 1'b0);
        tamac_result_image = {128{16'hDEAD}};
        req_valid = 1'b1;
        #1;
        check("matching FP16 TAMAC starts with SIGNED clear",
              tamac_start);
        tick;
        req_valid = 1'b0;
        check("cancel-target FP16 TAMAC became busy",
              req_busy && !req_done);
        req_cancel = 1'b1;
        tick;
        req_cancel = 1'b0;
        check("canceled FP16 TAMAC preserves complete canonical bank",
              !req_done && !req_busy &&
              req_fault == MEX_FAULT_NONE &&
              bank_state == bank_snapshot &&
              bank_state[2047:1024] == 1024'd0);

        // Neither an integer request against FP state nor another FP encoding
        // may enter the datapath before a deliberate CLEAR or LOAD transition.
        lifecycle_fault(1'b1, TMUL_TAMAC, 1'b1,
                        5'd4, 2'd0, TMODE_16, 1'b1,
                        MEX_FAULT_ILLEGAL);
        lifecycle_fault(1'b1, TMUL_TAMAC, 1'b1,
                        5'd4, 2'd0, TMODE_BF16, 1'b0,
                        MEX_FAULT_ILLEGAL);

        lifecycle_success(ETSYS_TACC_CLEAR, 5'd4, 2'd0,
                          TMODE_BF16, 1'b1);
        check("BF16 CLEAR ignores and clears signed format bit",
              status_raw[TACC_STATUS_FORMAT_EW_MSB:
                         TACC_STATUS_FORMAT_EW_LSB] == TMODE_BF16 &&
              !status_raw[TACC_STATUS_BIT_FORMAT_SIGNED]);
        check("BF16 CLEAR zeroizes the whole persistent bank",
              bank_state == 2048'd0);
        uut.dirty_reg = 1'b0;
        #1;
        check("BF16 TAMAC fixture begins from clean valid state",
              !status_raw[TACC_STATUS_BIT_DIRTY]);

        set_request(1'b1, TMUL_TAMAC, 1'b1,
                    5'd4, 2'd0, TMODE_BF16, 1'b0);
        tamac_result_image =
            {1024'd0, {16{64'h8000_0000_0000_0001}}};
        req_valid = 1'b1;
        #1;
        check("matching BF16 TAMAC starts with SIGNED clear",
              tamac_start);
        tick;
        req_valid = 1'b0;
        check("admitted BF16 TAMAC is busy before completion",
              req_busy && !req_done);
        bank_snapshot = bank_state;
        tamac_done = 1'b1;
        tick;
        tamac_done = 1'b0;
        check("BF16 TAMAC response precedes atomic image commit",
              req_done && !req_busy &&
              req_fault == MEX_FAULT_NONE &&
              bank_state == bank_snapshot &&
              !status_raw[TACC_STATUS_BIT_DIRTY]);
        tick;
        check("retired BF16 TAMAC commits exact canonical dirty image",
              bank_state ==
                  {1024'd0, {16{64'h8000_0000_0000_0001}}} &&
              status_raw[TACC_STATUS_BIT_DIRTY] &&
              status_raw[TACC_STATUS_FORMAT_EW_MSB:
                         TACC_STATUS_FORMAT_EW_LSB] == TMODE_BF16 &&
              !status_raw[TACC_STATUS_BIT_FORMAT_SIGNED]);

        // SIGNED set remains a matching BF16 request.  A terminal source
        // fault must discard even a fully supplied noncanonical poison image.
        bank_snapshot = bank_state;
        set_request(1'b1, TMUL_TAMAC, 1'b1,
                    5'd4, 2'd0, TMODE_BF16, 1'b1);
        tamac_result_image = {128{16'hBEEF}};
        req_valid = 1'b1;
        #1;
        check("matching BF16 TAMAC starts with SIGNED set",
              tamac_start);
        tick;
        req_valid = 1'b0;
        check("fault-target BF16 TAMAC became busy",
              req_busy && !req_done);
        tamac_fault = MEX_FAULT_BUS;
        tamac_fault_addr = 64'h0000_0000_0010_00C0;
        tamac_done = 1'b1;
        tick;
        tamac_done = 1'b0;
        tamac_fault = MEX_FAULT_NONE;
        tamac_fault_addr = 64'd0;
        check("BF16 TAMAC source fault reports exact address before commit",
              req_done && !req_busy &&
              req_fault == MEX_FAULT_BUS &&
              req_fault_addr == 64'h0000_0000_0010_00C0 &&
              bank_state == bank_snapshot);
        tick;
        check("faulted BF16 TAMAC preserves complete canonical bank",
              bank_state == bank_snapshot &&
              bank_state[2047:1024] == 1024'd0 &&
              status_raw[TACC_STATUS_BIT_DIRTY]);

        lifecycle_fault(1'b1, TMUL_TAMAC, 1'b1,
                        5'd4, 2'd0, TMODE_32, 1'b0,
                        MEX_FAULT_ILLEGAL);
        lifecycle_fault(1'b1, TMUL_TAMAC, 1'b1,
                        5'd4, 2'd0, TMODE_FP16, 1'b1,
                        MEX_FAULT_ILLEGAL);

        // Caller cancellation is terminal but non-retiring and preserves the
        // complete shared physical state when no FORCE is pending.
        metadata_snapshot = status_raw &
            ~(64'd1 << TACC_STATUS_BIT_BUSY);
        bank_snapshot = bank_state;
        start_lifecycle(ETSYS_TACC_CLEAR, 5'd4, 2'd0,
                        TMODE_16, 1'b1);
        req_cancel = 1'b1;
        tick;
        req_cancel = 1'b0;
        check("caller cancel suppresses completion and fault",
              !req_done && req_fault == MEX_FAULT_NONE && !req_busy);
        check("caller cancel preserves lifecycle metadata",
              status_raw == metadata_snapshot);
        check("caller cancel preserves persistent bank",
              bank_state == bank_snapshot);

        lifecycle_fault(1'b0, ETSYS_TACC_RELEASE, 1'b1,
                        5'd5, 2'd1, TMODE_8, 1'b0,
                        MEX_FAULT_ILLEGAL);
        metadata_snapshot = status_raw;
        start_lifecycle(ETSYS_TACC_RELEASE, 5'd4, 2'd0,
                        TMODE_8, 1'b0);
        req_cancel = 1'b1;
        tick;
        req_cancel = 1'b0;
        check("canceled RELEASE does not complete", !req_done && !req_busy);
        check("canceled RELEASE preserves ownership",
              status_raw == metadata_snapshot);

        lifecycle_success(ETSYS_TACC_RELEASE, 5'd4, 2'd0,
                          TMODE_8, 1'b0);
        check("owner RELEASE returns exact FREE status",
              status_raw == {43'd0, TACC_OWNER_NONE, 16'd0});

        // Control privilege is deliberately narrow: only bit zero in user
        // mode faults, while reserved bits are ignored.
        lifecycle_success(ETSYS_TACC_TRY, 5'd4, 2'd0,
                          TMODE_8, 1'b0);
        lifecycle_success(ETSYS_TACC_CLEAR, 5'd4, 2'd0,
                          TMODE_32, 1'b1);
        control_write(1'b1, 64'd1, MEX_FAULT_PRIV);
        control_write(1'b1, 64'd2, MEX_FAULT_NONE);
        force_caller_id = 5'd9;
        control_write(1'b0, 64'd1, MEX_FAULT_ILLEGAL);
        force_caller_id = 5'bxxxxx;
        control_write(1'b0, 64'd1, MEX_FAULT_ILLEGAL);
        force_caller_id = 5'd4;

        uut.bank_reg = {256{8'h3C}};
        #1;
        control_write(1'b0, 64'd1, MEX_FAULT_NONE);
        check("idle supervisor FORCE wipes ownership and metadata",
              status_raw == {43'd0, TACC_OWNER_NONE, 16'd0} &&
              bank_state == 2048'd0);

        // Same-cycle accepted supervisor FORCE must make req_ready low and
        // defeat idle admission.  The one-cycle request pulse is intentionally
        // dropped here; tile integration is responsible for retaining it.
        set_request(1'b0, ETSYS_TACC_TRY, 1'b1,
                    5'd4, 2'd0, TMODE_8, 1'b0);
        req_valid = 1'b1;
        force_priv = 1'b0;
        force_wdata = 64'd1;
        force_valid = 1'b1;
        #1;
        check("same-cycle supervisor FORCE deasserts req_ready", !req_ready);
        tick;
        req_valid = 1'b0;
        force_valid = 1'b0;
        check("same-cycle FORCE is acknowledged", force_done);
        check("same-cycle FORCE prevents lifecycle admission",
              !req_busy && !req_done &&
              status_raw == {43'd0, TACC_OWNER_NONE, 16'd0});
        tick;

        // Active FORCE publishes the active instruction's completion first,
        // then leaves the physical state wiped on the response-sampling edge.
        lifecycle_success(ETSYS_TACC_TRY, 5'd4, 2'd0,
                          TMODE_8, 1'b0);
        start_lifecycle(ETSYS_TACC_CLEAR, 5'd4, 2'd0,
                        TMODE_16, 1'b1);
        force_priv = 1'b0;
        force_wdata = 64'd1;
        force_valid = 1'b1;
        tick;
        force_valid = 1'b0;
        check("active FORCE control write is acknowledged", force_done);
        check("active lifecycle operation still publishes completion",
              req_done && req_fault == MEX_FAULT_NONE);
        tick;
        check("active FORCE wipes at response-sampling boundary",
              status_raw == {43'd0, TACC_OWNER_NONE, 16'd0} &&
              bank_state == 2048'd0);

        // Engine reset has broader scope than caller cancellation: it wipes
        // immediately and suppresses any terminal response.
        lifecycle_success(ETSYS_TACC_TRY, 5'd4, 2'd0,
                          TMODE_8, 1'b0);
        lifecycle_success(ETSYS_TACC_CLEAR, 5'd4, 2'd0,
                          TMODE_16, 1'b1);
        set_request(1'b0, ETSYS_TACC_LOAD, 1'b1,
                    5'd4, 2'd0, TMODE_8, 1'b0);
        req_valid = 1'b1;
        tick;
        req_valid = 1'b0;
        check("engine-reset target owns the transfer stage request",
              xfer_req && req_busy);
        engine_reset = 1'b1;
        #1;
        check("engine reset cancels an owned transfer before leaf wipe",
              xfer_cancel);
        tick;
        check("engine reset suppresses active completion",
              !req_done && !req_busy && req_fault == MEX_FAULT_NONE);
        check("engine reset immediately restores exact FREE state",
              status_raw == {43'd0, TACC_OWNER_NONE, 16'd0} &&
              bank_state == 2048'd0);
        engine_reset = 1'b0;
        tick;
        check("engine returns ready after reset", req_ready && force_ready);

        if (fail_count == 0) begin
            $display("ALL %0d TACC LIFECYCLE TESTS PASSED", pass_count);
            $finish;
        end else begin
            $fatal(1, "%0d TACC lifecycle checks failed", fail_count);
        end
    end

endmodule
