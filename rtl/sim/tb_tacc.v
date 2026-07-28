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
    reg         req_cancel;
    wire        req_done;
    wire        req_busy;
    wire [2:0]  req_fault;

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
        .req_cancel              (req_cancel),
        .req_retire              (1'b1),
        .req_done                (req_done),
        .req_busy                (req_busy),
        .req_fault               (req_fault),
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

    // Validation failures are one-base-cycle responses: completion and fault
    // appear on the acceptance edge, with no BUSY interval or mutation.
    task lifecycle_fault;
        input       is_tamac;
        input [2:0] funct;
        input       canonical;
        input [4:0] caller_id;
        input [1:0] caller_slot;
        input [2:0] format_ew;
        input       format_signed;
        input [2:0] expected_fault;
    begin
        metadata_snapshot = status_raw &
            ~(64'd1 << TACC_STATUS_BIT_BUSY);
        bank_snapshot = bank_state;
        set_request(is_tamac, funct, canonical, caller_id, caller_slot,
                    format_ew, format_signed);
        check("validation-fault request is ready", req_ready);
        req_valid = 1'b1;
        tick;
        req_valid = 1'b0;
        check("validation fault completes in acceptance cycle",
              req_done && req_fault == expected_fault);
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
        req_cancel = 1'b0;
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

        // Test-only deposit makes every bank bit observable without
        // prematurely adding a production LOAD/TAMAC write interface.
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
        start_lifecycle(ETSYS_TACC_CLEAR, 5'd4, 2'd0,
                        TMODE_8, 1'b0);
        engine_reset = 1'b1;
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
