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
    reg [63:0]  mex_gpr_val;
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
    wire [31:0]  tile_addr;
    wire         tile_wen;
    wire [511:0] tile_rdata;
    wire         tile_ack;
    wire         tile_error;
    wire [63:0]  tile_fault_addr;
    wire         ext_tile_req;
    wire [63:0]  ext_tile_addr;
    wire         ext_tile_wen;
    wire [511:0] ext_tile_rdata;
    wire         ext_tile_ack;
    wire         ext_tile_error;
    wire [63:0]  ext_tile_fault_addr;
    wire         tile_source_cancel;

    reg [511:0] tamac_mem_a;
    reg [511:0] tamac_mem_b;
    reg         tamac_mem_respond;
    reg         tamac_mem_error;
    reg [31:0]  tamac_mem_error_addr;
    reg [63:0]  tamac_mem_fault_addr;
    integer     tamac_mem_req_count;

    localparam [31:0] TAMAC_ADDR_A = 32'h0000_0100;
    localparam [31:0] TAMAC_ADDR_B = 32'h0000_0200;
    localparam [63:0] TAMAC_EXT_ADDR_A =
        64'h0000_0000_0010_0100;
    localparam [63:0] TAMAC_EXT_ADDR_B =
        64'h0000_0000_0010_0200;

    assign tile_rdata =
        (tile_addr == TAMAC_ADDR_A) ? tamac_mem_a :
        (tile_addr == TAMAC_ADDR_B) ? tamac_mem_b :
        512'd0;
    assign tile_ack = tile_req && tamac_mem_respond;
    assign tile_error =
        tile_ack && tamac_mem_error &&
        (tile_addr == tamac_mem_error_addr);
    assign tile_fault_addr =
        tile_error ? tamac_mem_fault_addr : 64'd0;
    assign ext_tile_rdata =
        (ext_tile_addr == TAMAC_EXT_ADDR_A) ?
        tamac_mem_a :
        (ext_tile_addr == TAMAC_EXT_ADDR_B) ?
        tamac_mem_b : 512'd0;
    assign ext_tile_ack = ext_tile_req && tamac_mem_respond;
    assign ext_tile_error = ext_tile_ack && tamac_mem_error;
    assign ext_tile_fault_addr =
        ext_tile_error ? tamac_mem_fault_addr : 64'd0;

    always @(posedge clk) begin
        if (!rst_n)
            tamac_mem_req_count <= 0;
        else if (tile_req || ext_tile_req)
            tamac_mem_req_count <= tamac_mem_req_count + 1;
    end

    integer pass_count;
    integer fail_count;
    integer vector_fd;
    integer vector_scan;
    integer vector_case_count;
    integer vector_repeat_index;
    integer vector_observed_cycles;
    integer vector_total_observed_cycles;
    integer vector_ew;
    integer vector_signed;
    integer vector_ss;
    integer vector_repeats;
    integer vector_cycles;
    integer vector_total_cycles;
    reg [511:0] vector_name;
    reg [16383:0] vector_line;
    reg [63:0] vector_scalar;
    reg [511:0] vector_source_a;
    reg [511:0] vector_source_b;
    reg [2047:0] vector_initial_tacc;
    reg [2047:0] vector_final_tacc;
    reg [2047:0] cycle_bank_snapshot;

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
        .mex_gpr_val            (mex_gpr_val),
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
        .tile_addr              (tile_addr),
        .tile_wen               (tile_wen),
        .tile_wdata             (),
        .tile_rdata             (tile_rdata),
        .tile_ack               (tile_ack),
        .tile_error             (tile_error),
        .tile_fault_addr        (tile_fault_addr),
        .ext_tile_req           (ext_tile_req),
        .ext_tile_addr          (ext_tile_addr),
        .ext_tile_wen           (ext_tile_wen),
        .ext_tile_wdata         (),
        .ext_tile_rdata         (ext_tile_rdata),
        .ext_tile_ack           (ext_tile_ack),
        .ext_tile_error         (ext_tile_error),
        .ext_tile_fault_addr    (ext_tile_fault_addr),
        .tile_source_cancel     (tile_source_cancel)
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
        reg         deferred_class;
    begin
        deferred_class =
            (funct == ETSYS_TACC_LOAD) ||
            (funct == ETSYS_TACC_STORE);
        set_lifecycle(funct, funct_byte);
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        if (deferred_class) begin
            check("image validation waits one transport interval",
                  !mex_done && mex_busy &&
                  mex_fault == MEX_FAULT_NONE &&
                  !tacc_status_raw[TACC_STATUS_BIT_BUSY]);
            tick;
            check("image validation fault completes in second base cycle",
                  mex_done && !mex_busy &&
                  mex_fault == MEX_FAULT_ILLEGAL);
        end else begin
            check("validation fault completes in first base cycle",
                  mex_done && !mex_busy &&
                  mex_fault == MEX_FAULT_ILLEGAL);
        end
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
        check("address preflight waits one transport interval",
              !mex_done && mex_busy &&
              mex_fault == MEX_FAULT_NONE &&
              !tacc_status_raw[TACC_STATUS_BIT_BUSY]);
        check("address preflight emits no stage or memory request",
              !tacc_xfer_req && !tile_req && !ext_tile_req);
        tick;
        check("address preflight completes in second base cycle",
              mex_done && !mex_busy &&
              mex_fault == expected_fault);
        check("address preflight reports the first forbidden byte",
              mex_fault_addr == expected_addr);
        check("address preflight emits no stage or memory request",
              !tacc_xfer_req && !tile_req && !ext_tile_req);
        tick;
    end
    endtask

    task load_tacc_image;
        input [2:0]    format_ew;
        input          format_signed;
        input [2047:0] image;
    begin
        write_csr(
            CSR_TMODE,
            {59'd0, format_signed, 1'b0, format_ew});
        write_csr(CSR_TSRC0, 64'd0);
        tacc_xfer_load_image = image;
        set_lifecycle(ETSYS_TACC_LOAD,
                      {5'd0, ETSYS_TACC_LOAD});
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("vector LOAD owns image stage",
              tacc_xfer_req && !tacc_xfer_store);
        tacc_xfer_response_token = tacc_xfer_token;
        tacc_xfer_done = 1'b1;
        tick;
        tacc_xfer_done = 1'b0;
        check("vector LOAD reaches terminal response",
              mex_done && mex_fault == MEX_FAULT_NONE);
        tick;
        check("vector LOAD publishes exact initial image",
              uut.tacc_bank_state == image);
    end
    endtask

    task dispatch_tamac_once;
        input [1:0] source_form;
        input integer expected_cycles;
        input integer expected_source_reads;
        output integer observed_cycles;
        reg [2047:0] bank_before;
        reg [255:0] legacy_acc_before;
        integer observed_stalls;
    begin
        mex_ss          = source_form;
        mex_op          = MEX_TMUL;
        mex_funct       = TMUL_TAMAC;
        mex_funct_byte  = {5'd0, TMUL_TAMAC};
        mex_ext_mod     = 4'd0;
        mex_ext_active  = 1'b0;
        mex_caller_id   = 5'd4;
        mex_caller_slot = 2'd0;
        bank_before = uut.tacc_bank_state;
        legacy_acc_before = legacy_acc_state;
        tamac_mem_req_count = 0;
        observed_cycles = 0;
        observed_stalls = 0;

        mex_valid = 1'b1;
        tick;
        observed_cycles = observed_cycles + 1;
        observed_stalls = observed_stalls + mex_stall_cycle;
        mex_valid = 1'b0;
        check("TAMAC admission keeps old bank private",
              uut.tacc_bank_state == bank_before);

        while (!mex_done && observed_cycles < 16) begin
            tick;
            observed_cycles = observed_cycles + 1;
            observed_stalls = observed_stalls + mex_stall_cycle;
            if (!mex_done)
                check("TAMAC partial beat is not architecturally visible",
                      uut.tacc_bank_state == bank_before);
        end

        check("TAMAC terminates without fault",
              mex_done && mex_fault == MEX_FAULT_NONE);
        check("TAMAC exact engine-local cycle count",
              observed_cycles == expected_cycles);
        check("TAMAC serialized source-beat count",
              tamac_mem_req_count == expected_source_reads);
        check("uncontended TAMAC records no transport stall",
              observed_stalls == 0);
        check("TAMAC terminal still preserves pre-retirement bank",
              uut.tacc_bank_state == bank_before);
        check("TAMAC leaves legacy ACC byte-exact",
              legacy_acc_state == legacy_acc_before);
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
        mex_gpr_val = 64'd0;
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
        tamac_mem_a = 512'd0;
        tamac_mem_b = 512'd0;
        tamac_mem_respond = 1'b1;
        tamac_mem_error = 1'b0;
        tamac_mem_error_addr = 32'd0;
        tamac_mem_fault_addr = 64'd0;
        tamac_mem_req_count = 0;

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

        // Noncanonical lifecycle fails in one cycle. STORE-without-valid-state
        // retains the locked two-cycle image-operation validation latency.
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

        // Execute the checked-in Phase-1 emulator oracle vectors.  The file
        // carries complete 2,048-bit initial/final images, not a duplicate RTL
        // arithmetic oracle.
        vector_case_count = 0;
        vector_fd = $fopen("tamac_integer_vectors.vec", "r");
        if (vector_fd == 0)
            $fatal(1, "cannot open tamac_integer_vectors.vec");
        while (!$feof(vector_fd)) begin
            vector_line = {16384{1'b0}};
            vector_scan = $fgets(vector_line, vector_fd);
            vector_scan = $sscanf(
                vector_line,
                "%s %d %d %d %d %d %d %h %h %h %h %h",
                vector_name,
                vector_ew,
                vector_signed,
                vector_ss,
                vector_repeats,
                vector_cycles,
                vector_total_cycles,
                vector_scalar,
                vector_source_a,
                vector_source_b,
                vector_initial_tacc,
                vector_final_tacc);
            if (vector_scan == 12) begin
                vector_case_count = vector_case_count + 1;
                $display("  TAMAC vector: %0s", vector_name);
                load_tacc_image(
                    vector_ew[2:0],
                    vector_signed[0],
                    vector_initial_tacc);
                tamac_mem_a = vector_source_a;
                tamac_mem_b = vector_source_b;
                mex_gpr_val = vector_scalar;

                case (vector_ss)
                    0: begin
                        write_csr(CSR_TSRC0,
                                  {32'd0, TAMAC_ADDR_A});
                        write_csr(CSR_TSRC1,
                                  {32'd0, TAMAC_ADDR_B});
                    end
                    1: begin
                        write_csr(CSR_TSRC0,
                                  {32'd0, TAMAC_ADDR_A});
                    end
                    3: begin
                        write_csr(CSR_TDST,
                                  {32'd0, TAMAC_ADDR_A});
                        write_csr(CSR_TSRC0,
                                  {32'd0, TAMAC_ADDR_B});
                    end
                    default:
                        $fatal(1, "fixture has illegal TAMAC SS");
                endcase

                vector_total_observed_cycles = 0;
                for (vector_repeat_index = 0;
                     vector_repeat_index < vector_repeats;
                     vector_repeat_index = vector_repeat_index + 1) begin
                    dispatch_tamac_once(
                        vector_ss[1:0],
                        vector_cycles,
                        (vector_ss == 1) ? 1 : 2,
                        vector_observed_cycles);
                    vector_total_observed_cycles =
                        vector_total_observed_cycles +
                        vector_observed_cycles;
                end
                check("fixture repeat-total cycle count",
                      vector_total_observed_cycles ==
                      vector_total_cycles);
                check("fixture final TACC image matches emulator",
                      uut.tacc_bank_state == vector_final_tacc);
                check("successful fixture leaves TACC dirty",
                      tacc_status_raw[TACC_STATUS_BIT_DIRTY]);
            end
        end
        $fclose(vector_fd);
        check("all six integer TAMAC fixtures executed",
              vector_case_count == 6);

        // TAMAC source reads use the ordinary internal/external 512-bit lane,
        // never the four-beat canonical-image stage.
        load_tacc_image(TMODE_32, 1'b0, 2048'd0);
        tamac_mem_a = {16{32'd7}};
        mex_gpr_val = 64'hA5A5_5A5A_0000_0005;
        write_csr(CSR_TSRC0, TAMAC_EXT_ADDR_A);
        dispatch_tamac_once(2'd1, 3, 1,
                            vector_observed_cycles);
        check("external-source broadcast computes exact result",
              uut.tacc_bank_state[63:0] ==
              64'h0000_0000_0000_0023);
        check("external-source TAMAC keeps U32 inactive bytes zero",
              uut.tacc_bank_state[2047:1024] == 1024'd0);
        check("external TAMAC bypasses canonical image stage",
              !tacc_xfer_req);

        // A bad second source must be discovered before source A is read.
        // This is the hardware form of the emulator's all-span preflight
        // callback test.
        write_csr(CSR_TSRC0, {32'd0, TAMAC_ADDR_A});
        write_csr(CSR_TSRC1, 64'h0000_0000_000F_FFE0);
        mex_ss = 2'd0;
        mex_op = MEX_TMUL;
        mex_funct = TMUL_TAMAC;
        mex_funct_byte = {5'd0, TMUL_TAMAC};
        mex_ext_mod = 4'd0;
        mex_ext_active = 1'b0;
        tamac_mem_req_count = 0;
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("second-source span fault waits one transport interval",
              !mex_done && mex_busy &&
              mex_fault == MEX_FAULT_NONE &&
              !tacc_status_raw[TACC_STATUS_BIT_BUSY]);
        check("second-source preflight prevents first source read",
              tamac_mem_req_count == 0 &&
              !tile_req && !ext_tile_req);
        tick;
        check("second-source span fault completes in second base cycle",
              mex_done && !mex_busy &&
              mex_fault == MEX_FAULT_BUS);
        check("second-source span fault reports first forbidden byte",
              mex_fault_addr == 64'h0000_0000_0010_0000);
        check("second-source preflight prevents first source read",
              tamac_mem_req_count == 0 &&
              !tile_req && !ext_tile_req);
        tick;

        // Dynamic target errors qualify the acknowledged source beat and
        // retire without exposing any partial arithmetic.
        write_csr(CSR_TSRC1, {32'd0, TAMAC_ADDR_B});
        cycle_bank_snapshot = uut.tacc_bank_state;
        tamac_mem_error = 1'b1;
        tamac_mem_error_addr = TAMAC_ADDR_B;
        tamac_mem_fault_addr = 64'h0000_0000_0000_0220;
        tamac_mem_req_count = 0;
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        tick;
        tick;
        tamac_mem_error = 1'b0;
        check("second source target error reaches TAMAC terminal",
              mex_done && mex_fault == MEX_FAULT_BUS);
        check("dynamic TAMAC error reports target fault address",
              mex_fault_addr == 64'h0000_0000_0000_0220);
        check("dynamic source fault preserves persistent bank",
              uut.tacc_bank_state == cycle_bank_snapshot);
        check("dynamic second-source error issues exactly two reads",
              tamac_mem_req_count == 2);
        tick;
        check("fault retirement cannot publish staged arithmetic",
              uut.tacc_bank_state == cycle_bank_snapshot);

        // Cancellation while a source response is stalled suppresses the MEX
        // completion and raises the private source-lane drain request.
        tamac_mem_respond = 1'b0;
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        caller_cancel[0] = 1'b1;
        tick;
        caller_cancel[0] = 1'b0;
        check("source-wait cancel suppresses TAMAC response",
              !mex_done && !mex_busy &&
              mex_fault == MEX_FAULT_NONE);
        check("source-wait cancel preserves persistent bank",
              uut.tacc_bank_state == cycle_bank_snapshot);
        check("source-wait cancel requests arbiter drain",
              tile_source_cancel);
        tamac_mem_respond = 1'b1;
        tick;
        check("source cancellation pulse is edge-bounded",
              !tile_source_cancel);

        // Reserved function bits and unsupported immediate form remain
        // fail-closed before memory traffic.
        mex_ss = 2'd0;
        mex_funct_byte = 8'h26;
        tamac_mem_req_count = 0;
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("noncanonical TAMAC waits one transport interval",
              !mex_done && mex_busy &&
              mex_fault == MEX_FAULT_NONE &&
              !tacc_status_raw[TACC_STATUS_BIT_BUSY]);
        check("noncanonical TAMAC emits no source request",
              tamac_mem_req_count == 0);
        tick;
        check("noncanonical TAMAC faults in second base cycle",
              mex_done && !mex_busy &&
              mex_fault == MEX_FAULT_ILLEGAL);
        check("noncanonical TAMAC emits no source request",
              tamac_mem_req_count == 0);
        tick;

        mex_ss = 2'd2;
        mex_funct_byte = {5'd0, TMUL_TAMAC};
        mex_valid = 1'b1;
        tick;
        mex_valid = 1'b0;
        check("immediate TAMAC form waits one transport interval",
              !mex_done && mex_busy &&
              mex_fault == MEX_FAULT_NONE &&
              !tacc_status_raw[TACC_STATUS_BIT_BUSY]);
        tick;
        check("immediate TAMAC source form faults in second base cycle",
              mex_done && !mex_busy &&
              mex_fault == MEX_FAULT_ILLEGAL);
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

        check("TACC timing bench observed no memory write request",
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
