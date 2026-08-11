// ============================================================================
// tb_wots.v -- production WOTS controller and shared-Keccak contract
// ============================================================================
//
// Expected chain values were generated independently with Python hashlib
// SHAKE256 over PK.seed || modified ADRS || node.  The bench supplies the DMA
// capture/terminal edges explicitly; mp64_wots never reads a test array or a
// private permutation core out of band.

`timescale 1ns/1ps
`include "mp64_pkg.vh"

module tb_wots;

    localparam [1:0] STATUS_IDLE  = 2'd0;
    localparam [1:0] STATUS_BUSY  = 2'd1;
    localparam [1:0] STATUS_DONE  = 2'd2;
    localparam [1:0] STATUS_ERROR = 2'd3;

    localparam [127:0] EXPECT_START3_STEPS2 =
        128'h40c6609d399ea3a3c3ed5e62d5db0197;
    localparam [127:0] EXPECT_START14_STEPS1 =
        128'hba72b3c66fab7a9b2d3a07f470669b45;
    localparam [127:0] EXPECT_START0_STEPS15 =
        128'h8071a720747ac1bb0e039b02525b0927;

    reg clk;
    reg rst_n;
    initial clk = 1'b0;
    always #5 clk = ~clk;

    reg          w_req;
    reg [4:0]    w_addr;
    reg [63:0]   w_wdata;
    reg          w_wen;
    reg [1:0]    w_size;
    wire [63:0]  w_rdata;
    wire         w_ack;
    wire         w_active;

    wire         dma_valid;
    wire [63:0]  dma_addr;
    reg          dma_accept;
    reg [63:0]   dma_rdata;
    reg          dma_resp_valid;
    reg [1:0]    dma_resp_code;

    reg          sha_req;
    reg [6:0]    sha_addr;
    reg [63:0]   sha_wdata;
    reg          sha_wen;
    reg [1:0]    sha_size;
    wire [63:0]  sha_rdata;
    wire         sha_ack;

    wire         sha_claim;
    wire         sha_grant;
    wire         sha_owned;
    wire         sha_perm_req;
    wire [1599:0] sha_state_in;
    wire [1599:0] sha_state_out;
    wire         sha_perm_busy;
    wire         sha_perm_done;
    wire         sha_release;
    wire         sha_abort;

    integer pass_count;
    integer fail_count;
    integer claim_count;
    integer private_clean;
    integer test_i;

    mp64_wots #(
        .BANK0_SIZE  (64'h100),
        .N_BUS_PORTS (2)
    ) u_wots (
        .clk(clk),
        .rst_n(rst_n),
        .req(w_req),
        .addr(w_addr),
        .wdata(w_wdata),
        .wen(w_wen),
        .size(w_size),
        .rdata(w_rdata),
        .ack(w_ack),
        .active(w_active),
        .dma_valid(dma_valid),
        .dma_addr(dma_addr),
        .dma_accept(dma_accept),
        .dma_rdata(dma_rdata),
        .dma_resp_valid(dma_resp_valid),
        .dma_resp_code(dma_resp_code),
        .sha_claim(sha_claim),
        .sha_grant(sha_grant),
        .sha_owned(sha_owned),
        .sha_perm_req(sha_perm_req),
        .sha_state_in(sha_state_in),
        .sha_state_out(sha_state_out),
        .sha_perm_busy(sha_perm_busy),
        .sha_perm_done(sha_perm_done),
        .sha_release(sha_release),
        .sha_abort(sha_abort)
    );

    mp64_sha3 u_sha (
        .clk(clk),
        .rst_n(rst_n),
        .req(sha_req),
        .addr(sha_addr),
        .wdata(sha_wdata),
        .wen(sha_wen),
        .size(sha_size),
        .rdata(sha_rdata),
        .ack(sha_ack),
        .sha3_stream_en(1'b1),
        .keccak_f1600_en(1'b1),
        .wots_claim(sha_claim),
        .wots_grant(sha_grant),
        .wots_owned(sha_owned),
        .wots_perm_req(sha_perm_req),
        .wots_state_in(sha_state_in),
        .wots_state_out(sha_state_out),
        .wots_perm_busy(sha_perm_busy),
        .wots_perm_done(sha_perm_done),
        .wots_release(sha_release),
        .wots_abort(sha_abort)
    );

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n)
            claim_count <= 0;
        else if (sha_grant)
            claim_count <= claim_count + 1;
    end

    task check;
        input [8*120-1:0] label;
        input             condition;
        begin
            if (condition) begin
                pass_count = pass_count + 1;
                $display("  [PASS] %0s", label);
            end else begin
                fail_count = fail_count + 1;
                $display("  [FAIL] %0s", label);
            end
        end
    endtask

    task reset_all;
        begin
            rst_n          = 1'b0;
            w_req          = 1'b0;
            w_addr         = 5'd0;
            w_wdata        = 64'd0;
            w_wen          = 1'b0;
            w_size         = BUS_BYTE;
            dma_accept     = 1'b0;
            dma_rdata      = 64'd0;
            dma_resp_valid = 1'b0;
            dma_resp_code  = BUS_RESP_OK;
            sha_req        = 1'b0;
            sha_addr       = 7'd0;
            sha_wdata      = 64'd0;
            sha_wen        = 1'b0;
            sha_size       = BUS_BYTE;
            repeat (3) @(negedge clk);
            rst_n = 1'b1;
            @(negedge clk);
        end
    endtask

    task wots_write;
        input [4:0] address;
        input [7:0] value;
        begin
            @(negedge clk);
            w_req   = 1'b1;
            w_addr  = address;
            w_wdata = {56'd0, value};
            w_wen   = 1'b1;
            w_size  = BUS_BYTE;
            @(posedge clk); #1;
            check("legal WOTS byte write acknowledges", w_ack);
            @(negedge clk);
            w_req = 1'b0;
            w_wen = 1'b0;
        end
    endtask

    task wots_read;
        input  [4:0] address;
        output [7:0] value;
        begin
            @(negedge clk);
            w_req   = 1'b1;
            w_addr  = address;
            w_wdata = 64'd0;
            w_wen   = 1'b0;
            w_size  = BUS_BYTE;
            @(posedge clk); #1;
            check("legal WOTS byte read acknowledges", w_ack);
            value = w_rdata[7:0];
            @(negedge clk);
            w_req = 1'b0;
        end
    endtask

    task invalid_access;
        input [4:0] address;
        input       write_access;
        input [1:0] access_size;
        begin
            @(negedge clk);
            w_req   = 1'b1;
            w_addr  = address;
            w_wdata = 64'hffff_ffff_ffff_ffff;
            w_wen   = write_access;
            w_size  = access_size;
            @(posedge clk); #1;
            check("invalid WOTS whole access does not acknowledge", !w_ack);
            @(negedge clk);
            w_req  = 1'b0;
            w_wen  = 1'b0;
            w_size = BUS_BYTE;
        end
    endtask

    task sha_write;
        input [6:0] address;
        input [7:0] value;
        begin
            @(negedge clk);
            sha_req   = 1'b1;
            sha_addr  = address;
            sha_wdata = {56'd0, value};
            sha_wen   = 1'b1;
            sha_size  = BUS_BYTE;
            @(posedge clk); #1;
            check("legal SHA byte command acknowledges", sha_ack);
            @(negedge clk);
            sha_req = 1'b0;
            sha_wen = 1'b0;
        end
    endtask

    task sha_read;
        input  [6:0] address;
        output [7:0] value_out;
        begin
            @(negedge clk);
            sha_req   = 1'b1;
            sha_addr  = address;
            sha_wdata = 64'd0;
            sha_wen   = 1'b0;
            sha_size  = BUS_BYTE;
            @(posedge clk); #1;
            check("legal SHA byte read acknowledges", sha_ack);
            value_out = sha_rdata[7:0];
            @(negedge clk);
            sha_req = 1'b0;
        end
    endtask

    task program_request;
        input [63:0] context_address;
        input [7:0]  start_value;
        input [7:0]  step_value;
        integer byte_index;
        begin
            for (byte_index = 0; byte_index < 8;
                 byte_index = byte_index + 1)
                wots_write(byte_index[4:0],
                           context_address[byte_index*8 +: 8]);
            wots_write(5'h08, step_value);
            wots_write(5'h09, start_value);
        end
    endtask

    task wait_status;
        input [1:0] expected;
        input integer limit;
        output [7:0] observed;
        integer watchdog;
        begin
            observed = 8'hff;
            watchdog = 0;
            // Compare the complete architectural byte.  The 8'hff sentinel's
            // low bits alias STATUS_ERROR, so a low-bit-only comparison would
            // let every ERROR wait pass without sampling the device once.
            while (observed != {6'd0, expected} && watchdog < limit) begin
                wots_read(5'h0a, observed);
                watchdog = watchdog + 1;
            end
            check("WOTS reaches expected architectural status",
                  observed == {6'd0, expected});
        end
    endtask

    function [7:0] context_byte;
        input integer byte_index;
        input integer variant;
        begin
            if (variant == 0)
                context_byte = byte_index[7:0];
            else if (byte_index < 16)
                context_byte = byte_index[7:0];
            else if (byte_index < 48)
                context_byte = 8'h20 + (byte_index - 16);
            else
                context_byte = 8'h80 + (byte_index - 48);
        end
    endfunction

    task accept_dma;
        input [63:0] expected_address;
        integer watchdog;
        begin
            watchdog = 0;
            while (!dma_valid && watchdog < 200) begin
                @(negedge clk);
                watchdog = watchdog + 1;
            end
            check("DMA request becomes valid", dma_valid);
            check("DMA request address is exact and ascending",
                  dma_addr == expected_address);
            dma_accept = 1'b1;
            @(posedge clk); #1;
            @(negedge clk);
            dma_accept = 1'b0;
        end
    endtask

    task respond_dma;
        input [63:0] expected_address;
        input [7:0]  byte_value;
        input [1:0]  response_code;
        begin
            dma_rdata = 64'd0;
            dma_rdata[expected_address[2:0]*8 +: 8] = byte_value;
            dma_resp_code  = response_code;
            dma_resp_valid = 1'b1;
            @(posedge clk); #1;
            @(negedge clk);
            dma_resp_valid = 1'b0;
            dma_resp_code  = BUS_RESP_OK;
        end
    endtask

    task service_dma;
        input [63:0] expected_address;
        input [7:0]  byte_value;
        input [1:0]  response_code;
        begin
            accept_dma(expected_address);
            respond_dma(expected_address, byte_value, response_code);
        end
    endtask

    task feed_context;
        input [63:0] base_address;
        input integer variant;
        integer byte_index;
        begin
            for (byte_index = 0; byte_index < 64;
                 byte_index = byte_index + 1)
                service_dma(base_address + byte_index,
                            context_byte(byte_index, variant),
                            BUS_RESP_OK);
        end
    endtask

    task check_dout;
        input [127:0] expected;
        integer byte_index;
        reg [7:0] value;
        begin
            for (byte_index = 0; byte_index < 16;
                 byte_index = byte_index + 1) begin
                wots_read(5'h10 + byte_index, value);
                check("DOUT byte matches independent expected vector",
                      value == expected[byte_index*8 +: 8]);
            end
        end
    endtask

    task read_cycles;
        output [31:0] cycle_value;
        reg [7:0] cycle_byte;
        begin
            wots_read(5'h0c, cycle_byte);
            cycle_value[7:0] = cycle_byte;
            wots_read(5'h0d, cycle_byte);
            cycle_value[15:8] = cycle_byte;
            wots_read(5'h0e, cycle_byte);
            cycle_value[23:16] = cycle_byte;
            wots_read(5'h0f, cycle_byte);
            cycle_value[31:24] = cycle_byte;
        end
    endtask

    task check_reset_quiescent;
        integer byte_index;
        reg architectural_zero;
        reg private_zero;
        reg shared_zero;
        begin
            architectural_zero = 1'b1;
            private_zero = 1'b1;
            shared_zero = 1'b1;

            for (byte_index = 0; byte_index < 16;
                 byte_index = byte_index + 1) begin
                if (u_wots.dout[byte_index] !== 8'd0)
                    architectural_zero = 1'b0;
                if (u_wots.result_private[byte_index] !== 8'd0)
                    private_zero = 1'b0;
            end
            for (byte_index = 0; byte_index < 64;
                 byte_index = byte_index + 1) begin
                if (u_wots.context_stage[byte_index] !== 8'd0)
                    private_zero = 1'b0;
                if (u_sha.output_window[byte_index] !== 8'd0 ||
                    u_sha.crossing_tail[byte_index] !== 8'd0)
                    shared_zero = 1'b0;
            end

            check("common reset clears WOTS architectural state including CYCLES",
                  architectural_zero &&
                  u_wots.context_addr_reg == 64'd0 &&
                  u_wots.steps_reg == 8'd0 &&
                  u_wots.start_reg == 8'd0 &&
                  u_wots.status_reg == STATUS_IDLE &&
                  u_wots.error_reg == 8'd0 &&
                  u_wots.cycles_reg == 32'd0);
            check("common reset scrubs WOTS private state and counters",
                  private_zero && u_wots.state == 4'd0 &&
                  u_wots.cleanup_kind == 2'd2 &&
                  u_wots.pending_error == 8'd0 &&
                  u_wots.active_context_addr == 64'd0 &&
                  u_wots.active_steps == 8'd0 &&
                  u_wots.active_start == 8'd0 &&
                  u_wots.current_node == 128'd0 &&
                  u_wots.perm_state_reg == 1600'd0 &&
                  u_wots.dma_index == 6'd0 &&
                  u_wots.chain_index == 4'd0 &&
                  u_wots.service_cycles == 6'd0 &&
                  u_wots.dma_accept_count == 0 && !u_wots.req_seen);
            check("common reset withdraws every WOTS external handshake",
                  !w_ack && w_rdata == 64'd0 && !w_active &&
                  !dma_valid && dma_addr == 64'd0 &&
                  !sha_claim && !sha_perm_req &&
                  sha_state_in == 1600'd0 &&
                  !sha_release && !sha_abort);
            check("common reset clears shared SHA owner and service state",
                  shared_zero && u_sha.owner == 2'd0 &&
                  u_sha.phase == 2'd0 && u_sha.error_code == 8'd0 &&
                  u_sha.mode == 2'd0 && u_sha.din_ptr == 8'd0 &&
                  u_sha.squeeze_pos == 9'd0 &&
                  u_sha.state_index == 5'd0 &&
                  u_sha.crossing_count == 7'd0 &&
                  u_sha.operation == 3'd0 &&
                  u_sha.operation_cycles == 6'd0 &&
                  !u_sha.cleanup_pending && u_sha.cleanup_reason == 2'd0 &&
                  !u_sha.req_seen && !sha_ack && sha_rdata == 64'd0 &&
                  !u_sha.wots_claim_seen && !sha_grant &&
                  !sha_owned && !sha_perm_busy && !sha_perm_done);
            check("common reset scrubs the sole shared Keccak core",
                  !u_sha.core_start && !u_sha.core_load_start &&
                  u_sha.core_state_in == 1600'd0 &&
                  !u_sha.core_lane_we && u_sha.core_lane_index == 5'd0 &&
                  u_sha.core_lane_wdata == 64'd0 &&
                  u_sha.core_lane_wstrb == 8'd0 && !u_sha.core_clear &&
                  sha_state_out == 1600'd0 &&
                  u_sha.u_keccak_core.state_out == 1600'd0 &&
                  !u_sha.u_keccak_core.busy &&
                  !u_sha.u_keccak_core.done &&
                  !u_sha.u_keccak_core.clear_done &&
                  u_sha.u_keccak_core.round_count == 5'd0);
        end
    endtask

    reg [7:0] value;
    reg [7:0] observed_status;
    reg [31:0] retained_cycles;
    reg [31:0] stable_cycles;

    initial begin
        pass_count = 0;
        fail_count = 0;
        $display("=== tb_wots: checked WOTS/shared-Keccak contract ===");

        // --------------------------------------------------------------
        // Byte-only aperture and persistent programming bytes.
        // --------------------------------------------------------------
        reset_all;
        wots_read(5'h0a, value);
        check("reset status is IDLE", value[1:0] == STATUS_IDLE);
        wots_write(5'h00, 8'ha5);
        invalid_access(5'h00, 1'b1, BUS_DWORD);
        invalid_access(5'h0b, 1'b1, BUS_BYTE);
        wots_read(5'h00, value);
        check("faulting accesses do not mutate programming", value == 8'ha5);
        wots_read(5'h0b, value);
        check("faulting accesses do not mutate ERROR", value == 8'd0);
        wots_write(5'h0a, 8'd2);
        wots_write(5'h0a, 8'h81);
        wots_read(5'h0a, value);
        check("invalid command publishes persistent ERROR",
              value[1:0] == STATUS_ERROR);
        wots_read(5'h0b, value);
        check("invalid command maps to error 1", value == 8'd1);
        repeat (8) @(posedge clk);
        wots_write(5'h00, 8'h5a);
        wots_write(5'h0a, 8'd1);
        wots_write(5'h0a, 8'h82);
        wots_read(5'h0a, value);
        check("ERROR terminal rejects programming, GO, and later invalid commands",
              value[1:0] == STATUS_ERROR);
        wots_read(5'h0b, value);
        check("ERROR terminal preserves its first error until CLEAR",
              value == 8'd1);
        wots_read(5'h00, value);
        check("ERROR terminal preserves programming bytes until CLEAR",
              value == 8'd0);
        wots_write(5'h0a, 8'd2);
        wots_read(5'h0a, value);
        check("terminal CLEAR returns ERROR to IDLE",
              value[1:0] == STATUS_IDLE);

        // --------------------------------------------------------------
        // Ordered GO validation and stable terminal programming bytes.
        // --------------------------------------------------------------
        program_request(64'hff, 8'hff, 8'hff);
        wots_write(5'h0a, 8'd1);
        wait_status(STATUS_ERROR, 8, observed_status);
        wots_read(5'h0b, value);
        check("STEPS validation has first priority", value == 8'd3);
        wots_read(5'h08, value);
        check("validation error preserves raw STEPS byte", value == 8'hff);

        wots_write(5'h0a, 8'd2);
        program_request(64'd0, 8'd16, 8'd0);
        wots_write(5'h0a, 8'd1);
        wots_read(5'h0b, value);
        check("START geometry is validated before span/owner", value == 8'd4);

        wots_write(5'h0a, 8'd2);
        program_request(64'd0, 8'd15, 8'd1);
        wots_write(5'h0a, 8'd1);
        wots_read(5'h0b, value);
        check("geometry pair START=15 STEPS=1 is exactly above the boundary",
              value == 8'd4);

        wots_write(5'h0a, 8'd2);
        program_request(64'd0, 8'd1, 8'd15);
        wots_write(5'h0a, 8'd1);
        wots_read(5'h0b, value);
        check("geometry pair START=1 STEPS=15 is exactly above the boundary",
              value == 8'd4);

        wots_write(5'h0a, 8'd2);
        program_request(64'hc1, 8'd15, 8'd0);
        wots_write(5'h0a, 8'd1);
        wots_read(5'h0b, value);
        check("one byte above the final Bank0 span is rejected",
              value == 8'd5);

        wots_write(5'h0a, 8'd2);
        program_request(64'hffff_ffff_ffff_ffff, 8'd0, 8'd0);
        wots_write(5'h0a, 8'd1);
        wots_read(5'h0b, value);
        check("one byte below Bank0 wraps and is rejected", value == 8'd5);

        wots_write(5'h0a, 8'd2);
        program_request(64'd0, 8'd15, 8'd0);
        wots_write(5'h0a, 8'd1);
        check("exact first Bank0 span passes complete validation",
              w_active && dma_valid && dma_addr == 64'd0);
        wots_write(5'h0a, 8'd2);
        wait_status(STATUS_IDLE, 16, observed_status);

        // --------------------------------------------------------------
        // Zero-step identity: real 64-byte DMA, no shared claim.
        // --------------------------------------------------------------
        reset_all;
        program_request(64'hc0, 8'd15, 8'd0);
        wots_write(5'h0a, 8'd1);
        check("valid zero-step GO enters BUSY", w_active);
        check("GO resets CYCLES before the first BUSY service cycle",
              u_wots.cycles_reg == 32'd0);

        // Every legal write still acknowledges while BUSY, but programming,
        // GO, and invalid commands must preserve the active request.
        wots_write(5'h00, 8'ha5);
        wots_write(5'h08, 8'd7);
        wots_write(5'h09, 8'd6);
        wots_write(5'h0a, 8'd1);
        wots_write(5'h0a, 8'h81);
        wots_read(5'h0a, value);
        check("BUSY rejects programming, GO, and invalid commands",
              value[1:0] == STATUS_BUSY && w_active);
        wots_read(5'h0b, value);
        check("BUSY command rejection does not create a terminal error",
              value == 8'd0);
        wots_read(5'h00, value);
        check("BUSY preserves the programmed context address",
              value == 8'hc0);
        wots_read(5'h08, value);
        check("BUSY preserves the programmed STEPS byte", value == 8'd0);
        wots_read(5'h09, value);
        check("BUSY preserves the programmed START byte", value == 8'd15);
        check("BUSY rejection leaves the same first DMA beat pending",
              dma_valid && dma_addr == 64'hc0 && claim_count == 0);

        feed_context(64'hc0, 0);
        wait_status(STATUS_DONE, 16, observed_status);
        check("zero-step request never claims Keccak", claim_count == 0);
        check("zero-step request stops after exactly 64 reads", !dma_valid &&
              u_wots.dma_index == 6'd0);
        for (test_i = 0; test_i < 16; test_i = test_i + 1) begin
            wots_read(5'h10 + test_i, value);
            check("zero-step DOUT is context input identity",
                  value == 8'h30 + test_i);
        end
        wots_read(5'h00, value);
        check("exact final Bank0 span succeeds and remains latched",
              value == 8'hc0);
        read_cycles(retained_cycles);
        check("zero-step CYCLES records DMA/cleanup service",
              retained_cycles != 32'd0);

        // DONE is a stable terminal publication.  No legal non-CLEAR write
        // may reprogram it, restart it, replace ERROR, or disturb DOUT.
        wots_write(5'h00, 8'ha5);
        wots_write(5'h08, 8'd1);
        wots_write(5'h09, 8'd0);
        wots_write(5'h0a, 8'd1);
        wots_write(5'h0a, 8'h81);
        repeat (24) @(posedge clk);
        wots_read(5'h0a, value);
        check("DONE remains stable across programming, GO, and invalid commands",
              value[1:0] == STATUS_DONE);
        wots_read(5'h0b, value);
        check("stable DONE retains ERROR=0", value == 8'd0);
        wots_read(5'h00, value);
        check("stable DONE retains programming until CLEAR", value == 8'hc0);
        private_clean = 1;
        for (test_i = 0; test_i < 16; test_i = test_i + 1)
            if (u_wots.dout[test_i] !== 8'h30 + test_i)
                private_clean = 0;
        check("zero-step DOUT remains stable over extra clocks and writes",
              private_clean);
        read_cycles(stable_cycles);
        check("terminal clocks and rejected writes do not change CYCLES",
              stable_cycles == retained_cycles);

        wots_write(5'h0a, 8'd2);
        read_cycles(stable_cycles);
        check("CLEAR retains the complete last CYCLES value",
              stable_cycles == retained_cycles);
        wots_read(5'h00, value);
        check("terminal CLEAR zeros programming", value == 8'd0);

        // A subsequent GO resets a previously retained nonzero count.
        program_request(64'd0, 8'd0, 8'd0);
        read_cycles(stable_cycles);
        check("programming after CLEAR leaves retained CYCLES unchanged",
              stable_cycles == retained_cycles);
        wots_write(5'h0a, 8'd1);
        check("a later GO resets retained CYCLES to zero",
              u_wots.cycles_reg == 32'd0);
        wots_write(5'h0a, 8'd2);
        wait_status(STATUS_IDLE, 16, observed_status);

        // --------------------------------------------------------------
        // Independent two-step vector and exact constructed-state layout.
        // --------------------------------------------------------------
        reset_all;
        program_request(64'h20, 8'd3, 8'd2);
        wots_write(5'h0a, 8'd1);
        feed_context(64'h20, 1);
        @(posedge clk); #1;
        check("first nonzero step is submitted to shared SHA service",
              sha_perm_req && sha_owned);
        sha_read(7'h01, value);
        check("SHA front end remains responsive with WOTS owner status",
              value == 8'h0d && sha_owned);
        check("constructed state preserves seed", sha_state_in[127:0] ==
              128'h0f0e0d0c0b0a09080706050403020100);
        check("constructed ADRS hash field is big-endian step 3",
              sha_state_in[44*8 +: 32] == 32'h0300_0000);
        check("constructed state carries initial node",
              sha_state_in[48*8 +: 128] ==
              128'h8f8e8d8c8b8a89888786858483828180);
        check("constructed SHAKE padding bytes are exact",
              sha_state_in[64*8 +: 8] == 8'h1f &&
              sha_state_in[135*8 +: 8] == 8'h80);
        check("constructed capacity is zero",
              sha_state_in[1599:1088] == 512'd0);
        wait_status(STATUS_DONE, 100, observed_status);
        check("nonzero request claims shared service once", claim_count == 1);
        check("DONE releases shared Keccak owner", !sha_owned);
        check_dout(EXPECT_START3_STEPS2);
        read_cycles(retained_cycles);
        repeat (32) @(posedge clk);
        private_clean = 1;
        for (test_i = 0; test_i < 16; test_i = test_i + 1)
            if (u_wots.dout[test_i] !==
                EXPECT_START3_STEPS2[test_i*8 +: 8])
                private_clean = 0;
        check("nonzero DOUT remains stable over extra terminal clocks",
              private_clean && u_wots.status_reg == STATUS_DONE &&
              u_wots.error_reg == 8'd0);
        read_cycles(stable_cycles);
        check("nonzero terminal CYCLES remains stable over extra clocks",
              stable_cycles == retained_cycles);

        private_clean = 1;
        for (test_i = 0; test_i < 64; test_i = test_i + 1)
            if (u_wots.context_stage[test_i] !== 8'd0)
                private_clean = 0;
        check("terminal publication follows private context scrub",
              private_clean && u_wots.current_node == 128'd0 &&
              u_wots.perm_state_reg == 1600'd0 &&
              u_wots.active_context_addr == 64'd0);
        check("shared Keccak resident state is wiped before DONE",
              u_sha.u_keccak_core.state_out == 1600'd0);

        // Valid edge geometry: highest nonzero start and longest chain.
        reset_all;
        program_request(64'h20, 8'd14, 8'd1);
        wots_write(5'h0a, 8'd1);
        feed_context(64'h20, 1);
        wait_status(STATUS_DONE, 80, observed_status);
        check_dout(EXPECT_START14_STEPS1);

        reset_all;
        program_request(64'h20, 8'd0, 8'd15);
        wots_write(5'h0a, 8'd1);
        feed_context(64'h20, 1);
        wait_status(STATUS_DONE, 400, observed_status);
        check_dout(EXPECT_START0_STEPS15);

        // --------------------------------------------------------------
        // A pre-existing raw owner rejects nonzero WOTS before DMA.
        // --------------------------------------------------------------
        reset_all;
        sha_write(7'h00, 8'd6);
        program_request(64'h20, 8'd0, 8'd1);
        wots_write(5'h0a, 8'd1);
        wait_status(STATUS_ERROR, 16, observed_status);
        wots_read(5'h0b, value);
        check("busy shared owner maps to checked error 2", value == 8'd2);
        check("owner failure issues no DMA beat", !dma_valid);
        wots_write(5'h0a, 8'd2);
        sha_write(7'h00, 8'd7);

        // --------------------------------------------------------------
        // Classified terminal DMA faults.
        // --------------------------------------------------------------
        reset_all;
        program_request(64'd0, 8'd0, 8'd0);
        wots_write(5'h0a, 8'd1);
        service_dma(64'd0, 8'ha5, BUS_RESP_TARGET_FAULT);
        wait_status(STATUS_ERROR, 16, observed_status);
        wots_read(5'h0b, value);
        check("target response maps to DMA target fault", value == 8'd6);

        wots_write(5'h0a, 8'd2);
        program_request(64'd0, 8'd0, 8'd0);
        wots_write(5'h0a, 8'd1);
        service_dma(64'd0, 8'ha5, BUS_RESP_MEM_TIMEOUT);
        wait_status(STATUS_ERROR, 16, observed_status);
        wots_read(5'h0b, value);
        check("memory-timeout response maps to error 7", value == 8'd7);

        wots_write(5'h0a, 8'd2);
        program_request(64'd0, 8'd0, 8'd0);
        wots_write(5'h0a, 8'd1);
        service_dma(64'd0, 8'ha5, BUS_RESP_PROTOCOL);
        wait_status(STATUS_ERROR, 16, observed_status);
        wots_read(5'h0b, value);
        check("reserved response maps to protocol error 9", value == 8'd9);

        // --------------------------------------------------------------
        // Local accept deadline: final-edge acceptance wins; otherwise 8.
        // --------------------------------------------------------------
        wots_write(5'h0a, 8'd2);
        program_request(64'd0, 8'd0, 8'd0);
        wots_write(5'h0a, 8'd1);
        u_wots.dma_accept_count = 17'd65790;
        dma_accept = 1'b1;
        @(posedge clk); #1;
        @(negedge clk);
        dma_accept = 1'b0;
        check("acceptance on local deadline edge wins", u_wots.state == 4'd4);
        respond_dma(64'd0, 8'h11, BUS_RESP_OK);
        wots_write(5'h0a, 8'd2);
        wait_status(STATUS_IDLE, 16, observed_status);

        program_request(64'd0, 8'd0, 8'd0);
        wots_write(5'h0a, 8'd1);
        u_wots.dma_accept_count = 17'd65790;
        @(posedge clk); #1;
        wait_status(STATUS_ERROR, 16, observed_status);
        wots_read(5'h0b, value);
        check("unaccepted terminal deadline maps to local error 8",
              value == 8'd8);

        // --------------------------------------------------------------
        // CLEAR withdraws preaccept and accepted same-edge capture drains.
        // --------------------------------------------------------------
        wots_write(5'h0a, 8'd2);
        program_request(64'd0, 8'd0, 8'd0);
        wots_write(5'h0a, 8'd1);
        wots_write(5'h0a, 8'd2);
        wait_status(STATUS_IDLE, 16, observed_status);
        check("preaccept CLEAR withdraws request-valid", !dma_valid);

        program_request(64'd0, 8'd0, 8'd0);
        wots_write(5'h0a, 8'd1);
        // Let req_seen observe the GO request low, then present CLEAR and
        // request-accept on the same clock edge.
        @(posedge clk); #1;
        @(negedge clk);
        w_req      = 1'b1;
        w_addr     = 5'h0a;
        w_wdata    = 64'd2;
        w_wen      = 1'b1;
        w_size     = BUS_BYTE;
        dma_accept = 1'b1;
        @(posedge clk); #1;
        check("same-edge CLEAR command acknowledges", w_ack);
        check("capture wins and enters abort drain", w_active &&
              u_wots.state == 4'd8 && !dma_valid);
        @(negedge clk);
        w_req      = 1'b0;
        w_wen      = 1'b0;
        dma_accept = 1'b0;
        u_wots.cycles_reg = 32'hffff_fffe;
        respond_dma(64'd0, 8'h22, BUS_RESP_OK);
        wait_status(STATUS_IDLE, 16, observed_status);
        wots_read(5'h0c, value);
        check("CYCLES saturates through accepted-beat abort drain",
              value == 8'hff);
        check("drained CLEAR issues no successor beat", !dma_valid);

        // --------------------------------------------------------------
        // CLEAR during a live permutation waits for safe SHA wipe/release.
        // --------------------------------------------------------------
        reset_all;
        program_request(64'h20, 8'd0, 8'd1);
        wots_write(5'h0a, 8'd1);
        feed_context(64'h20, 1);
        while (!sha_perm_busy)
            @(negedge clk);
        wots_write(5'h0a, 8'd2);
        check("permutation CLEAR stays BUSY during ordered cleanup", w_active);
        wait_status(STATUS_IDLE, 80, observed_status);
        check("permutation CLEAR releases shared owner", !sha_owned);
        check("permutation CLEAR wipes shared resident state",
              u_sha.u_keccak_core.state_out == 1600'd0);
        wots_read(5'h10, value);
        check("aborted permutation publishes no DOUT", value == 8'd0);

        // --------------------------------------------------------------
        // Common reset is fail-closed in each irrevocability phase.
        // --------------------------------------------------------------
        reset_all;
        program_request(64'd0, 8'd0, 8'd0);
        wots_write(5'h0a, 8'd1);
        repeat (4) @(posedge clk);
        #1;
        check("reset setup reaches an unaccepted DMA request",
              dma_valid && u_wots.state == 4'd3 &&
              u_wots.cycles_reg != 32'd0);
        reset_all;
        check_reset_quiescent;

        program_request(64'd0, 8'd0, 8'd0);
        wots_write(5'h0a, 8'd1);
        accept_dma(64'd0);
        check("reset setup reaches one accepted outstanding DMA beat",
              !dma_valid && u_wots.state == 4'd4 && w_active);
        reset_all;
        check_reset_quiescent;

        program_request(64'h20, 8'd0, 8'd1);
        wots_write(5'h0a, 8'd1);
        feed_context(64'h20, 1);
        test_i = 0;
        while (!sha_perm_busy && test_i < 40) begin
            @(negedge clk);
            test_i = test_i + 1;
        end
        check("reset setup reaches a live shared permutation",
              sha_perm_busy && sha_owned && w_active &&
              u_sha.u_keccak_core.state_out != 1600'd0);
        reset_all;
        check_reset_quiescent;

        $display("------------------------------------------------------------");
        $display("WOTS contract: %0d passed, %0d failed",
                 pass_count, fail_count);
        if (fail_count != 0)
            $fatal(1, "WOTS contract failures");
        $finish;
    end

    initial begin
        #5_000_000;
        $fatal(1, "tb_wots timeout");
    end

endmodule
