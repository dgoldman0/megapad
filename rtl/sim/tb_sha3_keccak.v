// ============================================================================
// tb_sha3_keccak.v -- authoritative SHA3/SHAKE/raw-Keccak RTL contract bench
// ============================================================================

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_sha3_keccak;

    reg clk;
    reg rst_n;
    always #5 clk = ~clk;

    reg          req;
    reg [6:0]    addr;
    reg [63:0]   wdata;
    reg          wen;
    reg [1:0]    size;
    wire [63:0]  rdata;
    wire         ack;

    reg          sha3_stream_en;
    reg          keccak_f1600_en;
    reg          wots_claim;
    wire         wots_grant;
    wire         wots_owned;
    reg          wots_perm_req;
    reg [1599:0] wots_state_in;
    wire [1599:0] wots_state_out;
    wire         wots_perm_busy;
    wire         wots_perm_done;
    reg          wots_release;
    reg          wots_abort;

    mp64_sha3 dut (
        .clk              (clk),
        .rst_n            (rst_n),
        .req              (req),
        .addr             (addr),
        .wdata            (wdata),
        .wen              (wen),
        .size             (size),
        .rdata            (rdata),
        .ack              (ack),
        .sha3_stream_en   (sha3_stream_en),
        .keccak_f1600_en  (keccak_f1600_en),
        .wots_claim       (wots_claim),
        .wots_grant       (wots_grant),
        .wots_owned       (wots_owned),
        .wots_perm_req    (wots_perm_req),
        .wots_state_in    (wots_state_in),
        .wots_state_out   (wots_state_out),
        .wots_perm_busy   (wots_perm_busy),
        .wots_perm_done   (wots_perm_done),
        .wots_release     (wots_release),
        .wots_abort       (wots_abort)
    );

    integer pass_count;
    integer fail_count;
    integer cycles;
    integer i;
    integer b;
    reg [63:0] rd;

    task check;
        input condition;
        input [8*96-1:0] label;
        begin
            if (condition) begin
                pass_count = pass_count + 1;
                $display("PASS: %0s", label);
            end else begin
                fail_count = fail_count + 1;
                $display("FAIL: %0s", label);
            end
        end
    endtask

    task write_access;
        input [6:0] a;
        input [63:0] d;
        input [1:0] access_size;
        begin
            @(negedge clk);
            req   = 1'b1;
            addr  = a;
            wdata = d;
            wen   = 1'b1;
            size  = access_size;
            cycles = 0;
            while (!ack && cycles < 80) begin
                @(posedge clk);
                #1;
                cycles = cycles + 1;
            end
            check(ack, "well-formed write acknowledged");
            @(negedge clk);
            req = 1'b0;
            wen = 1'b0;
            @(posedge clk);
            #1;
        end
    endtask

    task read_access;
        input [6:0] a;
        input [1:0] access_size;
        output [63:0] value;
        begin
            @(negedge clk);
            req  = 1'b1;
            addr = a;
            wen  = 1'b0;
            size = access_size;
            cycles = 0;
            while (!ack && cycles < 80) begin
                @(posedge clk);
                #1;
                cycles = cycles + 1;
            end
            check(ack, "well-formed read acknowledged");
            value = rdata;
            @(negedge clk);
            req = 1'b0;
            @(posedge clk);
            #1;
        end
    endtask

    task expect_no_ack;
        input [6:0] a;
        input direction;
        input [1:0] access_size;
        begin
            @(negedge clk);
            req   = 1'b1;
            addr  = a;
            wdata = 64'hffff_ffff_ffff_ffff;
            wen   = direction;
            size  = access_size;
            repeat (5) begin
                @(posedge clk);
                #1;
                check(!ack, "forbidden whole access is not acknowledged");
            end
            @(negedge clk);
            req = 1'b0;
            wen = 1'b0;
            @(posedge clk);
            #1;
        end
    endtask

    task wait_status;
        input [7:0] wanted;
        input integer bound;
        begin
            cycles = 0;
            while ({dut.owner, dut.phase} != wanted && cycles < bound) begin
                @(posedge clk);
                #1;
                cycles = cycles + 1;
            end
            check({dut.owner, dut.phase} == wanted,
                  "status reached expected phase within architectural bound");
        end
    endtask

    task clear_mmio;
        begin
            write_access(7'h00, 64'd7, BUS_BYTE);
            wait_status(8'h00, 64);
        end
    endtask

    task set_mode_and_init;
        input [1:0] selected_mode;
        begin
            write_access(7'h02, {62'd0, selected_mode}, BUS_BYTE);
            write_access(7'h00, 64'd1, BUS_BYTE);
            read_access(7'h01, BUS_BYTE, rd);
            check(rd[7:0] == 8'h04, "INIT claims idle MMIO sponge owner");
        end
    endtask

    task write_raw_lane;
        input [4:0] lane_no;
        input [63:0] value;
        begin
            write_access(7'h50, {59'd0, lane_no}, BUS_BYTE);
            write_access(7'h58, value, BUS_DWORD);
        end
    endtask

    task read_raw_lane;
        input [4:0] lane_no;
        output [63:0] value;
        begin
            write_access(7'h50, {59'd0, lane_no}, BUS_BYTE);
            read_access(7'h58, BUS_DWORD, value);
        end
    endtask

    // --------------------------------------------------------------------
    // Independent, test-local Keccak oracle.  It runs all rounds in zero
    // simulation time and shares no state or control with the DUT.
    // --------------------------------------------------------------------
    reg [63:0] ref_state [0:24];
    reg [63:0] ref_c [0:4];
    reg [63:0] ref_d [0:4];
    reg [63:0] ref_b [0:24];
    reg [63:0] ref_next [0:24];
    reg [7:0]  ref_tail [0:39];

    function [63:0] ref_rc;
        input integer round_no;
        begin
            case (round_no)
                0:  ref_rc = 64'h0000_0000_0000_0001;
                1:  ref_rc = 64'h0000_0000_0000_8082;
                2:  ref_rc = 64'h8000_0000_0000_808a;
                3:  ref_rc = 64'h8000_0000_8000_8000;
                4:  ref_rc = 64'h0000_0000_0000_808b;
                5:  ref_rc = 64'h0000_0000_8000_0001;
                6:  ref_rc = 64'h8000_0000_8000_8081;
                7:  ref_rc = 64'h8000_0000_0000_8009;
                8:  ref_rc = 64'h0000_0000_0000_008a;
                9:  ref_rc = 64'h0000_0000_0000_0088;
                10: ref_rc = 64'h0000_0000_8000_8009;
                11: ref_rc = 64'h0000_0000_8000_000a;
                12: ref_rc = 64'h0000_0000_8000_808b;
                13: ref_rc = 64'h8000_0000_0000_008b;
                14: ref_rc = 64'h8000_0000_0000_8089;
                15: ref_rc = 64'h8000_0000_0000_8003;
                16: ref_rc = 64'h8000_0000_0000_8002;
                17: ref_rc = 64'h8000_0000_0000_0080;
                18: ref_rc = 64'h0000_0000_0000_800a;
                19: ref_rc = 64'h8000_0000_8000_000a;
                20: ref_rc = 64'h8000_0000_8000_8081;
                21: ref_rc = 64'h8000_0000_0000_8080;
                22: ref_rc = 64'h0000_0000_8000_0001;
                23: ref_rc = 64'h8000_0000_8000_8008;
                default: ref_rc = 64'd0;
            endcase
        end
    endfunction

    function integer ref_rotation;
        input integer lane_no;
        begin
            case (lane_no)
                0: ref_rotation=0;   1: ref_rotation=1;
                2: ref_rotation=62;  3: ref_rotation=28;
                4: ref_rotation=27;  5: ref_rotation=36;
                6: ref_rotation=44;  7: ref_rotation=6;
                8: ref_rotation=55;  9: ref_rotation=20;
                10: ref_rotation=3;  11: ref_rotation=10;
                12: ref_rotation=43; 13: ref_rotation=25;
                14: ref_rotation=39; 15: ref_rotation=41;
                16: ref_rotation=45; 17: ref_rotation=15;
                18: ref_rotation=21; 19: ref_rotation=8;
                20: ref_rotation=18; 21: ref_rotation=2;
                22: ref_rotation=61; 23: ref_rotation=56;
                24: ref_rotation=14;
                default: ref_rotation=0;
            endcase
        end
    endfunction

    function [63:0] ref_rol;
        input [63:0] value;
        input integer amount;
        begin
            if (amount == 0)
                ref_rol = value;
            else
                ref_rol = (value << amount) | (value >> (64-amount));
        end
    endfunction

    function [7:0] ref_byte;
        input integer byte_no;
        begin
            ref_byte = ref_state[byte_no/8][(byte_no%8)*8 +: 8];
        end
    endfunction

    function [63:0] ref_qword;
        input integer byte_no;
        integer qb;
        begin
            ref_qword = 64'd0;
            for (qb = 0; qb < 8; qb = qb + 1)
                ref_qword[qb*8 +: 8] = ref_byte(byte_no+qb);
        end
    endfunction

    task ref_zero;
        integer k;
        begin
            for (k = 0; k < 25; k = k + 1)
                ref_state[k] = 64'd0;
        end
    endtask

    task ref_xor_byte;
        input integer byte_no;
        input [7:0] value;
        begin
            ref_state[byte_no/8][(byte_no%8)*8 +: 8] =
                ref_state[byte_no/8][(byte_no%8)*8 +: 8] ^ value;
        end
    endtask

    task ref_permute;
        integer round_no;
        integer rx;
        integer ry;
        integer ri;
        begin
            for (round_no = 0; round_no < 24;
                 round_no = round_no + 1) begin
                for (rx = 0; rx < 5; rx = rx + 1)
                    ref_c[rx] = ref_state[rx] ^ ref_state[rx+5] ^
                                ref_state[rx+10] ^ ref_state[rx+15] ^
                                ref_state[rx+20];
                for (rx = 0; rx < 5; rx = rx + 1)
                    ref_d[rx] = ref_c[(rx+4)%5] ^
                                ref_rol(ref_c[(rx+1)%5], 1);
                for (ry = 0; ry < 5; ry = ry + 1)
                    for (rx = 0; rx < 5; rx = rx + 1) begin
                        ri = rx + 5*ry;
                        ref_b[ry + 5*((2*rx + 3*ry)%5)] =
                            ref_rol(ref_state[ri] ^ ref_d[rx],
                                    ref_rotation(ri));
                    end
                for (ry = 0; ry < 5; ry = ry + 1)
                    for (rx = 0; rx < 5; rx = rx + 1)
                        ref_next[rx+5*ry] = ref_b[rx+5*ry] ^
                            (~ref_b[((rx+1)%5)+5*ry] &
                              ref_b[((rx+2)%5)+5*ry]);
                ref_next[0] = ref_next[0] ^ ref_rc(round_no);
                for (ri = 0; ri < 25; ri = ri + 1)
                    ref_state[ri] = ref_next[ri];
            end
        end
    endtask

    function [63:0] zero_permutation_lane;
        input integer lane_no;
        begin
            case (lane_no)
                0:  zero_permutation_lane=64'hf1258f7940e1dde7;
                1:  zero_permutation_lane=64'h84d5ccf933c0478a;
                2:  zero_permutation_lane=64'hd598261ea65aa9ee;
                3:  zero_permutation_lane=64'hbd1547306f80494d;
                4:  zero_permutation_lane=64'h8b284e056253d057;
                5:  zero_permutation_lane=64'hff97a42d7f8e6fd4;
                6:  zero_permutation_lane=64'h90fee5a0a44647c4;
                7:  zero_permutation_lane=64'h8c5bda0cd6192e76;
                8:  zero_permutation_lane=64'had30a6f71b19059c;
                9:  zero_permutation_lane=64'h30935ab7d08ffc64;
                10: zero_permutation_lane=64'heb5aa93f2317d635;
                11: zero_permutation_lane=64'ha9a6e6260d712103;
                12: zero_permutation_lane=64'h81a57c16dbcf555f;
                13: zero_permutation_lane=64'h43b831cd0347c826;
                14: zero_permutation_lane=64'h01f22f1a11a5569f;
                15: zero_permutation_lane=64'h05e5635a21d9ae61;
                16: zero_permutation_lane=64'h64befef28cc970f2;
                17: zero_permutation_lane=64'h613670957bc46611;
                18: zero_permutation_lane=64'hb87c5a554fd00ecb;
                19: zero_permutation_lane=64'h8c3ee88a1ccf32c8;
                20: zero_permutation_lane=64'h940c7922ae3a2614;
                21: zero_permutation_lane=64'h1841f924a2c509e4;
                22: zero_permutation_lane=64'h16f53526e70465c2;
                23: zero_permutation_lane=64'h75f644e97f30a13b;
                24: zero_permutation_lane=64'heaf1ff7b5ceca249;
                default: zero_permutation_lane=64'd0;
            endcase
        end
    endfunction

    initial begin
        clk               = 1'b0;
        rst_n             = 1'b0;
        req               = 1'b0;
        addr              = 7'd0;
        wdata             = 64'd0;
        wen               = 1'b0;
        size              = BUS_BYTE;
        sha3_stream_en    = 1'b1;
        keccak_f1600_en   = 1'b1;
        wots_claim        = 1'b0;
        wots_perm_req     = 1'b0;
        wots_state_in     = 1600'd0;
        wots_release      = 1'b0;
        wots_abort        = 1'b0;
        pass_count        = 0;
        fail_count        = 0;

        repeat (4) @(posedge clk);
        rst_n = 1'b1;
        repeat (2) @(posedge clk);
        #1;

        // Reset and whole-access preflight.
        read_access(7'h01, BUS_BYTE, rd);
        check(rd[7:0] == 8'h00, "reset status is none/IDLE");
        read_access(7'h03, BUS_BYTE, rd);
        check(rd[7:0] == 8'h00, "reset error is clear");
        expect_no_ack(7'h04, 1'b0, BUS_BYTE);
        expect_no_ack(7'h01, 1'b1, BUS_BYTE);
        expect_no_ack(7'h02, 1'b1, BUS_WORD);
        expect_no_ack(7'h11, 1'b0, BUS_DWORD);
        read_access(7'h03, BUS_BYTE, rd);
        check(rd[7:0] == 8'h00,
              "bus-faulting accesses do not mutate ERROR");

        // Complete-byte decode, mode validation, and capability priority.
        write_access(7'h00, 64'h102, BUS_BYTE);
        read_access(7'h03, BUS_BYTE, rd);
        check(rd[7:0] == 8'h01, "command aliases are rejected");
        clear_mmio();
        write_access(7'h02, 64'd4, BUS_BYTE);
        read_access(7'h03, BUS_BYTE, rd);
        check(rd[7:0] == 8'h03, "invalid CTRL reports mode error");
        clear_mmio();

        sha3_stream_en = 1'b0;
        write_access(7'h00, 64'd1, BUS_BYTE);
        read_access(7'h03, BUS_BYTE, rd);
        check(rd[7:0] == 8'h06, "disabled hash command reports unavailable");
        clear_mmio();
        sha3_stream_en = 1'b1;

        // SHA3-256("") exact NIST known answer and stable qword DOUT.
        set_mode_and_init(2'd0);
        write_access(7'h00, 64'd3, BUS_BYTE);
        wait_status(8'h06, 32);
        read_access(7'h10, BUS_DWORD, rd);
        check(rd == 64'h66d71ebff8c6ffa7, "SHA3-256 empty qword 0");
        read_access(7'h18, BUS_DWORD, rd);
        check(rd == 64'h62d661a05647c151, "SHA3-256 empty qword 1");
        read_access(7'h20, BUS_DWORD, rd);
        check(rd == 64'hfa493be44dff80f5, "SHA3-256 empty qword 2");
        read_access(7'h28, BUS_DWORD, rd);
        check(rd == 64'h4a43f8804b0ad882, "SHA3-256 empty qword 3");
        read_access(7'h10, BUS_DWORD, rd);
        check(rd == 64'h66d71ebff8c6ffa7, "DOUT remains stable in DONE");
        read_access(7'h30, BUS_DWORD, rd);
        check(rd == 64'd0, "SHA3-256 zero-fills remainder of DOUT");
        clear_mmio();

        // SHA3-512("") exact NIST known answer.
        set_mode_and_init(2'd1);
        write_access(7'h00, 64'd3, BUS_BYTE);
        wait_status(8'h06, 32);
        read_access(7'h10, BUS_DWORD, rd);
        check(rd == 64'hc59a3aa2cc739fa6, "SHA3-512 empty qword 0");
        read_access(7'h18, BUS_DWORD, rd);
        check(rd == 64'h6e755a18dc67b5c8, "SHA3-512 empty qword 1");
        read_access(7'h20, BUS_DWORD, rd);
        check(rd == 64'h5958e24f1682c997, "SHA3-512 empty qword 2");
        read_access(7'h28, BUS_DWORD, rd);
        check(rd == 64'ha6805c47c1dcd1e0, "SHA3-512 empty qword 3");
        read_access(7'h30, BUS_DWORD, rd);
        check(rd == 64'h4cf9f5f13a12b215, "SHA3-512 empty qword 4");
        read_access(7'h38, BUS_DWORD, rd);
        check(rd == 64'h58c53a2c40e9e311, "SHA3-512 empty qword 5");
        read_access(7'h40, BUS_DWORD, rd);
        check(rd == 64'he3d3b6959d1900f5, "SHA3-512 empty qword 6");
        read_access(7'h48, BUS_DWORD, rd);
        check(rd == 64'h26cd1d2886857501, "SHA3-512 empty qword 7");
        clear_mmio();

        // Exact-rate automatic absorption.  The following DIN is held and
        // then accepted as byte zero, never acknowledged and discarded.
        set_mode_and_init(2'd0);
        for (i = 0; i < 136; i = i + 1)
            write_access(7'h08, 64'd0, BUS_BYTE);
        @(negedge clk);
        req   = 1'b1;
        addr  = 7'h08;
        wdata = 64'd0;
        wen   = 1'b1;
        size  = BUS_BYTE;
        cycles = 0;
        while (!ack && cycles < 40) begin
            @(posedge clk);
            #1;
            cycles = cycles + 1;
        end
        check(ack && cycles > 1 && cycles <= 32,
              "DIN after a full block is held within the 32-cycle bound");
        @(negedge clk);
        req = 1'b0;
        wen = 1'b0;
        @(posedge clk);
        #1;
        write_access(7'h00, 64'd3, BUS_BYTE);
        wait_status(8'h06, 32);

        ref_zero();
        ref_permute();
        ref_xor_byte(1, 8'h06);
        ref_xor_byte(135, 8'h80);
        ref_permute();
        for (i = 0; i < 4; i = i + 1) begin
            read_access(7'h10+i*8, BUS_DWORD, rd);
            check(rd == ref_qword(i*8),
                  "exact-rate plus held byte matches independent oracle");
        end
        clear_mmio();

        // SHAKE128 empty: known first qword plus consecutive 64-byte windows,
        // including a NEXT that crosses the 168-byte rate boundary.
        set_mode_and_init(2'd2);
        write_access(7'h00, 64'd3, BUS_BYTE);
        wait_status(8'h06, 32);
        ref_zero();
        ref_xor_byte(0, 8'h1f);
        ref_xor_byte(167, 8'h80);
        ref_permute();
        read_access(7'h10, BUS_DWORD, rd);
        check(rd == 64'h7d828fe8a42b9c7f,
              "SHAKE128 empty first qword known answer");
        for (i = 0; i < 8; i = i + 1) begin
            read_access(7'h10+i*8, BUS_DWORD, rd);
            check(rd == ref_qword(i*8), "SHAKE FINAL window is sequential");
        end

        write_access(7'h00, 64'd4, BUS_BYTE);
        wait_status(8'h06, 32);
        for (i = 0; i < 8; i = i + 1) begin
            read_access(7'h10+i*8, BUS_DWORD, rd);
            check(rd == ref_qword(64+i*8),
                  "SHAKE NEXT window without permutation is sequential");
        end

        for (i = 0; i < 40; i = i + 1)
            ref_tail[i] = ref_byte(128+i);
        ref_permute();
        write_access(7'h00, 64'd4, BUS_BYTE);
        wait_status(8'h06, 32);
        for (i = 0; i < 64; i = i + 1) begin
            read_access(7'h10+i, BUS_BYTE, rd);
            if (i < 40)
                check(rd[7:0] == ref_tail[i],
                      "SHAKE crossing window preserves old-rate tail");
            else
                check(rd[7:0] == ref_byte(i-40),
                      "SHAKE crossing window continues at new-rate head");
        end
        clear_mmio();

        // Raw zero-state published vector and exact little-endian lane map.
        write_access(7'h00, 64'd6, BUS_BYTE);
        wait_status(8'h0a, 32);
        for (i = 0; i < 25; i = i + 1) begin
            read_raw_lane(i, rd);
            check(rd == zero_permutation_lane(i),
                  "published zero-state Keccak-f[1600] lane");
        end
        clear_mmio();

        // Byte and qword round trips, no index auto-increment, and a nonzero
        // full-state differential permutation.
        write_raw_lane(5'd7, 64'h0123_4567_89ab_cdef);
        read_raw_lane(5'd7, rd);
        check(rd == 64'h0123_4567_89ab_cdef,
              "aligned qword lane round trip");
        write_access(7'h5b, 64'haa, BUS_BYTE);
        read_access(7'h50, BUS_BYTE, rd);
        check(rd[4:0] == 5'd7, "STATE_INDEX does not auto-increment");
        read_access(7'h58, BUS_DWORD, rd);
        check(rd == 64'h0123_4567_aaab_cdef,
              "byte write changes only selected little-endian lane byte");
        clear_mmio();

        ref_zero();
        for (i = 0; i < 25; i = i + 1) begin
            ref_state[i] = 64'h9e37_79b9_7f4a_7c15 ^ i;
            write_raw_lane(i, ref_state[i]);
        end
        ref_permute();
        write_access(7'h00, 64'd6, BUS_BYTE);
        wait_status(8'h0a, 32);
        for (i = 0; i < 25; i = i + 1) begin
            read_raw_lane(i, rd);
            check(rd == ref_state[i],
                  "nonzero raw state matches independent permutation oracle");
        end

        // Invalid index is a device error; raw writes after DONE are rejected.
        write_access(7'h50, 64'd25, BUS_BYTE);
        read_access(7'h03, BUS_BYTE, rd);
        check(rd[7:0] == 8'h04, "invalid raw lane index reports error 4");
        clear_mmio();

        keccak_f1600_en = 1'b0;
        write_access(7'h58, 64'h1234, BUS_DWORD);
        read_access(7'h03, BUS_BYTE, rd);
        check(rd[7:0] == 8'h06, "disabled raw write reports unavailable");
        clear_mmio();
        keccak_f1600_en = 1'b1;

        // Busy rejection preserves the active operation, ERROR, and DOUT.
        set_mode_and_init(2'd0);
        write_access(7'h00, 64'd3, BUS_BYTE);
        write_access(7'h00, 64'd2, BUS_BYTE);
        read_access(7'h03, BUS_BYTE, rd);
        check(rd[7:0] == 8'h00,
              "command during BUSY preserves active operation and ERROR");
        wait_status(8'h06, 32);
        clear_mmio();

        // Abort a raw permutation and prove ordered wipe/release.
        write_raw_lane(5'd0, 64'hfeed_face_dead_beef);
        write_access(7'h00, 64'd6, BUS_BYTE);
        write_access(7'h00, 64'd7, BUS_BYTE);
        wait_status(8'h00, 64);
        read_access(7'h03, BUS_BYTE, rd);
        check(rd[7:0] == 8'h00, "CLEAR resets error after busy abort");

        // Shared WOTS ownership keeps the MMIO front responsive while every
        // state/command mutation, including command 7, is preserved/rejected.
        @(negedge clk);
        wots_claim = 1'b1;
        @(posedge clk);
        #1;
        check(wots_grant && wots_owned, "WOTS claim acquires shared service");
        @(negedge clk);
        wots_claim = 1'b0;
        read_access(7'h01, BUS_BYTE, rd);
        check(rd[7:0] == 8'h0d, "STATUS remains 0x0d while WOTS owns service");
        write_access(7'h00, 64'd7, BUS_BYTE);
        write_access(7'h50, 64'd9, BUS_BYTE);
        read_access(7'h03, BUS_BYTE, rd);
        check(rd[7:0] == 8'h00,
              "MMIO mutations during WOTS ownership preserve ERROR");
        read_access(7'h01, BUS_BYTE, rd);
        check(rd[7:0] == 8'h0d, "MMIO CLEAR cannot release WOTS ownership");

        wots_state_in = 1600'd0;
        @(negedge clk);
        wots_perm_req = 1'b1;
        @(posedge clk);
        #1;
        @(negedge clk);
        wots_perm_req = 1'b0;
        cycles = 0;
        while (!wots_perm_done && cycles < 40) begin
            @(posedge clk);
            #1;
            cycles = cycles + 1;
        end
        check(wots_perm_done && cycles <= 32,
              "WOTS shared permutation completes within 32 cycles");
        check(wots_state_out[63:0] == zero_permutation_lane(0),
              "WOTS service uses the common Keccak datapath");

        @(negedge clk);
        wots_abort = 1'b1;
        @(posedge clk);
        #1;
        @(negedge clk);
        wots_abort = 1'b0;
        cycles = 0;
        while (wots_owned && cycles < 70) begin
            @(posedge clk);
            #1;
            cycles = cycles + 1;
        end
        check(!wots_owned && cycles <= 64,
              "WOTS abort wipes and releases within 64 cycles");

        // Reclaiming exposes a zeroized resident state before new work.
        @(negedge clk);
        wots_claim = 1'b1;
        @(posedge clk);
        #1;
        check(wots_grant && wots_state_out == 1600'd0,
              "WOTS release boundary exposes zeroized service state");
        @(negedge clk);
        wots_claim = 1'b0;
        wots_release = 1'b1;
        @(posedge clk);
        #1;
        @(negedge clk);
        wots_release = 1'b0;
        cycles = 0;
        while (wots_owned && cycles < 70) begin
            @(posedge clk);
            #1;
            cycles = cycles + 1;
        end
        check(!wots_owned, "WOTS normal release returns owner none");

        $display("------------------------------------------------------------");
        $display("SHA3/Keccak contract: %0d passed, %0d failed",
                 pass_count, fail_count);
        if (fail_count != 0)
            $fatal(1, "SHA3/Keccak contract failures");
        $finish;
    end

    initial begin
        #2_000_000;
        $fatal(1, "tb_sha3_keccak timeout");
    end

endmodule
