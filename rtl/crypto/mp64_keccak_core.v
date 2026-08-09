// ============================================================================
// mp64_keccak_core.v -- reusable Keccak-f[1600] round service
// ============================================================================
//
// This module is the only Keccak round datapath in the portable RTL.  It owns
// the 25 little-endian 64-bit lanes and applies one round per clock for 24
// clocks.  Policy (SHA3 padding, squeezing, MMIO ownership, and WOTS
// arbitration) deliberately lives in the front ends.
//
// Lane i occupies state_{in,out}[64*i +: 64], where i = x + 5*y.  An idle
// caller may update one lane with byte strobes, start the resident state, or
// atomically load and start a complete state.  clear is accepted in every
// phase, cancels at the current round boundary, and wipes all lanes.

module mp64_keccak_core (
    input  wire          clk,
    input  wire          rst_n,

    input  wire          start,
    input  wire          load_start,
    input  wire [1599:0] state_in,

    input  wire          lane_we,
    input  wire [4:0]    lane_index,
    input  wire [63:0]   lane_wdata,
    input  wire [7:0]    lane_wstrb,
    output wire [63:0]   lane_rdata,
    output wire [1599:0] state_out,

    input  wire          clear,
    output reg           clear_done,
    output reg           busy,
    output reg           done
);

    reg [63:0] lanes [0:24];
    reg [4:0]  round_count;

    reg [63:0] column [0:4];
    reg [63:0] theta_d [0:4];
    reg [63:0] rho_pi [0:24];
    reg [63:0] next_lane [0:24];

    integer x;
    integer y;
    integer comb_lane;
    integer seq_lane;
    integer byte_no;

    function [63:0] round_constant;
        input [4:0] round_no;
        begin
            case (round_no)
                5'd0:  round_constant = 64'h0000_0000_0000_0001;
                5'd1:  round_constant = 64'h0000_0000_0000_8082;
                5'd2:  round_constant = 64'h8000_0000_0000_808a;
                5'd3:  round_constant = 64'h8000_0000_8000_8000;
                5'd4:  round_constant = 64'h0000_0000_0000_808b;
                5'd5:  round_constant = 64'h0000_0000_8000_0001;
                5'd6:  round_constant = 64'h8000_0000_8000_8081;
                5'd7:  round_constant = 64'h8000_0000_0000_8009;
                5'd8:  round_constant = 64'h0000_0000_0000_008a;
                5'd9:  round_constant = 64'h0000_0000_0000_0088;
                5'd10: round_constant = 64'h0000_0000_8000_8009;
                5'd11: round_constant = 64'h0000_0000_8000_000a;
                5'd12: round_constant = 64'h0000_0000_8000_808b;
                5'd13: round_constant = 64'h8000_0000_0000_008b;
                5'd14: round_constant = 64'h8000_0000_0000_8089;
                5'd15: round_constant = 64'h8000_0000_0000_8003;
                5'd16: round_constant = 64'h8000_0000_0000_8002;
                5'd17: round_constant = 64'h8000_0000_0000_0080;
                5'd18: round_constant = 64'h0000_0000_0000_800a;
                5'd19: round_constant = 64'h8000_0000_8000_000a;
                5'd20: round_constant = 64'h8000_0000_8000_8081;
                5'd21: round_constant = 64'h8000_0000_0000_8080;
                5'd22: round_constant = 64'h0000_0000_8000_0001;
                5'd23: round_constant = 64'h8000_0000_8000_8008;
                default: round_constant = 64'd0;
            endcase
        end
    endfunction

    function [5:0] rotation_offset;
        input [4:0] lane_no;
        begin
            case (lane_no)
                5'd0:  rotation_offset = 6'd0;
                5'd1:  rotation_offset = 6'd1;
                5'd2:  rotation_offset = 6'd62;
                5'd3:  rotation_offset = 6'd28;
                5'd4:  rotation_offset = 6'd27;
                5'd5:  rotation_offset = 6'd36;
                5'd6:  rotation_offset = 6'd44;
                5'd7:  rotation_offset = 6'd6;
                5'd8:  rotation_offset = 6'd55;
                5'd9:  rotation_offset = 6'd20;
                5'd10: rotation_offset = 6'd3;
                5'd11: rotation_offset = 6'd10;
                5'd12: rotation_offset = 6'd43;
                5'd13: rotation_offset = 6'd25;
                5'd14: rotation_offset = 6'd39;
                5'd15: rotation_offset = 6'd41;
                5'd16: rotation_offset = 6'd45;
                5'd17: rotation_offset = 6'd15;
                5'd18: rotation_offset = 6'd21;
                5'd19: rotation_offset = 6'd8;
                5'd20: rotation_offset = 6'd18;
                5'd21: rotation_offset = 6'd2;
                5'd22: rotation_offset = 6'd61;
                5'd23: rotation_offset = 6'd56;
                5'd24: rotation_offset = 6'd14;
                default: rotation_offset = 6'd0;
            endcase
        end
    endfunction

    function [63:0] rotate_left;
        input [63:0] value;
        input [5:0] amount;
        begin
            if (amount == 0)
                rotate_left = value;
            else
                rotate_left = (value << amount) |
                              (value >> (7'd64 - {1'b0, amount}));
        end
    endfunction

    always @(*) begin
        for (x = 0; x < 5; x = x + 1)
            column[x] = lanes[x] ^ lanes[x+5] ^ lanes[x+10] ^
                        lanes[x+15] ^ lanes[x+20];

        for (x = 0; x < 5; x = x + 1)
            theta_d[x] = column[(x+4)%5] ^
                         rotate_left(column[(x+1)%5], 6'd1);

        for (y = 0; y < 5; y = y + 1)
            for (x = 0; x < 5; x = x + 1) begin
                comb_lane = x + 5*y;
                rho_pi[y + 5*((2*x + 3*y) % 5)] =
                    rotate_left(lanes[comb_lane] ^ theta_d[x],
                                rotation_offset(comb_lane[4:0]));
            end

        for (y = 0; y < 5; y = y + 1)
            for (x = 0; x < 5; x = x + 1)
                next_lane[x + 5*y] = rho_pi[x + 5*y] ^
                    (~rho_pi[((x+1)%5) + 5*y] &
                      rho_pi[((x+2)%5) + 5*y]);

        next_lane[0] = next_lane[0] ^ round_constant(round_count);
    end

    genvar gi;
    generate
        for (gi = 0; gi < 25; gi = gi + 1) begin : g_state_flatten
            assign state_out[gi*64 +: 64] = lanes[gi];
        end
    endgenerate

    assign lane_rdata = (lane_index < 5'd25) ? lanes[lane_index] : 64'd0;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            busy        <= 1'b0;
            done        <= 1'b0;
            clear_done  <= 1'b0;
            round_count <= 5'd0;
            for (seq_lane = 0; seq_lane < 25; seq_lane = seq_lane + 1)
                lanes[seq_lane] <= 64'd0;
        end else begin
            done       <= 1'b0;
            clear_done <= 1'b0;

            // clear has highest priority.  A clock edge is a complete Keccak
            // round boundary, so no partially evaluated round is published.
            if (clear) begin
                busy        <= 1'b0;
                round_count <= 5'd0;
                clear_done  <= 1'b1;
                for (seq_lane = 0; seq_lane < 25;
                     seq_lane = seq_lane + 1)
                    lanes[seq_lane] <= 64'd0;
            end else if (busy) begin
                for (seq_lane = 0; seq_lane < 25;
                     seq_lane = seq_lane + 1)
                    lanes[seq_lane] <= next_lane[seq_lane];

                if (round_count == 5'd23) begin
                    busy        <= 1'b0;
                    round_count <= 5'd0;
                    done        <= 1'b1;
                end else begin
                    round_count <= round_count + 5'd1;
                end
            end else if (load_start) begin
                for (seq_lane = 0; seq_lane < 25;
                     seq_lane = seq_lane + 1)
                    lanes[seq_lane] <= state_in[seq_lane*64 +: 64];
                busy        <= 1'b1;
                round_count <= 5'd0;
            end else if (start) begin
                busy        <= 1'b1;
                round_count <= 5'd0;
            end else if (lane_we && lane_index < 5'd25) begin
                for (byte_no = 0; byte_no < 8; byte_no = byte_no + 1)
                    if (lane_wstrb[byte_no])
                        lanes[lane_index][byte_no*8 +: 8] <=
                            lane_wdata[byte_no*8 +: 8];
            end
        end
    end

endmodule
