// ============================================================================
// mp64_pll_asic.v — ASIC PLL Stub
// ============================================================================
// Replace body with analog PLL macro from the foundry PDK.
// Port interface identical to rtl/prim/mp64_pll.v.
//

module mp64_pll #(
    parameter CLK_MUL = 5,
    parameter CLK_DIV = 10,
    parameter REF_MHZ = 200
)(
    input  wire  clk_in,
    input  wire  rst_in,
    output wire  clk_out,
    output reg   locked
);

    // STUB — behavioural passthrough (replace with analog PLL macro)
    reg [3:0] lock_cnt;

    always @(posedge clk_in or posedge rst_in) begin
        if (rst_in) begin
            lock_cnt <= 4'd0;
            locked   <= 1'b0;
        end else begin
            if (!locked) begin
                if (lock_cnt == 4'd10)
                    locked <= 1'b1;
                else
                    lock_cnt <= lock_cnt + 4'd1;
            end
        end
    end

    assign clk_out = clk_in;

endmodule
