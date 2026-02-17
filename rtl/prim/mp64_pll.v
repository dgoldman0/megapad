// ============================================================================
// mp64_pll.v — Portable PLL / Clock Generator Wrapper
// ============================================================================
//
// Behavioural model of a PLL that generates a system clock from a
// reference input.  In simulation the output is simply a divided/
// multiplied version of clk_in.  Platform overrides should instantiate
// vendor MMCM / PLL macros with the same port interface.
//
// Parameters:
//   CLK_MUL  — feedback multiplier  (MMCME2_BASE CLKFBOUT_MULT_F equiv)
//   CLK_DIV  — output divider       (CLKOUT0_DIVIDE equiv)
//   REF_MHZ  — reference frequency (informational, for DRC checks)
//
// Coding standard:
//   Verilog-2001, synchronous, non-blocking assigns.
//

module mp64_pll #(
    parameter CLK_MUL = 5,         // multiplication factor
    parameter CLK_DIV = 10,        // division factor
    parameter REF_MHZ = 200        // reference clock MHz (informational)
)(
    input  wire  clk_in,           // reference clock
    input  wire  rst_in,           // active-high reset
    output wire  clk_out,          // generated system clock
    output reg   locked            // PLL lock indicator
);

    // ========================================================================
    // Behavioural simulation model
    // ========================================================================
    // In simulation, just pass the clock through and assert locked after
    // a few reference cycles (modelling PLL lock time).

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

    // In behavioural sim, output clock = input clock (1:1).
    // Platform overrides synthesise the real frequency relationship.
    assign clk_out = clk_in;

endmodule
