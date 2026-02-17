// ============================================================================
// mp64_rst_sync.v — Reset Synchroniser (Portable)
// ============================================================================
//
// Converts an asynchronous active-low reset into a synchronous active-low
// reset aligned to the rising edge of clk.
//
// Architecture:
//   - Async assertion (rst_n_in falling immediately drives chain low)
//   - Synchronous de-assertion through SYNC_STAGES flip-flops
//   - Output rst_n_out is safe to use as a synchronous reset
//
// Parameters:
//   - SYNC_STAGES: number of synchroniser flip-flops (default 4)
//
// Coding standard: Verilog-2001.
// NOTE: This is the ONE module that legitimately uses async reset in
// its sensitivity list — the synchroniser chain must respond immediately
// to the assertion of the external reset.
//

module mp64_rst_sync #(
    parameter SYNC_STAGES = 4
)(
    input  wire clk,
    input  wire rst_n_in,   // async active-low reset input
    output wire rst_n_out   // sync active-low reset output
);

    // ========================================================================
    // Synchroniser shift register
    // ========================================================================
    reg [SYNC_STAGES-1:0] sync_r;

    // Async assert, sync de-assert
    always @(posedge clk or negedge rst_n_in) begin
        if (!rst_n_in)
            sync_r <= {SYNC_STAGES{1'b0}};
        else
            sync_r <= {sync_r[SYNC_STAGES-2:0], 1'b1};
    end

    assign rst_n_out = sync_r[SYNC_STAGES-1];

endmodule
