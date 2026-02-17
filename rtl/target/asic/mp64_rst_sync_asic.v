// ============================================================================
// mp64_rst_sync_asic.v — ASIC Reset Synchronizer Stub
// ============================================================================
// The generic flip-flop chain is generally fine for ASIC.
// Replace if the PDK provides a dedicated reset synchronizer cell.
// Port interface identical to rtl/prim/mp64_rst_sync.v.
//

module mp64_rst_sync #(
    parameter STAGES = 3
)(
    input  wire clk,
    input  wire rst_async_n,    // asynchronous active-low reset
    output wire rst_sync_n      // synchronised active-low reset
);

    reg [STAGES-1:0] sync_chain;

    always @(posedge clk or negedge rst_async_n) begin
        if (!rst_async_n)
            sync_chain <= {STAGES{1'b0}};
        else
            sync_chain <= {sync_chain[STAGES-2:0], 1'b1};
    end

    assign rst_sync_n = sync_chain[STAGES-1];

endmodule
