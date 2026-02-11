// ============================================================================
// mp64_timer.v — Timer Peripheral
// ============================================================================
//
// 32-bit free-running counter with compare-match interrupt.
// Used by KDOS for scheduler preemption (TIME-SLICE = 50,000 cycles).
//
// Features:
//   - 32-bit counter incrementing every clock cycle
//   - 32-bit compare register for match detection
//   - Auto-reload: counter resets to 0 on match when enabled
//   - Compare-match interrupt with acknowledge (W1C)
//

`include "mp64_defs.vh"

module mp64_timer (
    input  wire        clk,
    input  wire        rst_n,

    // === MMIO register interface ===
    input  wire        req,
    input  wire [3:0]  addr,       // byte offset within timer block
    input  wire [7:0]  wdata,
    input  wire        wen,
    output reg  [7:0]  rdata,
    output reg         ack,

    // === Interrupt output ===
    output wire        irq
);

    // ========================================================================
    // Registers
    // ========================================================================
    reg [31:0] counter;     // free-running 32-bit counter
    reg [31:0] compare;     // compare-match value
    reg [7:0]  control;     // bit0=enable, bit1=irq_enable, bit2=auto_reload
    reg        match_flag;  // set on compare match

    wire enabled     = control[0];
    wire irq_enabled = control[1];
    wire auto_reload = control[2];

    // ========================================================================
    // Counter logic
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            counter    <= 32'd0;
            match_flag <= 1'b0;
        end else begin
            if (enabled) begin
                if (counter == compare && compare != 32'd0) begin
                    match_flag <= 1'b1;
                    if (auto_reload)
                        counter <= 32'd0;
                    else
                        counter <= counter + 1;
                end else begin
                    counter <= counter + 1;
                end
            end

            // W1C: clear match flag on write to STATUS
            if (req && wen && addr == TIMER_STATUS) begin
                if (wdata[0])
                    match_flag <= 1'b0;
            end
        end
    end

    // ========================================================================
    // Control register write
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            control <= 8'd0;
            compare <= 32'd0;
        end else if (req && wen) begin
            case (addr)
                TIMER_CTRL: control <= wdata;
                // Compare register: byte-addressed writes (LE)
                TIMER_CMP + 0: compare[ 7: 0] <= wdata;
                TIMER_CMP + 1: compare[15: 8] <= wdata;
                TIMER_CMP + 2: compare[23:16] <= wdata;
                TIMER_CMP + 3: compare[31:24] <= wdata;
            endcase
        end
    end

    // ========================================================================
    // Interrupt
    // ========================================================================
    assign irq = match_flag & irq_enabled;

    // ========================================================================
    // Register read mux
    // ========================================================================
    always @(posedge clk) begin
        ack <= 1'b0;
        if (req) begin
            ack <= 1'b1;
            case (addr)
                TIMER_COUNT + 0: rdata <= counter[ 7: 0];
                TIMER_COUNT + 1: rdata <= counter[15: 8];
                TIMER_COUNT + 2: rdata <= counter[23:16];
                TIMER_COUNT + 3: rdata <= counter[31:24];
                TIMER_CMP + 0:   rdata <= compare[ 7: 0];
                TIMER_CMP + 1:   rdata <= compare[15: 8];
                TIMER_CMP + 2:   rdata <= compare[23:16];
                TIMER_CMP + 3:   rdata <= compare[31:24];
                TIMER_CTRL:      rdata <= control;
                TIMER_STATUS:    rdata <= {7'd0, match_flag};
                default:         rdata <= 8'd0;
            endcase
        end
    end

endmodule
