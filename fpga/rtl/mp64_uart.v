// ============================================================================
// mp64_uart.v — UART Peripheral
// ============================================================================
//
// 8N1 UART with configurable baud rate.  Matches the MMIO register layout
// expected by the BIOS (EMIT, KEY, KEY?).
//
// Features:
//   - 16-byte TX and RX FIFOs
//   - Configurable baud rate (default 115200 at 100 MHz)
//   - Status register with TX ready, RX available, TX empty flags
//   - Optional RX/TX interrupt generation
//

`include "mp64_defs.vh"

module mp64_uart #(
    parameter CLK_FREQ  = 100_000_000,
    parameter BAUD_RATE = 115200
) (
    input  wire        clk,
    input  wire        rst_n,

    // === MMIO register interface ===
    input  wire        req,
    input  wire [3:0]  addr,       // byte offset within UART block
    input  wire [7:0]  wdata,
    input  wire        wen,
    output reg  [7:0]  rdata,
    output reg         ack,

    // === Interrupt output ===
    output wire        irq,

    // === Physical pins ===
    output wire        tx,
    input  wire        rx
);

    // ========================================================================
    // Baud rate generator
    // ========================================================================
    localparam DIVISOR  = CLK_FREQ / BAUD_RATE;
    localparam DIV_BITS = $clog2(DIVISOR > 1 ? DIVISOR : 2);  // minimum 1 bit

    reg [DIV_BITS-1:0] tx_div, rx_div;
    wire tx_tick = (tx_div == 0);
    wire rx_tick = (rx_div == 0);

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            tx_div <= 0;
            rx_div <= 0;
        end else begin
            tx_div <= (tx_div == DIVISOR-1) ? 0 : tx_div + 1;
            rx_div <= (rx_div == DIVISOR-1) ? 0 : rx_div + 1;
        end
    end

    // ========================================================================
    // TX path — shift register + FIFO
    // ========================================================================
    // TX FIFO: 16 × 8-bit
    reg [7:0]  tx_fifo [0:15];
    reg [3:0]  tx_wr_ptr, tx_rd_ptr;
    reg [4:0]  tx_count;
    wire       tx_full  = (tx_count == 5'd16);
    wire       tx_empty = (tx_count == 5'd0);

    // TX shift register
    reg [9:0]  tx_shift;    // start bit + 8 data + stop bit
    reg [3:0]  tx_bit_cnt;
    reg        tx_active;

    assign tx = tx_active ? tx_shift[0] : 1'b1;  // idle high

    integer tx_rst_i;
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            tx_wr_ptr  <= 4'd0;
            tx_rd_ptr  <= 4'd0;
            tx_count   <= 5'd0;
            tx_active  <= 1'b0;
            tx_shift   <= 10'h3FF;
            tx_bit_cnt <= 4'd0;
            for (tx_rst_i = 0; tx_rst_i < 16; tx_rst_i = tx_rst_i + 1)
                tx_fifo[tx_rst_i] <= 8'd0;
        end else begin
            // FIFO write (from register interface)
            if (req && wen && addr == UART_TX && !tx_full) begin
                tx_fifo[tx_wr_ptr] <= wdata;
                tx_wr_ptr <= tx_wr_ptr + 1;
                tx_count  <= tx_count + 1;
            end

            // TX shift register
            if (tx_tick) begin
                if (tx_active) begin
                    tx_shift   <= {1'b1, tx_shift[9:1]};  // shift right
                    tx_bit_cnt <= tx_bit_cnt + 1;
                    if (tx_bit_cnt == 4'd9) begin
                        tx_active <= 1'b0;
                    end
                end else if (!tx_empty) begin
                    // Load next byte from FIFO
                    tx_shift   <= {1'b1, tx_fifo[tx_rd_ptr], 1'b0};  // stop, data, start
                    tx_bit_cnt <= 4'd0;
                    tx_active  <= 1'b1;
                    tx_rd_ptr  <= tx_rd_ptr + 1;
                    tx_count   <= tx_count - 1;
                end
            end
        end
    end

    // ========================================================================
    // RX path — shift register + FIFO
    // ========================================================================
    // RX FIFO: 16 × 8-bit
    reg [7:0]  rx_fifo [0:15];
    reg [3:0]  rx_wr_ptr, rx_rd_ptr;
    reg [4:0]  rx_count;
    wire       rx_avail = (rx_count != 5'd0);
    wire       rx_full  = (rx_count == 5'd16);

    // RX shift register
    reg [7:0]  rx_shift;
    reg [3:0]  rx_bit_cnt;
    reg        rx_active;
    reg [DIV_BITS-1:0] rx_sample_cnt;  // sample at mid-bit

    // Synchronize RX input (2-FF synchronizer)
    reg rx_sync1, rx_sync2;
    always @(posedge clk) begin
        rx_sync1 <= rx;
        rx_sync2 <= rx_sync1;
    end

    integer rx_rst_i;
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            rx_wr_ptr    <= 4'd0;
            rx_rd_ptr    <= 4'd0;
            rx_count     <= 5'd0;
            rx_active    <= 1'b0;
            rx_bit_cnt   <= 4'd0;
            rx_sample_cnt<= 0;
            for (rx_rst_i = 0; rx_rst_i < 16; rx_rst_i = rx_rst_i + 1)
                rx_fifo[rx_rst_i] <= 8'd0;
        end else begin
            if (!rx_active) begin
                // Wait for start bit (falling edge)
                if (!rx_sync2) begin
                    rx_active     <= 1'b1;
                    rx_bit_cnt    <= 4'd0;
                    rx_sample_cnt <= DIVISOR / 2;  // sample at center of bit
                end
            end else begin
                if (rx_sample_cnt == 0) begin
                    rx_sample_cnt <= DIVISOR - 1;
                    if (rx_bit_cnt == 4'd0) begin
                        // Verify start bit
                        if (rx_sync2) begin
                            rx_active <= 1'b0;  // false start
                        end else begin
                            rx_bit_cnt <= 4'd1;
                        end
                    end else if (rx_bit_cnt <= 4'd8) begin
                        // Sample data bit
                        rx_shift[rx_bit_cnt - 1] <= rx_sync2;
                        rx_bit_cnt <= rx_bit_cnt + 1;
                    end else begin
                        // Stop bit — push to FIFO
                        if (!rx_full) begin
                            rx_fifo[rx_wr_ptr] <= rx_shift;
                            rx_wr_ptr <= rx_wr_ptr + 1;
                            rx_count  <= rx_count + 1;
                        end
                        rx_active <= 1'b0;
                    end
                end else begin
                    rx_sample_cnt <= rx_sample_cnt - 1;
                end
            end

            // FIFO read (from register interface)
            if (req && !wen && addr == UART_RX && rx_avail) begin
                rx_rd_ptr <= rx_rd_ptr + 1;
                rx_count  <= rx_count - 1;
            end
        end
    end

    // ========================================================================
    // Control register
    // ========================================================================
    reg [7:0] control;  // bit0: RX IRQ enable, bit1: TX IRQ enable

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            control <= 8'd0;
        end else if (req && wen && addr == UART_CONTROL) begin
            control <= wdata;
        end
    end

    // ========================================================================
    // Status register
    // ========================================================================
    wire [7:0] status = {2'b0, tx_empty, 3'b0, rx_avail, !tx_full};
    //                    bit7:6=0, bit5=tx_empty, bit4:2=0, bit1=rx_avail, bit0=tx_ready

    // ========================================================================
    // Interrupt generation
    // ========================================================================
    assign irq = (control[0] & rx_avail) | (control[1] & tx_empty);

    // ========================================================================
    // Register read mux
    // ========================================================================
    always @(posedge clk) begin
        ack <= 1'b0;
        if (req) begin
            ack <= 1'b1;
            case (addr)
                UART_RX:      rdata <= rx_avail ? rx_fifo[rx_rd_ptr] : 8'd0;
                UART_STATUS:  rdata <= status;
                UART_CONTROL: rdata <= control;
                default:      rdata <= 8'd0;
            endcase
        end
    end

endmodule
