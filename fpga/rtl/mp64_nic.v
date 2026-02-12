// ============================================================================
// mp64_nic.v — Network Interface Controller
// ============================================================================
//
// Simple frame-based NIC for external data ingestion.  Matches the MMIO
// register layout expected by the BIOS (NET-STATUS, NET-RECV, NET-SEND).
//
// In the FPGA prototype, this can be connected to:
//   - An RMII PHY (10/100 Mbps Ethernet)
//   - A SPI-based Ethernet module (ENC28J60, W5500)
//   - A UART-bridge for testing (frames sent as length-prefixed packets)
//
// This module implements the register interface and DMA engine.
// The actual PHY interface is a separate wrapper module.
//

`include "mp64_defs.vh"

module mp64_nic (
    input  wire        clk,
    input  wire        rst_n,

    // === MMIO register interface ===
    input  wire        req,
    input  wire [6:0]  addr,       // 7-bit offset (0x00-0x7F)
    input  wire [7:0]  wdata,
    input  wire        wen,
    output reg  [7:0]  rdata,
    output reg         ack,

    // === Interrupt output ===
    output wire        irq,

    // === DMA interface (to memory bus) ===
    output reg         dma_req,
    output reg  [63:0] dma_addr,
    output reg  [7:0]  dma_wdata,
    output reg         dma_wen,
    input  wire [7:0]  dma_rdata,
    input  wire        dma_ack,

    // === PHY interface (directly to external module) ===
    output reg         phy_tx_valid,
    output reg  [7:0]  phy_tx_data,
    input  wire        phy_tx_ready,
    input  wire        phy_rx_valid,
    input  wire [7:0]  phy_rx_data,
    output reg         phy_rx_ready,
    input  wire        phy_link_up
);

    // ========================================================================
    // Registers
    // ========================================================================
    reg [7:0]  cmd;
    reg [7:0]  status;        // bit0=tx_busy, bit1=rx_avail, bit2=link, bit7=present
    reg [63:0] dma_base;
    reg [15:0] frame_len;
    reg [7:0]  irq_ctrl;      // bit0=rx_irq_en, bit1=tx_irq_en
    reg [7:0]  irq_status;    // bit0=rx_irq_pend, bit1=tx_irq_pend
    reg [15:0] tx_count;
    reg [15:0] rx_count;

    // MAC address (hardcoded: 02:4D:50:36:34:00)
    wire [47:0] mac_addr = 48'h024D50363400;

    // Data window: 96-byte register-accessible frame buffer
    reg [7:0] data_window [0:95];

    // RX FIFO: simple single-frame buffer
    reg [7:0]  rx_buf [0:1499];  // MTU = 1500
    reg [10:0] rx_len;
    reg        rx_frame_ready;

    // ========================================================================
    // Status register
    // ========================================================================
    reg tx_busy;
    always @(*) begin
        status = 8'h80;                              // bit7=present
        status[2] = phy_link_up;                     // bit2=link
        status[1] = rx_frame_ready;                  // bit1=rx_avail
        status[0] = tx_busy;                         // bit0=tx_busy
    end

    // ========================================================================
    // Interrupt logic
    // ========================================================================
    assign irq = (irq_ctrl[0] & irq_status[0]) | (irq_ctrl[1] & irq_status[1]);

    // ========================================================================
    // RX path: PHY → rx_buf → DMA → RAM
    // ========================================================================
    localparam RX_IDLE    = 3'd0;
    localparam RX_RECEIVE = 3'd1;
    localparam RX_DMA     = 3'd2;
    localparam RX_DONE    = 3'd3;

    reg [2:0]  rx_state;
    reg [10:0] rx_byte_cnt;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            rx_state       <= RX_IDLE;
            rx_frame_ready <= 1'b0;
            rx_len         <= 11'd0;
            rx_count       <= 16'd0;
            phy_rx_ready   <= 1'b1;
        end else begin
            case (rx_state)
                RX_IDLE: begin
                    phy_rx_ready <= 1'b1;
                    if (phy_rx_valid) begin
                        rx_buf[0]   <= phy_rx_data;
                        rx_byte_cnt <= 11'd1;
                        rx_state    <= RX_RECEIVE;
                    end
                end

                RX_RECEIVE: begin
                    if (phy_rx_valid && rx_byte_cnt < 11'd1500) begin
                        rx_buf[rx_byte_cnt] <= phy_rx_data;
                        rx_byte_cnt <= rx_byte_cnt + 1;
                    end else if (!phy_rx_valid) begin
                        // End of frame
                        rx_len         <= rx_byte_cnt;
                        rx_frame_ready <= 1'b1;
                        rx_count       <= rx_count + 1;
                        irq_status[0]  <= 1'b1;
                        phy_rx_ready   <= 1'b0;  // back-pressure until consumed
                        rx_state       <= RX_DONE;
                    end
                end

                RX_DONE: begin
                    // Wait for CMD=RECV to initiate DMA transfer to RAM
                    if (req && wen && addr[6:0] == NIC_CMD && wdata == 8'h02) begin
                        rx_byte_cnt <= 11'd0;
                        rx_state    <= RX_DMA;
                    end
                end

                RX_DMA: begin
                    dma_req   <= 1'b1;
                    dma_addr  <= dma_base + {53'd0, rx_byte_cnt};
                    dma_wdata <= rx_buf[rx_byte_cnt];
                    dma_wen   <= 1'b1;
                    if (dma_ack) begin
                        dma_req <= 1'b0;
                        if (rx_byte_cnt == rx_len - 1) begin
                            frame_len      <= {5'd0, rx_len};
                            rx_frame_ready <= 1'b0;
                            rx_state       <= RX_IDLE;
                        end else begin
                            rx_byte_cnt <= rx_byte_cnt + 1;
                        end
                    end
                end
            endcase
        end
    end

    // ========================================================================
    // TX path: RAM → DMA → tx_buf → PHY
    // ========================================================================
    localparam TX_IDLE     = 3'd0;
    localparam TX_DMA_READ = 3'd1;
    localparam TX_SEND     = 3'd2;
    localparam TX_DONE     = 3'd3;

    reg [2:0]  tx_state;
    reg [10:0] tx_byte_cnt;
    reg [7:0]  tx_buf [0:1499];  // MTU = 1500
    reg [10:0] tx_len;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            tx_state     <= TX_IDLE;
            tx_busy      <= 1'b0;
            tx_count     <= 16'd0;
            phy_tx_valid <= 1'b0;
            phy_tx_data  <= 8'd0;
            tx_len       <= 11'd0;
        end else begin
            case (tx_state)
                TX_IDLE: begin
                    phy_tx_valid <= 1'b0;
                    // CMD=SEND (0x01) triggers TX
                    if (req && wen && addr[6:0] == NIC_CMD && wdata == 8'h01) begin
                        tx_busy     <= 1'b1;
                        tx_len      <= frame_len[10:0];
                        tx_byte_cnt <= 11'd0;
                        tx_state    <= TX_DMA_READ;
                    end
                end

                TX_DMA_READ: begin
                    // DMA read from RAM into tx_buf
                    // NOTE: DMA is shared with RX. TX only runs when RX is idle.
                    if (rx_state == RX_IDLE || rx_state == RX_DONE) begin
                        dma_req  <= 1'b1;
                        dma_addr <= dma_base + {53'd0, tx_byte_cnt};
                        dma_wen  <= 1'b0;  // read
                        if (dma_ack) begin
                            tx_buf[tx_byte_cnt] <= dma_rdata;
                            dma_req <= 1'b0;
                            if (tx_byte_cnt == tx_len - 11'd1) begin
                                tx_byte_cnt <= 11'd0;
                                tx_state    <= TX_SEND;
                            end else begin
                                tx_byte_cnt <= tx_byte_cnt + 11'd1;
                            end
                        end
                    end
                end

                TX_SEND: begin
                    // Stream bytes to PHY
                    if (phy_tx_ready) begin
                        phy_tx_valid <= 1'b1;
                        phy_tx_data  <= tx_buf[tx_byte_cnt];
                        if (tx_byte_cnt == tx_len - 11'd1) begin
                            tx_state <= TX_DONE;
                        end else begin
                            tx_byte_cnt <= tx_byte_cnt + 11'd1;
                        end
                    end else begin
                        phy_tx_valid <= 1'b0;
                    end
                end

                TX_DONE: begin
                    phy_tx_valid  <= 1'b0;
                    tx_busy       <= 1'b0;
                    tx_count      <= tx_count + 16'd1;
                    irq_status[1] <= 1'b1;
                    tx_state      <= TX_IDLE;
                end
            endcase
        end
    end

    // ========================================================================
    // Register read/write
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            dma_base   <= 64'd0;
            frame_len  <= 16'd0;
            irq_ctrl   <= 8'd0;
            irq_status <= 8'd0;
        end else if (req && wen) begin
            case (addr[6:0])
                // DMA address (64-bit LE, offset +0x02..+0x09)
                7'h02: dma_base[ 7: 0] <= wdata;
                7'h03: dma_base[15: 8] <= wdata;
                7'h04: dma_base[23:16] <= wdata;
                7'h05: dma_base[31:24] <= wdata;
                7'h06: dma_base[39:32] <= wdata;
                7'h07: dma_base[47:40] <= wdata;
                7'h08: dma_base[55:48] <= wdata;
                7'h09: dma_base[63:56] <= wdata;
                // Frame length (16-bit LE)
                7'h0A: frame_len[ 7:0] <= wdata;
                7'h0B: frame_len[15:8] <= wdata;
                // IRQ control
                7'h0C: irq_ctrl <= wdata;
                // IRQ status (W1C)
                7'h0D: irq_status <= irq_status & ~wdata;
                // Data window (offset 0x20-0x7F)
                default: if (addr[6:5] == 2'b01)
                    data_window[addr[5:0]] <= wdata;
            endcase
        end
    end

    always @(posedge clk) begin
        ack <= 1'b0;
        if (req) begin
            ack <= 1'b1;
            case (addr[6:0])
                7'h00: rdata <= cmd;
                7'h01: rdata <= status;
                7'h0A: rdata <= frame_len[7:0];
                7'h0B: rdata <= frame_len[15:8];
                7'h0C: rdata <= irq_ctrl;
                7'h0D: rdata <= irq_status;
                // MAC address (6 bytes at offset 0x0E-0x13)
                7'h0E: rdata <= mac_addr[47:40];
                7'h0F: rdata <= mac_addr[39:32];
                7'h10: rdata <= mac_addr[31:24];
                7'h11: rdata <= mac_addr[23:16];
                7'h12: rdata <= mac_addr[15: 8];
                7'h13: rdata <= mac_addr[ 7: 0];
                // TX/RX counters
                7'h14: rdata <= tx_count[7:0];
                7'h15: rdata <= tx_count[15:8];
                7'h16: rdata <= rx_count[7:0];
                7'h17: rdata <= rx_count[15:8];
                // Data window
                default: if (addr[6:5] == 2'b01)
                    rdata <= data_window[addr[5:0]];
                else
                    rdata <= 8'd0;
            endcase
        end
    end

endmodule
