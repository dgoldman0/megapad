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

`include "mp64_pkg.vh"

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
    reg [7:0]  status;        // bit0=tx_busy, bit1=rx_avail, bit2=link,
                              // bit3=error (sticky until RESET),
                              // bit4=rx_dma_busy, bit7=present
    reg [63:0] dma_base;
    reg [15:0] frame_len;
    reg [7:0]  irq_ctrl;      // bit0=rx_irq_en, bit1=tx_irq_en
    reg [7:0]  irq_status;    // bit0=rx_irq_pend, bit1=tx_irq_pend
    reg [15:0] tx_count;
    reg [15:0] rx_count;

    wire cmd_send  = req && wen && (addr[6:0] == 7'h00) && (wdata == 8'h01);
    wire cmd_recv  = req && wen && (addr[6:0] == 7'h00) && (wdata == 8'h02);
    wire cmd_reset = req && wen && (addr[6:0] == 7'h00) && (wdata == 8'h04);

    // One byte-DMA request may be outstanding at a time.  Ownership is
    // registered per beat so an RX arrival or command cannot withdraw or
    // rewrite a stalled TX request (and vice versa) before acknowledgement.
    localparam [1:0] DMA_NONE = 2'd0;
    localparam [1:0] DMA_RX   = 2'd1;
    localparam [1:0] DMA_TX   = 2'd2;
    reg [1:0] dma_owner;
    wire rx_dma_granted = (dma_owner == DMA_RX);
    wire tx_dma_granted = (dma_owner == DMA_TX);

    // MAC address (hardcoded: 02:4D:50:36:34:00)
    wire [47:0] mac_addr = 48'h024D50363400;

    // Address-indexed diagnostic window (not a FIFO or frame transport)
    reg [7:0] data_window [0:95];
    reg [95:0] data_window_valid;

    // RX FIFO: simple single-frame buffer
    localparam [10:0] MAX_FRAME_LEN = 11'd1514;
    reg [7:0]  rx_buf [0:1513];  // 1500-byte L3 MTU + 14-byte Ethernet header
    reg [10:0] rx_len;
    reg        rx_frame_ready;
    reg        rx_overflow;
    reg        rx_error;

    // ========================================================================
    // Status register
    // ========================================================================
    reg tx_busy;
    reg tx_error;
    wire rx_dma_busy = (rx_state == RX_DMA);
    always @(*) begin
        status = 8'h80;                              // bit7=present
        status[2] = phy_link_up;                     // bit2=link
        status[3] = rx_error | tx_error;              // bit3=error
        status[4] = rx_dma_busy;                      // bit4=RX DMA in progress
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
    localparam RX_DROP    = 3'd4;

    reg [2:0]  rx_state;
    reg [10:0] rx_byte_cnt;
    reg [63:0] rx_dma_base;

    // Event wires let the register bank remain the sole procedural owner of
    // frame_len, counters, and interrupt state.
    reg  rx_frame_good;
    reg  rx_frame_bad;
    wire rx_dma_done    = (rx_state == RX_DMA) && rx_dma_granted && dma_ack &&
                          (rx_byte_cnt == rx_len - 11'd1);

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            rx_state       <= RX_IDLE;
            rx_frame_ready <= 1'b0;
            rx_len         <= 11'd0;
            rx_byte_cnt    <= 11'd0;
            rx_overflow    <= 1'b0;
            rx_error       <= 1'b0;
            rx_frame_good  <= 1'b0;
            rx_frame_bad   <= 1'b0;
            rx_dma_base    <= 64'd0;
            phy_rx_ready   <= 1'b1;
        end else if (cmd_reset) begin
            // Abort an in-flight receive immediately.  If RESET arrives in
            // the middle of a PHY frame, consume its remaining bytes without
            // ever publishing the suffix as a new frame.
            rx_state       <= phy_rx_valid ? RX_DROP : RX_IDLE;
            rx_frame_ready <= 1'b0;
            rx_len         <= 11'd0;
            rx_byte_cnt    <= 11'd0;
            rx_overflow    <= 1'b0;
            rx_error       <= 1'b0;
            rx_frame_good  <= 1'b0;
            rx_frame_bad   <= 1'b0;
            rx_dma_base    <= 64'd0;
            phy_rx_ready   <= 1'b1;
        end else begin
            rx_frame_good <= 1'b0;
            rx_frame_bad  <= 1'b0;
            case (rx_state)
                RX_IDLE: begin
                    phy_rx_ready <= 1'b1;
                    if (phy_rx_valid) begin
                        rx_buf[0]   <= phy_rx_data;
                        rx_byte_cnt <= 11'd1;
                        rx_overflow <= 1'b0;
                        rx_state    <= RX_RECEIVE;
                    end
                end

                RX_RECEIVE: begin
                    if (phy_rx_valid) begin
                        if (rx_byte_cnt < MAX_FRAME_LEN) begin
                            rx_buf[rx_byte_cnt] <= phy_rx_data;
                            rx_byte_cnt <= rx_byte_cnt + 11'd1;
                        end else begin
                            // Consume the rest of an oversized PHY frame but
                            // never publish a silently truncated prefix.
                            rx_overflow <= 1'b1;
                        end
                    end else if (!phy_rx_valid) begin
                        // End of frame
                        if (rx_overflow) begin
                            rx_len         <= 11'd0;
                            rx_frame_ready <= 1'b0;
                            rx_error       <= 1'b1;
                            rx_frame_bad   <= 1'b1;
                            phy_rx_ready   <= 1'b1;
                            rx_state       <= RX_IDLE;
                        end else begin
                            rx_len         <= rx_byte_cnt;
                            rx_frame_ready <= 1'b1;
                            rx_frame_good  <= 1'b1;
                            phy_rx_ready   <= 1'b0;  // back-pressure until consumed
                            rx_state       <= RX_DONE;
                        end
                    end
                end

                RX_DONE: begin
                    // Wait for CMD=RECV to initiate DMA transfer to RAM
                    if (cmd_recv) begin
                        rx_byte_cnt <= 11'd0;
                        rx_dma_base <= dma_base;
                        rx_state    <= RX_DMA;
                    end
                end

                RX_DMA: begin
                    // A duplicate RECV while this transaction is busy is
                    // ignored.  It cannot restart DMA or clear/publish the
                    // operation's eventual length.
                    if (rx_dma_granted && dma_ack) begin
                        if (rx_byte_cnt == rx_len - 1) begin
                            rx_frame_ready <= 1'b0;
                            phy_rx_ready   <= 1'b1;
                            rx_state       <= RX_IDLE;
                        end else begin
                            rx_byte_cnt <= rx_byte_cnt + 1;
                        end
                    end
                end

                RX_DROP: begin
                    // RESET may interrupt a frame while VALID is asserted.
                    // Keep READY high to drain it, then re-arm at the gap.
                    phy_rx_ready <= 1'b1;
                    if (!phy_rx_valid)
                        rx_state <= RX_IDLE;
                end

                default: begin
                    rx_state       <= RX_IDLE;
                    rx_frame_ready <= 1'b0;
                    rx_len         <= 11'd0;
                    rx_byte_cnt    <= 11'd0;
                    rx_overflow    <= 1'b0;
                    rx_error       <= 1'b1;
                    phy_rx_ready   <= 1'b1;
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
    reg [7:0]  tx_buf [0:1513];  // maximum no-FCS Ethernet frame
    reg [10:0] tx_len;
    reg [63:0] tx_dma_base;

    wire tx_done_event = (tx_state == TX_DONE);

    // The registered beat owner drives the complete request tuple until ACK.
    // RX receives the next free beat when both engines are pending, but can
    // never preempt a TX beat that is already visible to the memory bus.
    always @(*) begin
        dma_req   = 1'b0;
        dma_addr  = 64'd0;
        dma_wdata = 8'd0;
        dma_wen   = 1'b0;

        if (rx_dma_granted) begin
            dma_req   = 1'b1;
            dma_addr  = rx_dma_base + {53'd0, rx_byte_cnt};
            dma_wdata = rx_buf[rx_byte_cnt];
            dma_wen   = 1'b1;
        end else if (tx_dma_granted) begin
            dma_req   = 1'b1;
            dma_addr  = tx_dma_base + {53'd0, tx_byte_cnt};
            dma_wen   = 1'b0;
        end
    end

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            dma_owner <= DMA_NONE;
        end else if (cmd_reset) begin
            dma_owner <= DMA_NONE;
        end else begin
            case (dma_owner)
                DMA_NONE: begin
                    if (rx_state == RX_DMA)
                        dma_owner <= DMA_RX;
                    else if (tx_state == TX_DMA_READ)
                        dma_owner <= DMA_TX;
                end
                DMA_RX: if (dma_ack)
                    dma_owner <= DMA_NONE;
                DMA_TX: if (dma_ack)
                    dma_owner <= DMA_NONE;
                default: dma_owner <= DMA_NONE;
            endcase
        end
    end

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            tx_state     <= TX_IDLE;
            tx_busy      <= 1'b0;
            tx_byte_cnt  <= 11'd0;
            phy_tx_valid <= 1'b0;
            phy_tx_data  <= 8'd0;
            tx_len       <= 11'd0;
            tx_dma_base  <= 64'd0;
            tx_error     <= 1'b0;
        end else if (cmd_reset) begin
            // RESET has priority over a simultaneous DMA/PHY acknowledgement,
            // so neither a cancelled transfer nor a stranded request can be
            // reported as complete.
            tx_state     <= TX_IDLE;
            tx_busy      <= 1'b0;
            tx_byte_cnt  <= 11'd0;
            phy_tx_valid <= 1'b0;
            phy_tx_data  <= 8'd0;
            tx_len       <= 11'd0;
            tx_dma_base  <= 64'd0;
            tx_error     <= 1'b0;
        end else begin
            // SEND is accepted only from IDLE.  A command while DMA or PHY
            // transmission is active leaves that operation intact and
            // latches the normal sticky command error.
            if (cmd_send && tx_state != TX_IDLE)
                tx_error <= 1'b1;
            case (tx_state)
                TX_IDLE: begin
                    phy_tx_valid <= 1'b0;
                    // CMD=SEND (0x01) triggers TX
                    if (cmd_send) begin
                        if (frame_len == 16'd0 || frame_len > 16'd1514) begin
                            tx_busy  <= 1'b0;
                            tx_error <= 1'b1;
                        end else begin
                            tx_busy     <= 1'b1;
                            tx_len      <= frame_len[10:0];
                            tx_dma_base <= dma_base;
                            tx_byte_cnt <= 11'd0;
                            tx_state    <= TX_DMA_READ;
                        end
                    end
                end

                TX_DMA_READ: begin
                    // DMA read from RAM into tx_buf
                    // NOTE: DMA is shared with RX. TX only advances when the
                    // combinational arbiter above grants it and memory ACKs.
                    if (tx_dma_granted && dma_ack) begin
                        tx_buf[tx_byte_cnt] <= dma_rdata;
                        if (tx_byte_cnt == tx_len - 11'd1) begin
                            tx_byte_cnt <= 11'd0;
                            tx_state    <= TX_SEND;
                        end else begin
                            tx_byte_cnt <= tx_byte_cnt + 11'd1;
                        end
                    end
                end

                TX_SEND: begin
                    // Present a byte, then advance only after that registered
                    // VALID/data pair is accepted.  Both remain stable under
                    // back-pressure, including for the final byte.
                    if (!phy_tx_valid) begin
                        phy_tx_valid <= 1'b1;
                        phy_tx_data  <= tx_buf[tx_byte_cnt];
                    end else if (phy_tx_ready) begin
                        if (tx_byte_cnt == tx_len - 11'd1) begin
                            phy_tx_valid <= 1'b0;
                            tx_state     <= TX_DONE;
                        end else begin
                            tx_byte_cnt <= tx_byte_cnt + 11'd1;
                            phy_tx_data <= tx_buf[tx_byte_cnt + 11'd1];
                        end
                    end
                end

                TX_DONE: begin
                    phy_tx_valid  <= 1'b0;
                    tx_busy       <= 1'b0;
                    tx_state      <= TX_IDLE;
                end


                default: begin
                    tx_state     <= TX_IDLE;
                    tx_busy      <= 1'b0;
                    phy_tx_valid <= 1'b0;
                end
            endcase
        end
    end

    // ========================================================================
    // Register read/write
    // ========================================================================
    // Register bank and event accounting.  No state-machine block writes any
    // of these registers, avoiding the unsynthesizable multi-driver ownership
    // that previously existed between RX, TX, and MMIO logic.
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            dma_base   <= 64'd0;
            frame_len  <= 16'd0;
            irq_ctrl   <= 8'd0;
            irq_status <= 8'd0;
            tx_count   <= 16'd0;
            rx_count   <= 16'd0;
            data_window_valid <= 96'd0;
        end else if (cmd_reset) begin
            // Match native RESET: preserve programmed DMA base and IRQ
            // enables, but invalidate all buffered/observable transfer state.
            frame_len  <= 16'd0;
            irq_status <= 8'd0;
            tx_count   <= 16'd0;
            rx_count   <= 16'd0;
            data_window_valid <= 96'd0;
        end else begin
            if (req && wen) begin
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
                    // IRQ status (W1C).  Same-cycle device events below win.
                    7'h0D: irq_status <= irq_status & ~wdata;
                    // Data window (offset 0x20-0x7F)
                    default: if (addr[6:0] >= 7'h20) begin
                        data_window[addr[6:0] - 7'h20] <= wdata;
                        data_window_valid[addr[6:0] - 7'h20] <= 1'b1;
                    end
                endcase
            end

            if (rx_frame_good) begin
                rx_count      <= rx_count + 16'd1;
                irq_status[0] <= 1'b1;
            end
            if (tx_done_event) begin
                tx_count      <= tx_count + 16'd1;
                irq_status[1] <= 1'b1;
            end

            // A rejected frame publishes no prefix.  A successful receive
            // publishes its length only on the final acknowledged RAM write.
            // RECV with no published frame has the native deterministic
            // result (length zero).  A duplicate during RX_DMA is ignored so
            // the in-flight transaction remains the sole publisher.
            if (cmd_recv && rx_state != RX_DONE && rx_state != RX_DMA)
                frame_len <= 16'd0;
            if (rx_frame_bad)
                frame_len <= 16'd0;
            if (rx_dma_done)
                frame_len <= {5'd0, rx_len};
        end
    end

    always @(posedge clk) begin
        ack <= 1'b0;
        if (req) begin
            ack <= 1'b1;
            case (addr[6:0])
                7'h00: rdata <= 8'd0;  // CMD is write-only
                7'h01: rdata <= status;
                7'h02: rdata <= dma_base[ 7: 0];
                7'h03: rdata <= dma_base[15: 8];
                7'h04: rdata <= dma_base[23:16];
                7'h05: rdata <= dma_base[31:24];
                7'h06: rdata <= dma_base[39:32];
                7'h07: rdata <= dma_base[47:40];
                7'h08: rdata <= dma_base[55:48];
                7'h09: rdata <= dma_base[63:56];
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
                default: if (addr[6:0] >= 7'h20)
                    rdata <= data_window_valid[addr[6:0] - 7'h20]
                           ? data_window[addr[6:0] - 7'h20] : 8'd0;
                else
                    rdata <= 8'd0;
            endcase
        end
    end

endmodule
