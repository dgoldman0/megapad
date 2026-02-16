// ============================================================================
// tb_mp64_soc.v — Megapad-64 SoC Testbench
// ============================================================================
//
// Simulation testbench for mp64_soc:
//   - 100 MHz clock, active-low reset with 200ns hold
//   - BIOS ROM loaded from hex file into BRAM at time 0
//   - UART TX monitor (prints characters to console)
//   - Simulated SD card (optional)
//   - External memory stub (256 KiB)
//   - Waveform dump (VCD)
//

`timescale 1ns / 1ps

`include "mp64_defs.vh"

module tb_mp64_soc;

    // ========================================================================
    // Clock & Reset
    // ========================================================================
    reg sys_clk;
    reg sys_rst_n;

    initial sys_clk = 1'b0;
    always #5 sys_clk = ~sys_clk;  // 100 MHz

    initial begin
        sys_rst_n = 1'b0;
        #200;
        sys_rst_n = 1'b1;
        $display("[TB] Reset released at %0t ns", $time);
    end

    // ========================================================================
    // External memory stub
    // ========================================================================
    reg [63:0] ext_mem [0:32767];  // 256 KiB external

    wire        phy_req;
    wire [31:0] phy_addr;
    wire        phy_wen;
    wire [63:0] phy_wdata;
    wire [3:0]  phy_burst_len;
    reg  [63:0] phy_rdata;
    reg         phy_rvalid;
    reg         phy_ready;

    // Simple 2-cycle latency PHY model
    reg [3:0]  phy_state;
    reg [3:0]  phy_cnt;
    reg [31:0] phy_cur_addr;
    reg [3:0]  phy_burst_rem;

    localparam PHY_IDLE   = 4'd0;
    localparam PHY_READ   = 4'd1;
    localparam PHY_WRITE  = 4'd2;
    localparam PHY_BURST  = 4'd3;

    always @(posedge sys_clk or negedge sys_rst_n) begin
        if (!sys_rst_n) begin
            phy_rvalid <= 1'b0;
            phy_ready  <= 1'b1;
            phy_state  <= PHY_IDLE;
            phy_rdata  <= 64'd0;
        end else begin
            phy_rvalid <= 1'b0;

            case (phy_state)
                PHY_IDLE: begin
                    phy_ready <= 1'b1;
                    if (phy_req) begin
                        phy_ready     <= 1'b0;
                        phy_cur_addr  <= phy_addr;
                        phy_burst_rem <= phy_burst_len;
                        phy_cnt       <= 4'd1;  // 1-cycle latency
                        if (phy_wen)
                            phy_state <= PHY_WRITE;
                        else
                            phy_state <= PHY_READ;
                    end
                end

                PHY_READ: begin
                    if (phy_cnt > 0) begin
                        phy_cnt <= phy_cnt - 1;
                    end else begin
                        phy_rdata  <= ext_mem[phy_cur_addr[17:3]];
                        phy_rvalid <= 1'b1;
                        if (phy_burst_rem > 0) begin
                            phy_cur_addr  <= phy_cur_addr + 32'd8;
                            phy_burst_rem <= phy_burst_rem - 1;
                        end else begin
                            phy_ready <= 1'b1;
                            phy_state <= PHY_IDLE;
                        end
                    end
                end

                PHY_WRITE: begin
                    if (phy_cnt > 0) begin
                        phy_cnt <= phy_cnt - 1;
                    end else begin
                        ext_mem[phy_cur_addr[17:3]] <= phy_wdata;
                        if (phy_burst_rem > 0) begin
                            phy_cur_addr  <= phy_cur_addr + 32'd8;
                            phy_burst_rem <= phy_burst_rem - 1;
                        end else begin
                            phy_ready <= 1'b1;
                            phy_state <= PHY_IDLE;
                        end
                    end
                end
            endcase
        end
    end

    // ========================================================================
    // UART loopback / monitor
    // ========================================================================
    wire uart_txd;
    reg  uart_rxd;

    initial uart_rxd = 1'b1;  // idle high

    // TX bit capture (115200 baud ≈ 8681 ns per bit)
    localparam BAUD_NS = 8681;
    reg [7:0] uart_shift;
    reg [3:0] uart_bitcnt;
    reg       uart_sampling;

    initial begin
        uart_sampling = 1'b0;
        uart_bitcnt   = 4'd0;
    end

    always @(negedge uart_txd) begin
        if (!uart_sampling) begin
            uart_sampling = 1'b1;
            uart_bitcnt   = 4'd0;
            #(BAUD_NS / 2);  // center of start bit (should be 0)
            #BAUD_NS;        // skip start bit
            repeat (8) begin
                uart_shift = {uart_txd, uart_shift[7:1]};
                #BAUD_NS;
            end
            // stop bit
            if (uart_txd) begin
                if (uart_shift >= 8'h20 && uart_shift <= 8'h7E)
                    $write("%c", uart_shift);
                else if (uart_shift == 8'h0A)
                    $write("\n");
                else if (uart_shift == 8'h0D)
                    ;  // ignore CR
                else
                    $display("[UART] 0x%02h", uart_shift);
            end else begin
                $display("[UART] framing error");
            end
            uart_sampling = 1'b0;
        end
    end

    // ========================================================================
    // SPI-SD stub
    // ========================================================================
    wire sd_sck, sd_mosi, sd_cs_n;
    reg  sd_miso;

    initial begin
        sd_miso = 1'b1;
    end

    // ========================================================================
    // NIC stub
    // ========================================================================
    reg  nic_rx_valid;
    reg  [7:0] nic_rx_data;
    wire nic_tx_valid;
    wire [7:0] nic_tx_data;
    wire nic_rx_ready;
    reg  nic_tx_ready;
    reg  nic_link_up;

    initial begin
        nic_rx_valid = 1'b0;
        nic_rx_data  = 8'd0;
        nic_tx_ready = 1'b1;
        nic_link_up  = 1'b0;
    end

    // ========================================================================
    // DUT
    // ========================================================================
    wire [7:0] debug_leds;

    mp64_soc u_soc (
        .sys_clk      (sys_clk),
        .sys_rst_n    (sys_rst_n),
        .uart_rxd     (uart_rxd),
        .uart_txd     (uart_txd),
        .phy_req      (phy_req),
        .phy_addr     (phy_addr),
        .phy_wen      (phy_wen),
        .phy_wdata    (phy_wdata),
        .phy_burst_len(phy_burst_len),
        .phy_rdata    (phy_rdata),
        .phy_rvalid   (phy_rvalid),
        .phy_ready    (phy_ready),
        .sd_sck       (sd_sck),
        .sd_mosi      (sd_mosi),
        .sd_miso      (sd_miso),
        .sd_cs_n      (sd_cs_n),
        .nic_tx_valid (nic_tx_valid),
        .nic_tx_data  (nic_tx_data),
        .nic_tx_ready (nic_tx_ready),
        .nic_rx_valid (nic_rx_valid),
        .nic_rx_data  (nic_rx_data),
        .nic_rx_ready (nic_rx_ready),
        .nic_link_up  (nic_link_up),
        .debug_leds   (debug_leds)
    );

    // ========================================================================
    // BIOS ROM loading
    // ========================================================================
    initial begin
        // Load BIOS binary into BRAM via hierarchical reference
        $display("[TB] Loading BIOS...");
        $readmemh("bios.hex", u_soc.u_memory.bank0);
        $display("[TB] BIOS loaded.");
    end

    // ========================================================================
    // Simulation control
    // ========================================================================
    initial begin
        $dumpfile("mp64_soc.vcd");
        $dumpvars(0, tb_mp64_soc);

        #50_000_000;  // 50 ms = 5M cycles at 100 MHz
        $display("\n[TB] Simulation timeout at %0t ns", $time);
        $finish;
    end

    // ========================================================================
    // UART TX capture — print characters as they enter the TX FIFO
    // ========================================================================
    always @(posedge sys_clk) begin
        if (sys_rst_n &&
            u_soc.u_uart.req &&
            u_soc.u_uart.wen &&
            u_soc.u_uart.addr == 4'd0 && // UART_TX = 0
            !u_soc.u_uart.tx_full) begin
            $write("%c", u_soc.u_uart.wdata);
        end
    end

    // Debug: any MMIO write at all (disabled for clean output)
    // always @(posedge sys_clk) begin
    //     if (sys_rst_n && u_soc.u_bus.mmio_req) begin
    //         $display("[MMIO] ...");
    //     end
    // end

    // Watchdog: detect core 0 HALT (secondary cores halt normally)
    always @(posedge sys_clk) begin
        if (sys_rst_n &&
            u_soc.core[0].u_cpu.cpu_state == 4'd7) begin  // CPU_HALT
            #1000;
            $display("\n[TB] Core 0 halted at t=%0t ns, PC=0x%016h",
                     $time, u_soc.core[0].u_cpu.R[u_soc.core[0].u_cpu.psel]);
            // Show key registers
            $display("[TB] R0=0x%016h R1=0x%016h R2=0x%016h R3=0x%016h",
                     u_soc.core[0].u_cpu.R[0],
                     u_soc.core[0].u_cpu.R[1],
                     u_soc.core[0].u_cpu.R[2],
                     u_soc.core[0].u_cpu.R[3]);
            $display("[TB] R4=0x%016h R8=0x%016h R10=0x%016h R15=0x%016h",
                     u_soc.core[0].u_cpu.R[4],
                     u_soc.core[0].u_cpu.R[8],
                     u_soc.core[0].u_cpu.R[10],
                     u_soc.core[0].u_cpu.R[15]);
            $display("[TB] psel=%0d xsel=%0d flags=0x%02h",
                     u_soc.core[0].u_cpu.psel,
                     u_soc.core[0].u_cpu.xsel,
                     u_soc.core[0].u_cpu.flags);
            $finish;
        end
    end

    // Periodic status
    integer cycle_cnt;
    initial cycle_cnt = 0;
    always @(posedge sys_clk) begin
        cycle_cnt <= cycle_cnt + 1;
        if (cycle_cnt % 100000 == 0 && cycle_cnt > 0) begin
            $display("[TB] %0d cycles, PC=0x%016h, flags=0x%02h, state=%0d, R1=0x%016h, R10=0x%016h",
                     cycle_cnt,
                     u_soc.core[0].u_cpu.R[u_soc.core[0].u_cpu.psel],
                     u_soc.core[0].u_cpu.flags,
                     u_soc.core[0].u_cpu.cpu_state,
                     u_soc.core[0].u_cpu.R[1],
                     u_soc.core[0].u_cpu.R[10]);
        end
        // Early detailed dump: first 200 cycles after reset (disabled)
        // if (cycle_cnt >= 20 && cycle_cnt <= 500 && cycle_cnt % 10 == 0) begin
        //     ...
        // end
    end

endmodule
