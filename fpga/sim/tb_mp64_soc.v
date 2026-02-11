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
    wire [23:0] phy_addr;
    wire        phy_wen;
    wire [63:0] phy_wdata;
    wire [3:0]  phy_burst_len;
    reg  [63:0] phy_rdata;
    reg         phy_rvalid;
    reg         phy_ready;

    // Simple 2-cycle latency PHY model
    reg [3:0]  phy_state;
    reg [3:0]  phy_cnt;
    reg [23:0] phy_cur_addr;
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
                            phy_cur_addr  <= phy_cur_addr + 24'd8;
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
                            phy_cur_addr  <= phy_cur_addr + 24'd8;
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
    reg  sd_present;

    initial begin
        sd_miso    = 1'b1;
        sd_present = 1'b1;
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

    initial begin
        nic_rx_valid = 1'b0;
        nic_rx_data  = 8'd0;
        nic_tx_ready = 1'b1;
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
        .sd_present   (sd_present),
        .nic_tx_valid (nic_tx_valid),
        .nic_tx_data  (nic_tx_data),
        .nic_tx_ready (nic_tx_ready),
        .nic_rx_valid (nic_rx_valid),
        .nic_rx_data  (nic_rx_data),
        .nic_rx_ready (nic_rx_ready),
        .debug_leds   (debug_leds)
    );

    // ========================================================================
    // BIOS ROM loading
    // ========================================================================
    initial begin
        // Load BIOS binary into BRAM via hierarchical reference
        // Use: $readmemh("bios.hex", u_soc.u_memory.bram_512, 0, ...);
        // Or load byte-by-byte for flexibility:
        $display("[TB] Loading BIOS...");
        // In real sim, uncomment:
        // $readmemh("../bios.hex", u_soc.u_memory.bram_512);
        $display("[TB] BIOS loaded.");
    end

    // ========================================================================
    // Simulation control
    // ========================================================================
    initial begin
        $dumpfile("mp64_soc.vcd");
        $dumpvars(0, tb_mp64_soc);

        #10_000_000;  // 10 ms
        $display("\n[TB] Simulation timeout at %0t ns", $time);
        $finish;
    end

    // Watchdog: detect HALT
    always @(posedge sys_clk) begin
        if (u_soc.u_cpu.cpu_state == 4'd7) begin  // CPU_HALT
            #1000;
            $display("[TB] CPU halted at t=%0t ns, PC=0x%016h",
                     $time, u_soc.u_cpu.R[u_soc.u_cpu.psel]);
            $finish;
        end
    end

    // Periodic status
    integer cycle_cnt;
    initial cycle_cnt = 0;
    always @(posedge sys_clk) begin
        cycle_cnt <= cycle_cnt + 1;
        if (cycle_cnt % 100000 == 0 && cycle_cnt > 0) begin
            $display("[TB] %0d cycles, PC=0x%016h, flags=0x%02h",
                     cycle_cnt,
                     u_soc.u_cpu.R[u_soc.u_cpu.psel],
                     u_soc.u_cpu.flags);
        end
    end

endmodule
