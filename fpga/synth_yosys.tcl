# =============================================================================
# synth_yosys.tcl — Yosys Synthesis Script for Megapad-64
# =============================================================================
#
# Open-source synthesis sanity check targeting Xilinx 7-series.
# Reports LUT/FF/BRAM/DSP utilisation for the logic fabric.
#
# Notes:
#   - mp64_memory is blackboxed (1 MiB array maps to BRAM; Yosys 0.33
#     cannot infer 512-bit-wide block RAMs and tries to flatten).
#   - mp64_synth_top is skipped (IBUFDS/MMCME2_BASE/BUFG are Vivado-only).
#   - Top module is mp64_soc; BRAM usage is estimated separately below.
#
# Usage:  cd fpga && yosys -s synth_yosys.tcl
#

# ---- Blackbox stubs for memory-heavy modules (map to BRAM) ------------------
read_verilog -sv <<EOT
(* blackbox *)
module mp64_memory (
    input         clk,
    input         rst_n,
    input         cpu_req,
    input  [63:0] cpu_addr,
    input  [63:0] cpu_wdata,
    input         cpu_wen,
    input  [1:0]  cpu_size,
    output [63:0] cpu_rdata,
    output        cpu_ack,
    input         tile_req,
    input  [19:0] tile_addr,
    input         tile_wen,
    input  [511:0] tile_wdata,
    output [511:0] tile_rdata,
    output        tile_ack,
    output        ext_req,
    output [63:0] ext_addr,
    output [63:0] ext_wdata,
    output        ext_wen,
    output [1:0]  ext_size,
    input  [63:0] ext_rdata,
    input         ext_ack
);
endmodule

(* blackbox *)
module mp64_nic (
    input         clk,
    input         rst_n,
    input         req,
    input  [6:0]  addr,
    input  [7:0]  wdata,
    input         wen,
    output [7:0]  rdata,
    output        ack,
    output        irq,
    output        dma_req,
    output [63:0] dma_addr,
    output [7:0]  dma_wdata,
    output        dma_wen,
    input  [7:0]  dma_rdata,
    input         dma_ack,
    output        phy_tx_valid,
    output [7:0]  phy_tx_data,
    input         phy_tx_ready,
    input         phy_rx_valid,
    input  [7:0]  phy_rx_data,
    output        phy_rx_ready,
    input         phy_link_up
);
endmodule

(* blackbox *)
module mp64_icache (
    input         clk,
    input         rst_n,
    input  [63:0] fetch_addr,
    input         fetch_valid,
    output [63:0] fetch_data,
    output        fetch_hit,
    output        fetch_stall,
    output        bus_valid,
    output [63:0] bus_addr,
    input  [63:0] bus_rdata,
    input         bus_ready,
    output        bus_wen,
    output [1:0]  bus_size,
    input         inv_all,
    input         inv_line,
    input  [63:0] inv_addr,
    output [63:0] stat_hits,
    output [63:0] stat_misses
);
endmodule
EOT

# ---- Read RTL (everything except blackboxed modules & synth wrapper) --------
read_verilog -sv -DSIMULATION=0 \
    rtl/mp64_defs.vh      \
    rtl/mp64_soc.v        \
    rtl/mp64_cpu.v        \
    rtl/mp64_bus.v        \
    rtl/mp64_extmem.v     \
    rtl/mp64_uart.v       \
    rtl/mp64_timer.v      \
    rtl/mp64_disk.v       \
    rtl/mp64_tile.v       \
    rtl/mp64_mailbox.v    \
    rtl/mp64_fp16_alu.v   \
    rtl/mp64_aes.v        \
    rtl/mp64_sha3.v       \
    rtl/mp64_crc.v

# ---- Synthesise for Xilinx 7-series ----------------------------------------
synth_xilinx -top mp64_soc -family xc7 -edif build/mp64_synth.edif

# ---- Write utilisation report -----------------------------------------------
tee -o build/yosys_stats.txt stat -tech xilinx

log ===================================================================
log BRAM estimate (blackboxed modules — maps directly to BRAM):
log   Main memory:  16384 x 512-bit  = 256 RAMB36 (1 MiB TDP)
log   I-cache (x4): 2 x 256 x 64-bit data + 256 x 8-bit tags per core
log                  = (4+1) x 4 = 20 RAMB18 = 10 RAMB36
log   NIC buffers:  2 x 1500 x 8-bit + 96 x 8-bit = 4 RAMB18 = 2 RAMB36
log   Total BRAM:   ~268 RAMB36 of 445 (K325T) = 60%
log ===================================================================
log Synthesis complete. See build/yosys_stats.txt for resource utilisation.
