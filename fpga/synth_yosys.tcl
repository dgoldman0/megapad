# =============================================================================
# synth_yosys.tcl — Yosys Synthesis Script for Megapad-64
# =============================================================================
#
# Open-source synthesis sanity check targeting Xilinx 7-series.
# Reports LUT/FF/BRAM/DSP utilisation for the logic fabric.
#
# The Xilinx-7 target wrappers decompose 512-bit SRAM primitives into
# 8 × 64-bit BRAM slices that Yosys can infer as RAMB36E1 / RAMB18E1.
#
# Top module: mp64_memory (the primary RAM consumer; validates that the
# decomposed SRAM blocks map correctly to BRAM).  The full SoC top
# (mp64_soc) is not yet integrated.
#
# Usage:  cd megapad-64 && yosys -s fpga/synth_yosys.tcl
#

# ---- NIC blackbox (deep FIFOs — not relevant for RAM testing) ---------------
read_verilog -sv <<EOT
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
EOT

# ---- Package / include files ------------------------------------------------
read_verilog -sv -DSIMULATION=0 \
    rtl/pkg/mp64_defs.vh       \
    rtl/pkg/mp64_pkg.vh        \
    rtl/pkg/mp64_cpu_common.vh

# ---- Xilinx-7 target primitives (override portable prim/) -------------------
# These wrappers decompose wide memories into narrow BRAM-friendly slices.
read_verilog -sv \
    rtl/target/xilinx7/mp64_sram_dp_xilinx7.v \
    rtl/target/xilinx7/mp64_sram_sp_xilinx7.v \
    rtl/target/xilinx7/mp64_clkgate_xilinx7.v \
    rtl/target/xilinx7/mp64_mul_xilinx7.v     \
    rtl/target/xilinx7/mp64_pll_xilinx7.v

# ---- Primitive helpers (not overridden by target) ----------------------------
# (mp64_rom and mp64_rst_sync omitted — not needed for mp64_memory target)

# ---- Memory subsystem (primary synthesis target) ----------------------------
read_verilog -sv -Irtl/pkg \
    rtl/mem/mp64_memory.v      \
    rtl/mem/mp64_extmem.v

# ---- Synthesise for Xilinx 7-series ----------------------------------------
log ===================================================================
log  Synthesis target: mp64_memory
log ===================================================================

synth_xilinx -top mp64_memory -family xc7 -edif fpga/build/mp64_synth.edif

# ---- Write utilisation report -----------------------------------------------
tee -o fpga/build/yosys_stats.txt stat -tech xilinx

log ===================================================================
log BRAM estimate (mp64_memory — 4 banks × 16384 × 512-bit):
log   Per bank:  8 slices × 16384×64-bit = 8 × 32 RAMB36 = 256 RAMB36
log   4 banks:   1024 RAMB36 total  (4 MiB TDP)
log   K325T has 445 RAMB36 — full design requires a larger FPGA
log     or reduced bank count / bank depth.
log ===================================================================
log Synthesis complete.  See fpga/build/yosys_stats.txt for details.
