# =============================================================================
# synth_yosys_soc.tcl — Yosys Full-SoC Synthesis for Megapad-64
# =============================================================================
#
# Synthesises the complete mp64_soc with all sub-modules for Xilinx 7-series.
# This includes CPU cores, I-caches, clusters, bus, memory, tile engine,
# all peripherals, and crypto accelerators.
#
# Usage:  cd megapad-64 && yosys -s fpga/synth_yosys_soc.tcl
#

# ---- NIC blackbox (deep FIFOs — too complex for stateless synthesis) --------
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
read_verilog -sv \
    rtl/target/xilinx7/mp64_sram_dp_xilinx7.v \
    rtl/target/xilinx7/mp64_sram_sp_xilinx7.v \
    rtl/target/xilinx7/mp64_clkgate_xilinx7.v \
    rtl/target/xilinx7/mp64_mul_xilinx7.v     \
    rtl/target/xilinx7/mp64_pll_xilinx7.v

# ---- Core -------------------------------------------------------------------
read_verilog -sv -Irtl/pkg \
    rtl/core/mp64_alu.v        \
    rtl/core/mp64_fp16_alu.v   \
    rtl/core/mp64_cpu.v        \
    rtl/core/mp64_cpu_micro.v  \
    rtl/core/mp64_icache.v     \
    rtl/core/mp64_cluster.v

# ---- Bus ---------------------------------------------------------------------
read_verilog -sv -Irtl/pkg \
    rtl/bus/mp64_bus.v

# ---- Memory ------------------------------------------------------------------
read_verilog -sv -Irtl/pkg \
    rtl/mem/mp64_memory.v      \
    rtl/mem/mp64_extmem.v

# ---- GPU / Tile engine -------------------------------------------------------
read_verilog -sv -Irtl/pkg \
    rtl/gpu/mp64_tile.v

# ---- Peripherals -------------------------------------------------------------
read_verilog -sv -Irtl/pkg \
    rtl/periph/mp64_uart.v     \
    rtl/periph/mp64_timer.v    \
    rtl/periph/mp64_disk.v     \
    rtl/periph/mp64_mailbox.v  \
    rtl/periph/mp64_crc.v      \
    rtl/periph/mp64_trng.v

# ---- Crypto accelerators ----------------------------------------------------
read_verilog -sv -Irtl/pkg \
    rtl/crypto/mp64_aes.v      \
    rtl/crypto/mp64_sha3.v     \
    rtl/crypto/mp64_field_alu.v \
    rtl/crypto/mp64_ntt.v      \
    rtl/crypto/mp64_kem.v

# ---- SoC integration --------------------------------------------------------
read_verilog -sv -Irtl/pkg \
    rtl/soc/mp64_soc.v

# ---- Synthesise for Xilinx 7-series ----------------------------------------
log ===================================================================
log  Synthesis target: mp64_soc  (full SoC)
log ===================================================================

synth_xilinx -top mp64_soc -family xc7 -edif fpga/build/mp64_soc_synth.edif

# ---- Write utilisation report -----------------------------------------------
tee -o fpga/build/yosys_soc_stats.txt stat -tech xilinx
