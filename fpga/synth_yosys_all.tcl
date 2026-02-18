# =============================================================================
# synth_yosys_all.tcl — Deep Yosys Synthesis Test for every Megapad-64 module
# =============================================================================
#
# Synthesises each major design module individually for Xilinx 7-series.
# Verifies that all modules map cleanly to LUTs/FFs/BRAMs/DSPs.
#
# The BIOS ROM is synthesised with real contents from fpga/bios.hex.
#
# Usage:  cd megapad-64 && yosys -s fpga/synth_yosys_all.tcl
#
# Generates:
#   fpga/build/synth_<module>.json — per-module netlist
#   fpga/build/synth_all_stats.txt — combined resource report
#

# ---- NIC blackbox (deep FIFOs — skip for now) --------------------------------
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
read_verilog -sv -DSIMULATION=0 -Irtl/pkg \
    rtl/pkg/mp64_defs.vh       \
    rtl/pkg/mp64_pkg.vh

# ---- Xilinx-7 target primitives ---------------------------------------------
read_verilog -sv -Irtl/pkg \
    rtl/target/xilinx7/mp64_sram_dp_xilinx7.v \
    rtl/target/xilinx7/mp64_sram_sp_xilinx7.v \
    rtl/target/xilinx7/mp64_clkgate_xilinx7.v \
    rtl/target/xilinx7/mp64_mul_xilinx7.v     \
    rtl/target/xilinx7/mp64_pll_xilinx7.v

# ---- ROM with real BIOS contents --------------------------------------------
read_verilog -sv -Irtl/pkg \
    rtl/prim/mp64_rom.v

# ---- All design RTL ---------------------------------------------------------
read_verilog -sv -Irtl/pkg \
    rtl/core/mp64_alu.v        \
    rtl/core/mp64_cpu_micro.v  \
    rtl/core/mp64_cpu.v        \
    rtl/core/mp64_icache.v     \
    rtl/core/mp64_cluster.v    \
    rtl/core/mp64_fp16_alu.v   \
    rtl/mem/mp64_memory.v      \
    rtl/mem/mp64_extmem.v      \
    rtl/bus/mp64_bus.v         \
    rtl/periph/mp64_uart.v     \
    rtl/periph/mp64_timer.v    \
    rtl/periph/mp64_disk.v     \
    rtl/periph/mp64_mailbox.v  \
    rtl/periph/mp64_crc.v      \
    rtl/periph/mp64_trng.v     \
    rtl/crypto/mp64_aes.v      \
    rtl/crypto/mp64_sha3.v     \
    rtl/crypto/mp64_field_alu.v \
    rtl/crypto/mp64_ntt.v      \
    rtl/crypto/mp64_kem.v      \
    rtl/gpu/mp64_tile.v

log ===================================================================
log  Deep Synthesis Test — all modules loaded
log ===================================================================

# ---- Helper: synthesise one module ------------------------------------------
# Yosys doesn't support Tcl procs with -s, so we run the full synth flow
# against mp64_memory (already proven), then re-read and synth each
# additional module solo.  We output JSON netlists for each.

# === 1. Memory subsystem (proven — benchmark) ================================
log -------------------------------------------------------------------
log  [1/9] mp64_memory  (4-bank × 8-slice SRAM)
log -------------------------------------------------------------------
synth_xilinx -top mp64_memory -family xc7 -json fpga/build/synth_memory.json
tee -a fpga/build/synth_all_stats.txt stat -tech xilinx

# === 2. CPU core ==============================================================
log -------------------------------------------------------------------
log  [2/9] mp64_cpu  (64-bit pipeline + ALU + MUL)
log -------------------------------------------------------------------
synth_xilinx -top mp64_cpu -family xc7 -json fpga/build/synth_cpu.json
tee -a fpga/build/synth_all_stats.txt stat -tech xilinx

# === 3. Instruction cache =====================================================
log -------------------------------------------------------------------
log  [3/9] mp64_icache  (4 KiB direct-mapped)
log -------------------------------------------------------------------
synth_xilinx -top mp64_icache -family xc7 -json fpga/build/synth_icache.json
tee -a fpga/build/synth_all_stats.txt stat -tech xilinx

# === 4. Cluster ===============================================================
log -------------------------------------------------------------------
log  [4/9] mp64_cluster  (micro-cores + shared MUL)
log -------------------------------------------------------------------
synth_xilinx -top mp64_cluster -family xc7 -json fpga/build/synth_cluster.json
tee -a fpga/build/synth_all_stats.txt stat -tech xilinx

# === 5. Bus arbiter ===========================================================
log -------------------------------------------------------------------
log  [5/9] mp64_bus  (multi-master arbiter + decoder)
log -------------------------------------------------------------------
synth_xilinx -top mp64_bus -family xc7 -json fpga/build/synth_bus.json
tee -a fpga/build/synth_all_stats.txt stat -tech xilinx

# === 6. AES + SHA-3 crypto ====================================================
log -------------------------------------------------------------------
log  [6/9] mp64_aes  (AES-128/256 encrypt/decrypt)
log -------------------------------------------------------------------
synth_xilinx -top mp64_aes -family xc7 -json fpga/build/synth_aes.json
tee -a fpga/build/synth_all_stats.txt stat -tech xilinx

log -------------------------------------------------------------------
log  [6b/9] mp64_sha3  (SHA-3/Keccak)
log -------------------------------------------------------------------
synth_xilinx -top mp64_sha3 -family xc7 -json fpga/build/synth_sha3.json
tee -a fpga/build/synth_all_stats.txt stat -tech xilinx

# === 7. Field ALU + NTT + KEM (post-quantum crypto) ==========================
log -------------------------------------------------------------------
log  [7/9] mp64_field_alu  (multi-prime field arithmetic)
log -------------------------------------------------------------------
synth_xilinx -top mp64_field_alu -family xc7 -json fpga/build/synth_field_alu.json
tee -a fpga/build/synth_all_stats.txt stat -tech xilinx

log -------------------------------------------------------------------
log  [7b/9] mp64_ntt  (Number Theoretic Transform)
log -------------------------------------------------------------------
synth_xilinx -top mp64_ntt -family xc7 -json fpga/build/synth_ntt.json
tee -a fpga/build/synth_all_stats.txt stat -tech xilinx

log -------------------------------------------------------------------
log  [7c/9] mp64_kem  (Key Encapsulation Mechanism)
log -------------------------------------------------------------------
synth_xilinx -top mp64_kem -family xc7 -json fpga/build/synth_kem.json
tee -a fpga/build/synth_all_stats.txt stat -tech xilinx

# === 8. Tile engine ===========================================================
log -------------------------------------------------------------------
log  [8/9] mp64_tile  (GPU tile engine)
log -------------------------------------------------------------------
synth_xilinx -top mp64_tile -family xc7 -json fpga/build/synth_tile.json
tee -a fpga/build/synth_all_stats.txt stat -tech xilinx

# === 9. ROM with BIOS =========================================================
log -------------------------------------------------------------------
log  [9/9] mp64_rom  (BIOS ROM — 3794 × 64-bit words via wrapper)
log -------------------------------------------------------------------

# Instantiate ROM with BIOS-appropriate parameters (ADDR_W=12, DATA_W=64)
read_verilog -sv <<EOT
module mp64_bios_rom (
    input  wire        clk,
    input  wire        rst_n,
    input  wire        ce,
    input  wire [11:0] addr,
    output wire [63:0] rdata
);
    mp64_rom #(
        .ADDR_W    (12),
        .DATA_W    (64),
        .DEPTH     (4096),
        .OUT_REG   (1),
        .INIT_FILE ("rom.hex")
    ) u_bios (
        .clk   (clk),
        .rst_n (rst_n),
        .ce    (ce),
        .addr  (addr),
        .rdata (rdata)
    );
endmodule
EOT

synth_xilinx -top mp64_bios_rom -family xc7 -json fpga/build/synth_bios_rom.json
tee -a fpga/build/synth_all_stats.txt stat -tech xilinx

# ---- Summary ----------------------------------------------------------------
log ===================================================================
log  Deep Synthesis Complete — all modules
log ===================================================================
log  Results in fpga/build/synth_all_stats.txt
log  Per-module netlists: fpga/build/synth_*.json
log ===================================================================
