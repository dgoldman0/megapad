# =============================================================================
# synth_genesys2.tcl — Vivado Synthesis Script for Megapad-64 on Genesys 2
# =============================================================================
#
# Target:  Digilent Genesys 2 — Kintex-7 325T (xc7k325tffg900-2)
# Usage:   vivado -mode batch -source synth_genesys2.tcl
#          vivado -mode tcl   -source synth_genesys2.tcl   (interactive)
#
# Outputs: fpga/build/mp64_synth_top.dcp   — post-synth checkpoint
#          fpga/build/utilisation.rpt       — resource usage
#          fpga/build/timing_summary.rpt    — timing summary
#

set PROJ_DIR  [file dirname [file normalize [info script]]]
set REPO_DIR  [file normalize ${PROJ_DIR}/..]
set RTL_DIR   ${REPO_DIR}/rtl
set CONST_DIR ${PROJ_DIR}/constraints
set BUILD_DIR ${PROJ_DIR}/build

file mkdir ${BUILD_DIR}

# -------------------------------------------------------------------------
# 1. Create in-memory project
# -------------------------------------------------------------------------
create_project -in_memory -part xc7k325tffg900-2

set_property target_language Verilog [current_project]

# -------------------------------------------------------------------------
# 2. Add RTL sources
# -------------------------------------------------------------------------
set rtl_files [list \
    ${RTL_DIR}/pkg/mp64_defs.vh                    \
    ${RTL_DIR}/pkg/mp64_pkg.vh                     \
    ${RTL_DIR}/pkg/mp64_cpu_common.vh              \
    ${RTL_DIR}/target/xilinx7/mp64_sram_dp_xilinx7.v \
    ${RTL_DIR}/target/xilinx7/mp64_sram_sp_xilinx7.v \
    ${RTL_DIR}/target/xilinx7/mp64_clkgate_xilinx7.v \
    ${RTL_DIR}/target/xilinx7/mp64_mul_xilinx7.v   \
    ${RTL_DIR}/target/xilinx7/mp64_pll_xilinx7.v   \
    ${RTL_DIR}/core/mp64_alu.v                     \
    ${RTL_DIR}/core/mp64_bitfield.v                \
    ${RTL_DIR}/core/mp64_dict.v                    \
    ${RTL_DIR}/core/mp64_fp16_alu.v                \
    ${RTL_DIR}/core/mp64_string.v                  \
    ${RTL_DIR}/core/mp64_cpu.v                     \
    ${RTL_DIR}/core/mp64_cpu_micro.v               \
    ${RTL_DIR}/core/mp64_icache.v                  \
    ${RTL_DIR}/core/mp64_cluster.v                 \
    ${RTL_DIR}/bus/mp64_bus.v                      \
    ${RTL_DIR}/mem/mp64_memory.v                   \
    ${RTL_DIR}/mem/mp64_extmem.v                   \
    ${RTL_DIR}/gpu/mp64_tile.v                     \
    ${RTL_DIR}/periph/mp64_uart.v                  \
    ${RTL_DIR}/periph/mp64_timer.v                 \
    ${RTL_DIR}/periph/mp64_disk.v                  \
    ${RTL_DIR}/periph/mp64_nic.v                   \
    ${RTL_DIR}/periph/mp64_mailbox.v               \
    ${RTL_DIR}/periph/mp64_rtc.v                   \
    ${RTL_DIR}/periph/mp64_trng.v                  \
    ${RTL_DIR}/crypto/mp64_aes.v                   \
    ${RTL_DIR}/crypto/mp64_crc_isa.v               \
    ${RTL_DIR}/crypto/mp64_field_alu_isa.v         \
    ${RTL_DIR}/crypto/mp64_kem.v                   \
    ${RTL_DIR}/crypto/mp64_ntt.v                   \
    ${RTL_DIR}/crypto/mp64_sha2_isa.v              \
    ${RTL_DIR}/crypto/mp64_sha3.v                  \
    ${RTL_DIR}/crypto/mp64_wots.v                  \
    ${RTL_DIR}/soc/mp64_core_bus_mux.v             \
    ${RTL_DIR}/soc/mp64_tile_port_arbiter.v        \
    ${RTL_DIR}/soc/mp64_soc.v                      \
    ${RTL_DIR}/target/xilinx7/mp64_synth_top.v     \
]

foreach f $rtl_files {
    if {[file extension $f] eq ".vh"} {
        read_verilog -sv -include_dirs ${RTL_DIR}/pkg $f
        set_property IS_GLOBAL_INCLUDE TRUE [get_files [file tail $f]]
    } else {
        read_verilog -sv -include_dirs ${RTL_DIR}/pkg $f
    }
}

# -------------------------------------------------------------------------
# 3. Add constraints
# -------------------------------------------------------------------------
read_xdc ${CONST_DIR}/genesys2.xdc

# -------------------------------------------------------------------------
# 4. Synthesise
# -------------------------------------------------------------------------
set_property top mp64_synth_top [current_fileset]

synth_design \
    -top mp64_synth_top \
    -part xc7k325tffg900-2 \
    -flatten_hierarchy rebuilt \
    -directive AreaOptimized_high \
    -retiming on \
    -verilog_define SIMULATION=0

write_checkpoint -force ${BUILD_DIR}/mp64_post_synth.dcp

# -------------------------------------------------------------------------
# 5. Reports
# -------------------------------------------------------------------------
report_utilization -file ${BUILD_DIR}/utilisation.rpt
report_utilization -hierarchical -file ${BUILD_DIR}/utilisation_hier.rpt
report_timing_summary -file ${BUILD_DIR}/timing_summary.rpt \
    -max_paths 20 -report_unconstrained
report_timing -of_objects [get_timing_paths -max_paths 10 -slack_type violated] \
    -file ${BUILD_DIR}/timing_violations.rpt

# Summary to stdout
puts "================================================================"
puts "  Synthesis Complete"
puts "================================================================"
report_utilization
puts ""
puts "Worst negative slack:"
set wns [get_property SLACK [get_timing_paths -max_paths 1]]
puts "  WNS = ${wns} ns"
puts ""
puts "Checkpoint: ${BUILD_DIR}/mp64_post_synth.dcp"
puts "================================================================"

# -------------------------------------------------------------------------
# 6. Optional: Place & Route (uncomment for full implementation)
# -------------------------------------------------------------------------
# opt_design
# place_design
# phys_opt_design
# route_design
# write_checkpoint -force ${BUILD_DIR}/mp64_post_route.dcp
# report_utilization -file ${BUILD_DIR}/utilisation_post_route.rpt
# report_timing_summary -file ${BUILD_DIR}/timing_post_route.rpt \
#     -max_paths 20 -report_unconstrained
# write_bitstream -force ${BUILD_DIR}/mp64_genesys2.bit
