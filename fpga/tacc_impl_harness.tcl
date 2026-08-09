# Common routed-implementation harness for the three-build TACC comparison.
#
# This file is copied and hash-attested independently of each historical
# source snapshot.  Vivado is not available in the development environment
# where it was prepared, so every report/query below must receive a first dry
# run on the eventual hardware-development workstation.  Any unsupported
# query is a hard failure; the Python checker never fabricates missing data.

foreach required_env {
    TACC_SOURCE_ROOT
    TACC_BUILD_DIR
    TACC_MEM_DEPTH
} {
    if {![info exists ::env($required_env)] ||
        [string trim $::env($required_env)] eq ""} {
        error "missing required environment variable $required_env"
    }
}

set TACC_SOURCE_ROOT [file normalize $::env(TACC_SOURCE_ROOT)]
set TACC_BUILD_DIR [file normalize $::env(TACC_BUILD_DIR)]
set TACC_MEM_DEPTH [string trim $::env(TACC_MEM_DEPTH)]

if {![string is integer -strict $TACC_MEM_DEPTH] ||
    $TACC_MEM_DEPTH < 512 ||
    $TACC_MEM_DEPTH > 16384 ||
    (($TACC_MEM_DEPTH & ($TACC_MEM_DEPTH - 1)) != 0)} {
    error "TACC_MEM_DEPTH must be a power of two from 512 through 16384"
}
if {![file isdirectory $TACC_SOURCE_ROOT]} {
    error "TACC_SOURCE_ROOT is not a directory: $TACC_SOURCE_ROOT"
}
file mkdir $TACC_BUILD_DIR

# Literal metadata is kept beside literal command options so the runner can
# audit the flow without evaluating arbitrary Tcl.
set TACC_MEASUREMENT_PART xc7k325tffg900-2
set TACC_MEASUREMENT_TOP mp64_soc
set TACC_MEASUREMENT_CLOCK_MHZ 100.0
set TACC_SYNTH_FLATTEN_HIERARCHY none
set TACC_SYNTH_DIRECTIVE AreaOptimized_high
set TACC_SYNTH_RETIMING on
set TACC_SYNTH_VERILOG_DEFINE SIMULATION=0
set TACC_OPT_DIRECTIVE Explore
set TACC_PLACE_DIRECTIVE Explore
set TACC_PHYS_OPT_DIRECTIVE Explore
set TACC_ROUTE_DIRECTIVE Explore

proc require_file {path} {
    if {![file isfile $path]} {
        error "required source file is missing: $path"
    }
    return $path
}

proc append_report_line {path line} {
    set stream [open $path a]
    puts $stream $line
    close $stream
}

proc original_module_cells {module_name} {
    return [get_cells -hierarchical -quiet -filter \
        "ORIG_REF_NAME == $module_name || REF_NAME =~ ${module_name}*"]
}

proc primitive_resources_under {roots} {
    set lut 0
    set ff 0
    set bram 0.0
    set dsp 0
    foreach root $roots {
        set root_name [get_property NAME $root]
        foreach cell [get_cells -hierarchical -quiet "${root_name}/*"] {
            set ref_name [get_property REF_NAME $cell]
            if {[regexp {^LUT[1-6](_2)?$} $ref_name]} {
                incr lut
            } elseif {[regexp {^FD} $ref_name]} {
                incr ff
            } elseif {[regexp {^RAMB36} $ref_name]} {
                set bram [expr {$bram + 1.0}]
            } elseif {[regexp {^RAMB18} $ref_name]} {
                set bram [expr {$bram + 0.5}]
            } elseif {[regexp {^DSP48} $ref_name]} {
                incr dsp
            }
        }
    }
    return [list $lut $ff $bram $dsp]
}

proc count_named_primitives_under {roots name_pattern} {
    set count 0
    foreach root $roots {
        set root_name [get_property NAME $root]
        foreach cell [get_cells -hierarchical -quiet "${root_name}/*"] {
            set cell_name [get_property NAME $cell]
            if {[string match $name_pattern $cell_name]} {
                incr count
            }
        }
    }
    return $count
}

proc count_original_module_under {root module_name} {
    set count 0
    set root_name [get_property NAME $root]
    foreach cell [get_cells -hierarchical -quiet "${root_name}/*"] {
        set ref_name [get_property REF_NAME $cell]
        set original_ref [get_property -quiet ORIG_REF_NAME $cell]
        if {$original_ref eq $module_name ||
            [string match "${module_name}*" $ref_name]} {
            incr count
        }
    }
    return $count
}

set package_headers [list \
    rtl/pkg/mp64_defs.vh \
    rtl/pkg/mp64_pkg.vh \
    rtl/pkg/mp64_cpu_common.vh \
]

# The locked baselines share this required source set.  Only the exact-FP,
# TACC-bank, and TACC-transfer modules were introduced by later landings.
set required_rtl [list \
    rtl/target/xilinx7/mp64_sram_dp_xilinx7.v \
    rtl/target/xilinx7/mp64_sram_sp_xilinx7.v \
    rtl/target/xilinx7/mp64_clkgate_xilinx7.v \
    rtl/target/xilinx7/mp64_mul_xilinx7.v \
    rtl/target/xilinx7/mp64_pll_xilinx7.v \
    rtl/core/mp64_alu.v \
    rtl/core/mp64_bitfield.v \
    rtl/core/mp64_dict.v \
    rtl/core/mp64_fp16_alu.v \
    rtl/core/mp64_string.v \
    rtl/core/mp64_cpu.v \
    rtl/core/mp64_cpu_micro.v \
    rtl/core/mp64_icache.v \
    rtl/core/mp64_cluster.v \
    rtl/bus/mp64_bus.v \
    rtl/mem/mp64_memory.v \
    rtl/mem/mp64_extmem.v \
    rtl/gpu/mp64_tile.v \
    rtl/periph/mp64_uart.v \
    rtl/periph/mp64_timer.v \
    rtl/periph/mp64_disk.v \
    rtl/periph/mp64_nic.v \
    rtl/periph/mp64_mailbox.v \
    rtl/periph/mp64_rtc.v \
    rtl/periph/mp64_trng.v \
    rtl/crypto/mp64_aes.v \
    rtl/crypto/mp64_crc_isa.v \
    rtl/crypto/mp64_field_alu_isa.v \
    rtl/crypto/mp64_kem.v \
    rtl/crypto/mp64_ntt.v \
    rtl/crypto/mp64_sha2_isa.v \
    rtl/crypto/mp64_sha3.v \
    rtl/soc/mp64_core_bus_mux.v \
    rtl/soc/mp64_tile_port_arbiter.v \
    rtl/soc/mp64_soc.v \
]
set version_added_rtl [list \
    rtl/core/mp64_fp_exact.v \
    rtl/gpu/mp64_tacc.v \
    rtl/soc/mp64_tacc_transfer.v \
]

create_project -in_memory -part xc7k325tffg900-2
set_property target_language Verilog [current_project]
set_property source_mgmt_mode None [current_project]
set_param general.maxThreads 1

set include_dir [file join $TACC_SOURCE_ROOT rtl pkg]
foreach relative $package_headers {
    set source_path [require_file [file join $TACC_SOURCE_ROOT $relative]]
    read_verilog -sv -include_dirs $include_dir $source_path
    set_property IS_GLOBAL_INCLUDE TRUE [get_files [file tail $source_path]]
}
foreach relative $required_rtl {
    set source_path [require_file [file join $TACC_SOURCE_ROOT $relative]]
    read_verilog -sv -include_dirs $include_dir $source_path
}
foreach relative $version_added_rtl {
    set source_path [file join $TACC_SOURCE_ROOT $relative]
    if {[file isfile $source_path]} {
        read_verilog -sv -include_dirs $include_dir $source_path
    }
}

set constraint_path [file join [file dirname [info script]] \
    constraints tacc_measurement.xdc]
read_xdc $constraint_path

synth_design \
    -top mp64_soc \
    -part xc7k325tffg900-2 \
    -flatten_hierarchy none \
    -directive AreaOptimized_high \
    -retiming on \
    -verilog_define SIMULATION=0 \
    -generic MEM_DEPTH=$TACC_MEM_DEPTH

write_checkpoint -force [file join $TACC_BUILD_DIR tacc_post_synth.dcp]
report_utilization \
    -file [file join $TACC_BUILD_DIR utilisation.rpt]

opt_design -directive Explore
place_design -directive Explore
phys_opt_design -directive Explore
route_design -directive Explore

write_checkpoint -force [file join $TACC_BUILD_DIR tacc_post_route.dcp]
report_utilization \
    -file [file join $TACC_BUILD_DIR utilisation_post_route.rpt]
report_timing_summary \
    -delay_type max \
    -max_paths 20 \
    -report_unconstrained \
    -file [file join $TACC_BUILD_DIR timing_post_route.rpt]
report_utilization \
    -hierarchical \
    -file [file join $TACC_BUILD_DIR utilisation_post_route_hier.rpt]
report_route_status \
    -file [file join $TACC_BUILD_DIR tacc_route_status.rpt]

set timing_report [file join $TACC_BUILD_DIR timing_post_route.rpt]
set measured_clocks [get_clocks -quiet -of_objects [get_ports sys_clk]]
if {[llength $measured_clocks] != 1} {
    error "expected exactly one applied clock on sys_clk"
}
set measured_period [get_property PERIOD [lindex $measured_clocks 0]]
set all_max_paths [get_timing_paths -quiet -delay_type max \
    -max_paths 1000000 -nworst 1]
if {[llength $all_max_paths] >= 1000000} {
    error "unconstrained-path query reached its fail-closed path limit"
}
set unconstrained_count 0
foreach timing_path $all_max_paths {
    set path_slack [get_property SLACK $timing_path]
    if {[regexp -nocase {inf} $path_slack]} {
        incr unconstrained_count
    }
}
set worst_path [lindex [get_timing_paths -delay_type max \
    -max_paths 1 -no_report_unconstrained] 0]
if {$worst_path eq ""} {
    error "routed design has no constrained maximum-delay timing path"
}
set routed_wns [get_property SLACK $worst_path]
set routed_fmax [expr {1000.0 / ($measured_period - $routed_wns)}]
append_report_line $timing_report \
    "TACC_TIMING clock_period_ns $measured_period"
append_report_line $timing_report \
    "TACC_TIMING unconstrained_paths $unconstrained_count"
append_report_line $timing_report "TACC_TIMING wns_ns $routed_wns"
append_report_line $timing_report "TACC_TIMING fmax_mhz $routed_fmax"

set tile_cells [original_module_cells mp64_tile]
set tacc_cells [original_module_cells mp64_tacc]
set hierarchy_report \
    [file join $TACC_BUILD_DIR utilisation_post_route_hier.rpt]
append_report_line $hierarchy_report \
    "TACC_HIERARCHY mp64_tile [llength $tile_cells]"
append_report_line $hierarchy_report \
    "TACC_HIERARCHY mp64_tacc [llength $tacc_cells]"

foreach {tile_lut tile_ff tile_bram tile_dsp} \
        [primitive_resources_under $tile_cells] {}
foreach {tacc_lut tacc_ff tacc_bram tacc_dsp} \
        [primitive_resources_under $tacc_cells] {}
foreach {module lut ff bram dsp} [list \
        mp64_tile $tile_lut $tile_ff $tile_bram $tile_dsp \
        mp64_tacc $tacc_lut $tacc_ff $tacc_bram $tacc_dsp] {
    append_report_line $hierarchy_report \
        "TACC_HIER_RESOURCE ${module}.lut $lut"
    append_report_line $hierarchy_report \
        "TACC_HIER_RESOURCE ${module}.ff $ff"
    append_report_line $hierarchy_report \
        "TACC_HIER_RESOURCE ${module}.bram $bram"
    append_report_line $hierarchy_report \
        "TACC_HIER_RESOURCE ${module}.dsp $dsp"
}

set route_report [file join $TACC_BUILD_DIR tacc_route_status.rpt]
set routed_fully [report_route_status -boolean_check ROUTED_FULLY]
set route_errors [report_route_status -boolean_check ERRORS_IN_ROUTES]
append_report_line $route_report \
    "TACC_ROUTE_STATUS is_route_design $routed_fully"
if {$routed_fully} {
    append_report_line $route_report "TACC_ROUTE_STATUS status routed"
} else {
    append_report_line $route_report "TACC_ROUTE_STATUS status unrouted"
}
append_report_line $route_report \
    "TACC_ROUTE_STATUS errors_in_routes $route_errors"

set feedback_cells [original_module_cells mp64_fp32_feedback_rne]
set feedback_count [llength $feedback_cells]
set tile_count [llength $tile_cells]
set tacc_count [llength $tacc_cells]
set max_feedback_lanes 0
foreach tile_cell $tile_cells {
    set tile_feedback_count \
        [count_original_module_under $tile_cell mp64_fp32_feedback_rne]
    if {$tile_feedback_count > $max_feedback_lanes} {
        set max_feedback_lanes $tile_feedback_count
    }
}

set tacc_bank_bits \
    [count_named_primitives_under $tacc_cells "*bank_reg_reg*"]
set transfer_cells [original_module_cells mp64_tacc_transfer]
set stage_bits \
    [count_named_primitives_under $transfer_cells "*stage_image_reg*"]
set feedback_stage_bits \
    [count_named_primitives_under $tile_cells "*fp_tamac_*_reg*"]

set tacc_multiplier_cells 0
foreach root $tacc_cells {
    set root_name [get_property NAME $root]
    foreach cell [get_cells -hierarchical -quiet "${root_name}/*"] {
        set ref_name [get_property REF_NAME $cell]
        set original_ref [get_property -quiet ORIG_REF_NAME $cell]
        if {[regexp {^DSP48|MULT} $ref_name] ||
            [regexp {mp64_mul} $original_ref]} {
            incr tacc_multiplier_cells
        }
    }
}

set multiplier_sharing_ok [expr {$tacc_multiplier_cells == 0}]
set fp_adder_sharing_ok [expr {
    ($tacc_count == 0 && $feedback_count == 0) ||
    ($tacc_count > 0 && $feedback_count == (16 * $tile_count))
}]
set bounded_feedback_ok [expr {
    ($tacc_count == 0) ||
    ($feedback_count == (16 * $tile_count) && $feedback_stage_bits > 0)
}]

set structure_report [file join $TACC_BUILD_DIR tacc_structure.rpt]
set structure_stream [open $structure_report w]
puts $structure_stream \
    "TACC_STRUCTURAL tacc_specific_multiplier_arrays $tacc_multiplier_cells"
puts $structure_stream \
    "TACC_STRUCTURAL max_fp_feedback_lanes_per_engine $max_feedback_lanes"
puts $structure_stream \
    "TACC_STRUCTURAL persistent_tacc_bits $tacc_bank_bits"
puts $structure_stream \
    "TACC_STRUCTURAL shared_tacc_stage_bits $stage_bits"
puts $structure_stream \
    "TACC_STRUCTURAL tacc_bram_cells [expr {int(ceil($tacc_bram))}]"
puts $structure_stream \
    "TACC_STRUCTURAL multiplier_sharing_verified $multiplier_sharing_ok"
puts $structure_stream \
    "TACC_STRUCTURAL fp_adder_sharing_verified $fp_adder_sharing_ok"
puts $structure_stream \
    "TACC_STRUCTURAL bounded_feedback_path_verified $bounded_feedback_ok"
close $structure_stream

puts "TACC implementation reports written to $TACC_BUILD_DIR"
