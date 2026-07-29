## Common constraints for the three-build TACC physical comparison.
##
## This is deliberately not a board constraint file.  The comparison
## measures mp64_soc directly so all three source snapshots see the same
## internal 100 MHz clock and no board-wrapper or package-pin differences.

create_clock -name sys_clk -period 10.000 [get_ports sys_clk]

set nonclock_inputs [remove_from_collection [all_inputs] [get_ports sys_clk]]
if {[llength $nonclock_inputs] > 0} {
    set_false_path -from $nonclock_inputs
}

set design_outputs [all_outputs]
if {[llength $design_outputs] > 0} {
    set_false_path -to $design_outputs
}
