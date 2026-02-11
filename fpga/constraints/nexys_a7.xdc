## ============================================================================
## nexys_a7.xdc — Xilinx Constraints for Digilent Nexys A7-200T
## ============================================================================
##
## Megapad-64 FPGA Prototype — Pin & Timing Constraints
##
## Target: Nexys A7 (xc7a200tsbg484-1)
## Clock:  100 MHz on-board oscillator (E3)
##

## ============================================================================
## Clock
## ============================================================================
set_property -dict {PACKAGE_PIN E3 IOSTANDARD LVCMOS33} [get_ports sys_clk]
create_clock -period 10.000 -name sys_clk [get_ports sys_clk]

## ============================================================================
## Reset — active low, directly from CPU_RESETN button
## ============================================================================
set_property -dict {PACKAGE_PIN C12 IOSTANDARD LVCMOS33} [get_ports sys_rst_n]

## ============================================================================
## UART (directly via USB-UART bridge on board)
## ============================================================================
set_property -dict {PACKAGE_PIN D4 IOSTANDARD LVCMOS33} [get_ports uart_txd]
set_property -dict {PACKAGE_PIN C4 IOSTANDARD LVCMOS33} [get_ports uart_rxd]

## ============================================================================
## Debug LEDs (accent — LD0..LD7)
## ============================================================================
set_property -dict {PACKAGE_PIN H17 IOSTANDARD LVCMOS33} [get_ports {debug_leds[0]}]
set_property -dict {PACKAGE_PIN K15 IOSTANDARD LVCMOS33} [get_ports {debug_leds[1]}]
set_property -dict {PACKAGE_PIN J13 IOSTANDARD LVCMOS33} [get_ports {debug_leds[2]}]
set_property -dict {PACKAGE_PIN N14 IOSTANDARD LVCMOS33} [get_ports {debug_leds[3]}]
set_property -dict {PACKAGE_PIN R18 IOSTANDARD LVCMOS33} [get_ports {debug_leds[4]}]
set_property -dict {PACKAGE_PIN V17 IOSTANDARD LVCMOS33} [get_ports {debug_leds[5]}]
set_property -dict {PACKAGE_PIN U17 IOSTANDARD LVCMOS33} [get_ports {debug_leds[6]}]
set_property -dict {PACKAGE_PIN U16 IOSTANDARD LVCMOS33} [get_ports {debug_leds[7]}]

## ============================================================================
## SD Card (directly via Pmod JA connector or on-board micro-SD)
## ============================================================================
## On-board micro-SD slot:
set_property -dict {PACKAGE_PIN B1 IOSTANDARD LVCMOS33} [get_ports sd_sck]
set_property -dict {PACKAGE_PIN A1 IOSTANDARD LVCMOS33} [get_ports sd_mosi]
set_property -dict {PACKAGE_PIN C1 IOSTANDARD LVCMOS33} [get_ports sd_miso]
set_property -dict {PACKAGE_PIN D2 IOSTANDARD LVCMOS33} [get_ports sd_cs_n]

## ============================================================================
## External Memory — Pmod JB (directly HyperRAM PMOD or similar)
## ============================================================================
## These are placeholder pins — actual assignment depends on the
## specific HyperRAM PMOD or daughter board used.
##
## JB header (directly usable for a HyperRAM PMOD):
##   JB1=A14, JB2=A16, JB3=B15, JB4=B16, JB7=A15, JB8=A17, JB9=C15, JB10=C16
##
## set_property -dict {PACKAGE_PIN A14 IOSTANDARD LVCMOS33} [get_ports phy_*]
## (Uncomment and assign when specific PHY module is chosen)

## ============================================================================
## NIC — Pmod JC (directly usable for Ethernet PMOD or SPI NIC)
## ============================================================================
## Placeholder for future NIC PHY wiring.
## JC header: E15, E16, D15, C15, J17, J18, K15, J15
##
## set_property -dict {PACKAGE_PIN E15 IOSTANDARD LVCMOS33} [get_ports nic_*]

## ============================================================================
## Timing Constraints
## ============================================================================

## All I/O operates in sys_clk domain — no multi-cycle paths yet.
## SD SPI is sys_clk/4 = 25 MHz, well within LVCMOS33 switching.

## False paths for async inputs
set_false_path -from [get_ports uart_rxd]
set_false_path -from [get_ports sd_miso]

## ============================================================================
## Bitstream Configuration
## ============================================================================
set_property BITSTREAM.GENERAL.COMPRESS TRUE [current_design]
set_property BITSTREAM.CONFIG.SPI_BUSWIDTH 4 [current_design]
set_property CONFIG_MODE SPIx4 [current_design]
set_property BITSTREAM.CONFIG.CONFIGRATE 33 [current_design]

## ============================================================================
## BRAM Utilisation Note
## ============================================================================
## Artix-7 200T has 1620 KB of BRAM.
## 1 MiB internal RAM = 1024 KB ≈ 63% of available BRAM.
## Remaining ~596 KB available for FIFOs, tile scratch, etc.
