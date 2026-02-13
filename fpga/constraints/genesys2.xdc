## ============================================================================
## genesys2.xdc — Xilinx Constraints for Digilent Genesys 2
## ============================================================================
##
## Megapad-64 FPGA Prototype — Pin & Timing Constraints
##
## Target: Genesys 2 (xc7k325tffg900-2)
## Clock:  200 MHz on-board differential oscillator → MMCM → 100 MHz sys_clk
##
## Resources:
##   - 203,800 LUTs / 407,600 FFs
##   - 890 × 36Kb BRAM  = 4,005 KB
##   - 840 × DSP48E1
##   - DDR3 SODIMM (up to 1GB)
##   - Gigabit Ethernet PHY (Marvell 88E1111)
##   - USB-UART bridge (FT232R)
##   - micro-SD slot
##   - FMC-HPC connector
##

## ============================================================================
## Clock — 200 MHz LVDS oscillator
## ============================================================================
set_property -dict {PACKAGE_PIN AD12 IOSTANDARD LVDS} [get_ports sys_clk_p]
set_property -dict {PACKAGE_PIN AD11 IOSTANDARD LVDS} [get_ports sys_clk_n]
create_clock -period 5.000 -name sys_clk_200 [get_ports sys_clk_p]

## ============================================================================
## Reset — active low, directly from CPU_RESETN button
## ============================================================================
set_property -dict {PACKAGE_PIN R19 IOSTANDARD LVCMOS33} [get_ports sys_rst_n]

## ============================================================================
## UART — USB-UART bridge (directly via FT232R)
## ============================================================================
set_property -dict {PACKAGE_PIN Y23 IOSTANDARD LVCMOS33} [get_ports uart_txd]
set_property -dict {PACKAGE_PIN Y20 IOSTANDARD LVCMOS33} [get_ports uart_rxd]

## ============================================================================
## Debug LEDs — accent (LD0..LD7)
## ============================================================================
set_property -dict {PACKAGE_PIN T28  IOSTANDARD LVCMOS33} [get_ports {debug_leds[0]}]
set_property -dict {PACKAGE_PIN V19  IOSTANDARD LVCMOS33} [get_ports {debug_leds[1]}]
set_property -dict {PACKAGE_PIN U30  IOSTANDARD LVCMOS33} [get_ports {debug_leds[2]}]
set_property -dict {PACKAGE_PIN U29  IOSTANDARD LVCMOS33} [get_ports {debug_leds[3]}]
set_property -dict {PACKAGE_PIN V20  IOSTANDARD LVCMOS33} [get_ports {debug_leds[4]}]
set_property -dict {PACKAGE_PIN V26  IOSTANDARD LVCMOS33} [get_ports {debug_leds[5]}]
set_property -dict {PACKAGE_PIN W24  IOSTANDARD LVCMOS33} [get_ports {debug_leds[6]}]
set_property -dict {PACKAGE_PIN W23  IOSTANDARD LVCMOS33} [get_ports {debug_leds[7]}]

## ============================================================================
## SD Card — on-board micro-SD slot
## ============================================================================
set_property -dict {PACKAGE_PIN R28  IOSTANDARD LVCMOS33} [get_ports sd_sck]
set_property -dict {PACKAGE_PIN R29  IOSTANDARD LVCMOS33} [get_ports sd_mosi]
set_property -dict {PACKAGE_PIN R26  IOSTANDARD LVCMOS33} [get_ports sd_miso]
set_property -dict {PACKAGE_PIN T30  IOSTANDARD LVCMOS33} [get_ports sd_cs_n]

## ============================================================================
## External Memory PHY — directly tie off for synthesis test
## (DDR3 MIG core would replace this in production)
## ============================================================================
## phy_req, phy_addr, phy_wen, phy_wdata, phy_burst_len are outputs
## phy_rdata, phy_rvalid, phy_ready are inputs
## For synthesis-only, we leave these unconnected (handled in synth wrapper)

## ============================================================================
## NIC — placeholder (Pmod JA or FMC)
## ============================================================================
## NIC PHY signals mapped to Pmod JA header for bring-up
## Actual Ethernet: on-board Marvell 88E1111 PHY uses RGMII on dedicated pins
## (separate NIC PHY bridge module needed for production)
##
## set_property -dict {PACKAGE_PIN ... IOSTANDARD LVCMOS33} [get_ports nic_*]

## ============================================================================
## Timing Constraints
## ============================================================================

## MMCM output creates 100 MHz sys_clk domain
## (created by MMCM instantiation in wrapper — declared here for STA)
## create_generated_clock is auto-inferred by Vivado from MMCM primitive

## False paths for async inputs
set_false_path -from [get_ports uart_rxd]
set_false_path -from [get_ports sd_miso]
set_false_path -from [get_ports sys_rst_n]

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
## Kintex-7 325T has 4,005 KB of BRAM (890 × 36Kb blocks).
## 1 MiB internal RAM = 1024 KB ≈ 25% of available BRAM.
## Remaining ~2,981 KB available for I-cache, FIFOs, tile scratch, NIC buffers.
