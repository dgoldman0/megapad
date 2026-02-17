# ============================================================================
# rtl/sim/Makefile — Phase 1 testbench runner
# ============================================================================

IVERILOG ?= iverilog
VVP      ?= vvp

# Common include path
INC = -I../pkg

# Source directories
PRIM = ../prim
SOC  = ../soc
PLAT = ../platform/sim

# Primitive sources (shared)
PRIM_SRC = $(PRIM)/mp64_sram_sp.v \
           $(PRIM)/mp64_sram_dp.v \
           $(PRIM)/mp64_rom.v \
           $(PRIM)/mp64_clkgate.v \
           $(PRIM)/mp64_rst_sync.v

.PHONY: all clean sram_sp sram_dp rom clkgate rst_sync platform_sim

all: sram_sp sram_dp rom clkgate rst_sync platform_sim
	@echo ""
	@echo "============================================"
	@echo " Phase 1: All testbenches complete"
	@echo "============================================"

# --- Single-port SRAM ---
sram_sp: tb_sram_sp.vvp
	$(VVP) $<

tb_sram_sp.vvp: tb_sram_sp.v $(PRIM)/mp64_sram_sp.v
	$(IVERILOG) $(INC) -o $@ $^

# --- Dual-port SRAM ---
sram_dp: tb_sram_dp.vvp
	$(VVP) $<

tb_sram_dp.vvp: tb_sram_dp.v $(PRIM)/mp64_sram_dp.v
	$(IVERILOG) $(INC) -o $@ $^

# --- ROM ---
rom: tb_rom.vvp
	$(VVP) $<

tb_rom.vvp: tb_rom.v $(PRIM)/mp64_rom.v
	$(IVERILOG) $(INC) -o $@ $^

# --- Clock gate ---
clkgate: tb_clkgate.vvp
	$(VVP) $<

tb_clkgate.vvp: tb_clkgate.v $(PRIM)/mp64_clkgate.v
	$(IVERILOG) $(INC) -o $@ $^

# --- Reset synchroniser ---
rst_sync: tb_rst_sync.vvp
	$(VVP) $<

tb_rst_sync.vvp: tb_rst_sync.v $(PRIM)/mp64_rst_sync.v
	$(IVERILOG) $(INC) -o $@ $^

# --- Platform sim (full stack) ---
platform_sim: tb_platform_sim.vvp
	$(VVP) $<

tb_platform_sim.vvp: tb_platform_sim.v $(PLAT)/mp64_platform_sim.v \
                     $(SOC)/mp64_top.v $(PRIM)/mp64_rst_sync.v
	$(IVERILOG) $(INC) -o $@ $^

# --- Clean ---
clean:
	rm -f *.vvp *.vcd
