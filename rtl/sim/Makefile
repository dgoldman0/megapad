# ============================================================================
# rtl/sim/Makefile — Phase 1 & 2 testbench runner
# ============================================================================

IVERILOG ?= iverilog
VVP      ?= vvp

# Common include path
INC = -I../pkg

# Source directories
PRIM = ../prim
SOC  = ../soc
PLAT = ../platform/sim
BUS  = ../bus
MEM  = ../mem

# Primitive sources (shared)
PRIM_SRC = $(PRIM)/mp64_sram_sp.v \
           $(PRIM)/mp64_sram_dp.v \
           $(PRIM)/mp64_rom.v \
           $(PRIM)/mp64_clkgate.v \
           $(PRIM)/mp64_rst_sync.v

.PHONY: all clean phase1 phase2 \
        sram_sp sram_dp rom clkgate rst_sync platform_sim \
        bus_arbiter memory extmem

all: phase1 phase2
	@echo ""
	@echo "============================================"
	@echo " All testbenches complete"
	@echo "============================================"

phase1: sram_sp sram_dp rom clkgate rst_sync platform_sim
	@echo ""
	@echo "--- Phase 1 complete ---"

phase2: bus_arbiter memory extmem
	@echo ""
	@echo "--- Phase 2 complete ---"

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

# ==========================================================================
# Phase 2: Bus & Memory
# ==========================================================================

# --- Bus arbiter ---
bus_arbiter: tb_bus_arbiter.vvp
	$(VVP) $<

tb_bus_arbiter.vvp: tb_bus_arbiter.v $(BUS)/mp64_bus.v
	$(IVERILOG) $(INC) -o $@ $^

# --- Memory subsystem ---
memory: tb_memory.vvp
	$(VVP) $<

tb_memory.vvp: tb_memory.v $(MEM)/mp64_memory.v $(PRIM)/mp64_sram_dp.v
	$(IVERILOG) $(INC) -o $@ $^

# --- External memory controller ---
extmem: tb_extmem.vvp
	$(VVP) $<

tb_extmem.vvp: tb_extmem.v $(MEM)/mp64_extmem.v
	$(IVERILOG) $(INC) -o $@ $^
