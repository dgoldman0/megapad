# ============================================================================
# rtl/sim/Makefile — Phase 1, 2 & 3 testbench runner
# ============================================================================

IVERILOG ?= iverilog
VVP      ?= vvp
IVFLAGS  ?= -g2012

# Common include path
INC = -I../pkg

# Source directories
PRIM = ../prim
SOC  = ../soc
PLAT = ../platform/sim
BUS  = ../bus
MEM  = ../mem
CORE = ../core

# Primitive sources (shared)
PRIM_SRC = $(PRIM)/mp64_sram_sp.v \
           $(PRIM)/mp64_sram_dp.v \
           $(PRIM)/mp64_rom.v \
           $(PRIM)/mp64_clkgate.v \
           $(PRIM)/mp64_rst_sync.v \
           $(PRIM)/mp64_mul.v

# Core sources (Phase 3)
CORE_SRC = $(CORE)/mp64_alu.v \
           $(CORE)/mp64_icache.v \
           $(CORE)/mp64_cpu.v \
           $(CORE)/mp64_cpu_micro.v \
           $(CORE)/mp64_cluster.v

.PHONY: all clean phase1 phase2 phase3 \
        sram_sp sram_dp rom clkgate rst_sync platform_sim mul \
        bus_arbiter memory extmem \
        alu icache cpu_smoke cluster

all: phase1 phase2 phase3
	@echo ""
	@echo "============================================"
	@echo " All testbenches complete"
	@echo "============================================"

phase1: sram_sp sram_dp rom clkgate rst_sync platform_sim mul
	@echo ""
	@echo "--- Phase 1 complete ---"

phase2: bus_arbiter memory extmem
	@echo ""
	@echo "--- Phase 2 complete ---"

# --- Single-port SRAM ---
sram_sp: tb_sram_sp.vvp
	$(VVP) $<

tb_sram_sp.vvp: tb_sram_sp.v $(PRIM)/mp64_sram_sp.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

# --- Dual-port SRAM ---
sram_dp: tb_sram_dp.vvp
	$(VVP) $<

tb_sram_dp.vvp: tb_sram_dp.v $(PRIM)/mp64_sram_dp.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

# --- ROM ---
rom: tb_rom.vvp
	$(VVP) $<

tb_rom.vvp: tb_rom.v $(PRIM)/mp64_rom.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

# --- Clock gate ---
clkgate: tb_clkgate.vvp
	$(VVP) $<

tb_clkgate.vvp: tb_clkgate.v $(PRIM)/mp64_clkgate.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

# --- Reset synchroniser ---
rst_sync: tb_rst_sync.vvp
	$(VVP) $<

tb_rst_sync.vvp: tb_rst_sync.v $(PRIM)/mp64_rst_sync.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

# --- Platform sim (full stack) ---
platform_sim: tb_platform_sim.vvp
	$(VVP) $<

tb_platform_sim.vvp: tb_platform_sim.v $(PLAT)/mp64_platform_sim.v \
                     $(SOC)/mp64_top.v $(PRIM)/mp64_rst_sync.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

# --- 64-bit multiplier ---
mul: tb_mul.vvp
	$(VVP) $<

tb_mul.vvp: tb_mul.v $(PRIM)/mp64_mul.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

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
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

# --- Memory subsystem ---
memory: tb_memory.vvp
	$(VVP) $<

tb_memory.vvp: tb_memory.v $(MEM)/mp64_memory.v $(PRIM)/mp64_sram_dp.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

# --- External memory controller ---
extmem: tb_extmem.vvp
	$(VVP) $<

tb_extmem.vvp: tb_extmem.v $(MEM)/mp64_extmem.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

# ==========================================================================
# Phase 3: CPU Core, Micro-Core & Cluster
# ==========================================================================

phase3: alu icache cpu_smoke cluster
	@echo ""
	@echo "--- Phase 3 complete ---"

# --- ALU ---
alu: tb_alu.vvp
	$(VVP) $<

tb_alu.vvp: tb_alu.v $(CORE)/mp64_alu.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

# --- I-cache ---
icache: tb_icache.vvp
	$(VVP) $<

tb_icache.vvp: tb_icache.v $(CORE)/mp64_icache.v $(PRIM)/mp64_sram_sp.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

# --- CPU smoke test (major core, no I-cache) ---
cpu_smoke: tb_cpu_smoke.vvp
	$(VVP) $<

tb_cpu_smoke.vvp: tb_cpu_smoke.v $(CORE)/mp64_cpu.v $(CORE)/mp64_alu.v \
                  $(CORE)/mp64_icache.v $(PRIM)/mp64_sram_sp.v \
                  $(PRIM)/mp64_sram_dp.v $(PRIM)/mp64_mul.v \
                  $(MEM)/mp64_memory.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^

# --- Cluster (4 micro-cores + shared MUL/DIV) ---
cluster: tb_cluster.vvp
	$(VVP) $<

tb_cluster.vvp: tb_cluster.v $(CORE)/mp64_cluster.v $(CORE)/mp64_cpu_micro.v \
                $(CORE)/mp64_alu.v $(PRIM)/mp64_mul.v
	$(IVERILOG) $(IVFLAGS) $(INC) -o $@ $^
