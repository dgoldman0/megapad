// ============================================================================
// ASIC Target Stubs — Megapad-64
// ============================================================================
//
// Placeholder wrappers for ASIC synthesis.  Each module has the same port
// interface as its rtl/prim/ counterpart but the body is left empty,
// ready for foundry-specific macro instantiation.
//
// To use:  include this directory's files instead of rtl/prim/ at
// synthesis time, then fill in vendor macro instantiations.
//
// Files:
//   mp64_mul_asic.v       — multiplier    (replace with DesignWare DW02_mult)
//   mp64_sram_sp_asic.v   — single-port SRAM (replace with SRAM compiler macro)
//   mp64_sram_dp_asic.v   — dual-port SRAM
//   mp64_clkgate_asic.v   — clock gate    (replace with ICG cell)
//   mp64_pll_asic.v       — PLL           (replace with analog PLL macro)
//   mp64_rst_sync_asic.v  — reset sync    (generic is usually fine)
//
