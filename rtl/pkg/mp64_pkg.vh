// ============================================================================
// mp64_pkg.vh — Megapad-64 Portable Parameter Package
// ============================================================================
//
// Technology-agnostic constants shared by all RTL modules.
//
// Contents:
//   §1  System-level defaults (overridable via module parameters)
//   §2  ISA instruction families & encodings
//   §3  ALU / CPU FSM shared encodings
//   §4  Tile engine encodings (MEX sub-ops, TMODE, reductions)
//   §5  CSR address map
//   §6  Memory & address map
//   §7  MMIO peripheral offsets & register maps
//   §8  Bus protocol constants
//   §9  Interrupt vectors
//   §10 Crypto / accelerator register maps
//
// Coding rules:
//   - `localparam` for architecture constants (ISA, CSR, MMIO layout)
//   - System tunables (clock freq, core count, memory depth) are passed
//     as module parameters — default values listed here for reference only.
//
// Verilog-2001.  No vendor primitives.
//

`ifndef MP64_PKG_VH
`define MP64_PKG_VH

// ============================================================================
// §1 — System-Level Defaults
// ============================================================================
// These are *reference* values.  Actual values flow as parameters from
// mp64_top → mp64_soc → each sub-module.

localparam MP64_CLOCK_HZ_DEFAULT   = 100_000_000;  // 100 MHz
localparam MP64_CELL               = 8;             // 8 bytes per cell (64-bit)
localparam MP64_TILE_BYTES         = 64;            // 64-byte tile
localparam MP64_TILE_BITS          = 512;           // 64 × 8
localparam MP64_MAX_REGS           = 16;            // 16 GPRs

// Multi-core defaults
localparam MP64_NUM_CORES_DEFAULT       = 4;
localparam MP64_CORE_ID_BITS            = 5;        // 5-bit core IDs (0–31)
localparam MP64_NUM_CLUSTERS_DEFAULT    = 3;
localparam MP64_MICRO_PER_CLUSTER       = 4;
// Derived (at default counts):
//   NUM_MICRO_CORES = NUM_CLUSTERS × MICRO_PER_CLUSTER = 12
//   NUM_ALL_CORES   = NUM_CORES + NUM_MICRO_CORES = 16
//   NUM_BUS_PORTS   = NUM_CORES + NUM_CLUSTERS = 7

// Cluster scratchpad
localparam MP64_CLUSTER_SPAD_BYTES = 1024;          // 1 KiB per cluster
localparam MP64_CLUSTER_SPAD_DEPTH = 128;           // 128 × 64-bit words
// Scratchpad address detection: addr[63:32] == SPAD_HI
localparam [31:0] MP64_SPAD_HI = 32'hFFFF_FE00;

// Per-core stack region layout (within 1 MiB Bank 0)
//   Core 0: 0xF0000–0xFFFFF (boot core)
//   Core 1: 0xE0000–0xEFFFF
//   Core 2: 0xD0000–0xDFFFF
//   Core 3: 0xC0000–0xCFFFF
localparam MP64_CORE_STACK_SIZE = 65536;            // 64 KiB per major core
localparam [19:0] MP64_CORE0_STACK_TOP = 20'hFFFFF;
localparam [19:0] MP64_CORE1_STACK_TOP = 20'hEFFFF;
localparam [19:0] MP64_CORE2_STACK_TOP = 20'hDFFFF;
localparam [19:0] MP64_CORE3_STACK_TOP = 20'hCFFFF;

// ============================================================================
// §2 — ISA Instruction Families (upper nibble of opcode byte)
// ============================================================================
localparam [3:0] FAM_SYS    = 4'h0;
localparam [3:0] FAM_INC    = 4'h1;
localparam [3:0] FAM_DEC    = 4'h2;
localparam [3:0] FAM_BR     = 4'h3;
localparam [3:0] FAM_LBR    = 4'h4;
localparam [3:0] FAM_MEM    = 4'h5;
localparam [3:0] FAM_IMM    = 4'h6;
localparam [3:0] FAM_ALU    = 4'h7;
localparam [3:0] FAM_MEMALU = 4'h8;
localparam [3:0] FAM_IO     = 4'h9;
localparam [3:0] FAM_SEP    = 4'hA;
localparam [3:0] FAM_SEX    = 4'hB;
localparam [3:0] FAM_MULDIV = 4'hC;
localparam [3:0] FAM_CSR    = 4'hD;
localparam [3:0] FAM_MEX    = 4'hE;
localparam [3:0] FAM_EXT    = 4'hF;

// EXT prefix modifier values
localparam [3:0] EXT_IMM64  = 4'd0;     // 64-bit immediate follows
localparam [3:0] EXT_SKIP   = 4'd6;     // Conditional skip
localparam [3:0] EXT_ETALU  = 4'd8;     // Extended tile ALU

// ============================================================================
// §3 — ALU & CPU FSM Shared Encodings
// ============================================================================

// --- CPU FSM states (4-bit) ---
localparam [3:0] CPU_FETCH      = 4'd0;
localparam [3:0] CPU_DECODE     = 4'd1;
localparam [3:0] CPU_EXECUTE    = 4'd2;
localparam [3:0] CPU_MEM_READ   = 4'd3;
localparam [3:0] CPU_MEM_WRITE  = 4'd4;
localparam [3:0] CPU_MEX_WAIT   = 4'd5;  // major core only
localparam [3:0] CPU_IRQ        = 4'd6;
localparam [3:0] CPU_HALT       = 4'd7;
localparam [3:0] CPU_FETCH_MORE = 4'd8;
localparam [3:0] CPU_MULDIV     = 4'd9;  // major core only
localparam [3:0] CPU_MEM_READ2  = 4'd10; // RTI flags pop
localparam [3:0] CPU_IRQ_PUSH   = 4'd11; // push flags in IRQ sequence
localparam [3:0] CPU_IRQ_LOAD   = 4'd12; // load IVT vector from memory
localparam [3:0] CPU_MEMALU_RD  = 4'd13; // MEMALU: reading M(R(X))
localparam [3:0] CPU_MEMALU_WB  = 4'd14; // reserved
localparam [3:0] CPU_SKIP       = 4'd15; // SKIP: fetch next for length

// --- ALU operation codes (4-bit) ---
localparam [3:0] ALU_ADD = 4'd0;
localparam [3:0] ALU_SUB = 4'd1;
localparam [3:0] ALU_AND = 4'd2;
localparam [3:0] ALU_OR  = 4'd3;
localparam [3:0] ALU_XOR = 4'd4;
localparam [3:0] ALU_MOV = 4'd5;
localparam [3:0] ALU_NOT = 4'd6;
localparam [3:0] ALU_NEG = 4'd7;
localparam [3:0] ALU_SHL = 4'd8;
localparam [3:0] ALU_SHR = 4'd9;
localparam [3:0] ALU_SAR = 4'd10;
localparam [3:0] ALU_CMP = 4'd11;
localparam [3:0] ALU_ADC = 4'd12;
localparam [3:0] ALU_SBB = 4'd13;
localparam [3:0] ALU_ROL = 4'd14;
localparam [3:0] ALU_ROR = 4'd15;

// --- Post-action codes (multi-cycle ops) ---
localparam [2:0] POST_NONE     = 3'd0;
localparam [2:0] POST_RTI_POP2 = 3'd1;  // after popping PC, pop flags
localparam [2:0] POST_IRQ_VEC  = 3'd2;  // after pushing, load IVT vector

// ============================================================================
// §4 — Tile Engine (MEX) Encodings
// ============================================================================

// MEX sub-operation (bits [3:2] of MEX opcode)
localparam [1:0] MEX_TALU = 2'b00;
localparam [1:0] MEX_TMUL = 2'b01;
localparam [1:0] MEX_TRED = 2'b10;
localparam [1:0] MEX_TSYS = 2'b11;

// TALU functions (bits [2:0])
localparam [2:0] TALU_ADD = 3'd0;
localparam [2:0] TALU_SUB = 3'd1;
localparam [2:0] TALU_AND = 3'd2;
localparam [2:0] TALU_OR  = 3'd3;
localparam [2:0] TALU_XOR = 3'd4;
localparam [2:0] TALU_MIN = 3'd5;
localparam [2:0] TALU_MAX = 3'd6;
localparam [2:0] TALU_ABS = 3'd7;

// Extended TALU (via EXT_ETALU modifier)
localparam [2:0] ETALU_VSHR = 3'd0;
localparam [2:0] ETALU_VSHL = 3'd1;
localparam [2:0] ETALU_VSEL = 3'd2;
localparam [2:0] ETALU_VCLZ = 3'd3;

// TMUL functions
localparam [2:0] TMUL_MUL    = 3'd0;
localparam [2:0] TMUL_DOT    = 3'd1;
localparam [2:0] TMUL_WMUL   = 3'd2;
localparam [2:0] TMUL_MAC    = 3'd3;
localparam [2:0] TMUL_FMA    = 3'd4;
localparam [2:0] TMUL_DOTACC = 3'd5;

// TRED functions
localparam [2:0] TRED_SUM    = 3'd0;
localparam [2:0] TRED_MIN    = 3'd1;
localparam [2:0] TRED_MAX    = 3'd2;
localparam [2:0] TRED_POPC   = 3'd3;
localparam [2:0] TRED_L1     = 3'd4;
localparam [2:0] TRED_SUMSQ  = 3'd5;
localparam [2:0] TRED_MINIDX = 3'd6;
localparam [2:0] TRED_MAXIDX = 3'd7;

// TSYS functions
localparam [2:0] TSYS_TRANS   = 3'd0;  // 8×8 byte transpose
localparam [2:0] TSYS_SHUFFLE = 3'd1;  // Permute lanes by index tile
localparam [2:0] TSYS_MOVBANK = 3'd2;  // Tile copy
localparam [2:0] TSYS_LOADC   = 3'd3;  // Load from cursor
localparam [2:0] TSYS_ZERO    = 3'd4;  // Zero tile
localparam [2:0] TSYS_PACK    = 3'd5;  // Pack (narrow)
localparam [2:0] TSYS_UNPACK  = 3'd6;  // Unpack (widen)
localparam [2:0] TSYS_RROT    = 3'd7;  // Row/col rotate or mirror

// Extended TSYS (via EXT_ETALU modifier)
localparam [2:0] ETSYS_LOAD2D  = 3'd0;  // Strided 2D tile gather
localparam [2:0] ETSYS_STORE2D = 3'd1;  // Strided 2D tile scatter

// TMODE CSR bit positions
localparam TMODE_BIT_SIGNED   = 4;
localparam TMODE_BIT_SATURATE = 5;
localparam TMODE_BIT_ROUNDING = 6;

// TMODE element width encoding (bits [2:0])
localparam [2:0] TMODE_8    = 3'b000;  // 64 × 8-bit lanes
localparam [2:0] TMODE_16   = 3'b001;  // 32 × 16-bit lanes
localparam [2:0] TMODE_32   = 3'b010;  // 16 × 32-bit lanes
localparam [2:0] TMODE_64   = 3'b011;  //  8 × 64-bit lanes
localparam [2:0] TMODE_FP16 = 3'b100;  // 32 × FP16 lanes
localparam [2:0] TMODE_BF16 = 3'b101;  // 32 × BF16 lanes

// ============================================================================
// §5 — CSR Address Map
// ============================================================================

// Core control CSRs
localparam [7:0] CSR_FLAGS     = 8'h00;  // [S I G P V N C Z]
localparam [7:0] CSR_PSEL      = 8'h01;  // PC selector
localparam [7:0] CSR_XSEL      = 8'h02;  // X register selector
localparam [7:0] CSR_SPSEL     = 8'h03;  // SP selector
localparam [7:0] CSR_IVTBASE   = 8'h04;  // IVT base address
localparam [7:0] CSR_D         = 8'h05;  // Legacy 1802 D (8-bit)
localparam [7:0] CSR_DF        = 8'h06;  // Legacy 1802 DF (carry alias)
localparam [7:0] CSR_QREG      = 8'h07;  // Legacy 1802 Q flip-flop
localparam [7:0] CSR_TREG      = 8'h08;  // Legacy 1802 T (saved X|P)
localparam [7:0] CSR_IE        = 8'h09;  // Interrupt enable
localparam [7:0] CSR_PRIV      = 8'h0A;  // 0=supervisor, 1=user
localparam [7:0] CSR_MPU_BASE  = 8'h0B;  // MPU lower bound
localparam [7:0] CSR_MPU_LIMIT = 8'h0C;  // MPU upper bound

// Tile engine CSRs
localparam [7:0] CSR_SB        = 8'h10;  // Tile bank
localparam [7:0] CSR_SR        = 8'h11;  // Tile cursor row
localparam [7:0] CSR_SC        = 8'h12;  // Tile cursor col
localparam [7:0] CSR_SW        = 8'h13;  // Tile cursor stride
localparam [7:0] CSR_TMODE     = 8'h14;
localparam [7:0] CSR_TCTRL     = 8'h15;
localparam [7:0] CSR_TSRC0     = 8'h16;
localparam [7:0] CSR_TSRC1     = 8'h17;
localparam [7:0] CSR_TDST      = 8'h18;
localparam [7:0] CSR_ACC0      = 8'h19;
localparam [7:0] CSR_ACC1      = 8'h1A;
localparam [7:0] CSR_ACC2      = 8'h1B;
localparam [7:0] CSR_ACC3      = 8'h1C;

// Multi-core CSRs
localparam [7:0] CSR_COREID    = 8'h20;  // RO: core ID
localparam [7:0] CSR_NCORES    = 8'h21;  // RO: total cores
localparam [7:0] CSR_MBOX      = 8'h22;  // IPI mask / send
localparam [7:0] CSR_IPIACK    = 8'h23;  // Acknowledge IPI
localparam [7:0] CSR_IVEC_ID   = 8'h24;  // Current IRQ vector ID
localparam [7:0] CSR_TRAP_ADDR = 8'h25;  // Faulting address

// Memory size / CPU ID
localparam [7:0] CSR_MEGAPAD_SZ = 8'h30;
localparam [7:0] CSR_CPUID      = 8'h31;

// Strided / 2D tile addressing CSRs
localparam [7:0] CSR_TSTRIDE_R = 8'h40;
localparam [7:0] CSR_TSTRIDE_C = 8'h41;
localparam [7:0] CSR_TTILE_H   = 8'h42;
localparam [7:0] CSR_TTILE_W   = 8'h43;

// DMA CSRs
localparam [7:0] CSR_DMA_RING_BASE = 8'h50;
localparam [7:0] CSR_DMA_RING_SIZE = 8'h51;
localparam [7:0] CSR_DMA_HEAD      = 8'h52;
localparam [7:0] CSR_DMA_TAIL      = 8'h53;
localparam [7:0] CSR_DMA_STATUS    = 8'h54;
localparam [7:0] CSR_DMA_CTRL      = 8'h55;

// QoS CSRs
localparam [7:0] CSR_QOS_WEIGHT    = 8'h58;
localparam [7:0] CSR_QOS_BWLIMIT   = 8'h59;

// Memory BIST CSRs
localparam [7:0] CSR_BIST_CMD       = 8'h60;
localparam [7:0] CSR_BIST_STATUS    = 8'h61;
localparam [7:0] CSR_BIST_FAIL_ADDR = 8'h62;
localparam [7:0] CSR_BIST_FAIL_DATA = 8'h63;

// Tile datapath self-test CSRs
localparam [7:0] CSR_TILE_SELFTEST  = 8'h64;
localparam [7:0] CSR_TILE_ST_DETAIL = 8'h65;

// Performance counters
localparam [7:0] CSR_PERF_CYCLES   = 8'h68;
localparam [7:0] CSR_PERF_STALLS   = 8'h69;
localparam [7:0] CSR_PERF_TILEOPS  = 8'h6A;
localparam [7:0] CSR_PERF_EXTMEM   = 8'h6B;
localparam [7:0] CSR_PERF_CTRL     = 8'h6C;

// Cluster-level CSRs
localparam [7:0] CSR_CL_PRIV          = 8'h6D;
localparam [7:0] CSR_CL_MPU_BASE      = 8'h6E;
localparam [7:0] CSR_CL_MPU_LIMIT     = 8'h6F;
localparam [7:0] CSR_CL_IVTBASE       = 8'h73;
localparam [7:0] CSR_BARRIER_ARRIVE   = 8'h74;
localparam [7:0] CSR_BARRIER_STATUS   = 8'h75;

// I-cache CSRs
localparam [7:0] CSR_ICACHE_CTRL   = 8'h70;
localparam [7:0] CSR_ICACHE_HITS   = 8'h71;
localparam [7:0] CSR_ICACHE_MISSES = 8'h72;

// ============================================================================
// §6 — Memory & Address Map
// ============================================================================

// Physical address width
localparam MP64_PHYS_ADDR_BITS = 32;

// Internal memory defaults (overridable as module params)
localparam MP64_BANK_SIZE_DEFAULT  = 1048576;     // 1 MiB per bank
localparam MP64_BANK_ADDR_BITS     = 20;          // log2(1 MiB)
localparam MP64_NUM_MEM_BANKS      = 4;           // 1 system + 3 HBW
localparam MP64_HBW_BANKS          = 3;

// Tile port geometry
localparam MP64_TILE_PORT_WIDTH = 512;
localparam MP64_TILE_ADDR_BITS  = 14;             // log2(16384) rows

// CPU port geometry
localparam MP64_CPU_PORT_WIDTH  = 64;
localparam MP64_CPU_ADDR_BITS   = 17;             // log2(131072) words

// HBW bank base addresses
localparam [31:0] MP64_HBW_BASE_ADDR = 32'hFFD0_0000;
// Bank 1: 0xFFD0_0000 – 0xFFDF_FFFF
// Bank 2: 0xFFE0_0000 – 0xFFEF_FFFF
// Bank 3: 0xFFF0_0000 – 0xFFFF_FFFF

// External memory limit
localparam [31:0] MP64_EXT_MEM_MAX = MP64_HBW_BASE_ADDR;
localparam MP64_EXT_BURST_LEN     = 8;            // 8-beat = 64 bytes

// MMIO
localparam [63:0] MP64_MMIO_BASE = 64'hFFFF_FF00_0000_0000;
localparam [31:0] MP64_MMIO_HI   = 32'hFFFF_FF00;

// ============================================================================
// §7 — MMIO Peripheral Offsets & Register Maps
// ============================================================================

// Peripheral base offsets (low 12 bits from MMIO_BASE)
localparam [11:0] UART_BASE       = 12'h000;
localparam [11:0] TIMER_BASE      = 12'h100;
localparam [11:0] DISK_BASE       = 12'h200;
localparam [11:0] SYSINFO_BASE    = 12'h300;
localparam [11:0] NIC_BASE        = 12'h400;
localparam [11:0] MBOX_BASE       = 12'h500;
localparam [11:0] SPINLOCK_BASE   = 12'h600;
localparam [11:0] AES_BASE        = 12'h700;
localparam [11:0] SHA_BASE        = 12'h780;
localparam [11:0] CRC_BASE        = 12'h7C0;
localparam [11:0] QOS_BASE        = 12'h7E0;
localparam [11:0] FIELD_ALU_BASE  = 12'h840;
localparam [11:0] NTT_BASE        = 12'h8C0;
localparam [11:0] KEM_BASE        = 12'h900;

// --- UART registers ---
localparam [3:0] UART_TX      = 4'h0;
localparam [3:0] UART_RX      = 4'h1;
localparam [3:0] UART_STATUS  = 4'h2;
localparam [3:0] UART_CONTROL = 4'h3;

// --- Timer registers ---
localparam [3:0] TIMER_COUNT  = 4'h0;
localparam [3:0] TIMER_CMP    = 4'h4;
localparam [3:0] TIMER_CTRL   = 4'h8;
localparam [3:0] TIMER_STATUS = 4'h9;

// --- Disk registers ---
localparam [3:0] DISK_CMD     = 4'h0;
localparam [3:0] DISK_STATUS  = 4'h1;
localparam [3:0] DISK_SECTOR  = 4'h2;
localparam [3:0] DISK_DMA     = 4'h6;
localparam [3:0] DISK_SECN    = 4'hE;
localparam [3:0] DISK_DATA    = 4'hF;

// --- NIC registers ---
localparam [3:0] NIC_CMD      = 4'h0;
localparam [3:0] NIC_STATUS   = 4'h1;
localparam [3:0] NIC_DMA      = 4'h2;
localparam [3:0] NIC_FRAMELEN = 4'hA;
localparam [3:0] NIC_IRQCTRL  = 4'hC;
localparam [3:0] NIC_IRQSTAT  = 4'hD;

// --- Mailbox registers ---
localparam [3:0] MBOX_DATA_LO = 4'h0;
localparam [3:0] MBOX_DATA_HI = 4'h4;
localparam [3:0] MBOX_SEND    = 4'h8;
localparam [3:0] MBOX_STATUS  = 4'h9;
localparam [3:0] MBOX_ACK     = 4'hA;

// --- Hardware spinlocks ---
localparam MP64_NUM_SPINLOCKS  = 8;
localparam [3:0] SLOCK_ACQUIRE = 4'h0;
localparam [3:0] SLOCK_RELEASE = 4'h1;

// ============================================================================
// §8 — Bus Protocol
// ============================================================================

localparam [1:0] BUS_BYTE  = 2'd0;
localparam [1:0] BUS_HALF  = 2'd1;
localparam [1:0] BUS_WORD  = 2'd2;
localparam [1:0] BUS_DWORD = 2'd3;

// ============================================================================
// §9 — Interrupt Vectors
// ============================================================================

localparam [2:0] IRQ_RESET  = 3'd0;
localparam [2:0] IRQ_NMI    = 3'd1;
localparam [2:0] IRQ_ILLOP  = 3'd2;
localparam [2:0] IRQ_ALIGN  = 3'd3;
localparam [2:0] IRQ_DIVZ   = 3'd4;
localparam [2:0] IRQ_BUS    = 3'd5;
localparam [2:0] IRQ_SWTRAP = 3'd6;
localparam [2:0] IRQ_TIMER  = 3'd7;

localparam [7:0] IRQX_ILLEGAL_OP = 8'd4;
localparam [7:0] IRQX_SW_TRAP    = 8'd6;

localparam [3:0] IRQX_IPI   = 4'd8;
localparam [3:0] IRQX_UART  = 4'd9;
localparam [3:0] IRQX_NIC   = 4'd10;
localparam [3:0] IRQX_DISK  = 4'd11;
localparam [3:0] IRQX_AES   = 4'd12;
localparam [3:0] IRQX_SHA   = 4'd13;
localparam [3:0] IRQX_DMA   = 4'd14;
localparam [3:0] IRQX_PRIV  = 4'd15;

localparam [1:0] IRQ_ROUTE_DEFAULT = 2'd0;

// ============================================================================
// §10 — Crypto & Accelerator Register Maps
// ============================================================================

// --- AES-256-GCM ---
localparam [6:0] AES_KEY0     = 7'h00;
localparam [6:0] AES_IV0      = 7'h20;
localparam [6:0] AES_AAD_LEN  = 7'h30;
localparam [6:0] AES_DATA_LEN = 7'h34;
localparam [6:0] AES_CMD      = 7'h38;
localparam [6:0] AES_STATUS   = 7'h39;
localparam [6:0] AES_DIN      = 7'h40;
localparam [6:0] AES_DOUT     = 7'h50;
localparam [6:0] AES_TAG0     = 7'h60;

// --- SHA-3 ---
localparam [5:0] SHA_CMD      = 6'h00;
localparam [5:0] SHA_STATUS   = 6'h01;
localparam [5:0] SHA_DIN      = 6'h10;
localparam [5:0] SHA_DOUT     = 6'h20;
localparam [5:0] SHA_RATE     = 6'h28;
localparam [5:0] SHA_CTRL     = 6'h29;

// --- CRC ---
localparam [4:0] CRC_POLY     = 5'h00;
localparam [4:0] CRC_INIT     = 5'h04;
localparam [4:0] CRC_DIN      = 5'h08;
localparam [4:0] CRC_RESULT   = 5'h10;
localparam [4:0] CRC_CTRL     = 5'h18;

// --- Field ALU (GF(2^255-19)) ---
localparam [5:0] FALU_OPERAND_A0 = 6'h00;
localparam [5:0] FALU_OPERAND_A1 = 6'h08;
localparam [5:0] FALU_OPERAND_A2 = 6'h10;
localparam [5:0] FALU_OPERAND_A3 = 6'h18;
localparam [5:0] FALU_OPERAND_B0 = 6'h20;
localparam [5:0] FALU_OPERAND_B1 = 6'h28;
localparam [5:0] FALU_OPERAND_B2 = 6'h30;
localparam [5:0] FALU_OPERAND_B3 = 6'h38;
localparam [5:0] FALU_CMD        = 6'h3F;
localparam [5:0] FALU_STATUS     = 6'h00;
localparam [5:0] FALU_RESULT0    = 6'h08;
localparam [5:0] FALU_RESULT1    = 6'h10;
localparam [5:0] FALU_RESULT2    = 6'h18;
localparam [5:0] FALU_RESULT3    = 6'h20;

// --- NTT (256-point) ---
localparam [5:0] NTT_CMD    = 6'h00;
localparam [5:0] NTT_Q      = 6'h08;
localparam [5:0] NTT_IDX    = 6'h10;
localparam [5:0] NTT_LOAD_A = 6'h18;
localparam [5:0] NTT_LOAD_B = 6'h20;
localparam [5:0] NTT_STATUS = 6'h00;
localparam [5:0] NTT_RESULT = 6'h20;

// --- ML-KEM-512 ---
localparam [5:0] KEM_CMD      = 6'h00;
localparam [5:0] KEM_BUF_SEL  = 6'h08;
localparam [5:0] KEM_DIN      = 6'h10;
localparam [5:0] KEM_IDX_SET  = 6'h18;
localparam [5:0] KEM_STATUS   = 6'h00;
localparam [5:0] KEM_DOUT     = 6'h10;
localparam [5:0] KEM_BUF_SIZE = 6'h18;
localparam [5:0] KEM_IDX      = 6'h20;

// ============================================================================
// §11 — Utility Functions (included inside module bodies)
// ============================================================================
// NOTE: Functions cannot be in a `include header at file scope in
// Verilog-2001.  These are placed here so that modules including this
// package get them automatically.  They MUST be `included inside the
// module body.
//
// Condition code evaluator — used by BR/LBR/SKIP
//
//   cond[3:0]:
//     0 AL   1 EQ   2 NE   3 CS   4 CC   5 MI   6 PL   7 VS
//     8 VC   9 GT   A LE   B BQ   C BNQ  D SAT  E EF   F NV
//

// To use: `include "mp64_pkg.vh" at file scope, then
// `include "mp64_cpu_funcs.vh" inside the module body.
// (Verilog-2001 does not allow functions outside modules.)

`endif // MP64_PKG_VH
