// ============================================================================
// mp64_defs.vh — Megapad-64 shared constants & parameters
// ============================================================================
//
// Included by all RTL modules.  Defines the ISA encoding, memory map,
// MMIO offsets, tile engine parameters, and bus protocol constants.
//

`ifndef MP64_DEFS_VH
`define MP64_DEFS_VH

// ----------------------------------------------------------------------------
// System parameters
// ----------------------------------------------------------------------------
parameter CLOCK_HZ       = 100_000_000;   // 100 MHz system clock
parameter CELL            = 8;             // 8 bytes per Forth cell (64-bit)
parameter TILE_BYTES      = 64;            // 64-byte tile
parameter TILE_BITS       = 512;           // 64 × 8
parameter MAX_REGS        = 16;            // 16 GPRs

// ----------------------------------------------------------------------------
// Multi-core parameters
// ----------------------------------------------------------------------------
parameter NUM_CORES       = 4;             // Major (full) cores
parameter CORE_ID_BITS    = 4;             // 4-bit core IDs (0-15)

// Micro-cluster parameters
parameter NUM_CLUSTERS    = 3;             // Micro-core clusters
parameter MICRO_PER_CLUSTER = 4;           // Micro-cores per cluster
parameter NUM_MICRO_CORES = NUM_CLUSTERS * MICRO_PER_CLUSTER;  // 12 total
parameter NUM_ALL_CORES   = NUM_CORES + NUM_MICRO_CORES;       // 16 total

// Bus port topology: each full core = 1 port, each cluster = 1 port
parameter NUM_BUS_PORTS   = NUM_CORES + NUM_CLUSTERS;          // 7 bus ports
parameter BUS_PORT_BITS   = 3;             // ceil(log2(7))

// Micro-core global ID mapping:
//   Full cores:  0 – 3
//   Cluster 0:   4 – 7  (micro-cores 0-3)
//   Cluster 1:   8 – 11 (micro-cores 0-3)
//   Cluster 2:  12 – 15 (micro-cores 0-3)
parameter MICRO_ID_BASE   = NUM_CORES;     // first micro-core global ID

// Cluster scratchpad
parameter CLUSTER_SPAD_BYTES = 1024;       // 1 KiB per cluster
parameter CLUSTER_SPAD_DEPTH = CLUSTER_SPAD_BYTES / CELL;  // 128 × 64-bit

// Per-core stack region sizes (within 1 MiB shared BRAM)
// Layout: Core 0 boots from 0x0000, stacks at top of respective 64 KiB zone
//   Core 0: stack region 0xF0000–0xFFFFF  (boot core)
//   Core 1: stack region 0xE0000–0xEFFFF
//   Core 2: stack region 0xD0000–0xDFFFF
//   Core 3: stack region 0xC0000–0xCFFFF
// Micro-cores use cluster scratchpad or software-managed stack zones.
parameter CORE_STACK_SIZE = 65536;         // 64 KiB per major core
parameter CORE0_STACK_TOP = 20'hFFFFF;
parameter CORE1_STACK_TOP = 20'hEFFFF;
parameter CORE2_STACK_TOP = 20'hDFFFF;
parameter CORE3_STACK_TOP = 20'hCFFFF;

// ----------------------------------------------------------------------------
// Internal memory — 1 MiB BRAM, dual-port
// ----------------------------------------------------------------------------
parameter INT_MEM_BYTES   = 1048576;       // 1 MiB
parameter INT_MEM_DEPTH   = INT_MEM_BYTES / CELL;  // 131072 × 64-bit words
parameter INT_ADDR_BITS   = 20;            // log2(1 MiB)

// Tile port (Port A): 512-bit wide → 8192 rows × 512 bits
parameter TILE_PORT_WIDTH = 512;
parameter TILE_PORT_DEPTH = INT_MEM_BYTES / (TILE_PORT_WIDTH / 8);  // 16384
parameter TILE_ADDR_BITS  = 14;            // log2(16384)

// CPU port (Port B): 64-bit wide → 131072 rows × 64 bits
parameter CPU_PORT_WIDTH  = 64;
parameter CPU_PORT_DEPTH  = INT_MEM_BYTES / (CPU_PORT_WIDTH / 8);   // 131072
parameter CPU_ADDR_BITS   = 17;            // log2(131072)

// ----------------------------------------------------------------------------
// External memory
// ----------------------------------------------------------------------------
parameter EXT_MEM_MAX     = 32'h0100_0000; // Up to 16 MiB addressable
parameter EXT_BURST_LEN   = 8;             // 8-beat burst (64 bytes = 1 tile)

// ----------------------------------------------------------------------------
// Address map
// ----------------------------------------------------------------------------
// Internal RAM: 0x0000_0000 – 0x000F_FFFF  (1 MiB)
// External RAM: 0x0010_0000 – 0x00FF_FFFF  (up to 15 MiB)
// MMIO:         0xFFFF_FF00_0000_0000 – 0xFFFF_FF00_0000_04FF
//
// For FPGA, we decode the top 32 bits to distinguish MMIO:
//   if addr[63:32] == 0xFFFF_FF00 → MMIO
//   else → memory (internal if addr < INT_MEM_BYTES, else external)

parameter [63:0] MMIO_BASE = 64'hFFFF_FF00_0000_0000;
parameter [31:0] MMIO_HI   = 32'hFFFF_FF00;  // upper 32 bits for MMIO detect

// MMIO peripheral offsets (from MMIO_BASE, low 12 bits)
parameter [11:0] UART_BASE   = 12'h000;
parameter [11:0] TIMER_BASE  = 12'h100;
parameter [11:0] DISK_BASE   = 12'h200;
parameter [11:0] SYSINFO_BASE= 12'h300;
parameter [11:0] NIC_BASE    = 12'h400;
parameter [11:0] MBOX_BASE   = 12'h500;   // Inter-core mailbox
parameter [11:0] SPINLOCK_BASE = 12'h600; // Hardware spinlocks
parameter [11:0] AES_BASE     = 12'h700; // AES-256-GCM accelerator
parameter [11:0] SHA_BASE     = 12'h780; // SHA-3/SHAKE accelerator
parameter [11:0] CRC_BASE     = 12'h7C0; // CRC32/CRC64 accelerator
parameter [11:0] QOS_BASE     = 12'h7E0; // QoS global config
parameter [11:0] X25519_BASE  = 12'h840; // X25519 ECDH accelerator
parameter [11:0] FIELD_ALU_BASE = 12'h840; // Field ALU (supersedes X25519)
parameter [11:0] NTT_BASE      = 12'h8C0; // 256-point NTT accelerator
parameter [11:0] KEM_BASE      = 12'h900; // ML-KEM-512 accelerator

// UART registers (byte offsets from UART_BASE)
parameter [3:0] UART_TX      = 4'h0;
parameter [3:0] UART_RX      = 4'h1;
parameter [3:0] UART_STATUS  = 4'h2;
parameter [3:0] UART_CONTROL = 4'h3;

// Timer registers
parameter [3:0] TIMER_COUNT  = 4'h0;  // +0..+3 (32-bit)
parameter [3:0] TIMER_CMP    = 4'h4;  // +4..+7 (32-bit)
parameter [3:0] TIMER_CTRL   = 4'h8;
parameter [3:0] TIMER_STATUS = 4'h9;

// Storage registers
parameter [3:0] DISK_CMD     = 4'h0;
parameter [3:0] DISK_STATUS  = 4'h1;
parameter [3:0] DISK_SECTOR  = 4'h2;  // +2..+5 (32-bit)
parameter [3:0] DISK_DMA     = 4'h6;  // +6..+D (64-bit)
parameter [3:0] DISK_SECN    = 4'hE;
parameter [3:0] DISK_DATA    = 4'hF;

// NIC registers
parameter [3:0] NIC_CMD      = 4'h0;
parameter [3:0] NIC_STATUS   = 4'h1;
parameter [3:0] NIC_DMA      = 4'h2;  // +2..+9 (64-bit)
parameter [3:0] NIC_FRAMELEN = 4'hA;  // +A..+B (16-bit)
parameter [3:0] NIC_IRQCTRL  = 4'hC;
parameter [3:0] NIC_IRQSTAT  = 4'hD;

// Mailbox registers (offsets from MBOX_BASE)
// Each core has a 64-bit mailbox slot + doorbell + status
// Slot[n] = MBOX_BASE + n*16
parameter [3:0] MBOX_DATA_LO = 4'h0;     // +0: mailbox data low 32 bits
parameter [3:0] MBOX_DATA_HI = 4'h4;     // +4: mailbox data high 32 bits
parameter [3:0] MBOX_SEND    = 4'h8;     // +8: write target_core_id to send IPI
parameter [3:0] MBOX_STATUS  = 4'h9;     // +9: bit[n] = pending IPI from core n
parameter [3:0] MBOX_ACK     = 4'hA;     // +A: write core_id to clear pending bit

// Hardware spinlock registers (offsets from SPINLOCK_BASE)
// 8 hardware spinlocks, test-and-set
parameter NUM_SPINLOCKS    = 8;
parameter [3:0] SLOCK_ACQUIRE = 4'h0;    // read: returns 0=acquired, 1=busy
parameter [3:0] SLOCK_RELEASE = 4'h1;    // write: release lock
// Lock N at SPINLOCK_BASE + N*4

// ----------------------------------------------------------------------------
// ISA — Instruction families (upper nibble of opcode byte)
// ----------------------------------------------------------------------------
parameter [3:0] FAM_SYS   = 4'h0;
parameter [3:0] FAM_INC   = 4'h1;
parameter [3:0] FAM_DEC   = 4'h2;
parameter [3:0] FAM_BR    = 4'h3;
parameter [3:0] FAM_LBR   = 4'h4;
parameter [3:0] FAM_MEM   = 4'h5;
parameter [3:0] FAM_IMM   = 4'h6;
parameter [3:0] FAM_ALU   = 4'h7;
parameter [3:0] FAM_MEMALU= 4'h8;
parameter [3:0] FAM_IO    = 4'h9;
parameter [3:0] FAM_SEP   = 4'hA;
parameter [3:0] FAM_SEX   = 4'hB;
parameter [3:0] FAM_MULDIV= 4'hC;
parameter [3:0] FAM_CSR   = 4'hD;
parameter [3:0] FAM_MEX   = 4'hE;
parameter [3:0] FAM_EXT   = 4'hF;

// MEX tile engine sub-operations
parameter [1:0] MEX_TALU  = 2'b00;
parameter [1:0] MEX_TMUL  = 2'b01;
parameter [1:0] MEX_TRED  = 2'b10;
parameter [1:0] MEX_TSYS  = 2'b11;

// TALU functions
parameter [2:0] TALU_ADD  = 3'd0;
parameter [2:0] TALU_SUB  = 3'd1;
parameter [2:0] TALU_AND  = 3'd2;
parameter [2:0] TALU_OR   = 3'd3;
parameter [2:0] TALU_XOR  = 3'd4;
parameter [2:0] TALU_MIN  = 3'd5;
parameter [2:0] TALU_MAX  = 3'd6;
parameter [2:0] TALU_ABS  = 3'd7;

// Extended TALU functions (via EXT modifier 8)
parameter [2:0] ETALU_VSHR = 3'd0;
parameter [2:0] ETALU_VSHL = 3'd1;
parameter [2:0] ETALU_VSEL = 3'd2;
parameter [2:0] ETALU_VCLZ = 3'd3;

// TMUL functions
parameter [2:0] TMUL_MUL    = 3'd0;
parameter [2:0] TMUL_DOT    = 3'd1;
parameter [2:0] TMUL_WMUL   = 3'd2;
parameter [2:0] TMUL_MAC    = 3'd3;
parameter [2:0] TMUL_FMA    = 3'd4;
parameter [2:0] TMUL_DOTACC = 3'd5;

// EXT prefix modifier values
parameter [3:0] EXT_IMM64  = 4'd0;   // 64-bit immediate
parameter [3:0] EXT_SKIP   = 4'd6;   // Conditional skip
parameter [3:0] EXT_ETALU  = 4'd8;   // Extended tile ALU

// TRED functions
parameter [2:0] TRED_SUM  = 3'd0;
parameter [2:0] TRED_MIN  = 3'd1;
parameter [2:0] TRED_MAX  = 3'd2;
parameter [2:0] TRED_POPC = 3'd3;
parameter [2:0] TRED_L1   = 3'd4;
parameter [2:0] TRED_SUMSQ  = 3'd5;
parameter [2:0] TRED_MINIDX = 3'd6;
parameter [2:0] TRED_MAXIDX = 3'd7;

// TSYS functions
parameter [2:0] TSYS_TRANS   = 3'd0;   // 8×8 byte transpose
parameter [2:0] TSYS_SHUFFLE = 3'd1;   // Permute lanes by index tile
parameter [2:0] TSYS_MOVBANK = 3'd2;   // Tile copy
parameter [2:0] TSYS_LOADC   = 3'd3;   // Load from cursor
parameter [2:0] TSYS_ZERO    = 3'd4;   // Zero tile
parameter [2:0] TSYS_PACK    = 3'd5;   // Pack (narrow elements)
parameter [2:0] TSYS_UNPACK  = 3'd6;   // Unpack (widen elements)
parameter [2:0] TSYS_RROT    = 3'd7;   // Row/column rotate or mirror

// Extended TSYS functions (via EXT modifier 8)
parameter [2:0] ETSYS_LOAD2D  = 3'd0;   // Strided 2D tile gather
parameter [2:0] ETSYS_STORE2D = 3'd1;   // Strided 2D tile scatter

// TMODE extended bits
parameter TMODE_BIT_SIGNED   = 4;      // Bit 4: signed mode
parameter TMODE_BIT_SATURATE = 5;      // Bit 5: saturating arithmetic
parameter TMODE_BIT_ROUNDING = 6;      // Bit 6: rounding mode for shifts

// Tile modes (TMODE CSR bits [2:0] — 3-bit EW encoding)
parameter [2:0] TMODE_8    = 3'b000;   // 64 × 8-bit  lanes (u8/i8)
parameter [2:0] TMODE_16   = 3'b001;   // 32 × 16-bit lanes (u16/i16)
parameter [2:0] TMODE_32   = 3'b010;   // 16 × 32-bit lanes (u32/i32)
parameter [2:0] TMODE_64   = 3'b011;   //  8 × 64-bit lanes (u64/i64)
parameter [2:0] TMODE_FP16 = 3'b100;   // 32 × 16-bit lanes (IEEE 754 fp16)
parameter [2:0] TMODE_BF16 = 3'b101;   // 32 × 16-bit lanes (bfloat16)

// ----------------------------------------------------------------------------
// CSR addresses  (matches emulator megapad64.py numbering)
// ----------------------------------------------------------------------------
parameter [7:0] CSR_FLAGS    = 8'h00;   // Packed flags byte [S I G P V N C Z]
parameter [7:0] CSR_PSEL     = 8'h01;   // PC selector
parameter [7:0] CSR_XSEL     = 8'h02;   // X register selector
parameter [7:0] CSR_SPSEL    = 8'h03;   // SP selector
parameter [7:0] CSR_IVTBASE  = 8'h04;   // Interrupt vector table base
parameter [7:0] CSR_D        = 8'h05;   // Legacy 1802 D register (8-bit)
parameter [7:0] CSR_DF       = 8'h06;   // Legacy 1802 DF (carry alias)
parameter [7:0] CSR_QREG     = 8'h07;   // Legacy 1802 Q output flip-flop
parameter [7:0] CSR_TREG     = 8'h08;   // Legacy 1802 T register (saved X|P)
parameter [7:0] CSR_IE       = 8'h09;   // Interrupt enable (alias of flag_i)
parameter [7:0] CSR_SB       = 8'h10;   // Tile bank
parameter [7:0] CSR_SR       = 8'h11;   // Tile cursor row
parameter [7:0] CSR_SC       = 8'h12;   // Tile cursor col
parameter [7:0] CSR_SW       = 8'h13;   // Tile cursor stride
parameter [7:0] CSR_TMODE    = 8'h14;
parameter [7:0] CSR_TCTRL    = 8'h15;
parameter [7:0] CSR_TSRC0    = 8'h16;
parameter [7:0] CSR_TSRC1    = 8'h17;
parameter [7:0] CSR_TDST     = 8'h18;
parameter [7:0] CSR_ACC0     = 8'h19;
parameter [7:0] CSR_ACC1     = 8'h1A;
parameter [7:0] CSR_ACC2     = 8'h1B;
parameter [7:0] CSR_ACC3     = 8'h1C;

// Multi-core CSR addresses
parameter [7:0] CSR_COREID   = 8'h20;   // Read-only: core ID (0–3)
parameter [7:0] CSR_NCORES   = 8'h21;   // Read-only: number of cores
parameter [7:0] CSR_MBOX     = 8'h22;   // Read: pending IPI mask, Write: send IPI
parameter [7:0] CSR_IPIACK   = 8'h23;   // Write: acknowledge IPI from core N
parameter [7:0] CSR_IVEC_ID  = 8'h24;   // Current interrupt vector ID
parameter [7:0] CSR_TRAP_ADDR= 8'h25;   // Faulting address

// Strided / 2D tile addressing CSRs (§2.5)
parameter [7:0] CSR_TSTRIDE_R = 8'h40;  // Row stride in bytes
parameter [7:0] CSR_TSTRIDE_C = 8'h41;  // Column stride in bytes
parameter [7:0] CSR_TTILE_H   = 8'h42;  // Tile height (1–8)
parameter [7:0] CSR_TTILE_W   = 8'h43;  // Tile width (1–64)

parameter [7:0] CSR_MEGAPAD_SZ=8'h30;   // Memory size config (read-only)
parameter [7:0] CSR_CPUID    = 8'h31;   // CPU identification (read-only)

// DMA CSR addresses (per-core tile DMA engine)
parameter [7:0] CSR_DMA_RING_BASE = 8'h50;
parameter [7:0] CSR_DMA_RING_SIZE = 8'h51;
parameter [7:0] CSR_DMA_HEAD      = 8'h52;
parameter [7:0] CSR_DMA_TAIL      = 8'h53;
parameter [7:0] CSR_DMA_STATUS    = 8'h54;
parameter [7:0] CSR_DMA_CTRL      = 8'h55;

// QoS CSR addresses (per-core)
parameter [7:0] CSR_QOS_WEIGHT    = 8'h58;
parameter [7:0] CSR_QOS_BWLIMIT   = 8'h59;

// Performance counter CSR addresses
parameter [7:0] CSR_PERF_CYCLES  = 8'h68;  // Total clock cycles since reset
parameter [7:0] CSR_PERF_STALLS  = 8'h69;  // Stall cycles (bus/memory wait)
parameter [7:0] CSR_PERF_TILEOPS = 8'h6A;  // Tile engine operations completed
parameter [7:0] CSR_PERF_EXTMEM  = 8'h6B;  // External memory beats
parameter [7:0] CSR_PERF_CTRL    = 8'h6C;  // Bit 0: enable, Bit 1: reset all

// Memory BIST CSRs (§6.1)
parameter [7:0] CSR_BIST_CMD       = 8'h60;  // W: 0=idle, 1=full, 2=quick
parameter [7:0] CSR_BIST_STATUS    = 8'h61;  // R: 0=idle, 1=running, 2=pass, 3=fail
parameter [7:0] CSR_BIST_FAIL_ADDR = 8'h62;  // R: first failing address
parameter [7:0] CSR_BIST_FAIL_DATA = 8'h63;  // R: expected vs actual

// Tile Datapath Self-Test CSRs (§6.2)
parameter [7:0] CSR_TILE_SELFTEST  = 8'h64;  // W: 1=start; R: 0/1/2/3
parameter [7:0] CSR_TILE_ST_DETAIL = 8'h65;  // R: failed sub-test bitmask

// Instruction cache CSRs
parameter [7:0] CSR_ICACHE_CTRL    = 8'h70;  // W: bit0=enable, bit1=invalidate all
parameter [7:0] CSR_ICACHE_HITS    = 8'h71;  // R: hit counter (64-bit)
parameter [7:0] CSR_ICACHE_MISSES  = 8'h72;  // R: miss counter (64-bit)

// ----------------------------------------------------------------------------
// Bus protocol
// ----------------------------------------------------------------------------
// Internal bus: simple valid/ready handshake
//   master asserts bus_valid, bus_addr, bus_wdata, bus_wen, bus_size
//   slave  asserts bus_ready + bus_rdata when transaction completes
//
// bus_size: 0=byte, 1=halfword, 2=word, 3=doubleword

parameter [1:0] BUS_BYTE  = 2'd0;
parameter [1:0] BUS_HALF  = 2'd1;
parameter [1:0] BUS_WORD  = 2'd2;
parameter [1:0] BUS_DWORD = 2'd3;

// Tile bus: 512-bit dedicated path
//   tile_req, tile_addr (aligned to 64B), tile_wen
//   tile_rdata (512 bits), tile_wdata (512 bits), tile_ack

// ----------------------------------------------------------------------------
// Interrupt vectors
// ----------------------------------------------------------------------------
parameter [2:0] IRQ_RESET  = 3'd0;
parameter [2:0] IRQ_NMI    = 3'd1;
parameter [2:0] IRQ_ILLOP  = 3'd2;
parameter [2:0] IRQ_ALIGN  = 3'd3;
parameter [2:0] IRQ_DIVZ   = 3'd4;
parameter [2:0] IRQ_BUS    = 3'd5;
parameter [2:0] IRQ_SWTRAP = 3'd6;
parameter [2:0] IRQ_TIMER  = 3'd7;

// Extended interrupt sources (active on per-core irq_ext lines)
parameter [3:0] IRQX_IPI     = 4'd8;     // Inter-processor interrupt
parameter [3:0] IRQX_UART    = 4'd9;
parameter [3:0] IRQX_NIC     = 4'd10;
parameter [3:0] IRQX_DISK    = 4'd11;
parameter [3:0] IRQX_AES     = 4'd12;
parameter [3:0] IRQX_SHA     = 4'd13;
parameter [3:0] IRQX_DMA     = 4'd14;

// AES-256-GCM register offsets (from AES_BASE)
parameter [6:0] AES_KEY0     = 7'h00;  // ..0x1F: 256-bit key (8×32)
parameter [6:0] AES_IV0      = 7'h20;  // ..0x2B: 96-bit IV  (3×32)
parameter [6:0] AES_AAD_LEN  = 7'h30;
parameter [6:0] AES_DATA_LEN = 7'h34;
parameter [6:0] AES_CMD      = 7'h38;
parameter [6:0] AES_STATUS   = 7'h39;
parameter [6:0] AES_DIN      = 7'h40;  // ..0x4F: 128-bit data in
parameter [6:0] AES_DOUT     = 7'h50;  // ..0x5F: 128-bit data out
parameter [6:0] AES_TAG0     = 7'h60;  // ..0x6F: 128-bit GCM tag

// SHA-3 register offsets (from SHA_BASE)
parameter [5:0] SHA_CMD      = 6'h00;
parameter [5:0] SHA_STATUS   = 6'h01;
parameter [5:0] SHA_DIN      = 6'h10;  // 64-bit data in
parameter [5:0] SHA_DOUT     = 6'h20;  // 64-bit hash out
parameter [5:0] SHA_RATE     = 6'h28;  // read-only rate
parameter [5:0] SHA_CTRL     = 6'h29;

// CRC register offsets (from CRC_BASE)
parameter [4:0] CRC_POLY     = 5'h00;
parameter [4:0] CRC_INIT     = 5'h04;
parameter [4:0] CRC_DIN      = 5'h08;
parameter [4:0] CRC_RESULT   = 5'h10;
parameter [4:0] CRC_CTRL     = 5'h18;

// X25519 register offsets (from X25519_BASE)
// Write: 0x00..0x18 = scalar (4×64), 0x20..0x38 = point (4×64), 0x3F = CMD
// Read:  0x00 = STATUS, 0x08..0x20 = result (4×64)
parameter [5:0] X25519_SCALAR0 = 6'h00;
parameter [5:0] X25519_SCALAR1 = 6'h08;
parameter [5:0] X25519_SCALAR2 = 6'h10;
parameter [5:0] X25519_SCALAR3 = 6'h18;
parameter [5:0] X25519_POINT0  = 6'h20;
parameter [5:0] X25519_POINT1  = 6'h28;
parameter [5:0] X25519_POINT2  = 6'h30;
parameter [5:0] X25519_POINT3  = 6'h38;
parameter [5:0] X25519_CMD     = 6'h3F;
parameter [5:0] X25519_STATUS  = 6'h00;  // read: {done, busy}
parameter [5:0] X25519_RESULT0 = 6'h08;
parameter [5:0] X25519_RESULT1 = 6'h10;
parameter [5:0] X25519_RESULT2 = 6'h18;
parameter [5:0] X25519_RESULT3 = 6'h20;

// Field ALU register offsets (from FIELD_ALU_BASE = X25519_BASE = 0x840)
// Supersedes X25519: mode 0 = backward-compatible X25519 scalar multiply.
// Write: 0x00..0x18 = operand_a (4×64), 0x20..0x38 = operand_b (4×64), 0x3F = CMD
// Read:  0x00 = STATUS, 0x08..0x20 = RESULT (4×64, selected by result_sel)
// CMD bits: [0]=go, [4:1]=mode (0-7), [5]=result_sel (0=LO, 1=HI)
parameter [5:0] FALU_OPERAND_A0 = 6'h00;
parameter [5:0] FALU_OPERAND_A1 = 6'h08;
parameter [5:0] FALU_OPERAND_A2 = 6'h10;
parameter [5:0] FALU_OPERAND_A3 = 6'h18;
parameter [5:0] FALU_OPERAND_B0 = 6'h20;
parameter [5:0] FALU_OPERAND_B1 = 6'h28;
parameter [5:0] FALU_OPERAND_B2 = 6'h30;
parameter [5:0] FALU_OPERAND_B3 = 6'h38;
parameter [5:0] FALU_CMD        = 6'h3F;
parameter [5:0] FALU_STATUS     = 6'h00;  // read: {done, busy}
parameter [5:0] FALU_RESULT0    = 6'h08;
parameter [5:0] FALU_RESULT1    = 6'h10;
parameter [5:0] FALU_RESULT2    = 6'h18;
parameter [5:0] FALU_RESULT3    = 6'h20;

// NTT register offsets (from NTT_BASE = 0x8C0)
parameter [5:0] NTT_CMD         = 6'h00;  // W: bit[0]=go, bits[2:1]=op
parameter [5:0] NTT_Q           = 6'h08;  // RW: modulus (default 3329)
parameter [5:0] NTT_IDX         = 6'h10;  // RW: coefficient index (0-255)
parameter [5:0] NTT_LOAD_A      = 6'h18;  // W: wdata[31:0] → A[IDX]; IDX++
parameter [5:0] NTT_LOAD_B      = 6'h20;  // W: wdata[31:0] → B[IDX]; IDX++
parameter [5:0] NTT_STATUS      = 6'h00;  // R: {done, busy}
parameter [5:0] NTT_RESULT      = 6'h20;  // R: result[IDX]; IDX++

// KEM register offsets (from KEM_BASE = 0x900)
parameter [5:0] KEM_CMD         = 6'h00;  // W: 1=keygen, 2=encaps, 3=decaps
parameter [5:0] KEM_BUF_SEL     = 6'h08;  // W: buffer ID (0-4); resets idx
parameter [5:0] KEM_DIN         = 6'h10;  // W: wdata[7:0] → buf[sel][idx++]
parameter [5:0] KEM_IDX_SET     = 6'h18;  // W: wdata[15:0] → buf_idx
parameter [5:0] KEM_STATUS      = 6'h00;  // R: 0=idle, 1=busy, 2=done
parameter [5:0] KEM_DOUT        = 6'h10;  // R: buf[sel][idx++]
parameter [5:0] KEM_BUF_SIZE    = 6'h18;  // R: size of selected buffer
parameter [5:0] KEM_IDX         = 6'h20;  // R: current buf_idx

// IRQ routing: which core receives each peripheral IRQ (configurable)
// Default: core 0 gets all peripheral IRQs
parameter [1:0] IRQ_ROUTE_DEFAULT = 2'd0;

`endif // MP64_DEFS_VH
