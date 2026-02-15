// ============================================================================
// mp64_kem.v — ML-KEM-512 (Kyber) Key Encapsulation Mechanism Accelerator
// ============================================================================
//
// Hardware KEM accelerator providing key generation, encapsulation, and
// decapsulation via MMIO-accessible internal buffers.  Orchestrates an
// internal NTT unit and SHA-3 absorb/squeeze operations to implement the
// full ML-KEM-512 specification (FIPS 203).
//
// Buffer layout (internal BRAM):
//   ID  Name   Size    Description
//   0   SEED   64 B    d‖z (keygen) or coin (encaps)
//   1   PK     800 B   Encapsulation key  (ek)
//   2   SK     1632 B  Decapsulation key   (dk)
//   3   CT     768 B   Ciphertext
//   4   SS     32 B    Shared secret
//
// For this initial RTL version the crypto core is a structural skeleton:
// the register interface, buffer management, FSM, and bus protocol are
// fully implemented and synthesisable.  The actual polynomial / hash
// operations are represented as multi-cycle stubs that produce
// deterministic (but non-cryptographic) output for simulation and
// interface verification.  The stub will be replaced with a real
// ML-KEM datapath in phase 2.
//
// Target: Kintex-7 / Artix-7.
// Resource estimate:
//   ~4 BRAM18K  (3296 B buffer storage)
//   ~500 FFs    (FSM, counters, control)
//   ~300 LUTs   (address decode, muxing)
//
// MMIO base: 0x900 (64-byte block, addr[5:0]).
//
// Register map (64-bit bus, addressed by addr[5:3]):
//   ── Write ──────────────────────────────────────────────────────
//   0x00  CMD       wdata[2:0]=op  (0=nop, 1=keygen, 2=encaps, 3=decaps)
//   0x08  BUF_SEL   wdata[2:0]=buffer ID (0–4), resets buf_idx to 0
//   0x10  DIN       wdata[7:0]=data byte → buf[sel][idx]; idx++
//   0x18  IDX_SET   wdata[15:0] → buf_idx  (random-access positioning)
//
//   ── Read ───────────────────────────────────────────────────────
//   0x00  STATUS    {56'd0, status[7:0]}  (0=idle, 1=busy, 2=done)
//   0x08  BUF_SEL   {61'd0, buf_sel[2:0]}
//   0x10  DOUT      {56'd0, buf[sel][idx]}; idx++
//   0x18  BUF_SIZE  {48'd0, size[15:0]}  — size of selected buffer
//   0x20  IDX       {48'd0, buf_idx[15:0]}
//
// ============================================================================

`include "mp64_defs.vh"

module mp64_kem (
    input  wire        clk,
    input  wire        rst_n,

    // MMIO bus interface
    input  wire        req,
    input  wire [5:0]  addr,
    input  wire [63:0] wdata,
    input  wire        wen,
    output reg  [63:0] rdata,
    output reg         ack
);

    // ========================================================================
    // Buffer size constants
    // ========================================================================
    localparam BUF_SEED_SZ = 64;
    localparam BUF_PK_SZ   = 800;
    localparam BUF_SK_SZ   = 1632;
    localparam BUF_CT_SZ   = 768;
    localparam BUF_SS_SZ   = 32;
    localparam BUF_TOTAL   = BUF_SEED_SZ + BUF_PK_SZ + BUF_SK_SZ
                           + BUF_CT_SZ + BUF_SS_SZ;  // 3296

    // Buffer base offsets within unified memory
    localparam BUF_SEED_BASE = 0;
    localparam BUF_PK_BASE   = BUF_SEED_SZ;                    // 64
    localparam BUF_SK_BASE   = BUF_PK_BASE + BUF_PK_SZ;        // 864
    localparam BUF_CT_BASE   = BUF_SK_BASE + BUF_SK_SZ;        // 2496
    localparam BUF_SS_BASE   = BUF_CT_BASE + BUF_CT_SZ;        // 3264

    // ========================================================================
    // State machine
    // ========================================================================
    localparam S_IDLE    = 3'd0,
               S_KEYGEN  = 3'd1,
               S_ENCAPS  = 3'd2,
               S_DECAPS  = 3'd3,
               S_DONE    = 3'd4;

    reg [2:0]  state;
    reg [7:0]  status_reg;   // 0=idle, 1=busy, 2=done

    // ========================================================================
    // Buffer memory — unified byte-addressable array
    // ========================================================================
    reg [7:0] buf_mem [0:BUF_TOTAL-1];

    // ========================================================================
    // Control registers
    // ========================================================================
    reg [2:0]  buf_sel;       // selected buffer (0–4)
    reg [15:0] buf_idx;       // byte index within selected buffer
    reg [15:0] op_cnt;        // multi-cycle operation counter
    reg        dout_read;     // flag: DOUT was read, advance idx next cycle

    // ========================================================================
    // Buffer geometry look-up (combinational)
    // ========================================================================
    function [15:0] buf_base;
        input [2:0] sel;
        begin
            case (sel)
                3'd0: buf_base = BUF_SEED_BASE;
                3'd1: buf_base = BUF_PK_BASE;
                3'd2: buf_base = BUF_SK_BASE;
                3'd3: buf_base = BUF_CT_BASE;
                3'd4: buf_base = BUF_SS_BASE;
                default: buf_base = 16'd0;
            endcase
        end
    endfunction

    function [15:0] buf_size;
        input [2:0] sel;
        begin
            case (sel)
                3'd0: buf_size = BUF_SEED_SZ;
                3'd1: buf_size = BUF_PK_SZ;
                3'd2: buf_size = BUF_SK_SZ;
                3'd3: buf_size = BUF_CT_SZ;
                3'd4: buf_size = BUF_SS_SZ;
                default: buf_size = 16'd0;
            endcase
        end
    endfunction

    // Absolute address in buf_mem for the current selection + index
    wire [15:0] buf_abs_addr = buf_base(buf_sel) + buf_idx;

    // ========================================================================
    // Main sequential logic
    // ========================================================================
    integer k;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state      <= S_IDLE;
            status_reg <= 8'd0;
            buf_sel    <= 3'd0;
            buf_idx    <= 16'd0;
            op_cnt     <= 16'd0;
            dout_read  <= 1'b0;
            for (k = 0; k < BUF_TOTAL; k = k + 1)
                buf_mem[k] <= 8'd0;
        end else begin

            // Deferred DOUT index advance (from read handler)
            if (dout_read) begin
                buf_idx   <= buf_idx + 16'd1;
                dout_read <= 1'b0;
            end

            // ================================================================
            // MMIO writes
            // ================================================================
            if (req && wen) begin
                case (addr[5:3])
                    3'b000: begin  // CMD
                        if (state == S_IDLE || state == S_DONE) begin
                            case (wdata[2:0])
                                3'd1: begin  // KEYGEN
                                    status_reg <= 8'd1;
                                    op_cnt     <= 16'd0;
                                    state      <= S_KEYGEN;
                                end
                                3'd2: begin  // ENCAPS
                                    status_reg <= 8'd1;
                                    op_cnt     <= 16'd0;
                                    state      <= S_ENCAPS;
                                end
                                3'd3: begin  // DECAPS
                                    status_reg <= 8'd1;
                                    op_cnt     <= 16'd0;
                                    state      <= S_DECAPS;
                                end
                                default: ;   // nop
                            endcase
                        end
                    end
                    3'b001: begin  // BUF_SEL
                        buf_sel <= (wdata[2:0] > 3'd4) ? 3'd4 : wdata[2:0];
                        buf_idx <= 16'd0;
                    end
                    3'b010: begin  // DIN
                        if (buf_idx < buf_size(buf_sel))
                            buf_mem[buf_abs_addr] <= wdata[7:0];
                        buf_idx <= buf_idx + 16'd1;
                    end
                    3'b011: begin  // IDX_SET
                        buf_idx <= wdata[15:0];
                    end
                    default: ;
                endcase
            end

            // ================================================================
            // State machine — crypto operations
            //
            // STUB IMPLEMENTATION: deterministic fill for simulation.
            // Each operation runs for a fixed number of cycles to model
            // realistic latency, then fills the appropriate output buffers
            // with seed-derived deterministic data.
            //
            // Phase 2 will replace these stubs with actual ML-KEM-512
            // polynomial arithmetic (NTT, CBD, compress/decompress, hash).
            // ================================================================
            case (state)

                // ------------------------------------------------------------
                // S_KEYGEN — generate (ek, dk) from seed
                //
                // Stub: 256-cycle latency, fills PK and SK with
                //       XOR-scrambled seed bytes for deterministic output.
                // ------------------------------------------------------------
                S_KEYGEN: begin
                    if (op_cnt < 16'd256) begin
                        // Fill PK buffer: rotating XOR of seed
                        if (op_cnt < BUF_PK_SZ) begin
                            buf_mem[BUF_PK_BASE + op_cnt] <=
                                buf_mem[BUF_SEED_BASE + (op_cnt % BUF_SEED_SZ)]
                                ^ op_cnt[7:0];
                        end
                        // Fill SK buffer: different scramble
                        if (op_cnt < BUF_SK_SZ) begin
                            buf_mem[BUF_SK_BASE + op_cnt] <=
                                buf_mem[BUF_SEED_BASE + (op_cnt % BUF_SEED_SZ)]
                                ^ ~op_cnt[7:0];
                        end
                        op_cnt <= op_cnt + 16'd1;
                    end else begin
                        status_reg <= 8'd2;
                        state      <= S_DONE;
                    end
                end

                // ------------------------------------------------------------
                // S_ENCAPS — encapsulate: (PK, coin) → (CT, SS)
                //
                // Stub: 256-cycle latency, fills CT and SS.
                // ------------------------------------------------------------
                S_ENCAPS: begin
                    if (op_cnt < 16'd256) begin
                        // Fill CT
                        if (op_cnt < BUF_CT_SZ) begin
                            buf_mem[BUF_CT_BASE + op_cnt] <=
                                buf_mem[BUF_PK_BASE + (op_cnt % BUF_PK_SZ)]
                                ^ buf_mem[BUF_SEED_BASE + (op_cnt % BUF_SEED_SZ)]
                                ^ op_cnt[7:0];
                        end
                        // Fill SS
                        if (op_cnt < BUF_SS_SZ) begin
                            buf_mem[BUF_SS_BASE + op_cnt] <=
                                buf_mem[BUF_SEED_BASE + op_cnt]
                                ^ buf_mem[BUF_PK_BASE + op_cnt]
                                ^ 8'hA5;
                        end
                        op_cnt <= op_cnt + 16'd1;
                    end else begin
                        status_reg <= 8'd2;
                        state      <= S_DONE;
                    end
                end

                // ------------------------------------------------------------
                // S_DECAPS — decapsulate: (CT, SK) → SS
                //
                // Stub: 256-cycle latency, fills SS.
                // ------------------------------------------------------------
                S_DECAPS: begin
                    if (op_cnt < 16'd256) begin
                        if (op_cnt < BUF_SS_SZ) begin
                            buf_mem[BUF_SS_BASE + op_cnt] <=
                                buf_mem[BUF_CT_BASE + op_cnt]
                                ^ buf_mem[BUF_SK_BASE + op_cnt]
                                ^ 8'h5A;
                        end
                        op_cnt <= op_cnt + 16'd1;
                    end else begin
                        status_reg <= 8'd2;
                        state      <= S_DONE;
                    end
                end

                S_DONE: begin
                    // Wait for next CMD (handled in write section)
                end

            endcase
        end
    end

    // ========================================================================
    // MMIO read handler
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            rdata <= 64'd0;
            ack   <= 1'b0;
        end else begin
            ack <= 1'b0;
            if (req) begin
                ack <= 1'b1;
                if (!wen) begin
                    case (addr[5:3])
                        3'b000: rdata <= {56'd0, status_reg};         // STATUS
                        3'b001: rdata <= {61'd0, buf_sel};            // BUF_SEL
                        3'b010: begin                                 // DOUT
                            if (buf_idx < buf_size(buf_sel))
                                rdata <= {56'd0, buf_mem[buf_abs_addr]};
                            else
                                rdata <= 64'd0;
                            dout_read <= 1'b1;  // advance idx next cycle
                        end
                        3'b011: rdata <= {48'd0, buf_size(buf_sel)};  // BUF_SIZE
                        3'b100: rdata <= {48'd0, buf_idx};            // IDX
                        default: rdata <= 64'd0;
                    endcase
                end else begin
                    rdata <= 64'd0;
                end
            end
        end
    end

endmodule
