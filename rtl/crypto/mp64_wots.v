// ============================================================================
// mp64_wots.v — WOTS+ Chain Accelerator
// ============================================================================
//
// Computes an entire WOTS+ hash chain in hardware, iterating SHAKE-256
// internally without per-step CPU intervention.  Eliminates ~82% of
// Forth dispatch overhead for SPHINCS+ signing.
//
// MMIO base: 0x8A0 (32 bytes, within SoC MMIO space).
//
// Register map:
//   +0x00  WOTS_SEED   (W, 32b)  RAM address of PK.seed (16 bytes)
//   +0x04  WOTS_ADRS   (W, 32b)  RAM address of ADRS (32 bytes)
//   +0x08  WOTS_INPUT  (W, 32b)  RAM address of input (16 bytes)
//   +0x0C  WOTS_STEPS  (W, 8b)   Chain length (1–15)
//   +0x0D  WOTS_START  (W, 8b)   Start step index (0–14)
//   +0x0E  WOTS_GO     (W, 8b)   Write any value → begin chain
//   +0x0E  WOTS_STATUS (R, 8b)   0=idle, 1=busy, 2=done
//   +0x0F  WOTS_CYCLES (R, 8b)   Cycle count of last chain (÷64)
//   +0x10  WOTS_DOUT   (R, 16B)  Result bytes [0..15]
//
// DMA read port: reads 64 bytes from RAM at chain start (PK.seed 16B +
// ADRS 32B + input 16B).  Read-only, lowest bus priority.
//
// SHA3 engine: driven directly via internal port signals, NOT through
// the MMIO bus.  The SHA3 MMIO path returns busy while WOTS is active.
//

`include "mp64_pkg.vh"

module mp64_wots (
    input  wire        clk,
    input  wire        rst_n,

    // MMIO interface (byte-addressed within 32-byte window)
    input  wire        req,
    input  wire [4:0]  addr,       // byte offset 0x00–0x1F
    input  wire [63:0] wdata,
    input  wire        wen,
    output reg  [63:0] rdata,
    output reg         ack,

    // DMA read port — to bus arbiter
    output reg         dma_req,
    output reg  [31:0] dma_addr,
    input  wire [7:0]  dma_rdata,
    input  wire        dma_ack,

    // SHA3 engine direct interface
    output reg         sha3_cmd_valid,
    output reg  [2:0]  sha3_cmd,        // 1=INIT, 3=FINAL
    output reg         sha3_din_valid,
    output reg  [7:0]  sha3_din,
    input  wire [7:0]  sha3_dout,       // digest bytes
    input  wire        sha3_ready,      // engine idle
    output reg         sha3_mode_wr,
    output reg  [1:0]  sha3_mode_val,   // 3 = SHAKE-256

    // Status
    output wire        active,          // blocks SHA3 MMIO access
    output reg         irq_done        // pulse on chain completion
);

    // ========================================================================
    // FSM states
    // ========================================================================
    localparam [3:0] S_IDLE         = 4'd0;
    localparam [3:0] S_DMA_SEED     = 4'd1;
    localparam [3:0] S_DMA_ADRS     = 4'd2;
    localparam [3:0] S_DMA_INPUT    = 4'd3;
    localparam [3:0] S_SHA3_INIT    = 4'd4;
    localparam [3:0] S_ABSORB_SEED  = 4'd5;
    localparam [3:0] S_ABSORB_ADRS  = 4'd6;
    localparam [3:0] S_ABSORB_BUF   = 4'd7;
    localparam [3:0] S_FINALIZE     = 4'd8;
    localparam [3:0] S_WAIT_KECCAK  = 4'd9;
    localparam [3:0] S_SQUEEZE      = 4'd10;
    localparam [3:0] S_STEP_CHECK   = 4'd11;
    localparam [3:0] S_DONE         = 4'd12;

    reg [3:0] state, state_next;

    // ========================================================================
    // Configuration registers (written by CPU via MMIO)
    // ========================================================================
    reg [31:0] seed_addr;      // RAM address of PK.seed
    reg [31:0] adrs_addr;      // RAM address of ADRS
    reg [31:0] input_addr;     // RAM address of input
    reg [3:0]  step_limit;     // chain length (1–15)
    reg [3:0]  step_start;     // start step index

    // ========================================================================
    // Internal working registers
    // ========================================================================
    reg [7:0]  seed_reg  [0:15];  // PK.seed (16 bytes, loaded once)
    reg [7:0]  adrs_reg  [0:31];  // ADRS (32 bytes, hash field mutated)
    reg [7:0]  buf_reg   [0:15];  // chain buffer (16 bytes, fed back)
    reg [7:0]  dout_reg  [0:15];  // output latch
    reg [3:0]  step_count;        // current step within chain
    reg [1:0]  status;            // 0=idle, 1=busy, 2=done
    reg [6:0]  dma_byte_idx;      // byte counter for DMA reads
    reg [5:0]  absorb_idx;        // byte counter for SHA3 absorb
    reg [3:0]  squeeze_idx;       // byte counter for squeeze readout
    reg [15:0] cycle_counter;     // total cycles for profiling

    assign active = (status == 2'd1);

    // ========================================================================
    // MMIO read/write
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            seed_addr  <= 32'd0;
            adrs_addr  <= 32'd0;
            input_addr <= 32'd0;
            step_limit <= 4'd0;
            step_start <= 4'd0;
            ack        <= 1'b0;
            rdata      <= 64'd0;
        end else begin
            ack <= 1'b0;
            if (req) begin
                ack <= 1'b1;
                if (wen) begin
                    // Writes
                    case (addr)
                        5'h00: seed_addr  <= wdata[31:0];
                        5'h04: adrs_addr  <= wdata[31:0];
                        5'h08: input_addr <= wdata[31:0];
                        5'h0C: step_limit <= wdata[3:0];
                        5'h0D: step_start <= wdata[3:0];
                        // 5'h0E: WOTS_GO handled in FSM below
                        default: ;
                    endcase
                end else begin
                    // Reads
                    case (addr)
                        5'h0E: rdata <= {56'd0, 6'd0, status};
                        5'h0F: rdata <= {56'd0, cycle_counter[15:8]};
                        // DOUT bytes at +0x10..+0x1F
                        5'h10: rdata <= {56'd0, dout_reg[0]};
                        5'h11: rdata <= {56'd0, dout_reg[1]};
                        5'h12: rdata <= {56'd0, dout_reg[2]};
                        5'h13: rdata <= {56'd0, dout_reg[3]};
                        5'h14: rdata <= {56'd0, dout_reg[4]};
                        5'h15: rdata <= {56'd0, dout_reg[5]};
                        5'h16: rdata <= {56'd0, dout_reg[6]};
                        5'h17: rdata <= {56'd0, dout_reg[7]};
                        5'h18: rdata <= {56'd0, dout_reg[8]};
                        5'h19: rdata <= {56'd0, dout_reg[9]};
                        5'h1A: rdata <= {56'd0, dout_reg[10]};
                        5'h1B: rdata <= {56'd0, dout_reg[11]};
                        5'h1C: rdata <= {56'd0, dout_reg[12]};
                        5'h1D: rdata <= {56'd0, dout_reg[13]};
                        5'h1E: rdata <= {56'd0, dout_reg[14]};
                        5'h1F: rdata <= {56'd0, dout_reg[15]};
                        default: rdata <= 64'd0;
                    endcase
                end
            end
        end
    end

    // ========================================================================
    // Main FSM
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state         <= S_IDLE;
            status        <= 2'd0;
            dma_req       <= 1'b0;
            dma_addr      <= 32'd0;
            dma_byte_idx  <= 7'd0;
            absorb_idx    <= 6'd0;
            squeeze_idx   <= 4'd0;
            step_count    <= 4'd0;
            cycle_counter <= 16'd0;
            sha3_cmd_valid <= 1'b0;
            sha3_cmd       <= 3'd0;
            sha3_din_valid <= 1'b0;
            sha3_din       <= 8'd0;
            sha3_mode_wr   <= 1'b0;
            sha3_mode_val  <= 2'd0;
            irq_done       <= 1'b0;
        end else begin
            // Default: deassert single-cycle pulses
            sha3_cmd_valid <= 1'b0;
            sha3_din_valid <= 1'b0;
            sha3_mode_wr   <= 1'b0;
            irq_done       <= 1'b0;

            // Cycle counter (runs while busy)
            if (status == 2'd1)
                cycle_counter <= cycle_counter + 16'd1;

            case (state)

            // ----------------------------------------------------------------
            // IDLE: wait for WOTS_GO write
            // ----------------------------------------------------------------
            S_IDLE: begin
                if (req && wen && addr == 5'h0E) begin
                    // GO trigger
                    status        <= 2'd1;
                    step_count    <= step_start;
                    dma_byte_idx  <= 7'd0;
                    cycle_counter <= 16'd0;
                    dma_req       <= 1'b1;
                    dma_addr      <= seed_addr;
                    state         <= S_DMA_SEED;
                end
            end

            // ----------------------------------------------------------------
            // DMA_SEED: read 16 bytes of PK.seed from RAM
            // ----------------------------------------------------------------
            S_DMA_SEED: begin
                if (dma_ack) begin
                    seed_reg[dma_byte_idx[3:0]] <= dma_rdata;
                    if (dma_byte_idx == 7'd15) begin
                        // Done with seed, start ADRS
                        dma_byte_idx <= 7'd0;
                        dma_addr     <= adrs_addr;
                        state        <= S_DMA_ADRS;
                    end else begin
                        dma_byte_idx <= dma_byte_idx + 7'd1;
                        dma_addr     <= dma_addr + 32'd1;
                    end
                end
            end

            // ----------------------------------------------------------------
            // DMA_ADRS: read 32 bytes of ADRS from RAM
            // ----------------------------------------------------------------
            S_DMA_ADRS: begin
                if (dma_ack) begin
                    adrs_reg[dma_byte_idx[4:0]] <= dma_rdata;
                    if (dma_byte_idx == 7'd31) begin
                        // Done with ADRS, start input
                        dma_byte_idx <= 7'd0;
                        dma_addr     <= input_addr;
                        state        <= S_DMA_INPUT;
                    end else begin
                        dma_byte_idx <= dma_byte_idx + 7'd1;
                        dma_addr     <= dma_addr + 32'd1;
                    end
                end
            end

            // ----------------------------------------------------------------
            // DMA_INPUT: read 16 bytes of chain input from RAM
            // ----------------------------------------------------------------
            S_DMA_INPUT: begin
                if (dma_ack) begin
                    buf_reg[dma_byte_idx[3:0]] <= dma_rdata;
                    if (dma_byte_idx == 7'd15) begin
                        // All 64 bytes loaded; start first hash step
                        dma_req  <= 1'b0;
                        state    <= S_SHA3_INIT;
                    end else begin
                        dma_byte_idx <= dma_byte_idx + 7'd1;
                        dma_addr     <= dma_addr + 32'd1;
                    end
                end
            end

            // ----------------------------------------------------------------
            // SHA3_INIT: set SHAKE-256 mode and issue INIT command
            // ----------------------------------------------------------------
            S_SHA3_INIT: begin
                // Mutate ADRS hash field (bytes 28..31) with step_count
                adrs_reg[28] <= 8'd0;
                adrs_reg[29] <= 8'd0;
                adrs_reg[30] <= 8'd0;
                adrs_reg[31] <= {4'd0, step_count};

                // Set mode to SHAKE-256 (mode 3)
                sha3_mode_wr  <= 1'b1;
                sha3_mode_val <= 2'd3;

                // Issue INIT
                sha3_cmd_valid <= 1'b1;
                sha3_cmd       <= 3'd1;  // CMD_INIT

                absorb_idx <= 6'd0;
                state      <= S_ABSORB_SEED;
            end

            // ----------------------------------------------------------------
            // ABSORB_SEED: feed 16 bytes of PK.seed
            // ----------------------------------------------------------------
            S_ABSORB_SEED: begin
                if (sha3_ready) begin
                    sha3_din_valid <= 1'b1;
                    sha3_din       <= seed_reg[absorb_idx[3:0]];
                    if (absorb_idx == 6'd15) begin
                        absorb_idx <= 6'd0;
                        state      <= S_ABSORB_ADRS;
                    end else begin
                        absorb_idx <= absorb_idx + 6'd1;
                    end
                end
            end

            // ----------------------------------------------------------------
            // ABSORB_ADRS: feed 32 bytes of ADRS
            // ----------------------------------------------------------------
            S_ABSORB_ADRS: begin
                if (sha3_ready) begin
                    sha3_din_valid <= 1'b1;
                    sha3_din       <= adrs_reg[absorb_idx[4:0]];
                    if (absorb_idx == 6'd31) begin
                        absorb_idx <= 6'd0;
                        state      <= S_ABSORB_BUF;
                    end else begin
                        absorb_idx <= absorb_idx + 6'd1;
                    end
                end
            end

            // ----------------------------------------------------------------
            // ABSORB_BUF: feed 16 bytes of chain buffer
            // ----------------------------------------------------------------
            S_ABSORB_BUF: begin
                if (sha3_ready) begin
                    sha3_din_valid <= 1'b1;
                    sha3_din       <= buf_reg[absorb_idx[3:0]];
                    if (absorb_idx == 6'd15) begin
                        state <= S_FINALIZE;
                    end else begin
                        absorb_idx <= absorb_idx + 6'd1;
                    end
                end
            end

            // ----------------------------------------------------------------
            // FINALIZE: issue FINAL command to SHA3 engine
            // ----------------------------------------------------------------
            S_FINALIZE: begin
                sha3_cmd_valid <= 1'b1;
                sha3_cmd       <= 3'd3;  // CMD_FINAL
                squeeze_idx    <= 4'd0;
                state          <= S_WAIT_KECCAK;
            end

            // ----------------------------------------------------------------
            // WAIT_KECCAK: wait for Keccak-f[1600] to complete
            // ----------------------------------------------------------------
            S_WAIT_KECCAK: begin
                if (sha3_ready) begin
                    state <= S_SQUEEZE;
                end
            end

            // ----------------------------------------------------------------
            // SQUEEZE: read 16 bytes of digest into buf_reg
            // ----------------------------------------------------------------
            S_SQUEEZE: begin
                // Read digest byte at squeeze_idx from SHA3 output
                buf_reg[squeeze_idx] <= sha3_dout;
                if (squeeze_idx == 4'd15) begin
                    state <= S_STEP_CHECK;
                end else begin
                    squeeze_idx <= squeeze_idx + 4'd1;
                end
            end

            // ----------------------------------------------------------------
            // STEP_CHECK: increment step, loop or finish
            // ----------------------------------------------------------------
            S_STEP_CHECK: begin
                if (step_count + 4'd1 >= step_start + step_limit) begin
                    // Chain complete — latch output
                    state <= S_DONE;
                end else begin
                    // Next step
                    step_count <= step_count + 4'd1;
                    state      <= S_SHA3_INIT;
                end
            end

            // ----------------------------------------------------------------
            // DONE: latch result, set status=2
            // ----------------------------------------------------------------
            S_DONE: begin
                dout_reg[0]  <= buf_reg[0];
                dout_reg[1]  <= buf_reg[1];
                dout_reg[2]  <= buf_reg[2];
                dout_reg[3]  <= buf_reg[3];
                dout_reg[4]  <= buf_reg[4];
                dout_reg[5]  <= buf_reg[5];
                dout_reg[6]  <= buf_reg[6];
                dout_reg[7]  <= buf_reg[7];
                dout_reg[8]  <= buf_reg[8];
                dout_reg[9]  <= buf_reg[9];
                dout_reg[10] <= buf_reg[10];
                dout_reg[11] <= buf_reg[11];
                dout_reg[12] <= buf_reg[12];
                dout_reg[13] <= buf_reg[13];
                dout_reg[14] <= buf_reg[14];
                dout_reg[15] <= buf_reg[15];
                status   <= 2'd2;
                irq_done <= 1'b1;
                state    <= S_IDLE;
            end

            default: state <= S_IDLE;

            endcase
        end
    end

endmodule
