// ============================================================================
// mp64_memory.v — Megapad-64 Memory Subsystem
// ============================================================================
//
// Dual-port 1 MiB internal BRAM + bus arbiter for tile engine fast-path.
//
// Port A (tile port): 512-bit wide, single-cycle tile load/store.
//   Address granularity: 64 bytes (one tile).
//   Used exclusively by the tile engine for TSRC0/TSRC1 reads and TDST writes.
//
// Port B (CPU port): 64-bit wide, single-cycle read/write.
//   Address granularity: 8 bytes (one cell).
//   Used by the CPU for normal load/store instructions.
//
// Both ports can operate simultaneously with no contention (true dual-port).
// When both ports access the same 64-byte tile, Port A (tile) has priority
// and Port B stalls for one cycle.
//
// External memory requests are forwarded to the ext_mem interface when the
// address is >= INT_MEM_BYTES.
//

`include "mp64_defs.vh"

module mp64_memory (
    input  wire        clk,
    input  wire        rst_n,

    // === CPU port (Port B) — 64-bit ===
    input  wire        cpu_req,        // request valid
    input  wire [63:0] cpu_addr,       // byte address
    input  wire [63:0] cpu_wdata,      // write data
    input  wire        cpu_wen,        // 1=write, 0=read
    input  wire [1:0]  cpu_size,       // BUS_BYTE..BUS_DWORD
    output reg  [63:0] cpu_rdata,      // read data
    output reg         cpu_ack,        // transaction complete

    // === Tile port (Port A) — 512-bit ===
    input  wire        tile_req,       // request valid
    input  wire [19:0] tile_addr,      // byte address (64B aligned, bits [5:0]=0)
    input  wire        tile_wen,       // 1=write, 0=read
    input  wire [511:0]tile_wdata,     // 512-bit write data (full tile)
    output reg  [511:0]tile_rdata,     // 512-bit read data (full tile)
    output reg         tile_ack,       // transaction complete

    // === External memory interface (directly exposed) ===
    output reg         ext_req,
    output reg  [63:0] ext_addr,
    output reg  [63:0] ext_wdata,
    output reg         ext_wen,
    output reg  [1:0]  ext_size,
    input  wire [63:0] ext_rdata,
    input  wire        ext_ack
);

    // ========================================================================
    // Internal BRAM — true dual-port
    // ========================================================================
    //
    // Physical organization: 16384 rows × 512 bits = 1 MiB
    // Port A (tile): addressed by tile_addr[19:6] → 14-bit row index
    // Port B (CPU):  addressed by cpu_addr[19:0]
    //   - Row index: cpu_addr[19:6] (same 14-bit row as Port A)
    //   - Word select within row: cpu_addr[5:3] (which of 8 doublewords)
    //
    // For sub-doubleword access (byte, half, word), the CPU port reads the
    // full doubleword and the output mux selects the requested portion.
    //

    // BRAM storage — inferred as true dual-port block RAM by synthesis tools
    (* ram_style = "block" *)
    reg [511:0] mem [0:16383];  // 16384 × 512-bit = 1 MiB

    // Port A (tile) — 512-bit access, single cycle
    wire [13:0] tile_row = tile_addr[19:6];

    // Port B (CPU) — 64-bit access within a 512-bit row
    wire        cpu_is_internal = (cpu_addr[63:20] == 44'd0);  // addr < 1 MiB
    wire [13:0] cpu_row  = cpu_addr[19:6];
    wire [2:0]  cpu_word = cpu_addr[5:3];   // which 64-bit word in the row

    // Collision detection: both ports hitting the same row
    wire collision = tile_req && cpu_req && cpu_is_internal
                     && (tile_row == cpu_row);

    // Port B stall: CPU must wait if collision (tile has priority)
    wire cpu_stall = collision && !tile_wen;  // stall on tile read (1 cycle)

    // ========================================================================
    // Port A — Tile engine (always single-cycle for internal memory)
    // ========================================================================
    always @(posedge clk) begin
        tile_ack <= 1'b0;
        if (tile_req) begin
            if (tile_wen) begin
                mem[tile_row] <= tile_wdata;
            end
            tile_rdata <= mem[tile_row];
            tile_ack   <= 1'b1;
        end
    end

    // ========================================================================
    // Port B — CPU access
    // ========================================================================
    //
    // Three paths:
    //   1. Internal memory, no collision → single-cycle
    //   2. Internal memory, collision    → stall 1 cycle, then complete
    //   3. External memory              → forward to ext_mem interface
    //

    reg         cpu_stall_r;     // registered stall (for 1-cycle delay)
    reg  [511:0] cpu_row_data;   // full row read from BRAM

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            cpu_ack     <= 1'b0;
            cpu_rdata   <= 64'd0;
            cpu_stall_r <= 1'b0;
            ext_req     <= 1'b0;
        end else begin
            cpu_ack     <= 1'b0;
            ext_req     <= 1'b0;

            if (cpu_req && cpu_is_internal) begin
                // --- Internal memory access ---
                if (cpu_stall && !cpu_stall_r) begin
                    // Collision: tile engine has priority, stall 1 cycle
                    cpu_stall_r <= 1'b1;
                end else begin
                    cpu_stall_r <= 1'b0;

                    if (cpu_wen) begin
                        // Write: read-modify-write the 512-bit row
                        // (Insert the 64-bit word at the correct position)
                        cpu_row_data = mem[cpu_row];
                        case (cpu_size)
                            BUS_DWORD: cpu_row_data[cpu_word*64 +: 64] = cpu_wdata;
                            BUS_WORD:  cpu_row_data[cpu_addr[5:2]*32 +: 32] = cpu_wdata[31:0];
                            BUS_HALF:  cpu_row_data[cpu_addr[5:1]*16 +: 16] = cpu_wdata[15:0];
                            BUS_BYTE:  cpu_row_data[cpu_addr[5:0]*8  +:  8] = cpu_wdata[7:0];
                        endcase
                        mem[cpu_row] <= cpu_row_data;
                        cpu_ack <= 1'b1;
                    end else begin
                        // Read: extract the requested portion from the row
                        cpu_row_data = mem[cpu_row];
                        case (cpu_size)
                            BUS_DWORD: cpu_rdata <= cpu_row_data[cpu_word*64 +: 64];
                            BUS_WORD:  cpu_rdata <= {32'd0, cpu_row_data[cpu_addr[5:2]*32 +: 32]};
                            BUS_HALF:  cpu_rdata <= {48'd0, cpu_row_data[cpu_addr[5:1]*16 +: 16]};
                            BUS_BYTE:  cpu_rdata <= {56'd0, cpu_row_data[cpu_addr[5:0]*8  +:  8]};
                        endcase
                        cpu_ack <= 1'b1;
                    end
                end

            end else if (cpu_req && !cpu_is_internal) begin
                // --- External memory: forward request ---
                ext_req   <= 1'b1;
                ext_addr  <= cpu_addr;
                ext_wdata <= cpu_wdata;
                ext_wen   <= cpu_wen;
                ext_size  <= cpu_size;
                if (ext_ack) begin
                    cpu_rdata <= ext_rdata;
                    cpu_ack   <= 1'b1;
                    ext_req   <= 1'b0;
                end
            end
        end
    end

    // ========================================================================
    // Init for simulation — zero-fill memory
    // ========================================================================
    `ifdef SIMULATION
    integer i;
    initial begin
        for (i = 0; i < 16384; i = i + 1)
            mem[i] = 512'd0;
    end
    `endif

endmodule
