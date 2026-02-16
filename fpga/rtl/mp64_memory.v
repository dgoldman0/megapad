// ============================================================================
// mp64_memory.v — Megapad-64 Banked Memory Subsystem
// ============================================================================
//
// 4-bank architecture: 4 MiB total internal BRAM.
//
//   Bank 0 (1 MiB):  System BRAM at 0x0000_0000 – 0x000F_FFFF.
//                     Dual-port (CPU + Tile), tile access supported for
//                     backward compatibility but software should prefer HBW.
//
//   Bank 1 (1 MiB):  HBW at 0xFFD0_0000 – 0xFFDF_FFFF.  Dual-port.
//   Bank 2 (1 MiB):  HBW at 0xFFE0_0000 – 0xFFEF_FFFF.  Dual-port.
//   Bank 3 (1 MiB):  HBW at 0xFFF0_0000 – 0xFFFF_FFFF.  Dual-port.
//
// Port A (tile port): 512-bit wide, single-cycle tile load/store.
//   Routes to Bank 0 (legacy) or Banks 1–3 (HBW) based on tile_addr[31:0].
//
// Port B (CPU port): 64-bit wide, single-cycle read/write.
//   Routes to any bank based on cpu_addr.
//
// External memory requests are forwarded when the address falls between
// Bank 0 and HBW (i.e., not in any internal bank).
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
    input  wire [31:0] tile_addr,      // byte address (32-bit physical)
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
    // Internal BRAM — 4 banks × 16384 × 512-bit = 4 MiB
    // ========================================================================
    //
    // Each bank: 16384 rows × 512 bits = 1 MiB
    //   Row index:   addr[19:6]  (14 bits)
    //   Word within row: addr[5:3] (3 bits, selects one of 8 doublewords)
    //

    (* ram_style = "block" *)
    reg [511:0] bank0 [0:16383];  // Bank 0: system BRAM (0x0000_0000)
    (* ram_style = "block" *)
    reg [511:0] bank1 [0:16383];  // Bank 1: HBW (0xFFD0_0000)
    (* ram_style = "block" *)
    reg [511:0] bank2 [0:16383];  // Bank 2: HBW (0xFFE0_0000)
    (* ram_style = "block" *)
    reg [511:0] bank3 [0:16383];  // Bank 3: HBW (0xFFF0_0000)

    // ========================================================================
    // Tile port address decode
    // ========================================================================
    wire tile_is_bank0 = (tile_addr[31:20] == 12'd0);
    wire tile_is_hbw   = (tile_addr[31:20] >= 12'hFFD);
    wire tile_is_valid = tile_is_bank0 || tile_is_hbw;
    wire [1:0] tile_bank_sel = tile_is_hbw ? tile_addr[21:20] : 2'd0;
    wire [13:0] tile_row_idx = tile_addr[19:6];

    // ========================================================================
    // CPU port address decode
    // ========================================================================
    wire cpu_is_bank0 = (cpu_addr[63:20] == 44'd0);
    wire cpu_is_hbw   = (cpu_addr[63:32] == 32'd0) && (cpu_addr[31:20] >= 12'hFFD);
    wire cpu_is_internal = cpu_is_bank0 || cpu_is_hbw;
    wire [1:0] cpu_bank_sel = cpu_is_hbw ? cpu_addr[21:20] : 2'd0;
    wire [13:0] cpu_row_idx = cpu_addr[19:6];
    wire [2:0]  cpu_word    = cpu_addr[5:3];

    // ========================================================================
    // Collision detection (same bank, same row)
    // ========================================================================
    wire collision = tile_req && cpu_req && cpu_is_internal && tile_is_valid
                     && (tile_bank_sel == cpu_bank_sel)
                     && (tile_row_idx == cpu_row_idx);
    wire cpu_stall = collision && !tile_wen;

    // ========================================================================
    // Bank read helper — combinational mux
    // ========================================================================
    reg [511:0] tile_bank_rdata;
    always @(*) begin
        case (tile_bank_sel)
            2'd0: tile_bank_rdata = bank0[tile_row_idx];
            2'd1: tile_bank_rdata = bank1[tile_row_idx];
            2'd2: tile_bank_rdata = bank2[tile_row_idx];
            2'd3: tile_bank_rdata = bank3[tile_row_idx];
        endcase
    end

    // ========================================================================
    // Port A — Tile engine (single-cycle for all internal banks)
    // ========================================================================
    always @(posedge clk) begin
        tile_ack <= 1'b0;
        if (tile_req && tile_is_valid) begin
            if (tile_wen) begin
                case (tile_bank_sel)
                    2'd0: bank0[tile_row_idx] <= tile_wdata;
                    2'd1: bank1[tile_row_idx] <= tile_wdata;
                    2'd2: bank2[tile_row_idx] <= tile_wdata;
                    2'd3: bank3[tile_row_idx] <= tile_wdata;
                endcase
            end
            tile_rdata <= tile_bank_rdata;
            tile_ack   <= 1'b1;
        end
    end

    // ========================================================================
    // Port B — CPU access (bank-routed or external forward)
    // ========================================================================
    reg         cpu_stall_r;
    reg  [511:0] cpu_row_data;

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
                // --- Internal bank access ---
                if (cpu_stall && !cpu_stall_r) begin
                    cpu_stall_r <= 1'b1;
                end else begin
                    cpu_stall_r <= 1'b0;

                    // Read row from selected bank
                    case (cpu_bank_sel)
                        2'd0: cpu_row_data = bank0[cpu_row_idx];
                        2'd1: cpu_row_data = bank1[cpu_row_idx];
                        2'd2: cpu_row_data = bank2[cpu_row_idx];
                        2'd3: cpu_row_data = bank3[cpu_row_idx];
                    endcase

                    if (cpu_wen) begin
                        // Write: read-modify-write the 512-bit row
                        case (cpu_size)
                            BUS_DWORD: cpu_row_data[cpu_word*64 +: 64] = cpu_wdata;
                            BUS_WORD:  cpu_row_data[cpu_addr[5:2]*32 +: 32] = cpu_wdata[31:0];
                            BUS_HALF:  cpu_row_data[cpu_addr[5:1]*16 +: 16] = cpu_wdata[15:0];
                            BUS_BYTE:  cpu_row_data[cpu_addr[5:0]*8  +:  8] = cpu_wdata[7:0];
                        endcase
                        // Write back to selected bank
                        case (cpu_bank_sel)
                            2'd0: bank0[cpu_row_idx] <= cpu_row_data;
                            2'd1: bank1[cpu_row_idx] <= cpu_row_data;
                            2'd2: bank2[cpu_row_idx] <= cpu_row_data;
                            2'd3: bank3[cpu_row_idx] <= cpu_row_data;
                        endcase
                        cpu_ack <= 1'b1;
                    end else begin
                        // Read: extract the requested portion from the row
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
    // Init for simulation — zero-fill all banks
    // ========================================================================
    `ifdef SIMULATION
    integer i;
    initial begin
        for (i = 0; i < 16384; i = i + 1) begin
            bank0[i] = 512'd0;
            bank1[i] = 512'd0;
            bank2[i] = 512'd0;
            bank3[i] = 512'd0;
        end
    end
    `endif

endmodule
