// ============================================================================
// mp64_memory.v — Megapad-64 Banked Memory Subsystem (Portable)
// ============================================================================
//
// 4-bank architecture, 4 MiB total internal BRAM.
//
//   Bank 0 (1 MiB):  System at 0x0000_0000 – 0x000F_FFFF.
//   Bank 1 (1 MiB):  HBW    at 0xFFD0_0000 – 0xFFDF_FFFF.
//   Bank 2 (1 MiB):  HBW    at 0xFFE0_0000 – 0xFFEF_FFFF.
//   Bank 3 (1 MiB):  HBW    at 0xFFF0_0000 – 0xFFFF_FFFF.
//
// Port A (tile):  512-bit single-cycle (1-cycle SRAM latency).
// Port B (CPU):   64-bit w/ sub-word RMW for byte/half/word writes.
//
// Uses mp64_sram_dp primitives — no vendor RAM attributes.
// Addresses not matching any bank are forwarded to external memory.
//
// Timing:
//   Reads:              2 cycles (request + SRAM pipeline)
//   Dword writes:       2 cycles
//   Sub-word writes:    3 cycles (read-modify-write)
//   Tile (512b):        2 cycles
//   External forward:   variable (waits for ext_ack)
//
// Coding standard: Verilog-2001, sync reset, non-blocking assigns.
//

module mp64_memory #(
    parameter BANK_DEPTH  = 16384,    // rows per bank (@ 512-bit each)
    parameter ADDR_W_TILE = 14,       // log2(BANK_DEPTH)
    parameter ADDR_W_CPU  = 17        // log2(BANK_DEPTH * 8)
)(
    input  wire         clk,
    input  wire         rst_n,

    // === CPU port (64-bit) ===
    input  wire         cpu_req,
    input  wire [63:0]  cpu_addr,
    input  wire [63:0]  cpu_wdata,
    input  wire         cpu_wen,
    input  wire [1:0]   cpu_size,
    output reg  [63:0]  cpu_rdata,
    output reg          cpu_ack,

    // === Tile port (512-bit) ===
    input  wire         tile_req,
    input  wire [31:0]  tile_addr,
    input  wire         tile_wen,
    input  wire [511:0] tile_wdata,
    output reg  [511:0] tile_rdata,
    output reg          tile_ack,

    // === External memory forward ===
    output reg          ext_req,
    output reg  [63:0]  ext_addr,
    output reg  [63:0]  ext_wdata,
    output reg          ext_wen,
    output reg  [1:0]   ext_size,
    input  wire [63:0]  ext_rdata,
    input  wire         ext_ack
);

    `include "mp64_pkg.vh"

    // ========================================================================
    // CPU FSM states
    // ========================================================================
    localparam [1:0] MEM_IDLE     = 2'd0;
    localparam [1:0] MEM_CPU_RESP = 2'd1;
    localparam [1:0] MEM_CPU_RMW  = 2'd2;
    localparam [1:0] MEM_EXT_WAIT = 2'd3;

    reg [1:0] cpu_state;

    // ========================================================================
    // Bank address decode — Tile port
    // ========================================================================
    wire tile_is_bank0 = (tile_addr[31:20] == 12'd0);
    wire tile_is_hbw   = (tile_addr[31:20] >= 12'hFFD);
    wire tile_is_valid = tile_is_bank0 || tile_is_hbw;
    wire [1:0] tile_bank_sel = tile_is_hbw ? tile_addr[21:20] : 2'd0;

    // ========================================================================
    // Bank address decode — CPU port
    // ========================================================================
    wire cpu_is_bank0    = (cpu_addr[63:20] == 44'd0);
    wire cpu_is_hbw      = (cpu_addr[63:32] == 32'd0) &&
                           (cpu_addr[31:20] >= 12'hFFD);
    wire cpu_is_internal = cpu_is_bank0 || cpu_is_hbw;
    wire [1:0] cpu_bank_sel = cpu_is_hbw ? cpu_addr[21:20] : 2'd0;

    // ========================================================================
    // SRAM bank instances — 4 × mp64_sram_dp
    // ========================================================================
    // Port A: tile (512b), address = addr[19:6]
    // Port B: CPU  (64b),  address = addr[19:3]

    wire [511:0] bank_a_rdata [0:3];
    wire [63:0]  bank_b_rdata [0:3];

    // SRAM control — per bank
    wire [3:0]   bank_a_ce;
    wire [3:0]   bank_a_we;
    wire [3:0]   bank_b_ce;
    wire [3:0]   bank_b_we;

    // Shared addresses (only one bank is enabled per port)
    wire [ADDR_W_TILE-1:0] sram_tile_addr = tile_addr[19:6];
    wire [ADDR_W_CPU-1:0]  sram_cpu_addr  = cpu_addr[19:3];

    // CPU write data — either direct or merged (driven by FSM)
    reg  [63:0]  cpu_wdata_mux;

    genvar bi;
    generate
        for (bi = 0; bi < 4; bi = bi + 1) begin : g_bank
            mp64_sram_dp #(
                .ADDR_W_A (ADDR_W_TILE),
                .DATA_W_A (512),
                .ADDR_W_B (ADDR_W_CPU),
                .DATA_W_B (64),
                .DEPTH_A  (BANK_DEPTH)
            ) u_sram (
                .clk    (clk),
                .rst_n  (rst_n),
                .a_ce   (bank_a_ce[bi]),
                .a_we   (bank_a_we[bi]),
                .a_addr (sram_tile_addr),
                .a_wdata(tile_wdata),
                .a_rdata(bank_a_rdata[bi]),
                .b_ce   (bank_b_ce[bi]),
                .b_we   (bank_b_we[bi]),
                .b_addr (sram_cpu_addr),
                .b_wdata(cpu_wdata_mux),
                .b_rdata(bank_b_rdata[bi])
            );
        end
    endgenerate

    // ========================================================================
    // Tile port — pipelined (1-cycle SRAM latency)
    // ========================================================================
    // Accept new request only when not already processing one.

    wire tile_start = tile_req && tile_is_valid && !tile_active;

    reg        tile_active;
    reg [1:0]  tile_bank_sel_r;

    always @(posedge clk) begin
        if (!rst_n) begin
            tile_active     <= 1'b0;
            tile_bank_sel_r <= 2'd0;
            tile_ack        <= 1'b0;
            tile_rdata      <= 512'd0;
        end else begin
            tile_active <= tile_start;
            tile_ack    <= 1'b0;

            if (tile_start)
                tile_bank_sel_r <= tile_bank_sel;

            if (tile_active) begin
                tile_ack <= 1'b1;
                case (tile_bank_sel_r)
                    2'd0: tile_rdata <= bank_a_rdata[0];
                    2'd1: tile_rdata <= bank_a_rdata[1];
                    2'd2: tile_rdata <= bank_a_rdata[2];
                    2'd3: tile_rdata <= bank_a_rdata[3];
                endcase
            end
        end
    end

    // Tile SRAM enables (active during start cycle only)
    assign bank_a_ce[0] = tile_start && (tile_bank_sel == 2'd0);
    assign bank_a_ce[1] = tile_start && (tile_bank_sel == 2'd1);
    assign bank_a_ce[2] = tile_start && (tile_bank_sel == 2'd2);
    assign bank_a_ce[3] = tile_start && (tile_bank_sel == 2'd3);

    assign bank_a_we[0] = bank_a_ce[0] && tile_wen;
    assign bank_a_we[1] = bank_a_ce[1] && tile_wen;
    assign bank_a_we[2] = bank_a_ce[2] && tile_wen;
    assign bank_a_we[3] = bank_a_ce[3] && tile_wen;

    // ========================================================================
    // CPU port — FSM
    // ========================================================================
    wire cpu_start_internal = cpu_req && cpu_is_internal &&
                              (cpu_state == MEM_IDLE);
    wire cpu_start_external = cpu_req && !cpu_is_internal &&
                              (cpu_state == MEM_IDLE);
    wire cpu_is_subword     = (cpu_size != BUS_DWORD);

    // CPU SRAM read data — mux from selected bank
    reg [63:0] cpu_bank_rdata;
    reg [1:0]  cpu_bank_sel_r;

    always @(*) begin
        case (cpu_bank_sel_r)
            2'd0: cpu_bank_rdata = bank_b_rdata[0];
            2'd1: cpu_bank_rdata = bank_b_rdata[1];
            2'd2: cpu_bank_rdata = bank_b_rdata[2];
            2'd3: cpu_bank_rdata = bank_b_rdata[3];
        endcase
    end

    // Sub-word merge logic — compute merged 64-bit word
    reg [63:0] merged_word;
    always @(*) begin
        merged_word = cpu_bank_rdata;  // default = read-back
        case (cpu_size)
            BUS_BYTE: merged_word[cpu_addr[2:0]*8  +: 8]  = cpu_wdata[7:0];
            BUS_HALF: merged_word[cpu_addr[2:1]*16 +: 16] = cpu_wdata[15:0];
            BUS_WORD: merged_word[cpu_addr[2]*32   +: 32] = cpu_wdata[31:0];
            default:  merged_word = cpu_wdata;
        endcase
    end

    // CPU write data MUX: direct or merged (based on FSM state)
    always @(*) begin
        if (cpu_state == MEM_CPU_RMW)
            cpu_wdata_mux = merged_word;
        else
            cpu_wdata_mux = cpu_wdata;
    end

    // CPU SRAM bank enables
    // Active during: IDLE→start (read or dword write), and RMW→write-back
    // In IDLE, use combinational bank select (cpu_bank_sel);
    // in RMW, use registered (cpu_bank_sel_r) which was set during IDLE.
    reg  cpu_sram_ce;
    reg  cpu_sram_we;

    wire [1:0] active_cpu_bank = (cpu_state == MEM_IDLE) ? cpu_bank_sel
                                                         : cpu_bank_sel_r;

    assign bank_b_ce[0] = cpu_sram_ce && (active_cpu_bank == 2'd0);
    assign bank_b_ce[1] = cpu_sram_ce && (active_cpu_bank == 2'd1);
    assign bank_b_ce[2] = cpu_sram_ce && (active_cpu_bank == 2'd2);
    assign bank_b_ce[3] = cpu_sram_ce && (active_cpu_bank == 2'd3);

    assign bank_b_we[0] = cpu_sram_we && (active_cpu_bank == 2'd0);
    assign bank_b_we[1] = cpu_sram_we && (active_cpu_bank == 2'd1);
    assign bank_b_we[2] = cpu_sram_we && (active_cpu_bank == 2'd2);
    assign bank_b_we[3] = cpu_sram_we && (active_cpu_bank == 2'd3);

    // Track whether current response is a read (need to capture rdata)
    reg cpu_is_read_r;

    // CPU FSM
    always @(posedge clk) begin
        if (!rst_n) begin
            cpu_state      <= MEM_IDLE;
            cpu_ack        <= 1'b0;
            cpu_rdata      <= 64'd0;
            cpu_bank_sel_r <= 2'd0;
            cpu_is_read_r  <= 1'b0;
            ext_req        <= 1'b0;
            ext_addr       <= 64'd0;
            ext_wdata      <= 64'd0;
            ext_wen        <= 1'b0;
            ext_size       <= 2'd0;
        end else begin
            cpu_ack       <= 1'b0;

            case (cpu_state)

                // ============================================================
                // IDLE — accept new CPU request
                // ============================================================
                MEM_IDLE: begin
                    if (cpu_start_internal) begin
                        cpu_bank_sel_r <= cpu_bank_sel;

                        if (cpu_wen && cpu_is_subword) begin
                            // Sub-word write: read first for RMW
                            cpu_is_read_r <= 1'b0;
                            cpu_state     <= MEM_CPU_RMW;
                        end else begin
                            // Read or dword write
                            cpu_is_read_r <= !cpu_wen;
                            cpu_state     <= MEM_CPU_RESP;
                        end
                    end else if (cpu_start_external) begin
                        ext_req   <= 1'b1;
                        ext_addr  <= cpu_addr;
                        ext_wdata <= cpu_wdata;
                        ext_wen   <= cpu_wen;
                        ext_size  <= cpu_size;
                        cpu_state <= MEM_EXT_WAIT;
                    end
                end

                // ============================================================
                // CPU_RESP — SRAM data valid; capture rdata or ack write
                // ============================================================
                MEM_CPU_RESP: begin
                    cpu_ack <= 1'b1;
                    if (cpu_is_read_r)
                        cpu_rdata <= cpu_bank_rdata;
                    cpu_state <= MEM_IDLE;
                end

                // ============================================================
                // CPU_RMW — SRAM read data for sub-word write; merge & write
                // ============================================================
                MEM_CPU_RMW: begin
                    // merged_word is computed combinationally from
                    // cpu_bank_rdata and cpu_wdata.
                    // cpu_wdata_mux selects merged_word when state==RMW.
                    cpu_state     <= MEM_CPU_RESP;
                end

                // ============================================================
                // EXT_WAIT — waiting for external memory ack
                // ============================================================
                MEM_EXT_WAIT: begin
                    if (ext_ack) begin
                        ext_req   <= 1'b0;
                        cpu_ack   <= 1'b1;
                        cpu_rdata <= ext_rdata;
                        cpu_state <= MEM_IDLE;
                    end
                end

                default: cpu_state <= MEM_IDLE;
            endcase
        end
    end

    // CPU SRAM control — combinational from FSM
    always @(*) begin
        cpu_sram_ce = 1'b0;
        cpu_sram_we = 1'b0;

        case (cpu_state)
            MEM_IDLE: begin
                if (cpu_start_internal) begin
                    cpu_sram_ce = 1'b1;
                    cpu_sram_we = cpu_wen && !cpu_is_subword;
                end
            end
            MEM_CPU_RMW: begin
                // Write back merged data
                cpu_sram_ce = 1'b1;
                cpu_sram_we = 1'b1;
            end
            default: ;
        endcase
    end

endmodule
