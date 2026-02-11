// ============================================================================
// mp64_bus.v — Bus Arbiter & Address Decoder
// ============================================================================
//
// Routes CPU and tile engine requests to the correct target:
//   - Internal BRAM (addresses < 1 MiB)
//   - MMIO peripherals (address[63:32] == 0xFFFF_FF00)
//   - External memory (1 MiB ≤ address < MMIO)
//
// Priority scheme:
//   1. Tile engine memory requests (highest — latency-critical for pipelines)
//   2. CPU memory requests
//   3. CPU MMIO requests (always single-cycle, no contention)
//

`include "mp64_defs.vh"

module mp64_bus (
    input  wire        clk,
    input  wire        rst_n,

    // === CPU master port ===
    input  wire        cpu_valid,
    input  wire [63:0] cpu_addr,
    input  wire [63:0] cpu_wdata,
    input  wire        cpu_wen,
    input  wire [1:0]  cpu_size,
    output reg  [63:0] cpu_rdata,
    output reg         cpu_ready,

    // === Memory subsystem (BRAM + external forwarding) ===
    output reg         mem_req,
    output reg  [63:0] mem_addr,
    output reg  [63:0] mem_wdata,
    output reg         mem_wen,
    output reg  [1:0]  mem_size,
    input  wire [63:0] mem_rdata,
    input  wire        mem_ack,

    // === MMIO peripheral bus ===
    output reg         mmio_req,
    output reg  [11:0] mmio_addr,      // 12-bit offset within MMIO space
    output reg  [63:0] mmio_wdata,
    output reg         mmio_wen,
    output reg  [1:0]  mmio_size,
    input  wire [63:0] mmio_rdata,
    input  wire        mmio_ack
);

    // Address classification
    wire is_mmio = (cpu_addr[63:32] == MMIO_HI);
    wire is_mem  = !is_mmio;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            cpu_ready <= 1'b0;
            mem_req   <= 1'b0;
            mmio_req  <= 1'b0;
        end else begin
            cpu_ready <= 1'b0;
            mem_req   <= 1'b0;
            mmio_req  <= 1'b0;

            if (cpu_valid) begin
                if (is_mmio) begin
                    // MMIO access
                    mmio_req   <= 1'b1;
                    mmio_addr  <= cpu_addr[11:0];
                    mmio_wdata <= cpu_wdata;
                    mmio_wen   <= cpu_wen;
                    mmio_size  <= cpu_size;
                    if (mmio_ack) begin
                        cpu_rdata <= mmio_rdata;
                        cpu_ready <= 1'b1;
                    end
                end else begin
                    // Memory access (internal or external — mp64_memory decides)
                    mem_req   <= 1'b1;
                    mem_addr  <= cpu_addr;
                    mem_wdata <= cpu_wdata;
                    mem_wen   <= cpu_wen;
                    mem_size  <= cpu_size;
                    if (mem_ack) begin
                        cpu_rdata <= mem_rdata;
                        cpu_ready <= 1'b1;
                    end
                end
            end
        end
    end

endmodule
