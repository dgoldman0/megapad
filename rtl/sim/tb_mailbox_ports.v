// ============================================================================
// tb_mailbox_ports.v — Chiplet ports as mailbox participants
// ============================================================================
//
// Tests mp64_mailbox with two chiplet ports beside four full cores:
//   - a core rings a port by its ID, and the port sees its lowest pending
//     source and clears it when the doorbell is delivered;
//   - a port rings a core: IPI and the external flag rise, the status
//     register shows the port's bit, and the core acknowledges by port ID;
//   - a core's signal to another core leaves the external flag low;
//   - a port rings another port;
//   - IDs that name no participant are ignored;
//   - a signal raised in the cycle its predecessor is cleared stays pending;
//   - the CSR path reaches ports as the MMIO path does.
//

`timescale 1ns / 1ps
`include "mp64_pkg.vh"

module tb_mailbox_ports;
    localparam NC  = 4;
    localparam NPT = 2;
    localparam IDB = MP64_CORE_ID_BITS;
    localparam [IDB-1:0] PORT0 = 8'd16;
    localparam [IDB-1:0] PORT1 = 8'd17;

    reg clk, rst_n;
    initial clk = 0;
    always #5 clk = ~clk;

    reg         req, wen, requester_valid;
    reg  [11:0] addr;
    reg  [7:0]  wdata;
    wire [7:0]  rdata;
    wire        ack;
    reg  [IDB-1:0] requester_id;
    wire [NC-1:0]  ipi_out, ef_out;

    reg  [NC-1:0]     csr_wen;
    reg  [NC*8-1:0]   csr_addr;
    reg  [NC*64-1:0]  csr_wdata;
    wire [NC*64-1:0]  csr_rdata;

    reg  [NPT-1:0]     port_send_valid, port_take_valid;
    reg  [NPT*IDB-1:0] port_send_dst, port_take_src;
    wire [NPT-1:0]     port_db_valid;
    wire [NPT*IDB-1:0] port_db_src;

    mp64_mailbox #(
        .N_CORES(NC), .N_GLOBAL_CORES(16),
        .N_PORTS(NPT), .PORT_ID_BASE(PORT0)
    ) u_mbox (
        .clk(clk), .rst_n(rst_n),
        .req(req), .addr(addr), .wdata(wdata), .wen(wen),
        .rdata(rdata), .ack(ack),
        .requester_valid(requester_valid), .requester_id(requester_id),
        .ipi_out(ipi_out),
        .csr_ipi_wen(csr_wen), .csr_ipi_addr(csr_addr),
        .csr_ipi_wdata(csr_wdata), .csr_ipi_rdata(csr_rdata),
        .port_send_valid(port_send_valid), .port_send_dst(port_send_dst),
        .port_take_valid(port_take_valid), .port_take_src(port_take_src),
        .port_db_valid(port_db_valid), .port_db_src(port_db_src),
        .ef_out(ef_out)
    );

    integer pass_cnt, fail_cnt;
    task check;
        input [8*80-1:0] label;
        input            condition;
        begin
            if (condition) begin
                $display("  PASS: %0s", label);
                pass_cnt = pass_cnt + 1;
            end else begin
                $display("  FAIL: %0s", label);
                fail_cnt = fail_cnt + 1;
            end
        end
    endtask

    task mbox_write;
        input [IDB-1:0] core;
        input [3:0]     offset;
        input [7:0]     data;
        begin
            @(posedge clk);
            req <= 1'b1; wen <= 1'b1; requester_valid <= 1'b1;
            requester_id <= core; addr <= {8'h50, offset}; wdata <= data;
            @(posedge clk);
            req <= 1'b0; wen <= 1'b0;
            #1;
        end
    endtask

    reg [7:0] rd;
    task mbox_read;
        input [IDB-1:0] core;
        input [3:0]     offset;
        begin
            @(posedge clk);
            req <= 1'b1; wen <= 1'b0; requester_valid <= 1'b1;
            requester_id <= core; addr <= {8'h50, offset};
            @(posedge clk);
            rd = rdata;
            req <= 1'b0;
            #1;
        end
    endtask

    task port_send;
        input integer   port;
        input [IDB-1:0] dst;
        begin
            @(posedge clk);
            port_send_valid[port] <= 1'b1;
            port_send_dst[port*IDB +: IDB] <= dst;
            @(posedge clk);
            port_send_valid[port] <= 1'b0;
            #1;
        end
    endtask

    task port_take;
        input integer   port;
        input [IDB-1:0] src;
        begin
            @(posedge clk);
            port_take_valid[port] <= 1'b1;
            port_take_src[port*IDB +: IDB] <= src;
            @(posedge clk);
            port_take_valid[port] <= 1'b0;
            #1;
        end
    endtask

    task csr_write;
        input integer core;
        input [7:0]   caddr;
        input [63:0]  data;
        begin
            @(posedge clk);
            csr_wen[core] <= 1'b1;
            csr_addr[core*8 +: 8] <= caddr;
            csr_wdata[core*64 +: 64] <= data;
            @(posedge clk);
            csr_wen[core] <= 1'b0;
            #1;
        end
    endtask

    initial begin
        pass_cnt = 0; fail_cnt = 0;
        rst_n = 0; req = 0; wen = 0; addr = 0; wdata = 0;
        requester_valid = 0; requester_id = 0;
        csr_wen = 0; csr_addr = 0; csr_wdata = 0;
        port_send_valid = 0; port_take_valid = 0;
        port_send_dst = 0; port_take_src = 0;
        repeat (4) @(posedge clk);
        rst_n = 1;
        repeat (2) @(posedge clk);

        $display("\nA core rings a port");
        mbox_write(8'd2, MBOX_SEND, PORT0);
        check("port 0 has a doorbell pending", port_db_valid == 2'b01);
        check("its source is core 2", port_db_src[0 +: IDB] == 8'd2);
        check("no core's IPI line rises", ipi_out == 4'b0000);
        check("no external flag rises", ef_out == 4'b0000);
        mbox_write(8'd0, MBOX_SEND, PORT0);
        check("with two sources the lower ID is offered first",
              port_db_src[0 +: IDB] == 8'd0);
        port_take(0, 8'd0);
        check("delivering core 0's doorbell leaves core 2's",
              port_db_valid[0] && port_db_src[0 +: IDB] == 8'd2);
        port_take(0, 8'd2);
        check("delivering both empties the port", port_db_valid == 2'b00);

        $display("\nA port rings a core");
        port_send(0, 8'd1);
        check("core 1's IPI line rises", ipi_out == 4'b0010);
        check("core 1's external flag rises", ef_out == 4'b0010);
        mbox_read(8'd1, MBOX_STATUS);
        check("core 1's status shows participant 4, the first port",
              rd == 8'b0001_0000);
        mbox_write(8'd1, MBOX_ACK, PORT0);
        check("acknowledging the port's ID lowers the IPI line",
              ipi_out == 4'b0000);
        check("and the external flag", ef_out == 4'b0000);

        $display("\nA core rings a core");
        mbox_write(8'd0, MBOX_SEND, 8'd3);
        check("core 3's IPI line rises", ipi_out == 4'b1000);
        check("the external flag stays low", ef_out == 4'b0000);
        port_send(1, 8'd3);
        mbox_read(8'd3, MBOX_STATUS);
        check("core 3's status shows core 0 and the second port",
              rd == 8'b0010_0001);
        check("its external flag follows the port's bit", ef_out == 4'b1000);
        mbox_write(8'd3, MBOX_ACK, PORT1);
        check("acknowledging the port keeps the core's signal pending",
              ipi_out == 4'b1000 && ef_out == 4'b0000);
        mbox_write(8'd3, MBOX_ACK, 8'd0);
        check("acknowledging the core clears the line", ipi_out == 4'b0000);

        $display("\nA port rings a port");
        port_send(1, PORT0);
        check("port 0 sees port 1 as its source",
              port_db_valid == 2'b01 && port_db_src[0 +: IDB] == PORT1);
        port_take(0, PORT1);
        check("delivery clears it", port_db_valid == 2'b00);
        mbox_write(8'd1, MBOX_SEND, PORT1);
        check("a core reaches the second port by its ID",
              port_db_valid == 2'b10 &&
              port_db_src[IDB +: IDB] == 8'd1);
        port_take(1, 8'd1);

        $display("\nIDs that name no participant");
        mbox_write(8'd0, MBOX_SEND, 8'd5);       // a microcore's ID
        mbox_write(8'd0, MBOX_SEND, 8'd18);      // beyond the last port
        port_send(0, 8'd9);
        port_send(0, 8'd200);
        check("nothing becomes pending",
              ipi_out == 4'b0000 && port_db_valid == 2'b00);

        $display("\nA signal raised as its predecessor is cleared");
        mbox_write(8'd0, MBOX_SEND, PORT0);
        @(posedge clk);
        // Core 0 rings again in the cycle the bridge takes the doorbell.
        req <= 1'b1; wen <= 1'b1; requester_valid <= 1'b1;
        requester_id <= 8'd0; addr <= {8'h50, MBOX_SEND}; wdata <= PORT0;
        port_take_valid[0] <= 1'b1; port_take_src[0 +: IDB] <= 8'd0;
        @(posedge clk);
        req <= 1'b0; wen <= 1'b0; port_take_valid[0] <= 1'b0;
        #1;
        check("the new signal stays pending", port_db_valid[0] == 1'b1);
        port_take(0, 8'd0);
        check("and one more delivery clears it", port_db_valid[0] == 1'b0);

        $display("\nThe CSR path");
        csr_write(3, CSR_MBOX, {56'd0, PORT0});
        check("a CSR send from core 3 reaches port 0",
              port_db_valid[0] && port_db_src[0 +: IDB] == 8'd3);
        port_take(0, 8'd3);
        port_send(1, 8'd3);
        check("the pending mask read by CSR shows the second port",
              csr_rdata[3*64 +: 64] == 64'h20);
        csr_write(3, CSR_IPIACK, {56'd0, PORT1});
        check("a CSR acknowledge by port ID clears it",
              ipi_out == 4'b0000 && ef_out == 4'b0000);
        csr_write(3, CSR_MBOX, 64'h1_0000_0010);
        check("a CSR send whose upper bits are set names no participant",
              port_db_valid == 2'b00);

        $display("\n========================================");
        $display("  Mailbox Port Tests: %0d PASSED, %0d FAILED",
                 pass_cnt, fail_cnt);
        $display("========================================");
        $finish;
    end

    initial begin
        #10000000;
        $display("TIMEOUT");
        $finish;
    end
endmodule
