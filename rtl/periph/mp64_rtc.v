// ============================================================================
// mp64_rtc.v — Real-Time Clock / System Clock Peripheral
// ============================================================================
//
// Combined system clock with:
//   - 64-bit monotonic uptime counter in milliseconds (since boot)
//   - 64-bit epoch counter in milliseconds (since Unix epoch, settable)
//   - Calendar registers (sec/min/hour/day/mon/year/dow)
//   - Alarm interrupt on hour:min:sec match
//
// Reading byte 0 of UPTIME or EPOCH latches the full 64-bit value so
// that software can read bytes 1–7 without tearing.
//
// Register map (byte offsets from RTC_BASE, 32 bytes total):
//   0x00–0x07  UPTIME   (R)   64-bit ms since boot (latch on read +0)
//   0x08–0x0F  EPOCH    (RW)  64-bit ms since Unix epoch (latch on read +8)
//   0x10       SEC      (RW)  seconds  (0–59)
//   0x11       MIN      (RW)  minutes  (0–59)
//   0x12       HOUR     (RW)  hours    (0–23)
//   0x13       DAY      (RW)  day      (1–31)
//   0x14       MON      (RW)  month    (1–12)
//   0x15       YEAR_LO  (RW)  year low byte
//   0x16       YEAR_HI  (RW)  year high byte
//   0x17       DOW      (RW)  day of week (0=Sun … 6=Sat)
//   0x18       CTRL     (RW)  bit0=run, bit1=alarm_irq_en
//   0x19       STATUS   (RW)  bit0=alarm(W1C), bit1=1Hz(W1C), bit2=1ms(W1C)
//   0x1A       ALARM_S  (RW)  alarm seconds
//   0x1B       ALARM_M  (RW)  alarm minutes
//   0x1C       ALARM_H  (RW)  alarm hours
//   0x1D–0x1F  reserved
//

`include "mp64_pkg.vh"

module mp64_rtc (
    input  wire        clk,
    input  wire        rst_n,

    // === MMIO register interface ===
    input  wire        req,
    input  wire [4:0]  addr,       // byte offset within RTC block (32 B)
    input  wire [7:0]  wdata,
    input  wire        wen,
    output reg  [7:0]  rdata,
    output reg         ack,

    // === Interrupt output ===
    output wire        irq
);

    // ========================================================================
    // Counters
    // ========================================================================
    reg [63:0] uptime_ms;      // monotonic ms since boot
    reg [63:0] epoch_ms;       // ms since Unix epoch (settable)

    // Latch registers (snapshot on read of byte 0)
    reg [63:0] uptime_latch;
    reg [63:0] epoch_latch;

    // Calendar
    reg [5:0]  sec;            // 0–59
    reg [5:0]  min;            // 0–59
    reg [4:0]  hour;           // 0–23
    reg [4:0]  day;            // 1–31
    reg [3:0]  mon;            // 1–12
    reg [15:0] year;           // 0–65535
    reg [2:0]  dow;            // 0–6

    // Control / status
    reg [7:0]  ctrl;           // bit0=run, bit1=alarm_irq_en
    reg        alarm_flag;
    reg        tick_1hz_flag;
    reg        tick_1ms_flag;

    // Alarm registers
    reg [5:0]  alarm_sec;
    reg [5:0]  alarm_min;
    reg [4:0]  alarm_hour;

    wire running   = ctrl[0];
    wire alarm_ie  = ctrl[1];

    // ========================================================================
    // 1 ms prescaler — divides system clock by CLOCK_HZ/1000
    // ========================================================================
    localparam [16:0] MS_DIVISOR = CLOCK_HZ / 1000;  // 100,000 for 100 MHz
    reg [16:0] ms_prescaler;
    wire       one_ms_tick = (ms_prescaler == MS_DIVISOR - 1) && running;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n)
            ms_prescaler <= 17'd0;
        else if (!running)
            ms_prescaler <= 17'd0;
        else if (ms_prescaler == MS_DIVISOR - 1)
            ms_prescaler <= 17'd0;
        else
            ms_prescaler <= ms_prescaler + 1;
    end

    // ========================================================================
    // 1-second sub-counter (counts 1000 ms ticks)
    // ========================================================================
    reg [9:0] sec_prescaler;   // 0–999
    wire      one_sec_tick = (sec_prescaler == 10'd999) && one_ms_tick;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n)
            sec_prescaler <= 10'd0;
        else if (one_ms_tick) begin
            if (sec_prescaler == 10'd999)
                sec_prescaler <= 10'd0;
            else
                sec_prescaler <= sec_prescaler + 1;
        end
    end

    // ========================================================================
    // Days-in-month lookup (combinational)
    // ========================================================================
    reg [4:0] days_in_month;
    always @(*) begin
        case (mon)
            4'd1:  days_in_month = 5'd31;
            4'd2:  days_in_month = (year[1:0] == 2'd0 && (year % 100 != 0 || year % 400 == 0))
                                   ? 5'd29 : 5'd28;
            4'd3:  days_in_month = 5'd31;
            4'd4:  days_in_month = 5'd30;
            4'd5:  days_in_month = 5'd31;
            4'd6:  days_in_month = 5'd30;
            4'd7:  days_in_month = 5'd31;
            4'd8:  days_in_month = 5'd31;
            4'd9:  days_in_month = 5'd30;
            4'd10: days_in_month = 5'd31;
            4'd11: days_in_month = 5'd30;
            4'd12: days_in_month = 5'd31;
            default: days_in_month = 5'd31;
        endcase
    end

    // ========================================================================
    // Uptime + epoch ms counters
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            uptime_ms <= 64'd0;
            epoch_ms  <= 64'd0;
        end else if (one_ms_tick) begin
            uptime_ms <= uptime_ms + 1;
            epoch_ms  <= epoch_ms  + 1;
        end else if (req && wen) begin
            // Software can set epoch_ms byte-by-byte
            case (addr)
                5'h08: epoch_ms[ 7: 0] <= wdata;
                5'h09: epoch_ms[15: 8] <= wdata;
                5'h0A: epoch_ms[23:16] <= wdata;
                5'h0B: epoch_ms[31:24] <= wdata;
                5'h0C: epoch_ms[39:32] <= wdata;
                5'h0D: epoch_ms[47:40] <= wdata;
                5'h0E: epoch_ms[55:48] <= wdata;
                5'h0F: epoch_ms[63:56] <= wdata;
                default: ;
            endcase
        end
    end

    // ========================================================================
    // Counter latch logic (snapshot on read of byte 0 / byte 8)
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            uptime_latch <= 64'd0;
            epoch_latch  <= 64'd0;
        end else if (req && !wen) begin
            if (addr == 5'h00) uptime_latch <= uptime_ms;
            if (addr == 5'h08) epoch_latch  <= epoch_ms;
        end
    end

    // ========================================================================
    // Calendar counting logic
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            sec          <= 6'd0;
            min          <= 6'd0;
            hour         <= 5'd0;
            day          <= 5'd1;
            mon          <= 4'd1;
            year         <= 16'd2026;
            dow          <= 3'd0;
            alarm_flag   <= 1'b0;
            tick_1hz_flag<= 1'b0;
            tick_1ms_flag<= 1'b0;
        end else begin
            // 1 ms tick flag
            if (one_ms_tick)
                tick_1ms_flag <= 1'b1;

            // Calendar advances every second
            if (one_sec_tick) begin
                tick_1hz_flag <= 1'b1;

                if (sec == 6'd59) begin
                    sec <= 6'd0;
                    if (min == 6'd59) begin
                        min <= 6'd0;
                        if (hour == 5'd23) begin
                            hour <= 5'd0;
                            dow  <= (dow == 3'd6) ? 3'd0 : dow + 1;
                            if (day == days_in_month) begin
                                day <= 5'd1;
                                if (mon == 4'd12) begin
                                    mon  <= 4'd1;
                                    year <= year + 1;
                                end else begin
                                    mon <= mon + 1;
                                end
                            end else begin
                                day <= day + 1;
                            end
                        end else begin
                            hour <= hour + 1;
                        end
                    end else begin
                        min <= min + 1;
                    end
                end else begin
                    sec <= sec + 1;
                end

                // Alarm check
                if (sec == alarm_sec && min == alarm_min && hour == alarm_hour)
                    alarm_flag <= 1'b1;
            end

            // W1C: clear status flags on write to STATUS
            if (req && wen && addr == RTC_STATUS) begin
                if (wdata[0]) alarm_flag    <= 1'b0;
                if (wdata[1]) tick_1hz_flag <= 1'b0;
                if (wdata[2]) tick_1ms_flag <= 1'b0;
            end

            // Calendar register writes (set time)
            if (req && wen) begin
                case (addr)
                    RTC_SEC:     sec       <= wdata[5:0];
                    RTC_MIN:     min       <= wdata[5:0];
                    RTC_HOUR:    hour      <= wdata[4:0];
                    RTC_DAY:     day       <= wdata[4:0];
                    RTC_MON:     mon       <= wdata[3:0];
                    RTC_YEAR_LO: year[7:0] <= wdata;
                    RTC_YEAR_HI: year[15:8]<= wdata;
                    RTC_DOW:     dow       <= wdata[2:0];
                    RTC_ALARM_S: alarm_sec <= wdata[5:0];
                    RTC_ALARM_M: alarm_min <= wdata[5:0];
                    RTC_ALARM_H: alarm_hour<= wdata[4:0];
                    default: ;
                endcase
            end
        end
    end

    // ========================================================================
    // Control register write
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n)
            ctrl <= 8'd0;
        else if (req && wen && addr == RTC_CTRL)
            ctrl <= wdata;
    end

    // ========================================================================
    // Interrupt
    // ========================================================================
    assign irq = alarm_flag & alarm_ie;

    // ========================================================================
    // Register read mux
    // ========================================================================
    always @(posedge clk) begin
        ack <= 1'b0;
        if (req) begin
            ack <= 1'b1;
            case (addr)
                // Uptime ms (latched) — reading +0 triggers latch above,
                // but we return the *live* value for byte 0 so the latch
                // captures the same cycle.  Bytes 1-7 use the latch.
                5'h00: rdata <= uptime_ms[ 7: 0];
                5'h01: rdata <= uptime_latch[15: 8];
                5'h02: rdata <= uptime_latch[23:16];
                5'h03: rdata <= uptime_latch[31:24];
                5'h04: rdata <= uptime_latch[39:32];
                5'h05: rdata <= uptime_latch[47:40];
                5'h06: rdata <= uptime_latch[55:48];
                5'h07: rdata <= uptime_latch[63:56];

                // Epoch ms (latched)
                5'h08: rdata <= epoch_ms[ 7: 0];
                5'h09: rdata <= epoch_latch[15: 8];
                5'h0A: rdata <= epoch_latch[23:16];
                5'h0B: rdata <= epoch_latch[31:24];
                5'h0C: rdata <= epoch_latch[39:32];
                5'h0D: rdata <= epoch_latch[47:40];
                5'h0E: rdata <= epoch_latch[55:48];
                5'h0F: rdata <= epoch_latch[63:56];

                // Calendar
                RTC_SEC:     rdata <= {2'd0, sec};
                RTC_MIN:     rdata <= {2'd0, min};
                RTC_HOUR:    rdata <= {3'd0, hour};
                RTC_DAY:     rdata <= {3'd0, day};
                RTC_MON:     rdata <= {4'd0, mon};
                RTC_YEAR_LO: rdata <= year[7:0];
                RTC_YEAR_HI: rdata <= year[15:8];
                RTC_DOW:     rdata <= {5'd0, dow};

                // Control / status / alarm
                RTC_CTRL:    rdata <= ctrl;
                RTC_STATUS:  rdata <= {5'd0, tick_1ms_flag, tick_1hz_flag, alarm_flag};
                RTC_ALARM_S: rdata <= {2'd0, alarm_sec};
                RTC_ALARM_M: rdata <= {2'd0, alarm_min};
                RTC_ALARM_H: rdata <= {3'd0, alarm_hour};
                default:     rdata <= 8'd0;
            endcase
        end
    end

endmodule
