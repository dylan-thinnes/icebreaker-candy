`default_nettype none

`include "../include/led-pwm.v"

module top (
        input         CLK,
        input         BTN_N,
        output [15:0] LED_PANEL);

        led_main #(
            .FRAME_BITS(13),
            .DELAY(1)
        ) main (
            .CLK(CLK),
            .resetn_btn(BTN_N),
            .LED_PANEL(LED_PANEL));

endmodule

module painter24(
        input        clk,
        input        reset,
        input  [12:0] frame,
        input  [7:0] subframe,
        input  [5:0] x,
        input  [5:0] y,
        output [23:0] rgb24);

    wire [7:0] red, green, blue;
    reg [0:5] x_target, y_target;

    reg [24:0] divide;
    //reg active;
    reg [5:0] limit_x, limit_y;

    always @(posedge clk) begin
        if (divide == 240000) begin
            divide <= 0;
            limit_x <= limit_x + 1;
            limit_y <= limit_y + (limit_x == 0);
            //active <= !active;
        end else begin
            divide <= divide + 1;
        end
    end

    assign red = y == limit_y && x == limit_x ? 200 : 0;
    assign green = 0; // y == limit_y ? 200 : 0;
    assign blue = 0; // x == limit_x ? 200 : 0;
    assign rgb24 = {red, green, blue};

endmodule
