`default_nettype none

`include "../include/led-pwm.v"

module top (
    input         CLK,
    input         BTN_N,
    output [15:0] LED_PANEL
);
    led_main #(
        .FRAME_BITS(13),
        .DELAY(1)
    ) main (
        .CLK(CLK),
        .resetn_btn(BTN_N),
        .LED_PANEL(LED_PANEL)
    );
endmodule

module painter24(
    input         clk,
    input         reset,
    input  [12:0] frame,
    input  [7:0]  subframe,
    input  [5:0]  x,
    input  [5:0]  y,
    output [23:0] rgb24
);
    wire [7:0] red, green, blue;

    wire [5:0] truex, truey;
    assign truex = x - 6'b000001;
    assign truey = x == 0 ? {y[5], y[4:0] - 5'b00001} : y;

    wire [5:0] distx, disty, dist;
    wire [5:0] dist;
    assign distx = truex < 32 ? 32 - truex : truex - 32;
    assign disty = truey < 32 ? 32 - truey : truey - 32;
    assign dist = distx + disty;

    wire [7:0] scalar;
    wire [15:0] scaled_dist;
    assign scaled_dist = { {8{scalar[7]}}, scalar[7:0] } * {10'd0, dist};
    assign scalar = 100;

    hue2rgb #(.POWER(1)) hsv_driver(
        .hue(frame[7:0] + scaled_dist[15:8]),
        .red(red),
        .green(green),
        .blue(blue)
    );

    assign rgb24 = {red, green, blue};

endmodule

module hue2rgb#(
    parameter [2:0] POWER = 3'd6 // from 1 to 6
)(
    input  [7:0] hue,
    output [7:0] red,
    output [7:0] green,
    output [7:0] blue);

    hsv_channel #(.POWER(POWER)) red_driver(
        .hue(hue + 85),
        .value(red)
    );
    hsv_channel #(.POWER(POWER)) green_driver(
        .hue(hue),
        .value(green)
    );
    hsv_channel #(.POWER(POWER)) blue_driver(
        .hue(hue + 170),
        .value(blue)
    );
endmodule

module hsv_channel#(
        parameter [2:0] POWER = 3'd6 // from 1 to 6
)(
        input  [7:0] hue,
        output [7:0] value);

    assign value =
                hue < 43  ? hue * POWER
              : hue < 128 ? 43 * POWER
              : hue < 170 ? (170 - hue) * POWER
                          : 0;

endmodule

