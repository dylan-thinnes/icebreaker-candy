`default_nettype none

`include "../include/led-simple.v"

module top (
        input         CLK,
        input         BTN_N,
        output [15:0] LED_PANEL);

    led_main main (
        .CLK(CLK),
        .resetn_btn(BTN_N),
        .LED_PANEL(LED_PANEL));

endmodule

module painter(
        input        clk,
        input        reset,
        input [12:0] frame,
        input  [7:0] subframe,
        input  [5:0] x,
        input  [5:0] y,
        output [2:0] rgb);

    wire border, x_single_bit, y_single_bit;

    assign border = x == 0 || y == 0 || x == 63 || y == 63;
    assign x_single_bit = (| x) & (~|(x & (x - 1)));
    assign y_single_bit = (| y) & (~|(y & (y - 1)));

    //             BLUE GREEN RED
    assign rgb = {x_single_bit, y_single_bit, border};

endmodule
