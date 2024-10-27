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

    reg [24:0] divide;
    reg [5:0] limit_x_div, limit_y_div;
    reg [5:0] limit_x_sub, limit_y_sub;

    reg [12:0] last_frame;

    always @(posedge clk) begin
        if (divide == 240000) begin
            divide <= 0;
            limit_x_div <= limit_x_div + 1;
            limit_y_div <= limit_y_div + (limit_x_div == 63);
            //active <= !active;
        end else begin
            divide <= divide + 1;
        end

        if (last_frame != frame) begin
            last_frame <= frame;
            limit_x_sub <= limit_x_sub + 1;
            limit_y_sub <= limit_y_sub + (limit_x_sub == 63);
        end
    end

    assign red = y == limit_y_div && x == limit_x_div ? 200 : 0;
    assign green = y == limit_y_sub && x == limit_x_sub ? 200 : 0;
    assign blue = 0; // x == limit_x_sub ? 200 : 0;
    assign rgb24 = {red, green, blue};

endmodule
