module top
  (
    input         CLK,
    input         BTN_N,
    input         BTN1,
    input         BTN2,
    input         BTN3,
    output        LED1,
    output        LED2,
    output        LED3,
    output        LED4,
    output        LED5,
  );

  wire [13:0] addr;
  wire [15:0] wdata;
  wire wen;
  wire [15:0] rdata;

  wire        pll_clk;
  wire        pll_locked;
  pll_30mhz pll (
      .clk_pin(CLK),
      .locked(pll_locked),
      .pll_clk(pll_clk));

  wire        resetn;
  button_debouncer db (
      .clk(pll_clk),
      .button_pin(BTN_N),
      .level(resetn));

  SB_SPRAM256KA ram (
      .ADDRESS(addr),
      .DATAIN(wdata),
      .MASKWREN(4'b1111),
      .WREN(wen),
      .CHIPSELECT(1'b1),
      .CLOCK(pll_clk),
      .STANDBY(1'b0),
      .SLEEP(1'b0),
      .POWEROFF(1'b1),
      .DATAOUT(rdata)
  );

  clash_top clash_top(
      // domain
      .clk(pll_clk),
      .resetn(resetn),

      // i/o
      .led1(LED1),
      .led2(LED2),
      .led3(LED3),
      .led4(LED4),
      .led5(LED5),
      .btn1(BTN1),
      .btn2(BTN2),
      .btn3(BTN3),

      // ram
      .ram_addr(addr),
      .ram_wdata(wdata),
      .ram_wen(wen),
      .ram_rdata(rdata),
  );
endmodule

module pll_30mhz (
        input clk_pin,
        output locked,
        output pll_clk);

    /**
     * PLL configuration
     *
     * This Verilog header file was generated automatically
     * using the icepll tool from the IceStorm project.
     * It is intended for use with FPGA primitives SB_PLL40_CORE,
     * SB_PLL40_PAD, SB_PLL40_2_PAD, SB_PLL40_2F_CORE or SB_PLL40_2F_PAD.
     * Use at your own risk.
     *
     * Given input frequency:        12.000 MHz
     * Requested output frequency:   30.000 MHz
     * Achieved output frequency:    30.000 MHz
     */

    SB_PLL40_PAD #(
        .FEEDBACK_PATH("SIMPLE"),
        .DIVR(4'b0000),         // DIVR =  0
        .DIVF(7'b1001111),      // DIVF = 79
        .DIVQ(3'b101),          // DIVQ =  5
        .FILTER_RANGE(3'b001)   // FILTER_RANGE = 1
    ) the_pll (
        .PACKAGEPIN(clk_pin),
        .PLLOUTCORE(pll_clk),
        .LOCK(locked),
        .RESETB(1'b1),
        .BYPASS(1'b0)
    );

endmodule // pll30mhz

module button_debouncer (
        input  clk,
        input  button_pin,
        output level,
        output rising_edge,
        output falling_edge);

    localparam COUNT_BITS = 15;

    reg                  is_high;
    reg                  was_high;
    reg                  level_r;
    reg                  rising_edge_r;
    reg                  falling_edge_r;
    reg [COUNT_BITS-1:0] counter = 0;

    assign level        = level_r;
    assign falling_edge = rising_edge_r;
    assign rising_edge  = falling_edge_r;

    always @(posedge clk)
        if (counter) begin
            counter            <= counter + 1;
            rising_edge_r      <= 0;
            falling_edge_r     <= 0;
            was_high           <= is_high;
        end
        else begin
            // was_high           <= is_high;
            is_high            <= button_pin;
            level_r            <= is_high;
            if (is_high != was_high) begin
                counter        <= 1;
                rising_edge_r  <= is_high;
                falling_edge_r <= ~is_high;
            end
        end

endmodule // button_debouncer
