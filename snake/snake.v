`default_nettype none

module top (
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
    output        LEDR_N,
    output        LEDG_N,
    output [15:0] LED_PANEL
);
    wire [6:0] leds;
    assign LED1 = leds[0];
    assign LED2 = leds[1];
    assign LED3 = leds[2];
    assign LED4 = leds[3];
    assign LED5 = leds[4];
    assign LEDG_N = !leds[5];
    assign LEDR_N = !leds[6];

    led_main #(
        .FRAME_BITS(13),
        .DELAY(1)
    ) main (
        .CLK(CLK),
        .resetn_btn(BTN_N),
        .LED_PANEL(LED_PANEL),
        .btn1_bouncy(BTN1),
        .btn2_bouncy(BTN2),
        .btn3_bouncy(BTN3),
        .leds(leds),
    );
endmodule

module game_logic (
    input clk,
    input active,
    input reset,
    input left,
    input forward,
    input right,
    output [13:0] addr0,
    output [13:0] addr1,
    output [15:0] wdata0,
    output [15:0] wdata1,
    input [15:0] rdata0,
    input [15:0] rdata1,
    output wen0,
    output wen1,
    output [6:0] leds
);
    localparam
        MEMORY_MODE_DISPLAY_BOTH = 0,
        MEMORY_MODE_DISPLAY_ONE = 1,
        MEMORY_MODE_DATA_ONE = 2;

    // TODO: Might need to wait an additional clock cycle for the reads
    // i.e. one cycle sets the registers, next cycle reads the data
    // this is odd though, as resources say that reads aren't clocked, so shouldn't
    // need an additional cycle
    reg [1:0] memory_mode;
    reg [5:0] x;
    reg [5:0] y;
    reg wen_display_one;
    reg [1:0] wen_display_both;
    reg wen_data_one;
    assign addr0 = memory_mode[1] == 0 ? {3'b000,y[4:0],x} : {2'b01,data_addr};
    assign addr1 = {3'b000,y[4:0],x};
    assign wen0 =
        memory_mode == MEMORY_MODE_DISPLAY_BOTH ? wen_display_both[0] :
        memory_mode == MEMORY_MODE_DISPLAY_ONE ? wen_display_one & ~y[5] :
        wen_data_one;
    assign wen1 =
        memory_mode == MEMORY_MODE_DISPLAY_BOTH ? wen_display_both[1] :
        memory_mode == MEMORY_MODE_DISPLAY_ONE ? wen_display_one & y[5] :
        0;

    reg last_left;
    reg last_right;
    reg turned_this_move;

    reg [11:0] head_addr;
    reg [11:0] tail_addr;
    reg [11:0] fruit_addr;
    reg generating_fruit;
    reg [11:0] generating_fruit_counter;
    reg [12:0] read_data_addr;
    wire [11:0] read_data_addr_current;
    assign read_data_addr_current = read_data_addr[11:0] - 12'd1;
    reg first_run;

    reg [11:0] data_addr;
    reg [15:0] data_write;
    wire [15:0] data_read = rdata0;

    reg [19:0] random_counter;

    reg [4:0] r;
    reg [5:0] g;
    reg [4:0] b;
    assign wdata0 = memory_mode[1] == 0 ? {r,g,b} : data_write;
    assign wdata1 = {r,g,b};

    reg [19:0] divider;
    wire tick;
    assign tick = divider[18:0] == 19'd0;

    reg [1:0] set_delta_x;
    reg [1:0] set_delta_y;
    wire [5:0] delta_x;
    assign delta_x = {{4{set_delta_x[1]}},set_delta_x};
    wire [5:0] delta_y;
    assign delta_y = {{4{set_delta_y[1]}},set_delta_y};

    reg [2:0] draw_head_y;
    reg [2:0] draw_tail_y;
    assign leds[0] = draw_tail_y[0];
    assign leds[1] = draw_tail_y[1];
    assign leds[2] = draw_tail_y[2];
    assign leds[3] = draw_head_y[0];
    assign leds[4] = draw_head_y[1];
    assign leds[5] = draw_head_y[2];
    assign leds[6] = LOSE == game_state;

    reg [5:0] cursor_x;
    reg [5:0] cursor_y;

    localparam
        STARTUP = 0,
        CLEAR_SCREEN_AND_DATA = 1,
        CLEARING_SCREEN = 2,
        CLEARING_DATA = 3,
        WAIT_START = 4,
        WAITING = 5, // If left or right clicked, update deltas, move to await lifted
                     // If n time has passed:
                     //   update cursorx+y, compare to fruit, if same, set generating fruit, fruit pos, etc.
                     //   if not same, incremement tail_addr
                     // move to READ_DATA, set READ_DATA_index to 0, increment head_addr, setup first read
        WAIT_DIVISION = 6, // wait for fruit division to conclude
        READ_DATA = 7, // if READ_DATA_index == 64*64, move to AWAIT_LIFTED
                       // if READ_DATA_index is current location, and read value is non-0, lose?
                       // if READ_DATA_index is current location, and read value is 0, draw current hue to screen, move to SET_HEAD_DATA
                       // if read value is tail_addr, draw 0 to screen, move to SET_TAIL_DATA <previous tail location can be a fruit>
                       // if read value is 0, and generating fruit, and fruit pos is >0, decremement fruit counter, incrememnt READ_DATA_index, setup read of that index
                       // if read value is 0, and generating fruit, and fruit pos == 0, draw the fruit at this location, incrememnt READ_DATA_index, move to DO_READ
        DO_READ = 8, // setup read of current READ_DATA_index, move to WAIT_READ
        WAIT_READ = 9, // Wait for current read, increment read addr
        SET_HEAD_DATA = 10, // set data at READ_DATA_index to head_addr, increment READ_DATA_index, move to DO_READ
        SET_TAIL_DATA = 11, // set data at READ_DATA_index to 0, move to DO_READ

        LOSE = 12;

    reg [3:0] game_state;
    reg [3:0] state_after_clear;

    reg [7:0] hue;
    wire [7:0] cursor_red, cursor_green, cursor_blue;
    hue2rgb #(.POWER(1)) mk_rgb (
        .hue(hue),
        .red(cursor_red),
        .green(cursor_green),
        .blue(cursor_blue)
    );

    wire division_done;
    reg start_division;
    (* keep="soft" *)
    wire [19:0] unused_qoutient;
    wire [19:0] division_remainder;
    long_divider #(.IO_WIDTH(20)) mk_long_divider (
        .clk(clk),
        .numerator(random_counter),
        .denominator({8'd0, (12'd64 * 12'd64) - (head_addr - tail_addr)}),
        .data_valid(start_division),
        .data_ready(division_done),
        .quotient(unused_qoutient),
        .remainder(division_remainder),
        .reset(reset)
    );

    always @(posedge clk) begin
        if (!reset) begin
            wen_display_one <= 1'b0;
            wen_display_both <= 2'b00;
            wen_data_one <= 1'b0;
            divider <= 20'd0;
            set_delta_x <= 2'b1;
            set_delta_y <= 2'b0;
            cursor_x <= 6'd0;
            cursor_y <= 6'd0;
            head_addr <= 12'd0;
            tail_addr <= 12'd0;
            data_addr <= 12'd0;
            data_write <= 16'd0;
            fruit_addr <= 12'd0;
            random_counter <= 20'd0;
            generating_fruit <= 1'b0;
            generating_fruit_counter <= 12'd0;
            game_state <= STARTUP;
            first_run <= 1'b0;
            hue <= 0;
        end else if (active) begin
            divider <= divider + 20'd1;
            memory_mode <= MEMORY_MODE_DISPLAY_ONE;
            wen_display_one <= 1'b0;
            wen_display_both <= 2'b00;
            wen_data_one <= 1'b0;
            start_division <= 1'b0;
            last_left <= left;
            last_right <= right;

            case (game_state)
                STARTUP: begin
                    game_state <= CLEAR_SCREEN_AND_DATA;
                    state_after_clear <= WAIT_START;
                    first_run <= 1'b1;
                    fruit_addr <= {6'd32, 6'd32};
                    cursor_x <= 6'd32;
                    cursor_y <= 6'd32;
                    set_delta_x <= 2'b1;
                    set_delta_y <= 2'b0;
                    head_addr <= 12'd5;

                    draw_tail_y <= 3'd0;
                    draw_head_y <= 3'd0;
                end
                CLEAR_SCREEN_AND_DATA: begin
                    memory_mode <= MEMORY_MODE_DISPLAY_BOTH;
                    x <= 0;
                    y <= 0;
                    wen_display_both <= 2'b11;
                    r <= 5'd0;
                    g <= 6'd0;
                    b <= 5'd0;
                    game_state <= CLEARING_SCREEN;
                end
                CLEARING_SCREEN: begin
                    memory_mode <= MEMORY_MODE_DISPLAY_BOTH;
                    x <= x + 1;
                    y[4:0] <= y[4:0] + (x == 6'b111111);
                    wen_display_both <= 2'b11;
                    r <= 5'd0;
                    g <= 6'd0;
                    b <= 5'd0;
                    if (x == 6'b111111 && y == 6'b011111) begin
                        game_state <= CLEARING_DATA;
                        data_addr <= 12'd0;
                    end
                end
                CLEARING_DATA: begin
                    memory_mode <= MEMORY_MODE_DATA_ONE;
                    data_addr <= data_addr + 12'd1;
                    wen_data_one <= 1'b1;
                    data_write <= 16'd0;
                    if (data_addr == 12'b111111111111) begin
                        game_state <= state_after_clear;
                        x <= 6'd0;
                        y <= 6'd0;
                        data_addr <= 12'd0;
                    end
                end
                WAIT_START: begin
                    // TODO: do some fun rendering
                    if (left || right || forward) begin
                        game_state <= WAITING;
                        random_counter <= random_counter + divider;
                    end
                end
                WAITING: begin
                    if (tick) begin
                        hue <= hue + 8'd4;
                        cursor_x <= cursor_x + delta_x;
                        cursor_y <= cursor_y + delta_y;
                        turned_this_move <= 1'b0;
                        if ((cursor_x == 6'd0 && delta_x == 6'd63)
                                || (cursor_x == 6'd63 && delta_x == 6'd1)
                                || (cursor_y == 6'd0 && delta_y == 6'd63)
                                || (cursor_y == 6'd63 && delta_y == 6'd1))
                            game_state <= LOSE;
                        else if ({cursor_x + delta_x, cursor_y + delta_y} == fruit_addr || first_run) begin
                            generating_fruit <= 1'd1;
                            start_division <= 1'b1;
                            first_run <= 1'b0;
                            game_state <= WAIT_DIVISION;
                        end else begin
                            memory_mode <= MEMORY_MODE_DATA_ONE;
                            data_addr <= 12'd0;
                            game_state <= DO_READ;
                            tail_addr <= tail_addr + 12'd1;
                        end
                        read_data_addr <= 13'd0;
                        head_addr <= head_addr + 12'd1;
                    end else if (right && ~last_right && ~turned_this_move) begin // counterclockwise
                        set_delta_x <= -set_delta_y;
                        set_delta_y <=  set_delta_x;
                        random_counter <= random_counter + divider;
                        turned_this_move <= 1'b1;
                    end else if (left && ~last_left && ~turned_this_move) begin // clockwise
                        set_delta_x <=  set_delta_y;
                        set_delta_y <= -set_delta_x;
                        random_counter <= random_counter + divider;
                        turned_this_move <= 1'b1;
                    end
                end
                WAIT_DIVISION: begin
                    if (division_done) begin
                        read_data_addr <= 13'd0;
                        memory_mode <= MEMORY_MODE_DATA_ONE;
                        data_addr <= 12'd0;
                        game_state <= DO_READ;
                        generating_fruit_counter <= division_remainder[11:0];
                    end
                end
                READ_DATA: begin
                    if (read_data_addr == 13'd4097) begin
                        game_state <= WAITING;
                    end else if (read_data_addr_current == {1'b0, cursor_x, cursor_y}) begin
                        if (data_read == 16'd0) begin
                            wen_display_one <= 1'b1;
                            x <= cursor_x;
                            y <= cursor_y;
                            r <= cursor_red[7:3];
                            g <= cursor_green[7:2];
                            b <= cursor_blue[7:3];

                            draw_head_y <= cursor_y[2:0];
                            game_state <= SET_HEAD_DATA;
                        end else begin
                            game_state <= LOSE;
                        end
                    end else if (data_read == {4'd1, tail_addr}) begin
                        wen_display_one <= 1'b1;
                        x <= read_data_addr_current[11:6];
                        y <= read_data_addr_current[5:0];
                        r <= 6'd0;
                        g <= 5'd0;
                        b <= 6'd0;
                        
                        draw_tail_y <= read_data_addr_current[2:0];
                        game_state <= SET_TAIL_DATA;
                    end else if (data_read == 16'd0 && generating_fruit) begin
                        if (generating_fruit_counter == 12'd0) begin
                            generating_fruit <= 0;
                            fruit_addr <= read_data_addr_current;
                            wen_display_one <= 1'b1;
                            x <= read_data_addr_current[11:6];
                            y <= read_data_addr_current[5:0];
                            r <= 5'b11111;
                            g <= 6'b111111;
                            b <= 5'b11111;
                            game_state <= DO_READ;
                        end else begin
                            generating_fruit_counter <= generating_fruit_counter - 12'd1;
                            memory_mode <= MEMORY_MODE_DATA_ONE;
                            read_data_addr <= read_data_addr + 13'd1;
                            data_addr <= read_data_addr[11:0] + 12'd1;
                        end
                    end else begin
                        memory_mode <= MEMORY_MODE_DATA_ONE;
                        read_data_addr <= read_data_addr + 13'd1;
                        data_addr <= read_data_addr[11:0] + 12'd1;
                    end
                end
                DO_READ: begin
                    memory_mode <= MEMORY_MODE_DATA_ONE;
                    data_addr <= read_data_addr[11:0];
                    game_state <= WAIT_READ;
                end
                WAIT_READ: begin
                    memory_mode <= MEMORY_MODE_DATA_ONE;
                    read_data_addr <= read_data_addr + 13'd1;
                    data_addr <= read_data_addr[11:0] + 12'd1;
                    game_state <= READ_DATA;
                end
                SET_HEAD_DATA: begin
                    memory_mode <= MEMORY_MODE_DATA_ONE;
                    data_addr <= read_data_addr_current;
                    wen_data_one <= 1'd1;
                    data_write <= {4'd1, head_addr};
                    read_data_addr <= read_data_addr + 13'd1;
                    game_state <= DO_READ;
                end
                SET_TAIL_DATA: begin
                    memory_mode <= MEMORY_MODE_DATA_ONE;
                    data_addr <= read_data_addr_current;
                    wen_data_one <= 1'd1;
                    data_write <= 16'd0;
                    game_state <= DO_READ;
                end
                LOSE: begin
                    // TODO: implement something nice here
                end
            endcase
        end else begin
            wen_display_one <= 1'b0;
            wen_display_both <= 2'b00;
            wen_data_one <= 1'b0;
            start_division <= 1'b0;
        end
    end
endmodule

module long_divider #(
        parameter IO_WIDTH = 20
    ) (
        input clk,
        input [(IO_WIDTH - 1):0] numerator,
        input [(IO_WIDTH - 1):0] denominator,
        input data_valid,
        input reset,
        output reg data_ready,
        output reg [(IO_WIDTH - 1):0] quotient,
        output reg [(IO_WIDTH - 1):0] remainder
    );

    localparam
        DIVISION_SETUP = 0,
        DIVISION_LOOP = 1,
        DIVISION_FINALIZE = 2,
        INDEX_WIDTH = $clog2(IO_WIDTH + 1);

    reg [1:0] division_state;

    reg [(IO_WIDTH - 1):0] numerator_reg;
    reg [(IO_WIDTH - 1):0] denominator_reg;
    reg [(IO_WIDTH - 1):0] quotient_temp;
    reg [(IO_WIDTH - 1):0] remainder_temp;
    reg [(INDEX_WIDTH - 1):0] index;

    wire [(IO_WIDTH - 1):0] remainder_temp_next;
    assign remainder_temp_next = {remainder_temp[(IO_WIDTH-2):0], numerator_reg[index]};

    always @(posedge clk) begin
        if (!reset) begin
            data_ready <= 1'b0;
            quotient <= 0;
            remainder <= 0;
            division_state <= DIVISION_SETUP;
        end else begin
            case (division_state)
                DIVISION_SETUP: begin
                    if (data_valid) begin
                        division_state <= DIVISION_LOOP;
                        numerator_reg <= numerator; 
                        denominator_reg <= denominator; 
                        index <= IO_WIDTH - 1;
                        data_ready <= 1'b0;
                        quotient_temp <= 0;
                        remainder_temp <= 0;
                    end
                end
                DIVISION_LOOP: begin
                    if (remainder_temp_next >= denominator_reg) begin
                        remainder_temp <= remainder_temp_next - denominator_reg;
                        quotient_temp[index] <= 1'b1;            
                    end else remainder_temp <= remainder_temp_next;

                    if (index == 0) division_state <= DIVISION_FINALIZE;

                    index <= index - 1;
                end
                DIVISION_FINALIZE: begin
                    division_state <= DIVISION_SETUP;

                    quotient <= quotient_temp;
                    remainder <= remainder_temp;

                    data_ready <= 1'b1;
                end
            endcase
        end
    end
endmodule

// Pipeline for driving an LED panel with 24 bit RGB graphics.  It
// uses pseudo-PWM at ~53 Hz.
//
// Client should instantiate the `led_main` module and define a
// `painter24` module.  `painter24` should compute a 24 bit RGB pixel
// value, given <frame, subframe, x, y>.
//
// The `DELAY` parameter describes how many clock cycles `painter`
// uses to calculate each pixel.


module led_main #(
        parameter USE_RESETN_BUTTON =  1,
        parameter FRAME_BITS        = 10,
        parameter DELAY             =  1
    ) (
        input CLK,
        input resetn_btn,
        input btn1_bouncy,
        input btn2_bouncy,
        input btn3_bouncy,
        output [6:0] leds,
        output pll_clk,
        output reset,
        output [15:0] LED_PANEL
    );

    // Dimensions
    localparam db = $clog2(DELAY); // delay bits
    localparam fb = FRAME_BITS;    // frame bits
    localparam sb = 8;             // subframe bits
    localparam ab = 5;             // address bits
    localparam rb = 6;             // row bits
    localparam cb = 6;             // column bits
    localparam eb = fb + sb + ab + rb; // extended counter bits

    localparam dh = db - 1;        // delay high bit
    localparam fh = fb - 1;        // frame high bit
    localparam sh = sb - 1;        // subframe high bit
    localparam ah = ab - 1;        // address high bit
    localparam rh = rb - 1;        // row high bit
    localparam ch = cb - 1;        // column high bit
    localparam eh = eb - 1;        // extended counter high bit

    localparam cc = 1 << cb;       // column count

    wire [ah:0] addr;
    wire [sh:0] subframe;
    wire [fh:0] frame;
    wire [rh:0] y0, y1;
    wire [ch:0] x;

    assign {frame, subframe, addr, x} = painter_counter;
    assign y0 = {1'b0, addr};
    assign y1 = {1'b1, addr};

    wire        pll_clk;
    wire        pll_locked;
    wire        resetn;
    wire [23:0] rgb24_0;
    wire [23:0] rgb24_1;
    wire  [2:0] rgb3_0;
    wire  [2:0] rgb3_1;
    wire [eh:0] painter_counter;
    wire        led_driver_ready;

    incrementer #(
        .DELAY(DELAY),
        .WIDTH(eb),
        .SUBFRAME_LSB(ab + rb),
        .SUBFRAME_MSB(sh + ab + rb)
    ) painter_ticker (
        .clk(pll_clk),
        .reset(!led_driver_ready),
        .counter(painter_counter),
        .steal_subframe(steal_subframe));

    wire steal_subframe;
    wire [13:0] addr0, addr1;
    wire [15:0] wdata0, wdata1;
    wire wen0, wen1;
    wire [15:0] rdata0, rdata1;

    SB_SPRAM256KA ram0 (
        .ADDRESS(addr0),
        .DATAIN(wdata0),
        .MASKWREN(4'b1111),
        .WREN(wen0),
        .CHIPSELECT(1'b1),
        .CLOCK(pll_clk),
        .STANDBY(1'b0),
        .SLEEP(1'b0),
        .POWEROFF(1'b1),
        .DATAOUT(rdata0)
    );

    SB_SPRAM256KA ram1 (
        .ADDRESS(addr1),
        .DATAIN(wdata1),
        .MASKWREN(4'b1111),
        .WREN(wen1),
        .CHIPSELECT(1'b1),
        .CLOCK(pll_clk),
        .STANDBY(1'b0),
        .SLEEP(1'b0),
        .POWEROFF(1'b1),
        .DATAOUT(rdata1)
    );

    wire [13:0] logic_addr0;
    wire [13:0] logic_addr1;
    wire [15:0] rdata0_safe;
    wire [15:0] rdata1_safe;
    assign rdata0_safe = (!prev_steal_subframe && steal_subframe) ? saved_rdata0 : rdata0;
    assign rdata1_safe = (!prev_steal_subframe && steal_subframe) ? saved_rdata1 : rdata1;
    reg prev_steal_subframe;
    reg [15:0] saved_rdata0;
    reg [15:0] saved_rdata1;

    always @(posedge pll_clk) begin
        if (prev_steal_subframe && !steal_subframe) begin
            saved_rdata0 <= rdata0;
            saved_rdata1 <= rdata1;
        end
        prev_steal_subframe <= steal_subframe;
    end

    game_logic game (
        .clk(pll_clk),
        .active(steal_subframe),
        .reset(resetn),
        .addr0(logic_addr0),
        .addr1(logic_addr1),
        .wdata0(wdata0),
        .wdata1(wdata1),
        .rdata0(rdata0_safe),
        .rdata1(rdata1_safe),
        .wen0(logic_wen0),
        .wen1(logic_wen1),
        .left(btn3),
        .forward(btn2),
        .right(btn1),
        .leds(leds)
    );

    wire logic_wen0, logic_wen1;
    assign wen0 = steal_subframe ? logic_wen0 : 1'b0;
    assign wen1 = steal_subframe ? logic_wen1 : 1'b0;

    assign addr0 = steal_subframe ? logic_addr0 : {y0[4:0],x};
    assign addr1 = steal_subframe ? logic_addr1 : {y1[4:0],x};

    // When a subframe is being stolen (used for logic), the pwm modules need to emit 0
    assign rgb24_0 = steal_subframe ? 24'd0 : {rdata0[15:11],3'b000,rdata0[10:5],2'b00,rdata0[4:0],3'b000};
    assign rgb24_1 = steal_subframe ? 24'd0 : {rdata1[15:11],3'b000,rdata1[10:5],2'b00,rdata1[4:0],3'b000};

    pwm pwm_r0 (
        .clk(pll_clk),
        .reset(reset),
        .subframe(subframe),
        .color8(rgb24_0[7:0]),
        .color1(rgb3_0[0]));

    pwm pwm_g0 (
        .clk(pll_clk),
        .reset(reset),
        .subframe(subframe),
        .color8(rgb24_0[15:8]),
        .color1(rgb3_0[1]));

    pwm pwm_b0 (
        .clk(pll_clk),
        .reset(reset),
        .subframe(subframe),
        .color8(rgb24_0[23:16]),
        .color1(rgb3_0[2]));

    pwm pwm_r1 (
        .clk(pll_clk),
        .reset(reset),
        .subframe(subframe),
        .color8(rgb24_1[7:0]),
        .color1(rgb3_1[0]));

    pwm pwm_g1 (
        .clk(pll_clk),
        .reset(reset),
        .subframe(subframe),
        .color8(rgb24_1[15:8]),
        .color1(rgb3_1[1]));

    pwm pwm_b1 (
        .clk(pll_clk),
        .reset(reset),
        .subframe(subframe),
        .color8(rgb24_1[23:16]),
        .color1(rgb3_1[2]));

    led_driver #(
        .FRAME_BITS(FRAME_BITS),
        .DELAY(DELAY)
    ) driver (
        .clk(pll_clk),
        .reset(reset),
        .rgb0(rgb3_0),
        .rgb1(rgb3_1),
        .ready(led_driver_ready),
        .LED_PANEL(LED_PANEL));

    pll_30mhz pll (
        .clk_pin(CLK),
        .locked(pll_locked),
        .pll_clk(pll_clk));

    wire btn1;
    button_debouncer db1 (
        .clk(pll_clk),
        .button_pin(btn1_bouncy),
        .level(btn1));

    wire btn2;
    button_debouncer db2 (
        .clk(pll_clk),
        .button_pin(btn2_bouncy),
        .level(btn2));

    wire btn3;
    button_debouncer db3 (
        .clk(pll_clk),
        .button_pin(btn3_bouncy),
        .level(btn3));

    generate
        if (USE_RESETN_BUTTON) begin
            button_debouncer db (
                .clk(pll_clk),
                .button_pin(resetn_btn),
                .level(resetn));
        end
        else
            assign resetn = 1;
    endgenerate

    reset_logic rl (
        .resetn(resetn),
        .pll_clk(pll_clk),
        .pll_locked(pll_locked),
        .reset(reset));

endmodule // led_main


module led_driver #(
        parameter     FRAME_BITS = 10,
        parameter     DELAY      = 1
    ) (
        input         clk,
        input         reset,
        input   [2:0] rgb0,
        input   [2:0] rgb1,
        output        ready,
        output [15:0] LED_PANEL);

    // State machine.
    localparam
        S_START   = 0,
        S_R1      = 1,
        S_R1E     = 2,
        S_R2      = 3,
        S_R2E     = 4,
        S_SDELAY  = 5,
        S_SHIFT0  = 6,
        S_SHIFT   = 7,
        S_SHIFTN  = 8,
        S_BLANK   = 9,
        S_UNBLANK = 10;

    // FM6126 Init Values
    localparam FM_R1     = 16'h7FFF;
    localparam FM_R2     = 16'h0040;

    // Route outputs to LED panel with registers as needed.
    reg   [2:0] led_rgb0;
    reg   [2:0] led_rgb1;
    reg   [4:0] led_addr;
    wire        led_blank;
    wire        led_latch;
    wire        led_sclk;
    wire        P1A1, P1A2, P1A3, P1A4, P1A7, P1A8, P1A9, P1A10;
    wire        P1B1, P1B2, P1B3, P1B4, P1B7, P1B8, P1B9, P1B10;

    // This panel has swapped red and blue wires.
    // assign {P1A3, P1A2, P1A1}              = led_rgb0;
    // assign {P1A9, P1A8, P1A7}              = led_rgb1;
    assign {P1A1, P1A2, P1A3}              = led_rgb0;
    assign {P1A7, P1A8, P1A9}              = led_rgb1;
    assign {P1B10, P1B4, P1B3, P1B2, P1B1} = led_addr;
    assign P1B7                            = led_blank;
    assign P1B8                            = led_latch;
    assign P1B9                            = led_sclk;
    assign {P1A4, P1A10}                   = 0;
    assign LED_PANEL = {P1B10, P1B9, P1B8, P1B7,  P1B4, P1B3, P1B2, P1B1,
                        P1A10, P1A9, P1A8, P1A7,  P1A4, P1A3, P1A2, P1A1};

    assign ready = (state >= S_SDELAY);

    // Dimensions
    localparam db = $clog2(DELAY); // delay bits
    localparam ab = 5;             // address bits
    localparam cb = 6;             // column bits
    localparam lb = ab + cb;       // counter bits

    localparam dh = db - 1;        // delay high bit
    localparam ah = ab - 1;        // address high bit
    localparam ch = cb - 1;        // column high bit
    localparam lh = lb - 1;        // counter high bit

    localparam cc = 1 << cb;       // column count

    wire [ah:0] addr;
    wire [ch:0] col;

    reg  [dh:0] delay;
    reg  [lh:0] counter;
    reg   [1:0] blank;
    reg   [1:0] latch;
    reg   [1:0] sclk;
    reg   [3:0] state;
    reg  [15:0] init_reg;
    reg   [6:0] init_lcnt;

    assign {addr, col} = counter;

    always @(posedge clk)
        if (reset) begin
            led_rgb0              <= 0;
            led_rgb1              <= 0;
            led_addr              <= 0;
            delay                 <= DELAY - 1;
            counter               <= 0;
            blank                 <= 2'b11;
            latch                 <= 2'b00;
            sclk                  <= 2'b00;
            state                 <= S_START;
        end
        else
            case (state)

                S_START:          // Exit reset; start shifting column data.
                    begin
                        blank     <= 2'b11; // blank until first row is latched
                        // Setup FM6126 init
                        init_reg  <= FM_R1;
                        init_lcnt <= 52;
                        state <= S_R1;
                        // ChipOne panels can skip the init sequence
                        //state <= S_SDELAY;
                    end

                // Setting FM6126 Registers
                S_R1:
                    begin
                        led_rgb0  <= init_reg[15] ? 3'b111 : 3'b000;
                        led_rgb1  <= init_reg[15] ? 3'b111 : 3'b000;
                        init_reg  <= {init_reg[14:0], init_reg[15]};

                        latch     <= init_lcnt[6] ? 2'b11 : 2'b00;
                        init_lcnt <= init_lcnt - 1;

                        counter   <= counter + 1;
                        sclk      <= 2'b10;

                        if (counter[5:0] == 63) begin
                            state <= S_R1E;
                        end
                    end

                S_R1E:
                    begin
                        latch     <= 2'b00;
                        sclk      <= 2'b00;
                        init_reg  <= FM_R2;
                        init_lcnt <= 51;
                        state     <= S_R2;
                    end

                S_R2:
                    begin
                        led_rgb0  <= init_reg[15] ? 3'b111 : 3'b000;
                        led_rgb1  <= init_reg[15] ? 3'b111 : 3'b000;
                        init_reg  <= {init_reg[14:0], init_reg[15]};

                        latch     <= init_lcnt[6] ? 2'b11 : 2'b00;
                        init_lcnt <= init_lcnt - 1;

                        counter   <= counter + 1;
                        sclk      <= 2'b10;

                        if (counter[5:0] == 63) begin
                            state <= S_R2E;
                        end
                    end

                S_R2E:
                    begin
                        latch      <= 2'b00;
                        sclk       <= 2'b00;
                        counter    <= 0;
                        state      <= S_SDELAY;
                    end

                S_SDELAY:         // Startup delay
                    begin
                        delay     <= delay - 1;
                        if(!delay)
                            state      <= S_SHIFT;
                    end

                S_SHIFT0:         // Shift first column.
                    begin
                        led_rgb0  <= rgb0;
                        led_rgb1  <= rgb1;
                        counter   <= counter + 1;
                        blank     <= 2'b00;
                        sclk      <= 2'b10;
                        state     <= S_SHIFT;
                    end

                S_SHIFT:          // Shift a column.
                    begin
                        led_rgb0  <= rgb0;
                        led_rgb1  <= rgb1;
                        counter   <= counter + 1;
                        sclk      <= 2'b10;
                        if (col == cc - 2) // next column will be the last.
                            state <= S_SHIFTN;
                    end

                S_SHIFTN:         // Shift the last column; start BLANK.
                    begin
                        blank     <= blank | 2'b01;
                        led_rgb0  <= rgb0;
                        led_rgb1  <= rgb1;
                        state     <= S_BLANK;
                    end

                S_BLANK:          // Drain shift register; pulse LATCH.
                    begin
                        blank     <= 2'b11;
                        latch     <= 2'b11;
                        sclk      <= 2'b00;
                        state     <= S_UNBLANK;
                    end

                S_UNBLANK:        // End BLANK; start next row.
                    begin
                        led_addr  <= addr;
                        counter   <= counter + 1;
                        blank     <= 2'b10;
                        latch     <= 2'b00;
                        state     <= S_SHIFT0;
                    end

            endcase

    ddr led_blank_ddr (
        .clk(clk),
        .data(blank),
        .ddr_pin(led_blank));

    ddr led_latch_ddr (
        .clk(clk),
        .data(latch),
        .ddr_pin(led_latch));

    ddr led_sclk_ddr (
        .clk(clk),
        .data(sclk),
        .ddr_pin(led_sclk));

endmodule // led_driver


module pwm (
    input       clk,
    input       reset,
    input [7:0] subframe,
    input [7:0] color8,
    output      color1);

    // reverse bits to make flicker faster.
    wire [7:0] cmp;
    assign cmp = {subframe[0], subframe[1], subframe[2], subframe[3],
                  subframe[4], subframe[5], subframe[6], subframe[7]};

    assign color1 = ~reset & (color8 > cmp);

endmodule // pwm


module incrementer #(
        parameter DELAY        = 1,
        parameter WIDTH        = 32,
        parameter SUBFRAME_LSB = 11,
        parameter SUBFRAME_MSB = 19
    ) (
        input              clk,
        input              reset,
        output [WIDTH-1:0] counter,
        output steal_subframe);

    localparam S_COUNT = 3'b001;
    localparam S_WAIT1 = 3'b010;
    localparam S_WAIT2 = 3'b100;

    localparam fb = WIDTH - SUBFRAME_MSB - 1;
    localparam sb = SUBFRAME_MSB - SUBFRAME_LSB + 1;
    localparam cb = SUBFRAME_MSB + 1;

    localparam fh = WIDTH - 1;
    localparam fl = SUBFRAME_MSB + 1;
    localparam sh = SUBFRAME_MSB;
    localparam sl = SUBFRAME_LSB;
    localparam ch = SUBFRAME_LSB - 1;

    // Wrap counter one subframe early.
    // Compare counter to {subframe, other} == {11...10, 11...1}.
    localparam skip_subframe = {cb{1'b1}} & ~(1 << sl);

    reg [fh:fl] frame_r;
    reg [sh: 0] counter_r;
    reg  [2: 0] state;

    assign counter = {frame_r, counter_r};

    // Steal every 16th subframe
    localparam sl4 = sl + 3;
    assign steal_subframe = counter_r[sl4:sl] == 4'b1111;

    always @(posedge clk)
        if (reset) begin
            frame_r <= 0;
            counter_r <= 0;
            state <= S_COUNT;
        end
        else
            case (state)

                S_COUNT:
                    begin
                        counter_r     <= counter_r + 1;
                        if (counter_r[5:0] == 63) begin
                            state     <= S_WAIT1;
                        end
                        if (counter_r == skip_subframe) begin
                            counter_r <= 0;
                            frame_r   <= frame_r + 1;
                        end
                    end

                S_WAIT1:
                    state <= S_WAIT2;

                S_WAIT2:
                    state <= S_COUNT;

            endcase

endmodule // incrementer


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


module reset_logic (
        input pll_clk,
        input pll_locked,
        input resetn,
        output reset);

    reg [3:0] count;
    wire reset_i;

    assign reset_i = ~count[3] | ~resetn;

    always @(posedge pll_clk or negedge pll_locked)
        if (~pll_locked)
            count <= 0;
        else if  (~count[3])
            count <= count + 1;

    SB_GB rst_gb (
        .USER_SIGNAL_TO_GLOBAL_BUFFER(reset_i),
        .GLOBAL_BUFFER_OUTPUT(reset));

endmodule // reset_logic


module ddr (
        input       clk,
        input [1:0] data,
        output      ddr_pin);

    SB_IO #(
        .PIN_TYPE(6'b010001)
    ) it (
        .PACKAGE_PIN(ddr_pin),
        .LATCH_INPUT_VALUE(1'b0),
        .INPUT_CLK(clk),
        .OUTPUT_CLK(clk),
        .D_OUT_0(data[0]),
        .D_OUT_1(data[1]));

endmodule // ddr

module hue2rgb#(
    parameter [2:0] POWER = 3'd6 // from 1 to 6
)(
    input  [7:0] hue,
    output [7:0] red,
    output [7:0] green,
    output [7:0] blue);

    hsv_channel #(.POWER(POWER)) red_driver(
        .hue(hue + 8'd85),
        .value(red)
    );
    hsv_channel #(.POWER(POWER)) green_driver(
        .hue(hue),
        .value(green)
    );
    hsv_channel #(.POWER(POWER)) blue_driver(
        .hue(hue + 8'd170),
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
