`timescale 1ns / 1ps
`default_nettype none

// Project F: Display Timings
// (C)2019 Will Green, Open Source Hardware released under the MIT License
// Learn more at https://projectf.io

//128K video memory is not enough for 640x480x4
`define RES_640_400
//`define RES_1024_768

module display_timings #(
    H_RES=640,                      // horizontal resolution (pixels)
    V_RES=480,                      // vertical resolution (lines)
    H_FP=16,                        // horizontal front porch
    H_SYNC=96,                      // horizontal sync
    H_BP=48,                        // horizontal back porch
    V_FP=10,                        // vertical front porch
    V_SYNC=2,                       // vertical sync
    V_BP=33,                        // vertical back porch
    H_POL=0,                        // horizontal sync polarity (0:neg, 1:pos)
    V_POL=0                         // vertical sync polarity (0:neg, 1:pos)
    )
    (
    input  wire i_pix_clk,          // pixel clock
    input  wire i_rst,              // reset: restarts frame (active high)
    output wire o_hs,               // horizontal sync
    output wire o_vs,               // vertical sync
    output wire o_de,               // display enable: high during active video
    output wire o_frame,            // high for one tick at the start of each frame
    output wire o_scanline,         // high for one tick at the start of each scanline
    output reg  o_vblank,           // high during vertical blank phase
    output reg signed [15:0] o_sx,  // horizontal beam position (including blanking)
    output reg signed [15:0] o_sy   // vertical beam position (including blanking)
    );

    // Horizontal: sync, active, and pixels
    localparam signed H_STA  = 0 - H_FP - H_SYNC - H_BP;    // horizontal start
    localparam signed HS_STA = H_STA + H_FP;                // sync start
    localparam signed HS_END = HS_STA + H_SYNC;             // sync end
    localparam signed HA_STA = 0;                           // active start
    localparam signed HA_END = H_RES - 1;                   // active end

    // Vertical: sync, active, and pixels
    localparam signed V_STA  = 0 - V_FP - V_SYNC - V_BP;    // vertical start
    localparam signed VS_STA = V_STA + V_FP;                // sync start
    localparam signed VS_END = VS_STA + V_SYNC;             // sync end
    localparam signed VA_STA = 0;                           // active start
    localparam signed VA_END = V_RES - 1;                   // active end

    // generate sync signals with correct polarity
    assign o_hs = H_POL ? (o_sx > HS_STA && o_sx <= HS_END)
        : ~(o_sx > HS_STA && o_sx <= HS_END);
    assign o_vs = V_POL ? (o_sy > VS_STA && o_sy <= VS_END)
        : ~(o_sy > VS_STA && o_sy <= VS_END);

    // display enable: high during active period
    assign o_de = (o_sx >= 0 && o_sy >= 0);

    // o_frame: high for one tick at the start of each frame
    assign o_frame = (o_sy == V_STA && o_sx == H_STA);
    // o_scanline: high for one tick at the start of each visible scanline
    assign o_scanline = (o_sy >= VA_STA) && (o_sy <= VA_END) && (o_sx == H_STA);

    always @(posedge i_pix_clk)
    begin
        if(o_frame) o_vblank <= 1;
        else if (o_de) o_vblank <= 0;
    end

    always @ (posedge i_pix_clk)
    begin
        if (i_rst)  // reset to start of frame
        begin
            o_sx <= H_STA;
            o_sy <= V_STA;
        end
        else
        begin
            if (o_sx == HA_END)  // end of line
            begin
                o_sx <= H_STA;
                if (o_sy == VA_END)  // end of frame
                    o_sy <= V_STA;
                else
                    o_sy <= o_sy + 16'sh1;
            end
            else
                o_sx <= o_sx + 16'sh1;
        end
    end
endmodule

// Project F: Display Controller VGA Demo
// (C)2020 Will Green, Open source hardware released under the MIT License
// Learn more at https://projectf.io

// This demo requires the following Verilog modules:
//  * display_clocks
//  * display_timings
//  * test_card_simple or another test card

module vgafb #(VMEM_ADDR_WIDTH = 15, VMEM_DATA_WIDTH = 32) (
    input  wire cpu_clk,             // cpu clock
    input  wire CLK,                // pixel clock
    input  wire RST_BTN,            // reset button
    input wire[3:0] reg_sel,        // register select/address
    output wire [VMEM_DATA_WIDTH-1:0] rd_data,
    input wire  [VMEM_DATA_WIDTH-1:0] wr_data,
    input wire                       rd_en,
    input wire                       wr_en,

    output wire VGA_HS,             // horizontal sync output
    output wire VGA_VS,             // vertical sync output
    output wire [3:0] VGA_R,        // 4-bit VGA red output
    output wire [3:0] VGA_G,        // 4-bit VGA green output
    output wire [3:0] VGA_B         // 4-bit VGA blue output
    );

    localparam BITS_PER_PIXEL = 4; localparam MAX_SHIFT_COUNT = 7;
    localparam REG_RD_ADDR = 0; localparam REG_WR_ADDR = 1; localparam REG_VMEM = 2;
    localparam REG_PAL_SLOT = 3; localparam REG_PAL_DATA = 4;
    localparam REG_CTL = 5;

    localparam COLOR_WIDTH = 12;
    localparam PALETTE_WIDTH = 4;

    // Display Clocks
    wire pix_clk = CLK;                   // pixel clock
    wire clk_lock = 1;                  // clock locked?

    wire vmem_rd_en;
    wire vmem_wr_en;
    wire [VMEM_DATA_WIDTH-1:0] vmem_rd_data;
    reg [VMEM_ADDR_WIDTH-1:0] cpu_rd_addr;
    reg [VMEM_ADDR_WIDTH-1:0] cpu_wr_addr;
    reg [VMEM_ADDR_WIDTH-1:0] pix_addr;
    wire [VMEM_DATA_WIDTH-1:0] pix_data;
    wire pix_rd;
    wire [VMEM_DATA_WIDTH-1:0] status;

    assign vmem_rd_en = rd_en;
    assign vmem_wr_en = (reg_sel == REG_VMEM) && wr_en;
    assign rd_data = (reg_sel == REG_VMEM)      ? vmem_rd_data :
                     (reg_sel == REG_RD_ADDR)   ? cpu_rd_addr :
                     (reg_sel == REG_WR_ADDR)   ? cpu_wr_addr :
                     (reg_sel == REG_CTL)       ? status :
                     32'hFFFFFFFF;

    wire [VMEM_ADDR_WIDTH-1:0] cpu_addr = vmem_wr_en ? cpu_wr_addr : cpu_rd_addr;

    bram_tdp #(.DATA(VMEM_DATA_WIDTH),.ADDR(VMEM_ADDR_WIDTH)) vram0 (
        .a_rd(vmem_rd_en), .a_clk(cpu_clk),
        .a_wr(vmem_wr_en), .a_addr(cpu_addr), .a_din(wr_data),
        .a_dout(vmem_rd_data),
        .b_clk(pix_clk), .b_addr(pix_addr), .b_dout(pix_data),
        .b_rd(pix_rd)
    );

    wire palette_wr_en = (reg_sel == REG_PAL_DATA) && wr_en;
    reg [PALETTE_WIDTH-1:0] palette_wr_slot;
    wire [COLOR_WIDTH-1:0] color_data;

    palette palette0(.wr_clk(cpu_clk), .rd_clk(pix_clk), .wr_en(palette_wr_en),
        .wr_slot(palette_wr_slot), .wr_data(wr_data[COLOR_WIDTH-1:0]),
        .rd_slot(pixel[3:0]), .rd_data(color_data));

    // Display Timings
    wire signed [15:0] sx;          // horizontal screen position (signed)
    wire signed [15:0] sy;          // vertical screen position (signed)
    wire h_sync;                    // horizontal sync
    wire v_sync;                    // vertical sync
    wire de;                        // display enable
    wire frame;                     // frame start
    wire scanline;                  // scanline start
    wire vblank;                    // vertical blank
    reg  vblank_buf;                // vertical blank in cpu clock domain

    display_timings #(              // 640x480  800x600 1280x720 1920x1080
`ifdef RES_1024_768
        .H_RES(1024),               //     640      800     1280      1920
        .V_RES(768),                //     480      600      720      1080
        .H_FP(24),                 //      16       40      110        88
        .H_SYNC(136),                //      96      128       40        44
        .H_BP(160),                 //      48       88      220       148
        .V_FP(3),                   //      10        1        5         4
        .V_SYNC(6),                 //       2        4        5         5
        .V_BP(29),                  //      33       23       20        36
        .H_POL(0),                  //       0        1        1         1
        .V_POL(0)                   //       0        1        1         1
 `endif
 `ifdef RES_640_400
        .H_RES(640),
        .V_RES(400),
        .H_FP(16),
        .H_SYNC(96),
        .H_BP(48),
        .V_FP(12),
        .V_SYNC(2),
        .V_BP(35),
        .H_POL(0),
        .V_POL(1)
 `endif
    )
    display_timings_inst (
        .i_pix_clk(CLK),
        .i_rst(!RST_BTN),
        .o_hs(h_sync),
        .o_vs(v_sync),
        .o_de(de),
        .o_frame(frame),
        .o_scanline(scanline),
        .o_vblank(vblank),
        .o_sx(sx),
        .o_sy(sy)
    );

    wire [7:0] red;
    wire [7:0] green;
    wire [7:0] blue;

    reg [VMEM_DATA_WIDTH-1:0] shifter;
    reg [4:0] shift_count;

    // delayed frame signal for pix_rd
    reg frame_d;

    assign pix_rd = frame_d || scanline || (shift_count == 2);

    assign status = { 4'b0001, {(VMEM_DATA_WIDTH-5){1'b0}}, vblank_buf};

    wire [BITS_PER_PIXEL-1:0] pixel = shifter[VMEM_DATA_WIDTH-1:VMEM_DATA_WIDTH-BITS_PER_PIXEL];

    always @(posedge pix_clk) frame_d <= frame;

    always @(posedge cpu_clk) vblank_buf <= vblank;

    always @(posedge cpu_clk)
    begin
        if(wr_en)
        begin
            case(reg_sel)
                REG_RD_ADDR: cpu_rd_addr <= wr_data;
                REG_WR_ADDR: cpu_wr_addr <= wr_data;
                REG_VMEM:    cpu_wr_addr <= cpu_wr_addr + 1; // auto-increment write addr on write
                REG_PAL_SLOT: palette_wr_slot <= wr_data[3:0];
            endcase
        end
        else
        if(rd_en && reg_sel == REG_VMEM) cpu_rd_addr <= cpu_rd_addr + 1;           // auto-increment read addr on read
    end

    always @(posedge pix_clk)
    begin
        if(scanline || shift_count == MAX_SHIFT_COUNT) // before start of a line
        begin                                       // or at the end of a word, reset shifter with pixel data
            shift_count <= 0;
            shifter <= pix_data;
        end
        else if(de)
        begin
            shift_count <= shift_count + 1;
            shifter <= shifter << BITS_PER_PIXEL;
        end

        if(frame)   // at start of frame, reset pixel pointer
            pix_addr <= 0;
        else if(shift_count == 1) // after the first pixel, increment address
            pix_addr <= pix_addr + 1;
    end

//    Hard-Coded RGBI palette
//    // Pixel = { red, green, blue, intensity }
//    assign red      = pixel[3] ? pixel[0] ? 255 : 127: 0;
//    assign green    = pixel[2] ? pixel[0] ? 255 : 127: 0;
//    assign blue     = pixel[1] ? pixel[0] ? 255 : 127: 0;

//    // VGA Output
//    // VGA Pmod is 12-bit so we take the upper nibble of each colour
//    assign VGA_HS   = h_sync;
//    assign VGA_VS   = v_sync;
//    assign VGA_R    = de ? red[7:4] : 4'b0;
//    assign VGA_G    = de ? green[7:4] : 4'b0;
//    assign VGA_B    = de ? blue[7:4] : 4'b0;

    // 12 bit RGB palette
    assign VGA_HS   = h_sync;
    assign VGA_VS   = v_sync;
    assign VGA_R    = de ? color_data[11:8] : 4'b0;
    assign VGA_G    = de ? color_data[7:4]  : 4'b0;
    assign VGA_B    = de ? color_data[3:0]  : 4'b0;
endmodule
