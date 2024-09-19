`timescale 1ns / 1ps
// either define clock as clk (100MHz on Arty)
// or as clk_1hz for debugging

`define clock cpuclk
`define clkfreq 83333333
//`define clock clk
//`define clkfreq 100000000
//`define clock clk_1hz
`define ENABLE_VGAFB
`define ENABLE_MICROSD

module top(
    input wire clk,
    input wire rst,
    input wire btn0,
    input wire sw0,
    input wire sw1,
    output wire led0,
    output wire led1,
    output wire led2,
    output wire led3,
    input wire uart_txd_in,
    output wire uart_rxd_out,

     // DDR3 SDRAM
    inout wire [15:0]   ddr3_dq,
    inout wire [1:0]    ddr3_dqs_n,
    inout wire [1:0]    ddr3_dqs_p,

    output wire [13:0]  ddr3_addr,
    output wire [2:0]   ddr3_ba,
    output wire         ddr3_ras_n,
    output wire         ddr3_cas_n,
    output wire         ddr3_we_n,
    output wire         ddr3_reset_n,
    output wire [0:0]   ddr3_ck_p,
    output wire [0:0]   ddr3_ck_n,
    output wire [0:0]   ddr3_cke,
    output wire [0:0]   ddr3_cs_n,
    output wire [1:0]   ddr3_dm,
    output wire [0:0]   ddr3_odt

`ifdef ENABLE_VGAFB
    ,
    output wire [3:0] VGA_R,
    output wire [3:0] VGA_G,
    output wire [3:0] VGA_B,

    output wire VGA_HS_O,
    output wire VGA_VS_O
`endif

`ifdef ENABLE_MICROSD
    ,
    output wire sd_cs_n,
    output wire sd_mosi,
    input wire  sd_miso,
    output wire sd_sck,
    input wire  sd_cd
`endif
);

    reg clk_1hz;
    reg [31:0] counter;

    localparam ADDR_WIDTH = 32, WIDTH = 32,
            ROMADDR_WIDTH = 11, IOADDR_WIDTH = 11, IOADDR_SEL = 4;
    
    wire [ADDR_WIDTH-1:0] mem_addr;
    wire [WIDTH-1:0] mem_read_data;
    wire [WIDTH-1:0] mem_write_data;
    (* KEEP *) wire mem_wait;

    (* KEEP *) wire mem_read_enable;
    (* KEEP *) wire mem_write_enable;
    (* KEEP *) wire io_enable;
    wire [WIDTH-1:0] io_rd_data;
    wire [IOADDR_SEL-1:0] io_slot = mem_addr[IOADDR_WIDTH-1:IOADDR_WIDTH-IOADDR_SEL];

    wire irq;

    // assign led0 = mem_wait;
    
    wire [WIDTH-1:0] debug_data1, debug_data2,
        debug_data3, debug_data4,
        debug_data5, debug_data6;

    assign led0 = debug_data6[0];

    wire cpuclk, cpuclk_locked;
    wire dram_refclk200;
    wire pixclk;
    cpu_clkgen cpuclk_0(~rst, clk, cpuclk, dram_refclk200, pixclk, cpuclk_locked); 

    // DRAM --------------------------------------------------------------------------
    wire [ADDR_WIDTH-1:0] dram_addr;
    wire [WIDTH-1:0] dram_read_data, dram_write_data;
    wire dram_read_enable, dram_write_enable, dram_wait;

    dram_bridge dram_bridge0 (dram_addr,
        dram_read_data, dram_write_data, dram_read_enable, dram_write_enable, dram_wait,
        rst, cpuclk, dram_refclk200,
        ddr3_dq, ddr3_dqs_n, ddr3_dqs_p, ddr3_addr,
        ddr3_ba, ddr3_ras_n, ddr3_cas_n, ddr3_we_n,
        ddr3_reset_n, ddr3_ck_p, ddr3_ck_n, ddr3_cke,
        ddr3_cs_n, ddr3_dm, ddr3_odt);

    mem #(.ADDR_WIDTH(ADDR_WIDTH), .DATA_WIDTH(WIDTH)) mem0(
        .clk(`clock), .rst_n(rst), .addr(mem_addr),
        .data_out(mem_read_data), .read_enable(mem_read_enable),
        .data_in(mem_write_data), .write_enable(mem_write_enable),
        .io_enable(io_enable),
        .io_rd_data(io_rd_data),
        .mem_wait(mem_wait),
        .dram_addr(dram_addr),
        .dram_read_data(dram_read_data),
        .dram_write_data(dram_write_data),
        .dram_read_enable(dram_read_enable),
        .dram_write_enable(dram_write_enable),
        .dram_wait(dram_wait)
    );

`ifdef ENABLE_VGAFB
    localparam FB_ADDR_WIDTH = 14;
    wire [FB_ADDR_WIDTH-1:0] fb_rd_addr;
    wire [FB_ADDR_WIDTH-1:0] fb_wr_addr;
    wire [WIDTH-1:0] fb_rd_data;
    wire [WIDTH-1:0] fb_wr_data;
    wire fb_rd_en, fb_wr_en;

    wire fb_cs_en = io_enable && (io_slot == 2);

    assign fb_rd_en = fb_cs_en && mem_read_enable;
    assign fb_wr_en = fb_cs_en && mem_write_enable;
    assign fb_wr_data = mem_write_data;

    vgafb vgafb0(`clock, pixclk, rst,
        mem_addr[3:0], fb_rd_data, fb_wr_data,
        fb_rd_en, fb_wr_en,
        VGA_HS_O, VGA_VS_O, VGA_R, VGA_G, VGA_B);
`endif

    // SPI SD card controller -------------------------------------------------------------------
`ifdef ENABLE_MICROSD
    wire [7:0]  spi_tx_data;
    (*KEEP*) wire [7:0]  spi_rx_data;
    wire        spi_tx_ready;   // ready to transmit new data
    wire        spi_tx_empty;   // tx fifo is empty
    wire        spi_rx_avail;   // a byte has been received
    wire        spi_rx_ovr;     // receiver overrun
    wire        spi_tx_write;   // write strobe
    wire        spi_rx_read;    // read strobe (clears rx_avail)

    wire        spi_card_detect;  // true is card is present
    wire        spi_card_changed; // card_detect signal has changed
    wire        spi_card_busy;    // card is busy (MISO/DO is 0)

    wire        spi_ctrl_write;  // set the following flags
    wire        spi_rx_filter_en;     // set to wait for start bit (1-to-0) when receiving
    wire        spi_txrx_en;       // enable transmitter and receiver
    wire        spi_sclk_f_en;     // enable spi clock without transceiver
    wire        spi_sclk_div_wr; // set clock divider from tx_data

    wire        spi_cs;         // cs signal for spi controller
    wire [WIDTH-1:0] spi_rd_data;

    assign spi_cs = io_enable && (io_slot == 1);

    // spi read data: [ 0,...,0,cd,cc,cb,tr,te,ra,ro,d,d,d,d,d,d,d,d ]
    // cd = card detect, cc = card changed, cb = card busy,
    // tr = transmitter ready, te = tx fifo empty,
    // ra = received byte available, ro = receive overrun, d = received byte
    assign spi_rd_data =
            { {WIDTH-15{1'b0}}, spi_card_detect, spi_card_changed, spi_card_busy,
                spi_tx_ready, spi_tx_empty,
                spi_rx_avail, spi_rx_ovr, spi_rx_data };

    // spi write data: [ 0,...,0,CW,CF,Cx,Cc,Cd,DR,DW,d,d,d,d,d,d,d,d ]
    // CW = control write, CF = enable receive filter, Cx = enable transceiver,
    // Cc = force spi clock on, Cd = write clock divider,
    // DR = read acknowledge, DW = data write, d = byte to be sent
    assign spi_ctrl_write =     spi_cs && mem_write_enable && mem_write_data[14];
    assign spi_rx_filter_en =   mem_write_data[13];
    assign spi_txrx_en   =      mem_write_data[12];
    assign spi_sclk_f_en  =     mem_write_data[11];
    assign spi_sclk_div_wr  =   spi_cs && mem_write_enable && mem_write_data[10];
    assign spi_rx_read =        mem_write_data[9];
    assign spi_tx_write =       spi_cs && mem_write_enable && mem_write_data[8];
    assign spi_tx_data =        mem_write_data[7:0];

    sdspi sdspi0(.clk(`clock), .reset(~rst),
            .tx_data(spi_tx_data), .rx_data(spi_rx_data),
            .tx_ready(spi_tx_ready), .tx_empty(spi_tx_empty),
            .rx_avail(spi_rx_avail), .rx_ovr(spi_rx_ovr),
            .tx_write(spi_tx_write), .rx_read(spi_rx_read),
            .card_detect(spi_card_detect), .card_changed(spi_card_changed), .card_busy(spi_card_busy),
            // ctrl_write is used with rx_filter_en, txrx_en and spiclk_f_en
            .ctrl_write(spi_ctrl_write),
            .rx_filter_en(spi_rx_filter_en), .txrx_en(spi_txrx_en), .spiclk_f_en(spi_sclk_f_en),
            //
            .spiclk_div_wr(spi_sclk_div_wr),
            .sd_cs_n(sd_cs_n),
            .sd_mosi(sd_mosi), .sd_miso(sd_miso), .sd_sck(sd_sck), .sd_cd(sd_cd));
`endif

    // UART -----------------------------------------------------------------------

    // uart write data: [ 0, 0, 0, 0, 0, T, C, 0, c, c, c, c, c, c, c, c ]
    // T = transmit enable, C = receiver clear, c = 8-bit-character
    // uart read data:  [ 0, 0, 0, 0, 0, 0, A, B, c, c, c, c, c, c, c, c ]
    // A = char available, B = tx busy, c = 8-bit-character
    wire uart_cs = io_enable && (io_slot == 0);
    wire uart_tx_en = uart_cs && mem_write_enable && mem_write_data[10];
    wire uart_rx_clear = uart_cs && mem_write_enable && mem_write_data[9];
    wire uart_rx_avail;
    wire uart_rx_busy, uart_tx_busy;
    wire uart_err;
    wire [7:0] uart_rx_data;
    wire [7:0] uart_tx_data;    
    wire [31:0] uart_baud = 32'd115200;
    wire [WIDTH-1:0] uart_rd_data;

    assign uart_tx_data = mem_write_data[7:0];
    assign uart_rd_data = { {WIDTH-10{1'b1}}, uart_rx_avail, uart_tx_busy, uart_rx_data };

    reg timer_tick;
    reg[23:0] tick_count;
    wire [1:0] irq_in = { timer_tick, uart_rx_avail };
    wire [1:0] irqc_rd_data0;
    wire [WIDTH-1:0] irqc_rd_data = { tick_count, 6'b0, irqc_rd_data0 };
    wire irqc_seten = mem_write_data[7];
    wire irqc_cs = io_enable && (io_slot == 3);

    assign io_rd_data = (io_slot == 0) ? uart_rd_data :
    `ifdef ENABLE_MICROSD
                        (io_slot == 1) ? spi_rd_data :
    `endif
    `ifdef ENABLE_VGAFB
                        (io_slot == 2) ? fb_rd_data :
    `endif
                        (io_slot == 3) ? irqc_rd_data:

                        -1;

    buart #(.CLKFREQ(`clkfreq)) uart0(`clock, rst,
        uart_baud,
        uart_txd_in, uart_rxd_out,
        uart_rx_clear, uart_tx_en,
        uart_rx_avail, uart_tx_busy,
        uart_tx_data, uart_rx_data);

     // CPU -----------------------------------------------------------------
    stackcpu cpu0(.clk(`clock), .rst(rst), .irq(irq),
        .addr(mem_addr),
        .data_in(mem_read_data), .read_enable(mem_read_enable),
        .data_out(mem_write_data), .write_enable(mem_write_enable),
        .mem_wait(mem_wait),
        .led1(led1), .led2(led2), .led3(led3),
        .debug_out1(debug_data1),
        .debug_out2(debug_data2),
        .debug_out3(debug_data3),
        .debug_out4(debug_data4),
        .debug_out5(debug_data5),
        .debug_out6(debug_data6));

    // Interrupt Controller
    irqctrl irqctrl0(`clock, irq_in, irqc_cs, mem_write_enable,
            irqc_seten, irqc_rd_data0,
            irq);

    // count clock ticks
    // generate interrupt every 20nth of a second
    always @ (posedge `clock)
    begin
        counter <= counter + 1;
        if (counter >= (`clkfreq/20))
        begin
                counter <= 0;
                timer_tick <= 1;
                tick_count <= tick_count + 1'b1;
        end
        else
        begin
            timer_tick <= 0;
        end
    end
endmodule
