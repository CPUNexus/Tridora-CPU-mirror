`timescale 1ns / 1ns
`default_nettype none

module sdspi_testbench();
    parameter CLOCK_NS = 10;
    integer i,j;

    reg[7:0] rx_testdata[0:3];
    reg[7:0] read_data;

   // Test signals
    reg clk = 0;
    reg reset = 0;
   
    reg[7:0] tx_data = 0;
    wire[7:0] rx_data;
    wire     tx_ready;
    wire     tx_empty; 
    wire     rx_avail;
    wire     rx_ovr;
    reg      tx_write = 0;
    reg      rx_read = 0;

    wire     card_detect;
    wire     card_changed;
    wire     card_busy;

    reg      ctrl_write = 0;
    reg      rx_filter_en = 0;
    reg      txrx_en = 0;
    reg      spiclk_f_en = 0;
    reg      spiclk_div_wr = 0;

    // PMOD connections
    wire  sd_cs_n;
    wire  sd_mosi;
    reg  sd_miso = 1;
    wire sd_sck;
    reg  sd_cd = 1;

    // Unit Under Test
    sdspi UUT (
        .clk(clk),
        .reset(reset),
        .tx_data(tx_data),
        .rx_data(rx_data),
        .tx_ready(tx_ready),
        .tx_empty(tx_empty),
        .rx_avail(rx_avail),
        .rx_ovr(rx_ovr),
        .tx_write(tx_write),
        .rx_read(rx_read),
        .card_detect(card_detect),
        .card_changed(card_changed),
        .card_busy(card_busy),
        .ctrl_write(ctrl_write),
        .rx_filter_en(rx_filter_en),
        .txrx_en(txrx_en),
        .spiclk_f_en(spiclk_f_en),
        .spiclk_div_wr(spiclk_div_wr),
        .sd_cs_n(sd_cs_n),
        .sd_mosi(sd_mosi),
        .sd_miso(sd_miso),
        .sd_sck(sd_sck),
        .sd_cd(sd_cd)
        );

    // testbench clock
    always
        #(CLOCK_NS/2) clk <= ~clk;

    initial
    begin
        rx_testdata[0] <= 'hFF;
        rx_testdata[1] <= 'hFF;
        rx_testdata[2] <= 'hCA;
        rx_testdata[3] <= 'hFE;

        // issue reset
        reset = 1'b1;
        #10
        reset = 1'b0;
        #10

        // set clock divider
        tx_data <= 3;
        spiclk_div_wr <= 1'b1;
        #10
        spiclk_div_wr <= 1'b0;
        #10
 
        // Card initialization phase,
        // cycle the clock at least 74 times
        // while cs is off and mosi is high .
        // we do 32 cycles
        spiclk_f_en <= 1'b1;
        ctrl_write <= 1'b1;
        #10
        spiclk_f_en <= 1'b0;
        ctrl_write <= 1'b0;
        #10
        for (i=0; i<32*16; i = i + 1)
            #10;
        spiclk_f_en <= 1'b0;
        ctrl_write <= 1'b1;
        #10
        ctrl_write <= 1'b0;
        #10
        for (i=0; i<32*16; i = i + 1)
            #10;

        // Write two bytes
        tx_data  <= 8'hAB;
        tx_write <= 1'b1;
        #10;
        tx_data  <= 8'hCD;
        tx_write <= 1'b1;
        #10;
        tx_write <= 1'b0;
        #10

        // start transceiver, enable rx_filter
        txrx_en <= 1'b1;
        rx_filter_en <= 1'b1;
        ctrl_write <= 1'b1;
        #10
        txrx_en <= 1'b0;
        rx_filter_en <= 1'b0;
        ctrl_write <= 1'b0;

        for (i = 0; i < 2048; i = i + 1)
        begin
            if (!sd_cs_n && (i % 16)==15) sd_miso <= rx_testdata[(i/(16*8)) % 4][7 - (i/16 % 8)];
            #10;
        end
        tx_write <= 1'b0;
        #10

        // read from rx fifo
        read_data <= rx_data;
        #10
        $display("read data 1: %02h", read_data);


        // strobe rx_read to go to next byte
        rx_read <= 1'b1;
        #10  // one cycle to transfer the data
        rx_read <= 1'b0;
        #10  // we need this extra cycle for the fifo tail to move

        read_data <= rx_data;
        #10
        $display("read data 2: %02h", read_data);

        // strobe rx_read to go to next byte
        rx_read <= 1'b1;
        #10
        rx_read <= 1'b0;
        #10  // we need this extra cycle for the fifo tail to move

        read_data <= rx_data;
        #10
        $display("read data 3: %02h", read_data);


        // set flag to turn transceiver off
        txrx_en <= 1'b0;
        ctrl_write <= 1'b1;
        #10
        ctrl_write <= 1'b0;
        #10;
 
        // wait for the transceiver to actually turn off
        for (i=0; i<32*16; i = i + 1)
            #10;

        #10
        // clear rx fifo
        for(i=0; i<14; i = i + 1)
        begin
            $display("clear fifo data %02h", rx_data);
            rx_read <= 1'b1;
            #10
            rx_read <= 1'b0;
            #10
            #10
            #10;     // simulate the four cycles of an instruction
        end

        $finish();
    end

    initial 
    begin
    // Required to dump signals
    $dumpfile("sdspi_tb_dump.vcd");
    $dumpvars(0);
    end
endmodule
