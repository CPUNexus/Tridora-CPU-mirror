`timescale 1ns / 1ps


// Every spi_clk_div cpu clock cycles the spi clock line is inverted.
// So a spi clock cycle is 2*spi_clk_div cpu clock cycles.
// spi_clk_count counts the cpu cycles from spi_clk_div down to zero.
// The resulting spi clock frequency is:
//      (cpu clock freq) / ((sclk_count + 1) * 2)
// So for a 83.33 MHz cpu clock, we get
// spi_clk_div = 10:   83.333 / 22  = 3.788 MHz
// spi_clk_div = 124: 83.333 / 250  = 333.33 KHz

 
module sdspi(
    input wire      clk,        // bus clock
    input wire      reset,
    
    input wire[7:0] tx_data,    // data to transmit
    output wire[7:0] rx_data,   // data received
    output wire     tx_ready,   // ready to write a data byte
    output wire     tx_empty,   // transmitter fifo is empty 
    output wire     rx_avail,   // a byte has been received
    output wire     rx_ovr,     // receiver overrun
    input wire      tx_write,   // write strobe
    input wire      rx_read,    // read strobe (clears rx_ovr)

    output wire     card_detect,  // true is card is present
    output wire     card_changed, // card_detect signal has changed
    output wire     card_busy,    // card is busy (MISO/DO is 0)

    input wire      ctrl_write,     // set the following flags
    input wire      rx_filter_en,      // set to discard received $FF bytes
    input wire      txrx_en,        // enable transmitter and receiver
    input wire      spiclk_f_en,    // enable spi clock without cs
    input wire      spiclk_div_wr,  // set clock divider  via tx_data

    // PMOD connections
    output wire  sd_cs_n,
    output reg  sd_mosi,
    input wire  sd_miso,
    output wire sd_sck,
    input wire  sd_cd
    );

    localparam CLKPHASE_0A = 2'b00;
    localparam CLKPHASE_0B = 2'b01;
    localparam CLKPHASE_1A = 2'b10;
    localparam CLKPHASE_1B = 2'b11;
    reg [1:0] clk_phase;

    reg     xcvr_on;        // if turned off, the rest of the current byte
                            // will still transmitted, and until then "running"
                            // will be 1
    (* KEEP *)
    reg     running;        // transmitting/receiving a byte (maybe a dummy byte)

    (* KEEP *) reg [3:0] xcvr_bitcount;  // number of bits left of the current byte

    reg [7:0] tx_shifter;
    wire     tx_fifo_wr_en;
    reg     tx_fifo_rd_en;
    wire    tx_fifo_full;
    wire    tx_fifo_empty;
    wire [7:0] tx_fifo_out;
 
    reg [7:0] rx_shifter;
    reg     rx_filter;
    reg     rx_fifo_wr_en;
    wire    rx_fifo_rd_en;
    wire    rx_fifo_full;
    wire    rx_fifo_empty;
    wire [7:0] rx_fifo_out;

    reg     rx_bit_recvd;   // this flag signals a received bit

    reg     rx_overrun; // byte received when rx fifo is full

    reg     c_changed;
    reg     c_cs;

    reg       spi_clk;          // the spi clock signal
    reg       spi_clk_on;       // enable clock, either via init mode or by xcvr_on
    reg       spi_clk_f_on;     // init clock mode, i.e. start clock but no tx/rx
    reg [6:0] spi_clk_count;    // counting cpu clock ticks
    reg [6:0] spi_clk_div;      // tick counter for spi clock phases
    wire      spi_clk_count_z = (spi_clk_count == 7'b0);
    reg       hphase_start;     // start of a spi clock half-phase

    assign tx_ready = !tx_fifo_full;
    assign tx_empty = tx_fifo_empty;
    assign rx_avail = !rx_fifo_empty;
    assign rx_ovr   = rx_overrun;
    assign rx_data = rx_fifo_out;

    assign card_busy = (sd_miso == 0);
    assign card_changed = c_changed;

    assign sd_sck = spi_clk;
    assign sd_cs_n = ~c_cs;

    assign card_detect = sd_cd;

    fifo #(.ADDR_WIDTH(4)) tx_fifo(clk, reset,
        tx_fifo_wr_en, tx_fifo_rd_en,
        tx_data, tx_fifo_out,
        tx_fifo_full,
        tx_fifo_empty
    );

    fifo #(.ADDR_WIDTH(8)) rx_fifo(clk, reset,
        rx_fifo_wr_en, rx_fifo_rd_en,
        rx_shifter, rx_fifo_out,
        rx_fifo_full,
        rx_fifo_empty   
    );

    // spi clock
    always @(posedge clk)
    begin
        if(reset)
        begin
            spi_clk <= 1;   // CLK is high when inactive
            spi_clk_on <= 0;
            spi_clk_count <= 0;
        end
        else if(spi_clk_on)
        begin

            // set spi_clk at start of every half-phase
            if(hphase_start)
            case(clk_phase)
                     CLKPHASE_0A:   spi_clk <= 1'b0;
                     CLKPHASE_0B:   spi_clk <= 1'b0;
                     CLKPHASE_1A:   spi_clk <= 1'b1;
                     CLKPHASE_1B:   spi_clk <= 1'b1;
            endcase

            if(spi_clk_count_z)
            begin
                spi_clk_count <= spi_clk_div;
                clk_phase <= clk_phase + 2'b1;
            end
            else
                spi_clk_count <= spi_clk_count - 7'd1;
        end

        // start the clock if needed
        if( (spi_clk_on == 0) && (running || spi_clk_f_on))
        begin
            spi_clk_on <= 1;
            spi_clk_count <= spi_clk_div;
            clk_phase <= CLKPHASE_1A;
        end

        // turn off the clock if transceiver not running
        // and the force-clock-on flag is not set
        if( (spi_clk_on == 1) && (!running && !spi_clk_f_on))
        begin
            spi_clk_on <= 0;
            spi_clk <= 1'b1;
        end
    end

    // half-phase-start flag trails spi_clk_count_z by one tick
    always @(posedge clk)
    begin
        if(reset)
            hphase_start <= 0;
        else
            hphase_start <= spi_clk_on && spi_clk_count_z;
    end

    // handle the force clock enable flag
    always @(posedge clk)
    begin
        if (reset)
            spi_clk_f_on <= 0;
        else
        if (ctrl_write)
            spi_clk_f_on <= spiclk_f_en;
    end

    // clock divider
    always @(posedge clk)
    begin
        if (spiclk_div_wr) spi_clk_div <= tx_data[6:0];
    end

    // card_changed flag
    always @(posedge clk)
    begin
        if(sd_cd)
            c_changed <= 1;
        else if(ctrl_write || reset)
            c_changed <= 0;
    end

    // cs signal
    always @(posedge clk)
    begin
        if(hphase_start && clk_phase == CLKPHASE_0A || !running)
            c_cs <= running;
    end

    // transmitter
    always @(posedge clk)
    begin
        if(reset)
        begin
            // ???? we start the bitcount at 1 because we start
            // at the second clock phase where the bitcount
            // is decremented and the next byte gets loaded
            xcvr_bitcount <= 0;

            tx_shifter <= 8'b1;
            xcvr_on <= 0;
            sd_mosi <= 1;
            tx_fifo_rd_en <= 0;
        end
        else
        begin
            // handle a control write to disable the transceiver
            if(ctrl_write && !txrx_en && xcvr_on)
                xcvr_on <= 0;
		// a byte might still be in transit, so
		// we do not disable the transceiver
		// immediately (see "handle running status" below)
            else
            // handle control write to enable the transceiver
            if(ctrl_write && txrx_en && !running)
            begin
                xcvr_on <= 1;
                xcvr_bitcount <= 0;
                // next clock phase must be 1B when starting the transceiver,
                // so that the first byte is loaded into the shifter then
                tx_shifter <= 8'b11111111;
                // in case the transceiver is enabled, but no data is in the fifo,
                // initialize the shifter with $FF
            end
            else
            // handle clock phases
            if (running)
            begin
                if(hphase_start)
                case(clk_phase)
                                            // set mosi signal at start of clock pulse
                             CLKPHASE_0A:   sd_mosi <= tx_shifter[7];
                             CLKPHASE_0B:   ;
                             CLKPHASE_1A:   begin   // shift at rising clock
                                                tx_shifter <= tx_shifter << 1;
                                                xcvr_bitcount <= xcvr_bitcount - 1;
                                            end
                             CLKPHASE_1B:   begin   // in the middle of the high clock pulse,
                                                    // fetch the next byte if there are no bits
                                                    // left in the shift register
                                                if (xcvr_bitcount == 0)
                                                begin
                                                    if(!tx_fifo_empty)
                                                    begin
                                                        tx_shifter <= tx_fifo_out;
                                                        tx_fifo_rd_en <= 1;
                                                    end
                                                    else
                                                        tx_shifter <= 8'b11111111;
                                                    xcvr_bitcount <= 8;
                                                end
                                                else
                                                    tx_fifo_rd_en <= 0;
                                            end
                endcase
                else
                    tx_fifo_rd_en <= 0;
            end
        end
    end

    // handle data write
    assign tx_fifo_wr_en = tx_write && !tx_fifo_full;

    // Enable fifo read signal if fifo is not empty.
    // The data at the fifo tail is always available at
    // rx_data, the read signal just moves the tail pointer
    // forward.
    assign rx_fifo_rd_en = rx_read && !rx_fifo_empty;

    // receiver
    always @(posedge clk)
    begin
        if(reset)
        begin
            rx_bit_recvd <= 0;
            rx_shifter <= 8'b11111111;
            rx_fifo_wr_en <= 0;
            rx_filter <= 0;
            rx_overrun <= 0;
        end
        else
        begin
            // handle a control write
            if(ctrl_write)
            begin
                rx_filter <= rx_filter_en;
                rx_overrun <= 0;
                if(txrx_en && !running)
                    rx_shifter <= 8'b0;
            end

            if (running && hphase_start)
            case(clk_phase)
                         CLKPHASE_0A:   ;
                         CLKPHASE_0B:   ;
                         CLKPHASE_1A:   ;
                         CLKPHASE_1B:   begin   // in the middle of the high clock pulse,
                                                // sample MISO and put into shift register
                                                // and shift at the same time
                                            rx_shifter <= { rx_shifter[6:0],sd_miso};
                                            rx_bit_recvd <= 1;
                                        end
            endcase

            if (rx_bit_recvd && !sd_cs_n && clk_phase == CLKPHASE_1B)
            begin
                rx_bit_recvd <= 0;

                // if a complete byte was received, bitcount will be
                // 8 because the transmitter has already loaded the next byte
                // at this half-phase
                if (xcvr_bitcount == 8)
                begin
                    // discard $FF bytes if filter is enabled
                    if(!rx_filter || rx_shifter != 8'b11111111)
                    begin
                        if(rx_fifo_full)        // discard received byte if fifo is full
                            rx_overrun <= 1;    // and set overrun flag
                        else
                            rx_fifo_wr_en <= 1; // otherwise, enable fifo write strobe,
                                                // fifo will take data from rx_shifter
                    end

                    // turn off filter if a byte != $FF was received
                    if (rx_filter && rx_shifter != 8'b11111111)
                        rx_filter <= 0;
                end
            end
            else
                rx_fifo_wr_en <= 0;

        end
    end

    // handle running status
    // (especially keep transmitter running when there are still bits left to be
    // transmitted)
    always@(posedge clk)
    begin
        if (reset)
            running <= 0;
        else
        begin
            // if we want to turn the transceiver on, set running flag
            if (!running && xcvr_on)
                running <= 1;

            // when running and a byte has been transmitted,
            // check if we should turn the transceiver off
            if (running && hphase_start && xcvr_bitcount==0)
                if(clk_phase == CLKPHASE_1B)
                    if (!xcvr_on)
                        running <= 0;
        end
    end
endmodule
