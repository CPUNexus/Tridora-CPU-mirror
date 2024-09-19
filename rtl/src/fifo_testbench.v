`timescale 1ns / 1ns
`default_nettype none

module fifo_testbench();
   // Test signals
    reg clk = 0;
    reg reset = 0;
    reg wr_en = 0;
    reg rd_en = 0;
    reg [7:0] wr_data = 0;
    wire  [7:0] rd_data;
    wire wr_full;
    wire rd_empty;

    parameter CLOCK_NS = 10;

    // Unit Under Test
    fifo #(
        .DATA_WIDTH(8),
        .ADDR_WIDTH(4)
    ) UUT (
        .clk(clk),
        .reset(reset),
        .wr_en(wr_en),
        .rd_en(rd_en),
        .rd_data(rd_data),
        .wr_data(wr_data),
        .wr_full(wr_full),
        .rd_empty(rd_empty)
        );

    // testbench clock
    always
        #(CLOCK_NS/2) clk <= ~clk;

    initial
    begin
        // issue reset
        reset = 1'b1;
        #10
        reset = 1'b0;
        #10

        // Write two bytes
        wr_data  <= 8'hAB;
        wr_en <= 1'b1;
        #10;
        wr_data  <= 8'hCD;
        wr_en <= 1'b1;
        #10;
        wr_en <= 1'b0;
        #10
        
        // read fifo tail
        if (rd_data == 8'hAB)
        $display("Pass - Byte 1");
        else
        $display("Failed - Byte 2");

        // read/remove byte from tail
        rd_en  <= 1'b1;
        #10
        // check next byte
        if (rd_data == 8'hCD)
        $display("Pass - Byte 2");
        else
        $display("Failed - Byte 2");
        
        // remove 2nd byte
        rd_en <= 1'b1;
        #10

        rd_en <= 1'b0;
        #10

        // Write until full
        rd_en <= 1'b0;
        wr_en  <= 1'b0;

        for (integer i = 0; i < 16; i = i + 1) begin
            wr_data  <= i;
            wr_en <= 1'b1;
            #10;
        end
        wr_en <= 1'b0;
        
        if (wr_full)
        $display("Pass - Fifo full");
        else
        $display("Failed - Fifo full");


        // read until empty
        rd_en <= 1'b0;
        wr_en  <= 1'b0;

        for (integer i = 0; i < 16; i = i + 1) begin
            rd_en <= 1'b1;
            #10;
        end
        rd_en <= 1'b0;
        
        if (rd_empty)
        $display("Pass - Fifo empty");
        else
        $display("Failed - Fifo empty");
        $finish();
    end

    initial 
    begin
    // Required to dump signals
    $dumpfile("fifo_tb_dump.vcd");
    $dumpvars(0);
    end
endmodule
