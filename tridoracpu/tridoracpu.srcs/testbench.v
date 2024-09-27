`timescale 1ns/1ps
`default_nettype none

module testbench();
    reg clk;
    reg rst_n;
    wire btn0;
    wire sw0;
    wire sw1;
    wire led0;
    wire led1;
    wire led2;
    wire led3;
    wire uart_txd_in;
    wire uart_rxd_out;
    
    wire [15:0]   ddr3_dq;
    wire [1:0]    ddr3_dqs_n;
    wire [1:0]    ddr3_dqs_p;
    wire [13:0]  ddr3_addr;
    wire [2:0]   ddr3_ba;
    wire         ddr3_ras_n;
    wire         ddr3_cas_n;
    wire         ddr3_we_n;
    wire         ddr3_reset_n;
    wire [0:0]   ddr3_ck_p;
    wire [0:0]   ddr3_ck_n;
    wire [0:0]   ddr3_cke;
    wire [0:0]   ddr3_cs_n;
    wire [1:0]   ddr3_dm;
    wire [0:0]   ddr3_odt;

    integer t;

    top top0(clk, rst_n, btn0, sw0,sw1, led0, led1, led2, led3, uart_txd_in, uart_rxd_out,
        ddr3_dq, ddr3_dqs_n, ddr3_dqs_p, ddr3_addr, ddr3_ba, ddr3_ras_n, ddr3_cas_n,
        ddr3_we_n, ddr3_reset_n, ddr3_ck_p, ddr3_ck_n, ddr3_cke, ddr3_cs_n, ddr3_dm, ddr3_odt);        

    initial begin
      clk = 1;
      t = 0;
      rst_n = 0;
    end

    always #5.0 clk = ~clk;

    always @(posedge clk) begin
        t <= t + 1;
        if (t == 2)
            rst_n = 1;
        if (t == 400)
            $finish;
    end
endmodule
