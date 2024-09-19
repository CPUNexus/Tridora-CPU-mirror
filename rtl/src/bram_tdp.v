`timescale 1ns / 1ps
// taken from https://danstrother.com/2010/09/11/inferring-rams-in-fpgas/
// modified for one read/write-port and one read-only-port,
/// A parameterized, inferable, true dual-port, dual-clock block RAM in Verilog.

module bram_tdp #(
    parameter DATA = 72,
    parameter ADDR = 10
) (
    // Port A
    input   wire                a_clk,
    input   wire                a_rd,
    input   wire                a_wr,
    input   wire    [ADDR-1:0]  a_addr,
    input   wire    [DATA-1:0]  a_din,
    output  reg     [DATA-1:0]  a_dout,
    // Port B
    input   wire                b_clk,
    input   wire    [ADDR-1:0]  b_addr,
    output  reg     [DATA-1:0]  b_dout,
    input   wire                b_rd
);

// Shared memory
    reg [DATA-1:0] mem [(2**ADDR)-1:0];

    wire a_en = a_rd || a_wr;

// Port A
always @(posedge a_clk) begin
    if(a_en)
    begin
        if(a_wr)
            mem[a_addr] <= a_din;
        else if(a_rd)
            a_dout <= mem[a_addr];
    end
end

// Port B
always @(posedge b_clk) begin
    if(b_rd)
        b_dout      <= mem[b_addr];
end

endmodule
