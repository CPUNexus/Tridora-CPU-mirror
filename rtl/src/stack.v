`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 17.01.2021 20:59:29
// Design Name: 
// Module Name: stack
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module stack
    #(parameter ADDR_WIDTH=4, DATA_WIDTH=16)
    (
    input wire clk,
    input wire [ADDR_WIDTH-1:0] rd_addr,
    input wire [ADDR_WIDTH-1:0] wr_addr,
    input wire wr_enable,
    output wire [DATA_WIDTH-1:0] rd_data,
    input wire [DATA_WIDTH-1:0] wr_data
    );
    
    reg [DATA_WIDTH-1:0] stack[0:2**ADDR_WIDTH-1];
    
    always @(posedge clk)
    begin
        if(wr_enable) stack[wr_addr] <= wr_data;
    end
    
    assign rd_data = stack[rd_addr];
endmodule
