`timescale 1ns / 1ps
// taken from https://danstrother.com/2010/09/11/inferring-rams-in-fpgas/
// modified for one read/write-port and one read-only-port,
/// A parameterized, inferable, true dual-port, dual-clock block RAM in Verilog.

module palette #(
    parameter SLOTS_WIDTH = 4, COLOR_WIDTH = 12
) (
    input   wire                       wr_clk,
    input   wire                       rd_clk,
    input   wire                       wr_en,
    input   wire    [SLOTS_WIDTH-1:0]  wr_slot,
    input   wire    [COLOR_WIDTH-1:0]  wr_data,
    input   wire    [SLOTS_WIDTH-1:0]  rd_slot,
    output  wire    [COLOR_WIDTH-1:0]  rd_data
);

// Shared memory
    reg [COLOR_WIDTH-1:0] colors [(2**SLOTS_WIDTH)-1:0];

    assign rd_data = colors[rd_slot];

    always @(posedge wr_clk) begin
        if(wr_en) colors[wr_slot] <= wr_data;
    end

endmodule

