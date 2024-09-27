`timescale 1ns / 1ps

// a simple fifo
module fifo #(parameter DATA_WIDTH = 8, ADDR_WIDTH = 4)(
    input wire clk,
    input wire reset,
    input wire wr_en,
    input wire rd_en,
    input wire [DATA_WIDTH-1:0] wr_data,
    output wire [DATA_WIDTH-1:0] rd_data,
    output wire wr_full,
    output wire rd_empty
    );

    reg [DATA_WIDTH-1:0] mem [0:2**ADDR_WIDTH-1];
    reg [ADDR_WIDTH:0]  head_x = 0;  // head and tail have one extra bit 
    reg [ADDR_WIDTH:0]  tail_x = 0;  // for detecting overflows
    wire [ADDR_WIDTH-1:0] head = head_x[ADDR_WIDTH-1:0];
    wire [ADDR_WIDTH-1:0] tail = tail_x[ADDR_WIDTH-1:0];
    
    assign rd_data = mem[tail];
    // the fifo is full when head and tail pointer are the same
    // and the extra bits differ (a wraparound occured)
    assign wr_full = (head == tail) && (head_x[ADDR_WIDTH] != tail_x[ADDR_WIDTH]);
    // the fifo is empty when head and tail pointer are the same
    // and the extra bits are the same (no wraparound)
    assign rd_empty = (head == tail) && (head_x[ADDR_WIDTH] == tail_x[ADDR_WIDTH]);
  
    // Writing to FIFO
    always @(posedge clk) begin
        if (reset)
            head_x <= 0;
        else if (wr_en)
        begin
            mem[head] <= wr_data;
            // move head, possible wraparound
            head_x <= head_x + 1'b1;
        end
    end

    // Reading from FIFO
    always @(posedge clk)
    begin
        if (reset)
            tail_x <= 0;
        else if (rd_en)
        begin
            // rd_data always has current tail data
            // move tail, possible wraparound
            tail_x <= tail_x + 1'b1;
        end
    end
endmodule