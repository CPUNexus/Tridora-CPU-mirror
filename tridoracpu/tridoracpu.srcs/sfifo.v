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

    reg [DATA_WIDTH-1:0] mem [0:ADDR_WIDTH-1];
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
        if (wr_en)
        begin
            mem[head[ADDR_WIDTH-1:0]] <= wr_data;
            // move head, possible wraparound
            head_x <= head_x + 1'b1;
        end
    end

    // Reading from FIFO
    always @(posedge clk)
    begin
        if (rd_en)
        begin
            // rd_data always has current tail data
            // move tail, possible wraparound
            tail <= tail + 1'b1;
        end
    end

    // Calculating full/empty flags, referenced from zipcpu.com
    wire	[c_DEPTH:0]	dblnext, nxtread;
    assign	dblnext = wraddr + 2;
    assign	nxtread = rdaddr + 1'b1;

    always @(posedge i_Clock, negedge i_Reset)
    
        // Reset case
        if (!i_Reset)
        begin
            // Reset output flags
            fifo_full <= 1'b0;
            fifo_empty <= 1'b1;
            
        end else casez({ i_Write_En, i_Read_En, !fifo_full, !fifo_empty })
        4'b01?1: begin	// A successful read
            fifo_full  <= 1'b0;
            fifo_empty <= (nxtread == wraddr);
        end
        4'b101?: begin	// A successful write
            fifo_full <= (dblnext == rdaddr);
            fifo_empty <= 1'b0;
        end
        4'b11?0: begin	// Successful write, failed read
            fifo_full  <= 1'b0;
            fifo_empty <= 1'b0;
        end
        4'b11?1: begin	// Successful read and write
            fifo_full  <= fifo_full;
            fifo_empty <= 1'b0;
        end
        default: begin end
        endcase
    
endmodule