`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 05.01.2021 21:53:41
// Design Name: 
// Module Name: mem
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

// 32 bit wide rom with byte addressing (address bits 1-0 are ignored)
module rom32 #(parameter ADDR_WIDTH = 11, DATA_WIDTH = 32)
(
    input wire clk,
    input wire [ADDR_WIDTH-1:0] addr,
    output reg [DATA_WIDTH-1:0] data_out,
    input wire read_enable
    );
    
  wire [ADDR_WIDTH-2:0] internal_addr = addr[ADDR_WIDTH-1:2];   // -> ignore bit 0
  reg [DATA_WIDTH-1:0] rom [0:(2**(ADDR_WIDTH-2))-1];
  
  initial begin
    $readmemb("rom.mem", rom);
  end
  
  always @(posedge clk) data_out <= rom[internal_addr];
  
endmodule

module ram32 #(parameter ADDR_WIDTH = 16, DATA_WIDTH = 32)

(
    input wire clk,
    input wire [ADDR_WIDTH-1:0] addr,
    output reg [DATA_WIDTH-1:0] data_out,
    input wire read_enable,
    input wire [DATA_WIDTH-1:0] data_in,
    input wire write_enable
    );
  
  reg [DATA_WIDTH-1:0] ram [0:(2**(ADDR_WIDTH-2))-1];           // 32bit words with byte addressing
  wire [ADDR_WIDTH-2:0] internal_addr = addr[ADDR_WIDTH-1:2];   // -> ignore bit 1-0
    
  always @(posedge clk)
  begin
        if(read_enable)
            data_out <= ram[internal_addr]; 
        if(write_enable)
            ram[internal_addr] <= data_in;
  end
endmodule

module mem #(parameter ADDR_WIDTH = 32,
                parameter DATA_WIDTH = 32)
(
    input wire clk, rst_n,
    input wire [ADDR_WIDTH-1:0] addr,
    output wire [DATA_WIDTH-1:0] data_out,
    input wire read_enable,
    input wire [DATA_WIDTH-1:0] data_in,
    input wire write_enable,
    output wire io_enable,
    input wire [DATA_WIDTH-1:0] io_rd_data,
    output wire mem_wait,

    output wire [ADDR_WIDTH-1:0] dram_addr,
    input wire [DATA_WIDTH-1:0]  dram_read_data,
    output wire [DATA_WIDTH-1:0] dram_write_data,
    output wire                  dram_read_enable,
    output wire                  dram_write_enable,
    input wire                   dram_wait
    );
  
  wire [DATA_WIDTH-1:0] ram_out, rom_out, dram_out;

  // address map:
  // ROM $0000 - $07FF 2K
  // IO  $0800 - $0FFF 2K
  // RAM1 $1000 - $FFFF 60K
  // RAM2 $10000 - $FFFFFFFF ~4GB

  wire ram_cs = addr[ADDR_WIDTH-1:12] != { {(ADDR_WIDTH-12){1'b0}}};
  wire ram1_cs = ram_cs && (addr[ADDR_WIDTH-1:16] == { {(ADDR_WIDTH-16){1'b0}}});
  wire ram2_cs = ram_cs && !ram1_cs;
  wire rom_cs = !ram_cs && addr[11] == 1'b0;
  wire io_cs = !ram_cs && addr[11]  == 1'b1;

  assign io_enable = io_cs;

  wire ram_read  = ram1_cs && read_enable;
  wire ram_write = ram1_cs && write_enable;
  wire rom_read  = rom_cs && read_enable;

  reg [DATA_WIDTH-1:0] data_buf;
  
  localparam SEL_RAM1 = 0;
  localparam SEL_RAM2 = 1;
  localparam SEL_ROM = 2;
  localparam SEL_IO  = 3;
  localparam SEL_ERR = 4;

  reg [1:0] out_sel;

  // test
  reg [1:0] wait_state;

  ram32 #(.ADDR_WIDTH(16)) ram0  // 64KB RAM
  (
    .clk(clk),
    .addr(addr[15:0]),
    .data_out(ram_out),
    .read_enable(ram_read),
    .data_in(data_in),
    .write_enable(ram_write)
  );
  
  rom32 #(.ADDR_WIDTH(11)) rom0  // 2KB ROM
  (
    .clk(clk),
    .addr(addr[10:0]),
    .data_out(rom_out),
    .read_enable(rom_read)
  );

    assign dram_out = dram_read_data;
    assign dram_addr = addr;
    assign dram_write_data = data_in;
    assign dram_read_enable = ram2_cs & read_enable;
    assign dram_write_enable = ram2_cs & write_enable;

    assign data_out =   (out_sel == SEL_RAM1 ) ? ram_out :
                        (out_sel == SEL_RAM2 ) ? dram_out :
                        (out_sel == SEL_ROM ) ? rom_out :
                        (out_sel == SEL_IO )  ? io_rd_data :
                        data_buf;

    assign mem_wait = ram2_cs && dram_wait;

    always @(posedge clk)
    begin
        data_buf <= data_out;
        if(read_enable) out_sel <=
                            ram1_cs ? SEL_RAM1 :
                            ram2_cs ? SEL_RAM2:
                            rom_cs ? SEL_ROM :
                            io_cs  ? SEL_IO :
                            SEL_ERR;
    end
endmodule
