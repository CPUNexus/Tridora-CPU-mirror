`timescale 1ns / 1ps

module dram_bridge #(ADDR_WIDTH = 32, WIDTH = 32)
(
    // local bus
    input wire [ADDR_WIDTH-1:0] mem_addr,
    output wire [WIDTH-1:0]     mem_read_data,
    input wire [WIDTH-1:0]      mem_write_data,
    input wire                  mem_read_enable,
    input wire                  mem_write_enable,
    output wire                 mem_wait,

    input wire                  rst_n,
    input wire                  dram_front_clk,
    input wire                  dram_refclk,

    // DDR3 SDRAM
    inout wire [15:0]   ddr3_dq,
    inout wire [1:0]    ddr3_dqs_n,
    inout wire [1:0]    ddr3_dqs_p,

    output wire [13:0]  ddr3_addr,
    output wire [2:0]   ddr3_ba,
    output wire         ddr3_ras_n,
    output wire         ddr3_cas_n,
    output wire         ddr3_we_n,
    output wire         ddr3_reset_n,
    output wire [0:0]   ddr3_ck_p,
    output wire [0:0]   ddr3_ck_n,
    output wire [0:0]   ddr3_cke,
    output wire [0:0]   ddr3_cs_n,
    output wire [1:0]   ddr3_dm,
    output wire [0:0]   ddr3_odt
);

    localparam DRAM_ADDR_WIDTH = 28, DRAM_DATA_WIDTH = 128, DRAM_MASK_WIDTH = 16;
    wire [DRAM_ADDR_WIDTH-1:0]            app_addr;
    wire [2:0]                            app_cmd;
    wire                                  app_en;
    wire                                  app_rdy;
    wire [DRAM_DATA_WIDTH-1:0]            app_rd_data;
    wire                                  app_rd_data_end;
    wire                                  app_rd_data_valid;
    wire [DRAM_DATA_WIDTH-1:0]            app_wdf_data;
    wire                                  app_wdf_end;
    wire [DRAM_MASK_WIDTH-1:0]            app_wdf_mask;
    wire                                  app_wdf_rdy;
    wire                                  app_sr_active;
    wire                                  app_ref_ack;
    wire                                  app_zq_ack;
    wire                                  app_wdf_wren;
    wire [11:0]                           device_temp;
    wire                                  ui_clk, ui_rst_sync;
    wire                                  init_calib_complete;

    localparam CMD_READ = 3'b1;
    localparam CMD_WRITE = 3'b0;

    mig_dram_0 dram0(
    // Inouts
        .ddr3_dq(ddr3_dq),
        .ddr3_dqs_n(ddr3_dqs_n),
        .ddr3_dqs_p(ddr3_dqs_p),
  // Outputs
        .ddr3_addr(ddr3_addr),
        .ddr3_ba(ddr3_ba),
        .ddr3_ras_n(ddr3_ras_n),
        .ddr3_cas_n(ddr3_cas_n),
        .ddr3_we_n(ddr3_we_n),
        .ddr3_reset_n(ddr3_reset_n),
        .ddr3_ck_p(ddr3_ck_p),
        .ddr3_ck_n(ddr3_ck_n),
        .ddr3_cke(ddr3_cke),
        .ddr3_cs_n(ddr3_cs_n),
        .ddr3_dm(ddr3_dm),
        .ddr3_odt(ddr3_odt),
// Application interface ports
       .app_addr                       (app_addr),
       .app_cmd                        (app_cmd),
       .app_en                         (app_en),
       .app_wdf_data                   (app_wdf_data),
       .app_wdf_mask                   (app_wdf_mask),
       .app_wdf_end                    (app_wdf_end),
       .app_wdf_wren                   (app_wdf_wren),
       .app_rd_data                    (app_rd_data),
       .app_rd_data_end                (app_rd_data_end),
       .app_rd_data_valid              (app_rd_data_valid),
       .app_rdy                        (app_rdy),
       .app_wdf_rdy                    (app_wdf_rdy),
       .app_sr_req                     (1'b0),
       .app_ref_req                    (1'b0),
       .app_zq_req                     (1'b0),
       .app_sr_active                  (app_sr_active),
       .app_ref_ack                    (app_ref_ack),
       .app_zq_ack                     (app_zq_ack),
       .ui_clk                         (ui_clk),
       .ui_clk_sync_rst                (ui_rst_sync),

// System Clock Ports
       .sys_clk_i                      (dram_front_clk),
// Reference Clock Ports
       .clk_ref_i                      (dram_refclk),
       .device_temp                    (device_temp),
       .init_calib_complete            (init_calib_complete),
       .sys_rst                        (rst_n)
    );

//    reg [DRAM_DATA_WIDTH-1:0] 	read_cache;
//    reg [ADDR_WIDTH-1:0]      	cached_addr;
//    wire 		      	cache_hit = cached_addr == mem_addr;
//    wire [DRAM_DATA_WIDTH-1:0]  read_data_wrapper = cache_hit ? read_cache : app_rd_data;

    reg [WIDTH-1:0] read_buf;
    reg read_inprogress = 0;

    assign app_rd_data_end = 1'b1;
    //assign app_wdf_mask = 16'b1111111111111100;

    // addresses on the memory interface are aligned to 16 bytes
    // and 28 bits wide (=256MB)
    assign app_addr = { mem_addr[DRAM_ADDR_WIDTH:4], 4'b0000 };
    //assign app_addr = { 28'b0 };
 
    // select a word from the 128 bits transferred by the dram controller
    // according to the lower bits of the address (ignoring bits 1:0)
    wire [WIDTH-1:0] read_word;
    wire [1:0] word_sel = mem_addr[3:2];

    assign read_word =  word_sel == 3'b11 ? app_rd_data[31:0]  :
                        word_sel == 3'b10 ? app_rd_data[63:32] :
                        word_sel == 3'b01 ? app_rd_data[95:64] :
                                            app_rd_data[127:96];

    assign mem_read_data = app_rd_data_valid ? read_word : read_buf;

    // set the write mask according to the lower bits of the address
    // (ignoring bit 0)
    assign app_wdf_mask = word_sel == 3'b11 ? 16'b1111111111110000 :
                          word_sel == 3'b10 ? 16'b1111111100001111 :
                          word_sel == 3'b01 ? 16'b1111000011111111 :
                                              16'b0000111111111111 ;

    wire write_ready = mem_write_enable & app_wdf_rdy & app_rdy;
    assign app_wdf_wren = mem_write_enable & write_ready;
    assign app_wdf_end =  mem_write_enable & write_ready;
    assign app_wdf_data = { {4{mem_write_data}} };

    assign mem_wait =   (mem_read_enable & ~read_inprogress) |
                        (mem_write_enable & (~app_wdf_rdy | ~app_rdy)) |
                        (read_inprogress & ~app_rd_data_valid);

    assign app_en = (mem_read_enable & ~read_inprogress) |
            (mem_write_enable & write_ready);
    assign app_cmd = mem_read_enable ? CMD_READ : CMD_WRITE;

    always @(posedge dram_front_clk)
    begin
        if(mem_read_enable & ~read_inprogress & app_rdy)
            read_inprogress <= 1;
        if(read_inprogress & app_rd_data_valid)
            read_inprogress <= 0;
        if(mem_read_enable & app_rd_data_valid)
            read_buf <= mem_read_data;
    end
endmodule
