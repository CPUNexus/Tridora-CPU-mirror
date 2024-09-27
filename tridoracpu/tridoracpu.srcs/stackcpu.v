`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////

module stackcpu #(parameter ADDR_WIDTH = 32, WIDTH = 32,
    WORDSIZE = 4, WORDSIZE_SHIFT = 2) (
    input wire clk,
    input wire rst,

    input wire irq,

    output reg [ADDR_WIDTH-1:0] addr,
    input wire [WIDTH-1:0] data_in,
    output wire read_enable,
    output wire [WIDTH-1:0] data_out,
    output wire write_enable,
    input wire mem_wait,
    
    output wire led1,
    output wire led2,
    output wire led3,
    
    output wire [WIDTH-1:0] debug_out1,
    output wire [WIDTH-1:0] debug_out2,
    output wire [WIDTH-1:0] debug_out3,
    output wire [WIDTH-1:0] debug_out4,
    output wire [WIDTH-1:0] debug_out5,
    output wire [WIDTH-1:0] debug_out6
    );
    
    localparam EVAL_STACK_INDEX_WIDTH = 6;

    wire reset = !rst;

    (* KEEP *) reg [1:0] seq_state;
    localparam FETCH = 2'b00; localparam DECODE = 2'b01; localparam EXEC = 2'b10; localparam MEM = 2'b11;

    (* KEEP*) reg [WIDTH-1:0] X, nX;
    wire [WIDTH-1:0] Y;
    (* KEEP *) reg [WIDTH-1:0] PC, nPC;
    reg [WIDTH-1:0] RP, nRP;
    reg [WIDTH-1:0] FP, BP;
    reg [WIDTH-1:0] IV,IR;

    wire [WIDTH-1:0] pc_next_ins = PC + 2;

    reg [EVAL_STACK_INDEX_WIDTH-1:0] ESP, nESP;
    reg stack_write;

    reg irq_pending;
 
    // eval stack
    stack #(.ADDR_WIDTH(EVAL_STACK_INDEX_WIDTH), .DATA_WIDTH(WIDTH)) estack (
        .clk(clk),
        .rd_addr(ESP),
        .wr_addr(nESP),
        .wr_enable(stack_write),
        .rd_data(Y),
        .wr_data(X)
    );

    reg  [15:0] ins;

    wire [WIDTH-1:0] operand;
    
    // decoded instructions
    wire ins_loadrel;
    wire ins_load;
    wire ins_loadc;
    wire ins_store;
    wire ins_aluop;
    wire ins_ext;
    wire ins_xfer;
    wire ins_branch;
    wire ins_cbranch;
    
    // decoded extended instructions
    wire ins_mem, ins_loadi, ins_storei;
    wire ins_fpadj;
    wire ins_reg, ins_loadreg, ins_storereg;
    wire ins_reg_fp, ins_reg_bp, ins_reg_rp;
    wire ins_reg_iv, ins_reg_ir;
    wire ins_reg_esp;
    wire loadstore_base;
    wire cbranch_n;
    
    wire xfer_x2p, xfer_r2p, xfer_p2r;
    wire [1:0] xfer_rs;
    
    wire [3:0] aluop;
    wire [1:0] aluop_sd;
    wire aluop_x2y, aluop_ext;
    
    wire cmp_i, cmp_e, cmp_l;
    
    wire mem_read;
    wire mem_write;
    
    wire x_is_zero;
    // wire [WIDTH-1:0] y_plus_operand = Y + operand;

    wire x_equals_y = X == Y;
    wire y_lessthan_x  = $signed(Y) < $signed(X);
    wire yx_unsigned_less = Y < X;

    reg [WIDTH-1:0] mem_write_data;

    wire mem_read_enable, mem_write_enable;
    
    assign read_enable = mem_read_enable;
    assign data_out = mem_write_data;
    assign write_enable = mem_write_enable;

    // debug output ------------------------------------------------------------------------------------
    assign led1 = reset;
    assign led2 = ins_loadc;
    assign led3 = ins_branch;
//    assign debug_out1 = { mem_read_enable, mem_write_enable, x_is_zero,
//                    ins_branch, ins_aluop, y_lessthan_x, x_equals_y, {7{1'b0}}, seq_state};
//    assign debug_out2 = data_in;
//    assign debug_out3 = nX;
//    assign debug_out4 = nPC;
//    assign debug_out5 = ins;
//    assign debug_out6 = IV;
    //--------------------------------------------------------------------------------------------------

    // instruction decoding
    assign ins_branch  = (ins[15:13] == 3'b000);
    assign ins_aluop   = (ins[15:13] == 3'b001);
    assign ins_store   = (ins[15:13] == 3'b010);
    assign ins_xfer    = (ins[15:13] == 3'b011);
    assign ins_load    = (ins[15:13] == 3'b100);
    assign ins_cbranch = (ins[15:13] == 3'b101);
    assign ins_loadc   = (ins[15:13] == 3'b110);
    assign ins_ext     = (ins[15:13] == 3'b111);
    
    // sub-decode LOAD/STORE
    assign loadstore_base = ins[0];
    
    // sub-decode CBRANCH
    assign cbranch_n = ins[0];
    
    // sub-decode XFER
    assign xfer_x2p = ins[0];
    assign xfer_r2p = ins[7];
    assign xfer_p2r = ins[6];
    assign xfer_rs  = ins[9:8];
    
    // sub-decode OP
    assign aluop = ins[12:9];
    assign aluop_x2y = ins[6];
    assign aluop_sd = ins[5:4];
    assign aluop_ext = ins[7];
    
    // sub-decode OP.CMP
    assign cmp_i = ins[2];
    assign cmp_e = ins[1];
    assign cmp_l = ins[0];
    
    assign x_is_zero = X == {WIDTH{1'b0}};
    
    // decode extended instructions
    assign ins_reg    = (ins_ext && ins[12:10] == 3'b000);
    assign ins_mem    = (ins_ext && ins[12:10] == 3'b001);
    assign ins_loadi  = (ins_mem && ins[9] == 1'b0);
    assign ins_storei = (ins_mem && ins[9] == 1'b1);
    assign ins_fpadj  = (ins_ext && ins[12:10] == 3'b011);    
    assign ins_loadrel= (ins_ext && ins[12:10] == 3'b101);

    // sub-decode LOADREG/STOREREG
    assign ins_loadreg  = (ins_reg && ins[9] == 1'b0);
    assign ins_storereg = (ins_reg && ins[9] == 1'b1);
    assign ins_reg_fp   = (ins_reg && ins[3:0] == 4'b0000);
    assign ins_reg_bp   = (ins_reg && ins[3:0] == 4'b0001);
    assign ins_reg_rp   = (ins_reg && ins[3:0] == 4'b0010);
    assign ins_reg_iv   = (ins_reg && ins[3:0] == 4'b0011);
    assign ins_reg_ir   = (ins_reg && ins[3:0] == 4'b0100);
    assign ins_reg_esp  = (ins_reg && ins[3:0] == 4'b0101);
    
    assign mem_read = ins_loadi || ins_load || ins_loadrel || (ins_xfer && xfer_r2p);
    assign mem_write = ins_storei || ins_store || (ins_xfer && xfer_p2r);
    
    assign mem_read_enable  = (seq_state == FETCH) || (seq_state == EXEC && mem_read);
    assign mem_write_enable = (seq_state == MEM && mem_write);
    
    initial
    begin
        PC <= 0; nPC <= 0; seq_state <= MEM;
        ESP <= -1; nESP <= -1;
        addr <= 0;
        FP <= 0; BP <= 0; RP <= 0; nRP <= 0;
        IV <= 0; IR <= 0;
        irq_pending <= 0;
    end

    // instruction sequencer
    always @(posedge clk)
    begin
        if(reset)
            seq_state <= MEM;
        else if(mem_wait == 1'b0)
        case(seq_state)
            FETCH: seq_state <= DECODE;
            DECODE: seq_state <= EXEC;
            EXEC: seq_state <= MEM;
            MEM: seq_state <= FETCH;
            default: seq_state <= FETCH;
        endcase
    end

    // operand register
    assign operand =
            (ins_load || ins_store || ins_branch || ins_cbranch) ? 
                                { {(WIDTH-13){ins[12]}}, ins[12:1], 1'b0 }
            : (ins_loadc) ?     { {(WIDTH-13){ins[12]}}, ins[12:0] } // sign extend
            : (ins_aluop || ins_mem) ? 
                                { {(WIDTH-4){1'b0}}, ins[3:0] }
            : (ins_loadrel) ?   { {(WIDTH-10){1'b0}}, ins[9:0] }
            : (ins_fpadj) ?     { {(WIDTH-10){ins[9]}}, ins[9:0] } // sign extend
            :                   { {WIDTH{1'b0}} };
    
    // program counter
    always @(posedge clk)
    begin
        if(reset) nPC <= 0;
        else
        case(seq_state)
        EXEC:
            if(ins_xfer && xfer_x2p) nPC <= X;
            else if(ins_branch || (ins_cbranch && (x_is_zero != cbranch_n))) nPC <= PC + operand;
            else nPC <= pc_next_ins;
        MEM:
            if(ins_xfer && xfer_r2p) nPC <= data_in;
            else if(irq_pending) nPC <= IV;
        endcase
    end

    // return stack pointer
    always @*
    begin
        if(seq_state == EXEC || seq_state == DECODE || seq_state == MEM)
        begin
            if (ins_xfer) nRP <= RP +
                ({ {(ADDR_WIDTH-3){xfer_rs[1]}},xfer_rs} << WORDSIZE_SHIFT);
                // sign extend xfer_rs and multiply by word size
            else if (ins_storereg && ins_reg_rp) nRP <= X;
            else nRP <= RP;
        end
        else nRP <= nRP;
    end
    
    // instruction fetch
    // depending on bit 1 of the PC, read either the upper or lower half word as an instruction
    always @* if(seq_state == DECODE) ins <= PC[1] ? data_in[15:0] : data_in[31:16];

    // RAM read/write
    always @(posedge clk)
    begin        
        if(reset)
        begin
            addr <= 0;
            mem_write_data <= 0;
        end
        else
        case(seq_state)
            DECODE:
                if(ins_load || ins_store)      // read from address in BP/FP + offset
                    addr <= operand + ( loadstore_base ? BP: FP);
                else if (ins_loadi)            // read from address in X
                    addr <= X;
                else if (ins_storei)           // write to address in Y
                    addr <= Y;
                else if (ins_loadrel)          // read from address next to current instruction
                    addr <= PC + operand;
                else if (ins_xfer && xfer_r2p) // read from return stack 
                    addr <= RP;                // use the current RP
                else if (ins_xfer && xfer_p2r) // write to return stack
                    addr <= nRP;               // use the new RP
            EXEC:
                begin
                    if (ins_store)
                        mem_write_data <= X;
                    else if (ins_storei)
                        mem_write_data <= X;
                    else if (ins_xfer && xfer_p2r)
                        mem_write_data <= pc_next_ins;
                    else
                        mem_write_data <= 0;
                end
            MEM:
                if(!mem_wait) // do not change the address if mem_wait is active
                begin
                    if(ins_xfer && xfer_r2p) addr <= data_in;  // on RET take addr for next instruction from the data we just read from mem
                    else addr <= irq_pending ? IV : nPC;       // prepare fetch cycle
                end
        endcase
    end
    
    // X/ToS-Register
    always @(posedge clk)
    begin
        if(reset) nX <= 0;
        else
        case(seq_state)
            // default: nX <= X;
            FETCH, DECODE:;
            EXEC:
                if(ins_loadc) nX <= operand;
                else if(ins_cbranch || ins_store || ins_storereg || (ins_xfer && xfer_x2p)) nX <= Y;
                else if(ins_storei) nX <= Y + operand;
                else if(ins_loadreg && ins_reg_fp) nX <= FP;
                else if(ins_loadreg && ins_reg_bp) nX <= BP;
                else if(ins_loadreg && ins_reg_rp) nX <= RP;
                else if(ins_loadreg && ins_reg_iv) nX <= IV;
                else if(ins_loadreg && ins_reg_ir) nX <= IR;
                else if(ins_loadreg && ins_reg_esp) nX <= ESP;
                else if(ins_aluop)
                begin
                    case(aluop)
                        4'b0000: nX = X + Y;       // ADD
                        4'b0001: nX = Y - X;       // SUB
                        4'b0010: nX = ~X;          // NOT
                        4'b0011: nX = X & Y;       // AND
                        4'b0100: nX = X | Y;       // OR
                        4'b0101: nX = X ^ Y;       // XOR
                        4'b0110: nX =              // CMP
                            cmp_i ^ ((cmp_e && x_equals_y) || (cmp_l && y_lessthan_x));
                        4'b0111: nX = Y;           // Y
                        4'b1000: nX = aluop_ext ? X >>> 1 : X >> 1;     // SHR
                        4'b1001: nX = operand[1] ? X << 2 : X << 1;     // SHL
                        4'b1010: nX = X + operand; // INC
                        4'b1011: nX = X - operand; // DEC
                        4'b1100: nX =              // CMPU
                            cmp_i ^ ((cmp_e && x_equals_y) || (cmp_l && yx_unsigned_less));
                        // 4'b1101: nX = X[7:0] << ((3 - Y[1:0]) << 3); // BPLC
                        4'b1101: nX = Y[1:0] == 0   ? { X[7:0], 24'b0 } :
                                        Y[1:0] == 1 ? { 8'b0, X[7:0], 16'b0 } :
                                        Y[1:0] == 2 ? { 16'b0, X[7:0], 8'b0 } :
                                                      { 24'b0, X[7:0]}; // BPLC
                        4'b1110: nX = { X[23:16], X[15:8], X[7:0], X[31:24] }; // BROT
                        4'b1111: nX = { 24'b0, Y[1:0] == 0 ? X[31:24] : Y[1:0] == 1 ? X[23:16] :
                                               Y[1:0] == 2 ? X[15: 8] : X[7:0] }; // BSEL
//                        4'b1110: nX = X * Y;       // MUL
//                        4'b1111: nX = X / Y;       // DIV
                        default: nX = X;
                    endcase
                end
            MEM:
                if (ins_loadi || ins_load || ins_loadrel)
                    nX = data_in;
            endcase
    end
    
    // estack movement
    wire [EVAL_STACK_INDEX_WIDTH-1:0] delta =
            ((ins_load || ins_loadc || ins_loadreg || ins_loadrel))     ? 1
        :   ((ins_aluop || ins_loadi || ins_storei || ins_xfer))        ?
				{ {(EVAL_STACK_INDEX_WIDTH-2){aluop_sd[1]}},aluop_sd} // sign extend
        :   ((ins_store || ins_cbranch || ins_xfer || ins_storereg))    ? -1
        :   0;


    always @*
    begin
        if(reset)
            nESP <= 0;
        else
        if(seq_state == EXEC)
        begin
            nESP = ESP + delta;
        end
    end

    always @(posedge clk)
    begin
        // when to write (old) X back to stack (new Y)
	    // stack write is a reg so it is 1 in the next cycle i.e. MEM state
        stack_write <= (seq_state == EXEC &&
            (ins_load || ins_loadc || ins_loadrel || ins_loadreg
            || ((ins_loadi || ins_storei || ins_aluop) && aluop_x2y)));
    end

    // FP register
    always @(posedge clk)
    begin
        if(seq_state == EXEC)
        begin
            if(ins_fpadj) FP <= FP + operand;
            else if(ins_storereg && ins_reg_fp) FP <= X;
        end
    end
    
    // BP register
    always @(posedge clk) if(seq_state == EXEC && ins_storereg && ins_reg_bp) BP <= X;

   // IV register
    always @(posedge clk)
    begin
        if(reset)
            IV <= 0;
        else if(seq_state == EXEC && ins_storereg && ins_reg_iv)
            IV <= X;
    end

    // IR register
    always @(posedge clk)
    begin
        if(seq_state == MEM && irq_pending) IR <= nPC; // use nPC as interrupt return addr
    end

    // process irq
    always @(posedge clk)
    begin
        if(seq_state == MEM && irq_pending && !(ins_xfer & xfer_r2p))   // in FETCH state, clear irq_pending.
            irq_pending <= 0;
        else
            irq_pending <= irq_pending || irq;  // else set irq_pending when irq is high
    end

    // advance CPU state
    always @ (posedge clk)
    begin
        if(reset)
            { PC, X, ESP, RP } <= { {WIDTH{1'b0}}, {WIDTH{1'b0}}, {WIDTH{1'b0}}, {WIDTH{1'b0}} };
        else if(seq_state == FETCH)
            { PC, X, ESP, RP } <= { nPC, nX, nESP, nRP};
    end
endmodule
