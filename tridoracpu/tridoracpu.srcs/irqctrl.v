`timescale 1ns / 1ps

module irqctrl #(IRQ_LINES = 2, IRQ_DELAY_WIDTH = 4) (
        input wire clk,
        input wire [IRQ_LINES-1:0] irq_in,
        input wire cs,
        input wire wr_en,
        input wire irq_wr_seten,
        output wire [IRQ_LINES-1:0] rd_data,
        output wire irq_out
    );

    reg [IRQ_LINES-1:0] irq_status;         // a 1 bit here means we have seen an interrupt on that line
    reg [IRQ_LINES-1:0] irq_mask;           // a bit in irq_status is only set if the corresponding bit in irq_mask is not set
                                            // irq_mask is set from irq_status when an interrupt occurs (ie irq_out is set)

    reg irq_enabled;                        // globally enable/disable irq_out
    reg [IRQ_DELAY_WIDTH-1:0] irq_delay;    // counter to delay irq_out for a few cycles
    reg irq_delaying;                       // delay is active

    wire irq_pending = (irq_status != 0);

    assign rd_data = irq_mask;
    assign irq_out = irq_enabled && irq_pending && !irq_delaying;

    // irq_status and irq_pending flags
    always @(posedge clk)
    begin
        if(irq_out)                            // when an interrupt is being signaled to the cpu,
            irq_status <= 0;                   // clear irq status, status will be copied to irq_mask (see below)
        else
        if(irq_in != 0)
            irq_status <= irq_status | (irq_in & ~irq_mask);  // add active irq to irq_status
    end

    // irq mask
    always @(posedge clk)
    begin
        if (cs && wr_en && irq_wr_seten)    // when enabling interrupts, clear mask
            irq_mask <= 0;
        else
        if(irq_out)                         // when signalling an interrupt, set mask from status
            irq_mask <= irq_status;
    end

    // manage irq_enabled and irq_delay/irq_delaying
    always @(posedge clk)
    begin
        if(cs && wr_en)         // when writing to control register
        begin
            if(irq_wr_seten)    // if wr_seten flag is set, enable interrupts and start delay    
            begin
                irq_enabled <= 1;
                irq_delaying <= 1;
                irq_delay <= 1;
            end
            else
                irq_enabled <= 0; // else disable interrupts
        end
        else if(irq_out) irq_enabled <= 0;  // after sending interrupt to cpu, disable further interrupts

        if(irq_delaying)	// the delay gives the CPU a chance to return from an interrupt handler
        begin			// if an interrupt is triggered again right after re-enabling interrupts
            if(irq_delay==0)
            begin
                irq_delay <= 1;
                irq_delaying <= 0;
            end
            else
                irq_delay <= irq_delay + 1;
        end
    end
 endmodule
