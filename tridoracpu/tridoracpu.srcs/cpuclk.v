`timescale 1ns / 1ps

module cpu_clkgen(
    input wire rst,
    input wire clk100,
    output wire cpuclk,
    output wire dram_refclk,
    output wire pixclk,
    output wire locked
    );

    wire cpuclk_pre, clk_fb, refclk_pre, pixclk_pre;

    MMCME2_BASE #(
        .BANDWIDTH("OPTIMIZED"),    // Jitter programming (OPTIMIZED, HIGH, LOW)
        .CLKFBOUT_MULT_F(10.0),     // Multiply value for all CLKOUT (2.000-64.000).
        .CLKFBOUT_PHASE(0.0),       // Phase offset in degrees of CLKFB (-360.000-360.000).
        .CLKIN1_PERIOD(10.0),       // Input clock period in ns to ps resolution (i.e. 33.333 is 30 MHz).
        // CLKOUT0_DIVIDE - CLKOUT6_DIVIDE: Divide amount for each CLKOUT (1-128)
        .CLKOUT0_DIVIDE_F(12.0),    // Divide amount for CLKOUT0 (1.000-128.000).
        .CLKOUT1_DIVIDE(5),
        .CLKOUT2_DIVIDE(40),        // 40 = 25MHz pixel clock (should be 25.175MHz per spec) for 640x480
        //.CLKOUT2_DIVIDE(25),        // 25 = 40MHz pixel clock for 800x600
        //.CLKOUT2_DIVIDE(15),        // 15 = 66.66MHz pixel clock (should be 65.0Mhz per spec) for 1024x768
        .CLKOUT3_DIVIDE(1),
        .CLKOUT4_DIVIDE(1),
        .CLKOUT5_DIVIDE(1),
        .CLKOUT6_DIVIDE(1),
        // CLKOUT0_DUTY_CYCLE - CLKOUT6_DUTY_CYCLE: Duty cycle for each CLKOUT (0.01-0.99).
        .CLKOUT0_DUTY_CYCLE(0.5),
        .CLKOUT1_DUTY_CYCLE(0.5),
        .CLKOUT2_DUTY_CYCLE(0.5),
        .CLKOUT3_DUTY_CYCLE(0.5),
        .CLKOUT4_DUTY_CYCLE(0.5),
        .CLKOUT5_DUTY_CYCLE(0.5),
        .CLKOUT6_DUTY_CYCLE(0.5),
        // CLKOUT0_PHASE - CLKOUT6_PHASE: Phase offset for each CLKOUT (-360.000-360.000).
        .CLKOUT0_PHASE(0.0),
        .CLKOUT1_PHASE(0.0),
        .CLKOUT2_PHASE(0.0),
        .CLKOUT3_PHASE(0.0),
        .CLKOUT4_PHASE(0.0),
        .CLKOUT5_PHASE(0.0),
        .CLKOUT6_PHASE(0.0),
        .CLKOUT4_CASCADE("FALSE"),      // Cascade CLKOUT4 counter with CLKOUT6 (FALSE, TRUE)
        .DIVCLK_DIVIDE(1),     // Master division value (1-106)
        .REF_JITTER1(0.010),            // Reference input jitter in UI (0.000-0.999).
        .STARTUP_WAIT("FALSE")          // Delays DONE until MMCM is locked (FALSE, TRUE)
    )

    MMCME2_BASE_inst (
        /* verilator lint_off PINCONNECTEMPTY */
        // Clock Outputs: 1-bit (each) output: User configurable clock outputs
        .CLKOUT0(cpuclk_pre),           // 1-bit output: CLKOUT0
        .CLKOUT0B(),                    // 1-bit output: Inverted CLKOUT0
        .CLKOUT1(refclk_pre),           // 1-bit output: CLKOUT1
        .CLKOUT1B(),                    // 1-bit output: Inverted CLKOUT1
        .CLKOUT2(pixclk_pre),           // 1-bit output: CLKOUT2
        .CLKOUT2B(),                    // 1-bit output: Inverted CLKOUT2
        .CLKOUT3(),                     // 1-bit output: CLKOUT3
        .CLKOUT3B(),                    // 1-bit output: Inverted CLKOUT3
        .CLKOUT4(),                     // 1-bit output: CLKOUT4
        .CLKOUT5(),                     // 1-bit output: CLKOUT5
        .CLKOUT6(),                     // 1-bit output: CLKOUT6
        // Feedback Clocks: 1-bit (each) output: Clock feedback ports
        .CLKFBOUT(clk_fb),              // 1-bit output: Feedback clock
        .CLKFBOUTB(),                   // 1-bit output: Inverted CLKFBOUT
        // Status Ports: 1-bit (each) output: MMCM status ports
        .LOCKED(locked),              // 1-bit output: LOCK
        // Clock Inputs: 1-bit (each) input: Clock input
        .CLKIN1(clk100),                 // 1-bit input: Clock
        // Control Ports: 1-bit (each) input: MMCM control ports
        .PWRDWN(),                      // 1-bit input: Power-down
        /* verilator lint_on PINCONNECTEMPTY */
        .RST(rst),                    // 1-bit input: Reset
        // Feedback Clocks: 1-bit (each) input: Clock feedback ports
        .CLKFBIN(clk_fb)                // 1-bit input: Feedback clock
    );

    BUFG bufg_cpuclk(.I(cpuclk_pre), .O(cpuclk));
    BUFG bufg_refclk(.I(refclk_pre), .O(dram_refclk));
    BUFG bufg_pixclk(.I(pixclk_pre), .O(pixclk));
endmodule
