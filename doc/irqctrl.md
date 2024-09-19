# Interrupt Controller
The interrupt controller uses a single register at address: $980

## Reading the status register

|_bit_  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|t |t |t |t |t |t |t |t |t |t|t |t |t |t |t |t |

|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|t |t |t |t |t |t |t |t |- |- |- |- |- |- |p1 |p0 |


|Bitfields|Description|
|---------|-----------|
| _t_     | unsigned 24 bit counter of timer ticks since reset
| _p1_    | IRQ 1 (timer tick) interrupt pending if 1
| _p0_    | IRQ 0 (UART) interrupt pending if 1



## Writing the status register

|_bit_  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |

|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |- |- |- |- |- |- |- |i |- |- |- |- |- |- |- |


|Bitfields|Description|
|---------|-----------|
| _i_     | 1 = interrupts enabled, 0 = interrupts disabled


## Notes
Interrupt processing is disabled on reset and needs to be enabled by writing a value with
bit 7 set to the status register (i.e. 127).

An interrupt is only signaled once to the CPU whenever one of the IRQ signals becomes active.

Reading the status register will reflect all pending interrupts since enabling interrupt processing.

Interrupt processing needs to be re-enabled after an interrupt occurs by setting bit 7 in the status register again. This will also clear all pending interrupts.

