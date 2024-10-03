# UART
The UART a single register at address $900.
It uses a fixed serial configuration of 115200 bps, 8 data bits, 1 stop bit, no parity.

## Reading the UART register

|_bit_  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |

|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |- |- |- |- |- |A |B |d |d |d |d |d |d | d | d |

|Bitfields|Description|
|---------|-----------|
| _A_    | data available, at least one byte has been received
| _B_    | TX busy, a byte is being transmitted and no data can be written
| _d_    | 8 data bits

## Writing the UART register

|_bit_  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |

|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |- |- |- |- |T |C |- |d |d |d |d |d |d | d | d |


|Bitfields|Description|
|---------|-----------|
| _T_     | transmit enable, writes a data byte
| _C_    | clear receiver, acknowledges a received byte
| _d_    | 8 data bits


## Notes
A 16 byte FIFO is used when receiving data.

When reading data, each byte needs to be acknowledged by writing the _C_ flag to the UART register.

When the FIFO is empty, the _A_ flag in the UART register will be 0.
