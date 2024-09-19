# SPI SD-Card Controller
The SPI-SD-Card controller uses a single register at address $880.

## Reading the register
|_bit_  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |- |- |- |- |- |- |- |- |-|- |- |- |- |- |- |

|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |cd |cc |cb |tr |te |ra |ro |d |d |d |d |d |d |d |d |


|Bitfields|Description|
|---------|-----------|
| _cd_     | card detect |
| _cc_    | card changed |
| _cb_    | card busy |
| _tr_    | transmitter ready |
| _te_    | transmitter fifo empty |
| _ra_    | received byte available |
| _ro_    | receiver overrun |
| _d_     | received byte data |

Reading the register does not advance to the next byte in the read fifo. This is done by using the DR bit on a register write (see below).

## Writing the register

|_bit_  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |- |- |- |- |- |- |- |- |-|- |- |- |- |- |- |

|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |CW |CF |Cx |Cc |Cd |DR |DW |D |D |D |D |D |D |D |D |


|Bitfields|Description|
|---------|-----------|
| _CW_    | control write |
| _CF_    | enable receive filter |
| _Cx_    | enable transceiver |
| _Cc_    | force spi clock on |
| _Cd_    | write clock divider |
| _DR_    | read acknowledge |
| _DW_    | data write |
| _D_     | byte data |

* CF, Cx and Cc flags are used together with CW
* Cd together with d sets the clock divider
* DW together with d writes a data byte
* if the receive filter is set, all received bytes are ignored until a byte is received that is not $FF
* receiving a byte that is not $FF disables the receive filter
* Cc is used to enable the clock without sending/receiving anything - used for card initialization

Example transaction:

1. read register, loop until _te_ is set
1. write command bytes to register (_DW_ | data)
1. write _Cx_|_CF_ to register
1. read register, loop until _ra_ is set
1. process data byte
1. write _DR_ to register
1. repeat last three steps until complete response has been read
1. wait a bit/send a few more $FF bytes
1. disable transceiver, write _CW_ to register (Cx = 0)

