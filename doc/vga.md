# VGA Controller

Registers
|Name|Address|Description|
|----|-------|-----------|
|_FB_RA_ | $900 | Read Address |
|_FB_WA_ | $901 | Write Address |
| _FB_IO_ | $902 | I/O Register |
| _FB_PS_ | $903 | Palette Select |
| _FB_PD_ | $904 | Palette Data |
| _FB_CTL_ | $905 | Control Register |



## Pixel Data
Pixel data is organized in 32-bit-words. With four bits per pixel, one word
contains eight pixels.

|_bit_  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|p0 | p0  | p0  | p0  | p1 | p1 | p1 | p1 | p2 | p2 | p2 | p2 | p3 | p3 | p3 | p3 |

|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|p4 | p4  | p4  | p4  | p5 | p5 | p5 | p5 | p6 | 62 | p6 | p6 | p7 | p7 | p7 | p7 |

|Bitfields|Description|
|---------|-----------|
| _p0_      | 4 bits color value (leftmost pixel) |
| _p1_      | 4 bits color value  |
| _p2_      | 4 bits color value  |
| _p3_      | 4 bits color value  |
| _p4_      | 4 bits color value  |
| _p5_      | 4 bits color value  |
| _p6_      | 4 bits color value  |
| _p7_      | 4 bits color value  (rightmost pixel) |

Video memory uses a linear layout, with words using an address increment of one.
The first word (horizontal pixel coordinates 0-3) is at address 0, the second (coordinates 4-7) at address 1 etc.
The first line starts at address 0, the second at address 80 etc.

To access video memory, the corresponding video memory address must be written to a latch register, then pixel data can be read or written by the I/O register. Reading and writing uses separate latch registers (the "Read Adress" and "Write Address" registers. To read the same word and write it back, both addresses need to be set.
Both registers have an auto-increment function. After reading the I/O register, the FB_RA register is ingremented by one. After writing to the I/O register, the FB_WA register is incremented by one.

## Palette Data
The VGA controller uses a 16 color palette. The palette can be changed with the FB_PS and FB_PD registers. Writing to the FB_PS register selects a palette slot. Valid values are 0-15. After a palette slot is selected, color data can be read from and written to the FB_PD register. Color data is organized as follows:

|_bit_  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |

|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |- |- |- |r |r |r |r |g |g |g |g |b |b |b |b |

| _Bitfields_| Description |
|------------|--------------|
| _r_          | 4 bits red intensity |
| _g_          | 4 bits green intensity |
| _b_          | 4 bits blue intensity |

The FB_PS and PB_FD registers cannot be read.

## Control Register
The control register contains status information. It can only be read.

|_bit_  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|m |m |m |m |- |- |- |- |- |- |- |- |- |- |- |- |

|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |vb |


| _Bitfields_| Description |
|------------|--------------|
| _m_          | 4 bits mode indicator |
| _vb_         | vertical blank |

The _m_ field indicates the current graphics mode. At the time of writing, it is
always 1 which denotes a 640x400x4 mode.
The _vb_ bit is 1 when the video signal generator is in its vertical blank phase.
