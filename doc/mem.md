# Memory Layout

The Tridora system uses the following memory layout:

|Address (hex) |Address (decimal)|Description|
|-------|-----------|------------------------|
|$00000000| 0 | ROM     |
|$00000800| 2048 | I/O area|
|$00001000| 4096 | RAM (SRAM)|
|$00010000| 65536 | RAM (DRAM)|

## Accessing Words and Bytes
Memory is word-oriented, so there is no access to individual bytes. Memory transfers always use 32 bits.
Word addresses in RAM and ROM use an increment of 4, so the first memory word is at address 0, the second is at address 4 etc.

This way, you can express a pointer to a specific byte within a word.
The lower two bits of the address are ignored when accessing RAM or ROM. So if you use 1 as a memory address, you still get the memory word at address 0.
The lower two bits of the address can be viewed as a byte address (0-3) within the word.

The _BSEL_ and _BPLC_ instructions are designed to assist with accessing bytes within a word.

The byte ordering is big-endian.

## Accessing the I/O Area
The I/O area organizes memory slightly different. Here, pointing out individual bytes is not very useful, so the I/O controllers use register addresses with increments of one. In practice, there is only the VGA framebuffer controller which uses multiple registers.

The individual I/O controllers each have a memory area of 128 bytes, so there is a maximum number of 16 I/O controllers.

Currently, only I/O slots 0-3 are being used.

|I/O slot| Address | Controller |
|--------|---------|------------|
| 0      | $800    | UART       |
| 1      | $880    | SPI-SD     |
| 2      | $900    | VGA        |
| 3      | $980    | IRQC       |
