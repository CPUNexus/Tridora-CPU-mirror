# Tridora System
Tridora is a homebrew CPU written in Verilog and a matching software environment,
including a Pascal compiler and assembler.
Everything was created from the ground up (except soldering stuff).
Everything is as simple as possible while still being reasonably useful.
Everything is open source, so you can read, understand and modify the whole system, hardware and software.

## Overview
- homebrew CPU written in Verilog implemented on an FPGA
- 32-bit word-oriented stack machine architecture
- running at 83 MHz on an Arty-A7 board with four clocks per instruction
- has its own instruction set architecture, compatible with nothing
- additional IO controllers on FPGA: UART (serial console), SD-Card, VGA
- Pascal compiler written from zero
- CPU and compiler were designed together
- minimal operating system
- editor, compiler, assembler run natively
- so you can develop programs directly on the machine
- small: CPU has ~500 lines of Verilog, compiler ~9000 LoC
- compiler written in Pascal and can compile itself
- cross-compiler/-assembler can be compiled with FPC
- compiler does its own Pascal dialect with some restrictions and some extensions
- emulator available

## Demos
### Videos

|Compiling and Running Hello-World|Moving Lines Demo|
|---|---|
|<video src="https://insignificance.de/tridora/tdr-hello.mp4" controls></video>|<video src="https://insignificance.de/tridora/tdr-lines.mp4" controls></video>|


### Pictures

|Mandelbrot|Image Viewer|Game of Life|
|---|---|---|
|![image](https://insignificance.de/tridora/tdr-mandelbrot.jpg)| ![image](https://insignificance.de/tridora/tdr-ara.jpg) | ![image](https://insignificance.de/tridora/tdr-conway.jpg) |

## Supported Boards
- Arty A7 (with two PMODs for microSD cards and VGA output)
- Nexys A7 (planned)

## Pascal Language
- Wirth Pascal
- no function types/parameters
- arbitrary length strings (2GB)
- safe strings (runtime information about max/current size)
- tiny sets (machine word sized), that means no SET OF CHAR
- array literals with IN-operator, which can replace most uses of SET OF CHAR
- nested procedures with some limitations
- 32 bit software floating point with low precision (5-6 digits)
- break and exit statements, no continue yet
- static variable initialization for global variables
- non-standard file i/o (because the standard sucks, obl. XKCD reference)

## Standard Library
- everything from Wirth Pascal
- some things from TP3.0
- some graphics functionality (to be expanded in the future)

## Operating System
- not a real operating system, more of a program loader
- some assembly routines for I/O resident in memory
- one program image loaded at a time at a fixed address
- most parts of the operating system are contained in the program image
- file system is very primitive: only contiguous blocks, no subdirectories
- simple shell reminiscent of TP3.0, edit, compile, run programs

## Building the FPGA bitstream
- install Vivado (known to work with 2020.1, known NOT to work with 2024.1)
- install the package for your board in Vivado (Tools -> Vivado Store -> Boards)
- start Vivado and open the project file **tridoracpu.xpr** in the **tridoracpu** directory
- run synthesis, implementation and bitstream generation (Flow -> Generate Bitstream)
- program your device (Flow -> Open Hardware Manager), the bitstream file should be in **tridoracpu/tridoracpu.runs/impl_1**
- the bitstream file for (temporarily) programming your device is named **top.bit**, the file for flashing your device is named **top.bin**

## Documentation
- [Instruction Reference](doc/tridoracpu.md)
- [Memory Layout](doc/mem.md)
- [SD-Card controller](doc/spisd.md)
- [VGA controller](doc/vga.md)

More documentation is coming as time permits.
