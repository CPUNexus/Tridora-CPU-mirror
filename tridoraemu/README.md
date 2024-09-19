# Tridora Emulator
- an emulator for the Tridora CPU / System
- emulates the CPU, UART, SD-Card controller, VGA controller
- supports reading the tick counter from the interrupt controller, but does not support any interrupts
- written in Golang

## Getting started
From the command line, run the *tridoraemu* or *tridoraemu.exe* program inside the *tridoraemu* directory (see below for details).

A precompiled binary for Windows is provided. 

To build the program yourself, you need to have the Go language installed on your system. Building has been tested on Windows and Linux.

## Building
Run the following commands inside the *tridoraemu* directory:

    go get
    go build

On the first run, this may take a while as the go build system fetches some external libraries and compiles them.

## Running the emulator
Start the *tridoraemu* binary in the same directory as the SD-Card image file (*sdcard.img*) and the ROM file (*rommon.prog*). It needs to be started on the command line as it uses the terminal window for the serial console. On startup, the emulator opens the VGA framebuffer window which is only used for graphics output.


The Tridora software (esp. the editor) requires a decent vt100-compatible (plus colors) terminal emulator. It has been successfully tested with (new) Windows Terminal, Alacritty, WezTerm and xterm.

The color scheme in the editor is meant for a dark terminal background.

The runtime system expects the Backspace key to send the DEL character (ASCII 127).

## Stopping the emulator
To stop the emulator, close the VGA framebuffer window.
The emulator will also stop if it encounters an infinite loop (a BRANCH @+0 instruction).

## Things to try out
On the ROM monitor prompt, press *B* to boot from the SD-card image. This should boot into the shell, which will first require you to enter the current date and time.

In the shell, try the *L* command to list directories and the *V* command to change volumes. The *Examples* volume contains some example programs in source form.

The programs *lines*, *conway* and *mandelbrot*, among others, show some (hopefully) interesting VGA graphics. The *viewpict* program can show image files (*.pict files) which contain 640x400x4 bitmaps. A few sample image files are provided.

To compile a program, set the file name (e.g. *lines.pas*) with the *W* command in the shell. Then, use *B* and *R* to build and run the program.

To edit the source file, have the name set with *W* and then use the *E* shell command. Inside the editor, press F1 for the help screen (and RETURN to leave the help screen). Control-X exits the editor, abandoning any changes.

The volume *Testvolume 1* (note the space) contains a precompiled game called *chase*. This is a game that was written for UCSD Pascal around 1980, and compiles with a few lines of changes with the Tridora Pascal compiler. The source code is also provided on that volume.

You can run the program with the *O* command in the shell (just press Return for the program arguments), or you can set the workfile name with *W* and then use the *R* command.

The *K* command in the shell is used to reclaim the space occupied by deleted or overwritten files.

A running program can be terminated by pressing Control-C, but only if the program is expecting keyboard input at that time.
