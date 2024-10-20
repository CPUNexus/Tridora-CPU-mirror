# Using the Shell

The shell is started on booting the Tridora-System, and also each
time a program exits.

Its main purpose is to start the editor and compiler, and finally run your
compiled program.

You can also list directory contents and do some file management like
copying and deleting files.

## The Menu
The shell displays a menu with a bit of status information and a list
of possible commands, each represented by a single letter.
Pressing the corresponding key will invoke a specific command.

Some commands will prompt for more information if needed.

The menu looks like this:

```
W)orkfile: lines.pas
V)olume:   Examples
L)ist directory  K)runch volume  O)ther program
D)elete file  RenaM)e file  coP)y file
E)dit  C)ompile  A)ssemble  B)uild  R)un
> 
```

## The Workfile
When working with the shell, for many functions you
need to set a _workfile_ which then will be passed as a parameter
to other programs. In most cases, you will set the workfile to your
source file (e.g. **hello.pas**). When appropriate, the shell will
change the file name extension given to the called program, while the
workfile name setting in the shell will remain the same.

For example, when calling the assembler, the workfile name **hello.pas**
will automatically changed to **hello.s**. When running the program,
the name will be changed to **hello.prog**.

## Volumes and the Default Volume
Volumes in the Tridora-System are partitions on the SD-Card which are
accessed by a nice name of up to 32 characters. A filename can contain
a volume specification, which is prepended to the filename with a `#`
character in front and a `:` at the end of the volume name. For example:
`#Examples:lines.pas` is a file named **lines.pas** on a volume with
the name of **Examples**.

If a filename does not contain a volume specification (i.e. it does
not start with a `#` character), the file subsystem will look for the
file on the default volume.

In the second line of the menu, the shell displays the current default
volume. The default volume can be changed with the _V_ command.

## Commands
Below is a list and short description of each command. Pressing any
other key will redisplay the menu.

### W - Change Workfile
Changes the workfile. To clear the workfile name, just press RETURN at
the prompt. If you accidently pressed _W_, you can press _Control-C_
to keep the old name.

### V - Change Default Volume
Changes the default volume. Lists all available volumes and then prompts
for the name of the new default volume.
Enter the name without a leading `#`. Press
return if you do not want to change the default volume.

### L - List Directory
Lists all files on the default volume.

### K - Reclaim Space and Defragment Volume
Calls the reclaim program.

The Tridora-filesystem does not automatically free up space from
deleted files, and it needs to be defragmented occasionally.
This is done with the _K_ command which executes the program **#SYSTEM:reclaim.prog**.

First, the program scans the default volume for space used up by deleted files.
If it finds any, it asks you if you want to reclaim that space. Press *y* to proceed
or *n* to exit the reclaim program, returning to the shell.

After reclaiming space, the program checks if there are multiple free space
regions on the volume. If this is the case, you are asked if you want to perform
the crunch operation, which will move files around so there are no more gaps in the
space allocated by files. On large volumes, this can take some time, so you can
choose if you want to proceed or not with *y* or *n*.

### O - Execute Program
This command allows you to call any program, independent of the current workfile.
It also allows you to specify parameters.

When using the _O_ command, you will be prompted for a program name. You can
also use a file name with a volume specification if the program file is not on the
default volume. If you omit the file extension **.prog**, it is added automatically.

After entering the program name, you can specify any program arguments. You have
to enter the arguments line by line, pressing RETURN after each argument.
After the last argument, just press RETURN (or if you do not want to give any arguments).

### D - Delete File
Will prompt you for a file name, then will try to delete that file.

### M - Rename File
Will prompt you for an old and a new filename, then will try to perform a rename.
You cannot use a volume specification in both filenames.
A file with the new name must not exist.

### P - Copy File
Will prompt you for an old an a new filename, then will try to perform a copy.
A file with the new name must not exist.
Copying works across different volumes.

The implementation of the _P_ command is inefficient, which will be noticable for larger files (>100KB).

### E - Edit
Starts the editor, editing the workfile.

### C - Compile
Calls the compiler with the workfile.

### A - Assemble
Calls the assembler with the workfile and the extension **.s**.

### B - Build
Calls the compiler with the workfile and then the assembler.

### R - Run
Runs the program compiled/assembled from the workfile, with the extension **.prog**.
