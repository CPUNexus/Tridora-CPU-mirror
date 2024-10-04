# Tridora CPU
<!--
#
# Copyright 2021 Sebastian Lederer
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
-->
Tridora CPU is a softcore CPU written in Verilog.
It is a stack machine design inspired by the J1 CPU and the NOVIX NC4016.
Unlike these CPUs, which are optimized for the Forth programming language,
Tridora CPU has some features intended to support Algol-like programming languages like Pascal. 
Essentially, it has specific instructions for accessing local variables on a stack frame.

## Architecture
The Tridora CPU has no user-accessible general-purpose registers.
It has three stacks: The __evaluation stack__, the __return stack__ and the __user stack__.

There are three user visible registers:
  * FP - Frame Pointer Register
  * BP - Base Pointer Register
  * RP - Return Stack Pointer Register

Registers and the evaluation stack elements have a fixed word size, which is currently 32 bits.

Memory access uses 32-bit words. Addressing still uses byte addresses, so the first memory word is at address 0, the next memory word is at address 4 and so on. There is no support for unaligned memory access or accessing bytes directly.

All instructions are 16 bits wide.

### Evaluation Stack
The evaluation stack is used for passing arguments to instructions or subroutines. Most instructions take one or two elements of the evaluation stack and leave the result on the stack.

The evaluation stack is implemented as internal register file for fast access. The top of the stack is held in an internal register called X. The next-to-the-top stack value is held in an internal register called Y. Instructions can only modify the X register (or memory). The names X and Y are not visible to the programmer.

The maximum depth of the evaluation stack is implementation-dependent. It should be at least 32 words deep.

### Return Stack
The return stack stores return addresses from subroutine calls. The RP register points to a memory area.

### User Stack
The user stack contains stack frames/activation records for higher-level programming languages. It is represented by the FP register.

## Instruction Format
There are 8 basic instructions which are encoded as bits 15-13 of an instruction word. Some instructions use the rest of the available bits (12-0) as an immediate operand. Some of the basic instructions use additional bits to encode more instructions.


The basic instructions are:

|instruction|description|opcode|
|---|---|---|
|BRANCH|relative branch|000| 
|ALU|ALU op|001|
|STORE|store to local memory|010|
|XFER|transfer program control|011|
|LOAD|load from local memory|100|
|CBRANCH|conditional branch|101|
|LOADC|load constant|110|
|EXT|extended instructions|111|

## Instructions
Note:
  * X is the top-of-stack element
  * Y is the next-to-top-of-stack-element
  * nX is the new top-of-stack-element

|Instruction|Operand|Description|
|-----------|-------|-----------|
|BRANCH|signed relative 13-bit-offset|unconditional branch|
|ADD|-|nX = X + Y|
|SUB|-|nX = Y - X|
|NOT|-|nX = ~X   |
|AND|-|nX = X & Y|
|OR|-|nX = X \| Y|
|XOR|-|nX = X ^ Y|
|CMP|comparison selector|signed compare, leaves 1 on stack if comparison is true, 0 otherwise|
|SHR|-|shift right|
|SHL|-|shift left|
|INC|4-bit unsigned constant|increment, nX = nX + operand|
|DEC|4-bit unsigned constant|decrement|
|CMPU|comparison selector|unsigned compare, leaves 1 on stack if comparison is true, 0 otherwise|
|BPLC|-|byte place|
|BROT|-|byte rotate, rotate left by 8 bits|
|BSEL|-|byte select|
|Y|-|copies Y (next-to-top-of-stack)|
|DUP|-|duplicates top-of-stack|
|NIP|-|removes next-to-top-of-stack element|
|DROP|-|removes top-of-stack element|
|SWAP|-|swap top- and next-to-top-of-stack elements|
|OVER|-|duplicates next-to-top-of-stack element|
|STORE|13-bit unsigned offset|store relative to FP or BP register|
|JUMP|-|jump to value of top-of-stack|
|CALL|-|subroutine call to value of top-of-stack|
|RET|-|return from subroutine|
|LOAD|13-bit-unsigned offset|load relative to FP or BP|
|CBRANCH|13-bit signed offset|conditional branch, branch if top-of-stack is not zero|
|LOADC|13-bit signed constant|load constant with sign-extend|
|LOADI|-|load from memory address contained in X|
|STOREI|4-bit unsigned constant|store X to memory address contained in Y, post-increment address by operand|
|LOADREL|-|load PC relative, load from memory word at an offset to the program counter|
|LOADREG|register spec|load from special register|
|STOREREG|register spec|store X to special register|
|FPADJ|10-bit signed constant|adjust FP register|
### Register Specification
|Spec|Description|
|----|-----------|
|FP  | Frame Pointer Register|
|BP  | Base Pointer Register |
|RP  | Return Stack Pointer  |

### Comparison Selectors
|Sel|Description      | Function |
|---|-----------------|----------|
|EQ | equal           | X == Y   |
|LT | less than       | Y < X    |
|NE | not equal       | X != Y   |
|LE | less or equal   | Y <= X   |
|GE | greater or equal| Y >= X   |
|GT | greater than    | Y > X    |

## Instruction Reference
### BRANCH
#### Description
Adds a signed 13-bit constant to the program counter. The offset needs to be an even
integer. An offset of 2 sets the PC to the next instruction (i.e. NOP). An offset of 0 is an infinite loop.
#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |0 |x |x |x |x |x |x |x |x |x |x |x |x |x |


|Bitfields|Description|
|---------|-----------|
| x       | operand   |

#### Examples
An infinite loop, using a label:

	LOOP:	BRANCH LOOP

### ADD
#### Description
nX = X + Y

Performs an integer addition on the two topmost stack elements. Leaves the result on the stack.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |0 |0 |0 |0 |u |u |X2Y| S | S |x |x |x |x |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| x       | operand(unused)  |


#### Examples
Add the value of local variable at offset 4 and constant 100, replacing the two arguments with the result:

	LOAD 4
	LOADC 100
	ADD

After this instruction sequence, the stack contains only the result value.

Add the value of local variable at offset 4 and constant 100, keeping the arguments on the stack and adding the result on top:

	LOAD 4
	LOADC 100
	ADD.S1.X2Y
### SUB
#### Description
nX = Y - X

Subtracts the next-to-top-of-stack value from the topmost stack element. Leaves the result on the stack.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |0 |0 |0 |1 |u |u |X2Y| S | S |x |x |x |x |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| x       | operand(unused)  |


#### Examples
Subtract the value of local variable at offset 4 from constant 100, replacing the two arguments with the result:

	LOADC 100
	LOAD 4
	SUB

After this instruction sequence, the stack contains only the result value.

Subtract the value of local variable at offset 4 from constant 100, keeping the arguments on the stack and adding the result on top:

	LOADC 100
	LOAD 4
	SUB.S1.X2Y

### NOT
#### Description
nX = ~X

Inverts all bits of topmost stack element. Stack pointer does not change.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |0 |0 |1 |0 |u |u |X2Y| S | S |x |x |x |x |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| x       | operand(unused)  |


#### Examples

Invert top-of-stack element:

	LOADC -1
	NOT

After this instruction sequence, the stack contains value 0.

### AND
#### Description
nX = X & Y

Performs a bitwise AND operation on the two topmost stack elements. Leaves the result on the stack.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |0 |0 |1 |1 |u |u |X2Y| S | S |x |x |x |x |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| x       | operand(unused)  |


#### Examples
Perform an AND on local variable at offset 4 and constant $FF (decimal 255), replacing the two arguments with the result:

	LOAD 4
	LOADC $FF
	AND

After this instruction sequence, the stack contains only the result value.

Isolate bit 3 from local variable at offset 4:

	LOAD 4
	LOADC 8
	AND

After this instruction sequence, the stack contains 8 or 0, depending on whether bit 3 was set in the local variable.

### OR
#### Description
nX = X | Y

Performs a bitwise OR operation on the two topmost stack elements. Leaves the result on the stack.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |0 |1 |0 |0 |u |u |X2Y| S | S |x |x |x |x |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| x       | operand(unused)  |


#### Examples
Perform an OR on local variable at offset 4 and constant 1, replacing the two arguments with the result:

	LOAD 4
	LOADC 1
	OR

After this instruction sequence, the stack contains only the result value.

Set bit 3 in value from local variable at offset 4:

	LOAD 4
	LOADC 8
	OR

After this instruction sequence, the stack contains the value from local variable with bit 3 set.

### XOR
#### Description
nX = X ^ Y

Performs a bitwise exclusive or (XOR) operation on the two topmost stack elements. Leaves the result on the stack.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |0 |1 |0 |1 |u |u |X2Y| S | S |x |x |x |x |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| x       | operand(unused)  |


#### Examples
Perform an XOR on local variable at offset 4 and binary constant 1010 (decimal 10), replacing the two arguments with the result:

	LOAD 4
	LOADC %1010
	XOR

After this instruction sequence, the stack contains only the result value.

### CMP
#### Description
Compares the two topmost stack elements, using the comparison selector given as operand.
The stack values are assumed to be signed integers.
Leaves 0 on the stack if the comparison is not valid, 1 otherwise. For the operand, there are
several predefined symbols, as shown in the table below.

For unsigned comparison see the CMPU instruction.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |0 |1 |1 |0 |u |u |X2Y| S | S |0 |ci |ce |cl |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| ci      | if 1, invert comparison result  |
| ce      | if 1, test if X is equal to Y  |
| cl      | if 1, test if Y is less than X  |

### Comparison Selectors
|Sel|Description      | Function | ci|ce|cl|
|---|-----------------|----------|---|--|--|
|EQ | equal           | X == Y   | 0 | 1| 0|
|LT | less than       | Y < X    | 0 | 0| 1|
|NE | not equal       | X != Y   | 1 | 1| 0|
|LE | less or equal   | Y <= X   | 0 | 1| 1|
|GE | greater or equal| Y >= X   | 1 | 0| 1|
|GT | greater than    | Y > X    | 1 | 1| 1|

#### Examples
Test if constant -10 is lower than local variable at offset 4, replacing the two arguments with the result:

	LOADC -10
	LOAD 4
	CMP LT

After this instruction sequence, the stack contains only the result value (0 if the variable was less than -10, 1 otherwise).

Test if local variable at offset 4 is equal to 12, placing the result on top of the stack:

	LOAD 4
	LOADC 12
	CMP.S1.X2Y EQ

After this instruction sequence, the stack contains the two arguments and the comparison result (1 if the variable equals 12, 0 otherwise).

### Y
#### Description
nX = Y

Gives the value of the next-to-top-of-stack element as result. This instruction is used in combination with the modifiers to
implement the instructions DROP, SWAP and OVER.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |0 |1 |1 |0 |u |u |X2Y| S | S |x |x |x |x |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| x       | operand(unused)  |


#### Examples
See DROP, SWAP and OVER.


### DROP
#### Description
Removes the topmost stack element. This is an alias for Y.SM1.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |0 |1 |1 |0 |u |u |X2Y| S | S |x |x |x |x |


|Bitfields|Description|
|---------|-----------|
| X2Y     | copy X register to Y (i.e. update stack), set to 0 for DROP |
| S       | stack movement, SM1,S0 or S1, set to SM1(-1) for DROP|
| u       | unused   |
| x       | operand(unused)  |


#### Examples

	LOADC 1
	DROP

After this instruction sequence, the stack is empty.

### SWAP
#### Description
Swaps the topmost and next to topmost stack elements. This is an alias for Y.S0.X2Y.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |0 |1 |1 |0 |u |u |X2Y| S | S |x |x |x |x |


|Bitfields|Description|
|---------|-----------|
| X2Y     | copy X register to Y (i.e. update stack), set to 1 for SWAP |
| S       | d by modifiers SM1,S0,S1, set to S0(0) for SWAP|
| u       | unused   |
| x       | operand(unused)  |


#### Examples

	LOADC 1
	LOADC 2
	SWAP

Before the SWAP instruction, the stack contains (1,2). After the SWAP instruction, the stack contains (2,1).

### OVER
#### Description
Duplicates the next-to-topmost stack element, adding it on top of the stack. This is an alias for Y.S1.X2Y.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |0 |1 |1 |0 |u |u |X2Y| S | S |x |x |x |x |


|Bitfields|Description|
|---------|-----------|
| X2Y     | copy X register to Y (i.e. update stack), set to 1 for OVER |
| S       | d by modifiers SM1,S0,S1, set to S1(1) for OVER|
| u       | unused   |
| x       | operand(unused)  |


#### Examples

	LOADC 1
	LOADC 2
	OVER

Before the OVER instruction, the stack contains (1,2). After the OVER instruction, the stack contains (1,2,1).

### SHR
#### Description
nX = X >> 1

Shift the content of X to the right by one bit. The new most significant bit can either
be 0 (logic shift), or a copy of the previous most significant bit (arithmetic shift).
This is determined by the XT modifier (sign extend).

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |1 |0 |0 |0 |u |XT|X2Y| S | S |x |x |x |x |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| XT      | if 0, shift in 0, if 1, shift in sign bit |
| x       | operand(unused)  |


#### Examples
Right-shift the value of local variable at offset 4 by 1, shifting in 0:

	LOAD 4
	SHR

After this instruction sequence, the stack contains the shifted value.
The shifted value replaces the old value on the stack.

Right-shift with sign-extend, putting the new value on top of the stack, keeping
the old value on the stack:

	LOAD 4
	SHR.S1.X2Y

After this sequence, the stack contains the original value of the variable and the
shifted value.

### SHL
#### Description
nX = X << 1

Shift the content of X to the left by one bit. The new least significant bit 
is set to 0.

If the operand is set to 2, the content of X is shifted left by two bits instead.
Every other operand value (including zero) where bit 1 is set to zero results in a shift by one bit.

The assembler treats the operand as optional and, if present, it should be set to 1 or 2.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |1 |0 |0 |1 |u |u |X2Y| S | S |o |o |o |o |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| x       | operand  |


#### Examples
Left-shift the value of local variable at offset 4 by 1, shifting in 0:

	LOAD 4
	SHL

After this instruction sequence, the stack contains the shifted value.
The shifted value replaces the old value on the stack.

Left-shift , shifting in 0, putting the new value on top of the stack, keeping
the old value on the stack:

	LOAD 4
	SHL.S1.X2Y

After this sequence, the stack contains the original value of the variable and the
shifted value.

Multiply the value of local variable at offset 12 by 4, using a two-bit-shift:

	LOAD 12
	SHL 2

After this sequence, the stack contains the shifted value.

### INC
#### Description
nX = X + operand

Add a small constant to the topmost stack value.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |1 |0 |0 |1 |u |u |X2Y| S | S |o |o |o |o |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| o       | 4-bit unsigned operand  |

#### Examples
Load local variable at offset 4 and increment by 1:

	LOAD 4
	INC 1

After this instruction sequence, the stack contains the incremented value.
The incremented value replaces the old value on the stack.

Load variable and increment by 3, putting the new value on top of the stack, keeping
the old value on the stack:

	LOAD 4
	INC.S1.X2Y 3

After this sequence, the stack contains the original value of the variable and the
incremented value.

### DEC
#### Description
nX = X - operand

Substract a small constant from the topmost stack value.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |1 |0 |1 |1 |u |u |X2Y| S | S |o |o |o |o |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| o       | 4-bit unsigned operand  |

#### Examples
Load local variable at offset 4 and decrement by 1:

	LOAD 4
	DEC 1

After this instruction sequence, the stack contains the decremented value.
The decremented value replaces the old value on the stack.

Load variable and decrement by 3, putting the new value on top of the stack, keeping
the old value on the stack:

	LOAD 4
	DEC.S1.X2Y 3

After this sequence, the stack contains the original value of the variable and the
decremented value.

### DUP
#### Description
Duplicate the topmost stack element, adding it on top of the stack. This is an alias for INC.S0.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |1 |0 |0 |1 |u |u |X2Y| S | S |o |o |o |o |


|Bitfields|Description|
|---------|-----------|
| X2Y     | copy X register to Y (i.e. update stack), set to 0 for DUP |
| S       | signed 2-bit field that specifies the stack movement, set to S0 (0) for DUP |
| u       | unused   |
| o       | 4-bit unsigned operand, set to 0 for DUP  |

#### Examples

	LOADC 1
	DUP	

After this instruction sequence, the stack contains (1,1).

### NIP
#### Description
Remove the next-to-topmost stack element. This is an alias for INC.SM1.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |1 |0 |0 |1 |u |u |X2Y| S | S |o |o |o |o |


|Bitfields|Description|
|---------|-----------|
| X2Y     | copy X register to Y (i.e. update stack), set to 0 for NIP |
| S       | signed 2-bit field that specifies the stack movement, set to SM1 (-1) for NIP |
| u       | unused   |
| o       | 4-bit unsigned operand, set to 0 for NIP  |

#### Examples

	LOADC 1
	LOADC 2
	LOADC 3
	NIP	

After this instruction sequence, the stack contains (1,3).

### BPLC
#### Description
Byte place - place a byte inside a word. The LSB of the topmost stack contains
the byte value. The next-to-top-of-stack element determines the byte position.
The most significant byte is represented by the value 0, the least significant byte
by the value 3. Only bits 0-2 are taken into consideration.

In other words, a byte address can be used to place a byte from
a 32-bit word.

All other bytes are set to zero.

See also BSEL.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |1 |1 |0 |1 |u |u |X2Y| S | S |u |u |u |u |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |

#### Examples

Place the value $42 as the most significant byte in the top-of-stack word:

	LOADC 0
	LOADC $42
	BPLC

After this instruction sequence, the stack contains the value $42000000.

Change a byte in an existing word:

		LOADCP AWORD ; addr for STOREI below
		DUP	; duplicate addr for LOADI
		LOADI	; load original value
		LOADC 2 ; load byte position
		LOADC $FF ; and byte value
		BPLC
		NOT	; create mask
		AND	; clear byte we want to set
		LOADC 2
		LOADC $77
		BPLC	; place byte
		OR	; combine with other bytes from original word
		STOREI
		DROP
	AWORD:	.WORD $1234ABCD

After this instruction sequence, the word at AWORD contains the value $123477CD.

### CMPU
#### Description
Compares the two topmost stack elements, using the comparison selector given as operand.
The stack values are assumed to be unsigned integers.
Leaves 0 on the stack if the comparison is not valid, 1 otherwise. For the operand, there are
several predefined symbols, as shown in the table below.

For signed comparison see the CMP instruction.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |1 |1 |0 |0 |u |u |X2Y| S | S |0 |ci |ce |cl |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |
| ci      | if 1, invert comparison result  |
| ce      | if 1, test if X is equal to Y  |
| cl      | if 1, test if Y is less than X  |

### Comparison Selectors
|Sel|Description      | Function | ci|ce|cl|
|---|-----------------|----------|---|--|--|
|EQ | equal           | X == Y   | 0 | 1| 0|
|LT | less than       | Y < X    | 0 | 0| 1|
|NE | not equal       | X != Y   | 1 | 1| 0|
|LE | less or equal   | Y <= X   | 0 | 1| 1|
|GE | greater or equal| Y >= X   | 1 | 0| 1|
|GT | greater than    | Y > X    | 1 | 1| 1|

#### Examples
Test if constant 10 is lower than local variable at offset 4, replacing the two arguments with the result:

	LOADC 10
	LOAD 4
	CMPU LT

After this instruction sequence, the stack contains only the result value (0 if the variable was greater or equal to 10, 1 otherwise).

Test if local variable at offset 4 is equal to 12, placing the result on top of the stack:

	LOAD 4
	LOADC 12
	CMPU.S1.X2Y EQ

After this instruction sequence, the stack contains the two arguments and the comparison result (1 if the variable equals 12, 0 otherwise).

### BROT
#### Description

Byte rotate - rotate 
the topmost stack element left by 8 bits.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |1 |1 |1 |0 |u |u |X2Y| S | S |u |u |u |u |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |

#### Examples
Swap high and low half-words:

	LOADCP $1234ABCD
	BROT
	BROT

After this instruction sequence, the stack contains the value $ABCD1234.

### BSEL
#### Description

Byte select - replace the topmost stack element with a byte value from itself selected
by the value of the next-to-top-of-stack element. The most significant byte is
represented by the value 0, the least significant byte by the value 3. Only the
bits 0-2 of the value are taken into consideration.

In other words, a byte address can be used to select that byte from
a 32-bit word.

Bits 8-31 of the result are set to zero.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |0 |1 |1 |1 |1 |1 |u |u |X2Y| S | S |u |u |u |u |


|Bitfields|Description|
|---------|-----------|
| X2Y     | if this bit is set, copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| u       | unused   |

#### Examples

Select the most significant byte from the top-of-stack word:

	LOADC 0
	LOADCP $1234ABCD
	BSEL

After this instruction sequence, the stack contains the value $12.

Load a byte from the address contained in the top-of-stack word:

		LOADCP AWORD+2
		LOADI.S1.X2Y
		BSEL
	AWORD:	.WORD $1234ABCD

After this instruction sequence, the stack contains the value $AB.
Since bits 0-1 are ignored by the LOADI instruction, the word at the address
AWORD instead of AWORD+2 is placed on the stack. The BSEL instruction looks only at the bits 0-1,
so it selects the corresponding byte specified by the address.

### STORE
#### Description
Store the value of the topmost stack element into a local variable specified by a 13-bit unsigned constant.
The effective address for the memory transfer is calculated by adding the constant to either the FP register (default)
or the BP register (specified by modifier B).

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |1 |0 |o |o |o |o |o |o |o| o | o |o |o |o |B |


|Bitfields|Description|
|---------|-----------|
| o       | 13-bit unsigned operand (bit 0 is always 0) |
| B       | if set to 1, use BP register as base address, else use FP register |

#### Examples
Store value 0 into local variable at offset 4:

	LOADC 0
	STORE 4

Store value 1 into global variable at offset 1024 (assuming the BP register points to global variables):

	LOADC 1
	STORE.B 1024

### XFER
#### Description
Transfer program control. Used with several modifiers to implement JUMP, CALL and RET.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |1 |1 |u |u |u |RS |RS |R2P|P2R|S|S|u|u|u|X2P|


|Bitfields|Description|
|---------|-----------|
| u       | unused    |
| RS      | 2-bit signed field specifying the return stack movement (-1,0 or 1, modifiers RSM1, RS0, RS1)  |
| R2P     | set program counter to value from return stack  |
| P2R     | write program counter value to memory address in return stack pointer|
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| X2P     | set program counter to X |

#### Examples
Execute a JUMP instruction:

	LOADC EXIT
	JUMP

Execute a CALL instruction:

	LOADC MYSUBROUTINE
	CALL

Execute a RET instruction:

	EMPTYSUBROUTINE:
		RET

	
### JUMP
#### Description
Jump to memory address contained in the topmost stack element. The topmost stack element is removed afterwards.

Alias for XFER.SM1.X2P.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |1 |1 |u |u |u |RS |RS |R2P|P2R|S|S|u|u|u|X2P|


|Bitfields|Description|
|---------|-----------|
| u       | unused    |
| RS      | 2-bit signed field specifying the return stack movement, set to -1 for JUMP |
| R2P     | set program counter to value from return stack, set to 0 for JUMP  |
| P2R     | write program counter value to return stack, set to 0 for JUMP |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| X2P     | set program counter to X, set to 1 for JUMP |

#### Examples
See XFER instruction.
	
### CALL
#### Description
Execute a subroutine call. The address of the next instruction is written to memory referenced by the return stack pointer. Then the program counter is set to the value of the topmost stack element, which is removed afterwards.
The return stack pointer is decremented.

Alias for XFER.RS1.SM1.P2R.X2P.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |1 |1 |u |u |u |RS |RS |R2P|P2R|S|S|u|u|u|X2P|


|Bitfields|Description|
|---------|-----------|
| u       | unused    |
| RS      | 2-bit signed field specifying the return stack movement, set to 1 for CALL |
| R2P     | set program counter to value from return stack, set to 0 for CALL  |
| P2R     | write program counter value to return stack, set to 1 for CALL |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| X2P     | set program counter to X, set to 0 for CALL |

#### Examples
See XFER instruction.
	
### RET
#### Description
Return from subroutine. The program counter is set to the value read from memory referenced by the return stack pointer.
The return stack pointer is incremented.

Alias for XFER.RSM1.R2P.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|0 |1 |1 |u |u |u |RS |RS |R2P|P2R|S|S|u|u|u|X2P|


|Bitfields|Description|
|---------|-----------|
| u       | unused    |
| RS      | 2-bit signed field specifying the return stack movement, set to -1 for RET |
| R2P     | set program counter to value from return stack, set to 1 for RET  |
| P2R     | write program counter value to return stack, set to 0 for RET |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| X2P     | set program counter to X, set to 0 for RET |

#### Examples
See XFER instruction.
	
### CBRANCH
#### Description
Conditional branch. Executes a branch specified by a 13-bit signed constant if the topmost stack element is nonzero.
The topmost stack element is then removed.

If the modifier Z is used, the branch is executed if the topmost stack element is zero.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|1 |0 |1 |x |x |x |x |x |x |x |x |x |x |x |x |N |


|Bitfields|Description|
|---------|-----------|
| x       | operand   |
| N       | if 1 (default), branch if X if nonzero, if 0 (modifier Z), branch if X is zero |

#### Examples
Branch if local variable at offset 4 is nonzero:

		LOAD 4
		CBRANCH CONTINUE
		RET
	CONTINUE:
		...	

Branch if local variables at offset 4 and 16 are both zero:

		LOAD 4
		LOAD 16
		OR
		CBRANCH.Z EXIT
		...
	EXIT:
		RET

### LOAD
#### Description
Load the value a local variable specified by a 13-bit unsigned constant onto the stack.
The effective address for the memory transfer is calculated by adding the constant to either the FP register (default)
or the BP register (specified by modifier B).

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|1 |0 |0 |o |o |o |o |o |o |o| o | o |o |o |o |B |


|Bitfields|Description|
|---------|-----------|
| o       | 13-bit unsigned operand (bit 0 is always 0) |
| B       | if set to 1, use BP register as base address, else use FP register |

#### Examples
Load local variable at offset 4:

	LOAD 4

Load global variable at offset 1024 (assuming the BP register points to global variables):

	LOAD.B 1024

### LOADC
#### Description
Load a 13-bit signed constant onto the stack.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|1 |1 |0 |o |o |o |o |o |o |o| o | o |o |o |o |o |


|Bitfields|Description|
|---------|-----------|
| o       | 13-bit signed operand  |

#### Examples
Load constants 1,2,-4000 onto the stack:

	LOADC 1
	LOADC 2
	LOADC -4000	

### LOADI
#### Description

Load from memory address contained in the topmost stack element. The topmost stack element is replaced by the value read from memory.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|1 |1 |1 |0 |0 |1 |W |u |u |X2Y|S|S|o |o |o |o |


|Bitfields|Description|
|---------|-----------|
| W       | perform memory write, set to 0 for LOADI |
| X2Y     | copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| o       | 4 bit unsigned operand, unused for LOADI | 
#### Examples
Take pointer from local variable at offset 4 and load from that address:

	LOAD 4
	LOADI

### STOREI
#### Description

Store value from the topmost stack element at memory address contained in next-to-topmost stack element. Removes two elements from the stack and puts the address back on top, incremented by a small constant (which may be zero).

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|1 |1 |1 |0 |0 |1 |W |u |u |X2Y|S|S|o |o |o |o |


|Bitfields|Description|
|---------|-----------|
| W       | perform memory write, set to 1 for STOREI |
| X2Y     | copy X register to Y (i.e. update stack) |
| S       | signed 2-bit field that specifies the stack movement. Can be -1,0 or 1, specified by modifiers SM1,S0,S1|
| o       | 4 bit unsigned operand | 
#### Examples
Take pointer from local variable at offset 4 and store the constant value 11 at that address:

	LOAD 4
	LOADC 11
	STOREI
	DROP

After this instruction sequence, the stack is empty.

Store constant value 42 with post-increment (pointer is at local variable offset 4):

	LOAD 4
	LOADC 42
	STOREI 4

After this instruction sequence, the stack contains the pointer value incremented by 4.

### LOADREG
#### Description

Load content of a register onto the stack.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|1 |1 |1 |0 |0 |0 |W |u |u |u |u |u |o |o |o |o |


|Bitfields|Description|
|---------|-----------|
| W       | if set to 1, write to register, else read from register (0 for LOADREG)|
| u       | unused | 
| o       | register specification |

#### Register Specification
|Spec|Description|operand field|
|----|-----------|-------------|
|FP  | Frame Pointer Register|0000|
|BP  | Base Pointer Register |0001|
|RP  | Return Stack Pointer  |0010|

#### Examples
Get content of BP register:

	LOADREG BP

### STOREREG
#### Description

Set register to value of the topmost stack element, which is removed afterwards.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|1 |1 |1 |0 |0 |0 |W |u |u |u |u |u |o |o |o |o |


|Bitfields|Description|
|---------|-----------|
| W       | if set to 1, write to register, else read from register (1 for STOREREG)|
| u       | unused | 
| o       | register specification |

#### Register Specification
|Spec|Description|operand field|
|----|-----------|-------------|
|FP  | Frame Pointer Register|0000|
|BP  | Base Pointer Register |0001|
|RP  | Return Stack Pointer  |0010|

#### Examples
Set BP register to 2000 (hexadecimal):

	LOADCP $2000
	STOREREG BP

### LOADREL
#### Description

Load PC relative: Load a word-sized value from the address calculated by adding an unsigned offset to the program counter.

The assembler supports a pseudo-instruction LOADCP (load from constant pool) that automatically places values into a constant pool
which then can be used with the LOADREL instruction. A constant pool is automatically placed at the end of a program. Additional
constant pools can be created anywhere with the .CPOOL directive. Since the offset to LOADREL can be at most 1022, more than one
constant pool is required when the code size exceeds 1022 bytes.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|1 |1 |1 |1 |0 |1 |o |o |o |o |o |o |o |o |o |o |


|Bitfields|Description|
|---------|-----------|
| o       | unsigned 10-bit offset to the program counter |

#### Examples
Load a large constant:

	LOADCP 30000

### LOADCP
#### Description

Load from constant pool: A pseudo-instruction to make using the LOADREL instruction easier. See the LOADREL instruction.


### FPADJ
#### Description

Adjust frame pointer. Add a signed 10-bit constant to the FP register. Used to adjust the user stack
when entering or leaving a subroutine.

#### Instruction format
|_bit_  |15|14|13|12|11|10|09|08|07|06|05|04|03|02|01|00|
|-      |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |- |
|_Value_|1 |1 |1 |0 |1 |1 |o |o |o |o |o |o |o |o |o |o |


|Bitfields|Description|
|---------|-----------|
| o       | 10-bit signed operand |

#### Examples
Make room for 32 bytes of local variables when entering a procedure:

	MYPROC:
		FPADJ -32
		...

Move frame pointer back by 32 bytes when leaving the procedure:

	MYPROC:
		...
		FPADJ 32
		RET


