// Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details
package main

import (
	"fmt"
	"strings"
)

type word uint32
const wordbits = 32
const wordbytes = 4
const wordmask = 0xFFFFFFFF
const hsbmask  = 0x80000000
const estackDepth = 64
type CPU struct {
	ESP word;
	X word;
	PC, FP,BP,RP word;
	IR,IV word;

	estack [estackDepth] word;

	mem *Mem;

	stopped bool
	trace bool
	singlestep bool
}

func sign_extend(bits word, wordbits int) int {
	signmask := word(1 << (wordbits - 1))
	signbit := (bits & signmask) != 0
	// fmt.Printf("sign_extend %b %v signmask %08X signbit %v\n", bits, wordbits, signmask, signbit)
	if signbit {
		return int(bits & ^signmask) - int(signmask)
	} else {
		return int(bits)
	}
}

func (c *CPU) initialize() {
	c.ESP = 0
	c.PC  = 0
	c.X   = 0
	c.FP  = 65020	// these are the values set by the ROM monitor
	c.RP  = 65024   // for FP and RP

	c.singlestep = false
}

func (c *CPU) printEstack() {
	fmt.Printf("[")
	for i := word(1); i <= c.ESP; i++ {
		fmt.Printf(" %08X", c.estack[i])
	}
	fmt.Printf(" %08X ] (%v)\n", c.X, c.ESP)
}

func (c *CPU) showStep(desc string, operand int, opWidth int) {
	if !c.trace { return }

	var opStr string
	if opWidth == 0 {
		opStr = ""
	} else {
		opStr = fmt.Sprintf(" %04X", operand)
	}

	fmt.Printf("%08x %-10s%5s    ", c.PC, desc, opStr)
	c.printEstack()
}

func (c *CPU) getOperand(insWord word, bits int) int {
	return int(insWord & word((1 << bits) - 1))
}

func (c *CPU) getSignedOperand(insWord word, bits int) int {
	bitValue := (1 << (bits - 1))
	signBit := insWord & word(bitValue)
	bitMask := (bitValue << 1) - 1
	//fmt.Printf("getSignedOperand bitValue: %v, bitMask: %v\n", bitValue, bitMask)
	o := int(insWord & word(bitMask))
	if signBit != 0 {
		o = int(o) - (bitValue << 1)
	}
	//fmt.Printf("getSignedOperand: %v %v -> %d\n",insWord, bits, o)
	return o
}

func (c *CPU) getBit(insWord word, bitnum int) int {
	return int(insWord >> bitnum) & 1
}

func (c *CPU) getBits(insWord word, mask word, shiftCount int) word {
	return (insWord & mask) >> shiftCount
}

func (c *CPU) getSignedBits(insWord word, mask word, shiftCount int) int {
	result := (insWord & mask) >> shiftCount
	signMask := ((mask >> shiftCount) + 1) >> 1 
	//fmt.Printf("getSignedBits %016b signMask %v signBit %v\n", insWord, signMask, insWord & signMask)
	if result & signMask != 0 {
		return - int(result &  ^signMask)
	} else {
		return int(result)
	}
}

func (c * CPU) addToWord(v word, offset int) word {
	if offset < 0 {
		v -= word(-offset)
	} else {
		v += word(offset)
	}
	return v
}

func (c *CPU) step() error {
	nPC := c.PC
	nFP := c.FP
	nBP := c.BP
	nRP := c.RP
	nX  := c.X
	nESP := c.ESP
	
	Y := c.estack[c.ESP]

	insWord, err := c.mem.read(c.PC)
	if err != nil { return err }
	if c.PC % 4 == 0 {
		insWord = insWord >> 16
	} else {
		insWord = insWord & 0xFFFF
	}

	baseIns := insWord >> 13

	nPC += 2

	x2y := false
	deltaESP := 0

	oplen := 0

	switch baseIns {
	// BRANCH
	case 0b000:
		operand := c.getSignedOperand(insWord, 13)

		c.showStep("BRANCH", operand, 13)

		nPC = word(int(c.PC) + operand)

		if operand == 0 {
			fmt.Printf("BRANCH 0 encountered - stopped at PC %08X\n", nPC)
			c.stopped = true
		}
	// ALU
	case 0b001:
		aluop :=       c.getBits(insWord,   0b0001111000000000, 9)
		deltaESP = c.getSignedBits(insWord, 0b0000000000110000, 4)
		ext := c.getBit(insWord, 7) != 0
		x2y = c.getBit(insWord, 6) != 0
		operand := c.getOperand(insWord, 4)
		// fmt.Printf("aluop %v %v %v %v %v\n", aluop, s, ext, x2y, operand)
		name := "ALU"
		switch aluop {
		case 0:
			name = "ADD"
			nX = c.X + Y
		case 1:
			name = "SUB"
			nX = Y - c.X
		case 2:
			name = "NOT"
			nX = ^ c.X
		case 3:
			name = "AND"
			nX = c.X & Y
		case 4:
			name = "OR"
			nX = c.X | Y
		case 5:
			name = "XOR"
			nX = c.X ^ Y
		case 6:
			name = "CMP"
			oplen = 2
			cmp_i  := c.getBit(insWord,2) != 0
			cmp_eq := c.getBit(insWord,1) != 0
			cmp_lt := c.getBit(insWord,0) != 0
			s_x := sign_extend(c.X, wordbits)
			s_y := sign_extend(Y, wordbits)
			result := (cmp_eq && (s_x == s_y)) || (cmp_lt && (s_y < s_x))
			if cmp_i { result = !result }
			if result { nX = 1 } else { nX = 0 }
		case 7:
			name = "Y"
			if !x2y && (deltaESP == -1) { name = "DROP" }
			if x2y && (deltaESP == 0)   { name = "SWAP" }
			nX = Y
		case 8:
			name = "SHR"
			nX = c.X >> 1
			if ext {
				nX = nX | (c.X & hsbmask)
			}
		case 9:
			name = "SHL"
			nX = c.X << 1
			if operand & 2 != 0 {
				nX = nX << 1
			}
			oplen = 2
		case 10:
			name = "INC"
			oplen = 4
			if (operand == 0) && (deltaESP == 1) && x2y { name = "DUP" }
			nX = c.X + word(operand)
		case 11:
			name = "DEC"
			oplen = 4
			nX = c.X - word(operand)
		case 12:
			name = "CMPU"
			oplen = 2
			cmp_i  := c.getBit(insWord,2) != 0
			cmp_eq := c.getBit(insWord,1) != 0
			cmp_lt := c.getBit(insWord,0) != 0
			result := (cmp_eq && (c.X == Y)) || (cmp_lt && (Y < c.X))
			if cmp_i { result = !result }
			if result { nX = 1 } else { nX = 0 }
		case 13:
			name = "BPLC"
			nX = (c.X & 0xFF) << ((3 - (Y & 3)) * 8)
		case 14:
			name = "BROT"
			nX = ((c.X & 0x00FFFFFF) << 8 ) | ((c.X & 0xFF000000) >> 24)
		case 15:
			name = "BSEL"
			shift := (3 - (Y & 3)) * 8
			nX = (c.X >> shift) & 0xFF
		}
		c.showStep(name, operand, oplen)
	// STORE
	case 0b010:
		operand := c.getOperand(insWord, 13)
		var ea word
		var name string
		if (insWord & 1) == 1 {
			name = "STORE.B"
			ea = c.BP + word(operand)
		} else {
			name = "STORE"
			ea = c.FP + word(operand)
		}

		c.showStep(name, operand, oplen)

		err = c.mem.write(c.X, ea)
		if err != nil { return err }

		deltaESP = -1
		nX = Y
	// XFER
	case 0b011:
		var name string

		deltaRP := c.getSignedBits(insWord, 0b0000001100000000, 8)
		deltaESP = c.getSignedBits(insWord, 0b0000000000110000, 4)
		r2p := c.getBit(insWord, 7) != 0
		p2r := c.getBit(insWord, 6) != 0
		x2p := c.getBit(insWord, 0) != 0

		if deltaRP >= 0 {
			nRP = c.RP + word(deltaRP * wordbytes)
		} else {
			nRP = c.RP - word(-deltaRP * wordbytes)
		}

		if (deltaRP == 1) && (deltaESP == -1) && p2r && x2p {
			name = "CALL"
		} else
		if (deltaRP == -1) && (deltaESP == 0) && r2p {
			name = "RET"
		} else
		if (deltaRP == 0) && (deltaESP == -1) && x2p && !p2r {
			name = "JUMP"
		} else {
			var b strings.Builder
			b.WriteString("XFER")
			if deltaRP == -1 { b.WriteString(".RSM1") }
			if deltaRP == 1 { b.WriteString(".RS1") }
			if deltaESP == -1 { b.WriteString(".SM1") }
			if deltaESP == 1 { b.WriteString(".S1") }
			if r2p { b.WriteString(".R2P") }
			if p2r { b.WriteString(".P2R") }
			if x2p { b.WriteString(".X2P") }
			name = b.String()
		}

		c.showStep(name, 0, 0)

		if r2p {
			nPC, err = c.mem.read(c.RP)
			if err != nil { return err }
		}
		if p2r {
			err = c.mem.write(nPC, nRP)
			if err != nil { return err }
		}
		if x2p {
			nPC = c.X
			nX = Y
		}

	// LOAD
	case 0b100:
		operand := c.getOperand(insWord, 13)
		var ea word
		var name string
		if (insWord & 1) == 1 {
			name = "LOAD.B"
			operand &= ^1
			ea = c.BP + word(operand)
		} else {
			name = "LOAD"
			ea = c.FP + word(operand)
		}

		c.showStep(name, operand, oplen)

		deltaESP = 1
		x2y = true

		nX, err = c.mem.read(ea)
		if err != nil { return err }
	// CBRANCH
	case 0b101:
		operand := c.getSignedOperand(insWord, 13)
		var name string
		invert := (operand & 1) == 0
		operand = operand & -2 // clear bit 0

		if invert { name = "CBRANCH.Z" } else { name = "CBRANCH" }

		c.showStep(name, operand, 13)

		deltaESP = -1
		nX = Y

		if (c.X != 0 && !invert) || (c.X == 0 && invert) {
			nPC = word(int(c.PC) + operand)
		}
	// LOADC
	case 0b110:
		operand := c.getSignedOperand(insWord, 13)
		oplen = 13

		c.showStep("LOADC", operand, oplen)

		deltaESP = 1
		x2y = true
		nX = word(operand)
	// EXT
	case 0b111:
		extop :=         c.getBits(insWord, 0b0001111000000000, 10)
		deltaESP = c.getSignedBits(insWord, 0b0000000000110000, 4)
		writeFlag := c.getBit(insWord, 9) != 0
		// signExtend := c.getBit(insWord,7) != 0
		x2y       = c.getBit(insWord, 6) != 0
		operand := c.getOperand(insWord, 4)

		var name string

		switch extop {
		// LOADREG/STOREREG
		case 0:
			oplen = 4
			if writeFlag {
				name = "STOREREG"
				switch operand {
				case 0: nFP = c.X
				case 1: nBP = c.X
				case 2: nRP = c.X
				case 3: c.IV  = c.X // should be nIV
				case 4: c.IR  = c.X // should be nIR
				default: fmt.Errorf("Invalid STOREREG operand %v at %08X", operand, c.PC)
				}

				c.showStep(name, operand, oplen)
				deltaESP = -1
				x2y = false
				nX = Y
			} else {
				name = "LOADREG"
				switch operand {
				case 0: nX = c.FP
				case 1: nX = c.BP
				case 2: nX = c.RP
				case 3: nX = c.IV
				case 4: nX = c.IR
				case 5: nX = c.ESP
				default: fmt.Errorf("Invalid LOADREG operand %v at %08X", operand, c.PC)
				}	
				c.showStep(name, operand, oplen)
				deltaESP = 1
				x2y = true
			}
		// LOADI/STOREI
		case 1:
			if writeFlag { name = "STOREI" } else { name = "LOADI" }
			c.showStep(name, operand, oplen)

			if writeFlag {
				oplen = 4
				err = c.mem.write(c.X, Y)
				if err != nil { return err }
				nX = Y + word(operand)
			} else {
				nX, err = c.mem.read(c.X)
				if err != nil { return err }
			}

		// FPADJ
		case 3:
			operand := c.getSignedOperand(insWord, 10)
			oplen = 10
			nFP = c.FP + word(operand)
			deltaESP = 0
			x2y = false
			c.showStep("FPADJ", operand, oplen)
		// LOADREL
		case 5:
			offset := c.getOperand(insWord, 10)

			c.showStep("LOADREL", offset, 10)

			nX, err = c.mem.read(c.PC + word(offset))
			if err != nil { return err }
			x2y = true
			deltaESP = 1
		default:
			return fmt.Errorf("Invalid EXT instruction %v at %08X", extop, c.PC)
		}
	default:
		return fmt.Errorf("Invalid instruction %04X at %08X", insWord, c.PC)
	}

	nESP = c.addToWord(nESP, deltaESP)
	if nESP < 0 || nESP >= estackDepth {
		return fmt.Errorf("estack overflow %v at %08X", nESP, c.PC)
	}
 
	if x2y {
		c.estack[nESP] = c.X
	}

	c.PC = nPC
	c.FP = nFP
	c.BP = nBP
	c.X = nX
	c.RP = nRP
	c.ESP = nESP

	return nil
}
