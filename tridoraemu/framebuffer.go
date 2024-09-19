// Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details
package main

import (
	// "fmt"
	"image/color"
	"github.com/hajimehoshi/ebiten/v2"
)

const VmemWords = 32768
const PaletteSlots = 16
const FB_RA = 0
const FB_WA = 1
const FB_IO = 2
const FB_PS = 3
const FB_PD = 4
const FB_CTL= 5

const PixelMask = 0b11110000000000000000000000000000
const PixelPerWord = 8
const VmemWidth = 32
const BitsPerPixel = 4
const ScreenWidth = 640
const ScreenHeight = 400
const WordsPerLine = ScreenWidth / PixelPerWord

type Framebuffer struct {
	framebuffer *ebiten.Image
	palette [PaletteSlots] color.Color
	readAddr word
	writeAddr word
	paletteSlot word
	vmem [VmemWords]word
	readCount int
}

func (f *Framebuffer) initialize() {
	f.framebuffer = ebiten.NewImage(ScreenWidth, ScreenHeight)
	for i := 0; i <PaletteSlots; i++ {
		f.palette[i] = color.RGBA{0,0,0,0}
	}
}

func (f *Framebuffer) read(byteaddr word) (word, error) {
	result := word(0)

	addr := byteaddr & 0x7F
	switch addr {
	case FB_RA: result = f.readAddr
	case FB_WA: result = f.writeAddr
	case FB_IO: result = f.readVmem()
	case FB_PS: result = f.paletteSlot
	case FB_PD: result = f.readPalette()
	case FB_CTL: result = f.readCtl()
	default:
	}
	return result, nil
}

func (f *Framebuffer) write(value word, byteaddr word) (error) {
	addr := byteaddr & 0x7F
	switch addr {
	case FB_RA: f.readAddr = value
	case FB_WA: f.writeAddr = value
	case FB_IO: f.writeVmem(value)
	case FB_PS: f.paletteSlot = value
	case FB_PD: f.writePalette(value)
	case FB_CTL: f.writeCtl(value)
	default:
	}

	idle(false)

	return nil
}

func (f *Framebuffer) readVmem() word {
	result := f.vmem[f.readAddr & (VmemWords - 1)]
	f.readAddr += 1
	return result
}

func (f *Framebuffer) writeVmem(value word) {
	vaddr := f.writeAddr & (VmemWords - 1)
	f.vmem[vaddr] = value

	y := vaddr / WordsPerLine
	x := vaddr % WordsPerLine * PixelPerWord

	for i := 0; i < PixelPerWord; i++ {
		pixel := (value & PixelMask) >> (VmemWidth - BitsPerPixel)
		value = value << BitsPerPixel
		
		col := f.palette[pixel]
		//fmt.Printf("set pixel %v, %v\n", x,y)
		f.framebuffer.Set(int(x), int(y), col)
		x = x + 1
	}

	f.writeAddr += 1
}

func (f *Framebuffer) readPalette() word {
	return word(0)
}

func (f *Framebuffer) writePalette(value word) {
	// 4 bits per color channel
	r := uint8((value & 0b111100000000) >> 8)
	g := uint8((value & 0b000011110000) >> 4)
	b := uint8((value & 0b000000001111) >> 0)

	// scale to 0-255
	r = r << 4
	g = g << 4
	b = b << 4

	f.palette[f.paletteSlot] = color.RGBA{r,g,b,0}
}

func (f *Framebuffer) readCtl() word {
	if f.readCount == 0 {
		f.readCount = 1000
		return word(0)
	} else {
		f.readCount -= 1
		return word(1)
	}
}

func (f *Framebuffer) writeCtl(value word) {
}
