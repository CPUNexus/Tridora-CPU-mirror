// Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details
package main

import (
	"fmt"
	"log"
	"errors"
	"flag"
	"time"
	"github.com/hajimehoshi/ebiten/v2"
	// "github.com/hajimehoshi/ebiten/v2/ebitenutil"
	// "image/color"
)

const IdleTicks = 1000

var consoleChan chan byte
var cpu CPU
var mem Mem
var uart UART
var framebuffer Framebuffer
var sdspi SDSPI
var irqc IRQC

var Terminated = errors.New("terminated")

var idleCounter int = IdleTicks

func idle(canGoIdle bool) {
	if canGoIdle {
		if idleCounter > 0 { idleCounter -= 1 }
	} else {
		if idleCounter != IdleTicks { idleCounter = IdleTicks }
	}
}

type Game struct{
		x,y int
		stepsPerFrame int
		lastFrameDuration time.Duration
	}

func (g *Game) Update() error {
	startTime := time.Now()

	for i := 0; i < g.stepsPerFrame; i++ {
		err := cpu.step()
		if err != nil {
			log.Printf("Stopped by error at PC %08X",cpu.PC)
			log.Print(err)
			return Terminated
		}

		if cpu.stopped { return Terminated }

		if idleCounter == 0 { break }
	}
	g.lastFrameDuration = time.Since(startTime)

	return nil
}

func (g *Game) Draw(screen *ebiten.Image) {
	screen.DrawImage(framebuffer.framebuffer, nil)

	/*
	buf := fmt.Sprintf("PC: %08X FP: %08X RP: %08X ESP: %2X\n%v", cpu.PC, cpu.FP, cpu.RP, cpu.ESP, g.lastFrameDuration)
	ebitenutil.DebugPrint(screen, buf)

	screen.Set(g.x, g.y, color.RGBA{255,0,0,0})
	screen.Set(g.x, g.y+1, color.RGBA{0,255,0,0})
	screen.Set(g.x, g.y+2, color.RGBA{0,255,255,0})
	screen.Set(g.x, g.y+3, color.RGBA{255,255,255,0})
	g.x += 1
	if g.x > 319 { g.x = 0 }
	*/

	// if idleCounter == 0 { ebitenutil.DebugPrint(screen, "idle") }
}

func (g *Game) Layout(outsideWidth, outsideHeight int) (screenWidth, screenHeight int) {
	return 640, 400
}

func main() {
	var codefile string = ""

	addrPtr := flag.Int("a",0,"starting address")
	tracePtr := flag.Bool("t",false,"trace")
	cardImgPtr := flag.String("i", "sdcard.img", "SD card image file")
	flag.Parse()
	if len(flag.Args()) > 0 {
		codefile = flag.Args()[0]
	} else {
		codefile = "rommon.prog"
	}

	log.SetFlags(0)
	oldState, err := SetRawConsole()
	if err != nil {
		panic(err)
	}
	defer RestoreConsole(oldState)

	cpu.initialize()
	mem.initialize(4096 * 1024 / 4)

	uart.cpu = &cpu
	mem.attachIO(&uart, 0)

	err = sdspi.openImage(*cardImgPtr)
	if err != nil {
		panic(err)
	}
	defer sdspi.closeImage()
	//sdspi.debug = true
	mem.attachIO(&sdspi, 1)

	framebuffer.initialize()
	mem.attachIO(&framebuffer, 2)

	irqc.initialize()
	mem.attachIO(&irqc, 3)

	cpu.mem = &mem
	cpu.PC = word(*addrPtr)

	if codefile != "" {
		mem.loadFromFile(codefile, *addrPtr)
	}

	consoleChan = make(chan byte)
	uart.consoleChan = make(chan byte)

	ebiten.SetWindowSize(800, 600)
	ebiten.SetWindowTitle("Tridora Framebuffer")

	g := Game{}
	cpu.trace = *tracePtr
	g.stepsPerFrame = 166666
	// g.stepsPerFrame = 1

	go func(ch chan byte) {
		for {
			buf := make([] byte,1);
			n, err := ConsoleRead(buf)
			if err != nil {
				fmt.Println("read error on stdin, closing channel")
				close(ch)
				return
			}
			if n > 0 {ch <- buf[0] }
		}
	}(uart.consoleChan)

	if err := ebiten.RunGame(&g); err != Terminated && err != nil {
		log.Panic(err)
	}
}
