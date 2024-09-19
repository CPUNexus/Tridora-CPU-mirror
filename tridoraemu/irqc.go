// Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details
package main
import (
	"time"
	// "fmt"
)

const MSecsPerTick = 50

type IRQC struct {
	start time.Time
}

func (i *IRQC) initialize() {
	i.start = time.Now()
}

func (i *IRQC) read(byteaddr word) (word, error) {
	elapsedms := time.Since(i.start).Milliseconds()
	elapsedTicks := elapsedms / MSecsPerTick
	result := word((elapsedTicks & 0x0FFFFFFF) << 8)
	//fmt.Printf("** IRQC read: %08X (%v)\n", result, elapsedms)
	return result, nil
}

func (i *IRQC) write(value word, byteaddr word) (error) {
	return nil
}

