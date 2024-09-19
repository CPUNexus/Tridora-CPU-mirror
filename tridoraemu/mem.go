// Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details
package main
import (
	"fmt"
	"log"
	"os"
	"io"
	"bufio"
	"encoding/binary"
	)

const IOStartAddr = 2048

const RAMStartAddr = 4096

const IOSlotSize = 128

const IOSlotCount = 16

type Mem struct {
	ram [] word
	iohandler [IOSlotCount] IOHandler
}

func (m *Mem) wordAddr(byteaddr word) (int, error) {
	wordaddr := int(byteaddr / 4)
	if wordaddr >= len(m.ram) {
		return 0, fmt.Errorf("Invalid address %08X", byteaddr)
	}
	return wordaddr, nil
}

func (m *Mem) initialize(sizewords int) {
	m.ram = make([] word, sizewords)
	for i := 0; i < len(m.ram);  i++ {
		m.ram[i] = 0
	}
}

func (m *Mem) loadFromFile(path string, startAddr int) {
	f, err := os.Open(path)
	if err != nil {
		panic(err)
	}
	defer f.Close()

	buf := make([]byte,4)

	reader := bufio.NewReader(f)
	count := 0
	wordaddr := startAddr / 4
	for {
		n, e := reader.Read(buf)
		if e != nil && e != io.EOF {
			panic(e)
		}
		if n < 4 {
			if n == 2 {
				m.ram[wordaddr] = word(binary.BigEndian.Uint32(buf) & 0xFFFF0000)
				count += 2
			}

			fmt.Printf("%v bytes read at %08X from %v\n", count, startAddr, path)
			break
		} else {
			m.ram[wordaddr] = word(binary.BigEndian.Uint32(buf))
			// fmt.Printf("%08X %08X\n", addr, m.ram[addr])
			count += 4
			wordaddr += 1
		}
	}
}

func (m *Mem) attachIO(h IOHandler, slot int) {
	if m.iohandler[slot] != nil {
		log.Panicf("I/O handler %d already attached", slot)
	}

	m.iohandler[slot] = h
}

func (m *Mem) read(byteaddr word) (word, error) {
	if byteaddr >= IOStartAddr && byteaddr < RAMStartAddr {
		ioslot := (byteaddr - IOStartAddr) / IOSlotSize
		if m.iohandler[ioslot] != nil {
			return m.iohandler[ioslot].read(byteaddr)
		}
		return 42, nil
	}

	wordaddr, err := m.wordAddr(byteaddr)
	if err == nil {
		return m.ram[wordaddr], err
	} else {
		return 0, err
	}
}

func (m *Mem) write(value word, byteaddr word) error {
	if byteaddr < IOStartAddr {
		return fmt.Errorf("Write to ROM area at %08X value %08X", byteaddr, value)
	}

	if byteaddr >= IOStartAddr && byteaddr < RAMStartAddr {
		ioslot := (byteaddr - IOStartAddr) / IOSlotSize
		if m.iohandler[ioslot] != nil {
			return m.iohandler[ioslot].write(value, byteaddr)
		}
		return nil
	}

	wordaddr, err := m.wordAddr(byteaddr)
	if err == nil {
		m.ram[wordaddr] = value
	}
	return err
}
