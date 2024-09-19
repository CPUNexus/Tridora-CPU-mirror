// Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details
package main
import (
//	"fmt"
	"os"
	"errors"
	)

type UART struct {
	available bool
	buf [] byte
	consoleChan chan byte
	cpu *CPU
}

func (u *UART) read(byteaddr word) (word, error) {
	var result word = 0

	if len(u.buf) > 0 {
		result = word(u.buf[0])
		result |= 512
	} else {
		select {
		case inbyte, ok := <-u.consoleChan:
			if ! ok {
				return 0, errors.New("console channel error")
			} else {
				u.buf = make([]byte, 1)
				u.buf[0] = inbyte
				// fmt.Println("Read input:", inbyte)
				idle(false)
			}
		default:
			idle(true)
		}
	}

	return result, nil
}

func (u *UART) write(value word, byteaddr word) (error) {
	var err error = nil

	idle(false)

	if value & 512 != 0 {
		u.buf = u.buf[1:]
		// fmt.Println("rx_clear: len ", len(u.buf))
	}

	if value & 1024 != 0 {
		buf := make([] byte, 1)
		buf[0] = byte(value & 255)
		_ , err = os.Stdout.Write(buf)
	}

	return err
}

