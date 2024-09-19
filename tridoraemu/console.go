// +build !windows
// Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details

package main

import (
	"os"
	"golang.org/x/term"
)

type ConsoleState struct {
	state term.State
}

func SetRawConsole() (*ConsoleState, error) {
	oldState, err := term.MakeRaw(int(os.Stdin.Fd()))

	return &ConsoleState{*oldState}, err
}

func RestoreConsole(st *ConsoleState) error {
	return term.Restore(int(os.Stdin.Fd()), &st.state)
}

func ConsoleRead(buf []byte) (count int, err error) {
	n, e := os.Stdin.Read(buf)
	return n, e
}

func ConsoleWrite(char byte) (err error) {
	buf := make([] byte, 1)
	buf[0] = char
	_ , e := os.Stdout.Write(buf)
	return e
}
