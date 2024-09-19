// +build windows
// Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details

package main

import (
	"io"
	"os"
	"golang.org/x/sys/windows"
)

type ConsoleState struct {
	modeStdin uint32
	modeStdout uint32
}

func SetRawConsole() (*ConsoleState, error) {
	var stIn uint32
	var stOut uint32

	stdinFd := os.Stdin.Fd()

	if err := windows.GetConsoleMode(windows.Handle(stdinFd), &stIn); err != nil {
		return nil, err
	}
	raw := stIn &^ (windows.ENABLE_ECHO_INPUT | windows.ENABLE_PROCESSED_INPUT | windows.ENABLE_LINE_INPUT | windows.ENABLE_PROCESSED_OUTPUT)
	raw |= windows.ENABLE_VIRTUAL_TERMINAL_INPUT
	if err := windows.SetConsoleMode(windows.Handle(stdinFd), raw); err != nil {
		return nil, err
	}

/*
	stdoutFd := os.Stdout.Fd()

	if err := windows.GetConsoleMode(windows.Handle(stdoutFd), &stOut); err != nil {
		return nil, err
	}
	raw = stOut | windows.ENABLE_VIRTUAL_TERMINAL_INPUT | windows.ENABLE_PROCESSED_OUTPUT
	if err := windows.SetConsoleMode(windows.Handle(stdoutFd), raw); err != nil {
		return nil, err
	}
*/
	return &ConsoleState{stIn,stOut}, nil
}

func RestoreConsole(st *ConsoleState) error {
	stdinFd := os.Stdin.Fd()
	stdoutFd := os.Stdout.Fd()

	err := windows.SetConsoleMode(windows.Handle(stdinFd), st.modeStdin)
	if err != nil { return err }
	err = windows.SetConsoleMode(windows.Handle(stdoutFd), st.modeStdin)
	return err
}

func ConsoleRead(buf []byte) (count int, err error) {
	n, e := os.Stdin.Read(buf)
	if e == io.EOF {	// ugly hack to handle ^Z on windows
			// this can probably be done in a better way
			// but tbh I am glad it works and I don't
			// have to dig deeper into that windows
			// console i/o crap
		n = 1; buf[0] = 26
		return n, nil
	}
	return n, e
}

func ConsoleWrite(char byte) (err error) {
	buf := make([] byte, 1)
	buf[0] = char
	_ , err = os.Stdout.Write(buf)
	return err
}

