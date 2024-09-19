// Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details
package main

type IOHandler interface {
	read(byteaddr word) (word, error)
	write(value word, byteaddr word) (error)
}
