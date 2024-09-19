// Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details
package main

import (
	"os"
	"io"
	"encoding/binary"
	"fmt"
)

type SDState uint

const (
	IDLE	SDState = iota
	WRCMD
	WRDATA
	RDDATA
)

const (
	CTRL_WRITE =    0b100000000000000
	RX_FILTER_EN =  0b010000000000000
	TXRX_EN =       0b001000000000000
	CLK_F_EN =      0b000100000000000
	CLK_DIV_WR =    0b000010000000000
	RX_RD =         0b000001000000000
	TX_WR =         0b000000100000000

	C_D =           0b100000000000000
	C_CHG =         0b010000000000000
	C_BUSY =        0b001000000000000
	TX_RDY =        0b000100000000000
	TX_EMPTY =      0b000010000000000
	RX_AVAIL =      0b000001000000000
	RX_OVR =        0b000000100000000
)

type SDSPI struct {
	state SDState
	ksectors uint
	lastSector uint
	imgfile *os.File
	cmd uint
	cmdcount uint
	arg uint
	receiving bool
	blockaddr uint
	readbuf []byte
	readpos int
	writebuf []byte
	writepos int
	debug bool
	dbgwaiten bool
}

func (s *SDSPI) openImage(filename string) error {
	var err error
	s.imgfile, err = os.OpenFile(filename, os.O_RDWR, 0644)
	if err != nil { return err }

	buf := make([]byte,4)
	_, err = s.imgfile.ReadAt(buf, 48)
	if err != nil { return err }

	blocks := binary.BigEndian.Uint32(buf)

	s.ksectors = uint(blocks / 1024)
	s.lastSector = uint(blocks - 1)

	fmt.Printf("opened SD card image %v, PHYS blocks: %v\n", filename, blocks)

	return nil
}

func (s *SDSPI) closeImage() {
	s.imgfile.Close()
}

func (s *SDSPI) read(byteaddr word) (word, error) {
	result := word(0)

	// always detect a card, transmitter always ready
	result = C_D | TX_RDY

	if s.debug {
		fmt.Printf("** SDSPI read readbuf len: %v receiving: %v readpos: %v\n",
			len(s.readbuf), s.receiving, s.readpos)
	}

	if s.receiving && len(s.readbuf) > 0 {
		if s.debug { fmt.Printf("  byte: %02X\n", s.readbuf[s.readpos]) }
		result |= RX_AVAIL  // there is data to be read
		result |= word(s.readbuf[s.readpos])
		// the read position is advanced only by writing RX_RD to the
                // SDSPI register

	} else {
		result |= 0xFF
	}


	// always signal TX_EMPTY since we immediately process
	// all written data
	result |= TX_EMPTY

	return result, nil
}

func (s *SDSPI) sendIdleResponse() {
	s.readbuf = []byte{0x01}
}

func (s *SDSPI) sendOkResponse() {
	s.readbuf = []byte{0x00}
}

func (s *SDSPI) sendDataResponse() {
	s.readbuf = []byte{0b00101}
}

func (s *SDSPI) sendBusy() {
	s.readbuf = append(s.readbuf, 0xFF, 0x00, 0xFF)
}

func (s *SDSPI) sendDataPkt(dataBytes []byte) {
	s.readbuf = append(s.readbuf, 0xFE)	// data token
	s.readbuf = append(s.readbuf, dataBytes...) // data block
	s.readbuf = append(s.readbuf, 0, 0)    // crc/unused in SPI mode
}

func (s *SDSPI) sendCSD() {
	size := s.ksectors - 1

        sizehi  := byte((size & 0b1111110000000000000000) >> 16)
        sizemid := byte((size & 0b0000001111111100000000) >> 8)
        sizelow := byte((size & 0b0000000000000011111111))

	s.sendDataPkt( []byte{0b01000000,
                0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6,
                sizehi, sizemid, sizelow,
                0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF})
}

func (s *SDSPI) readSendBlock() {
	buf := make([]byte, 512)

	if s.arg <= s.lastSector {
		s.imgfile.Seek(int64(s.arg) * 512, 0)
		_, err := s.imgfile.Read(buf)
		if err != nil && err != io.EOF { panic(err) }
	}

	s.sendDataPkt(buf)
}

func (s *SDSPI) writeBlock() {
	if s.arg <= s.lastSector {
		s.imgfile.Seek(int64(s.arg) * 512, 0)
		_, err := s.imgfile.Write(s.writebuf)
		if err != nil { panic(err) }
	}
	s.writebuf = make([]byte, 0)
	s.writepos = 0
}

func (s *SDSPI) write(value word, byteaddr word) (error) {
	if (value & CTRL_WRITE) != 0 {
		s.receiving = (value & TXRX_EN) != 0
	}

	if s.debug { fmt.Printf("** SDSPI write %032b\n", value) }

	if (value & CLK_DIV_WR) != 0 {
		if s.debug {
			fmt.Printf("** SDSPI clock divider set to %v\n", value & 0xFF)
		}
	}

	if (value & RX_RD) != 0 {
		// advance read position when RX_RD i set
		s.readpos += 1
		if s.readpos >= len(s.readbuf) {
			s.readbuf = make([]byte, 0)
			s.readpos = 0
			// if in WRDATA state, do not go IDLE when all data has been read.
			// In that case, we just read the R1 response for the write command
			// and after that the data packet will be written.
			if s.state != WRDATA { s.state = IDLE }
		}
	}

	if (value & TX_WR) != 0 {
		// we ignore the TXRX_EN flag for the transmitter and
		// always process data written with TX_WR
		value8 := value & 0xFF
		switch s.state {
		case IDLE:
			if value8 != 0xFF {
				s.state = WRCMD
				s.cmd = uint(value & 0x3F)
				s.arg = 0
				s.cmdcount = 5
				if s.debug {
					fmt.Printf("  cmd:  %02d\n", s.cmd)
				}
			}
		case WRCMD:
			if s.cmdcount > 0 {	// any more argument bytes to be received?
				s.cmdcount -= 1
				if s.cmdcount == 0 {
					s.state = RDDATA
					switch s.cmd {
					case  0: s.sendIdleResponse()		// GO_IDLE_STATE
					case  8: s.readbuf = []byte{0x01, 0xA1, 0xA2, 0xA3, 0xA4} // SEND_IF_COND
					case  9: s.sendOkResponse()		// SEND_CSD
						 s.sendCSD()
					case 16: s.sendOkResponse()		// SET_BLOCKLEN, ignored
					case 17: s.sendOkResponse()		// READ_SINGLE_BLOCK
						 s.readSendBlock()
					case 24: s.sendOkResponse()		// WRITE_SINGLE_BLOCK
						 s.sendOkResponse()
						 s.state = WRDATA
					case 58: s.readbuf = []byte{0x01, 0xB1, 0xB2, 0xB3, 0xB4}  // READ_OCR
					case 55: s.sendIdleResponse()		// APP_CMD, we just ignore it and treat CMD41 as ACMD41
					case 41: s.sendOkResponse()		// APP_SEND_OP_COND
					default:
						if s.debug {
							fmt.Printf("** SDSPI invalid CMD %v\n", s.cmd)
						}
					}
				} else {
					// process an argument byte
					s.arg = uint((s.arg << 8)) | uint(value8)
				}
			} else {
				if s.debug {
					fmt.Printf("** SDSPI extra bytes in command %v\n", value8)
				}
			}
		case WRDATA:
			if len(s.writebuf) == 0 {
				// wait for data token
				if value8 == 0xFE {	// data token found
					s.writebuf = make([]byte, 512)
					s.writepos = 0
				}
			} else { // collecting data bytes to write
				if s.writepos < 512 {
					s.writebuf[s.writepos] = byte(value8)
				}
				s.writepos += 1
				// after getting and ignoring two crc bytes, write block
				// and return to idle state
				if s.writepos >= 514 {
					s.state = IDLE
					s.writeBlock()
					s.sendDataResponse()
					s.sendBusy()
				}
			}
		default:
			if value8 != 0xFF {
				if s.debug {
					fmt.Printf("** SDSPI invalid state %v on TX_WR byte %v\n", s.state, value8)
				}
			}
		}
	}

	return nil
}

