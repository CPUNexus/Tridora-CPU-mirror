	.EQU SPIREG $880

	.EQU SPI_CTRL_WRITE	%100000000000000
	.EQU SPI_RX_FILTER_EN	%010000000000000
	.EQU SPI_TXRX_EN	%001000000000000
	.EQU SPI_CLK_F_EN	%000100000000000
	.EQU SPI_CLK_DIV_WR	%000010000000000
	.EQU SPI_RX_RD		%000001000000000
	.EQU SPI_TX_WR		%000000100000000

	.EQU SPI_C_D		%100000000000000
	.EQU SPI_C_CHG		%010000000000000
	.EQU SPDI_C_BUSY	%001000000000000
	.EQU SPI_TX_RDY		%000100000000000
	.EQU SPI_TX_EMPTY	%000010000000000
	.EQU SPI_RX_AVAIL	%000001000000000
	.EQU SPI_RX_OVR		%000000100000000

	.EQU SPI_TXRX_EN_MASK	~SPI_TXRX_EN

_WAIT:
	LOADC 10
_WAITL:
	LOADC WAIT1MSEC
	CALL
	DEC 1
	DUP
	CBRANCH.NZ _WAITL
	DROP
	RET

INITSDCARD:
	LOADC WAIT1MSEC
	CALL

	LOADC _SPIINIT1
	CALL

	;LOADC 'I'
	;LOADCP CONOUT
	;CALL

	LOADC _WAITSPITXRDY
	CALL

	;LOADC 'W'
	;LOADCP CONOUT
	;CALL

	; send RESET CARD command
	LOADC $95	; send cmd0 with arg 0 and checksum $95
	LOADC $0
	LOADC $0
	LOADC SENDCMD_R1
	CALL

	DROP	; TODO: handle errors
	;LOADCP PRINTHEXW ; print status returned by the card
	;CALL
	;LOADCP NEWLINE
	;CALL

	;LOADC '9'
	;LOADCP CONOUT
	;CALL

	LOADC _WAITSPITXRDY
	CALL

	LOADC _WAIT
	CALL

	;LOADC '1'
	;LOADCP CONOUT
	;CALL

	LOADC $87
	LOADC $01AA
	LOADC $8
	LOADC SENDCMD_R7
	CALL

	DROP

	LOADC _WAITSPITXRDY
	CALL

	;LOADC '2'
	;LOADCP CONOUT
	;CALL

	;LOADCP _WAIT
	;CALL

	;LOADC '.'
	;LOADCP CONOUT
	;CALL

	LOADC CARDINITV2
	CALL

	;LOADC '+'
	;LOADCP CONOUT
	;CALL


	LOADC CARDFASTCLK
	CALL

	;LOADC '3'
	;LOADCP CONOUT
	;CALL

	; CMD16: set block size to 512 byte
	LOADC 0
	LOADC 512
	LOADC 16
	LOADCP SENDCMD_R1
	CALL

	DROP

	;LOADCP _WAIT
	;CALL

	;LOADC '4'
	;LOADCP CONOUT
	;CALL

	RET

; read a 512-byte-block from the card
; args: block number
; returns: 0 on success
CARDREADBLK:
	LOADC 128 ; number of words in a block
	SWAP	; move block number up the stack
	LOADCP CARD_BUF
	SWAP
	LOADC 0
	SWAP
	LOADC 17	; CMD17: read block
	LOADC SENDCMD_PKT
	CALL
	RET

; send the card initialization command
; wait until the card responds
CARDINITV2:
	LOADC 100 ; try up to 100 times
CARD_LOOP1:
	LOADC 50 ; wait 50 msec
CARD_LOOP2:
	LOADC WAIT1MSEC
	CALL
	DEC 1
	DUP
	CBRANCH.NZ CARD_LOOP2
	DROP	; remove loop count value

	LOADC $0
	LOADC $0
	LOADC 58
	LOADC SENDCMD_R7 ; send CMD58
	CALL
	DROP		; ignore result (why?)

	LOADC $0
	LOADCP $40000000
	LOADC 41
	LOADC SENDACMD_R1 ; send ACMD41
	CALL

	CBRANCH.Z CARD_OK	; if result is zero, the command succeded
				; and the card initialization is finished
	DEC 1
	DUP
	CBRANCH.NZ CARD_LOOP1
	DROP	; remove outer loop count value

	RET
CARD_OK:
	DROP	; remove outer loop count value

	; CMD16: set block size to 512 byte
	LOADC 0
	LOADC 512
	LOADC 16
	LOADC SENDCMD_R1
	CALL
	DROP ; ignore return value

	RET

; set fast transfer rate
CARDFASTCLK:
	LOADC SPIREG
	; set clock divider to ~2,6MHz
	LOADC SPI_CLK_DIV_WR+10
	STOREI
	DROP
	RET

; perform first phase of card initialization
; which is to enable clock and wait a bit
; leaves the clock running
_SPIINIT1:
	LOADC SPIREG
	; set clock divider to ~325KHz
	LOADC SPI_CLK_DIV_WR+64
	STOREI
	DROP

	; clear all flags + enable clock
	; /CS and MOSI are default high
	LOADC SPIREG
	LOADCP SPI_CTRL_WRITE,SPI_CLK_F_EN
	STOREI
	DROP

	; we should wait at least for 74 clock cycles now
	LOADC 2	; wait 2 msec, that should be ~300 cycles
_SPIINIT1L:
	LOADC WAIT1MSEC
	CALL
	DEC 1
	DUP
	CBRANCH.NZ _SPIINIT1L
	DROP

	LOADC SPIREG
	LOADCP SPI_CTRL_WRITE ; disable clock
	STOREI
	DROP

	LOADCP WAIT1MSEC
	CALL

	RET

; wait for transmission to finish
; (wait for TX_EMPTY bit)
_SPIWAITTX:
	LOADC SPIREG
	LOADI
	LOADCP SPI_TX_EMPTY
	AND
	CBRANCH.Z _SPIWAITTX
	RET

; finalize a command that has been sent:
; wait until the transmitter is idle
; then disable clock and set MOSI high
_SPIENDCMD:
	LOADC $FF
	LOADC _SENDBYTE
	CALL
	LOADC $FF
	LOADC _SENDBYTE
	CALL

	;LOADC 'E'
	;LOADCP CONOUT
	;CALL

	LOADC _SPIWAITTX
	CALL

	;LOADC 'w'
	;LOADCP CONOUT
	;CALL
	LOADC _WAIT_S	; wait a short time
	CALL

	LOADC SPIREG
	LOADCP SPI_IDLE_FLAGS ; turn off transceiver
	LOADI
	STOREI
	DROP

	; wait for a few instructions
	LOADC 100
SPIEND_LP: DEC 1
	DUP
	CBRANCH.NZ SPIEND_LP
	DROP

	RET

_WAIT_S:
	LOADC 100
_WAIT_S_L:
	DEC 1
	DUP
	CBRANCH.NZ _WAIT_S_L
	DROP
	RET

; clear RX fifo
CLEAR_RX_FIFO:
CLEAR_RX_L1:
	LOADC SPIREG
	LOADI

	;DUP
	;LOADCP PRINTHEXW
	;CALL
	;LOADCP NEWLINE
	;CALL

	LOADC SPI_RX_AVAIL
	AND
	CBRANCH.Z CLEAR_RX_X
	LOADC SPIREG
	LOADC SPI_RX_RD
	STOREI
	DROP

	; FIXME: it seems that this
	; does not remove a byte from the fifo,
	; rx_avail stays on, but only after the first
	; byte has been received and read

	;LOADC 'x'
	;LOADCP CONOUT
	;CALL

	BRANCH CLEAR_RX_L1
CLEAR_RX_X:
	RET

_WAITSPITXRDY:
	LOADC SPIREG
	LOADI
	LOADCP SPI_TX_RDY
	AND
	CBRANCH.Z _WAITSPITXRDY
	RET

; send a command and receive a data packet response
; args: packet size in words, buffer pointer
;	checksum byte, 32-bit cmd arg, cmd number
; returns: 0 on success
SENDCMD_PKT:
	; first send the command
	LOADC SENDCMD_0
	CALL

	LOADC _RCVBYTE ; receive R1 response
	CALL

	CBRANCH.NZ SENDCMD_PKT_E ; on success we get 0

	; now wait for data token
SENDCMD_PKT_L:
	LOADC _RCVBYTE
	CALL
	LOADC $FF
	CMP EQ
	CBRANCH SENDCMD_PKT_L

	; parameters for _RCVWORDS are on the stack now
	LOADC _RCVWORDS
	CALL

	; receive 2 crc bytes
	LOADC _RCVBYTE
	CALL
	BROT
	LOADC _RCVBYTE
	CALL
	OR

	; terminate command
	LOADC _SPIENDCMD
	CALL

	DROP	; we ignore the checksum for now

	LOADC 0
	RET
SENDCMD_PKT_E:
	DROP ; remove remaining args
	DROP
	LOADC -1 ; return code for error
	RET

; send a command and receive a 1-byte-response (R1)
; args: checksum byte, 32-bit cmd arg, cmd number
; returns: received byte
SENDCMD_R1:
	LOADC SENDCMD_0
	CALL

	LOADC _RCVBYTE
	CALL

	;LOADC 'R'
	;LOADCP CONOUT
	;CALL

	;terminate command (/cs high, disable clock)
	LOADC _SPIENDCMD
	CALL

	RET

; send a command
; args: checksum byte, 32-bit cmd arg, cmd number
SENDCMD_0:
	; clear RX FIFO first
	LOADC CLEAR_RX_FIFO
	CALL

	;LOADC '>'
	;LOADCP CONOUT
	;CALL

	; cmd byte is at TOS at this point
	LOADC $40 ; or in start of frame bit
	OR
	LOADC _SENDBYTE
	CALL
	; cmd arg is at TOS now
	LOADC _SENDWORD
	CALL
	; checksum byte is at TOS now
	LOADC _SENDBYTE
	CALL

	LOADC _XCVR_ENABLE	; enable transceiver last,
	CALL			; a complete command should
	RET			; fit into the tx fifo

; send ACMD and receive a 1-byte-response (R1)
; args: checksum byte, 32-bit cmd arg, ACMD number
; returns: received byte or -1 if first response byte
;	indicated an error
SENDACMD_R1:
	LOADC $0
	LOADC $0
	LOADC 55	; send CMD55
	LOADC SENDCMD_R1
	CALL

	LOADC 1 ; 1 = idle state, no errors
	CMP NE
	CBRANCH.NZ SENDACMD_ERR

	; pass our args to SENDCMD_R1
	LOADC SENDCMD_R1
	CALL
	RET

SENDACMD_ERR:
	LOADCP -1
	RET

; send a command and receive a 4+1-byte-response (R7)
; args: checksum byte, 32-bit cmd arg, cmd number
; returns: received word or -1 if first response byte
;	indicated an error

SENDCMD_R7:
	; send the command
	LOADC SENDCMD_0
	CALL

	;LOADC '7'
	;LOADCP CONOUT
	;CALL

	LOADC _RCVBYTE
	CALL

	LOADC _RCVWORD
	CALL

	;terminate command (/cs high, disable clock)
	LOADC _SPIENDCMD
	CALL

	SWAP	; swap 1st response byte with received word
	LOADC %011111110 ; check for any error flags
	AND
	CBRANCH.Z SENDCMD_R7_NOERR
	DROP
	LOADC -1
SENDCMD_R7_NOERR:
	RET

; send a word as 4 bytes, msb first
_SENDWORD:
	DUP	; remember original value for later

	BROT	; rotate msb to lsb (byte 0)
	LOADC 255
	AND.S0	; isolate byte, keep previous value
	LOADC _SENDBYTE
	CALL

	BROT	; byte 1
	LOADC 255
	AND.S0	
	LOADC _SENDBYTE
	CALL

	BROT    ; byte 2
	LOADC 255
	AND
	LOADC _SENDBYTE
	CALL

	; byte 3 is already on the stack
	LOADC 255 
	AND	  
	LOADC _SENDBYTE
	CALL

	RET

; receive multiple 4-byte-words and store into
; memory buffer
; args: number of words, pointer to buffer
_RCVWORDS:
	FPADJ -4
	STORE 0	; store pointer arg into local variable
		; keep counter on stack
_RCVWORDS_LP:
	LOAD 0	; load buf pointer for STOREI
	LOADC _RCVWORD
	CALL	; receive a word
	STOREI 4 ; store to buf with postincrement
	STORE 0  ; store pointer variable

	DEC 1	; decrement word counter
	DUP
	CBRANCH.NZ _RCVWORDS_LP ; if not null, loop
	DROP	; remove counter value

	FPADJ 4
	RET

; receive 4 bytes, return as word
_RCVWORD:
	LOADC _RCVBYTE ; receive first byte
	CALL
	BROT		; rotate byte to left

	LOADC _RCVBYTE	; receive second byte
	CALL
	OR		; or first and second byte together
	BROT		; rotate 1st + 2nd to left

	LOADC _RCVBYTE	; receive third byte
	CALL
	OR
	BROT

	LOADCP _RCVBYTE	; receive fourth byte
	CALL
	OR
	RET

_XCVR_ENABLE:
	LOADC SPIREG
	LOADC SPI_TX_FLAGS
	LOADI
	STOREI
	DROP
	RET

; send a byte
; args: byte to be sent
_SENDBYTE:
	LOADC SPIREG
	LOADI	; load spi io register
	LOADCP SPI_TX_RDY
	AND	; check tx_rdy bit
	CBRANCH.Z _SENDBYTE ; if not set, loop
	
	LOADC SPI_TX_WR ; TX_WR bit
	OR	; OR in byte to be send

	LOADC SPIREG
	SWAP    ; swap value and addr for STOREI
	STOREI	; store word (flags + data) to io register
	DROP	; remove STOREI result

	RET

; receive a byte. receiver must be enabled.
; returns: received byte
_RCVBYTE:
	LOADC SPIREG
	LOADI	; load spi io register
	LOADC SPI_RX_AVAIL
	AND.S0 ; check rx_avail bit, keep original value
	CBRANCH.NZ RECVGOTIT
	DROP	; rx_avail not set, remove register value and loop
	BRANCH _RCVBYTE
RECVGOTIT:
	LOADC SPIREG
	LOADC SPI_RX_RD ; remove one byte from rx fifo
	STOREI
	DROP

	LOADC 255
	AND	  ; keep bits 7-0
	RET

SPI_TX_FLAGS: .WORD SPI_CTRL_WRITE + SPI_TXRX_EN + SPI_RX_FILTER_EN
SPI_IDLE_FLAGS: .WORD SPI_CTRL_WRITE
