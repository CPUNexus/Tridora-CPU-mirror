	.EQU CR 13
	.EQU LF 10
	.EQU EOT 4
	.EQU ACK 6
	.EQU NAK 21
	.EQU STX 2
	.EQU UART_REG 2048
	.EQU MON_ADDR 64512

	BRANCH 2	; the very first instruction is not
			; executed correctly
	LOADCP 65020 ; initialise FP and RP registers
	STOREREG FP
	LOADCP 65024
	STOREREG RP

	LOADCP MON_ADDR
	LOADCP 4096
	STOREI
	DROP
CMDLOOP0:
	LOADC MESSAGE
	LOADC PRINTLINE
	CALL

CMDLOOP:
	LOADC NEWLINE
	CALL
	LOADC PROMPT
	CALL
CMDLOOP1:
	LOADC CONIN
	CALL
	LOADC TOUPPER
	CALL

	DUP
	LOADC CONOUT
	CALL

	LOADC 'A'
	CMP.S0 EQ
	CBRANCH.Z CMD1
	LOADC CMD_A
	CALL
	BRANCH CMDLOOP2
CMD1:
	LOADC 'X'
	CMP.S0 EQ
	CBRANCH.Z CMD2
	LOADC CMD_X
	CALL
	BRANCH CMDLOOP2
CMD2:
	LOADC 'D'
	CMP.S0 EQ
	CBRANCH.Z CMD3
	LOADC CMD_D
	CALL
	BRANCH CMDLOOP2
CMD3:
	LOADC 'G'
	CMP.S0 EQ
	CBRANCH.Z CMD4
	LOADC CMD_G
	CALL
	BRANCH CMDLOOP2
CMD4:	
	LOADC 'L'
	CMP.S0 EQ
	CBRANCH.Z CMD5
	LOADC CMD_L
	CALL
	BRANCH CMDLOOP2
CMD5:
	LOADC 'B'
	CMP.S0 EQ
	CBRANCH.Z CMD6
	LOADC CMD_B
	CALL
	BRANCH CMDLOOP2
CMD6:
	DROP
	BRANCH CMDLOOP0
CMDLOOP2:
	DROP
	BRANCH CMDLOOP

; ---- Command 'A': set current address
CMD_A:
	LOADC 32
	LOADC CONOUT
	CALL
	LOADC READHEX
	CALL
	CBRANCH.Z CMD_A_INVALID ; 0 if not valid input
	LOADCP MON_ADDR
	SWAP
	STOREI
	DROP	; drop STOREI address
	RET
CMD_A_INVALID:
	DROP
	LOADC '.'
	LOADC CONOUT
	CALL
	RET
	
; ---- Command 'X': examine current address 
CMD_X:
	FPADJ -8	; reserve space for 4 bytes of local variables
	LOADCP MON_ADDR
	LOADI
	STORE 0		; current memory address
	LOADC 4		; print 8 words
	STORE 4		; Loop counter
CMD_X_LOOP:
	LOADC 32	; print a a space
	LOADC CONOUT
	CALL
	LOAD 0		; load word via current address
	LOADI
	LOADC PRINTHEXW	; print it
	CALL
	LOAD 0
	INC  4		; increment current address
	STORE 0
	LOAD 4
	DEC 1
	DUP
	STORE 4
	CBRANCH.NZ CMD_X_LOOP
	LOADCP MON_ADDR
	LOAD 0
	STOREI
	DROP
	FPADJ 8
	RET

; ---- Command 'D': deposit words at current address
CMD_D:
	FPADJ -4
	LOADC 4		; max number of words
	STORE 0
CMD_D_LOOP:
	LOADC 32	; print a space
	LOADC CONOUT
	CALL
	LOADC READHEX
	CALL
	DUP
	CBRANCH.Z CMD_D_EXIT ; check for invalid input
	SWAP	; swap return code and value
	LOADCP MON_ADDR
	LOADI	; get current address
	SWAP	; swap address and value for STOREI
	STOREI 4 ; store the value with post-increment of address
	LOADCP MON_ADDR
	SWAP	; swap destination address and value for STOREI
	STOREI	; store the new address
	DROP
	LOADC 2 ; compare return code (swapped above) to 2
	CMP EQ  ; check for valid input and return key
	CBRANCH CMD_D_EXIT
	LOAD 0
	DEC 1
	DUP
	STORE 0	
	CBRANCH.NZ CMD_D_LOOP
CMD_D_EXIT:
	FPADJ 4
	RET

CMD_G:
	DROP			; remove input char
	LOADCP NEWLINE
	CALL
	LOADCP MON_ADDR
	LOADI
	JUMP

CMD_L:
	LOADCP NEWLINE
	CALL
	LOADCP RCVBLOCKS
	CALL
	LOADCP NEWLINE
	CALL
	RET

PROMPT:
	LOADC '['
	LOADC CONOUT
	CALL
	LOADCP MON_ADDR
	LOADI
	LOADC PRINTHEXW
	CALL
	LOADC PROMPT2
	LOADC PRINTLINE
	CALL
	RET

NEWLINE:
	LOADC CR
	LOADC CONOUT
	CALL
	LOADC LF
	LOADC CONOUT
	CALL
	RET

; print string of byte characters
; takes pointer to string on eval stack
PRINTLINE:
	DUP	; duplicate address as arg to printchar
	LOADC PRINTCHAR
	CALL
	CBRANCH.Z PRINTLINE_EXIT ; if char is zero, exit
	INC 1 ; increment address
	BRANCH PRINTLINE
PRINTLINE_EXIT:
	DROP	; remove address from stack
	RET

; print a single character
; takes a byte pointer on eval stack
; returns character on eval stack
PRINTCHAR:
	LOADI.S1.X2Y	; load word, keep address on stack
	BSEL		; select byte of a word via address
	DUP		; check for null byte
	CBRANCH.Z PRINTCHAR_XT
	DUP
	LOADC CONOUT
	CALL
PRINTCHAR_XT:
	RET

; print a 32-bit hexadecimal number
; takes the value on the stack

PRINTHEXW:
	BROT
	DUP
	LOADC PRINTHEXB
	CALL
	BROT
	DUP
	LOADC PRINTHEXB
	CALL
	BROT
	DUP
	LOADC PRINTHEXB
	CALL
	BROT
	LOADC PRINTHEXB
	CALL
	RET
	
PRINTHEXB:
	DUP
	SHR
	SHR
	SHR
	SHR
	LOADC PRINTNIBBLE
	CALL
	LOADC PRINTNIBBLE
	CALL
	RET
	
PRINTNIBBLE:
	LOADC 15
	AND ; isolate nibble
	LOADC 10
	CMPU.S0 GE ; nibble >= 10 ?
	CBRANCH.NZ PRINTNIBBLE_1 ; then print a-f
	LOADC '0' ; else print 0-9
	BRANCH PRINTNIBBLE_2
PRINTNIBBLE_1:
	LOADC 55 ; 55 + 10 == 'A'
PRINTNIBBLE_2:
	ADD
	LOADC CONOUT
	CALL
	RET

; ------ read a 8-digit hexadecimal number from the console
; stores variables on the user stack, so the FP register must be
; inizialized.
; returns two values on the eval stack:
;   - return code (topmost)
;     0 - no valid number
;     1 - valid number
;     2 - valid number and enter was pressed
;   - result value

READHEX:
	FPADJ -8
	LOADC 0 ; current value
	STORE 0
	LOADC 8 ; max number of digits
	STORE 4 ; remaining digits counter
READHEX_1:
	LOADC CONIN
	CALL
	LOADC CR     ; RETURN pressed?
	CMP.S0 EQ
	CBRANCH READHEX_RT
	DUP
	LOADC CONOUT ; echo character
	CALL
	LOADC CONVHEXDIGIT
	CALL
	LOADC -1
	CMP.S0 EQ  ; invalid character?
	CBRANCH.NZ READHEX_XT
	LOAD 0
	SHL 2	; shift previous nibble
	SHL 2
	OR ; combine with last digit
	STORE 0
	LOAD 4
	DEC 1
	DUP
	STORE 4
	CBRANCH.NZ READHEX_1
	BRANCH READHEX_XT1
READHEX_RT:   ; if no digits were entered, set return code
	DROP  ; drop read character
	LOAD 4  ;remaining digits counter
	LOADC 8
	CMP NE
	CBRANCH READHEX_RT2
	LOADC 0	; no valid input
	BRANCH READHEX_XT3
READHEX_RT2:
	LOADC 2 ; valid input and return pressed
	BRANCH READHEX_XT3	
READHEX_XT:
	DROP
	LOAD 4
	LOADC 8
	CMP EQ  ; if no digits were entered
	CBRANCH READHEX_XT0
READHEX_XT1:
	LOADC 1 ; valid input flag
	BRANCH READHEX_XT3
READHEX_XT0:
	LOADC 0
READHEX_XT3:
	LOAD 0
	SWAP
	FPADJ 8
	RET

; ------ convert character on the eval stack to upper case
TOUPPER:
	LOADC 'a'
	CMP.S0 LT
	CBRANCH TOUPPER_XT
	LOADC 'z'
	CMP.S0 GT
	CBRANCH TOUPPER_XT
	LOADC 32
	SUB
TOUPPER_XT:
	RET

; ------ convert hexadecimal digit to integer
; ------ takes an ascii character as parameter on the eval stack
; ------ returns an integer value from 0-15 on the eval stack,
; ------ or -1 if the character was not a valid hexadecimal digit

CONVHEXDIGIT:
	LOADC TOUPPER
	CALL
	LOADC '0'   
	CMP.S0 LT ; character < '0'?
	CBRANCH.NZ CONVHEXDIGIT_ERR
	LOADC '9' 
	CMP.S0 GT ; character > '9'?
	CBRANCH.NZ CONVHEXDIGIT_ISALPHA
	LOADC '0' ; character is between '0' and '9', subtract '0'
	SUB
	BRANCH CONVHEXDIGIT_NBL
CONVHEXDIGIT_ISALPHA:
	LOADC 'A'   
	CMP.S0 LT ; character < 'A'?
	CBRANCH.NZ CONVHEXDIGIT_ERR
	LOADC 'F' 
	CMP.S0 GT ; character > 'F'?
	CBRANCH.NZ CONVHEXDIGIT_ERR
	LOADC 55 ; character is between 'A' and 'F', subtract ('A' - 10)
	SUB
CONVHEXDIGIT_NBL:
	RET
CONVHEXDIGIT_ERR:
	DROP     ; remove character from stack
	LOADC -1 ; error
	RET

; --------- output a character on serial console
; --------- takes a character (padded to a word) on the eval stack
CONOUT:
	LOADC UART_REG	; address of UART register
	LOADI		; load status
	LOADC 256	; check bit 8 (tx_busy)
	AND
	CBRANCH.NZ CONOUT ; loop if bit 8 is not zero

	; transmitter is idle now, write character
	LOADC UART_REG	; address of UART register 
	SWAP		; swap character and address for STOREI
	LOADC 1024	; TX enable bit
	OR		; OR in the character
	STOREI
	DROP
	RET

; ---- wait until a character is received and return it on eval stack
CONIN:
	LOADC WAITFORBYTE
	CALL
	LOADC -1	; -1 means timeout
	CMP.S0 NE
	CBRANCH CONIN_XT ; exit if no timeout
	DROP		; remove last result
	BRANCH CONIN
CONIN_XT:
	RET


	.EQU L_BLOCKSIZE 32
	.EQU L_WORDSIZE 4
	.EQU CKSUM_PATTERN $AFFECAFE
RCVBLOCKS:
	LOADCP MON_ADDR ; pointer to current write position, 
	LOADI		; kept on stack
RCVBLOCKS_L:
	LOADC WAITFORBYTE ; read header byte
	CALL
	LOADC -1
	CMP.S0 EQ
	CBRANCH RCVBLOCKS_XT ; exit on timeout

	; check for EOT -> end
	LOADC EOT
	CMP.S0 EQ
	CBRANCH RCVBLOCKS_XT

	; check for STX -> read block
	LOADC STX
	CMP.S0 EQ
	CBRANCH RCVBLOCKS_CONT

	; anything else -> send NAK
	BRANCH RCVBLOCKS_RETRY

RCVBLOCKS_CONT:
	DROP	; remove header byte
	DUP	; duplicate pointer
	LOADC READBLOCK
	CALL
	LOADC -1
	CMP.S0 EQ ; check for timeout
	CBRANCH RCVBLOCKS_XT ; exit on timeout

	LOADC -2
	CMP.S0 EQ ; check for checksum error
	CBRANCH RCVBLOCKS_RETRY

	DROP	; remove return code
	LOADC L_BLOCKSIZE ; advance pointer
	ADD

	LOADC ACK	; send ACK
	LOADC CONOUT
	CALL

	; next block
	BRANCH RCVBLOCKS_L

RCVBLOCKS_RETRY:
	DROP ; remove read byte
	; send NAK
	LOADC NAK
	LOADC CONOUT
	CALL

	; next block
	BRANCH RCVBLOCKS_L

RCVBLOCKS_XT:
	DROP ; remove pointer
	DROP ; remove read byte
	RET

	
; ---- read a sequence of binary words and store into memory
; - arguments: pointer to memory area
READBLOCK:
	FPADJ -12
	STORE 0		; buffer pointer
	LOADCP L_BLOCKSIZE
	STORE 4		; remaining bytes
	LOADC 0
	STORE 8		; checksum
READBLOCK_L:
	LOADCP READWORD ; read a word
	CALL
	LOADC -1	; check for timeout
	CMP EQ
	CBRANCH.NZ READBLOCK_ERR
	
	; DUP		; debug
	; LOADC PRINTHEXW
	; CALL


	DUP		; duplicate read word
	LOAD 8		; load checksum
	ADD		; checksum = ((checksum + data) ^ pattern) << 1
	LOADCP CKSUM_PATTERN
	XOR
	SHL
	STORE 8		; store new checkcsum

	LOAD 0		; load buffer pointer
	SWAP		; swap value and pointer for STOREI
	STOREI 4	; store word and increment pointer
	STORE 0		; store pointer

	LOAD 4		; load remaining bytes
	DEC L_WORDSIZE	; decrement by word size
	DUP
	STORE 4		; store 
	CBRANCH.NZ READBLOCK_L  ; loop if remaining words not zero


	LOADCP READWORD ; read checksum
	CALL
	LOADC -1	; check for timeout
	CMP EQ
	CBRANCH READBLOCK_ERR

	LOAD 8		; load checksum
	CMP EQ
	CBRANCH READBLOCK_OK
	LOADC -2	; return code for checksum error
	BRANCH READBLOCK_XT
READBLOCK_OK:
	LOADC 0		; return 0
READBLOCK_XT:
	FPADJ 12
	RET

READBLOCK_ERR:
	DROP		; remove result
	LOAD 4		; return number of missing bytes
	FPADJ 8
	RET

; --- read four bytes (msb to lsb) and return as word
; returns: word, error code (-1 for error, 0 otherwise)

READWORD:
	LOADCP WAITFORBYTE
	CALL
	DUP
	LOADC -1	; check for error
	CMP EQ
	CBRANCH.NZ READWORD_ERR
			; first byte is now on stack
	BROT		; rotate byte left

	LOADCP WAITFORBYTE
	CALL
	DUP
	LOADC -1	; check for error
	CMP EQ
	CBRANCH.NZ READWORD_ERR
			; second byte is now on stack
	OR		; OR last byte with this byte
	BROT		; rotate bytes left

	LOADCP WAITFORBYTE
	CALL
	DUP
	LOADC -1	; check for error
	CMP EQ
	CBRANCH.NZ READWORD_ERR
			; third byte is now on stack
	OR		; OR last byte with this byte
	BROT

	LOADCP WAITFORBYTE
	CALL
	DUP
	LOADC -1	; check for error
	CMP EQ
	CBRANCH.NZ READWORD_ERR
			; fourth byte is now on stack
	OR		; OR last byte with this byte

	LOADC 0		; error code (0: no error)
	RET

READWORD_ERR:
	LOADC -1	; error code
	RET

;---- wait a fixed amount of cycles for a character to be
;     received on the UART.
; returns character or -1 on timeout

	.EQU MAX_WAIT 20000000
WAITFORBYTE:
	LOADCP MAX_WAIT ; maximum wait loops
WAITFORBYTE_L:
	LOADC UART_REG	; address of UART register
	LOADI		; load status
	LOADC 512	; check bit 9 (rx_avail)
	AND
	CBRANCH WAITFORBYTE_RX ; if bit 9 is one, a character is available
	DEC 1
	DUP
	CBRANCH.NZ WAITFORBYTE_L
	DROP		; remove wait counter from stack
	LOADC -1	; error code
	RET
WAITFORBYTE_RX:
	DROP		; remove wait counter from stack
	LOADC UART_REG	
	LOADI		; read register again
	LOADC 255	; mask status bits
	AND
	LOADC UART_REG	; I/O address
	LOADC 512	; set bit 9 (rx_clear)
	STOREI		; write register
	DROP		; remove address left by STOREI
	RET

	.CPOOL

;---- boot from SD-card

	; declare buffer addresses used by sdcardlib.s
	.EQU CSD_BUF 63984
	.EQU CARD_BUF 64000
CMD_B:
	DROP			; remove input char
	LOADCP NEWLINE
	CALL

	FPADJ -4
	; initialize card
	LOADC INITSDCARD
	CALL

	; read partition block
	LOADC 0
	LOADC CARDREADBLK
	CALL

	DUP			; non-zero return code means error
	CBRANCH.Z CMD_B_1
CMD_B_ERR:
	LOADC PRINTHEXW		; print error code
	CALL
	LOADC NEWLINE
	CALL
	LOADC 0			; if we return, we need to
				; put a fake input char back on the
				; stack because the main loop will
				; try to remove it
	FPADJ 4
	RET

CMD_B_1:
	DROP			; remove error code
	; check boot partition slot (boot flag)
	LOADCP CARD_BUF,104	; offset partition flags second part slot
	LOADI
	LOADC 2			; PartFlags [PartBoot]
	CMP EQ
	CBRANCH CMD_B_C

	; no boot partition
	LOADC $B0
	BRANCH CMD_B_ERR

CMD_B_C:
	; get start block
	LOADCP CARD_BUF,108	; offset startBlock
	LOADI
	; get block count
	LOADCP CARD_BUF,124	; offset bootBlocks
	LOADI

	FPADJ -4		; allocate space for address var
	LOADCP MON_ADDR
	LOADI
	STORE 0			; initialize dest addr
CMD_B_L:
	; read block
	OVER			; duplicate block no
	LOADC CARDREADBLK
	CALL

	DUP			; check for error
	CBRANCH.Z CMD_B_C2	; continue if zero (no error)

	NIP			; remove start and count, keep error code
	NIP
	BRANCH CMD_B_ERR
CMD_B_C2: DROP			; remove error code
CMD_B_2:
	; copy to destination
	LOAD 0	; dest addr
	LOADC COPY_BLK
	CALL

	; decrement count and loop
	LOAD 0		; increment dest addr
	LOADC 512
	ADD
	STORE 0

	SWAP		; swap block no/count, blockno is now ToS
	INC 1
	SWAP		; count is now ToS
	DEC 1
	DUP
	CBRANCH.NZ CMD_B_L	; if not zero, loop

	; jump to coreloader
	DROP
	DROP
	FPADJ 4
	LOADCP MON_ADDR
	LOADI
	JUMP

CMD_B_XT:
	FPADJ 4
	RET

; copy a sdcard block to destination address
; block size is always 512 byte, source
; is always CARD_BUF
; parameters: dest addr
COPY_BLK:
	FPADJ -4
	LOADC 128	; word count
	STORE 0

	LOADCP CARD_BUF ; src addr
COPY_BLK1:
	SWAP
			; [ src addr, dest addr ]
	OVER		; [ saddr, daddr, saddr ]
	LOADI		; [ saddr, daddr, sword ]
	STOREI 4	; [ saddr, daddr + 4 ]
	SWAP		; [ daddr + 4, saddr ]
	INC 4		; [ daddr + 4, saddr + 4]

	LOAD 0		; load and decrement counter
	DEC 1
	DUP
	STORE 0		; store it again
	CBRANCH.NZ COPY_BLK1 ; if not zero, loop

	DROP		; remove saddr and daddr
	DROP

	FPADJ 4
	RET
	
	.CPOOL

; wait approx. 1 millisecond
;
; 83.333 MHz Clock, three instructions a 4 cycles
; 83333 / 12 = 6944.4166
; works only if executed without wait states (i.e.
; from BRAM/SRAM)
WAIT1MSEC:
	LOADCP 6944
WAIT1LOOP:
	DEC 1
	DUP
	CBRANCH.NZ WAIT1LOOP
	DROP
	RET

%include "sdcardboot.s"
	.CPOOL
MESSAGE:
	.BYTE 13,10,"ROM Monitor v3.0.3", 13, 10,
	      "Set A)ddress   D)eposit   eX)amine   L)oad   G)o  B)oot",13,10,0
PROMPT2:
	.BYTE "]> ",0
END:
