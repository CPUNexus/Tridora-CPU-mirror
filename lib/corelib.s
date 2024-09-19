; Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details

	.EQU CR 13
	.EQU LF 10
	.EQU UART_REG 2048
	.EQU IRQC_REG 2432

	.EQU PROG_START 24576
	.EQU FP_START	24060
	.EQU RP_START   24064
NEWLINE:
	LOADC CR
	LOADCP CONOUT
	CALL
	LOADC LF
	LOADCP CONOUT
	CALL
	RET

; print string of byte characters
; takes pointer to string on eval stack
PRINTLINE:
	DUP	; duplicate address as arg to printchar
	LOADCP _PRINTCHAR
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
_PRINTCHAR:
	LOADI.S1.X2Y	; load word, keep address on stack
	BSEL		; select byte of a word via address
	DUP		; check for null byte
	CBRANCH.Z _PRINTCHAR_XT
	DUP
	LOADCP CONOUT
	CALL
_PRINTCHAR_XT:
	RET

; print a 32-bit hexadecimal number
; takes the value on the stack
PRINTHEXW:
	BROT
	DUP
	LOADCP _PRINTHEXB
	CALL
	BROT
	DUP
	LOADCP _PRINTHEXB
	CALL
	BROT
	DUP
	LOADCP _PRINTHEXB
	CALL
	BROT
	LOADCP _PRINTHEXB
	CALL
	RET

_PRINTHEXB:
	DUP
	SHR
	SHR
	SHR
	SHR
	LOADCP _PRINTNIBBLE
	CALL
	LOADCP _PRINTNIBBLE
	CALL
	RET

_PRINTNIBBLE:
	LOADC 15  ; isolate nibble
	AND
	LOADC 10
	CMPU.S0 GE ; nibble >= 10 ?
	CBRANCH.NZ _PRINTNIBBLE_1 ; then print a-f
	LOADC '0' ; else print 0-9
	BRANCH _PRINTNIBBLE_2
_PRINTNIBBLE_1:
	LOADC 55 ; 55 + 10 == 'A'
_PRINTNIBBLE_2:
	ADD
	LOADCP CONOUT
	CALL
	RET

_MUL10:
	SHL	; x * 2
	DUP
	SHL 2 ; x * 8
	ADD ; x * 2 + x * 8 = x * 10
	RET

; shift left multiple times
; parameters: value, count
; returns: shifted value
_SHLM:
	DUP
	CBRANCH.Z _SHLM_X	; if count is zero, exit
	LOADC 8
	CMPU.S0 LT		; count is less than 8?
	CBRANCH _SHLM_1
				; if >= 8, continue here
	DEC 8			; decrement counter by 8
	SWAP			; swap counter and value
	BROT			; byte rotate value
	LOADCP $FFFFFF00	; and mask the lowest 8 bits
	AND			; to get a shift left by 8
	SWAP			; swap back counter and value
	BRANCH _SHLM
_SHLM_1:
	DEC 1
	SWAP
	SHL
	SWAP
	BRANCH _SHLM
_SHLM_X:
	DROP	; drop counter
	RET

; shift right multiple times
; parameters: value, count
; returns: shifted value
_SHRM:
	DUP
	CBRANCH.Z _SHRM_X	; if count is zero, exit
	DEC 1
	SWAP
	SHR
	SWAP
	BRANCH _SHRM
_SHRM_X:
	DROP	; remove counter
	RET


; --- print signed integer as decimal
; parameter: value
PRINTDEC:
	DUP
	LOADC 0
	CMP GE
	CBRANCH PRINTDECU
	LOADC '-'
	LOADCP CONOUT
	CALL
	NOT
	INC 1 ; negate value
	; fallthrough to PRINTDECU

; print unsigned integer as decimal
; parameter: value
; local vars:
;  0: current value
;  4: pointer to digit value table
;  8: digit counter
; 12: flag for leading zeroes
PRINTDECU:
	FPADJ -16
	STORE 0
	LOADCP CONVDEC_TAB
	STORE 4
	LOADC 0
	STORE 12
PRDEC_NEXTDIGIT:
	LOADC 0
	STORE 8
	LOAD 4
	LOADI   ; load via pointer
	CBRANCH.Z PRDECU_DONE ; if digit value is 0, we are done
DIGIT_LOOP:
	LOAD 4
	LOADI
	LOAD 0
	CMPU GT
	CBRANCH DIGIT_DONE
	LOAD 0	
	LOAD 4
	LOADI
	SUB
	STORE 0
	LOAD 8
	INC 1
	STORE 8
	BRANCH DIGIT_LOOP
DIGIT_DONE:
	LOAD 8
	CBRANCH.NZ DIGIT_OUT 	; if digit is not 0, print it
	LOAD 12		     	; is it a leading zero, ignore it
	CBRANCH.Z DIGIT_NEXT
DIGIT_OUT:
	LOADC 1			; set leading zero flag
	STORE 12
	LOAD 8
	LOADCP _PRINTDIGIT
	CALL
DIGIT_NEXT:
	LOAD 4			; increment digit value pointer
	INC 4
	STORE 4
	BRANCH PRDEC_NEXTDIGIT
PRDECU_DONE:
	LOAD 0
	LOADCP _PRINTDIGIT
	CALL
	FPADJ 16
	RET

_PRINTDIGIT:
	LOADC '0'
	ADD
	LOADCP CONOUT
	CALL
	RET
CONVDEC_TAB:
	.WORD 1000000000,100000000,10000000,1000000,100000,10000,1000,100,10,0

; ------ read a 8-digit hexadecimal number from the console
; stores variables on the user stack, so the FP register must be
; inizialized.
; returns two values on the eval stack:
;   - return code (topmost)
;     0 - no valid number
;     1 - valid number
;     2 - valid number and enter was pressed
;   - result value
_READHEX:
	FPADJ -8
	LOADC 0 ; current value
	STORE 0
	LOADC 8 ; max number of digits
	STORE 4 ; remaining digits counter
_READHEX_1:
	LOADCP CONIN
	CALL
	LOADC CR     ; RETURN pressed?
	CMP.S0 EQ
	CBRANCH _READHEX_RT
	DUP
	LOADCP CONOUT ; echo character
	CALL
	LOADCP _CONVHEXDIGIT
	CALL
	LOADC -1
	CMP.S0 EQ  ; invalid character?
	CBRANCH.NZ _READHEX_XT
	LOAD 0
	SHL 1	; shift previous nibble
	SHL 1
	SHL 1
	SHL 1
	OR ; combine with last digit
	STORE 0
	LOAD 4
	DEC 1
	DUP
	STORE 4
	CBRANCH.NZ _READHEX_1
	BRANCH _READHEX_XT1
_READHEX_RT:   ; if no digits were entered, set return code
	DROP  ; drop read character
	LOAD 4  ;remaining digits counter
	LOADC 8
	CMP NE
	CBRANCH _READHEX_RT2
	LOADC 0	; no valid input
	BRANCH _READHEX_XT3
_READHEX_RT2:
	LOADC 2 ; valid input and return pressed
	BRANCH _READHEX_XT3	
_READHEX_XT:
	DROP
	LOAD 4
	LOADC 8
	CMP EQ  ; if no digits were entered
	CBRANCH _READHEX_XT0
_READHEX_XT1:
	LOADC 1 ; valid input flag
	BRANCH _READHEX_XT3
_READHEX_XT0:
	LOADC 0
_READHEX_XT3:
	LOAD 0
	SWAP
	FPADJ 8
	RET

; ------ convert character on the eval stack to upper case
UPCASE:
	LOADC 'a'
	CMP.S0 LT
	CBRANCH UPCASE_XT
	LOADC 'z'
	CMP.S0 GT
	CBRANCH UPCASE_XT
	LOADC 32
	SUB
UPCASE_XT:
	RET

; ------ convert hexadecimal digit to integer
; ------ takes an ascii character as parameter on the eval stack
; ------ returns an integer value from 0-15 on the eval stack,
; ------ or -1 if the character was not a valid hexadecimal digit
_CONVHEXDIGIT:
	LOADCP UPCASE
	CALL
	LOADC '0'
	CMP.S0 LT ; character < '0'?
	CBRANCH.NZ _CONVHEXDIGIT_ERR
	LOADC '9'
	CMP.S0 GT ; character > '9'?
	CBRANCH.NZ _CONVHEXDIGIT_ISALPHA
	LOADC '0' ; character is between '0' and '9', subtract '0'
	SUB
	BRANCH _CONVHEXDIGIT_NBL
_CONVHEXDIGIT_ISALPHA:
	LOADC 'A'
	CMP.S0 LT ; character < 'A'?
	CBRANCH.NZ _CONVHEXDIGIT_ERR
	LOADC 'F'
	CMP.S0 GT ; character > 'F'?
	CBRANCH.NZ _CONVHEXDIGIT_ERR
	LOADC 55 ; character is between 'A' and 'F', subtract ('A' - 10)
	SUB
_CONVHEXDIGIT_NBL:
	RET
_CONVHEXDIGIT_ERR:
	DROP     ; remove character from stack
	LOADC -1 ; error
	RET

; --------- output a character on serial console
; --------- takes a character (as the lsb of a word) on the eval stack
CONOUT:
	LOADC UART_REG	; address of UART register
	LOADI		; load status
	LOADC 256	; check bit 8 (tx_busy)
	AND
	CBRANCH.NZ CONOUT ; loop if bit 8 is not zero

	; transmitter is idle now, write character
	LOADC 1024	; TX enable bit
	OR
	LOADC UART_REG	; I/O address
	SWAP		; swap addr and value as args for STOREI
	STOREI
	DROP
	RET

; ---------- check if a character has been received
; returns: 1 if a character has been received, 0 otherwise
CONAVAIL:
	LOADC 0 	; preliminary result
	LOADC UART_REG	; address of UART register
	LOADI		; load status
	LOADC 512	; check bit 9 (rx_avail)
	AND
	CBRANCH.Z CONAVAIL_0 ; if bit is zero, we are done
	INC 1		; add 1 to preliminary result
CONAVAIL_0:
	RET

; ---------- wait until a character is received and return it on eval stack
CONIN:
	LOADC UART_REG	; address of UART register
	LOADI		; load status
	LOADC 512	; check bit 9 (rx_avail)
	AND
	CBRANCH.Z CONIN ; loop if bit 9 is zero
	LOADC UART_REG
	LOADI		; read register again
	LOADC 255	; mask status bits
	AND
	LOADC UART_REG	; I/O address
	LOADC 512	; set bit 9 (rx_clear)
	STOREI		; write register
	DROP
	RET

; return absolute value
; parameters: value
; returns: abs(value)
ABS:
	LOADC 0
	CMP.S0 GE
	CBRANCH ABS_XT
	DEC 1	; negate
	NOT
ABS_XT:
	RET

; signed multiplication
_MUL:
	; fallthrough to MULU

; unsigned multiplication: x * y
; parameters: [x, y]
; returns: x * y
_MULU:
    FPADJ -16
    STORE 0     ; x
    STORE 4     ; y
    LOADC 32
    STORE 8     ; bit count
    LOADC 0
    STORE 12    ; result
MULU_LOOP:
    LOAD 8
    CBRANCH.Z MULU_XT   ; if count is zero, exit
    LOAD 0
    LOADC 1
    AND.S0       ; get bit 0
    CBRANCH.Z MULU_1    ; if bit 0 is zero, next binary digit
    LOAD 12
    LOAD 4
    ADD         ; result = result + y
    STORE 12
MULU_1:
    SHR         ; x = x >> 1
    STORE 0
    LOAD 4
    SHL         ; y = y << 1
    STORE 4
    LOAD 8
    DEC 1       ; count = count -1
    STORE 8
    BRANCH MULU_LOOP
MULU_XT:
    LOAD 12
    FPADJ 16
    RET

; signed integer division
; parameters: [x,y]
; result: x/y
_DIV:
	FPADJ -4
	LOADC 0
	STORE 0		; clear negate flag
	DUP
	LOADC 0
	CMP GE		; is y positive?
	CBRANCH DIV_ISPOS
				; y is negative
	NOT
	INC 1		; negate y
	LOADC -1
	STORE 0		; set negate flag
DIV_ISPOS:
	SWAP	; swap x and y
	DUP
	LOADC 0
	CMP GE		; is x positive?
	CBRANCH DIV_ISPOS2
				; x is negative
	NOT
	INC 1		; negate x
	LOAD 0
	NOT			; invert negate flag
	STORE 0
DIV_ISPOS2:
	SWAP	; swap back y and x
	LOADCP _DIVMODU
	CALL
	DROP	; throw away remainder
	LOAD 0
	CBRANCH.Z DIV_XT ; negate flag set?
	NOT
	INC 1			; negate value
DIV_XT:
	FPADJ 4
	RET

; signed integer modulo
; the result is negative if x is negative.
; the sign of y is ignored.
; parameters: [x,y]
; returns: remainder of x/y
_MOD:
	FPADJ -4
	LOADC 0
	STORE 0		; clear negate flag
	DUP
	LOADC 0
	CMP GE		; is y positive?
	CBRANCH MOD_ISPOS
				; y is negative
	NOT
	INC 1		; negate y
MOD_ISPOS:
	SWAP	; swap x and y
	DUP
	LOADC 0
	CMP GE		; is x positive?
	CBRANCH MOD_ISPOS2
				; x is negative
	NOT
	INC 1		; negate x
	LOAD 0
	NOT			; invert negate flag
	STORE 0
MOD_ISPOS2:
	SWAP	; swap back y and x
	LOADCP _DIVMODU
	CALL
	NIP	; throw away quotient
	LOAD 0
	CBRANCH.Z MOD_XT ; negate flag set?
	NOT
	INC 1			; negate value
MOD_XT:

	FPADJ 4
	RET

; unsigned integer division
; parameters: [x,y]
; result: x/y
_DIVU:
	LOADCP _DIVMODU ; just call DIVMODU and throw away the remainder
	CALL
	DROP
	RET

; unsigned integer division with remainder
; parameters: [x,y]
; result: [ quotient, remainder ]
_DIVMODU:
    FPADJ -20
    STORE 0 ; y
    STORE 4 ; x
    LOADC 32
    STORE 8 ; bit count
    LOADC 0
    STORE 12 ; tmp value
    LOADC 0
    STORE 16 ; result
DIVU_LOOP:
    LOAD 8
    CBRANCH.Z DIVU_END  ; if count is zero, exit
    LOAD 16
    SHL                 ; result = result << 1
    STORE 16
    LOAD 12
    SHL                 ; tmp << 1
    LOAD 4
    LOADCP $80000000    ; msb of x
    AND
    CBRANCH.Z DIVU_1
    INC 1               ; tmp[0] is 0, so +1 means tmp[0] = 1
DIVU_1:
    DUP
    STORE 12            ; tmp = tmp << 1 | msb
    LOAD 0
    CMPU GE             ; tmp >= y?
    CBRANCH.Z DIVU_2
    LOAD 16
    INC 1               ; result = result | 1
    STORE 16
    LOAD 12
    LOAD 0
    SUB
    STORE 12            ; tmp = tmp - y
DIVU_2:
    LOAD 4
    SHL
    STORE 4
    LOAD 8
    DEC 1
    STORE 8
    BRANCH DIVU_LOOP
DIVU_END:
    LOAD 16	; result (quotient)
    LOAD 12	; remainder
    FPADJ 20
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

; clear a memory block
; parameters: addr, length in bytes
; length must be multiple of wordsize.
; if it is not, the last (partial) word is not cleared.
_CLEARMEM:
	SHR
	SHR	; calculate length in words

CLEARMEM_L:
	DUP
	CBRANCH.Z CLEARMEM_X ; if zero words to do, exit
	SWAP	; swap counter and addr
	LOADC 0
	STOREI 4 ; store with post-increment
	SWAP	; swap counter and addr back
	DEC 1	; decrement counter
	BRANCH CLEARMEM_L
CLEARMEM_X:
	DROP
	DROP
	RET

; copy a number of words from source to destination
; parameters: [ dest, source, count ]
; source and destination may not overlap
	.EQU COPYWORDS_COUNT 0
	.EQU COPYWORDS_SRC 4
	.EQU COPYWORDS_DEST 8
_COPYWORDS:
	FPADJ -12
	STORE COPYWORDS_COUNT	; store args to local vars
	STORE COPYWORDS_SRC
	STORE COPYWORDS_DEST
	LOAD COPYWORDS_COUNT	; we will keep count on the stack inside the loop
COPYWORDS_L0:
	DUP			; count is on tos, duplicate it
	CBRANCH.Z COPYWORDS_XT	; check if count is zero
	DEC 1			; if not, decrement
	LOAD COPYWORDS_DEST	; load dest addr for STOREI below
	LOAD COPYWORDS_SRC	; load src addr
	INC.S1.X2Y 4		; increment by 4 and put as new value on tos
	STORE COPYWORDS_SRC	; store again, old addr is now on tos
	LOADI			; load value by old addr
	STOREI 4		; store value and post-increment
	STORE COPYWORDS_DEST	; store post-incremented addr
	BRANCH COPYWORDS_L0
COPYWORDS_XT:
	DROP			; drop count value
	FPADJ 12
	RET

; compare a number of words
; parameters: [ dest, source, count ]
; returns: 1 if all words are equal, 0 otherwise
	.EQU CMPWORDS_COUNT 0
	.EQU CMPWORDS_SRC 4
	.EQU CMPWORDS_DEST 8
_CMPWORDS:
	FPADJ -12
	STORE CMPWORDS_COUNT	; store args to local vars
	STORE CMPWORDS_SRC
	STORE CMPWORDS_DEST
	LOAD CMPWORDS_COUNT
CMPWORDS_L0:
	DUP			; count is on tos, duplicate it
	CBRANCH.Z CMPWORDS_XT	; check if count is zero
	DEC 1			; if not, decrement
	LOAD CMPWORDS_SRC	; load src addr
	INC.S1.X2Y 4		; increment by 4 and put as new value on tos
	STORE CMPWORDS_SRC	; store again, old addr is now on tos
	LOADI			; load src value
	LOAD CMPWORDS_DEST	; load dest addr
	INC.S1.X2Y 4		; increment by 4 and put as new value on tos
	STORE CMPWORDS_DEST	; store again, old addr is now on tos
	LOADI			; load dest value
	CMPU EQ
	CBRANCH CMPWORDS_L0	; if words are equal, continue loop
	DROP			; drop count value
	LOADC 0				; load exit code 0
	BRANCH CMPWORDS_XT2
CMPWORDS_XT:
	DROP				; drop count value
	LOADC 1				; load exit code 1
CMPWORDS_XT2:
	FPADJ 12
	RET

	.CPOOL
; --------- Graphics Library ---------------
	; vga controller registers
	.EQU FB_RA	$900
	.EQU FB_WA	$901
	.EQU FB_IO	$902
	.EQU FB_PS	$903
	.EQU FB_PD	$904
	.EQU FB_CTL	$905
; set a pixel in fb memory
; parameters: x,y - coordinates
PUTPIXEL_1BPP:
	; calculate vmem address:
	OVER ; duplicate x
	; divide x by 32
	SHR
	SHR
	SHR
	SHR
	SHR
	SWAP
	; multiply y by words per line
	SHL 2
	SHL 2
	SHL

	ADD ; add results together for vmem addr

	DUP
	LOADCP FB_WA
	SWAP
	STOREI	; store to framebuffer write addr register
	DROP
	LOADCP FB_RA ; and to framebuffer read addr register
	SWAP
	STOREI
	DROP

	; x is now at top of stack
	; get bit value from x modulo 32
	LOADC 31
	AND
	SHL 2 ; (x & 31) * 4 = offset into table
	LOADCP INT_TO_PIX_TABLE
	ADD
	LOADI

	LOADCP FB_IO
	; read old vmem value
	LOADCP FB_IO
	LOADI
	; or in new bit
	OR
	; write new value
	STOREI
	DROP

	RET

INT_TO_PIX_TABLE:
	.WORD %10000000_00000000_00000000_00000000
	.WORD %01000000_00000000_00000000_00000000
	.WORD %00100000_00000000_00000000_00000000
	.WORD %00010000_00000000_00000000_00000000
	.WORD %00001000_00000000_00000000_00000000
	.WORD %00000100_00000000_00000000_00000000
	.WORD %00000010_00000000_00000000_00000000
	.WORD %00000001_00000000_00000000_00000000
	.WORD %00000000_10000000_00000000_00000000
	.WORD %00000000_01000000_00000000_00000000
	.WORD %00000000_00100000_00000000_00000000
	.WORD %00000000_00010000_00000000_00000000
	.WORD %00000000_00001000_00000000_00000000
	.WORD %00000000_00000100_00000000_00000000
	.WORD %00000000_00000010_00000000_00000000
	.WORD %00000000_00000001_00000000_00000000
	.WORD %00000000_00000000_10000000_00000000
	.WORD %00000000_00000000_01000000_00000000
	.WORD %00000000_00000000_00100000_00000000
	.WORD %00000000_00000000_00010000_00000000
	.WORD %00000000_00000000_00001000_00000000
	.WORD %00000000_00000000_00000100_00000000
	.WORD %00000000_00000000_00000010_00000000
	.WORD %00000000_00000000_00000001_00000000
	.WORD %00000000_00000000_00000000_10000000
	.WORD %00000000_00000000_00000000_01000000
	.WORD %00000000_00000000_00000000_00100000
	.WORD %00000000_00000000_00000000_00010000
	.WORD %00000000_00000000_00000000_00001000
	.WORD %00000000_00000000_00000000_00000100
	.WORD %00000000_00000000_00000000_00000010
	.WORD %00000000_00000000_00000000_00000001

PUTMPIXEL:
	LOADC 1
; set a pixel in fb memory
; parameters: x,y,color - coordinates, color value (0-15)
PUTPIXEL:
PUTPIXEL_4BPP:
	.EQU PUTPIXEL_X 0
	.EQU PUTPIXEL_Y 4
	.EQU PUTPIXEL_COLOR 8
	.EQU PUTPIXEL_PIXPOS 12
	.EQU PUTPIXEL_FS 16

	FPADJ -PUTPIXEL_FS

	STORE PUTPIXEL_COLOR
	STORE PUTPIXEL_Y
	STORE PUTPIXEL_X


	; calculate vmem address: (x / 8) + (y * 80)
	LOAD PUTPIXEL_X
	; divide x by 8
	SHR
	SHR
	SHR

	LOAD PUTPIXEL_Y
	; multiply y by words per line
	SHL 2
	SHL 2 ; * 16
	DUP
	SHL 2; * 64
	ADD  ; x*16 + x*64

	ADD ; add results together for vmem addr

	LOADCP FB_WA
	OVER
	STOREI	; store to framebuffer write addr register
	DROP
	LOADCP FB_RA ; and to framebuffer read addr register
	SWAP	; swap addr and value for STOREI
	STOREI
	DROP

	LOAD PUTPIXEL_X
	;		|0000.0000|0000.0000|0000.0000|0000.1111|
	LOADC 7
	AND		; calculate pixel position in word
	LOADC 7
	SWAP
	SUB		; pixpos = 7 - (x & 7)
	STORE PUTPIXEL_PIXPOS

	LOAD PUTPIXEL_COLOR
	LOAD PUTPIXEL_PIXPOS
	SHR		; rcount = pixpos / 2
ROTLOOP_:
	DUP		; exit loop if rcount is 0
	CBRANCH.Z ROTLOOP_END
	SWAP		; pixel value is now on top of stack
	BROT		; value = value << 8
	SWAP		; rcount is now on top of stack
	DEC 1		; rcount = rcount - 1
	BRANCH ROTLOOP_
ROTLOOP_END:
	DROP		; drop rcount
	; shifted pixel value is now at top of stack
	LOAD PUTPIXEL_PIXPOS
	LOADC 1
	AND
	CBRANCH.Z EVEN_PIXPOS
	SHL 2	; if pixpos is odd, shift by 4 bits
	SHL 2
EVEN_PIXPOS:
	LOAD PUTPIXEL_X
	; get bit value from x modulo 8
	LOADC 7
	AND
	SHL 2 ; (x & 7) * 4 = offset into table
	LOADCP INT_TO_MASK_TABLE
	ADD
	LOADI

	; read old vmem value
	LOADCP FB_IO
	LOADI
	; mask bits
	AND
	; or in shifted pixel value
	OR

	; write new value
	LOADCP FB_IO
	SWAP
	STOREI
	DROP

	FPADJ PUTPIXEL_FS
	RET

	.CPOOL

INT_TO_MASK_TABLE:
	.WORD %00001111_11111111_11111111_11111111
	.WORD %11110000_11111111_11111111_11111111
	.WORD %11111111_00001111_11111111_11111111
	.WORD %11111111_11110000_11111111_11111111
	.WORD %11111111_11111111_00001111_11111111
	.WORD %11111111_11111111_11110000_11111111
	.WORD %11111111_11111111_11111111_00001111
	.WORD %11111111_11111111_11111111_11110000

; draw a line between two points
; parameters: x0, y0, x1, y1, color
	.EQU DL_X0 0
	.EQU DL_Y0 4
	.EQU DL_X1 8
	.EQU DL_Y1 12
	.EQU DL_DX 16
	.EQU DL_DY 20
	.EQU DL_ERR 24
	.EQU DL_E2  28
	.EQU DL_SX 32
	.EQU DL_SY 36
	.EQU DL_COL 40
	.EQU STACKFRAME_SIZE 44

DRAWLINE_M:
	LOADC 1
DRAWLINE:
	FPADJ -STACKFRAME_SIZE

	STORE DL_COL ; store args
	STORE DL_Y1
	STORE DL_X1
	STORE DL_Y0
	STORE DL_X0

	LOAD DL_X1 ; dx = abs(x1-x0)
	LOAD DL_X0
	SUB
	LOADCP ABS
	CALL
	STORE DL_DX

	LOAD DL_Y1 ; dy = -abs(y1-y0)
	LOAD DL_Y0
	SUB
	LOADCP ABS
	CALL
	DEC 1
	NOT
	STORE DL_DY

	LOAD DL_X0 ; sx = (x0<x1) ? 1 : -1
	LOAD DL_X1
	CMP LT
	CBRANCH DL_SX0
	LOADC -1
	BRANCH DL_SX1
DL_SX0: LOADC 1
DL_SX1: STORE DL_SX

	LOAD DL_Y0 ; sy = (y0<y1) ? 1 : -1
	LOAD DL_Y1
	CMP LT
	CBRANCH DL_SY0
	LOADC -1
	BRANCH DL_SY1
DL_SY0: LOADC 1
DL_SY1: STORE DL_SY

	LOAD DL_DX ; err = dx + dy
	LOAD DL_DY
	ADD
	STORE DL_ERR

DL_LOOP:
	LOAD DL_X0
	LOAD DL_Y0
	LOAD DL_COL
	LOADCP PUTPIXEL
	CALL

	LOAD DL_X0 ; if x0 == x1 and y0 == y1, exit
	LOAD DL_X1
	CMP NE
	CBRANCH DL_CONT

	LOAD DL_Y0
	LOAD DL_Y1
	CMP EQ
	CBRANCH DL_END

DL_CONT:
	LOAD DL_ERR ; e2 = 2 * err
	SHL
	STORE DL_E2

	LOAD DL_E2 ; if e2 > dy
	LOAD DL_DY
	CMP GT
	CBRANCH.Z DL_SKIP1

	LOAD DL_ERR ; err += dy
	LOAD DL_DY
	ADD
	STORE DL_ERR

	LOAD DL_X0 ; x0 += sx
	LOAD DL_SX
	ADD
	STORE DL_X0

DL_SKIP1:
	LOAD DL_E2 ; if e2 < dx
	LOAD DL_DX
	CMP LT
	CBRANCH.Z DL_SKIP2

	LOAD DL_ERR ; err += dx
	LOAD DL_DX
	ADD
	STORE DL_ERR

	LOAD DL_Y0 ; y0 += sy
	LOAD DL_SY
	ADD
	STORE DL_Y0

DL_SKIP2:
	BRANCH DL_LOOP
DL_END:
	FPADJ STACKFRAME_SIZE
	RET

; initialize the palette registers
INITPALETTE:
	LOADCP DEFAULT_PALETTE ; load pointer to color table
	LOADC 0		; load counter
INITPAL_0:
	DUP
	LOADC FB_PS ; store counter to palette select register
	SWAP	; swap addr and value for STOREI
	STOREI
	DROP

	SWAP 	; pointer on top of stack
	DUP
	LOADI	; load color value
	LOADC FB_PD
	SWAP	; swap addr and value for STOREI
	STOREI	; store to palette data register
	DROP
	INC 4	; increment pointer

	SWAP 	; counter on top of stack
	DUP
	LOADC 15
	CMPU EQ
	CBRANCH INITPAL_X ; exit if counter is 15

	INC 1	; increment counter
	BRANCH INITPAL_0

INITPAL_X:
	DROP ; remove counter and pointer
	DROP
	RET

; set a palette register
; parameters [ palette slot nr, color value ]
SETPALETTE:
	SWAP		; slot nr to top
	LOADC FB_PS	; load address of palette select register
	SWAP		; swap addr and slot nr for STOREI
	STOREI
	DROP		; remove addr from STOREI
			; left on stack now: color value
	LOADC FB_PD	; load address of palette data register
	SWAP		; swap addr and color value for STOREI
	STOREI
	DROP		; remove addr

	RET

DEFAULT_PALETTE:
	.WORD 0,    $FFF, $F00, $0F0, $00F, $0FF, $F0F, $FF0
	.WORD $777, $777, $700, $070, $007, $077, $707, $770

; set whole video memory to zero
CLEARGRAPHICS:
	LOADC 0
CL_LOOP:
	LOADC FB_WA
	OVER ; duplicate value
	STOREI
	DROP

	LOADC FB_IO
	LOADC 0
	STOREI
	DROP

	INC 1

	LOADCP 32768
	CMP.S0 NE
	CBRANCH CL_LOOP

	DROP

	RET

INITGRAPHICS:
	LOADCP CLEARGRAPHICS
	CALL
	LOADCP INITPALETTE
	CALL
	RET

; wait for vertical blank
; we first wait for the VBLANK bit
; to become zero to make sure we
; catch the beginning of the vertical blank
WAITVSYNC:
	; wait for VBLANK to become zero
	LOADC FB_CTL
	LOADI		; read control register
	LOADC 1		; check bit 0 (VBLANK)
	AND
	CBRANCH.NZ WAITVSYNC ; if set, loop
VSYNC_WAIT1:
	; wait for VBLANK to become one
	LOADC FB_CTL
	LOADI		; read control register
	LOADC 1		; check bit 0 (VBLANK)
	AND
	CBRANCH.Z VSYNC_WAIT1 ; if not set, loop
	RET

; args: number of bytes, pointer to buf,
HEXDUMP:
	DUP
	LOADI
	LOADCP PRINTHEXW
	CALL
	LOADC ' '
	LOADCP CONOUT
	CALL
	INC 4

	DUP
	LOADI
	LOADCP PRINTHEXW
	CALL
	LOADC ' '
	LOADCP CONOUT
	CALL
	INC 4

	DUP
	LOADI
	LOADCP PRINTHEXW
	CALL
	LOADC ' '
	LOADCP CONOUT
	CALL
	INC 4

	DUP
	LOADI
	LOADCP PRINTHEXW
	CALL
	LOADC ' '
	LOADCP CONOUT
	CALL
	INC 4

	LOADCP NEWLINE
	CALL

	SWAP		; swap pointer and counter
	LOADC 16
	SUB
	DUP
	CBRANCH.Z HEXDUMP_END ; end if counter is zero
	SWAP		; swap back counter and pointer
	BRANCH HEXDUMP

HEXDUMP_END:
	DROP
	DROP
	RET

; inquire cursor position
; args: pointer to columns variable, pointer to rows variable
GETCURSORPOS:
	LOADCP TERM_CPR_STR
	LOADCP PRINTLINE
	CALL

	LOADCP CONIN ; skip ESC
	CALL
	DROP
	LOADCP CONIN ; and '['
	CALL
	DROP

	LOADC ';'
	LOADCP _TERMRCVINT
	CALL
	STOREI
	DROP

	LOADC 'R'
	LOADCP _TERMRCVINT
	CALL
	STOREI
	DROP

	RET

; receive digits and compose an integer value
; up to a termination character or  an ';'
; args: 	termination character
; returns: 	-1 on error (invalid digit)

_TERMRCVINT:
	FPADJ -4
	STORE 0

	LOADC 0	; start with 0 value
RCVINT_L:
	LOADCP CONIN
	CALL

	DUP		; duplicate received char
	LOAD 0		; compare with terminator
	CMP EQ		; if equal,
	CBRANCH RCVINT_XT ; exit

	LOADC '0'	; subtract ascii value to get
	SUB		; numerical value

	DUP
	LOADC 0		; check if less than zero
	CMP LT
	CBRANCH RCVINT_ERR ; if yes, error

	DUP
	LOADC 9
	CMP GT		; check if > 9
	CBRANCH RCVINT_ERR ; if yes, error

	SWAP
	LOADCP _MUL10	; old value * 10 (shift digits to the left)
	CALL

	ADD		; add to value
	BRANCH RCVINT_L ; next digit
RCVINT_ERR:
	DROP
	DROP
	LOADC -1
	BRANCH RCVINT_XT2
RCVINT_XT:
	DROP
RCVINT_XT2:
	FPADJ 4
	RET

TERM_CPR_STR: .BYTE 27, "[6n", 0 ; ANSI Cursor Position Report

	.CPOOL

GETTICKS:
	LOADC IRQC_REG
	LOADI
	LOADC -256
	AND
	BROT
	BROT
	BROT
	RET

	.EQU CRLD_BLOCK 0
	.EQU CRLD_BYTES 4
	.EQU CRLD_ADDR 8
	.EQU CRLD_FS 12

; load a program image from sd card
; args: device id, block no, size in bytes
CORELOAD:
	; We need to set the FP and RP registers,
	;	because we might overwrite that
	;	memory area where the calling program
	;	has its user and return stack

	LOADCP FP_START
	STOREREG FP
	LOADCP RP_START
	STOREREG RP

	FPADJ -CRLD_FS
	STORE CRLD_BYTES
	STORE CRLD_BLOCK
	DROP	; ignore device ID,
		; we support only one sd card

	; divide bytes by 512 and add one
	; to get block count
	LOAD CRLD_BYTES
	LOADC 9
	LOADCP _SHRM
	CALL
	INC 1
	; keep block count on stack

	; start at address 24576
	LOADCP PROG_START
	STORE CRLD_ADDR

CRLD_LP:
	; read a block
	LOAD CRLD_BLOCK
	LOADCP CARDREADBLK
	CALL
	DROP	; ignore error for now

	LOAD CRLD_ADDR
	LOADCP CARD_BUF
	LOADC 128
	LOADCP _COPYWORDS
	CALL

	; advance dest pointer
	LOAD CRLD_ADDR
	LOADC 512
	ADD
	STORE CRLD_ADDR

	; increment block number
	LOAD CRLD_BLOCK
	INC 1
	STORE CRLD_BLOCK

	; decrement block count on stack
	DEC 1
	DUP
	CBRANCH.NZ CRLD_LP ; if block count not zero, loop
	DROP	; remove block count
CRLD_CLEAN:
	; clean up partial block
	LOADC 512
	LOAD CRLD_BYTES
	INC 3	; round up to next word
	LOADC -4
	AND
	LOADC 511 ; get remainder by 512
	AND
	SUB	; subtract to get number of remaining bytes
		; in last block

	LOAD CRLD_ADDR	; this is now the addr of the last
			; byte in the last block + 1
	OVER		; duplicate number of remaining bytes
	SUB		; and subtract from addr

	SWAP		; swap addr and number of remaining bytes
	LOADCP _CLEARMEM
	CALL

	; clear estack

	; release our stack frame
	FPADJ CRLD_FS

	; jump to program start address
	LOADCP PROG_START
	JUMP

	; no RET

	.CPOOL

READDIRBLK:
; parameters: [ blkno, ptr to DirBlock, ptr to error return value, device id ]
; same routine as READBLOCK, is referenced by two names to convert buffer types

; parameters: [ blkno, ptr to PartitionTableBlock, ptr to error return value, device id ]
READPARTBLK:
; same routine as READBLOCK, is referenced by two names to convert buffer types

READBLOCK:
; parameters: [ blkno, ptr to IOBlock, ptr to error return value, device id ]
	DROP	; ignore device id

	LOADC 0
	STOREI	; set return value to zero
	DROP
	SWAP	; swap blkno and ptr
	LOADCP CARDREADBLK ; read that block number
	CALL
	DROP 	; ignore error for now
		; TODO: store it via error ptr
		; ptr to PartitionTableBlock is now on ToS
	LOADCP  CARD_BUF
	LOADC 128
	LOADCP _COPYWORDS ; copy block to destination buffer
	CALL

	RET

WRITEPARTBLK:
WRITEDIRBLK:
; parameters: [ blkno, ptr to IOBlock, ptr to error return value, device id ]
WRITEBLOCK:
	DROP	; ignore device id

	LOADC 0
	STOREI	; set return value to zero
	DROP

	LOADCP  CARD_BUF
	SWAP
	LOADC 128
	LOADCP _COPYWORDS ; copy block to card_buf
	CALL

	LOADCP CARDWRITEBLK ; write that block number
	CALL
	DROP 	; ignore error for now
		; TODO: store it via error ptr
		; ptr to PartitionTableBlock is now on ToS
	RET

%include "sdcardlib.s"

	.CPOOL

SHELLWORKFILE:	.WORD 0,68
	.BLOCK 17
SHELLCMD: .WORD 0,40
	.BLOCK 8
SHELLARG: .WORD 0
PARGCOUNT: .WORD 0
PARGS:	.WORD 0,80
	.BLOCK 20
	.WORD 0,80
	.BLOCK 20
	.WORD 0,80
	.BLOCK 20
	.WORD 0,80
	.BLOCK 20
	.WORD 0,80
	.BLOCK 20
	.WORD 0,80
	.BLOCK 20
	.WORD 0,80
	.BLOCK 20
	.WORD 0,80
	.BLOCK 20

DEFAULTVOLUME: .WORD 0,32
	.BLOCK 8

SYSCLOCK:
	.BLOCK 6
SYSBOOTTICKS:
	.WORD 0
SYSLASTTICKS:
	.WORD 0

; copy words to screen memory
; args: pointer to 32000 words of pixel data
PUTSCREEN:
	LOADC FB_WA
	LOADC 0		; initialize write address register
	STOREI
	DROP

	LOADCP 32000	; word count
PUTSCREEN_L0:
	SWAP		; [ count, addr ]
	DUP
	LOADI		; load pixel word
	LOADC FB_IO
	SWAP		; swap addr and value for STOREI
	STOREI		; store to vmem io register
	DROP

	INC 4		; next word

	SWAP		; swap addr and count
	DEC 1		; decrement count

	DUP
	CBRANCH.NZ PUTSCREEN_L0	; loop if count is not zero

	DROP	; remove counter and addr
	DROP

	RET


%export _MUL
%export _MULU
%export _DIV
%export _DIVU
%export _MOD
%export _DIVMODU
%export _SHRM
%export _SHLM
%export _COPYWORDS
%export _CMPWORDS
%export _CLEARMEM
