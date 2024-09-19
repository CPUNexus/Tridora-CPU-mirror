; Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details
	.EQU LOADER_START 4096

	.EQU STRM_FS 16
	.EQU STRM_PTR 0
	.EQU STRM_IDX 4
	.EQU STRM_LEN 8
	.EQU STRM_DELTA 12

; copy bytes inside a pascal string, moving up
; args: string ptr, index, length, delta
STRMOVEUP:
	FPADJ -STRM_FS
	STORE STRM_DELTA
	STORE STRM_LEN
	STORE STRM_IDX
	STORE STRM_PTR


	LOADCP STRM_CHKARGS
	CALL
	CBRANCH.Z STRMU_XT

	; args are ok, call _BMOVEUP
	; offset
	LOAD STRM_DELTA
	; src
	LOAD STRM_IDX
	DEC 1		; adjust for base 1
	LOAD STRM_PTR
	INC 8		; skip string header
	ADD
	; count
	LOAD STRM_LEN

	LOADCP _BMOVEUP
	CALL
STRMU_XT:
	FPADJ STRM_FS
	RET

; check args for STRMOVEUP and STRMOVEDOWN
; shares stack frame with caller
; returns 0 if args are invalid
STRM_CHKARGS:	; check if length or index are negative
	LOAD STRM_LEN
	LOADC 0
	CMP LE
	CBRANCH STRMC_XT

	LOAD STRM_IDX
	LOADC 0
	CMP LE
	CBRANCH STRMC_XT

	LOAD STRM_DELTA
	LOADC 1
	CMP LT
	CBRANCH STRMC_XT

	; check if index + len + delta < string end
	LOAD STRM_PTR
	LOADI		; get string size

	LOAD STRM_IDX 
	LOAD STRM_DELTA
	ADD
	LOAD STRM_LEN
	ADD
	DEC 1		; adjust for base 1

	CMP LT
	CBRANCH STRMC_XT

	LOADC 1
	RET
STRMC_XT:
	LOADC 0
	RET

; copy bytes inside a pascal string, moving down
; index is the destination, index + delta the source
; args: string ptr, index, length, delta
STRMOVEDOWN:
	FPADJ -STRM_FS
	STORE STRM_DELTA
	STORE STRM_LEN
	STORE STRM_IDX
	STORE STRM_PTR

	LOADCP STRM_CHKARGS
	CALL
	CBRANCH.Z STRMD_XT

	; args are ok, call _BMOVEDOWN
	; offset
	LOAD STRM_DELTA
	; src
	LOAD STRM_IDX
	DEC 1		; adjust for base 1
	LOAD STRM_PTR
	INC 8		; skip string header
	ADD
	; count
	LOAD STRM_LEN

	LOADCP _BMOVEDOWN
	CALL
STRMD_XT:
	FPADJ STRM_FS
	RET

; copy bytes from src to src + offset, moving
; upwards. the copied byte ranges may overlap.
; count and offset must be > 0
; args:  offset, src, count
	.EQU BMU_COUNT 0
	.EQU BMU_SRC 4
	.EQU BMU_OFFS 8
	.EQU BMU_FS 12
_BMOVEUP:
	FPADJ -BMU_FS
	DUP
	STORE BMU_COUNT
	DEC 1
	ADD	; store end of source range
	STORE BMU_SRC
	STORE BMU_OFFS

	; copy bytes from end to start, descending
	LOAD BMU_COUNT	; keep count on stack for the loop
_BMU_L:

	LOAD BMU_SRC
	LOAD BMU_OFFS
	ADD		; dest addr = src + offs
	LOADI.S1.X2Y	; [ dest, word ]
	OVER		; [ dest, word, dest ]
	LOADC $FF	; [ dest, word, dest, $FF]
	BPLC
	NOT		; [ dest, word, mask ]
	AND		; [ dest, masked word ]

	OVER		; [ dest, masked, dest ]
	LOAD BMU_SRC	; [ dest, masked, dest, src ]
	LOADI.S1.X2Y	; [ dest, masked, dest, src, src word ]
	BSEL		; [ dest, masked, dest, src byte ]
	BPLC		; [ dest, masked, dest byte ]
	OR		; [ dest, dest word ]
	STOREI
	DROP

	DEC 1
	DUP
	CBRANCH.Z _BMU_X
	LOAD BMU_SRC
	DEC 1
	STORE BMU_SRC
	BRANCH _BMU_L
_BMU_X:
	DROP
	FPADJ BMU_FS
	RET

; copy bytes from dest + offset to dest, moving
; downwards. the copied byte ranges may overlap.
; count and offset must be > 0
; args:  offset, dest, count
	.EQU BMD_COUNT 0
	.EQU BMD_DEST 4
	.EQU BMD_OFFS 8
	.EQU BMD_FS 12
_BMOVEDOWN:
	FPADJ -BMD_FS
	STORE BMD_COUNT
	STORE BMD_DEST
	STORE BMD_OFFS

	; copy bytes from start to end
	LOAD BMU_COUNT	; keep count on stack for the loop
_BMD_L:
	LOAD BMD_DEST	; [ dest ]
	LOADI.S1.X2Y	; [ dest, dest word ]
	OVER		; [ dest, word, dest ]
	LOADC $FF
	BPLC
	NOT		; [ dest, word, mask ]
	AND		; [ dest, masked word ]
	OVER		; [ dest, masked word, dest ]

	DUP
	LOAD BMD_OFFS
	ADD		; src addr = dest + offs
	LOADI.S1.X2Y	; get source word
	BSEL		; get source byte
			; stack now: [ dest, masked word, dest, source byte ]
	BPLC		; [ dest, masked word, rotated byte ]
	OR		; [ dest, new word ]
	STOREI
	DROP

	DEC 1
	DUP
	CBRANCH.Z _BMD_X
	LOAD BMD_DEST
	INC 1
	STORE BMD_DEST
	BRANCH _BMD_L
_BMD_X:
	DROP
	FPADJ BMD_FS
	RET

; copy bytes between two arrays of words
; no bounds check is performed
; parameters: [ dest, destoffset, src, srcoffset, length ]
	.EQU CPB_LEN  0
	.EQU CPB_DPTR 4
	.EQU CPB_SPTR 8
	.EQU CPB_FS 12
COPYBUF:
	FPADJ -CPB_FS
	STORE CPB_LEN
	ADD	; add src and srcoffset
	STORE CPB_SPTR
	ADD	; add dest and destoffset
	STORE CPB_DPTR

	; check if source ptr and dest prtr
	; are word-aligned
	LOAD CPB_SPTR
	LOAD CPB_DPTR
	OR
	LOADC 3
	AND
	CBRANCH COPYBYTES ; if not, continue with COPYBYTES

COPYBUF_1:
	LOAD CPB_LEN	; check if length is smaller than
	LOADC 4		; word size
	CMP LT
	CBRANCH COPYBYTES ; if yes, continue with COPYBYTES

	; else copy whole words
	LOAD CPB_DPTR
	LOAD CPB_SPTR
	LOAD CPB_LEN
	SHR	; COPYWORDS needs number of words, not bytes
	SHR	; so divide by 4
	LOADCP _COPYWORDS
	CALL

	; calculate remaining bytes
	LOAD CPB_LEN
	DUP	; duplicate for offset calculation below
	LOADC 3
	AND	; get lower two bits for remaining byte count
	DUP
	CBRANCH.Z COPYBUF_XT ; if zero remaining, exit
	STORE CPB_LEN ; store as new length
	LOADC ~3 ; calculate length rounded down to word size
	AND	; by setting lower two bits to zero
	DUP	; duplicate it to calculate two offsets
	LOAD CPB_DPTR
	ADD
	STORE CPB_DPTR  ; move dest ptr to remaining bytes
	LOAD CPB_SPTR
	ADD
	STORE CPB_SPTR	; move src ptr to remaining bytes
	BRANCH COPYBYTES

COPYBUF_XT:
	DROP	; cleanup stack
	DROP
	FPADJ CPB_FS
	RET

; Copy single bytes, branch from COPYBUF
COPYBYTES:
	LOAD CPB_LEN		; put counter on stack
COPYBYTES_L:
	DUP			; check if remaining length is 0
	CBRANCH.Z COPYBYTES_XT	; exit loop if true
	LOAD CPB_DPTR		; load dest ptr for SETSTRINGCHAR below
	INC.S1.X2Y 1		; increment dest ptr and keep old value
	STORE CPB_DPTR		; store incremented dest ptr, old value is now ToS
	LOAD CPB_SPTR		; load src ptr
	INC.S1.X2Y 1		; increment src ptr and keep old value
	STORE CPB_SPTR		; store incremented src ptr, old value is now ToS
	LOADI.S1.X2Y		; load source word, keep addr
	BSEL			; select byte from word via addr
	LOADCP _SETSTRINGCHAR	; put byte into destination
	CALL
	DEC 1			; decrement counter
	BRANCH COPYBYTES_L
COPYBYTES_XT:
	DROP		; remove counter
	FPADJ CPB_FS
	RET

; helper routine to read a char from a file
; parameters: [ pointer to file record ]
READFSCHAR:
	LOADCP READFSCHARBUF
	LOADC 1  ; length arg
	LOADCP READFS
	CALL

	LOADC 0		; byte address for BSEL
	LOADCP READFSCHARBUF
	LOADI

	BSEL		; get the most significant byte
	RET

READFSCHARBUF:
	.WORD 0

; helper routine to write a byte to a file
; parameters: [ pointer to file record, char value ]
WRITEFSCHAR:
	BROT	; rotate char value to MSB
	BROT
	BROT
	LOADCP WRITEFSCHARBUF
	SWAP
	STOREI ; store rotated value to buffer
		; keep address on stack (WRITEFSCHARBUF)
	LOADC 1 ; length arg
	LOADCP WRITEFS
	JUMP	; save one RET

WRITEFSCHARBUF:
	.WORD 0

; helper routine to write a string to a file
; parameters: pointer to file record, string pointer
WRITEFSSTRING:
	DUP	; duplicate string ptr
	INC 8	; skip string header
	SWAP	; put original string ptr on ToS
	LOADI	; load length from string header
		; stack is now [ ptr to file, string ptr+8, length ]
	LOADCP WRITEFS
	JUMP
; write a number of words to a channel
; parameters: [ ptr to file record, word pointer, word count ]
WRITECHANWORDS:
	; we ignore the file record since there is only
	; one device for now
	DUP
	CBRANCH.Z WRITECHANW_XT	; if count is zero, exit
	DEC 1	; decrement counter
	SWAP	; swap counter and word ptr
	LOADI.S1.X2Y	; load word, keep addr
	LOADCP CONOUTW
	CALL
	INC 4	; increment addr
	SWAP	; swap back word ptr and counter
	BRANCH WRITECHANWORDS
WRITECHANW_XT:
	DROP
	DROP
	DROP
	RET


; read a number of words from a channel
; parameters: [ ptr to file record, destination pointer, word count ]
READCHANWORDS:
	; we ignore the file record since there is only
	; one device for now
	DUP
	CBRANCH.Z WRITECHANW_XT	; if count is zero, exit
	DEC 1	; decrement counter
	SWAP	; swap counter and word ptr
	LOADCP CONINW	; read four bytes
	CALL
	STOREI 4	; store and post-increment
	SWAP	; swap back word ptr and counter
	BRANCH READCHANWORDS
READCHANW_XT:
	DROP
	DROP
	DROP
	RET

; --- check if a string is already initialized
; parameters: addr
; returns: 1 if it is initialied, 0 otherwise
_STRINGISINITED:
	LOADI.S1.X2Y	; load length field, keep addr
	CBRANCH.NZ STRIN_XT1 ; if not zero, it is already inited
	INC 4		; increment addr for max length field
	LOADI
	CBRANCH.NZ STRIN_XT2 ; if not zero, already initialized
	LOADC 0	; return 0
	RET
STRIN_XT1:
	DROP	; remove addr
STRIN_XT2:
	LOADC 1 ; return 1
	RET

; --- initialise a string if it is not already initialized
; parameters: [ length, addr ]
_INITSTRING:
	DUP
	LOADCP _STRINGISINITED
	CALL		; check if string is already initialized
	CBRANCH.Z _INITSTRINGF
	DROP		; if yes, drop args and return immediately
	DROP
	RET

; --- initialise a string, do not check if it is already
; initialized.
; parameters: [ length, addr ]
_INITSTRINGF:
	LOADC 0	; [ length, addr, 0 ]
	STOREI 4 ; set length to 0, store with post-increment [ length, addr + 4 ]
	SWAP	; swap addr and length  [ addr + 4, length ]
	STOREI	4; store max length, post-increment addr by 4  [ addr + 8 ]
	LOADC 0	; [addr + 8, 0 ]
	STOREI	; zero first word [ addr + 8 ]
	DROP	; drop addr from STOREI
	RET

; --- initialise string from another
; parameters: [ dest, src, length ]
_INITSTRINGFROM:
	FPADJ -4
	STORE 0			; [ dest, src ]
	OVER			; [ dest, src, dest ]
	LOAD 0			; [ dest, src, dest, length ]
	SWAP			; [ dest, src, length, dest ]
	LOADCP _INITSTRINGF
	CALL			; [ dest, src ]
	LOADCP _COPYSTRING
	CALL			; [ ]
	FPADJ 4
	RET

; --- copy a pascal string to another
; parameters: [ dest, src ]
	.EQU CPSTR_DST 0
	.EQU CPSTR_SRC 4
	.EQU CPSTR_DSTMAX 8
	.EQU CPSTR_SRCLEN 12
	.EQU CPSTR_FS 16
_COPYSTRING:
	FPADJ -CPSTR_FS
	SWAP	; get dest addr first
	DUP		; dup pointer for LOADI
	STORE CPSTR_DST ; destination pointer
	INC 4	; increment pointer
	LOADI	; load max length of destination
	STORE CPSTR_DSTMAX ; and store into local var
	DUP		; dup src pointer for LOADI
	STORE CPSTR_SRC ; source pointer
	LOADI	; load length of source
	DUP     ; duplicate length for CMPU below
	STORE CPSTR_SRCLEN ; and store into local var
	LOAD CPSTR_DSTMAX  ; if dest max size is zero,
	CBRANCH.NZ CPSTR_0  ; throw a runtime error
	LOADCP _ERRMSG_CONSTWR
	LOADCP _RUNTIME_ERR
	JUMP
CPSTR_0:
	LOAD CPSTR_DSTMAX
	; if dest.max_length < src.length, use dest.max_length as src.length
	CMPU LE			; src.length <= dest.max_length?
	CBRANCH CPSTR_1
	LOAD CPSTR_DSTMAX
	STORE CPSTR_SRCLEN
CPSTR_1:
	; set dest.length to src.length
	LOAD CPSTR_DST  ; load dst addr
	DUP				; dup for INC/STORE below
	LOAD CPSTR_SRCLEN
	STOREI			; store src length to dst length
	DROP			; drop addr from STOREI
	INC 8			; skip length and max length words
	STORE CPSTR_DST
	LOAD CPSTR_SRC  ; skip length and max length words
	INC 8
	STORE CPSTR_SRC

	; copy ((src.length+3) div 4) words from src to dest
	LOAD CPSTR_SRCLEN ; calculate number of words
	LOADC 3
	ADD
	SHR
	SHR
CPSTR_L1:
	DUP				; duplicate word counter
	CBRANCH.Z CPSTR_LE ; if it is zero, exit loop
	LOAD CPSTR_DST	; load dst addr for STOREI below
	LOAD CPSTR_SRC	; load src addr
	LOADI        	; load value
	STOREI 4		; store value, increment dst addr
	STORE CPSTR_DST ; store new dst addr
	LOAD CPSTR_SRC
	INC 4			; increment src addr
	STORE CPSTR_SRC ; and store it
	DEC 1			; decrement word counter
	BRANCH CPSTR_L1
CPSTR_LE:
	; mask bytes in last word depending on length modulo 4:
	; 0: $00000000
	; 1: $FF000000
	; 2: $FFFF0000
	; 3: $FFFFFF00
	LOAD CPSTR_DST
	DEC 4			; get pointer to last word
	LOADI.X2Y.S1	; load value of last word, keep addr
	LOAD CPSTR_SRCLEN ; length of string
	LOADC 3
	AND		; modulo 4
	SHL 2		; *4
	LOADCP CPSTR_MASK ; + addr of mask table
	ADD
	LOADI			; load mask value
	AND
	STOREI			; store masked value
	DROP			; remove addr from STOREI
	DROP			; drop word counter
CPSTR_XT:
	FPADJ CPSTR_FS
	RET
CPSTR_MASK:
	.WORD $FFFFFFFF, $FF000000, $FFFF0000, $FFFFFF00

	.CPOOL

; --- append a pascal string to another
; parameters: [ dest, src ]
_APPENDSTRING:
	.EQU APSTR_SRCLEN 0
	.EQU APSTR_DSTLEN 4
	.EQU APSTR_SRC 8
	.EQU APSTR_DST 12
	.EQU APSTR_DSTMAX 16
	.EQU APSTR_FS 20
	FPADJ -APSTR_FS
	LOADI.X2Y.S1	; load src length, keep addr
	STORE APSTR_SRCLEN ; store it to local var
	INC 8			; increment addr to point to first char/word
	STORE APSTR_SRC ; store it to local var
	LOADI.X2Y.S1	; load dest length, keep addr
	STORE APSTR_DSTLEN ; store it to local var
	INC 4			; increment addr to point to dest max length
	LOADI.X2Y.S1	; load it, keep addr
	STORE APSTR_DSTMAX ; store it to local var
	INC 4			; increment addr to point to first char/word
	STORE APSTR_DST ; store it to local var

	LOAD APSTR_DSTMAX  ; if dest max size is zero,
	CBRANCH.NZ APSTR_0  ; throw a runtime error
	LOADCP _ERRMSG_CONSTWR
	LOADCP _RUNTIME_ERR
	JUMP

APSTR_0:
	; if src.length + dest.length <= dest.maxlength
	;   use src.length as number of characters
	; else
	;   use dest.maxlength - dest.length as number of characters

	LOAD APSTR_SRCLEN ; char counter
	DUP
	LOAD APSTR_DSTLEN
	ADD
	LOAD APSTR_DSTMAX
	CMPU LE
	CBRANCH APSTR_1
	DROP			; drop char counter
	LOAD APSTR_DSTMAX
	LOAD APSTR_DSTLEN
	SUB				; calculate dest.maxlength - dest.length as char counter
APSTR_1:
	; set dest.length to dest.length + number of characters
	LOAD APSTR_DST 
	DEC 8			; get pointer to dest.length for STOREI
	OVER 			; duplicate char counter
	LOAD APSTR_DSTLEN
	ADD				; dest.length = dest.length + char counter
	STOREI
	DROP			; drop STOREI addr, char counter is now on top of stack
	; set dest pointer to start + dest.length
	LOAD APSTR_DST
	LOAD APSTR_DSTLEN
	ADD
	STORE APSTR_DST
APSTR_L0:
	; while number of characters > 0:
	DUP	; duplicate number of remaining characters
	CBRANCH.Z APSTR_3	; if zero, exit loop
	DEC 1				; decrement char counter

	;   load src value, byte select by src addr
	LOAD APSTR_SRC
	LOADI.X2Y.S1
	BSEL
	LOAD APSTR_DST		; load dest pointer
	LOADCP APPENDCHAR_U
	CALL

	;   increment src and dest pointer
	LOAD APSTR_DST
	INC 1
	STORE APSTR_DST
	LOAD APSTR_SRC
	INC 1
	STORE APSTR_SRC
	BRANCH APSTR_L0
APSTR_3:
	DROP ; remove char counter
	FPADJ APSTR_FS
	RET

; write four bytes from a word to console, msb first
; parameters: [ word value ]
CONOUTW:
	BROT	; rotate msb to lsb
	DUP
	LOADC 255 ; mask out all other bytes
	AND
	LOADCP CONOUT
	CALL

	BROT	; rotate msb to lsb
	DUP
	LOADC 255
	AND
	LOADCP CONOUT
	CALL

	BROT	; rotate msb to lsb
	DUP
	LOADC 255
	AND
	LOADCP CONOUT
	CALL

	BROT	; rotate msb to lsb
	LOADC 255
	AND
	LOADCP CONOUT
	CALL

	RET

; read four bytes from console to a word
; returns word on estack
CONINW:
	LOADCP CONIN
	CALL

	BROT
	LOADCP CONIN
	CALL
	OR

	BROT
	LOADCP CONIN
	CALL
	OR

	BROT
	LOADCP CONIN
	CALL
	OR
	RET

; put a byte-sized character into a word.
; this is a utility route for APPENDSTRING, no bounds checking is done.
; bytes in the word after this character are cleared.
; example: destination word is 'ABCD', addr points to word addr +1,
;			character is 'X', result: word contains 'AX\0\0'
; parameters [ character, byte pointer ]
APPENDCHAR_U:
	FPADJ -4
	DUP
	STORE 0
	SWAP	; [ ptr, char ]
	BPLC	; [ rotated char ]
	LOAD 0	; [ rot char, ptr ]
	LOADI	; load word value
	LOAD 0	; calculate mask value by addr modulo 4
	LOADC 3
	AND
	SHL 2
	LOADCP _APPENDMASK
	ADD
	LOADI	; load mask
	AND	; mask word

	OR 	; OR masked dest value with rotated char value
	LOAD 0 	; get word addr again
	SWAP	; swap for STOREI
	STOREI
	DROP	; remove addr from STOREI
	FPADJ 4
	RET
_APPENDMASK: .WORD $00000000, $FF000000, $FFFF0000, $FFFFFF00

; Append a character to a pascal string.
; parameters: [ string ptr, char value ]
; Does a bounds check, so it can be called from Pascal.
APPENDCHAR:
	SWAP	; put string ptr first
	LOADI.S1.X2Y ; get current length, keep ptr
	OVER
	INC 4
	LOADI	; get maximum length
	CMPU LT ; if current length > max length, we can add a character
	CBRANCH.NZ APPENDCHAR_1

	;if not, check for attempt to write to a string constant
	; (which has max length 0)
	NIP	;  we don't need the char value anymore, the string ptr is ToS
	INC 4	; last time we use str ptr, so no DUP
	LOADI	; get max length
	CBRANCH.NZ APPENDCHAR_0

	LOADCP _ERRMSG_CONSTWR
	LOADCP _RUNTIME_ERR
	JUMP
APPENDCHAR_0:
	; if the string is already at max length,
	; we do nothing, analogous to the + operator
	RET

APPENDCHAR_1:	; on stack here: [ char value, str ptr ]
		; calculate byte ptr from str ptr
	LOADI.S1.X2Y	; get length, keep str ptr
	INC 1	; increase length by 1
	STOREI  ; store new length
	LOADI.S1.X2Y ; load it again; keep ptr
	ADD	; add length to str ptr
	INC 8-1	; byte ptr starts at 0, so adjust for header size - 1
	LOADCP APPENDCHAR_U
	JUMP

; compare two pascal strings for equality.
; the last word must be padded with null bytes
; if string length is not divisible by 4
; (which is ensured by _INITSTRING/APPENDSTRING).
; parameters: [ string1, string2 ]
; returns: 0 if not equal, 1 if equal
_CMPSTRING:
	.EQU CMPSTR_S1 0
	.EQU CMPSTR_S2 4
	.EQU CMPSTR_FS 8
	FPADJ -CMPSTR_FS
	STORE CMPSTR_S2		; store s2 string pointer
	STORE CMPSTR_S1		; store s1 string pointer
	LOAD CMPSTR_S1
	LOADI				; load s1.length
	LOAD CMPSTR_S2
	LOADI				; load s2.length
	CMPU NE				; if string lengths are not the same, 
	CBRANCH CMPSTR_XT_NE1 ; strings are not equal

	LOAD CMPSTR_S1
	LOADI.X2Y.S1		; load s1 length again, keep addr
	INC 3				; calculate number of words to compare:
	SHR					; count = (length + 3) div 4
	SHR					
	SWAP				; put addr on top
	INC 8				; increment addr to skip length fields
	STORE CMPSTR_S1		; store new addr
	LOAD CMPSTR_S2		; skip length fields for s2 pointer too
	INC 8
	STORE CMPSTR_S2
	; word counter is now on top of stack
CMPSTR_L0:
	DUP					; if word counter is zero, strings are equal
	CBRANCH.Z CMPSTR_XT_EQ
	LOAD CMPSTR_S1		; load current word of s1 and s2
	LOADI
	LOAD CMPSTR_S2
	LOADI
	CMPU NE
	CBRANCH CMPSTR_XT_NE    ; if not equal, strings are not equal
	LOAD CMPSTR_S1		; increment s1 pointer
	INC 4
	STORE CMPSTR_S1
	LOAD CMPSTR_S2		; increment s2 pointer
	INC 4
	STORE CMPSTR_S2
	DEC 1				; decrement word counter
	BRANCH CMPSTR_L0	; loop
CMPSTR_XT_EQ:
	DROP		; remove word counter
	LOADC 1		; return 1
	BRANCH CMPSTR_XT
CMPSTR_XT_NE:
	DROP		; remove word counter
CMPSTR_XT_NE1:
	LOADC 0		; return 0
CMPSTR_XT:
	FPADJ CMPSTR_FS
	RET

; compare two pascal strings lexicographically
; parameters: [ pointer to string a, pointer to string b ]
; returns: -1 if a < b, 0 if a = b, 1 if a > b
; NOTE: does not really do a lexicographic comparison, but
; 	a byte-wise numeric comparison. this works for ascii
;	but not for other characters like umlauts etc.
;	requires that the unused bytes in the last word
;	are set to zero (which is guaranteed by _INITSTRING
;	and APPENDSTRING).

	.EQU CMPSTRL_A_COUNT 0
	.EQU CMPSTRL_B_COUNT 4
	.EQU CMPSTRL_A 8
	.EQU CMPSTRL_B 12
	.EQU CMPSTRL_OFFS 16
	.EQU CMPSTRL_RESULT 20
	.EQU CMPSTRL_FS 24
_CMPSTRINGL:
	FPADJ -CMPSTRL_FS
	STORE CMPSTRL_B
	STORE CMPSTRL_A
	LOAD CMPSTRL_A	; load ptr to A
	LOADI		; get length field
	INC 3		; add wordsize - 1
	SHR
	SHR		; divide by 4 to get length in words
	STORE CMPSTRL_A_COUNT
	LOAD CMPSTRL_B	; load ptr to B
	LOADI		; get length field
	INC 3		; add wordsize - 1
	SHR
	SHR		; divide by 4 to get length in words
	STORE CMPSTRL_B_COUNT

	LOADC 8
	STORE CMPSTRL_OFFS ; offset for both strings, start after string header

CMPSTRL_L:			; loop start
	LOAD CMPSTRL_A_COUNT
	CBRANCH.NZ CMPSTRL_1 ; if word count A if zero:
	LOAD CMPSTRL_B_COUNT
	CBRANCH.NZ CMPSTRL_1A ; and word count B is zero,
	LOADC 0
	BRANCH CMPSTRL_XT  ; strings are equal
CMPSTRL_1A:
			; word count A is zero, word count B is not zero: A < B
	LOADC -1
	BRANCH CMPSTRL_XT
CMPSTRL_1:
	LOAD CMPSTRL_B_COUNT
	CBRANCH.NZ CMPSTRL_2 ; if word count B is zero (and word count A cannot be zero here)
	LOADC 1
	BRANCH CMPSTRL_XT ; A > B
CMPSTRL_2:		; both strings have remaining words to compare here
	LOAD CMPSTRL_OFFS
	LOAD CMPSTRL_A ; word address = A ptr + offset
	ADD
	LOADI		; load word from A
	LOAD CMPSTRL_OFFS ; word address = B ptr + offset
	LOAD CMPSTRL_B
	ADD
	LOADI		; load word from B
	OVER
	OVER		; duplicate both words
	CMPU LT		; check if word from A < word from B
	CBRANCH.Z CMPSTRL_2A
	DROP
	DROP
	LOADC -1	; result code for A < B
	BRANCH CMPSTRL_XT
CMPSTRL_2A:
	CMPU GT		; check if word from A > word from B
	CBRANCH CMPSTRL_2B
	; nope, they are equal
	; continue loop: decrease counters, increase offset
	LOAD CMPSTRL_A_COUNT
	DEC 1
	STORE CMPSTRL_A_COUNT
	LOAD CMPSTRL_B_COUNT
	DEC 1
	STORE CMPSTRL_B_COUNT
	LOAD CMPSTRL_OFFS
	INC 4
	STORE CMPSTRL_OFFS
	BRANCH CMPSTRL_L
CMPSTRL_2B:
	LOADC 1	; result code for A > B
CMPSTRL_XT:
	FPADJ CMPSTRL_FS
	RET

	.CPOOL

; create a string from a character
; requires a pointer to a buffer of 12 bytes (minimum string buffer size)
; parameters: [ char value, pointer to string buffer ]
_CHARTOSTRING:
	LOADC 1
	STOREI 4 ; store 1 to length field, post-increment addr
	LOADC 1
	STOREI 4 ; store 1 to max length field, post-increment addr
	SWAP	; swap addr and char value, char is now on ToS
	LOADC 255 ; mask lsb of word
	AND
	BROT	; rotate lsb to msb
	BROT
	BROT
	STOREI ; store it
	DROP
	RET

; convert string and index to a byte pointer.
; does a bounds check.
; parameters [ str ptr, char index ]
; returns: byte ptr
_INDEXSTRING:
	DEC 1	; adjust to base 0, string indices start at 1
	OVER	; [ str, index, str ]
	LOADI	; [ str, index, length ]
	CMPU.S0 GE ; [ str, index, cmp ]
	CBRANCH INDEXSTRING_ERR ; [ str, index ]
	INC 8	; account for string header
	ADD	; add index to ptr to get byte ptr
	RET
INDEXSTRING_ERR:
	DROP
	DROP
	LOADCP _ERRMSG_STRIDX
	LOADCP _RUNTIME_ERR
	JUMP

; set a char at a specific index in a string.
; expects a byte pointer and a char value.
; the byte pointer contains the byte address in bits 0-1.
; no bounds check is performed.
; parameters: [ byte ptr, char value ]
_SETSTRINGCHAR:
	BPLC.S0	; [ ptr, rotated char ]
	OVER	; [ ptr, rot char, ptr ]
	LOADC $FF ;[ptr, rot char, ptr, $FF]
	BPLC.S0	; [ ptr, rot char, ptr, ~mask ]
	NOT	; [ ptr, rot char, ptr, mask ]
	SWAP	; [ ptr, rot char, mask, ptr ]
	LOADI	; [ ptr, rot char, mask, word ]
	AND	; [ ptr, rot char, masked word ]
	OR	; [ ptr, new word ]
	STOREI	; [ ptr ]
	DROP
	RET

; convert string to char
; string must have length of 1, otherwise a runtime error occurs
; parameters [ pointer to string ]
; returns: char value
_STRINGTOCHAR:
	DUP	; dup addr for later
	LOADI	; load string length
	LOADC 1 ; compare with 1
	CMPU EQ
	CBRANCH STRINGTOCHAR_1
	DROP
	LOADCP _ERRMSG_STR2CHAR ; if not 1, issue runtime error
	LOADCP _RUNTIME_ERR
	JUMP
STRINGTOCHAR_1:
	INC 8	; increment addr to skip string header
	LOADI	; load first string word
	BROT	; rotate msb byte to lsb
		; which is the first char/byte of the string.
		; since the last string word is always zero-padded,
		; we dont need to mask the byte
	RET

; set the current length of a pascal string.
; must be less or equal than the maximum length of the string.
; if the new length is larger than the old length, the string
; is padded with zero bytes.
; parameters: [ pointer to string, new length ]
	.EQU SETSTRL_NEWLEN	0
	.EQU SETSTRL_PTR	4
	.EQU SETSTRL_FS		8
_SETSTRINGLENGTH:
	FPADJ -SETSTRL_FS
	DUP
	STORE SETSTRL_NEWLEN
	SWAP			; [ newlen, ptr ]
	DUP			; [ newlen, ptr, ptr ]
	STORE SETSTRL_PTR	; [ newlen, ptr ]
	INC 4		; skip to max length field [ newlen, ptr+4 ]
	LOADI		; load max length [ newlen, maxlen ]
	CMPU.S1.X2Y GE	; new length > max_length?  [ newlen, maxlen, cmp_result ]
	CBRANCH.Z SETSTRL_0_0 ; if not, skip the following [ newlen, maxlen ]
			; if it is, clamp new length to max length
	NIP		; [ maxlen ]
	DUP		; [ maxlen, maxlen ]
	STORE SETSTRL_NEWLEN ; store maxlen as newlen [ ]
	BRANCH SETSTRL_0
SETSTRL_0_0:
	DROP		; remove maxlen
SETSTRL_0:		; newlen is on stack here
	LOAD SETSTRL_PTR ; load ptr to string, size field
			; stack is now: [ newlen, ptr ]
	; loop from addr + old_length - 1
	;	to addr + new_length - 1:
	;   SETSTRINGCHAR(a, chr(0))
	LOADI		; load current length	     [ newlen, oldlen ]
	CMPU LE		; new length <= old length?
	CBRANCH SETSTRL_1 ; if yes, skip padding

	; string is being expanded, need to pad with zero bytes
	LOAD SETSTRL_NEWLEN ; load new length
	LOAD SETSTRL_PTR ; load string pointer yet again
	LOADI		; load current(old) length
	SUB		; calculate length difference (old - new)
			; this is out char counter
	LOAD SETSTRL_PTR ; load string pointer yet again
	LOADI.S1.X2Y    ; load old string length
	ADD		; add to pointer
	INC 8		; adjust for string header

SETSTRL_0_L:
	DUP		; duplicate pointer
	LOADC 0		; zero arg
	LOADCP _SETSTRINGCHAR
	CALL

	INC 1		; increment pointer
	SWAP		; swap pointer and counter
	DEC 1		; decrement counter
	DUP
	CBRANCH.Z SETSTRL_1_0 ; if counter is zero, end loop
	SWAP		; swap pointer and counter again
	BRANCH SETSTRL_0_L
SETSTRL_1_0:
	DROP
	DROP		; remove pointer and counter
SETSTRL_1:		; pad last word of string with zero bytes
	LOAD SETSTRL_PTR
	INC 8		; get pointer to first word
	LOAD SETSTRL_NEWLEN
	ADD		; get pointer to last word
	LOADI.S1.X2Y	; load word, keep addr
	OVER		; duplicate addr
	LOADC 3
	AND		; addr modulo 4
	SHL 2		; * 4 = offset into table borrowed from COPYSTRING
	LOADCP CPSTR_MASK
	ADD		; add up to addr of mask
	LOADI		; load mask
	; now on stack: [ word ptr, word, word mask ]
	AND		; apply mask [ word ptr, word AND mask ]
	STOREI		; store new word
	DROP		; remove addr from STOREI

SETSTRL_XT2:
	LOAD SETSTRL_PTR	; store new length in the length field
	LOAD SETSTRL_NEWLEN	; of the string
	STOREI
	DROP

	FPADJ SETSTRL_FS
	RET

; check if a char value is contained
; within a string.
; parameters: [ value, ptr to string ]
; returns: nonzero if string contains value, 0 if not
_ISCHARINSTRING:
	FPADJ -4
	SWAP
	STORE 0		; store value to find
	LOADI.S1.X2Y	; load string length
	SWAP		; [ length, ptr ]
	INC 8		; skip string header
	SWAP		; [ ptr+8, length ]
ISCHIS_L:
	DUP 		; check if bytes remaining
	CBRANCH.Z ISCHIS_XT0 ; if zero, exit
	SWAP		; [ length, ptr+n ]
	DUP		; [ length, ptr+n, ptr+n ]
	LOADI.S1.X2Y	; [ length, ptr+n, ptr+n, word ]
	BSEL		; [ length, ptr+n, char ]
	LOAD 0
	CMP EQ		; compare with char value
	CBRANCH ISCHIS_FND ; found it
			; [ length, ptr+n ]
	INC 1		; increment byte ptr
	SWAP		; [ ptr+n, length ]
	DEC 1		; decrement counter
	BRANCH ISCHIS_L
ISCHIS_FND:
	LOADC 1
	BRANCH ISCHIS_XT
ISCHIS_XT0:
	LOADC 0
ISCHIS_XT:
	NIP	; remove ptr
	NIP	; and counter, keeping the result code
	FPADJ 4
	RET

; check if an integer value is contained
; within an array of integers
; parameters: [ value, ptr to array, size in words ]
; returns: 1 if  array contains value, 0 if not
_ISINTINARRAY:
	FPADJ -4
	SWAP	; swap size and array
	STORE 0	; store ptr
ISINTINA_L:
	DUP			; duplicate word counter
	CBRANCH.Z ISINTINA_XT0	; if counter is zero, exit
	OVER			; copy value to top of stack
	LOAD 0			; load ptr
	INC.S1.X2Y 4		; increment ptr, keep old value on stack
	STORE 0			; store new ptr
	LOADI			; load array element via old ptr
	CMP NE			; compare value and array element
	CBRANCH ISINTINA_1	; if not equal, skip the following
	DROP
	DROP			; drop counter and value
	LOADC 1			; we found our value, load 1 as return value
	BRANCH ISINTINA_XT	; and branch to exit
ISINTINA_1:
	DEC 1			; decrease counter
	BRANCH ISINTINA_L	; and loop
ISINTINA_XT0:
	DROP			; drop counter and value
	DROP
	LOADC 0			; return 0
ISINTINA_XT:
	FPADJ 4
	RET

; Set a bit in a word. The bit is specified by index (0-31)
; where 0 indicates the lsb and 31 the msb.
; parameters: [ word, bit number ]
; returns: word with bit set
_SETBIT:
	SHL 2	; bit number * 4 = offset into table
	LOADCP _BITTABLE
	ADD
	LOADI	; get bit value
	OR	; combine original value and shifted bit
	RET

; Clear a bit in a word. The bit is specified by index (0-31)
; where 0 indicates the lsb and 31 the msb.
; parameters: [ word, bit number ]
; returns: word with bit cleared
_CLEARBIT:
	SHL 2	; bit number * 4 = offset into table
	LOADCP _BITTABLE
	ADD
	LOADI	; get bit value
	NOT	; invert mask
	AND	; combine original value and shifted bit
	RET

; test if a bit is set a word. The bit is specified by index (0-31)
; where 0 indicates the lsb and 31 the msb.
; parameters: [ bit number, word ]
; returns: 1 if bit is set, 0 otherwise
_TESTBIT:
	SWAP
	SHL 2	; bit number * 4 = offset into table
	LOADCP _BITTABLE
	ADD
	LOADI	; get bit value
	AND	; combine original value and shifted bit
	LOADC 0
	CMP NE ; if not zero, return 1
	RET

; Convert an integer array to a 32-bit set, by
; iterating over all array values and calling SETBIT.
; parameters: [ array ptr, array size in words ]
; returns: set value as a single word
_ARRAYTOSET:
	FPADJ -4 ; local var offset 0 is the result value
	LOADC 0
	STORE 0 ; clear result value
ARRAYTOSET_L:
	DUP	; duplicate word counter
	CBRANCH.Z ARRAYTOSET_X	; if zero, we are done
	SWAP	; swap counter and ptr, ptr is on ToS
	LOADI.S1.X2Y ; load value from array, keep ptr
	LOADC 31
	AND	; clamp bit number to 31
	LOAD 0	; load result value
	SWAP	; SETBIT wants [ value, bit number ]
	LOADCP _SETBIT ; set the bit
	CALL
	STORE 0	; store result
	INC 4	; increment ptr
	SWAP	; swap ptr and counter again
	DEC 1	; decrement counter
	BRANCH ARRAYTOSET_L ; and loop
ARRAYTOSET_X:
	DROP	; remove word counter
	DROP	; remove ptr
	LOAD 0	; load result
	FPADJ 4
	RET

_BITTABLE:
	.WORD %00000000000000000000000000000001
	.WORD %00000000000000000000000000000010
	.WORD %00000000000000000000000000000100
	.WORD %00000000000000000000000000001000
	.WORD %00000000000000000000000000010000
	.WORD %00000000000000000000000000100000
	.WORD %00000000000000000000000001000000
	.WORD %00000000000000000000000010000000
	.WORD %00000000000000000000000100000000
	.WORD %00000000000000000000001000000000
	.WORD %00000000000000000000010000000000
	.WORD %00000000000000000000100000000000
	.WORD %00000000000000000001000000000000
	.WORD %00000000000000000010000000000000
	.WORD %00000000000000000100000000000000
	.WORD %00000000000000001000000000000000
	.WORD %00000000000000010000000000000000
	.WORD %00000000000000100000000000000000
	.WORD %00000000000001000000000000000000
	.WORD %00000000000010000000000000000000
	.WORD %00000000000100000000000000000000
	.WORD %00000000001000000000000000000000
	.WORD %00000000010000000000000000000000
	.WORD %00000000100000000000000000000000
	.WORD %00000001000000000000000000000000
	.WORD %00000010000000000000000000000000
	.WORD %00000100000000000000000000000000
	.WORD %00001000000000000000000000000000
	.WORD %00010000000000000000000000000000
	.WORD %00100000000000000000000000000000
	.WORD %01000000000000000000000000000000
	.WORD %10000000000000000000000000000000

	.CPOOL

	.EQU _HEAP_HDR_SZ 8
	.EQU _HEAP_MIN_SZ 32
; MEM_ALLOC
; allocate a chunk of memory
; parameters: [ size in bytes ]
; returns: starting address of allocated memory, or 0 if
;		no more space is available
	.EQU MA_REQD_SIZE 0
	.EQU MA_CURCHUNK 4
	.EQU MA_LASTCHUNK 8
	.EQU MA_CURCHUNK_SIZE 12
	.EQU MA_NEWCHUNK 16
	.EQU MA_FS 20
_MEM_ALLOC:
	FPADJ -MA_FS
	; round up requested size
	LOADC _HEAP_HDR_SZ
	ADD
	LOADC _HEAP_MIN_SZ -1
	ADD
	LOADC ~_HEAP_MIN_SZ + 1
	AND
	STORE MA_REQD_SIZE

	;LOAD MA_REQD_SIZE
	;LOADCP PRINTDEC
	;CALL
	;LOADCP NEWLINE
	;CALL

	; get current alloc pointer (points to last previously seen free chunk)
	LOADCP _HEAP_CUR
	LOADI
	STORE MA_CURCHUNK
MEM_ALLOC_L0:
	; get next free chunk
	LOAD MA_CURCHUNK
	DUP
	STORE MA_LASTCHUNK	; last chunk = cur chunk
	LOADI			; load next ptr
	DUP			; dup ptr for accessing the size
	STORE MA_CURCHUNK 	; cur chunk = cur chunk^.next
	INC 4			; skip to size field
	LOADI			; load it
	STORE MA_CURCHUNK_SIZE	; and store to local var

	;LOADC ':'
	;LOADCP CONOUT
	;CALL

	;LOAD MA_CURCHUNK
	;LOADCP PRINTHEXW
	;CALL

	;LOADCP 's'
	;LOADCP CONOUT
	;CALL

	;LOAD MA_CURCHUNK_SIZE
	;LOADCP PRINTHEXW
	;CALL

	;LOADCP '?'
	;LOADCP CONOUT
	;CALL

	;LOAD MA_REQD_SIZE
	;LOADCP PRINTHEXW
	;CALL

	;LOADCP NEWLINE
	;CALL

	; if requested size is less or equal than chunk size, jump to allocation
	LOAD MA_REQD_SIZE
	LOAD MA_CURCHUNK_SIZE
	CMPU LE
	CBRANCH MEM_ALLOC_A
	; if back to where we started, return failure
	LOADCP _HEAP_CUR
	LOADI
	LOAD MA_CURCHUNK
	CMPU NE
	CBRANCH MEM_ALLOC_L0 ; else, go to next chunk

	;LOADC 'e'
	;LOADCP CONOUT
	;CALL

	; TODO: grow heap and add new chunk, check for
	; collision with downward growing user stack (FP)

	LOADC 0
	BRANCH MEM_ALLOC_XT

	; allocation:
MEM_ALLOC_A:
	LOAD MA_CURCHUNK
	STORE MA_NEWCHUNK

	LOADCP _HEAP_CUR	; remember last chunk
	LOAD MA_LASTCHUNK	; for next invocation
	STOREI
	DROP
	; split chunk:
	;	skip if requested_size = chunk size
	LOAD MA_CURCHUNK_SIZE
	LOAD MA_REQD_SIZE
	CMPU EQ
	CBRANCH MEM_ALLOC_A0
	;	new chunk is head portion of current chunk
	;	new chunk size = requested size
	LOAD MA_NEWCHUNK
	INC 4 ; skip zo size field
	LOAD MA_REQD_SIZE
	STOREI   ; store size
	DROP

	;LOADC '|'
	;LOADCP CONOUT
	;CALL

	; current chunk is the split off free chunk
	;	current chunk start += requested size
	;	current chunk size -= requested size
	LOAD MA_CURCHUNK_SIZE
	LOAD MA_REQD_SIZE
	SUB	; new size = old size - requested
	LOAD MA_CURCHUNK
	LOADI  ; load old next ptr

	LOAD MA_CURCHUNK
	LOAD MA_REQD_SIZE
	ADD	; new addr of current chunk = old addr + requested size
	DUP
	STORE MA_CURCHUNK
	; the stack at this point: [ new size, old next ptr, cur chunk addr ]
	; now write new header
	SWAP		; swap next ptr and addr
	STOREI 4	; store next ptr to addr, leave addr + 4
	SWAP		; swap addr and size
	STOREI		; store size
	DROP

	; previous chunk next pointer = current chunk pointer
	LOAD MA_LASTCHUNK
	LOAD MA_CURCHUNK
	STOREI
	DROP
	BRANCH MEM_ALLOC_A1

MEM_ALLOC_A0:
	;LOADC 'O'
	;LOADCP CONOUT
	;CALL

	; if chunk was not split: previous chunk next ptr = current chunk next ptr
	LOAD MA_LASTCHUNK
	LOAD MA_CURCHUNK
	LOADI
	STOREI
	DROP

MEM_ALLOC_A1:
	;	return new chunk pointer + header size
	LOAD MA_NEWCHUNK
	INC _HEAP_HDR_SZ

MEM_ALLOC_XT:
	FPADJ MA_FS
	RET

; MEM_FREE
; free a chunk of memory which was allocated by MEM_ALLOC
; parameters: [ address previously returned by MEM_ALLOC ]
	.EQU MF_FREECHUNK 0
	.EQU MF_CURCHUNK 4
	.EQU MF_NEXTCHUNK 8
	.EQU MF_CURCHUNK_END 12
	.EQU MF_FS	16
_MEM_FREE:
	FPADJ -MF_FS
	DEC _HEAP_HDR_SZ
	DUP	; dup for comparison below	[ ptr, ptr ]
	DUP	; dup for other comparison below [ ptr, ptr, ptr ]
	STORE MF_FREECHUNK ; to-be-freed chunk pointer = address - header size  [ptr, ptr ]
	LOADCP _HEAP_ANCHOR ; [ ptr, ptr, anchor ]
	CMP GT			; [ ptr , cmp ]
	CBRANCH MEM_FREE_L	; [ ptr ]

	DUP
	LOADCP PRINTHEXW
	CALL

	DROP

	LOADCP _ERRMSG_MEMFREE
	LOADCP _RUNTIME_ERR
	JUMP
MEM_FREE_L:
	LOADCP _HEAP_CUR
	LOADI	; load current heap pointer
	DUP	; dup for comparison below
	STORE MF_CURCHUNK
			; on the stack now: [ freechunk,  heap_cur ]
	CMPU GE
	CBRANCH MEM_FREE_L0
	LOADCP _HEAP_ANCHOR
	STORE MF_CURCHUNK
MEM_FREE_L0:
	; get current alloc pointer (points to last seen free chunk)
	LOAD MF_CURCHUNK
	DUP
	DUP
	INC 4	; skip curchunk ptr to size field
	LOADI   ; load size
	ADD	; add to curchunk ptr to get end of chunk
	STORE	MF_CURCHUNK_END
	LOADI	 ; get next chunk pointer
	STORE MF_NEXTCHUNK

	;LOADC 'N'
	;LOADCP CONOUT
	;CALL
	;LOAD MF_NEXTCHUNK
	;LOADCP PRINTHEXW
	;CALL
	;LOADC 'C'
	;LOADCP CONOUT
	;CALL
	;LOAD MF_CURCHUNK
	;LOADCP PRINTHEXW
	;CALL
	;LOADC 'F'
	;LOADCP CONOUT
	;CALL
	;LOAD MF_FREECHUNK
	;LOADCP PRINTHEXW
	;CALL
	;LOADCP NEWLINE
	;CALL

        ; if next ptr < to-be-freed, skip to next chunk
	LOAD MF_NEXTCHUNK
	LOAD MF_FREECHUNK
	CMPU LT
	CBRANCH MEM_FREE_CT1

MEM_FREE_INS:
	LOAD MF_FREECHUNK
	LOAD MF_NEXTCHUNK
	CMP NE
	CBRANCH MEM_FREE_INS1

	LOADCP _ERRMSG_MEMDFREE
	LOADCP _RUNTIME_ERR
	JUMP

MEM_FREE_INS1:

	;	to-be-freed chunk next ptr = next chunk
	LOAD MF_FREECHUNK
	LOAD MF_NEXTCHUNK
	STOREI
	DROP

	;	current chunk next ptr = to-be-freed chunk
	LOAD MF_CURCHUNK
	LOAD MF_FREECHUNK
	STOREI
	DROP

	;LOADC 'i'
	;LOADCP CONOUT
	;CALL

	; merge down if needed
	;	if current chunk end = to-be-freed chunk start:
	;		current chunk size += to-be-freed size
	;		to-be-freed chunk = current chunk
	;		current chunk next = next
	LOAD MF_CURCHUNK_END
	LOAD MF_FREECHUNK
	CMPU NE
	CBRANCH MEM_FREE_CT

	;LOADC 'v'
	;LOADCP CONOUT
	;CALL

	LOAD MF_CURCHUNK
	INC 4
	DUP		; dup addr of cur chunk size for STOREI below
	LOADI		; get cur chunk size
	LOAD MF_FREECHUNK
	INC 4
	LOADI		; get to-be-freed chunk size
	ADD
	STOREI		; store new cur chunk size
	DROP

	LOAD MF_CURCHUNK ; set current chunk next pointer again
	LOAD MF_NEXTCHUNK
	STOREI		; store and reuse addr
	STORE MF_FREECHUNK ; to-be-freed chunk becomes current chunk

	; merge up if needed
	;	if to-be-freed chunk end = next chunk start:
	;		to-be-freed-chunk size += next chunk size
	;		to-be-freed next ptr = next chunk next ptr
MEM_FREE_CT:
	LOAD MF_FREECHUNK
	DUP
	INC 4
	LOADI
	ADD		; calculate to-be-freed chunk end
	LOAD MF_NEXTCHUNK
	CMPU NE
	CBRANCH MEM_FREE_CT0

	;LOADC '^'
	;LOADCP CONOUT
	;CALL

	LOAD MF_FREECHUNK ; store next chunk next ptr to to-be-freed next ptr
	LOAD MF_NEXTCHUNK
	LOADI
	STOREI 4	; store and post-increment addr to to-be-freed size field
	LOADI.S1.X2Y	; load size and keep addr
	LOAD MF_NEXTCHUNK
	INC 4
	LOADI		; get next chunk size
	ADD
	STOREI
	DROP

MEM_FREE_CT0:

	BRANCH MEM_FREE_XT

MEM_FREE_CT1:
	;LOADC '>'
	;LOADCP CONOUT
	;CALL
	; if we are at the end of the list, insert there
	LOADCP _HEAP_ANCHOR
	LOAD MF_NEXTCHUNK
	CMPU EQ
	CBRANCH MEM_FREE_INS

	; move to next chunk
	LOAD MF_NEXTCHUNK
	STORE MF_CURCHUNK
	BRANCH MEM_FREE_L0

MEM_FREE_XT:
	; reset current heap pointer because
	; a merge might have invalidated it
	LOADCP _HEAP_CUR
	LOADCP _HEAP_ANCHOR
	STOREI
	DROP

	FPADJ MF_FS
	RET

; MEM_INIT
; Initialize dynamic memory, user stack and return stack.
; Since the return stack is no longer valid afterwards, directly
; jumps to _MAIN instead of using RET.

; parameters: [ start of heap address ]
_MEM_INIT:
	; initialize anchor chunk with start of heap address
	; and heap size - header size
	LOADCP _HEAP_CUR ; load addr of _HEAP_CUR for STOREI below
	LOADCP _HEAP_ANCHOR
	DUP
	DUP	; anchor points to itself at first
	STOREI 4
	LOADC 0 ; store size 0
	STOREI
	DROP
	STOREI  ; store _HEAP_ANCHOR to _HEAP_CUR
	DROP

	LOADCP _HEAP_SZ_PTR	; load the value of the heap size from
	LOADI			; the program header

	; set user stack pointer to heap start + heap size + stack size
	OVER	; [ start, size, start ]
	OVER	; [ start, size, start, size ]
	ADD	; [ start, size, start+size ]
	LOADCP _STACK_SZ_PTR
	LOADI
	ADD	; add user stack size to get new FP value [ start, size, start+heapsize+stacksize]
	STOREREG FP

	LOADREG FP
	INC 4
	STOREREG RP	; set RP to start right after user stack
			; this trashes the previous return stack,
			; so we cannot use RET at the end of MEM_INIT

	; set chunk header
	OVER 	; [ start, size, start ]
	LOADC 0	; [ start, size, start, 0 ]	; set next chunk ptr
	STOREI 4 ; [ start, size , start + 4 ]	; to zero or we get an error in _MEM_FREE
	SWAP	; [ start, start + 4, size ]
	STOREI					; store the size
	DROP	; [ start ]

	LOADC _HEAP_HDR_SZ
	ADD	; adjust chunk address for header

	LOADCP _MEM_FREE ; add chunk to free list
	CALL

	LOADCP _MAIN
	JUMP

; allocate a string with MEM_ALLOC.
; the string is also initialized.
; parameters: [ max length ]
; returns: pointer to allocated string, or zero if
;		no heap space available

_STRING_ALLOC:
	DUP			; [ length, length ]
	INC 8			; adjust length for string header
	LOADCP _MEM_ALLOC
	CALL			; [ length, addr ]
	DUP			; [ length, addr, addr ]
	CBRANCH.Z STRING_ALLOC_E  ; [ length, addr ]
	SWAP			; [ addr, length ]
	OVER			; [ addr, length, addr ]
	LOADCP _INITSTRINGF	; set max string length (forced)
	CALL			; [ addr ]
	RET
STRING_ALLOC_E:
	SWAP
	DROP
	RET

	.CPOOL

LENGTH:
	LOADI
	RET

MAXLENGTH:
	INC 4
	LOADI
	RET

; issue a runtime error if pointer is zero
_CHECK_ALLOC:
	;TODO: check for heap overrun by user stack

	;DUP
	;LOADCP _CHECK_CHUNK ; check for corrupted free list
	;CALL

	DUP
	CBRANCH.NZ _CHECK_ALLOC_OK
	LOADCP _ERRMSG_MEMALLOC
	LOADCP _RUNTIME_ERR
	JUMP
_CHECK_ALLOC_OK:
	RET

; print the free list for debugging
MEM_DUMP:
	LOADC 35
	LOADCP CONOUT
	CALL
	LOADCP _HEAP_ANCHOR
	DUP
	LOADCP PRINTHEXW
	CALL
	LOADC '^'
	LOADCP CONOUT
	CALL
	LOADCP _HEAP_CUR
	LOADI
	LOADCP PRINTHEXW
	CALL
	LOADCP NEWLINE
	CALL
MEM_DUMP_L0:
	DUP	; dup cur ptr for later
	DUP	; dup cur ptr for printing
	LOADCP PRINTHEXW
	CALL
	LOADC ' '
	LOADCP CONOUT
	CALL
	INC 4
	LOADI
	LOADCP PRINTHEXW
	CALL
	LOADC '>'
	LOADCP CONOUT
	CALL
	LOADI	; load next ptr
	DUP
	LOADCP PRINTHEXW
	CALL
	LOADCP NEWLINE
	CALL
	DUP	; dup for comparison
	LOADCP _HEAP_ANCHOR
	CMPU NE	; if next ptr is anchor, we are done
	CBRANCH MEM_DUMP_L0
	DROP
	RET

; check if a pointer is part of the free list
; args: pointer returned by MEM_ALLOC
; throws runtime error if the pointer is found
_CHECK_CHUNK:
	FPADJ -4
	LOADC _HEAP_HDR_SZ	; adjust for header
	SUB
	STORE 0			; store chunk addr in local var

	LOADCP _HEAP_ANCHOR	; start loop with anchor address
CHK_CH_L:
	DUP		; current chunk addr is on stack
	LOADI		; load next ptr
	LOAD 0		; load addr to be checked
	CMP EQ		; if equal, error
	CBRANCH.NZ CHK_ERR

	LOADI		; load next ptr again
	LOADCP _HEAP_ANCHOR
	CMP.S0 EQ	; compare with anchor, keep ptr on stack
	CBRANCH.Z CHK_CH_L ; in not equal, loop

	DROP		; remove current chunk ptr

	FPADJ 4
	RET
CHK_ERR:
	LOAD 0
	LOADCP PRINTHEXW
	CALL
	LOADCP NEWLINE
	CALL

	LOADCP MEM_DUMP
	CALL
	; remove one return stack entry
	; for reporting the correct PC
	; since _CHECK_CHUNK is
	; always called by another checking
	; routine (e.g. _CHECK_ALLOC)
	LOADREG RP
	DEC 4
	STOREREG RP

	LOADCP _ERRMSG_MEMBROKEN
	LOADCP _RUNTIME_ERR
	JUMP

; array bounds check
; parameters [ index, array size ]
; throws a runtime error when not inside bounds
_BOUNDSCHECK:
	CMPU LT
	CBRANCH _BOUNDSCHECK_OK
	LOADCP _ERRMSG_ARRAY_OOB
	LOADCP _RUNTIME_ERR
	JUMP
_BOUNDSCHECK_OK:
	RET

; subrange check
; parameters: [ value, min, max ]
; throws runtime error when not inside bounds
_RANGECHECK:
	FPADJ -4
	STORE 0 ; store max value
	OVER	; dup value, stack is now [ v, min,  v ]
	CMP GT	; if min is greater than value
	CBRANCH _RANGE_ERR ; then it is a runtime error
	LOAD 0	; load max again, stack is now [ v, max ]
	CMP GT	; if value is greater than max
	CBRANCH _RANGE_ERR ; then it is a runtime error
	FPADJ 4
	RET
_RANGE_ERR:
	LOADCP _ERRMSG_RANGE
	LOADCP _RUNTIME_ERR
	JUMP

; enum range check
; parameters: [ value, max ]
; throws runtime error when outside bounds
_ENUMCHECK:
	CMPU GT
	CBRANCH _ENUM_ERR
	RET
_ENUM_ERR:
	LOADCP _ERRMSG_ENUM
	LOADCP _RUNTIME_ERR
	JUMP

HALT:
	LOADCP PTERM
	JUMP

; Show runtime error, to be called from Pascal.
; The string must end in a null byte, for example
; by having a greater maximum string length than
; actual length, or by adding a chr(0) at the end.
; parameters: [ ptr to pascal string ]
RUNTIMEERROR:
	INC 8	; skip string header
	; fall through to _RUNTIME_ERR

; show runtime error message and abort program
; args: error message
_RUNTIME_ERR:
	LOADCP _ERRMSG_1
	LOADCP PRINTLINE
	CALL
	LOADREG RP
	LOADI
	LOADCP PRINTHEXW
	CALL
	LOADCP NEWLINE
	CALL
	LOADCP PRINTLINE
	CALL
	LOADCP NEWLINE
	CALL

	LOADCP PTERM
	JUMP

	.EQU _ESP_EMPTY 0

_CLEARESTACK:
	LOADREG ESP
	LOADC _ESP_EMPTY
	CMP EQ
	CBRANCH _CLEARESTACK_XT
	DROP
	BRANCH _CLEARESTACK
_CLEARESTACK_XT:
	RET

; Terminate program: clear estack and
; jump to coreloader
PTERM:
	LOADCP _CLEARESTACK
	CALL
	LOADCP LOADER_START
	JUMP

; Return an integer representation
; of a real(float32) number.
; This is our native format, so
; we do not have to do anything.
ENCODEFLOAT32:
	RET

	.CPOOL

_HEAP_START:	.WORD 0
_HEAP_MAX:	.WORD 0
_HEAP_CUR:	.WORD 0
_HEAD_FIRSTF:	.WORD 0
_HEAP_ANCHOR:	.WORD 0,0

_ERRMSG_1: .BYTE 13,10,"Runtime error at PC ",0
_ERRMSG_ARRAY_OOB: .BYTE "Array index out of bounds",0
_ERRMSG_CONSTWR: .BYTE "Write to constant string",0
_ERRMSG_STR2CHAR: .BYTE "Invalid conversion from string to char",0
_ERRMSG_STRIDX: .BYTE "String index out of bounds",0
_ERRMSG_RANGE: .BYTE "Range check",0
_ERRMSG_ENUM: .BYTE "Invalid enum value",0
_ERRMSG_MEMFREE: .BYTE "Invalid pointer in dispose",0
_ERRMSG_MEMDFREE: .BYTE "Chunk already disposed",0
_ERRMSG_MEMALLOC: .BYTE "Out of heap space",0
_ERRMSG_MEMBROKEN: .BYTE "Heap corrupted",0
NEWLINESTR:
	.WORD 2,0
	.BYTE 13,10

	.CPOOL
