; Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details

; FLOAT32 format:
;| 31 | 30-8      | 7-0   |
;|sign| fraction  | exp   |

; exp has a bias of 128
; sign = 1: negative number

	.EQU FLOAT32_EXPMASK %00000000000000000000000011111111
	.EQU FLOAT32_FRCMASK %01111111111111111111111100000000
	.EQU FLOAT32_SGNMASK %10000000000000000000000000000000
	.EQU FLOAT32_FRCMSBM %01000000000000000000000000000000
	.EQU FLOAT32_OVRFLOW %10000000000000000000000000000000
	.EQU FLOAT32_RNDMASK %00000000000000000000000010000000
	.EQU FLOAT32_RNDINCR %00000000000000000000000100000000
	.EQU FLOAT32_FRCLSB  %00000000000000000000000100000000

	.EQU FLOAT32_MAX_EXP 255
	.EQU FLOAT32_BIAS 127
	.EQU FLOAT32_FRCBITS 23
	.EQU FLOAT32_EXP_BITS 8

; unpack a float32 value into
; three fields: exponent, fraction, sign
; ptr is a pointer to an array with the three fields
; the sign flag is 0 or 1.

; parameters: [ floatval, ptr ]
_UNPACKFLOAT32:
	OVER			; [ float, ptr, float ]
	LOADC FLOAT32_EXPMASK	; [ float, ptr, float, mask ]
	AND			; [ float, ptr, exp ]
	STOREI 4		; [ float, ptr + 4 ]
	OVER			; [ float, ptr + 4, float ]
	LOADCP FLOAT32_FRCMASK	; [ float, ptr + 4 , float, mask ]
	AND			; [ float, ptr + 4 , frac ]
	STOREI 4		; [ float, ptr + 8 ]
	SWAP			; [ ptr + 8, float ]
	LOADCP FLOAT32_SGNMASK	; [ ptr + 8, float, mask ]
	AND			; [ ptr + 8, signbit ]
	LOADC 0			; [ ptr + 8, signbit, 0 ]
	CMPU NE			; [ ptr + 8, sign ]
	STOREI			; [ ptr + 8 ]
	DROP			; []
	RET

; pack exponent, fraction, sign into a single 32-bit value
; ptr is a pointer to an array with the three fields
; parameters: [ ptr ]
; returns: float value
_PACKFLOAT32:
	LOADI.S1.X2Y	; [ ptr, exp ]
	OVER		; [ ptr, exp, ptr ]
	INC 4		; [ ptr, exp, ptr + 4 ]
	LOADI		; [ ptr, exp, frac ]
	LOADCP FLOAT32_FRCMASK ; [ ptr, exp, frac, mask ]
	AND		; [ ptr, exp, frac ]
	OR		; [ ptr, exp|frac ]
	OVER		; [ ptr, exp|frac, ptr ]
	INC 8		; [ ptr, exp|frac, ptr + 8]
	LOADI		; [ ptr, exp|frac, signflag ]
	CBRANCH.Z PACKFL_1 ; [ ptr, exp | frac ]
	LOADCP FLOAT32_SGNMASK ; [ ptr, exp|frac, signmask ]
	ADD		; [ ptr, floatval ]
PACKFL_1:
	SWAP		; [ floatval, ptr ]
	DROP		; [ floatval ]
	RET

; adds to floating point values a and b
; parameters: [ a, b ]
; returns: sum of a and b
	.EQU FL_E_A 0
	.EQU FL_F_A 4
	.EQU FL_S_A 8
	.EQU FL_E_B 12
	.EQU FL_F_B 16
	.EQU FL_S_B 20
	.EQU FL_E_R 24
	.EQU FL_F_R 28
	.EQU FL_S_R 32
	.EQU ADDFL_SUB 36
	.EQU ADDFL_FS 40

_ADDFLOAT32:
	FPADJ -ADDFL_FS
	; unpack b into e_b and f_b
	LOADREG FP
	LOADC FL_E_B
	ADD		; addr of FL_E_B
	LOADCP _UNPACKFLOAT32
	CALL
	; unpack a into e_a and f_a
	LOADREG FP	; addr of FL_E_A
	LOADCP _UNPACKFLOAT32
	CALL


	; if abs(a) < abs(b), swap a and b (this includes e_a < e_b)
	LOAD FL_E_A
	LOAD FL_E_B
	CMPU GT
	CBRANCH ADDFL_1 ; don't swap if e_a > e_b
	; e_a <= e_b
	; check if e_a < e_b
	LOAD FL_E_A
	LOAD FL_E_B
	CMPU LT
	CBRANCH ADDFL_1_1 ; swap if e_a < e_b
	; e_a = e_b
	; check fractions
	LOAD FL_F_A
	LOAD FL_F_B
	CMPU GE
	CBRANCH ADDFL_1 ; dont't  swap if f_a >= f_b

ADDFL_1_1:
	;LOADC '"'
	;LOADCP CONOUT
	;CALL

	LOAD FL_E_A
	LOAD FL_E_B
	STORE FL_E_A
	STORE FL_E_B
	LOAD FL_F_A
	LOAD FL_F_B
	STORE FL_F_A
	STORE FL_F_B
	LOAD FL_S_A
	LOAD FL_S_B
	STORE FL_S_A
	STORE FL_S_B
ADDFL_1:
	; e_r := e_a, result exp is exp from a
	LOAD FL_E_A
	STORE FL_E_R

	LOAD  FL_S_A	; take result sign from a (which might be swapped with b here)
	STORE FL_S_R

	LOAD FL_S_A	; check if the signs of a and b differ
	LOAD FL_S_B
	XOR	; if the xor of both sign flags is 1, signs are not the same
	STORE ADDFL_SUB ; then we do a subtraction later

	; ? if (e_a - e_b) > precision +1:
	; ?  set f_r to f_a

	; shift right f_b by (e_a - e_b) bits
	LOAD FL_E_A
	LOAD FL_E_B
	SUB

	;LOADCP NEWLINE
	;CALL
	;LOADC 'e'
	;LOADCP CONOUT
	;CALL
	;DUP
	;LOADCP PRINTDEC
	;CALL
	;LOADCP NEWLINE
	;CALL

	LOAD FL_F_B		; on stack now: [ counter, f_b ]
ADDFL_L1:
	OVER			; check counter for zero
	CBRANCH.Z ADDFL_2	; if yes, loop is done
	SHR			; shift right
	SWAP			; swap counter and value
	DEC 1			; decrement counter
	SWAP			; swap back
	BRANCH ADDFL_L1		; next
ADDFL_2:
	STORE FL_F_B		; store result
	DROP			; remove counter

	;LOADC 'a'
	;LOADCP CONOUT
	;CALL
	;LOAD FL_F_A
	;LOADCP PRINTHEXW
	;CALL
	;LOADCP NEWLINE
	;CALL
	;LOADC 'b'
	;LOADCP CONOUT
	;CALL
	;LOAD FL_F_B
	;LOADCP PRINTHEXW
	;CALL
	;LOADCP NEWLINE
	;CALL

	; set f_r to f_a + f_b
	; (or f_r = f_a - f_b if we have a negative sign somewhere)
	LOAD FL_F_A
	LOAD FL_F_B
	LOAD ADDFL_SUB ; do we need a subtract?
	CBRANCH ADDFL_2_1 ; yes, skip add instruction
	ADD		; no, do add and skip sub instruction
	BRANCH ADDFL_2_2
ADDFL_2_1:
	;LOADC '-'
	;LOADCP CONOUT
	;CALL
	;LOADCP NEWLINE
	;CALL

	SUB
ADDFL_2_2:
	;DUP
	;LOADCP PRINTHEXW
	;CALL
	;LOADCP NEWLINE
	;CALL

	; normalize:
	; if overflow bit of f_r is set, scale right
	DUP
	LOADCP FLOAT32_OVRFLOW	; isolate overflow bit
	AND

	CBRANCH.Z ADDFL_3

	;LOADC 'V'
	;LOADCP CONOUT
	;CALL

	;   scale right: shift right f_r  by one, increment e_r by one
	SHR
	LOAD FL_E_R
	INC 1
	STORE FL_E_R
ADDFL_3:
	; check if result is zero
	DUP
	CBRANCH.NZ ADDFL_3_1
	; set exponent to something useful
	LOADC 0
	STORE FL_E_R
	; clear sign, we don't want -0
	LOADC 0
	STORE FL_S_R

	BRANCH ADDFL_4

ADDFL_3_1:
	; if bit 30 of f_r is zero, scale left
	DUP
	LOADCP FLOAT32_FRCMSBM	; isolate msb of fraction
	AND
	CBRANCH.NZ ADDFL_4

	;LOADC '<'
	;LOADCP CONOUT
	;CALL
	;LOADCP NEWLINE
	;CALL

	;   scale left and repeat previous step
	SHL	; shift fraction
	LOAD FL_E_R	; decrement exponent
	DEC 1
	STORE FL_E_R
	BRANCH ADDFL_3	; repeat
ADDFL_4:
	; round:
	; simple round algorithm: if the bit below
	; the least significat bit is 1, round up
	; and check the overflow bit again

	DUP
	LOADC FLOAT32_RNDMASK ; test rounding bit
	AND
	CBRANCH.Z ADDFL_5	; if not set, skip rounding

	;LOADC '~'
	;LOADCP CONOUT
	;CALL
	;LOADCP NEWLINE
	;CALL

	LOADC FLOAT32_RNDINCR
	ADD

	LOADCP FLOAT32_FRCMASK ; remove rounded off bits
	AND

	BRANCH ADDFL_3_1	; check for overflow again
ADDFL_5:

	STORE FL_F_R

	;LOADCP NEWLINE
	;CALL
	;LOADC 'E'
	;LOADCP CONOUT
	;CALL

	;LOAD FL_E_R
	;LOADCP PRINTHEXW
	;CALL
	;LOADCP NEWLINE
	;CALL

	; check exponent range overflow
	LOADCP FLOAT32_EXP_CHECK
	CALL
ADDFL_6:
	; pack e_r,f_r,s_r into r
	LOADREG FP
	LOADC FL_E_R
	ADD
	LOADCP _PACKFLOAT32
	CALL

	FPADJ ADDFL_FS
	RET

; check exponent value of local var FL_E_R
; for overflow and issue runtime error on failure.
; uses stack frame of caller.
FLOAT32_EXP_CHECK:
	; check exponent range overflow
	LOAD FL_E_R
	LOADC FLOAT32_MAX_EXP
	CMPU LE
	CBRANCH FLOAT32_EXP_XT

	LOADCP FLOAT32_ERR_OVR
	LOADCP _RUNTIME_ERR
	JUMP
FLOAT32_EXP_XT:
	RET

; subtract floating point value b from a
; parameters: [ a, b ]
; returns: a - b
_SUBFLOAT32:
	; just flip sign bit of b and continue with _ADDFLOAT32
	LOADCP $80000000
	XOR
	BRANCH _ADDFLOAT32

; multiply two floating point values a and b
; parameters: [ a, b ]
; returns: a * b
	.EQU MULFL_FS	36
_MULFLOAT32:
	FPADJ -MULFL_FS ; same stack frame layout as _ADDFLOAT32

	DUP
	CBRANCH.NZ _MULFLOAT32_1	; check if b is zero
	; if yes, just return zero
	NIP	; remove a, take b (which is zero) as return value
	BRANCH _MULFLOAT32_X
_MULFLOAT32_1:
	; unpack b into e_b and f_b
	LOADREG FP
	LOADC FL_E_B
	ADD		; addr of FL_E_B
	LOADCP _UNPACKFLOAT32
	CALL

	DUP
	CBRANCH.NZ _MULFLOAT32_2 ; check if a is zero
	; if yes, return zero
	; take a (which is zero) as return value
	BRANCH _MULFLOAT32_X
_MULFLOAT32_2:
	; unpack a into e_a and f_a
	LOADREG FP	; addr of FL_E_A
	LOADCP _UNPACKFLOAT32
	CALL

	; s_r = s_a xor s_b
	LOAD FL_S_A
	LOAD FL_S_B
	XOR
	STORE FL_S_R

	; e_r = e_a + e_b - bias
	LOAD FL_E_A
	LOAD FL_E_B
	ADD
	LOADC FLOAT32_BIAS
	SUB
	STORE FL_E_R

	; check exponent range overflow
	LOADCP FLOAT32_EXP_CHECK
	CALL
	; f_r = fractmult(f_a, f_b)
	LOADCP _FRACTMULT
	CALL

	LOADCP NORMROUND_FL ; normalize and round
	CALL

	; pack result
	LOADREG FP
	LOADC FL_E_R
	ADD
	LOADCP _PACKFLOAT32
	CALL
_MULFLOAT32_X:
	; 32 bit float result is now on stack
	FPADJ MULFL_FS
	RET

; multiply fractions.
; uses stack frame/local variables from caller.
; FL_F_B is destroyed.
; result is written to FL_R
_FRACTMULT:
	LOADC FLOAT32_FRCBITS ; this is the counter which we keep on stack

	LOADC 0 ; start with result zero
	STORE FL_F_R

	; for each digit of the fraction of b, from right to left,
	; the fraction of a is added to the result, if that digit is 1.
	; on each iteration, the result is shifted right.
_FRACTMULT_1:
	LOAD FL_F_R
	SHR
	STORE FL_F_R ; shift result frac for next iteration

	LOAD FL_F_B
	LOADC FLOAT32_FRCLSB
	AND		; isolate lowest bit

	CBRANCH.Z _FRACTMULT_2 ; if not set, don't add in this iteration

	LOAD FL_F_R	; otherwise, add fraction of a to result
	LOAD FL_F_A
	ADD
	STORE FL_F_R

_FRACTMULT_2:
	LOAD FL_F_B
	SHR
	STORE FL_F_B ; shift fraction b for next digit

	DEC 1	; decrease counter
	DUP
	CBRANCH _FRACTMULT_1 ; if not zero, next loop iteration

	DROP	; drop counter
	RET

; divide floating point value a by b
; parameters: [ a, b ]
; returns: a / b
	.EQU DIVFL_FS	36
_DIVFLOAT32:
	FPADJ -DIVFL_FS ; same stack frame layout as _ADDFLOAT32
	DUP
	CBRANCH.NZ _DIVFLOAT32_0  ; check b for zero
	LOADCP FLOAT32_ERR_DIVZ ; if it is, we have a division by zero
	LOADCP _RUNTIME_ERR	; and we issue a runtime error
	JUMP
_DIVFLOAT32_0:
	OVER			; check a for zero
	CBRANCH.NZ _DIVFLOAT32_0_1
	; if it is we just return zero
	DROP			; remove args
	DROP
	LOADC 0
	BRANCH _DIVFLOAT32_XT

_DIVFLOAT32_0_1:
	; unpack b into e_b and f_b
	LOADREG FP
	LOADC FL_E_B
	ADD		; addr of FL_E_B
	LOADCP _UNPACKFLOAT32
	CALL
	; unpack a into e_a and f_a
	LOADREG FP	; addr of FL_E_A
	LOADCP _UNPACKFLOAT32
	CALL

	; s_r = s_a xor s_b
	LOAD FL_S_A
	LOAD FL_S_B
	XOR
	STORE FL_S_R

	; e_r = e_a - e_b + bias
	LOAD FL_E_A
	LOAD FL_E_B
	SUB
	LOADC FLOAT32_BIAS
	ADD
	STORE FL_E_R

	; check exponent range overflow
	LOADCP FLOAT32_EXP_CHECK
	CALL
	; f_r = fractdiv(f_a, f_b)
	LOADCP _FRACTDIV
	CALL

	LOADCP NORMROUND_FL ; normalize and round
	CALL

	; pack result
	LOADREG FP
	LOADC FL_E_R
	ADD
	LOADCP _PACKFLOAT32
	CALL
_DIVFLOAT32_XT:
	; 32 bit float result is now on stack
	FPADJ DIVFL_FS
	RET

; divide fraction of a by b.
; uses stackframe of caller.
; FL_F_A is destroyed.
; places result into FL_F_R.
_FRACTDIV:
	LOADC 0
	STORE FL_F_R

	LOADC FLOAT32_FRCBITS	; load counter
_FRACTDIV_1:
	LOAD FL_F_R	; load result for later
	LOAD FL_F_B
	LOAD FL_F_A
	CMPU GT
	CBRANCH _FRACTDIV_2 ; if b > a, skip next section
	INC 1		; a fits into b, so add 1 to result
	LOAD FL_F_A	; subtract divisor from dividend
	LOAD FL_F_B
	SUB
	STORE FL_F_A

_FRACTDIV_2:
	SHL		; shift result left for next digit
	STORE FL_F_R	; and store it again
	LOAD FL_F_A	; shift the dividend left for next digit
	SHL
	STORE FL_F_A

	DEC 1		; decrement counter
	DUP
	CBRANCH.NZ _FRACTDIV_1	; loop if it is not zero

	DROP		; remove counter

	LOAD FL_F_R	; undo the last shift of the result
	SHR
	BROT		; and shift left again to make room for the exponent
	STORE FL_F_R
	RET


; normalize and round f_r.
; uses stack frame of caller.
NORMROUND_FL:
	LOAD FL_F_R
NORMFL_0:
	; if overflow bit of f_r is set, scale right
	DUP
	LOADCP FLOAT32_OVRFLOW	; isolate overflow bit
	AND
	CBRANCH.Z NORMFL_1 ; if bit is zero, skip the scaling

	; scale right: shift right f_r  by one, increment e_r by one
	SHR
	LOAD FL_E_R
	INC 1
	STORE FL_E_R

NORMFL_1:
	; check if result is zero
	DUP
	CBRANCH.NZ NORMFL_1_1
	; set exponent to something useful
	LOADC 0
	STORE FL_E_R
	; clear sign flag, we don't want -0
	LOADC 0
	STORE FL_S_R

	BRANCH NORMFL_2

	; normalize:
	; if bit 30 of f_r is zero, scale left
NORMFL_1_1:
	DUP
	LOADCP FLOAT32_FRCMSBM	; isolate msb of fraction
	AND
	CBRANCH.NZ NORMFL_2 ; skip if bit is set
	;   scale left and repeat previous step
	SHL	; shift fraction
	LOAD FL_E_R	; decrement exponent
	DEC 1
	STORE FL_E_R
	BRANCH NORMFL_1_1 ; repeat

NORMFL_2:
	; round:
	; simple round algorithm: if the bit below
	; the least significat bit is 1, round up
	; and check the overflow bit again
	DUP
	LOADC FLOAT32_RNDMASK ; test rounding bit
	AND
	CBRANCH.Z NORMFL_3	; if not set, skip rounding

	LOADC FLOAT32_RNDINCR
	ADD

	LOADCP FLOAT32_FRCMASK ; remove rounded off bits
	AND

	BRANCH 	NORMFL_0	; check for overflow again
NORMFL_3:
	STORE FL_F_R
	RET

; return the fractional part of a floating point number
; parameters: float32 value
; returns: float32 value
	.EQU FRACTFL_EXP 12
	.EQU FRACTFL_FS 20
_FRACTFLOAT32:
	FPADJ -FRACTFL_FS
	DUP	; check for zero
	CBRANCH.Z FRACTFL_XT ; if yes, just return zero
	LOADREG FP
	LOADCP _UNPACKFLOAT32
	CALL

	LOAD FL_E_A	; remove bias from exponent
	LOADC FLOAT32_BIAS
	SUB
	DUP
	STORE FRACTFL_EXP

	LOADC 0		; if the exponent is negative,
	CMP LT		; there are no digits before the point
	CBRANCH FRACTFL_PK ; and we just return the same value

	LOAD FRACTFL_EXP ; the exponent indicates how far
			 ; we need to shift the fraction to the left
	INC 1		 ; at exp 0 we need to shift by 1

	LOADC FLOAT32_BIAS - 1
	STORE FL_E_A	; the new exponent is -1 because we
			; shifted by (old exp + 1) bits

	LOAD FL_F_A	; load fraction, shift count is already on stack
FRACTFL_L:
	SHL		; shift fraction
	SWAP		; swap fraction and shift count
	DEC 1		; decrement count
	SWAP		; swap back shift count and fraction
	OVER		; check count for zero
	CBRANCH FRACTFL_L ; if not, loop

	NIP		; remove count
	LOADCP FLOAT32_FRCMASK
	AND		; remove superfluous bits

	DUP		; check if fraction is zero
	CBRANCH.NZ FRACTFL_1
			; if yes, directly return
	BRANCH FRACTFL_XT

	; normalize
FRACTFL_1:
	DUP
	LOADCP FLOAT32_FRCMSBM ; isolate msb of fraction
	AND
	CBRANCH.NZ FRACTFL_2 ; skip if bit is set
	;   scale left and repeat previous step
	SHL	; shift fraction
	LOAD FL_E_A	; decrement exponent
	DEC 1
	STORE FL_E_A
	BRANCH FRACTFL_1 ; repeat
FRACTFL_2:
	STORE FL_F_A
FRACTFL_PK:
	LOADREG FP
	LOADCP _PACKFLOAT32
	CALL
FRACTFL_XT:
	FPADJ FRACTFL_FS
	RET

; truncate a floating point number - return
; the integer value before the decimal point.
; issues a runtime error if the integer value
; does not fit into a signed 32-bit integer.
; parameters: float32 value
; returns:  integer
	.EQU TRUNCFL_SHIFT 12
	.EQU TRUNCFL_FS 16
_TRUNCFLOAT32:
	FPADJ -TRUNCFL_FS
	LOADREG FP
	LOADCP _UNPACKFLOAT32
	CALL

	LOAD FL_E_A	; remove bias from exponent
	LOADC FLOAT32_BIAS
	SUB
	DUP
	STORE FL_E_A

			; if the exponent
	LOADC 0		; is negative,
	CMP LT		; our result is zero
	CBRANCH TRUNCFL_ZERO

	; shift fraction  all the way to the right
	LOAD FL_F_A	; shift fraction right by 8 bits
	BROT		; by rotating 3*8 bits left
	BROT		; we know that the original bits 7-0
	BROT		; are zero

	; calculate number of bits to shift:
	; fraction bits - 1 - exponent
	LOADC FLOAT32_FRCBITS - 1
	LOAD FL_E_A
	SUB
	DUP
	STORE TRUNCFL_SHIFT
	LOADC 0			; is shift count >= 0?
	CMP GE
	CBRANCH TRUNCFL_2

	; shift count is negative, need to shift left
	; check for integer overflow first
	LOAD TRUNCFL_SHIFT
	NOT		; negate shift count
	INC 1

	DUP		; shift count >= bits in a word - bits in the fraction?
	LOADC 32 - FLOAT32_FRCBITS
	CMP GE
	CBRANCH TRUNCFL_ERR	; then we have an overflow

	; otherwise, shift n bits left
TRUNCFL_1:
	DUP
	CBRANCH.Z TRUNCFL_3	; if zero, we are done

	DEC 1		; decrement shift count
	SWAP		; swap shift count and fraction
	SHL		; shift fraction left
	SWAP		; swap back
	BRANCH TRUNCFL_1 ; and loop

TRUNCFL_2:			; shift right, fraction is on ToS
	LOAD TRUNCFL_SHIFT	; bits to shift
TRUNCFL_L1:
	DUP
	CBRANCH.Z TRUNCFL_3	; if zero, we are done

	DEC 1		; decrement shift count
	SWAP		; swap shift count and fraction
	SHR		; shift fraction right
	SWAP		; swap back
	BRANCH TRUNCFL_L1 ; and loop
TRUNCFL_3:
	DROP	; drop shift count
	BRANCH TRUNCFL_EXIT
TRUNCFL_ZERO:
	LOADC 0
TRUNCFL_EXIT:
	LOAD FL_S_A		; check sign
	CBRANCH.Z TRUNCFL_EXIT1
	; if not zero, negate result
	NOT
	INC 1
TRUNCFL_EXIT1:
	FPADJ TRUNCFL_FS
	RET

TRUNCFL_ERR:
	DROP
	LOADCP FLOAT32_ERR_TRUNC
	LOADCP _RUNTIME_ERR
	JUMP

; like _TRUNCFLOAT32, but return a float32 value
; parameters [ float value ]
; returns: float value
; TODO: write a real routine which might be faster
; than just calling _TRUNCFLOAT32 and INTTOFLOAT32.
; Also, _TRUNCFLOAT32 can cause an integer overflow
; where INT should not.
_INTFLOAT32:
	LOADCP _TRUNCFLOAT32
	CALL
	LOADCP _INTTOFLOAT32
	CALL
	RET

	.CPOOL

; multiply/divide a float value by a multitude of 2
; shift count < 1: shift right (divide)
; shuft count > 0: shift left (multiply)
; parameters: [ float value, shift count ]
; returns: float value
	.EQU SHF32_FS 36
	.EQU SHF32_COUNT 12
SHIFTFLOAT32:
	FPADJ -SHF32_FS

	; store shift count parameter
	STORE SHF32_COUNT
	; Unpack float value.
	; We use the result variable slot
	; because EXP_CHECK uses that.
	LOADREG FP
	LOADC FL_E_R
	ADD
	LOADCP _UNPACKFLOAT32
	CALL

	; we just add the shift count to
	; the exponent
	LOAD FL_E_R
	LOAD SHF32_COUNT
	ADD
	STORE FL_E_R

	; and check for overflow
	LOADCP FLOAT32_EXP_CHECK
	CALL

	; pack up the result
	LOADREG FP
	LOADC FL_E_R
	ADD
	LOADCP _PACKFLOAT32
	CALL

	FPADJ SHF32_FS
	RET

; convert an integer to a float32
; parameters: an integer
; returns: a float32 value
	.EQU INTF32_FS 12
_INTTOFLOAT32:
	FPADJ -INTF32_FS

	; is intval zero?
	DUP
	CBRANCH.Z INTTOFLOAT32_XT ; if yes, we are done, float zero has the same
				  ; representation as integer zero

	; is intval < 0?
	LOADC 0
	CMP.S0 LT
	DUP
	STORE FL_S_A ; store cmp result as sign flag
	CBRANCH.Z INTF32_1 ; skip negate if cmp result is 0
	;   negate
	NOT
	INC 1
INTF32_1:
	LOADC 30 ; set counter
INTF32_L:
	; loop if bit 31 is not set:
	SWAP  ; swap counter and value (value is now ToS)

	LOADCP $40000000
	AND.S0
	CBRANCH INTF32_2
	;   shift left
	SHL
	; swap counter and value again (counter is now ToS)
	SWAP
	;   decrease counter
	DEC 1
	BRANCH INTF32_L
INTF32_2:
	; store shifted value as fraction
	STORE FL_F_A
	; set exponent to counter + bias
	LOADC FLOAT32_BIAS
	ADD
	STORE FL_E_A
	; pack components
	LOADREG FP
	LOADCP _PACKFLOAT32
	CALL
	BRANCH INTTOFLOAT32_XT
INTTOFLOAT32_XT:
	FPADJ INTF32_FS
	RET

; get the binary exponent of a floating point value
; (without the bias)
; parameters: [ float value ]
; returns: exponent (-128..128)
_GETFLOAT32EXP:
	LOADC FLOAT32_EXPMASK
	AND
	LOADC FLOAT32_BIAS
	SUB
	RET

; compare an integer to a floating point value
; parameters: a:integer, b: float
; calls INTTOFLOAT32 on a, then calls CMPFLOAT32
; returns: -1 if a < b
;           0 if a = b
_CMPINTFLOAT32:
	SWAP	; swap a and b, a is now on top of stack
	LOADCP _INTTOFLOAT32
	CALL	; a is now a float
	SWAP	; swap a and b back, b is on top of stack again
	; fallthrough to CMPFLOAT32

; compare two floating point values
; parameters: two floating point values a and b
; returns: -1 if a < b
;           0 if a = b
;           1 if a > b
	.EQU CMPFL_FS 24
_CMPFLOAT32:
	FPADJ -CMPFL_FS
	; unpack b
	LOADREG FP
	LOADC FL_E_B
	ADD
	LOADCP _UNPACKFLOAT32
	CALL
	;unpack a
	LOADREG FP
	LOADCP _UNPACKFLOAT32
	CALL

	;if sign(a) and not sign(b):
	;	return -1
	;if not sign(a) and sign(b):
	;	return 1
	LOAD FL_S_A
	LOAD FL_S_B
	XOR
	CBRANCH.Z CMPFL_2 ; jump if signs are the same
	; signs are not the same
	LOAD FL_S_A
	CBRANCH.Z CMPFL_1 ; jump if a is positive
	; a is negative, b must be positive, so a < b
CMPFL_M1:
	LOADC -1
	BRANCH CMPFL_XT
CMPFL_1:
	; a is positive, b must be negative, so a > b
	LOADC 1
	BRANCH CMPFL_XT
CMPFL_2:
	; signs are the same, compare exponents now.
	; we can compare the exponent values directly without
	; subtracting the bias because negative exponents
	; are smaller than positive exponents due to the added
	; bias.
	; if we determine a result, from now on the result
	; must be inverted if the sign of a and b is negative.

	;if exp(a) < exp(b):
	;	return -1 * sign(a)
	LOAD FL_E_A
	LOAD FL_E_B
	CMPU GE
	CBRANCH CMPFL_3 ; jump if exp(a) >= exp(b)
	; exp(a) < exp(b), so abs(a) < abs(b)
	LOADC -1
CMPFL_2_1:
	; invert result if signs of a and b are negative
	LOAD FL_S_A
	CBRANCH.Z CMPFL_XT  ; sign is clear, no need to invert

	NOT	; negate result and exit
	INC 1
	BRANCH CMPFL_XT
CMPFL_3:
	; exp(a) >= exp(b)
	; check if exp(a) > exp(b)
	LOAD FL_E_A
	LOAD FL_E_B
	CMPU GT
	CBRANCH CMPFL_5  ; jump if yes
	; exp(a) = exp(b)
	; exponents are the same, compare fractions now

	;if frac(a) < frac(b):
	;	return -1 * sign(a)
	LOAD FL_F_A
	LOAD FL_F_B
	CMPU GE
	CBRANCH CMPFL_4 ; jump if frac(a) >= frac(b)

	; frac(a) < frac(b)
	LOADC -1
	BRANCH CMPFL_2_1 ; check for inversion of result
	;if frac(a) > frac(b):
	;	return 1 * neg_result
CMPFL_4:
	; frac(a) >= frac(b)
	; check if frac(a) > frac(b)
	LOAD FL_F_A
	LOAD FL_F_B
	CMPU GT
	CBRANCH CMPFL_5 ; jump if frac(a) > frac(b)
	; frac(a) = frac(b)
	LOADC 0
	BRANCH CMPFL_XT
CMPFL_5:
	; exp(a) > exp(b):
	;	return 1 * sign(a)
	LOADC 1
	BRANCH CMPFL_2_1 ; check for inversion of result
CMPFL_XT:
	FPADJ CMPFL_FS
	RET

; Negate a float32 number.
; just flip the highest bit, no
; need to unpack.
; if it is zero, do nothing, we
; do not want a -0.0 value
_NEGFLOAT32:
	DUP
	CBRANCH.Z _NEGFL_XT
	LOADCP $80000000
	XOR
_NEGFL_XT:
	RET

	.CPOOL

FLOAT32_ERR_OVR:
	.BYTE "floating point overflow",0
FLOAT32_ERR_TRUNC:
	.BYTE "integer overflow",0
FLOAT32_ERR_DIVZ:
	.BYTE "float division by zero",0
