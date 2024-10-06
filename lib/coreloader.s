; Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details
	.ORG 4096

CORELOADER:
	; initialize program stack and
	; return stack pointers
	LOADCP 24060
	STOREREG FP
	LOADCP 24064
	STOREREG RP

	LOADCP SYSBOOTTICKS
	LOADCP GETTICKS
	CALL
	STOREI
	DROP

	LOADCP INITSDCARD
	CALL

	;LOADCP FIND_SYSPART	; no need to call, it never
	;CALL			; returns, so just fall through

	.EQU PART_START 0
	.EQU EXTENT_SIZE 4
	.EQU DIR_SIZE 8
	.EQU SLOT_NO 12
	.EQU SIZE_BYTES 16
	.EQU PRG_START_BLK 20
	.EQU FIND_FS 24

	.EQU PARTENTRY_SIZE 64
	.EQU DIRENTRY_SIZE 64
FIND_SYSPART:
	FPADJ -FIND_FS
	; load block 0
	LOADC 0
	LOADCP CARDREADBLK
	CALL

	DUP			; non-zero return code means error
	CBRANCH.Z FIND_1

	LOADCP PRINTHEXW
	CALL
	LOADCP NEWLINE
	CALL
	LOADC 0
	JUMP

FIND_1:
	DROP	; remove return code

	;LOADC 512
	;LOADCP CARD_BUF
	;LOADCP HEXDUMP
	;CALL

	; address of the first partition entry
	LOADCP CARD_BUF
FIND_L:
	DUP ; dup addr for comparison
	LOADCP SYSPART_NAME
	LOADC SYSNAME_WORDS
	LOADCP _CMPWORDS
	CALL
	CBRANCH.NZ FIND_FOUND
	; go to next entry
	LOADC PARTENTRY_SIZE
	ADD

	; check if address is still
	; within the data block
	DUP
	LOADCP CARD_BUF,512
	CMP LT
	CBRANCH FIND_L

	; remove address
	DROP

	; not found, complain and
	; go back to ROM monitor
	LOADCP SYSPART_ERR
	LOADCP PRINTLINE
	CALL

	LOADC 0
	JUMP

FIND_FOUND:
	; address of the part entry is on stack

	; check if partition is enabled
	DUP	 ; duplicate address
	LOADC 40 ; add PartFlags field offset
	ADD
	LOADI
	LOADC 1
	AND	; check bit 0 (PartEnabled)
	CBRANCH.Z FIND_L ; if not set, continue loop


	; address of part entry is still on stack
	DUP
	LOADC 44	; add startBlock field offset
	ADD
	LOADI		; get start block number
	STORE PART_START

	; address of part entry is still on stack
	DUP
	LOADC 52	; move to extentSize field
	ADD
	LOADI		; get value
	STORE EXTENT_SIZE

	; address of part entry is still on stack
	LOADC 56	; move to dirSize field
	ADD
	LOADI		; get value
	STORE DIR_SIZE

	LOADC 0
	STORE SLOT_NO	; start with dirslot 0

	LOAD PART_START  ; start with first block of the partition
FIND_FILE:
	DUP		; duplicate block number
	LOADCP CARDREADBLK ; read that block
	CALL
	DROP		; ignore error

	; scan directory entries for shell file name
	LOADCP CARD_BUF
FIND_FILE_L:
	DUP
	LOADCP SHELL_NAME
	LOADC SHELLNAME_WORDS
	LOADCP _CMPWORDS	; compare names
	CALL
	CBRANCH.NZ FIND_F_FOUND	; exit loop if names match

	; check if current dirslot no
	; is below maximum number of slots
	LOAD SLOT_NO
	LOAD DIR_SIZE
	CMP GE
	CBRANCH FIND_F_NOTFOUND ; max slots reached, exit

	; add 1 to SLOT_NO
	LOAD SLOT_NO
	INC 1
	STORE SLOT_NO

	; address is still on stack
	LOADC DIRENTRY_SIZE
	ADD		; go to next dir entry

	; check if address is still
	; below end of data block
	DUP
	LOADCP CARD_BUF,512
	CMP LT
	CBRANCH FIND_FILE_L ; if it is below, loop

	DROP	; remove dir entry addr

	; block no is still on stack
	INC 1
	BRANCH FIND_FILE ; read next block

FIND_F_NOTFOUND:
	LOADCP SHELL_ERR
	LOADCP PRINTLINE
	CALL

	; remove entry addr and block number
	DROP
	DROP
	LOADC 0
	JUMP

FIND_F_FOUND:
	; found the file name, now check if it has the right flags

	; address of dir entry is still on stack
	DUP
	LOADC 40	; add flags field offset
	ADD
	LOADI	; load flags
	LOADC 16 ; test for SlotFirst flag
	AND
	CBRANCH.Z FIND_FILE_L	; if not set, continue loop

	;LOADCP FOUND_MSG
	;LOADCP PRINTLINE
	;CALL

	; we got the right file, now calculate start block
	; and get file size from dir entry

	; address of dir entry is still on stack
	; phys start block = part start + slot_no * (extent_size/512)
	LOAD EXTENT_SIZE
	LOADC 9
	LOADCP _SHRM
	CALL
	LOAD SLOT_NO
	LOADCP _MUL
	CALL
	LOAD PART_START
	ADD

	;DUP
	;LOADCP PRINTHEXW
	;CALL
	;LOADC ' '
	;LOADCP CONOUT
	;CALL

	STORE PRG_START_BLK

	; address of dir entry is still on stack
	LOADC 44
	ADD	; add sizeBytes field offset
	LOADI	; get size in bytes

	;DUP
	;LOADCP PRINTHEXW
	;CALL
	;LOADCP NEWLINE
	;CALL

	STORE SIZE_BYTES

	; remove block number
	DROP

	; set argument count to 0
	; in case this gets called
	; by a terminating program
	LOADCP PARGCOUNT
	LOADC 0
	STOREI
	DROP

	LOADC 0	; device id is always 0
	LOAD PRG_START_BLK
	LOAD SIZE_BYTES
	; release our stack frame
	FPADJ FIND_FS

	; load program
	LOADCP CORELOAD
	CALL

	LOADC 0
	JUMP

	.CPOOL

	.EQU SYSNAME_WORDS 4
SYSPART_NAME:
	.WORD 6, 32
	.BYTE "SYSTEM"
SYSPART_ERR:
	.BYTE "No ""SYSTEM"" partition.",13,10,10,0
	.EQU SHELLNAME_WORDS 5
SHELL_NAME:
	.WORD 10,32
	.BYTE "shell.prog"
SHELL_ERR:
	.BYTE "No shell on ""SYSTEM"" partition.",13,10,10,0

FOUND_MSG:
	.BYTE " shell.prog ",0

%include "corelib.s"
