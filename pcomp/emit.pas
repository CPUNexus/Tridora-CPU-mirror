(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
procedure emitOperator(op: string); forward;
procedure emitLoadIndirect; forward;
procedure emitCall(name:string); forward;
procedure emitCallRaw(name:string); forward;
procedure emitLabelRaw(name:IdentString); forward;
procedure emitInc(amount: integer); forward;
procedure emitDec(amount: integer); forward;
procedure emitLoadConstantInt(i: integer); forward;
procedure emitLoadNegConstInt(i: integer); forward;

procedure rewindStringList(var list:StringList); forward;
function nextStringListItem(var list:StringList; var returnStr: IdentString): boolean;
	forward;

procedure cpuAllocStackframe(aProc:ProcRef);
begin
	if aProc^.isNested then
	begin
		if aProc^.vars.offset <> 0 then
			errorExit2('internal error in cpuAllocStackFrame for', aProc^.name );
		
		(* allocate space for the outer frame pointer and old BP *)
		aProc^.vars.offset := aProc^.vars.offset + (wordSize*2);
		aProc^.parameters.offset := aProc^.parameters.offset + (wordSize*2);
	end;
end;

procedure countIns(amount: integer);
begin
	insCount := insCount + amount;
end;

procedure CPoolIfHighMark(jumpOver:boolean); forward;

procedure emitIns(ins: string);
begin
	writeln(outfile, #9, ins);
	countIns(1);
	CPoolIfHighMark(true);
end;

function getLocalLabel(prefix:IdentString;no:integer):IdentString;
var buf: string[12];
begin
	str(no,buf);
	getLocalLabel := prefix + buf + globalSuffix;
end;

procedure emitLocalLabel(prefix:IdentString;no:integer);
begin
	writeln(outfile, prefix,no,globalSuffix,':');
end;

procedure emitInsLabel(prefix:IdentString;no:integer);
begin
	writeln(outfile, #9, prefix,no,globalSuffix);
end;


procedure emitCpool(jumpOver:boolean);
begin
	insCount := 0;
	if jumpOver then emitIns('.CPOOLNOP') else emitIns('.CPOOL');
end;

procedure CPoolIfLowMark(jumpOver:boolean);
begin
	if insCount > lowCpoolMark then emitCpool(jumpOver);
end;

procedure CPoolIfHighMark(jumpOver:boolean);
begin
	if insCount > highCpoolMark then emitCpool(jumpOver);
end;

procedure emitIns2(ins, op: string);
begin
	writeln(outfile, #9, ins, ' ', op);
	countIns(1);
end;

procedure emitIns2Int(ins: string; op: integer);
begin
	writeln(outfile, #9, ins, ' ', op);
	countIns(1);
end;

procedure emitPrologue;
begin
	writeln(outfile, #9, '.ORG ', startAddress);
	emitIns2('BRANCH', '@+16');
	emitIns2('BRANCH', '@+$AFE');
	emitLabelRaw('_HEAP_SZ_PTR');
	emitIns2Int('.WORD', defaultHeapSize);
	emitLabelRaw('_STACK_SZ_PTR');
	emitIns2Int('.WORD', defaultStackSize);
	emitIns2Int('.WORD', 0);
	emitIns2('LOADCP','_END'); (* end of program is start of heap *)
	emitIns2('LOADCP', '_MEM_INIT');	(* MEM_INIT initializes heap and sets FP/RP *)
								(* since RP is not initialized yet, we cannot use CALL
									and MEM_INIT jumps to _MAIN after it is done *)
	emitIns('JUMP');
end;

function bytes2words(size:integer):integer;
begin
	bytes2words := (size + (wordSize-1)) div wordSize
end;

procedure emitGlobalVars;
var v: SymblRef;
    wordsCount: integer;
begin
	v := mainProcedure^.vars.first;

	while v <> nil do
	begin
		if not v^.isExternal then
		begin
			wordsCount := bytes2words(v^.size);
			if v^.symType.baseType in [ ArrayType, RecordType ] then
			begin
				(* if an array has initial values, it is handled by
					emitArrayConsts *)
				if not v^.hasInitialValue then
					writeln(outfile, v^.name, ':', #9, '.BLOCK ', wordsCount)
			end
			else if v^.symType.baseType = StringType then
			begin
				(* if a global string variable has an initial value, it
					is handled by emitConstStrs *)
				if not v^.hasInitialValue then
				begin
					writeln(outfile, v^.name, ':', #9, '.WORD 0');
					emitIns2Int('.WORD', v^.symType.stringLength);
					emitIns2Int('.BLOCK', wordsCount - 2);
				end
			end
			else
				(* integer, real, boolean, char *)
				writeln(outfile, v^.name, ':', #9, '.WORD ', v^.initialValue);
		end;
		v := v^.next;
	end;
end;

procedure emitString(var s:KeywordString; maxLength:integer);
var pad:integer;
    c:char;
	inQuotes:boolean;
	first:boolean;

procedure writeComma;
begin
	if (not inQuotes) and (not first) then
		write(outfile, ',');
	first := false;
end;

procedure startQuotes;
begin
	if not inQuotes then
	begin
		writeComma;
		write(outfile,'"');
		inQuotes := true;
	end;
end;

procedure endQuotes;
begin
	if inQuotes then
	begin
		write(outfile,'"');
		inQuotes := false;
	end;
end;

procedure writeAsString;
begin
	startQuotes;
	write(outfile,c);
end;

procedure writeAsNum;
begin
	endQuotes;
	writeComma;
	write(outfile, ord(c));
end;

begin
		inQuotes := false;
		first := true;
		writeln(outfile,#9,'.WORD ', length(s), ',', maxLength);
		if length(s) > 0 then
		begin
			write(outfile,#9,'.BYTE ');
			for c in s do
				(* handle " inside strings *)
				if c = '"' then
				begin
					startQuotes;
					write(outfile,'""')
				end
				else
				(* handle non-printable characters *)
				if ord(c) < ord(' ') then
					writeAsNum
				else
					writeAsString;
			endQuotes;
			writeln(outfile);
		end;
		if maxLength <> 0 then
		begin
			pad := bytes2words(maxLength) - bytes2words(length(s));
			if pad > 0 then
				writeln(outfile,#9, '.BLOCK ', pad);
		end;	
end;

procedure emitConstStrs;
var c: ConstStrRef;
begin
	c := firstConstStr;
	while c <> nil do
	begin
		if c^.extraLabel <> nil then
			writeln(outfile, c^.extraLabel^,':');
		(* TODO: quote special characters *)
		emitLocalLabel('_C_S_', c^.no);
		emitString(c^.value, c^.length);
		c := c^.next;
	end;
end;

procedure emitArrayConsts;
var current: ArrayConstRef;
    elem: ^OpaqueDataElement;
	count: integer;
begin
	current := firstArrayConst;
	while current <> nil do
	begin
		if current^.extraLabel <> nil then
			writeln(outfile, current^.extraLabel^, ':');
		emitLocalLabel('_C_A_', current^.id);

		elem := current^.firstElement;
		count := 0; (* counts the items in a single .WORD directive *)
		while elem <> nil do
		begin
			if elem^.isStringValue then
			begin
				writeln(outfile);
				count := -1; (* make count zero in next iteration *)
				emitString(elem^.strValue^, elem^.maxLength);
			end
			else
			if count = 0  then
			begin
				writeln(outfile);
				write(outfile,#9,'.WORD ', elem^.intValue)
			end
			else
				write(outfile,',', elem^.intValue);
			count := (count + 1) and 7;
			elem := elem^.next;
		end;
		writeln(outfile);
		current := current^.next;
	end;
end;

procedure emitInclude(s:string);
begin
	writeln(outfile, '%include "',s,'"');
	emitIns('.CPOOL');
end;

procedure emitUnitEpilogue;
begin
	emitIns('.CPOOL');
	emitGlobalVars;
	emitConstStrs;
	emitArrayConsts;
end;

procedure emitEpilogue;
var unitName:IdentString;
begin
	if useStandalone then
		emitIns2Int('LOADC', 0)
	else
		emitIns2('LOADCP', 'PTERM');
	emitIns('JUMP');

	emitIns('.CPOOL');
	emitGlobalVars;
	emitConstStrs;
	emitArrayConsts;

	if useStandalone then
		emitInclude('corelib.s')
	else
		emitInclude('coreloader.lsym');

	emitInclude('float32.lib');
	emitInclude('runtime.lib');
	emitInclude('stdlib.lib');

	rewindStringList(usedUnits);
	while nextStringListItem(usedUnits, unitName) do
		emitInclude(unitName + UnitSuffix2);

	emitLabelRaw('_END');
end;

procedure emitMainStart;
begin
	writeln(outfile,'_MAIN:');
end;

procedure emitNewSymbol(scope: SymbolScope; var name: string; offset: integer);
begin
	(* if scope = LocalSymbol then writeln(outfile, #9, '.EQU ', name, ' ', offset); *)
end;

procedure emitDup;
begin
	emitIns('DUP');
end;

(* call checkerror from stdlib, file ptr is already
   on stack and needs to stay on stack *)
procedure emitCheckError;
begin
	emitDup;
	emitCall('CHECKERROR');
end;

procedure emitDefaultOutput;
begin
	emitIns2('LOADCP', 'OUTPUT');
	emitCheckError;
	emitIns('SWAP');
end;

procedure emitWriteFileArg;
begin
	emitIns('OVER');
end;

procedure emitWrite(typeTag: TypeTagString);
begin
	emitCall('FWRITE' + typeTag);
end;

procedure emitWriteNewline;
begin
	emitIns2('LOADCP','NEWLINESTR');
	emitIns('OVER');
	emitLoadConstantInt(0);
	emitCall('FWRITESTRING');
end;

procedure emitDefaultNewline;
begin
	emitIns2('LOADCP','NEWLINESTR');
	emitIns2('LOADCP', 'OUTPUT');
	emitLoadConstantInt(0);
	emitCall('FWRITESTRING');
end;

procedure emitWriteEnd;
begin
	emitIns('DROP');
end;

procedure emitWriteWords(size:integer);
begin
	emitLoadConstantInt(size);
	emitCall('FWRITEWORDS');
end;

procedure emitDefaultInput;
begin
	emitIns2('LOADCP', 'INPUT');
	emitCheckError;
	emitIns('SWAP');
end;

procedure emitReadFileArg;
begin
	emitIns('OVER');
end;

procedure emitRead(typeTag: TypeTagString);
begin
	emitCall('FREAD' + typeTag);
end;

procedure emitReadWords(size:integer);
begin
	emitLoadConstantInt(size);
	emitCall('FREADWORDS');
end;

procedure emitReadNewline;
begin
	emitCall('SKIPEOLN');
end;

procedure emitReadDefaultNewline;
begin
	emitIns2('LOADCP', 'INPUT');
	emitReadNewline;
end;

procedure emitReadEnd;
begin
	emitIns('DROP');
end;

procedure emitLoadConstant(c: string);
begin
	emitIns2('LOADC', c);
end;

procedure emitLoadConstantInt(i: integer);
var s: string[32];
    rest:integer;
begin
	if i < 0 then
		emitLoadNegConstInt(i)
	else
	begin
		str(i,s);
		if i > MaxShortOffset then
		begin
			rest := i - MaxShortOffset;
			if rest <= MaxTinyOffset then
			begin
				emitLoadConstantInt(MaxShortOffset);
				emitInc(rest); (* a LOADC + INC is shorter that a LOADCP *)
			end
			else
				emitIns2('LOADCP', s);
		end
		else
			emitLoadConstant(s);
	end;
end;

procedure emitLoadNegConstInt(i: integer);
var s: string[32];
    rest:integer;
begin
	if i > 0 then
		errorExit2('internal error in emitLoadNegConstInt', '')
	else
	begin
		str(i,s);
		if i < -MaxShortOffset - 1 then
		begin
			rest := i + MaxShortOffset + 1;  (* max negative short number is -4096 *)
			if abs(rest) <= MaxTinyOffset then
			begin
				emitLoadNegConstInt(-(MaxShortOffset-1));
				emitDec(rest); (* a LOADC + INC is shorter that a LOADCP *)
			end
			else
				emitIns2('LOADCP', s);
		end
		else
			emitLoadConstant(s);
	end;
end;

procedure emitLoadConstantReal(r: real);
begin
	emitLoadConstantInt(encodeFloat32(r));
end;

procedure emitLoadOffset(sym: SymblRef);
begin
	writeln(outfile,#9, 'LOADC ', sym^.offset);
	countIns(1);
end;

procedure emitConstBoolean(b: boolean);
begin
	if b then
		emitLoadConstant('1')
	else
		emitLoadConstant('0');
end;

procedure emitSwap;
begin
	emitIns('SWAP');
end;

function isShortLoadStore(var loc: MemLocation):boolean;
begin
	isShortLoadStore := (loc.memLoc = LocalMem) and (loc.offset <= MaxUShortOffset);
end;

procedure emitLoadLocalAddr(var name: IdentString; offset: integer);
begin
	writeln(outfile,#9,' ; ', name);
	emitIns2('LOADREG', 'FP');
	emitInc(offset);
end;

procedure emitStoreLocal(offset:integer; var name: IdentString);
begin
	if offset <= MaxUShortOffset then
	begin
		writeln(outfile,#9, 'STORE ', offset, ' ; ', name);
		countIns(1);
	end
	else
	begin
		(* if it is not a short store, the address is already on the stack *)
		emitIns('STOREI');
		emitIns('DROP');
	end
end;

procedure emitStoreNested(offset:integer; distance: integer; var name: IdentString);
begin
	if offset <= MaxUShortOffset then
	begin
		if distance = 1 then
		begin
			writeln(outfile,#9, 'STORE.B ', offset, ' ; ', name);
			countIns(1);
		end
		else
		begin
			emitIns('STOREI');
			emitIns('DROP');
		end;
	end
	else
	begin
		(* if it is not a short store, the address is already on the stack *)
		emitIns('STOREI');
		emitIns('DROP');
	end
end;

procedure emitLoadNestedAddr(var name: IdentString; distance, offset: integer);
var i:integer;
begin
	writeln(outfile,#9,' ; ', name);
	if distance = 1 then
		emitIns2('LOADREG', 'BP')
	else
	begin
		emitIns2Int('LOAD.B', 0);
		if distance > 2 then
		begin
			for i := 3 to distance do
				emitIns('LOADI');
		end;
	end;
	emitInc(offset);
end;

procedure emitStoreArg(sym: SymblRef);
begin
	emitStoreLocal(sym^.offset, sym^.name);
end;

function isLocalIndirect(var loc:MemLocation):boolean;
begin
	isLocalIndirect := loc.offset > MaxUShortOffset;
end;

(* place address of a local variable on stack for accessing it later.
   this only emits code if the offset is greater than MaxUShortOffset.
   otherwise, for accessing the variable LOAD or STORE is used and
   no address on the stack is needed. *)
procedure emitLocalMemLoc(var loc:MemLocation);
begin
	if  isLocalIndirect(loc) then
		emitLoadLocalAddr(loc.name, loc.offset);
end;

function isNestedIndirect(var loc:MemLocation):boolean;
begin
	isNestedIndirect := (loc.offset > MaxUShortOffset) or (loc.scopeDistance > 1);
end;

(* Place address of a nested variable on stack for accessing it later.
   This only emits code if the offset is greater than MaxUShortOffset,
   or if the variable is from a distant outer scope (distance > 1).
   Otherwise, for accessing the variable LOAD or STORE is used and
   no address on the stack is needed. *)
procedure emitNestedMemLoc(var loc:MemLocation);
begin
	if isNestedIndirect(loc) then
		emitLoadNestedAddr(loc.name, loc.scopeDistance, loc.offset);
end;

procedure emitLoadGlobalAddr(var name: IdentString; offset: integer);
begin
	if offset = 0 then
		writeln(outfile,#9, 'LOADCP ', name, ' ; ', name)
	else
		(* using the LOADCP constant with offset syntax *)
		writeln(outfile,#9, 'LOADCP ', name, ',', offset, ' ; ', name);
	countIns(1);
end;

procedure emitLoadTempAddr(var name: IdentString; offset: integer);
begin
	if offset <= 0 then
		errorExit2('internal error: invalid temporary offset', name)
	else
	begin
		emitIns2('LOADREG', 'FP');
		emitDec(offset);
	end;
end;

procedure emitWithStmntMemLoc(var loc:MemLocation; withSlot: integer);
var offset: integer;
begin
	offset := withStmntStack[withSlot].tempLoc.offset;
	emitLoadTempAddr(loc.name, offset);
	emitIns('LOADI');
	emitInc(loc.offset);
end;

procedure emitLoadLocal(offset: integer; var name: IdentString);
begin
	if offset <= MaxUShortOffset then
	begin
		writeln(outfile,#9, 'LOAD ', offset, ' ; ', name);
		countIns(1);
	end
	else
	begin
		(* if it is not a short load, the address is already on stack *)
		emitIns('LOADI');
	end;
end;

procedure emitLoadNested(offset: integer; distance:integer; var name: IdentString);
begin
	if offset <= MaxUShortOffset then
	begin
		if distance = 1 then
		begin
			writeln(outfile,#9, 'LOAD.B ', offset, ' ; ', name);
			countIns(1);
		end
		else
		begin
			emitIns('LOADI');
		end;
	end
	else
	begin
		(* if it is not a short load, the address is already on stack *)
		emitIns('LOADI');
	end;
end;

procedure emitShiftLeft(count: Integer);
var d: Integer;
begin
	while count > 0 do
	begin
		if count >= 8 then
		begin
			emitIns('BROT');
			emitIns2('LOADC', '-$100'); (* $FFFFFF00 *)
			emitIns('AND');
			d := 8;
		end
		else
		if count >= 2 then
		begin
			emitIns2('SHL','2');
			d := 2;
		end
		else
		begin
			emitIns('SHL');
			d := 1;
		end;
		count := count - d;
	end;
end;

(*
	try to emit code for quickly multiplying
	numbers by shifting, used for array indices.
	uses naive heuristics to convert powers of two
	to shifts, otherwise uses the multiply routine.
*)
procedure emitFastMul(fac: integer);
begin
	if fac = 1024 then emitShiftLeft(10)
	else if fac = 512 then emitShiftLeft(9)
	else if fac = 256 then emitShiftLeft(8)
	else if fac = 128 then emitShiftLeft(7)
	else if fac = 64 then emitShiftLeft(6)
	else if fac = 32 then emitShiftLeft(5)
	else if fac = 16 then emitShiftLeft(4)
	else if fac = 8 then emitShiftLeft(3)
	else if fac = 4 then emitShiftLeft(2)
	else
	begin
		emitLoadConstantInt(fac);
		emitOperator('MULU');
	end;
end;

(* emit code to calculate the address of an array element.
	the address and the index number are already on stack.
	also emits code for a bounds check, so we must know
	the array type.*)
procedure emitIndexToAddr(var symType: TypeSpec);
begin
	if symType.arrayStart <> 0 then
		(* adjust index to base 0 *)
		emitDec(symType.arrayStart);
	emitIns('DUP');
	emitLoadConstantInt(symType.arrayLength);
	emitCallRaw('_BOUNDSCHECK');
	emitFastMul(symtype.elementType^.size);
	emitIns('ADD');
end;

(* Emit code to calculate the byte address of an indexed string
    (i.e. char at a specific position in the string).
	The address and the index value are already on stack.
	Does a bounds check.
	Leaves the byte address on stack. *)
procedure emitStringIndexToAddr;
begin
	emitCallRaw('_INDEXSTRING');
end;

procedure emitSubrangeCheckRaw(min,max:integer);
begin
	emitLoadConstantInt(min);
	emitLoadConstantInt(max);
	emitCall('_RANGECHECK');
end;

procedure emitSubrangeCheck(min,max:integer);
begin
	emitDup; (* duplicate the value that is being checked *)
	emitSubrangeCheckRaw(min,max);
end;

procedure emitEnumCheck(max:integer);
begin
	emitDup; (* duplicate the value that is being checked *)
	emitLoadConstantInt(max);
	emitCall('_ENUMCHECK');
end;

procedure emitLoadStringChar;
begin
	emitIns('LOADI.S1.X2Y');
	emitIns('BSEL');
end;

procedure emitSetStringChar;
begin
	emitCallRaw('_SETSTRINGCHAR');
end;

procedure emitSetStringLength;
begin
	emitCallRaw('_SETSTRINGLENGTH');
end;

procedure emitLoadIndirect;
begin
	emitIns('LOADI');
end;

procedure emitStoreIndirect;
begin
	emitIns('STOREI');
	emitIns('DROP');
end;

procedure emitFpAdjust(offset: integer);
begin
	if abs(offset) > 0 then
	begin
		if abs(offset) > MaxShorterOffset then
		begin
			emitIns2('LOADREG', 'FP');
			if offset < 0 then
			begin
				emitLoadConstantInt(-offset);
				emitIns('SUB');			
			end
			else
			begin
				emitLoadConstantInt(offset);
				emitIns('ADD');
			end;
			emitIns2('STOREREG', 'FP');
		end
		else
			emitIns2Int('FPADJ', offset);
	end;
end;

procedure emitCallRaw(name:string);
begin
	emitIns2('LOADCP', name);
	emitIns('CALL');
end;

procedure emitCall(name:string);
var tempsSize:integer;
begin
	tempsSize := curProcedure^.tempsSize;
	emitFpAdjust(-tempsSize);
	emitIns2('LOADCP', name);
	emitIns('CALL');
	emitFpAdjust(tempsSize);
end;

procedure emitCopy(bytes: integer);
begin
	emitLoadConstantInt(bytes div wordSize);
	emitCall('_COPYWORDS');
end;

procedure clearLocalVar(sym:SymblRef);
begin
	emitLoadLocalAddr(sym^.name, sym^.offset);
	emitLoadConstantInt(sym^.size);
	emitCallRaw('_CLEARMEM');
end;

procedure emitClearAlloc(typePtr:TypeSpecPtr);
begin
	emitDup;
	emitLoadConstantInt(typePtr^.size);
	emitCallRaw('_CLEARMEM');
end;

procedure emitCheckAlloc;
begin
	(* TODO: change back to emitCallRaw when
		_CHECK_ALLOC does not use the program stack anymore
		(that is, if it does not call _CHECK_CHUNK )*)
	emitCall('_CHECK_ALLOC');
end;

procedure emitMemAlloc;
begin
	emitCall('_MEM_ALLOC');
end;

procedure emitMemFree;
begin
	emitCall('_MEM_FREE');
end;

(* requires char value and pointer to string buf already on stack,
   leaves the string ptr *)
procedure emitConvCharToString;
begin
	(* we need to leave the buffer addr on stack *)
	emitSwap;
	emitIns('OVER');
	(* after this, we have [ bufaddr, char, bufaddr ] on stack *)
	emitCallRaw('_CHARTOSTRING');
end;

(* requires a string pointer on the stack, leaves a char value*)
procedure emitConvStringToChar;
begin
	emitCallRaw('_STRINGTOCHAR');
end;

procedure emitInitTempString(var name: IdentString; offset, length: integer);
begin
	emitLoadConstantInt(length);
	emitLoadTempAddr(name, offset);
	emitCallRaw('_INITSTRINGF');
end;

procedure emitForceInitString(var name: IdentString; offset, length: integer);
begin
	emitLoadConstantInt(length);
	emitLoadLocalAddr(name, offset);
	emitCallRaw('_INITSTRINGF');
end;

procedure emitInitString(var name: IdentString; offset, length: integer);
begin
	emitLoadConstantInt(length);
	emitLoadLocalAddr(name, offset);
	emitCallRaw('_INITSTRING');
end;

(* variant of emitInitString where the address is already on the stack *)
procedure emitInitStringShort(length: integer);
begin
	emitLoadConstantInt(length);
	emitIns('OVER');
	emitCall('_INITSTRING');
end;

(* variant of emitInitString where the address is already in next-to-top *)
(* which is only used for read/readln *)
procedure emitInitStringSwapped(length: integer);
begin
	emitIns('OVER');
	emitLoadConstantInt(length);
	emitSwap;
	emitCall('_INITSTRING');
end;

procedure emitInitStringFrom(length: integer);
begin
	emitLoadConstantInt(length);
	emitCall('_INITSTRINGFROM');
end;

(* on the stack: [ max string length ] *)
procedure emitStringAlloc;
begin
	emitCall('_STRING_ALLOC'); (* [ addr ]*)
end;

procedure emitCopyString;
begin
	emitCall('_COPYSTRING');
end;

procedure emitAppendString;
begin
	emitCall('_APPENDSTRING');
end;

procedure emitLabelRaw(name:IdentString);
begin
	writeln(outfile, name,':');
end;

procedure emitLabel(aLabl: LablRef);
begin
	writeln(outfile, '_L',aLabl^.id,aLabl^.name, globalSuffix, ':');
end;

procedure emitLabelJump(aLabl: LablRef);
begin
	(* use .LBRANCH directive instead of BRANCH so the assembler
	    can use the JUMP instruction if the offset is too large for BRANCH *)
	writeln(outfile,#9, '.LBRANCH', ' ', '_L',aLabl^.id,aLabl^.name, globalSuffix);
	countIns(5);
	CPoolIfLowMark(false);
end;

(* TODO: make this useful for normal and nested procedures,
   and remove the "if aProc^.isNested then getProcedureLabel" stuff *)
procedure getProcedureLabel(aProc:ProcRef;var dest:IdentString);
var numberStr:string[8];
begin
	if aProc^.isNested then
	begin
		str(aProc^.id, numberStr);
		dest := '_NST' + globalSuffix + numberStr + aProc^.name;
	end
	else
		dest := aProc^.name;
end;

function getProcFsLabel(aProc:ProcRef):IdentString;
begin
	getProcFsLabel := aProc^.name + '_FS_';
end;

function getExitLabel(aProc:ProcRef):IdentString;
begin
	getProcedureLabel(aProc, getExitLabel);
	getExitLabel := getExitLabel + '_XT';
end;

procedure emitExitLabel(aProc:ProcRef);
begin
	emitLabelRaw(getExitLabel(aProc));
end;

procedure emitProcedurePrologue(aProc:ProcRef);
 var procLabel: IdentString;
begin
	getProcedureLabel(aProc, procLabel);
	emitLabelRaw(procLabel);

	emitFpAdjust(-aProc^.vars.offset);

	if aProc^.isNested then
	begin
		(* store old BP at offset 4 and
			pointer to outer frame at offset 0 *)
		emitIns2('LOADREG','BP');
		emitIns2('STORE','4');
		emitDup;
		emitIns2('STOREREG', 'BP');
		emitIns2('STORE','0');
	end;
end;

procedure emitProcedureEpilogue(aProc:ProcRef);
begin
	if aProc^.isNested then
	begin
		(* restore old BP when exiting a nested procedure *)
		emitIns2('LOAD','4');
		emitIns2('STOREREG','BP');
	end;

	emitFpAdjust(aProc^.vars.offset);
	emitIns('RET');
	CPoolIfLowMark(false);
end;

procedure emitExit(aProc:ProcRef);
var i:integer;
begin
	(* clean up estack *)
	for i := 1 to aProc^.estackCleanup do
		emitIns('DROP');
	emitIns2('.LBRANCH', getExitLabel(aProc));
end;

(* Call a procedure.
   the FP register must be adjusted before and after to
   account for temporaries used by the caller.
   When calling from a nested procedure, we need to restore
   BP register after a call (because it is possible that the called
   procedure called another nested procedure and therefore BP was changed.
   See emitProcedureEpilogue above.
   *)
procedure emitProcedureCall(aProc: ProcRef);
var procLabel: IdentString;
begin
	(* pass pointer to stackframe of caller for nested procedures *)
	if aProc^.isNested then
	begin
		if aProc^.level = curProcedure^.level then
			emitIns2('LOADREG', 'BP')
		else
		if aProc^.level > curProcedure^.level then
			emitIns2('LOADREG','FP')
		else
			(* TODO: calling nested aProc with a lower nesting level.
				need to chase a chain of old BP pointers. *)
			errorExit2('internal error: outward call of nested aProc not implemented', '');
	end;

	emitFpAdjust(-curProcedure^.tempsSize);

	if aProc^.isNested then
	begin
		getProcedureLabel(aProc, procLabel);
		emitIns2('LOADCP', procLabel);
	end
	else
		emitIns2('LOADCP', aProc^.name);
	emitIns('CALL');

	emitFpAdjust(curProcedure^.tempsSize);
end;

procedure emitFunctionValueReturn(sym: SymblRef);
begin
	emitLoadLocal(sym^.offset, sym^.name);
end;

procedure emitStrCall(typeTag:TypeTagString);
begin
	writeln(outfile, #9, 'LOADCP ', typeTag,'STR');
	countIns(1);
	writeln(outfile, #9, 'CALL');
end;

procedure emitValCall(typeTag:TypeTagString);
begin
	writeln(outfile, #9, 'LOADCP ', typeTag,'VAL');
	countIns(1);
	writeln(outfile, #9, 'CALL');
end;

procedure emitLoadConstStr(c: ConstStrRef);
begin
	writeln(outfile, #9, 'LOADCP ', getLocalLabel('_C_S_',c^.no));
	countIns(1);
end;

procedure emitLoadArrayConst(c: ArrayConstRef);
begin
	writeln(outfile, #9, 'LOADCP ', getLocalLabel('_C_A_', c^.id));
end;

procedure emitOperator(op: string);
begin
	if (op = 'MUL') or (op = 'MULU') or (op = 'DIV') or (op = 'DIVU') or (op = 'MOD') then
		emitCall('_' + op)
	else
		emitIns(op);
end;

procedure emitShiftMultiple(op: string);
begin
	emitCallRaw('_' + op);
end;

procedure emitFloatOperator(op: string);
begin
	emitCall('_' + op + 'FLOAT32');
end;

procedure emitTruncFloat;
begin
	emitCall('_TRUNCFLOAT32');
end;

procedure emitFractFloat;
begin
	emitCall('_FRACTFLOAT32');
end;

procedure emitIntFloat;
begin
	emitCall('_INTFLOAT32');
end;

procedure emitSqrInt;
begin
	emitDup;
	emitOperator('MUL');
end;

procedure emitSqrFloat;
begin
	emitDup;
	emitFloatOperator('MUL');
end;

procedure emitIntToFloat;
begin
	emitCall('_INTTOFLOAT32');
end;

procedure emitComparison(op: string);
begin
	emitIns2('CMP', op);
end;

procedure emitFloatComparison(op: string);
begin
	emitCall('_CMPFLOAT32');
	emitLoadConstantInt(0);
	emitComparison(op);
end;

procedure emitIntFloatComparison(op: string);
begin
	emitCall('_CMPINTFLOAT32');
	emitLoadConstantInt(0);
	emitComparison(op);
end;

procedure emitStringComparison;
begin
	emitCall('_CMPSTRING');
end;

procedure emitStringLexiComparison(op: string);
begin
	emitCall('_CMPSTRINGL');
	emitLoadConstantInt(0);
	emitComparison(op);
end;

procedure emitMemComparison(var typ: TypeSpec);
begin
	emitLoadConstantInt(typ.size div wordSize);
	emitCall('_CMPWORDS');
end;

procedure emitIsInArray(count:integer);
begin
	emitLoadConstantInt(count);
	emitCall('_ISINTINARRAY');
end;

procedure emitIsInString;
begin
	emitCall('_ISCHARINSTRING');
end;


procedure emitIsInSet;
begin
	emitCallRaw('_TESTBIT');
end;

procedure emitAddToSet;
begin
	emitCallRaw('_SETBIT');
end;

procedure emitRemoveFromSet;
begin
	emitCallRaw('_CLEARBIT');
end;

procedure emitArrayToSet(len:integer);
begin
	emitLoadConstantInt(len);
	emitCall('_ARRAYTOSET');
end;

(* emitInc and emitDec emit different instruction sequences
   depending on the amount: For a zero amount, nothing is emitted,
   for small values INC/DEC are used, otherwise LOADC/LOADCP and
	ADD/SUB *)

procedure emitInc(amount: integer);
begin
	if amount = 0 then
	begin
		(* nothing to do *)
	end
	else
	if amount <= MaxTinyOffset then
		emitIns2Int('INC', amount)
	else
	begin
		emitLoadConstantInt(amount);
		emitIns('ADD');
	end;
end;

procedure emitDec(amount: integer);
begin
	if amount = 0 then
	begin
		(* nothing to do *)
	end
	else
	if amount <= MaxTinyOffset then
		emitIns2Int('DEC', amount)
	else
	begin
		emitLoadConstantInt(amount);
		emitIns('SUB');
	end;
end;

procedure emitNegate;
begin
	emitOperator('NOT');
	emitInc(1);
end;

procedure emitAbsInt;
begin
	emitCallRaw('ABS');
end;

procedure emitBooleanNot;
begin
	emitIns2Int('LOADC', 0);
	emitIns('CMP EQ');
end;

procedure emitNot;
begin
	emitIns('NOT');
end;

procedure emitOdd;
begin
	emitIns2Int('LOADC',1);
	emitIns('AND');
end;

procedure emitSetAdd;
begin
	emitIns('OR');
end;

procedure emitSetSubtract;
begin
	emitIns('NOT');
	emitIns('AND');
end;

procedure emitSetIntersect;
begin
	emitIns('AND');
end;

procedure emitSetCompare;
begin
	emitIns('CMP EQ');
end;

procedure emitSetCompareNE;
begin
	emitIns('CMP NE');
end;

procedure emitSetIsSubset;
begin
	emitSetSubtract;
	emitBooleanNot;
end;

procedure emitIfBranch(no: integer);
begin
	writeln(outfile, #9, '.LCBRANCHZ ', getLocalLabel('_IF_ELSE', no));
	countIns(6);
end;

procedure emitElseBranch(no: integer);
begin
	writeln(outfile, #9, '.LBRANCH ', getLocalLabel('_IF_END', no));
	countIns(5); (* worst case for .LBRANCH is 10 bytes *)
	CPoolIfLowMark(false);
end;

procedure emitIfLabel(no: integer);
begin
	emitLocalLabel('_IF_END', no);
end;

procedure emitElseLabel(no: integer);
begin
	emitLocalLabel('_IF_ELSE', no);
end;

procedure emitWhileStart(no: integer);
begin
	emitLocalLabel('_WHILE_START', no);
end;

procedure emitWhileBranch(no: integer);
begin
	writeln(outfile, #9, '.LCBRANCHZ ', getLocalLabel('_WHILE_END', no));
	countIns(6);
end;

procedure emitWhileEnd(no: integer);
begin
	writeln(outfile, #9, '.LBRANCH ', getLocalLabel('_WHILE_START', no));
	countIns(5);
	CPoolIfLowMark(false);
	emitLocalLabel('_WHILE_END', no);
end;

function getEndLabel(name:IdentString; no: integer):IdentString;
var nstr: string[24];
begin
	str(no, nstr);
	getEndLabel := '_' + name + '_END' + nstr + globalSuffix;
end;

function getWhileEndLabel(no: integer):IdentString;
begin
	getWhileEndLabel := getEndLabel('WHILE', no);
end;

procedure emitRepeatStart(no: integer);
begin
	emitLocalLabel('_REPEAT_START', no);
end;

procedure emitRepeatBranch(no: integer);
begin
	writeln(outfile, #9, '.LCBRANCHZ ', getLocalLabel('_REPEAT_START', no));
	countIns(6);
end;

procedure emitRepeatEnd(no: integer);
begin
	emitLocalLabel('_REPEAT_END', no);
end;

function getRepeatEndLabel(no: integer):IdentString;
begin
	getRepeatEndLabel := getEndLabel('REPEAT', no);
end;

procedure emitForStart(no: integer);
begin
	curProcedure^.estackCleanup := curProcedure^.estackCleanup + 1;
	emitLocalLabel('_FOR_START', no);
end;

procedure emitForBranch(no: integer);
begin
	emitIns('OVER');
	emitComparison('GT');
	writeln(outfile, #9, '.LCBRANCH ', getLocalLabel('_FOR_END', no));
	countIns(6);
end;

procedure emitForDowntoBranch(no: integer);
begin
	emitIns('OVER');
	emitComparison('LT');
	writeln(outfile, #9, '.LCBRANCH ', getLocalLabel('_FOR_END', no));
	countIns(6);
end;

procedure emitForEnd(no: integer);
begin
	writeln(outfile, #9, '.LBRANCH ', getLocalLabel('_FOR_START', no));
	countIns(5);
	CPoolIfLowMark(false);
	emitLocalLabel('_FOR_END', no);
	emitIns('DROP');
	curProcedure^.estackCleanup := curProcedure^.estackCleanup - 1;
end;

procedure emitForInStrHeader;
begin
	emitDup;
	emitLoadIndirect;
	emitSwap;
	emitInc(StringHeaderSize);
end;

procedure emitForInHeader(count:integer);
begin
	emitLoadConstantInt(count);
	emitSwap;
end;

procedure emitForInStart(no:integer);
begin
	emitForStart(no);
	(* emitForStart increments estackCleanup by one, for in uses two
		estack elements so add one more *)
	curProcedure^.estackCleanup := curProcedure^.estackCleanup + 1;
	emitIns('OVER');
	writeln(outfile, #9, '.LCBRANCHZ ', getLocalLabel('_FOR_END', no));
	countIns(6);
end;

(* sym and mem are the symbol reference and memory location
	of the loop variable *)
procedure emitForInStrMid(sym:SymblRef; mem:MemLocation);
begin
	(* if the loop variable is a local variable and can be accessed
		with a short load/store, then the stack layout at this point is:
			[ count, char ptr ]
		Otherwise, it is:
			[ count, char ptr, loop var addr ]
		so we have to use different instructions for each case *)
	if isShortLoadStore(mem) then
		emitIns('DUP')
	else
		emitIns('OVER');
	emitIns('LOADI.S1.X2Y');
	emitIns('BSEL');
end;

(* sym and mem are the symbol reference and memory location
	of the loop variable *)
procedure emitForInMid(sym:SymblRef; srcMem:MemLocation);
begin
	(* if the loop variable is a local variable and can be accessed
		with a short load/store, then the stack layout at this point is:
			[ count, char ptr ]
		Otherwise, it is:
			[ count, char ptr, loop var addr ]
		so we have to use different instructions for each case *)
	if isScalar(sym^.symType) and isShortLoadStore(srcMem) then
		emitIns('DUP')
	else
		emitIns('OVER');
end;

procedure emitForInStrIter(no:integer);
begin
	emitInc(1);
	emitSwap;
	emitDec(1);
	emitSwap;
	writeln(outfile, #9, '.LBRANCH ', getLocalLabel('_FOR_START',no));
	countIns(5);
end;

procedure emitForInIter(no:integer; var typ:TypeSpec);
begin
	emitInc(typ.elementType^.size);
	emitSwap;
	emitDec(1);
	emitSwap;
	writeln(outfile, #9, '.LBRANCH ', getLocalLabel('_FOR_START',no));
	countIns(5);
end;

procedure emitForInEnd(no:integer);
begin
	CPoolIfLowMark(false);
	emitLocalLabel('_FOR_END',no);
	emitIns('DROP');
	emitIns('DROP');
	curProcedure^.estackCleanup := curProcedure^.estackCleanup - 2;
end;

function getForEndLabel(no: integer):IdentString;
begin
	getForEndLabel := getEndLabel('FOR', no);
end;

procedure emitCaseStart(no:integer);
begin
	curProcedure^.estackCleanup := curProcedure^.estackCleanup + 1;
end;

procedure emitCaseLabelLabel(no,valueNo,subVal: integer);
begin
	writeln(outfile, '_CASE_', no,'_', valueNo, '_', subVal, globalSuffix, ':');
end;

procedure emitCaseLabelStart(no,valueNo,subVal: integer);
begin
	emitCaseLabelLabel(no, valueNo, subVal);
	emitDup;
end;

procedure emitCaseLabelBranchOp(cmpOp:string; no, valueNo, subVal: integer; last: boolean);
begin
	emitIns2('CMP', cmpOp);
	writeln(outfile, #9, '.LCBRANCH ', '_CASE_', no, '_', valueNo, '_', subVal + 1,
		globalSuffix);
	countIns(5);
	if not last then
	begin
		writeln(outfile, #9, '.LBRANCH ', '_CASE_', no, '_', valueNo, 'M', globalSuffix);
		countIns(5);
	end
end;

procedure emitCaseLabelBranch(no, valueNo, subVal: integer; last: boolean);
begin
	emitCaseLabelBranchOp('NE', no, valueNo, subVal, last);
end;

procedure emitCaseRangeLoBranch(no, valueNo, subVal: integer; last: boolean);
begin
	emitCaseLabelBranchOp('LT', no, valueNo, subVal, last);
	emitDup;
end;

procedure emitCaseRangeHiBranch(no, valueNo, subVal: integer; last: boolean);
begin
	emitCaseLabelBranchOp('GT', no, valueNo, subVal, last);
end;

procedure emitCaseLabelMatch(no, valueNo: integer);
begin
	writeln(outfile, '_CASE_', no, '_', valueNo,'M', globalSuffix, ':');
end;

procedure emitCaseLabelEnd(no: integer);
begin
	writeln(outfile,  #9, '.LBRANCH ', '_CASE_', no, globalSuffix, '_END');
	countIns(5);
	CPoolIfLowMark(false);
end;

procedure emitCaseEnd(no, valueNo: integer);
begin
	writeln(outfile, '_CASE_', no, '_', valueNo, globalSuffix, ':');
	writeln(outfile, '_CASE_', no, globalSuffix, '_END', ':');
	emitIns('DROP');
	curProcedure^.estackCleanup := curProcedure^.estackCleanup - 1;
end;

procedure emitBreak(var aLabl:IdentString);
begin
	emitIns2('.LBRANCH', aLabl);
	countIns(4); (* worst case for .LBRANCH is 10 bytes *)
end;

procedure emitAbsFloat32;
begin
	emitIns2('LOADCP','$7FFFFFFF');
	emitIns('AND');
end;

procedure emitNegFloat32;
begin
	emitCallRaw('_NEGFLOAT32');
	(* alternatively, just emit the
		code for it:
    emitDup;
	emitIns2('CBRANCH.Z', '@+6');
	emitIns2('LOADCP','$80000000');
	emitIns('XOR');
	*)
end;
