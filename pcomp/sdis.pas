(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
program sdis;
{$R+}
{$MODE objfpc}
type
	InputFileType = file of char;
	OutputFileType = text;

	KeywordString = string[128];
	IdentString = string[80];

	SymbolType = (ConstSymbol, LabelSymbol, SpecialSymbol);


	Symbol = record
		name:KeywordString;
		value:integer;
		typ:SymbolType;
	end;

	HashEntry = record
		key:integer;
		data:IdentString;
		next:^HashEntry;
	end;

	HashRef = ^HashEntry;
	HashBucket = ^HashEntry;
	HashTable = array [0..255] of HashBucket;

var infile:InputfileType;
    filename:string;
    pc:integer;
    symbolTable: HashTable;

procedure errorExit;
begin
	close(infile);
	halt;
end;

procedure errorExit2(message1, message2: string);
begin
	writeln;
	writeln('Error: ', message1, ' ', message2);
	errorExit;
end;

procedure openFile(var f:InputFileType;var filename:string);
begin
{$I-}
	assign(f, filename);
	reset(f);

	if IOResult <> 0 then
		errorExit2('cannot open file ', filename);
{$I+}
end;

function readChar:char;
var c:char;
begin
	read(infile,c);
	readChar := c;
end;

procedure readEol;
var c:char;
begin
	c := readChar;
	if c = #13 then
	begin
		c := readChar;
	end;
end;


function readBin(len:integer):integer;
var i,v:integer;
    c:char;
begin
	v := 0;
	for i := 1 to len do
	begin
		c := readChar;
		v := v shl 8;
		v := v or ord(c);
	end;
	readBin := v;
end;

function readAsciiBin(len:integer):integer;
var i:integer;
    w:integer;
    c:char;
    bits:integer;
begin
	bits := len * 8;
	w := 0;
	for i := 1 to bits do
	begin
		w := w shl 1;
		c := readChar;
		if c = '1' then
			w := w or 1
		else
		if c = '0' then begin end;
	end;
	readAsciiBin := w;

	(* read end of line *)
	if (pc and 3) = 2 then
		readEol;
end;

function readBytes(len:integer):integer;
var w:integer;
    c:char;
    i:integer;
begin
	w := 0;
	for i := 1 to len do
	begin
		read(infile, c);
		w := (w shl 8) or ord(c);
	end;

	readBytes := w;
	writeln('readBytes ',len, ': ', w);
end;

function readInstruction:integer;
begin
	(* readInstruction := readBytes(2); *)
	readInstruction := readBin(2);
end;

function readWord:integer;
begin
	readWord := readBin(4);
end;

function convertHex(var digits:KeywordString):integer;
var i,v,len:integer;
    c:char;
begin
	len := length(digits);

	i := 1;
	convertHex := 0;

	while i <= len do
	begin
		convertHex := convertHex shl 4;
		c := UpCase(digits[i]);
		if (c >= 'A') and (c <= 'F') then
			v := ord(c) - ord('A') + 10
		else
		if (c >= '0') and (c <= '9') then
			v := ord(c) - ord('0')
		else
			errorExit2('invalid number',digits);
		convertHex := convertHex + v;
		i := i + 1;
	end;
end;

procedure hexstr(value:integer;var output:string);
var i:integer;
    nibble:integer;
    c:char;
begin
	output := '00000000';

	for i := 8 downto 1 do
	begin
		nibble := value and $F;
		if nibble > 9 then
			c := chr( ord('A') + nibble - 10)
		else
			c := chr( ord('0') + nibble);

		output[i] := c;
		value := value shr 4;
		if value = 0 then break;
	end;
end;

procedure writeHex(value:integer);
var s:string;
begin
	hexstr(value,s);
	write('$',s);
end;

procedure printAsciiWord(w:integer);
var i:integer;
    c:char;
begin
	write('"');
	for i := 1 to 4 do
	begin
		c := chr(((w shr 24) and $FF));
		w := w shl 8;
		if (c < ' ') or (c > '~') then
			c := '.';
		write(c);
	end;
	write('"');
end;


{$R-}
(* disable range checks for 32-bit hash functions *)

(* hash a 32-bit integer into an 8-bit integer *)
function hashint(value:integer):integer;
var i:integer;
begin
	hashint := 0;
	value := value xor $B298AB49; (* some random 32-bit constant *)
	for i := 1 to 4 do
	begin
		hashint := hashint xor (value and $FF);
		value := value shr 8;
	end;
end;

{$R+}

procedure putHashed(var t:HashTable;key:integer;var data:KeywordString);
var i:integer;
    newEntry:^HashEntry;
    bucket:HashBucket;
begin
	new(newEntry);
	newEntry^.data := data;
	newEntry^.key := key;

	i := hashint(key);
	bucket := t[i];
	newEntry^.next := bucket;
	t[i] := newEntry;
end;

function getHashed(var t:HashTable;key:integer):HashRef;
var bucket:HashBucket;
    current:^HashEntry;
    found:boolean;
begin
	getHashed := nil;
	bucket := t[hashint(key)];
	current := bucket;
	found := false;

	while (current <> nil) and not found do
	begin
		if current^.key = key then
		begin
			getHashed := current;
			found := true;
		end;
		current := current^.next;
	end;
end;

function getHashBucket(var t:HashTable;key:integer):HashRef;
begin
	getHashBucket := t[hashint(key)];
end;

procedure dumpHash(var t:HashTable);
var i:integer;
    bucket:HashBucket;
    current:HashRef;
begin
	for i := 0 to 255 do
	begin
		write('bucket ',i:4, ' ');
		bucket := t[i];
		current := bucket;
		while current <> nil do
		begin
			write(current^.key, ':', current^.data, ' ');
			current := current^.next;
		end;
		writeln;
	end;
end;

procedure printEol;
begin
	writeln;
end;

procedure printHex(value:integer);
var s:string[8];
begin
	write('$');
	hexstr(value, s);
	write(s);
end;

procedure printOperand(operand:integer);
var sym:HashRef;
begin
	sym := getHashed(symbolTable, operand);
	if sym <> nil then
	begin
		write(sym^.data);
		write('  ; ');
	end;

	printHex(operand);
end;

procedure printSpacedOperand(operand:integer);
begin
	write(' ');
	printOperand(operand);
end;

(* operates on numbers with less than 32 bits, signmask indicates the
	highest bit which is the sign *)
function makepositive(operand, signmask:integer):integer;
begin
	if (operand and signmask) <> 0 then
		makepositive := signmask - (operand and (not signmask))
	else
		makepositive := operand;
end;

function signExtend(operand, signmask:integer):integer;
begin
	if (operand and signmask) <> 0 then
		signExtend := -(signmask - (operand and (not signmask)))
	else
		signExtend := operand;
end;

procedure printSignedOperand(operand, signmask:integer);
var sym:HashRef;
begin
	write(' ');
	sym := getHashed(symbolTable, operand);
	if sym <> nil then
		write(sym^.data)
	else
	begin
		if operand and signmask <> 0 then
		begin
			write('-');
			operand := makepositive(operand, signmask);
		end;
		printHex(operand);
	end;
end;

procedure decodeBranch(operand:integer);
begin
	write('BRANCH');
	printSpacedOperand(pc + signExtend(operand, $1000));
end;

procedure decodeCbranch(operand:integer);
begin
	write('CBRANCH');
	if (operand and 1) = 1 then
		write('.NZ')
	else
		write('.Z');

	printSpacedOperand(pc + signExtend((operand and $FFFE), $100));
end;


procedure decodeLoadc(operand:integer);
begin
	write('LOADC');
	printSignedOperand(operand, $1000);
end;

procedure decodeLoadStore(name:string; operand:integer);
begin
	write(name);
	if (operand and 1) = 1 then
		write('.B');

	printSpacedOperand(operand and $FFFE);
end;

procedure decodeModifier(name:string;value, mask:integer; operand:integer; visible:boolean);
begin
	if (operand and mask) = value then
		if visible then
			write('.',name);
end;

procedure decodeXfer(operand:integer);
begin
	write('XFER');
	decodeModifier('RSM1', $0300, $0300, operand, true);
	decodeModifier('RS0',  $0000, $0300, operand, false);
	decodeModifier('RS1',  $0100, $0300, operand, true);
	decodeModifier('R2P',  $0080, $0080, operand, true);
	decodeModifier('P2R',  $0040, $0040, operand, true);
	decodeModifier('SM1',  $0030, $0030, operand, true);
	decodeModifier('S0',   $0000, $0030, operand, false);
	decodeModifier('S1',   $0010, $0030, operand, true);
	decodeModifier('X2P',  $0001, $0001, operand, true);
end;

procedure printCmpOperand(operand:integer);
begin
	case operand of
		2: write('EQ');
		6: write('NE');
		1: write('LT');
		3: write('LE');
		5: write('GE');
		7: write('GT');
		else write('<unknown:',operand,'>');
	end;
end;

procedure decodeAlu(operand:integer);
var aluop:integer;
begin
	write('ALU');
	decodeModifier('ADD',  $0000, $1e00, operand, true);
	decodeModifier('SUB',  $0200, $1e00, operand, true);
	decodeModifier('NOT',  $0400, $1e00, operand, true);
	decodeModifier('AND',  $0600, $1e00, operand, true);
	decodeModifier('OR',   $0800, $1e00, operand, true);
	decodeModifier('XOR',  $0a00, $1e00, operand, true);
	decodeModifier('CMP',  $0c00, $1e00, operand, true);
	decodeModifier('Y',    $0e00, $1e00, operand, true);
	decodeModifier('SHR',  $1000, $1e00, operand, true);
	decodeModifier('SHL',  $1200, $1e00, operand, true);
	decodeModifier('INC',  $1400, $1e00, operand, true);
	decodeModifier('DEC',  $1600, $1e00, operand, true);
	decodeModifier('BPLC', $1a00, $1e00, operand, true);
	decodeModifier('BROT', $1c00, $1e00, operand, true);
	decodeModifier('BSEL', $1e00, $1e00, operand, true);
	decodeModifier('CMPU', $1800, $1e00, operand, true);

	decodeModifier('SM1',  $0030, $0030, operand, true);
	decodeModifier('S0',   $0000, $0030, operand, false);
	decodeModifier('S1',   $0010, $0030, operand, true);
	decodeModifier('X2Y',  $0040, $0040, operand, true);
	decodeModifier('NX2Y', $0000, $0040, operand, false);
	decodeModifier('XT',   $0080, $0080, operand, true);

	aluop := operand and $1e00;
	operand := operand and 15;

	if (aluop = $1800) or (aluop = $0c00) then
	begin
		write(' ');
		printCmpOperand(operand);
	end
	else
	if operand > 0 then
		printSpacedOperand(operand);
end;

procedure decodeLoadrel(offset:integer);
begin
	write('LOADREL');
	printSpacedOperand(pc + offset);
end;

procedure decodeMem(operand:integer);
begin
	if (operand and $0200) <> 0 then
	begin
		write('STOREI');
		decodeModifier('SM1',  $0030, $0030, operand, false);
		decodeModifier('S0',   $0000, $0030, operand, true);
		decodeModifier('S1',   $0010, $0030, operand, true);
		decodeModifier('X2Y',  $0040, $0040, operand, true);
		decodeModifier('NX2Y', $0000, $0040, operand, false);
		decodeModifier('XT',   $0080, $0080, operand, true);
	end
	else
	begin
		write('LOADI');
		decodeModifier('SM1',  $0030, $0030, operand, true);
		decodeModifier('S0',   $0000, $0030, operand, false);
		decodeModifier('S1',   $0010, $0030, operand, true);
		decodeModifier('X2Y',  $0040, $0040, operand, true);
		decodeModifier('NX2Y', $0000, $0040, operand, false);
		decodeModifier('XT',   $0080, $0080, operand, true);
	end;
	operand := operand and 15;
	if operand > 0 then
		printSpacedOperand(operand);
end;

procedure printRegOperand(operand:integer);
begin
	case operand of
		0: write('FP');
		1: write('BP');
		2: write('RP');
		3: write('IV');
		4: write('IR');
		5: write('ESP');
		else write('<unknown:',operand,'>');
	end;
end;

procedure decodeReg(operand:integer);
begin
	if (operand and $0200) <> 0 then
		write('STOREREG ')
	else
		write('LOADREG ');

	operand := operand and 15;
	printRegOperand(operand);
end;

procedure decodeExt(operand:integer);
var extop:integer;
begin
	extop := (operand and $1C00) shr 10;

	if extop = 0 then
		decodeReg(operand)
	else
	if extop = 1 then
		decodeMem(operand)
	(*
	else
	if extop = 2 then
	begin
		{ unused }
	end *)
	else
	if extop = 3 then
	begin
		write('FPADJ ');
		printSignedOperand(operand and $03FF, $200);
	end
	else
	if extop = 5 then
		decodeLoadrel(operand and $03FF)
	else
		write('<EXT unknown:', extop,'>');
end;

procedure decodeInstruction(w:integer);
var baseIns:integer;
    baseOperand:integer;
begin
	baseIns := (w and $E000) shr 13;
	baseOperand := (w and $1FFF);

	(* writeln(baseIns, ' ', baseOperand); *)

	if baseIns = 0 then
		decodeBranch(baseOperand)
	else
	if baseIns = 1 then
		decodeAlu(baseOperand)
	else
	if baseIns = 2 then
		decodeLoadStore('STORE', baseOperand)
	else
	if baseIns = 3 then
		decodeXfer(baseOperand)
	else
	if baseIns = 4 then
		decodeLoadStore('LOAD', baseOperand)
	else
	if baseIns = 5 then
		decodeCbranch(baseOperand)
	else
	if baseIns = 6 then
		decodeLoadc(baseOperand)
	else
	if baseIns = 7 then
		decodeExt(baseOperand)
	else
		write('???');

	pc := pc + 2;

	(* write(' (', baseIns, ')');
	writeHex(w); *)

	printEol;
end;

function isConstantPool(sym:HashRef):boolean;
begin
	isConstantPool := false;

	if sym <> nil then
	begin
		if length(sym^.data) >= 4 then
			isConstantPool :=
				(sym^.data[1] = '_') and
				(sym^.data[2] = 'C') and
				(sym^.data[3] = 'P') and
				(sym^.data[4] = '_');
	end;
end;

function isStringConstant(sym:HashRef):boolean;
begin
	isStringConstant := false;

	if sym <> nil then
	begin
		if length(sym^.data) >= 5 then
			isStringConstant :=
				(sym^.data[1] = '_') and
				(sym^.data[2] = 'C') and
				(sym^.data[3] = '_') and
				(sym^.data[4] = 'S') and
				(sym^.data[5] = '_');
	end;
end;

procedure decodeIntConstant(upperHalf:integer);
var lowerHalf:integer;
    w:integer;
begin
{$R-}
	pc := pc + 2;
	(* need to increment pc in two steps
		because readBin uses the pc to detect line endings
		(which is probably a bad idea *)

	lowerHalf := readInstruction;
	w := (upperHalf shl 16) or lowerHalf;

	pc := pc + 2;

	write('.WORD ');
	printOperand(w);
	writeln;

{$R+}
end;

procedure printPaddedLabel(sym:HashRef); forward;
procedure printPc; forward;

procedure printLeadin;
begin
	printPc;
	printPaddedLabel(nil);
end;

procedure decodeString(upperHalf:integer);
var lowerHalf:integer;
    curLength,maxLength:integer;
    i,wordCount:integer;
    w:integer;
begin
	pc := pc + 2;
	lowerHalf := readInstruction;
	pc := pc + 2;
	curLength := (upperHalf shl 16) or lowerHalf;
	maxLength := readWord;

	write('.WORD ');
	printHex(curLength);
	writeln;

	printLeadin;
	write('.WORD ');
	printHex(maxLength);
	writeln;
	pc := pc + 4;

	wordCount := curLength;
	if maxLength > curLength then
		wordCount := maxLength;

	wordCount := (wordCount + 3) shr 2;

	for i := 1 to wordCount do
	begin
		w := readWord;
		printLeadin;
		write('.BYTE ');
		printAsciiWord(w);
		writeln;
		pc := pc + 4;
	end;
end;

procedure printPaddedLabel(sym:HashRef);
var pad:integer;
begin
	pad := 24;
	if sym <> nil then
	begin
		write(sym^.data);
		write(':');

		pad := pad - length(sym^.data) - 1;
	end;

	while pad > 0 do
	begin
		write(' ');
		pad := pad - 1;
	end;
	write('  ');
end;

procedure printPc;
var hexaddr:string[8];
begin
	hexstr(pc, hexaddr);
	write(hexaddr, ' ');
end;

procedure printLabels(adr:integer);
var bucket:HashBucket;
    current:HashRef;
    first:boolean;
begin
	(* there can be multiple labels
	   at an instruction address,
	   so go through all elements
	   in the corresponding hash bucket *)
	first := true;
	bucket := getHashBucket(symbolTable, adr);
	current := bucket;
	while current <> nil do
	begin
		if current^.key = adr then
		begin
			if not first then
			begin
				writeln;
				(* printPc; *)
				write('         ');
			end
			else
				first := false;

			printPaddedLabel(current);
		end;
		current := current^.next;
	end;

	if first then
		printPaddedLabel(nil);
end;

procedure decodeFile;
var w:integer;
    sym:HashRef;
begin
	while not eof(infile) do
	begin
		printPc;
		printLabels(pc);

		w := readInstruction;

		sym := getHashed(symbolTable, pc);
		if isConstantPool(sym) then
			decodeIntConstant(w)
		else
		if isStringConstant(sym) then
			decodeString(w)
		else
			decodeInstruction(w);
	end;
end;

procedure testHash;
var s:string;
    result:HashRef;
    i:integer;
begin
	s := 'einszweidrei';
	putHashed(symbolTable, 123, s);
	s := 'vierfuenf';
	putHashed(symbolTable, 45, s);
	s := 'null';
	putHashed(symbolTable, 0, s);
	s := '0x7FFF1234';
	putHashed(symbolTable, $7FFF1234, s);

	result := getHashed(symbolTable, 123);
	writeln('getHashed 123:', result^.data);

	result := getHashed(symbolTable, 45);
	writeln('getHashed 45:', result^.data);

	result := getHashed(symbolTable, 0);
	writeln('getHashed 0:', result^.data);

	result := getHashed(symbolTable, $7FFF1234);
	writeln('getHashed $7FFF1234:', result^.data);

	for i := 1 to 5000 do
	begin
		str(i,s);
		putHashed(symbolTable,i,s);
	end;
end;

procedure readKeyword(var fil:InputFileType; var wordBuf:string);
var c:char;
    skipWhite:boolean;
    done:boolean;
begin
	wordBuf := '';
	done := false;
	skipWhite := true;

	repeat
		read(fil,c);
		if c in [ ' ', #9, #13, #10, #0 ] then
		begin
			if not skipWhite then
				done := true;
		end
		else
		begin
			wordBuf := wordBuf + c;
			skipWhite := false;
		end;
	until done or eof(fil);
	if c = #13 then (* skip over CR/LF *)
		read(fil,c);
end;

procedure readSymbolTable(var filename:string);
var buf:string;
    fil:InputFileType;
    symStr:string;
    num:integer;
begin
	openFile(fil, filename);
	while not eof(fil) do
	begin
		readKeyword(fil,buf);
		readKeyword(fil,symStr);
		num := convertHex(buf);
		putHashed(symbolTable, num, symStr);
	end;

	close(fil);
end;

function parseOrigin(s:string):integer;
var i,c:integer;
begin
	val(s,i,c);
	if c > 0 then
		errorExit2('invalid number',s);
	parseOrigin := i;
end;

begin
	if paramCount < 1 then halt;

	if paramCount >= 2 then
	begin
		filename := paramStr(2);
		readSymbolTable(filename);
	end;

	if paramCount >= 3 then
		pc := parseOrigin(paramStr(3));

	filename := paramStr(1);
	openFile(infile, filename);
	decodeFile;
	close(infile);

	(* dumpHash(symbolTable); *)
end.
