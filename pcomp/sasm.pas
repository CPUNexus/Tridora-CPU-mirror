(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
{$MODE objfpc}
{$H600}
{$S4}
program sasm;
{$!}{$ifdef FPC}uses math,crt;{$endif}
{$R+}
type 	TokenType = (
		PlusToken, MinusToken, AsteriskToken, SlashToken,
		SemicolonToken, EOFToken, EOLToken,
		NumberToken, KeywordToken, LabelToken, DirectiveToken,
		StringLitToken, CharLitToken, MetaKeywordToken,
		CommaToken, DotToken, ColonToken, PercentToken, TildeToken,
		AndToken, OrToken, XorToken,
		AtToken, UnknownToken
	);

	IdentString = string[120];
	KeywordString = string[255];
	InsString = string[24];
	AddrString = string[8];

	Token = record
		tokenText: string[255];
		tokenKind: TokenType;
	end;

	OperandType = (
		NoOprnd,
		U13WOprnd, (* unsigned 13 bit word-aligned operand e.g. LOAD and STORE *)
		S13Oprnd,  (* signed 13 bit operand e.g. LOADC *)
		RelWOprnd, (* signed 13 bit word-aligned operand, PC-relative, e.g. BRANCH/CBRANCH *)
		S10Oprnd,  (* signed 10 bit operand, e.g. FPADJ *)
		U4Oprnd,   (* unsigned 4 bit operand, would used for modifiers but is actually unused *)
		CmpOprnd,  (* comparison operand, unsigned 4 bit, actually unused *)
		RegOprnd,  (* register id operand for LOADREG, 4 bit unsigned *)
		RelU10Oprnd, (* PC-relative 10 bit unsigned, for LOADREL *)
		OptOprnd   (* optional 4 bit operand for ALU *)
	);

	EncodingEntry = record
		mask: integer;
		value: integer;
	end;

	InstructionWord = integer;
	MachineWord = integer;
	OutputWord = record
		intvalue:integer;
	end;

	ModifierEntry = record
		keyword: string[24];
		encoding: EncodingEntry;
		next: ^ModifierEntry;
	end;

	OpcodeData = record	(* the key is stored in the tree as metadata *)
		encoding: EncodingEntry;
		modifiers: ^ModifierEntry;
		operand: OperandType;
		id: integer; (* unique id for this opcode or alias *)
	end;

	SymbolType = (ConstSymbol, LabelSymbol, SpecialSymbol, SkippedSymbol);

	Symbol = record
		value:integer;
		typ:SymbolType;
		aligned:boolean;
		padded:boolean;
		exported:boolean;
	end;

	SymbolRef = ^Symbol;

	CPoolEntry = record
		labelname: string[40];
		value: KeywordString;
		offset: integer;
		symbolic: boolean;
		sym: SymbolRef;
		next: ^CPoolEntry;
		prev: ^CPoolEntry;
	end;

	CPoolRef = ^CPoolEntry;

	LabelList = record
		name: IdentString;
		next: ^LabelList;
		prev: ^LabelList;
	end;

	LabelListRef = ^LabelList;

	UnresolvedBranch = record
		target:IdentString;
		origin:integer;
		maxDistance:integer;
		shrinkage:integer;
		labels:LabelListRef;
		next:^UnresolvedBranch;
	end;

	UnresBranchRef = ^UnresolvedBranch;

{$I 'platform-types+.pas'} (* defines OutputFileType, InputFileType, SymFileType *)

	InputFileState = record
		name: string;
		filevar: InputFileType;
		line: integer;
	end;

	TreeDataType = (TDString, TDInteger, TDSymbol, TDOpcode);
	Treedata = record
		case typ:Treedatatype of
		TDString:(stringdata:string);
		TDInteger:(intdata:integer);
		TDSymbol:(symboldata:Symbol);
		TDOpcode:(opcodedata:OpcodeData);
	end;

{$I 'treetypes.pas'}

const	insSize = 2;
	wordSize = 4;
	wordSizeMask = 3;
	MaxUShortOffset = 8191;
	MaxShortOffset = 4095;
	MaxShorterOffset = 511;
	MaxTinyOffset  = 15;
	Unresolved = 2147483647; (* max integer - 1 *)
        MaxIntegerDigits = 24;
	wordBits = 32;
        MaxIncludes = 4;
        FilenameSuffix = '.s';
        OutfileSuffix = '.prog';
	SymFileSuffix = '.sym';
	AsciifileSuffix = '.mem';

	progressSteps = 511;

	shortcutChar = '`';
	firstShCChar = 'A';
	lastShCChar = 'i';

var
	curToken, nextToken, lastToken: Token;

	infileOpened: boolean;
	outputEnabled: boolean;
	asciiOutput: boolean;
	lastOpcode: TreeDataRef;

	bufferedChar: char;
	buffered: boolean;
	infile: InputFileType;
	outfile: OutputFileType;
	filename: string;
	outfilename: string;
	editOnError, runOnSuccess: boolean;
	lineno: integer;
	prevFiles: array[1..MaxIncludes] of InputFileState;
	includeLevel: integer;
	paramPos: integer;

	pc: integer;
	pass:integer;

	bytesCount:integer;

	symbolTable: TreeRef;
	opcodeTable: TreeRef;
	nextOpcodeId: integer;
	constantPool: CPoolRef;
	cPoolCount: integer;
	nextConstId: integer;

	firstUnresBranch: ^UnresolvedBranch;

	LOADCPId: integer;

	outputPrefix: string;
	includePrefix: string;

	shortcuts: array[firstShCChar..lastShCChar] of OpcodeData;

procedure errorExit2(message1, message2: string); forward;

{$I 'platform+.pas'}
{$I 'treeimpl.pas'}


procedure verifyTree(node:TreeRef); forward;

procedure dumpSymbolTable; forward;


procedure cleanup;
begin
	if infileOpened then
		close(infile);
	if outputEnabled then
		close(outfile);
end;


procedure errorExit;
begin
	cleanup;
	(* dumpSymbolTable; *)
	halt;
end;

procedure errorLine(line:integer);
begin
	if curToken.tokenKind = EOLToken then
		lineno := lineno - 1;
	writeln('at line ',lineno, ' in ', filename);
end;

procedure errorExit2(message1, message2: string);
var errormsg:string[128];
begin
	errormsg := message1 + ' ' + message2;
	writeln;
	writeln('Error: ', errormsg);
	errorLine(lineno);
	cleanup;
	if editOnError then
		ExecEditor(filename, lineno, errormsg)
	else
		halt;
end;

function descToken(kind:tokenType):string;
begin
	case kind of
	PlusToken, MinusToken, AsteriskToken, SlashToken, AndToken, OrToken, XorToken:
		descToken := 'one of + - * / & | ^';
	SemicolonToken:
		descToken := '";"';
	EOFToken:
		descToken := 'end-of-file';
	EOLToken:
		descToken := 'end-of-line';
	NumberToken:
		descToken := 'number';
	KeywordToken:
		descToken := 'keyword';
	LabelToken:
		descToken := 'label';
	DirectiveToken:
		descToken := 'directive';
	StringLitToken:
		descToken := 'string literal';
	CharLitToken:
		descToken := 'char literal';
	MetaKeywordToken:
		descToken := 'meta directive';
	CommaToken:
		descToken := '","';
	DotToken:
		descToken := '"."';
	ColonToken:
		descToken := '":"';
	PercentToken:
		descToken := '"."';
	AtToken:
		descToken := '"@"'
	else
		descToken := '<unknown>';
	end;
end;

procedure makeCPoolLabel(var labelname:IdentString);
var digits:string[16];
begin
	str(nextConstId, digits);
	labelname := '_CP_' + digits;
end;

procedure hexstr(value:integer;var output:string); forward;

procedure putCPoolEntry(var constant:KeywordString; offset:integer; symbolic:boolean;var labelname:IdentString);
var newEntry:CPoolRef;
begin
	makeCPoolLabel(labelname);

	new(newEntry);

	newEntry^.labelname := labelname;
	newEntry^.value := constant;
	newEntry^.offset := offset;
	newEntry^.symbolic := symbolic;
	newEntry^.next := constantPool;
	newEntry^.prev := nil;

	if constantPool <> nil then
		constantPool^.prev := newEntry;
	constantPool := newEntry;

	nextConstId := nextConstId + 1;
end;

function isNumber(var s:string):boolean;
begin
	if isdigit(s[1]) then
		isNumber := true
	else
		isNumber := s[1] in [ '-', '$', '%' ];
end;

function convertNumber(digits:KeywordString):integer; forward;
function findSymbol(var keyword:KeywordString):TreeDataRef; forward;

procedure addCPoolEntry(var constant:KeywordString; offset:integer; var labelname:IdentString);
begin
	putCPoolEntry(constant, offset, not isNumber(constant), labelname);
	cPoolCount := cPoolCount + 1;
end;

function getSymbolValue(var keyword:KeywordString):integer; forward;

(*
   Get the address of a cpool entry for a constant value.
   If the pool does not contain the constant, create
   a new entry.
   The constant needs to be a string because it might
   be a symbol. In pass 1, the value of the symbol is not
   known, so to reuse a value we need to use the symbolic name.

   The offset is an optional numerical value or symbol that gets
   added to the constant before it is put into the pool.
   This is used by the compiler for global variables
   (arrays, record fields). Can be zero.
 *)

function getCPoolAddr(var constant:KeywordString;offset:integer):integer;
var labelname:IdentString;
    current:CPoolRef;
    found:CPoolRef;
begin
	getCPoolAddr := pc;

	found := nil;
	current := constantPool;
	while (current <> nil) and (found = nil) do
	begin
		if (constant = current^.value) and (offset = current^.offset) then
			found := current;
		current := current^.next;
	end;

	if found <> nil then
	begin
		(* value already exists in pool *)
		labelname := found^.labelname;
	end
	else
		(* value not found, add it to the pool and
		   set the label name for the new entry *)
		addCPoolEntry(constant,offset,labelname);

	(* writeln(' [P', pass, ' cpool ', constant, ' -> ', labelname, '] '); *)
	getCPoolAddr := getSymbolValue(labelname);
end;

procedure printPassNo;
begin
	write('P', pass, ' ');
end;

procedure printCurrentLineno;
begin
	write(#13);
	printPassNo;
	write(filename, '    ', lineno);
	ClrEol;
end;

procedure printLastLineno;
begin
	printCurrentLineno;
end;

procedure beginInclude(var newname: string);
var newfile: InputFileType;
begin
	if includeLevel = MaxIncludes then
		errorExit2('Too many nested includes', '');

	includeLevel := includeLevel + 1;

	prevFiles[includeLevel].filevar := infile;
	prevFiles[includeLevel].name := filename;
	prevFiles[includeLevel].line := lineno;

	openFileWithDefault(newfile, newname);

	infile := newfile;
	filename := newname;
	lineno := 1;
	buffered := false;
end;

procedure endInclude;
begin
	if includeLevel = 0 then
		errorExit2('Internal error in', 'endInclude');

	close(infile);

	infile := prevFiles[includeLevel].filevar;
	filename := prevFiles[includeLevel].name;
	lineno := prevFiles[includeLevel].line;

	buffered := false;

	includeLevel := includeLevel - 1;
end;

function includeIsActive:boolean;
begin
	includeIsActive := includeLevel > 0;
end;

function nextChar: char;
var ch: char;
begin
	if buffered then
	begin
		ch := bufferedChar;
		buffered := false;
	end
	else
	begin
		if not eof(infile) then
		begin
			read(infile, ch);
		end
		else
		begin
			(* we reached end-of-file, was this
				the end of an include file? *)
			if includeIsActive then
			begin
				 (* if yes, switch back to previous file *)
				endInclude;
				ch := ' '; (* return a space which will get skipped *)
			end
			else
				(* no, return null character which becomes an EOFToken *)
				ch := #0;
		end
	end;
	if ch = #10 then lineno := lineno + 1;
	nextChar := ch;
end;

function peekChar: char;
var tmpChar: char;
begin
	if buffered then
	begin
		peekChar := bufferedChar;
	end
	else
	begin
		if not eof(infile) then
		begin
			read(infile, tmpChar);
			peekChar := tmpChar;
			bufferedChar := tmpChar;
			buffered := true;
		end
		else
		begin
			(* at the eof of an include,
				just return an extra space and let nextChar
				do the work *)
			if includeIsActive then
			begin
				peekChar := ' ';
				buffered := false; (* force nextChar to do real I/O *)
			end
			else
				peekChar := #0;
		end
	end
end;

procedure skipWhitespace;
var c:char;
begin
	while peekChar() in [ #13, #32, #9 ] do
		c := nextChar;
end;

function integerFromString(digits:KeywordString):integer;
var value,error:integer;
begin
	val(digits, value, error);
	if error <> 0 then
		errorExit2('Invalid integer value', digits);
	integerFromString := value;
end;

function convertHex(var digits:KeywordString):integer;
var i,v,len:integer;
    c:char;
begin
	len := length(digits);

	i := 2;
	convertHex := 0;

	while i <= len do
	begin
		convertHex := convertHex shl 4;
		c := digits[i];
		if (c >= 'A') and (c <= 'F') then
			v := ord(c) - ord('A') + 10
		else
		if (c >= '0') and (c <= '9') then
			v := ord(c) - ord('0')
		else
			errorExit2('Invalid number',digits);
		convertHex := convertHex + v;
		i := i + 1;
	end;
end;

function convertBin(var digits:KeywordString):integer;
var i,v,len:integer;
    c:char;
begin
	len := length(digits);

	i := 2;
	convertBin := 0;

	while i <= len do
	begin
		c := digits[i];
		if c <> '_' then  (* ignore '_' for a syntax like 0000_0001 *)
		begin
			convertBin := convertBin shl 1;

			if (c >= '0') and (c <= '1') then
				v := ord(c) - ord('0')
			else
				errorExit2('Invalid number',digits);

			convertBin := convertBin + v;
		end;
		i := i + 1;
	end;
end;

function convertChar(digits:KeywordString):integer;
begin
	convertChar := ord(digits[1]);
end;

function convertNumber(digits:KeywordString):integer;
var negate:boolean;
begin
	negate := digits[1] = '-'; 
	(* we need to keep the sign for decimals
		because we cannot represent abs(-maxint)
		as a signed 32-bit integer and integerFromString
		uses val() *)
	if negate then
		if (digits[2] in [ '$', '%' ]) then
			delete(digits,1,1);

	if digits[1] = '$' then
		convertNumber := convertHex(digits)
	else
	if digits[1] = '%' then
		convertNumber := convertBin(digits)
	else
	begin
		negate := false;
		convertNumber := integerFromString(digits);
	end;
	if negate then
		convertNumber := -convertNumber;
end;

function getCharLitValue(tokenText:string):integer;
begin
	(* is is a one-character-string-literal like 'A' ? *)
	if length(tokenText) = 1 then
		getCharLitValue := ord(tokenText[1])
	else
		errorExit2('Cannot use string as char here', tokenText);
end;

(* scan for an integer number. the first digit is already in curChar.
   digits are written to keyword. *)
procedure getDigits(curChar: char; var keyword: KeywordString);
begin
	keyword := keyword + curChar;
	while peekChar in [ '0'..'9' ] do
	begin
		keyword := keyword + nextChar;
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

(* Scan for an integer number in hexadecimal format.
   The hex marker '$' is already in curChar.
   Digits are written to keyword. *)
procedure getHexDigits(curChar: char; var keyword: KeywordString);
begin
	keyword := keyword + curChar;
	while peekChar in [ '0'..'9', 'A'..'F' ] do
	begin
		keyword := keyword + nextChar;
	end;
end;

procedure getToken(var tokenReturn:Token;stringTokens:boolean);
var curChar,pkChar: char;
    keyword: KeywordString;
	startLine: string[12];

function isKeywordChar(ch:char):boolean;
begin
	isKeywordChar := (ch >= 'A') and (ch <= 'Z') or
			(ch >= 'a') and (ch <= 'z') or
			(ch >= '0') and (ch <= '9') or
			(ch = '_') or (ch = '.');
end;

function isAlpha(ch:char):boolean;
begin
	isAlpha := ((ch >= 'A') and (ch <= 'Z')) or
		((ch >= 'a') and (ch <= 'z'));
end;

function isKeywordStart(ch:char):boolean;
begin
	isKeywordStart := ((ch >= 'A') and (ch <= 'Z')) or
		((ch >= 'a') and (ch <= 'z')) or
		(ch = '_');
end;

begin
	curChar := nextChar;

	tokenReturn.tokenText := curChar;

	if curChar = shortcutChar then (* two character instruction shortcut *)
	begin
		keyword := curChar + nextChar;

		(* shortcuts can have modifiers *)
		while isKeywordChar(peekChar) do
		begin
			curChar := Upcase(nextChar);
			if (length(keyword) < 80) and (curChar <> #0) then keyword := keyword + curChar;
		end;

		tokenReturn.tokenKind := KeywordToken;
		tokenReturn.tokenText := keyword;
	end
	else
	if curChar = #0 then
		tokenReturn.tokenKind := EOFToken
	else
	if curChar = #10 then
		tokenReturn.tokenKind := EOLToken
	else
	if curChar = '+' then
		tokenReturn.tokenKind := PlusToken
	else
	if curChar = '-' then
		tokenReturn.tokenKind := MinusToken
	else
	if curChar = '*' then
			tokenReturn.tokenKind := AsteriskToken
	else
	if curChar = '/' then
		tokenReturn.tokenKind := SlashToken
	else
	if curChar = '~' then
		tokenReturn.tokenKind := TildeToken
	else
	if curChar = '@' then
		tokenReturn.tokenKind := AtToken
	else
	if curChar = ',' then
		tokenReturn.tokenKind := CommaToken
	else
	if curChar = '&' then
		tokenReturn.tokenKind := AndToken
	else
	if curChar = '|' then
		tokenReturn.tokenKind := OrToken
	else
	if curChar = '^' then
		tokenReturn.tokenKind := XorToken
	else
	if curChar = '.' then
	begin
		pkChar := peekChar;
		if isAlpha(pkChar) then
		begin
			keyword := Upcase(curChar);
			while isKeywordChar(peekChar) do
			begin
				curChar := Upcase(nextChar);
				if (length(keyword) < 80) and (curChar <> #0) then keyword := keyword + curChar;
			end;
			tokenReturn.tokenText := keyword;
			tokenReturn.tokenKind := DirectiveToken;
		end
		else
			tokenReturn.tokenKind := DotToken;
	end
	else
	if curChar = '%' then
		(* percent sign can be the start of a binary number or
			an include directive *)
	begin
		pkChar := peekChar;
		if pkChar in ['A'..'Z', 'a'..'z' ] then (* is it a meta directive? *)
		begin
			keyword := Upcase(curChar);
			while peekChar in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do
			begin
				curChar := Upcase(nextChar);
				if (length(keyword) < 80) and (curChar <> #0) then keyword := keyword + curChar;
			end;
			tokenReturn.tokenText := keyword;
			tokenReturn.tokenKind := MetaKeywordToken;
		end
		else
		if pkChar in ['0'..'1' ] then	(* is it a binary number? *)
		begin
			keyword := curChar;
			while peekChar in ['0','1','_' ] do
			begin
				curChar := nextChar;
				if (length(keyword) < 80) and (curChar <> #0) then keyword := keyword + curChar;
			end;
			tokenReturn.tokenText := keyword;
			tokenReturn.tokenKind := NumberToken;
		end
		else
			tokenReturn.tokenKind := PercentToken;  (* this is most likely unusable *)
	end
	else
	if curChar = ';' then
		tokenReturn.tokenKind := SemicolonToken
	else
	if isKeywordStart(curChar) then
	begin
		keyword := Upcase(curChar);
		while isKeywordChar(peekChar) do
		begin
			curChar := Upcase(nextChar);
			if (length(keyword) < 80) and (curChar <> #0) then keyword := keyword + curChar;
		end;

		tokenReturn.tokenText := keyword;
		if peekChar = ':' then
		begin
			tokenReturn.tokenKind := LabelToken;
			curChar := nextChar;
		end
		else
			tokenReturn.tokenKind := KeywordToken;
	end
	else
	if isdigit(curChar) then
	begin
		keyword := '';
		getDigits(curChar, keyword);
		tokenReturn.tokenText := keyword;
		tokenReturn.tokenKind := NumberToken;
	end
	else
	if curChar = '$' then
	begin
		keyword := '';
		getHexDigits(curChar, keyword);
		tokenReturn.tokenText := keyword;
		tokenReturn.tokenKind := NumberToken;
	end
	else
	if (curChar = '''') and stringTokens then
	begin
		keyword := nextChar;
		curChar := nextChar;
		tokenReturn.tokenKind := CharLitToken;
		tokenReturn.tokenText := keyword;

		if curChar <> '''' then
			errorExit2('Invalid character literal, missing ''', keyword);
	end
	else
	if (curChar = '"') and stringTokens then
	begin
		str(lineno, startLine);
		keyword := '';
		curChar := nextChar;
		(* add characters as long as the current char is not '
		  (or if it is a double ') and not EOF *)
		while (not ((curChar = '"') and (peekChar <> '"'))) and (curChar <> #0 ) do
		begin
			if (curChar = '"') and (peekChar = '"') then
			begin
				keyword := keyword + curChar;
				curChar := nextChar;
			end
			else
				keyword := keyword + curChar;
			curChar := nextChar;
		end;
		if curChar = #0 then
			errorExit2('Unterminated string constant starting at line', startLine);
		tokenReturn.tokenText := keyword;
		(* string literals with a length of 1 are char literals
			which may be converted into string constants later  *)
		if length(keyword) = 1 then
			tokenReturn.tokenKind := CharLitToken
		else
			tokenReturn.tokenKind := StringLitToken;
	end
	else
		tokenReturn.tokenKind := UnknownToken;
end;

(* check for (and do not consume) a specific token, returns true on match *)
function checkToken(kind: TokenType): boolean;
begin
	checkToken := curToken.tokenKind = kind;
end;

(*  move to next token without any processing.
	sets curToken global variable. *)
procedure skipToNextToken;
begin
	getToken(nextToken, true);
	curToken := nextToken;
end;

(* read the next token into the global variable curToken.
   skips whitespace and comments.
*)
procedure readNextToken;
var c:char;
begin
	skipWhitespace;

	lastToken := curToken;
	getToken(nextToken, true);

	curToken := nextToken;

	if curToken.tokenKind = SemicolonToken then
	begin
		repeat
			c := nextChar;
		until c = #10;
		curToken.tokenKind := EOLToken;
	end;
end;

(* match (and consume) a token or exit with error *)
procedure matchToken(kind: TokenType);
begin
	if curToken.tokenKind <> kind then
		errorExit2('Expected ' + descToken(kind) + ', found', curToken.tokenText);
	readNextToken;
end;

(* match (and consume) a token, returning true, or if no match, do not
	consume token and return false *)
function matchTokenOrNot(wantedToken: TokenType): boolean;
begin
	if checkToken(wantedToken) then
	begin
		matchTokenOrNot := true;
		readNextToken;
	end
	else
		matchTokenOrNot := false;
end;

procedure emitInstructionWord(value:InstructionWord); forward;
procedure fixupLabel(oldPc, newPc:integer); forward;

procedure alignOutput(amount:integer);
var mask,o:integer;
    oldPc:integer;
begin
	oldPc := pc;
	mask := amount - 1;

	o := pc and mask;
	if o = 2 then
	begin
		emitInstructionWord(0);
		(* if there was a label, we need to fix the address *)
		fixupLabel(oldPc, pc);
	end
	else
	if o = 0 then
	begin (* do nothing *) end
	else
		errorExit2('internal error: bad alignment', '');
end;

procedure emitBinByte(value:integer);
var c:char;
begin
	c := chr(value and $FF);
	write(outfile,c);
end;

procedure emitBin16(value:InstructionWord);
var hi,lo:integer;
begin
	hi := value and $FF00 shr 8;
	lo := value and $00FF;
	emitBinByte(hi);
	emitBinByte(lo);
end;

procedure emitBin32(value:MachineWord);
var b3,b2,b1,b0:integer;
begin
	b0 := value and $FF;
	value := value shr 8;
	b1 := value and $FF;
	value := value shr 8;
	b2 := value and $FF;
	value := value shr 8;
	b3 := value and $FF;
	emitBinByte(b3);
	emitBinByte(b2);
	emitBinByte(b1);
	emitBinByte(b0);
end;

procedure emitAsciiBin(encoded:InstructionWord);
var i:integer;
   digit:char;
begin
	for i := 1 to 16 do
	begin
		if (encoded and $8000) <> 0 then
			digit := '1'
		else
			digit := '0';

		write(outfile, digit);
		encoded := encoded shl 1;
	end;
	if((pc and 3) = 2) then
		writeln(outfile);
end;

(* assumes 32-bit alignment *)
procedure emitAsciiBin32(value:MachineWord);
var i:integer;
   digit:char;
begin
	for i := 1 to 32 do
	begin
		if (value and $80000000) <> 0 then
			digit := '1'
		else
			digit := '0';

		write(outfile, digit);
		value := value shl 1;
	end;
	writeln(outfile);
end;

procedure emitAsciiByte(value:integer);
var i:integer;
   digit:char;
begin
	for i := 1 to 8 do
	begin
		if (value and $80) <> 0 then
			digit := '1'
		else
			digit := '0';

		write(outfile, digit);
		value := value shl 1;
	end;

	if (pc and wordSizeMask) = 3 then
		writeln(outfile);
end;

(*
  Emit a single byte.
  Make sure to do proper alignment afterwards.
  Mainly used for .BYTE directive.
*)
procedure emitByte(value:integer);
begin
	if outputEnabled then
	begin
		if asciiOutput then
			emitAsciiByte(value)
		else
			emitBinByte(value);
	end;
	bytesCount := bytesCount + 1;
	pc := pc + 1;
end;

(* assumes aligned output *)
procedure emitWord(value:MachineWord);
begin
	if outputEnabled then
	begin
		if asciiOutput then
			emitAsciiBin32(value)
		else
			emitBin32(value);
	end;
	bytesCount := bytesCount + 4;
	pc := pc + 4;
end;

procedure emitInstructionWord(value:InstructionWord);
begin
	if outputEnabled then
	begin
		if asciiOutput then
			emitAsciiBin(value)
		else
			emitBin16(value);
	end;
	bytesCount := bytesCount + 2;
	pc := pc + 2;
end;

procedure emitInstruction(var opcode:OpcodeData; encoded:InstructionWord);
begin
	emitInstructionWord(encoded);
end;

procedure emitBlock(count:integer; value:integer);
var i:integer;
begin
	alignOutput(wordSize);
	for i := 1 to count do
		emitWord(value);
end;

procedure encodeOperand(value:integer; var op:OpcodeData; var encoded:InstructionWord); forward;
procedure getBaseAndModifiers(var ins:KeywordString; var opcode:OpcodeData; var encoded:InstructionWord); forward;

procedure encodeInstruction(ins:string; operand:integer; var encoded:InstructionWord);
var opcode:OpcodeData;
begin
	getBaseAndModifiers(ins, opcode, encoded);
	if opcode.id = LOADCPId then
		errorExit2('internal error in encodeInstruction', curToken.tokenText);
	encodeOperand(operand, opcode, encoded);
end;

procedure ClearTree(var root:TreeRef);
begin
	while root <> nil do
	begin
		(* delete subtrees first to reduce reshuffling of nodes *)
		if root^.left <> nil then TreeDelete(root^.left, root^.left^.key^);
		if root^.right <> nil then TreeDelete(root^.right, root^.right^.key^);
		TreeDelete(root, root^.key^);
	end;
	verifyTree(root);
end;

procedure ClearCPool;
var current:CPoolRef;
    next:CPoolRef;
begin
	current := constantPool;

	while current <> nil do
	begin
		next := current^.next;
		dispose(current);
		current := next;
	end;
	constantPool := nil;
end;

procedure createSymbol(var keyword:IdentString; typ:SymbolType; value:integer; aligned,padded:boolean);
	forward;
procedure dumpCPool; forward;

procedure emitConstantPool(branch:boolean);
var current:CPoolRef;
    labelname:IdentString;
    value:KeywordString;
    intValue:integer;
    size:integer;
    encoded:InstructionWord;
    padded:boolean;
begin
	(* writeln('*** emitConstantPool at ', pc, ' count ', cPoolCount); *)
	if branch then
	begin
		(* calculate size of all constants in bytes *)
		size := cPoolCount * wordSize;

		(*
			Add padding if alignment is needed.
			This happens when the pc is at a word boundary (pc and wordSize = 0).
			Then after the branch instruction, the pc is at a half-word boundary
			and we need a padding half-word to get the correct alignment for the
			word-sized constants.
		*)
		if (pc and wordSizeMask) = 0 then size := size + insSize;

		(* encode the instruction, adjust operand for the size of the branch instruction*)
		encodeInstruction('BRANCH', pc + size + insSize, encoded);
		emitInstructionWord(encoded);
	end;


	current := constantPool;
	padded := false;

	(* if the constant pool is empty, do no alignment *)
	if current <> nil then
	begin
		(* remember if we needed alignment padding *)
		padded := (pc and wordSizeMask) = insSize;
		alignOutput(wordSize);
	end;

	(* the cpool list has the latest entries at the front,
		so go to the tail first and then go backward *)
	if current <> nil then
		while current^.next <> nil do
			current := current^.next;

	while current <> nil do
	begin
		labelname := current^.labelname;
		value := current^.value;

		createSymbol(labelname, LabelSymbol, pc, true, padded);
		(* only the first entry is marked as padded *)
		if padded then padded := false;
		if current^.symbolic then
			intValue := getSymbolValue(value)
		else
			intValue := convertNumber(value);

		if intValue <> Unresolved then
			intValue := intValue + current^.offset;
		emitWord(intValue);

		current := current^.prev;
	end;

	(* writeln('*** emitConstantPool new pc ', pc); *)
	(* dumpCPool; *)
	ClearCPool;
	cPoolCount := 0;
end;

procedure encodeOperand(value:integer; var op:OpcodeData; var encoded:InstructionWord);
var mask, negativeMask:integer;
    valueStr:string;
    isSigned:boolean;
    min, max: integer;
begin
	case op.operand of
	NoOprnd:     begin mask := 0;     negativeMask := 0;     end;
	U13WOprnd:   begin mask := $1FFE; negativeMask := $0000; end;
	S13Oprnd:    begin mask := $1FFF; negativeMask := $1000; end;
	RelWOprnd:   begin mask := $1FFE; negativeMask := $1000; value := value - pc; end;
	S10Oprnd:    begin mask := $03FF; negativeMask := $0200; end;
	U4Oprnd:     begin mask := $000F; negativeMask := $0000; end;
	CmpOprnd:    begin mask := $000F; negativeMask := $0000; end;
	RegOprnd:    begin mask := $000F; negativeMask := $0000; end;
	RelU10Oprnd: begin mask := $03FF; negativeMask := $0000; value := value - pc; end;
	OptOprnd:    begin mask := $000F; negativeMask := $0000; end;
	end;

	isSigned := negativeMask <> 0;

	if isSigned then
	begin
		min := -negativeMask;
		max := -min + 1;
	end
	else
	begin
		min := 0;
		max := mask;
	end;

	if (value < min) or (value > max) then
	begin
		(* if not on the last pass (generating output),
			we ignore values which are out of range,
			because they might change after the 1st pass *)

		if not outputEnabled then
			value := 0
		else
		begin
			if op.operand = RelU10Oprnd then
			begin
				writeln;
				writeln('bad RelU10Oprnd:', value, ' pc:', pc, ' pass:', pass);
				dumpCPool;
			end;
			str(value, valueStr);
			errorExit2('Invalid operand value', valueStr);
		end;
	end;

	encoded := (encoded and not mask) or (value and mask);
end;

function getSymbol(var keyword:KeywordString):TreeDataRef;
begin
	getSymbol := TreeSearch(symbolTable, keyword);
	if (getSymbol = nil) and (pass > 1) then
			errorExit2('Undeclared symbol', keyword);
end;

function getLabelAddr(var keyword:KeywordString):integer;
var sym:TreeDataRef;
begin
	sym := getSymbol(curToken.tokenText);

	if sym = nil then
		getLabelAddr := Unresolved
	else
		getLabelAddr := sym^.symboldata.value;
end;

function findSymbol(var keyword:KeywordString):TreeDataRef;
begin
	findSymbol := TreeSearch(symbolTable, keyword);
end;

function getSymbolValue(var keyword:KeywordString):integer;
var data:TreeDataRef;
begin
	data := TreeSearch(symbolTable, keyword);

	if (data = nil) then
	begin
		if pass = 1 then
			(* in pass 1, we do not care about undefined symbols *)
			getSymbolValue := Unresolved
		else
			errorExit2('Undeclared symbol', keyword);
	end
	else
		getSymbolValue := data^.symboldata.value;
end;

procedure addUncertainLabel(var name:IdentString);
var current:UnresBranchRef;
    newLListEntry:LabelListRef;
begin
	current := firstUnresBranch;

	while (current <> nil) do
	begin
		(* writeln('**addUncertainLabel ',name, '  for unresBranch ', current^.target); *)
		(* put new label list entry at head of the list *)
		new(newLListEntry);
		newLListEntry^.next := current^.labels;
		newLListEntry^.prev := nil;
		newLListEntry^.name := name;
		if current^.labels <> nil then
			current^.labels^.prev := newLListEntry;
		current^.labels := newLListEntry;

		current := current^.next;
	end;
end;

procedure addUnresBranch(var name:IdentString; origin:integer; maxDistance,shrink:integer);
var newUnresBranch:^UnresolvedBranch;
begin
	new(newUnresBranch);
	newUnresBranch^.target := name;
	newUnresBranch^.origin := origin;
	newUnresBranch^.maxDistance := maxDistance;
	newUnresBranch^.shrinkage := shrink;
	newUnresBranch^.labels := nil;
	newUnresBranch^.next := firstUnresBranch;
	firstUnresBranch := newUnresBranch;

	(* writeln('** addUnresBranch ', name, ' at ', origin); *)
end;

(* Delete unresolved branch entry with target *name*.
   Will delete multiple occurrences of the same name. *)
procedure deleteUnresBranch(var name:IdentString);
var last,current,temp:^UnresolvedBranch;
begin
	current := firstUnresBranch;
	last := nil;

	while (current <> nil) do
	begin
		if current^.target = name then
		begin
			if last = nil then
				firstUnresBranch := current^.next
			else
				last^.next := current^.next;
			temp := current^.next;
			dispose(current);
			current := temp;
		end
		else
		begin
			last := current;
			current := current^.next;
		end;
	end;
end;

procedure addUnresBranch2(var name:IdentString; o:integer; max,shrink:integer);
var newUnresBranch:^UnresolvedBranch;
begin
	new(newUnresBranch);
	with newUnresBranch^ do
	begin
		target := name;
		origin := o;
		maxDistance := max;
		shrinkage := shrink;
		labels := nil;
		next := firstUnresBranch;
	end;
	firstUnresBranch := newUnresBranch;
end;

(* Check if a label declaration resolves an unresolved branch
   (.LBRANCH/.LCBRANCH). If it does, apply the code size correction
   to all labels we encountered since the branch. *)
procedure checkUnresBranches(var name:IdentString);
var r:UnresBranchRef;
    current,last,next:LabelListRef;
    sym:TreeDataRef;
    distance:integer;
    shrink:boolean;
    adjustment:integer;
begin
	r := firstUnresBranch;

	while r <> nil do
	begin
		if r^.target = name then
		begin
			distance := pc - r^.origin;
			shrink := distance <= r^.maxDistance;
{			writeln('** checkUnresBranches found ', name, ' at ', pc, ' distance ', pc - r^.origin, ' line ', lineno);
			writeln('   ', r^.origin, ' ', r^.maxDistance, ' ', r^.shrinkage); }
			current := r^.labels;
			last := nil;

			if shrink then
				pc := pc - r^.shrinkage;
			(* writeln('    short:', shrink); *)

			while current <> nil do
			begin
				(* go through all labels we encountered since the LBRANCH/LCBRANCH,
					process and dispose of the list entries *)

				(* writeln('    ', current^.name); *)

				if shrink then
				begin
					sym := findSymbol(current^.name);
					with sym^.symboldata do
					begin
						(* writeln('        ', value, ' -', r^.shrinkage, ' ', aligned, ' ', padded); *)
						value := value - r^.shrinkage;
						(* writeln('           ', value); *)
					end;
				end;
				last := current;
				current := current^.next;
			end;

			(* walk through list backwards to check alignment,
				that is in order of occurrence *)
			adjustment := 0;
			current := last;
			while current <> nil do
			begin
				if shrink then
				begin
					sym := findSymbol(current^.name);
					with sym^.symboldata do
					begin
						if aligned then
						begin
							(* if this label needs alignment, shift all labels
								after this one by one instruction word (2 bytes) *)
							if ((value + adjustment) and wordSizeMask) = insSize then
							begin
								(* if a constant pool entry was already padded,
								   do not add second padding but remove first
								   padding instead *)
								if not padded then
								begin
									padded := true;
									pc := pc + insSize;
									adjustment := adjustment + insSize;
								end
								else
								begin
									padded := false;
									pc := pc - insSize;
									adjustment := adjustment - insSize;
								end;
							end;
						end;
						value := value + adjustment;
						(* if adjustment <> 0 then
							writeln('  ', current^.name, ' adjusted to ', value, ' by ', adjustment); *)
					end;
				end;
{$ifndef FPC}
				{writeln('   disposing ', current^.name, ' ', current);}
{$endif}
				next := current^.prev;
				dispose(current);
				current := next;
			end;
			r^.labels := nil;
		end;
		r := r^.next;
	end;
	deleteUnresBranch(name);
end;

(* Create a symbol table entry.
   On passes other than 1, symbol must exist and the value is updated. *)
procedure createSymbol(var keyword:IdentString; typ:SymbolType; value:integer; aligned, padded:boolean);
var d:TreeData;
    dref:TreeDataRef;
begin
	dref := TreeSearch(symbolTable, keyword);

	if pass = 1 then
	begin
		if dref <> nil then
			errorExit2('Duplicate label', keyword);
		d.typ := TDSymbol;
		d.symboldata.typ := typ;
		d.symboldata.value := value;
		d.symboldata.aligned := aligned;
		d.symboldata.padded := padded;
		d.symboldata.exported := false;
		TreeInsert(symbolTable, keyword, d);

		addUncertainLabel(keyword);
		checkUnresBranches(keyword);
	end
	else
	begin
		if dref = nil then
		begin
			dumpCPool;
			ErrorExit2('internal error in createSymbol', keyword);
		end;

		if dref^.symboldata.value <> value then
			(* writeln('////// label changed value ', keyword, ' ',
				dref^.symboldata.value, ' -> ', value); *)
		dref^.symboldata.value := value;
	end;
end;

(* Change the address of a label after it has been
   created. This happens if an alignment is required
   e.g. for a .WORD directive which has a label *)
procedure fixupLabel(oldPc, newPc:integer);
var current:TreeRef;
    walkState:TreeWalkState;
begin
	TreeWalkFirst(symbolTable, walkState, current);
	while current <> nil do
	begin
		if current^.data^.symboldata.value = oldPc then
		begin
			current^.data^.symboldata.aligned := true;
			current^.data^.symboldata.value := newPc;
			current^.data^.symboldata.padded := (newPc <> oldPc);

			(* if newPc <> oldPc then
				writeln('     aligning ', current^.key^, ' ', oldPc, ' -> ', newPc); *)
		end;
		TreeWalkNext(walkState, current);
	end;
end;

function parsePrimary:integer;
var applyNot:boolean;
    negate:boolean;
    value:integer;
begin
	if checkToken(TildeToken) then
	begin
		readNextToken;
		applyNot := true;
	end
	else
		applyNot := false;

	if checkToken(MinusToken) then
	begin
		readNextToken;
		negate := true;
	end
	else
		negate := false;

	if checkToken(NumberToken) then
	begin
		(* let convertNumber handle negative numbers
			because the statement
			"value :=  -value" below would
			not work for -maxint.
			abs(-maxint) cannot be represented
			as a signed 32-bit integer  *)
		if negate then
		begin
			value := convertNumber('-' + curToken.tokenText);
			negate := false;
		end
		else
			value := convertNumber(curToken.tokenText);
	end
	else
	if checkToken(KeywordToken) then
		value := getSymbolValue(curToken.tokenText)
	else
	if checkToken(CharLitToken) then
		value := convertChar(curToken.tokenText)
	else
	if checkToken(AtToken) then
		value := pc
	else
		errorExit2('number or symbol expected, got', descToken(curToken.tokenKind));

	if applyNot then
		value := not value;

	if negate then
		value := - value;

	readNextToken;

	parsePrimary := value;
end;

function parseExpression:integer;
var value:integer;

function parseNextPrimary:integer;
begin
	readNextToken;
	parseNextPrimary := parsePrimary();
end;

begin
	value := parsePrimary;

	while not (curToken.tokenKind in [ EOLToken, CommaToken ]) do
	begin
		if checkToken(PlusToken) then
			value := value + parseNextPrimary
		else
		if checkToken(MinusToken) then
			value := value - parseNextPrimary
		else
		if checkToken(AsteriskToken) then
			value := value * parseNextPrimary
		else
		if checkToken(AndToken) then
			value := value and parseNextPrimary
		else
		if checkToken(OrToken) then
			value := value or parseNextPrimary
		else
		if checkToken(XorToken) then
			value := value xor parseNextPrimary
		else
		if checkToken(SlashToken) then
			value := value  div parseNextPrimary
		else
			errorExit2('Expected one of + - * / & | ^ but got', curToken.tokenText);
	end;

	parseExpression := value;
end;

procedure parseLabel;
begin
	(* writeln(':::: parseLabel ', curToken.tokenText, ' at ', pc); *)
	createSymbol(curToken.tokenText, LabelSymbol, pc, false, false);

	readNextToken;
end;

procedure parseOneWordArg;
var operandValue:integer;
begin
	operandValue := parseExpression;
	emitWord(operandValue);
end;

procedure parseWordArgs;
begin
	alignOutput(wordSize);
	parseOneWordArg;
	while checkToken(CommaToken) do
	begin
		readNextToken;
		(* if there is a comma at the end of the line,
			continue to the next line *)
		if checkToken(EOLToken) then
			readNextToken;

		parseOneWordArg;
	end;
end;

procedure parseOneByteArg;
var bytevalue:integer;
    c:char;
begin
	if checkToken(StringLitToken) then
	begin
		for c in curToken.tokenText do
			emitByte(ord(c));
		readNextToken;
	end
	else
	begin
		bytevalue := parseExpression;
		emitByte(byteValue);
	end;
end;

procedure parseByteArgs;
begin
	alignOutput(wordSize);
	parseOneByteArg;
	while checkToken(CommaToken) do
	begin
		readNextToken;
		(* if there is a comma at the end of the line,
			continue to the next line *)
		if checkToken(EOLToken) then
			readNextToken;

		parseOneByteArg;
	end;

	(* align to word *)
	while (pc and wordsizeMask) <> 0 do
		emitByte(0);
end;

procedure dumpOpcodeTable;
var walkState:TreeWalkState;
    walkRes:TreeRef;
begin
	writeln('Opcode Table:');
	TreeWalkStart(opcodeTable, walkState);
	repeat
		TreeWalkNext(walkState, walkRes);
		if walkRes <> nil then
			writeln(walkRes^.key^);
	until walkRes = nil;
end;

function changeFileSuffix(filename: string; suffix:string): string; forward;

procedure writeSymbolTable;
var walkState:TreeWalkState;
    walkRes:TreeRef;
    h:string;
    f:SymFileType;
    fname:string;
    c:char;
begin
	fname := changeFileSuffix(filename, SymFileSuffix);
	overwriteFile(f, fname);

	TreeWalkStart(symbolTable, walkState);
	repeat
		TreeWalkNext(walkState, walkRes);
		if walkRes <> nil then
		begin
			if walkRes^.data^.symboldata.typ in [ LabelSymbol, ConstSymbol ] then
			begin
				hexstr(walkRes^.data^.symboldata.value, h);
				if walkRes^.data^.symboldata.typ = ConstSymbol then
					c := '='
				else
				if walkRes^.data^.symboldata.exported then
					c := '!'
				else
					c := ' ';
				writeln(f, h, ' ', c, walkRes^.key^);
			end;
		end;
	until walkRes = nil;
	close(f);
end;

procedure dumpSymbolTable;
var walkState:TreeWalkState;
    walkRes:TreeRef;
    h:string;
begin
	writeln('Symbol Table:');
	TreeWalkStart(symbolTable, walkState);
	repeat
		TreeWalkNext(walkState, walkRes);
		if walkRes <> nil then
		begin
			if walkRes^.data^.symboldata.typ = LabelSymbol then
			begin
				hexstr(walkRes^.data^.symboldata.value, h);
				writeln(h, ' ', walkRes^.key^);
			end;
		end;
	until walkRes = nil;
end;

procedure dumpCPool;
var current:CPoolRef;
begin
	writeln('dump constant pool ', cPoolCount, ' ', nextConstId);

	current := constantPool;

	while current <> nil do
	begin
		writeln(current^.labelname, ': ', current^.value, ' ',
			getSymbolValue(current^.labelname), ' ');
		current := current^.next;
	end;
end;

procedure emitLoadcp(var operand:KeywordString; numOffset:integer);
var cpooladdr:integer;
    encoded:InstructionWord;
begin
	cpooladdr := getCPoolAddr(operand, numOffset);
	(* writeln('** emitLoadcp for ',operand, '/', numOffset, ' ', cpooladdr); *)
	encodeInstruction('LOADREL', cpooladdr, encoded);
	emitInstructionWord(encoded);
end;

procedure parseLoadcp;
var opstr:KeywordString;
    data:TreeDataRef;
    numOffset:integer;
begin
	if not (curToken.tokenKind in [ KeywordToken, NumberToken, MinusToken ]) then
		errorExit2('Identifier or number expected, got', curToken.tokenText);

	if checkToken(MinusToken) then
	begin
		opstr := '-';
		readNextToken;
		if not checkToken(NumberToken) then
			errorExit2('Invalid number', curToken.tokenText);
		opstr := opstr + curToken.tokenText;
	end
	else
		opstr := curToken.tokenText;

	readNextToken;

	(* check for optional offset *)
	if checkToken(CommaToken) then
	begin
		readNextToken;
		(* offset can be either a number literal *)
		if checkToken(NumberToken) then
			numOffset := convertNumber(curToken.tokenText)
		else
		(* or a symbol *)
		if checkToken(KeywordToken) then
		begin
			data := findSymbol(curToken.tokenText);
			if data = nil then
				errorExit2('Cannot use unresolved symbol for LOADCP offset:',
					curToken.tokenText);
			numOffset := data^.symboldata.value;
		end
		else
			errorExit2('Number or symbol required, got', curToken.tokenText);
		readNextToken;
	end
	else
		numOffset := 0;

	emitLoadcp(opstr, numOffset);
end;

procedure parseLbranch;
var value:integer;
    distance:integer;
    offset,shrinkage:integer;
    pad:boolean;
    encoded:InstructionWord;
begin
	if not checkToken(KeywordToken) then
		errorExit2('identifier expected, got', curToken.tokenText);

	value := getLabelAddr(curToken.tokenText);

	distance := value - pc;
	if (value = Unresolved) or (distance > 4095) or (distance < -4096) then
	begin
		if (pc and 3) = 0 then	(* no padding *)
		begin
			pad := false;
			(* total size 8 bytes *)
			offset := 4;	(* offset for LOADREL *)
			shrinkage := 6; (* difference to short form size *)
		end
		else
		begin
			pad := true;
			(* total size 10 bytes *)
			offset := 6;	(* offset for LOADREL with padding *)
			shrinkage := 8; (* difference to short form size *)
		end;
		if value = Unresolved then
			addUnresBranch(curToken.tokenText, pc, 4095, shrinkage);

		encodeInstruction('LOADREL', pc + offset, encoded);
		emitInstructionWord(encoded);

		encodeInstruction('JUMP', 0, encoded);
		emitInstructionWord(encoded);

		if pad then
			emitInstructionWord(0);
		emitWord(value);
	end
	else
	begin
		encodeInstruction('BRANCH', value, encoded);
		emitInstructionWord(encoded);
	end;

	(*
	if value = Unresolved then
		writeln('** parseLbranch ', curToken.tokenText, ' unresolved')
	else
	if (distance > 4095) then
		writeln('** parseLbranch ', curToken.tokenText, ' long ', distance)
	else
		writeln('** parseLbranch ', curToken.tokenText, ' short ', distance);
	*)

	readNextToken;
end;

procedure parseLcbranch(negate:boolean);
var value:integer;
    relValue:integer;
    offset,shrinkage:integer;
    encoded:InstructionWord;
    modifier:string[4];
    pad:boolean;
begin
	if not checkToken(KeywordToken) then
		errorExit2('identifier expected, got', curToken.tokenText);

	value := getLabelAddr(curToken.tokenText);

	relValue := value - pc;

	modifier := '';

	if (value = Unresolved) or (relValue > 4095) or (relValue < -4096) then
	begin
		if (pc and 3) = 2 then (* no padding *)
		begin
			pad := false;
			(* total size 10 bytes *)
			offset := 4;	(* offset for LOADREL *)
			shrinkage := 8; (* difference to short form size *)
		end
		else
		begin
			pad := true;
			(* total size 12 bytes *)
			offset := 6;	 (* offset for LOADREL with padding *)
			shrinkage := 10; (* difference to short form size *)
		end;

		if value = Unresolved then
			addUnresBranch(curToken.tokenText, pc, 4095, shrinkage);

		(* writeln('*** long cbranch triggered:', value, ' ', relValue, ' pass:',pass);
		writeln('***   pc ', pc, ' offset ', offset, ' insSize ', insSize);
		writeln('***   ', pc + offset + insSize); *)
		if not negate then
			modifier := '.Z';
		(* branch over CBRANCH, LOADREL, padding and literal value *)
		encodeInstruction('CBRANCH' + modifier, pc + insSize + offset + wordSize, encoded);
		emitInstructionWord(encoded);

		encodeInstruction('LOADREL', pc + offset, encoded);
		emitInstructionWord(encoded);

		encodeInstruction('JUMP', 0, encoded);
		emitInstructionWord(encoded);

		if pad then
			emitInstructionWord(0);
		emitWord(value);
	end
	else
	begin
		if negate then
			modifier := '.Z';
		encodeInstruction('CBRANCH' + modifier, value, encoded);
		emitInstructionWord(encoded);
	end;

	readNextToken;
end;

procedure parseDirective;
var operandValue:integer;
    count:integer;
    name:IdentString;
    oldsym:TreeDataRef;
begin
	readNextToken;

	if lastToken.tokenText = '.LBRANCH' then
		parseLbranch
	else
	if lastToken.tokenText = '.LCBRANCH' then
		parseLcbranch(false)
	else
	if lastToken.tokenText = '.LCBRANCHZ' then
		parseLcbranch(true)
	else
	if lastToken.tokenText = '.ORG' then
	begin
		operandValue := parseExpression;
		pc := operandValue;
	end
	else
	if lastToken.tokenText = '.EQU' then
	begin
		matchToken(KeywordToken);
		name := lastToken.tokenText;
		operandValue := parseExpression;

		oldsym := findSymbol(name);
		if oldsym <> nil then
		begin
			if oldsym^.symboldata.value <> operandValue then
				errorExit2('Symbol already declared:', name);
		end
		else
			createSymbol(name, ConstSymbol, operandValue, false, false);
	end
	else
	if lastToken.tokenText = '.WORD' then
		parseWordArgs
	else
	if lastToken.tokenText = '.BYTE' then
		parseByteArgs
	else
	if lastToken.tokenText = '.CPOOL' then
		emitConstantPool(false)
	else
	if lastToken.tokenText = '.CPOOLNOP' then
		emitConstantPool(true)
	else
	if lastToken.tokenText = '.BLOCK' then
	begin
		count := parseExpression;
		if matchTokenOrNot(CommaToken) then
			operandValue := parseExpression
		else
			operandValue := 0;
		emitBlock(count, operandValue);
	end
	else
		errorExit2('Unrecognized directive', lastToken.tokenText);
end;

procedure parseMetaDirective;
var filename:string;
    sym:TreeDataRef;
begin
	readNextToken;
	if lastToken.tokenText = '%INCLUDE' then
	begin
		if curToken.tokenKind in [StringLitToken, KeywordToken] then
		begin
			filename := curToken.tokenText;
			readNextToken;
			beginInclude(filename);
		end
		else
			errorExit2('Filename expected', '');
	end
	else
	if lastToken.tokenText = '%EXPORT' then
	begin
		if curToken.tokenKind = KeywordToken then
		begin
			sym := findSymbol(curToken.tokenText);
			if sym = nil then
				errorExit2('Undeclared symbol', curToken.tokenText);
			sym^.symboldata.exported := true;
		end
		else
			errorExit2('Symbol expected', '');
		readNextToken;
	end
	else
		errorExit2('Invalid meta directive', lastToken.tokenText);
end;

procedure encode(var entry:EncodingEntry; var value:InstructionWord);
begin
	value := (value and not entry.mask) or entry.value;
end;

procedure getModifier(var m:KeywordString; var opcode:OpcodeData; var encoded:InstructionWord);
var cur:^ModifierEntry;
begin
	if opcode.id <> 0 then
	begin
		cur := opcode.modifiers;
		while cur <> nil do
		begin
			if m = cur^.keyword then
			begin
				encode(cur^.encoding, encoded);
				break;
			end;
			cur := cur^.next;
		end;
		if cur = nil then
			errorExit2('Invalid modifier', m);
	end;
end;

procedure getMnemonic(var m:KeywordString; var opcode:OpcodeData; var encoded:InstructionWord);
var data:TreeDataRef;
    ch:char;
begin
	if m[1] = shortcutChar then
	begin
		ch := m[2];
		if (ch < firstShCChar)	or (ch > lastShCChar) then
			errorExit2('invalid shortcut', m);
		opcode := shortcuts[ch];
		if opcode.id = -1 then
			errorExit2('invalid shortcut', m);
		encode(opcode.encoding, encoded);
	end
	else
	begin
		data := TreeSearch(opcodeTable, m);
		if data = nil then
		begin
			errorExit2('Unrecognized instruction', m);
			opcode.id := 0;
		end
		else
		begin
			opcode := data^.opcodedata;
			encode(opcode.encoding, encoded);
		end;
	end;
end;

procedure getBaseAndModifiers(var ins:KeywordString; var opcode:OpcodeData; var encoded:InstructionWord);
var i:integer;
    insLength:integer;
    slice:string;
    startPos:integer;

function scanchar(c:char; curPos:integer):integer;
begin
	scanchar := 0;

	while curPos <= insLength do
	begin
		if ins[curPos] = c then
		begin
			scanchar := curPos;
			break;
		end
		else
			curPos := curPos + 1;
	end;
end;

begin
	encoded := 0;
	insLength := length(ins);

	i := pos('.',ins);

	{ writeln('** getModifiers ',i); }

	if i > 1 then
	begin
		slice := copy(ins,1,i-1);
		getMnemonic(slice, opcode, encoded);
		repeat
			startPos := i + 1;
			i := scanchar('.', startPos);
			if i > 0 then
			begin
				slice := copy(ins,startPos,i-startPos);
				getModifier(slice, opcode, encoded);
			end;
		until i < 1;

		(* last slice *)
		slice := copy(ins, startPos, insLength-startPos+1);
		getModifier(slice, opcode, encoded);
	end
	else
		getMnemonic(ins, opcode, encoded);
end;

procedure parseInstruction;
var operandValue:integer;
    opcode:OpcodeData;
    encodedIns:InstructionWord;
begin
	getBaseAndModifiers(curToken.tokenText, opcode, encodedIns);

	readNextToken;

	if opcode.id = LOADCPId then
		parseLoadcp
	else
	begin
		if opcode.operand <> NoOprnd then
		begin
			if not checkToken(EOLToken) then
				operandValue := parseExpression
			else
			begin
				if opcode.operand = OptOprnd then
					operandValue := 0
				else
					errorExit2('Missing operand', lastToken.tokenText)
			end
		end
		else
			operandValue := 0;

		encodeOperand(operandValue, opcode, encodedIns);
		emitInstruction(opcode, encodedIns);
	end;
end;

procedure parseLine;
begin
	(* writeln('## P', pass, ' line ', lineno:4, ' pc ', pc:8); *)

	if checkToken(LabelToken) then
		parseLabel;

	if checkToken(DirectiveToken) then
		parseDirective
	else
	if checkToken(KeywordToken) then
		parseInstruction
	else
	if checkToken(MetaKeywordToken) then
		parseMetaDirective
	else
	if checkToken(EOLToken) then
		begin end (* empty line *)
	else
	begin
		(* writeln(curToken.tokenKind); *)
		errorExit2('Invalid syntax', curToken.tokenText);
	end;

	matchToken(EOLToken);
end;

procedure parseFile;
begin
	readNextToken;
	repeat
		parseLine;
		if (lineno and progressSteps) = 0 then
			printCurrentLineno;
	until checkToken(EOFToken);
	emitConstantPool(false);
end;

(* Add an instruction to the opcodeTable (which is a tree).
   mask specifies the bits which are used by the opcode.
*)
procedure addOpcode(mnemonic:InsString; value, mask:integer; oprnd: OperandType);
var data:TreeData;

begin
	data.typ := TDOpcode;
	data.opcodedata.encoding.value := value;
	data.opcodedata.encoding.mask  := mask;
	data.opcodedata.modifiers := nil;
	data.opcodedata.operand := oprnd;
	data.opcodedata.id := nextOpcodeId;

	nextOpcodeId := nextOpcodeId + 1;

	TreeInsert(opcodeTable, mnemonic, data);

	lastOpcode := TreeSearch(opcodeTable, mnemonic);
end;

(* Add a modifier to the instruction that was last added by addOpcode.
   mask specifies the bits used by the modifier.
*)
procedure addModifier(key:InsString; value, mask:integer);
var newModifier:^ModifierEntry;
    cur:^ModifierEntry;
begin
	if lastOpcode = nil then
		errorExit2('internal error in addModifier', key);

	new(newModifier);
	newModifier^.keyword := key;
	newModifier^.encoding.value := value;
	newModifier^.encoding.mask  := mask;
	newModifier^.next := nil;


	cur := lastOpcode^.opcodedata.modifiers;
	if cur = nil then
	begin
		lastOpcode^.opcodedata.modifiers := newModifier
	end
	else
	begin
		while cur^.next <> nil do cur := cur^.next;
		cur^.next := newModifier;
	end;
end;

procedure addAlias(key:InsString; dest:InsString);
var opcode:OpcodeData;
    encoded:InstructionWord;
begin
	(* encode the instruction and modifiers *)
	getBaseAndModifiers(dest, opcode, encoded);
	(* add a new opcode entry with the alias name and
		the values we just calculated *)
	addOpcode(key, encoded, opcode.encoding.mask, opcode.operand);
	(* copy the modifier list (list of valid modifiers)
		from the original instruction *)
	lastOpcode^.opcodedata.modifiers := opcode.modifiers;
end;

procedure addShortcut(ch:char; dest:InsString);
var opcode:OpcodeData;
    encoded:InstructionWord;
begin
	if shortcuts[ch].id <> -1 then
		errorExit2('internal error in addShortcut for', dest);

	getBaseAndModifiers(dest, opcode, encoded);
	shortcuts[ch] := opcode;
end;

procedure addSpecialOperand(key:IdentString; value:integer);
begin
	createSymbol(key, SpecialSymbol, value, false, false)
end;

procedure initSpecialOperands;
begin
	pass := 1; (* createSymbol only creates symbols on pass 1 *)

	(* create special operand symbols *)
	addSpecialOperand('FP', 0);
	addSpecialOperand('BP', 1);
	addSpecialOperand('RP', 2);
	addSpecialOperand('IV', 3);
	addSpecialOperand('IR', 4);
	addSpecialOperand('ESP', 5);
	addSpecialOperand('EQ', 2);
	addSpecialOperand('LT', 1);
	addSpecialOperand('NE', 6);
	addSpecialOperand('LE', 3);
	addSpecialOperand('GE', 5);
	addSpecialOperand('GT', 7);
end;

procedure initOpcodes;
begin
	addOpcode('BRANCH', $0000, $E000, RelWOprnd);

	addOpcode('LOADC',  $C000, $E000, S13Oprnd);

	addOpcode('LOAD',   $8000, $E000, U13WOprnd);
	addModifier('B',    $0001, $0001);

	addOpcode('STORE',  $4000, $E000, U13WOprnd);
	addModifier('B',    $0001, $0001);

	addOpcode('CBRANCH',$A001, $E001, RelWOprnd);
	addModifier('N',    $0001, $0001);
	addModifier('NZ',   $0001, $0001);
	addModifier('Z',    $0000, $0001);

	addOpcode('XFER',   $6000, $E000, NoOprnd);
	addModifier('RSM1', $0300, $0300);
	addModifier('RS0',  $0000, $0300);
	addModifier('RS1',  $0100, $0300);
	addModifier('R2P',  $0080, $0080);
	addModifier('P2R',  $0040, $0040);
	addModifier('SM1',  $0030, $0030);
	addModifier('S0',   $0000, $0030);
	addModifier('S1',   $0010, $0030);
	addModifier('X2P',  $0001, $0001);

	addOpcode('ALU',    $2000, $E000, OptOprnd);
	addModifier('SM1',  $0030, $0030);
	addModifier('S0',   $0000, $0030);
	addModifier('S1',   $0010, $0030);
	addModifier('X2Y',  $0040, $0040);
	addModifier('NX2Y', $0000, $0040);
	addModifier('XT',   $0080, $0080);
	addModifier('ADD',  $0000, $1e00);
	addModifier('SUB',  $0200, $1e00);
	addModifier('NOT',  $0400, $1e00);
	addModifier('AND',  $0600, $1e00);
	addModifier('OR',   $0800, $1e00);
	addModifier('XOR',  $0a00, $1e00);
	addModifier('CMP',  $0c00, $1e00);
	addModifier('Y',    $0e00, $1e00);
	addModifier('SHR',  $1000, $1e00);
	addModifier('SHL',  $1200, $1e00);
	addModifier('INC',  $1400, $1e00);
	addModifier('DEC',  $1600, $1e00);
	addModifier('BPLC', $1a00, $1e00);
	addModifier('BROT', $1c00, $1e00);
	addModifier('BSEL', $1e00, $1e00);
	addModifier('CMPU', $1800, $1e00);

	(* addOpcode('EXT',    $E000, $E000); *)

	addOpcode('MEM',    $E400, $FFF0, OptOprnd);
	addModifier('W',    $0200, $0200);
	addModifier('SM1',  $0030, $0030);
	addModifier('S0',   $0000, $0030);
	addModifier('S1',   $0010, $0030);
	addModifier('X2Y',  $0040, $0040);
	addModifier('NX2Y', $0000, $0040);

	addOpcode('LOADREL',$F400, $FC00, RelU10Oprnd);

	LOADCPId := nextOpcodeId;
	addOpcode('LOADCP', $F400, $FC00, RelU10Oprnd);

	addOpcode('REG',    $E000, $FFF0, RegOprnd);
	addModifier('W',    $0200, $0200);

	addOpcode('FPADJ',  $EC00, $FC00, S10Oprnd);

	addAlias('JUMP', 'XFER.SM1.X2P');
	addAlias('CALL', 'XFER.RS1.SM1.P2R.X2P');
	addAlias('RET', 'XFER.RSM1.R2P');
	addAlias('ADD', 'ALU.ADD.SM1');
	addAlias('SUB', 'ALU.SUB.SM1');
	addAlias('NOT', 'ALU.NOT.S0');
	addAlias('AND', 'ALU.AND.SM1');
	addAlias('OR', 'ALU.OR.SM1');
	addAlias('XOR', 'ALU.XOR.SM1');
	addAlias('CMP', 'ALU.CMP.SM1');
	addAlias('SHR', 'ALU.SHR.S0');
	addAlias('SHL', 'ALU.SHL.S0');
	addAlias('DUP', 'ALU.INC.S1.X2Y');
	addAlias('NIP', 'ALU.INC.SM1');
	addAlias('INC', 'ALU.INC.S0');
	addAlias('DEC', 'ALU.DEC.S0');
	addAlias('CMPU', 'ALU.CMPU.SM1');
	addAlias('BPLC', 'ALU.BPLC.SM1');
	addAlias('BROT', 'ALU.BROT.S0');
	addAlias('BSEL', 'ALU.BSEL.SM1');
	addAlias('Y', 'ALU.Y.S1.X2Y');
	addAlias('DROP', 'ALU.Y.SM1');
	addAlias('SWAP', 'ALU.Y.S0.X2Y');
	addAlias('OVER', 'ALU.Y.S1.X2Y');
	addAlias('LOADI', 'MEM');
	addAlias('STOREI', 'MEM.W.SM1');
	addAlias('LOADREG', 'REG');
	addAlias('STOREREG', 'REG.W');
end;

procedure initShortcuts;
var ch:char;
begin
	for ch := firstShCChar to lastShCChar do
		shortcuts[ch].id := -1;

	addShortcut('A', 'ADD');
	addShortcut('B', 'BRANCH');
	addShortcut('C', 'CALL');
	addShortcut('D', 'DUP');
	addShortcut('E', 'LOADREL');
	addShortcut('F', 'LOAD');
	addShortcut('G', 'LOADREG');
	addShortcut('H', 'SHL');
	addShortcut('I', 'LOADI');
	addShortcut('J', 'JUMP');
	addShortcut('K', 'LOADC');
	addShortcut('L', 'LOADCP');
	addShortcut('M', 'STORE');
	addShortcut('N', 'NIP');
	addShortcut('O', 'OR');
	addShortcut('P', 'DROP');
	(* Q is unused *)
	addShortcut('R', 'RET');
	addShortcut('S', 'STOREI');
	addShortcut('T', 'NOT');
	addShortcut('U', 'CMPU');
	addShortcut('V', 'OVER');
	addShortcut('W', 'SWAP');
	addShortcut('X', 'XOR');
	(* Y is ununsed *)
	addShortcut('Z', 'SUB');
	addShortcut('a', 'AND');
	addShortcut('b', 'CBRANCH');
	addShortcut('c', 'CMP');
	addShortcut('d', 'DEC');
	(* e is unused *)
	addShortcut('f', 'FPADJ');
	addShortcut('g', 'STOREREG');
	addShortcut('h', 'SHR');
	addShortcut('i', 'INC');
end;

function changeFileSuffix(filename: string; suffix:string): string;
var suffixPos:integer;
begin
	suffixPos := pos(filenameSuffix, filename);
	if suffixPos > 0 then
		setlength(filename, suffixPos-1);
	filename := filename + suffix;
	changeFileSuffix := filename;
end;

procedure performPass(passNo:integer);
begin
	lineno := 1;
	nextConstId := 0;
	cPoolCount := 0;
	pc := 0;
	bytesCount := 0;
	pass := passNo;

	outputEnabled := pass = 2;

	openFileWithDefault(infile, filename);
	infileOpened := true;

	if outputEnabled then
		overwriteFile(outfile, outfilename);

	parseFile;
	printLastLineno;

	close(infile);

	if outputEnabled then
		close(outfile);

	(* dumpSymbolTable; *)
end;

procedure verifyNodeKey(node:TreeRef);
var c:integer;
begin
	if node = nil then
		errorExit2('verifyNodeKey FAIL node is nil', '');
	if node^.key = nil then
		errorExit2('verifyNodeKey FAIL key is nil', '');

	if length(node^.key^) < 1 then
		errorExit2('verifyNodeKey FAIL key has zero length', '');

	c := ord(node^.key^[1]);

	if not (
			((c >= ord('0')) and (c <= ord('9')))
		or
			((c >= ord('A')) and (c <= ord('F')))
	) then
	begin
		writeln('verifyNodeKey FAIL at ', node^.key^, ' ', c);
		if node^.parent <> nil then
			writeln('  parent:', node^.parent^.key^);
		errorExit;
	end;
end;

procedure verifyTree(node:TreeRef);
begin
	if node <> nil then
	begin
		verifyNodeKey(node);

		if node^.right <> nil then
		begin
			if node^.right^.parent <> node then
				errorExit2('verifyTree FAIL parent check right at', node^.key^);
			verifyTree(node^.right);
		end;

		if node^.left <> nil then
		begin
			if node^.left^.parent <> node then
				errorExit2('verifyTree FAIL parent check left at', node^.key^);
			verifyTree(node^.left);
		end;
	end;
end;

begin
	infileOpened := false;
	outputEnabled := false;
	asciiOutput := false;
	editOnError := false;
	runOnSuccess := false;
	buffered := false;
	includeLevel := 0;
	symbolTable := nil;
	opcodeTable := nil;
	lastOpcode := nil;
	firstUnresBranch := nil;

	nextOpcodeId := 1;

	if ParamCount < 1 then halt;

	paramPos := 1;
	filename := '';
	outfilename := '';

	while paramPos <= ParamCount do
	begin
		if paramStr(paramPos) = '-e' then
			editOnError := true
		else
		if paramStr(paramPos) = '-R' then
			runOnSuccess := true
		else
		if paramStr(paramPos) = '-A' then
			asciiOutput := true
		else
		begin
			if length(filename) = 0 then
				filename := ParamStr(paramPos)
			else
				outfilename := ParamStr(paramPos);
		end;
		paramPos := paramPos + 1;
	end;

	initPlatform;
	initOpcodes;
	initSpecialOperands;
	initShortcuts;

	if length(outfilename) = 0 then
	begin
		if asciiOutput then
			outfilename := changeFileSuffix(filename, asciifileSuffix)
		else
			outfilename := changeFileSuffix(filename, outfileSuffix)
	end;

	writeln('Assembling ', filename, ' to ', outfilename);

	performPass(1);
	performPass(2);

	writeln(#13, lineno - 1, ' lines, program size ', bytesCount, ' bytes.');

	(* dumpOpcodeTable; *)
	(* dumpSymbolTable; *)
	writeSymbolTable;

	if runOnSuccess then
		ExecProgram(outfilename);
end.
