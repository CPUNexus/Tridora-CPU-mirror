(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
program PascalCompiler;
{$R+}
{$!}{$ifdef FPC}uses math,crt;{$endif}

type 	TokenType = (
		AssignmentToken, PlusToken, MinusToken, AsteriskToken, SlashToken,
		SemicolonToken, EOFToken, PointerToken,
		CommentStartToken, CommentEndToken, CommentAltStartToken, CommentAltEndToken,
		NumberToken,
		IdentToken, StringLitToken, CharLitToken,
		StringToken, IntegerToken,
		BooleanToken, RealToken, CharToken, TrueToken, FalseToken,
		LabelToken, GotoToken,
		IfToken, ThenToken, ElseToken, WhileToken,
		RepeatToken, DoToken, UntilToken, ForToken, ToToken, InToken,
		DowntoToken, BreakToken, ContinueToken,
		BeginToken, EndToken, WithToken,
		VarToken, TypeToken, CaseToken, ConstToken, RecordToken,
		CommaToken, EqToken, EqEqToken, NotEqToken, LtToken,
		LtEqToken, GtToken, GtEqToken, LParenToken, RParenToken,
		LBracketToken, RBracketToken, ColonToken,
		NotToken, AndToken, OrToken, XorToken, DivToken, ModToken,
		ShlToken, ShrToken,	NilToken,
		ProcedureToken, FunctionToken,
		ArrayToken, OfToken, DotToken, SetToken,
		ForwardToken, ExternalToken, ProgramToken, PackedToken,
		UnitToken, ImplementationToken, InterfaceToken, UsesToken,
		UnknownToken
	);

	IdentString = string[120];
	CompOpString = string[4];
	KeywordString = string[255];
	TypeTagString = string[8];

	StringRef = ^string;

	SymbolScope = ( GlobalSymbol, LocalSymbol, ParameterSymbol, WithStmntSymbol );
	SymbolType  = ( NoType, IntegerType, StringType, RealType, BooleanType, CharType,
		ArrayType, RecordType, PointerType, StringCharType, EnumType, 
		SetType, UnresolvedType );

	SpecialProc = ( NoSP, NewSP, DisposeSP, ReadSP, WriteSP, ReadlnSP, WritelnSP,
			SetlengthSP, ValSP, StrSP, ExitSP );
	SpecialFunc = ( NoSF, TruncSF, FracSF, IntSF, SqrSF, SuccSF, PredSF,
			OddSF, ChrSF, OrdSF, AbsSF);

 	Token = record
		tokenText: string[255];
		tokenKind: TokenType;
	end;

	StringList = record
		head: ^StringListItem;
		tail: ^StringListItem;
		current: ^StringListItem;
	end;

	StringListItem = record
		value: IdentString;
		next: ^StringListItem;
	end;

	IntList = record
		head: ^IntListItem;
		tail: ^IntListItem;
		current: ^IntListItem;
	end;

	IntListItem = record
		value: integer;
		next: ^IntListItem;
	end;

	TypeSpec = record
		size: integer; (* size in bytes *)
		subStart: integer;
		subEnd: integer;
		hasSubrange: boolean;
		case baseType: SymbolType of
			IntegerType,RealType,BooleanType,CharType: ();
			EnumType: (enumId, enumLength: integer; enumList: StringList);
			StringType: (stringLength: integer); (* max length *)
			ArrayType: (arrayLength, arrayStart, arrayEnd: integer;
						elementType: ^TypeSpec; indexEnumId: integer);
			RecordType: (fields: ^FieldListItem);
			PointerType: (pointedType: ^TypeSpec);
			SetType: (memberBaseType: SymbolType; memberEnumId: integer);
			StringCharType: (); (* used internally when getting a char from a string *)
			UnresolvedType:(sourceLine:integer; typeName: ^IdentString);
	end;

	TypeSpecPtr = ^TypeSpec;

	TypeRef = ^TypeItem;

	TypeItem = record
		typePtr: ^TypeSpec;
		name: IdentString;
		next: TypeRef;
	end;

	FieldListItem = record
		fieldType: TypeSpec;
		name: IdentString;
		offset: integer;
		isVariant: boolean;
		tagField: ^FieldListItem;
		tagValues: IntList;
		next: ^FieldListItem;
	end;

	FieldRef = ^FieldListItem;

	SymblRef = ^Symbl;
	Symbl = record
		name: IdentString;
		symType: TypeSpec;
		scope: SymbolScope;
		level: integer;
		size: integer;
		offset: integer;
		isParam: boolean;
		isVarParam: boolean;
		isConst: boolean;
		isExternal: boolean;
		initialized: boolean;
		initialValue: integer;
		hasInitialValue: boolean;
		withStmntSlot: integer;
		next: SymblRef;
	end;

	MemLocType = (NoMem, GlobalMem, LocalMem, NestedMem, Indirect, TemporaryMem, OnStack);

	MemLocation = record
		memLoc: MemLocType;
		offset: integer;
		scopeDistance: integer;
		name: IdentString;
		typ: TypeSpec;
		initialized: boolean;
		origSym: SymblRef;
	end;

	SymbolTable = record
		first: SymblRef;
		offset: integer;
		scope: SymbolScope;
		level: integer;
	end;

	LablRef = ^Labl;
	Labl = record
		name: IdentString;
		id:   integer;
		next: LablRef;
	end;

	OpaqueDataElement = record
		(* TODO: need optional string values here
			if we want to have readable record fields
			or arrays of string type *)
		next: ^OpaqueDataElement;
		case isStringValue: boolean of
		false: (intValue: integer);
		true:  (strValue: ^string; maxLength:integer);
	end;

	OpaqueDataRef = ^OpaqueDataElement;

	ArrayConstList = record
		id: integer;
		count: integer;
		firstElement: ^OpaqueDataElement;
		next: ^ArrayConstList;
		extraLabel: ^IdentString;
	end;

	ArrayConstRef = ^ArrayConstList;

	ConstStrRef = ^ConstStr;
	ConstStr = record
		no: integer;
		value: string[255];
		length: integer;
		extraLabel: ^IdentString;
		next: ConstStrRef;
	end;

	ConstListItem = record
		next: ^ConstListItem;
		name: IdentString;
		typ: TypeSpec;
		realValue: real;
		intValue: integer;
		arrayValue: ArrayConstRef; (* FIXME: rename to opaqueValue or similar *)
		strValue: ConstStrRef;
		enumRef: TypeRef;
	end;

	ConstRef = ^ConstListItem;

	ProcRef = ^Proc;
	Proc = record
		name: IdentString;
		id: integer;
		parent: ProcRef;
		level: integer;
		isForward: boolean;
		isNested: boolean;
		hasNested: boolean;
		parameters: SymbolTable;
		vars: SymbolTable;
		returnType: TypeSpec;
		returnsAggregate: boolean;
		next: ProcRef;
		procedures: ProcRef;
		labels: LablRef;
		constants: ConstRef;
		types: TypeRef;
		unresolved: TypeRef;
		tempsSize: integer;
		estackCleanup: integer;
		hasExit: boolean;
	end;

	WithStmntAnchor = record
		recordLoc: MemLocation;
		tempLoc: MemLocation;
		tmpSymbol: SymblRef;
	end;

{$I 'platform-types+.pas' }

	InputFileState = record
		name: string;
		filevar: InputFileType;
		line: integer;
	end;

const	insSize = 2;
      	wordSize = 4;
		lowCpoolMark = 240;
		highCpoolMark = 400;
		StringHeaderSize = 8;
		MaxUShortOffset = 8191;
		MaxShortOffset = 4095;
		MaxShorterOffset = 511;
		MaxTinyOffset  = 15;
		WithStackDepth = 8;
		DefaultStringLength = 80;
		MaxIntegerDigits = 24;
		Float32ExpBits = 8;
		Float32FractBits = 23;
		Float32ExpBias = 127;
		Float32ExpMax = 255;
		wordBits = 32;
		startAddress = 24576;
		MaxIncludes = 4;
		StdLibName = 'stdlib';
		UnitSuffix1 = '.inc';
		UnitSuffix2 = '.lib';
		FilenameSuffix = '.pas';
		OutfileSuffix = '.s';
		InputFileName = 'INPUT';
		OutputFileName = 'OUTPUT';
		FileTypeName = 'FILE';
		PlatformTag = 'tdr';
		PlatformMagic = '+';
		ProgressSteps = 255;
var
		keywords: array [TokenType] of string[32] = (
		':=', '+', '-', '*', '/', ';' , '<end-of-file>', '^', '{', '}', '(*', '*)',
		'number', 'identifier', '$', 'c',
		'STRING', 'INTEGER', 'BOOLEAN', 'REAL', 'CHAR', 'TRUE', 'FALSE',
		'LABEL', 'GOTO', 'IF', 'THEN', 'ELSE',
		'WHILE', 'REPEAT', 'DO', 'UNTIL', 'FOR', 'TO', 'IN',
		'DOWNTO', 'BREAK', 'CONTINUE',
		'BEGIN', 'END', 'WITH',
		'VAR', 'TYPE', 'CASE', 'CONST', 'RECORD',
		',', '=', '==', '!=', '<', '<=', '>', '>=',
		'(', ')', '[', ']', ':', 'NOT', 'AND', 'OR', 'XOR', 'DIV', 'MOD',
		'SHL', 'SHR', 'NIL',
		'PROCEDURE', 'FUNCTION',
		'ARRAY', 'OF', '.', 'SET',
		'FORWARD', 'EXTERNAL', 'PROGRAM', 'PACKED',
		'UNIT', 'IMPLEMENTATION', 'INTERFACE', 'USES',
		'_' );
		specialprocnames: array [SpecialProc] of string[12] = (
			'_', 'NEW', 'DISPOSE', 'READ', 'WRITE', 'READLN', 'WRITELN', 'SETLENGTH',
			'VAL','STR', 'EXIT');
		specialfuncnames: array [SpecialFunc] of string[8] = (
			'_', 'TRUNC', 'FRAC', 'INT', 'SQR', 'SUCC', 'PRED', 'ODD',
			'CHR', 'ORD', 'ABS' );
		typenames: array[SymbolType] of string[8] = (
			'NONE?', 'INTEGER', 'STRING', 'REAL', 'BOOLEAN', 'CHAR', 'ARRAY', 'RECORD',
			'POINTER', 'STRCHR?', 'ENUM', 'SET', 'UNRES?'
		);
	curToken, nextToken, lastToken: Token;
	bufferedChar: char;
	buffered: boolean;
	infile: InputFileType;
	outfile: text;
	filename: string;
	outfilename: string;
	lineno: integer;
	ifCount: integer;
	whileCount: integer;
	forCount: integer;
	repeatCount: integer;
	caseCount: integer;
	nestedProcsCount: integer;
	enumCount: integer;
	anonTypeCount: integer;
	curBreakLabel: IdentString;
	firstConstStr, lastConstStr: ConstStrRef;
	firstArrayConst, lastArrayConst: ArrayConstRef;
	constStrNo: integer;
	arrayConstNo: integer;
	curProcedure: ProcRef;
	mainProcedure: ProcRef;
	defaultHeapSize: integer;
	defaultStackSize: integer;
	insCount: integer;
	emptyIntList: IntList;
	withStmntStack: array [1..WithStackDepth] of WithStmntAnchor;
	withStmntCount: integer;
	globalSuffix: IdentString;
	fileTyp: TypeSpec;
	useStdlib, useStandalone: boolean;
	editOnError, runAsm, runProg: boolean;
	paramPos: integer;
	prevFiles: array[1..MaxIncludes] of InputFileState;
	includeLevel: integer;
	usedUnits: StringList;
	outputPrefix: string[16];
	includePrefix: string[16];

procedure errorExit2(message1, message2: string); forward;
procedure errorExit1(message1: string); forward;
procedure checkDuplicateSymbol(var name:IdentString); forward;
function getStringWordCount(maxLength: integer): integer; forward;
procedure readNextToken; forward;
procedure matchToken(kind: TokenType); forward;
function checkToken(kind: TokenType): boolean; forward;
procedure parseExpression(var typeReturn: TypeSpec); forward;
procedure errorLine(line:integer); forward;
procedure errorExit; forward;
function isScalar(var typ: TypeSpec): boolean; forward;
function isFunction(aProc: ProcRef): boolean; forward;
procedure getRangePart(var value:integer; var typeReturn: TypeSpec); forward;
function parseInteger: Integer; forward;
procedure parseLvalue(var memLocReturn: MemLocation); forward;
procedure parseSpecialFunction(sf: SpecialFunc; var returnType: TypeSpec); forward;
procedure parseArrayIndex(var arrayTyp: TypeSpec; var name:IdentString;
    var elType:TypeSpec); forward;
procedure parseStringIndex; forward;
procedure parseTypeSpec(var typSpec: TypeSpec; allowUnresolved:boolean); forward;
procedure parseEnumDecl(var name:IdentString;var typeReturn: TypeSpec); forward;
procedure parseConstValue(constData: ArrayConstRef; var expectedType: TypeSpec); forward;
procedure parseProgramBlock; forward;
procedure parseStatement; forward;
function findProcedure(var name: IdentString; aProc:ProcRef): ProcRef; forward;
procedure parseProcedure; forward;
procedure parseFunction; forward;
procedure parseCharExprTail(var typeA: TypeSpec); forward;
procedure parseStringExprTail(dstType: TypeSpec); forward;
procedure parseSetExprTail(var typeA: TypeSpec); forward;
procedure loadVarParamRef(var loc: MemLocation); forward;
procedure loadAddr(var loc: MemLocation); forward;
procedure allocTemporary(aProc: ProcRef;
	var typ: TypeSpec; var memLocReturn: MemLocation); forward;
procedure disposeWithStmntTmp; forward;
procedure convertToIndirect(var mem: MemLocation); forward;
function matchTokenOrNot(wantedToken: TokenType): boolean; forward;

{$I 'platform+.pas'}
{$I 'float32+.pas'}
{$I 'emit.pas'}

procedure initStringList(var list:StringList);
begin
	with list do
	begin
		head := nil;
		tail := nil;
		current := nil;
	end;
end;

procedure addToStringList(var list:StringList; var name: IdentString);
var itemRef: ^StringListItem;
begin
	new(itemRef);
	itemRef^.value := name;
	itemRef^.next := nil;

	with list do
	begin
		if head = nil then
		begin
			head := itemRef;
			tail := itemRef;
			current := itemRef;
		end
		else
		begin
			head^.next := itemRef;
			head := itemRef;
		end;
	end;
end;

function nextStringListItem(var list:StringList; var returnStr: IdentString): boolean;
begin
	if list.current = nil then
		nextStringListItem := false
	else
	begin
		returnStr := list.current^.value;
		list.current := list.current^.next;
		nextStringListItem := true;
	end;
end;

procedure rewindStringList(var list:StringList);
begin
	list.current := list.tail;
end;

procedure disposeStringList(var list:StringList);
var itemRef, next: ^StringListItem;
begin
	itemRef := list.tail;
	while itemRef <> nil do
	begin
		next := itemRef^.next;
		dispose(itemRef);
		itemRef := next;
	end;
end;

procedure initIntList(var list:IntList);
begin
	with list do
	begin
		head := nil;
		tail := nil;
		current := nil;
	end;
end;

procedure addToIntList(var list:IntList; var anInteger: integer);
var itemRef: ^IntListItem;
begin
	new(itemRef);
	itemRef^.value := anInteger;
	itemRef^.next := nil;

	with list do
	begin
		if head = nil then
		begin
			head := itemRef;
			tail := itemRef;
			current := itemRef;
		end
		else
		begin
			head^.next := itemRef;
			head := itemRef;
		end;
	end;
end;

function nextIntListItem(var list:IntList; var returnValue: integer): boolean;
begin
	if list.current = nil then
		nextIntListItem := false
	else
	begin
		returnValue := list.current^.value;
		list.current := list.current^.next;
		nextIntListItem := true;
	end;
end;

procedure rewindIntList(var list:IntList);
begin
	list.current := list.tail;
end;

procedure disposeIntList(var list:IntList);
var itemRef, next: ^IntListItem;
begin
	itemRef := list.tail;
	while itemRef <> nil do
	begin
		next := itemRef^.next;
		dispose(itemRef);
		itemRef := next;
	end;
end;

function findSymbol(var table: SymbolTable; var name: IdentString): SymblRef;
var current: SymblRef;
begin
	current := table.first;
	while (current <> nil) do
		if (current^.name <> name) then
			current := current^.next
		else
			break;
	findSymbol := current;
end;

function addSymbol(var table: SymbolTable; var name: IdentString; var typ: TypeSpec;
	isParam, isVarParam: boolean): SymblRef;
var current: SymblRef;
    newSymbol: SymblRef;
begin
	checkDuplicateSymbol(name);

	new(newSymbol);
	newSymbol^.name := name;
	newSymbol^.offset := table.offset;
	newSymbol^.next := nil;
	newSymbol^.scope := table.scope;
	newSymbol^.level := table.level;
	newSymbol^.size := typ.size;
	newSymbol^.symType := typ; (* TODO: needs a deep copy for aggregate types *)
	newSymbol^.isParam := isParam;
	newSymbol^.isVarParam := isVarParam;
	newSymbol^.isExternal := false;
	newSymbol^.initialized := (table.scope = GlobalSymbol) or isVarParam or isParam;

	if isVarParam then
		table.offset := table.offset + wordSize
	else
		table.offset := table.offset + typ.size;

	emitNewSymbol(table.scope, name, newSymbol^.offset);

	if table.first = nil then
		table.first := newSymbol
	else
	begin
		current := table.first;
		while current^.next <> nil do current := current^.next;
		current^.next := newSymbol;
	end;

	addSymbol := newSymbol;
end;

(* Create a pseudo symbol for a record field that is accessed inside
	a with statement.
	Because we need to return a pointer to a symbol in findHieraSymbol,
	we have to allocate a TypeSpec record which needs to be freed later
	(in parseWithStatement)
	*)
function createPseudoSym(var name: string; field: FieldRef;
	withSlot: integer): SymblRef;
var sym: SymblRef;
	typ: TypeSpec;
begin
	typ := field^.fieldType;

	new(sym);
	sym^.name := name;
	sym^.symType := typ;
	sym^.scope := WithStmntSymbol;
	sym^.level := 0;
	sym^.size := typ.size;
	sym^.offset := field^.offset;
	sym^.isParam := false;
	sym^.isVarParam := false;
	sym^.isConst := false;
	sym^.isExternal := false;
	sym^.initialized := false;
	sym^.initialValue := 0;
	sym^.hasInitialValue := false;
	sym^.withStmntSlot := withSlot;
	sym^.next := nil;

	createPseudoSym := sym;
end;

function findWithStmntSym(var name: string): SymblRef;
var sym: SymblRef;
	i: integer;
	w: WithStmntAnchor;
	field: ^FieldListItem;
begin
	sym := nil;

	for i := withStmntCount downto 1 do
	begin
		w := withStmntStack[i];
		field := w.recordLoc.typ.fields;
		while field <> nil do
		begin
			if field^.name = name then
			begin
				sym := createPseudoSym(name, field, i);
 				(* If there was a pseudo-symbol allocated earlier,
					free it. The last one is freed in parseWithStatement.
					We can overwrite the previous pointer because it is only
					used from findHieraSymbol until initMemLocation/parseMemlocation.
					The call sequence looks like this:
						- findHieraSymbol  	  -> pseudo-sym allocated
						- initMemLocation  	  -> used
						- read/writeVariable  -> ignored
					
					If we have multiple accesses to the same record within a
					statement, the call sequence is as follows:
						- findHieraSymbol a		-> allocate psym for a
						- initMemLocation a		-> psym-a is used
						- findHieraSymbol b		-> allocate for b, free psym-a
						- initMemLocation b		-> psym-b is used
						- readVariable b		-> memloc-b is used
						- writeVariable a		-> memloc-a is used
					*)
				disposeWithStmntTmp;
				withStmntStack[i].tmpSymbol := sym;
				break;
			end;
			field := field^.next;
		end;

		if sym <> nil then
			break;
	end;

	findWithStmntSym := sym;
end;

function findHieraSymbol(var name: string): SymblRef;
var sym: SymblRef;
	aProc: ProcRef;
begin

	(* TODO: check the with-stack, use WithScope in that case.
		Also add this to initMemLocation/parseMemLocation *)
	sym := nil;
	aProc := curProcedure;

	sym := findWithStmntSym(name);

	(* if not a record field from a with statement, check
		for variable names recursively *)
	if (sym = nil) and (aProc <> nil) then
			(* aProc can be nil during the initialization of the main procedure *)
	repeat
		sym := findSymbol(aProc^.vars, name);
		if sym = nil then
			aProc := aProc^.parent;
	until (sym <> nil) or (aProc = nil);

	findHieraSymbol := sym;
end;

function findConstant(aProc:ProcRef; var name:IdentString): ConstRef;
var current: ConstRef;
begin
	current := aProc^.constants;
	while current <> nil do
	begin
		if current^.name = name then
			break
		else
			current := current^.next;
	end;
	findConstant := current;
end;

function findConstantHiera(var name:IdentString): ConstRef;
var aProc:ProcRef;
begin
	findConstantHiera := nil;
	aProc := curProcedure;
	while aProc <> nil do
	begin
		findConstantHiera := findConstant(aProc, name);
		if findConstantHiera <> nil then
			break
		else
			aProc := aProc^.parent;
	end;
end;

function addConstant(var name:IdentString): ConstRef;
var current,newConst: ConstRef;
begin
	checkDuplicateSymbol(name);

	new(newConst);
	newConst^.name := name;
	newConst^.next := nil;
	newConst^.typ.baseType := NoType;

	if curProcedure^.constants = nil then
		curProcedure^.constants := newConst
	else
	begin
		current := curProcedure^.constants;
		while current^.next <> nil do current := current^.next;
		current^.next := newConst;
	end;
	addConstant := newConst;
end;

function findLabel(var aProc: ProcRef; var name: IdentString): LablRef;
var current: LablRef;
begin
	current := aProc^.labels;
	while (current <> nil) do
	begin
	    if current^.name = name then
			break
		else
			current := current^.next;
	end;
	findLabel := current;
end;

procedure addLabel(var aProc: ProcRef; var name: IdentString);
var current, newLabl: ^Labl;
begin
	checkDuplicateSymbol(name);

	new(newLabl);
	newLabl^.name := name;
	newLabl^.id   := aProc^.id;
	newLabl^.next := nil;
	
	if aProc^.labels = nil then
		aProc^.labels := newLabl
	else
	begin
		current := aProc^.labels;
		while current^.next <> nil do current := current^.next;
		current^.next := newLabl;
	end;
end;

function addArrayConst: ArrayConstRef;
var newArrayConst: ArrayConstRef;
begin
	arrayConstNo := arrayConstNo + 1;

	new(newArrayConst);
	newArrayConst^.firstElement := nil;
	newArrayConst^.next := nil;
	newArrayConst^.id := arrayConstNo;
	newArrayConst^.extraLabel := nil;

	if firstArrayConst = nil then
		firstArrayConst := newArrayConst
	else
		lastArrayConst^.next := newArrayConst;

	lastArrayConst := newArrayConst;

	addArrayConst := newArrayConst;
end;

function addNamedArrayConst(var name:IdentString; var first:boolean): ArrayConstRef;
var constData: ArrayConstRef;
begin
	constData := addArrayConst;
	if first then
	begin
		new(constData^.extraLabel);
		constData^.extraLabel^ := name;
		first := false;
	end;
	addNamedArrayConst := constData;
end;

function addConstElem(arrayConst: ArrayConstRef): OpaqueDataRef;
var newElem,current: ^OpaqueDataElement;
begin
	new(newElem);
	newElem^.next := nil;

	current := arrayConst^.firstElement;
	if current = nil then
		arrayConst^.firstElement := newElem
	else
	begin
		while current^.next <> nil do current := current^.next;
		current^.next := newElem;
	end;

	addConstElem := newElem;
end;

procedure addArrayConstElem(arrayConst: ArrayConstRef;value:integer);
var newElem: ^OpaqueDataElement;
begin
	newElem := addConstElem(arrayConst);
	newElem^.isStringValue := false;
	newElem^.intValue := value;
end;

procedure addStrConstElem(arrayConst: ArrayConstRef; var aString:KeywordString;
	maxLength:integer);
var newElem: ^OpaqueDataElement;
begin
	if length(aString) > maxLength then
		errorExit2('String constant length exceeds declared length','');

	newElem := addConstElem(arrayConst);
	newElem^.isStringValue := true;
	new(newElem^.strValue);
	newElem^.strValue^ := aString;
	newElem^.maxLength := maxLength;
end;

function findConstStr(var value: string):ConstStrRef;
var current: ConstStrRef;
begin
	current := firstConstStr;
	while (current <> nil) and (current^.value <> value) do current := current^.next;
	findConstStr := current;
end;

function addConstStrRaw(var value: string): ConstStrRef;
var newstring: ConstStrRef;
begin
	new(newstring);
	newstring^.value := value;
	newstring^.next := nil;
	newstring^.no := constStrNo;
	newstring^.length := 0;
	newstring^.extraLabel := nil;

	constStrNo := constStrNo + 1;

	if lastConstStr = nil then
	begin
		firstConstStr := newstring;
		lastConstStr  := newstring;
	end
	else
	begin
		lastConstStr^.next := newstring;
		lastConstStr := newstring;
	end;
	addConstStrRaw := newstring;
end;

(* create a string constant/literal and return a pointer to it.
	if a constant with the same value already exists, it is reused.
	*)
function addConstStr(var value: string): ConstStrRef;
var newstring: ConstStrRef;
begin
	newstring := findConstStr(value);
	if newstring = nil then
	begin
		newstring := addConstStrRaw(value);
	end;
	addConstStr := newstring;
end;

procedure nextAnonTypeName(var name:IdentString);
var buf:string[16];
begin
	str(anonTypeCount, buf);
	name := '_anon' + buf;
	anonTypeCount := anonTypeCount + 1;
end;

procedure dumpTypes; forward;

procedure addType(var newType: TypeSpec; var name:IdentString);
var curItem: TypeRef;
	newTypeSpec: ^TypeSpec;
	newItem: TypeRef;
begin
	checkDuplicateSymbol(name);

	curItem := curProcedure^.types;

	new(newTypeSpec);
	newTypeSpec^ := newType;

	new(newItem);
	newItem^.name := name;
	newItem^.typePtr  := newTypeSpec;
	newItem^.next := nil;

	(* if list is empty, set first item *)
	if curItem = nil then
	begin
		curProcedure^.types := newItem;
	end
	else
	begin
		(* find the end of the list *)
		while curItem^.next <> nil do
			curItem := curItem^.next;
		curItem^.next := newItem;
	end;
	{ dumpTypes; }
end;

function findTypeRef(aProc: ProcRef; var name: IdentString): TypeRef;
var curItem: TypeRef;
begin
	findTypeRef := nil;
	curItem := aProc^.types;
	while curItem <> nil do
	begin
		if curItem^.name = name then
		begin
			findTypeRef := curItem;
			break;
		end;
		curItem := curItem^.next;
	end;
end;

function findTypeRefHiera(var name: IdentString): TypeRef;
var aProc:ProcRef;
begin
	findTypeRefHiera := nil;
	aProc := curProcedure;
	while aProc <> nil do
	begin
		findTypeRefHiera := findTypeRef(aProc, name);
		if findTypeRefHiera <> nil then
			break
		else
			aProc := aProc^.parent;
	end;
end;

(* TODO: use findTypeRef *)
function findType(aProc:ProcRef; var name: IdentString): TypeSpec;
var curItem: TypeRef;
begin
	findType.baseType := NoType;
	curItem := aProc^.types;
	while curItem <> nil do
	begin
		{ writeln('***** findType searching ', name, ' ', curItem^.name,
			' ', curItem^.typePtr^.baseType); }
		if curItem^.name = name then
		begin
			findType := curItem^.typePtr^;
			{ writeln('***** findType found ', curItem^.name); }
			break;
		end;
		curItem := curItem^.next;
	end;
end;

(* TODO: use findTypeRefHiera *)
function findTypeHiera(var name: IdentString): TypeSpec;
var aProc: ProcRef;
begin
	findTypeHiera.baseType := NoType;
	aProc := curProcedure;
	while aProc <> nil do
	begin
		findTypeHiera := findType(aProc, name);
		if findTypeHiera.baseType <> NoType then
			break
		else
			aProc := aProc^.parent;
	end;
end;

function findEnumById(enumId: integer):TypeRef;
var aProc:ProcRef;
	curItem: TypeRef;
begin
	findEnumById := nil;
	aProc := curProcedure;
	while (aProc <> nil) and (findEnumById = nil) do
	begin
		curItem := aProc^.types;
		while curItem <> nil do
		begin
			if (curItem^.typePtr^.baseType = EnumType)
				and (curItem^.typePtr^.enumId = enumId) then
			begin
				findEnumById := curItem;
				break;
			end;
			curItem := curItem^.next;
		end;
		aProc := aProc^.parent;
	end;
end;

procedure dumpTypes;
var curType: TypeRef;
begin
	curType := curProcedure^.types;
	while curType <> nil do
	begin
		writeln(curType^.name:20,' ', curType^.typePtr^.baseType, ' ',
			curType^.typePtr^.size);
		curType := curType^.next;
	end;
end;

procedure checkDuplicateVar(var name:IdentString);
var typ:TypeSpec;
begin
	if curProcedure <> nil then
		if findSymbol(curProcedure^.vars, name) <> nil then
			errorExit2('duplicate identifier', name);

	typ := findTypeHiera(name);
	if typ.baseType <> NoType then
		errorExit2('duplicate identifier (type)', name);

	if findConstantHiera(name) <> nil then
		errorExit2('duplicate identifier (constant)', name);
end;

procedure checkDuplicateSymbol(var name:IdentString);
begin
	checkDuplicateVar(name);
	(* FIXME: this should most likely be searchProcedure, not findProcedure*)
	if findProcedure(name, curProcedure) <> nil then
		errorExit2('duplicate identifier (procedure/function)', name);
end;

procedure setBaseType(var typ: TypeSpec; baseTyp: SymbolType);
begin
	typ.size := wordSize;
	typ.baseType := baseTyp;
	typ.hasSubrange := false;
end;

procedure setSubrange(var typ: TypeSpec; rStart,rEnd: integer);
begin
	typ.hasSubrange := true;
	typ.subStart    := rStart;
	typ.subEnd      := rEnd;
end;

(* TODO: wrong name, Pointer is not a scalar *)
function isScalar(var typ: TypeSpec): boolean;
begin
	isScalar := typ.baseType in [ IntegerType, BooleanType, RealType, CharType, PointerType,
		EnumType ];
end;

function isAggregate(var typ:TypeSpec): boolean;
begin
	isAggregate := typ.baseType in [ ArrayType, RecordType, StringType ];
end;

(* check if type is a single value (used only for constant declaration?)*)
function isSimpleType(var typ: TypeSpec): boolean;
begin
	isSimpleType := typ.baseType in [ IntegerType, BooleanType, RealType, CharType,	EnumType ];
end;

function isDirectType(var typ: TypeSpec): boolean;
begin
	isDirectType := typ.baseType in [ IntegerType, BooleanType, RealType, CharType,	EnumType,
		SetType, PointerType ];
end;

(* check if valid type for array indexing *)
function isIndexType(var typ: TypeSpec): boolean;
begin
	isIndexType := typ.baseType in [ IntegerType, BooleanType, CharType, EnumType ];
end;

procedure setStringTypeSize(var typeReturn:TypeSpec; length:integer);
begin
	setBaseType(typeReturn, StringType);
	typeReturn.size := StringHeaderSize + ((length div wordSize) + 1) * wordSize;
	typeReturn.stringLength := length;
end;

procedure convertStringToChar(var typeReturn:TypeSpec);
begin
	setBaseType(typeReturn, CharType);
	emitConvStringToChar;
end;

procedure convertCharToString(var typeReturn:TypeSpec);
var temp: MemLocation;
begin
	setBaseType(typeReturn, StringType);
	setStringTypeSize(typeReturn,1);
	allocTemporary(curProcedure, typeReturn, temp);
	loadAddr(temp);
	emitConvCharToString;
end;

procedure convertIntToReal(var typeReturn:TypeSpec);
begin
	setBaseType(typeReturn, RealType);
	emitIntToFloat();
end;

procedure convertPrevIntToReal(var typeReturn:TypeSpec);
begin
	setBaseType(typeReturn, RealType);
	emitSwap;
	emitIntToFloat();
	emitSwap;
end;

function getTypeName(t: SymbolType):TypeTagString;
begin
	getTypeName := typeNames[t];
end;

procedure matchBaseType(var typ: TypeSpec; wantedBaseType: SymbolType);
begin
	if typ.baseType <> wantedBaseType then
		errorExit1('Error: Expected type ' + getTypeName(wantedBaseType) +
			', got ' + getTypeName(typ.baseType));
end;

procedure matchBaseTypes(var typeA, typeB: TypeSpec; wantedBaseType: SymbolType);
begin
	matchBaseType(typeA, wantedBaseType);
	matchBaseType(typeB, wantedBaseType);
end;

procedure matchLogicOpTypes(var typeA, typeB:TypeSpec);
begin
	if (typeA.baseType <> typeB.baseType) or
		((typeA.baseType <> IntegerType) and
		(typeA.baseType <> BooleanType)) then
		errorExit2('Either two boolean or two integer operands expected', '');
end;

procedure matchSymbolType(actualType: TypeSpec; sym: SymblRef);
begin
	(* TODO: match complex types *)
	if sym^.symType.baseType <> actualType.baseType then
		errorExit1('Error: expected type ' + getTypeName(sym^.symType.baseType) +
			' for ' + sym^.name + ', got ' + getTypeName(actualType.baseType));
end;

(* check if two types are the same.
	Arrays must have same length and element type.
	Records must be the same type alias (that is, 
		having the same field types is not enough)
	
	This is like matchTypes but it returns a value
	instead of throwing errors.
*)
function isSameType(var typeA, typeB: TypeSpec):boolean;
begin
	isSameType := false;

	if typeA.baseType = typeB.baseType then
	begin
		isSameType := true; (* if the base types match, we set the return
								value to true here, with more checks below *)
		if typeA.baseType = EnumType then
			isSameType := typeA.enumId = typeB.enumId
		else
		if typeA.baseType = ArrayType then
			isSameType := isSameType(typeA.elementType^, typeB.elementType^) and
			 (typeA.arrayLength = typeB.arrayLength)
		else if typeA.baseType = RecordType then
			(* the pointer to the first record field works as an unique
				identifier, because the field items are allocated
				exactly once when compiling and are never deallocated.*)
			isSameType := typeA.fields = typeB.fields
		else if typeA.baseType = PointerType then
		begin
			(* pointedType is nil for the nil pointer value
				which is compatible with all pointer types *)
			if (typeB.pointedType <> nil) and (typeA.pointedType <> nil) then
				isSameType := isSameType(typeA.pointedType^, typeB.pointedType^);
		end;
	end;
end;

procedure matchTypes(var typeA, typeB: TypeSpec);
begin
	matchBaseType(typeA, typeB.baseType);
	if typeA.baseType = EnumType then
	begin
		if typeA.enumId <> typeB.enumId then
			errorExit2('Incompatible enum types for', lastToken.tokenText);
	end
	else
	if typeA.baseType = ArrayType then
	begin
		matchTypes(typeA.elementType^, typeB.elementType^);
		if typeA.arrayLength <> typeB.arrayLength then
			errorExit2('Incompatible arrays', '');
	end
	else if typeA.baseType = RecordType then
	begin
		(* the pointer to the first record field works as an unique
			identifier, because the field items are allocated
			exactly once when compiling and are never deallocated.*)
		if typeA.fields <> typeB.fields then
			errorExit2('Incompatible record types', '');
	end
	else if typeA.baseType = PointerType then
	begin
		(* pointedType is nil for the nil pointer value
			which is compatible with all pointer types *)
		if (typeB.pointedType <> nil) and (typeA.pointedType <> nil) then
			matchTypes(typeA.pointedType^, typeB.pointedType^);
	end;
end;

(* checks if a type is compatible with real.
   accepts real or integer. integer will be converted
   to real. value must be already on stack. *)
procedure matchRealType(var typeB: TypeSpec);
begin
	if typeB.baseType = IntegerType then
		convertIntToReal(typeB)
	else
		matchBaseType(typeB, RealType);
end;

(* Match argument types for an arithmetic operation of reals.
   One of both args can be an integer and will be converted.
   Both args must already be on the stack.
   *)
procedure matchRealCompatibleArgs(var typeA, typeB: TypeSpec);
begin
	if typeA.baseType = IntegerType then
		convertPrevIntToReal(typeA);
	if typeB.baseType = IntegerType then
		convertIntToReal(typeB);
	matchBaseType(typeA, RealType);
	matchBaseType(typeB, RealType);
end;

procedure matchComparableTypes(var typeA, typeB: TypeSpec);
begin
	if (typeA.baseType = RealType) and (typeB.baseType = IntegerType) then
	begin
		(* writeln('**** real/integer comparison'); *)
	end
	else if (typeA.baseType = IntegerType) and (typeB.baseType = RealType) then
	begin
		(* writeln('**** integer/real comparison'); *)
	end
	else if (typeA.baseType <> typeB.baseType) then
	begin
		if (typeA.baseType = CharType) and (typeB.baseType = StringType) then
			convertStringToChar(typeB)
		else
		if (typeA.baseType = StringType) and (typeB.baseType = CharType) then
			convertCharToString(typeB)
		else
		begin
			errorExit1('types ' + getTypeName(typeA.baseType) + ' and '+
				getTypeName(typeB.baseType) + ' are not comparable');
		end;
	end
	else
	begin
		if not (isScalar(typeA) or (typeA.baseType = StringType)) then
				matchTypes(typeA, typeB);
		(* FIXME: what happens when the if condition is not met? *)
	end;
end;

(* match types and in some cases, try to convert non-matching types.
   typeB is converted to typeA, if needed. A conversion is performed
   on the topmost stack element (if any).
   currently implemented: string -> char, char -> string, integer -> real
   *)
procedure matchAndConvertTypes(var typeA, typeB: TypeSpec);
begin
	if (typeA.baseType = CharType) and (typeB.baseType = StringType) then
		convertStringToChar(typeB)
	else
	if (typeA.baseType = StringType) and (typeB.baseType = CharType) then
		convertCharToString(typeB)
	else
	if (typeA.baseType = StringCharType) and (typeB.baseType = CharType) then
		(* StringCharType and CharType are compatible and conversion will be
			handled by parseMemLocation and readVariable/writeVariable*)
	else
	if (typeA.baseType = RealType) and (typeB.baseType = IntegerType) then
		convertIntToReal(typeB)
	else
		matchTypes(typeB, typeA); (* reverse order to get correct error message *)
end;

procedure setIndirect(var mem: MemLocation);
begin
	if mem.memLoc <> Indirect then
	begin
		mem.memLoc := Indirect;
		(* mem.name := mem.name + '<i>'; *)
		mem.offset := -1;
	end;
end;

procedure initNoMemLocation(var loc: MemLocation);
begin
	loc.memLoc := NoMem;
	loc.offset := -1;
	loc.name := 'NoMem';
	loc.typ.baseType := NoType;
end;

(* initialize a MemLocation object from a Symbol. *)
procedure initMemLocation(sym: SymblRef; var loc: MemLocation);
begin
	loc.memLoc := NoMem;
	loc.name := sym^.name;
	loc.offset := 0;
	loc.typ := sym^.symType;
	loc.initialized := sym^.initialized;
	loc.origSym := sym;

	if sym^.isVarParam then
	begin
		(* is it a var parameter from an outer scope? *)
		if sym^.level < curProcedure^.level then
		begin
			loc.memLoc := NestedMem;
			loc.offset := sym^.offset;
			loc.scopeDistance := curProcedure^.level - sym^.level;
		end
		else
		begin
			loc.memLoc := LocalMem;
			loc.offset := sym^.offset;
		end;
		loadVarParamRef(loc);
		setIndirect(loc);
		(* for var parameters the local variable slot contains
						the address of the value
			FIXME: 		loadVarParamRef should not be done here
				- why not?
				- because it might be possible we want to initialize
					the MemLocation record without emitting code
				- but we do emit code to start the address calculation
				  for other cases, too, see below *)
	end
	else
	if sym^.scope = GlobalSymbol then
	begin
		loc.memLoc := GlobalMem;
		(* nothing to do, name and offset are already set *)
	end
	else if sym^.scope in [ LocalSymbol, ParameterSymbol ] then              
	begin
		(* is it a variable from an outer scope? *)
		if sym^.level < curProcedure^.level then
		begin
			loc.memLoc := NestedMem;
			loc.offset := sym^.offset;
			loc.scopeDistance := curProcedure^.level - sym^.level;
			emitNestedMemLoc(loc);
			if isNestedIndirect(loc) then
				setIndirect(loc);
		end
		else
		begin
			loc.memLoc := LocalMem;
			loc.offset := sym^.offset;
			emitLocalMemLoc(loc);
			if isLocalIndirect(loc) then
				setIndirect(loc);
		end;
	end
	else if sym^.scope = WithStmntSymbol then
	begin
		loc.memLoc := Indirect;
		loc.offset := sym^.offset;
		emitWithStmntMemLoc(loc, sym^.withStmntSlot);
	end
	else
		errorExit2('Internal error in initMemLocation', sym^.name);
end;

function findField(var typ:TypeSpec; var name:IdentString): FieldRef;
var curField: FieldRef;
begin
	if typ.baseType <> RecordType then
		errorExit2('Invalid record field access:', name);
	findField := nil;
	curField := typ.fields;
	while curField <> nil do
	begin
		if curField^.name = name then
		begin
			findField := curField;
			break;
		end;
		curField := curField^.next;
	end;
end;

function findSpecialProcedure(var name: IdentString): SpecialProc;
var i: SpecialProc;
begin
	findSpecialProcedure := NoSP;

	for i := NoSP to ExitSP do
		if name = specialprocnames[i] then
		begin
			findSpecialProcedure := i;
			break;
		end;
end;

function findSpecialFunction(var name: IdentString): SpecialFunc;
var i: SpecialFunc;
begin
	findSpecialFunction := NoSF;

	for i := NoSF to AbsSF do
		if name = specialfuncnames[i] then
		begin
			findSpecialFunction := i;
			break;
		end;
end;

function findProcedure(var name: IdentString; aProc:ProcRef): ProcRef;
var current: ProcRef;
begin
	if aProc <> nil then
	begin
		current := aProc^.procedures;
		while (current <> nil) do
			if (current^.name <> name) then
				current := current^.next
			else
				break;
		findProcedure := current;
	end
	else
		findProcedure := nil;
end;

(* do a nested search for a procedure, i.e. search in procedures
   at the current scope, then continue search at all outer scopes if not found. *)
function searchProcedure(var name: IdentString): ProcRef;
var parent: ProcRef;
begin
	parent := curProcedure;
	repeat
		searchProcedure := findProcedure(name, parent);
		if searchProcedure = nil then parent := parent^.parent;
	until (parent = nil) or (searchProcedure <> nil);
end;

function createProcedure(var name:IdentString; parent:ProcRef):ProcRef;
var newProc, current:ProcRef;
begin
	new(newProc);
	newProc^.name := name;
	newProc^.next := nil;
	newProc^.tempsSize := 0;
	newProc^.returnsAggregate := false;
	newProc^.parent := parent;
	newProc^.level := 0;
	newProc^.isForward := false;
	newProc^.isNested := false;
	newProc^.hasNested := false;
	newProc^.procedures := nil;
	newProc^.labels := nil;
	newProc^.types := nil;
	newProc^.unresolved := nil;
	newProc^.constants := nil;
	newProc^.hasExit := false;
	newProc^.estackCleanup := 0;
	nestedProcsCount := nestedProcsCount + 1;
	newProc^.id := nestedProcsCount;
	setBaseType(newProc^.returnType, NoType);

	if parent <> nil then
	begin
		newProc^.level := parent^.level + 1;
		if newProc^.level > 0 then newProc^.isNested := true;

		if (parent^.procedures) = nil then
			parent^.procedures := newProc
		else
		begin
			current := parent^.procedures;
			while current^.next <> nil do current := current^.next;
			current^.next := newProc;
		end;
	end;

	newProc^.vars.first := nil;
	newProc^.vars.offset := 0;
	newProc^.vars.scope := LocalSymbol;
	newProc^.vars.level := newProc^.level;
	newProc^.parameters.first := nil;
	newProc^.parameters.offset := 0;
	newProc^.parameters.level := newProc^.level;

	{
	if parent <> nil then
		writeln('***** createProcedure ', newProc^.name, ' parent:', newProc^.parent^.name,
			' level:', newProc^.level);
	}

	(* do some preparations for the stack frame,
		e.g. allocate space for the link to
		outer stack frames for nested procedures *)
	cpuAllocStackFrame(newProc);

	createProcedure := newProc;
end;

function addProcedure(name: IdentString; hasReturnValue: boolean; parent:ProcRef): ProcRef;
var fwdDecl:ProcRef;
begin	
	checkDuplicateVar(name);

	fwdDecl := findProcedure(name, parent);
	if fwdDecl <> nil then
	begin
		if not fwdDecl^.isForward then
			errorExit2('duplicate identifier (procedure/function)', name);
		addProcedure := fwdDecl;
	end
	else
		addProcedure := createProcedure(name, parent);
end;

procedure addParam(aProc: ProcRef; name: IdentString; typSpec: TypeSpec; isVarParam: boolean);
var pSym, vSym: SymblRef;
begin
	(* parameters are added to both the parameter list and the
	   list of local variables *)
	pSym := addSymbol(aProc^.parameters, name, typSpec, true, isVarParam);
	vSym := addSymbol(aProc^.vars, name, typSpec, true, isVarParam);
	pSym^.scope := ParameterSymbol;
	vSym^.scope := ParameterSymbol;
	pSym^.isParam := true;
	vSym^.isParam := true;
end;

procedure printLineStats;
begin
	write(#13);
	ClrEol;
	writeln(filename:16, lineno - 1:8, ' lines.');
end;

procedure beginInclude(var newname: string);
var newfile: InputFileType;
    p:integer;
begin
	if includeLevel = MaxIncludes then
		errorExit2('Too many nested includes', '');
	
	includeLevel := includeLevel + 1;

	prevFiles[includeLevel].filevar := infile;
	prevFiles[includeLevel].name := filename;
	prevFiles[includeLevel].line := lineno;

	p := pos(PlatformMagic, newname);
	if p > 0 then
		      insert(PlatformTag, newname, p + 1);

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

	printLineStats;

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
	if ch = #10 then
	begin
		lineno := lineno + 1;
		if (lineno and ProgressSteps) = 0 then
		begin
			write(#13, filename, ' ', lineno);
			ClrEol;
		end;
	end;
	nextChar := ch;
end;

procedure skipChar;
var ch:char;
begin
	ch := nextChar;
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
begin
	while peekChar() in [ #10, #13, #32, #9, #12 ] do
		skipChar;
end;

function findToken(var keyword: string): TokenType;
var	i: TokenType;
begin
	findToken := UnknownToken;

	for i := StringToken to UnknownToken do
	begin
		if keywords[i] = keyword then
		begin
			findToken := i;
			break;
		end
	end
end;

(* Convert hexadecimal digits to integer like val().
   digits may or may not start with a '$' character. *)
procedure hexVal(var digits:string; var retval:integer; var error:integer);
var i,v,len:integer;
    c:char;
begin
	len := length(digits);

	if (len > 0) and (digits[1] = '$') then
		i := 2
	else
		i := 1;
	retval := 0;
	error := 0;

	while (i <= len) and (error = 0) do
	begin
		retval := retval shl 4;
		c := digits[i];
		if (c >= 'A') and (c <= 'F') then
			v := ord(c) - ord('A') + 10
		else
		if (c >= '0') and (c <= '9') then
			v := ord(c) - ord('0')
		else
			error := i;
		retval := retval + v;
		i := i + 1;
	end;
end;

function integerFromString(digits:KeywordString):integer;
var value,error:integer;
begin
	if (length(digits) > 0) and (digits[1] = '$') then
		hexVal(digits, value, error)
	else
		val(digits, value, error);
	if error <> 0 then
		errorExit2('invalid integer value', digits);
	integerFromString := value;
end;

function getInteger:integer;
var curChar: char;
    digits: string[24];
begin
	if peekChar = '-' then
	begin
		curChar := nextChar;
		digits := '-';
	end
	else
		digits := '';

	while peekChar in ['0'..'9'] do
	begin
		curChar := nextChar;
		if (length(digits) < MaxIntegerDigits) and (curChar <> #0) then
			digits := digits + curChar;
	end;

	getInteger := integerFromString(digits);
end;

function realFromString(var digits:KeywordString): real;
var code: Integer;
    v: real;
begin
	val(digits, v, code);
	if code <> 0 then errorExit2('invalid real value', digits);
	realFromString := v;
end;

function getCharLitValue(tokenText:string):integer;
begin
	(* is is a one-character-string-literal like 'A' ? *)
	if length(tokenText) = 1 then
		getCharLitValue := ord(tokenText[1])
	else
		errorExit2('cannot use string as char here', tokenText);
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

(* Scan for an integer number in hexadecimal format.
   The hex marker '$' is already in curChar.
   Digits are written to keyword. *)
procedure getHexDigits(curChar: char; var keyword: KeywordString);
begin
	keyword := keyword + curChar;
	while upcase(peekChar) in [ '0'..'9', 'A'..'F' ] do
	begin
		keyword := keyword + upcase(nextChar);
	end;
end;

(* Scan for an integer or real number. All digits up to the first non-digit are
   already in curToken.tokenText.
   Returns all digits/characters in digits and either
   IntegerType or RealType in typeReturn *)
procedure getNumber(var digits:IdentString; var typeReturn:TypeSpec);
begin
	digits := curToken.tokenText;

	if checkToken(MinusToken) then
	begin
		readNextToken;
		digits := digits + curToken.tokenText;
	end;

	if not (peekChar in [ '.', 'E', 'e' ]) then
	begin
		setBaseType(typeReturn, IntegerType);
	end
	else
	begin
		if peekChar = '.' then (* is there a decimal point? *)
		begin
			digits := digits + nextChar;
			if peekChar in [ '0'..'9'] then  (* is there a fraction after the decimal point ? *)
			begin
				getDigits(nextChar, digits);
			end;
		end;
		if peekChar in ['E','e'] then	(* is there an exponent? *)
		begin
			digits := digits + nextChar;
			
			if peekChar in ['+', '-'] then (* exponent can have a sign *)
			begin
				digits := digits + nextChar;
			end;

			if peekChar in ['0'..'9'] then (* now we require some exponent digits *)
			begin
				getDigits(nextChar, digits);
			end
			else
				errorExit2('invalid number format', digits);
		end;
		setBaseType(typeReturn, RealType);
	end;
	readNextToken;
end;

(* parse an integer or real number. all digits up to the first non-digit are
   already in curToken.tokenText. leaves the number on the stack. *)
(* FIXME: use getNumber *)
procedure parseNumber(last:TokenType;var typeReturn: TypeSpec);
var digits: KeywordString;
    value: integer;
	r:     real;
begin
	if last = MinusToken then
		digits := '-' + curToken.tokenText
	else
		digits := curToken.tokenText;

	if not (peekChar in [ '.', 'E', 'e' ]) then
	begin
		value := integerFromString(digits);
		emitLoadConstantInt(value);
		setBaseType(typeReturn, IntegerType);
	end
	else
	begin
		if peekChar = '.' then (* is there a decimal point?*)
		begin
			digits := digits + nextChar;
			if peekChar in [ '0'..'9'] then  (* is there a fraction after the decimal point ? *)
			begin
				getDigits(nextChar, digits);
			end;
		end;
		if peekChar in ['E','e'] then	(* is there an exponent? *)
		begin
			digits := digits + nextChar;
			
			if peekChar in ['+', '-'] then (* exponent can have a sign *)
			begin
				digits := digits + nextChar;
			end;

			if peekChar in ['0'..'9'] then (* now we require some exponent digits *)
			begin
				getDigits(nextChar, digits);
			end
			else
				errorExit2('invalid number format', digits);
		end;
		r := realFromString(digits);
		emitLoadConstantReal(r);
		setBaseType(typeReturn, RealType);
	end;
	readNextToken;
end;

procedure getToken(var tokenReturn:Token;stringTokens:boolean);
var curChar,pkChar: char;
    keyword: KeywordString;
	startLine: string[12];
begin
	curChar := nextChar;
	
	tokenReturn.tokenText := curChar;

	if curChar = #0 then
		tokenReturn.tokenKind := EOFToken
	else
	if curChar = '+' then
		tokenReturn.tokenKind := PlusToken
	else
	if curChar = '-' then
		tokenReturn.tokenKind := MinusToken
	else
	if curChar = '*' then
	begin
		pkChar := peekChar;
		if pkChar = ')' then
		begin
			skipChar;
			tokenReturn.tokenText := tokenReturn.tokenText + pkChar;
			tokenReturn.tokenKind := CommentAltEndToken;
		end
		else
			tokenReturn.tokenKind := AsteriskToken;
	end
	else
	if curChar = '/' then
		tokenReturn.tokenKind := SlashToken
	else
	if curChar = '(' then
	begin
		pkChar := peekChar;
		if pkChar = '*' then
		begin
			skipChar;
			tokenReturn.tokenText := tokenReturn.tokenText + pkChar;
			tokenReturn.tokenKind := CommentAltStartToken;
		end
		else
			tokenReturn.tokenKind := LParenToken;
	end
	else
	if curChar = ')' then
		tokenReturn.tokenKind := RParenToken
	else
	if curChar = '{' then
		tokenReturn.tokenKind := CommentStartToken
	else
	if curChar = '}' then
		tokenReturn.tokenKind := CommentEndToken
	else
	if curChar = '[' then
		tokenReturn.tokenKind := LBracketToken
	else
	if curChar = ']' then
		tokenReturn.tokenKind := RBracketToken
	else
	if curChar = ',' then
		tokenReturn.tokenKind := CommaToken
	else
	if curChar = '=' then
	begin
		pkChar := peekChar;
		if pkChar = '=' then
		begin
			skipChar;
			tokenReturn.tokenText := tokenReturn.tokenText + pkChar;
			tokenReturn.tokenKind := EqEqToken;
		end
		else
			tokenReturn.tokenKind := EqToken;
	end
	else
	if curChar = '>' then
	begin
		pkChar := peekChar;
		if pkChar = '=' then
		begin
			skipChar;
			tokenReturn.tokenText := tokenReturn.tokenText + pkChar;
			tokenReturn.tokenKind := GtEqToken;
		end
		else
			tokenReturn.tokenKind := GtToken;
	end
	else
	if curChar = '<' then
	begin
		pkChar := peekChar;
		if pkChar = '=' then
		begin
			skipChar;
			tokenReturn.tokenText := tokenReturn.tokenText + pkChar;
			tokenReturn.tokenKind := LtEqToken;
		end
		else
		if pkChar = '>' then
		begin
			skipChar;
			tokenReturn.tokenText := tokenReturn.tokenText + pkChar;
			tokenReturn.tokenKind := NotEqToken;
		end
		else
			tokenReturn.tokenKind := LtToken;
	end
	else
	if curChar = '.' then
	begin
		tokenReturn.tokenKind := DotToken;
	end
	else
	if curChar = '^' then
	begin
		tokenReturn.tokenKind := PointerToken;
	end
	else
	if curChar = ';' then
	begin
		tokenReturn.tokenKind := SemicolonToken;
	end
	else
	if curChar = ':' then
	begin
		pkChar := peekChar;
		if pkChar = '=' then
		begin
			skipChar;
			tokenReturn.tokenText := tokenReturn.tokenText + pkChar;
			tokenReturn.tokenKind := AssignmentToken;
		end
		else
			tokenReturn.tokenKind := ColonToken;
	end
	else
	if curChar in ['A'..'Z', 'a'..'z' ] then
	begin
		keyword := Upcase(curChar);
		while peekChar in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do
		begin
			curChar := Upcase(nextChar);
			if (length(keyword) < 80) and (curChar <> #0) then keyword := keyword + curChar;
		end;
		tokenReturn.tokenText := keyword;
		tokenReturn.tokenKind := findToken(keyword);
		if tokenReturn.tokenKind = UnknownToken then tokenReturn.tokenKind := IdentToken;
	end
	else
	if curChar in ['0'..'9' ] then
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
		str(lineno, startLine);
		keyword := '';
		curChar := nextChar;
		(* add characters as long as the current char is not '
		  (or if it is a double ') and not EOF *)
		while (not ((curChar = '''') and (peekChar <> ''''))) and (curChar <> #0 ) do
		begin
			if (curChar = '''') and (peekChar = '''') then
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
	if curChar = '#' then
	begin
		tokenReturn.tokenText := chr(getInteger);
		tokenReturn.tokenKind := CharLitToken;
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

(* Parse a compiler directive which is inside a comment.
   The start token of the comment has already been parsed. *)
procedure parseDirective(closingToken:TokenType);
var ch:char;
    filename:string;
begin
	ch := nextChar; (* skip $ character *)
	ch := nextChar; (* this is our directive *)
	if ch = 'I' then
	begin
		if peekChar = ' ' then
		begin
			readNextToken;
			(* we require the include filename to be enclosed
				in single quotes for simplicity *)
			if curToken.tokenKind <> StringLitToken then
				errorExit2('Include filename must be enclosed in single quotes','');
			filename := curToken.tokenText;
			readNextToken;
			if curToken.tokenKind <> closingToken then
				errorExit2('Invalid directive', '');

			beginInclude(filename);
			matchToken(closingToken);
		end
	end
	else
	if ch = 'H' then
	begin
		if (peekChar = ' ') or isDigit(peekChar) then
		begin
			readNextToken;
			defaultHeapSize := integerFromString(curToken.tokenText) * 1024;
			readNextToken;
		end;
		matchToken(closingToken);
	end
	else
	if ch = 'S' then
	begin
		if (peekChar = ' ') or isDigit(peekChar) then
		begin
			readNextToken;
			defaultStackSize := integerFromString(curToken.tokenText) * 1024;
			readNextToken;
		end;
		matchToken(closingToken);
	end
	else
	if ch = '!' then
	(* special comment till end of line *)
	begin
		while not (nextChar = #13) do (* nothing *);
		readNextToken;
	end
	else
	begin
		(* no directive recognized, treat as comment *)
		while not matchTokenOrNot(closingToken) do
		begin
			skipWhitespace;
			skipToNextToken;
		end;
	end;
end;

(*  This will skip a comment, works with both comment styles depending
	on closingToken. Also processes compiler directives. *)
procedure skipComment(closingToken: TokenType);
var startLine:string[8];
	done:boolean;
begin
	if peekChar = '$' then
		parseDirective(closingToken)
	else
	begin
		str(lineno,startLine);

		if closingToken = CommentEndToken then
		while nextChar <> '}' do
		begin
			if eof(infile) then
				errorExit2('runaway comment starting at line', startLine);
		end
		else
		if closingToken = CommentAltEndToken then
		begin
			done := false;
			repeat
				if eof(infile) then
					errorExit2('runaway comment starting at line', startLine);
				(* we cannot use getToken because it would not work with
					string literals or numbers inside comments *)
				if nextChar = '*' then
					if peekChar = ')' then
						done := nextChar = ')';
			until done;
		end;

		skipWhitespace;
		skipToNextToken;
	end;
end;

(* read the next token into the global variable curToken.
   skips whitespace and comments.
*)
procedure readNextToken;
begin
	skipWhitespace;

	lastToken := curToken;
	getToken(nextToken, true);
	curToken := nextToken;

	while curToken.tokenKind in [ CommentStartToken, CommentAltStartToken ] do
	begin
		if checkToken(CommentAltStartToken) then
		skipComment(CommentAltEndToken)
		else
		if checkToken(CommentStartToken) then
			skipComment(CommentEndToken)
	end;
end;

function checkComparisonOperator(aTokenType: TokenType): boolean;
begin
	checkComparisonOperator := aTokenType in
		[ LtToken, LtEqToken, EqToken, NotEqToken, GtEqToken, GtToken ];
end;

function getCompareOpFromToken(tok: TokenType): CompOpString;
begin
	if not checkComparisonOperator(tok) then
		errorExit2('invalid comparison operator token', '');
		case tok of
			LtToken:	getCompareOpFromToken := 'LT';
			LtEqToken:  getCompareOpFromToken := 'LE';
			EqToken:    getCompareOpFromToken := 'EQ';
			NotEqToken: getCompareOpFromToken := 'NE';
			GtEqToken:  getCompareOpFromToken := 'GE';
			GtToken:    getCompareOpFromToken := 'GT';
		end;
end;

procedure cleanup;
begin
	close(infile);
	close(outfile);
end;

procedure errorExit;
begin
	cleanup;
	halt;
end;

procedure errorLine(line:integer);
begin
	writeln('at line ',lineno, ' in ', filename);
end;

procedure errorExit2(message1, message2: string);
var errormsg:string[128];
begin
	errormsg := message1 + ' ' + message2;
	write(#13); ClrEol;
	writeln('Error: ', errormsg);
	errorLine(lineno);
	if editOnError then
	begin
		cleanup;
		ExecEditor(filename, lineno, errormsg)
	end;
	errorExit;
end;

procedure errorExit1(message1:string);
begin
	errorExit2(message1, '');
end;

function quoteToken(var s:string):string;
begin
	if length(s) = 1 then
		quoteToken := '''' + s + ''''
	else
		quoteToken := s;
end;

(* match (and consume) a token or exit with error *)
procedure matchToken(kind: TokenType);
var errormsg:string[128];
begin
	if curToken.tokenKind <> kind then
	begin
		errormsg := 'Expected ' + quoteToken(keywords[kind]) +
			', got ' + quoteToken(curToken.tokenText);
		errorExit1(errormsg);
	end;
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

(* like matchTokenOrNot, but does not return a value *)
procedure optionalToken(wantedToken: TokenType);
begin
	if checkToken(wantedToken) then
		readNextToken;
end;

(* match a token. if the token is matched, consume it and return true.
   if the current token is SemicolonToken, consume it and try to match the
   next token then return true.
   otherwise, return false and do not consume token.
   Multiple consecutive semicolons are also matched and consumed.

   Example with tok1 = EndToken:
   	"; END" returns true
	"; BEGIN" returns false
	"END" returns true
	"BEGIN" returns false
   *)

   (* FIXME: the line reported in errors is now sometimes off by one,
   		because this function scans after a possible semicolon and into
		the	next line *)
function matchEndOf(tok1: TokenType): boolean;
begin
	matchEndOf := false;
	if (curToken.tokenKind = tok1) then
	begin
		matchEndOf := true;
		readNextToken;
	end
	else
	begin
		while checkToken(SemicolonToken) do
			readNextToken;
		if matchTokenOrNot(tok1) then
			matchEndOf := true
		else
		if curToken.tokenKind in [ EndToken, UntilToken, EOFToken ] then
			errorExit2('Missing', quoteToken(keywords[tok1]))
	end;
end;

function getStringWordCount(maxLength:integer): integer;
begin
	getStringWordCount := (maxLength + (wordSize - 1)) div wordSize;
end;

function getStringMemSize(maxLength: integer): integer;
var size: integer;
begin
	size := 2 * wordSize;
	size := size + getStringWordCount(maxLength) * wordSize;
	getStringMemSize := size;
end;

procedure compareStrings(operatr: TokenType);
var compOp: CompOpString;
begin
	compOp := getCompareOpFromToken(operatr);
	if operatr = EqToken then
		emitStringComparison
	else if operatr = NotEqToken then
	begin
		emitStringComparison;
		emitBooleanNot;
	end
	else
		emitStringLexiComparison(compOp);
end;

procedure compareAggregate(operatr: TokenType; var typ: TypeSpec);
begin
	emitMemComparison(typ);
	if operatr <> EqToken then
	begin
		if operatr = NotEqToken then
			emitBooleanNot
		else
			errorExit2('Invalid comparison operator for aggregate type','');
	end;
end;

(* Scan for an integer and return its value. nothing is placed on the stack.
   Handles constant identifiers.
   should be called getInteger then for consistency.
   *)
function parseInteger: Integer;
var cnst: ConstRef;
    digits: string[12];
begin
	(* handle possible constant *)
	if checkToken(IdentToken) then
	begin
		cnst := findConstantHiera(curToken.tokenText);
		if cnst = nil then
			errorExit2('Number or constant identifier expected, got',
				curToken.tokenText);			
		if cnst^.typ.baseType <> IntegerType then
			errorExit2('Not an integer constant:', curToken.tokenText);
		parseInteger := cnst^.intValue;
	end
	else
	begin
		digits := curToken.tokenText;
		if matchTokenOrNot(MinusToken) then
		begin
			readNextToken;
			digits := digits + curToken.tokenText;
		end;
		parseInteger := integerFromString(digits);
	end;
	readNextToken;
end;

procedure parseConstant(var typeReturn: TypeSpec);
var intValue:integer;
begin
	getRangePart(intValue, typeReturn);
	emitLoadConstantInt(intValue);
end;


(*
  write a value to a variable.
  accessScalar or parseMemLocation must have been called
  before, with the same symbol reference.
  Subrange checks can be switched off, which is only used
  in for-loop iterations.
*)
procedure writeVariable2(var mem: MemLocation; checkSubranges:boolean);
begin
	if mem.typ.baseType in [ IntegerType, BooleanType, RealType, CharType, PointerType,
		EnumType, SetType ]
	then
	begin
		if mem.typ.hasSubrange and checkSubranges then
			emitSubrangeCheck(mem.typ.subStart,mem.typ.subEnd);
		case mem.memLoc of
		Indirect: emitStoreIndirect;
		GlobalMem: errorExit2('internal error: accessing GlobalMem', mem.name);
		LocalMem: emitStoreLocal(mem.offset, mem.name);
		NestedMem: emitStoreNested(mem.offset, mem.scopeDistance, mem.name);
		end;
	end
	else if mem.typ.baseType = StringType then
		emitCopyString
	else if mem.typ.baseType in [ ArrayType, RecordType ] then
		emitCopy(mem.typ.size)
	else if mem.typ.baseType = StringCharType then
		emitSetStringChar    (* store char to byte ptr *)
	else
		errorExit2('internal error: writeVariable baseType not handled for', mem.name);
end;


(*
  Write a value to a variable.
  accessScalar or parseMemLocation must have been called
  before, with the same symbol reference.
  Subrange checks are enabled.
  See writeVariable2.
*)

procedure writeVariable(var mem:MemLocation);
begin
	writeVariable2(mem, true);
end;

(*
  read value from a memory location.
  see parseMemLocation which parses an identifier into a
  memory location.
  if it is an indirect access, the address has to be on the stack already.
  this can be done with parseMemLocation or accessScalar.
*)
procedure readVariable(var mem: MemLocation);
begin
	if mem.typ.baseType in [ IntegerType, BooleanType, RealType, CharType, PointerType,
		EnumType, SetType ] then
	begin
		case mem.memLoc of
		Indirect:	emitLoadIndirect;
		GlobalMem:	errorExit2('internal error: accessing GlobalMem for', mem.name);
		LocalMem:	emitLoadLocal(mem.offset, mem.name);
		NestedMem:  emitLoadNested(mem.offset, mem.scopeDistance, mem.name);
		end;
	end
	else if mem.typ.baseType = ArrayType then
		(* nothing to do to access a whole array, its address
			is already on the stack *)
	else if mem.typ.baseType = StringType then
	begin end
		(* nothing to do to read a string variable, its
			address is already on the stack *)
	else if mem.typ.baseType = RecordType then
		(* nothing to do to read a record variable, its
			address is already on the stack *)
	else if mem.typ.baseType = StringCharType then
	begin
		emitLoadStringChar;  (* load char from byte ptr *)
		setBaseType(mem.typ, CharType); (* we now have a char on stack *)
	end
	else
		errorExit2('internal error: reading memloc', mem.name);
end;

procedure convertToIndirect(var mem: MemLocation);
begin
	if mem.memLoc <> Indirect then
	begin
		if mem.memLoc = GlobalMem then
			emitLoadGlobalAddr(mem.name, mem.offset)
		else if mem.memLoc = LocalMem then
			emitLoadLocalAddr(mem.name, mem.offset)
		else if mem.memLoc = NestedMem then
			emitLoadNestedAddr(mem.name, mem.scopeDistance, mem.offset);

		setIndirect(mem);
	end;
end;

procedure addMemOffset(var mem: MemLocation; delta: integer);
begin
	(* if the location is indirect, i.e. the address is already on stack,
		we need to emit code for increasing the address.
		otherwise, we just increase the offset *)
	if mem.MemLoc in [ Indirect, OnStack ] then
		emitInc(delta);
	mem.offset := mem.offset + delta;
end;

(* Parse possible qualifiers of a symbol down to a memory location.
	This can be a scalar variable, an array element, or a record field.
    Places the address on the stack if it is not a local variable.
	This is called after the identifier has been parsed and the
	symbol (SymblRef) has already been determined.

	"Qualifiers" are: Array indices(brackets), record field qualifiers,
		pointer dereferencing (^ operator).

	ForceIndirect is (only?) used when passing var parameters.
	The sym parameter can be nil if memReturn has already been
	initialized.

	Scalar variables should resolve to GlobalMem with a symbol name
		or to LocalMem with an offset.
	Record fields should resolve to GlobalMem with symbol + offset
		or LocalMem with cumulative offset.
	If the MemLocType is already Indirect (e.g. array of record),
		they address is	already on stack.
	Arrays of arrays or arrays as record fields should
		calculate an offset as usual and add to address on stack.
  *)

(* TODO: rename to something like parseQualifiers *)
procedure parseSymMemLoc(sym:SymblRef;forceIndirect: boolean; var memReturn: MemLocation);
var aFieldRef: ^FieldListItem;
    elementType: TypeSpec;
	pointerMemLoc: MemLocation;
begin
	if sym <> nil then
		initMemLocation(sym, memReturn);

	while curToken.tokenKind in [ LBracketToken, DotToken, PointerToken ] do
	begin
		if checkToken(LBracketToken) then
		begin
			(* indexing an array *)
			convertToIndirect(memReturn);
			if memReturn.typ.baseType = ArrayType then
			begin
				parseArrayIndex(memReturn.typ, memReturn.name, elementType);
				memReturn.typ := elementType;

				(* strings contained in arrays need to be initialized *)
				if memReturn.typ.baseType = StringType then
					memReturn.initialized := false;
			end
			else
			if memReturn.typ.baseType = StringType then
			begin
				parseStringIndex;
				memReturn.typ.baseType := StringCharType;
			end
			else
				errorExit2('invalid subscript for', memReturn.name);
		end
		else if checkToken(DotToken) then
		begin
			(* accessing a record field *)
			readNextToken;
			aFieldRef := findField(memReturn.typ, curToken.tokenText);
			if aFieldRef = nil then
				errorExit2('invalid field name', curToken.tokenText);
			readNextToken;
			addMemOffset(memReturn, aFieldRef^.offset);
			memReturn.typ := aFieldRef^.fieldType;

			(* strings contained in records need to be initialized *)
			if memReturn.typ.baseType = StringType then
				memReturn.initialized := false;

			{
			if aFieldRef^.isVariant then
				writeln('******* variant:', aFieldRef^.isVariant,
				' first case value:', aFieldRef^.tagValues.tail^.value);
			}
		end
		else if checkToken(PointerToken) then
		begin
			(* dereferencing a pointer *)
			readNextToken;
			if memReturn.typ.baseType <> PointerType then
				if sym <> nil then
					errorExit2('not a pointer:', sym^.name)
				else
					errorExit2('not a pointer', '');

			pointerMemLoc := memReturn;

			convertToIndirect(memReturn);

			memReturn.typ := memReturn.typ.pointedType^;
			memReturn.initialized := true;
			(* assume that the variable the pointer points to
				is initialized. otherwise, passing a pointer to
				a string would overwrite the string header with
				wrong values, if the string lengths
				of the argument and the parameter differ *)
			
			(* Function return values are encoded as NoMem,
				which means the value (not the address of the pointer)
				is already on stack. In this case,
				we need to skip the emitLoadIndirect. *)
			if pointerMemLoc.memLoc <> OnStack then
				emitLoadIndirect; (* the pointer variable contains the address *)
		end;
	end;

	if (memReturn.typ.baseType in [StringType, RecordType, ArrayType ])
		or forceIndirect or
		(memReturn.memLoc = GlobalMem) then
			convertToIndirect(memReturn);
end;

(* parse an identifier and possible qualifiers, see parseSymMemLoc() *)
procedure parseMemLocation(forceIndirect: boolean; var memReturn: MemLocation);
var sym: SymblRef;
begin
	if curToken.tokenKind <> IdentToken then
		errorExit2('Expected identifier, got', curToken.tokenText);
	sym := findHieraSymbol(curToken.tokenText);
	if sym = nil then
		errorExit2('Undeclared variable', curToken.tokenText);
	readNextToken;
	parseSymMemLoc(sym, forceIndirect, memReturn);
end;

procedure loadAddr(var loc: MemLocation);
begin
	case loc.memLoc of
	GlobalMem:	  emitLoadGlobalAddr(loc.name, loc.offset);
	LocalMem:  	  emitLoadLocalAddr(loc.name, loc.offset);
	NestedMem:    emitLoadNestedAddr(loc.name, loc.scopeDistance, loc.offset);
	Indirect:  	  errorExit2('internal error: loadAddr with Indirect', loc.name);
	TemporaryMem: emitLoadTempAddr(loc.name, loc.offset);
	end;
end;

(* load the pointer/reference of a local or nested var parameter *)
procedure loadVarParamRef(var loc: MemLocation);
begin
		if loc.memLoc = LocalMem then
			emitLoadLocal(loc.offset, loc.name)
		else
		if loc.memLoc = NestedMem then
			emitLoadNested(loc.offset, loc.scopeDistance,
				loc.name)
		else
			errorExit2('internal error in loadVarParamRef','');	
end;

(* calculate the address for a variable that fits into one word.
   code is emitted to put the address on the stack, if it is not
   a short access. a MemLocation record is returned which can
   be passed to readVariable/writeVariable for the actual access.
*)
procedure accessScalar(sym: SymblRef; var memLocReturn: MemLocation);
begin
	initMemLocation(sym, memLocReturn);
	if sym^.scope = GlobalSymbol then
	begin
		convertToIndirect(memLocReturn);
	end
	else if (sym^.scope in [ LocalSymbol, ParameterSymbol ]) and sym^.isVarParam then
		(* for var parameters the local variable slot contains
			the address of the value, so we do a emitLoad... not a
			emitLoad...Addr *)
		loadVarParamRef(memLocReturn)
	else
	begin
		if memLocReturn.memLoc = LocalMem then
			emitLocalMemLoc(memLocReturn)
		else if memLocReturn.memLoc = NestedMem then
			emitNestedMemLoc(memLocReturn);
	end;
end;

procedure accessVariable(sym: SymblRef; var memLocReturn: MemLocation);
begin
	accessScalar(sym, memLocReturn);
	if not isScalar(sym^.symType) then
		convertToIndirect(memLocReturn);
end;

procedure dumpVars(var table: SymbolTable);
var sym: SymblRef;
begin
	writeln('dumpVars ', table.scope);
	sym := table.first;
	while sym <> nil do
	begin
		writeln('  ', sym^.name, ' ', sym^.symType.baseType);
	end;
end;

(* the pointer to the string is already on the stack here *)
procedure parseStringIndex;
var typeReturn: TypeSpec;
begin
	matchToken(LBracketToken);
	parseExpression(typeReturn); (* now we have the string ptr and index value on stack*)
	matchBaseType(typeReturn, IntegerType);
	emitStringIndexToAddr;
	matchToken(RBracketToken);
end;

procedure parseArrayIndex(var arrayTyp: TypeSpec; var name: IdentString;var elType:TypeSpec);
var typeReturn: TypeSpec;
begin
    elType := arrayTyp;
	matchToken(LBracketToken);
	repeat
		parseExpression(typeReturn);
		if typeReturn.baseType = EnumType then
		begin
			if arrayTyp.indexEnumId = 0 then
				errorExit2('invalid array subscript type for', name);
			if typeReturn.enumId <> arrayTyp.indexEnumId then
				errorExit2('invalid array subscript type for', name)
		end
		else
		if not isIndexType(typeReturn) then
			errorExit2('invalid array subscript type for', name);

		emitIndexToAddr(elType);

		elType := elType.elementType^;

		if checkToken(CommaToken) then
		begin
			if elType.baseType <> ArrayType then
				errorExit2('invalid array subscript for', name)
		end;
	until not matchTokenOrNot(CommaToken);
	matchToken(RBracketToken);
end;

(* parse accessing an array constant.
  merge this with parseMemLocation? *)
procedure parseArrayConstAccess(cnst:ConstRef; var returnType:TypeSpec);
begin
	emitLoadArrayConst(cnst^.arrayValue);
	if checkToken(LBracketToken) then
	begin
		parseArrayIndex(cnst^.typ, cnst^.name, returnType);
		emitLoadIndirect;
	end
end;

procedure parseVarBlock; forward;

procedure parseCall(aProc: ProcRef; var optionalDest: MemLocation); forward;

(* Parse an identifier as part of an expression. also parses array indices,
	record fields and pointer dereferencing.
	See also parseLvalue which parses an identifier as the left hand side
	of an assignment.
	Emits code to place the result value on the eval stack.
   *)
procedure parseIdentifier(var returnType: TypeSpec);
var sym: SymblRef;
	cnst: ConstRef;
    func: ProcRef;
	memLoc: MemLocation;
	sf: SpecialFunc;
	name:IdentString;
	isCall:boolean;
begin
	name := curToken.tokenText;
	readNextToken;

	cnst := findConstantHiera(name);
	if cnst <> nil then
	begin
		returnType := cnst^.typ;
		case cnst^.typ.baseType of
		IntegerType, CharType, BooleanType:
			emitLoadConstantInt(cnst^.intValue);
		RealType:
			emitLoadConstantReal(cnst^.realValue);
		ArrayType:
			parseArrayConstAccess(cnst, returnType);
		StringType:
			emitLoadConstStr(cnst^.strValue);
		EnumType:
			begin
				emitLoadConstantInt(cnst^.intValue);
				returnType := cnst^.enumRef^.typePtr^;
			end
		else
			errorExit2('internal error in parseIdentifier constant',cnst^.name);
		end;
	end
	else
	begin
		(* function call syntax? *)
		isCall := checkToken(LParenToken);
		(* is it a variable?*)
		sym := findHieraSymbol(name);
		if isCall or (sym = nil) then
		begin
			(* if no symbol found, or if we have parentheses,
				it must be a function *)
			func := searchProcedure(name);
			if func = nil then
			begin
				sf := findSpecialFunction(name);
				if sf <> NoSF then
					parseSpecialFunction(sf, returnType)
				else
					errorExit2('Undeclared identifier', name)
			end
			else
			begin
				if not isFunction(func) then
					errorExit2('procedure cannot be called as a function:', name)
				else
				begin
					initNoMemLocation(memLoc);
					parseCall(func, memLoc);
					returnType := func^.returnType;
					(* a function return value can have qualifiers like array
						indexing or pointer dereferencing, so try to parse them *)
					(* TODO: redundant code with parseSymMemLoc, at least
						make a checkQualifierToken() function *)

					if curToken.tokenKind in [ LBracketToken, DotToken, PointerToken] then
					begin
						memLoc.typ := func^.returnType;
						(* address is already on stack, so the memloc type is OnStack *)
						memLoc.memLoc := OnStack; 
						parseSymMemLoc(nil, false, memLoc);
						readVariable(memLoc);
						returnType := memLoc.typ;
					end;
				end
			end
		end
		else
		begin
			(* we found a sym, so it is a variable *)
			parseSymMemLoc(sym, false, memLoc);
			readVariable(memLoc);
			returnType := memLoc.typ;
			{
			if returnType.baseType = SetType then
				writeln('********** parseIdentifier Set var enum id ', returnType.memberEnumId);
			}

		end;
	end;
end;

procedure parseLvalue(var memLocReturn: MemLocation);
begin
	parseMemLocation(false, memLocReturn);
end;

procedure parsePrimary(var typeReturn: TypeSpec);
var c:ConstStrRef;
begin
	if checkToken(LParenToken) then
	begin
		readNextToken;
		parseExpression(typeReturn);
		matchToken(RParenToken);
	end
	else
	if checkToken(NumberToken) then
	begin
		(* parse integer and real *)
		parseNumber(PlusToken, typeReturn);
	end
	else
	if checkToken(IdentToken) then
	begin
		parseIdentifier(typeReturn);
	end
	else
	if checkToken(StringLitToken) then
	begin
		c := addConstStr(curToken.tokenText);
		emitLoadConstStr(c);
		setStringTypeSize(typeReturn, c^.length);
		readNextToken;
	end
	else
	if checkToken(TrueToken) then
	begin
		emitConstBoolean(true);
		setBaseType(typeReturn, BooleanType);
		readNextToken;
	end
	else
	if checkToken(FalseToken) then
	begin
		emitConstBoolean(false);
		setBaseType(typeReturn, BooleanType);
		readNextToken;
	end
	else
	if checkToken(NilToken) then
	begin
		emitLoadConstantInt(0);
		setBaseType(typeReturn, PointerType);
		typeReturn.pointedType := nil;
		readNextToken;
	end
	else
	if checkToken(CharLitToken) then
	begin
		(* TODO: convert to string constant if destination is string *)
		emitLoadConstantInt(getCharLitValue(curToken.tokenText));
		setBaseType(typeReturn, CharType);
		readNextToken;
	end
	else
		errorExit2('Unexpected token ', quoteToken(curToken.tokenText));
end;

procedure parseUnary(var typeReturn: TypeSpec);
begin
	if checkToken(PlusToken) then
	begin
		readNextToken;
		parsePrimary(typeReturn);
		if not (typeReturn.baseType in [IntegerType, RealType]) then
			errorExit2('Expected INTEGER or REAL type for unary','+')
	end
	else
	if checkToken(MinusToken) then
	begin
		readNextToken;
		if checkToken(NumberToken) then
			parseNumber(MinusToken, typeReturn)
		else
		begin
			parsePrimary(typeReturn);
			if typeReturn.baseType = IntegerType then
				emitNegate
			else if typeReturn.baseType = RealType then
				emitNegFloat32;
		end;

		if not (typeReturn.baseType in [ IntegerType, RealType]) then
				errorExit2('Expected INTEGER or REAL type for unary','-');
	end
	else
	if checkToken(NotToken) then
	begin
		readNextToken;
		parsePrimary(typeReturn);
		if typeReturn.baseType = BooleanType then
			emitBooleanNot
		else
		if typeReturn.baseType = IntegerType then
			emitNot
		else
			errorExit2('Boolean or integer operand expected', '');
	end
	else
		parsePrimary(typeReturn);
end;

procedure shiftIntegerOp(var typeA, typeB: TypeSpec; op: string);
begin
	matchBaseTypes(typeA, typeB, IntegerType);
	emitShiftMultiple(op);
end;

procedure integerOp(var typeA, typeB: TypeSpec; op: string);
begin
	matchBaseTypes(typeA, typeB, IntegerType);
	emitOperator(op);
end;

procedure realOp(var typeA, typeB: TypeSpec; op: string);
begin
	matchRealCompatibleArgs(typeA, typeB);
	emitFloatOperator(op);
end;

procedure arithmeticOp(var typeA, typeB:  TypeSpec; op: string);
begin
	if (typeA.baseType = RealType) or (typeB.baseType = RealType) then
		realOp(typeA, typeB, op)
	else
		integerOp(typeA, typeB, op);
end;

procedure logicOp(var typeA, typeB:  TypeSpec; op: string);
begin
	matchLogicOpTypes(typeA, typeB);
	emitOperator(op);
end;

procedure parseTerm(var typeReturn: TypeSpec);
var operatr: TokenType;
	typeA, typeB: TypeSpec;
begin
	typeA := typeReturn;
	typeB := typeReturn;
	parseUnary(typeA); (* parse first operand *)

	(* ugly hack for set expressions *)
	if typeA.baseType = SetType then
		parseSetExprTail(typeA)
	else
	begin
		while curToken.tokenKind in [AsteriskToken, DivToken, ModToken, SlashToken,
			AndToken, ShrToken, ShlToken ] do
		begin
			operatr := curToken.tokenKind;
			readNextToken;
			(* / check first operand for real type, realOp checks second operand *)
			if operatr = SlashToken then matchRealType(typeA);
			parseUnary(typeB); (* parse second operand *)
			if operatr = AsteriskToken   then arithmeticOp(typeA, typeB, 'MUL')
			else if operatr = DivToken	 then arithmeticOp(typeA, typeB, 'DIV')
			else if operatr = ModToken   then arithmeticOp(typeA, typeB, 'MOD')
			else if operatr = ShlToken   then shiftIntegerOp(typeA, typeB, 'SHLM')
			else if operatr = ShrToken   then shiftIntegerOp(typeA, typeB, 'SHRM')
			else if operatr = SlashToken then realOp(typeA, typeB, 'DIV')
			else if operatr = AndToken   then logicOp(typeA, typeB, 'AND');
		end;
	end;
	typeReturn := typeA;
end;

procedure dumpArrayConst(a:ArrayConstRef);
var curElem: ^OpaqueDataElement;
begin
	curElem := a^.firstElement;
	writeln('**** dumpArrayConst id ', a^.id);
	write('**** dumpArrayConst [ ');
	while curElem <> nil do
	begin
		if curElem^.isStringValue then
			write(curElem^.strValue^)
		else
			write(curElem^.intValue,' ');
		curElem := curElem^.next;
	end;
	writeln(']');
end;

function getCharValue:integer;
var	cons: ConstRef;
begin
	if checkToken(IdentToken) then
	begin
		cons := findConstantHiera(curToken.tokenText);
		if cons = nil then
			errorExit2('invalid character constant', curToken.tokenText);
		matchBaseType(cons^.typ, CharType);
		getCharValue := cons^.intValue;
		readNextToken;
	end
	else
	begin
		matchToken(CharLitToken);
		getCharValue := ord(lastToken.tokenText[1]);
	end;
end;

procedure parseCharConstArray(arrayConst:ArrayConstRef);
var count: integer;
    value: integer;
	startValue,endValue: integer;
begin
	count := 0;

	while curToken.tokenKind in [CharLitToken, IdentToken] do
	begin
		startValue := getCharValue;
		endValue := startValue;
		(* process a subrange specification *)
		if checkToken(DotToken) then
		begin
			readNextToken;
			if not matchTokenOrNot(DotToken) then
				errorExit2('invalid subrange spec after', lastToken.tokenText);
			endValue := getCharValue;
		end;

		for value := startValue to endValue do
		begin
			count := count + 1;
			addArrayConstElem(arrayConst, value);
		end;

		if checkToken(CommaToken) then
		begin
			readNextToken;
			if not (curToken.tokenKind in [CharLitToken, IdentToken]) then
				errorExit2('char literal or constant expected, got', curToken.tokenText);
		end;
	end;
	arrayConst^.count := count;
end;

(* TODO: merge with parseCharConstArray? *)
(* TODO: reuse parseArrayLitValue, parseConstValue, getConstvalue? *)
procedure parseIntConstArray(arrayConst:ArrayConstRef);
var count: integer;
    value: integer;
	startValue, endValue: integer;
begin
	count := 0;
	while curToken.tokenKind in [NumberToken, IdentToken] do
	begin
		startValue := parseInteger;
		endValue := startValue;
		(* process a subrange specification *)
		if checkToken(DotToken) then
		begin
			readNextToken;
			if not matchTokenOrNot(DotToken) then
				errorExit2('invalid subrange spec after', lastToken.tokenText);
			endValue := parseInteger;
		end;

		for value := startValue to endValue do
		begin
			count := count + 1;
			addArrayConstElem(arrayConst, value);
		end;

		if checkToken(CommaToken) then
		begin
			readNextToken;
			if not (curToken.tokenKind in [NumberToken, IdentToken]) then
				errorExit2('integer literal or constant expected, got', curToken.tokenText);
		end
	end;
	arrayConst^.count := count;
end;

function getBooleanValue:boolean;
begin
	if checkToken(TrueToken) then
		getBooleanValue := true
	else if checkToken(FalseToken) then
		getBooleanValue := false
	else
		errorExit2('Expected TRUE or FALSE, got', curToken.tokenText);
	readNextToken;
end;

procedure getRangePart(var value:integer; var typeReturn: TypeSpec);
var cnst: ConstRef;
begin
	setBaseType(typeReturn, NoType);

	if checkToken(IdentToken) then
	begin
		(* is it a constant ? *)
		cnst := findConstantHiera(curToken.tokenText);
		if cnst <> nil then
		begin
			typeReturn := cnst^.typ;
			if cnst^.typ.baseType in [IntegerType, EnumType, CharType ] then
				value := cnst^.intValue
			else
				errorExit2('scalar value or constant identifier expected, got',
					curToken.tokenText);
		end
		else
			errorExit2('scalar value or constant identifier expected, got',
				curToken.tokenText);
		readNextToken;
	end
	else if checkToken(CharLitToken) then
	begin
		setBaseType(typeReturn, CharType);
		value := ord(curToken.tokenText[1]);
		readNextToken;
	end
	else if checkToken(TrueToken) or checkToken(FalseToken) then
	begin
		setBaseType(typeReturn, BooleanType);
		value := ord(getBooleanValue);
	end
	else
	begin
		setBaseType(typeReturn, IntegerType);
		value := parseInteger;
	end;
end;

(* TODO: merge with parseCharConstArray? *)
procedure parseEnumConstArray(arrayConst:ArrayConstRef);
var count: integer;
    value: integer;
	startValue, endValue: integer;
	typeA, typeB: TypeSpec;
begin
	count := 0;
	while checkToken(IdentToken) do
	begin
		getRangePart(startValue, typeA);
		endValue := startValue;

		(* process a subrange specification *)
		if checkToken(DotToken) then
		begin
			readNextToken;
			if not matchTokenOrNot(DotToken) then
				errorExit2('invalid subrange spec after', lastToken.tokenText);
			getRangePart(endValue, typeB)
		end;

		for value := startValue to endValue do
		begin
			count := count + 1;
			addArrayConstElem(arrayConst, value);
		end;

		if checkToken(CommaToken) then
		begin
			readNextToken;
			if not checkToken(IdentToken) then
				errorExit2('integer literal or constant expected, got', curToken.tokenText);
		end
	end;
	arrayConst^.count := count;
end;

(* parse and convert an array literal without the brackets *)
function getArrayConstRaw(var typeReturn: TypeSpec): ArrayConstRef;
var newArrayConst: ArrayConstRef;
    newArrayType:^TypeSpec;
	cons: ConstRef;
	baseType: SymbolType;
begin
	setBaseType(typeReturn, ArrayType);

	newArrayConst := addArrayConst;

	new(newArrayType);

	(* check for symbolic constant to determine type *)
	if checkToken(IdentToken) then
	begin
		cons := findConstantHiera(curToken.tokenText);
		if cons = nil then
			errorExit2('invalid element identifier', curToken.tokenText);
		baseType := cons^.typ.baseType;
		case baseType of
		IntegerType: parseIntConstArray(newArrayConst);
		CharType:	 parseCharConstArray(newArrayConst);
		EnumType:	 parseEnumConstArray(newArrayConst);
		else
			errorExit2('element must be of integer, char or enum type:', curToken.tokenText);
		end;
		newArrayType^ := cons^.typ;
	end
	else if checkToken(CharLitToken) then
	begin
		setBaseType(newArrayType^, CharType);
		parseCharConstArray(newArrayConst);
	end
	else if checkToken(NumberToken) then
	begin
		setBaseType(newArrayType^, IntegerType);	(* TODO: handle real numbers *)
		parseIntConstArray(newArrayConst);
	end
	else
		errorExit2('invalid set/array literal at', curToken.tokenText);

	typeReturn.arrayLength := newArrayConst^.count;
	typeReturn.arrayStart := 1;
	typeReturn.arrayEnd := typeReturn.arrayLength;
	typeReturn.elementType := newArrayType;
	typeReturn.size := typeReturn.elementType^.size * typeReturn.arrayLength;

	{ dumpArrayConst(newArrayConst); }

	getArrayConstRaw := newArrayConst;
end;

(* parse and convert an array literal including the brackets *)
function getArrayConst(var typeReturn: TypeSpec): ArrayConstRef;
begin
	matchToken(LBracketToken);
	getArrayConst := getArrayConstRaw(typeReturn);
	matchToken(RBracketToken);
end;

procedure dumpEnumById(enumId:integer);
var typRef: TypeRef;
	names: StringList;
	name:  IdentString;
begin
	typRef := findEnumById(enumId);
	if typRef = nil then
		writeln('enum id ', enumId, ' not found')
	else
	begin
		names := typRef^.typePtr^.enumList;
		rewindStringList(names);
		while(nextStringListItem(names, name)) do write(name, ' ');
		writeln;
	end;
end;

(* we handle that what is syntactically a set literal in 
   standard pascal as an array literal. <- Should be called
   "parseArrayLiteral" then.
   Real set literals are parsed in parseSetTerm/parseSetExpression *)
procedure parseSetLiteral(var typeReturn: TypeSpec);
var newArrayConst: ArrayConstRef;
begin
	newArrayConst := getArrayConst(typeReturn);
	emitLoadArrayConst(newArrayConst);
end;

(* parse some comma-separated set items without the brackets *)
procedure parseSetValue(var elementType: TypeSpec);
var sym:SymblRef;
	cnst: ConstRef;
	savedType: TypeSpec;
begin
	(* we don't know the type at first *)
	setBaseType(savedType, NoType);
	(* start with an empty set value on the stack,
		then set bits below *)
	emitLoadConstantInt(0);
	repeat
		if checkToken(IdentToken) 
		then
		begin
			(* is it a variable? *)
			sym := findHieraSymbol(curToken.tokenText);
			if sym <> nil then
			begin
				parseIdentifier(elementType);
				if savedType.baseType = NoType then
					savedType := elementType;
			end
			else
			begin
				(* if not, it should be a constant *)
				cnst := findConstantHiera(curToken.tokenText);
				if cnst = nil then
					errorExit2('Integer constant or variable expected, got',
						curToken.tokenText);
				elementType := cnst^.enumRef^.typePtr^;
				if savedType.baseType = NoType then
					savedType := elementType;
				emitLoadConstantInt(cnst^.intValue);
			end;
			emitAddToSet;
			readNextToken;
		end
		else if checkToken(NumberToken) then
		begin
			errorExit2('Integers in sets not implemented yet',
				curToken.tokenText);
		end
		else if checkToken(RBracketToken) then
		begin
			(* empty set is permissible *)
		end
		else
			errorExit2('Integer constant or variable expected, got',
				curToken.tokenText);

		if savedType.baseType <> NoType then
			matchTypes(savedType, elementType);

	until not matchTokenOrNot(CommaToken);
end;

procedure parseSetTerm(var setTypeReturn: TypeSpec);
var typ:TypeSpec;
	elementType:TypeSpec;
begin
	if checkToken(LBracketToken) then
	begin
		(* handle a set literal *)
		readNextToken;
		parseSetValue(elementType);
		matchToken(RBracketToken);
		setBaseType(setTypeReturn, SetType);
		setTypeReturn.memberBaseType := elementType.baseType;
		if elementType.baseType = EnumType then
			setTypeReturn.memberEnumId := elementType.enumId;
		setTypeReturn.hasSubrange := elementType.hasSubrange;
		setTypeReturn.subStart    := elementType.subStart;
		setTypeReturn.subEnd      := elementType.subEnd;
	end
	else
	if checkToken(IdentToken) then
	begin
		(* handle a set variable *)
		parseIdentifier(typ);
		matchBaseType(typ, SetType);
		setTypeReturn := typ;
	end
	else
		errorExit2('rest of parseSetTerm not implemented yet',
			curToken.tokenText);
end;

(* Parse second part of a set expression.
   typeA needs to be set to the set type (not the member type).
	Valid operators are: +, -, *, =, <>, <= 
	*)

procedure parseSetExprTail(var typeA: TypeSpec);
var tok:TokenType;
	typeB: TypeSpec;
begin
	tok := curToken.tokenKind;

	while tok in [ PlusToken, MinusToken, AsteriskToken,
		EqToken, NotEqToken, LtEqToken, GtEqToken] do
	begin
		readNextToken;
		parseSetTerm(typeB);
		{
		dumpEnumById(typeA.memberEnumId);
		dumpEnumById(typeB.memberEnumId);
		}
		matchTypes(typeA, typeB);

		case tok of
		PlusToken:		emitSetAdd;
		MinusToken:		emitSetSubtract;
		AsteriskToken:	emitSetIntersect;
		EqToken:		emitSetCompare;
		NotEqToken:		emitSetCompareNE;
		LtEqToken:		emitSetIsSubset;
		GtEqToken:		begin emitSwap; emitSetIsSubset; end;
		end;

		if tok in [ EqToken, NotEqToken, LtEqToken, GtEqToken ] then
			setBaseType(typeA, BooleanType);

		tok := curToken.tokenKind;
	end;
end;

(* Parse a set expression (which may be a single term), return
  the type in typeReturn *)
procedure parseSetExpression(var typeReturn: TypeSpec);
var elementType:TypeSpec;
begin
	parseSetTerm(elementType);
	setBaseType(typeReturn, SetType);
	typeReturn.memberBaseType := elementType.baseType;
	if typeReturn.memberBaseType = EnumType then
		typeReturn.memberEnumId   := elementType.enumId;
	typeReturn.hasSubrange := elementType.hasSubrange;
	typeReturn.subStart    := elementType.subStart;
	typeReturn.subEnd      := elementType.subEnd;
	parseSetExprTail(typeReturn);
end;

procedure parseInOperator(var typeA: TypeSpec);
var typeB: TypeSpec;
begin
	if not isSimpleType(typeA) then
		errorExit2('invalid IN operand*', lastToken.tokenText);

	matchToken(InToken);
	parseExpression(typeB);
	if (typeB.baseType = ArrayType) then
	begin
		{
		writeln('**** parseInOperator types ',
			typeA.baseType, ' -> ', typeB.elementType^.baseType);
		if typeA.baseType = EnumType then
			writeln('       enum types ', typeA.enumId, ' ', typeB.elementType^.enumId);
		}
		matchTypes(typeA, typeB.elementType^);
		emitIsInArray(typeB.arrayLength);
	end
	else
	if typeB.baseType = SetType then
	begin
		matchBaseType(typeA, typeB.memberBaseType);
		if (typeB.memberBaseType = EnumType) and
			(typeA.enumId <> typeB.memberEnumId) then
			errorExit2('Invalid IN operand', lastToken.tokenText);
		emitIsInSet;
	end
	else
	if typeB.baseType = StringType then
	begin
		if not (typeA.baseType in [ CharType, StringCharType ]) then
			errorExit2('Invalid IN operand before', lastToken.tokenText);
		emitIsInString;
	end
	else
		errorExit2('invalid IN operand', lastToken.tokenText);
end;

procedure parseSimpleExpression(var typeReturn: TypeSpec);
var operatr: TokenType;
	typeA, typeB: TypeSpec;
begin
	if checkToken(LBracketToken) then
		parseSetLiteral(typeReturn)
	else
	begin
		parseTerm(typeA);
		(* special cases for char and string expressions *)
		if typeA.baseType = CharType then
			parseCharExprTail(typeA)
		else
		if typeA.baseType = StringType then
			parseStringExprTail(typeA)
		else
		begin
			while curToken.tokenKind in [PlusToken, MinusToken, OrToken, XorToken ] do
			begin
				operatr := curToken.tokenKind;
				readNextToken;
				parseTerm(typeB);
				if operatr = PlusToken       then arithmeticOp(typeA, typeB, 'ADD')
				else if operatr = MinusToken then arithmeticOp(typeA, typeB, 'SUB')
				else if operatr = OrToken    then logicOp(typeA, typeB, 'OR')
				else if operatr = XorToken   then logicOp(typeA, typeB, 'XOR');
			end;
		end;
		typeReturn := typeA;
	end;
end;

(* Parse an expression. The value of the expression is placed on the stack.
   The type is returned in typeReturn.
   In case of an aggregate type, a temporary is allocated and the address
   is placed on the stack *)
procedure parseExpression(var typeReturn: TypeSpec);
var operatr: TokenType;
	typeA, typeB: TypeSpec;
	compOp: CompOpString;
begin
	parseSimpleExpression(typeA);
	typeReturn := typeA;
	if checkToken(InToken) then
	begin
		parseInOperator(typeA);
		setBaseType(typeReturn, BooleanType);
	end
	else
	if checkComparisonOperator(curToken.tokenKind) then
	begin
		operatr := curToken.tokenKind;
		compOp := getCompareOpFromToken(operatr);
		readNextToken;
		parseSimpleExpression(typeB);
		matchComparableTypes(typeA, typeB);
		setBaseType(typeReturn, BooleanType);
		if typeA.baseType = RealType then
		begin
			matchRealType(typeB); (* converts b from int to real if necessary *)
			emitFloatComparison(compOp);
		end
		else if (typeA.baseType = IntegerType) and (typeB.baseType = RealType) then
		begin
			(* special case for comparing integer to real *)
			emitIntFloatComparison(compOp);
		end
		else if isScalar(typeA) then
		begin
			emitComparison(compOp);
		end
		else
		begin
			if typeA.baseType = StringType then
				compareStrings(operatr)
			else
				compareAggregate(operatr, typeA); 
		end;
	end;
end;

procedure parseStringPrimary;
var aConstStr: ConstStrRef;
    typeReturn: TypeSpec;
begin
	if checkToken(StringLitToken) or checkToken(CharLitToken) then
	begin
		aConstStr := addConstStr(curToken.tokenText);
		emitLoadConstStr(aConstStr);
		readNextToken;
	end
	else if checkToken(IdentToken) then
	begin
		parseIdentifier(typeReturn);
		if typeReturn.baseType = CharType then
			convertCharToString(typeReturn);
		matchBaseType(typeReturn, StringType);
	end
	else
		errorExit2('Expected string, got', curToken.tokenText);
end;

(* Emit a copy or initfrom call, depending on the 
	initialization flag of the MemLocation.
	We cannot always initialize strings on assignment,
	as this would corrupt var parameters or
	dereferenced string pointers.
	We use the initialized field of Symbl and MemLocation
	to track if a string has already been initialized.
	*)
procedure initOrCopyString(var dstMem: MemLocation);
begin
	if dstMem.initialized then
		emitCopyString
	else
		emitInitStringFrom(dstMem.typ.stringLength);
end;

(* allocate a temporary and initialize it from the
   string pointer on the stack, which is then removed.*)
procedure getTempFromString(srcType:TypeSpec; var tempReturn: MemLocation);
var typ:TypeSpec;
begin
	typ := srcType;
	if typ.stringLength < DefaultStringLength then
		setStringTypeSize(typ, DefaultStringLength);

	allocTemporary(curProcedure, typ, tempReturn);
	loadAddr(tempReturn); (* put address of temporary on stack *)
	emitSwap;		(* and swap it with src for COPYSTRING *)
	initOrCopyString(tempReturn); (* copy src to temp *)
end;

(* convert a string to a temporary.
   allocate temp space and copy source to temp.
   requires src string address on stack,
   which is then replaced by the address of the temporary *)
procedure convertStringToTemp(srcType:TypeSpec; var tempReturn: MemLocation);
begin
	getTempFromString(srcType, tempReturn);
	emitLoadTempAddr(tempReturn.name, tempReturn.offset);
end;

(* parse the tail of a string expression, that is
   everything after a plus operator if there is one
   (including the plus operator) *)
procedure parseStringExprTail(dstType: TypeSpec);
var temp: MemLocation;
begin
	if checkToken(PlusToken) then
	begin
		(* if there is a plus operator, we need to allocate a
			temporary to build the concatenated string which is 
			then copied to the destination. this is required
			so that it is possible to have the same string variable
			on the left and the right side of the assignment.
			example: s := '/' + s
		*)
		getTempFromString(dstType, temp);
		while checkToken(PlusToken) do
		begin
			readNextToken;
			emitLoadTempAddr(temp.name, temp.offset);
			parseStringPrimary;
			emitAppendString;
		end;
		(* put temporary address on stack as src for final COPYSTRING call *)
		emitLoadTempAddr(temp.name, temp.offset);
	end;
end;

(* parse a string expression, which can be a single string identifier/literal or
	a concatenation with a plus operator *)
procedure parseStringExpression(var dstMem: MemLocation);
begin
	parseStringPrimary; (* parse first primary, addr is placed on stack *)
	parseStringExprTail(dstMem.typ); (* parse the rest, if any *)
	initOrCopyString(dstMem); (* copy to destination *)
end;

(* parse the tail of a char expression, which can be a "+" operator,
	making it a string expression, or an IN operator *)
procedure parseCharExprTail(var typeA: TypeSpec);
begin
	if checkToken(PlusToken) then
	begin
		convertCharToString(typeA);
		parseStringExprTail(typeA);
	end
	else
	if checkToken(InToken) then
	begin
		parseInOperator(typeA);
		setBaseType(typeA, BooleanType);
	end;
end;

procedure parseCompoundStatement;
begin
	if checkToken(BeginToken) then
	begin
		readNextToken;
		while not checkToken(EndToken) do
		begin
			parseStatement;
			if not checkToken(EndToken) then
				matchToken(SemicolonToken);
		end;
		matchToken(EndToken);
	end
	else
		parseStatement;
end;

(* Parse a range specification in the form 1..10.
   Handles constants, enums, subrange types.
   In case of an enum, the type is returned in typeReturn.
   Otherwise, typeReturn is set to NoType.
   For enum and subrange types, a single type identifier stands
   for the start and the end value, like so:
		type aSubrangeType = 1..10;
		type anEnum = (one, two, three);
		var anArray: array [aSubrangeType] of boolean;
		var array2:  array [anEnum] of boolean;

	Cases to cover:
	1..10				-> returns integer type with subrange
	1..c				->    "
	c..10				->    "
	c1..c2				->    "
	enum-type			-> returns enum type
	enumval1..enumval2  -> returns enum type
	subrange-type		-> returns enum type
    *)
procedure getRange(var typeReturn:TypeSpec);
var typ,typ2: TypeSpec;
	need2nd: boolean;
	rStart,rEnd: integer;
begin
	need2nd := true;
	setBaseType(typeReturn, NoType);
	setBaseType(typ, NoType);
	setBaseType(typ2, NoType);

	if checkToken(IdentToken) then
	begin
		(* is it a enum or subrange type identifier? *)
		typ := findTypeHiera(curToken.tokenText);
		if typ.baseType <> NoType then
		begin
			readNextToken;
			need2nd := false;
			if typ.baseType = EnumType then
			begin
				setSubrange(typ, 0, typ.enumLength - 1);
				typeReturn := typ;
			end
			else
			if (typ.baseType = IntegerType) and (typ.hasSubrange) then
			begin
				typeReturn := typ;
			end
			(* TODO: can also be a set type identifier *)
			else
				errorExit2('invalid range specification', curToken.tokenText);
		end
		else
			(* should be a constant now, maybe with an enum type *)
			getRangePart(rStart, typ);
	end
	else if curToken.tokenKind in [ NumberToken, MinusToken, CharLitToken ] then
	begin
		(* integer and char can also be handled by getRangePart *)
		getRangePart(rStart, typ);
	end
	else
		errorExit2('invalid range specification', curToken.tokenText);

	if need2nd then
	begin
		matchToken(DotToken);
		matchToken(DotToken);

		getRangePart(rEnd,typ2);

		if typ.baseType <> typ2.baseType then
			errorExit2('invalid range specification', lastToken.tokenText);

		if rStart > rEnd then
			errorExit2('range start must be less than end', lastToken.tokenText);

		typeReturn := typ;
		setSubrange(typeReturn, rStart, rEnd);
	end;
end;

procedure parseRangeSpec(var typeReturn:TypeSpec);
begin
	getRange(typeReturn);
end;

(* Parse the range part of an array declaration and try to determine
	the element type.
	When handling multiple array dimensions,
	parseArraySpecPart is called recusively for each part
	(like "1..10,1..10").
	Returns the resulting type in the typSpec var parameter,
	also creates a complete type chain for the element type
	or types for multidimensional arrays.

	Parses the end of the array spec including the right bracket
	and the "OF" type declaration.
*)
procedure parseArraySpecPart(var typSpec: TypeSpec);
var rangeStart,rangeEnd: integer;
	range: TypeSpec;
	newType: ^TypeSpec;
begin
		getRange(range);
		
		rangeStart := range.subStart;
		rangeEnd   := range.subEnd;

		typSpec.baseType := ArrayType;
		typSpec.arrayStart := rangeStart;
		typSpec.arrayEnd := rangeEnd;
		typSpec.arrayLength := rangeEnd - rangeStart + 1;
		if range.baseType = EnumType then
			typSpec.indexEnumId := range.enumId
		else
			typSpec.indexEnumId := 0;

		new(newType);

		if checkToken(CommaToken) then
		begin
			readNextToken;
			parseArraySpecPart(newType^);
			(* need to call recursively to calculate the element sizes
				from right to left *)
		end
		else
		begin
			matchToken(RBracketToken);
			matchToken(OfToken);
			new(newType);
			parseTypeSpec(newType^, false);
		end;
		typSpec.elementType := newType;
		typSpec.size := newType^.size * typSpec.arrayLength;
end;

procedure addUnresolvedType(typePtr:TypeSpecPtr);
var t:TypeRef;
    newItem: TypeRef;
begin
	new(newItem);
	newItem^.typePtr := typePtr;
	newItem^.name := '';
	newItem^.next := nil;

	t := curProcedure^.unresolved;
	if t = nil then
		curProcedure^.unresolved := newItem
	else
	begin
		(* get to end of list *)
		while t^.next <> nil do t := t^.next;
		t^.next := newItem;
	end;
end;

procedure parseAnonRecordType(var typeReturn: TypeSpec); forward;

procedure parseTypeSpec(var typSpec: TypeSpec; allowUnresolved:boolean);
var	length: integer;
    pointedType: ^TypeSpec;
	cnst: ConstRef;
	nameStr: ^IdentString;
	elementType: TypeSpec;
	namebuf: IdentString;
begin
	if not (curToken.tokenKind in
	[ IntegerToken, RealToken, StringToken, BooleanToken, CharToken,
		ArrayToken, LParenToken, IdentToken, PointerToken, NumberToken,
		MinusToken, CharLitToken, SetToken, RecordToken, PackedToken ]) then
		errorExit2('invalid type', curToken.tokenText);

	typSpec.size := wordSize; (* use a sensible default *)
	typSpec.hasSubrange := false;

	if checkToken(LParenToken) then
	begin
		nextAnonTypeName(namebuf);
		parseEnumDecl(namebuf, typSpec);
	end
	else
	if checkToken(PointerToken) then
	begin
		readNextToken;
		typSpec.baseType := PointerType;
		new(pointedType);
		parseTypeSpec(pointedType^, true);
		typSpec.pointedType := pointedType;
		(* pointers can point to a yet not declared type *)
		if pointedType^.baseType = UnresolvedType then
			addUnresolvedType(pointedType);
	end
	else
	if checkToken(StringToken) then
	begin
		typSpec.baseType := StringType;
		length := DefaultStringLength;
		readNextToken;
		if checkToken(LBracketToken) then
		begin
			readNextToken;
			length := parseInteger;
			matchToken(RBracketToken);
		end;
		typSpec.size := getStringMemSize(length);
		typSpec.baseType := StringType;
		typSpec.stringLength := length;
	end
	else if checkToken(IntegerToken) then
	begin
		typSpec.baseType := IntegerType;
		readNextToken;
	end
	else if checkToken(RealToken) then
	begin
		typSpec.baseType := RealType;
		readNextToken;
	end
	else if checkToken(BooleanToken) then
	begin
		typSpec.baseType := BooleanType;
		readNextToken;
	end
	else if checkToken(CharToken) then
	begin
		typSpec.baseType := CharType;
		readNextToken;
	end
	else if checkToken(NumberToken) or checkToken(MinusToken) then
	begin
		parseRangeSpec(typSpec)
	end
	else if checkToken(CharLitToken) then
	begin
		parseRangeSpec(typSpec)
	end
	else if checkToken(IdentToken) then
	begin
		(* if it is a constant it must be part of a range *)
		cnst := findConstantHiera(curToken.tokenText);
		if cnst <> nil then
		begin
			parseRangeSpec(typSpec);
		end
		else
		begin
			(* if it is not a constant, it must be a type identifier *)
			typSpec := findTypeHiera(curToken.tokenText);
			if typSpec.baseType = NoType then
			begin
				if not allowUnresolved then
					errorExit2('invalid type', curToken.tokenText);
				setBaseType(typSpec, UnresolvedType);
				new(nameStr);
				nameStr^ := curToken.tokenText;
				typSpec.typeName := nameStr;
				typSpec.sourceLine := lineno;
			end;
			readNextToken;
		end;
	end
	else if checkToken(SetToken) then
	begin
		readNextToken;
		matchToken(OfToken);
		parseTypeSpec(elementType,false);
		if not (elementType.baseType in [IntegerType, BooleanType, CharType, EnumType])
			then errorExit2('invalid set member type', lastToken.tokenText);
		if elementType.baseType in [IntegerType, CharType] then
		begin
			if not (elementType.hasSubrange and
				(elementType.subStart >=0) and (elementType.subEnd < wordBits)) then
				errorExit2('Unsupported set size', '');
		end
		else if elementType.baseType = EnumType then
		begin
			if elementType.hasSubrange then
			begin
				if not ((elementType.subStart >= 0) and
					(elementType.subEnd < wordBits)) then
					errorExit2('Unsupported set size', '');
			end
			else
				if elementType.enumLength > wordBits then
					errorExit2('Unsupported set size', '');
			typSpec.memberEnumId := elementType.enumId;
		end;
		(* if it is not integer, char or enum, it is boolean, which will
			most certainly within a word *)

		setBaseType(typSpec, SetType);
		typSpec.memberBaseType	:= elementType.baseType;
		typSpec.hasSubrange		:= elementType.hasSubrange;
		typSpec.subStart 		:= elementType.subStart;
		typSpec.subEnd 			:= elementType.subStart;
	end
	else 
	begin
		optionalToken(PackedToken);
		if checkToken(ArrayToken) then
		begin
			readNextToken;
			matchToken(LBracketToken);
			parseArraySpecPart(typSpec);
		end
		else
		if checkToken(RecordToken) then
			parseAnonRecordType(typSpec)
		else
			(* TODO: test if it really cannot happen and remove *)
			(* happens at the moment with something like "packed char" *)
			errorExit2('invalid type (should not happen)', curToken.tokenText);
	end
	
end;

procedure parseRecordDecl(var newTypeName:IdentString); forward;

procedure parseAnonRecordType(var typeReturn: TypeSpec);
var typeNam:IdentString;
    recTypeRef: TypeRef;
begin
	nextAnonTypeName(typeNam);
	parseRecordDecl(typeNam);
	recTypeRef := findTypeRef(curProcedure, typeNam);
	typeReturn := recTypeRef^.typePtr^;
end;

procedure validateParam(var forwardParam:SymblRef; aProc:ProcRef;
	var name:IdentString; var typ:TypeSpec; isVarParam:boolean);
var valid:boolean;
begin
	valid := true;
	if forwardParam = nil then
		valid := false
	else
	if forwardParam^.name <> name then
		valid := false
	else
	if not isSameType(forwardParam^.symType, typ) then
		valid := false
	else
		valid := forwardParam^.isVarParam = isVarParam;

	if not valid then
		errorExit2('Parameters do not match forward declaration for', aProc^.name);

	forwardParam := forwardParam^.next;
end;

procedure parseParameter(aProc: ProcRef; var forwardParam:SymblRef);
var name: IdentString;
	names: StringList;
	typSpec: TypeSpec;
	isVarParam: boolean;
begin
	initStringList(names);

	if checkToken(VarToken) then
	begin
		isVarParam := true;
		readNextToken;
	end
	else
		isVarParam := false;

	repeat
		addToStringList(names, curToken.tokenText);
		matchToken(IdentToken);
	until not matchTokenOrNot(CommaToken);
	matchToken(ColonToken);
	parseTypeSpec(typSpec, false);

	(* create parameters with the declared type from the list of names *)
	while(nextStringListItem(names, name)) do
	begin
		if aProc^.isForward then
			validateParam(forwardParam, aProc, name, typSpec, isVarParam)
		else
			addParam(aProc, name, typSpec, isVarParam);
	end;

	disposeStringList(names);
end;

procedure storeArg(sym: SymblRef);
begin
	if not sym^.isVarParam then
	begin
		(* aggregates which are not var params need to be copied *)
		if sym^.symType.baseType in [ RecordType, ArrayType ] then
		begin
			emitLoadLocalAddr(sym^.name, sym^.offset);
			emitSwap; (* COPYWORDS wants src on ToS then dest *)
			emitCopy(sym^.size);
		end
		else if sym^.symType.baseType = StringType then
		begin
			emitLoadLocalAddr(sym^.name, sym^.offset);
			emitSwap; (* INITSTRINGFROM wants src on ToS then dest *)
			emitInitStringFrom(sym^.symType.stringLength);
		end
		else
			emitStoreArg(sym);
	end
	else
		emitStoreArg(sym);
end;

procedure initLocalString(sym: SymblRef);
begin
	if (not sym^.isVarParam) and (not sym^.initialized) then
	begin
		emitForceInitString(sym^.name, sym^.offset, sym^.symType.stringLength);
		sym^.initialized := true;
	end;
end;

procedure initTemporaryString(loc: MemLocation);
begin
	emitInitTempString(loc.name, loc.offset, loc.typ.stringLength);
end;

function typeContainsString(typ: TypeSpec): boolean;
var field: FieldRef;
begin
	typeContainsString := false;

	if typ.baseType = StringType then
		typeContainsString := true
	else
	if typ.baseType = RecordType then
	begin
		field := typ.fields;
		while (not typeContainsString) and (field <> nil) do
		begin
			typeContainsString := typeContainsString(field^.fieldType);
			field := field^.next;		
		end
	end
	else
	if typ.baseType = ArrayType then
		typeContainsString := typeContainsString(typ.elementType^)
end;

(* this procedure should initialize local variables,
	but since we do not guarantee variable initialization,
	we don't do anything here, for now, except for strings.
	strings are normally initialized on assignment, but when
	we pass them as var parameters before they have been assigned,
	they need to be initialized.
	If a record or an array contains a string, we call CLEARMEM
	for that variable, so it can be recognized as uninitialized. *)
procedure initLocalVars(aProc: ProcRef);
var sym: SymblRef;
begin
	sym := aProc^.vars.first;
	while sym <> nil do
	begin
		if sym^.symType.baseType = StringType then
			initLocalString(sym)
		else if typeContainsString(sym^.symType)
				and not (sym^.isVarParam or sym^.isParam) then
			clearLocalVar(sym);

		sym := sym^.next;
	end;
end;

function getReturnVar(aProc: ProcRef): SymblRef;
var sym: SymblRef;
begin
	sym := findSymbol(aProc^.vars, aProc^.name);
	if sym = nil then
		errorExit2('internal error: returnVar not found', aProc^.name)
	else
		getReturnVar := sym;
end;


(* call storeArg for each entry in the parameter list
	in reverse order, using recursion *)
procedure reverseArgs(sym: SymblRef);
begin
	if sym^.next <> nil then
		reverseArgs(sym^.next);
	storeArg(sym);
end;

procedure fetchProcedureArgs(aProc: ProcRef);
var sym: SymblRef;
begin
	if aProc^.returnsAggregate then
		storeArg(getReturnVar(aProc));

	sym := aProc^.parameters.first;
	if sym <> nil then reverseArgs(sym)
end;

procedure parseParameterList(var aProc: ProcRef);
var forwardParam:SymblRef;
begin
	forwardParam := aProc^.parameters.first;

	while not checkToken(RParenToken) do
	begin
		if not (curToken.tokenKind in [ IdentToken, VarToken ]) then
			errorExit2('Expected identifier, got', curToken.tokenText);
		parseParameter(aProc, forwardParam);
		if checkToken(SemicolonToken) then
		begin
			readNextToken;
			if not (curToken.tokenKind in [ IdentToken, VarToken ]) then
				errorExit2('Expected identifier, got', curToken.tokenText)
		end
		else
		if not checkToken(RParenToken) then
			errorExit2('Expected ; or ), got', curToken.tokenText);
	end;
end;

procedure parseLabelBlock;
begin

	repeat
		readNextToken;
		matchToken(IdentToken);
		addLabel(curProcedure, lastToken.tokenText);
	until not checkToken(CommaToken);
	matchToken(SemicolonToken);
end;

procedure parseProcOrFunc;
begin
	if checkToken(ProcedureToken) then
		parseProcedure
	else if checkToken(FunctionToken) then
		parseFunction
	else
		errorExit2('Expected PROCEDURE or FUNCTION, got', curToken.tokenText);
end;

procedure parseProcOrFuncBody(aProc: ProcRef; returnVar: SymblRef);
begin
	if checkToken(ExternalToken) then
	begin
		(* for an externally declared function, no
			code is emitted *)
		readNextToken;
	end
	else
	if checkToken(ForwardToken) then
	begin
		(* for a forward declaration, just set the isForward flag *)
		aProc^.isForward := true;
		readNextToken;
	end
	else
	begin
		aProc^.isForward := false; (* If there was a forward declaration,
			we are using its aProc record. Set the isForward field to false
			for that case, to prevent multiple procedure statements. *)

		(* parse var, type, const, label statements,
		 	also nested procedures *)
		parseProgramBlock;

		emitProcedurePrologue(aProc);
		fetchProcedureArgs(aProc);
		initLocalVars(aProc);
		parseCompoundStatement;

		if aProc^.hasExit then
			emitExitLabel(aProc);

		if returnVar <> nil then
		begin
			(*  if return value is an aggregate,
				returnVar is a var parameter. in this case,
				nothing needs to be done here, because
				the value has already been set at the
				destination.
				we still return the pointer to the destination
				so it can be evaluated without any special handling. *)
			emitFunctionValueReturn(returnVar);
		end;
		emitProcedureEpilogue(aProc);
	end;
end;

procedure parseFunction;
var aProc, previousProc: ProcRef;
    name: IdentString;
    sym:  SymblRef;
	returnType: TypeSpec;
	returnsAggregate: boolean;
begin
	readNextToken;

	name := curToken.tokenText;

	previousProc := curProcedure;
	aProc := addProcedure(name, true, previousProc);
	curProcedure := aProc;

	readNextToken;
	if checkToken(LParenToken) then
	begin
		readNextToken;
		parseParameterList(aProc);
		matchToken(RParenToken);
	end;

	(* parse return type declaration *)
	matchToken(ColonToken);
	parseTypeSpec(returnType, false);
	matchToken(SemicolonToken);

	(* If we parse the function the second time after
		a forward declaration, we must not add
		the result variable a second time.*)
	if not aProc^.isForward then
	begin
		(* add function name as local variable for return value *)
		(*  if the return value is an aggregate,
			make the return value a var parameter *)
		returnsAggregate := isAggregate(returnType);
		sym := addSymbol(aProc^.vars, name, returnType, false, returnsAggregate);

		aProc^.returnsAggregate := returnsAggregate;
		aProc^.returnType := sym^.symType;
	end
	else
		(* take the return var from forward declaration *)
		sym := findSymbol(aProc^.vars, name); 

	parseProcOrFuncBody(aProc, sym);

	curProcedure := previousProc;
end;

procedure parseProcedure;
var aProc, previousProc: ProcRef;
begin
	readNextToken;

	previousProc := curProcedure;
	aProc := addProcedure(curToken.tokenText, false, previousProc);
	curProcedure := aProc;

	readNextToken;
	if checkToken(LParenToken) then
	begin
		readNextToken;
		parseParameterList(aProc);
		matchToken(RParenToken);
	end;
	matchToken(SemicolonToken);

	parseProcOrFuncBody(aProc, nil);

	curProcedure := previousProc;
end;

procedure parseVarParam(var typeReturn: TypeSpec);
var mem: MemLocation;
begin
	parseMemLocation(true, mem);	(* put memory loc of variable on stack *)
	typeReturn := mem.typ;
	if (mem.typ.baseType = StringType) and (not mem.initialized) then
	begin
		emitInitStringShort(mem.typ.stringLength);
		mem.initialized := true;
		{ FIXME: the following causes a bug with string
			initialization if the string is passed
			as a var parameter in a nested procedure.
			why was this needed in the first place? }
		{ mem.origSym^.initialized := true; }
	end;
end;

function isFunction(aProc: ProcRef): boolean;
begin
	isFunction := aProc^.returnType.baseType <> NoType;
end;

procedure parseNew;
var memLoc: MemLocation;
    typeReturn: TypeSpec;
begin
	matchToken(LParenToken);

	parseLvalue(memLoc);
	matchBaseType(memLoc.typ,PointerType);

	if memLoc.typ.pointedType^.baseType = StringType then
	begin
		if checkToken(CommaToken) then
		begin
			readNextToken;
			parseExpression(typeReturn);
			matchBaseType(typeReturn, IntegerType);
		end
		else
			emitLoadConstantInt(memLoc.typ.pointedType^.stringLength);
		emitStringAlloc;
	end
	else
	begin
		emitLoadConstantInt(memLoc.typ.pointedType^.size);
		emitMemAlloc;

		if typeContainsString(memLoc.typ.pointedType^) then
			emitClearAlloc(memLoc.typ.pointedType);
	end;
	emitCheckAlloc;

	(*We need to call CLEARMEM when the allocated type
		contains strings.
		INITSTRING checks if the header is non-zero to see if
		the string is already initialized, and the allocated
		chunk might contain random data so it would look
		like an initialized string. *)

	writeVariable(memLoc);

	matchToken(RParenToken);
end;

procedure parseDispose;
var memLoc: MemLocation;
begin
	matchToken(LParenToken);
	parseMemLocation(false, memLoc);
	matchBaseType(memLoc.typ, PointerType);
	readVariable(memLoc);
	emitMemFree;
	matchToken(RParenToken);
end;

function isFileVariable(var name:IdentString):boolean;
var sym:SymblRef;
begin
	sym := findHieraSymbol(name);
	if sym = nil then
		errorExit2('Undeclared variable', name);
	isFileVariable := isSameType(sym^.symType, fileTyp);
end;

(* Parse optional width and precision specifications
   for str and write.
   Count specifies the possible number of specs (1 or 2).
   If a spec is not there,
   a zero is put onto the stack for each missing
   spec.
   *)
procedure parseFieldSpecs(var argType:TypeSpec);
var specType:TypeSpec;
	max, i:integer;
begin
	if argType.baseType in
			[StringType, IntegerType, BooleanType, PointerType, EnumType ] then
		max := 1
	else
	if argType.baseType = RealType then
		max := 2
	else
	if argType.baseType = CharType then
		max := 0
	else
		max := 0;

		(* Chars should also have a field width by
			the standard, but that's not very useful
			and it slows things down.
			We could call a different routine if a field
			width is specified. We could do that for all
			types of course. *)

	if max > 0 then
	begin
		if checkToken(ColonToken) then
		begin
			readNextToken;
			parseExpression(specType);
			matchBaseType(specType, IntegerType);
			if max = 2 then
			begin
				if checkToken(ColonToken) then
				begin
					readNextToken;
					parseExpression(specType);
					matchBaseType(specType, IntegerType);
				end
				else
					emitLoadConstantInt(0);
			end
			else
				if checkToken(ColonToken) then
					errorExit2('Fraction length not allowed' , '');
		end
		else
			for i := 1 to max do
				emitLoadConstantInt(0);
	end;
end;

procedure writeByType(var typ:TypeSpec);
begin
	emitWriteFileArg;
    parseFieldSpecs(typ);

	if typ.baseType = StringType then
		emitWrite('STRING')
	else
	if typ.baseType = CharType then
		emitWrite('CHAR')
	else
	if typ.baseType = RealType then
		emitWrite('REAL')
	else
	if typ.baseType in [ IntegerType, BooleanType, PointerType, EnumType ] then
		emitWrite('INT')
	else  (* everything else is raw binary*)
		emitWriteWords(typ.size);
end;

procedure parseWrite(newline:boolean);
var	typeReturn: TypeSpec;
	isFirst:boolean;
	hasFileArg:boolean;
	count:integer;
begin
	count := 0;
	if matchTokenOrNot(LParenToken) then (* can be empty and have no parentheses *)
	begin
		if not checkToken(RParenToken) then (* can be empty inside parentheses *)		
		begin
			isFirst := true;
			hasFileArg := false;
			repeat
				parseExpression(typeReturn);
				if isFirst then
				begin
					if isSameType(typeReturn, fileTyp) then
					begin
						(* File var address is on stack now
							from parseExpression.
						*)
						hasFileArg := true;
						emitCheckError;
					end
					else
					begin
						(* the first arg is already on stack and it needs to be written *)
						emitDefaultOutput;
					end;
				end;

				(* ignore the first arg if it is a file arg *)
				if not (isFirst and hasFileArg) then
					writeByType(typeReturn);

				isFirst := false;
				count := count + 1;
			until not matchTokenOrNot(CommaToken);
			if newline then
				emitWriteNewline;
			emitWriteEnd;
		end;
		matchToken(RParenToken);
	end;
	if (count = 0) and newline then
		emitDefaultNewline;
end;

procedure readByType(var mem:MemLocation);
begin
	emitReadFileArg;

	if mem.typ.baseType = CharType then
	begin
		(* freadchar is a special case, it returns
			a char value on the estack to make it a
			bit faster. The address of the destination variable
			has already been put on the stack by parseMemLocation,
			so we do a writeVariable to store the result. *)
		emitRead('CHAR');
		writeVariable(mem);
	end
	else
	begin
	(* For all other types, the address of the variable
			is passed as a var parameter. *)
		if mem.typ.baseType = StringType then
		begin
			if not mem.initialized then
				emitInitStringSwapped(mem.typ.stringLength);
			emitRead('STRING');
		end
		else
		if mem.typ.baseType = RealType then
			emitRead('REAL')
		else
		if mem.typ.baseType in [IntegerType, BooleanType, PointerType] then
			emitRead('INT')
		else
			emitReadWords(mem.typ.size);
	end;
end;

procedure parseRead(newline:boolean);
var mem: MemLocation;
	isFirst:boolean;
	hasFileArg:boolean;
	count:integer;
begin
	count := 0;
	if matchTokenOrNot(LParenToken) then (* can be empty and have no parentheses *)
	begin
		if not checkToken(RParenToken) then (* can be empty inside parentheses *)
		begin
			isFirst := true;
			hasFileArg := false;
			repeat
				parseMemLocation(true, mem); (* get destination memLoc, force indirect *)
				if isFirst then
				begin
					if isSameType(mem.typ, fileTyp) then
					begin
						(* File var address is on stack now
							from parseLvalue.
						*)
						hasFileArg := true;
						(* for read/write, we generate a call to checkerror,
							because otherwise when reading/writing multiple
							variables, we get a runtime error if the first
							read/write gets an error and a second variable
							is being read/written.
							But we want to be able to check with
							IOResult.
							For other file operations (e.g. seek),
							the code in stdlib does the checkerror call.
							*)
						emitCheckError;
					end
					else
					begin
						emitDefaultInput;
					end;
				end;
				(* ignore the first arg if it is a file arg *)
				if not (isFirst and hasFileArg) then
					readByType(mem);

				isFirst := false;
				count := count + 1;
			until not matchTokenOrNot(CommaToken);
			if newline then
				emitReadNewline
			else
				emitReadEnd;
			matchToken(RParenToken);
		end;
	end;
	if (count = 0) and newline then
		emitReadDefaultNewline;
end;
procedure parseSimpleSP(var typeReturn: TypeSpec);
begin
	readNextToken;
	matchToken(LParenToken);
	parseExpression(typeReturn);
	matchToken(RParenToken);
end;

procedure parseSetLength;
var argType: TypeSpec;
begin
	matchToken(LParenToken);
	parseExpression(argType);
	matchBaseType(argType, StringType);
	matchToken(CommaToken);
	parseExpression(argType);
	matchBaseType(argType, IntegerType);
	matchToken(RParenToken);
	emitSetStringLength;
end;

procedure parseSimpleSF(var typeReturn: TypeSpec);
begin
	matchToken(LParenToken);
	parseExpression(typeReturn);
	matchToken(RParenToken);
end;

procedure parseChr(var typeReturn: TypeSpec);
begin
	parseSimpleSF(typeReturn);
	matchBaseType(typeReturn, IntegerType);
	setBaseType(typeReturn, CharType);
end;

procedure parseOrd(var typeReturn: TypeSpec);
begin
	parseSimpleSF(typeReturn);
	if not (typeReturn.baseType in
		[ CharType, EnumType, BooleanType, IntegerType ]) then
		errorExit2('invalid argument type for ORD', '');
	(* no code is required, just the type conversion *)
	setBaseType(typeReturn, IntegerType);
end;

procedure parseOdd(var typeReturn: TypeSpec);
begin
	parseSimpleSF(typeReturn);
	matchBaseType(typeReturn, IntegerType);
	setBaseType(typeReturn, BooleanType);
	emitOdd;
end;

procedure parseAbs(var typeReturn: TypeSpec);
begin
	parseSimpleSF(typeReturn);
	if typeReturn.baseType = IntegerType then
		emitAbsInt
	else
	if typeReturn.baseType = RealType then
		emitAbsFloat32
	else
		errorExit2('Integer or real type required for ABS', '');
end;

procedure parseTrunc(var typeReturn: TypeSpec);
begin
	parseSimpleSF(typeReturn);
	matchBaseType(typeReturn, RealType);
	emitTruncFloat;
	setBaseType(typeReturn, IntegerType);
end;

procedure parseFrac(var typeReturn: TypeSpec);
begin
	parseSimpleSF(typeReturn);
	matchBaseType(typeReturn, RealType);
	emitFractFloat;
end;

procedure parseInt(var typeReturn: TypeSpec);
begin
	parseSimpleSF(typeReturn);
	matchBaseType(typeReturn, RealType);
	emitIntFloat;
end;

procedure parseSucc(var typeReturn: TypeSpec);
begin
	parseSimpleSF(typeReturn);
	if typeReturn.baseType in [ IntegerType, CharType ] then
		emitInc(1)
	else
	if typeReturn.baseType = EnumType then
	begin
		emitInc(1);
		emitEnumCheck(typeReturn.enumLength - 1);
	end
	else
		errorExit2('Integer, char or enum type expected', '');
end;

procedure parsePred(var typeReturn: TypeSpec);
begin
	parseSimpleSF(typeReturn);
	if typeReturn.baseType in [ IntegerType, CharType ] then
		emitDec(1)
	else
	if typeReturn.baseType = EnumType then
	begin
		emitDec(1);
		emitEnumCheck(typeReturn.enumLength - 1);
	end
	else
		errorExit2('integer, char or enum type expected', '');
end;

procedure parseSqr(var typeReturn: TypeSpec);
begin
	parseSimpleSF(typeReturn);
	if typeReturn.baseType = IntegerType then
		emitSqrInt
	else
	if typeReturn.baseType = RealType then
		emitSqrFloat
	else
		errorExit2('integer or real argument expected for sqr', '');
end;

procedure parseValSP;
var valType:TypeSpec;
	codeType:TypeSpec;
    strType:TypeSpec;
begin
	matchToken(LParenToken);

	parseExpression(strType); (* first arg must be a string *)
	matchBaseType(strType, StringType);

	matchToken(CommaToken);

	parseVarParam(valType); (* this can be integer or real *)

	matchToken(CommaToken);

	parseVarParam(codeType); (* the return code must be integer *)

	if valType.baseType = IntegerType then
		emitValCall('INT')
	else
	if valType.baseType = RealType then
		emitValCall('REAL')
	else
		errorExit2('Expected INTEGER or REAL variable','');

	matchBaseType(codeType, IntegerType);

	matchToken(RParenToken);
end;

procedure parseStrSP;
var numType:TypeSpec;
	argType:TypeSpec;
begin
	matchToken(LParenToken);
	parseExpression(numType);

	parseFieldSpecs(numType);

	matchToken(CommaToken);
	parseExpression(argType); (* FIXME: use parseVarParam *)
	matchBaseType(argType, StringType);

	if numType.baseType = IntegerType then
		emitStrCall('INT')
	else
	if numType.baseType = RealType then
		emitStrCall('REAL')
	else
		errorExit2('Invalid argument type close to', lastToken.tokenText);

	matchToken(RParenToken);
end;

procedure parseExitSP;
begin
	(* check for optional empty parentheses *)
	if matchTokenOrNot(LParenToken) then
		(* we do not support parameters for exit() *)
		matchToken(RParenToken);

	if not curProcedure^.hasExit then
		curProcedure^.hasExit := true;
	emitExit(curProcedure);
end;

procedure spNotImplemented;
begin
	errorExit2('special procedure/function not implemented:', lastToken.tokenText);
end;

procedure parseSpecialFunction(sf: SpecialFunc; var returnType: TypeSpec);
begin
	case sf of
	NoSF:
		errorExit2('internal error in parseSpecialFunction:', curToken.tokenText);
	TruncSF:
		parseTrunc(returnType);
	FracSF:
		parseFrac(returnType);
	IntSF:
		parseInt(returnType);
	SqrSF:
		parseSqr(returnType);
	SuccSF:
		parseSucc(returnType);
	PredSF:
		parsePred(returnType);
	OddSF:
		parseOdd(returnType);
	ChrSF:
		parseChr(returnType);
	OrdSF:
		parseOrd(returnType);
	AbsSF:
		parseAbs(returnType);
	end;
end;

procedure parseSpecialProcCall(sp: SpecialProc);
begin
	case sp of
	NoSP:
		errorExit2('internal error in parseSpecialProcCall', lastToken.tokenText);
	NewSP:
		parseNew;
	DisposeSP:
		parseDispose;
	ReadSP:
		parseRead(false);
	WriteSP:
		parseWrite(false);
	ReadlnSP:
		parseRead(true);
	WritelnSP:
		parseWrite(true);
	SetlengthSP:
		parseSetLength;
	ValSP:
		parseValSP;
	StrSP:
		parseStrSP;
	ExitSP:
		parseExitSP;
	(* TODO: inc() and dec() *)
	end;
end;

procedure parseProcedureCall(var name: IdentString);
var aProc: ProcRef;
	noMemLocation: MemLocation;
	sp: SpecialProc;
begin
	sp := NoSP;
	readNextToken;
	aProc := searchProcedure(name);

	if aProc = nil then (* no procedure found, try special procedures *)
		sp := findSpecialProcedure(name);
	
	if (aProc = nil) and (sp = NoSP) then
			(* neither regular nor special procedure, error *)
			errorExit2('Undeclared identifier', name);

	if sp <> NoSP then
		parseSpecialProcCall(sp)
	else
	begin
		if isFunction(aProc) then
			errorExit2('function cannot be called as a procedure:', name);

		initNoMemLocation(noMemLocation);
		parseCall(aProc, noMemLocation);
	end;
end;

function markTemporaries(aProc: ProcRef): integer;
begin
	markTemporaries := aProc^.tempsSize;
end;

procedure allocTemporary(aProc: ProcRef;
	var typ: TypeSpec; var memLocReturn: MemLocation);
begin
	aProc^.tempsSize := aProc^.tempsSize + typ.size;
	memLocReturn.memLoc := TemporaryMem;
	memLocReturn.offset := aProc^.tempsSize;
	memLocReturn.name := '<tmp>';
	memLocReturn.typ := typ;
	memLocReturn.initialized := false;
end;

procedure releaseTemporaries(aProc: ProcRef; offset: integer);
begin
	aProc^.tempsSize := offset;
end;


(*
	If the procedure has an aggregate return value,
	you can pass a MemLocation for the return variable
	so it can be used in an aggregate assignment without using
	a temporary.
	Otherwise,  you should pass a MemLocation instance where
	the memLoc field is set to NoMem. A temporary is then created
	and its MemLocation passed back in optionalDest.
	FIXME: this is not implemented? A temporary is always created.
	
	Argument passing: Args are passed on the eval stack.

	If a nested procedure is called, a pointer to the parent's
	stack frame is passed as an invisible first arg
	(needs to be stored at offset 0).

	For aggregate returns, a temporary is allocated and
	passed as a invisible var parameter at the last arg
	position. This is used by the called function as the return
	variable.
	*)

procedure parseCall(aProc: ProcRef; var optionalDest: MemLocation);
var arg: SymblRef;
    typeReturn: TypeSpec;
	tempRetval: MemLocation;
	retvalVar: SymblRef;

begin
	initNoMemLocation(tempRetval);

	arg := aProc^.parameters.first;

	if arg = nil
	then
	begin
		if checkToken(LParenToken) then
		begin
			readNextToken;
			matchToken(RParenToken);
		end
	end
	else
	begin
		matchToken(LParenToken);
		repeat
			(* FIXME: dont convert for var params - why?*)
			if arg^.isVarParam then
				parseVarParam(typeReturn)
			else
			if arg^.symType.baseType = SetType then
			(* special handling of sets for set literals *)
				parseSetExpression(typeReturn)
			else
				parseExpression(typeReturn);
			(* TODO: release temporaries after each parameter *)
			matchAndConvertTypes(arg^.symType, typeReturn);
			if arg^.symType.hasSubrange then
				emitSubrangeCheck(arg^.symType.subStart, arg^.symType.subEnd);
			arg := arg^.next;
			if arg <> nil then
				matchToken(CommaToken);
		until arg = nil;
		matchToken(RParenToken);
	end;

	(* if the called function returns an aggregate, allocate a temporary
		and pass it as an invisible arg.
		this arg is passed last and becomes the return value local variable. *)
	if aProc^.returnsAggregate then
	begin
		retvalVar := getReturnVar(aProc);
		(* allocate space on program stack *)
		allocTemporary(curProcedure, retvalVar^.symType, tempRetval);
		optionalDest := tempRetVal;
		(* string temporaries need to be initialized *)
		if retvalVar^.symType.baseType = StringType then
			initTemporaryString(tempRetval);
		(* put the address of the temporary on the stack *)
		emitLoadTempAddr(tempRetval.name, tempRetval.offset);
	end;

	emitProcedureCall(aProc);

	{
	if aProc^.returnsAggregate then
		writeln('***** parseCall returnsAggregate ', aProc^.returnsAggregate);
	}
end;

(* parse the right hand side of an assignment and generate code *)
procedure parseAssignmentPart(sym: SymblRef; var mem: memLocation);
var  typeReturn: TypeSpec;
begin
	if mem.typ.baseType = StringType
	then
		(* we need to pass the memLoc here because the
			result is directly written to the destination
			in the optimized case *)
		parseStringExpression(mem)
	else if mem.typ.baseType = SetType
	then
	begin
		(* parsing a set expression leaves a word on the stack
			so we don't need a memLoc and explicitly call
			writeVariable here *)
		parseSetExpression(typeReturn);
		writeVariable(mem);
	end
	else
	begin
		parseExpression(typeReturn);
		matchAndConvertTypes(mem.typ, typeReturn);
		writeVariable(mem);
	end;
end;

(* parse a complete assignment statement and generate code *)
procedure parseAssignment(sym: SymblRef);
var  mem: memLocation;
begin
	parseLvalue(mem);
	matchToken(AssignmentToken);
	parseAssignmentPart(sym, mem);
end;

procedure initConstListItem(var value:ConstListItem);
begin
	value.next       := nil;
	value.name       := '';
	value.arrayValue := nil;
	value.strValue   := nil;
	value.enumRef    := nil;
end;

procedure getConstValue(var value:ConstListItem);
var digits:string[24];
	typ:TypeSpec;
	cnst:ConstRef;
	newStr: ConstStrRef;
begin
	if checkToken(NumberToken) or checkToken(MinusToken) then
	begin
		getNumber(digits, typ);

		if typ.baseType = IntegerType then
			value.intValue := integerFromString(digits)
		else if typ.baseType = RealType then
			value.realValue := realFromString(digits)
		else
			errorExit2('internal error getConstValue', digits);
	end
	else if checkToken(StringLitToken) then
	begin
		setBaseType(typ, StringType);
		newStr := addConstStr(curToken.tokenText);
		value.strValue := newStr;
		readNextToken;
	end
	else if checkToken(CharLitToken) then
	begin
		setBaseType(typ, CharType);
		(* char constants are stored as integer *)
		value.intValue := getCharValue;
	end
	else if checkToken(TrueToken) or checkToken(FalseToken) then
	begin
		setBaseType(typ, BooleanType);
		(* boolean constants are stored as integer *)
		value.intValue := ord(getBooleanValue);
	end
	else if checkToken(IdentToken) then
	begin
		cnst := findConstantHiera(curToken.tokenText);
		if cnst = nil then
			errorExit2('Constant expected, got', curToken.tokenText);
		(* copy all relevant fields *)
		value.typ := cnst^.typ;
		value.realValue  := cnst^.realValue;
		value.intValue   := cnst^.intValue;
		value.arrayValue := cnst^.arrayValue;
		value.strValue   := cnst^.strValue;
		value.enumRef    := cnst^.enumRef;
		typ := value.typ;
		readNextToken;
	end
	else
		errorExit2('Constant value expected, got', curToken.tokenText);
	value.typ := typ;
end;

procedure getStringValue(var dest:KeywordString);
var cnst:ConstRef;
begin
	if checkToken(IdentToken) then
	begin
		cnst := findConstantHiera(curToken.tokenText);
		if cnst = nil then
			errorExit2('String constant expected, got', curToken.tokenText);
		if cnst^.typ.baseType <> StringType then
			errorExit2('String constant expected, got', curToken.tokenText);
		dest := cnst^.strValue^.value;
		readNextToken;
	end
	else
	begin
		if not matchTokenOrNot(CharLitToken) then
			matchToken(StringLitToken);
		dest := lastToken.tokenText;
	end;
end;

(* encode a constant value of a simple type in our most basic type (integer)
	which is used to store constant data for variable initializations *)
function encodeConstValue(var constValue:ConstListItem): integer;
begin
		if constValue.typ.baseType = RealType then
		(* this makes the assumption that a real fits into an integer *)
			encodeConstValue := encodefloat32(constValue.realValue)
		else
			encodeConstValue := constValue.intValue;
end;

procedure parseArrayLitValue(constData: ArrayConstRef; var typ: TypeSpec);
var count, endCount: integer;
begin
	matchToken(LParenToken);
	endCount := typ.arrayLength;
	for count := 1 to endCount do
	begin
		parseConstValue(constData, typ.elementType^);
		if count < endCount then
			matchToken(CommaToken);
	end;
	matchToken(RParenToken);
end;

procedure parseRecordLitValue(constData: ArrayConstRef; var typ: TypeSpec);
var curField:FieldRef;
begin
	matchToken(LParenToken);

	curField := typ.fields;
	while curField <> nil do
	begin
		if curField^.isVariant then
			errorExit2('variant records cannot be initialized','');

		if isSimpleType(curField^.fieldType) 
			or (curField^.fieldType.baseType = StringType) then
		begin
			parseConstValue(constData, curField^.fieldType);
		end
		else
		if curField^.fieldType.baseType = ArrayType then
			parseArrayLitValue(constData, curField^.fieldType)
		else
			errorExit2('invalid record field initialization for',
				curField^.name);
		curField := curField^.next;
		if curField <> nil then
			matchToken(CommaToken);
	end;

	matchToken(RParenToken);
end;

procedure parseConstValue(constData: ArrayConstRef; var expectedType: TypeSpec);
var constValue:ConstListItem;
	strConst: KeywordString;
begin
			if expectedType.baseType = StringType then
			begin
				getStringValue(strConst);
				addStrConstElem(constData, strConst, expectedType.stringLength);
			end
			else
			if expectedType.baseType = ArrayType then
				parseArrayLitValue(constData, expectedType)
			else
			if expectedType.baseType = RecordType then
				parseRecordLitValue(constData, expectedType)
			else
			begin
				initConstListItem(constValue);
				getConstValue(constValue);
				matchTypes(constValue.typ, expectedType);
				addArrayConstElem(constData, encodeConstValue(constValue));
			end;
end;

procedure parseVarInitialization(sym:SymblRef);
var baseType: SymbolType;
	constValue:ConstListItem;
	constData:ArrayConstRef;
	first: boolean;
begin
	first := true;

	if curProcedure <> mainProcedure then
		errorExit2('Only global variables can be initialized:', sym^.name);
	
	baseType := sym^.symType.baseType;

	if baseType = StringType then
	begin
		(* strings with initialization data are handled like arrays
			with opaque data *)
		constData := addNamedArrayConst(sym^.name, first);
		parseConstValue(constData, sym^.symType);
	end
	else if isSimpleType(sym^.symType) then
	begin
		initConstListItem(constValue);
		getConstValue(constValue);
		sym^.initialValue := constValue.intValue;
	end
	else if baseType = ArrayType then
	begin
		constData := addNamedArrayConst(sym^.name, first);
		parseArrayLitValue(constData, sym^.symType);
	end
	else if baseType = RecordType then
	begin
		constData := addNamedArrayConst(sym^.name, first);
		parseRecordLitValue(constData, sym^.symType);
	end
	else
		errorExit2('internal error in parseVarInitialization: invalid baseType for',
			sym^.name);

	sym^.hasInitialValue := true;
end;

procedure parseSingleVarStatement;
var name: IdentString;
    sym: SymblRef;
	names: StringList;
	typSpec: TypeSpec;
	hasNext: boolean;
	isExternal: boolean;
begin
	(* first, gather list of variable names *)
	initStringList(names);
	repeat
		matchToken(IdentToken);
		addToStringList(names, lastToken.tokenText);
	until not matchTokenOrNot(CommaToken);
	matchToken(ColonToken);

	parseTypeSpec(typSpec, false);

	(* handle initialization *)
	if checkToken(EqToken) then
	begin
		readNextToken;

		hasNext := nextStringListItem(names, name);
		if not hasNext then
			errorExit2('internal error when parsing var statement','');
		sym := addSymbol(curProcedure^.vars, name, typSpec, false, false);
		parseVarInitialization(sym);
		(* check if there is more than one variable *)
		hasNext := nextStringListItem(names, name);
		if hasNext then
			errorExit2('Cannot initialize multiple variables:',name);
	end
	else
	begin
		(* if external keyword follows after the type spec, it is
			an external variable *)
		isExternal := matchTokenOrNot(ExternalToken);
		(* create variables with the declared type from the list of names *)
		while(nextStringListItem(names, name)) do
		begin
			sym := addSymbol(curProcedure^.vars, name, typSpec, false, false);
			if isExternal then
				if curProcedure = mainProcedure
				then
					sym^.isExternal := true
				else
					errorExit2('Local variable cannot be declared external', name);
		end;
	end;

	disposeStringList(names);
end;

procedure parseVarBlock;
begin
	matchToken(VarToken);
	while checkToken(IdentToken) do
	begin
		parseSingleVarStatement;
		matchToken(SemicolonToken);
	end;
end;

procedure parseForInStatement(var sym:SymblRef;forNo:integer);
var containerType:TypeSpec;
    mem: MemLocation;
	elementMem: MemLocation;
begin
	matchToken(InToken);
	(* parseMemLocation(true, container); containerType := container^.typ; *)
	parseExpression(containerType);
	(* TODO: would be nice if for-in worked with enum types *)
	if containerType.baseType = ArrayType then
	begin
		matchTypes(containerType.elementType^, sym^.symType);
		matchToken(DoToken);

		emitForInHeader(containerType.arrayLength);
		emitForInStart(forNo);
		accessVariable(sym, mem);
		elementMem.memLoc := Indirect;
		elementMem.typ := containerType.elementType^;
		elementMem.origSym := nil;
		elementMem.name := '<in-loop-element>';
		elementMem.offset := 0;
		elementMem.scopeDistance := 0;
		elementMem.initialized := false;
		emitForInMid(sym, mem);
		readVariable(elementMem);
		writeVariable(mem);

		parseCompoundStatement;

		emitForInIter(forNo, containerType);
		emitForInEnd(forNo);
	end
	else
	if containerType.baseType = StringType then
	begin
		matchBaseType(sym^.symType, CharType);
		matchToken(DoToken);

		emitForInStrHeader;
		emitForInStart(forNo);
		accessScalar(sym, mem);
		emitForInStrMid(sym, mem);
		writeVariable(mem);

		parseCompoundStatement;

		emitForInStrIter(forNo);
		emitForInEnd(forNo);
	end
	else
		errorExit2('Array or string expected', lastToken.tokenText);
end;

procedure parseForStatement;
var sym: SymblRef;
	name: IdentString;
	typeReturn: TypeSpec;
	mem: MemLocation;
	tmpCount: integer;
	down: boolean;
	prevBreakLabel:IdentString;
begin
	readNextToken;
	tmpCount := forCount;
	forCount := forCount + 1;
	prevBreakLabel := curBreakLabel;
	curBreakLabel := getForEndLabel(tmpCount);
	name := curToken.tokenText;
	sym := findHieraSymbol(name);
	if sym = nil then
		errorExit2('Undeclared variable', name);
	readNextToken;
	if checkToken(InToken) then
		parseForInStatement(sym, tmpCount)
	else
	begin
		matchToken(AssignmentToken);
		if not (sym^.symType.baseType in [ IntegerType, CharType, BooleanType,
			 EnumType ]) then
				errorExit2('Invalid type for loop variable', sym^.name);
		accessScalar(sym, mem); (* FOR initializer *)
		parseAssignmentPart(sym, mem);
		
		if not (curToken.tokenKind in [ ToToken, DowntoToken ]) then
			errorExit2('Expected TO or DOWNTO, got', curToken.tokenText);
		down := checkToken(DowntoToken);
		readNextToken;

		parseExpression(typeReturn);     (* FOR end condition is kept on stack *)
		matchTypes(typeReturn, sym^.symType);
	
		emitForStart(tmpCount);

		(* read and check loop variable *)
		accessScalar(sym, mem);
		readVariable(mem);
		if down then
			emitForDowntoBranch(tmpCount)
		else
			emitForBranch(tmpCount);
		(* We need to check for a subrange at the start of the loop, not
		   at the end. After the last iteration the control
		   variable will be out of range, so we cannot do the subrange
		   check there. *)
		if (sym^.symType.baseType = IntegerType) and
			(sym^.symType.hasSubrange) then
		begin
			(* need to read the variable again *)
			accessScalar(sym, mem);
			readVariable(mem);
			emitSubrangeCheckRaw(sym^.symType.subStart, sym^.symType.subEnd);
		end;

		matchToken(DoToken);
		parseCompoundStatement;	(* FOR body *)
		accessScalar(sym, mem);		(* increment counter variable *)
		accessScalar(sym, mem);		(* load mem loc twice for write and read *)
		readVariable(mem);
		if down then
			emitDec(1)
		else
			emitInc(1);
		writeVariable2(mem, false);
		emitForEnd(tmpCount);	(* branch to beginning of loop *)
	end;
	curBreakLabel := prevBreakLabel;
end;

procedure parseIfStatement;
var tmpCount: integer;
	typeReturn: TypeSpec;
begin
	readNextToken;
	tmpCount := ifCount; (* local copy of the if counter to allow for nested ifs *)
	ifCount := ifCount + 1;
	parseExpression(typeReturn);
	matchBaseType(typeReturn, BooleanType);
	matchToken(ThenToken);
	emitIfBranch(tmpCount);
	parseCompoundStatement;
	if matchTokenOrNot(ElseToken) then
	begin
		emitElseBranch(tmpCount);
		emitElseLabel(tmpCount);
		parseCompoundStatement;
	end
	else
		emitElseLabel(tmpCount);
	emitIfLabel(tmpCount);
end;

procedure parseWhileStatement;
var tmpCount: integer;
	typeReturn: TypeSpec;
	prevBreakLabel: IdentString;
begin
	readNextToken;
	tmpCount := whileCount;
	whileCount := whileCount + 1;
	prevBreakLabel := curBreakLabel;
	curBreakLabel := getWhileEndLabel(tmpCount);
	emitWhileStart(tmpCount);
	parseExpression(typeReturn);
	matchBaseType(typeReturn, BooleanType);
	emitWhileBranch(tmpCount);
	matchToken(DoToken);
	parseCompoundStatement;
	emitWhileEnd(tmpCount);
	curBreakLabel := prevBreakLabel;
end;

procedure parseRepeatStatement;
var tmpCount: integer;
	typeReturn: TypeSpec;
	prevBreakLabel: IdentString;
begin
	readNextToken;
	tmpCount := repeatCount;
	repeatCount := repeatCount + 1;
	prevBreakLabel := curBreakLabel;
	curBreakLabel := getRepeatEndLabel(tmpCount);
	emitRepeatStart(tmpCount);
	repeat
		parseStatement;
	until matchEndOf(UntilToken);
	parseExpression(typeReturn);
	matchBaseType(typeReturn, BooleanType);
	emitRepeatBranch(tmpCount);
	emitRepeatEnd(tmpCount);
	curBreakLabel := prevBreakLabel;
end;

procedure parseCaseStatement;
var tmpCount, caseLabelCount, caseSubValCount: integer;
	selectorType, caseType: TypeSpec;
begin
	readNextToken;
	tmpCount := caseCount;
	caseCount := caseCount + 1;
	caseLabelCount := 0;
	
	parseExpression(selectorType);  (* parse case selector *)
	matchToken(OfToken);

	emitCaseStart(tmpCount);
	repeat
		caseSubValCount := 0;
		repeat
			(* emit the label which is used by the previous case clause if
				it does not match *)
			emitCaseLabelStart(tmpCount, caseLabelCount, caseSubValCount);
			parseConstant(caseType);
			if matchTokenOrNot(DotToken) then
			begin
				if matchTokenOrNot(DotToken) then
				begin
					(* handle ranges which use two comparisons *)
					emitCaseRangeLoBranch(tmpCount, caseLabelCount, caseSubValCount, true);
					parseConstant(caseType);
					emitCaseRangeHiBranch(tmpCount, caseLabelCount, caseSubValCount,
						not checkToken(CommaToken));
				end
			end
			else
				emitCaseLabelBranch(tmpCount, caseLabelCount, caseSubValCount,
					not checkToken(CommaToken));
			matchTypes(selectorType, caseType);
			caseSubValCount := caseSubValCount + 1;
		until not matchTokenOrNot(CommaToken);
		matchToken(ColonToken);
		(* this label is used for clauses with multiple values to jump to on a match *)
		emitCaseLabelMatch(tmpCount, caseLabelCount);
		(* parse the (compound) statement which is executed on a match *)
		parseCompoundStatement;
		emitCaseLabelEnd(tmpCount);
		(* last normal clause may omit the semicolon, otherwise it is required *)
		if not (curToken.tokenKind in [ EndToken, ElseToken]) then
			matchToken(SemicolonToken);
		(* emit label to catch the last conditional branch of a multi-value clause *)
		emitCaseLabelLabel(tmpCount, caseLabelCount, caseSubValCount);
		caseLabelCount := caseLabelCount + 1;

		(* check for a final ELSE clause *)
		if checkToken(ElseToken) then
		begin
			readNextToken;
			(* just generate the code, which will be put after
				the last no-match-label and therefore will be
				executed if the last clause does not match *)
			parseCompoundStatement;
			if checkToken(SemicolonToken) then readNextToken;
			if not checkToken(EndToken) then
				errorExit2('ELSE must be last case clause', curToken.tokenText);
		end;
	until matchTokenOrNot(EndToken);
	emitCaseEnd(tmpCount, caseLabelCount);
end;

procedure parseBreakStatement;
begin
	if length(curBreakLabel) = 0 then
		errorExit2('BREAK not within loop', '');
	emitBreak(curBreakLabel);
	readNextToken;
end;

procedure disposeWithStmntTmp;
begin
	with withStmntStack[withStmntCount] do
	begin
		if 	tmpSymbol <> nil then
		begin
			dispose(tmpSymbol);
			tmpSymbol := nil;
		end;
	end;
end;

procedure parseWithStmntPart;
var withLoc, tLoc: MemLocation;
	tempType: TypeSpec;
begin
	parseMemLocation(true,withLoc);	(* parse the memory location of the record to be opened *)
	(* allocate a temporary for the address of the record *)
	setBaseType(tempType, PointerType);
	allocTemporary(curProcedure, tempType, tLoc);

	(* add it to the with-stack *)
	withStmntCount := withStmntCount + 1;
	if withStmntCount > WithStackDepth then
		errorExit2('Too many nested WITH statements','');
	withStmntStack[withStmntCount].tmpSymbol := nil;

	with withStmntStack[withStmntCount] do
	begin
		recordLoc := withLoc; (* the memloc of the opened record *)
		tempLoc   := tLoc;    (* the memloc of the temporary which stores the
								address of the opened record *)
	end;

	(* store the record address to the temporary *)
	emitLoadTempAddr(withLoc.name, tLoc.offset);
	emitSwap;
	emitStoreIndirect;
end;

procedure parseWithStatement;
var	tempMark: integer;
    oldWithStmntCount: integer;
begin
	tempMark := markTemporaries(curProcedure);
	oldWithStmntCount := withStmntCount;

	readNextToken;

	(* the with clause can contain multiple comma separated records *)
	repeat
		parseWithStmntPart;
	until not matchTokenOrNot(CommaToken);

	matchToken(DoToken);

	parseCompoundStatement;

	(* remove entries from with-stack *)
	while withStmntCount > oldWithStmntCount do
	begin
		disposeWithStmntTmp;
		withStmntCount := withStmntCount -1;
	end;
	
	releaseTemporaries(curProcedure,tempMark);
end;

procedure parseLabel(var aLabl:LablRef);
begin
	emitLabel(aLabl);
	readNextToken;
	matchToken(ColonToken);
end;

procedure parseStatement;
var sym: SymblRef;
	cnst: ConstRef;
    aLabl: LablRef;
    name: IdentString;
	tempMark: integer;
begin
	(* temporaries used during the statement
		can be released afterwards, so mark
		the temp space now and release later *)
	tempMark := markTemporaries(curProcedure);

	(* try to parse a label before every statement.
	   if we succeed, continue to parse (because there is no
	   	semicolon after a label, so it is not a complete statement)
	*)
	if checkToken(IdentToken) then
	begin
		name := curToken.tokenText;
		aLabl := findLabel(curProcedure, name);
		if aLabl <> nil then
			parseLabel(aLabl);
	end;

	if checkToken(GotoToken) then
	begin
		readNextToken;
		matchToken(IdentToken);
		aLabl := findLabel(curProcedure, lastToken.tokenText);
		if aLabl = nil then errorExit2('GOTO to undefined label', lastToken.tokenText);
		emitLabelJump(aLabl);
	end
	else
	if checkToken(IfToken) then
		parseIfStatement
	else
	if checkToken(WhileToken) then
		parseWhileStatement
	else
	if checkToken(RepeatToken) then
		parseRepeatStatement
	else
	if checkToken(ForToken) then
		parseForStatement
	else
	if checkToken(BreakToken) then
		parseBreakStatement
	else
	if checkToken(CaseToken) then
		parseCaseStatement
	else
	if checkToken(WithToken) then
		parseWithStatement
	else
	if checkToken(IdentToken) then
	begin
		(* this can be either a procedure call or an assignment *)
		name := curToken.tokenText;
		sym := findHieraSymbol(name);
		if sym <> nil
		then
			parseAssignment(sym)
		else
		begin
			(* check if it is a constant *)
			cnst := findConstantHiera(name);
			if cnst <> nil then
				errorExit2('variable identifier expected, got constant', name);

			(* now it can only be a procedure *)
			parseProcedureCall(name);
		end;
	end
	else
	begin
		if (curToken.tokenKind = ElseToken) then
		begin
			(* two consecutive else tokens mean an empty else clause followed by another*)
			if	not (lastToken.tokenKind in [ElseToken, ThenToken]) then
				errorExit2('Unexpected ELSE, check for erroneous ; after previous statement',
				 '')
		end
		else if not (curToken.tokenKind in [SemicolonToken, EndToken, UntilToken ]) then
		begin
		    (* For an empty statement, the semicolon or end token is not consumed.
			   If not an empty statement, it is an error. *)
			errorExit2('Unexpected token', quoteToken(keywords[curToken.tokenKind]));
		end;
	end;

	releaseTemporaries(curProcedure, tempMark);
end;

procedure parseRecordField(var recordTyp: TypeSpec; var offset:integer;
	isVariant:boolean; tagField:FieldRef; var tagValues:IntList);
var fieldType: TypeSpec;
	fieldName: IdentString;
	curField: ^FieldListItem;
	newField: ^FieldListItem;
	names: StringList;
begin
	initStringList(names);
	repeat
		addToStringList(names, curToken.tokenText);
		matchToken(IdentToken);
	until not matchTokenOrNot(CommaToken);
	matchToken(ColonToken);

	parseTypeSpec(fieldType, false);

	curField := recordTyp.fields;
	(* go to last field in list *)
	if curField <> nil then
		while curField^.next <> nil do curField := curField^.next;

	while(nextStringListItem(names, fieldName)) do
	begin

		new(newField);
		newField^.name := fieldName;
		newField^.offset := offset;
		newField^.fieldType := fieldType;
		newField^.isVariant := isVariant;
		newField^.tagField  := tagField;
		newField^.tagValues := tagValues;
		newField^.next := nil;

		if curField = nil then
			recordTyp.fields := newField
		else
			curField^.next := newField;

		curField := newField;

		offset := offset + fieldType.size;
	end;

	disposeStringList(names);
end;

procedure parseRecordFields(var recordTyp: TypeSpec; var offset:integer;
	isVariant:boolean; tagField:FieldRef; var tagValues:IntList);
begin
	while checkToken(IdentToken) do
	begin
		parseRecordField(recordTyp, offset, isVariant, tagField, tagValues);
		if checkToken(SemicolonToken) then
			readNextToken;
	end;
end;

procedure parseVariantRecord(var recordTyp: TypeSpec; var offset:integer);
var tagField:FieldRef;
	tagValue:integer;
	tagValueType:TypeSpec;
	variantOffset:integer;
	maxSize:integer;
	caseValues: IntList;
begin
	matchToken(CaseToken);

	parseRecordField(recordTyp, offset, false, nil, emptyIntList);
	(* get the tag field which was just added at then end of the list *)
	tagField := recordTyp.fields;
	while tagField^.next <> nil do tagField := tagField^.next;

	matchToken(OfToken);

	maxSize := 0;
	repeat
		variantOffset := offset;
		initIntList(caseValues);
		(* there can be a comma-separated list of case values *)
		repeat
			getRangePart(tagValue, tagValueType);
			addToIntList(caseValues, tagValue);
		until not matchTokenOrNot(CommaToken);
		matchToken(ColonToken);
		matchToken(LParenToken);
		parseRecordFields(recordTyp, variantOffset, true, tagField, caseValues);
		if variantOffset > maxSize then
			maxSize := variantOffset;
		matchToken(RParenToken);
		matchToken(SemicolonToken);
		{
		while nextIntListItem(caseValues, tagValue) do
			writeln('******* parseVariantRecord case values:', tagValue);
		}
		rewindIntList(caseValues);

		(* the caseValues list is not disposed, it stays attached to
			the field list of the record type *)
	until checkToken(EndToken);
	offset := maxSize;
end;

procedure parseRecordDecl(var newTypeName:IdentString);
var offset: integer;
    recordTyp: TypeSpec;
begin
	offset := 0;

	setBaseType(recordTyp, RecordType);
	recordTyp.fields := nil;

	matchToken(RecordToken);
	repeat
		if checkToken(CaseToken) then
			parseVariantRecord(recordTyp, offset)
		else
			parseRecordField(recordTyp, offset, false, nil, emptyIntList);

		if checkToken(SemicolonToken) then
			readNextToken
		else if not checkToken(EndToken) then
			errorExit2('Expected ; or END, got', curToken.tokenText);
	until checkToken(EndToken);
	readNextToken;

	recordTyp.size := offset;

	addType(recordTyp, newTypeName);
end;

procedure parseEnumDecl(var name:IdentString;var typeReturn: TypeSpec);
var ident: IdentString;
    value: integer;
    cnst:  ConstRef;
	enumTyp: TypeSpec;
	enumRef: TypeRef;
	identList: StringList;
begin
	value := 0;
	initStringList(identList);

	setBaseType(enumTyp, EnumType);
	addType(enumTyp, name);
	enumRef := findTypeRef(curProcedure, name);

	matchToken(LParenToken);
	repeat
		ident := curToken.tokenText;
		matchToken(IdentToken);
		addToStringList(identList, ident);
		value := value + 1;
	until not matchTokenOrNot(CommaToken);
	matchToken(RParenToken);

	enumCount := enumCount + 1;

	value := 0;
	while nextStringListItem(identList, ident) do
	begin
		cnst := addConstant(ident);
		cnst^.typ.baseType := EnumType;
		cnst^.typ.enumId := enumCount;
		cnst^.intValue := value;
		cnst^.enumRef := enumRef;
		value := value + 1;
	end;

	enumRef^.typePtr^.enumLength := value;
	enumRef^.typePtr^.enumList := identList;
	enumRef^.typePtr^.enumId := enumCount;

	typeReturn := enumRef^.typePtr^;
end;

procedure parseTypeStatement;
var newTypeName: IdentString;
	newType: TypeSpec;
begin
	(* newType.baseType := NoType; *)
	newTypeName := curToken.tokenText;
	matchToken(IdentToken);
	matchToken(EqToken);
	optionalToken(PackedToken);
	if checkToken(RecordToken) then (* TODO: move to parseTypeSpec*)
		parseRecordDecl(newTypeName)
	else
	if checkToken(LParenToken) then (* TODO: move to parseTypeSpec*)
		parseEnumDecl(newTypeName, newType)
	else
	begin
		parseTypeSpec(newType, false);
		addType(newType, newTypeName);
	end;
end;

procedure parseTypeBlock;
begin
	matchToken(TypeToken);
	while checkToken(IdentToken) do
	begin
		parseTypeStatement;
		matchToken(SemicolonToken);
	end;
end;

procedure parseConstBlock;
var name:IdentString;
	typeReturn:TypeSpec;
	newConst: ConstRef;
begin
	matchToken(ConstToken);
	repeat
		matchToken(IdentToken);
		name := lastToken.tokenText;
		matchToken(EqToken);

		newConst := addConstant(name);

		if checkToken(LBracketToken) then
		begin
			setBaseType(typeReturn, NoType);
			newConst^.arrayValue := getArrayConst(typeReturn);
			newConst^.typ := typeReturn;
		end
		else
			getConstValue(newConst^);

		matchToken(SemicolonToken);
	until not checkToken(IdentToken);
end;

procedure processUnresolvedTypes(aProc:ProcRef);
var typeListItem, t:TypeRef;
    typePtr: ^TypeSpec;
begin
	typeListItem := aProc^.unresolved;
	while typeListItem <> nil do
	begin
		typePtr := typeListItem^.typePtr;
		if typePtr^.baseType = UnresolvedType then
		begin
			t := findTypeRef(aProc, typePtr^.typeName^);
			if t = nil then
			begin
				errorExit2('unresolved type', typePtr^.typeName^);
			end
			else
			begin
				(* overwrite the unresolved type spec with the one we just found *)
				typeListItem^.typePtr^ := t^.typePtr^;
			end;
		end;
		typeListItem := typeListItem^.next;
	end;
end;

procedure parseProgramBlock;
begin
	(* parse var, type and const statements *)
	while curToken.tokenKind in [ VarToken, TypeToken, ConstToken, LabelToken ] do
	begin
		if checkToken(VarToken) then parseVarBlock
		else if checkToken(TypeToken) then parseTypeBlock
		else if checkToken(ConstToken) then parseConstBlock
		else if checkToken(LabelToken) then parseLabelBlock
	end;

	processUnresolvedTypes(curProcedure);

	(* parse functions and procedures *)
	while checkToken(ProcedureToken) or checkToken(FunctionToken) do
	begin
			parseProcOrFunc;
			matchToken(SemicolonToken);
	end;
end;

procedure parseLib(n:IdentString);
var libFile: InputFileType;
    prevFile: InputFileType;
    prevLineno: integer;
    prevFilename: string[255];
	newFilename: string[255];
begin
	prevFile := infile;
	prevLineno := lineno;
	prevFilename := filename;

	newFilename := n + UnitSuffix1;
	openFileWithDefault(libFile, newFilename);

	filename := newFilename;
	infile := libFile;
	lineno := 1;
	buffered := false;

	readNextToken;

	parseProgramBlock;

	if not checkToken(EOFToken) then
		errorExit2('Expected <end-of-file>','');

	close(libFile);

	printLineStats;

	infile := prevFile;
	lineno := prevLineno;
	filename := prevFilename;

	buffered := false;
end;

procedure parseStdLib;
var name:IdentString;
begin
	parseLib(StdlibName);

	(* the file type is declared in stdlib, so
		we can look it up now *)
	name := 'FILE';
	fileTyp := findType(mainProcedure, name);
end;

procedure setGlobalSuffix;
begin
	globalSuffix := '_' + mainProcedure^.name;
end;

procedure parseUnit;
begin
	matchToken(UnitToken);
	matchToken(IdentToken);
	mainProcedure^.name := lastToken.tokenText;
	setGlobalSuffix;

	matchToken(SemicolonToken);

	matchToken(ImplementationToken);

	parseProgramBlock;

	matchToken(EndToken);
	matchToken(DotToken);
	matchToken(EOFToken);
end;

function lower(c:char):char;
begin
	if (ord(c) >= ord('A')) and
		(ord(c) <= ord('Z')) then
		lower := chr(ord(c) + 32) (* assumes ASCII*)
	else
		lower := c;
end;

procedure parseUsesStatement;
var unitName:IdentString;
    c:char;
begin
	repeat
		matchToken(IdentToken);
		unitName := '';
		for c in lastToken.tokenText do
			unitName := unitName + lower(c);
		addToStringList(usedUnits, unitName);
	until not matchTokenOrNot(CommaToken);

	if not checkToken(SemicolonToken) then
		matchToken(SemicolonToken);

	while nextStringListItem(usedUnits, unitName) do
		parseLib(unitName);

	readNextToken; (* read token from main input file *)
end;

procedure parseProgram;
begin
	(* require Program statement *)
	matchToken(ProgramToken);
	matchToken(IdentToken);
	mainProcedure^.name := lastToken.tokenText;
	(* we don't do anything with the program name *)
	(* and we parse but otherwise ignore file declarations *)
	if matchTokenOrNot(LParenToken) then
	begin
		repeat
			matchToken(IdentToken);
		until not matchTokenOrNot(CommaToken);
		matchToken(RParenToken);
	end;
	matchToken(SemicolonToken);

	if matchTokenOrNot(UsesToken) then
			parseUsesStatement;

	(* parse var, type and const statements and procedures/functions *)
	parseProgramBlock;

	(* parse main program *)
	emitMainStart();
	parseCompoundStatement;
	matchToken(DotToken);

	(* nothing should be after the main program *)
	matchToken(EOFToken);
end;

procedure parseProgramOrUnit(useStdlib:boolean);
begin
	if useStdlib then
		parseStdlib;

	readNextToken;

	if checkToken(ProgramToken) then
	begin
		emitPrologue;
		parseProgram;
		emitEpilogue;
	end
	else
	if checkToken(UnitToken) then
	begin
		parseUnit;
		emitUnitEpilogue;
	end
	else
		errorExit2('PROGRAM or UNIT expected, got', curToken.tokenText);

end;

function changeFileSuffix(filename: string): string;
var suffixPos:integer;
begin
	suffixPos := pos(filenameSuffix, filename);
	if suffixPos > 0 then
		setlength(filename, suffixPos-1);
	filename := filename + outfileSuffix;
	changeFileSuffix := filename;
end;

procedure initMainProcedure;
begin
	mainProcedure := addProcedure('_MAIN', false, nil);
	mainProcedure^.vars.scope := GlobalSymbol;
	mainProcedure^.procedures := nil;
	mainProcedure^.next := nil;
	mainProcedure^.types := nil;
	mainProcedure^.unresolved := nil;
	mainProcedure^.constants := nil;
	mainProcedure^.level := -1;
	curProcedure := mainProcedure;
end;

begin
	initPlatform;

	buffered := false;
	firstConstStr := nil;
	firstArrayConst := nil;
	constStrNo := 0;
	arrayConstNo := 0;
	ifCount := 0;
	whileCount := 0;
	forCount := 0;
	repeatCount := 0;
	caseCount := 0;
	nestedProcsCount := 0;
	enumCount := 0;
	anonTypeCount := 0;
	curBreakLabel := '';
	lineno := 1;
	includeLevel := 0;
	defaultHeapSize := 262144;
	defaultStackSize := 16384;
	withStmntCount := 0;
	insCount := 0;
	initStringList(usedUnits);
	initIntList(emptyIntList);
	initMainProcedure;
	globalSuffix := '';
	useStdlib := true;
	useStandalone := false;
	editOnError := false;
	runProg := false;
	runAsm := true;
	paramPos := 1;

	filename := '';
	outfilename := '';

	while paramPos <= paramCount do
	begin
		if paramStr(paramPos) = '-n' then (* do not include stdlib.inc *)
			useStdlib := false
		else
		if paramStr(paramPos) = '-s' then (* use standalone corelib *)
			useStandalone := true
		else
		if paramStr(paramPos) = '-e' then (* call editor on error *)
			editOnError := true
		else
		if paramStr(paramPos) = '-R' then (* run compiled/assembled program *)
			runProg := true
		else
		if paramStr(paramPos) = '-S' then (* do not run assembler *)
			runAsm := false
		else
		if paramStr(paramPos) = '-H' then (* set heap size *)
		begin
			paramPos := paramPos + 1;
			DefaultHeapSize := integerFromString(ParamStr(paramPos)) * 1024;
		end
		else
		begin
			if length(filename) = 0 then
				filename := paramStr(paramPos)
			else
				outfilename := paramStr(paramPos);
		end;
		paramPos := paramPos + 1;
	end;

	if length(outfilename) = 0 then
		outfilename := changeFileSuffix(filename);

	if length(filename) = 0 then
	begin
		writeln('No file name given.');
		halt;
	end;

	writeln('Compiling ', filename, ' to ', outfilename);
	openFileWithDefault(infile, filename);

	overwriteFile(outfile, outfilename);

	parseProgramOrUnit(useStdlib);

	printLineStats;

	cleanup;

	if runAsm then
		ExecAssembler(outfilename, runProg, editOnError);
end.
