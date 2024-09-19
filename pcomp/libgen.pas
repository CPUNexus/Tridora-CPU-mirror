(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
program libgen;

const	shortcutChar = '`';
	firstShCChar = 'A';
	lastShCChar = 'i';

	OutfileSuffix = '.lib';

{$I 'platfile-types+.pas'}

type
	InsString = string[24];

var shortcuts:array [firstShCChar..lastShCChar] of InsString;
    infile:TextFile;
    outfile:TextFile;
    infileName:string;
    outfileName:string;
    lineCount:integer;

procedure errorExit2(reason:string;arg:string); forward;

{$I 'platfile+.pas'}

procedure errorExit2(reason:string;arg:string);
begin
	writeln;
	writeln('Error: ', reason, ' ', arg);
	halt;
end;

procedure addShortcut(ch:char; dest:InsString);
begin
	shortcuts[ch] := dest;
end;

function findShortcut(ins:InsString):char;
var ch:char;
begin
	findShortCut := #0;

	for ch := firstShCChar to lastShCChar do
	begin
		if shortcuts[ch] = ins then
		begin
			findShortcut := ch;
			break;
		end;
	end;
	{ if findShortCut = #0 then writeln('findShortcut:#0'); }
end;

procedure initShortcuts;
begin
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

procedure processLine(var linebuf:string);
var labelEnd:integer;
    dotPos:integer;
    endPos:integer;
    insStart:integer;
    insEnd:integer;
    labelBuf:string;
    insBuf:string;
    restBuf:string;
    short:char;

procedure scanLine;
var c:char;
    i:integer;
begin
	labelEnd := 0;
	dotPos := 0;
	endPos := 0;
	insStart := 0;
	insEnd := 0;

	i := 1;
	for c in linebuf do
	begin
		if (labelEnd = 0) and (c = ':') then
		begin
			insStart := 0;
			insEnd := 0;
			labelEnd := i;
		end
		else
		if (dotPos = 0) and (c = '.') then
		begin
			insEnd := i - 1;
			dotPos := i;
		end
		else
		if c = ';' then break
		else
		if c in [ ' ', #9 ] then
		begin
			if (insStart <> 0 ) and (insEnd = 0) then
				insEnd := i - 1;
		end
		else
		if c in [ '''', '"' ] then
		begin
			(* we do not want to deal with string quoting,
				so if we encounter some quotes,
				just do nothing *)
			insStart := 0;
			insEnd := 0;
			labelEnd := 0;
			endPos := length(linebuf);
			break;
		end
		else
		begin
			if insStart = 0 then
				insStart := i;
			endPos := i;
			{ writeln('c:', c, ' i:', i, ' insStart:', insStart); }
		end;

		i := i + 1;
	end;
	if insEnd = 0 then insEnd := endPos;
end;

begin
	if length(linebuf) > 0 then
		if linebuf[1] <> '%' then
	begin
		scanLine;
		if labelEnd > 0 then
			labelBuf := copy(linebuf,1,labelEnd)
		else
			labelBuf := '';

		if insStart > 0 then
			insBuf := copy(linebuf, insStart, insEnd - insStart + 1)
		else
			insBuf := '';

		if endPos <> insEnd then
			restBuf := copy(linebuf, insEnd + 1, endPos - insEnd + 1)
		else
			restBuf := '';
	{
		writeln('ins ', insBuf);
		writeln('label ', labelBuf);
		writeln('rest ', restBuf);
		writeln('insStart ', insStart);
		writeln('insEnd ', insEnd);
		writeln('dotPos ', dotPos);
		writeln('endPos ', endPos);
	}
		short := #0;
		if length(insBuf) > 0 then
		begin
			(* if we found an instruction, try to find a shortcut *)
			short := findShortcut(insBuf);

			if short <> #0 then
				writeln(outfile, labelBuf, '`', short, restBuf)
			else
				(* if no shortcut, we still remove comments and whitespace *)
				writeln(outfile, labelBuf, ' ', insBuf, restBuf);
		end
		else
			(* no instruction found, probably a directive, so
				no change *)
			writeln(outfile, linebuf);
	end
	else
		writeln(outfile, linebuf);
end;

procedure processAllLines;
var linebuf:string;
begin
	while not eof(infile) do
	begin
		readln(infile, linebuf);
		lineCount := lineCount + 1;
		if (lineCount and 255) = 1 then
			write(lineCount, ' lines', #13);
		processLine(linebuf);
	end;
	writeln(lineCount, ' lines');
end;

procedure test;
var buf:string;
begin
	outfile := output;

	buf := 'LABEL: SOMEINS.MOD1.MOD2 ARG ; a comment';
	processLine(buf);
	buf := '  SOMEINS.MOD1.MOD2 ARG ; a comment';
	processLine(buf);
	buf := '	LOADCP 1';
	processLine(buf);
	buf := '	JUMP';
	processLine(buf);
	buf := 'LABEL:   FPADJ -20';
	processLine(buf);
	buf := 'LABEL:   .BYTE ":;123"';
	processLine(buf);
	buf := 'LABEL:   .LCBRANCH SOMEWHERE';
	processLine(buf);
	buf := 'LABEL:   LOADC '';''';
	processLine(buf);
end;

function changeSuffix(var fname:string):string;
var dotPos:integer;
    found:boolean;
begin
	found := false;

	for dotPos := length(fname) downto 1 do
		if fname[dotPos] = '.' then
		begin
			found := true;
			break;
		end;

	if found then
		changeSuffix := copy(fname,1, dotPos - 1) + OutfileSuffix
	else
		changeSuffix := fname + OutfileSuffix;
end;

begin
	initShortcuts;

	{ test;
	halt; }

	outfileName := '';

	case ParamCount of
	0:	begin
			write('Source file: ');
			readln(infileName);
		end;
	1:	infileName := ParamStr(1);
	2:	begin
			infileName := ParamStr(1);
			outfileName := ParamStr(2);
		end
	else
		begin
			writeln('Invalid arguments.');
			halt;
		end;
	end;

	if length(outfileName) = 0 then
		outfileName := changeSuffix(infileName);

	writeln('Output file: ', outfileName);

	openTextFile(infile, infileName);
	overwriteTextFile(outfile, outfileName);

	processAllLines;

	close(infile);
	close(outfile);
end.
