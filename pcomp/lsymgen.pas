(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
program lsymgen;

const OutfileSuffix = '.lsym';

{$I 'platfile-types+.pas'}

var
    outfile:TextFile;
    infile:TextFile;
    lineno:integer;

procedure errorExit2(reason:string;arg:string); forward;

{$I 'platfile+.pas'}

procedure errorExit2(reason:string;arg:string);
begin
	writeln;
	writeln('Error: ', reason, ' ', arg);
	writeln('at ', lineno);
	halt;
end;

function rpos(c:char; var s:string):integer;
var i:integer;
begin
	for i := length(s) downto 1 do
		if s[i] = '.' then break;
	if i = 1 then
		rpos := 0
	else
		rpos := i;
end;

function strcontains(var s:string; c:char):boolean;
begin
	strcontains := pos(c, s) > 0;
end;

function getOutfileName(infileName:string):string;
var p:integer;
begin
	p := rpos('.', infileName);
	if p > 1 then
		getOutfileName := copy(infileName, 1, p - 1)
	else
		getOutfileName := infileName;

	getOutfileName := getOutfileName + OutfileSuffix;
end;

procedure splitLine(var line:string; var addr:string; var name:string;
		var clean:boolean);
var n,l:integer;
begin
	n := pos(' ', line);

	if n <= 1 then
		errorExit2('invalid syntax:', line);

	addr := copy(line, 1, n - 1);

	l := length(line);
	while (n < l) and (line[n] = ' ') do
		n := n + 1;

	name := copy(line, n, l - n + 1);


	(* symbols starting with '!' are explicitly exported *)
	if name[1] = '!' then
	begin
		clean := true;
		name := copy(name, 2, length(name) - 1);
	end
	else
		clean := (not strcontains( name, '_')) and (name[1] <> '=');
end;

procedure processFile(inpath,outpath:string);
var line:string;
    addr,name:string;
    clean:boolean;
begin
	lineno := 0;
	writeln('writing file ', outpath);

	openTextFile(infile, inpath);

	overwriteTextFile(outfile, outpath);

	while not eof(infile) do
	begin
		readln(infile, line);
		splitLine(line, addr, name, clean);
		if clean then
			writeln(outfile, #9, '.EQU ', name, ' $', addr);
	end;
	close(infile);
	close(outfile);
end;

begin
	if ParamCount > 0 then
	begin
		processFile(ParamStr(1), getOutfileName(ParamStr(1)));
	end
	else
		writeln('No file name given.');
end.
