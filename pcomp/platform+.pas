(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
procedure initPlatform;
begin
	outputPrefix := '';
	includePrefix := '../lib/';
end;

procedure newString(var s:StringRef;len:integer);
begin
	new(s);
end;

procedure openFileWithDefault(var f:InputFileType; filename:string);
begin
{$I-}
	assign(f, filename);
	reset(f);

	if IOResult <> 0 then
	begin
		assign(f, includePrefix + '/' + filename);
		reset(f);
		if IOResult <> 0 then
			errorExit2('cannot open file', filename);
	end;
{$I+}
end;

procedure overwriteFile(var f:OutputFileType; filename:string);
begin
	assign(f, outputPrefix + filename);
	rewrite(f);
end;

function isdigit(aChar:char):boolean;
begin
	isdigit := (ord(aChar) >= ord('0')) and (ord(aChar) <= ord('9'));
end;

procedure ExecEditor(var filename:string; lineno:integer; errormsg:string);
begin
	halt;
end;

procedure ExecAssembler(var filename:string; doRun:boolean; editOnError:boolean);
begin
	halt;
end;

procedure ExecProgram(var filename:string);
begin
	halt;
end;
