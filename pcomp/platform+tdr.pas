(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
procedure initPlatform;
begin
	outputPrefix := '';
	includePrefix := '#SYSTEM:';
end;

procedure newString(var s:StringRef;len:integer);
begin
	new(s,len);
end;

procedure openFileWithDefault(var f:InputFileType; filename:string);
begin
	open(f, filename, ModeReadOnly);
	if IOResult(f) <> 0 then
	begin
		open(f, includePrefix + filename, ModeReadOnly);
		if IOResult(f) <> 0 then
			errorExit2('cannot open file', filename);
	end;
end;

procedure overwriteFile(var f:OutputFileType; filename:string);
begin
	open(f, outputPrefix + filename, ModeOverwrite);
end;

procedure printExecErr(filename:string; error:integer);
begin
	writeln('PExec failed for ', filename, ': ', ErrorStr(error));
end;

procedure ExecEditor(var filename:string; lineno:integer; errormsg:string);
var args:PArgVec;
    error:integer;
    digits:string[12];
begin
	str(lineno, digits);
	args[0] := '-l'; args[1] := digits;
	args[2] := '-E'; args[3] := errormsg;
	args[4] := filename;
	PExec('#SYSTEM:editor.prog', args, 5, error);
	printExecErr('#SYSTEM:editor.prog', error);
end;

procedure ExecAssembler(var filename:string; doRun:boolean; editOnError:boolean);
var args:PArgVec;
    argPos:integer;
    error:integer;
begin
	if doRun then
	begin
		args[0] := '-R';
		argPos := 1;
	end
	else
		argPos := 0;
	if editOnError then
	begin
		args[argPos] := '-e';
		argPos := argPos + 1;
	end;
	args[argPos] := filename;
	PExec('#SYSTEM:sasm.prog', args, argPos + 1, error);
	printExecErr('#SYSTEM:editor.prog', error);
end;

procedure ExecProgram(var filename:string);
var args:PArgVec;
	error:integer;
begin
	writeln('Running ', filename, '...');
	PExec(filename, args, 0, error);
	printExecErr(filename, error);
end;
