(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
procedure openTextFile(var f:TextFile; filename:string);
begin
	open(f, filename, ModeReadOnly);
	if IOResult(f) <> 0 then
		errorExit2('cannot open file', filename);
end;

procedure overwriteTextFile(var f:TextFile; filename:string);
begin
	open(f, filename, ModeOverwrite);
end;
