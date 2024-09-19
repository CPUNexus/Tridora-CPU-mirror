(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
procedure openTextFile(var f:TextFile; filename:string);
begin
{$I-}
	assign(f, filename);
	reset(f);

	if IOResult <> 0 then
		errorExit2('cannot open file', filename);
{$I+}
end;

procedure overwriteTextFile(var f:TextFile; filename:string);
begin
	assign(f, filename);
	rewrite(f);
end;
