(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
procedure ClrScr;
begin
	write(#27, '[2J');
	write(#27, '[H');
end;

procedure ClrEol;
begin
	write(#27, '[K');
end;

procedure CrtInit;
begin
	write(#27, 'c');
end;

procedure GotoXY(x,y:integer);
begin
	write(#27,'[', y, ';', x, 'H');
end;

procedure InsLine;
begin
	write(#27,'[L');
end;

procedure DelLine;
begin
	write(#27,'[M');
end;

procedure GetCursorPos(var x,y:integer); external; (* from corelib.s *)

procedure GetTermSize(var maxx,maxy:integer);
var x,y:integer;
begin
	GetCursorPos(x,y);
	GotoXY(9999,9999);
	GetCursorPos(maxx,maxy);
	GotoXY(x,y);
end;

procedure TextColor(col:integer);
begin
	write(#27,'[38;5;',col,'m');
end;

procedure TextBackground(bgcol:integer);
begin
	write(#27,'[48;5;',bgcol,'m');
end;

procedure TextDefault;
begin
	write(#27,'[0m');
end;
