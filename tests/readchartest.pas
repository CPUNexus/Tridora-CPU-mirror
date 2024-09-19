program readchartest;
var c:char;
    kbd:file;
begin
	open(kbd, '%KBD', ModeReadonly);
	while true do
	begin
		read(kbd, c);
		writeln(ord(c));
	end;
	close(kbd);
end.
