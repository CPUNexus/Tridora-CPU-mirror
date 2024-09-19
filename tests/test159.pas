program test159;
var s:string[131072];
    i:integer;
    c:char;
    buf:string;
begin
	writeln('creating test string...');
	c := 'A';
	for i := 1 to maxlength(s) do
	begin
		appendchar(s,c);
		c := succ(c);
		if c = 'z' then
			c := 'A';
	end;

	writeln('string length: ', length(s));

	writeln(s[1], s[2], s[3]);

	writeln('moving stuff...');
	repeat
		write('>');
		readln(buf);
		strmoveup(s, 1,100000,1);
		writeln(s[1], s[2], s[3]);
	until buf = 'x';
end.
