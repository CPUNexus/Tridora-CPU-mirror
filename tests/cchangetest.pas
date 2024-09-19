program cchangetest;
var c:char;
begin
	repeat
		writeln('cardchanged: ', cardchanged);
		read(c);
	until c = #27;
end.
