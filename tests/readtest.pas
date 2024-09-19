program readtest;
var filename:string;
    buf:char;
    f:file;
    count:integer;
    t:DateTime;
begin
	write('Enter filename: ');
	readln(filename);

	t := GetTime;
	writeln('start:', TimeStr(t, true));
	open(f, filename, ModeReadOnly);

	count := 0;
	while not eof(f) do
	begin
		read(f,buf);
		count := count + 1;
	end;
	close(f);
	t := GetTime;
	writeln('end:', TimeStr(t, true));
	writeln(count, ' bytes read.');
end.
