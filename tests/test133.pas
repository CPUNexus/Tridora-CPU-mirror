program test133;
var f:file;
    buf:string;
begin
	open(f, 'newfile.text', ModeOverwrite);
	writeln(f,'This is a test file created by a Pascal program.');
	writeln(f,'There is nothing else of interest here.');
	close(f);

	open(f, 'newfile.text', ModeReadonly);
	while not eof(f) do
	begin
		readln(f,buf);
		writeln(buf);
	end;
	close(f);
end.
