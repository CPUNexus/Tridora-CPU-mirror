program viewpict;
type PictData = record
		magic, mode:integer;
		palette: array [0..15] of integer;
		pixeldata: array [0..31999] of integer;
	end;


var pic:PictData;
    filename:string;
    infile:file;
    ch:char;

procedure loadPalette(var pic:PictData);
var i:integer;
begin
	for i := 0 to 15 do
		setpalette(i, pic.palette[i]);
end;

procedure loadPic(var pic:PictData);
begin
	PutScreen(pic.pixeldata);
end;

begin
	if ParamCount > 0 then
		filename := ParamStr(1)
	else
	begin
		write('Filename> ');
		readln(filename);
	end;

	open(infile, filename, ModeReadonly);
	read(infile, pic);
	close(infile);

	writeln('magic: ', pic.magic, ' mode:', pic.mode);

	loadPalette(pic);
	loadPic(pic);
	read(ch);
end.
