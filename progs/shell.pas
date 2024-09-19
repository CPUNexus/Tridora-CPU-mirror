(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
program shell;
const EDITORPROG = '#SYSTEM:editor.prog';
      COMPILERPROG = '#SYSTEM:pcomp.prog';
      ASMPROG = '#SYSTEM:sasm.prog';
      RECLAIMPROG = '#SYSTEM:reclaim.prog';

const PageMargin = 3;
      MenuHeight = 6;

var cmd:char;
    ShellWorkfile:pathnamestr external;
    ShellCmd:string[40] external;
    ShellArg:integer external;

procedure checkClock;
var line:string;
    digits:string[4];
    error:integer;
    yy,mm,dd,h,m,s:integer;
    isValid:boolean;

function lineIsValid:boolean;
var c:char;
begin
	lineIsValid := false;

	if length(line) = 14 then
	begin
		for c in line do
			if not isDigit(c) then
				break;
		lineIsValid := true;
	end;
end;

function isInRange(v,lo,hi:integer):boolean;
begin
	isInRange := (v>=lo) and (v<=hi);
end;

begin
	if SysClock.year = 0 then
	begin
		writeln('System clock not set - please enter date and time:');
		repeat
			isValid := false;
			write('YYYYMMDDHHMMSS> ');
			readln(line);

			if lineIsValid then
			begin
				isValid := true;
				digits := copy(line,1,4);
				val(digits, yy, error);
				isValid := isValid and isInRange(yy, 1950, 3000);

				digits := copy(line,5,2);
				val(digits, mm, error);
				isValid := isValid and isInRange(mm, 1, 12);

				digits := copy(line,7,2);
				val(digits, dd, error);
				isValid := isValid and isInRange(dd, 1, 31);

				digits := copy(line,9,2);
				val(digits, h, error);
				isValid := isValid and isInRange(h, 0, 23);

				digits := copy(line,11,2);
				val(digits, m, error);
				isValid := isValid and isInRange(m, 0, 59);

				digits := copy(line,13,2);
				val(digits, s, error);
				isValid := isValid and isInRange(s, 0, 59);
			end;
		until isValid;

		SysClock.year := yy;
		SysClock.month := mm;
		SysClock.day := dd;
		SysClock.hours := h;
		SysClock.minutes := m;
		SysClock.seconds := s;

		writeln('System clock is ', DateStr(SysClock), ' ', TimeStr(SysClock, true));
	end;
end;

procedure writew(s:string;width:integer);
var w,i:integer;
begin
	write(s);
	w := width - length(s);
	if w > 0 then
		for i := 1 to w do
			write(' ');
end;

procedure splitFilename(var n:filenamestr;
	var basename:filenamestr;var extension:filenamestr);
var i:integer;
begin
	for i := length(n) downto 1 do
	begin
		if n[i] = '.' then
			break;
	end;
	if i > 1 then
	begin
		basename := copy(n, 1, i  - 1);
		extension := copy(n, i, length(n) - i + 1);
		{ writeln('** splitFilename ',basename, ' ', extension); }
	end
	else
	begin
		basename := n;
		extension := '';
	end;
end;

function replaceExtension(var n:pathnamestr; newExt:filenamestr):pathnamestr;
var basename:filenamestr;
    ext:filenamestr;
begin
	splitFilename(n, basename, ext);
	replaceExtension := basename + newExt;
end;

procedure waitForKey; forward;

procedure listDirectory;
var volid:integer;
    error:integer;
    index:integer;
    dirs:DirectorySlot;
    ftime:DateTime;
    screenW,screenH:integer;
    count:integer;
begin
	GetTermSize(screenW, screenH);

	volid := findvolume(DefaultVolume);
	if volid < 1 then
		writeln('Volume ', DefaultVolume, ' not found.')
	else
	begin
		count := PageMargin;

		writeln('reading directory of ', DefaultVolume);
		openvolumeid(volid);
		readdirfirst(volid, index, dirs, error);
		while index > 0 do
		begin
			if dirs.modTime = 0 then
			begin
				ftime.year := 1970;
				ftime.month := 1;
				ftime.day := 1;
				ftime.hours := 0;
				ftime.minutes := 0;
				ftime.seconds := 0;
			end
			else
				ftime := GetDateTime(dirs.modTime);
			writew(dirs.name, 34);
			writew(DateStr(ftime) + ' ' + TimeStr(ftime,false), 22);
			writeln(dirs.sizeBytes:12, '  ', dirs.generation);

			count := count + 1;
			if count >= screenH then
			begin
				count := PageMargin;
				waitForKey;
			end;

			readdirnext(volid, index, dirs, error);
		end;
		closevolumeid(volid);

		if count + MenuHeight >= screenH then
			waitForKey;
	end;
end;

function volumeExists(var n:volumenamestr):boolean;
var volid:integer;
begin
	volid := findvolume(n);
	if volid < 1 then
		volumeExists := false
	else
	begin
		closevolumeid(volid);
		volumeExists := true;
	end;
end;

procedure listVolumes;
var i:integer;
begin
	InitDevices;
	writeln('Available volumes:');
	for i := 1 to VolumeCount do
		writeln(VolumeTable[i].part.name);
end;

procedure changeVolume;
var n:volumenamestr;
begin
	listVolumes;
	write('Enter volume name: ');
	readln(n);
	if length(n) > 0 then
		if volumeExists(n) then
			SetDefaultVolume(n)
		else
			writeln('Volume ', n , ' not found.');
end;

procedure removeFile;
var n:filenamestr;
    error:integer;
begin
	write('File to delete: ');
	readln(n);

	if length(n) > 0 then
	begin
		erase(n, error);

		if error <> 0 then
			writeln('Error deleting ', n, ': ', ErrorStr(error));
	end;
end;

procedure renameFile;
var n1,n2:filenamestr;
    error:integer;
begin
	write('File to rename: ');
	readln(n1);
	write('New name: ');
	readln(n2);
	rename(n1, n2, error);

	if error <> 0 then
		writeln('Error renaming ', n1, ': ', ErrorStr(error));
end;

procedure copyFile;
var n1,n2:filenamestr;
    error:integer;
    src,dst:file;
    ch:char;
    count:integer;
begin
	write('File to copy: ');
	readln(n1);
	write('New file name: ');
	readln(n2);

	open(src, n1, ModeReadonly);
	if IOResult(src) <> 0 then
	begin
		writeln('Error opening ', n1, ': ', ErrorStr(IOResult(src)));
		exit;
	end;

	open(dst, n2, ModeCreate);
	if IOResult(dst) <> 0 then
	begin
		writeln('Error opening ', n2, ': ', ErrorStr(IOResult(dst)));
		close(src);
		exit;
	end;

	write('Copying ',n1, ' to ', n2, '...');
	count := 0;
	while not eof(src) do
	begin
		read(src,ch); (* not efficient but keep it simple *)
		write(dst,ch);
		count := count + 1;
		if (count and 8191) = 0 then write('.');
	end;

	close(dst);
	close(src);

	writeln;
end;

procedure setWorkfile;
var n:filenamestr;
begin
	write('Enter workfile name: ');
	readln(n);

	ShellWorkfile := n;
	ShellCmd := '';
	ShellArg := 0;
end;

procedure requireWorkfile;
begin
	while length(ShellWorkFile) = 0 do
		setWorkfile;
end;

procedure edit(gotoLine:integer);
var error:integer;
    digits:string[10];
    args:PArgVec;
begin
	requireWorkfile;
	if gotoLine > 0 then
	begin
		str(gotoLine,digits);
		args[0] := '-l';
		args[1] := digits;
		args[2] := ShellWorkFile;
		PExec(EDITORPROG, args, 3, error);
	end
	else
		PExec2(EDITORPROG, ShellWorkFile, error);
	writeln('PExec error ', error);
end;

procedure assemble;
var filename:filenamestr;
    error:integer;
begin
	requireWorkfile;
	filename := replaceExtension(ShellWorkFile, '.s');
	PExec2(ASMPROG, filename, error);
	writeln('PExec error ', error);
end;

procedure compile;
var filename:filenamestr;
    error:integer;
begin
	requireWorkfile;
	filename := replaceExtension(ShellWorkFile, '.pas');
	PExec3(COMPILERPROG, '-S', filename, error);
	writeln('PExec error ', error);
end;

procedure build;
var filename:filenamestr;
    error:integer;
begin
	requireWorkfile;
	filename := replaceExtension(ShellWorkFile, '.pas');
	PExec2(COMPILERPROG, filename, error);
	writeln('PExec error ', error);
end;

procedure run;
var args:PArgVec;
    error:integer;
    prgname:pathnamestr;
begin
	requireWorkfile;
	prgname := replaceExtension(ShellWorkfile, '.prog');
	writeln('Running ', prgname);
	PExec(prgname, args, 0, error);
	writeln('Pexec failed, error ', error);
end;

procedure krunch;
var error:integer;
begin
	PExec2(RECLAIMPROG, DefaultVolume, error);
	writeln('PExec error ', error);
end;

procedure runProgram;
var args:PArgVec;
    argCount:integer;
    error:integer;
    prgname:pathnamestr;
    a:string;
begin
	write('Enter program file name: ');
	readln(prgname);

	if length(prgname) > 0 then
	begin
		if pos('.', prgname) = 0 then prgname := prgname + '.prog';

		writeln('Enter program arguments line-by-line, empty line to finish.');

		(* entering the arguments line by line is ugly, but it saves us from
			the hassle of dealing with word boundary detection and quoting *)
		argCount := 0;
		repeat
			write('arg ', argCount + 1, ': ');
			readln(a);
			if length(a) > 0 then
			begin
				args[argCount] := a;
				argCount := argCount + 1;
			end;
		until (length(a) = 0) or (argCount > PArgMax);

		writeln('Running ', prgname);
		PExec(prgname, args, argCount, error);
		writeln('Pexec failed, error ', error);
	end;
end;

procedure showMenu;
begin
	writeln;
	writeln('W)orkfile: ', ShellWorkfile);
	writeln('V)olume:   ', DefaultVolume);
	writeln('L)ist directory  K)runch volume  O)ther program');
	writeln('D)elete file  RenaM)e file  coP)y file');
	writeln('E)dit  C)ompile  A)ssemble  B)uild  R)un');
	write('> ');
end;

procedure command(cmd:char;arg:integer);
begin
	case cmd of
	'L':	listDirectory;
	'V':	changeVolume;
	'W':	setWorkfile;
	'R':	run;
	'A':	assemble;
	'C':	compile;
	'B':	build;
	'E':	edit(arg);
	'D':	removeFile;
	'M':	renameFile;
	'P':	copyFile;
	'K':	krunch;
	'O':	runProgram;
	else	;
	end;
end;

procedure waitForKey;
var c:char;
begin
	writeln;
	writeln('-- press any key to continue --');
	c := conin;
end;

begin
	if length(DefaultVolume) = 0 then
		SetDefaultVolume('SYSTEM');

	checkClock;

	if length(ShellCmd) > 0 then
	begin
		if ShellCmd[1] = 'W' then
		begin
			waitForKey;
			delete(Shellcmd,1,1);
		end;

		if length(ShellCmd) > 0 then
			command(ShellCmd[1], ShellArg);

		ShellCmd := '';
	end;

	while true do
	begin
		showMenu;
		read(cmd);
		writeln;
		command(Upcase(cmd), ShellArg);
	end;
end.
