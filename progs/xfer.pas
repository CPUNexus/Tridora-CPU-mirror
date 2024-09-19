(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
program xfer;

const CksumPattern = $AFFECAFE;
      SOH = #1;
      STX = #2;
      EOT = #4;
      ENQ = #5;
      ACK = #6;
      BEL = #7;
      NAK = #21;

      TimeoutTicks = 200;

var blockNo:integer;
    buf:^string;
    invalid:boolean;
    cmd:char;
    filename:string;
    size:integer;
    done:boolean;
    xferFile:file;

function calcChksum(last,this:integer):integer;
begin
	calcChksum := ((last + this) xor CksumPattern) shl 1;
end;

function readword:integer;
var b3,b2,b1,b0:char;
begin
	b3 := conin;
	b2 := conin;
	b1 := conin;
	b0 := conin;

	readword := (ord(b3) shl 24) or
		(ord(b2) shl 16) or
		(ord(b1) shl 8) or
		ord(b0);
end;

procedure serReadBlock(var success:boolean);
var w0,w1,w2,w3,w4,w5,w6,w7,w8:integer;
    chksum:integer;
    s:integer;

procedure writeByte(b:char);
begin
	if size <> 0 then
	begin
		write(xferFile,b);
		size := size - 1;
	end;
end;

procedure appendWordToFile(w:integer);
var b3,b2,b1,b0:char;
begin
	b0 := chr(w and 255);
	w := w shr 8;

	b1 := chr(w and 255);
	w := w shr 8;

	b2 := chr(w and 255);
	w := w shr 8;

	b3 := chr(w);

	writeByte(b3);
	writeByte(b2);
	writeByte(b1);
	writeByte(b0);
end;

procedure calcChksum(d:integer);
begin
	chksum := ((chksum + d) xor CksumPattern) shl 1;
end;

begin
	chksum := 0;

	w0 := readword;
	w1 := readword;
	w2 := readword;
	w3 := readword;
	w4 := readword;
	w5 := readword;
	w6 := readword;
	w7 := readword;

	s := readword;

	calcChksum(w0);
	calcChksum(w1);
	calcChksum(w2);
	calcChksum(w3);
	calcChksum(w4);
	calcChksum(w5);
	calcChksum(w6);
	calcChksum(w7);

	if s <> chksum then
	begin
		success := false;
		write(NAK);
		{ writeln('invalid chksum ', s, ' ', chksum); }
	end
	else
	begin
		success := true;
		appendWordToFile(w0);
		appendWordToFile(w1);
		appendWordToFile(w2);
		appendWordToFile(w3);
		appendWordToFile(w4);
		appendWordToFile(w5);
		appendWordToFile(w6);
		appendWordToFile(w7);
		blockNo := blockNo + 1;
		write(ACK);
	end;
end;

procedure waitForByte(var byteReceived:char; var timeoutReached:boolean);
var ticks:integer;
    done:boolean;
begin
	timeoutReached := true;
	ticks := getticks;
	done := false;

	repeat
		if conavail then
		begin
			done := true;
			timeoutReached := false;
			byteReceived := conin;
		end;
	until done or (getticks > ticks + TimeoutTicks);
end;

procedure waitForHeader(var invalid:boolean);
var done:boolean;
    timedOut:boolean;
    ch:char;
begin
	waitForByte(ch, timedOut);
	invalid := (ch <> STX) or timedOut;
end;

procedure receiveHeader(var invalid:boolean);
var ch:char;
    timedOut:boolean;
    w:integer;
    cksum:integer;
begin
	{ send protocol version, then wait for size header }
	write('1');

	waitForByte(ch, timedOut);
	if timedOut or (ch <> SOH) then
	begin
		invalid := true;
		exit;
	end;

	cksum := 0;

	w := readword;
	cksum := readword;

	if w <> (not cksum) then
	begin
		write(NAK);
		w := 0;
		writeln('h chksum error');
	end
	else
		write(ACK);

	if w > 0 then
	begin
		size := w;
		waitForHeader(invalid);
	end
	else
		invalid := true;
end;

procedure receiveFile;
var ch:char;
    invalid, timedOut:boolean;
    ok:boolean;
    done:boolean;
    errorCount:integer;
begin
	if length(filename) = 0 then
	begin
		writeln('Filename not set.');
		exit;
	end;

	errorCount := 0;

	waitForByte(ch, timedOut);
	if timedOut then
	begin
		writeln('Timeout waiting for transmission start (ENQ or STX).');
		exit;
	end;

	if ch = ENQ then
		receiveHeader(invalid)
	else
	if ch = STX then
	begin
		size := -1;
		invalid := false;
	end
	else
		invalid := true;

	if not invalid then
	begin
		open(xferFile, filename, ModeOverwrite);
		done := false;
		repeat
			serReadBlock(ok);
			if not ok then errorCount := errorCount + 1;

			waitForByte(ch, timedOut);
			if timedOut then
				writeln('Timeout waiting for next block (STX)');
			if ch = EOT then
				done := true
			else
			if ch <> STX then
			begin
				writeln('Invalid header byte (expected STX)');
				done := true;
			end;
		until done or timedOut;

		close(xferFile);

		writeln(blockNo, ' blocks received, ', errorCount, ' checksum errors. ', ord(ch));
	end
	else
		writeln('Invalid or no header received.', size);
end;

function getWordFromFile:integer;

function getCharFromFile:integer;
var c:char;
begin
	if size > 0 then
	begin
		read(xferFile,c);
		size := size - 1;
	end
	else
		c := #0;

	getCharFromFile := ord(c);
end;

begin
	getWordFromFile := getCharFromFile shl 8;
	getWordFromFile := (getWordFromFile or getCharFromFile) shl 8;
	getWordFromFile := (getWordFromFile or getCharFromFile) shl 8;
	getWordFromFile := (getWordFromFile or getCharFromFile);
end;

procedure sendword(w:integer);
var b3,b2,b1,b0:char;
begin
	b0 := chr(w and 255);
	w := w shr 8;

	b1 := chr(w and 255);
	w := w shr 8;

	b2 := chr(w and 255);
	w := w shr 8;

	b3 := chr(w and 255);

	write(b3,b2,b1,b0);
end;

procedure sendFile;
var ch:char;
    w,cksum:integer;
    wordCount:integer;
    lastSize,lastPos:integer;
    timedOut:boolean;
    done:boolean;
begin
	if length(filename) = 0 then
	begin
		writeln('Filename not set.');
		exit;
	end;

	{ wait for start byte }
	ch := conin;
	if ch <> BEL then
	begin
		writeln('Invalid start character received.');
		exit;
	end;

	open(xferFile, filename, ModeReadonly);
	if IOResult(xferFile) <> 0 then
	begin
		writeln('Error opening file: ', ErrorStr(IOResult(xferFile)));
		exit;
	end;

	size := filesize(xferFile);
	done := false;

	{ send size header: SOH, size word, inverted size word }
	write(SOH);
	sendword(size);
	sendword(not size);

	{ check for ACK }
	waitForByte(ch, timedOut);

	if timedOut then
		writeln('Timeout sending size header ')
	else
	if ch <> ACK then
		writeln('Error sending size header ', ord(ch))
	else
	repeat
		lastPos := filepos(xferFile);
		lastSize := size;

		write(STX);
		{ send a block: STX, 8 words, checksum word }
		cksum := 0;
		for wordCount := 1 to 8 do
		begin
			w := getWordFromFile;
			cksum := calcChkSum(cksum, w);
			sendword(w);
		end;
		sendword(cksum);

		{ check for ACK/NAK }
		waitForByte(ch, timedOut);
		if timedOut then
		begin
			writeln('Timeout waiting for ACK');
			done := true;
		end
		else
		if ch = NAK then
		begin
			seek(xferFile, lastPos);
			size := lastSize;
		end
		else
		if ch = ACK then
		begin
			if size = 0 then done := true;
		end
		else
		begin
			writeln('Invalid reply after sending block');
			done := true;
		end;
	until done;
	write(EOT);
	close(xferFile);
end;

procedure setFilename;
begin
	write('Filename> ');
	readln(filename);
end;

procedure listDirectory;
var volid:integer;
    error:integer;
    index:integer;
    dirs:DirectorySlot;
begin
	volid := findvolume(DefaultVolume);
	if volid < 1 then
		writeln('Volume ', DefaultVolume, ' not found.')
	else
	begin
		openvolumeid(volid);
		readdirfirst(volid, index, dirs, error);
		while (index > 0) and (error = 0) do
		begin
			writeln(dirs.name);
			readdirnext(volid, index, dirs, error);
		end;
	end;
	writeln;
end;

begin
	writeln('L) upload (receive)   D) download (send).');
	writeln('S) set filename       Y) directory         X) exit');

	done := false;

	repeat
		write('> ');
		read(cmd);
		writeln;
		case upcase(cmd) of
			'L': receiveFile;
			'D': sendFile;
			'S': setFilename;
			'X': done := true;
			'Y': listDirectory;
			else writeln('?');
		end;
	until done;
end.
