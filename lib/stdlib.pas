(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
unit stdlib;
implementation

const precision = 7;
      pi = 3.14159263;

const MaxInt = 2147483647;
      YearBias = 1970;

const TicksPerSec = 20;

const MaxVolumes = 32;
      DefaultBufSize = 4096;
      DefaultBufBlocks = 8;
      DirSlotSize = 64;

const IONoError 	=  0;
      IOFileNotFound	=  1;
      IOVolNotFound	=  2;
      IOPathInvalid 	=  3;
      IOFileExists 	=  4;
      IOFileClosed 	=  5;
      IOSeekInvalid 	=  6;
      IONoSpace 	=  7;
      IOReadOnly 	=  8;
      IOInvalidOp	=  9;
      IOInvalidFormat   = 10;
      IOUserIntr	= 11;
      IOMaxErr          = 11;

const PArgMax = 7;

type IOBlock  = array [0..127] of integer;
type IOBuffer = array [0..7] of IOBlock;

type filetype = (IOChannel, IODiskFile);
type filemode = (ModeReadonly, ModeCreate, ModeModify, ModeOverwrite, ModeAppend);
type file = record
		mode: filemode;
		lastError: integer;
		errorAck: boolean;
		ateoln:boolean;
		case typ:filetype of
		IOChannel: (channelid:integer;
			bufchar:char; buflen:integer;
			ateof:boolean;
			noecho:boolean; (* read chars are not echoed *)
			raw:boolean;  (* turn off backspace processing on input, CR processing on output *)
			nointr: boolean); (* turn off keyboard interrupt character processing *)

		IODiskFile: (volumeid: integer;fileno: integer; filpos:integer; bufStart:integer;
				size:integer; sizeExtents:integer;
				bufBlocks, extentBlocks:integer;
				changed: boolean;
				buffer: ^IOBuffer;
				bufpos: integer;
				bufsize: integer;
				needsflush: boolean;
		);
	end;
type text = file;

type fscanmode = (ScanInteger, ScanReal, ScanString);

type filenamestr = string[32];
type pathnamestr = string[68];
type volumenamestr = string[32];

type PartFlags = set of (PartEnabled, PartBoot, PartLast, PartPhysical, PartDefault);

type Partition = record
		name: volumenamestr;
		flags: PartFlags;
		startBlock: integer;
		blocks: integer;
		extentSize: integer; (* size of an extent in bytes, power of two > 512 *)
		dirSize: integer; (* number of directory slots *)
		bootBlocks: integer;
	end;

type PartitionTableBlock = array[0..7] of Partition;

type Volume = record
                part: Partition;
                deviceId: integer;
                partitionId: integer;
                startSlot: integer;  (* first directory slot known to be in use *)
		freeSlot: integer;   (* a directory slot that is probably free *)
		dirCache: ^DirBlock;
		cachedBlock: integer; (* cached volume block number in dirCache *)
		cacheDirty: boolean;
                openFilesCount: integer;
        end;

type DirSlotFlags = set of (SlotFree, SlotReserved, SlotDeleted, SlotEndScan, SlotFirst, SlotExtent, SlotReadonly);

type Timestamp = integer;

type DirectorySlot = record
                name: filenamestr;         (* the name of the file *)
                flags: DirSlotFlags;       (* see above *)
                sizeBytes: integer;     (* the size of the file in bytes *)
                createTime: Timestamp;  (* creation time of the file *)
                modTime: Timestamp;     (* time of last file modification *)
                generation: integer;    (* increased each time a file is overwritten *)
                owner: integer;         (* unused *)
        end;

     DirBlock = array [0..7] of DirectorySlot;

type PArgVec = array[0..PArgMax] of string;

type DateTime = record
		year:integer;
		month: 1..12;
		day: 1..31;
		hours: 0..23;
		minutes: 0..59;
		seconds: 0..59;
	end;

var input,output:file;
var DefaultVolumeId:integer;
    VolumeTable: array [1..MaxVolumes] of Volume;
    VolumeCount: integer;
    DevicesInitialized: boolean;

	(* the max string length must be at least one byte
		larger than the longest initialization value,
		so that we have a zero byte at the end
		and we can pass the address of the first
		character to the runtime error routine
		which takes null-terminated strings.
	*)
var ioerrordesc: array [0..11] of string[20] = (
	'No error',
        'File not found',
	'Volume not found',
	'Path invalid',
	'File already exists',
	'File closed',
	'Seek invalid',
	'No space',
	'File is readonly',
	'Invalid operation',
	'Invalid format',
	'Interrupted by user'
	);

    matherror:string[38] = 'Invalid argument to sqrt/ln/tan/cotan';
    pexecerror:string[28]= 'Invalid arguments for PExec';

    random_state:integer;

    PArgs:array [0..PArgMax] of string external;
    PArgCount:integer external;

    ShellCmd: string[40] external;
    ShellArg: integer external;

    DefaultVolume: volumenamestr external;

var DateTimeMTab: array[0..1, 1..12] of integer = (
	(31,28,31,30,31,30,31,31,30,31,30,31),
	(31,29,31,30,31,30,31,31,30,31,30,31)
    );

var SysBootTicks, SysLastTicks:integer external;
    SysClock:DateTime external;


FUNCTION LENGTH(s:STRING):INTEGER; EXTERNAL;
FUNCTION MAXLENGTH(s:STRING):INTEGER; EXTERNAL;
procedure appendchar(var s:string; aChar:char); external;
procedure RuntimeError(var s:string); external;
procedure coreload(devId:integer; physBlock:integer; sizeBytes:integer); external;
procedure initsdcard; external;
function cardchanged:boolean; external;

procedure readpartblk(blkno:integer;var partblk:PartitionTableBlock;
	var error:integer;devid: integer); external;
procedure readdirblk(blkno:integer;var dirblk:DirBlock;
	var error:integer;devid: integer); external;
procedure readblock(blkno:integer;var buf:IOBlock;
	var error:integer; devid: integer); external;

procedure writepartblk(blkno:integer;var partblk:PartitionTableBlock;
	var error:integer;devid: integer); external;
procedure writedirblk(blkno:integer;var dirblk:DirBlock;
	var error:integer;devid: integer); external;
procedure writeblock(blkno:integer;var buf:IOBlock;
	var error:integer; devid: integer); external;

procedure copybuf(dest:^IOBuffer;destOffset:integer; src:^IOBuffer; srcOffset:integer; length: integer); external;
function readfschar(var f:file):char; external;
procedure writefschar(var f:file; aChar:char); external;
procedure writefsstring(var f:file; var s:string); external;
procedure writechanwords(var f:file; src: ^IOBuffer; wordCount:integer); external;
procedure readchanwords(var f:file; src: ^IOBuffer; wordCount:integer); external;

function conin():char; external;
procedure conout(c:char); external;

function shiftfloat32(aReal:real; shiftCount:integer):real; external;
function getfloat32exp(aReal:real):integer; external;

procedure conoutw(w:integer); external;
function coninw():integer; external;

function getticks():integer; external;
procedure wait1msec; external;

procedure writechannel(var f:file; aChar:char); forward;
function eof(var fil:file):boolean; forward;
function eoln(var fil:file):boolean; forward;
function freadchar(var f:file):char; forward;
procedure pushback(var aFile:file; aChar:char); forward;
procedure fileerror(var fil:file; error:integer); forward;
procedure initDevices; forward;
function findvolume(name:string):integer; forward;

procedure AdvanceTime(var d:DateTime;seconds:integer);
var secsRest, minutesRest, hoursRest:integer;
    newSecs, newMinutes, newHours:integer;
    newDays, newMonth, newYear:integer;
    minutesDelta, hoursDelta, daysDelta:integer;
    mpdIndex, daysPerMonth:integer;

function isLeapYear:boolean;
begin
	isLeapYear := ((newYear mod 4) = 0)
		and ((newYear mod 100) <> 0)
		or ((newYear mod 400) = 0);
end;

begin

	secsRest := seconds mod 60;
	minutesDelta := seconds div 60;
	minutesRest := minutesDelta mod 60;

	newSecs := d.seconds + secsRest;
	if newSecs >= 60 then
	begin
		newSecs := newSecs - 60;
		minutesDelta := minutesDelta + 1;
		minutesRest := minutesRest + 1;
	end;
	d.seconds := newSecs;

	hoursDelta := minutesDelta div 60;
	hoursRest := hoursDelta mod 24;

	newMinutes := d.minutes + minutesRest;
	if newMinutes >= 60 then
	begin
		newMinutes := newMinutes - 60;
		hoursDelta := hoursDelta + 1;
		hoursRest := hoursRest + 1;
	end;
	d.minutes := newMinutes;

	daysDelta := hoursDelta div 24;
	newHours := d.hours + hoursRest;
	if newHours >= 24 then
	begin
		newHours := newHours - 24;
		daysDelta := daysDelta + 1;
	end;
	d.hours := newHours;

	newDays := d.day + daysDelta;

	newMonth := d.month;
	newYear := d.year;

	if isLeapYear then
		mpdIndex := 1
	else
		mpdIndex := 0;

	daysPerMonth := DateTimeMTab[mpdIndex][newMonth];
	while newDays > daysPerMonth do
	begin
		newMonth := newMonth + 1;
		newDays := newDays - daysPerMonth;

		if newMonth > 12 then
		begin
			newYear := newYear + 1;
			newMonth := 1;
			if isLeapYear then
				mpdIndex := 1
			else
				mpdIndex := 0;
		end;
		daysPerMonth := DateTimeMTab[mpdIndex][newMonth];
	end;

	d.day := newDays;
	d.month := newMonth;
	d.year := newYear;
end;

function GetTime:DateTime;
var now,delta:integer;
    secs:integer;
begin
	if SysClock.year = 0 then
	begin
		SysClock.year := 2001;
		SysClock.month := 1;
		SysClock.day := 1;
	end;

	now := GetTicks;
	delta := now - SysLastTicks;
	SysLastTicks := now;
	secs := delta div TicksPerSec;
	AdvanceTime(SysClock, secs);
	GetTime := SysClock;
end;

function TimeStr(d:DateTime;showSeconds:boolean):string;
var digits:string[4];
begin
	str(d.hours,digits);
	if d.hours<10 then
		TimeStr := '0';
	TimeStr := TimeStr + digits + ':';

	str(d.minutes,digits);
	if d.minutes<10 then
		appendchar(TimeStr,'0');
	TimeStr := TimeStr + digits;

	if showSeconds then
	begin
		appendchar(TimeStr, ':');
		str(d.seconds,digits);
		if d.seconds<10 then
			appendchar(TimeStr,'0');
		TimeStr := TimeStr + digits;
	end;
end;

function DateStr(d:DateTime):string;
var digits:string[4];
begin
	str(d.year,digits);
	DateStr := DateStr + digits + '-';

	str(d.month,digits);
	if d.month<10 then
		appendchar(DateStr,'0');
	DateStr := DateStr + digits;

	appendchar(DateStr, '-');
	str(d.day,digits);
	if d.day<10 then
		appendchar(DateStr,'0');
	DateStr := DateStr + digits;
end;

function GetTimestamp(var d:DateTime):Timestamp;
var i:Timestamp;
begin
	i := (d.year - YearBias) shl 24;
	i := i or (d.month shl 20);
	i := i or (d.day shl 15);
	i := i or (d.hours shl 10);
	i := i or (d.minutes shl 4);
	i := i or (d.seconds shr 2); (* seconds / 4 *)
	GetTimestamp := i;
end;

function GetDateTime(ts:Timestamp):DateTime;
begin
	GetDateTime.seconds := (ts and $0F) shl 2;
	ts := ts shr 4;

	GetDateTime.minutes := ts and $3F;
	ts := ts shr 6;

	GetDateTime.hours := ts and $1F;
	ts := ts shr 5;

	GetDateTime.day := ts and $1F;
	ts := ts shr 5;

	GetDateTime.month := ts and $0F;
	ts := ts shr 4;

	GetDateTime.year := YearBias + (ts and $FF);
end;

function GetCurTimestamp:Timestamp;
var now:DateTime;
begin
	now := GetTime;
	GetCurTimestamp := GetTimestamp(now);
end;

function copy(s:string;index,count:integer):string;
var len:integer;
begin
	copy := '';
	len := length(s);
	if index < 1 then index := 1;
	while (count > 0) and (index <= len) do
	begin
		copy := copy + s[index];
		index := index + 1;
		count := count - 1;
	end;
end;

procedure insert(ins: string; var dest: string; position:integer);
var i,count,from,to_:integer;
begin
	if position < 1 then position := 1;
	if position > length(dest) + 1 then position := length(dest) + 1;

	from  := length(dest);
	count := length(dest) - position + 1;
	to_   := from + length(ins);
	setlength(dest, length(dest) + length(ins));

	for i := 1 to count do
	begin
		if to_ <= maxlength(dest) then
		begin
			dest[to_] := dest[from];
			to_ := to_ - 1;
			from := from - 1;
		end;
	end;

	to_ := position;

	count := length(ins);
	for i := 1 to count do
	begin
		if to_ <= maxlength(dest) then
		begin
			dest[to_] := ins[i];
			to_ := to_ + 1;
		end;
	end;
end;

procedure delete(var s:string; from:integer; count:integer);
var i,len,last:integer;
begin
	len := length(s);
	if (from > 0) and (from <= len) and (count > 0) then
	begin
		if from + count <= len then
		begin
			last := len - count;
			for i := from to last do
				s[i] := s[i+count];
		end
		else
			last := from - 1;

		setlength(s,last);
	end;
end;

(* Find a substring inside a string, return the
   index of the character where the substring was found,
   or zero if the substring was not found.

   The substring is passed by value, so you can pass a
   string literal. The string to be searched in is passed
   as a var parameter for speed.

   That means you cannot use pos to search inside a string
   literal. Hopefully this is not something you want to do.
   *)
function pos(substr:string;var s:string):integer;
var substrlen:integer;
    slen:integer;
    searchpos:integer;
    subchar:char;
    subpos:integer;
    found:boolean;
    i:integer;

begin
        found := false;
	substrlen := length(substr);
	slen := length(s);

	searchpos := 1;
	subpos := 1;

	if(substrlen > 0) and (slen>0) then
	begin
		while not found and (searchpos <= slen) do
		begin
			(* compare character by character *)
			if substr[subpos] <> s[searchpos] then
			begin
				(* If a character does not match, reset the
					character index of the substring.
					Go to the next character of the searched
					string only if we are already at the
					beginning of the substring.
					Otherwise we need to check the current character
					against the first character of the substring. *)

				if subpos = 1 then
					searchpos := searchpos + 1;
				subpos := 1;
			end
			else
			begin
				(* character does match *)
				if subpos = 1 then
					(* remember start of this search attempt *)
					pos := searchpos;

				(* if this was the last character of the substring,
					we are successful *)
				if subpos = substrlen then
					found := true
				else
				begin
					(* else go to next characters *)
					subpos := subpos + 1;
					searchpos := searchpos + 1;
				end;
			end;
		end;
	end;

	if not found then
		pos := 0;
end;

function pwroften(exp:integer):real;
var i:integer;
    res:real;
    sofar:integer;
begin
	if exp = 0 then
		res := 1
	else if exp = 1 then
		res := 10
	else
	begin
		sofar := 1;
		res := 10;
		while sofar shl 1 <= exp do
		begin
			res := res * res;
			sofar := sofar shl 1;
		end;
		for i := sofar + 1 to exp do res := res * 10;
	end;

	pwroften := res;
end;

(* calculate the power of e using a Taylor series *)
function exp(exponent:real):real;
var x,p,frc,i,l:real;
begin
	x := exponent;
	frc := x;
	p := 1.0 + x;
	i := 1.0;

	repeat
		i := i + 1.0;
		frc := frc * (x / i);
		l := p;
		p := p + frc;
	until l = p;

	exp := p;
end;

(*
	calculate natural logarithm
	see https://stackoverflow.com/a/71994145
	no idea what algorithm this is :/
*)
function ln(n:real):real;
const euler = 2.7182818284590452354;
var a,b:integer;
    c,d,e,f:real;
    cn:real;
begin
	a := 0;

	if n > 0 then
	begin
		d := n / euler;
		while d > 1.0 do
		begin
			a := a + 1;
			n := d;
			d := n / euler;
		end;
		d := n * euler;
		while d < 1.0 do
		begin
			a := a - 1;
			n := d;
			d := n * euler;
		end;

		d := 1.0 / (n - 1);
		d := d + d + 1.0;
		e := d * d;
		c := 0;

		b := 1;
		f := 1.0;
		cn := c + 1.0 / (b * f);
		while (c + 0.00001) < cn do
		begin
			c := cn;
			b := b + 2;
			f := f * e;
			cn := c + 1.0 / (b * f);
		end;
		c := cn * 2.0 / d;

	end
	else
		RuntimeError(matherror);

	ln := a + c;
end;

(* calculate square root via Newton-Raphson method *)
function sqrt(n:real):real;
var error:real;
    guess, newGuess:real;
    diff, lastDiff:real;
begin
	if n < 0.0 then
		RuntimeError(matherror)
	else
	if n = 0.0 then
		sqrt := 0.0
	else
	begin
		guess := n / 2.0;
		error := n / 100000;	(* adapt the acceptable error to the argument *)

		diff := 0.0;

		repeat
			lastDiff := diff;
			(* newGuess := (guess + n/guess) / 2; *)
			(* a slight performance improvement by using shiftfloat
				instead of division *)
			newGuess := shiftfloat32(guess + n/guess, -1);
			diff := abs(newGuess - guess);
			guess := newGuess;
			(* we stop if the difference to the last guess is below
				the acceptable error threshold, if we somehow
				hit zero, or if the last difference is exactly the
				same as the new one *)
		until (diff < error) or (guess = 0.0) or (diff = lastDiff);

		sqrt := guess;
	end;
end;

function floor(x:real):integer;
begin
	if x < 0.0 then
		(* -3.7 gets floored to -4.0 *)
		x := x - 0.9999999;

	floor := trunc(x);
end;


function round(x:real):integer;
begin
	round := trunc(x+0.5);
end;

function sin(x:real):real;
var k,y:real;
    quadrant:integer;
    invert:boolean;
const twobypi = 0.6366198;
      pihalf = 1.5707963;

	function sin_taylor(x:real):real;
	var x2,x3,x5:real;
	begin
		x2 := x * x;
		x3 := x2 * x;
		x5 := x3 * x2;

		sin_taylor := x - x3 / 6.0 + x5 / 120.0;
	end;

begin
	if x < 0 then
	begin
		x := -x;
		invert := true;
	end
	else
		invert := false;

	k := floor( x * twobypi);
	y := x - k * pihalf;

	quadrant := trunc(k) mod 4;

	case quadrant of
		0: sin := sin_taylor(y);
		1: sin := sin_taylor(pihalf - y);
		2: sin := -sin_taylor(y);
		3: sin := -sin_taylor(pihalf - y);
	end;

	if invert then
		sin := -sin;
end;

function cos(x:real):real;
const pihalf = 1.57079632;
begin
	cos := sin(x + pihalf);
end;

(* arctan and tancot implemented after
	"Methods and programs for mathematical functions"
	by Stephen L. Moshier
   and the Cephes mathematical library by the same author.
*)

function arctan(x:real):real;
const tan3pi8 = 2.14121356;
      tanpi8  = 0.41421356;
      pihalf  = 1.57079632;
      piquart = 0.78539816;
var y,z:real;
    negate:boolean;
begin
	if x < 0.0 then
	begin
		x := -x;
		negate := true;
	end
	else
		negate := false;

	if x > tan3pi8 then
	begin
		y := pihalf;
		x := -(1.0/x);
	end
	else
	if x > tanpi8 then
	begin
		y := piquart;
		x := (x-1.0)/(x+1.0);
	end
	else
		y := 0.0;

	z := x * x;
	y := y +
		((( 8.05374449538e-2 * z
		  - 1.38776856032E-1) * z
		  + 1.99777106478E-1) * z
		  - 3.33329491539E-1) * z * x
		  + x;

	if negate then
		y := -y;
	arctan := y;
end;

function tancot(x:real; doCot:boolean):real;
const DP1 = 0.78515625;
      DP2 = 2.41875648e-4;
      DP3 = 3.77489497e-8;
      FOPI = 1.27323954;
      lossth = 8192.0;
var y,z,zz:real;
    j:integer;
    negate:boolean;
begin
	if x < 0 then
	begin
		x := -x;
		negate := true;
	end
	else
		negate := false;

	if x > lossth then
		RuntimeError(matherror);

	j := trunc(FOPI * x);
	y := j;

	if (j and 1) <> 0 then
	begin
		j := j + 1;
		y := y + 1.0;
	end;

	z := ((x - y * DP1)- y * DP2) - y * DP3;
	zz := z * z;

	if x > 1.0E-4 then
	begin
		y :=((((( 9.38540185543E-3 * zz
			+ 3.11992232697E-3) * zz
			+ 2.44301354525E-2) * zz
			+ 5.34112807005E-2) * zz
			+ 1.33387994085E-1) * zz
			+ 3.33331568548E-1) * zz * z
			+ z;
	end
	else
		y := z;

	if (j and 2) <> 0 then
	begin
		if doCot then
			y := -y
		else
			y := -1.0/y;
	end
	else
	if doCot then
		y := 1.0/y;

	if negate then
		y := -y;

	tancot := y;
end;

function tan(x:real):real;
begin
	tan := tancot(x, false);
end;

function cotan(x:real):real;
begin
	cotan := tancot(x, true);
end;

procedure fillchar(var s:string; startpos,count:integer; theChar:char);
var i:integer;
    endpos:integer;
    p1:integer;
begin
	endpos := length(s);
	setlength(s, endpos + count);
        p1 := startpos + count;

	for i := endpos downto startpos do
		s[i+count] := s[i];

	p1 := p1 - 1;
	for i := startpos to p1 do
		s[i] := theChar;
end;

procedure intstr(v:integer;fieldWidth:integer;var rbuf:string);
var buf:string[12];	(* signed 32 bit number can have at most 10 digits *)
    digit:integer;
    i:integer;
    isNegative:boolean;
begin
	buf := '';
	isNegative := false;

	(* special case for smallest integer
		which we cannot negate *)
	if v = -2147483648 then
	begin
		buf := '8463847412';
		isNegative := true;
	end
	else
	begin
		if v < 0 then
		begin
			isNegative := true;
			v := -v;
		end;

		repeat
			digit := v mod 10;
			v := v div 10;		(* this could be a single DIVMOD call in assembly *)
			buf := buf + chr(digit + 48); (* ascii code for '0' *)
		until v = 0;
	end;

	rbuf := '';
	if isNegative then
		rbuf := rbuf + '-';

	(* field width is used by str() special procedure *)
	if fieldWidth > length(rbuf) then
		fillchar(rbuf, 1, fieldWidth - length(rbuf), ' ');

	for i := length(buf) downto 1 do
		rbuf := rbuf + buf[i];
end;

procedure realstr(x:real;  w, d: integer; var s: string[30]);
var j, truncx, expx: integer;
    normx: real;
begin
	(* check w and d for validity *)
	if (w < 0) or (d < 0) then
		begin w := 0; d := 0 end;

	(* take abs(x), normalize it and calculate exponent *)
	if x < 0 then
		begin x := -x;  s := '-' end
	else
		s := ' ';

	expx := 0;  normx := x;

	if x >= 1.0 then  (* divide down to size *)
		while normx >= 10.0 do
		begin
			expx := expx+1;
			normx := x/pwroften(expx)
		end
	else
	if x <> 0 then  (* multiply up to size *)
		repeat
			expx := expx-1;  normx := x*pwroften(-expx)
		until normx >= 1;

	(* round number according to some very tricky rules *)
	if (d=0) or (d+expx+1 > precision) then  (* scientific notation, or decimal places *)
		normx := normx + 5/pwroften(precision)        (* overspecified *)
	else if d+expx+1 >= 0.0 then
		normx := normx + 5/pwroften(d+expx+1);
	(* if d+expx+1 < 0, then number is effectively 0.0 *)

	(* if we just blew normalized stuff then fix it up *)
	if normx >= 10.0 then
		begin  expx := expx+1;  normx := normx/10.0 end;

	(* put the digits into a string *)
	for j := 1 to precision do
	begin
		truncx := trunc(normx);
		s := s + chr(truncx+ord('0'));
		normx := (normx-truncx)*pwroften(1)
	end;

	(* put number into proper form *)
	if (d=0) or (expx >= 6) then  (* scientific notation *)
	begin
		insert('.', s, 3);
		if expx <> 0 then
		begin
			s := s + 'E';
			if expx < 0 then
				begin s := s + '-';  expx := -expx end;
			if expx > 9 then
				s := s + chr(expx div 10 + ord('0'));
			s := s + chr(expx mod 10 + ord('0'))
		end;
	end
	else  (* some kind of fixed point notation *)
	if expx >= 0 then
	begin
		insert('.', s, 3+expx);
		for j := 1 to d-(5-expx) do
			s := s + ' '; (* add blanks if over-precision *)
		setlength(s, 3 + expx + d); (* 6 digits after point, 3 + exp chars before *)
	end
	else
	begin
		insert('0.',s,2);
		for j := 1 to -expx-1 do
			insert('0',s,4); (* leading zeroes *)
		setlength(s, 3 + d); (* 3  chars before point *)

	 (* fillchar(s[9-expx], d-6+expx, ' ');*) (* put in blanks for over-precision*)
	end;

	if w > length(s) then
		fillchar(s, 1, w - length(s), ' ');
end;


function isdigit(aChar:char):boolean;
begin
	isdigit := (aChar >= '0') and (aChar <= '9');
end;

function iswhite(aChar:char):boolean;
begin
	iswhite := aChar in [ #32, #9, #13, #10 ];
end;

procedure skipwhite(var s:string;var i:integer);
var l:integer;
    c:char;
begin
	for c in s do
		if not (c in [ #10, #13, #32, #9 ]) then
			break
		else
			i := i + 1;
end;

procedure intval(s:string; var value,code:integer);
var i,v,l,d:integer;
    digit:char;
    negate:boolean;
    valid:boolean;
begin
	i := 1; v := 0; l := length(s);
	negate := false; valid := false;
	skipwhite(s,i);
	code := l+1;	(* for an empty string, we return a position after the end *)

	if length(s) >= i then
	begin
		digit := s[i];
		if digit = '-' then
		begin
			negate := true;
			i := i + 1;
		end
		else
		if digit = '+' then
			i := i + 1;

		while (i <= l) do
		begin
			digit := s[i];
			valid := isdigit(digit);
			if valid then
			begin
				d := ord(digit) - ord('0');
				v := v * 10 + d;
			end
			else
			begin
				(* invalid digit, set error position *)
				code := i;
				break;
			end;
			i := i + 1;
		end;
	end;

	if valid and (i = l + 1)  then
	(* if we are after the end of the string and have a valid result *)
	begin
		if negate then
			value := -v
		else
			value := v;
		code := 0;
	end;
end;

procedure realval(s:string;var v:real;var code:integer);
label ext;
var ch: char; neg,xvalid: boolean; ipot: integer;
      x:real;
      i:integer;
      feof: boolean;
      digitval:real;

function nextchar:char;
begin
	if i<=length(s) then
	begin
		nextchar := s[i];
		i := i + 1;
		feof := false;
	end
	else
	begin
		nextchar := #0;
		feof := true;
	end;
end;

procedure sreadint(var e:integer);
var digits: string[4];
    status: integer;
begin
	e := 0;
	digits := copy(s, i , 4);
	intval(digits,e,status);
	if status <> 0 then
	begin
		(*
		writeln('***sreadint error at ', status, ' for ', digits,
			' ', length(digits), ' ', i); *)
		i := i + status;
		xvalid := false;
	end
	else
		i := i + length(digits);
end;

begin
	i := 1;
	x := 0; neg := false; xvalid := false;

	skipwhite(s,i);

	ch := nextchar;
	if (ch = '+') or (ch = '-') then
	begin
		neg := ch = '-';
		ch := nextchar
	end;

	while isdigit(ch) and not feof do
	begin
		xvalid := true;
		x := x*10 + (ord(ch)-ord('0'));
		ch := nextchar;
	end;
	if feof then goto ext;

	ipot := -1;
	if ch = '.' then
	begin
		ipot := 0;
		repeat
			ch := nextchar;
			if isdigit(ch) then
			begin
				xvalid := true; ipot := ipot + 1;
				digitval := (ord(ch)-ord('0'))/pwroften(ipot);
				(* x := x + (ord(ch)-ord('0'))/pwroften(ipot); *)
				x := x + digitval;
			end
		until feof or not isdigit(ch);
		if feof then goto ext;
	end;

	if ((ch = 'e') or (ch = 'E')) and (xvalid or (ipot < 0)) then
	begin
		sreadint(ipot);
		if feof then goto ext;
		if ipot < 0 then
			x := x/pwroften(abs(ipot))
		else
			x := x*pwroften(ipot);
	end;
ext:
	(* if processing stopped before the end of string,
		we encountered an invalid character,
		so we indicate failure *)
	if i <= length(s) then	
		xvalid := false;

	if xvalid then
	begin
		if neg then x := -x;
		v := x;
		code := 0;
	end
	else
		code := i - 1;
end;

procedure checkerror(var fil:file);
begin
	if fil.lastError <> 0 then
	begin
		if not fil.errorAck then
			RuntimeError(ioerrordesc[fil.lastError])
		else
		begin
			fil.lastError := 0;
			fil.errorAck := false;
		end;
	end;
end;

procedure handleBackspace(var aFile:file; var buf:string; var bytesRemoved:integer);
var len:integer;
    removedChar:integer;
    highbits:integer;
begin
	bytesRemoved := 0;
	len := length(buf);
	if len > 0 then
	begin
		if aFile.typ = IOChannel then
		begin
			(* write BS, space, BS sequence to delete one character *)
			writechannel(aFile, #8);
			writechannel(aFile, #32); 
			writechannel(aFile, #8);
		end;
		repeat
			removedChar := ord(buf[len]);
			bytesRemoved := bytesRemoved + 1;
			len := len - 1;

			(* since a string really contains bytes, not chars,
				we need to check for UTF-8-encoded multibyte characters *)

			(* isolate the two leftmost bits of the byte we just removed *)
			highbits := removedChar and $C0;

			(* A byte that is part of a multibyte character and is not the
				first byte has 10 the two highest bits.
				11 is the first byte of a multibyte character,
				a 7-bit ASCII character has 00 or 01.*)
		until (highbits <> $80) or (len = 0);
		setlength(buf, len);
	end
end;

procedure fscanbuf(var aFile:file; mode: fscanmode; var buf:string);
var bytesRead:integer;
    maxBytes:integer;
    aChar:char;
    done: boolean;
    bytesRemoved: integer;
    isChannel: boolean;
    skipchar: boolean;

function isSeparator(aChar:char):boolean;
begin
	case mode of
	ScanInteger: isSeparator := not (isDigit(aChar) or (aChar = '-'));
	ScanReal:    isSeparator := not (isdigit(aChar) or (aChar in [ '+', '-', '.', 'E', 'e' ]));
	ScanString:  isSeparator := (aChar = #13) or (aChar = #10);
	end;
end;

begin
	maxBytes := maxlength(buf);
	bytesRead := 0; done := false; skipchar := false;
	buf := '';

	isChannel :=  aFile.typ = IOChannel;

	repeat
		if eof(aFile) then
			done := true
		else
		begin
			aChar := freadchar(aFile);

			if isChannel then
			begin
				if aChar = #127 then (* DEL *)
				begin
					if not (aFile.raw or aFile.noecho) then
					begin
						handleBackspace(aFile, buf, bytesRemoved);
						bytesRead := bytesRead - bytesRemoved;
					end;
					skipchar := true;
				end
				else if aChar = #4 then	(* don't put EOF char into buffer *)
					skipchar := true
				else
					skipchar := false;
			end;

			if not skipchar then
			begin
				if isSeparator(aChar) then
				begin
					done := true;
					pushback(aFile, aChar);
				end
				else
				begin
					appendchar(buf, aChar);
					bytesRead := bytesRead + 1;
				end;
			end;
		end
		if bytesRead = maxBytes then
			done := true;
	until done;
end;

procedure fskipwhite(var f:file);
var c:char;
begin
	repeat
		c := freadchar(f);
	until eof(f) or not iswhite(c);
	pushback(f, c);	
end;

procedure freadint(var v:integer;var f:file);
var buf:string[24];
    errpos:integer;
begin
	errpos := -1;
	fskipwhite(f);
	fscanbuf(f, ScanInteger, buf);

	if f.lastError = 0 then
		val(buf, v, errpos);
	if errpos <> 0 then
	begin
		fileerror(f, IOInvalidFormat);
		checkerror(f);
	end;
end;

procedure freadreal(var v:real;var f:file);
var buf:string[40];
    errpos:integer;
begin
	fskipwhite(f);
	fscanbuf(f,ScanReal, buf);
	if f.lastError = 0 then
		val(buf, v, errpos);
	if errpos <> 0 then
		fileerror(f, IOInvalidFormat);
end;

procedure freadstring(var s:string; var f:file);
begin
	fscanbuf(f, ScanString, s);
end;

procedure skipeoln(var aFile:file);
var aChar:char;
begin
	repeat
		aChar := freadchar(aFile);
	until (aChar = #13) or eof(aFile);

	(*
		If it is a disk file, try to read the
		LF character that should follow the CR
		character.
		On a channel (i.e. the console), we
		only get the CR character. *)

	if aFile.typ <> IOChannel then
	begin
		if not eof(aFile) then
		begin
			aChar := freadchar(aFile);
			if aChar <> #10 then
				pushback(aFile, aChar);
		end;
	end;
end;


(*
 *************** Filesystem *********************************
*)

procedure SetDefaultVolume(volname:volumenamestr);
var volid:integer;
begin
	volid := findvolume(volname);
	if volid > 0 then
	begin
		DefaultVolume := volname;
		DefaultVolumeId := volid;
	end;
end;

procedure addPartitions(devid:integer; var partblk:PartitionTableBlock; var isLast:boolean);
var partNo:integer;
    flags:PartFlags;
begin
	partNo := 0;
	for partNo := 0 to 7 do
	begin
		flags := partblk[partNo].flags;
		if PartLast in flags then isLast := true;
		if PartEnabled in flags then
		begin
			volumeCount := volumeCount + 1;

			with volumeTable[volumeCount] do
			begin	{
				writeln('** addPartitions #', partNo, ' vol #', volumeCount);
				writeln('** addPartitions #', partNo, ' start', partblk[partno].startBlock);
				writeln('** addPartitions .', ord(partblk[partno].name[1]));
				writeln('** addPartitions >', length(partblk[partno].name));
				writeln('** addPartitions ', partblk[partNo].name); }
				part := partblk[partNo];
				deviceId := devid;
				partitionId := partNo;
				startSlot := 0;
				freeSlot := 0;
				dirCache := nil;
				cachedBlock := -1;
				cacheDirty := false;
				openFilesCount := 0;
			end;

			{ if (PartDefault in flags) and (DefaultVolumeId = 0) then
				DefaultVolumeId := volumeCount; }

			{writeln('added volume ', volumeCount);}
		end;
	end;
end;

procedure readPartitions(devid: integer);
var blkNo:integer;
    partblk:PartitionTableBlock;
    isLast:boolean;
    error:integer;
begin
        blkNo := 0;
	isLast := false;
	error := 0;

	for blkNo := 0 to 7 do
	begin
		readpartblk(blkNo, partblk, error, devid);
		if error = 0 then
			addPartitions(devid, partblk, isLast)
		else
			(* TODO: some real error handling *)
			writeln('Error reading partition block ', blkNo);

		if isLast or (error <> 0) then break;
	end;
end;

procedure readdevice(deviceId:integer;blockNo:integer;var buf:IOBlock; var error:integer);
begin
	(* TODO: check for card change *)
	readblock(blockNo, buf, error, deviceId);
	{ writeln('readblock ', blockNo); }
end;

procedure writedevice(deviceId:integer;blockNo:integer;var buf:IOBlock; var error:integer);
begin
	(* TODO: check for card change *)
	writeblock(blockNo, buf, error, deviceId);
	{ writeln('writeblock ', blockNo); }
end;

function getphysblockno(volumeid:integer; blockNo:integer):integer;
begin
	(* TODO: check for valid volume id and blockNumber, how to return error? *)
	getphysblockno := volumetable[volumeid].part.startBlock + blockNo;
end;

(* read some consecutive blocks from a volume *)
procedure readvolumeblks(volumeid:integer; destbuf:^iobuffer; blkno:integer; blkCount: integer; var error:integer);
var deviceblk:integer;
    deviceid:integer;
    i:integer;
begin
	deviceblk := getphysblockno(volumeid,blkno); (* TODO: check valid block number *)
	deviceid := volumetable[volumeid].deviceid;
	i := 0;

	{ writeln('***** readvolumeblk ', blkno, ' ', blkCount, ' ', destbuf); }
	while blkCount > 0 do
	begin
		readdevice(deviceid, deviceblk, destbuf^[i], error); (* read one block *)
		(* TODO: should be able to read multiple blocks from the card *)
		{ writeln('  data: ', destbuf^[i][0]); }
		blkCount := blkCount - 1;
		deviceblk := deviceblk + 1;
		i := i + 1;
	end;
end;

(* write some consecutive blocks onto a volume *)
procedure writevolumeblks(volumeid:integer; srcbuf:^iobuffer; blkno:integer; blkCount: integer; var error:integer);
var deviceblk:integer;
    deviceid:integer;
    i:integer;
begin
	deviceblk := getphysblockno(volumeid,blkno); (* TODO: check valid block number *)
	deviceid := volumetable[volumeid].deviceid;
	i := 0;
	while blkCount > 0 do
	begin
		writedevice(deviceid, deviceblk, srcbuf^[i], error); (* write one block *)
		(* TODO: should be able to write multiple blocks to the card, maybe do an erase cmd before *)
		blkCount := blkCount - 1;
		deviceblk := deviceblk + 1;
		i := i + 1;
	end;
end;

function findvolume(name:string):integer;
var volidx:integer;
begin
	initDevices;

	findvolume := 0;
	for volidx := 1 to volumeCount do
	begin
		if volumeTable[volidx].part.name = name then
		begin
			findvolume := volidx;
			break;
		end;
	end;
end;

procedure flushdircache(volumeid:integer;var error:integer);
begin
	with volumeTable[volumeid] do
	begin
		if (dirCache <> nil) and (cachedBlock >= 0) and cacheDirty then
		begin
			{ writeln('*** flushdircache'); }
			writedirblk(getPhysBlockNo(volumeid, cachedBlock), dirCache^, error, deviceId);
			cacheDirty := false;
		end;
	end;
end;

procedure openvolumeid(volid:integer);
begin
	with volumeTable[volid] do
	begin
		if dirCache = nil then
			new(dirCache);
		openFilesCount := openFilesCount + 1;
	end;
end;

procedure closevolumeid(volid:integer);
var error:integer;
begin
	with volumeTable[volid] do
	begin
		openFilesCount := openFilesCount - 1;
		if openFilesCount = 0 then
		begin
			flushdircache(volid, error);
			cachedBlock := -1;
			dispose(dirCache);
			dirCache := nil;

		end;
	end;
end;

procedure loaddirblock(volumeid:integer;dirblkno:integer;var error:integer);
begin
	with volumeTable[volumeid] do
	begin
		if cachedBlock <> dirblkno then
		begin
			flushdircache(volumeid, error);
			{ writeln(' loaddirblock dirBlkNo:', dirblkno, ' phys:', getPhysBlockNo(volumeid, dirblkno)); }
			readdirblk(getPhysBlockNo(volumeid, dirblkno), dirCache^, error, deviceId);
			cachedBlock := dirblkno;
		end;
	end;
end;

(* read a specific directory slot from a volume *)
procedure getdirslot(volumeid:integer;slotNo:integer;var result:DirectorySlot;var error:integer);
var dirblkno:integer;
    slotOffset:integer;
begin
	error := 0;

	with volumeTable[volumeid] do
	begin
		dirblkno := slotNo div 8;
		slotOffset := slotNo mod 8;

		(* writeln('get dirBlkNo:', dirblkno, ' slotOffset:', slotOffset); *)

		loaddirblock(volumeid, dirblkno, error);
		result := dirCache^[slotOffset];
	end;
end;

(* write a specific directory slot of a volume *)
procedure putdirslot(volumeid:integer;slotNo:integer;var dirslot:DirectorySlot;var error:integer);
var dirblkno:integer;
    slotOffset:integer;
begin
	with volumeTable[volumeid] do
	begin
		dirblkno := slotNo div 8;
		slotOffset := slotNo mod 8;

		{ writeln('put dirBlkNo:', dirblkno, ' slotOffset:', slotOffset); }

		loaddirblock(volumeid, dirblkno, error);
		(* TODO: check for error *)
		dirCache^[slotOffset] := dirslot;
		cacheDirty := true;
	end;
end;

(* find a free directory slot, return the slot number *)
function finddirslot(volid:integer; var error:integer):integer;
var slotno:integer;
    maxSlots:integer;
    dirslot:DirectorySlot;
    done:boolean;
begin
	finddirslot := -1;

	with volumeTable[volid] do
	begin
		maxSlots := part.dirSize;
		slotno := startSlot;
		{ writeln('** finddirslot startSlot ', slotno, ' maxSlots ', maxSlots); }
        	done := false;
		repeat
			getdirslot(volid, slotno, dirslot, error);
			{ writeln('** slot ', slotno, ' ', dirslot.name); }
			if SlotFree in dirslot.flags then
			begin
				finddirslot := slotno;
				done := true;
				freeSlot := slotno;
				{ writeln('** free slot found at ', slotno); }
			end
			slotNo := slotNo + 1;
		until done or (slotNo >= maxSlots) or (error <> 0);
	end;
end;


(* read in the file buffer for the current seek position *)
procedure readbuf(var fil:file;var error:integer);
var blkno:integer;
begin
        (* calculate block number from seek position and start block *)
	(* fil.bufStart := fil.filpos and not 511; *) (* if we had arithmetic AND *)
	fil.bufStart := fil.filpos - fil.filpos mod 512;
        blkno := fil.bufStart div 512 +
                fil.fileno * fil.extentBlocks; (* fileno is the directory slot number
                                                which is equivalent to the start extent *)
        (* read the number of blocks equivalent to the buffer size from the device *)
        readvolumeblks(fil.volumeid, fil.buffer, blkno, fil.bufBlocks, error);
	{ writeln('  readbuf data: ', fil.buffer^[0][0]); }
end;

procedure fileerror(var fil:file; error:integer);
begin
	(* should check if there was an error already
	   and throw a runtime error in that case *)
	fil.lastError := error;
	fil.errorAck := false;
end;

function IOResult(var fil:file):integer;
begin
	IOResult := fil.lastError;
	fil.errorAck := true;
end;

function ErrorStr(err:integer):string;
begin
	if err <= IOMaxErr then
		ErrorStr := ioerrordesc[err]
	else
		ErrorStr := 'Invalid error code';
end;

(* TODO: should eof return false if the file
   is in error state? *)
function eof(var fil:file):boolean;
begin
	if fil.typ = IODiskFile then
		eof := fil.filpos >= fil.size
	else
		eof := fil.ateof;
end;

function eoln(var fil:file):boolean;
begin
	eoln := eof(fil) or fil.ateoln;
end;

(* read from filesystem.
   destbuf is a opaque pointer to a number of words specified by len.
   len is specified in bytes, and does not have to be a multiple of the word size.
   (really? maybe two options: either len is 1 (scanning for string end),
   or a multiple of the word size (reading in binary data))
   The compiler converts a passed aggregate object to the opaque pointer.
   This pointer is then passed to the assembly routine copybuf *)
procedure readfs(var fil:file; destbuf:^IOBuffer; len:integer);
var bufleft, partial:integer;
    destpos:integer;
    blkno: integer;
    error: integer;
begin
        error := 0;
	destpos := 0;

        (* check for read beyond end of file *)
        if fil.filpos + len > fil.size then
                len := fil.size - fil.filpos;
        (* TODO: how to represent a short read?
                set error to EOF? add a var parameter
                which returns the number of bytes read?
        *)
	(* writeln('**** readfs ', len, ' at ', fil.filpos); *)
        while (len > 0) and (error = 0) do
        begin
                if fil.bufpos < fil.bufsize then (* is something left in the buffer? *)
                begin
                        bufleft := fil.bufsize - fil.bufpos;
			(*writeln('**** readfs ++  ', bufleft);
			writeln('  ** ', fil.buffer^[0][0]);*)
                        if len > bufleft then
                                partial := bufleft
                        else
                                partial := len;
                        copybuf(destbuf, destpos, fil.buffer, fil.bufpos, partial);
			(*writeln('  *> ', destbuf^[0][0]);*)
                        len := len - partial;
                        fil.bufpos := fil.bufpos + partial;
                        fil.filpos := fil.filpos + partial;
			destpos := destpos + partial;
                end
                else
		begin
                        readbuf(fil, error);
			fil.bufpos := 0;
		end;
        end;

	if error <> 0 then
		fileerror(fil, error);
end;

(* write back the file buffer *)
procedure flushfile(var fil:file);
var blkno:integer;
    error:integer;
begin
        blkno := fil.bufStart div 512 +
                fil.fileno * fil.extentBlocks;
        (* write buffer back to disk *)
        writevolumeblks(fil.volumeid, fil.buffer, blkno, fil.bufBlocks, error);
        if error <> 0 then
                fileerror(fil, error);
        fil.needsflush := false;
end;

(* seek to a specific byte position in a file *)
(* a seek beyond the end of the file is an error,
   except to the position one byte beyond. *)
procedure seek(var fil:file; position:integer);
var blkno:integer;
    error:integer;
begin
	checkerror(fil);

	if fil.typ = IOChannel then
		fileerror(fil, IOSeekInvalid)
	else
	begin
		if fil.needsflush then (* write back current buffer if necessary *)
			flushfile(fil);
		(* check for seek beyond end of file  or append-only mode *)
		if (position > fil.size) or (fil.mode = ModeAppend) then
			fileerror(fil, IOSeekInvalid)
		else
		begin
			fil.filpos := position;
			fil.bufpos := position mod fil.bufsize;
			(* if the new file position is outside current buffer,
				read new buffer *)
			if (position < fil.bufStart) or
				(position >= fil.bufStart + fil.bufSize) then
			begin
				{ writeln('***** seek readbuf ', position); }
				readbuf(fil, error);
				if error <> 0 then
					fileerror(fil, error);
			end;
		end;
	end;
end;

function filepos(var fil:file):integer;
begin
	if fil.typ = IOChannel then
		filepos := 0
	else
		filepos := fil.filpos;
end;

function filesize(var fil:file):integer;
begin
	if fil.typ = IOChannel then
		filesize := -1
	else
		filesize := fil.size;
end;

(* allocate more extents for a file *)
procedure extendfile(var fil:file; newSize:integer);
var newExtents:integer;
    entry:DirectorySlot;
    endSlot:integer;
    i:integer;
    error:integer;
begin
	if newSize > fil.size then
	begin
		newExtents := newSize div (fil.extentBlocks * 512) + 1;
		{ writeln('extendfile old extents:', fil.sizeExtents, ' new extents:', newExtents, ' extentBlocks:', fil.extentBlocks); }
		if newExtents > fil.sizeExtents then
		begin
			(* we need to allocate one or more new extents *)
			endSlot := fil.fileno + newExtents - 1; (* extent number starts at zero *)
			(* start after the first extent of the file *)
			for i := fil.fileno + fil.sizeExtents to endSlot do
			begin
				(* read in the directory slot *)
				getdirslot(fil.volumeid, i, entry, error);
				if not (SlotFree in entry.flags) then
				begin
					{ writeln('extendfile IONoSpace'); }
					(* if it is not free, we can't extend the file
						and we return an error *)
					fileerror(fil, IONoSpace);
					break;
				end
				else
				begin
					{ writeln('extendfile marked slot ', i); }
					(* mark the slot as in use *)
					entry.flags := entry.flags - [SlotFree,SlotEndScan] + [SlotExtent];
					(* write back the slot *)
					putdirslot(fil.volumeid, i, entry, error);
					if error <> 0 then
						fileerror(fil, error);
				end;
			end; 
			(* read(dummy); *)
		end;
	
		if fil.lastError = 0 then
		begin
			fil.size := newSize;
			fil.sizeExtents := newExtents;
			(* update directory here? *)
		end;
	end;
end;

(* write to filesystem *)
(* srcbuf is used as a generic pointer to an array of words, len is the actual
   length in bytes, and the length does not have to be word-aligned
    The compiler converts any passed aggregate object into the pointer type. 
   The pointer is then passed opaquely to the assembly routine copybuf. *)
(* TODO: what about strings? a small assembly routine that gets a pointer
    to the string and converts by skipping the string header and adding a length arg? *)
procedure writefs(var fil:file; srcbuf:^IOBuffer; len:integer);
var
    bufleft:integer;
    srcleft:integer;
    srcpos:integer;
    blkno:integer;
    newpos:integer;
    error:integer;
label errexit;

begin
	bufleft := fil.bufsize - fil.bufpos;
        srcleft := len;
	srcpos := 0;
        error := 0;

	if fil.mode = ModeReadonly then
	begin
		fileerror(fil, IOReadOnly);
		goto errexit;
	end;

	newpos := fil.filpos + len;
	if newpos > fil.size then
	begin
		extendfile(fil, newpos);
		if fil.lastError <> 0 then goto errexit;
	end;

	{ if len = 1 then
		writeln('writefs write char: ', srcbuf^[0][0]); }

	while (srcleft > 0) and (error = 0) do
	begin
		fil.changed := true;
		fil.needsflush := true;

		{ writeln('writefs bufpos:', fil.bufpos, ' srcleft:', srcleft, ' bufleft:', bufleft); }
		(* will we cross a buffer boundary? *)
		if srcleft > bufleft then
		begin
			{ writeln('writefs part ', srcpos, ' -> ', fil.bufpos, ' ', bufleft); }
			(* copy the part from the source that fits into the file buffer *)
			copybuf(fil.buffer, fil.bufpos, srcbuf, srcpos, bufleft);
			(* the bufffer is flushed below  *)

			(* reset buffer position and advance pointer *)
			fil.bufpos := 0;
			fil.filpos := fil.filpos + bufleft;
			srcleft := srcleft - bufleft;
			srcpos := srcpos + bufleft;
			bufleft := fil.bufsize;
		end
		else (* the data we want to write fits into the buffer *)
		begin
			{ writeln('writefs ____ ', srcpos, ' -> ', fil.bufpos, ' ', srcleft); }
			(* copy what is left of the source into buffer *)
			copybuf(fil.buffer, fil.bufpos, srcbuf, srcpos, srcleft);
			(* advance buffer position and file pointer *)
			fil.bufpos := fil.bufpos + srcleft;
			fil.filpos := fil.filpos + srcleft;
			bufleft := bufleft - srcleft;
			srcleft := 0;
		end;

		(* if we moved out of the current iobuffer, read
			in the new one *)
		if fil.filpos >= fil.bufStart + fil.bufSize then
		begin
			{ writeln('writefs flush at ', fil.filpos, ' ', fil.bufStart); }
			flushfile(fil);

			(* Only read in new buffer
				if the data left to write is not
				larger than the buffer size.
				In that case, the whole buffer would
				be overwritten anyway. *)
			if srcleft < fil.bufSize then
				readbuf(fil, error)
			else
				fil.bufStart := fil.bufStart + fil.bufSize;
		end;

		if error <> 0 then
			fileerror(fil, error);
	end;
errexit:
end;

function findfile(volid:integer; var name:filenamestr; var dirslot:DirectorySlot;var error:integer):integer;
var slotno:integer;
    maxSlots:integer;
    done:boolean;
    found:boolean;
begin
	findfile := -1;
	with volumeTable[volid] do
	begin
		maxSlots := part.dirSize;
		slotno := startSlot;
		{ writeln('** findfile ', slotno); }
        	done := false;
		found := false;
		repeat
			getdirslot(volid, slotno, dirslot, error);
			{ writeln('** slot ', slotno, ' flags: ', dirslot.flags, ' name:', dirslot.name, ' error:', error); }
			if not (SlotDeleted in dirslot.flags) and (SlotFirst in dirslot.flags)
				and (name = dirslot.name) then
			begin
				findfile := slotno;
				done := true;
				found := true;
				{ writeln('** found at slot ', slotno); }
			end
			if SlotEndScan in dirslot.flags then
				done := true;

			slotNo := slotNo + 1;
		until done or (slotNo >= maxSlots) or (error <> 0);

		if (error = 0) and (not found) then
			error := IOFileNotFound;
	end;
end;

(* initialize a file record from a directory slot *)
procedure openfile(volid:integer; slotno:integer; var dirslot:DirectorySlot; var aFile:File; mode:filemode);
var extentSize:integer;
begin
	extentSize := volumeTable[volid].part.extentSize;

	aFile.typ := IODiskFile;
	aFile.mode := mode;
	new(aFile.buffer);
	aFile.bufpos := 0;
	aFile.bufsize := DefaultBufSize;
	aFile.needsflush := false;
	aFile.changed := false;
	aFile.lastError := 0;
	aFile.errorAck := false;
	aFile.volumeid := volid;
	aFile.fileno := slotno;
	aFile.filpos := 0;
	aFile.bufStart := 1;
	aFile.size := dirslot.sizeBytes;
	aFile.sizeExtents := dirslot.sizeBytes div extentSize + 1;
	aFile.bufBlocks := DefaultBufBlocks;
	aFile.extentBlocks := extentSize div 512;

	seek(aFile,0);
end;

procedure updatedirslot(var aFile:file);
var dirs: DirectorySlot;
    error: integer;
begin
	getdirslot(aFile.volumeid, aFile.fileno, dirs, error);
	{ writeln('updatedirslot  1 ', aFile.fileno, ' ', error); }
	if error = 0 then
	begin
		dirs.sizeBytes := aFile.size;
		dirs.modTime := GetCurTimestamp;
		putdirslot(aFile.volumeid, aFile.fileno, dirs, error);
	end;
	{ writeln('updatedirslot 2 ', aFile.fileno, ' ', error); }
	fileerror(aFile, error);
end;

procedure close(var aFile:file);
begin
	if aFile.typ = IODiskFile then
	begin
		{ writeln('close needsflush:', aFile.needsflush, ' changed:', aFile.changed, ' error:', aFile.lastError); }
		if aFile.needsflush then
			flushfile(aFile);
		if aFile.lastError = 0 then
		begin
			fileerror(aFile, IOFileClosed);
			{ writeln('close f.buffer:', aFile.buffer); }
			dispose(aFile.buffer);
			aFile.buffer := nil;

			if aFile.changed then
				updatedirslot(aFile);

		end;

		closevolumeid(aFile.volumeid);
	end;
end;

procedure deletefile(volid:integer; slotno:integer; var dirslot:DirectorySlot; var error:integer);
begin
	dirslot.flags := dirslot.flags - [SlotFirst] + [SlotDeleted];
	putdirslot(volid, slotno, dirslot, error);
end;

(* Create a new file. If slotno is not 0, it points to a directory
   slot of an existing file with the same name, and dirslot is set.
   In this case, the old file will be deleted and a new directory slot is
   allocated. The new slot is returned in slotno and dirslot.
   If overwrite is set to false, no file will be created and
   error will be set to IOFileExists. *)
procedure createfile(volid:integer; name:filenamestr; overwrite:boolean;
	var slotno:integer; var dirslot:DirectorySlot; var error:integer);
var generation:integer;
    done:boolean;
    oldslotno:integer;
    olddirslot:DirectorySlot;
    createTs:Timestamp;
    nowTs:Timestamp;
begin
	generation := 0;
	oldslotno := findfile(volid, name, olddirslot, error);

	if (not overwrite) and (oldslotno > 0) then
	begin
		(* TODO: this is redundant, see open which
			is the only point from where createfile
			is called *)
		error := IOFileExists;
		slotno := -1;
	end
	else
	begin
		nowTs := GetCurTimestamp;

		if overwrite and (oldslotno > 0) then
		begin
			(* if we overwrite a file, increment
				generation number and use the
				old creation time *)
			generation := olddirslot.generation + 1;
			createTs := olddirslot.createTime;
		end
		else
			createTs := nowTs;

		slotno := finddirslot(volid, error);
		if slotno <= 0 then
			error := IONoSpace
		else
		if (slotno > 0) and (error = 0) then
		begin
			getdirslot(volid, slotno, dirslot, error);
			dirslot.name := name;
			dirslot.flags := [SlotFirst];
			dirslot.sizeBytes := 0;
			dirslot.generation := generation;
			dirslot.owner := 0;
			dirslot.modTime := nowTs;
			dirslot.createTime := createTs;
			putdirslot(volid, slotno, dirslot, error);
			if overwrite and (oldslotno > 0) then
				deletefile(volid, oldslotno, olddirslot, error);
		end
	end;
end;

procedure initDevices;
begin
	if cardchanged then
		DevicesInitialized := false;

	(* we just handle one sdcard device here *)
	if not DevicesInitialized then
	begin
		DefaultVolumeId := 0;

		initsdcard;
		volumeCount := 0;
		readPartitions(0);	
		DevicesInitialized := true;

		(* DefaultVolume may be set by the shell *)
		if length(DefaultVolume) > 0  then
			DefaultVolumeId := findvolume(DefaultVolume);

		(* If DefaultVolumeId is still not set, just use the
		   first volume. *)
		if (DefaultVolumeId = 0) and (volumeCount > 0) then
			DefaultVolumeId := 1;
	end;
end;

procedure readdirnext(volid:integer; var index:integer; var dirslot:DirectorySlot; var error:integer);
var lastSlot:integer;
    found:boolean;
begin
	lastSlot := volumeTable[volid].part.dirSize - 1;
	found := false;

	repeat
		getdirslot(volid, index, dirslot, error);
		index := index + 1;
		found := SlotFirst in dirslot.flags;
	until found or (SlotEndScan in dirslot.flags) or
		(index = lastSlot) or (error <> 0);

	if not found then
		index := -1;
end;

procedure readdirfirst(volid:integer; var index:integer; var dirslot:DirectorySlot; var error:integer);
begin
	initDevices;
	index := volumeTable[volid].startSlot;
	readdirnext(volid, index, dirslot, error);
end;

function charpos(searchChar:char; var s:string):integer;
var c:char;
    p:integer;
begin
	charpos := 0;
	p := 1;

	for c in s do
	begin
		if c = searchChar then
		begin
			charpos := p;
			break;
		end;
		p := p + 1;
	end;
end;

(* Open volume by name and search for a file.
   Increases the open counter of the volume,
   so you need to call closevolumeid() at some point later.
*)
procedure openvolpath(path:pathnamestr; var volid:integer;
	var fname:filenamestr;
	var slotno:integer; var dirs:DirectorySlot; var error:integer);
var i:integer;
    separatorPos:integer;
    volname:filenamestr;
begin
	initDevices;
	slotno := 0;
	error := 0;
	volid := 0;

	if path[1] = '#' then
	begin
		separatorPos := charpos(':', path);
		if separatorPos > 0 then
		begin
			volname := copy(path, 2, separatorPos - 2);
			fname := copy(path, separatorPos + 1, length(path) - separatorPos);
			{ writeln('openvolpath volname:', volname, ' fname:', fname, ' separatorPos:', separatorPos); }
			volid := findvolume(volname);
		end
	end
	else
	begin
		volid := DefaultVolumeId;
		fname := path;
	end;

	if volid > 0 then
	begin
		openvolumeid(volid);
		slotno := findfile(volid, fname, dirs, error)
	end
	else
		error := IOVolNotFound;

	(* writeln('openvolpath ', path, ' volid ', volid, ' slotno ', slotno, ' error ', error); *)
end;

procedure rename(oldname:filenamestr; newname:filenamestr; var error:integer);
var olddirs:DirectorySlot;
    newdirs:DirectorySlot;
    volid:integer;
    oldslotno:integer;
    newslotno:integer;
    fname:filenamestr;
begin
	volid := 0;

	(* cannot specify a volume name in the new filenamestr specification,
		or a channel name *)
	if newname[1] in [ '#', '%' ] then
		error := IOPathInvalid
	else
	begin
		(* locate the old file *)
		openvolpath(oldname, volid, fname, oldslotno, olddirs, error);
		if error = 0 then
		begin
			{ writeln('rename slot ', oldslotno, ' checking for ', newname); }
			(* check if new filenamestr already exists *)
			newslotno := findfile(volid, newname, newdirs, error);
			if error = IOFileNotFound then
			(* if new filename was not found, we can rename *)
			begin
				error := IONoError;
				olddirs.name := newname;
				putdirslot(volid, oldslotno, olddirs, error);
			end
			else
			if error = 0 then
			(* if new filename was found, we can not rename
				and return an error *)
				error := IOFileExists;

			(* otherwise we return the error set by findfile *)
		end;
		if volid > 0 then
			closevolumeid(volid);
	end;
end;

procedure erase(name:pathnamestr; var error:integer);
var dirs:DirectorySlot;
    volid:integer;
    slotno:integer;
    fname:filenamestr;
begin
	error := 0;

	if name[1] = '%' then
		error := IOPathInvalid
	else
	begin
		(* locate the file *)
		openvolpath(name, volid, fname, slotno, dirs, error);
		{ writeln('** erase slot ', slotno, ' e:', error); }
		if error = 0 then
		begin
			if SlotReadonly in dirs.flags then
				error := IOReadOnly
			else
				deletefile(volid, slotno, dirs, error);
		end;

		if volid > 0 then
			closevolumeid(volid);
	end;
end;

procedure writechannel(var f:file; aChar:char);
begin
	conout(aChar);
end;

procedure writechannelw(var f:file; word:integer);
begin
	conoutw(word);
end;

procedure echochannel(var f:file; aChar:char);
begin
	if not f.noecho then
	begin
		if f.raw then
			writechannel(f, aChar)
		else
		if (aChar <> #8) and (aChar <> #127) and (aChar <> #4) then
		begin
			writechannel(f,aChar);
			if aChar = #13 then
				writechannel(f, #10);
		end;
	end;
end;

function readchannel(var f:file):char;
var aChar:char;
begin
	if f.buflen > 0 then
	begin
		aChar := f.bufchar;
		f.buflen := 0;
	end
	else
	begin
		aChar := conin();
		echochannel(f, aChar);
	end;

	f.ateof := aChar = #4;  (* set atEof flag if ^D entered *)

	if (f.nointr = false) and (aChar = #3) then
	begin
		fileerror(f, IOUserIntr);
		checkerror(f)
	end;

	readchannel := aChar;
end;

function freadchar(var f:file):char;
var error:integer;
begin
	if f.typ = IOChannel then
		freadchar := readchannel(f)
	else
		freadchar := readfschar(f);

	f.ateoln := (freadchar = #13) or (freadchar = #10);
end;

procedure fwritechar(aChar:char; var f:file);
begin
	if f.typ = IOChannel then
		writechannel(f, aChar)
	else
		writefschar(f, aChar);
end;

procedure fwritestring(var aString:string; var f:file; w:integer);
var ch:char;
    missing,i:integer;
begin
	missing := w - length(aString);
	if missing > 0 then
	begin
		for i := 1 to missing do
			fwritechar(' ', f);
	end;

	if f.typ = IOChannel then
	begin
		for ch in aString do
			writechannel(f, ch)
	end
	else
	begin
		{ writeln('fwritestring to file'); }
		writefsstring(f, aString);
	end;
end;

procedure fwriteint(v:integer; var f:file; w:integer);
var rbuf:string[12];
begin
	(* use field width 0 for intstr because fwritestring can
           handle any widths without needing a buffer *)
	intstr(v, 0, rbuf);
	fwritestring(rbuf, f, w);
end;

procedure fwritereal(v:real; var f:file; w,d:integer);
var rbuf:string[48];
begin
	realstr(v, w, d, rbuf);
	fwritestring(rbuf, f, w);
end;

(* size must be multiple of word size (hardcoded to be 4) *)
procedure fwritewords(words:^IOBuffer; var f:file; size:integer);
begin
	if f.typ = IODiskFile then
		writefs(f, words, size)
	else
		writechanwords(f, words, size shr 2);
end;

(* size must be multiple of word size (hardcoded to be 4) *)
procedure freadwords(words:^IOBuffer; var f:file; size:integer);
var w,count:integer;
begin
	if f.typ = IODiskFile then
		readfs(f, words, size)
	else
		readchanwords(f, words, size shr 2);
end;

(* Pushes one character back onto an input stream.
   For a channel, the next character read will be aChar.

   For a disk file, aChar is ignored and the file position
   is just changed.

   It is not valid to push back a character if the seek position is 0.
*)

procedure pushback(var aFile:file; aChar:char);
begin
	if aFile.typ = IODiskFile then
		seek(aFile, aFile.filpos - 1)
	else
	begin
		aFile.bufchar := aChar;
		aFile.buflen := 1;
	end;
end;

procedure openchannel(name:filenamestr; var f:file; mode:filemode; var error:integer);
begin
	f.typ := IOChannel;
	f.mode := mode;
	f.buflen := 0;
	f.ateof := false;
	f.noecho := false;
	f.raw  := false;
	f.nointr := false;

	if name = '%CON' then
		f.channelid := 0
	else
	if name = '%KBD' then
	begin
		f.channelid := 0;
		f.noecho := true;
		f.raw := true;
	end
	else
	if name = '%RAW' then
	begin
		f.channelid := 0;
		f.noecho := true;
		f.raw := true;
		f.nointr := true;
	end
	else
		error := IOFileNotFound;
end;

procedure open(var f:file; name:pathnamestr; mode: filemode);
var error:integer;
    dirs:DirectorySlot;
    slotno:integer;
    volid:integer;
    exclusive:boolean;
    overwrite: boolean;
    createmissing: boolean;
    fname:filenamestr;
begin
	if name[1] = '%' then
		openchannel(name, f, mode, error)
	else
	begin
		volid := 0;

		exclusive := (mode = ModeCreate);
		overwrite := (mode = ModeOverwrite);
		createmissing := (mode = ModeCreate) or (mode = ModeOverwrite) or (mode = ModeAppend);

		openvolpath(name, volid, fname, slotno, dirs, error);

		if (error = 0) and exclusive then
		begin
			fileerror(f, IOFileExists);
			error := IOFileExists;
		end;

		if ((error = IOFileNotFound) and createmissing) or
		   ((error = 0) and overwrite) then
			(* TODO: overwrite flag is redundant, if we get here,
				we always want the file overwritten *)
			createfile(volid, fname, overwrite, slotno, dirs, error);

		if error = 0 then
		begin
			openfile(volid, slotno, dirs, f, mode);

			if mode = ModeAppend then
				seek(f, f.size);
		end;

		if (error <> 0) and (volid > 0) then
			closevolumeid(volid);

		if error <> 0 then
			fileerror(f, error);
	end;
end;

procedure noecho(var f:file;noecho:boolean;var old:boolean);
begin
	if f.typ <> IOChannel then
		fileerror(f, IOInvalidOp)
	else
	begin
		old := f.noecho;
		f.noecho := noecho;
	end;
end;

(*
  implementation of Xorshift algorithm by George Marsaglia,
  see: Marsaglia, George (July 2003).
  "Xorshift RNGs". Journal of Statistical Software. 8 (14).
   doi:10.18637/jss.v008.i14
*)

function random:integer;
var x:integer;
begin
	x := random_state;
	x := x xor (x shl 13);
	x := x xor (x shr 17);
	x := x xor (x shl 5);

	random_state := x;
	if x < 0 then x := abs(x);
	random := x;
end;

procedure randomize;
begin
	random_state := getticks() xor $AFFECAFE;
end;

(* there is already an assembly routine upcase
   in lib.s, so we do not need this one. *)
{
function upcase(aChar:char):char;
begin
	(* use cascaded IF to make it a teeny bit faster
		than using AND *)
	if ord(aChar) >= ord('a') then
		if ord(aChar) <= ord('z') then
			upcase := chr(ord(aChar) - 32)
		else
	else
		upcase := aChar;
end;
}

{$I 'stdterm.inc'}  (* terminal handling procedures *)

(* Execute a program from a file.
   If there is an error accessing the file, this procedure
   returns and sets the error variable accordingly.
   Otherwise, program execution is turned over to
   the new program and PExec does not return.

   The arguments for the new program is passed with
   the args array. argCount specifies how many arguments
   are actually used. If argCount is invalid (negative or
   larger than the maximum (PArgLast + 1), PExec returns
   with the error code set to IOInvalidOp.
 *)

procedure PExec(prgfile:pathnamestr; var args:PArgVec; argCount:integer;var error:integer);
var volid:integer;
    fname:filenamestr;
    dirslot:DirectorySlot;
    slotno:integer;
    i:integer;
    startblock:integer;
    physblock:integer;
    devId:integer;
begin
	if (argCount >= PArgMax) or (argCount < 0) then
		error := IOInvalidOp
	else
	begin
		openvolpath(prgfile, volid, fname, slotno, dirslot, error);
		if error = 0 then
		begin
			with VolumeTable[volid] do
			begin
				(* get the physical device id from the volume table *)
				devId := deviceId;
				(* calculate start block of the file
					relative to volume start *)
				startblock := slotno * part.extentSize div 512;
			end;

			(* get physical block number *)
			physblock := getPhysBlockNo(volid, startblock);
			closevolumeid(volid);

			(* set external Pargs array, clear the array elements which are
				not used *)
			PArgs[0] := prgfile;
			for i := 1 to argCount do
				PArgs[i] := args[i-1];
			for i := argCount + 1 to PArgMax do
				PArgs[i] := '';
			PArgCount := argCount;

			(* this will overwrite the current program *)
			coreload(devId, physblock, dirslot.sizeBytes);
		end;
	end;
end;

procedure PExec2(prgfile:pathnamestr; arg1:string; var error:integer);
var args:PArgVec;
begin
	args[0] := arg1;
	PExec(prgfile, args, 1, error);
end;

procedure PExec3(prgfile:pathnamestr; arg1, arg2:string; var error:integer);
var args:PArgVec;
begin
	args[0] := arg1;
	args[1] := arg2;
	PExec(prgfile, args, 2, error);
end;

function ParamStr(i:integer):string;
begin
	if (i < 0 ) or (i > PArgMax) then
		ParamStr := ''
	else
		ParamStr := PArgs[i];
end;

function ParamCount():integer;
begin
	ParamCount := PArgCount;
end;

procedure SetShellCmd(cmd:string[40]; arg:integer);
begin
	ShellCmd := cmd;
	ShellArg := arg;
end;

PROCEDURE delay(ms:INTEGER);
VAR count:INTEGER;
BEGIN

        count := ms;
        WHILE count > 0 DO
        BEGIN
                WAIT1MSEC;
                count := count - 1;
        END;
END;

end.
