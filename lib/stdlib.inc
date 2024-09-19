(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
{ const pi = 3.14169253; }
const MaxInt = 2147483647;

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
                (* dirFile: ^file;      (* pseudo-file for accessing the directory *)
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

var input,output:file external;
var DefaultVolumeId:integer external; (* do we need this here? *)
    VolumeTable: array [1..MaxVolumes] of Volume external; (* and this *)
    VolumeCount: integer external; (* and this *)

    DefaultVolume: volumenamestr external;

    SysBootTicks, SysLastTicks:integer external;
    SysClock:DateTime external;

(* from graphics.s *)
PROCEDURE DRAWLINE(x1,y1,x2,y2, color:INTEGER); EXTERNAL;
PROCEDURE PUTPIXEL(x,y, color:INTEGER); EXTERNAL;
PROCEDURE CLEARGRAPHICS; EXTERNAL;
PROCEDURE INITGRAPHICS; EXTERNAL;
PROCEDURE SETPALETTE(slot, color:INTEGER);EXTERNAL;
PROCEDURE PUTSCREEN(VAR pixeldata: ARRAY [0..31999] OF INTEGER); EXTERNAL;

function conin():char; external;
procedure conout(c:char); external;
FUNCTION CONAVAIL:BOOLEAN; EXTERNAL;
PROCEDURE WAIT1MSEC; EXTERNAL;
function upcase(aChar:char):char; external;
function getticks():integer; external;

(* from float32.s *)
function shiftfloat32(aReal:real; shiftCount:integer):real; external;
function getfloat32exp(aReal:real):integer; external;

(* from runtime.s *)
FUNCTION LENGTH(s:STRING):INTEGER; EXTERNAL;
FUNCTION MAXLENGTH(s:STRING):INTEGER; EXTERNAL;
procedure appendchar(var s:string; aChar:char); external;
procedure strmoveup(var s:string;index,length,delta:integer); external;
procedure strmovedown(var s:string;index,length,delta:integer); external;
procedure RuntimeError(var s:string); external;

(* from stdlib *)
function copy(s:string;index,count:integer):string; external;
procedure insert(ins: string; var dest: string; position:integer); external;
procedure delete(var s:string; from:integer; count:integer); external;
function pos(substr:string;var s:string):integer; external;
function pwroften(exp:integer):real; external;
function exp(exponent:real):real; external;
function ln(power:real):real; external;
function sqrt(n:real):real; external;
function floor(x:real):integer; external;
function round(x:real):integer; external;
function sin(x:real):real; external;
function cos(x:real):real; external;
function arctan(x:real):real; external;
function tan(x:real):real; external;
function cotan(x:real):real; external;

procedure fillchar(var s:string; startpos,count:integer; theChar:char); external;

procedure cardinitv2; external;
function cardsize:integer; external;
function cardchanged:boolean; external;

procedure readpartblk(blkno:integer;var partblk:PartitionTableBlock;
	var error:integer;devid: integer); external;
procedure readdirblk(blkno:integer;var dirblk:DirBlock;
	var error:integer;devid: integer); external;
procedure readblock(blkno:integer;var buf:IOBlock;
	var error:integer; devid: integer); external;

procedure writedirblk(blkno:integer;var dirblk:DirBlock;
	var error:integer;devid: integer); external;
procedure writepartblk(blkno:integer;var partblk:PartitionTableBlock;
	var error:integer;devid: integer); external;
procedure writeblock(blkno:integer;var buf:IOBlock;
	var error:integer; devid: integer); external;

procedure copybuf(dest:^IOBuffer;destOffset:integer; src:^IOBuffer; srcOffset:integer; length: integer); external;
function readfschar(var f:file):char; external;
procedure writefschar(var f:file; aChar:char); external;
procedure writefsstring(var f:file; var s:string); external;

procedure conoutw(w:integer); external;
function coninw():integer; external;

procedure SetDefaultVolume(volname:volumenamestr); external;
procedure addPartitions(devid:integer; var partblk:PartitionTableBlock; var isLast:boolean); external;
procedure readPartitions(devid: integer); external;
procedure initDevices; external;
procedure readdevice(deviceId:integer;blockNo:integer;var buf:IOBlock; var error:integer); external;
procedure writedevice(deviceId:integer;blockNo:integer;var buf:IOBlock; var error:integer); external;
procedure readvolumeblks(volumeid:integer; destbuf:^iobuffer; blkno:integer; blkCount: integer; var error:integer);
	external;
procedure writevolumeblks(volumeid:integer; srcbuf:^iobuffer; blkno:integer; blkCount: integer; var error:integer);
	external;
function findvolume(name:string):integer; external;
procedure openvolumeid(volid:integer); external;
procedure closevolumeid(volid:integer); external;
function IOResult(var fil:file):integer; external;
function ErrorStr(err:integer):string; external;
function eof(var fil:file):boolean; external;
function eoln(var fil:file):boolean; external;
procedure readfs(var fil:file; destbuf:^IOBuffer; len:integer); external;
procedure flushfile(var fil:file); external;
procedure seek(var fil:file; position:integer); external;
function filepos(var fil:file):integer; external;
function filesize(var fil:file):integer; external;
procedure extendfile(var fil:file; newSize:integer); external;
procedure writefs(var fil:file; srcbuf:^IOBuffer; len:integer); external;
procedure close(var aFile:file); external;
procedure readdirnext(volid:integer; var index:integer; var dirslot:DirectorySlot; var error:integer); external;
procedure readdirfirst(volid:integer; var index:integer; var dirslot:DirectorySlot; var error:integer); external;
function charpos(searchChar:char; var s:string):integer; external;
procedure rename(oldname:filenamestr; newname:filenamestr; var error:integer); external;
procedure erase(name:pathnamestr; var error:integer); external;
function readchannel(var f:file):char; external;
procedure writechannel(var f:file; aChar:char); external;
function freadchar(var f:file):char; external;
procedure fwritechar(aChar:char; var f:file); external;
procedure fwritestring(var aString:string; var f:file; w:integer); external;
procedure fwriteint(v:integer; var f:file; w:integer); external;
procedure fwritereal(v:real; var f:file; w,d:integer); external;
procedure pushback(var aFile:file; aChar:char); external;
procedure skipeoln(var aFile:file); external;
procedure fscanbuf(var aFile:file; mode: fscanmode; var buf:string); external;
procedure freadint(var v:integer;var f:file); external;
procedure freadreal(var v:real;var f:file); external;

procedure openchannel(name:filenamestr; var f:file; mode:filemode; var error:integer); external;
procedure open(var f:file; name:pathnamestr; mode: filemode); external;
procedure noecho(var f:file; noecho:boolean; var old:boolean); external;

procedure intstr(v:integer; fieldWith:integer; var rbuf:string);
	external;
procedure realstr(x:real;  w, d: integer; var s: string[30]); external;
procedure intval(s:string; var value,code:integer); external;
procedure realval(s:string; var value:real;var code:integer); external;
function isdigit(aChar:char):boolean; external;
function iswhite(aChar:char):boolean; external;

procedure halt; external;

function random:integer; external;
procedure randomize; external;

(* from stdterm.inc *)
procedure ClrScr; external;
procedure ClrEol; external;
procedure CrtInit; external;
procedure GotoXY(x,y:integer); external;
procedure InsLine; external;
procedure DelLine; external;
procedure GetCursorPos(var x,y:integer); external;
procedure GetTermSize(var maxx,maxy:integer); external;
procedure TextColor(col:integer); external;
procedure TextBackground(bgcol:integer); external;
procedure TextDefault; external;

procedure PTerm; external; (* from runtime.s *)
procedure PExec(prgfile:pathnamestr; var args:PArgVec; argCount:integer;var error:integer); external;
procedure PExec2(prgfile:pathnamestr; arg1:string; var error:integer); external;
procedure PExec3(prgfile:pathnamestr; arg1, arg2:string; var error:integer); external;
function ParamStr(i:integer):string; external;
function ParamCount():integer; external;

procedure SetShellCmd(cmd:string[40];arg:integer); external;

function GetTime:DateTime; external;
function TimeStr(d:DateTime;showSeconds:boolean):string; external;
function DateStr(d:DateTime):string; external;
function GetTimestamp(var d:DateTime):integer; external;
function GetDateTime(ts:Timestamp):DateTime; external;
procedure delay(ms:integer); external;
