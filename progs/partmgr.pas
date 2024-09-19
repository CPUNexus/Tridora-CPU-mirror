(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
program partmgr;
const MaxPartitions = 32;
      LastPartBlock = 7;
      PartsPerBlock = 8;

var partTable:array[0..LastPartBlock] of PartitionTableBlock;
    changed:array[0..LastPartBlock] of boolean;
    detectedCardSize:integer;
    cmd:char;
    done:boolean;
    lastPartNo:integer;

function flags2str(flags:PartFlags):string;
begin
	flags2str := '';

	if PartEnabled in flags then flags2str := flags2str + 'E ';
	if PartBoot in flags then flags2str := flags2str + 'B ';
	if PartLast in flags then flags2str := flags2str + 'L ';
	if PartPhysical in flags then flags2str := flags2str + 'P ';
	if PartDefault in flags then flags2str := flags2str + 'D ';
end;

function str2flags(var s:string):PartFlags;
begin
	str2flags := [];

	if 'E' in s then str2flags := str2flags + [PartEnabled];
	if 'B' in s then str2flags := str2flags + [PartBoot];
	if 'L' in s then str2flags := str2flags + [PartLast];
	if 'P' in s then str2flags := str2flags + [PartPhysical];
	if 'D' in s then str2flags := str2flags + [PartDefault];
end;

function sanitizeName(var name:string):string;
begin
	if (length(name) <= 32) and (maxlength(name) = 32) then
		sanitizeName := name
	else
		sanitizeName := '<invalid>';
end;

procedure changeNumber(prompt:string; var num:integer);
var buf:string;
    err:integer;
    digits:string;
begin
	str(num, digits);
	buf := prompt + ' [' + digits + ']> ';
	write(buf:30);
	readln(buf);
	val(buf,num,err);
end;


procedure readPartTable;
var done:boolean;
    curblk:integer;
    error:integer;
    devid:integer;
begin
	done := false;
	curblk := 0;
	devid := 0;	(* we only support one device *)

	while not done do
	begin
		changed[curBlk] := false;

		readPartBlk(curblk, partTable[curblk], error, devid);
		if error <> 0 then
		begin
			done := true;
			writeln('Error ', error,
				' reading partition block ', curblk);
		end
		else
			curblk := curblk + 1;


		if curBlk > LastPartBlock then
			done := true;
	end;
end;

procedure writePartBlock(no:integer);
var error:integer;
    devid:integer;
begin
	devid := 0;
	writePartBlk(no, partTable[no], error, devid);
	if error <> 0 then
			writeln('Error ', error,
				' reading partition block ', no);
end;

procedure writePartitions;
var blkNo:integer;
begin
	for blkNo := 0 to LastPartBlock do
	begin
		if changed[blkNo] then
		begin
			writeln('Writing back partition block ',blkNo);
			writePartBlock(blkNo);
		end;
	end;
end;

function getPartition(partNo:integer):Partition;
var blkNo:integer;
begin
	blkNo := partNo div PartsPerBlock;
	if (blkNo < 0) or (blkNo > LastPartBlock) then
		writeln('internal error: invalid part no in getPartition')
	else
		getPartition := partTable[blkNo][partNo mod PartsPerBlock];
	{ writeln('** getPartition: ', blkNo, ' ', partNo mod PartsPerBlock); }
end;

procedure putPartition(var part:Partition; partNo:integer);
var blkNo:integer;
begin
	blkNo := partNo div PartsPerBlock;
	{ writeln('** putPartition: ', blkNo, ' ', partNo mod PartsPerBlock); }
	if (blkNo < 0) or (blkNo > LastPartBlock) then
		writeln('internal error: invalid part no in getPartition')
	else
	begin
		partTable[blkNo][partNo mod PartsPerBlock] := part;
		changed[blkNo] := true;
	end;
end;

function isEmptyPart(var part:Partition):boolean;
begin
	isEmptyPart := (part.startBlock = 0) and (part.blocks = 0);
end;

procedure printPartTable;
var blkNo, partNo:integer;
    partBlk:PartitionTableBlock;
    part:Partition;
    totalPartNo:integer;
begin
	totalPartNo := 0;

	writeln('Partition Table:');
	writeln('No.   ', 'Flags':11, 'Name':32, 'Start':10, 'Size':10);

	for blkNo := 0 to LastPartBlock do
	begin
		partBlk := partTable[blkNo];
		for partNo := 0 to PartsPerBlock - 1 do
		begin
			part := partBlk[partNo];

			if not isEmptyPart(part) then
			begin
				write(totalPartNo:3, ':  ', flags2Str(part.flags):11,
					sanitizeName(part.name):32,
					part.startBlock:10,
					part.blocks:10);
				if PartBoot in part.flags then write(' ', part.bootBlocks);
				writeln;
				lastPartNo := totalPartNo;
			end;
			totalPartNo := totalPartNo + 1;
		end;
	end;
	writeln('Flags: P=Physical  B=Boot  E=Enabled  L=Last  D=Default');
end;

function askPartNo:integer;
var i:integer;
    s:string;
begin
	askPartNo := -1;
	write('Enter partition number (0-', lastPartNo, ')> ');
	readln(s);
	if length(s) > 0 then
	begin
		val(s,askPartNo,i);
		if i > 0 then
			writeln('Invalid partition number');
	end;
end;

function askConfirmPartNo:integer;
var partNo:integer;
    part:Partition;
    answer:char;
begin
	askConfirmPartNo := -1;

	partNo := askPartNo;

	if partNo >= 0 then
	begin
		part := getPartition(partNo);
		write('Any data on partition ', partNo,
			' (', sanitizeName(part.name), ') ',
			'will be destroyed. Sure [y/n]? ');
		readln(answer);
		if upcase(answer) = 'Y' then
			askConfirmPartNo := partNo;
	end;
end;

function guessExtentSize(blocks:integer):integer;
begin
	if blocks >= 4194304 then (* 2 GB *)
		guessExtentSize := 1048576 (* use 1MB extents  *)
	else
	if blocks >= 1048576 then (* 512 MB *)
		guessExtentSize := 524288 (* use 512K extents *)
	else
	if blocks >=  524288 then (* 256 MB *)
		guessExtentSize := 131072
	else
	if blocks >=  262144 then (* 128 MB *)
		guessExtentSize := 65536
	else
	if blocks >= 32768 then (* 16 MB *)
		guessExtentSize := 16384
	else 
		guessExtentSize := 8192;
end;

function getDirSize(extentSize,blocks:integer):integer;
begin
	getDirSize := blocks div (extentSize div 512);
end;

procedure createFilesystem(partNo:integer); forward;

procedure addVolume;
var nextFreeBlock:integer;
    nextBlock:integer;
    extentSize:integer;
    size:integer;
    i:integer;
    part:Partition;
    maxBlocks:integer;
    freeBlocks:integer;
    newPartNo:integer;
    newPart:Partition;
begin
	part := getPartition(0);
	maxBlocks := part.blocks;
	nextFreeBlock := 0;
	(* read all partitions *)
	for i := 1 to lastPartNo do
	begin
		part := getPartition(i);
		nextBlock := part.startBlock + part.blocks;
		if nextBlock > nextFreeBlock then
			nextFreeBlock := nextBlock;
	end;
	(* remember last used block *)
	writeln('next free partition: ', lastPartNo + 1,
		' next free block: ', nextFreeBlock);

	freeBlocks := maxBlocks - nextFreeBlock;

	if freeBlocks < 1 then
		writeln('Cannot add partition - no free blocks after last partition.')
	else
	begin
		newPartNo := lastPartNo + 1;
		(* remove last partition flag on previous last partition *)
		part.flags := part.flags - [PartLast];
		putPartition(part, lastPartNo);

		(* create new partition *)
		size := freeBlocks;
		changeNumber('Size (blocks)', size);
		write('Name> ':30);
		readln(newPart.name);

		newPart.startBlock := nextFreeBlock;
		newPart.blocks := size;
		newPart.extentSize := guessExtentSize(size);
		newPart.dirSize := getDirSize(newPart.extentSize, size);
		newPart.bootBlocks := 0;
		(* mark new partition as last partition *)
		newPart.flags := [ PartEnabled, PartLast ];
		putPartition(newPart, newPartNo);

		writeln('Partition ', newPartNo, ' created, extent size:', newPart.extentSize,
			' directory size: ', newPart.dirSize);
		createFilesystem(newPartNo);

		lastPartNo := lastPartNo + 1;
	end;
end;

procedure renameVolume;
var partNo:integer;
    newName:string;
    part:Partition;

begin
	partNo := askPartNo;
	if partNo >= 0 then
	begin
		part := getPartition(partNo);
		writeln('Old partition/volume name: ', sanitizeName(part.name));
		write('New partion/volume name:   ');
		readln(newName);
		if length(newName) > 0 then
		begin
			part.name := newName;
			putPartition(part, partNo);
		end;
	end;
end;

procedure toggleDefaultFlag;
var partNo:integer;
    part:Partition;

begin
	partNo := askPartNo;
	if partNo >= 0 then
	begin
		part := getPartition(partNo);
		write('Default flag ');
		if PartDefault in part.flags then
		begin
			part.flags := part.flags - [PartDefault];
			write('cleared');
		end
		else
		begin
			part.flags := part.flags + [PartDefault];
			write('set');
		end;
		writeln(' on partition ', partNo, ' (', sanitizeName(part.name), ').');
		putPartition(part, partNo);
	end;
end;

procedure deleteVolume;
var partNo:integer;
    part:Partition;
begin
	partNo := askConfirmPartNo;
	if partNo >= 0 then
	begin
		part := getPartition(partNo);
		part.flags := [];
		part.name := '';
		part.startBlock := 0;
		part.blocks := 0;
		part.extentSize := 0;
		part.dirSize := 0;
		part.bootBlocks := 0;
		putPartition(part, partNo);

		writeln('Partition ', partNo, ' deleted.');

		(* try to fix last partition flag *)
		(* only works if the previous entry has
			a valid partition *)
		if partNo = lastPartNo then
		begin
			lastPartNo := lastPartNo - 1;
			part := getPartition(lastPartNo);
			part.flags := part.flags + [PartLast];
			putPartition(part, lastPartNo);
		end;
	end;
end;

procedure validatePartTable;
var partNo:integer;
    phys:Partition;
    part,part2:Partition;
    answer:char;
    p,p2:integer;
    valid:boolean;
begin
	valid := true;

	phys := getPartition(0);
	if not (PartPhysical in phys.flags) then
	begin
		writeln('PHYS partition missing, initialize card first!');
		exit;
	end;

	if phys.blocks <> detectedCardSize then
	begin
		write('PHYS partition size does not match detected card size, fix? [y/n]');
		readln(answer);
		if upcase(answer) = 'Y' then
		begin
			phys.blocks := detectedCardSize;
			putPartition(phys,0);
		end
		else
			valid := false;
	end;

	for p := 1 to lastPartNo do
	begin
		part := getPartition(p);
		if (part.startBlock < 0) or (part.startBlock + part.blocks > phys.blocks) then
		begin
			writeln('Partition ', p, ' outside of physical bounds.');
			valid := false;
		end;

		if PartEnabled in part.flags then
			if part.dirSize <> getDirSize(part.extentSize, part.blocks) then
			begin
				write('Partition ', p, ' has an invalid directory size (is ');
				writeln(part.dirSize, ', should be ',
					getDirSize(part.extentSize, part.blocks), ').');
				valid := false;
			end;

		for p2 := 1 to lastPartNo do
		begin
			part2 := getPartition(p2);
			if (p <> p2) then
			begin
				if ((part.startBlock >= part2.startBlock) and
					(part.startBlock < part2.startBlock + part2.blocks)) or
					((part2.startBlock > part.startBlock) and
					(part2.startBlock < part.startBlock + part.blocks))
				then
				begin
					writeln('Partition ',p ,' overlaps with partition ', p2);
					valid := false;
				end;

				if (part.name = part2.name) and (p > p2) then
				begin
					writeln('Duplicate volume name ', part.name);
					valid := false;
				end;
			end;
		end;
	end;
	write('Partition table is ');
	if not valid then write('in');
	writeln('valid.');
end;

procedure checkNewCard; forward;

procedure initializeCard;
var part:Partition;
    answer:char;
    p:integer;
begin
	writeln('Initializing a card will create an empty partition table with');
	writeln('the standard PHYS and BOOT partitions.');
	write('This will likely destroy any data on the card - sure? [y/n] ');
	readln(answer);
	if upcase(answer) <> 'Y' then exit;

	(* create PHYS partition using detectedcardblocks *)
	part.name := 'PHYS';
	part.startBlock := 0;
	part.blocks := detectedCardSize;
	part.flags := [PartPhysical];
	part.extentSize := 0;
	part.dirSize := 0;
	part.bootBlocks := 0;
	putPartition(part,0);

	(* create BOOT partition without PartBoot flag *)
	part.name := 'BOOT';
	part.startBlock := 16; (* 16 possible partition blocks with 8 partitions each *)
	part.blocks := 8192 - 16; (* align first volume to 4MB *)
	part.flags := [PartBoot, PartLast];
	putPartition(part,1);

	part.name := '';
	part.startBlock := 0;
	part.blocks := 0;
	part.flags := [];

	for p := 2 to 7 do
		putPartition(part,p);

	writeln('Empty partition table created.');
end;

procedure createFilesystem(partNo:integer);
var firstDirBlock:integer;
    slot:DirectorySlot;
    dirblk:DirBlock;
    dirblockCount:integer;
    metaSlotsCount:integer;
    dirSlotsPerBlock:integer;
    dirSlotsPerExtent:integer;
    part:Partition;
    ts:Timestamp;
    i,b:integer;
    error,devid:integer;
begin
	devid := 0;
	ts := 0;

	part := getPartition(partNo);
	firstDirBlock := part.startBlock;
	dirSlotsPerBlock := 512 div 64;
	dirSlotsPerExtent := part.extentSize div 64;
	dirblockCount := (part.dirSize - 1) div dirSlotsPerBlock + 1;
	metaSlotsCount := (part.dirSize  - 1) div dirSlotsPerExtent + 1;

	writeln('partition size: ', part.blocks);
	writeln('extent size:    ', part.extentSize);
	writeln('directory size: ', part.dirSize);
{	writeln('dirslots per extent:', dirSlotsPerExtent);
	writeln('dirblocks: ', dirblockCount);
	writeln('metaslots: ', metaSlotsCount);
	writeln('first dir block: ', firstDirBlock);
}
	for b := firstDirBlock to firstDirBlock + dirblockCount - 1 do
	begin
		for i := 0 to dirSlotsPerBlock - 1 do
		begin
			if metaSlotsCount > 0 then
			begin
				(* write DIR/Reserved directory slots *)
				slot.name := 'DIR';
				slot.flags := [ SlotReserved ];
				metaSlotsCount := metaSlotsCount - 1;
			end
			else
			begin
				(* write Free + EndScan directory slots *)
				slot.name := '';
				slot.flags := [ SlotFree , SlotEndScan ];
			end;
			slot.sizeBytes := 0;
			slot.createTime := ts;
			slot.modTime := ts;
			slot.generation := 0;
			slot.owner := 0;

			dirBlk[i] := slot;
		end;
		writedirblk(b, dirBlk, error, devid);
		if error > 0 then
			writeln('error writing block ', b, ': ', error);
	end;
	writeln('Volume ', part.name, ' initialized.');
end;

procedure initializeVolume;
var partNo:integer;
    part:Partition;
begin
	partNo := askConfirmPartNo;
	if partNo >= 0 then
	begin
		part := getPartition(partNo);
		if not (PartEnabled in part.flags) then
			writeln('Wrong partition flags (must be Enabled)')
		else
			createFilesystem(partNo);
	end;
end;


procedure rawEdit;
var partNo:integer;
    newName:string;
    part:Partition;
    buf:string;
begin
	writeln('Raw editing partition entry - use caution!');
	
	partNo := askPartNo;
	if partNo >= 0 then
	begin
		part := getPartition(partNo);
		writeln('Volume name: ', sanitizeName(part.name));
		write('Flags> ':30);
		readln(buf);
		if length(buf) > 0 then
			part.flags := str2flags(buf);
		changeNumber('Start block', part.startBlock);
		changeNumber('Size (blocks)', part.blocks);
		changeNumber('Extent size (blocks)', part.extentSize);
		changeNumber('Dir size (slots)', part.dirSize);
		changeNumber('Boot blocks', part.bootBlocks);

		putPartition(part, partNo);
	end;
end;

procedure installBoot;
var bootfile:file;
    name:string;
    part:Partition;
    partNo:integer;
    buf:IOBlock;
    b,blkCount:integer;
    devId:integer;
    error:integer;

procedure readWordsIntoBuf;
var i:integer;
    w:integer;
    c1,c2,c3,c4:char;
begin
	for i := 0 to 127 do
	begin
		if not eof(bootfile) then
		begin
			read(bootfile, c1, c2, c3, c4);
			w := (ord(c1) shl 24) or
				(ord(c2) shl 16) or
				(ord(c3) shl 8) or
				ord(c4);
		end
		else
			w := 0;
		buf[i] := w;
	end;
end;

begin
	devId := 0; (* only one device supported *)
	partNo := 1; (* BOOT partition is always at position 1 *)

	part := getPartition(partNo);
	if part.name <> 'BOOT' then
	begin
		writeln('No BOOT partition at position 1.');
		exit;
	end;

	write('Boot file name> ');
	readln(name);
	if length(name) > 0 then
	begin
		open(bootfile, name, ModeReadonly);
		if IOResult(bootfile) <> 0 then
			writeln('Error opening file: ', ErrorStr(IOResult(bootfile)))
		else
		begin
			blkCount := filesize(bootfile) div 512 + 1;
			if blkCount > part.blocks then
				writeln('Boot partition too small, need ', blkCount)
			else
			begin
				part.bootBlocks := blkCount;
				if not (PartBoot in part.flags) then
				begin
					write('Boot flag set');
					writeln(' on partition ', partNo,
						' (', sanitizeName(part.name), ').');
					part.flags := part.flags + [PartBoot];
				end;
				putPartition(part, partNo);
				for b := 0 to blkCount - 1 do
				begin
					readWordsIntoBuf;
					writeblock(part.startBlock + b, buf, error,  devId);
					if error <> 0 then
						writeln('Error in writeblock ', b, ': ', error);
				end;
				writeln(blkCount, ' boot blocks written.');
			end;
			close(bootfile);
		end;
	end;
end;

procedure showMenu;
begin
	writeln;
        writeln('L)ist partitions  V)alidate partition table  T)oggle default volume flag');
	writeln('A)dd volume  R)ename volume  D)elete volume  I)nitialize volume');
	writeln('Read N)ew card  Initialize C)ard');
	writeln('E)dit partition  Install B)oot file  eX)it');
	write('> ');
end;

procedure invalidCommand;
begin
	writeln('Invalid command.');
end;

procedure command(cmd:char; var done:boolean);
begin
	case cmd of
	'L': printPartTable;
	'A': addVolume;
	'R': renameVolume;
	'D': deleteVolume;
	'V': validatePartTable;
	'T': toggleDefaultFlag;
	'I': initializeVolume;
	'N': checkNewCard;
	'C': initializeCard;
	'B': installBoot;
	'E': rawEdit;
	'X',#24: done := true;
	else invalidCommand;
	end;
end;

function changesPending:boolean;
var i:integer;
begin
	changesPending := false;
	for i := 0 to LastPartBlock do
		if changed[i] then changesPending := true;
end;

procedure checkNewCard;
begin
	if changesPending then
		writeln('WARNING: Discarding partition table changes.');
	initDevices;
	detectedCardSize := cardsize;
	writeln('Detected card size: ', detectedCardSize);
	readPartTable;
	printPartTable;
end;

begin
	checkNewCard;

	repeat
		showMenu;
		read(cmd);
		writeln;
		command(Upcase(cmd), done);
	until done;
	writePartitions;
end.
