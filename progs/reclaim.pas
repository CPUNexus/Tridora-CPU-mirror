(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
program reclaim;
var volname:string;
    ch:char;
    count,areas:integer;

(* we use some stuff internal to stdlib.pas *)
(* procedure readvolumeblks(volumeid:integer; destbuf:^iobuffer; blkno:integer; blkCount: integer; var error:integer);
	external;
procedure writevolumeblks(volumeid:integer; srcbuf:^iobuffer; blkno:integer; blkCount: integer; var error:integer);
	external; *)
procedure getdirslot(volumeid:integer;slotNo:integer;var result:DirectorySlot;var error:integer);
	external;
procedure putdirslot(volumeid:integer;slotNo:integer;var dirslot:DirectorySlot;var error:integer);
	external;

procedure scanVolume(volname:string;dryrun:boolean;verbose:boolean;
			var reclaimCount:integer;var freeAreas:integer);
var volid:integer;
    i:integer;
    error:integer;
    dirslot:DirectorySlot;
    done:boolean;
    fileCount, deletedCount:integer;
    freeCount:integer;
    fileSlotCount:integer;
    reservedCount:integer;
    freeAreaCount:integer;
    inFreeArea:boolean;
    endSlot:integer;
    lastUsed:integer;
    deletedExtent:boolean;

procedure clearDirSlot;
begin
	reclaimCount := reclaimCount + 1;

	if not dryrun then
	begin
		dirslot.name := '';
		dirslot.flags := [SlotFree];
		dirslot.sizeBytes := 0;
		dirslot.createTime := 0;
		dirslot.modTime := 0;
		dirslot.generation := 0;

		putdirslot(volid, i, dirslot, error);
		if error <> IONoError then
		begin
			write('Error writing directory slot ',i);
			writeln(': ', ErrorStr(error));
			done := true;
		end;
	end;
end;

procedure markLastSlot;
var slotNo:integer;
begin
	(* we actually mark the slot after the last used slot *)
	if not dryrun then
	begin
		if lastUsed < endSlot then
		begin
			writeln('Updating directory...');
			slotNo := lastUsed + 1;
			getdirslot(volid, slotNo, dirslot, error);
			if error <> IONoError then
			begin
				write('Error reading directory slot ', slotNo);
				writeln(': ', ErrorStr(error));
			end;

			if not (SlotEndScan in dirslot.flags) then
				dirslot.flags := dirslot.flags + [SlotEndScan];

			putdirslot(volid, slotNo, dirslot, error);
			if error <> IONoError then
			begin
				write('Error writing directory slot ', lastUsed);
				writeln(': ', ErrorStr(error));
			end;
		end;
	end;
end;

procedure beginFreeArea;
begin
	freeCount := freeCount + 1;
	if not inFreeArea then
	begin
		inFreeArea := true;
		freeAreaCount := freeAreaCount + 1;
	end;
end;

procedure endFreeArea;
begin
	if inFreeArea then
		inFreeArea := false;
end;

begin
	volid := findvolume(volname);
	if volid < 1 then
		writeln('Volume ', volname, ' not found.')
	else
	begin
		done := false;
		deletedExtent := false;
		inFreeArea := false;
		fileCount := 0;
		deletedCount := 0;
		reclaimCount := 0;
		freeCount := 0;
		reservedCount := 0;
		fileSlotCount := 0;
		freeAreaCount := 0;
		lastUsed := 0;

		openvolumeid(volid);
		i := volumeTable[volid].startSlot;
		endSlot := volumeTable[volid].part.dirSize - 1;

		if verbose then
		begin
			write('Volume ', volname);
			write('  start slot:', i);
			write('  dir size: ', endSlot + 1);
			writeln('  extent size: ', volumeTable[volid].part.extentSize);
		end;

		writeln('Reading directory...');
		repeat	
			getdirslot(volid, i, dirslot, error);
			if error <> IONoError then
			begin
				write('Error reading directory slot ',i);
				writeln(': ', ErrorStr(error));
				done := true;
			end
			else
			begin
				if SlotEndScan in dirslot.flags then
					done := true;
				if SlotFirst in dirslot.flags then
				begin
					lastUsed := i;
					fileCount := fileCount + 1;
					deletedExtent := false;
					endFreeArea;
				end
				else
				if SlotDeleted in dirslot.flags then
				begin
					deletedCount := deletedCount + 1;
					deletedExtent := true;
					clearDirSlot;
					(* we consider a deleted file
						as a free area here *)
					if not dryrun then
						beginFreeArea;
				end
				else
				if SlotExtent in dirslot.flags then
				begin
					if deletedExtent then
						clearDirSlot
					else
						lastUsed := i;
				end
				else
				if SlotReserved in dirslot.flags then
					reservedCount := reservedCount + 1
				else
				if SlotFree in dirslot.flags then
					beginFreeArea;
			end;
			if i = endSlot then
				done := true;
			i := i + 1;
		until done;

		markLastSlot;
		closevolumeid(volid);
		i := i - 1;

		if verbose then
		begin
			writeln('last used slot: ', lastUsed);
			writeln('max slots:      ', endSlot + 1);
			writeln('free slots:     ', endSlot - i + freeCount);
			writeln('reserved slots: ', reservedCount);
			writeln;
		end;

		write(fileCount, ' files, ', deletedCount, ' deleted files, ');
		write(reclaimCount);
		if dryrun then
			writeln(' reclaimable slots, ', freeAreaCount, ' free regions.')
		else
			writeln(' reclaimed slots, ', freeAreaCount, ' free regions.');
	end;

	freeAreas := freeAreaCount;
end;

function scanDirSlots(volid:integer;startSlot:integer;var dirslot:DirectorySlot;wanted:DirSlotFlags):integer;
var done:boolean;
    error:integer;
    curSlot,lastSlot:integer;
begin
	scanDirSlots := 0;
        curSlot := startSlot;
	lastSlot := volumeTable[volid].part.dirSize - 1;
	done := false;

	repeat
		(* writeln('** getdirslot ', curSlot); *)
		getdirslot(volid, curSlot, dirslot, error);
		if wanted <= dirslot.flags then
		begin
			scanDirSlots := curSlot;
			(* writeln('   found'); *)
			done := true;
		end
		else
		begin
			curSlot := curSlot + 1;
			if curSlot > lastSlot then
				done := true;
		end;
	until done;
end;

procedure crunchVolume(volname:string);
var volid:integer;
    extentSize:integer;
    extentBlocks:integer;
    fileExtents,fileBlocks:integer;
    endSlot:integer;
    i:integer;
    error:integer;
    done:boolean;
    freeStart, occStart:integer;
    freeSlot:DirectorySlot;
    occSlot:DirectorySlot;
    freeSlotNo,occSlotNo:integer;
    copySlotIdx:integer;
    clearStart:integer;
    clearSlotNo:integer;
    dummy:integer;
    copyDirSlot:DirectorySlot;

procedure getFileBlocks(slotno:integer;dirslot:DirectorySlot;var startBlock,blockCount:integer);
begin
	startBlock := slotno * extentBlocks;
	blockCount := (511 + dirslot.sizeBytes) div 512;
	if blockCount = 0 then
		blockCount := 1; (* can happen if filesize is 0 *)
end;

procedure copyBlocks(srcBlock,destBlock:integer;blockCount:integer);
const blocksPerBuf = 8;
var bufptr:^IOBuffer;
    curBlocks:integer;
    error:integer;
begin
	new(bufptr);

	while blockCount > 0 do
	begin
		if blockCount > blocksPerBuf then
			curBlocks := blocksPerBuf
		else
			curBlocks := blockCount;

		readvolumeblks(volid, bufptr, srcBlock, curBlocks, error);
		if error <> IONoError then
		begin
			writeln('Error reading block ', srcBlock, ' on volume ', volname);
			blockCount := -9999;
		end;

		writevolumeblks(volid, bufptr, destBlock, curBlocks, error);
		if error <> IONoError then
		begin
			writeln('Error writing block ', srcBlock, ' on volume ', volname);
			blockCount := -9999;
		end;

		srcBlock := srcBlock + curBlocks;
		destBlock := destBlock + curBlocks;
		blockCount := blockCount - curBlocks;
	end;

	dispose(bufptr);
end;

begin
	volid := findvolume(volname);
	if volid < 1 then
		writeln('Volume ', volname, ' not found.')
	else
	begin
		openvolumeid(volid);

		endSlot := volumeTable[volid].part.dirSize - 1;
		extentSize := volumeTable[volid].part.extentSize;
		extentBlocks := extentSize div 512;

		(* start at first dirslot *)
		i := volumeTable[volid].startSlot;
		done := false;

		while not done do
		begin
			(* find a free slot *)
			freeSlotNo := scanDirSlots(volid, i, freeSlot, [SlotFree]);
			if freeSlotNo <> 0 then
			begin
				(* writeln('found free slot ', freeSlotNo); *)

				(* find next occupied slot *)
				occSlotNo := scanDirSlots(volid, freeSlotNo + 1, occSlot, [SlotFirst]);
				if occSlotNo <> 0 then
				begin
					fileExtents := (occSlot.sizeBytes + extentSize - 1) div extentSize;
					if fileExtents = 0 then fileExtents := 1;

					(* writeln('found occupied slot ', occSlotNo); *)

					(* crunch free space *)

					(* determine volume block of free region start *)
					getFileBlocks(freeSlotNo, freeSlot, freeStart, dummy);
					(* determine volume block of file and size in blocks *)
					getFileBlocks(occSlotNo, occSlot, occStart, fileBlocks);
					writeln('moving ', occSlot.name,
						' with ', fileExtents,
						' extents from ', occSlotNo, ' to ', freeSlotNo);
					(* writeln('  block: ', occStart, ' to ', freeStart,
						' blocks: ', fileBlocks, ' extents: ', fileExtents); *)

					(* copy blocks from file to free region, starting at first *)
					copyBlocks(occStart, freeStart, fileBlocks);
					(* copy occupied dirslots (first and extent) to free dirslot *)
					for copySlotIdx := 0 to fileExtents - 1 do
					begin
						getdirslot(volid, occSlotNo + copySlotIdx, copyDirSlot, error);
						if error <> IONoError then
						begin
							writeln('Error reading dirslot ', occSlotNo + copySlotIdx);
							break;
						end;
						putdirslot(volid, freeSlotNo + copySlotIdx, copyDirSlot, error);
						if error <> IONoError then
						begin
							writeln('Error writing dirslot ', freeSlotNo + copySlotIdx);
							break;
						end;
					end;
					(* mark dirslots of moved file as free *)
					clearStart := occSlotNo;
					(* check for overlap of new and old file region *)
					if freeSlotNo + fileExtents > occSlotNo then
						clearStart := freeSlotNo + fileExtents;
					for clearSlotNo := clearStart to occSlotNo + fileExtents - 1 do
					begin
						(* writeln('clearing dirslot ', clearSlotNo); *)
						putdirslot(volid, clearSlotNo, freeSlot, error);
						if error <> IONoError then
						begin
							writeln('Error writing dirslot ', clearSlotNo);
							break;
						end;
					end;

					i := i + fileExtents;
				end
				else	(* no occupied slot found *)
					done := true;
					(* TODO: mark first free slot of last free region as EndScan *)
			end
			else	(* no free slot found *)
				done := true;
		end;
		closevolumeid(volid);
	end;
end;

begin
	if ParamCount > 0 then
		volname := ParamStr(1)
	else
	begin
		write('Volume name> ');
		readln(volname);
	end;
		
	initDevices;
	scanVolume(volname, true, true, count, areas);

	if count > 0 then
	begin
		write('Proceed with reclaim (y/n)? ');
		read(ch);
		writeln;
		if upcase(ch) = 'Y' then
			scanVolume(volname, false, false, count, areas);
	end;

	if areas > 1 then
	begin
		write('Free space is fragmented, crunch (y/n)? ');
		read(ch);
		writeln;
		if upcase(ch) = 'Y' then
			crunchVolume(volname);
	end;
end.
