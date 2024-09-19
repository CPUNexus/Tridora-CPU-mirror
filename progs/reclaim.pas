(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
program reclaim;
var volname:string;
    ch:char;
    count:integer;

(* we use some stuff internal to stdlib.pas *)
procedure getdirslot(volumeid:integer;slotNo:integer;var result:DirectorySlot;var error:integer);
	external;
procedure putdirslot(volumeid:integer;slotNo:integer;var dirslot:DirectorySlot;var error:integer);
	external;

procedure scanVolume(volname:string;dryrun:boolean;verbose:boolean;var reclaimCount:integer);
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
	scanVolume(volname, true, true, count);

	if count > 0 then
	begin
		write('Proceed with reclaim (y/n)? ');
		read(ch);
		writeln;
		if upcase(ch) = 'Y' then
			scanVolume(volname, false, false, count);
	end;
end.
