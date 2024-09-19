(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
program dumpdir;
var volname:string;
    volid:integer;

(* we use some stuff internal to stdlib.pas *)
procedure getdirslot(volumeid:integer;slotNo:integer;var result:DirectorySlot;var error:integer);
	external;

procedure dumpdir(volid:integer);
var dirs:DirectorySlot;
    i:integer;
    lastSlot:integer;
    error:integer;
begin
	lastSlot := volumeTable[volid].part.dirSize - 1;
	openvolumeid(volid);

	for i := 0 to lastSlot do
	begin
		getdirslot(volid, i, dirs, error);
		with dirs do
		begin
			write('slot ', i, '   ', name, '   ', sizeBytes, '  G', generation);
			if SlotFirst in flags then write(' First');
			if SlotExtent in flags then write(' Extent');
			if SlotReserved in flags then write(' Resvd');
			if SlotDeleted in flags then write(' Del');
			if SlotFree in flags then write(' Free');
			if SlotEndScan in flags then write(' End');
			writeln;
			if SlotEndScan in flags then break;
		end;
	end;

	closevolumeid(volid);
end;

begin
	if ParamCount > 0 then
		volname := ParamStr(1)
	else
	begin
		write('Volume name> ');
		readln(volname);
	end;
	volid := findvolume(volname);
	if volid < 1 then
		writeln('Volume not found.')
	else
		dumpdir(volid);
end.
