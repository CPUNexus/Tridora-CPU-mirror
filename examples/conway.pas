program conway;
const cellwidth = 4;
      cellheight = 4;
      cols = 40;
      rows = 25;

      WHITE = 1;
      BLACK = 0;

type gridType = array [1..rows, 1..cols] of integer;

var grid:gridType;
    ch:char;

procedure initGrid(var g:gridType);
var x,y:integer;
begin
	randomize;
	for y := 1 to rows do
		for x := 1 to cols do
			if (random and 1024) > 512 then
				grid[y,x] := 1;
end;

procedure updateGrid;
var oldGrid:gridType;
    neighbors:integer;
    x,y:integer;
    wasAlive:boolean;
    isAlive:boolean;
    gen:integer;
begin
	oldGrid := grid;
	for y := 1 to rows do
		for x := 1 to cols do
		begin
			wasAlive := oldGrid[y,x] > 0;
			isAlive := false;

			neighbors := 0;

			if y > 1 then
			begin
				if x > 1 then
					if oldGrid[y-1,x-1] > 0 then neighbors := neighbors + 1;

				if oldGrid[y-1,x] > 0 then neighbors := neighbors + 1;

				if x < cols then
					if oldGrid[y-1,x+1] > 0 then neighbors := neighbors + 1;
			end;

			if x > 1 then
				if oldGrid[y,x-1] > 0 then neighbors := neighbors + 1;
			if x < cols then
				if oldGrid[y,x+1] > 0 then neighbors := neighbors + 1;

			if y < rows then
			begin
				if x > 1 then
					if oldGrid[y+1,x-1] > 0 then neighbors := neighbors + 1;

				if oldGrid[y+1,x] > 0 then neighbors := neighbors + 1;

				if x < cols then
					if oldGrid[y+1,x+1] > 0 then neighbors := neighbors + 1;
			end;

			if wasAlive then
			begin
				if (neighbors = 2) or (neighbors = 3) then
					isAlive := true;
			end
			else
			if neighbors = 3 then
				isAlive := true;
			if isAlive then
			begin
				gen := grid[y,x];
				if gen < 8 then gen := gen + 1;
				grid[y,x] := gen;
			end
			else
				grid[y,x] := 0;
		end;
end;

procedure drawGrid;
var x,y:integer;
    color:integer;
    screenx,screeny:integer;
begin
	for x := 1 to cols do
		for y := 1 to rows do
		begin
			color := grid[y,x];

			screenx := x * cellwidth;
			screeny := y * cellheight;
			putpixel(screenx,screeny,color);
			putpixel(screenx+1,screeny,color);
			putpixel(screenx,screeny+1,color);
			putpixel(screenx+1,screeny+1,color);
		end;
end;

begin
	initGraphics;
	initGrid(grid);
	repeat
		drawGrid;
		updateGrid;
		{ delay(100); }
	until conavail;
	read(ch);
end.
