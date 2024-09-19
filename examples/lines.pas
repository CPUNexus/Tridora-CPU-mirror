PROGRAM lines;

PROCEDURE movinglines(max_x, max_y, max_col, ms:INTEGER);
VAR x1,y1:INTEGER;
VAR x2,y2:INTEGER;
VAR delta_x1, delta_y1:INTEGER;
VAR delta_x2, delta_y2:INTEGER;
VAR col:INTEGER;
BEGIN

	x1 := 120;
	y1 := 90;

	x2 := 340;
	y2 := 220;

	delta_x1 := 9;
	delta_y1 := 4;

	delta_x2 := 3;
	delta_y2 := 7;

	col := 1;

	WHILE NOT CONAVAIL DO
	BEGIN
		x1 := x1 + delta_x1;
		y1 := y1 + delta_y1;

		x2 := x2 + delta_x2;
		y2 := y2 + delta_y2;

		IF (x1 > max_x) OR (x1 < 0) THEN
		BEGIN
			delta_x1 := -delta_x1;
			x1 := x1 + delta_x1;
		END;

		IF (y1 > max_y) OR (y1 < 0) THEN
		BEGIN
			delta_y1 := -delta_y1;
			y1 := y1 + delta_y1;
		END;

		IF (x2 > max_x) OR (x2 < 0) THEN
		BEGIN
			delta_x2 := -delta_x2;
			x2 := x2 + delta_x2;
		END;

		IF (y2 > max_y) OR (y2 < 0) THEN
		BEGIN
			delta_y2 := -delta_y2;
			y2 := y2 + delta_y2;
		END;

		col := col + 1;

		IF col > max_col THEN col := 1;

		DRAWLINE(x1,y1,x2,y2,col);
		
		delay(ms);
	END;
END;

BEGIN
	initgraphics;
	movinglines(639,399,15,0);
END.
