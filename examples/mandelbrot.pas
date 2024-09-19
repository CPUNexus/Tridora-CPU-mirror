program mandelbrot;

const width = 459; height = 405;
      xstart = -2.02; xend = 0.7;
      ystart = -1.2; yend = 1.2;
      maxIterations = 25;
      maxColors = 15;

var dx,dy:real;
    col,row:integer;
    cx,cy:real;
    iterations:integer;
    colors:array[0..15] of integer = { ($000, $020, $031, $042,
    					$053, $064, $075, $086,
					$097, $0A8, $0B9, $0CA,
					$0DB, $0EC, $0FD, $0FF); }

					($000, $100, $200, $411,
					 $522, $633, $744, $855,
					 $966, $A77, $B88, $C99,
					 $DAA, $EBB, $FCC, $FDD);
    c:integer;
   
function iterate(x,y:real):integer;
var zx,zy:real;
    tmp:real;
    count:integer;
begin
	zx := 0.0; zy := 0.0; count := 0;

	repeat
		tmp := zx*zx - zy*zy + x;
		zy := 2.0*zx*zy + cy;
		zx := tmp;
		count := count + 1;
	until (zx*zx + zy*zy > 4.0) or (count = MaxIterations);

	iterate := count;
end;

begin
	initgraphics;
	for c:=0 to 15 do
		setpalette(c, colors[c]);

	dx := (xend - xstart) / (width - 1);
	dy := (yend - ystart) / (height - 1);

	for col := 0 to width - 1 do
	begin
		cx := xstart + col * dx;
		for row := 0 to height - 1 do
		begin
			cy := yend - row * dy;
			iterations := iterate(cx, cy);
			if iterations = MaxIterations then
				c := 0
			else
				c := iterations mod MaxColors + 1;
			putpixel(col, row, c);
		end;
	end;
end.
