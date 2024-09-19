program test109;

const screenwidth = 640;
      screenheight = 400;
      screenmidx = 319;
      screenmidy = 199;

      xrange = 14.0;
      yrange = 3.0;

      xmin = -7.0;
      xmax =  7.0;
      ymin = -1.5;
      ymax =  1.5;

      xstep = 0.005;


var scalex,scaley: real;
    value:real;
    curx:real;

    testcounter:integer;

function screenx(x:real):integer;
begin
	screenx := trunc((x + xmax) * scalex);
	{ writeln(x, ' -x-> ', screenx);}
end;

function screeny(y:real):integer;
begin
	screeny := trunc((ymax - y) * scaley);
	{ writeln(y, ' -y-> ', screeny); }
end;

procedure drawCoords;
begin
	drawline(screenx(xmin), screeny(0), screenx(xmax), screeny(0), 8);
	drawline(screenx(0), screeny(ymin), screenx(0), screeny(ymax), 8);
end;

procedure plot(x,y:real;nr:integer);
begin
	if (x>=xmin) and (x<=xmax)
		and (y>=ymin) and (y<=ymax) then
		putpixel( screenx(x), screeny(y), 3 + nr);
end;

procedure test(x:real; delta:real);
begin
	writeln('-----------test-----------------');
end;

function squareroot(x:real):real;
begin
	if x = 0.0 then
		squareroot := 0.0
	else
		squareroot := sqrt(x);
end;

function logn(x:real):real;
begin
	if x <= 0.0 then
		logn := 0.0
	else
		logn := ln(x);
end;

function dafunc(x:real;nr:integer):real;
begin
	{
	testcounter := testcounter + 1;
	if testcounter = 20 then
		test(x, xstep); }
	{ writeln('dafunc ', testcounter, ' x:', x, ' + 0.1:', x + 0.1); }
	case nr of
	0: dafunc := sin(x);
	1: dafunc := cos(x);
	2: dafunc := arctan(x);
	3: dafunc := tan(x);
	4: dafunc := cotan(x);
	5: dafunc := logn(x);
	end;
end;

procedure graph(nr:integer);
begin
	curx := xmin;
	{ curx := 0.0; }
	while curx < xmax do
	begin
		value := dafunc(curx, nr);
		plot(curx, value, nr);
		curx := curx + xstep;
	end;
end;

begin
	initgraphics;
	scalex := screenwidth / xrange;
	scaley := screenheight / yrange;
	drawCoords;
	graph(0);
	graph(1);
	graph(2);
	graph(3);
	graph(4);
	graph(5);
end.
