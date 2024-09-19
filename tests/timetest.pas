program timetest;
var time:DateTime;
begin
	while true do
	begin
		writeln('ticks: ', GetTicks);
		time := GetTime;
		writeln('h:', time.hours, ' m:', time.minutes, ' s:', time.seconds);
		writeln(DateStr(time), ' ', TimeStr(time,true));
		readln;
	end;
end.
