program settest;
type myset = set of (alpha, beta, gamma, delta);
var a,b:myset;
begin
  a := [alpha, gamma];
  b := [alpha, gamma, delta];
  writeln (a <= b);
  writeln (a >= b);
  writeln (b >= a);
  writeln ((b - [alpha, gamma]) >= a);
end.
