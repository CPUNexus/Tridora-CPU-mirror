(* a simple test program to say
   hello to the world *)

program hello;
begin
	(* if there is an argument, use it *)
	if ParamCount > 0 then
		writeln('Hello ', ParamStr(1))
	else
		writeln('Hello World!');
end.
{ Note that the last END needs to be followed by the . character,
  not by a ; character. This is because ; means that there is
  another statement. It does not mark the end of the statement
  like in other languages. The . marks the end of the program text. }
