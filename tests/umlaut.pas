(*
   test program for
   multibyte characters
   and tabs
			*)
program umlaut;
var	s:string = 'ÄÖÜß';
begin
	writeln('Falsches Üben von');
	writeln('Xylophonmusik quält jeden');
	writeln('größeren Zwerg.');
	writeln;
	writeln(s);
	writeln(length(s));
end.
