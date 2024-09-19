(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
function encodefloat32(r:real):integer;
var	intpart:real;
	fract: real;
	exponent:integer;
	sign:integer;
	i:integer;
	digit, bitpos:integer;
	intlength,fractlength:integer;
	intbin:integer;
	fractbin:integer;
	floatbin:integer;
begin
	intbin := 0; fractbin := 0; floatbin := 0;

	if r<0 then
	begin
		r := abs(r);
		sign := 1;
	end
	else
		sign := 0;

	if r = 0.0 then
	begin
		intpart := 0.0;
		fract := 0.0;
		intlength := 0;
		fractlength := 0;
		intbin := 0;
		fractbin := 0;
		floatbin := 0;
	end
	else
	begin
		intpart := r;
		fract := frac(r);
		exponent := floor(log2(intpart));

		intlength := exponent+1;
		fractlength :=  wordbits - intlength - Float32ExpBits - 1;
	end;
		(* FIXME: log2 gives division by zero on zero arg *)


	(* process bits before the point *)
	for i := 1 to intlength do
	begin
		(* digit := round(intpart mod 2.0); *)
		(* calculate real remainder in a portable way *)
		digit := floor(intpart - 2 * Int(intpart / 2));

		(* if we used up all the bits in the fraction part of
			the float32 encoding, shift everything right
			and put bit at the top *)
		if i > Float32FractBits then
		begin
			bitpos := Float32FractBits-1;
			intbin := intbin shr 1;
		end
		else
			bitpos := i - 1;

		if digit > 0 then intbin := intbin  + (1 << bitpos);

		intpart := intpart / 2.0;
	end;

    (* limit the integer bits *)
	if intlength > Float32FractBits then intlength := Float32FractBits;

	(* process bits after the point, if we have any bits left *)
	if fractlength > 0 then
	begin	
		for i := 1 to fractlength do
		begin
			fract := fract * 2;
			digit := trunc(fract) and 1;
			fractbin := (fractbin shl 1) + digit;
		end;

	end;

	floatbin := (intbin << (Float32FractBits - intlength)) + fractbin;

	if floatbin = 0 then (* if mantissa is zero, return a clean zero value *)
		encodefloat32 := 0
	else
	begin
		exponent := exponent + Float32ExpBias;
		if (exponent > Float32ExpMax) or (exponent < 0) then
			errorExit2('float exponent overflow','');
		encodefloat32 := (sign shl (wordBits-1)) + (floatbin << Float32ExpBits) + exponent;
	end;
end;
