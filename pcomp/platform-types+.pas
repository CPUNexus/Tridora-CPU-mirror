(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
type
	OutputFileType = text;
	InputFileType = file of char;
	SymFileType = text;

	ExecAction = (Edit, Assemble, Run);
