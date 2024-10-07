# The Mostly Missing Tridora Pascal Programming Guide
## Strings
Strings are dynamically sized arrays of bytes. They are declared with a maximum size and carry runtime information about the current size and the maximum size.
Passing strings of different maximum sizes as parameters is possible and safe because of the maximum size information. The size field uses the **integer** type, so a string theoretically can be 2GB in size.

When allocating a pointer to a string with **new**, you can specify a maximum size that is being allocated, so you can allocate less memory than the string type specifies. The string header fields will reflect the allocated size.

Example:
```
type strPtrType: ^string[1024];

var p: ^strPtrType;

	new(strPtr, 80);
```
When using **new** without the additional parameter, this would always allocate memory for 1024 bytes. If you at some point know that a string will be smaller, you can save memory by allocating only the amount needed.

Note that the **char** type is a 32-bit type, and strings contain bytes. Non-ASCII characters in strings are expected to be UTF-8-encoded. Although a **char** variable could in theory hold a 32-bit Unicode character, this is not supported by the standard library.

When indexing a string, the result is a **char** type with bits 31 to 8 set to zero. When assigning a **char** to an indexed string element, the destination byte is set to bits 7 to 0 from the **char** variable. Bits 31 to 8 are ignored.

## For-in Loop
The for-in-loop which allows iterating over a string or a linear array of scalar variables is supported. It is more efficient than using a for loop for indexing a string or an array, because no bounds checks are needed.

## Sets
The maximum number of elements in a set is 32. This makes a SET OF CHAR impossible.

When using a SET OF CHAR in other Pascal dialects, it is most often used with a set literal like this:
```
var c:char;

	read(c);
	if c in [ 'y', 'n' ] then
		....
```

In Tridora Pascal, this syntax also works because `[ 'y', 'n' ]` will not be treated as a set literal, but as an array literal.
The _in_ operator also works for linear arrays, so the _if_ statement will have the same result.

Note that the array _in_ operator will be more inefficient for larger ranges (i.e. `'A'..'z'`), but more efficient for sparse sets (i.e. `'A','z'`).

## I/O
I/O handling in Tridora Pascal is mostly compatible with other Pascal dialects when reading/writing simple variables from/to the console. There are big differences when opening/reading/writing files explicitly.

### Tridora's Solution
The original Wirth Pascal has very limited file I/O capabilities. Therefore, most Pascal dialects have their own, incompatible extensions for handling file I/O.

In this tradition, Tridora Pascal obviously needs to have its own incompatible file handling.

The goal is to mostly ignore the stuff from other Pascal dialects and instead use something more intuitive and more recognizable from other programming languages:
- files are sequences of bytes
- files are used with _open_, _read_/_write_, _close_
- files can be opened in different modes


The implementation also has the following properties:
- file variables have the type _file_
- the _open_ procedure has the file variable, the file name and the mode as parameters
- _read_/_write_ do ASCII conversion on scalar variables, records and array are processed as binary
- enums and booleans are treated as integers
- _readln_/_writeln_ operate as expected
- other file operations available are _eof_, _eoln_ and _seek_
- for error handling there is a function _IOResult_
- terminating the program without calling _close_ on open files will lose data

Differences from other Pascal dialects are:
- there are no _FILE OF_ types
- no _get_, _put_, _reset_, _rewrite_, _assign_
 
### Opening Files
There are five modes for opening files which (hopefully) cover all common use cases:
- _ModeReadonly_: file is opened only for reading, file must exist
- _ModeCreate_: file is opened for reading and writing, file must not exist
- _ModeModify_: file is opened for reading and writing, file must exist
- _ModeOverwrite_: file is opened for reading and writing, file will be truncated
- _ModeAppend_: file is opened for writing, data is always written to the end of the file

Example:
```
var f:file;
    c:char;
    a,b:integer;

	open(f, 'myfile.text', ModeReadonly);
	read(a,b,c);
	close(f);
```

### Error Handling
When an I/O error occurs, the _IOResult_ function can be called to get the error code. Unlike TP, the _IOResult_ function requires a
file variable as a parameter. When you call _IOResult_, an error that may have occurred is considered to be _acknowledged_. If an
error is not ackowledged and you do another I/O operation, a runtime error is thrown.

That means you can either write programs without checking for I/O errors, while resting assured that the program will exit if an I/O error occurs. You can also choose to check for errors with _IOResult_ if you want to avoid having runtime errors.

The function _ErrorStr_ from the standard library takes an error code as an argument and returns the corresponding textual description as a string.

Example:
```
procedure tryToReadFile;
var f:file;
begin
	open(f, 'myfile.text', ModeReadonly);
	if IOResult(f) <> IONoError then
	begin
		writeln('Could not open file: ', ErrorStr(IOResult(f)));
		exit;
	end;

	...

	close(f);
end;
```
