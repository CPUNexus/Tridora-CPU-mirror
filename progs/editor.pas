(* Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details *)
{$H400}
program editor;

const COMPILERPROG = '#SYSTEM:pcomp.prog';
      ASMPROG = '#SYSTEM:sasm.prog';

const MAX_LENGTH = 512;
      MAX_LINES = 10000;
      MAX_SCREENH = 256;
      MAX_KEYWORD = 31;

      MAX_CLIPB_SIZE = 300;

const ARROW_LEFT  = 100000;
      ARROW_UP    = 100001;
      ARROW_RIGHT = 100002;
      ARROW_DOWN  = 100003;
      HOME_KEY    = 100004;
      END_kEY     = 100005;
      PG_UP       = 100006;
      PG_DOWN     = 100007;
      DELETE_KEY  = 100008;
      INSERT_KEY  = 100009;
      HOME_KEY_M  = 100010;
      END_KEY_M   = 100011;
      HELP_KEY    = 100012;

const TOPSTAT_BG = 130;
      TOPSTAT_FG = 7;
      BOTSTAT_BG = 22;
      BOTSTAT_FG = 15;
      BOTSTAT_BG_W = 124;
      BOTSTAT_FG_W = 15;
      BOTSTAT_BG_P = 28;
      BOTSTAT_FG_P = 15;
      BOTSTAT_BG_I = 15;
      BOTSTAT_FG_I = 0;
      TEXT_FG = 246;
      TEXT_BG = 0;
      NUM_FG  = 219;
      IDENT_FG = 145;
      KEYWORD_FG =29;
      PUNCT_FG = 111;
      COMMENT_FG = 66;
      STRLIT_FG = 130;
      NONTEXT_FG = 57;

const vMargins = 2;
      topMargin = 1;
      botMargin = 1;
      hScrollDelta = 8;

type linestr = string[MAX_LENGTH];
     lineref = ^linestr;

type HiliteCat = (Unknown, WhiteSpc, Keyword, Ident, Number, Punct, Comment, StrLit);

var lines: array [1..MAX_LINES] of ^linestr;
    lineFlags: array [1..MAX_SCREENH] of boolean;
    linecount:integer;
    screenX:integer;
    screenW, screenH:integer;
    pageSize:integer;
    curX, curY:integer;
    topY:integer;
    colOffs:integer;
    editBuf:^linestr;
    editLine:integer;
    emptyLine:lineref;

    i:integer;
    filename:string;
    isNewFile:boolean;
    isModified:boolean;
    infile:file;
    linebuf:linestr;

    con:file;

    curColor:integer;

    botStatMsg:string;
    botStatFgColor:integer;
    botStatBgColor:integer;

    autoindent: boolean;
    highlight: boolean;

    clipboard: array [1..MAX_CLIPB_SIZE] of lineref;
    clipboardSz: integer;
    keepClips: boolean;

    catColors: array [Unknown..StrLit] of integer =
	( TEXT_FG, 0, KEYWORD_FG, IDENT_FG, NUM_FG, PUNCT_FG, COMMENT_FG, STRLIT_FG );

    keywords: array [0..MAX_KEYWORD] of string[20] = (
			'VAR', 'IF', 'THEN', 'ELSE', 'BEGIN', 'END', 'PROCEDURE', 'FUNCTION',
			'WHILE', 'FOR', 'DO', 'IN', 'OF', 'CASE', 'TO', 'REPEAT', 'UNTIL',
			'CHAR', 'INTEGER', 'REAL', 'BOOLEAN', 'ARRAY', 'RECORD', 'STRING',
			'MOD', 'DIV', 'AND', 'OR', 'NOT',
			'TYPE', 'CONST',
			'PROGRAM'
		);
    paramPos:integer;
    errorLine:integer;
    errorMsg:string;
    errLineStr:string[12];

    PArgs:array [0..PArgMax] of string external;
    PArgCount:integer external;

procedure debugOut(s:string;i1,i2:integer); forward;

procedure showCursor(doShow:boolean);
begin
	write(#27,'[?25');
	if doShow then
		write('h')
	else
		write('l');
end;


procedure moveCursor;
begin
	GotoXY(screenX - colOffs, curY - topY + topMargin + 1);
end;

procedure initLineFlags;
var i:integer;
begin
	for i := 1 to MAX_SCREENH do
		lineFlags[i] := false;
end;

procedure getScreenSize;
var c:char;
begin
	(* empty keyboard buffer *)
	while conavail do read(con, c);

	GetTermSize(screenW, screenH);
	pageSize := screenH - vMargins;
	(* set scrolling region - DECSTBM *)
	write(#27,'[2;', 1 + pageSize, 'r');
end;

procedure checkScreenSize;
begin
	if (screenW < 80) or (screenH <22) then
	begin
		writeln('Need a minimum screen size of 80x22.');
		halt;
	end;
end;

procedure initScreen;
begin
	editBuf := nil;
	editLine := -1;
	new(emptyLine,12);
	emptyLine^ := '';

	initLineFlags;
	TextDefault;
	ClrScr;
	getScreenSize;
	checkScreenSize;
	colOffs := 0;
	screenX := colOffs + 1;
	curX := colOffs + 1;
	curY := 1;
	topY := 1;
	moveCursor;
	botStatFgColor := BOTSTAT_FG;
	botStatBgColor := BOTSTAT_BG;
	botStatMsg := '';
end;

procedure resetScreen;
begin
	TextDefault;
	write(#27,'[r'); (* reset scrolling region *)
	ClrScr;
end;

procedure saveCursor;
begin
	write(#27,'7'); (* save cursor position *);
end;

procedure restoreCursor;
begin
	write(#27,'8'); (* restore cursor position *);
end;


procedure showTopStatus;
var c:char;
begin
	if isModified then
		c := '*'
	else
		c := ' ';

	GotoXY(1,1);

	TextBackground(TOPSTAT_BG);
	TextColor(TOPSTAT_FG);

	write('File ', filename, c, '  row:', curY, '/', linecount);

	write('   Indent ');
	if autoindent then write('ON') else write('OFF');

	write('   ');
	if keepClips then write('+') else write(' ');
	write('Clipb.: ');
	if clipboardSz = 0 then write('empty')
	else write(clipboardSz, ' lines');

	ClrEol;

	TextDefault;
end;

procedure showBotStatus;
begin
	GotoXY(1, screenH);
	TextColor(botStatFgColor);
	TextBackground(botStatBgColor);
	write('   ', botStatMsg);
	ClrEol;
	TextDefault;
end;

procedure updateStatus;
begin
	showCursor(false);
	saveCursor;
	showTopStatus;
	restoreCursor;
	showCursor(true);
end;

function getKey:integer; forward;

procedure statusMsg(msg:string;warn:boolean;confirm:boolean);
var key:integer;
    hinted:boolean;
begin
	botStatMsg := msg;
	if confirm then
		botStatMsg := botStatMsg + ' | [RETURN]';

	if warn then
	begin
		botStatFgColor := BOTSTAT_FG_W;
		botStatBgColor := BOTSTAT_BG_W;
	end
	else
	begin
		botStatFgColor := BOTSTAT_FG;
		botStatBgColor := BOTSTAT_BG;
	end;

	if length(botStatMsg) + 6 > screenW then
		setLength(botStatMsg, screenW - 6);

	showCursor(false);
	saveCursor;
	showBotStatus;
	restoreCursor;
	showCursor(true);
	if confirm then
	begin
		hinted := false;
		repeat
			key := getKey;
			if key <> 13 then
			begin
				write(con, #7);
				if not hinted then
				begin
					hinted := true;
					botStatMsg := botStatMsg + '<==';
					saveCursor;
					showBotStatus;
					restoreCursor;
				end;
			end;
		until key = 13;
		statusMsg('', false, false);
	end;

end;

procedure clearStatus;
begin
	statusMsg('', false, false);
end;

procedure debugOut(s:string;i1,i2:integer);
begin
	write(#27,'7'); (* save cursor position *);
	GotoXY(40,1);
	TextDefault;
	write(s);
	if i1 >= 0 then write(' ',i1);
	if i2 >= 0 then write(' ',i2);
        write('.');
	TextColor(curColor);
	write(#27,'8'); (* restore cursor position *);
end;

procedure changeColor(col:integer);
begin
	if col <> curColor then
	begin
		TextColor(col);
		curColor := col;
	end;
end;

(* check if a byte is part of a multibyte character,
  either leading or continuation byte *)
function isMBChar(b:char):boolean;
begin
	(* check if 8th bit is set *)
	isMBChar := (ord(b) and $80) <> 0;
end;

(* check if a byte is a leading byte of a multibyte
	character sequence *)
function isMBLead(b:char):boolean;
begin
	(* check if upper two bits are 11  *)
	isMBLead := (ord(b) and $C0) = $C0;
end;

(* check if a byte is a continuation byte of a multibyte
	character sequence *)
function isMBCont(b:char):boolean;
begin
	(* check if upper two bits are 10  *)
	isMBCont := (ord(b) and $C0) = $80;
end;

(* determine the length of a multibyte sequence
   from the leading byte *)
function getMBLength(leadingByte:char):integer;
begin
	if (ord(leadingByte) and $80) = 0 then
		getMBLength := 1
	else
	if (ord(leadingByte) and $E0) = $C0 then
		getMBLength := 2
	else
	if (ord(leadingByte) and $F0) = $E0 then
		getMBLength := 3
	else
	if (ord(leadingByte) and $F8) = $F0 then
		getMBLength := 4
	else (* invalid encoding *)
		getMBLength := 1;
end;

(* ask for input in bottom status line, optionally only digits *)
procedure prompt(msg:string;var strReturn:string;numMode:boolean);
var buf:string;
    maxLen:integer;
    i,l:integer;
    key:integer;
    c:char;
    done:boolean;
    isMB:boolean;

procedure doBackspace;
begin
	if l > 0 then
	begin
		repeat
			isMB := isMBChar(strReturn[l]);
			l := l - 1;
		until  (l < 1 ) or  not isMB;
		setLength(strReturn, l);
		write(con, #8, ' ', #8);
	end;
end;

function isValidChar(ch:char):boolean;
begin
	if numMode then
		isValidChar := isDigit(ch)
	else
		isValidChar := (ord(ch) >= 32)
end;

begin
	maxLen := maxlength(strReturn);
	strReturn := '';

	showCursor(false);

	(* draw line in prompt background color *)
	GotoXY(1,ScreenH);
	TextColor(BOTSTAT_FG_P);
	TextBackground(BOTSTAT_BG_P);
	ClrEol;
	write('  ', msg, ' ');

	(* draw the input field *)
	TextColor(BOTSTAT_FG_I);
	TextBackground(BOTSTAT_BG_I);
	for i := 1 to maxLen do
		write(' ');

	(* place cursor at start of input field *)
	TextBackground(BOTSTAT_FG_P);
	GotoXY(length(msg) + 4, ScreenH);

	showCursor(true);

	done := false;
	repeat
		key := getKey;
		if key < 255 then  (* ignore cursor keys etc *)
		begin
			c := chr(key);
			l := length(strReturn);

			if (c = #8) or (c = #127) then
				doBackspace
			else
			if isValidChar(c) then
			begin
				if l < maxLen then
				begin
					appendchar(strReturn, c);
					write(con, c);
				end;
			end;

			if c = #13 then
				done := true;
		end;
	until done;

	TextDefault;
	clearStatus;
	moveCursor;
end;

function getLineRef(l:integer):lineref;
begin
	if l = editLine then
		if editBuf <> nil then
			getLineRef := editBuf
		else
			getLineRef := lines[l]
	else	(* duplicate else branch for a litte more speed *)
		getLineRef := lines[l];

	if getLineRef = nil then
		getLineRef := emptyLine;
end;

function getLineLength(l:integer):integer;
begin
	getLineLength := length(getLineRef(l)^);
end;

(*
   Determine the number of bytes a character on the screen has.
   ASCII characters are one byte long, others are multibyte characters
   which can have different lengths.
   lineno is the line number in the lines array,
   strind is the string index for that line.
   If strind points at a continuation byte of a mb sequence,
   the result is the remaining number of bytes of the mb sequence.
*)
function charBytes(lineno:integer;strInd:integer):integer;
var lineptr:^linestr;
    inMBSeq:boolean;
begin
	lineptr := getLineRef(lineno);
	charBytes := 1;
	if isMBChar(lineptr^[strind]) then
	begin
		repeat
			strind := strind + 1;
			if strind <= length(lineptr^) then
			begin
				inMBSeq := isMBCont(lineptr^[strind]);
				if inMBSeq then
					charBytes := charBytes + 1;
			end
			else (* always terminate loop if we are at end of string *)
				inMBSeq := false;
		until not inMBSeq;
	end;
end;

(* get the number of extra spaces to print
  for a tab at screen column x *)
function getTabPadding(x:integer):integer;
begin
	getTabPadding := 7 - ((x - 1) mod 8);
end;


function findStrInd(lineno:integer;screenX:integer):integer;
var ind,x:integer;
    c:char;
    expTab:boolean;
    lineptr:^linestr;
    linelen:integer;
begin
	lineptr := getLineRef(lineno);
	linelen := length(lineptr^);
	ind := 1; x := 1; expTab := false;
	while x < screenX do
	begin
		if ind > linelen then
			break;
		c := lineptr^[ind];
		if expTab then
		begin
			x := x + getTabPadding(x);
			expTab := false;
		end;
		if c = #9 then
			expTab := true;

		x := x + 1;
		ind := ind + charBytes(lineno,ind);
	end;
	(* debugOut('findStrInd', ind, -1 ); *)
	findStrInd := ind;
end;

(* calculate cursor position from string index of the
	specified line *)
function findScreenX(lineno:integer;strind:integer):integer;
var x:integer;
    c:char;
    expTab:boolean;
    lineptr:^linestr;
begin
	lineptr := getLineRef(lineno);
	x := 0; expTab := false;
	for c in lineptr^ do
	begin
		if expTab then
		begin
			x := x + getTabPadding(x);
			expTab := false;
		end;

		strind := strind - 1;
		if c = #9 then
			expTab := true;
		if isMBLead(c) or not isMBCont(c) then
			x := x + 1;
		if strind = 0 then
			break;
	end;
	(* can return zero if the line is empty *)
	findScreenX := x;
end;

procedure showPage; forward;

(* Calculate new cursor and line buffer position.
   The line buffer position can differ from the
   cursor position due to tabs and multibyte characters.
   The cursor position may be changed because it is inside
   an expanded tab character.
   Changes curX and screenX global variables.
*)
procedure reposition;
var x,ind:integer;
    c:char;
    expTab:boolean;
    indPad:integer;
    lineptr:^linestr;
    linelen:integer;
begin
	lineptr := getLineRef(curY);
	linelen := length(lineptr^);

	x := 0; ind := 0; expTab := false; indPad := 0;
	while x < screenX do
	begin
		ind := ind + 1;
		if ind > linelen then
		begin
			ind := ind - 1;
			break;
		end;

		if indPad > 0 then
		begin
			ind := ind + indPad;
			indPad := 0;
		end;

		if expTab then
		begin
			x := x + getTabPadding(x);
			expTab := false;
		end;

		x := x + 1;

		c := lineptr^[ind];
		if c = #9 then
			expTab := true
		else
		if isMBChar(c) then
			indPad := charBytes(curY,ind)-1;
	end;
	if ind < 1 then ind := 1;
	if x < 1 then x := 1;
	curX := ind;
	screenX := x;
	(* debugOut('reposition ', curX, x); *)
	if screenX <= colOffs then
	begin
		colOffs := (screenX div hScrollDelta) * hScrollDelta;
		(* debugOut('repos',screenX, colOffs); *)
		showPage;
	end;
	moveCursor;
end;

procedure hscroll;
begin
	if screenX > colOffs + screenW then
	begin
		(* move viewport so the cursor is visible *)
		colOffs := (screenX div hScrollDelta) * hScrollDelta;
		(* move viewport again to the end of the line if close
			to the right border *)
		colOffs := colOffs - ((screenW div hScrollDelta) - 1) * hScrollDelta;
		(* debugOut('eolR',screenX, colOffs); *)
		showPage;
	end
	else
	if screenX < colOffs then
	begin
		(* move viewport so the cursor is visible *)
		colOffs := (screenX div hScrollDelta) * hScrollDelta + hScrollDelta;
		showPage;
	end;

	moveCursor;
end;

procedure gotoCol(col:integer);
var l:lineref;
    len:integer;
begin
	l := getLineRef(curY);
	len := length(l^);
	if len = 0 then (* empty line *)
	begin
		curX := 1;
		screenX := 1;
	end
	else
	begin
		curX := col;
		screenX := findScreenX(curY, curX);
	end;
	hscroll;
end;

function isKeyword(var s:string):boolean;
var i:integer;
    c:char;
    upBuf:string[MAX_LENGTH];
begin
	isKeyword := false;

	if highlight then
	begin
		upBuf := '';
		for c in s do appendchar(upBuf,upcase(c));


		for i := 0 to MAX_KEYWORD do
		begin
			if keywords[i] = upBuf then
			begin
				isKeyword := true;
				break;
			end;
		end;
	end;
end;

function isalpha(c:char):boolean;
begin
	isalpha :=	((ord(c) >= ord('A')) and (ord(c) <= ord('Z'))) or
			((ord(c) >= ord('a')) and (ord(c) <= ord('z'))) or
			(c = '_');
end;

function ispunct(c:char):boolean;
begin
	ispunct := 	((ord(c) >= ord('(')) and (ord(c) <= ord('/'))) or
			((ord(c) >= ord(':')) and (ord(c) <= ord('>'))) or
			((ord(c) >= ord('[')) and (ord(c) <= ord('^')));
end;

function getCat(c:char):HiliteCat;
begin
	if isalpha(c) then
		getCat := Keyword
	else
	if isdigit(c) then
		getCat := Number
	else
	if iswhite(c) then
		getCat := WhiteSpc
	else
	if  c = '''' then
		getCat := StrLit
	else
	if c = '{' then
		getCat := Comment
	else
	if ispunct(c) then
		getCat := Punct
	else
		getCat := Unknown;
end;

procedure markOpenComment(i:integer; flag:boolean);
begin
	lineFlags[i] := flag;
end;

function lineIsComment(i:integer):boolean;
begin
	lineIsComment := false;

	i := i - 1;

	if (i>0) and (i<=MAX_SCREENH) then
		lineIsComment := lineFlags[i];
end;

procedure showLine(i,l:integer);
var tmpl:^linestr;
    c:char;
    lastChar:char;
    cat, prevCat:HiliteCat;
    x:integer;
    maxX:integer;
    lineLen:integer;
    pad:integer;
    j:integer;
    wordBuf:string[MAX_LENGTH];
    buffering:boolean;
    inComment:boolean;
    inStrLit:boolean;
    nextColor:integer;

procedure showChar(aChar:char);
begin
	if (x > colOffs) and (x <= maxX) then
	begin
		changeColor(nextColor);
		conout(aChar);
	end;
	x := x + 1;
end;

procedure setNextColor(i:integer);
begin
	nextColor := i;
end;

procedure flushBuf;
var b:char;
begin
	if buffering then
	begin
		if isKeyword(wordBuf) then
			setNextColor(KEYWORD_FG)
		else
			setNextColor(IDENT_FG);
		for b in wordBuf do showChar(b);
		wordBuf := '';
		buffering := false;
	end;
end;

begin
	cat := Unknown;
	buffering := false;
        inStrLit := false;
	wordBuf := '';
	lastChar := #0;

	inComment := lineIsComment(i);

	GotoXY(1,i);

	maxX := colOffs + screenW;
	x := 1;
	if l <= linecount then
	begin
		tmpl := getLineRef(l);
		if inComment then
			setNextColor(COMMENT_FG);
		lineLen := length(tmpl^);
		for c in tmpl^ do
		begin
			(* handle tab characters *)
			if c = #9 then
			begin
				flushBuf;

				pad := getTabPadding(x);
				for j := 1 to pad do
					showChar(' ');
				setNextColor(curColor);
				c := ' ';
				(* c is printed below anyway, so
					make it a space *)
			end;

			(* handle comments *)
			if (c = '*') and (lastChar = '(') then
			begin
				(* we cheat and don't color the first char
					of a "( *"-style comment correctly *)
				inComment := true;
				setNextColor(COMMENT_FG);
			end;

			if inComment then
			begin
				if c = '}' then
					inComment := false
				else
				if (c = ')') and (lastChar = '*') then
				begin
					(* we also color the last char of the
						comment incorrectly for symmetry *)
					inComment := false;
					setNextColor(PUNCT_FG);
				end;
				showChar(c);
			end
			else
			if inStrLit then
			begin
				if c = '''' then
					inStrLit := false;
				showChar(c);
			end
			else
			begin
				cat := getCat(c);

				if cat = Comment then
					inComment := true;
				if cat = StrLit then
					inStrLit := true;

				if cat = Keyword then
				begin
					if not buffering then buffering := true;
					appendchar(wordBuf, c);
				end
				else
				begin
					flushBuf;
					setNextColor(catColors[cat]);
					showChar(c);
				end;
			end;
			lastChar := c;
		end;
		flushBuf;
		markOpenComment(i, inComment);
	end
	else
	begin
		TextColor(NONTEXT_FG);
		write('~');
	end;
	ClrEol;
end;

procedure redrawCurLine;
begin
	showLine(curY - topY + topMargin + 1, curY);
end;

procedure showPage;
var i,l:integer;
begin
	showCursor(false);
	saveCursor;
	l := topY;
	TextBackground(TEXT_BG);
	changeColor(TEXT_FG);
	for i := topMargin + 1  to screenH - botMargin do
	begin
		showLine(i,l);
		l := l + 1;
	end;
	TextDefault;
	restoreCursor;
	showCursor(true);
end;

procedure scrollDown;
var i,y:integer;
begin
	topY := topY - 1;
	lineFlags[topMargin] := false;
	for i := screenH downto topMargin + 1 do
		lineFlags[i] := lineFlags[i-1];
	moveCursor;
	write(#27,'M'); (* RI - scrolls down when at top margin  *)
	showLine(topMargin + 1, topY);
	moveCursor;

	(* if the new line at the top has an open comment,
		redraw all following lines until the comment is
		closed *)
	if lineFlags[topMargin + 1] = true then
	begin
		y := topY;
		for i := topMargin + 2 to 1 + pageSize do
		begin
			y := y + 1;
			showLine(i,y);
			if lineFlags[i] = false then
				break;
		end;
	end;
end;

procedure scrollUp;
var i:integer;
begin
	topY := topY + 1;
	for i := topMargin to screenH - botMargin do
		lineFlags[i] := lineFlags[i+1];
	moveCursor;
	write(#27,'D'); (* IND - scrolls up when at bot margin  *)
	showLine(topMargin + pageSize, topY + pageSize - botMargin);
	moveCursor;
end;

procedure scrollLeft;
begin
	colOffs := colOffs + hScrollDelta;
	showPage;
	moveCursor;
end;

procedure scrollRight;
begin
	colOffs := colOffs - hScrollDelta;
	showPage;
	moveCursor;
end;

procedure showScreen;
begin
	showTopStatus;
	showPage;
	moveCursor;
end;

procedure leaveLine; forward;

procedure moveUp;
begin
	if curY > 1 then
	begin
		leaveLine;
		curY := curY - 1;
		if curY < topY then
			scrollDown;
		reposition;
		updateStatus;
	end;
end;

procedure moveDown;
begin
	if curY < lineCount then
	begin
		leaveLine;
		curY := curY + 1;
		if curY >= (topY + pageSize) then
			scrollUp;
		reposition;
		updateStatus;
	end;
end;

procedure moveLeft;
var newChar:char;
begin
	if curX > 1 then
	begin
		curX := curX - 1;
		newChar := getLineRef(curY)^[curX];
		if newChar = #9 then
			screenX := findScreenX(curY, curX) + 1
		else
		if isMBChar(newChar) then
		begin
			repeat
				curX := curX - 1;
				newChar := getLineRef(curY)^[curX];
			until isMBLead(newChar);
		end;
		screenX := screenX - 1;

		if (screenX <= colOffs) then
			scrollRight;
		moveCursor;
	end;
end;

procedure moveRight;
var curChar:char;
    l:lineref;
begin
	l := getLineRef(curY);
	if curX <= length(l^) then
	begin
		curChar := l^[curX];
		if curChar = #9 then
			screenX := screenX + getTabPadding(screenX);
		curX := curX + 1;
		if isMBChar(curChar) then
			curX := curX + charBytes(curY, curX);
		screenX := screenX + 1;
	end;

	if (screenX - colOffs) >= screenW then
		scrollLeft;
	moveCursor;
end;

procedure moveBOL;
var oldColOffs:integer;
begin
	oldColOffs := colOffs;
	curX := 1;
	screenX := 1;
	colOffs := 0;
	if oldColOffs > 0 then
		showPage;
	moveCursor;
end;

procedure moveEOL;
var lastChar:char;
    l:lineref;
    len:integer;
begin
	l := getLineRef(curY);
	len := length(l^);
	if len = 0 then (* empty line *)
	begin
		curX := 1;
		screenX := 1;
	end
	else
	begin
		curX := len;
		(* we actually move the cursor one char
			past the end of the line *)
		screenX := findScreenX(curY, curX);
		(* if the last char is a tab, we
			need to apply the padding *)
		lastChar := l^[curX];
		if lastChar = #9 then
			screenX := screenX + getTabPadding(screenX);
		screenX := screenX + 1;
		curX := curX + 1;
	end;
	{ debugOut('moveEOL',curX, screenX); }
	if screenX > colOffs + screenW then
	begin
		(* move viewport so the cursor is visible *)
		colOffs := (screenX div hScrollDelta) * hScrollDelta;
		(* move viewport again to the end of the line if close
			to the right border *)
		colOffs := colOffs - ((screenW div hScrollDelta) - 1) * hScrollDelta;
		(* debugOut('eolR',screenX, colOffs); *)
		showPage;
	end;
	moveCursor;
end;

procedure pageUp;
var delta:integer;
    oldTopY:integer;
    oldY:integer;
begin
	oldY := curY;
	oldTopY := topY;
	delta := pageSize - 1;

	topY := topY - delta;

	(* don't move past first line *)
	if topY < 1 then
		topY := 1;

	curY := curY - delta;
	(* if on first page, just move
		cursor to first line *)
	if curY < 1 then
		curY := 1;

	if oldY <> curY then
	begin
		leaveLine;
		reposition;
		if topY <> oldTopY then
			showPage;
		updateStatus;
		moveCursor;
	end;
end;

procedure pageDown;
var delta:integer;
    oldY:integer;
begin
	oldY := curY;
	delta := pageSize - 1;
	if (topY + delta) < lineCount then
	begin
		topY := topY + delta;
		curY := curY + delta;

		if curY > lineCount then
			curY := lineCount;
		showPage;
	end
	else
		curY := lineCount;

	if oldY <> curY then
	begin
		leaveLine;
		reposition;
		updateStatus;
		moveCursor;
	end;
end;

function getKey:integer;
var c:char;
    c1:char;
    escSeq:string;

(* process trailing '~' character for keypad keys *)
function kpKey(k:integer):integer;
var buf:char;
begin
	kpKey := k;
	read(con,buf);
	if not (buf = '~') then
	begin
		if buf = ';' then
		begin
			(* xterm sends ESC [1;5H and ESC [1;5F for
				Ctrl-Home and Ctrl-End *)
			read(con, buf);
			if buf = '5' then
			begin
				read(con, buf);
				if buf = 'H' then
					kpKey := HOME_KEY_M
				else
				if buf = 'F' then
					kpKey := END_KEY_M;
			end;
		end
		else
		if buf = '1' then
		begin
			(* ESC [11~ for F1 key *)
			read(con, buf);
			if buf = '~' then
				kpKey := HELP_KEY
			else
				debugOut('inv F-Key', ord(buf), -1);
		end
		else
			debugOut('inv KP', ord(buf), -1);
	end;
end;

(* alt-home and alt-end in putty (?)
	are sent like this:
	ESC ESC [1~ and
	ESC ESC [4~
*)

function modKpKey:integer;
var buf:char;
begin
	modKpKey := 0;
	read(con, buf);
	if buf = '[' then
	begin
		read(con, buf);
		if buf = '1' then
			modKpKey := HOME_KEY_M
		else
		if buf = '4' then
			modKpKey := END_KEY_M;
		read(con, buf);
		if buf <> '~' then
			modKpKey := 0;
	end;
end;

(* F1 in xterm: ESC O P *)
function fnKey:integer;
var buf:char;
begin
	fnKey := 0;
	read(con, buf);
	if buf = 'P' then
		fnKey := HELP_KEY;
end;

begin (* getKey *)
	read(con,c);
	if c = #27 then
	begin
		read(con,c);
		if c = '[' then
		begin
			read(con,c);
			case c of
			'D':		getKey := ARROW_LEFT;
			'A':		getKey := ARROW_UP;
			'C':		getKey := ARROW_RIGHT;
			'B':		getKey := ARROW_DOWN;
			'H':		getKey := HOME_KEY;
			'F':		getKey := END_KEY;
			'1':		getKey := kpKey(HOME_KEY);
			'2':		getKey := kpKey(INSERT_KEY);
			'3':		getKey := kpKey(DELETE_KEY);
			'4':		getKey := kpKey(END_KEY);
			'5':		getKey := kpKey(PG_UP);
			'6':		getKey := kpKey(PG_DOWN);
			'7'..'9','0':	getKey := kpKey(0);
			else
				getKey := 0;
			end;
		end
		else
		if c = 'O' then
			getKey := fnKey
		else
		if c = #27 then
			getKey := modKpKey
		else
			getKey := 0; (* unknown escape sequence *)
	end
	else
		getKey := ord(c);
end;

function makeNewLine(len:integer):lineref;
var nl:lineref;
begin
	if len = 0 then
		makeNewLine := nil
	else
	begin
		new(nl, len);
		nl^ := '';
		makeNewLine := nl;
	end;
end;

function makeLineCopy(l:lineref):lineref;
var
    newl:lineref;
begin
	if l = nil then
		makeLineCopy := nil
	else
	if length(l^) = 0 then
		makeLineCopy := nil
	else
	begin
		new(newl, length(l^));
		newl^ := l^;
		makeLineCopy := newl;
	end;
end;

procedure beginEditBuf;
var srcLine:lineref;
begin
	if editBuf = nil then
	begin
		isModified := true;
		new(editBuf);
		editLine := curY;
		srcLine := lines[editLine];
		if srcLine <> nil then
			editBuf^ := srcLine^
		else
			editBuf^ := '';
	end;
end;

procedure commitEditBuf;
var newLine:^linestr;
    oldLine:^linestr;
begin
	if editBuf <> nil then
	begin
		(* allocate a new string with the required size *)
		newLine := makeNewLine(length(editBuf^));
		(* copy edit buffer contents *)
		if length(editBuf^) > 0 then
			newLine^ := editBuf^;
		(* dispose old line string *)
		oldLine := lines[editLine];
		if oldLine <> nil then
			dispose(oldLine);
		(* set new line string *)
		lines[editLine] := newLine;
		(* dispose edit buffer *)
		dispose(editBuf);
		editBuf := nil;
		editLine := 0;
	end;
end;

procedure leaveLine;
begin
	commitEditBuf;
	keepClips := false;
	clearStatus;
end;

procedure insertMBChar(var bytes:string);
var l:integer;
    c:char;
    insertCount:integer;
    x:integer;
begin
	(* TODO: check for max line length *)
	beginEditBuf;
	l := length(editBuf^);
	insertCount := length(bytes);
	setLength(editBuf^, l + insertCount);
	strmoveup(editBuf^, curX, l - curX + 1, insertCount);
	x := curX;
	for c in bytes do
	begin
		editBuf^[x] := c;
		x := x + 1;
	end;
	showCursor(false);
	showLine(curY - topY + topMargin + 1, curY);
	moveRight;
	showCursor(true);
end;

procedure backspaceChar;
var c:char;
    l,bcount:integer;
    x:integer;
    isAtEOL:boolean;
    done:boolean;
begin
	beginEditBuf;
	l := length(editBuf^);
	bcount := 0;
	x := curX;
	isAtEOL := x > l;

	repeat
		x := x - 1;
		c := editBuf^[x];
		bcount := bcount + 1;
	until isMBLead(c) or (not isMBChar(c));

	(* FIXME: wrong cursor movement if backspacing
		at the end of a line consisting of multiple
		tabs *)

	(* TODO: refactor moveLeft and backspaceChar
		to reuse common code *)
	strmovedown(editBuf^, x, l - curX + 1, bcount);
	setLength(editBuf^, l - bcount);

	showCursor(false);

	{ debugOut('bsCh', curX, bcount); }

	curX := curX - bcount;

	if c = #9 then
	begin
		(* find new screen position of the cursor *)
		screenX := findScreenX(curY, curX);
		(* if the cursor was positioned after the
			end of the line, and we deleted a tab,
			just use moveEOL to deal with the
			new cursor position *)
		if isAtEOL then
			moveEOL;
	end
	else
		screenX := screenX - 1;

	showLine(curY - topY + topMargin + 1, curY);
	hscroll;
	showCursor(true);
end;

procedure deleteChar;
var c:char;
    l,bcount:integer;
begin
	beginEditBuf;
	l := length(editBuf^);
	c := editBuf^[curX];
	bcount := getMBLength(c);

	strmovedown(editBuf^, curX, l - curX - bcount + 1, bcount);
	setLength(editBuf^, l - bcount);
	showCursor(false);
	showLine(curY - topY + topMargin + 1, curY);
	moveCursor;
	showCursor(true);
end;

procedure trimLine(l:lineref);
var len:integer;
    done:boolean;
begin
	done := false;
	repeat
		len := length(l^);
		if len > 0 then
			if l^[len] in [ ' ', #9 ] then
				setLength(l^, len - 1)
			else
				done := true
		else
			done := true;
	until done;
end;

procedure splitLine;
var newLine:^linestr;
    l:integer;
    x,y:integer;
    c:char;
begin
	beginEditBuf;

	l := length(editBuf^) - curX + 1;

	newLine := makeNewLine(l);
	if l > 0 then
		for x := curX to curX + l - 1 do
			appendchar(newLine^, editBuf^[x]);

	setLength(editBuf^, curX - 1);

	commitEditBuf;

	for y := lineCount downto curY + 1 do
		lines[y + 1] := lines[y];

	lines[y + 1] := newLine;
	lineCount := lineCount + 1;

	curX := 1; screenX := 1; colOffs := 0;
	moveDown;
	showPage;
end;

procedure joinUpLine;
var c:char;
    nextLine:lineref;
    i:integer;
begin
	beginEditBuf;
	if curY < lineCount then
	begin
		nextLine := lines[curY + 1];
		if nextLine <> nil then
		begin
			for c in nextLine^ do
				appendchar(editBuf^, c);
			dispose(nextLine);
		end;
		commitEditBuf;
		for i := curY + 1 to linecount - 1 do
			lines[i] := lines[i+1];

		linecount := linecount - 1;
	end;
end;

procedure backspaceKey;
var predLine:integer;
    newX:integer;
begin
	if curX > 1 then
		backspaceChar
	else
	if curY > 1 then
	begin
		leaveLine;
		curY := curY - 1;
		moveEOL;
		joinUpLine;
		showPage;
	end;
end;

procedure deleteKey;
var nextLine:integer;
    len:integer;
    newX:integer;
begin
	len := getLineLength(curY);

	if curX <= len then
		deleteChar
	else
	if curY < lineCount then
	begin
		leaveLine;
		joinUpLine;
		showPage;
	end;
end;

procedure tab;
var buf:string[1];
begin
	buf := #9;
	insertMBChar(buf);
end;

procedure enter;
var lastLine:lineref;
    c:char;
    i:integer;
    buf:string[4];
begin
	splitLine;
	if autoindent and (curY > 1) then
	begin
		lastLine := getLineRef(curY - 1);
		if length(lastLine^) > 0 then
		begin
			if length(lastLine^) > 3 then
			begin
				if copy(lastLine^,1,4) = 'var ' then
				begin
					buf := '    ';
					insertMBChar(buf);
					moveEOL;
				end;
			end;

			for c in lastLine^ do
			begin
				if (c <> #32) and (c <>  #9 ) then
					break;
				buf := c;
				insertMBChar(buf);
			end;
		end;
		trimLine(lastLine);
	end;
	clearStatus;
end;

procedure writeFile(var success:boolean); forward;

procedure save;
var success:boolean;
begin	
	writeFile(success);
	if success then
	begin
		isNewFile := false;
		isModified := false;
		updateStatus;
	end;
end;

procedure saveNQuit(var success:boolean);
begin
	writeFile(success);
end;

procedure undo;
begin
	if editLine > 0 then
	begin
		dispose(editBuf);
		editLine := 0;
		editBuf := nil;
		showCursor(false);
		redrawCurLine;
		reposition;
		showCursor(true);
	end;
end;

procedure quit(var success:boolean);
var key:integer;
begin
	if isModified then
	begin
		statusMsg('File was modified, press [RETURN] to discard changes and quit...', true, false);
		key := getKey;
		success := key = 13;
		if not success then
			clearStatus;
	end
	else
		success := true;
end;

procedure gotoLine(l:integer);
begin
	if l < 1 then
		l := 1
	else
	if l > lineCount then
		l := lineCount;

	topY := l - (screenH div 2);
	if topY < 1 then
		topY := 1;
	curY := l;
        curX := 1;
	showPage;
	reposition;
	updateStatus;
end;

procedure cleanup;
begin
	close(con);
	resetScreen;
end;

function endsWith(var s:string; suffix:string):boolean;
var len, lenSuffix:integer;
begin
	endsWith := false;

	len := length(s);
	lenSuffix := length(suffix);
	if len >= length(suffix) then
		endsWith :=  suffix = copy(s, len - lenSuffix + 1, lenSuffix);
end;

function isPasFile(var filename:string):boolean;
begin
	isPasFile := endsWith(filename, '.pas');
end;

function isAsmFile(var filename:string):boolean;
begin
	isAsmFile := endsWith(filename, '.s');
end;

procedure buildNRun(doAsm:boolean;doRun:boolean);
var error:integer;
    success:boolean;
    args:PArgVec;
    argPos:integer;
    prg:string;
begin
	success := true;
	if isModified then
		save;
	if success then
	begin
		if isAsmFile(filename) then
			prg := ASMPROG
		else
			prg := COMPILERPROG;

		cleanup;
		writeln('Running ', prg ,'...');
		(* ask the shell to start the editor
			again after the last program exits *)
		SetShellCmd('WE', curY);

		(* ask the compiler/assembler to call
			the editor on error *)
		args[0] := '-e';
		argPos := 1;
		if not doAsm then
		begin
			(* tell the compiler to only
				create the assembly file *)
			args[argPos] := '-S';
			argPos := argPos + 1;
		end;
		if doRun then
		begin
			(* ask the compiler/assembler to run the program *)
			args[argPos] := '-R';
			argPos := argPos + 1;
		end;
		args[argPos] := filename;
		PExec(prg, args, argPos + 1, error);
		writeln('PExec failed, error ', error);
	end;
end;

procedure switchOptions;
begin
	autoindent := not autoindent;
	updateStatus;

	(* toggle autoindent,
	toggle case-insensitve search?*)
end;

procedure askLine;
var buf:string[12];
    l,p:integer;
begin
	prompt('Go to line:', buf, true);
	val(buf, l, p);
	if p = 0 then
		gotoLine(l);
end;

procedure clearClipboard;
var ci:integer;
begin
	for ci := 1 to clipboardSz do
	begin
		if clipboard[ci] <> nil then
		begin
			dispose(clipboard[ci]);
			clipboard[ci] := nil;
		end;
	end;

	clipboardSz := 0;
end;

procedure lineToClipboard(var success:boolean);
var l:lineref;
    cur:lineref;
begin
	if not keepClips then
		clearClipboard;

	if clipboardSz < MAX_CLIPB_SIZE then
	begin
		clipboardSz := clipboardSz + 1;

		cur := getLineRef(curY);
		l := makeNewLine(length(cur^));
		if l <> nil then
			l^ := cur^;
		clipboard[clipboardSz] := l;
		success := true;
	end
	else
	begin
		success := false;
		statusMsg('Clipboard full', true, false);
	end;
end;

procedure copyLine;
var success:boolean;
begin
	lineToClipboard(success);
	if success then
	begin
		moveBOL;
		moveDown;
		keepClips := true;
		updateStatus;
	end;
end;

procedure deleteLine;
var l:lineref;
    success:boolean;
begin
	commitEditBuf;
	lineToClipboard(success);
	keepClips := true;
	if lines[curY] <> nil then
	begin
		dispose(lines[curY]);
		lines[curY] := nil;
	end;
	joinUpLine;
	if curY > lineCount then
		curY := lineCount;
	if lineCount = 0 then
	begin
		curY := 1;
		linecount := 1;
		lines[1] := makeNewLine(0);
	end;

	moveBOL;
	showPage;
	updateStatus;
end;

procedure insertClipboard;
var i:integer;
    ci:integer;
begin
	if lineCount + clipboardSz <= MAX_LINES then
	begin
		if clipboardSz > 0 then
		begin
			(* move rest of the lines away *)
			for i := lineCount downto curY do
				lines[i + clipboardSz] := lines[i];

			ci := 1;
			for i := curY to curY + clipboardSz - 1 do
			begin
				lines[i] := makeLineCopy(clipboard[ci]);
				ci := ci + 1;
			end;

			lineCount := lineCount + clipboardSz;

			isModified := true;
		end
	end
	else
		statusMsg('Maximum number of lines reached.', true, false);
end;

procedure paste;
begin
	commitEditBuf;
	insertClipboard;
	showPage;
	keepClips := false;
	updateStatus;
end;

procedure enableCollect;
begin
	keepClips := true;
	updateStatus;
end;

procedure findReplace(ignoreCase:boolean);
var buf:string[40];
    p:integer;
    i:integer;
    l:lineref;
    done:boolean;
    startPos:integer;
    replace:boolean;
    reverse:boolean;
    wrapped:boolean;
    savedX,savedY:integer;
    replaceBuf:string[40];
    linesSearched:integer;

procedure doReplace;
var delta:integer;
    s,d:integer;
    tailLength:integer;
begin
	if replace = false then
	begin
		replace := true;
		prompt('Replace with:', replaceBuf, false);
	end;
	if p > 0 then
	begin
		beginEditBuf;
		delta := length(replaceBuf) - length(buf);

		if delta > 0 then
		begin
			tailLength := length(editBuf^) - p + 1 - length(buf);
			setLength(editBuf^, length(editBuf^) + delta);
			strmoveup(editBuf^, p + length(buf),
				tailLength, delta);
		end
		else
		if delta < 0 then
		begin
			strmovedown(editBuf^, p,
				length(editBuf^) - p  + delta + 1, -delta);
			setLength(editBuf^, length(editBuf^) + delta);
		end;

		(* TODO: check for max line length *)

		s := 1;
		for d := p to p + length(replaceBuf) - 1 do
		begin
			editBuf^[d] := replaceBuf[s];
			s := s + 1;
		end;
		(* commitEditBuf; *)
		showCursor(false);
		TextDefault;
		redrawCurLine;
		showCursor(true);

		moveCursor;
	end;
end;

procedure findPrompt;
var key:integer;
    ch:char;
    valid:boolean;
begin
	showCursor(false);
	GotoXY(1,ScreenH);
	TextColor(BOTSTAT_FG_P);
	TextBackground(BOTSTAT_BG_P);
	ClrEol;
	write('  Find: <space> next, [RET] finish, [B]ackwards, [R]eplace (');
	if ignoreCase then write('Ign.')
	else write('Match ');
	write('Case');
	if wrapped then write(' WRAP', #7);
	write(')>');

	moveCursor;

	showCursor(true);

	repeat
		key := getKey;
		valid := true;
		case key of
		13:	done := true; (* CR *)
		32:	reverse := false; (* Space *)
		84,114:	doReplace;	(* R, r *)
		66,98:	reverse := true;  (* B, b *)
		else
		begin
			valid := false;
			write(con, #7); (* BEL *)
		end;
	end;
	until valid;
	TextDefault;
end;

(* modified from stdlib.pas *)
function posIgnCase(substr:string;var s:string;startPos:integer):integer;
var substrlen:integer;
    slen:integer;
    searchpos:integer;
    subchar:char;
    subpos:integer;
    found:boolean;
    c1,c2:char;
    delta:integer;
begin
        found := false;
	substrlen := length(substr);
	slen := length(s);

	subpos := 1;

	if reverse then
	begin
		searchpos := startPos - 1;
		delta := -1;
	end
	else
	begin
		searchpos := startPos;
		delta := 1;
	end;

	if(substrlen > 0) and (slen>0) and ((startPos + substrlen - 1) <= slen) then
	begin
		while not found and (searchpos + subpos - 1 <= slen) and (searchpos > 0) do
		begin
			(* compare character by character *)
			c1 := substr[subpos];
			c2 := s[searchpos + subpos - 1];
			if ignoreCase then
			begin
				c1 := upcase(c1);
				c2 := upcase(c2);
			end;

			if c1 <> c2 then
			begin
				(* If a character does not match, reset the
					character index of the substring
					and go to next character *)
				searchpos := searchpos + delta;
				subpos := 1;
			end
			else
			begin
				(* character does match *)
				if subpos = 1 then
					(* remember start of this search attempt *)
					posIgnCase := searchpos;

				(* if this was the last character of the substring,
					we are successful *)
				if subpos = substrlen then
					found := true
				else
					(* else go to next characters *)
					subpos := subpos + 1;
			end;
		end;
	end;

	if not found then
		posIgnCase := 0;
end;

begin (* findReplace *)
	replace := false;
	reverse := false;
	wrapped := false;

	savedX := curX;
	savedY := curY;
        linesSearched := 0;
	startPos := curX;
	prompt('Find text:', buf, false);

	if length(buf) > 0 then
	begin
		leaveLine;
		i := curY;
		done := false;
		repeat
			l := getLineRef(i);
			p := posIgnCase(buf,l^,startPos);
			if p > 0 then
			begin
				linesSearched := 0;
				gotoLine(i);
				gotoCol(p);
				savedX := curX;
				savedY := curY;
				findPrompt;
				wrapped := false;
				if reverse then
					startPos := p - 1
				else
					startPos := p + 1;
			end
			else
			begin
				(* move to next line *)
				leaveLine;
				linesSearched := linesSearched + 1;
				if reverse then
				begin
					i := i - 1;
					if i < 1 then
					begin
						i := linecount;
						wrapped := true;
					end;
					startPos := getLineLength(i) - length(buf) + 1;
				end
				else
				begin
					startPos := 1;
					i := i + 1;
					if i > linecount then
					begin
						i := 1;
						wrapped := true;
					end;
				end;

				if linesSearched > lineCount then
				begin
					statusMsg('Nothing found.', false, true);
					done := true;
				end;
			end;
		until done;
	end;

	gotoLine(savedY);
	curX := savedX;
	reposition;
	statusMsg('', false, false);
end;

procedure writeClip;
var newFilename:string[50];
    clipfile:file;
    ci:integer;
begin
	prompt('Write clipboard to file:', newFilename, false);
	if length(newFilename) > 0 then
	begin
		open(clipfile, newfilename, ModeCreate);
		if IOResult(clipfile) = IONoError then
		begin
			for ci := 1 to clipboardSz do
				writeln(clipfile, clipboard[ci]^);
			close(clipfile);
			statusMsg('Write successful.', false, false);
		end
		else
			statusMsg('Write clipboard failed - ' +
				ErrorStr(IOResult(clipfile)), true, true);
	end;
end;

procedure readClip;
var fname:string[50];
    clipfile:file;
    ci:integer;
    linebuf:string[255];
    l:lineref;
    error:boolean;
begin
	prompt('Read clipboard from file:', fname, false);

	if length(fname) = 0 then exit;

	clearClipboard;
	ci := 0;
	open(clipfile, fname, ModeReadonly);
	if IOResult(clipfile) <> IONoError then
		statusMsg('Error reading file - '
			+ ErrorStr(IOResult(clipfile)), true, true)
	else
	begin
		error := false;
		while not eof(clipfile) or error do
		begin
			if ci >= MAX_CLIPB_SIZE then
			begin
				statusMsg('Clipboard full', true, false);
				error := true;
			end
			else
			begin
				readln(clipfile, linebuf);
				ci := ci + 1;
				l := makeNewLine(length(linebuf));
				if l <> nil then
					l^ := linebuf;
				clipboard[ci] := l;
			end;
		end;
		clipboardSz := ci;
		close(clipfile);
		if not error then
			statusMsg('File successfully read to clipboard.', false, false);
	end;
	updateStatus;
end;

procedure saveNewFile(var success:boolean);
var newFilename:string[50];
    oldName:string;
    oldNewFlag:boolean;
begin
	prompt('New filename:', newFilename, false);
	if length(newFilename) > 0 then
	begin
		oldName := filename;
		oldNewFlag := isNewFile;

		filename := newFilename;
		isNewFile := true;
		writeFile(success);
		if not success then
		begin
			filename := oldName;
			isNewFile := oldNewFlag;
		end;
	end
	else
		success := false;
end;

procedure redraw;
begin
	getScreenSize;
	showScreen;
	gotoLine(curY);
end;

procedure helpScreen;
var i:integer;
begin
	showCursor(false);
	GotoXY(1,3);
	TextColor(15);
	(* TextBackground(24); *)
	TextBackground(23);
	TextColor(15);
	write('   Keyboard Commands                 (^ = Control + other key)');
	ClrEol;
	TextBackground(22);
	TextColor(15);
	for i := 1 to 18 do
	begin
		writeln;
		ClrEol;
	end;

	GotoXY(1,5);
	writeln('Saving/Exiting                       Editing/Clipboard');
	writeln('^Q  save and Quit                    ^Y  delete line (and copy to clipb.)');
	writeln('^W  save                             ^C  Copy line');
	writeln('^A  save As new and quit             ^V  paste clipboard contents');
	writeln('^X  eXit without saving              ^R  Read clipboard from file');
	writeln('                                     ^O  write clipboard to file');
	writeln('                                     ^K  Keep previous clipboard on ^Y/^C');
	writeln('Compiling                            ^Z  undo changes to current line');
	writeln('^B  Build (compile, assemble)');
	writeln('^N  build and ruN                    Others');
	writeln('^P  comPile only                     ^T  Toggle audoindent');
	writeln('                                     ^L  redraw screen');
	writeln('                                     ^F  Find/replace');
	writeln('Moving around                        ^E  find/replace (match casE)');
	writeln('^G Goto line');
	writeln('Move with cursor keys, Home, End, PgUp, PgDown');
	writeln;
	GotoXY(ScreenW,ScreenH);
	statusMsg('Help Screen', false, true);
	showCursor(true);
	redraw;
end;

procedure edit;
var key:integer;
    c:char;
    buf:string[4];
    i:integer;
    done:boolean;
begin
	done := false;
	repeat
		key := getKey;
		case key of
		ARROW_LEFT:moveLeft;
		ARROW_UP:moveUp;
		ARROW_RIGHT:moveRight;
		ARROW_DOWN:moveDown;
		PG_UP:pageUp;
		PG_DOWN:pageDown;
		HOME_KEY:moveBOL;
		END_KEY:moveEOL;
		INSERT_KEY:;
		DELETE_KEY:deleteKey;
		HOME_KEY_M:gotoLine(1);
		END_KEY_M:gotoLine(lineCount);
		HELP_KEY:helpScreen;
		8:  backspaceKey;
		127: backspaceKey;
		9:  tab;
		13: enter;
		2:  buildNRun(true,false);	(* ^B *)
		14: buildNRun(true, true);	(* ^N *)
		16: buildNRun(false, false);	(* ^P *)
		23: save;			(* ^W *)
		17: saveNQuit(done);		(* ^Q *)
		24: quit(done);			(* ^X *)
		15: writeClip;			(* ^O *)
		1:  saveNewFile(done);		(* ^A *)
		26: undo;			(* ^Z *)
		7:  askLine;			(* ^G *)
		18: readClip;			(* ^R *)
		20: switchOptions;		(* ^T *)
		6:  findReplace(true);		(* ^F *)
		5:  findReplace(false);		(* ^E *)
		25: deleteLine;			(* ^Y *)
		3:  copyLine;			(* ^C *)
		22: paste;			(* ^V *)
		11: enableCollect;		(* ^K *)
		12: redraw;			(* ^L *)
		else
			if key > 31 then
			begin
				buf := '';
				c := chr(key);
				appendchar(buf,c);
				for i := 2 to getMBLength(c) do
				begin
					c := chr(getKey);
					appendchar(buf, c);
				end;
				insertMBChar(buf);
			end
			else
				statusMsg('                            Press F1 for help.', false, false);
		end;
	until done; (* ^X *)
end;

procedure readFile(var f:file);
var
    tmpline:^linestr;
begin
	write('Reading file ', filename, '...');
	linecount := 0;
	while not eof(f) do
	begin
		linecount := linecount + 1;
		readln(f, linebuf);
		if length(linebuf) = 0 then
			tmpline := nil
		else
		begin
			new(tmpline, length(linebuf));
			tmpline^ := linebuf;
		end;
		lines[linecount] := tmpline;
		if (linecount and 511) = 0 then
			write('.');
	end;
	isNewFile := false;
	isModified := false;
end;

procedure newFile;
begin
	linecount := 1;
	lines[1] := makeNewLine(0);
	isNewFile := true;
	isModified := false;
end;

procedure writeFile(var success:boolean);
var mode:filemode;
    outfile:file;
    i:integer;
    l:lineref;
begin
	if isNewFile then
		mode := ModeCreate
	else
		mode := ModeOverwrite;

	open(outfile, filename, mode);

	if IOResult(outfile) <> IONoError then
	begin
		statusMsg('Write failed - ' + ErrorStr(IOResult(outfile)), true, true);
		success := false;
	end
	else
	begin
		commitEditBuf;
		statusMsg('Writing file...', false, false);
		success := true;
		for i := 1 to linecount do
		begin
			l := lines[i];
			if l <> nil then
				writeln(outfile, l^)
			else
				writeln(outfile);
			if IOResult(outfile) <> IONoError then
			begin
				statusMsg('Error writing file: ' +
					ErrorStr(IOResult(outfile)), true, true);
				success := false;
				break;
			end;
		end;
		close(outfile);
		if success then
		begin
			statusMsg('Write successful.', false, false);
			isModified := false;
		end;
	end;
end;

begin
	errorLine := 0;
	paramPos := 1;
	filename := '';
	autoindent := true;
	keepClips := false;

	while paramPos <= ParamCount do
	begin
		if paramStr(paramPos) = '-l' then
		begin
			paramPos := paramPos + 1;
			errLineStr := ParamStr(paramPos);
			val(errLineStr, errorLine, i);
		end
		else
		if paramStr(paramPos) = '-E' then
		begin
			paramPos := paramPos + 1;
			errorMsg := ParamStr(paramPos);
		end
		else
			filename := ParamStr(paramPos);
		paramPos := paramPos + 1;
	end;

	if length(filename) = 0 then
	begin
		write('Filename: ');
		readln(filename);
	end;

	highlight := isPasFile(filename);

	open(infile, filename, ModeReadonly);
	if IOResult(infile) = IOFileNotFound then
		newFile
	else
	if IOResult(infile) <> IONoError then
	begin
		writeln('Error opening file ', filename, ': ',
			ErrorStr(IOResult(infile)));
		halt;
	end
	else
	begin
		readFile(infile);
		close(infile);
	end;

	open(con, '%RAW', ModeOverwrite);

	initScreen;
	showScreen;

	if isNewFile then
		statusMsg('New file.                           Press F1 for help.', false, false)
	else
		statusMsg('Successfully read file.             Press F1 for help', false, false);


	if errorLine > 0 then
	begin
		gotoLine(errorLine);
		if length(errorMsg) > 0 then
			statusMsg('E: ' + errorMsg + ' at line ' + errLineStr, true, true);
	end;

	edit;

	(* hack to remember the current line
		if we open the same file again *)
	SetShellCmd('', curY);

	cleanup;
end.
