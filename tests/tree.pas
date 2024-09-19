program tree;
type TreedataType = (TDString, TDInteger);
type Treedata = record
        case typ:Treedatatype of 
        TDString:(stringdata:string);
        TDInteger:(intdata:integer);
    end;
type TreeNode = record
        parent: ^TreeNode;
        left,right: ^TreeNode;
        height: integer;
	key: ^string;
	data: ^Treedata;
    end;

type TreeRef = ^TreeNode;

type TreeWalkState = record
		currentNode:TreeRef;
	end;

var t:TreeRef;
    k:string;
    d:TreeData;
    i:integer;
    searchres:^Treedata;
    walkState:TreeWalkState;
    walkRes:TreeRef;

procedure mem_dump; external;

function makeTreeNode(var d:TreeData;var key:string;nparent:TreeRef):TreeRef;
var newNode:TreeRef;
    newKey:^string;
begin
	new(newNode);
        new(newKey,length(key));
	{ new(newKey); }
	new(newNode^.data);
	newKey^ := key;
	with newNode^ do
	begin
		key := newKey;
		parent := nparent;
		left := nil;
		right := nil;
		height := 1;
		data^ := d;
	end;
	makeTreeNode := newNode;
end;

function MeasureTree(root:TreeRef):integer;
var leftHeight, rightHeight:integer;
begin
	if root = nil then
		MeasureTree := 0
	else
	begin
		if root^.left <> nil then
			leftHeight := root^.left^.height
		else
			leftHeight := 0;
		if root^.right <> nil then
			rightHeight := root^.right^.height
		else
			rightHeight := 0;
		if rightHeight > leftHeight then
			MeasureTree := rightHeight + 1
		else
			MeasureTree := leftHeight + 1;
	end;
end;

function GetTreeBalance(root:TreeRef):integer;
begin
	if root = nil then
		GetTreeBalance := 0
	else
		GetTreeBalance := MeasureTree(root^.left) - MeasureTree(root^.right);
end;

function RotateTreeRight(x:TreeRef):TreeRef;
var z,tmp:TreeRef;
begin
	writeln('RotateTreeRight at ', x^.key^);
	z := x^.left;
	tmp := z^.right;
	z^.right := x;
	z^.parent := x^.parent;
	x^.parent := z;
	x^.left := tmp;
	if tmp <> nil then
		tmp^.parent := x;
	x^.height := MeasureTree(x);
	z^.height := MeasureTree(z);
	RotateTreeRight := z;
end;

function RotateTreeLeft(x:TreeRef):TreeRef;
var z,tmp:TreeRef;
begin
	writeln('RotateTreeLeft at ', x^.key^);
	z := x^.right;
	tmp := z^.left;
	z^.left := x;
	z^.parent := x^.parent;
	x^.parent := z;
	x^.right := tmp;
	if tmp <> nil then
		tmp^.parent := x;
	x^.height := MeasureTree(x);
	z^.height := MeasureTree(z);
	RotateTreeLeft := z;
end;

function TreeInsert4(root:TreeRef;var key:string;var data:TreeData;
	parent:TreeRef):TreeRef;
var balance:integer;
begin
	if root = nil then
		root := makeTreeNode(data, key, parent)
	else
	if key < root^.key^ then
		root^.left := TreeInsert4(root^.left, key, data, root)
	else
		root^.right := TreeInsert4(root^.right, key, data, root);

	root^.height := MeasureTree(root);

	balance := GetTreeBalance(root);
	if balance > 1 then
	begin
		if key < root^.left^.key^ then
			root := RotateTreeRight(root)
		else
		begin
			root^.left := RotateTreeLeft(root^.left);
			root := RotateTreeRight(root);
		end;
	end
	else
	if balance < -1 then
	begin
		if key > root^.right^.key^ then
			root := RotateTreeLeft(root)
		else
		begin
			root^.right := RotateTreeRight(root^.right);
			root := RotateTreeLeft(root);
		end;
	end;

	TreeInsert4 := root;
end;

procedure TreeInsert(var root:TreeRef;var key:string;var data:TreeData);
begin
	root := TreeInsert4(root,key,data,nil);
end;

procedure DisposeTreeNode(node:TreeRef);
begin
	dispose(node^.key);
	dispose(node^.data);
	dispose(node);
end;

function TreeLeftmost(node:TreeRef):TreeRef;
begin
	TreeLeftmost := nil;
	if node <> nil then
	begin
		repeat
			TreeLeftmost := node;
			node := node^.left;
		until node = nil;
	end;
end;

procedure PrintTreeRef(node:TreeRef);
begin
	if node = nil then
		write('nil')
	else
		write(node^.key^);
end;

procedure PrintTreeNode(node:TreeRef);
begin
	write(' -');
	PrintTreeRef(node);
	if node <> nil then
	begin
		write(' ^');
		PrintTreeRef(node^.parent);
		write(' <');
		PrintTreeRef(node^.left);
		write(' >');
		PrintTreeRef(node^.right);
	end;
	writeln;
end;

function TreeDeleteFn(root:TreeRef;var key:string):TreeRef;
var tmp,oldParent:TreeRef;
    balance:integer;
begin
	if root <> nil then
	begin
		if key < root^.key^ then
			root^.left := TreeDeleteFn(root^.left, key)
		else
		if key > root^.key^ then
			root^.right := TreeDeleteFn(root^.right, key)
		else
		begin
			if root^.left = nil then
			begin
				tmp := root;
				oldParent := root^.parent;
				root := root^.right;
				if root <> nil then
					root^.parent := oldParent;
				DisposeTreeNode(tmp);
			end
			else
			if root^.right = nil then
			begin
				tmp := root;
				oldParent := root^.parent;
				root := root^.left;
				if root <> nil then
					root^.parent := oldParent;
				DisposeTreeNode(tmp);
			end
			else
			begin
				writeln('TreeDelete search leftmost from ', root^.key^);
				PrintTreeNode(root);
				tmp := TreeLeftmost(root^.right);
				if maxlength(tmp^.key^) <> maxlength(root^.key^) then
				begin	(* reallocate key, the swapped key might have a different length *)
					write('reallocating key ', length(root^.key^));
					dispose(root^.key);
					new(root^.key, length(tmp^.key^));
					writeln(' -> ', maxlength(root^.key^));
				end;
				root^.key^ := tmp^.key^;
				root^.data^ := tmp^.data^;
				writeln('TreeDelete delete leftmost ', tmp^.key^);
				PrintTreeNode(tmp);
				writeln('oldParent: ');
				PrintTreeNode(tmp^.parent);
				oldParent := tmp^.parent;
				if oldParent^.left = tmp then
					oldParent^.left := TreeDeleteFn(oldParent^.left, tmp^.key^)
				else
				if oldParent^.right = tmp then
					oldParent^.right := TreeDeleteFn(oldParent^.right, tmp^.key^)
				else
					writeln('TreeDelete internal error');
			end;

			if root <> nil then
			begin
				root^.height := MeasureTree(root);
				balance := GetTreeBalance(root);
				if balance > 1 then
				begin
					if GetTreeBalance(root^.left) >=0 then
						root := RotateTreeRight(root)
					else
					begin
						root^.left := RotateTreeLeft(root^.left);
						root := RotateTreeRight(root);
					end;
				end
				else
				if balance < -1 then
				begin
					if GetTreeBalance(root^.right) <= 0 then
						root^.right := RotateTreeLeft(root)
					else
					begin
						root^.right := RotateTreeRight(root^.right);
						root := RotateTreeLeft(root);
					end;
				end;
			end;
		end;
	end;
	TreeDeleteFn := root;
end;

procedure TreeDelete(var root:TreeRef;var key:string);
begin
	root := TreeDeleteFn(root,key);
end;

function TreeSearch(root:TreeRef;var key:string):^TreeData;
begin
	if root <> nil then
	begin
		if key = root^.key^ then
			TreeSearch := root^.data
		else
		if key < root^.key^ then
			TreeSearch := TreeSearch(root^.left, key)
		else
			TreeSearch := TreeSearch(root^.right, key);
	end
	else
		TreeSearch := nil;
end;

procedure TreeWalkStart(t:TreeRef; var state:TreeWalkState);
begin
	(* start at leftmost node of the tree *)
	state.currentNode := TreeLeftmost(t);
end;

procedure TreeWalkNext(var state:TreeWalkState;var res:TreeRef);
var last,current,old,right:TreeRef;
begin
	current := state.currentNode;

	res := current;

	if current <> nil then
	begin
			(* descending right *)
		if current^.right <> nil then
		begin
			state.currentNode := TreeLeftmost(current^.right);
		end
		else	(* ascending *)
		begin
			old := current;
			repeat
				last := current;
				current := current^.parent;
				if current <> nil then
					right := current^.right;
			until (right <> last) or (current = nil); (* ascend left edges *)
			state.currentNode := current;
		end;
	end;
end;

procedure indent(i:integer);
var c:integer;
begin
	for c := 1 to i do
		write('  ');
end;

procedure PrintStringTree(node:TreeRef;level:integer);
begin
	if node <> nil then
	begin
		if node^.left <> nil then
			PrintStringTree(node^.left, level + 1);
		indent(level);
		PrintTreeNode(node);
		if node^.right <> nil then
			PrintStringTree(node^.right, level + 1);
	end;
end;

procedure DoASearch(t:TreeRef; s:string);
var res:^TreeData;
begin
	res := TreeSearch(t, s);
	write('searching for ',s);
	if res = nil then
		writeln(' nil')
	else
		writeln(res^.stringdata);
end;

begin
	mem_dump;
	{
	t := nil;
	k := 'test1';
	d.typ := TDString;
	d.stringdata := 'data1';
	TreeInsert(t,k,d);

	k := 'test0';
	d.typ := TDString;
	d.stringdata := 'data0';
	TreeInsert(t,k,d);

	k := 'test3';
	d.typ := TDString;
	d.stringdata := 'data3';
	TreeInsert(t,k,d);

	k := 'test2';
	d.typ := TDString;
	d.stringdata := 'data2';
	TreeInsert(t,k,d);

	k := 'test4';
	d.typ := TDString;
	d.stringdata := 'data4';
	TreeInsert(t,k,d);

	writeln('root: ', t^.key^);

	PrintStringTree(t,1);
	}
	writeln('------------');

	t := nil;
	d.typ := TDString;
	d.stringdata := 'data';

	for i := 1 to 30 do
	begin
		str(i,k);
		d.stringdata := 'data' + k;
		k := 'test' + k;
		TreeInsert(t,k,d);

		if i >99 then
		begin
			writeln('root: ', t^.key^);
			PrintStringTree(t,1);
			writeln('------------');
			readln;
		end;
	end;

	writeln('root: ', t^.key^);
	PrintStringTree(t,1);
	writeln('------------');


	DoASearch(t,'test21');
	DoASearch(t,'test2');
	DoASearch(t,'test17');

	k := 'test17';
	TreeDelete(t,k);
	writeln('root: ', t^.key^);
	PrintStringTree(t,1);
	writeln('------------');
	DoASearch(t,'test17');



	TreeWalkStart(t, walkState);
	repeat
		TreeWalkNext(walkState, walkRes);
		if walkRes <> nil then
			writeln(walkRes^.data^.stringdata);
	until walkRes = nil;



	for i := 1 to 30 do
	begin
		str(i,k);
		k := 'test' + k;
		writeln('deleting ', k);
		TreeDelete(t,k);
	end;
	if t <> nil then
		writeln('root: ', t^.key^)
	else
		writeln('root: nil');
	PrintStringTree(t,1);
	writeln('------------');

	mem_dump;
end.
