function makeTreeNode(var d:TreeData;var key:string;nparent:TreeRef):TreeRef;
var newNode:TreeRef;
    newKey:^string;
begin
	new(newNode);
        { new(newKey,length(key)); }
	newString(newKey, length(key));
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
	(* writeln('RotateTreeRight at ', x^.key^); *)
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
	(* writeln('RotateTreeLeft at ', x^.key^); *)
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
				tmp := TreeLeftmost(root^.right);
				root^.key^ := tmp^.key^;
				root^.data^ := tmp^.data^;
				oldParent := tmp^.parent;
				if oldParent^.left = tmp then
					oldParent^.left := TreeDeleteFn(oldParent^.left, tmp^.key^)
				else
				if oldParent^.right = tmp then
					oldParent^.right := TreeDeleteFn(oldParent^.right, tmp^.key^)
				else
				begin
					writeln('TreeDelete internal error at', root^.key^);
				end;
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
						root := RotateTreeLeft(root)
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

function TreeSearch(root:TreeRef;var key:string):TreeDataRef;
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
var last,current,right:TreeRef;
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

procedure TreeWalkFirst(t:TreeRef; var state:TreeWalkState; var first:TreeRef);
begin
	TreeWalkStart(t, state);
	TreeWalkNext(state, first);
end;
