{
type TreedataType = (TDString, TDInteger);

type Treedata = record
        case typ:Treedatatype of 
        TDString:(stringdata:string);
        TDInteger:(intdata:integer);
    end;
}
type StringRef = ^string;

type TreeNode = record
        parent: ^TreeNode;
        left,right: ^TreeNode;
        height: integer;
	key: StringRef;
	data: ^Treedata;
    end;

type TreeRef = ^TreeNode;
     TreeDataRef = ^Treedata;

type TreeWalkState = record
		currentNode:TreeRef;
	end;
