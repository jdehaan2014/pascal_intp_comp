unit uCodeImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCode, uCodeFactory, fgl, AnyObject;

type

  //Attribute keys for an intermediate code node.

  TCodeKeys = (ckLine, ckID, ckValue);

  { TCodeKeyImpl }

  TICodeKeyImpl = class(Specialize TEnum<TCodeKeys>, ICodeKey)
  end;

Operator := (T1: TICodeKeyImpl.Values): TICodeKeyImpl;
Operator := (T1: TICodeKeyImpl.Values): ICodeKey;
operator := (T1: ICodeKey): TICodeKeyImpl.Values;
operator = (T1: ICodeKey; T2: TICodeKeyImpl.Values): boolean;
operator = (T1: TICodeKeyImpl.Values; T2: ICodeKey): boolean;

type

   //Node types of the intermediate code parse tree

  TCodeNodeTypes = (
    //program structures
    ctProgram, ctProcedure, ctFunction,
    //statements
    ctCompound, ctAssign, ctLoop, ctCall, ctParameters, ctIf, ctSelect,
    ctSelectBranch, ctSelectConstants, ctNoOp,
    //relational operators
    ctEQ, ctNE, ctLT, ctLE, ctGT, ctGE, ctNot,
    //additive operators
    ctAdd, ctSubtract, ctOr, ctNegate,
    //multiplicative operators
    ctMultiply, ctIntegerDivide, ctFloatDivide, ctMod, ctAnd,
    //operands
    ctVariable, ctSubscripts, ctField,
    ctIntegerConstant, ctRealConstant, ctStringConstant, ctBooleanConstant
  );

  { TCodeNodeTypeImpl }

  TICodeNodeTypeImpl = class(Specialize TEnum<TCodeNodeTypes>, ICodeNodeType)
  end;

Operator := (Value: TICodeNodeTypeImpl.Values): TICodeNodeTypeImpl;
Operator := (Value: TICodeNodeTypeImpl.Values): ICodeNodeType;
Operator := (Value: ICodeNodeType): TICodeNodeTypeImpl.Values;
Operator = (Value1: ICodeNodeType; Value2: TICodeNodeTypeImpl.Values): boolean;
operator = (Value1: TICodeNodeTypeImpl.Values; Value2: ICodeNodeType): boolean;

Type

  { TCodeImpl }

  //An implementation of the intermediate code as a parse tree.

  { TICodeImpl }

  TICodeImpl = class(TAnyObject, IIntermediateCode)
    private
      Root: ICodeNode;       // root node
    public
      Function setRoot(Node: ICodeNode): ICodeNode;  //Set and return the root node.
      Function getRoot: ICodeNode;        //Get the root node.
  end;

  { TNodeMap }

  TCodeNodeMap = Specialize TFPGMap<TICodeKeyImpl.Values, TObject>;

  //An implementation of a node of the intermediate code.

  { TICodeNodeImpl }

  TICodeNodeImpl = class(TCodeNodeMap, ICodeNode)
    private
      Typ: ICodeNodeType;                      // node type
      Parent: ICodeNode;                       // parent node
      Children: TICodeNodeList;                // children array
    public
      constructor Create(ATyp: ICodeNodeType);
      function GetObject: TObject;             //return instance of object
      Function getType: ICodeNodeType;         //return the node type.
      Function getParent: ICodeNode;           //Return the parent of this node.
      Function AddChild(Node: ICodeNode): ICodeNode;  //Add a child node.
      function getChildren: TICodeNodeList;  //Return an array list of this node's children.
      procedure setAttribute(Key: ICodeKey; Value: TObject);  //Set a node attribute.
      function getAttribute(Key: ICodeKey): TObject;  //Get the value of a node attribute.
      function CopyNode: ICodeNode;  //Make a copy of this node.
      function toString: string; override;
  end;

implementation

{ TCodeKeyImpl }

Operator := (T1: TICodeKeyImpl.Values): TICodeKeyImpl;
begin
  Result := TICodeKeyImpl.Create(T1);
end;

Operator := (T1: TICodeKeyImpl.Values): ICodeKey;
begin
  Result := TICodeKeyImpl.Create(T1);
end;

operator := (T1: ICodeKey): TICodeKeyImpl.Values;
var
  Arg: TICodeKeyImpl.Values;
begin
  ReadStr(T1.toString, Arg);
  Result := TICodeKeyImpl.Create(Arg);
end;

operator = (T1: ICodeKey; T2: TICodeKeyImpl.Values): boolean;
var
  Arg: TICodeKeyImpl.Values;
begin
  ReadStr(T1.toString, Arg);
  Result := Arg = T2;
end;

operator = (T1: TICodeKeyImpl.Values; T2: ICodeKey): boolean;
var
  Arg: TICodeKeyImpl.Values;
begin
  ReadStr(T2.toString, Arg);
  Result := Arg = T1;
end;



{ TCodeNodeTypeImpl }

operator := (Value: TICodeNodeTypeImpl.Values): TICodeNodeTypeImpl;
begin
  Result := TICodeNodeTypeImpl.Create(Value);
end;

operator := (Value: TICodeNodeTypeImpl.Values): ICodeNodeType;
begin
  Result := TICodeNodeTypeImpl.Create(Value);
end;

operator := (Value: ICodeNodeType): TICodeNodeTypeImpl.Values;
var
  Arg: TICodeNodeTypeImpl.Values;
begin
  ReadStr(Value.toString, Arg);
  Result := TICodeNodeTypeImpl.Create(Arg);
end;

operator = (Value1: ICodeNodeType; Value2: TICodeNodeTypeImpl.Values): boolean;
var
  Arg: TICodeNodeTypeImpl.Values;
begin
  ReadStr(Value1.toString, Arg);
  Result := Arg = Value2;
end;

operator = (Value1: TICodeNodeTypeImpl.Values; Value2: ICodeNodeType): boolean;
var
  Arg: TICodeNodeTypeImpl.Values;
begin
  ReadStr(Value2.toString, Arg);
  Result := Arg = Value1;
end;




{ TCodeNodeImpl }

constructor TICodeNodeImpl.Create(ATyp: ICodeNodeType);
begin
  Inherited Create;
  Typ := ATyp;
  Parent := Nil;
  Children := TICodeNodeList.Create;
end;

//return object intance
function TICodeNodeImpl.GetObject: TObject;
begin
  Result := Self;
end;

//return the node type.
function TICodeNodeImpl.getType: ICodeNodeType;
begin
  Result := Typ;
end;

//Return the parent of this node.
function TICodeNodeImpl.getParent: ICodeNode;
begin
  Result := Parent;
end;

//Add a child node.
function TICodeNodeImpl.AddChild(Node: ICodeNode): ICodeNode;
begin
  if Node <> Nil then begin
    Children.Add(Node);
    TICodeNodeImpl(Node.getObject).Parent := Self;
  end;
  Result := Node;
end;

//Return an array list of this node's children.
function TICodeNodeImpl.getChildren: TICodeNodeList;
begin
  Result := Children;
end;

//Set a node attribute.
procedure TICodeNodeImpl.setAttribute(Key: ICodeKey; Value: TObject);
begin
  //Add(TICodeKeyImpl(Key.getObject).Value, Value);
  KeyData[TICodeKeyImpl(Key.getObject).Value] := Value;
end;

//Get the value of a node attribute.
function TICodeNodeImpl.getAttribute(Key: ICodeKey): TObject;
var
  index: longint;
begin
  index := indexOf(TICodeKeyImpl(Key.getObject).Value);
  if index < 0 then
    Result := Nil
  else
    Result := KeyData[TICodeKeyImpl(Key.getObject).Value];
end;

//Make a copy of this node.
function TICodeNodeImpl.CopyNode: ICodeNode;
var
  NodeCopy: TICodeNodeImpl;
  i: integer;
begin
  //create a copy with the same type.
  NodeCopy := TICodeNodeImpl((TICodeFactory.CreateICodeNode(Typ)).getObject);

  // Iterate over the entries and append them to the copy.
  for i := 0 to Count-1 do
    NodeCopy.Add(Keys[i], Data[i]);

  Result := NodeCopy;
end;

function TICodeNodeImpl.toString: string;
begin
  Result := Typ.toString; //ICodeNodeType(Typ).toString;
end;

{ TCodeImpl }

function TICodeImpl.setRoot(Node: ICodeNode): ICodeNode;
begin
  Root := Node;
  Result := Root;
end;

function TICodeImpl.getRoot: ICodeNode;
begin
  Result := Root;
end;



end.

