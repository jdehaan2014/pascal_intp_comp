unit uExecutors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uSource,
  uMessages, uCode, uCodeImpl, uRuntimeErrorCode, uExecutor, uObjectUtils,
  uGenericInterfaces, uSymTabImpl, fgl, uPredefined, uMemoryFactory, uMemory,
  uMemoryImpl, uTypesImpl, uBackendFactory, uDefinitionImpl,
  uToken, uTokenType, uPascalTokenTyp,
  uBaseScanner;


type

  { TStatementExecutor }

  TStatementExecutor = class(TExecutor)
    public
      constructor Create(Parent: TExecutor);
      function Execute(Node: ICodeNode): TObject; //execute a statement
    protected
      function toPascal(TargetType: ITypeSpec; Value: TObject): TObject;
      function toObject(TargetType: ITypeSpec; Value: TObject): TObject;
      function CopyOf(Value: TObject; Node: ICodeNode): TObject;
      function CheckRange(Node: ICodeNode; Typ: ITypeSpec; Value: TOBject): TObject;
    private
      function CopyRecord(Value: TMemoryMap; Node: ICodeNode): TObject;
      function CopyArray(ValueCells: TCellList; Node: ICodeNode): TCellList;
      procedure sendSourceLineMessage(Node: ICodeNode);
    protected
      procedure sendAssignMessage(Node: ICodeNode; VariableName: string;
        Value: TObject);
      procedure sendFetchMessage(Node: ICodeNode; VariableName: string;
        Value: TObject);
      procedure sendCallMessage(Node: ICodeNode; RoutineName: string);
      procedure sendReturnMessage(Node: ICodeNode; RoutineName: string);
    private
      function getLineNumber(Node: ICodeNode): TObject;
  end;

  { TCompoundExecutor }

  TCompoundExecutor = class(TStatementExecutor)
    public
      constructor Create(Parent: TExecutor);
      function Execute(Node: ICodeNode): TObject;  //execute a compound statement
  end;

  { TAssignmentExecutor }

  TAssignmentExecutor = class(TStatementExecutor)
    public
      constructor Create(Parent: TExecutor);
      function Execute(Node: ICodeNode): TObject;  //execute a assignment statement
    protected
      procedure AssignValue(Node: ICodeNode; TargetID: ISymTabEntry;
        TargetCell: ICell; TargetType: ITypeSpec; Value: TObject; ValueType: ITypeSpec);
  end;

  { TExpressionExecutor }

  TExpressionExecutor = class(TStatementExecutor)
    public
      constructor Create(Parent: TExecutor);
      function Execute(Node: ICodeNode): TObject;  //execute a assignment statement
      function ExecuteVariable(Node: ICodeNode): ICell;
    private
      const
        ArithOps : TICodeNodeTypeSet = [
          ctAdd, ctSubtract, ctMultiply, ctFloatDivide, ctIntegerDivide, ctMod];
        RelOps : TICodeNodeTypeSet = [ctEQ,ctNE,ctLT,ctLE,ctGT,ctGE];
      function ExecuteValue(Node: ICodeNode): TObject;
      function ExecuteBinaryOperator(Node: ICodeNode;
        nodeType: TICodeNodeTypeImpl.Values): TObject;
  end;

  { TLoopExecutor }

  TLoopExecutor = class(TStatementExecutor)
    public
      constructor Create(Parent: TExecutor);
      function Execute(Node: ICodeNode): TObject;  //execute a loop statement
  end;

  { TIfExecutor }

  TIfExecutor = class(TStatementExecutor)
    public
      constructor Create(Parent: TExecutor);
      function Execute(Node: ICodeNode): TObject;  //execute an if statement
  end;

  { TSelectExecutor }

  TSelectExecutor = class(TStatementExecutor)
    private
      function SearchBranches(selectValue: TObject;
        selectChildren: TICodeNodeList): ICodeNode;
      function SearchConstants(selectValue: TObject; branchNode: ICodeNode
        ): boolean;
    public
      constructor Create(Parent: TExecutor);
      function Execute(Node: ICodeNode): TObject;  //execute a case statement
  end;

  { TCallExecutor }

  TCallExecutor = class(TStatementExecutor)
    public
      constructor Create(Parent: TExecutor);
      function Execute(Node: ICodeNode): TObject;  //execute a procedure/function call
  end;

  { TCallDeclaredExecutor }

  TCallDeclaredExecutor = class(TCallExecutor)
    public
      constructor Create(Parent: TExecutor);
      function Execute(Node: ICodeNode): TObject;
    private
      procedure ExecuteActualParms(ActualNodes: TICodeNodeList;
        FormalIds: TSymTabEntries; NewAR: IActivationRecord);
  end;

  { TCallStandardExecutor }

  TCallStandardExecutor = class(TCallExecutor)
  public
    constructor Create(Parent: TExecutor);
    destructor Destroy;
    function Execute(Node: ICodeNode): TObject;
  private
    ExpressionExecutor: TExpressionExecutor;
    function ExecuteReadReadln(CallNode: ICodeNode; RoutineCode: IRoutineCode):
      TObject;
    function ParseNumber(Token: TToken; Typ: ITypeSpec): TNumber;
    function ParseBoolean(Token: TToken): TBoolean;
    function ExecuteWriteWriteln(CallNode: ICodeNode; RoutineCode: IRoutineCode):
      TObject;
    function ExecuteEofEoln(CallNode: ICodeNode; RoutineCode: IRoutineCode)
      : TBoolean;
    function ExecuteAbsSqr(CallNode: ICodeNode; RoutineCode: IRoutineCode; ActualNode: ICodeNode)
      : TNumber;
    function ExecuteArctanCosExpLnSinSqrt(CallNode: ICodeNode; RoutineCode: IRoutineCode; ActualNode: ICodeNode)
      : TFloat;
    function ExecutePredSucc(CallNode: ICodeNode; RoutineCode: IRoutineCode; ActualNode: ICodeNode; Typ: ITypeSpec)
      : TInteger;
    function ExecuteChr(CallNode: ICodeNode; RoutineCode: IRoutineCode; ActualNode: ICodeNode)
      : TChar;
    function ExecuteOdd(CallNode: ICodeNode; RoutineCode: IRoutineCode; ActualNode: ICodeNode)
      : TBoolean;
    function ExecuteOrd(CallNode: ICodeNode; RoutineCode: IRoutineCode; ActualNode: ICodeNode)
      : TInteger;
    function ExecuteRoundTrunc(CallNode: ICodeNode; RoutineCode: IRoutineCode; ActualNode: ICodeNode)
      : TInteger;
  end;

implementation

{ TStatementExecutor }

constructor TStatementExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

function TStatementExecutor.Execute(Node: ICodeNode): TObject;
var
  NodeType: TICodeNodeTypeImpl.Values;
  CompoundExecutor: TCompoundExecutor;
  AssignmentExecutor: TAssignmentExecutor;
  LoopExecutor: TLoopExecutor;
  IfExecutor: TIfExecutor;
  SelectExecutor: TSelectExecutor;
  CallExecutor: TCallExecutor;
begin
  NodeType := TICodeNodeTypeImpl.toTyp(Node.getType);
  //send a message about the current source line
  sendSourceLineMessage(Node);
  case NodeType of
    ctCompound:
      begin
        CompoundExecutor := TCompoundExecutor.Create(Self);
        Result := CompoundExecutor.Execute(Node);
      end;
    ctAssign:
      begin
        AssignmentExecutor := TAssignmentExecutor.Create(Self);
        Result := AssignmentExecutor.Execute(Node);
      end;
    ctLoop:
      begin
        LoopExecutor := TLoopExecutor.Create(Self);
        Result := LoopExecutor.Execute(Node);
      end;
    ctIf:
      begin
        IfExecutor := TIfExecutor.Create(Self);
        Result := IfExecutor.Execute(Node);
      end;
    ctSelect:
      begin
        SelectExecutor := TSelectExecutor.Create(Self);
        Result := SelectExecutor.Execute(Node);
      end;
    ctCall:
      begin
        CallExecutor := TCallExecutor.Create(Self);
        Result := CallExecutor.Execute(Node);
      end;
    ctNoOp: Result := Nil;
    else begin
      ErrorHandler.Flag(Node, rcUNIMPLEMENTED_FEATURE, Self);
      Result := Nil;
    end;
  end;
end;

//convert an string Object to a Pascal string or character
function TStatementExecutor.toPascal(TargetType: ITypeSpec; Value: TObject)
  : TObject;
var
  AString: String;
  CharCells: TCellList;   // defined in uExecutor
  i: integer;
begin
  if Value is TString then begin
    AString := TString(Value).Value;
    if TargetType = TPredefined.charType then
      Result := TChar.Create(AString[1]) //Pascal character
    else if TargetType.isPascalString then begin
      CharCells := TCellList.Create;
      for i := 1 to Length(AString) do
        CharCells[i-1] :=                             //note: string starts at 1
          TMemoryFactory.CreateCell(TChar.Create(AString[i]));
      Result := CharCells;
    end
    else Result := Value;
  end
  else Result := Value;
end;

//convert Pascal string to a string Object
function TStatementExecutor.toObject(TargetType: ITypeSpec; Value: TObject)
  : TObject;
var
  AString: String;
  CharCells: TCellList;   // defined in uExecutor
  i: integer;
begin
  if (Value is TCellList) and
     (TCellList(Value)[0].getValue is TChar) then
  begin
    CharCells := TCellList(Value);
    setLength(AString, CharCells.Count);
    //build an Object of string
    for i := 1 to CharCells.Count do
      AString[i] := TChar(CharCells[i-1].getValue).charValue;
    Result := TString.Create(AString);
  end
  else
    Result := Value;
end;

//Return a copy of a Pascal value
function TStatementExecutor.CopyOf(Value: TObject; Node: ICodeNode): TObject;
var
  aCopy: TObject = Nil;
begin
  if Value is TInteger then
    aCopy := TInteger.Create(TInteger(Value).intValue)
  else if Value is TFloat then
    aCopy := TFloat.Create(TFloat(Value).floatValue)
  else if Value is TChar then
    aCopy := TChar.Create(TChar(Value).charValue)
  else if Value is TBoolean then
    aCopy := TBoolean.Create(TBoolean(Value).boolValue)
  else if Value is TString then
    aCopy := TString.Create(TString(Value).Value)
  else if Value is TMemoryMap then
    aCopy := CopyRecord(TMemoryMap(Value), Node)
  else
    aCopy := CopyArray(TCellList(Value), Node);

  Result := aCopy;
end;

//Runtime range check
function TStatementExecutor.CheckRange(Node: ICodeNode; Typ: ITypeSpec;
  Value: TOBject): TObject;
var
  minValue, maxValue: integer;
begin
  if Typ.getForm = tfSubRange then begin
    minValue := TInteger(Typ.getAttribute(tkSubRangeMinValue)).intValue;
    maxValue := TInteger(Typ.getAttribute(tkSubRangeMaxValue)).intValue;
    if TInteger(Value).intValue < minValue then begin
      ErrorHandler.Flag(Node, rcVALUE_RANGE, Self);
      Result := TInteger.Create(minValue);
    end
    else if TInteger(Value).intValue > maxValue then begin
      ErrorHandler.Flag(Node, rcVALUE_RANGE, Self);
      Result := TInteger.Create(maxValue);
    end
    else
      Result := Value;
  end
  else
    Result := Value;
end;

//Return a copy of a pascal record
function TStatementExecutor.CopyRecord(Value: TMemoryMap; Node: ICodeNode)
  : TObject;
var
  aCopy, Entries: TMemoryMap;
  NewKey: string;
  ValueCell: ICell;
  NewValue: TObject;
  i: integer;
begin
  aCopy := TMemoryMap.Create;
  if Value <> Nil then begin
    Entries := TMemoryMapImpl(Value);
    for i := 0 to Entries.Count-1 do begin
      NewKey := Entries.Keys[i];
      ValueCell := Entries.Data[i];
      NewValue := CopyOf(ValueCell.getValue, Node);
      aCopy.KeyData[NewKey] := TMemoryFactory.CreateCell(NewValue);
    end;
  end
  else
    ErrorHandler.Flag(Node, rcUNITIALIZED_VALUE, Self);
  Result := aCopy;
end;



//Return a copy of a pascal array
function TStatementExecutor.CopyArray(ValueCells: TCellList; Node: ICodeNode)
  : TCellList;
var
  Count: integer;
  ValueCell: ICell;
  i: integer;
  NewValue: TObject;
begin
  if ValueCells <> Nil then begin
    Count := ValueCells.Count;
    Result := TCellList.Create;
    for i := 0 to Count-1 do begin
      ValueCell := ValueCells[i];
      NewValue := CopyOf(ValueCell.getValue, Node);
      Result[i] := TMemoryFactory.CreateCell(NewValue);
    end;
  end
  else begin
    ErrorHandler.Flag(Node, rcUNITIALIZED_VALUE, Self);
    Result := TCellList.Create;
    Result.Add(Nil);
  end;
end;

//Send a message about the current source line.
procedure TStatementExecutor.sendSourceLineMessage(Node: ICodeNode);
var
  lineNumber: TObject;
begin
  lineNumber := Node.getAttribute(ckLine);
  if lineNumber <> Nil then
    sendMessage(TMessage.Create(mtSourceLine,
      TSourceLineMsg.Create('', TInteger(lineNumber).Value)));
end;

//Send a message about an assignment operation
procedure TStatementExecutor.sendAssignMessage(Node: ICodeNode;
  VariableName: string; Value: TObject);
var
  lineNumber: TObject;
begin
  lineNumber := getLineNumber(Node);
  if lineNumber <> Nil then
    sendMessage(TMessage.Create(mtAssign,
      TAssignMsg.Create(TInteger(lineNumber).Value, VariableName, Value)));
end;

//Send a message about a value fetch operation.
procedure TStatementExecutor.sendFetchMessage(Node: ICodeNode;
  VariableName: string; Value: TObject);
var
  lineNumber: TObject;
begin
  lineNumber := getLineNumber(Node);
  if lineNumber <> Nil then
    sendMessage(TMessage.Create(mtFetch,
      TFetchMsg.Create(TInteger(lineNumber).Value, VariableName, Value)));
end;

//Send a message about a call to a declared procedure or function
procedure TStatementExecutor.sendCallMessage(Node: ICodeNode; RoutineName: string);
var
  lineNumber: TObject;
begin
  lineNumber := getLineNumber(Node);
  if lineNumber <> Nil then
    sendMessage(TMessage.Create(mtCall,
      TCallMsg.Create(TInteger(lineNumber).Value, RoutineName)));
end;

//Send a message about a return from a declared procedure or function
procedure TStatementExecutor.sendReturnMessage(Node: ICodeNode; RoutineName: string);
var
  lineNumber: TObject;
begin
  lineNumber := getLineNumber(Node);
  if lineNumber <> Nil then
    sendMessage(TMessage.Create(mtReturn,
      TReturnMsg.Create(TInteger(lineNumber).Value, RoutineName)));
end;

//Get the source line number of a parse tree node.
function TStatementExecutor.getLineNumber(Node: ICodeNode): TObject;
var
  lineNumber: TObject = Nil;
begin
  //Go up the parent links to look for a line number
  if Node <> Nil then
    lineNumber := Node.getAttribute(ckLine);
  while (Node <> Nil) and (lineNumber = Nil) do begin
    Node := Node.getParent;
    if Node <> Nil then
      lineNumber := Node.getAttribute(ckLine);
  end;
  Result := lineNumber;
end;

{ TCompoundExecutor }

constructor TCompoundExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

function TCompoundExecutor.Execute(Node: ICodeNode): TObject;
var
  StatementExecutor: TStatementExecutor;
  Children: TICodeNodeList;
  child: ICodeNode;
begin
  //loop over the children of the COMPOUND node and execute each child
  StatementExecutor := TStatementExecutor.Create(Self);
  Children := Node.getChildren;
  for child in Children do
    StatementExecutor.Execute(child);
  Result := Nil;
end;


{ TAssignmentExecutor }

constructor TAssignmentExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

function TAssignmentExecutor.Execute(Node: ICodeNode): TObject;
var
  ExpressionExecutor: TExpressionExecutor;
  Children: TICodeNodeList;
  VariableNode, ExpressionNode: ICodeNode;
  Value: TObject;
  VariableID: ISymTabEntry;
  TargetCell: ICell;
  TargetType, ValueType: ITypeSpec;
begin
  //the ASSIGN node's children are the target variable end the expression
  Children := Node.getChildren;
  VariableNode := Children[0];
  ExpressionNode := Children[1];
  VariableID := VariableNode.getAttribute(ckID) as ISymTabEntry;
  //Execute the target variable to get its reference and
  //execute the expression to get its value.
  ExpressionExecutor := TExpressionExecutor.Create(Self);
  TargetCell := ExpressionExecutor.ExecuteVariable(VariableNode);
  TargetType := VariableNode.getTypeSpec;
  ValueType  := ExpressionNode.getTypeSpec.baseType;
  Value := ExpressionExecutor.Execute(ExpressionNode);

  AssignValue(Node, VariableID, TargetCell, TargetType, Value, ValueType);
  inc(ExecutionCount);
  result := Nil;
end;

//Assign a value to a target cell
procedure TAssignmentExecutor.AssignValue(Node: ICodeNode; TargetID: ISymTabEntry;
  TargetCell: ICell; TargetType: ITypeSpec; Value: TObject; ValueType: ITypeSpec);
var
  TargetLength, ValueLength, i: integer;
  StringValue: string;
begin
  //range check
  Value := CheckRange(Node, TargetType, Value);
  //Set the target's value. Convert an integer value to real if necessary
  if (TargetType = TPredefined.realType) and
     (ValueType = TPredefined.integerType) then
  begin
    TargetCell.setValue(TFloat.Create(TInteger(Value).intValue)) ;
  end
   // String assignment:
   //   target length < value length: truncate the value
   //   target length > value length: blank pad the value
  else if TargetType.isPascalString then
  begin
    TargetLength := TInteger(TargetType.getAttribute(tkArrayElementCount)).Value;
    ValueLength := TInteger(ValueType.getAttribute(tkArrayElementCount)).Value;
    StringValue := TSTring(Value).Value;
    // Truncate the value string.
    if TargetLength < ValueLength then
      StringValue := StringValue.Substring(0, TargetLength)
    // Pad the value string with blanks at the right end.
    else if TargetLength > ValueLength then
    begin
      setLength(StringValue, TargetLength);
      for i := ValueLength+1 to TargetLength do
        StringValue[i] := ' ';
    end;
    TargetCell.setValue(
      CopyOf(toPascal(TargetType, TSTring.Create(StringValue)), Node));
  end
  // Simple assignment.
  else begin
    TargetCell.setValue(copyOf(toPascal(TargetType, Value), Node));
  end;

  sendAssignMessage(Node, TargetId.getName, Value);
end;

{ TExpressionExecutor }

constructor TExpressionExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

//execute an exprssion
function TExpressionExecutor.Execute(Node: ICodeNode): TObject;
var
  Typ: ITypeSpec;
  nodeType: TICodeNodeTypeImpl.Values;
  Children: TICodeNodeList;
  ExpressionNode: ICodeNode;
  Value: TObject;
  intValue, NestingLevel: Integer;
  fltValue: Double;
  boolValue: Boolean;
  FunctionId: ISymTabEntry;
  RoutineCode: IRoutineCode;
  CallExecutor: TCallExecutor;
  FunctionName: string;
  AR: IActivationRecord;
  FunctionValueCell: ICell;
begin
  nodeType := TICodeNodeTypeImpl.toTyp(Node.getType);
  case nodeType of
    ctVariable: Result := ExecuteValue(Node); //Return the variable's value
    ctIntegerConstant:
      begin
        Typ := Node.getTypeSpec;
        Value := TInteger(Node.getAttribute(ckValue));
        if Typ = TPredefined.booleanType then
          Result := TBoolean.Create(TInteger(Value).intValue = 1)
        else
          Result := Value;
      end;
    ctRealConstant   : Result := TFloat(Node.getAttribute(ckValue));
    ctStringConstant : Result := TString(Node.getAttribute(ckValue));
    ctNegate:
      begin
        //get the NEGATE node's expression node child
        Children := Node.getChildren;
        ExpressionNode := Children[0];
        //execute the expression and return the negative of its value
        Value := Execute(ExpressionNode);
        if Value is TInteger then begin
          intValue := TInteger(Value).intValue;
          Result := TInteger.Create(-intValue);
        end
        else begin
          fltValue := TFloat(Value).floatValue;
          Result := TFloat.Create(-fltValue);
        end
      end;
    ctNot:
      begin
        //get the NOT node's expression node child
        Children := Node.getChildren;
        ExpressionNode := Children[0];
        //execute the expression and return the NOT of its value
        Value := Execute(ExpressionNode);
        boolValue := TBoolean(Value).boolValue;
        Result := TBoolean.Create(not boolValue);
      end;
    ctCall:
      begin
        // Execute a function call.
        FunctionId := Node.getAttribute(ckID) as ISymTabEntry;
        RoutineCode := TRoutineCodeImpl(FunctionId.getAttribute(skRoutineCode)).Value;
        CallExecutor := TCallExecutor.Create(Self);
        Value := CallExecutor.Execute(Node);
        //If it was a declared function, obtain the function value from its name
        if RoutineCode = rcDeclared then begin
          FunctionName := FunctionId.getName;
          NestingLevel := FunctionId.getSymTab.getNestingLevel;
          AR := RuntimeStack.getTopMost(NestingLevel);
          FunctionValueCell := AR.getCell(FunctionName);
          Value := FunctionValueCell.getValue;

          sendFetchMessage(Node, FunctionId.getName, Value);
        end;
        Result := Value;
      end;
    else begin
      Result := executeBinaryOperator(Node, nodeType);
    end;
  end;
end;

//return a variable's value
function TExpressionExecutor.ExecuteValue(Node: ICodeNode): TObject;
var
  VariableID: ISymTabEntry;
  VariableName: string;
  VariableType: ITypeSpec;
  VariableCell: ICell;
  Value: TObject;
begin
  VariableID := Node.getAttribute(ckID) as ISymTabEntry;
  VariableName := VariableID.getName;
  VariableType := VariableID.getTypeSpec;
  //Get the variable's value.
  VariableCell := ExecuteVariable(Node);
  Value := VariableCell.getValue;
  if Value <> Nil then begin
    Value := ToObject(VariableType, Value);
  end
  //Uninitialized value error: Use a default value.
  else begin
    ErrorHandler.Flag(Node, rcUNITIALIZED_VALUE, Self);

    Value := TBackendFactory.DefaultValue(VariableType);
    VariableCell.setValue(Value);
  end;
  sendFetchMessage(Node, VariableName, Value);
  result := Value;
end;

//execute a variable an return the reference to its cell
function TExpressionExecutor.ExecuteVariable(Node: ICodeNode): ICell;
var
  VariableID, FieldId: ISymTabEntry;
  VariableName, FieldName: string;
  VariableType, IndexType: ITypeSpec;
  VariableCell: ICell;
  NestinGLevel, minIndex, index: integer;
  AR: IActivationRecord;
  Modifiers, Subscripts: TICodeNodeList;
  Modifier, Subscript: ICodeNode;
  NodeType: ICodeNodeType;
  Value: TObject;
  Map: TMemoryMap;
begin
  VariableID := Node.getAttribute(ckID) as ISymTabEntry;
  VariableName := VariableID.getName;
  VariableType := VariableID.getTypeSpec;
  NestingLevel := VariableID.getSymTab.getNestingLevel;
  // Get the variable reference from the appropriate activation record.
  AR := RuntimeStack.getTopMost(NestingLevel);
  VariableCell := AR.getCell(VariableName);
  Modifiers := Node.getChildren;
  // Reference to a reference: Use the original reference.
  if VariableCell.getValue is ICell then
    VariableCell := VariableCell.getValue as ICell;
  // Execute any array subscripts or record fields.
  for Modifier in Modifiers do begin
    NodeType := Modifier.getType;
    //Subscripts
    if TICodeNodeTypeImpl.toTyp(NodeType) = ctSubscripts then begin
      Subscripts := Modifier.getChildren;
      // Compute a new reference for each subscript.
      for Subscript in Subscripts do begin
        IndexType := VariableType.getAttribute(tkArrayIndexType) as ITypeSpec;
        if IndexType.getForm = tfSubrange then
          minIndex := TInteger(IndexType.getAttribute(tkSubRangeMinValue)).Value
        else
          minIndex := 0;
        Value := TInteger(Execute(Subscript));
        Value := TInteger(CheckRange(Node, IndexType, Value));
        index := TInteger(Value).intValue - minIndex;
        VariableCell := TCellList(VariableCell.getValue)[index];
        VariableType := VariableType.getAttribute(tkArrayElementType) as ITypeSpec;
      end;
    end
    // Field.
    else if TICodeNodeTypeImpl.toTyp(NodeType) = ctField then begin
      FieldID := Modifier.getAttribute(ckID) as ISymTabEntry;
      FieldName := FieldID.getName;
      // Compute a new reference for the field
      Map := TMemoryMap(VariableCell.getValue);
      VariableCell := Map.KeyData[FieldName];
      VariableType := FieldID.getTypeSpec;
    end;
  end;
  Result := VariableCell;
end;

//set of arithatic operator node types
function TExpressionExecutor.ExecuteBinaryOperator(Node: ICodeNode;
  nodeType: TICodeNodeTypeImpl.Values): TObject;
var
  Children: TICodeNodeList;
  operandNode1, operandNode2: ICodeNode;
  operand1, operand2: TObject;
  IntegerMode: boolean = False;
  CharacterMode: boolean = False;
  StringMode: boolean = False;
  intValue1, intValue2, comp: integer;
  fltValue1, fltValue2: double;
  boolValue1, boolvalue2: boolean;
  charValue1, charValue2: char;
  strValue1, strValue2: string;

begin
  Result := Nil;  //should never happen
  //get the 2 operand children of the operator node
  Children := Node.getChildren;
  operandNode1 := Children[0];
  operandNode2 := Children[1];
  //operands
  operand1 := Execute(operandNode1);
  operand2 := Execute(operandNode2);

  if (operand1 is TInteger) and (operand2 is TInteger) then
    IntegerMode := True
  else if ((operand1 is TChar) or
           ((operand1 is TString) and (TString(operand1).Length = 1))
          ) and
          ((operand2 is TChar) or
           ((operand2 is TString) and (TString(operand2).Length = 1))
          ) then
    CharacterMode := True
  else if (operand1 is TString) and (operand2 is TString) then
    StringMode := True;

  //arithmetic operators
  if nodeType in ArithOps then begin
    if IntegerMode then begin
      intValue1 := TInteger(operand1).Value;
      intValue2 := TInteger(operand2).Value;
      //integer operations
      case nodeType of
        ctAdd:      Result := TInteger.Create(intValue1+intValue2);
        ctSubtract: Result := TInteger.Create(intValue1-intValue2);
        ctMultiply: Result := TInteger.Create(intValue1*intValue2);
        ctFloatDivide:
          begin
            //check for division by zero
            if intValue2 <> 0 then
              Result := TFloat.Create(intValue1/intValue2)
            else begin
              errorHandler.Flag(Node, rcDIVISION_BY_ZERO, Self);
              Result := TFloat.Create(0.0);
            end;
          end;
        ctIntegerDivide:
          begin
            //check for division by zero
            if intValue2 <> 0 then
              Result := TInteger.Create(intValue1 div intValue2)
            else begin
              errorHandler.Flag(Node, rcDIVISION_BY_ZERO, Self);
              Result := TInteger.Create(0);
            end;
          end;
        ctMod:
          begin
            //check for division by zero
            if intValue2 <> 0 then
              Result := TInteger.Create(intValue1 mod intValue2)
            else begin
              errorHandler.Flag(Node, rcDIVISION_BY_ZERO, Self);
              Result := TInteger.Create(0);
            end;
          end;
      end;
    end
    else begin //not integerMode
      if operand1 is TInteger then
        fltValue1 := TInteger(operand1).Value
      else
        fltValue1 := TFloat(operand1).Value;
      if operand2 is TInteger then
        fltValue2 := TInteger(operand2).Value
      else
        fltValue2 := TFloat(operand2).Value;
      case nodeType of
        ctAdd:      Result := TFloat.Create(fltValue1+fltValue2);
        ctSubtract: Result := TFloat.Create(fltValue1-fltValue2);
        ctMultiply: Result := TFloat.Create(fltValue1*fltValue2);
        ctFloatDivide:
          begin
            //check for division by zero
            if fltValue2 <> 0.0 then
              Result := TFloat.Create(fltValue1/fltValue2)
            else begin
              errorHandler.Flag(Node, rcDIVISION_BY_ZERO, Self);
              Result := TFloat.Create(0.0);
            end;
          end;
      end;
    end;
  end
  else if nodeType in [ctAnd, ctOr] then begin        // AND and OR
    boolValue1 := TBoolean(operand1).Value;
    boolvalue2 := TBoolean(operand2).Value;
    case nodeType of
      ctAnd: Result := TBoolean.Create(boolValue1 and boolvalue2);
      ctOr:  Result := TBoolean.Create(boolValue1 or  boolvalue2);
    end;
  end
  else if IntegerMode then begin           //relational operators
    intValue1 := TInteger(operand1).Value;
    intValue2 := TInteger(operand2).Value;
    case nodeType of     // integer operands
      ctEQ: Result := TBoolean.Create(intValue1 =  intValue2);
      ctNE: Result := TBoolean.Create(intValue1 <> intValue2);
      ctLT: Result := TBoolean.Create(intValue1 <  intValue2);
      ctLE: Result := TBoolean.Create(intValue1 <= intValue2);
      ctGT: Result := TBoolean.Create(intValue1 >  intValue2);
      ctGE: Result := TBoolean.Create(intValue1 >= intValue2);
    end;
  end
  else if CharacterMode then begin
    if operand1 is TChar then
      charValue1 := TChar(operand1).Value
    else
      charValue1 := TString(operand1).CharAt(1);

    if operand2 is TChar then
      charValue2 := TChar(operand2).Value
    else
      charValue2 := TString(operand2).CharAt(1);

    case nodeType of     // char operands
      ctEQ: Result := TBoolean.Create(charValue1 =  charValue2);
      ctNE: Result := TBoolean.Create(charValue1 <> charValue2);
      ctLT: Result := TBoolean.Create(charValue1 <  charValue2);
      ctLE: Result := TBoolean.Create(charValue1 <= charValue2);
      ctGT: Result := TBoolean.Create(charValue1 >  charValue2);
      ctGE: Result := TBoolean.Create(charValue1 >= charValue2);
    end;
  end
  else if StringMode then begin
    strValue1 := TString(operand1).Value;
    strValue2 := TString(operand2).Value;
    comp := strValue1.CompareTo(strValue2);

    case nodeType of     // char operands
      ctEQ: Result := TBoolean.Create(comp = 0);
      ctNE: Result := TBoolean.Create(comp <> 0);
      ctLT: Result := TBoolean.Create(comp < 0);
      ctLE: Result := TBoolean.Create(comp <= 0);
      ctGT: Result := TBoolean.Create(comp > 0);
      ctGE: Result := TBoolean.Create(comp >= 0);
    end;
  end
  else begin
    if operand1 is TInteger then
      fltValue1 := TInteger(operand1).Value
    else
      fltValue1 := TFloat(operand1).Value;
    if operand2 is TInteger then
      fltValue2 := TInteger(operand2).Value
    else
      fltValue2 := TFloat(operand2).Value;
    case nodeType of     // integer operands
      ctEQ: Result := TBoolean.Create(fltValue1 =  fltValue2);
      ctNE: Result := TBoolean.Create(fltValue1 <> fltValue2);
      ctLT: Result := TBoolean.Create(fltValue1 <  fltValue2);
      ctLE: Result := TBoolean.Create(fltValue1 <= fltValue2);
      ctGT: Result := TBoolean.Create(fltValue1 >  fltValue2);
      ctGE: Result := TBoolean.Create(fltValue1 >= fltValue2);
    end;
  end
end;

{ TLoopExecutor }

constructor TLoopExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

function TLoopExecutor.Execute(Node: ICodeNode): TObject;
var
  exitLoop: boolean = False;
  exprNode: ICodeNode = Nil;
  Child: ICodeNode;
  loopChildren: TICodeNodeList;
  ExpressionExecutor: TExpressionExecutor;
  StatementExecutor: TStatementExecutor;
  childTyp: TICodeNodeTypeImpl.Values;
begin
  loopChildren := Node.getChildren;
  ExpressionExecutor := TExpressionExecutor.Create(Self);
  StatementExecutor := TStatementExecutor.Create(Self);
  // Loop until the TEST expression value is True
  while not exitLoop do begin
    Inc(ExecutionCount);   // count the loop statement itself
    //Execute the children of the LOOP node
    for Child in loopChildren do begin
      childTyp := TICodeNodeTypeImpl.toTyp(Child.getType);
      // TEST node?
      if childTyp = ctTest then begin
        if exprNode = Nil then
          exprNode := Child.getChildren[0];
        exitLoop := TBoolean(ExpressionExecutor.Execute(exprNode)).Value;
      end
      //Statement node
      else
        StatementExecutor.Execute(Child);
      //Exit if the TEST expression value is True
      if exitLoop then
        Break;
    end;
  end;
  Result := Nil;
end;

{ TIfExecutor }

constructor TIfExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

//execute an IF statement
function TIfExecutor.Execute(Node: ICodeNode): TObject;
var
  b: boolean = False;
  exprNode, thenStmtNode, elseStmtNode: ICodeNode;
  Children: TICodeNodeList;
  ExpressionExecutor: TExpressionExecutor;
  StatementExecutor: TStatementExecutor;
begin
  //get the IF node's children
  Children := Node.getChildren;
  exprNode := Children[0];
  thenStmtNode := Children[1];
  if Children.Count > 2 then
    elseStmtNode := Children[2]
  else
    elseStmtNode := Nil;
  ExpressionExecutor := TExpressionExecutor.Create(Self);
  StatementExecutor := TStatementExecutor.Create(Self);
  //Evaluate the expr to determine which statement to execute
  b := TBoolean(ExpressionExecutor.Execute(exprNode)).Value;
  if b then
    StatementExecutor.Execute(thenStmtNode)
  else if elseStmtNode <> Nil then
    StatementExecutor.Execute(elseStmtNode);
  Inc(ExecutionCount);    // count the IF statement itself
  Result := Nil;
end;

{ TSelectExecutor }

constructor TSelectExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

function TSelectExecutor.Execute(Node: ICodeNode): TObject;
var
  selectChildren: TICodeNodeList;
  exprNode, statementNode, selectedBranchNode: ICodeNode;
  selectValue: TObject;
  ExpressionExecutor: TExpressionExecutor;
  StatementExecutor: TStatementExecutor;
begin
  //Get the SELECT node's children
  selectChildren := Node.getChildren;
  exprNode := selectChildren[0];
  //Evaluate the SELECT expression.
  ExpressionExecutor := TExpressionExecutor.Create(Self);
  selectValue := ExpressionExecutor.Execute(exprNode);
  //Attempt to select a SELECT branch
  selectedBranchNode := SearchBranches(selectValue, selectChildren);
  //If there was a selection, execute the SELECT branch statement
  if selectedBranchNode <> Nil then begin
    statementNode := selectedBranchNode.getChildren[1];
    StatementExecutor := TStatementExecutor.Create(Self);
    StatementExecutor.Execute(statementNode);
  end;
  Inc(ExecutionCount);  // count the SELECT statement itself
  Result := Nil;
end;

function TSelectExecutor.SearchBranches(
  selectValue: TObject; selectChildren: TICodeNodeList): ICodeNode;
var
  i: integer;
  branchNode: ICodeNode;
begin
  Result := Nil;
  //Loop over the SELECT_BRANCHes to find a match
  for i := 1 to selectChildren.Count - 1 do begin  //start at 1 as 0 is the expr
    branchNode := selectChildren[i];
    if SearchConstants(selectValue, branchNode) then begin
      Result := branchNode;
      Break;
    end;
  end;
end;

function TSelectExecutor.SearchConstants(
  selectValue: TObject; branchNode: ICodeNode): boolean;
var
  constantsNode, constNode: ICodeNode;
  constantsList: TICodeNodeList;
  intConstant: integer;
  strConstant, chrConstant: string;
begin
  //Get the list of SELECT_CONSTANTS values
  constantsNode := branchNode.getChildren[0];
  constantsList := constantsNode.getChildren;
  //Search the list of constants
  if selectValue is TInteger then
    for constNode in constantsList do begin
      intConstant := TInteger(constNode.getAttribute(ckValue)).Value;
      if TInteger(selectValue).Value = intConstant then begin
        Result := True; //match
        Break;
      end;
    end
  else begin
    if selectValue is TChar then
      chrConstant := TChar(selectValue).Value
    else
      chrConstant := TString(selectValue).Value;
    for constNode in constantsList do begin
      strConstant := TString(constNode.getAttribute(ckValue)).Value;
      if chrConstant = strConstant then begin
        Result := True; //match
        Break;
      end;
    end;
  end;
end;

{ TCallExecutor }

constructor TCallExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

function TCallExecutor.Execute(Node: ICodeNode): TObject;
var
  RoutineID: ISymTabEntry;
  RoutineCode: IRoutineCode;
  CallDeclaredExecutor: TCallDeclaredExecutor;
  CallStandardExecutor: TCallStandardExecutor;
begin
  //RoutineID := TSymTabEntryImpl(Node.getAttribute(ckID));
  RoutineID := Node.getAttribute(ckID) as ISymTabEntry;
  RoutineCode := TRoutineCodeImpl(RoutineID.getAttribute(skRoutineCode)).Value;
  if RoutineCode = rcDeclared then begin
    CallDeclaredExecutor := TCallDeclaredExecutor.Create(Self);
    Result := CallDeclaredExecutor.Execute(Node);
  end
  else begin
    CallStandardExecutor := TCallStandardExecutor.Create(Self);
    Result := CallStandardExecutor.Execute(Node);
  end;
  inc(ExecutionCount);  //count the call statement
end;

{ TCallDeclaredExecutor }

constructor TCallDeclaredExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

//Execute a call to a declared procedure or function.
function TCallDeclaredExecutor.Execute(Node: ICodeNode): TObject;
var
  RoutineID: ISymTabEntry;
  newAR: IActivationRecord;
  ParmsNode: ICodeNode;
  ActualNodes: TICodeNodeList;
  FormalIds: TSymTabEntries;
  aICode: IIntermediateCode;
  RootNode: ICodeNode;
  StatementExecutor: TStatementExecutor;
  Value: TObject;
begin
  RoutineID := Node.getAttribute(ckID) as ISymTabEntry;
  newAR := TMemoryFactory.CreateActivationRecord(RoutineId);  //hier gaat fout
  // Execute any actual parameters and initialize
  // the formal parameters in the new activation record.
  if Node.getChildren.Count > 0 then begin
    ParmsNode := Node.getChildren[0];
    ActualNodes := ParmsNode.getChildren;
    FormalIds := TSymTabEntries(RoutineId.getAttribute(skRoutineParms));
    ExecuteActualParms(ActualNodes, FormalIds, newAR);
  end;
  // Push the new activation record.
  RuntimeStack.Push(newAR);

  sendCallMessage(Node, RoutineId.getName);
  // Get the root node of the routine's intermediate code.
  aICode := (RoutineId.getAttribute(skRoutineICode)) as IIntermediateCode;
  RootNode := aICode.getRoot;
  // Execute the routine.
  StatementExecutor := TStatementExecutor.Create(Self);
  Value := StatementExecutor.Execute(RootNode);
  // Pop off the activation record.
  RuntimeStack.Pop;

  sendReturnMessage(Node, RoutineId.getName);
  Result := Value;
end;

//Execute the actual parameters of a call.
procedure TCallDeclaredExecutor.ExecuteActualParms(ActualNodes: TICodeNodeList;
  FormalIds: TSymTabEntries; NewAR: IActivationRecord);
var
  ExpressionExecutor: TExpressionExecutor;
  AssignmentExecutor: TAssignmentExecutor;
  i: Integer;
  FormalID: ISymTabEntry;
  FormalDefn: IDefinition;
  FormalCell, ActualCell: ICell;
  ActualNode: ICodeNode;
  FormalType, ValueType: ITypeSpec;
  Value: TObject;
begin
  ExpressionExecutor := TExpressionExecutor.Create(Self);
  AssignmentExecutor := TAssignmentExecutor.Create(Self);
  for i := 0 to FormalIDs.Count-1 do begin
    FormalID := FormalIds[i];
    FormalDefn := FormalID.getDefinition;
    FormalCell := NewAR.getCell(FormalID.getName);
    ActualNode := ActualNodes[i];
    //Value parameter
    if FormalDefn = defValue_Parm then begin
      FormalType := FormalID.getTypeSpec;
      ValueType := ActualNode.getTypeSpec.baseType;
      Value := ExpressionExecutor.Execute(ActualNode);
      AssignmentExecutor.AssignValue(ActualNode, FormalId, FormalCell,
        FormalType, Value, ValueType);
    end
    //VAR parameter
    else begin
      ActualCell := ExpressionExecutor.ExecuteVariable(ActualNode);
      FormalCell.setValue(ActualCell.getObject);
    end;
  end;
end;


{ TCallStandardExecutor }

constructor TCallStandardExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
  ExpressionExecutor := TExpressionExecutor.Create(Self);
end;

destructor TCallStandardExecutor.Destroy;
begin
  ExpressionExecutor.Free;
  Inherited;
end;

function TCallStandardExecutor.Execute(Node: ICodeNode): TObject;
var
  RoutineID: ISymTabEntry;
  RoutineCode: IRoutineCode;
  Typ: ITypeSpec;
  ActualNode, ParmsNode: ICodeNode;
begin
  //RoutineID := TSymTabEntryImpl(Node.getAttribute(ckID));
  RoutineID := Node.getAttribute(ckID) as ISymTabEntry;
  RoutineCode := TRoutineCodeImpl(RoutineId.getAttribute(skRoutineCode)).Value;
  Typ := Node.getTypeSpec;
  ActualNode := Nil;

  // Get the actual parameters of the call.
  if Node.getChildren.Count > 0 then begin
    ParmsNode := Node.getChildren[0];
    ActualNode := ParmsNode.getChildren[0];
  end;
  case TRoutineCodeImpl.toTyp(RoutineCode) of
    rcRead, rcReadLn: Result := ExecuteReadReadln(Node, RoutineCode);
    rcWrite, rcWriteLn: Result := ExecuteWriteWriteln(Node, RoutineCode);
    rcEof, rcEoln: Result := ExecuteEofEoln(Node, RoutineCode);
    rcAbs, rcSqr: Result := ExecuteAbsSqr(Node, RoutineCode, ActualNode);
    rcArctan, rcCos, rcExp, rcLn, rcSin, rcSqrt:
      Result := ExecuteArctanCosExpLnSinSqrt(Node, RoutineCode, ActualNode);
    rcPred, rcSucc: Result := ExecutePredSucc(Node, RoutineCode, ActualNode, Typ);
    rcChr: Result := ExecuteChr(Node, RoutineCode, ActualNode);
    rcOdd: Result := ExecuteOdd(Node, RoutineCode, ActualNode);
    rcOrd: Result := ExecuteOrd(Node, RoutineCode, ActualNode);
    rcRound, rcTrunc: Result := ExecuteRoundTrunc(Node, RoutineCode, ActualNode);
    else Result := Nil;
  end;
end;

function TCallStandardExecutor.ExecuteReadReadln(CallNode: ICodeNode;
  RoutineCode: IRoutineCode): TObject;
var
  ActualNode, ParmsNode: ICodeNode;
  Typ, baseType: ITypeSpec;
  Actuals: TICodeNodeList;
  VariableCell: ICell;
  Value: TObject;
  ch: char;
  Token: TToken;
  ActualID: ISymTabEntry;
begin
  if CallNode.getChildren.Count > 0 then
    ParmsNode := CallNode.getChildren[0]
  else
    ParmsNode := Nil;
  if ParmsNode <> Nil then begin
    Actuals := ParmsNode.getChildren;
    // Loop to process each actual parameter.
    for ActualNode in Actuals do begin
      Typ := ActualNode.getTypeSpec;
      baseType := Typ.baseType;
      VariableCell := ExpressionExecutor.ExecuteVariable(ActualNode);
      try
        if baseType = TPredefined.integerType then begin
          Token := StandardIn.NextToken;
          Value := TInteger(ParseNumber(Token, BaseType));
        end
        else if baseType = TPredefined.realType then begin
          Token := StandardIn.NextToken;
          Value := TFloat(ParseNumber(Token, BaseType));
        end
        else if baseType = TPredefined.booleanType then begin
          Token := StandardIn.NextToken;
          Value := ParseBoolean(Token);
        end
        else if baseType = TPredefined.charType then begin
          ch := StandardIn.NextChar;
          if (ch = LineEnding) or (ch = FileEnding) then
            ch := ' ';
          Value := TChar.Create(ch);
        end
        else raise Exception.Create('');
      except
        on E: Exception do begin
          Errorhandler.Flag(CallNode, rcINVALID_INPUT, Self);
          if Typ = TPredefined.realType then
            Value := TFloat.Create(0.0)
          else if Typ = TPredefined.charType then
            Value := TChar.Create(' ')
          else if Typ = TPredefined.booleanType then
            Value := TBoolean.Create(False)
          else
            Value := TInteger.Create(0);
        end;
      end;
      //range check and set the value
      Value := CheckRange(CallNode, Typ, Value);
      VariableCell.setValue(Value);
      //ActualID := TSymTabEntryImpl(ActualNode.getAttribute(ckID));
      ActualID := ActualNode.getAttribute(ckID) as ISymTabEntry;
      SendAssignMessage(CallNode, ActualID.getName, Value);
    end;
  end;
  //skip the rest of the input line for readln
  if RoutineCode = rcReadLn then begin
    try
      StandardIn.SkipToNextLine;
    except
      on E: Exception do
        Errorhandler.Flag(CallNode, rcINVALID_INPUT, Self);
    end;
  end;
  Result := Nil;
end;

//Parse an integer or real value from the standard input.
function TCallStandardExecutor.ParseNumber(Token: TToken; Typ: ITypeSpec)
  : TNumber;
var
  TokenTyp, Sign: ITokenTyp;
  Value: TNumber;
begin
  TokenTyp := Token.getTyp;
  Sign := ttNone;
  //leading sign?
  if (TokenTyp = ttPlus) or (TokenTyp = ttMinus) then begin
    Sign := TokenTyp;
    Token := StandardIn.NextToken;
    TokenTyp := Token.getTyp;
  end;
  //integer value
  if TokenTyp = ttInteger then begin
    if Sign = ttMinus then
      Value := TInteger.ValueOf( -(TInteger(Token.getValue).Value))
    else Value := TInteger(Token.getValue);
    if Typ = TPredefined.integerType then
      Result := Value
    else Result := TFloat.Create(TInteger(Value).intValue);
  end
  //real value
  else if TokenTyp = ttReal then begin
    if Sign = ttMinus then
      Value := TFloat.ValueOf( -(TFloat(Token.getValue).Value))
    else Value := TFloat(Token.getValue);
    if Typ = TPredefined.realType then
      Result := Value
    else Result := TInteger.Create(TFloat(Value).intValue);
  end
  //bad input
  else raise Exception.Create('Wrong input');
end;

//Parse a boolean value from the standard input.
function TCallStandardExecutor.ParseBoolean(Token: TToken): TBoolean;
var
  BoolText: String;
begin
  if Token.getTyp = ttIdentifier then begin
    BoolText := Token.getText;
    if BoolText.ToLower = 'true' then
      Result := TBoolean.Create(True)
    else if BoolText.ToLower = 'false' then
      Result := TBoolean.Create(False)
    else raise Exception.Create('Boolean value expected');
  end
  else raise Exception.Create('Boolean value expected');
end;

//Execute a call to write or writeln.
function TCallStandardExecutor.ExecuteWriteWriteln(CallNode: ICodeNode;
  RoutineCode: IRoutineCode): TObject;
var
  ParmsNode, WriteParmNode, ExprNode: ICodeNode;
  Actuals, Children: TICodeNodeList;
  DataType: ITypeSpec;
  Value: TObject;
  TypeCode, JavaFormat: string;
  w, p: integer;
begin
  if CallNode.getChildren.Count > 0 then
    ParmsNode := CallNode.getChildren[0]
  else
    ParmsNode := Nil;
  if ParmsNode <> Nil then begin
    Actuals := ParmsNode.getChildren;
    //Loop to process each WRITE_PARM actual parameter node.
    for WriteParmNode in Actuals do begin
      Children := WriteParmNode.getChildren;
      ExprNode := Children[0];
      DataType := ExprNode.getTypeSpec.baseType;
      if DataType.isPascalString then TypeCode := 's'
      else if DataType = TPredefined.integerType then TypeCode := 'd'
      else if DataType = TPredefined.realType then TypeCode := 'f'
      else if DataType = TPredefined.booleanType then TypeCode := 'b'
      else if DataType = TPredefined.charType then TypeCode := 'c'
      else TypeCode := 's';
      Value := ExpressionExecutor.Execute(ExprNode);
      if (DataType = TPredefined.charType) and (Value is TString) then
        Value := TChar.Create(TString(Value).CharAt(1));
      JavaFormat := '%';
      //process any field width and precision values
      if Children.Count > 1 then begin
        w := TInteger(Children[1].getAttribute(ckValue));
        if w = 0 then JavaFormat += '1'
        else JavaFormat += IntToStr(w);
      end;
      if Children.Count > 2 then begin
        p := TInteger(Children[2].getAttribute(ckValue));
        JavaFormat += '.';
        if p = 0 then JavaFormat += '1'
        else JavaFormat += IntToStr(p);
      end;
      //write the formatted value to the standard output
      case TypeCode of
        'b': Write(Format(JavaFormat+'s', [TBoolean(Value).toString]));
        'c': Write(Format(JavaFormat+'s', [TChar(Value).Value]));
        'd': Write(Format(JavaFormat+'d', [TInteger(Value).intValue]));
        'f': Write(Format(JavaFormat+'f', [TFloat(Value).floatValue]));
        's': Write(Format(JavaFormat+'s', [TString(Value).Value]));
      end;
    end;
  end;
  //line feed for writeln;
  if RoutineCode = rcWriteLn then
    Writeln;
  Result := Nil;
end;

//execute a call to eof or eoln
function TCallStandardExecutor.ExecuteEofEoln(CallNode: ICodeNode; RoutineCode: IRoutineCode)
  : TBoolean;
begin
  Try
    if RoutineCode = rcEOF then
      Result := StandardIn.atEof
    else
      Result := StandardIn.atEol;
  Except
    on E: Exception do begin
      Errorhandler.Flag(CallNode, rcINVALID_INPUT, Self);
      Result := True;
    end;
  end;
end;

//execute a call to abs or sqr
function TCallStandardExecutor.ExecuteAbsSqr(CallNode: ICodeNode;
  RoutineCode: IRoutineCode; ActualNode: ICodeNode): TNumber;
var
  ArgValue: TObject;
  intValue: integer;
  fltValue: real;
begin
  ArgValue := ExpressionExecutor.Execute(ActualNode);
  if ArgValue is TInteger then begin
    intValue := TInteger(ArgValue);
    if RoutineCode = rcABS then
      Result := TInteger.Create(Abs(intValue))
    else Result := TInteger.Create(intValue * intValue); //rcSQR
  end
  else begin
    fltValue := TFloat(ArgValue);
    if RoutineCode = rcABS then
      Result := TFloat.Create(Abs(fltValue))
    else Result := TFloat.Create(fltValue * fltValue); //rcSQR
  end;
end;

function TCallStandardExecutor.ExecuteArctanCosExpLnSinSqrt(CallNode: ICodeNode;
  RoutineCode: IRoutineCode; ActualNode: ICodeNode)
  : TFloat;
var
  ArgValue: TObject;
  fltValue: real;
begin
  ArgValue := ExpressionExecutor.Execute(ActualNode);
  if ArgValue is TInteger then
    fltValue := TInteger(ArgValue).Value
  else fltValue := TFloat(ArgValue);
  case TRoutineCodeImpl.toTyp(RoutineCode) of
    rcARCTAN: Result := Arctan(fltValue);
    rcCOS: Result := Cos(fltValue);
    rcEXP: Result := Exp(fltValue);
    rcSIN: Result := Sin(fltValue);
    rcLN :
      if fltValue > 0.0 then
        Result := Ln(fltValue)
      else begin
        Errorhandler.Flag(CallNode, rcINVALID_STANDARD_FUNCTION_ARGUMENT, Self);
        Result := 0.0;
      end;
    rcSQRT:
      if fltValue > 0.0 then
        Result := Sqrt(fltValue)
      else begin
        Errorhandler.Flag(CallNode, rcINVALID_STANDARD_FUNCTION_ARGUMENT, Self);
        Result := 0.0;
      end;
    else Result := 0.0;
  end;
end;

function TCallStandardExecutor.ExecutePredSucc(CallNode: ICodeNode;
  RoutineCode: IRoutineCode; ActualNode: ICodeNode; Typ: ITypeSpec): TInteger;
var
  intValue, newValue: integer;
begin
  intValue := TInteger(ExpressionExecutor.Execute(ActualNode));
  if RoutineCode = rcPRED then
    newValue := intValue - 1
  else newValue := intValue + 1;
  newValue := TInteger(CheckRange(CallNode, Typ, TInteger.ValueOf(newValue)));
  Result := newValue;
end;

function TCallStandardExecutor.ExecuteChr(CallNode: ICodeNode;
  RoutineCode: IRoutineCode; ActualNode: ICodeNode): TChar;
var
  intValue: integer;
begin
  intValue := TInteger(ExpressionExecutor.Execute(ActualNode));
  Result := Chr(intValue);
end;

function TCallStandardExecutor.ExecuteOdd(CallNode: ICodeNode;
  RoutineCode: IRoutineCode; ActualNode: ICodeNode): TBoolean;
var
  intValue: integer;
begin
  intValue := TInteger(ExpressionExecutor.Execute(ActualNode));
  Result := Odd(intValue);
end;

function TCallStandardExecutor.ExecuteOrd(CallNode: ICodeNode;
  RoutineCode: IRoutineCode; ActualNode: ICodeNode): TInteger;
var
  Value: TObject;
begin
  Value := ExpressionExecutor.Execute(ActualNode);
  if Value is TChar then
    Result := Ord(TChar(Value).charValue)
  else if Value is TString then
    Result := Ord(TString(Value).CharAt(1))
  else
    Result := TInteger(Value);
end;

function TCallStandardExecutor.ExecuteRoundTrunc(CallNode: ICodeNode;
  RoutineCode: IRoutineCode; ActualNode: ICodeNode): TInteger;
var
  fltValue: real;
begin
  fltValue := TFloat(ExpressionExecutor.Execute(ActualNode));
  if RoutineCode = rcROUND then
    Result := Round(fltValue)
  else
    Result := Trunc(fltValue);
end;



end.

