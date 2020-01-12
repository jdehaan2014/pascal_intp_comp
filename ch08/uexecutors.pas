unit uExecutors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uMessages, uCode, uCodeImpl, uRuntimeErrorCode, uExecutor, uObjectUtils,
  uSymTab, uSymTabImpl, AnyObject, fgl;

type

  { TStatementExecutor }

  TStatementExecutor = class(TExecutor)
    public
      constructor Create(Parent: TExecutor);
      function Execute(Node: ICodeNode): TObject; //execute a statement
    private
      procedure sendSourceLineMessage(Node: ICodeNode);
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
    private
      procedure sendAssignMessage(Node: ICodeNode; variableName: string; Value: TObject);
  end;

  { TExpressionExecutor }

  TExpressionExecutor = class(TStatementExecutor)
    public
      constructor Create(Parent: TExecutor);
      function Execute(Node: ICodeNode): TObject;  //execute a assignment statement
    private
      const
        ArithOps : TICodeNodeTypeSet = [
          ctAdd, ctSubtract, ctMultiply, ctFloatDivide, ctIntegerDivide, ctMod];
        RelOps : TICodeNodeTypeSet = [ctEQ,ctNE,ctLT,ctLE,ctGT,ctGE];
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


implementation

{ TStatementExecutor }

constructor TStatementExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

function TStatementExecutor.Execute(Node: ICodeNode): TObject;
var
  nodeType: TICodeNodeTypeImpl.Values;
  compoundExecutor: TCompoundExecutor;
  assignmentExecutor: TAssignmentExecutor;
  loopExecutor: TLoopExecutor;
  ifExecutor: TIfExecutor;
  selectExecutor: TSelectExecutor;
begin
  nodeType := TICodeNodeTypeImpl.toTyp(Node.getType);
  //send a message about the current source line
  sendSourceLineMessage(Node);
  case nodeType of
    ctCompound:
      begin
        compoundExecutor := TCompoundExecutor.Create(Self);
        Result := compoundExecutor.Execute(Node);
      end;
    ctAssign:
      begin
        assignmentExecutor := TAssignmentExecutor.Create(Self);
        Result := assignmentExecutor.Execute(Node);
      end;
    ctLoop:
      begin
        loopExecutor := TLoopExecutor.Create(Self);
        Result := loopExecutor.Execute(Node);
      end;
    ctIf:
      begin
        ifExecutor := TIfExecutor.Create(Self);
        Result := ifExecutor.Execute(Node);
      end;
    ctSelect:
      begin
        selectExecutor := TSelectExecutor.Create(Self);
        Result := selectExecutor.Execute(Node);
      end;
    ctNoOp: Result := Nil;
    else begin
      errorHandler.Flag(Node, rcUNIMPLEMENTED_FEATURE, Self);
      Result := Nil;
    end;
  end;
end;

procedure TStatementExecutor.sendSourceLineMessage(Node: ICodeNode);
var
  lineNumber: TObject;
begin
  lineNumber := Node.getAttribute(ckLine);
  if lineNumber <> Nil then
    sendMessage(TMessage.Create(mtSourceLine,
      TSourceLineMsg.Create('', TInteger(lineNumber).Value)));
end;

{ TCompoundExecutor }

constructor TCompoundExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

function TCompoundExecutor.Execute(Node: ICodeNode): TObject;
var
  statementExecutor: TStatementExecutor;
  Children: TICodeNodeList;
  child: ICodeNode;
begin
  //loop over the children of the COMPOUND node and execute each child
  statementExecutor := TStatementExecutor.Create(Self);
  Children := Node.getChildren;
  for child in Children do
    statementExecutor.Execute(child);
  Result := Nil;
end;


{ TAssignmentExecutor }

constructor TAssignmentExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

function TAssignmentExecutor.Execute(Node: ICodeNode): TObject;
var
  expressionExecutor: TExpressionExecutor;
  Children: TICodeNodeList;
  variableNode, expressionNode: ICodeNode;
  value: TObject;
  variableID: ISymTabEntry;
begin
  //the ASSIGN node's children are the target variable end the expression
  Children := Node.getChildren;
  variableNode := Children[0];
  expressionNode := Children[1];
  //execute the expression and its value
  expressionExecutor := TExpressionExecutor.Create(Self);
  value := expressionExecutor.Execute(expressionNode);
  //writeln('Value= ', value.toString);
  //set the value as an attribute of the variable's symbol table entry
  variableID := TSymTabEntryImpl(variableNode.getAttribute(ckID));
  variableID.setAttribute(skDataValue, value);

  sendAssignMessage(Node, variableID.getName, value);

  inc(executionCount);
  result := Nil;
end;

//send a message about the assignment operation
procedure TAssignmentExecutor.sendAssignMessage(Node: ICodeNode;
  variableName: string; Value: TObject);
var
  lineNumber: TObject;
begin
  lineNumber := Node.getAttribute(ckLine);

  //send an assign message
  if lineNumber <> Nil then
    sendMessage(TMessage.Create(mtAssign,
      TAssignMsg.Create((lineNumber as TInteger).Value, variableName, Value)));
end;

{ TExpressionExecutor }

constructor TExpressionExecutor.Create(Parent: TExecutor);
begin
  Inherited Create(Parent);
end;

//execute an exprssion
function TExpressionExecutor.Execute(Node: ICodeNode): TObject;
var
  nodeType: TICodeNodeTypeImpl.Values;
  Entry: ISymTabEntry;
  Children: TICodeNodeList;
  expressionNode: ICodeNode;
  Value: TObject;
  intValue: Integer;
  fltValue: Double;
  boolValue: Boolean;
begin
  nodeType := TICodeNodeTypeImpl.toTyp(Node.getType);
  case nodeType of
    ctVariable:
      begin
        //get the variable's symbol table entry and return its value
        Entry := TSymTabEntryImpl(Node.getAttribute(ckID));
        Result := Entry.getAttribute(skDataValue);
      end;
    ctIntegerConstant: Result := TInteger(Node.getAttribute(ckValue));
    ctRealConstant   : Result := TFloat(Node.getAttribute(ckValue));
    ctStringConstant : Result := TString(Node.getAttribute(ckValue));
    ctNegate:
      begin
        //get the NEGATE node's expression node child
        Children := Node.getChildren;
        expressionNode := Children[0];
        //execute the expression and return the negative of its value
        Value := Execute(expressionNode);
        if Value is TInteger then begin
          intValue := TInteger(Value).Value;
          Result := TInteger.Create(-intValue);
        end
        else begin
          fltValue := TFloat(Value).Value;
          Result := TFloat.Create(-fltValue);
        end
      end;
    ctNot:
      begin
        //get the NOT node's expression node child
        Children := Node.getChildren;
        expressionNode := Children[0];
        //execute the expression and return the NOT of its value
        Value := Execute(expressionNode);
        boolValue := TBoolean(Value).Value;
        Result := TBoolean.Create(not boolValue);
      end;
    else begin
      Result := executeBinaryOperator(Node, nodeType);
    end;
  end;
end;

//set of arithatic operator node types
function TExpressionExecutor.ExecuteBinaryOperator(Node: ICodeNode;
  nodeType: TICodeNodeTypeImpl.Values): TObject;
var
  Children: TICodeNodeList;
  operandNode1, operandNode2: ICodeNode;
  operand1, operand2: TObject;
  integerMode: boolean;
  intValue1, intValue2: integer;
  fltValue1, fltValue2: double;
  boolValue1, boolvalue2: boolean;

begin
  //get the 2 operand children of the operator node
  Children := Node.getChildren;
  operandNode1 := Children[0];
  operandNode2 := Children[1];
  //operands
  operand1 := Execute(operandNode1);
  operand2 := Execute(operandNode2);
  integerMode := (operand1 is TInteger) and (operand2 is TInteger);
  //arithmetic operators
  if nodeType in ArithOps then begin
    if integerMode then begin
      intValue1 := TInteger(operand1).Value;
      intValue2 := TInteger(operand2).Value;
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
  else if nodeType in RelOps then begin           //relational operators
    if integerMode then begin
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
  end
  else Result := TInteger(0);
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
    Inc(ExecutionCount);   // count the loop statement istelf
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
      else StatementExecutor.Execute(Child);
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
  strConstant: string;
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
  else
    for constNode in constantsList do begin
      strConstant := TString(constNode.getAttribute(ckValue)).Value;
      if TString(selectValue).Value = strConstant then begin
        Result := True; //match
        Break;
      end;
    end;
end;


end.

