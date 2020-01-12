unit uExecutors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uMessages, uCode, uCodeImpl, uRuntimeErrorCode, uExecutor, uObjectUtils,
  uSymTab, uSymTabImpl, AnyObject;

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



end.

