unit uGenerators;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uCodeGen, uGenericInterfaces, uLocals, uCode, uSymTabImpl,
  uDirective, uInstruction, uDefinitionImpl, uObjectUtils, uCodeImpl,
  uPredefined, uTypesImpl;

type

  { TProgramGenerator }

  TProgramGenerator = class(TCodeGenerator)
    private
      ProgramID: ISymTabEntry;
      ProgName: String;
    public
      Constructor Create(Parent:TCodeGenerator);
      procedure Generate(Node: ICodeNode); override;
    private
      procedure GenerateFields;
      procedure GenerateConstructor;
      procedure GenerateRoutines;
      procedure GenerateMainMethod;
      procedure GenerateMainMethodPrologue;
      procedure GenerateMainMethodCode;
      procedure GenerateMainMethodEpilogue;
  end;

  { TStructuredDataGenerator }

  TStructuredDataGenerator = class(TCodeGenerator)
    public
      constructor Create(Parent: TCodeGenerator);
      procedure Generate(RoutineID: ISymTabEntry); override;
  end;

  { TDeclaredRoutineGenerator }

  TDeclaredRoutineGenerator = class(TCodeGenerator)
    private
      RoutineID: ISymTabEntry;
      RoutineName: String;
      FunctionValueSlot: Integer; //function return value slot number
    public
      constructor Create(Parent:TCodeGenerator);
      procedure Generate(aRoutineID: ISymTabEntry); override;
    private
      procedure GenerateRoutineHeader;
      procedure GenerateRoutineLocals;
      procedure GenerateRoutineCode;
      procedure GenerateRoutineReturn;
      procedure GenerateRoutineEpilogue;
  end;

  { TStatementGenerator }

  TStatementGenerator = class(TCodeGenerator)
    public
      Constructor Create(Parent: TCodeGenerator);
      procedure Generate(Node: ICodeNode); override;
    private
      function getLineNumber(Node: ICodeNode): integer;
  end;

  { TExpressionGenerator }

  TExpressionGenerator = class(TStatementGenerator)
    public
      Constructor Create(Parent: TCodeGenerator);
      procedure Generate(Node: ICodeNode); override;
    protected
      procedure GenerateLoadValue(VariableNode: ICodeNode);
      function GenerateLoadVariable(VariableNode: ICodeNode): ITypeSpec;
    private
      const
        // Set of arithmetic operator node types.
        ARITH_OPS: TICodeNodeTypeSet = [ctAdd, ctSubtract, ctMultiply,
          ctIntegerDivide, ctFloatDivide, ctMod];
      procedure GenerateBinaryOperator(Node: ICodeNode;
        NodeType: TICodeNodeTypeImpl.Values);
  end;

  { TCompoundGenerator }

  TCompoundGenerator = class(TStatementGenerator)
    public
      Constructor Create(Parent: TCodeGenerator);
      procedure Generate(Node: ICodeNode); override;
  end;

  { TAssignmentGenerator }

  TAssignmentGenerator = class(TStatementGenerator)
    public
      Constructor Create(Parent: TCodeGenerator);
      procedure Generate(Node: ICodeNode); override;
    private
      procedure GenerateScalarAssignment(TargetType: ITypeSpec;
        TargetID: ISymTabEntry; Index, NestingLevel: integer;
        ExprNode: ICodeNode; ExprType: ITypeSpec;
        ExpressionGenerator: TExpressionGenerator);
  end;

implementation

{ TProgramGenerator }

constructor TProgramGenerator.Create(Parent: TCodeGenerator);
begin
  Inherited Create(Parent);
end;

//Generate code for the main program
procedure TProgramGenerator.Generate(Node: ICodeNode);
begin
  ProgramID := SymTabStack.getProgramID;
  ProgName := ProgramID.getName;

  LocalVariables := TLocalVariables.Create(0);
  LocalStack := TLocalStack.Create;

  EmitDirective(dirCLASS_PUBLIC, ProgName);
  EmitDirective(dirSUPER, 'java/lang/Object');

  GenerateFields;
  GenerateConstructor;
  GenerateRoutines;
  GenerateMainMethod;
end;

//generate directives for the fields
procedure TProgramGenerator.GenerateFields;
var
  SymTab: ISymTab;
  ID: ISymTabEntry;
  IDs: TSymTabEntries;
  Defn: IDefinition;
begin
  //Runtime timer and standard in
  EmitBlankLine;
  EmitDirective(dirFIELD_PRIVATE_STATIC, '_runTimer', 'LRunTimer;');
  EmitDirective(dirFIELD_PRIVATE_STATIC, '_standardIn', 'LPascalTextIn;');

  SymTab := ProgramID.getAttribute(skRoutineSymtab) as ISymTab;
  IDs := SymTab.SortedEntries;
  EmitBlankLine;
  //Loop over all the program's identifiers and emit a .field directive for each.
  for ID in IDs do begin
    Defn := ID.getDefinition;
    if Defn = defVariable then
      EmitDirective(dirFIELD_PRIVATE_STATIC, ID.getName, TypeDescriptor(ID));
  end;
end;

//generate code for the main program constructor
procedure TProgramGenerator.GenerateConstructor;
begin
  EmitBlankLine;
  EmitDirective(dirMETHOD_PUBLIC, '<init>()V');

  EmitBlankLine;
  Emit(_ALOAD_0);
  Emit(_INVOKENONVIRTUAL, 'java/lang/Object/<init>()V');
  Emit(_RETURN);

  EmitBlankLine;
  EmitDirective(dirLIMIT_LOCALS, 1);
  EmitDirective(dirLIMIT_STACK, 1);

  EmitDirective(dirEND_METHOD);
end;

//generate code for any nested procedures and functions
procedure TProgramGenerator.GenerateRoutines;
var
  RouineIDs: TSymTabEntries;
  ID: ISymTabEntry;
  DeclaredRoutineGenerator: TDeclaredRoutineGenerator;
begin
  DeclaredRoutineGenerator := TDeclaredRoutineGenerator.Create(Self);
  RouineIDs := TSymTabEntries(ProgramID.getAttribute(skRoutineRoutines));
  //generate code for each procedure or function
  for ID in RouineIDs do
    DeclaredRoutineGenerator.Generate(ID);
end;

//generate code for the program body as the main method
procedure TProgramGenerator.GenerateMainMethod;
var
  StructuredDataGenerator: TStructuredDataGenerator;
begin
  EmitBlankLine;
  EmitDirective(dirMETHOD_PUBLIC_STATIC, 'main([Ljava/lang/String;)V');
  GenerateMainMethodPrologue;
  //generate code to allocate any arrays, records, and strings
  StructuredDataGenerator := TStructuredDataGenerator.Create(Self);
  StructuredDataGenerator.Generate(ProgramID);
  GenerateMainMethodCode;
  GenerateMainMethodEpilogue;
end;

//generate the main method prologue.
procedure TProgramGenerator.GenerateMainMethodPrologue;
var
  Name: String;
begin
  Name := ProgramId.getName;

  // Runtime timer.
  EmitBlankLine;
  Emit(_NEW, 'RunTimer');
  Emit(_DUP);
  Emit(_INVOKENONVIRTUAL, 'RunTimer/<init>()V');
  Emit(_PUTSTATIC, Name + '/_runTimer', 'LRunTimer;');

  // Standard in.
  Emit(_NEW, 'PascalTextIn');
  Emit(_DUP);
  Emit(_INVOKENONVIRTUAL, 'PascalTextIn/<init>()V');
  Emit(_PUTSTATIC, Name + '/_standardIn LPascalTextIn;');

  LocalStack.Use(3);
end;

//Generate code for the main method.
procedure TProgramGenerator.GenerateMainMethodCode;
var
  aICode: IIntermediateCode;
  Root: ICodeNode;
  StatementGenerator: TStatementGenerator;
begin
  aICode := ProgramId.getAttribute(skRoutineICode) as IIntermediateCode;
  Root := aICode.getRoot;

  EmitBlankLine;

  // Generate code for the compound statement.
  StatementGenerator := TStatementGenerator.Create(Self);
  StatementGenerator.Generate(Root);
end;

//Generate the main method epilogue.
procedure TProgramGenerator.GenerateMainMethodEpilogue;
begin
  // Print the execution time.
  EmitBlankLine;
  Emit(_GETSTATIC, ProgName + '/_runTimer', 'LRunTimer;');
  Emit(_INVOKEVIRTUAL, 'RunTimer.printElapsedTime()V');

  LocalStack.Use(1);

  EmitBlankLine;
  Emit(_RETURN);
  EmitBlankLine;

  EmitDirective(dirLIMIT_LOCALS, LocalVariables.Count);
  EmitDirective(dirLIMIT_STACK, LocalStack.Capacity);
  EmitDirective(dirEND_METHOD);
end;

{ TStructuredDataGenerator }

constructor TStructuredDataGenerator.Create(Parent: TCodeGenerator);
begin
  Inherited Create(Parent);
end;

procedure TStructuredDataGenerator.Generate(RoutineID: ISymTabEntry);
begin
  //generate code to allocate the structured data of a program, procedure or function
end;

{ TDeclaredRoutineGenerator }

constructor TDeclaredRoutineGenerator.Create(Parent: TCodeGenerator);
begin
  Inherited Create(Parent);
end;

//Generate code for a declared procedure or function
procedure TDeclaredRoutineGenerator.Generate(aRoutineID: ISymTabEntry);
var
  RoutineSymTab: ISymTab;
  StructuredDataGenerator: TStructuredDataGenerator;
begin
  RoutineId := aRoutineId;
  RoutineName := RoutineId.getName;

  RoutineSymTab := RoutineId.getAttribute(skRoutineSymTab) as ISymTab;
  LocalVariables := TLocalVariables.Create(RoutineSymTab.maxSlotNumber);
  LocalStack := TLocalStack.Create;

  // Reserve an extra variable for the function return value.
  if RoutineId.getDefinition = defFunction then begin
    FunctionValueSlot := LocalVariables.Reserve;
    RoutineId.setAttribute(skSlot, TInteger.Create(FunctionValueSlot));
  end;

  GenerateRoutineHeader;
  GenerateRoutineLocals;

  // Generate code to allocate any arrays, records, and strings.
  StructuredDataGenerator := TStructuredDataGenerator.Create(Self);
  StructuredDataGenerator.Generate(RoutineId);

  GenerateRoutineCode;
  GenerateRoutineReturn;
  GenerateRoutineEpilogue;
end;

//Generate the routine header.
procedure TDeclaredRoutineGenerator.GenerateRoutineHeader;
var
  Name: String;
  ParmID: ISymTabEntry;
  ParmIDs: TSymTabEntries;
  Buffer: TString;
begin
  Name := RoutineId.getName;
  ParmIDs := TSymTabEntries(RoutineId.getAttribute(skRoutineParms));
  Buffer := TString.Create;

  // Procedure or function name.
  Buffer.Append(Name);
  Buffer.Append('(');
  // Parameter and return type descriptors.
  if ParmIDs <> Nil then
    for ParmID in parmIDs do
      Buffer.Append(TypeDescriptor(ParmID));
  Buffer.Append(')');
  Buffer.Append(TypeDescriptor(RoutineId));

  EmitBlankLine;
  EmitDirective(dirMETHOD_PRIVATE_STATIC, Buffer.toString);
end;

//Generate directives for the local variables.
procedure TDeclaredRoutineGenerator.GenerateRoutineLocals;
var
  SymTab: ISymTab;
  ID: ISymTabEntry;
  IDs: TSymTabEntries;
  Defn: IDefinition;
  Slot: Integer;
begin
  SymTab := RoutineId.getAttribute(skRoutineSymtab) as ISymTab;
  IDs := SymTab.sortedEntries;

  EmitBlankLine;
  // Loop over all the routine's identifiers and
  // emit a .var directive for each variable and formal parameter.
  for ID in IDs do begin
    Defn := ID.getDefinition;
    if (Defn = defVariable) or (Defn = defValue_Parm) or (Defn = defVar_Parm) then
    begin
      Slot := TInteger(ID.getAttribute(skSlot)).Value;
      EmitDirective(dirVAR,
        IntToStr(Slot) + ' is ' + ID.getName, TypeDescriptor(ID));
    end;
  end;
  // Emit an extra .var directive for an implied function variable.
  if RoutineId.getDefinition = defFunction then
    EmitDirective(dirVAR, IntToStr(FunctionValueSlot) +  ' is ' +
       RoutineName, TypeDescriptor(RoutineId.getTypeSpec));
end;

//Generate code for the routine's body.
procedure TDeclaredRoutineGenerator.GenerateRoutineCode;
var
  Code: IIntermediateCode;
  Root: ICodeNode;
  StatementGenerator: TStatementGenerator;
begin
  Code := RoutineID.getAttribute(skRoutineICode) as IIntermediateCode;
  Root := Code.getRoot;
  EmitBlankLine;
  //Generate code for the compound statement
  StatementGenerator := TStatementGenerator.Create(Self);
  StatementGenerator.Generate(Root);
end;

//Generate the routine's return code.
procedure TDeclaredRoutineGenerator.GenerateRoutineReturn;
var
  Typ: ITypeSpec;
begin
  EmitBlankLine;
  //Function: Return the value in the implied function variable
  if RoutineID.getDefinition = defFunction then begin
    Typ := RoutineID.getTypeSpec;
    EmitLoadLocal(Typ, FunctionValueSlot);
    EmitReturnValue(Typ);
    LocalStack.Use(1);
  end
  //procedure: just return
  else
    Emit(_RETURN);
end;

//Generate the routine's epilogue.
procedure TDeclaredRoutineGenerator.GenerateRoutineEpilogue;
begin
  EmitBlankLine;
  EmitDirective(dirLIMIT_LOCALS, LocalVariables.Count);
  EmitDirective(dirLIMIT_STACK, LocalStack.Capacity);
  EmitDirective(dirEND_METHOD);
end;

{ TStatementGenerator }

constructor TStatementGenerator.Create(Parent: TCodeGenerator);
begin
  Inherited Create(Parent);
end;

//Generate code for a statement
procedure TStatementGenerator.Generate(Node: ICodeNode);
var
  NodeType: TICodeNodeTypeImpl.Values;
  Line: integer = 0;
  CompoundGenerator: TCompoundGenerator;
  AssignmentGenerator: TAssignmentGenerator;
begin
  NodeType := TICodeNodeTypeImpl(Node.getType.getObject).Value;
  if NodeType <> ctCompound then begin
    Line := getLineNumber(Node);
    EmitDirective(dirLINE, Line);
  end;
  //generate code for a statement according to the type of statement
  case NodeType of
    ctCompound:
      begin
        CompoundGenerator := TCompoundGenerator.Create(Self);
        CompoundGenerator.Generate(Node);
      end;
    ctAssign:
      begin
        AssignmentGenerator := TAssignmentGenerator.Create(Self);
        AssignmentGenerator.Generate(Node);
      end;
  end;
  //verify that the stack height after eacht statement is 0
  if LocalStack.getSize <> 0 then
    Exception.CreateFmt('Stack size error: size = %d after line %d',
      [LocalStack.getSize, Line]);
end;

function TStatementGenerator.getLineNumber(Node: ICodeNode): integer;
var
  LineNumber: TInteger = Nil;
begin
  //go up the parent links to look for a line number
  LineNumber := TInteger(Node.getAttribute(ckLine));
  while (Node <> Nil) and (LineNumber = Nil) do begin
    Node := Node.getParent;
    LineNumber := TInteger(Node.getAttribute(ckLine));
  end;
  Result := LineNumber.Value;
end;


{ TExpressionGenerator }

constructor TExpressionGenerator.Create(Parent: TCodeGenerator);
begin
  Inherited Create(Parent);
end;

//Generate code to evaluate an expression.
procedure TExpressionGenerator.Generate(Node: ICodeNode);
var
  NodeType: TICodeNodeTypeImpl.Values;
  Typ: ITypeSpec;
  IntValue: Integer;
  FltValue: Real;
  StrValue: String;
  Children: TICodeNodeList;
  ExpressionNode: ICodeNode;
begin
  NodeType := TICodeNodeTypeImpl(Node.getType.getObject).Value;
  case NodeType of
    ctVariable: GenerateLoadValue(Node);
    ctIntegerConstant:
      begin
        Typ := Node.getTypeSpec;
        IntValue := TInteger(Node.getAttribute(ckValue)).Value;
        //Generate code to load a boolean constant
        // 0 (false) or 1 (true).
        if Typ = TPredefined.booleanType then begin
          if IntValue = 1 then
            EmitLoadConstant(1)
          else
            EmitLoadConstant(0);
        end
        else
          EmitLoadConstant(IntValue);
        LocalStack.Increase(1);
      end;
    ctRealConstant:
      begin
        FltValue := TFloat(Node.getAttribute(ckValue)).Value;
        //Generate code to load a float constant.
        EmitLoadConstant(FltValue);
        LocalStack.Increase(1);
      end;
    ctStringConstant:
      begin
        StrValue := TString(Node.getAttribute(ckValue)).Value;
        //Generate code to load a string constant.
        if Node.getTypeSpec = TPredefined.charType then
          EmitLoadConstant(Ord(StrValue[1]))
        else
          EmitLoadConstant(StrValue);
        LocalStack.Increase(1);
      end;
    ctNegate:
      begin
        //Get the NEGATE node's expression node child.
        Children := Node.getChildren;
        ExpressionNode := children[0];
        //Generate code to evaluate the expression and negate its value.
       Generate(ExpressionNode);
       if ExpressionNode.getTypeSpec = TPredefined.integerType then
         Emit(_INEG)
       else
         Emit(_FNEG);
      end;
    ctNot:
      begin
        //Get the NOT node's expression node child.
        Children := Node.getChildren;
        ExpressionNode := children[0];
        // Generate code to evaluate the expression and NOT its value.
        Generate(ExpressionNode);
        emit(_ICONST_1);
        emit(_IXOR);
        LocalStack.Use(1);
      end;
    else
      GenerateBinaryOperator(Node, NodeType);
  end;
end;

//Generate code to load a variable's value.
procedure TExpressionGenerator.GenerateLoadValue(VariableNode: ICodeNode);
begin
  GenerateLoadVariable(VariableNode);
end;

//Generate code to load a variable's address (structured) or value (scalar).
function TExpressionGenerator.GenerateLoadVariable(VariableNode: ICodeNode
  ): ITypeSpec;
var
  VariableId: ISymTabEntry;
  VariableType: ITypeSpec;
begin
  VariableId := VariableNode.getAttribute(ckID) as ISymTabEntry;
  VariableType := VariableId.getTypeSpec;
  EmitLoadVariable(VariableId);
  LocalStack.Increase(1);
  Result := VariableType;
end;

//Generate code to evaluate a binary operator.
procedure TExpressionGenerator.GenerateBinaryOperator
  (Node: ICodeNode; NodeType: TICodeNodeTypeImpl.Values);
var
  Children: TICodeNodeList;
  OperandNode1, OperandNode2: ICodeNode;
  Typ1, Typ2: ITypeSpec;
  IntegerMode, RealMode, CharacterMode, StringMode: boolean;
  TrueLabel, NextLabel: TLabel;
begin
  Children := Node.getChildren;
  OperandNode1 := Children[0];
  OperandNode2 := Children[1];
  Typ1 := OperandNode1.getTypeSpec;
  Typ2 := OperandNode2.getTypeSpec;
  IntegerMode :=   TTypeChecker.areBothInteger(Typ1, Typ2) or
                   (Typ1.getForm = tfEnumeration) or
                   (Typ2.getForm = tfEnumeration);
  RealMode :=      TTypeChecker.isAtLeastOneReal(Typ1, Typ2) or
                   (NodeType = ctFloatDivide);
  CharacterMode := TTypeChecker.isChar(Typ1) and TTypeChecker.isChar(Typ2);
  StringMode :=    Typ1.isPascalString and Typ2.isPascalString;

  if not StringMode then begin
    // Emit code to evaluate the first operand.
    Generate(OperandNode1);
    if RealMode and TTypeChecker.isInteger(Typ1) then
      Emit(_I2F);

    // Emit code to evaluate the second operand.
    Generate(OperandNode2);
    if RealMode and TTypeChecker.isInteger(Typ2) then
      Emit(_I2F);
  end;

  // ====================
  // Arithmetic operators
  // ====================

  if ARITH_OPS.Contains(NodeType) then begin
    if IntegerMode then begin
      case NodeType of
        ctAdd:           Emit(_IADD);
        ctSubtract:      Emit(_ISUB);
        ctMultiply:      Emit(_IMUL);
        ctFloatDivide:   Emit(_FDIV);
        ctIntegerDivide: Emit(_IDIV);
        ctMod:           Emit(_IREM);
      end;
    end
    else begin
      case NodeType of
        ctAdd:           Emit(_FADD);
        ctSubtract:      Emit(_FSUB);
        ctMultiply:      Emit(_FMUL);
        ctFloatDivide:   Emit(_FDIV);
      end;
    end;
    LocalStack.Decrease(1);
  end

  // ==========
  // AND and OR
  // ==========

  else if NodeType = ctAnd then begin
    Emit(_IAND);
    LocalStack.Decrease(1);
  end
  else if NodeType = ctOr then begin
    Emit(_IOR);
    LocalStack.Decrease(1);
  end

  // ====================
  // Relational operators
  // ====================
  else begin
    TrueLabel := TLabel.newLabel;
    NextLabel := TLabel.newLabel;
    if IntegerMode or CharacterMode then begin
      case nodeType of
        ctEQ: Emit(_IF_ICMPEQ, TrueLabel);
        ctNE: Emit(_IF_ICMPNE, TrueLabel);
        ctLT: Emit(_IF_ICMPLT, TrueLabel);
        ctLE: Emit(_IF_ICMPLE, TrueLabel);
        ctGT: Emit(_IF_ICMPGT, TrueLabel);
        ctGE: Emit(_IF_ICMPGE, TrueLabel);
      end;
      LocalStack.Decrease(2);
    end
    else if RealMode then begin
      Emit(_FCMPG);
      case nodeType of
        ctEQ: Emit(_IFEQ, TrueLabel);
        ctNE: Emit(_IFNE, TrueLabel);
        ctLT: Emit(_IFLT, TrueLabel);
        ctLE: Emit(_IFLE, TrueLabel);
        ctGT: Emit(_IFGT, TrueLabel);
        ctGE: Emit(_IFGE, TrueLabel);
      end;
      LocalStack.Decrease(2);
    end;

    Emit(_ICONST_0); // false
    Emit(_GOTO, NextLabel);
    EmitLabel(TrueLabel);
    Emit(_ICONST_1); // true
    EmitLabel(NextLabel);

    LocalStack.Increase(1);
  end;
end;

{ TCompoundGenerator }

constructor TCompoundGenerator.Create(Parent: TCodeGenerator);
begin
  inherited Create(Parent);
end;

//Generate code for a compound statement
procedure TCompoundGenerator.Generate(Node: ICodeNode);
var
  Children: TICodeNodeList;
  Child: ICodeNode;
  StatementGenerator: TStatementGenerator;
begin
  Children := Node.getChildren;
  //Loop over the statement children of the ctCompound node and generate
  //code for each statement. Emit a _NOP if there are no statements.
  if Children.Count = 0 then
    Emit(_NOP)
  else begin
    StatementGenerator := TStatementGenerator.Create(Self);
    for Child in Children do
      StatementGenerator.Generate(Child);
  end;
end;

{ TAssignmentGenerator }

constructor TAssignmentGenerator.Create(Parent: TCodeGenerator);
begin
  inherited Create(Parent);
end;

//Generate code for an assignment statement.
procedure TAssignmentGenerator.Generate(Node: ICodeNode);
var
  AssignmentType, TargetType, ExprType: ITypeSpec;
  AssignChildren: TICodeNodeList;
  TargetNode, ExprNode: ICodeNode;
  TargetID: ISymTabEntry;
  ExpressionGenerator: TExpressionGenerator;
  Slot,                     // local variables array slot number of the target
  NestingLevel: integer;    // nesting level of the target
  SymTab: ISymTab;          // symbol table that contains the target id
begin
  AssignmentType := Node.getTypeSpec;
  //The ASSIGN node's children are the target variable and the expression.
  AssignChildren := Node.getChildren;
  TargetNode := AssignChildren[0];
  ExprNode := AssignChildren[1];
  TargetID := TargetNode.getAttribute(ckID) as ISymTabEntry;
  TargetType := TargetNode.getTypeSpec;
  ExprType := ExprNode.getTypeSpec;
  ExpressionGenerator := TExpressionGenerator.Create(Self);
  // Assign a function value. Use the slot number of the function value.
  if TargetId.getDefinition = DefFunction then begin
    Slot := TInteger(TargetId.getAttribute(skSLOT)).Value;
    NestingLevel := 2;
  end
  // Standard assignment.
  else begin
    SymTab := TargetId.getSymTab;
    Slot := TInteger(TargetId.getAttribute(skSLOT)).Value;
    NestingLevel := SymTab.getNestingLevel;
  end;

  // Generate code to do the assignment.
  GenerateScalarAssignment(TargetType, TargetId, Slot, NestingLevel,
                           ExprNode, ExprType, ExpressionGenerator);
end;

//Generate code to assign a scalar value.
procedure TAssignmentGenerator.GenerateScalarAssignment(TargetType: ITypeSpec;
  TargetID: ISymTabEntry; Index, NestingLevel: integer; ExprNode: ICodeNode;
  ExprType: ITypeSpec; ExpressionGenerator: TExpressionGenerator);
var
  Value: TInteger;
  ChrValue: Char;
begin
  // Generate code to evaluate the expression.
  // Special cases: float variable := integer constant
  //                float variable := integer expression
  //                char variable  := single-character string constant
  if TargetType = TPredefined.realType then begin
    if ExprNode.getType = ctIntegerConstant then begin
      Value := TInteger(ExprNode.getAttribute(ckVALUE));
      EmitLoadConstant(Value.floatValue);
      LocalStack.Increase(1);
    end
    else begin
      ExpressionGenerator.Generate(ExprNode);
      if ExprType.baseType = TPredefined.integerType then
        Emit(_I2F);
    end
  end
  else if (TargetType = TPredefined.charType) and
          (ExprNode.getType = ctStringConstant) then begin
    ChrValue := TString(ExprNode.getAttribute(ckVALUE)).CharAt(0);
    EmitLoadConstant(Ord(ChrValue));
    LocalStack.Increase(1);
  end
  else
    ExpressionGenerator.Generate(ExprNode);

  //Generate code to store the expression value into the target variable.
  EmitStoreVariable(TargetId, NestingLevel, Index);
  if isWrapped(targetId) then
    LocalStack.Decrease(2)
  else
    LocalStack.Decrease(1);
end;


end.

