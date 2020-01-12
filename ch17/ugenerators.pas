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
      const
        C_SETLENGTH = 'java/lang/StringBuilder.setLength(I)V';
        C_PAD_BLANKS = 'PaddedString.blanks(II)Ljava/lang/StringBuilder;';
        C_APPEND_STRING =
          'java/lang/StringBuilder.append(Ljava/lang/String;)' +
          'Ljava/lang/StringBuilder;';
        C_APPEND_CHARSEQUENCE =
          'java/lang/StringBuilder.append(Ljava/lang/CharSequence;)' +
          'Ljava/lang/StringBuilder;';
        C_STRINGBUILDER_SUBSTRING =
          'java/lang/StringBuilder.substring(II)Ljava/lang/String;';
        C_STRING_SUBSTRING = 'java/lang/String.substring(II)Ljava/lang/String;';
      procedure GenerateScalarAssignment(TargetType: ITypeSpec;
        TargetID: ISymTabEntry; Index, NestingLevel: integer;
        ExprNode: ICodeNode; ExprType: ITypeSpec;
        ExpressionGenerator: TExpressionGenerator);
      procedure GenerateStringAssignment(TargetType: ITypeSpec;
        ExprNode: ICodeNode; ExprType: ITypeSpec;
        ExpressionGenerator: TExpressionGenerator);
  end;

  { TCallGenerator }

  TCallGenerator = class(TStatementGenerator)
    public
      Constructor Create(Parent: TCodeGenerator);
      procedure Generate(Node: ICodeNode); override;
  end;

  { TCallDeclaredGenerator }

  TCallDeclaredGenerator = class(TCallGenerator)
    public
      Constructor Create(Parent: TCodeGenerator);
      procedure Generate(Node: ICodeNode); override;
    private
      procedure GenerateActualParams(CallNode: ICodeNode);
      procedure GenerateWrap(ActualNode: ICodeNode; FormalType: ITypeSpec;
        WrapSlot: integer; ExprGenerator: TExpressionGenerator);
      procedure CloneActualParameter(FormalType: ITypeSpec);
      procedure GenerateCall(CallNode: ICodeNode);
      procedure GenerateCallEpilogue(CallNode: ICodeNode);
      procedure GenerateUnwrap(ActualID: ISymTabEntry; FormalType: ITypeSpec;
        AProgramName: string);
  end;

  { TCallStandardGenerator }

  TCallStandardGenerator = class(TCallGenerator)
    public
      Constructor Create(Parent: TCodeGenerator);
      destructor Destroy; override;
      procedure Generate(Node: ICodeNode); override;
    private
      ExprGenerator: TExpressionGenerator;
      procedure GenerateReadReadln(CallNode: ICodeNode; RoutineCode: IRoutineCode);
      procedure GenerateEofEoln(CallNode: ICodeNode; RoutineCode: IRoutineCode);
      procedure GenerateWriteWriteln(CallNode: ICodeNode; RoutineCode: IRoutineCode);
      procedure GenerateAbsSqr(RoutineCode: IRoutineCode; ActualNode: ICodeNode);
      procedure GenerateArctanCosExpLnSinSqrt(RoutineCode: IRoutineCode;
        ActualNode: ICodeNode);
      procedure GeneratePredSucc(RoutineCode: IRoutineCode; ActualNode: ICodeNode);
      procedure GenerateChr(ActualNode: ICodeNode);
      procedure GenerateOdd(ActualNode: ICodeNode);
      procedure GenerateOrd(ActualNode: ICodeNode);
      procedure GenerateRoundTrunc(RoutineCode: IRoutineCode; ActualNode: ICodeNode);
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
  //assign to a VAR param: Load the address of the wrapper
  if isWrapped(TargetID) then begin
    EmitLoadLocal(Nil, Slot);
    LocalStack.Increase(1);
  end
  //assign a pascal string;
  else if AssignmentType.isPascalString then begin
    EmitLoadVariable(TargetID);
    LocalStack.Increase(1);
  end;

  // Generate code to do the assignment.
  if TargetType.isPascalString then
    GenerateStringAssignment(AssignmentType, ExprNode, ExprType,
                             ExpressionGenerator)
  else
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

//Generate code to assign a Pascal string value
procedure TAssignmentGenerator.GenerateStringAssignment(TargetType: ITypeSpec;
  ExprNode: ICodeNode; ExprType: ITypeSpec;
  ExpressionGenerator: TExpressionGenerator);
var
  TargetLength, SourceLength: Integer;
  Value: TString;
  Appender, SubstringRoutine: string;
begin
  TargetLength := TInteger(TargetType.getAttribute(tkArrayElementCount)).Value;
  Emit(_DUP);
  EmitLoadConstant(0);
  Emit(_INVOKEVIRTUAL, C_SETLENGTH);
  LocalStack.Use(2, 2);

  //Generate code to load the source string.
  if ExprNode.getType = ctStringConstant then begin
    Value := TString(ExprNode.getAttribute(ckVALUE));
    SourceLength := Value.Length;
    Appender := C_APPEND_STRING;
    EmitLoadConstant(Value.toString);
    LocalStack.Increase(1);
  end
  else begin
    SourceLength := TInteger(ExprType.getAttribute(tkArrayElementCount)).Value;
    Appender := C_APPEND_CHARSEQUENCE;
    ExpressionGenerator.Generate(ExprNode);
  end;
  // Same lengths.
  if TargetLength = SourceLength then begin
    Emit(_INVOKEVIRTUAL, Appender);
    LocalStack.Decrease(1);
  end
  // Truncate if necessary.
  else if TargetLength < SourceLength then begin
    EmitLoadConstant(0);
    EmitLoadConstant(TargetLength);
    if ExprNode.getType = ctStringConstant then
      SubstringRoutine := C_STRING_SUBSTRING
    else
      SubstringRoutine := C_STRINGBUILDER_SUBSTRING;
    Emit(_INVOKEVIRTUAL, SubstringRoutine);
    Emit(_INVOKEVIRTUAL, Appender);
    LocalStack.Use(2, 3);
  end
  // Blank-pad if necessary.
  else begin
    Emit(_INVOKEVIRTUAL, Appender);
    EmitLoadConstant(TargetLength);
    EmitLoadConstant(SourceLength);
    Emit(_INVOKESTATIC, C_PAD_BLANKS);
    Emit(_INVOKEVIRTUAL, C_APPEND_CHARSEQUENCE);
    LocalStack.Use(2, 3);
  end;

  Emit(_POP);
  LocalStack.Decrease(1);
end;

{ TCallGenerator }

constructor TCallGenerator.Create(Parent: TCodeGenerator);
begin
  inherited Create(Parent);
end;

//Generate code to call a procedure or function
procedure TCallGenerator.Generate(Node: ICodeNode);
var
  RoutineID: ISymTabEntry;
  RoutineCode: TRoutineCodeImpl.Values;
  CallDeclaredGenerator: TCallDeclaredGenerator;
  CallStandardGenerator: TCallStandardGenerator;
begin
  RoutineID := Node.getAttribute(ckID) as ISymTabEntry;
  RoutineCode := TRoutineCodeImpl(RoutineID.getAttribute(skRoutineCode)).Value;
  if RoutineCode = rcDeclared then begin
    CallDeclaredGenerator := TCallDeclaredGenerator.Create(Self);
    CallDeclaredGenerator.Generate(Node);
  end
  else begin
    CallStandardGenerator := TCallStandardGenerator.Create(Self);
    CallStandardGenerator.Generate(Node);
  end;
end;

{ TCallDeclaredGenerator }

constructor TCallDeclaredGenerator.Create(Parent: TCodeGenerator);
begin
  Inherited Create(Parent);
end;

//Generate code to call to a declared procedure or function
procedure TCallDeclaredGenerator.Generate(Node: ICodeNode);
var
  RoutineId: ISymTabEntry;
begin
  //Generate code for any actual parameters
  if Node.getChildren.Count > 0 then
    GenerateActualParams(Node);
  //Generate code to make the call
  GenerateCall(Node);
  //Generate code for the epilogue
  if Node.getChildren.Count > 0 then
    GenerateCallEpilogue(Node);
  //A function call leaves a value on the operand stack
  RoutineId := Node.getAttribute(ckID) as ISymTabEntry;
  if RoutineId.getDefinition = defFunction then
    LocalStack.Increase(1)
end;

//Generate code for the actual parameters of a call
procedure TCallDeclaredGenerator.GenerateActualParams(CallNode: ICodeNode);
var
  RoutineId, FormalID, ActualID: ISymTabEntry;
  ParmsNode, ActualNode: ICodeNode;
  ActualNodes: TICodeNodeList;
  FormalIDs: TSymTabEntries;
  exprGenerator: TExpressionGenerator;
  i, Value: Integer;
  FormalType, ActualType: ITypeSpec;
  WrapSlot, ActualSlot: TInteger;
begin
  RoutineId := CallNode.getAttribute(ckID) as ISymTabEntry;
  ParmsNode := CallNode.getChildren[0];
  ActualNodes := ParmsNode.getChildren;
  FormalIDs := RoutineId.getAttribute(skRoutineParms) as TSymTabEntries;
  exprGenerator := TExpressionGenerator.Create(Self);
  //Iterate over the formal parameters
  for i := 0 to FormalIDs.Count - 1 do begin
    FormalID := FormalIDs[i];
    ActualNode := ActualNodes[i];
    ActualID := ActualNode.getAttribute(ckID) as ISymTabEntry;
    FormalType := FormalId.getTypeSpec;
    ActualType := ActualNode.getTypeSpec;
    //VAR parameter: An actual parameter that is not structured needs wrapping
    if isWrapped(FormalID) then begin
      WrapSlot := TInteger(ActualId.getAttribute(skWrapSlot));
      //Already wrapped: Load the wrapper
      if WrapSlot <> Nil then begin
        EmitLoadLocal(Nil, WrapSlot.Value);
        LocalStack.Increase(1);
      end
      //Actual parameter is itself a VAR param: No further wrapping
      else if ActualId.getDefinition = defVar_Parm then begin
        ActualSlot := TInteger(ActualId.getAttribute(skSlot));
        EmitLoadLocal(Nil, ActualSlot.Value);
        LocalStack.Increase(1);
      end
      //Need to wrap: Reserve a temporary variable to hold the wrapper's address
      else begin
        WrapSlot := LocalVariables.Reserve;
        ActualId.setAttribute(skWrapSlot, WrapSlot);
        GenerateWrap(ActualNode, FormalType, WrapSlot, ExprGenerator);
      end;
    end
    //Value parameter: Actual parameter is a constant string
    else if (FormalType = TPredefined.charType) and
            (ActualNode.getType = ctStringConstant) then begin
      Value := Ord(TString(ActualNode.getAttribute(ckValue)).CharAt(0));
      EmitLoadConstant(Value);
      LocalStack.Increase(1);
    end
    //Value param: all other types
    else begin
      exprGenerator.Generate(ActualNode);
      EmitRangeCheck(FormalType);
      //real formal := integer actual
      if (FormalType = TPredefined.realType) and
         (ActualType.baseType = TPredefined.integerType) then
        Emit(_I2F)
      //structured data needs to be cloned.
      else if needsCloning(FormalID) then
        CloneActualParameter(FormalType);
    end;
  end;
end;

//Wrap an actual parameter to pass it by reference in a proc/func call
procedure TCallDeclaredGenerator.GenerateWrap(ActualNode: ICodeNode;
  FormalType: ITypeSpec; WrapSlot: integer; ExprGenerator: TExpressionGenerator);
var
  Wrapper, Init: string;
begin
  //Wrap the value of an actual parameter
  Wrapper := VarParmWrapper(FormalType);  //selected wrapper
  //create the wrapper
  Emit(_NEW, Wrapper);
  Emit(_DUP);
  LocalStack.Increase(2);
  //generate code to evaluate the actual parameter value
  ExprGenerator.Generate(ActualNode);
  //invoke the wrapper's constructor to wrap the parameter value
  Init := Wrapper + '/<init>(' +TypeDescriptor(FormalType) + ')V';
  Emit(_INVOKENONVIRTUAL, Init);
  LocalStack.Decrease(1);
  //store wrapper's address into a temp variable and leave copy on opertand stack
  Emit(_DUP);
  EmitStoreLocal(Nil, WrapSlot);
  LocalStack.Use(1);
end;

//clone an actual param value to be passed by value
procedure TCallDeclaredGenerator.CloneActualParameter(FormalType: ITypeSpec);
begin
  Emit(_INVOKESTATIC, 'Cloner.deepClone(Ljava/lang/Object;)' + 'Ljava/lang/Object;');
  EmitCheckCast(FormalType);
end;

//Generate code to make the call
procedure TCallDeclaredGenerator.GenerateCall(CallNode: ICodeNode);
var
  RoutineId, ParmID: ISymTabEntry;
  RoutineName: String;
  ParmIDs: TSymTabEntries;
  Buffer: TString;
begin
  RoutineId := CallNode.getAttribute(ckID) as ISymTabEntry;
  RoutineName := RoutineId.getName;
  ParmIDs := TSymTabEntries(RoutineId.getAttribute(skRoutineParms));
  Buffer := TString.Create;
  //Procedure or function name
  Buffer.Append(ProgramName);
  Buffer.Append('/');
  Buffer.Append(RoutineName);
  Buffer.Append('(');
  //Parameter and return type descriptors
  if ParmIDs <> Nil then
    for ParmID in ParmIDs do
      Buffer.Append(TypeDescriptor(ParmID));
  Buffer.Append(')');
  Buffer.Append(TypeDescriptor(RoutineId));
  //Generate a call to the routine
  Emit(_INVOKESTATIC, Buffer.toString);
  if ParmIDs <> Nil then
    LocalStack.Decrease(ParmIDs.Count);
end;

//Generate code for the call epilogue.
procedure TCallDeclaredGenerator.GenerateCallEpilogue(CallNode: ICodeNode);
var
  RoutineID, FormalID, ActualID: ISymTabEntry;
  ParmsNode, ActualNode: ICodeNode;
  ActualNodes: TICodeNodeList;
  FormalIDs: TSymTabEntries;
  i: integer;
  FormalType: ITypeSpec;
begin
  RoutineID := CallNode.getAttribute(ckID) as ISymTabEntry;
  ParmsNode := CallNode.getChildren[0];
  ActualNodes := ParmsNode.getChildren;
  FormalIDs := TSymTabEntries(RoutineId.getAttribute(skRoutineParms));
  //Iterate over the formal parameters
  for i := 0 to FormalIDs.Count - 1 do begin
    FormalID := FormalIDs[i];
    ActualNode := ActualNodes[i];
    FormalType := FormalID.getTypeSpec;
    //wrapped parameters only
    if isWrapped(FormalID) then begin
      ActualID := ActualNode.getAttribute(ckID) as ISymTabEntry;
      //If the actual parameter is itself a VAR parameter, keep it wrapped
      //otherwise unwrap its value;
      if ActualID.getDefinition <> defVar_Parm then
        GenerateUnwrap(ActualID, FormalType, ProgramName)
    end;
  end;
end;

//Generate the code to unwrap an actual parameter value
procedure TCallDeclaredGenerator.GenerateUnwrap
  (ActualID: ISymTabEntry; FormalType: ITypeSpec; AProgramName: string);
var
  SymTab: ISymTab;
  NestingLevel: Integer;
  Wrapper, TypeDesc, ActualName: string;
  ActualSlot, WrapSlot: TInteger;
begin
  SymTab := ActualID.getSymTab;
  ActualSlot := TInteger(ActualID.getAttribute(skSlot));
  WrapSlot := TInteger(ActualID.getAttribute(skWrapSlot));
  TypeDesc := TypeDescriptor(FormalType);
  NestingLevel := SymTab.getNestingLevel;
  Wrapper := VarParmWrapper(FormalType);
  //load the wrapper and get its value
  EmitLoadLocal(Nil, WrapSlot.Value);
  Emit(_GETFIELD, Wrapper + '/value', TypeDesc);
  //Store the value back into the original variable.
  if NestingLevel = 1 then begin
    ActualName := AProgramName + '/' + ActualID.getName;
    Emit(_PUTSTATIC, ActualName, TypeDesc);
  end
  else
    EmitStoreLocal(FormalType, ActualSlot.Value);
  LocalStack.Use(1, 2);
end;

{ TCallStandardGenerator }

constructor TCallStandardGenerator.Create(Parent: TCodeGenerator);
begin
  inherited Create(Parent);
end;

destructor TCallStandardGenerator.Destroy;
begin
  if Assigned(ExprGenerator) then ExprGenerator.Free;
  inherited Destroy;
end;

procedure TCallStandardGenerator.Generate(Node: ICodeNode);
var
  RoutineID: ISymTabEntry;
  RoutineCode: IRoutineCode;
  ActualNode, ParmsNode: ICodeNode;
begin
  ActualNode := Nil;
  RoutineID := Node.getAttribute(ckID) as ISymTabEntry;
  RoutineCode := RoutineID.getAttribute(skRoutineCode) as IRoutineCode;
  ExprGenerator := TExpressionGenerator.Create(Self);
  //Get the actual parameters of the call.
  if Node.getChildren.Count > 0 then begin
    ParmsNode := Node.getChildren[0];
    ActualNode := ParmsNode.getChildren[0];
  end;
  case TRoutineCodeImpl.toTyp(RoutineCode) of
    rcRead, rcReadLn:   GenerateReadReadln(Node, RoutineCode);
    rcWrite, rcWriteLn: GenerateWriteWriteln(Node, RoutineCode);
    rcEOF, rcEOLN:      GenerateEofEoln(Node, RoutineCode);
    rcABS, rcSQR:       GenerateAbsSqr(RoutineCode, ActualNode);
    rcARCTAN, rcCOS, rcEXP, rcLN, rcSIN, rcSQRT:
      GenerateArctanCosExpLnSinSqrt(RoutineCode, ActualNode);
    rcPRED, rcSUCC:     GeneratePredSucc(RoutineCode, ActualNode);
    rcCHR:              GenerateChr(ActualNode);
    rcODD:              GenerateOdd(ActualNode);
    rcORD:              GenerateOrd(ActualNode);
    rcROUND, rcTRUNC:   GenerateRoundTrunc(RoutineCode, ActualNode);
  end;
end;

procedure TCallStandardGenerator.GenerateReadReadln
  (CallNode: ICodeNode; RoutineCode: IRoutineCode);
var
  ParmsNode: ICodeNode = Nil;
  ActualNode: ICodeNode;
  ProgName, StandardInName: String;
  Actuals: TICodeNodeList;
  VariableID: ISymTabEntry;
  ActualType, BaseType: ITypeSpec;
begin
  if CallNode.getChildren.Count >0 then
    ParmsNode := CallNode.getChildren[0];
  ProgName := SymTabStack.getProgramID.getName;
  StandardInName := ProgName + '/_standardIn';
  if ParmsNode <> Nil then begin
    Actuals := ParmsNode.getChildren;
    //Loop to process each actual parameter.
    for ActualNode in Actuals do begin
      VariableID := ActualNode.getAttribute(ckID) as ISymTabEntry;
      ActualType := ActualNode.getTypeSpec;
      BaseType := ActualType.baseType;
      //Generate code to call the appropriate PascalTextIn method
      Emit(_GETSTATIC, StandardInName, 'LPascalTextIn;');
      if BaseType = TPredefined.integerType then
        Emit(_INVOKEVIRTUAL, 'PascalTextIn.readInteger()I')
      else if BaseType = TPredefined.realType then
        Emit(_INVOKEVIRTUAL, 'PascalTextIn.readReal()F')
      else if BaseType = TPredefined.booleanType then
        Emit(_INVOKEVIRTUAL, 'PascalTextIn.readBoolean()Z')
      else if BaseType = TPredefined.charType then
        Emit(_INVOKEVIRTUAL, 'PascalTextIn.readChar()C');
      LocalStack.Increase(1);
      //Store the value that was read into the actual parameter.
      EmitStoreVariable(VariableID);
      LocalStack.Decrease(1);
    end;
  end;
  // READLN: Skip the rest of the input line.
  if RoutineCode = rcReadLn then begin
    Emit(_GETSTATIC, StandardInName, 'LPascalTextIn;');
    Emit(_INVOKEVIRTUAL, 'PascalTextIn.nextLine()V');
    LocalStack.Use(1);
  end;
end;

procedure TCallStandardGenerator.GenerateEofEoln
  (CallNode: ICodeNode; RoutineCode: IRoutineCode);
begin

end;

procedure TCallStandardGenerator.GenerateWriteWriteln
  (CallNode: ICodeNode; RoutineCode: IRoutineCode);
begin

end;

procedure TCallStandardGenerator.GenerateAbsSqr
  (RoutineCode: IRoutineCode; ActualNode: ICodeNode);
begin

end;

procedure TCallStandardGenerator.GenerateArctanCosExpLnSinSqrt
  (RoutineCode: IRoutineCode; ActualNode: ICodeNode);
begin

end;

procedure TCallStandardGenerator.GeneratePredSucc
  (RoutineCode: IRoutineCode; ActualNode: ICodeNode);
begin

end;

procedure TCallStandardGenerator.GenerateChr(ActualNode: ICodeNode);
begin

end;

procedure TCallStandardGenerator.GenerateOdd(ActualNode: ICodeNode);
begin

end;

procedure TCallStandardGenerator.GenerateOrd(ActualNode: ICodeNode);
begin

end;

procedure TCallStandardGenerator.GenerateRoundTrunc
  (RoutineCode: IRoutineCode; ActualNode: ICodeNode);
begin

end;


end.

