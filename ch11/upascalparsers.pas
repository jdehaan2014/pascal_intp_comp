unit uPascalParsers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPascalParserTD, uToken, uPascalTokentyp,
  uCode, uCodeFactory, uCodeImpl, uPascalErrorHandler, uPascalErrorCode,
  uGenericInterfaces, uSymtabImpl, fgl, ueoftoken, uObjectUtils, uTokenType,
  AnyObject, uPredefined, uDefinitionImpl, uTypesImpl, uTypesFactory,
  uTypeHelpers;

type

  { TStatementParser }

  TStatementParser = class(TPascalParserTD)
    protected
      procedure setLineNumber(Node: ICodeNode; Token: TToken);
      procedure ParseList(Token: TToken; ParentNode: ICodeNode;
        Terminator: TPascalTokenTyp.Values; ErrorCode: TPascalErrorCode);
      const
        //sync set for starting a statement
        StmtStartSet: TPascalTokenTypSet = [ttBegin, ttCase, ttFor, ttIf,
          ttRepeat, ttWhile, ttIdentifier, ttSemicolon];
        //sync set for following a statement
        StmtFollowSet: TPascalTokenTypSet = [ttSemicolon, ttEnd, ttElse,
          ttUntil, ttDot];

    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
  end;

  { TCompoundStatementParser }

  TCompoundStatementParser = class(TStatementParser)
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
  end;

  { TAssignmentStatementParser }

  TAssignmentStatementParser = class(TStatementParser)
    private
      const
        //sync set for the := token
        ColonEqualsSet: TPascalTokenTypSet = [ttPlus, ttMinus, ttIdentifier,
          ttInteger, ttReal, ttString, ttNot, ttLeftParen, ttColonEquals,
          ttSemicolon, ttEnd, ttElse, ttUntil, ttDot];
      var
        isFunctionTarget: boolean;
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
      function ParseFunctionNameAssignment(Token: TToken): ICodeNode;
  end;


  TOpsMap = specialize TFPGMap<TPascalTokenTyp.Values, ICodeNodeType>;

  { TExpressionParser }

  TExpressionParser = class(TStatementParser)
    public
      class constructor Create;
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
    private
      const
        RelOps: TPascalTokenTypSet = [
           ttEquals, ttNotEquals, ttLessThan,
           ttLessEquals, ttGreaterThan, ttGreaterEquals];
        AddOps: TPascalTokenTypSet = [ttPlus, ttMinus, ttOr];
        MulOps: TPascalTokenTypSet = [
          ttStar, ttSlash, ttDiv, ttMod, ttAnd];

        //sync set for starting an expression
        ExprStartSet: TPascalTokenTypSet = [ttPlus, ttMinus, ttIdentifier,
          ttInteger, ttReal, ttString, ttNot, ttLeftParen];

      class var
        RelOpsMap: TOpsMap;
        AddOpsMap: TOpsMap;
        MulOpsMap: TOpsMap;

      function ParseExpression(Token: TToken): ICodeNode;
      function ParseSimpleExpression(Token: TToken): ICodeNode;
      function ParseTerm(Token: TToken): ICodeNode;
      function ParseFactor(Token: TToken): ICodeNode;
      function ParseIdentifier(Token: TToken): ICodeNode;
  end;

  { TRepeatStatementParser }

  TRepeatStatementParser = class(TStatementParser)
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
  end;

  { TWhileStatementParser }

  TWhileStatementParser = class(TStatementParser)
    private
      const
        //sync set for DO
        DoSet: TPascalTokenTypSet = [ttBegin, ttCase, ttFor, ttIf,
          ttRepeat, ttWhile, ttIdentifier, ttSemicolon, ttDo,
          ttEnd, ttElse, ttUntil, ttDot];
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
  end;

  { TForStatementParser }

  TForStatementParser = class(TStatementParser)
    private
      const
        //sync set for TO or DOWNTO
        ToDowntoSet: TPascalTokenTypSet = [ttPlus, ttMinus, ttIdentifier,
          ttInteger, ttReal, ttString, ttNot, ttLeftParen, ttTo, ttDownto,
          ttSemicolon, ttEnd, ttElse, ttUntil, ttDot];
        //sync set for DO
        DoSet: TPascalTokenTypSet = [ttBegin, ttCase, ttFor, ttIf,
          ttRepeat, ttWhile, ttIdentifier, ttSemicolon, ttDo,
          ttEnd, ttElse, ttUntil, ttDot];
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
  end;

  { TIfStatementParser }

  TIfStatementParser = class(TStatementParser)
    private
      const
        //sync set for THEN
        ThenSet: TPascalTokenTypSet = [ttBegin, ttCase, ttFor, ttIf,
          ttRepeat, ttWhile, ttIdentifier, ttSemicolon, ttThen,
          ttEnd, ttElse, ttUntil, ttDot];
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
  end;

  { TCaseStatementParser }

  TCaseStatementParser = class(TStatementParser)
    private
      type
        TConstantSet = specialize TFPGList<TObject>;
      const
        //sync set for CASE
        ConstantStartSet: TPascalTokenTypSet = [ttIdentifier, ttInteger,
          ttPlus, ttMinus, ttString];
        //sync set for OF
        Of_Set: TPascalTokenTypSet = [ttIdentifier, ttInteger,
          ttPlus, ttMinus, ttString, ttOf,
          ttSemicolon, ttEnd, ttElse, ttUntil, ttDot];
        //sync set for COMMA
        CommaSet: TPascalTokenTypSet = [ttIdentifier, ttInteger,
          ttPlus, ttMinus, ttString, ttComma, ttColon,
          ttBegin, ttCase, ttFor, ttIf, ttRepeat, ttWhile,
          ttSemicolon, ttEnd, ttElse, ttUntil, ttDot];

        function ParseBranch(Token: TToken; expressionType: ITypeSpec;
          constantSet: TConstantSet): ICodeNode;
        procedure ParseConstantList(Token: TToken; expressionType: ITypeSpec;
          constantsNode: ICodeNode; constantSet: TConstantSet);
        function ParseConstant(Token: TToken; expressionType: ITypeSpec;
          constantSet: TConstantSet): ICodeNode;
        function ParseIdentifierConstant(Token: TToken; Sign: ITokenTyp): ICodeNode;
        function ParseIntegerConstant(Value: string; Sign: ITokenTyp): ICodeNode;
        function ParseCharConstant(Token: TToken; Value: string; Sign: ITokenTyp): ICodeNode;
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
  end;


  { TBlockParser }

  TBlockParser = class(TPascalParserTD)
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken; RoutineID: ISymTabEntry): ICodeNode;
  end;

  { TDeclarationsParser }

  TDeclarationsParser = class(TPascalParserTD)
    public
      const
        DeclarationStartSet: TPascalTokenTypSet = [ttConst, ttType, ttVar,
          ttProcedure, ttFunction, ttBegin];
        TypeStartSet: TPascalTokenTypSet = [ttType, ttVar, ttProcedure,
          ttFunction, ttBegin];
        VarStartSet: TPascalTokenTypSet = [ttVar, ttProcedure, ttFunction,
          ttBegin];
        RoutineStartSet: TPascalTokenTypSet = [ttProcedure, ttFunction, ttBegin];
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken; ParentID: ISymTabEntry): ISymTabEntry;
  end;

  { TConstantDefinitionsParser }

  TConstantDefinitionsParser = class(TDeclarationsParser)
    public
      const
        IdentifierSet: TPascalTokenTypSet = [ttType, ttVar, ttProcedure,
          ttFunction, ttBegin, ttIdentifier];
        ConstantStartSet: TPascalTokenTypSet = [ttIdentifier, ttInteger, ttReal,
          ttPlus, ttMinus, ttString, ttSemicolon];
        EqualsSet: TPascalTokenTypSet = [ttIdentifier, ttInteger, ttReal,
          ttPlus, ttMinus, ttString, ttSemicolon, ttEquals];
        NextStartSet: TPascalTokenTypSet = [ttType, ttVar, ttProcedure,
          ttFunction, ttBegin, ttSemicolon, ttEquals, ttIdentifier];

    protected
      function ParseConstant(Token: TToken): TObject;
      function ParseIdentifierConstant(Token: TToken; Sign: ITokenTyp): TObject;
      function getConstantType(Value: TObject): ITypeSpec;
      function getConstantType(Identifier: TToken): ITypeSpec;
    public
      constructor Create(Parent: TPascalParserTD);
      procedure Parse(Token: TToken);
  end;

  { TTypeDefinitionsParser }

  TTypeDefinitionsParser = class(TDeclarationsParser)
    private
      const
        IdentifierSet: TPascalTokenTypSet = [ttVar, ttProcedure, ttFunction,
          ttBegin, ttIdentifier];
        EqualsSet: TPascalTokenTypSet = [ttIdentifier, ttInteger, ttReal,
          ttPlus, ttMinus, ttString, ttSemicolon, ttEquals];
        FollowSet: TPascalTokenTypSet = [ttSemicolon];
        NextStartSet: TPascalTokenTypSet = [ttVar, ttProcedure, ttFunction,
          ttBegin, ttSemicolon, ttIdentifier];
    public
      constructor Create(Parent: TPascalParserTD);
      procedure Parse(Token: TToken);
  end;


  { TTypeSpecificationParser }

  TTypeSpecificationParser = class(TPascalParserTD)
    private
      const
        TypeStartSet: TPascalTokenTypSet = [ttIdentifier, ttInteger, ttReal,
          ttPlus, ttMinus, ttString, ttSemicolon, ttLeftParen, ttComma,
          ttArray, ttRecord];
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ITypeSpec;
  end;

  { TSimpleTypeParser }

  TSimpleTypeParser = class(TTypeSpecificationParser)
    private
      const
        SimpleTypeStartSet: TPascalTokenTypSet = [ttIdentifier, ttInteger, ttReal,
          ttPlus, ttMinus, ttString, ttSemicolon, ttLeftParen, ttComma];
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ITypeSpec;
  end;

  { TSubrangeTypeParser }

  TSubrangeTypeParser = class(TTypeSpecificationParser)
    private
      function checkValueType(Token: TToken; Value: TObject; Typ: ITypeSpec): TObject;
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ITypeSpec;
  end;

  { TEnumerationTypeParser }

  TEnumerationTypeParser = class(TTypeSpecificationParser)
    private
      const
        EnumConstantStartSet: TPascalTokenTypSet = [ttIdentifier, ttComma];
        EnumDefinitionFollowSet: TPascalTokenTypSet = [ttVar, ttProcedure,
          ttFunction, ttBegin, ttRightParen, ttSemicolon];
      procedure ParseEnumerationIdentifier(Token: TToken; Value: integer;
        EnumerationType: ITypeSpec; Constants: TSymTabEntries);
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ITypeSpec;
  end;

  { TArrayTypeParser }

  TArrayTypeParser = class(TTypeSpecificationParser)
    private
      const
        LeftBracketSet: TPascalTokenTypSet = [ttIdentifier, ttInteger, ttReal,
          ttPlus, ttMinus, ttString, ttSemicolon, ttLeftParen, ttComma,
          ttLeftBracket, ttRightBracket];
        RightBracketSet: TPascalTokenTypSet = [ttRightBracket, ttOf, ttSemicolon];
        Of_Set: TPascalTokenTypSet = [ttIdentifier, ttInteger, ttReal,
          ttPlus, ttMinus, ttString, ttSemicolon, ttLeftParen, ttComma,
          ttArray, ttRecord, ttOf];
        IndexStartSet: TPascalTokenTypSet = [ttIdentifier, ttInteger, ttReal,
          ttPlus, ttMinus, ttString, ttSemicolon, ttLeftParen, ttComma];
        IndexEndSet: TPascalTokenTypSet = [ttRightBracket, ttOf, ttSemicolon];
        IndexFollowSet: TPascalTokenTypSet = [ttIdentifier, ttInteger, ttReal,
          ttPlus, ttMinus, ttString, ttSemicolon, ttLeftParen, ttComma,
          ttRightBracket, ttOf];

      function ParseIndexTypeList(Token: TToken; ArrayType: ITypeSpec):ITypeSpec;
      procedure ParseIndexType(Token: TToken; ArrayType: ITypeSpec);
      function ParseElementType(Token: TToken): ITypeSpec;
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ITypeSpec;
  end;

  { TRecordTypeParser }

  TRecordTypeParser = class(TTypeSpecificationParser)
    private
      const
        EndSet: TPascalTokenTypSet = [ttVar, ttProcedure, ttFunction,
          ttBegin, ttEnd, ttSemicolon];
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ITypeSpec;
  end;

  { TVariableDeclarationsParser }

  TVariableDeclarationsParser = class(TDeclarationsParser)
    private
      var
        Definition: IDefinition; //how to define the identifier
      function ParseIdentifier(Token: TToken): ISymTabEntry;
    protected
      const
        IdentifierSet: TPascalTokenTypSet = [ttVar, ttProcedure, ttFunction,
          ttBegin, ttIdentifier, ttEnd, ttSemicolon];
        NextStartSet: TPascalTokenTypSet = [ttProcedure, ttFunction, ttBegin,
          ttIdentifier, ttSemicolon];
        IdentifierStartSet: TPascalTokenTypSet = [ttIdentifier, ttComma];
        IdentifierFollowSet: TPascalTokenTypSet = [ttColon, ttSemicolon,
          ttVar, ttProcedure, ttFunction, ttBegin];
        CommaSet: TPascalTokenTypSet = [ttColon, ttSemicolon, ttIdentifier,
          ttComma];
        ColonSet: TPascalTokenTypSet = [ttColon, ttSemicolon];
      procedure setDefinition(ADefinition: IDefinition);
      function ParseIdentifierSublist(Token: TToken; AFollowSet,
        ACommaSet: TPascalTokenTypSet): TSymTabEntries;
      function ParseTypeSpec(Token: TToken): ITypeSpec;
    public
      constructor Create(Parent: TPascalParserTD);
      procedure Parse(Token: TToken);
  end;

  { TVariableParser }

  TVariableParser = class(TStatementParser)
    private
      const
        SubscriptFieldStartSet: TPascalTokenTypSet = [ttLeftBracket, ttDot];
        RightBracketSet: TPascalTokenTypSet = [ttRightBracket, ttEquals, ttSemicolon];
      var
        isFunctionTarget: boolean;
      function ParseSubscripts(variableType: ITypeSpec): ICodeNode;
      function ParseField(variableType: ITypeSpec): ICodeNode;
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
      function Parse(Token: TToken; VariableID: ISymTabEntry): ICodeNode;
      function ParseFunctionNameTarget(Token: TToken): ICodeNode;
  end;

  { TProgramParser }

  TProgramParser = class(TDeclarationsParser)
    public
      const
        ProgramStartSet: TPascalTokenTypSet = [ttProgram, ttConst, ttType, ttVar,
          ttProcedure, ttFunction, ttBegin, ttSemicolon];
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken; ParentID: ISymTabEntry): ISymTabEntry;
  end;

  { TDeclaredRoutineParser }

  TDeclaredRoutineParser = class(TDeclarationsParser)
    private
      const
        ParameterSet: TPascalTokenTypSet = [ttConst, ttType, ttVar,
          ttProcedure, ttFunction, ttBegin, ttIdentifier, ttRightParen];
        LeftParenSet: TPascalTokenTypSet = [ttConst, ttType, ttVar,
          ttProcedure, ttFunction, ttBegin, ttLeftParen, ttSemicolon, ttColon];
        RightParenSet: TPascalTokenTypSet = [ttConst, ttType, ttVar,
          ttProcedure, ttFunction, ttBegin, ttSemicolon, ttColon, ttRightParen];
        ParameterFollowSet: TPascalTokenTypSet = [ttConst, ttType, ttVar,
          ttProcedure, ttFunction, ttBegin, ttSemicolon, ttColon, ttRightParen];
        CommaSet: TPascalTokenTypSet = [ttConst, ttType, ttVar,
          ttProcedure, ttFunction, ttBegin, ttComma, ttColon, ttIdentifier,
          ttSemicolon, ttRightParen];
      class var DummyCounter: integer;  //counter for dummy routine names
      function ParseRoutineName(Token: TToken; DummyName: string): ISymTabEntry;
      procedure ParseHeader(Token: TToken; RoutineID: ISymTabEntry);
      function ParseParmSublist(Token: TToken; RoutineID: ISymTabEntry
        ): TSymTabEntries;
    protected
      procedure ParseFormalParameters(Token: TToken; RoutineID: ISymTabEntry);
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken; ParentID: ISymTabEntry): ISymTabEntry;
  end;

  { TCallParser }

  TCallParser = class(TStatementParser)
    private
      const
        CommaSet: TPascalTokenTypSet = [ttPlus, ttMinus, ttIdentifier,
          ttInteger, ttReal, ttString, ttNot, ttLeftParen, ttComma, ttRightParen];
      procedure CheckActualParameter(Token: TToken; FormalID: ISymTabEntry;
        ActualNode: ICodeNode);
      function ParseWriteSpec(Token: TToken): ICodeNode;
    protected
      function ParseActualParameters(Token: TToken; pfID: ISymTabEntry;
        isDeclared, isReadReadLn, isWriteWriteLn: boolean): ICodeNode;
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
  end;

  { TCallDeclaredParser }

  TCallDeclaredParser = class(TCallParser)
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
  end;

  { TCallStandardParser }

  TCallStandardParser = class(TCallParser)
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
    private
      function ParseReadReadLn(Token: TToken; CallNode: ICodeNode;
        pfID: ISymTabEntry): ICodeNode;
      function ParseWriteWriteLn(Token: TToken; CallNode: ICodeNode;
        pfID: ISymTabEntry): ICodeNode;
      function ParseEofEoLn(Token: TToken; CallNode: ICodeNode;
        pfID: ISymTabEntry): ICodeNode;
      function ParseAbsSqr(Token: TToken; CallNode: ICodeNode;
        pfID: ISymTabEntry): ICodeNode;
      function ParseArcTanCosExpLnSinSqrt(Token: TToken; CallNode: ICodeNode;
        pfID: ISymTabEntry): ICodeNode;
      function ParsePredSucc(Token: TToken; CallNode: ICodeNode;
        pfID: ISymTabEntry): ICodeNode;
      function ParseChr(Token: TToken; CallNode: ICodeNode;
        pfID: ISymTabEntry): ICodeNode;
      function ParseOdd(Token: TToken; CallNode: ICodeNode;
        pfID: ISymTabEntry): ICodeNode;
      function ParseOrd(Token: TToken; CallNode: ICodeNode;
        pfID: ISymTabEntry): ICodeNode;
      function ParseRoundTrunc(Token: TToken; CallNode: ICodeNode;
        pfID: ISymTabEntry): ICodeNode;
      function CheckParmCount(Token: TToken; ParmsNode: ICodeNode;
        Count: integer): boolean;
  end;


implementation

{ TStatementParser }

constructor TStatementParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//parse a statement
function TStatementParser.Parse(Token: TToken): ICodeNode;
var
  StatementNode: ICodeNode;
  CompoundParser : TCompoundStatementParser;
  AssignmentParser : TAssignmentStatementParser;
  RepeatParser : TRepeatStatementParser;
  WhileParser : TWhileStatementParser;
  ForParser : TForStatementParser;
  IfParser : TIfStatementParser;
  CaseParser : TCaseStatementParser;
  Name: string; ID: ISymTabEntry; IDDefn: IDefinition;
  CallParser: TCallParser;
begin
  StatementNode := Nil;
  case TPascalTokenTyp.toTyp(Token.getTyp) of
    ttBegin:
      begin
        CompoundParser := TCompoundStatementParser.Create(Self);
        StatementNode := CompoundParser.Parse(Token);
      end;
    ttIdentifier:   //assignment statement or procedure call
      begin
        Name := Token.getText.ToLower;
        ID := SymTabStack.Lookup(Name);
        if ID <> Nil then
          IDDefn := ID.getDefinition
        else
          IDDefn := defUndefined;
        //Assignment statement or procedure call
        case TDefinitionImpl.toTyp(IDDefn) of
          defVariable, defValue_Parm, defVar_Parm, defUndefined:
            begin
              AssignmentParser := TAssignmentStatementParser.Create(Self);
              StatementNode := AssignmentParser.Parse(Token);
            end;
          defFunction:
            begin
              AssignmentParser := TAssignmentStatementParser.Create(Self);
              StatementNode := AssignmentParser.ParseFunctionNameAssignment(Token);
            end;
          defProcedure:
            begin
              CallParser := TCallParser.Create(Self);
              StatementNode := CallParser.Parse(Token);
            end;
          else begin
            ErrorHandler.Flag(Token, ecUNEXPECTED_TOKEN, Self);
            Token := NextToken;   // consume identifier
          end;
        end;
      end;
    ttRepeat:
      begin
        RepeatParser := TRepeatStatementParser.Create(Self);
        StatementNode := RepeatParser.Parse(Token);
      end;
    ttWhile:
      begin
        WhileParser := TWhileStatementParser.Create(Self);
        StatementNode := WhileParser.Parse(Token);
      end;
    ttFor:
      begin
        ForParser := TForStatementParser.Create(Self);
        StatementNode := ForParser.Parse(Token);
      end;
    ttIf:
      begin
        IfParser := TIfStatementParser.Create(Self);
        StatementNode := IfParser.Parse(Token);
      end;
    ttCase:
      begin
        CaseParser := TCaseStatementParser.Create(Self);
        StatementNode := CaseParser.Parse(Token);
      end;
    else
      StatementNode :=
        TICodeFactory.CreateICodeNode(ctNoOp);
  end;
  // Set the current line number as an attribute.
  setLineNumber(StatementNode, Token);
  Result := StatementNode;
end;

// Set the current line number as an attribute.
procedure TStatementParser.setLineNumber(Node: ICodeNode; Token: TToken);
begin
  if Node <> Nil then
    Node.setAttribute(ckLine, TInteger.Create(Token.getLineNum));
end;

//Parse a statement list.
procedure TStatementParser.ParseList(Token: TToken; ParentNode: ICodeNode;
  Terminator: TPascalTokenTyp.Values; ErrorCode: TPascalErrorCode);
var
  StatementNode: ICodeNode;
  TokenTyp: ITokenTyp;
  TerminatorSet: TPascalTokenTypSet;
begin
  TerminatorSet := StmtStartSet + [Terminator];
  // Loop to parse each statement until the END token or the end of the source file.
  while not (Token is TEOFToken) and (Token.getTyp <> Terminator) do begin
    // Parse a statement.  The parent node adopts the statement node.
    StatementNode := Parse(Token);
    ParentNode.addChild(StatementNode);
    Token := CurrentToken;
    TokenTyp := Token.getTyp;
    // Look for the semicolon between statements.
    if TokenTyp = ttSemicolon then
      Token := NextToken  // consume the ;
    // If at the start of the next  statement, then missing a semicolon.
    else if StmtStartSet.Contains(TokenTyp) then
      ErrorHandler.Flag(Token, ecMISSING_SEMICOLON, Self);
    // Sync at the start of the next statement or at the terminator
    Token := Synchronize(TerminatorSet);
  end;
  // Look for the terminator token.
  if Token.getTyp = Terminator then
    Token := NextToken   // consume terminator token
  else
    ErrorHandler.Flag(Token, ErrorCode, Self);
end;

{ TCompoundStatementParser }

constructor TCompoundStatementParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//Parse a compound statement.
function TCompoundStatementParser.Parse(Token: TToken): ICodeNode;
var
  CompoundNode: ICodeNode;
  StatementParser: TStatementParser;
begin
  Token := NextToken;   // consume Begin
  // create a compound node
  CompoundNode := TICodeFactory.CreateICodeNode(ctCompound);

  // Parse the statement list terminated by the END token.
  StatementParser := TStatementParser.Create(Self);
  StatementParser.ParseList(Token, CompoundNode, ttEnd, ecMISSING_END);

  Result := CompoundNode;
end;

{ TAssignmentStatementParser }

constructor TAssignmentStatementParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
  isFunctionTarget := False;
end;

//Parse an assignment statement.
function TAssignmentStatementParser.Parse(Token: TToken): ICodeNode;
var
  AssignNode, TargetNode, ExprNode: ICodeNode;
  ExpressionParser: TExpressionParser;
  VariableParser: TVariableParser;
  TargetType, ExprType: ITypeSpec;
begin
  // Create the ASSIGN node.
  AssignNode := TICodeFactory.CreateICodeNode(ctAssign);
  //Parse the target variable
  VariableParser := TVariableParser.Create(Self);
  if isFunctionTarget then
    TargetNode := VariableParser.ParseFunctionNameTarget(Token)
  else
    TargetNode := VariableParser.Parse(Token);
  if TargetNode <> Nil then
    TargetType := TargetNode.getTypeSpec
  else
    TargetType := TPredefined.undefinedType;

  // The ASSIGN node adopts the variable node as its first child.
  AssignNode.addChild(TargetNode);

  // Synchronize on the := token.
  Token := Synchronize(ColonEqualsSet);
  if Token.getTyp = ttColonEquals then
    Token := NextToken   // consume :=
  else
    ErrorHandler.Flag(Token, ecMISSING_COLON_EQUALS, Self);

  // Parse the expression.  The ASSIGN node adopts the expression's
  // node as its second child.
  ExpressionParser := TExpressionParser.Create(Self);
  ExprNode := ExpressionParser.Parse(Token);
  AssignNode.addChild(ExprNode);

  // Type check: Assignment compatible?
  if ExprNode <> Nil then
    ExprType := ExprNode.getTypeSpec
  else
    ExprType := TPredefined.undefinedType;
  if not TTypeChecker.areAssignmentCompatible(TargetType, ExprType) then
    ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);

  AssignNode.setTypeSpec(TargetType);
  Result := AssignNode;
end;

//Parse an assignment to a function name
function TAssignmentStatementParser.ParseFunctionNameAssignment(Token: TToken): ICodeNode;
begin
  isFunctionTarget := True;
  Result := Parse(Token);
end;


{ TExpressionParser }

class constructor TExpressionParser.Create;
begin
  RelOpsMap := TOpsMap.Create;
  RelOpsMap.Add(ttEquals, ctEQ); // ct = CodenodeType
  RelOpsMap.Add(ttNotEquals, ctNE);
  RelOpsMap.Add(ttLessThan, ctLT);
  RelOpsMap.Add(ttLessEquals, ctLE);
  RelOpsMap.Add(ttGreaterThan, ctGT);
  RelOpsMap.Add(ttGreaterEquals, ctGE);

  AddOpsMap := TOpsMap.Create;
  AddOpsMap.Add(ttPlus, ctAdd);
  AddOpsMap.Add(ttMinus, ctSubtract);
  AddOpsMap.Add(ttOr, ctOr);

  MulOpsMap := TOpsMap.Create;
  MulOpsMap.Add(ttStar, ctMultiply);
  MulOpsMap.Add(ttSlash, ctFloatDivide);
  MulOpsMap.Add(ttDiv, ctIntegerDivide);
  MulOpsMap.Add(ttMod, ctMod);
  MulOpsMap.Add(ttAnd, ctAnd);
end;

constructor TExpressionParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//Parse an expression.
function TExpressionParser.Parse(Token: TToken): ICodeNode;
begin
  Result := ParseExpression(Token);
end;

//Parse an expression.
function TExpressionParser.ParseExpression(Token: TToken): ICodeNode;
var
  RootNode, OpNode, SimExprNode: ICodeNode;
  TokenTyp: ITokenTyp;
  NodeType: ICodeNodeType;
  ResultType, SimExprType: ITypeSpec;
begin
  //Parse a simple expression and make the root of its tree the root node.
  RootNode := ParseSimpleExpression(Token);
  if RootNode <> Nil then
    ResultType := RootNode.getTypeSpec
  else
    ResultType := TPredefined.UndefinedType;

  Token := CurrentToken;
  TokenTyp := Token.getTyp;
  //Look for a relational operator.
  if RelOps.Contains(TokenTyp) then begin  // see typehelper TPascalTokenTypSet
    //Create a new operator node and adopt the current tree as its first child.
    NodeType := RelOpsMap.KeyData[TPascalTokenTyp.toTyp(TokenTyp)];
    OpNode := TICodeFactory.CreateICodeNode(NodeType);
    OpNode.addChild(RootNode);
    Token := NextToken;  // consume the operator
    //Parse the second simple expression.  The operator node adopts
    //the simple expression's tree as its second child
    SimExprNode := ParseSimpleExpression(Token);
    OpNode.addChild(SimExprNode);
    //The operator node becomes the new root node.
    RootNode := OpNode;

    //Type check: Operands must be comparison compatible
    if SimExprNode <> Nil then
      SimExprType := SimExprNode.getTypeSpec
    else
      SimExprType := TPredefined.UndefinedType;
    if TTypeChecker.areComparisonCompatible(ResultType, SimExprType) then
      ResultType := TPredefined.BooleanType
    else begin
      ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
      ResultType := TPredefined.UndefinedType;
    end;
  end;
  if RootNode <> Nil then
    RootNode.setTypeSpec(ResultType);
  Result := RootNode;
end;

//Parse a simple expression.
function TExpressionParser.ParseSimpleExpression(Token: TToken): ICodeNode;
var
  RootNode, OpNode, NegateNode, TermNode: ICodeNode;
  TokenTyp, SignTyp, OperatorTyp: ITokenTyp;
  NodeType: ICodeNodeType;
  SignToken: TToken = Nil;
  ResultType, TermType: ITypeSpec;
begin
  SignTyp := ttNone;    // type of leading sign (if any)
  //Look for a leading + or - sign.
  TokenTyp := Token.getTyp;
  if (TokenTyp = ttPlus) or (TokenTyp = ttMinus) then begin
    SignTyp := TokenTyp;
    SignToken := Token;
    Token := NextToken;        // consume + or -
  end;
  //Parse a term and make the root of its tree the root node.
  RootNode := ParseTerm(Token);
  if RootNode <> Nil then
    ResultType := RootNode.getTypeSpec
  else
    ResultType := TPredefined.UndefinedType;
  //Type check - leading sign
  if (SignTyp <> ttNone) and (not TTypeChecker.isIntegerOrReal(ResultType)) then
    ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
  // Was there a leading - sign?
  if SignTyp = ttMinus then begin
    //Create a NEGATE node and adopt the current tree as its child.
    NegateNode := TICodeFactory.CreateICodeNode(ctNegate);
    NegateNode.addChild(RootNode);
    NegateNode.setTypeSpec(RootNode.getTypeSpec);
    //The NEGATE node becomes the new root node.
    RootNode := NegateNode;
  end;
  Token := CurrentToken;
  TokenTyp := Token.getTyp;
  //Loop over additive operators.
  while AddOps.Contains(TokenTyp) do begin
    OperatorTyp := TokenTyp;
    //Create a new operator node and adopt the current tree as its first child.
    NodeType := AddOpsMap.KeyData[TPascalTokenTyp.toTyp(OperatorTyp)];
    OpNode := TICodeFactory.CreateICodeNode(NodeType);
    OpNode.addChild(RootNode);
    Token := NextToken;  // consume the operator
    //Parse another term. The operator node adopts the term's tree as its second child.
    TermNode := ParseTerm(Token);
    OpNode.addChild(TermNode);
    if TermNode <> Nil then
      TermType := TermNode.getTypeSpec
    else
      TermType := TPredefined.UndefinedType;
    //The operator node becomes the new root node.
    RootNode := OpNode;
    //Determine the result type
    case TPascalTokenTyp.toTyp(OperatorTyp) of
      ttPlus, ttMinus:
          //both operands integer ==> integer result
          if TTypeChecker.areBothInteger(ResultType, TermType) then
            ResultType := TPredefined.integerType
          //both operands real or one real ==> real result
          else if TTypeChecker.isAtLeastOneReal(ResultType, TermType) then
            ResultType := TPredefined.realType
          else
            ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
      ttOr:
          //both operands boolean ==> boolean result
          if TTypeChecker.areBothBoolean(ResultType, TermType) then
            ResultType := TPredefined.booleanType
          else
            ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
    end;
    RootNode.setTypeSpec(ResultType);
    Token := CurrentToken;
    TokenTyp := Token.getTyp;
  end;
  Result := RootNode;
end;

//Parse a term.
function TExpressionParser.ParseTerm(Token: TToken): ICodeNode;
var
  RootNode, OpNode, FactorNode: ICodeNode;
  TokenTyp, OperatorTyp: ITokenTyp;
  NodeType: ICodeNodeType;
  ResultType, FactorType: ITypeSpec;
begin
  //Parse a factor and make its node the root node.
  RootNode := ParseFactor(Token);
  if RootNode <> Nil then
    ResultType := RootNode.getTypeSpec
  else
    ResultType := TPredefined.UndefinedType;
  Token := CurrentToken;
  TokenTyp := Token.getTyp;
  //Loop over multiplicative operators.
  while MulOps.Contains(TokenTyp) do begin
    OperatorTyp := TokenTyp;
    //Create a new operator node and adopt the current tree as its first child.
    NodeType := MulOpsMap.KeyData[TPascalTokenTyp.toTyp(OperatorTyp)];
    OpNode := TICodeFactory.CreateICodeNode(NodeType);
    OpNode.addChild(RootNode);
    Token := NextToken;  // consume the operator
    //Parse another factor. The operator node adopts the term's tree as its second child.
    FactorNode := ParseFactor(Token);
    OpNode.addChild(FactorNode);
    if FactorNode <> Nil then
      FactorType := FactorNode.getTypeSpec
    else
      FactorType := TPredefined.UndefinedType;

    //The operator node becomes the new root node.
    RootNode := OpNode;
    //Determine the result type
    case TPascalTokenTyp.toTyp(OperatorTyp) of
      ttStar:
          //both operands integer ==> integer result
          if TTypeChecker.areBothInteger(ResultType, FactorType) then
            ResultType := TPredefined.integerType
          //both operands real or one real ==> real result
          else if TTypeChecker.isAtLeastOneReal(ResultType, FactorType) then
            ResultType := TPredefined.realType
          else
            ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
      ttSlash:
          //all integer and real operand combinations
          if TTypeChecker.areBothInteger(ResultType, FactorType) or
             TTypeChecker.isAtLeastOneReal(ResultType, FactorType) then
            ResultType := TPredefined.realType
          else
            ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
      ttDiv, ttMod:
        //both operands integer ==> integer result
        if TTypeChecker.areBothInteger(ResultType, FactorType) then
          ResultType := TPredefined.integerType
        else
          ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
      ttAnd:
          //both operands boolean ==> boolean result
          if TTypeChecker.areBothBoolean(ResultType, FactorType) then
            ResultType := TPredefined.booleanType
          else
            ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
    end;
    RootNode.setTypeSpec(ResultType);

    Token := CurrentToken;
    TokenTyp := Token.getTyp;
  end;
  Result := RootNode;
end;

//Parse a factor.
function TExpressionParser.ParseFactor(Token: TToken): ICodeNode;
var
  RootNode, FactorNode: ICodeNode;
  TokenTyp: ITokenTyp;
  ResultType, FactorType: ITypeSpec;
  Value: TObject;
  strValue: string;
begin
  TokenTyp := Token.getTyp;
  RootNode := Nil;
  case TPascalTokenTyp.toTyp(TokenTyp) of
    ttIdentifier: RootNode := ParseIdentifier(Token);
    ttInteger:
      begin
        // Create an INTEGER_CONSTANT node as the root node.
        RootNode := TICodeFactory.CreateICodeNode(ctIntegerConstant);
        RootNode.setAttribute(ckValue, Token.getValue);
        Token := NextToken;  // consume the number
        RootNode.setTypeSpec(TPredefined.integerType);
      end;
    ttReal:
      begin
        // Create an REAL_CONSTANT node as the root node.
        RootNode := TICodeFactory.CreateICodeNode(ctRealConstant);
        RootNode.setAttribute(ckValue, Token.getValue);
        Token := NextToken;  // consume the number
        RootNode.setTypeSpec(TPredefined.realType);
      end;
    ttString:
      begin
        value := Token.getValue;
        // Create an STRING_CONSTANT node as the root node.
        RootNode := TICodeFactory.CreateICodeNode(ctStringConstant);
        RootNode.setAttribute(ckValue, Value);
        strValue := TString(Value).Value;
        if Length(strValue) = 1 then
          ResultType := TPredefined.charType
        else
          ResultType := TTypeFactory.CreateStringType(strValue);
        Token := NextToken;  // consume the string
        RootNode.setTypeSpec(ResultType);
      end;
    ttNot:
      begin
        Token := NextToken;    // consume the NOT
        // Create a NOT node as the root node.
        RootNode := TICodeFactory.CreateICodeNode(ctNot);
        //Parse the factor. The NOT node adopts the factor node as its child.
        FactorNode := ParseFactor(Token);
        RootNode.addChild(FactorNode);
        if FactorNode <> Nil then
          FactorType := FactorNode.getTypeSpec
        else
          FactorType := TPredefined.UndefinedType;
        if not TTypeChecker.isBoolean(FactorType) then
          ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
        RootNode.setTypeSpec(TPredefined.booleanType);
      end;
    ttLeftParen:
      begin
        Token := NextToken;    //consume '('
        // Parse an expression and make its node the root node.
        RootNode := ParseExpression(Token);
        if RootNode <> Nil then
          ResultType := RootNode.getTypeSpec
        else
          ResultType := TPredefined.UndefinedType;
        // Look for the matching ) token.
        Token := CurrentToken;
        if Token.getTyp = ttRightParen then
          Token := NextToken       //consume ')'
        else
          ErrorHandler.Flag(Token, ecMISSING_RIGHT_PAREN, Self);
        RootNode.setTypeSpec(ResultType);
      end;
    else
      ErrorHandler.Flag(Token, ecUNEXPECTED_TOKEN, Self);
  end;
  Result := RootNode;
end;

//Parse an identifier
function TExpressionParser.ParseIdentifier(Token: TToken): ICodeNode;
var
  RootNode: ICodeNode = Nil;
  Name: string;
  ID: ISymTabEntry;
  defnCode: IDefinition;
  Value: TObject;
  Typ: ITypeSpec;
  VariableParser: TVariableParser;
  CallParser: TCallParser;
begin
  //Look up the identifier in the symbol table stack.
  //Flag the identifier as undefined if it's not found.
  Name := Token.getText.toLower;
  ID := SymTabStack.Lookup(Name);
  if ID = Nil then begin    //undefined
    ErrorHandler.Flag(Token, ecIDENTIFIER_UNDEFINED, Self);
    ID := SymTabStack.EnterLocal(Name);
    ID.setDefinition(defUndefined);
    ID.setTypeSpec(TPredefined.UndefinedType);
  end;
  defnCode := ID.getDefinition;
  case TDefinitionImpl.toTyp(defnCode) of
    defConstant:
      begin
        Value := ID.getAttribute(skConstantValue);
        Typ := ID.getTypeSpec;
        if Value is TInteger then begin
          RootNode := TICodeFactory.CreateICodeNode(ctIntegerConstant);
          RootNode.setAttribute(ckValue, Value);
        end
        else if Value is TFloat then begin
          RootNode := TICodeFactory.CreateICodeNode(ctRealConstant);
          RootNode.setAttribute(ckValue, Value);
        end
        else if Value is TString then begin
          RootNode := TICodeFactory.CreateICodeNode(ctStringConstant);
          RootNode.setAttribute(ckValue, Value);
        end;
        ID.AppendLineNumber(Token.getLineNum);
        Token := NextToken;  // consume the constant identifier
        if RootNode <> Nil then
          RootNode.setTypeSpec(Typ);
      end;
    defEnumeration_Constant:
      begin
        Value := ID.getAttribute(skConstantValue);
        Typ := ID.getTypeSpec;
        RootNode := TICodeFactory.CreateICodeNode(ctIntegerConstant);
        RootNode.setAttribute(ckValue, Value);
        ID.AppendLineNumber(Token.getLineNum);
        Token := NextToken;  // consume the enum constant identifier
        RootNode.setTypeSpec(Typ);
      end;
    defFunction:
      begin
        CallParser := TCallParser.Create(Self);
        RootNode := CallParser.Parse(Token);
      end
    else begin
        VariableParser := TVariableParser.Create(Self);
        RootNode := VariableParser.Parse(Token, ID);
      end;
  end;
  Result := RootNode;
end;

{ TRepeatStatementParser }

constructor TRepeatStatementParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

function TRepeatStatementParser.Parse(Token: TToken): ICodeNode;
var
  loopNode, testNode, ExprNode: ICodeNode;
  StatementParser: TStatementParser;
  ExpressionParser: TExpressionParser;
  ExprType: ITypeSpec;
begin
  Token := nextToken;   //consume REPEAT
  //Create the LOOP and TEST nodes
  loopNode := TICodeFactory.CreateICodeNode(ctLoop);
  testNode := TICodeFactory.CreateICodeNode(ctTest);
  //Parse the statement list terminated by the UNTIL token
  //The LOOP node is the parent of the statement subtrees
  StatementParser := TStatementParser.Create(Self);
  StatementParser.ParseList(Token, loopNode, ttUntil, ecMISSING_UNTIL);
  Token := currentToken;
  //Parse the expression
  //The TEST node adopts the expression subtree as its only child
  //The LOOP node adopts the TEST node
  ExpressionParser := TExpressionParser.Create(Self);
  ExprNode := ExpressionParser.Parse(Token);
  testNode.addChild(ExprNode);
  loopNode.addChild(testNode);
  //Type check: The test expression must be boolean.
  if ExprNode <> Nil then
    ExprType := ExprNode.getTypeSpec
  else
    ExprType := TPredefined.UndefinedType;
  if not TTypeChecker.isBoolean(ExprType) then
    ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
  Result := loopNode;
end;

{ TWhileStatementParser }

constructor TWhileStatementParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//parse a WHILE statement
function TWhileStatementParser.Parse(Token: TToken): ICodeNode;
var
  loopNode, breakNode, notNode, ExprNode: ICodeNode;
  StatementParser: TStatementParser;
  ExpressionParser: TExpressionParser;
  ExprType: ITypeSpec;
begin
  Token := nextToken;   //consume WHILE
  //Create the LOOP and TEST and NOT nodes
  loopNode := TICodeFactory.CreateICodeNode(ctLoop);
  breakNode := TICodeFactory.CreateICodeNode(ctTest);
  notNode := TICodeFactory.CreateICodeNode(ctNot);
  // The LOOP node adopts the TEST node as its first child.
  // The TEST node adopts the NOT node as its only child.
  loopNode.addChild(breakNode);
  breakNode.addChild(notNode);
  // Parse the expression.
  // The NOT node adopts the expression subtree as its only child.
  ExpressionParser := TExpressionParser.Create(Self);
  ExprNode := ExpressionParser.Parse(Token);
  notNode.addChild(ExprNode);
  //Type check: The test expression must be boolean.
  if ExprNode <> Nil then
    ExprType := ExprNode.getTypeSpec
  else
    ExprType := TPredefined.UndefinedType;
  if not TTypeChecker.isBoolean(ExprType) then
    ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
  // Synchronize at the DO.
  Token := Synchronize(DoSet);
  if Token.getTyp = ttDo then
    Token := nextToken
  else
    ErrorHandler.Flag(Token, ecMISSING_DO, Self);
  // Parse the statement.
  // The LOOP node adopts the statement subtree as its second child.
  StatementParser := TStatementParser.Create(Self);
  loopNode.addChild(StatementParser.Parse(Token));
  Result := loopNode;
end;

{ TForStatementParser }

constructor TForStatementParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

function TForStatementParser.Parse(Token: TToken): ICodeNode;
var
  TargetToken: TToken;
  loopNode, testNode, compoundNode,
  initAssignNode, RelOpNode, controlVarNode, nextAssignNode,
  arithOpNode, oneNode, ExprNode: ICodeNode;
  AssignmentParser: TAssignmentStatementParser;
  StatementParser: TStatementParser;
  ExpressionParser: TExpressionParser;
  Direction: ITokenTyp;
  ControlType, ExprType: ITypeSpec;
begin
  Token := nextToken;  // consume the FOR
  TargetToken := Token;
  //Create the COMPOUND, LOOP and TEST nodes
  compoundNode := TICodeFactory.CreateICodeNode(ctCompound);
  loopNode := TICodeFactory.CreateICodeNode(ctLoop);
  testNode := TICodeFactory.CreateICodeNode(ctTest);
  //Parse the embedded initial assignment
  AssignmentParser := TAssignmentStatementParser.Create(Self);
  initAssignNode := AssignmentParser.Parse(Token);
  if initAssignNode <> Nil then
    ControlType := initAssignNode.getTypeSpec
  else
    ControlType := TPredefined.UndefinedType;
  //Set the current line number attribute
  setLineNumber(initAssignNode, TargetToken);
  //Type check: The control variable's type must be integer or enumeration.
  if (not TTypeChecker.isInteger(ControlType)) and
     (controlType.getForm <> tfEnumeration) then
    ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
  //The COMPOUND node adopts the initial ASSIGN and the LOOP nodes
  //as its first and second children
  compoundNode.addChild(initAssignNode);
  compoundNode.addChild(loopNode);
  //Synchronize at the TO or DOWNTO
  Token := Synchronize(ToDowntoSet);
  Direction := Token.getTyp;
  //Look for TO or DOWNTO
  if (Direction = ttTO) or (Direction = ttDOWNTO) then
    Token := nextToken  //consume TO or DOWNTO
  else
    ErrorHandler.Flag(Token, ecMISSING_TO_DOWNTO, Self);
  //Create a relational operator node: GT for TO; LT for DOWNTO
  if Direction = ttTO then
    RelOpNode := TICodeFactory.CreateICodeNode(ctGT)
  else
    RelOpNode := TICodeFactory.CreateICodeNode(ctLT);
  RelOpNode.setTypeSpec(TPredefined.booleanType);
  //Copy the control VARIABLE node. The relational operator node adopts
  //the copied VARIABLE node as its first child
  controlVarNode := initAssignNode.getChildren[0];
  RelOpNode.addChild(controlVarNode.copyNode);
  //Parse the termination expression. The relational operator node
  //adopts the expression as its second child.
  ExpressionParser := TExpressionParser.Create(Self);
  ExprNode := ExpressionParser.Parse(Token);
  RelOpNode.addChild(ExprNode);
  //Type check: The termination expression type must be assignment
  //             compatible with the control variable's type.
  if ExprNode <> Nil then
    ExprType := ExprNode.getTypeSpec
  else
    ExprType := TPredefined.UndefinedType;
  if not TTypeChecker.areAssignmentCompatible(ControlType, ExprType) then
    ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);

  //The TEST node adopts the relational operator as its only child.
  //The LOOP node adopts the TEST node as its first child.
  testNode.addChild(RelOpNode);
  loopNode.addChild(testNode);
  //Sync at the DO
  Token := Synchronize(DoSet);
  if Token.getTyp = ttDo then
    Token := nextToken  //consume DO
  else
    ErrorHandler.Flag(Token, ecMISSING_DO, Self);
  //Parse the nested statement. The LOOP node adopts the statement
  //node as its second child.
  StatementParser := TStatementParser.Create(Self);
  loopNode.addChild(StatementParser.Parse(Token));
  //Create an assignment with a copy of the control variable to advance
  //the value of the variable.
  nextAssignNode := TICodeFactory.CreateICodeNode(ctAssign);
  nextAssignNode.setTypeSpec(ControlType);
  nextAssignNode.addChild(ControlVarNode.copyNode);
  //Create arithmetic operator node. ADD for TO, SUBTRACT for DOWNTO
  if Direction = ttTO then
    arithOpNode := TICodeFactory.CreateICodeNode(ctAdd)
  else
    arithOpNode := TICodeFactory.CreateICodeNode(ctSubtract);
  arithOpNode.setTypeSpec(TPredefined.integerType);
  //The operator node adopts a copy of the loop variable as its
  //first child and the value 1 as its second child.
  arithOpNode.addChild(controlVarNode.copyNode);
  oneNode := TICodeFactory.CreateICodeNode(ctIntegerConstant);
  oneNode.setAttribute(ckValue, TInteger.Create(1));
  oneNode.setTypeSpec(TPredefined.integerType);
  arithOpNode.addChild(oneNode);
  //The next ASSIGN node adopts the arithmetic operator node as its second
  //child. The loop node adopts the next ASSIGN node as its third child.
  nextAssignNode.addChild(arithOpNode);
  loopNode.addChild(nextAssignNode);
  //Set the current line number attribute
  setLineNumber(nextAssignNode, TargetToken);
  Result := compoundNode;
end;

{ TIfStatementParser }

constructor TIfStatementParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

function TIfStatementParser.Parse(Token: TToken): ICodeNode;
var
  ifNode, ExprNode: ICodeNode;
  StatementParser: TStatementParser;
  ExpressionParser: TExpressionParser;
  ExprType: ITypeSpec;
begin
  Token := nextToken;    //consume the IF
  //Create the IF node
  ifNode := TICodeFactory.CreateICodeNode(ctIf);
  //Parse the expression
  //The IF node adopts the expression as its first child
  ExpressionParser := TExpressionParser.Create(Self);
  ExprNode := ExpressionParser.Parse(Token);
  ifNode.addChild(ExprNode);
  //Type check: The expression type must be boolean.
  if ExprNode <> Nil then
    ExprType := ExprNode.getTypeSpec
  else
    ExprType := TPredefined.UndefinedType;
  if not TTypeChecker.isBoolean(ExprType) then
    ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
  //Sync at the THEN
  Token := Synchronize(ThenSet);
  if Token.getTyp = ttThen then
    Token := nextToken  //consume THEN
  else
    ErrorHandler.Flag(Token, ecMISSING_THEN, Self);
  //Parse the THEN statement
  //The IF nodeadopts the statement subtree as its second child.
  StatementParser := TStatementParser.Create(Self);
  ifNode.addChild(StatementParser.Parse(Token));
  Token := currentToken;
  //Look for an ELSE
  if Token.getTyp = ttElse then begin
    Token := nextToken;
    //Parse the ELSE statement.
    //The IF node adopts the statement subtree as its third child.
    ifNode.addChild(StatementParser.Parse(Token));
  end;
  Result := ifNode;
end;

{ TCaseStatementParser }

//Parse a CASE branch
function TCaseStatementParser.ParseBranch(Token: TToken; expressionType: ITypeSpec;
  constantSet: TConstantSet): ICodeNode;
var
  branchNode, constantsNode: ICodeNode;
  StatementParser: TStatementParser;
begin
  // Create an SELECT_BRANCH node and a SELECT_CONSTANTS node.
  // The SELECT_BRANCH node adopts the SELECT_CONSTANTS node as its first child.
  branchNode := TICodeFactory.CreateICodeNode(ctSelectBranch);
  constantsNode := TICodeFactory.CreateICodeNode(ctSelectConstants);
  branchNode.addChild(constantsNode);
  // Parse the list of CASE branch constants.
  // The SELECT_CONSTANTS node adopts each constant.
  ParseConstantList(Token, expressionType, constantsNode, constantSet);
  // Look for the : token.
  Token := currentToken;
  if Token.getTyp = ttColon then
    Token := nextToken  // consume the :
  else
    ErrorHandler.Flag(Token, ecMISSING_COLON, Self);
  // Parse the CASE branch statement. The SELECT_BRANCH node adopts
  // the statement subtree as its second child.
  StatementParser := TStatementParser.Create(Self);
  branchNode.addChild(StatementParser.Parse(Token));
  Result := branchNode;
end;

//parse a list of CASE branch constants
procedure TCaseStatementParser.ParseConstantList(Token: TToken; expressionType: ITypeSpec;
  constantsNode: ICodeNode; constantSet: TConstantSet);
begin
  //Loop to parse each constant
  while ConstantStartSet.Contains(TPascalTokenTyp.toTyp(Token.getTyp)) do begin
    //The constants list node adopts the constant node
    constantsNode.addChild(parseConstant(Token, expressionType, constantSet));

    // Synchronize at the comma between constants.
    Token := Synchronize(CommaSet);

    // Look for the comma.
    if Token.getTyp = ttComma then
      Token := nextToken  // consume the ,

    // If at the start of the next constant, then missing a comma.
    else if ConstantStartSet.Contains(TPascalTokenTyp.toTyp(Token.getTyp)) then
      ErrorHandler.Flag(Token, ecMISSING_COMMA, Self);
  end;
end;

//parse a CASE branch constant
function TCaseStatementParser.ParseConstant(Token: TToken; expressionType: ITypeSpec;
  constantSet: TConstantSet): ICodeNode;
var
  Sign: ITokenTyp;
  TokenTyp: ITokenTyp;
  constantNode: ICodeNode = Nil;
  Value: TObject;
  i, index: integer;
  ConstantType: ITypeSpec = Nil;
begin
  Sign := ttNone;
  //Synchronize at th start of a constant
  Token := Synchronize(ConstantStartSet);
  TokenTyp := Token.getTyp;
  if (TokenTyp = ttPlus) or (TokenTyp = ttMinus) then begin
    Sign := TokenTyp;
    Token := nextToken;
  end;
  //Parse the constant
  case TPascalTokenTyp.toTyp(Token.getTyp) of
    ttIdentifier:
      begin
        constantNode := parseIdentifierConstant(Token, Sign);
        if constantNode <> Nil then
          ConstantType := ConstantNode.getTypeSpec;
      end;
    ttInteger:
      begin
        constantNode := parseIntegerConstant(Token.getText, Sign);
        constantType := TPredefined.integerType;
      end;
    ttString:
      begin
        constantNode := parseCharConstant(Token, Token.getValue.toString, Sign);
        constantType := TPredefined.charType;
      end
    else ErrorHandler.Flag(Token, ecINVALID_CONSTANT, Self);
  end;
  //Check for reused constants
  if constantNode <> Nil then begin
    Value := constantNode.getAttribute(ckValue);

    index := -1;
    for i := 0 to constantSet.Count-1 do begin
      if TObject(constantSet[i]).toString = Value.toString then begin
        index := i;
        break;
      end;
    end;

    if index < 0 then
      constantSet.Add(Value)
    else
      ErrorHandler.Flag(Token, ecCASE_CONSTANT_REUSED, Self);
  end;
  // Type check: The constant type must be comparison compatible
  //             with the CASE expression type.
  if Not TTypeChecker.areComparisonCompatible(expressionType, constantType) then
    ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);

  nextToken;  // consume the constant
  constantNode.setTypeSpec(constantType);
  Result := constantNode;
end;

//parse a IDENTIFIER case constant
function TCaseStatementParser.ParseIdentifierConstant(Token: TToken;
  Sign: ITokenTyp): ICodeNode;
var
  constantNode: ICodeNode = Nil;
  constantType: ITypeSpec = Nil;
  Name: String;
  ID: ISymTabEntry;
  defnCode: IDefinition;
  constantValue: TObject;
begin
  //Look up the identifier in the symbol table stack.
  Name := Token.getText.toLower;
  ID := symTabStack.Lookup(Name);
  //Undefined.
  if ID = Nil then begin
    ID := symTabStack.enterLocal(Name);
    ID.setDefinition(defUndefined);
    ID.setTypeSpec(TPredefined.undefinedType);
    ErrorHandler.Flag(Token, ecIDENTIFIER_UNDEFINED, Self);
    Exit(Nil);
  end;
  defnCode := ID.getDefinition;
  //Constant identifier.
  if (defnCode = defConstant) or (defnCode = defEnumeration_Constant) then begin
    constantValue := ID.getAttribute(skConstantValue);
    constantType := ID.getTypeSpec;
    //Type check: Leading sign permitted only for integer constants.
    if (Sign <> ttNone) and not TTypeChecker.isInteger(constantType) then
      ErrorHandler.Flag(Token, ecINVALID_CONSTANT, Self);

    constantNode := TICodeFactory.CreateICodeNode(ctIntegerConstant);
    constantNode.setAttribute(ckValue, constantValue);
  end;
  ID.appendLineNumber(token.getLineNum);

  if constantNode <> Nil then
    constantNode.setTypeSpec(constantType);
  Result := constantNode;
end;

//parse a INTEGER case constant
function TCaseStatementParser.ParseIntegerConstant(Value: string;
  Sign: ITokenTyp): ICodeNode;
var
  constantNode: ICodeNode;
  intValue: integer;
begin
  constantNode := TICodeFactory.CreateICodeNode(ctIntegerConstant);
  intValue := TInteger.parseInt(value);
  if Sign = ttMinus then
    intValue := - intValue;
  constantNode.setAttribute(ckValue, TInteger.Create(intValue));
  Result := constantNode;
end;

//parse a character CASE constant
function TCaseStatementParser.ParseCharConstant(Token: TToken; Value: string;
  Sign: ITokenTyp): ICodeNode;
var
  constantNode: ICodeNode = Nil;
begin
  if Sign <> ttNone then
    ErrorHandler.Flag(Token, ecINVALID_CONSTANT, Self)
  else begin
    if Length(Value) = 1 then begin
      constantNode := TICodeFactory.CreateICodeNode(ctStringConstant);
      constantNode.setAttribute(ckValue, TString.Create(Value));
    end
    else
      ErrorHandler.Flag(Token, ecINVALID_CONSTANT, Self)
  end;
  Result := constantNode;
end;

constructor TCaseStatementParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

function TCaseStatementParser.Parse(Token: TToken): ICodeNode;
var
  selectNode, exprNode: ICodeNode;
  ExpressionParser: TExpressionParser;
  ConstantSet: TConstantSet;
  tokenTyp: ITokenTyp;
  exprType: ITypeSpec;
begin
  Token := nextToken;  // consume CASE
  // Create a SELECT node.
  selectNode := TICodeFactory.CreateICodeNode(ctSelect);
  // Parse the CASE expression.
  // The SELECT node adopts the expression subtree as its first child.
  ExpressionParser := TExpressionParser.Create(Self);
  ExprNode := ExpressionParser.Parse(Token);
  selectNode.addChild(ExprNode);
  //Type check: The CASE expression's type must be integer, character,
  //            or enumeration
  if ExprNode <> Nil then
    ExprType := ExprNode.getTypeSpec
  else
    ExprType := TPredefined.UndefinedType;
  if not TTypeChecker.isInteger(ExprType) and
     not TTypeChecker.isChar(ExprType) and (exprType.getForm <> tfEnumeration)
     then
       ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
  // Synchronize at the OF.
  Token := Synchronize(Of_Set);
  if Token.getTyp = ttOf then
    token := nextToken()   // consume the OF
  else
    ErrorHandler.Flag(Token, ecMISSING_OF, Self);
  // Set of CASE branch constants.
  ConstantSet := TConstantSet.Create;
  // Loop to parse each CASE branch until the END token
  // or the end of the source file.
  while not (Token is TEofToken) and (Token.getTyp <> ttEnd) do begin
    // The SELECT node adopts the CASE branch subtree.
    selectNode.addChild(ParseBranch(Token, exprType, constantSet));
    Token := currentToken;
    TokenTyp := Token.getTyp;
    // Look for the semicolon between CASE branches.
    if TokenTyp = ttSemicolon then
      Token := nextToken  // consume the ;
    // If at the start of the next constant, then missing a semicolon.
    else if ConstantStartSet.contains(TPascalTokenTyp.toTyp(TokenTyp)) then
      ErrorHandler.Flag(Token, ecMISSING_SEMICOLON, Self);
  end;
  // Look for the END token.
  if Token.getTyp = ttEnd then
    Token := nextToken  // consume END
  else
    ErrorHandler.Flag(Token, ecMISSING_END, Self);
  Result := selectNode;
end;

{ TBlockParser }

constructor TBlockParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//Parse a block
function TBlockParser.Parse(Token: TToken; RoutineID: ISymTabEntry): ICodeNode;
var
  DeclarationsParser: TDeclarationsParser;
  StatementParser: TStatementParser;
  TokenTyp: ITokenTyp;
  rootNode: ICodeNode;
begin
  DeclarationsParser := TDeclarationsParser.Create(Self);
  StatementParser := TStatementParser.Create(Self);
  //Parse any declaration
  DeclarationsParser.Parse(Token, RoutineID);
  Token := Synchronize(StatementParser.StmtStartSet);
  TokenTyp := Token.getTyp;
  rootNode := Nil;
  // Look for the BEGIN token to parse a compound statement.
  if TokenTyp = ttBegin then
    rootNode := StatementParser.Parse(Token)
  // Missing BEGIN: Attempt to parse anyway if possible.
  else begin
    ErrorHandler.Flag(Token, ecMISSING_BEGIN, Self);
    if StatementParser.StmtStartSet.contains(TokenTyp) then begin
      rootNode := TICodeFactory.CreateICodeNode(ctCompound);
      StatementParser.parseList(Token, rootNode, ttEnd, ecMISSING_END);
    end;
  end;
  Result := rootNode;
end;

{ TDeclarationsParser }

constructor TDeclarationsParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

function TDeclarationsParser.Parse(Token: TToken; ParentID: ISymTabEntry): ISymTabEntry;
var
  ConstantDefinitionsParser: TConstantDefinitionsParser;
  TypeDefinitionsParser: TTypeDefinitionsParser;
  VariableDeclarationsParser: TVariableDeclarationsParser;
  RoutineParser: TDeclaredRoutineParser;
  TokenTyp: ITokenTyp;
begin
  Token := Synchronize(DeclarationStartSet);
  if Token.getTyp = ttConst then begin
    Token := nextToken;  // consume CONST
    ConstantDefinitionsParser := TConstantDefinitionsParser.Create(Self);
    ConstantDefinitionsParser.Parse(Token);
  end;

  Token := Synchronize(TypeStartSet);
  if Token.getTyp = ttType then begin
    Token := nextToken;  // consume TYPE
    TypeDefinitionsParser := TTypeDefinitionsParser.Create(Self);
    TypeDefinitionsParser.Parse(Token);
  end;

  Token := Synchronize(VarStartSet);
  if Token.getTyp = ttVar then begin
    Token := nextToken;  // consume VAR
    VariableDeclarationsParser := TVariableDeclarationsParser.Create(Self);
    VariableDeclarationsParser.setDefinition(defVariable);
    VariableDeclarationsParser.Parse(Token);
  end;

  Token := Synchronize(RoutineStartSet);
  TokenTyp := Token.getTyp;
  while (TokenTyp = ttProcedure) or (TokenTyp = ttFunction) do begin
    RoutineParser := TDeclaredRoutineParser.Create(Self);
    RoutineParser.Parse(Token, ParentID);
    //Look for one or more semicolons after a definition
    Token := CurrentToken;
    if Token.getTyp = ttSemicolon then
      while Token.getTyp = ttSemicolon do
        Token := NextToken;      //consume ;
    Token := Synchronize(RoutineStartSet);
    TokenTyp := Token.getTyp;
  end;
  Result := Nil;
end;


{ TConstantDefinitionsParser }

function TConstantDefinitionsParser.ParseConstant(Token: TToken): TObject;
var
  Sign: ITokenTyp;
  TokenTyp: ITokenTyp;
  intValue: integer;
  fltValue: double;
begin
  Sign := ttNone;
  Token := Synchronize(ConstantStartSet);
  TokenTyp := Token.getTyp;
  if (TokenTyp = ttPlus) or (TokenTyp = ttMinus) then begin
    Sign := TokenTyp;
    Token := nextToken;       //consume sign
  end;
  case TPascalTokenTyp.toTyp(Token.getTyp) of
    ttIdentifier: Result := ParseIdentifierConstant(Token, Sign);
    ttInteger: begin
                 intValue := TInteger(Token.getValue).Value;
                 nextToken;   //consume number
                 if Sign = ttMinus then
                   Result := TInteger.Create(-intValue)
                 else
                   Result := TInteger.Create(intValue);
               end;
    ttReal:    begin
                 fltValue := TFloat(Token.getValue).Value;
                 nextToken;   //consume number
                 if Sign = ttMinus then
                   Result := TFloat.Create(-fltValue)
                 else
                   Result := TFloat.Create(fltValue);
               end;
    ttString:  begin
                 if Sign <> ttNone then
                   ErrorHandler.Flag(Token, ecINVALID_CONSTANT, Self);
                 nextToken;  //consume the string
                 Result := TString(Token.getValue);
               end;
    else begin
           ErrorHandler.Flag(Token, ecINVALID_CONSTANT, Self);
           Result := Nil;
         end;
  end;
end;

function TConstantDefinitionsParser.ParseIdentifierConstant(Token: TToken;
  Sign: ITokenTyp): TObject;
var
  Name: string;
  ID: ISymTabEntry;
  Definition: IDefinition;
  Value: TObject;
  intValue: integer;
  fltValue: Double;
begin
  Definition := defNone;
  Name := Token.getText.ToLower;
  ID := symTabStack.Lookup(Name);
  nextToken;  //consume identiier
  //The identifier must have already been defined as an constant identifier.
  if ID = Nil then begin
    ErrorHandler.Flag(Token, ecIDENTIFIER_UNDEFINED, Self);
    Exit(NIL);
  end;
  Definition := ID.getDefinition;
  if Definition = defConstant then begin
    Value := ID.getAttribute(skConstantValue);
    ID.appendLineNumber(Token.getLineNum);
    if Value is TInteger then begin
      intValue := TInteger(Value).Value;
      if Sign = ttMinus then
        Result := TInteger.Create(-intValue)
      else
        Result := TInteger.Create(intValue);
    end
    else if Value is TFloat then begin
      fltValue := TFloat(Value).Value;
      if Sign = ttMinus then
        Result := TFloat.Create(-fltValue)
      else
        Result := TFloat.Create(fltValue);
    end
    else if Value is TString then begin
      if Sign <> ttNone then
        ErrorHandler.Flag(Token, ecINVALID_CONSTANT, Self);
      Result := Value;
    end
    else Result := Nil;
  end
  else if Definition = defEnumeration_Constant then begin
    Value := ID.getAttribute(skConstantValue);
    ID.appendLineNumber(Token.getLineNum);
    if Sign <> ttNone then
      ErrorHandler.Flag(Token, ecINVALID_CONSTANT, Self);
    Result := Value;
  end
  else if Definition = defNone then begin
    ErrorHandler.Flag(Token, ecNOT_CONSTANT_IDENTIFIER, Self);
    Result := Nil;
  end
  else begin
    ErrorHandler.Flag(Token, ecINVALID_CONSTANT, Self);
    Result := Nil;
  end;
end;

//Return the type of a constant given its value.
function TConstantDefinitionsParser.getConstantType(Value: TObject): ITypeSpec;
var
  constantType: ITypeSpec = Nil;
begin
  if Value is TInteger then
    constantType := TPredefined.integerType
  else if Value is TFloat then
    constantType := TPredefined.realType
  else if Value is TString then begin
    if TString(Value).Length = 1 then
      constantType := TPredefined.charType
    else
      constantType := TTypeFactory.CreateStringType(TString(Value).Value);
  end;
  Result := constantType;
end;

//Return the type of a constant given its identifier.
function TConstantDefinitionsParser.getConstantType(Identifier: TToken
  ): ITypeSpec;
var
  Name: string;
  ID: ISymTabEntry;
  Definition: IDefinition;
begin
  Name := Identifier.getText.ToLower;
  ID := symTabStack.Lookup(Name);
  if ID = Nil then
    Exit(Nil);
  Definition := ID.getDefinition;
  if (Definition = defConstant) or (Definition = defEnumeration_Constant) then
    Result := ID.getTypeSpec
  else
    Result := Nil;
end;

constructor TConstantDefinitionsParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

procedure TConstantDefinitionsParser.Parse(Token: TToken);
var
  Name: string;
  constantID: ISymTabEntry;
  constantToken: TToken;
  Value: TObject;
  constantType: ITypeSpec;
  TokenTyp: ITokenTyp;
begin
  Token := Synchronize(IdentifierSet);
  //Loop to parse a sequence of constant definitions separated by semicolons.
  while Token.getTyp = ttIdentifier do begin
    Name := Token.getText.toLower;
    constantID := symTabStack.lookupLocal(Name);
    //Enter new identifier into symbol table but don't set how it's defined yet
    if constantID = Nil then begin
      constantID := symTabStack.enterLocal(Name);
      constantID.appendLineNumber(Token.getLineNum);
      constantID.setDefinition(defNone);
    end
    else begin
      ErrorHandler.Flag(Token, ecIDENTIFIER_REDEFINED, Self);
      constantID := Nil;
    end;
    Token := nextToken;  // consume the identifier token
    //Synchronize on the = token.
    Token := Synchronize(EqualsSet);
    if Token.getTyp = ttEquals then
      Token := nextToken  // consume the =
    else
      ErrorHandler.Flag(Token, ecMISSING_EQUALS, Self);
    //Parse the constant value.
    constantToken := Token;
    Value := ParseConstant(Token);
    //Set identifier to be a constant and set its value.
    if constantID <> Nil then begin
      constantID.setDefinition(defConstant);
      constantID.setAttribute(skConstantValue, Value);
      //Set the constant's type.
      if constantToken.getTyp = ttIdentifier then
        constantType := getConstantType(constantToken)
      else
        constantType := getConstantType(Value);
      constantID.setTypeSpec(constantType);
    end;
    Token := currentToken;
    TokenTyp := Token.getTyp;
    //Look for one or more semicolons after a definition.
    if TokenTyp = ttSemicolon then begin
      while Token.getTyp = ttSemicolon do
        Token := nextToken;  // consume the ;
    end
    // If at the start of the next definition or declaration,
    // then missing a semicolon.
    else if NextStartSet.Contains(TokenTyp) then
      ErrorHandler.Flag(Token, ecMISSING_SEMICOLON, Self);
    Token := Synchronize(IdentifierSet);
  end;
end;

{ TTypeDefinitionsParser }

constructor TTypeDefinitionsParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

procedure TTypeDefinitionsParser.Parse(Token: TToken);
var
  Name: string;
  TypeID: ISymTabEntry;
  TypeSpecificationParser: TTypeSpecificationParser;
  Typ: ITypeSpec;
  TokenTyp: ITokenTyp;
begin
  Token := Synchronize(IdentifierSet);
  //Loop to parse a sequence of type definitions separated by semicolons.
  while Token.getTyp = ttIdentifier do begin
    Name := Token.getText.toLower;
    TypeID := symTabStack.LookupLocal(Name);
    //Enter new identifier into symbol table but don't set how it's defined yet.
    if typeID = Nil then begin
      TypeID := symTabStack.EnterLocal(Name);
      TypeID.AppendLineNumber(Token.getLineNum);
      TypeID.setDefinition(defNone);
    end
    else begin
      ErrorHandler.Flag(Token, ecIDENTIFIER_REDEFINED, Self);
      TypeID := Nil;
    end;
    Token := nextToken;  // consume the indetifier token
    //Synchronize on the = token.
    Token := Synchronize(EqualsSet);
    if Token.getTyp = ttEquals then
      Token := nextToken  // consume the =
    else
      ErrorHandler.Flag(Token, ecMISSING_EQUALS, Self);
    //Parse the type specification
    TypeSpecificationParser := TTypeSpecificationParser.Create(Self);
    Typ := TypeSpecificationParser.Parse(Token);
    //Set identifier to be a type and set its type specification.
    if TypeID <> Nil then
      TypeID.setDefinition(defType);
    //Cross-link the type identifier and the type specification.
    if (Typ <> Nil) and (TypeID <> Nil) then begin
      if Typ.getIdentifier = Nil then
        Typ.setIdentifier(TypeID);
      TypeID.setTypeSpec(Typ);
    end
    else Token := Synchronize(FollowSet);
    Token := currentToken;
    TokenTyp := Token.getTyp;
    // Look for one or more semicolons after a definition.
    if TokenTyp = ttSemicolon then begin
      while Token.getTyp = ttSemicolon do
        Token := nextToken;  // consume the ;
    end
    // If at the start of the next definition or declaration,
    // then missing a semicolon.
    else if NextStartSet.Contains(TokenTyp) then
      ErrorHandler.Flag(Token, ecMISSING_SEMICOLON, Self);
    Token := Synchronize(IdentifierSet);
  end;
end;


{ TTypeSpecificationParser }

constructor TTypeSpecificationParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//Parse a Pascal type specification.
function TTypeSpecificationParser.Parse(Token: TToken): ITypeSpec;
var
  ArrayTypeParser: TArrayTypeParser;
  RecordTypeParser: TRecordTypeParser;
  SimpleTypeParser: TSimpleTypeParser;
begin
  //Synchronize at the start of a type specification.
  Token := Synchronize(TypeStartSet);
  case TPascalTokenTyp.toTyp(Token.getTyp) of
    ttArray:
      begin
        ArrayTypeParser := TArrayTypeParser.Create(Self);
        Result := ArrayTypeParser.Parse(Token);
      end;
    ttRecord:
      begin
        RecordTypeParser := TRecordTypeParser.Create(Self);
        Result := RecordTypeParser.Parse(Token);
      end;
    else begin
      SimpleTypeParser := TSimpleTypeParser.Create(Self);
      Result := SimpleTypeParser.Parse(Token);
    end;
  end;
end;

{ TSimpleTypeParser }

constructor TSimpleTypeParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

function TSimpleTypeParser.Parse(Token: TToken): ITypeSpec;
var
  Name: string;
  ID: ISymTabEntry;
  Definition: IDefinition;
  SubrangeTypeParser: TSubrangeTypeParser;
  EnumerationTypeParser: TEnumerationTypeParser;
begin
  //Synchronize at the start of a simple type specification.
  Token := Synchronize(SimpleTypeStartSet);
  case TPascalTokenTyp.toTyp(Token.getTyp) of
    ttIdentifier:
      begin
        Name := Token.getText.toLower;
        ID := symTabStack.Lookup(Name);
        if ID <> Nil then begin
          Definition := ID.getDefinition;
          //It's either a type identifier or the start of a subrange type.
          if Definition = defType then begin
            ID.appendLineNumber(Token.getLineNum);
            Token := nextToken;  // consume the identifier
            //Return the type of the referent type.
            Result := ID.getTypeSpec;
          end
          else if (Definition <> defConstant) and
                  (Definition <> defEnumeration_Constant) then
          begin
            ErrorHandler.Flag(Token, ecNOT_TYPE_IDENTIFIER, Self);
            Token := nextToken;  // consume the identifier
            Result := Nil;
          end
          else begin
            SubrangeTypeParser := TSubrangeTypeParser.Create(Self);
            Result := SubrangeTypeParser.Parse(Token);
          end;
        end
        else begin
          ErrorHandler.Flag(Token, ecIDENTIFIER_UNDEFINED, Self);
          Token := nextToken;  // consume the identifier
          Result := Nil;
        end;
      end;
    ttLeftParen:
      begin
        EnumerationTypeParser := TEnumerationTypeParser.Create(Self);
        Result := EnumerationTypeParser.Parse(Token);
      end;
    ttComma, ttSemicolon:
      begin
        ErrorHandler.Flag(Token, ecINVALID_TYPE, Self);
        Result := Nil;
      end;
    else begin
      SubrangeTypeParser := TSubrangeTypeParser.Create(Self);
      Result := SubrangeTypeParser.Parse(Token);
    end;
  end;
end;


{ TSubrangeTypeParser }

constructor TSubrangeTypeParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//Check a value of a type specification
function TSubrangeTypeParser.checkValueType(Token: TToken; Value: TObject;
  Typ: ITypeSpec): TObject;
var
  ch: char;
begin
  if Typ = Nil then
    Exit(Value);
  if Typ = TPredefined.integerType then
    Result := Value
  else if Typ = TPredefined.charType then begin
    ch := TString(Value).Value[1];
    Result := TInteger.Create(ch.getNumericValue);
  end
  else if Typ.getForm = tfEnumeration then
    Result := Value
  else begin
    ErrorHandler.Flag(Token, ecINVALID_SUBRANGE_TYPE, Self);
    Result := Value;
  end;
end;

//Parse a Pascal subrange type specification
function TSubrangeTypeParser.Parse(Token: TToken): ITypeSpec;
var
  minValue, maxValue: TObject;
  subrangeType: ITypeSpec;
  constantToken: TToken;
  constantParser: TConstantDefinitionsParser;
  minType, maxType: ITypeSpec;
  sawDotDot: Boolean;
  TokenTyp: ITokenTyp;
begin
  subrangeType := TTypeFactory.createType(tfSubRange);
  minValue := Nil;
  maxValue := Nil;
  //Parse the minimum constant.
  constantToken := Token;
  constantParser := TConstantDefinitionsParser.Create(Self);
  minValue := constantParser.ParseConstant(Token);
  //Set the minimum constant's type.
  if constantToken.getTyp = ttIdentifier then
    minType := constantParser.getConstantType(constantToken)
  else
    minType := constantParser.getConstantType(minValue);
  minValue := checkValueType(constantToken, minValue, minType);
  Token := currentToken;
  sawDotDot := False;
  //Look for the .. token
  if Token.getTyp = ttDotDot then begin
    Token := nextToken;   //consume the .. token
    sawDotDot := True;
  end;
  TokenTyp := Token.getTyp;
  //At the start of the maximum constant?
  //if TConstantDefinitionsParser.ConstantStartSet.Contains(TokenTyp) then begin
  if constantParser.ConstantStartSet.Contains(TokenTyp) then begin
    if not sawDotDot then
      ErrorHandler.Flag(Token, ecMISSING_DOT_DOT, Self);
    // Parse the maximum constant.
    Token := Synchronize(constantParser.ConstantStartSet);
    constantToken := Token;
    maxValue := ConstantParser.ParseConstant(Token);
    //Set the maximum constant's type
    if constantToken.getTyp = ttIdentifier then
      maxType := constantParser.getConstantType(constantToken)
    else
      maxType := constantParser.getConstantType(maxValue);
    maxValue := checkValueType(constantToken, maxValue, maxType);
    //Are the min and max value types valid?
    if (minType = Nil) or (maxType = Nil) then
      ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self)
    //Are the min and max value types the same?
    else if minType <> maxType then
      ErrorHandler.Flag(Token, ecINVALID_SUBRANGE_TYPE, Self)
    //Min value > max value?
    else if (minValue <> Nil) and (maxValue <> Nil) and
            (TInteger(minValue).Value >= TInteger(maxValue).Value) then
      ErrorHandler.Flag(Token, ecMIN_GT_MAX, Self);
  end
  else
    ErrorHandler.Flag(Token, ecINVALID_SUBRANGE_TYPE, Self);

  SubrangeType.setAttribute(tkSubRangeBaseType, minType.getObject);
  SubrangeType.setAttribute(tkSubRangeMinValue, minValue);
  SubrangeType.setAttribute(tkSubRangeMaxValue, maxValue);
  Result := SubrangeType;
end;

{ TEnumerationTypeParser }

constructor TEnumerationTypeParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//Parse an enumeration identifier.
procedure TEnumerationTypeParser.ParseEnumerationIdentifier(Token: TToken;
  Value: integer; EnumerationType: ITypeSpec; Constants: TSymTabEntries);
var
  TokenTyp: ITokenTyp;
  Name: string;
  constantID: ISymTabEntry;
begin
  TokenTyp := Token.getTyp;
  if TokenTyp = ttIdentifier then begin
    Name := Token.getText.ToLower;
    constantID := SymTabStack.LookupLocal(Name);
    if constantID <> Nil then
      ErrorHandler.Flag(Token, ecIDENTIFIER_REDEFINED, Self)
    else begin
      constantID := SymTabStack.EnterLocal(Name);
      constantID.setDefinition(defEnumeration_Constant);
      constantID.setTypeSpec(EnumerationType);
      constantID.setAttribute(skConstantValue, TInteger.Create(Value));
      constantID.appendLineNumber(Token.getLineNum);
      Constants.Add(constantID);
    end;
    Token := nextToken;   // consume identifier
  end
  else
    ErrorHandler.Flag(Token, ecMISSING_IDENTIFIER, Self);
end;

//Parse a Pascal enumeration type specification.
function TEnumerationTypeParser.Parse(Token: TToken): ITypeSpec;
var
  EnumerationType: ITypeSpec;
  Value: integer = -1;
  Constants: TSymTabEntries;
  TokenTyp: ITokenTyp;
begin
  EnumerationType := TTypeFactory.CreateType(tfEnumeration);
  Constants := TSymTabEntries.Create;
  Token := nextToken;    //consume the opening (
  repeat
    Token := Synchronize(EnumConstantStartSet);
    Inc(Value);
    ParseEnumerationIdentifier(Token, Value, EnumerationType, Constants);
    Token := currentToken;
    TokenTyp := Token.getTyp;
    //Look for the comma
    if TokenTyp = ttComma then begin
      Token := nextToken;    //consume the ,
      if EnumDefinitionFollowSet.Contains(Token.getTyp) then
        ErrorHandler.Flag(Token, ecMISSING_IDENTIFIER, Self);
    end
    else if EnumConstantStartSet.Contains(TokenTyp) then
      ErrorHandler.Flag(Token, ecMISSING_COMMA, Self);
  until EnumDefinitionFollowSet.Contains(Token.getTyp);
  //Look for the closing )
  if Token.getTyp = ttRightParen then
    Token := nextToken    //consume the )
  else
    ErrorHandler.Flag(Token, ecMISSING_RIGHT_PAREN, Self);

  EnumerationType.setAttribute(tkEnumerationConstants, Constants);
  Result := EnumerationType;
end;

{ TArrayTypeParser }

constructor TArrayTypeParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//Parse the list of index type specifications.
function TArrayTypeParser.ParseIndexTypeList(Token: TToken; ArrayType: ITypeSpec
  ): ITypeSpec;
var
  ElementType, newElementType: ITypeSpec;
  anotherIndex: boolean  = False;
  TokenTyp: ITokenTyp;
begin
  ElementType := ArrayType;
  Token := nextToken;    //consume the [
  //Parse the list of index type specifications
  repeat
    anotherIndex := False;
    //Parse the index type
    Token := Synchronize(IndexStartSet);
    ParseIndexType(Token, ElementType);
    //Synchronize at the , token
    Token := Synchronize(IndexFollowSet);
    TokenTyp := Token.getTyp;
    if (TokenTyp <> ttComma) and (TokenTyp <> ttRightBracket) then begin
      if IndexStartSet.Contains(TokenTyp) then begin
        ErrorHandler.Flag(Token, ecMISSING_COMMA, Self);
        anotherIndex := True;
      end;
    end
    //Create an ARRAY element type object for each subsequent index type.
    else if TokenTyp = ttComma then begin
      newElementType := TTypeFactory.CreateType(tfArray);
      ElementType.setAttribute(tkArrayElementType, newElementType.getObject);
      ElementType := newElementType;
      Token := nextToken;  // consume ,
      anotherIndex := True;
    end;
  until not anotherIndex;
  Result := ElementType;
end;

//Parse an index type specification.
procedure TArrayTypeParser.ParseIndexType(Token: TToken; ArrayType: ITypeSpec);
var
  SimpleTypeParser: TSimpleTypeParser;
  indexType: ITypeSpec;
  Form: ITypeForm;
  Count: integer = 0;
  minValue, maxValue: TInteger;
  Constants: TSymtabEntries;
begin
  SimpleTypeParser := TSimpleTypeParser.Create(Self);
  indexType := SimpleTypeParser.Parse(Token);
  if indexType <> Nil then
    ArrayType.setAttribute(tkArrayIndexType, indexType.getObject)
  else begin
    ArrayType.setAttribute(tkArrayIndexType, Nil);
    Exit;
  end;
  Form := indexType.getForm;
  //Check the index type and set the element count.
  if Form = tfSubrange then begin
    minValue := TInteger(indexType.getAttribute(tkSubRangeMinValue));
    maxValue := TInteger(indexType.getAttribute(tkSubRangeMaxValue));
    if (minValue <> Nil) and (maxValue <> Nil) then
      Count := maxValue.Value - minValue.Value + 1;
  end
  else if Form = tfEnumeration then begin
    Constants := TSymtabEntries(indexType.getAttribute(tkEnumerationConstants));
    Count := Constants.Count;
  end
  else
    ErrorHandler.Flag(Token, ecINVALID_INDEX_TYPE, Self);

  ArrayType.setAttribute(tkArrayElementCount, TInteger.Create(Count));
end;

//Parse the element type specification.
function TArrayTypeParser.ParseElementType(Token: TToken): ITypeSpec;
Var
  TypeSpecificationParser: TTypeSpecificationParser;
begin
  TypeSpecificationParser := TTypeSpecificationParser.Create(Self);
  Result := TypeSpecificationParser.Parse(Token);
end;

//Parse a Pascal array type specification.
function TArrayTypeParser.Parse(Token: TToken): ITypeSpec;
var
  ArrayType, ElementType: ITypeSpec;
begin
  ArrayType := TTypeFactory.CreateType(tfArray);
  Token := nextToken;  //consume ARRAY
  //Synchronize at the [ token.
  Token := Synchronize(LeftBracketSet);
  if Token.getTyp <> ttLeftBracket then
    ErrorHandler.Flag(Token, ecMISSING_LEFT_BRACKET, Self);
  //Parse the list of index types.
  ElementType := ParseIndexTypeList(Token, ArrayType);
  //Sync at the ] token
  Token := Synchronize(RightBracketSet);
  if Token.getTyp = ttRightBracket then
    Token := nextToken   //consume ]
  else
    ErrorHandler.Flag(Token, ecMISSING_RIGHT_BRACKET, Self);
  //Synchronize at OF.
  Token := Synchronize(Of_Set);
  if Token.getTyp = ttOf then
    Token := nextToken   //consume OF
  else
    ErrorHandler.Flag(Token, ecMISSING_OF, Self);
  //Parse the element type.
  elementType.setAttribute(tkArrayElementType, parseElementType(Token).getObject);
  Result := ArrayType;
end;

{ TRecordTypeParser }

constructor TRecordTypeParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//Parse a Pascal record type specification.
function TRecordTypeParser.Parse(Token: TToken): ITypeSpec;
var
  recordType: ITypeSpec;
  VariableDeclarationsParser: TVariableDeclarationsParser;
begin
  recordType := TTypeFactory.CreateType(tfRecord);
  Token := nextToken;  // consume RECORD
  //Push a symbol table for the RECORD type specification.
  recordType.setAttribute(tkRecordSymTab, symTabStack.Push.getObject);
  //Parse the field declarations.
  VariableDeclarationsParser := TVariableDeclarationsParser.Create(Self);
  VariableDeclarationsParser.setDefinition(defField);
  VariableDeclarationsParser.Parse(Token);
  //Pop off the record's symbol table.
  symTabStack.Pop;
  //Synchronize at the END.
  Token := Synchronize(EndSet);
  if Token.getTyp = ttEnd then
    Token := nextToken   //consume END
  else
    ErrorHandler.Flag(Token, ecMISSING_END, Self);
  Result := recordType;
end;

{ TVariableDeclarationsParser }

function TVariableDeclarationsParser.ParseIdentifier(Token: TToken
  ): ISymTabEntry;
var
  ID: ISymTabEntry = Nil;
  Name: string;
begin
  if Token.getTyp = ttIdentifier then begin
    Name := Token.getText.ToLower;
    ID := SymTabStack.LookupLocal(Name);
    //Enter a new identifier into the symbol table.
    if ID = Nil then begin
      ID := SymTabStack.EnterLocal(Name);
      ID.setDefinition(Definition);
      ID.appendLineNumber(Token.getLineNum);
    end
    else
      ErrorHandler.Flag(Token, ecIDENTIFIER_REDEFINED, Self);
    Token := nextToken   //consume Identifier token
  end
  else
    ErrorHandler.Flag(Token, ecMISSING_IDENTIFIER, Self);

  Result := ID;
end;

procedure TVariableDeclarationsParser.setDefinition(ADefinition: IDefinition);
begin
  Definition := ADefinition;
end;

//Parse a sublist of identifiers and their type spec
function TVariableDeclarationsParser.ParseIdentifierSublist(Token: TToken;
  AFollowSet, ACommaSet: TPascalTokenTypSet): TSymTabEntries;
var
  Sublist: TSymTabEntries;
  TokenTyp: ITokenTyp;
  ID, variableId: ISymTabEntry;
  Typ: ITypeSpec;
begin
  Sublist := TSymTabEntries.Create;
  repeat
    Token := Synchronize(IdentifierStartSet);
    ID := ParseIdentifier(Token);
    if ID <> Nil then
      Sublist.Add(ID);
    Token := Synchronize(ACommaSet);
    TokenTyp := Token.getTyp;
    //Look for the comma.
    if TokenTyp = ttComma then begin
      Token := nextToken;  // consume the ,
      if AFollowSet.Contains(Token.getTyp) then
        ErrorHandler.Flag(Token, ecMISSING_IDENTIFIER, Self);
    end
    else if IdentifierStartSet.Contains(TokenTyp) then
      ErrorHandler.Flag(Token, ecMISSING_COMMA, Self);
  until AFollowSet.Contains(Token.getTyp);

  if Definition <> defProgram_Parm then begin
    //Parse the type specification.
    Typ := ParseTypeSpec(Token);

    //Assign the type specification to each identifier in the list.
    for variableId in Sublist do
      variableId.setTypeSpec(Typ);
  end;
  Result := Sublist;
end;

//Parse the type specification
function TVariableDeclarationsParser.ParseTypeSpec(Token: TToken): ITypeSpec;
var
  TypeSpecificationParser: TTypeSpecificationParser;
  Typ: ITypeSpec;
begin
  //Synchronize on the : token.
  Token := Synchronize(ColonSet);
  if Token.getTyp = ttColon then
    Token := nextToken  // consume the :
  else
    ErrorHandler.Flag(Token, ecMISSING_COLON, Self);

  //Parse the type specification.
  TypeSpecificationParser := TTypeSpecificationParser.Create(Self);
  Typ := TypeSpecificationParser.Parse(Token);
  Result := Typ;
end;

constructor TVariableDeclarationsParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//Parse variable declarations.
procedure TVariableDeclarationsParser.Parse(Token: TToken);
var
  TokenTyp: ITokenTyp;
begin
  Token := Synchronize(IdentifierSet);
  //Loop to parse a sequence of variable declarations separated by semicolons.
  while Token.getTyp = ttIdentifier do begin
    //Parse the identifier sublist and its type specification.
    ParseIdentifierSublist(Token, IdentifierFollowSet, CommaSet);

    Token := currentToken;
    TokenTyp := Token.getTyp;

    //Look for one or more semicolons after a definition.
    if TokenTyp = ttSemicolon then begin
      while Token.getTyp = ttSemicolon do
        Token := nextToken;  // consume the ;
    end
    //If at the start of the next definition or declaration,
    //then missing a semicolon.
    else if NextStartSet.Contains(TokenTyp) then
      ErrorHandler.Flag(Token, ecMISSING_SEMICOLON, Self);

    Token := Synchronize(IdentifierSet);
  end;
end;

{  TVariableParser  }

constructor TVariableParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
  isFunctionTarget := False;
end;

//Parse a variable
function TVariableParser.Parse(Token: TToken): ICodeNode;
var
  Name: string;
  VariableID: ISymTabEntry;
begin
  Name := Token.getText.toLower;
  VariableID := SymTabStack.Lookup(Name);
  //if not found flag error and enter identifier as undefined with type undefined
  if VariableID = Nil then begin
    ErrorHandler.Flag(Token, ecIDENTIFIER_UNDEFINED, Self);
    VariableID := symTabStack.EnterLocal(Name);
    VariableID.setDefinition(defUndefined);
    VariableID.setTypeSpec(TPredefined.undefinedType);
  end;
  Result := Parse(Token, VariableID);
end;

//Parse a variable
function TVariableParser.Parse(Token: TToken; VariableID: ISymTabEntry): ICodeNode;
var
  defnCode: IDefinition;
  variableNode, subFldNode: ICodeNode;
  variableType: ITypeSpec;
begin
  //Check how the variable is defined.
  defnCode := variableId.getDefinition;
  if not ((defnCode = defVariable) or (defnCode = defValue_Parm) or
          (defnCode = defVar_Parm) or
          (isFunctionTarget and (defnCode = defFunction))) then
    ErrorHandler.Flag(Token, ecINVALID_IDENTIFIER_USAGE, Self);
  variableId.appendLineNumber(Token.getLineNum);
  variableNode := TICodeFactory.createICodeNode(ctVariable);
  variableNode.setAttribute(ckID, variableId.getObject);
  Token := nextToken;  // consume the identifier
  //Parse array subscripts or record fields.
  variableType := variableId.getTypeSpec;
  while SubscriptFieldStartSet.Contains(Token.getTyp) do begin
    if Token.getTyp = ttLeftBracket then
      subFldNode := parseSubscripts(variableType)   // []
    else
      subFldNode := parseField(variableType);       // .field
    Token := currentToken;
    // Update the variable's type.
    // The variable node adopts the SUBSCRIPTS or FIELD node.
    variableType := subFldNode.getTypeSpec;
    variableNode.addChild(subFldNode);
  end;
  variableNode.setTypeSpec(variableType);
  Result := variableNode;
end;

//Parse a function name as the target of an assignment statement
function TVariableParser.ParseFunctionNameTarget(Token: TToken): ICodeNode;
begin
  isFunctionTarget := True;
  Result := Parse(Token);
end;

//Parse a set of comma-separated subscript expressions.
function TVariableParser.ParseSubscripts(variableType: ITypeSpec): ICodeNode;
var
  Token: TToken;
  ExpressionParser: TExpressionParser;
  subscriptsNode, exprNode: ICodeNode;
  exprType, indexType: ITypeSpec;
begin
  ExpressionParser := TExpressionParser.Create(Self);
  //Create a SUBSCRIPTS node.
  subscriptsNode := TICodeFactory.createICodeNode(ctSubscripts);
  repeat
    Token := nextToken;  // consume the [ or , token
    //The current variable is an array.
    if variableType.getForm = tfArray then begin
      //Parse the subscript expression.
      exprNode := ExpressionParser.Parse(Token);
      if ExprNode <> Nil then
        ExprType := ExprNode.getTypeSpec
      else
        ExprType := TPredefined.UndefinedType;

      //The subscript expression type must be assignment
      //compatible with the array index type.
      IndexType := TTypeSpecImpl(VariableType.getAttribute(tkArrayIndexType));

      if not TTypeChecker.areAssignmentCompatible(IndexType, ExprType) then
        ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);

      //The SUBSCRIPTS node adopts the subscript expression tree.
      SubscriptsNode.addChild(exprNode);

      //Update the variable's type.
      VariableType := TTypeSpecImpl(VariableType.getAttribute(tkArrayElementType));
    end
    else begin   //Not an array type, so too many subscripts.
      ErrorHandler.Flag(Token, ecTOO_MANY_SUBSCRIPTS, Self);
      ExpressionParser.Parse(Token);
    end;
    Token := currentToken;
  until Token.getTyp <> ttComma;

  //Synchronize at the ] token.
  Token := Synchronize(RightBracketSet);
  if Token.getTyp = ttRightBracket then
    Token := nextToken  // consume the ] token
  else
    ErrorHandler.Flag(Token, ecMISSING_RIGHT_BRACKET, Self);

  subscriptsNode.setTypeSpec(variableType);
  Result := subscriptsNode;
end;

//Parse a record field.
function TVariableParser.ParseField(variableType: ITypeSpec): ICodeNode;
var
  fieldNode: ICodeNode;
  Token: TToken;
  TokenTyp: ITokenTyp;
  variableForm: ITypeForm;
  SymTab: ISymTab;
  FieldName: string;
  FieldID: ISymTabEntry;
begin
  //Create a FIELD node.
  fieldNode := TICodeFactory.createICodeNode(ctField);
  Token := nextToken;  // consume the . token
  TokenTyp := Token.getTyp;
  variableForm := variableType.getForm;
  if (TokenTyp = ttIdentifier) and (variableForm = tfRecord) then begin
    SymTab := TSymTabImpl(variableType.getAttribute(tkRecordSymTab));
    FieldName := Token.getText.toLower;
    FieldID := SymTab.Lookup(FieldName);
    if FieldID <> Nil then begin
      variableType := FieldID.getTypeSpec;
      FieldID.AppendLineNumber(Token.getLineNum);
      // Set the field identifier's name.
      fieldNode.setAttribute(ckID, FieldId.getObject);
    end
    else ErrorHandler.Flag(Token, ecINVALID_FIELD, Self);
  end
  else ErrorHandler.Flag(Token, ecINVALID_FIELD, Self);
  Token := nextToken;   //consume field identifier
  FieldNode.setTypeSpec(VariableType);
  Result := FieldNode;
end;

{ TProgramParser }

constructor TProgramParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//parse a program
function TProgramParser.Parse(Token: TToken; ParentID: ISymTabEntry
  ): ISymTabEntry;
var
  RoutineParser: TDeclaredRoutineParser;
begin
  Token := Synchronize(ProgramStartSet);
  //Parse the program
  RoutineParser := TDeclaredRoutineParser.Create(Self);
  RoutineParser.Parse(Token, ParentID);
  //Look fot the final period.
  Token := CurrentToken;
  if Token.getTyp <> ttDot then
    ErrorHandler.Flag(Token, ecMISSING_PERIOD, Self);
  Result := Nil;
end;

{ TDeclaredRoutineParser }

//Parse a routine's name
function TDeclaredRoutineParser.ParseRoutineName(Token: TToken;
  DummyName: string): ISymTabEntry;
var
  RoutineID: ISymTabEntry = Nil;
  RoutineName: string = '';
begin
  //Parse the routine name identifier
  if Token.getTyp = ttIdentifier then begin
    RoutineName := Token.getText.ToLower;
    RoutineID := SymTabStack.LookupLocal(RoutineName);

    //Not already defined locally; Enter into local symbol table
    if RoutineID = Nil then
      RoutineID := SymTabStack.EnterLocal(RoutineName)
    //If already defined, it should be a forward definition
    else if TRoutineCodeImpl(
           RoutineID.getAttribute(skRoutineCode)).Value <> rcForward then
      begin
        RoutineID := Nil;
        ErrorHandler.Flag(Token, ecIDENTIFIER_REDEFINED, Self);
      end;

    Token := NextToken; //consume routine name ident
  end
    else ErrorHandler.Flag(Token, ecMISSING_IDENTIFIER, Self);

  //If necessary, create a dummy routine name global table entry
  if RoutineID = Nil then
    RoutineID := SymTabStack.EnterLocal(DummyName);

  Result := RoutineID;
end;

//Parse the routine's formal parameter list and the function return type.
procedure TDeclaredRoutineParser.ParseHeader(Token: TToken;
  RoutineID: ISymTabEntry);
var
  VariableDeclarationsParser: TVariableDeclarationsParser;
  Typ: ITypeSpec;
  Form: ITypeForm;
begin
  //Parse the routine's formal parameters
  ParseFormalParameters(Token, RoutineID);
  Token := CurrentToken;
  //If this is a function, parse and set its return type
  if RoutineID.getDefinition = defFunction then begin
    VariableDeclarationsParser := TVariableDeclarationsParser.Create(Self);
    VariableDeclarationsParser.setDefinition(defFunction);
    Typ := VariableDeclarationsParser.ParseTypeSpec(Token);
    Token := CurrentToken;
    //The return type cannot be an array or record
    if Typ <> Nil then begin
      Form := Typ.getForm;
      if (Form = tfArray) or (Form = tfRecord) then
        ErrorHandler.Flag(Token, ecINVALID_TYPE, self);
    end
    else //missing return type
      Typ := TPredefined.undefinedType;
    RoutineID.setTypeSpec(Typ);
    Token := CurrentToken;
  end;
end;

//Parse a sublist of formal parameter declarations
function TDeclaredRoutineParser.ParseParmSublist(Token: TToken;
  RoutineID: ISymTabEntry): TSymTabEntries;
var
  isProgram: boolean;
  ParmDefn: IDefinition;
  TokenTyp: ITokenTyp;
  VariableDeclarationsParser: TVariableDeclarationsParser;
  SubList: TSymTabEntries;
begin
  isProgram := RoutineID.getDefinition = defProgram;
  if isProgram then
    ParmDefn := defProgram_Parm
  else
    ParmDefn := defNone;
  TokenTyp := Token.getTyp;
  //VAR or value parameter
  if TokenTyp = ttVar then begin
    if not isProgram then
      ParmDefn := defVar_Parm
    else
      ErrorHandler.Flag(Token, ecINVALID_VAR_PARM, self);
    Token := NextToken;   // consume Var
  end
  else if not isProgram then
    ParmDefn := defValue_Parm;
  //Parse the parameter sublist and its type spec
  VariableDeclarationsParser := TVariableDeclarationsParser.Create(Self);
  VariableDeclarationsParser.setDefinition(ParmDefn);
  SubList := VariableDeclarationsParser.ParseIdentifierSublist(Token,
               ParameterFollowSet, CommaSet);

  Token := CurrentToken;
  TokenTyp := Token.getTyp;
  if not isProgram then begin
    //look for one or more semicolons after a sublist
    if TokenTyp = ttSemicolon then begin
      while Token.getTyp = ttSemicolon do
        Token := NextToken;  //consume ;
    end
    //if at the start of the next sublist, then missing a semicolon
    else if VariableDeclarationsParser.NextStartSet.Contains(TokenTyp) then
      ErrorHandler.Flag(Token, ecMISSING_SEMICOLON, self);
    Token := Synchronize(ParameterSet);
  end;
  Result := Sublist;
end;

//Parse a routine's formal parameter list
procedure TDeclaredRoutineParser.ParseFormalParameters(Token: TToken;
  RoutineID: ISymTabEntry);
var
  Parms, ParamSubList: TSymTabEntries;
  TokenTyp: ITokenTyp;
  i: integer;
begin
  //Parse the formal params if there is a '('
  Token := Synchronize(LeftParenSet);
  if Token.getTyp = ttLeftParen then begin
    Token := NextToken;  // consume (
    Parms := TSymTabEntries.Create;
    Token := Synchronize(ParameterSet);
    TokenTyp := Token.getTyp;
    //Loop to parse sublists of formal parameter declarations
    while (TokenTyp = ttIdentifier) or (TokenTyp = ttVar) do begin
      ParamSubList := ParseParmSublist(Token, RoutineID);
      for i := 0 to ParamSubList.Count - 1 do
        Parms.Add(ParamSubList[i]);
      Token := CurrentToken;
      TokenTyp := Token.getTyp;
    end;
    //Closing right parenthesis
    if Token.getTyp = ttRightParen then
      Token := NextToken   // consume )
    else
      ErrorHandler.Flag(Token, ecMISSING_RIGHT_PAREN, self);

    RoutineID.setAttribute(skRoutineParms, Parms);
  end;
end;

constructor TDeclaredRoutineParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
  DummyCounter := 0;
end;

//Parse a standard subroutine declaration
function TDeclaredRoutineParser.Parse(Token: TToken; ParentID: ISymTabEntry
  ): ISymTabEntry;
var
  DummyName: string = '';
  RoutineID: ISymTabEntry = Nil;
  RoutineDefn: IDefinition;
  RoutineTyp: ITokenTyp;
  AICode: IIntermediateCode;
  SymTab: ISymTab;
  SubRoutines: TSymTabEntries;
  BlockParser: TBlockParser;
  rootNode: ICodeNode;

  function getRoutineCode(obj: TObject): IRoutineCode;
  begin
    if obj<>Nil then
      Result := TRoutineCodeImpl(obj).Value
    else
      Result := rcNone;
  end;

begin
  RoutineDefn := defNone;
  RoutineTyp := Token.getTyp;
  //Initialize
  case TPascalTokenTyp.toTyp(RoutineTyp) of
    ttProgram:   begin
                   Token := NextToken;  // consume Program
                   RoutineDefn := defProgram;
                   DummyName := 'DummyProgramName'.ToLower;
                 end;
    ttProcedure: begin
                   Token := NextToken;  // consume Procedure
                   RoutineDefn := defProcedure;
                   Inc(DummyCounter);
                   DummyName := 'DummyProcedureName_'.ToLower +
                                Format('%03d', [DummyCounter])
                 end;
    ttFunction:  begin
                   Token := NextToken;  // consume Function
                   RoutineDefn := defFunction;
                   Inc(DummyCounter);
                   DummyName := 'DummyFunctionName_'.ToLower +
                                Format('%03d', [DummyCounter])
                 end;
    else begin
           RoutineDefn := defProgram;
           DummyName := 'DummyProgramName'.ToLower;
         end;
  end;
  //Parse the routine name
  RoutineID := ParseRoutineName(Token, DummyName);
  RoutineID.setDefinition(RoutineDefn);
  Token := CurrentToken;
  //Create new intermediate code for the routine
  AICode := TICodeFactory.CreateICode;
  RoutineID.setAttribute(skRoutineICode, AICode.getObject);
  RoutineID.setAttribute(skRoutineRoutines, TSymTabEntries.Create);
  //Push the routine's new symbol table onto the stack.
  //If it was forwarded, push its existing symbol table.
  if getRoutineCode(RoutineId.getAttribute(skRoutineCode)) = rcForward then
  begin
    SymTab := TSymTabImpl(RoutineId.getAttribute(skRoutineSymTab));
    SymTabStack.Push(SymTab);
  end
  else
    RoutineId.setAttribute(skRoutineSymTab, SymTabStack.Push.getObject);

  //Program: Set the program identifier in the symbol table stack.
  if RoutineDefn = defProgram then
    SymTabStack.setProgramId(RoutineId)
  //Non-forwarded procedure or function: Append to the parent's list of routines
  else if getRoutineCode(RoutineId.getAttribute(skRoutineCode)) <> rcForward then
  begin
    SubRoutines := TSymTabEntries(ParentId.getAttribute(skRoutineRoutines));
    Subroutines.Add(RoutineId);
  end;

  //If the routine was forwarded, there should not be any formal parameters or
  //a function return type. But parse them anyway if they're there.
  if getRoutineCode(RoutineId.getAttribute(skRoutineCode)) = rcForward then
  begin
    if Token.getTyp <> ttSemicolon then begin
      ErrorHandler.Flag(Token, ecALREADY_FORWARDED, Self);
      ParseHeader(Token, RoutineId);
    end;
  end
  //Parse the routine's formal parameters and function return type.
  else ParseHeader(Token, RoutineId);

  //Look for the semicolon.
  Token := CurrentToken;
  if Token.getTyp = ttSemicolon then begin
    repeat
      Token := NextToken;  // consume ;
    until Token.getTyp <> ttSemicolon;
  end
  else ErrorHandler.Flag(Token, ecMISSING_SEMICOLON, Self);

  //Parse the routine's block or forward declaration.
  if (Token.getTyp = ttIdentifier) and (Token.getText.ToLower = 'forward') then
  begin
    Token := NextToken;  // consume forward
    RoutineId.setAttribute(skRoutineCode, TRoutineCodeImpl.Create(rcForward));
  end
  else begin
    RoutineId.setAttribute(skRoutineCode, TRoutineCodeImpl.Create(rcDeclared));
    BlockParser := TBlockParser.Create(Self);
    RootNode := BlockParser.Parse(Token, RoutineId);
    AiCode.setRoot(RootNode);
  end;

  //Pop the routine's symbol table off the stack.
  SymTabStack.Pop;
  Result := RoutineId;
end;

{ TCallParser }

//Check an actual parameter against the corresponding formal parameter.
procedure TCallParser.CheckActualParameter(Token: TToken; FormalID: ISymTabEntry;
  ActualNode: ICodeNode);
var
  FormalDefn: IDefinition;
  FormalType, ActualType: ITypeSpec;
begin
  FormalDefn := FormalId.getDefinition;
  FormalType := FormalId.getTypeSpec;
  ActualType := ActualNode.getTypeSpec;
  //VAR parameter: the actual parameter must be a variable of the same type
  //  as the formal parameter
  if FormalDefn = defVar_Parm then begin
    if (ActualNode.getType <> ctVariable) or (ActualType <> FormalType) then
      ErrorHandler.Flag(Token, ecINVALID_VAR_PARM, Self);
  end
  //Value parameter: the actual parameter must be assignment compatible
  //  with the formal parameter
  else if not TTypeChecker.areAssignmentCompatible(FormalType, ActualType) then
    ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
end;

//Parse the field width or the precision for an actual parameter of a call to
// write or writeln.
function TCallParser.ParseWriteSpec(Token: TToken): ICodeNode;
var
  ExpressionParser: TExpressionParser;
  SpecNode: Icodenode;
begin
  if Token.getTyp = ttColon then begin
    Token := NextToken;  //consume :
    ExpressionParser := TExpressionParser.Create(Self);
    SpecNode := ExpressionParser.Parse(Token);
    if SpecNode.getType = ctIntegerConstant then
      Result := SpecNode
    else begin
      ErrorHandler.Flag(Token, ecINVALID_NUMBER, Self);
      Result := Nil;
    end;
  end;
end;

//Parse the actual parameters of a procedure or function call.
function TCallParser.ParseActualParameters(Token: TToken; pfID: ISymTabEntry;
  isDeclared, isReadReadLn, isWriteWriteLn: boolean): ICodeNode;
var
  ExpressionParser: TExpressionParser;
  parmsNode, ActualNode, ExprNode: ICodeNode;
  FormalParms: TSymTabEntries = Nil;
  ParmCount: integer = 0;
  ParmIndex: integer = -1;
  FormalId: ISymTabEntry;
  Typ: ITypeSpec;
  Form: ITypeForm;
  TokenTyp: ITokenTyp;
begin
  ExpressionParser := TExpressionParser.Create(Self);
  parmsNode := TICodeFactory.CreateICodeNode(ctParameters);
  if isDeclared then begin
    FormalParms := TSymTabEntries(pfID.getAttribute(skRoutineParms));
    if FormalParms <> Nil then
      ParmCount := FormalParms.Count;
  end;
  if Token.getTyp <> ttLeftParen then begin
    if ParmCount <> 0 then
      ErrorHandler.Flag(Token, ecWRONG_NUMBER_OF_PARMS, Self);
    Exit(Nil);
  end;
  Token := NextToken;  // consume opening (
  //Loop to parse each actual parameter.
  while Token.getTyp <> ttRightParen do begin
    ActualNode := ExpressionParser.Parse(Token);
    //Declared procedure or function: Check the number of actual parameters,
    //and check each actual parameter against the corresponding formal parameter.
    if isDeclared then begin
      Inc(ParmIndex);
      if ParmIndex < ParmCount then begin
        FormalId := FormalParms[ParmIndex];
        CheckActualParameter(Token, FormalId, ActualNode);
      end
      else if ParmIndex = ParmCount then
        ErrorHandler.Flag(Token, ecWRONG_NUMBER_OF_PARMS, Self);
    end
    //read or readln: Each actual parameter must be a variable that is
    //                a scalar, boolean, or subrange of integer.
    else if isReadReadln then begin
      Typ := ActualNode.getTypeSpec;
      Form := Typ.getForm;
      if not ((ActualNode.getType = ctVariable) and
              ((Form = tfScalar) or (Typ = TPredefined.booleanType) or
               ((Form = tfSubrange) and (Typ.baseType = TPredefined.integerType))
              )
             ) then
        ErrorHandler.Flag(Token, ecINVALID_VAR_PARM, Self);
    end
    //write or writeln: The type of each actual parameter must be a scalar,
    //boolean, or a Pascal string. Parse any field width and precision.
    else if isWriteWriteLn then begin
      //Create a WRITE_PARM node which adopts the expression node.
      ExprNode := ActualNode;
      ActualNode := TICodeFactory.CreateICodeNode(ctWriteParm);
      ActualNode.addChild(ExprNode);
      Typ := ExprNode.getTypeSpec.baseType;
      Form := Typ.getForm;
      if not ((Form = tfScalar) or (Typ = TPredefined.booleanType) or
              (Typ.isPascalString) ) then
        ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self);
      //Optional field width.
      Token := CurrentToken;
      ActualNode.addChild(ParseWriteSpec(Token));
      //Optional precision.
      Token := CurrentToken;
      ActualNode.addChild(ParseWriteSpec(Token));
    end;
    ParmsNode.addChild(ActualNode);
    Token := Synchronize(CommaSet);
    TokenTyp := Token.getTyp;
    //Look for the comma.
    if TokenTyp = ttComma then
      Token := NextToken  // consume ,
    else if ExpressionParser.ExprStartSet.Contains(TokenTyp) then
      ErrorHandler.Flag(Token, ecMISSING_COMMA, Self)
    else if TokenTyp <> ttRightParen then
      Token := Synchronize(ExpressionParser.ExprStartSet);
  end;
  Token := NextToken;  // consume closing )
  if (ParmsNode.getChildren.Count = 0) or
     (isDeclared and (ParmIndex <> ParmCount-1)) then
    ErrorHandler.Flag(Token, ecWRONG_NUMBER_OF_PARMS, Self);
  Result := ParmsNode;
end;

constructor TCallParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//Parse a call to a declared procedure or function.
function TCallParser.Parse(Token: TToken): ICodeNode;
var
  pfID: ISymTabEntry;
  RoutineCode: IRoutineCode;
  CallDeclaredParser: TCallDeclaredParser;
  CallStandardParser: TCallStandardParser;
begin
  pfID := SymTabStack.Lookup(Token.getText.toLower);
  RoutineCode := TRoutineCodeImpl(pfID.getAttribute(skRoutineCode)).Value;
  if (RoutineCode = rcDeclared) or (RoutineCode = rcForward) then begin
    CallDeclaredParser := TCallDeclaredParser.Create(Self);
    Result := CallDeclaredParser.Parse(Token);
  end
  else begin
    CallStandardParser := TCallStandardParser.Create(Self);
    Result := CallStandardParser.Parse(Token);
  end;
end;

{ TCallDeclaredParser }

constructor TCallDeclaredParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

//Parse a call to a declared procedure or function
function TCallDeclaredParser.Parse(Token: TToken): ICodeNode;
var
  CallNode, ParmsNode: ICodeNode;
  pfID: ISymTabEntry;
begin
  // Create the CALL node.
  CallNode := TICodeFactory.CreateICodeNode(ctCall);
  pfID := SymTabStack.Lookup(Token.getText.toLower);
  CallNode.setAttribute(ckID, pfID.getObject);
  CallNode.setTypeSpec(pfID.getTypeSpec);

  Token := NextToken;  // consume procedure or function identifier

  ParmsNode := ParseActualParameters(Token, pfID, True, False, False);

  CallNode.addChild(ParmsNode);
  Result := CallNode;
end;

{ TCallStandardParser }

constructor TCallStandardParser.Create(Parent: TPascalParserTD);
begin
  Inherited Create(Parent);
end;

function TCallStandardParser.Parse(Token: TToken): ICodeNode;
var
  CallNode: ICodeNode;
  pfID: ISymTabEntry;
  RoutineCode: IRoutineCode;
begin
  CallNode := TICodeFactory.CreateICodeNode(ctCall);
  pfID := SymTabStack.Lookup(Token.getText.toLower);
  RoutineCode := TRoutineCodeImpl(pfID.getAttribute(skRoutineCode)).Value;
  CallNode.setAttribute(ckID, pfID.getObject);
  Token := NextToken; //consume procedure or function
  case TRoutineCodeImpl.toTyp(RoutineCode) of
    rcRead, rcReadLn:   Result := ParseReadReadln(Token, CallNode, pfID);
    rcWrite, rcWriteLn: Result := ParseWriteWriteLn(Token, CallNode, pfID);
    rcEOF, rcEOLN:      Result := ParseEofEoLn(Token, CallNode, pfID);
    rcABS, rcSQR:       Result := ParseAbsSqr(Token, CallNode, pfID);
    rcARCTAN, rcCOS, rcEXP, rcLN, rcSIN, rcSQRT:
      Result := ParseArcTanCosExpLnSinSqrt(Token, CallNode, pfID);
    rcPRED, rcSUCC:     Result := ParsePredSucc(Token, CallNode, pfID);
    rcCHR:              Result := ParseChr(Token, CallNode, pfID);
    rcODD:              Result := ParseOdd(Token, CallNode, pfID);
    rcORD:              Result := ParseOrd(Token, CallNode, pfID);
    rcROUND, rcTRUNC:   Result := ParseRoundTrunc(Token, CallNode, pfID);
    else Result := Nil;
  end;
end;

//Parse a call to Read or ReadLn
function TCallStandardParser.ParseReadReadLn(Token: TToken; CallNode: ICodeNode;
  pfID: ISymTabEntry): ICodeNode;
var
  ParmsNode: ICodeNode;
begin
  ParmsNode := ParseActualParameters(Token, pfID, False, True, False);
  CallNode.addChild(ParmsNode);

  // Read must have parameters.
  if (pfID = TPredefined.readId) and (CallNode.getChildren.Count = 0) then
    ErrorHandler.Flag(Token, ecWRONG_NUMBER_OF_PARMS, Self);

  Result := CallNode;
end;

//Parse a call to Write or WriteLn
function TCallStandardParser.ParseWriteWriteLn(Token: TToken; CallNode: ICodeNode;
  pfID: ISymTabEntry): ICodeNode;
var
  ParmsNode: ICodeNode;
begin
  ParmsNode := ParseActualParameters(Token, pfID, False, False, True);
  CallNode.addChild(ParmsNode);

  // Write must have parameters.
  if (pfID = TPredefined.writeId) and (CallNode.getChildren.Count = 0) then
    ErrorHandler.Flag(Token, ecWRONG_NUMBER_OF_PARMS, Self);

  Result := CallNode;
end;

//Parse a call to eof or eoln
function TCallStandardParser.ParseEofEoLn(Token: TToken; CallNode: ICodeNode;
  pfID: ISymTabEntry): ICodeNode;
var
  ParmsNode: ICodeNode;
begin
  ParmsNode := ParseActualParameters(Token, pfID, False, False, False);
  CallNode.addChild(ParmsNode);

  //No actual parameters.
  if CheckParmCount(Token, ParmsNode, 0) then
    CallNode.setTypeSpec(TPredefined.booleanType);

  Result := CallNode;
end;

//Parse a call to abs or sqr
function TCallStandardParser.ParseAbsSqr(Token: TToken; CallNode: ICodeNode;
  pfID: ISymTabEntry): ICodeNode;
var
  ParmsNode: ICodeNode;
  argType: ITypeSpec;
begin
  ParmsNode := ParseActualParameters(Token, pfID, False, False, False);
  CallNode.addChild(ParmsNode);

  //There should be one integer or real parameter.
  //The function return type is the parameter type.
  if CheckParmCount(Token, ParmsNode, 1) then begin
    argType := ParmsNode.getChildren[0].getTypeSpec.baseType;
    if (argType = TPredefined.integerType) or
       (argType = TPredefined.realType) then
      CallNode.setTypeSpec(argType)
    else
      ErrorHandler.Flag(Token, ecINVALID_TYPE, Self);
  end;

  Result := CallNode;
end;

//Parse a call to arctan, cos, exp, ln, sin, or sqrt
function TCallStandardParser.ParseArcTanCosExpLnSinSqrt(Token: TToken;
  CallNode: ICodeNode; pfID: ISymTabEntry): ICodeNode;
var
  ParmsNode: ICodeNode;
  argType: ITypeSpec;
begin
  ParmsNode := ParseActualParameters(Token, pfID, False, False, False);
  CallNode.addChild(ParmsNode);

  //There should be one integer or real parameter.
  //The function return type is real.
  if CheckParmCount(Token, ParmsNode, 1) then begin
    argType := ParmsNode.getChildren[0].getTypeSpec.baseType;
    if (argType = TPredefined.integerType) or
       (argType = TPredefined.realType) then
      CallNode.setTypeSpec(TPredefined.realType)
    else
      ErrorHandler.Flag(Token, ecINVALID_TYPE, Self);
  end;

  Result := CallNode;
end;

//Parse a call to pred or succ
function TCallStandardParser.ParsePredSucc(Token: TToken; CallNode: ICodeNode;
  pfID: ISymTabEntry): ICodeNode;
var
  ParmsNode: ICodeNode;
  argType: ITypeSpec;
begin
  ParmsNode := ParseActualParameters(Token, pfID, False, False, False);
  CallNode.addChild(ParmsNode);

  //There should be one integer or enumeration parameter.
  //The function return type is the parameter type.
  if CheckParmCount(Token, ParmsNode, 1) then begin
    argType := ParmsNode.getChildren[0].getTypeSpec.baseType;
    if (argType = TPredefined.integerType) or
       (argType.getForm = tfEnumeration) then
      CallNode.setTypeSpec(argType)
    else
      ErrorHandler.Flag(Token, ecINVALID_TYPE, Self);
  end;

  Result := CallNode;
end;

//Parse a call to chr.
function TCallStandardParser.ParseChr(Token: TToken; CallNode: ICodeNode;
  pfID: ISymTabEntry): ICodeNode;
var
  ParmsNode: ICodeNode;
  argType: ITypeSpec;
begin
  ParmsNode := ParseActualParameters(Token, pfID, False, False, False);
  CallNode.addChild(ParmsNode);

  //There should be one integer parameter.
  //The function return type is character.
  if CheckParmCount(Token, ParmsNode, 1) then begin
    argType := ParmsNode.getChildren[0].getTypeSpec.baseType;
    if (argType = TPredefined.integerType) then
      CallNode.setTypeSpec(TPredefined.charType)
    else
      ErrorHandler.Flag(Token, ecINVALID_TYPE, Self);
  end;

  Result := CallNode;
end;

//Parse a call to odd.
function TCallStandardParser.ParseOdd(Token: TToken; CallNode: ICodeNode;
  pfID: ISymTabEntry): ICodeNode;
var
  ParmsNode: ICodeNode;
  argType: ITypeSpec;
begin
  ParmsNode := ParseActualParameters(Token, pfID, False, False, False);
  CallNode.addChild(ParmsNode);

  //There should be one integer parameter.
  //The function return type is boolean.
  if CheckParmCount(Token, ParmsNode, 1) then begin
    argType := ParmsNode.getChildren[0].getTypeSpec.baseType;
    if (argType = TPredefined.integerType) then
      CallNode.setTypeSpec(TPredefined.booleanType)
    else
      ErrorHandler.Flag(Token, ecINVALID_TYPE, Self);
  end;

  Result := CallNode;
end;

//Parse a call to ord.
function TCallStandardParser.ParseOrd(Token: TToken; CallNode: ICodeNode;
  pfID: ISymTabEntry): ICodeNode;
var
  ParmsNode: ICodeNode;
  argType: ITypeSpec;
begin
  ParmsNode := ParseActualParameters(Token, pfID, False, False, False);
  CallNode.addChild(ParmsNode);

  //There should be one character or enumeration parameter.
  //The function return type is integer.
  if CheckParmCount(Token, ParmsNode, 1) then begin
    argType := ParmsNode.getChildren[0].getTypeSpec.baseType;
    if (argType = TPredefined.charType) or
       (argType.getForm = tfEnumeration) then
      CallNode.setTypeSpec(TPredefined.integerType)
    else
      ErrorHandler.Flag(Token, ecINVALID_TYPE, Self);
  end;

  Result := CallNode;
end;

function TCallStandardParser.ParseRoundTrunc(Token: TToken; CallNode: ICodeNode;
  pfID: ISymTabEntry): ICodeNode;
var
  ParmsNode: ICodeNode;
  argType: ITypeSpec;
begin
  ParmsNode := ParseActualParameters(Token, pfID, False, False, False);
  CallNode.addChild(ParmsNode);

  //There should be one real parameter.
  //The function return type is integer.
  if CheckParmCount(Token, ParmsNode, 1) then begin
    argType := ParmsNode.getChildren[0].getTypeSpec.baseType;
    if argType = TPredefined.realType then
      CallNode.setTypeSpec(TPredefined.integerType)
    else
      ErrorHandler.Flag(Token, ecINVALID_TYPE, Self);
  end;

  Result := CallNode;
end;

//Check the number of actual parameters.
function TCallStandardParser.CheckParmCount(Token: TToken; ParmsNode: ICodeNode;
  Count: integer): boolean;
begin
  if ((ParmsNode = Nil) and (Count = 0)) or
     (ParmsNode.getChildren.Count = Count) then
    Result := True
  else begin
    ErrorHandler.Flag(Token, ecWRONG_NUMBER_OF_PARMS, Self);
    Result := False;
  end;
end;



end.

