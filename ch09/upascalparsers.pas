unit uPascalParsers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPascalParserTD, uToken, uPascalTokentyp,
  uCode, uCodeFactory, uCodeImpl, uPascalErrorHandler, uPascalErrorCode,
  uGenericInterfaces, uSymtabImpl, fgl, ueoftoken, uObjectUtils, uTokenType,
  AnyObject, uPredefined, uDefinitionImpl, uTypesImpl, uTypesFactory;

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
    public
      constructor Create(Parent: TPascalParserTD);
      function Parse(Token: TToken): ICodeNode;
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

        function ParseBranch(Token: TToken; constantSet: TConstantSet): ICodeNode;
        procedure ParseConstantList(Token: TToken; constantsNode: ICodeNode;
          constantSet: TConstantSet);
        function ParseConstant(Token: TToken; constantSet: TConstantSet): ICodeNode;
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
      procedure Parse(Token: TToken);
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
      type
        TConstants = specialize TFPGList<ISymTabEntry>;

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
      function ParseIdentifierSublist(Token: TToken): TSymTabEntries;
      function ParseTypeSpec(Token: TToken): ITypeSpec;
    public
      constructor Create(Parent: TPascalParserTD);
      procedure Parse(Token: TToken);
  end;

//generic function Question<T>(Test: Boolean; IfTrue, IfFalse: T): T;

implementation


generic function Question<T>(Test: Boolean; IfTrue, IfFalse: T): T;
begin
  if Test then
    Result := IfTrue
  else
    Result := IfFalse;
end;


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
begin
  StatementNode := Nil;
  case TPascalTokenTyp.toTyp(Token.getTyp) of
    ttBegin:
      begin
        CompoundParser := TCompoundStatementParser.Create(Self);
        StatementNode := CompoundParser.Parse(Token);
      end;
    ttIdentifier:   //assignment starts with an identifier
      begin
        AssignmentParser := TAssignmentStatementParser.Create(Self);
        StatementNode := AssignmentParser.Parse(Token);
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
    // If at the start of the next  statement,
    // then missing a semicolon.
    else if StmtStartSet.Contains(TokenTyp) then
      ErrorHandler.Flag(Token, ecMISSING_SEMICOLON, Self);
    // Sync at the start of the next statement or at the terminator
    Token := Synchronize(TerminatorSet);
  end;
  // Look for the terminator token.
  if Token.getTyp = Terminator then
    Token := NextToken
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
end;

//Parse an assignment statement.
function TAssignmentStatementParser.Parse(Token: TToken): ICodeNode;
var
  AssignNode, VariableNode: ICodeNode;
  TargetName: string;
  TargetID: ISymtabEntry;
  ExpressionParser: TExpressionParser;
begin
  // Create the ASSIGN node.
  AssignNode := TICodeFactory.CreateICodeNode(ctAssign);
  // Look up the target identifer in the symbol table stack.
  // Enter the identifier into the table if it's not found.
  TargetName := LowerCase(Token.getText);
  TargetID := SymtabStack.Lookup(TargetName);
  if TargetID = Nil then
    TargetID := SymtabStack.EnterLocal(TargetName);
  TargetId.AppendLineNumber(Token.getLineNum);
  Token := NextToken;   // consume identifier token
  // Create the variable node and set its name attribute.
  VariableNode := TICodeFactory.CreateICodeNode(ctVariable);
  VariableNode.setAttribute(ckID, TargetId.getObject);
  // The ASSIGN node adopts the variable node as its first child.
  AssignNode.addChild(VariableNode);
  // Synchronize on the := token.
  Token := Synchronize(ColonEqualsSet);
  if Token.getTyp = ttColonEquals then
    Token := NextToken   // consume :=
  else
    ErrorHandler.Flag(Token, ecMISSING_COLON_EQUALS, Self);
  // Parse the expression.  The ASSIGN node adopts the expression's
  // node as its second child.
  ExpressionParser := TExpressionParser.Create(Self);
  AssignNode.addChild(ExpressionParser.Parse(token));
  Result := AssignNode;
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
  RootNode, OpNode: ICodeNode;
  //Token: TToken;
  TokenTyp: ITokenTyp;
  NodeType: ICodeNodeType;
begin
  //Parse a simple expression and make the root of its tree the root node.
  RootNode := ParseSimpleExpression(Token);
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
    OpNode.addChild(ParseSimpleExpression(Token));
    // The operator node becomes the new root node.
    RootNode := OpNode;
  end;
  Result := RootNode;
end;

//Parse a simple expression.
function TExpressionParser.ParseSimpleExpression(Token: TToken): ICodeNode;
var
  RootNode, OpNode, NegateNode: ICodeNode;
  //Token: TToken;
  TokenTyp, SignTyp: ITokenTyp;
  NodeType: ICodeNodeType;
begin
  SignTyp := ttNone;    // type of leading sign (if any)
  //Look for a leading + or - sign.
  TokenTyp := Token.getTyp;
  if (TokenTyp = ttPlus) or (TokenTyp = ttMinus) then begin
    SignTyp := TokenTyp;
    Token := NextToken;
  end;
  //Parse a term and make the root of its tree the root node.
  RootNode := ParseTerm(Token);
  // Was there a leading - sign?
  if SignTyp = ttMinus then begin
    //Create a NEGATE node and adopt the current tree as its child.
    NegateNode := TICodeFactory.CreateICodeNode(ctNegate);
    NegateNode.addChild(RootNode);
    //The NEGATE node becomes the new root node.
    RootNode := NegateNode;
  end;
  Token := CurrentToken;
  TokenTyp := Token.getTyp;
  //Loop over additive operators.
  while AddOps.Contains(TokenTyp) do begin
    //Create a new operator node and adopt the current tree as its first child.
    NodeType := AddOpsMap.KeyData[TPascalTokenTyp.toTyp(TokenTyp)];
    OpNode := TICodeFactory.CreateICodeNode(NodeType);
    OpNode.addChild(RootNode);
    Token := NextToken;  // consume the operator
    //Parse another term. The operator node adopts the term's tree as its second child.
    OpNode.addChild(ParseTerm(Token));
    //The operator node becomes the new root node.
    RootNode := OpNode;
    Token := CurrentToken;
    TokenTyp := Token.getTyp;
  end;
  Result := RootNode;
end;

//Parse a term.
function TExpressionParser.ParseTerm(Token: TToken): ICodeNode;
var
  RootNode, OpNode: ICodeNode;
  TokenTyp: ITokenTyp;
  NodeType: ICodeNodeType;
begin
  //Parse a factor and make its node the root node.
  RootNode := ParseFactor(Token);
  Token := CurrentToken;
  TokenTyp := Token.getTyp;
  //Loop over multiplicative operators.
  while MulOps.Contains(TokenTyp) do begin
    //Create a new operator node and adopt the current tree as its first child.
    NodeType := MulOpsMap.KeyData[TPascalTokenTyp.toTyp(TokenTyp)];
    OpNode := TICodeFactory.CreateICodeNode(NodeType);
    OpNode.addChild(RootNode);
    Token := NextToken;  // consume the operator
    //Parse another factor. The operator node adopts the term's tree as its second child.
    OpNode.addChild(ParseFactor(Token));
    //The operator node becomes the new root node.
    RootNode := OpNode;
    Token := CurrentToken;
    TokenTyp := Token.getTyp;
  end;
  Result := RootNode;
end;

//Parse a factor.
function TExpressionParser.ParseFactor(Token: TToken): ICodeNode;
var
  RootNode: ICodeNode;
  TokenTyp: ITokenTyp;
  Name: string;
  ID: ISymTabEntry;
begin
  TokenTyp := Token.getTyp;
  RootNode := Nil;
  case TPascalTokenTyp.toTyp(TokenTyp) of
    ttIdentifier:
      begin
        //Look up the identifier in the symbol table stack.
        //Flag the identifier as undefined if it's not found.
        Name := Lowercase(Token.getText);
        ID := symtabStack.Lookup(Name);
        if ID = Nil then begin
          ErrorHandler.Flag(Token, ecIDENTIFIER_UNDEFINED, Self);
          ID := symTabStack.EnterLocal(Name);
        end;
        RootNode := TICodeFactory.CreateICodeNode(ctVariable);
        RootNode.setAttribute(ckID, ID.getObject);
        ID.AppendLineNumber(Token.getLineNum);
        Token := NextToken;  // consume the identifier
      end;
    ttInteger:
      begin
        // Create an INTEGER_CONSTANT node as the root node.
        RootNode := TICodeFactory.CreateICodeNode(ctIntegerConstant);
        RootNode.setAttribute(ckValue, Token.getValue);
        Token := NextToken;  // consume the number
      end;
    ttReal:
      begin
        // Create an REAL_CONSTANT node as the root node.
        RootNode := TICodeFactory.CreateICodeNode(ctRealConstant);
        RootNode.setAttribute(ckValue, Token.getValue);
        Token := NextToken;  // consume the number
      end;
    ttString:
      begin
        // Create an STRING_CONSTANT node as the root node.
        RootNode := TICodeFactory.CreateICodeNode(ctStringConstant);
        RootNode.setAttribute(ckValue, Token.getValue);
        Token := NextToken;  // consume the number
      end;
    ttNot:
      begin
        Token := NextToken;    // consume the NOT
        // Create a NOT node as the root node.
        RootNode := TICodeFactory.CreateICodeNode(ctNot);
        //Parse the factor. The NOT node adopts the factor node as its child.
        RootNode.addChild(ParseFactor(Token));
      end;
    ttLeftParen:
      begin
        Token := NextToken;    //consume '('
        // Parse an expression and make its node the root node.
        RootNode := ParseExpression(Token);
        // Look for the matching ) token.
        Token := CurrentToken;
        if Token.getTyp = ttRightParen then
          Token := NextToken       //consume ')'
        else
          ErrorHandler.Flag(Token, ecMISSING_RIGHT_PAREN, Self);
      end;
    else
      ErrorHandler.Flag(Token, ecUNEXPECTED_TOKEN, Self);
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
  loopNode, testNode: ICodeNode;
  StatementParser: TStatementParser;
  ExpressionParser: TExpressionParser;
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
  testNode.addChild(ExpressionParser.Parse(Token));
  loopNode.addChild(testNode);
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
  loopNode, breakNode, notNode: ICodeNode;
  StatementParser: TStatementParser;
  ExpressionParser: TExpressionParser;
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
  notNode.addChild(ExpressionParser.Parse(Token));
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
  arithOpNode, oneNode: ICodeNode;
  AssignmentParser: TAssignmentStatementParser;
  StatementParser: TStatementParser;
  ExpressionParser: TExpressionParser;
  Direction: ITokenTyp;
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
  //Set the current line number attribute
  setLineNumber(initAssignNode, TargetToken);
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
  //Copy the control VARIABLE node. The relational operator node adopts
  //the copied VARIABLE node as its first child
  controlVarNode := initAssignNode.getChildren[0];
  RelOpNode.addChild(controlVarNode.copyNode);
  //Parse the termination expression. The relational operator node
  //adopts the expression as its second child.
  ExpressionParser := TExpressionParser.Create(Self);
  RelOpNode.addChild(ExpressionParser.Parse(Token));
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
  nextAssignNode.addChild(controlVarNode.copyNode);
  //Create arithmetic operator node. ADD for TO, SUBTRACT for DOWNTO
  if Direction = ttTO then
    arithOpNode := TICodeFactory.CreateICodeNode(ctAdd)
  else
    arithOpNode := TICodeFactory.CreateICodeNode(ctSubtract);
  //The operator node adopts a copy of the loop variable as its
  //first child and the value 1 as its second child.
  arithOpNode.addChild(controlVarNode.copyNode);
  oneNode := TICodeFactory.CreateICodeNode(ctIntegerConstant);
  oneNode.setAttribute(ckValue, TInteger.Create(1));
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
  ifNode: ICodeNode;
  StatementParser: TStatementParser;
  ExpressionParser: TExpressionParser;
begin
  Token := nextToken;    //consume the IF
  //Create the IF node
  ifNode := TICodeFactory.CreateICodeNode(ctIf);
  //Parse the expression
  //The IF node adopts the expression as its first child
  ExpressionParser := TExpressionParser.Create(Self);
  ifNode.addChild(ExpressionParser.Parse(Token));
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
function TCaseStatementParser.ParseBranch(Token: TToken;
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
  ParseConstantList(Token, constantsNode, constantSet);
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
procedure TCaseStatementParser.ParseConstantList(Token: TToken;
  constantsNode: ICodeNode; constantSet: TConstantSet);
begin
  //Loop to parse each constant
  while ConstantStartSet.Contains(TPascalTokenTyp.toTyp(Token.getTyp)) do begin
    //The constants list node adopts the constant node
    constantsNode.addChild(parseConstant(Token, constantSet));

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
function TCaseStatementParser.ParseConstant(Token: TToken;
  constantSet: TConstantSet): ICodeNode;
var
  Sign: ITokenTyp;
  TokenTyp: ITokenTyp;
  constantNode: ICodeNode = Nil;
  Value: TObject;
  i, index: integer;
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
    ttIdentifier: constantNode := parseIdentifierConstant(Token, Sign);
    ttInteger: constantNode := parseIntegerConstant(Token.getText, Sign);
    ttString: constantNode := parseCharConstant(Token, Token.getValue.toString, Sign);
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
  nextToken;  // consume the constant
  Result := constantNode;
end;

//parse a IDENTIFIER case constant
function TCaseStatementParser.ParseIdentifierConstant(Token: TToken;
  Sign: ITokenTyp): ICodeNode;
begin
  // Placeholder: Don't allow for now.
  ErrorHandler.Flag(Token, ecINVALID_CONSTANT, Self);
  Result := Nil;
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
  selectNode: ICodeNode;
  ExpressionParser: TExpressionParser;
  ConstantSet: TConstantSet;
  tokenTyp: ITokenTyp;
begin
  Token := nextToken;  // consume CASE
  // Create a SELECT node.
  selectNode := TICodeFactory.CreateICodeNode(ctSelect);
  // Parse the CASE expression.
  // The SELECT node adopts the expression subtree as its first child.
  ExpressionParser := TExpressionParser.Create(Self);
  selectNode.addChild(ExpressionParser.Parse(Token));
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
    selectNode.addChild(ParseBranch(Token, constantSet));
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
  DeclarationsParser.Parse(Token);

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

procedure TDeclarationsParser.Parse(Token: TToken);
var
  ConstantDefinitionsParser: TConstantDefinitionsParser;
  TypeDefinitionsParser: TTypeDefinitionsParser;
  VariableDeclarationsParser: TVariableDeclarationsParser;
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
    Token := nextToken;
  end;
  case TPascalTokenTyp.toTyp(Token.getTyp) of
    ttIdentifier: Result := ParseIdentifierConstant(Token, Sign);
    ttInteger: begin
                 intValue := TInteger(Token.getValue).Value;
                 nextToken;   //consume number
                 Result := specialize Question<TObject>(Sign = ttMinus,
                   TInteger.Create(-intValue), TInteger.Create(intValue));
               end;
    ttReal:    begin
                 fltValue := TFloat(Token.getValue).Value;
                 nextToken;   //consume number
                 Result := specialize Question<TObject>(Sign = ttMinus,
                   TFloat.Create(-fltValue), TFloat.Create(fltValue));
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
  Name := Lowercase(Token.getText);
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
      Result := specialize Question<TObject>(Sign = ttMinus,
        TInteger.Create(-intValue), TInteger.Create(intValue));
    end
    else if Value is TFloat then begin
      fltValue := TFloat(Value).Value;
      Result := specialize Question<TObject>(Sign = ttMinus,
        TFloat.Create(-fltValue), TFloat.Create(fltValue));
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
      constantType := specialize Question<ITypeSpec>(
        constantToken.getTyp = ttIdentifier,
        getConstantType(constantToken), getConstantType(Value));
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
  typeID: ISymTabEntry;
  TypeSpecificationParser: TTypeSpecificationParser;
  Typ: ITypeSpec;
  TokenTyp: ITokenTyp;
begin
  Token := Synchronize(IdentifierSet);
  //Loop to parse a sequence of type definitions separated by semicolons.
  while Token.getTyp = ttIdentifier do begin
    Name := Token.getText.toLower;
    typeID := symTabStack.lookupLocal(Name);
    //Enter new identifier into symbol table but don't set how it's defined yet.
    if typeID = Nil then begin
      typeID := symTabStack.enterLocal(Name);
      typeID.appendLineNumber(Token.getLineNum);
      typeID.setDefinition(defNone);
    end
    else begin
      ErrorHandler.Flag(Token, ecIDENTIFIER_REDEFINED, Self);
      typeID := Nil;
    end;
    Token := nextToken;  // consume the type token
    //Synchronize on the = token.
    Token := Synchronize(EqualsSet);
    if Token.getTyp = ttEquals then
      Token := nextToken  // consume the =
    else
      ErrorHandler.Flag(Token, ecMISSING_EQUALS, Self);
    //Parse the type specification
    TypeSpecificationParser := TTypeSpecificationParser.Create(Self);
    Typ := TypeSpecificationParser.Parse(Token);
    //Set identifier to be a type and set its type specificationt.
    if typeID <> Nil then
      typeID.setDefinition(defType);
    //Cross-link the type identifier and the type specification.
    if (Typ <> Nil) and (typeID <> Nil) then begin
      if Typ.getIdentifier = Nil then
        Typ.setIdentifier(typeID);
      typeID.setTypeSpec(Typ);
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
          // It's either a type identifier or the start of a subrange type.
          if Definition = defType then begin
            ID.appendLineNumber(Token.getLineNum);
            Token := nextToken;  // consume the identifier
            // Return the type of the referent type.
            Result := ID.getTypeSpec;
          end
          else if (Definition <> defConstant) and
                  (Definition <> defEnumeration_Constant) then begin
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
    Result := TInteger.Create(Ord(ch));
  end
  else if Typ.getForm = tfEnumeration then
    Result := Value
  else begin
    ErrorHandler.Flag(Token, ecINVALID_SUBRANGE_TYPE, Self);
    Result := Value;
  end;
end;

function TSubrangeTypeParser.Parse(Token: TToken): ITypeSpec;
var
  minValue, maxValue: TObject;
  subrangeType: ITypeSpec;
  constantToken: TToken;
  constantParser: TConstantDefinitionsParser;
  minType, maxType: ITypeSpec;
  sawDotDot: Boolean = False;
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
  minType := specialize Question<ITypeSpec>(constantToken.getTyp = ttIdentifier,
    constantParser.getConstantType(constantToken),    // if true
    constantParser.getConstantType(minValue));        // if false
  minValue := checkValueType(constantToken, minValue, minType);
  Token := currentToken;
  //Look for the .. token
  if Token.getTyp = ttDotDot then begin
    Token := nextToken;   //consume the .. token
    sawDotDot := True;
  end;
  TokenTyp := Token.getTyp;
  //At the start of the maximum constant?
  if TConstantDefinitionsParser.ConstantStartSet.Contains(TokenTyp) then begin
    if not sawDotDot then
      ErrorHandler.Flag(Token, ecMISSING_DOT_DOT, Self);
    // Parse the maximum constant.
    Token := Synchronize(TConstantDefinitionsParser.ConstantStartSet);
    constantToken := Token;
    maxValue := ConstantParser.ParseConstant(Token);
    //Set the maximum constant's type
    maxType := specialize Question<ITypeSpec>(constantToken.getTyp = ttIdentifier,
      constantParser.getConstantType(constantToken),    // if true
      constantParser.getConstantType(maxValue));        // if false
    maxValue := checkValueType(constantToken, maxValue, maxType);
    //Are the min and max value types valid?
    if (minType = Nil) or (maxType = Nil) then
      ErrorHandler.Flag(Token, ecINCOMPATIBLE_TYPES, Self)
    //Are the min and max value types the same?
    else if minType <> maxType then
      ErrorHandler.Flag(Token, ecINVALID_SUBRANGE_TYPE, Self)
    //Min value > max value?
    else if (minType <> Nil) and (maxType <> Nil) and
            (TInteger(minValue).Value > TInteger(maxValue).Value) then
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
  elementType, newElementType: ITypeSpec;
  anotherIndex: boolean  = False;
  TokenTyp: ITokenTyp;
begin
  elementType := ArrayType;
  Token := nextToken;    //consume the [
  //Parse the list of index type specifications
  repeat
    anotherIndex := False;
    //Parse the index type
    Token := Synchronize(IndexStartSet);
    ParseIndexType(Token, elementType);
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
  Constants: TConstants;
begin
  SimpleTypeParser := TSimpleTypeParser.Create(Self);
  indexType := SimpleTypeParser.Parse(Token);
  if indexType = Nil then begin
    ArrayType.setAttribute(tkArrayIndexType, Nil);
    Exit;
  end;
  ArrayType.setAttribute(tkArrayIndexType, indexType.getObject);
  Form := indexType.getForm;
  //Check the index type and set the element count.
  if Form = tfSubrange then begin
    minValue := TInteger(indexType.getAttribute(tkSubRangeMinValue));
    maxValue := TInteger(indexType.getAttribute(tkSubRangeMaxValue));
    if (minValue <> Nil) and (maxValue <> Nil) then
      Count := maxValue.Value - minValue.Value + 1;
  end
  else if Form = tfEnumeration then begin
    Constants := TConstants(indexType.getAttribute(tkEnumerationConstants));
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
    Token := nextToken   //consume [
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

function TVariableDeclarationsParser.ParseIdentifierSublist(Token: TToken
  ): TSymTabEntries;
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
    Token := Synchronize(CommaSet);
    TokenTyp := Token.getTyp;
    //Look for the comma.
    if TokenTyp = ttComma then begin
      Token := nextToken;  // consume the ,
      if IdentifierFollowSet.Contains(Token.getTyp) then
        ErrorHandler.Flag(Token, ecMISSING_IDENTIFIER, Self);
    end
    else if IdentifierStartSet.Contains(TokenTyp) then
      ErrorHandler.Flag(Token, ecMISSING_COMMA, Self);
  until IdentifierFollowSet.Contains(Token.getTyp);

  //Parse the type specification.
  Typ := ParseTypeSpec(Token);

  //Assign the type specification to each identifier in the list.
  for variableId in Sublist do
    variableId.setTypeSpec(Typ);

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
    ParseIdentifierSublist(Token);

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



end.

