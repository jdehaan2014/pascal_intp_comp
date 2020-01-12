unit uPascalParsers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPascalParserTD, uToken, uPascalTokentyp,
  uCode, uCodeFactory, uCodeImpl, uPascalErrorHandler, uPascalErrorCode,
  usymtab, uSymtabImpl, fgl, ueoftoken, uObjectUtils, uTokenType, AnyObject;

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



end.

