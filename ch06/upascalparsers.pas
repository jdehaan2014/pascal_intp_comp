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

      class var
        RelOpsMap: TOpsMap;
        AddOpsMap: TOpsMap;
        MulOpsMap: TOpsMap;

      function ParseExpression(Token: TToken): ICodeNode;
      function ParseSimpleExpression(Token: TToken): ICodeNode;
      function ParseTerm(Token: TToken): ICodeNode;
      function ParseFactor(Token: TToken): ICodeNode;
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
  CompoundStatementParser : TCompoundStatementParser;
  AssignmentStatementParser : TAssignmentStatementParser;
begin
  StatementNode := Nil;
  case TPascalTokenTyp.toTyp(Token.getTyp) of
    ttBegin:
      begin
        CompoundStatementParser := TCompoundStatementParser.Create(Self);
        StatementNode := CompoundStatementParser.Parse(Token);
      end;
    ttIdentifier:   //assignment starts with an identifier
      begin
        AssignmentStatementParser := TAssignmentStatementParser.Create(Self);
        StatementNode := AssignmentStatementParser.Parse(Token);
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
begin
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
    // If at the start of the next assignment statement,
    // then missing a semicolon.
    else if TokenTyp = ttIdentifier then
      ErrorHandler.Flag(Token, ecMISSING_SEMICOLON, Self)
    // Unexpected token.
    else if TokenTyp <> Terminator then begin
      ErrorHandler.Flag(Token, ecUNEXPECTED_TOKEN, Self);
      Token := NextToken; //consume unexpected token
    end;
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
  // Look for the := token.
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



end.

