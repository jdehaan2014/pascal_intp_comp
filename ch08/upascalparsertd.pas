unit uPascalParserTD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubaseparser, ubasescanner, utoken, umessages,
  uPascalTokentyp, dateutils, uPascalErrorHandler, uPascalErrorCode,
  uCode, uCodeFactory, uEOFToken;

type

  { TPascalParserTD }

  TPascalParserTD = class(TBaseParser)
  private
    protected
      class var
        ErrorHandler: TPascalErrorHandler;
    public
      class constructor Create;
      constructor Create(AScanner: TBaseScanner);
      constructor Create(Parent: TPascalParserTD);
      procedure Parse; override;
      function getErrorCount: integer; override;
      function getErrorHandler: TPascalErrorHandler;
      function Synchronize(syncSet: TPascalTokenTypSet): TToken;
  end;

implementation

uses uPascalParsers;

{ TPascalParserTD }

class constructor TPascalParserTD.Create;
begin
  ErrorHandler := TPascalErrorHandler.Create;
end;

constructor TPascalParserTD.Create(AScanner : TBaseScanner);
begin
  inherited Create(AScanner);
end;

constructor TPascalParserTD.Create(Parent: TPascalParserTD);
begin
  inherited Create(Parent.getScanner);
end;

{Parse a Pascal source program and generate the symbol table
 and the intermediate code}
procedure TPascalParserTD.Parse;
var
  StartTime: TDateTime;
  ElapsedTime: Double;
  rootNode: ICodeNode;
  Token: TToken;
  statementParser: TStatementParser;

begin
  StartTime := TimeOf(Now);
  ICode := TICodeFactory.CreateICode;

  try
    Token := NextToken;
    rootNode := Nil;

    // look for the 'Begin' token to parse a compound statement
    if Token.getTyp = ttBegin then begin
      statementParser := TStatementParser.Create(Self);
      rootNode := statementParser.Parse(Token);
      Token := currentToken;
    end
    else
      ErrorHandler.Flag(Token, ecUNEXPECTED_TOKEN, Self);

    //look for the final period.
    if Token.getTyp <> ttDot then
      ErrorHandler.Flag(Token, ecMISSING_PERIOD, Self);
    Token := currentToken;

    //set the parse tree root node
    if rootNode <> Nil then
      ICode.setRoot(rootNode);

    // Send the parser summary message
    ElapsedTime := SecondSpan(StartTime, TimeOf(Now));

    SendMessage(TMessage.Create(mtParserSummary,
      TParserSummaryMsg.Create(Token.getLineNum, getErrorCount, ElapsedTime)));

  except
    on E: EInOutError do
      ErrorHandler.AbortTranslation(ecIO_ERROR, Self)
  end;
end;

function TPascalParserTD.getErrorCount : integer;
begin
  result := ErrorHandler.getErrorCount;
end;

function TPascalParserTD.getErrorHandler: TPascalErrorHandler;
begin
  Result := ErrorHandler;
end;

//synchronize the parser
function TPascalParserTD.Synchronize(syncSet: TPascalTokenTypSet): TToken;
var
  Token: TToken;
begin
  Token := currentToken;
  //if the current token is not in the synchronization set, then
  //it is unexpected and the parser must recover.
  if not syncSet.Contains(Token.getTyp) then begin
    //Flag the unexpected token
    errorHandler.Flag(Token, ecUNEXPECTED_TOKEN, Self);
    //Recover by skipping tokens that are not in the sync set
    repeat
      Token := nextToken;
    until (Token is TEOFToken) or (syncSet.Contains(Token.getTyp));
  end;
  Result := Token;
end;

end.

