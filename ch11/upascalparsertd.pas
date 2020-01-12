unit uPascalParserTD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubaseparser, ubasescanner, utoken, umessages,
  uPascalTokentyp, dateutils, uPascalErrorHandler, uPascalErrorCode,
  uCode, uCodeFactory, uEOFToken, uPredefined, uDefinitionImpl,
  uGenericInterfaces, uSymtabImpl;

type

  { TPascalParserTD }

  TPascalParserTD = class(TBaseParser)
    protected
      class var
        ErrorHandler: TPascalErrorHandler;
    private
      RoutineID: ISymTabEntry;
    public
      class constructor Create;
      constructor Create(AScanner: TBaseScanner);
      constructor Create(Parent: TPascalParserTD);
      property getRoutineID: ISymTabEntry read RoutineID;
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
  Token: TToken;
  ProgramParser: TProgramParser;

begin
  StartTime := TimeOf(Now);
  TPredefined.Initialize(SymTabStack);

  try
    Token := NextToken;
    //Parse a program
    ProgramParser := TProgramParser.Create(Self);
    ProgramParser.Parse(Token, Nil);
    Token := currentToken;

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

