unit uPascalParserTD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubaseparser, ubasescanner, utoken, uEOFToken, umessages,
  uTokenType, uPascalTokentyp, dateutils, uPascalErrorHandler, uPascalErrorCode,
  uSymtab;

type

  { TPascalParserTD }

  TPascalParserTD = class(TBaseParser)
    protected
      class var
        ErrorHandler: TPascalErrorHandler;
    public
      class constructor Create;
      constructor Create(AScanner: TBaseScanner);
      procedure Parse; override;
      function getErrorCount: integer; override;
  end;

implementation

{ TPascalParserTD }

class constructor TPascalParserTD.Create;
begin
  ErrorHandler := TPascalErrorHandler.Create;
end;

constructor TPascalParserTD.Create(AScanner : TBaseScanner);
begin
  inherited Create(AScanner);
end;

{Parse a Pascal source program and generate the symbol table
 and the intermediate code}
procedure TPascalParserTD.Parse;
var
  Token: TToken;
  StartTime: TDateTime;
  ElapsedTime: Double;
  TokenTyp: ITokenTyp;
  Name: string;
  Entry: ISymTabEntry;
begin
  StartTime := TimeOf(Now);

  try
    // loop over each token until end of file
    Token := NextToken;
    while not (Token is TEOFToken) do begin        // instance of
      TokenTyp := Token.getTyp;
      // cross reference only the identifiers
      if TokenTyp = ttIdentifier then begin
        Name := LowerCase(Token.getText);

        // if it's not already in the symbol table,
        // create and enter a new entry for the identifier.
        Entry := symTabStack.Lookup(Name);
        if Entry = Nil then
          Entry := symTabStack.EnterLocal(Name);

        // Append the current line number to the entry.
        Entry.appendLineNumber(Token.getLineNum);
      end
      else if TokenTyp = ttError then
        ErrorHandler.Flag(Token, TPascalErrorCode(Token.getValue), Self);
      Token := NextToken;
    end;

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

end.

