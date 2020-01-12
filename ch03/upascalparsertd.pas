unit uPascalParserTD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubaseparser, ubasescanner, utoken, uEOFToken, umessages,
  uTokenType, uPascalTokentyp, dateutils, uPascalErrorHandler, uPascalErrorCode;

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
  StartTime: QWord;
  Elapsed: LongWord;
  TokenTyp: ITokenTyp;
begin
  startTime := getTickCount64;
  try
    // loop over each token until end of file
    Token := NextToken;
    while not (Token is TEOFToken) do begin
      TokenTyp := Token.getTyp;
      if TokenTyp <> ttError then begin
        // format each token
        with Token do
          sendMessage(TMessage.Create(mtToken,
            TTokenMsg.Create(getLineNum, getPosition, TokenTyp, getText, getValue)));
      end
      else begin
        ErrorHandler.Flag(Token, TPascalErrorCode(Token.getValue), Self);
      end;
      Token := NextToken;
    end;

    // Send the parser summary message
    Elapsed := getTickCount64 - startTime;

    SendMessage(TMessage.Create(mtParserSummary,
      TParserSummaryMsg.Create(Token.getLineNum, getErrorCount, Elapsed)));

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

