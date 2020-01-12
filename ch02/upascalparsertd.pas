unit uPascalParserTD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubaseparser, ubasescanner, utoken, utokens, umessages,
  dateutils;

type

  { TPascalParserTD }

  TPascalParserTD = class(TBaseParser)
    public
      constructor Create(AScanner: TBaseScanner);
      procedure Parse; override;
      function getErrorCount: integer; override;
  end;

implementation

{ TPascalParserTD }

constructor TPascalParserTD.Create(AScanner : TBaseScanner);
begin
  inherited Create(AScanner);
end;

{Parse a Pascal source program and generate the symbol table
 and the intermediate code
}
procedure TPascalParserTD.Parse;
var
  Token: TToken;
  StartTime, EndTime: TDateTime;
  ElapsedTime: real;
begin
  StartTime := TimeOf(Now);
  Token := NextToken;
  while not (Token is TEOFToken) do begin
    Token := NextToken;
  end;

  // Send the parser summary message
  EndTime := TimeOf(Now);
  ElapsedTime := SecondSpan(StartTime,EndTime);

  SendMessage(TMessage.Create(mtParserSummary,
    TParserSummaryMsg.Create(Token.getLineNum, getErrorCount, ElapsedTime)));

end;

function TPascalParserTD.getErrorCount : integer;
begin
  result := 0;
end;

end.

