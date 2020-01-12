unit uPascalScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubasescanner, usource, utoken, utokens;

type

  { TPascalScanner }

  TPascalScanner = class(TBaseScanner)
    protected
      function ExtractToken: TToken; override;
    public
      constructor Create(ASource: TSource);
  end;

implementation

{ TPascalScanner }

function TPascalScanner.ExtractToken : TToken;
var
  Token: TToken;
  CurrChar: char;
begin
  CurrChar := CurrentChar;
  if CurrChar = FileEnding then
    Token := TEOFToken.Create(Source)
  else
    Token := TToken.Create(Source);
  Result := Token;
end;

constructor TPascalScanner.Create(ASource : TSource);
begin
  inherited Create(ASource);
end;

end.

