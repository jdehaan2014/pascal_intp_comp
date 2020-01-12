unit uPascalScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubasescanner, usource, utoken, uEOFtoken,
  uPascalTokentyp, uTypeHelpers, uPascalToken, uPascalErrorCode;

type

  { TPascalScanner }

  TPascalScanner = class(TBaseScanner)
    protected
      function ExtractToken: TToken; override;
    public
      constructor Create(ASource: TSource);
      procedure SkipWhiteSpace;
  end;

implementation

{ TPascalScanner }

//Extract and return the next Pascal token from the source.
function TPascalScanner.ExtractToken : TToken;
const
  SingleQuoteChar = #39;  // '
var
  Token: TToken;
  CurrChar: char;
begin
  SkipWhiteSpace;
  CurrChar := CurrentChar;
  //construct next token. Current character determines its type
  if CurrChar.isFileEnding then
    Token := TEOFToken.Create(Source)
  else if CurrChar.isLetter then
    Token := TPascalWordToken.Create(Source)
  else if CurrChar.isDigit then
    Token := TPascalNumberToken.Create(Source)
  else if CurrChar = SingleQuoteChar then
    Token := TPascalStringToken.Create(Source)
  else if TPascalTokenTyp.isSpecialSymbol(CurrChar) then
    Token := TPascalSpecialSymbolToken.Create(Source)
  else begin
    Token :=
      TPascalErrorToken.Create(Source, ecINVALID_CHARACTER, CurrChar);
    NextChar;
  end;
  Result := Token;
end;


constructor TPascalScanner.Create(ASource : TSource);
begin
  inherited Create(ASource);
end;

//Skip whitespace characters by consuming them.  A comment is whitespace.
procedure TPascalScanner.SkipWhiteSpace;
var
  CurrChar : Char;
begin
  CurrChar := CurrentChar;
  while CurrChar.isWhiteSpace or (CurrChar = '{') do begin
    if CurrChar = '{' then begin  //comment
      repeat
        CurrChar := NextChar;     // consume commented characters
      until (CurrChar = '}') or CurrChar.isFileEnding;
      if CurrChar = '}' then
        CurrChar := NextChar;
    end
    else CurrChar := NextChar; //not a comment so skip white space character
  end;
end;

end.

