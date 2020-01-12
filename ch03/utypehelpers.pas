unit uTypeHelpers;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface
uses
  Classes, SysUtils;


type

  { TVariantHelper }

  TVariantHelper = type helper for Variant
    public
      function toString: string;
      function toInt: integer;
      function toReal: real;
      function toLongInt: LongInt;
      function toSingle: Single;
      function toBool: boolean;
      function toChar: Char;
      constructor Create(aArgument: Variant);
  end;

  { TCharHelper }

  TCharHelper = type helper for Char
    public
      function isWhiteSpace: boolean;
      function isDigit: boolean;
      function isLetter: boolean;
      function isLetterOrDigit: boolean;
      function isLineEnding: boolean;
      function isFileEnding: boolean;
      function isSingleQuote: boolean;
      function isDoubleQuote: boolean;
      function getNumericValue: byte;
  end;



implementation

{ TVariantHelper }

constructor TVariantHelper.Create(aArgument: Variant);
begin
  Self := aArgument;
end;

function TVariantHelper.toString: string;
begin
  WriteStr(Result, Self);
end;

function TVariantHelper.toInt: integer;
begin
  Result := Integer(Self);
end;

function TVariantHelper.toLongInt: LongInt;
begin
  Result := LongInt(Self);
end;

function TVariantHelper.toReal: real;
begin
  Result := Real(Self);
end;

function TVariantHelper.toSingle: Single;
begin
  Result := Single(Self);
end;

function TVariantHelper.toBool: boolean;
begin
  Result := boolean(Self);
end;

function TVariantHelper.toChar: Char;
begin
  Result := Char(Self);
end;


{ TCharHelper }

function TCharHelper.isWhiteSpace: boolean;
const
  Tab      = #9;
  NewLine  = #10;
  CR       = #13;
  Space    = #32;
begin
  Result := Self in [Tab, NewLine, CR, Space];
end;

function TCharHelper.isDigit: boolean;
begin
  result := Self in ['0'..'9'];
end;

function TCharHelper.isLetter: boolean;
begin
  result := Upcase(Self) in ['A'..'Z'];
end;

function TCharHelper.isLetterOrDigit: boolean;
begin
  Result := Self.isDigit or Self.isLetter;
end;

function TCharHelper.isLineEnding: boolean;
begin
  Result := Self = LineEnding;
end;

function TCharHelper.isFileEnding: boolean;
const
  FileEnding = #3; //on Unix '^D';   // on windows ^Z    #26
begin
  Result := Self = FileEnding;
end;

function TCharHelper.isSingleQuote: boolean;
const
  SingleQuoteChar = #39;  // '
begin
  Result := Self = SingleQuoteChar;
end;

function TCharHelper.isDoubleQuote: boolean;
const
  DoubleQuoteChar = #34;  // "
begin
  Result := Self = DoubleQuoteChar;
end;

function TCharHelper.getNumericValue: byte;
begin
  Result := Ord(Self) - Ord('0');
end;



end.

