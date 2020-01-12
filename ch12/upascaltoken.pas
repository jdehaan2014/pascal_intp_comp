unit uPascalToken;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, utoken, usource, uPascalErrorCode,
  uPascalTokenTyp, uTypeHelpers, uObjectUtils;

Type

  { TPascalToken }

  TPascalToken = class(TToken)
    public
      constructor Create(ASource: TSource);
  end;

  { TPascalErrorToken }

  TPascalErrorToken = class(TPascalToken)
    protected
      procedure Extract; override;
    public
      constructor Create(ASOurce: TSource; AErrorCode: TPascalErrorCode;
        ATokenText: string);
  end;

  { TPascalWordToken }

  TPascalWordToken = class(TPascalToken)
    protected
      procedure Extract; override;
    public
      constructor Create(ASource: TSource);
  end;

  { TPascalNumberToken }

  TPascalNumberToken = class(TPascalToken)
    protected
      procedure Extract; override;
      procedure ExtractNumber(var TextBuffer: string);
    private
      const MaxExponent = 37;
      function UnsignedIntegerDigits(var ATextBuffer: string): string;
      function ComputeIntegerValue(Digits: string): LongInt;
      function ComputeFloatValue(AWholeDigits, AFractionDigits,
        AExponentDigits: string; AExpSign: char): real;
    public
      constructor Create(ASource: TSource);
  end;

  { TPascalSpecialSymbolToken }

  TPascalSpecialSymbolToken = class(TPascalToken)
    protected
      procedure Extract; override;
    public
      constructor Create(ASource: TSource);
  end;

  { TPascalStringToken }

  TPascalStringToken = class(TPascalToken)
    protected
      procedure Extract; override;
    public
      constructor Create(ASource: TSource);
  end;


implementation

uses math;

{ TPascalToken }

constructor TPascalToken.Create(ASource : TSource);
begin
  Inherited Create(ASOurce);
end;

{ TPascalErrorToken }

constructor TPascalErrorToken.Create(ASOurce : TSource;
  AErrorCode : TPascalErrorCode; ATokenText : string);
begin
  Inherited Create(ASource);

  Typ := ttError;
  Value := AErrorCode;
  Lexeme := ATokenText;
end;

procedure TPascalErrorToken.Extract;
begin
  //do nothing
end;

{ TPascalWordToken }

procedure TPascalWordToken.Extract;
var
  CurrChar : Char;
  TextBuffer: string = '';
begin
  //extract a pascal word token from the source
  CurrChar := CurrentChar;
  //Get the word characters (letter or digit).  The scanner has
  // already determined that the first character is a letter.
  while CurrChar.isLetterOrDigit do begin
    TextBuffer += CurrChar;
    CurrChar := NextChar;
  end;
  Lexeme := TextBuffer;
  // is it a reserved word or an identifier?
  if TPascalTokenTyp.ReservedWords.IndexOf(Lowercase(Lexeme)) >= 0 then
    Typ := TPascalTokenTyp.ValueOf(Lexeme)
  else
    Typ := ttIdentifier;
end;

constructor TPascalWordToken.Create(ASource : TSource);
begin
  Inherited Create(ASource);
end;

{ TPascalNumberToken }

procedure TPascalNumberToken.Extract;
var
  textBuffer: string = '';        // contains token's characters
begin
  ExtractNumber(textBuffer);
  Lexeme := textBuffer;
end;

procedure TPascalNumberToken.ExtractNumber(var TextBuffer: string);
var
  WholeDigits: string = '';        // digits before the decimal point
  FractionDigits: string = '';     // digits after the decimal point
  ExponentDigits: string = '';     // exponent digits
  ExponentSign: char = '+';        // + or -
  SawDotDot: boolean = False;      // true if saw .. token
  IntegerValue: LongInt;     // 4 bytes
  FloatValue: Single;        // 4 bytes
  CurrChar : Char;
begin
  Typ := ttInteger;         // assume integer token for now
  //extract the digits of the whole part of the number
  WholeDigits := UnsignedIntegerDigits(TextBuffer);
  if TPascalTokenTyp.toTyp(Typ) = ttError then Exit;
  //check if there's a '.' or a '..'
  CurrChar := CurrentChar;
  if CurrChar = '.' then begin
    if PeekChar = '.' then
      SawDotDot := True   // it's '..' ; don't consume it
    else begin
      Typ := ttReal;
      TextBuffer += CurrChar;
      CurrChar := NextChar;  // consume '.'
      //collect digits of the fraction part
      FractionDigits := UnsignedIntegerDigits(TextBuffer);
      if TPascalTokenTyp.toTyp(Typ) = ttError then Exit;
    end;
  end;
  // is there an exponent part? But not if there's a '..'
  CurrChar := CurrentChar;
  if (not SawDotDot) and (Upcase(CurrChar) = 'E') then begin
    Typ := ttReal;
    TextBuffer += CurrChar;
    CurrChar := NextChar;  // consume 'E' or 'e'
    // exponent sign?
    if CurrChar in ['+', '-'] then begin
      TextBuffer += CurrChar;
      ExponentSign := CurrChar;
      CurrChar := NextChar;  // consume sign
    end;
    //extract the digits of the exponent
    ExponentDigits := UnsignedIntegerDigits(TextBuffer);
    //compute value of integer token
  end;
  if TPascalTokenTyp.toTyp(Typ) = ttInteger then begin
    IntegerValue := ComputeIntegerValue(WholeDigits);
    if TPascalTokenTyp.toTyp(Typ) <> ttError then
      Value := TInteger.Create(IntegerValue);
  end
  else if TPascalTokenTyp.toTyp(Typ) = ttReal then begin
    FloatValue := ComputeFloatValue(WholeDigits, FractionDigits,
                       ExponentDigits, ExponentSign);
    if TPascalTokenTyp.toTyp(Typ) <> ttError then
      Value := TFloat.Create(FloatValue);
  end;
end;

//Extract and return the digits of an unsigned integer
function TPascalNumberToken.UnsignedIntegerDigits(var ATextBuffer : string) : string;
var
  CurrChar: char;
  Digits: string = '';
begin
  Result := '';
  CurrChar := CurrentChar;
  // Must have at least one digit.
  if not CurrChar.isDigit then begin
    Typ := ttError;
    Value := TPascalErrorCode.Create(ecINVALID_NUMBER);
    Exit(Result);
  end;

  //extract the digits
  while CurrChar.isDigit do begin
    ATextBuffer += CurrChar;
    Digits += CurrChar;
    CurrChar := NextChar; //consume digit
  end;
  Result := Digits;
end;

//Compute and return the integer value of a string of digits.
function TPascalNumberToken.ComputeIntegerValue(Digits : string) : LongInt; //4  bytes
var
  integerValue: integer = 0;
  prevValue: integer = -1;      // overflow occurred if prefValue > integerValue
  index: integer = 1;
begin
  if Digits = '' then
    Exit(0);

  //loop over digits to compute the integer value as long as no overflow
  while (index <= Length(Digits)) and (integerValue >= prevValue) do begin
    prevValue := integerValue;
    integerValue := 10 * integerValue + (Digits[index]).getNumericValue;
    inc(index);
  end;
  // no overflow: return the integer value
  if integerValue >= prevValue then
    Result := integerValue
  else begin // overflow: set integer out of range error
    Typ := ttError;
    Value := TPascalErrorCode.Create(ecRANGE_INTEGER);
    Result := 0;
  end;
end;

//Compute and return the float value of a real number.
function TPascalNumberToken.ComputeFloatValue(AWholeDigits, AFractionDigits,
  AExponentDigits: string; AExpSign: char): real; // 4 bytes
var
  floatValue: real = 0.0;
  exponentValue: integer = 0;
  digits: string = '';
  index: integer = 1;
begin
  exponentValue := ComputeIntegerValue(AExponentDigits);
  digits := AWholeDigits;
  // negate the exponent if the sign is '-'
  if AExpSign = '-' then
    exponentValue := -exponentValue;
  // if there are any fraction digits, adjust the component value
  // and append the fraction digits
  if AFractionDigits <> '' then begin
    exponentValue -= Length(AFractionDigits);
    digits += AFractionDigits;
  end;
  // check for a real number out of range error
  if Abs(exponentValue + Length(AWholeDigits)) > MaxExponent then begin
    Typ := ttError;
    Value := TPascalErrorCode.Create(ecRANGE_REAL);
    Result := 0;
  end;
  // loop over the digits to compute the float value
  while index <= Length(digits) do begin
    floatValue := 10 * floatValue + (digits[index]).getNumericValue;
    inc(index);
  end;
  // adjust the float value based on the exponent value
  if exponentValue <> 0 then
    floatValue *= Power(10, exponentValue);
  Result := floatValue;
end;

constructor TPascalNumberToken.Create(ASource : TSource);
begin
  Inherited Create(ASource);
end;


{ TPascalSpecialSymbolToken }

//Extract a Pascal special symbol token from the source.
procedure TPascalSpecialSymbolToken.Extract;
var
  CurrChar : Char;
begin
  CurrChar := CurrentChar;
  Lexeme := CurrChar;
  Typ := ttNone;
  case CurrChar of
    '+', '-', '*', '/', ',', ';', '\', '=', '(', ')', '[', ']', '{', '}', '^' :
          begin   // Single-character special symbols.
            NextChar; // consume character
          end;
    ':' : begin
            CurrChar := NextChar;
            if CurrChar = '=' then begin
              Lexeme += CurrChar;
              NextChar;
            end;
          end;
    '<' : begin
            CurrChar := NextChar;
            if CurrChar = '=' then begin
              Lexeme += CurrChar;
              NextChar;
            end
            else if CurrChar = '>' then begin
              Lexeme += CurrChar;
              NextChar;
            end
          end;
    '>' : begin
            CurrChar := NextChar;
            if CurrChar = '=' then begin
              Lexeme += CurrChar;
              NextChar;
            end
          end;
    '.' : begin
            CurrChar := NextChar;
            if CurrChar = '.' then begin
              Lexeme += CurrChar;
              NextChar;
            end
          end;
     else begin
            NextChar;
            Typ := ttError;
            Value := TPascalErrorCode.Create(ecINVALID_CHARACTER);
          end;
  end;
  //set the type if no error
  if Typ = ttNone then
    Typ := TPascalTokenTyp.getSpecialTyp(Lexeme);
end;

constructor TPascalSpecialSymbolToken.Create(ASource : TSource);
begin
  Inherited Create(ASource);
end;


{ TPascalStringToken }

//Extract a Pascal string token from the source.
procedure TPascalStringToken.Extract;
const
  SingleQuoteChar = #39;  // '
var
  TextBuffer: string ='';
  ValueBuffer: string = '';
  CurrChar: char;
begin
  CurrChar := NextChar;  // consume initial quote
  TextBuffer += SingleQuoteChar;
  //get string characters
  repeat
    //Replace any whitespace character with a blank.
    if CurrChar.isWhiteSpace then CurrChar := ' ';
    if (not CurrChar.isSingleQuote) and (not CurrChar.isFileEnding) then begin
      TextBuffer += CurrChar;
      ValueBuffer += CurrChar;
      CurrChar := NextChar;  // consume char
    end;
    //Quote? Two consecutive quotes represent a single quote
    if CurrChar.isSingleQuote then begin
      while CurrChar.isSingleQuote and PeekChar.isSingleQuote do begin
        TextBuffer += SingleQuoteChar + SingleQuoteChar;
        ValueBuffer += CurrChar;
        CurrChar := NextChar; // consume
        CurrChar := NextChar; //         pair of quotes
      end;
    end;
  until CurrChar.isSingleQuote or CurrChar.isFileEnding;
  if CurrChar.isSingleQuote then begin
    NextChar;  // consume final quote
    TextBuffer += SingleQuoteChar;
    Typ := ttString;
    Value := TString.Create(ValueBuffer);
  end
  else begin
    Typ := ttError;
    Value := TPascalErrorCode.Create(ecUNEXPECTED_EOF);
  end;
  Lexeme := TextBuffer;
end;

constructor TPascalStringToken.Create(ASource : TSource);
begin
  Inherited Create(ASource);
end;




end.

