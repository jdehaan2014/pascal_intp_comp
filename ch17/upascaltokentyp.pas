unit uPascalTokentyp;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, utokentype, AnyObject, uObjectUtils, fgl;

type
  { TPascalTokenTyp }


  TPascalTokenTyp = class(TAnyObject, ITokenTyp)
    public
      type
        Values = (
          // Reserved words.
          ttAnd, ttArray, ttBegin, ttCase, ttConst, ttDiv, ttDo, ttDownto, ttElse,
          ttEnd, ttFile, ttFor, ttFunction, ttGoto, ttIf, ttIn, ttLabel, ttMod,
          ttNil, ttNot, ttOf, ttOr, ttPacked, ttProcedure, ttProgram, ttRecord,
          ttRepeat, ttSet, ttThen, ttTo, ttType, ttUntil, ttVar, ttWhile, ttWith,

          // Special symbols.
          ttPlus, ttMinus, ttStar, ttSlash,
          ttColonEquals, ttDot, ttComma, ttSemicolon, ttColon, ttQuote, ttEquals,
          ttNotEquals, ttLessThan, ttLessEquals, ttGreaterEquals, ttGreaterThan,
          ttLeftParen, ttRightParen, ttLeftBracket, ttRightBracket,
          ttLeftBrace, ttRightBrace, ttUpArrow, ttDotDot,

          ttIdentifier, ttInteger, ttReal, ttString,
          ttError, ttEndOfFile, ttNone
        );

        TReservedWords = specialize TFPGList<string>;

        const
          FirstReservedIndex = ttAnd;
          LastReservedIndex  = ttWith;
          FirstSpecialIndex = ttPlus;
          LastSpecialIndex  = ttDotDot;

    private
      FValue: Values;
      function getEnum: Values;
      procedure setEnum(AValue: Values);
    public
      property Value: Values read getEnum write setEnum;

      constructor Create(AVAlue: Values);
      function toString: string; override;

      class var
        ReservedWords: TReservedWords;
        SpecialSymbols: TStringArray;

      class function toTyp(from: ITokenTyp): Values; static;
      class procedure AddReservedWords; static;
      class procedure AddSpecialSymbols; static;
      class function ValueOf(AText: string): Values; static;
      class function getSpecialTyp(AText: string): Values; static;
      class function isSpecialSymbol(AChar: char): boolean; static;

  end;


  TPascalTokenTypSet = set of TPascalTokenTyp.Values;
  TPascalTokenTypSetHelper = type helper for TPascalTokenTypSet
    function Contains(TokenTyp: ITokenTyp): boolean;
  end;

Operator := (Value: TPascalTokenTyp.Values): ITokenTyp;
Operator := (Value: ITokenTyp): TPascalTokenTyp.Values;
Operator = (Value1: ITokenTyp; Value2: TPascalTokenTyp.Values): boolean;
Operator = (Value1: TPascalTokenTyp.Values; Value2: ITokenTyp): boolean;

implementation

function TPascalTokenTypSetHelper.Contains(TokenTyp: ITokenTyp): boolean;
begin
  Result := TPascalTokenTyp(TokenTyp.getObject).Value in Self;
end;


{ Operators on TPascalTokenTyp }

Operator := (Value: TPascalTokenTyp.Values): ITokenTyp;
begin
  Result := TPascalTokenTyp.Create(Value);
end;

operator := (Value: ITokenTyp): TPascalTokenTyp.Values;
var
  Arg: TPascalTokenTyp.Values;
begin
  ReadStr(Value.toString, Arg);
  Result := TPascalTokenTyp.Create(Arg);
end;

Operator = (Value1: ITokenTyp; Value2: TPascalTokenTyp.Values): boolean;
var
  Arg: TPascalTokenTyp.Values;
begin
  ReadStr(Value1.toString, Arg);
  Result := Arg = Value2;
end;

operator = (Value1: TPascalTokenTyp.Values; Value2: ITokenTyp): boolean;
var
  Arg: TPascalTokenTyp.Values;
begin
  ReadStr(Value2.toString, Arg);
  Result := Arg = Value1;
end;



{ TPascalTokenTyp }

function TPascalTokenTyp.getEnum: Values;
begin
  Result := FValue;
end;

procedure TPascalTokenTyp.setEnum(AValue: Values);
begin
  FValue := AValue;
end;

function TPascalTokenTyp.toString: string;
begin
  WriteStr(Result, FValue);
end;

class function TPascalTokenTyp.toTyp(from: ITokenTyp): Values;
begin
  ReadStr(from.toString, Result)
end;

constructor TPascalTokenTyp.Create(AVAlue: Values);
begin
  FValue := AValue;
end;


class procedure TPascalTokenTyp.AddReservedWords;
var
  Keyword: Values;
  KeyStr: string;
begin
  ReservedWords := TReservedWords.Create;
  for Keyword := FirstReservedIndex to LastReservedIndex do begin
    WriteStr(KeyStr, Keyword);
    KeyStr := LowerCase(Copy(KeyStr, 3, Length(KeyStr)-2));
    ReservedWords.Add(KeyStr);
  end;
end;

class procedure TPascalTokenTyp.AddSpecialSymbols;
begin
  SpecialSymbols := TStringArray.Create([
    '+', '-', '*', '/', ':=', '.', ',', ';', ':', '''',
    '=', '<>', '<', '<=', '>=', '>',
    '(', ')', '[', ']', '{', '}', '^', '..'
  ]);
end;

class function TPascalTokenTyp.ValueOf(AText: string): Values;
var
  Typ: Values;
  Key: string;
begin
  Result := ttNone;
  for Typ := FirstReservedIndex to LastReservedIndex do begin
    WriteStr(Key, Typ);
    Key := Lowercase(Copy(Key, 3, 255));
    if Key = Lowercase(AText) then
      Exit(Typ);
  end;
end;

class function TPascalTokenTyp.getSpecialTyp(AText: string): Values;
var
  Index, Start: integer;
begin
  Result := ttNone;
  Start := Ord(FirstSpecialIndex);
  for Index := 0 to SpecialSymbols.Count-1 do
    if SpecialSymbols[Index] = AText then begin
      Result := Values(Start+Index);
      Break;
    end;
end;

class function TPascalTokenTyp.isSpecialSymbol(AChar: char): boolean;
begin
  Result := AChar in ['+', '-', '*', '/', ':', '.', ',', ';', '''',
                      '=', '<', '>', '(', ')', '[', ']', '{', '}', '^'];
end;



initialization
  TPascalTokenTyp.AddReservedWords;
  TPascalTokenTyp.AddSpecialSymbols;
finalization
  TPascalTokenTyp.ReservedWords.Free;
  TPascalTokenTyp.SpecialSymbols.Free;

end.

