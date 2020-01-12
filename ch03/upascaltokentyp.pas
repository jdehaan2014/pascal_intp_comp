unit uPascalTokentyp;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, utokentype, AnyObject, uObjectUtils, fgl;

type
  { TPascalTokenTyp }

  TPascalTokenTyp = class(TAnyObject, ITokenTyp)
   public
    Type
      TTokenTyp = (
        // Reserved words.
        ttAnd, ttArray, ttBegin, ttCase, ttConst, ttDiv, ttDo, ttDownto, ttElse,
        ttEnd, ttFile, ttFor, ttFunction, ttGoto, ttIf, ttIn, ttLabel, ttMod,
        ttNil, ttNot, ttOf, ttOr, ttPacked, ttProcedure, ttProgram, ttRecord,
        ttRepeat, ttSet, ttThen, ttTo, ttType, ttUntil, ttVar, ttWhile, ttWith,

        // Special symbols.
        ttPlus, ttMinus, ttStar, ttSlash,
        ttColonEquals, ttDot, ttComma, ttSemicolon, ttColon, ttQuote,
        ttEquals, ttNotEquals, ttLessThan, ttLessEquals, ttGreaterEquals, ttGreaterThan,
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

    class var
      ReservedWords: TReservedWords;
      SpecialSymbols: TStringArray;

    class procedure AddReservedWords; static;
    class procedure AddSpecialSymbols; static;
    class function ValueOf(AText: string): TTokenTyp; static;
    class function getSpecialTyp(AText: string): TTokenTyp; static;
    class function isSpecialSymbol(AChar: char): boolean; static;

   private
    FValue: TTokenTyp;
   public
    function getObject: TObject;    // return object instance
    function toString: string;
    class constructor Create;
    constructor Create(AValue: TTokenTyp);
    property Value: TTokenTyp read FValue;
  end;

Operator := (T1: TPascalTokenTyp.TTokenTyp): TPascalTokenTyp;
Operator := (T1: TPascalTokenTyp.TTokenTyp): ITokenTyp;
Operator := (T1: ITokenTyp): TPascalTokenTyp.TTokenTyp;
Operator = (T1: ITokenTyp; T2: TPascalTokenTyp.TTokenTyp): boolean;
Operator = (T1: TPascalTokenTyp.TTokenTyp; T2: ITokenTyp): boolean;

implementation

Operator := (T1: TPascalTokenTyp.TTokenTyp): TPascalTokenTyp;
begin
  Result := TPascalTokenTyp.Create(T1);
end;

Operator := (T1: TPascalTokenTyp.TTokenTyp): ITokenTyp;
begin
  Result := TPascalTokenTyp.Create(T1);
end;

operator := (T1: ITokenTyp): TPascalTokenTyp.TTokenTyp;
var
  Arg: TPascalTokenTyp.TTokenTyp;
begin
  ReadStr(T1.toString, Arg);
  Result := TPascalTokenTyp.Create(Arg);
end;

Operator = (T1: ITokenTyp; T2: TPascalTokenTyp.TTokenTyp): boolean;
var
  Arg: TPascalTokenTyp.TTokenTyp;
begin
  ReadStr(T1.toString, Arg);
  Result := Arg = T2;
end;

operator = (T1: TPascalTokenTyp.TTokenTyp; T2: ITokenTyp): boolean;
var
  Arg: TPascalTokenTyp.TTokenTyp;
begin
  ReadStr(T2.toString, Arg);
  Result := Arg = T1;
end;

{ TPascalTokenTyp }

class procedure TPascalTokenTyp.AddReservedWords;
var
  Keyword: TTokenTyp;
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

class function TPascalTokenTyp.ValueOf(AText: string): TTokenTyp;
var
  Typ: TTokenTyp;
  Key: string;
begin
  Result := ttNone;
  for Typ := FirstReservedIndex to LastReservedIndex do begin
    WriteStr(Key, Typ);
    Key := Lowercase(Copy(Key, 3, Length(Key)-2));
    if Key = Lowercase(AText) then
      Exit(Typ);
  end;
end;

class function TPascalTokenTyp.getSpecialTyp(AText: string): TTokenTyp;
var
  Index, Start: integer;
begin
  Result := ttNone;
  Start := Ord(FirstSpecialIndex);
  for Index := 0 to SpecialSymbols.Count-1 do
    if SpecialSymbols[Index] = AText then begin
      Result := TTokenTyp(Start+Index);
      Break;
    end;
end;

class function TPascalTokenTyp.isSpecialSymbol(AChar: char): boolean;
begin
  Result := AChar in ['+', '-', '*', '/', ':', '.', ',', ';', '''',
                      '=', '<', '>', '(', ')', '[', ']', '{', '}', '^'];
end;

function TPascalTokenTyp.getObject: TObject;
begin
  Result := Self;
end;

function TPascalTokenTyp.toString: string;
begin
  WriteStr(Result, self.FValue);
end;

class constructor TPascalTokenTyp.Create;
begin
  AddReservedWords;
  AddSpecialSymbols;
end;

constructor TPascalTokenTyp.Create(AValue: TTokenTyp);
begin
  FValue := AValue;
end;

end.

