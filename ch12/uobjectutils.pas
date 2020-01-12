unit uObjectUtils;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils;


const Debug = False;

//generic function Question<T>(Test: Boolean; IfTrue, IfFalse: T): T;

type

{ TDynArray }

  Generic TDynArray<T> = class
    private
      FCount: integer;
      FItems: array of T;
      function getItem(i: integer): T;
      procedure setItem(i: integer; Value: T);
      //function Equals(Index: integer; const Item: T): boolean;
    public
      property Count: integer read FCount;
      property Items[i: integer]: T read getItem write setItem; default;
      constructor Create;
      constructor Create(Elements: Array of T);
      procedure Add(const Item: T);
      //function IndexOf(const Item: T): integer;
  end;

  TStringArray = specialize TDynArray<string>;
  TIntArray = specialize TDynArray<integer>;
  TFloatArray = specialize TDynArray<real>;
  TBoolArray = specialize TDynArray<boolean>;
  TObjectArray = specialize TDynArray<TObject>;


  { TString }

  TString = class
    private
      FValue: string;
      procedure setValue(Value: string);
      function getItem(i: integer): char;
      procedure setItem(i: integer; AValue: char);
    public
      property Value: string read FValue write setValue;
      property Items[i: Integer]: char read getItem write setItem; default;
      constructor Create;
      constructor Create(AValue: string);
      constructor Create(AChar: char; Count: integer);
      function CharAt(const index: integer): char;
      function Contains(const S: string): boolean;
      function Contains(const C: char): boolean;
      function Equals(S: TString): boolean;
      function Format(const Fmt: string; const Args: array of Const): TString;
      function isEmpty: boolean;
      function Length: integer;
      function toLowerCase: TString;
      function toUpperCase: TString;
      Function toString: string; override;
      class function ValueOf(const B: boolean): TString;
      class function ValueOf(const C: char): TString;
      class function ValueOf(const R: real): TString;
      class function ValueOf(const I: integer): TString;
  end;

  TStringObjArray = specialize TDynArray<TString>;


  { TBoolean }

  TBoolean = class
    private
      FValue: boolean;
      procedure setValue(Value: boolean);
    public
      property Value: boolean read FValue write setValue;
      constructor Create;
      constructor Create(AValue: boolean);
      function Equals(B: TBoolean): boolean;
      function boolValue: boolean;
      Function toString: string; override;
      class function ValueOf(const B: boolean): TBoolean;
      class function ValueOf(const S: string): TBoolean;
  end;

  TBoolObjArray = specialize TDynArray<TBoolean>;

  { TNumber }

  TNumber = class
    private
      byteVal: byte;
    public
      constructor Create; virtual; abstract;
      function byteValue: byte; virtual;
      function doubleValue: double; virtual; abstract;
      function floatValue: real; virtual; abstract;
      function intValue: integer; virtual; abstract;
      function longValue: longint; virtual; abstract;
      //function shortValue: shortint;
  end;

  { TInteger }

  TInteger = class(TNumber)
    private
      FValue: integer;
      procedure setValue(Value: integer);
    public
      property Value: integer read FValue write setValue;
      constructor Create; override;
      constructor Create(AValue: integer);
      constructor Create(S: TString);
      function byteValue: byte; override;
      function doubleValue: double; override;
      function floatValue: real; override;
      function intValue: integer; override;
      function longValue: longint; override;
      function Equals(I: TInteger): boolean;
      Function toString: string; override;
      function toFloat: double;
      class function ValueOf(const I: integer): TInteger;
      class function ValueOf(const S: string): TInteger;
      class function ParseInt(const S: string): integer;
  end;

  TIntObjArray = specialize TDynArray<TInteger>;

  { TFloat }

  TFloat = class(TNumber)
    private
      FValue: real;
      procedure setValue(Value: real);
    public
      property Value: real read FValue write setValue;
      constructor Create; override;
      constructor Create(AValue: real);
      constructor Create(S: TString);
      function byteValue: byte; override;
      function doubleValue: double; override;
      function floatValue: real; override;
      function intValue: integer; override;
      function longValue: longint; override;
      function Equals(F: TFloat): boolean;
      Function toString: string; override;
      class function ValueOf(const F: real): TFloat;
      class function ValueOf(const S: string): TFloat;
  end;

  TFloatObjArray = specialize TDynArray<TFloat>;

  { TChar }

  TChar = class
    private
      FValue: char;
      procedure setValue(AValue: char);
    public
      property Value: char read FValue write setValue;
      constructor Create(AValue: char);
      function charValue: char;
      Function toString: string; override;
      class function ValueOf(const C: char): TChar;
  end;

  // TString operations
  operator := (s1: string): TString;
  operator := (s1: TString): String;
  Operator +  (s1: TString; s2: TString) : TString;
  Operator +  (s1: TString; s2: String) : TString;
  Operator +  (s1: string; s2: TString) : String;

  // TBoolean operations
  operator :=  (b: boolean): TBoolean;
  operator :=  (b: TBoolean): boolean;
  operator and (b1: TBoolean; b2: TBoolean): TBoolean;
  operator or  (b1: TBoolean; b2: TBoolean): TBoolean;
  operator not (b: TBoolean): TBoolean;

  // TInteger operations
  operator := (i: integer): TInteger;
  operator := (i: TInteger): integer;
  Operator +  (i1: TInteger; i2: TInteger) : TInteger;
  Operator +  (i1: TInteger; i2: integer) : TInteger;
  Operator +  (i1: integer; i2: TInteger) : integer;
  Operator -  (i1: TInteger; i2: TInteger) : TInteger;
  Operator -  (i1: TInteger; i2: integer) : TInteger;
  Operator -  (i1: integer; i2: TInteger) : integer;
  Operator *  (i1: TInteger; i2: TInteger) : TInteger;
  Operator *  (i1: TInteger; i2: integer) : TInteger;
  Operator *  (i1: integer; i2: TInteger) : integer;
  Operator div  (i1: TInteger; i2: TInteger) : TInteger;
  Operator div  (i1: TInteger; i2: integer) : TInteger;
  Operator div  (i1: integer; i2: TInteger) : integer;

  // TFloat operations
  operator := (r: real): TFloat;
  operator := (r: TFloat): real;
  Operator +  (r1: TFloat; r2: TFloat) : TFloat;
  Operator +  (r1: TFloat; r2: real) : TFloat;
  Operator +  (r1: real; r2: TFloat) : real;
  Operator -  (r1: TFloat; r2: TFloat) : TFloat;
  Operator -  (r1: TFloat; r2: real) : TFloat;
  Operator -  (r1: real; r2: TFloat) : real;
  Operator *  (r1: TFloat; r2: TFloat) : TFloat;
  Operator *  (r1: TFloat; r2: real) : TFloat;
  Operator *  (r1: real; r2: TFloat) : real;
  Operator /  (r1: TFloat; r2: TFloat) : TFloat;
  Operator /  (r1: TFloat; r2: real) : TFloat;
  Operator /  (r1: real; r2: TFloat) : real;

  // TChar operations
  operator := (c: char): TChar;
  operator := (c: TChar): char;

{Generic function IIF<t>(Const Cond: Boolean; Const ATrueValue, AFalseValue: T)
  : t; inline;}

implementation
uses TypInfo;

{generic function Question<T>(Test: Boolean; IfTrue, IfFalse: T): T;
  begin
    if Test then
      Result := IfTrue
    else
      Result := IfFalse;
  end;  }


{ TDynArray }

function TDynArray.getItem(i: integer): T;
begin
  Result := FItems[i];
end;

procedure TDynArray.setItem(i: integer; Value: T);
begin
  FItems[i] := Value;
end;

constructor TDynArray.Create;
begin
  SetLength(FItems, 0);
  FCount := 0;
end;

constructor TDynArray.Create(Elements: array of T);
var
  i: integer;
begin
  if High(Elements) < 0 then begin
    //no arguments are passed
    SetLength(FItems, 0);
    FCount := 0;
  end
  else begin
    SetLength(FItems, High(Elements) + 1);
    FCount := Length(FItems);
    for i := 0 to High(Elements) do
      FItems[i] := Elements[i];
  end;
end;

procedure TDynArray.Add(const Item: T);
begin
  SetLength(FItems, FCount+1);
  FItems[FCount] := Item;
  Inc(FCount);
end;

{ TString }

procedure TString.setValue(Value: string);
begin
  FValue := Value;
end;

function TString.getItem(i: integer): char;
begin
  Result := FValue[i];
end;

procedure TString.setItem(i: integer; AValue: char);
begin
  FValue[i] := AValue;
end;

constructor TString.Create;
begin
  FValue := '';
end;

constructor TString.Create(AValue: string);
begin
  FValue := AValue;
end;

constructor TString.Create(AChar: char; Count: integer);
begin
  FValue := StringOfChar(AChar, Count);
end;

function TString.CharAt(const index: integer): char;
begin
  Result := FValue[index];
end;

function TString.Contains(const S: string): boolean;
begin
  Result := Pos(S, FValue) > 0;
end;

function TString.Contains(const C: char): boolean;
begin
  Result := Pos(C, FValue) > 0;
end;

function TString.Equals(S: TString): boolean;
begin
  Result := FValue = S.Value;
end;

function TString.Format(const Fmt: string; const Args: array of const): TString;
begin
  FValue := SysUtils.Format(Fmt, Args);
  Result := Self;
end;

function TString.isEmpty: boolean;
begin
  Result := System.Length(FValue) = 0
end;

function TString.Length: integer;
begin
  Result := System.Length(FValue);
end;

function TString.toLowerCase: TString;
begin
  FValue := LowerCase(FValue);
  Result := Self;
end;

function TString.toString: string;
begin
  Result := Self.FValue;
end;

function TString.toUpperCase: TString;
begin
  FValue := UpperCase(FValue);
  Result := Self;
end;

class function TString.ValueOf(const B: boolean): TString;
var
  AValue: string;
begin
  AValue := BoolToStr(B);
  Result := TString.Create(AValue);
end;

class function TString.ValueOf(const C: char): TString;
var
  AValue: string;
begin
  WriteStr(AValue, C);
  Result := TString.Create(AValue);
end;

class function TString.ValueOf(const R: real): TString;
var
  AValue: string;
begin
  AValue := FloatToStr(R);
  Result := TString.Create(AValue);
end;

class function TString.ValueOf(const I: integer): TString;
var
  AValue: string;
begin
  AValue := IntToStr(I);
  Result := TString.Create(AValue);
end;


{ TBoolean }

procedure TBoolean.setValue(Value: boolean);
begin
  FValue := Value;
end;

constructor TBoolean.Create;
begin
  FValue := False;
end;

constructor TBoolean.Create(AValue: boolean);
begin
  FValue := AValue;
end;

function TBoolean.Equals(B: TBoolean): boolean;
begin
  Result := FValue = B.Value;
end;

function TBoolean.boolValue: boolean;
begin
  Result := FValue;
end;

function TBoolean.toString: string;
begin
  Result := BoolToStr(FValue, True);
end;

class function TBoolean.ValueOf(const B: boolean): TBoolean;
begin
  Result := TBoolean.Create(B);
end;

class function TBoolean.ValueOf(const S: string): TBoolean;
begin
  if UpperCase(S) = 'TRUE' then
    Result := TBoolean.Create(True)
  else
    Result := TBoolean.Create(False);
end;


{ TNumber }

function TNumber.byteValue: byte;
begin
  Result := ByteVal;
end;


{ TInteger }

procedure TInteger.setValue(Value: integer);
begin
  FValue := Value;
end;

constructor TInteger.Create;
begin
  FValue := 0;
end;

constructor TInteger.Create(AValue: integer);
begin
  FValue := AValue;
end;

constructor TInteger.Create(S: TString);
begin
  if not TryStrToInt(S.Value, FValue) then
    Self := Nil;
end;

function TInteger.byteValue: byte;
begin
  Result := Byte(FValue);
end;

function TInteger.doubleValue: double;
begin
  Result := Double(FValue);
end;

function TInteger.floatValue: real;
begin
  Result := Single(FValue);
end;

function TInteger.intValue: integer;
begin
  Result := FValue;
end;

function TInteger.longValue: longint;
begin
  Result := LongInt(FValue);
end;

function TInteger.Equals(I: TInteger): boolean;
begin
  Result := FValue = I.Value;
end;

function TInteger.toString: string;
begin
  Result := IntToStr(FValue);
end;

function TInteger.toFloat: double;
begin
  Result := Double(FValue);
end;

class function TInteger.ValueOf(const I: integer): TInteger;
begin
  Result := TInteger.Create(I);
end;

class function TInteger.ValueOf(const S: string): TInteger;
var
  AValue: integer;
begin
  if TryStrToInt(S, AValue) then
    Result := TInteger.Create(AValue)
  else
    Result := Nil
end;

class function TInteger.ParseInt(const S: string): integer;
begin
  Result := strToInt(S);
end;

{ TFloat }

procedure TFloat.setValue(Value: real);
begin
  FValue := Value;
end;

constructor TFloat.Create;
begin
  FValue := 0.0;
end;

constructor TFloat.Create(AValue: real);
begin
  FValue := AValue;
end;

constructor TFloat.Create(S: TString);
begin
  if not TryStrToFloat(S.Value, FValue) then
    Self := Nil;
end;

function TFloat.byteValue: byte;
begin
  Result := Byte(Trunc(FValue));
end;

function TFloat.doubleValue: double;
begin
  Result := Double(FValue);
end;

function TFloat.floatValue: real;
begin
  Result := Real(FValue);
end;

function TFloat.intValue: integer;
begin
  Result := Integer(Trunc(FValue));
end;

function TFloat.longValue: longint;
begin
  Result := LongInt(Trunc(FValue));
end;

function TFloat.Equals(F: TFloat): boolean;
begin
  Result := FValue = F.Value;
end;

function TFloat.toString: string;
begin
  Result := FloatToStr(FValue);
end;

class function TFloat.ValueOf(const F: real): TFloat;
begin
  Result := TFloat.Create(F);
end;

class function TFloat.ValueOf(const S: string): TFloat;
var
  AValue: real;
begin
  if TryStrToFloat(S, AValue) then
    Result := TFloat.Create(AValue)
  else
    Result := Nil
end;

{ TChar }

procedure TChar.setValue(AValue: char);
begin
  FValue := AValue;
end;

constructor TChar.Create(AValue: char);
begin
  FValue := AValue;
end;

function TChar.charValue: char;
begin
  Result := FValue;
end;

function TChar.toString: string;
begin
  Result := FValue;
end;

class function TChar.ValueOf(const C: char): TChar;
begin
  Result := TChar.Create(C);
end;


{ TString operators }
operator := (s1: string): TString;
begin
  Result := TString.Create(s1);
end;

operator:=(s1: TString): String;
begin
  Result := s1.toString;
end;

operator + (s1: TString; s2: TString): TString;
begin
  Result := TString.Create(s1.Value + s2.Value);
end;

operator + (s1: TString; s2: String): TString;
begin
  Result := TString.Create(s1.Value + s2);
end;

operator + (s1: string; s2: TString): String;
begin
  Result := s1 + s2.Value;
end;

operator := (b: boolean): TBoolean;
begin
  Result := TBoolean.Create(b);
end;

operator := (b: TBoolean): boolean;
begin
  result := b.Value;
end;

operator and(b1: TBoolean; b2: TBoolean): TBoolean;
begin
  Result := TBoolean.Create(b1.Value and b2.Value);
end;

operator or(b1: TBoolean; b2: TBoolean): TBoolean;
begin
  Result := TBoolean.Create(b1.Value or b2.Value);
end;

operator not(b: TBoolean): TBoolean;
begin
  Result := TBoolean.Create(not b.Value);
end;

// Integer operators

operator := (i: integer): TInteger;
begin
  Result := TInteger.Create(i);
end;

operator:=(i: TInteger): integer;
begin
  Result := i.Value;
end;

operator + (i1: TInteger; i2: TInteger): TInteger;
begin
  Result := TInteger.Create(i1.Value + i2.Value);
end;

operator + (i1: TInteger; i2: integer): TInteger;
begin
  Result := TInteger.Create(i1.Value + i2);
end;

operator + (i1: integer; i2: TInteger): integer;
begin
  Result := i1 + i2.Value;
end;

operator - (i1: TInteger; i2: TInteger): TInteger;
begin
  Result := TInteger.Create(i1.Value - i2.Value);
end;

operator - (i1: TInteger; i2: integer): TInteger;
begin
  Result := TInteger.Create(i1.Value - i2);
end;

operator - (i1: integer; i2: TInteger): integer;
begin
  Result := i1 - i2.Value;
end;

operator * (i1: TInteger; i2: TInteger): TInteger;
begin
  Result := TInteger.Create(i1.Value * i2.Value);
end;

operator * (i1: TInteger; i2: integer): TInteger;
begin
  Result := TInteger.Create(i1.Value * i2);
end;

operator * (i1: integer; i2: TInteger): integer;
begin
  Result := i1 * i2.Value;
end;

operator div(i1: TInteger; i2: TInteger): TInteger;
begin
  Result := TInteger.Create(i1.Value div i2.Value);
end;

operator div(i1: TInteger; i2: integer): TInteger;
begin
  Result := TInteger.Create(i1.Value div i2);
end;

operator div(i1: integer; i2: TInteger): integer;
begin
  Result := i1 div i2.Value;
end;

// Float operators

operator := (r: real): TFloat;
begin
  Result := TFloat.Create(r);
end;

operator := (r: TFloat): real;
begin
  Result := r.Value;
end;

operator + (r1: TFloat; r2: TFloat): TFloat;
begin
  Result := TFloat.Create(r1.Value + r2.Value);
end;

operator + (r1: TFloat; r2: real): TFloat;
begin
  Result := TFloat.Create(r1.Value + r2);
end;

operator + (r1: real; r2: TFloat): real;
begin
  Result := r1 + r2.Value;
end;

operator - (r1: TFloat; r2: TFloat): TFloat;
begin
  Result := TFloat.Create(r1.Value - r2.Value);
end;

operator - (r1: TFloat; r2: real): TFloat;
begin
  Result := TFloat.Create(r1.Value - r2);
end;

operator - (r1: real; r2: TFloat): real;
begin
  Result := r1 - r2.Value;
end;

operator * (r1: TFloat; r2: TFloat): TFloat;
begin
  Result := TFloat.Create(r1.Value * r2.Value);
end;

operator * (r1: TFloat; r2: real): TFloat;
begin
  Result := TFloat.Create(r1.Value * r2);
end;

operator * (r1: real; r2: TFloat): real;
begin
  Result := r1 * r2.Value;
end;

operator / (r1: TFloat; r2: TFloat): TFloat;
begin
  Result := TFloat.Create(r1.Value / r2.Value);
end;

operator / (r1: TFloat; r2: real): TFloat;
begin
  Result := TFloat.Create(r1.Value / r2);
end;

operator / (r1: real; r2: TFloat): real;
begin
  Result := r1 / r2.Value;
end;

operator := (c: char): TChar;
begin
  Result := TChar.Create(c);
end;

operator := (c: TChar): char;
begin
  Result := c.Value;
end;



end.


