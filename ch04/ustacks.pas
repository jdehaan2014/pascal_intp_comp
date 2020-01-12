unit uStacks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, General;

type

  { TSimpleStack }

  generic TSimpleStack<T> = class
   private
    FItems: array of T;
    FCount: integer;
    FTop: integer;
    function GetItem(i: integer): T;
    function GetTop: T;
   public
    constructor Create;
    procedure DeleteAll;
    procedure Push(Value: T);
    procedure Add(Value: T);
    function Pop: T;
    property Count: integer read FCount;
    property Top: T read GetTop;
    property Items[i: integer]: T read GetItem; default;
  end;

  TValueNameItem = record
    Value: integer;
    Name : string;
  end;

  TIntegerStack   = specialize TSimpleStack<Integer>;
  TFloatStack   = specialize TSimpleStack<Real>;
  TCharStack   = specialize TSimpleStack<Char>;
  TStringStack   = specialize TSimpleStack<String>;
  TValueNameStack = specialize TSimpleStack<TValueNameItem>;

implementation

// TStack implementation

constructor TSimpleStack.Create;
begin
  FCount := 0;
  FTop := -1;
  Setlength(FItems, 0);
end;

procedure TSimpleStack.DeleteAll;
var
  i: Integer;
  Data: T;
begin
  for i := 1 to FCount do
    Data := Pop;
  SetLength(FItems, 0);
  FCount := 0;
  FTop := -1;
end;


function TSimpleStack.GetItem(i: integer): T;
begin
  Result := FItems[i];
end;

function TSimpleStack.GetTop: T;
begin
  Result := FItems[FTop];
end;

procedure TSimpleStack.Push(Value: T);
begin
  SetLength(FItems, FCount + 1);
  FItems[FCount] := Value;
  FTop := FCount;
  Inc(FCount);
end;

procedure TSimpleStack.Add(Value: T);
begin
  SetLength(FItems, FCount + 1);
  FItems[FCount] := Value;
  FTop := FCount;
  Inc(FCount);
end;

function TSimpleStack.Pop: T;
begin
  if FCount > 0 then begin
    Dec(FCount);
    Result := FItems[FCount];
    SetLength(FItems, FCount);
    FTop := FCount-1;
  end;
end;



end.

