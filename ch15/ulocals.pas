unit uLocals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  //List of booleans to keep track of reserved local variables. The i-th
  //element is true if the i-th variable is being used, else it's false.
  //The final size of the list is the total nr of local vars used by the method.
  TReservedList = Specialize TFPGList<boolean>;

  //Maintain a method's local variable array

  { TLocalVariables }

  TLocalVariables = class
    private
      Reserved: TReservedList;
    public
      constructor Create(Index: Integer);
      function Reserve: Integer;
      procedure Release(Index: Integer);
      function Count: Integer;
  end;

  //Maintain a method's local runtime stack.

  { TLocalStack }

  TLocalStack = class
    private
      Size: Integer;
      maxSize: Integer;
    public
      property getSize: Integer read Size;  // return current stack size
      property Capacity: Integer read maxSize; //return max attained stack size
      Constructor Create;
      procedure Increase(Amount: Integer);
      procedure Decrease(Amount: Integer);
      procedure Use(Amount: Integer);
      procedure Use(AmountIncrease, AmountDecrease: Integer);
  end;

  //Jasmin instruction label.

  { TLabel }

  TLabel = class
    private
      class var Index: Integer;   //index for generating label strings.
      var Caption: String;        //the label string.
    public
      class constructor Create;
      Constructor Create;
      function newLabel: TLabel;
      function toString: String; override;
  end;

implementation
uses math;

{ TLocalVariables }

constructor TLocalVariables.Create(Index: Integer);
var
  i: Integer;
begin
  Reserved := TReservedList.Create;
  //initially reserve local vars 0 to index-1
  for i := 0 to Index-1 do
    Reserved.Add(True);
end;

//Reserve a local var
function TLocalVariables.Reserve: Integer;
var
  i: Integer;
begin
  //Search for existing but unreserved local var.
  for i := 0 to Reserved.Count-1 do
    if not Reserved[i] then begin
      Reserved[i] := True;
      Exit(i);
    end;
  //Reserved a new var
  Reserved.Add(True);
  Result := Reserved.Count-1;
end;

//Release a local var that's no longer needed.
procedure TLocalVariables.Release(Index: Integer);
begin
  Reserved[Index] := False;
end;

function TLocalVariables.Count: Integer;
begin
  Result := Reserved.Count;
end;

{ TLocalStack }

constructor TLocalStack.Create;
begin
  Size := 0;
  maxSize := 0;
end;

//increase the stack size by a given amount
procedure TLocalStack.Increase(Amount: Integer);
begin
  Size += Amount;
  maxSize := Max(maxSize, Size);
end;

//decrease the stack size by a given amount
procedure TLocalStack.Decrease(Amount: Integer);
begin
  Size -= Amount;
end;

//increase and decrease the stack size by the same amount
procedure TLocalStack.Use(Amount: Integer);
begin
  Increase(Amount);
  Decrease(Amount);
end;

//increase and decrease the stack size by the different amounts
procedure TLocalStack.Use(AmountIncrease, AmountDecrease: Integer);
begin
  Increase(AmountIncrease);
  Decrease(AmountDecrease);
end;

{ TLabel }

class constructor TLabel.Create;
begin
  Index := 0;
end;

constructor TLabel.Create;
begin
  Inc(Index);
  Caption := 'L' + Format('%.3d', [Index]);
end;

function TLabel.newLabel: TLabel;
begin
  Result := TLabel.Create;
end;

function TLabel.toString: String;
begin
  Result := Caption;
end;



end.

