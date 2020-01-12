unit AnyObject;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils;

type
  IAnyInterface = Interface
    ['IAnyInterface']
    function getObject: TObject;
    function toString: string;
  end;

  { TAnyObject }

  TAnyObject = class(TObject, IAnyInterface)
    function getObject: TObject;
  end;

  generic IEnum<T> = interface(IAnyInterface)
    ['IEnumTyp']
    function getEnum: T;
    procedure setEnum(AValue: T);
    property Value: T read getEnum write setEnum;
    function toPrtStr: string;
  end;

  { TEnumTyp }

  generic TEnum<TEnumType> = class(TAnyObject, specialize IEnum<TEnumType>)
    public
      type
        Values = TEnumType;
    private
      FValue: TEnumType;
      function getEnum: TEnumType;
      procedure setEnum(AValue: TEnumType);
    public
      property Value: TEnumType read getEnum write setEnum;
      function toString: string; override;
      function getObject: TObject;
      constructor Create(AVAlue: TEnumType);
      function toPrtStr: string;
  end;


implementation

{ TAnyObject }

function TAnyObject.getObject: TObject;
begin
  Result := Self;
end;

{ TEnum }

function TEnum.getEnum: TEnumType;
begin
  Result := FValue;
end;

procedure TEnum.setEnum(AValue: TEnumType);
begin
  FValue := AValue;
end;

function TEnum.toString: string;
begin
  WriteStr(Result, FValue);
end;

function TEnum.getObject: TObject;
begin
  Result := Self;
end;

constructor TEnum.Create(AVAlue: TEnumType);
begin
  FValue := AValue;
end;

function TEnum.toPrtStr: string;
begin
  Result := Copy(Self.toString, 3, 255);
end;


end.

