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
      constructor Create(AVAlue: TEnumType);
  end;


{
uses RTTI
class function GetName<T>(AValue: T):string;
class function GetValue<T>(AName: string): T;

TRttiEnumerationType.GetName...
}

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

constructor TEnum.Create(AVAlue: TEnumType);
begin
  FValue := AValue;
end;


end.

