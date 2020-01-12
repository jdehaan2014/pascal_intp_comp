unit uTypesFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uGenericInterfaces;

type

  { TTypeFactory }

  TTypeFactory = class
    class function CreateType(Form: ITypeForm): ITypeSpec; static;
    class function CreateStringType(Value: string): ITypeSpec; static;
  end;

implementation
uses uTypesImpl;

{ TTypeFactory }

//Create a type specification of a given form
class function TTypeFactory.CreateType(Form: ITypeForm): ITypeSpec;
begin
  Result := TTypeSpecImpl.Create(Form);
end;

//Create a string type specification
class function TTypeFactory.CreateStringType(Value: string): ITypeSpec;
begin
  Result := TTypeSpecImpl.Create(Value);
end;

end.

