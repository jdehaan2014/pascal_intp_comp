unit uDefinitionImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uGenericInterfaces, AnyObject, uObjectUtils;

type

  { TDefinitionImpl }

  TDefinitionImpl = class(TAnyObject, IDefinition)
    public
      type
        Values = (defConstant, defEnumeration_Constant, defType, defVariable,
                  defField, defValue_Parm, defVar_Parm, defProgram_Parm,
                  defProgram, defProcedure, defFunction,
                  defUndefined, defNone);
    private
      FText: string;
      FValue: Values;
    public
      property Value: Values read FValue;
      function getText: string;
      function toString: string; override;
      constructor Create(AValue: Values);
      class function toTyp(from: IDefinition): Values;
  end;

Operator := (T1: TDefinitionImpl.Values): TDefinitionImpl;
Operator := (T1: TDefinitionImpl.Values): IDefinition;
operator := (T1: IDefinition): TDefinitionImpl.Values;
operator = (T1: IDefinition; T2: TDefinitionImpl.Values): boolean;
operator = (T1: TDefinitionImpl.Values; T2: IDefinition): boolean;


implementation

operator:=(T1: TDefinitionImpl.Values): TDefinitionImpl;
begin
  Result := TDefinitionImpl.Create(T1);
end;

operator:=(T1: TDefinitionImpl.Values): IDefinition;
begin
  Result := TDefinitionImpl.Create(T1);
end;

operator:=(T1: IDefinition): TDefinitionImpl.Values;
var
  Arg: TDefinitionImpl.Values;
begin
  ReadStr(T1.toString, Arg);
  Result := TDefinitionImpl.Create(Arg);
end;

operator=(T1: IDefinition; T2: TDefinitionImpl.Values): boolean;
var
  Arg: TDefinitionImpl.Values;
begin
  ReadStr(T1.toString, Arg);
  Result := Arg = T2;
end;

operator=(T1: TDefinitionImpl.Values; T2: IDefinition): boolean;
var
  Arg: TDefinitionImpl.Values;
begin
  ReadStr(T2.toString, Arg);
  Result := Arg = T1;
end;

{ TDefinitionImpl }

function TDefinitionImpl.getText: string;
begin
  Result := Lowercase(FText);
end;

function TDefinitionImpl.toString: string;
begin
  //Result := UpperCase(FText);
  WriteStr(Result, FValue);
end;

constructor TDefinitionImpl.Create(AValue: Values);
begin
  FValue := AValue;
  WriteStr(FText, AValue);
  FText := Copy(FText, 4, 255);
end;

class function TDefinitionImpl.toTyp(from: IDefinition): Values;
begin
  ReadStr(from.toString, Result)
end;


end.

