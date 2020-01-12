unit uBackendFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubackend, uGenericInterfaces, uObjectUtils, uDebugger,
  uDebuggerType, uMemory, uDebuggerImpl;

type

  //A factory class that creates compiler and interpreter components.

  { TBackendFactory }

  TBackendFactory = class
    public
      class function CreateBackend(Operation, InputPath: string): TBackend; static;
      class function CreateDebugger(Typ: TDebuggerType; Backend: TBackend;
        RuntimeStack: IRuntimeStack): TDebugger; static;
      class function DefaultValue(Typ: ITypeSpec): TObject; static;
  end;

implementation
uses uPredefined, uCodeGen, uExecutor;


{ TBackendFactory }

//Create a compiler or an interpreter back end component.
class function TBackendFactory.CreateBackend(Operation, InputPath : string) : TBackend;
begin
  try
    if UpperCase(Operation) = 'COMPILE' then
      Result := TCodeGenerator.Create
    else if UpperCase(Operation) = 'EXECUTE' then
      Result := TExecutor.Create(InputPath)
    else
      raise Exception.Create('Backend factory: Invalid operation ' + '"' + Operation + '"')
  except
    on E: Exception do begin
      Writeln(E.Message);
      Writeln('***** Backend Factory error. *****');
    end;
  end;
end;

//Create a debugger
class function TBackendFactory.CreateDebugger(Typ: TDebuggerType; Backend: TBackend;
  RuntimeStack: IRuntimeStack): TDebugger;
begin
  Case Typ of
    dtCOMMAND_LINE: Result := TCommandLineDebugger.Create(Backend, RuntimeStack);
    dtGUI: Result := Nil
    else Result := Nil;
  end;
end;

//return the default value for a datatype
class function TBackendFactory.DefaultValue(Typ: ITypeSpec): TObject;
begin
  Typ := Typ.baseType;
  if Typ = TPredefined.integerType then
    Result := TInteger.Create(0)
  else if Typ = TPredefined.realType then
    Result := TFloat.Create(0.0)
  else if Typ = TPredefined.booleanType then
    Result := TBoolean.Create(False)
  else if Typ = TPredefined.charType then
    Result := TChar.Create('#')
  else
    Result := TString.Create('#');
end;


end.

