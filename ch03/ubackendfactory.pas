unit uBackendFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubackend, uCodeGen, uExecutor;

type

  //A factory class that creates compiler and interpreter components.

  { TBackendFactory }

  TBackendFactory = class
    public
      class function CreateBackend(Operation: string): TBackend;
  end;

implementation

{ TBackendFactory }

//Create a compiler or an interpreter back end component.
class function TBackendFactory.CreateBackend(Operation : string) : TBackend;
begin
  if UpperCase(Operation) = 'COMPILE' then
    Result := TCodeGenerator.Create
  else if UpperCase(Operation) = 'EXECUTE' then
    Result := TExecutor.Create
  else
    raise Exception.Create('Backend factory: Invalid operation ' + '"' + Operation + '"')

end;

end.

