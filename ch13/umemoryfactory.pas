unit uMemoryFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMemory, uGenericInterfaces;

type

  { TMemoryFactory }

  TMemoryFactory = class
    class function CreateRuntimeStack: IRuntimeStack;
    class function CreateRuntimeDisplay: IRuntimeDisplay;
    class function CreateActivationRecord(RoutineID: ISymTabEntry): IActivationRecord;
    class function CreateMemoryMap(ASymTab: ISymTab): IMemoryMap;
    class function CreateCell(Value: TObject): ICell;
  end;

implementation
uses uMemoryImpl;

{ TMemoryFactory }

class function TMemoryFactory.CreateRuntimeStack: IRuntimeStack;
begin
  Result := TRuntimeStackImpl.Create;
end;

class function TMemoryFactory.CreateRuntimeDisplay: IRuntimeDisplay;
begin
  Result := TRuntimeDisplayImpl.Create;
end;

class function TMemoryFactory.CreateActivationRecord(RoutineID: ISymTabEntry)
  : IActivationRecord;
begin
  Result := TActivationRecordImpl.Create(RoutineID);
end;

class function TMemoryFactory.CreateMemoryMap(ASymTab: ISymTab): IMemoryMap;
begin
  Result := TMemoryMapImpl.Create(ASymTab);
end;

class function TMemoryFactory.CreateCell(Value: TObject): ICell;
begin
  Result := TCellImpl.Create(Value);
end;

end.

