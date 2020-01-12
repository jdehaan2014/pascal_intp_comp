unit uMemoryImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AnyObject, uMemory, fgl, uGenericInterfaces, uTypesImpl,
  uObjectUtils, uSymTabImpl, uDefinitionImpl;

type

  //The interpreter's runtime memory cell.

  { TCellImpl }

  TCellImpl = class(TAnyObject, ICell)
    private
      FValue: TObject;   // value contained in the memory cell
    public
      constructor Create(AValue: TObject);
      destructor Destroy;
      procedure setValue(newValue: TObject);  //Set a new value into the cell.
      function getValue: TObject;  //return the value in the cell.
  end;

  { TActivationRecordImpl }

  TActivationRecordImpl = class(TAnyObject, IActivationRecord)
    private
      FRoutineID: ISymTabEntry;  // symbol table entry of the routine's name
      FLink: IActivationRecord;  // dynamic link to the previous record
      FNestingLevel: integer;    // scope nesting level of this record
      FMemoryMap: IMemoryMap;    // memory map of this stack record
    public
      constructor Create(RoutineID: ISymTabEntry);
      function getRoutineId: ISymTabEntry;  //symbol table entry of the routine's name
      function getCell(Name: String): ICell; //memory cell for the given name from the memory map
      function getAllNames: TNameList;  //list of all names in the memory map
      function getNestingLevel: integer;  //the scope nesting level
      function LinkedTo: IActivationRecord; //activation record to which this record is dynamically linked
      function MakeLinkTo(ar: IActivationRecord): IActivationRecord; //make dynamic link
  end;

  TMemoryMap = Specialize TFPGMap<string, ICell>;
  TObjects = specialize TFPGObjectList<TObject>;

  { TMemoryMapImpl }

  TMemoryMapImpl = class(TMemoryMap, IMemoryMap)
    public
      constructor Create(ASymTab: ISymTab);
      function GetObject: TObject;             //return instance of object
      function getCell(Name: string): ICell;
      function getAllNames: TNameList;
    private
      function AllocateCellValue(Typ: ITypeSpec): TObject;
      function AllocateArrayCells(Typ: ITypeSpec): TCellList;
      function AllocateRecordMap(Typ: ITypeSpec): IMemoryMap;
  end;

  { TRuntimeDisplayImpl }

  TRuntimeDisplayImpl = class(TActivationRecords, IRuntimeDisplay)
    public
      constructor Create;
      function GetObject: TObject;             //return instance of object
      function getActivationRecord(ANestingLevel: integer): IActivationRecord;
      procedure CallUpdate(ANestingLevel: integer; ar: IActivationRecord);
      procedure ReturnUpdate(NestingLevel: integer);
  end;


  { TRuntimeStackImpl }

  TRuntimeStackImpl = class(TActivationRecords, IRuntimeStack)
    private
      FDisplay: IRuntimeDisplay;
    public
      constructor Create;
      function GetObject: TObject;             //return instance of object
      function Records: TActivationRecords;
      function getTopMost(ANestingLevel: integer): IActivationRecord;
      function CurrentNestingLevel: integer;
      procedure Pop;
      procedure Push(ar: IActivationRecord);
  end;

implementation
uses uMemoryFactory;

{ TCellImpl }

constructor TCellImpl.Create(AValue: TObject);
begin
  //inherited Create;
  FValue := AValue;
end;

destructor TCellImpl.Destroy;
begin
  FValue.Free;
  Inherited;
end;

procedure TCellImpl.setValue(newValue: TObject);
begin
  FValue := newValue;
end;

function TCellImpl.getValue: TObject;
begin
  Result := FValue;
end;

{ TActivationRecordImpl }

constructor TActivationRecordImpl.Create(RoutineID: ISymTabEntry);
var
  SymTab: ISymTab;
begin
  SymTab := RoutineId.getAttribute(skRoutineSymtab) as ISymTab;
  FRoutineID := RoutineID;
  FNestingLevel := SymTab.getNestingLevel;
  FMemoryMap := TMemoryFactory.CreateMemoryMap(SymTab);
end;

function TActivationRecordImpl.getRoutineId: ISymTabEntry;
begin
  Result := FRoutineID;
end;

//memory cell for the given name from the memory map
function TActivationRecordImpl.getCell(Name: String): ICell;
begin
  Result := FMemoryMap.getCell(Name);
end;

//list of all names in the memory map
function TActivationRecordImpl.getAllNames: TNameList;
begin
  Result := FMemoryMap.getAllNames;
end;

function TActivationRecordImpl.getNestingLevel: integer;
begin
  Result := FNestingLevel;
end;

//activation record to which this record is dynamically linked
function TActivationRecordImpl.LinkedTo: IActivationRecord;
begin
  Result := FLink;
end;

//Make a dynamic link from this activation record to another one.
function TActivationRecordImpl.MakeLinkTo(ar: IActivationRecord)
  : IActivationRecord;
begin
  FLink := ar;
  Result := Self;
end;


{ TMemoryMapImpl }

//Create a memory map and allocate its memory cells based on symbol table entries
constructor TMemoryMapImpl.Create(ASymTab: ISymTab);
var
  Entries: TSymTabEntries;
  Entry: ISymTabEntry;
  defn: IDefinition;
  Name: string;
  Typ: ITypeSpec;
begin
  Inherited Create;
  Entries := ASymTab.SortedEntries;
  for Entry in Entries do begin
    defn := Entry.getDefinition;
    // Not a VAR parameter: Allocate cells for the data type in the map
    if (defn = defVariable) or (defn = defFunction) or
       (defn = defValue_Parm) or (defn = defField) then
    begin
      Name := Entry.getName;
      Typ := Entry.getTypeSpec;
      KeyData[Name] := TMemoryFactory.CreateCell(AllocateCellValue(Typ));
    end
    // VAR parameter: Allocate a single cell to hold a reference in the map
    else if defn = defVar_Parm then
    begin
      Name := Entry.getName;
      KeyData[Name] := TMemoryFactory.CreateCell(Nil);
    end;
  end;
end;

function TMemoryMapImpl.GetObject: TObject;
begin
  Result := Self;
end;

function TMemoryMapImpl.getCell(Name: string): ICell;
var
  index: integer;
begin
  index := indexOf(Name);
  if index < 0 then
    Result := Nil
  else
    Result := KeyData[Name];
end;

//Return the list of all the names.
function TMemoryMapImpl.getAllNames: TNameList;
var
  i: integer;
begin
  Result := TNameList.Create;
  for i := 0 to Count - 1 do
    Result.Add(Keys[i]);
end;

//Make an allocation for a value of a given data type for a memory cell.
function TMemoryMapImpl.AllocateCellValue(Typ: ITypeSpec): TObject;
var
  Form: ITypeForm;
begin
  Form := Typ.getForm;
  case TTypeFormImpl.toTyp(Form) of
    tfArray:  Result := AllocateArrayCells(Typ) as TObject;
    tfRecord: Result := AllocateRecordMap(Typ).getObject;
    else Result := Nil; //uninitialized scalar value
  end;
end;

//Allocate the memory cells of an array
function TMemoryMapImpl.AllocateArrayCells(Typ: ITypeSpec): TCellList;
var
  elmtCount, i: integer;
  elmtType: ITypeSpec;
  elmt: ICell;
  Allocation: TCellList;
begin
  elmtCount := TInteger(Typ.getAttribute(tkArrayElementCount)).intValue;
  elmtType := Typ.getAttribute(tkArrayElementType) as ITypeSpec;
  Allocation := TCellList.Create;
  for i := 0 to elmtCount-1 do begin
    elmt := TMemoryFactory.CreateCell(AllocateCellValue(elmtType));
    Allocation.Insert(i, elmt)
  end;
  Result := Allocation;
end;

//Allocate the memory map for a record.
function TMemoryMapImpl.AllocateRecordMap(Typ: ITypeSpec): IMemoryMap;
var
  aSymTab: ISymTab;
begin
  aSymTab := Typ.getAttribute(tkRecordSymTab) as ISymTab;
  Result := TMemoryFactory.CreateMemoryMap(aSymTab);
end;

{ TRuntimeDisplayImpl }

constructor TRuntimeDisplayImpl.Create;
begin
  inherited Create;
  Add(Nil);    // dummy element 0 (never used)
end;

function TRuntimeDisplayImpl.GetObject: TObject;
begin
  Result := Self;
end;

//Get the activation record at a given nesting level.
function TRuntimeDisplayImpl.getActivationRecord(ANestingLevel: integer)
  : IActivationRecord;
begin
  Result := Items[ANestingLevel];
end;

//Update the display for a call to a routine at a given nesting level.
procedure TRuntimeDisplayImpl.CallUpdate(ANestingLevel: integer;
  AR: IActivationRecord);
var
  PrevAr: IActivationRecord;
begin
  // Next higher nesting level: Append a new element at the top.
  if ANestingLevel >= Count then
    Add(AR)
  else
  begin
    // Existing nesting level: Set at the specified level.
    PrevAr := Items[ANestingLevel];
    Items[ANestingLevel] := AR.MakeLinkTo(PrevAr);
  end;
end;

//Update the display for a return from a routine at a given nesting level.
procedure TRuntimeDisplayImpl.ReturnUpdate(NestingLevel: integer);
var
  TopIndex: integer;
  AR, PrevAr: IActivationRecord;
begin
  TopIndex := Count - 1;
  AR := Items[NestingLevel];    // AR about to be popped off
  PrevAr := AR.LinkedTo;        // previous AR it points to
  //Point the element at that nesting level to the previous activation record.
  if PrevAr <> Nil then
    Items[NestingLevel] := prevAr
  //The top element has become null, so remove it.
  else if NestingLevel = TopIndex then
    Delete(TopIndex);
end;

{ TRuntimeStackImpl }

constructor TRuntimeStackImpl.Create;
begin
  inherited Create;
  FDisplay := TMemoryFactory.CreateRuntimeDisplay;
end;

function TRuntimeStackImpl.GetObject: TObject;
begin
  Result := Self;
end;

//return an array list of the activation records on the stack.
function TRuntimeStackImpl.Records: TActivationRecords;
begin
  Result := Self;
end;

//Get the topmost activation record at a given nesting level.
function TRuntimeStackImpl.getTopMost(ANestingLevel: integer): IActivationRecord;
begin
  Result := FDisplay.getActivationRecord(ANestingLevel);
end;

//return the current nesting level.
function TRuntimeStackImpl.CurrentNestingLevel: integer;
var
  TopIndex: integer;
begin
  TopIndex := Count - 1;
  if TopIndex >= 0 then
    Result := Items[TopIndex].getNestingLevel
  else
    Result := -1;
end;

procedure TRuntimeStackImpl.Push(AR: IActivationRecord);
var
  NestingLevel: integer;
begin
  NestingLevel := AR.getNestingLevel;
  Add(AR);
  FDisplay.CallUpdate(NestingLevel, AR);
end;

procedure TRuntimeStackImpl.Pop;
begin
  FDisplay.ReturnUpdate(CurrentNestingLevel);
  Delete(Count-1);
end;




end.

