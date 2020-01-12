unit uSymtabImpl;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, uGenericInterfaces, uSymtabFactory, fgl, AnyObject;

type

  { TSymtabStackImpl }

  //An implementation of the symbol table stack.
  TSymtabStackImpl = class(specialize TFPGList<ISymtab>, ISymtabStack)
    private
      CurrentNestingLevel: integer;  // current scope nesting level
      ProgramID: ISymTabEntry;       // entry for the main program ID
    public
      function getObject: TObject;
      function toString: string; override;
      constructor Create;
      procedure setProgramID(ID: ISymTabEntry);
      function getProgramID: ISymTabEntry;
      function getCurrentNestingLevel: integer;
      function getLocalSymtab: ISymtab;
      function Push: ISymTab;                       //Push a new symtable onto the stack
      function Push(SymTab: ISymTab): ISymTab;      //Push a symtable onto the stack
      function Pop: ISymTab;                        //pop symtable off the stack
      function EnterLocal(Name: string): ISymtabEntry;
      function LookupLocal(Name: string): ISymtabEntry;
      function Lookup(Name: string): ISymtabEntry;
  end;


  TSymTabEntryMap = specialize TFPGMap<String, ISymtabEntry>;

  { TSymTabImpl }

  //An implementation of the symbol table.
  TSymTabImpl = class(TSymTabEntryMap, ISymtab)
    private
      FNestingLevel: integer;
    public
      function toString: string; override;
      function getObject: TObject;
      constructor Create(ANestingLevel: integer);
      function Enter(Name: string): ISymtabEntry;
      function Lookup(Name: string): ISymtabEntry;
      function SortedEntries: TSymTabEntries;
      function getNestingLevel: integer;
  end;


  //Attribute keys for a symbol table entry.

  { TSymtabKeyImpl }

  TSymtabKeyImpl = class(TAnyObject, ISymTabKey)
    public
      type
        Values = (
          skConstantValue,            //constant
          skRoutineCode, skRoutineSymtab, skRoutineICode, skRoutineParms,
          skRoutineRoutines,          //procedure or function
          skDataValue,                //variable or record field value
          skSlot, skWrapSlot          //local variables array slot numbers
        );
    private
      FValue: Values;
      function getEnum: Values;
      procedure setEnum(AValue: Values);
    public
      property Value: Values read getEnum write setEnum;
      function toString: string; override;
      class function toTyp(from: ISymtabKey): Values; static;
      constructor Create(AVAlue: Values);
  end;

Operator := (T1: TSymtabKeyImpl.Values): TSymtabKeyImpl;
Operator := (T1: TSymtabKeyImpl.Values): ISymtabKey;
Operator := (T1: ISymtabKey): TSymtabKeyImpl.Values;
Operator = (T1: ISymtabKey; T2: TSymtabKeyImpl.Values): boolean;


type

  TAttributeMap = specialize TFPGMap<TSymtabKeyImpl.Values, TObject>;

  //An implementation of a symbol table entry.

  { TSymTabEntryImpl }

  TSymTabEntryImpl = class(TAttributeMap, ISymtabEntry)
    private
      FName: string;         // entry name
      FSymtab: ISymtab;      // parent symbol table
      FDefinition: IDefinition; //how the identifier is defined
      FTypeSpec: ITypeSpec;  //type specification
      FLineNumbers: TLineNumbers;  // source line numbers
    public
      function toString: string; override;
      function getObject: TObject;
      constructor Create(AName: string; ASymTab: ISymtab);
      destructor Destroy;
      function getName: string;
      function getSymtab: ISymtab;
      procedure setDefinition(ADefinition: IDefinition);
      function getDefinition: IDefinition;
      procedure setTypeSpec(ATypeSpec: ITypeSpec);
      function getTypeSpec: ITypeSpec;
      procedure AppendLineNumber(LineNumber: integer); //append source line num
      function getLineNumbers: TLineNumbers;
      procedure setAttribute(Key: ISymtabKey; Value: TObject); //set attr of entry
      function getAttribute(Key: ISymtabKey): TObject; //get the attr's value of the entry
  end;


  { TRoutineCodeImpl }

  TRoutineCodeImpl = class(TAnyObject, IRoutineCode)
    public
      type
        Values = (
          rcDeclared, rcForward,
          rcRead, rcReadLn, rcWrite, rcWriteLn,
          rcABS, rcARCTAN, rcCHR, rcCOS, rcEOF, rcEOLN, rcEXP, rcLN, rcODD,
          rcORD, rcPRED, rcROUND, rcSIN, rcSQR, rcSQRT, rcSUCC, rcTRUNC,
          rcNone
        );
    private
      FValue: Values;
      function getEnum: Values;
      procedure setEnum(AValue: Values);
    public
      property Value: Values read getEnum write setEnum;
      function toString: string; override;
      class function toTyp(from: IRoutineCode): Values; static;
      constructor Create(AVAlue: Values);
  end;

Operator := (T1: TRoutineCodeImpl.Values): TRoutineCodeImpl;
Operator := (T1: TRoutineCodeImpl.Values): IRoutineCode;
Operator := (T1: IRoutineCode): TRoutineCodeImpl.Values;
Operator = (T1: IRoutineCode; T2: TRoutineCodeImpl.Values): boolean;


implementation

{ TSymtabStackImpl }

function TSymtabStackImpl.getObject: TObject;
begin
  Result := Self;
end;

function TSymtabStackImpl.toString: string;
begin
  WriteStr(Result, CurrentNestingLevel);
end;

constructor TSymtabStackImpl.Create;
begin
  inherited Create;
  CurrentNestingLevel := 0;
  Add(TSymTabFactory.CreateSymTab(CurrentNestingLevel));
end;

procedure TSymtabStackImpl.setProgramID(ID: ISymTabEntry);
begin
  ProgramID := ID;
end;

function TSymtabStackImpl.getProgramID: ISymTabEntry;
begin
  Result := ProgramID;
end;

//return the current nesting level.
function TSymtabStackImpl.getCurrentNestingLevel: integer;
begin
  Result := CurrentNestingLevel;
end;

//Return the local symbol table which is at the top of the stack.
function TSymtabStackImpl.getLocalSymtab: ISymtab;
begin
  Result := Items[CurrentNestingLevel];
end;

function TSymtabStackImpl.Push: ISymTab;
var
  SymTab: ISymTab;
begin
  CurrentNestingLevel += 1;
  SymTab := TSymTabFactory.CreateSymTab(CurrentNestingLevel);
  Add(SymTab);
  Result := SymTab;
end;

function TSymtabStackImpl.Push(SymTab: ISymTab): ISymTab;
begin
  CurrentNestingLevel += 1;
  Add(SymTab);
  Result := SymTab;
end;

function TSymtabStackImpl.Pop: ISymTab;
var
  SymTab: ISymTab;
begin
  SymTab := Items[CurrentNestingLevel];
  Delete(CurrentNestingLevel);
  Dec(CurrentNestingLevel);
  Result := SymTab;
end;

//Create and enter a new entry into the local symbol table.
function TSymtabStackImpl.EnterLocal(Name: string): ISymtabEntry;
begin
  Result := Items[CurrentNestingLevel].Enter(Name);
end;

//Look up an existing symbol table entry in the local symbol table.
function TSymtabStackImpl.LookupLocal(Name: string): ISymtabEntry;
begin
  Result := Items[CurrentNestingLevel].Lookup(Name);
end;

//Look up an existing symbol table entry throughout the stack.
function TSymtabStackImpl.Lookup(Name: string): ISymtabEntry;
var
  foundEntry: ISymtabEntry = Nil;
  i: integer;
begin
  //Search the current and enclosing scopes
  i := CurrentNestingLevel;
  while (i >= 0) and (foundEntry = Nil) do begin
    foundEntry := Items[i].Lookup(Name);
    Dec(i);
  end;
  Result := foundEntry;
end;



{ TSymtabKeyImpl }

function TSymtabKeyImpl.getEnum: Values;
begin
  Result := FValue;
end;

procedure TSymtabKeyImpl.setEnum(AValue: Values);
begin
  FValue := AValue;
end;

function TSymtabKeyImpl.toString: string;
begin
  WriteStr(Result, FValue);
end;

class function TSymtabKeyImpl.toTyp(from: ISymtabKey): Values;
begin
  ReadStr(from.toString, Result)
end;

constructor TSymtabKeyImpl.Create(AVAlue: Values);
begin
  FValue := AValue;
end;

operator:=(T1: TSymtabKeyImpl.Values): TSymtabKeyImpl;
begin
  Result := TSymtabKeyImpl.Create(T1);
end;

operator:=(T1: TSymtabKeyImpl.Values): ISymtabKey;
begin
  Result := TSymtabKeyImpl.Create(T1);
end;

operator:=(T1: ISymtabKey): TSymtabKeyImpl.Values;
var
  Arg: TSymtabKeyImpl.Values;
begin
  ReadStr(T1.toString, Arg);
  Result := TSymtabKeyImpl.Create(Arg);
end;

operator=(T1: ISymtabKey; T2: TSymtabKeyImpl.Values): boolean;
var
  Arg: TSymtabKeyImpl.Values;
begin
  ReadStr(T1.toString, Arg);
  Result := Arg = T2;
end;


{ TSymTabImpl }

function TSymTabImpl.toString: string;
begin
  WriteStr(Result, FNestingLevel);
end;

function TSymTabImpl.getObject: TObject;
begin
  Result := Self;
end;

constructor TSymTabImpl.Create(ANestingLevel: integer);
begin
  Inherited Create;
  FNestingLevel := ANestingLevel;
end;

//Create and enter a new entry into the symbol table.
function TSymTabImpl.Enter(Name: string): ISymtabEntry;
var
  Entry: ISymTabEntry;
begin
  Entry := TSymtabFactory.CreateSymtabEntry(Name, Self);
  KeyData[Name] := Entry;

  Result := Entry;
end;

//Look up an existing symbol table entry.
function TSymTabImpl.Lookup(Name: string): ISymtabEntry;
var
  index: LongInt;
begin
  index := IndexOf(Name);
  if index < 0 then
    Result := Nil
  else
    Result := KeyData[Name];
end;

//return a list of symbol table entries sorted by name.
function TSymTabImpl.SortedEntries: TSymTabEntries;
var
  i: Integer;
  Entries: TSymTabEntryMap;
begin
  Entries := TSymTabEntryMap.Create;
  Entries.Sorted := True;
  for i := 0 to Self.Count - 1 do
    Entries.Add(Self.Keys[i], Self.Data[i]);
  Entries.Sort;

  Result := TSymtabEntries.Create;

  // Iterate over the sorted entries and append them to the list.
  for i := 0 to Entries.Count-1 do
    Result.Add(Entries.Data[i]);

  Entries.Free;
end;


//return the scope nesting level of this entry.
function TSymTabImpl.getNestingLevel: integer;
begin
  Result := FNestingLevel;
end;

{ TSymtabEntryImpl }

function TSymTabEntryImpl.toString: string;
begin
  Result := FName;
end;

constructor TSymTabEntryImpl.Create(AName: string; ASymTab: ISymtab);
begin
  Inherited Create;
  FName := AName;
  FSymtab := ASymtab;
  FLineNumbers := TLineNumbers.Create;
end;

destructor TSymTabEntryImpl.Destroy;
begin
  FLineNumbers.Free;
  Inherited;
end;

function TSymTabEntryImpl.getObject: TObject;
begin
  Result := Self;
end;

//return the name of the entry.
function TSymTabEntryImpl.getName: string;
begin
  Result := FName;
end;

//return the symbol table that contains this entry.
function TSymTabEntryImpl.getSymtab: ISymtab;
begin
  Result := FSymTab;
end;

procedure TSymTabEntryImpl.setDefinition(ADefinition: IDefinition);
begin
  FDefinition := ADefinition;
end;

function TSymTabEntryImpl.getDefinition: IDefinition;
begin
  Result := FDefinition;
end;

procedure TSymTabEntryImpl.setTypeSpec(ATypeSpec: ITypeSpec);
begin
  FTypeSpec := ATypeSpec;
end;

function TSymTabEntryImpl.getTypeSpec: ITypeSpec;
begin
  Result := FTypeSpec;
end;

//Append a source line number to the entry.
procedure TSymTabEntryImpl.AppendLineNumber(LineNumber: integer);
begin
  FLineNumbers.Add(LineNumber);
end;

//return the list of source line numbers for the entry.
function TSymTabEntryImpl.getLineNumbers: TLineNumbers;
begin
  Result := FLineNumbers;
end;

//Set an attribute of the entry.
procedure TSymTabEntryImpl.setAttribute(Key: ISymtabKey; Value: TObject);
begin
  KeyData[TSymTabKeyImpl.toTyp(Key)] := Value;
end;

//Get the value of an attribute of the entry.
function TSymTabEntryImpl.getAttribute(Key: ISymtabKey): TObject;
var
  index: longint;
begin
  index := indexOf(TSymTabKeyImpl.toTyp(Key));
  if index < 0 then
    Result := Nil
  else
    Result := KeyData[TSymTabKeyImpl.toTyp(Key)];
end;

{ TRoutineCodeImpl }

function TRoutineCodeImpl.getEnum: Values;
begin
  Result := FValue;
end;

procedure TRoutineCodeImpl.setEnum(AValue: Values);
begin
  FValue := AValue;
end;

function TRoutineCodeImpl.toString: string;
begin
  WriteStr(Result, FValue);
end;

class function TRoutineCodeImpl.toTyp(from: IRoutineCode): Values;
begin
  ReadStr(from.toString, Result)
end;

constructor TRoutineCodeImpl.Create(AVAlue: Values);
begin
  FValue := AValue;
end;



Operator := (T1: TRoutineCodeImpl.Values): TRoutineCodeImpl;
begin
  Result := TRoutineCodeImpl.Create(T1);
end;

Operator := (T1: TRoutineCodeImpl.Values): IRoutineCode;
begin
  Result := TRoutineCodeImpl.Create(T1);
end;

Operator := (T1: IRoutineCode): TRoutineCodeImpl.Values;
var
  Arg: TRoutineCodeImpl.Values;
begin
  ReadStr(T1.toString, Arg);
  Result := TRoutineCodeImpl.Create(Arg);
end;

Operator = (T1: IRoutineCode; T2: TRoutineCodeImpl.Values): boolean;
var
  Arg: TRoutineCodeImpl.Values;
begin
  ReadStr(T1.toString, Arg);
  Result := Arg = T2;
end;



end.

