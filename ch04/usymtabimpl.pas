unit uSymtabImpl;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, usymtab, uSymtabFactory, fgl, AnyObject;

type

  { TSymtabStackImpl }

  //An implementation of the symbol table stack.
  TSymtabStackImpl = class(specialize TFPGList<ISymtab>, ISymtabStack)
    private
      CurrentNestingLevel: integer;  // current scope nesting level
    public
      function getObject: TObject;
      function toString: string; override;
      constructor Create;
      function getCurrentNestingLevel: integer;
      function getLocalSymtab: ISymtab;
      function EnterLocal(Name: string): ISymtabEntry;
      function LookupLocal(Name: string): ISymtabEntry;
      function Lookup(Name: string): ISymtabEntry;
  end;


  TSymTabEntyMap = specialize TFPGMap<String, ISymtabEntry>;

  { TSymTabImpl }

  //An implementation of the symbol table.
  TSymTabImpl = class(TSymTabEntyMap, ISymtab)
    private
      NestingLevel: integer;
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
     Type
       TSymtabKey = (
         skConstantValue,            //constant
         skRoutineCode, skRoutineSymtab, skRoutineICode, skRoutineParms,
         skRoutineRoutines,          //procedure or function
         skDataValue                 //variable or record field value
       );
   private
     FValue: TSymtabKey;
   public
     function toString: string; override;
     constructor Create(AValue: TSymtabKey);
     property Value: TSymtabKey read FValue;
  end;

Operator := (T1: TSymtabKeyImpl.TSymtabKey): TSymtabKeyImpl;
Operator := (T1: TSymtabKeyImpl.TSymtabKey): ISymtabKey;
Operator := (T1: ISymtabKey): TSymtabKeyImpl.TSymtabKey;
Operator = (T1: ISymtabKey; T2: TSymtabKeyImpl.TSymtabKey): boolean;


type

  TAttributeMap = specialize TFPGMap<TSymtabKeyImpl.TSymtabKey, TObject>;

  //An implementation of a symbol table entry.

  { TSymTabEntryImpl }

  TSymTabEntryImpl = class(TAttributeMap, ISymtabEntry)
    private
      Name: string;         // entry name
      Symtab: ISymtab;      // parent symbol table
      LineNumbers: TLineNumbers;  // source line numbers
    public
      function toString: string; override;
      function getObject: TObject;
      constructor Create(AName: string; ASymtab: ISymtab);
      destructor Destroy; override;
      function getName: string;
      function getSymtab: ISymtab;
      procedure AppendLineNumber(LineNumber: integer); //append source line num
      function getLineNumbers: TLineNumbers;
      procedure setAttribute(Key: ISymtabKey; Value: TObject); //set attr of entry
      function getAttrubute(Key: ISymtabKey): TObject; //get the attr's value of the entry
  end;


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

//return the current nesting level.
function TSymtabStackImpl.getCurrentNestingLevel: integer;
begin
  Result := CurrentNestingLevel;
end;

//Return the local symbol table which is at the top of the stack.
function TSymtabStackImpl.getLocalSymtab: ISymtab;
begin
  Result := get(CurrentNestingLevel);
end;

//Create and enter a new entry into the local symbol table.
function TSymtabStackImpl.EnterLocal(Name: string): ISymtabEntry;
begin
  Result := get(CurrentNestingLevel).Enter(Name);
end;

//Look up an existing symbol table entry in the local symbol table.
function TSymtabStackImpl.LookupLocal(Name: string): ISymtabEntry;
begin
  Result := get(CurrentNestingLevel).Lookup(Name);
end;

//Look up an existing symbol table entry throughout the stack.
function TSymtabStackImpl.Lookup(Name: string): ISymtabEntry;
begin
  Result := LookupLocal(Name);
end;



{ TSymtabKeyImpl }

function TSymtabKeyImpl.toString: string;
begin
  WriteStr(Result, FValue);
end;

constructor TSymtabKeyImpl.Create(AValue: TSymtabKey);
begin
  FValue := AValue;
end;

operator:=(T1: TSymtabKeyImpl.TSymtabKey): TSymtabKeyImpl;
begin
  Result := TSymtabKeyImpl.Create(T1);
end;

operator:=(T1: TSymtabKeyImpl.TSymtabKey): ISymtabKey;
begin
  Result := TSymtabKeyImpl.Create(T1);
end;

operator:=(T1: ISymtabKey): TSymtabKeyImpl.TSymtabKey;
var
  Arg: TSymtabKeyImpl.TSymtabKey;
begin
  ReadStr(T1.toString, Arg);
  Result := TSymtabKeyImpl.Create(Arg);
end;

operator=(T1: ISymtabKey; T2: TSymtabKeyImpl.TSymtabKey): boolean;
var
  Arg: TSymtabKeyImpl.TSymtabKey;
begin
  ReadStr(T1.toString, Arg);
  Result := Arg = T2;
end;


{ TSymTabImpl }

function TSymTabImpl.toString: string;
begin
  WriteStr(Result, NestingLevel);
end;

function TSymTabImpl.getObject: TObject;
begin
  Result := Self;
end;

constructor TSymTabImpl.Create(ANestingLevel: integer);
begin
  Inherited Create;
  NestingLevel := ANestingLevel;
end;

//Create and enter a new entry into the symbol table.
function TSymTabImpl.Enter(Name: string): ISymtabEntry;
begin
  Result := TSymtabFactory.CreateSymtabEntry(Name, Self);
  Add(Name, Result);
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
    Result := GetData(index);
end;

//return a list of symbol table entries sorted by name.
function TSymTabImpl.SortedEntries: TSymTabEntries;
var
  i: Integer;
  Entries: TSymTabEntyMap;
begin
  Entries := TSymTabEntyMap.Create;
  Entries.Sorted := True;
  Entries := Self;
  Result := TSymtabEntries.Create;
  // Iterate over the sorted entries and append them to the list.
  for i := 0 to Entries.Count-1 do
    Result.Add(Data[i]);
  Entries.Free;
end;

//return the scope nesting level of this entry.
function TSymTabImpl.getNestingLevel: integer;
begin
  Result := NestingLevel;
end;

{ TSymtabEntryImpl }

function TSymtabEntryImpl.toString: string;
begin
  Result := Name;
end;

constructor TSymtabEntryImpl.Create(AName: string; ASymtab: ISymtab);
begin
  Inherited Create;
  Name := AName;
  Symtab := ASymtab;
  LineNumbers := TLineNumbers.Create;
end;

destructor TSymtabEntryImpl.Destroy;
begin
  if assigned(LineNumbers) then LineNumbers.Destroy;
  Inherited Destroy;
end;

function TSymtabEntryImpl.getObject: TObject;
begin
  Result := Self;
end;

//return the name of the entry.
function TSymtabEntryImpl.getName: string;
begin
  Result := Name;
end;

//return the symbol table that contains this entry.
function TSymtabEntryImpl.getSymtab: ISymtab;
begin
  Result := Symtab;
end;

//Append a source line number to the entry.
procedure TSymtabEntryImpl.AppendLineNumber(LineNumber: integer);
begin
  LineNumbers.Add(LineNumber);
end;

//return the list of source line numbers for the entry.
function TSymtabEntryImpl.getLineNumbers: TLineNumbers;
begin
  Result := LineNumbers;
end;

//Set an attribute of the entry.
procedure TSymtabEntryImpl.setAttribute(Key: ISymtabKey; Value: TObject);
begin
  Add(Key, Value);
end;

//Get the value of an attribute of the entry.
function TSymtabEntryImpl.getAttrubute(Key: ISymtabKey): TObject;
var
  index: longint;
begin
  index := indexOf(Key);
  if index < 0 then
    Result := Nil
  else
    Result := KeyData[Key];
end;

end.

