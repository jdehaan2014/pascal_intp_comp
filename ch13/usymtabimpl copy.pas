unit uSymtabImpl;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, usymtab, uSymtabFactory, fgl, uObjectUtils, AnyObject;

type

  { TSymtabStackDelegate }

  //An implementation of the symbol table stack.
  TSymtabStackDelegate = class(specialize TFPGList<ISymtab>, ISymtabStack)
    private
      CurrentNestingLevel: integer;  // current scope nesting level
    public
      function getObject: TObject;
      function toString: string;
      constructor Create;
      function getCurrentNestingLevel: integer;
      function getLocalSymtab: ISymtab;
      function EnterLocal(Name: string): ISymtabEntry;
      function LookupLocal(Name: string): ISymtabEntry;
      function Lookup(Name: string): ISymtabEntry;
  end;

  { TSymtabStackImpl }

  TSymtabStackImpl = class(TAnyObject, ISymtabStack)
    private
      FSymtabStack: TSymtabStackDelegate;
      function toString: string;
    public
      property SymtabStack: TSymtabStackDelegate
               read FSymtabStack
               implements ISymtabStack;
      constructor Create;
      destructor Destroy;
  end;


  { TSymtabDelegate }

  //An implementation of the symbol table.
  TSymtabDelegate = class(specialize TFPGMap<String, ISymtabEntry>, ISymtab)
    public
      type
        TSymTabEntries = specialize TFPGList<ISymtabEntry>;
    private
      FNestingLevel: integer;
      function toString: string;
    public
      function getObject: TObject;
      constructor Create(NestingLevel: integer);
      function Enter(Name: string): ISymtabEntry;
      function Lookup(Name: string): ISymtabEntry;
      function SortedEntries: TSymTabEntries;
      function getNestingLevel: integer;
  end;

  { TSymTabImpl }

  TSymTabImpl = class(TAnyObject, ISymtab)
    private
      FSymTab: TSymTabDelegate;
      function toString: string;
    public
      property SymTab: TSymTabDelegate
               read FSymTab
               implements ISymTab;
      constructor Create;
      destructor Destroy;
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
     function toString: string;
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

  { TSymtabEntryDelegate }

  TSymtabEntryDelegate = class(TAttributeMap, ISymtabEntry)
    private
      Name: string;         // entry name
      Symtab: ISymtab;      // parent symbol table
      LineNumbers: TLineNumbers;  // source line numbers
    public
      function toString: string;
      constructor Create(AName: string; ASymtab: ISymtab);
      destructor Destroy;
      function getObject: TObject;
      function getName: string;
      function getSymtab: ISymtab;
      procedure AppendLineNumber(LineNumber: integer); //append source line num
      function getLineNumbers: TLineNumbers;
      procedure setAttribute(Key: ISymtabKey; Value: TObject); //set attr of entry
      function getAttrubute(Key: ISymtabKey): TObject; //get the attr's value of the entry
  end;

  { TSymTabEntryImpl }

  TSymTabEntryImpl = class(TAnyObject, ISymtabEntry)
    private
      FSymTabEntry: TSymtabEntryDelegate;
      function toString: string;
    public
      property SymTabEntry: TSymtabEntryDelegate
               read FSymTabEntry
               implements ISymtabEntry;
      constructor Create(AName: string; ASymtab: ISymtab);
      destructor Destroy;
  end;

implementation

{ TSymtabStack }

function TSymtabStackDelegate.getObject: TObject;
begin
  Result := Self;
end;

function TSymtabStackDelegate.toString: string;
begin
  WriteStr(Result, CurrentNestingLevel);
end;

constructor TSymtabStackDelegate.Create;
begin
  CurrentNestingLevel := 0;
  Add(TSymTabFactory.CreateSymTab(CurrentNestingLevel));
end;

//return the current nesting level.
function TSymtabStackDelegate.getCurrentNestingLevel: integer;
begin
  Result := CurrentNestingLevel;
end;

//Return the local symbol table which is at the top of the stack.
function TSymtabStackDelegate.getLocalSymtab: ISymtab;
begin
  Result := Get(IndexOf(CurrentNestingLevel));
end;

//Create and enter a new entry into the local symbol table.
function TSymtabStackDelegate.EnterLocal(Name: string): ISymtabEntry;
begin
  Result := (Get(CurrentNestingLevel)).Enter(Name);
end;

//Look up an existing symbol table entry in the local symbol table.
function TSymtabStackDelegate.LookupLocal(Name: string): ISymtabEntry;
begin
  Result := Get(IndexOf(CurrentNestingLevel)).Lookup(Name);
end;

//Look up an existing symbol table entry throughout the stack.
function TSymtabStackDelegate.Lookup(Name: string): ISymtabEntry;
begin
  Result := LookupLocal(Name);
end;


{ TSymtabStackImpl }

function TSymtabStackImpl.toString: string;
begin
  Result := FSymtabStack.toString;
end;

constructor TSymtabStackImpl.Create;
begin
  FSymtabStack := TSymtabStackDelegate.Create;
end;

destructor TSymtabStackImpl.Destroy;
begin
  FSymtabStack.Free;
  Inherited;
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


{ TSymtabDelegate }

function TSymtabDelegate.toString: string;
begin
  // nothing
  Result := '';
end;

function TSymtabDelegate.getObject: TObject;
begin

end;

constructor TSymtabDelegate.Create(NestingLevel: integer);
begin
  FNestingLevel := NestingLevel;
end;


//Create and enter a new entry into the symbol table.
function TSymtabDelegate.Enter(Name: string): ISymtabEntry;
begin
  Result := TSymtabFactory.CreateSymtabEntry(Name, Self);
  Add(Name, Result);
end;

//Look up an existing symbol table entry.
function TSymtabDelegate.Lookup(Name: string): ISymtabEntry;
begin
  Result := get(IndexOf(Name));
end;

//return a list of symbol table entries sorted by name.
function TSymtabDelegate.SortedEntries: TSymtabEntries;
var
  i: Integer;
begin
  Result := TSymtabEntries.Create;
  // Iterate over the sorted entries and append them to the list.
  for i := 0 to FEntries.Count-1 do
    Result.Add(FEntries.Data[i]);
end;

//return the scope nesting level of this entry.
function TSymtabDelegate.getNestingLevel: integer;
begin
  Result := FNestingLevel;
end;

{ TSymtabEntryImpl }

function TSymtabEntryDelegate.toString: string;
begin
  Result := Name;
end;

constructor TSymtabEntryDelegate.Create(AName: string; ASymtab: ISymtab);
begin
  Name := AName;
  Symtab := ASymtab;
  LineNumbers := TLineNumbers.Create;
end;

destructor TSymtabEntryDelegate.Destroy;
begin
  LineNumbers.Destroy;
  Inherited Destroy;
end;

function TSymtabEntryDelegate.getObject: TObject;
begin
  Result := Self;
end;

//return the name of the entry.
function TSymtabEntryDelegate.getName: string;
begin
  Result := Name;
end;

//return the symbol table that contains this entry.
function TSymtabEntryDelegate.getSymtab: ISymtab;
begin
  Result := Symtab;
end;

//Append a source line number to the entry.
procedure TSymtabEntryDelegate.AppendLineNumber(LineNumber: integer);
begin
  LineNumbers.Add(LineNumber);
end;

//return the list of source line numbers for the entry.
function TSymtabEntryDelegate.getLineNumbers: TLineNumbers;
begin
  Result := LineNumbers;
end;

//Set an attribute of the entry.
procedure TSymtabEntryDelegate.setAttribute(Key: ISymtabKey; Value: TObject);
begin
  Add(Key, Value);
end;

//Get the value of an attribute of the entry.
function TSymtabEntryDelegate.getAttrubute(Key: ISymtabKey): TObject;
begin
  Result := KeyData[Key];
end;

{ TSymTabEntryImpl }

function TSymTabEntryImpl.toString: string;
begin
  // nothing
  Result := '';
end;

constructor TSymTabEntryImpl.Create(AName: string; ASymtab: ISymtab);
begin
  FSymTabEntry := TSymtabEntryDelegate.Create(AName, ASymtab);
end;

destructor TSymTabEntryImpl.Destroy;
begin
  FSymTabEntry.Destroy;
  Inherited Destroy;
end;



end.

