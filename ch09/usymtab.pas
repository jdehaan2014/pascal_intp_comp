unit usymtab;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fgl, AnyObject, uDefinition, uTypes;

type

  ISymTabKey = interface(IAnyInterface)
    ['ISymTabKey']
    //the interface for an attribute key of a symbol table entry
  end;

  ISymTab = interface;

  TLineNumbers = specialize TFPGList<integer>;

  //the interface for a symbol table entry.

  { ISymTabEntry }

  ISymTabEntry = interface(IAnyInterface)
    ['ISymTabEntry']
    function getName: string; //return name of entry
    function getSymTab: ISymTab; //return symtab that contains this entry
    procedure setDefinition(Definition: IDefinition);
    function getDefinition: IDefinition;
    procedure setTypeSpec(TypeSpec: ITypeSpec);
    function getTypeSpec: ITypeSpec;
    procedure appendLineNumber(LineNumber: integer); //append source line num
    function getLineNumbers: TLineNumbers; // ArrayList<integer>
    procedure setAttribute(Key: ISymTabKey; Value: TObject); //set attr of entry
    function getAttribute(Key: ISymTabKey): TObject; //get the attr's value of the entry
  end;

  TSymTabEntries = specialize TFPGList<ISymTabEntry>;

  //the framework interface that represents the symbol table
  ISymTab = interface(IAnyInterface)
    ['ISymTab']
    function getNestingLevel: integer;  // return scope nesting level of this entry
    function Enter(Name: string): ISymTabEntry; //create + enter new entry in symtab
    function Lookup(Name: string): ISymTabEntry; //lookup existing entry
    function SortedEntries: TSymTabEntries;  // return list (ArrayList<SymtabEntry>)
  end;


  ISymTabStack = interface(IAnyInterface)
    ['ISymTabStack']
    procedure setProgramID(Entry: ISymTabEntry);
    function getProgramID: ISymTabEntry;
    function getCurrentNestingLevel: integer;
    function getLocalSymtab: ISymTab;             //return local symtab on top of stack
    function Push: ISymTab;                       //Push a new symtable onto the stack
    function Push(SymTab: ISymTab): ISymTab;      //Push a symtable onto the stack
    function Pop: ISymTab;                        //pop symtable off the stack
    function EnterLocal(Name: string): ISymTabEntry;  //Create new entry in local table
    function LookupLocal(Name: string): ISymTabEntry; //lookup existing symtab in loca table
    function Lookup(Name: string): ISymTabEntry; //lookup existing symtab througout the stack
  end;

implementation

end.

