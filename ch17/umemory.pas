unit uMemory;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, AnyObject, fgl, uGenericInterfaces;

type

  //Interface for the interpreter's runtime memory cell.
  ICell = interface(IAnyInterface)
    ['ICell']
    procedure setValue(newValue: TObject);  //Set a new value into the cell.
    function getValue: TObject;  //return the value in the cell.
  end;

  TCellList = specialize TFPGList<ICell>;

  TNameList = specialize TFPGList<string>;

  //Interface for the interpreter's runtime activation record.
  IActivationRecord = interface(IAnyInterface)
    ['IActivationRecord']
    function getRoutineId: ISymTabEntry;  //symbol table entry of the routine's name
    function getCell(Name: String): ICell; //memory cell for the given name from the memory map
    function getAllNames: TNameList;  //list of all names in the memory map
    function getNestingLevel: integer;  //the scope nesting level
    function LinkedTo: IActivationRecord; //activation record to which this record is dynamically linked
    function MakeLinkTo(ar: IActivationRecord): IActivationRecord; //make dynamic link
  end;

  //interface for the interpreter' runtime memory map.
  IMemoryMap = interface(IAnyInterface)
    ['IMemoryMap']
    function getCell(Name: string): ICell;
    function getAllNames: TNameList;
  end;

  //interface for the interpreter' runtime display.
  IRuntimeDisplay = interface(IAnyInterface)
    ['IRuntimeDisplay']
    //Get the activation record at a given nesting level.
    function getActivationRecord(NestingLevel: integer): IActivationRecord;
    //Update the display for a call to a routine at a given nesting level.
    procedure CallUpdate(NestingLevel: integer; ar: IActivationRecord);
    //Update the display for a return from a routine at a given nesting level.
    procedure ReturnUpdate(NestingLevel: integer);
  end;

  TActivationRecords = specialize TFPGList<IActivationRecord>;

  //Interface for the interpreter's runtime stack.
  IRuntimeStack = interface(IAnyInterface)
    ['IRuntimeStack']
    function Records: TActivationRecords; //return list of records on the stack
    //Get the topmost activation record at a given nesting level.
    function getTopMost(NestingLevel: integer): IActivationRecord;
    function CurrentNestingLevel: integer;  //the current nesting level
    procedure Pop; //Pop an activation record off the stack.
    procedure Push(ar: IActivationRecord); //Push an activation record onto the stack.
  end;

implementation

end.

