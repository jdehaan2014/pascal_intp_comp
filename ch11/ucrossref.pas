unit uCrossRef;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uGenericInterfaces, uSymtabImpl, uTypesImpl, fgl,
  uDefinitionImpl, uObjectUtils;

//Generate a cross-reference listing.
type

  { TCrossReferencer }

  TCrossReferencer = class
    private
      type
        TRecordTypes = specialize TFPGList<ITypeSpec>;
      const
        NAME_WIDTH = 16;
        NAME_FORMAT       = '%-16s';
        NUMBERS_LABEL     = ' Line numbers    ';
        NUMBERS_UNDERLINE = ' ------------    ';
        NUMBER_FORMAT = ' %.3d';
        LABEL_WIDTH  = length(NUMBERS_LABEL);
        INDENT_WIDTH = NAME_WIDTH + LABEL_WIDTH;
        ENUM_CONSTANT_FORMAT = '%16s = %s';
      class var
        INDENT: string[INDENT_WIDTH];
      procedure PrintRoutine(RoutineID: ISymTabEntry);
      procedure PrintColumnHeadings;
      procedure PrintSymtab(Symtab: ISymtab; recordTypes: TRecordTypes);
      procedure PrintEntry(Entry: ISymTabEntry; recordTypes: TRecordTypes);
      procedure PrintType(Typ: ITypeSpec);
      procedure PrintTypeDetail(Typ: ITypeSpec; recordTypes: TRecordTypes);
      procedure PrintRecords(recordTypes: TRecordTypes);
      function toString(Value: TObject): string;
    public
      class constructor Create;
      procedure Print(SymtabStack: ISymtabStack);
  end;

implementation

{ TCrossReferencer }

class constructor TCrossReferencer.Create;
begin
  Fillchar(INDENT, SizeOf(INDENT), ' ');
end;

//Print the cross-reference table.
procedure TCrossReferencer.Print(SymTabStack: ISymTabStack);
var
  ProgramID: ISymTabEntry;
begin
  Writeln(LineEnding + '===== CROSS-REFERENCE TABLE =====');

  ProgramID := SymTabStack.getProgramID;
  PrintRoutine(ProgramID);
end;

//Print a coss-reference table for a routine
procedure TCrossReferencer.PrintRoutine(RoutineID: ISymTabEntry);
var
  Definition: IDefinition;
  SymTab: ISymTab;
  newRecordTypes: TRecordTypes;
  RoutineIDs:  TSymTabEntries;
  rtnID: ISymTabEntry;
begin
  Definition := RoutineID.getDefinition;
  WriteLn(LineEnding,
      '*** ', Definition.toString.Substring(3), ' ', RoutineID.getName, ' ***');

  PrintColumnHeadings;

  //Print the entries in the routine's symbol table
  SymTab := (RoutineID.getAttribute(skRoutineSymTab) as ISymTab);
  newRecordTypes := TRecordTypes.Create;
  PrintSymTab(SymTab, newRecordTypes);

  //Print cross-reference tables for any records defined in the routine.
  if newRecordTypes.Count > 0 then
    PrintRecords(newRecordTypes);

  //Print any procedures and functions defined in the routine.
  RoutineIDs := RoutineID.getAttribute(skRoutineRoutines) as TSymTabEntries;
  if RoutineIDs <> Nil then
    for rtnID in RoutineIDs do
      PrintRoutine(rtnID);
end;

procedure TCrossReferencer.PrintColumnHeadings;
begin
  Writeln;
  writeln(Format(NAME_FORMAT, ['Identifier']) + NUMBERS_LABEL +     'Type specification');
  writeln(Format(NAME_FORMAT, ['----------']) + NUMBERS_UNDERLINE + '------------------');
end;

//Print the entries in a symbol table.
procedure TCrossReferencer.PrintSymtab(Symtab: ISymtab;
  recordTypes: TRecordTypes);
var
  SortedEntries: TSymtabEntries;
  Entry: ISymtabEntry;
  LineNumbers: TLineNumbers;
  LineNumber: integer;
begin
  // Loop over the sorted list of symbol table entries.
  SortedEntries := Symtab.SortedEntries;
  for Entry in SortedEntries do begin
    LineNumbers := Entry.getLineNumbers;
    // for each entry print the identifier name followed by the line numbers
    write(Format(NAME_FORMAT, [Entry.getName]));
    if LineNumbers <> Nil then begin
      for LineNumber in LineNumbers do
        write(Format(NUMBER_FORMAT, [LineNumber]));
    end;
    writeln;
    PrintEntry(Entry, recordTypes);
  end;
end;

//Print a symbol table entry.
procedure TCrossReferencer.PrintEntry(Entry: ISymTabEntry;
  recordTypes: TRecordTypes);
var
  Definition: IDefinition;
  NestingLevel: integer;
  Typ: ITypeSpec;
  Value: TObject;
begin
  Definition := Entry.getDefinition;
  NestingLevel := Entry.getSymTab.getNestingLevel;
  WriteLn(INDENT, 'Defined as: ', Definition.getText);
  WriteLn(INDENT, 'Scope nesting level: ', NestingLevel);
  //Print the type specification.
  Typ := Entry.getTypeSpec;
  PrintType(Typ);
  case TDefinitionImpl.toTyp(Definition) of
    defConstant:
      begin
        Value := Entry.getAttribute(skConstantValue);
        WriteLn(INDENT, 'Value = ', toString(Value));
        //Print the type details only if the type is unnamed.
        if Typ.getIdentifier = Nil then
          PrintTypeDetail(Typ, recordTypes);
      end;
    defEnumeration_Constant:
      begin
        Value := Entry.getAttribute(skConstantValue);
        WriteLn(INDENT, 'Value = ', toString(Value));
      end;
    defType:
      begin
        //Print the type details only when the type is first defined.
        if Entry = Typ.getIdentifier then
          PrintTypeDetail(Typ, recordTypes);
      end;
    defVariable:
      begin
        //Print the type details only if the type is unnamed.
        if Typ.getIdentifier = Nil then
          PrintTypeDetail(Typ, recordTypes);
      end;
  end;
end;

//Print a type specification.
procedure TCrossReferencer.PrintType(Typ: ITypeSpec);
var
  Form: ITypeForm;
  TypeID: ISymTabEntry;
  TypeName: string;
begin
  if Typ <> Nil then begin
    Form := Typ.getForm;
    TypeID := Typ.getIdentifier;
    if TypeID <> Nil then
      TypeName := TypeID.getName
    else
      TypeName := '<unnamed>';
    WriteLn(INDENT,
       'Type form = ', Form.toString.Substring(2), ', Type ID = ', TypeName);
  end;
end;

//Print the details of a type specification.
procedure TCrossReferencer.PrintTypeDetail(Typ: ITypeSpec;
  recordTypes: TRecordTypes);
var
  Form: ITypeForm;
  ConstantID: ISymTabEntry;
  ConstantIDs: TSymTabEntries;
  Name: string;
  Value, minValue, maxValue: TObject;
  baseTypeSpec, indexType, elementType: ITypeSpec;
  Count: integer = 0;
begin
  Form := Typ.getForm;
  case TTypeFormImpl.toTyp(Form) of
    tfEnumeration:
      begin
        ConstantIDs := Typ.getAttribute(tkEnumerationConstants) as TSymTabEntries;
        WriteLn(INDENT, '--- Enumeration constants ---');
        //Print each enumeration constant and its value.
        for ConstantID in ConstantIDs do begin
          Name := ConstantID.getName;
          Value := ConstantID.getAttribute(skConstantValue);
          WriteLn(INDENT, Format(ENUM_CONSTANT_FORMAT, [Name, Value.ToString]));
        end;
      end;
    tfSubRange:
      begin
        minValue := Typ.getAttribute(tkSubRangeMinValue);
        maxValue := Typ.getAttribute(tkSubRangeMaxValue);
        baseTypeSpec := TTypeSpecImpl(Typ.getAttribute(tkSubRangeBaseType));
        WriteLn(INDENT, '--- Base type ---');
        PrintType(baseTypeSpec);
        //Print the base type details only if the type is unnamed.
        if baseTypeSpec.getIdentifier = Nil then
          PrintTypeDetail(baseTypeSpec, recordTypes);
        Write(INDENT, 'Range = ');
        WriteLn(toString(minValue), '..', toString(maxValue));
      end;
    tfArray:
      begin
        indexType := TTypeSpecImpl(Typ.getAttribute(tkArrayIndexType));
        elementType := TTypeSpecImpl(Typ.getAttribute(tkArrayElementType));
        Count := TInteger(Typ.getAttribute(tkArrayElementCount)).Value;
        WriteLn(INDENT, '--- INDEX TYPE ---');
        PrintType(indexType);
        //Print the index type details only if the type is unnamed.
        if indexType.getIdentifier  = Nil then
          PrintTypeDetail(indexType, recordTypes);
        WriteLn(INDENT, '--- ELEMENT TYPE ---');
        PrintType(elementType);
        WriteLn(INDENT, Count, ' elements');
        //Print the element type details only if the type is unnamed.
        if elementType.getIdentifier = Nil then
          PrintTypeDetail(elementType, recordTypes);
      end;
    tfRecord: recordTypes.Add(Typ);
  end;
end;

//Print cross-reference tables for records defined in the routine.
procedure TCrossReferencer.PrintRecords(recordTypes: TRecordTypes);
var
  recordType: ITypeSpec;
  recordID: ISymTabEntry;
  Name: string;
  SymTab: ISymTab;
  newRecordTypes: TRecordTypes;
begin
  for recordType in recordTypes do begin
    recordID := recordType.getIdentifier;
    if recordID <> Nil then
      Name := recordID.getName
    else
      Name := '<unnamed>';

    WriteLn(LineEnding, '--- RECORD ', Name, ' ---');
    PrintColumnHeadings;

    //Print the entries in the record's symbol table.
    SymTab := recordType.getAttribute(tkRecordSymTab) as ISymTab;
    newRecordTypes := TRecordTypes.Create;
    PrintSymTab(SymTab, newRecordTypes);

    //Print cross-reference tables for any nested records.
    if newRecordTypes.Count > 0 then
      PrintRecords(newRecordTypes);
  end;
end;

function TCrossReferencer.toString(Value: TObject): string;
begin
  if Value is TString then
    Result := '''' + TString(Value) + ''''
  else
    Result := Value.ToString;
end;

end.

