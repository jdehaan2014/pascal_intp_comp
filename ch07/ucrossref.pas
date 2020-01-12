unit uCrossRef;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uSymtab;

//Generate a cross-reference listing.
type

  { TCrossReferencer }

  TCrossReferencer = class
    private
      const
        NAME_WIDTH = 16;
        NAME_FORMAT       = '%-16s';
        NUMBERS_LABEL     = ' Line numbers    ';
        NUMBERS_UNDERLINE = ' ------------    ';
        NUMBER_FORMAT = ' %.3d';
        LABEL_WIDTH  = length(NUMBERS_LABEL);
        INDENT_WIDTH = NAME_WIDTH + LABEL_WIDTH;
      class var
        INDENT: string[INDENT_WIDTH];
    public
      class constructor Create;
      procedure Print(SymtabStack: ISymtabStack);
      procedure PrintColumnHeadings;
      procedure PrintSymtab(Symtab: ISymtab);
  end;

implementation

{ TCrossReferencer }

class constructor TCrossReferencer.Create;
begin
  Fillchar(INDENT, SizeOf(INDENT), ' ');
end;

//Print the cross-reference table.
procedure TCrossReferencer.Print(SymtabStack: ISymtabStack);
begin
  Writeln(LineEnding + '===== CROSS-REFERENCE TABLE =====');
  PrintColumnHeadings;
  PrintSymtab(SymtabStack.getLocalSymtab);
end;

procedure TCrossReferencer.PrintColumnHeadings;
begin
  Writeln;
  writeln(Format(NAME_FORMAT, ['Identifier']) + NUMBERS_LABEL);
  writeln(Format(NAME_FORMAT, ['----------']) + NUMBERS_UNDERLINE);
end;

//Print the entries in a symbol table.
procedure TCrossReferencer.PrintSymtab(Symtab: ISymtab);
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
      writeln;
    end;
  end;
end;

end.

