unit uParseTreePrinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCode, uCodeImpl, uGenericInterfaces, uObjectUtils,
  AnyObject, uSymTabImpl;

type

  { TParseTreePrinter }

  TParseTreePrinter = class
    private
      const
        IndentWidth = 4;
        LineWidth = 80;
      var
        PrintStream: TStringList;      // output print stream
        Len: integer;         // output line length
        Indent: string;       // indent spaces
        Indentation: string;  // indentation of a line
        Line: string;         // output line
      procedure PrintNode(Node: TICodeNodeImpl);
      procedure Append(AText: string);
      procedure PrintLine;
      procedure PrintAttributes(Node: TICodeNodeImpl);
      procedure PrintAttribute(KeyString: string; Value: TObject);
      procedure PrintChildNodes(ChildNodes: TICodeNodeList);
      procedure PrintTypeSpec(Node: TICodeNodeImpl);
      procedure PrintRoutine(RoutineID: ISymTabEntry);
    public
      constructor Create(APrintStream: TStringList);
      procedure Print(SymTabStack: ISymTabStack; var OutputList: TStringList);

  end;

implementation

{ TParseTreePrinter }

//Print a parse tree node.
procedure TParseTreePrinter.PrintNode(Node: TICodeNodeImpl);
var
  ChildNodes: TICodeNodeList;
begin
  //opening tag
  Append(Indentation);
  Append('<' + Copy(Node.toString,3,255));
  PrintAttributes(Node);
  PrintTypeSpec(Node);
  ChildNodes := Node.getChildren;
  // Print the node's children followed by the closing tag.
  if (ChildNodes <> Nil) and (ChildNodes.Count > 0) then begin
    Append('>');
    PrintLine;
    PrintChildNodes(ChildNodes);
    Append(Indentation); Append('</' + Copy(Node.toString,3,255) + '>');
  end
  else begin           // No children: Close off the tag.
    Append(' ');
    Append('/>');
  end;
  PrintLine;
end;

//Append text to the output line.
procedure TParseTreePrinter.Append(AText: string);
var
  TextLength: integer;
  LineBreak: boolean = False;
begin
  TextLength := Length(AText);
  //wrap lines that are too long
  if (Len + TextLength) > LineWidth then begin
    PrintLine;
    Line += Indentation;
    Len :=  Length(Indentation);
    LineBreak := True;
  end;
  //append the text
  if not (LineBreak and (AText = ' ')) then begin
    Line += AText;
    Len += TextLength;
  end;
end;

//Print an output line.
procedure TParseTreePrinter.PrintLine;
begin
  if Len > 0 then begin
    PrintStream.Add(Line);
    Line := '';
    Len := 0;
  end;
end;

//print a parse tree node's attributes
procedure TParseTreePrinter.PrintAttributes(Node: TICodeNodeImpl);
var
  SaveIndentation,
  keyAttrib: string;
  i: integer;
begin
  SaveIndentation := Indentation;
  Indentation += Indent;
  // Iterate over the node entries and print them.
  for i := 0 to Node.Count-1 do begin
    // remove 'ct' from the code node string
    keyAttrib := Copy(TICodeKeyImpl(Node.Keys[i]).toString, 3, 255);
    PrintAttribute(keyAttrib, Node.Data[i]);
  end;
  Indentation := SaveIndentation;
end;

//Print a node attribute as key="value".
procedure TParseTreePrinter.PrintAttribute(KeyString: string; Value: TObject);
var
  isSymTabEntry: Boolean;
  ValueStr, aText: string;
  Level: integer;
begin
  // If the value is a symbol table entry, use the identifier's name.
  // Else just use the value string.
  isSymTabEntry := Value is ISymTabEntry;
  if isSymTabEntry then
    ValueStr := (Value as ISymTabEntry).getName
  else
    ValueStr := Value.toString;
  aText := Lowercase(KeyString) + '="' + ValueStr + '"';
  Append(' '); Append(aText);
  // Include an identifier's nesting level.
  if isSymTabEntry then begin
    Level := (Value as ISymTabEntry).getSymtab.getNestingLevel;
    PrintAttribute('LEVEL', TInteger.Create(Level));
  end;
end;

//Print a parse tree node's child nodes.
procedure TParseTreePrinter.PrintChildNodes(ChildNodes: TICodeNodeList);
var
  SaveIndentation: string;
  i: integer;
begin
  SaveIndentation := Indentation;
  Indentation += Indent;
  // Iterate over the node entries and print them.
  for i := 0 to ChildNodes.Count-1 do
    PrintNode(TICodeNodeImpl(ChildNodes[i].getObject));
  Indentation := SaveIndentation;
end;

//Print a parse tree node's type specification.
procedure TParseTreePrinter.PrintTypeSpec(Node: TICodeNodeImpl);
var
  TypeSpec: ITypeSpec;
  SaveMargin, TypeName: string;
  TypeID: ISymTabEntry;
begin
  TypeSpec := Node.getTypeSpec;
  if TypeSpec <> Nil then begin
    SaveMargin := Indentation;
    Indentation := Indentation + Indent;
    TypeID := TypeSpec.getIdentifier;
    //Named type: Print the type identifier's name
    if TypeID <> Nil then
      TypeName := TypeID.getName
    else  //Unnamed type: print an artificial type identifier name
      TypeName := '$Anonymous';
    PrintAttribute('Type_ID', TString.Create(TypeName));
    Indentation := SaveMargin;
  end;
end;

procedure TParseTreePrinter.PrintRoutine(RoutineID: ISymTabEntry);
var
  Definition: IDefinition;
  ICode: IIntermediateCode;
  RoutineIDs: TSymTabEntries;
  rtnID: ISymTabEntry;
begin
  Definition := RoutineID.getDefinition;
  writeln(LineEnding, '*** ', Definition.toString.Substring(3),
    ' ', RoutineID.getName, ' ***', LineEnding);
  //Print the intermediate code in the routine's symbol table entry.
  ICode := RoutineID.getAttribute(skRoutineICode) as IIntermediateCode;
  if ICode.getRoot <> Nil then
    PrintNode(TICodeNodeImpl(ICode.getRoot.getObject));
  //Print any procedures and functions defined in the routine
  RoutineIDs := RoutineID.getAttribute(skRoutineRoutines) as TSymTabEntries;
  if RoutineIDs <> Nil then
    for rtnID in RoutineIDs do
      PrintRoutine(rtnID);
end;

constructor TParseTreePrinter.Create(APrintStream: TStringList);
var
  i: Integer;
begin
  PrintStream := APrintStream;
  Len := 0;
  Indentation := '';
  Line := '';
  for i := 1 to IndentWidth do Indent += ' ';
end;

//Print the intermediate code as a parse tree.
procedure TParseTreePrinter.Print(SymTabStack: ISymTabStack;
  var OutputList: TStringList);
var
  ProgramID: ISymTabEntry;
begin
  PrintStream.Add(LineEnding + '===== INTERMEDIATE CODE =====' + LineEnding);
  ProgramID := SymTabStack.getProgramID;
  PrintRoutine(ProgramID);

  PrintLine;
  OutputList := PrintStream;  // export the stringlist
end;

end.


