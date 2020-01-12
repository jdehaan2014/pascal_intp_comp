unit uDebuggerImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDebugger, uBackend, uMemory, uMemoryImpl, uMessages,
  uObjectUtils, uGenericInterfaces, uPascalTokentyp,
  uTypesImpl, uPredefined, uSymTabImpl, fgl, anyobject;

type

  { TNameValuePair }

  TNameValuePair = class
    private
      const
        MaxDisplayedElements = 10;
      var
        FVariableName: string;
        FValueString: TString;
      class procedure ArrayValueString(CellArray: TCellList; Buffer: TString); static;
      class procedure RecordValueString(aRecord: TMemoryMap; Buffer: TString); static;
    public
      class function ValueString(Value: TObject): TString; static;
      Constructor Create(VariableName: string; Value: TObject);
      function getVariableName: string;
      function getValueString: TString;
  end;


  { TCellTypePair }

  TCellTypePair = class
    private
      FCell: ICell;     //memory cell
      FTyp: ITypeSpec;  //data type
      FDebugger: TDebugger; //parent debugger
    protected
      const
        //Synchronization set for variable modifiers.
        ModifierSet: TPascalTokenTypSet = [ttLeftBracket, ttDot];
      procedure ParseVariable;
      procedure setValue(Value: TObject);
    public
      Constructor Create(Typ: ITypeSpec; Cell: ICell; Debugger: TDebugger);
      property getCell: ICell read FCell;
      property getType: ITypeSpec read FTyp;
    private
      procedure ParseArrayVariable(ArrayVar: TCellList);
      procedure ParseRecordVariable(RecordVar: TMemoryMap);
      procedure RangeCheck(Value: integer; Typ: ITypeSpec; ErrorMessage: string);
  end;

  { TCommandProcessor }

  TCommandProcessor = class
    private
      FDebugger: TDebugger;   // the debugger
      FStepping: boolean;     //true when single stepping
    public
      constructor Create(Debugger: TDebugger);
    protected
      procedure ProcessMessage(Message: TMessage);
      function ParseCommand: boolean;
    private
      function ExecuteCommand(Command: string): boolean;
      procedure Stack;
      procedure Show;
      procedure Assign;
      function CreateCellTypePair: TCellTypePair;
      procedure CheckForSemicolon;
  end;

  { TCommandLineDebugger }

  TCommandLineDebugger = class(TDebugger)
    private
      class var CommandProcessor: TCommandProcessor;
      type
        TBackendMessageListener = class(TAnyObject, IMessageListener)
         public
          //Called by the back end whenever it produces a message.
          procedure MessageReceived(Message: TMessage);
        end;

    public
      Constructor Create(Backend: TBackend; RuntimeStack: IRuntimeStack);
      class procedure ProcessMessage(Message: TMessage); override;
      procedure PromptForCommand; override;
      function ParseCommand: boolean; override;
      procedure atStatement(LineNumber: Integer); override;
      procedure atBreakpoint(LineNumber: Integer); override;
      procedure atWatchpointValue(LineNumber: Integer; Name: String;
        Value: TObject); override;
      procedure atWatchPointAssignment(LineNumber: Integer; Name: String;
        Value: TObject); override;
      procedure CallRoutine(LineNumber: Integer; RoutineName: String); override;
      procedure ReturnRoutine(LineNumber: Integer; RoutineName: String); override;
      procedure DisplayValue(ValueString: TString); override;
      procedure DisplayCallStack(Stack: TObjectStack); override;
      procedure Quit; override;
      procedure CommandError(ErrorMessage: string); override;
      procedure RuntimeError(ErrorMessage: string; LineNumber: Integer); override;
  end;

implementation

{ TNameValuePair }

constructor TNameValuePair.Create(VariableName: string; Value: TObject);
begin
  FVariableName := VariableName;
  FValueString := ValueString(Value);
end;

function TNameValuePair.getVariableName: string;
begin
  Result := FVariableName;
end;

function TNameValuePair.getValueString: TString;
begin
  Result := FValueString;
end;

//convert a value into a value string
class function TNameValuePair.ValueString(Value: TObject): TString;
var
  Buffer: TString;
begin
  Buffer := TString.Create;
  //unefined value
  if Value = Nil then
    Buffer.Append('?')
  else if Value is ICell then  //dereference a VAR parameter
    Buffer.Append(ValueString((Value as ICell).getValue))
  else if Value is TCellList then  //array value
    ArrayValueString(TCellList(Value), Buffer)
  else if Value is TMemoryMap then //record value
    RecordValueString(TMemoryMap(Value), Buffer)
  else if Value is TChar then  //character value
    Buffer.Append('''').Append(TChar(Value)).Append('''')
  else  //numeric or boolean value
    Buffer.Append(Value.toString);
  Result := Buffer;
end;

//convert an array value into a value string
class procedure TNameValuePair.ArrayValueString
  (CellArray: TCellList; Buffer: TString);
var
  ElementCount: integer = 0;
  First: boolean = True;
  Cell: ICell;
begin
  Buffer.Append('[');
  //loop over each array element up to MaxDisplayedElements times
  for Cell in CellArray do begin
    if First then
      First := False
    else
      Buffer.Append(', ');
    ElementCount += 1;
    if ElementCount <= MaxDisplayedElements then
      Buffer.Append(ValueString(Cell.getValue))
    else begin
      Buffer.Append('...');
      Break;
    end;
  end;
  Buffer.Append(']');
end;

class procedure TNameValuePair.RecordValueString
  (aRecord: TMemoryMap; Buffer: TString);
var
  First: boolean = True;
  Entries: TMemoryMap;
  i: integer;
begin
  Buffer.Append('{');
  Entries := aRecord;
  for i := 0 to Entries.Count - 1 do begin
    if First then
      First := False
    else
      Buffer.Append(', ');
    with Entries do
      Buffer.Append(Keys[i]).Append(': ').Append(ValueString(Data[i].getObject));
  end;
  Buffer.Append('}');
end;



{ TCellTypePair }

constructor TCellTypePair.Create
  (Typ: ITypeSpec; Cell: ICell; Debugger: TDebugger);
begin
  FTyp := Typ;
  FCell := Cell;
  FDebugger := Debugger;
  ParseVariable;
end;

//Parse a variable in the command to obtain its memory cell.
procedure TCellTypePair.ParseVariable;
var
  Form: ITypeForm;
  Value: TObject;
begin
  Form := FTyp.getForm;
  Value := FCell.getValue;
  //loop to process array subscripts and record fields
  while ModifierSet.Contains(FDebugger.CurrentToken.getTyp) do begin
    if Form = tfArray then
      ParseArrayVariable(TCellList(Value))
    else if Form = tfRecord then
      ParseRecordVariable(TMemoryMap(Value));
    Value := FCell.getValue;
    Form := FTyp.getForm;
  end;
end;

//Parse an array variable.
procedure TCellTypePair.ParseArrayVariable(ArrayVar: TCellList);
var
  Index, minValue: integer;
  IndexType: ITypeSpec;
begin
  FDebugger.nextToken;
  Index := FDebugger.getInteger('Integer index expected.');
  minValue := 0;
  IndexType := FTyp.getAttribute(tkArrayIndexType) as ITypeSpec;
  RangeCheck(Index, IndexType, 'Index out of range.');
  FTyp := FTyp.getAttribute(tkArrayElementType) as ITypeSpec;
  if IndexType.getForm = tfSubrange then
    minValue := TInteger(IndexType.getAttribute(tkSubRangeMinValue)).intValue;
  FCell := ArrayVar[Index-minValue];
  if FDebugger.CurrentToken.getTyp = ttRightBracket then
    FDebugger.NextToken
  else
    Raise Exception.Create('] expected.');
end;

//Parse a record variable.
procedure TCellTypePair.ParseRecordVariable(RecordVar: TMemoryMap);
var
  FieldName: string;
  SymTab: ISymTab;
  ID: ISymTabEntry;
begin
  FDebugger.nextToken;
  FieldName := FDebugger.getWord('Field name expected.');
  if RecordVar.indexOf(FieldName) >= 0 then
    FCell := RecordVar.KeyData[FieldName]
  else
    Raise Exception.Create('Invalid field name');
  SymTab := FTyp.getAttribute(tkRecordSymTab) as ISymTab;
  ID := SymTab.Lookup(FieldName);
  FTyp := ID.getTypeSpec;
end;

//Set the value of the cell.
procedure TCellTypePair.setValue(Value: TObject);
begin
  if ((FTyp.baseType = TPredefined.integerType) and (Value is TInteger) or
      (FTyp = TPredefined.realType) and (Value is TFloat) or
      (FTyp = TPredefined.booleanType) and (Value is TBoolean) or
      (FTyp = TPredefined.charType) and (Value is TChar)
     ) then
  begin
    if FTyp.baseType = TPredefined.integerType then begin
      RangeCheck(TInteger(Value).intValue, FTyp, 'Value out of range.');
      FCell.setValue(Value);
    end
    else
      Raise Exception.Create('Type mismatch.');
  end;
end;

//Do a range check on an integer value.
procedure TCellTypePair.RangeCheck(Value: integer; Typ: ITypeSpec;
  ErrorMessage: string);
var
  Form: ITypeForm;
  minValue: TInteger = Nil;
  maxValue: TInteger = Nil;
  Constants: TSymTabEntries;
begin
  Form := FTyp.getForm;
  if Form = tfSubrange then begin
    minValue := TInteger(FTyp.getAttribute(tkSubRangeMinValue));
    maxValue := TInteger(FTyp.getAttribute(tkSubRangeMaxValue));
  end
  else if Form = tfEnumeration then begin
    Constants := TSymTabEntries(FTyp.getAttribute(tkEnumerationConstants));
    minValue := TInteger.Create(0);
    maxValue := TInteger.Create(Constants.Count - 1);
  end;
  if (minValue <> Nil) and
     ((Value < TInteger(minValue).intValue) or (Value > TInteger(maxValue).intValue)
     ) then
    Raise Exception.Create(ErrorMessage);
end;


{ TCommandProcessor }

constructor TCommandProcessor.Create(Debugger: TDebugger);
begin
  FDebugger := Debugger;
  FStepping := True;
end;

//Process a msg from the backend
procedure TCommandProcessor.ProcessMessage(Message: TMessage);
var
  Typ: TMessageType;
  LineNumber: Integer;
  VariableName, RoutineName, ErrorMsg: string;
  Value: TObject;
begin
  Typ := Message.getTyp;
  case Typ of
    mtSourceLine:
      begin
        LineNumber := (Message.getBody as TSourceLineMsg).LineNum;
        if FStepping then begin
          FDebugger.atStatement(LineNumber);
          FDebugger.ReadCommands;
        end
        else if FDebugger.isBreakpoint(LineNumber) then begin
          FDebugger.atBreakpoint(LineNumber);
          FDebugger.ReadCommands;
        end;
      end;
    mtFetch:
      begin
        VariableName := (Message.getBody as TFetchMsg).TargetVariableName.toLower;
        if FDebugger.isWatchpoint(VariableName) then begin
          LineNumber   := (Message.getBody as TFetchMsg).SourceLineNumber;
          Value        := (Message.getBody as TFetchMsg).ExpressionValue;
          FDebugger.atWatchpointValue(LineNumber, VariableName, Value);
        end;
      end;
    mtAssign:
      begin
        VariableName := (Message.getBody as TAssignMsg).TargetVariableName.toLower;
        if FDebugger.isWatchpoint(VariableName) then begin
          LineNumber   := (Message.getBody as TAssignMsg).SourceLineNumber;
          Value        := (Message.getBody as TAssignMsg).ExpressionValue;
          FDebugger.atWatchpointAssignment(LineNumber, VariableName, Value);
        end;
      end;
    mtCall:
      begin
        LineNumber  := (Message.getBody as TCallMsg).SourceLineNumber;
        RoutineName := (Message.getBody as TCallMsg).CalledRoutineName;
        FDebugger.CallRoutine(LineNumber, RoutineName);
      end;
    mtReturn:
      begin
        LineNumber  := (Message.getBody as TReturnMsg).SourceLineNumber;
        RoutineName := (Message.getBody as TReturnMsg).ReturnedRoutineName;
        FDebugger.ReturnRoutine(LineNumber, RoutineName);
      end;
    mtRuntimeError:
      begin
        ErrorMsg   := (Message.getBody as TRuntimeErrorMsg).RuntimeErrorMsg;
        LineNumber := (Message.getBody as TRuntimeErrorMsg).SourceLineNumber;
        FDebugger.RuntimeError(ErrorMsg, LineNumber);
      end;
  end;
end;

//Parse a debugger command
function TCommandProcessor.ParseCommand: boolean;
var
  AnotherCommand: boolean = True;
  Command: string;
begin
  //parse a command
  try
    FDebugger.NextToken;
    Command := FDebugger.getWord('Command expected.');
    AnotherCommand := ExecuteCommand(Command);
  except
    on E: Exception do
      FDebugger.CommandError(E.Message);
  end;

  //Skip to the next command
  try
    FDebugger.SkipToNextCommand;
  except
    on E: Exception do
      FDebugger.CommandError(E.Message);
  end;

  Result := AnotherCommand;
end;

function TCommandProcessor.ExecuteCommand(Command: string): boolean;
var
  LineNumber: integer;
  Name: string;
begin
  FStepping := False;
  case Command of
    'assign':  begin
                 Assign;
                 Result := True;
               end;
    'break':   begin
                 LineNumber := FDebugger.getInteger('Line number expected.');
                 CheckForSemicolon;
                 FDebugger.setBreakpoint(LineNumber);
                 Result := True;
               end;
    'go':      begin
                 CheckForSemicolon;
                 Result := False;
               end;
    'quit':    begin
                 CheckForSemicolon;
                 FDebugger.Quit;
               end;
    'show':    begin
                 Show;
                 Result := True;
               end;
    'stack':   begin
                 CheckForSemicolon;
                 Stack;
                 Result := True;
               end;
    'step':    begin
                 FStepping := True;
                 CheckForSemicolon;
                 Result := False;
               end;
    'unbreak': begin
                 LineNumber := FDebugger.getInteger('Line number expected.');
                 CheckForSemicolon;
                 FDebugger.unsetBreakpoint(LineNumber);
                 Result := True;
               end;
    'unwatch': begin
                 Name := FDebugger.getWord('Variable name expected.');
                 CheckForSemicolon;
                 FDebugger.unsetWatchpoint(Name);
                 Result := True;
               end;
    'watch':   begin
                 Name := FDebugger.getWord('Variable name expected.');
                 CheckForSemicolon;
                 FDebugger.setWatchpoint(Name);
                 Result := True;
               end;
    else raise Exception.Create('Invalid command: ' + Command + '.');
  end;
end;

//create the call stack for display
procedure TCommandProcessor.Stack;
var
  CallStack: TObjectStack;
  RuntimeStack: IRuntimeStack;
  ArList: TActivationRecords;
  Ar: IActivationRecord;
  RoutineID: ISymTabEntry;
  i: integer;
  Name: string;
  Value: TObject;
begin
  CallStack := TObjectStack.Create;
  //Loop over the activation records on the runtime stack starting at stack top
  RuntimeStack := FDebugger.getRuntimeStack;
  ArList := RuntimeStack.Records;
  for i := ArList.Count - 1 downto 0 do begin
    Ar := ArList[i];
    RoutineID := Ar.getRoutineID;
    //add the symbol table entry of the procedure or function
    CallStack.Add(RoutineID.getObject);
    //create and add a name-value pair for each local variable
    for Name in Ar.getAllNames do begin
      Value := Ar.getCell(Name).getValue;
      CallStack.Add(TNameValuePair.Create(Name, Value));
    end;
  end;
  //display the call stack
  FDebugger.DisplayCallStack(CallStack);
end;

//show the current value of a variable
procedure TCommandProcessor.Show;
var
  Pair: TCellTypePair;
  Cell: ICell;
begin
  Pair := CreateCellTypePair;
  Cell := Pair.getCell;
  CheckForSemicolon;
  FDebugger.DisplayValue(TNameValuePair.ValueString(Cell.getValue));
end;

//assign a new value to a variable
procedure TCommandProcessor.Assign;
var
  Pair: TCellTypePair;
  newValue: TObject;
begin
  Pair := CreateCellTypePair;
  newValue := FDebugger.getValue('Invalid value.');
  CheckForSemicolon;
  Pair.setValue(newValue);
end;

//Create a cell-data type pair.
function TCommandProcessor.CreateCellTypePair: TCellTypePair;
var
  RuntimeStack: IRuntimeStack;
  Ar: IActivationRecord = Nil;
  ID: ISymTabEntry;
  CurrentLevel, Level: integer;
  Cell: ICell = Nil;
  VariableName: string;
  SymTab: ISymTab;
begin
  RuntimeStack := FDebugger.getRuntimeStack;
  CurrentLevel := RuntimeStack.CurrentNestingLevel;
  //parse the variable name
  VariableName := FDebugger.getWord('Variable name expected.');
  //find the variable's cell in the call stack
  Level := CurrentLevel;
  while (Cell = Nil) and (Level > 0) do begin
    Level -= 1;                                           //NOTE!!!
    Ar := RuntimeStack.getTopMost(Level);
    Cell := Ar.getCell(VariableName);
  end;
  if Cell = Nil then
    Raise Exception.Create('Undeclared variable name "' + VariableName + '".');
  //VAR parameter
  if Cell.getValue is ICell then
    Cell := Cell.getValue as ICell;
  //find the variable's symbol table entry
  SymTab := Ar.getRoutineID.getAttribute(skRoutineSymtab) as ISymTab;
  ID := SymTab.Lookup(VariableName);
  Result := TCellTypePair.Create(ID.getTypeSpec, Cell, FDebugger);
end;

procedure TCommandProcessor.CheckForSemicolon;
begin
  if FDebugger.CurrentToken.getTyp <> ttSemicolon then
    raise Exception.Create('Invalid command syntax.');
end;

{ TCommandLineDebugger }

//listener for back end messages
procedure TCommandLineDebugger.TBackendMessageListener.MessageReceived(Message: TMessage);
begin
  ProcessMessage(Message);
end;


constructor TCommandLineDebugger.Create
  (Backend: TBackend; RuntimeStack: IRuntimeStack);
begin
  inherited Create(Backend, RuntimeStack);
  CommandProcessor := TCommandProcessor.Create(Self);
  Backend.addMessageListener(TBackendMessageListener.Create);
end;

//process a msg from the backend
class procedure TCommandLineDebugger.ProcessMessage(Message: TMessage);
begin
  CommandProcessor.ProcessMessage(Message);
end;

//Display a prompt for a debugger command
procedure TCommandLineDebugger.PromptForCommand;
begin
  Write('>>> Command? ');
end;

//Parse a debugger command
function TCommandLineDebugger.ParseCommand: boolean;
begin
  Result := CommandProcessor.ParseCommand;
end;

//process a source statement
procedure TCommandLineDebugger.atStatement(LineNumber: Integer);
begin
  WriteLn(LineEnding + '>>> At line ', LineNumber);
end;

//process a breakpoint at a statement
procedure TCommandLineDebugger.atBreakpoint(LineNumber: Integer);
begin
  WriteLn(LineEnding + '>>> Breakpoint at line ', LineNumber);
end;

//process the current value of a watchpoint variablet
procedure TCommandLineDebugger.atWatchpointValue
  (LineNumber: Integer; Name: String; Value: TObject);
begin
  WriteLn(LineEnding + '>>> At line ', LineNumber, ': ',
            Name, ': ', Value.toString);
end;

//process the assigning a new value to a watchpoint variablet
procedure TCommandLineDebugger.atWatchPointAssignment
  (LineNumber: Integer; Name: String; Value: TObject);
begin
  WriteLn(LineEnding + '>>> At line ', LineNumber, ': ',
            Name, ' := ', Value.toString);
end;

//process calling a declared procedure or function
procedure TCommandLineDebugger.CallRoutine
  (LineNumber: Integer; RoutineName: String);
begin
  //nothing yet
end;

//process returning from a declared procedure or function
procedure TCommandLineDebugger.ReturnRoutine
  (LineNumber: Integer; RoutineName: String);
begin
  //nothing yet
end;

//Display a value
procedure TCommandLineDebugger.DisplayValue(ValueString: TString);
begin
  Writeln(ValueString.Value);
end;

//Display the call stack
procedure TCommandLineDebugger.DisplayCallStack(Stack: TObjectStack);
var
  Item: TObject;
  RoutineID: ISymTabEntry;
  RoutineName: string;
  Level: integer;
  Definition: IDefinition;
  Pair: TNameValuePair;
begin
  for Item in Stack do begin
    //name of procedure or function
    if Item is ISymTabEntry then begin
      RoutineID := Item as ISymTabEntry;
      RoutineName := RoutineID.getName;
      Level := RoutineID.getSymTab.getNestingLevel;
      Definition := RoutineID.getDefinition;
      WriteLn(Level, ': ', Definition.getText.toUpper, ' ', RoutineName);
    end
    else if Item is TNameValuePair then begin // Variable name-value pair
      Pair := TNameValuePair(Item);
      Write('  ', Pair.getVariableName, ': ');
      DisplayValue(Pair.getValueString);
    end;
  end;
end;

//Terminate execution of the source program
procedure TCommandLineDebugger.Quit;
begin
  WriteLn('Program terminated.');
  Halt(-1);
end;

//Handle a debugger command error
procedure TCommandLineDebugger.CommandError(ErrorMessage: string);
begin
  WriteLn('!!! ERROR: ', ErrorMessage);
end;

//Handle a source program error
procedure TCommandLineDebugger.RuntimeError
  (ErrorMessage: string; LineNumber: Integer);
begin
  Write('!!! RUNTIME ERROR');
  if LineNumber > 0 then
    Write(' at line ', LineNumber:3);
  WriteLn(': ',  ErrorMessage);
end;

end.

