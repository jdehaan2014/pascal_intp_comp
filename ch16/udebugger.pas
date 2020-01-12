unit uDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, anyobject, fgl, uObjectUtils, uMessages, ubackend,
  ubasescanner, uPascalScanner, uSource, uTextReader, uToken, uTokenType,
  uPascalTokenTyp, uMemory;

type
  //list for maintaining different object types
  TObjectStack = specialize TFPGObjectList<TObject>;

  TBreakPoints = specialize TFPGList<Integer>;
  TWatchPoints = specialize TFPGList<String>;

  //Interface for the interactive source-level debugger.
  TDebugger = class
    private
      FRuntimeStack: IRuntimeStack;   //runtime stack
      BreakPoints: TBreakPoints;     //list of breakpoints
      WatchPoints: TWatchPoints;     //list of watchpoints
      CommandInput: TBaseScanner;    //input source for commands

      {type
        TBackendMessageListener = class(TAnyObject, IMessageListener)
         public
          //Called by the back end whenever it produces a message.
          procedure MessageReceived(Message: TMessage);
        end;  }

    public
      constructor Create(Backend: TBackend; RuntimeStack: IRuntimeStack);
      function getRuntimeStack: IRuntimeStack;
      procedure ReadCommands;
      function currentToken: TToken;
      function nextToken: TToken;
      function getWord(errorMessage: string): String;
      function getInteger(errorMessage: string): Integer;
      function getValue(errorMessage: string): TObject;
      procedure SkipToNextCommand;
      procedure setBreakpoint(LineNumber: Integer);
      procedure unsetBreakpoint(LineNumber: Integer);
      function isBreakpoint(LineNumber: Integer): boolean;
      procedure setWatchPoint(Name: String);
      procedure unsetWatchPoint(Name: String);
      function isWatchPoint(Name: String): boolean;
      //process a message from the backend  note: CLASS added!
      class procedure ProcessMessage(Message: TMessage); virtual; abstract;
      //class procedure ProcessMessage(Message: TMessage); virtual;
      //display a prompt for a debugger command
      procedure PromptForCommand; virtual; abstract;
      //parse a debugger command
      function ParseCommand: boolean; virtual; abstract;
      //process a source statement
      procedure atStatement(LineNumber: Integer); virtual; abstract;
      //process a breakpoint at a statement
      procedure atBreakpoint(LineNumber: Integer); virtual; abstract;
      //process the current value of a watchpoint variablet
      procedure atWatchPointValue(LineNumber: Integer; Name: String;
        Value: TObject); virtual; abstract;
      //process the assigning a new value to a watchpoint variablet
      procedure atWatchPointAssignment(LineNumber: Integer; Name: String;
        Value: TObject); virtual; abstract;
      //process calling a declared procedure or function
      procedure CallRoutine(LineNumber: Integer;
        RoutineName: String); virtual; abstract;
      //process returning from a declared procedure or function
      procedure ReturnRoutine(LineNumber: Integer;
        RoutineName: String); virtual; abstract;
      //Display a value
      procedure DisplayValue(ValueString: TString); virtual; abstract;
      //Display the call stack
      procedure DisplayCallStack(Stack: TObjectStack); virtual; abstract;
      //Terminate execution of the source program
      procedure Quit; virtual; abstract;
      //Handle a debugger command error
      procedure CommandError(ErrorMessage: string); virtual; abstract;
      //Handle a source program error
      procedure RuntimeError(ErrorMessage: string;
        LineNumber: Integer); virtual; abstract;
  end;

implementation

//listener for back end messages
{procedure TDebugger.TBackendMessageListener.MessageReceived(Message: TMessage);
begin
  ProcessMessage(Message);
end; }

constructor TDebugger.Create(Backend: TBackend; RuntimeStack: IRuntimeStack);
begin
  FRuntimeStack := RuntimeStack;
  //Backend.addMessageListener(TBackendMessageListener.Create);
  BreakPoints := TBreakPoints.Create;
  WatchPoints := TWatchPoints.Create;
  try
    CommandInput := TPascalScanner.Create(
      TSource.Create(TTextReader.Create(THandleStream.Create(StdInputHandle)))
    );
  except
    on Ignore: Exception do begin
      Writeln('Command input initialization error');
    end;
  end;
end;

function TDebugger.getRuntimeStack: IRuntimeStack;
begin
  Result := FRuntimeStack;
end;

//read the debugger commands
procedure TDebugger.ReadCommands;
begin
  repeat
    PromptForCommand;
  until not ParseCommand;
end;

//return the current token from the command input
function TDebugger.currentToken: TToken;
begin
  Result := CommandInput.currentToken;
end;

//return the next token from the command input
function TDebugger.nextToken: TToken;
begin
  Result := CommandInput.nextToken;
end;

//get the next word token from the command input
function TDebugger.getWord(errorMessage: string): String;
var
  Token: TToken;
  Typ: ITokenTyp;
begin
  Result := '';
  Token := currentToken;
  Typ := Token.getTyp;
  if Typ = ttIdentifier then begin
    Result := Token.getText.toLower;
    nextToken;
  end
  else
    raise Exception.Create(errorMessage);
end;

//get the next integer constant token from the command input
function TDebugger.getInteger(errorMessage: string): Integer;
var
  Token: TToken;
  Typ: ITokenTyp;
  Value: Integer;
begin
  Token := currentToken;
  Typ := Token.getTyp;
  if Typ = ttInteger then begin
    Value := TInteger(Token.getValue).intValue;
    nextToken;
    Result := Value;
  end
  else
    raise Exception.Create(errorMessage);
end;

//get the next constant value from the command input
function TDebugger.getValue(errorMessage: string): TObject;
var
  Token: TToken;
  TokenTyp: ITokenTyp;
  intValue: Integer;
  fltValue: Real;
  strValue, Name: string;
  Sign, Minus: boolean;
begin
  Result := Nil;
  Token := currentToken;
  TokenTyp := Token.getTyp;
  Sign := False;
  Minus := False;
  //unary plus or minus sign
  if (TokenTyp = ttMinus) or (TokenTyp = ttPlus) then begin
    Sign := True;
    Minus := TokenTyp = ttMinus;
    Token := nextToken;
    TokenTyp := Token.getTyp;
  end;
  case TPascalTokenTyp.toTyp(TokenTyp) of
    ttInteger:
      begin
        intValue := TInteger(Token.getValue).intValue;
        nextToken;
        if Minus then intValue := -intValue;
        Result := TInteger.Create(intValue);
      end;
    ttReal:
      begin
        fltValue := TFloat(Token.getValue).floatValue;
        nextToken;
        if Minus then fltValue := -fltValue;
        Result := TFloat.Create(fltValue);
      end;
    ttString:
      begin
        if Sign then
          Raise Exception.Create(errorMessage)
        else begin
          strValue := TString(Token.getValue).Value;
          nextToken;
          Result := TChar.Create(strValue[1]);
        end;
      end;
    ttIdentifier:
      begin
        if Sign then
          Raise Exception.Create(errorMessage)
        else begin
          Name := Token.getText;
          nextToken;
          if Name.toLower = 'true' then
            Result := TBoolean.Create(True)
          else if Name.toLower = 'false' then
            Result := TBoolean.Create(False)
          else
            Raise Exception.Create(errorMessage);
        end;
      end;
    else Raise Exception.Create(errorMessage)
  end;
end;

//skip the rest of this command input line
procedure TDebugger.SkipToNextCommand;
begin
  CommandInput.SkipToNextLine;
end;

//set a breakpoint at a source line
procedure TDebugger.setBreakpoint(LineNumber: Integer);
begin
  BreakPoints.Add(LineNumber);
end;

//remove a breakpoint at a source line
procedure TDebugger.unsetBreakpoint(LineNumber: Integer);
begin
  BreakPoints.Remove(LineNumber);
end;

//check if a source line is at a breakpoint
function TDebugger.isBreakpoint(LineNumber: Integer): boolean;
begin
  if BreakPoints.IndexOf(LineNumber) >= 0 then
    Result := True
  else
    Result := False;
end;

//set a watchpoint at a variable
procedure TDebugger.setWatchPoint(Name: String);
begin
  WatchPoints.Add(Name);
end;

//remove a watchpoint at a variable
procedure TDebugger.unsetWatchPoint(Name: String);
begin
  WatchPoints.Remove(Name);
end;

//check if a variable is a watchpoint
function TDebugger.isWatchPoint(Name: String): boolean;
begin
  if WatchPoints.IndexOf(Name) >= 0 then
    Result := True
  else
    Result := False;
end;


end.

