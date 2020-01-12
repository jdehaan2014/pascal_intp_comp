unit uIDEControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  LISTING_TAG     = '!LISTING:';
  PARSER_TAG      = '!PARSER:';
  SYNTAX_TAG      = '!SYNTAX:';
  INTERPRETER_TAG = '!INTERPRETER:';

  DEBUGGER_AT_TAG       = '!DEBUGGER.AT:';
  DEBUGGER_BREAK_TAG    = '!DEBUGGER.BREAK:';
  DEBUGGER_ROUTINE_TAG  = '!DEBUGGER.ROUTINE:';
  DEBUGGER_VARIABLE_TAG = '!DEBUGGER.VARIABLE:';



type
  IIDEControl = interface
    //set or get the path of the source file
    procedure setSourcePath(SourcePath: string);
    function getSourcePath: string;

    //set or get the path of the runtime input data file
    procedure setInputPath(InputPath: string);
    function getInputPath: string;

    //start / stop the debugger
    procedure StartDebuggerProcess(SourceName: string);
    procedure StopDebuggerProcess;

    //Send a command or runtime input text to the debugger process.
    procedure sendToDebuggerProcess(Text: string);

    //Set the editor window's message.
    procedure setEditWindowMessage(Message: string; Color: string);

    //Clear the editor window's syntax errors.
    procedure clearEditWindowErrors;

    //Add a syntax error message to the editor window's syntax errors.
    procedure addToEditWindowErrors(Line: string);

    //Show the debugger window.
    procedure showDebugWindow(SourceName: string);

    //Clear the debugger window's listing.
    procedure clearDebugWindowListing;

    //Add a line to the debugger window's listing.
    procedure addToDebugWindowListing(Line: string);

    //Select a listing line in the debugger window.
    procedure selectDebugWindowListingLine(LineNumber: integer);

    //Set the debugger to a listing line.
    procedure setDebugWindowAtListingLine(LineNumber: integer);

    //Set the debugger to break at a listing line.
    procedure breakDebugWindowAtListingLine(LineNumber: integer);

    //Set the debugger window's message.
    procedure setDebugWindowMessage(Message: string; Color: string);

    //Stop the debugger.
    procedure stopDebugWindow;

    //Show the call stack window.
    procedure showCallStackWindow(SourceName: string);

    //Initialize the call stack display.
    procedure initializeCallStackWindow;

    //Add an invoked routine to the call stack display.
    procedure addRoutineToCallStackWindow(Level, Header: string);

    //Add a local variable to the call stack display.
    procedure addVariableToCallStackWindow(Name, Value: string);

    //Complete the call stack display.
    procedure completeCallStackWindow;

    //Show the console window.
    procedure showConsoleWindow(SourceName: string);

    //Clear the console window's output.
    procedure clearConsoleWindowOutput;

    //Add output text to the console window.
    procedure addToConsoleWindowOutput(Text: string);

    //Enable runtime input from the console window.
    procedure enableConsoleWindowInput;

    //Disable runtime input from the console window.
    procedure disableConsoleWindowInput;
  end;

implementation

end.

