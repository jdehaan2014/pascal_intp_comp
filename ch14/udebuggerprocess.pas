unit uDebuggerProcess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uIDEControl, Process;

type

  { TDebuggerProcess }

  TDebuggerProcess = class(TThread)
    private
     const
       CommandStr = 'Pascal execute %s %s';
     var
      Control: iIDEControl;            //the IDE control interface
      Process: TProcess;               //the debugger process
      SourceName: string;              //source file name
      toDebuggerStream: TStream;       //IDE to Debugger I/O stream
      DebuggerOutput: TDebuggerOutput; //Debugger process output
      haveSyntaxErrors: boolean;       //true if there are syntax errors
      Debugging: boolean;              //true if debugging process I/O
    public
      constructor Create(aControl: iIDEControl; aSourceName: string);
      procedure Run;
  end;

implementation

{ TDebuggerProcess }

constructor TDebuggerProcess.Create(aControl: iIDEControl; aSourceName: string);
begin
  Inherited Create;
  Control := aControl;
  SourceName := aSourceName;
end;

//run the process
procedure TDebuggerProcess.Run;
var
  Command: String;
begin
  try
    //start the Pascal debugger process
    Command := Format(CommandStr, [Control.getSourcePath, Control.getInputPath]);
    Process :=
  except
  end;
end;

end.

