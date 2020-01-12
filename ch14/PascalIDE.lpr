program PascalIDE;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, IDEMain, uIDEControl, IDEDebugger, IDECallStack, IDEConsoleWindow,
  IDE_Editor, uDebuggerProcess
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormCallStack, FormCallStack);
  Application.CreateForm(TFormConsole, FormConsole);
  Application.CreateForm(TFormEditor, FormEditor);
  Application.CreateForm(TFormDebug, FormDebug);
  Application.Run;
end.

