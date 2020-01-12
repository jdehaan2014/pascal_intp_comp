unit IDEDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls;

type

  { TFormDebug }

  TFormDebug = class(TForm)
    ButtonGo: TButton;
    ButtonSingleStep: TButton;
    ButtonAutoStep: TButton;
    ButtonSlowerStepping: TButton;
    ButtonFasterStepping: TButton;
    ButtonQuit: TButton;
    EditInterpreterMsg: TLabeledEdit;
    Panel1: TPanel;
    Debugger: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormDebug: TFormDebug;

implementation

{$R *.lfm}

{ TFormDebug }

procedure TFormDebug.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caMinimize;
end;

end.

