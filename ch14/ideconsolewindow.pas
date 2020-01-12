unit IDEConsoleWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormConsole }

  TFormConsole = class(TForm)
    ButtonEnter: TButton;
    EditRuntimeInput: TLabeledEdit;
    MemoOutput: TMemo;
    Panel1: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  FormConsole: TFormConsole;

implementation

{$R *.lfm}

{ TFormConsole }

procedure TFormConsole.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction := caMinimize;
end;

end.

