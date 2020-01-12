unit IDECallStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls;

type

  { TFormCallStack }

  TFormCallStack = class(TForm)
    ButtonChange: TButton;
    EditName: TLabeledEdit;
    EditValue: TLabeledEdit;
    Panel1: TPanel;
    TreeView1: TTreeView;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormCallStack: TFormCallStack;

implementation

{$R *.lfm}

{ TFormCallStack }

procedure TFormCallStack.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caMinimize;
end;

end.

