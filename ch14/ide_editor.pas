unit IDE_Editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, EditBtn, Buttons;

type

  { TFormEditor }

  TFormEditor = class(TForm)
    EditParserMsg: TEdit;
    FileNameEdit1: TFileNameEdit;
    FileNameEdit2: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    ListBoxSyntaxErrors: TListBox;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    ToolBar1: TToolBar;
    ButtonNew: TToolButton;
    ButtonRedo: TToolButton;
    ButtonOpen: TToolButton;
    ButtonSave: TToolButton;
    ToolButton1: TToolButton;
    ButtonRun: TToolButton;
    ToolButton4: TToolButton;
    ButtonCut: TToolButton;
    ButtonCopy: TToolButton;
    ButtonPaste: TToolButton;
    ToolButton8: TToolButton;
    ButtonUndo: TToolButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  FormEditor: TFormEditor;

implementation

{$R *.lfm}

{ TFormEditor }

procedure TFormEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caMinimize;
end;

end.

