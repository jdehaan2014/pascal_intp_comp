unit IDEMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, Forms, Controls,
  Graphics, Dialogs, ComCtrls, ExtCtrls, EditBtn, Buttons, Menus,
  IDE_Editor, IDECallStack, IDEDebugger, IDEConsoleWindow, uIDEControl;

type

  { TFormMain }

  TFormMain = class(TForm)
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItemViewConsole: TMenuItem;
    MenuItemViewDebugger: TMenuItem;
    MenuItemViewCallStack: TMenuItem;
    MenuItemViewEditor: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemFile: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ButtonExit: TToolButton;
    ButtonViewConsole: TToolButton;
    ButtonViewDebugger: TToolButton;
    ButtonViewCallStack: TToolButton;
    ButtonViewEditor: TToolButton;
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonViewCallStackClick(Sender: TObject);
    procedure ButtonViewConsoleClick(Sender: TObject);
    procedure ButtonViewDebuggerClick(Sender: TObject);
    procedure ButtonViewEditorClick(Sender: TObject);
    procedure MenuItemViewCallStackClick(Sender: TObject);
    procedure MenuItemViewConsoleClick(Sender: TObject);
    procedure MenuItemViewDebuggerClick(Sender: TObject);
    procedure MenuItemViewEditorClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }


procedure TFormMain.MenuItemViewEditorClick(Sender: TObject);
begin
  MenuItemViewEditor.Checked := not MenuItemViewEditor.Checked;
  if MenuItemViewEditor.Checked then begin
    FormEditor.Visible := True;
    ButtonViewEditor.Down := True;
  end
  else begin
    FormEditor.Visible := False;
    ButtonViewEditor.Down := False;
  end;
end;

procedure TFormMain.MenuItemViewCallStackClick(Sender: TObject);
begin
  MenuItemViewCallStack.Checked := not MenuItemViewCallStack.Checked;
  if MenuItemViewCallStack.Checked then begin
    FormCallStack.Visible := True;
    ButtonViewCallStack.Down := True;
  end
  else begin
    FormCallStack.Visible := False;
    ButtonViewCallStack.Down := False;
  end;
end;

procedure TFormMain.MenuItemViewConsoleClick(Sender: TObject);
begin
  MenuItemViewConsole.Checked := not MenuItemViewConsole.Checked;
  if MenuItemViewConsole.Checked then begin
    FormConsole.Visible := True;
    ButtonViewConsole.Down := True;
  end
  else begin
    FormConsole.Visible := False;
    ButtonViewConsole.Down := False;
  end;
end;

procedure TFormMain.MenuItemViewDebuggerClick(Sender: TObject);
begin
  MenuItemViewDebugger.Checked := not MenuItemViewDebugger.Checked;
  if MenuItemViewDebugger.Checked then begin
    FormDebug.Visible := True;
    ButtonViewDebugger.Down := True;
  end
  else begin
    FormDebug.Visible := False;
    ButtonViewDebugger.Down := False;
  end;
end;

procedure TFormMain.ButtonExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ButtonViewCallStackClick(Sender: TObject);
begin
  if ButtonViewCallStack.Down then begin
    FormCallStack.Visible := True;
    MenuItemViewCallStack.Checked := True;
  end
  else begin
    FormCallStack.Visible := False;
    MenuItemViewCallStack.Checked := False;
  end;
end;

procedure TFormMain.ButtonViewConsoleClick(Sender: TObject);
begin
  if ButtonViewConsole.Down then begin
    FormConsole.Visible := True;
    MenuItemViewConsole.Checked := True;
  end
  else begin
    FormConsole.Visible := False;
    MenuItemViewConsole.Checked := False;
  end;
end;

procedure TFormMain.ButtonViewDebuggerClick(Sender: TObject);
begin
  if ButtonViewDebugger.Down then begin
    FormDebug.Visible := True;
    MenuItemViewDebugger.Checked := True;
  end
  else begin
    FormDebug.Visible := False;
    MenuItemViewDebugger.Checked := False;
  end;
end;

procedure TFormMain.ButtonViewEditorClick(Sender: TObject);
begin
  if ButtonViewEditor.Down then begin
    FormEditor.Visible := True;
    MenuItemViewEditor.Checked := True;
  end
  else begin
    FormEditor.Visible := False;
    MenuItemViewEditor.Checked := False;
  end;
end;

end.

