unit GUI;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Test program for Metron }

{ Version 1.0.0 (Atlas) }

{ (c) Johannes W. Dietrich, 1994 - 2026 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2026 }

{ Handler for measurements and readings }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://puma-repository.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, ActnList, StdActns, LCLType, Metron;

type

{ TMetronTesterMainForm }

  TMetronTesterMainForm = class(TForm)
    ActionList1: TActionList;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    HelpAction1: THelpAction;
    InputEdit1: TLabeledEdit;
    InputEdit2: TLabeledEdit;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    EditMenu: TMenuItem;
    HelpMenu: TMenuItem;
    CloseItem: TMenuItem;
    MenuItem1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    HelpItem: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    SelectAllItem: TMenuItem;
    PasteItem: TMenuItem;
    UndoItem: TMenuItem;
    QuitItem: TMenuItem;
    MenuItem3: TMenuItem;
    OpenItem: TMenuItem;
    NewItem: TMenuItem;
    ResultEdit: TLabeledEdit;
    PlusButton: TButton;
    TimesButton: TButton;
    MinusButton: TButton;
    DivButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure PlusButtonClick(Sender: TObject);
    procedure QuitItemClick(Sender: TObject);
    procedure TimesButtonClick(Sender: TObject);
    procedure MinusButtonClick(Sender: TObject);
    procedure DivButtonClick(Sender: TObject);
  private
    procedure GetReadings(Sender: TObject);
    procedure AdaptMenus(Sender: TObject);
  public
    input1, input2, output: TReading;
  end;

var
  MetronTesterMainForm: TMetronTesterMainForm;

implementation

{$R *.lfm}


{ TMetronTesterMainForm }

procedure TMetronTesterMainForm.GetReadings(Sender: TObject);
begin
  input1.text := InputEdit1.Text;
  input2.text := InputEdit2.Text;
end;

procedure TMetronTesterMainForm.AdaptMenus(Sender: TObject);
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF DARWIN}
  modifierKey := [ssMeta];
  {$ELSE}
  modifierKey := [ssCtrl];
  {$ENDIF}
  NewItem.ShortCut := ShortCut(VK_N, modifierKey);   // modifierKey might be
  OpenItem.ShortCut := ShortCut(VK_O, modifierKey);  // replaced by
  CloseItem.ShortCut := ShortCut(VK_W, modifierKey); // ssModifier
  QuitItem.ShortCut := ShortCut(VK_Q, modifierKey);
  UndoItem.ShortCut := ShortCut(VK_Z, modifierKey);
  CutItem.ShortCut := ShortCut(VK_X, modifierKey);
  CopyItem.ShortCut := ShortCut(VK_C, modifierKey);
  PasteItem.ShortCut := ShortCut(VK_V, modifierKey);
  SelectAllItem.ShortCut := ShortCut(VK_A, modifierKey);
end;

procedure TMetronTesterMainForm.PlusButtonClick(Sender: TObject);
begin
  GetReadings(Sender);
  ResultEdit.Text := (input1 + input2).Text;
end;

procedure TMetronTesterMainForm.FormCreate(Sender: TObject);
begin
  AdaptMenus(Sender);
end;

procedure TMetronTesterMainForm.QuitItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMetronTesterMainForm.TimesButtonClick(Sender: TObject);
begin
  GetReadings(Sender);
  ResultEdit.Text := (input1 * input2).Text;
end;

procedure TMetronTesterMainForm.MinusButtonClick(Sender: TObject);
begin
  GetReadings(Sender);
  ResultEdit.Text := (input1 - input2).Text;
end;

procedure TMetronTesterMainForm.DivButtonClick(Sender: TObject);
begin
  GetReadings(Sender);
  ResultEdit.Text := (input1 / input2).Text;
end;

end.

