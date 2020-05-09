unit GUI;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Demo implementation: Viewer for HL7 messages }

{ Version 1.1.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ Viewer and browser for HL7 messages }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, LCLType, ExtCtrls, Buttons, PairSplitter, Menus, HL7, MSH, types;

const
  FEEDBACK_TEXT = '  Status Code: ';

type

  { TMainForm }

  TMainForm = class(TForm)
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    EditMenu: TMenuItem;
    Divider11: TMenuItem;
    HelpMenu: TMenuItem;
    AppleMenu: TMenuItem;
    MacAboutItem: TMenuItem;
    CloseMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    Divider21: TMenuItem;
    CutMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    Divider12: TMenuItem;
    QuitMenuItem: TMenuItem;
    NewMenuItem: TMenuItem;
    NewButton: TToolButton;
    OpenButton: TToolButton;
    SaveButton: TToolButton;
    SaveAsButton: TToolButton;
    UndoMenuItem: TMenuItem;
    MessageGroupBox: TGroupBox;
    MessageMemo: TMemo;
    MessageFileSaveDialog: TSaveDialog;
    MessageFileOpenDialog: TOpenDialog;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    ScrollBox1: TScrollBox;
    SegmentsGroupBox: TGroupBox;
    FieldsGroupBox: TGroupBox;
    ComponentsGroupBox: TGroupBox;
    SegmentsListBox: TListBox;
    FieldsListBox: TListBox;
    ComponentsListBox: TListBox;
    SubComponentsListBox: TListBox;
    SubComponentsGroupBox: TGroupBox;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    procedure ShowAboutWindow(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure ComponentsListBoxClick(Sender: TObject);
    procedure FieldsListBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SegmentsListBoxClick(Sender: TObject);
    procedure SegmentsListBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ToolBar1Click(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  gMessage: THL7Message;

implementation

{$R *.lfm}

{ TMainForm }

procedure AdaptMenus;
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  MainForm.WinAboutItem.Visible := False;
  MainForm.AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  MainForm.WinAboutItem.Visible := True;
  MainForm.AppleMenu.Visible := False;
  {$ENDIF}
  MainForm.NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  MainForm.OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  MainForm.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  MainForm.SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  MainForm.QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  MainForm.UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  MainForm.RedoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey + [ssShift]);
  MainForm.CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  MainForm.CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  MainForm.PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
end;

procedure TMainForm.OpenMenuItemClick(Sender: TObject);
var
  theSegment: THL7Segment;
begin
  if MessageFileOpenDialog.Execute then
  begin
    SegmentsListBox.Clear;
    FieldsListBox.Clear;
    ComponentsListBox.Clear;
    SubComponentsListBox.Clear;
    ReadHL7File(gMessage, MessageFileOpenDialog.FileName);
    MainForm.Caption := 'HL7 Message Viewer: ' +
      ExtractFileName(MessageFileOpenDialog.FileName);
    MessageMemo.Lines.Clear;
    MessageMemo.Lines.Add(AdjustLineBreaks(gMessage.contentString));
    theSegment := gMessage.FirstSegment;
    while theSegment <> nil do
    begin
      SegmentsListBox.Items.Add(theSegment.contentString);
      theSegment := theSegment.nextSibling;
    end;
    Statusbar1.Panels[0].Text := FEEDBACK_TEXT + IntToStr(gMessage.StatusCode);
  end;
end;

procedure TMainForm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainForm.SegmentsListBoxDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  listColor: TColor;
begin
  with (Control as TListBox).Canvas do
  begin
    if (odSelected in State) then
      listColor := clHighLight
    else if odd(index) then
      listColor := clWhite
    else
      listColor := clMoneyGreen;
    Brush.Style := bsSolid;
    Brush.Color := listColor;
    Pen.Style := psClear;
    FillRect(aRect);
    Brush.Style := bsClear;
    TextOut(aRect.Left, aRect.Top, (Control as TListBox).Items[index]);
  end;
end;

procedure TMainForm.ToolBar1Click(Sender: TObject);
begin

end;

procedure TMainForm.WinAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TMainForm.SegmentsListBoxClick(Sender: TObject);
var
  Count, index, n: integer;
  theSegment: THL7Segment;
  theField: THL7Field;
begin
  if gMessage <> nil then
  begin
    FieldsListBox.Clear;
    ComponentsListBox.Clear;
    SubComponentsListBox.Clear;
    Count := 0;
    index := SegmentsListBox.ItemIndex;
    theSegment := gMessage.FirstSegment;
    while (theSegment <> nil) and (Count < index) do
    begin
      theSegment := theSegment.nextSibling;
      Count := Count + 1;
    end;
    n := 1;
    theField := theSegment.FirstOccurrence.FirstField;
    while theField <> nil do
    begin
      if (theSegment.segmentType = 'MSH') and (n = 2) then
        FieldsListBox.Items.Add(gMessage.CompiledDelimiters(gMessage.Delimiters))
      else
        FieldsListBox.Items.Add(theField.contentString);
      n := n + 1;
      theField := theField.nextSibling;
    end;
  end;
end;

procedure TMainForm.FieldsListBoxClick(Sender: TObject);
var
  Count, index: integer;
  theSegment: THL7Segment;
  theField: THL7Field;
  theComponent: THL7Component;
begin
  if gMessage <> nil then
  begin
    ComponentsListBox.Clear;
    SubComponentsListBox.Clear;
    Count := 0;
    index := SegmentsListBox.ItemIndex;
    theSegment := gMessage.FirstSegment;
    while (theSegment <> nil) and (Count < index) do
    begin
      theSegment := theSegment.nextSibling;
      Count := Count + 1;
    end;
    Count := 0;
    index := FieldsListBox.ItemIndex;
    theField := theSegment.FirstOccurrence.FirstField;
    while (theField <> nil) and (Count < index) do
    begin
      theField := theField.nextSibling;
      Count := Count + 1;
    end;
    theComponent := theField.FirstComponent;
    while theComponent <> nil do
    begin
      ComponentsListBox.Items.Add(theComponent.contentString);
      theComponent := theComponent.nextSibling;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AdaptMenus;
end;

procedure TMainForm.ComponentsListBoxClick(Sender: TObject);
var
  Count, index: integer;
  theSegment: THL7Segment;
  theField: THL7Field;
  theComponent: THL7Component;
  theSubComponent: THL7SubComponent;
begin
  if gMessage <> nil then
  begin
    SubComponentsListBox.Clear;
    Count := 0;
    index := SegmentsListBox.ItemIndex;
    theSegment := gMessage.FirstSegment;
    while (theSegment <> nil) and (Count < index) do
    begin
      theSegment := theSegment.nextSibling;
      Count := Count + 1;
    end;
    Count := 0;
    index := FieldsListBox.ItemIndex;
    theField := theSegment.FirstOccurrence.FirstField;
    while (theField <> nil) and (Count < index) do
    begin
      theField := theField.nextSibling;
      Count := Count + 1;
    end;
    Count := 0;
    index := ComponentsListBox.ItemIndex;
    theComponent := theField.FirstComponent;
    while (theComponent <> nil) and (Count < index) do
    begin
      theComponent := theComponent.nextSibling;
      Count := Count + 1;
    end;
    if theComponent <> nil then
      theSubComponent := theComponent.FirstSubComponent
    else
      theSubComponent := nil;
    while theSubComponent <> nil do
    begin
      SubComponentsListBox.Items.Add(theSubComponent.contentString);
      theSubComponent := theSubComponent.nextSibling;
    end;
    Statusbar1.Panels[0].Text := FEEDBACK_TEXT + IntToStr(gMessage.StatusCode);
  end
  else
    Statusbar1.Panels[0].Text := FEEDBACK_TEXT + '--';
end;

procedure TMainForm.ShowAboutWindow(Sender: TObject);
begin
  ShowMessage('HL7 Message Viewer, a demo program for PUMA HL7 Engine');
end;

procedure TMainForm.CloseMenuItemClick(Sender: TObject);
begin
  QuitMenuItemClick(Sender);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  //SetTextLineEnding(MessageMemo.Lines, tlbsCR);
end;

procedure TMainForm.MacAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TMainForm.SaveMenuItemClick(Sender: TObject);
begin
  if MessageFileSaveDialog.Execute then
  begin
    WriteHL7File(gMessage, MessageFileSaveDialog.FileName);
  end;
end;

end.
