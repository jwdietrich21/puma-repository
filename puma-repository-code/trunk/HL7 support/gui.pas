unit GUI;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Demo implementation: Viewer for HL7 messages }

{ Version 0.9 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ Parser and compiler for HL7 messages }

{ Source code released under the BSD License }
{ See http://puma-repository.sf.net for details }


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Buttons, HL7, MSH;

type

  { TMainForm }

  TMainForm = class(TForm)
    MessageFileSaveDialog: TSaveDialog;
    OpenButton: TButton;
    MessageFileOpenDialog: TOpenDialog;
    SaveButton: TButton;
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
    procedure OpenButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SegmentsListBoxClick(Sender: TObject);
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

procedure TMainForm.OpenButtonClick(Sender: TObject);
var
  theSegment: THL7Segment;
begin
  if MessageFileOpenDialog.Execute then
  begin
    ReadHL7File(gMessage, MessageFileOpenDialog.FileName);
    theSegment := gMessage.FirstSegment;
    while theSegment <> nil do
    begin
      SegmentsListBox.Items.Add(theSegment.contentString);
      theSegment := theSegment.nextSibling;
    end;
  end;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  if MessageFileSaveDialog.Execute then
  begin
    WriteHL7File(gMessage, MessageFileSaveDialog.FileName);
  end;
end;

procedure TMainForm.SegmentsListBoxClick(Sender: TObject);
var
  count, index: integer;
  theSegment: THL7Segment;
  theField: THL7Field;
begin
  FieldsListBox.Clear;
  ComponentsListBox.Clear;
  SubComponentsListBox.Clear;
  count := 0;
  index := SegmentsListBox.ItemIndex;
  theSegment := gMessage.FirstSegment;
  while (theSegment <> nil) and (count < index) do
  begin
    theSegment := theSegment.nextSibling;
    count := count + 1;
  end;
  theField := theSegment.FirstOccurrence.FirstField;
  while theField <> nil do
  begin
    FieldsListBox.Items.Add(theField.contentString);
    theField := theField.nextSibling;
  end;
end;


end.

