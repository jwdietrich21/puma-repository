unit GUI;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ EDF Inspector }

{ Version 1.0 (Alpha Centauri) }

{ (c) Johannes W. Dietrich, 1994 - 2018 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2018 }

{ Parser and compiler for EDF and EDF+ data files }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  ComCtrls, Menus, LCLType, StdCtrls, ExtCtrls, LazUTF8, uPSComponent, EDF, Grids;

const
  FEEDBACK_TEXT = '  Status Code: ';

type

  { TMainForm }

  TMainForm = class(TForm)
    AppleMenu: TMenuItem;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    HeaderRecordValueListEditor: TValueListEditor;
    HelpMenu: TMenuItem;
    ImageList1: TImageList;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    EDFFileOpenDialog: TOpenDialog;
    CellContentMemo: TMemo;
    NewButton: TToolButton;
    NewMenuItem: TMenuItem;
    OpenButton: TToolButton;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    SaveAsButton: TToolButton;
    SaveButton: TToolButton;
    SaveMenuItem: TMenuItem;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    procedure CloseMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HeaderRecordValueListEditorSelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
    procedure MacAboutItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;
  gEDFFile: TEDFDoc;

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

procedure TMainForm.MacAboutItemClick(Sender: TObject);
begin
  ShowMessage('EDF Inspector, a demo program for PUMA EDF Engine');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AdaptMenus;
end;

procedure TMainForm.HeaderRecordValueListEditorSelectCell(Sender: TObject;
  aCol, aRow: Integer; var CanSelect: Boolean);
var
  contentString: AnsiString;
begin
  contentString := HeaderRecordValueListEditor.Cells[1, aRow];
  CellContentMemo.Lines.Clear;
  CellContentMemo.Lines.AddText(contentString);
end;

procedure TMainForm.CloseMenuItemClick(Sender: TObject);
begin
  QuitMenuItemClick(Sender);
end;

procedure TMainForm.WinAboutItemClick(Sender: TObject);
begin
  MacAboutItemClick(Sender);
end;

procedure TMainForm.OpenMenuItemClick(Sender: TObject);
var
  i, ns: integer;
  Labels, Transducers, PhysDims, PhysMins, PhysMaxs: AnsiString;
  DigMins, DigMaxs, Prefilters, NumsOfSamples, Reserved2s: AnsiString;
  // fStream: TFileStream; // Alternative stream form for testing purposes
begin
  if EDFFileOpenDialog.Execute then
  begin
    HeaderRecordValueListEditor.Strings.Clear;
    if not assigned(gEDFFile) then
      gEDFFile := TEDFDoc.Create;
    // fStream := TFileStream.Create(UTF8ToSys(EDFFileOpenDialog.FileName), fmOpenRead); // testing
    gEDFFile.ReadFromFile(UTF8ToSys(EDFFileOpenDialog.FileName));
    // gEDFFile.ReadFromStream(fStream); // for testing of stream variant
    MainForm.Caption := 'EDF Inspector: ' +
      ExtractFileName(EDFFileOpenDialog.FileName);
    HeaderRecordValueListEditor.Row := 0;
    HeaderRecordValueListEditor.InsertRow('Version', gEDFFile.version, true);
    HeaderRecordValueListEditor.InsertRow('Local Patient ID', gEDFFile.LocalPatID, true);
    HeaderRecordValueListEditor.InsertRow('Local Recording ID', gEDFFile.LocalRecID, true);
    HeaderRecordValueListEditor.InsertRow('Start Date', gEDFFile.StartDate, true);
    HeaderRecordValueListEditor.InsertRow('Start Time', gEDFFile.StartTime, true);
    HeaderRecordValueListEditor.InsertRow('Number of Bytes', gEDFFile.NumOfBytes, true);
    HeaderRecordValueListEditor.InsertRow('Reserved', gEDFFile.Reserved, true);
    HeaderRecordValueListEditor.InsertRow('Number of Records', gEDFFile.NumOfDataRecs, true);
    HeaderRecordValueListEditor.InsertRow('Duration of a Record', gEDFFile.DurationOfData, true);
    HeaderRecordValueListEditor.InsertRow('Number of Signals', gEDFFile.NumOfSignals, true);
    Labels := gEDFFile.SignalLabel[0];
    Transducers := gEDFFile.Transducer[0];
    PhysDims := gEDFFile.PhysDim[0];
    PhysMins := gEDFFile.PhysMin[0];
    PhysMaxs := gEDFFile.PhysMax[0];
    DigMins := gEDFFile.DigMin[0];
    DigMaxs := gEDFFile.DigMax[0];
    Prefilters := gEDFFile.Prefilter[0];
    NumsOfSamples := gEDFFile.NumOfSamples[0];
    Reserved2s := GEDFFile.Reserved2[0];
    if TryStrToInt(TrimRight(gEDFFile.NumOfSignals), ns) and (ns > 0) then
    for i := 1 to ns do
      begin
        Labels := Labels + ' | ' + gEDFFile.SignalLabel[i];
        Transducers := Transducers + ' | ' + gEDFFile.Transducer[i];
        PhysDims := PhysDims + ' | ' + gEDFFile.PhysDim[i];
        PhysMins := PhysMins + ' | ' + gEDFFile.PhysMin[i];
        PhysMaxs := PhysMaxs + ' | ' + gEDFFile.PhysMax[i];
        DigMins := DigMins + ' | ' + gEDFFile.DigMin[i];
        DigMaxs := DigMaxs + ' | ' + gEDFFile.DigMax[i];
        Prefilters := Prefilters + ' | ' + gEDFFile.Prefilter[i];
        NumsOfSamples := NumsOfSamples + ' | ' + gEDFFile.NumOfSamples[i];
        Reserved2s := Reserved2s + ' | ' + gEDFFile.Reserved2[i];
      end;
    HeaderRecordValueListEditor.InsertRow('Labels', Labels, true);
    HeaderRecordValueListEditor.InsertRow('Transducers', Transducers, true);
    HeaderRecordValueListEditor.InsertRow('Physical Dimension', PhysDims, true);
    HeaderRecordValueListEditor.InsertRow('Physical Minimum', PhysMins, true);
    HeaderRecordValueListEditor.InsertRow('Physical Maximum', PhysMaxs, true);
    HeaderRecordValueListEditor.InsertRow('Digital Minimum', DigMins, true);
    HeaderRecordValueListEditor.InsertRow('Digital Maximum', DigMaxs, true);
    HeaderRecordValueListEditor.InsertRow('Prefiltering', Prefilters, true);
    HeaderRecordValueListEditor.InsertRow('Number of Samples', NumsOfSamples, true);
    HeaderRecordValueListEditor.InsertRow('Reserved', '', true);
    Statusbar1.Panels[0].Text := FEEDBACK_TEXT + IntToStr(gEDFFile.StatusCode);
    // fStream.Free; // for testing of stream functionality
  end;
end;

procedure TMainForm.QuitMenuItemClick(Sender: TObject);
begin
  if assigned(gEDFFile) then
    gEDFFile.Free;
  application.Terminate;
end;

end.

