unit GUI;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Test Function Generator }

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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs, ComCtrls, Menus, LCLType, StdCtrls, ExtCtrls, Spin, EditBtn, EDF;

type

  tTSType = (sine, square, saw, ecg);
  tTimeSeries = record
    time: array of real;
    values: array of real;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    AppleMenu: TMenuItem;
    FreqLabel: TLabel;
    AmpLabel: TLabel;
    DurLabel: TLabel;
    SecLabel: TLabel;
    Chart1: TChart;
    CloseMenuItem: TMenuItem;
    FreqSpinEdit: TFloatSpinEdit;
    AmpSpinEdit: TFloatSpinEdit;
    FunctionComboBox: TComboBox;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    AULabel: TLabel;
    ImageList1: TImageList;
    HzLabel: TLabel;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
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
    DurSpinEdit: TSpinEdit;
    ToolBar1: TToolBar;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    ySeries1: TLineSeries;
    procedure CloseMenuItemClick(Sender: TObject);
    procedure DurSpinEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AmpSpinEditChange(Sender: TObject);
    procedure FreqSpinEditChange(Sender: TObject);
    procedure FunctionComboBoxChange(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
  private

  public
    timeSeries: tTimeSeries;
    procedure DrawFunction;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

function GetTimeSeries(const theType: tTSType; const amplitude, frequency: double;
                    const duration: integer): tTimeSeries;
var
  i: integer;
begin
  case theType of
  sine:
    begin

    end;
  square:
    begin

    end;
  saw:
    begin

    end;
  ecg:
    begin

    end;
  end;
end;

{ TMainForm }

procedure TMainForm.SaveMenuItemClick(Sender: TObject);
begin

end;

procedure TMainForm.DrawFunction;
var
  theType: tTSType;
begin
  if FunctionComboBox.ItemIndex >= 0 then
  begin
    theType := tTSType(FunctionComboBox.ItemIndex);
    timeSeries := GetTimeSeries(theType, AmpSpinEdit.Value, FreqSpinEdit.Value, DurSpinEdit.Value);
  end;
end;

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

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AdaptMenus;
end;

procedure TMainForm.AmpSpinEditChange(Sender: TObject);
begin
  DrawFunction;
end;

procedure TMainForm.FreqSpinEditChange(Sender: TObject);
begin
  DrawFunction;
end;

procedure TMainForm.FunctionComboBoxChange(Sender: TObject);
begin
  DrawFunction;
end;

procedure TMainForm.CloseMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainForm.DurSpinEditChange(Sender: TObject);
begin
  DrawFunction;
end;

procedure TMainForm.MacAboutItemClick(Sender: TObject);
begin
  ShowMessage('Test Function Generator, a demo program for PUMA EDF Engine');
end;

procedure TMainForm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainForm.WinAboutItemClick(Sender: TObject);
begin
  MacAboutItemClick(Sender);
end;

end.

