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
  Dialogs, ComCtrls, Menus, LCLType, StdCtrls, ExtCtrls, Spin, EditBtn,
  ButtonPanel, EDF;

type

  tTSType = (sine, square, saw, ecg);
  tTimeSeries = record
    time: array of real;
    values: array of real;
  end;
  tSignalRecord = record
    basicFunction: tTSType;
    amplitude: double;
    frequency: double;
    duration: integer;
    timeSeries: tTimeSeries;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    AppleMenu: TMenuItem;
    FreqLabel: TLabel;
    AmpLabel: TLabel;
    DurLabel: TLabel;
    HeaderControl1: THeaderControl;
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
    ySeries: TLineSeries;
    procedure CloseMenuItemClick(Sender: TObject);
    procedure GetParameters;
    procedure SetParameters;
    procedure DurSpinEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AmpSpinEditChange(Sender: TObject);
    procedure FreqSpinEditChange(Sender: TObject);
    procedure FunctionComboBoxChange(Sender: TObject);
    procedure HeaderControl1SectionClick(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure MacAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
  private

  public
    signalRecord: array[0..4] of tSignalRecord;
    signal: integer;
    procedure DrawFunction;
  end;

operator mod(const a, b: real) c: real; inline;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

operator mod(const a, b: real) c: real; inline;
{ implements modulo function for real values. Source: http://wiki.freepascal.org/Mod }
begin
  c:= a - b * Int(a / b);
end;

function GetTimeSeries(const theType: tTSType; const amplitude, frequency: double;
                    const duration: integer): tTimeSeries;
{ Fourier synthesis of ECG after H-U Harten, H. Naegerl and H-D Schulte
  Mathematik fuer Mediziner, VCH, Weinheim 1987, ISBN 3-527-15313-6 }
var
  i, imax: integer;
  omega, period: real;
begin
  imax := 1000; // number of points on x-axis
  SetLength(result.time, imax);
  SetLength(result.values, imax);
  case theType of
  sine:
    begin
      omega := 2 * pi * frequency;
      for i := 0 to imax - 1 do
      begin
        result.time[i] := i * duration / imax;
        result.values[i] := amplitude * sin(omega * i * duration / imax);
      end;
    end;
  square:
    begin
      period := 1 / frequency;
      for i := 0 to imax - 1 do
      begin
        result.time[i] := i * duration / imax;
        result.values[i] := amplitude * round(i * duration / imax mod period);
      end;
    end;
  saw:
    begin
      period := 1 / frequency;
      for i := 0 to imax - 1 do
      begin
        result.time[i] := i * duration / imax;
        result.values[i] := amplitude * (i * duration / imax mod period);
      end;
    end;
  ecg:
    begin
      omega := 2 * pi * frequency;
      for i := 0 to imax - 1 do
      begin
        result.time[i] := i * duration / imax;
        result.values[i] := amplitude *
                             (0.7001 * sin(omega * i * duration / imax - 2.76)
                            + 0.5278 * sin(2 * omega * i * duration / imax - 0.51)
                            + 0.6344 * sin(3 * omega * i * duration / imax + 2.04)
                            + 0.2399 * sin(4 * omega * i * duration / imax + 0.10)
                            + 0.8119 * sin(5 * omega * i * duration / imax - 1.09)
                            + 0.5678 * sin(6 * omega * i * duration / imax - 3.12)
                            + 0.6301 * sin(7 * omega * i * duration / imax + 1.01)
                            + 0.6975 * sin(8 * omega * i * duration / imax - 1.00)
                            + 0.6721 * sin(9 * omega * i * duration / imax - 2.94)
                            + 0.6541 * sin(10 * omega * i * duration / imax + 1.39)
                            + 0.5851 * sin(11 * omega * i * duration / imax - 0.65)
                            + 0.5626 * sin(12 * omega * i * duration / imax - 2.60)
                            + 0.5009 * sin(13 * omega * i * duration / imax + 1.76)
                            + 0.4144 * sin(14 * omega * i * duration / imax - 0.21)
                            + 0.3619 * sin(15 * omega * i * duration / imax - 2.14)
                            + 0.3100 * sin(16 * omega * i * duration / imax + 2.14)
                            + 0.2571 * sin(17 * omega * i * duration / imax + 0.28)
                            + 0.2032 * sin(18 * omega * i * duration / imax - 1.61)
                            + 0.1515 * sin(19 * omega * i * duration / imax + 2.72)
                            + 0.1258 * sin(20 * omega * i * duration / imax + 0.28)
                            + 0.0825 * sin(21 * omega * i * duration / imax - 1.11)
                            + 0.0671 * sin(22 * omega * i * duration / imax - 3.09)
                            + 0.0383 * sin(23 * omega * i * duration / imax + 1.16)
                            + 0.0200 * sin(24 * omega * i * duration / imax - 0.48)
                            + 0.0100 * sin(25 * omega * i * duration / imax - 0.54)
                            + 0.0200 * sin(26 * omega * i * duration / imax - 1.41)
                            + 0.0260 * sin(27 * omega * i * duration / imax + 2.75)
                            + 0.0298 * sin(28 * omega * i * duration / imax + 0.70)
                            + 0.0342 * sin(29 * omega * i * duration / imax - 1.40)
                            + 0.0296 * sin(30 * omega * i * duration / imax + 2.90)
                            + 0.0270 * sin(31 * omega * i * duration / imax + 0.68)
                            + 0.0328 * sin(32 * omega * i * duration / imax - 1.53)
                            + 0.0203 * sin(33 * omega * i * duration / imax + 2.29)
                            + 0.0211 * sin(34 * omega * i * duration / imax + 0.08)
                            + 0.0255 * sin(35 * omega * i * duration / imax - 2.28)
                            + 0.0317 * sin(36 * omega * i * duration / imax + 1.86)
                            + 0.0412 * sin(37 * omega * i * duration / imax - 0.17)
                            + 0.0411 * sin(38 * omega * i * duration / imax - 2.19)
                            + 0.0477 * sin(39 * omega * i * duration / imax + 2.10)
                            + 0.0526 * sin(40 * omega * i * duration / imax + 0.12)
                            + 0.0501 * sin(41 * omega * i * duration / imax - 1.79)
                            + 0.0531 * sin(42 * omega * i * duration / imax + 2.53)
                            + 0.0515 * sin(43 * omega * i * duration / imax + 0.59)
                            + 0.0500 * sin(44 * omega * i * duration / imax - 1.27)
                            + 0.0441 * sin(45 * omega * i * duration / imax + 3.04)
                            + 0.0416 * sin(46 * omega * i * duration / imax + 1.14)
                            + 0.0370 * sin(47 * omega * i * duration / imax - 0.73)
                            + 0.0297 * sin(48 * omega * i * duration / imax - 2.59)
                            + 0.0255 * sin(49 * omega * i * duration / imax + 1.90)
                            + 0.0211 * sin(50 * omega * i * duration / imax + 0.11));
      end;
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
  i: integer;
begin
  if FunctionComboBox.ItemIndex >= 0 then
  begin
    theType := SignalRecord[signal].basicFunction;
    SignalRecord[signal].timeSeries := GetTimeSeries(theType, SignalRecord[signal].amplitude, SignalRecord[signal].frequency, SignalRecord[signal].duration);
    ySeries.BeginUpdate;
    ySeries.Clear;
    for i := 0 to length(SignalRecord[signal].timeSeries.time) - 1 do
      ySeries.AddXY(SignalRecord[signal].timeSeries.time[i], SignalRecord[signal].timeSeries.values[i]);
    ySeries.EndUpdate;
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
var
  i: integer;
begin
  AdaptMenus;
  signal := 0;
  for i := 0 to 4 do
  begin
    SignalRecord[i].basicFunction := tTSType(FunctionComboBox.ItemIndex);
    SignalRecord[i].frequency := FreqSpinEdit.Value;
    SignalRecord[i].amplitude := AmpSpinEdit.Value;
    SignalRecord[i].duration := DurSpinEdit.Value;
  end;
end;

procedure TMainForm.AmpSpinEditChange(Sender: TObject);
begin
  GetParameters;
  DrawFunction;
end;

procedure TMainForm.FreqSpinEditChange(Sender: TObject);
begin
  GetParameters;
  DrawFunction;
end;

procedure TMainForm.FunctionComboBoxChange(Sender: TObject);
begin
  GetParameters;
  DrawFunction;
end;

procedure TMainForm.HeaderControl1SectionClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  Chart1.Title.Text.Text := Section.Text;
  signal := Section.Index;
  SetParameters;
  DrawFunction;
end;

procedure TMainForm.CloseMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainForm.GetParameters;
begin
  SignalRecord[signal].basicFunction := tTSType(FunctionComboBox.ItemIndex);
  SignalRecord[signal].frequency := FreqSpinEdit.Value;
  SignalRecord[signal].amplitude := AmpSpinEdit.Value;
  SignalRecord[signal].duration := DurSpinEdit.Value;
end;

procedure TMainForm.SetParameters;
begin
  FunctionComboBox.ItemIndex := longint(SignalRecord[signal].basicFunction);
  FreqSpinEdit.Value := SignalRecord[signal].frequency;
  AmpSpinEdit.Value := SignalRecord[signal].amplitude;
  DurSpinEdit.Value := SignalRecord[signal].duration;
end;

procedure TMainForm.DurSpinEditChange(Sender: TObject);
begin
  GetParameters;
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

