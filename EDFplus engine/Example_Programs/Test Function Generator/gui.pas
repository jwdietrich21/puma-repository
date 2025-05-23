unit GUI;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Test Function Generator }

{ Version 1.2.0 (Hyperborea) }

{ (c) Johannes W. Dietrich, 1994 - 2020 }
{ (c) Oivind Toien, 2020 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2020 }

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
{$IFDEF LCLCocoa}
  {$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs, ComCtrls, Menus, LCLType, LazUTF8, StdCtrls, ExtCtrls, Spin,
  EditBtn, Math, EDF
  {$IFDEF WINDOWS}
  , Windows, Win32Proc, registry
  {$ENDIF}
  {$IFDEF DARWIN}
  , MacOSAll
  {$ENDIF}
  {$IFDEF LCLCocoa}
  , CocoaAll, CocoaUtils
  {$ENDIF}
  ;

const
  ABOUT_MESSAGE = 'Test Function Generator 1.2.0 (Hyperborea), a demo program for PUMA EDF Engine';
  BACKCOLOUR = clDefault;

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
    Indicator2: TShape;
    Indicator3: TShape;
    Indicator4: TShape;
    Indicator5: TShape;
    SaveDialog1: TSaveDialog;
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
    Indicator1: TShape;
    ToolBar1: TToolBar;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    ySeries: TLineSeries;
    procedure CloseMenuItemClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure GetParameters;
    procedure SaveButtonClick(Sender: TObject);
    procedure SetParameters;
    function EDFDoc: TEDFDoc;
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

{$IFDEF FPC}
{$IFDEF VER3}
operator mod(const a, b: real) c: real; inline;
{$ELSE}
function fmod(const a, b: real): real; inline;
{$ENDIF}
{$ENDIF}

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{$IFDEF FPC}
{$IFDEF VER3}
operator mod(const a, b: real) c: real; inline;
{ implements modulo function for real values. Source: http://wiki.freepascal.org/Mod }
begin
  c := a - b * Int(a / b);
end;
{$ELSE}
function fmod(const a, b: real): real; inline;
{ modulo operator in form of a traditional function for old FPC versions }
begin
  result := a - b * trunc(a / b);
end;
{$ENDIF}
{$ENDIF}

function IsMinMacOS(Maj, Min: integer): boolean;
  { returns true, if this app runs on a macOS version as specified or newer }
  {$IFDEF DARWIN}
var
  Major, Minor: SInt32;
  theError: SInt16;
  {$ENDIF}
begin
  result := false;
  {$IFDEF DARWIN}
  theError := Gestalt(gestaltSystemVersionMajor, Major);
  if theError = 0 then
    theError := Gestalt(gestaltSystemVersionMinor, Minor);
  if theError = 0 then
    if (Major = Maj) and (Minor >= Min) or (Major > Maj) then
      Result := True;
  {$ENDIF}
end;

function MojaveOrNewer: boolean;
  { returns true, if this app runs on macOS X 10.14 Mojave or newer }
begin
  result := false;
  {$IFDEF DARWIN}
  result := IsMinMacOS(10, 14);
  {$ENDIF}
end;

{$IFDEF LCLCocoa}
{The following two functions were suggested by Hansaplast at https://forum.lazarus.freepascal.org/index.php/topic,43111.msg304366.html}

// Retrieve key's string value from user preferences. Result is encoded using NSStrToStr's default encoding.
function GetPrefString(const KeyName : string) : string;
begin
  Result := NSStringToString(NSUserDefaults.standardUserDefaults.stringForKey(NSStr(@KeyName[1])));
end;
{$ENDIF}

// DarkTheme: Detects if the Dark Theme (true) has been enabled or not (false)
function DarkTheme: boolean;
{$IFDEF Windows}
const
  KEYPATH = '\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  KEYNAME = 'AppsUseLightTheme';
  WindowsDarkModeSupported: boolean = false; // may be set to true in future versions
var
  LightKey: boolean;
  Registry: TRegistry;
{$ENDIF}
begin
  Result := false;
  {$IFDEF Windows}
  if WindowsDarkModeSupported then
  begin
    Registry := TRegistry.Create;
    try
      Registry.RootKey := HKEY_CURRENT_USER;
      if Registry.OpenKeyReadOnly(KEYPATH) then
        begin
          if Registry.ValueExists(KEYNAME) then
            LightKey := Registry.ReadBool(KEYNAME)
          else
            LightKey := true;
        end
      else
        LightKey := true;
        Result := not LightKey
    finally
      Registry.Free;
    end;
  end
  else
  Result := false;
  {$ELSE}
  {$IFDEF LCLCocoa}
  if MojaveOrNewer then
    Result := pos('DARK',UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0
  else
    Result := false;
  {$ELSE}
  Result := false;
  {$ENDIF}
  {$ENDIF}
end;

procedure CreateLargeFile(var EDFDoc: TEDFDoc; const aFileName: ansistring);
var
  i, k:     longint;   // record and sample index
  imax:     longint;   // number of records
  j:        integer;   // signal index
  jmax:     integer;   // number of signals
  rawValue: smallint;
  fStream:  TFileStream;
begin
  if assigned(EDFDoc) then
    begin
      fStream := TFileStream.Create(aFileName, fmCreate);
      { Write Header Record: }
      WriteHeaderRecord(EDFDoc, fStream);
      { The next part simulates creating a large data record. }
      { In this example it isn't acually that large, but similar code can }
      { be used for really large data chunks. }
      imax := EDFDoc.iNumOfDataRecs;
      jmax := EDFDoc.iNumOfSignals;
      for i := 0 to imax - 1 do
        for j := 0 to jmax - 1 do
          for k := 0 to EDFDoc.iNumOfSamples[j] - 1 do
          begin
            rawValue := EDFDoc.RawDataRecord[i, j, k];
            fStream.Write(NToLE(rawValue), 2);
          end;
      fStream.Free;
    end;
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
  sine:        // sine wave
    begin
      omega := 2 * pi * frequency;
      for i := 0 to imax - 1 do
      begin
        result.time[i] := i * duration / imax;
        result.values[i] := amplitude * sin(omega * i * duration / imax);
      end;
    end;
  square:      // square wave
    begin
      period := 1 / frequency;
      for i := 0 to imax - 1 do
      begin
        result.time[i] := i * duration / imax;
        {$IFDEF FPC}
        {$IFDEF VER3}
        result.values[i] := amplitude * round(i * duration / imax mod period);
        {$ELSE}
        result.values[i] := amplitude * round(fmod(i * duration / imax, period));
        {$ENDIF}
        {$ENDIF}
      end;
    end;
  saw:         // ramp
    begin
      period := 1 / frequency;
      for i := 0 to imax - 1 do
      begin
        result.time[i] := i * duration / imax;
        {$IFDEF FPC}
        {$IFDEF VER3}
        result.values[i] := amplitude * (i * duration / imax mod period);
        {$ELSE}
        result.values[i] := amplitude * (fmod(i * duration / imax, period));
        {$ENDIF}
        {$ENDIF}
      end;
    end;
  ecg:         // simulated ECG
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
var
  theDoc: TEDFDoc;
begin
  theDoc := EDFDoc;
  if SaveDialog1.Execute then
  begin
    case SaveDialog1.FilterIndex of
    { Standard variant using a self-contained EDFDoc object : }
    1: WriteEDFFile(theDoc, UTF8ToSys(SaveDialog1.FileName));
    { Alternative approach for very large files : }
    2: CreateLargeFile(theDoc, UTF8ToSys(SaveDialog1.FileName));
    end;
  end;
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
  end
  else
    ySeries.Clear;
end;

procedure AdaptMenus;
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF Darwin}
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
  FormPaint(Sender);
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
  if HeaderControl = HeaderControl1 then
    begin
    Chart1.Title.Text.Text := Section.Text;
    signal := Section.Index;
    SetParameters;
    DrawFunction;
    case Section.Index of
    1: Indicator2.Visible := true;
    2: Indicator3.Visible := true;
    3: Indicator4.Visible := true;
    4: Indicator5.Visible := true;
    end;
  end;
end;

procedure TMainForm.CloseMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  if DarkTheme then
    begin
      Color := BACKCOLOUR;
      Toolbar1.Color := BACKCOLOUR;
      Chart1.Color := BACKCOLOUR;
      Chart1.BackColor := BACKCOLOUR;
      Chart1.Legend.BackgroundBrush.Color := BACKCOLOUR;
    end
  else
    begin
      Color := clWhite;
      Toolbar1.Color := clWhite;
      Chart1.Color := clWhite;
      Chart1.BackColor := clWhite;
      Chart1.Legend.BackgroundBrush.Color := clWhite;
    end;
end;

procedure TMainForm.GetParameters;
begin
  SignalRecord[signal].basicFunction := tTSType(FunctionComboBox.ItemIndex);
  SignalRecord[signal].frequency := FreqSpinEdit.Value;
  SignalRecord[signal].amplitude := AmpSpinEdit.Value;
  SignalRecord[signal].duration := DurSpinEdit.Value;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  SaveMenuItemClick(Sender);
end;

procedure TMainForm.SetParameters;
begin
  FunctionComboBox.ItemIndex := longint(SignalRecord[signal].basicFunction);
  FreqSpinEdit.Value := SignalRecord[signal].frequency;
  AmpSpinEdit.Value := SignalRecord[signal].amplitude;
  DurSpinEdit.Value := SignalRecord[signal].duration;
end;

function TMainForm.EDFDoc: TEDFDoc;
{ Creates a rather simple EDF document for testing and demonstration purposes }
var
  i, k, m: longint;
  imax, kmax, Samples, signalIndex: longint;
  j: integer;
  jmax: integer;
  maxDur, RecordBytes: integer;
  oldFormatSettings: TFormatSettings;
begin
  oldFormatSettings := DefaultFormatSettings;
  DefaultFormatSettings.ShortMonthNames := kShortEnglishMonths;
  RecordBytes := 0;
  Samples := 0;
  maxDur := 0;
  result := TEDFDoc.Create;
  result.LocalPatID := '01234567 M 12-MAY-1904 John Doe';
  result.LocalRecID := 'Startdate ' + UpperCase(FormatDateTime('dd-mmm-yyyy', now))
                       + ' 98765 Dr. Frankenstayn EDF_Engine';
  result.dStartDate := now;
  result.dStartTime := now;
  result.iNumOfSignals := length(signalRecord);
  for j := 0 to result.iNumOfSignals - 1 do
  begin
    if signalRecord[j].duration > maxDur then
      maxDur := signalRecord[j].duration;
    RecordBytes := RecordBytes + length(signalRecord[j].timeSeries.values) * 2;
    signalIndex := longint(signalRecord[j].basicFunction);
    if signalIndex >= 0 then
    begin
      result.SignalLabel[j] := FunctionComboBox.Items[signalIndex];
      result.Transducer[j] := 'Simulated time series';
      result.PhysDim[j] := 'AU';
      if length(signalRecord[j].timeSeries.values) > 0 then
      begin
        result.PhysMin[j] := MinValue(signalRecord[j].timeSeries.values);
        result.PhysMax[j] := MaxValue(signalRecord[j].timeSeries.values);
        result.digMin[j] := round(result.ePhysMin[j] * 100);
        result.digMax[j] := round(result.ePhysMax[j] * 100);
      end
      else
      begin
        result.PhysMin[j] := -1;
        result.PhysMax[j] := 0;
        result.digMin[j] := -1;
        result.digMax[j] := 0;
      end;
      result.Prefilter[j] := 'None';
      result.iNumOfSamples[j] := length(signalRecord[j].timeSeries.time);
      if result.iNumOfSamples[j] > Samples then
        Samples := result.iNumOfSamples[j];
        { TODO -oJWD : Sample count should be signal-specific }
    end
    else
    begin
      result.PhysMin[j] := -1;
      result.PhysMax[j] := 0;
      result.digMin[j] := -1;
      result.digMax[j] := 0;
      result.iNumOfSamples[j] := 0;
    end;
  end;
  imax := 1 + RecordBytes div kMaxRecordBytes; // Number of records
  result.iNumOfDataRecs := imax;
  result.iDurationOfData := maxDur;
  jmax := result.iNumOfSignals;                // Number of signals
  kmax := Samples div imax;                    // Number of samples per record
  for i := 0 to imax - 1 do
  for j := 0 to jmax - 1 do
  for k := 0 to kmax - 1 do
  begin
    m := i * kmax + k;
    if result.iNumOfSamples[j] > 0 then
    begin
      result.ScaledDataRecord[i, j, k] := signalRecord[j].timeSeries.values[m];
      result.RawDataRecord[i, j, k] := result.Unscaled[i, j, k];
    end;
  end;
  DefaultFormatSettings := oldFormatSettings;
end;


procedure TMainForm.DurSpinEditChange(Sender: TObject);
begin
  GetParameters;
  DrawFunction;
end;

procedure TMainForm.MacAboutItemClick(Sender: TObject);
begin
  ShowMessage(ABOUT_MESSAGE);
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

