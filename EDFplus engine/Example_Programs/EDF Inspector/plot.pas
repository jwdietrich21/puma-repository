unit Plot;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ EDF Inspector }

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
  Dialogs, StdCtrls, ColorBox, Spin, EDF
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

type

  { TPlotForm }

  TPlotForm = class(TForm)
    Chart1: TChart;
    Chart2: TChart;
    ColorListBox1: TColorListBox;
    ColorListBox2: TColorListBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    SpinEdit1: TSpinEdit;
    ySeries1: TLineSeries;
    ySeries2: TLineSeries;
    procedure ColorListBox1Click(Sender: TObject);
    procedure ColorListBox2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private

  public
    openFile: TEDFDoc;
    procedure DrawTimeSeries;
    procedure ShowPlot;
  end;

const
  BACKCOLOUR = clDefault;

var
  PlotForm: TPlotForm;

function DarkTheme: boolean;

implementation

{$R *.lfm}

{ TPlotForm }

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

procedure TPlotForm.ComboBox1Change(Sender: TObject);
begin
  DrawTimeSeries;
end;

procedure TPlotForm.FormCreate(Sender: TObject);
begin
  FormPaint(Self);
end;

procedure TPlotForm.FormPaint(Sender: TObject);
begin
  if DarkTheme then
    begin
      Color := BACKCOLOUR;
      Chart1.Color := BACKCOLOUR;
      Chart1.BackColor := BACKCOLOUR;
      Chart2.Color := BACKCOLOUR;
      Chart2.BackColor := BACKCOLOUR;
    end
  else
    begin
      Color := clWhite;
      Chart1.Color := clWhite;
      Chart1.BackColor := clWhite;
      Chart2.Color := clWhite;
      Chart2.BackColor := clWhite;
    end;
end;

procedure TPlotForm.SpinEdit1Change(Sender: TObject);
begin
  DrawTimeSeries;
end;

procedure TPlotForm.ColorListBox1Click(Sender: TObject);
begin
  ySeries1.SeriesColor := ColorListBox1.Selected;
end;

procedure TPlotForm.ColorListBox2Click(Sender: TObject);
begin
  ySeries2.SeriesColor := ColorListBox2.Selected;
end;

procedure TPlotForm.DrawTimeSeries;
var
  scaledValue: single;
  kmax: longint;
  m, k, i: longint;
  j: integer;
begin
  if assigned(openFile) and (openFile.StatusCode = noErr) then
  begin
    m := 1;
    j := ComboBox1.ItemIndex;
    ySeries1.BeginUpdate;
    ySeries1.Clear;
    for i := 0 to SpinEdit1.Value do  // Records
    begin
      if i > high(openFile.ScaledDataRecord) then // data not available?
        kmax := 0
      else
        kmax := high(openFile.ScaledDataRecord[i, j]);  // Samples
      if kmax > 0 then
      for k := 0 to kmax do  // Samples
      begin
        scaledValue := openFile.ScaledDataRecord[i, j, k];
        ySeries1.AddXY(m, scaledValue);
        m := 2 + i * kmax + k;
      end;
      application.ProcessMessages;
    end;
    ySeries1.EndUpdate;
    m := 1;
    j := ComboBox2.ItemIndex;
    ySeries2.BeginUpdate;
    ySeries2.Clear;
    for i := 0 to SpinEdit1.Value do  // Records
    begin
      if i > high(openFile.ScaledDataRecord) then // data not available?
        kmax := 0
      else
        kmax := high(openFile.ScaledDataRecord[i, j]);  // Samples
     if kmax > 0 then
     for k := 0 to kmax do  // Samples
      begin
        scaledValue := openFile.ScaledDataRecord[i, j, k];
        ySeries2.AddXY(m, scaledValue);
        m := 2 + i * kmax + k;
      end;
      application.ProcessMessages;
    end;
    ySeries2.EndUpdate;
  end;
end;

procedure TPlotForm.ShowPlot;
var
  j: integer;
  imax: longint;
  jmax: integer;
begin
  if assigned(openFile) then
  begin
    imax := high(openFile.ScaledDataRecord);        // Records
    jmax := high(openFile.ScaledDataRecord[0]);     // Signals
    ComboBox1.Items.Clear;
    ComboBox2.Items.Clear;
    SpinEdit1.MaxValue := imax;
    SpinEdit1.Value := 0;
    for j := 0 to jmax do
    begin
      ComboBox1.Items.Add(openFile.SignalLabel[j]);
      ComboBox2.Items.Add(openFile.SignalLabel[j]);
    end;
    ComboBox1.ItemIndex := 0;
    ComboBox2.ItemIndex := 1;
    DrawTimeSeries;
  end;
end;


end.

