unit Plot;

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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs, StdCtrls, EDF;

type

  { TPlotForm }

  TPlotForm = class(TForm)
    Chart1: TChart;
    ComboBox1: TComboBox;
    ySeries: TLineSeries;
    procedure ComboBox1Change(Sender: TObject);
  private

  public
    openFile: TEDFDoc;
    procedure DrawTimeSeries;
    procedure ShowPlot;
  end;

var
  PlotForm: TPlotForm;

implementation

{$R *.lfm}

{ TPlotForm }

procedure TPlotForm.ComboBox1Change(Sender: TObject);
begin
  DrawTimeSeries;
end;

procedure TPlotForm.DrawTimeSeries;
var
  scaledValue: single;
  imax, kmax: longint;
  m, k, i: longint;
  j: integer;
begin
  imax := high(openFile.ScaledDataRecord);        // Records
  kmax := high(openFile.ScaledDataRecord[0, 0]);  // Samples
  m := 1;
  j := ComboBox1.ItemIndex;
  ySeries.BeginUpdate;
  ySeries.Clear;
  for i := 0 to 10 {imax} do  // Records
  begin
    for k := 0 to kmax do  // Samples
    begin
      scaledValue := openFile.ScaledDataRecord[i, j, k];
      ySeries.AddXY(m, scaledValue);
      m := 2 + i * kmax + k;
    end;
    application.ProcessMessages;
  end;
  ySeries.EndUpdate;
end;

procedure TPlotForm.ShowPlot;
var
  j: integer;
  jmax: integer;
begin
  if assigned(openFile) then
  begin
    jmax := high(openFile.ScaledDataRecord[0]);     // Signals
    for j := 0 to jmax do
    begin
      ComboBox1.Items.Add(openFile.SignalLabel[j]);
    end;
    ComboBox1.ItemIndex := 0;
    DrawTimeSeries;
  end;
end;


end.

