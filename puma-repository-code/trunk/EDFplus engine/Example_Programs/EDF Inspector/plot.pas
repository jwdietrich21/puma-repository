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
  private

  public
    openFile: TEDFDoc;
    procedure ShowPlot;
  end;

var
  PlotForm: TPlotForm;

implementation

{$R *.lfm}

{ TPlotForm }

procedure TPlotForm.ShowPlot;
var
  i, k, m: longint;
  imax, kmax, mmax: longint;
  j: integer;
  jmax: integer;
  scaledValue: single;
begin
  if assigned(openFile) then
  begin
    imax := high(openFile.ScaledDataRecord);        // Records
    jmax := high(openFile.ScaledDataRecord[0]);     // Signals
    kmax := high(openFile.ScaledDataRecord[0, 0]);  // Samples
    mmax := (imax + 1) * (kmax + 1);
    for j := 0 to jmax do
    begin
      ComboBox1.Items.Add(openFile.SignalLabel[j]);
    end;
    ComboBox1.ItemIndex := 0;
    m := 1;
    ySeries.BeginUpdate;
    for i := 0 to 10 {imax} do  // Records
    begin
      for j := 0 to jmax do  // Signals
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
end;

end.

