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
  Dialogs, StdCtrls, ColorBox, Spin, EDF;

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
    procedure SpinEdit1Change(Sender: TObject);
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
  if openFile.StatusCode = noErr then
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

