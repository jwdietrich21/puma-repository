unit DataRecordGrid;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ EDF Inspector }

{ Version 1.0 (Aquila) }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ComCtrls, EDF;

type

  { TValuesGridForm }

  TValuesGridForm = class(TForm)
    RawDataGrid: TStringGrid;
    ScaledDataGrid: TStringGrid;
    PageControl1: TPageControl;
    ProgressBar1: TProgressBar;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure PageControl1Change(Sender: TObject);
  private

  public
    ScaledDataReady: boolean;
    openFile: TEDFDoc;
    procedure ShowDataRecord;
  end;

var
  ValuesGridForm: TValuesGridForm;

implementation

{$R *.lfm}

{ TValuesGridForm }

procedure TValuesGridForm.PageControl1Change(Sender: TObject);
var
  i, k, m: longint;
  imax, kmax, mmax: longint;
  j: integer;
  jmax: integer;
  scaledValue: single;
  c: TGridColumn;
begin
  if (PageControl1.TabIndex = 1) and not ScaledDataReady then
  begin
    if assigned(openFile) then
    begin
      ProgressBar1.Position := 0;
      ScaledDataGrid.BeginUpdate;
      ScaledDataGrid.Cells[0, 0] := 'n';
      ScaledDataGrid.Columns.Clear;
      imax := high(openFile.ScaledDataRecord);        // Records
      jmax := high(openFile.ScaledDataRecord[0]);     // Signals
      kmax := high(openFile.ScaledDataRecord[0, 0]);  // Samples
      mmax := (imax + 1) * (kmax + 1);
      ScaledDataGrid.RowCount := mmax;
      for j := 0 to jmax do
      begin
        c := ScaledDataGrid.Columns.Add;
        c.Title.Caption := openFile.SignalLabel[j];
      end;
      for m := 1 to mmax - 1 do
      ScaledDataGrid.Cells[0, m] := IntToStr(m);
      ScaledDataGrid.EndUpdate;
      application.ProcessMessages;
      m := 1;
      ScaledDataGrid.BeginUpdate;
      for i := 0 to imax do    // Records
      begin
        for j := 0 to jmax do  // Signals
        for k := 0 to kmax do  // Samples
        begin
          scaledValue := openFile.ScaledDataRecord[i, j, k];
          ScaledDataGrid.Cells[j + 1, m] := FloatToStrF(scaledValue, ffGeneral, 3, 2);
          m := i * kmax + k;
        end;
        if imax > 0 then
          ProgressBar1.Position := trunc(i / imax * 100);
        application.ProcessMessages;
      end;
      ScaledDataGrid.EndUpdate;
      ScaledDataGrid.TopRow := 1;
      ProgressBar1.Position := 0;
      ValuesGridForm.ScaledDataReady := true;
    end;
  end;
end;

procedure TValuesGridForm.ShowDataRecord;
var
  i, k, m: longint;
  imax, kmax, mmax: longint;
  j: integer;
  jmax: integer;
  rawValue: SmallInt;
  c: TGridColumn;
begin
  if assigned(openFile) then
  begin
    RawDataGrid.BeginUpdate;
    RawDataGrid.Cells[0, 0] := 'n';
    RawDataGrid.Columns.Clear;
    imax := high(openFile.RawDataRecord);        // Records
    jmax := high(openFile.RawDataRecord[0]);     // Signals
    kmax := high(openFile.RawDataRecord[0, 0]);  // Samples
    mmax := (imax + 1) * (kmax + 1);
    RawDataGrid.RowCount := mmax;
    for j := 0 to jmax do
    begin
      c := RawDataGrid.Columns.Add;
      c.Title.Caption := openFile.SignalLabel[j];
    end;
    for m := 1 to mmax - 1 do
    RawDataGrid.Cells[0, m] := IntToStr(m);
    RawDataGrid.EndUpdate;
    application.ProcessMessages;
    m := 1;
    RawDataGrid.BeginUpdate;
    for i := 0 to imax do    // Records
    begin
      for j := 0 to jmax do  // Signals
      for k := 0 to kmax do  // Samples
      begin
        rawValue := openFile.RawDataRecord[i, j, k];
        RawDataGrid.Cells[j + 1, m] := IntToStr(RawValue);
        m := i * kmax + k;
      end;
      if imax > 0 then
        ProgressBar1.Position := trunc(i / imax * 100);
      application.ProcessMessages;
    end;
    RawDataGrid.EndUpdate;
    RawDataGrid.TopRow := 1;
  end;
end;

end.

