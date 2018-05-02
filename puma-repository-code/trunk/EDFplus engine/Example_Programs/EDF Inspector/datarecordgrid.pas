unit DataRecordGrid;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ComCtrls, EDF;

type

  { TValuesGridForm }

  TValuesGridForm = class(TForm)
    DataGrid: TStringGrid;
    ProgressBar1: TProgressBar;
    StatusBar1: TStatusBar;
  private

  public
    procedure ShowDataRecord(theEDFFile: TEDFDoc);
  end;

var
  ValuesGridForm: TValuesGridForm;

implementation

{$R *.lfm}

{ TValuesGridForm }

procedure TValuesGridForm.ShowDataRecord(theEDFFile: TEDFDoc);
var
  i, k, m: longint;
  imax, kmax, mmax: longint;
  j: integer;
  jmax: integer;
  rawValue: SmallInt;
  c: TGridColumn;
begin
  if assigned(theEDFFile) then
  begin
    DataGrid.BeginUpdate;
    DataGrid.Cells[0, 0] := 'n';
    DataGrid.Columns.Clear;
    imax := high(theEDFFile.DataRecord);        // Records
    jmax := high(theEDFFile.DataRecord[0]);     // Signals
    kmax := high(theEDFFile.DataRecord[0, 0]);  // Samples
    mmax := (imax + 1) * (kmax + 1);
    DataGrid.RowCount := mmax;
    for j := 0 to jmax do
    begin
      c := DataGrid.Columns.Add;
      c.Title.Caption := theEDFFile.SignalLabel[j];
    end;
    for m := 1 to mmax - 1 do
    DataGrid.Cells[0, m] := IntToStr(m);
    DataGrid.EndUpdate;
    application.ProcessMessages;
    m := 1;
    DataGrid.BeginUpdate;
    for i := 0 to imax do  // Records
    begin
      for j := 0 to jmax do  // Signals
      for k := 0 to kmax do  // Samples
      begin
        rawValue := theEDFFile.DataRecord[i, j, k];
        DataGrid.Cells[j + 1, m] := IntToStr(RawValue);
        m := i * kmax + k;
      end;
      ProgressBar1.Position := trunc(i / imax * 100);
      application.ProcessMessages;
    end;
    DataGrid.EndUpdate;
    DataGrid.TopRow := 1;
  end;
end;

end.

