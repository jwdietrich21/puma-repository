program EDFInspector;

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

{ $DEFINE debug}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, pascalscript, tachartlazaruspkg, GUI, DataRecordGrid, Plot
  {$IFDEF debug}
  , SysUtils
  {$ENDIF}
;

{$R *.res}

begin
  {$IFDEF debug}
  if FileExists('heaptrace.trc') then
    DeleteFile('heaptrace.trc');
  SetHeapTraceOutput('heaptrace.trc');
  {$ENDIF}
  Application.Title := 'EDF Inspector';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TValuesGridForm, ValuesGridForm);
  ValuesGridForm.Visible := false;
  ValuesGridForm.ScaledDataReady := false;
  Application.CreateForm(TPlotForm, PlotForm);
  PlotForm.Visible := false;
  Application.Run;
end.

