program edfplustests;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ EDF testing project for FPCUnit }

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

uses
  Interfaces, Forms, GuiTestRunner, EDFTestCases, EDFplus, EDF;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

