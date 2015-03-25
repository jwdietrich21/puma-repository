program hl7tests;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 testing project for FPCUnit }

{ Version 1.7.0 }

{ (c) Johannes W. Dietrich, 1994 - 2015 }
{ (c) Marek Skorupski 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

{ Parser and compiler for HL7 messages }

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
  Interfaces, Forms, GuiTestRunner, HL7TestCases, OBX, HL7, MSA, MSH, OBR, NTE,
  ERR, EVN, PID, PV1, NK1, SPM, PV2, ORC, BLG, BPO, FT1, MLLP;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
