program hl7tests;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit}

{ Version 0.9 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ Parser and converter for measurement units }

{ Source code released under the BSD License }
{ See http://puma-repository.sf.net for details }

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  hl7,
  GuiTestRunner,
  HL7TestCases;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.