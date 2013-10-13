program UnitConverterTests;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Version 1.1.0 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ Parser and converter for measurement units }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, UnitConverterTestCase, unitconverter;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

