program UnitConverterTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, UnitConverterTestCase, unitconverter;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

