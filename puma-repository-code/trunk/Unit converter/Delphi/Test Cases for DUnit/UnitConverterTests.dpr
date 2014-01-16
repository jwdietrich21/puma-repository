program UnitConverterTests;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  unitconvertertestcase in 'unitconvertertestcase.pas',
  UnitConverter in '..\unitconverter.pas';

{$R *.RES}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
