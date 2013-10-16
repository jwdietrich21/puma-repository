unit HL7TestCases;

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

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, HL7;

type

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
  end;

  { TBaseStructureTestCases }

  TBaseStructureTestCases = class(TTestCase)
  published
    procedure VersionTestcase1;
  end;

var
  TestHL7Message: THL7Message;

implementation

{ TBaseStructureTestCases }

procedure TBaseStructureTestCases.VersionTestcase1;
begin
  TestHL7Message := THL7Message.Create('2.5');
  AssertEquals('2.5', TestHL7Message.HL7Version);
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

{ -- Base functionality test -- }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertNull('This test is bound to succeed', nil);
end;

initialization
  RegisterTest(TControlTestCases);
  RegisterTest(TBaseStructureTestCases);
end.

