unit UnitConverterTestCase;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Unit Converter }

{ Version 1.1.2 }

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
  Classes, SysUtils, fpcunit, testutils, testregistry, Math, unitconverter;

const
  T4_MOLAR_MASS = 776.87; {molar mass of T4}
  T3_MOLAR_MASS = 650.97; {molar mass of T3}

type

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
  end;

  TUnitParserTestCases = class(TTestCase)
  published
    procedure TestCase1;
    procedure TestCase2;
    procedure TestCase3;
    procedure TestCase4;
    procedure TestCase5;
    procedure TestCase11;
  end;

  TMeasurementParserTestCases = class(TTestCase)
  published
    procedure TestCase1;
    procedure TestCase2;
    procedure TestCase3;
    procedure TestCase4;
    procedure TestCase5;
    procedure TestCase6;
    procedure TestCase7;
  end;

  TconverterTestCases = class(TTestCase)
  published
    procedure TestCase1;
    procedure TestCase2;
    procedure TestCase3;
    procedure TestCase4;
    procedure TestCase5;
    procedure TestCase6;
    procedure TestCase7;
    procedure TestCase8;
    procedure TestCase9;
    procedure TestCase10;
    procedure TestCase11;
    procedure TestCase12;
    procedure TestCase13;
    procedure TestCase14;
    procedure TestCase15;
    procedure TestCase16;
  end;


implementation

{ -- Base functionality test -- }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertNull('This test is bound to succeed', nil);
end;

{ -- Unit parser tests -- }

procedure TUnitParserTestCases.TestCase1;
{ NA string }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('NA');
  AssertEquals('NA', theUnitElements.MassPrefix);
  AssertEquals('NA', theUnitElements.MassUnit);
  AssertEquals('NA', theUnitElements.VolumePrefix);
  AssertEquals('NA', theUnitElements.VolumeUnit);
end;

procedure TUnitParserTestCases.TestCase2;
{ empty string }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('');
  AssertEquals('', theUnitElements.MassPrefix);
  AssertEquals('', theUnitElements.MassUnit);
  AssertEquals('', theUnitElements.VolumePrefix);
  AssertEquals('', theUnitElements.VolumeUnit);
end;

procedure TUnitParserTestCases.TestCase3;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('mU/l');
  AssertEquals('m', theUnitElements.MassPrefix);
  AssertEquals('U', theUnitElements.MassUnit);
  AssertEquals('', theUnitElements.VolumePrefix);
  AssertEquals('l', theUnitElements.VolumeUnit);
end;

procedure TUnitParserTestCases.TestCase4;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('ng/dl');
  AssertEquals('n', theUnitElements.MassPrefix);
  AssertEquals('g', theUnitElements.MassUnit);
  AssertEquals('d', theUnitElements.VolumePrefix);
  AssertEquals('l', theUnitElements.VolumeUnit);
end;

procedure TUnitParserTestCases.TestCase5;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('pg/ml');
  AssertEquals('p', theUnitElements.MassPrefix);
  AssertEquals('g', theUnitElements.MassUnit);
  AssertEquals('m', theUnitElements.VolumePrefix);
  AssertEquals('l', theUnitElements.VolumeUnit);
end;

procedure TUnitParserTestCases.TestCase11;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('pmol/l');
  AssertEquals('p', theUnitElements.MassPrefix);
  AssertEquals('mol', theUnitElements.MassUnit);
  AssertEquals('', theUnitElements.VolumePrefix);
  AssertEquals('l', theUnitElements.VolumeUnit);
end;

{ -- Measurement parser tests -- }

procedure TMeasurementParserTestCases.TestCase1;
{ Empty string }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('');
  AssertTrue(isNaN(theMeasurement.Value));
  AssertEquals('', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase2;
{ Zero value }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('0');
  AssertEquals(0, theMeasurement.Value);
  AssertEquals('', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase3;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('1 mU/l');
  AssertEquals(1, theMeasurement.Value);
  AssertEquals('mU/l', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase4;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('1,3 ng/dl');
  AssertEquals(1.3, theMeasurement.Value);
  AssertEquals('ng/dl', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase5;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('4 pg/ml');
  AssertEquals(4, theMeasurement.Value);
  AssertEquals('pg/ml', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase6;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('1.6 ng/dl');
  AssertEquals(1.6, theMeasurement.Value);
  AssertEquals('ng/dl', theMeasurement.uom);
end;

procedure TMeasurementParserTestCases.TestCase7;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('0.01 mU/l');
  AssertEquals(0.01, theMeasurement.Value);
  AssertEquals('mU/l', theMeasurement.uom);
end;

{ -- Unit converter tests -- }

procedure TconverterTestCases.TestCase1;
{empty value}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('', 1, 'ng/dl');
  AssertEquals('', theResultString);
end;

procedure TconverterTestCases.TestCase2;
{T4: pmol/l to ng/dl}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('20 pmol/l', T4_MOLAR_MASS, 'ng/dl');
  AssertEquals('1.55', LeftStr(theResultString, 4));
  AssertEquals('ng/dl', RightStr(theResultString, 5));
end;

procedure TconverterTestCases.TestCase3;
{T4: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('20 pmol/l', T4_MOLAR_MASS, 'ng/l');
  AssertEquals('15.5', LeftStr(theResultString, 4));
  AssertEquals('ng/l', RightStr(theResultString, 4));
end;

procedure TconverterTestCases.TestCase4;
{T4: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('18 ng/l', T4_MOLAR_MASS, 'pmol/l');
  AssertEquals('23.1', LeftStr(theResultString, 4));
  AssertEquals('pmol/l', RightStr(theResultString, 6));
end;

procedure TconverterTestCases.TestCase5;
{T4: ng/dl to pmol/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('1.8 ng/dl', T4_MOLAR_MASS, 'pmol/l');
  AssertEquals('23.1', LeftStr(theResultString, 4));
  AssertEquals('pmol/l', RightStr(theResultString, 6));
end;

procedure TconverterTestCases.TestCase6;
{T4: identical units}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('18 ng/l', T4_MOLAR_MASS, 'ng/l');
  AssertEquals('18', LeftStr(theResultString, 2));
  AssertEquals('ng/l', RightStr(theResultString, 4));
end;

procedure TconverterTestCases.TestCase7;
{T3: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('3.2 ng/l', T3_MOLAR_MASS, 'pmol/l');
  AssertEquals('4.9', LeftStr(theResultString, 3));
  AssertEquals('pmol/l', RightStr(theResultString, 6));
end;

procedure TconverterTestCases.TestCase8;
{T3: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('5 pmol/l', T3_MOLAR_MASS, 'ng/l');
  AssertEquals('3.2', LeftStr(theResultString, 3));
  AssertEquals('ng/l', RightStr(theResultString, 4));
end;

procedure TconverterTestCases.TestCase9;
{T3: pmol/l to pg/ml}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('5 pmol/l', T3_MOLAR_MASS, 'pg/ml');
  AssertEquals('3.2', LeftStr(theResultString, 3));
  AssertEquals('pg/ml', RightStr(theResultString, 5));
end;

procedure TconverterTestCases.TestCase10;
{T4: ng/l to mol/l}
var
  theResult: real;
begin
  theResult := ValueFromUnit('18 ng/l', T4_MOLAR_MASS, 'mol/l');
  AssertTrue((theResult > 23.0e-12) and (theResult < 23.2e-12));
end;

procedure TconverterTestCases.TestCase11;
{T4: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := UnitFromValue(20, T4_MOLAR_MASS, 'pmol/l', 'ng/l');
  AssertEquals('15.5', LeftStr(theResultString, 4));
  AssertEquals('ng/l', RightStr(theResultString, 4));
end;

procedure TconverterTestCases.TestCase12;
{T4: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := UnitFromValue(18, T4_MOLAR_MASS, 'ng/l', 'pmol/l');
  AssertEquals('23.1', LeftStr(theResultString, 4));
  AssertEquals('pmol/l', RightStr(theResultString, 6));
end;

procedure TconverterTestCases.TestCase13;
{T4: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := UnitFromValueF(20, T4_MOLAR_MASS, 'pmol/l', 'ng/l', ffNumber, 2, 2);
  AssertEquals('15.5', LeftStr(theResultString, 4));
  AssertEquals('ng/l', RightStr(theResultString, 4));
end;

procedure TconverterTestCases.TestCase14;
{T4: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := UnitFromValue(18, T4_MOLAR_MASS, 'ng/l', 'pmol/l');
  theResultString := UnitFromValueF(18, T4_MOLAR_MASS, 'ng/l', 'pmol/l', ffNumber, 2, 2);
  AssertEquals('23.1', LeftStr(theResultString, 4));
  AssertEquals('pmol/l', RightStr(theResultString, 6));
end;

procedure TconverterTestCases.TestCase15;
{T3: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnitF('3.2 ng/l', T3_MOLAR_MASS, 'pmol/l', ffNumber, 2, 2);
  AssertEquals('4.9', LeftStr(theResultString, 3));
  AssertEquals('pmol/l', RightStr(theResultString, 6));
end;

procedure TconverterTestCases.TestCase16;
{T3: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnitF('5 pmol/l', T3_MOLAR_MASS, 'ng/l', ffNumber, 2, 2);
  AssertEquals('3.2', LeftStr(theResultString, 3));
  AssertEquals('ng/l', RightStr(theResultString, 4));
end;

initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TUnitParserTestCases);
  RegisterTest(TMeasurementParserTestCases);
  RegisterTest(TconverterTestCases);

end.

