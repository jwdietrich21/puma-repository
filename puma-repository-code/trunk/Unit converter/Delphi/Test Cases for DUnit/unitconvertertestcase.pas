unit unitconvertertestcase;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Unit Converter }

{ Version 1.2.2 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ Parser and converter for measurement units }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://puma-repository.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

interface

uses

 Classes, SysUtils, TestFrameWork, Math, unitconverter;

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
   procedure TestCase101;
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
{    procedure TestCase14;
    procedure TestCase15;
    procedure TestCase16;
    procedure TestCase101;  }
  end;

implementation

{ -- FPC adapter functions -- }
{ -- Emulate functionality of Free Pascal in Delphi -- }

function AssertTrue(ACondition: boolean): boolean;
begin
  result := ACondition;
end;

{ -- Base functionality test -- }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  Check(true);
end;

{ -- Unit parser tests -- }

procedure TUnitParserTestCases.TestCase1;
{ NA string }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('NA');
  Check(theUnitElements.MassPrefix = 'NA');
  Check(theUnitElements.MassUnit = 'NA');
  Check(theUnitElements.VolumePrefix = 'NA');
  Check(theUnitElements.VolumeUnit = 'NA');
end;

procedure TUnitParserTestCases.TestCase2;
{ empty string }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('');
  Check(theUnitElements.MassPrefix = '');
  Check(theUnitElements.MassUnit = '');
  Check(theUnitElements.VolumePrefix = '');
  Check(theUnitElements.VolumeUnit = '');
end;

procedure TUnitParserTestCases.TestCase3;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('mU/l');
  Check(theUnitElements.MassPrefix = 'm');
  Check(theUnitElements.MassUnit = 'U');
  Check(theUnitElements.VolumePrefix = '');
  Check(theUnitElements.VolumeUnit = 'l');
end;

procedure TUnitParserTestCases.TestCase4;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('ng/dl');
  Check(theUnitElements.MassPrefix = 'n');
  Check(theUnitElements.MassUnit = 'g');
  Check(theUnitElements.VolumePrefix = 'd');
  Check(theUnitElements.VolumeUnit = 'l');
end;

procedure TUnitParserTestCases.TestCase5;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('pg/ml');
  Check(theUnitElements.MassPrefix = 'p');
  Check(theUnitElements.MassUnit = 'g');
  Check(theUnitElements.VolumePrefix = 'm');
  Check(theUnitElements.VolumeUnit = 'l');
end;

procedure TUnitParserTestCases.TestCase11;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('pmol/l');
  Check(theUnitElements.MassPrefix = 'p');
  Check(theUnitElements.MassUnit = 'mol');
  Check(theUnitElements.VolumePrefix = '');
  Check(theUnitElements.VolumeUnit = 'l');
end;

procedure TUnitParserTestCases.TestCase101;
{ Typical example }
var
  theUnitElements: tUnitElements;
begin
  theUnitElements := ParsedUnitString('/nl');
  Check(theUnitElements.MassPrefix = 'NA');
  Check(theUnitElements.MassUnit = 'NA');
  Check(theUnitElements.VolumePrefix = 'n');
  Check(theUnitElements.VolumeUnit = 'l');
end;

{ -- Measurement parser tests -- }

procedure TMeasurementParserTestCases.TestCase1;
{ Empty string }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('');
  AssertTrue(isNaN(theMeasurement.Value));
  Check(theMeasurement.uom = '');
end;

procedure TMeasurementParserTestCases.TestCase2;
{ Zero value }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('0');
  Check(theMeasurement.Value = 0);
  Check(theMeasurement.uom = '');
end;

procedure TMeasurementParserTestCases.TestCase3;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('1 mU/l');
  Check(theMeasurement.Value = 1);
  Check(theMeasurement.uom = 'mU/l');
end;

procedure TMeasurementParserTestCases.TestCase4;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('1,3 ng/dl');
  Check(theMeasurement.Value = 1.3);
  Check(theMeasurement.uom = 'ng/dl');
end;

procedure TMeasurementParserTestCases.TestCase5;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('4 pg/ml');
  Check(theMeasurement.Value = 4);
  Check(theMeasurement.uom = 'pg/ml');
end;

procedure TMeasurementParserTestCases.TestCase6;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('1.6 ng/dl');
  Check(theMeasurement.Value = 1.6);
  Check(theMeasurement.uom = 'ng/dl');
end;

procedure TMeasurementParserTestCases.TestCase7;
{ Typical measurement result }
var
  theMeasurement: tMeasurement;
begin
  theMeasurement := ParsedMeasurement('0.01 mU/l');
  Check(theMeasurement.Value = 0.01);
  Check(theMeasurement.uom = 'mU/l' );
end;

{ -- Unit converter tests -- }

procedure TconverterTestCases.TestCase1;
{empty value}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('', 1, 'ng/dl');
  Check(theResultString = '');
end;

procedure TconverterTestCases.TestCase2;
{T4: pmol/l to ng/dl}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('20 pmol/l', T4_MOLAR_MASS, 'ng/dl');
  Check(LeftStr(theResultString, 4) = '1.55');
  Check(RightStr(theResultString, 5) = 'ng/dl');
end;

procedure TconverterTestCases.TestCase3;
{T4: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('20 pmol/l', T4_MOLAR_MASS, 'ng/l');
  Check(LeftStr(theResultString, 4) = '15.5');
  Check(RightStr(theResultString, 4) = 'ng/l');
end;

procedure TconverterTestCases.TestCase4;
{T4: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('18 ng/l', T4_MOLAR_MASS, 'pmol/l');
  Check(LeftStr(theResultString, 4) = '23.1');
  Check(RightStr(theResultString, 6) = 'pmol/l');
end;

procedure TconverterTestCases.TestCase5;
{T4: ng/dl to pmol/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('1.8 ng/dl', T4_MOLAR_MASS, 'pmol/l');
  Check(LeftStr(theResultString, 4) = '23.1');
  Check(RightStr(theResultString, 6) = 'pmol/l');
end;

procedure TconverterTestCases.TestCase6;
{T4: identical units}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('18 ng/l', T4_MOLAR_MASS, 'ng/l');
  Check(LeftStr(theResultString, 2) = '18');
  Check(RightStr(theResultString, 4) = 'ng/l');
end;

procedure TconverterTestCases.TestCase7;
{T3: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('3.2 ng/l', T3_MOLAR_MASS, 'pmol/l');
  Check(LeftStr(theResultString, 3) = '4.9');
  Check(RightStr(theResultString, 6) = 'pmol/l');
end;

procedure TconverterTestCases.TestCase8;
{T3: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('5 pmol/l', T3_MOLAR_MASS, 'ng/l');
  Check(LeftStr(theResultString, 3) = '3.2');
  Check(RightStr(theResultString, 4) = 'ng/l');
end;

procedure TconverterTestCases.TestCase9;
{T3: pmol/l to pg/ml}
var
  theResultString: String;
begin
  theResultString := ConvertedUnit('5 pmol/l', T3_MOLAR_MASS, 'pg/ml');
  Check(LeftStr(theResultString, 3) = '3.2');
  Check(RightStr(theResultString, 5) = 'pg/ml');
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
  Check(LeftStr(theResultString, 4) = '15.5');
  Check(RightStr(theResultString, 4) = 'ng/l');
end;

procedure TconverterTestCases.TestCase12;
{T4: ng/l to pmol/l}
var
  theResultString: String;
begin
  theResultString := UnitFromValue(18, T4_MOLAR_MASS, 'ng/l', 'pmol/l');
  Check(LeftStr(theResultString, 4) = '23.1');
  Check(RightStr(theResultString, 6) = 'pmol/l');
end;

procedure TconverterTestCases.TestCase13;
{T4: pmol/l to ng/l}
var
  theResultString: String;
begin
  theResultString := UnitFromValueF(20, T4_MOLAR_MASS, 'pmol/l', 'ng/l', ffNumber, 4, 2);
  Check(LeftStr(theResultString, 4) = '15.5');
  Check(RightStr(theResultString, 4) = 'ng/l');
end;

{$IFDEF CONVERSIONREADY}
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

procedure TconverterTestCases.TestCase101;
{Cell count: /nl to /µl}
var
  theResultString: String;
begin
  theResultString := ConvertedUnitF('5 /nl', 1, '/µl', ffNumber, 2, 2);
  AssertEquals('5,000.00', LeftStr(theResultString, 8));
end;

{$ENDIF}

initialization

TestFramework.RegisterTest(TControlTestCases.Suite);
TestFramework.RegisterTest(TUnitParserTestCases.Suite);
TestFramework.RegisterTest(TMeasurementParserTestCases.Suite);
TestFramework.RegisterTest(TconverterTestCases.Suite);

end.