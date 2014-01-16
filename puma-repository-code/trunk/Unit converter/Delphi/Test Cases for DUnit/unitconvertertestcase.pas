unit unitconvertertestcase;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Unit Converter }

{ Version 1.2.1 }

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

implementation

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

initialization

TestFramework.RegisterTest(TControlTestCases.Suite);
TestFramework.RegisterTest(TUnitParserTestCases.Suite);
end.