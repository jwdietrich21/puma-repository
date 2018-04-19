unit EDFTestCases;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ EDF test cases }

{ Version 1.0 (Alpha Centauri) }

{ (c) Johannes W. Dietrich, 1994 - 2018 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2018 }

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

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, EDF;

type

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
  end;

  { TEDFplusDocTestCases }

  { TEDFDocTestCases }

  TEDFDocTestCases = class(TTestCase)
  published
    procedure VersionCheck;
    procedure DateCheck;
    procedure TimeCheck;
  end;

implementation


{ -- Base functionality test -- }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertNull('This test is bound to succeed', nil);
end;

{ TEDFplusDocTestCases }

procedure TEDFDocTestCases.VersionCheck;
var
  theDoc: TEDFDoc;
  theHeader: AnsiString;
begin
  theDoc := TEDFDoc.Create;
  AssertEquals('0       ', theDoc.version);
  theHeader := theDoc.header;
  AssertEquals('0', theDoc.header[1]);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.DateCheck;
const
  TestDate = '19.04.18';
var
  theDoc: TEDFDoc;
  startDateString: Str8;
begin
  theDoc := TEDFDoc.Create;
  theDoc.StartDate := TestDate;
  AssertEquals(TestDate, theDoc.StartDate);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.TimeCheck;
const
  TestTime = '13.21.23';
var
  theDoc: TEDFDoc;
  startTimeString: Str8;
begin
  theDoc := TEDFDoc.Create;
  theDoc.StartTime := TestTime;
  AssertEquals(TestTime, theDoc.StartTime);
  theDoc.Destroy;
end;


initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TEDFDocTestCases);
end.

