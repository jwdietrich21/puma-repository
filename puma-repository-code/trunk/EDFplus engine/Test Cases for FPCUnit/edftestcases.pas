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

  TEDFDocTestCases = class(TTestCase)
  published
    procedure VersionCheck;
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


initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TEDFDocTestCases);
end.

