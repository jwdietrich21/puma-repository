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
    procedure SizeCheck;
    procedure RecNumCheck;
    procedure DataDurationCheck;
    procedure NumOfSignalsCheck;
    procedure LabelsCheck;
    procedure TransducersCheck;
    procedure PhysDimsCheck;
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
  TestDate1 = '19.04.18';
var
  theDoc: TEDFDoc;
  startDateString: Str8;
  TestDate2: TDateTime;
begin
  theDoc := TEDFDoc.Create;
  theDoc.StartDate := TestDate1;
  AssertEquals(TestDate1, theDoc.StartDate);
  TestDate2 := EncodeDate(2018, 4, 23);
  theDoc.dStartDate := TestDate2;
  AssertEquals(TestDate2, theDoc.dStartDate);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.TimeCheck;
const
  TestTime1 = '13.21.23';
var
  theDoc: TEDFDoc;
  startTimeString: Str8;
  TestTime2: TDateTime;
begin
  theDoc := TEDFDoc.Create;
  theDoc.StartTime := TestTime1;
  AssertEquals(TestTime1, theDoc.StartTime);
  TestTime2 := EncodeTime(22, 15, 51, 0);
  theDoc.dStartTime := TestTime2;
  AssertEquals(TestTime2, theDoc.dStartTime);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.SizeCheck;
var
  theDoc: TEDFDoc;
begin
  theDoc := TEDFDoc.Create;
  AssertEquals(length(theDoc.header), StrToInt(theDoc.NumOfBytes));
  AssertEquals(length(theDoc.header), theDoc.iNumOfBytes);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.RecNumCheck;
var
  theDoc: TEDFDoc;
begin
  theDoc := TEDFDoc.Create;
  theDoc.NumOfDataRecs := FormatFloat(kZero8, 13);
  AssertEquals(13, StrToInt(theDoc.NumOfDataRecs));
  theDoc.iNumOfDataRecs := 23;
  AssertEquals(23, theDoc.iNumOfDataRecs);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.DataDurationCheck;
var
  theDoc: TEDFDoc;
begin
  theDoc := TEDFDoc.Create;
  theDoc.DurationOfData := FormatFloat(kZero8, 10);
  AssertEquals(10, StrToInt(theDoc.DurationOfData));
  theDoc.DurationOfData := 24;
  AssertEquals(24, theDoc.iDurationOfData);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.NumOfSignalsCheck;
var
  theDoc: TEDFDoc;
begin
  theDoc := TEDFDoc.Create;
  theDoc.NumOfSignals := FormatFloat(kZero4, 24);
  AssertEquals(24, StrToInt(theDoc.NumOfSignals));
  theDoc.iNumOfSignals := 21;
  AssertEquals(21, theDoc.iNumOfSignals);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.LabelsCheck;
var
  theDoc: TEDFDoc;
begin
  theDoc := TEDFDoc.Create;
  theDoc.NumOfSignals := 6;
  theDoc.SignalLabel[0] := 'I';
  theDoc.SignalLabel[1] := 'II';
  theDoc.SignalLabel[2] := 'III';
  theDoc.SignalLabel[3] := 'ECG avR';
  theDoc.SignalLabel[4] := 'ECG avL';
  theDoc.SignalLabel[5] := 'ECG avF';
  AssertEquals('ECG avL', theDoc.SignalLabel[4]); // retrievable?
  theDoc.NumOfSignals := 12;
  theDoc.SignalLabel[8] := 'ECG V3'; // can label be appended discontinuously?
  AssertEquals('ECG V3', theDoc.SignalLabel[8]);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.TransducersCheck;
var
  theDoc: TEDFDoc;
begin
  theDoc := TEDFDoc.Create;
  theDoc.NumOfSignals := 6;
  theDoc.Transducer[3] := 'AgAgCl electrode';
  AssertEquals('AgAgCl electrode', theDoc.Transducer[3]);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.PhysDimsCheck;
var
  theDoc: TEDFDoc;
begin
  theDoc := TEDFDoc.Create;
  theDoc.NumOfSignals := 12;
  theDoc.PhysDim[3] := 'mV';
  AssertEquals('mV', theDoc.PhysDim[3]);
  theDoc.Destroy;
end;


initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TEDFDocTestCases);
end.
