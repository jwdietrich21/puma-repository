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

  { TControlTestCases }

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
    procedure CodeVersionCheck;
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
    procedure PhysMinMaxCheck;
    procedure DigMinMaxCheck;
    procedure PreFilterCheck;
    procedure NumOfSamplesCheck;
    procedure EDFFullDocTest;
  end;

implementation


{ -- Base functionality test -- }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertNull('This test is bound to succeed', nil);
end;

procedure TControlTestCases.CodeVersionCheck;
{ The subsequent tests are compatible with EDF Engine version 1.0 }
begin
  AssertEquals(1, EDFEngine_major);
  AssertEquals(0, EDFEngine_minor);
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
  AssertEquals(length(theDoc.header), StrToInt(Trim(theDoc.NumOfBytes)));
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

procedure TEDFDocTestCases.PhysMinMaxCheck;
var
  theDoc: TEDFDoc;
begin
  theDoc := TEDFDoc.Create;
  theDoc.NumOfSignals := 12;
  theDoc.PhysMin[3] := '-3';
  AssertEquals('-3', theDoc.PhysMin[3]);
  theDoc.ePhysMin[7] := -5;
  AssertEquals(-5, theDoc.ePhysMin[7]);
  theDoc.PhysMax[3] := '3';
  AssertEquals('3', theDoc.PhysMax[3]);
  theDoc.ePhysMax[5] := 7;
  AssertEquals(7, theDoc.ePhysMax[5]);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.DigMinMaxCheck;
var
  theDoc: TEDFDoc;
begin
  theDoc := TEDFDoc.Create;
  theDoc.NumOfSignals := 12;
  theDoc.DigMin[3] := '-2048';
  AssertEquals('-2048', theDoc.DigMin[3]);
  theDoc.DigMax[3] := '2047';
  AssertEquals('2047', theDoc.DigMax[3]);
  theDoc.iDigMin[9] := -8192;
  AssertEquals(-8192, theDoc.iDigMin[9]);
  theDoc.iDigMax[9] := 8191;
  AssertEquals(8191, theDoc.iDigMax[9]);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.PreFilterCheck;
var
  theDoc: TEDFDoc;
begin
  theDoc := TEDFDoc.Create;
  theDoc.NumOfSignals := 6;
  theDoc.Prefilter[3] := 'HP:0.1Hz LP:75Hz';
  AssertEquals('HP:0.1Hz LP:75Hz', theDoc.Prefilter[3]);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.NumOfSamplesCheck;
var
  theDoc: TEDFDoc;
begin
  theDoc := TEDFDoc.Create;
  theDoc.NumOfSignals := 4;
  theDoc.NumOfSamples[0] := '15000';
  AssertEquals('15000', theDoc.NumOfSamples[0]);
  theDoc.NumOfSamples[1] := '3';
  AssertEquals('3', theDoc.NumOfSamples[1]);
  theDoc.iNumOfSamples[2] := 1300;
  AssertEquals(1300, theDoc.iNumOfSamples[2]);
  theDoc.iNumOfSamples[3] := 7;
  AssertEquals(7, theDoc.iNumOfSamples[3]);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.EDFFullDocTest;
{ very short and simple example, not EDF+-compliant }
var
  theDoc: TEDFDoc;
begin
  theDoc := TEDFDoc.Create;
  theDoc.LocalPatID := 'John Doe';
  theDoc.LocalRecID := 'simulated test recording';
  theDoc.dStartDate := now;
  theDoc.dStartTime := now;
  theDoc.iNumOfDataRecs := 1;
  theDoc.iDurationOfData := 13;
  theDoc.iNumOfSignals := 2;
  theDoc.SignalLabel[0] := 'test signal 1';
  theDoc.SignalLabel[1] := 'test signal 2';
  theDoc.Transducer[0] := 'AgAgCl electrode';
  theDoc.Transducer[1] := 'thermistor';
  theDoc.PhysDim[0] := 'mV';
  theDoc.PhysDim[1] := 'degreeC';
  theDoc.ePhysMin[0] := -3.1;
  theDoc.ePhysMax[0] := 3;
  theDoc.ePhysMin[1] := 34.4;
  theDoc.ePhysMax[1] := 40.2;
  theDoc.idigMin[0] := -2048;
  theDoc.idigMax[0] := 2047;
  theDoc.idigMin[1] := -2048;
  theDoc.idigMax[1] := 2047;
  theDoc.Prefilter[0] := 'none';
  theDoc.Prefilter[1] := 'LP:0.1Hz';
  theDoc.iNumOfSamples[0] := 31;
  theDoc.iNumOfSamples[1] := 3;
  AssertEquals('test signal 2', theDoc.SignalLabel[1]);
  AssertEquals(3, theDoc.iNumOfSamples[1]);
  theDoc.Destroy;
end;



initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TEDFDocTestCases);
end.

