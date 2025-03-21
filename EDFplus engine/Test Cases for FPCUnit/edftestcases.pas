unit EDFTestCases;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ EDF test cases }

{ Version 1.2.0 (Hyperborea) }

{ (c) Johannes W. Dietrich, 1994 - 2020 }
{ (c) Oivind Toien, 2020 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2020 }

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
  Classes, SysUtils, fpcunit, testregistry, DateUtils, Math, EDF, EDFplus;

type

  { TControlTestCases }

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
    procedure CodeVersionCheck;
  end;

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

  { TEDFplusDocTestCases }

  TEDFPlusDocTestCases = class(TTestCase)
  published
    procedure EDFFullDocTest1;
    procedure EDFFullDocTest2;
    procedure TALTest;
  end;


implementation

{ -- Base functionality test -- }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertNull('This test is bound to succeed', nil);
end;

procedure TControlTestCases.CodeVersionCheck;
{ The subsequent tests are compatible with EDF Engine version 1.1 }
begin
  AssertEquals(1, EDFEngine_major);
  AssertEquals(2, EDFEngine_minor);
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
  AssertEquals('0', theHeader[1]);
  theDoc.Destroy;
end;

procedure TEDFDocTestCases.DateCheck;
const
  TestDate1 = '19.04.18';
var
  theDoc: TEDFDoc;
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
  j, k: integer;
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
  for j := 1 to theDoc.iNumOfSignals do
  for k := 1 to theDoc.iNumOfSamples[j - 1] do
  begin
    theDoc.RawDataRecord[0, j - 1, k - 1] := j * 100 + k;
  end;
  AssertEquals(130, theDoc.RawDataRecord[0, 0, 29]);
  AssertEquals(201, theDoc.RawDataRecord[0, 1, 0]);
  theDoc.Destroy;
end;

{ TEDFPlusDocTestCases }

procedure TEDFPlusDocTestCases.EDFFullDocTest1;
var
  theDoc: TEDFplusDoc;
  j, k: integer;
  PatData: TLocalPatRecord;
  RecData: TLocalRecRecord;
begin
  theDoc := TEDFPlusDoc.Create;
  PatData.Name := 'Lieschen Müller';
  PatData.HospitalCode := '01234567 54321';
  PatData.Sex := 'F';
  PatData.sBirthDate := '31-Dec-1913';
  theDoc.LocalPatID := PatData;
  RecData.sStartDate := '13-May-1991';
  RecData.HospitalAdminCode := 'BN 01 01189';
  RecData.InvestigatorID := 'JWD';
  RecData.Equipment := 'Lazarus and Free Pascal';
  theDoc.LocalRecID := RecData;
  theDoc.StartDate := '13.05.91';
  theDoc.StartTime := '19.01.13';
  theDoc.RecordingType := EDF_C;
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
  AssertEquals('31-Dec-1913', theDoc.LocalPatID.sBirthDate);
  AssertEquals('BN 01 01189', theDoc.LocalRecID.HospitalAdminCode);
  AssertTrue(theDoc.RecordingType = EDF_C);
  AssertEquals('test signal 2', theDoc.SignalLabel[1]);
  AssertEquals(3, theDoc.iNumOfSamples[1]);
  for j := 1 to theDoc.iNumOfSignals do
  for k := 1 to theDoc.iNumOfSamples[j - 1] do
  begin
    theDoc.RawDataRecord[0, j - 1, k - 1] := j * 100 + k;
  end;
  AssertEquals(130, theDoc.RawDataRecord[0, 0, 29]);
  AssertEquals(201, theDoc.RawDataRecord[0, 1, 0]);
  theDoc.Destroy;
end;

procedure TEDFPlusDocTestCases.EDFFullDocTest2;
var
  theDoc: TEDFplusDoc;
  j, k: integer;
  PatData: TLocalPatRecord;
  RecData: TLocalRecRecord;
begin
  theDoc := TEDFPlusDoc.Create;
  PatData.UseDateTime := true;
  PatData.Name := 'Max Mustermann';
  PatData.HospitalCode := '01234568 54322';
  PatData.Sex := 'M';
  PatData.dBirthDate := EncodeDate(1908, 05, 31);
  theDoc.LocalPatID := PatData;
  RecData.UseDateTime := true;
  RecData.dStartDate := EncodeDate(1969, 07, 21);
  RecData.HospitalAdminCode := 'BN_01_01189';
  RecData.InvestigatorID := 'JWD';
  RecData.Equipment := 'Lazarus and Free Pascal';
  theDoc.LocalRecID := RecData;
  theDoc.dStartDate := RecData.dStartDate;
  theDoc.dStartTime := EncodeTime(2, 56, 20, 0);
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
  AssertEquals(EncodeDate(1908, 05, 31), theDoc.dLocalPatID.dBirthDate);
  AssertEquals(EncodeDate(1969, 07, 21), theDoc.dLocalRecID.dStartDate);
  AssertEquals('test signal 2', theDoc.SignalLabel[1]);
  AssertEquals(3, theDoc.iNumOfSamples[1]);
  AssertEquals(0, theDoc.TimePoint[0, 0]);
  AssertEquals(13, theDoc.TimePoint[0, 31]);
  AssertEquals(ComposeDateTime(theDoc.dStartDate, theDoc.dStartTime), theDoc.TimeStamp[0, 0, 0]);
  AssertEquals(IncSecond(ComposeDateTime(theDoc.dStartDate, theDoc.dStartTime), trunc(theDoc.TimePoint[1, 3])), theDoc.TimeStamp[0, 1, 3]);
  for j := 1 to theDoc.iNumOfSignals do
  for k := 1 to theDoc.iNumOfSamples[j - 1] do
  begin
    theDoc.RawDataRecord[0, j - 1, k - 1] := j * 100 + k;
  end;
  AssertEquals(130, theDoc.RawDataRecord[0, 0, 29]);
  AssertEquals(201, theDoc.RawDataRecord[0, 1, 0]);
  theDoc.Destroy;
end;

procedure TEDFPlusDocTestCases.TALTest;
var
  theDoc: TEDFplusDoc;
  PatData: TLocalPatRecord;
  RecData: TLocalRecRecord;
  Annotation1, Annotation2, Annotation3: TTALRecord;
  Annotation4, Annotation5, Annotation6: TTALRecord;
begin
  theDoc := TEDFPlusDoc.Create;
  PatData.Name := 'John Doe';
  PatData.HospitalCode := '98765432 14567';
  PatData.Sex := 'M';
  PatData.dBirthDate := EncodeDate(1900, 05, 13);
  theDoc.LocalPatID := PatData;
  RecData.dStartDate := EncodeDate(1969, 07, 21);
  RecData.HospitalAdminCode := 'BN_01_01190';
  RecData.InvestigatorID := 'JWD';
  RecData.Equipment := 'Lazarus and Free Pascal';
  theDoc.LocalRecID := RecData;
  theDoc.dStartDate := RecData.dStartDate;
  theDoc.dStartTime := EncodeTime(2, 56, 20, 0);
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
  Annotation1.onset := NaN;
  Annotation1.duration := 13;
  SetLength(Annotation1.comment, 1);
  Annotation1.comment[0] := 'Recording starts';
  theDoc.AddAnnotation(0, Annotation1);
  Annotation2.onset := 0;
  Annotation2.duration := 660;
  SetLength(Annotation2.comment, 1);
  Annotation2.comment[0] := 'Sleep Stage W';
  theDoc.AddAnnotation(0, Annotation2);
  Annotation3.onset := 993.2;
  Annotation3.duration := 1.2;
  SetLength(Annotation3.comment, 2);
  Annotation3.comment[0] := 'Limb movement';
  Annotation3.comment[1] := 'R + L leg';
  theDoc.AddAnnotation(0, Annotation3);
  Annotation4 := theDoc.Annotation[0, 0];
  AssertTrue(IsNaN(Annotation4.onset));
  AssertEquals(Annotation1.duration, Annotation4.duration);
  AssertEquals(Annotation1.comment[0], Annotation4.comment[0]);
  Annotation5 := theDoc.Annotation[0, 1];
  AssertEquals(Annotation2.onset, Annotation5.onset);
  AssertEquals(Annotation2.duration, Annotation5.duration);
  AssertEquals(Annotation2.comment[0], Annotation5.comment[0]);
  Annotation6 := theDoc.Annotation[0, 2];
  AssertEquals(Annotation3.onset, Annotation6.onset);
  AssertEquals(Annotation3.duration, Annotation6.duration);
  AssertEquals(Annotation3.comment[0], Annotation6.comment[0]);
  AssertEquals(Annotation3.comment[1], Annotation6.comment[1]);
  AssertEquals(theDoc.RecordStart[0], 13);
  theDoc.Destroy;
end;

initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TEDFDocTestCases);
  RegisterTest(TEDFPlusDocTestCases);
end.

