unit PID;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for patient identification segments }

{ Version 1.7.0 (Hermes) }

 { (c) J. W. Dietrich, 1994 - 2014 }
 { (c) Ludwig Maximilian University of Munich 1995 - 2002 }
 { (c) University of Ulm Hospitals 2002-2004 }
 { (c) Ruhr University of Bochum 2005 - 2014 }

{ Parser and compiler for HL7 messages }

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
  Classes, SysUtils, HL7;

const
  PID_ID = 'PID';

type
  tPID = record
    SetID:     tSI;
    PatientID: str20;
    PatientIDList: str250;
    AltPatID:  str20;
    PatientName: tXPN;
    MothersMaidenName: tXPN;
    BirthDateTime: tDTM;
    AdminSex:  tIS;
    PatientAlias: tXPN;
    Race:      tCE;
    PatientAddress: tXAD;
    CountyCode: tIS;
    HomePhoe:  tXTN;  // Should be HomePhone; retained for backwards compatibility
    BusinessPhone: tXTN;
    PrimaryLanguage, MaritalStatus: tCE;
    Religion, PatientAccountNumber: tCE;
    SSNNumber: tST;
    DriverLicenseNumber: tDLN;
    MothersID: tCX;
    EthnicGroup: tCE;
    BirthPlace: str250;
    MultipleBirthID: tID;
    BirthOrder: tNM;
    Citizenship, VeteransMilitaryStatus: tCE;
    Nationality: tCE;
    PatientDeathDateTime: tDTM;
    PatientDeathIndicator, IDUnknownIndicator: tID;
    IDReliabilityIndicator: tIS;
    IDReliabilityCode: tCWE;
    LastUpdateDateTime: tDTM;
    LastUpdateFacility: tHD;
    SpeciesCode, BreedCode: tCE;
    Strain:    tST;
    ProductionClassCode: tCE;
    TribalCitizenship: tCWE;
    PatientTelecomInformation: tXTN;  // Introduced in HL7 2.7
  end;

function PID_Segment(message: THL7Message): THL7Segment;
procedure GetPID(aSegment: THL7Segment; out PIDRecord: tPID);
procedure GetPID(message: THL7Message; out PIDRecord: tPID);
procedure SetPID(message: THL7Message; aSegment: THL7Segment);
procedure SetPID(message: THL7message; PIDRecord: tPID);
procedure ClearPID(var PIDRecord: tPID);

implementation

function PID_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(PID_ID, '0')
  else
    Result := nil;
end;

procedure GetPID(aSegment: THL7Segment; out PIDRecord: tPID);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'PID') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with PIDRecord do
      begin
        nextField := aSegment.FirstOccurrence.FirstField.nextSibling;
        SetID     := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientIDList := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AltPatID  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientName := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MothersMaidenName := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BirthDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdminSex  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientAlias := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Race      := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientAddress := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CountyCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        HomePhoe  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BusinessPhone := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PrimaryLanguage := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MaritalStatus := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Religion  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientAccountNumber :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SSNNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DriverLicenseNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MothersID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EthnicGroup := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BirthPlace := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MultipleBirthID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BirthOrder := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Citizenship := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VeteransMilitaryStatus :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Nationality := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientDeathDateTime :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientDeathIndicator :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        IDUnknownIndicator := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        IDReliabilityIndicator :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        IDReliabilityCode :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        LastUpdateDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        LastUpdateFacility := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpeciesCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BreedCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Strain    := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ProductionClassCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TribalCitizenship := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientTelecomInformation :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
    ClearPID(PIDRecord);
end;

procedure GetPID(message: THL7Message; out PIDRecord: tPID);
var
  curSegment: THL7Segment;
begin
  curSegment := PID_Segment(message);
  GetPID(curSegment, PIDRecord);
end;

procedure SetPID(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetPID(message: THL7message; PIDRecord: tPID);
var
  newSegment: THL7Segment;
  FieldSep:   char;
  theString:  ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with PIDRecord do
    theString := PID_ID + FieldSep + SetID + FieldSep + PatientID +
      FieldSep + PatientIDList + FieldSep + AltPatID + FieldSep +
      PatientName + FieldSep + MothersMaidenName + FieldSep +
      BirthDateTime + FieldSep + AdminSex + FieldSep + PatientAlias +
      FieldSep + Race + FieldSep + PatientAddress + FieldSep +
      CountyCode + FieldSep + HomePhoe + FieldSep + BusinessPhone +
      FieldSep + PrimaryLanguage + FieldSep + MaritalStatus +
      FieldSep + Religion + FieldSep + PatientAccountNumber +
      FieldSep + SSNNumber + FieldSep + DriverLicenseNumber +
      FieldSep + MothersID + FieldSep + EthnicGroup + FieldSep +
      BirthPlace + FieldSep + MultipleBirthID + FieldSep + BirthOrder +
      FieldSep + Citizenship + FieldSep + VeteransMilitaryStatus +
      FieldSep + Nationality + FieldSep + PatientDeathDateTime +
      FieldSep + PatientDeathIndicator + FieldSep + IDUnknownIndicator +
      FieldSep + IDReliabilityIndicator + FieldSep + IDReliabilityCode +
      FieldSep + LastUpdateDateTime + FieldSep + LastUpdateFacility +
      FieldSep + SpeciesCode + FieldSep + BreedCode + FieldSep +
      Strain + FieldSep + ProductionClassCode + FieldSep + TribalCitizenship +
      FieldSep + PatientTelecomInformation + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure ClearPID(var PIDRecord: tPID);
begin
  FillChar(PIDRecord, SizeOf(tPID), 0);
end;

end.
