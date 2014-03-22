unit PID;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for patient identification segments }

{ Version 1.5 }

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
    SetID: tSI;
    PatientID: str20;
    PatientIDList: str250;
    AltPatID: str20;
    PatientName: tXPN;
    MothersMaidenName: tXPN;
    BirthDateTime: tDTM;
    AdminSex: tIS;
    PatientAlias: tXPN;
    Race: tCE;
    PatientAddress: tXAD;
    CountyCode: tIS;
    HomePhoe: tXTN;
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
    LastUpdateDateTime: tDTM;
    LastUpdateFacility: tHD;
    SpeciesCode, BreedCode: tCE;
    Strain: tST;
    ProductionClassCode: tCE;
    TribalCitizenship: tCWE;
  end;

function PID_Segment(message: THL7Message): THL7Segment;
procedure GetPID(message: THL7Message; out PIDRecord: tPID);
procedure SetPID(message: THL7Message; aSegment: THL7Segment);
procedure SetPID(message: THL7message; PIDRecord: tPID);

implementation

function PID_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(PID_ID, '0')
  else
    Result := nil;
end;

procedure GetPID(message: THL7Message; out PIDRecord: tPID);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := PID_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with PIDRecord do
      begin
        nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
        SetID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientIDList := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AltPatID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientName := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MothersMaidenName := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BirthDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdminSex := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientAlias := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Race := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientAddress := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CountyCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        HomePhoe := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BusinessPhone := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PrimaryLanguage := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MaritalStatus := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Religion := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientAccountNumber :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SSNNumber := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DriverLicenseNumber := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MothersID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EthnicGroup := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BirthPlace := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MultipleBirthID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BirthOrder := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Citizenship := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VeteransMilitaryStatus :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Nationality := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientDeathDateTime :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientDeathIndicator :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        IDUnknownIndicator := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        IDReliabilityIndicator :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        LastUpdateDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        LastUpdateFacility := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpeciesCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BreedCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Strain := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ProductionClassCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TribalCitizenship := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end;
end;

procedure SetPID(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetPID(message: THL7message; PIDRecord: tPID);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
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
      BirthPlace + FieldSep + MultipleBirthID + FieldSep +
      BirthOrder + FieldSep + Citizenship + FieldSep +
      VeteransMilitaryStatus + FieldSep + Nationality + FieldSep +
      PatientDeathDateTime + FieldSep + PatientDeathIndicator +
      FieldSep + IDUnknownIndicator + FieldSep + IDReliabilityIndicator +
      FieldSep + LastUpdateDateTime + FieldSep + LastUpdateFacility +
      FieldSep + SpeciesCode + FieldSep + BreedCode + FieldSep +
      Strain + FieldSep + ProductionClassCode + FieldSep +
      TribalCitizenship + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

end.
