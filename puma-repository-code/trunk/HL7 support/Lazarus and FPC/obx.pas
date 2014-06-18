unit OBX;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for observation / result segments }

{ Version 1.6 }

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
  OBX_ID = 'OBX';

type
  tOBX = record
    SetID: tSI;
    ValueType: tID;
    ObsID: tCE;
    obsSubID: tST;
    obsValue: ansistring;
    Units: tCE;
    RefRange: tST;
    AbnormFlags: tIS;  // referred to as Interpretation Codes in HL7 2.7
    probability: tNM;
    Nature, status: tID;
    RRDate: tDTM;
    UDAC: tST;
    ObsDateTime: tDTM;
    prodID: tCE;
    respObs: tXCN;
    observMethod: tCE;
    EquipInstID: tEI;
    AnalysisDateTime: tDTM;
    ObservationSite: tCWE;  // Introduced in HL7 2.7
    ObservationInstanceID: tEI;  // Introduced in HL7 2.7
    MoodCode: tCNE;  // Introduced in HL7 2.7
    PerformingOrgName: tXON;  // Introduced in HL7 2.7
    PerformingOrgAddr: tXAD;  // Introduced in HL7 2.7
    PerformingOrgMedicalDirector: tXCN;  // Introduced in HL7 2.7
    PatientResultsReleaseCat: tID;  // Introduced in HL7 2.7
  end;

function OBX_Segment(message: THL7Message): THL7Segment;
procedure GetOBX(message: THL7Message; out OBXRecord: tOBX);
procedure GetOBX(message: THL7Message; out SetID: tSI; out ValueType: tID;
  out ObsID: tCE; obsSubID: tST; out obsValue: ansistring; out Units: tCE;
  out RefRange: tST; AbnormFlags: tIS; out probability: tNM;
  out Nature, status: tID; out RRDate: tDTM; UDAC: tST; out ObsDateTime: tDTM;
  out prodID: tCE; respObs: tXCN; observMethod: tCE; EquipInstID: tEI;
  out AnalysisDateTime: tDTM);
  deprecated;
procedure GetOBX(message: THL7Message; out SetID: str4; out ValueType: str2; out ObsID: str250;
  obsSubID: str20; out obsValue: AnsiString; out Units: str250; out RefRange: str60;
  AbnormFlags, probability: str5; out Nature: str2; out status: char; out RRDate: str26;
  UDAC: str20; out ObsDateTime: str26; out prodID, respObs, observMethod: str250;
  EquipInstID: str22; out AnalysisDateTime: str26);
  deprecated;
procedure SetOBX(message: THL7Message; aSegment: THL7Segment);
procedure SetOBX(message: THL7Message; OBXRecord: tOBX);
procedure SetOBX(message: THL7Message; SetID: tSI; ValueType: tID;
  ObsID: tCE; obsSubID: tST; obsValue: ansistring; Units: tCE;
  RefRange: tST; AbnormFlags: tIS; probability: tNM; Nature, status: tID;
  RRDate: tDTM; UDAC: tST; ObsDateTime: tDTM; prodID: tCE; respObs: tXCN;
  observMethod: tCE; EquipInstID: tEI; AnalysisDateTime: tDTM);
  deprecated;
procedure SetOBX(message: THL7Message; SetID: str4; ValueType: str2; ObsID: str250;
  obsSubID: str20; obsValue: AnsiString; Units: str250; RefRange: str60;
  AbnormFlags, probability: str5; Nature: str2; status: char; RRDate: str26;
  UDAC: str20; ObsDateTime: str26; prodID, respObs, observMethod: str250;
  EquipInstID: str22; AnalysisDateTime: str26);
  deprecated;
procedure ClearOBX(OBXRecord: tOBX);

implementation

function OBX_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(OBX_ID, '0')
  else
    Result := nil;
end;

procedure GetOBX(message: THL7Message; out OBXRecord: tOBX);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := OBX_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with OBXRecord do
      begin
        nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
        SetID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ValueType := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObsID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        obsSubID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        obsValue := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Units := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        RefRange := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AbnormFlags := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        probability := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Nature := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        status := curSegment.FirstOccurrence.GetNextFieldContent(nextField)[1];
        RRDate := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        UDAC := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObsDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        prodID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        respObs := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        observMethod := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EquipInstID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AnalysisDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObservationSite := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObservationInstanceID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MoodCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PerformingOrgName := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PerformingOrgAddr := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PerformingOrgMedicalDirector := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientResultsReleaseCat := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end;
end;

procedure GetOBX(message: THL7Message; out SetID: tSI; out ValueType: tID;
  out ObsID: tCE; obsSubID: tST; out obsValue: ansistring; out Units: tCE;
  out RefRange: tST; AbnormFlags: tIS; out probability: tNM;
  out Nature, status: tID; out RRDate: tDTM; UDAC: tST; out ObsDateTime: tDTM;
  out prodID: tCE; respObs: tXCN; observMethod: tCE; EquipInstID: tEI;
  out AnalysisDateTime: tDTM);
{ deprecated method, retained for backward-compatibility only, }
{ capsules new version of polymorphic GetOBX }
var
  OBXRecord: tOBX;
begin
  GetOBX(message, OBXRecord);
  SetID := OBXRecord.SetID;
  ValueType := OBXRecord.ValueType;
  ObsID := OBXRecord.ObsID;
  obsSubID := OBXRecord.obsSubID;
  obsValue := OBXRecord.obsValue;
  Units := OBXRecord.Units;
  RefRange := OBXRecord.RefRange;
  AbnormFlags := OBXRecord.AbnormFlags;
  probability := OBXRecord.probability;
  Nature := OBXRecord.Nature;
  status := OBXRecord.status;
  RRDate := OBXRecord.RRDate;
  UDAC := OBXRecord.UDAC;
  ObsDateTime := OBXRecord.ObsDateTime;
  prodID := OBXRecord.prodID;
  respObs := OBXRecord.respObs;
  observMethod := OBXRecord.observMethod;
  EquipInstID := OBXRecord.EquipInstID;
  AnalysisDateTime := OBXRecord.AnalysisDateTime;
end;

procedure GetOBX(message: THL7Message; out SetID: str4; out ValueType: str2;
  out ObsID: str250; obsSubID: str20; out obsValue: AnsiString; out
  Units: str250; out RefRange: str60; AbnormFlags, probability: str5; out
  Nature: str2; out status: char; out RRDate: str26; UDAC: str20; out
  ObsDateTime: str26; out prodID, respObs, observMethod: str250;
  EquipInstID: str22; out AnalysisDateTime: str26);
{ deprecated method, retained for backward-compatibility only, }
{ capsules new version of polymorphic GetOBX }
var
  OBXRecord: tOBX;
begin
  GetOBX(message, OBXRecord);
  SetID := OBXRecord.SetID;
  ValueType := OBXRecord.ValueType;
  ObsID := OBXRecord.ObsID;
  obsSubID := OBXRecord.obsSubID;
  obsValue := OBXRecord.obsValue;
  Units := OBXRecord.Units;
  RefRange := OBXRecord.RefRange;
  AbnormFlags := OBXRecord.AbnormFlags;
  probability := OBXRecord.probability;
  Nature := OBXRecord.Nature;
  status := OBXRecord.status[1];
  RRDate := OBXRecord.RRDate;
  UDAC := OBXRecord.UDAC;
  ObsDateTime := OBXRecord.ObsDateTime;
  prodID := OBXRecord.prodID;
  respObs := OBXRecord.respObs;
  observMethod := OBXRecord.observMethod;
  EquipInstID := OBXRecord.EquipInstID;
  AnalysisDateTime := OBXRecord.AnalysisDateTime;
end;

procedure SetOBX(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetOBX(message: THL7Message; OBXRecord: tOBX);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with OBXRecord do
    theString := OBX_ID + FieldSep + SetID + FieldSep + ValueType +
      FieldSep + ObsID + FieldSep + obsSubID + FieldSep + obsValue +
      FieldSep + Units + FieldSep + RefRange + FieldSep + AbnormFlags +
      FieldSep + probability + FieldSep + Nature + FieldSep + status +
      FieldSep + RRDate + FieldSep + UDAC + FieldSep + ObsDateTime +
      FieldSep + prodID + FieldSep + respObs + FieldSep + observMethod +
      FieldSep + EquipInstID + FieldSep + AnalysisDateTime + FieldSep +
      ObservationSite + FieldSep + ObservationInstanceID + FieldSep +
      MoodCode + FieldSep + PerformingOrgName + FieldSep + PerformingOrgAddr +
      FieldSep + PerformingOrgMedicalDirector + FieldSep +
      PatientResultsReleaseCat + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure SetOBX(message: THL7Message; SetID: tSI; ValueType: tID;
  ObsID: tCE; obsSubID: tST; obsValue: ansistring; Units: tCE;
  RefRange: tST; AbnormFlags: tIS; probability: tNM; Nature, status: tID;
  RRDate: tDTM; UDAC: tST; ObsDateTime: tDTM; prodID: tCE; respObs: tXCN;
  observMethod: tCE; EquipInstID: tEI; AnalysisDateTime: tDTM);
{ deprecated method, retained for backward-compatibility only, }
{ capsules new version of polymorphic SetOBX }
var
  OBXRecord: tOBX;
begin
  OBXRecord.SetID := SetID;
  OBXRecord.ValueType := ValueType;
  OBXRecord.ObsID := ObsID;
  OBXRecord.obsSubID := obsSubID;
  OBXRecord.obsValue := obsValue;
  OBXRecord.Units := Units;
  OBXRecord.RefRange := RefRange;
  OBXRecord.AbnormFlags := AbnormFlags;
  OBXRecord.probability := probability;
  OBXRecord.Nature := Nature;
  OBXRecord.status := status;
  OBXRecord.RRDate := RRDate;
  OBXRecord.UDAC := UDAC;
  OBXRecord.ObsDateTime := ObsDateTime;
  OBXRecord.prodID := prodID;
  OBXRecord.respObs := respObs;
  OBXRecord.observMethod := observMethod;
  OBXRecord.EquipInstID := EquipInstID;
  OBXRecord.AnalysisDateTime := AnalysisDateTime;
  OBXRecord.ObservationSite := '';
  OBXRecord.ObservationInstanceID := '';
  OBXRecord.MoodCode := '';
  OBXRecord.PerformingOrgName := '';
  OBXRecord.PerformingOrgAddr := '';
  OBXRecord.PerformingOrgMedicalDirector := '';
  OBXRecord.PatientResultsReleaseCat := '';
  SetOBX(message, OBXRecord);
end;

procedure SetOBX(message: THL7Message; SetID: str4; ValueType: str2;
  ObsID: str250; obsSubID: str20; obsValue: AnsiString; Units: str250;
  RefRange: str60; AbnormFlags, probability: str5; Nature: str2; status: char;
  RRDate: str26; UDAC: str20; ObsDateTime: str26; prodID, respObs,
  observMethod: str250; EquipInstID: str22; AnalysisDateTime: str26);
{ deprecated method, retained for backward-compatibility only, }
{ capsules new version of polymorphic SetOBX }
var
  OBXRecord: tOBX;
begin
  OBXRecord.SetID := SetID;
  OBXRecord.ValueType := ValueType;
  OBXRecord.ObsID := ObsID;
  OBXRecord.obsSubID := obsSubID;
  OBXRecord.obsValue := obsValue;
  OBXRecord.Units := Units;
  OBXRecord.RefRange := RefRange;
  OBXRecord.AbnormFlags := AbnormFlags;
  OBXRecord.probability := probability;
  OBXRecord.Nature := Nature;
  OBXRecord.status := status;
  OBXRecord.RRDate := RRDate;
  OBXRecord.UDAC := UDAC;
  OBXRecord.ObsDateTime := ObsDateTime;
  OBXRecord.prodID := prodID;
  OBXRecord.respObs := respObs;
  OBXRecord.observMethod := observMethod;
  OBXRecord.EquipInstID := EquipInstID;
  OBXRecord.AnalysisDateTime := AnalysisDateTime;
  OBXRecord.ObservationSite := '';
  OBXRecord.ObservationInstanceID := '';
  OBXRecord.MoodCode := '';
  OBXRecord.PerformingOrgName := '';
  OBXRecord.PerformingOrgAddr := '';
  OBXRecord.PerformingOrgMedicalDirector := '';
  OBXRecord.PatientResultsReleaseCat := '';
  SetOBX(message, OBXRecord);
end;

procedure ClearOBX(OBXRecord: tOBX);
begin
  FillChar(OBXRecord, SizeOf(OBXRecord), 0);
end;

end.
