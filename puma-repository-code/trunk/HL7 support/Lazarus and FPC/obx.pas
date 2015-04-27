unit OBX;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for observation / result segments }

{ Version 2.0.0 (Hermes) }

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
    SetID:     tSI;
    ValueType: tID;
    ObsID:     tCE;
    obsSubID:  tST;
    obsValue:  ansistring;
    Units:     tCE;
    RefRange:  tST;
    AbnormFlags: tIS;  // referred to as Interpretation Codes in HL7 2.7
    probability: tNM;
    Nature, status: tID;
    RRDate:    tDTM;
    UDAC:      tST;
    ObsDateTime: tDTM;
    prodID:    tCE;
    respObs:   tXCN;
    observMethod: tCE;
    EquipInstID: tEI;
    AnalysisDateTime: tDTM;
    ObservationSite: tCWE;  // Introduced in HL7 2.7
    ObservationInstanceID: tEI;  // Introduced in HL7 2.7
    MoodCode:  tCNE;  // Introduced in HL7 2.7
    PerformingOrgName: tXON;  // Introduced in HL7 2.7
    PerformingOrgAddr: tXAD;  // Introduced in HL7 2.7
    PerformingOrgMedicalDirector: tXCN;  // Introduced in HL7 2.7
    PatientResultsReleaseCat: tID;  // Introduced in HL7 2.7
    RootCause, LocalProcessControl: tCWE;  // Introduced in HL7 2.8
  end;

function OBX_Segment(message: THL7Message): THL7Segment;
procedure GetOBX(aSegment: THL7Segment; out OBXRecord: tOBX);
procedure GetOBX(message: THL7Message; out OBXRecord: tOBX);
procedure GetOBX(message: THL7Message; out SetID: tSI; out ValueType: tID;
  out ObsID: tCE; obsSubID: tST; out obsValue: ansistring; out Units: tCE;
  out RefRange: tST; AbnormFlags: tIS; out probability: tNM;
  out Nature, status: tID; out RRDate: tDTM; UDAC: tST; out ObsDateTime: tDTM;
  out prodID: tCE; respObs: tXCN; observMethod: tCE; EquipInstID: tEI;
  out AnalysisDateTime: tDTM);
  deprecated;
procedure GetOBX(message: THL7Message; out SetID: str4; out ValueType: str2;
  out ObsID: str250; obsSubID: str20; out obsValue: ansistring;
  out Units: str250; out RefRange: str60; AbnormFlags, probability: str5;
  out Nature: str2; out status: char; out RRDate: str26; UDAC: str20;
  out ObsDateTime: str26; out prodID, respObs, observMethod: str250;
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
procedure SetOBX(message: THL7Message; SetID: str4; ValueType: str2;
  ObsID: str250; obsSubID: str20; obsValue: ansistring; Units: str250;
  RefRange: str60; AbnormFlags, probability: str5; Nature: str2;
  status: char; RRDate: str26; UDAC: str20; ObsDateTime: str26;
  prodID, respObs, observMethod: str250; EquipInstID: str22; AnalysisDateTime: str26);
  deprecated;
procedure ClearOBX(var OBXRecord: tOBX);

implementation

function OBX_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(OBX_ID, '0')
  else
    Result := nil;
end;

procedure GetOBX(aSegment: THL7Segment; out OBXRecord: tOBX);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'OBX') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with OBXRecord do
      begin
        nextField := aSegment.FirstOccurrence.FirstField.nextSibling;
        SetID     := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ValueType := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObsID     := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        obsSubID  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        obsValue  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Units     := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        RefRange  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AbnormFlags := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        probability := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Nature    := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        status    := aSegment.FirstOccurrence.GetNextFieldContent(nextField)[1];
        RRDate    := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        UDAC      := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObsDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        prodID    := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        respObs   := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        observMethod := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EquipInstID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AnalysisDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObservationSite := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObservationInstanceID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MoodCode  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PerformingOrgName := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PerformingOrgAddr := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PerformingOrgMedicalDirector :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientResultsReleaseCat :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        RootCause :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        LocalProcessControl :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
    ClearOBX(OBXRecord);
end;

procedure GetOBX(message: THL7Message; out OBXRecord: tOBX);
var
  curSegment: THL7Segment;
begin
  curSegment := OBX_Segment(message);
  GetOBX(curSegment, OBXRecord);
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
  SetID     := OBXRecord.SetID;
  ValueType := OBXRecord.ValueType;
  ObsID     := OBXRecord.ObsID;
  obsSubID  := OBXRecord.obsSubID;
  obsValue  := OBXRecord.obsValue;
  Units     := OBXRecord.Units;
  RefRange  := OBXRecord.RefRange;
  AbnormFlags := OBXRecord.AbnormFlags;
  probability := OBXRecord.probability;
  Nature    := OBXRecord.Nature;
  status    := OBXRecord.status;
  RRDate    := OBXRecord.RRDate;
  UDAC      := OBXRecord.UDAC;
  ObsDateTime := OBXRecord.ObsDateTime;
  prodID    := OBXRecord.prodID;
  respObs   := OBXRecord.respObs;
  observMethod := OBXRecord.observMethod;
  EquipInstID := OBXRecord.EquipInstID;
  AnalysisDateTime := OBXRecord.AnalysisDateTime;
end;

procedure GetOBX(message: THL7Message; out SetID: str4; out ValueType: str2;
  out ObsID: str250; obsSubID: str20; out obsValue: ansistring;
  out Units: str250; out RefRange: str60; AbnormFlags, probability: str5;
  out Nature: str2; out status: char; out RRDate: str26; UDAC: str20;
  out ObsDateTime: str26; out prodID, respObs, observMethod: str250;
  EquipInstID: str22; out AnalysisDateTime: str26);
 { deprecated method, retained for backward-compatibility only, }
 { capsules new version of polymorphic GetOBX }
var
  OBXRecord: tOBX;
begin
  GetOBX(message, OBXRecord);
  SetID     := OBXRecord.SetID;
  ValueType := OBXRecord.ValueType;
  ObsID     := OBXRecord.ObsID;
  obsSubID  := OBXRecord.obsSubID;
  obsValue  := OBXRecord.obsValue;
  Units     := OBXRecord.Units;
  RefRange  := OBXRecord.RefRange;
  AbnormFlags := OBXRecord.AbnormFlags;
  probability := OBXRecord.probability;
  Nature    := OBXRecord.Nature;
  status    := OBXRecord.status[1];
  RRDate    := OBXRecord.RRDate;
  UDAC      := OBXRecord.UDAC;
  ObsDateTime := OBXRecord.ObsDateTime;
  prodID    := OBXRecord.prodID;
  respObs   := OBXRecord.respObs;
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
  FieldSep:   char;
  theString:  ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
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
      PatientResultsReleaseCat + FieldSep + RootCause + FieldSep +
      LocalProcessControl + FieldSep;
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
  OBXRecord.SetID     := SetID;
  OBXRecord.ValueType := ValueType;
  OBXRecord.ObsID     := ObsID;
  OBXRecord.obsSubID  := obsSubID;
  OBXRecord.obsValue  := obsValue;
  OBXRecord.Units     := Units;
  OBXRecord.RefRange  := RefRange;
  OBXRecord.AbnormFlags := AbnormFlags;
  OBXRecord.probability := probability;
  OBXRecord.Nature    := Nature;
  OBXRecord.status    := status;
  OBXRecord.RRDate    := RRDate;
  OBXRecord.UDAC      := UDAC;
  OBXRecord.ObsDateTime := ObsDateTime;
  OBXRecord.prodID    := prodID;
  OBXRecord.respObs   := respObs;
  OBXRecord.observMethod := observMethod;
  OBXRecord.EquipInstID := EquipInstID;
  OBXRecord.AnalysisDateTime := AnalysisDateTime;
  OBXRecord.ObservationSite := '';
  OBXRecord.ObservationInstanceID := '';
  OBXRecord.MoodCode  := '';
  OBXRecord.PerformingOrgName := '';
  OBXRecord.PerformingOrgAddr := '';
  OBXRecord.PerformingOrgMedicalDirector := '';
  OBXRecord.PatientResultsReleaseCat := '';
  SetOBX(message, OBXRecord);
end;

procedure SetOBX(message: THL7Message; SetID: str4; ValueType: str2;
  ObsID: str250; obsSubID: str20; obsValue: ansistring; Units: str250;
  RefRange: str60; AbnormFlags, probability: str5; Nature: str2;
  status: char; RRDate: str26; UDAC: str20; ObsDateTime: str26;
  prodID, respObs, observMethod: str250; EquipInstID: str22; AnalysisDateTime: str26);
 { deprecated method, retained for backward-compatibility only, }
 { capsules new version of polymorphic SetOBX }
var
  OBXRecord: tOBX;
begin
  OBXRecord.SetID     := SetID;
  OBXRecord.ValueType := ValueType;
  OBXRecord.ObsID     := ObsID;
  OBXRecord.obsSubID  := obsSubID;
  OBXRecord.obsValue  := obsValue;
  OBXRecord.Units     := Units;
  OBXRecord.RefRange  := RefRange;
  OBXRecord.AbnormFlags := AbnormFlags;
  OBXRecord.probability := probability;
  OBXRecord.Nature    := Nature;
  OBXRecord.status    := status;
  OBXRecord.RRDate    := RRDate;
  OBXRecord.UDAC      := UDAC;
  OBXRecord.ObsDateTime := ObsDateTime;
  OBXRecord.prodID    := prodID;
  OBXRecord.respObs   := respObs;
  OBXRecord.observMethod := observMethod;
  OBXRecord.EquipInstID := EquipInstID;
  OBXRecord.AnalysisDateTime := AnalysisDateTime;
  OBXRecord.ObservationSite := '';
  OBXRecord.ObservationInstanceID := '';
  OBXRecord.MoodCode  := '';
  OBXRecord.PerformingOrgName := '';
  OBXRecord.PerformingOrgAddr := '';
  OBXRecord.PerformingOrgMedicalDirector := '';
  OBXRecord.PatientResultsReleaseCat := '';
  SetOBX(message, OBXRecord);
end;

procedure ClearOBX(var OBXRecord: tOBX);
begin
  FillChar(OBXRecord, SizeOf(OBXRecord), 0);
end;

end.
