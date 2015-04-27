unit OBR;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for observation request segments }

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
  OBR_ID = 'OBR';

type
  tOBR = record
    SetID: tSI;
    PlacOrdNumb, FillOrdNumb: tEI;
    USI: tCE;  { Universal Service Identifier }
    Priority: tID;
    ReqDateTime, ObsDateTime, ObsEndDateTime: tDTM;
    CollectionVolume: tCQ;
    CollectorIdentifier: tXCN;
    SpecimenActionCode: tID;
    DangerCode: tCE;
    RelevantClinicalInfo: tST;
    SpecimenReceivedDateTime: tTS;
    SpecimenSource: tSPS;
    OrderingProvider: tXCN;
    OrderCallbackPhoneNumber: tXTN;
    PlacerField1, PlacerField2: ansistring;
    FillerField1, FillerField2: ansistring;
    ResultsRptStatusChng: tTS;
    ChargeToPractice: tMOC;
    DiagnosticServSectID, ResultStatus: tID;
    ParentResult: tPRL;
    QuantityTiming: tTQ;
    ResultCopiesTo: tXCN;
    Parent: tEIP;
    TransportationMode: tID;
    ReasonForStudy: tCE;
    PrincipalResultInterpreter, AssistantResultInterpreter: tNDL;
    Technician, Transcriptionist: tNDL;
    ScheduledDateTime: tTS;
    NumberOfSampleContainers: tNM;
    TransportLogisticsOfCollSampl, CollectorsComment: tCE;
    TransportArrangementResponsibility: tCE;
    TransportArranged, EscortRequired: tID;
    PlannedPatientTransportComment: tCE;
    ProcedureCode, ProcedureCodeModifier: tCE;
    PlacerSupplServiceInfo, FillerSupplServiceInfo: tCE;
    MedicallyNecessaryDuplProcReason: tCWE;
    ResultHandling: tIS;  // Introduced in HL7 2.7
    ParentUniversalServiceID: tCWE; // Introduced in HL7 2.7
    ObservationGroupID, ParentObservationGroupID: tEI; // Introduced in HL7 2.7
    AltPlacerOrderNumber: tCX; // Introduced in HL7 2.7
    ParentOrder: tEIP; // Introduced in HL7 2.7.1
  end;

function OBR_Segment(message: THL7Message): THL7Segment;
procedure GetOBR(aSegment: THL7Segment; out OBRRecord: tOBR);
procedure GetOBR(message: THL7Message; out OBRRecord: tOBR);
procedure GetOBR(message: THL7Message; out SetID: tSI;
  out PlacOrdNumb, FillOrdNumb: tEI; out USI: tCE; out Priority: tID;
  out ReqDateTime, ObsDateTime, ObsEndDateTime: tDTM);
  deprecated;
procedure GetOBR(message: THL7Message; out SetID: str4;
  out PlacOrdNumb, FillOrdNumb: str22; out USI: str250; out Priority: Str2;
  out ReqDateTime, ObsDateTime, ObsEndDateTime: str26);
  deprecated;
procedure SetOBR(message: THL7Message; aSegment: THL7Segment);
procedure SetOBR(message: THL7Message; OBRRecord: tOBR);
procedure SetOBR(message: THL7Message; SetID: tSI; PlacOrdNumb, FillOrdNumb: tEI;
  USI: tCE; Priority: tID; ReqDateTime, ObsDateTime, ObsEndDateTime: tDTM);
  deprecated;
procedure SetOBR(message: THL7Message; SetID: str4; PlacOrdNumb, FillOrdNumb: str22;
  USI: str250; Priority: Str2; ReqDateTime, ObsDateTime, ObsEndDateTime: str26);
  deprecated;
procedure ClearOBR(var OBRRecord: tOBR);

implementation

function OBR_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(OBR_ID, '0')
  else
    Result := nil;
end;

procedure GetOBR(aSegment: THL7Segment; out OBRRecord: tOBR);
var
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'OBR') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with OBRRecord do
      begin
        nextField := aSegment.FirstOccurrence.FirstField.nextSibling;
        SetID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlacOrdNumb := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FillOrdNumb := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        USI := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Priority := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReqDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObsDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObsEndDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CollectionVolume := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CollectorIdentifier := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenActionCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DangerCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        RelevantClinicalInfo :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenReceivedDateTime :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenSource := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderingProvider := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderCallbackPhoneNumber :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlacerField1 := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlacerField2 := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FillerField1 := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FillerField2 := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ResultsRptStatusChng :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ChargeToPractice := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DiagnosticServSectID :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ResultStatus := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ParentResult := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        QuantityTiming := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ResultCopiesTo := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Parent := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransportationMode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReasonForStudy := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PrincipalResultInterpreter :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AssistantResultInterpreter :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Technician := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Transcriptionist := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ScheduledDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        NumberOfSampleContainers := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransportLogisticsOfCollSampl := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CollectorsComment := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransportArrangementResponsibility := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransportArranged := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EscortRequired := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlannedPatientTransportComment := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ProcedureCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ProcedureCodeModifier := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlacerSupplServiceInfo := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FillerSupplServiceInfo := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MedicallyNecessaryDuplProcReason := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ResultHandling := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ParentUniversalServiceID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObservationGroupID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ParentObservationGroupID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AltPlacerOrderNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ParentOrder := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
  ClearOBR(OBRRecord);
end;

procedure GetOBR(message: THL7Message; out OBRRecord: tOBR);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := OBR_Segment(message);
  GetOBR(curSegment, OBRRecord);
end;

procedure GetOBR(message: THL7Message; out SetID: tSI;
  out PlacOrdNumb, FillOrdNumb: tEI; out USI: tCE; out Priority: tID;
  out ReqDateTime, ObsDateTime, ObsEndDateTime: tDTM);
{ deprecated method, retained for backward-compatibility only, }
{ capsules new version of polymorphic GetOBR }
var
  OBRRecord: tOBR;
begin
  GetOBR(message, OBRRecord);
  SetID := OBRRecord.SetID;
  PlacOrdNumb := OBRRecord.PlacOrdNumb;
  FillOrdNumb := OBRRecord.FillOrdNumb;
  USI := OBRRecord.USI;
  Priority := OBRRecord.Priority;
  ReqDateTime := OBRRecord.ReqDateTime;
  ObsDateTime := OBRRecord.ObsDateTime;
  ObsEndDateTime := OBRRecord.ObsEndDateTime;
end;

procedure GetOBR(message: THL7Message; out SetID: str4; out PlacOrdNumb,
  FillOrdNumb: str22; out USI: str250; out Priority: Str2; out ReqDateTime,
  ObsDateTime, ObsEndDateTime: str26);
{ deprecated method, retained for backward-compatibility only, }
{ capsules new version of polymorphic GetOBR }
var
  OBRRecord: tOBR;
begin
  GetOBR(message, OBRRecord);
  SetID := OBRRecord.SetID;
  PlacOrdNumb := OBRRecord.PlacOrdNumb;
  FillOrdNumb := OBRRecord.FillOrdNumb;
  USI := OBRRecord.USI;
  Priority := OBRRecord.Priority;
  ReqDateTime := OBRRecord.ReqDateTime;
  ObsDateTime := OBRRecord.ObsDateTime;
  ObsEndDateTime := OBRRecord.ObsEndDateTime;
end;

procedure SetOBR(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetOBR(message: THL7Message; OBRRecord: tOBR);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with OBRRecord do
  begin
    theString := OBR_ID + FieldSep + SetID + FieldSep + PlacOrdNumb +
      FieldSep + FillOrdNumb + FieldSep + USI + FieldSep + Priority +
      FieldSep + ReqDateTime + FieldSep + ObsDateTime + FieldSep + ObsEndDateTime;
    theString := theString + FieldSep + CollectionVolume + FieldSep +
      CollectorIdentifier + FieldSep + SpecimenActionCode + FieldSep +
      DangerCode + FieldSep + RelevantClinicalInfo + FieldSep +
      SpecimenReceivedDateTime + FieldSep + SpecimenSource + FieldSep +
      OrderingProvider + FieldSep + OrderCallbackPhoneNumber + FieldSep;
    theString := theString + PlacerField1 + FieldSep + PlacerField2 +
      FieldSep + FillerField1 + FieldSep + FillerField2 + FieldSep +
      ResultsRptStatusChng + FieldSep + ChargeToPractice + FieldSep +
      DiagnosticServSectID + FieldSep + ResultStatus + FieldSep +
      ParentResult + FieldSep + QuantityTiming + FieldSep;
    theString := theString + ResultCopiesTo + FieldSep + Parent +
      FieldSep + TransportationMode + FieldSep + ReasonForStudy + FieldSep +
      PrincipalResultInterpreter + FieldSep + AssistantResultInterpreter +
      FieldSep + Technician + FieldSep + Transcriptionist + FieldSep +
      ScheduledDateTime + FieldSep + NumberOfSampleContainers + FieldSep +
      TransportLogisticsOfCollSampl + FieldSep + CollectorsComment + FieldSep +
      TransportArrangementResponsibility + FieldSep + TransportArranged +
      FieldSep + EscortRequired + FieldSep + PlannedPatientTransportComment +
      FieldSep + ProcedureCode + FieldSep + ProcedureCodeModifier + FieldSep +
      PlacerSupplServiceInfo + FieldSep + FillerSupplServiceInfo + FieldSep +
      MedicallyNecessaryDuplProcReason + FieldSep + ResultHandling + FieldSep +
      ParentUniversalServiceID + FieldSep + ObservationGroupID + FieldSep +
      ParentObservationGroupID + FieldSep + AltPlacerOrderNumber + FieldSep +
      ParentOrder + FieldSep;
  end;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure SetOBR(message: THL7Message; SetID: tSI; PlacOrdNumb, FillOrdNumb: tEI;
  USI: tCE; Priority: tID; ReqDateTime, ObsDateTime, ObsEndDateTime: tDTM);
{ deprecated method, retained for backward-compatibility only, }
{ capsules new version of polymorphic SetOBR }
var
  OBRRecord: tOBR;
begin
  OBRRecord.SetID := SetID;
  OBRRecord.PlacOrdNumb := PlacOrdNumb;
  OBRRecord.FillOrdNumb := FillOrdNumb;
  OBRRecord.USI := USI;
  OBRRecord.Priority := Priority;
  OBRRecord.ReqDateTime := ReqDateTime;
  OBRRecord.ObsDateTime := ObsDateTime;
  OBRRecord.ObsEndDateTime := ObsEndDateTime;
  OBRRecord.CollectionVolume := '';
  OBRRecord.CollectorIdentifier := '';
  OBRRecord.SpecimenActionCode := '';
  OBRRecord.DangerCode := '';
  OBRRecord.RelevantClinicalInfo := '';
  OBRRecord.SpecimenReceivedDateTime := '';
  OBRRecord.SpecimenSource := '';
  OBRRecord.OrderingProvider := '';
  OBRRecord.OrderCallbackPhoneNumber := '';
  OBRRecord.PlacerField1 := '';
  OBRRecord.PlacerField2 := '';
  OBRRecord.FillerField1 := '';
  OBRRecord.FillerField2 := '';
  OBRRecord.ResultsRptStatusChng := '';
  OBRRecord.ChargeToPractice := '';
  OBRRecord.DiagnosticServSectID := '';
  OBRRecord.ResultStatus := '';
  OBRRecord.ParentResult := '';
  OBRRecord.QuantityTiming := '';
  OBRRecord.ResultCopiesTo := '';
  OBRRecord.Parent := '';
  OBRRecord.TransportationMode := '';
  OBRRecord.ReasonForStudy := '';
  OBRRecord.PrincipalResultInterpreter := '';
  OBRRecord.AssistantResultInterpreter := '';
  OBRRecord.Technician := '';
  OBRRecord.Transcriptionist := '';
  OBRRecord.ScheduledDateTime := '';
  OBRRecord.NumberOfSampleContainers := '';
  OBRRecord.TransportLogisticsOfCollSampl := '';
  OBRRecord.CollectorsComment := '';
  OBRRecord.TransportArrangementResponsibility := '';
  OBRRecord.TransportArranged := '';
  OBRRecord.EscortRequired := '';
  OBRRecord.PlannedPatientTransportComment := '';
  OBRRecord.ProcedureCode := '';
  OBRRecord.ProcedureCodeModifier := '';
  OBRRecord.PlacerSupplServiceInfo := '';
  OBRRecord.FillerSupplServiceInfo := '';
  OBRRecord.MedicallyNecessaryDuplProcReason := '';
  OBRRecord.ResultHandling := '';
  OBRRecord.ParentUniversalServiceID := '';
  OBRRecord.ObservationGroupID := '';
  OBRRecord.ParentObservationGroupID := '';
  OBRRecord.AltPlacerOrderNumber := '';
  SetOBR(message, OBRRecord);
end;

procedure SetOBR(message: THL7Message; SetID: str4; PlacOrdNumb,
  FillOrdNumb: str22; USI: str250; Priority: Str2; ReqDateTime, ObsDateTime,
  ObsEndDateTime: str26);
{ deprecated method, retained for backward-compatibility only, }
{ capsules new version of polymorphic SetOBR }
var
  OBRRecord: tOBR;
begin
  OBRRecord.SetID := SetID;
  OBRRecord.PlacOrdNumb := PlacOrdNumb;
  OBRRecord.FillOrdNumb := FillOrdNumb;
  OBRRecord.USI := USI;
  OBRRecord.Priority := Priority;
  OBRRecord.ReqDateTime := ReqDateTime;
  OBRRecord.ObsDateTime := ObsDateTime;
  OBRRecord.ObsEndDateTime := ObsEndDateTime;
  OBRRecord.CollectionVolume := '';
  OBRRecord.CollectorIdentifier := '';
  OBRRecord.SpecimenActionCode := '';
  OBRRecord.DangerCode := '';
  OBRRecord.RelevantClinicalInfo := '';
  OBRRecord.SpecimenReceivedDateTime := '';
  OBRRecord.SpecimenSource := '';
  OBRRecord.OrderingProvider := '';
  OBRRecord.OrderCallbackPhoneNumber := '';
  OBRRecord.PlacerField1 := '';
  OBRRecord.PlacerField2 := '';
  OBRRecord.FillerField1 := '';
  OBRRecord.FillerField2 := '';
  OBRRecord.ResultsRptStatusChng := '';
  OBRRecord.ChargeToPractice := '';
  OBRRecord.DiagnosticServSectID := '';
  OBRRecord.ResultStatus := '';
  OBRRecord.ParentResult := '';
  OBRRecord.QuantityTiming := '';
  OBRRecord.ResultCopiesTo := '';
  OBRRecord.Parent := '';
  OBRRecord.TransportationMode := '';
  OBRRecord.ReasonForStudy := '';
  OBRRecord.PrincipalResultInterpreter := '';
  OBRRecord.AssistantResultInterpreter := '';
  OBRRecord.Technician := '';
  OBRRecord.Transcriptionist := '';
  OBRRecord.ScheduledDateTime := '';
  OBRRecord.NumberOfSampleContainers := '';
  OBRRecord.TransportLogisticsOfCollSampl := '';
  OBRRecord.CollectorsComment := '';
  OBRRecord.TransportArrangementResponsibility := '';
  OBRRecord.TransportArranged := '';
  OBRRecord.EscortRequired := '';
  OBRRecord.PlannedPatientTransportComment := '';
  OBRRecord.ProcedureCode := '';
  OBRRecord.ProcedureCodeModifier := '';
  OBRRecord.PlacerSupplServiceInfo := '';
  OBRRecord.FillerSupplServiceInfo := '';
  OBRRecord.MedicallyNecessaryDuplProcReason := '';
  OBRRecord.ResultHandling := '';
  OBRRecord.ParentUniversalServiceID := '';
  OBRRecord.ObservationGroupID := '';
  OBRRecord.ParentObservationGroupID := '';
  OBRRecord.AltPlacerOrderNumber := '';
  SetOBR(message, OBRRecord);
end;

procedure ClearOBR(var OBRRecord: tOBR);
begin
  FillChar(OBRRecord, SizeOf(OBRRecord), 0);
end;

end.
