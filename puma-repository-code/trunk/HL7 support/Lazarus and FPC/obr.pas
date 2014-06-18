unit OBR;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for observation request segments }

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
  end;

function OBR_Segment(message: THL7Message): THL7Segment;
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

procedure GetOBR(message: THL7Message; out OBRRecord: tOBR);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := OBR_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with OBRRecord do
      begin
        nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
        SetID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlacOrdNumb := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FillOrdNumb := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        USI := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Priority := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReqDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObsDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObsEndDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CollectionVolume := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CollectorIdentifier := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenActionCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DangerCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        RelevantClinicalInfo :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenReceivedDateTime :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenSource := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderingProvider := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderCallbackPhoneNumber :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlacerField1 := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlacerField2 := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FillerField1 := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FillerField2 := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ResultsRptStatusChng :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ChargeToPractice := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DiagnosticServSectID :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ResultStatus := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ParentResult := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        QuantityTiming := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ResultCopiesTo := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Parent := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransportationMode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReasonForStudy := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PrincipalResultInterpreter :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AssistantResultInterpreter :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Technician := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Transcriptionist := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ScheduledDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        NumberOfSampleContainers := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransportLogisticsOfCollSampl := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CollectorsComment := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransportArrangementResponsibility := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransportArranged := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EscortRequired := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlannedPatientTransportComment := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ProcedureCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ProcedureCodeModifier := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlacerSupplServiceInfo := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FillerSupplServiceInfo := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MedicallyNecessaryDuplProcReason := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ResultHandling := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ParentUniversalServiceID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ObservationGroupID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ParentObservationGroupID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AltPlacerOrderNumber := curSegment.FirstOccurrence.GetNextFieldContent(nextField)
      end;
  end;
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
      ParentObservationGroupID + FieldSep + AltPlacerOrderNumber + FieldSep;
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
