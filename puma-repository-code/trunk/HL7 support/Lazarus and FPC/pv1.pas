unit PV1;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for patient visit segments }

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
  PV1_ID = 'PV1';

type
  tPV1 = record
    SetID:    tSI;
    PatientClass: tIS;
    AssignedPatientLocation: tPL;
    AdmissionType: tIS;
    PreadmitNumber: tCX;
    PriorPatientLocation: tPL;
    AttendingDoctor, ReferringDoctor, ConsultingDoctor: tXCN;
    HospitalService: tIS;
    TemporaryLocation: tPL;
    PreadmitTestIndicator, ReadmissionIndicator, AdmitSource: tIS;
    AmbulatoryStatus, VIPIndicator: tIS;
    AdmittingDoctor: tXCN;
    PatientType: tIS;
    VisitNumber: tCX;
    FinancialClass: tFC;
    ChargePriceIndicator, CourtesyCode: tIS;
    CreditRate, ContractCode: tIS;
    ContractEffectiveDate: tDT;
    ContractAmount, ContractPeriod: tNM;
    InterestCode, TransferToBadDeptCode: tIS;
    TransferToBadDeptDate: tDT;
    BadDeptAgencyCode: tIS;
    BadDeptTransferAmount, BadDeptRecoveryAmount: tNM;
    DeleteAccountIndicator: tIS;
    DeleteAccountDate: tDT;
    DischargeDisposition: tIS;
    DischargedToLocation: tDLD;
    DietType: tCE;
    ServicingFacility, BedStatus, AccountStatus: tIS;
    PendingLocation, PriorTemporaryLocation: tPL;
    AdmitDateTime, DischargeDateTime: tTS;
    CurrentPatientBalance, TotalCharges: tNM;
    TotalAdustments, TotalPayments: tNM;
    AlternateVisitID: tCX;
    VisitIndicator: tIS;
    OtherHealthcareProvider: tXCN;
    ServiceEpisodeDescription: tST; // Introduced in HL7 2.7
    ServiceEpisodeID: tCX; // Introduced in HL7 2.7
  end;

function PV1_Segment(message: THL7Message): THL7Segment;
procedure GetPV1(aSegment: THL7Segment; out PV1Record: tPV1);
procedure GetPV1(message: THL7Message; out PV1Record: tPV1);
procedure SetPV1(message: THL7Message; aSegment: THL7Segment);
procedure SetPV1(message: THL7message; PV1Record: tPV1);
procedure ClearPV1(var PV1Record: tPV1);

implementation

function PV1_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(PV1_ID, '0')
  else
    Result := nil;
end;

procedure GetPV1(aSegment: THL7Segment; out PV1Record: tPV1);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'PV1') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with PV1Record do
      begin
        nextField  := aSegment.FirstOccurrence.FirstField.nextSibling;
        SetID      := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientClass := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AssignedPatientLocation :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdmissionType := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PreadmitNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PriorPatientLocation :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AttendingDoctor := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReferringDoctor := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ConsultingDoctor := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        HospitalService := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TemporaryLocation := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PreadmitTestIndicator :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReadmissionIndicator :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdmitSource := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AmbulatoryStatus := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VIPIndicator := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdmittingDoctor := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientType := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FinancialClass := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ChargePriceIndicator :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CourtesyCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CreditRate := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContractCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContractEffectiveDate :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContractAmount := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContractPeriod := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        InterestCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransferToBadDeptCode :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransferToBadDeptDate :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BadDeptAgencyCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BadDeptTransferAmount :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BadDeptRecoveryAmount :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DeleteAccountIndicator :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DeleteAccountDate := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DischargeDisposition :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DischargedToLocation :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DietType   := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ServicingFacility := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BedStatus  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AccountStatus := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PendingLocation := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PriorTemporaryLocation :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdmitDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DischargeDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CurrentPatientBalance :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TotalCharges := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TotalAdustments := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TotalPayments := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AlternateVisitID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitIndicator := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OtherHealthcareProvider :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ServiceEpisodeDescription :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ServiceEpisodeID :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
    ClearPV1(PV1Record);
end;

procedure GetPV1(message: THL7Message; out PV1Record: tPV1);
var
  curSegment: THL7Segment;
begin
  curSegment := PV1_Segment(message);
  GetPV1(curSegment, PV1Record);
end;

procedure SetPV1(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetPV1(message: THL7message; PV1Record: tPV1);
var
  newSegment: THL7Segment;
  FieldSep:   char;
  theString:  ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with PV1Record do
    theString := PV1_ID + FieldSep + SetID + FieldSep + PatientClass +
      FieldSep + AssignedPatientLocation + FieldSep + AdmissionType +
      FieldSep + PreadmitNumber + FieldSep + PriorPatientLocation +
      FieldSep + AttendingDoctor + FieldSep + ReferringDoctor +
      FieldSep + ConsultingDoctor + FieldSep + HospitalService +
      FieldSep + TemporaryLocation + FieldSep + PreadmitTestIndicator +
      FieldSep + ReadmissionIndicator + FieldSep + AdmitSource +
      FieldSep + AmbulatoryStatus + FieldSep + VIPIndicator + FieldSep +
      AdmittingDoctor + FieldSep + PatientType + FieldSep + VisitNumber +
      FieldSep + FinancialClass + FieldSep + ChargePriceIndicator +
      FieldSep + CourtesyCode + FieldSep + CreditRate + FieldSep +
      ContractCode + FieldSep + ContractEffectiveDate + FieldSep +
      ContractAmount + FieldSep + ContractPeriod + FieldSep +
      InterestCode + FieldSep + TransferToBadDeptCode + FieldSep +
      TransferToBadDeptDate + FieldSep + BadDeptAgencyCode + FieldSep +
      BadDeptTransferAmount + FieldSep + BadDeptRecoveryAmount +
      FieldSep + DeleteAccountIndicator + FieldSep + DeleteAccountDate +
      FieldSep + DischargeDisposition + FieldSep + DischargedToLocation +
      FieldSep + DietType + FieldSep + ServicingFacility + FieldSep +
      BedStatus + FieldSep + AccountStatus + FieldSep + PendingLocation +
      FieldSep + PriorTemporaryLocation + FieldSep + AdmitDateTime +
      FieldSep + DischargeDateTime + FieldSep + CurrentPatientBalance +
      FieldSep + TotalCharges + FieldSep + TotalAdustments + FieldSep +
      TotalPayments + FieldSep + AlternateVisitID + FieldSep +
      VisitIndicator + FieldSep + OtherHealthcareProvider + FieldSep +
      FieldSep + ServiceEpisodeDescription + FieldSep + ServiceEpisodeID + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure ClearPV1(var PV1Record: tPV1);
begin
  FillChar(PV1Record, SizeOf(tPV1), 0);
end;

end.
