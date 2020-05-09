unit PV1;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for patient visit segments }

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
  PV1_ID = 'PV1';

type
  tPV1 = record
    SetID: tSI;
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
procedure GetPV1(message: THL7Message; out PV1Record: tPV1);
procedure SetPV1(message: THL7Message; aSegment: THL7Segment);
procedure SetPV1(message: THL7message; PV1Record: tPV1);

implementation

function PV1_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(PV1_ID, '0')
  else
    Result := nil;
end;

procedure GetPV1(message: THL7Message; out PV1Record: tPV1);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := PV1_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with PV1Record do
      begin
        nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
        SetID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientClass := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AssignedPatientLocation :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdmissionType := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PreadmitNumber := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PriorPatientLocation :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AttendingDoctor := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReferringDoctor := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ConsultingDoctor := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        HospitalService := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TemporaryLocation := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PreadmitTestIndicator :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReadmissionIndicator :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdmitSource := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AmbulatoryStatus := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VIPIndicator := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdmittingDoctor := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientType := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitNumber := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FinancialClass := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ChargePriceIndicator :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CourtesyCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CreditRate := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContractCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContractEffectiveDate :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContractAmount := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContractPeriod := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        InterestCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransferToBadDeptCode :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransferToBadDeptDate :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BadDeptAgencyCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BadDeptTransferAmount :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BadDeptRecoveryAmount :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DeleteAccountIndicator :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DeleteAccountDate := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DischargeDisposition :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DischargedToLocation :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DietType := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ServicingFacility := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BedStatus := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AccountStatus := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PendingLocation := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PriorTemporaryLocation :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdmitDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DischargeDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CurrentPatientBalance :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TotalCharges := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TotalAdustments := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TotalPayments := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AlternateVisitID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitIndicator := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OtherHealthcareProvider :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ServiceEpisodeDescription :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ServiceEpisodeID :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end;
end;

procedure SetPV1(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetPV1(message: THL7message; PV1Record: tPV1);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with PV1Record do
    theString := PV1_ID + FieldSep + SetID + FieldSep + PatientClass +
      FieldSep + AssignedPatientLocation + FieldSep + AdmissionType +
      FieldSep + PreadmitNumber + FieldSep + PriorPatientLocation + FieldSep +
      AttendingDoctor + FieldSep + ReferringDoctor + FieldSep + ConsultingDoctor +
      FieldSep + HospitalService + FieldSep + TemporaryLocation + FieldSep +
      PreadmitTestIndicator + FieldSep + ReadmissionIndicator + FieldSep +
      AdmitSource + FieldSep + AmbulatoryStatus + FieldSep + VIPIndicator +
      FieldSep + AdmittingDoctor + FieldSep + PatientType + FieldSep +
      VisitNumber + FieldSep + FinancialClass + FieldSep + ChargePriceIndicator +
      FieldSep + CourtesyCode + FieldSep + CreditRate + FieldSep +
      ContractCode + FieldSep + ContractEffectiveDate + FieldSep +
      ContractAmount + FieldSep + ContractPeriod + FieldSep + InterestCode +
      FieldSep + TransferToBadDeptCode + FieldSep + TransferToBadDeptDate +
      FieldSep + BadDeptAgencyCode + FieldSep + BadDeptTransferAmount +
      FieldSep + BadDeptRecoveryAmount + FieldSep + DeleteAccountIndicator +
      FieldSep + DeleteAccountDate + FieldSep + DischargeDisposition +
      FieldSep + DischargedToLocation + FieldSep + DietType + FieldSep +
      ServicingFacility + FieldSep + BedStatus + FieldSep + AccountStatus +
      FieldSep + PendingLocation + FieldSep + PriorTemporaryLocation +
      FieldSep + AdmitDateTime + FieldSep + DischargeDateTime + FieldSep +
      CurrentPatientBalance + FieldSep + TotalCharges + FieldSep +
      TotalAdustments + FieldSep + TotalPayments + FieldSep + AlternateVisitID +
      FieldSep + VisitIndicator + FieldSep + OtherHealthcareProvider + FieldSep
      + FieldSep + ServiceEpisodeDescription + FieldSep + ServiceEpisodeID +
      FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

end.
