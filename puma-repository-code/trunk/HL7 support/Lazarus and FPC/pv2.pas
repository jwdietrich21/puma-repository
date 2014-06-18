unit PV2;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for patient visit segments }

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
  PV2_ID = 'PV2';

type
  tPV2 = record
    PriorPendingLocation: tPL;
    AccommodationCode, AdmitReason, TransferReason: tCWE;
    PatientValuables, PatientValuablesLocation: tST;
    VisitUserCode: tCWE;
    ExpAdmitDateTime, ExpDischargeDateTime: tDTM;
    EstLOS, ActLOS: tNM;
    VisitDescription: tST;
    ReferralSourceCode: tXCN;
    PreviousServiceDate: tDT;
    EpmploymentIllnessRelatedID: tID;
    PurgeStatusCode: tCWE;
    PurgeStatusDate: tDT;
    SpecialProgramCode: tCWE;
    RetentionID: tID;
    ExpNumberOfInsurancePlans: tNM;
    VisitPublicityCode: tCWE;
    VisitProtectionID: tID;
    ClinicOrgName: tXON;
    PatientStatusCode, VisitPriorityCode: tCWE;
    PreviousTreatmentDate: tDT;
    ExpDischargeDispo: tCWE;
    SignatureOnFileDate, FirstSimilarIllnessDate: tDT;
    PatientChargeAdjustmentCode, RecurringServiceCode: tCWE;
    BillingMediaCode: tID;
    ExpSurgeryDateTime: tDTM;
    MilPartnershipCode, MilNonAvailCode: tID;
    NewbornBabyID, BabyDetainedID: tID;
    ModeOfArrivalCode, RecreationalDrugUseCode: tCWE;
    AdmissionLevelOfCareCode, PrecautionCode, PatientConditionCode: tCWE;
    LivingWillCode, OrganDonorCode, AdvanceDirectiveCode: tCWE;
    PatientStatusEffectiveDate: tCWE;
    ExpectedLOAReturnDateTime, ExpectedPreadmissionTestingDateTime: tDTM;
    NotifyClergyCode: tID;
    AdvanceDirectiveLastVerifiedDate: tDT; // Introduced in HL7 2.6
  end;

function PV2_Segment(message: THL7Message): THL7Segment;
procedure GetPV2(message: THL7Message; out PV2Record: tPV2);
procedure SetPV2(message: THL7Message; aSegment: THL7Segment);
procedure SetPV2(message: THL7message; PV2Record: tPV2);
procedure ClearPV2(var PV2Record: tPV2);

implementation

function PV2_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(PV2_ID, '0')
  else
    Result := nil;
end;

procedure GetPV2(message: THL7Message; out PV2Record: tPV2);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := PV2_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with PV2Record do
      begin
        nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
        PriorPendingLocation :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AccommodationCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdmitReason :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransferReason := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientValuables := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientValuablesLocation :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitUserCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpAdmitDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpDischargeDateTime :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EstLOS := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ActLOS := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitDescription := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReferralSourceCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PreviousServiceDate := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EpmploymentIllnessRelatedID :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PurgeStatusCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PurgeStatusDate := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecialProgramCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        RetentionID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpNumberOfInsurancePlans :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitPublicityCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitProtectionID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ClinicOrgName := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientStatusCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitPriorityCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PreviousTreatmentDate :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpDischargeDispo := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SignatureOnFileDate := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FirstSimilarIllnessDate :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientChargeAdjustmentCode :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        RecurringServiceCode :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BillingMediaCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpSurgeryDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MilPartnershipCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MilNonAvailCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        NewbornBabyID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BabyDetainedID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ModeOfArrivalCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        RecreationalDrugUseCode :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdmissionLevelOfCareCode :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PrecautionCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientConditionCode :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        LivingWillCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrganDonorCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdvanceDirectiveCode :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientStatusEffectiveDate :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpectedLOAReturnDateTime :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpectedPreadmissionTestingDateTime :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        NotifyClergyCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdvanceDirectiveLastVerifiedDate :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end;
end;

procedure SetPV2(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetPV2(message: THL7message; PV2Record: tPV2);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with PV2Record do
    theString := PV2_ID + FieldSep + PriorPendingLocation + FieldSep +
      AccommodationCode + FieldSep + AdmitReason + FieldSep + TransferReason +
      FieldSep + PatientValuables + FieldSep + PatientValuablesLocation +
      FieldSep + VisitUserCode + FieldSep + ExpAdmitDateTime +
      FieldSep + ExpDischargeDateTime + FieldSep + EstLOS + FieldSep +
      ActLOS + FieldSep + VisitDescription + FieldSep + ReferralSourceCode +
      FieldSep + PreviousServiceDate + FieldSep + EpmploymentIllnessRelatedID +
      FieldSep + PurgeStatusCode + FieldSep + PurgeStatusDate +
      FieldSep + SpecialProgramCode + FieldSep + RetentionID +
      FieldSep + ExpNumberOfInsurancePlans + FieldSep + VisitPublicityCode +
      FieldSep + VisitProtectionID + FieldSep + ClinicOrgName +
      FieldSep + PatientStatusCode + FieldSep + VisitPriorityCode +
      FieldSep + PreviousTreatmentDate + FieldSep + ExpDischargeDispo +
      FieldSep + SignatureOnFileDate + FieldSep + FirstSimilarIllnessDate +
      FieldSep + PatientChargeAdjustmentCode + FieldSep + RecurringServiceCode +
      FieldSep + BillingMediaCode + FieldSep + ExpSurgeryDateTime +
      FieldSep + MilPartnershipCode + FieldSep + MilNonAvailCode +
      FieldSep + NewbornBabyID + FieldSep + BabyDetainedID + FieldSep +
      ModeOfArrivalCode + FieldSep + RecreationalDrugUseCode + FieldSep +
      AdmissionLevelOfCareCode + FieldSep + PrecautionCode + FieldSep +
      PatientConditionCode + FieldSep + LivingWillCode + FieldSep +
      OrganDonorCode + FieldSep + AdvanceDirectiveCode + FieldSep +
      PatientStatusEffectiveDate + FieldSep + ExpectedLOAReturnDateTime +
      FieldSep + ExpectedPreadmissionTestingDateTime + FieldSep +
      NotifyClergyCode + FieldSep + AdvanceDirectiveLastVerifiedDate + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure ClearPV2(var PV2Record: tPV2);
begin
  FillChar(PV2Record, SizeOf(PV2Record), 0);
end;

end.
