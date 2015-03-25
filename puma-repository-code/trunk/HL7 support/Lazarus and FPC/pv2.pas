unit PV2;

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
  PV2_ID = 'PV2';

type
  tPV2 = record
    PriorPendingLocation: tPL;
    AccommodationCode, AdmitReason, TransferReason: tCWE;
    PatientValuables, PatientValuablesLocation: tST;
    VisitUserCode:    tCWE;
    ExpAdmitDateTime, ExpDischargeDateTime: tDTM;
    EstLOS, ActLOS:   tNM;
    VisitDescription: tST;
    ReferralSourceCode: tXCN;
    PreviousServiceDate: tDT;
    EpmploymentIllnessRelatedID: tID;
    PurgeStatusCode:  tCWE;
    PurgeStatusDate:  tDT;
    SpecialProgramCode: tCWE;
    RetentionID:      tID;
    ExpNumberOfInsurancePlans: tNM;
    VisitPublicityCode: tCWE;
    VisitProtectionID: tID;
    ClinicOrgName:    tXON;
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
procedure GetPV2(aSegment: THL7Segment; out PV2Record: tPV2);
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

procedure GetPV2(aSegment: THL7Segment; out PV2Record: tPV2);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'PV2') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with PV2Record do
      begin
        nextField   := aSegment.FirstOccurrence.FirstField.nextSibling;
        PriorPendingLocation :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AccommodationCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdmitReason :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransferReason := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientValuables := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientValuablesLocation :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitUserCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpAdmitDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpDischargeDateTime :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EstLOS      := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ActLOS      := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitDescription := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReferralSourceCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PreviousServiceDate := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EpmploymentIllnessRelatedID :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PurgeStatusCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PurgeStatusDate := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecialProgramCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        RetentionID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpNumberOfInsurancePlans :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitPublicityCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitProtectionID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ClinicOrgName := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientStatusCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VisitPriorityCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PreviousTreatmentDate :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpDischargeDispo := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SignatureOnFileDate := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FirstSimilarIllnessDate :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientChargeAdjustmentCode :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        RecurringServiceCode :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BillingMediaCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpSurgeryDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MilPartnershipCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MilNonAvailCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        NewbornBabyID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BabyDetainedID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ModeOfArrivalCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        RecreationalDrugUseCode :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdmissionLevelOfCareCode :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PrecautionCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientConditionCode :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        LivingWillCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrganDonorCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdvanceDirectiveCode :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientStatusEffectiveDate :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpectedLOAReturnDateTime :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpectedPreadmissionTestingDateTime :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        NotifyClergyCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdvanceDirectiveLastVerifiedDate :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
    ClearPV2(PV2Record);
end;

procedure GetPV2(message: THL7Message; out PV2Record: tPV2);
var
  curSegment: THL7Segment;
begin
  curSegment := PV2_Segment(message);
  GetPV2(curSegment, PV2Record);
end;

procedure SetPV2(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetPV2(message: THL7message; PV2Record: tPV2);
var
  newSegment: THL7Segment;
  FieldSep:   char;
  theString:  ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with PV2Record do
    theString := PV2_ID + FieldSep + PriorPendingLocation + FieldSep +
      AccommodationCode + FieldSep + AdmitReason + FieldSep +
      TransferReason + FieldSep + PatientValuables + FieldSep +
      PatientValuablesLocation + FieldSep + VisitUserCode + FieldSep +
      ExpAdmitDateTime + FieldSep + ExpDischargeDateTime + FieldSep +
      EstLOS + FieldSep + ActLOS + FieldSep + VisitDescription +
      FieldSep + ReferralSourceCode + FieldSep + PreviousServiceDate +
      FieldSep + EpmploymentIllnessRelatedID + FieldSep + PurgeStatusCode +
      FieldSep + PurgeStatusDate + FieldSep + SpecialProgramCode +
      FieldSep + RetentionID + FieldSep + ExpNumberOfInsurancePlans +
      FieldSep + VisitPublicityCode + FieldSep + VisitProtectionID +
      FieldSep + ClinicOrgName + FieldSep + PatientStatusCode +
      FieldSep + VisitPriorityCode + FieldSep + PreviousTreatmentDate +
      FieldSep + ExpDischargeDispo + FieldSep + SignatureOnFileDate +
      FieldSep + FirstSimilarIllnessDate + FieldSep + PatientChargeAdjustmentCode +
      FieldSep + RecurringServiceCode + FieldSep + BillingMediaCode +
      FieldSep + ExpSurgeryDateTime + FieldSep + MilPartnershipCode +
      FieldSep + MilNonAvailCode + FieldSep + NewbornBabyID + FieldSep +
      BabyDetainedID + FieldSep + ModeOfArrivalCode + FieldSep +
      RecreationalDrugUseCode + FieldSep + AdmissionLevelOfCareCode +
      FieldSep + PrecautionCode + FieldSep + PatientConditionCode +
      FieldSep + LivingWillCode + FieldSep + OrganDonorCode + FieldSep +
      AdvanceDirectiveCode + FieldSep + PatientStatusEffectiveDate +
      FieldSep + ExpectedLOAReturnDateTime + FieldSep +
      ExpectedPreadmissionTestingDateTime + FieldSep + NotifyClergyCode +
      FieldSep + AdvanceDirectiveLastVerifiedDate + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure ClearPV2(var PV2Record: tPV2);
begin
  FillChar(PV2Record, SizeOf(PV2Record), 0);
end;

end.
