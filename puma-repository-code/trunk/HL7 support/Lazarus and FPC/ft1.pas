unit FT1;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for financial transaction segments }

{ Version 1.7.0 (Hermes) }

{ (c) Johannes W. Dietrich, 1994 - 2015 }
{ (c) Marek Skorupski 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

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
  FT1_ID = 'FT1';

type
  tFT1 = record
    SetID: tSI; // Pole 1: Numer segmentu typu NTE
    TransactionID: tST;
    TransactionBatchID: tST;
    TransactionDate: tTS;
    TransactionPostingDate: tTS;
    TransactionType: tIS;
    TransactionCode: tCE;
    TransactionDescription: tST;
    TransactionDescriptionAlt: tST;
    TransactionQuantity: tNM;
    TransactionAmountExtended: tCP;
    TransactionAmountUnit: tCP;
    DepartmentCode: tCE;
    InsurancePlanID: tIS; // referred to as Health Plan ID in HL7 2.7+
    InsuranceAmount: tCP;
    AssignedPatientLocation: tPL;
    FeeSchedule: tIS;
    PatientType: tIS;
    DiagnosisCode: tCE;
    PerformedByCode: tXCN;
    OrderedByCode: tXCN;
    UnitCost: tCP;
    FillerOrderNumber: tEI;
    EnteredByCode: tXCN;
    ProcedureCode: tCE;
    ProcedureCodeModifier, AdvancedBeneficiaryNoticeCode: tCE;
    MedicallyNecessaryDuplicateProcedureReason, NDCCode: tCE;
    PaymentReferenceID: tCX;
    TransactionReferenceKey: tSI;
    PerformingFacility, OrderingFacility: tXON;
    ItemNumber: tCE;
    ModelNumber: tST;
    SpecialProcessingCode, ClinicCode: tCE;
    ReferralNumber, AuthorizationNumber: tCX;
    ServiceProviderTaxonomyCode, RevenueCode: tCE;
    PrescriptionNumber: tST;
    NDCQtyandUOM: tCQ;
  end;

function FT1_Segment(message: THL7Message): THL7Segment;
procedure GetFT1(aSegment: THL7Segment; out FT1Record: tFT1);
procedure GetFT1(message: THL7Message; out FT1Record: tFT1);
procedure SetFT1(message: THL7Message; aSegment: THL7Segment);
procedure SetFT1(message: THL7Message; FT1Record: tFT1);
procedure ClearFT1(var FT1Record: tFT1);

implementation

function FT1_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(FT1_ID, '0')
  else
    Result := nil;
end;

procedure GetFT1(aSegment: THL7Segment; out FT1Record: tFT1);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'FT1') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with FT1Record do
      begin
        nextField := aSegment.FirstOccurrence.FirstField.nextSibling;
        SetID     := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransactionID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransactionBatchID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransactionDate := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransactionPostingDate := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransactionType := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransactionCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransactionDescription := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransactionDescriptionAlt := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransactionQuantity := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransactionAmountExtended := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransactionAmountUnit := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DepartmentCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        InsurancePlanID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        InsuranceAmount := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AssignedPatientLocation := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FeeSchedule := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PatientType := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DiagnosisCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PerformedByCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderedByCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        UnitCost := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FillerOrderNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EnteredByCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ProcedureCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ProcedureCodeModifier := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdvancedBeneficiaryNoticeCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MedicallyNecessaryDuplicateProcedureReason := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        NDCCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PaymentReferenceID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TransactionReferenceKey := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PerformingFacility := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderingFacility := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ItemNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ModelNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecialProcessingCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ClinicCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReferralNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AuthorizationNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ServiceProviderTaxonomyCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        RevenueCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PrescriptionNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        NDCQtyandUOM := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
    ClearFT1(FT1Record);
end;

procedure GetFT1(message: THL7Message; out FT1Record: tFT1);
var
  curSegment: THL7Segment;
begin
  curSegment := FT1_Segment(message);
  GetFT1(curSegment, FT1Record);
end;

procedure SetFT1(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetFT1(message: THL7Message; FT1Record: tFT1);
var
  newSegment: THL7Segment;
  FieldSep:   char;
  theString:  ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with FT1Record do
    theString := FT1_ID + FieldSep + SetID + FieldSep + TransactionID +
      FieldSep + TransactionBatchID + FieldSep + TransactionDate + FieldSep +
      TransactionPostingDate + FieldSep + TransactionType + FieldSep + TransactionCode +
      FieldSep + TransactionDescription + FieldSep + TransactionDescriptionAlt +
      FieldSep + TransactionQuantity + FieldSep + TransactionAmountExtended +
      FieldSep + TransactionAmountUnit + FieldSep + DepartmentCode +
      FieldSep + InsurancePlanID + FieldSep + InsuranceAmount +
      FieldSep + AssignedPatientLocation + FieldSep + FeeSchedule +
      FieldSep + PatientType + FieldSep + DiagnosisCode +
      FieldSep + PerformedByCode + FieldSep + OrderedByCode +
      FieldSep + UnitCost + FieldSep + FillerOrderNumber +
      FieldSep + EnteredByCode + FieldSep + ProcedureCode + FieldSep +
      ProcedureCodeModifier + FieldSep +  AdvancedBeneficiaryNoticeCode +
      FieldSep + MedicallyNecessaryDuplicateProcedureReason + FieldSep +
      NDCCode + FieldSep + PaymentReferenceID + FieldSep +
      TransactionReferenceKey + FieldSep + PerformingFacility + FieldSep +
      OrderingFacility + FieldSep + ItemNumber + FieldSep +
      ModelNumber + FieldSep + SpecialProcessingCode + FieldSep +
      ClinicCode + FieldSep + ReferralNumber + FieldSep + AuthorizationNumber +
      FieldSep + ServiceProviderTaxonomyCode + FieldSep + RevenueCode +
      FieldSep + PrescriptionNumber + FieldSep + NDCQtyandUOM + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure ClearFT1(var FT1Record: tFT1);
begin
  FillChar(FT1Record, SizeOf(FT1Record), 0);
end;

end.

