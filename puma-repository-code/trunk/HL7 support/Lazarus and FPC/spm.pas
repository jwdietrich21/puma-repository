unit SPM;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for specimen segments }

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
  SPM_ID = 'SPM';

type
  tSPM = record
    SetID:      tSI;
    SpecimenID, SpecimenParentID: tEIP;
    SpecimenType, SpecimenTypeMod: tCWE;
    SpecimenAdditives, SpecimenCollectionMethods: tCWE;
    SpecimenSourceSite, SpecimenSourceSiteMod: tCWE;
    SpecimenCollectionSite, SpecimenRole: tCWE;
    SpecimenCollectionAmount: tCQ;
    GroupedSpecimenCount: tNM;
    SpecimenDescription: tST;
    SpecimenHandlingCode, SpecimenRiskCode: tCWE;
    SpecimenCollectionDateTime: tDR;
    SpecimenReceivedDateTime, SpecimenExpirDateTime: tDTM;
    SpecimenAvailability: tID;
    SpecimenRejectReason, SpecimenQuality: tCWE;
    SpecimenAppropriateness, SpecimenCondition: tCWE;
    SpecimenCurrentQuality: tCQ;
    NumberOfSpecimenContainers: tNM;
    ContainerType, ContainerCondition: tCWE;
    SpecimenChildRole: tCWE;
    AccessionID, otherSpecimenID: tCX;
    ShipmentID: tEI
  end;

function SPM_Segment(message: THL7Message): THL7Segment;
procedure GetSPM(aSegment: THL7Segment; out SPMRecord: tSPM);
procedure GetSPM(message: THL7Message; out SPMRecord: tSPM);
procedure SetSPM(message: THL7Message; aSegment: THL7Segment);
procedure SetSPM(message: THL7message; SPMRecord: tSPM);
procedure ClearSPM(var SPMRecord: tSPM);

implementation

function SPM_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(SPM_ID, '0')
  else
    Result := nil;
end;

procedure GetSPM(aSegment: THL7Segment; out SPMRecord: tSPM);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'SPM') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with SPMRecord do
      begin
        nextField  := aSegment.FirstOccurrence.FirstField.nextSibling;
        SetID      := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenParentID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenType := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenTypeMod := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenAdditives := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenCollectionMethods :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenSourceSite := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenSourceSiteMod :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenCollectionSite :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenRole := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenCollectionAmount :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        GroupedSpecimenCount :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenDescription := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenHandlingCode :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenRiskCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenCollectionDateTime :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenReceivedDateTime :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenExpirDateTime :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenAvailability :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenRejectReason :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenQuality := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenAppropriateness :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenCondition := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenCurrentQuality :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        NumberOfSpecimenContainers :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContainerType := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContainerCondition := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenChildRole := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AccessionID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        otherSpecimenID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ShipmentID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
    ClearSPM(SPMRecord);
end;

procedure GetSPM(message: THL7Message; out SPMRecord: tSPM);
var
  curSegment: THL7Segment;
begin
  curSegment := SPM_Segment(message);
  GetSPM(curSegment, SPMRecord);
end;

procedure SetSPM(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetSPM(message: THL7message; SPMRecord: tSPM);
var
  newSegment: THL7Segment;
  FieldSep:   char;
  theString:  ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with SPMRecord do
    theString := SPM_ID + FieldSep + SetID + FieldSep + SpecimenID +
      FieldSep + SpecimenParentID + FieldSep + SpecimenType + FieldSep +
      SpecimenTypeMod + FieldSep + SpecimenAdditives + FieldSep +
      SpecimenCollectionMethods + FieldSep + SpecimenSourceSite +
      FieldSep + SpecimenSourceSiteMod + FieldSep + SpecimenCollectionSite +
      FieldSep + SpecimenRole + FieldSep + SpecimenCollectionAmount +
      FieldSep + GroupedSpecimenCount + FieldSep + SpecimenDescription +
      FieldSep + SpecimenHandlingCode + FieldSep + SpecimenRiskCode +
      FieldSep + SpecimenCollectionDateTime + FieldSep + SpecimenReceivedDateTime +
      FieldSep + SpecimenExpirDateTime + FieldSep + SpecimenAvailability +
      FieldSep + SpecimenRejectReason + FieldSep + SpecimenQuality +
      FieldSep + SpecimenAppropriateness + FieldSep + SpecimenCondition +
      FieldSep + SpecimenCurrentQuality + FieldSep + NumberOfSpecimenContainers +
      FieldSep + ContainerType + FieldSep + ContainerCondition +
      FieldSep + SpecimenChildRole + FieldSep + AccessionID + FieldSep +
      otherSpecimenID + FieldSep + ShipmentID + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure ClearSPM(var SPMRecord: tSPM);
begin
  FillChar(SPMRecord, SizeOf(tSPM), 0);
end;

end.
