unit SPM;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for specimen segments }

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
  SPM_ID = 'SPM';

type
  tSPM = record
    SetID: tSI;
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
procedure GetSPM(message: THL7Message; out SPMRecord: tSPM);
procedure SetSPM(message: THL7Message; aSegment: THL7Segment);
procedure SetSPM(message: THL7message; SPMRecord: tSPM);

implementation

function SPM_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(SPM_ID, '0')
  else
    Result := nil;
end;

procedure GetSPM(message: THL7Message; out SPMRecord: tSPM);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := SPM_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with SPMRecord do
      begin
        nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
        SetID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenParentID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenType := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenTypeMod := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenAdditives := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenCollectionMethods :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenSourceSite := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenSourceSiteMod :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenCollectionSite :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenRole := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenCollectionAmount :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        GroupedSpecimenCount :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenDescription := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenHandlingCode :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenRiskCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenCollectionDateTime :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenReceivedDateTime :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenExpirDateTime :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenAvailability :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenRejectReason :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenQuality := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenAppropriateness :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenCondition := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenCurrentQuality :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        NumberOfSpecimenContainers :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContainerType := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContainerCondition := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SpecimenChildRole := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AccessionID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        otherSpecimenID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ShipmentID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end;
end;

procedure SetSPM(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetSPM(message: THL7message; SPMRecord: tSPM);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
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
      FieldSep + ContainerType + FieldSep + ContainerCondition + FieldSep +
      SpecimenChildRole + FieldSep + AccessionID + FieldSep +
      otherSpecimenID + FieldSep + ShipmentID + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

end.
