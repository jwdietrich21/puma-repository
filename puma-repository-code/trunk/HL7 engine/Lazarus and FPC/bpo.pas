unit BPO;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for Blood Product Order segments }

{ Version 2.0.0 (Hermes) }

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
  BPO_ID = 'BPO';

type
  tBPO = record
    SetID: tSI;
    BPUniversalServiceIdentifier: tCE;
    BPProcessingRequirements: tCE;
    BPQuantity: tNM;
    BPAmount: tNM;
    BPUnits: tCE;
    BPIntendedUseDateTime: tTS; // DTM in HL7 2.7+ (equivalent to tTS)
    BPIntendedDispenseFromLocation: tPL;
    BPIntendedDispenseFromAddress: tXAD;
    BPRequestedDispenseDateTime: tTS; // DTM in HL7 2.7+ (equivalent to tTS)
    BPRequestedDispenseToLocation: tPL;
    BPRequestedDispenseToAddress: tXAD;
    BPIndicationforUse: tCE;
    BPInformedConsentIndicator: tID;
  end;

function BPO_Segment(message: THL7Message): THL7Segment;
procedure GetBPO(aSegment: THL7Segment; out BPORecord: tBPO);
procedure GetBPO(message: THL7Message; out BPORecord: tBPO);
procedure SetBPO(message: THL7Message; aSegment: THL7Segment);
procedure SetBPO(message: THL7Message; BPORecord: tBPO);
procedure ClearBPO(var BPORecord: tBPO);

implementation

function BPO_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(BPO_ID, '0')
  else
    Result := nil;
end;

procedure GetBPO(aSegment: THL7Segment; out BPORecord: tBPO);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'BPO') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with BPORecord do
      begin
        nextField := aSegment.FirstOccurrence.FirstField.nextSibling;
        SetID     := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPUniversalServiceIdentifier := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPProcessingRequirements := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPQuantity := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPAmount := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPUnits := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPIntendedUseDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPIntendedDispenseFromLocation := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPIntendedDispenseFromAddress := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPRequestedDispenseDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPRequestedDispenseToLocation := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPRequestedDispenseToAddress := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPIndicationforUse := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BPInformedConsentIndicator := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
    ClearBPO(BPORecord);
end;

procedure GetBPO(message: THL7Message; out BPORecord: tBPO);
var
  curSegment: THL7Segment;
begin
  curSegment := BPO_Segment(message);
  GetBPO(curSegment, BPORecord);
end;

procedure SetBPO(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetBPO(message: THL7Message; BPORecord: tBPO);
var
  newSegment: THL7Segment;
  FieldSep:   char;
  theString:  ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with BPORecord do
    theString := BPO_ID + FieldSep + SetID + FieldSep + BPUniversalServiceIdentifier +
      FieldSep + BPProcessingRequirements + FieldSep + BPQuantity + FieldSep + BPAmount +
      FieldSep + BPUnits + FieldSep + BPIntendedUseDateTime + FieldSep + BPIntendedDispenseFromLocation +
      FieldSep + BPIntendedDispenseFromAddress + FieldSep + BPRequestedDispenseDateTime +
      FieldSep + BPRequestedDispenseToLocation + FieldSep + BPRequestedDispenseToAddress +
      FieldSep + BPIndicationforUse + FieldSep + BPInformedConsentIndicator + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure ClearBPO(var BPORecord: tBPO);
begin
  FillChar(BPORecord, SizeOf(BPORecord), 0);
end;

end.

