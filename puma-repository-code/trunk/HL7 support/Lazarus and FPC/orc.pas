unit ORC;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for Common Order Segments }

{ Version 1.7.0 }

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
  ORC_ID = 'ORC';

type
  tORC = record
    OrderControl: tID;
    PlacerOrderNumber, FillerOrderNumber, PlacerGroupNumber: tEI;
    OrderStatus: tID;
    ResponseFlag: tID;
    QuantityTiming: tCQ;
    Parent: tEIP;
    DateTimeofTransaction: tTS;
    EnteredBy, VerifiedBy, OrderingProvider: tXCN;
    EnterersLocation: tPL;
    CallBackPhoneNumber: tXTN;
    OrderEffectiveDateTime: tTS;
    OrderControlCodeReason: tCE;
    EnteringOrganization, EnteringDevice: tCE;
    ActionBy: tXCN;
  end;

function ORC_Segment(message: THL7Message): THL7Segment;
procedure GetORC(aSegment: THL7Segment; out ORCRecord: tORC);
procedure GetORC(message: THL7Message; out ORCRecord: tORC);
procedure SetORC(message: THL7Message; aSegment: THL7Segment);
procedure SetORC(message: THL7Message; ORCRecord: tORC);
procedure ClearORC(var ORCRecord: tORC);

implementation

function ORC_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(ORC_ID, '0')
  else
    Result := nil;
end;

procedure GetORC(aSegment: THL7Segment; out ORCRecord: tORC);
var
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'ORC') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with ORCRecord do
      begin
        nextField := aSegment.FirstOccurrence.FirstField.nextSibling;
        OrderControl := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlacerOrderNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FillerOrderNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlacerGroupNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderStatus := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ResponseFlag := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        QuantityTiming := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Parent := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DateTimeofTransaction := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EnteredBy := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VerifiedBy := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderingProvider := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EnterersLocation := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CallBackPhoneNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderEffectiveDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderControlCodeReason := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EnteringOrganization := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EnteringDevice := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ActionBy := aSegment.FirstOccurrence.GetNextFieldContent(nextField)
      end;
  end
  else
  ClearORC(ORCRecord);
end;

procedure GetORC(message: THL7Message; out ORCRecord: tORC);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := ORC_Segment(message);
  GetORC(curSegment, ORCRecord);
end;

procedure SetORC(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetORC(message: THL7Message; ORCRecord: tORC);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with ORCRecord do
  begin
    theString := ORC_ID + FieldSep + OrderControl + FieldSep + PlacerOrderNumber +
      FieldSep + FillerOrderNumber + FieldSep + PlacerGroupNumber + FieldSep + OrderStatus +
      FieldSep + ResponseFlag + FieldSep + QuantityTiming + FieldSep + Parent +
      FieldSep + DateTimeofTransaction + FieldSep + EnteredBy + FieldSep + VerifiedBy +
      FieldSep + OrderingProvider + FieldSep + EnterersLocation + FieldSep + CallBackPhoneNumber +
      FieldSep + OrderEffectiveDateTime + FieldSep + OrderControlCodeReason + FieldSep + EnteringOrganization +
      FieldSep + EnteringDevice + FieldSep + ActionBy;
  end;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure ClearORC(var ORCRecord: tORC);
begin
  FillChar(ORCRecord, SizeOf(ORCRecord), 0);
end;

end.
