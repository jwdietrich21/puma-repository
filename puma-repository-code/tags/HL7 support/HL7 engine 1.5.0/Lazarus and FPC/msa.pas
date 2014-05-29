unit MSA;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for message acknowledgement segments }

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
  MSA_ID = 'MSA';

type
  tMSA = record
    AckCode: tID;
    controlID: str20;
    textMessage: str80;
    exSeqNum: tNM;
    delAckType: char;
    ErrorCond: tCE;
    MessageWaitingNumber: tNM; // Introduced in HL7 2.7
    MessageWaitingPriority: tID; // Introduced in HL7 2.7
  end;

function MSA_Segment(message: THL7Message): THL7Segment;
procedure GetMSA(message: THL7Message; out MSARecord: tMSA);
procedure GetMSA(message: THL7Message; out AckCode: tID; out controlID: str20;
  out textMessage: str80; out exSeqNum: tNM; out delAckType: char;
  out ErrorCond: tCE);
  deprecated;
procedure GetMSA(message: THL7Message; out AckCode: str2; out controlID: str20;
  out textMessage: str80; out exSeqNum: Str15; out delAckType: char;
  out ErrorCond: Str250);
  deprecated;
procedure SetMSA(message: THL7Message; aSegment: THL7Segment);
procedure SetMSA(message: THL7Message; MSARecord: tMSA);
procedure SetMSA(message: THL7Message; AckCode: tID; controlID: str20;
  textMessage: str80; exSeqNum: tNM; delAckType: char; ErrorCond: tCE);
  deprecated;
procedure SetMSA(message: THL7Message; AckCode: str2; controlID: str20;
  textMessage: str80; exSeqNum: Str15; delAckType: char; ErrorCond: Str250);
  deprecated;

implementation

function MSA_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(MSA_ID, '0')
  else
    Result := nil;
end;

procedure GetMSA(message: THL7Message; out MSARecord: tMSA);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := MSA_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with MSArecord do
      begin
        nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
        AckCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        controlID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        textMessage := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        exSeqNum := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        delAckType := char(0); // deprecated field
        ErrorCond := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MessageWaitingNumber := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MessageWaitingPriority := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end;
end;

procedure GetMSA(message: THL7Message; out AckCode: tID; out controlID: str20;
  out textMessage: str80; out exSeqNum: tNM; out delAckType: char;
  out ErrorCond: tCE);
{ deprecated method, maintained for backward-compatibility, capsules }
{ new version of polymorphic GetMSA }
var
  MSARecord: tMSA;
begin
  GetMSA(message, MSARecord);
  AckCode := MSARecord.AckCode;
  controlID := MSARecord.controlID;
  textMessage := MSARecord.textMessage;
  exSeqNum := MSARecord.exSeqNum;
  delAckType := MSARecord.delAckType;
  ErrorCond := MSARecord.ErrorCond;
end;

procedure GetMSA(message: THL7Message; out AckCode: str2; out controlID: str20;
  out textMessage: str80; out exSeqNum: Str15; out delAckType: char; out
  ErrorCond: Str250);
{ deprecated method, maintained for backward-compatibility, capsules }
{ new version of polymorphic GetMSA }
var
  MSARecord: tMSA;
begin
  GetMSA(message, MSARecord);
  AckCode := MSARecord.AckCode;
  controlID := MSARecord.controlID;
  textMessage := MSARecord.textMessage;
  exSeqNum := MSARecord.exSeqNum;
  delAckType := MSARecord.delAckType;
  ErrorCond := MSARecord.ErrorCond;
end;

procedure SetMSA(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetMSA(message: THL7Message; MSARecord: tMSA);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with MSArecord do
    theString := MSA_ID + FieldSep + AckCode + FieldSep + controlID +
    FieldSep + textMessage + FieldSep + exSeqNum + FieldSep +
    delAckType + FieldSep + ErrorCond + FieldSep + MessageWaitingNumber +
    FieldSep + MessageWaitingPriority + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure SetMSA(message: THL7Message; AckCode: tID; controlID: str20;
  textMessage: str80; exSeqNum: tNM; delAckType: char; ErrorCond: tCE);
{ deprecated method, maintained for backward-compatibility, capsules }
{ new version of polymorphic SetMSA }
var
  MSARecord: tMSA;
begin
  MSARecord.AckCode := AckCode;
  MSARecord.controlID := controlID;
  MSARecord.textMessage := textMessage;
  MSARecord.exSeqNum := exSeqNum;
  MSARecord.delAckType := delAckType;
  MSARecord.ErrorCond := ErrorCond;
  MSARecord.MessageWaitingNumber := '';
  MSARecord.MessageWaitingPriority := '';
  SetMSA(message, MSARecord);
end;

procedure SetMSA(message: THL7Message; AckCode: str2; controlID: str20;
  textMessage: str80; exSeqNum: Str15; delAckType: char; ErrorCond: Str250);
{ deprecated method, maintained for backward-compatibility, capsules }
{ new version of polymorphic SetMSA }
var
  MSARecord: tMSA;
begin
  MSARecord.AckCode := AckCode;
  MSARecord.controlID := controlID;
  MSARecord.textMessage := textMessage;
  MSARecord.exSeqNum := exSeqNum;
  MSARecord.delAckType := delAckType;
  MSARecord.ErrorCond := ErrorCond;
  MSARecord.MessageWaitingNumber := '';
  MSARecord.MessageWaitingPriority := '';
  SetMSA(message, MSARecord);
end;

end.
