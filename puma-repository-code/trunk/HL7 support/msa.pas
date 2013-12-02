unit MSA;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for message headers }

{ Version 0.9 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ Parser and compiler for HL7 messages }

{ Source code released under the BSD License }
{ See http://puma-repository.sf.net for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HL7;

function MSA_Segment(message: THL7Message): THL7Segment;
procedure GetMSA(message: THL7Message; out AckCode: str2; out controlID: str20;
  out textMessage: str80; out exSeqNum: Str15; out delAckType: char;
  out ErrorCond: Str250);
procedure SetMSA(message: THL7Message; aSegment: THL7Segment);
procedure SetMSA(message: THL7Message; AckCode: str2; controlID: str20;
  textMessage: str80; exSeqNum: Str15; delAckType: char; ErrorCond: Str250);

implementation

function MSA_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment('MSA', '0')
  else
    Result := nil;
end;

procedure GetMSA(message: THL7Message; out AckCode: str2; out controlID: str20;
  out textMessage: str80; out exSeqNum: Str15; out delAckType: char;
  out ErrorCond: Str250);
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
    begin
      nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
      AckCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      controlID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      textMessage := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      exSeqNum := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      delAckType := char(0); // deprecated field
      ErrorCond := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
    end;
  end;
end;

procedure SetMSA(message: THL7Message; aSegment: THL7Segment);
begin
  message.ReplaceSegment('MSA', '0', aSegment, True);
end;

procedure SetMSA(message: THL7Message; AckCode: str2; controlID: str20;
  textMessage: str80; exSeqNum: Str15; delAckType: char; ErrorCond: Str250);
var
  newSegment: THL7Segment;
  FieldSep: char;
  dateTime: str26;
  versionID: str60;
  theString: AnsiString;
begin
  newSegment := THL7Segment.Create(message, '');
  theString := 'MSA|' + AckCode + FieldSep + controlID +
    FieldSep + textMessage + FieldSep + exSeqNum + FieldSep +
    delAckType + FieldSep + ErrorCond + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

end.
