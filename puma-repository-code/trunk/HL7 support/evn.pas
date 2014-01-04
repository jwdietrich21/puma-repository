unit EVN;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for event type segments }

{ Version 1.3 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ Parser and compiler for HL7 messages }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://puma-repository.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HL7;

const
  EVN_ID = 'EVN';

function EVN_Segment(message: THL7Message): THL7Segment;
procedure GetEVN(message: THL7Message; out evtTypeCode: char;
    out recDateTime, plannedDateTime: tDTM; out reasonCode: tCWE;
    out opID: tXCN; out evtOccurred: tDTM; out evtFacility: tHD);
procedure SetEVN(message: THL7Message; aSegment: THL7Segment);
procedure SetEVN(message: THL7Message; evtTypeCode: char;
    recDateTime, plannedDateTime: tDTM; reasonCode: tCWE;
    opID: tXCN; evtOccurred: tDTM; evtFacility: tHD);

implementation

function EVN_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(EVN_ID, '0')
  else
    Result := nil;
end;

procedure GetEVN(message: THL7Message; out evtTypeCode: char; out recDateTime,
  plannedDateTime: tDTM; out reasonCode: tCWE; out opID: tXCN; out
  evtOccurred: tDTM; out evtFacility: tHD);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := EVN_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
    begin
      nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
      evtTypeCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField)[1];
      recDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      plannedDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      reasonCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      opID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      evtOccurred := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      evtFacility := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
    end;
  end;
end;

procedure SetEVN(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetEVN(message: THL7Message; evtTypeCode: char; recDateTime,
  plannedDateTime: tDTM; reasonCode: tCWE; opID: tXCN; evtOccurred: tDTM;
  evtFacility: tHD);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: AnsiString;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  theString := EVN_ID + FieldSep + evtTypeCode + FieldSep + recDateTime +
    FieldSep + plannedDateTime + FieldSep + reasonCode + FieldSep +
    opID + FieldSep + evtOccurred + FieldSep + evtFacility +
    FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

end.

