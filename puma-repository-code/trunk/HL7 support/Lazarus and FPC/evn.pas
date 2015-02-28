unit EVN;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for event type segments }

{ Version 1.7.0 }

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
  EVN_ID = 'EVN';

type
  tEVN = record
    evtTypeCode: char;
    recDateTime, plannedDateTime: tDTM;
    reasonCode: tCWE;
    opID: tXCN;
    evtOccurred: tDTM;
    evtFacility: tHD;
  end;

function EVN_Segment(message: THL7Message): THL7Segment;
procedure GetEVN(aSegment: THL7Segment; out EVNRecord: tEVN);
procedure GetEVN(message: THL7Message; out EVNRecord: tEVN);
procedure GetEVN(message: THL7Message; out evtTypeCode: char;
  out recDateTime, plannedDateTime: tDTM; out reasonCode: tCWE;
  out opID: tXCN; out evtOccurred: tDTM; out evtFacility: tHD);
  deprecated;
procedure SetEVN(message: THL7Message; aSegment: THL7Segment);
procedure SetEVN(message: THL7message; EVNRecord: tEVN);
procedure SetEVN(message: THL7Message; evtTypeCode: char;
  recDateTime, plannedDateTime: tDTM; reasonCode: tCWE; opID: tXCN;
  evtOccurred: tDTM; evtFacility: tHD);
  deprecated;
procedure ClearEVN(var EVNRecord: tEVN);

implementation

function EVN_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(EVN_ID, '0')
  else
    Result := nil;
end;

procedure GetEVN(aSegment: THL7Segment; out EVNRecord: tEVN);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'EVN') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with EVNRecord do
      begin
        nextField := aSegment.FirstOccurrence.FirstField.nextSibling;
        evtTypeCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField)[1];
        recDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        plannedDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        reasonCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        opID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        evtOccurred := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        evtFacility := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
    ClearEVN(EVNRecord);
end;

procedure GetEVN(message: THL7Message; out EVNRecord: tEVN);
var
  curSegment: THL7Segment;
begin
  curSegment := EVN_Segment(message);
  GetEVN(curSegment, EVNRecord);
end;

procedure GetEVN(message: THL7Message; out evtTypeCode: char;
  out recDateTime, plannedDateTime: tDTM; out reasonCode: tCWE;
  out opID: tXCN; out evtOccurred: tDTM; out evtFacility: tHD);
 { deprecated method, retained for backward-compatibility only, }
 { capsules new version of polymorphic GetEVN }
var
  EVNRecord: tEVN;
begin
  GetEVN(message, EVNRecord);
  evtTypeCode := EVNRecord.evtTypeCode;
  recDateTime := EVNRecord.recDateTime;
  plannedDateTime := EVNRecord.plannedDateTime;
  reasonCode := EVNRecord.reasonCode;
  opID := EVNRecord.opID;
  evtOccurred := EVNRecord.evtOccurred;
  evtFacility := EVNRecord.evtFacility;
end;

procedure SetEVN(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetEVN(message: THL7message; EVNRecord: tEVN);
var
  newSegment: THL7Segment;
  FieldSep:   char;
  theString:  ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with EVNRecord do
    theString := EVN_ID + FieldSep + evtTypeCode + FieldSep + recDateTime +
      FieldSep + plannedDateTime + FieldSep + reasonCode + FieldSep +
      opID + FieldSep + evtOccurred + FieldSep + evtFacility + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure SetEVN(message: THL7Message; evtTypeCode: char;
  recDateTime, plannedDateTime: tDTM; reasonCode: tCWE; opID: tXCN;
  evtOccurred: tDTM; evtFacility: tHD);
 { deprecated method, retained for backward-compatibility only, }
 { capsules new version of polymorphic SetEVN }
var
  EVNRecord: tEVN;
begin
  EVNRecord.evtTypeCode := evtTypeCode;
  EVNRecord.recDateTime := recDateTime;
  EVNRecord.plannedDateTime := plannedDateTime;
  EVNRecord.reasonCode := reasonCode;
  EVNRecord.opID := opID;
  EVNRecord.evtOccurred := evtOccurred;
  EVNRecord.evtFacility := evtFacility;
  SetEVN(message, EVNRecord);
end;

procedure ClearEVN(var EVNRecord: tEVN);
begin
  FillChar(EVNRecord, SizeOf(EVNRecord), 0);
end;

end.


