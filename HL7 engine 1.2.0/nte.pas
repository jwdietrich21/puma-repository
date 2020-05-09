unit NTE;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for notes and comments segment }

{ Version 1.2 }

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

const
  NTE_ID = 'NTE';

function NTE_Segment(message: THL7Message): THL7Segment;
procedure GetNTE(message: THL7Message; out SetID: str4; out CommentSource: str8;
  out comment: ansistring; out commentType: str250);
procedure SetNTE(message: THL7Message; aSegment: THL7Segment);
procedure SetNTE(message: THL7Message; SetID: str4; CommentSource: str8;
  comment: ansistring; commentType: str250);

implementation

function NTE_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(NTE_ID, '0')
  else
    Result := nil;
end;

procedure GetNTE(message: THL7Message; out SetID: str4; out CommentSource: str8;
  out comment: ansistring; out commentType: str250);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := NTE_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
    begin
      nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
      SetID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      CommentSource := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      comment := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      commentType := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
    end;
  end;
end;

procedure SetNTE(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetNTE(message: THL7Message; SetID: str4; CommentSource: str8;
  comment: ansistring; commentType: str250);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  theString := NTE_ID + FieldSep + SetID + FieldSep + CommentSource +
    FieldSep + comment + FieldSep + commentType + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;


end.

