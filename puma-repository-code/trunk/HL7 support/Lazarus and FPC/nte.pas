unit NTE;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for notes and comments segment }

{ Version 1.6 }

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
  NTE_ID = 'NTE';

type
  tNTE = record
    SetID:     tSI;
    CommentSource: tID;
    comment:   tFT;
    commentType: tCE;
    EnteredBy: tXCN; // Introduced in HL7 2.7
    EnteredDateTime, EffectiveStartDate, ExpirationDate: tDTM; // Introduced in HL7 2.7
  end;

function NTE_Segment(message: THL7Message): THL7Segment;
procedure GetNTE(aSegment: THL7Segment; out NTERecord: tNTE);
procedure GetNTE(message: THL7Message; out NTERecord: tNTE);
procedure GetNTE(message: THL7Message; out SetID: tSI; out CommentSource: tID;
  out comment: tFT; out commentType: tCE);
  deprecated;
procedure GetNTE(message: THL7Message; out SetID: str4; out CommentSource: str8;
  out comment: ansistring; out commentType: str250);
  deprecated;
procedure SetNTE(message: THL7Message; aSegment: THL7Segment);
procedure SetNTE(message: THL7Message; NTERecord: tNTE);
procedure SetNTE(message: THL7Message; SetID: tSI; CommentSource: tID;
  comment: tFT; commentType: tCE);
  deprecated;
procedure SetNTE(message: THL7Message; SetID: str4; CommentSource: str8;
  comment: ansistring; commentType: str250);
  deprecated;
procedure ClearNTE(var NTERecord: tNTE);

implementation

function NTE_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(NTE_ID, '0')
  else
    Result := nil;
end;

procedure GetNTE(aSegment: THL7Segment; out NTERecord: tNTE);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'NTE') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with NTERecord do
      begin
        nextField := aSegment.FirstOccurrence.FirstField.nextSibling;
        SetID     := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CommentSource := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        comment   := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        commentType := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EnteredBy := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EnteredDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EffectiveStartDate := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ExpirationDate := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
    ClearNTE(NTERecord);
end;

procedure GetNTE(message: THL7Message; out NTERecord: tNTE);
var
  curSegment: THL7Segment;
begin
  curSegment := NTE_Segment(message);
  GetNTE(curSegment, NTERecord);
end;

procedure GetNTE(message: THL7Message; out SetID: tSI; out CommentSource: tID;
  out comment: tFT; out commentType: tCE);
 { deprecated method, retained for backward-compatibility only, }
 { capsules new version of polymorphic GetNTE }
var
  NTERecord: tNTE;
begin
  GetNTE(message, NTERecord);
  SetID   := NTERecord.SetID;
  CommentSource := NTERecord.CommentSource;
  comment := NTERecord.comment;
  commentType := NTERecord.commentType;
end;

procedure GetNTE(message: THL7Message; out SetID: str4; out CommentSource: str8;
  out comment: ansistring; out commentType: str250);
 { deprecated method, retained for backward-compatibility only, }
 { capsules new version of polymorphic GetNTE }
var
  NTERecord: tNTE;
begin
  GetNTE(message, NTERecord);
  SetID   := NTERecord.SetID;
  CommentSource := NTERecord.CommentSource;
  comment := NTERecord.comment;
  commentType := NTERecord.commentType;
end;

procedure SetNTE(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetNTE(message: THL7Message; NTERecord: tNTE);
var
  newSegment: THL7Segment;
  FieldSep:   char;
  theString:  ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with NTERecord do
    theString := NTE_ID + FieldSep + SetID + FieldSep + CommentSource +
      FieldSep + comment + FieldSep + commentType + FieldSep +
      EnteredBy + FieldSep + EnteredDateTime + FieldSep + EffectiveStartDate +
      FieldSep + ExpirationDate + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure SetNTE(message: THL7Message; SetID: tSI; CommentSource: tID;
  comment: tFT; commentType: tCE);
 { deprecated method, retained for backward-compatibility only, }
 { capsules new version of polymorphic SetNTE }
var
  NTERecord: tNTE;
begin
  NTERecord.SetID     := SetID;
  NTERecord.CommentSource := CommentSource;
  NTERecord.comment   := comment;
  NTERecord.commentType := commentType;
  NTERecord.EnteredBy := '';
  NTERecord.EnteredDateTime := '';
  NTERecord.EffectiveStartDate := '';
  NTERecord.ExpirationDate := '';
  SetNTE(message, NTERecord);
end;

procedure SetNTE(message: THL7Message; SetID: str4; CommentSource: str8;
  comment: ansistring; commentType: str250);
 { deprecated method, retained for backward-compatibility only, }
 { capsules new version of polymorphic SetNTE }
var
  NTERecord: tNTE;
begin
  NTERecord.SetID     := SetID;
  NTERecord.CommentSource := CommentSource;
  NTERecord.comment   := comment;
  NTERecord.commentType := commentType;
  NTERecord.EnteredBy := '';
  NTERecord.EnteredDateTime := '';
  NTERecord.EffectiveStartDate := '';
  NTERecord.ExpirationDate := '';
  SetNTE(message, NTERecord);
end;

procedure ClearNTE(var NTERecord: tNTE);
begin
  FillChar(NTERecord, SizeOf(NTERecord), 0);
end;

end.

