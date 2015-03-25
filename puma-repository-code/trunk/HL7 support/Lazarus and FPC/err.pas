unit ERR;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for error segments }

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
  ERR_ID = 'ERR';

  SEV_WARNING = 'W';
  SEV_INFO    = 'I';
  SEV_ERROR   = 'E';
  SEV_FATAL   = 'F';

type
  tERR = record
    ErrCodeLoc: tELD;
    ErrLoc:     tERL;
    ErrCode:    tCWE;
    severity:   tID;
    appErrCode: tCWE;
    appErrPar:  str80;
    DiagInfo, UserMessage: ansistring;
    InformPersIndic: tIS;
    OverrideType, OverrideReason: tCWE;
    HelpDeskContact: tXTN;
  end;

function ERR_Segment(message: THL7Message): THL7Segment;
procedure GetERR(aSegment: THL7Segment; out ERRRecord: tERR);
procedure GetERR(message: THL7Message; out ERRRecord: tERR);
procedure GetERR(message: THL7Message; out ErrCodeLoc: tELD; out ErrLoc: tERL;
  out ErrCode: tCWE; out severity: tID; out appErrCode: tCWE;
  out appErrPar: str80; DiagInfo, UserMessage: ansistring; InformPersIndic: tIS;
  OverrideType, OverrideReason: tCWE; HelpDeskContact: tXTN);
  deprecated;
procedure GetERR(message: THL7Message; out ErrCodeLoc, ErrLoc, ErrCode: string;
  out severity: char; out appErrCode, appErrPar, DiagInfo, UserMessage,
  InformPersIndic, OverrideType, OverrideReason, HelpDeskContact: string);
  deprecated;
procedure SetERR(message: THL7Message; aSegment: THL7Segment);
procedure SetERR(message: THL7Message; ERRRecord: tERR);
procedure SetERR(message: THL7Message; ErrCodeLoc: tELD; ErrLoc: tERL;
  ErrCode: tCWE; severity: tID; appErrCode: tCWE; appErrPar: str80;
  DiagInfo, UserMessage: ansistring; InformPersIndic: tIS;
  OverrideType, OverrideReason: tCWE; HelpDeskContact: tXTN);
  deprecated;
procedure SetERR(message: THL7Message; ErrCodeLoc, ErrLoc, ErrCode: string;
  severity: char; appErrCode, appErrPar, DiagInfo, UserMessage,
  InformPersIndic, OverrideType, OverrideReason, HelpDeskContact: string);
  deprecated;
procedure ClearERR(var ERRRecord: tERR);

implementation

function ERR_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(ERR_ID, '0')
  else
    Result := nil;
end;

procedure GetERR(aSegment: THL7Segment; out ERRRecord: tERR);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'ERR') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with ErrRecord do
      begin
        nextField   := aSegment.FirstOccurrence.FirstField.nextSibling;
        ErrCodeLoc  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ErrLoc      := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ErrCode     := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        severity    := aSegment.FirstOccurrence.GetNextFieldContent(nextField)[1];
        appErrCode  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        appErrPar   := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DiagInfo    := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        UserMessage := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        InformPersIndic := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OverrideType := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OverrideReason := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        HelpDeskContact := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
    ClearERR(ERRRecord);
end;

procedure GetERR(message: THL7Message; out ERRRecord: tERR);
var
  curSegment: THL7Segment;
begin
  curSegment := ERR_Segment(message);
  GetERR(curSegment, ERRRecord);
end;

procedure GetERR(message: THL7Message; out ErrCodeLoc: tELD; out ErrLoc: tERL;
  out ErrCode: tCWE; out severity: tID; out appErrCode: tCWE;
  out appErrPar: str80; DiagInfo, UserMessage: ansistring; InformPersIndic: tIS;
  OverrideType, OverrideReason: tCWE; HelpDeskContact: tXTN);
 { deprecated method, retained for backward-compatibility only, }
 { capsules new version of polymorphic GetERR }
var
  ERRRecord: tERR;
begin
  GetERR(message, ERRRecord);
  ErrCodeLoc  := ERRRecord.ErrCodeLoc;
  ErrLoc      := ERRRecord.ErrLoc;
  ErrCode     := ERRRecord.ErrCode;
  severity    := ERRRecord.severity;
  appErrCode  := ERRRecord.appErrCode;
  appErrPar   := ERRRecord.appErrPar;
  DiagInfo    := ERRRecord.DiagInfo;
  UserMessage := ERRRecord.UserMessage;
  InformPersIndic := ERRRecord.InformPersIndic;
  OverrideType := ERRRecord.OverrideType;
  OverrideReason := ERRRecord.OverrideReason;
  HelpDeskContact := ERRRecord.HelpDeskContact;
end;

procedure GetERR(message: THL7Message; out ErrCodeLoc, ErrLoc, ErrCode: string;
  out severity: char; out appErrCode, appErrPar, DiagInfo, UserMessage,
  InformPersIndic, OverrideType, OverrideReason, HelpDeskContact: string);
 { deprecated method, retained for backward-compatibility only, }
 { capsules new version of polymorphic GetERR }
var
  ERRRecord: tERR;
begin
  GetERR(message, ERRRecord);
  ErrCodeLoc  := ERRRecord.ErrCodeLoc;
  ErrLoc      := ERRRecord.ErrLoc;
  ErrCode     := ERRRecord.ErrCode;
  severity    := ERRRecord.severity[1];
  appErrCode  := ERRRecord.appErrCode;
  appErrPar   := ERRRecord.appErrPar;
  DiagInfo    := ERRRecord.DiagInfo;
  UserMessage := ERRRecord.UserMessage;
  InformPersIndic := ERRRecord.InformPersIndic;
  OverrideType := ERRRecord.OverrideType;
  OverrideReason := ERRRecord.OverrideReason;
  HelpDeskContact := ERRRecord.HelpDeskContact;
end;

procedure SetERR(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetERR(message: THL7Message; ERRRecord: tERR);
var
  newSegment: THL7Segment;
  FieldSep:   char;
  theString:  ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with ERRREcord do
    theString := ERR_ID + FieldSep + ErrCodeLoc + FieldSep + ErrLoc +
      FieldSep + ErrCode + FieldSep + severity + FieldSep + appErrCode +
      FieldSep + appErrPar + FieldSep + DiagInfo + FieldSep + UserMessage +
      FieldSep + InformPersIndic + FieldSep + OverrideType + FieldSep +
      OverrideReason + FieldSep + HelpDeskContact + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure SetERR(message: THL7Message; ErrCodeLoc: tELD; ErrLoc: tERL;
  ErrCode: tCWE; severity: tID; appErrCode: tCWE; appErrPar: str80;
  DiagInfo, UserMessage: ansistring; InformPersIndic: tIS;
  OverrideType, OverrideReason: tCWE; HelpDeskContact: tXTN);
 { deprecated method, retained for backward-compatibility only, }
 { capsules new version of polymorphic SetERR }
var
  ERRRecord: tERR;
begin
  ERRRecord.ErrCodeLoc  := ErrCodeLoc;
  ERRRecord.ErrLoc      := ErrLoc;
  ERRRecord.ErrCode     := ErrCode;
  ERRRecord.severity    := severity;
  ERRRecord.appErrCode  := appErrCode;
  ERRRecord.appErrPar   := appErrPar;
  ERRRecord.DiagInfo    := DiagInfo;
  ERRRecord.UserMessage := UserMessage;
  ERRRecord.InformPersIndic := InformPersIndic;
  ERRRecord.OverrideType := OverrideType;
  ERRRecord.OverrideReason := OverrideReason;
  ERRRecord.HelpDeskContact := HelpDeskContact;
  SetErr(message, ERRRecord);
end;

procedure SetERR(message: THL7Message; ErrCodeLoc, ErrLoc, ErrCode: string;
  severity: char; appErrCode, appErrPar, DiagInfo, UserMessage,
  InformPersIndic, OverrideType, OverrideReason, HelpDeskContact: string);
 { deprecated method, retained for backward-compatibility only, }
 { capsules new version of polymorphic SetERR }
var
  ERRRecord: tERR;
begin
  ERRRecord.ErrCodeLoc  := ErrCodeLoc;
  ERRRecord.ErrLoc      := ErrLoc;
  ERRRecord.ErrCode     := ErrCode;
  ERRRecord.severity    := severity;
  ERRRecord.appErrCode  := appErrCode;
  ERRRecord.appErrPar   := appErrPar;
  ERRRecord.DiagInfo    := DiagInfo;
  ERRRecord.UserMessage := UserMessage;
  ERRRecord.InformPersIndic := InformPersIndic;
  ERRRecord.OverrideType := OverrideType;
  ERRRecord.OverrideReason := OverrideReason;
  ERRRecord.HelpDeskContact := HelpDeskContact;
  SetErr(message, ERRRecord);
end;

procedure ClearERR(var ERRRecord: tERR);
begin
  FillChar(ERRRecord, SizeOf(ERRRecord), 0);
end;

end.
