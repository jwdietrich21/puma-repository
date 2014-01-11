unit ERR;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for error segments }

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
  ERR_ID = 'ERR';

  E_MESS_ACC = '0';
  E_SEG_SEQ_ERR = '100';
  E_REQ_FLD_MIS = '101';
  E_DTA_TYP_ERR = '102';
  E_TBL_VAL_NFD = '103';
  E_VAL_TOO_LNG = '104';
  E_UNS_MSG_TYP = '200';
  E_UNS_EVT_COD = '201';
  E_UNS_PRC_ID = '202';
  E_UNS_VER_ID = '203';
  E_UNK_KEY_ID = '204';
  E_DUP_KEY_ID = '205';
  E_AOO_REC_LCK = '206';
  E_APP_INT_ERR = '207';

  SEV_WARNING = 'W';
  SEV_INFO = 'I';
  SEV_ERROR = 'E';
  SEV_FATAL = 'F';

type
  tERR = record
    ErrCodeLoc, ErrLoc, ErrCode: string;
    severity: char;
    appErrCode, appErrPar, DiagInfo, UserMessage: string;
    InformPersIndic, OverrideType, OverrideReason, HelpDeskContact: string;
  end;

function ERR_Segment(message: THL7Message): THL7Segment;
procedure GetERR(message: THL7Message; out ERRRecord: tERR);
procedure GetERR(message: THL7Message; out ErrCodeLoc, ErrLoc, ErrCode: string;
  out severity: char; out appErrCode, appErrPar, DiagInfo, UserMessage,
  InformPersIndic, OverrideType, OverrideReason, HelpDeskContact: string);
procedure SetERR(message: THL7Message; aSegment: THL7Segment);
procedure SetERR(message: THL7Message; ERRRecord: tERR);
procedure SetERR(message: THL7Message; ErrCodeLoc, ErrLoc, ErrCode: string;
  severity: char; appErrCode, appErrPar, DiagInfo, UserMessage,
  InformPersIndic, OverrideType, OverrideReason, HelpDeskContact: string);

implementation

function ERR_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(ERR_ID, '0')
  else
    Result := nil;
end;

procedure GetERR(message: THL7Message; out ERRRecord: tERR);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := ERR_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with ErrRecord do
      begin
        nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
        ErrCodeLoc := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ErrLoc := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ErrCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        severity := curSegment.FirstOccurrence.GetNextFieldContent(nextField)[1];
        appErrCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        appErrPar := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DiagInfo := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        UserMessage := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        InformPersIndic := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OverrideType := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OverrideReason := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        HelpDeskContact := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end;

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
  ErrCodeLoc := ERRRecord.ErrCodeLoc;
  ErrLoc := ERRRecord.ErrLoc;
  ErrCode := ERRRecord.ErrCode;
  severity := ERRRecord.severity;
  appErrCode := ERRRecord.appErrCode;
  appErrPar := ERRRecord.appErrPar;
  DiagInfo := ERRRecord.DiagInfo;
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
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
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

procedure SetERR(message: THL7Message; ErrCodeLoc, ErrLoc, ErrCode: string;
  severity: char; appErrCode, appErrPar, DiagInfo, UserMessage,
  InformPersIndic, OverrideType, OverrideReason, HelpDeskContact: string);
{ deprecated method, retained for backward-compatibility only, }
{ capsules new version of polymorphic SetERR }
var
  ERRRecord: tERR;
begin
  ERRRecord.ErrCodeLoc := ErrCodeLoc;
  ERRRecord.ErrLoc := ErrLoc;
  ERRRecord.ErrCode := ErrCode;
  ERRRecord.severity := severity;
  ERRRecord.appErrCode := appErrCode;
  ERRRecord.appErrPar := appErrPar;
  ERRRecord.DiagInfo := DiagInfo;
  ERRRecord.UserMessage := UserMessage;
  ERRRecord.InformPersIndic := InformPersIndic;
  ERRRecord.OverrideType := OverrideType;
  ERRRecord.OverrideReason := OverrideReason;
  ERRRecord.HelpDeskContact := HelpDeskContact;
  SetErr(message, ERRRecord);
end;

end.
