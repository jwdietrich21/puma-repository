unit ERR;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for error segments }

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
  E_MESS_ACC = '0';
  E_SEG_SEQ_ERR = '100';
  E_REQ_FLD_MIS = '101';
  E_DTA_TYP_ERR = '102';
  E_TBL_VAL_NFD = '103';
  E_VAL_TOO_LNG = '104';
  E_UNS_MSG_TYP = '200';
  E_UNS_EVT_COD = '201';
  E_UNS_PRC_ID  = '202';
  E_UNS_VER_ID  = '203';
  E_UNK_KEY_ID  = '204';
  E_DUP_KEY_ID  = '205';
  E_AOO_REC_LCK = '206';
  E_APP_INT_ERR = '207';

  SEV_WARNING = 'W';
  SEV_INFO = 'I';
  SEV_ERROR = 'E';
  SEV_FATAL = 'F';

function ERR_Segment(message: THL7Message): THL7Segment;
procedure GetERR(message: THL7Message; out ErrCodeLoc, ErrLoc, ErrCode: string;
  out severity: char; out appErrCode, appErrPar, DiagInfo, UserMessage,
  InformPersIndic, OverrideType, OverrideReason, HelpDeskContact: string);
procedure SetERR(message: THL7Message; aSegment: THL7Segment);
procedure SetERR(message: THL7Message; ErrCodeLoc, ErrLoc, ErrCode: string;
  severity: char; appErrCode, appErrPar, DiagInfo, UserMessage,
  InformPersIndic, OverrideType, OverrideReason, HelpDeskContact: string);

implementation

function ERR_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment('ERR', '0')
  else
    Result := nil;
end;

procedure GetERR(message: THL7Message; out ErrCodeLoc, ErrLoc, ErrCode: string;
  out severity: char; out appErrCode, appErrPar, DiagInfo, UserMessage,
  InformPersIndic, OverrideType, OverrideReason, HelpDeskContact: string);
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

procedure SetERR(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetERR(message: THL7Message; ErrCodeLoc, ErrLoc, ErrCode: string;
  severity: char; appErrCode, appErrPar, DiagInfo, UserMessage,
  InformPersIndic, OverrideType, OverrideReason, HelpDeskContact: string);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  theString := 'ERR|' + ErrCodeLoc + FieldSep + ErrLoc + FieldSep +
    ErrCode + FieldSep + severity + FieldSep + appErrCode + FieldSep +
    appErrPar + FieldSep + DiagInfo + FieldSep + UserMessage + FieldSep +
    InformPersIndic + FieldSep + OverrideType + FieldSep + OverrideReason +
    FieldSep + HelpDeskContact + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

end.
