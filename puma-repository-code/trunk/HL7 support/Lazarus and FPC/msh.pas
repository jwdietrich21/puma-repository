unit MSH;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for message headers }

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
{ http://puma-repository.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HL7;

type
  tMSH = record
    delimiters: str5;
    sendingApp, sendingFac, receivingApp, receivingFac: tHD;
    dateTime: tDTM;
    security: str40;
    messageType: tMSG;
    controlID: str20;
    processingID: tPT;
    versionID: tVID;
    sequenceNumber: tNM;
    continuationPointer: str180;
    AccAckType, AppAckType: tID;
    countryCode: tID;
    charSet: tID;
    messageLanguage: tCE;
    altCharHandlScheme: tID;
    profileID: tEI;
  end;

function MSH_Segment(message: THL7Message): THL7Segment;
procedure GetMSH(message: THL7Message; out MSHRecord: tMSH);
procedure GetMSH(message: THL7Message; out delimiters: str5;
  out sendingApp, sendingFac, receivingApp, receivingFac: tHD;
  out dateTime: tDTM; out security: str40; out messageType: tMSG;
  out controlID: str20; out processingID: tPT; out versionID: tVID;
  sequenceNumber: tNM; out continuationPointer: str180;
  out AccAckType, AppAckType: tID; out countryCode: tID; out charSet: tID;
  out messageLanguage: tCE; out altCharHandlScheme: tID; out profileID: tEI);
procedure GetMSH(message: THL7Message; out delimiters: str5;
  out sendingApp, sendingFac, receivingApp, receivingFac: str227;
  out dateTime: str26; out security: str40; out messageType: str15;
  out controlID: str20; out processingID: str3; out versionID: str60;
  sequenceNumber: str15; out continuationPointer: str180;
  out AccAckType, AppAckType: Str2; out countryCode: str3; out charSet: str16;
  out messageLanguage: str250; out altCharHandlScheme: str20; out profileID: str427);
procedure SetMSH(message: THL7Message; aSegment: THL7Segment);
procedure SetMSH(message: THL7Message; MSHRecord: tMSH; autoDate: boolean);
procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: tHD; security: str40;
  messageType: tMSG; processingID: tPT; sequenceNumber: tNM;
  continuationPointer: str180; AccAckType, AppAckType: tID;
  countryCode: tID; charSet: tID; messageLanguage: tCE;
  altCharHandlScheme: tID; profileID: tEI);
procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: tHD; dateTime: tDTM;
  security: str40; messageType: tMSG; controlID: str20; processingID: tPT;
  versionID: tVID; sequenceNumber: tNM; continuationPointer: str180;
  AccAckType, AppAckType: tID; countryCode: tID; charSet: tID;
  messageLanguage: tCE; altCharHandlScheme: tID; profileID: tEI);
procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: str227;
  security: str40; messageType: str15;
  processingID: str3; sequenceNumber: str15;
  continuationPointer: str180; AccAckType, AppAckType: Str2;
  countryCode: str3; charSet: str16; messageLanguage: str250;
  altCharHandlScheme: str20; profileID: str427);
procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: str227;
  dateTime: str26; security: str40; messageType: str15; controlID: str20;
  processingID: str3; versionID: str60; sequenceNumber: str15;
  continuationPointer: str180; AccAckType, AppAckType: Str2;
  countryCode: str3; charSet: str16; messageLanguage: str250;
  altCharHandlScheme: str20; profileID: str427);

implementation

function MSH_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(MSH_ID, '0')
  else
    Result := nil;
end;

procedure GetMSH(message: THL7Message; out MSHRecord: tMSH);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  MSHRecord.delimiters := message.CompiledDelimiters(message.Delimiters);
  curSegment := MSH_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with MSHRecord do
      begin
        nextField := curSegment.FirstOccurrence.FirstField.nextSibling.nextSibling;
        sendingApp := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        sendingFac := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        receivingApp := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        receivingFac := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        dateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        security := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        messageType := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        controlID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        processingID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        versionID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        sequenceNumber := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        continuationPointer := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AccAckType := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AppAckType := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        countryCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        charSet := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        messageLanguage := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        altCharHandlScheme := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        profileID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end;
end;

procedure GetMSH(message: THL7Message; out delimiters: str5;
  out sendingApp, sendingFac, receivingApp, receivingFac: tHD;
  out dateTime: tDTM; out security: str40; out messageType: tMSG;
  out controlID: str20; out processingID: tPT; out versionID: tVID;
  sequenceNumber: tNM; out continuationPointer: str180;
  out AccAckType, AppAckType: tID; out countryCode: tID; out charSet: tID;
  out messageLanguage: tCE; out altCharHandlScheme: tID; out profileID: tEI);
{ deprecated method, maintained for backward-compatibility only, }
{ capsules new version of polymorphic GetMSH }
var
  MSHRecord: tMSH;
begin
  GetMSH(message, MSHRecord);
  delimiters := MSHRecord.delimiters;
  sendingApp := MSHRecord.sendingApp;
  sendingFac := MSHRecord.sendingFac;
  receivingApp := MSHRecord.receivingApp;
  receivingFac := MSHRecord.receivingFac;
  dateTime := MSHRecord.dateTime;
  security := MSHRecord.security;
  messageType := MSHRecord.messageType;
  controlID := MSHRecord.controlID;
  processingID := MSHRecord.processingID;
  versionID := MSHRecord.versionID;
  sequenceNumber := MSHRecord.sequenceNumber;
  continuationPointer := MSHRecord.continuationPointer;
  AccAckType := MSHRecord.AccAckType;
  AppAckType := MSHRecord.AppAckType;
  countryCode := MSHRecord.countryCode;
  charSet := MSHRecord.charSet;
  messageLanguage := MSHRecord.messageLanguage;
  altCharHandlScheme := MSHRecord.altCharHandlScheme;
  profileID := MSHRecord.profileID;
end;

procedure GetMSH(message: THL7Message; out delimiters: str5; out sendingApp,
  sendingFac, receivingApp, receivingFac: str227; out dateTime: str26; out
  security: str40; out messageType: str15; out controlID: str20; out
  processingID: str3; out versionID: str60; sequenceNumber: str15; out
  continuationPointer: str180; out AccAckType, AppAckType: Str2; out
  countryCode: str3; out charSet: str16; out messageLanguage: str250; out
  altCharHandlScheme: str20; out profileID: str427);
{ deprecated method, maintained for backward-compatibility only, }
{ capsules new version of polymorphic GetMSH }
var
  MSHRecord: tMSH;
begin
  GetMSH(message, MSHRecord);
  delimiters := MSHRecord.delimiters;
  sendingApp := MSHRecord.sendingApp;
  sendingFac := MSHRecord.sendingFac;
  receivingApp := MSHRecord.receivingApp;
  receivingFac := MSHRecord.receivingFac;
  dateTime := MSHRecord.dateTime;
  security := MSHRecord.security;
  messageType := MSHRecord.messageType;
  controlID := MSHRecord.controlID;
  processingID := MSHRecord.processingID;
  versionID := MSHRecord.versionID;
  sequenceNumber := MSHRecord.sequenceNumber;
  continuationPointer := MSHRecord.continuationPointer;
  AccAckType := MSHRecord.AccAckType;
  AppAckType := MSHRecord.AppAckType;
  countryCode := MSHRecord.countryCode;
  charSet := MSHRecord.charSet;
  messageLanguage := MSHRecord.messageLanguage;
  altCharHandlScheme := MSHRecord.altCharHandlScheme;
  profileID := MSHRecord.profileID;
end;

procedure SetMSH(message: THL7Message; aSegment: THL7Segment);
begin
  message.ReplaceSegment(MSH_ID, '0', aSegment, True);
end;

procedure SetMSH(message: THL7Message; MSHRecord: tMSH; autoDate: boolean);
var
  newSegment: THL7Segment;
  FieldSep: char;
  autodateTime: tDTM;
  autocontrolID: str20;
  autoversionID: str60;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  if autoDate then
    autodateTime := EncodedDateTime(Now)
  else
    autodateTime := MSHRecord.dateTime;
  autoversionID := message.HL7Version;
  autocontrolID := message.ControlID;
  with MSHRecord do
    theString := MSH_ID + delimiters + FieldSep + sendingApp + FieldSep +
      sendingFac + FieldSep + receivingApp + FieldSep + receivingFac +
      FieldSep + autodateTime + FieldSep + security + FieldSep +
      messageType + FieldSep + autocontrolID + FieldSep + processingID +
      FieldSep + autoversionID + FieldSep + sequenceNumber + FieldSep +
      continuationPointer + FieldSep + AccAckType + FieldSep + AppAckType +
      FieldSep + countryCode + FieldSep + charSet + FieldSep +
      messageLanguage + FieldSep + altCharHandlScheme + FieldSep +
      profileID + FieldSep;
  newSegment.contentString := theString;
  message.ReplaceSegment(MSH_ID, '0', newSegment, True);
end;

procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: tHD; security: str40;
  messageType: tMSG; processingID: tPT; sequenceNumber: tNM;
  continuationPointer: str180; AccAckType, AppAckType: tID;
  countryCode: tID; charSet: tID; messageLanguage: tCE;
  altCharHandlScheme: tID; profileID: tEI);
{ deprecated method, maintained for backward-compatibility only, }
{ capsules new version of polymorphic SetMSH }
var
  MSHRecord: tMSH;
begin
  MSHRecord.delimiters := delimiters;
  MSHRecord.sendingApp := sendingApp;
  MSHRecord.sendingFac := sendingFac;
  MSHRecord.receivingApp := receivingApp;
  MSHRecord.receivingFac := receivingFac;
  MSHRecord.dateTime := EncodedDateTime(Now);
  MSHRecord.security := security;
  MSHRecord.messageType := messageType;
  MSHRecord.controlID := message.ControlID;
  MSHRecord.processingID := processingID;
  MSHRecord.versionID := message.HL7Version;
  MSHRecord.sequenceNumber := sequenceNumber;
  MSHRecord.continuationPointer := continuationPointer;
  MSHRecord.AccAckType := AccAckType;
  MSHRecord.AppAckType := AppAckType;
  MSHRecord.countryCode := countryCode;
  MSHRecord.charSet := charSet;
  MSHRecord.messageLanguage := messageLanguage;
  MSHRecord.altCharHandlScheme := altCharHandlScheme;
  MSHRecord.profileID := profileID;
  SetMSH(message, MSHRecord, true);
end;

procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: tHD; dateTime: tDTM;
  security: str40; messageType: tMSG; controlID: str20; processingID: tPT;
  versionID: tVID; sequenceNumber: tNM; continuationPointer: str180;
  AccAckType, AppAckType: tID; countryCode: tID; charSet: tID;
  messageLanguage: tCE; altCharHandlScheme: tID; profileID: tEI);
{ deprecated method, maintained for backward-compatibility only, }
{ capsules new version of polymorphic SetMSH }
var
  MSHRecord: tMSH;
begin
  MSHRecord.delimiters := delimiters;
  MSHRecord.sendingApp := sendingApp;
  MSHRecord.sendingFac := sendingFac;
  MSHRecord.receivingApp := receivingApp;
  MSHRecord.receivingFac := receivingFac;
  MSHRecord.dateTime := dateTime;
  MSHRecord.security := security;
  MSHRecord.messageType := messageType;
  MSHRecord.controlID := message.ControlID;
  MSHRecord.processingID := processingID;
  MSHRecord.versionID := message.HL7Version;
  MSHRecord.sequenceNumber := sequenceNumber;
  MSHRecord.continuationPointer := continuationPointer;
  MSHRecord.AccAckType := AccAckType;
  MSHRecord.AppAckType := AppAckType;
  MSHRecord.countryCode := countryCode;
  MSHRecord.charSet := charSet;
  MSHRecord.messageLanguage := messageLanguage;
  MSHRecord.altCharHandlScheme := altCharHandlScheme;
  MSHRecord.profileID := profileID;
  SetMSH(message, MSHRecord, false);
end;

procedure SetMSH(message: THL7Message; delimiters: str5; sendingApp,
  sendingFac, receivingApp, receivingFac: str227; security: str40;
  messageType: str15; processingID: str3; sequenceNumber: str15;
  continuationPointer: str180; AccAckType, AppAckType: Str2; countryCode: str3;
  charSet: str16; messageLanguage: str250; altCharHandlScheme: str20;
  profileID: str427);
{ deprecated method, maintained for backward-compatibility only, }
{ capsules new version of polymorphic SetMSH }
var
  MSHRecord: tMSH;
begin
  MSHRecord.delimiters := delimiters;
  MSHRecord.sendingApp := sendingApp;
  MSHRecord.sendingFac := sendingFac;
  MSHRecord.receivingApp := receivingApp;
  MSHRecord.receivingFac := receivingFac;
  MSHRecord.dateTime := EncodedDateTime(Now);
  MSHRecord.security := security;
  MSHRecord.messageType := messageType;
  MSHRecord.controlID := message.ControlID;
  MSHRecord.processingID := processingID;
  MSHRecord.versionID := message.HL7Version;
  MSHRecord.sequenceNumber := sequenceNumber;
  MSHRecord.continuationPointer := continuationPointer;
  MSHRecord.AccAckType := AccAckType;
  MSHRecord.AppAckType := AppAckType;
  MSHRecord.countryCode := countryCode;
  MSHRecord.charSet := charSet;
  MSHRecord.messageLanguage := messageLanguage;
  MSHRecord.altCharHandlScheme := altCharHandlScheme;
  MSHRecord.profileID := profileID;
  SetMSH(message, MSHRecord, true);
end;

procedure SetMSH(message: THL7Message; delimiters: str5; sendingApp,
  sendingFac, receivingApp, receivingFac: str227; dateTime: str26;
  security: str40; messageType: str15; controlID: str20; processingID: str3;
  versionID: str60; sequenceNumber: str15; continuationPointer: str180;
  AccAckType, AppAckType: Str2; countryCode: str3; charSet: str16;
  messageLanguage: str250; altCharHandlScheme: str20; profileID: str427);
{ deprecated method, maintained for backward-compatibility only, }
{ capsules new version of polymorphic SetMSH }
var
  MSHRecord: tMSH;
begin
  MSHRecord.delimiters := delimiters;
  MSHRecord.sendingApp := sendingApp;
  MSHRecord.sendingFac := sendingFac;
  MSHRecord.receivingApp := receivingApp;
  MSHRecord.receivingFac := receivingFac;
  MSHRecord.dateTime := dateTime;
  MSHRecord.security := security;
  MSHRecord.messageType := messageType;
  MSHRecord.controlID := message.ControlID;
  MSHRecord.processingID := processingID;
  MSHRecord.versionID := message.HL7Version;
  MSHRecord.sequenceNumber := sequenceNumber;
  MSHRecord.continuationPointer := continuationPointer;
  MSHRecord.AccAckType := AccAckType;
  MSHRecord.AppAckType := AppAckType;
  MSHRecord.countryCode := countryCode;
  MSHRecord.charSet := charSet;
  MSHRecord.messageLanguage := messageLanguage;
  MSHRecord.altCharHandlScheme := altCharHandlScheme;
  MSHRecord.profileID := profileID;
  SetMSH(message, MSHRecord, false);
end;

end.
