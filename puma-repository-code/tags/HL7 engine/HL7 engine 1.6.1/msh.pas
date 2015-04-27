unit MSH;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for message headers }

{ Version 1.6.1 }

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
    delimiters:   str5;
    sendingApp, sendingFac, receivingApp, receivingFac: tHD;
    dateTime:     tDTM;
    security:     str40;
    messageType:  tMSG;
    controlID:    str20;
    processingID: tPT;
    versionID:    tVID;
    sequenceNumber: tNM;
    continuationPointer: str180;
    AccAckType, AppAckType: tID;
    countryCode:  tID;
    charSet:      tID;
    messageLanguage: tCE;
    altCharHandlScheme: tID;
    profileID:    tEI;
    SendingRespOrg, ReceivingRespOrt: tXON;  // Introduced in HL7 2.7
    SendingNetworkAddr, ReceivingNetworkAddr: tHD; // Introduced in HL7 2.7
  end;

function MSH_Segment(message: THL7Message): THL7Segment;
procedure GetMSH(aSegment: THL7Segment; out MSHRecord: tMSH);
procedure GetMSH(message: THL7Message; out MSHRecord: tMSH);
procedure GetMSH(message: THL7Message; out delimiters: str5;
  out sendingApp, sendingFac, receivingApp, receivingFac: tHD;
  out dateTime: tDTM; out security: str40; out messageType: tMSG;
  out controlID: str20; out processingID: tPT; out versionID: tVID;
  sequenceNumber: tNM; out continuationPointer: str180;
  out AccAckType, AppAckType: tID; out countryCode: tID; out charSet: tID;
  out messageLanguage: tCE; out altCharHandlScheme: tID; out profileID: tEI);
  deprecated;
procedure GetMSH(message: THL7Message; out delimiters: str5;
  out sendingApp, sendingFac, receivingApp, receivingFac: str227;
  out dateTime: str26; out security: str40; out messageType: str15;
  out controlID: str20; out processingID: str3; out versionID: str60;
  sequenceNumber: str15; out continuationPointer: str180;
  out AccAckType, AppAckType: Str2; out countryCode: str3; out charSet: str16;
  out messageLanguage: str250; out altCharHandlScheme: str20; out profileID: str427);
  deprecated;
procedure SetMSH(message: THL7Message; aSegment: THL7Segment);
procedure SetMSH(message: THL7Message; MSHRecord: tMSH; autoDate: boolean);
procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: tHD; security: str40;
  messageType: tMSG; processingID: tPT; sequenceNumber: tNM;
  continuationPointer: str180; AccAckType, AppAckType: tID; countryCode: tID;
  charSet: tID; messageLanguage: tCE; altCharHandlScheme: tID; profileID: tEI);
  deprecated;
procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: tHD; dateTime: tDTM;
  security: str40; messageType: tMSG; controlID: str20; processingID: tPT;
  versionID: tVID; sequenceNumber: tNM; continuationPointer: str180;
  AccAckType, AppAckType: tID; countryCode: tID; charSet: tID;
  messageLanguage: tCE; altCharHandlScheme: tID; profileID: tEI);
  deprecated;
procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: str227;
  security: str40; messageType: str15; processingID: str3; sequenceNumber: str15;
  continuationPointer: str180; AccAckType, AppAckType: Str2; countryCode: str3;
  charSet: str16; messageLanguage: str250; altCharHandlScheme: str20; profileID: str427);
  deprecated;
procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: str227;
  dateTime: str26; security: str40; messageType: str15; controlID: str20;
  processingID: str3; versionID: str60; sequenceNumber: str15;
  continuationPointer: str180; AccAckType, AppAckType: Str2;
  countryCode: str3; charSet: str16; messageLanguage: str250;
  altCharHandlScheme: str20; profileID: str427);
  deprecated;
procedure ClearMSH(var MSHRecord: tMSH);

implementation

function MSH_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(MSH_ID, '0')
  else
    Result := nil;
end;

procedure GetMSH(aSegment: THL7Segment; out MSHRecord: tMSH);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'MSH') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with MSHRecord do
      begin
        nextField    := aSegment.FirstOccurrence.FirstField.nextSibling.nextSibling;
        sendingApp   := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        sendingFac   := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        receivingApp := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        receivingFac := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        dateTime     := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        security     := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        messageType  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        controlID    := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        processingID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        versionID    := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        sequenceNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        continuationPointer := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AccAckType   := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AppAckType   := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        countryCode  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        charSet      := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        messageLanguage := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        altCharHandlScheme := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        profileID    := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SendingRespOrg := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReceivingRespOrt := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        SendingNetworkAddr := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ReceivingNetworkAddr := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
  ClearMSH(MSHRecord);
end;

procedure GetMSH(message: THL7Message; out MSHRecord: tMSH);
var
  curSegment: THL7Segment;
begin
  MSHRecord.delimiters := message.CompiledDelimiters(message.Delimiters);
  curSegment := MSH_Segment(message);
  GetMSH(curSegment, MSHRecord);
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
  delimiters   := MSHRecord.delimiters;
  sendingApp   := MSHRecord.sendingApp;
  sendingFac   := MSHRecord.sendingFac;
  receivingApp := MSHRecord.receivingApp;
  receivingFac := MSHRecord.receivingFac;
  dateTime     := MSHRecord.dateTime;
  security     := MSHRecord.security;
  messageType  := MSHRecord.messageType;
  controlID    := MSHRecord.controlID;
  processingID := MSHRecord.processingID;
  versionID    := MSHRecord.versionID;
  sequenceNumber := MSHRecord.sequenceNumber;
  continuationPointer := MSHRecord.continuationPointer;
  AccAckType   := MSHRecord.AccAckType;
  AppAckType   := MSHRecord.AppAckType;
  countryCode  := MSHRecord.countryCode;
  charSet      := MSHRecord.charSet;
  messageLanguage := MSHRecord.messageLanguage;
  altCharHandlScheme := MSHRecord.altCharHandlScheme;
  profileID    := MSHRecord.profileID;
end;

procedure GetMSH(message: THL7Message; out delimiters: str5;
  out sendingApp, sendingFac, receivingApp, receivingFac: str227;
  out dateTime: str26; out security: str40; out messageType: str15;
  out controlID: str20; out processingID: str3; out versionID: str60;
  sequenceNumber: str15; out continuationPointer: str180;
  out AccAckType, AppAckType: Str2; out countryCode: str3; out charSet: str16;
  out messageLanguage: str250; out altCharHandlScheme: str20; out profileID: str427);
 { deprecated method, maintained for backward-compatibility only, }
 { capsules new version of polymorphic GetMSH }
var
  MSHRecord: tMSH;
begin
  GetMSH(message, MSHRecord);
  delimiters   := MSHRecord.delimiters;
  sendingApp   := MSHRecord.sendingApp;
  sendingFac   := MSHRecord.sendingFac;
  receivingApp := MSHRecord.receivingApp;
  receivingFac := MSHRecord.receivingFac;
  dateTime     := MSHRecord.dateTime;
  security     := MSHRecord.security;
  messageType  := MSHRecord.messageType;
  controlID    := MSHRecord.controlID;
  processingID := MSHRecord.processingID;
  versionID    := MSHRecord.versionID;
  sequenceNumber := MSHRecord.sequenceNumber;
  continuationPointer := MSHRecord.continuationPointer;
  AccAckType   := MSHRecord.AccAckType;
  AppAckType   := MSHRecord.AppAckType;
  countryCode  := MSHRecord.countryCode;
  charSet      := MSHRecord.charSet;
  messageLanguage := MSHRecord.messageLanguage;
  altCharHandlScheme := MSHRecord.altCharHandlScheme;
  profileID    := MSHRecord.profileID;
end;

procedure SetMSH(message: THL7Message; aSegment: THL7Segment);
begin
  message.ReplaceSegment(MSH_ID, '0', aSegment, True);
end;

procedure SetMSH(message: THL7Message; MSHRecord: tMSH; autoDate: boolean);
var
  newSegment:    THL7Segment;
  FieldSep:      char;
  autodateTime:  tDTM;
  autocontrolID: str20;
  autoversionID: str60;
  theString:     ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
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
      profileID + FieldSep + SendingRespOrg + FieldSep + ReceivingRespOrt +
      FieldSep + SendingNetworkAddr + FieldSep + ReceivingNetworkAddr + FieldSep;
  newSegment.contentString := theString;
  message.ReplaceSegment(MSH_ID, '0', newSegment, True);
end;

procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: tHD; security: str40;
  messageType: tMSG; processingID: tPT; sequenceNumber: tNM;
  continuationPointer: str180; AccAckType, AppAckType: tID; countryCode: tID;
  charSet: tID; messageLanguage: tCE; altCharHandlScheme: tID; profileID: tEI);
 { deprecated method, maintained for backward-compatibility only, }
 { capsules new version of polymorphic SetMSH }
var
  MSHRecord: tMSH;
begin
  MSHRecord.delimiters   := delimiters;
  MSHRecord.sendingApp   := sendingApp;
  MSHRecord.sendingFac   := sendingFac;
  MSHRecord.receivingApp := receivingApp;
  MSHRecord.receivingFac := receivingFac;
  MSHRecord.dateTime     := EncodedDateTime(Now);
  MSHRecord.security     := security;
  MSHRecord.messageType  := messageType;
  MSHRecord.controlID    := message.ControlID;
  MSHRecord.processingID := processingID;
  MSHRecord.versionID    := message.HL7Version;
  MSHRecord.sequenceNumber := sequenceNumber;
  MSHRecord.continuationPointer := continuationPointer;
  MSHRecord.AccAckType   := AccAckType;
  MSHRecord.AppAckType   := AppAckType;
  MSHRecord.countryCode  := countryCode;
  MSHRecord.charSet      := charSet;
  MSHRecord.messageLanguage := messageLanguage;
  MSHRecord.altCharHandlScheme := altCharHandlScheme;
  MSHRecord.profileID    := profileID;
  MSHRecord.SendingRespOrg := '';
  MSHRecord.ReceivingRespOrt := '';
  MSHRecord.SendingNetworkAddr := '';
  MSHRecord.ReceivingNetworkAddr := '';
  SetMSH(message, MSHRecord, True);
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
  MSHRecord.delimiters   := delimiters;
  MSHRecord.sendingApp   := sendingApp;
  MSHRecord.sendingFac   := sendingFac;
  MSHRecord.receivingApp := receivingApp;
  MSHRecord.receivingFac := receivingFac;
  MSHRecord.dateTime     := dateTime;
  MSHRecord.security     := security;
  MSHRecord.messageType  := messageType;
  MSHRecord.controlID    := message.ControlID;
  MSHRecord.processingID := processingID;
  MSHRecord.versionID    := message.HL7Version;
  MSHRecord.sequenceNumber := sequenceNumber;
  MSHRecord.continuationPointer := continuationPointer;
  MSHRecord.AccAckType   := AccAckType;
  MSHRecord.AppAckType   := AppAckType;
  MSHRecord.countryCode  := countryCode;
  MSHRecord.charSet      := charSet;
  MSHRecord.messageLanguage := messageLanguage;
  MSHRecord.altCharHandlScheme := altCharHandlScheme;
  MSHRecord.profileID    := profileID;
  MSHRecord.SendingRespOrg := '';
  MSHRecord.ReceivingRespOrt := '';
  MSHRecord.SendingNetworkAddr := '';
  MSHRecord.ReceivingNetworkAddr := '';
  SetMSH(message, MSHRecord, False);
end;

procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: str227;
  security: str40; messageType: str15; processingID: str3; sequenceNumber: str15;
  continuationPointer: str180; AccAckType, AppAckType: Str2;
  countryCode: str3; charSet: str16; messageLanguage: str250;
  altCharHandlScheme: str20; profileID: str427);
 { deprecated method, maintained for backward-compatibility only, }
 { capsules new version of polymorphic SetMSH }
var
  MSHRecord: tMSH;
begin
  MSHRecord.delimiters   := delimiters;
  MSHRecord.sendingApp   := sendingApp;
  MSHRecord.sendingFac   := sendingFac;
  MSHRecord.receivingApp := receivingApp;
  MSHRecord.receivingFac := receivingFac;
  MSHRecord.dateTime     := EncodedDateTime(Now);
  MSHRecord.security     := security;
  MSHRecord.messageType  := messageType;
  MSHRecord.controlID    := message.ControlID;
  MSHRecord.processingID := processingID;
  MSHRecord.versionID    := message.HL7Version;
  MSHRecord.sequenceNumber := sequenceNumber;
  MSHRecord.continuationPointer := continuationPointer;
  MSHRecord.AccAckType   := AccAckType;
  MSHRecord.AppAckType   := AppAckType;
  MSHRecord.countryCode  := countryCode;
  MSHRecord.charSet      := charSet;
  MSHRecord.messageLanguage := messageLanguage;
  MSHRecord.altCharHandlScheme := altCharHandlScheme;
  MSHRecord.profileID    := profileID;
  MSHRecord.SendingRespOrg := '';
  MSHRecord.ReceivingRespOrt := '';
  MSHRecord.SendingNetworkAddr := '';
  MSHRecord.ReceivingNetworkAddr := '';
  SetMSH(message, MSHRecord, True);
end;

procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: str227;
  dateTime: str26; security: str40; messageType: str15; controlID: str20;
  processingID: str3; versionID: str60; sequenceNumber: str15;
  continuationPointer: str180; AccAckType, AppAckType: Str2;
  countryCode: str3; charSet: str16; messageLanguage: str250;
  altCharHandlScheme: str20; profileID: str427);
 { deprecated method, maintained for backward-compatibility only, }
 { capsules new version of polymorphic SetMSH }
var
  MSHRecord: tMSH;
begin
  MSHRecord.delimiters   := delimiters;
  MSHRecord.sendingApp   := sendingApp;
  MSHRecord.sendingFac   := sendingFac;
  MSHRecord.receivingApp := receivingApp;
  MSHRecord.receivingFac := receivingFac;
  MSHRecord.dateTime     := dateTime;
  MSHRecord.security     := security;
  MSHRecord.messageType  := messageType;
  MSHRecord.controlID    := message.ControlID;
  MSHRecord.processingID := processingID;
  MSHRecord.versionID    := message.HL7Version;
  MSHRecord.sequenceNumber := sequenceNumber;
  MSHRecord.continuationPointer := continuationPointer;
  MSHRecord.AccAckType   := AccAckType;
  MSHRecord.AppAckType   := AppAckType;
  MSHRecord.countryCode  := countryCode;
  MSHRecord.charSet      := charSet;
  MSHRecord.messageLanguage := messageLanguage;
  MSHRecord.altCharHandlScheme := altCharHandlScheme;
  MSHRecord.profileID    := profileID;
  MSHRecord.SendingRespOrg := '';
  MSHRecord.ReceivingRespOrt := '';
  MSHRecord.SendingNetworkAddr := '';
  MSHRecord.ReceivingNetworkAddr := '';
  SetMSH(message, MSHRecord, False);




end;

procedure ClearMSH(var MSHRecord: tMSH);
begin
  FillChar(MSHRecord, SizeOf(MSHRecord), 0);
end;

end.
