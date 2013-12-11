unit MSH;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for message headers }

{ Version 1.0 }

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

function MSH_Segment(message: THL7Message): THL7Segment;
procedure GetMSH(message: THL7Message; out delimiters: str5;
  out sendingApp, sendingFac, receivingApp, receivingFac: str227;
  out dateTime: str26; out security: str40; out messageType: str15;
  out controlID: str20; out processingID: str3; out versionID: str60;
  sequenceNumber: str15; out continuationPointer: str180;
  out AccAckType, AppAckType: Str2; out countryCode: str3; out charSet: str16;
  out messageLanguage: str250; out altCharHandlScheme: str20; out profileID: str427);
procedure SetMSH(message: THL7Message; aSegment: THL7Segment);
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
    Result := message.FoundSegment('MSH', '0')
  else
    Result := nil;
end;

procedure GetMSH(message: THL7Message; out delimiters: str5;
  out sendingApp, sendingFac, receivingApp, receivingFac: str227;
  out dateTime: str26; out security: str40; out messageType: str15;
  out controlID: str20; out processingID: str3; out versionID: str60;
  sequenceNumber: str15; out continuationPointer: str180;
  out AccAckType, AppAckType: Str2; out countryCode: str3; out charSet: str16;
  out messageLanguage: str250; out altCharHandlScheme: str20; out profileID: str427);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  delimiters := message.CompiledDelimiters(message.Delimiters);
  curSegment := MSH_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
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

procedure SetMSH(message: THL7Message; aSegment: THL7Segment);
begin
  message.ReplaceSegment('MSH', '0', aSegment, True);
end;

procedure SetMSH(message: THL7Message; delimiters: str5; sendingApp,
  sendingFac, receivingApp, receivingFac: str227; security: str40;
  messageType: str15; processingID: str3;
  sequenceNumber: str15; continuationPointer: str180; AccAckType,
  AppAckType: Str2; countryCode: str3; charSet: str16; messageLanguage: str250;
  altCharHandlScheme: str20; profileID: str427);
var
  newSegment: THL7Segment;
  FieldSep: char;
  dateTime: str26;
  controlID: str20;
  versionID: str60;
  theString: AnsiString;
begin
  dateTime := EncodedDateTime(Now);
  versionID := message.HL7Version;
  FieldSep := message.Delimiters.FieldSeparator;
  controlID := message.ControlID;
  newSegment := THL7Segment.Create(message, '');
  theString := 'MSH' + delimiters + FieldSep + sendingApp +
    FieldSep + sendingFac + FieldSep + receivingApp + FieldSep +
    receivingFac + FieldSep + dateTime + FieldSep + security + FieldSep +
    messageType + FieldSep + controlID + FieldSep + processingID +
    FieldSep + versionID + FieldSep + sequenceNumber + FieldSep +
    continuationPointer + FieldSep + AccAckType + FieldSep +
    AppAckType + FieldSep + countryCode + FieldSep + charSet + FieldSep +
    messageLanguage + FieldSep + altCharHandlScheme + FieldSep + profileID + FieldSep;
  newSegment.contentString := theString;
  message.ReplaceSegment('MSH', '0', newSegment, True);
end;

procedure SetMSH(message: THL7Message; delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: str227;
  dateTime: str26; security: str40; messageType: str15; controlID: str20;
  processingID: str3; versionID: str60; sequenceNumber: str15;
  continuationPointer: str180; AccAckType, AppAckType: Str2;
  countryCode: str3; charSet: str16; messageLanguage: str250;
  altCharHandlScheme: str20; profileID: str427);
var
  newSegment: THL7Segment;
  FieldSep: char;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  newSegment.contentString := 'MSH' + delimiters + FieldSep + sendingApp +
    FieldSep + sendingFac + FieldSep + receivingApp + FieldSep +
    receivingFac + FieldSep + dateTime + FieldSep + security + FieldSep +
    messageType + FieldSep + controlID + FieldSep + processingID +
    FieldSep + versionID + FieldSep + sequenceNumber + FieldSep +
    continuationPointer + FieldSep + AccAckType + FieldSep +
    AppAckType + FieldSep + countryCode + FieldSep + charSet + FieldSep +
    messageLanguage + FieldSep + altCharHandlScheme + FieldSep + profileID + FieldSep;
  message.ReplaceSegment('MSH', '0', newSegment, True);
end;

end.
