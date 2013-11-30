unit MSH;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for message headers}

{ Version 0.9 }

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
  out sendingApp: str15; out sendingFac: str20; out receivingApp, receivingFac: str30;
  out dateTime: str26; out messageType: str7; out controlID: char;
  out versionID: str8; out AccAckType, AppAckType: Str2);

implementation

function MSH_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment('MSH', '0')
  else
    Result := nil;
end;

procedure GetMSH(message: THL7Message; out delimiters: str5;
  out sendingApp: str15; out sendingFac: str20; out receivingApp, receivingFac: str30;
  out dateTime: str26; out messageType: str7; out controlID: char;
  out versionID: str8; out AccAckType, AppAckType: Str2);
var
  curSegment: THL7Segment;
  curField: THL7Field;
begin
  delimiters := message.CompiledDelimiters(message.Delimiters);
  curSegment := MSH_Segment(message);
  curField := curSegment.FirstOccurrence.FirstField.nextSibling.nextSibling;
  sendingApp := curField.contentString;
  curField := curField.nextSibling;
  sendingFac := curField.contentString;
  curField := curField.nextSibling;
  receivingApp := curField.contentString;
  curField := curField.nextSibling;
  receivingFac := curField.contentString;
  curField := curField.nextSibling;
  dateTime := curField.contentString;
  curField := curField.nextSibling;
  messageType := curField.contentString;
  curField := curField.nextSibling;
  controlID := curField.contentString[1];
  curField := curField.nextSibling;
  versionID := curField.contentString;
  curField := curField.nextSibling;
  AccAckType := curField.contentString;
  curField := curField.nextSibling;
  AppAckType := curField.contentString;
  curField := curField.nextSibling;
end;

end.
