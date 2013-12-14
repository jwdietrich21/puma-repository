unit OBX;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for observation / result segments }

{ Version 1.1 }

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

function OBX_Segment(message: THL7Message): THL7Segment;
procedure GetOBX(message: THL7Message; out SetID: str4; out ValueType: str2; out ObsID: str250;
  obsSubID: str20; out obsValue: AnsiString; out Units: str250; out RefRange: str60;
  AbnormFlags, probability: str5; out Nature: str2; out status: char; out RRDate: str26;
  UDAC: str20; out ObsDateTime: str26; out prodID, respObs, observMethod: str250;
  EquipInstID: str22; out AnalysisDateTime: str26);
procedure SetOBX(message: THL7Message; aSegment: THL7Segment);
procedure SetOBX(message: THL7Message; SetID: str4; ValueType: str2; ObsID: str250;
  obsSubID: str20; obsValue: AnsiString; Units: str250; RefRange: str60;
  AbnormFlags, probability: str5; Nature: str2; status: char; RRDate: str26;
  UDAC: str20; ObsDateTime: str26; prodID, respObs, observMethod: str250;
  EquipInstID: str22; AnalysisDateTime: str26);

implementation

function OBX_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment('OBX', '0')
  else
    Result := nil;
end;

procedure GetOBX(message: THL7Message; out SetID: str4; out ValueType: str2; out ObsID: str250;
  obsSubID: str20; out obsValue: AnsiString; out Units: str250; out RefRange: str60;
  AbnormFlags, probability: str5; out Nature: str2; out status: char; out RRDate: str26;
  UDAC: str20; out ObsDateTime: str26; out prodID, respObs, observMethod: str250;
  EquipInstID: str22; out AnalysisDateTime: str26);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := OBX_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
    begin
      nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
      SetID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      ValueType := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      ObsID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      obsSubID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      obsValue := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      Units := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      RefRange := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      AbnormFlags := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      probability := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      Nature := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      status := curSegment.FirstOccurrence.GetNextFieldContent(nextField)[1];
      RRDate := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      UDAC := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      ObsDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      prodID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      respObs := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      observMethod := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      EquipInstID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      AnalysisDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
    end;
  end;
end;

procedure SetOBX(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetOBX(message: THL7Message; SetID: str4; ValueType: str2; ObsID: str250;
  obsSubID: str20; obsValue: AnsiString; Units: str250; RefRange: str60;
  AbnormFlags, probability: str5; Nature: str2; status: char; RRDate: str26;
  UDAC: str20; ObsDateTime: str26; prodID, respObs, observMethod: str250;
  EquipInstID: str22; AnalysisDateTime: str26);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: AnsiString;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  theString := 'OBX|' + SetID + FieldSep + ValueType +
    FieldSep + ObsID + FieldSep + obsSubID + FieldSep +
    obsValue + FieldSep + Units + FieldSep + RefRange +
    FieldSep + AbnormFlags + FieldSep + probability + FieldSep + Nature +
    FieldSep + status + FieldSep + RRDate + FieldSep + UDAC + FieldSep +
    ObsDateTime + FieldSep + prodID + FieldSep + respObs + FieldSep +
    observMethod + FieldSep + EquipInstID + FieldSep + AnalysisDateTime + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

end.

