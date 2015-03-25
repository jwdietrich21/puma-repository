unit BLG;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for billing segments }

{ Version 1.7.0 (Hermes) }

{ (c) Johannes W. Dietrich, 1994 - 2015 }
{ (c) Marek Skorupski 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

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
  BLG_ID = 'BLG';

type
  tBLG = record
    WhentoCharge: tCM;
    ChargeType: tID;
    AccountID: tCK;
    ChargeTypeReason: tCE;
  end;

function BLG_Segment(message: THL7Message): THL7Segment;
procedure GetBLG(aSegment: THL7Segment; out BLGRecord: tBLG);
procedure GetBLG(message: THL7Message; out BLGRecord: tBLG);
procedure SetBLG(message: THL7Message; aSegment: THL7Segment);
procedure SetBLG(message: THL7Message; BLGRecord: tBLG);
procedure ClearBLG(var BLGRecord: tBLG);

implementation

function BLG_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(BLG_ID, '0')
  else
    Result := nil;
end;

procedure GetBLG(aSegment: THL7Segment; out BLGRecord: tBLG);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'BLG') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with BLGRecord do
      begin
        nextField := aSegment.FirstOccurrence.FirstField.nextSibling;
        WhentoCharge := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ChargeType := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AccountID := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ChargeTypeReason := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
    ClearBLG(BLGRecord);
end;

procedure GetBLG(message: THL7Message; out BLGRecord: tBLG);
var
  curSegment: THL7Segment;
begin
  curSegment := BLG_Segment(message);
  GetBLG(curSegment, BLGRecord);
end;

procedure SetBLG(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetBLG(message: THL7Message; BLGRecord: tBLG);
var
  newSegment: THL7Segment;
  FieldSep:   char;
  theString:  ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with BLGRecord do
    theString := BLG_ID + FieldSep + WhentoCharge + FieldSep + ChargeType +
      FieldSep + AccountID + FieldSep + ChargeTypeReason + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure ClearBLG(var BLGRecord: tBLG);
begin
  FillChar(BLGRecord, SizeOf(BLGRecord), 0);
end;

end.

