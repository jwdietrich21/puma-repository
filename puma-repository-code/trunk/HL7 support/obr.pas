unit OBR;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for observation request segments }

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
  OBR_ID = 'OBR';

function OBR_Segment(message: THL7Message): THL7Segment;
procedure GetOBR(message: THL7Message; out SetID: str4;
  out PlacOrdNumb, FillOrdNumb: str22; out USI: str250; out Priority: Str2;
  out ReqDateTime, ObsDateTime, ObsEndDateTime: tDTM);
procedure SetOBR(message: THL7Message; aSegment: THL7Segment);
procedure SetOBR(message: THL7Message; SetID: str4; PlacOrdNumb, FillOrdNumb: str22;
  USI: str250; Priority: Str2; ReqDateTime, ObsDateTime, ObsEndDateTime: tDTM);

implementation

function OBR_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(OBR_ID, '0')
  else
    Result := nil;
end;

procedure GetOBR(message: THL7Message; out SetID: str4;
  out PlacOrdNumb, FillOrdNumb: str22; out USI: str250; out Priority: Str2;
  out ReqDateTime, ObsDateTime, ObsEndDateTime: tDTM);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := OBR_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
    begin
      nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
      SetID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      PlacOrdNumb := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      FillOrdNumb := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      USI := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      Priority := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      ReqDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      ObsDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      ObsEndDateTime := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
    end;
  end;
end;

procedure SetOBR(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetOBR(message: THL7Message; SetID: str4; PlacOrdNumb, FillOrdNumb: str22;
  USI: str250; Priority: Str2; ReqDateTime, ObsDateTime, ObsEndDateTime: tDTM);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: AnsiString;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  theString := OBR_ID + FieldSep + SetID + FieldSep + PlacOrdNumb +
    FieldSep + FillOrdNumb + FieldSep + USI + FieldSep +
    Priority + FieldSep + ReqDateTime + FieldSep + ObsDateTime +
    FieldSep + ObsEndDateTime + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

end.
