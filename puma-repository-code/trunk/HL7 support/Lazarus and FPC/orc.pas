unit ORC;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for Common Order Segments }

{ Version 1.7.0 }

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
  ORC_ID = 'ORC';

type
  tORC = record
    OrderControl: tID;
    PlacerOrderNumber, FillerOrderNumber, PlacerGroupNumber: tEI;
    OrderStatus: tID;
    ResponseFlag: tID;
    QuantityTiming: tCQ;
    Parent: tEIP;
    DateTimeofTransaction: tTS;
    EnteredBy, VerifiedBy, OrderingProvider: tXCN;
    EnterersLocation: tPL;
    CallBackPhoneNumber: tXTN;
    OrderEffectiveDateTime: tTS;
    OrderControlCodeReason: tCE;
    EnteringOrganization, EnteringDevice: tCE;
    ActionBy: tXCN;
  end;

function ORC_Segment(message: THL7Message): THL7Segment;
procedure GetORC(aSegment: THL7Segment; out ORCRecord: tORC);
procedure GetORC(message: THL7Message; out ORCRecord: tORC);
procedure GetORC(message: THL7Message; out OrderControl: tID;
  out PlacerOrderNumber, FillerOrderNumber, PlacerGroupNumber: tEI;
  out OrderStatus: tID;
  out ResponseFlag: tID;
  out QuantityTiming: tCQ;
  out Parent: tEIP;
  out DateTimeofTransaction: tTS;
  out EnteredBy, VerifiedBy, OrderingProvider: tXCN;
  out EnterersLocation: tPL;
  out CallBackPhoneNumber: tXTN;
  out OrderEffectiveDateTime: tTS;
  out OrderControlCodeReason: tCE;
  out EnteringOrganization, EnteringDevice: tCE;
  out ActionBy: tXCN);
  deprecated;
procedure GetORC(message: THL7Message; out OrderControl: str2;
  out PlacerOrderNumber, FillerOrderNumber, PlacerGroupNumber: str22;
  out OrderStatus: str2;
  out ResponseFlag: str1;
  out QuantityTiming: str200;
  out Parent: str200;
  out DateTimeofTransaction: str26;
  out EnteredBy, VerifiedBy, OrderingProvider: str120;
  out EnterersLocation: str80;
  out CallBackPhoneNumber: str40;
  out OrderEffectiveDateTime: str26;
  out OrderControlCodeReason: str200;
  out EnteringOrganization, EnteringDevice: str60;
  out ActionBy: str120);
  deprecated;
procedure SetORC(message: THL7Message; aSegment: THL7Segment);
procedure SetORC(message: THL7Message; ORCRecord: tORC);
procedure SetORC(message: THL7Message; OrderControl: tID;
  PlacerOrderNumber, FillerOrderNumber, PlacerGroupNumber: tEI;
  OrderStatus: tID;
  ResponseFlag: tID;
  QuantityTiming: tCQ;
  Parent: tEIP;
  DateTimeofTransaction: tTS;
  EnteredBy, VerifiedBy, OrderingProvider: tXCN;
  EnterersLocation: tPL;
  CallBackPhoneNumber: tXTN;
  OrderEffectiveDateTime: tTS;
  OrderControlCodeReason: tCE;
  EnteringOrganization, EnteringDevice: tCE;
  ActionBy: tXCN);
  deprecated;
procedure SetORC(message: THL7Message; OrderControl: str2;
  PlacerOrderNumber, FillerOrderNumber, PlacerGroupNumber: str22;
  OrderStatus: str2;
  ResponseFlag: str1;
  QuantityTiming: str200;
  Parent: str200;
  DateTimeofTransaction: str26;
  EnteredBy, VerifiedBy, OrderingProvider: str120;
  EnterersLocation: str80;
  CallBackPhoneNumber: str40;
  OrderEffectiveDateTime: str26;
  OrderControlCodeReason: str200;
  EnteringOrganization, EnteringDevice: str60;
  ActionBy: str120);
  deprecated;
procedure ClearORC(var ORCRecord: tORC);

implementation

function ORC_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(ORC_ID, '0')
  else
    Result := nil;
end;

procedure GetORC(aSegment: THL7Segment; out ORCRecord: tORC);
var
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'ORC') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with ORCRecord do
      begin
        nextField := aSegment.FirstOccurrence.FirstField.nextSibling;
        OrderControl := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlacerOrderNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        FillerOrderNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PlacerGroupNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderStatus := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ResponseFlag := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        QuantityTiming := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Parent := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DateTimeofTransaction := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EnteredBy := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VerifiedBy := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderingProvider := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EnterersLocation := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        CallBackPhoneNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderEffectiveDateTime := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrderControlCodeReason := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EnteringOrganization := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EnteringDevice := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ActionBy := aSegment.FirstOccurrence.GetNextFieldContent(nextField)
      end;
  end
  else
  ClearORC(ORCRecord);
end;

procedure GetORC(message: THL7Message; out ORCRecord: tORC);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := ORC_Segment(message);
  GetORC(curSegment, ORCRecord);
end;

procedure GetORC(message: THL7Message; out OrderControl: tID;
  out PlacerOrderNumber, FillerOrderNumber, PlacerGroupNumber: tEI;
  out OrderStatus: tID;
  out ResponseFlag: tID;
  out QuantityTiming: tCQ;
  out Parent: tEIP;
  out DateTimeofTransaction: tTS;
  out EnteredBy, VerifiedBy, OrderingProvider: tXCN;
  out EnterersLocation: tPL;
  out CallBackPhoneNumber: tXTN;
  out OrderEffectiveDateTime: tTS;
  out OrderControlCodeReason: tCE;
  out EnteringOrganization, EnteringDevice: tCE;
  out ActionBy: tXCN);
var
  ORCRecord: tORC;
begin
  GetORC(message, ORCRecord);
  OrderControl := ORCRecord.OrderControl;
  PlacerOrderNumber := ORCRecord.PlacerOrderNumber;
  FillerOrderNumber := ORCRecord.FillerOrderNumber;
  PlacerGroupNumber := ORCRecord.PlacerGroupNumber;
  OrderStatus := ORCRecord.OrderStatus;
  ResponseFlag := ORCRecord.ResponseFlag;
  QuantityTiming := ORCRecord.QuantityTiming;
  Parent := ORCRecord.Parent;
  DateTimeofTransaction := ORCRecord.DateTimeofTransaction;
  EnteredBy := ORCRecord.EnteredBy;
  VerifiedBy := ORCRecord.VerifiedBy;
  OrderingProvider := ORCRecord.OrderingProvider;
  EnterersLocation := ORCRecord.EnterersLocation;
  CallBackPhoneNumber := ORCRecord.CallBackPhoneNumber;
  OrderEffectiveDateTime := ORCRecord.OrderEffectiveDateTime;
  OrderControlCodeReason := ORCRecord.OrderControlCodeReason;
  EnteringOrganization := ORCRecord.EnteringOrganization;
  EnteringDevice := ORCRecord.EnteringDevice;
  ActionBy := ORCRecord.ActionBy;
end;

procedure GetORC(message: THL7Message; out OrderControl: str2;
  out PlacerOrderNumber, FillerOrderNumber, PlacerGroupNumber: str22;
  out OrderStatus: str2;
  out ResponseFlag: str1;
  out QuantityTiming: str200;
  out Parent: str200;
  out DateTimeofTransaction: str26;
  out EnteredBy, VerifiedBy, OrderingProvider: str120;
  out EnterersLocation: str80;
  out CallBackPhoneNumber: str40;
  out OrderEffectiveDateTime: str26;
  out OrderControlCodeReason: str200;
  out EnteringOrganization, EnteringDevice: str60;
  out ActionBy: str120);
var
  ORCRecord: tORC;
begin
  GetORC(message, ORCRecord);
  OrderControl := ORCRecord.OrderControl;
  PlacerOrderNumber := ORCRecord.PlacerOrderNumber;
  FillerOrderNumber := ORCRecord.FillerOrderNumber;
  PlacerGroupNumber := ORCRecord.PlacerGroupNumber;
  OrderStatus := ORCRecord.OrderStatus;
  ResponseFlag := ORCRecord.ResponseFlag;
  QuantityTiming := ORCRecord.QuantityTiming;
  Parent := ORCRecord.Parent;
  DateTimeofTransaction := ORCRecord.DateTimeofTransaction;
  EnteredBy := ORCRecord.EnteredBy;
  VerifiedBy := ORCRecord.VerifiedBy;
  OrderingProvider := ORCRecord.OrderingProvider;
  EnterersLocation := ORCRecord.EnterersLocation;
  CallBackPhoneNumber := ORCRecord.CallBackPhoneNumber;
  OrderEffectiveDateTime := ORCRecord.OrderEffectiveDateTime;
  OrderControlCodeReason := ORCRecord.OrderControlCodeReason;
  EnteringOrganization := ORCRecord.EnteringOrganization;
  EnteringDevice := ORCRecord.EnteringDevice;
  ActionBy := ORCRecord.ActionBy;
end;

procedure SetORC(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetORC(message: THL7Message; ORCRecord: tORC);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with ORCRecord do
  begin
    theString := ORC_ID + FieldSep + OrderControl + FieldSep + PlacerOrderNumber +
      FieldSep + FillerOrderNumber + FieldSep + PlacerGroupNumber + FieldSep + OrderStatus +
      FieldSep + ResponseFlag + FieldSep + QuantityTiming + FieldSep + Parent +
      FieldSep + DateTimeofTransaction + FieldSep + EnteredBy + FieldSep + VerifiedBy +
      FieldSep + OrderingProvider + FieldSep + EnterersLocation + FieldSep + CallBackPhoneNumber +
      FieldSep + OrderEffectiveDateTime + FieldSep + OrderControlCodeReason + FieldSep + EnteringOrganization +
      FieldSep + EnteringDevice + FieldSep + ActionBy;
  end;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure SetORC(message: THL7Message; OrderControl: tID;
  PlacerOrderNumber, FillerOrderNumber, PlacerGroupNumber: tEI;
  OrderStatus: tID;
  ResponseFlag: tID;
  QuantityTiming: tCQ;
  Parent: tEIP;
  DateTimeofTransaction: tTS;
  EnteredBy, VerifiedBy, OrderingProvider: tXCN;
  EnterersLocation: tPL;
  CallBackPhoneNumber: tXTN;
  OrderEffectiveDateTime: tTS;
  OrderControlCodeReason: tCE;
  EnteringOrganization, EnteringDevice: tCE;
  ActionBy: tXCN);
var
  ORCRecord: tORC;
begin
  ORCRecord.OrderControl := OrderControl;
  ORCRecord.PlacerOrderNumber := PlacerOrderNumber;
  ORCRecord.FillerOrderNumber := FillerOrderNumber;
  ORCRecord.PlacerGroupNumber := PlacerGroupNumber;
  ORCRecord.OrderStatus := OrderStatus;
  ORCRecord.ResponseFlag := ResponseFlag;
  ORCRecord.QuantityTiming := QuantityTiming;
  ORCRecord.Parent := Parent;
  ORCRecord.DateTimeofTransaction := DateTimeofTransaction;
  ORCRecord.EnteredBy := EnteredBy;
  ORCRecord.VerifiedBy := VerifiedBy;
  ORCRecord.OrderingProvider := OrderingProvider;
  ORCRecord.EnterersLocation := EnterersLocation;
  ORCRecord.CallBackPhoneNumber := CallBackPhoneNumber;
  ORCRecord.OrderEffectiveDateTime := OrderEffectiveDateTime;
  ORCRecord.OrderControlCodeReason := OrderControlCodeReason;
  ORCRecord.EnteringOrganization := EnteringOrganization;
  ORCRecord.EnteringDevice := EnteringDevice;
  ORCRecord.ActionBy := ActionBy;
  SetORC(message, ORCRecord);
end;

procedure SetORC(message: THL7Message; OrderControl: str2;
  PlacerOrderNumber, FillerOrderNumber, PlacerGroupNumber: str22;
  OrderStatus: str2;
  ResponseFlag: str1;
  QuantityTiming: str200;
  Parent: str200;
  DateTimeofTransaction: str26;
  EnteredBy, VerifiedBy, OrderingProvider: str120;
  EnterersLocation: str80;
  CallBackPhoneNumber: str40;
  OrderEffectiveDateTime: str26;
  OrderControlCodeReason: str200;
  EnteringOrganization, EnteringDevice: str60;
  ActionBy: str120);
var
  ORCRecord: tORC;
begin
  ORCRecord.OrderControl := OrderControl;
  ORCRecord.PlacerOrderNumber := PlacerOrderNumber;
  ORCRecord.FillerOrderNumber := FillerOrderNumber;
  ORCRecord.PlacerGroupNumber := PlacerGroupNumber;
  ORCRecord.OrderStatus := OrderStatus;
  ORCRecord.ResponseFlag := ResponseFlag;
  ORCRecord.QuantityTiming := QuantityTiming;
  ORCRecord.Parent := Parent;
  ORCRecord.DateTimeofTransaction := DateTimeofTransaction;
  ORCRecord.EnteredBy := EnteredBy;
  ORCRecord.VerifiedBy := VerifiedBy;
  ORCRecord.OrderingProvider := OrderingProvider;
  ORCRecord.EnterersLocation := EnterersLocation;
  ORCRecord.CallBackPhoneNumber := CallBackPhoneNumber;
  ORCRecord.OrderEffectiveDateTime := OrderEffectiveDateTime;
  ORCRecord.OrderControlCodeReason := OrderControlCodeReason;
  ORCRecord.EnteringOrganization := EnteringOrganization;
  ORCRecord.EnteringDevice := EnteringDevice;
  ORCRecord.ActionBy := ActionBy;
  SetORC(message, ORCRecord);
end;

procedure ClearORC(var ORCRecord: tORC);
begin
  FillChar(ORCRecord, SizeOf(ORCRecord), 0);
end;

end.
