unit EDF;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ EDF+ base unit }

{ Version 1.0 (Alpha Centauri) }

{ (c) Johannes W. Dietrich, 1994 - 2018 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2018 }

{ Parser and compiler for EDF and EDF+ data files }

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
  Classes, SysUtils, StrUtils, Math, URIParser, DateUtils;

type

  str4 = string[4];
  str8  = string[8];
  str16 = string[16];
  str32 = string[32];
  str44 = string[44];
  str80 = string[80];

const

  ksCR   = #13;
  ksLF   = #10;
  ksCRLF = #13#10;

  noErr          = 0;
  headermalform  = 1;
  termErr        = 2;
  sizemismatch   = 3;
  unsuppVers     = 4;
  emptydata      = 5;
  saveErr        = 6;
  readErr        = 7;
  createErr      = 9;
  stringRangeErr = 11;

  kEDFVersion     : str8 = '0       ';
  kUnknown        : str8 = '-1      ';
  kEmpty0         : char = char(0);
  kEmpty8         : str8 = '        ';
  kEmpty44        : str44 = '                                            ';
  kEmpty80        : str80 = '                                                                                ';
  kZero4          : str4 = '0000';
  kZero8          : str8 = '00000000';
  kDefaultDate    : str8 = '01.01.85';
  kDefaultTime    : str8 = '00.00.00';
  kStartDate      = 'Startdate';
  kEDFAnnotations = 'EDF Annotations';

type

{ TEDFDoc }

TEDFDoc = class
  private
    // Fields of EDF and EDF+ header record
    prVersion: str8;             // Version of data format
    prLocalPatID: str80;         // Local patient identification
    prLocalRecID: str80;         // Local recording identification
    prStartDate: str8;           // Start date of recording (dd.mm.yy)
    prStartTime: str8;           // Start time of recording (hh.mm.ss)
    prNumOfBytes: str8;          // Number of bytes in header record
    prReserved: str44;           // Reserved
    prNumOfDataRecs: str8;       // Number of data records
    prDurOfData: str8;           // Duration of a data record
    prNumOfSignals: str4;        // Number of signals in data record
    prLabel: AnsiString;         // Label for signal
    prTransducer: AnsiString;    // Transducer type
    prPhysDim: AnsiString;       // Physical dimension
    prPhysMin: AnsiString;       // Physical minimum
    prPhysMax: AnsiString;       // Physical maximum
    prDigMin: AnsiString;        // Digital minimum
    prDigMax: AnsiString;        // Ditial maximum
    prPrefilter: AnsiString;     // Prefiltering
    prNumOfSamples: AnsiString;  // Nr of samples in each data record
    prReserved2: AnsiString;     // Reserved
    // Official EDF/EDF+ fields end here.
    status:    integer;
  protected
    HeaderText: ansistring;
    // DataChunk: (Buffer type still to be determined)
    procedure CompileHeaderText;
    function ExtractHeaderText(const start, count: integer): AnsiString;
    function GetVersion: Str8;
    function GetLocalPatID: Str80;
    procedure SetLocalPatID(const ID: Str80);
  public
    constructor Create;
    destructor Destroy; override;
    property version: Str8 Read GetVersion;
    property header: AnsiString Read HeaderText;
    property LocalPatID: Str80 Read GetLocalPatID Write SetLocalPatID;
    property StatusCode: integer Read status;
  end;


implementation

procedure TEDFDoc.CompileHeaderText;
begin
  HeaderText := prVersion + prLocalPatID + prLocalRecID + prStartDate +
                prStartTime + prNumOfBytes + prReserved + prNumOfDataRecs +
                prDurOfData + prNumOfSignals + prLabel + prTransducer +
                prPhysDim + prPhysMin + prPhysMax + prDigMin + prDigMax +
                prPrefilter + prNumOfSamples + prReserved2;
end;

function TEDFDoc.ExtractHeaderText(const start, count: integer): AnsiString;
begin
  if (length(HeaderText) >= start) and (length(HeaderText) >= start + count) then
  begin
    Result := copy(HeaderText, start, count);
  end
  else
  begin
    Status := stringRangeErr;
    Result := '';
  end;
end;

function TEDFDoc.GetVersion: Str8;
begin
  Result := prVersion;
end;

function TEDFDoc.GetLocalPatID: Str80;
begin
  ExtractHeaderText(8, 80);
end;

procedure TEDFDoc.SetLocalPatID(const ID: Str80);
begin
  prLocalPatID := ID;
  CompileHeaderText;
end;

constructor TEDFDoc.Create;
var
  headerLength: longint;
begin
  inherited Create;
  status := 0;
  prVersion := kEDFVersion;
  prNumOfDataRecs := kUnknown;
  prLocalPatID := kEmpty80;
  prLocalRecID := kEmpty80;
  prStartDate := kDefaultDate;
  prStartTime := kDefaultTime;
  prReserved := kEmpty44;
  prNumOfDataRecs := kUnknown;
  prDurOfData := kZero8;
  prNumOfSignals := kZero4;
  prLabel := kEmpty0;
  prTransducer := kEmpty0;
  prPhysDim := kEmpty0;
  prPhysMin := kEmpty0;
  prPhysMax := kEmpty0;
  prDigMin := kEmpty0;
  prDigMax := kEmpty0;
  prPrefilter := kEmpty0;
  prNumOfSamples := kEmpty0;
  prReserved2 := kEmpty0;
  headerLength := length(prVersion) + length(prLocalPatID) +
                length(prLocalRecID) + length(prStartDate) +
                length(prStartTime) + length(kEmpty8) +
                length(prReserved) + length(prNumOfDataRecs) +
                length(prDurOfData) + length(prNumOfSignals) +
                length(prLabel) + length(prTransducer) + length(prPhysDim) +
                length(prPhysMin) + length(prPhysMax) + length(prDigMin) +
                length(prDigMax) + length(prPrefilter) +
                length(prNumOfSamples) + length(prReserved2);
  prNumOfBytes := FormatFloat('00000000', headerLength);
  CompileHeaderText;
end;

destructor TEDFDoc.Destroy;
begin
  inherited Destroy;
end;

end.

