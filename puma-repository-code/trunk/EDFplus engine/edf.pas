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
  rangeErr       = 10;
  strRangeErr    = 11;
  strFormatErr   = 12;

  kEDFVersion     : str8 = '0       ';
  kUnknown        : str8 = '-1      ';
  kEmpty0         = '';
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
    FHeaderText: AnsiString;
    // DataChunk: (Buffer type still to be determined)
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
    procedure CompileHeaderText;
    function ExtractedHeaderText(const start, count: integer): AnsiString;
    procedure CalcHeaderLength;
    function HeaderString: AnsiString;
    function GetVersion: Str8;
    function GetLocalPatID: Str80;
    procedure SetLocalPatID(const ID: Str80);
    function GetLocalRecID: Str80;
    procedure SetLocalRecID(const ID: Str80);
    function GetStartDate: Str8;
    function dGetStartDate: tDateTime;
    procedure SetStartDate(const DateStr: Str8);
    procedure SetStartDate(const Date: tDateTime);
    function GetStartTime: Str8;
    function dGetStartTime: tDateTime;
    procedure SetStartTime(const TimeStr: Str8);
    procedure SetStartTime(const Time: tDateTime);
    function GetNumOfBytes: Str8;
    function iGetNumOfBytes: longint;
    function GetNumOfDataRecs: Str8;
    function iGetNumOfDataRecs: longint;
    procedure SetNumOfDataRecs(const NumOfRecs: Str8);
    procedure SetNumOfDataRecs(const nr: longint);
    function GetDurOfData: Str8;
    function iGetDurOfData: longint;
    procedure SetDurOfData(const duration: Str8);
    procedure SetDurOfData(const dd: longint);
    function GetNumOfSignals: Str4;
    function iGetNumOfSignals: integer;
    procedure SetNumOfSignals(const ns: Str4);
    procedure SetNumOfSignals(const ns: integer);
    procedure SetLabel(const position: integer; const theLabel: str16);
    function GetLabel(const position: integer): str16;
    procedure SetTransducer(const position: integer; const transducer: str80);
    function GetTransducer(const position: integer): str80;
    procedure SetPhysDim(const position: integer; const dimension: str8);
    function GetPhysDim(const position: integer): Str8;
  public
    constructor Create;
    destructor Destroy; override;
    property version: Str8 Read GetVersion;
    property header: AnsiString Read FHeaderText;
    property LocalPatID: Str80 Read GetLocalPatID Write SetLocalPatID;
    property LocalRecID: Str80 Read GetLocalRecID Write SetLocalRecID;
    property StartDate: Str8 Read GetStartDate Write SetStartDate;
    property dStartDate: TDateTime Read dGetStartDate Write SetStartDate;
    property StartTime: Str8 Read GetStartTime Write SetStartTime;
    property dStartTime: TDateTime Read dGetStartTime Write SetStartTime;
    property NumOfBytes: Str8 Read GetNumOfBytes;
    property iNumOfBytes: longint Read iGetNumOfBytes;
    property NumOfDataRecs: Str8 Read GetNumOfDataRecs Write SetNumOfDataRecs;
    property iNumOfDataRecs: longint Read iGetNumOfDataRecs Write SetNumOfDataRecs;
    property DurationOfData: Str8 Read GetDurOfData Write SetDurOfData;
    property iDurationOfData: longint Read iGetDurOfData Write SetDurOfData;
    property NumOfSignals: Str4 read GetNumOfSignals Write SetNumOfSignals;
    property iNumOfSignals: integer Read iGetNumOfSignals Write SetNumOfSignals;
    property SignalLabel[i: integer]: Str16 read GetLabel Write SetLabel;
    property Transducer[i: integer]: Str80 read GetTransducer Write SetTransducer;
    property PhysDim[i: integer]: Str8 read GetPhysDim Write SetPhysDim;
    property StatusCode: integer Read status;
  end;


implementation

procedure TEDFDoc.CompileHeaderText;
begin
  CalcHeaderLength;
  FHeaderText := HeaderString;
end;

function TEDFDoc.ExtractedHeaderText(const start, count: integer): AnsiString;
begin
  if (start >= 0) and (count >= 0) and (length(FHeaderText) >= start + count - 1) then
  begin
    Result := copy(FHeaderText, start, count);
  end
  else
  begin
    Status := strRangeErr;
    Result := '';
  end;
end;

procedure TEDFDoc.CalcHeaderLength;
var
  headerLength: longint;
begin
  headerLength := length(HeaderString);
  prNumOfBytes := FormatFloat(kZero8, headerLength);
end;

function TEDFDoc.HeaderString: AnsiString;
var
  chunk1, chunk2: AnsiString;
begin
  chunk1 := prVersion + prLocalPatID + prLocalRecID + prStartDate +
            prStartTime + prNumOfBytes + prReserved + prNumOfDataRecs +
            prDurOfData;
  chunk2 := prNumOfSignals + prLabel + prTransducer +
            prPhysDim + prPhysMin + prPhysMax + prDigMin + prDigMax +
            prPrefilter + prNumOfSamples + prReserved2;
  result := chunk1 + chunk2;
end;

function TEDFDoc.GetVersion: Str8;
begin
  Result := ExtractedHeaderText(1, 8);
end;

function TEDFDoc.GetLocalPatID: Str80;
begin
  result := ExtractedHeaderText(9, 80);
end;

procedure TEDFDoc.SetLocalPatID(const ID: Str80);
begin
  prLocalPatID := ID;
  CompileHeaderText;
end;

function TEDFDoc.GetLocalRecID: Str80;
begin
  result := ExtractedHeaderText(89, 80);
end;

procedure TEDFDoc.SetLocalRecID(const ID: Str80);
begin
  prLocalRecID := ID;
  CompileHeaderText;
end;

function TEDFDoc.GetStartDate: Str8;
begin
  result := ExtractedHeaderText(169, 8);
end;

function TEDFDoc.dGetStartDate: tDateTime;
var
  sdString: Str8;
  theFormat: TFormatSettings;
begin
  //theFormat := DefaultFormatSettings;
  theFormat.DateSeparator := '.';
  theFormat.ShortDateFormat := 'dd.mm.yy';
  { TODO -oJWD : Adapt for special EDF convention for two-digits years: }
  theFormat.TwoDigitYearCenturyWindow := 85;  // correct?
  sdString := GetStartDate;
  if not TryStrToDate(sdString, result, theFormat) then
    status := strFormatErr;
end;

procedure TEDFDoc.SetStartDate(const DateStr: Str8);
begin
  prStartDate := DateStr;
  CompileHeaderText;
end;

procedure TEDFDoc.SetStartDate(const Date: tDateTime);
var
  sdString: Str8;
begin
  sdString := FormatDateTime('dd.mm.yy', Date);
  { TODO -oJWD : Adapt for special EDF convention for two-digits years }
  SetStartDate(sdString);
end;

function TEDFDoc.GetStartTime: Str8;
begin
  result := ExtractedHeaderText(177, 8);
end;

function TEDFDoc.dGetStartTime: tDateTime;
var
  stString: Str8;
  theFormat: TFormatSettings;
begin
  theFormat.TimeSeparator := '.';
  theFormat.ShortTimeFormat := 'hh.nn.ss';
  stString := GetStartTime;
  if not TryStrToTime(stString, result, theFormat) then
    status := strFormatErr;
end;

procedure TEDFDoc.SetStartTime(const TimeStr: Str8);
begin
  prStartTime := TimeStr;
  CompileHeaderText;
end;

procedure TEDFDoc.SetStartTime(const Time: tDateTime);
var
  stString: Str8;
begin
  stString := FormatDateTime('hh.nn.ss', Time);
  SetStartTime(stString);
end;

function TEDFDoc.GetNumOfBytes: Str8;
begin
  result := ExtractedHeaderText(185, 8);
end;

function TEDFDoc.iGetNumOfBytes: longint;
var
  nbString: Str8;
begin
  nbString := GetNumOfBytes;
  if not TryStrToInt(nbString, result) then
    status := strFormatErr;
end;

function TEDFDoc.GetNumOfDataRecs: Str8;
begin
  result := ExtractedHeaderText(237, 8);
end;

function TEDFDoc.iGetNumOfDataRecs: longint;
var
  nrString: Str8;
begin
  nrString := GetNumOfDataRecs;
  if not TryStrToInt(nrString, result) then
    status := strFormatErr;
end;

procedure TEDFDoc.SetNumOfDataRecs(const NumOfRecs: Str8);
begin
  prNumOfDataRecs := NumOfRecs;
  CompileHeaderText;
end;

procedure TEDFDoc.SetNumOfDataRecs(const nr: longint);
var
  nrString: Str8;
begin
  if nr > 99999999 then begin
    status := rangeErr;
    nrString := FloatToStr(NaN);
  end
  else
    nrString := FormatFloat(kZero8, nr);
  SetNumOfDataRecs(nrString);
end;

function TEDFDoc.GetDurOfData: Str8;
begin
  result := ExtractedHeaderText(245, 8);
end;

function TEDFDoc.iGetDurOfData: longint;
var
  ddString: Str8;
begin
  ddString := GetDurOfData;
  if not TryStrToInt(ddString, result) then
    status := strFormatErr;
end;

procedure TEDFDoc.SetDurOfData(const duration: Str8);
begin
  prDurOfData := duration;
  CompileHeaderText;
end;

procedure TEDFDoc.SetDurOfData(const dd: longint);
var
  ddString: Str8;
begin
  if dd > 99999999 then begin
    status := rangeErr;
    ddString := FloatToStr(NaN);
  end
  else
    ddString := FormatFloat(kZero8, dd);
  SetDurOfData(ddString);
end;

function TEDFDoc.GetNumOfSignals: Str4;
begin
  result := ExtractedHeaderText(253, 4);
end;

function TEDFDoc.iGetNumOfSignals: integer;
var
  nsString: Str4;
begin
  nsString := GetNumOfSignals;
  if not TryStrToInt(nsString, result) then
    status := strFormatErr;
end;

procedure TEDFDoc.SetNumOfSignals(const ns: Str4);
begin
  prNumOfSignals := ns;
  CompileHeaderText;
end;

procedure TEDFDoc.SetNumOfSignals(const ns: integer);
var
  nsString: Str4;
begin
  if ns > 9999 then begin
    status := rangeErr;
    nsString := FloatToStr(NaN);
  end
  else
    nsString := FormatFloat(kZero4, ns);
  SetNumOfSignals(nsString);
end;

procedure TEDFDoc.SetLabel(const position: integer; const theLabel: str16);
var
  filledString: str16;
  ns: integer;
begin
  if not TryStrToInt(prNumOfSignals, ns) then
    status := strFormatErr
  else if position > (ns - 1) then // outside range?
  begin
    status := rangeErr;
  end
  else
  begin
    if length(prLabel) < ns * 16 then // Label string to short?
      prLabel := PadRight(prLabel, ns * 16);
    filledString := PadRight(theLabel, 16); // fill with spaces for length 16
    prLabel := StuffString(prLabel, position * 16 + 1, 16, filledString);
  end;
end;

function TEDFDoc.GetLabel(const position: integer): str16;
var
  subString: Str16;
  ns: integer;
begin
  if not TryStrToInt(prNumOfSignals, ns) then
    status := strFormatErr
  else if position > (ns - 1) then
  begin
    status := rangeErr;
    result := '';
  end
  else
  begin
    subString := copy(prLabel, position * 16 + 1, 16);
    result := TrimRight(subString);
  end;
end;

procedure TEDFDoc.SetTransducer(const position: integer; const transducer: str80
  );
var
  filledString: str80;
  ns: integer;
begin
  if not TryStrToInt(prNumOfSignals, ns) then
    status := strFormatErr
  else if position > (ns - 1) then // outside range?
  begin
    status := rangeErr;
  end
  else
  begin
    if length(prTransducer) < ns * 80 then // Label string to short?
      prTransducer := PadRight(prTransducer, ns * 80);
    filledString := PadRight(transducer, 80); // fill with spaces for length 16
    prTransducer := StuffString(prTransducer, position * 80 + 1, 80, filledString);
  end;
end;

function TEDFDoc.GetTransducer(const position: integer): str80;
var
  subString: Str80;
  ns: integer;
begin
  if not TryStrToInt(prNumOfSignals, ns) then
    status := strFormatErr
  else if position > (ns - 1) then
  begin
    status := rangeErr;
    result := '';
  end
  else
  begin
    subString := copy(prTransducer, position * 80 + 1, 80);
    result := TrimRight(subString);
  end;
end;

procedure TEDFDoc.SetPhysDim(const position: integer; const dimension: str8);
var
  filledString: str8;
  ns: integer;
begin
  if not TryStrToInt(prNumOfSignals, ns) then
    status := strFormatErr
  else if position > (ns - 1) then // outside range?
  begin
    status := rangeErr;
  end
  else
  begin
    if length(prPhysDim) < ns * 8 then // Label string to short?
      prPhysDim := PadRight(prPhysDim, ns * 8);
    filledString := PadRight(dimension, 8); // fill with spaces for length 16
    prPhysDim := StuffString(prPhysDim, position * 8 + 1, 8, filledString);
  end;
end;

function TEDFDoc.GetPhysDim(const position: integer): Str8;
var
  subString: Str8;
  ns: integer;
begin
  if not TryStrToInt(prNumOfSignals, ns) then
    status := strFormatErr
  else if position > (ns - 1) then
  begin
    status := rangeErr;
    result := '';
  end
  else
  begin
    subString := copy(prPhysDim, position * 8 + 1, 8);
    result := TrimRight(subString);
  end;
end;

constructor TEDFDoc.Create;
begin
  inherited Create;
  status := 0;
  prVersion := kEDFVersion;
  prNumOfDataRecs := kUnknown;
  prLocalPatID := kEmpty80;
  prLocalRecID := kEmpty80;
  prStartDate := kDefaultDate;
  prStartTime := kDefaultTime;
  prNumOfBytes := kZero8;
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
  CompileHeaderText;
end;

destructor TEDFDoc.Destroy;
begin
  inherited Destroy;
end;

end.
