unit EDF;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ EDF base unit }

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
  TRawDataRecord = Array of array of array of SmallInt;
  TScaledDataRecord = Array of array of array of Single;

const

  EDFEngine_major = 1;
  EDFEngine_minor = 0;
  EDFEngine_release = 0;
  EDFEngine_patch = 0;
  EDFEngine_fullversion = ((EDFEngine_major *  100 + EDFEngine_minor) * 100 + EDFEngine_release) * 100 + EDFEngine_patch;
  EDFEngine_version = '1.0.0.0';
  EDFEngine_internalversion = 'Alpha Centauri';

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
  kEmpty4         : str4 = '    ';
  kEmpty8         : str8 = '        ';
  kEmpty16        : str16 = '                ';
  kEmpty44        : str44 = '                                            ';
  kEmpty80        : str80 = '                                                                                ';
  kZero4          : str4 = '0000';
  kZero8          : str8 = '00000000';
  kDefaultDate    : str8 = '01.01.85';
  kDefaultTime    : str8 = '00.00.00';
  kStartDate      = 'Startdate';
  kEDFAnnotations = 'EDF Annotations';

  kVersionPos     = 1;
  kLocalPatIDPos  = 9;
  kLocalRecIDPos  = 89;
  kStartDatePos   = 169;
  kStartTimePos   = 177;
  kNumOfBytesPos  = 185;
  kReservedPos    = 193;
  kNumOfDRecsPos  = 237;
  kDurOfDataPos   = 245;
  kNumOfSigPos    = 253;
  kVarStartPos    = kNumOfSigPos + 4;

  kMaxRecordBytes = 61440;

type

{ TEDFDoc }

TEDFDoc = class
  private
    FHeaderText: AnsiString;     // Header record
    FRawDataRecord: TRawDataRecord;    // digitizer output, dr * ns * nsa
    FScaledDataRecord: TScaledDataRecord;  // reconstructed biological data
    { Fields of EDF and EDF+ header record: }
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
    { Official EDF/EDF+ header record fields end here. }
    status:    integer;
  protected
    procedure CompileHeaderText;
    procedure ParseHeaderText;
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
    function GetReserved: Str44;
    procedure SetReserved(const ReservedStr: Str44);
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
    function ValidPosition(const position: integer; var ns: integer): boolean;
    procedure SetLabel(const position: integer; const theLabel: str16);
    function GetLabel(const position: integer): str16;
    procedure SetTransducer(const position: integer; const transducer: str80);
    function GetTransducer(const position: integer): str80;
    procedure SetPhysDim(const position: integer; const dimension: str8);
    function GetPhysDim(const position: integer): Str8;
    procedure SetPhysMin(const position: integer; const physmin: str8);
    procedure SetPhysMin(const position: integer; const physmin: extended);
    function GetPhysMin(const position: integer): Str8;
    function eGetPhysMin(const position: integer): extended;
    procedure SetPhysMax(const position: integer; const physmax: str8);
    procedure SetPhysMax(const position: integer; const physmax: extended);
    function GetPhysMax(const position: integer): Str8;
    function eGetPhysMax(const position: integer): extended;
    procedure SetDigMin(const position: integer; const digmin: str8);
    procedure SetDigMin(const position: integer; const digmin: longint);
    function GetDigMin(const position: integer): Str8;
    function iGetDigMin(const position: integer): longint;
    procedure SetDigMax(const position: integer; const digmax: str8);
    procedure SetDigMax(const position: integer; const digmax: longint);
    function GetDigMax(const position: integer): Str8;
    function iGetDigMax(const position: integer): longint;
    procedure SetPrefilter(const position: integer; const prefilter: str80);
    function GetPrefilter(const position: integer): str80;
    procedure SetNumOfSamples(const position: integer; const numOfSamples: str8);
    procedure SetNumOfSamples(const position: integer; const numOfSamples: longint);
    function GetNumOfSamples(const position: integer): Str8;
    function iGetNumOfSamples(const position: integer): longint;
    procedure SetReserved2(const position: integer; const Reserved2Str: Str32);
    function GetReserved2(const position: integer): Str32;
    function GetAdjustmentFactor(const position: integer): extended;
    procedure DimDataRecord;
    function GetScaled(const aRecord: longint; aSignal: integer; aSample: longint): Single;
    function GetUnscaled(const aRecord: longint; aSignal: integer; aSample: longint): SmallInt;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Error;
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
    property Reserved: Str44 Read GetReserved Write SetReserved;
    property NumOfDataRecs: Str8 Read GetNumOfDataRecs Write SetNumOfDataRecs;
    property iNumOfDataRecs: longint Read iGetNumOfDataRecs Write SetNumOfDataRecs;
    property DurationOfData: Str8 Read GetDurOfData Write SetDurOfData;
    property iDurationOfData: longint Read iGetDurOfData Write SetDurOfData;
    property NumOfSignals: Str4 read GetNumOfSignals Write SetNumOfSignals;
    property iNumOfSignals: integer Read iGetNumOfSignals Write SetNumOfSignals;
    property SignalLabel[i: integer]: Str16 read GetLabel Write SetLabel;
    property Transducer[i: integer]: Str80 read GetTransducer Write SetTransducer;
    property PhysDim[i: integer]: Str8 read GetPhysDim Write SetPhysDim;
    property PhysMin[i: integer]: Str8 read GetPhysMin Write SetPhysMin;
    property ePhysMin[i: integer]: extended read eGetPhysMin Write SetPhysMin;
    property PhysMax[i: integer]: Str8 read GetPhysMax Write SetPhysMax;
    property ePhysMax[i: integer]: extended read eGetPhysMax Write SetPhysMax;
    property digMin[i: integer]: Str8 read GetdigMin Write SetdigMin;
    property idigMin[i: integer]: longint read iGetdigMin Write SetdigMin;
    property digMax[i: integer]: Str8 read GetdigMax Write SetdigMax;
    property idigMax[i: integer]: longint read iGetdigMax Write SetdigMax;
    property Prefilter[i: integer]: Str80 read GetPrefilter Write SetPrefilter;
    property NumOfSamples[i: integer]: Str8 read GetNumOfSamples Write SetNumOfSamples;
    property iNumOfSamples[i: integer]: longint read iGetNumOfSamples Write SetNumOfSamples;
    property Reserved2[i: integer]: Str32 Read GetReserved2 Write SetReserved2;
    property RawDataRecord: TRawDataRecord Read FRawDataRecord Write FRawDataRecord;
    property ScaledDataRecord: TScaledDataRecord Read FScaledDataRecord Write FScaledDataRecord;
    property AdjustmentFactor[i: integer]: extended Read GetAdjustmentFactor;
    property Scaled[aRecord: longint; aSignal: integer; sSample: longint]: single Read GetScaled;
    property UnScaled[aRecord: longint; aSignal: integer; sSample: longint]: SmallInt Read GetUnscaled;
    property StatusCode: integer Read status;
    procedure ReadFromFile(const aFileName: AnsiString);
    procedure ReadFromStream(const aStream: TStream);
  public
    DataSize, TotalSize: longint;
  end;

procedure ReadEDFFile(var EDFDoc: TEDFDoc; aStream: TStream; const aBaseURI: AnsiString); overload;
procedure ReadEDFFile(var EDFDoc: TEDFDoc; aStream: TStream); overload;
procedure ReadEDFFile(var EDFDoc: TEDFDoc; const aFileName: AnsiString); overload;
procedure ReadNewEDFFile(out EDFDoc: TEDFDoc; const aFileName: AnsiString);
procedure WriteEDFFile(var EDFDoc: TEDFDoc; aStream: TStream; const aBaseURI: AnsiString); overload;
procedure WriteEDFFile(var EDFDoc: TEDFDoc; const aFileName: AnsiString); overload;

implementation

procedure ReadHeaderRecord(var EDFDoc: TEDFDoc; mstream: TMemoryStream);
{ reads a header record from a given memory stream }
var
  sstream: TStringStream;
  headerLength, version: str8;
  iHeaderLength: longint;
begin
  headerLength := kEmpty8;
  sstream := TStringStream.Create('');
  mstream.Seek(kVersionPos - 1, soFromBeginning);
  mstream.ReadBuffer(version[1], 8);
  version := str8(version);
  if version = kEDFVersion then
  begin
    mstream.Seek(kNumOfBytesPos - 1, soFromBeginning);
    mstream.ReadBuffer(headerLength[1], 8);
    headerLength := Trim(headerLength);
    mstream.Seek(0, soFromBeginning);
    if TryStrToInt(headerLength, iHeaderLength) then
    begin
      sstream.CopyFrom(mstream, iHeaderLength);
      sstream.Seek(0, soFromBeginning);
      EDFDoc.FHeaderText := sstream.ReadString(iHeaderLength);
      EDFDoc.ParseHeaderText;
    end
    else
    begin
      raise EConvertError.Create('');
      EDFDoc.status := headermalform;
      EDFDoc.Error;
    end;
  end
  else
    begin
      EDFDoc.status := unsuppVers;
      EDFDoc.prVersion := version;
      EDFDoc.Error;
    end;
  sstream.Free;
end;

procedure ReadDataRecords(var EDFDoc: TEDFDoc; mstream: TMemoryStream);
var
  i, k, m: longint;
  imax, kmax: longint;
  j: integer;
  jmax: integer;
  rawValue: SmallInt;
  calibrator, physmins: array of extended;
  digmins: array of longint;
begin
  if assigned(EDFDoc) and assigned(mstream) and (mstream.Size > 0) then
  begin
    imax := EDFDoc.iNumOfDataRecs;
    jmax := EDFDoc.iNumOfSignals;
    kmax := EDFDoc.iNumOfSamples[0]; // maximum number of samples over all signals
    SetLength(calibrator, jmax);
    SetLength(physmins, jmax);
    SetLength(digmins, jmax);
    for j := 0 to jmax - 1 do
    begin
      m := EDFDoc.iNumOfSamples[j];
      if m > kmax then
        kmax := m;
      calibrator[j] := EDFDoc.AdjustmentFactor[j]; // pre-calculating these
      physmins[j] := EDFDoc.ePhysMin[j];           // values improves speed
      digmins[j] := EDFDoc.idigMin[j];
    end;
    SetLength(EDFDoc.FRawDataRecord, imax, jmax, kmax);
    SetLength(EDFDoc.FScaledDataRecord, imax, jmax, kmax);
    EDFDoc.DataSize := imax * jmax * kmax * SizeOf(SmallInt);
    EDFDoc.TotalSize := EDFDoc.iGetNumOfBytes + EDFDoc.DataSize;
    mstream.Seek(EDFDoc.iGetNumOfBytes, soFromBeginning);
    for i := 0 to imax - 1 do  // Records
    for j := 0 to jmax - 1 do  // Signals
    for k := 0 to EDFDoc.iNumOfSamples[j] - 1 do  // Samples
    begin
      mstream.Read(rawValue, 2);
      EDFDoc.FRawDataRecord[i, j, k] := LEtoN(rawValue);
      EDFDoc.ScaledDataRecord[i, j, k] := physmins[j] +
        calibrator[j] * (EDFDoc.FRawDataRecord[i, j, k] - digmins[j]);
      {Much slower alternative: EDFDoc.ScaledDataRecord[i, j, k] :=
        EDFDoc.Scaled[i, j, k];}
    end;
  end;
end;

procedure ReadEDFFile(var EDFDoc: TEDFDoc; aStream: TStream;
  const aBaseURI: AnsiString);
{ reads and parses an EDF file from an URI }
var
  mstream: TMemoryStream;
begin
  mstream := TMemoryStream.Create;
  if aStream.Size > 0 then
  begin
    aStream.Position := 0;
    mstream.CopyFrom(aStream, aStream.Size);
    ReadHeaderRecord(EDFDoc, mstream);
    if EDFDoc.status = noErr then
      ReadDataRecords(EDFDoc, mstream);
  end
  else
  begin
    EDFDoc.status := readErr; { create empty document with status code 7 }
  end;
  mstream.Free;
end;

procedure ReadEDFFile(var EDFDoc: TEDFDoc; aStream: TStream);
begin
  REadEDFFile(EDFDoc, aStream, 'stream:');
end;

procedure ReadEDFFile(var EDFDoc: TEDFDoc; const aFileName: AnsiString);
{ reads and parses and EDF file from a file spsecified by name }
var
  mstream: TMemoryStream;
begin
  mstream := TMemoryStream.Create;
  try
    mstream.LoadFromFile(aFileName);
    ReadHeaderRecord(EDFDoc, mstream);
    if EDFDoc.status = noErr then
      ReadDataRecords(EDFDoc, mstream);
  except
    on E:Exception do
    begin
      EDFDoc.status := readErr; { create empty document with status code 7 }
    end;
  end;
  mstream.Free;
end;

procedure ReadNewEDFFile(out EDFDoc: TEDFDoc; const aFileName: AnsiString);
{ reads and parses and EDF file from a file spsecified by name }
begin
  EDFDoc := TEDFDoc.Create;
  ReadEDFFile(EDFDoc, aFileName);
end;

procedure WriteEDFFile(var EDFDoc: TEDFDoc; aStream: TStream;
  const aBaseURI: AnsiString);
begin
  { TODO -oJWD : still to be implemented }
  { NtoLE }
end;

procedure WriteEDFFile(var EDFDoc: TEDFDoc; const aFileName: AnsiString);
begin
  { TODO -oJWD : still to be implemented }
  { NtoLE }
end;

procedure TEDFDoc.CompileHeaderText;
begin
  CalcHeaderLength;
  FHeaderText := HeaderString;
end;

procedure TEDFDoc.ParseHeaderText;
var
  ns: integer;
begin
  prVersion := ExtractedHeaderText(kVersionPos, 8);
  prLocalPatID := ExtractedHeaderText(kLocalPatIDPos, 80);  ;
  prLocalRecID := ExtractedHeaderText(kLocalRecIDPos, 80);
  prStartDate := ExtractedHeaderText(kStartDatePos, 8);
  prStartTime := ExtractedHeaderText(kStartTimePos, 8);
  prNumOfBytes := ExtractedHeaderText(kNumOfBytesPos, 8);
  prReserved := '';
  prNumOfDataRecs := ExtractedHeaderText(kNumOfDRecsPos, 8);
  prDurOfData := ExtractedHeaderText(kDurOfDataPos, 8);
  prNumOfSignals := ExtractedHeaderText(kNumOfSigPos, 4);
  if not TryStrToInt(Trim(prNumOfSignals), ns) then
    begin
      status := strFormatErr;
      prLabel := '';
      prTransducer := '';
      prPhysDim := '';
      prPhysMin := '';
      prPhysMax := '';
      prDigMin := '';
      prDigMax := '';
      prPrefilter := '';
      prNumOfSamples := '';
      prReserved2 := '';
    end
  else
  begin
    prLabel := ExtractedHeaderText(kVarStartPos, ns * 16);
    prTransducer := ExtractedHeaderText(kVarStartPos + ns * 16, ns * 80);   ;
    prPhysDim := ExtractedHeaderText(kVarStartPos + ns * (16 + 80), ns * 8);
    prPhysMin := ExtractedHeaderText(kVarStartPos + ns * (16 + 80 + 8), ns * 8);
    prPhysMax := ExtractedHeaderText(kVarStartPos + ns * (16 + 80 + 16), ns * 8);
    prDigMin := ExtractedHeaderText(kVarStartPos + ns * (16 + 80 + 16 + 8), ns * 8);
    prDigMax := ExtractedHeaderText(kVarStartPos + ns * (16 + 80 + 16 + 16), ns * 8);
    prPrefilter := ExtractedHeaderText(kVarStartPos + ns * (16 + 80 + 16 + 24), ns * 80);
    prNumOfSamples := ExtractedHeaderText(kVarStartPos + ns * (16 + 80 + 16 + 24 + 80), ns * 8);
    prReserved2 := ExtractedHeaderText(kVarStartPos + ns * (16 + 80 + 16 + 24 + 80 + 8), ns * 8);
  end;
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
  prNumOfBytes := PadRight(IntToStr(headerLength), 8);
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
  Result := ExtractedHeaderText(kVersionPos, 8);
end;

function TEDFDoc.GetLocalPatID: Str80;
begin
  result := ExtractedHeaderText(kLocalPatIDPos, 80);
end;

procedure TEDFDoc.SetLocalPatID(const ID: Str80);
begin
  prLocalPatID := PadRight(ID, 80);
  CompileHeaderText;
end;

function TEDFDoc.GetLocalRecID: Str80;
begin
  result := ExtractedHeaderText(kLocalRecIDPos, 80);
end;

procedure TEDFDoc.SetLocalRecID(const ID: Str80);
begin
  prLocalRecID := PadRight(ID, 80);
  CompileHeaderText;
end;

function TEDFDoc.GetStartDate: Str8;
begin
  result := ExtractedHeaderText(kStartDatePos, 8);
end;

function TEDFDoc.dGetStartDate: tDateTime;
var
  sdString: Str8;
  theFormat: TFormatSettings;
begin
  theFormat.DateSeparator := '.';
  theFormat.ShortDateFormat := 'dd.mm.yy';
  theFormat.TwoDigitYearCenturyWindow := 85; // EDF convention
  sdString := GetStartDate;
  if not TryStrToDate(sdString, result, theFormat) then
    status := strFormatErr;
end;

procedure TEDFDoc.SetStartDate(const DateStr: Str8);
begin
  if length(DateStr) <> 8 then
  begin
    status := createErr;
    prStartDate := '';
  end
  else
  begin
  prStartDate := DateStr;
  end;
  CompileHeaderText;
end;

procedure TEDFDoc.SetStartDate(const Date: tDateTime);
var
  sdString: Str8;
  oldFormatSettings: TFormatSettings;
begin
  oldFormatSettings := DefaultFormatSettings;
  DefaultFormatSettings.TwoDigitYearCenturyWindow := 85; // EDF convention
  sdString := FormatDateTime('dd.mm.yy', Date);
  SetStartDate(sdString);
  DefaultFormatSettings := oldFormatSettings;
end;

function TEDFDoc.GetStartTime: Str8;
begin
  result := ExtractedHeaderText(kStartTimePos, 8);
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
  if length(TimeStr) <> 8 then
  begin
    status := createErr;
    prStartTime := '';
  end
  else
  begin
    prStartTime := TimeStr;
  end;
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
  result := ExtractedHeaderText(kNumOfBytesPos, 8);
end;

function TEDFDoc.iGetNumOfBytes: longint;
var
  nbString: Str8;
begin
  nbString := GetNumOfBytes;
  if not TryStrToInt(Trim(nbString), result) then
    status := strFormatErr;
end;

function TEDFDoc.GetReserved: Str44;
begin
  result := ExtractedHeaderText(kReservedPos, 44);
end;

procedure TEDFDoc.SetReserved(const ReservedStr: Str44);
begin
  prReserved := PadRight(ReservedStr, 44);
  CompileHeaderText;
end;

function TEDFDoc.GetNumOfDataRecs: Str8;
begin
  result := ExtractedHeaderText(kNumOfDRecsPos, 8);
end;

function TEDFDoc.iGetNumOfDataRecs: longint;
var
  nrString: Str8;
begin
  nrString := GetNumOfDataRecs;
  if not TryStrToInt(Trim(nrString), result) then
    status := strFormatErr;
end;

procedure TEDFDoc.SetNumOfDataRecs(const NumOfRecs: Str8);
begin
  prNumOfDataRecs := PadRight(NumOfRecs, 8);
  CompileHeaderText;
  DimDataRecord;
end;

procedure TEDFDoc.SetNumOfDataRecs(const nr: longint);
var
  nrString: Str8;
begin
  if (nr < 0) or (nr > 99999999) then begin
    status := rangeErr;
    nrString := FloatToStr(NaN);
  end
  else if nr = 0 then
    nrString := kUnknown
  else
    nrString := IntToStr(nr);
  SetNumOfDataRecs(nrString);
end;

function TEDFDoc.GetDurOfData: Str8;
begin
  result := ExtractedHeaderText(kDurOfDataPos, 8);
end;

function TEDFDoc.iGetDurOfData: longint;
var
  ddString: Str8;
begin
  ddString := GetDurOfData;
  if not TryStrToInt(Trim(ddString), result) then
    status := strFormatErr;
end;

procedure TEDFDoc.SetDurOfData(const duration: Str8);
begin
  prDurOfData := PadRight(duration, 8);
  CompileHeaderText;
end;

procedure TEDFDoc.SetDurOfData(const dd: longint);
var
  ddString: Str8;
begin
  if (dd < 0) or (dd > 99999999) then begin
    status := rangeErr;
    ddString := FloatToStr(NaN);
  end
  else
    ddString := IntToStr(dd);
  SetDurOfData(ddString);
end;

function TEDFDoc.GetNumOfSignals: Str4;
begin
  result := ExtractedHeaderText(kNumOfSigPos, 4);
end;

function TEDFDoc.iGetNumOfSignals: integer;
var
  nsString: Str4;
begin
  nsString := GetNumOfSignals;
  if not TryStrToInt(Trim(nsString), result) then
    status := strFormatErr;
end;

procedure TEDFDoc.SetNumOfSignals(const ns: Str4);
var
  ins: integer;
begin
  if not TryStrToInt(Trim(ns), ins) then // valid number representation?
    begin
      status := strFormatErr;
    end
  else
  begin
    if length(prLabel) < ins * 16 then // Label string too short?
      prLabel := PadRight(prLabel, ins * 16);
    if length(prTransducer) < ins * 80 then // Transducer string too short?
      prTransducer := PadRight(prTransducer, ins * 80);
    if length(prPhysDim) < ins * 8 then // PhysDim string too short?
      prPhysDim := PadRight(prPhysDim, ins * 8);
    if length(prPhysMin) < ins * 8 then // PhysMin string too short?
      prPhysMin := PadRight(prPhysMin, ins * 8);
    if length(prPhysMax) < ins * 8 then // PhysMax string too short?
      prPhysMax := PadRight(prPhysMax, ins * 8);
    if length(prDigMin) < ins * 8 then // DigMin string too short?
      prDigMin := PadRight(prDigMin, ins * 8);
    if length(prDigMax) < ins * 8 then // DigMax string too short?
      prDigMax := PadRight(prDigMax, ins * 8);
    if length(prPrefilter) < ins * 80 then // Prefilter string too short?
      prPrefilter := PadRight(prPrefilter, ins * 80);
    if length(prNumOfSamples) < ins * 8 then // PhysMin string too short?
      prNumOfSamples := PadRight(prNumOfSamples, ins * 8);
    if length(prReserved2) < ins * 32 then // Prefilter string too short?
      prReserved2 := PadRight(prReserved2, ins * 32);
    prNumOfSignals := PadRight(ns, 4);
    CompileHeaderText;
    DimDataRecord;
  end;
end;

procedure TEDFDoc.SetNumOfSignals(const ns: integer);
var
  nsString: Str4;
begin
  if (ns < 0) or (ns > 9999) then begin
    status := rangeErr;
    nsString := FloatToStr(NaN);
  end
  else
    nsString := IntToStr(ns);
  SetNumOfSignals(nsString);
end;

function TEDFDoc.ValidPosition(const position: integer; var ns: integer): boolean;
{ Checks if index position for addressing signals is valid }
begin
  ns := 0;
  if not TryStrToInt(Trim(NumOfSignals), ns) then // valid number representation?
    begin
      status := strFormatErr;
      result := false;
    end
  else if position > (ns - 1) then // outside range?
  begin
    status := rangeErr;
    result := false;
  end
  else // no errors
    result := true;
end;

procedure TEDFDoc.SetLabel(const position: integer; const theLabel: str16);
var
  filledString: str16;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    if length(prLabel) < ns * 16 then // Label string too short?
      prLabel := PadRight(prLabel, ns * 16);
    filledString := PadRight(theLabel, 16); // fill with spaces for length 16
    prLabel := StuffString(prLabel, position * 16 + 1, 16, filledString);
    CompileHeaderText;
  end;
end;

function TEDFDoc.GetLabel(const position: integer): str16;
var
  subString: Str16;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    subString := copy(prLabel, position * 16 + 1, 16);
    result := Trim(subString);
  end
  else
    result := '';
end;

procedure TEDFDoc.SetTransducer(const position: integer; const transducer: str80
  );
var
  filledString: str80;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    if length(prTransducer) < ns * 80 then // Transducer string too short?
      prTransducer := PadRight(prTransducer, ns * 80);
    filledString := PadRight(transducer, 80); // fill with spaces for length 80
    prTransducer := StuffString(prTransducer, position * 80 + 1, 80, filledString);
    CompileHeaderText;
  end;
end;

function TEDFDoc.GetTransducer(const position: integer): str80;
var
  subString: Str80;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    subString := copy(prTransducer, position * 80 + 1, 80);
    result := Trim(subString);
  end
  else
    result := '';
end;

procedure TEDFDoc.SetPhysDim(const position: integer; const dimension: str8);
var
  filledString: str8;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    if length(prPhysDim) < ns * 8 then // PhysDim string too short?
      prPhysDim := PadRight(prPhysDim, ns * 8);
    filledString := PadRight(dimension, 8); // fill with spaces for length 8
    prPhysDim := StuffString(prPhysDim, position * 8 + 1, 8, filledString);
    CompileHeaderText;
  end;
end;

function TEDFDoc.GetPhysDim(const position: integer): Str8;
var
  subString: Str8;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    subString := copy(prPhysDim, position * 8 + 1, 8);
    result := Trim(subString);
  end
  else
    result := '';
end;

procedure TEDFDoc.SetPhysMin(const position: integer; const physmin: str8);
var
  filledString: str8;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    if length(prPhysMin) < ns * 8 then // PhysMin string too short?
      prPhysMin := PadRight(prPhysMin, ns * 8);
    filledString := PadRight(physmin, 8); // fill with spaces for length 8
    prPhysMin := StuffString(prPhysMin, position * 8 + 1, 8, filledString);
    CompileHeaderText;
  end;
end;

procedure TEDFDoc.SetPhysMin(const position: integer; const physmin: extended);
var
  pmString: Str8;
  oldFormatSettings: TFormatSettings;
begin
  if (physMin > 99999999) or (physMin < -9999999) then begin
    status := rangeErr;
    pmString := FloatToStr(NaN);
  end
  else
  begin
    oldFormatSettings := DefaultFormatSettings;
    DefaultFormatSettings.DecimalSeparator := '.';
    pmString := FloatToStr(physMin);
    DefaultFormatSettings := oldFormatSettings;
  end;
  SetPhysMin(position, pmString);
end;

function TEDFDoc.GetPhysMin(const position: integer): Str8;
var
  subString: Str8;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    subString := copy(prPhysMin, position * 8 + 1, 8);
    result := Trim(subString);
  end
  else result := '';
end;

function TEDFDoc.eGetPhysMin(const position: integer): extended;
var
  pmString: Str8;
  oldFormatSettings: TFormatSettings;
begin
  pmString := GetPhysMin(position);
  oldFormatSettings := DefaultFormatSettings;
  DefaultFormatSettings.DecimalSeparator := '.';
  if not TryStrToFloat(Trim(pmString), result) then
    status := strFormatErr;
  DefaultFormatSettings := oldFormatSettings;
end;

procedure TEDFDoc.SetPhysMax(const position: integer; const physmax: str8);
var
  filledString: str8;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    if length(prPhysMax) < ns * 8 then // PhysMax string too short?
      prPhysMax := PadRight(prPhysMax, ns * 8);
    filledString := PadRight(physmax, 8); // fill with spaces for length 8
    prPhysMax := StuffString(prPhysMax, position * 8 + 1, 8, filledString);
    CompileHeaderText;
  end;
end;

procedure TEDFDoc.SetPhysMax(const position: integer; const physmax: extended);
var
  pmString: Str8;
  oldFormatSettings: TFormatSettings;
begin
  if (physMax > 99999999) or (physMax < -9999999) then begin
    status := rangeErr;
    pmString := FloatToStr(NaN);
  end
  else
  begin
    oldFormatSettings := DefaultFormatSettings;
    DefaultFormatSettings.DecimalSeparator := '.';
    pmString := FloatToStr(physMax);
    DefaultFormatSettings := oldFormatSettings;
  end;
  SetPhysMax(position, pmString);
end;

function TEDFDoc.GetPhysMax(const position: integer): Str8;
var
  subString: Str8;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    subString := copy(prPhysMax, position * 8 + 1, 8);
    result := Trim(subString);
  end
  else result := '';
end;

function TEDFDoc.eGetPhysMax(const position: integer): extended;
var
  pmString: Str8;
  oldFormatSettings: TFormatSettings;
begin
  pmString := GetPhysMax(position);
  oldFormatSettings := DefaultFormatSettings;
  DefaultFormatSettings.DecimalSeparator := '.';
  if not TryStrToFloat(Trim(pmString), result) then
    status := strFormatErr;
  DefaultFormatSettings := oldFormatSettings;
end;

procedure TEDFDoc.SetDigMin(const position: integer; const digmin: str8);
var
  filledString: str8;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    if length(prDigMin) < ns * 8 then // DigMin string too short?
      prDigMin := PadRight(prDigMin, ns * 8);
    filledString := PadRight(digmin, 8); // fill with spaces for length 8
    prDigMin := StuffString(prDigMin, position * 8 + 1, 8, filledString);
    CompileHeaderText;
  end;
end;

procedure TEDFDoc.SetDigMin(const position: integer; const digmin: longint);
var
  dmString: Str8;
begin
  if (digmin > 99999999) or (digmin < -9999999) then begin
    status := rangeErr;
    dmString := FloatToStr(NaN);
  end
  else
    dmString := IntToStr(digMin);
  SetDigMin(position, dmString);
end;

function TEDFDoc.GetDigMin(const position: integer): Str8;
var
  subString: Str8;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    subString := copy(prDigMin, position * 8 + 1, 8);
    result := Trim(subString);
  end
  else result := '';
end;

function TEDFDoc.iGetDigMin(const position: integer): longint;
var
  dmString: Str8;
begin
  dmString := GetDigMin(position);
  if not TryStrToInt(Trim(dmString), result) then
    status := strFormatErr;
end;

procedure TEDFDoc.SetDigMax(const position: integer; const digmax: str8);
var
  filledString: str8;
  ns: integer;
  idm: longint;
begin
  if ValidPosition(position, ns) then
  begin
    if not TryStrToInt(Trim(digmax), idm) then
      status := strFormatErr
    else
    begin
      if idm <= iDigMin[position] then // EDF+ dditional specification #5
        status := sizemismatch
      else
      begin
        if length(prDigMax) < ns * 8 then // DigMax string too short?
          prDigMax := PadRight(prDigMax, ns * 8);
        filledString := PadRight(digmax, 8); // fill with spaces for length 8
        prDigMax := StuffString(prDigMax, position * 8 + 1, 8, filledString);
        CompileHeaderText;
      end;
    end;
  end;
end;

procedure TEDFDoc.SetDigMax(const position: integer; const digmax: longint);
var
  dmString: Str8;
begin
  if (digmax > 99999999) or (digmax < -9999999) then begin
    status := rangeErr;
    dmString := FloatToStr(NaN);
  end
  else
    dmString := IntToStr(digMax);
  SetDigMax(position, dmString);
end;

function TEDFDoc.GetDigMax(const position: integer): Str8;
var
  subString: Str8;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    subString := copy(prDigMax, position * 8 + 1, 8);
    result := Trim(subString);
  end
  else result := '';
end;

function TEDFDoc.iGetDigMax(const position: integer): longint;
var
  dmString: Str8;
begin
  dmString := GetDigMax(position);
  if not TryStrToInt(Trim(dmString), result) then
    status := strFormatErr;
end;

procedure TEDFDoc.SetPrefilter(const position: integer; const prefilter: str80);
var
  filledString: str80;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    if length(prPrefilter) < ns * 80 then // Prefilter string too short?
      prPrefilter := PadRight(prPrefilter, ns * 80);
    filledString := PadRight(prefilter, 80); // fill with spaces for length 80
    prPrefilter := StuffString(prPrefilter, position * 80 + 1, 80, filledString);
    CompileHeaderText;
  end;
end;

function TEDFDoc.GetPrefilter(const position: integer): str80;
var
  subString: Str80;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    subString := copy(prPrefilter, position * 80 + 1, 80);
    result := Trim(subString);
  end
  else
    result := '';
end;

procedure TEDFDoc.SetNumOfSamples(const position: integer;
  const numOfSamples: str8);
var
  filledString: str8;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    if length(prNumOfSamples) < ns * 8 then // PhysMin string too short?
      prNumOfSamples := PadRight(prNumOfSamples, ns * 8);
    filledString := PadRight(numOfSamples, 8); // fill with spaces for length 8
    prNumOfSamples := StuffString(prNumOfSamples, position * 8 + 1, 8, filledString);
    CompileHeaderText;
    DimDataRecord;
  end;
end;

procedure TEDFDoc.SetNumOfSamples(const position: integer;
  const numOfSamples: longint);
var
  nsaString: Str8;
begin
  if (numOfSamples < 0) or (numOfSamples > 99999999) then begin
    status := rangeErr;
    nsaString := FloatToStr(NaN);
  end
  else
    nsaString := IntToStr(numOfSamples);
  SetNumOfSamples(position, nsaString);
end;

function TEDFDoc.GetNumOfSamples(const position: integer): Str8;
var
  subString: Str8;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    subString := copy(prNumOfSamples, position * 8 + 1, 8);
    result := Trim(subString);
  end
  else result := '';
end;

function TEDFDoc.iGetNumOfSamples(const position: integer): longint;
var
  nsaString: Str8;
begin
  nsaString := GetNumOfSamples(position);
  if not TryStrToInt(Trim(nsaString), result) then
    status := strFormatErr;
end;

procedure TEDFDoc.SetReserved2(const position: integer;
  const Reserved2Str: Str32);
var
  filledString: str80;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    if length(prReserved2) < ns * 32 then // Prefilter string too short?
      prReserved2 := PadRight(prReserved2, ns * 32);
    filledString := PadRight(prReserved2, 32); // fill with spaces for length 32
    prReserved2 := StuffString(prReserved2, position * 32 + 1, 32, filledString);
    CompileHeaderText;
  end;
end;

function TEDFDoc.GetReserved2(const position: integer): Str32;
var
  subString: Str32;
  ns: integer;
begin
  if ValidPosition(position, ns) then
  begin
    subString := copy(prReserved2, position * 32 + 1, 32);
    result := Trim(subString);
  end
  else
    result := '';
end;

function TEDFDoc.GetAdjustmentFactor(const position: integer): extended;
begin
  result := (ePhysMax[position] - ePhysMin[position]) /
            (iDigMax[position] - iDigMin[Position]);
end;

procedure TEDFDoc.DimDataRecord;
var
  imax, kmax: longint;
  jmax: integer;
  j: integer;
  m: longint;
begin
  imax := iNumOfDataRecs;
  jmax := iNumOfSignals;
  kmax := iNumOfSamples[0]; // maximum number of samples over all signals
  for j := 1 to jmax - 1 do
  begin
    m := iNumOfSamples[j];
    if m > kmax then
      kmax := m;
  end;
  if imax < 0 then imax := 0;
  if jmax < 0 then jmax := 0;
  if kmax < 0 then kmax := 0;
  SetLength(FRawDataRecord, imax, jmax, kmax);
  SetLength(FScaledDataRecord, imax, jmax, kmax);
 end;

function TEDFDoc.GetScaled(const aRecord: longint; aSignal: integer;
  aSample: longint): Single;
begin
  result := ePhysMin[aSignal] + AdjustmentFactor[aSignal] *
        (FRawDataRecord[aRecord, aSignal, aSample] - iDigMin[aSignal]);
end;

function TEDFDoc.GetUnscaled(const aRecord: longint; aSignal: integer;
  aSample: longint): SmallInt;
begin
  result := Round(iDigMin[aSignal] + (FScaledDataRecord[aRecord, aSignal, aSample] -
         ePhysMin[aSignal]) / AdjustmentFactor[aSignal]);
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
  prNumOfBytes := kEmpty8;
  prReserved := kEmpty44;
  prNumOfDataRecs := kUnknown;
  prDurOfData := kEmpty8;
  prNumOfSignals := kEmpty4;
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
  self := nil;
end;

procedure TEDFDoc.Error;
{ Creates an empty record for read errors }
begin
  prNumOfDataRecs := kUnknown;
  prLocalPatID := kEmpty80;
  prLocalRecID := kEmpty80;
  prStartDate := kUnknown;
  prStartTime := kUnknown;
  prNumOfBytes := kUnknown;
  prReserved := kEmpty44;
  prNumOfDataRecs := kUnknown;
  prDurOfData := kUnknown;
  prNumOfSignals := kEmpty4;
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

procedure TEDFDoc.ReadFromFile(const aFileName: AnsiString);
begin
  ReadEDFFile(self, aFileName);
end;

procedure TEDFDoc.ReadFromStream(const aStream: TStream);
begin
  ReadEDFFile(self, aStream);
end;

end.

