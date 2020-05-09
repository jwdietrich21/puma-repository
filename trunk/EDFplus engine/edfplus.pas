unit EDFplus;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ EDF+ base unit }

{ Version 1.0 (Aquila) }

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
  Classes, SysUtils, StrUtils, Math, DateUtils, EDF;

type

  str11 = string[11];
  str68 = string[68];

  TLocalPatRecord = record
    HospitalCode: str68;
    Sex: char;
    Name: str68;
    case UseDateTime: boolean of
      true: (dBirthDate: TDateTime);
      false: (sBirthDate: Str11);
  end;
  TLocalRecRecord = record
    HospitalAdminCode: str68;
    InvestigatorID: str68;
    Equipment: str68;
    case UseDateTime: boolean of
      true: (dStartDate: TDateTime);
      false: (sStartDate: Str11);
  end;
  TTALRecord = record
    onset, duration: real;
    comment: array of string;
  end;

  TRecordingType = (EDF_C, EDF_D, EDF_U);
  TAnnotation = array of array of TTALRecord;

const

  kStartDate       = 'Startdate';
  kContinuous      = 'EDF+C';
  kDiscontinuous   = 'EDF+D';
  kAnnotationsHead = 'EDF Annotations';

  kAnnotationsDMin = -32768;
  kAnnotationsDMax = 32767;
  kAnnotationsPMin = -1;
  kAnnotationsPMax = 1;

  OnsetMarker      = 21;
  DurationMarker   = 20;
  Terminator       = 0;

  TALOnset         = chr(OnsetMarker);
  TALDuration      = chr(DurationMarker);
  TALTerminator    = chr(Terminator);

type

  { TEDFplusDoc }

  TEDFplusDoc = class(TEDFDoc)
    private
      FAnnotations: TAnnotation;
    protected
      function GetLocalPatID: TLocalPatRecord;
      function dGetLocalPatID: TLocalPatRecord;
      procedure SetLocalPatID(const ID: TLocalPatRecord);
      function GetLocalRecID: TLocalRecRecord;
      function dGetLocalRecID: TLocalRecRecord;
      procedure SetLocalRecID(const ID: TLocalRecRecord);
      function GetRecordingType: TRecordingType;
      procedure SetRecordingType(const theType: TRecordingType);
      function GetAnnotations: TAnnotation;
      function GetAnnotation(const i: longint; const j: integer): TTALRecord;
      function GetRecordStart(const i: longint): real;
      procedure SetAnnotations;
    public
      constructor Create;
      destructor Destroy; override;
      property LocalPatID: TLocalPatRecord read GetLocalPatID write SetLocalPatID;
      property dLocalPatID: TLocalPatRecord read dGetLocalPatID write SetLocalPatID;
      property LocalRecID: TLocalRecRecord read GetLocalRecID write SetLocalRecID;
      property dLocalRecID: TLocalRecRecord read dGetLocalRecID write SetLocalRecID;
      property RecordingType: TRecordingType read GetRecordingType write SetRecordingType;
      property Annotation[aRecord: longint; index: integer]: TTALRecord read GetAnnotation;
      procedure AddAnnotation(const aRecord: longint; const theAnnotation: TTALRecord);
      property RecordStart[aRecord: longint]: real read GetRecordStart;
    end;


implementation

function TEDFplusDoc.GetLocalPatID: TLocalPatRecord;
var
  containerString: Str80;
begin
  containerString := inherited;
  Result.HospitalCode := AnsiReplaceText(ExtractDelimited(1, containerString, [' ']), '_', ' ');
  Result.Sex := char(ExtractDelimited(2, containerString, [' '])[1]);
  Result.sBirthDate := ExtractDelimited(3, containerString, [' ']);
  Result.Name := AnsiReplaceText(ExtractDelimited(4, containerString, [' ']), '_', ' ');
end;

function TEDFplusDoc.dGetLocalPatID: TLocalPatRecord;
var
  containerString: Str80;
  theDateString, MonthStr: String;
  theYear, theMonth, theDay: integer;
  theFormat: TFormatSettings;
begin
  theFormat := DefaultFormatSettings;
  theFormat.DateSeparator := '-';
  theFormat.ShortDateFormat := 'dd-mmm-yyyy';
  theFormat.ShortMonthNames := kShortEnglishMonths;
  containerString := inherited GetLocalPatID;
  Result.HospitalCode := AnsiReplaceText(ExtractDelimited(1, containerString, [' ']), '_', ' ');
  Result.Sex := char(ExtractDelimited(2, containerString, [' '])[1]);
  theDateString := ExtractDelimited(3, containerString, [' ']);
  if not TryStrToDate(theDateString, Result.dBirthDate, theFormat) then
  begin
    if not TryStrToInt(LeftStr(theDateString, 2), theDay) then
      self.status := strFormatErr;
    MonthStr := MidStr(theDateString, 4, 3);
    theMonth := 0;
    repeat
      theMonth := theMonth + 1;
    until (MonthStr = kShortEnglishMonths[theMonth]) or (theMonth > 11);
    if not TryStrToInt(RightStr(theDateString, 4), theYear) then
      self.status := strFormatErr;
    Result.dBirthDate := EncodeDate(theYear, theMonth, theDay);
  end;
  Result.Name := AnsiReplaceText(ExtractDelimited(4, containerString, [' ']), '_', ' ');
end;

procedure TEDFplusDoc.SetLocalPatID(const ID: TLocalPatRecord);
var
  containerString: Str80;
  theFormat: TFormatSettings;
begin
  theFormat := DefaultFormatSettings;
  if ID.UseDateTime or (ID.sBirthDate = '') then
  begin
    theFormat.DateSeparator := '-';
    theFormat.ShortDateFormat := 'dd-mmm-yyyy';
    theFormat.ShortMonthNames := kShortEnglishMonths;
    containerString := AnsiReplaceText(ID.HospitalCode, ' ', '_') + ' ' +
    ID.Sex + ' ' + FormatDateTime('dd-mmm-yyyy', ID.dBirthDate, theFormat) + ' ' +
    AnsiReplaceText(ID.Name, ' ', '_')
  end
  else
    containerString := AnsiReplaceText(ID.HospitalCode, ' ', '_') + ' ' +
      ID.Sex + ' ' + ID.sBirthDate + ' ' + AnsiReplaceText(ID.Name, ' ', '_');
  if length(containerString) <= 80 then
    inherited SetLocalPatID(containerString)
  else
    begin
      self.status := sizemismatch;
      inherited SetLocalPatID(kEmpty80);
    end;
end;

function TEDFplusDoc.GetLocalRecID: TLocalRecRecord;
var
  containerString: Str80;
begin
  containerString := inherited;
  Result.sStartDate := ExtractDelimited(2, containerString, [' ']);
  Result.HospitalAdminCode := AnsiReplaceText(ExtractDelimited(3, containerString, [' ']), '_', ' ');
  Result.InvestigatorID := AnsiReplaceText(ExtractDelimited(4, containerString, [' ']), '_', ' ');
  Result.Equipment := AnsiReplaceText(ExtractDelimited(5, containerString, [' ']), '_', ' ');
end;

function TEDFplusDoc.dGetLocalRecID: TLocalRecRecord;
var
  containerString: Str80;
  theDateString, MonthStr: String;
  theYear, theMonth, theDay: integer;
  theFormat: TFormatSettings;
begin
  theFormat := DefaultFormatSettings;
  theFormat.DateSeparator := '-';
  theFormat.ShortDateFormat := 'dd-mmm-yyyy';
  theFormat.ShortMonthNames := kShortEnglishMonths;
  containerString := inherited GetLocalRecID;
  theDateString := ExtractDelimited(2, containerString, [' ']);
  if not TryStrToDate(theDateString, Result.dStartDate, theFormat) then
  begin
    if not TryStrToInt(LeftStr(theDateString, 2), theDay) then
      self.status := strFormatErr;
    MonthStr := MidStr(theDateString, 4, 3);
    theMonth := 0;
    repeat
      theMonth := theMonth + 1;
    until (MonthStr = kShortEnglishMonths[theMonth]) or (theMonth > 11);
    if not TryStrToInt(RightStr(theDateString, 4), theYear) then
      self.status := strFormatErr;
    Result.dStartDate := EncodeDate(theYear, theMonth, theDay);
  end;
  Result.HospitalAdminCode := AnsiReplaceText(ExtractDelimited(3, containerString, [' ']), '_', ' ');
  Result.InvestigatorID := AnsiReplaceText(ExtractDelimited(4, containerString, [' ']), '_', ' ');
  Result.Equipment := AnsiReplaceText(ExtractDelimited(5, containerString, [' ']), '_', ' ');
end;

procedure TEDFplusDoc.SetLocalRecID(const ID: TLocalRecRecord);
var
  containerString: Str80;
  theFormat: TFormatSettings;
begin
  theFormat := DefaultFormatSettings;
  if ID.UseDateTime or (ID.sStartDate = '') then
  begin
    theFormat.DateSeparator := '-';
    theFormat.ShortDateFormat := 'dd-mmm-yyyy';
    theFormat.ShortMonthNames := kShortEnglishMonths;
    containerString := kStartDate + ' ' +
      FormatDateTime('dd-mmm-yyyy', ID.dStartDate, theFormat) + ' ' +
      AnsiReplaceText(ID.HospitalAdminCode, ' ', '_') + ' ' +
      AnsiReplaceText(ID.InvestigatorID, ' ', '_') + ' ' +
      AnsiReplaceText(ID.Equipment, ' ', '_') + ' ';
  end
  else
    containerString := kStartDate + ' ' +
      ID.sStartDate + ' ' +
      AnsiReplaceText(ID.HospitalAdminCode, ' ', '_') + ' ' +
      AnsiReplaceText(ID.InvestigatorID, ' ', '_') + ' ' +
      AnsiReplaceText(ID.Equipment, ' ', '_') + ' ';
  if length(containerString) <= 80 then
    inherited SetLocalRecID(containerString)
  else
    begin
      self.status := sizemismatch;
      inherited SetLocalRecID(kEmpty80);
    end;
end;

function TEDFplusDoc.GetRecordingType: TRecordingType;
begin
  if UpperCase(LeftStr(Reserved, 5)) = kContinuous then
    result := EDF_C
  else if UpperCase(LeftStr(Reserved, 5)) = kDiscontinuous then
    result := EDF_D
  else
    result := EDF_U;
end;

procedure TEDFplusDoc.SetRecordingType(const theType: TRecordingType);
begin
  case theType of
  EDF_C:
    Reserved := PadRight(kContinuous, 44);
  EDF_D:
    Reserved := PadRight(kDiscontinuous, 44);
  otherwise
    Reserved := kEmpty44;
  end;
end;

function TEDFplusDoc.GetAnnotations: TAnnotation;
var
  i, k: longint;
  imax, kmax: longint;
  j, l, m, n, s:    integer;
  jmax: integer;
  ch: smallint;
  subString, compString: String;
  theFormat: TFormatSettings;

  function emptyAnnotation: TTALRecord;
  begin
    SetLength(result.comment, 1);
    result.comment[0] := '';
    result.onset := -1;
    result.duration := -1;
  end;

  function NextCh: smallint;
  var
    ch: smallint;
  begin
    ch := RawDataRecord[i, l, k];
    inc(k);
    Result:=ch;
  end;

  function MaxComments: integer;
  var
    c1, c2: longint;
  begin
    result := 0;
    for c1 := 0 to imax - 1 do
    for c2 := 0 to kmax - 1 do
    begin
      if RawDataRecord[c1, l, c2] = 0 then
      inc(result);
    end
  end;

begin
  theFormat := DefaultFormatSettings;
  theFormat.DecimalSeparator := '.';
  imax := iNumOfDataRecs;
  jmax := iNumOfSignals;
  l := -1;
  k := 0;
  m := 0;
  s := 0;
  subString := '';
  for j := 0 to jmax - 1 do // find annotation signal
    begin
      if SignalLabel[j] = kAnnotationsHead then
        l := j;
    end;
  if l >= 0 then
  begin
    kmax := iNumOfSamples[l] * 2; // Number of chars = 2 * number of smallints
    SetLength(result, imax);
    SetLength(result[0], MaxComments);
    for i := 0 to imax - 1 do
      begin
        repeat // read timestamp offset of record start
          ch := NextCh;
          if ch < DurationMarker then
          begin
            status := annotErr;
            result[i,s] := emptyAnnotation;
            break;
          end;
          if (chr(ch) <> '+') and (chr(ch) <> '-') and (k < 2) then
          begin
            status := annotErr;
            result[i,s] := emptyAnnotation;
            break;
          end;
          while ch > 45 do
            begin
              subString := subString + chr(ch);
              ch := NextCh;
            end;
        until (ch = DurationMarker) or (k = kmax - 1);
        if ch = DurationMarker then
        begin
          ch := NextCh;
          if ch = DurationMarker then
            begin
              if TryStrToFloat(subString, result[i,s].duration, theFormat) then
                m := 1
              else
              begin
                status := annotErr;
                result[i,s] := emptyAnnotation;
                break;
              end
            end
          else
            begin
              status := annotErr;
              result[i,s] := emptyAnnotation;
              break;
            end;
          subString := '';
        end;
        repeat // read remaining annotations
          ch := NextCh;
          if ch < DurationMarker then
          begin
            status := annotErr;
            result[i,s] := emptyAnnotation;
            break;
          end;
          if (m = 0) and (chr(ch) <> '+') and (chr(ch) <> '-') and (k < 2) then
          begin
            status := annotErr;
            result[i,s] := emptyAnnotation;
            break;
          end;
          while ch > 42 do
            begin
              subString := subString + chr(ch);
              ch := NextCh;
            end;
          if ch = DurationMarker then
          begin
            if m = 0 then
            begin
              if TryStrToFloat(subString, result[i,s].duration, theFormat) then
                m := 1
              else
              begin
                status := annotErr;
                result[i,s] := emptyAnnotation;
                break;
              end
            end
            else
            begin
              compString := subString + #9;
              subString := '';
              inc(m);
            end;
          end;
          if ch = OnsetMarker then
          begin
            if TryStrToFloat(subString, result[i,s].onset, theFormat) then
              m := 1
            else
            begin
              status := annotErr;
              result[i,s] := emptyAnnotation;
              break;
            end
          end;
          if ch = Terminator then
          begin
            SetLength(result[i,s].comment, m);
            for n := 0 to m - 1 do
              result[i,s].comment[n] := ExtractDelimited(n, compString, [#9]);
            m := 0;
            inc(s);
          end;
        until k = kmax - 1;
      end;
  end;
end;

function TEDFplusDoc.GetAnnotation(const i: longint; const j: integer): TTALRecord;
begin
  if length(FAnnotations) = 0 then
    FAnnotations := GetAnnotations;
  result := FAnnotations[i, j];
end;

function TEDFplusDoc.GetRecordStart(const i: longint): real;
var
  anAnnotation: TTALRecord;
begin
  anAnnotation := Annotation[i, 0];
  result := anAnnotation.duration;
end;

procedure TEDFplusDoc.SetAnnotations;
var
  i, k: longint;
  j, l: integer;
  imax, kmax, m: longint;
  jmax: integer;
  onsetString, durationString, annotString, compString: String;
  theFormat: TFormatSettings;
  rawData: TRawDataRecord;

  procedure SetCh(const ch : smallint);
  begin
    if k > length(rawData[i, l - 1]) - 1 then
      SetLength(rawData[i, l - 1], k + 1);
    rawData[i, l - 1, k] := ch;
    inc(k);
  end;

  procedure SetString(const theString: String);
  var
    p: longint;
  begin
    for p := 1 to length(theString) do
    begin
      SetCh(ord(theString[p]));
    end;
  end;

begin
  theFormat := DefaultFormatSettings;
  theFormat.DecimalSeparator := '.';
  imax := iNumOfDataRecs;
  jmax := iNumOfSignals;
  kmax := iNumOfSamples[0]; // maximum number of samples over all signals
  for j := 0 to jmax - 1 do
  begin
    m := iNumOfSamples[j];
    if m > kmax then
      kmax      := m;
  end;
  SetLength(rawData, imax, jmax, kmax);
  l := -1;
  k := 0;
  for j := 0 to jmax - 1 do // find annotation signal
    begin
      if SignalLabel[j] = kAnnotationsHead then
      l := j;
    end;
  if l < 0 then
  begin
    // create new signal for annotations
    iNumOfSignals := jmax + 1;
    l := jmax + 1;
  end;
  rawData := RawDataRecord;
  for i := 0 to imax - 1 do
  begin
    jmax := length(FAnnotations[i]);
    for j := 0 to jmax - 1 do
    begin
      if IsNan(FAnnotations[i,j].onset) then
        onsetString := ''
      else
        onsetString := FloatToStr(FAnnotations[i,j].onset, theFormat) + TALOnset;
      if IsNan(FAnnotations[i,j].duration) then
        durationString := ''
      else
        begin
          durationString := FloatToStr(FAnnotations[i,j].duration, theFormat) + TALDuration;
          if k = 0 then
            durationString := durationString + TALDuration; // marker for record gap
        end;
      annotString := '';
      for m := 0 to length(FAnnotations[i,j].comment) - 1 do
        annotString := annotString + FAnnotations[i,j].comment[m] + TALDuration;
      compString := onsetString + durationString + annotString + TALTerminator;
      SetString(compString);
    end;
  end;
  RawDataRecord := rawData;
end;

constructor TEDFplusDoc.Create;
begin
  inherited Create;
  SetLength(FAnnotations, 0);
end;

destructor TEDFplusDoc.Destroy;
begin
  inherited Destroy;
end;

procedure TEDFplusDoc.AddAnnotation(const aRecord: longint; const theAnnotation: TTALRecord);
begin
  if length(FAnnotations) < aRecord + 1 then
    SetLength(FAnnotations, aRecord + 1);
  SetLength(FAnnotations[aRecord], length(FAnnotations[aRecord]) + 1);
  FAnnotations[aRecord, length(FAnnotations[aRecord]) - 1] := theAnnotation;
  SetAnnotations;
end;

end.

