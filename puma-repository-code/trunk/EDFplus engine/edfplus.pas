unit EDFplus;

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
  Classes, SysUtils, StrUtils, Math, URIParser, DateUtils, EDF;

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

  TRecordingType = (EDF_C, EDF_D, EDF_U);

const

  kStartDate   = 'Startdate';
  kContinuous  = 'EDF+C';
  kInterrupted = 'EDF+D';

type

  { TEDFplusDoc }

  TEDFplusDoc = class(TEDFDoc)
    protected
      function GetLocalPatID: TLocalPatRecord;
      function dGetLocalPatID: TLocalPatRecord;
      procedure SetLocalPatID(const ID: TLocalPatRecord);
      function GetLocalRecID: TLocalRecRecord;
      function dGetLocalRecID: TLocalRecRecord;
      procedure SetLocalRecID(const ID: TLocalRecRecord);
      function GetRecordingType: TRecordingType;
      procedure SetRecordingType(const theType: TRecordingType);
    public
      constructor Create;
      destructor Destroy; override;
      property LocalPatID: TLocalPatRecord read GetLocalPatID write SetLocalPatID;
      property dLocalPatID: TLocalPatRecord read dGetLocalPatID write SetLocalPatID;
      property LocalRecID: TLocalRecRecord read GetLocalRecID write SetLocalRecID;
      property dLocalRecID: TLocalRecRecord read dGetLocalRecID write SetLocalRecID;
      property RecordingType: TRecordingType read GetRecordingType write SetRecordingType;
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
  else if UpperCase(LeftStr(Reserved, 5)) = kInterrupted then
    result := EDF_D
  else
    result := EDF_U;
end;

procedure TEDFplusDoc.SetRecordingType(const theType: TRecordingType);
begin
  case theType of
  EDF_C: Reserved := PadRight(kContinuous, 44);
  EDF_D: Reserved := PadRight(kInterrupted, 44);
  otherwise Reserved := kEmpty44;
  end;
end;

constructor TEDFplusDoc.Create;
begin
  inherited Create;
end;

destructor TEDFplusDoc.Destroy;
begin
  inherited Destroy;
end;

end.

