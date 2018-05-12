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

  TLocalPatRecord = record
    HospitalCode: Str80;
    Sex: char;
    Name: Str80;
    case UseDateTime: boolean of
      true: (dBirthDate: TDateTime);
      false: (sBirthDate: Str11);
  end;

  { TEDFplusDoc }

  TEDFplusDoc = class(TEDFDoc)
    protected
      function GetLocalPatID: TLocalPatRecord;
      function dGetLocalPatID: TLocalPatRecord;
      procedure SetLocalPatID(const ID: TLocalPatRecord);
    public
      constructor Create;
      destructor Destroy; override;
      property LocalPatID: TLocalPatRecord read GetLocalPatID write SetLocalPatID;
      property dLocalPatID: TLocalPatRecord read dGetLocalPatID write SetLocalPatID;
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
begin
  containerString := inherited GetLocalPatID;
  Result.HospitalCode := AnsiReplaceText(ExtractDelimited(1, containerString, [' ']), '_', ' ');
  Result.Sex := char(ExtractDelimited(2, containerString, [' '])[1]);
  theDateString := ExtractDelimited(3, containerString, [' ']);
  if not TryStrToInt(LeftStr(theDateString, 2), theDay) then
    self.status := strFormatErr;
  MonthStr := MidStr(theDateString, 4, 3);
  theMonth := 0;
  repeat
    theMonth := theMonth + 1;
  until (MonthStr = kShortEnglishMonths[theMonth]) or (theMonth > 11);
  if not TryStrToInt(RightStr(theDateString, 4), theYear) then
    self.status := strFormatErr;
  // FPC functions don't support three-characters shorts months on macOS
  Result.dBirthDate := EncodeDate(theYear, theMonth, theDay);
  Result.Name := AnsiReplaceText(ExtractDelimited(4, containerString, [' ']), '_', ' ');
end;

procedure TEDFplusDoc.SetLocalPatID(const ID: TLocalPatRecord);
var
  containerString: Str80;
begin
  if ID.UseDateTime or (ID.sBirthDate = '') then
    containerString := AnsiReplaceText(ID.HospitalCode, ' ', '_') + ' ' +
    ID.Sex + ' ' + FormatDateTime('dd-mmm-yyyy', ID.dBirthDate) + ' ' +
    AnsiReplaceText(ID.Name, ' ', '_')
  else
    containerString := AnsiReplaceText(ID.HospitalCode, ' ', '_') + ' ' +
    ID.Sex + ' ' + ID.sBirthDate + ' ' + AnsiReplaceText(ID.Name, ' ', '_');
  inherited SetLocalPatID(containerString);
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

