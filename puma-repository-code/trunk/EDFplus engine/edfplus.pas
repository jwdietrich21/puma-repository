unit EDFplus;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ EDF+ base unit }

{ Version 1.0 () }

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

const

  ksCR   = #13;
  ksLF   = #10;
  ksCRLF = #13#10;

  noErr          = 0;
  termErr        = 2;
  unsuppVers     = 4;
  saveErr        = 6;
  readErr        = 7;
  createErr      = 9;
  stringRangeErr = 11; // Please check for consistency

  kEDFVersion     = '0       ';
  kStartDate      = 'Startdate';
  kEDFAnnotations = 'EDF Annotations';

type

  str4 = string[4];
  str8  = string[8];
  str16 = string[16];
  str32 = string[32];
  str44 = string[44];
  str80 = string[80];

{ TEDFplusDoc }

TEDFplusDoc = class
  private
    prVersion: str8;
    prLocalPatID: str80;
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

procedure TEDFplusDoc.CompileHeaderText;
begin
  HeaderText := prVersion + prLocalPatID;
end;

function TEDFplusDoc.ExtractHeaderText(const start, count: integer): AnsiString;
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

function TEDFplusDoc.GetVersion: Str8;
begin
  Result := prVersion;
end;

function TEDFplusDoc.GetLocalPatID: Str80;
begin
  ExtractHeaderText(8, 80);
end;

procedure TEDFplusDoc.SetLocalPatID(const ID: Str80);
begin
  prLocalPatID := ID;
  CompileHeaderText;
end;

constructor TEDFplusDoc.Create;
begin
  inherited Create;
  status := 0;
  prVersion := kEDFVersion;
  prLocalPatID := '';
  CompileHeaderText;
end;

destructor TEDFplusDoc.Destroy;
begin
  inherited Destroy;
end;

end.

