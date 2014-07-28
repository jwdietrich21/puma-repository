unit HL7;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 base unit }

{ Version 1.6 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ Parser and compiler for HL7 messages }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://puma-repository.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{
Status code of HL7 message:
 0: No Error.
 4: This HL7 version is not supported.
 6: Error saving file.
 7: Error reading file.
 8: Segment not found.
}

{$mode objfpc}

interface

uses
  Classes, SysUtils, StrUtils, Math, URIParser, DateUtils;

const

  ksCR = #13;
  ksLF = #10;
  ksCRLF = #13#10;

  noErr = 0;
  unsuppVers = 4;
  saveErr = 6;
  readErr = 7;
  segNotFound = 8;

  STANDARD_DELIMITERS = '|^~\&';
  SEGMENT_DELIMITER = ksCR;

  ACKNOWLEDGEMENT_OK = 'AA';
  ACKNOWLEDGEMENT_ERROR = 'AE';
  ACKNOWLEDGEMENT_REJECT = 'AR';
  COMMIT_ACCEPT = 'CA';
  COMMIT_ERROR = 'CE';
  COMMIT_REJECT = 'CR';

  ESCAPE_HIGHLIGHTING = '\H\';   {start highlighting}
  ESCAPE_NORMAL = '\N\';     {end highlighting}
  ESCAPE_FIELD = '\F\';
  ESCAPE_COMPONENT = '\S\';
  ESCAPE_SUBCOMPONENT = '\T\';
  ESCAPE_REPETITION = '\R\';
  ESCAPE_ESCAPE = '\E\';

  ESCAPE_ISO_IR6_G0 = '\C2842\';   {ISO 646 : ASCII}
  ESCAPE_ISO_IR100 = '\C2D41\';    {ISO 8859 : Latin Alphabet 1}
  ESCAPE_ISO_IR101 = '\C2D42\';    {ISO 8859 : Latin Alphabet 2}
  ESCAPE_ISO_IR109 = '\C2D43\';    {ISO 8859 : Latin Alphabet 3}
  ESCAPE_ISO_IR110 = '\C2D44\';    {ISO 8859 : Latin Alphabet 4}
  ESCAPE_ISO_IR144 = '\C2D4C\';    {ISO 8859 : Cyrillic}
  ESCAPE_ISO_IR127 = '\C2D47\';    {ISO 8859 : Arabic}
  ESCAPE_ISO_IR126 = '\C2D46\';    {ISO 8859 : Greek}
  ESCAPE_ISO_IR138 = '\C2D48\';    {ISO 8859 : Hebrew}
  ESCAPE_ISO_IR148 = '\C2D4D\';    {ISO 8859 : Latin Alphabet 5}
  ESCAPE_ISO_IR14  = '\C284A\';    {JIS X 0201 -1976: Romaji}
  ESCAPE_ISO_IR13  = '\C2949\';    {JIS X 0201 : Katakana}
  ESCAPE_ISO_IR87  = '\M2442\';    {JIS X 0208 : Kanji, hiragana and katakana}
  ESCAPE_ISO_IR159 = '\M242844\';  {JIS X 0212 : Supplementary Kanji}

  ERROR_COND_MSG_ACC = '0';           {Message accepted}
  ERROR_COND_SEG_SEQ_ERR = '100';     {Segment sequence error}
  ERROR_COND_REQ_FLD_MISS = '101';    {Required field missing}
  ERROR_COND_DATA_TYPE_ERR = '102';   {Data type error}
  ERROR_COND_TBL_VAL_NOT_FND = '103'; {Table value not found}
  ERROR_COND_UNSUPP_MSG_TYPE = '200'; {Unsupported message type}
  ERROR_COND_UNSUPP_EVT_CODE = '201'; {Unsupported event code}
  ERROR_COND_UNSUPP_PROC_ID = '202';  {Unsupported processing id}
  ERROR_COND_UNSUPP_VERS_ID = '200';  {Unsupported version id}
  ERROR_COND_UNKNOWN_KEY_ID = '204';  {Unknown key identifier}
  ERROR_COND_DUPL_KEY_ID = '205';     {Duplicate key identifier}
  ERROR_COND_APPL_REC_LOCK = '206';   {Application record locked}
  ERROR_COND_INT_ERR = '207';         {Internal error}

  MSH_ID = 'MSH';

type

  str2 = string[2];
  str3 = string[3];
  str4 = string[4];
  str5 = string[5];
  str8 = string[8];
  str15 = string[15];
  str16 = string[16];
  str20 = string[20];
  str22 = string[22];
  str25 = string[25];
  str26 = string[26];
  str40 = string[40];
  str50 = string[50];
  str53 = string[53];
  str60 = string[60];
  str80 = string[80];
  str180 = string[180];
  str227 = string[227];
  str241 = string[241];
  str250 = string[250];
  str427 = AnsiString;

  tCE = str250;      { HL7 CE type (Coded entry, deprecated as of HL7 v2.6) }
  tCNE = AnsiString; { HL7 2.6 CNE type (Coded with no exceptions) }
  tCWE = AnsiString; { HL7 2.6 CWE type (coded with exceptions) }
  tCX = str250;      { HL7 CX type (Extended composite ID with check digit) }
  tCQ = AnsiString;  { HL7 CQ type (Composite quantity with units) }
  tDLD = AnsiString; { HL7 DLD type (Discharge to location and date) }
  tDLN = str25;      { HL7 DLN type (Driver's license number) }
  tDR = str53;       { HL7 DR type (date/time range) }
  tDTM = str26;      { HL7 2.7 DTM type (Date/time) }
  tDT = str8;        { HL7 DT type (Date) }
  tEI = str427;      { HL7 EI type (Entity identifier) }
  tEIP = AnsiString; { HL7 EIP type (Entity identifier pair) }
  tELD = AnsiString; { HL7 ELD type (Error location and description, deprecated as of HL7 v2.5) }
  tERL = str180;     { HL7 ERL type (Error location) }
  tFC = str50;       { HL7 FC type (Financial class) }
  tFT = AnsiString;  { HL7 FT type (Formatted text data) }
  tHD = AnsiString;  { HL7 HD type (Hierarchic designator) }
  tID = AnsiString;  { HL7 ID type (Coded value for HL7 defined tables) }
  tIS = str20;       { HL7 2.5 IS type (Coded value for user-defined tables) }
  tJCC = AnsiString; { HL7 JCC type (Job code/class) }
  tMSG = str15;      { HL7 MSG type (Message type) }
  tMOC = AnsiString; { HL7 MOC type (Money and charge code) }
  tNDL = AnsiString; { HL7 NDL type (Name with date and location) }
  tNM = str16;       { HL7 NM type (ASCII-represented number) }
  tPL = str80;       { HL7 PL type (Person location) }
  tPRL = AnsiString; { HL7 PRL type (Parent result link) }
  tPT = str3;        { HL7 PT type (Processing type) }
  tSI = str4;        { HL7 SI type (Sequence ID) }
  tSPS = AnsiString; { HL7 SPS type (Specimen source, deprecated as of HL7 v2.5) }
  tST = AnsiString;  { HL7 ST type (Dtring data ) }
  tTQ = str250;      { HL7 TQ type (timing/quantity, deprecated as of HL7 v2.6) }
  tTS = str26;       { HL7 2.5 TS type (Time stamp, deprecated as of HL7 v2.6) }
  tVID = str60;      { HL7 VID type (Version identifier) }
  tXAD = str250;     { HL7 XAD type (Extended address) }
  tXCN = str250;     { HL7 XCN type (Extended composite ID number and name for persons) }
  tXON = AnsiString; { HL7 XON type (Extended composite name and identification number for organizations) }
  tXPN = str250;     { HL7 XPN type (Extended person name) }
  tXTN = str250;     { HL7 XAD type (Extended telecommunications number) }

  THL7Delimiters = record
    SegmentTerminator, FieldSeparator, ComponentSeparator: char;
    SubcomponentSeparator, RepetitionSeparator, EscapeCharacter: char;
  end;

  THL7Message = class;
  THL7Segment = class;
  THL7Occurrence = class;
  THL7Field = class;
  THL7Component = class;
  THL7SubComponent = class;

  { THL7MessageSection }

  THL7MessageSection = class
  private
    FText: ansistring;
  protected
    FOwner: THL7MessageSection;
    FMessage: THL7Message;
    destructor Destroy; override;
    procedure ParseMessageString(const aString: ansistring); virtual; abstract;
    function CompiledMessageString: ansistring; virtual; abstract;
  public
    property contentString: ansistring read CompiledMessageString
      write ParseMessageString;
  end;

  { THL7Segment }

  THL7Segment = class(THL7MessageSection)
  protected
    FPreviousSibling, FNextSibling: THL7Segment;
    SegmentName: str3;
    FlOwner: THL7Message;
    procedure ParseMessageString(const aString: ansistring);
    function CompiledMessageString: ansistring;
  public
    FirstOccurrence: THL7Occurrence;
    constructor Create(owner: THL7Message; SegmentText: ansistring);
    destructor Destroy; override;
    function NewOccurrence(const OccurrencesText: ansistring): THL7Occurrence;
    procedure AllocOccurrences(const OccurrencesText: ansistring);
    property contentString: ansistring read CompiledMessageString
      write ParseMessageString;
    property previousSibling: THL7Segment read FPreviousSibling;
    property nextSibling: THL7Segment read FNextSibling;
    property segmentType: str3 read SegmentName write SegmentName;
  end;

  { THL7Occurrence }

  THL7Occurrence = class(THL7MessageSection)
  protected
    FPreviousSibling, FNextSibling: THL7Occurrence;
    procedure ParseMessageString(const aString: ansistring);
    function CompiledMessageString: ansistring;
  public
    FirstField: THL7Field;
    constructor Create(owner: THL7Segment; OccurrencesText: ansistring);
    destructor Destroy; override;
    function NewField: THL7Field;
    function GetNextFieldContent(var aField: THL7Field): String;
    procedure AllocFields(const FieldText: ansistring);
    property contentString: ansistring read CompiledMessageString
      write ParseMessageString;
    property previousSibling: THL7Occurrence read FPreviousSibling;
    property nextSibling: THL7Occurrence read FNextSibling;
  end;

  { THL7Field }

  THL7Field = class(THL7MessageSection)
  protected
    FPreviousSibling, FNextSibling: THL7Field;
    procedure ParseMessageString(const aString: ansistring);
    function CompiledMessageString: ansistring;
  public
    FirstComponent: THL7Component;
    constructor Create(owner: THL7Occurrence; FieldText: ansistring);
    destructor Destroy; override;
    function NewComponent: THL7Component;
    procedure AllocComponents(const ComponentText: ansistring);
    property contentString: ansistring read CompiledMessageString
      write ParseMessageString;
    property previousSibling: THL7Field read FPreviousSibling;
    property nextSibling: THL7Field read FNextSibling;
  end;

  { THL7Component }

  THL7Component = class(THL7MessageSection)
  protected
    FPreviousSibling, FNextSibling: THL7Component;
    procedure ParseMessageString(const aString: ansistring);
    function CompiledMessageString: ansistring;
  public
    FirstSubComponent: THL7SubComponent;
    constructor Create(owner: THL7Field; ComponentText: ansistring);
    destructor Destroy; override;
    function NewSubComponent: THL7SubComponent;
    procedure AllocSubComponents(const SubComponentText: ansistring);
    property contentString: ansistring read CompiledMessageString
      write ParseMessageString;
    property previousSibling: THL7Component read FPreviousSibling;
    property nextSibling: THL7Component read FNextSibling;
  end;

  { THL7SubComponent }

  THL7SubComponent = class(THL7MessageSection)
  protected
    FPreviousSibling, FNextSibling: THL7SubComponent;
    procedure ParseMessageString(const aString: ansistring);
    function CompiledMessageString: ansistring;
  public
    constructor Create(owner: THL7Component; SubComponentText: ansistring);
    destructor Destroy; override;
    property contentString: ansistring read CompiledMessageString
      write ParseMessageString;
    property previousSibling: THL7SubComponent read FPreviousSibling;
    property nextSibling: THL7SubComponent read FNextSibling;
  end;

  { THL7Message }

  THL7Message = class
  private
    HL7_version: string;
    HL7Delimiters: THL7Delimiters;
    Status: integer;
    ErrorCond: string;
  protected
    HL7Text: ansistring;
    procedure SetHL7Version(const aValue: string);
    procedure ParseMessageString(const aString: ansistring);
    function CompiledMessageString: ansistring;
  public
    ControlID: Str20;
    FirstSegment: THL7Segment;
    procedure ParseDelimiters(DelimiterDefinition: ansistring);
    function CompiledDelimiters(const delimiters: THL7Delimiters): str5;
    function Encoded(const aString: ansistring): ansistring;
    function EncodedHex(const aNumber: integer): string;
    function Decoded(const aString: ansistring): ansistring;
    function DecodedHex(const aString: string): integer;
    constructor Create(version: string);
    destructor Destroy; override;
    property HL7Version: string read HL7_version write SetHL7Version;
    property Delimiters: THL7Delimiters read HL7Delimiters write HL7Delimiters;
    function FoundSegment(const aSegmentName, SetID: Str3): THL7Segment;
    function FoundSegment(const aSegmentName, SetID: Str3;
      beginWith: THL7Segment): THL7Segment;
    function FoundSegment(const aSegmentName, SetID: Str3;
      beginWith: THL7Segment; out lastSegment: THL7Segment): THL7Segment;
    function NewSegment: THL7Segment;
    procedure AddSegment(theSegment: THL7Segment);
    procedure DeleteSegment(const aSegmentName, SetID: Str3);
    procedure ReplaceSegment(const aSegmentName, SetID: Str3; by: THL7Segment);
    procedure ReplaceSegment(const aSegmentName, SetID: Str3; by: THL7Segment; insertAlways: boolean);
    procedure AllocSegments(const SegmentText: ansistring);
    property contentString: ansistring read CompiledMessageString
      write ParseMessageString;
    property StatusCode: integer read status;
    property ErrorCondition: string read ErrorCond write ErrorCond;
  end;

procedure ReadHL7File(out HL7Doc: THL7Message; const aFileName: ansistring); overload;
procedure ReadHL7File(out HL7Doc: THL7Message; var aFile: Text); overload;
procedure ReadHL7File(out HL7Doc: THL7Message; aStream: TStream); overload;
procedure ReadHL7File(out HL7Doc: THL7Message; aStream: TStream; const aBaseURI: ansistring);
  overload;
procedure WriteHL7File(HL7Doc: THL7Message; const aFileName: ansistring); overload;
procedure WriteHL7File(HL7Doc: THL7Message; var aFile: Text); overload;
procedure WriteHL7File(HL7Doc: THL7Message; aStream: TStream); overload;
function EncodedDateTime(DateTime: TDateTime): string;
function DecodeDateTime(StringRepresentation: string): TDateTime;


implementation

procedure ReadHL7File(out HL7Doc: THL7Message; const aFileName: ansistring);
{reads and parses an HL7 message from file}
var
  theStream: TStream;
begin
  if FileExists(aFileName) then
    begin
      theStream := TFileStream.Create(AFilename, fmOpenRead + fmShareDenyWrite);
      try
        ReadHL7File(HL7Doc, theStream, FilenameToURI(AFilename));
      finally
        if theStream <> nil then
          theStream.Free
        else
          begin
            HL7Doc.Status := readErr;
            HL7Doc.ErrorCondition := ERROR_COND_INT_ERR;
          end;
      end;
    end
  else
    begin
      HL7Doc := THL7Message.Create('2.0');
      HL7Doc.Status := readErr;          { create empty message with status code 7 }
      HL7Doc.ErrorCondition := ERROR_COND_INT_ERR;
    end;
end;

procedure ReadHL7File(out HL7Doc: THL7Message; var aFile: Text);
{reads and parses an HL7 message from text file}
var
  theString, nextFragment: ansiString;
begin
  HL7Doc := nil;
  theString := '';
  try
    reset(aFile);
    repeat
      readln(aFile, nextFragment);
      theString := theString + nextFragment + #13;
    until EOF(aFile);
  finally
    CloseFile(aFile);
  end;
  if theString <> '' then
    begin
      theString := StringReplace(theString, ksCRLF, ksCR, [rfReplaceAll, rfIgnoreCase]);
      HL7Doc := THL7Message.Create('2.5');
      HL7Doc.contentString := theString;
    end
  else
    begin
      HL7Doc := THL7Message.Create('2.0');
      HL7Doc.Status := readErr;          { create empty message with status code 7 }
      HL7Doc.ErrorCondition := ERROR_COND_INT_ERR;
    end;
end;

procedure ReadHL7File(out HL7Doc: THL7Message; aStream: TStream);
{reads and parses an HL7 message from stream}
begin
  ReadHL7File(HL7Doc, aStream, 'stream:');
end;

procedure ReadHL7File(out HL7Doc: THL7Message; aStream: TStream; const aBaseURI: ansistring);
{reads and parses an HL7 message from URI}
var
  theString: AnsiString;
begin
  HL7Doc := nil;
  if aStream.Size > 0 then
  begin
    SetLength(theString, aStream.Size);
    aStream.Read(theString[1], aStream.Size);
    if theString <> '' then
    begin
      theString := StringReplace(theString, ksCRLF, ksCR, [rfReplaceAll, rfIgnoreCase]); // correct line endings
      HL7Doc := THL7Message.Create('2.5');
      HL7Doc.contentString := theString;
      HL7Doc.Status := noErr;
    end
    else
      begin
        HL7Doc := THL7Message.Create('2.0');
        HL7Doc.Status := readErr;          { create empty message with status code 7 }
        HL7Doc.ErrorCondition := ERROR_COND_INT_ERR;
      end;
  end
  else
    begin
      HL7Doc := THL7Message.Create('2.0');
      HL7Doc.Status := readErr;          { create empty message with status code 7 }
      HL7Doc.ErrorCondition := ERROR_COND_INT_ERR;
    end;
end;

procedure WriteHL7File(HL7Doc: THL7Message; const aFileName: ansistring);
{compiles and writes an HL7 message to file}
var
  theStream: TStream;
begin
  theStream := TFileStream.Create(aFilename, fmOpenWrite or fmCreate);
  try
    WriteHL7File(HL7Doc, theStream);
  finally
    if theStream <> nil then
      theStream.Free
    else
      begin
        HL7Doc.Status := saveErr;
        HL7Doc.ErrorCondition := ERROR_COND_INT_ERR;
      end;
  end;
end;

procedure WriteHL7File(HL7Doc: THL7Message; var aFile: Text);
{compiles and writes an HL7 message to text file}
var
  theString: AnsiString;
  textLength: integer;
begin
  theString := HL7Doc.contentString;
  textLength := length(theString);
  try
    rewrite(aFile);
    write(aFile, theString);
  finally
    CloseFile(aFile);
  end;
  if IOResult <> 0 then
    begin
      HL7Doc.Status := saveErr;
      HL7Doc.ErrorCondition := ERROR_COND_INT_ERR;
    end;
end;

procedure WriteHL7File(HL7Doc: THL7Message; aStream: TStream);
{compiles and writes an HL7 message to stream}
var
  theString: AnsiString;
  textLength: integer;
begin
  theString := HL7Doc.contentString;
  textLength := length(theString);
  try
    aStream.WriteBuffer(theString[1], textLength);
  except
    begin
      HL7Doc.Status := saveErr;
      HL7Doc.ErrorCondition := ERROR_COND_INT_ERR;
    end;
  end;
end;

function EncodedDateTime(DateTime: TDateTime): string;
begin
  Result := FormatDateTime('YYYYMMDDhhnnss', DateTime);
end;

function DecodeDateTime(StringRepresentation: string): TDateTime;
var
  theYear, theMonth, theDay, theHour, theMinute, theSecond: longint;
  oldDateFormat: string;
  format: TFormatSettings;
begin
  oldDateFormat := ShortDateFormat;
  ShortDateFormat := 'YYYYMMDDhhnnss';
  format.DateSeparator := char(0);
  format.TimeSeparator := char(0);
  format.TimeAMString := char(0);
  format.TimePMString := char(0);
  format.ShortDateFormat := 'YYYYMMDDhhnnss';
  if not TryStrToDateTime(StringRepresentation, result, format) then
  begin
    if length(StringRepresentation) = 14 then
    begin
      try
        theYear := StrToIntDef(copy(StringRepresentation, 1, 4), -1);
        theMonth := StrToIntDef(copy(StringRepresentation, 5, 2), -1);
        theDay := StrToIntDef(copy(StringRepresentation, 7, 2), -1);
        theHour := StrToIntDef(copy(StringRepresentation, 9, 2), -1);
        theMinute := StrToIntDef(copy(StringRepresentation, 11, 2), -1);
        theSecond := StrToIntDef(copy(StringRepresentation, 13, 2), -1);
        result := EncodeDateTime(theYear, theMonth, theDay, theHour, theMinute, theSecond, 0);
      except
        on E: EConvertError do
          result := NaN;
      end;
    end
    else if length(StringRepresentation) = 8 then
    begin
      try
        theYear := StrToIntDef(copy(StringRepresentation, 1, 4), -1);
        theMonth := StrToIntDef(copy(StringRepresentation, 5, 2), -1);
        theDay := StrToIntDef(copy(StringRepresentation, 7, 2), -1);
        result := EncodeDateTime(theYear, theMonth, theDay, 0, 0, 0, 0);
      except
        on E: EConvertError do
          result := NaN;
      end;
    end
    else
      result := NaN;
  end;
  ShortDateFormat := oldDateFormat;
end;

function NextSection(const aString: ansistring; var Pos: integer;
  const delim: char): ansistring;
  {extracts a substring from a AnsiString, beginning with Pos and ending with delim}
var
  i, j, l: integer;
  theString: ansistring;
begin
  theString := aString;
  if pos >= 1 then
    Delete(theString, 1, pos);
  i := system.pos(delim, theString);
  if i = 0 then
  begin
    Result := theString;
    pos := length(aString);
  end
  else
    Result := copy(theString, 1, i - 1);
  Inc(pos, i);
end;

{ THL7MessageSection }

destructor THL7MessageSection.Destroy;
begin
  inherited Destroy;
end;

{ THL7SubComponent }

procedure THL7SubComponent.ParseMessageString(const aString: ansistring);
begin
  FText := aString;
end;

function THL7SubComponent.CompiledMessageString: ansistring;
begin
  if FMessage <> nil then
    Result := FMessage.Encoded(FText)
  else
    Result := FText;
end;

constructor THL7SubComponent.Create(owner: THL7Component; SubComponentText: ansistring);
begin
  inherited Create;
  FOwner := owner;
  if owner <> nil then
    FMessage := owner.FMessage;
  FNextSibling := nil;
  contentString := SubComponentText;
end;

destructor THL7SubComponent.Destroy;
var
  remainingSiblings: THL7SubComponent;
begin
  remainingSiblings := FNextSibling;
  if remainingSiblings <> nil then
    remainingSiblings.Destroy;
  inherited Destroy;
end;

{ THL7Component }

procedure THL7Component.ParseMessageString(const aString: ansistring);
begin
  FText := aString;
  if (aString <> '') and (FMessage <> nil) and
    (pos(FMessage.Delimiters.ComponentSeparator, aString) = 0) and
    (pos(FMessage.Delimiters.SubComponentSeparator, aString) <> 0) then
    {true components with subcomponents only}
    AllocSubcomponents(aString)
  else
    FirstSubComponent := nil;
end;

function THL7Component.CompiledMessageString: ansistring;
var
  currSubComponent: THL7SubComponent;
begin
  currSubComponent := FirstSubComponent;
  if currSubComponent = nil then
    Result := FText
  else
  begin
    FText := '';
    while currSubComponent <> nil do
    begin
      FText := FText + currSubComponent.contentString;
      currSubComponent := currSubComponent.nextSibling;
      if currSubComponent <> nil then
        FText := FText + FMessage.Delimiters.SubcomponentSeparator;
    end;
    Result := FText;
  end;
end;

constructor THL7Component.Create(owner: THL7Field; ComponentText: ansistring);
begin
  inherited Create;
  FOwner := owner;
  if owner <> nil then
    FMessage := owner.FMessage;
  FNextSibling := nil;
  contentString := ComponentText;
end;

destructor THL7Component.Destroy;
var
  remainingSiblings: THL7Component;
begin
  remainingSiblings := FNextSibling;
  if remainingSiblings <> nil then
    remainingSiblings.Destroy;
  if FirstSubComponent <> nil then
    FirstSubComponent.Destroy;
  inherited Destroy;
end;

function THL7Component.NewSubComponent: THL7SubComponent;
var
  theSubcomponent, currSubcomponent: THL7Subcomponent;
begin
  theSubcomponent := THL7Subcomponent.Create(self, '');
  currSubcomponent := FirstSubcomponent;
  if currSubcomponent = nil then
    FirstSubcomponent := theSubcomponent
  else
  begin
    while currSubcomponent.FNextSibling <> nil do
      currSubcomponent := currSubcomponent.FNextSibling;
    currSubcomponent.FNextSibling := theSubcomponent;
  end;
  Result := theSubcomponent;
end;

procedure THL7Component.AllocSubComponents(const SubComponentText: ansistring);
var
  theSubcomponent: THL7Subcomponent;
  lastPos: integer;
  singleSubComponentText: ansistring;
begin
  theSubcomponent := NewSubComponent;
  if (FMessage = nil) or (pos(FMessage.Delimiters.SubComponentSeparator,
    SubComponentText) = 0) then
    theSubcomponent.ParseMessageString(SubComponentText)
  else
  begin
    lastPos := 0;
    while lastPos < length(SubComponentText) do
    begin
      singleSubComponentText :=
        NextSection(SubComponentText, lastPos,
        FMessage.Delimiters.SubComponentSeparator);
      theSubComponent.ParseMessageString(singleSubComponentText);
      if (lastPos < length(SubComponentText)) or (singleSubComponentText = '') then
      begin
        theSubComponent.FNextSibling := THL7SubComponent.Create(self, '');
        theSubComponent := theSubComponent.FNextSibling;
      end;
    end;
  end;
end;

{ THL7Field }

procedure THL7Field.ParseMessageString(const aString: ansistring);
begin
  FText := aString;
  if (aString <> '') and (FMessage <> nil) and
    (pos(FMessage.Delimiters.FieldSeparator, aString) = 0) and
    ((pos(FMessage.Delimiters.ComponentSeparator, aString) > 0) or
    (pos(FMessage.Delimiters.SubComponentSeparator, aString) > 0)) then
    {true fields only}
    AllocComponents(aString);
end;

function THL7Field.CompiledMessageString: ansistring;
var
  currComponent: THL7Component;
begin
  currComponent := FirstComponent;
  if currComponent = nil then
    Result := FText
  else
  begin
    FText := '';
    while currComponent <> nil do
    begin
      FText := FText + currComponent.contentString;
      currComponent := currComponent.nextSibling;
      if currComponent <> nil then
        FText := FText + FMessage.Delimiters.ComponentSeparator;
    end;
    Result := FText;
  end;
end;

constructor THL7Field.Create(owner: THL7Occurrence; FieldText: ansistring);
begin
  inherited Create;
  FOwner := owner;
  if owner <> nil then
    FMessage := owner.FMessage;
  FNextSibling := nil;
  contentString := FieldText;
end;

destructor THL7Field.Destroy;
var
  remainingSiblings: THL7Field;
begin
  remainingSiblings := FNextSibling;
  if remainingSiblings <> nil then
    remainingSiblings.Destroy;
  if FirstComponent <> nil then
    FirstComponent.Destroy;
  inherited Destroy;
end;

function THL7Field.NewComponent: THL7Component;
var
  theComponent, currComponent: THL7Component;
begin
  theComponent := THL7Component.Create(self, '');
  currComponent := FirstComponent;
  if currComponent = nil then
    FirstComponent := theComponent
  else
  begin
    while currComponent.FNextSibling <> nil do
      currComponent := currComponent.FNextSibling;
    currComponent.FNextSibling := theComponent;
  end;
  Result := theComponent;
end;

procedure THL7Field.AllocComponents(const ComponentText: ansistring);
var
  theComponent: THL7Component;
  lastPos: integer;
  singleComponentText: ansistring;
begin
  theComponent := NewComponent;
  if (FMessage = nil) or (pos(FMessage.Delimiters.ComponentSeparator,
    ComponentText) = 0) then
    theComponent.ParseMessageString(ComponentText)
  else
  begin
    lastPos := 0;
    while lastPos < length(ComponentText) do
    begin
      singleComponentText :=
        NextSection(ComponentText, lastPos, FMessage.Delimiters.ComponentSeparator);
      theComponent.ParseMessageString(singleComponentText);
      if (lastPos < length(ComponentText)) or (singleComponentText = '') then
      begin
        theComponent.FNextSibling := THL7Component.Create(self, '');
        theComponent := theComponent.FNextSibling;
      end;
    end;
  end;
end;

{ THL7Occurrence }

procedure THL7Occurrence.ParseMessageString(const aString: ansistring);
begin
  FText := aString;
  AllocFields(aString);
end;

function THL7Occurrence.CompiledMessageString: ansistring;
begin
  Result := FText;
end;

constructor THL7Occurrence.Create(owner: THL7Segment; OccurrencesText: ansistring);
begin
  inherited Create;
  FOwner := owner;
  if owner <> nil then
    FMessage := owner.FMessage;
  FNextSibling := nil;
  contentString := OccurrencesText;
end;

destructor THL7Occurrence.Destroy;
var
  remainingSiblings: THL7Occurrence;
begin
  remainingSiblings := FNextSibling;
  if remainingSiblings <> nil then
    remainingSiblings.Destroy;
  if FirstField <> nil then
    FirstField.Destroy;
  inherited Destroy;
end;

function THL7Occurrence.NewField: THL7Field;
var
  theField, currField: THL7Field;
begin
  theField := THL7Field.Create(self, '');
  currField := FirstField;
  if currField = nil then
    FirstField := theField
  else
  begin
    while currField.FNextSibling <> nil do
      currField := currField.FNextSibling;
    currField.FNextSibling := theField;
  end;
  Result := theField;
end;

function THL7Occurrence.GetNextFieldContent(var aField: THL7Field): String;
begin
  if aField <> nil then
  begin
    result := aField.contentString;
    aField := aField.nextSibling;
  end
  else
    result := '';
end;

procedure THL7Occurrence.AllocFields(const FieldText: ansistring);
var
  theField: THL7Field;
  lastPos: integer;
  singleFieldText: ansistring;
begin
  theField := NewField;
  if (FMessage = nil) or (pos(FMessage.Delimiters.FieldSeparator, FieldText) = 0) then
    theField.ParseMessageString(FieldText)
  else
  begin
    lastPos := 0;
    while lastPos < length(FieldText) do
    begin
      singleFieldText := NextSection(FieldText, lastPos,
        FMessage.Delimiters.FieldSeparator);
      theField.ParseMessageString(singleFieldText);
      if (lastPos < length(FieldText)) or (singleFieldText = '') then
      begin
        theField.FNextSibling := THL7Field.Create(self, '');
        theField := theField.FNextSibling;
      end;
    end;
  end;
end;

{ THL7Segment }

procedure THL7Segment.ParseMessageString(const aString: ansistring);
begin
  FText := aString;
  SegmentName := LeftStr(FText, 3);
  if (aString <> '') and (FMessage <> nil) and
    (pos(FMessage.Delimiters.SegmentTerminator, aString) = 0) then
    {true segments only}
    NewOccurrence(aString);
end;

function THL7Segment.CompiledMessageString: ansistring;
var
  curField: THL7Field;
begin
  if FirstOccurrence = nil then
    Result := FText
  else
  begin
    curField := FirstOccurrence.FirstField;
    if curField = nil then
      Result := FText
    else
    begin
      FText := '';
      while curField <> nil do
      begin
        FText := FText + curField.contentString;
        if FText = MSH_ID then
        begin
          {bypass parsing of delimiter definition sequence:}
          curField := curField.nextSibling;
          FText := FText + FMessage.Delimiters.FieldSeparator + curField.FText;
        end;
        curField := curField.nextSibling;
        if curField <> nil then
          FText := FText + FMessage.Delimiters.FieldSeparator;
      end;
      {ensures that each field, also the last one, is properly terminated:}
      if RightStr(FText, 1) <> FMessage.Delimiters.FieldSeparator then
        FText := FText + FMessage.Delimiters.FieldSeparator;
      Result := FText;
    end;
  end;
end;

constructor THL7Segment.Create(owner: THL7Message; SegmentText: ansistring);
begin
  inherited Create;
  FlOwner := owner;
  FMessage := FlOwner;
  FNextSibling := nil;
  contentString := SegmentText;
end;

destructor THL7Segment.Destroy;
var
  remainingSiblings: THL7Segment;
begin
  remainingSiblings := FNextSibling;
  if remainingSiblings <> nil then
    remainingSiblings.Destroy;
  if FirstOccurrence <> nil then
    FirstOccurrence.Destroy;
  inherited Destroy;
end;

function THL7Segment.NewOccurrence(const OccurrencesText: ansistring): THL7Occurrence;
var
  theOccurrence, currOccurrence: THL7Occurrence;
begin
  theOccurrence := THL7Occurrence.Create(self, OccurrencesText);
  currOccurrence := FirstOccurrence;
  if currOccurrence = nil then
    FirstOccurrence := theOccurrence
  else
  begin
    while currOccurrence.FNextSibling <> nil do
      currOccurrence := currOccurrence.FNextSibling;
    currOccurrence.FNextSibling := theOccurrence;
  end;
  Result := theOccurrence;
end;

procedure THL7Segment.AllocOccurrences(const OccurrencesText: ansistring);
begin

end;

{ THL7Message }

procedure THL7Message.SetHL7Version(const aValue: string);
begin
  HL7_version := aValue;
  if LeftStr(HL7_version, 1) <> '2' then
  begin
    Status := unsuppVers;
    ErrorCondition := ERROR_COND_UNSUPP_VERS_ID;
  end;
end;

procedure THL7Message.ParseMessageString(const aString: ansistring);
begin
  HL7Text := aString;
  if (aString <> '') then
    {true fields only}
    AllocSegments(aString);
end;

function THL7Message.CompiledMessageString: ansistring;
var
  currSegment: THL7Segment;
begin
  currSegment := FirstSegment;
  if currSegment = nil then
    Result := HL7Text
  else
  begin
    HL7Text := '';
    while currSegment <> nil do
    begin
      HL7Text := HL7Text + currSegment.contentString;
      currSegment := currSegment.nextSibling;
      if currSegment <> nil then
        HL7Text := HL7Text + Delimiters.SegmentTerminator;
    end;
    Result := HL7Text;
  end;
end;


procedure THL7Message.ParseDelimiters(DelimiterDefinition: ansistring);
begin
  HL7Delimiters.SegmentTerminator := SEGMENT_DELIMITER;
  if DelimiterDefinition = '' then
    DelimiterDefinition := STANDARD_DELIMITERS;
  HL7Delimiters.FieldSeparator := DelimiterDefinition[1];
  HL7Delimiters.ComponentSeparator := DelimiterDefinition[2];
  HL7Delimiters.SubcomponentSeparator := DelimiterDefinition[5];
  HL7Delimiters.RepetitionSeparator := DelimiterDefinition[3];
  HL7Delimiters.EscapeCharacter := DelimiterDefinition[4];
end;

function THL7Message.CompiledDelimiters(const delimiters: THL7Delimiters): str5;
begin
  if delimiters.SegmentTerminator = char(0) then
    Result := STANDARD_DELIMITERS
  else
    Result := delimiters.FieldSeparator + delimiters.ComponentSeparator +
      delimiters.RepetitionSeparator + delimiters.EscapeCharacter +
      delimiters.SubcomponentSeparator;
end;

function THL7Message.Encoded(const aString: ansistring): ansistring;
  { Escapes encoding characters }
var
  theString: ansistring;
begin
  theString := AnsiReplaceText(aString, '\', ESCAPE_ESCAPE);
  theString := AnsiReplaceText(theString, Delimiters.FieldSeparator, ESCAPE_FIELD);
  theString := AnsiReplaceText(theString, Delimiters.RepetitionSeparator,
    ESCAPE_REPETITION);
  theString := AnsiReplaceText(theString, Delimiters.ComponentSeparator,
    ESCAPE_COMPONENT);
  theString := AnsiReplaceText(theString, Delimiters.SubcomponentSeparator,
    ESCAPE_SUBCOMPONENT);
  Result := theString;
end;

function THL7Message.EncodedHex(const aNumber: integer): string;
var
  theString: string;
begin
  theString := IntToHex(aNumber, 0);
  theString := '\X' + theString + '\';
  Result := theString;
end;

function THL7Message.Decoded(const aString: ansistring): ansistring;
var
  theString: ansistring;
begin
  theString := AnsiReplaceText(aString, ESCAPE_ESCAPE, '\');
  theString := AnsiReplaceText(theString, ESCAPE_FIELD, Delimiters.FieldSeparator);
  theString := AnsiReplaceText(theString, ESCAPE_REPETITION,
    Delimiters.RepetitionSeparator);
  theString := AnsiReplaceText(theString, ESCAPE_COMPONENT,
    Delimiters.ComponentSeparator);
  theString := AnsiReplaceText(theString, ESCAPE_SUBCOMPONENT,
    Delimiters.SubcomponentSeparator);
  Result := theString;
end;

function THL7Message.DecodedHex(const aString: string): integer;
var
  theString: string;
  theNumber: integer;
begin
  theString := aString;
  Delete(theString, 1, 2);
  Delete(theString, length(theString), 1);
  theNumber := Hex2Dec(theString);
  Result := theNumber;
end;

constructor THL7Message.Create(version: string);
begin
  inherited Create;
  ParseDelimiters(STANDARD_DELIMITERS);  {Default delimiter definition}
  Status := noErr;
  ErrorCondition := ERROR_COND_MSG_ACC;
  HL7_Version := version;
  FirstSegment := nil;
end;

destructor THL7Message.Destroy;
begin
  if FirstSegment <> nil then
    FirstSegment.Destroy;
  inherited Destroy;
end;

function THL7Message.FoundSegment(const aSegmentName, SetID: Str3): THL7Segment;
var
  found: boolean;
  currSegment: THL7Segment;
begin
  found := False;
  Result := nil;
  currSegment := FirstSegment;
  repeat
    if (currSegment <> nil) and (currSegment.SegmentName = aSegmentName) and
      ((SetID = '0') or
      (currSegment.FirstOccurrence.FirstField.nextSibling.contentString = SetID)) then
    begin
      found := True;
      Result := currSegment;
    end
    else
    if currSegment <> nil then
      currSegment := currSegment.nextSibling;
  until (found = True) or (currSegment = nil);
end;

function THL7Message.FoundSegment(const aSegmentName, SetID: Str3;
  beginWith: THL7Segment): THL7Segment;
var
  found: boolean;
  currSegment: THL7Segment;
begin
  found := False;
  Result := nil;
  currSegment := beginWith;
  repeat
    if (currSegment <> nil) and (currSegment.SegmentName = aSegmentName) and
      ((SetID = '0') or
      (currSegment.FirstOccurrence.FirstField.nextSibling.contentString = SetID)) then
    begin
      found := True;
      Result := currSegment;
    end
    else
    if currSegment <> nil then
      currSegment := currSegment.nextSibling;
  until (found = True) or (currSegment = nil);
end;

function THL7Message.FoundSegment(const aSegmentName, SetID: Str3;
  beginWith: THL7Segment; out lastSegment: THL7Segment): THL7Segment;
var
  found: boolean;
  currSegment: THL7Segment;
begin
  found := False;
  Result := nil;
  lastSegment := nil;
  currSegment := beginWith;
  repeat
    if (currSegment <> nil) and (currSegment.SegmentName = aSegmentName) and
      ((SetID = '0') or
      (currSegment.FirstOccurrence.FirstField.nextSibling.contentString = SetID)) then
    begin
      found := True;
      Status := noErr;
      ErrorCondition := ERROR_COND_MSG_ACC;
      Result := currSegment;
    end
    else
    if currSegment <> nil then
      begin
        lastSegment := currSegment;
        currSegment := currSegment.nextSibling;
      end;
  until (found = True) or (currSegment = nil);
  if not found then
    begin
      Status := segNotFound;
      ErrorCondition := ERROR_COND_SEG_SEQ_ERR;
    end;
end;

function THL7Message.NewSegment: THL7Segment;
var
  theSegment, currSegment: THL7Segment;
begin
  theSegment := THL7Segment.Create(self, '');
  currSegment := FirstSegment;
  if currSegment = nil then
    FirstSegment := theSegment
  else
  begin
    while currSegment.NextSibling <> nil do
      currSegment := currSegment.NextSibling;
    currSegment.FNextSibling := theSegment;
  end;
  Result := theSegment;
end;

procedure THL7Message.AddSegment(theSegment: THL7Segment);
var
  currSegment: THL7Segment;
begin
  currSegment := FirstSegment;
  if currSegment = nil then
    FirstSegment := theSegment
  else
  begin
    while currSegment.NextSibling <> nil do
      currSegment := currSegment.NextSibling;
    currSegment.FNextSibling := theSegment;
  end;
end;

procedure THL7Message.DeleteSegment(const aSegmentName, SetID: Str3);
var
  currSegment, tempSegment, lastSegment: THL7Segment;
begin
  currSegment := FoundSegment(aSegmentName, SetID, self.FirstSegment, lastSegment);
  if currSegment <> nil then
    begin
      tempSegment := currSegment.nextSibling;
      if lastSegment = nil then
        self.FirstSegment := tempSegment
      else
        lastSegment.FnextSibling := tempSegment;
      currSegment.FnextSibling := nil;
      currSegment.Destroy;
    end;
end;

procedure THL7Message.ReplaceSegment(const aSegmentName, SetID: Str3;
  by: THL7Segment);
var
  currSegment, lastSegment: THL7Segment;
begin
  currSegment := FoundSegment(aSegmentName, SetID, self.FirstSegment, lastSegment);
  if currSegment <> nil then
    begin
      by.FnextSibling := currSegment.nextSibling;
      if lastSegment = nil then
        self.FirstSegment := by
      else
        lastSegment.FnextSibling := by;
      currSegment.FnextSibling := nil;
      currSegment.Destroy;
    end;
end;

procedure THL7Message.ReplaceSegment(const aSegmentName, SetID: Str3;
  by: THL7Segment; insertAlways: boolean);
var
  currSegment, lastSegment: THL7Segment;
begin
  currSegment := FoundSegment(aSegmentName, SetID, self.FirstSegment, lastSegment);
  if currSegment = nil then
  begin
    currSegment := FirstSegment;
    if currSegment = nil then
      FirstSegment := by
    else
    begin
      while currSegment.NextSibling <> nil do
        currSegment := currSegment.NextSibling;
      currSegment.FNextSibling := by;
    end;
  end
  else
    begin
      by.FnextSibling := currSegment.nextSibling;
      if lastSegment = nil then
        self.FirstSegment := by
      else
        lastSegment.FnextSibling := by;
      currSegment.FnextSibling := nil;
      currSegment.Destroy;
    end;
end;

procedure THL7Message.AllocSegments(const SegmentText: ansistring);
var
  theSegment: THL7Segment;
  lastPos: integer;
  singleSegmentText: ansistring;
begin
  theSegment := NewSegment;
  theSegment.contentString := SegmentText;
  if (pos(Delimiters.SegmentTerminator, SegmentText) = 0) then
    theSegment.ParseMessageString(SegmentText)
  else
  begin
    lastPos := 0;
    while lastPos < length(SegmentText) do
    begin
      singleSegmentText :=
        NextSection(SegmentText, lastPos, Delimiters.SegmentTerminator);
      theSegment.ParseMessageString(singleSegmentText);
      if (lastPos < length(SegmentText)) or (singleSegmentText = '') then
      begin
        theSegment.FNextSibling := THL7Segment.Create(self, '');
        theSegment := theSegment.FNextSibling;
      end;
    end;
  end;
end;


end.
