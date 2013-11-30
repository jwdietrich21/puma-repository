unit HL7;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 base unit}

{ Version 0.9 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ Parser and compiler for HL7 messages }

{ Source code released under the BSD License }
{ See http://puma-repository.sf.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, StrUtils;

const

  STANDARD_DELIMITERS = '|^~\&';
  SEGMENT_DELIMITER = char(13);

  ACKNOWLEDGEMENT_OK = 'AA';
  ACKNOWLEDGEMENT_ERROR = 'AE';
  ACKNOWLEDGEMENT_REJECT = 'AR';

  ESCAPE_HIGHLIGHTING = '\H\';   {start highlighting}
  ESCAPE_NORMAL = '\N\';     {end highlighting}
  ESCAPE_FIELD = '\F\';
  ESCAPE_COMPONENT = '\S\';
  ESCAPE_SUBCOMPONENT = '\T\';
  ESCAPE_REPETITION = '\R\';
  ESCAPE_ESCAPE = '\E\';

  ESCAPE_ISO_IR6_G0 = '\C2842\';    {ISO 646 : ASCII}
  ESCAPE_ISO_IR100 = '\C2D41\';    {ISO 8859 : Latin Alphabet 1}
  ESCAPE_ISO_IR101 = '\C2D42\';    {ISO 8859 : Latin Alphabet 2}
  ESCAPE_ISO_IR109 = '\C2D43\';    {ISO 8859 : Latin Alphabet 3}
  ESCAPE_ISO_IR110 = '\C2D44\';    {ISO 8859 : Latin Alphabet 4}
  ESCAPE_ISO_IR144 = '\C2D4C\';    {ISO 8859 : Cyrillic}
  ESCAPE_ISO_IR127 = '\C2D47\';    {ISO 8859 : Arabic}
  ESCAPE_ISO_IR126 = '\C2D46\';    {ISO 8859 : Greek}
  ESCAPE_ISO_IR138 = '\C2D48\';    {ISO 8859 : Hebrew}
  ESCAPE_ISO_IR148 = '\C2D4D\';    {ISO 8859 : Latin Alphabet 5}
  ESCAPE_ISO_IR14 = '\C284A\';    {JIS X 0201 -1976: Romaji}
  ESCAPE_ISO_IR13 = '\C2949\';    {JIS X 0201 : Katakana}
  ESCAPE_ISO_IR87 = '\M2442\';    {JIS X 0208 : Kanji, hiragana and katakana}
  ESCAPE_ISO_IR159 = '\M242844\';  {JIS X 0212 : Supplementary Kanji}

  MSH_ID = 'MSH';

type

  str2 = string[2];
  str3 = string[3];
  str5 = string[5];
  str7 = string[7];
  str8 = string[8];
  str15 = string[15];
  str20 = string[20];
  str26 = string[26];
  str30 = string[30];

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
  protected
    HL7Text: ansistring;
    procedure SetHL7Version(const aValue: string);
    procedure ParseMessageString(const aString: ansistring);
    function CompiledMessageString: ansistring;
  public
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
    procedure DeleteSegment(const aSegmentName, SetID: Str3);
    function ReplaceSegment(const aSegmentName, SetID: Str3): THL7Segment;
    function ReplaceSegment(const aSegmentName, SetID: Str3; insertAlways: boolean): THL7Segment;
    procedure AllocSegments(const SegmentText: ansistring);
    property contentString: ansistring read CompiledMessageString
      write ParseMessageString;
  end;

procedure ReadHL7File(out HL7Doc: THL7Message; const aFileName: ansistring); overload;
procedure ReadHL7File(out HL7Doc: THL7Message; var f: Text); overload;
procedure ReadHL7File(out HL7Doc: THL7Message; f: TStream); overload;
procedure ReadHL7File(out HL7Doc: THL7Message; f: TStream; const aBaseURI: ansistring);
  overload;
procedure WriteHL7File(HL7Doc: THL7Message; const AFileName: ansistring); overload;
procedure WriteHL7File(HL7Doc: THL7Message; var AFile: Text); overload;
procedure WriteHL7File(HL7Doc: THL7Message; AStream: TStream); overload;


implementation

procedure ReadHL7File(out HL7Doc: THL7Message; const aFileName: ansistring);
{reads and parses an HL7 message from file}
begin

end;

procedure ReadHL7File(out HL7Doc: THL7Message; var f: Text);
{reads and parses an HL7 message from text file}
begin

end;

procedure ReadHL7File(out HL7Doc: THL7Message; f: TStream);
{reads and parses an HL7 message from stream}
begin

end;

procedure ReadHL7File(out HL7Doc: THL7Message; f: TStream; const aBaseURI: ansistring);
{reads and parses an HL7 message from URI}
begin

end;

procedure WriteHL7File(HL7Doc: THL7Message; const AFileName: ansistring);
{compiles and writes an HL7 message to file}
begin

end;

procedure WriteHL7File(HL7Doc: THL7Message; var AFile: Text);
{compiles and writes an HL7 message to text file}
begin

end;

procedure WriteHL7File(HL7Doc: THL7Message; AStream: TStream);
{compiles and writes an HL7 message to stream}
begin

end;

function NextSection(const aString: ansistring; var Pos: integer;
  const delim: char): ansistring;
  {extracts a substring from a AnsiString, beginning with Pos and ending with delim}
var
  i, j, l: integer;
  theString: ansistring;
begin
  theString := aString;
  if pos > 1 then
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
  curSegment: THL7Segment;
begin
  curSegment := FirstSegment;
  if curSegment = nil then
    Result := HL7Text
  else
  begin
    HL7Text := '';
    while curSegment <> nil do
    begin
      HL7Text := HL7Text + curSegment.contentString;
      curSegment := curSegment.nextSibling;
      if curSegment <> nil then
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
  curSegment: THL7Segment;
begin
  found := False;
  Result := nil;
  curSegment := FirstSegment;
  repeat
    if (curSegment <> nil) and (curSegment.SegmentName = aSegmentName) and
      ((SetID = '0') or
      (curSegment.FirstOccurrence.FirstField.nextSibling.contentString = SetID)) then
    begin
      found := True;
      Result := curSegment;
    end
    else
    if curSegment <> nil then
      curSegment := curSegment.nextSibling;
  until (found = True) or (curSegment = nil);
end;

function THL7Message.FoundSegment(const aSegmentName, SetID: Str3;
  beginWith: THL7Segment): THL7Segment;
var
  found: boolean;
  curSegment: THL7Segment;
begin
  found := False;
  Result := nil;
  curSegment := beginWith;
  repeat
    if (curSegment <> nil) and (curSegment.SegmentName = aSegmentName) and
      ((SetID = '0') or
      (curSegment.FirstOccurrence.FirstField.nextSibling.contentString = SetID)) then
    begin
      found := True;
      Result := curSegment;
    end
    else
    if curSegment <> nil then
      curSegment := curSegment.nextSibling;
  until (found = True) or (curSegment = nil);
end;

function THL7Message.FoundSegment(const aSegmentName, SetID: Str3;
  beginWith: THL7Segment; out lastSegment: THL7Segment): THL7Segment;
var
  found: boolean;
  curSegment: THL7Segment;
begin
  found := False;
  Result := nil;
  lastSegment := nil;
  curSegment := beginWith;
  repeat
    if (curSegment <> nil) and (curSegment.SegmentName = aSegmentName) and
      ((SetID = '0') or
      (curSegment.FirstOccurrence.FirstField.nextSibling.contentString = SetID)) then
    begin
      found := True;
      Result := curSegment;
    end
    else
    if curSegment <> nil then
      begin
        lastSegment := curSegment;
        curSegment := curSegment.nextSibling;
      end;
  until (found = True) or (curSegment = nil);
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
    while currSegment.FNextSibling <> nil do
      currSegment := currSegment.FNextSibling;
    currSegment.FNextSibling := theSegment;
  end;
  Result := theSegment;
end;

procedure THL7Message.DeleteSegment(const aSegmentName, SetID: Str3);
var
  curSegment, tempSegment, lastSegment: THL7Segment;
begin
  curSegment := FoundSegment(aSegmentName, SetID, self.FirstSegment, lastSegment);
  if CurSegment <> nil then
    begin
      tempSegment := CurSegment.nextSibling;
      if lastSegment = nil then
        self.FirstSegment := tempSegment
      else
        lastSegment.FnextSibling := tempSegment;
      curSegment.FnextSibling := nil;
      CurSegment.Destroy;
    end;
end;

function THL7Message.ReplaceSegment(const aSegmentName, SetID: Str3): THL7Segment;
var
  theSegment, currSegment: THL7Segment;
begin
  theSegment := THL7Segment.Create(self, '');
  currSegment := FirstSegment;
  if currSegment = nil then
    FirstSegment := theSegment
  else
  begin
    while currSegment.FNextSibling <> nil do
      currSegment := currSegment.FNextSibling;
    currSegment.FNextSibling := theSegment;
  end;
  Result := theSegment;
end;

function THL7Message.ReplaceSegment(const aSegmentName, SetID: Str3;
  insertAlways: boolean): THL7Segment;
begin

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