unit HL7;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit}

{ Version 0.9 }

 { (c) J. W. Dietrich, 1994 - 2013 }
 { (c) Ludwig Maximilian University of Munich 1995 - 2002 }
 { (c) University of Ulm Hospitals 2002-2004 }
 { (c) Ruhr University of Bochum 2005 - 2013 }

{ Parser and converter for measurement units }

 { Source code released under the BSD License }
 { See http://puma-repository.sf.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, StrUtils;

const
  STANDARD_DELIMITERS = '|^~\&';
  ACKNOWLEDGEMENT_OK = 'AA';
  ACKNOWLEDGEMENT_ERROR = 'AE';
  ACKNOWLEDGEMENT_REJECT = 'AR';
  ESCAPE_HIGHLIGHTING = '\H\';   {start highlighting}
  ESCAPE_NORMAL     = '\N\';     {end highlighting}
  ESCAPE_FIELD      = '\F\';
  ESCAPE_COMPONENT  = '\S\';
  ESCAPE_SUBCOMPONENT = '\T\';
  ESCAPE_REPETITION = '\R\';
  ESCAPE_ESCAPE     = '\E\';

type

  THL7Delimiters = record
    SegmentTerminator, FieldSeparator, ComponentSeparator: char;
    SubcomponentSeparator, RepetitionSeparator, EscapeCharacter: char;
  end;

  THL7Message    = class;
  THL7Segment    = class;
  THL7Occurrence = class;
  THL7Field      = class;
  THL7Component  = class;
  THL7SubComponent = class;

  { THL7MessageSection }

  THL7MessageSection = class
  private
    FText: AnsiString;
  protected
    FOwner:   THL7MessageSection;
    FMessage: THL7Message;
    destructor Destroy; override;
    procedure ParseMessageString(const aString: AnsiString); virtual; abstract;
    function CompiledMessageString: AnsiString; virtual; abstract;
  public
    property contentString: AnsiString Read CompiledMessageString Write ParseMessageString;
  end;

  { THL7Segment }

  THL7Segment = class(THL7MessageSection)
  protected
    FPreviousSibling, FNextSibling: THL7Segment;
    SegmentName: AnsiString;
    FlOwner:     THL7Message;
    procedure ParseMessageString(const aString: AnsiString);
    function CompiledMessageString: AnsiString;
  public
    FirstOccurrence: THL7Occurrence;
    constructor Create(owner: THL7Message; SegmentText: AnsiString);
    destructor Destroy; override;
    function NewOccurrence(const OccurrencesText: AnsiString): THL7Occurrence;
    procedure AllocOccurrences(const OccurrencesText: AnsiString);
    property contentString: AnsiString Read CompiledMessageString Write ParseMessageString;
    property previousSibling: THL7Segment Read FPreviousSibling;
    property nextSibling: THL7Segment Read FNextSibling;
  end;

  { THL7Occurrence }

  THL7Occurrence = class(THL7MessageSection)
  protected
    FPreviousSibling, FNextSibling: THL7Occurrence;
    procedure ParseMessageString(const aString: AnsiString);
    function CompiledMessageString: AnsiString;
  public
    FirstField: THL7Field;
    constructor Create(owner: THL7Segment; OccurrencesText: AnsiString);
    destructor Destroy; override;
    function NewField: THL7Field;
    procedure AllocFields(const FieldText: AnsiString);
    property contentString: AnsiString Read CompiledMessageString Write ParseMessageString;
    property previousSibling: THL7Occurrence Read FPreviousSibling;
    property nextSibling: THL7Occurrence Read FNextSibling;
  end;

  { THL7Field }

  THL7Field = class(THL7MessageSection)
  protected
    FPreviousSibling, FNextSibling: THL7Field;
    procedure ParseMessageString(const aString: AnsiString);
    function CompiledMessageString: AnsiString;
  public
    FirstComponent: THL7Component;
    constructor Create(owner: THL7Occurrence; FieldText: AnsiString);
    destructor Destroy; override;
    function NewComponent: THL7Component;
    procedure AllocComponents(const ComponentText: AnsiString);
    property contentString: AnsiString Read CompiledMessageString Write ParseMessageString;
    property previousSibling: THL7Field Read FPreviousSibling;
    property nextSibling: THL7Field Read FNextSibling;
  end;

  { THL7Component }

  THL7Component = class(THL7MessageSection)
  protected
    FPreviousSibling, FNextSibling: THL7Component;
    procedure ParseMessageString(const aString: AnsiString);
    function CompiledMessageString: AnsiString;
  public
    FirstSubComponent: THL7SubComponent;
    constructor Create(owner: THL7Field; ComponentText: AnsiString);
    destructor Destroy; override;
    function NewSubComponent: THL7SubComponent;
    procedure AllocSubComponents(const SubComponentText: AnsiString);
    property contentString: AnsiString Read CompiledMessageString Write ParseMessageString;
    property previousSibling: THL7Component Read FPreviousSibling;
    property nextSibling: THL7Component Read FNextSibling;
  end;

  { THL7SubComponent }

  THL7SubComponent = class(THL7MessageSection)
  protected
    FPreviousSibling, FNextSibling: THL7SubComponent;
    procedure ParseMessageString(const aString: AnsiString);
    function CompiledMessageString: AnsiString;
  public
    constructor Create(owner: THL7Component; SubComponentText: AnsiString);
    destructor Destroy; override;
    property contentString: AnsiString Read CompiledMessageString Write ParseMessageString;
    property previousSibling: THL7SubComponent Read FPreviousSibling;
    property nextSibling: THL7SubComponent Read FNextSibling;
  end;

  { THL7Message }

  THL7Message = class
  private
    HL7_version:   string;
    HL7Delimiters: THL7Delimiters;
  protected
    HL7Text: AnsiString;
    procedure SetHL7Version(const aValue: string);
    procedure ParseMessageString(const aString: AnsiString);
    function CompiledMessageString: AnsiString;
  public
    FirstSegment: THL7Segment;
    procedure SetDelimiters(DelimiterDefinition: AnsiString);
    function Encoded(const aString: AnsiString): AnsiString;
    function EncodedHex(const aNumber: integer): string;
    function Decoded(const aString: AnsiString): AnsiString;
    function DecodedHex(const aString: string): integer;
    constructor Create(version: string);
    destructor Destroy; override;
    property HL7Version: string Read HL7_version Write SetHL7Version;
    property Delimiters: THL7Delimiters Read HL7Delimiters Write HL7Delimiters;
    function FoundSegment(const aSegmentName: AnsiString): THL7Segment;
    function NewSegment: THL7Segment;
    procedure AllocSegments(const SegmentText: AnsiString);
    property contentString: AnsiString Read CompiledMessageString Write ParseMessageString;
  end;

procedure ReadHL7File(out HL7Doc: THL7Message; const aFileName: AnsiString); overload;
procedure ReadHL7File(out HL7Doc: THL7Message; var f: Text); overload;
procedure ReadHL7File(out HL7Doc: THL7Message; f: TStream); overload;
procedure ReadHL7File(out HL7Doc: THL7Message; f: TStream; const aBaseURI: AnsiString);
  overload;
procedure WriteHL7File(HL7Doc: THL7Message; const AFileName: AnsiString); overload;
procedure WriteHL7File(HL7Doc: THL7Message; var AFile: Text); overload;
procedure WriteHL7File(HL7Doc: THL7Message; AStream: TStream); overload;


implementation

procedure ReadHL7File(out HL7Doc: THL7Message; const aFileName: AnsiString);
begin

end;

procedure ReadHL7File(out HL7Doc: THL7Message; var f: Text);
begin

end;

procedure ReadHL7File(out HL7Doc: THL7Message; f: TStream);
begin

end;

procedure ReadHL7File(out HL7Doc: THL7Message; f: TStream; const aBaseURI: AnsiString);
begin

end;

procedure WriteHL7File(HL7Doc: THL7Message; const AFileName: AnsiString);
begin

end;

procedure WriteHL7File(HL7Doc: THL7Message; var AFile: Text);
begin

end;

procedure WriteHL7File(HL7Doc: THL7Message; AStream: TStream);
begin

end;

function NextSection(const aString: AnsiString; var Pos: integer; const delim: char): AnsiString;
{extracts a substring from a AnsiString, beginning with Pos and ending with delim}
var
  i, j, l:   integer;
  theString: AnsiString;
begin
  theString := aString;
  if pos > 1 then
    Delete(theString, 1, pos);
  i := system.pos(delim, theString);
  if i = 0 then
  begin
    Result := theString;
    pos    := length(aString);
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

procedure THL7SubComponent.ParseMessageString(const aString: AnsiString);
begin
  FText := aString;
end;

function THL7SubComponent.CompiledMessageString: AnsiString;
begin
  Result := FText;
end;

constructor THL7SubComponent.Create(owner: THL7Component; SubComponentText: AnsiString);
begin
  inherited Create;
  FOwner := owner;
  if owner <> nil then
    FMessage    := owner.FMessage;
  FNextSibling  := nil;
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

procedure THL7Component.ParseMessageString(const aString: AnsiString);
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

function THL7Component.CompiledMessageString: AnsiString;
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

constructor THL7Component.Create(owner: THL7Field; ComponentText: AnsiString);
begin
  inherited Create;
  FOwner := owner;
  if owner <> nil then
    FMessage    := owner.FMessage;
  FNextSibling  := nil;
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
  theSubcomponent  := THL7Subcomponent.Create(self, '');
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

procedure THL7Component.AllocSubComponents(const SubComponentText: AnsiString);
var
  theSubcomponent: THL7Subcomponent;
  lastPos: integer;
  singleSubComponentText: AnsiString;
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
      if lastPos < length(SubComponentText) then
      begin
        theSubComponent.FNextSibling := THL7SubComponent.Create(self, '');
        theSubComponent := theSubComponent.FNextSibling;
      end;
    end;
  end;
end;

{ THL7Field }

procedure THL7Field.ParseMessageString(const aString: AnsiString);
begin
  FText := aString;
  if (aString <> '') and (FMessage <> nil) and
    (pos(FMessage.Delimiters.FieldSeparator, aString) = 0) and
    ((pos(FMessage.Delimiters.ComponentSeparator, aString) > 0) or
    (pos(FMessage.Delimiters.SubComponentSeparator, aString) > 0)) then
    {true fields only}
    AllocComponents(aString);
end;

function THL7Field.CompiledMessageString: AnsiString;
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

constructor THL7Field.Create(owner: THL7Occurrence; FieldText: AnsiString);
begin
  inherited Create;
  FOwner := owner;
  if owner <> nil then
    FMessage    := owner.FMessage;
  FNextSibling  := nil;
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
  theComponent  := THL7Component.Create(self, '');
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

procedure THL7Field.AllocComponents(const ComponentText: AnsiString);
var
  theComponent: THL7Component;
  lastPos:      integer;
  singleComponentText: AnsiString;
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
      if lastPos <= length(ComponentText) then
      begin
        theComponent.FNextSibling := THL7Component.Create(self, '');
        theComponent := theComponent.FNextSibling;
      end;
    end;
  end;
end;

{ THL7Occurrence }

procedure THL7Occurrence.ParseMessageString(const aString: AnsiString);
begin
  FText := aString;
  AllocFields(aString);
end;

function THL7Occurrence.CompiledMessageString: AnsiString;
begin
  Result := FText;
end;

constructor THL7Occurrence.Create(owner: THL7Segment; OccurrencesText: AnsiString);
begin
  inherited Create;
  FOwner := owner;
  if owner <> nil then
    FMessage    := owner.FMessage;
  FNextSibling  := nil;
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
  theField  := THL7Field.Create(self, '');
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

procedure THL7Occurrence.AllocFields(const FieldText: AnsiString);
var
  theField: THL7Field;
  lastPos:  integer;
  singleFieldText: AnsiString;
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
      if lastPos <= length(FieldText) then
      begin
        theField.FNextSibling := THL7Field.Create(self, '');
        theField := theField.FNextSibling;
      end;
    end;
  end;
end;

{ THL7Segment }

procedure THL7Segment.ParseMessageString(const aString: AnsiString);
begin
  FText := aString;
  SegmentName := LeftStr(FText, 3);
  if (aString <> '') and (FMessage <> nil) and
    (pos(FMessage.Delimiters.SegmentTerminator, aString) = 0) then
    {true segments only}
    NewOccurrence(aString);
end;

function THL7Segment.CompiledMessageString: AnsiString;
begin
  Result := FText;
end;

constructor THL7Segment.Create(owner: THL7Message; SegmentText: AnsiString);
begin
  inherited Create;
  FlOwner      := owner;
  FMessage     := FlOwner;
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

function THL7Segment.NewOccurrence(const OccurrencesText: AnsiString): THL7Occurrence;
var
  theOccurrence, currOccurrence: THL7Occurrence;
begin
  theOccurrence  := THL7Occurrence.Create(self, OccurrencesText);
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

procedure THL7Segment.AllocOccurrences(const OccurrencesText: AnsiString);
begin

end;

{ THL7Message }

procedure THL7Message.SetHL7Version(const aValue: string);
begin
  HL7_version := aValue;
end;

procedure THL7Message.ParseMessageString(const aString: AnsiString);
begin
  HL7Text := aString;
  if (aString <> '') then
    {true fields only}
    AllocSegments(aString);
end;

function THL7Message.CompiledMessageString: AnsiString;
begin
  Result := HL7Text;
end;

procedure THL7Message.SetDelimiters(DelimiterDefinition: AnsiString);
begin
  HL7Delimiters.SegmentTerminator := char(13);
  if DelimiterDefinition = '' then
    DelimiterDefinition := STANDARD_DELIMITERS;
  HL7Delimiters.FieldSeparator := DelimiterDefinition[1];
  HL7Delimiters.ComponentSeparator := DelimiterDefinition[2];
  HL7Delimiters.SubcomponentSeparator := DelimiterDefinition[5];
  HL7Delimiters.RepetitionSeparator := DelimiterDefinition[3];
  HL7Delimiters.EscapeCharacter := DelimiterDefinition[4];
end;

function THL7Message.Encoded(const aString: AnsiString): AnsiString;
  { Escapes encoding characters }
var
  theString: AnsiString;
begin
  theString := AnsiReplaceText(aString, '\', ESCAPE_ESCAPE);
  theString := AnsiReplaceText(theString, Delimiters.FieldSeparator, ESCAPE_FIELD);
  theString := AnsiReplaceText(theString, Delimiters.RepetitionSeparator,
    ESCAPE_REPETITION);
  theString := AnsiReplaceText(theString, Delimiters.ComponentSeparator,
    ESCAPE_COMPONENT);
  theString := AnsiReplaceText(theString, Delimiters.SubcomponentSeparator,
    ESCAPE_SUBCOMPONENT);
  Result    := theString;
end;

function THL7Message.EncodedHex(const aNumber: integer): string;
var
  theString: string;
begin
  theString := IntToHex(aNumber, 0);
  theString := '\X' + theString + '\';
  Result    := theString;
end;

function THL7Message.Decoded(const aString: AnsiString): AnsiString;
var
  theString: AnsiString;
begin
  theString := AnsiReplaceText(aString, ESCAPE_ESCAPE, '\');
  theString := AnsiReplaceText(theString, ESCAPE_FIELD, Delimiters.FieldSeparator);
  theString := AnsiReplaceText(theString, ESCAPE_REPETITION,
    Delimiters.RepetitionSeparator);
  theString := AnsiReplaceText(theString, ESCAPE_COMPONENT,
    Delimiters.ComponentSeparator);
  theString := AnsiReplaceText(theString, ESCAPE_SUBCOMPONENT,
    Delimiters.SubcomponentSeparator);
  Result    := theString;
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
  Result    := theNumber;
end;

constructor THL7Message.Create(version: string);
begin
  inherited Create;
  SetDelimiters(STANDARD_DELIMITERS);  {Default delimiter definition}
  HL7_Version  := version;
  FirstSegment := nil;
end;

destructor THL7Message.Destroy;
begin
  if FirstSegment <> nil then
    FirstSegment.Destroy;
  inherited Destroy;
end;

function THL7Message.FoundSegment(const aSegmentName: AnsiString): THL7Segment;
begin

end;

function THL7Message.NewSegment: THL7Segment;
var
  theSegment, currSegment: THL7Segment;
begin
  theSegment  := THL7Segment.Create(self, '');
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

procedure THL7Message.AllocSegments(const SegmentText: AnsiString);
var
  theSegment: THL7Segment;
  lastPos:    integer;
  singleSegmentText: AnsiString;
begin
  theSegment := NewSegment;
  theSegment.contentString := SegmentText;
  if (pos(Delimiters.SegmentTerminator, SegmentText) = 0) then
    theSegment.ParseMessageString(SegmentText)
  else
  begin
    lastPos := 0;
    while lastPos < length(SegmentText) - 1 do
    begin
      singleSegmentText :=
        NextSection(SegmentText, lastPos, Delimiters.SegmentTerminator);
      theSegment.ParseMessageString(singleSegmentText);
      if lastPos < length(SegmentText) - 1 then
      begin
        theSegment.FNextSibling := THL7Segment.Create(self, '');
        theSegment := theSegment.FNextSibling;
      end;
    end;
  end;
end;


end.