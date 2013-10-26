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
  ESCAPE_NORMAL = '\N\';   {end highlighting}
  ESCAPE_FIELD = '\F\';
  ESCAPE_COMPONENT = '\S\';
  ESCAPE_SUBCOMPONENT = '\T\';
  ESCAPE_REPETITION = '\R\';
  ESCAPE_ESCAPE = '\E\';

type

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
    FText: string;
  protected
    FPreviousSibling, FNextSibling: THL7MessageSection;
    FOwner: THL7MessageSection;
    FMessage: THL7Message;
    destructor Destroy; override;
    procedure SetContent(const aString: string); virtual; abstract;
  public
    property contentString: string read FText write SetContent;
    property previousSibling: THL7MessageSection read FPreviousSibling;
    property nextSibling: THL7MessageSection read FNextSibling;
  end;

  { THL7Segment }

  THL7Segment = class(THL7MessageSection)
  protected
    SegmentName: string;
    FlOwner: THL7Message;
    procedure SetContent(const aString: string);
  public
    FirstOccurrence: THL7Occurrence;
    constructor Create(owner: THL7Message; SegmentText: string);
    destructor Destroy; override;
    function NewOccurrence(const OccurrencesText: string): THL7Occurrence;
    property contentString: string read FText write SetContent;
  end;

  { THL7Occurrence }

  THL7Occurrence = class(THL7MessageSection)
  protected
    procedure SetContent(const aString: string);
  public
    FirstField: THL7Field;
    constructor Create(owner: THL7Segment; OccurrencesText: string);
    destructor Destroy; override;
    function NewField(const FieldText: string): THL7Field;
    property contentString: string read FText write SetContent;
  end;

  { THL7Field }

  THL7Field = class(THL7MessageSection)
  protected
    procedure SetContent(const aString: string);
  public
    FirstComponent: THL7Component;
    constructor Create(owner: THL7Occurrence; FieldText: string);
    destructor Destroy; override;
    function NewComponent(const ComponentText: string): THL7Component;
    property contentString: string read FText write SetContent;
  end;

  { THL7Component }

  THL7Component = class(THL7MessageSection)
  protected
    procedure SetContent(const aString: string);
  public
    FirstSubComponent: THL7SubComponent;
    constructor Create(owner: THL7Field; ComponentText: string);
    destructor Destroy; override;
    function NewSubComponent(const SubComponentText: string): THL7SubComponent;
    property contentString: string read FText write SetContent;
  end;

  { THL7SubComponent }

  THL7SubComponent = class(THL7MessageSection)
  protected
    procedure SetContent(const aString: string);
  public
    constructor Create(owner: THL7Component; SubComponentText: string);
    destructor Destroy; override;
    property contentString: string read FText write SetContent;
  end;

  THL7Message = class
  private
    HL7_version: string;
    HL7Delimiters: THL7Delimiters;
  protected
    HL7Text: string;
    procedure SetHL7Version(const aValue: string);
  public
    FirstSegment: THL7Segment;
    procedure SetDelimiters(DelimiterDefinition: string);
    function Encoded(const aString: string): string;
    function EncodedHex(const aNumber: integer): string;
    function Decoded(const aString: string): string;
    function DecodedHex(const aString: string): integer;
    constructor Create(version: string);
    destructor Destroy; override;
    property HL7Version: string read HL7_version write SetHL7Version;
    property Delimiters: THL7Delimiters read HL7Delimiters write HL7Delimiters;
    function FoundSegment(const aSegmentName: string): THL7Segment;
    function NewSegment(const SegmentText: string): THL7Segment;
  end;

procedure ReadHL7File(out ADoc: THL7Message; const aFileName: string); overload;
procedure ReadHL7File(out ADoc: THL7Message; var f: Text); overload;
procedure ReadHL7File(out ADoc: THL7Message; f: TStream); overload;
procedure ReadHL7File(out ADoc: THL7Message; f: TStream; const aBaseURI: string);
  overload;

implementation

procedure ReadHL7File(out ADoc: THL7Message; const aFileName: string);
begin

end;

procedure ReadHL7File(out ADoc: THL7Message; var f: Text);
begin

end;

procedure ReadHL7File(out ADoc: THL7Message; f: TStream);
begin

end;

procedure ReadHL7File(out ADoc: THL7Message; f: TStream; const aBaseURI: string);
begin

end;

function NextSection(const aString: string; var Pos: Integer; const delim: char): string;
var
  i, j, l: Integer;
  theString: string;
begin
  theString := aString;
  if pos > 1 then delete(theString, 1, pos);
  i := system.pos(delim, theString);
  Result := copy(theString, 1, i - 1);
  inc(pos, i);
end;


{ THL7MessageSection }

destructor THL7MessageSection.Destroy;
var
  remainingSiblings: THL7MessageSection;
begin
  remainingSiblings := FNextSibling;
  if remainingSiblings <> nil then
    remainingSiblings.Destroy;
  inherited Destroy;
end;

{ THL7SubComponent }

procedure THL7SubComponent.SetContent(const aString: string);
begin
  FText := aString;
end;

constructor THL7SubComponent.Create(owner: THL7Component; SubComponentText: string);
begin
  inherited Create;
  FOwner := owner;
  if owner <> nil then
    FMessage := owner.FMessage;
  FNextSibling := nil;
  contentString := SubComponentText;
end;

destructor THL7SubComponent.Destroy;
begin
  inherited Destroy;
end;

{ THL7Component }

procedure THL7Component.SetContent(const aString: string);
begin
  FText := aString;
end;

constructor THL7Component.Create(owner: THL7Field; ComponentText: string);
begin
  inherited Create;
  FOwner := owner;
  if owner <> nil then
    FMessage := owner.FMessage;
  FNextSibling := nil;
  if ComponentText = '' then
    FirstSubComponent := nil
  else
    NewSubComponent(ComponentText);
  contentString := ComponentText;
end;

destructor THL7Component.Destroy;
begin
  if FirstSubComponent <> nil then
    FirstSubComponent.Destroy;
  inherited Destroy;
end;

function THL7Component.NewSubComponent(const SubComponentText: string
  ): THL7SubComponent;
var
  theSubcomponent, currSubcomponent: THL7Subcomponent;
begin
  theSubcomponent := THL7Subcomponent.Create(self, SubcomponentText);
  currSubcomponent := FirstSubcomponent;
  if currSubcomponent = nil then
    FirstSubcomponent := theSubcomponent
  else
  begin
    while currSubcomponent.FNextSibling <> nil do
      currSubcomponent := THL7Subcomponent(currSubcomponent.FNextSibling);
    currSubcomponent.FNextSibling := theSubcomponent;
  end;
  Result := theSubcomponent;
end;

{ THL7Field }

procedure THL7Field.SetContent(const aString: string);
begin
  FText := aString;
end;

constructor THL7Field.Create(owner: THL7Occurrence; FieldText: string);
begin
  inherited Create;
  FOwner := owner;
  if owner <> nil then
    FMessage := owner.FMessage;
  FNextSibling := nil;
  if FieldText = '' then
    FirstComponent := nil
  else
    NewComponent(FieldText);
  contentString := FieldText;
end;

destructor THL7Field.Destroy;
begin
  if FirstComponent <> nil then
    FirstComponent.Destroy;
  inherited Destroy;
end;

function THL7Field.NewComponent(const ComponentText: string): THL7Component;
var
  theComponent, currComponent: THL7Component;
begin
  theComponent := THL7Component.Create(self, ComponentText);
  currComponent := FirstComponent;
  if currComponent = nil then
    FirstComponent := theComponent
  else
  begin
    while currComponent.FNextSibling <> nil do
      currComponent := THL7Component(currComponent.FNextSibling);
    currComponent.FNextSibling := theComponent;
  end;
  Result := theComponent;
end;

{ THL7Occurrence }

procedure THL7Occurrence.SetContent(const aString: string);
begin
  FText := aString;
  if FirstField = nil then
    NewField(aString);
  FirstField.contentString := FText;
end;

constructor THL7Occurrence.Create(owner: THL7Segment; OccurrencesText: string);
begin
  inherited Create;
  FOwner := owner;
  if owner <> nil then
    FMessage := owner.FMessage;
  FNextSibling := nil;
  if OccurrencesText = '' then
    FirstField := nil
  else
    NewField(OccurrencesText);
  contentString := OccurrencesText;
end;

destructor THL7Occurrence.Destroy;
begin
  if FirstField <> nil then
    FirstField.Destroy;
  inherited Destroy;
end;

function THL7Occurrence.NewField(const FieldText: string): THL7Field;
var
  theField, currField: THL7Field;
  lastPos: integer;
  singleFieldText: string;
begin
  theField := THL7Field.Create(self, '');
  currField := FirstField;
  if currField = nil then
    FirstField := theField
  else
  begin
    while currField.FNextSibling <> nil do
      currField := THL7Field(currField.FNextSibling);
    currField.FNextSibling := theField;
  end;
  if (FMessage = nil) or (pos(FMessage.Delimiters.FieldSeparator, FieldText) = 0) then
    theField.SetContent(FieldText)
  else
  begin
    lastPos := 0;
    while lastPos < length(FieldText) - 1 do
      begin
        singleFieldText := NextSection(FieldText, lastPos, FMessage.Delimiters.FieldSeparator);
        theField.SetContent(singleFieldText);
        if lastPos < length(FieldText) - 1 then
          begin
          theField.FNextSibling := THL7Field.Create(self, '');
          theField := THL7Field(theField.FNextSibling);
          end;
      end;
  end;
  Result := theField;
end;

{ THL7Segment }

procedure THL7Segment.SetContent(const aString: string);
begin
  FText := aString;
  SegmentName := LeftStr(FText, 3);
end;

constructor THL7Segment.Create(owner: THL7Message; SegmentText: string);
begin
  inherited Create;
  FlOwner := owner;
  FMessage := FlOwner;
  FNextSibling := nil;
  if SegmentText = '' then
    FirstOccurrence := nil
  else
    NewOccurrence(SegmentText);
  contentString := SegmentText;
end;

destructor THL7Segment.Destroy;
begin
  if FirstOccurrence <> nil then
    FirstOccurrence.Destroy;
  inherited Destroy;
end;

function THL7Segment.NewOccurrence(const OccurrencesText: string): THL7Occurrence;
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
      currOccurrence := THL7Occurrence(currOccurrence.FNextSibling);
    currOccurrence.FNextSibling := theOccurrence;
  end;
  Result := theOccurrence;
end;

{ THL7Message }

procedure THL7Message.SetHL7Version(const aValue: string);
begin
  HL7_version := aValue;
end;

procedure THL7Message.SetDelimiters(DelimiterDefinition: string);
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

function THL7Message.Encoded(const aString: string): string;
  { Escapes encoding characters }
var
  theString: string;
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

function THL7Message.Decoded(const aString: string): string;
var
  theString: string;
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
  SetDelimiters(STANDARD_DELIMITERS);  {Default delimiter definition}
  HL7_Version := version;
  FirstSegment := nil;
end;

destructor THL7Message.Destroy;
begin
  if FirstSegment <> nil then
    FirstSegment.Destroy;
  inherited Destroy;
end;

function THL7Message.FoundSegment(const aSegmentName: string): THL7Segment;
begin

end;

function THL7Message.NewSegment(const SegmentText: string): THL7Segment;
var
  theSegment, currSegment: THL7Segment;
begin
  theSegment := THL7Segment.Create(self, SegmentText);
  currSegment := FirstSegment;
  if currSegment = nil then
    FirstSegment := theSegment
  else
  begin
    while currSegment.FNextSibling <> nil do
      currSegment := THL7Segment(currSegment.FNextSibling);
    currSegment.FNextSibling := theSegment;
  end;
  Result := theSegment;
end;


end.