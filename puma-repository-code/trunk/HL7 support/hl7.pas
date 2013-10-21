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
  Classes, SysUtils;

const
  STANDARD_DELIMITERS = '|^~\&';

type

  THL7Delimiters = record
    SegmentTerminator, FieldSeparator, ComponentSeparator: char;
    SubcomponentSeparator, RepetitionSeparator, EscapeCharacter: char;
  end;

  THL7Message = class;
  THL7Segment = class;
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
    destructor Destroy; override;
    procedure SetContent(const aString: string); virtual; abstract;
  public
    property contentString: string read FText write SetContent;
  end;

  { THL7Segment }

  THL7Segment = class(THL7MessageSection)
  protected
    SegmentName: string;
    FlOwner: THL7Message;
    procedure SetContent(const aString: string);
  public
    FirstField: THL7Field;
    constructor Create(owner: THL7Message; SegmentText: string);
    destructor Destroy; override;
    property contentString: string read FText write SetContent;
  end;

  { THL7Field }

  THL7Field = class(THL7MessageSection)
  protected
    procedure SetContent(const aString: string);
  public
    FirstComponent: THL7Component;
    constructor Create(owner: THL7Segment; FieldText: string);
    destructor Destroy; override;
    property contentString: string read FText write SetContent;
  end;

  { THL7Component }

  THL7Component = class(THL7MessageSection)
  public
    FirstSubComponent: THL7SubComponent;
    constructor Create(owner: THL7Field; FieldText: string);
    destructor Destroy; override;
    property contentString: string read FText write FText;
  end;

  THL7SubComponent = class(THL7MessageSection)
  public
    property contentString: string read FText write FText;
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
    constructor Create(version: string);
    destructor Destroy; override;
    property HL7Version: string read HL7_version write SetHL7Version;
    property Delimiters: THL7Delimiters read HL7Delimiters write HL7Delimiters;
    function FoundSegment(const aSegmentName: string): THL7Segment;
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

{ THL7Component }

constructor THL7Component.Create(owner: THL7Field; FieldText: string);
begin
  inherited Create;
  FOwner := owner;
  FNextSibling := nil;
  FText := FieldText;
end;

destructor THL7Component.Destroy;
begin
  inherited Destroy;
end;

{ THL7Field }

procedure THL7Field.SetContent(const aString: string);
begin
  FText := aString;
end;

constructor THL7Field.Create(owner: THL7Segment; FieldText: string);
begin
  inherited Create;
  FOwner := owner;
  FNextSibling := nil;
  FText := FieldText;
  FirstComponent := THL7Component.Create(self, '');
end;

destructor THL7Field.Destroy;
begin
  if FirstComponent <> nil then
    FirstComponent.Destroy;
  inherited Destroy;
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
  FNextSibling := nil;
  contentString := SegmentText;
  FirstField := THL7Field.Create(self, '');
end;

destructor THL7Segment.Destroy;
begin
  if FirstField <> nil then
    FirstField.Destroy;
  inherited Destroy;
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
    DelimiterDefinition := STANDARD_DELIMITERS
  else
  begin
    HL7Delimiters.FieldSeparator := DelimiterDefinition[1];
    HL7Delimiters.ComponentSeparator := DelimiterDefinition[2];
    HL7Delimiters.SubcomponentSeparator := DelimiterDefinition[5];
    HL7Delimiters.RepetitionSeparator := DelimiterDefinition[3];
    HL7Delimiters.EscapeCharacter := DelimiterDefinition[4];
  end;
end;

constructor THL7Message.Create(version: string);
begin
  inherited Create;
  SetDelimiters(STANDARD_DELIMITERS);  {Default delimiter definition}
  HL7_Version := version;
  FirstSegment := THL7Segment.Create(self, '');
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


end.
