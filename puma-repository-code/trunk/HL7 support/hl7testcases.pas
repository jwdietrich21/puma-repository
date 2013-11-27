unit HL7TestCases;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit}

{ Version 0.9 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ Parser and compiler for HL7 messages }

{ Source code released under the BSD License }
{ See http://puma-repository.sf.net for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, HL7;

const
  EXAMPLE_SEGMENT1 =
    'MSH|^~\&|EPIC|EPICADT|SMS|SMSADT|199912271408|CHARRIS|ADT^A04|1817457|D|2.5|';
  EXAMPLE_SEGMENT2 =
    'PID||0493575^^^2^ID 1|454721||DOE^JOHN^^^^|DOE^JOHN^^^^|19480203|M||B|254 MYSTREET AVE^^MYTOWN^OH^44123^USA||(216)123-4567|||M|NON|400003403~1129086|';
  EXAMPLE_SEGMENT3 =
    'NK1||ROE^MARIE^^^^|SPO||(216)123-4567||EC|||||||||||||||||||||||||||';
  EXAMPLE_SEGMENT4 =
    'PV1||O|168 ~219~C~PMA^^^^^^^^^||||277^ALLEN MYLASTNAME^BONNIE^^^^|||||||||| ||2688684|||||||||||||||||||||||||199912271408||||||002376853';
  EXAMPLE_SEGMENT5 = 'PID|||||Thomas&Gregory||19481211|M';
  EXAMPLE_SEGMENT6 = 'OBR|||||||||';
  EXAMPLE_SEGMENT7 = 'OBR|||||""||||';
  EXAMPLE_FIELD1   = '0493575^^^2^ID 1';
  EXAMPLE_FIELD2   = '168 ~219~C~PMA^^^^^^^^^';
  EXAMPLE_FIELD3   = 'DOE^JOHN^^^^';
  EXAMPLE_FIELD4   = '254 MYSTREET AVE^^MYTOWN^OH^44123^USA';
  EXAMPLE_FIELD5   = 'BID&Twice a day at institution specified times&HL7xxx^^^^12^h^Y|';
  EXAMOLE_FIELD6   = '13.5&18^M~12.0 & 16^F';
  EXAMPLE_MESSAGE  = EXAMPLE_SEGMENT1 + SEGMENT_DELIMITER + EXAMPLE_SEGMENT2 +
    SEGMENT_DELIMITER + EXAMPLE_SEGMENT3 + SEGMENT_DELIMITER + EXAMPLE_SEGMENT4;

type

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
  end;

  { TMessageTestCases }

  TMessageTestCases = class(TTestCase)
  published
    procedure VersionTestCase1;
    procedure WholeMessageParseTestCase1;
    procedure WholeMessageCompileTestCase1;
  end;

  { TStringEncodingTestCases }

  TStringEncodingTestCases = class(TTestCase)
  published
    procedure DelimiterTestCase1;
    procedure DelimiterTestCase2;
    procedure EncodingTestCase1;
    procedure EncodingTestCase2;
    procedure DecodingTestCase1;
    procedure DecodingTestCase2;
  end;

  { TSegmentsTestCases }

  TSegmentsTestCases = class(TTestCase)
  published
    procedure SegmentsParseTestCase1;
    procedure SegmentsParseTestCase2;
    procedure SegmentsParseTestCase3;
    procedure SegmentsCompileTestCase1;
  end;

  { TFieldsTestCases }

  TFieldsTestCases = class(TTestCase)
  published
    procedure FieldsParseTestCase1;
    procedure FieldsParseTestCase2;
    procedure FieldsParseTestCase3;
    procedure FieldsCompileTestCase1;
  end;

  { TComponentTestCases }

  TComponentTestCases = class(TTestCase)
  published
    procedure ComponentParseTestCase1;
    procedure ComponentParseTestCase2;
    procedure ComponentParseTestCase3;
    procedure ComponentCompileTestCase1;
  end;

  { TSubComponentParseTestCases }

  { TSubComponentTestCases }

  TSubComponentTestCases = class(TTestCase)
  published
    procedure SubComponentParseTestCase1;
    procedure SubComponentParseTestCase2;
    procedure SubComponentParseTestCase3;
  end;



var
  TestHL7Message: THL7Message;

implementation

{ -- Base functionality test -- }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertNull('This test is bound to succeed', nil);
end;

{ TMessageTestCases }

procedure TMessageTestCases.VersionTestCase1;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
    AssertEquals('2.5', TestHL7Message.HL7Version);
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TMessageTestCases.WholeMessageParseTestCase1;
var
  fieldContent: string;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE;
    if (TestHL7Message.FirstSegment = nil) or
      (TestHL7Message.FirstSegment.nextSibling = nil) or
      (TestHL7Message.FirstSegment.nextSibling.nextSibling = nil) then
      fail('Segment could not be created.')
    else
    begin
      if TestHL7Message.FirstSegment.nextSibling.nextSibling.FirstOccurrence = nil then
        fail('Occurrence could not be created.')
      else
      begin
        if TestHL7Message.FirstSegment.nextSibling.nextSibling.FirstOccurrence.FirstField
          = nil then
          fail('Field could not be created.')
        else
        if (TestHL7Message.FirstSegment.nextSibling.nextSibling.
          FirstOccurrence.FirstField.nextSibling = nil) or
          (TestHL7Message.FirstSegment.nextSibling.nextSibling.
          FirstOccurrence.FirstField.nextSibling.nextSibling = nil) then
          fail('Field could not be found.')
        else
        begin
          fieldContent := TestHL7Message.FirstSegment.nextSibling.
            nextSibling.FirstOccurrence.FirstField.nextSibling.nextSibling.contentString;
          AssertEquals('ROE^MARIE^^^^', fieldContent);
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TMessageTestCases.WholeMessageCompileTestCase1;
var
  messageContent: string;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE;
    if (TestHL7Message.FirstSegment = nil) or
      (TestHL7Message.FirstSegment.nextSibling = nil) or
      (TestHL7Message.FirstSegment.nextSibling.nextSibling = nil) then
      fail('Segment could not be created.')
    else
    begin
      if TestHL7Message.FirstSegment.nextSibling.nextSibling.FirstOccurrence = nil then
        fail('Occurrence could not be created.')
      else
      begin
        if TestHL7Message.FirstSegment.nextSibling.nextSibling.FirstOccurrence.FirstField
          = nil then
          fail('Field could not be created.')
        else
        if (TestHL7Message.FirstSegment.nextSibling.nextSibling.
          FirstOccurrence.FirstField.nextSibling = nil) or
          (TestHL7Message.FirstSegment.nextSibling.nextSibling.
          FirstOccurrence.FirstField.nextSibling.nextSibling = nil) then
          fail('Field could not be found.')
        else
        begin
          messageContent := TestHL7Message.contentString;
          AssertEquals(EXAMPLE_MESSAGE, messageContent);
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

{ TStringEncodingTestCases }

procedure TStringEncodingTestCases.DelimiterTestCase1;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.SetDelimiters('');
    AssertEquals('|', TestHL7Message.Delimiters.FieldSeparator);
    AssertEquals('^', TestHL7Message.Delimiters.ComponentSeparator);
    AssertEquals('~', TestHL7Message.Delimiters.RepetitionSeparator);
    AssertEquals('\', TestHL7Message.Delimiters.EscapeCharacter);
    AssertEquals('&', TestHL7Message.Delimiters.SubcomponentSeparator);
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TStringEncodingTestCases.DelimiterTestCase2;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.SetDelimiters('#/@+*');
    AssertEquals('#', TestHL7Message.Delimiters.FieldSeparator);
    AssertEquals('/', TestHL7Message.Delimiters.ComponentSeparator);
    AssertEquals('@', TestHL7Message.Delimiters.RepetitionSeparator);
    AssertEquals('+', TestHL7Message.Delimiters.EscapeCharacter);
    AssertEquals('*', TestHL7Message.Delimiters.SubcomponentSeparator);
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TStringEncodingTestCases.EncodingTestCase1;
const
  STRING_WITH_SPECIAL_SYMBOLS =
    'Escape: \, field: |, repetition: ~, component: ^, subcomponent: &';
  ESCAPED_EXAMPLE_STRING      =
    'Escape: \E\, field: \F\, repetition: \R\, component: \S\, subcomponent: \T\';
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.SetDelimiters('');
    AssertEquals(ESCAPED_EXAMPLE_STRING, TestHL7Message.Encoded(
      STRING_WITH_SPECIAL_SYMBOLS));
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TStringEncodingTestCases.EncodingTestCase2;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    AssertEquals('\X139\', TestHL7Message.EncodedHex(313));
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TStringEncodingTestCases.DecodingTestCase1;
const
  STRING_WITH_SPECIAL_SYMBOLS =
    'Escape: \, field: |, repetition: ~, component: ^, subcomponent: &';
  ESCAPED_EXAMPLE_STRING      =
    'Escape: \E\, field: \F\, repetition: \R\, component: \S\, subcomponent: \T\';
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.SetDelimiters('');
    AssertEquals(STRING_WITH_SPECIAL_SYMBOLS,
      TestHL7Message.Decoded(ESCAPED_EXAMPLE_STRING));
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TStringEncodingTestCases.DecodingTestCase2;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    AssertEquals(313, TestHL7Message.DecodedHex('\X139\'));
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

{ TSegmentsTestCases }

procedure TSegmentsTestCases.SegmentsParseTestCase1;
var
  testSegment: THL7Segment;
begin
  testSegment := THL7Segment.Create(nil, 'test');
  if testSegment = nil then
    fail('Segment could not be created.')
  else
  begin
    testSegment.contentString := EXAMPLE_SEGMENT1;
    AssertEquals(EXAMPLE_SEGMENT1, testSegment.contentString);
  end;
  if testSegment <> nil then
    testSegment.Destroy;
end;

procedure TSegmentsTestCases.SegmentsParseTestCase2;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.AllocSegments(EXAMPLE_SEGMENT2);
    if TestHL7Message.FirstSegment = nil then
      fail('Segment could not be created.')
    else
    begin
      AssertEquals(EXAMPLE_SEGMENT2, TestHL7Message.FirstSegment.contentString);
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TSegmentsTestCases.SegmentsParseTestCase3;
var
  theSegment: THL7Segment;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.AllocSegments(EXAMPLE_SEGMENT3);
    if TestHL7Message.FirstSegment = nil then
      fail('Segment could not be created.')
    else
    begin
      theSegment := TestHL7Message.FirstSegment;
      AssertEquals(EXAMPLE_SEGMENT3, theSegment.contentString);
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TSegmentsTestCases.SegmentsCompileTestCase1;
var
  segmentContent: string;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE;
    if (TestHL7Message.FirstSegment = nil) or
      (TestHL7Message.FirstSegment.nextSibling = nil) or
      (TestHL7Message.FirstSegment.nextSibling.nextSibling = nil) then
      fail('Segment could not be created.')
    else
    begin
      segmentContent := TestHL7Message.FirstSegment.nextSibling.nextSibling.contentString;
      AssertEquals(EXAMPLE_SEGMENT3, segmentContent);
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

{ TFieldsTestCases }

procedure TFieldsTestCases.FieldsParseTestCase1;
var
  TestField: THL7Field;
begin
  TestField := THL7Field.Create(nil, EXAMPLE_FIELD1);
  if TestField = nil then
    fail('Field could not be created.')
  else
  begin
    AssertEquals(EXAMPLE_FIELD1, TestField.contentString);
  end;
  if TestField <> nil then
    TestField.Destroy;
end;

procedure TFieldsTestCases.FieldsParseTestCase2;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    if TestHL7Message.NewSegment = nil then
      fail('Segment could not be created.')
    else
    begin
      TestHL7Message.FirstSegment.NewOccurrence('');
      if TestHL7Message.FirstSegment.FirstOccurrence = nil then
        fail('Occurrence could not be created.')
      else
      begin
        TestHL7Message.FirstSegment.FirstOccurrence.NewField;
        if TestHL7Message.FirstSegment.FirstOccurrence.FirstField = nil then
          fail('Field could not be created.')
        else
        begin
          TestHL7Message.FirstSegment.FirstOccurrence.FirstField.contentString :=
            EXAMPLE_FIELD3;
          AssertEquals(EXAMPLE_FIELD3,
            TestHL7Message.FirstSegment.FirstOccurrence.FirstField.contentString);
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TFieldsTestCases.FieldsParseTestCase3;
var
  fieldContent: string;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.AllocSegments(EXAMPLE_SEGMENT2);
    if TestHL7Message.FirstSegment = nil then
      fail('Segment could not be created.')
    else
    begin
      if TestHL7Message.FirstSegment.FirstOccurrence = nil then
        fail('Occurrence could not be created.')
      else
      begin
        if TestHL7Message.FirstSegment.FirstOccurrence.FirstField = nil then
          fail('Field could not be created.')
        else
        if (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.nextSibling = nil) or
          (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.nextSibling.nextSibling
          = nil) or (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
          nextSibling.nextSibling.nextSibling = nil) then
          fail('Field could not be found.')
        else
        begin
          fieldContent := TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
            nextSibling.nextSibling.nextSibling.contentString;
          AssertEquals('454721', fieldContent);
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TFieldsTestCases.FieldsCompileTestCase1;
var
  fieldContent, componentContent: string;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.AllocSegments(EXAMPLE_SEGMENT5);
    if TestHL7Message.FirstSegment = nil then
      fail('Segment could not be created.')
    else
    begin
      if TestHL7Message.FirstSegment.FirstOccurrence = nil then
        fail('Occurrence could not be created.')
      else
      begin
        if TestHL7Message.FirstSegment.FirstOccurrence.FirstField = nil then
          fail('Field could not be created.')
        else
        if (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.nextSibling = nil) or
          (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.nextSibling.nextSibling
          = nil) or (TestHL7Message.FirstSegment.FirstOccurrence.
          FirstField.nextSibling.nextSibling.nextSibling = nil) or
          (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
          nextSibling.nextSibling.nextSibling.nextSibling = nil) or
          (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
          nextSibling.nextSibling.nextSibling.nextSibling.nextSibling = nil) then
          fail('Field could not be found.')
        else
        begin
          fieldContent := TestHL7Message.FirstSegment.FirstOccurrence.
            FirstField.nextSibling.nextSibling.nextSibling.nextSibling.
            nextSibling.contentString;
          AssertEquals('Thomas&Gregory', fieldContent);
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;


{ TComponentTestCases }

procedure TComponentTestCases.ComponentParseTestCase1;
var
  TestComponent: THL7Component;
begin
  TestComponent := THL7Component.Create(nil, 'test');
  if TestComponent = nil then
    fail('Component could not be created.')
  else
  begin
    AssertEquals('test', TestComponent.contentString);
  end;
  if TestComponent <> nil then
    TestComponent.Destroy;
end;

procedure TComponentTestCases.ComponentParseTestCase2;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    if TestHL7Message.NewSegment = nil then
      fail('Segment could not be created.')
    else
    begin
      TestHL7Message.FirstSegment.NewOccurrence('');
      if TestHL7Message.FirstSegment.FirstOccurrence = nil then
        fail('Occurrence could not be created.')
      else
      begin
        TestHL7Message.FirstSegment.FirstOccurrence.NewField;
        if TestHL7Message.FirstSegment.FirstOccurrence.FirstField = nil then
          fail('Field could not be created.')
        else
        begin
          TestHL7Message.FirstSegment.FirstOccurrence.FirstField.NewComponent;
          if TestHL7Message.FirstSegment.FirstOccurrence.FirstField.FirstComponent
            = nil then
            fail('Component could not be created')
          else
          begin
            TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
              FirstComponent.contentString := 'test';
            AssertEquals('test',
              TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
              FirstComponent.contentString);
          end;
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TComponentTestCases.ComponentParseTestCase3;
var
  fieldContent, componentContent: string;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.AllocSegments(EXAMPLE_SEGMENT3);
    if TestHL7Message.FirstSegment = nil then
      fail('Segment could not be created.')
    else
    begin
      if TestHL7Message.FirstSegment.FirstOccurrence = nil then
        fail('Occurrence could not be created.')
      else
      begin
        if TestHL7Message.FirstSegment.FirstOccurrence.FirstField = nil then
          fail('Field could not be created.')
        else
        if (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.nextSibling = nil) or
          (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.nextSibling.nextSibling
          = nil) then
          fail('Field could not be found.')
        else
        begin
          fieldContent := TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
            nextSibling.nextSibling.contentString;
          AssertEquals('ROE^MARIE^^^^', fieldContent);
          if (TestHL7Message.FirstSegment.FirstOccurrence.
            FirstField.nextSibling.nextSibling.FirstComponent = nil) or
            (TestHL7Message.FirstSegment.FirstOccurrence.
            FirstField.nextSibling.nextSibling.FirstComponent.nextSibling = nil) then
            fail('Component could not be found.')
          else
          begin
            componentContent :=
              TestHL7Message.FirstSegment.FirstOccurrence.
              FirstField.nextSibling.nextSibling.FirstComponent.
              nextSibling.contentString;
            AssertEquals('MARIE', componentContent);
          end;
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TComponentTestCases.ComponentCompileTestCase1;
var
  fieldContent, componentContent: string;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.AllocSegments(EXAMPLE_SEGMENT5);
    if TestHL7Message.FirstSegment = nil then
      fail('Segment could not be created.')
    else
    begin
      if TestHL7Message.FirstSegment.FirstOccurrence = nil then
        fail('Occurrence could not be created.')
      else
      begin
        if TestHL7Message.FirstSegment.FirstOccurrence.FirstField = nil then
          fail('Field could not be created.')
        else
        if (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.nextSibling = nil) or
          (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.nextSibling.nextSibling
          = nil) or (TestHL7Message.FirstSegment.FirstOccurrence.
          FirstField.nextSibling.nextSibling.nextSibling = nil) or
          (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
          nextSibling.nextSibling.nextSibling.nextSibling = nil) or
          (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
          nextSibling.nextSibling.nextSibling.nextSibling.nextSibling = nil) then
          fail('Field could not be found.')
        else
        begin
          fieldContent := TestHL7Message.FirstSegment.FirstOccurrence.
            FirstField.nextSibling.nextSibling.nextSibling.nextSibling.
            nextSibling.contentString;
          AssertEquals('Thomas&Gregory', fieldContent);
          if (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
            nextSibling.nextSibling.nextSibling.nextSibling.nextSibling.FirstComponent =
            nil) then
            fail('Component could not be found.')
          else
          begin
            componentContent :=
              TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
              nextSibling.nextSibling.nextSibling.nextSibling.nextSibling.FirstComponent.
              contentString;
            AssertEquals('Thomas&Gregory', componentContent);
          end;
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

{ TSubComponentTestCases }

procedure TSubComponentTestCases.SubComponentParseTestCase1;
var
  TestSubComponent: THL7SubComponent;
begin
  TestSubComponent := THL7SubComponent.Create(nil, 'test');
  if TestSubComponent = nil then
    fail('Subcomponent could not be created.')
  else
  begin
    AssertEquals('test', TestSubComponent.contentString);
  end;
  if TestSubComponent <> nil then
    TestSubComponent.Destroy;
end;

procedure TSubComponentTestCases.SubComponentParseTestCase2;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.AllocSegments('empty');
    if TestHL7Message.FirstSegment = nil then
      fail('Segment could not be created.')
    else
    begin
      if TestHL7Message.FirstSegment.FirstOccurrence = nil then
        fail('Occurrence could not be created.');
    end;
    begin
      if TestHL7Message.FirstSegment.FirstOccurrence.FirstField = nil then
        fail('Field could not be created.')
      else
      begin
        TestHL7Message.FirstSegment.FirstOccurrence.FirstField.AllocComponents('');
        if TestHL7Message.FirstSegment.FirstOccurrence.FirstField.FirstComponent
          = nil then
          fail('Component could not be created')
        else
        begin
          TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
            FirstComponent.NewSubComponent;
          if TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
            FirstComponent.FirstSubComponent = nil then
            fail('Subcomponent could not be created')
          else
          begin
            TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
              FirstComponent.contentString :=
              'test';
            AssertEquals('test',
              TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
              FirstComponent.contentString);
          end;
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TSubComponentTestCases.SubComponentParseTestCase3;
var
  fieldContent, componentContent, subcomponentContent: string;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.AllocSegments(EXAMPLE_SEGMENT5);
    if TestHL7Message.FirstSegment = nil then
      fail('Segment could not be created.')
    else
    begin
      if TestHL7Message.FirstSegment.FirstOccurrence = nil then
        fail('Occurrence could not be created.')
      else
      begin
        if TestHL7Message.FirstSegment.FirstOccurrence.FirstField = nil then
          fail('Field could not be created.')
        else
        if (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.nextSibling = nil) or
          (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.nextSibling.nextSibling
          = nil) or (TestHL7Message.FirstSegment.FirstOccurrence.
          FirstField.nextSibling.nextSibling.nextSibling = nil) or
          (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
          nextSibling.nextSibling.nextSibling.nextSibling = nil) or
          (TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
          nextSibling.nextSibling.nextSibling.nextSibling.nextSibling = nil) then
          fail('Field could not be found.')
        else
        begin
          fieldContent := TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
            nextSibling.nextSibling.nextSibling.nextSibling.nextSibling.contentString;
          AssertEquals('Thomas&Gregory', fieldContent);
          if TestHL7Message.FirstSegment.FirstOccurrence.
            FirstField.nextSibling.nextSibling.nextSibling.nextSibling.
            nextSibling.FirstComponent = nil then
            fail('Component could not be found.')
          else
          begin
            componentContent :=
              TestHL7Message.FirstSegment.FirstOccurrence.
              FirstField.nextSibling.nextSibling.nextSibling.nextSibling.
              nextSibling.FirstComponent.contentString;
            AssertEquals('Thomas&Gregory', componentContent);
            if TestHL7Message.FirstSegment.FirstOccurrence.
              FirstField.nextSibling.nextSibling.nextSibling.nextSibling.
              nextSibling.FirstComponent.FirstSubComponent.nextSibling = nil then
              fail('Subcomponent could not be found.')
            else
            begin
              subcomponentContent :=
                TestHL7Message.FirstSegment.FirstOccurrence.FirstField.
                nextSibling.nextSibling.nextSibling.nextSibling.
                nextSibling.FirstComponent.FirstSubComponent.nextSibling.contentString;
              AssertEquals('Gregory', subcomponentContent);
            end;
          end;
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

initialization
  RegisterTest(TControlTestCases);
  RegisterTest(TMessageTestCases);
  RegisterTest(TStringEncodingTestCases);
  RegisterTest(TSegmentsTestCases);
  RegisterTest(TFieldsTestCases);
  RegisterTest(TComponentTestCases);
  RegisterTest(TSubComponentTestCases);
end.
