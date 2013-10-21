unit HL7TestCases;

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

type

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
  end;

  { TBaseStructureTestCases }

  TBaseStructureTestCases = class(TTestCase)
  published
    procedure VersionTestCase1;
  end;

  { TStringEncodingTestCases }

  TStringEncodingTestCases = class(TTestCase)
  published
    procedure DelimiterTestCase1;
    procedure DelimiterTestCase2;
  end;

  { TSegmentsTestCases }

  TSegmentsTestCases = class(TTestCase)
  published
    procedure SegmentsTestCase1;
    procedure SegmentsTestCase2;
  end;

  { TFieldsTestCases }

  TFieldsTestCases = class(TTestCase)
  published
    procedure FieldsTestCase1;
    procedure FieldsTestCase2;
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

{ TBaseStructureTestCases }

procedure TBaseStructureTestCases.VersionTestCase1;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
    AssertEquals('2.5', TestHL7Message.HL7Version);
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

{ TSegmentsTestCases }

procedure TSegmentsTestCases.SegmentsTestCase1;
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

procedure TSegmentsTestCases.SegmentsTestCase2;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    if TestHL7Message.FirstSegment = nil then
      fail('Segment could not be created.')
    else
    begin
      TestHL7Message.FirstSegment.contentString := EXAMPLE_SEGMENT2;
      AssertEquals(EXAMPLE_SEGMENT2, TestHL7Message.FirstSegment.contentString);
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

{ TFieldsTestCases }

procedure TFieldsTestCases.FieldsTestCase1;
var
  TestField: THL7Field;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestField.contentString := EXAMPLE_SEGMENT3;
    AssertEquals(EXAMPLE_SEGMENT3, TestField.contentString);
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TFieldsTestCases.FieldsTestCase2;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    if TestHL7Message.FirstSegment = nil then
      fail('Segment could not be created.')
    else
    begin
      if TestHL7Message.FirstSegment.FirstField = nil then
        fail('Field could not be created.')
      else
      begin
        TestHL7Message.FirstSegment.FirstField.contentString := EXAMPLE_SEGMENT4;
        AssertEquals(EXAMPLE_SEGMENT4, TestHL7Message.FirstSegment.FirstField.contentString);
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

initialization
  RegisterTest(TControlTestCases);
  RegisterTest(TBaseStructureTestCases);
  RegisterTest(TStringEncodingTestCases);
  RegisterTest(TSegmentsTestCases);
  RegisterTest(TFieldsTestCases);
end.
