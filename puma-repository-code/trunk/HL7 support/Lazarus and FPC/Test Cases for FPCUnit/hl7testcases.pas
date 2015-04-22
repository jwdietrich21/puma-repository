unit HL7TestCases;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 test cases }

{ Version 1.7.0 (Hermes) }

{ (c) Johannes W. Dietrich, 1994 - 2015 }
{ (c) Marek Skorupski 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

{ Parser and compiler for HL7 messages }

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
  Classes, SysUtils, fpcunit, testutils, testregistry, DateUtils,
  HL7, MSH, MSA, ERR, OBR, OBX, ORC, SPM, NTE, EVN, PID, PV1, PV2, NK1,
  BLG, BPO, FT1, MLLP;

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
  EXAMPLE_SEGMENT8 = 'NTE|||This is a test report for PUMA HL7 units';
  EXAMPLE_SEGMENT9 =
    'NK1||DOE^DONNA^^^^|MTH||(299)123-4567||CN|||||||||||||||||||||||||||';
  EXAMPLE_SEGMENT10 =
    'MSH|^~\&|SPINA Thyr|RUB|medico|BMH|201311302157||PRF^R04|12345|P|2.5|||||276|ASCII|';
  EXAMPLE_MSA_SEGMENT = 'MSA|AA|CDB22222|P|';
  EXAMPLE_ERR_SEGMENT = 'ERR| |PID^1^11^^9|103|E';
  EXAMPLE_EVN_SEGMENT = 'EVN||200605290901||||200605290900';
  EXAMPLE_NTE_SEGMENT =
    'NTE|1||Blood in tube was clotted, resulting in a rejection of the specimen and leaving the lab unable to perform this test. Please resubmit a new specimen, if test is still desired.|';
  EXAMPLE_OBR_SEGMENT =
    'OBR|1|43215^OE|98765^EKG|93000^EKG REPORT|||198801111330|||1235^TAYLOR^ROBERT^M||||198801111330||P030||||||198801120930||||||P011^PRESLEY^ELVIS^AARON^^^MD|43214^OE|';
  EXAMPLE_OBR_SEGMENT2 =
    'OBR|1|15810^H_Dx_2_0|16699480030^MB|123^Erythrocyte sedimentation rate^L|||20110331150551-0800|||||||||^Smith^John||15810||008847||20110615102200|||F||||OBX|1|ST|30341-2^Erythrocyte sedimentation rate^LN||test not performed||||||X|||20110331140551-0800||33445566^Levin^Henry^^^^^^&2.16.840.1.113883.3.72.5.30.1&ISO^L^^^EN|||20110331150551-0800||||Century Hospital^^^^^&2.16.840.1.113883.3.72.5.30.1&ISO^XX^^^987|2070 Test Park^^Los Angeles^CA^90067^^B|2343242^Knowsalot^Phil^J.^III^Dr.^^^&2.16.840.1.113883.3.72.5.30.1&ISO^L^^^DN‘';
  EXAMPLE_ORC_SEGMENT = 'ORC|PA|A226677^PC|89-458^EKG';
  EXAMPLE_SPM_SEGMENT =
    'SPM|1|||119297000^BLD^SCT^BldSpc^Blood^99USA^^^Blood Specimen|||||||||||||20110103143428||||RC^Clotting^HL70490^CLT^Clotted^99USA^^^Blood clotted in tube|||CLOT^Clotted^HL70493^CLT^Clotted^99USA^^^clotted blood';
  EXAMPLE_PV2_SEGMENT =
    'PV2|||0101^vollstationaer, Normalfall^GSG0001||||||20050405|4||||||||||||||||||||||||||N|N';
  EXAMPLE_FT1_SEGMENT =
    'FT1|1|||19950715|19950716|CG|B1238^BIOPSY-SKIN^SYSTEMA|||1|||ONC|A357||||||P8765^KILDARE^BEN';
  EXAMPLE_FIELD1   = '0493575^^^2^ID 1';
  EXAMPLE_FIELD2   = '168 ~219~C~PMA^^^^^^^^^';
  EXAMPLE_FIELD3   = 'DOE^JOHN^^^^';
  EXAMPLE_FIELD4   = '254 MYSTREET AVE^^MYTOWN^OH^44123^USA';
  EXAMPLE_FIELD5   = 'BID&Twice a day at institution specified times&HL7xxx^^^^12^h^Y|';
  EXAMOLE_FIELD6   = '13.5&18^M~12.0 & 16^F';
  EXAMPLE_MESSAGE1 = EXAMPLE_SEGMENT1 + SEGMENT_DELIMITER + EXAMPLE_SEGMENT2 +
    SEGMENT_DELIMITER + EXAMPLE_SEGMENT3 + SEGMENT_DELIMITER + EXAMPLE_SEGMENT4;
  EXAMPLE_MESSAGE2 = EXAMPLE_SEGMENT1 + SEGMENT_DELIMITER + EXAMPLE_SEGMENT2 +
    SEGMENT_DELIMITER + EXAMPLE_SEGMENT4;
  EXAMPLE_MESSAGE3 = EXAMPLE_MESSAGE1 + SEGMENT_DELIMITER + EXAMPLE_SEGMENT8;
  EXAMPLE_MESSAGE4 = EXAMPLE_SEGMENT1 + SEGMENT_DELIMITER + EXAMPLE_SEGMENT2 +
    SEGMENT_DELIMITER + EXAMPLE_SEGMENT9 + SEGMENT_DELIMITER + EXAMPLE_SEGMENT4;
  EXAMPLE_MESSAGE5 = EXAMPLE_SEGMENT1 + SEGMENT_DELIMITER + EXAMPLE_SEGMENT2 +
    SEGMENT_DELIMITER + EXAMPLE_OBR_SEGMENT2 + SEGMENT_DELIMITER +
    EXAMPLE_NTE_SEGMENT + SEGMENT_DELIMITER + EXAMPLE_SPM_SEGMENT;
  EXAMPLE_MESSAGE6 = EXAMPLE_MESSAGE4 + SEGMENT_DELIMITER + EXAMPLE_PV2_SEGMENT;
  EXAMPLE_MESSAGE7 = EXAMPLE_MESSAGE4 + SEGMENT_DELIMITER + EXAMPLE_ORC_SEGMENT;
  EXAMPLE_MESSAGE8 = EXAMPLE_SEGMENT1 + SEGMENT_DELIMITER + EXAMPLE_EVN_SEGMENT
    + SEGMENT_DELIMITER + EXAMPLE_SEGMENT2 + SEGMENT_DELIMITER +
    EXAMPLE_FT1_SEGMENT;

type

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
  end;

  { TMSHTestCases }

  TMSHTestCases = class(TTestCase)
  published
    procedure MSHGetCase1;
    procedure MSHGetCase2;
    procedure MSHSetCase1;
    procedure MSHSetCase2;
    procedure MSHSetCase3;
  end;

  { TMSATestCases }

  TMSATestCases = class(TTestCase)
  published
    procedure MSASetCase1;
  end;

  { TERRTestCases }

  TERRTestCases = class(TTestCase)
  published
    procedure ERRSetCase1;
  end;

  { TOBRTestCases }

  TOBRTestCases = class(TTestCase)
  published
    procedure OBRSetCase1;
  end;

  { TOBXTestCases }

  TOBXTestCases = class(TTestCase)
  published
    procedure OBXSetCase1;
  end;

  { TORCTestCases }

  TORCTestCases = class(TTestCase)
  published
    procedure ORCSetCase1;
  end;

  { TFT1TestCases }

  TFT1TestCases = class(TTestCase)
  published
    procedure FT1SetCase1;
  end;

  { TSPMTestCases }

  TSPMTestCases = class(TTestCase)
  published
    procedure SPMSetCase1;
  end;

  { TEVNTestCases }

  TEVNTestCase = class(TTestCase)
  published
    procedure EVNTestCase1;
  end;

  { TNTETestCases }

  TNTETestCases = class(TTestCase)
  published
    procedure NTESetCase1;
  end;

  { tPIDTestCases }

  TPIDTestCases = class(TTestCase)
  published
    procedure TPIDTestCase1;
  end;

  { tPV1TestCases }

  TPV1TestCases = class(TTestCase)
  published
    procedure TPV1TestCase1;
  end;

  { tPV2TestCases }

  TPV2TestCases = class(TTestCase)
  published
    procedure TPV2TestCase1;
  end;

  { tNK1TestCases }

  TNK1TestCases = class(TTestCase)
  published
    procedure TNK1TestCase1;
  end;

  { TMessageTestCases }

  TMessageTestCases = class(TTestCase)
  published
    procedure VersionTestCase1;
    procedure WholeMessageParseTestCase1;
    procedure WholeMessageCompileTestCase1;
    procedure WholeMessageFindTestCase1;
    procedure WholeMessageFindTestCase2;
    procedure WholeMessageFindTestCase3;
    procedure WholeMessageDeleteTestCase;
    procedure WholeMessageReplaceTestCase1;
    procedure WholeMessageReplaceTestCase2;
  end;

  { TStringEncodingTestCases }

  TStringEncodingTestCases = class(TTestCase)
  published
    procedure DelimiterParseTestCase1;
    procedure DelimiterParseTestCase2;
    procedure DelimiterCompileTestCase1;
    procedure DelimiterCompileTestCase2;
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

  { TSubComponentTestCases }

  TSubComponentTestCases = class(TTestCase)
  published
    procedure SubComponentParseTestCase1;
    procedure SubComponentParseTestCase2;
    procedure SubComponentParseTestCase3;
  end;

  {TEncodingTestCases}

  TEncodingTestCases = class(TTestCase)
  published
    procedure EncodeDateTimeTestCase;
    procedure DecodeDateTimeTestCase;
  end;

  {TMLLPTestCases}

  TMLLPTestCases = class(TTestCase)
  published
    procedure BlockToMessageTestCase;
    procedure MessageToBlockTestCase1;
    procedure MessageToBlockTestCase2;
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

{ TMSHTestCases }

procedure TMSHTestCases.MSHGetCase1;
var
  testSegment: THL7Segment;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    testSegment := MSH_Segment(TestHL7Message);
    AssertEquals('MSH', testSegment.segmentType);
  end;
end;

procedure TMSHTestCases.MSHGetCase2;
var
  testSegment:  THL7Segment;
  delimiters:   str5;
  sendingApp, sendingFac, receivingApp, receivingFac: tHD;
  dateTime:     tDTM;
  security:     str40;
  messageType:  tMSG;
  controlID:    str20;
  processingID: tPT;
  versionID:    tVID;
  sequenceNumber: tNM;
  continuationPointer: str180;
  AccAckType, AppAckType: tID;
  countryCode:  tID;
  charSet:      tID;
  messageLanguage: tCE;
  altCharHandlScheme: tID;
  profileID:    tEI;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    GetMSH(TestHL7Message, delimiters, sendingApp,
      sendingFac, receivingApp, receivingFac, dateTime,
      security, messageType, controlID, processingID,
      versionID, sequenceNumber, continuationPointer,
      AccAckType, AppAckType, countryCode, charSet,
      messageLanguage, altCharHandlScheme, profileID);
    AssertEquals('199912271408', dateTime);
  end;
end;

procedure TMSHTestCases.MSHSetCase1;
var
  testSegment:  THL7Segment;
  delimiters:   str5;
  sendingApp, sendingFac, receivingApp, receivingFac: tHD;
  dateTime:     tDTM;
  security:     str40;
  messageType:  tMSG;
  controlID:    str20;
  processingID: tPT;
  versionID:    tVID;
  sequenceNumber: tNM;
  continuationPointer: str180;
  AccAckType, AppAckType: tID;
  countryCode:  tID;
  charSet:      tID;
  messageLanguage: tCE;
  altCharHandlScheme: tID;
  profileID:    tEI;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    testSegment := THL7Segment.Create(TestHL7Message, '');
    testSegment.contentString := EXAMPLE_SEGMENT10;
    SetMSH(TestHL7Message, testSegment);
    GetMSH(TestHL7Message, delimiters, sendingApp,
      sendingFac, receivingApp, receivingFac, dateTime,
      security, messageType, controlID, processingID,
      versionID, sequenceNumber, continuationPointer,
      AccAckType, AppAckType, countryCode, charSet,
      messageLanguage, altCharHandlScheme, profileID);
    AssertEquals('201311302157', dateTime);
  end;
end;

procedure TMSHTestCases.MSHSetCase2;
var
  testSegment:  THL7Segment;
  delimiters:   str5;
  sendingApp, sendingFac, receivingApp, receivingFac: tHD;
  dateTime:     tDTM;
  security:     str40;
  messageType:  tMSG;
  controlID:    str20;
  processingID: tPT;
  versionID:    tVID;
  sequenceNumber: tNM;
  continuationPointer: str180;
  AccAckType, AppAckType: tID;
  countryCode, countryCode2: tID;
  charSet:      tID;
  messageLanguage: tCE;
  altCharHandlScheme: tID;
  profileID:    tEI;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    delimiters   := STANDARD_DELIMITERS;
    sendingApp   := 'TestApp1';
    sendingFac   := 'Dr. Mabuse';
    receivingApp := 'TestApp2';
    receivingFac := 'Dr. Frankenstein';
    dateTime     := EncodedDateTime(Now);
    messageType  := 'ADT^A04';
    security     := '';
    messageType  := '';
    controlID    := EncodedDateTime(Now) + IntToStr(random(13000));
    processingID := '';
    versionID    := '';
    sequenceNumber := '';
    continuationPointer := '';
    AccAckType   := '';
    AppAckType   := '';
    countryCode  := '276';
    charSet      := '';
    messageLanguage := '';
    altCharHandlScheme := '';
    profileID    := '';
    SetMSH(TestHL7Message, delimiters, sendingApp,
      sendingFac, receivingApp, receivingFac, dateTime,
      security, messageType, controlID, processingID,
      versionID, sequenceNumber, continuationPointer,
      AccAckType, AppAckType, countryCode, charSet,
      messageLanguage, altCharHandlScheme, profileID);
    GetMSH(TestHL7Message, delimiters, sendingApp,
      sendingFac, receivingApp, receivingFac, dateTime,
      security, messageType, controlID, processingID,
      versionID, sequenceNumber, continuationPointer,
      AccAckType, AppAckType, countryCode2, charSet,
      messageLanguage, altCharHandlScheme, profileID);
    AssertEquals('276', countryCode2);
  end;
end;

procedure TMSHTestCases.MSHSetCase3;
var
  testSegment:  THL7Segment;
  delimiters:   str5;
  sendingApp, sendingFac, receivingApp, receivingFac: tHD;
  dateTime:     tDTM;
  security:     str40;
  messageType:  tMSG;
  controlID:    str20;
  processingID: tPT;
  versionID, versionID2: tVID;
  sequenceNumber: tNM;
  continuationPointer: str180;
  AccAckType, AppAckType: tID;
  countryCode:  tID;
  charSet:      tID;
  messageLanguage: tCE;
  altCharHandlScheme: tID;
  profileID:    tEI;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    delimiters  := STANDARD_DELIMITERS;
    sendingApp  := 'TestApp1';
    sendingFac  := 'Dr. Mabuse';
    receivingApp := 'TestApp2';
    receivingFac := 'Dr. Frankenstein';
    messageType := 'ADT^A04';
    controlID   := '';
    processingID := '';
    sequenceNumber := '';
    continuationPointer := '';
    AccAckType  := '';
    AppAckType  := '';
    charSet     := '';
    messageLanguage := '';
    altCharHandlScheme := '';
    profileID   := '';
    countryCode := '276';
    SetMSH(TestHL7Message, delimiters, sendingApp,
      sendingFac, receivingApp, receivingFac,
      security, messageType, processingID,
      sequenceNumber, continuationPointer,
      AccAckType, AppAckType, countryCode, charSet,
      messageLanguage, altCharHandlScheme, profileID);
    GetMSH(TestHL7Message, delimiters, sendingApp,
      sendingFac, receivingApp, receivingFac, dateTime,
      security, messageType, controlID, processingID,
      versionID2, sequenceNumber, continuationPointer,
      AccAckType, AppAckType, countryCode, charSet,
      messageLanguage, altCharHandlScheme, profileID);
    AssertEquals('2.5', versionID2);
  end;
end;

{ TMSATestCases }

procedure TMSATestCases.MSASetCase1;
var
  AckCode, AckCode2: tID;
  controlID:   str20;
  textMessage: str80;
  exSeqNum:    tNM;
  delAckType:  char;
  ErrorCond:   tCE;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    AckCode     := ACKNOWLEDGEMENT_OK;
    controlID   := 'CDB22222';
    textMessage := 'P';
    exSeqNum    := '';
    delAckType  := char(0);
    ErrorCond   := '';
    SetMSA(TestHL7Message, AckCode, controlID,
      textMessage, exSeqNum, delAckType, ErrorCond);
    GetMSA(TestHL7Message, AckCode2, controlID,
      textMessage, exSeqNum, delAckType, ErrorCond);
    AssertEquals(AckCode, AckCode2);
  end;
end;

{ TERRTestCases }

procedure TERRTestCases.ERRSetCase1;
var
  ErrCodeLoc: tELD;
  ErrLoc, ErrLoc2: tERL;
  ErrCode, ErrCode2: tCWE;
  severity:   tID;
  appErrCode: tCWE;
  appErrPar:  str80;
  DiagInfo, UserMessage: ansistring;
  InformPersIndic: tIS;
  OverrideType, OverrideReason: tCWE;
  HelpDeskContact: tXTN;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    ErrCodeLoc  := '';
    ErrLoc      := 'PID^1^11^^9';
    ErrCode     := ERROR_COND_TBL_VAL_NOT_FND;
    severity    := SEV_ERROR;
    appErrCode  := '';
    appErrPar   := '';
    DiagInfo    := '';
    UserMessage := '';
    InformPersIndic := '';
    OverrideType := '';
    OverrideReason := '';
    HelpDeskContact := '';
    SetERR(TestHL7Message, ErrCodeLoc, ErrLoc, ErrCode, severity, appErrCode, appErrPar,
      DiagInfo, UserMessage, InformPersIndic, OverrideType, OverrideReason,
      HelpDeskContact);
    GetERR(TestHL7Message, ErrCodeLoc, ErrLoc2, ErrCode2, severity,
      appErrCode, appErrPar,
      DiagInfo, UserMessage, InformPersIndic, OverrideType, OverrideReason,
      HelpDeskContact);
    AssertEquals(ErrLoc, ErrLoc2);
    AssertEquals(ErrCode, ErrCode2);
  end;
end;

{ TOBRTestCases }

procedure TOBRTestCases.OBRSetCase1;
var
  SetID:    tSI;
  PlacOrdNumb, FillOrdNumb: tEI;
  USI:      tCE;
  Priority: tID;
  ReqDateTime, ObsDateTime, ObsEndDateTime: tDTM;
  ReqDateTime2: tDTM;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    SetID    := '1';
    PlacOrdNumb := '43215^OE';
    FillOrdNumb := '98765^EKG';
    USI      := '93000^EKG REPORT';
    Priority := '';
    ReqDateTime := '198801111330';
    ObsDateTime := '';
    ObsEndDateTime := '';
    SetOBR(TestHL7Message, SetID, PlacOrdNumb, FillOrdNumb, USI,
      Priority, ReqDateTime, ObsDateTime, ObsEndDateTime);
    GetOBR(TestHL7Message, SetID, PlacOrdNumb, FillOrdNumb, USI,
      Priority, ReqDateTime2, ObsDateTime, ObsEndDateTime);
    AssertEquals(ReqDateTime, ReqDateTime2);
  end;
end;

{ TOBXTestCases }

procedure TOBXTestCases.OBXSetCase1;
var
  SetID:     tSI;
  ValueType: tID;
  ObsID:     tCE;
  obsSubID:  tST;
  obsValue, obsValue2: ansistring;
  Units:     tCE;
  RefRange:  tST;
  AbnormFlags: tIS;
  probability: tNM;
  Nature, status: tID;
  RRDate:    tDTM;
  UDAC:      tST;
  ObsDateTime: tDTM;
  prodID:    tCE;
  respObs:   tXCN;
  observMethod: tCE;
  EquipInstID: tEI;
  AnalysisDateTime: tDTM;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    SetID     := '1';
    ValueType := 'ST';
    ObsID     := '8897-1^QRS COMPLEX^LN';
    obsSubID  := '';
    obsValue  := '91';
    Units     := '/MIN';
    RefRange  := '';
    AbnormFlags := '';
    probability := '';
    Nature    := '';
    status    := 'F';
    RRDate    := '';
    UDAC      := '';
    ObsDateTime := '198804011230';
    prodID    := '';
    respObs   := '';
    observMethod := '';
    EquipInstID := '';
    AnalysisDateTime := '';
    SetOBX(TestHL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);
    GetOBX(TestHL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue2, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);
    AssertEquals(obsValue, obsValue2);
  end;
end;

{ TORCTestCases }

procedure TORCTestCases.ORCSetCase1;
const
  TestFillerOrderNumber =
    '89-458^EKG';
var
  ORCRecord: tORC;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE7;
    GetORC(TestHL7Message, ORCRecord);
    AssertEquals(TestFillerOrderNumber, ORCRecord.FillerOrderNumber);
  end;
end;

{ TFT1TestCases }

procedure TFT1TestCases.FT1SetCase1;
const
  TestTransactionCode =
    'B1238^BIOPSY-SKIN^SYSTEMA';
var
  FT1Record: tFT1;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE8;
    GetFT1(TestHL7Message, FT1Record);
    AssertEquals(TestTransactionCode, FT1Record.TransactionCode);
  end;
end;

{ TSPMTestCases }

procedure TSPMTestCases.SPMSetCase1;
const
  TestSpecimenRejectReason =
    'RC^Clotting^HL70490^CLT^Clotted^99USA^^^Blood clotted in tube';
var
  SPMRecord: tSPM;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE5;
    GetSPM(TestHL7Message, SPMRecord);
    AssertEquals(TestSpecimenRejectReason, SPMRecord.SpecimenRejectReason);
  end;
end;


{ TEVNTestCase }

procedure TEVNTestCase.EVNTestCase1;
var
  evtTypeCode: char;
  recDateTime, recDateTime2, plannedDateTime: tDTM;
  reasonCode: tCWE;
  opID: tXCN;
  evtOccurred, evtOccurred2: tDTM;
  evtFacility: tHD;
begin
  TestHL7Message := THL7Message.Create('2.7');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    evtTypeCode := char(0);
    recDateTime := '200605290901';
    plannedDateTime := '';
    reasonCode := '';
    opID := '';
    evtOccurred := '200605290900';
    evtFacility := '';
    SetEVN(TestHL7Message, evtTypeCode, recDateTime, plannedDateTime,
      reasonCode, opID, evtOccurred, evtFacility);
    GetEVN(TestHL7Message, evtTypeCode, recDateTime2, plannedDateTime,
      reasonCode, opID, evtOccurred2, evtFacility);
    AssertEquals(recDateTime, recDateTime2);
    AssertEquals(evtOccurred, evtOccurred2);
  end;
end;


{ TNTETestCases }

procedure TNTETestCases.NTESetCase1;
var
  SetID, SetID2:     tSI;
  CommentSource, CommentSource2: tID;
  comment, comment2: tFT;
  commentType, commentType2: tCE;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    SetID   := '1';
    CommentSource := 'O';
    comment := 'This is a test segment for PUMA HL7 units';
    commentType := 'RE';
    SetNTE(TestHL7Message, SetID, CommentSource, comment,
      commentType);
    GetNTE(TestHL7Message, SetID2, CommentSource2, comment2,
      commentType2);
    AssertEquals(comment, comment2);
  end;
end;

{ TPIDTestCases }

procedure TPIDTestCases.TPIDTestCase1;
const
  TestPatientAccountNumber = '400003403~1129086';
var
  PIDRecord: tPID;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    GetPID(TestHL7Message, PIDRecord);
    AssertEquals(TestPatientAccountNumber, PIDRecord.PatientAccountNumber);
  end;
end;

{ TPV1TestCases }

procedure TPV1TestCases.TPV1TestCase1;
const
  TestAlternateVisitID = '002376853';
var
  PV1Record: tPV1;
begin
  TestHL7Message := THL7Message.Create('2.7');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE4;
    GetPV1(TestHL7Message, PV1Record);
    AssertEquals(TestAlternateVisitID, PV1Record.AlternateVisitID);
  end;
end;

{ TPV2TestCases }

procedure TPV2TestCases.TPV2TestCase1;
const
  TestLOS = '4';
var
  PV2Record: tPV2;
begin
  TestHL7Message := THL7Message.Create('2.7');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE6;
    GetPV2(TestHL7Message, PV2Record);
    AssertEquals(TestLOS, PV2Record.EstLOS);
  end;
end;

{ TNK1TestCases }

procedure TNK1TestCases.TNK1TestCase1;
const
  TestPhoneNumber = '(216)123-4567';
  TestContactRole = 'EC';
var
  NK1Record: tNK1;
begin
  TestHL7Message := THL7Message.Create('2.7');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    GetNK1(TestHL7Message, NK1Record);
    AssertEquals(TestPhoneNumber, NK1Record.PhoneNumber);
    AssertEquals(TestContactRole, NK1Record.ContactRole);
  end;
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
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
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
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
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
          AssertEquals(EXAMPLE_MESSAGE1, LeftStr(messageContent,
            length(EXAMPLE_MESSAGE1)));
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TMessageTestCases.WholeMessageFindTestCase1;
var
  segmentContent: string;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
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
          segmentContent := TestHL7Message.FoundSegment('NK1', '0').contentString;
          AssertEquals(EXAMPLE_SEGMENT3, segmentContent);
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TMessageTestCases.WholeMessageFindTestCase2;
var
  segmentFound: THL7Segment;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
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
          segmentFound := TestHL7Message.FoundSegment('NK1', '0', nil);
          AssertNull(segmentFound);
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TMessageTestCases.WholeMessageFindTestCase3;
var
  segmentContent: string;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
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
          segmentContent := TestHL7Message.FoundSegment('NK1', '0',
            TestHL7Message.FirstSegment.nextSibling).contentString;
          AssertEquals(EXAMPLE_SEGMENT3, segmentContent);
        end;
      end;
    end;
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TMessageTestCases.WholeMessageDeleteTestCase;
var
  messageContent: string;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    TestHL7Message.DeleteSegment('NK1', '0');
    messageContent := TestHL7Message.contentString;
    AssertEquals(EXAMPLE_MESSAGE2, LeftStr(messageContent,
      length(EXAMPLE_MESSAGE2)));
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TMessageTestCases.WholeMessageReplaceTestCase1;
var
  segmentContent: string;
  testSegment:    THL7Segment;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    testSegment := THL7Segment.Create(nil, '');
    testSegment.contentString := EXAMPLE_SEGMENT8;
    TestHL7Message.ReplaceSegment('NTE', '0', testSegment, True);
    segmentContent := TestHL7Message.FirstSegment.nextSibling.
      nextSibling.nextSibling.nextSibling.contentString;
    AssertEquals(EXAMPLE_SEGMENT8, segmentContent);
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TMessageTestCases.WholeMessageReplaceTestCase2;
var
  messageContent: string;
  testSegment:    THL7Segment;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    testSegment := THL7Segment.Create(nil, EXAMPLE_SEGMENT9);
    TestHL7Message.ReplaceSegment('NK1', '0', testSegment, True);
    messageContent := TestHL7Message.contentString;
    AssertEquals(EXAMPLE_MESSAGE4, LeftStr(messageContent,
      length(EXAMPLE_MESSAGE4)));
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

{ TStringEncodingTestCases }

procedure TStringEncodingTestCases.DelimiterParseTestCase1;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.ParseDelimiters('');
    AssertEquals('|', TestHL7Message.Delimiters.FieldSeparator);
    AssertEquals('^', TestHL7Message.Delimiters.ComponentSeparator);
    AssertEquals('~', TestHL7Message.Delimiters.RepetitionSeparator);
    AssertEquals('\', TestHL7Message.Delimiters.EscapeCharacter);
    AssertEquals('&', TestHL7Message.Delimiters.SubcomponentSeparator);
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TStringEncodingTestCases.DelimiterParseTestCase2;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    TestHL7Message.ParseDelimiters('#/@+*');
    AssertEquals('#', TestHL7Message.Delimiters.FieldSeparator);
    AssertEquals('/', TestHL7Message.Delimiters.ComponentSeparator);
    AssertEquals('@', TestHL7Message.Delimiters.RepetitionSeparator);
    AssertEquals('+', TestHL7Message.Delimiters.EscapeCharacter);
    AssertEquals('*', TestHL7Message.Delimiters.SubcomponentSeparator);
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TStringEncodingTestCases.DelimiterCompileTestCase1;
var
  testSequence:   str5;
  testDelimiters: THL7Delimiters;
begin
  TestHL7Message := THL7Message.Create('2.5');
  testDelimiters.SegmentTerminator := char(0);
  testDelimiters.ComponentSeparator := char(0);
  testDelimiters.EscapeCharacter := char(0);
  testDelimiters.FieldSeparator := char(0);
  testDelimiters.RepetitionSeparator := char(0);
  testDelimiters.SubcomponentSeparator := char(0);
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    testSequence := TestHL7Message.CompiledDelimiters(testDelimiters);
    AssertEquals(STANDARD_DELIMITERS, testSequence);
  end;
  if TestHL7Message <> nil then
    TestHL7Message.Destroy;
end;

procedure TStringEncodingTestCases.DelimiterCompileTestCase2;
var
  testSequence:   str5;
  testDelimiters: THL7Delimiters;
begin
  TestHL7Message := THL7Message.Create('2.5');
  testDelimiters.SegmentTerminator := char(13);
  testDelimiters.ComponentSeparator := '/';
  testDelimiters.EscapeCharacter := '+';
  testDelimiters.FieldSeparator := '#';
  testDelimiters.RepetitionSeparator := '@';
  testDelimiters.SubcomponentSeparator := '*';
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    testSequence := TestHL7Message.CompiledDelimiters(testDelimiters);
    AssertEquals('#/@+*', testSequence);
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
    TestHL7Message.ParseDelimiters('');
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
    TestHL7Message.ParseDelimiters('');
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
    TestHL7Message.contentString := EXAMPLE_MESSAGE1;
    if (TestHL7Message.FirstSegment = nil) or
      (TestHL7Message.FirstSegment.nextSibling = nil) or
      (TestHL7Message.FirstSegment.nextSibling.nextSibling = nil) then
      fail('Segment could not be created.')
    else
    begin
      segmentContent := TestHL7Message.FirstSegment.nextSibling.
        nextSibling.contentString;
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
  fieldContent: string;
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

{ TEncodingTestCases }

procedure TEncodingTestCases.EncodeDateTimeTestCase;
var
  theDateTime: TDateTime;
begin
  theDateTime := EncodeDateTime(1994, 12, 27, 21, 13, 0, 0);
  AssertEquals('19941227211300', EncodedDateTime(theDateTime));
end;

procedure TEncodingTestCases.DecodeDateTimeTestCase;
var
  timeRepresentation: string;
  desiredDateTime, theDateTime: TDateTime;
begin
  desiredDateTime := EncodeDateTime(1994, 12, 27, 21, 13, 0, 0);
  timeRepresentation := '19941227211300';
  theDateTime := DecodeDateTime(timeRepresentation);
  AssertEquals(desiredDateTime, theDateTime);
end;

{ TMLLPTestCases }

procedure TMLLPTestCases.BlockToMessageTestCase;
var
  testBlock:     string;
  testMLLPBlock: TMLLP;
begin
  testMLLPBlock := TMLLP.Create;
  if testMLLPBlock = nil then
    fail('MLLP block could not be created.')
  else
  begin
    testBlock := SB + EXAMPLE_MESSAGE1 + EB + ksCR;
    testMLLPBlock.block := testBlock;
    AssertEquals(EXAMPLE_MESSAGE1, LeftStr(testMLLPBlock.message.contentString,
      length(EXAMPLE_MESSAGE1)));
  end;
  if testMLLPBlock <> nil then
    testMLLPBlock.Destroy;
end;

procedure TMLLPTestCases.MessageToBlockTestCase1;
var
  testBlock:     string;
  testMLLPBlock: TMLLP;
begin
  TestHL7Message := THL7Message.Create('2.5');
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    testMLLPBlock := TMLLP.Create;
    if testMLLPBlock = nil then
      fail('MLLP block could not be created.')
    else
    begin
      testBlock := SB + EXAMPLE_MESSAGE2 + EB + ksCR;
      TestHL7Message.contentString := EXAMPLE_MESSAGE2;
      testMLLPBlock.message := TestHL7Message;
      AssertEquals(LeftStr(testBlock, length(EXAMPLE_MESSAGE2)),
        LeftStr(testMLLPBlock.block, length(EXAMPLE_MESSAGE2)));
    end;
  end;
  if testMLLPBlock <> nil then
    testMLLPBlock.Destroy;
end;

procedure TMLLPTestCases.MessageToBlockTestCase2;
var
  testBlock:     string;
  testMLLPBlock: TMLLP;
begin
  if TestHL7Message = nil then
    fail('Message could not be created.')
  else
  begin
    testMLLPBlock := TMLLP.Create;
    if testMLLPBlock = nil then
      fail('MLLP block could not be created.')
    else
    begin
      testBlock := SB + EXAMPLE_MESSAGE2 + EB + ksCR;
      testMLLPBlock.message := EXAMPLE_MESSAGE2;
      AssertEquals(LeftStr(testBlock, length(EXAMPLE_MESSAGE2)),
        LeftStr(testMLLPBlock.block, length(EXAMPLE_MESSAGE2)));
    end;
  end;
  if testMLLPBlock <> nil then
    testMLLPBlock.Destroy;
end;

initialization
  RegisterTest(TControlTestCases);
  RegisterTest(TMSHTestCases);
  RegisterTest(TMSATestCases);
  RegisterTest(TERRTestCases);
  RegisterTest(TOBRTestCases);
  RegisterTest(TOBXTestCases);
  RegisterTest(TORCTestCases);
  RegisterTest(tFT1TestCases);
  RegisterTest(TSPMTestCases);
  RegisterTest(TNTETestCases);
  RegisterTest(TEVNTestCase);
  RegisterTest(TPIDTestCases);
  RegisterTest(TPV1TestCases);
  RegisterTest(TPV2TestCases);
  RegisterTest(TNK1TestCases);
  RegisterTest(TMessageTestCases);
  RegisterTest(TStringEncodingTestCases);
  RegisterTest(TSegmentsTestCases);
  RegisterTest(TFieldsTestCases);
  RegisterTest(TComponentTestCases);
  RegisterTest(TSubComponentTestCases);
  RegisterTest(TEncodingTestCases);
  RegisterTest(TMLLPTestCases);
end.
