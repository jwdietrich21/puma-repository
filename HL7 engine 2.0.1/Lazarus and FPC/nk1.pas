unit NK1;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for next of kin segments }

{ Version 2.0.1 (Hermes) }

 { (c) J. W. Dietrich, 1994 - 2015 }
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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HL7;

const
  NK1_ID = 'NK1';

type
  tNK1 = record
    SetID:     tSI;
    Name:      tXPN;
    Relationship: tCE;
    Address:   tXAD;
    PhoneNumber, BusinessPhoneNumber: tXTN;
    ContactRole: tCE;
    StartDate, EndDate: tDT;
    JobTitle:  tST;
    JobCode:   tJCC;
    EmployeeNumber: tCX;
    OrganizationName: tXON;
    MaritalStatus: tCE;
    AdministrativeSex: tIS;
    DateTimeOfBirth: tTS;
    LivingDependency, AmbulatoryStatus: tIS;
    Citizenship, PrimaryLanguage: tCE;
    LivingArrangement: tIS;
    PublicityCode: tCE;
    ProtectionIndicator: tID;
    StudentIndicator: tIS;
    Religion:  tCE;
    MothersMaidenName: tXPN;
    Nationality, EthnicGroup, ContactReason: tCE;
    ContactPersonsName: tXPN;
    ContactPersonsTelephoneNumber: tXTN;
    ContactPersonsAddress: TXAD;
    Identifiers: tCX;
    JobStatus: tIS;
    Race:      tCE;
    Handicap:  tIS;
    ContactPersonSocialSecurityNumber, BirthPlace: tST;
    VIPIndicator: tIS;
    TelecomInformation, ContactPersonsTelecomInformation: tXTN; // Introduced in HL7 2.7
  end;

function NK1_Segment(message: THL7Message): THL7Segment;
procedure GetNK1(aSegment: THL7Segment; out NK1Record: tNK1);
procedure GetNK1(message: THL7Message; out NK1Record: tNK1);
procedure SetNK1(message: THL7Message; aSegment: THL7Segment);
procedure SetNK1(message: THL7message; NK1Record: tNK1);
procedure ClearNK1(var NK1Record: tNK1);

implementation

function NK1_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(NK1_ID, '0')
  else
    Result := nil;
end;

procedure GetNK1(aSegment: THL7Segment; out NK1Record: tNK1);
var
  nextField:      THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  if (aSegment <> nil) and (aSegment.segmentType = 'NK1') then
  begin
    nextOccurrence := aSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with NK1Record do
      begin
        nextField := aSegment.FirstOccurrence.FirstField.nextSibling;
        SetID     := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Name      := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Relationship := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Address   := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PhoneNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BusinessPhoneNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactRole := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        StartDate := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EndDate   := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        JobTitle  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        JobCode   := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EmployeeNumber := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrganizationName := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MaritalStatus := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdministrativeSex := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DateTimeOfBirth := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        LivingDependency := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AmbulatoryStatus := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Citizenship := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PrimaryLanguage := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        LivingArrangement := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PublicityCode := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ProtectionIndicator := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        StudentIndicator := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Religion  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MothersMaidenName := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Nationality := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EthnicGroup := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactReason := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactPersonsName := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactPersonsTelephoneNumber :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactPersonsAddress :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Identifiers := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        JobStatus := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Race      := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Handicap  := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactPersonSocialSecurityNumber :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BirthPlace := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VIPIndicator := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        TelecomInformation := aSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactPersonsTelecomInformation :=
          aSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end
  else
    ClearNK1(NK1Record);
end;

procedure GetNK1(message: THL7Message; out NK1Record: tNK1);
var
  curSegment: THL7Segment;
begin
  curSegment := NK1_Segment(message);
  GetNK1(curSegment, NK1Record);
end;

procedure SetNK1(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetNK1(message: THL7message; NK1Record: tNK1);
var
  newSegment: THL7Segment;
  FieldSep:   char;
  theString:  ansistring;
begin
  FieldSep   := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with NK1Record do
    theString := NK1_ID + FieldSep + SetID + FieldSep + Name +
      FieldSep + Relationship + FieldSep + Address + FieldSep +
      PhoneNumber + FieldSep + BusinessPhoneNumber + FieldSep +
      ContactRole + FieldSep + StartDate + FieldSep + EndDate +
      FieldSep + JobTitle + FieldSep + JobCode + FieldSep + EmployeeNumber +
      FieldSep + OrganizationName + FieldSep + MaritalStatus + FieldSep +
      AdministrativeSex + FieldSep + DateTimeOfBirth + FieldSep +
      LivingDependency + FieldSep + AmbulatoryStatus + FieldSep +
      Citizenship + FieldSep + PrimaryLanguage + FieldSep + LivingArrangement +
      FieldSep + PublicityCode + FieldSep + ProtectionIndicator + FieldSep +
      StudentIndicator + FieldSep + Religion + FieldSep + MothersMaidenName +
      FieldSep + Nationality + FieldSep + EthnicGroup + FieldSep +
      ContactReason + FieldSep + ContactPersonsName + FieldSep +
      ContactPersonsTelephoneNumber + FieldSep + ContactPersonsAddress +
      FieldSep + Identifiers + FieldSep + JobStatus + FieldSep +
      Race + FieldSep + Handicap + FieldSep + ContactPersonSocialSecurityNumber +
      FieldSep + BirthPlace + FieldSep + VIPIndicator + FieldSep +
      TelecomInformation + FieldSep + ContactPersonsTelecomInformation + FieldSep;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

procedure ClearNK1(var NK1Record: tNK1);
begin
  FillChar(NK1Record, SizeOf(NK1Record), 0);
end;

end.
