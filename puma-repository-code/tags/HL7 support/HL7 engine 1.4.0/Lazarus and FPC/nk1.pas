unit NK1;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for next of kin segments }

{ Version 1.4 }

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HL7;

const
  NK1_ID = 'NK1';

type
  tNK1 = record
    SetID: tSI;
    Name: tXPN;
    Relationship: tCE;
    Address: tXAD;
    PhoneNumber, BusinessPhoneNumber: tXTN;
    ContactRole: tCE;
    StartDate, EndDate: tDT;
    JobTitle: tST;
    JobCode: tJCC;
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
    Religion: tCE;
    MothersMaidenName: tXPN;
    Nationality, EthnicGroup, ContactReason: tCE;
    ContactPersonsName: tXPN;
    ContactPersonsTelephoneNumber: tXTN;
    ContactPersonsAddress: TXAD;
    Identifiers: tCX;
    JobStatus: tIS;
    Race: tCE;
    Handicap: tIS;
    ContactPersonSocialSecurityNumber, BirthPlace: tST;
    VIPIndicator: tIS;
  end;

function NK1_Segment(message: THL7Message): THL7Segment;
procedure GetNK1(message: THL7Message; out NK1Record: tNK1);
procedure SetNK1(message: THL7Message; aSegment: THL7Segment);
procedure SetNK1(message: THL7message; NK1Record: tNK1);

implementation

function NK1_Segment(message: THL7Message): THL7Segment;
begin
  if message <> nil then
    Result := message.FoundSegment(NK1_ID, '0')
  else
    Result := nil;
end;

procedure GetNK1(message: THL7Message; out NK1Record: tNK1);
var
  curSegment: THL7Segment;
  nextField: THL7Field;
  nextOccurrence: THL7Occurrence;
begin
  curSegment := NK1_Segment(message);
  if curSegment <> nil then
  begin
    nextOccurrence := curSegment.FirstOccurrence;
    if nextOccurrence <> nil then
      with NK1Record do
      begin
        nextField := curSegment.FirstOccurrence.FirstField.nextSibling;
        SetID := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Name := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Relationship := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Address := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PhoneNumber := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BusinessPhoneNumber := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactRole := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        StartDate := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EndDate := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        JobTitle := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        JobCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EmployeeNumber := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        OrganizationName := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MaritalStatus := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AdministrativeSex := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        DateTimeOfBirth := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        LivingDependency := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        AmbulatoryStatus := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Citizenship := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PrimaryLanguage := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        LivingArrangement := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        PublicityCode := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ProtectionIndicator := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        StudentIndicator := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Religion := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        MothersMaidenName := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Nationality := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        EthnicGroup := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactReason := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactPersonsName := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactPersonsTelephoneNumber :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactPersonsAddress :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Identifiers := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        JobStatus := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Race := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        Handicap := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        ContactPersonSocialSecurityNumber :=
          curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        BirthPlace := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
        VIPIndicator := curSegment.FirstOccurrence.GetNextFieldContent(nextField);
      end;
  end;
end;

procedure SetNK1(message: THL7Message; aSegment: THL7Segment);
begin
  message.AddSegment(aSegment);
end;

procedure SetNK1(message: THL7message; NK1Record: tNK1);
var
  newSegment: THL7Segment;
  FieldSep: char;
  theString: ansistring;
begin
  FieldSep := message.Delimiters.FieldSeparator;
  newSegment := THL7Segment.Create(message, '');
  with NK1Record do
    theString := NK1_ID + FieldSep + SetID + FieldSep + Name +
      FieldSep + Relationship + FieldSep + Address + FieldSep + PhoneNumber +
      FieldSep + BusinessPhoneNumber + FieldSep + ContactRole + FieldSep +
      StartDate + FieldSep + EndDate + FieldSep + JobTitle + FieldSep +
      JobCode + FieldSep + EmployeeNumber + FieldSep + OrganizationName +
      FieldSep + MaritalStatus + FieldSep + AdministrativeSex + FieldSep +
      DateTimeOfBirth + FieldSep + LivingDependency + FieldSep + AmbulatoryStatus +
      FieldSep + Citizenship + FieldSep + PrimaryLanguage + FieldSep +
      LivingArrangement + FieldSep + PublicityCode + FieldSep + ProtectionIndicator +
      FieldSep + StudentIndicator + FieldSep + Religion + FieldSep +
      MothersMaidenName + FieldSep + Nationality + FieldSep + EthnicGroup +
      FieldSep + ContactReason + FieldSep + ContactPersonsName + FieldSep +
      ContactPersonsTelephoneNumber + FieldSep + ContactPersonsAddress +
      FieldSep + Identifiers + FieldSep + JobStatus + FieldSep + Race +
      FieldSep + Handicap + FieldSep + ContactPersonSocialSecurityNumber +
      FieldSep + BirthPlace + FieldSep + VIPIndicator;
  newSegment.contentString := theString;
  message.AddSegment(newSegment);
end;

end.
