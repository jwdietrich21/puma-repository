unit MetronTestCases;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Test cases for Metron }

{ Version 1.0.0 (Atlas) }

{ (c) Johannes W. Dietrich, 1994 - 2026 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2026 }

{ Handler for measurements and readings }

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
  Classes, SysUtils, fpcunit, testutils, testregistry, Metron;

type

  { TControlTestCases }

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
    procedure CodeVersionCheck;
  end;

  { ReadingsTestCases }

  ReadingsTestCases = class(TTestCase)
  published
    procedure BasicRecordTest;
    procedure SummationTest;
    procedure SubtractionTest;
    procedure MultiplicationTest;
    procedure DivisionTest;
  end;

implementation

{ TControlTestCases }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertNull('This test is bound to succeed', nil);
end;

procedure TControlTestCases.CodeVersionCheck;
{ The subsequent tests are compatible with Metron version 1.0 }
begin
  Assertequals(1, Metron_major);
  Assertequals(0, Metron_minor);
end;

procedure ReadingsTestCases.BasicRecordTest;
var
  a: TReading;
begin
  a.Value := 13;
  a.flag := equal;
  Assertequals('13', a.Text);
  a.Value := -3.14;
  Assertequals('-3', LeftStr(a.Text, 2));  // skips decimal separator, which
  Assertequals('14', RightStr(a.Text, 2)); // depends on locale
  a.Text := '7';
  Assertequals(7, a.Value);
  a.Text := '-231';
  Assertequals(-231, a.Value);
  a.Text := '<2';
  Assertequals(2, a.Value);
  AssertTrue(a.flag = below);
  Assertequals(2, a.Value);
  a.Text := '>100';
  Assertequals(100, a.Value);
  AssertTrue(a.flag = above);
  Assertequals(100, a.Value);
end;

procedure ReadingsTestCases.SummationTest;
{ Correct results:
+ B |  10 | <10 | >10 |  -10 | <-10 | >-10 |
A   |     |     |     |      |      |      |
--------------------------------------------
  2 |  12 | <12 | >12 |   -8 |  <-8 |  >-8 |
 <2 | <12 | <12 | NaN |  <-8 |  <-8 |  NaN |
 >2 | >12 | NaN | >12 |  >-8 |  NaN |  >-8 |
 -2 |   8 |  <8 |  >8 |  -12 | <-12 | >-12 |
<-2 |  <8 |  <8 | NaN | <-12 | <-12 |  NaN |
>-2 |  >8 | NaN |  >8 | >-12 |  NaN | >-12 |
}
var
  a, b, c: TReading;
begin
  a.Value := 2;
  b.Value := 10;
  c := a + b;
  Assertequals(12, c.Value);
  a.Value := 2;
  a.flag := below;
  c := a + b;
  Assertequals('<', c.Text[1]);
  Assertequals('12', RightStr(c.Text, 2));
  a.Text := '<10';
  c := a + b;
  Assertequals('<', c.Text[1]);
  Assertequals('20', RightStr(c.Text, 2));
  a.Text := '>2';
  c := a + b;
  Assertequals('>', c.Text[1]);
  Assertequals('12', RightStr(c.Text, 2));
  a.Text := '-2';
  c := a + b;
  Assertequals(8, c.Value);
  a.Text := '<-2';
  c := a + b;
  Assertequals('<', c.Text[1]);
  Assertequals('8', RightStr(c.Text, 1));
  a.Text := '>-2';
  c := a + b;
  Assertequals('>', c.Text[1]);
  Assertequals('8', RightStr(c.Text, 1));
  Assertequals(8, c.Value);
  AssertTrue(c.flag = above);

  b.Text := '<10';
  a.flag := equal;
  a.Value := 2;
  c := a + b;
  AssertTrue(c.flag = below);
  Assertequals(12, c.Value);
  a.flag := below;
  a.Value := 2;
  c := a + b;
  AssertTrue(c.flag = below);
  Assertequals(12, c.Value);
  a.flag := above;
  a.Value := 2;
  c := a + b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
  a.flag := equal;
  a.Value := -2;
  c := a + b;
  AssertTrue(c.flag = below);
  Assertequals(8, c.Value);
  a.flag := below;
  a.Value := -2;
  c := a + b;
  AssertTrue(c.flag = below);
  Assertequals(8, c.Value);
  a.flag := above;
  a.Value := -2;
  c := a + b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));

  b.Text := '>10';
  a.flag := equal;
  a.Value := 2;
  c := a + b;
  AssertTrue(c.flag = above);
  Assertequals(12, c.Value);
  a.flag := below;
  a.Value := 2;
  c := a + b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
  a.flag := above;
  a.Value := 2;
  c := a + b;
  AssertTrue(c.flag = above);
  Assertequals(12, c.Value);
  a.flag := equal;
  a.Value := -2;
  c := a + b;
  AssertTrue(c.flag = above);
  Assertequals(8, c.Value);
  a.flag := below;
  a.Value := -2;
  c := a + b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
  a.flag := above;
  a.Value := -2;
  c := a + b;
  AssertTrue(c.flag = above);
  Assertequals(8, c.Value);

  b.Text := '-10';
  a.flag := equal;
  a.Value := 2;
  c := a + b;
  AssertTrue(c.flag = equal);
  Assertequals(-8, c.Value);
  a.flag := below;
  a.Value := 2;
  c := a + b;
  AssertTrue(c.flag = below);
  Assertequals(-8, c.Value);
  a.flag := above;
  a.Value := 2;
  c := a + b;
  AssertTrue(c.flag = above);
  Assertequals(-8, c.Value);
  a.flag := equal;
  a.Value := -2;
  c := a + b;
  AssertTrue(c.flag = equal);
  Assertequals(-12, c.Value);
  a.flag := below;
  a.Value := -2;
  c := a + b;
  AssertTrue(c.flag = below);
  Assertequals(-12, c.Value);
  a.flag := above;
  a.Value := -2;
  c := a + b;
  AssertTrue(c.flag = above);
  Assertequals(-12, c.Value);

  b.Text := '<-10';
  a.flag := equal;
  a.Value := 2;
  c := a + b;
  AssertTrue(c.flag = below);
  Assertequals(-8, c.Value);
  a.flag := below;
  a.Value := 2;
  c := a + b;
  AssertTrue(c.flag = below);
  Assertequals(-8, c.Value);
  a.flag := above;
  a.Value := 2;
  c := a + b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
  a.flag := equal;
  a.Value := -2;
  c := a + b;
  AssertTrue(c.flag = below);
  Assertequals(-12, c.Value);
  a.flag := below;
  a.Value := -2;
  c := a + b;
  AssertTrue(c.flag = below);
  Assertequals(-12, c.Value);
  a.flag := above;
  a.Value := -2;
  c := a + b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));

  b.Text := '>-10';
  a.flag := equal;
  a.Value := 2;
  c := a + b;
  AssertTrue(c.flag = above);
  Assertequals(-8, c.Value);
  a.flag := below;
  a.Value := 2;
  c := a + b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
  a.flag := above;
  a.Value := 2;
  c := a + b;
  AssertTrue(c.flag = above);
  Assertequals(-8, c.Value);
  a.flag := equal;
  a.Value := -2;
  c := a + b;
  AssertTrue(c.flag = above);
  Assertequals(-12, c.Value);
  a.flag := below;
  a.Value := -2;
  c := a + b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
  a.flag := above;
  a.Value := -2;
  c := a + b;
  AssertTrue(c.flag = above);
  Assertequals(-12, c.Value);
end;

procedure ReadingsTestCases.SubtractionTest;
{ Correct results:
.  B |    2 |   <2 |   >2 |   -2 | <-2 | >-2 |
A    |      |      |      |      |     |     |
----------------------------------------------
  10 |    8 |   >8 |   <8 |   12 | >12 | <12 |
 <10 |   <8 |  NaN |   <8 |  <12 | NaN | <12 |
 >10 |   >8 |   >8 |  NaN |  >12 | >12 | NaN |
 -10 |  -12 | >-12 | <-12 |   -8 | >-8 | <-8 |
<-10 | <-12 |  NaN | <-12 |  <-8 | NaN | <-9 |
>-10 | >-12 | >-12 |  NaN |  >-8 | >-8 | NaN |
}
var
  a, b, c: TReading;
begin
  b.flag := equal;
  b.Value := 2;
  a.flag := equal;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = equal);
  Assertequals(8, c.Value);
  a.flag := below;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = below);
  Assertequals(8, c.Value);
  a.flag := above;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = above);
  Assertequals(8, c.Value);
  a.flag := equal;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = equal);
  Assertequals(-12, c.Value);
  a.flag := below;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = below);
  Assertequals(-12, c.Value);
  a.flag := above;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = above);
  Assertequals(-12, c.Value);

  b.flag := below;
  b.Value := 2;
  a.flag := equal;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = above);
  Assertequals(8, c.Value);
  a.flag := below;
  a.Value := 10;
  c := a - b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
  a.flag := above;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = above);
  Assertequals(8, c.Value);
  a.flag := equal;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = above);
  Assertequals(-12, c.Value);
  a.flag := below;
  a.Value := -10;
  c := a - b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
  a.flag := above;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = above);
  Assertequals(-12, c.Value);

  b.flag := above;
  b.Value := 2;
  a.flag := equal;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = below);
  Assertequals(8, c.Value);
  a.flag := below;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = below);
  Assertequals(8, c.Value);
  a.flag := above;
  a.Value := 10;
  c := a - b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
  a.flag := equal;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = below);
  Assertequals(-12, c.Value);
  a.flag := below;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = below);
  Assertequals(-12, c.Value);
  a.flag := above;
  a.Value := -10;
  c := a - b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));

  b.flag := equal;
  b.Value := -2;
  a.flag := equal;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = equal);
  Assertequals(12, c.Value);
  a.flag := below;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = below);
  Assertequals(12, c.Value);
  a.flag := above;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = above);
  Assertequals(12, c.Value);
  a.flag := equal;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = equal);
  Assertequals(-8, c.Value);
  a.flag := below;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = below);
  Assertequals(-8, c.Value);
  a.flag := above;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = above);
  Assertequals(-8, c.Value);

  b.flag := below;
  b.Value := -2;
  a.flag := equal;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = above);
  Assertequals(12, c.Value);
  a.flag := below;
  a.Value := 10;
  c := a - b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
  a.flag := above;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = above);
  Assertequals(12, c.Value);
  a.flag := equal;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = above);
  Assertequals(-8, c.Value);
  a.flag := below;
  a.Value := -10;
  c := a - b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
  a.flag := above;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = above);
  Assertequals(-8, c.Value);

  b.flag := above;
  b.Value := -2;
  a.flag := equal;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = below);
  Assertequals(12, c.Value);
  a.flag := below;
  a.Value := 10;
  c := a - b;
  AssertTrue(c.flag = below);
  Assertequals(12, c.Value);
  a.flag := above;
  a.Value := 10;
  c := a - b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
  a.flag := equal;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = below);
  Assertequals(-8, c.Value);
  a.flag := below;
  a.Value := -10;
  c := a - b;
  AssertTrue(c.flag = below);
  Assertequals(-8, c.Value);
  a.flag := above;
  a.Value := -10;
  c := a - b;
  Assertequals(LowerCase('NaN'), LowerCase(c.Text));
end;

procedure ReadingsTestCases.MultiplicationTest;
var
  a, b, c: TReading;
begin
  b.flag := equal;
  b.Value := 2;
  a.flag := equal;
  a.Value := 10;
  c := a * b;
  AssertTrue(c.flag = equal);
  Assertequals(20, c.Value);
  a.flag := below;
  a.Value := 10;
  c := a * b;
  AssertTrue(c.flag = below);
  Assertequals(20, c.Value);
  a.flag := above;
  a.Value := 10;
  c := a * b;
  AssertTrue(c.flag = above);
  Assertequals(20, c.Value);
  a.flag := equal;
  a.Value := -10;
  c := a * b;
  AssertTrue(c.flag = equal);
  Assertequals(-20, c.Value);
  a.flag := below;
  a.Value := -10;
  c := a * b;
  AssertTrue(c.flag = below);
  Assertequals(-20, c.Value);
  a.flag := above;
  a.Value := -10;
  c := a * b;
  AssertTrue(c.flag = above);
  Assertequals(-20, c.Value);

end;

procedure ReadingsTestCases.DivisionTest;
var
  a, b, c: TReading;
begin
  b.flag := equal;
  b.Value := 2;
  a.flag := equal;
  a.Value := 10;
  c := a / b;
  AssertTrue(c.flag = equal);
  Assertequals(5, c.Value);
  a.flag := below;
  a.Value := 10;
  c := a / b;
  AssertTrue(c.flag = below);
  Assertequals(5, c.Value);
  a.flag := above;
  a.Value := 10;
  c := a / b;
  AssertTrue(c.flag = above);
  Assertequals(5, c.Value);

  a.Text := '4';
  b.Text := '2';
  c := a / b;
  AssertTrue(c.flag = equal);
  Assertequals(2, c.Value);

  a.Text := '-4';
  b.Text := '2';
  c := a / b;
  AssertTrue(c.flag = equal);
  Assertequals(-2, c.Value);

  a.Text := '<4';
  b.Text := '2';
  c := a / b;
  AssertTrue(c.flag = below);
  Assertequals(2, c.Value);

  a.Text := '<-4';
  b.Text := '2';
  c := a / b;
  AssertTrue(c.flag = below);
  Assertequals(-2, c.Value);

  a.Text := '>4';
  b.Text := '2';
  c := a / b;
  AssertTrue(c.flag = above);
  Assertequals(2, c.Value);

  a.Text := '>-4';
  b.Text := '2';
  c := a / b;
  AssertTrue(c.flag = above);
  Assertequals(-2, c.Value);

end;


initialization

  RegisterTest(TControlTestCases);
  RegisterTest(ReadingsTestCases);
end.

