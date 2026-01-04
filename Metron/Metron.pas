unit Metron;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Metron base unit }

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
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, Math;

const
  Metron_major   = 1;
  Metron_minor   = 0;
  Metron_release = 0;
  Metron_patch   = 0;
  Metron_fullversion = ((Metron_major * 100 + Metron_minor) *
    100 + Metron_release) * 100 + Metron_patch;
  Metron_version = '1.0.0.0';
  Metron_internalversion = 'Atlas';

  {NaN = 0 / 0;
  Infinity = 1 / 0;}

type

  TFlag = (equal, below, above);

  { TReading }

  TReading = record
  private
    FText: string;
    Fvalue: double;
    FFlag: TFlag;
    procedure SetValue(value: double);
    procedure SetText(text: string);
    procedure SetFlag(flag: TFlag);
  public
    property flag: TFlag read FFlag write SetFlag;
    property Text: string read FText write SetText;
    property Value: double read FValue write SetValue;
  end;

  operator + (const a: TReading; const b: TReading): TReading;
  operator - (const a: TReading; const b: TReading): TReading;
  operator * (const a: TReading; const b: TReading): TReading;
  operator / (const a: TReading; const b: TReading): TReading;

implementation

{ TReading }

procedure TReading.SetValue(value: double);
begin
  FValue := value;
  case Flag of
  below: FText := '< ' + FloatToStr(value);
  equal: FText := FloatToStr(value);
  above: FText := '> ' + FloatToStr(value);
  otherwise
    Flag := equal; // FText will automatically be assigned by SetFlag
  end;
end;

procedure TReading.SetText(text: string);
begin
  FText := text;
  if LeftStr(text, 1) = '<' then
  begin
    FFlag := below;
    FValue := StrToFloat(RightStr(text, length(text) - 1));
  end
  else if LeftStr(text, 1) = '>' then
  begin
    FFlag := above;
    FValue := StrToFloat(RightStr(text, length(text) - 1));
  end
  else
  begin
    FFlag := equal;
    FValue := StrToFloat(text);
  end;
end;

procedure TReading.SetFlag(flag: TFlag);
begin
  FFlag := flag;
  SetValue(FValue);  // refresh after changing flag
end;

function sum(a, b: TReading): TReading;
var
  num1, num2: double;
begin
  num1 := a.Value;
  num2 := b.Value;
  result.Value := num1 + num2;
  result.flag := equal;
  if (a.flag = below) and (b.flag = above) or (a.flag = above) and (b.flag = below) then
    begin
      result.Value := NaN;
      result.flag := equal;
    end
  else if (a.flag = below) or (b.flag = below) then
    result.flag := below
  else if (a.flag = above) or (b.flag = above) then
    result.flag := above;
end;

function diff(a, b: TReading): TReading;
var
  num1, num2: double;
begin
  num1 := a.Value;
  num2 := b.Value;
  result.Value := num1 - num2;
  result.flag := equal;
  if (a.flag = below) and (b.flag = below) or (a.flag = above) and (b.flag = above) then
    begin
      result.Value := NaN;
      result.flag := equal;
    end
  else if a.flag = equal then
    begin
      if b.flag = below then
        result.flag := above
      else if b.flag = above then
        result.flag := below;
    end
  else result.flag := a.flag;
end;

function mul(a, b: TReading): TReading;
var
  num1, num2: double;
begin
  num1 := a.Value;
  num2 := b.Value;
  result.Value := num1 * num2;
  result.flag := equal;
  if sign(num1) = sign(num2) then
    begin
      if (a.flag = below) and (b.flag = above) or (a.flag = above) and (b.flag = below) then
        result.Value := NaN
      else if (a.flag = below) or (b.flag = below) then
        begin
          if num1 > 0 then
            result.flag := below
          else
            result.flag := above;
        end
      else if (a.flag = above) or (b.flag = above) then
        begin
          if num1 > 0 then
            result.flag := above
          else
            result.flag := below;
        end;
    end
  else
    begin
      if (a.flag = below) and (b.flag = below) or (a.flag = above) and (b.flag = above) then
        result.Value := NaN
      else
      begin
        if num2 > 0 then
          begin
            result.flag := a.flag;
            if b.flag = below then
              result.flag := above
            else if b.flag = above then
              result.flag := below;
          end
        else
          begin
            if a.flag = above then
              result.flag := below
            else if a.flag = below then
              result.flag := above
            else
              result.flag := b.flag;
          end;
      end;
    end;
end;

function divi(a, b: TReading): TReading;
var
  num1, num2: double;
begin
  num1 := a.Value;
  num2 := b.Value;
  result.flag := equal;
  if b.Value = 0 then
    result.Value := Math.Infinity
  else
    result.Value := num1 / num2;
  if sign(num1) = sign(num2) then
    begin
    if (a.flag = below) and (b.flag = below) or (a.flag = above) and (b.flag = above) then
      result.Value := NaN
    else if (b.flag = equal) and (num1 > 0) then
      result.flag := a.flag
    else if (b.flag = equal) and (num1 < 0) then
      begin
      if a.flag = below then
        result.flag := above
      else if a.flag = above then
        result.flag := below;
      end
    else if num1 > 0 then
      begin
        if b.flag = below then
          result.flag := above
        else if b.flag = above then
          result.flag := below;
      end
    else
      result.flag := b.flag;
    end
  else
    begin
      if (a.flag = below) and (b.flag = above) or (a.flag = above) and (b.flag = below) then
          result.Value := NaN
      else if (b.flag = equal) and (num1 < 0) then
        result.flag := a.flag
      else if (b.flag = equal) and (num1 > 0) then
        begin
        if a.flag = below then
          result.flag := above
        else if a.flag = above then
          result.flag := below;
        end
      else if num1 > 0 then
        begin
          if b.flag = below then
            result.flag := above
          else if b.flag = above then
            result.flag := below;
        end
      else
        result.flag := b.flag;
    end
end;

operator + (const a: TReading; const b: TReading): TReading;
begin
  result := sum(a, b);
end;

operator - (const a: TReading; const b: TReading): TReading;
begin
  result := diff(a, b);
end;

operator * (const a: TReading; const b: TReading): TReading;
begin
  result := mul(a, b);
end;

operator / (const a: TReading; const b: TReading): TReading;
begin
  result := divi(a, b);
end;

end.

