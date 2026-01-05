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
  Metron_major = 1;
  Metron_minor = 0;
  Metron_release = 0;
  Metron_patch = 0;
  Metron_fullversion = ((Metron_major * 100 + Metron_minor) * 100 + Metron_release) *
    100 + Metron_patch;
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
    procedure SetValue(Value: double);
    procedure SetText(Text: string);
    procedure SetFlag(flag: TFlag);
  public
    property flag: TFlag read FFlag write SetFlag;
    property Text: string read FText write SetText;
    property Value: double read FValue write SetValue;
  end;

operator +(const a: TReading; const b: TReading): TReading;
operator +(const a: double; const b: TReading): TReading;
operator +(const a: TReading; const b: double): TReading;
operator -(const a: TReading; const b: TReading): TReading;
operator *(const a: TReading; const b: TReading): TReading;
operator /(const a: TReading; const b: TReading): TReading;

implementation

{ TReading }

procedure TReading.SetValue(Value: double);
begin
  FValue := Value;
  case Flag of
    below: FText := '< ' + FloatToStr(Value);
    equal: FText := FloatToStr(Value);
    above: FText := '> ' + FloatToStr(Value);
    otherwise
      Flag := equal; // FText will automatically be assigned by SetFlag
  end;
end;

procedure TReading.SetText(Text: string);
begin
  FText := Text;
  if LeftStr(Text, 1) = '<' then
  begin
    FFlag := below;
    FValue := StrToFloat(RightStr(Text, length(Text) - 1));
  end
  else if LeftStr(Text, 1) = '>' then
  begin
    FFlag := above;
    FValue := StrToFloat(RightStr(Text, length(Text) - 1));
  end
  else
  begin
    FFlag := equal;
    FValue := StrToFloat(Text);
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
  Result.Value := num1 + num2;
  Result.flag := equal;
  if (a.flag = below) and (b.flag = above) or (a.flag = above) and (b.flag = below) then
  begin
    Result.Value := NaN;
    Result.flag := equal;
  end
  else if (a.flag = below) or (b.flag = below) then
    Result.flag := below
  else if (a.flag = above) or (b.flag = above) then
    Result.flag := above;
end;

function sum(a: double; b: TReading): TReading;
var
  temp: TReading;
begin
  temp.flag := equal;
  temp.Value := a;
  result := sum(temp, b);
end;

function sum(a: TReading; b: double): TReading;
var
  temp: TReading;
begin
  temp.flag := equal;
  temp.Value := b;
  result := sum(a, temp);
end;

function diff(a, b: TReading): TReading;
var
  num1, num2: double;
begin
  num1 := a.Value;
  num2 := b.Value;
  Result.Value := num1 - num2;
  Result.flag := equal;
  if (a.flag = below) and (b.flag = below) or (a.flag = above) and (b.flag = above) then
  begin
    Result.Value := NaN;
    Result.flag := equal;
  end
  else if a.flag = equal then
  begin
    if b.flag = below then
      Result.flag := above
    else if b.flag = above then
      Result.flag := below;
  end
  else
    Result.flag := a.flag;
end;

function mul(a, b: TReading): TReading;
var
  num1, num2: double;
begin
  num1 := a.Value;
  num2 := b.Value;
  Result.Value := num1 * num2;
  Result.flag := equal;
  if sign(num1) = sign(num2) then
  begin
    if (a.flag = below) and (b.flag = above) or (a.flag = above) and
      (b.flag = below) then
      Result.Value := NaN
    else if (a.flag = below) or (b.flag = below) then
    begin
      if num1 > 0 then
        Result.flag := below
      else
        Result.flag := above;
    end
    else if (a.flag = above) or (b.flag = above) then
    begin
      if num1 > 0 then
        Result.flag := above
      else
        Result.flag := below;
    end;
  end
  else
  begin
    if (a.flag = below) and (b.flag = below) or (a.flag = above) and
      (b.flag = above) then
      Result.Value := NaN
    else
    begin
      if num2 > 0 then
      begin
        Result.flag := a.flag;
        if b.flag = below then
          Result.flag := above
        else if b.flag = above then
          Result.flag := below;
      end
      else
      begin
        if a.flag = above then
          Result.flag := below
        else if a.flag = below then
          Result.flag := above
        else
          Result.flag := b.flag;
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
  Result.flag := equal;
  if b.Value = 0 then
    Result.Value := Math.Infinity
  else
    Result.Value := num1 / num2;
  if sign(num1) = sign(num2) then
  begin
    if (a.flag = below) and (b.flag = below) or (a.flag = above) and
      (b.flag = above) then
      Result.Value := NaN
    else
    begin
      if sign(num2) > 0 then
        if a.flag <> equal then
          Result.flag := a.flag
        else
        begin
          if sign(num1) < 0 then
            Result.flag := b.flag
          else
            case b.flag of
              below:
                Result.flag := above;
              above:
                Result.flag := below;
            end;
        end
      else
        case a.flag of
          below:
            Result.flag := above;
          above:
            Result.flag := below;
          equal:
          begin
            if sign(num1) < 0 then
              Result.flag := b.flag
            else
              case b.flag of
                below:
                  Result.flag := above;
                above:
                  Result.flag := below;
              end;
          end;
        end;
    end;
  end
  else
  begin
    if (a.flag = below) and (b.flag = above) or (a.flag = above) and
      (b.flag = below) then
      Result.Value := NaN
    else
    begin
      if sign(num2) > 0 then
        if a.flag <> equal then
          Result.flag := a.flag
        else
        begin
          if sign(num1) < 0 then
            Result.flag := b.flag
          else
            case b.flag of
              below:
                Result.flag := above;
              above:
                Result.flag := below;
            end;
        end
      else
        case a.flag of
          below:
            Result.flag := above;
          above:
            Result.flag := below;
          equal:
          begin
            if sign(num1) < 0 then
              Result.flag := b.flag
            else
              case b.flag of
                below:
                  Result.flag := above;
                above:
                  Result.flag := below;
              end;
          end;
        end;
    end;
  end;
end;

operator +(const a: TReading; const b: TReading): TReading;
begin
  Result := sum(a, b);
end;

operator + (const a: double; const b: TReading): TReading;
begin
  Result := sum(a, b);
end;

operator + (const a: TReading; const b: double): TReading;
begin
  Result := sum(a, b);
end;

operator -(const a: TReading; const b: TReading): TReading;
begin
  Result := diff(a, b);
end;

operator *(const a: TReading; const b: TReading): TReading;
begin
  Result := mul(a, b);
end;

operator /(const a: TReading; const b: TReading): TReading;
begin
  Result := divi(a, b);
end;

end.
