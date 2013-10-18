unit UnitConverter;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ Unit Converter }

{ Version 1.1.2 }

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
  Classes, SysUtils, Math;

const
  MAXFACTORS = 10; {number of supported prefixes for measurement units}

type
  tMeasurement = record
    Value: extended;
    uom: string;
  end;
  tUnitElements = record
       MassPrefix, MassUnit, VolumePrefix, VolumeUnit: String;
  end;

var
  gPosition: integer;
  PrefixLabel: array[0..MAXFACTORS - 1] of string;
  PrefixFactor: array[0..MAXFACTORS - 1] of real;
  UnitLabel: array[0..MAXFACTORS - 1] of string;

function DecodeGreek(theString: string): string;
function EncodeGreek(theString: string): string;
function ParsedUnitString(theString: String): TUnitElements;
function ParsedMeasurement(measurement: string): tMeasurement;
function ConvertedValue(value, molarMass: real; fromUnit, toUnit: string): real;
function ValueFromUnit(fromValue: string; molarMass: real; toUnit: string): real;
function UnitFromValue(value, molarMass: real; fromUnit, toUnit: string): string;
function UnitFromValueF(value, molarMass: real; fromUnit, toUnit: string; format: TFloatFormat; precision, digits: integer): string;
function ConvertedUnit(fromValue: string; molarMass: real; toUnit: string): string;
function ConvertedUnitF(fromValue: string; molarMass: real; toUnit: string; format: TFloatFormat; precision, digits: integer): string;

implementation

const
  kTAB = chr(9);
  kLF = chr(10);
  kCR = chr(13);
  kETB = char(23);
  DEC_POINT = '.';
  DEC_COMMA = ',';

procedure InitConversionFactors;
{sets labels and appropriate conversion factors for the elements of measurement units}
begin
  PrefixLabel[0] := '';
  PrefixLabel[1] := 'd';
  PrefixLabel[2] := 'c';
  PrefixLabel[3] := 'm';
  PrefixLabel[4] := 'µ';
  PrefixLabel[5] := 'n';
  PrefixLabel[6] := 'p';
  PrefixLabel[7] := 'f';
  PrefixFactor[0] := 1;
  PrefixFactor[1] := 1e-1;
  PrefixFactor[2] := 1e-2;
  PrefixFactor[3] := 1e-3;
  PrefixFactor[4] := 1e-6;
  PrefixFactor[5] := 1e-9;
  PrefixFactor[6] := 1e-12;
  PrefixFactor[7] := 1e-15;
  UnitLabel[0] := 'g';
  UnitLabel[1] := 'mol';
end;

function ValidChar(theChar: char): boolean;
  {check a character in a string representing a number for validity}
type
  format1 = set of char;
var
  formatn1, formatn2, formatd1, formatd2, formate, formata, formato,
  formatc, formatl, validformat: format1;
begin
  formatn1 := ['1'..'9', '0', kTAB];
  formatn2 := ['1'..'9', '0'];
  formatd1 := ['.', ','];
  formatd2 := ['.'];
  formate := ['e', 'E'];
  formata := [' '..chr(255)];
  formato := ['+', '-'];
  formatc := [char($1c), char($1d), char($1e), char($1f), char($08)];
  formatl := [kCR, kLF];
  validformat := formatn2 + formatd1 + formate + formato;
  if theChar in validformat then
    ValidChar := True
  else
    ValidChar := False;
end;

function NextChar(theString: string): char;
{ read next char of string }
begin
  if gPosition <= length(theString) then
  begin
    NextChar := theString[gPosition];
    gPosition := gPosition + 1;
  end
  else
    NextChar := kETB;
end;

function EncodeGreek(theString: string): string;
{encodes greek mu letter as ASCII substitution sequence}
var
  theFlags: TReplaceFlags;
begin
  theFlags := [rfReplaceAll, rfIgnoreCase];
  Result := StringReplace(theString, #194#181, 'mc', theFlags);
end;

function DecodeGreek(theString: string): string;
{decodes ASCII substitution sequence for greek mu letter}
var
  theFlags: TReplaceFlags;
begin
  theFlags := [rfReplaceAll, rfIgnoreCase];
  result := UTF8Decode(StringReplace(theString, 'mc', #194#181, theFlags));
end;

function ParsedUnitString(theString: string): TUnitElements;
  {parses a string for measurement unit and breaks it up in single components of a TUnitElements record}
var
  theElements: TUnitElements;
  tempPos:integer;
begin
  with theElements do
  begin
    if theString = '' then
    begin
      MassPrefix := '';
      MassUnit := '';
      VolumePrefix := '';
      VolumeUnit := '';
    end
    else if theString = 'NA' then
    begin
      MassPrefix := 'NA';
      MassUnit := 'NA';
      VolumePrefix := 'NA';
      VolumeUnit := 'NA';
    end
    else
      with theElements do
      begin
        if (copy(theString, 1, 1) = 'g') and (copy(theString, 2, 1) = '/') then {e.g. 'g/l'}
          begin
            MassPrefix := '';
            MassUnit := 'g';
          end
        else if (copy(theString, 1, 3) = 'mol') then {e.g. 'mol/l'}
          begin
            MassPrefix := '';
            MassUnit := 'mol';
          end
        else
          begin
            if copy(theString, 1, 1) = 'm' then
            begin
              if copy(theString, 2, 1) = 'c' then
              begin
                MassPrefix := PrefixLabel[4]; {mc -> µ}
                temppos := pos('/', theString) - 2;
                MassUnit := copy(theString, 3, pos('/', theString) - 3);
              end
              else
              begin
                MassPrefix := copy(theString, 1, 1);
                MassUnit := copy(theString, 2, pos('/', theString) - 2);
              end;
            end
            else
            begin
              MassPrefix := copy(theString, 1, 1);
              MassUnit := copy(theString, 2, pos('/', theString) - 2);
            end;
          end;
        VolumePrefix := copy(theString, pos('/', theString) + 1, 1);
        VolumeUnit := 'l';
        if VolumePrefix = VolumeUnit then
          VolumePrefix := '';  {no prefix set}
      end;
  end;
  ParsedUnitString := theElements;
end;

function ParsedMeasurement(measurement: string): tMeasurement;
{ decompose measurement result into numeric value and unit }
var
  ch: char;

  function Number: extended;
  {$IFDEF FPC}  {version for FPC}
  var
    i, n: integer;
    valString: string;
  begin
    valString := '';
    n := length(measurement);
    number := NaN;
    if n > 0 then
    begin
      i := 1;
      ch := measurement[i];
      while (ValidChar(ch)) and (i <= n) do
      begin
        valString := valString + ch;
        inc(i);
        ch := measurement[i];
      end;
      if pos(DEC_COMMA, valString) > 0 then
        decimalSeparator := DEC_COMMA
      else
        decimalSeparator := DEC_POINT;
      number := StrToFloat(valString);
      gPosition := i + 1;
    end;
  end;
  {$ELSE}  {version for other compilers}
  var
    i, j, d, e, f: integer;
    k, l: extended;
    dig: array[0..31] of char;
    wholes, exponent: boolean;
    exponent_sign, base_sign: integer;
  begin
    wholes := True;
    exponent := False;
    f := 0;
    i := 0;
    exponent_sign := 1;
    base_sign := 1;
    repeat
      dig[i] := ch;
      i := i + 1;
      ch := NextChar(measurement);
    until not (ValidChar(ch));
    j := 0;
    k := 0;
    l := 0;
    repeat
      if (dig[j] <> '.') and (dig[j] <> ',') and (uppercase(dig[j]) <>
        uppercase('e')) and (dig[j] <> '+') and (dig[j] <> '-') then
      begin
        d := integer(dig[j]) - $30;
        if not exponent then
        begin
          if wholes then
          begin
            if (d < 10) and ((maxlongint - d) div 10 >= k) then
              k := k * 10 + d;
          end
          else
          begin
            f := f + 1;
            k := k + d / (exp(ln(10) * f));
          end;
        end
        else
        begin
          if wholes then
          begin
            if (d < 10) and ((maxlongint - d) div 10 >= l) then
              l := l * 10 + d;
          end
          else
          begin
            f := f + 1;
            l := l + d / (exp(ln(10) * f));
          end;
        end;
      end
      else if (dig[j] = '.') or (dig[j] = ',') then
        wholes := False
      else if uppercase(dig[j]) = uppercase('e') then
      begin
        exponent := True;
        wholes := True;
      end
      else if dig[j] = '-' then
        if exponent then
          exponent_sign := -1
        else
          base_sign := -1;
      j := j + 1;
    until j = i;
    k := k * base_sign;
    l := l * exponent_sign;
    number := k * exp(ln(10) * l);
  end;
  {$ENDIF}

begin
  gPosition := 1;
  ch := NextChar(measurement);
  if not ValidChar(ch) then
  begin
    parsedMeasurement.Value := NaN;
    parsedMeasurement.uom := '';
  end
  else
  begin
    parsedMeasurement.Value := Number;
    parsedMeasurement.uom := RightStr(measurement, length(measurement) - gPosition + 1);
  end;
end;

function ConvertedValue(value, molarMass: real; fromUnit, toUnit: string): real;
{ converts value from one measurement unit to another one }
var
  fromUnitElements, toUnitElements: TUnitElements;
  i, fromMpIndex, fromMuIndex, fromVpIndex, toMpIndex, toMuIndex, toVpIndex: integer;
  conversionFactor: real;
begin
  if isNaN(value) then
  ConvertedValue := NaN
  else
    begin
      fromMpIndex := 0;    {Index for mass prefix}
      fromMuIndex := 0;    {index for mass unit}
      fromVpIndex := 0;    {index for volume prefix}
      toMpIndex := 0;    {Index for mass prefix}
      toMuIndex := 0;    {index for mass unit}
      toVpIndex := 0;    {index for volume prefix}
      fromUnitElements := ParsedUnitString(EncodeGreek(fromUnit));
      toUnitElements := ParsedUnitString(EncodeGreek(toUnit));
      for i := MAXFACTORS - 1 downto 0 do
        begin
          if fromUnitElements.MassPrefix = PrefixLabel[i] then fromMpIndex := i;
          if fromUnitElements.MassUnit = UnitLabel[i] then fromMuIndex := i;
          if fromUnitElements.VolumePrefix = PrefixLabel[i] then fromVpIndex := i;
          if toUnitElements.MassPrefix = PrefixLabel[i] then toMpIndex := i;
          if toUnitElements.MassUnit = UnitLabel[i] then toMuIndex := i;
          if toUnitElements.VolumePrefix = PrefixLabel[i] then toVpIndex := i;
        end;
      if (fromUnitElements.MassUnit = 'mol') and (toUnitElements.MassUnit = 'g') then        {SI to conventional}
        conversionFactor := PrefixFactor[fromMpIndex] * molarMass / PrefixFactor[fromVpIndex] * PrefixFactor[toVpIndex] / PrefixFactor[toMpIndex]
      else if (fromUnitElements.MassUnit = 'g') and (toUnitElements.MassUnit = 'mol') then        {conventional to SI}
        conversionFactor := PrefixFactor[fromMpIndex] * 1 / molarMass / PrefixFactor[fromVpIndex] * PrefixFactor[toVpIndex] / PrefixFactor[toMpIndex]
      else if fromUnitElements.MassUnit = toUnitElements.MassUnit then         {identical units}
        conversionFactor := PrefixFactor[fromMpIndex] / PrefixFactor[fromVpIndex] * PrefixFactor[toVpIndex] / PrefixFactor[toMpIndex]
      else conversionFactor := NaN;
      ConvertedValue := value * conversionFactor;
    end;
end;

function ValueFromUnit(fromValue: string; molarMass: real; toUnit: string): real;
{ converts value from one measurement unit to another one and delivers numeric result }
var
  value, target: real;
  fromUnit: string;
  theMeasurement: tMeasurement;
begin
  if fromValue = '' then
  ValueFromUnit := NaN
  else
    begin
      theMeasurement := ParsedMeasurement(fromValue);
      value := theMeasurement.Value;
      fromUnit := theMeasurement.uom;
      target := ConvertedValue(value, molarMass, fromUnit, toUnit);
      ValueFromUnit := target;
    end;
end;

function UnitFromValue(value, molarMass: real; fromUnit, toUnit: string): string;
var
  target: real;
begin
  if isNaN(value) then
  UnitFromValue := 'NaN'
  else
    begin
      target := ConvertedValue(value, molarMass, fromUnit, toUnit);
      UnitFromValue := FloatToStr(target) + ' ' + toUnit;
    end;
end;

function UnitFromValueF(value, molarMass: real; fromUnit, toUnit: string; format: TFloatFormat; precision, digits: integer): string;
var
  target: real;
begin
  if isNaN(value) then
  UnitFromValueF := 'NaN'
  else
    begin
      target := ConvertedValue(value, molarMass, fromUnit, toUnit);
      UnitFromValueF := FloatToStrF(target, format, precision, digits) + ' ' + toUnit;
    end;
end;

function ConvertedUnit(fromValue: string; molarMass: real; toUnit: string): string;
{ converts value from one measurement unit to another one and delivers string with result }
var
  value, target: real;
  fromUnit: string;
  theMeasurement: tMeasurement;
begin
  if fromValue = '' then
  ConvertedUnit := ''
  else
    begin
      theMeasurement := ParsedMeasurement(fromValue);
      value := theMeasurement.Value;
      fromUnit := theMeasurement.uom;
      target := ConvertedValue(value, molarMass, fromUnit, toUnit);
      ConvertedUnit := FloatToStr(target) + ' ' + toUnit;
    end;
end;

function ConvertedUnitF(fromValue: string; molarMass: real; toUnit: string; format: TFloatFormat; precision, digits: integer): string;
var
  value, target: real;
  fromUnit: string;
  theMeasurement: tMeasurement;
begin
  if fromValue = '' then
  ConvertedUnitF := ''
  else
    begin
      theMeasurement := ParsedMeasurement(fromValue);
      value := theMeasurement.Value;
      fromUnit := theMeasurement.uom;
      target := ConvertedValue(value, molarMass, fromUnit, toUnit);
      ConvertedUnitF := FloatToStrF(target, format, precision, digits) + ' ' + toUnit;
    end;
end;

initialization
  InitConversionFactors;

end.
