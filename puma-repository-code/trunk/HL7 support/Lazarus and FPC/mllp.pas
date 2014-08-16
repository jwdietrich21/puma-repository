unit MLLP;

{ PUMA Repository }

{ Pascal Units for Medical Applications }

{ HL7 support unit for MLLP (minimal lower layer protocol) }

{ Version 1.6 }

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

{
Status code of MLLP object:
 0: No Error.
 2: Block not properly terminated.
 4: This HL7 version is not supported.
 9: Error creating HL7 message.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, HL7;

const
  SB = chr($B);
  EB = chr($1c);

type

  { TMLLP }

  TMLLP = class(TObject)
  private
    FMessage: THL7Message;
    Status: integer;
  protected
    function GetMessage: THL7Message;
    procedure SetMessage(theMessage: THL7Message);
    procedure SetMessage(theMessageString: ansistring);
    function AsBlock: ansistring;
    procedure SetBlock(theBlock: ansistring);
  public
    constructor Create;
    destructor Destroy;
    property message: THL7Message read GetMessage write SetMessage;
    property block: ansistring read AsBlock write SetBlock;
    property StatusCode: integer read status;
  end;

implementation

{ TMLLP }

function TMLLP.GetMessage: THL7Message;
begin
  Result := FMessage;
end;

procedure TMLLP.SetMessage(theMessage: THL7Message);
begin
  FMessage := theMessage;
end;

procedure TMLLP.SetMessage(theMessageString: ansistring);
begin
  if FMessage = nil then
    FMessage := THL7Message.Create('2.5');
  if message = nil then
    status := createErr
  else
  begin
    FMessage.contentString := theMessageString;
    status := noErr;
  end;
end;

function TMLLP.AsBlock: ansistring;
begin
  Result := SB + FMessage.contentString + EB + ksCR;
end;

procedure TMLLP.SetBlock(theBlock: ansistring);
begin
  if (length(theBlock) > 2) and (theBlock[1] = SB) and
    (RightStr(theBlock, 2) = EB + ksCR) then
  begin
    if FMessage = nil then
      FMessage := THL7Message.Create('2.5');
    FMessage.contentString := MidStr(theBlock, 2, length(theBlock) - 3);
  end
  else
    status := termErr;
end;

constructor TMLLP.Create;
begin
  inherited Create;
  block := '';
  Message := THL7Message.Create('2.5');
  if message = nil then
    status := createErr
  else
    status := noErr;
end;

destructor TMLLP.Destroy;
var
  tempMessage: THL7Message;
begin
  if Message <> nil then
  begin
    tempMessage := Message;
    Message := nil;
    tempMessage.Destroy;
  end;
  inherited Destroy;
end;

end.