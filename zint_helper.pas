unit zint_helper;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, zint;

function StrToArrayOfByte(const AString : String) : TArrayOfByte;
function ArrayOfByteToString(const AArray : TArrayOfByte) : String;
function StrToArrayOfChar(const AString : String) : TArrayOfChar;
function ArrayOfCharToString(const AArray : TArrayOfChar) : String;
function ArrayOfCharToArrayOfByte(const AArray : TArrayOfChar) : TArrayOfByte;
function ArrayOfByteToArrayOfChar(const AArray : TArrayOfByte) : TArrayOfChar;
procedure ArrayCopy(var ADestination : TArrayOfChar; const ASource : TArrayOfByte; ACount : Integer = MaxInt); overload;
procedure ArrayCopy(var ADestination : TArrayOfByte; const ASource : TArrayOfChar; ACount : Integer = MaxInt); overload;
procedure ArrayCopy(var ADestination : TArrayOfChar; const ASource : TArrayOfChar; ACount : Integer = MaxInt); overload;
procedure ArrayCopy(var ADestination : TArrayOfByte; const ASource : TArrayOfByte; ACount : Integer = MaxInt); overload;
procedure Fill(var ADestination : TArrayOfChar; ACount : Integer; AChar : Char; AStartIndex : Integer = 0); overload;
procedure Fill(var ADestination : TArrayOfSmallInt; ACount : Integer; AValue : Smallint; AStartIndex : Integer = 0); overload;
procedure Fill(var ADestination : TArrayOfInteger; ACount : Integer; AValue : Integer; AStartIndex : Integer = 0); overload;
procedure Fill(var ADestination : TArrayOfByte; ACount : Integer; AValue : Byte; AStartIndex : Integer = 0); overload;

implementation

uses
  zint_common;

function StrToArrayOfByte(const AString: String): TArrayOfByte;
var
  c : Char;
  i : Integer;
begin
  SetLength(Result, Length(AString) + 1);
  i := 0;
  for c in AString do
  begin
    Result[i]:=Ord(c);
    inc(i);
  end;
  Result[i] := 0;
end;

function ArrayOfByteToString(const AArray: TArrayOfByte): String;
var
  i : Integer;
begin
  Result := '';

  for i := Low(AArray) to ustrlen(AArray) - 1 do
    Result := Result + Chr(AArray[i]);
end;

function StrToArrayOfChar(const AString: String): TArrayOfChar;
var
  i : Integer;
  c : Char;
begin
  SetLength(Result, Length(AString) + 1);
  i := Low(Result);
  for c in AString do
  begin
    Result[i] := c;
    Inc(i);
  end;
  Result[High(Result)] := Chr(0);
end;

function ArrayOfCharToString(const AArray: TArrayOfChar): String;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to strlen(AArray) - 1 do
    Result := Result + AArray[i];
end;

function ArrayOfCharToArrayOfByte(const AArray: TArrayOfChar): TArrayOfByte;
var
  i : Integer;
begin
  SetLength(Result, strlen(AArray) + 1);
  ArrayCopy(Result, AArray);
end;

function ArrayOfByteToArrayOfChar(const AArray: TArrayOfByte): TArrayOfChar;
begin
  SetLength(Result, Length(AArray));
  ArrayCopy(Result, AArray);
end;

procedure ArrayCopy(var ADestination: TArrayOfChar;
  const ASource: TArrayOfByte; ACount: Integer);
var
  i, j, cnt : Integer;
begin
  i := Low(ADestination);
  j := Low(ASource);
  cnt := 0;
  while (i <= High(ADestination)) and (j <= High(ASource)) and (cnt <= ACount) do
  begin
    ADestination[i] := Chr(ASource[j]);
    Inc(i);
    Inc(j);
    Inc(cnt);
  end;
end;

procedure ArrayCopy(var ADestination: TArrayOfByte;
  const ASource: TArrayOfChar; ACount: Integer);
var
  i, j, cnt : Integer;
begin
  i := Low(ADestination);
  j := Low(ASource);
  cnt := 0;
  while (i <= High(ADestination)) and (j <= High(ASource)) and (cnt <= ACount) do
  begin
    ADestination[i] := Ord(ASource[j]);
    Inc(i);
    Inc(j);
    Inc(cnt);
  end;
end;

procedure ArrayCopy(var ADestination: TArrayOfChar;
  const ASource: TArrayOfChar; ACount: Integer);
var
  i, j, cnt : Integer;
begin
  i := Low(ADestination);
  j := Low(ASource);
  cnt := 0;
  while (i <= High(ADestination)) and (j <= High(ASource)) and (cnt <= ACount) do
  begin
    ADestination[i] := ASource[j];
    Inc(i);
    Inc(j);
    Inc(cnt);
  end;
end;

procedure ArrayCopy(var ADestination : TArrayOfByte; const ASource : TArrayOfByte; ACount : Integer = MaxInt);
var
  i, j, cnt : Integer;
begin
  i := Low(ADestination);
  j := Low(ASource);
  cnt := 0;
  while (i <= High(ADestination)) and (j <= High(ASource)) and (cnt <= ACount) do
  begin
    ADestination[i] := ASource[j];
    Inc(i);
    Inc(j);
    Inc(cnt);
  end;
end;

procedure Fill(var ADestination: TArrayOfChar; ACount: Integer; AChar: Char;
  AStartIndex: Integer);
var
  i : Integer;
begin
  for i := AStartIndex to AStartIndex + ACount - 1 do
    ADestination[i] := AChar;
end;

procedure Fill(var ADestination: TArrayOfSmallInt; ACount: Integer;
  AValue: Smallint; AStartIndex: Integer);
var
  i : Integer;
begin
  for i := AStartIndex to AStartIndex + ACount - 1 do
    ADestination[i] := AValue;
end;

procedure Fill(var ADestination: TArrayOfInteger; ACount: Integer;
  AValue: Integer; AStartIndex: Integer);
var
  i : Integer;
begin
  for i := AStartIndex to AStartIndex + ACount - 1 do
    ADestination[i] := AValue;
end;

procedure Fill(var ADestination: TArrayOfByte; ACount: Integer; AValue: Byte;
  AStartIndex: Integer);
var
  i : Integer;
begin
  for i := AStartIndex to AStartIndex + ACount - 1 do
    ADestination[i] := AValue;
end;

end.

