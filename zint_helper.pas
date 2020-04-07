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
procedure ArrayCopy(var ADestination : TArrayOfChar; const ASource : TArrayOfByte; ACount : NativeInt = MaxInt); overload;
procedure ArrayCopy(var ADestination : TArrayOfByte; const ASource : TArrayOfChar; ACount : NativeInt = MaxInt); overload;
procedure ArrayCopy(var ADestination : TArrayOfChar; const ASource : TArrayOfChar; ACount : NativeInt = MaxInt); overload;
procedure ArrayCopy(var ADestination : TArrayOfByte; const ASource : TArrayOfByte; ACount : NativeInt = MaxInt); overload;
procedure Fill(var ADestination : TArrayOfChar; ACount : NativeInt; AChar : Char; AStartIndex : NativeInt = 0); overload;
procedure Fill(var ADestination : TArrayOfSmallInt; ACount : NativeInt; AValue : Smallint; AStartIndex : NativeInt = 0); overload;
procedure Fill(var ADestination : TArrayOfInteger; ACount : NativeInt; AValue : NativeInt; AStartIndex : NativeInt = 0); overload;
procedure Fill(var ADestination : TArrayOfByte; ACount : NativeInt; AValue : Byte; AStartIndex : NativeInt = 0); overload;

implementation

uses
  zint_common;

function StrToArrayOfByte(const AString: String): TArrayOfByte;
var
  Len : NativeInt;
begin
  Len := AString.Length;

  if Len > 0 then
    Result := @RawByteString(AString)[1];

  SetLength(Result, Len + 1); //For terminal #0
  Result[len] := 0;
end;

function ArrayOfByteToString(const AArray: TArrayOfByte): String;
var
  i, ArrayLen : NativeInt;
begin
  ArrayLen := ustrlen(AArray);
  SetLength(Result, ArrayLen);

  if ArrayLen > 0 then
    for i := Low(AArray) to ArrayLen - 1 do
      Result[i + 1] := Chr(AArray[i]);
end;

function StrToArrayOfChar(const AString: String): TArrayOfChar;
var
  len : NativeInt;
begin
  len := AString.Length;
  SetLength(Result, len + 1);
  AString.CopyTo(0, Result[0], 0, len);
  Result[len] := #0;
end;

function ArrayOfCharToString(const AArray: TArrayOfChar): String;
var
  i, ArrayLen : NativeInt;
begin
  ArrayLen := strlen(AArray);
  SetLength(Result, ArrayLen);

  for i := 0 to ArrayLen - 1 do
    Result[i + 1] := AArray[i];
end;

function ArrayOfCharToArrayOfByte(const AArray: TArrayOfChar): TArrayOfByte;
begin
  SetLength(Result, strlen(AArray) + 1);
  ArrayCopy(Result, AArray);
end;

function ArrayOfByteToArrayOfChar(const AArray: TArrayOfByte): TArrayOfChar;
begin
  SetLength(Result, Length(AArray));
  ArrayCopy(Result, AArray);
end;

procedure ArrayCopy(var ADestination: TArrayOfChar; const ASource: TArrayOfByte; ACount: NativeInt);
var
//  i, j, cnt : NativeInt;
  i : NativeInt;
begin
//  i := Low(ADestination);
//  j := Low(ASource);
//  cnt := 0;

//  while (i <= High(ADestination)) and (j <= High(ASource)) and (cnt <= ACount) do
//  begin
//    ADestination[i] := Char(ASource[j]);
//    Inc(i);
//    Inc(j);
//    Inc(cnt);
//  end;

  if High(ADestination) < ACount then
    ACount := High(ADestination);

  if High(ASource) < ACount then
   ACount := High(ASource);

  for I := Low(ASource) to ACount - 1 do
    ADestination[i] := Char(ASource[i]);
end;

procedure ArrayCopy(var ADestination: TArrayOfByte; const ASource: TArrayOfChar; ACount: NativeInt);
var
//  i, j, cnt : NativeInt;
  i : NativeInt;
begin
//  i := Low(ADestination);
//  j := Low(ASource);
//  cnt := 0;
//  while (i <= High(ADestination)) and (j <= High(ASource)) and (cnt <= ACount) do
//  begin
//    ADestination[i] := Ord(ASource[j]);
//    Inc(i);
//    Inc(j);
//    Inc(cnt);
//  end;

  if High(ADestination) < ACount then
    ACount := High(ADestination);

  if High(ASource) < ACount then
   ACount := High(ASource);

  for I := Low(ASource) to ACount do
    ADestination[i] := Ord(ASource[i]);   // What happens if Char is > 255 ?
end;

procedure ArrayCopy(var ADestination: TArrayOfChar; const ASource: TArrayOfChar; ACount: NativeInt);
//var
//  i, j, cnt : Integer;
begin
//  i := Low(ADestination);
//  j := Low(ASource);
//  cnt := 0;
//  while (i <= High(ADestination)) and (j <= High(ASource)) and (cnt <= ACount) do
//  begin
//    ADestination[i] := ASource[j];
//    Inc(i);
//    Inc(j);
//    Inc(cnt);
//  end;
  if High(ADestination) < ACount then
    ACount := High(ADestination);

  if High(ASource) < ACount then
    ACount := High(ASource);

  Move(ASource[0], ADestination[0], ACount * SizeOf(Char));
end;

procedure ArrayCopy(var ADestination : TArrayOfByte; const ASource : TArrayOfByte; ACount : NativeInt = MaxInt);
//var
//  i, j, cnt : Integer;
begin
//  i := Low(ADestination);
//  j := Low(ASource);
//  cnt := 0;
//  while (i <= High(ADestination)) and (j <= High(ASource)) and (cnt <= ACount) do
//  begin
//    ADestination[i] := ASource[j];
//    Inc(i);
//    Inc(j);
//    Inc(cnt);
//  end;
  if High(ADestination) < ACount then
    ACount := High(ADestination);

  if High(ASource) < ACount then
    ACount := High(ASource);

  Move(ASource[0], ADestination[0], ACount);
end;

procedure Fill(var ADestination: TArrayOfChar; ACount: NativeInt; AChar: Char; AStartIndex: NativeInt);
//var
//  i : NativeInt;
begin
//  for i := AStartIndex to AStartIndex + ACount - 1 do
//    ADestination[i] := AChar;
  FillChar(ADestination[AStartIndex], ACount, AChar);
end;

procedure Fill(var ADestination: TArrayOfSmallInt; ACount: NativeInt; AValue: Smallint; AStartIndex: NativeInt);
var
  i : NativeInt;
begin
  for i := AStartIndex to AStartIndex + ACount - 1 do
    ADestination[i] := AValue;
end;

procedure Fill(var ADestination: TArrayOfInteger; ACount: NativeInt; AValue: NativeInt; AStartIndex: NativeInt);
var
  i : NativeInt;
begin
  for i := AStartIndex to AStartIndex + ACount - 1 do
    ADestination[i] := AValue;
end;

procedure Fill(var ADestination: TArrayOfByte; ACount: NativeInt; AValue: Byte; AStartIndex: NativeInt);
//var
//  i : NativeInt;
begin
//  for i := AStartIndex to AStartIndex + ACount - 1 do
//    ADestination[i] := AValue;
  FillChar(ADestination[AStartIndex], ACount, AValue);
end;

end.

