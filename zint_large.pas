unit zint_large;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b complete
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, zint;

const BCD : array[0..39] of SmallInt = (
	0, 0, 0, 0,
	1, 0, 0, 0,
	0, 1, 0, 0,
	1, 1, 0, 0,
	0, 0, 1, 0,
	1, 0, 1, 0,
	0, 1, 1, 0,
	1, 1, 1, 0,
	0, 0, 0, 1,
	1, 0, 0, 1
);

procedure binary_add(var accumulator : TArrayOfSmallInt; const input_buffer : TArrayOfSmallInt);
procedure binary_subtract(var accumulator : TArrayOfSmallInt; const input_buffer : TArrayOfSmallInt);
procedure shiftdown(var buffer : TArrayOfSmallInt);
procedure shiftup(var buffer : TArrayOfSmallInt);
function islarger(const accum : TArrayOfSmallInt; const reg : TArrayOfSmallInt) : SmallInt;
procedure binary_load(var reg : TArrayOfSmallInt; const data : TArrayOfChar; const src_len : Cardinal);
procedure hex_dump(const input_buffer : TArrayOfSmallInt);

implementation

{ Binary addition }
procedure binary_add(var accumulator : TArrayOfSmallInt; const input_buffer : TArrayOfSmallInt);
var
  i, carry, done : Integer;
begin
  carry := 0;

  for i := 0 to 111 do
  begin
    done := 0;
    if (((input_buffer[i] = 0) and (accumulator[i] = 0)) and ((carry = 0) and (done = 0))) then
    begin
      accumulator[i] := 0;
      carry := 0;
      done := 1;
    end;
    if (((input_buffer[i] = 0) and (accumulator[i] = 0)) and ((carry = 1) and (done = 0))) then
    begin
      accumulator[i] := 1;
      carry := 0;
      done := 1;
    end;
    if (((input_buffer[i] = 0) and (accumulator[i] = 1)) and ((carry = 0) and (done = 0))) then
    begin
      accumulator[i] := 1;
      carry := 0;
      done := 1;
    end;
    if (((input_buffer[i] = 0) and (accumulator[i] = 1)) and ((carry = 1) and (done = 0))) then
    begin
      accumulator[i] := 0;
      carry := 1;
      done := 1;
    end;
    if (((input_buffer[i] = 1) and (accumulator[i] = 0)) and ((carry = 0) and (done = 0))) then
    begin
      accumulator[i] := 1;
      carry := 0;
      done := 1;
    end;
    if (((input_buffer[i] = 1) and (accumulator[i] = 0)) and ((carry = 1) and (done = 0))) then
    begin
      accumulator[i] := 0;
      carry := 1;
      done := 1;
    end;
    if (((input_buffer[i] = 1) and (accumulator[i] = 1)) and ((carry = 0) and (done = 0))) then
    begin
      accumulator[i] := 0;
      carry := 1;
      done := 1;
    end;
    if (((input_buffer[i] = 1) and (accumulator[i] = 1)) and ((carry = 1) and (done = 0))) then
    begin
      accumulator[i] := 1;
      carry := 1;
      //done := 1;
    end;
  end;
end;

{ 2's compliment subtraction }
{ take input_buffer from accumulator and put answer in accumulator }
procedure binary_subtract(var accumulator : TArrayOfSmallInt; const input_buffer : TArrayOfSmallInt);
var
  i : Integer;
  sub_buffer : TArrayOfSmallInt;
begin
  SetLength(sub_buffer, 112);

  for i := 0 to 111 do
  begin
    if (input_buffer[i] = 0) then
      sub_buffer[i] := 1
    else
      sub_buffer[i] := 0;
  end;
  binary_add(accumulator, sub_buffer);

  sub_buffer[0] := 1;

  for i := 1 to 111 do
    sub_buffer[i] := 0;

  binary_add(accumulator, sub_buffer);
end;

procedure shiftdown(var buffer : TArrayOfSmallInt);
var
  i : Integer;
begin
  buffer[102] := 0;
  buffer[103] := 0;

  for i := 0 to 101 do
    buffer[i] := buffer[i + 1];
end;

procedure shiftup(var buffer : TArrayOfSmallInt);
var
  i : Integer;
begin
  for i := 102 downto 1 do
    buffer[i] := buffer[i - 1];

  buffer[0] := 0;
end;

{ Returns 1 if accum[] is larger than reg[], else  }
function islarger(const accum : TArrayOfSmallInt; const reg : TArrayOfSmallInt) : SmallInt;
var
  i, latch, larger : Integer;
begin
  latch := 0;
  i := 103;
  larger := 0;

  repeat
    if ((accum[i] = 1) and (reg[i] = 0)) then
    begin
      latch := 1;
      larger := 1;
    end;
    if ((accum[i] = 0) and (reg[i] = 1)) then
      latch := 1;

    Dec(i);
  until not ((latch = 0) and (i >= -1));

  result := larger; exit;
end;

procedure binary_load(var reg : TArrayOfSmallInt; const data : TArrayOfChar; const src_len : Cardinal);
var
  read, i : Integer;
  temp : TArrayOfSmallInt;
begin
  SetLength(temp, 112);

  for i := 0 to 111 do
    reg[i] := 0;

  for read := 0 to  src_len - 1 do
  begin

    for i := 0 to 111 do
      temp[i] := reg[i];

    for i := 0 to 8 do
      binary_add(reg, temp);

    temp[0] := BCD[StrToInt(data[read]) * 4];
    temp[1] := BCD[(StrToInt(data[read]) * 4) + 1];
    temp[2] := BCD[(StrToInt(data[read]) * 4) + 2];
    temp[3] := BCD[(StrToInt(data[read]) * 4) + 3];
    for i := 4 to 111 do
      temp[i] := 0;

    binary_add(reg, temp);
  end;
end;

procedure hex_dump(const input_buffer : TArrayOfSmallInt);
var
  i, digit, byte_space : Integer;
begin
  byte_space := 1;
  i := 100;
  while i >= 0 do
  begin
    digit := 0;
    Inc(digit, 1 * input_buffer[i]);
    Inc(digit, 2 * input_buffer[i + 1]);
    Inc(digit, 4 * input_buffer[i + 2]);
    Inc(digit, 8 * input_buffer[i + 3]);

    case digit of
      0: Write('0');
      1: Write('1');
      2: Write('2');
      3: Write('3');
      4: Write('4');
      5: Write('5');
      6: Write('6');
      7: Write('7');
      8: Write('8');
      9: Write('9');
      10: Write('A');
      11: Write('B');
      12: Write('C');
      13: Write('D');
      14: Write('E');
      15: Write('F');
    end;
    if (byte_space = 1) then
    begin
      byte_space := 0
    end
    else
    begin
      byte_space := 1;
      Write(' ');
    end;
    Dec(i, 4);
  end;
  WriteLn('');
end;

end.

