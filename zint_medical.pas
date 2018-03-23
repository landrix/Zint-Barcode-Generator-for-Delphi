unit zint_medical;

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

function pharma_one(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function pharma_two(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function codabar(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function code32(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses
  zint_common, zint_code, zint_helper;

const CALCIUM = '0123456789-$:/.+ABCD';

const CodaTable : array[0..19] of String = ('11111221', '11112211', '11121121', '22111111', '11211211', '21111211',
	'12111121', '12112111', '12211111', '21121111', '11122111', '11221111', '21112121', '21211121',
	'21212111', '11212121', '11221211', '12121121', '11121221', '11122211');

function pharma_one(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
  { 'Pharmacode can represent only a single integer from 3 to 131070. Unlike other
     commonly used one-dimensional barcode schemes, pharmacode does not store the data in a
     form corresponding to the human-readable digits; the number is encoded in binary, rather
     than decimal. Pharmacode is read from right to left: with n as the bar position starting
     at 0 on the right, each narrow bar adds 2n to the value and each wide bar adds 2(2^n).
     The minimum barcode is 2 bars and the maximum 16, so the smallest number that could
     be encoded is 3 (2 narrow bars) and the biggest is 131070 (16 wide bars).'
     - http://en.wikipedia.org/wiki/Pharmacode }

  { This code uses the One Track Pharamacode calculating algorithm as recommended by
     the specification at http://www.laetus.com/laetus.php?request=file&id=69 }
var
  tester : Cardinal;
  counter, error_number, h : Integer;
  inter : TArrayOfChar; { 131070 . 17 bits }
  dest : TArrayOfChar; { 17 * 2 + 1 }
begin
  SetLength(inter, 18);
  Fill(inter, 18, #0);
  SetLength(dest, 64);

  error_number := 0;

  if (_length > 6) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(NEON, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  tester := StrToIntDef(ArrayOfByteToString(source), 0);

  if ((tester < 3) or (tester > 131070)) then
  begin
    strcpy(symbol.errtxt, 'Data out of range');
    result := ZERROR_INVALID_DATA; exit;
  end;

  repeat
    if ((tester and 1) = 0) then
    begin
      concat(inter, 'W');
      tester := (tester - 2) div 2;
    end
    else
    begin
      concat(inter, 'N');
      tester := (tester - 1) div 2;
    end;
  until not (tester <> 0);

  h := strlen(inter) - 1;
  dest[0] := #0;
  for counter := h downto 0 do
  begin
    if (inter[counter] = 'W') then
      concat(dest, '32')
    else
      concat(dest, '12');
  end;

  expand(symbol, dest);

  result := error_number; exit;
end;

function pharma_two_calc(symbol : zint_symbol; source : TArrayOfByte; var dest : TArrayOfChar) : Integer;
  { This code uses the Two Track Pharamacode defined in the document at
     http://www.laetus.com/laetus.php?request=file&id=69 and using a modified
     algorithm from the One Track system. This standard accepts integet values
     from 4 to 64570080. }
var
  tester : Cardinal;
  counter, h : Integer;
  inter : TArrayOfChar;
  error_number : Integer;
begin
  SetLength(inter, 17);
  tester := StrToIntDef(ArrayOfByteToString(source), 0);

  if ((tester < 4) or (tester > 64570080)) then
  begin
    strcpy(symbol.errtxt, 'Data out of range');
    result := ZERROR_INVALID_DATA; exit;
  end;
  error_number := 0;
  strcpy(inter, '');
  repeat
    case tester mod 3 of
      0:
      begin
        concat(inter, '3');
        tester := (tester - 3) div 3;
      end;
      1:
      begin
        concat(inter, '1');
        tester := (tester - 1) div 3;
      end;
      2:
      begin
        concat(inter, '2');
        tester := (tester - 2) div 3;
      end;
    end;
  until not (tester <> 0);

  h := strlen(inter) - 1;
  for counter := h downto 0 do
    dest[h - counter] := inter[counter];

  dest[h + 1] := #0;

  result := error_number; exit;
end;

{ Draws the patterns for two track pharmacode }
function pharma_two(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  height_pattern : TArrayOfChar;
  loopey, h : Cardinal;
  writer : Integer;
  error_number : Integer;
begin
  SetLength(height_pattern, 200);
  error_number := 0;
  strcpy(height_pattern, '');

  if (_length > 8) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(NEON, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;
  error_number := pharma_two_calc(symbol, source, height_pattern);
  if (error_number <> 0) then
  begin
    result := error_number; exit;
  end;

  writer := 0;
  h := strlen(height_pattern);
  for loopey := 0 to h - 1 do
  begin
    if ((height_pattern[loopey] = '2') or (height_pattern[loopey] = '3')) then
    begin
      set_module(symbol, 0, writer);
    end;
    if ((height_pattern[loopey] = '1') or (height_pattern[loopey] = '3')) then
    begin
      set_module(symbol, 1, writer);
    end;
    Inc(writer, 2);
  end;
  symbol.rows := 2;
  symbol.width := writer - 1;


  result := error_number; exit;
end;

{ The Codabar system consisting of simple substitution }
//chaosben: some changes where made based on the article at http://en.wikipedia.org/wiki/Codabar}
function codabar(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  i, j, error_number : Integer;
  dest : TArrayOfChar;
  local_source : TArrayOfByte;
const
  CODABAR_DELIMITERS : array[0..7] of Char = ('A', 'B', 'C', 'D', 'T', 'N', '*', 'E');
begin
  SetLength(dest, 512);
  error_number := 0;
  strcpy(dest, '');

  SetLength(local_source, Length(source));
  ArrayCopy(local_source, source);

  if (_length > 60) then
  begin { No stack smashing please }
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  to_upper(local_source);

  //replace alternate delimiters
  for i := 0 to _length - 1 do
  begin
    for j := 4 to 7 do
      if local_source[i] = Ord(CODABAR_DELIMITERS[j]) then
        local_source[i] := Ord(CODABAR_DELIMITERS[j - 4]);
  end;

  error_number := is_sane(CALCIUM, local_source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  for i := 1 to _length - 2 do
  begin
    for j := Low(CODABAR_DELIMITERS) to High(CODABAR_DELIMITERS) do
    begin
      if local_source[i] = Ord(CODABAR_DELIMITERS[j]) then
      begin
        strcpy(symbol.errtxt, 'The character "' + Chr(source[i]) + '" can only be used as first and/or last character.');
        result := ZERROR_INVALID_DATA; exit;
      end;
    end;
  end;

  {if ((source[0] <> Ord('A')) and (source[0] <> Ord('B')) and (source[0] <> Ord('C')) and (source[0] <> Ord('D'))) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := ZERROR_INVALID_DATA; exit;
  end;

  if ((source[_length - 1] <> Ord('A')) and (source[_length - 1] <> Ord('B')) and
        (source[_length - 1] <> Ord('C')) and (source[_length - 1] <> Ord('D'))) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := ZERROR_INVALID_DATA; exit;
  end;}

  for i := 0 to _length - 1 do
    lookup(CALCIUM, CodaTable, local_source[i], dest);

  expand(symbol, dest);
  ustrcpy(symbol.text, source);
  result := error_number; exit;
end;

{ Italian Pharmacode }
function code32(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  i, zeroes, error_number, checksum, checkpart, checkdigit : Integer;
  localstr, risultante : TArrayOfChar;
  pharmacode, remainder, devisor : Integer;
  codeword : array[0..5] of Integer;
  tabella : TArrayOfChar;
begin
  SetLength(tabella, 34);

  { Validate the input }
  if (_length > 8) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(NEON, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  { Add leading zeros as required }
  zeroes := 8 - _length;
  SetLength(localstr, 10);
  Fill(localstr, zeroes, '0');
  SetLength(risultante, 7);
  localstr[zeroes]:=#0;
  concat(localstr, source);

  { Calculate the check digit }
  checksum := 0;
  checkpart := 0;
  for i := 0 to 3 do
  begin
    checkpart := StrToInt(localstr[i * 2]);
    Inc(checksum, checkpart);
    checkpart := 2 * (StrToInt(localstr[(i * 2) + 1]));
    if (checkpart >= 10) then
      Inc(checksum, (checkpart - 10) + 1)
    else
      Inc(checksum, checkpart);
  end;

  { Add check digit to data string }
  checkdigit := checksum mod 10;
  concat(localstr, IntToStr(checkdigit));

  { Convert string into an integer value }
  pharmacode := StrToIntDef(ArrayOfCharToString(localstr), 0);

  { Convert from decimal to base-32 }
  devisor := 33554432;
  for i := 5 downto 0 do
  begin
    codeword[i] := pharmacode div devisor;
    remainder := pharmacode mod devisor;
    pharmacode := remainder;
    devisor := devisor div 32;
  end;

  { Look up values in 'Tabella di conversione' }
  strcpy(tabella, '0123456789BCDFGHJKLMNPQRSTUVWXYZ');
  for i := 5 downto 0 do
    risultante[5 - i] := tabella[codeword[i]];
  risultante[6] := #0;

  { Plot the barcode using Code 39 }
  error_number := c39(symbol, ArrayOfCharToArrayOfByte(risultante), strlen(risultante));
  if (error_number <> 0) then begin result := error_number; exit; end;

  { Override the normal text output with the Pharmacode number }
  ustrcpy(symbol.text, 'A');
  uconcat(symbol.text, localstr);

  result := error_number; exit;
end;

end.

