unit zint_2of5;

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
  zint;

function matrix_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function industrial_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function iata_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function logic_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function interleaved_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function itf14(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function dpleit(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function dpident(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses zint_common, zint_helper;

const
  C25MatrixTable : array[0..9] of String = ('113311', '311131', '131131', '331111', '113131', '313111',
	'133111', '111331', '311311', '131311');

  C25IndustTable : array[0..9] of String = ('1111313111', '3111111131', '1131111131', '3131111111', '1111311131',
	'3111311111', '1131311111', '1111113131', '3111113111', '1131113111');

  C25InterTable : array[0..9] of String = ('11331', '31113', '13113', '33111', '11313', '31311', '13311', '11133',
	'31131', '13131');

function check_digit(count : Cardinal) : Char; inline;
begin
  Result := itoc((10 - (count mod 10)) mod 10);
end;

{ Code 2 of 5 Standard (Code 2 of 5 Matrix) }
function matrix_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  i, error_number : Integer;
  dest : TArrayOfChar; { 6 + 80 * 6 + 6 + 1 ~ 512 }
begin
  SetLength(dest, 512);
  //error_number := 0;

  if(_length > 80) then
  begin
		strcpy(symbol.errtxt, 'Input too long');
		result := ZERROR_TOO_LONG; exit;
	end;

	error_number := is_sane(NEON, source, _length);
	if(error_number = ZERROR_INVALID_DATA) then
  begin
		strcpy(symbol.errtxt, 'Invalid characters in data');
		result := error_number; exit;
  end;

	{ start character }
	strcpy(dest, '411111');

	for i := 0 to _length - 1 do
  begin
		lookup(NEON, C25MatrixTable, source[i], dest);
  end;

	{ Stop character }
	concat(dest, '41111');

	expand(symbol, dest);
	ustrcpy(symbol.text, source);
	result := error_number; exit;
end;

{ Code 2 of 5 Industrial }
function industrial_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number : Integer;
  dest : TArrayOfChar; { 6 + 40 * 10 + 6 + 1 }
  i : Integer;
begin
  SetLength(dest, 512);
  //error_number := 0;

  if (_length > 45) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(NEON, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid character in data');
    result := error_number; exit;
  end;

  { start character }
  strcpy(dest, '313111');

  for i := 0 to _length - 1 do
    lookup(NEON, C25IndustTable, source[i], dest);

  { Stop character }
  concat(dest, '31113');

  expand(symbol, dest);
  ustrcpy(symbol.text, source);
  result := error_number; exit;
end;

{ Code 2 of 5 IATA }
function iata_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number : Integer;
  dest : TArrayOfChar; { 4 + 45 * 10 + 3 + 1 }
  i : Integer;
begin
  SetLength(dest, 512);
  //error_number := 0;

  if (_length > 45) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result :=  ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(NEON, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  { start }
  strcpy(dest, '1111');

  for i := 0 to _length - 1 do
    lookup(NEON, C25IndustTable, source[i], dest);

  { stop }
  concat(dest, '311');

  expand(symbol, dest);
  ustrcpy(symbol.text, source);
  result := error_number; exit;
end;

{ Code 2 of 5 Data Logic }
function logic_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number : Integer;
  dest : TArrayOfChar; { 4 + 80 * 6 + 3 + 1 }
  i : Integer;
begin
  SetLength(dest, 512);
  //error_number := 0;

  if (_length > 80) then
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

  { start character }
  strcpy(dest, '1111');

  for i := 0 to _length - 1 do
    lookup(NEON, C25MatrixTable, source[i], dest);

  { Stop character }
  concat (dest, '311');

  expand(symbol, dest);
  ustrcpy(symbol.text, source);
  result := error_number; exit;
end;

{ Code 2 of 5 Interleaved }
function interleaved_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number : Integer;
  bars, spaces, mixed, dest : TArrayOfChar;
  temp : TArrayOfByte;
  i, j, k : Integer;
begin
  SetLength(bars, 7); SetLength(spaces, 7); SetLength(mixed, 14); SetLength(dest, 1000);
  SetLength(temp, _length + 2);
  //error_number := 0;

  if (_length > 89) then
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

  ustrcpy(temp, '');
  { Input must be an even number of characters for Interlaced 2 of 5 to work:
     if an odd number of characters has been entered then add a leading zero }
  if (_length and 1) <> 0 then
  begin
    ustrcpy(temp, '0');
    Inc(_length);
  end;
  uconcat(temp, source);

  { start character }
  strcpy(dest, '1111');

  i := 0;
  while i < _length do
  begin
    { look up the bars and the spaces and put them in two strings }
    strcpy(bars, '');
    lookup(NEON, C25InterTable, temp[i], bars);
    strcpy(spaces, '');
    lookup(NEON, C25InterTable, temp[i + 1], spaces);

    k := 0;
    { then merge (interlace) the strings together }
    for j := 0 to 4 do
    begin
      mixed[k] := bars[j]; Inc(k);
      mixed[k] := spaces[j]; Inc(k);
    end;
    mixed[k] := #0;
    concat(dest, mixed);
    Inc(i, 2);
  end;

  { Stop character }
  concat (dest, '311');

  expand(symbol, dest);
  ustrcpy(symbol.text, temp);
  result := error_number; exit;
end;

function itf14(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number, zeroes : Integer;
  count : Cardinal;
  localstr : TArrayOfChar;
  i : Integer;
begin
  SetLength(localstr, 16);
  //error_number := 0;

  count := 0;

  if (_length > 13) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  error_number := is_sane(NEON, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid character in data');
    result := error_number; exit;
  end;

  { Add leading zeros as required }
  zeroes := 13 - _length;
  for i := 0 to zeroes - 1 do
    localstr[i] := '0';
  localstr[zeroes] := #0;
  concat(localstr, source);

  { Calculate the check digit - the same method used for EAN-13 }

  for i := 12 downto 0 do
  begin
    Inc(count, ctoi(localstr[i]));

    if (i and 1) = 0 then
      Inc(count, 2 * ctoi(localstr[i]));
  end;

  localstr[13] := check_digit(count);
	localstr[14] := #0;
  error_number := interleaved_two_of_five(symbol, ArrayOfCharToArrayOfByte(localstr), strlen(localstr));
  ustrcpy(symbol.text, ArrayOfCharToString(localstr));
  result := error_number; exit;
end;

{ Deutsche Post Leitcode }
function dpleit(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number : Integer;
  count : Cardinal;
  localstr : TArrayOfChar;
  zeroes : Integer;
  i : Integer;
begin
  SetLength(localstr, 16);
  //error_number := 0;
  count := 0;
  if (_length > 13) then
  begin
    strcpy(symbol.errtxt, 'Input wrong _length');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(NEON, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  zeroes := 13 - _length;
  for i := 0 to zeroes - 1 do
    localstr[i] := '0';
  localstr[zeroes] := #0;
  concat(localstr, source);

  for i := 12 downto 0 do
  begin
    Inc(count, 4 * ctoi(localstr[i]));

    if (i and 1) <> 0 then
      Inc(count, 5 * ctoi(localstr[i]));
  end;

  localstr[13] := check_digit(count);
  localstr[14] := #0;
  error_number := interleaved_two_of_five(symbol, ArrayOfCharToArrayOfByte(localstr), strlen(localstr));
  ustrcpy(symbol.text, ArrayOfCharToString(localstr));
  result := error_number; exit;
end;

{ Deutsche Post Identcode }
function dpident(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number, zeroes : Integer;
  count : Cardinal;
  localstr : TArrayOfChar;
  i : Integer;
begin
  SetLength(localstr, 16);
  count := 0;
  if (_length > 11) then
  begin
    strcpy(symbol.errtxt, 'Input wrong _length');
    result := ZERROR_TOO_LONG; exit;
  end;

  error_number := is_sane(NEON, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
    begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    //result := error_number;
  end;

  zeroes := 11 - _length;
  for i := 0 to zeroes - 1 do
    localstr[i] := '0';
  localstr[zeroes] := #0;
  concat(localstr, source);

  for i := 10 downto 0 do
  begin
    Inc(count, 4 * ctoi(localstr[i]));

    if (i and 1) <> 0 then
      Inc(count, 5 * ctoi(localstr[i]));
  end;

  localstr[11] := check_digit(count);
  localstr[12] := #0;
  error_number := interleaved_two_of_five(symbol, ArrayOfCharToArrayOfByte(localstr), strlen(localstr));
  ustrcpy(symbol.text, ArrayOfCharToString(localstr));
  result := error_number; exit;
end;

end.

