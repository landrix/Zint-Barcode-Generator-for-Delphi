unit zint_telepen;

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

function telepen(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;
function telepen_num(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;

implementation

uses zint_common, zint_helper;

const SODIUM = '0123456789X';

const TeleTable : array[0..126] of String =
(
	'1111111111111111',	'1131313111',	'33313111',	'1111313131',	'3111313111',	'11333131',	'13133131',	'111111313111',
	'31333111',		'1131113131',	'33113131',	'1111333111',	'3111113131',	'1113133111',	'1311133111',	'111111113131',
	'3131113111',		'11313331',	'333331',	'111131113111',	'31113331',	'1133113111',	'1313113111',	'1111113331',
	'31131331',		'113111113111',	'3311113111',	'1111131331',	'311111113111',	'1113111331',	'1311111331',	'11111111113111',
	'31313311',		'1131311131',	'33311131',	'1111313311',	'3111311131',	'11333311',	'13133311',	'111111311131',
	'31331131',		'1131113311',	'33113311',	'1111331131',	'3111113311',	'1113131131',	'1311131131',	'111111113311',
	'3131111131',		'1131131311',	'33131311',	'111131111131',	'3111131311',	'1133111131',	'1313111131',	'111111131311',
	'3113111311',		'113111111131', '3311111131',	'111113111311',	'311111111131',	'111311111311', '131111111311',	'11111111111131',
	'3131311111',		'11313133',	'333133',	'111131311111',	'31113133',	'1133311111',	'1313311111',	'1111113133',
	'313333',		'113111311111',	'3311311111',	'11113333',	'311111311111',	'11131333',	'13111333',	'11111111311111',
	'31311133',		'1131331111',	'33331111', '	1111311133',	'3111331111',	'11331133',	'13131133',	'111111331111',
	'3113131111',		'1131111133',	'33111133',	'111113131111', '3111111133',	'111311131111', '131111131111', '111111111133',
	'31311313',		'113131111111', '3331111111',	'1111311313',	'311131111111', '11331313',	'13131313',	'11111131111111',
	'3133111111',		'1131111313',	'33111313',	'111133111111', '3111111313',	'111313111111', '131113111111', '111111111313',
	'313111111111',		'1131131113',	'33131113',	'11113111111111','3111131113',	'113311111111', '131311111111', '111111131113',
	'3113111113',		'11311111111111','331111111111','111113111113', '31111111111111','111311111113','131111111113');

function telepen(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;
var
  i, count, check_digit : Integer;
  error_number : Integer;
  dest : TArrayOfChar; {14 + 30 * 14 + 14 + 14 + 1 ~ 512 }
begin
  SetLength(dest, 512);
  error_number := 0;

  count := 0;

  if (src_len > 30) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  { Start character }
  strcpy(dest, TeleTable[Ord('_')]);

  for i := 0 to src_len - 1 do
  begin
    if (source[i] > 126) then
    begin
      { Cannot encode extended ASCII }
      strcpy(symbol.errtxt, 'Invalid characters in input data');
      //result := ZERROR_INVALID_DATA;
    end;
    concat(dest, TeleTable[source[i]]);
    Inc(count, source[i]);
  end;

  check_digit := 127 - (count mod 127);
  if (check_digit = 127) then check_digit := 0;
  concat(dest, TeleTable[check_digit]);

  { Stop character }
  concat(dest, TeleTable[Ord('z')]);

  expand(symbol, dest);
  for i := 0 to src_len - 1 do
  begin
    if (source[i] = 0) then
      symbol.text[i] := Ord(' ')
    else
      symbol.text[i] := source[i];
  end;
  symbol.text[src_len] := 0;
  result := error_number;
  exit;
end;

function telepen_num(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;
var
  i, count, check_digit, glyph : Integer;
  error_number, temp_length : Integer;
  dest : TArrayOfChar;
  temp : TArrayOfByte;
begin
  temp_length := src_len;
  SetLength(dest, 1024);
  SetLength(temp, 64);
  //error_number := 0;
  count := 0;

  if (temp_length > 60) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  ustrcpy(temp, source);
  to_upper(temp);
  error_number := is_sane(NEON, temp, temp_length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  { Add a leading zero if required }
  if (temp_length and 1) <> 0 then
  begin
    for i := temp_length - 1 downto 1 do
      temp[i] := temp[i - 1];
    temp[0] := Ord('0');

    Inc(temp_length);
    temp[temp_length] := 0;
  end;

  { Start character }
  strcpy(dest, TeleTable[Ord('_')]);

  i := 0;
  while i < temp_length do
  begin
    if (temp[i] = Ord('X')) then
    begin
      strcpy(symbol.errtxt, 'Invalid position of X in Telepen data');
      result := ZERROR_INVALID_DATA; exit;
    end;

    if (temp[i + 1] = Ord('X')) then
    begin
      glyph := ctoi(Chr(temp[i])) + 17;
      Inc(count, glyph);
    end
    else
    begin
      glyph := (10 * ctoi(Chr(temp[i]))) + ctoi(Chr(temp[i + 1]));
      Inc(glyph, 27);
      Inc(count, glyph);
    end;
    concat(dest, TeleTable[glyph]);
    Inc(i, 2);
  end;

  check_digit := 127 - (count mod 127);
  if (check_digit = 127) then check_digit := 0;
  concat(dest, TeleTable[check_digit]);

  { Stop character }
  concat(dest, TeleTable[Ord('z')]);

  expand(symbol, dest);
  ustrcpy(symbol.text, temp);
  result := error_number; exit;
end;


end.

