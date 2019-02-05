unit zint_postal;

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

function post_plot(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
function planet_plot(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
function korea_post(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
function fim(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
function royal_plot(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function kix_code(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function daft_code(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function flattermarken(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
function japan_post(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses zint_common, zint_helper;

const DAFTSET = 'DAFT';
const KRSET = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
const KASUTSET = '1234567890-abcdefgh';
const CHKASUTSET = '0123456789-abcdefgh';
const SHKASUTSET = '1234567890-ABCDEFGHIJKLMNOPQRSTUVWXYZ';

{ PostNet number encoding table - In this table L is long as S is short }
const PNTable : array[0..9] of String = ('LLSSS', 'SSSLL', 'SSLSL', 'SSLLS', 'SLSSL', 'SLSLS', 'SLLSS', 'LSSSL',
	'LSSLS', 'LSLSS');
const PLTable : array[0..9] of String = ('SSLLL', 'LLLSS', 'LLSLS', 'LLSSL', 'LSLLS', 'LSLSL', 'LSSLL', 'SLLLS',
	'SLLSL', 'SLSLL');

const RoyalValues : array[0..35] of String = ('11', '12', '13', '14', '15', '10', '21', '22', '23', '24', '25',
	'20', '31', '32', '33', '34', '35', '30', '41', '42', '43', '44', '45', '40', '51', '52',
	'53', '54', '55', '50', '01', '02', '03', '04', '05', '00');

{ 0 = Full, 1 = Ascender, 2 = Descender, 3 = Tracker }
const RoyalTable : array[0..35] of String = ('3300', '3210', '3201', '2310', '2301', '2211', '3120', '3030', '3021',
	'2130', '2121', '2031', '3102', '3012', '3003', '2112', '2103', '2013', '1320', '1230',
	'1221', '0330', '0321', '0231', '1302', '1212', '1203', '0312', '0303', '0213', '1122',
	'1032', '1023', '0132', '0123', '0033');

const FlatTable : array[0..9] of String = ('0504', '18', '0117', '0216', '0315', '0414', '0513', '0612', '0711',
	'0810');

const KoreaTable : array[0..9] of String = ('1313150613', '0713131313', '0417131313', '1506131313',
	'0413171313', '17171313', '1315061313', '0413131713', '17131713', '13171713');

const JapanTable : array[0..18] of String = ('114', '132', '312', '123', '141', '321', '213', '231', '411', '144',
	'414', '324', '342', '234', '432', '243', '423', '441', '111');

{ Handles the PostNet system used for Zip codes in the US }
function postnet(symbol : zint_symbol; const source : TArrayOfByte; var dest : TArrayOfChar; _length : Integer) : Integer;
var
  i, sum, check_digit : Integer;
  error_number : Integer;
begin
  //error_number := 0;

  if (_length > 38) then
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
  sum := 0;

  { start character }
  strcpy(dest, 'L');

  for i := 0 to _length - 1 do
  begin
    lookup(NEON, PNTable, source[i], dest);
    Inc(sum, ctoi(Chr(source[i])));
  end;

  check_digit := (10 - (sum mod 10)) mod 10;
  concat(dest, PNTable[check_digit]);

  { stop character }
  concat(dest, 'L');

  result := error_number; exit;
end;

{ Puts PostNet barcodes into the pattern matrix }
function post_plot(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  height_pattern : TArrayOfChar; { 5 + 38 * 5 + 5 + 5 +  1 ~ 256 }
  loopey, h : Integer;
  writer : Integer;
  error_number : Integer;
begin
  SetLength(height_pattern, 256);
  //error_number := 0;

  error_number := postnet(symbol, source, height_pattern, _length);
  if (error_number <> 0) then
  begin
    result := error_number; exit;
  end;

  writer := 0;
  h := strlen(height_pattern);
  for loopey := 0 to h - 1 do
  begin
    if (height_pattern[loopey] = 'L') then
      set_module(symbol, 0, writer);
    set_module(symbol, 1, writer);
    Inc(writer, 3);
  end;
  symbol.row_height[0] := 6;
  symbol.row_height[1] := 6;
  symbol.rows := 2;
  symbol.width := writer - 1;

  result := error_number; exit;
end;

{ Handles the PLANET  system used for item tracking in the US }
function planet(symbol : zint_symbol; const source : TArrayOfByte; var dest : TArrayOfChar; _length : Integer) : Integer;
var
  i, sum, check_digit : Integer;
  error_number : Integer;
begin
  //error_number := 0;

  if (_length > 38) then
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
  sum := 0;

  { start character }
  strcpy(dest, 'L');

  for i := 0 to _length - 1 do
  begin
    lookup(NEON, PLTable, source[i], dest);
    Inc(sum, ctoi(Chr(source[i])));
  end;

  check_digit := (10 - (sum mod 10)) mod 10;
  concat(dest, PLTable[check_digit]);

  { stop character }
  concat(dest, 'L');

  result := error_number; exit;
end;

{ Puts PLANET barcodes into the pattern matrix }
function planet_plot(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  height_pattern : TArrayOfChar; { 5 + 38 * 5 + 5 + 5 +  1 ~ 256 }
  loopey, h : Integer;
  writer : Integer;
  error_number : Integer;
begin
  SetLength(height_pattern, 256);
  //error_number := 0;

  error_number := planet(symbol, source, height_pattern, _length);
  if (error_number <> 0) then
  begin
    result := error_number; exit;
  end;

  writer := 0;
  h := strlen(height_pattern);
  for loopey := 0 to h - 1 do
  begin
    if (height_pattern[loopey] = 'L') then
      set_module(symbol, 0, writer);
    set_module(symbol, 1, writer);
    Inc(writer, 3);
  end;
  symbol.row_height[0] := 6;
  symbol.row_height[1] := 6;
  symbol.rows := 2;
  symbol.width := writer - 1;
  result := error_number; exit;
end;

{ Korean Postal Authority }
function korea_post(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  total, loop, check, zeroes, error_number : Integer;
  localstr, dest : TArrayOfChar;
begin
  SetLength(localstr, 8);
  SetLength(dest, 80);
  //error_number := 0;
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
  zeroes := 6 - _length;
  for loop := 0 to zeroes - 1 do
    localstr[loop] := '0';
  localstr[zeroes] := #0;
  concat(localstr, source);

  total := 0;
  for loop := 0 to 5 do
    Inc(total, ctoi(localstr[loop]));

  check := 10 - (total mod 10);
  if (check = 10) then check := 0;
  localstr[6] := itoc(check);
  localstr[7] := #0;
  dest[0] := #0;
  for loop := 5 downto 0 do
    lookup(NEON, KoreaTable, localstr[loop], dest);

  lookup(NEON, KoreaTable, localstr[6], dest);
  expand(symbol, dest);
  ustrcpy(symbol.text, ArrayOfCharToArrayOfByte(localstr));
  result := error_number; exit;
end;

{ The simplest barcode symbology ever! Supported by MS Word, so here it is! }
{ glyphs from http://en.wikipedia.org/wiki/Facing_Identification_Mark }
function fim(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  dest : TArrayOfChar;
begin
  SetLength(dest, 16);
  Fill(dest, Length(dest), Chr(0));

  if (_length > 1) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  case Chr(source[0]) of
    'a',
    'A': strcpy(dest, '111515111');
    'b',
    'B': strcpy(dest, '13111311131');
    'c',
    'C': strcpy(dest, '11131313111');
    'd',
    'D': strcpy(dest, '1111131311111');
    else
    begin
      strcpy(symbol.errtxt, 'Invalid characters in data');
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  expand(symbol, dest);
  result := 0; exit;
end;

{ Handles the 4 State barcodes used in the UK by Royal Mail }
function rm4scc(const source : TArrayOfByte; var dest : TArrayOfChar; _length : Integer) : Char;
var
  i : Integer;
  top, bottom, row, column, check_digit : Integer;
  values : TArrayOfChar;
begin
  SetLength(values, 3);
  top := 0;
  bottom := 0;

  { start character }
  strcpy(dest, '1');

  for i := 0 to _length - 1 do
  begin
    lookup(KRSET, RoyalTable, source[i], dest);
    strcpy(values, RoyalValues[posn(KRSET, source[i])]);
    Inc(top, ctoi(values[0]));
    Inc(bottom, ctoi(values[1]));
  end;

  { Calculate the check digit }
  row := (top mod 6) - 1;
  column := (bottom mod 6) - 1;
  if (row = -1) then row := 5;
  if (column = -1) then column := 5;
  check_digit := (6 * row) + column;
  concat(dest, RoyalTable[check_digit]);

  { stop character }
  concat(dest, '0');

  result := StrToArrayOfChar(KRSET)[check_digit];
end;

{ Puts RM4SCC into the data matrix }
function royal_plot(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  height_pattern : TArrayOfChar;
  loopey, h : Integer;
  writer : Integer;
  error_number : Integer;
begin
  SetLength(height_pattern, 200);
  strcpy(height_pattern, '');
  //error_number := 0;

  if (_length > 120) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  to_upper(source);
  error_number := is_sane(KRSET, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  rm4scc(source, height_pattern, _length);

  writer := 0;
  h := strlen(height_pattern);
  for loopey := 0 to h - 1 do
  begin
    if ((height_pattern[loopey] = '1') or (height_pattern[loopey] = '0')) then
      set_module(symbol, 0, writer);
    set_module(symbol, 1, writer);
    if ((height_pattern[loopey] = '2') or (height_pattern[loopey] = '0')) then
      set_module(symbol, 2, writer);
    Inc(writer, 2);
  end;

  symbol.row_height[0] := 3;
  symbol.row_height[1] := 2;
  symbol.row_height[2] := 3;
  symbol.rows := 3;
  symbol.width := writer - 1;

  result := error_number; exit;
end;

{ Handles Dutch Post TNT KIX symbols }
{ The same as RM4SCC but without check digit }
{ Specification at http://www.tntpost.nl/zakelijk/klantenservice/downloads/kIX_code/download.aspx }
function kix_code(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  height_pattern, localstr : TArrayOfChar;
  loopey : Integer;
  writer, i, h : Integer;
  error_number : Integer;
begin
  SetLength(height_pattern, 50);
  SetLength(localstr, 20);
  strcpy(height_pattern, '');

  //error_number := 0;

  if (_length > 18) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  to_upper(source);
  error_number := is_sane(KRSET, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  strcpy(localstr, ArrayOfByteToArrayOfChar(source));

  { Encode data }
  for i := 0 to 17 do
    lookup(KRSET, RoyalTable, localstr[i], height_pattern);

  writer := 0;
  h := strlen(height_pattern);
  for loopey := 0 to h - 1 do
  begin
    if ((height_pattern[loopey] = '1') or (height_pattern[loopey] = '0')) then
      set_module(symbol, 0, writer);
    set_module(symbol, 1, writer);
    if ((height_pattern[loopey] = '2') or (height_pattern[loopey] = '0')) then
      set_module(symbol, 2, writer);
    Inc(writer, 2);
  end;

  symbol.row_height[0] := 3;
  symbol.row_height[1] := 2;
  symbol.row_height[2] := 3;
  symbol.rows := 3;
  symbol.width := writer - 1;

  result := error_number; exit;
end;

{ Handles DAFT Code symbols }
{ Presumably 'daft' doesn't mean the same thing in Germany as it does in the UK! }
function daft_code(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  height_pattern : TArrayOfChar;
  loopey, h : Integer;
  writer, i, error_number : Integer;
begin
  SetLength(height_pattern, 100);
  strcpy(height_pattern, '');

  //error_number := 0;
  if (_length > 50) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  to_upper(source);
  error_number := is_sane(DAFTSET, source, _length);

  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  for i := 0 to _length - 1 do
  begin
    if (source[i] = Ord('D')) then concat(height_pattern, '2');
    if (source[i] = Ord('A')) then concat(height_pattern, '1');
    if (source[i] = Ord('F')) then concat(height_pattern, '0');
    if (source[i] = Ord('T')) then concat(height_pattern, '3');
  end;

  writer := 0;
  h := strlen(height_pattern);
  for loopey := 0 to h - 1 do
  begin
    if ((height_pattern[loopey] = '1') or (height_pattern[loopey] = '0')) then
      set_module(symbol, 0, writer);
    set_module(symbol, 1, writer);
    if ((height_pattern[loopey] = '2') or (height_pattern[loopey] = '0')) then
      set_module(symbol, 2, writer);
    Inc(writer, 2);
  end;

  symbol.row_height[0] := 3;
  symbol.row_height[1] := 2;
  symbol.row_height[2] := 3;
  symbol.rows := 3;
  symbol.width := writer - 1;

  result := error_number; exit;
end;

{ Flattermarken - Not really a barcode symbology and (in my opinion) probably not much use
  but it's supported by TBarCode so it's supported by Zint! }
function flattermarken(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  loop, error_number : Integer;
  dest : TArrayOfChar; { 90 * 4 + 1 ~ }
begin
  SetLength(dest, 512);
  //error_number := 0;

  if (_length > 90) then
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
  dest[0] := #0;
  for loop := 0 to _length - 1 do
    lookup(NEON, FlatTable, source[loop], dest);

  expand(symbol, dest);
  result := error_number; exit;
end;

{ Japanese Postal Code (Kasutama Barcode) }
function japan_post(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number, h : Integer;
  pattern : TArrayOfChar;
  writer, loopey, inter_posn, i, sum, check : Integer;
  check_char : Char;
  inter : TArrayOfChar;
  local_source : TArrayOfByte;
begin
  SetLength(pattern, 69);
  SetLength(inter, 23);
  SetLength(local_source, _length + 1);

  //inter_posn := 0;
  //error_number := 0;

  ustrcpy(local_source, source);
  for i := 0 to _length - 1 do
    local_source[i] := source[i];
  to_upper(local_source);
  error_number := is_sane(SHKASUTSET, local_source, _length);

  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;
  Fill(inter, 20, 'd');{ Pad character CC4 }
  inter[20] := #0;

  i := 0;
  inter_posn := 0;
  repeat
    if (((local_source[i] >= Ord('0')) and (local_source[i] <= Ord('9'))) or (local_source[i] = Ord('-'))) then
    begin
      inter[inter_posn] := Chr(local_source[i]);
      Inc(inter_posn);
    end
    else
    begin
      if ((local_source[i] >= Ord('A')) and (local_source[i] <= Ord('J'))) then
      begin
        inter[inter_posn] := 'a';
        inter[inter_posn + 1] := Chr(local_source[i] - Ord('A') + Ord('0'));
        Inc(inter_posn, 2);
      end;
      if ((local_source[i] >= Ord('K')) and (local_source[i] <= Ord('T'))) then
      begin
        inter[inter_posn] := 'b';
        inter[inter_posn + 1] := Chr(local_source[i] - Ord('K') + Ord('0'));
        Inc(inter_posn, 2);
      end;
      if ((local_source[i] >= Ord('U')) and (local_source[i] <= Ord('Z'))) then
      begin
        inter[inter_posn] := 'c';
        inter[inter_posn + 1] := Chr(local_source[i] - Ord('U') + Ord('0'));
        Inc(inter_posn, 2);
      end;
    end;
    Inc(i);
  until not ((i < _length) and (inter_posn < 20));
  inter[20] := #0;

  strcpy(pattern, '13'); { Start }

  sum := 0;
  for i := 0 to 19 do
  begin
    concat(pattern, JapanTable[posn(KASUTSET, inter[i])]);
    Inc(sum, posn(CHKASUTSET, inter[i]));
  end;

  { Calculate check digit }
  check := 19 - (sum mod 19);
  if (check = 19) then check := 0;
  if (check <= 9) then check_char := Chr(check + Ord('0'));
  if (check = 10) then check_char := '-';
  if (check >= 11) then check_char := Chr((check - 11) + Ord('a'));
  concat(pattern, JapanTable[posn(KASUTSET, check_char)]);

  concat(pattern, '31'); { Stop }

  { Resolve pattern to 4-state symbols }
  writer := 0;
  h := strlen(pattern);
  for loopey := 0 to h - 1 do
  begin
    if ((pattern[loopey] = '2') or (pattern[loopey] = '1')) then
      set_module(symbol, 0, writer);
    set_module(symbol, 1, writer);
    if ((pattern[loopey] = '3') or (pattern[loopey] = '1')) then
      set_module(symbol, 2, writer);
    Inc(writer, 2);
  end;

  symbol.row_height[0] := 3;
  symbol.row_height[1] := 2;
  symbol.row_height[2] := 3;
  symbol.rows := 3;
  symbol.width := writer - 1;

  result := error_number; exit;
end;

end.

