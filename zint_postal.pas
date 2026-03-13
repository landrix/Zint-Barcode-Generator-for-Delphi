unit zint_postal;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    b3a3c0d updated to Zint 2.16.0.9 (2026-03-13) - all functions
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

const DAFTSET = 'FADT';
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
{ Also handles Brazilian CEPNet }
function postnet(symbol : zint_symbol; const source : TArrayOfByte; var dest : TArrayOfChar; _length : Integer) : Integer;
var
  i, sum, check_digit : Integer;
  error_number : Integer;
begin
  error_number := 0;

  if (_length > 38) then
  begin
    strcpy(symbol.errtxt, Format('Error 480: Input length %d too long (maximum 38)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  if (symbol.symbology = BARCODE_CEPNET) then
  begin
    if (_length <> 8) then
    begin
      strcpy(symbol.errtxt, Format('Warning 780: Input length %d wrong (should be 8 digits)', [_length]));
      error_number := ZWARN_NONCOMPLIANT;
    end;
  end
  else
  begin
    if (_length <> 5) and (_length <> 9) and (_length <> 11) then
    begin
      strcpy(symbol.errtxt, Format('Warning 479: Input length %d is not standard (should be 5, 9 or 11 digits)', [_length]));
      error_number := ZWARN_NONCOMPLIANT;
    end;
  end;

  for i := 0 to _length - 1 do
  begin
    if (source[i] < Ord('0')) or (source[i] > Ord('9')) then
    begin
      strcpy(symbol.errtxt, Format('Error 481: Invalid character at position %d in input (digits only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  sum := 0;

  { Start character }
  strcpy(dest, 'L');

  for i := 0 to _length - 1 do
  begin
    lookup(NEON, PNTable, source[i], dest);
    Inc(sum, ctoi(Chr(source[i])));
  end;

  check_digit := (10 - (sum mod 10)) mod 10;
  concat(dest, PNTable[check_digit]);

  { Stop character }
  concat(dest, 'L');

  result := error_number;
end;

{ Puts PostNet barcodes into the pattern matrix }
function post_plot(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  height_pattern : TArrayOfChar;
  loopey, h : Integer;
  writer : Integer;
  error_number : Integer;
begin
  SetLength(height_pattern, 256);

  error_number := postnet(symbol, source, height_pattern, _length);
  if (error_number >= ZERROR_TOO_LONG) then
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
    Inc(writer, 2);
  end;
  symbol.row_height[0] := 6;
  symbol.row_height[1] := 6;
  symbol.rows := 2;
  symbol.width := writer - 1;

  result := error_number;
end;

{ Handles the PLANET system used for item tracking in the US }
function planet(symbol : zint_symbol; const source : TArrayOfByte; var dest : TArrayOfChar; _length : Integer) : Integer;
var
  i, sum, check_digit : Integer;
  error_number : Integer;
begin
  error_number := 0;

  if (_length > 38) then
  begin
    strcpy(symbol.errtxt, Format('Error 482: Input length %d too long (maximum 38)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  if (_length <> 11) and (_length <> 13) then
  begin
    strcpy(symbol.errtxt, Format('Warning 478: Input length %d is not standard (should be 11 or 13 digits)', [_length]));
    error_number := ZWARN_NONCOMPLIANT;
  end;

  for i := 0 to _length - 1 do
  begin
    if (source[i] < Ord('0')) or (source[i] > Ord('9')) then
    begin
      strcpy(symbol.errtxt, Format('Error 483: Invalid character at position %d in input (digits only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  sum := 0;

  { Start character }
  strcpy(dest, 'L');

  for i := 0 to _length - 1 do
  begin
    lookup(NEON, PLTable, source[i], dest);
    Inc(sum, ctoi(Chr(source[i])));
  end;

  check_digit := (10 - (sum mod 10)) mod 10;
  concat(dest, PLTable[check_digit]);

  { Stop character }
  concat(dest, 'L');

  result := error_number;
end;

{ Puts PLANET barcodes into the pattern matrix }
function planet_plot(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  height_pattern : TArrayOfChar;
  loopey, h : Integer;
  writer : Integer;
  error_number : Integer;
begin
  SetLength(height_pattern, 256);

  error_number := planet(symbol, source, height_pattern, _length);
  if (error_number >= ZERROR_TOO_LONG) then
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
    Inc(writer, 2);
  end;
  symbol.row_height[0] := 6;
  symbol.row_height[1] := 6;
  symbol.rows := 2;
  symbol.width := writer - 1;

  result := error_number;
end;

{ Korean Postal Authority }
function korea_post(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  total, i, check, zeroes, error_number : Integer;
  local_source : TArrayOfByte;
  dest : TArrayOfChar;
  d : Integer;
  posns : array[0..5] of Integer;
begin
  SetLength(local_source, 8);
  SetLength(dest, 80);
  error_number := 0;

  if (_length > 6) then
  begin
    strcpy(symbol.errtxt, Format('Error 484: Input length %d too long (maximum 6)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  for i := 0 to _length - 1 do
  begin
    if (source[i] < Ord('0')) or (source[i] > Ord('9')) then
    begin
      strcpy(symbol.errtxt, Format('Error 485: Invalid character at position %d in input (digits only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  zeroes := 6 - _length;
  for i := 0 to zeroes - 1 do
    local_source[i] := Ord('0');
  for i := 0 to _length - 1 do
    local_source[zeroes + i] := source[i];

  total := 0;
  for i := 0 to 5 do
  begin
    posns[i] := local_source[i] - Ord('0');
    Inc(total, posns[i]);
  end;

  check := 10 - (total mod 10);
  if (check = 10) then check := 0;
  local_source[6] := Ord(itoc(check));

  d := 0;
  for i := 5 downto 0 do
  begin
    lookup(NEON, KoreaTable, local_source[i], dest);
    d := strlen(dest);
  end;
  lookup(NEON, KoreaTable, local_source[6], dest);

  expand(symbol, dest);

  { Set HRT }
  local_source[7] := 0;
  ustrcpy(symbol.text, local_source);

  result := error_number;
end;

{ The simplest barcode symbology ever! Supported by MS Word, so here it is! }
{ glyphs from http://en.wikipedia.org/wiki/Facing_Identification_Mark }
function fim(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number : Integer;
  ch : Char;
begin
  error_number := 0;

  if (_length > 1) then
  begin
    strcpy(symbol.errtxt, Format('Error 486: Input length %d too long (maximum 1)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  ch := UpCase(Chr(source[0]));

  case ch of
    'A': expand(symbol, StrToArrayOfChar('111515111'));
    'B': expand(symbol, StrToArrayOfChar('13111311131'));
    'C': expand(symbol, StrToArrayOfChar('11131313111'));
    'D': expand(symbol, StrToArrayOfChar('1111131311111'));
    'E': expand(symbol, StrToArrayOfChar('1317131'));
    else
    begin
      strcpy(symbol.errtxt, 'Error 487: Invalid character in input ("A", "B", "C", "D" or "E" only)');
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  result := error_number;
end;

{ Handles the 4 State barcodes used in the UK by Royal Mail }
function rm4scc(const source : TArrayOfByte; var dest : TArrayOfChar; _length : Integer) : Char;
var
  i : Integer;
  top, bottom, row, column, check_digit : Integer;
  p : Integer;
begin
  top := 0;
  bottom := 0;

  { Start character }
  strcpy(dest, '1');

  for i := 0 to _length - 1 do
  begin
    p := posn(KRSET, Chr(source[i]));
    lookup(KRSET, RoyalTable, source[i], dest);
    Inc(top, Ord(RoyalValues[p][1]) - Ord('0'));
    Inc(bottom, Ord(RoyalValues[p][2]) - Ord('0'));
  end;

  { Calculate the check digit }
  row := (top mod 6) - 1;
  column := (bottom mod 6) - 1;
  if (row = -1) then row := 5;
  if (column = -1) then column := 5;
  check_digit := (6 * row) + column;
  concat(dest, RoyalTable[check_digit]);

  { Stop character }
  concat(dest, '0');

  result := KRSET[check_digit + 1]; { 1-based string index }
end;

{ Puts RM4SCC into the data matrix }
function royal_plot(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  height_pattern : TArrayOfChar;
  loopey, h : Integer;
  writer : Integer;
  error_number : Integer;
  i : Integer;
begin
  SetLength(height_pattern, 210);
  strcpy(height_pattern, '');
  error_number := 0;

  if (_length > 50) then
  begin
    strcpy(symbol.errtxt, Format('Error 488: Input length %d too long (maximum 50)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;
  to_upper(source);
  for i := 0 to _length - 1 do
  begin
    if posn(KRSET, Chr(source[i])) = -1 then
    begin
      strcpy(symbol.errtxt, Format('Error 489: Invalid character at position %d in input (alphanumerics only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
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

  result := error_number;
end;

{ Handles Dutch Post TNT KIX symbols }
{ The same as RM4SCC but without check digit or stop/start chars }
function kix_code(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  height_pattern : TArrayOfChar;
  loopey : Integer;
  writer, i, h : Integer;
  error_number : Integer;
begin
  SetLength(height_pattern, 75);
  strcpy(height_pattern, '');
  error_number := 0;

  if (_length > 18) then
  begin
    strcpy(symbol.errtxt, Format('Error 490: Input length %d too long (maximum 18)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;
  to_upper(source);
  for i := 0 to _length - 1 do
  begin
    if posn(KRSET, Chr(source[i])) = -1 then
    begin
      strcpy(symbol.errtxt, Format('Error 491: Invalid character at position %d in input (alphanumerics only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  { Encode data }
  for i := 0 to _length - 1 do
    lookup(KRSET, RoyalTable, source[i], height_pattern);

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

  result := error_number;
end;

{ Handles DAFT Code symbols }
function daft_code(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  loopey : Integer;
  writer, i : Integer;
  p : Integer;
begin
  if (_length > 576) then
  begin
    strcpy(symbol.errtxt, Format('Error 492: Input length %d too long (maximum 576)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;
  to_upper(source);

  for i := 0 to _length - 1 do
  begin
    p := posn(DAFTSET, Chr(source[i]));
    if p = -1 then
    begin
      strcpy(symbol.errtxt, Format('Error 493: Invalid character at position %d in input ("D", "A", "F" and "T" only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  writer := 0;
  for loopey := 0 to _length - 1 do
  begin
    p := posn(DAFTSET, Chr(source[loopey]));
    { FADT: F=0(Full), A=1(Asc), D=2(Desc), T=3(Track) }
    if (p = 1) or (p = 0) then { A or F }
      set_module(symbol, 0, writer);
    set_module(symbol, 1, writer);
    if (p = 2) or (p = 0) then { D or F }
      set_module(symbol, 2, writer);
    Inc(writer, 2);
  end;

  symbol.row_height[0] := 3;
  symbol.row_height[1] := 2;
  symbol.row_height[2] := 3;
  symbol.rows := 3;
  symbol.width := writer - 1;

  result := 0;
end;

{ Flattermarken - Not really a barcode symbology! }
function flattermarken(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  i, error_number : Integer;
  dest : TArrayOfChar;
begin
  SetLength(dest, 512);
  error_number := 0;

  if (_length > 128) then
  begin
    strcpy(symbol.errtxt, Format('Error 494: Input length %d too long (maximum 128)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  for i := 0 to _length - 1 do
  begin
    if (source[i] < Ord('0')) or (source[i] > Ord('9')) then
    begin
      strcpy(symbol.errtxt, Format('Error 495: Invalid character at position %d in input (digits only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  dest[0] := #0;
  for i := 0 to _length - 1 do
    lookup(NEON, FlatTable, source[i], dest);

  expand(symbol, dest);
  result := error_number;
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
  error_number := 0;

  if (_length > 20) then
  begin
    strcpy(symbol.errtxt, Format('Error 496: Input length %d too long (maximum 20)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  SetLength(local_source, _length + 1);
  for i := 0 to _length - 1 do
    local_source[i] := source[i];
  local_source[_length] := 0;
  to_upper(local_source);

  for i := 0 to _length - 1 do
  begin
    if posn(SHKASUTSET, Chr(local_source[i])) = -1 then
    begin
      strcpy(symbol.errtxt, Format('Error 497: Invalid character at position %d in input (alphanumerics and "-" only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  Fill(inter, 20, 'd'); { Pad character CC4 }
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
      if (local_source[i] <= Ord('J')) then
      begin
        inter[inter_posn] := 'a';
        inter[inter_posn + 1] := Chr(local_source[i] - Ord('A') + Ord('0'));
        Inc(inter_posn, 2);
      end
      else if (local_source[i] <= Ord('T')) then
      begin
        inter[inter_posn] := 'b';
        inter[inter_posn + 1] := Chr(local_source[i] - Ord('K') + Ord('0'));
        Inc(inter_posn, 2);
      end
      else { U-Z }
      begin
        inter[inter_posn] := 'c';
        inter[inter_posn + 1] := Chr(local_source[i] - Ord('U') + Ord('0'));
        Inc(inter_posn, 2);
      end;
    end;
    Inc(i);
  until not ((i < _length) and (inter_posn < 20));

  if (i <> _length) or (inter[20] <> #0) then
  begin
    strcpy(symbol.errtxt, 'Error 477: Input too long, requires too many symbol characters (maximum 20)');
    result := ZERROR_TOO_LONG; exit;
  end;

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
  if (check <= 9) then
    check_char := Chr(check + Ord('0'))
  else
  if (check = 10) then
    check_char := '-'
  else
    check_char := Chr((check - 11) + Ord('a'));
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

  result := error_number;
end;

end.

