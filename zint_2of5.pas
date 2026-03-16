unit zint_2of5;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    b3a3c0d complete (2of5.c + 2of5inter.c + 2of5inter_based.c)
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

uses SysUtils, zint_common, zint_helper;

const
  C25MatrixTable : array[0..9] of String = ('113311', '311131', '131131', '331111', '113131', '313111',
	'133111', '111331', '311311', '131311');

  C25IndustTable : array[0..9] of String = ('1111313111', '3111111131', '1131111131', '3131111111', '1111311131',
	'3111311111', '1131311111', '1111113131', '3111113111', '1131113111');

  C25InterTable : array[0..9] of String = ('11331', '31113', '13113', '33111', '11313', '31311', '13311', '11133',
	'31131', '13131');

  C25MatrixStartStop : array[0..1] of String = ('411111', '41111');
  C25IndustStartStop : array[0..1] of String = ('313111', '31113');
  C25IataLogicStartStop : array[0..1] of String = ('1111', '311');

{ GS1 check digit: weighting 1,3,1,3... from left, then (10 - sum mod 10) mod 10 }
function gs1_check_digit(const source : TArrayOfByte; _length : Integer) : Byte;
var
  i, count, factor : Integer;
begin
  count := 0;
  if (_length and 1) <> 0 then factor := 3 else factor := 1;
  for i := 0 to _length - 1 do
  begin
    count := count + factor * ctoi(Chr(source[i]));
    factor := factor xor $02; { Toggles 1 and 3 }
  end;
  Result := Ord(itoc((10 - (count mod 10)) mod 10));
end;

{ Common to Standard (Matrix), Industrial, IATA, and Data Logic }
function c25_common(symbol : zint_symbol; source : TArrayOfByte; _length : Integer;
  max : Integer; is_matrix : Boolean; const start_stop : array of String;
  start_length : Integer; error_base : Integer) : Integer;
var
  i, d : Integer;
  dest : TArrayOfChar;
  local_source : TArrayOfByte;
  have_checkdigit : Boolean;
begin
  SetLength(dest, 1200);
  SetLength(local_source, max + 2);

  have_checkdigit := (symbol.option_2 = 1) or (symbol.option_2 = 2);

  if (_length > max) then
  begin
    strcpy(symbol.errtxt, Format('Error %d: Input length %d too long (maximum %d)', [error_base, _length, max]));
    Result := ZERROR_TOO_LONG; exit;
  end;

  for i := 0 to _length - 1 do
  begin
    if not ((source[i] >= Ord('0')) and (source[i] <= Ord('9'))) then
    begin
      strcpy(symbol.errtxt, Format('Error %d: Invalid character at position %d in input (digits only)', [error_base + 1, i + 1]));
      Result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  for i := 0 to _length - 1 do
    local_source[i] := source[i];

  if have_checkdigit then
  begin
    local_source[_length] := gs1_check_digit(source, _length);
    Inc(_length);
  end;

  { Start character }
  strcpy(dest, start_stop[0]);

  if is_matrix then
  begin
    for i := 0 to _length - 1 do
      lookup(NEON, C25MatrixTable, local_source[i], dest);
  end
  else
  begin
    for i := 0 to _length - 1 do
      lookup(NEON, C25IndustTable, local_source[i], dest);
  end;

  { Stop character }
  concat(dest, start_stop[1]);

  expand(symbol, dest);

  { Exclude check digit from HRT if hidden }
  d := _length;
  if symbol.option_2 = 2 then Dec(d);
  for i := 0 to d - 1 do
    symbol.text[i] := local_source[i];
  symbol.text[d] := 0;

  if (symbol.output_options and BARCODE_CONTENT_SEGS) <> 0 then
  begin
    SetLength(symbol.content_segs, 1);
    SetLength(symbol.content_segs[0].Source, _length);
    if _length > 0 then
      Move(local_source[0], symbol.content_segs[0].Source[0], _length);
    symbol.content_segs[0].Length := _length;
    symbol.content_segs[0].ECI := 0;
    symbol.content_segs[0].SourceMode := -1;
    symbol.content_segs_count := 1;
  end;

  Result := 0;
end;

{ Code 2 of 5 Standard (Code 2 of 5 Matrix) }
function matrix_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
begin
  Result := c25_common(symbol, source, _length, 112, True, C25MatrixStartStop, 6, 301);
end;

{ Code 2 of 5 IATA }
function iata_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
begin
  Result := c25_common(symbol, source, _length, 80, False, C25IataLogicStartStop, 4, 305);
end;

{ Code 2 of 5 Data Logic }
function logic_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
begin
  Result := c25_common(symbol, source, _length, 113, True, C25IataLogicStartStop, 4, 307);
end;

{ Code 2 of 5 Industrial }
function industrial_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
begin
  Result := c25_common(symbol, source, _length, 79, False, C25IndustStartStop, 6, 303);
end;

{ Common to Interleaved, and to ITF-14, DP Leitcode, DP Identcode }
function c25_inter_common(symbol : zint_symbol; source : TArrayOfByte; _length : Integer;
  checkdigit_option : Integer) : Integer;
var
  i, j, d : Integer;
  dest : TArrayOfChar;
  local_source : TArrayOfByte;
  have_checkdigit : Boolean;
  bars, spaces : String;
begin
  SetLength(dest, 700);
  SetLength(local_source, 128);

  have_checkdigit := (checkdigit_option = 1) or (checkdigit_option = 2);

  if (_length > 125) then
  begin
    strcpy(symbol.errtxt, Format('Error 309: Input length %d too long (maximum 125)', [_length]));
    Result := ZERROR_TOO_LONG; exit;
  end;

  for i := 0 to _length - 1 do
  begin
    if not ((source[i] >= Ord('0')) and (source[i] <= Ord('9'))) then
    begin
      strcpy(symbol.errtxt, Format('Error 310: Invalid character at position %d in input (digits only)', [i + 1]));
      Result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  { Input must be an even number of characters for Interlaced 2 of 5 to work:
    if an odd number of characters has been entered and no check digit or an even number and have check digit
    then add a leading zero }
  if have_checkdigit = not Odd(_length) then
  begin
    local_source[0] := Ord('0');
    for i := 0 to _length - 1 do
      local_source[i + 1] := source[i];
    Inc(_length);
  end
  else
  begin
    for i := 0 to _length - 1 do
      local_source[i] := source[i];
  end;

  if have_checkdigit then
  begin
    local_source[_length] := gs1_check_digit(local_source, _length);
    Inc(_length);
  end;

  { Start character }
  strcpy(dest, '1111');

  i := 0;
  while i < _length do
  begin
    { Look up the bars and the spaces }
    bars := C25InterTable[local_source[i] - Ord('0')];
    spaces := C25InterTable[local_source[i + 1] - Ord('0')];

    { Then merge (interlace) the strings together }
    d := strlen(dest);
    for j := 0 to 4 do
    begin
      dest[d] := bars[j + 1]; Inc(d);
      dest[d] := spaces[j + 1]; Inc(d);
    end;
    dest[d] := #0;
    Inc(i, 2);
  end;

  { Stop character }
  concat(dest, '311');

  expand(symbol, dest);

  { Exclude check digit from HRT if hidden }
  d := _length;
  if (symbol.option_2 = 2) then Dec(d);
  for i := 0 to d - 1 do
    symbol.text[i] := local_source[i];
  symbol.text[d] := 0;

  if (symbol.output_options and BARCODE_CONTENT_SEGS) <> 0 then
  begin
    SetLength(symbol.content_segs, 1);
    SetLength(symbol.content_segs[0].Source, _length);
    if _length > 0 then
      Move(local_source[0], symbol.content_segs[0].Source[0], _length);
    symbol.content_segs[0].Length := _length;
    symbol.content_segs[0].ECI := 0;
    symbol.content_segs[0].SourceMode := -1;
    symbol.content_segs_count := 1;
  end;

  Result := 0;
end;

{ Code 2 of 5 Interleaved ISO/IEC 16390:2007 }
function interleaved_two_of_five(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
begin
  Result := c25_inter_common(symbol, source, _length, symbol.option_2);
end;

{ Interleaved 2-of-5 (ITF-14) }
function itf14(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number, zeroes, i, src_off : Integer;
  local_source : TArrayOfByte;
  have_check_digit : Byte;
  check_digit_val : Byte;
begin
  SetLength(local_source, 16);
  have_check_digit := 0;
  src_off := 0;

  { Allow and ignore any AI prefix }
  if ((_length = 17) or (_length = 18)) and
     (((source[0] = Ord('[')) and (source[1] = Ord('0')) and (source[2] = Ord('1')) and (source[3] = Ord(']'))) or
      ((source[0] = Ord('(')) and (source[1] = Ord('0')) and (source[2] = Ord('1')) and (source[3] = Ord(')')))) then
  begin
    src_off := 4;
    Dec(_length, 4);
  end
  else if ((_length = 15) or (_length = 16)) and (source[0] = Ord('0')) and (source[1] = Ord('1')) then
  begin
    src_off := 2;
    Dec(_length, 2);
  end;

  if (_length > 14) then
  begin
    strcpy(symbol.errtxt, Format('Error 311: Input length %d too long (maximum 14)', [_length]));
    Result := ZERROR_TOO_LONG; exit;
  end;

  for i := 0 to _length - 1 do
  begin
    if not ((source[src_off + i] >= Ord('0')) and (source[src_off + i] <= Ord('9'))) then
    begin
      strcpy(symbol.errtxt, Format('Error 312: Invalid character at position %d in input (digits only)', [i + 1]));
      Result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  if (_length = 14) then
  begin
    have_check_digit := source[src_off + 13];
    Dec(_length);
  end;

  { Add leading zeros as required }
  zeroes := 13 - _length;
  for i := 0 to zeroes - 1 do
    local_source[i] := Ord('0');
  for i := 0 to _length - 1 do
    local_source[zeroes + i] := source[src_off + i];

  { Calculate the check digit - the same method used for EAN-13 }
  check_digit_val := gs1_check_digit(local_source, 13);
  if (have_check_digit <> 0) and (have_check_digit <> check_digit_val) then
  begin
    strcpy(symbol.errtxt, Format('Error 850: Invalid check digit ''%s'', expecting ''%s''',
      [Chr(have_check_digit), Chr(check_digit_val)]));
    Result := ZERROR_INVALID_CHECK; exit;
  end;
  local_source[13] := check_digit_val;

  error_number := c25_inter_common(symbol, local_source, 14, 0);

  if (error_number < ZERROR_TOO_LONG) then
  begin
    if not ((symbol.output_options and BARCODE_BOX <> 0) or
            (symbol.output_options and BARCODE_BIND <> 0) or
            (symbol.output_options and BARCODE_BIND_TOP <> 0)) then
    begin
      symbol.output_options := symbol.output_options or BARCODE_BOX;
      if symbol.border_width = 0 then
        symbol.border_width := 5;
    end;
  end;

  { HRT }
  for i := 0 to 13 do
    symbol.text[i] := local_source[i];
  symbol.text[14] := 0;

  Result := error_number;
end;

{ Deutsche Post Leitcode }
function dpleit(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number, i, zeroes : Integer;
  count : Cardinal;
  factor : Integer;
  local_source : TArrayOfByte;
  hrt : String;
begin
  SetLength(local_source, 16);
  count := 0;

  if (_length > 13) then
  begin
    strcpy(symbol.errtxt, Format('Error 313: Input length %d too long (maximum 13)', [_length]));
    Result := ZERROR_TOO_LONG; exit;
  end;

  for i := 0 to _length - 1 do
  begin
    if not ((source[i] >= Ord('0')) and (source[i] <= Ord('9'))) then
    begin
      strcpy(symbol.errtxt, Format('Error 314: Invalid character at position %d in input (digits only)', [i + 1]));
      Result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  zeroes := 13 - _length;
  for i := 0 to zeroes - 1 do
    local_source[i] := Ord('0');
  for i := 0 to _length - 1 do
    local_source[zeroes + i] := source[i];

  factor := 4;
  for i := 12 downto 0 do
  begin
    count := count + Cardinal(factor) * Cardinal(ctoi(Chr(local_source[i])));
    factor := factor xor $0D; { Toggles 4 and 9 }
  end;

  local_source[13] := Ord(itoc((10 - (count mod 10)) mod 10));

  error_number := c25_inter_common(symbol, local_source, 14, 0);

  { HRT formatting: XXXXX.XXX.XXX.XXX }
  hrt := '';
  for i := 0 to 13 do
    hrt := hrt + Chr(local_source[i]);
  hrt := Copy(hrt, 1, 5) + '.' + Copy(hrt, 6, 3) + '.' + Copy(hrt, 9, 3) + '.' + Copy(hrt, 12, 3);
  for i := 1 to Length(hrt) do
    symbol.text[i - 1] := Ord(hrt[i]);
  symbol.text[Length(hrt)] := 0;

  Result := error_number;
end;

{ Deutsche Post Identcode }
function dpident(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number, zeroes, i : Integer;
  count : Cardinal;
  factor : Integer;
  local_source : TArrayOfByte;
  hrt : String;
begin
  SetLength(local_source, 14);
  count := 0;

  if (_length > 11) then
  begin
    strcpy(symbol.errtxt, Format('Error 315: Input length %d too long (maximum 11)', [_length]));
    Result := ZERROR_TOO_LONG; exit;
  end;

  for i := 0 to _length - 1 do
  begin
    if not ((source[i] >= Ord('0')) and (source[i] <= Ord('9'))) then
    begin
      strcpy(symbol.errtxt, Format('Error 316: Invalid character at position %d in input (digits only)', [i + 1]));
      Result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  zeroes := 11 - _length;
  for i := 0 to zeroes - 1 do
    local_source[i] := Ord('0');
  for i := 0 to _length - 1 do
    local_source[zeroes + i] := source[i];

  factor := 4;
  for i := 10 downto 0 do
  begin
    count := count + Cardinal(factor) * Cardinal(ctoi(Chr(local_source[i])));
    factor := factor xor $0D; { Toggles 4 and 9 }
  end;

  local_source[11] := Ord(itoc((10 - (count mod 10)) mod 10));

  error_number := c25_inter_common(symbol, local_source, 12, 0);

  { HRT formatting: XX.XX X.XXX.XXX X }
  hrt := '';
  for i := 0 to 11 do
    hrt := hrt + Chr(local_source[i]);
  hrt := Copy(hrt, 1, 2) + '.' + Copy(hrt, 3, 2) + ' ' + hrt[5] + '.' + Copy(hrt, 6, 3) + '.' + Copy(hrt, 9, 3) + ' ' + hrt[12];
  for i := 1 to Length(hrt) do
    symbol.text[i - 1] := Ord(hrt[i]);
  symbol.text[Length(hrt)] := 0;

  Result := error_number;
end;

end.

