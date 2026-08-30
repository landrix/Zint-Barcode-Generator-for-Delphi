unit zint_medical;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    b3a3c0d updated to Zint 2.16.0.9 (2026-03-13) - pharma_one, pharma_two, code32, pzn
    3432bc9 complete - codabar (unchanged, to be moved to own unit later)
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
function pzn(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

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
  i : Integer;
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
    strcpy(symbol.errtxt, Format('Error 350: Input length %d too long (maximum 6)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;
  for i := 0 to _length - 1 do
  begin
    if (source[i] < Ord('0')) or (source[i] > Ord('9')) then
    begin
      strcpy(symbol.errtxt, Format('Error 351: Invalid character at position %d in input (digits only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  tester := StrToIntDef(ArrayOfByteToString(source), 0);

  if ((tester < 3) or (tester > 131070)) then
  begin
    strcpy(symbol.errtxt, Format('Error 352: Input value ''%d'' out of range (3 to 131070)', [tester]));
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

  if (symbol.output_options and COMPLIANT_HEIGHT) <> 0 then
    { C: medical.c:95. Laetus Pharmacode Guide 1.2, einspurige Standardhoehe
      8mm / 0.5mm (X). }
    error_number := set_height(symbol, 16.0, 0.0, 0.0, 0)
  else
    set_height(symbol, 0.0, 50.0, 0.0, 1);

  if (symbol.output_options and BARCODE_CONTENT_SEGS) <> 0 then
  begin
    SetLength(symbol.content_segs, 1);
    SetLength(symbol.content_segs[0].Source, _length);
    if _length > 0 then
      Move(source[0], symbol.content_segs[0].Source[0], _length);
    symbol.content_segs[0].Length := _length;
    symbol.content_segs[0].ECI := 0;
    symbol.content_segs[0].SourceMode := -1;
    symbol.content_segs_count := 1;
  end;

  result := error_number; exit;
end;

function pharma_two_calc(tester : Cardinal; var dest : TArrayOfChar) : Integer;
  { This code uses the Two Track Pharamacode defined in the document at
     http://www.laetus.com/laetus.php?request=file&id=69 and using a modified
     algorithm from the One Track system. This standard accepts integer values
     from 4 to 64570080. }
var
  counter, h : Integer;
  inter : TArrayOfChar;
begin
  SetLength(inter, 17);
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

  result := h + 1;
end;

{ Draws the patterns for two track pharmacode }
function pharma_two(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  i : Integer;
  tester : Cardinal;
  height_pattern : TArrayOfChar;
  loopey, h : Integer;
  writer : Integer;
  error_number : Integer;
begin
  SetLength(height_pattern, 200);
  error_number := 0;
  strcpy(height_pattern, '');

  if (_length > 8) then
  begin
    strcpy(symbol.errtxt, Format('Error 354: Input length %d too long (maximum 8)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;
  for i := 0 to _length - 1 do
  begin
    if (source[i] < Ord('0')) or (source[i] > Ord('9')) then
    begin
      strcpy(symbol.errtxt, Format('Error 355: Invalid character at position %d in input (digits only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  tester := StrToIntDef(ArrayOfByteToString(source), 0);

  if ((tester < 4) or (tester > 64570080)) then
  begin
    strcpy(symbol.errtxt, Format('Error 353: Input value ''%d'' out of range (4 to 64570080)', [tester]));
    result := ZERROR_INVALID_DATA; exit;
  end;

  h := pharma_two_calc(tester, height_pattern);

  writer := 0;
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

  if (symbol.output_options and COMPLIANT_HEIGHT) <> 0 then
    { C: medical.c:184. Laetus Pharmacode Guide 1.4, zweispurig:
      Mindesthoehe 8mm / 2mm (X max) = 4X, also 2X je Zeile;
      Standard 8mm / 1mm = 8X, Maximum 12mm / 0.8mm (X min) = 15X. }
    error_number := set_height(symbol, 2.0, 8.0, 15.0, 0)
  else
    set_height(symbol, 0.0, 10.0, 0.0, 1);

  if (symbol.output_options and BARCODE_CONTENT_SEGS) <> 0 then
  begin
    SetLength(symbol.content_segs, 1);
    SetLength(symbol.content_segs[0].Source, _length);
    if _length > 0 then
      Move(source[0], symbol.content_segs[0].Source[0], _length);
    symbol.content_segs[0].Length := _length;
    symbol.content_segs[0].ECI := 0;
    symbol.content_segs[0].SourceMode := -1;
    symbol.content_segs_count := 1;
  end;

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
  //error_number := 0;
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
  saved_option_2 : Integer;
begin
  SetLength(tabella, 34);

  { Validate the input }
  if (_length > 8) then
  begin
    strcpy(symbol.errtxt, Format('Error 360: Input length %d too long (maximum 8)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;
  for i := 0 to _length - 1 do
  begin
    if (source[i] < Ord('0')) or (source[i] > Ord('9')) then
    begin
      strcpy(symbol.errtxt, Format('Error 361: Invalid character at position %d in input (digits only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
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
  //checkpart := 0;
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
  saved_option_2 := symbol.option_2;
  if (symbol.option_2 = 1) or (symbol.option_2 = 2) then
    symbol.option_2 := 0; { Don't let c39 add its own check digit }

  error_number := c39(symbol, ArrayOfCharToArrayOfByte(risultante), strlen(risultante));

  if (saved_option_2 = 1) or (saved_option_2 = 2) then
    symbol.option_2 := saved_option_2; { Restore }

  if (error_number <> 0) then begin result := error_number; exit; end;

  if (symbol.output_options and COMPLIANT_HEIGHT) <> 0 then
    { C: medical.c:273. Allegato A, Caratteristiche tecniche del bollino
      farmaceutico: X ist mit 0.250mm angegeben, Hoehe und Ruhezonen bleiben
      bei ISO/IEC 16388:2007 (Code 39). Mindesthoehe 5mm / 0.25mm = 20, was
      ueber den 15% der Breite liegt ((10 * 8 + 19) * 0.15 = 14.85).
      Derselbe Wert dient als Standard. }
    error_number := set_height(symbol, 20.0, 20.0, 0.0, 0)
  else
    set_height(symbol, 0.0, 50.0, 0.0, 1);

  { Override the normal text output with the Pharmacode number }
  ustrcpy(symbol.text, 'A');
  uconcat(symbol.text, localstr);

  result := error_number; exit;
end;

{ Pharmazentralnummer (PZN) }
{ PZN https://www.ifaffm.de/mandanten/1/documents/04_ifa_coding_system/IFA_Info_Code_39_EN.pdf }
{ PZN https://www.ifaffm.de/mandanten/1/documents/04_ifa_coding_system/
       IFA-Info_Check_Digit_Calculations_PZN_PPN_UDI_EN.pdf }
function pzn(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  i, error_number, zeroes : Integer;
  count, check_digit : Integer;
  have_check_digit : Byte;
  local_source : TArrayOfByte; { '-' prefix + 8 digits }
  pzn7 : Integer;
  saved_option_2 : Integer;
begin
  if symbol.option_2 = 1 then pzn7 := 1 else pzn7 := 0;
  saved_option_2 := symbol.option_2;

  if (_length > 8 - pzn7) then
  begin
    strcpy(symbol.errtxt, Format('Error 325: Input length %d too long (maximum %d)', [_length, 8 - pzn7]));
    result := ZERROR_TOO_LONG; exit;
  end;

  have_check_digit := 0;
  if (_length = 8 - pzn7) then
  begin
    have_check_digit := source[7 - pzn7];
    Dec(_length);
  end;

  for i := 0 to _length - 1 do
  begin
    if (source[i] < Ord('0')) or (source[i] > Ord('9')) then
    begin
      strcpy(symbol.errtxt, Format('Error 326: Invalid character at position %d in input (digits only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  SetLength(local_source, 10); { '-' + up to 8 digits + null }
  local_source[0] := Ord('-');
  zeroes := 7 - pzn7 - _length + 1;
  for i := 1 to zeroes - 1 do
    local_source[i] := Ord('0');
  Move(source[0], local_source[zeroes], _length);

  count := 0;
  for i := 1 to 7 - pzn7 do
    count := count + (i + pzn7) * ctoi(Chr(local_source[i]));

  check_digit := count mod 11;

  if (check_digit = 10) then
  begin
    strcpy(symbol.errtxt, 'Error 327: Invalid PZN, check digit is ''10''');
    result := ZERROR_INVALID_DATA; exit;
  end;

  if (have_check_digit <> 0) and (ctoi(Chr(have_check_digit)) <> check_digit) then
  begin
    strcpy(symbol.errtxt, Format('Error 890: Invalid check digit ''%s'', expecting ''%s''',
      [Chr(have_check_digit), itoc(check_digit)]));
    result := ZERROR_INVALID_CHECK; exit;
  end;

  local_source[8 - pzn7] := Ord(itoc(check_digit));

  if (symbol.option_2 = 1) or (symbol.option_2 = 2) then
    symbol.option_2 := 0; { Need to overwrite so c39 doesn't add a check digit itself }

  error_number := c39(symbol, local_source, 9 - pzn7);

  if (saved_option_2 = 1) or (saved_option_2 = 2) then
    symbol.option_2 := saved_option_2; { Restore }

  if (symbol.output_options and COMPLIANT_HEIGHT) <> 0 then
  begin
    { C: medical.c:357. Technical Information regarding PZN Coding V 2.1
      (25.02.2019), Code size: "normales" X 0.25mm (0.187 - 0.45),
      Hoehe 8mm bis 20mm bei X 0.25mm; genannt werden 10mm, also
      10mm / 0.25mm = 40 als Standard. }
    if error_number < ZERROR_TOO_LONG then
      error_number := set_height(symbol, 17.7777786 { 8.0 / 0.45 }, 40.0,
                                         106.951874 { 20.0 / 0.187 }, 0);
  end
  else
  begin
    if error_number < ZERROR_TOO_LONG then
      set_height(symbol, 0.0, 50.0, 0.0, 1);
  end;

  { HRT }
  ustrcpy(symbol.text, 'PZN - ');
  for i := 1 to 8 - pzn7 do
    uconcat(symbol.text, Chr(local_source[i]));

  result := error_number;
end;

end.

