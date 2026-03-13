unit zint_code;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    b3a3c0d updated to Zint 2.16.0.9 (2026-03-13) - code_11, c39, ec39, c93, vin
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, zint;

function code_11(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function c39(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function ec39(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function c93(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function vin(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function channel_code(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;

procedure NextB(Chan : Integer; i : Integer; MaxB : Integer; MaxS : Integer; var S, B : TArrayOfInteger; var value, target_value : Integer; var pattern : TArrayOfChar);
procedure NextS(Chan : Integer; i : Integer; MaxS : Integer; MaxB : Integer; var S, B : TArrayOfInteger; var value, target_value : Integer; var pattern : TArrayOfChar);

implementation

uses
  zint_common, zint_helper;

const SODIUM : String	= '0123456789-';
const SILVER : String =	'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%abcd';

const C11Table : array[0..10] of String = ('111121', '211121', '121121', '221111', '112121', '212111', '122111',
	'111221', '211211', '211111', '112111');

{ Code 39 tables checked against ISO/IEC 16388:2007 }

{ Incorporates Table A1 }
const C39Table : array[0..42] of String = ( '1112212111', '2112111121', '1122111121', '2122111111', '1112211121',
	'2112211111', '1122211111', '1112112121', '2112112111', '1122112111', '2111121121',
	'1121121121', '2121121111', '1111221121', '2111221111', '1121221111', '1111122121',
	'2111122111', '1121122111', '1111222111', '2111111221', '1121111221', '2121111211',
	'1111211221', '2111211211', '1121211211', '1111112221', '2111112211', '1121112211',
	'1111212211', '2211111121', '1221111121', '2221111111', '1211211121', '2211211111',
	'1221211111', '1211112121', '2211112111', '1221112111', '1212121111', '1212111211',
	'1211121211', '1112121211');
{ Code 39 character assignments (Table 1) }

const EC39Ctrl : array[0..127] of String = ('%U', '$A', '$B', '$C', '$D', '$E', '$F', '$G', '$H', '$I', '$J', '$K',
	'$L', '$M', '$N', '$O', '$P', '$Q', '$R', '$S', '$T', '$U', '$V', '$W', '$X', '$Y', '$Z',
	'%A', '%B', '%C', '%D', '%E', ' ', '/A', '/B', '/C', '/D', '/E', '/F', '/G', '/H', '/I', '/J',
	'/K', '/L', '-', '.', '/O', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '/Z', '%F',
	'%G', '%H', '%I', '%J', '%V', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
	'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '%K', '%L', '%M', '%N', '%O',
	'%W', '+A', '+B', '+C', '+D', '+E', '+F', '+G', '+H', '+I', '+J', '+K', '+L', '+M', '+N', '+O',
	'+P', '+Q', '+R', '+S', '+T', '+U', '+V', '+W', '+X', '+Y', '+Z', '%P', '%Q', '%R', '%S', '%T');
{ Encoding the full ASCII character set in Code 39 (Table A2) }

const C93Ctrl : array[0..127] of String = ('bU', 'aA', 'aB', 'aC', 'aD', 'aE', 'aF', 'aG', 'aH', 'aI', 'aJ', 'aK',
	'aL', 'aM', 'aN', 'aO', 'aP', 'aQ', 'aR', 'aS', 'aT', 'aU', 'aV', 'aW', 'aX', 'aY', 'aZ',
	'bA', 'bB', 'bC', 'bD', 'bE', ' ', 'cA', 'cB', 'cC', 'cD', 'cE', 'cF', 'cG', 'cH', 'cI', 'cJ',
	'cK', 'cL', 'cM', 'cN', 'cO', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'cZ', 'bF',
	'bG', 'bH', 'bI', 'bJ', 'bV', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
	'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'bK', 'bL', 'bM', 'bN', 'bO',
	'bW', 'dA', 'dB', 'dC', 'dD', 'dE', 'dF', 'dG', 'dH', 'dI', 'dJ', 'dK', 'dL', 'dM', 'dN', 'dO',
	'dP', 'dQ', 'dR', 'dS', 'dT', 'dU', 'dV', 'dW', 'dX', 'dY', 'dZ', 'bP', 'bQ', 'bR', 'bS', 'bT');

const C93Table : array[0..46] of String = ('131112', '111213', '111312', '111411', '121113', '121212', '121311',
	'111114', '131211', '141111', '211113', '211212', '211311', '221112', '221211', '231111',
	'112113', '112212', '112311', '122112', '132111', '111123', '111222', '111321', '121122',
	'131121', '212112', '212211', '211122', '211221', '221121', '222111', '112122', '112221',
	'122121', '123111', '121131', '311112', '311211', '321111', '112131', '113121', '211131',
	'121221', '312111', '311121', '122211');


{ *********************** CODE 11 ******************** }

{ Code 11 }
function code_11(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  i, j : Integer;
  h, c_digit, c_weight, c_count, k_digit, k_weight, k_count : Integer;
  weight : array[0..140] of Integer;
  error_number : Integer;
  num_check_digits : Integer;
  dest : TArrayOfChar; { 6 + 140 * 6 + 2 * 6 + 5 + 1 = 864 }
  d : Integer;
  checkstr : TArrayOfByte;
begin
  SetLength(dest, 870);
  SetLength(checkstr, 3);
  error_number := 0;

  if (_length > 140) then
  begin
    strcpy(symbol.errtxt, Format('Error 320: Input length %d too long (maximum 140)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  for i := 0 to _length - 1 do
  begin
    if not ((source[i] >= Ord('0')) and (source[i] <= Ord('9')) or (source[i] = Ord('-'))) then
    begin
      strcpy(symbol.errtxt, Format('Error 321: Invalid character at position %d in input (digits and "-" only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  if (symbol.option_2 < 0) or (symbol.option_2 > 2) then
  begin
    strcpy(symbol.errtxt, Format('Error 339: Invalid check digit version ''%d'' (1 or 2 only)', [symbol.option_2]));
    result := ZERROR_INVALID_OPTION; exit;
  end;

  if symbol.option_2 = 2 then
    num_check_digits := 0
  else if symbol.option_2 = 1 then
    num_check_digits := 1
  else
    num_check_digits := 2;

  { Start character }
  strcpy(dest, '112211');
  d := 6;

  { Draw main body of barcode }
  for i := 0 to _length - 1 do
  begin
    if (source[i] = Ord('-')) then
      weight[i] := 10
    else
      weight[i] := ctoi(Chr(source[i]));
    lookup(SODIUM, C11Table, source[i], dest);
    Inc(d, 6);
  end;

  if (num_check_digits > 0) then
  begin
    c_weight := 1;
    c_count := 0;
    { Calculate C checksum }
    for h := _length - 1 downto 0 do
    begin
      Inc(c_count, c_weight * weight[h]);
      Inc(c_weight);
      if (c_weight > 10) then
        c_weight := 1;
    end;
    c_digit := c_count mod 11;

    checkstr[0] := Ord(SODIUM[c_digit + 1]); { 1-based string index }
    lookup(SODIUM, C11Table, Chr(checkstr[0]), dest);
    Inc(d, 6);

    if (num_check_digits = 2) then
    begin
      k_weight := 1;
      k_count := 0;
      weight[_length] := c_digit;
      { Calculate K checksum }
      for h := _length downto 0 do
      begin
        Inc(k_count, k_weight * weight[h]);
        Inc(k_weight);
        if (k_weight > 9) then
          k_weight := 1;
      end;
      k_digit := k_count mod 11;

      checkstr[1] := Ord(SODIUM[k_digit + 1]); { 1-based string index }
      lookup(SODIUM, C11Table, Chr(checkstr[1]), dest);
      Inc(d, 6);
    end;
  end;

  { Stop character }
  concat(dest, '11221');

  expand(symbol, dest);

  ustrcpy(symbol.text, source);
  if (num_check_digits > 0) then
  begin
    { Append num_check_digits bytes from checkstr to symbol.text }
    j := ustrlen(symbol.text);
    for i := 0 to num_check_digits - 1 do
      symbol.text[j + i] := checkstr[i];
    symbol.text[j + num_check_digits] := 0;
  end;

  result := error_number; exit;
end;

{ Code 39 }
function c39(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  i : Integer;
  counter : Integer;
  check_digit : Char;
  error_number : Integer;
  dest : TArrayOfChar;
begin
  SetLength(dest, 890); { 10 (Start) + 86*10 + 10 (Check) + 9 (Stop) + 1 = 890 }
  error_number := 0;
  counter := 0;

  if (symbol.option_2 < 0) or (symbol.option_2 > 2) then
    symbol.option_2 := 0;

  { LOGMARS MIL-STD-1189 Rev. B Section 5.2.6.2 }
  if (symbol.symbology = BARCODE_LOGMARS) and (_length > 30) then
  begin
    strcpy(symbol.errtxt, Format('Error 322: Input length %d too long (maximum 30)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end
  { Prevent encoded_data out-of-bounds for BARCODE_HIBC_39 due to wider bars }
  else if (symbol.symbology = BARCODE_HIBC_39) and (_length > 70) then
  begin
    strcpy(symbol.errtxt, Format('Error 319: Input length %d too long (maximum 68)', [_length - 2]));
    result := ZERROR_TOO_LONG; exit;
  end
  else if (_length > 86) then
  begin
    strcpy(symbol.errtxt, Format('Error 323: Input length %d too long (maximum 86)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  to_upper(source);

  for i := 0 to _length - 1 do
  begin
    if posn(SILVER, Chr(source[i])) = -1 then
    begin
      strcpy(symbol.errtxt, Format('Error 324: Invalid character at position %d in input (alphanumerics, space and "-.$/+%%" only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  { Start character }
  strcpy(dest, '1211212111');

  for i := 0 to _length - 1 do
  begin
    lookup(SILVER, C39Table, source[i], dest);
    Inc(counter, posn(SILVER, Chr(source[i])));
  end;

  check_digit := #0;
  if (symbol.option_2 = 1) or (symbol.option_2 = 2) then
  begin
    counter := counter mod 43;
    check_digit := SILVER[counter + 1]; { 1-based string index }
    lookup(SILVER, C39Table, Ord(check_digit), dest);
  end;

  { Stop character }
  concat(dest, '121121211');

  if (symbol.symbology = BARCODE_LOGMARS) or (symbol.symbology = BARCODE_HIBC_39) then
  begin
    { LOGMARS and HIBC use wider 'wide' bars than normal Code 39 }
    for i := 0 to strlen(dest) - 1 do
    begin
      if (dest[i] = '2') then
        dest[i] := '3';
    end;
  end;

  expand(symbol, dest);

  { Display a space check digit as _, otherwise it looks like an error }
  if (symbol.option_2 = 1) and (check_digit = ' ') then
    check_digit := '_';

  if (symbol.symbology = BARCODE_CODE39) then
  begin
    ustrcpy(symbol.text, '*');
    uconcat(symbol.text, source);
    if (symbol.option_2 = 1) then { Visible check digit }
    begin
      symbol.text[_length + 1] := Ord(check_digit);
      symbol.text[_length + 2] := Ord('*');
      symbol.text[_length + 3] := 0;
    end
    else
    begin
      symbol.text[_length + 1] := Ord('*');
      symbol.text[_length + 2] := 0;
    end;
  end
  else
  begin
    ustrcpy(symbol.text, source);
    if (symbol.option_2 = 1) then { Visible check digit }
    begin
      symbol.text[_length] := Ord(check_digit);
      symbol.text[_length + 1] := 0;
    end;
  end;

  result := error_number; exit;
end;

{ ************** EXTENDED CODE 39 *************** }

{ Extended Code 39 - ISO/IEC 16388:2007 Annex A }
function ec39(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  buffer : TArrayOfByte;
  i : Integer;
  b : Integer;
  error_number : Integer;
  check_digit : Byte;
  have_option_2 : Integer;
begin
  SetLength(buffer, 174); buffer[0] := 0; { 86 * 2 + 1 }
  b := 0;
  have_option_2 := symbol.option_2;

  if (_length > 86) then
  begin
    strcpy(symbol.errtxt, Format('Error 328: Input length %d too long (maximum 86)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  { Create a buffer string and place control characters into it }
  for i := 0 to _length - 1 do
  begin
    if (source[i] > 127) then
    begin
      strcpy(symbol.errtxt, Format('Error 329: Invalid character at position %d in input, extended ASCII not allowed', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
    if Length(EC39Ctrl[source[i]]) = 2 then
    begin
      buffer[b] := Ord(EC39Ctrl[source[i]][1]); Inc(b);
      buffer[b] := Ord(EC39Ctrl[source[i]][2]); Inc(b);
    end
    else
    begin
      buffer[b] := Ord(EC39Ctrl[source[i]][1]); Inc(b);
    end;
  end;

  if (b > 86) then
  begin
    strcpy(symbol.errtxt, Format('Error 317: Input too long, requires %d symbol characters (maximum 86)', [b]));
    result := ZERROR_TOO_LONG; exit;
  end;
  buffer[b] := 0;

  { If option_2=2, temporarily make it 1 so c39 generates a visible check digit in HRT }
  if symbol.option_2 = 2 then
    symbol.option_2 := 1;

  { Then send the buffer to the C39 function }
  error_number := c39(symbol, buffer, b);
  if (error_number >= ZERROR_TOO_LONG) then
  begin
    result := error_number; exit;
  end;

  { Save visible check digit if applicable }
  check_digit := 0;
  if (symbol.option_2 = 1) then
  begin
    { For non-CODE39 symbologies, c39 writes check at position b (no leading *) }
    check_digit := symbol.text[b];
  end;

  { Restore option_2 if it was hidden }
  { (option_2 was already set to 1 by us if it was 2) }

  { Copy over source to HRT, subbing space for unprintables }
  for i := 0 to _length - 1 do
  begin
    if (source[i] <> 0) and (source[i] >= 32) and (source[i] < 127) then
      symbol.text[i] := source[i]
    else
      symbol.text[i] := Ord(' ');
  end;

  if (have_option_2 = 1) and (check_digit <> 0) then
  begin
    symbol.text[_length] := check_digit;
    symbol.text[_length + 1] := 0;
  end
  else
    symbol.text[_length] := 0;

  result := error_number; exit;
end;

{ ******************** CODE 93 ******************* }

{ Code 93 is an advancement on Code 39 and the definition is a lot tighter }
function c93(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  { SILVER includes the extra characters a, b, c and d to represent Code 93 specific
     shift characters 1, 2, 3 and 4 respectively. These characters are never used by
     c39() and ec39() }

  i : Integer;
  h, weight, c, k, error_number : Integer;
  values : array[0..124] of Integer; { 123 + 2 (Checks) }
  buffer : TArrayOfChar;
  dest : TArrayOfChar;
  set_copy : TArrayOfChar;
begin
  SetLength(buffer, 248); { 123*2 + 1 }
  SetLength(dest, 770); { 6 (Start) + 123*6 + 2*6 (Checks) + 7 (Stop) + 1 = 764 }
  set_copy := StrToArrayOfChar(SILVER);
  error_number := 0;
  strcpy(buffer, '');

  if (_length > 123) then
  begin
    strcpy(symbol.errtxt, Format('Error 330: Input length %d too long (maximum 123)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  { Message Content }
  for i := 0 to _length - 1 do
  begin
    if (source[i] > 127) then
    begin
      strcpy(symbol.errtxt, Format('Error 331: Invalid character at position %d in input, extended ASCII not allowed', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
    concat(buffer, C93Ctrl[source[i]]);
  end;

  { Now we can check the true _length of the barcode }
  h := strlen(buffer);
  if (h > 123) then
  begin
    strcpy(symbol.errtxt, Format('Error 332: Input too long, requires %d symbol characters (maximum 123)', [h]));
    result := ZERROR_TOO_LONG; exit;
  end;

  for i := 0 to h - 1 do
    values[i] := posn(SILVER, buffer[i]);

  { Putting the data into dest[] is not done until after check digits are calculated }

  { Check digit C }
  c := 0;
  weight := 1;
  for i := h - 1 downto 0 do
  begin
    Inc(c, values[i] * weight);
    Inc(weight);
    if (weight = 21) then
      weight := 1;
  end;
  c := c mod 47;
  values[h] := c;

  { Check digit K }
  k := 0;
  weight := 1;
  for i := h downto 0 do
  begin
    Inc(k, values[i] * weight);
    Inc(weight);
    if (weight = 16) then
      weight := 1;
  end;
  k := k mod 47;
  values[h + 1] := k;
  Inc(h, 2);

  { Start character }
  strcpy(dest, '111141');

  for i := 0 to h - 1 do
    lookup(SILVER, C93Table, set_copy[values[i]], dest);

  { Stop character }
  concat(dest, '1111411');
  expand(symbol, dest);

  { HRT: by default just the source, check digits shown only if option_2=1 }
  for i := 0 to _length - 1 do
  begin
    if (source[i] <> 0) and (source[i] >= 32) and (source[i] < 127) then
      symbol.text[i] := source[i]
    else
      symbol.text[i] := Ord(' ');
  end;

  if (symbol.option_2 = 1) then
  begin
    symbol.text[_length] := Ord(set_copy[c]);
    symbol.text[_length + 1] := Ord(set_copy[k]);
    symbol.text[_length + 2] := 0;
  end
  else
    symbol.text[_length] := 0;

  result := error_number; exit;
end;


{ ******************** VIN ******************** }

{ Vehicle Identification Number (VIN) }
function vin(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
const
  vin_weight : array[0..16] of Integer = (8, 7, 6, 5, 4, 3, 2, 10, 0, 9, 8, 7, 6, 5, 4, 3, 2);
var
  dest : TArrayOfChar;
  input_check, output_check : Char;
  sum, value, i : Integer;
begin
  SetLength(dest, 210); { 10 + 10 + 17*10 + 9 + 1 = 200 }

  { Check length }
  if (_length <> 17) then
  begin
    strcpy(symbol.errtxt, Format('Error 336: Input length %d wrong (17 characters required)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  to_upper(source);

  { Check input characters, I, O and Q are not allowed }
  for i := 0 to _length - 1 do
  begin
    if not (((source[i] >= Ord('0')) and (source[i] <= Ord('9'))) or
            ((source[i] >= Ord('A')) and (source[i] <= Ord('Z')))) then
    begin
      strcpy(symbol.errtxt, Format('Error 337: Invalid character at position %d in input (alphanumerics only, excluding "IOQ")', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
    if (source[i] = Ord('I')) or (source[i] = Ord('O')) or (source[i] = Ord('Q')) then
    begin
      strcpy(symbol.errtxt, Format('Error 337: Invalid character at position %d in input (alphanumerics only, excluding "IOQ")', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  { Check digit only valid for North America (positions 1-5) }
  if (source[0] >= Ord('1')) and (source[0] <= Ord('5')) then
  begin
    input_check := Chr(source[8]);

    sum := 0;
    for i := 0 to 16 do
    begin
      if (source[i] <= Ord('9')) then
        value := source[i] - Ord('0')
      else if (source[i] <= Ord('H')) then
        value := (source[i] - Ord('A')) + 1
      else if (source[i] <= Ord('R')) then
        value := (source[i] - Ord('J')) + 1
      else { S..Z }
        value := (source[i] - Ord('S')) + 2;
      Inc(sum, value * vin_weight[i]);
    end;

    output_check := Chr(Ord('0') + (sum mod 11));
    if (output_check = ':') then
      output_check := 'X'; { Check digit was 10 }

    if (input_check <> output_check) then
    begin
      strcpy(symbol.errtxt, Format('Error 338: Invalid check digit ''%s'' (position 9), expecting ''%s''', [input_check, output_check]));
      result := ZERROR_INVALID_CHECK; exit;
    end;
  end;

  { Start character }
  strcpy(dest, '1211212111');

  { Import character 'I' prefix? }
  if (symbol.option_2 = 1) then
    lookup(SILVER, C39Table, Ord('I'), dest);

  { Copy glyphs to symbol }
  for i := 0 to 16 do
    lookup(SILVER, C39Table, source[i], dest);

  { Stop character }
  concat(dest, '121121211');

  expand(symbol, dest);

  ustrcpy(symbol.text, source);

  result := 0;
end;

{ NextS() and NextB() are from ANSI/AIM BC12-1998 and are Copyright (c) AIM 1997 }
{ Their are used here on the understanding that they form part of the specification
   for Channel Code and therefore their use is permitted under the following terms
   set out in that document:

   'It is the intent and understanding of AIM [t]hat the symbology presented in this
   specification is entirely in the public domain and free of all use restrictions,
   licenses and fees. AIM USA, its memer companies, or individual officers
   assume no liability for the use of this document.' }


procedure CheckCharacter(var pattern : TArrayOfChar; const value, target_value : Integer; const S, B : TArrayOfInteger);
var
  i : Integer;
  part : TArrayOfChar;
begin
  SetLength(part, 3);
  if (value = target_value) then
  begin
    { Target reached - save the generated pattern }
    strcpy(pattern, '11110');
    for i := 0 to 10 do
    begin
      part[0] := itoc(S[i]);
      part[1] := itoc(B[i]);
      part[2] := #0;
      concat(pattern, part);
    end;
  end;
end;

procedure NextB(Chan : Integer; i : Integer; MaxB : Integer; MaxS : Integer; var S, B : TArrayOfInteger; var value, target_value : Integer; var pattern : TArrayOfChar);
var
  _b : Integer;
begin
  if (S[i]+B[i-1]+S[i-1]+B[i-2] > 4) then _b := 1 else _b := 2;
  if (i < Chan + 2) then
  begin
    while _b <= MaxB do
    begin
      B[i] := _b;
      NextS(Chan, i + 1, MaxS, MaxB + 1 - _b, S, B, value, target_value, pattern);
      Inc(_b);
    end;
  end
  else if (_b <= MaxB) then
  begin
    B[i] := MaxB;
    CheckCharacter(pattern, value, target_value, S, B);
    Inc(value);
  end;
end;

procedure NextS(Chan : Integer; i : Integer; MaxS : Integer; MaxB : Integer; var S, B : TArrayOfInteger; var value, target_value : Integer; var pattern : TArrayOfChar);
var
  _s : Integer;
begin
  if (i < Chan + 2) then _s := 1 else _s := MaxS;
  while _s <= MaxS do
  begin
    S[i] := _s;
    NextB(Chan, i, MaxB, MaxS + 1 - _s, S, B, value, target_value, pattern);
    Inc(_s);
  end;
end;

{ Channel Code - According to ANSI/AIM BC12-1998 }
function channel_code(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  S, B : TArrayOfInteger;
  value, target_value : Integer;
  pattern : TArrayOfChar;
  channels, i : Integer;
  error_number, range, zeroes : Integer;
  hrt : TArrayOfChar;
begin
  SetLength(S, 11);
  SetLength(B, 11);
  SetLength(pattern, 30);
  //error_number := 0;
  range := 0;
  SetLength(hrt, 9);

  target_value := 0;

  if (_length > 7) then
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

  if ((symbol.option_2 < 3) or (symbol.option_2 > 8)) then channels := 0 else channels := symbol.option_2;
  if (channels = 0) then channels := _length + 1;
  if (channels = 2) then channels := 3;

  for i := 0 to _length - 1 do
  begin
    target_value := target_value * 10;
    Inc(target_value, ctoi(Chr(source[i])));
  end;

  case channels of
    3: if (target_value > 26) then range := 1;
    4: if (target_value > 292) then range := 1;
    5: if (target_value > 3493) then range := 1;
    6: if (target_value > 44072) then range := 1;
    7: if (target_value > 576688) then range := 1;
    8: if (target_value > 7742862) then range := 1;
  end;
  if (range <> 0) then
  begin
    strcpy(symbol.errtxt, 'Value out of range');
    result := ZERROR_INVALID_DATA; exit;
  end;

  for i := 0 to 10 do begin B[i] := 0; S[i] := 0; end;

  B[0] := 1; S[1] := 1; B[1] := 1; S[2] := 1; B[2] := 1;
  value := 0;
  NextS(channels, 3, channels, channels, S, B, value, target_value, pattern);

  zeroes := channels - 1 - _length;
  Fill(hrt, zeroes, '0');
  hrt[zeroes] := #0;
  concat(hrt, source);
  ustrcpy(symbol.text, ArrayOfCharToArrayOfByte(hrt));

  expand(symbol, pattern);

  result := error_number; exit;
end;

end.

