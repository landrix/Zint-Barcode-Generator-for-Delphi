unit zint_upcean;

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

function eanx(symbol: zint_symbol; const source: TArrayOfByte; src_len : Integer): integer;

implementation

uses
  SysUtils, zint_common, zint_helper;

const
  SODIUM = '0123456789+';
  EAN2 = 102;
  EAN5 = 105;

{ UPC and EAN tables checked against EN 797:1996 }

const UPCParity0 : Array [0..9] of String = ('BBBAAA', 'BBABAA', 'BBAABA', 'BBAAAB', 'BABBAA', 'BAABBA', 'BAAABB',
  'BABABA', 'BABAAB', 'BAABAB'); { Number set for UPC-E symbol (EN Table 4) }
const UPCParity1 : Array [0..9] of String = ('AAABBB', 'AABABB', 'AABBAB', 'AABBBA', 'ABAABB', 'ABBAAB', 'ABBBAA',
  'ABABAB', 'ABABBA', 'ABBABA'); { Not covered by BS EN 797:1995 }
const EAN2Parity : Array [0..3] of String = ('AA', 'AB', 'BA', 'BB'); { Number sets for 2-digit add-on (EN Table 6) }
const EAN5Parity : Array [0..9] of String = ('BBAAA', 'BABAA', 'BAABA', 'BAAAB', 'ABBAA', 'AABBA', 'AAABB', 'ABABA',
  'ABAAB', 'AABAB'); { Number set for 5-digit add-on (EN Table 7) }
const EAN13Parity : Array [0..9] of String = ('AAAAA', 'ABABB', 'ABBAB', 'ABBBA', 'BAABB', 'BBAAB', 'BBBAA', 'BABAB',
  'BABBA', 'BBABA'); { Left hand of the EAN-13 symbol (EN Table 3) }
const EANsetA : Array [0..9] of String = ('3211', '2221', '2122', '1411', '1132', '1231', '1114', '1312', '1213',
  '3112'); { Representation set A and C (EN Table 1) }
const EANsetB : Array [0..9] of String = ('1123', '1222', '2212', '1141', '2311', '1321', '4111', '2131', '3121',
  '2113'); { Representation set B (EN Table 1) }

function upc_check(const source : TArrayOfChar): Char;
var
  i, count, check_digit : Cardinal;
begin { Calculate the correct check digit for a UPC barcode }
  count := 0;

  for i := 0 to strlen(source) - 1 do
  begin
    Inc(count, ctoi(source[i]));

    if not((i and 1)<>0) then
      inc(count, 2 * (ctoi(source[i])));
  end;

  check_digit := 10 - (count mod 10);
  if (check_digit = 10) then check_digit := 0;
  Result:=itoc(check_digit);
end;

procedure upca_draw(const source : TArrayOfChar; var dest : TArrayOfChar);
var
  i, half_way : Cardinal;
begin { UPC A is usually used for 12 digit numbers, but this function takes a source of any _length }

  half_way := strlen(source) div 2;

  { start character }
  concat (dest, '111');

  for i := 0 to strlen(source)-1 do
  begin
    if (i = half_way) then
    begin
      { middle character - separates manufacturer no. from product no. }
      { also inverts right hand characters }
      concat(dest, '11111');
    end;

    lookup(NEON, EANsetA, source[i], dest);
  end;

  { stop character }
  concat (dest, '111');
end;

procedure upca(var symbol : zint_symbol; const source : TArrayOfByte; var dest : TArrayOfChar);
var
  _length : Integer;
  gtin : TArrayOfChar;
begin { Make a UPC A barcode when we haven't been given the check digit }
  SetLength(gtin, 15);

  strcpy(gtin, ArrayOfByteToString(source));
  _length := strlen(gtin);
  gtin[_length] := upc_check(gtin);
  gtin[_length + 1] := #0;
  upca_draw(gtin, dest);
  ustrcpy(symbol.text, ArrayOfCharToArrayOfByte(gtin));
end;

procedure upce(var symbol : zint_symbol; const source : TArrayOfByte; var dest : TArrayOfChar);
var
  i, num_system : Cardinal;
  emode : Byte;
  check_digit : Char;
  equivalent, parity, temp, hrt: TArrayOfByte;
begin { UPC E is a zero-compressed version of UPC A }
  SetLength(equivalent,12);
  SetLength(parity,8);
  SetLength(temp,8);
  SetLength(hrt,9);

  { Two number systems can be used - system 0 and system 1 }
  if (ustrlen(source) = 7)then
  begin
    case Chr(source[0]) of
      '0': num_system := 0;
      '1': num_system := 1;
    else
      num_system := 0;
      source[0] := Ord('0');
    end;
    ustrcpy(temp, source);
    ustrcpy(hrt, source);
    for i := 1 to 7 do
      source[i - 1] := Ord(temp[i]);
  end
  else
  begin
    num_system := 0;
    hrt[0] := Ord('0');
    hrt[1] := 0;
    uconcat(hrt, source);
  end;

  { Expand the zero-compressed UPCE code to make a UPCA equivalent (EN Table 5) }
  emode := source[5];
  for i := 0 to 10 do
    equivalent[i] := Ord('0');
  if (num_system = 1) then equivalent[0] := temp[0];
  equivalent[1] := source[0];
  equivalent[2] := source[1];
  equivalent[11] := 0;

  case Chr(emode) of
    '0',
    '1',
    '2': begin
      equivalent[3] := emode;
      equivalent[8] := source[2];
      equivalent[9] := source[3];
      equivalent[10] := source[4];
      end;
    '3': begin
      equivalent[3] := source[2];
      equivalent[9] := source[3];
      equivalent[10] := source[4];
      if (((source[2] = Ord('0')) or (source[2] = Ord('1'))) or (source[2] = Ord('2'))) then
        { Note 1 - 'X3 shall not be equal to 0, 1 or 2' }
        strcpy(symbol.errtxt, 'Invalid UPC-E data');
      end;
    '4': begin
      equivalent[3] := source[2];
      equivalent[4] := source[3];
      equivalent[10] := source[4];
      if (source[3] = Ord('0')) then
        { Note 2 - 'X4 shall not be equal to 0' }
        strcpy(symbol.errtxt, 'Invalid UPC-E data');
      end;
    '5',
    '6',
    '7',
    '8',
    '9': begin
      equivalent[3] := source[2];
      equivalent[4] := source[3];
      equivalent[5] := source[4];
      equivalent[10] := emode;
      if (source[4] = Ord('0')) then
        { Note 3 - 'X5 shall not be equal to 0' }
        strcpy(symbol.errtxt, 'Invalid UPC-E data');
      end;
  end;

  { Get the check digit from the expanded UPCA code }

  check_digit := upc_check(ArrayOfByteToArrayOfChar(equivalent));

  { Use the number system and check digit information to choose a parity scheme }
  if (num_system = 1) then
    ustrcpy(parity, UPCParity1[ctoi(check_digit)])
  else
    ustrcpy(parity, UPCParity0[ctoi(check_digit)]);

  { Take all this information and make the barcode pattern }

  { start character }
  concat (dest, '111');

  for i := 0 to ustrlen(source) - 1 do
    case Chr(parity[i]) of
      'A': lookup(NEON, EANsetA, source[i], dest);
      'B': lookup(NEON, EANsetB, source[i], dest);
    end;

  { stop character }
  concat (dest, '111111');

  hrt[7] := Ord(check_digit);
  hrt[8] := 0;
  ustrcpy(symbol.text, hrt);
end;


procedure add_on(const source : TArrayOfByte; var dest : TArrayOfChar; mode : Integer);
var
  parity : TArrayOfChar;
  i, code_type : Cardinal;
  code_value, parity_bit : Integer;
  values : TArrayOfInteger;
  parity_sum: Integer;
begin { EAN-2 and EAN-5 add-on codes }
  SetLength(parity,6);

  { If an add-on then append with space }
  if (mode <> 0) then
    concat(dest, '9');

  { Start character }
  concat (dest, '112');

  { Determine EAN2 or EAN5 add-on }
  if (ustrlen(source) = 2) then
    code_type := EAN2
  else
    code_type := EAN5;

  { Calculate parity for EAN2 }
  if (code_type = EAN2) then
  begin
    code_value := (10 * ctoi(Chr(source[0]))) + ctoi(Chr(source[1]));
    parity_bit := code_value mod 4;
    strcpy(parity, EAN2Parity[parity_bit]);
  end;

  if (code_type = EAN5) then
  begin
    SetLength(values, 6);

    for i := 0 to 5 do
      values[i] := ctoi(Chr(source[i]));

    parity_sum := (3 * (values[0] + values[2] + values[4]));
    inc(parity_sum, (9 * (values[1] + values[3])));

    parity_bit := parity_sum mod 10;
    strcpy(parity, EAN5Parity[parity_bit]);
  end;

  for i := 0 to ustrlen(source)-1 do
  begin
    case parity[i] of
      'A': lookup(NEON, EANsetA, source[i], dest);
      'B': lookup(NEON, EANsetB, source[i], dest);
    end;

    { Glyph separator }
    if (i <> (ustrlen(source) - 1)) then
      concat (dest, '11');
  end;
end;


{ ************************ EAN-13 ****************** }

function ean_check(const source : TArrayOfChar) : Char;
var
  i : Integer;
  h, count, check_digit: Cardinal;
begin { Calculate the correct check digit for a EAN-13 barcode }
  count := 0;

  h := strlen(source);
  for i := h - 1 downto 0 do
  begin
    Inc(count, ctoi(source[i]));

    if (i and 1)<>0 then
      inc(count, 2 * ctoi(source[i]));
  end;
  check_digit := 10 - (count mod 10);
  if (check_digit = 10) then check_digit := 0;
  Result:=itoc(check_digit);
end;

procedure ean13(symbol : zint_symbol; const source : TArrayOfByte; var dest : TArrayOfChar);
var
  _length, i, half_way : Cardinal;
  parity : TArrayOfChar;
  gtin : TArrayOfChar;
begin
  SetLength(parity,6);
  SetLength(gtin,15);

  strcpy(parity, '');
  strcpy(gtin, ArrayOfByteToArrayOfChar(source));

  { Add the appropriate check digit }
  _length := strlen(gtin);
  gtin[_length] := ean_check(gtin);
  gtin[_length + 1] := #0;

  { Get parity for first half of the symbol }
  lookup(SODIUM, EAN13Parity, gtin[0], parity);

  { Now get on with the cipher }
  half_way := 7;

  { start character }
  concat (dest, '111');
  _length := strlen(gtin);
  for i := 1 to  _length do
  begin
    if (i = half_way) then
    begin
      { middle character - separates manufacturer no. from product no. }
      { also inverses right hand characters }
      concat (dest, '11111');
    end;

    if (((i > 1) and (i < 7)) and (parity[i - 2] = 'B')) then
      lookup(NEON, EANsetB, gtin[i], dest)
    else
      lookup(NEON, EANsetA, gtin[i], dest)
  end;

  { stop character }
  concat (dest, '111');

  ustrcpy(symbol.text, ArrayOfCharToArrayOfByte(gtin));
end;

procedure ean8(symbol : zint_symbol; const source : TArrayOfByte; var dest : TArrayOfChar);
var
  _length : Integer;
  gtin : TArrayOfChar;
begin { Make an EAN-8 barcode when we haven't been given the check digit }
  { EAN-8 is basically the same as UPC-A but with fewer digits }

  SetLength(gtin,10);

  strcpy(gtin, ArrayOfByteToArrayOfChar(source));
  _length := strlen(gtin);
  gtin[_length] := upc_check(gtin);
  gtin[_length + 1] := #0;
  upca_draw(gtin, dest);
  ustrcpy(symbol.text, ArrayOfCharToArrayOfByte(gtin));
end;

function isbn13_check(const source : TArrayOfChar) : Char; { For ISBN(13) only }
var
  i, weight, sum, check, h: Cardinal;
begin
  sum := 0;
  weight := 1;
  h := strlen(source) - 1;

  for i := 0 to h - 1 do
  begin
    inc(sum, ctoi(source[i]) * weight);
    if (weight = 1) then weight := 3 else weight := 1;
  end;

  check := sum mod 10;
  check := 10 - check;
  if (check = 10) then check := 0;
  Result:=itoc(check);
end;

function isbn_check(const source : TArrayOfChar) : Char; { For ISBN(10) and SBN only }
var
  i, weight, sum, check, h: Cardinal;
  check_char : Char;
begin
  sum := 0;
  weight := 1;
  h := strlen(source) - 1;

  for i := 0 to h - 1 do
  begin
    inc(sum, ctoi(source[i]) * weight);
    Inc(weight);
  end;

  check := sum mod 11;
  check_char := itoc(check);
  if (check = 10) then check_char := 'X';
  Result:=check_char;
end;

function isbn(symbol : zint_symbol; var source : TArrayOfByte; src_len : Cardinal; var dest : TArrayOfChar ) : Integer; { Make an EAN-13 barcode from an SBN or ISBN }
var
  i, error_number : Integer;
  check_digit : Char;
begin
  to_upper(source);
  error_number := is_sane('0123456789X', source, src_len);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in input');
    Result:= error_number;
    Exit;
  end;

  { Input must be 9, 10 or 13 characters }
  if (((src_len < 9) or (src_len > 13)) or ((src_len > 10) and (src_len < 13))) then
  begin
    strcpy(symbol.errtxt, 'Input wrong _length');
    Result:=ZERROR_TOO_LONG;
    Exit;
  end;

  if (src_len = 13) then { Using 13 character ISBN }
  begin
    if not((((source[0] = Ord('9')) and (source[1] = Ord('7'))) and
             ((source[2] = Ord('8')) or (source[2] = Ord('9'))))) then
    begin
      strcpy(symbol.errtxt, 'Invalid ISBN');
      Result:=ZERROR_INVALID_DATA;
      Exit;
    end;

    check_digit := isbn13_check(ArrayOfByteToArrayOfChar(source));
    if (source[src_len - 1] <> Ord(check_digit)) then
    begin
      strcpy(symbol.errtxt, 'Incorrect ISBN check');
      Result:=ZERROR_INVALID_CHECK;
      Exit;
    end;
    source[12] := 0;

    ean13(symbol, source, dest);
  end;

  if (src_len = 10) then { Using 10 digit ISBN }
  begin
    check_digit := isbn_check(ArrayOfByteToArrayOfChar(source));
    if (Ord(check_digit) <> source[src_len - 1]) then
    begin
      strcpy(symbol.errtxt, 'Incorrect ISBN check');
      Result:=ZERROR_INVALID_CHECK;
      Exit;
    end;
    for i := 13 downto 3 do
    begin
      source[i] := source[i - 3];
    end;
    source[0] := Ord('9');
    source[1] := Ord('7');
    source[2] := Ord('8');
    source[12] := 0;

    ean13(symbol, source, dest);
  end;

  if (src_len = 9) then { Using 9 digit SBN }
  begin
    { Add leading zero }
    for i := 10 downto 1 do
    begin
      source[i] := source[i - 1];
    end;
    source[0] := Ord('0');

    { Verify check digit }
    check_digit := isbn_check(ArrayOfByteToArrayOfChar(source));
    if (Ord(check_digit) <> source[ustrlen(source) - 1]) then
    begin
      strcpy(symbol.errtxt, 'Incorrect SBN check');
      Result:= ZERROR_INVALID_CHECK;
      Exit;
    end;

    { Convert to EAN-13 number }
    for i := 13 downto 3 do
    begin
      source[i] := source[i - 3];
    end;
    source[0] := Ord('9');
    source[1] := Ord('7');
    source[2] := Ord('8');
    source[12] := 0;

    ean13(symbol, source, dest);
  end;

  Result:=0;
end;

procedure ean_leading_zeroes(symbol : zint_symbol; const source : TArrayOfByte; var local_source: TArrayOfByte);
var
  first_part, second_part, zfirst_part, zsecond_part : TArrayOfByte;
  with_addon : Integer;
  first_len, second_len, zfirst_len, zsecond_len, i, h: Integer;
begin
  { Add leading zeroes to EAN and UPC strings }
  SetLength(first_part,20);
  SetLength(second_part,20);
  SetLength(zfirst_part,20);
  SetLength(zsecond_part,20);
  with_addon := 0;
  first_len := 0;
  second_len := 0;
  zfirst_len := 0;
  zsecond_len := 0;

  h := ustrlen(source);
  for i := 0 to h-1 do
  begin
    if (source[i] = Ord('+')) then
      with_addon := 1
    else
    begin
      if (with_addon = 0) then
        Inc(first_len)
      else
        Inc(second_len);
    end;
  end;

  ustrcpy(first_part, '');
  ustrcpy(second_part, '');
  ustrcpy(zfirst_part, '');
  ustrcpy(zsecond_part, '');

  { Split input into two strings }
  for i := 0 to first_len-1 do
  begin
    first_part[i] := source[i];
    first_part[i + 1] := 0;
  end;

  for i := 0 to second_len - 1 do
  begin
    second_part[i] := source[i + first_len + 1];
    second_part[i + 1] := 0;
  end;

  { Calculate target _lengths }
  if (second_len <= 5) then zsecond_len := 5;
  if (second_len <= 2) then zsecond_len := 2;
  if (second_len = 0) then zsecond_len := 0;
  case symbol.symbology of
    BARCODE_EANX,
    BARCODE_EANX_CC: begin
      if (first_len <= 12) then zfirst_len := 12;
      if (first_len <= 7) then zfirst_len := 7;
      if (second_len = 0) then
      begin
        if (first_len <= 5) then zfirst_len := 5;
        if (first_len <= 2) then zfirst_len := 2;
      end;
      end;
    BARCODE_UPCA,
    BARCODE_UPCA_CC:
      zfirst_len := 11;
    BARCODE_UPCE,
    BARCODE_UPCE_CC: begin
      if (first_len = 7) then zfirst_len := 7;
      if (first_len <= 6) then zfirst_len := 6;
      end;
    BARCODE_ISBNX:
      if (first_len <= 9) then zfirst_len := 9;
  end;


  { Add leading zeroes }
  for i := 0 to (zfirst_len - first_len)-1 do
    uconcat(zfirst_part, '0');
  uconcat(zfirst_part, first_part);

  for i := 0 to (zsecond_len - second_len)-1 do
    uconcat(zsecond_part, '0');
  uconcat(zsecond_part, second_part);

  { Copy adjusted data back to local_source }
  uconcat(local_source, zfirst_part);
  if (zsecond_len <> 0) then
  begin
    uconcat(local_source, '+');
    uconcat(local_source, zsecond_part);
  end;
end;

function eanx(symbol: zint_symbol; const source: TArrayOfByte; src_len : Integer): integer;
var
  first_part, second_part : TArrayOfByte;
  dest : TArrayOfChar;
  local_source : TArrayOfByte;
  writer, reader : Cardinal;
  latch, with_addon : Boolean;
  error_number, i : Integer;
begin
  { splits string to parts before and after '+' parts }
  SetLength(first_part, 20);
  SetLength(second_part, 20);
  SetLength(dest, 1000);
  SetLength(local_source, 41);

  with_addon := FALSE;
  latch := FALSE;
  writer := 0;

  if (src_len > 19) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    Result:=ZERROR_TOO_LONG;
    Exit;
  end;
  if (symbol.symbology <> BARCODE_ISBNX) then
  begin
    { ISBN has it's own checking routine }
    error_number := is_sane('0123456789+', source, src_len);
    if (error_number = ZERROR_INVALID_DATA) then
    begin
      strcpy(symbol.errtxt, 'Invalid characters in data');
      Result:=error_number;
      exit;
    end;
  end
  else
  begin
    error_number := is_sane('0123456789Xx+', source, src_len);
    if (error_number = ZERROR_INVALID_DATA) then
    begin
      strcpy(symbol.errtxt, 'Invalid characters in input');
      result:=error_number;
      Exit;
    end;
  end;

  { Add leading zeroes }
  ustrcpy(local_source, '');
  if (symbol.symbology = BARCODE_ISBNX) then
    to_upper(local_source);

  ean_leading_zeroes(symbol, source, local_source);

  for reader := 0 to ustrlen(local_source) - 1 do
    if (local_source[reader] = Ord('+')) then with_addon := TRUE;

  reader := 0;
  if (with_addon) then
  begin
    repeat
      if (local_source[reader] = Ord('+')) then
      begin
        first_part[writer] := 0;
        latch := TRUE;
        Inc(reader);
        writer := 0;
      end;

      if (latch) then
      begin
        second_part[writer] := local_source[reader];
        Inc(reader);
        Inc(writer);
      end
      else
      begin
        first_part[writer] := local_source[reader];
        Inc(reader);
        Inc(writer);
      end;
    until not (reader <= ustrlen(local_source));
  end
  else
    ustrcpy(first_part, local_source);


  case (symbol.symbology) of
    BARCODE_EANX:
      case(ustrlen(first_part)) of
        2: begin
             add_on(first_part, dest, 0);
             ustrcpy(symbol.text, first_part);
           end;
        5: begin
             add_on(first_part, dest, 0);
             ustrcpy(symbol.text, first_part);
           end;
        7: ean8(symbol, first_part, dest);
        12: ean13(symbol, first_part, dest);
      else
        strcpy(symbol.errtxt, 'Invalid length input');
        Result:=ZERROR_TOO_LONG;
        Exit;
      end;
    BARCODE_EANX_CC:
      case (ustrlen(first_part)) of
      { Adds vertical separator bars according to ISO/IEC 24723 section 11.4 }
        7: begin
              set_module(symbol, symbol.rows, 1);
              set_module(symbol, symbol.rows, 67);
              set_module(symbol, symbol.rows + 1, 0);
              set_module(symbol, symbol.rows + 1, 68);
              set_module(symbol, symbol.rows + 2, 1);
              set_module(symbol, symbol.rows + 1, 67);
              symbol.row_height[symbol.rows] := 2;
              symbol.row_height[symbol.rows + 1] := 2;
              symbol.row_height[symbol.rows + 2] := 2;
              Inc(symbol.rows, 3);
              ean8(symbol, first_part, dest);
          end;
        12: begin
              set_module(symbol, symbol.rows, 1);
              set_module(symbol, symbol.rows, 95);
              set_module(symbol, symbol.rows + 1, 0);
              set_module(symbol, symbol.rows + 1, 96);
              set_module(symbol, symbol.rows + 2, 1);
              set_module(symbol, symbol.rows + 2, 95);
              symbol.row_height[symbol.rows] := 2;
              symbol.row_height[symbol.rows + 1] := 2;
              symbol.row_height[symbol.rows + 2] := 2;
              Inc(symbol.rows, 3);
              ean13(symbol, first_part, dest);
          end;
        else
            strcpy(symbol.errtxt, 'Invalid length EAN input');
            Result:=ZERROR_TOO_LONG;
            Exit;
      end;
    BARCODE_UPCA:
      if (ustrlen(first_part) = 11) then
        upca(symbol, first_part, dest)
      else
      begin
        strcpy(symbol.errtxt, 'Input wrong _length');
        Result:=ZERROR_TOO_LONG;
        Exit;
      end;
    BARCODE_UPCA_CC:
      if (ustrlen(first_part) = 11) then
      begin
        set_module(symbol, symbol.rows, 1);
        set_module(symbol, symbol.rows, 95);
        set_module(symbol, symbol.rows + 1, 0);
        set_module(symbol, symbol.rows + 1, 96);
        set_module(symbol, symbol.rows + 2, 1);
        set_module(symbol, symbol.rows + 2, 95);
        symbol.row_height[symbol.rows] := 2;
        symbol.row_height[symbol.rows + 1] := 2;
        symbol.row_height[symbol.rows + 2] := 2;
        Inc(symbol.rows, 3);
        upca(symbol, first_part, dest);
      end
      else
      begin
        strcpy(symbol.errtxt, 'UPCA input wrong _length');
        Result:=ZERROR_TOO_LONG;
        Exit;
      end;
    BARCODE_UPCE:
      if ((ustrlen(first_part) >= 6) and (ustrlen(first_part) <= 7)) then
        upce(symbol, first_part, dest)
      else
      begin
        strcpy(symbol.errtxt, 'Input wrong _length');
        Result:=ZERROR_TOO_LONG;
        Exit;
      end;
    BARCODE_UPCE_CC:
      if ((ustrlen(first_part) >= 6) and (ustrlen(first_part) <= 7)) then
      begin
        set_module(symbol, symbol.rows, 1);
        set_module(symbol, symbol.rows, 51);
        set_module(symbol, symbol.rows + 1, 0);
        set_module(symbol, symbol.rows + 1, 52);
        set_module(symbol, symbol.rows + 2, 1);
        set_module(symbol, symbol.rows + 2, 51);
        symbol.row_height[symbol.rows] := 2;
        symbol.row_height[symbol.rows + 1] := 2;
        symbol.row_height[symbol.rows + 2] := 2;
        Inc(symbol.rows, 3);
        upce(symbol, first_part, dest);
      end
      else
      begin
        strcpy(symbol.errtxt, 'UPCE input wrong _length');
        Result:=ZERROR_TOO_LONG;
        Exit;
      end;
    BARCODE_ISBNX:
        begin
          error_number := isbn(symbol, first_part, ustrlen(first_part), dest);
          if (error_number > 4) then
          begin
            Result:= error_number;
            Exit;
          end;
        end;

  end;
  case (ustrlen(second_part)) of
    0: begin end;
    2: begin
      add_on(second_part, dest, 1);
      uconcat(symbol.text, '+');
      uconcat(symbol.text, second_part);
      end;
    5: begin
      add_on(second_part, dest, 1);
      uconcat(symbol.text, '+');
      uconcat(symbol.text, second_part);
      end;
    else
      strcpy(symbol.errtxt, 'Invalid length input');
      Result:=ZERROR_TOO_LONG;
      Exit
  end;

  expand(symbol, dest);

  case symbol.symbology of
    BARCODE_EANX_CC,
    BARCODE_UPCA_CC,
    BARCODE_UPCE_CC:
      begin
        { shift the symbol to the right one space to allow for separator bars }
        for i := (symbol.width + 1) downto 1 do
        begin
          if (module_is_set(symbol, symbol.rows - 1, i - 1)<>0) then
            set_module(symbol, symbol.rows - 1, i)
          else
            unset_module(symbol, symbol.rows - 1, i);
        end;
        unset_module(symbol, symbol.rows - 1, 0);
        Inc(symbol.width, 2);
      end;
  end;


  if ((symbol.errtxt[0] = 'w') and (error_number = 0)) then
    error_number := 1; { flag UPC-E warnings }

  Result:=error_number;
end;

end.
