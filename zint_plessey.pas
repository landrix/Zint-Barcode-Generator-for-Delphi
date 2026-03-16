unit zint_plessey;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    b3a3c0d updated to Zint 2.16.0.9 (2026-03-13) - plessey, msi_handle
}

interface

uses
  SysUtils, zint;

function plessey(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function msi_handle(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses zint_common, zint_helper;

const SSET = '0123456789ABCDEF';
const PlessTable : array[0..15] of String = ('13131313', '31131313', '13311313', '31311313', '13133113', '31133113',
	'13313113', '31313113', '13131331', '31131331', '13311331', '31311331', '13133131',
	'31133131', '13313131', '31313131');

const MSITable : array[0..9] of String = ('12121212', '12121221', '12122112', '12122121', '12211212', '12211221',
	'12212112', '12212121', '21121212', '21121221');

{ Convert 0-15 to hex character '0'-'F' }
function xtoc(val : Integer) : Char;
begin
  if val < 10 then
    Result := Chr(Ord('0') + val)
  else
    Result := Chr(Ord('A') + val - 10);
end;

{ Not MSI/Plessey but the older Plessey standard }
function plessey(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
const
  grid : array[0..8] of Byte = (1,1,1,1,0,1,0,0,1);
var
  i, check : Cardinal;
  checkptr : TArrayOfByte;
  dest : TArrayOfChar; { 8 + 67 * 8 + 2 * 8 + 9 + 1 = 570 }
  error_number : Integer;
  j : Integer;
  check_digits : Cardinal;
begin
  SetLength(dest, 570);
  error_number := 0;

  if (_length > 67) then
  begin
    strcpy(symbol.errtxt, Format('Error 370: Input length %d too long (maximum 67)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  for i := 0 to _length - 1 do
  begin
    if not (((source[i] >= Ord('0')) and (source[i] <= Ord('9'))) or
            ((source[i] >= Ord('A')) and (source[i] <= Ord('F')))) then
    begin
      strcpy(symbol.errtxt, Format('Error 371: Invalid character at position %d in input (digits and "ABCDEF" only)', [Integer(i) + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  SetLength(checkptr, _length * 4 + 8);
  FillChar(checkptr[0], Length(checkptr), 0);

  { Start character }
  strcpy(dest, '31311331');

  { Data area }
  for i := 0 to _length - 1 do
  begin
    check := posn(SSET, source[i]);
    lookup(SSET, PlessTable, source[i], dest);
    checkptr[4*i] := check and 1;
    checkptr[4*i+1] := (check shr 1) and 1;
    checkptr[4*i+2] := (check shr 2) and 1;
    checkptr[4*i+3] := (check shr 3) and 1;
  end;

  { CRC check digit code adapted from code by Leonid A. Broukhis
     used in GNU Barcode }

  for i := 0 to (4 * _length) - 1 do
  begin
    if (checkptr[i] <> 0) then
      for j := 0 to 8 do
        checkptr[Integer(i)+j] := checkptr[Integer(i)+j] xor grid[j];
  end;

  check_digits := 0;
  for i := 0 to 7 do
  begin
    case checkptr[_length * 4 + Integer(i)] of
      0: concat(dest, '13');
      1: begin
           concat(dest, '31');
           check_digits := check_digits or (1 shl i);
         end;
    end;
  end;

  { Stop character }
  concat(dest, '331311313');

  expand(symbol, dest);

  { HRT }
  ustrcpy(symbol.text, source);
  if (symbol.option_2 = 1) then
  begin
    uconcat(symbol.text, xtoc(check_digits and $F));
    uconcat(symbol.text, xtoc(check_digits shr 4));
  end;

  if (symbol.output_options and BARCODE_CONTENT_SEGS) <> 0 then
  begin
    SetLength(symbol.content_segs, 1);
    SetLength(symbol.content_segs[0].Source, _length + 2);
    if _length > 0 then
      Move(source[0], symbol.content_segs[0].Source[0], _length);
    symbol.content_segs[0].Source[_length] := Ord(xtoc(check_digits and $F));
    symbol.content_segs[0].Source[_length + 1] := Ord(xtoc(check_digits shr 4));
    symbol.content_segs[0].Length := _length + 2;
    symbol.content_segs[0].ECI := 0;
    symbol.content_segs[0].SourceMode := -1;
    symbol.content_segs_count := 1;
  end;

  result := error_number; exit;
end;

{ Modulo 10 check digit - Luhn algorithm }
function msi_check_digit_mod10(const source : TArrayOfByte; _length : Integer) : Char;
const
  vals : array[0..1, 0..9] of Integer = (
    (0, 2, 4, 6, 8, 1, 3, 5, 7, 9), { Doubled and digits summed }
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)  { Single }
  );
var
  i, x, undoubled : Integer;
begin
  x := 0;
  undoubled := 0;
  for i := _length - 1 downto 0 do
  begin
    x := x + vals[undoubled][ctoi(Chr(source[i]))];
    undoubled := 1 - undoubled;
  end;
  Result := itoc((10 - x mod 10) mod 10);
end;

{ Modulo 11 check digit - IBM weight system wrap = 7, NCR system wrap = 9 }
function msi_check_digit_mod11(const source : TArrayOfByte; _length : Integer; wrap : Integer) : Char;
var
  i, x, weight : Integer;
begin
  x := 0;
  weight := 2;
  for i := _length - 1 downto 0 do
  begin
    x := x + weight * ctoi(Chr(source[i]));
    Inc(weight);
    if weight > wrap then
      weight := 2;
  end;
  Result := itoc((11 - x mod 11) mod 11); { Returns 'A' for 10 }
end;

{ MSI Plessey }
function msi_handle(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  i : Integer;
  dest : TArrayOfChar;
  check_option : Integer;
  no_checktext : Integer;
  wrap : Integer;
  check_digit : Char;
  local_source : TArrayOfByte;
  local_length : Integer;
begin
  if (_length > 92) then
  begin
    strcpy(symbol.errtxt, Format('Error 372: Input length %d too long (maximum 92)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  for i := 0 to _length - 1 do
  begin
    if (source[i] < Ord('0')) or (source[i] > Ord('9')) then
    begin
      strcpy(symbol.errtxt, Format('Error 377: Invalid character at position %d in input (digits only)', [i + 1]));
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  check_option := symbol.option_2;
  no_checktext := 0;

  if (check_option >= 11) and (check_option <= 16) then
  begin
    Dec(check_option, 10);
    no_checktext := 1;
  end;
  if (check_option < 0) or (check_option > 6) then
    check_option := 0;

  SetLength(dest, 800); { 2 + 92*8 + 3*8 + 3 + 1 = 766 }
  SetLength(local_source, _length + 4); { Room for up to 3 check digits }
  Move(source[0], local_source[0], _length);
  local_length := _length;

  { Start character }
  strcpy(dest, '21');

  { Data area }
  for i := 0 to _length - 1 do
    lookup(NEON, MSITable, source[i], dest);

  { Determine wrap for mod-11 }
  wrap := 7;
  if check_option in [5, 6] then
    wrap := 9;

  { First check digit }
  if check_option in [1, 2] then
  begin
    { Mod 10 }
    check_digit := msi_check_digit_mod10(local_source, local_length);
    local_source[local_length] := Ord(check_digit);
    Inc(local_length);
    lookup(NEON, MSITable, Ord(check_digit), dest);
  end
  else if check_option in [3, 4, 5, 6] then
  begin
    { Mod 11 }
    check_digit := msi_check_digit_mod11(local_source, local_length, wrap);
    if check_digit = 'A' then { itoc(10) returns 'A' in Delphi (hex), ':' in C }
    begin
      local_source[local_length] := Ord('1'); Inc(local_length);
      local_source[local_length] := Ord('0'); Inc(local_length);
      lookup(NEON, MSITable, Ord('1'), dest);
      lookup(NEON, MSITable, Ord('0'), dest);
    end
    else
    begin
      local_source[local_length] := Ord(check_digit);
      Inc(local_length);
      lookup(NEON, MSITable, Ord(check_digit), dest);
    end;
  end;

  { Second check digit (mod 10) for options 2, 4, 6 }
  if check_option in [2, 4, 6] then
  begin
    check_digit := msi_check_digit_mod10(local_source, local_length);
    local_source[local_length] := Ord(check_digit);
    Inc(local_length);
    lookup(NEON, MSITable, Ord(check_digit), dest);
  end;

  { Stop character }
  concat(dest, '121');

  expand(symbol, dest);

  { HRT }
  if (no_checktext <> 0) or (check_option = 0) then
    ustrcpy(symbol.text, source)
  else
  begin
    local_source[local_length] := 0;
    ustrcpy(symbol.text, local_source);
  end;

  if (symbol.output_options and BARCODE_CONTENT_SEGS) <> 0 then
  begin
    SetLength(symbol.content_segs, 1);
    SetLength(symbol.content_segs[0].Source, local_length);
    if local_length > 0 then
      Move(local_source[0], symbol.content_segs[0].Source[0], local_length);
    symbol.content_segs[0].Length := local_length;
    symbol.content_segs[0].ECI := 0;
    symbol.content_segs[0].SourceMode := -1;
    symbol.content_segs_count := 1;
  end;

  result := 0;
end;


end.

