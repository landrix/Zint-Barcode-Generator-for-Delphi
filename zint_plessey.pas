unit zint_plessey;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b complete
}

interface

uses
  SysUtils, zint;

function plessey(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
function msi_handle(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses zint_common, zint_helper;

const SSET = '0123456789ABCDEF';
const PlessTable : array[0..15] of String = ('13131313', '31131313', '13311313', '31311313', '13133113', '31133113',
	'13313113', '31313113', '13131331', '31131331', '13311331', '31311331', '13133131',
	'31133131', '13313131', '31313131');

const MSITable : array[0..9] of String = ('12121212', '12121221', '12122112', '12122121', '12211212', '12211221',
	'12212112', '12212121', '21121212', '21121221');

{ Not MSI/Plessey but the older Plessey standard }
function plessey(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
const
  grid : array[0..8] of Byte = (1,1,1,1,0,1,0,0,1);
var
  i, check : Cardinal;
  checkptr : TArrayOfByte;
  dest : TArrayOfChar; { 8 + 65 * 8 + 8 * 2 + 9 + 1 ~ 1024 }
  error_number : Integer;
  j : Integer;
begin
  SetLength(dest, 1024);
  //error_number := 0;

  if (_length > 65) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(SSET, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;
  SetLength(checkptr, _length * 4 + 8);

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

  for i := 0 to 7 do
  begin
    case checkptr[_length * 4 + Integer(i)] of
      0: concat(dest, '13');
      1: concat(dest, '31');
    end;
  end;

  { Stop character }
  concat(dest, '331311313');

  expand(symbol, dest);
  ustrcpy(symbol.text, source);
  SetLength(checkptr, 0);
  result := error_number; exit;
end;

{ Plain MSI Plessey - does not calculate any check character }
function msi_plessey(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  i : Integer;
  dest : TArrayOfChar; { 2 + 55 * 8 + 3 + 1 ~ 512 }
begin
  SetLength(dest, 512);

  if (_length > 55) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  { start character }
  strcpy(dest, '21');

  for i := 0 to _length - 1 do
    lookup(NEON, MSITable, source[i], dest);

  { Stop character }
  concat (dest, '121');

  expand(symbol, dest);
  ustrcpy(symbol.text, source);
  result := 0; exit;
end;

{ MSI Plessey with Modulo 10 check digit - algorithm from Barcode Island
  http://www.barcodeisland.com/ }
function msi_plessey_mod10(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  i, wright, dau, pedwar, pump, n : Integer;
  un, tri : TArrayOfChar;
  error_number, h : Integer;
  dest : TArrayOfChar;
begin
  SetLength(un, 200);
  SetLength(tri, 32);
  SetLength(dest, 1000);

  error_number := 0;

  if (_length > 18) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  { start character }
  strcpy(dest, '21');

  { draw data section }
  for i := 0 to _length - 1 do
    lookup(NEON, MSITable, source[i], dest);

  { caluculate check digit }
  wright := 0;
  if not ((_length and 1) <> 0) then n := 1 else n := 0;
  i := n;
  while i < _length do
  begin
    un[wright] := Chr(source[i]);
    Inc(wright);
    Inc(i, 2);
  end;
  un[wright] := #0;

  dau := StrToInt(ArrayOfCharToString(un));
  dau := dau * 2;

  tri := StrToArrayOfChar(IntToStr(dau));

  pedwar := 0;
  h := strlen(tri);
  for i := 0 to h - 1 do
    Inc(pedwar, ctoi(tri[i]));

  if ((_length and 1) <> 0) then n := 1 else n := 0;
  i := n;
  while i < _length do
  begin
    Inc(pedwar, ctoi(Chr(source[i])));
    Inc(i, 2);
  end;

  pump := (10 - pedwar mod 10);
  if (pump = 10) then
    pump := 0;

  { draw check digit }
  lookup(NEON, MSITable, itoc(pump), dest);

  { Stop character }
  concat (dest, '121');
  expand(symbol, dest);

  ustrcpy(symbol.text, source);
  symbol.text[_length] := Ord(itoc(pump));
  symbol.text[_length + 1] := 0;
  result := error_number; exit;
end;

{ MSI Plessey with two Modulo 10 check digits - algorithm from
  Barcode Island http://www.barcodeisland.com/ }
function msi_plessey_mod1010(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;
var
  i, n, wright, dau, pedwar, pump, chwech : Integer;
  un, tri : TArrayOfChar;
  error_number, h : Integer;
  dest : TArrayOfChar;
begin
  SetLength(un, 16);
  SetLength(tri, 32);
  SetLength(dest, 1000);

  error_number := 0;

  if (src_len > 18) then
  begin { No Entry Stack Smashers! limit because of str.number conversion}
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  { start character }
  strcpy(dest, '21');

  { draw data section }
  for i := 0 to src_len - 1 do
    lookup(NEON, MSITable, source[i], dest);

  { calculate first check digit }
  wright := 0;

  if not ((src_len and 1) <> 0) then n := 1 else n := 0;
  i := n;
  while i < src_len do
  begin
    un[wright] := Chr(source[i]);
    Inc(wright);
    Inc(i, 2);
  end;
  un[wright] := #0;

  dau := StrToInt(ArrayOfCharToString(un));
  dau := dau * 2;

  tri := StrToArrayOfChar(IntToStr(dau));

  pedwar := 0;
  h := strlen(tri);
  for i := 0 to h - 1 do
    Inc(pedwar, ctoi(tri[i]));

  if ((src_len and 1) <> 0) then n := 1 else n := 0;
  i := n;
  while i < src_len do
  begin
    Inc(pedwar, ctoi(Chr(source[i])));
    Inc(i, 2);
  end;

  pump := 10 - pedwar mod 10;
  if (pump = 10) then
    pump := 0;

  { calculate second check digit }
  wright := 0;
  if ((src_len and 1) <> 0) then n := 1 else n := 0;
  i := n;
  while i < src_len do
  begin
    un[wright] := Chr(source[i]);
    Inc(wright);
    Inc(i, 2);
  end;
  un[wright] := itoc(pump);
  Inc(wright);
  un[wright] := #0;

  dau := StrToInt(ArrayOfCharToString(un));
  dau := dau * 2;

  tri := StrToArrayOfChar(IntToStr(dau));

  pedwar := 0;
  h := strlen(tri);
  for i := 0 to h - 1 do
    Inc(pedwar, ctoi(tri[i]));

  if not ((src_len and 1) <> 0) then i := 1 else i := 0;
  while i < src_len do
  begin
    Inc(pedwar, ctoi(Chr(source[i])));
    Inc(i, 2);
  end;

  chwech := 10 - pedwar mod 10;
  if (chwech = 10) then
    chwech := 0;

  { Draw check digits }
  lookup(NEON, MSITable, itoc(pump), dest);
  lookup(NEON, MSITable, itoc(chwech), dest);

  { Stop character }
  concat (dest, '121');

  expand(symbol, dest);

  ustrcpy(symbol.text, source);
  symbol.text[src_len] := Ord(itoc(pump));
  symbol.text[src_len + 1] := Ord(itoc(chwech));
  symbol.text[src_len + 2] := 0;

  result := error_number; exit;
end;

{ Calculate a Modulo 11 check digit using the system discussed on Wikipedia -
  see http://en.wikipedia.org/wiki/Talk:MSI_Barcode }
{ uses the IBM weight system }
function msi_plessey_mod11(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;
var
  i, weight, x, check : Integer;
  error_number : Integer;
  dest : TArrayOfChar;
begin
  SetLength(dest, 1000);

  error_number := 0;

  if (src_len > 55) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  { start character }
  strcpy(dest, '21');

  { draw data section }
  for i := 0 to src_len - 1 do
    lookup(NEON, MSITable, source[i], dest);

  { calculate check digit }
  x := 0;
  weight := 2;
  for i := src_len - 1 downto 0 do
  begin
    Inc(x, weight * ctoi(Chr(source[i])));
    Inc(weight);
    if (weight > 7) then
      weight := 2;
  end;

  check := (11 - (x mod 11)) mod 11;
  if (check = 10) then
  begin
    lookup(NEON, MSITable, '1', dest);
    lookup(NEON, MSITable, '0', dest);
  end
  else
  begin
    lookup(NEON, MSITable, itoc(check), dest);
  end;

  { stop character }
  concat (dest, '121');

  expand(symbol, dest);

  ustrcpy(symbol.text, source);
  if (check = 10) then
    uconcat(symbol.text, '10')
  else
    uconcat(symbol.text, itoc(check));

  result := error_number; exit;
end;

{ Combining the Barcode Island and Wikipedia code }
{ Verified against http://www.bokai.com/BarcodeJSP/applet/BarcodeSampleApplet.htm }
{ Weighted using the IBM system }
function msi_plessey_mod1110(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;
var
  i, weight, x, check, wright, dau, pedwar, pump, h : Integer;
  un, tri : TArrayOfChar;
  error_number : Integer;
  dest : TArrayOfChar;
  temp : TArrayOfByte;
  temp_len : Integer;
begin
  SetLength(un, 16);
  SetLength(tri, 16);
  SetLength(dest, 1000);
  SetLength(temp, 32);

  error_number := 0;

  if (src_len > 18) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  { start character }
  strcpy(dest, '21');

  { draw data section }
  for i := 0 to src_len - 1 do
    lookup(NEON, MSITable, source[i], dest);

  { calculate first (mod 11) digit }
  x := 0;
  weight := 2;
  for i := src_len - 1 downto 0 do
  begin
    Inc(x, weight * ctoi(Chr(source[i])));
    Inc(weight);
    if (weight > 7) then
      weight := 2;
  end;

  check := (11 - (x mod 11)) mod 11;
  ustrcpy(temp, source);
  temp_len := src_len;
  if (check = 10) then
  begin
    lookup(NEON, MSITable, '1', dest);
    lookup(NEON, MSITable, '0', dest);
    uconcat(temp, '10');
    Inc(temp_len, 2);
  end
  else
  begin
    lookup(NEON, MSITable, itoc(check), dest);
    temp[temp_len] := Ord(itoc(check));
    Inc(temp_len);
    temp[temp_len] := 0;
  end;

  { caluculate second (mod 10) check digit }
  wright := 0;
  if not ((temp_len and 1) <> 0) then i := 1 else i := 0;
  while i < temp_len do
  begin
    un[wright] := Chr(temp[i]);
    Inc(wright);
    Inc(i, 2);
  end;
  un[wright] := #0;

  dau := StrToInt(ArrayOfCharToString(un));
  dau := dau * 2;

  tri := StrToArrayOfChar(IntToStr(dau));

  pedwar := 0;
  h := strlen(tri);
  for i := 0 to h - 1 do
    Inc(pedwar, ctoi(tri[i]));

  if ((temp_len and 1) <> 0) then i := 1 else i := 0;
  while i < temp_len do
  begin
    Inc(pedwar, ctoi(Chr(temp[i])));
    Inc(i, 2)
  end;

  pump := 10 - pedwar mod 10;
  if (pump = 10) then
    pump := 0;

  { draw check digit }
  lookup(NEON, MSITable, itoc(pump), dest);

  { stop character }
  concat (dest, '121');
  expand(symbol, dest);

  temp[temp_len] := Ord(itoc(pump));
  Inc(temp_len);
  temp[temp_len] := 0;


  ustrcpy(symbol.text, temp);
  result := error_number; exit;
end;

function msi_handle(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number : Integer;
begin
  error_number := is_sane(NEON, source, _length);
  if (error_number <> 0) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in input data');
    result := ZERROR_INVALID_DATA; exit;
  end;

  if ((symbol.option_2 < 0) or (symbol.option_2 > 4)) then
    symbol.option_2 := 0;

  case symbol.option_2 of
    0: error_number := msi_plessey(symbol, source, _length);
    1: error_number := msi_plessey_mod10(symbol, source, _length);
    2: error_number := msi_plessey_mod1010(symbol, source, _length);
    3: error_number := msi_plessey_mod11(symbol, source, _length);
    4: error_number := msi_plessey_mod1110(symbol, source, _length);
  end;

  result := error_number; exit;
end;


end.

