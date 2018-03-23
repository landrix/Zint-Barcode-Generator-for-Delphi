unit zint_common;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b work in progress

  Notes:
    - currently missing: roundup, froundup (maybe not all have to be implemented)
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
 SysUtils, zint;

const
  _TRUE = 1;
  _FALSE = 0;
  SHIFTA = 90;
  LATCHA = 91;
  SHIFTB = 92;
  LATCHB = 93;
  SHIFTC = 94;
  LATCHC = 95;
  AORB = 96;
  ABORC = 97;
  CANDB = 98;
  CANDBB = 99;

// Pascal-specific things
function strlen(const AString : TArrayOfChar) : Integer;
procedure strcpy(var target : TArrayOfChar; const source : TArrayOfChar); overload;
procedure strcpy(var ATarget : TArrayOfChar; const ASource : String); overload;

{ The most commonly used set }
const NEON = '0123456789';

function ustrlen(const data : TArrayOfByte) : Integer;
procedure ustrcpy(var target : TArrayOfByte; const source : TArrayOfByte); overload;
procedure ustrcpy(var ATarget : TArrayOfByte; const ASource : String); overload;
procedure uconcat(var dest : TArrayOfByte; const source : TArrayOfByte); overload;
procedure uconcat(var ADest : TArrayOfByte; const ASource : TArrayOfChar); overload;
procedure uconcat(var ADest : TArrayOfByte; const ASource : String); overload;
procedure concat(var dest : TArrayOfChar; const source : TArrayOfChar); overload;
procedure concat(var ADest: TArrayOfChar; const ASource: String); overload;
procedure concat(var ADest: TArrayOfChar; const ASource: TArrayOfByte); overload;
function ctoi(source : Char) : Integer;
function itoc(source : Integer) : Char;
procedure to_upper(var source : TArrayOfByte);
function is_sane(const test_string : TArrayOfChar; const source : TArrayOfByte; _length : Integer) : Integer; overload;
function is_sane(const ATest_string : String; const ASource : TArrayOfByte; ALength : Integer) : Integer; overload;
function posn(const set_string : TArrayOfChar; const data : Char) : Integer; overload;
function posn(const ASet_string : String; const AData : Byte) : Integer; overload;
function posn(const ASet_string : String; const AData : Char) : Integer; overload;
procedure lookup(const set_string : TArrayOfChar; const table : array of String; const data : Char; var dest : TArrayOfChar); overload;
procedure lookup(const set_string : TArrayOfChar; const table : array of String; const data : Byte; var dest : TArrayOfChar); overload;
procedure lookup(const ASet_string : String; const ATable : array of String; const AData : Byte; var ADest : TArrayOfChar); overload;
procedure lookup(const ASet_string : String; const ATable : array of String; const AData : Char; var ADest : TArrayOfChar); overload;
function module_is_set(symbol : zint_symbol; y_coord : Integer; x_coord : Integer) : Integer;
procedure set_module(symbol : zint_symbol; y_coord : Integer; x_coord : Integer);
procedure unset_module(symbol : zint_symbol; y_coord : Integer; x_coord : Integer);
procedure expand(symbol : zint_symbol; data : TArrayOfChar);
function is_stackable(symbology : Integer) : Integer;
function is_extendable(symbology : Integer) : Integer;
function istwodigits(const source : TArrayOfByte; position : Integer) : Integer;
function froundup(input : Single) : Single;
function parunmodd(llyth : Byte) : Integer; overload;
function parunmodd(llyth : Char) : Integer; overload;
function latin1_process(symbol : zint_symbol; const source : TArrayOfByte; var preprocessed : TArrayOfByte; var _length : Integer) : Integer;

function utf8toutf16(symbol : zint_symbol; source: TArrayOfByte; vals: TArrayOfInteger; var _length : Integer): Integer;

procedure bscan(var binary : TArrayOfChar; data : Integer; h : Integer);

function nitems(a : TArrayOfInteger) : Integer; overload;

implementation

uses zint_helper;

function strlen(const AString: TArrayOfChar): Integer;
var
  i : Integer;
begin
  Result := High(AString) + 1;
  for i := Low(AString) to High(AString) do
    if AString[i] = #0 then
    begin
      Result := i;
      break;
    end;
end;

procedure strcpy(var target: TArrayOfChar; const source: TArrayOfChar);
var
  i, len : Integer;
begin
  len := strlen(source);
  for i := 0 to len - 1 do
    target[i] := source[i];
  target[len] := #0;
end;

procedure strcpy(var ATarget : TArrayOfChar; const ASource : String);
begin
  strcpy(ATarget, StrToArrayOfChar(ASource));
end;

{ Local replacement for strlen() with uint8_t strings }
function ustrlen(const data : TArrayOfByte) : Integer;
var
  i : Integer;
begin
  Result := High(data) - Low(data) + 1;
  for i := Low(data) to High(data) do
    if data[i] = 0 then
    begin
      Result := i - Low(data);
      break;
    end;
end;

{ Local replacement for strcpy() with uint8_t strings }
procedure ustrcpy(var target : TArrayOfByte; const source : TArrayOfByte);
var
  i, len : Integer;
begin
  len := ustrlen(source);
  for i := 0 to len - 1 do
    target[i] := source[i];
  target[len] := 0;
end;

procedure ustrcpy(var ATarget: TArrayOfByte; const ASource : String);
begin
  ustrcpy(ATarget, StrToArrayOfByte(ASource));
end;

procedure uconcat(var ADest: TArrayOfByte; const ASource: TArrayOfChar);
begin
  uconcat(ADest, ArrayOfCharToArrayOfByte(ASource));
end;

procedure uconcat(var ADest: TArrayOfByte; const ASource: String);
begin
  uconcat(ADest, StrToArrayOfByte(ASource));
end;

procedure concat(var dest: TArrayOfChar; const source: TArrayOfChar);
var
  i, j, n : Integer;
begin
  j := strlen(dest);
  n := strlen(source);
  for i := 0 to n do
    dest[i + j] := source[i];
end;

{ Concatinates dest[] with the contents of source[], copying /0 as well }
procedure uconcat(var dest : TArrayOfByte; const source : TArrayOfByte);
var
  i, j, n : Integer;
begin
  j := ustrlen(dest);
  n := ustrlen(source);
  for i := 0 to n do
    dest[i + j] := source[i];
end;

procedure concat(var ADest: TArrayOfChar; const ASource: String);
begin
  concat(ADest, StrToArrayOfChar(ASource));
end;

procedure concat(var ADest: TArrayOfChar; const ASource: TArrayOfByte);
begin
  concat(ADest, ArrayOfByteToString(ASource));
end;

{ Converts a character 0-9 to its equivalent integer value }
function ctoi(source : Char) : Integer;
begin
	if (source >= '0') and (source <= '9') then
		result := Ord(source) - Ord('0')
  else
	  result := Ord(source) - Ord('A') + 10;
end;

{ Converts an integer value to its hexadecimal character }
function itoc(source : Integer) : Char;
begin
  if (source >= 0) and (source <= 9) then
    Result := Chr(Ord('0') + source)
  else
    Result := Chr(Ord('A') + (source - 10));
end;

procedure to_upper(var source : TArrayOfByte);
var
  src_len, i : Integer;
begin
  src_len := ustrlen(source);
  for i := 0 to src_len - 1 do
    if (source[i] >= Ord('a')) and (source[i] <= Ord('z')) then
      source[i] := (source[i] - Ord('a')) + Ord('A');
end;

{ Verifies that a string only uses valid characters }
function is_sane(const test_string : TArrayOfChar; const source : TArrayOfByte; _length : Integer) : Integer;
var
  latch : Cardinal;
  i,j : Cardinal;
  lt : Integer;
begin
  lt := Length(test_string);

	for i := 0 to _length - 1 do
  begin
		latch := _FALSE;
		for j:= 0 to lt - 1 do
    begin
			if (source[i] = Ord(test_string[j])) then
      begin
				latch := _TRUE;
				break;
			end;
    end;
		if not (latch = _TRUE) then
    begin
			result := ZERROR_INVALID_DATA; exit;
		end;
	end;

	result := 0; exit;
end;

function is_sane(const ATest_string: String; const ASource: TArrayOfByte;
  ALength: Integer): Integer;
begin
  Result := is_sane(StrToArrayOfChar(ATest_string), ASource, ALength);
end;

{ Returns the position of data in set_string}
function posn(const set_string : TArrayOfChar; const data : Char) : Integer;
var
  n, i : Integer;
begin
  n := strlen(set_string);

  for i := 0 to n - 1 do
    if (data = set_string[i]) then
    begin
      result := i; exit;
    end;
  result := 0; exit;
end;

function posn(const ASet_string: String; const AData: Byte): Integer;
begin
  Result := posn(StrToArrayOfChar(ASet_string), Chr(AData));
end;

function posn(const ASet_string: String; const AData: Char): Integer;
begin
  Result := posn(StrToArrayOfChar(ASet_string), AData);
end;

{ Replaces huge switch statements for looking up in tables }
procedure lookup(const set_string : TArrayOfChar; const table : array of String; const data : Char; var dest : TArrayOfChar);
var
  n : Integer;
  i : Integer;
begin
  n := strlen(set_string);

	for i := 0 to n - 1 do
		if (data = set_string[i]) then
			concat(dest, StrToArrayOfChar(table[i]));
end;

procedure lookup(const set_string: TArrayOfChar; const table: array of String;
  const data: Byte; var dest: TArrayOfChar);
begin
  lookup(set_string, table, Chr(data), dest);
end;

procedure lookup(const ASet_string : String; const ATable : array of String; const AData : Byte; var ADest : TArrayOfChar);
begin
  lookup(StrToArrayOfChar(ASet_string), ATable, AData, ADest);
end;

procedure lookup(const ASet_string: String; const ATable: array of String;
  const AData: Char; var ADest: TArrayOfChar);
begin
  lookup(ASet_string, ATable, Ord(AData), ADest);
end;

function module_is_set(symbol : zint_symbol; y_coord : Integer; x_coord : Integer) : Integer;
begin
  result := (Ord(symbol.encoded_data[y_coord][x_coord div 7]) shr (x_coord mod 7)) and 1;
end;


procedure set_module(symbol : zint_symbol; y_coord : Integer; x_coord : Integer);
begin
	symbol.encoded_data[y_coord][x_coord div 7] := ((symbol.encoded_data[y_coord][x_coord div 7]) or (1 shl (x_coord mod 7)));
end;

procedure unset_module(symbol : zint_symbol; y_coord : Integer; x_coord : Integer);
begin
	symbol.encoded_data[y_coord][x_coord div 7] := ((symbol.encoded_data[y_coord][x_coord div 7]) and (not (1 shl (x_coord mod 7))));
end;

{ Expands from a width pattern to a bit pattern */ }
procedure expand(symbol : zint_symbol; data : TArrayOfChar);
var
  reader, n : Cardinal;
  writer, i : Integer;
  latch : Char;
begin
  n := strlen(data);
	writer := 0;
	latch := '1';

	for reader := 0 to n - 1 do
  begin
    for i := 0 to ctoi(data[reader]) - 1 do
    begin
			if (latch = '1') then set_module(symbol, symbol.rows, writer);
			Inc(writer);
		end;

    if latch = '1' then latch := '0' else latch := '1';
	end;

	if(symbol.symbology <> BARCODE_PHARMA) then
  begin
		if(writer > symbol.width) then
			symbol.width := writer;
  end
  else
  begin
		{ Pharmacode One ends with a space - adjust for this }
		if(writer > symbol.width + 2) then
			symbol.width := writer - 2;
	end;

	symbol.rows := symbol.rows + 1;
end;

{ Indicates which symbologies can have row binding }
function is_stackable(symbology : Integer) : Integer;
begin
  Result := 0;

	if(symbology < BARCODE_PDF417) then  Result := 1;
	if(symbology = BARCODE_CODE128B) then Result := 1;
	if(symbology = BARCODE_ISBNX) then Result := 1;
	if(symbology = BARCODE_EAN14) then Result := 1;
	if(symbology = BARCODE_NVE18) then Result := 1;
	if(symbology = BARCODE_KOREAPOST) then Result := 1;
	if(symbology = BARCODE_PLESSEY) then Result := 1;
	if(symbology = BARCODE_TELEPEN_NUM) then Result := 1;
	if(symbology = BARCODE_ITF14) then Result := 1;
	if(symbology = BARCODE_CODE32) then Result := 1;
end;

{ Indicates which symbols can have addon }
function is_extendable(symbology : Integer) : Integer;
begin
  Result := 0;

	if (symbology = BARCODE_EANX) then result := 1;
	if (symbology = BARCODE_UPCA) then result := 1;
	if (symbology = BARCODE_UPCE) then result := 1;
	if (symbology = BARCODE_ISBNX) then result := 1;
	if (symbology = BARCODE_UPCA_CC) then result := 1;
	if (symbology = BARCODE_UPCE_CC) then result := 1;
	if (symbology = BARCODE_EANX_CC) then result := 1;
end;

function istwodigits(const source : TArrayOfByte; position : Integer) : Integer;
begin
  if ((source[position] >= Ord('0')) and (source[position] <= Ord('9'))) then
  begin
    if ((source[position + 1] >= Ord('0')) and (source[position + 1] <= Ord('9'))) then
    begin
      result := 1; exit;
    end;
  end;

  result := 0; exit;
end;

function froundup(input : Single) : Single;
var
  fraction, output : Single;
begin
  fraction := 0; output := 0;

  fraction := input - Trunc(input);
  if (fraction > 0.01) then begin output := (input - fraction) + 1.0; end else begin output := input; end;

  result := output;
end;


function parunmodd(llyth : Char) : Integer;
begin
  Result:=parunmodd(Ord(llyth));
end;

function parunmodd(llyth : Byte) : Integer;
var
  modd : Integer;
begin
	modd := SHIFTB;

	if (llyth <= 31) then
    modd := SHIFTA
	else if ((llyth >= 48) and (llyth <= 57)) then
    modd := ABORC
	else if (llyth <= 95) then
    modd := AORB
	else if (llyth <= 127) then
    modd := SHIFTB
	else if (llyth <= 159) then
    modd := SHIFTA
	else if (llyth <= 223) then
    modd := AORB;

	result := modd; exit;
end;

{ Convert Unicode to Latin-1 for those symbologies which only support Latin-1 }
function latin1_process(symbol : zint_symbol; const source : TArrayOfByte; var preprocessed : TArrayOfByte; var _length : Integer) : Integer;
var
  i, j, next : Integer;
begin
  i := 0;
  j := 0;

	repeat
		next := -1;
		if (source[i] < 128) then
    begin
			preprocessed[j] := source[i];
      Inc(j);
			next := i + 1;
    end
    else
    begin
			if (source[i] = $C2) then
      begin
				preprocessed[j] := source[i + 1];
        Inc(j);
				next := i + 2;
      end;

			if(source[i] = $C3) then
      begin
				preprocessed[j] := source[i + 1] + 64;
        Inc(j);
				next := i + 2;
      end;
		end;
		if(next = -1) then
    begin
			strcpy(symbol.errtxt, 'error: Invalid character in input string (only Latin-1 characters supported)');
			result := ZERROR_INVALID_DATA; exit;
    end;
		i := next;
	until not (i < _length);
  preprocessed[j] := 0;
  _length := j;

	result := 0; exit;
end;

procedure bscan(var binary : TArrayOfChar; data : Integer; h : Integer);
begin
  while h <> 0 do
  begin
    if (data and h) <> 0 then
      concat(binary, '1')
    else
      concat(binary, '0');
    h := h shr 1;
  end;
end;

function nitems(a : TArrayOfInteger) : Integer; overload;
begin
  Result := Length(a);
end;

function utf8toutf16(symbol : zint_symbol; source: TArrayOfByte; vals: TArrayOfInteger; var _length : Integer): Integer;
var
  bpos, jpos, error_number : Integer;
  next : Integer;
begin
	bpos := 0;
	jpos := 0;
	error_number := 0;
	next := 0;
	repeat
		if source[bpos] <= $7f then
    begin
			// 1 byte mode (7-bit ASCII)
			vals[jpos] := source[bpos];
			next := bpos + 1;
			inc(jpos);
		end
    else
    begin
			if((source[bpos] >= $80) and (source[bpos] <= $bf)) then
      begin
				strcpy(symbol.errtxt, 'Corrupt Unicode data');
				Result:=ZERROR_INVALID_DATA;
        Exit;
			end;
			if((source[bpos] >= $c0) and (source[bpos] <= $c1)) then
      begin
				strcpy(symbol.errtxt, 'Overlong encoding not supported');
				Result:=ZERROR_INVALID_DATA;
        Exit;
			end;

			if((source[bpos] >= $c2) and (source[bpos] <= $df)) then
      begin
				// 2 byte mode
				vals[jpos] := ((source[bpos] and $1f) shl 6) + (source[bpos + 1] and $3f);
				next := bpos + 2;
				inc(jpos);
			end
      else
			if((source[bpos] >= $e0) and (source[bpos] <= $ef)) then
      begin
				// 3 byte mode
				vals[jpos] := ((source[bpos] and $0f) shl 12) + ((source[bpos + 1] and $3f) shl 6) + (source[bpos + 2] and $3f);
				next := bpos + 3;
				inc(jpos);
			end
      else
			if(source[bpos] >= $f0) then
      begin
				strcpy(symbol.errtxt, 'Unicode sequences of more than 3 bytes not supported');
				Result:= ZERROR_INVALID_DATA;
        Exit;
			end;
		end;

		bpos := next;

	until not (bpos < _length);

	_length := jpos;

	Result:=error_number;
end;


end.

