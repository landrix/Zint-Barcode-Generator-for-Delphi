unit zint_auspost;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b complete
    b3a3c0d complete
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, zint;

function australia_post(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses
  zint_reedsol, zint_common, zint_helper;

const GDSET : String = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz #';

const AusNTable : array[0..9] of String = ('00', '01', '02', '10', '11', '12', '20', '21', '22', '30');

const AusCTable : array[0..63] of String = ('222', '300', '301', '302', '310', '311', '312', '320', '321', '322',
	'000', '001', '002', '010', '011', '012', '020', '021', '022', '100', '101', '102', '110',
	'111', '112', '120', '121', '122', '200', '201', '202', '210', '211', '212', '220', '221',
	'023', '030', '031', '032', '033', '103', '113', '123', '130', '131', '132', '133', '203',
	'213', '223', '230', '231', '232', '233', '303', '313', '323', '330', '331', '332', '333',
	'003', '013');

const AusBarTable : array[0..63] of String = ('000', '001', '002', '003', '010', '011', '012', '013', '020', '021',
	'022', '023', '030', '031', '032', '033', '100', '101', '102', '103', '110', '111', '112',
	'113', '120', '121', '122', '123', '130', '131', '132', '133', '200', '201', '202', '203',
	'210', '211', '212', '213', '220', '221', '222', '223', '230', '231', '232', '233', '300',
	'301', '302', '303', '310', '311', '312', '313', '320', '321', '322', '323', '330', '331',
	'332', '333');

function convert_pattern(data : Char; shift : Integer) : Byte; inline;
begin
  result := (Ord(data) - Ord('0')) shl shift;
end;

{ Adds Reed-Solomon error correction to auspost }
procedure rs_error(var data_pattern : TArrayOfChar; d_pos : Integer);
var
  reader, triple_writer : Integer;
  triple : TArrayOfByte;
  result : TArrayOfByte;
  tmp : Byte;
  RSGlobals : TRSGlobals;
begin
  triple_writer := 0;
  SetLength(triple, 31);
  SetLength(result, 5);

  reader := 2;
  while reader < d_pos do
  begin
    triple[triple_writer] := convert_pattern(data_pattern[reader], 4)
      + convert_pattern(data_pattern[reader + 1], 2)
      + convert_pattern(data_pattern[reader + 2], 0);
    Inc(reader, 3);
    Inc(triple_writer);
  end;

  rs_init_gf($43, RSGlobals);
  rs_init_code(4, 1, RSGlobals);
  rs_encode(triple_writer, triple, result, RSGlobals);

  { Delphi rs_encode does not reverse result (unlike C zint_rs_encode), so reverse here }
  tmp := result[0]; result[0] := result[3]; result[3] := tmp;
  tmp := result[1]; result[1] := result[2]; result[2] := tmp;

  for reader := 0 to 3 do
  begin
    concat(data_pattern, AusBarTable[result[reader]]);
  end;

  rs_free(RSGlobals);
end;

{ Handles Australia Posts's 4 State Codes }
function australia_post(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
{ Customer Standard Barcode, Barcode 2 or Barcode 3 system determined automatically
   (i.e. the FCC doesn't need to be specified by the user) dependent
   on the _length of the input string }

{ The contents of data_pattern conform to the following standard:
   0 := Tracker, Ascender and Descender
   1 := Tracker and Ascender
   2 := Tracker and Descender
   3 := Tracker only }
var
  error_number, zeroes, i : Integer;
  writer : Integer;
  loopey, reader, h : Integer;

  data_pattern : TArrayOfChar;
  fcc : TArrayOfByte;
  localstr : TArrayOfByte;
begin
  SetLength(data_pattern, 200);
  SetLength(fcc, 3);
  fcc[0] := 0; fcc[1] := 0;
  SetLength(localstr, 30);
  error_number := 0;
  zeroes := 0;

  { Do all of the length checking first to avoid stack smashing }
  if (symbol.symbology = BARCODE_AUSPOST) then
  begin
    if (_length <> 8) and (_length <> 13) and (_length <> 16)
       and (_length <> 18) and (_length <> 23) then
    begin
      strcpy(symbol.errtxt, Format('Error 401: Input length %d wrong (8, 13, 16, 18 or 23 characters required)', [_length]));
      result := ZERROR_TOO_LONG; exit;
    end;
  end
  else if (_length > 8) then
  begin
    strcpy(symbol.errtxt, Format('Error 403: Input length %d too long (maximum 8)', [_length]));
    result := ZERROR_TOO_LONG; exit;
  end;

  { Check input immediately to catch invalid chars }
  i := not_sane(GDSET, source, _length);
  if (i <> 0) then
  begin
    strcpy(symbol.errtxt, Format('Error 404: Invalid character at position %d in input (alphanumerics, space and "#" only)', [i]));
    result := ZERROR_INVALID_DATA; exit;
  end;

  if (symbol.symbology = BARCODE_AUSPOST) then
  begin
    { Format control code (FCC) }
    case _length of
      8:
      begin
        fcc[0] := Ord('1'); fcc[1] := Ord('1');
      end;
      13:
      begin
        fcc[0] := Ord('5'); fcc[1] := Ord('9');
      end;
      16:
      begin
        fcc[0] := Ord('5'); fcc[1] := Ord('9');
        i := not_sane(NEON, source, _length);
        if (i <> 0) then
        begin
          strcpy(symbol.errtxt, Format('Error 402: Invalid character at position %d in input (digits only for FCC 59 length 16)', [i]));
          result := ZERROR_INVALID_DATA; exit;
        end;
      end;
      18:
      begin
        fcc[0] := Ord('6'); fcc[1] := Ord('2');
      end;
      23:
      begin
        fcc[0] := Ord('6'); fcc[1] := Ord('2');
        i := not_sane(NEON, source, _length);
        if (i <> 0) then
        begin
          strcpy(symbol.errtxt, Format('Error 406: Invalid character at position %d in input (digits only for FCC 62 length 23)', [i]));
          result := ZERROR_INVALID_DATA; exit;
        end;
      end;
    end;
  end
  else
  begin
    case symbol.symbology of
      BARCODE_AUSREPLY:
      begin fcc[0] := Ord('4'); fcc[1] := Ord('5'); end;
      BARCODE_AUSROUTE:
      begin fcc[0] := Ord('8'); fcc[1] := Ord('7'); end;
      BARCODE_AUSREDIRECT:
      begin fcc[0] := Ord('9'); fcc[1] := Ord('2'); end;
    end;

    { Add leading zeros as required }
    zeroes := 8 - _length;
    FillChar(localstr[0], zeroes, Ord('0'));
  end;

  Move(source[0], localstr[zeroes], _length);
  _length := _length + zeroes;

  { Verify that the first 8 characters are numbers }
  i := not_sane(NEON, localstr, 8);
  if (i <> 0) then
  begin
    strcpy(symbol.errtxt, Format('Error 405: Invalid character at position %d in DPID (first 8 characters) (digits only)', [i]));
    result := ZERROR_INVALID_DATA; exit;
  end;

  { Start character }
  strcpy(data_pattern, '13');

  { Encode the FCC }
  for reader := 0 to 1 do
    lookup(NEON, AusNTable, fcc[reader], data_pattern);

  { Delivery Point Identifier (DPID) }
  for reader := 0 to 7 do
    lookup(NEON, AusNTable, localstr[reader], data_pattern);

  { Customer Information }
  if (_length > 8) then
  begin
    if ((_length = 13) or (_length = 18)) then
    begin
      for reader := 8 to _length - 1 do
        lookup(GDSET, AusCTable, localstr[reader], data_pattern);
    end
    else if ((_length = 16) or (_length = 23)) then
    begin
      for reader := 8 to _length - 1 do
        lookup(NEON, AusNTable, localstr[reader], data_pattern);
    end;
  end;

  { Filler bar }
  h := strlen(data_pattern);
  case h of
  22,
  37,
  52:
    concat(data_pattern, '3');
  end;

  { Reed Solomon error correction }
  rs_error(data_pattern, strlen(data_pattern));

  { Stop character }
  concat(data_pattern, '13');

  { Turn the symbol into a bar pattern ready for plotting }
  writer := 0;
  h := strlen(data_pattern);
  for loopey := 0 to h - 1 do
  begin
    if ((data_pattern[loopey] = '1') or (data_pattern[loopey] = '0')) then
      set_module(symbol, 0, writer);
    set_module(symbol, 1, writer);
    if ((data_pattern[loopey] = '2') or (data_pattern[loopey] = '0')) then
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

