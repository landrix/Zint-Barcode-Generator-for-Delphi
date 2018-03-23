unit zint_auspost;

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
procedure rs_error(var data_pattern : TArrayOfChar);
var
  reader, triple_writer : Integer;
  triple, inv_triple : TArrayOfByte;
  result : TArrayOfByte;
  RSGlobals : TRSGlobals;
begin
  triple_writer := 0;
  SetLength(triple, 31);
  SetLength(inv_triple, 31);
  SetLength(result, 5);

  reader := 2;
  while reader < strlen(data_pattern) do
  begin
    triple[triple_writer] := convert_pattern(data_pattern[reader], 4)
      + convert_pattern(data_pattern[reader + 1], 2)
      + convert_pattern(data_pattern[reader + 2], 0);
    Inc(reader, 3);
    Inc(triple_writer);
  end;

  for reader := 0 to triple_writer - 1 do
    inv_triple[reader] := triple[(triple_writer - 1) - reader];

  rs_init_gf($43, RSGlobals);
  rs_init_code(4, 1, RSGlobals);
  rs_encode(triple_writer, inv_triple, result, RSGlobals);

  for reader := 4 downto 1 do
    concat(data_pattern, AusBarTable[result[reader - 1]]);

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
  error_number, zeroes : Integer;
  writer : Integer;
  loopey, reader, h : Cardinal;

  data_pattern : TArrayOfChar;
  fcc, dpid : TArrayOfChar;
  localstr : TArrayOfChar;
begin
  SetLength(data_pattern, 200);
  SetLength(fcc, 3); fcc[0] := #0; fcc[1] := #0;
  SetLength(dpid, 10);
  SetLength(localstr, 30);
  error_number := 0;
  strcpy(localstr, '');


  { Do all of the _length checking first to avoid stack smashing }
  if (symbol.symbology = BARCODE_AUSPOST) then
  begin
    { Format control code (FCC) }
    case _length of
      8:
        strcpy(fcc, '11');
      16:
      begin
        error_number := is_sane(NEON, source, _length);
        strcpy(fcc, '59');
      end;
      13:
        strcpy(fcc, '59');
      23:
      begin
        error_number := is_sane(NEON, source, _length);
        strcpy(fcc, '62');
      end;
      18:
        strcpy(fcc, '62');
      else
        strcpy(symbol.errtxt, 'Auspost input is wrong length');
        result := ZERROR_TOO_LONG; exit;
    end;
    if (error_number = ZERROR_INVALID_DATA) then
    begin
      strcpy(symbol.errtxt, 'Invalid characters in data');
      result := error_number; exit;
    end;
  end
  else
  begin
    if (_length > 8) then
    begin
      strcpy(symbol.errtxt, 'Auspost input is too long');
      result := ZERROR_TOO_LONG; exit;
    end;
    case symbol.symbology of
      BARCODE_AUSREPLY:
        strcpy(fcc, '45');
      BARCODE_AUSROUTE:
        strcpy(fcc, '87');
      BARCODE_AUSREDIRECT:
        strcpy(fcc, '92');
    end;

    { Add leading zeros as required }
    zeroes := 8 - _length;
    FillChar(localstr[0], zeroes, '0');
    localstr[8] := #0;
  end;

  concat(localstr, source);
  h := strlen(localstr);
  error_number := is_sane(GDSET, ArrayOfCharToArrayOfByte(localstr), h);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  { Verifiy that the first 8 characters are numbers }
  ArrayCopy(dpid, localstr, 8);
  dpid[8] := #0;
  error_number := is_sane(NEON, ArrayOfCharToArrayOfByte(dpid), strlen(dpid));
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in DPID');
    result := error_number; exit;
  end;

  { Start character }
  strcpy(data_pattern, '13');

  { Encode the FCC }
  for reader := 0  to 1 do
    lookup(NEON, AusNTable, fcc[reader], data_pattern);

  { Delivery Point Identifier (DPID) }
  for reader := 0 to 7 do
    lookup(NEON, AusNTable, dpid[reader], data_pattern);

  { Customer Information }
  if (h > 8) then
  begin
    if ((h = 13) or (h = 18)) then
    begin
      for reader := 8 to h - 1 do
        lookup(GDSET, AusCTable, localstr[reader], data_pattern);
    end
    else if ((h = 16) or (h = 23)) then
    begin
      for reader := 8 to h - 1 do
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
  rs_error(data_pattern);

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

