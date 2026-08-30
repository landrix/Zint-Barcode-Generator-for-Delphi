unit zint_code128;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    b3a3c0d complete (code128.c + code128_based.c)
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  zint;

function code_128(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function ean_128(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function gs1_128_cc(symbol : zint_symbol; source : TArrayOfByte; _length : Integer; cc_mode : Integer; cc_rows : Integer) : Integer;
function nve_18(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function ean_14(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses
  SysUtils, zint_common, zint_gs1, zint_helper;

const
  C128_SYMBOL_MAX   = 102;   { 102 * 10 + 10 (check digit) + 13 (Stop) = 1043 }
  C128_MAX          = 256;
  C128_VALUES_MAX   = C128_SYMBOL_MAX + 2; { Allow for check digit and Stop }

  { Code Set states }
  C128_A0     = 1;
  C128_B0     = 2;
  C128_A1     = 3;
  C128_B1     = 4;
  C128_C0     = 5;
  C128_C1     = 6;
  C128_STATES = 7;

  { Code 128 tables checked against ISO/IEC 15417:2007 }
  C128Table : array[0..106] of String = ('212222', '222122', '222221', '121223', '121322', '131222', '122213',
  	'122312', '132212', '221213', '221312', '231212', '112232', '122132', '122231', '113222',
  	'123122', '123221', '223211', '221132', '221231', '213212', '223112', '312131', '311222',
  	'321122', '321221', '312212', '322112', '322211', '212123', '212321', '232121', '111323',
  	'131123', '131321', '112313', '132113', '132311', '211313', '231113', '231311', '112133',
  	'112331', '132131', '113123', '113321', '133121', '313121', '211331', '231131', '213113',
  	'213311', '213131', '311123', '311321', '331121', '312113', '312311', '332111', '314111',
  	'221411', '431111', '111224', '111422', '121124', '121421', '141122', '141221', '112214',
  	'112412', '122114', '122411', '142112', '142211', '241211', '221114', '413111', '241112',
  	'134111', '111242', '121142', '121241', '114212', '124112', '124211', '411212', '421112',
  	'421211', '212141', '214121', '412121', '111143', '111341', '131141', '114113', '114311',
  	'411113', '411311', '113141', '114131', '311141', '411131', '211412', '211214', '211232',
  	'2331112');

  { Latch sequences between Code Set states [prior_cset][cset][0..2] }
  c128_latch_seq : array[0..6, 0..6, 0..2] of Byte = (
    { 0 (unused) }
    ((0,0,0), (0,0,0), (0,0,0), (0,0,0), (0,0,0), (0,0,0), (0,0,0)),
    { A0 }
    ((0,0,0), (0,0,0), (100,0,0), (101,101,0), (100,100,100), (99,0,0), (101,101,99)),
    { B0 }
    ((0,0,0), (101,0,0), (0,0,0), (101,101,101), (100,100,0), (99,0,0), (100,100,99)),
    { A1 }
    ((0,0,0), (101,101,0), (100,100,100), (0,0,0), (100,0,0), (101,101,99), (99,0,0)),
    { B1 }
    ((0,0,0), (101,101,101), (100,100,0), (101,0,0), (0,0,0), (100,100,99), (99,0,0)),
    { C0 }
    ((0,0,0), (101,0,0), (100,0,0), (101,101,101), (100,100,100), (0,0,0), (0,0,0)),
    { C1 }
    ((0,0,0), (101,101,101), (100,100,100), (101,0,0), (100,0,0), (0,0,0), (0,0,0))
  );

  { Lengths of latch sequences }
  c128_latch_len : array[0..6, 0..6] of Byte = (
    (0, 0, 0, 0, 0, 0, 0),
    (0, 0, 1, 2, 3, 1, 3),  { A0 }
    (0, 1, 0, 3, 2, 1, 3),  { B0 }
    (0, 2, 3, 0, 1, 3, 1),  { A1 }
    (0, 3, 2, 1, 0, 3, 1),  { B1 }
    (0, 1, 1, 3, 3, 0, 64), { C0 }
    (0, 3, 3, 1, 1, 64, 0)  { C1 }
  );

  { Start latch sequences for Normal[0], GS1_MODE[1], READER_INIT[2] }
  c128_start_latch_seq : array[0..2, 0..6, 0..3] of Byte = (
    { Normal }
    ((0,0,0,0), (103,0,0,0), (104,0,0,0), (103,101,101,0), (104,100,100,0), (105,0,0,0), (0,0,0,0)),
    { GS1_MODE }
    ((0,0,0,0), (103,102,0,0), (104,102,0,0), (103,102,101,101), (104,102,100,100), (105,102,0,0), (0,0,0,0)),
    { READER_INIT }
    ((0,0,0,0), (103,96,0,0), (104,96,0,0), (103,96,101,101), (104,96,100,100), (104,96,99,0), (0,0,0,0))
  );

  { Lengths of start latch sequences }
  c128_start_latch_len : array[0..2, 0..6] of Byte = (
    (0, 1, 1, 3, 3, 1, 64),  { Normal }
    (0, 2, 2, 4, 4, 2, 64),  { GS1_MODE }
    (0, 2, 2, 4, 4, 3, 64)   { READER_INIT }
  );

type
  TCostRow = array[0..6] of SmallInt;
  TModeRow = array[0..6] of ShortInt;
  TCostsArray = array of TCostRow;
  TModesArray = array of TModeRow;
  TPriorityArray = array[0..6] of Byte;
  TFncManualArray = array[0..C128_MAX - 1] of Byte;
  TValuesArray = array[0..C128_VALUES_MAX - 1] of Integer;

{ Output cost (length) for Code Sets A/B }
function c128_cost_ab(cset : Integer; ch : Byte; var p_mode : Integer) : Integer;
var
  mask_0x60 : Byte;
  is_a : Boolean;
begin
  mask_0x60 := ch and $60;
  is_a := (cset and 1) <> 0; { C128_A0A1 }
  Result := 1;

  { SHIFT }
  if (is_a and (mask_0x60 = $60)) or ((not is_a) and (mask_0x60 = 0)) then
  begin
    Inc(Result);
    p_mode := p_mode or $10;
  end;

  { FNC4 }
  if (cset <= C128_B0) = (ch >= 128) then
  begin
    Inc(Result);
    p_mode := p_mode or $20;
  end;
end;

{ Calculate encoding cost from position i starting in prior_cset (DAC-DM) }
function c128_cost(const source : TArrayOfByte; _length, i, prior_cset, start_idx : Integer;
  const priority : TPriorityArray; const fncs : TFncManualArray; const manuals : TFncManualArray;
  var costs : TCostsArray; var modes : TModesArray) : Integer;
var
  ch : Byte;
  is_fnc1, can_c, manual_c_fail : Boolean;
  min_cost, min_mode, p, cset, cost, mode, incr : Integer;
  latch_len_ptr : Integer; { 0 = use start_latch_len, 1..6 = use c128_latch_len[prior_cset] }
begin
  ch := source[i];
  latch_len_ptr := prior_cset;
  is_fnc1 := (ch = $1D) and (fncs[i] <> 0);
  can_c := is_fnc1 or ((ch >= Ord('0')) and (ch <= Ord('9')) and (source[i + 1] >= Ord('0')) and (source[i + 1] <= Ord('9')));
  manual_c_fail := (not can_c) and (manuals[i] = C128_C0);
  min_cost := 999999;
  min_mode := 0;

  p := 0;
  while priority[p] <> 0 do
  begin
    cset := priority[p];
    if cset >= C128_C0 then { C128_C0C1 }
    begin
      if can_c and ((manuals[i] = 0) or (manuals[i] = C128_C0)) then
      begin
        if is_fnc1 then incr := 1 else incr := 2;
        mode := prior_cset;
        cost := 1;
        if prior_cset <> cset then
        begin
          if latch_len_ptr = 0 then
            Inc(cost, c128_start_latch_len[start_idx][cset])
          else
            Inc(cost, c128_latch_len[latch_len_ptr][cset]);
          mode := cset;
        end;
        if i + incr < _length then
        begin
          if costs[i + incr][cset] <> 0 then
            Inc(cost, costs[i + incr][cset])
          else
            Inc(cost, c128_cost(source, _length, i + incr, cset, 0, priority, fncs, manuals, costs, modes));
        end;
        if cost < min_cost then
        begin
          min_cost := cost;
          min_mode := mode;
        end;
      end;
    end
    else
    begin
      { A/B sets }
      { C128_AB(cset): maps A0,A1->1, B0,B1->2 }
      if (manuals[i] = 0) or (manuals[i] = (cset shr Ord(cset > C128_B0))) or manual_c_fail then
      begin
        mode := cset;
        if is_fnc1 then
          cost := 1
        else
          cost := c128_cost_ab(cset, ch, mode);
        if prior_cset <> cset then
        begin
          if latch_len_ptr = 0 then
            Inc(cost, c128_start_latch_len[start_idx][cset])
          else
            Inc(cost, c128_latch_len[latch_len_ptr][cset]);
        end;
        if i + 1 < _length then
        begin
          if costs[i + 1][cset] <> 0 then
            Inc(cost, costs[i + 1][cset])
          else
            Inc(cost, c128_cost(source, _length, i + 1, cset, 0, priority, fncs, manuals, costs, modes));
        end;
        if cost < min_cost then
        begin
          min_cost := cost;
          min_mode := mode;
        end;
      end;
    end;
    Inc(p);
  end;

  costs[i][prior_cset] := min_cost;
  modes[i][prior_cset] := min_mode;
  Result := min_cost;
end;

{ Build optimal encoding sequence using DAC-DM }
function c128_set_values(const source : TArrayOfByte; _length, start_idx : Integer;
  const priority : TPriorityArray; const fncs : TFncManualArray; const manuals : TFncManualArray;
  var values : TValuesArray; p_final_cset : PInteger) : Integer;
var
  costs : TCostsArray;
  modes : TModesArray;
  glyph_count, cset, i, j : Integer;
  ch : Byte;
  is_fnc1 : Boolean;
  mode, prior_cset : Integer;
begin
  SetLength(costs, _length);
  SetLength(modes, _length);
  for i := 0 to _length - 1 do
  begin
    FillChar(costs[i], SizeOf(TCostRow), 0);
    FillChar(modes[i], SizeOf(TModeRow), 0);
  end;

  c128_cost(source, _length, 0, 0, start_idx, priority, fncs, manuals, costs, modes);

  if costs[0][0] > C128_SYMBOL_MAX then
  begin
    Result := costs[0][0];
    Exit;
  end;

  { Output codewords }
  glyph_count := 0;
  cset := 0;
  i := 0;
  while i < _length do
  begin
    ch := source[i];
    is_fnc1 := (ch = $1D) and (fncs[i] <> 0);
    mode := modes[i][cset];
    prior_cset := cset;

    cset := mode and $0F;
    if cset <> prior_cset then
    begin
      if prior_cset = 0 then
      begin
        for j := 0 to c128_start_latch_len[start_idx][cset] - 1 do
        begin
          values[glyph_count] := c128_start_latch_seq[start_idx][cset][j];
          Inc(glyph_count);
        end;
      end
      else
      begin
        for j := 0 to c128_latch_len[prior_cset][cset] - 1 do
        begin
          values[glyph_count] := c128_latch_seq[prior_cset][cset][j];
          Inc(glyph_count);
        end;
      end;
    end;

    if mode >= $30 then
    begin
      { Extended Shift A/B }
      values[glyph_count] := 100 + Ord((cset and 1) <> 0); { FNC4 }
      Inc(glyph_count);
      values[glyph_count] := 98; { SHIFT }
      Inc(glyph_count);
    end
    else if mode >= $20 then
    begin
      { Extended A/B }
      values[glyph_count] := 100 + Ord((cset and 1) <> 0); { FNC4 }
      Inc(glyph_count);
    end
    else if mode >= $10 then
    begin
      { Shift A/B }
      values[glyph_count] := 98; { SHIFT }
      Inc(glyph_count);
    end;

    if is_fnc1 then
    begin
      values[glyph_count] := 102; { FNC1 }
      Inc(glyph_count);
    end
    else if cset >= C128_C0 then
    begin
      values[glyph_count] := (ch - Ord('0')) * 10 + source[i + 1] - Ord('0');
      Inc(glyph_count);
      Inc(i); { Extra increment for Code C pair }
    end
    else
    begin
      { (ch & 0x7F) < 32 ? (ch & 0x7F) + 64 : (ch & 0x7F) - 32 }
      if (ch and $60) = 0 then
        values[glyph_count] := (ch and $7F) + 64
      else
        values[glyph_count] := (ch and $7F) - 32;
      Inc(glyph_count);
    end;
    Inc(i);
  end;

  if p_final_cset <> nil then
    p_final_cset^ := cset;

  Result := glyph_count;
end;

{ Write output symbol, calculating check digit }
procedure c128_expand(symbol : zint_symbol; var values : TValuesArray; glyph_count : Integer);
var
  dest : TArrayOfChar;
  total_sum, i : Integer;
begin
  SetLength(dest, 1000);
  strcpy(dest, '');

  { Start character and check digit calculation }
  concat(dest, C128Table[values[0]]);
  total_sum := values[0];

  for i := 1 to glyph_count - 1 do
  begin
    concat(dest, C128Table[values[i]]);
    Inc(total_sum, values[i] * i);
  end;
  total_sum := total_sum mod 103;
  concat(dest, C128Table[total_sum]);

  { Stop character }
  concat(dest, C128Table[106]);

  expand(symbol, dest);
end;

{ Set priority array based on data classification }
procedure c128_set_priority(var priority : TPriorityArray; have_a, have_b, have_c, have_extended : Boolean);
var
  i : Integer;
begin
  i := 0;
  if have_c then
  begin
    priority[i] := C128_C0;
    Inc(i);
  end;
  if have_b or (not have_a) then
  begin
    priority[i] := C128_B0;
    Inc(i);
  end;
  if have_a then
  begin
    priority[i] := C128_A0;
    Inc(i);
  end;
  if have_extended then
  begin
    if have_c then
    begin
      priority[i] := C128_C1;
      Inc(i);
    end;
    if have_b or (not have_a) then
    begin
      priority[i] := C128_B1;
      Inc(i);
    end;
    if have_a then
    begin
      priority[i] := C128_A1;
      Inc(i);
    end;
  end;
  priority[i] := 0;
end;

{ GS1 check digit - weight factor 1,3 alternating }
function gs1_check_digit(const source : TArrayOfByte; _length : Integer) : Byte;
var
  i, count, factor : Integer;
begin
  count := 0;
  if Odd(_length) then factor := 3 else factor := 1;
  for i := 0 to _length - 1 do
  begin
    Inc(count, factor * ctoi(Chr(source[i])));
    factor := factor xor $02;
  end;
  Result := Ord(itoc((10 - (count mod 10)) mod 10));
end;

{ Handle Code 128, Code 128AB and HIBC 128 }
function code_128(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  i : Integer;
  manuals : TFncManualArray;
  fncs : TFncManualArray;
  have_a, have_b, have_c, have_extended : Boolean;
  priority : TPriorityArray;
  values : TValuesArray;
  glyph_count : Integer;
  ab_only : Boolean;
  start_idx : Integer;
  ch, mask_0x60 : Byte;
  prev_digit, digit : Boolean;
begin
  FillChar(manuals, SizeOf(manuals), 0);
  FillChar(fncs, SizeOf(fncs), 0);
  FillChar(values, SizeOf(values), 0);

  ab_only := (symbol.symbology = BARCODE_CODE128B);
  if (symbol.output_options and READER_INIT) <> 0 then
    start_idx := 2
  else
    start_idx := 0;

  if _length > C128_MAX then
  begin
    strcpy(symbol.errtxt, Format('Input length %d too long (maximum %d)', [_length, C128_MAX]));
    Result := ZERROR_TOO_LONG;
    Exit;
  end;

  { Ensure NUL terminator for c128_cost source[i+1] look-ahead }
  if _length < Length(source) then
    source[_length] := 0;

  { Classify data to detect which Code Set states are needed }
  have_a := False;
  have_b := False;
  have_c := False;
  have_extended := False;

  if ab_only then
  begin
    for i := 0 to _length - 1 do
    begin
      ch := source[i];
      mask_0x60 := ch and $60;
      have_extended := have_extended or (ch >= 128);
      have_a := have_a or (mask_0x60 = 0);
      have_b := have_b or (mask_0x60 = $60);
    end;
  end
  else
  begin
    digit := False;
    for i := 0 to _length - 1 do
    begin
      ch := source[i];
      mask_0x60 := ch and $60;
      have_extended := have_extended or (ch >= 128);
      have_a := have_a or (mask_0x60 = 0);
      have_b := have_b or (mask_0x60 = $60);
      prev_digit := digit;
      digit := (ch >= Ord('0')) and (ch <= Ord('9'));
      have_c := have_c or (prev_digit and digit);
    end;
  end;

  c128_set_priority(priority, have_a, have_b, have_c, have_extended);

  glyph_count := c128_set_values(source, _length, start_idx, priority, fncs, manuals, values, nil);

  { Check if barcode is too long }
  if glyph_count > C128_SYMBOL_MAX then
  begin
    strcpy(symbol.errtxt, Format('Input too long, requires %d symbol characters (maximum %d)', [glyph_count, C128_SYMBOL_MAX]));
    Result := ZERROR_TOO_LONG;
    Exit;
  end;

  c128_expand(symbol, values, glyph_count);

  { HRT is set by caller in zint.pas for CODE128/CODE128B }

  Result := 0;
end;

{ Handle GS1-128 with composite support }
function gs1_128_cc(symbol : zint_symbol; source : TArrayOfByte; _length : Integer; cc_mode : Integer; cc_rows : Integer) : Integer;
var
  i : Integer;
  error_number : Integer;
  manuals : TFncManualArray;
  fncs : TFncManualArray;
  priority : TPriorityArray;
  values : TValuesArray;
  glyph_count : Integer;
  final_cset : Integer;
  separator_row : Integer;
  reduced : TArrayOfChar;
  reduced_buf : TArrayOfByte;
  reduced_length : Integer;
begin
  FillChar(manuals, SizeOf(manuals), 0);
  FillChar(values, SizeOf(values), 0);
  separator_row := 0;
  error_number := 0;

  if _length > C128_MAX then
  begin
    strcpy(symbol.errtxt, Format('Input length %d too long (maximum %d)', [_length, C128_MAX]));
    Result := ZERROR_TOO_LONG;
    Exit;
  end;

  { If part of a composite symbol make room for the separator pattern }
  if symbol.symbology = BARCODE_EAN128_CC then
  begin
    separator_row := symbol.rows;
    symbol.row_height[symbol.rows] := 1;
    Inc(symbol.rows);
  end;

  SetLength(reduced, _length + 1);
  if symbol.input_mode <> GS1_MODE then
  begin
    i := gs1_verify(symbol, source, _length, reduced);
    if i <> 0 then
    begin
      Result := i;
      Exit;
    end;
  end
  else
  begin
    { GS1 data already verified - copy as-is }
    for i := 0 to _length - 1 do
      reduced[i] := Chr(source[i]);
    reduced[_length] := #0;
  end;

  reduced_length := strlen(reduced);

  { Convert reduced to byte array, replacing '[' with $1D for FNC1 }
  SetLength(reduced_buf, reduced_length + 1);
  FillChar(fncs[0], reduced_length, 1); { All positions are FNC1-capable }
  for i := 0 to reduced_length - 1 do
  begin
    if reduced[i] = '[' then
      reduced_buf[i] := $1D
    else
      reduced_buf[i] := Ord(reduced[i]);
  end;
  reduced_buf[reduced_length] := 0; { NUL terminator }

  { GS1-128 only needs B and C code sets }
  c128_set_priority(priority, False, True, True, False);

  final_cset := 0;
  glyph_count := c128_set_values(reduced_buf, reduced_length, 1 {GS1_MODE}, priority, fncs, manuals, values, @final_cset);

  { Check length including linkage flag }
  if glyph_count + Ord(cc_mode <> 0) > C128_SYMBOL_MAX then
  begin
    strcpy(symbol.errtxt, Format('Input too long, requires %d symbol characters (maximum %d)',
      [glyph_count + Ord(cc_mode <> 0), C128_SYMBOL_MAX]));
    Result := ZERROR_TOO_LONG;
    Exit;
  end;

  { Linkage flags - determined by ISO/IEC 24723 section 7.4 }
  case cc_mode of
    1, 2:
    begin
      { CC-A or CC-B 2D component }
      if final_cset = C128_B0 then
      begin
        values[glyph_count] := 99;
        Inc(glyph_count);
      end
      else if final_cset = C128_C0 then
      begin
        values[glyph_count] := 101;
        Inc(glyph_count);
      end;
    end;
    3:
    begin
      { CC-C 2D component }
      if final_cset = C128_B0 then
      begin
        values[glyph_count] := 101;
        Inc(glyph_count);
      end
      else if final_cset = C128_C0 then
      begin
        values[glyph_count] := 100;
        Inc(glyph_count);
      end;
    end;
  end;

  c128_expand(symbol, values, glyph_count);

  { Add the separator pattern for composite symbols }
  if symbol.symbology = BARCODE_EAN128_CC then
  begin
    for i := 0 to symbol.width - 1 do
    begin
      if module_is_set(symbol, separator_row + 1, i) = 0 then
        set_module(symbol, separator_row, i);
    end;
  end;

  if (symbol.output_options and COMPLIANT_HEIGHT) <> 0 then
  begin
    { C: code128.c:622. GS1 General Specifications Release 26.0 5.12.3.2
      Tabelle 2 samt Fussnote (**), wie ITF-14: bei weiterer Platznot
      Hoehe 5.8mm / 1.016mm (X max); Standard 31.75mm / 0.495mm. }
    if symbol.symbology = BARCODE_EAN128_CC then
    begin
      { Rueckgabe ueber die temporaere lineare Struktur }
      if symbol.height <> 0 then
        symbol.height := 5.70866156  { 5.8 / 1.016 }
      else
        symbol.height := 64.1414108; { 31.75 / 0.495 }
    end
    else
    begin
      { C: keine Warnung aus zint_gs1_verify ueberschreiben. Im Port kann der
        Fall derzeit nicht eintreten - jede Rueckgabe <> 0 fuehrt oben zum
        Ausstieg, und Warnung 843 (code128.c:615) ist nicht portiert. Die
        Verzweigung bleibt trotzdem, damit sie stimmt, sobald 843 dazukommt. }
      if error_number = 0 then
        error_number := set_height(symbol, 5.70866156, 64.1414108, 0.0, 0)
      else
        set_height(symbol, 5.70866156, 64.1414108, 0.0, 1);
    end;
  end
  else
  begin
    if symbol.symbology = BARCODE_EAN128_CC then
    begin
      if cc_mode = 3 then
        symbol.height := 50.0 - cc_rows * 3 - 1.0
      else
        symbol.height := 50.0 - cc_rows * 2 - 1.0;
    end
    else
      set_height(symbol, 0.0, 50.0, 0.0, 1);
  end;

  { C: code128.c:647. Nur warnen, wenn sonst nichts gewarnt hat. }
  if (error_number = 0) and ((symbol.output_options and READER_INIT) <> 0) then
  begin
    strcpy(symbol.errtxt, 'Cannot use Reader Initialisation in GS1 mode, ignoring');
    error_number := ZWARN_INVALID_OPTION;
  end;

  { Set HRT: replace [ ] with ( ) }
  for i := 0 to _length - 1 do
  begin
    if source[i] = Ord('[') then
      symbol.text[i] := Ord('(')
    else if source[i] = Ord(']') then
      symbol.text[i] := Ord(')')
    else
      symbol.text[i] := source[i];
  end;
  symbol.text[_length] := 0;

  Result := error_number;
end;

{ Handle GS1-128 (formerly known as EAN-128) }
function ean_128(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
begin
  Result := gs1_128_cc(symbol, source, _length, 0, 0);
end;

{ Unified NVE-18 / EAN-14 helper }
function nve18_or_ean14(symbol : zint_symbol; source : TArrayOfByte; _length : Integer; data_len : Integer) : Integer;
var
  i, zeroes : Integer;
  ean128_equiv : TArrayOfByte;
  have_check_digit, check_digit : Byte;
  error_number : Integer;
  prefix_paren, prefix_bracket : String;
begin
  if data_len = 17 then
  begin
    prefix_paren := '(00)';
    prefix_bracket := '[00]';
  end
  else
  begin
    prefix_paren := '(01)';
    prefix_bracket := '[01]';
  end;

  { Allow and ignore any AI prefix }
  if ((_length = data_len + 4) or (_length = data_len + 1 + 4)) and
     ((_length >= 4) and
      ((Chr(source[0]) + Chr(source[1]) + Chr(source[2]) + Chr(source[3]) = prefix_paren) or
       (Chr(source[0]) + Chr(source[1]) + Chr(source[2]) + Chr(source[3]) = prefix_bracket))) then
  begin
    source := Copy(source, 4, _length - 4);
    Dec(_length, 4);
  end
  else if ((_length = data_len + 2) or (_length = data_len + 1 + 2)) and
          (source[0] = Ord(prefix_paren[2])) and (source[1] = Ord(prefix_paren[3])) then
  begin
    source := Copy(source, 2, _length - 2);
    Dec(_length, 2);
  end;

  if _length > data_len + 1 then
  begin
    strcpy(symbol.errtxt, Format('Input length %d too long (maximum %d)', [_length, data_len + 1]));
    Result := ZERROR_TOO_LONG;
    Exit;
  end;

  { Validate numeric }
  for i := 0 to _length - 1 do
  begin
    if (source[i] < Ord('0')) or (source[i] > Ord('9')) then
    begin
      strcpy(symbol.errtxt, Format('Invalid character at position %d in input (digits only)', [i + 1]));
      Result := ZERROR_INVALID_DATA;
      Exit;
    end;
  end;

  have_check_digit := 0;
  if _length = data_len + 1 then
  begin
    have_check_digit := source[data_len];
    Dec(_length);
  end;

  zeroes := data_len - _length;
  SetLength(ean128_equiv, data_len + 6);
  FillChar(ean128_equiv[0], Length(ean128_equiv), 0);

  { Build [01] or [00] prefixed string }
  ean128_equiv[0] := Ord('[');
  ean128_equiv[1] := Ord(prefix_paren[2]);
  ean128_equiv[2] := Ord(prefix_paren[3]);
  ean128_equiv[3] := Ord(']');
  for i := 0 to zeroes - 1 do
    ean128_equiv[4 + i] := Ord('0');
  for i := 0 to _length - 1 do
    ean128_equiv[4 + zeroes + i] := source[i];

  check_digit := gs1_check_digit(Copy(ean128_equiv, 4, data_len), data_len);

  if (have_check_digit <> 0) and (have_check_digit <> check_digit) then
  begin
    strcpy(symbol.errtxt, Format('Invalid check digit ''%s'', expecting ''%s''',
      [Chr(have_check_digit), Chr(check_digit)]));
    Result := ZERROR_INVALID_CHECK;
    Exit;
  end;

  ean128_equiv[data_len + 4] := check_digit;
  ean128_equiv[data_len + 5] := 0;

  error_number := ean_128(symbol, ean128_equiv, data_len + 5);
  Result := error_number;
end;

{ Add check digit if encoding an NVE18 symbol }
function nve_18(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
begin
  Result := nve18_or_ean14(symbol, source, _length, 17);
end;

{ EAN-14 - A version of GS1-128 }
function ean_14(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
begin
  Result := nve18_or_ean14(symbol, source, _length, 13);
end;

end.

