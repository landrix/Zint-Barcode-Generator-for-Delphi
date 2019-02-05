unit zint_code16k;

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

function code16k(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses zint_common, zint_helper;

type
  TGlobalList = array[0..1] of array[0..169] of Integer;

{ EN 12323 Table 1 - "Code 16K" character encodations }
const C16KTable : array[0..106] of String = ('212222', '222122', '222221', '121223', '121322', '131222', '122213',
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
  '211133');

{ EN 12323 Table 3 and Table 4 - Start patterns and stop patterns }
const C16KStartStop : array[0..7] of String = ('3211', '2221', '2122', '1411', '1132', '1231', '1114', '3112');

{ EN 12323 Table 5 - Start and stop values defining row numbers }
const C16KStartValues : array[0..15] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7);
const C16KStopValues : array[0..15] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 4, 5, 6, 7, 0, 1, 2, 3);

{ bring together same type blocks }
procedure grwp16(var indexliste : Integer; var list : TGlobalList);
var
  i, j : Integer;
begin
  if (indexliste > 1) then
  begin
    i := 1;
    while i < indexliste do
    begin
      if (list[1][i - 1] = list[1][i]) then
      begin
        { bring together }
        list[0][i - 1] := list[0][i - 1] + list[0][i];

        { decreace the list }
        for j := i + 1  to indexliste - 1 do
        begin
          list[0][j - 1] := list[0][j];
          list[1][j - 1] := list[1][j];
        end;
        Dec(indexliste);
        Dec(i);
      end;
      Inc(i);
    end;
  end;
end;

{ Implements rules from ISO 15417 Annex E }
procedure dxsmooth16(var indexliste : Integer; var list : TGlobalList);
var
  current, last, next, _length : Integer;
  i : Integer;
begin
  for i := 0 to indexliste - 1 do
  begin
    current := list[1][i];
    _length := list[0][i];

    if (i <> 0) then
      last := list[1][i - 1]
    else
      last := _FALSE;

    if (i <> indexliste - 1) then
      next := list[1][i + 1]
    else
      next := _FALSE;

    if (i = 0) then
    begin { first block }
      if ((indexliste = 1) and ((_length = 2) and (current = ABORC))) then
        { Rule 1a }
        list[1][i] := LATCHC;
      if (current = ABORC) then
      begin
        if (_length >= 4) then
          { Rule 1b }
          list[1][i] := LATCHC
        else
        begin
          list[1][i] := AORB;
          current := AORB;
        end;
      end;
      if (current = SHIFTA) then
        { Rule 1c }
        list[1][i] := LATCHA;
      if ((current = AORB) and (next = SHIFTA)) then
        { Rule 1c }
      begin list[1][i] := LATCHA; current := LATCHA; end;
      if (current = AORB) then
        { Rule 1d }
        list[1][i] := LATCHB;
    end
    else
    begin
      if ((current = ABORC) and (_length >= 4)) then
        { Rule 3 }
      begin list[1][i] := LATCHC; current := LATCHC; end;
      if (current = ABORC) then
      begin list[1][i] := AORB; current := AORB; end;
      if ((current = AORB) and (last = LATCHA)) then
        begin list[1][i] := LATCHA; current := LATCHA; end;
      if ((current = AORB) and (last = LATCHB)) then
        begin list[1][i] := LATCHB; current := LATCHB; end;
      if ((current = AORB) and (next = SHIFTA)) then
        begin list[1][i] := LATCHA; current := LATCHA; end;
      if ((current = AORB) and (next = SHIFTB)) then
        begin list[1][i] := LATCHB; current := LATCHB; end;
      if (current = AORB) then
        begin list[1][i] := LATCHB; current := LATCHB; end;
      if ((current = SHIFTA) and (_length > 1)) then
        { Rule 4 }
        begin list[1][i] := LATCHA; current := LATCHA; end;
      if ((current = SHIFTB) and (_length > 1)) then
        { Rule 5 }
        begin list[1][i] := LATCHB; current := LATCHB; end;
      if ((current = SHIFTA) and (last = LATCHA)) then
        begin list[1][i] := LATCHA; current := LATCHA; end;
      if ((current = SHIFTB) and (last = LATCHB)) then
        begin list[1][i] := LATCHB; current := LATCHB; end;
      if ((current = SHIFTA) and (last = LATCHC)) then
        begin list[1][i] := LATCHA; current := LATCHA; end;
      if ((current = SHIFTB) and (last = LATCHC)) then
        begin list[1][i] := LATCHB; {current := LATCHB;} end;
    end; { Rule 2 is implimented elsewhere, Rule 6 is implied }
  end;
  grwp16(indexliste, list);
end;

procedure c16k_set_a(source : Byte; var values : TArrayOfInteger; var bar_chars : Integer);
begin
  source := source and 127; { limit the range to 0-127 }
  if (source < 32) then
    Inc(source, 64)
  else
    Dec(source, 32);
  values[bar_chars] := source;
  Inc(bar_chars);
end;

procedure c16k_set_b(source : Byte; var values : TArrayOfInteger; var bar_chars : Integer);
begin
  source := source and 127; { limit the range to 0-127 }
  values[bar_chars] := source - 32;
  Inc(bar_chars);
end;

procedure c16k_set_c(source_a : Byte; source_b : Byte; var values : TArrayOfInteger; var bar_chars : Integer);
var
  weight : Integer;
begin
  weight := (10 * ctoi(Char(source_a))) + ctoi(Char(source_b));
  values[bar_chars] := weight;
  Inc(bar_chars);
end;

function code16k(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  width_pattern : TArrayOfChar;
  current_row, rows_needed, flip_flop, looper, first_check, second_check : Integer;
  indexliste, indexchaine, pads_needed, f_state : Integer;
  _set, fset : TArrayOfChar;
  Mode : Integer;
  last_set, current_set : Char;
  i, j, k, m, read, mx_reader, writer : Integer;
  values : TArrayOfInteger;
  bar_characters : Integer;
  glyph_count : Single;
  errornum, first_sum, second_sum : Integer;
  input_length : Integer;
  gs1, c_count : Integer;
  list : TGlobalList;
begin
  SetLength(width_pattern, 100);
  SetLength(_set, 160);
  Fill(_set, 160, ' ');
  SetLength(fset, 160);
  Fill(fset, 160, ' ');
  SetLength(values, 160);
  FillChar(values[0], Length(values), 0);
  errornum := 0;
  strcpy(width_pattern, '');
  input_length := _length;

  if (symbol.input_mode = GS1_MODE) then begin gs1 := 1; end else begin gs1 := 0; end;

  if (input_length > 157) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  bar_characters := 0;

  { Detect extended ASCII characters }
  for i := 0 to input_length - 1  do
  begin
    if (source[i] >= 128) then
      fset[i] := 'f';
  end;

  { Decide when to latch to extended mode }
  for i := 0 to input_length - 1 do
  begin
    j := 0;
    if (fset[i] = 'f') then
    begin
      repeat
        Inc(j);
      until not (fset[i + j] = 'f');
      if ((j >= 5) or ((j >= 3) and ((i + j) = (input_length - 1)))) then
      begin
        for k := 0 to j do
          fset[i + k] := 'F';
      end;
    end;
  end;

  { Decide if it is worth reverting to 646 encodation for a few characters }
  if (input_length > 1) then
  begin
    for i := 1 to input_length - 1 do
    begin
      if ((fset[i - 1] = 'F') and (fset[i] = ' ')) then
      begin
        { Detected a change from 8859-1 to 646 - count how long for }
        j := 0;
        while (fset[i + j] = ' ') and ((i + j) <= input_length) do Inc(j);
        if ((j < 5) or ((j < 3) and ((i + j) = (input_length - 1)))) then
        begin
          { Change to shifting back rather than latching back }
          for k := 0 to j - 1 do
            fset[i + k] := 'n';
        end;
      end;
    end;
  end;
  { Detect mode A, B and C characters }
  indexliste := 0;
  indexchaine := 0;

  mode := parunmodd(source[indexchaine]);
  if ((gs1 <> 0) and (source[indexchaine] = Ord('['))) then mode := ABORC; { FNC1 }

  for i := 0 to 159 do
    list[0][i] := 0;

  repeat
    list[1][indexliste] := mode;
    while ((list[1][indexliste] = mode) and (indexchaine <= input_length)) do
    begin
      Inc(list[0][indexliste]);
      Inc(indexchaine);
      mode := parunmodd(source[indexchaine]);
      if ((gs1 <> 0) and (source[indexchaine] = Ord('['))) then mode := ABORC; { FNC1 }
    end;
    Inc(indexliste);
  until not (indexchaine <= input_length);

  dxsmooth16(indexliste, list);

  { Put set data into _set[] }
  read := 0;
  for i := 0 to indexliste - 1 do
  begin
    for j := 0 to list[0][i] - 1 do
    begin
      case list[1][i] of
        SHIFTA: _set[read] := 'a';
        LATCHA: _set[read] := 'A';
        SHIFTB: _set[read] := 'b';
        LATCHB: _set[read] := 'B';
        LATCHC: _set[read] := 'C';
      end;
      Inc(read);
    end;
  end;

  { Adjust for strings which start with shift characters - make them latch instead }
  if (_set[0] = 'a') then
  begin
    i := 0;
    repeat
      _set[i] := 'A';
      Inc(i);
    until not (_set[i] = 'a');
  end;

  if (_set[1] = 'b') then
  begin
    i := 0;
    repeat
      _set[i] := 'B';
      Inc(i);
    until not (_set[i] = 'b');
  end;

  { Watch out for odd-length Mode C blocks }
  c_count := 0;
  i := 0;
  while i < read do
  begin
    if (_set[i] = 'C') then
    begin
      if (source[i] = Ord('[')) then
      begin
        if (c_count and 1) <> 0 then
        begin
          if ((i - c_count) <> 0) then
            _set[i - c_count] := 'B'
          else
            _set[i - 1] := 'B'
        end;
        c_count := 0;
      end
      else
        Inc(c_count);
    end
    else
    begin
      if (c_count and 1) <> 0 then
      begin
        if ((i - c_count) <> 0) then
          _set[i - c_count] := 'B'
        else
          _set[i - 1] := 'B';
      end;
      c_count := 0;
    end;
    Inc(i);
  end;
  if (c_count and 1) <> 0 then
  begin
    if ((i - c_count) <> 0) then
      _set[i - c_count] := 'B'
    else
      _set[i - 1] := 'B';
  end;
  for i := 1 to read - 1 do
  begin
    if ((_set[i] = 'C') and ((_set[i - 1] = 'B') and (_set[i + 1] = 'B'))) then
      _set[i] := 'B';
  end;

  { Make sure the data will fit in the symbol }
  last_set := ' ';
  glyph_count := 0.0;
  for i := 0 to input_length - 1 do
  begin
    if ((_set[i] = 'a') or (_set[i] = 'b')) then
      glyph_count := glyph_count + 1.0;

    if ((fset[i] = 'f') or (fset[i] = 'n')) then
      glyph_count := glyph_count + 1.0;

    if (((_set[i] = 'A') or (_set[i] = 'B')) or (_set[i] = 'C')) then
    begin
      if (_set[i] <> last_set) then
      begin
        last_set := _set[i];
        glyph_count := glyph_count + 1.0;
      end;
    end;
    if (i = 0) then
    begin
      if ((_set[i] = 'B') and (_set[1] = 'C')) then
        glyph_count := glyph_count - 1.0;

      if ((_set[i] = 'B') and (_set[1] = 'B')) then
      begin
        if (_set[2] = 'C') then
          glyph_count := glyph_count - 1.0;
      end;
      if (fset[i] = 'F') then
        glyph_count := glyph_count + 2.0;
    end
    else
    begin
      if ((fset[i] = 'F') and (fset[i - 1] <> 'F')) then
        glyph_count := glyph_count + 2.0;

      if ((fset[i] <> 'F') and (fset[i - 1] = 'F')) then
        glyph_count := glyph_count + 2.0;
    end;

    if ((_set[i] = 'C') and (not ((gs1 <> 0) and (source[i] = Ord('['))))) then
      glyph_count := glyph_count + 0.5
    else
      glyph_count := glyph_count + 1.0;
  end;

  if ((gs1 <> 0) and (_set[0] <> 'A')) then
    { FNC1 can be integrated with mode character }
    glyph_count := glyph_count - 1;

  if (glyph_count > 77.0) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  { Calculate how tall the symbol will be }
  glyph_count := glyph_count + 2.0;
  i := Trunc(glyph_count);
  rows_needed := (i div 5);
  if (i mod 5 > 0) then Inc(rows_needed);

  if (rows_needed = 1) then
    rows_needed := 2;

  { start with the mode character - Table 2 }
  m := 0;
  case _set[0] of
    'A': m := 0;
    'B': m := 1;
    'C': m := 2;
  end;

  if (symbol.output_options and READER_INIT) <> 0 then
  begin
    if (m = 2) then m := 5;
    if (gs1 <> 0) then
    begin
      strcpy(symbol.errtxt, 'Cannot use both GS1 mode and Reader Initialisation');
      result := ZERROR_INVALID_OPTION; exit;
    end
    else
    begin
      if ((_set[0] = 'B') and (_set[1] = 'C')) then m := 6;
    end;
    values[bar_characters] := (7 * (rows_needed - 2)) + m; { see 4.3.4.2 }
    values[bar_characters + 1] := 96; { FNC3 }
    Inc(bar_characters, 2);
  end
  else
  begin
    if (gs1 <> 0) then
    begin
      { Integrate FNC1 }
      case _set[0] of
        'B': m := 3;
        'C': m := 4;
      end;
    end
    else
    begin
      if ((_set[0] = 'B') and (_set[1] = 'C')) then m := 5;
      if (((_set[0] = 'B') and (_set[1] = 'B')) and (_set[2] = 'C')) then m := 6;
    end;
    values[bar_characters] := (7 * (rows_needed - 2)) + m; { see 4.3.4.2 }
    Inc(bar_characters);
  end;

  current_set := _set[0];
  f_state := 0; { f_state remembers if we are in Extended ASCII mode (value 1) or in ISO/IEC 646 mode (value 0) }
  if (fset[0] = 'F') then
  begin
    case current_set of
      'A':
      begin
        values[bar_characters] := 101;
        values[bar_characters + 1] := 101;
      end;
      'B':
      begin
        values[bar_characters] := 100;
        values[bar_characters + 1] := 100;
      end;
    end;
    Inc(bar_characters, 2);
    f_state := 1;
  end;

  read := 0;

  { Encode the data }
  repeat

    if ((read <> 0) and (_set[read] <> _set[read - 1])) then
    begin { Latch different code set }
      case _set[read] of
        'A':
        begin
          values[bar_characters] := 101;
          Inc(bar_characters);
          current_set := 'A';
        end;
        'B':
        begin
          values[bar_characters] := 100;
          Inc(bar_characters);
          current_set := 'B';
        end;
        'C':
        begin
          if (not ((read = 1) and (_set[0] = 'B'))) then
          begin { Not Mode C/Shift B }
            if (not ((read = 2) and ((_set[0] = 'B') and (_set[1] = 'B')))) then
            begin
              { Not Mode C/Double Shift B }
              values[bar_characters] := 99;
              Inc(bar_characters);
            end;
          end;
          current_set := 'C';
        end;
      end;
    end;

    if (read <> 0) then
    begin
      if ((fset[read] = 'F') and (f_state = 0)) then
      begin
        { Latch beginning of extended mode }
        case current_set of
          'A':
          begin
            values[bar_characters] := 101;
            values[bar_characters + 1] := 101;
          end;
          'B':
          begin
            values[bar_characters] := 100;
            values[bar_characters + 1] := 100;
          end;
        end;
        Inc(bar_characters, 2);
        f_state := 1;
      end;
      if ((fset[read] = ' ') and (f_state = 1)) then
      begin
        { Latch end of extended mode }
        case current_set of
          'A':
          begin
            values[bar_characters] := 101;
            values[bar_characters + 1] := 101;
          end;
          'B':
          begin
            values[bar_characters] := 100;
            values[bar_characters + 1] := 100;
          end;
        end;
        Inc(bar_characters, 2);
        f_state := 0;
      end;
    end;

    if ((fset[i] = 'f') or (fset[i] = 'n')) then
    begin
      { Shift extended mode }
      case current_set of
        'A':
          values[bar_characters] := 101; { FNC 4 }
        'B':
          values[bar_characters] := 100; { FNC 4 }
      end;
      Inc(bar_characters);
    end;

    if ((_set[i] = 'a') or (_set[i] = 'b')) then
    begin
      { Insert shift character }
      values[bar_characters] := 98;
      Inc(bar_characters);
    end;

    if (not ((gs1 <> 0) and (source[read] = Ord('[')))) then
    begin
      case _set[read] of { Encode data characters }
        'A',
        'a':
        begin
          c16k_set_a(source[read], values, bar_characters);
          Inc(read);
        end;
        'B',
        'b':
        begin
          c16k_set_b(source[read], values, bar_characters);
          Inc(read);
        end;
        'C':
        begin
          c16k_set_c(source[read], source[read + 1], values, bar_characters);
          Inc(read, 2);
        end;
      end;
    end
    else
    begin
      values[bar_characters] := 102;
      Inc(bar_characters);
      Inc(read);
    end;

  until not (read < ustrlen(source));

  pads_needed := 5 - ((bar_characters + 2) mod 5);
  if (pads_needed = 5) then
    pads_needed := 0;

  if ((bar_characters + pads_needed) < 8) then
    Inc(pads_needed, 8 - (bar_characters + pads_needed));

  for i := 0 to pads_needed - 1 do
  begin
    values[bar_characters] := 106;
    Inc(bar_characters);
  end;

  { Calculate check digits }
  first_sum := 0;
  second_sum := 0;
  for i := 0 to bar_characters - 1 do
  begin
    Inc(first_sum, (i+2) * values[i]);
    Inc(second_sum, (i+1) * values[i]);
  end;
  first_check := first_sum mod 107;
  Inc(second_sum, first_check * (bar_characters + 1));
  second_check := second_sum mod 107;
  values[bar_characters] := first_check;
  values[bar_characters + 1] :=  second_check;
  Inc(bar_characters, 2);

  for current_row := 0 to rows_needed - 1 do
  begin
    strcpy(width_pattern, '');
    concat(width_pattern, C16KStartStop[C16KStartValues[current_row]]);
    concat(width_pattern, '1');
    for i := 0 to 4 do
      concat(width_pattern, C16KTable[values[(current_row * 5) + i]]);
    concat(width_pattern, C16KStartStop[C16KStopValues[current_row]]);

    { Write the information into the symbol }
    writer := 0;
    flip_flop := 1;
    for mx_reader := 0 to strlen(width_pattern) - 1 do
    begin
      for looper := 0 to ctoi(width_pattern[mx_reader]) - 1 do
      begin
        if (flip_flop = 1) then
        begin
          set_module(symbol, current_row, writer);
          Inc(writer);
        end
        else
          Inc(writer);
      end;
      if (flip_flop = 0) then flip_flop := 1 else flip_flop := 0;
    end;
    symbol.row_height[current_row] := 10;
  end;

  symbol.rows := rows_needed;
  symbol.width := 70;
  result := errornum; exit;
end;

end.

