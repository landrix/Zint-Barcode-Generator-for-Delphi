unit zint_code1;

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

function code_one(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses zint_common, zint_helper, zint_reedsol, zint_large;

const c40_shift : array[0..127] of Integer = (
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 );

const c40_value : array[0..127] of Integer = (
	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
	3,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,4,5,6,7,8,9,10,11,12,13,
  15,16,17,18,19,20,21,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
  22,23,24,25,26,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31 );

const text_shift : array[0..127] of Integer = (
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  2, 2, 2, 2, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3 );

const text_value : array[0..127] of Integer = (
	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
	3,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,4,5,6,7,8,9,10,11,12,13,
	15,16,17,18,19,20,21,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
	22,23,24,25,26,0,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,27,28,29,30,31 );


const c1_height : array[0..7] of Integer = ( 16, 22, 28, 40, 52, 70, 104, 148 );
const c1_width : array[0..7] of Integer = ( 18, 22, 32, 42, 54, 76, 98, 134 );
const c1_data_length : array[0..7] of Integer = ( 10, 19, 44, 91, 182, 370, 732, 1480 );
const c1_ecc_length : array[0..7] of Integer = ( 10, 16, 26, 44, 70, 140, 280, 560 );
const c1_blocks : array[0..7] of Integer = ( 1, 1, 1, 1, 1, 2, 4, 8 );
const c1_data_blocks : array[0..7] of Integer = ( 10, 19, 44, 91, 182, 185, 183, 185 );
const c1_ecc_blocks : array[0..7] of Integer = ( 10, 16, 26, 44, 70, 70, 70, 70 );
const c1_grid_width : array[0..7] of Integer = ( 4, 5, 7, 9, 12, 17, 22, 30 );
const c1_grid_height : array[0..7] of Integer = ( 5, 7, 10, 15, 21, 30, 46, 68 );

const C1_ASCII = 1;
const C1_C40 = 2;
const C1_DECIMAL = 3;
const C1_TEXT = 4;
const C1_EDI = 5;
const C1_BYTE = 6;

procedure horiz(symbol : zint_symbol; row_no : Integer; full : Integer);
var
  i : Integer;
  lim : Integer;
begin
  if not (full <> 0) then i := 1 else i := 0;
  lim := symbol.width - i;

  while (i < lim) do
  begin
    set_module(symbol, row_no, i);
    Inc(i);
  end;
end;

procedure central_finder(symbol : zint_symbol; start_row : Integer; row_count : Integer; full_rows : Integer);
var
  i : Integer;
begin
  for i := 0 to row_count - 1 do
  begin
    if (i < full_rows) then
    begin
      horiz(symbol, start_row + (i * 2), 1);
    end
    else
    begin
      horiz(symbol, start_row + (i * 2), 0);
      if (i <> row_count - 1) then
      begin
        set_module(symbol, start_row + (i * 2) + 1, 1);
        set_module(symbol, start_row + (i * 2) + 1, symbol.width - 2);
      end;
    end;
  end;
end;

procedure vert(symbol : zint_symbol; column : Integer; height : Integer; top : Integer);
var
  i : Integer;
begin
  if (top <> 0) then
  begin
    for i := 0 to height - 1 do
      set_module(symbol, i, column);
  end
  else
  begin
    for i := 0 to height - 1 do
      set_module(symbol, symbol.rows - i - 1, column);
  end;
end;

procedure spigot(symbol : zint_symbol; row_no : Integer);
var
  i : Integer;
begin
  for i := symbol.width - 1 downto 1 do
  begin
    if (module_is_set(symbol, row_no, i - 1) <> 0) then
      set_module(symbol, row_no, i);
  end;
end;

function isedi(input : Byte) : Integer;
begin
  if (input = 13) or (input = Ord('*')) or (input = Ord('>')) or (input = Ord(' ')) or
      ((input >= Ord('0')) and (input <= Ord('9'))) or ((input >= Ord('A')) and (input <= Ord('Z'))) then
  begin
    result := 1; exit;
  end;

  result := 0; exit;
end;

function dq4bi(const source : TArrayOfByte; sourcelen : Integer; position : Integer) : Integer;
var
  i : Integer;
begin
  i := position;
  while (isedi(source[position + i]) <> 0) and ((position + i) < sourcelen) do Inc(i);

  if ((position + i) = sourcelen) then
  begin
    { Reached end of input }
    result := 0; exit;
  end;

  if (source[position + i - 1] = 13) then begin result := 1; exit; end;
  if (source[position + i - 1] = Ord('*')) then begin result := 1; exit; end;
  if (source[position + i - 1] = Ord('>')) then begin result := 1; exit; end;

  result := 0; exit;
end;

function c1_look_ahead_test(const source : TArrayOfByte; sourcelen : Integer; position : Integer; current_mode : Integer; gs1 : Integer) : Integer;
var
  ascii_count, c40_count, text_count, edi_count, byte_count : Integer;
  ascii_rnded, c40_rnded, text_rnded, edi_rnded, byte_rnded : Integer;
  cnt_1, sp, c, best_scheme : Integer;
  is_extended : Integer;
  c_is_c40, c_is_text, c_is_edi : Integer;

  function c1_mult_ceil(const v : Integer) : Integer;
  begin
    result := ((v + 5) div 6) * 6;
  end;
begin
  { Step J }
  if (current_mode = C1_ASCII) then
  begin
    ascii_count := 0;
    c40_count := 6;
    text_count := 6;
    edi_count := 6;
    byte_count := 12;
  end
  else
  begin
    ascii_count := 6;
    c40_count := 12;
    text_count := 12;
    edi_count := 12;
    byte_count := 18;
  end;

  case current_mode of
    C1_C40: c40_count := 0;
    C1_TEXT: text_count := 0;
    C1_EDI: edi_count := 0;
    C1_BYTE: byte_count := 0;
  end;

  for sp := position to sourcelen - 1 do
  begin
    c := source[sp];
    if (c > 127) then is_extended := 1 else is_extended := 0;

    { Step L }
    if ((c >= Ord('0')) and (c <= Ord('9'))) then
      Inc(ascii_count, 3)
    else if (is_extended <> 0) then
      ascii_count := c1_mult_ceil(ascii_count) + 12
    else
      ascii_count := c1_mult_ceil(ascii_count) + 6;

    c_is_c40 := 0;
    if (c = Ord(' ')) or ((c >= Ord('0')) and (c <= Ord('9'))) or ((c >= Ord('A')) and (c <= Ord('Z'))) then
      c_is_c40 := 1;
    if (c_is_c40 <> 0) then
      Inc(c40_count, 4)
    else if (is_extended <> 0) then
      Inc(c40_count, 16)
    else
      Inc(c40_count, 8);

    c_is_text := 0;
    if (c = Ord(' ')) or ((c >= Ord('0')) and (c <= Ord('9'))) or ((c >= Ord('a')) and (c <= Ord('z'))) then
      c_is_text := 1;
    if (c_is_text <> 0) then
      Inc(text_count, 4)
    else if (is_extended <> 0) then
      Inc(text_count, 16)
    else
      Inc(text_count, 8);

    c_is_edi := 0;
    if (c = 13) or (c = Ord('*')) or (c = Ord('>')) or (c = Ord(' ')) or
      ((c >= Ord('0')) and (c <= Ord('9'))) or ((c >= Ord('A')) and (c <= Ord('Z'))) then
      c_is_edi := 1;
    if (c_is_edi <> 0) then
      Inc(edi_count, 4)
    else if (is_extended <> 0) then
      Inc(edi_count, 26)
    else
      Inc(edi_count, 20);

    if (gs1 <> 0) and (c = Ord('[')) then
      Inc(byte_count, 18)
    else
      Inc(byte_count, 6);

    if (sp >= position + 3) then
    begin
      ascii_rnded := c1_mult_ceil(ascii_count);
      c40_rnded := c1_mult_ceil(c40_count);
      text_rnded := c1_mult_ceil(text_count);
      edi_rnded := c1_mult_ceil(edi_count);
      byte_rnded := c1_mult_ceil(byte_count);

      cnt_1 := byte_count + 6;
      if (cnt_1 <= ascii_rnded) and (cnt_1 <= c40_rnded) and (cnt_1 <= text_rnded) and (cnt_1 <= edi_rnded) then
      begin
        result := C1_BYTE; exit;
      end;

      cnt_1 := ascii_count + 6;
      if (cnt_1 <= c40_rnded) and (cnt_1 <= text_rnded) and (cnt_1 <= edi_rnded) and (cnt_1 <= byte_rnded) then
      begin
        result := C1_ASCII; exit;
      end;

      cnt_1 := text_rnded + 6;
      if (cnt_1 <= ascii_rnded) and (cnt_1 <= c40_rnded) and (cnt_1 <= edi_rnded) and (cnt_1 <= byte_rnded) then
      begin
        result := C1_TEXT; exit;
      end;

      cnt_1 := c40_rnded + 6;
      if (cnt_1 <= ascii_rnded) and (cnt_1 <= text_rnded) then
      begin
        if (c40_rnded < edi_rnded) then
        begin
          result := C1_C40; exit;
        end;
        if (c40_rnded = edi_rnded) then
        begin
          if (dq4bi(source, sourcelen, sp + 1) <> 0) then
            result := C1_EDI
          else
            result := C1_C40;
          exit;
        end;
      end;

      cnt_1 := edi_rnded + 6;
      if (cnt_1 <= ascii_rnded) and (cnt_1 <= c40_rnded) and (cnt_1 <= text_rnded) and (cnt_1 <= byte_rnded) then
      begin
        result := C1_EDI; exit;
      end;
    end;
  end;

  { Step K }
  ascii_rnded := c1_mult_ceil(ascii_count);
  c40_rnded := c1_mult_ceil(c40_count);
  text_rnded := c1_mult_ceil(text_count);
  edi_rnded := c1_mult_ceil(edi_count);
  byte_rnded := c1_mult_ceil(byte_count);

  if (byte_count <= ascii_rnded) and (byte_count <= c40_rnded) and (byte_count <= text_rnded) and (byte_count <= edi_rnded) then
    best_scheme := C1_BYTE
  else if (ascii_count <= c40_rnded) and (ascii_count <= text_rnded) and (ascii_count <= edi_rnded) and (ascii_count <= byte_rnded) then
    best_scheme := C1_ASCII
  else if (c40_rnded <= text_rnded) and (c40_rnded <= edi_rnded) then
    best_scheme := C1_C40
  else if (text_rnded <= edi_rnded) then
    best_scheme := C1_TEXT
  else
    best_scheme := C1_EDI;

  {$IFDEF DEBUG_ZINT}
  WriteLn;
  WriteLn(Format('> scores: ASCII %.2f  C40 %.2f  TEXT %.2f  EDI %.2f  BYTE %.2f',
    [ascii_count / 6.0, c40_count / 6.0, text_count / 6.0, edi_count / 6.0, byte_count / 6.0]));
  {$ENDIF}

  result := best_scheme; exit;
end;

procedure c1_eci_escape(const eci : Integer; const source : TArrayOfByte; _length : Integer;
  var eci_buf : TArrayOfByte; var eci_length : Integer);
var
  i, j, slash_count : Integer;
  eci_prefix : AnsiString;
begin
  slash_count := 0;
  for i := 0 to _length - 1 do
    if source[i] = Ord('\') then
      Inc(slash_count);

  eci_length := _length + 7 + slash_count;
  SetLength(eci_buf, eci_length);

  eci_prefix := AnsiString(Format('\%.6d', [eci]));
  for i := 1 to Length(eci_prefix) do
    eci_buf[i - 1] := Ord(eci_prefix[i]);

  j := 7;
  for i := 0 to _length - 1 do
  begin
    if source[i] = Ord('\') then
    begin
      eci_buf[j] := Ord('\');
      Inc(j);
    end;
    eci_buf[j] := source[i];
    Inc(j);
  end;
end;

function c1_is_last_single_ascii(const source : TArrayOfByte; _length : Integer; sp : Integer) : Integer;
begin
  if (_length - sp = 1) and (source[sp] <= 127) then
  begin
    result := 1; exit;
  end;
  if (_length - sp = 2) and istwodigits(source, sp) then
  begin
    result := 1; exit;
  end;
  result := 0; exit;
end;

function c1_encode(symbol : zint_symbol; const source : TArrayOfByte; var target : TArrayOfCardinal; _length : Integer; var last_mode : Integer; start_tp : Integer = 0) : Integer;
var
  current_mode, next_mode : Integer;
  sp, tp, gs1, latch : Integer;
  c40_buffer : TArrayOfInteger; c40_p : Integer;
  text_buffer : TArrayOfInteger; text_p : Integer;
  edi_buffer : TArrayOfInteger; edi_p : Integer;
  decimal_binary : TArrayOfChar;
  byte_start : Integer;
  i, j : Integer;
  shift_set, value, done : Integer;
  iv : Integer;
  decimal_count, data_left : Integer;
  bits_left_in_byte, target_count : Integer;
  sub_target : Integer;
  sub_value : Integer;
  target1, target2, target3 : Integer;
  temp_binary : TArrayOfChar;
begin
  SetLength(c40_buffer, 6);
  SetLength(text_buffer, 6);
  SetLength(edi_buffer, 6);
  SetLength(decimal_binary, 40);
  byte_start := 0;
  sp := 0;
  tp := start_tp;
  //latch := 0;
  Fill(c40_buffer, 6, 0);
  c40_p := 0;
  Fill(text_buffer, 6, 0);
  text_p := 0;
  Fill(edi_buffer, 6, 0);
  edi_p := 0;
  strcpy(decimal_binary, '');
  last_mode := C1_ASCII;

  if ((symbol.input_mode and $07) = GS1_MODE) then gs1 := 1 else gs1 := 0;
  if (gs1 <> 0) then begin target[tp] := 232; Inc(tp); end; { FNC1 }

  { Step A }
  current_mode := C1_ASCII;
  next_mode := C1_ASCII;

  repeat
    if (current_mode <> next_mode) then
    begin
      { Change mode }
      case next_mode of
        C1_C40: begin target[tp] := 230; Inc(tp); end;
        C1_TEXT: begin target[tp] := 239; Inc(tp); end;
        C1_EDI: begin target[tp] := 238; Inc(tp); end;
        C1_BYTE: begin target[tp] := 231; Inc(tp); end;
      end;
    end;

    if (current_mode <> C1_BYTE) and (next_mode = C1_BYTE) then
      byte_start := tp;

    current_mode := next_mode;

    if (current_mode = C1_ASCII) then
    begin { Step B - ASCII encodation }
      next_mode := C1_ASCII;

      if (_length - sp >= 21) then
      begin { Step B1 }
        j := 0;

        for i := 0 to 20 do
        begin
          if ((source[sp + i] >= Ord('0')) and (source[sp + i] <= Ord('9'))) then Inc(j);
        end;

        if (j = 21) then
        begin
          next_mode := C1_DECIMAL;
          strcpy(decimal_binary, '1111');
        end;
      end;

      if (next_mode = C1_ASCII) and (_length - sp >= 13) then
      begin { Step B2 }
        j := 0;

        for i := 0 to 12 do
        begin
          if (source[sp + i] >= Ord('0')) and (source[sp + i] <= Ord('9')) then
            Inc(j);
        end;

        if (j = 13) then
        begin
          latch := 0;
          for i := sp + 13 to _length - 1 do
          begin
            if (not ((source[sp + i] >= Ord('0')) and (source[sp + i] <= Ord('9')))) then latch := 1;
          end;

          if not (latch <> 0) then
          begin
            next_mode := C1_DECIMAL;
            strcpy(decimal_binary, '1111');
          end;
        end;
      end;

      if (next_mode = C1_ASCII) then
      begin { Step B3 }
        if istwodigits(source, sp) and (((sp + 1) <> _length)) then
        begin
          target[tp] := (10 * ctoi(Chr(source[sp]))) + ctoi(Chr(source[sp + 1])) + 130;
          Inc(tp);
          Inc(sp, 2);
        end
        else
        begin
          if (gs1 and source[sp] = Ord('[')) then
          begin
            if (_length - sp >= 15) then
            begin { Step B4 }
              j := 0;

              for i := 0 to 14 do
              begin
                if (source[sp + i] >= Ord('0')) and (source[sp + i] <= Ord('9')) then
                  Inc(j);
              end;

              if (j = 15) then
              begin
                target[tp] := 236; { FNC1 and change to Decimal }
                Inc(tp); Inc(sp);
                next_mode := C1_DECIMAL;
              end;
            end;

            if (_length - sp >= 7) then
            begin { Step B5 }
              j := 0;

              for i := 0 to 6 do
              begin
                if (source[sp + i] >= Ord('0')) and (source[sp + i] <= Ord('9')) then
                  Inc(j);
              end;

              if (j = 7) then
              begin
                latch := 0;
                for i := sp + 7 to _length - 1 do
                begin
                  if (not ((source[sp + i] >= Ord('0')) and (source[sp + i] <= Ord('9')))) then
                    latch := 1;
                end;

                if not (latch <> 0) then
                begin
                  target[tp] := 236; { FNC1 and change to Decimal }
                  Inc(tp); Inc(sp);
                  next_mode := C1_DECIMAL;
                end;
              end;
            end;
          end;

          if (next_mode = C1_ASCII) then
          begin
            { Step B6 }
            next_mode := c1_look_ahead_test(source, _length, sp, current_mode, gs1);

            if (next_mode = C1_ASCII) then
            begin
              if (source[sp] > 127) then
              begin
                { Step B7 }
                target[tp] := 235; Inc(tp); { FNC4 }
                target[tp] := (source[sp] - 128) + 1; Inc(tp); Inc(sp);
              end
              else
              begin
                { Step B8 }
                if ((gs1 <> 0) and (source[sp] = Ord('['))) then
                begin
                  target[tp] := 232; Inc(tp); Inc(sp); { FNC1 }
                end
                else
                begin
                  target[tp] := source[sp] + 1; Inc(tp); Inc(sp);
                end;
              end;
            end;
          end;
        end;
      end;
    end;

    if (current_mode = C1_C40) then
    begin { Step C - C40 encodation }
      done := 0; //latch := 0;

      next_mode := C1_C40;
      if (c40_p = 0) then
      begin
        if (c1_is_last_single_ascii(source, _length, sp) <> 0) then
        begin
          next_mode := C1_ASCII;
          done := 1;
        end;

        if (_length - sp >= 12) then
        begin
          j := 0;

          for i := 0 to 11 do
          begin
            if (source[sp + i] >= Ord('0')) and (source[sp + i] <= Ord('9')) then
              Inc(j);
          end;

          if (j = 12) then
          begin
            next_mode := C1_ASCII; done := 1;
          end;
        end;

        if (_length - sp >= 8) then
        begin
          j := 0;

          for i := 0 to 7 do
          begin
            if (source[sp + i] >= Ord('0')) and (source[sp + i] <= Ord('9')) then
              Inc(j);
          end;

          if (_length - sp = 8) then
          begin
            latch := 1;
          end
          else
          begin
            latch := 1;
            j := sp + 8;
            while j < _length do
            begin
              if (source[j] <= Ord('0')) or (source[j] >= Ord('9')) then
                latch := 0;

              Inc(j);
            end;
          end;

          if (j = 8) and (latch <> 0) then
          begin
            next_mode := C1_ASCII; done := 1;
          end;
        end;

        if (not (done <> 0)) then
        begin
          next_mode := c1_look_ahead_test(source, _length, sp, current_mode, gs1);
        end;
      end;

      if (next_mode <> C1_C40) then
      begin
        target[tp] := 255; Inc(tp); { Unlatch }
      end
      else
      begin
        if (source[sp] > 127) then
        begin
          c40_buffer[c40_p] := 1; Inc(c40_p);
          c40_buffer[c40_p] := 30; Inc(c40_p); { Upper Shift }
          shift_set := c40_shift[source[sp] - 128];
          value := c40_value[source[sp] - 128];
        end
        else
        begin
          shift_set := c40_shift[source[sp]];
          value := c40_value[source[sp]];
        end;

        if (gs1 <> 0) and (source[sp] = Ord('[')) then
        begin
          shift_set := 2;
          value := 27; { FNC1 }
        end;

        if (shift_set <> 0) then
        begin
          c40_buffer[c40_p] := shift_set - 1; Inc(c40_p);
        end;
        c40_buffer[c40_p] := value; Inc(c40_p);

        if (c40_p >= 3) then
        begin
          iv := (1600 * c40_buffer[0]) + (40 * c40_buffer[1]) + (c40_buffer[2]) + 1;
          target[tp] := iv div 256; Inc(tp);
          target[tp] := iv mod 256; Inc(tp);

          c40_buffer[0] := c40_buffer[3];
          c40_buffer[1] := c40_buffer[4];
          c40_buffer[2] := c40_buffer[5];
          c40_buffer[3] := 0;
          c40_buffer[4] := 0;
          c40_buffer[5] := 0;
          Dec(c40_p, 3);
        end;
        Inc(sp);
      end;
    end;

    if (current_mode = C1_TEXT) then
    begin { Step D - Text encodation }
      done := 0; //latch := 0;

      next_mode := C1_TEXT;
      if (text_p = 0) then
      begin
        if (c1_is_last_single_ascii(source, _length, sp) <> 0) then
        begin
          next_mode := C1_ASCII;
          done := 1;
        end;

        if (_length - sp >= 12) then
        begin
          j := 0;

          for i := 0 to 11 do
          begin
            if (source[sp + i] >= Ord('0')) and (source[sp + i] <= Ord('9')) then
              Inc(j);
          end;

          if (j = 12) then
          begin
            next_mode := C1_ASCII; done := 1;
          end;
        end;

        if (_length - sp >= 8) then
        begin
          j := 0;

          for i := 0 to 7 do
          begin
            if (source[sp + i] >= Ord('0')) and (source[sp + i] <= Ord('9')) then
              Inc(j);
          end;

          if (_length - sp = 8) then
          begin
            latch := 1;
          end
          else
          begin
            latch := 1;
            j := sp + 8;
            while j < _length do
            begin
              if ((source[j] <= Ord('0')) or (source[j] >= Ord('9'))) then latch := 0;
              Inc(j);
            end;
          end;

          if (j = 8) and (latch <> 0) then
          begin
            next_mode := C1_ASCII; done := 1;
          end;
        end;

        if (not (done <> 0)) then
        begin
          next_mode := c1_look_ahead_test(source, _length, sp, current_mode, gs1);
        end;
      end;

      if (next_mode <> C1_TEXT) then
      begin
        target[tp] := 255; Inc(tp); { Unlatch }
      end
      else
      begin
        if (source[sp] > 127) then
        begin
          text_buffer[text_p] := 1; Inc(text_p);
          text_buffer[text_p] := 30; Inc(text_p); { Upper Shift }
          shift_set := text_shift[source[sp] - 128];
          value := text_value[source[sp] - 128];
        end
        else
        begin
          shift_set := text_shift[source[sp]];
          value := text_value[source[sp]];
        end;

        if (gs1 <> 0) and (source[sp] = Ord('[')) then
        begin
          shift_set := 2;
          value := 27; { FNC1 }
        end;

        if (shift_set <> 0) then
        begin
          text_buffer[text_p] := shift_set - 1; Inc(text_p);
        end;
        text_buffer[text_p] := value; Inc(text_p);

        if (text_p >= 3) then
        begin
          iv := (1600 * text_buffer[0]) + (40 * text_buffer[1]) + (text_buffer[2]) + 1;
          target[tp] := iv div 256; Inc(tp);
          target[tp] := iv mod 256; Inc(tp);

          text_buffer[0] := text_buffer[3];
          text_buffer[1] := text_buffer[4];
          text_buffer[2] := text_buffer[5];
          text_buffer[3] := 0;
          text_buffer[4] := 0;
          text_buffer[5] := 0;
          Dec(text_p, 3);
        end;
        Inc(sp);
      end;
    end;

    if (current_mode = C1_EDI) then
    begin { Step E - EDI Encodation }
      value := 0; //latch := 0;

      next_mode := C1_EDI;
      if (edi_p = 0) then
      begin
        if (_length - sp >= 12) then
        begin
          j := 0;

          for i := 0 to 11 do
          begin
            if (source[sp + i] >= Ord('0')) and (source[sp + i] <= Ord('9')) then
              Inc(j);
          end;

          if (j = 12) then
            next_mode := C1_ASCII;
        end;

        if (_length - sp >= 8) then
        begin
          j := 0;

          for i := 0 to 7 do
          begin
            if (source[sp + i] >= Ord('0')) and (source[sp + i] <= Ord('9')) then
              Inc(j);
          end;

          if (_length - sp = 8) then
          begin
            latch := 1;
          end
          else
          begin
            latch := 1;
            j := sp + 8;
            while j < _length do
            begin
              if (source[j] <= Ord('0')) or (source[j] >= Ord('9')) then
                latch := 0;
              Inc(j);
            end;
          end;

          if (j = 8) and (latch <> 0) then
            next_mode := C1_ASCII;
        end;

        if (not ((isedi(source[sp]) <> 0) and (isedi(source[sp + 1]) <> 0) and (isedi(source[sp + 2]) <> 0))) then
          next_mode := C1_ASCII;
      end;

      if (next_mode <> C1_EDI) then
      begin
        target[tp] := 255; Inc(tp); { Unlatch }
      end
      else
      begin
        if (source[sp] = 13) then value := 0;
        if (source[sp] = Ord('*')) then value := 1;
        if (source[sp] = Ord('>')) then value := 2;
        if (source[sp] = Ord(' ')) then value := 3;
        if ((source[sp] >= Ord('0')) and (source[sp] <= Ord('9'))) then value := source[sp] - Ord('0') + 4;
        if ((source[sp] >= Ord('A')) and (source[sp] <= Ord('Z'))) then value := source[sp] - Ord('A') + 14;

        edi_buffer[edi_p] := value; Inc(edi_p);

        if (edi_p >= 3) then
        begin
          iv := (1600 * edi_buffer[0]) + (40 * edi_buffer[1]) + (edi_buffer[2]) + 1;
          target[tp] := iv div 256; Inc(tp);
          target[tp] := iv mod 256; Inc(tp);

          edi_buffer[0] := edi_buffer[3];
          edi_buffer[1] := edi_buffer[4];
          edi_buffer[2] := edi_buffer[5];
          edi_buffer[3] := 0;
          edi_buffer[4] := 0;
          edi_buffer[5] := 0;
          Dec(edi_p, 3);
        end;
        Inc(sp);
      end;
    end;

    if (current_mode = C1_DECIMAL) then
    begin { Step F - Decimal encodation }
      next_mode := C1_DECIMAL;

      data_left := _length - sp;
      decimal_count := 0;

      if (data_left >= 1) then
      begin
        if (source[sp] >= Ord('0')) and (source[sp] <= Ord('9')) then
          decimal_count := 1;
      end;
      if (data_left >= 2) then
      begin
        if (decimal_count = 1) and (source[sp + 1] >= Ord('0')) and (source[sp + 1] <= Ord('9')) then
          decimal_count := 2;
      end;
      if (data_left >= 3) then
      begin
        if (decimal_count = 2) and (source[sp + 2] >= Ord('0')) and (source[sp + 2] <= Ord('9')) then
          decimal_count := 3;
      end;

      if (decimal_count <> 3) then
      begin
        { Finish Decimal mode and go back to ASCII }

        concat(decimal_binary, '111111'); { Unlatch }

        target_count := 3;
        if (strlen(decimal_binary) <= 16) then
          target_count := 2;
        if (strlen(decimal_binary) <= 8) then
          target_count := 1;
        bits_left_in_byte := (8 * target_count) - strlen(decimal_binary);
        if (bits_left_in_byte = 8) then
          bits_left_in_byte := 0;

        if (bits_left_in_byte = 2) then
          concat(decimal_binary, '01');

        if ((bits_left_in_byte = 4) or (bits_left_in_byte = 6)) then
        begin
          if (decimal_count >= 1) then
          begin
            sub_value := ctoi(Chr(source[sp])) + 1;
            bscan(decimal_binary, sub_value, $08);
            Inc(sp);
          end
          else
          begin
            concat(decimal_binary, '1111');
          end;
        end;

        if (bits_left_in_byte = 6) then
          concat(decimal_binary, '01');

        { Binary buffer is full - transfer to target }
        if (target_count >= 1) then
        begin
          sub_target := 0;
          if (decimal_binary[0] = '1') then Inc(sub_target, 128);
          if (decimal_binary[1] = '1') then Inc(sub_target, 64);
          if (decimal_binary[2] = '1') then Inc(sub_target, 32);
          if (decimal_binary[3] = '1') then Inc(sub_target, 16);
          if (decimal_binary[4] = '1') then Inc(sub_target, 8);
          if (decimal_binary[5] = '1') then Inc(sub_target, 4);
          if (decimal_binary[6] = '1') then Inc(sub_target, 2);
          if (decimal_binary[7] = '1') then Inc(sub_target, 1);
          target[tp] := sub_target; Inc(tp);
        end;
        if (target_count >= 2) then
        begin
          sub_target := 0;
          if (decimal_binary[8] = '1') then Inc(sub_target, 128);
          if (decimal_binary[9] = '1') then Inc(sub_target, 64);
          if (decimal_binary[10] = '1') then Inc(sub_target, 32);
          if (decimal_binary[11] = '1') then Inc(sub_target, 16);
          if (decimal_binary[12] = '1') then Inc(sub_target, 8);
          if (decimal_binary[13] = '1') then Inc(sub_target, 4);
          if (decimal_binary[14] = '1') then Inc(sub_target, 2);
          if (decimal_binary[15] = '1') then Inc(sub_target, 1);
          target[tp] := sub_target; Inc(tp);
        end;
        if (target_count = 3) then
        begin
          sub_target := 0;
          if (decimal_binary[16] = '1') then Inc(sub_target, 128);
          if (decimal_binary[17] = '1') then Inc(sub_target, 64);
          if (decimal_binary[18] = '1') then Inc(sub_target, 32);
          if (decimal_binary[19] = '1') then Inc(sub_target, 16);
          if (decimal_binary[20] = '1') then Inc(sub_target, 8);
          if (decimal_binary[21] = '1') then Inc(sub_target, 4);
          if (decimal_binary[22] = '1') then Inc(sub_target, 2);
          if (decimal_binary[23] = '1') then Inc(sub_target, 1);
          target[tp] := sub_target; Inc(tp);
        end;

        { C-parity: c1_decimal_unlatch returns db_p=0; clear buffer so end-of-function check won't re-process }
        strcpy(decimal_binary, '');
        next_mode := C1_ASCII;
      end
      else
      begin
        { There are three digits - convert the value to binary }
        value := (100 * ctoi(Chr(source[sp]))) + (10 * ctoi(Chr(source[sp + 1]))) + ctoi(Chr(source[sp + 2])) + 1;

        bscan(decimal_binary, value, $200);
        Inc(sp, 3);

        { Transfer ALL available full bytes - C-parity: c1_decimal_binary_transfer flushes per-byte.
          Only after appending new digit data, NOT after unlatch (which already transferred its bits). }
        SetLength(temp_binary, 40);
        while strlen(decimal_binary) >= 8 do
        begin
          sub_target := 0;
          if (decimal_binary[0] = '1') then Inc(sub_target, 128);
          if (decimal_binary[1] = '1') then Inc(sub_target, 64);
          if (decimal_binary[2] = '1') then Inc(sub_target, 32);
          if (decimal_binary[3] = '1') then Inc(sub_target, 16);
          if (decimal_binary[4] = '1') then Inc(sub_target, 8);
          if (decimal_binary[5] = '1') then Inc(sub_target, 4);
          if (decimal_binary[6] = '1') then Inc(sub_target, 2);
          if (decimal_binary[7] = '1') then Inc(sub_target, 1);
          target[tp] := sub_target; Inc(tp);
          strcpy(temp_binary, '');
          for i := 0 to strlen(decimal_binary) - 8 do
            temp_binary[i] := decimal_binary[i + 8];
          strcpy(decimal_binary, temp_binary);
        end;
      end;
    end;

    if (current_mode = C1_BYTE) then
    begin
      next_mode := C1_BYTE;

      if (gs1 <> 0) and (source[sp] = Ord('[')) then
      begin
        next_mode := C1_ASCII;
      end
      else
      begin
        if (source[sp] <= 127) then
        begin
          next_mode := c1_look_ahead_test(source, _length, sp, current_mode, gs1);
        end;
      end;

      if (next_mode <> C1_BYTE) then
      begin
        { Insert byte field length }
        if ((tp - byte_start) <= 249) then
        begin
          for i := tp downto byte_start do
            target[i + 1] := target[i];

          target[byte_start] := (tp - byte_start);
          Inc(tp);
        end
        else
        begin
          for i := tp downto byte_start do
            target[i + 2] := target[i];

          target[byte_start] := 249 + ((tp - byte_start) div 250);
          target[byte_start + 1] := ((tp - byte_start) mod 250);
          Inc(tp, 2);
        end;
      end
      else
      begin
        target[tp] := source[sp];
        Inc(tp);
        Inc(sp);
      end;
    end;

    if (tp > 1480) then
    begin
      { Data is too large for symbol }
      strcpy(symbol.errtxt, 'Input data too long');
      result := 0; exit;
    end;
  until not (sp < _length);

  { Empty buffers }
  if ((current_mode = C1_C40) and (c40_p = 1)) or ((current_mode = C1_TEXT) and (text_p = 1)) then
  begin
    { C-parity: if exactly one codeword remains and the last buffered C40/TEXT char is eligible,
      encode that single char as ASCII instead of padded triplet + unlatch. }
    if symbol.option_2 = 10 then  { Version T }
    begin
      if tp <= 10 then data_left := 10 - tp
      else if tp <= 24 then data_left := 24 - tp
      else data_left := 38 - tp;
    end
    else
    begin
      data_left := c1_data_length[0] - tp;
      for i := 0 to 6 do
        if tp > c1_data_length[i] then
          data_left := c1_data_length[i + 1] - tp;
    end;

    if (data_left = 1) and (sp > 0) then
    begin
      if (current_mode = C1_C40) and
        ((source[sp - 1] = Ord(' ')) or
         ((source[sp - 1] >= Ord('0')) and (source[sp - 1] <= Ord('9'))) or
         ((source[sp - 1] >= Ord('A')) and (source[sp - 1] <= Ord('Z')))) then
      begin
        target[tp] := source[sp - 1] + 1;
        Inc(tp);
        c40_p := 0;
        current_mode := C1_ASCII;
      end
      else if (current_mode = C1_TEXT) and
        ((source[sp - 1] = Ord(' ')) or
         ((source[sp - 1] >= Ord('0')) and (source[sp - 1] <= Ord('9'))) or
         ((source[sp - 1] >= Ord('a')) and (source[sp - 1] <= Ord('z')))) then
      begin
        target[tp] := source[sp - 1] + 1;
        Inc(tp);
        text_p := 0;
        current_mode := C1_ASCII;
      end;
    end;
  end;

  if (c40_p = 2) then
  begin
    c40_buffer[2] := 1;
    iv := (1600 * c40_buffer[0]) + (40 * c40_buffer[1]) + (c40_buffer[2]) + 1;
    target[tp] := iv div 256; Inc(tp);
    target[tp] := iv mod 256; Inc(tp);
    target[tp] := 255; Inc(tp); { Unlatch }
  end;
  if (c40_p = 1) then
  begin
    c40_buffer[1] := 1;
    c40_buffer[2] := 31; { Pad }
    iv := (1600 * c40_buffer[0]) + (40 * c40_buffer[1]) + (c40_buffer[2]) + 1;
    target[tp] := iv div 256; Inc(tp);
    target[tp] := iv mod 256; Inc(tp);
    target[tp] := 255; Inc(tp); { Unlatch }
  end;
  if (text_p = 2) then
  begin
    text_buffer[2] := 1;
    iv := (1600 * text_buffer[0]) + (40 * text_buffer[1]) + (text_buffer[2]) + 1;
    target[tp] := iv div 256; Inc(tp);
    target[tp] := iv mod 256; Inc(tp);
    target[tp] := 255; Inc(tp); { Unlatch }
  end;
  if (text_p = 1) then
  begin
    text_buffer[1] := 1;
    text_buffer[2] := 31; { Pad }
    iv := (1600 * text_buffer[0]) + (40 * text_buffer[1]) + (text_buffer[2]) + 1;
    target[tp] := iv div 256; Inc(tp);
    target[tp] := iv mod 256; Inc(tp);
    target[tp] := 255; Inc(tp); { Unlatch }
  end;

  if (current_mode = C1_DECIMAL) then
  begin
    { Finish Decimal mode - conditionally add unlatch (C-parity: c1_codewords_remaining > 1) }
    { Compute remaining codewords available (mirrors c1_codewords_remaining()) }
    if symbol.option_2 = 10 then  { Version T }
    begin
      if tp <= 10 then data_left := 10 - tp
      else if tp <= 24 then data_left := 24 - tp
      else data_left := 38 - tp;
    end
    else  { Versions A to H (option_2 = 0..8) }
    begin
      data_left := c1_data_length[0] - tp;
      for i := 0 to 6 do
        if tp > c1_data_length[i] then
          data_left := c1_data_length[i + 1] - tp;
    end;

    if data_left > 1 then
      concat(decimal_binary, '111111'); { Unlatch }

    { Only process bits if there is something in the buffer }
    if strlen(decimal_binary) > 0 then
    begin
      target_count := 3;
      if (strlen(decimal_binary) <= 16) then target_count := 2;
      if (strlen(decimal_binary) <= 8) then target_count := 1;
      bits_left_in_byte := (8 * target_count) - strlen(decimal_binary);
      if (bits_left_in_byte = 8) then bits_left_in_byte := 0;

      if (bits_left_in_byte = 2) then
        concat(decimal_binary, '01');

      if ((bits_left_in_byte = 4) or (bits_left_in_byte = 6)) then
        concat(decimal_binary, '1111');

      if (bits_left_in_byte = 6) then
        concat(decimal_binary, '01');

      { Binary buffer is full - transfer to target }
      if (target_count >= 1) then
      begin
        sub_target := 0;
        if (decimal_binary[0] = '1') then Inc(sub_target, 128);
        if (decimal_binary[1] = '1') then Inc(sub_target, 64);
        if (decimal_binary[2] = '1') then Inc(sub_target, 32);
        if (decimal_binary[3] = '1') then Inc(sub_target, 16);
        if (decimal_binary[4] = '1') then Inc(sub_target, 8);
        if (decimal_binary[5] = '1') then Inc(sub_target, 4);
        if (decimal_binary[6] = '1') then Inc(sub_target, 2);
        if (decimal_binary[7] = '1') then Inc(sub_target, 1);
        target[tp] := sub_target; Inc(tp);
      end;
      if (target_count >= 2) then
      begin
        sub_target := 0;
        if (decimal_binary[8] = '1') then Inc(sub_target, 128);
        if (decimal_binary[9] = '1') then Inc(sub_target, 64);
        if (decimal_binary[10] = '1') then Inc(sub_target, 32);
        if (decimal_binary[11] = '1') then Inc(sub_target, 16);
        if (decimal_binary[12] = '1') then Inc(sub_target, 8);
        if (decimal_binary[13] = '1') then Inc(sub_target, 4);
        if (decimal_binary[14] = '1') then Inc(sub_target, 2);
        if (decimal_binary[15] = '1') then Inc(sub_target, 1);
        target[tp] := sub_target; Inc(tp);
      end;
      if (target_count = 3) then
      begin
        sub_target := 0;
        if (decimal_binary[16] = '1') then Inc(sub_target, 128);
        if (decimal_binary[17] = '1') then Inc(sub_target, 64);
        if (decimal_binary[18] = '1') then Inc(sub_target, 32);
        if (decimal_binary[19] = '1') then Inc(sub_target, 16);
        if (decimal_binary[20] = '1') then Inc(sub_target, 8);
        if (decimal_binary[21] = '1') then Inc(sub_target, 4);
        if (decimal_binary[22] = '1') then Inc(sub_target, 2);
        if (decimal_binary[23] = '1') then Inc(sub_target, 1);
        target[tp] := sub_target; Inc(tp);
      end;
    end;
  end;

  if (current_mode = C1_BYTE) then
  begin
    { Insert byte field length }
    if ((tp - byte_start) <= 249) then
    begin
      for i := tp downto byte_start do
        target[i + 1] := target[i];
      target[byte_start] := (tp - byte_start);
      Inc(tp);
    end
    else
    begin
      for i := tp downto byte_start do
        target[i + 2] := target[i];
      target[byte_start] := 249 + ((tp - byte_start) div 250);
      target[byte_start + 1] := ((tp - byte_start) mod 250);
      Inc(tp, 2);
    end;
  end;

  { Re-check _length of data }
  if (tp > 1480) then
  begin
    { Data is too large for symbol }
    strcpy(symbol.errtxt, 'Input data too long');
    result := 0; exit;
  end;
  {$IFDEF DEBUG_ZINT}
  WriteLn('targets:');
  for i := 0 to tp - 1 do
    Write(Format('[%d]',[ target[i]]));
  WriteLn;
  {$ENDIF}
  last_mode := current_mode;
  result := tp; exit;
end;

procedure block_copy(symbol : zint_symbol; const grid : TArrayOfArrayOfChar; start_row : Integer; start_col : Integer; height : Integer; width : Integer; row_offset : Integer; col_offset : Integer);
var
  i, j : Integer;
begin
  for i := start_row to (start_row + height) - 1 do
  begin
    for j := start_col to (start_col + width) - 1 do
    begin
      if (grid[i][j] = '1') then
        set_module(symbol, i + row_offset, j + col_offset);
    end;
  end;
end;

function code_one(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  size, data_blocks : Integer;
  datagrid : TArrayOfArrayOfChar;
  sub_version : Integer;
  codewords : Integer;
  elreg : TArrayOfSmallInt;
  data, ecc : TArrayOfCardinal;
  stream : TArrayOfInteger;
  block_width : Integer;
  i, j : Integer;
  row, col : Integer;
  data_length : Integer;
  data_cw, ecc_cw : Integer;
  sub_data, sub_ecc : TArrayOfCardinal;
  RSGlobals : TRSGlobals;
  base_gs1 : Integer;
  warning_number : Integer;
  warning_msg : String;
  last_mode : Integer;
  work_source : TArrayOfByte;
  work_length : Integer;
  use_eci_escape : Integer;
  eci_start_tp : Integer;
  all_digits : Integer;
begin
  sub_version := 3;
  size := 1;
  base_gs1 := 0;
  warning_number := 0;
  warning_msg := '';
  SetLength(datagrid, 136);
  for i := Low(datagrid) to High(datagrid) do SetLength(datagrid[i], 120);

  if ((symbol.input_mode and $07) = GS1_MODE) then
    base_gs1 := 1;

  if ((symbol.option_2 < 0) or (symbol.option_2 > 10)) then
  begin
    strcpy(symbol.errtxt, Format('Error 513: Version ''%d'' out of range (1 to 10)', [symbol.option_2]));
    result := ZERROR_INVALID_OPTION; exit;
  end;

  if (symbol.structapp.count <> 0) then
  begin
    if (symbol.option_2 = 9) then
    begin
      strcpy(symbol.errtxt, 'Error 714: Structured Append not available for Version S');
      result := ZERROR_INVALID_OPTION; exit;
    end;
    if (base_gs1 <> 0) then
    begin
      strcpy(symbol.errtxt, 'Error 710: Cannot have Structured Append and GS1 mode at the same time');
      result := ZERROR_INVALID_OPTION; exit;
    end;
    if (symbol.structapp.count < 2) or (symbol.structapp.count > 128) then
    begin
      strcpy(symbol.errtxt, Format('Error 711: Structured Append count ''%d'' out of range (2 to 128)', [symbol.structapp.count]));
      result := ZERROR_INVALID_OPTION; exit;
    end;
    if (symbol.structapp.index < 1) or (symbol.structapp.index > symbol.structapp.count) then
    begin
      strcpy(symbol.errtxt, Format('Error 712: Structured Append index ''%d'' out of range (1 to count %d)',
        [symbol.structapp.index, symbol.structapp.count]));
      result := ZERROR_INVALID_OPTION; exit;
    end;
    if symbol.structapp.id <> '' then
    begin
      strcpy(symbol.errtxt, 'Error 713: Structured Append ID not available for Code One');
      result := ZERROR_INVALID_OPTION; exit;
    end;
  end;

  if (symbol.option_2 = 9) then
  begin
    if (symbol.eci <> 0) or (base_gs1 <> 0) then
    begin
      warning_number := ZWARN_INVALID_OPTION;
      if (symbol.eci <> 0) and (base_gs1 <> 0) then
        warning_msg := 'Warning 511: ECI and GS1 mode ignored for Version S'
      else if (symbol.eci <> 0) then
        warning_msg := 'Warning 511: ECI ignored for Version S'
      else
        warning_msg := 'Warning 511: GS1 mode ignored for Version S';
    end;
  end
  else if (symbol.eci <> 0) and (base_gs1 <> 0) then
  begin
    warning_number := ZWARN_INVALID_OPTION;
    warning_msg := 'Warning 512: ECI ignored for GS1 mode';
  end;

  if (symbol.option_2 = 9) then
  begin
    { Version S }
    SetLength(elreg, 112);
    SetLength(data, 15); SetLength(ecc, 15);
    SetLength(stream, 30);

    if (_length > 18) then
    begin
      strcpy(symbol.errtxt, Format('Error 514: Input length %d too long for Version S (maximum 18)', [_length]));
      result := ZERROR_TOO_LONG; exit;
    end;
    for i := 0 to _length - 1 do
    begin
      if (source[i] < Ord('0')) or (source[i] > Ord('9')) then
      begin
        strcpy(symbol.errtxt, Format('Error 515: Invalid character at position %d in input (Version S encodes digits only)', [i + 1]));
        result := ZERROR_INVALID_DATA; exit;
      end;
    end;

    sub_version := 3; codewords := 12; block_width := 6; { Version S-30 }
    if (_length <= 12) then begin sub_version := 2; codewords := 8; block_width := 4; end; { Version S-20 }
    if (_length <= 6) then begin sub_version := 1; codewords := 4; block_width := 2; end; { Version S-10 }

    binary_load(elreg, ArrayOfByteToArrayOfChar(source), _length);
    {$IFDEF DEBUG_ZINT}
    hex_dump(elreg);
    {$ENDIF}

    for i := 0 to 14 do
    begin
      data[i] := 0;
      ecc[i] := 0;
    end;

    for i := 0 to codewords - 1 do
    begin
      Inc(data[codewords - i - 1], 1 * elreg[(i * 5)]);
      Inc(data[codewords - i - 1], 2 * elreg[(i * 5) + 1]);
      Inc(data[codewords - i - 1], 4 * elreg[(i * 5) + 2]);
      Inc(data[codewords - i - 1], 8 * elreg[(i * 5) + 3]);
      Inc(data[codewords - i - 1], 16 * elreg[(i * 5) + 4]);
    end;

    rs_init_gf($25, RSGlobals);
    rs_init_code(codewords, 1, RSGlobals);
    rs_encode_long(codewords, data, ecc, RSGlobals);
    rs_free(RSGlobals);

    for i := 0 to codewords - 1 do
    begin
      stream[i] := data[i];
      stream[i + codewords] := ecc[codewords - i - 1];
    end;

    for i := 0 to 135 do
      for j := 0 to 119 do
        datagrid[i][j] := '0';

    i := 0;
    for row := 0 to 1 do
    begin
      for col := 0 to block_width - 1 do
      begin
        if (stream[i] and $10) <> 0 then
          datagrid[row * 2][col * 5] := '1';
        if (stream[i] and $08) <> 0 then
          datagrid[row * 2][(col * 5) + 1] := '1';
        if (stream[i] and $04) <> 0 then
          datagrid[row * 2][(col * 5) + 2] := '1';
        if (stream[i] and $02) <> 0 then
          datagrid[(row * 2) + 1][col * 5] := '1';
        if (stream[i] and $01) <> 0 then
          datagrid[(row * 2) + 1][(col * 5) + 1] := '1';
        if (stream[i + 1] and $10) <> 0 then
          datagrid[row * 2][(col * 5) + 3] := '1';
        if (stream[i + 1] and $08) <> 0 then
          datagrid[row * 2][(col * 5) + 4] := '1';
        if (stream[i + 1] and $04) <> 0 then
          datagrid[(row * 2) + 1][(col * 5) + 2] := '1';
        if (stream[i + 1] and $02) <> 0 then
          datagrid[(row * 2) + 1][(col * 5) + 3] := '1';
        if (stream[i + 1] and $01) <> 0 then
          datagrid[(row * 2) + 1][(col * 5) + 4] := '1';
        Inc(i, 2);
      end;
    end;

    size := 9;
    symbol.option_2 := 9;
    symbol.rows := 8;
    symbol.width := 10 * sub_version + 1;
  end;

  if (symbol.option_2 = 10) then
  begin
    { Version T }
    { Buffer needs to hold encoded form of up to 90 bytes; worst case is ~91 codewords
      (BYTE mode latch + 90 bytes). 128 is safe headroom to avoid out-of-bounds writes
      inside c1_encode before the data_length > 38 guard triggers. }
    SetLength(data, 128); SetLength(ecc, 25);
    SetLength(stream, 65);

    use_eci_escape := 0;
    work_length := _length;
    SetLength(work_source, _length);
    if _length > 0 then
      Move(source[0], work_source[0], _length);

    if (symbol.eci <> 0) and (base_gs1 = 0) then
    begin
      c1_eci_escape(symbol.eci, source, _length, work_source, work_length);
      use_eci_escape := 1;
    end;

    eci_start_tp := 0;
    if (use_eci_escape <> 0) then
    begin
      data[0] := 129; { Pad }
      eci_start_tp := 1;
    end;

    if (work_length > 90) then
    begin
      strcpy(symbol.errtxt, Format('Error 519: Input length %d too long for Version T (maximum 90)', [work_length]));
      result := ZERROR_TOO_LONG; exit;
    end;

    all_digits := 1;
    for i := 0 to _length - 1 do
    begin
      if (source[i] < Ord('0')) or (source[i] > Ord('9')) then
      begin
        all_digits := 0;
        Break;
      end;
    end;

    if (use_eci_escape <> 0) and (all_digits <> 0) and (_length >= 77) then
    begin
      strcpy(symbol.errtxt, 'Error 516: Input too long for Version T, requires 39 codewords (maximum 38)');
      result := ZERROR_TOO_LONG; exit;
    end;

    for i := 0 to 39 do
      if i >= eci_start_tp then
        data[i] := 0;

    data_length := c1_encode(symbol, work_source, data, work_length, last_mode, eci_start_tp);

    if (data_length = 0) then
    begin
      result := ZERROR_TOO_LONG; exit;
    end;

    if (data_length > 38) then
    begin
      strcpy(symbol.errtxt, Format('Error 516: Input too long for Version T, requires %d codewords (maximum 38)', [data_length]));
      result := ZERROR_TOO_LONG; exit;
    end;

    size := 10;
    sub_version := 3; data_cw := 38; ecc_cw := 22; block_width := 12;
    if (data_length <= 24) then begin sub_version := 2; data_cw := 24; ecc_cw := 16; block_width := 8; end;
    if (data_length <= 10) then begin sub_version := 1; data_cw := 10; ecc_cw := 10; block_width := 4; end;

    if (data_cw > data_length) then
    begin
      if (last_mode <> C1_ASCII) and (last_mode <> C1_BYTE) then
      begin
        data[data_length] := 255; { Unlatch }
        Inc(data_length);
      end;
      for i := data_length to data_cw - 1 do
        data[i] := 129; { Pad }
    end;

    { Calculate error correction data }
    rs_init_gf($12d, RSGlobals);
    rs_init_code(ecc_cw, 1, RSGlobals);
    rs_encode_long(data_cw, data, ecc, RSGlobals);
    rs_free(RSGlobals);

    { 'Stream' combines data and error correction data }
    for i := 0 to data_cw - 1 do
      stream[i] := data[i];

    for i := 0 to ecc_cw - 1 do
      stream[data_cw + i] := ecc[ecc_cw - i - 1];

    for i := 0 to 135 do
      for j := 0 to 119 do
        datagrid[i][j] := '0';

    i := 0;
    for row := 0 to 4  do
    begin
      for col := 0 to block_width - 1 do
      begin
        if (stream[i] and $80) <> 0 then
          datagrid[row * 2][col * 4] := '1';
        if (stream[i] and $40) <> 0 then
          datagrid[row * 2][(col * 4) + 1] := '1';
        if (stream[i] and $20) <> 0 then
          datagrid[row * 2][(col * 4) + 2] := '1';
        if (stream[i] and $10) <> 0 then
          datagrid[row * 2][(col * 4) + 3] := '1';
        if (stream[i] and $08) <> 0 then
          datagrid[(row * 2) + 1][col * 4] := '1';
        if (stream[i] and $04) <> 0 then
          datagrid[(row * 2) + 1][(col * 4) + 1] := '1';
        if (stream[i] and $02) <> 0 then
          datagrid[(row * 2) + 1][(col * 4) + 2] := '1';
        if (stream[i] and $01) <> 0 then
          datagrid[(row * 2) + 1][(col * 4) + 3] := '1';
        Inc(i);
      end;
    end;

    symbol.rows := 16;
    symbol.width := (sub_version * 16) + 1;
    symbol.option_2 := 10;
  end;

  if ((symbol.option_2 <> 9) and (symbol.option_2 <> 10)) then
  begin
    { Version A to H }
    SetLength(data, 1500); SetLength(ecc, 600);
    SetLength(sub_data, 190); SetLength(sub_ecc, 75);
    SetLength(stream, 2100);

    use_eci_escape := 0;
    work_length := _length;
    SetLength(work_source, _length);
    if _length > 0 then
      Move(source[0], work_source[0], _length);

    if (symbol.eci <> 0) and (base_gs1 = 0) then
    begin
      c1_eci_escape(symbol.eci, source, _length, work_source, work_length);
      use_eci_escape := 1;
    end;

    eci_start_tp := 0;
    if (use_eci_escape <> 0) then
    begin
      data[0] := 129; { Pad }
      eci_start_tp := 1;
    end;

    for i := 0 to 1499 do
      if i >= eci_start_tp then
        data[i] := 0;

    data_length := c1_encode(symbol, work_source, data, work_length, last_mode, eci_start_tp);

    if (data_length = 0) then
    begin
      strcpy(symbol.errtxt, 'Error 517: Input too long, requires too many codewords (maximum 1480)');
      result := ZERROR_TOO_LONG; exit;
    end;

    for i := 7 downto 0 do
    begin
      if (c1_data_length[i] >= data_length) then
        size := i + 1;
    end;

    if (symbol.option_2 > size) then
    begin
      size := symbol.option_2;
    end;

    if (symbol.option_2 <> 0) and (symbol.option_2 < size) then
    begin
      strcpy(symbol.errtxt, Format('Error 518: Input too long for Version %s, requires %d codewords (maximum %d)',
        [Chr(Ord('A') + symbol.option_2 - 1), data_length, c1_data_length[symbol.option_2 - 1]]));
      result := ZERROR_TOO_LONG; exit;
    end;

    symbol.option_2 := size;

    if (c1_data_length[size - 1] > data_length) then
    begin
      if (last_mode <> C1_ASCII) and (last_mode <> C1_BYTE) then
      begin
        data[data_length] := 255; { Unlatch }
        Inc(data_length);
      end;
      for i := data_length to c1_data_length[size - 1] - 1 do
        data[i] := 129; { Pad }
    end;

    { Calculate error correction data }
    data_length := c1_data_length[size - 1];
    for i := 0 to 189 do
      sub_data[i] := 0;

    for i := 0 to 74 do
      sub_ecc[i] := 0;

    data_blocks := c1_blocks[size - 1];

    rs_init_gf($12d, RSGlobals);
    rs_init_code(c1_ecc_blocks[size - 1], 0, RSGlobals);

    for i := 0 to data_blocks - 1 do
    begin
      for j := 0 to c1_data_blocks[size - 1] - 1 do
      begin
        sub_data[j] := data[j * data_blocks + i];
      end;
      rs_encode_long(c1_data_blocks[size - 1], sub_data, sub_ecc, RSGlobals);
      for j := 0 to c1_ecc_blocks[size - 1] - 1 do
        ecc[c1_ecc_length[size - 1] - (j * data_blocks + i) - 1] := sub_ecc[j];
    end;
    rs_free(RSGlobals);

    { 'Stream' combines data and error correction data }
    for i := 0 to data_length - 1 do
      stream[i] := data[i];

    for i := 0 to c1_ecc_length[size - 1] - 1 do
      stream[data_length + i] := ecc[i];

    for i := 0 to 135 do
      for j := 0 to 119 do
        datagrid[i][j] := '0';

    i := 0;
    for row := 0 to c1_grid_height[size - 1] - 1 do
    begin
      for col := 0 to c1_grid_width[size - 1] - 1 do
      begin
        if (stream[i] and $80) <> 0 then
          datagrid[row * 2][col * 4] := '1';
        if (stream[i] and $40) <> 0 then
          datagrid[row * 2][(col * 4) + 1] := '1';
        if (stream[i] and $20) <> 0 then
          datagrid[row * 2][(col * 4) + 2] := '1';
        if (stream[i] and $10) <> 0 then
          datagrid[row * 2][(col * 4) + 3] := '1';
        if (stream[i] and $08) <> 0 then
          datagrid[(row * 2) + 1][col * 4] := '1';
        if (stream[i] and $04) <> 0 then
          datagrid[(row * 2) + 1][(col * 4) + 1] := '1';
        if (stream[i] and $02) <> 0 then
          datagrid[(row * 2) + 1][(col * 4) + 2] := '1';
        if (stream[i] and $01) <> 0 then
          datagrid[(row * 2) + 1][(col * 4) + 3] := '1';
        Inc(i);
      end;
    end;

    symbol.rows := c1_height[size - 1];
    symbol.width := c1_width[size - 1];
  end;

  case size of
    1: { Version A }
    begin
      central_finder(symbol, 6, 3, 1);
      vert(symbol, 4, 6, 1);
      vert(symbol, 12, 5, 0);
      set_module(symbol, 5, 12);
      spigot(symbol, 0);
      spigot(symbol, 15);
      block_copy(symbol, datagrid, 0, 0, 5, 4, 0, 0);
      block_copy(symbol, datagrid, 0, 4, 5, 12, 0, 2);
      block_copy(symbol, datagrid, 5, 0, 5, 12, 6, 0);
      block_copy(symbol, datagrid, 5, 12, 5, 4, 6, 2);
    end;
    2: { Version B }
    begin
      central_finder(symbol, 8, 4, 1);
      vert(symbol, 4, 8, 1);
      vert(symbol, 16, 7, 0);
      set_module(symbol, 7, 16);
      spigot(symbol, 0);
      spigot(symbol, 21);
      block_copy(symbol, datagrid, 0, 0, 7, 4, 0, 0);
      block_copy(symbol, datagrid, 0, 4, 7, 16, 0, 2);
      block_copy(symbol, datagrid, 7, 0, 7, 16, 8, 0);
      block_copy(symbol, datagrid, 7, 16, 7, 4, 8, 2);
    end;
    3: { Version C }
    begin
      central_finder(symbol, 11, 4, 2);
      vert(symbol, 4, 11, 1);
      vert(symbol, 26, 13, 1);
      vert(symbol, 4, 10, 0);
      vert(symbol, 26, 10, 0);
      spigot(symbol, 0);
      spigot(symbol, 27);
      block_copy(symbol, datagrid, 0, 0, 10, 4, 0, 0);
      block_copy(symbol, datagrid, 0, 4, 10, 20, 0, 2);
      block_copy(symbol, datagrid, 0, 24, 10, 4, 0, 4);
      block_copy(symbol, datagrid, 10, 0, 10, 4, 8, 0);
      block_copy(symbol, datagrid, 10, 4, 10, 20, 8, 2);
      block_copy(symbol, datagrid, 10, 24, 10, 4, 8, 4);
    end;
    4: { Version D }
    begin
      central_finder(symbol, 16, 5, 1);
      vert(symbol, 4, 16, 1);
      vert(symbol, 20, 16, 1);
      vert(symbol, 36, 16, 1);
      vert(symbol, 4, 15, 0);
      vert(symbol, 20, 15, 0);
      vert(symbol, 36, 15, 0);
      spigot(symbol, 0);
      spigot(symbol, 12);
      spigot(symbol, 27);
      spigot(symbol, 39);
      block_copy(symbol, datagrid, 0, 0, 15, 4, 0, 0);
      block_copy(symbol, datagrid, 0, 4, 15, 14, 0, 2);
      block_copy(symbol, datagrid, 0, 18, 15, 14, 0, 4);
      block_copy(symbol, datagrid, 0, 32, 15, 4, 0, 6);
      block_copy(symbol, datagrid, 15, 0, 15, 4, 10, 0);
      block_copy(symbol, datagrid, 15, 4, 15, 14, 10, 2);
      block_copy(symbol, datagrid, 15, 18, 15, 14, 10, 4);
      block_copy(symbol, datagrid, 15, 32, 15, 4, 10, 6);
    end;
    5: { Version E }
    begin
      central_finder(symbol, 22, 5, 2);
      vert(symbol, 4, 22, 1);
      vert(symbol, 26, 24, 1);
      vert(symbol, 48, 22, 1);
      vert(symbol, 4, 21, 0);
      vert(symbol, 26, 21, 0);
      vert(symbol, 48, 21, 0);
      spigot(symbol, 0);
      spigot(symbol, 12);
      spigot(symbol, 39);
      spigot(symbol, 51);
      block_copy(symbol, datagrid, 0, 0, 21, 4, 0, 0);
      block_copy(symbol, datagrid, 0, 4, 21, 20, 0, 2);
      block_copy(symbol, datagrid, 0, 24, 21, 20, 0, 4);
      block_copy(symbol, datagrid, 0, 44, 21, 4, 0, 6);
      block_copy(symbol, datagrid, 21, 0, 21, 4, 10, 0);
      block_copy(symbol, datagrid, 21, 4, 21, 20, 10, 2);
      block_copy(symbol, datagrid, 21, 24, 21, 20, 10, 4);
      block_copy(symbol, datagrid, 21, 44, 21, 4, 10, 6);
    end;
    6: { Version F }
    begin
      central_finder(symbol, 31, 5, 3);
      vert(symbol, 4, 31, 1);
      vert(symbol, 26, 35, 1);
      vert(symbol, 48, 31, 1);
      vert(symbol, 70, 35, 1);
      vert(symbol, 4, 30, 0);
      vert(symbol, 26, 30, 0);
      vert(symbol, 48, 30, 0);
      vert(symbol, 70, 30, 0);
      spigot(symbol, 0);
      spigot(symbol, 12);
      spigot(symbol, 24);
      spigot(symbol, 45);
      spigot(symbol, 57);
      spigot(symbol, 69);
      block_copy(symbol, datagrid, 0, 0, 30, 4, 0, 0);
      block_copy(symbol, datagrid, 0, 4, 30, 20, 0, 2);
      block_copy(symbol, datagrid, 0, 24, 30, 20, 0, 4);
      block_copy(symbol, datagrid, 0, 44, 30, 20, 0, 6);
      block_copy(symbol, datagrid, 0, 64, 30, 4, 0, 8);
      block_copy(symbol, datagrid, 30, 0, 30, 4, 10, 0);
      block_copy(symbol, datagrid, 30, 4, 30, 20, 10, 2);
      block_copy(symbol, datagrid, 30, 24, 30, 20, 10, 4);
      block_copy(symbol, datagrid, 30, 44, 30, 20, 10, 6);
      block_copy(symbol, datagrid, 30, 64, 30, 4, 10, 8);
    end;
    7: { Version G }
    begin
      central_finder(symbol, 47, 6, 2);
      vert(symbol, 6, 47, 1);
      vert(symbol, 27, 49, 1);
      vert(symbol, 48, 47, 1);
      vert(symbol, 69, 49, 1);
      vert(symbol, 90, 47, 1);
      vert(symbol, 6, 46, 0);
      vert(symbol, 27, 46, 0);
      vert(symbol, 48, 46, 0);
      vert(symbol, 69, 46, 0);
      vert(symbol, 90, 46, 0);
      spigot(symbol, 0);
      spigot(symbol, 12);
      spigot(symbol, 24);
      spigot(symbol, 36);
      spigot(symbol, 67);
      spigot(symbol, 79);
      spigot(symbol, 91);
      spigot(symbol, 103);
      block_copy(symbol, datagrid, 0, 0, 46, 6, 0, 0);
      block_copy(symbol, datagrid, 0, 6, 46, 19, 0, 2);
      block_copy(symbol, datagrid, 0, 25, 46, 19, 0, 4);
      block_copy(symbol, datagrid, 0, 44, 46, 19, 0, 6);
      block_copy(symbol, datagrid, 0, 63, 46, 19, 0, 8);
      block_copy(symbol, datagrid, 0, 82, 46, 6, 0, 10);
      block_copy(symbol, datagrid, 46, 0, 46, 6, 12, 0);
      block_copy(symbol, datagrid, 46, 6, 46, 19, 12, 2);
      block_copy(symbol, datagrid, 46, 25, 46, 19, 12, 4);
      block_copy(symbol, datagrid, 46, 44, 46, 19, 12, 6);
      block_copy(symbol, datagrid, 46, 63, 46, 19, 12, 8);
      block_copy(symbol, datagrid, 46, 82, 46, 6, 12, 10);
    end;
    8: { Version H }
    begin
      central_finder(symbol, 69, 6, 3);
      vert(symbol, 6, 69, 1);
      vert(symbol, 26, 73, 1);
      vert(symbol, 46, 69, 1);
      vert(symbol, 66, 73, 1);
      vert(symbol, 86, 69, 1);
      vert(symbol, 106, 73, 1);
      vert(symbol, 126, 69, 1);
      vert(symbol, 6, 68, 0);
      vert(symbol, 26, 68, 0);
      vert(symbol, 46, 68, 0);
      vert(symbol, 66, 68, 0);
      vert(symbol, 86, 68, 0);
      vert(symbol, 106, 68, 0);
      vert(symbol, 126, 68, 0);
      spigot(symbol, 0);
      spigot(symbol, 12);
      spigot(symbol, 24);
      spigot(symbol, 36);
      spigot(symbol, 48);
      spigot(symbol, 60);
      spigot(symbol, 87);
      spigot(symbol, 99);
      spigot(symbol, 111);
      spigot(symbol, 123);
      spigot(symbol, 135);
      spigot(symbol, 147);
      block_copy(symbol, datagrid, 0, 0, 68, 6, 0, 0);
      block_copy(symbol, datagrid, 0, 6, 68, 18, 0, 2);
      block_copy(symbol, datagrid, 0, 24, 68, 18, 0, 4);
      block_copy(symbol, datagrid, 0, 42, 68, 18, 0, 6);
      block_copy(symbol, datagrid, 0, 60, 68, 18, 0, 8);
      block_copy(symbol, datagrid, 0, 78, 68, 18, 0, 10);
      block_copy(symbol, datagrid, 0, 96, 68, 18, 0, 12);
      block_copy(symbol, datagrid, 0, 114, 68, 6, 0, 14);
      block_copy(symbol, datagrid, 68, 0, 68, 6, 12, 0);
      block_copy(symbol, datagrid, 68, 6, 68, 18, 12, 2);
      block_copy(symbol, datagrid, 68, 24, 68, 18, 12, 4);
      block_copy(symbol, datagrid, 68, 42, 68, 18, 12, 6);
      block_copy(symbol, datagrid, 68, 60, 68, 18, 12, 8);
      block_copy(symbol, datagrid, 68, 78, 68, 18, 12, 10);
      block_copy(symbol, datagrid, 68, 96, 68, 18, 12, 12);
      block_copy(symbol, datagrid, 68, 114, 68, 6, 12, 14);
    end;
    9: { Version S }
    begin
      horiz(symbol, 5, 1);
      horiz(symbol, 7, 1);
      set_module(symbol, 6, 0);
      set_module(symbol, 6, symbol.width - 1);
      unset_module(symbol, 7, 1);
      unset_module(symbol, 7, symbol.width - 2);
      case sub_version of
        1: { Version S-10 }
        begin
          set_module(symbol, 0, 5);
          block_copy(symbol, datagrid, 0, 0, 4, 5, 0, 0);
          block_copy(symbol, datagrid, 0, 5, 4, 5, 0, 1);
        end;
        2: { Version S-20 }
        begin
          set_module(symbol, 0, 10);
          set_module(symbol, 4, 10);
          block_copy(symbol, datagrid, 0, 0, 4, 10, 0, 0);
          block_copy(symbol, datagrid, 0, 10, 4, 10, 0, 1);
        end;
        3: { Version S-30 }
        begin
          set_module(symbol, 0, 15);
          set_module(symbol, 4, 15);
          set_module(symbol, 6, 15);
          block_copy(symbol, datagrid, 0, 0, 4, 15, 0, 0);
          block_copy(symbol, datagrid, 0, 15, 4, 15, 0, 1);
        end;
      end;
    end;
    10: { Version T }
    begin
      horiz(symbol, 11, 1);
      horiz(symbol, 13, 1);
      horiz(symbol, 15, 1);
      set_module(symbol, 12, 0);
      set_module(symbol, 12, symbol.width - 1);
      set_module(symbol, 14, 0);
      set_module(symbol, 14, symbol.width - 1);
      unset_module(symbol, 13, 1);
      unset_module(symbol, 13, symbol.width - 2);
      unset_module(symbol, 15, 1);
      unset_module(symbol, 15, symbol.width - 2);
      case sub_version of
        1: { Version T-16 }
        begin
          set_module(symbol, 0, 8);
          set_module(symbol, 10, 8);
          block_copy(symbol, datagrid, 0, 0, 10, 8, 0, 0);
          block_copy(symbol, datagrid, 0, 8, 10, 8, 0, 1);
        end;
        2: { Version T-32 }
        begin
          set_module(symbol, 0, 16);
          set_module(symbol, 10, 16);
          set_module(symbol, 12, 16);
          block_copy(symbol, datagrid, 0, 0, 10, 16, 0, 0);
          block_copy(symbol, datagrid, 0, 16, 10, 16, 0, 1);
        end;
        3: { Verion T-48 }
        begin
          set_module(symbol, 0, 24);
          set_module(symbol, 10, 24);
          set_module(symbol, 12, 24);
          set_module(symbol, 14, 24);
          block_copy(symbol, datagrid, 0, 0, 10, 24, 0, 0);
          block_copy(symbol, datagrid, 0, 24, 10, 24, 0, 1);
        end;
      end;
    end;
  end;

  for i := 0 to symbol.rows - 1 do
    symbol.row_height[i] := 1;

  if warning_number <> 0 then
  begin
    strcpy(symbol.errtxt, warning_msg);
    result := warning_number; exit;
  end;

  result := 0; exit;
end;

end.

