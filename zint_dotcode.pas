unit zint_dotcode;

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

function dotCode(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses
  System.SysUtils, System.Math, Winapi.Windows, zint_reedsol, zint_common;


{* DotCode symbol character dot patterns, from Annex C *}
const
  dot_patterns: array[0..112] of Word = (
    $155, $0ab, $0ad, $0b5, $0d5, $156, $15a, $16a, $1aa, $0ae,
    $0b6, $0ba, $0d6, $0da, $0ea, $12b, $12d, $135, $14b, $14d,
    $153, $159, $165, $169, $195, $1a5, $1a9, $057, $05b, $05d,
    $06b, $06d, $075, $097, $09b, $09d, $0a7, $0b3, $0b9, $0cb,
    $0cd, $0d3, $0d9, $0e5, $0e9, $12e, $136, $13a, $14e, $15c,
    $166, $16c, $172, $174, $196, $19a, $1a6, $1ac, $1b2, $1b4,
    $1ca, $1d2, $1d4, $05e, $06e, $076, $07a, $09e, $0bc, $0ce,
    $0dc, $0e6, $0ec, $0f2, $0f4, $117, $11b, $11d, $127, $133,
    $139, $147, $163, $171, $18b, $18d, $193, $199, $1a3, $1b1,
    $1c5, $1c9, $1d1, $02f, $037, $03b, $03d, $04f, $067, $073,
    $079, $08f, $0c7, $0e3, $0f1, $11e, $13c, $178, $18e, $19c,
    $1b8, $1c6, $1cc
    );

  GF = 113;
  PM = 3;

function get_dot(Dots: TArrayOfChar; Hgt, Wid, x, y: NativeInt): Boolean;
begin
  Result := False;

  if (x >= 0) and (x < Wid) and (y >= 0) and (y < Hgt) then
    Result := Dots[(y * Wid) + x] = '1';
end;

function clr_col(var Dots: TArrayOfChar; Hgt, Wid, x: NativeInt): Boolean;
var
  y: NativeInt;
begin
  y := x and 1;

  while y < Hgt do begin
    if get_dot(Dots, Hgt, Wid, x, y) then
      Exit(False);
    Inc(y, 2);
  end;

  Result := True;
end;

function clr_row(var Dots: TArrayOfChar; Hgt, Wid, y: NativeInt): Boolean;
var
  x: NativeInt;
begin
  x := y and 1;

  while x < Wid do begin
    if get_dot(Dots, Hgt, Wid, x, y) then
      Exit(False);
    Inc(x, 2);
  end;

  Result := True;
end;


{* Dot pattern scoring routine from Annex A *}
function score_array(Dots: TArrayOfChar; Hgt, Wid: NativeInt): NativeInt;
var
  x, y, worstedge, first, last, sum: NativeInt;
  penalty_local: NativeInt;
  penalty: NativeInt;
begin
  penalty_local := 0;
  penalty := 0;

    // first, guard against "pathelogical" gaps in the array
    if (Hgt and 1) > 0 then begin
        if (Hgt < 12) then begin
            sum := 0;
            for x := 1 to Wid - 2 {x < Wid - 1} do begin
                if not clr_col(Dots, Hgt, Wid, x) then begin
                    sum := 0;
                    if penalty_local <> 0 then begin
                        Inc(penalty, penalty_local);
                        penalty_local := 0;
                    end;
                end
                else begin
                    Inc(sum);
                    if (sum = 1) then
                        penalty_local := Hgt
                    else
                        penalty_local := penalty_local * Hgt;
                end;
            end;
        end;
    end
    else begin
        if (Wid < 12) then begin
            sum := 0;
            for y := 1 to Hgt - 2 {y < Hgt - 1} do begin
                if not clr_row(Dots, Hgt, Wid, y) then begin
                    sum := 0;
                    if penalty_local <> 0 then begin
                        Inc(penalty, penalty_local);
                        penalty_local := 0;
                    end;
                end
                else begin
                    Inc(sum);
                    if (sum = 1) then
                        penalty_local := Wid
                    else
                        penalty_local := penalty_local * Wid;
                end;
            end;
        end;
    end;

    sum := 0;
    first := -1;
    last := -1;

    // across the top edge, count printed dots and measure their extent
    x := 0;
    While x < Wid do begin
        if get_dot(Dots, Hgt, Wid, x, 0) then begin
            if first < 0 then
              first := x;

            last := x;
            inc(Sum);
        end;
        Inc(x, 2);
    end;
    worstedge := sum + last - first;
    worstedge := worstedge * Hgt;

    sum := 0;
    first := -1;
    last := -1;

    //across the bottom edge, ditto
    x := Wid and 1;
    while x < Wid do begin
        if get_dot(Dots, Hgt, Wid, x, Hgt - 1) then begin
            if first < 0 then
                first := x;
            last := x;
            Inc(sum);
        end;
        Inc(x, 2);
    end;
    Inc(sum, last - first);
    sum := sum * Hgt;
    if (sum < worstedge) then
        worstedge := sum;

    sum := 0;
    first := -1;
    last := -1;

    //down the left edge, ditto
    y := 0;
    while y < Hgt do begin
        if get_dot(Dots, Hgt, Wid, 0, y) then begin
            if first < 0 then
                first := y;

            last := y;
            Inc(sum);
        end;
        Inc(y, 2);
    end;
    Inc(sum, last - first);
    sum := sum * Wid;
    if (sum < worstedge) then
        worstedge := sum;

    sum := 0;
    first := -1;
    last := -1;

    //down the right edge, ditto
    y := Hgt and 1;
    While y < Hgt do begin
        if get_dot(Dots, Hgt, Wid, Wid - 1, y) then begin
            if first < 0 then
                first := y;

            last := y;
            Inc(sum);
        end;
        Inc(y, 2);
    end;
    Inc(sum, last - first);
    sum := sum * Wid;
    if (sum < worstedge) then
        worstedge := sum;

    // throughout the array, count the # of unprinted 5-somes (cross patterns)
    // plus the # of printed dots surrounded by 8 unprinted neighbors
    sum := 0;
    for y := 0 to Hgt - 1 do begin
        x := y and 1;
        While x < Wid do begin
            if (not get_dot(Dots, Hgt, Wid, x - 1, y - 1)
                    and not get_dot(Dots, Hgt, Wid, x + 1, y - 1)
                    and not get_dot(Dots, Hgt, Wid, x - 1, y + 1)
                    and not get_dot(Dots, Hgt, Wid, x + 1, y + 1)
                    and (not get_dot(Dots, Hgt, Wid, x, y)
                    or (not get_dot(Dots, Hgt, Wid, x - 2, y)
                    and not get_dot(Dots, Hgt, Wid, x, y - 2)
                    and not get_dot(Dots, Hgt, Wid, x + 2, y)
                    and not get_dot(Dots, Hgt, Wid, x, y + 2)))) then
                Inc(sum);
          Inc(x, 2);
        end;
    end;

    Result := worstedge - sum * sum - penalty;
end;


//-------------------------------------------------------------------------
// "rsencode(nd,nc)" adds "nc" R-S check words to "nd" data words in wd[]
// employing Galois Field GF, where GF is prime, with a prime modulus of PM
//-------------------------------------------------------------------------

procedure rsencode(nd, nc: integer; var wd: TArrayOfByte);
var
  i, j, k, nw, start, step: Integer;
  root: array [0..GF] of Integer;
  c: array [0..GF] of Integer;
begin

    // Start by generating "nc" roots (antilogs):
    root[0] := 1;
    i := 1;
    while (i <= nc) and (i < GF) do begin
      root[i] := (PM * root[i - 1]) Mod GF;
      Inc(i);
    end;

    // Here we compute how many interleaved R-S blocks will be needed
    nw := nd + nc;
    step := (nw + GF - 2) div (GF - 1);

    // ...& then for each such block:
    for start := 0 to step - 1 do begin
        ND := (nd - start + step - 1) div step;
        NW := (nw - start + step - 1) div step;
        NC := NW - ND;

        // first compute the generator polynomial "c" of order "NC":
        for i := 1 to NC do
            c[i] := 0;
        c[0] := 1;

        for i := 1 to NC do
            for j := NC downto 1 do
                c[j] := (GF + c[j] - (root[i] * c[j - 1]) mod GF) mod GF;

        // & then compute the corresponding checkword values into wd[]
        // ... (a) starting at wd[start] & (b) stepping by step
        for i := ND to NW - 1 do
            wd[start + i * step] := 0;
        for i := 0 to ND - 1 do begin
            k := (wd[start + i * step] + wd[start + ND * step]) mod GF;
            for j := 0 to NC - 2 {j < NC - 1} do
                wd[start + (ND + j) * step] := (GF - ((c[j + 1] * k) Mod GF) + wd[start + (ND + j + 1) * step]) mod GF;
            wd[start + (ND + NC - 1) * step] := (GF - ((c[NC] * k) mod GF)) mod GF;
        end;
        for i := ND to NW - 1 do
            wd[start + i * step] := (GF - wd[start + i * step]) mod GF;
    end;
end;

{* Check if the next character is directly encodable in code set A (Annex F.II.D) *}
function datum_a(const Source: TArrayOfByte; position, _length: integer): Boolean;
begin
  Result := False;

  if (position < _length) then
      if (source[position] <= 95) then
          Result := true;
end;

{* Check if the next character is directly encodable in code set B (Annex F.II.D) *}
function datum_b(const Source: TArrayOfByte; position, _length: integer): Boolean;
begin
  Result := False;
  if (position < _length) then begin
    if ((source[position] >= 32) and (source[position] <= 127)) then
      Result := True;

    if source[Position] in [9{HT}, 28{FS}, 29{GS}, 30{RS}] then
      Result := True;

    if (position <> _length - 2) then
      if ((source[position] = 13) and (source[position + 1] = 10)) then  // CRLF
        Result := True;
  end;
end;

{* Check if the next characters are directly encodable in code set C (Annex F.II.D) *}
function datum_c(const Source: TArrayOfByte; position, _length: integer): Boolean;
begin
  Result := False;
  if (position <= _length - 2) then
        if (((source[position] >= 48 {'0'}) and (source[position] <= 57 {'9'}))
                and ((source[position + 1] >= 48 {'0'}) and (source[position + 1] <= 57 {'9'}))) then
            Result := True;
end;

{* Returns how many consecutive digits lie immediately ahead (Annex F.II.A) *}
function n_digits(const Source: TArrayOfByte; position, length: integer): Integer;
var
  i: Integer;
begin
  i := position;
  While ((source[i] >= 48 {'0'}) and (source[i] <= 57 {'9'})) and (i < length) do
    inc(i);

  result := i - position;
end;

{* checks ahead for 10 or more digits starting "17xxxxxx10..." (Annex F.II.B) *}
function seventeen_ten(const Source: TArrayOfByte; position, _length: integer): Boolean;
begin
  Result := False;

  if (n_digits(source, position, _length) >= 10) then
    if (((source[position] = 49 {'1'}) and (source[position + 1] = 55{'7'}))
            and ((source[position + 8] = 49 {'1'}) and (source[position + 9] = 48 {'0'}))) then
      Result := True;
end;

{*  checks how many characters ahead can be reached while datum_c is true,
 *  returning the resulting number of codewords (Annex F.II.E)
 *}
function ahead_c(const Source: TArrayOfByte; position, length: integer): Integer;
var
  i: integer;
begin
  Result := 0;
  i := position;

  while (i < length) and datum_c(source, i, length) do begin
    inc(Result);
    inc(i, 2);
  end;
end;

{* Annex F.II.F *}
function try_c(const Source: TArrayOfByte; position, _length: integer): Integer;
begin
  Result := 0;

  if (n_digits(source, position, _length) > 0) then
        if (ahead_c(source, position, _length) > ahead_c(source, position + 1, _length)) then
            Result := ahead_c(source, position, _length);
end;

{* Annex F.II.G *}
function ahead_a(const Source: TArrayOfByte; position, _length: integer): integer;
var
  i: integer;
begin
  Result := 0;

  i := Position;
  while ((i < _length) and datum_a(source, i, _length)) and (try_c(source, i, _length) < 2) do begin
    Inc(i);
    Inc(Result);
  end;
end;

{* Annex F.II.H *}
function ahead_b(const Source: TArrayOfByte; position, _length: integer): Integer;
var
  i: Integer;
begin
  Result := 0;

  i := Position;
  while ((i < _length) and datum_b(source, i, _length)) and (try_c(source, i, _length) < 2) do begin
    Inc(i);
    Inc(Result);
  end;
end;

{* checks if the next character is in the range 128 to 255  (Annex F.II.I) *}
function binary(const Source: TArrayOfByte; position: integer): Boolean;
begin
  Result := (source[position] >= 128);
end;

{* Analyse input data stream and encode using algorithm from Annex F *}
function dotcode_encode_message(symbol : zint_symbol; const Source: TArrayOfByte; _length: integer; var codeword_array: TArrayOfByte;
                                var binary_finish: NativeInt): Integer;
var
  input_position, array_length, i: integer;
  encoding_mode: char;
  inside_macro: integer;

  debug: Boolean;
  binary_buffer_size: integer;
  lawrencium: array [0..6] of integer; // Reversed radix 103 values

//#if defined(_MSC_VER) /*&& _MSC_VER == 1200*/
//    uint64_t binary_buffer = 0;
//#else
//    uint64_t binary_buffer = 0ULL;
//#endif
  binary_buffer : UInt64;

  // Working vars
  a, b, c, m, n: integer;
  done: boolean;
begin
    input_position := 0;
    array_length := 0;
    encoding_mode := 'C';
    inside_macro := 0;
    debug := symbol.Debug;
    binary_buffer := 0;
    binary_buffer_size := 0;


    if (symbol.output_options and READER_INIT) <> 0 then begin
        codeword_array[array_length] := 109; // FNC3
        inc(array_length);
    end;

    if symbol.input_mode <> GS1_MODE then
        if (_length > 2) then
            if (((source[input_position] >= 48 {'0'}) and (source[input_position] <= 57 {'9'})) and
                    ((source[input_position + 1] >= 48 {'0'}) and (source[input_position + 1] <= 57 {'9'}))) then begin
                codeword_array[array_length] := 107; // FNC1
                Inc(array_length);
            end;

    if (symbol.eci > 3) then begin
        codeword_array[array_length] := 108; // FNC2
        Inc(array_length);
        if (symbol.eci <= 39) then begin
            codeword_array[array_length] := symbol.eci;
            Inc(array_length);
        end
        else begin
            // the next three codewords valued A, B & C encode the ECI value of
            // (A - 40) * 12769 + B * 113 + C + 40 (Section 5.2.1)
            a := (symbol.eci - 40) mod 12769;
            b := ((symbol.eci - 40) - (12769 * a)) mod 113;
            c := (symbol.eci - 40) - (12769 * a) - (113 * b);

            codeword_array[array_length] := a + 40;
            Inc(array_length);
            codeword_array[array_length] := b;
            Inc(array_length);
            codeword_array[array_length] := c;
            Inc(array_length);
        end;
    end;

    // Prevent encodation as a macro if a special character is in first position
    if (source[input_position] = 9) then begin
        codeword_array[array_length] := 101; // Latch A
        inc(array_length);
        codeword_array[array_length] := 73; // HT
        inc(array_length);
        encoding_mode := 'A';
    end;

    if (source[input_position] = 28) then begin
        codeword_array[array_length] := 101; // Latch A
        inc(array_length);
        codeword_array[array_length] := 92; // FS
        inc(array_length);
        encoding_mode := 'A';
    end;

    if (source[input_position] = 29) then begin
        codeword_array[array_length] := 101; // Latch A
        inc(array_length);
        codeword_array[array_length] := 93; // GS
        inc(array_length);
        encoding_mode := 'A';
    end;

    if (source[input_position] = 30) then begin
        codeword_array[array_length] := 101; // Latch A
        inc(array_length);
        codeword_array[array_length] := 94; // RS
        inc(array_length);
        encoding_mode := 'A';
    end;

    repeat
        done := False;
        {* Step A *}
        if ((input_position = _length - 2) and (inside_macro <> 0) and (inside_macro <> 100)) then begin
            // inside_macro only gets set to 97, 98 or 99 if the last two characters are RS/EOT
            inc(input_position, 2);
            done := true;
            if debug then
                OutputDebugString('A ');
        end;

        if ((input_position = _length - 1) and (inside_macro = 100)) then begin
            // inside_macro only gets set to 100 if the last character is EOT
            inc(input_position);
            done := true;
            if debug then
                OutputDebugString('A ');
        end;

        {* Step B1 *}
        if ((not done) and (encoding_mode = 'C')) then begin
            if ((array_length = 0) and (_length > 9)) then begin
                if ((source[input_position] = Ord('['))
                        and (source[input_position + 1] = Byte(')'))
                        and (source[input_position + 2] = Byte('>'))
                        and (source[input_position + 3] = 30) // RS
                        and (source[_length - 1] = 4)) {EOT} then begin


                    if ((source[input_position + 6] = 29) and (source[_length - 2] = 30)) { GS/RS } then begin
                        if ((source[input_position + 4] = 48 {'0'}) and (source[input_position + 5] = Byte('5'))) then begin
                            codeword_array[array_length] := 102; // Shift B
                            inc(array_length);
                            codeword_array[array_length] := 97; // Macro
                            Inc(array_length);
                            Inc(input_position, 7);
                            inside_macro := 97;
                            done := True;
                            if debug then
                                OutputDebugString('B1/1 ');
                        end;

                        if ((source[input_position + 4] = Byte('0')) and (source[input_position + 5] = Byte('6'))) then begin
                            codeword_array[array_length] := 102; // Shift B
                            inc(array_length);
                            codeword_array[array_length] := 98; // Macro
                            inc(array_length);
                            Inc(input_position, 7);
                            inside_macro := 98;
                            done := True;
                            if debug then
                                OutputDebugString('B1/2 ');
                        end;

                        if ((source[input_position + 4] = Byte('1')) and (source[input_position + 5] = Byte('2'))) then begin
                            codeword_array[array_length] := 102; // Shift B
                            inc(array_length);
                            codeword_array[array_length] := 99; // Macro
                            inc(array_length);
                            Inc(input_position, 7);
                            inside_macro := 99;
                            done := True;
                            if debug then
                                OutputDebugString('B1/3 ');
                        end;
                    end;

                    if ((not done) and (source[input_position] >= Byte('0')) and (source[input_position] <= Byte('9')) and
                            (source[input_position + 1] >= Byte('0')) and (source[input_position + 1] <= Byte('9'))) then begin
                        codeword_array[array_length] := 102; // Shift B
                        inc(array_length);
                        codeword_array[array_length] := 100; // Macro
                        inc(array_length);
                        inc(input_position, 4);
                        inside_macro := 100;
                        done := True;
                        if debug then
                            OutputDebugString('B1/4 ');
                    end;
                end;
            end;
        end;

        {* Step B2 *}
        if ((not done) and (encoding_mode = 'C')) then begin
            if (seventeen_ten(source, input_position, _length)) then begin
                codeword_array[array_length] := 100; // (17)...(10)
                Inc(array_length);
                codeword_array[array_length] := ((source[input_position + 2] - 48 {'0'}) * 10) + (source[input_position + 3] - 48 {'0'});
                Inc(array_length);
                codeword_array[array_length] := ((source[input_position + 4] - 48 {'0'}) * 10) + (source[input_position + 5] - 48 {'0'});
                Inc(array_length);
                codeword_array[array_length] := ((source[input_position + 6] - 48 {'0'}) * 10) + (source[input_position + 7] - 48 {'0'});
                Inc(array_length);
                Inc(input_position, 10);
                done := True;
                if debug then
                    OutputDebugString('B2/1 ');
            end;
        end;

        if ((not done) and (encoding_mode = 'C')) then begin
            if (datum_c(source, input_position, _length) or ((source[input_position] = Byte('[')) and (symbol.input_mode = GS1_MODE))) then begin
                if (source[input_position] = Byte('[')) then begin
                    codeword_array[array_length] := 107; // FNC1
                    Inc(input_position);
                end
                else begin
                    codeword_array[array_length] := ((source[input_position] - 48 {'0'}) * 10) + (source[input_position + 1] - 48 {'0'});
                    Inc(input_position, 2);
                end;
                inc(array_length);
                done := True;
                if debug then
                    OutputDebugString('B2/2 ');
            end;
        end;

        {* Setp B3 *}
        if ((not done) and (encoding_mode = 'C')) then begin
            if (binary(source, input_position)) then begin
                if (n_digits(source, input_position + 1, _length) > 0) then begin
                    if ((source[input_position] - 128) < 32) then begin
                        codeword_array[array_length] := 110; // Bin Shift A
                        inc(array_length);
                        codeword_array[array_length] := source[input_position] - 128 + 64;
                        Inc(array_length);
                    end
                    else begin
                        codeword_array[array_length] := 111; // Bin Shift B
                        Inc(array_length);
                        codeword_array[array_length] := source[input_position] - 128 - 32;
                        Inc(array_length);
                    end;
                    inc(input_position);
                end
                else begin
                    codeword_array[array_length] := 112; // Bin Latch
                    Inc(array_length);
                    encoding_mode := 'X';
                end;
                done := true;
                if debug then
                    OutputDebugString('B3 ');
            end;
        end;

        {* Step B4 *}
        if ((not done) and (encoding_mode = 'C')) then begin
            m := ahead_a(source, input_position, _length);
            n := ahead_b(source, input_position, _length);
            if (m > n) then begin
                codeword_array[array_length] := 101; // Latch A
                Inc(array_length);
                encoding_mode := 'A';
            end
            else begin
                if (n <= 4) then begin
                    codeword_array[array_length] := 101 + n; // nx Shift B
                    Inc(array_length);

                    for i := 0 to n - 1 do begin
                        codeword_array[array_length] := source[input_position] - 32;
                        Inc(array_length);
                        Inc(input_position);
                    end;
                end
                else begin
                    codeword_array[array_length] := 106; // Latch B
                    inc(array_length);
                    encoding_mode := 'B';
                end;
            end;
            done := True;
            if debug then
                OutputDebugString('B4 ');
        end;

        {* Step C1 *}
        if ((not done) and (encoding_mode = 'B')) then begin
            n := try_c(source, input_position, _length);

            if (n >= 2) then begin
                if (n <= 4) then begin
                    codeword_array[array_length] := 103 + (n - 2); // nx Shift C
                    Inc(array_length);
                    for i := 0 to n-1 do begin
                        codeword_array[array_length] := ((source[input_position] - Byte('0')) * 10) + (source[input_position + 1] - Byte('0'));
                        Inc(array_length);
                        Inc(input_position, 2);
                    end;
                end
                else begin
                    codeword_array[array_length] := 106; // Latch C
                    Inc(array_length);
                    encoding_mode := 'C';
                end;
                done := True;
                if debug then
                    OutputDebugString('C1 ');
            end;
        end;

        {* Step C2 *}
        if ((not done) and (encoding_mode = 'B')) then begin
            if ((source[input_position] = Byte('[')) and (symbol.input_mode = GS1_MODE)) then begin
                codeword_array[array_length] := 107; // FNC1
                inc(array_length);
                Inc(input_position);
                done := True;
                if debug then
                    OutputDebugString('C2/1 ');
            end
            else begin
                if (datum_b(source, input_position, _length)) then begin

                    if ((source[input_position] >= 32) and (source[input_position] <= 127)) then begin
                        codeword_array[array_length] := source[input_position] - 32;
                        done := True;
                    end
                    else if (source[input_position] = 13) then begin
                        {* CR/LF *}
                        codeword_array[array_length] := 96;
                        Inc(input_position);
                        done := True;

                    end
                    else if (input_position <> 0) then begin
                        {* HT, FS, GS and RS in the first data position would be interpreted as a macro (see table 2) *}
                        case (source[input_position]) of
                            9: // HT
                                codeword_array[array_length] := 97;
                            28: // FS
                                codeword_array[array_length] := 98;
                            29: // GS
                                codeword_array[array_length] := 99;
                            30: // RS
                                codeword_array[array_length] := 100;
                        end;
                        done := True;
                    end;

                    if done then begin
                        Inc(array_length);
                        Inc(input_position);
                        if debug then
                          OutputDebugString('C2/2 ');
                    end;
                end;
            end;
        end;

        {* Step C3 *}
        if ((not done) and (encoding_mode = 'B')) then begin
            if (binary(source, input_position)) then begin
                if (datum_b(source, input_position + 1, _length)) then begin
                    if ((source[input_position] - 128) < 32) then begin
                        codeword_array[array_length] := 110; // Bin Shift A
                        Inc(array_length);
                        codeword_array[array_length] := source[input_position] - 128 + 64;
                        Inc(array_length);
                    end
                    else begin
                        codeword_array[array_length] := 111; // Bin Shift B
                        Inc(array_length);
                        codeword_array[array_length] := source[input_position] - 128 - 32;
                        Inc(array_length);
                    end;
                    Inc(input_position);
                end
                else begin
                    codeword_array[array_length] := 112; // Bin Latch
                    Inc(array_length);
                    encoding_mode := 'X';
                end;
                done := True;
                 if debug then
                    OutputDebugString('C3 ');
            end;
        end;

        {* Step C4 *}
        if ((not done) and (encoding_mode = 'B')) then begin
            if (ahead_a(source, input_position, _length) = 1) then begin
                codeword_array[array_length] := 101; // Shift A
                inc(array_length);
                if (source[input_position] < 32) then
                    codeword_array[array_length] := source[input_position] + 64
                else
                    codeword_array[array_length] := source[input_position] - 32;
                Inc(array_length);
                Inc(input_position);
            end
            else begin
                codeword_array[array_length] := 102; // Latch A
                Inc(array_length);
                encoding_mode := 'A';
            end;
            done := True;
             if debug then
                OutputDebugString('C4 ');
        end;

        {* Step D1 *}
        if ((not done) and (encoding_mode = 'A')) then begin
            n := try_c(source, input_position, _length);
            if (n >= 2) then begin
                if (n <= 4) then begin
                    codeword_array[array_length] := 103 + (n - 2); // nx Shift C
                    Inc(array_length);
                    for i := 0 to n - 1 do begin
                        codeword_array[array_length] := ((source[input_position] - Byte('0')) * 10) + (source[input_position + 1] - Byte('0'));
                        Inc(array_length);
                        inc(input_position, 2);
                    end;
                end
                else begin
                    codeword_array[array_length] := 106; // Latch C
                    inc(array_length);
                    encoding_mode := 'C';
                end;
                done := True;
                 if debug then
                    OutputDebugString('D1 ');
            end;
        end;

        {* Step D2 *}
        if ((not done) and (encoding_mode = 'A')) then begin
            if ((source[input_position] = Byte('[')) and (symbol.input_mode = GS1_MODE)) then begin
                codeword_array[array_length] := 107; // FNC1
                Inc(array_length);
                Inc(input_position);
                done := True;
                 if debug then
                    OutputDebugString('D2/1 ');
            end
            else begin
                if (datum_a(source, input_position, _length)) then begin
                    if (source[input_position] < 32) then
                        codeword_array[array_length] := source[input_position] + 64
                    else
                        codeword_array[array_length] := source[input_position] - 32;
                    inc(array_length);
                    inc(input_position);
                    done := True;
                   if debug then
                      OutputDebugString('D2/2 ');
                end;
            end;
        end;

        {* Step D3 *}
        if ((not done) and (encoding_mode = 'A')) then begin
            if (binary(source, input_position)) then begin
                if (datum_a(source, input_position + 1, _length)) then begin
                    if ((source[input_position] - 128) < 32) then begin
                        codeword_array[array_length] := 110; // Bin Shift A
                        Inc(array_length);
                        codeword_array[array_length] := source[input_position] - 128 + 64;
                        Inc(array_length);
                    end
                    else begin
                        codeword_array[array_length] := 111; // Bin Shift B
                        Inc(array_length);
                        codeword_array[array_length] := source[input_position] - 128 - 32;
                        Inc(array_length);
                    end;
                    Inc(input_position);
                end
                else begin
                    codeword_array[array_length] := 112; // Bin Latch
                    inc(array_length);
                    encoding_mode := 'X';
                end;
                done := True;
                 if debug then
                    OutputDebugString('D3 ');
            end;
        end;

        {* Step D4 *}
        if ((not done) and (encoding_mode = 'A')) then begin
            n := ahead_b(source, input_position, _length);

            if (n <= 6) then begin
                codeword_array[array_length] := 95 + n; // nx Shift B
                inc(array_length);
                for i := 0 to n - 1 do begin
                    codeword_array[array_length] := source[input_position] - 32;
                    inc(array_length);
                    inc(input_position);
                end;
            end
            else begin
                codeword_array[array_length] := 102; // Latch B
                inc(array_length);
                encoding_mode := 'B';
            end;
            done := True;
             if debug then
                OutputDebugString('D4 ');
        end;

        {* Step E1 *}
        if ((not done) and (encoding_mode = 'X')) then begin
            n := try_c(source, input_position, _length);

            if (n >= 2) then begin
                {* Empty binary buffer *}
                for i := 0 to binary_buffer_size{binary_buffer_size + 1} do begin
                    lawrencium[i] := binary_buffer mod 103;
                    binary_buffer := binary_buffer div 103;
                end;

                for i := 0 to binary_buffer_size {binary_buffer_size + 1} do begin
                    codeword_array[array_length] := lawrencium[binary_buffer_size - i];
                    Inc(array_length);
                end;
                binary_buffer := 0;
                binary_buffer_size := 0;

                if (n <= 7) then begin
                    codeword_array[array_length] := 101 + n; // Interrupt for nx Shift C
                    inc(array_length);
                    for i := 0 to n - 1 do begin
                        codeword_array[array_length] := ((source[input_position] - Byte('0')) * 10) + (source[input_position + 1] - Byte('0'));
                        inc(array_length);
                        Inc(input_position, 2);
                    end;
                end
                else begin
                    codeword_array[array_length] := 111; // Terminate with Latch to C
                    Inc(array_length);
                    encoding_mode := 'C';
                end;
                done := True;
               if debug then
                  OutputDebugString('E1 ');
            end;
        end;

        {* Step E2 *}
        {* Section 5.2.1.1 para D.2.i states:
         * "Groups of six codewords, each valued between 0 and 102, are radix converted from
         * base 103 into five base 259 values..."
         *}
        if ((not done) and (encoding_mode = 'X')) then begin
            if (binary(source, input_position)
                    or binary(source, input_position + 1)
                    or binary(source, input_position + 2)
                    or binary(source, input_position + 3)) then begin
                binary_buffer := binary_buffer * 259;
                binary_buffer := binary_buffer + source[input_position];
                Inc(binary_buffer_size);

                if (binary_buffer_size = 5) then begin
                    for i := 0 to 5 do begin
                        lawrencium[i] := binary_buffer mod 103;
                        binary_buffer := binary_buffer div 103;
                    end;

                    for i := 0 to 5 do begin
                        codeword_array[array_length] := lawrencium[5 - i];
                        Inc(array_length);
                    end;
                    binary_buffer := 0;
                    binary_buffer_size := 0;
                end;
                Inc(input_position);
                done := True;
               if debug then
                  OutputDebugString('E2 ');
            end;
        end;

        {* Step E3 *}
        if ((not done) and (encoding_mode = 'X')) then begin
            {* Empty binary buffer *}
            for i := 0 to binary_buffer_size {binary_buffer_size + 1} do begin
                lawrencium[i] := binary_buffer mod 103;
                binary_buffer := binary_buffer div 103;
            end;

            for i := 0 to binary_buffer_size {binary_buffer_size + 1} do begin
                codeword_array[array_length] := lawrencium[binary_buffer_size - i];
                Inc(array_length);
            end;
            binary_buffer := 0;
            binary_buffer_size := 0;

            if (ahead_a(source, input_position, _length) > ahead_b(source, input_position, _length)) then begin
                codeword_array[array_length] := 109; // Terminate with Latch to A
                encoding_mode := 'A';
            end
            else begin
                codeword_array[array_length] := 110; // Terminate with Latch to B
                encoding_mode := 'B';
            end;
            Inc(array_length);
//            done := True;
             if debug then
                OutputDebugString('E3 ');
        end;
    until (input_position >= _length);          { while (input_position < length) }

    if (encoding_mode = 'X') then begin
        if (binary_buffer_size <> 0) then begin
            {* Empty binary buffer *}
            for i := 0 to binary_buffer_size {binary_buffer_size + 1} do begin
                lawrencium[i] := binary_buffer mod 103;
                binary_buffer := binary_buffer div 103;
            end;

            for i := 0 to binary_buffer_size {binary_buffer_size + 1} do begin
                codeword_array[array_length] := lawrencium[binary_buffer_size - i];
                Inc(array_length);
            end;
        end;
        binary_finish := 1;
    end;

     if debug then
        OutputDebugString('\n\n ');

    Result := array_length;
end;

{* Convert codewords to binary data stream *}
function make_dotstream(const masked_array: TArrayOfByte; array_length: integer; dot_stream: TArrayOfChar): Cardinal;
var
  i: NativeInt;
begin
//  strcpy(dot_stream, 0);
  dot_stream[0] := #0;

  {* Mask value is encoded as two dots *}
  bin_append(masked_array[0], 2, dot_stream);
//  bscan(dot_stream, masked_array[0], 2);

  {* The rest of the data uses 9-bit dot patterns from Annex C *}
  for i := 1 to array_length - 1 do
    bin_append(dot_patterns[masked_array[i]], 9, dot_stream);
//    bscan(dot_stream, dot_patterns[masked_array[i]], 9);

  Result := strlen(dot_stream);
end;


{* Determines if a given dot is a reserved corner dot
 * to be used by one of the last six bits
 *}
function is_corner(column, row, width, height: NativeInt): Boolean;
begin
  Result := False;

  {* Top Left *}
  if ((column = 0) and (row = 0)) then
      Result := True;

  {* Top Right *}
  if (height mod 2) <> 0 then begin
      if (((column = width - 2) and (row = 0))
              or ((column = width - 1) and (row = 1))) then
          Result := True;
  end
  else if ((column = width - 1) and (row = 0)) then
          Result := True;

  {* Bottom Left *}
  if (height mod 2) <> 0 then begin
      if ((column = 0) and (row = height - 1)) then
          Result := True;
  end
  else if (((column = 0) and (row = height - 2))
              or ((column = 1) and (row = height - 1))) then
          Result := True;

  {* Bottom Right *}
  if (((column = width - 2) and (row = height - 1))
          or ((column = width - 1) and (row = height - 2))) then
          Result := True;
end;

{* Place the dots in the symbol*}
procedure fold_dotstream(dot_stream: TArrayOfChar; width, height: NativeInt; dot_array: TArrayOfChar);
var
  column, row: NativeInt;
  input_position: NativeInt;
begin
  input_position := 0;

  if (height mod 2) <> 0 then begin
      {* Horizontal folding *}
      for row := 0 to height - 1 do begin
          for column := 0 to width - 1 do begin
              if (column + row) mod 2 = 0 then begin
                  if (is_corner(column, row, width, height)) then
                      dot_array[(row * width) + column] := 'C'
                  else begin
                      dot_array[((height - row - 1) * width) + column] := dot_stream[input_position];
                      Inc(input_position);
                  end;
              end
              else
                  dot_array[((height - row - 1) * width) + column] := #32 {' '}; // Non-data position
          end;
      end;

      {* Corners *}
      dot_array[width - 2] := dot_stream[input_position];
      Inc(input_position);
      dot_array[(height * width) - 2] := dot_stream[input_position];
      Inc(input_position);
      dot_array[(width * 2) - 1] := dot_stream[input_position];
      Inc(input_position);
      dot_array[((height - 1) * width) - 1] := dot_stream[input_position];
      Inc(input_position);
      dot_array[0] := dot_stream[input_position];
      Inc(input_position);
      dot_array[(height - 1) * width] := dot_stream[input_position];
  end
  else begin
      {* Vertical folding *}
      for column := 0 to width - 1 do begin
          for row := 0 to height - 1 do begin
              if (column + row) mod 2 = 0 then begin
                  if (is_corner(column, row, width, height)) then
                      dot_array[(row * width) + column] := 'C'
                  else begin
                      dot_array[(row * width) + column] := dot_stream[input_position];
                      Inc(input_position);
                  end;
              end
              else
                  dot_array[(row * width) + column] := #32 {' '}; // Non-data position
          end;
      end;

      {* Corners *}
      dot_array[((height - 1) * width) - 1] := dot_stream[input_position];
      Inc(input_position);
      dot_array[(height - 2) * width] := dot_stream[input_position];
      Inc(input_position);
      dot_array[(height * width) - 2] := dot_stream[input_position];
      Inc(input_position);
      dot_array[((height - 1) * width) + 1] := dot_stream[input_position];
      Inc(input_position);
      dot_array[width - 1] := dot_stream[input_position];
      Inc(input_position);
      dot_array[0] := dot_stream[input_position];
  end;
end;


function dotCode(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  i, j, k: NativeInt;
  jc, n_dots: Cardinal {size_t};
  data_length, ecc_length: NativeInt;
  min_dots: Integer;
  min_area: NativeInt;
  height, width: NativeInt;
  mask_score: array[0..3] of NativeInt;
  weight: NativeInt;
  dot_stream_length: Cardinal {size_t};
  high_score, best_mask: NativeInt;

  binary_finish: NativeInt;
  debug: Boolean;

  padding_dots: Integer;
  is_first: NativeInt;

  masked_codeword_array: TArrayOfByte;
  codeword_array: TArrayOfByte;
//#ifdef _MSC_VER
//    unsigned char* masked_codeword_array;
//#endif
//
//#ifndef _MSC_VER
//    unsigned char codeword_array[length * 3];
//#else
//    char* dot_stream;
//    char* dot_array;
//    unsigned char* codeword_array = (unsigned char *) _alloca(length * 3 * sizeof (unsigned char));
//#endif /* _MSC_VER */

  dot_stream: TArrayOfChar;
  dot_array: TArrayOfChar;

  // local var
  h, w: double;
begin
  binary_finish := 0;
  debug := symbol.Debug;

  SetLength(codeword_array, _length * 3);

    if (symbol.eci > 811799) then begin
        strcpy(symbol.errtxt, '525: Invalid ECI');
        Exit(ZERROR_INVALID_OPTION);
    end;

    data_length := dotcode_encode_message(symbol, source, _length, codeword_array, binary_finish);

    ecc_length := 3 + (data_length div 2);

    if (debug) then
      writeln(Format('Codeword length = %d, ECC length = %d\n', [data_length, ecc_length]));

    min_dots := 9 * (data_length + 3 + (data_length div 2)) + 2;
    min_area := min_dots * 2;

    if (symbol.option_2 = 0) then begin
        {* Automatic sizing *}
        {* Following Rule 3 (Section 5.2.2) and applying a recommended width to height ratio 3:2 *}
        {* Eliminates under sized symbols *}

        h := sqrt(min_area * 0.666);
        w := sqrt(min_area * 1.5);

        height := Trunc(h);
        width := Trunc(w);

        if ((width + height) mod 2 = 1) then begin
            if ((width * height) < min_area) then begin
                Inc(width);
                Inc(height);
            end
        end
        else begin
            if (h * width) < (w * height) then begin
                Inc(width);
                if (width * height) < min_area then begin
                    Dec(width);
                    Inc(height);
                    if (width * height) < min_area then
                        Inc(width, 2);
                end;
            end
            else begin
                Inc(height);
                if (width * height) < min_area then begin
                    Inc(width);
                    Dec(height);
                    if (width * height) < min_area then
                        Inc(height, 2);
                end;
            end;
        end;

    end
    else begin
        {* User defined width *}
        {* Eliminates under sized symbols *}

        width := symbol.option_2;
        height := (min_area + (width - 1)) div width;

        if ((width + height) mod 2) = 0 then
            Inc(height);
    end;

    if ((height > 200) or (width > 200)) then begin
        strcpy(symbol.errtxt, '526: Specified symbol size is too large');
        Exit(ZERROR_INVALID_OPTION);
    end;

    if ((height < 5) or (width < 5)) then begin
        strcpy(symbol.errtxt, '527: Specified symbol size has a dimension which is too small');
        Exit(ZERROR_INVALID_OPTION);
    end;

    n_dots := (height * width) div 2;

//#ifndef _MSC_VER
//    char dot_stream[height * width * 3];
//    char dot_array[width * height * sizeof (char) ];
//#else
//    dot_stream = (char *) _alloca(height * width * 3);
//    if (!dot_stream) return ZINT_ERROR_MEMORY;
//
//    dot_array = (char *) _alloca(width * height * sizeof (char));
//    if (!dot_array) return ZINT_ERROR_MEMORY;
//#endif

    SetLength(dot_stream, height * width * 3);
    SetLength(dot_array, width * height{ * sizeof(char)});

    {* Add pad characters *}
    padding_dots := integer(n_dots - min_dots); {* get the number of free dots available for padding *}
    is_first := 1; {* first padding character flag *}

    while (padding_dots >= 9) do begin
        if (padding_dots < 18) and (data_length mod 2 = 0) then
            Dec(padding_dots, 9)

        else if (padding_dots >= 18) then begin
            if ((data_length mod 2) = 0) then
              Dec(padding_dots, 9)
            else
              Dec(padding_dots, 18);
        end
        else
            break; {* not enough padding dots left for padding *}

        if ((is_first = 1) and (binary_finish = 1)) then
            codeword_array[data_length] := 109
        else
            codeword_array[data_length] := 106;

        Inc(data_length);
        is_first := 0;
    end;

    ecc_length := 3 + (data_length div 2);

//#ifndef _MSC_VER
//    unsigned char masked_codeword_array[data_length + 1 + ecc_length];
//#else
//    masked_codeword_array = (unsigned char *) _alloca((data_length + 1 + ecc_length) * sizeof (unsigned char));
//#endif /* _MSC_VER */

  SetLength(masked_codeword_array, data_length + 1 + ecc_length);
    {* Evaluate data mask options *}
    for i := 0 to 3 {<4} do begin
        case i of
            0: begin
                masked_codeword_array[0] := 0;
                for j := 0 to data_length - 1 do
                    masked_codeword_array[j + 1] := codeword_array[j];
            end;

            1: begin
                weight := 0;
                masked_codeword_array[0] := 1;
                for j := 0 to data_length - 1 do begin
                    masked_codeword_array[j + 1] := (weight + codeword_array[j]) mod 113;
                    Inc(weight, 3);
                end;
            end;

            2: begin
                weight := 0;
                masked_codeword_array[0] := 2;
                for j := 0 to data_length - 1 do begin
                    masked_codeword_array[j + 1] := (weight + codeword_array[j]) mod 113;
                    Inc(weight, 7);
                end;
            end;

            3: begin
                weight := 0;
                masked_codeword_array[0] := 3;
                for j := 0 to data_length - 1 do begin
                    masked_codeword_array[j + 1] := (weight + codeword_array[j]) mod 113;
                    inc(weight, 17);
                end;
            end;
        end;

        rsencode(data_length + 1, ecc_length, masked_codeword_array);

        dot_stream_length := make_dotstream(masked_codeword_array, (data_length + ecc_length + 1), dot_stream);

        {* Add pad bits *}
        for jc := dot_stream_length to n_dots - 1 do
            concat(dot_stream, '1');

        fold_dotstream(dot_stream, width, height, dot_array);

        mask_score[i] := score_array(dot_array, height, width);

        if debug then
            writeln(Format('Mask %d score is %d\n', [i, mask_score[i]]));
    end;

    high_score := mask_score[0];
    best_mask := 0;

    for i := 1 to 3 {< 4} do
        if (mask_score[i] > high_score) then begin
            high_score := mask_score[i];
            best_mask := i;
        end;

    if best_mask <> 3 then begin
        {* Reprocess to get symbol with best mask *}
        case best_mask of
            0: begin
                masked_codeword_array[0] := 0;
                for j := 0 to data_length - 1 do
                    masked_codeword_array[j + 1] := codeword_array[j];
            end;

            1: begin
                weight := 0;
                masked_codeword_array[0] := 1;
                for j := 0 to data_length - 1 do begin
                    masked_codeword_array[j + 1] := (weight + codeword_array[j]) mod 113;
                    Inc(weight, 3);
                end;
            end;

            2: begin
                weight := 0;
                masked_codeword_array[0] := 2;
                for j := 0 to data_length - 1 do begin
                    masked_codeword_array[j + 1] := (weight + codeword_array[j]) mod 113;
                    Inc(weight, 7);
                end;
            end;
        end;

        rsencode(data_length + 1, ecc_length, masked_codeword_array);
        dot_stream_length := make_dotstream(masked_codeword_array, (data_length + ecc_length + 1), dot_stream);

        {* Add pad bits *}
        for jc := dot_stream_length to n_dots - 1 do
            concat(dot_stream, '1');

        fold_dotstream(dot_stream, width, height, dot_array);
    end; {* else *} { the version with the best mask is already in memory }

    if debug then begin
        for k := 0 to height - 1 do
            for j := 0 to width - 1 do
                writeln('%c', dot_array[(k * width) + j]);
            writeln('\n');
    end;

    {* Copy values to symbol *}
    symbol.width := width;
    symbol.rows := height;

    for k := 0 to height - 1 do begin
        for j := 0 to width - 1 do
            if dot_array[(k * width) + j] = '1' then
                set_module(symbol, k, j);
        symbol.row_height[k] := 1;
    end;

    if ((symbol.output_options and BARCODE_DOTTY_MODE) = 0) then
        Inc(symbol.output_options, BARCODE_DOTTY_MODE);

    Result := 0;
end;

end.
