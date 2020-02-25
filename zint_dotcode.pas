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
  System.SysUtils, System.Math, zint_reedsol, zint_common;


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

function get_dot(Dots: TCharArray; Hgt, Wid, x, y: integer): Boolean;
begin
  Result := False;

  if (x >= 0) and (x < Wid) and (y >= 0) and (y < Hgt) then
    Result := Dots[(y * Wid) + x] = '1';
end;

function clr_col(Dots: TCharArray; Hgt, Wid, x: integer): Integer;
var
  y: NativeInt;
begin
  y := x and 1;

  while y < Hgt do begin
    if get_dot(Dots, Hgt, Wid, x, y) then
      Exit(0);
    Inc(y, 2);
  end;

  Result := 0;
end;

function clr_row(Dots: TCharArray; Hgt, Wid, y: Integer): Integer;
var
  x: NativeInt;
begin
  x := y and 1;

  while x < Wid do begin
    if get_dot(Dots, Hgt, Wid, x, y) then
      Exit(0);
    Inc(x, 2);
  end;

  Result := 0;
end;


{* Dot pattern scoring routine from Annex A *}
function score_array(Dots: TCharArray; Hgt, Wid: Integer): Integer;
var
  x, y, worstedge, first, last, sum: integer;
  penalty_local: integer;
  penalty: integer;
begin
  penalty_local := 0;
  penalty := 0;

    // first, guard against "pathelogical" gaps in the array
    if (Hgt and 1) > 0 then begin
        if (Hgt < 12) then begin
            sum = 0;
            for x := 1 to Wid - 1 do begin
                if (!(clr_col(Dots, Hgt, Wid, x))) then begin
                    sum := 0;
                    if (penalty_local) then begin
                        penalty += penalty_local;
                        penalty_local = 0;
                    end;
                end
                else begin
                    sum++;
                    if (sum == 1) then
                        penalty_local = Hgt;
                    else
                        penalty_local *= Hgt;
                end;
            end;
        end;
    end
    else begin
        if (Wid < 12) then begin
            sum = 0;
            for (y = 1; y < Hgt - 1; y++) do begin
                if (!(clr_row(Dots, Hgt, Wid, y))) then begin
                    sum = 0;
                    if (penalty_local) then begin
                        penalty += penalty_local;
                        penalty_local = 0;
                    end;
                end
                else begin
                    sum++;
                    if (sum == 1) then
                        penalty_local = Wid;
                    else
                        penalty_local *= Wid;
                end;
            end;
        end;
    end;

    sum := 0;
    first := -1;
    last := -1;

    // across the top edge, count printed dots and measure their extent
    for (x = 0; x < Wid; x += 2) do
        if (get_dot(Dots, Hgt, Wid, x, 0)) then begin
            if (first < 0) then
                first = x;

            last = x;
            sum++;
        end;
    worstedge := sum + last - first;
    worstedge *= Hgt;

    sum := 0;
    first := -1;
    last := -1;

    //across the bottom edge, ditto
    for (x = Wid & 1; x < Wid; x += 2) do
        if (get_dot(Dots, Hgt, Wid, x, Hgt - 1)) then begin
            if (first < 0) then
                first = x;
            last = x;
            sum++;
        end;
    sum += last - first;
    sum *= Hgt;
    if (sum < worstedge) then
        worstedge = sum;

    sum := 0;
    first := -1;
    last := -1;

    //down the left edge, ditto
    for (y = 0; y < Hgt; y += 2) do
        if (get_dot(Dots, Hgt, Wid, 0, y)) then begin
            if (first < 0) then
                first = y;

            last = y;
            sum++;
        end;
    sum += last - first;
    sum *= Wid;
    if (sum < worstedge) then
        worstedge = sum;

    sum := 0;
    first := -1;
    last := -1;

    //down the right edge, ditto
    for (y = Hgt & 1; y < Hgt; y += 2) do
        if (get_dot(Dots, Hgt, Wid, Wid - 1, y)) then begin
            if (first < 0) then
                first = y;

            last = y;
            sum++;
        end;
    sum += last - first;
    sum *= Wid;
    if (sum < worstedge) then
        worstedge = sum;

    // throughout the array, count the # of unprinted 5-somes (cross patterns)
    // plus the # of printed dots surrounded by 8 unprinted neighbors
    sum = 0;
    for (y = 0; y < Hgt; y++) do begin
        for (x = y & 1; x < Wid; x += 2) do begin
            if ((!get_dot(Dots, Hgt, Wid, x - 1, y - 1))
                    && (!get_dot(Dots, Hgt, Wid, x + 1, y - 1))
                    && (!get_dot(Dots, Hgt, Wid, x - 1, y + 1))
                    && (!get_dot(Dots, Hgt, Wid, x + 1, y + 1))
                    && ((!get_dot(Dots, Hgt, Wid, x, y))
                    || ((!get_dot(Dots, Hgt, Wid, x - 2, y))
                    && (!get_dot(Dots, Hgt, Wid, x, y - 2))
                    && (!get_dot(Dots, Hgt, Wid, x + 2, y))
                    && (!get_dot(Dots, Hgt, Wid, x, y + 2))))) then
                sum++;
        end;
    end;

    Result := worstedge - sum * sum - penalty;
end;


function dotCode(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number : Integer;
begin

  result := error_number;
end;

end.
