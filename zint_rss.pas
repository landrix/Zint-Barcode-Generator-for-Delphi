unit zint_rss;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: DWYWBDBU (do what you want, but dont blame us)

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b complete
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, zint;

function rss14(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;
function rsslimited(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;
function general_rules(field : TArrayOfChar; _type: TArrayOfChar) : Integer;
function rssexpanded(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;

implementation

uses
  zint_common, zint_composite, zint_large, zint_helper, zint_gs1;

const NUMERIC = 110;
const ALPHA	= 97;
const ISOIEC = 105;
const INVALID_CHAR = 100;
const ANY_ENC = 120;
const ALPHA_OR_ISO = 121;

const g_sum_table : array[0..8] of Integer = (0, 161, 961, 2015, 2715, 0, 336, 1036, 1516);
const t_table : array[0..8] of Integer = (1, 10, 34, 70, 126, 4, 20, 48, 81);
const modules_odd : array[0..8] of Integer = (12, 10, 8, 6, 4, 5, 7, 9, 11);
const modules_even : array[0..8] of Integer = (4, 6, 8, 10, 12, 10, 8, 6, 4);
const widest_odd : array[0..8] of Integer = (8, 6, 4, 3, 1, 2, 4, 6, 8);
const widest_even : array[0..8] of Integer = (1, 3, 5, 6, 8, 7, 5, 3, 1);
//the global widths-array-var has been replaced be the out parameter of the getrsswidths-function

const finder_pattern : array[0..44] of Integer = (
	3, 8, 2, 1, 1,
	3, 5, 5, 1, 1,
	3, 3, 7, 1, 1,
	3, 1, 9, 1, 1,
	2, 7, 4, 1, 1,
	2, 5, 6, 1, 1,
	2, 3, 8, 1, 1,
	1, 5, 7, 1, 1,
	1, 3, 9, 1, 1
);

const checksum_weight : array[0..31] of Integer = ( { Table 5 }
	1, 3, 9, 27, 2, 6, 18, 54,
	4, 12, 36, 29, 8, 24, 72, 58,
	16, 48, 65, 37, 32, 17, 51, 74,
	64, 34, 23, 69, 49, 68, 46, 59
);

{ RSS Limited Tables }
const t_even_ltd : array[0..6] of Integer = ( 28, 728, 6454, 203, 2408, 1, 16632 );
const modules_odd_ltd : array[0..6] of Integer = ( 17, 13, 9, 15, 11, 19, 7 );
const modules_even_ltd : array[0..6] of Integer = ( 9, 13, 17, 11, 15, 7, 19 );
const widest_odd_ltd : array[0..6] of Integer = ( 6, 5, 3, 5, 4, 8, 1 );
const widest_even_ltd : array[0..6] of Integer = ( 3, 4, 6, 4, 5, 1, 8 );
const checksum_weight_ltd : array[0..27] of Integer = ( { Table 7 }
	1, 3, 9, 27, 81, 65, 17, 51, 64, 14, 42, 37, 22, 66,
	20, 60, 2, 6, 18, 54, 73, 41, 34, 13, 39, 28, 84, 74
);
const finder_pattern_ltd : array[0..1231] of Integer = (
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 2, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 3, 2, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 3, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 1, 1, 1,
	1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 3, 2, 1, 1,
	1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 3, 1, 1, 1,
	1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 3, 1, 1, 1,
	1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 1,
	1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 3, 2, 1, 1,
	1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 3, 1, 1, 1,
	1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 3, 1, 1, 1,
	1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 3, 1, 1, 1,
	1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1,
	1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 3, 2, 1, 1,
	1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 3, 1, 1, 1,
	1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 3, 1, 1, 1,
	1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 3, 1, 1, 1,
	1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1,
	1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 3, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 2, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 2, 2, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 3, 2, 1, 2, 1, 1, 1,
	1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 1,
	1, 1, 1, 1, 1, 2, 1, 1, 2, 2, 2, 1, 1, 1,
	1, 1, 1, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 1,
	1, 1, 1, 1, 1, 3, 1, 1, 2, 1, 2, 1, 1, 1,
	1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 2, 2, 1, 1,
	1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1,
	1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 2, 1, 1, 1,
	1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 1, 1,
	1, 1, 1, 3, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1,
	1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 1, 1,
	1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1,
	1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 2, 1, 1, 1,
	1, 2, 1, 1, 1, 2, 1, 1, 2, 1, 2, 1, 1, 1,
	1, 2, 1, 2, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1,
	1, 3, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 3, 2, 1, 2, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 2, 3, 1, 1, 2, 1, 1,
	1, 1, 1, 2, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1,
	1, 2, 1, 1, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1,
	1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 3, 1, 1,
	1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 2, 2, 1, 1,
	1, 1, 1, 1, 1, 1, 2, 1, 1, 3, 2, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1,
	1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1,
	1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 2, 1, 1, 1,
	1, 1, 1, 2, 1, 1, 2, 2, 1, 1, 2, 1, 1, 1,
	1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 2, 1, 1, 1,
	1, 1, 1, 3, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1,
	1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1,
	1, 2, 1, 1, 1, 1, 2, 1, 1, 2, 2, 1, 1, 1,
	1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1,
	1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 3, 1, 1,
	1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 1, 1,
	1, 1, 1, 1, 2, 1, 1, 1, 1, 3, 2, 1, 1, 1,
	1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 2, 1, 1,
	1, 1, 1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 1, 1,
	1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1,
	1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1,
	1, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1,
	1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1,
	1, 2, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1,
	1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1,
	1, 3, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1,
	1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 3, 1, 1,
	1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1,
	1, 1, 2, 1, 1, 1, 1, 1, 1, 3, 2, 1, 1, 1,
	1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 2, 2, 1, 1,
	1, 1, 2, 1, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1,
	1, 1, 2, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1, 1,
	1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1,
	1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 1,
	1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1,
	2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1,
	2, 1, 1, 1, 1, 1, 1, 1, 1, 3, 2, 1, 1, 1,
	2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 2, 1, 1,
	2, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1,
	2, 1, 1, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1, 1,
	2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 1,
	2, 1, 1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 1, 1,
	2, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1
);

{ RSS Expanded Tables }
const g_sum_exp : array[0..4] of Integer = ( 0, 348, 1388, 2948, 3988 );
const t_even_exp : array[0..4] of Integer = ( 4, 20, 52, 104, 204 );
const modules_odd_exp : array[0..4] of Integer = ( 12, 10, 8, 6, 4 );
const modules_even_exp : array[0..4] of Integer = ( 5, 7, 9, 11, 13 );
const widest_odd_exp : array[0..4] of Integer = ( 7, 5, 4, 3, 1 );
const widest_even_exp : array[0..4] of Integer = ( 2, 4, 5, 6, 8 );
const checksum_weight_exp : array[0..183] of Integer = ( { Table 14 }
	1, 3, 9, 27, 81, 32, 96, 77,
	20, 60, 180, 118, 143, 7, 21, 63,
	189, 145, 13, 39, 117, 140, 209, 205,
	193, 157, 49, 147, 19, 57, 171, 91,
	62, 186, 136, 197, 169, 85, 44, 132,
	185, 133, 188, 142, 4, 12, 36, 108,
	113, 128, 173, 97, 80, 29, 87, 50,
	150, 28, 84, 41, 123, 158, 52, 156,
	46, 138, 203, 187, 139, 206, 196, 166,
	76, 17, 51, 153, 37, 111, 122, 155,
	43, 129, 176, 106, 107, 110, 119, 146,
	16, 48, 144, 10, 30, 90, 59, 177,
	109, 116, 137, 200, 178, 112, 125, 164,
	70, 210, 208, 202, 184, 130, 179, 115,
	134, 191, 151, 31, 93, 68, 204, 190,
	148, 22, 66, 198, 172, 94, 71, 2,
	6, 18, 54, 162, 64, 192, 154, 40,
	120, 149, 25, 75, 14, 42, 126, 167,
	79, 26, 78, 23, 69, 207, 199, 175,
	103, 98, 83, 38, 114, 131, 182, 124,
	161, 61, 183, 127, 170, 88, 53, 159,
	55, 165, 73, 8, 24, 72, 5, 15,
	45, 135, 194, 160, 58, 174, 100, 89
);
const finder_pattern_exp : array[0..59] of Integer = ( { Table 15 }
	1, 8, 4, 1, 1,
	1, 1, 4, 8, 1,
	3, 6, 4, 1, 1,
	1, 1, 4, 6, 3,
	3, 4, 6, 1, 1,
	1, 1, 6, 4, 3,
	3, 2, 8, 1, 1,
	1, 1, 8, 2, 3,
	2, 6, 5, 1, 1,
	1, 1, 5, 6, 2,
	2, 2, 9, 1, 1,
	1, 1, 9, 2, 2
);
const finder_sequence : array[0..109] of Integer = ( { Table 16 }
	1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0,
	1, 6, 3, 8, 0, 0, 0, 0, 0, 0, 0,
	1, 10, 3, 8, 5, 0, 0, 0, 0, 0, 0,
	1, 10, 3, 8, 7, 12, 0, 0, 0, 0, 0,
	1, 10, 3, 8, 9, 12, 11, 0, 0, 0, 0,
	1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0,
	1, 2, 3, 4, 5, 6, 7, 10, 9, 0, 0,
	1, 2, 3, 4, 5, 6, 7, 10, 11, 12, 0,
	1, 2, 3, 4, 5, 8, 7, 10, 9, 12, 11
);
const weight_rows : array[0..209] of Integer = (
	0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 5, 6, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 9, 10, 3, 4, 13, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 17, 18, 3, 4, 13, 14, 7, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 17, 18, 3, 4, 13, 14, 11, 12, 21, 22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 17, 18, 3, 4, 13, 14, 15, 16, 21, 22, 19, 20, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0, 0, 0, 0, 0, 0,
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 17, 18, 15, 16, 0, 0, 0, 0,
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 17, 18, 19, 20, 21, 22, 0, 0,
	0, 1, 2, 3, 4, 5, 6, 7, 8, 13, 14, 11, 12, 17, 18, 15, 16, 21, 22, 19, 20
);

{*********************************************************************
* combins(n,r): returns the number of Combinations of r selected from n:
*   Combinations := n! / ((n - r)! * r!)
*********************************************************************}
function combins(n : Integer; r : Integer) : Integer;
var
  i, j : Integer;
  maxDenom, minDenom : Integer;
  val : Integer;
begin
  if (n-r > r) then
  begin
    minDenom := r;
    maxDenom := n-r;
  end
  else
  begin
    minDenom := n-r;
    maxDenom := r;
  end;
  val := 1;
  j := 1;
  for i := n downto maxDenom + 1 do
  begin
    val := val * i;
    if (j <= minDenom) then
    begin
      val := val div j;
      Inc(j);
    end;
  end;
  while j <= minDenom do
  begin
    val := val div j;
    Inc(j);
  end;

  result := val; exit;
end;

{*********************************************************************
* getRSSwidths
* routine to generate widths for RSS elements for a given value.#
*
* Calling arguments:
* val := required value
* n := number of modules
* elements := elements in a set (RSS-14 & Expanded := 4; RSS Limited := 7)
* maxWidth := maximum module width of an element
* noNarrow := 0 will skip patterns without a one module wide element
*
* Return:
* static int widths[] := element widths
*********************************************************************}
procedure getRSSwidths(val, n, elements, maxWidth, noNarrow : Integer; out widths : TArrayOfInteger);
var
  bar,
  elmWidth,
  mxwElement,
  subVal, lessVal,
  narrowMask : Integer;
begin
  narrowMask := 0;
  SetLength(widths, 8);

  bar := 0;
  while bar < elements-1 do
  begin
    elmWidth := 1;
    narrowMask := narrowMask or (1 shl bar);
    while true do
    begin
      { get all combinations }
      subVal := combins(n-elmWidth-1, elements-bar-2);
      { less combinations with no single-module element }
      if ((not (noNarrow <> 0)) and (not (narrowMask <> 0)) and
               (n-elmWidth-(elements-bar-1) >= elements-bar-1)) then
      begin
        Dec(subVal, combins(n-elmWidth-(elements-bar), elements-bar-2));
      end;
      { less combinations with elements > maxVal }
      if (elements-bar-1 > 1) then
      begin
        lessVal := 0;
        for mxwElement := n-elmWidth-(elements-bar-2) downto maxWidth + 1 do
        begin
          Inc(lessVal, combins(n-elmWidth-mxwElement-1, elements-bar-3));
        end;
        Dec(subVal, lessVal * (elements-1-bar));
      end
      else
      if (n-elmWidth > maxWidth) then
      begin
        Dec(subVal);
      end;
      Dec(val, subVal);
      if (val < 0) then break;
      Inc(elmWidth);
      narrowMask := narrowMask and (not (1 shl bar));
    end;
    Inc(val, subVal);
    Dec(n, elmWidth);
    widths[bar] := elmWidth;
    Inc(bar);
  end;
  widths[bar] := n;
  exit;
end;

{ GS1 DataBar-14 }
function rss14(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;
var
  error_number, i, j, mask : Integer;
  accum, left_reg, right_reg, x_reg, y_reg : TArrayOfSmallInt;
  data_character, data_group, v_odd, v_even : TArrayOfInteger;
  data_widths : array[0..7] of array[0..4] of Integer;
  total_widths : TArrayOfInteger;
  checksum, c_left, c_right, writer : Integer;
  latch : Char;
  hrt, temp : TArrayOfChar;
  check_digit, count, separator_row : Integer;
  widths : TArrayOfInteger;
begin
  //error_number := 0;
  SetLength(accum, 112);
  SetLength(left_reg, 112);
  SetLength(right_reg, 112);
  SetLength(x_reg, 112);
  SetLength(y_reg, 112);
  SetLength(data_character, 4);
  SetLength(data_group, 4);
  SetLength(v_odd, 4);
  SetLength(v_even, 4);
  SetLength(total_widths, 46);
  SetLength(hrt, 15);
  SetLength(temp, 32);

  separator_row := 0;

  if (src_len > 13) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(NEON, source, src_len);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  { make some room for a separator row for composite symbols }
  case symbol.symbology of
    BARCODE_RSS14_CC,
    BARCODE_RSS14STACK_CC,
    BARCODE_RSS14_OMNI_CC:
    begin
      separator_row := symbol.rows;
      symbol.row_height[separator_row] := 1;
      Inc(symbol.rows, 1);
    end;
  end;

  for i := 0 to 111 do
  begin
    accum[i] := 0;
    x_reg[i] := 0;
    y_reg[i] := 0;
  end;

  for i := 0 to 3 do
  begin
    data_character[i] := 0;
    data_group[i] := 0;
  end;

  binary_load(accum, ArrayOfByteToArrayOfChar(source), src_len);

  strcpy(temp, '10000000000000');
  if (symbol.option_1 = 2) then
  begin
    { Add symbol linkage flag }
    binary_load(y_reg, temp, strlen(temp));
    binary_add(accum, y_reg);
    for i := 0 to 111 do
      y_reg[i] := 0;
  end;

  { Calculate left and right pair values }
  strcpy(temp, '4537077');
  binary_load(x_reg, temp, strlen(temp));

  for i := 0 to 23 do
    shiftup(x_reg);

  for i := 24 downto 0 do
  begin
    y_reg[i] := islarger(accum, x_reg);
    if (y_reg[i] = 1) then
      binary_subtract(accum, x_reg);
    shiftdown(x_reg);
  end;

  for i := 0 to 111 do
  begin
    left_reg[i] := y_reg[i];
    right_reg[i] := accum[i];
  end;

  { Calculate four data characters }
  strcpy(temp,  '1597');
  binary_load(x_reg, temp, strlen(temp));
  for i := 0 to 111 do begin
    accum[i] := left_reg[i];
  end;

  for i := 0 to 23 do
    shiftup(x_reg);

  for i := 24 downto 0 do
  begin
    y_reg[i] := islarger(accum, x_reg);
    if (y_reg[i] = 1) then
      binary_subtract(accum, x_reg);
    shiftdown(x_reg);
  end;

  data_character[0] := 0;
  data_character[1] := 0;
  mask := $2000;
  for i := 13 downto 0 do
  begin
    if (y_reg[i] = 1) then
      Inc(data_character[0], mask);
    if (accum[i] = 1) then
      Inc(data_character[1], mask);
    mask := mask shr 1;
  end;
  strcpy(temp,  '1597');
  binary_load(x_reg, temp, strlen(temp));
  for i := 0 to 111 do
    accum[i] := right_reg[i];

  for i := 0 to 23 do
    shiftup(x_reg);

  for i := 24 downto 0 do
  begin
    y_reg[i] := islarger(accum, x_reg);
    if (y_reg[i] = 1) then
      binary_subtract(accum, x_reg);
    shiftdown(x_reg);
  end;

  data_character[2] := 0;
  data_character[3] := 0;
  mask := $2000;
  for i := 13 downto 0 do
  begin
    if (y_reg[i] = 1) then
      Inc(data_character[2], mask);
    if (accum[i] = 1) then
      Inc(data_character[3], mask);
    mask := mask shr 1;
  end;

  { Calculate odd and even subset values }

  if ((data_character[0] >= 0) and (data_character[0] <= 160)) then data_group[0] := 0;
  if ((data_character[0] >= 161) and (data_character[0] <= 960)) then data_group[0] := 1;
  if ((data_character[0] >= 961) and (data_character[0] <= 2014)) then data_group[0] := 2;
  if ((data_character[0] >= 2015) and (data_character[0] <= 2714)) then data_group[0] := 3;
  if ((data_character[0] >= 2715) and (data_character[0] <= 2840)) then data_group[0] := 4;
  if ((data_character[1] >= 0) and (data_character[1] <= 335)) then data_group[1] := 5;
  if ((data_character[1] >= 336) and (data_character[1] <= 1035)) then data_group[1] := 6;
  if ((data_character[1] >= 1036) and (data_character[1] <= 1515)) then data_group[1] := 7;
  if ((data_character[1] >= 1516) and (data_character[1] <= 1596)) then data_group[1] := 8;
  if ((data_character[3] >= 0) and (data_character[3] <= 335)) then data_group[3] := 5;
  if ((data_character[3] >= 336) and (data_character[3] <= 1035)) then data_group[3] := 6;
  if ((data_character[3] >= 1036) and (data_character[3] <= 1515)) then data_group[3] := 7;
  if ((data_character[3] >= 1516) and (data_character[3] <= 1596)) then data_group[3] := 8;
  if ((data_character[2] >= 0) and (data_character[2] <= 160)) then data_group[2] := 0;
  if ((data_character[2] >= 161) and (data_character[2] <= 960)) then data_group[2] := 1;
  if ((data_character[2] >= 961) and (data_character[2] <= 2014)) then data_group[2] := 2;
  if ((data_character[2] >= 2015) and (data_character[2] <= 2714)) then data_group[2] := 3;
  if ((data_character[2] >= 2715) and (data_character[2] <= 2840)) then data_group[2] := 4;

  v_odd[0] := (data_character[0] - g_sum_table[data_group[0]]) div t_table[data_group[0]];
  v_even[0] := (data_character[0] - g_sum_table[data_group[0]]) mod t_table[data_group[0]];
  v_odd[1] := (data_character[1] - g_sum_table[data_group[1]]) mod t_table[data_group[1]];
  v_even[1] := (data_character[1] - g_sum_table[data_group[1]]) div t_table[data_group[1]];
  v_odd[3] := (data_character[3] - g_sum_table[data_group[3]]) mod t_table[data_group[3]];
  v_even[3] := (data_character[3] - g_sum_table[data_group[3]]) div t_table[data_group[3]];
  v_odd[2] := (data_character[2] - g_sum_table[data_group[2]]) div t_table[data_group[2]];
  v_even[2] := (data_character[2] - g_sum_table[data_group[2]]) mod t_table[data_group[2]];


  { Use RSS subset width algorithm }
  for i := 0 to 3 do
  begin
    if ((i = 0) or (i = 2)) then
    begin
      getRSSwidths(v_odd[i], modules_odd[data_group[i]], 4, widest_odd[data_group[i]], 1, widths);
      data_widths[0][i] := widths[0];
      data_widths[2][i] := widths[1];
      data_widths[4][i] := widths[2];
      data_widths[6][i] := widths[3];
      getRSSwidths(v_even[i], modules_even[data_group[i]], 4, widest_even[data_group[i]], 0, widths);
      data_widths[1][i] := widths[0];
      data_widths[3][i] := widths[1];
      data_widths[5][i] := widths[2];
      data_widths[7][i] := widths[3];
    end
    else
    begin
      getRSSwidths(v_odd[i], modules_odd[data_group[i]], 4, widest_odd[data_group[i]], 0, widths);
      data_widths[0][i] := widths[0];
      data_widths[2][i] := widths[1];
      data_widths[4][i] := widths[2];
      data_widths[6][i] := widths[3];
      getRSSwidths(v_even[i], modules_even[data_group[i]], 4, widest_even[data_group[i]], 1, widths);
      data_widths[1][i] := widths[0];
      data_widths[3][i] := widths[1];
      data_widths[5][i] := widths[2];
      data_widths[7][i] := widths[3];
    end;
  end;

  checksum := 0;
  { Calculate the checksum }
  for i := 0 to 7 do
  begin
    Inc(checksum, checksum_weight[i] * data_widths[i][0]);
    Inc(checksum, checksum_weight[i+8] * data_widths[i][1]);
    Inc(checksum, checksum_weight[i+16] * data_widths[i][2]);
    Inc(checksum, checksum_weight[i+24] * data_widths[i][3]);
  end;
  checksum := checksum mod 79;

  { Calculate the two check characters }
  if (checksum >= 8) then Inc(checksum);
  if (checksum >= 72) then Inc(checksum);
  c_left := checksum div 9;
  c_right := checksum mod 9;

  { Put element widths together }
  total_widths[0] := 1;
  total_widths[1] := 1;
  total_widths[44] := 1;
  total_widths[45] := 1;
  for i := 0 to 7 do
  begin
    total_widths[i + 2] := data_widths[i][0];
    total_widths[i + 15] := data_widths[7 - i][1];
    total_widths[i + 23] := data_widths[i][3];
    total_widths[i + 36] := data_widths[7 - i][2];
  end;
  for i := 0 to 4 do
  begin
    total_widths[i + 10] := finder_pattern[i + (5 * c_left)];
    total_widths[i + 31] := finder_pattern[(4 - i) + (5 * c_right)];
  end;

  { Put this data into the symbol }
  if ((symbol.symbology = BARCODE_RSS14) or (symbol.symbology = BARCODE_RSS14_CC)) then
  begin
    writer := 0;
    latch := '0';
    for i := 0 to 45 do
    begin
      for j := 0 to total_widths[i] - 1 do
      begin
        if (latch = '1') then set_module(symbol, symbol.rows, writer);
        Inc(writer);
      end;
      if (latch = '1') then
        latch := '0'
      else
        latch := '1';
    end;
    if (symbol.width < writer) then symbol.width := writer;
    if (symbol.symbology = BARCODE_RSS14_CC) then
    begin
      { separator pattern for composite symbol }
      for i := 4 to 91 do
      begin
        if (not (module_is_set(symbol, separator_row + 1, i) <> 0)) then
          set_module(symbol, separator_row, i);
      end;
      latch := '1';
      for i := 16 to 31 do
      begin
        if (not (module_is_set(symbol, separator_row + 1, i) <> 0)) then
        begin
          if (latch = '1') then
          begin
            set_module(symbol, separator_row, i);
            latch := '0';
          end
          else
          begin
            unset_module(symbol, separator_row, i);
            latch := '1';
          end;
        end
        else
        begin
          unset_module(symbol, separator_row, i);
          latch := '1';
        end;
      end;
      latch := '1';
      for i := 63 to 77 do
      begin
        if (not (module_is_set(symbol, separator_row + 1, i) <> 0)) then
        begin
          if (latch = '1') then
          begin
            set_module(symbol, separator_row, i);
            latch := '0';
          end
          else
          begin
            unset_module(symbol, separator_row, i);
            latch := '1';
          end;
        end
        else
        begin
          unset_module(symbol, separator_row, i);
          latch := '1';
        end;
      end;
    end;
    symbol.rows := symbol.rows + 1;

    count := 0;
    //check_digit := 0;

    { Calculate check digit from Annex A and place human readable text }
    ustrcpy(symbol.text, '(01)');
    for i := 0 to 13 do
      hrt[i] := '0';
    for i := 0 to src_len - 1 do
      hrt[12 - i] := Chr(source[src_len - i - 1]);
    hrt[14] := #0;

    for i := 0 to 12 do
    begin
      Inc(count, ctoi(hrt[i]));

      if (not ((i and 1) <> 0)) then
        Inc(count, 2 * (ctoi(hrt[i])));
    end;

    check_digit := 10 - (count mod 10);
    if (check_digit = 10) then check_digit := 0;
    hrt[13] := itoc(check_digit);

    uconcat(symbol.text, hrt);
  end;

  if ((symbol.symbology = BARCODE_RSS14STACK) or (symbol.symbology = BARCODE_RSS14STACK_CC)) then
  begin
    { top row }
    writer := 0;
    latch := '0';
    for i := 0 to 22 do
    begin
      for j := 0 to total_widths[i] - 1 do
      begin
        if (latch = '1') then
          set_module(symbol, symbol.rows, writer)
        else
          unset_module(symbol, symbol.rows, writer);
        Inc(writer);
      end;
      if (latch = '1') then
        latch := '0'
      else
        latch := '1';
    end;
    set_module(symbol, symbol.rows, writer);
    unset_module(symbol, symbol.rows, writer + 1);
    symbol.row_height[symbol.rows] := 5;
    { bottom row }
    symbol.rows := symbol.rows + 2;
    set_module(symbol, symbol.rows, 0);
    unset_module(symbol, symbol.rows, 1);
    writer := 0;
    latch := '1';
    for i := 23 to 45 do
    begin
      for j := 0 to total_widths[i] - 1 do
      begin
        if (latch = '1') then
          set_module(symbol, symbol.rows, writer + 2)
        else
          unset_module(symbol, symbol.rows, writer + 2);
        Inc(writer);
      end;
      if (latch = '1') then
        latch := '0'
      else
        latch := '1';
    end;
    symbol.row_height[symbol.rows] := 7;
    { separator pattern }
    for i := 4 to 45 do
    begin
      if (module_is_set(symbol, symbol.rows - 2, i) = module_is_set(symbol, symbol.rows, i)) then
      begin
        if (not (module_is_set(symbol, symbol.rows - 2, i) <> 0)) then
          set_module(symbol, symbol.rows - 1, i);
      end
      else
      begin
        if (not (module_is_set(symbol, symbol.rows - 1, i - 1) <> 0)) then
          set_module(symbol, symbol.rows - 1, i);
      end;
    end;
    symbol.row_height[symbol.rows - 1] := 1;
    if (symbol.symbology = BARCODE_RSS14STACK_CC) then
    begin
      { separator pattern for composite symbol }
      for i := 4 to 45 do
      begin
        if (not (module_is_set(symbol, separator_row + 1, i) <> 0)) then
          set_module(symbol, separator_row, i);
      end;
      latch := '1';
      for i := 16 to 31 do
      begin
        if (not (module_is_set(symbol, separator_row + 1, i) <> 0)) then
        begin
          if (latch = '1') then
          begin
            set_module(symbol, separator_row, i);
            latch := '0';
          end
          else
          begin
            unset_module(symbol, separator_row, i);
            latch := '1';
          end;
        end
        else
        begin
          unset_module(symbol, separator_row, i);
          latch := '1';
        end;
      end;
    end;
    symbol.rows := symbol.rows + 1;
    if (symbol.width < 50) then symbol.width := 50;
  end;

  if ((symbol.symbology = BARCODE_RSS14STACK_OMNI) or (symbol.symbology = BARCODE_RSS14_OMNI_CC)) then
  begin
    { top row }
    writer := 0;
    latch := '0';
    for i := 0 to 22 do
    begin
      for j := 0 to total_widths[i] - 1 do
      begin
        if (latch = '1') then set_module(symbol, symbol.rows, writer) else unset_module(symbol, symbol.rows, writer);
        Inc(writer);
      end;
      if latch = '1' then latch := '0' else latch := '1';
    end;
    set_module(symbol, symbol.rows, writer);
    unset_module(symbol, symbol.rows, writer + 1);
    { bottom row }
    symbol.rows := symbol.rows + 4;
    set_module(symbol, symbol.rows, 0);
    unset_module(symbol, symbol.rows, 1);
    writer := 0;
    latch := '1';
    for i := 23 to 45 do
    begin
      for j := 0 to total_widths[i] - 1 do
      begin
        if (latch = '1') then set_module(symbol, symbol.rows, writer + 2) else unset_module(symbol, symbol.rows, writer + 2);
        Inc(writer);
      end;
      if (latch = '1') then
        latch := '0'
      else
        latch := '1';
    end;
    { middle separator }
    i := 5;
    while i < 46 do
    begin
      set_module(symbol, symbol.rows - 2, i);
      Inc(i, 2);
    end;
    symbol.row_height[symbol.rows - 2] := 1;
    { top separator }
    for i := 4 to 45 do
    begin
      if (not (module_is_set(symbol, symbol.rows - 4, i) <> 0)) then
        set_module(symbol, symbol.rows - 3, i);
    end;
    latch := '1';
    for i := 17 to 32 do
    begin
      if (not (module_is_set(symbol, symbol.rows - 4, i) <> 0)) then
      begin
        if (latch = '1') then
        begin
          set_module(symbol, symbol.rows - 3, i);
          latch := '0';
        end
        else
        begin
          unset_module(symbol, symbol.rows - 3, i);
          latch := '1';
        end;
      end
      else
      begin
        unset_module(symbol, symbol.rows - 3, i);
        latch := '1';
      end;
    end;
    symbol.row_height[symbol.rows - 3] := 1;
    { bottom separator }
    for i := 4 to 45 do
    begin
      if (not (module_is_set(symbol, symbol.rows, i) <> 0)) then
        set_module(symbol, symbol.rows - 1, i);
    end;
    latch := '1';
    for i := 16 to 31 do
    begin
      if (not (module_is_set(symbol, symbol.rows, i) <> 0)) then
      begin
        if (latch = '1') then
        begin
          set_module(symbol, symbol.rows - 1, i);
          latch := '0';
        end
        else
        begin
          unset_module(symbol, symbol.rows - 1, i);
          latch := '1';
        end;
      end
      else
      begin
        unset_module(symbol, symbol.rows - 1, i);
        latch := '1';
      end;
    end;
    symbol.row_height[symbol.rows - 1] := 1;
    if (symbol.width < 50) then symbol.width := 50;
    if (symbol.symbology = BARCODE_RSS14_OMNI_CC) then
    begin
      { separator pattern for composite symbol }
      for i := 4 to 45 do
      begin
        if (not (module_is_set(symbol, separator_row + 1, i) <> 0)) then
          set_module(symbol, separator_row, i);
      end;
      latch := '1';
      for i := 16 to 31 do
      begin
        if (not (module_is_set(symbol, separator_row + 1, i) <> 0)) then
        begin
          if (latch = '1') then
          begin
            set_module(symbol, separator_row, i);
            latch := '0';
          end
          else
          begin
            unset_module(symbol, separator_row, i);
            latch := '1';
          end;
        end
        else
        begin
          unset_module(symbol, separator_row, i);
          latch := '1';
        end;
      end;
    end;
    symbol.rows := symbol.rows + 1;
  end;


  result := error_number; exit;
end;

{ GS1 DataBar Limited }
function rsslimited(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;
var
  error_number, i, mask : Integer;
  accum, left_reg, right_reg, x_reg, y_reg : TArrayOfSmallInt;
  left_group, right_group, left_odd, left_even, right_odd, right_even : Integer;
  left_character, right_character : Integer;
  left_widths, right_widths : TArrayOfInteger;
  checksum : Integer;
  check_elements, total_widths : TArrayOfInteger;
  writer, j, check_digit, count : Integer;
  latch : Char;
  hrt, temp : TArrayOfChar;
  separator_row : Integer;
  widths : TArrayOfInteger;
begin
  //error_number := 0;
  SetLength(accum, 112);
  SetLength(left_reg, 112);
  SetLength(right_reg, 112);
  SetLength(x_reg, 112);
  SetLength(y_reg, 112);
  SetLength(left_widths, 14);
  SetLength(right_widths, 14);
  SetLength(check_elements, 14);
  SetLength(total_widths, 46);
  SetLength(hrt, 15);
  SetLength(temp, 32);

  separator_row := 0;

  if (src_len > 13) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(NEON, source, src_len);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;
  if (src_len = 13) then
  begin
    if ((source[0] <> Ord('0')) and (source[0] <> Ord('1'))) then
    begin
      strcpy(symbol.errtxt, 'Input out of range');
      result := ZERROR_INVALID_DATA; exit;
    end;
  end;

  { make some room for a separator row for composite symbols }
  if (symbol.symbology = BARCODE_RSS_LTD_CC) then
  begin
    separator_row := symbol.rows;
    symbol.row_height[separator_row] := 1;
    Inc(symbol.rows, 1);
  end;

  for i := 0 to 111 do begin
    accum[i] := 0;
    x_reg[i] := 0;
    y_reg[i] := 0;
  end;

  binary_load(accum, ArrayOfByteToArrayOfChar(source), src_len);
  if (symbol.option_1 = 2) then
  begin
    { Add symbol linkage flag }
    strcpy(temp, '2015133531096');
    binary_load(y_reg, temp, strlen(temp));
    binary_add(accum, y_reg);
    for i := 0 to 111 do begin
      y_reg[i] := 0;
    end;
  end;

  { Calculate left and right pair values }
  strcpy(temp, '2013571');
  binary_load(x_reg, temp, strlen(temp));

  for i := 0 to 23 do
    shiftup(x_reg);

  for i := 24 downto 0 do begin
    y_reg[i] := islarger(accum, x_reg);
    if (y_reg[i] = 1) then
      binary_subtract(accum, x_reg);
    shiftdown(x_reg);
  end;

  for i := 0 to 111 do
  begin
    left_reg[i] := y_reg[i];
    right_reg[i] := accum[i];
  end;

  left_group := 0;
  strcpy(temp, '183063');
  binary_load(accum, temp, strlen(temp));
  if (islarger(left_reg, accum) <> 0) then left_group := 1;
  strcpy(temp, '820063');
  binary_load(accum, temp, strlen(temp));
  if (islarger(left_reg, accum) <> 0) then left_group := 2;
  strcpy(temp, '1000775');
  binary_load(accum, temp, strlen(temp));
  if (islarger(left_reg, accum) <> 0) then left_group := 3;
  strcpy(temp, '1491020');
  binary_load(accum, temp, strlen(temp));
  if (islarger(left_reg, accum) <> 0) then left_group := 4;
  strcpy(temp, '1979844');
  binary_load(accum, temp, strlen(temp));
  if (islarger(left_reg, accum) <> 0) then left_group := 5;
  strcpy(temp, '1996938');
  binary_load(accum, temp, strlen(temp));
  if (islarger(left_reg, accum) <> 0) then left_group := 6;
  right_group := 0;
  strcpy(temp, '183063');
  binary_load(accum, temp, strlen(temp));
  if (islarger(right_reg, accum) <> 0) then right_group := 1;
  strcpy(temp, '820063');
  binary_load(accum, temp, strlen(temp));
  if (islarger(right_reg, accum) <> 0) then right_group := 2;
  strcpy(temp, '1000775');
  binary_load(accum, temp, strlen(temp));
  if (islarger(right_reg, accum) <> 0) then right_group := 3;
  strcpy(temp, '1491020');
  binary_load(accum, temp, strlen(temp));
  if (islarger(right_reg, accum) <> 0) then right_group := 4;
  strcpy(temp, '1979844');
  binary_load(accum, temp, strlen(temp));
  if (islarger(right_reg, accum) <> 0) then right_group := 5;
  strcpy(temp, '1996938');
  binary_load(accum, temp, strlen(temp));
  if (islarger(right_reg, accum) <> 0) then right_group := 6;

  case left_group of
    1:
    begin
      strcpy(temp, '183064');
      binary_load(accum, temp, strlen(temp));
      binary_subtract(left_reg, accum);
    end;
    2:
    begin
      strcpy(temp, '820064');
      binary_load(accum, temp, strlen(temp));
      binary_subtract(left_reg, accum);
    end;
    3:
    begin
      strcpy(temp, '1000776');
      binary_load(accum, temp, strlen(temp));
      binary_subtract(left_reg, accum);
    end;
    4:
    begin
      strcpy(temp, '1491021');
      binary_load(accum, temp, strlen(temp));
      binary_subtract(left_reg, accum);
    end;
    5:
    begin
      strcpy(temp, '1979845');
      binary_load(accum, temp, strlen(temp));
      binary_subtract(left_reg, accum);
    end;
    6:
    begin
      strcpy(temp, '1996939');
      binary_load(accum, temp, strlen(temp));
      binary_subtract(left_reg, accum);
    end;
  end;

  case right_group of
    1:
    begin
      strcpy(temp, '183064');
      binary_load(accum, temp, strlen(temp));
      binary_subtract(right_reg, accum);
    end;
    2:
    begin
      strcpy(temp, '820064');
      binary_load(accum, temp, strlen(temp));
      binary_subtract(right_reg, accum);
    end;
    3:
    begin
      strcpy(temp, '1000776');
      binary_load(accum, temp, strlen(temp));
      binary_subtract(right_reg, accum);
    end;
    4:
    begin
      strcpy(temp, '1491021');
      binary_load(accum, temp, strlen(temp));
      binary_subtract(right_reg, accum);
    end;
    5:
    begin
      strcpy(temp, '1979845');
      binary_load(accum, temp, strlen(temp));
      binary_subtract(right_reg, accum);
    end;
    6:
    begin
      strcpy(temp, '1996939');
      binary_load(accum, temp, strlen(temp));
      binary_subtract(right_reg, accum);
    end;
  end;

  left_character := 0;
  right_character := 0;
  mask := $800000;
  for i := 23 downto 0 do
  begin
    if (left_reg[i] = 1) then
      Inc(left_character, mask);
    if (right_reg[i] = 1) then
      Inc(right_character, mask);

    mask := mask shr 1;
  end;

  left_odd := left_character div t_even_ltd[left_group];
  left_even := left_character mod t_even_ltd[left_group];
  right_odd := right_character div t_even_ltd[right_group];
  right_even := right_character mod t_even_ltd[right_group];

  getRSSwidths(left_odd, modules_odd_ltd[left_group], 7, widest_odd_ltd[left_group], 1, widths);
  left_widths[0] := widths[0];
  left_widths[2] := widths[1];
  left_widths[4] := widths[2];
  left_widths[6] := widths[3];
  left_widths[8] := widths[4];
  left_widths[10] := widths[5];
  left_widths[12] := widths[6];
  getRSSwidths(left_even, modules_even_ltd[left_group], 7, widest_even_ltd[left_group], 0, widths);
  left_widths[1] := widths[0];
  left_widths[3] := widths[1];
  left_widths[5] := widths[2];
  left_widths[7] := widths[3];
  left_widths[9] := widths[4];
  left_widths[11] := widths[5];
  left_widths[13] := widths[6];
  getRSSwidths(right_odd, modules_odd_ltd[right_group], 7, widest_odd_ltd[right_group], 1, widths);
  right_widths[0] := widths[0];
  right_widths[2] := widths[1];
  right_widths[4] := widths[2];
  right_widths[6] := widths[3];
  right_widths[8] := widths[4];
  right_widths[10] := widths[5];
  right_widths[12] := widths[6];
  getRSSwidths(right_even, modules_even_ltd[right_group], 7, widest_even_ltd[right_group], 0, widths);
  right_widths[1] := widths[0];
  right_widths[3] := widths[1];
  right_widths[5] := widths[2];
  right_widths[7] := widths[3];
  right_widths[9] := widths[4];
  right_widths[11] := widths[5];
  right_widths[13] := widths[6];

  checksum := 0;
  { Calculate the checksum }
  for i := 0 to 13 do begin
    Inc(checksum, checksum_weight_ltd[i] * left_widths[i]);
    inc(checksum, checksum_weight_ltd[i + 14] * right_widths[i]);
  end;
  checksum := checksum mod 89;

  for i := 0 to 13 do
    check_elements[i] := finder_pattern_ltd[i + (checksum * 14)];

  total_widths[0] := 1;
  total_widths[1] := 1;
  total_widths[44] := 1;
  total_widths[45] := 1;
  for i := 0 to 13 do
  begin
    total_widths[i + 2] := left_widths[i];
    total_widths[i + 16] := check_elements[i];
    total_widths[i + 30] := right_widths[i];
  end;

  writer := 0;
  latch := '0';
  for i := 0 to 45 do
  begin
    for j := 0 to total_widths[i] - 1 do
    begin
      if (latch = '1') then set_module(symbol, symbol.rows, writer) else unset_module(symbol, symbol.rows, writer);
      Inc(writer);
    end;
    if latch = '1' then latch := '0' else latch := '1';
  end;
  if (symbol.width < writer) then symbol.width := writer;
  symbol.rows := symbol.rows + 1;

  { add separator pattern if composite symbol }
  if (symbol.symbology = BARCODE_RSS_LTD_CC) then
  begin
    for i := 4 to 69 do
    begin
      if (not (module_is_set(symbol, separator_row + 1, i) <> 0)) then
        set_module(symbol, separator_row, i);
     end;
  end;

  { Calculate check digit from Annex A and place human readable text }

  //check_digit := 0;
  count := 0;

  ustrcpy(symbol.text, '(01)');
  for i := 0 to 13 do
    hrt[i] := '0';
  for i := 0 to src_len - 1 do
    hrt[12 - i] := Chr(source[src_len - i - 1]);

  for i := 0 to 12 do
  begin
    Inc(count, ctoi(hrt[i]));

    if (not ((i and 1) <> 0)) then
      Inc(count,2 * (ctoi(hrt[i])));
  end;

  check_digit := 10 - (count mod 10);
  if (check_digit = 10) then check_digit := 0;

  hrt[13] := itoc(check_digit);
  hrt[14] := #0;

  uconcat(symbol.text, hrt);

  result := error_number; exit;
end;


{ Attempts to apply encoding rules from secions 7.2.5.5.1 to 7.2.5.5.3 of ISO/IEC 24724:2006 }
function general_rules(field : TArrayOfChar; _type: TArrayOfChar) : Integer;
var
  block : array[0..1] of array[0..199] of Integer;
  block_count, i, j, k : Integer;
  current, next, last : Byte;
begin
  block_count := 0;

  block[0][block_count] := 1;
  block[1][block_count] := Ord(_type[0]);

  for i := 1 to strlen(_type) - 1 do
  begin
    current := Ord(_type[i]);
    last := Ord(_type[i - 1]);

    if (current = last) then
      block[0][block_count] := block[0][block_count] + 1
    else
    begin
      Inc(block_count);
      block[0][block_count] := 1;
      block[1][block_count] := Ord(_type[i]);
    end;
  end;

  Inc(block_count);

  for i := 0 to block_count - 1 do
  begin
    current := block[1][i];
    next := (block[1][i + 1] and $FF);

    if ((current = ISOIEC) and (i <> (block_count - 1))) then
    begin
      if ((next = ANY_ENC) and (block[0][i + 1] >= 4)) then
        block[1][i + 1] := NUMERIC;

      if ((next = ANY_ENC) and (block[0][i + 1] < 4)) then
        block[1][i + 1] := ISOIEC;

      if ((next = ALPHA_OR_ISO) and (block[0][i + 1] >= 5)) then
        block[1][i + 1] := ALPHA;

      if ((next = ALPHA_OR_ISO) and (block[0][i + 1] < 5)) then
        block[1][i + 1] := ISOIEC;

    end;

    if (current = ALPHA_OR_ISO) then
      block[1][i] := ALPHA;


    if ((current = ALPHA) and (i <> (block_count - 1))) then
    begin
      if ((next = ANY_ENC) and (block[0][i + 1] >= 6)) then
        block[1][i + 1] := NUMERIC;

      if ((next = ANY_ENC) and (block[0][i + 1] < 6)) then
      begin
        if ((i = block_count - 2) and (block[0][i + 1] >= 4)) then
          block[1][i + 1] := NUMERIC
        else
          block[1][i + 1] := ALPHA;
      end;
    end;

    if (current = ANY_ENC) then
      block[1][i] := NUMERIC;
  end;

  if (block_count > 1) then
  begin
    i := 1;
    while(i < block_count) do
    begin
      if (block[1][i - 1] = block[1][i]) then
      begin
        { bring together }
        block[0][i - 1] := block[0][i - 1] + block[0][i];
        j := i + 1;

        { decreace the list }
        while(j < block_count) do
        begin
          block[0][j - 1] := block[0][j];
          block[1][j - 1] := block[1][j];
          Inc(j);
        end;
        Dec(block_count);
        Dec(i);
      end;
      Inc(i);
    end;
  end;

  for i := 0 to block_count - 2 do
  begin
    if ((block[1][i] = NUMERIC) and ((block[0][i] and 1) <> 0)) then
    begin
      { Odd size numeric block }
      block[0][i] := block[0][i] - 1;
      block[0][i + 1] := block[0][i + 1] + 1;
    end;
  end;

  j := 0;
  for i := 0 to block_count - 1 do
  begin
    for k := 0 to  block[0][i] - 1 do
    begin
      _type[j] := Chr(block[1][i]);
      Inc(j);
    end;
  end;

  if ((block[1][block_count - 1] = NUMERIC) and ((block[0][block_count - 1] and 1) <> 0)) then
  begin
    { If the last block is numeric and an odd size, further
    processing needs to be done outside this procedure }
    result := 1; exit
  end
  else
  begin
    result := 0; exit;
  end;
end;

{ Handles all data encodation from section 7.2.5 of ISO/IEC 24724 }
function rss_binary_string(symbol : zint_symbol; const source : TArrayOfChar; var binary_string : TArrayOfChar) : Integer;
var
  encoding_method, i, mask, j, read_posn, latch, last_mode : Integer;
  general_field, general_field_type : TArrayOfChar;
  remainder, d1, d2, value : Integer;
  padstring : TArrayOfChar;
  weight_str, date_str, currency_str : TArrayOfChar;
  weight : Single;
  group : TArrayOfChar;
  group_val : Integer;
begin
  //last_mode := ISOIEC;
  SetLength(general_field, strlen(source) + 1);
  SetLength(general_field_type, strlen(source) + 1);
  SetLength(padstring, 40);

  read_posn := 0;
  //value := 0;

  { Decide whether a compressed data field is required and if so what
  method to use - method 2 := no compressed data field }

  if ((strlen(source) >= 16) and ((source[0] = '0') and (source[1] = '1'))) then
  begin
    { (01) and other AIs }
    encoding_method := 1;
    {$IFDEF DEBUG_ZINT} WriteLn('Choosing Method 1');{$ENDIF}
  end
  else
  begin
    { any AIs }
    encoding_method := 2;
    {$IFDEF DEBUG_ZINT} WriteLn('Choosing Mehod 2');{$ENDIF}
  end;

  if (((strlen(source) >= 20) and (encoding_method = 1)) and ((source[2] = '9') and (source[16] = '3'))) then
  begin
    { Possibly encoding method > 2 }
    {$IFDEF DEBUG_ZINT} WriteLn('Checking for other methods');{$ENDIF}

    if ((strlen(source) >= 26) and (source[17] = '1')) then
    begin
      { Methods 3, 7, 9, 11 and 13 }

      if (source[18] = '0') then
      begin
        { (01) and (31$) }
        SetLength(weight_str, 7);

        for i := 0 to 5 do
          weight_str[i] := source[20 + i];
        weight_str[6] := #0;

        if (weight_str[0] = '0') then
        begin { Maximum weight := 99999 }
          encoding_method := 7;

          if ((source[19] = '3') and (strlen(source) = 26)) then
          begin
            { (01) and (3103) }
            weight := StrToFloat(ArrayOfCharToString(weight_str)) / 1000.0;

            if (weight <= 32.767) then encoding_method := 3;
          end;

          if (strlen(source) = 34) then
          begin
            if ((source[26] = '1') and (source[27] = '1')) then
              { (01), (31$) and (11) - metric weight and production date }
              encoding_method := 7;

            if ((source[26] = '1') and (source[27] = '3')) then
              { (01), (31$) and (13) - metric weight and packaging date }
              encoding_method := 9;

            if ((source[26] = '1') and (source[27] = '5')) then
              { (01), (31$) and (15) - metric weight and 'best before' date }
              encoding_method := 11;

            if ((source[26] = '1') and (source[27] = '7')) then
              { (01), (31$) and (17) - metric weight and expiration date }
              encoding_method := 13;
          end;
        end;
      end;
      {$IFDEF DEBUG_ZINT} WriteLn(Format('Now using method %d', [encoding_method]));{$ENDIF}
    end;

    if ((strlen(source) >= 26) and (source[17] = '2')) then
    begin
      { Methods 4, 8, 10, 12 and 14 }

      if (source[18] = '0') then
      begin
        { (01) and (32$) }
        SetLength(weight_str, 7);

        for i := 0 to 5 do
          weight_str[i] := source[20 + i];
        weight_str[6] := #0;

        if (weight_str[0] = '0') then
        begin { Maximum weight := 99999 }
          encoding_method := 8;

          if (((source[19] = '2') or (source[19] = '3')) and (strlen(source) = 26)) then
          begin
            { (01) and (3202)/(3203) }

            if (source[19] = '3') then
            begin
              weight := StrToFloat(ArrayOfCharToString(weight_str)) / 1000.0;
              if (weight <= 22.767) then
                encoding_method := 4;
            end
            else
            begin
              weight := StrToFloat(ArrayOfCharToString(weight_str)) / 100.0;
              if (weight <= 99.99) then
                encoding_method := 4;
            end;
          end;

          if (strlen(source) = 34) then
          begin
            if ((source[26] = '1') and (source[27] = '1')) then
              { (01), (32$) and (11) - English weight and production date }
              encoding_method := 8;

            if ((source[26] = '1') and (source[27] = '3')) then
              { (01), (32$) and (13) - English weight and packaging date }
              encoding_method := 10;

            if ((source[26] = '1') and (source[27] = '5')) then
              { (01), (32$) and (15) - English weight and 'best before' date }
              encoding_method := 12;

            if ((source[26] = '1') and (source[27] = '7')) then
              { (01), (32$) and (17) - English weight and expiration date }
              encoding_method := 14;
          end;
        end;
      end;
      {$IFDEF DEBUG_ZINT} WriteLn(Format('Now using method %d', [encoding_method]));{$ENDIF}

    end;

    if (source[17] = '9') then
    begin
      { Methods 5 and 6 }
      if ((source[18] = '2') and ((source[19] >= '0') and (source[19] <= '3'))) then
        { (01) and (392x) }
        encoding_method := 5;

      if ((source[18] = '3') and ((source[19] >= '0') and (source[19] <= '3'))) then
        { (01) and (393x) }
        encoding_method := 6;

      {$IFDEF DEBUG_ZINT} WriteLn(Format('Now using method %d', [encoding_method]));{$ENDIF}
    end;
  end;

  case encoding_method of { Encoding method - Table 10 }
    1: begin concat(binary_string, '1XX'); read_posn := 16; end;
    2: begin concat(binary_string, '00XX'); read_posn := 0; end;
    3: begin concat(binary_string, '0100'); read_posn := strlen(source); end;
    4: begin concat(binary_string, '0101'); read_posn := strlen(source); end;
    5: begin concat(binary_string, '01100XX'); read_posn := 20; end;
    6: begin concat(binary_string, '01101XX'); read_posn := 23; end;
    7: begin concat(binary_string, '0111000'); read_posn := strlen(source); end;
    8: begin concat(binary_string, '0111001'); read_posn := strlen(source); end;
    9: begin concat(binary_string, '0111010'); read_posn := strlen(source); end;
    10: begin concat(binary_string, '0111011'); read_posn := strlen(source); end;
    11: begin concat(binary_string, '0111100'); read_posn := strlen(source); end;
    12: begin concat(binary_string, '0111101'); read_posn := strlen(source); end;
    13: begin concat(binary_string, '0111110'); read_posn := strlen(source); end;
    14: begin concat(binary_string, '0111111'); read_posn := strlen(source); end;
  end;
  {$IFDEF DEBUG_ZINT} WriteLn(Format('Setting binary := %s', [ArrayOfCharToString(binary_string)]));{$ENDIF}

  { Variable _length symbol bit field is just given a place holder (XX)
  for the time being }

  { Verify that the data to be placed in the compressed data field is all
  numeric data before carrying out compression }
  for i := 0 to read_posn - 1 do
  begin
    if ((source[i] < '0') or (source[i] > '9')) then
    begin
      if ((source[i] <> '[') and (source[i] <> ']')) then
      begin
        { Something is wrong }
        strcpy(symbol.errtxt, 'Invalid characters in input data');
        result := ZERROR_INVALID_DATA; exit;
      end;
    end;
  end;

  { Now encode the compressed data field }

  {$IFDEF DEBUG_ZINT} WriteLn('Proceeding to encode data');{$ENDIF}
  if (encoding_method = 1) then
  begin
    { Encoding method field '1' - general item identification data }
    SetLength(group, 4);

    group[0] := source[2];
    group[1] := #0;
    group_val := StrToInt(ArrayOfCharToString(group));

    mask := $08;
    for j := 0 to 3 do
    begin
      if (group_val and mask) <> 0 then
        concat(binary_string, '1')
      else
        concat(binary_string, '0');
      mask := mask shr 1;
    end;

    for i := 1 to 4 do
    begin
      group[0] := source[(i * 3)];
      group[1] := source[(i * 3) + 1];
      group[2] := source[(i * 3) + 2];
      group[3] := #0;
      group_val := StrToInt(ArrayOfCharToString(group));

      mask := $200;
      for j := 0 to 9 do
      begin
        if (group_val and mask) <> 0 then
          concat(binary_string, '1')
        else
          concat(binary_string, '0');
        mask := mask shr 1;
      end;
    end;
  end;


  if (encoding_method = 3) then
  begin
    { Encoding method field '0100' - variable weight item
    (0,001 kilogram icrements) }
    SetLength(group, 4);
    SetLength(weight_str, 7);

    for i := 1 to 4 do
    begin
      group[0] := source[(i * 3)];
      group[1] := source[(i * 3) + 1];
      group[2] := source[(i * 3) + 2];
      group[3] := #0;
      group_val := StrToInt(ArrayOfCharToString(group));

      mask := $200;
      for j := 0 to 9 do
      begin
        if (group_val and mask) <> 0 then
          concat(binary_string, '1')
        else
          concat(binary_string, '0');
        mask := mask shr 1;
      end;
    end;

    for i := 0 to 5 do
    begin
      weight_str[i] := source[20 + i];
    end;
    weight_str[6] := #0;
    group_val := StrToInt(ArrayOfCharToString(weight_str));

    mask := $4000;
    for j := 0 to 14 do
    begin
      if (group_val and mask) <> 0 then
        concat(binary_string, '1')
      else
        concat(binary_string, '0');
      mask := mask shr 1;
    end;
  end;

  if (encoding_method = 4) then
  begin
    { Encoding method field '0101' - variable weight item (0,01 or
    0,001 pound increment) }
    SetLength(group, 4);
    SetLength(weight_str, 7);

    for i := 1 to 4 do
    begin
      group[0] := source[(i * 3)];
      group[1] := source[(i * 3) + 1];
      group[2] := source[(i * 3) + 2];
      group[3] := #0;
      group_val := StrToInt(ArrayOfCharToString(group));

      mask := $200;
      for j := 0 to 9 do
      begin
        if (group_val and mask) <> 0 then
          concat(binary_string, '1')
        else
          concat(binary_string, '0');
        mask := mask shr 1;
      end;
    end;

    for i := 0 to 5 do
      weight_str[i] := source[20 + i];
    weight_str[6] := #0;
    group_val := StrToInt(ArrayOfCharToString(weight_str));

    if (source[19] = '3') then
      group_val := group_val + 10000;

    mask := $4000;
    for j := 0 to 14 do
    begin
      if (group_val and mask) <> 0 then
        concat(binary_string, '1')
      else
        concat(binary_string, '0');
      mask := mask shr 1;
    end;
  end;


  if ((encoding_method >= 7) and (encoding_method <= 14)) then
  begin
    { Encoding method fields '0111000' through '0111111' - variable
    weight item plus date }
    SetLength(group, 4);
    SetLength(weight_str, 8);
    SetLength(date_str, 4);

    for i := 1 to 4 do
    begin
      group[0] := source[(i * 3)];
      group[1] := source[(i * 3) + 1];
      group[2] := source[(i * 3) + 2];
      group[3] := #0;
      group_val := StrToInt(ArrayOfCharToString(group));

      mask := $200;
      for j := 0 to 9 do
      begin
        if (group_val and mask) <> 0 then
          concat(binary_string, '1')
        else
          concat(binary_string, '0');
        mask := mask shr 1;
      end;
    end;

    weight_str[0] := source[19];

    for i := 0 to 4 do
      weight_str[i + 1] := source[21 + i];
    weight_str[6] := #0;
    group_val := StrToInt(ArrayOfCharToString(weight_str));

    mask := $80000;
    for j := 0 to 19 do
    begin
      if (group_val and mask) <> 0 then
        concat(binary_string, '1')
      else
        concat(binary_string, '0');
      mask := mask shr 1;
    end;

    if (strlen(source) = 34) then
    begin
      { Date information is included }
      date_str[0] := source[28];
      date_str[1] := source[29];
      date_str[2] := #0;
      group_val := StrToInt(ArrayOfCharToString(date_str)) * 384;

      date_str[0] := source[30];
      date_str[1] := source[31];
      Inc(group_val, (StrToInt(ArrayOfCharToString(date_str)) - 1) * 32);

      date_str[0] := source[32];
      date_str[1] := source[33];
      Inc(group_val, StrToInt(ArrayOfCharToString(date_str)));
    end
    else
      group_val := 38400;

    mask := $8000;
    for j := 0 to 15 do
    begin
      if (group_val and mask) <> 0 then
        concat(binary_string, '1')
      else
        concat(binary_string, '0');
      mask := mask shr 1;
    end;

  end;

  if (encoding_method = 5) then
  begin
    { Encoding method field '01100' - variable measure item and price }
    SetLength(group, 4);

    for i := 1 to 4 do
    begin
      group[0] := source[(i * 3)];
      group[1] := source[(i * 3) + 1];
      group[2] := source[(i * 3) + 2];
      group[3] := #0;
      group_val := StrToInt(ArrayOfCharToString(group));

      mask := $200;
      for j := 0 to 9 do
      begin
        if (group_val and mask) <> 0 then
          concat(binary_string, '1')
        else
          concat(binary_string, '0');
        mask := mask shr 1;
      end;
    end;

    case source[19] of
      '0': concat(binary_string, '00');
      '1': concat(binary_string, '01');
      '2': concat(binary_string, '10');
      '3': concat(binary_string, '11');
    end;
  end;

  if (encoding_method = 6) then
  begin
    { Encoding method '01101' - variable measure item and price with ISO 4217
    Currency Code }

    SetLength(group, 4);
    SetLength(currency_str, 5);

    for i := 1 to 4 do
    begin
      group[0] := source[(i * 3)];
      group[1] := source[(i * 3) + 1];
      group[2] := source[(i * 3) + 2];
      group[3] := #0;
      group_val := StrToInt(ArrayOfCharToString(group));

      mask := $200;
      for j := 0 to 9 do
      begin
        if (group_val and mask) <> 0 then
          concat(binary_string, '1')
        else
          concat(binary_string, '0');
        mask := mask shr 1;
      end;
    end;

    case source[19] of
      '0': concat(binary_string, '00');
      '1': concat(binary_string, '01');
      '2': concat(binary_string, '10');
      '3': concat(binary_string, '11');
    end;

    for i := 0 to 2 do
      currency_str[i] := source[20 + i];
    currency_str[3] := #0;
    group_val := StrToInt(ArrayOfCharToString(currency_str));

    mask := $200;
    for j := 0 to 9 do
    begin
      if (group_val and mask) <> 0 then
        concat(binary_string, '1')
      else
        concat(binary_string, '0');
      mask := mask shr 1;
    end;
  end;

  { The compressed data field has been processed if appropriate - the
  rest of the data (if any) goes into a general-purpose data compaction field }

  j := 0;
  for i := read_posn to strlen(source) - 1 do
  begin
    general_field[j] := source[i];
    Inc(j);
  end;
  general_field[j] := #0;
  {$IFDEF DEBUG_ZINT} WriteLn(Format('General field data = %s', [ArrayOfCharToString(general_field)]));{$ENDIF}

  latch := 0;
  for i := 0 to strlen(general_field) - 1 do
  begin
    { Table 13 - ISO/IEC 646 encodation }
    if ((general_field[i] < ' ') or (general_field[i] > 'z')) then
    begin
      general_field_type[i] := Chr(INVALID_CHAR); latch := 1;
    end
    else
      general_field_type[i] := Chr(ISOIEC);

    if (general_field[i] = '#') then
    begin
      general_field_type[i] := Chr(INVALID_CHAR); latch := 1;
    end;
    if (general_field[i] = '$') then
    begin
      general_field_type[i] := Chr(INVALID_CHAR); latch := 1;
    end;
    if (general_field[i] = '@') then
    begin
      general_field_type[i] := Chr(INVALID_CHAR); latch := 1;
    end;
    if (general_field[i] = Chr(92)) then
    begin
      general_field_type[i] := Chr(INVALID_CHAR); latch := 1;
    end;
    if (general_field[i] = '^') then
    begin
      general_field_type[i] := Chr(INVALID_CHAR); latch := 1;
    end;
    if (general_field[i] = Chr(96)) then
    begin
      general_field_type[i] := Chr(INVALID_CHAR); latch := 1;
    end;

    { Table 12 - Alphanumeric encodation }
    if ((general_field[i] >= 'A') and (general_field[i] <= 'Z')) then
      general_field_type[i] := Chr(ALPHA_OR_ISO);

    if (general_field[i] = '*') then
      general_field_type[i] := Chr(ALPHA_OR_ISO);

    if (general_field[i] = ',') then
      general_field_type[i] := Chr(ALPHA_OR_ISO);

    if (general_field[i] = '-') then
      general_field_type[i] := Chr(ALPHA_OR_ISO);

    if (general_field[i] = '.') then
      general_field_type[i] := Chr(ALPHA_OR_ISO);

    if (general_field[i] = '/') then
      general_field_type[i] := Chr(ALPHA_OR_ISO);

    { Numeric encodation }
    if ((general_field[i] >= '0') and (general_field[i] <= '9')) then
      general_field_type[i] := Chr(ANY_ENC);

    if (general_field[i] = '[') then
      { FNC1 can be encoded in any system }
      general_field_type[i] := Chr(ANY_ENC);

  end;

  general_field_type[strlen(general_field)] := #0;
  {$IFDEF DEBUG_ZINT} WriteLn(Format('General field type: %s', [ArrayOfCharToString(general_field_type)]));{$ENDIF}

  if (latch = 1) then
  begin
    { Invalid characters in input data }
    strcpy(symbol.errtxt, 'Invalid characters in input data');
    result := ZERROR_INVALID_DATA; exit;
  end;

  for i := 0 to strlen(general_field) - 1 do
  begin
    if ((general_field_type[i] = Chr(ISOIEC)) and (general_field[i + 1] = '[')) then
      general_field_type[i + 1] := Chr(ISOIEC);
  end;

  for i := 0 to strlen(general_field) - 1 do
  begin
    if ((general_field_type[i] = Chr(ALPHA_OR_ISO)) and (general_field[i + 1] = '[')) then
      general_field_type[i + 1] := Chr(ALPHA_OR_ISO);
  end;

  latch := general_rules(general_field, general_field_type);
  {$IFDEF DEBUG_ZINT} WriteLn(Format('General field type: %s', [ArrayOfCharToString(general_field_type)]));{$ENDIF}

  last_mode := NUMERIC;

  { Set initial mode if not NUMERIC }
  if (general_field_type[0] = Chr(ALPHA)) then
  begin
    concat(binary_string, '0000'); { Alphanumeric latch }
    last_mode := ALPHA;
  end;
  if (general_field_type[0] = Chr(ISOIEC)) then
  begin
    concat(binary_string, '0000'); { Alphanumeric latch }
    concat(binary_string, '00100'); { ISO/IEC 646 latch }
    last_mode := ISOIEC;
  end;

  i := 0;
  repeat
    {$IFDEF DEBUG_ZINT} Write(Format('Processing character %d ', [i]));{$ENDIF}
    case Ord(general_field_type[i]) of
      NUMERIC:
      begin
        {$IFDEF DEBUG_ZINT} Write('as NUMERIC:');{$ENDIF}

        if (last_mode <> NUMERIC) then
        begin
          concat(binary_string, '000'); { Numeric latch }
          {$IFDEF DEBUG_ZINT} WriteLn('<NUMERIC LATCH>');{$ENDIF}
        end;

        {$IFDEF DEBUG_ZINT}Write(Format('  %s%s > ', [general_field[i], general_field[i + 1]]));{$ENDIF}
        if (general_field[i] <> '[') then
          d1 := ctoi(general_field[i])
        else
          d1 := 10;

        if (general_field[i + 1] <> '[') then
          d2 := ctoi(general_field[i + 1])
        else
          d2 := 10;

        value := (11 * d1) + d2 + 8;

        mask := $40;
        for j := 0 to 6 do
        begin
          if (value and mask) <> 0 then
            concat(binary_string, '1')
          else
            concat(binary_string, '0');
          {$IFDEF DEBUG_ZINT}if (value and mask) <> 0 then Write('1') else Write('0');{$ENDIF}
          mask := mask shr 1;
        end;

        Inc(i, 2);
        {$IFDEF DEBUG_ZINT} WriteLn;{$ENDIF}
        last_mode := NUMERIC;
      end;

      ALPHA:
      begin
        {$IFDEF DEBUG_ZINT} WriteLn('as ALPHA');{$ENDIF}
        if (i <> 0) then
        begin
          if (last_mode = NUMERIC) then
            concat(binary_string, '0000'); { Alphanumeric latch }

          if (last_mode = ISOIEC) then
            concat(binary_string, '00100'); { Alphanumeric latch }
        end;

        if ((general_field[i] >= '0') and (general_field[i] <= '9')) then
        begin
          value := Ord(general_field[i]) - 43;

          mask := $10;
          for j := 0 to 4 do
          begin
            if (value and mask) <> 0 then
              concat(binary_string, '1')
            else
             concat(binary_string, '0');
            mask := mask shr 1;
          end;
        end;

        if ((general_field[i] >= 'A') and (general_field[i] <= 'Z')) then
        begin
          value := Ord(general_field[i]) - 33;

          mask := $20;
          for j := 0 to 5 do
          begin
            if (value and mask) <> 0 then
              concat(binary_string, '1')
            else
              concat(binary_string, '0');
            mask := mask shr 1;
          end;
        end;

        last_mode := ALPHA;
        if (general_field[i] = '[') then begin concat(binary_string, '01111'); last_mode := NUMERIC; end; { FNC1/Numeric latch }
        if (general_field[i] = '*') then concat(binary_string, '111010'); { asterisk }
        if (general_field[i] = ',') then concat(binary_string, '111011'); { comma }
        if (general_field[i] = '-') then concat(binary_string, '111100'); { minus or hyphen }
        if (general_field[i] = '.') then concat(binary_string, '111101'); { period or full stop }
        if (general_field[i] = '/') then concat(binary_string, '111110'); { slash or solidus }

        Inc(i);
      end;

      ISOIEC:
      begin
        {$IFDEF DEBUG_ZINT} WriteLn('as ISOIEC');{$ENDIF}
        if (i <> 0) then
        begin
          if (last_mode = NUMERIC) then
          begin
            concat(binary_string, '0000'); { Alphanumeric latch }
            concat(binary_string, '00100'); { ISO/IEC 646 latch }
          end;
          if (last_mode = ALPHA) then
            concat(binary_string, '00100'); { ISO/IEC 646 latch }
        end;

        if ((general_field[i] >= '0') and (general_field[i] <= '9')) then
        begin
          value := Ord(general_field[i]) - 43;

          mask := $10;
          for j := 0 to 4 do
          begin
            if (value and mask) <> 0 then
              concat(binary_string, '1')
            else
              concat(binary_string, '0');
            mask := mask shr 1;
          end;
        end;

        if ((general_field[i] >= 'A') and (general_field[i] <= 'Z')) then
        begin
          value := Ord(general_field[i]) - 1;

          mask := $40;
          for j := 0 to 6 do
          begin
            if (value and mask) <> 0 then
              concat(binary_string, '1')
            else
              concat(binary_string, '0');
            mask := mask shr 1;
          end;
        end;

        if ((general_field[i] >= 'a') and (general_field[i] <= 'z')) then
        begin
          value := Ord(general_field[i]) - 7;

          mask := $40;
          for j := 0 to 6 do
          begin
            if (value and mask) <> 0 then
              concat(binary_string, '1')
            else
              concat(binary_string, '0');
            mask := mask shr 1;
          end;
        end;

        last_mode := ISOIEC;
        if (general_field[i] = '[') then begin concat(binary_string, '01111'); last_mode := NUMERIC; end; { FNC1/Numeric latch }
        if (general_field[i] = '!') then concat(binary_string, '11101000'); { exclamation mark }
        if (general_field[i] = Chr(34)) then concat(binary_string, '11101001'); { quotation mark }
        if (general_field[i] = Chr(37)) then concat(binary_string, '11101010'); { percent sign }
        if (general_field[i] = '&') then concat(binary_string, '11101011'); { ampersand }
        if (general_field[i] = Chr(39)) then concat(binary_string, '11101100'); { apostrophe }
        if (general_field[i] = '(') then concat(binary_string, '11101101'); { left parenthesis }
        if (general_field[i] = ')') then concat(binary_string, '11101110'); { right parenthesis }
        if (general_field[i] = '*') then concat(binary_string, '11101111'); { asterisk }
        if (general_field[i] = '+') then concat(binary_string, '11110000'); { plus sign }
        if (general_field[i] = ',') then concat(binary_string, '11110001'); { comma }
        if (general_field[i] = '-') then concat(binary_string, '11110010'); { minus or hyphen }
        if (general_field[i] = '.') then concat(binary_string, '11110011'); { period or full stop }
        if (general_field[i] = '/') then concat(binary_string, '11110100'); { slash or solidus }
        if (general_field[i] = ':') then concat(binary_string, '11110101'); { colon }
        if (general_field[i] = ';') then concat(binary_string, '11110110'); { semicolon }
        if (general_field[i] = '<') then concat(binary_string, '11110111'); { less-than sign }
        if (general_field[i] = '=') then concat(binary_string, '11111000'); { equals sign }
        if (general_field[i] = '>') then concat(binary_string, '11111001'); { greater-than sign }
        if (general_field[i] = '?') then concat(binary_string, '11111010'); { question mark }
        if (general_field[i] = '_') then concat(binary_string, '11111011'); { underline or low line }
        if (general_field[i] = ' ') then concat(binary_string, '11111100'); { space }

        Inc(i);
      end;
    end;
  until not (i + latch < strlen(general_field));
  {$IFDEF DEBUG_ZINT} WriteLn(Format('Resultant binary := %s', [ArrayOfCharToString(binary_string)]));{$ENDIF}
  {$IFDEF DEBUG_ZINT} WriteLn(Format('  length: %d', [strlen(binary_string)]));{$ENDIF}

  remainder := 12 - (strlen(binary_string) mod 12);
  if (remainder = 12) then remainder := 0;
  if (strlen(binary_string) < 36) then remainder := 36 - strlen(binary_string);

  if (latch = 1) then
  begin
    { There is still one more numeric digit to encode }
    {$IFDEF DEBUG_ZINT} WriteLn('Adding extra (odd) numeric digit');{$ENDIF}

    if (last_mode = NUMERIC) then
    begin
      if ((remainder >= 4) and (remainder <= 6)) then
      begin
        value := ctoi(general_field[i]);
        Inc(value);

        mask := $08;
        for j := 0 to 3 do
        begin
          if (value and mask) <> 0 then
            concat(binary_string, '1')
          else
            concat(binary_string, '0');
          mask := mask shr 1;
        end;
      end
      else
      begin
        d1 := ctoi(general_field[i]);
        d2 := 10;

        value := (11 * d1) + d2 + 8;

        mask := $40;
        for j := 0 to 6 do
        begin
          if (value and mask) <> 0 then
            concat(binary_string, '1')
          else
            concat(binary_string, '0');
          mask := mask shr 1;
        end;
      end;
    end
    else
    begin
      value := Ord(general_field[i]) - 43;

      mask := $10;
      for j := 0 to 4 do
      begin
        if (value and mask) <> 0 then
            concat(binary_string, '1')
          else
            concat(binary_string, '0');
        mask := mask shr 1;
      end;
    end;

    remainder := 12 - (strlen(binary_string) mod 12);
    if (remainder = 12) then remainder := 0;
    if (strlen(binary_string) < 36) then remainder := 36 - strlen(binary_string);
    {$IFDEF DEBUG_ZINT} WriteLn(Format('Resultant binary := %s', [ArrayOfCharToString(binary_string)]));{$ENDIF}
    {$IFDEF DEBUG_ZINT} WriteLn(Format('  length: %d', [strlen(binary_string)]));{$ENDIF}
  end;

  if (strlen(binary_string) > 252) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  { Now add padding to binary string (7.2.5.5.4) }
  i := remainder;
  if ((strlen(general_field) <> 0) and (last_mode = NUMERIC)) then
  begin
    strcpy(padstring, '0000');
    Dec(i, 4);
  end
  else
    strcpy(padstring, '');
  while i > 0 do
  begin
    concat(padstring, '00100');
    Dec(i, 5);
  end;

  padstring[remainder] := #0;
  concat(binary_string, padstring);

  { Patch variable _length symbol bit field }
  d1 := ((strlen(binary_string) div 12) + 1) and 1;
  if (strlen(binary_string) <= 156) then d2 := 0 else d2 := 1;

  if (encoding_method = 1) then
  begin
    if d1 <> 0 then binary_string[2] := '1' else binary_string[2] := '0';
    if d2 <> 0 then binary_string[3] := '1' else binary_string[3] := '0';
  end;
  if (encoding_method = 2) then
  begin
    if d1 <> 0 then binary_string[3] := '1' else binary_string[3] := '0';
    if d2 <> 0 then binary_string[4] := '1' else binary_string[4] := '0';
  end;
  if ((encoding_method = 5) or (encoding_method = 6)) then
  begin
    if d1 <> 0 then binary_string[6] := '1' else binary_string[6] := '0';
    if d2 <> 0 then binary_string[7] := '1' else binary_string[7] := '0';
  end;
  {$IFDEF DEBUG_ZINT} WriteLn(Format('Resultant binary := %s', [ArrayOfCharToString(binary_string)]));{$ENDIF}
  {$IFDEF DEBUG_ZINT} WriteLn(Format('  length: %d', [strlen(binary_string)]));{$ENDIF}
  result := 0; exit;
end;

{ GS1 DataBar Expanded }
function rssexpanded(symbol : zint_symbol; const source : TArrayOfByte; src_len : Integer) : Integer;
var
  i, j, k, l, data_chars : Integer;
  vs, group, v_odd, v_even : TArrayOfInteger;
  substring : array[0..20] of array[0..13] of Char;
  latch : Char;
  char_widths : array[0..20] of array[0..7] of Integer;
  checksum : Integer;
  check_widths : TArrayOfInteger;
  c_group : Integer;
  check_char, c_odd, c_even : Integer;
  elements : TArrayOfInteger;
  pattern_width, reader, writer : Integer;
  row, elements_in_sub, special_case_row, left_to_right : Integer;
  codeblocks : Integer;
  sub_elements : TArrayOfInteger;
  stack_rows, current_row, current_block : Integer;
  separator_row : Integer;
  reduced, binary_string : TArrayOfChar;
  widths : TArrayOfInteger;
begin
  SetLength(vs, 21);
  SetLength(group, 21);
  SetLength(v_odd, 21);
  SetLength(v_even, 21);
  SetLength(check_widths, 8);
  SetLength(elements, 235);
  SetLength(sub_elements, 235);
  SetLength(reduced, src_len + 1);
  SetLength(binary_string, 7 * (src_len + 1));

  separator_row := 0;
  reader := 0;

  if (symbol.input_mode <> GS1_MODE) then
  begin
    { GS1 data has not been verified yet }
    i := gs1_verify(symbol, source, src_len, reduced);
    if (i <> 0) then begin result := i; exit; end;
  end;

  if ((symbol.symbology = BARCODE_RSS_EXP_CC) or (symbol.symbology = BARCODE_RSS_EXPSTACK_CC)) then
  begin
    { make space for a composite separator pattern }
    separator_row := symbol.rows;
    symbol.row_height[separator_row] := 1;
    Inc(symbol.rows, 1);
  end;

  strcpy(binary_string, '');

  if (symbol.option_1 = 2) then
    concat(binary_string, '1')
  else
    concat(binary_string, '0');

  i := rss_binary_string(symbol, reduced, binary_string);
  if (i <> 0) then
  begin
    result := i; exit;
  end;

  data_chars := strlen(binary_string) div 12;

  for i := 0 to data_chars - 1 do
  begin
    for j := 0 to 11 do
      substring[i][j] := binary_string[(i * 12) + j];
    substring[i][12] := #0;
  end;

  for i := 0 to data_chars - 1 do
  begin
    vs[i] := 0;
    if (substring[i][0] = '1') then Inc(vs[i], 2048);
    if (substring[i][1] = '1') then Inc(vs[i], 1024);
    if (substring[i][2] = '1') then Inc(vs[i], 512);
    if (substring[i][3] = '1') then Inc(vs[i], 256);
    if (substring[i][4] = '1') then Inc(vs[i], 128);
    if (substring[i][5] = '1') then Inc(vs[i], 64);
    if (substring[i][6] = '1') then Inc(vs[i], 32);
    if (substring[i][7] = '1') then Inc(vs[i], 16);
    if (substring[i][8] = '1') then Inc(vs[i], 8);
    if (substring[i][9] = '1') then Inc(vs[i], 4);
    if (substring[i][10] = '1') then Inc(vs[i], 2);
    if (substring[i][11] = '1') then Inc(vs[i], 1);
  end;

  for i := 0 to data_chars - 1 do
  begin
    if (vs[i] <= 347) then group[i] := 1;
    if ((vs[i] >= 348) and (vs[i] <= 1387)) then group[i] := 2;
    if ((vs[i] >= 1388) and (vs[i] <= 2947)) then group[i] := 3;
    if ((vs[i] >= 2948) and (vs[i] <= 3987)) then group[i] := 4;
    if (vs[i] >= 3988) then group[i] := 5;
    v_odd[i] := (vs[i] - g_sum_exp[group[i] - 1]) div t_even_exp[group[i] - 1];
    v_even[i] := (vs[i] - g_sum_exp[group[i] - 1]) mod t_even_exp[group[i] - 1];

    getRSSwidths(v_odd[i], modules_odd_exp[group[i] - 1], 4, widest_odd_exp[group[i] - 1], 0, widths);
    char_widths[i][0] := widths[0];
    char_widths[i][2] := widths[1];
    char_widths[i][4] := widths[2];
    char_widths[i][6] := widths[3];
    getRSSwidths(v_even[i], modules_even_exp[group[i] - 1], 4, widest_even_exp[group[i] - 1], 1, widths);
    char_widths[i][1] := widths[0];
    char_widths[i][3] := widths[1];
    char_widths[i][5] := widths[2];
    char_widths[i][7] := widths[3];
  end;

  { 7.2.6 Check character }
  { The checksum value is equal to the mod 211 residue of the weighted sum of the widths of the
     elements in the data characters. }
  checksum := 0;
  for i := 0 to data_chars - 1 do
  begin
    row := weight_rows[(((data_chars - 2) div 2) * 21) + i];
    for j := 0 to 7 do
      Inc(checksum, (char_widths[i][j] * checksum_weight_exp[(row * 8) + j]));
  end;

  check_char := (211 * ((data_chars + 1) - 4)) + (checksum mod 211);

  c_group := 1;
  if (check_char <= 347) then c_group := 1;
  if ((check_char >= 348) and (check_char <= 1387)) then c_group := 2;
  if ((check_char >= 1388) and (check_char <= 2947)) then c_group := 3;
  if ((check_char >= 2948) and (check_char <= 3987)) then c_group := 4;
  if (check_char >= 3988) then c_group := 5;

  c_odd := (check_char - g_sum_exp[c_group - 1]) div t_even_exp[c_group - 1];
  c_even := (check_char - g_sum_exp[c_group - 1]) mod t_even_exp[c_group - 1];

  getRSSwidths(c_odd, modules_odd_exp[c_group - 1], 4, widest_odd_exp[c_group - 1], 0, widths);
  check_widths[0] := widths[0];
  check_widths[2] := widths[1];
  check_widths[4] := widths[2];
  check_widths[6] := widths[3];
  getRSSwidths(c_even, modules_even_exp[c_group - 1], 4, widest_even_exp[c_group - 1], 1, widths);
  check_widths[1] := widths[0];
  check_widths[3] := widths[1];
  check_widths[5] := widths[2];
  check_widths[7] := widths[3];

  { Initialise element array }
  pattern_width := ((((data_chars + 1) div 2) + ((data_chars + 1) and 1)) * 5) + ((data_chars + 1) * 8) + 4;
  for i := 0 to pattern_width - 1 do
    elements[i] := 0;

  elements[0] := 1;
  elements[1] := 1;
  elements[pattern_width - 2] := 1;
  elements[pattern_width - 1] := 1;

  { Put finder patterns in element array }
  for i := 0 to (((data_chars + 1) div 2) + ((data_chars + 1) and 1)) - 1 do
  begin
    k := ((((((data_chars + 1) - 2) div 2) + ((data_chars + 1) and 1)) - 1) * 11) + i;
    for j := 0 to 4  do
      elements[(21 * i) + j + 10] := finder_pattern_exp[((finder_sequence[k] - 1) * 5) + j];
  end;

  { Put check character in element array }
  for i := 0 to 7 do
    elements[i + 2] := check_widths[i];

  { Put forward reading data characters in element array }
  i := 1;
  while i < data_chars do
  begin
    for j := 0 to 7 do
      elements[(((i - 1) div 2) * 21) + 23 + j] := char_widths[i][j];
    Inc(i, 2);
  end;

  { Put reversed data characters in element array }
  i := 0;
  while i < data_chars do
  begin
    for j := 0 to 7 do
      elements[((i div 2) * 21) + 15 + j] := char_widths[i][7 - j];
    Inc(i, 2);
  end;

  if ((symbol.symbology = BARCODE_RSS_EXP) or (symbol.symbology = BARCODE_RSS_EXP_CC)) then
  begin
    { Copy elements into symbol }
    writer := 0;
    latch := '0';
    for i := 0 to pattern_width - 1 do
    begin
      for j := 0 to elements[i] - 1 do
      begin
        if (latch = '1') then set_module(symbol, symbol.rows, writer) else unset_module(symbol, symbol.rows, writer);
        Inc(writer);
      end;
      if (latch = '1') then
        latch := '0'
      else
        latch := '1';
    end;
    if (symbol.width < writer) then symbol.width := writer;
    symbol.rows := symbol.rows + 1;
    if (symbol.symbology = BARCODE_RSS_EXP_CC) then
    begin
      for j := 4 to (symbol.width - 4) - 1 do
      begin
        if (module_is_set(symbol, separator_row + 1, j) <> 0) then
          unset_module(symbol, separator_row, j)
        else
          set_module(symbol, separator_row, j);
      end;
      { finder bar adjustment }
      for j := 0 to (writer div 49) - 1 do
      begin
        k := (49 * j) + 18;
        for i := 0 to 14 do
        begin
          if ((not (module_is_set(symbol, separator_row + 1, i + k - 1) <> 0)) and
          (not (module_is_set(symbol, separator_row + 1, i + k) <> 0)) and
          (module_is_set(symbol, separator_row, i + k - 1) <> 0)) then
            unset_module(symbol, separator_row, i + k);
        end;
      end;
    end;

    { Add human readable text }
    for i := 0 to src_len do
    begin
      if ((source[i] <> Ord('[')) and (source[i] <> Ord(']'))) then
      begin
        symbol.text[i] := source[i];
      end
      else
      begin
        if (source[i] = Ord('[')) then
          symbol.text[i] := Ord('(');

        if (source[i] = Ord(']')) then
          symbol.text[i] := Ord(')');
      end;
    end;

  end
  else
  begin
    { RSS Expanded Stacked }
    codeblocks := (data_chars + 1) div 2;

    if ((symbol.option_2 < 1) or (symbol.option_2 > 10)) then
      symbol.option_2 := 2;

    if ((symbol.option_1 = 2) and (symbol.option_2 = 1)) then
    begin
      { 'There shall be a minimum of four symbol characters in the
      first row of an RSS Expanded Stacked symbol when it is the linear
      component of an EAN.UCC Composite symbol.' }
      symbol.option_2 := 2;
    end;

    stack_rows := codeblocks div symbol.option_2;
    if (codeblocks mod symbol.option_2 > 0) then
      Inc(stack_rows);

    current_block := 0;
    for current_row := 1 to stack_rows do
    begin
      for i := 0 to 234 do
        sub_elements[i] := 0;
      special_case_row := 0;

      { Row Start }
      sub_elements[0] := 1;
      sub_elements[1] := 1;
      elements_in_sub := 2;

      { Row Data }
      reader := 0;
      repeat
        if ((((symbol.option_2 and 1) <> 0) or ((current_row and 1) <> 0)) or
          ((current_row = stack_rows) and (codeblocks <> (current_row * symbol.option_2)) and
          ((((current_row * symbol.option_2) - codeblocks) and 1) <> 0))) then
        begin
          { left to right }
          left_to_right := 1;
          i := 2 + (current_block * 21);
          for j := 0 to 20 do
          begin
            sub_elements[j + (reader * 21) + 2] := elements[i + j];
            Inc(elements_in_sub);
          end;
        end
        else
        begin
          { right to left }
          left_to_right := 0;
          if ((current_row * symbol.option_2) < codeblocks) then
          begin
            { a full row }
            i := 2 + (((current_row * symbol.option_2) - reader - 1) * 21);
            for j := 0 to 20 do
            begin
              sub_elements[(20 - j) + (reader * 21) + 2] := elements[i + j];
              Inc(elements_in_sub);
            end;
          end
          else
          begin
            { a partial row }
            k := ((current_row * symbol.option_2) - codeblocks);
            l := (current_row * symbol.option_2) - reader - 1;
            i := 2 + ((l - k) * 21);
            for j := 0 to 20 do
            begin
              sub_elements[(20 - j) + (reader * 21) + 2] := elements[i + j];
              Inc(elements_in_sub);
            end;
          end;
        end;
        Inc(reader);
        Inc(current_block);
      until not ((reader < symbol.option_2) and (current_block < codeblocks));

      { Row Stop }
      sub_elements[elements_in_sub] := 1;
      sub_elements[elements_in_sub + 1] := 1;
      Inc(elements_in_sub, 2);

      if (current_row and 1) <> 0 then
        latch := '0'
      else
        latch := '1';

      if ((current_row = stack_rows) and (codeblocks <> (current_row * symbol.option_2)) and
        ((((current_row * symbol.option_2) - codeblocks) and 1) <> 0) ) then
      begin
        { Special bottom row }
        special_case_row := 1;
        sub_elements[0] := 2;
        latch := '0';
      end;

      writer := 0;
      for i := 0 to elements_in_sub - 1 do
      begin
        for j := 0 to sub_elements[i] - 1 do
        begin
          if (latch = '1') then set_module(symbol, symbol.rows, writer) else unset_module(symbol, symbol.rows, writer);
          Inc(writer);
        end;
        if (latch = '1') then
          latch := '0'
        else
          latch := '1';
      end;
      if (symbol.width < writer) then symbol.width := writer;

      if (current_row <> 1) then
      begin
        { middle separator pattern (above current row) }
        j := 5;
        while j < (49 * symbol.option_2) do
        begin
          set_module(symbol, symbol.rows - 2, j);
          Inc(j, 2);
        end;
        symbol.row_height[symbol.rows - 2] := 1;
        { bottom separator pattern (above current row) }
        for j := 4 to (writer - 4) - 1 do
        begin
          if (module_is_set(symbol, symbol.rows, j) <> 0) then
            unset_module(symbol, symbol.rows - 1, j)
          else
            set_module(symbol, symbol.rows - 1, j);
        end;
        symbol.row_height[symbol.rows - 1] := 1;
        { finder bar adjustment }
        for j := 0 to reader - 1 do
        begin
          if (special_case_row <> 0) then
            k := (49 * j) + 19
          else
            k := (49 * j) + 18;
          if (left_to_right <> 0) then
          begin
            for i := 0 to 14 do
            begin
              if ((not (module_is_set(symbol, symbol.rows, i + k - 1) <> 0)) and
              (not (module_is_set(symbol, symbol.rows, i + k) <> 0)) and
              (module_is_set(symbol, symbol.rows - 1, i + k - 1) <> 0)) then
                unset_module(symbol, symbol.rows - 1, i + k);
            end;
          end
          else
          begin
            for i := 14 downto 0 do
            begin
              if ((not (module_is_set(symbol, symbol.rows, i + k + 1) <> 0)) and
              (not (module_is_set(symbol, symbol.rows, i + k) <> 0)) and
              (module_is_set(symbol, symbol.rows - 1, i + k + 1) <> 0)) then
                unset_module(symbol, symbol.rows - 1, i + k);
            end;
          end;
        end;
      end;

      if (current_row <> stack_rows) then
      begin
        { top separator pattern (below current row) }
        for j := 4 to (writer - 4) - 1 do
        begin
          if (module_is_set(symbol, symbol.rows, j) <> 0) then
            unset_module(symbol, symbol.rows + 1, j)
          else
            set_module(symbol, symbol.rows + 1, j);
        end;
        symbol.row_height[symbol.rows + 1] := 1;
        { finder bar adjustment }
        for j := 0 to reader - 1 do
        begin
          k := (49 * j) + 18;
          if (left_to_right <> 0) then
          begin
            for i := 0 to 14 do
            begin
              if ((not (module_is_set(symbol, symbol.rows, i + k - 1) <> 0)) and
              (not (module_is_set(symbol, symbol.rows, i + k) <> 0)) and
              (module_is_set(symbol, symbol.rows + 1, i + k - 1) <> 0)) then
                unset_module(symbol, symbol.rows + 1, i + k);
            end;
          end
          else
          begin
            for i := 14 downto 0 do
            begin
              if ((not (module_is_set(symbol, symbol.rows, i + k + 1) <> 0)) and
              (not (module_is_set(symbol, symbol.rows, i + k) <> 0)) and
              (module_is_set(symbol, symbol.rows + 1, i + k + 1) <> 0)) then
                unset_module(symbol, symbol.rows + 1, i + k);
            end;
          end;
        end;
      end;

      symbol.rows := symbol.rows + 4;
    end;
    symbol.rows := symbol.rows - 3;
    if (symbol.symbology = BARCODE_RSS_EXPSTACK_CC) then
    begin
      for j := 4 to (symbol.width - 4) - 1 do
      begin
        if (module_is_set(symbol, separator_row + 1, j) <> 0) then
          unset_module(symbol, separator_row, j)
        else
          set_module(symbol, separator_row, j);
      end;
      { finder bar adjustment }
      for j := 0 to reader - 1 do
      begin
        k := (49 * j) + 18;
        for i := 0 to 14 do
        begin
          if ((not (module_is_set(symbol, separator_row + 1, i + k - 1) <> 0)) and
          (not (module_is_set(symbol, separator_row + 1, i + k) <> 0)) and
          (module_is_set(symbol, separator_row, i + k - 1) <> 0)) then
            unset_module(symbol, separator_row, i + k);
        end;
      end;
    end;

  end;

  result := 0;
end;

end.

