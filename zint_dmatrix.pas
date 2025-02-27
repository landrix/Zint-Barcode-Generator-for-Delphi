unit zint_dmatrix;

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

function dmatrix(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

  {fs 02/04/2018 added DMRE sizes}
  {fs 27/02/2025 added DMRE new sizes}
const
  NbOfSymbols = 48; {High(TdmSize)}


implementation

uses
  System.SysUtils, System.Math, zint_reedsol, zint_common;

const
  MAXBARCODE = 3116;

  DM_NULL = 0;
  DM_ASCII = 1;
  DM_C40 = 2;
  DM_TEXT = 3;
  DM_X12 = 4;
  DM_EDIFACT = 5;
  DM_BASE256 = 6;

c40_shift : array[0..127] of Integer = (
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 );

c40_value : array[0..127] of Integer = (
	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
	3,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,4,5,6,7,8,9,10,11,12,13,
	15,16,17,18,19,20,21,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
	22,23,24,25,26,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31 );

text_shift : array[0..127] of Integer = (
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	2, 2, 2, 2, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3 );

text_value : array[0..127] of Integer = (
	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
	3,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,4,5,6,7,8,9,10,11,12,13,
	15,16,17,18,19,20,21,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
	22,23,24,25,26,0,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,27,28,29,30,31 );



matrixbytes : array[0..NbOfSymbols - 1] of NativeInt = (
  {fs 02/04/2018 added DMRE sizes and adjusted against the C file}
//	3, 5, 5, 8, 10, 12, 16, 18, 22, 22, 30, 32, 36, 44, 49, 62, 86, 114, 144,
//	174, 204, 280, 368, 456, 576, 696, 816, 1050, 1304, 1558 );
    { 0}   3, { 10x10 }   5, { 12x12 }   5, {  8x18 }   8, { 14x14 }
    { 4}  10, {  8x32 }  12, { 16x16 }  16, { 12x26 }  18, { 18x18 }
    { 8}  18, {  8x48 }  22, { 20x20 }  22, { 12x36 }  24, {  8x64 }
    {12}  30, { 22x22 }  32, { 16x36 }  32, { 8x80 }   36, { 24x24 }
    {16}  38 {8x96},      43 {12x64},     44 {26x26},     44 {20x36},
    {20}  49 {16x48},     49 {8x120},     56 {20x44},     62 {32x32},
    {24}  62 {16x64},     63 {8x144},     64 {12x88},     70 {26x40},
    {28}  72 {22x48},     80 {24x48},     84 {20x64},     86 {36x36},
    {32}  90 {26x48},    108 {24x64},    114 {40x40},    118 {26x64},
    {36} 144 {44x44},    174 {48x48},    204 {52x52},    280 {64x64},
    {40} 368 {72x72},    456 {80x80},    576 {88x88},    696 {96x96},
    {44} 816 {104x104}, 1050 {120x120}, 1304 {132x132}, 1558 {144x144}
    );


{* Index into `dm_matrixbytes` array in `symbol->option_2` (CLI `--vers`) order,
   i.e. square symbols first, then standard rectangular, then DMRE.
   The bracketed comment value is the total data codewords value. *}

intsymbol : array[0..NbOfSymbols - 1] of NativeInt = (
//
//     0, {  1: 10x10 ,  3}  1, {  2: 12x12 ,  5}  3, {  3: 14x14 ,  8}  5, {  4: 16x16 , 12}
//     7, {  5: 18x18 , 18}  9, {  6: 20x20 , 22} 12, {  7: 22x22 , 30} 14, {  8: 24x24 , 36}
//    16, {  9: 26x26 , 44} 18, { 10: 32x32 , 62} 22, { 11: 36x36 , 86} 25, { 12: 40x40 ,114}
//    27, { 13: 44x44 ,144} 28, { 14: 48x48 ,174} 29, { 15: 52x52 ,204} 30, { 16: 64x64 ,280}
//    31, { 17: 72x72 ,368} 32, { 18: 80x80 ,456} 33, { 19: 88x88 ,576} 34, { 20: 96x96 ,696}
//    35, { 21:104x104,816} 36, { 22:120x120,1050}37, { 23:132x132,1304}38, { 24:144x144,1558}
//     2, { 25:  8x18 ,  5}  4, { 26:  8x32 , 10}  6, { 27: 12x26 , 16} 10, { 28: 12x36 , 22}
//    13, { 29: 16x36 , 32} 17, { 30: 16x48 , 49}  8, { 31:  8x48 , 18} 11, { 32:  8x64 , 24}
//    15, { 33: 12x64 , 43} 19, { 34: 16x64 , 62} 21, { 35: 24x48 , 80} 24, { 36: 24x64 ,108}
//    20, { 37: 26x40 , 70} 23, { 38: 26x48 , 90} 26 { 39: 26x64 ,118}
//    );

    { Standard DM square }
    {  1-4}  0 {10x10 (3)},      1 {12x12 (5)},       3 {14x14 (8)},       5 {16x16 (12)},
    {  5-8}  7 {18x18 (18)},     9 {20x20 (22)},     12 {22x22 (30)},     15 {24x24 (36)},
    { 9-12} 18 {26x26 (44)},    23 {32x32 (62)},     31 {36x36 (86)},     34 {40x40 (114)},
    {13-16} 36 {44x44 (144)},   37 {48x48 (174)},    38 {52x52 (204)},    39 {64x64 (280)},
    {17-20} 40 {72x72 (368)},   41 {80x80 (456)},    42 {88x88 (576)},    43 {96x96 (696)},
    {21-24} 44 {104x104 (816)}, 45 {120x120 (1050)}, 46 {132x132 (1304)}, 47 {144x144 (1558)},

    { Standard DM rectangular }
    {25-28}  2 {8x18 (5)},       4 {8x32 (10)},       6 {12x26 (16)},     10 {12x36 (22)},
    {29-30} 13 {16x36 (32)},    20 {16x48 (49)},

    { DMRE }
    {31-34}  8 {8x48 (18)},     11 {8x64 (24)},      14 {8x80 (32)},      16 {8x96 (38)},
    {35-38} 21 {8x120 (49)},    25 {8x144 (63)},     17 {12x64 (43)},     26 {12x88 (64)},
    {39-42} 24 {16x64 (62)},    19 {20x36 (44)},     22 {20x44 (56)},     30 {20x64 (84)},
    {43-46} 28 {22x48 (72)},    29 {24x48 (80)},     33 {24x64 (108)},    27 {26x40 (70)},
    {47-48} 32 {26x48 (90)},    35 {26x64 (118)}
  );

{* Following arrays in total data codewords order (`dm_matrixbytes`) *}




{* Whether the version is DMRE *}
{* This is the case, if intsymbol index >= 30 }
isDMRE : array[0..NbOfSymbols - 1] of Boolean = (
    { 0} False {10x10 (3)},     False {12x12 (5)},      False {8x18 (5)},       False {14x14 (8)},
    { 4} False {8x32 (10)},     False {16x16 (12)},     False {12x26 (16)},     False {18x18 (18)},
    { 8} True {8x48 (18)},      False {20x20 (22)},     False {12x36 (22)},     True {8x64 (24)},
    {12} False {22x22 (30)},    False {16x36 (32)},     True {8x80 (32)},       False {24x24 (36)},
    {16} True {8x96 (38)},      True {12x64 (43)},      False {26x26 (44)},     True {20x36 (44)},
    {20} False {16x48 (49)},    True {8x120 (49)},      True {20x44 (56)},      False {32x32 (62)},
    {24} True{16x64 (62)},      True {8x144 (63)},      True {12x88 (64)},      True {26x40 (70)},
    {28} True {22x48 (72)},     True {24x48 (80)},      True {20x64 (84)},      False {36x36 (86)},
    {32} True {26x48 (90)},     True {24x64 (108)},     False {40x40 (114)},    True {26x64 (118)},
    {36} False {44x44 (144)},   False {48x48 (174)},    False {52x52 (204)},    False {64x64 (280)},
    {40} False {72x72 (368)},   False {80x80 (456)},    False {88x88 (576)},    False {96x96 (696)},
    {44} False {104x104 (816)}, False {120x120 (1050)}, False {132x132 (1304)}, False {144x144 (1558)}
    );


  {fs 02/04/2018 Is the current code a rectangle code ?
  This is the case, if intsymbol index 25 >= < 30 }
isRectangle : array[0..NbOfSymbols - 1] of Boolean = (
    { 0} False {10x10 (3)},     False {12x12 (5)},      True {8x18 (5)},       False {14x14 (8)},
    { 4} True {8x32 (10)},      False {16x16 (12)},     True {12x26 (16)},     False {18x18 (18)},
    { 8} True {8x48 (18)},      False {20x20 (22)},     True {12x36 (22)},     True {8x64 (24)},
    {12} False {22x22 (30)},    True {16x36 (32)},     True {8x80 (32)},       False {24x24 (36)},
    {16} True {8x96 (38)},      True {12x64 (43)},      False {26x26 (44)},     True {20x36 (44)},
    {20} True {16x48 (49)},     True {8x120 (49)},      True {20x44 (56)},      False {32x32 (62)},
    {24} True{16x64 (62)},      True {8x144 (63)},      True {12x88 (64)},      True {26x40 (70)},
    {28} True {22x48 (72)},     True {24x48 (80)},      True {20x64 (84)},      False {36x36 (86)},
    {32} True {26x48 (90)},     True {24x64 (108)},     False {40x40 (114)},    True {26x64 (118)},
    {36} False {44x44 (144)},   False {48x48 (174)},    False {52x52 (204)},    False {64x64 (280)},
    {40} False {72x72 (368)},   False {80x80 (456)},    False {88x88 (576)},    False {96x96 (696)},
    {44} False {104x104 (816)}, False {120x120 (1050)}, False {132x132 (1304)}, False {144x144 (1558)}
    );


{* Horizontal matrix size *}
matrixH : array[0..NbOfSymbols - 1] of NativeInt = (
    { 0}  10 {10x10},    12 {12x12 },    8 {8x18},     14 {14x14},
    { 4}   8 {8x32},     16 {16x16},    12 {12x26},    18 {18x18},
    { 8}   8 {8x48},     20 {20x20},    12 {12x36},     8 {8x64},
    {12}  22 {22x22},    16 {16x36},     8 {8x80},     24 {24x24},
    {16}   8 {8x96},     12 {12x64},    26 {26x26},    20 {20x36},
    {20}  16 {16x48},     8 {8x120},    20 {20x44},    32 {32x32},
    {24}  16 {16x64},     8 {8x144},    12 {12x88},    26 {26x40},
    {28}  22 {22x48},    24 {24x48},    20 {20x64},    36 {36x36},
    {32}  26 {26x48},    24 {24x64},    40 {40x40},    26 {26x64},
    {36}  44 {44x44},    48 {48x48},    52 {52x52},    64 {64x64},
    {40}  72 {72x72},    80 {80x80},    88 {88x88},    96 {96x96},
    {44} 104 {104x104}, 120 {120x120}, 132 {132x132}, 144 {144x144}
    );


{* Vertical matrix sizes *}
matrixW : array[0..NbOfSymbols - 1] of NativeInt = (
    { 0}  10 {10x10},    12 {12x12},    18 {8x18},     14 {14x14},
    { 4}  32 {8x32},     16 {16x16},    26 {12x26},    18 {18x18},
    { 8}  48 {8x48},     20 {20x20},    36 {12x36},    64 {8x64},
    {12}  22 {22x22},    36 {16x36},    80 {8x80},     24 {24x24},
    {16}  96 {8x96},     64 {12x64},    26 {26x26},    36 {20x36},
    {20}  48 {16x48},   120 {8x120},    44 {20x44},    32 {32x32},
    {24}  64 {16x64},   144 {8x144},    88 {12x88},    40 {26x40},
    {28}  48 {22x48},    48 {24x48},    64 {20x64},    36 {36x36},
    {32}  48 {26x48},    64 {24x64},    40 {40x40},    64 {26x64},
    {36}  44 {44x44},    48 {48x48},    52 {52x52},    64 {64x64},
    {40}  72 {72x72},    80 {80x80},    88 {88x88},    96 {96x96},
    {44} 104 {104x104}, 120 {120x120}, 132 {132x132}, 144 {144x144}
    );


{* Horizontal submodule size (including subfinder) - see Table 7 Data region H + 2 *}
matrixFH : array[0..NbOfSymbols - 1] of NativeInt = (
    { 0} 10 {10x10},   12 {12x12},    8 {8x18},    14 {14x14},
    { 4}  8 {8x32},    16 {16x16},   12 {12x26},   18 {18x18},
    { 8}  8 {8x48},    20 {20x20},   12 {12x36},    8 {8x64},
    {12} 22 {22x22},   16 {16x36},    8 {8x80},    24 {24x24},
    {16}  8 {8x96},    12 {12x64},   26 {26x26},   20 {20x36},
    {20} 16 {16x48},    8 {8x120},   20 {20x44},   16 {32x32},
    {24} 16 {16x64},    8 {8x144},   12 {12x88},   26 {26x40},
    {28} 22 {22x48},   24 {24x48},   20 {20x64},   18 {36x36},
    {32} 26 {26x48},   24 {24x64},   20 {40x40},   26 {26x64},
    {36} 22 {44x44},   24 {48x48},   26 {52x52},   16 {64x64},
    {40} 18 {72x72},   20 {80x80},   22 {88x88},   24 {96x96},
    {44} 26 {104x104}, 20 {120x120}, 22 {132x132}, 24 {144x144}
    );

{* Vertical submodule size (including subfinder) - see Table 7 Data region W + 2 *}
matrixFW : array[0..NbOfSymbols - 1] of NativeInt = (
    { 0} 10 {10x10},   12 {12x12},   18 {8x18},    14 {14x14},
    { 4} 16 {8x32},    16 {16x16},   26 {12x26},   18 {18x18},
    { 8} 24 {8x48},    20 {20x20},   18 {12x36},   16 {8x64},
    {12} 22 {22x22},   18 {16x36},   20 {8x80},    24 {24x24},
    {16} 24 {8x96},    16 {12x64},   26 {26x26},   18 {20x36},
    {20} 24 {16x48},   20 {8x120},   22 {20x44},   16 {32x32},
    {24} 16 {16x64},   24 {8x144},   22 {12x88},   20 {26x40},
    {28} 24 {22x48},   24 {24x48},   16 {20x64},   18 {36x36},
    {32} 24 {26x48},   16 {24x64},   20 {40x40},   16 {26x64},
    {36} 22 {44x44},   24 {48x48},   26 {52x52},   16 {64x64},
    {40} 18 {72x72},   20 {80x80},   22 {88x88},   24 {96x96},
    {44} 26 {104x104}, 20 {120x120}, 22 {132x132}, 24 {144x144}
    );


{* Data Codewords per RS-Block *}
matrixdatablock : array[0..NbOfSymbols - 1] of NativeInt = (
    { 0}   3 {10x10},     5 {12x12},     5 {8x18},      8 {14x14},
    { 4}  10 {8x32},     12 {16x16},    16 {12x26},    18 {18x18},
    { 8}  18 {8x48},     22 {20x20},    22 {12x36},    24 {8x64},
    {12}  30 {22x22},    32 {16x36},    32 {8x80},     36 {24x24},
    {16}  38 {8x96},     43 {12x64},    44 {26x26},    44 {20x36},
    {20}  49 {16x48},    49 {8x120},    56 {20x44},    62 {32x32},
    {24}  62 {16x64},    63 {8x144},    64 {12x88},    70 {26x40},
    {28}  72 {22x48},    80 {24x48},    84 {20x64},    86 {36x36},
    {32}  90 {26x48},   108 {24x64},   114 {40x40},   118 {26x64},
    {36} 144 {44x44},   174 {48x48},   102 {52x52},   140 {64x64},
    {40}  92 {72x72},   114 {80x80},   144 {88x88},   174 {96x96},
    {44} 136 {104x104}, 175 {120x120}, 163 {132x132}, 156 {144x144}
    );


{* ECC Codewords per RS-Block *}
matrixrsblock : array[0..NbOfSymbols - 1] of NativeInt = (
    { 0}  5 {10x10},    7 {12x12},    7 {8x18},    10 {14x14},
    { 4} 11 {8x32},    12 {16x16},   14 {12x26},   14 {18x18},
    { 8} 15 {8x48},    18 {20x20},   18 {12x36},   18 {8x64},
    {12} 20 {22x22},   24 {16x36},   22 {8x80},    24 {24x24},
    {16} 28 {8x96},    27 {12x64},   28 {26x26},   28 {20x36},
    {20} 28 {16x48},   32 {8x120},   34 {20x44},   36 {32x32},
    {24} 36 {16x64},   36 {8x144},   36 {12x88},   38 {26x40},
    {28} 38 {22x48},   41 {24x48},   42 {20x64},   42 {36x36},
    {32} 42 {26x48},   46 {24x64},   48 {40x40},   50 {26x64},
    {36} 56 {44x44},   68 {48x48},   42 {52x52},   56 {64x64},
    {40} 36 {72x72},   48 {80x80},   56 {88x88},   68 {96x96},
    {44} 56 {104x104}, 68 {120x120}, 62 {132x132}, 62 {144x144}
    );



{$UNDEF RANGEON} {disable possible /d switch}
{$IFOPT R+}{$DEFINE RANGEON}{$ENDIF} {save initial switch state}
{$R-}
procedure ecc200placementbit(var _array : TArrayOfInteger; NR : Integer; NC : Integer; r : Integer; c : Integer; p : Integer; b : Byte);
begin
  if (r < 0) then
  begin
    Inc(r, NR);
    Inc(c, 4 - ((NR + 4) mod 8));
  end;
  if (c < 0) then
  begin
    Inc(c, NC);
    Inc(r, 4 - ((NC + 4) mod 8));
  end;
    // Necessary for 26x32,26x40,26x48,36x120,36x144,72x120,72x144
  if (r >= NR) then
    Dec(r, NR);

//  _array[r * NC + c] := (p shl 3) + Ord(b);
  _array[r * NC + c] := (p shl 3) + b;
end;
{$IFDEF RANGEON} {$R+} {$ENDIF}


procedure ecc200placementblock(var _array : TArrayOfInteger; NR : Integer; NC : Integer; r : Integer; c : Integer; p : Integer);
begin
  ecc200placementbit(_array, NR, NC, r - 2, c - 2, p, 7);
  ecc200placementbit(_array, NR, NC, r - 2, c - 1, p, 6);
  ecc200placementbit(_array, NR, NC, r - 1, c - 2, p, 5);
  ecc200placementbit(_array, NR, NC, r - 1, c - 1, p, 4);
  ecc200placementbit(_array, NR, NC, r - 1, c - 0, p, 3);
  ecc200placementbit(_array, NR, NC, r - 0, c - 2, p, 2);
  ecc200placementbit(_array, NR, NC, r - 0, c - 1, p, 1);
  ecc200placementbit(_array, NR, NC, r - 0, c - 0, p, 0);
end;

procedure ecc200placementcornerA(var _array : TArrayOfInteger; NR : Integer; NC : Integer; p : Integer);
begin
  ecc200placementbit(_array, NR, NC, NR - 1, 0, p, 7);
  ecc200placementbit(_array, NR, NC, NR - 1, 1, p, 6);
  ecc200placementbit(_array, NR, NC, NR - 1, 2, p, 5);
  ecc200placementbit(_array, NR, NC, 0, NC - 2, p, 4);
  ecc200placementbit(_array, NR, NC, 0, NC - 1, p, 3);
  ecc200placementbit(_array, NR, NC, 1, NC - 1, p, 2);
  ecc200placementbit(_array, NR, NC, 2, NC - 1, p, 1);
  ecc200placementbit(_array, NR, NC, 3, NC - 1, p, 0);
end;

procedure ecc200placementcornerB(var _array : TArrayOfInteger; NR : Integer; NC : Integer; p : Integer);
begin
  ecc200placementbit(_array, NR, NC, NR - 3, 0, p, 7);
  ecc200placementbit(_array, NR, NC, NR - 2, 0, p, 6);
  ecc200placementbit(_array, NR, NC, NR - 1, 0, p, 5);
  ecc200placementbit(_array, NR, NC, 0, NC - 4, p, 4);
  ecc200placementbit(_array, NR, NC, 0, NC - 3, p, 3);
  ecc200placementbit(_array, NR, NC, 0, NC - 2, p, 2);
  ecc200placementbit(_array, NR, NC, 0, NC - 1, p, 1);
  ecc200placementbit(_array, NR, NC, 1, NC - 1, p, 0);
end;

procedure ecc200placementcornerC(var _array : TArrayOfInteger; NR : Integer; NC : Integer; p : Integer);
begin
  ecc200placementbit(_array, NR, NC, NR - 3, 0, p, 7);
  ecc200placementbit(_array, NR, NC, NR - 2, 0, p, 6);
  ecc200placementbit(_array, NR, NC, NR - 1, 0, p, 5);
  ecc200placementbit(_array, NR, NC, 0, NC - 2, p, 4);
  ecc200placementbit(_array, NR, NC, 0, NC - 1, p, 3);
  ecc200placementbit(_array, NR, NC, 1, NC - 1, p, 2);
  ecc200placementbit(_array, NR, NC, 2, NC - 1, p, 1);
  ecc200placementbit(_array, NR, NC, 3, NC - 1, p, 0);
end;

procedure ecc200placementcornerD(var _array : TArrayOfInteger; NR : Integer; NC : Integer; p : Integer);
begin
  ecc200placementbit(_array, NR, NC, NR - 1, 0, p, 7);
  ecc200placementbit(_array, NR, NC, NR - 1, NC - 1, p, 6);
  ecc200placementbit(_array, NR, NC, 0, NC - 3, p, 5);
  ecc200placementbit(_array, NR, NC, 0, NC - 2, p, 4);
  ecc200placementbit(_array, NR, NC, 0, NC - 1, p, 3);
  ecc200placementbit(_array, NR, NC, 1, NC - 3, p, 2);
  ecc200placementbit(_array, NR, NC, 1, NC - 2, p, 1);
  ecc200placementbit(_array, NR, NC, 1, NC - 1, p, 0);
end;

// Annex M placement alorithm main function
procedure ecc200placement(var _array : TArrayOfInteger; NR : Integer; NC : Integer);
var
  r, c, p : Integer;
begin
  // invalidate
  for r := 0 to NR - 1 do
    for c := 0 to NC - 1 do
      _array[r * NC + c] := 0;
  // start
  p := 1;
  r := 4;
  c := 0;
  repeat
    // check corner
    if (r = NR) and (c = 0) then
    begin ecc200placementcornerA(_array, NR, NC, p); Inc(p); end;
    if (r = NR - 2) and (c = 0) and ((NC mod 4) <> 0) then
    begin ecc200placementcornerB(_array, NR, NC, p); Inc(p); end;
    if (r = NR - 2) and (c = 0) and ((NC mod 8) = 4) then
    begin ecc200placementcornerC(_array, NR, NC, p); Inc(p); end;
    if (r = NR + 4) and (c = 2) and ((NC mod 8) = 0) then
    begin ecc200placementcornerD(_array, NR, NC, p); Inc(p); end;
    // up/right
    repeat
      if (r < NR) and (c >= 0) and (_array[r * NC + c] = 0) then
      begin ecc200placementblock(_array, NR, NC, r, c, p); Inc(p); end;
      Dec(r, 2);
      Inc(c, 2);
    until not ((r >= 0) and (c < NC));
    Inc(r);
    Inc(c, 3);
    // down/left
    repeat
      if (r >= 0) and (c < NC) and (_array[r * NC + c] = 0) then
      begin ecc200placementblock(_array, NR, NC, r, c, p); Inc(p); end;
      Inc(r, 2);
      Dec(c, 2);
    until not ((r < NR) and (c >= 0));
    Inc(r, 3);
    Inc(c);
  until not ((r < NR) or (c < NC));
  // unfilled corner
  if (_array[NR * NC - 1] = 0) then
  begin
    _array[NR * NC - 1] := 1; _array[NR * NC - NC - 2] := 1;
  end;
end;

// calculate and append ecc code, and if necessary interleave
procedure ecc200(var binary : TArrayOfByte; bytes : Integer; datablock : Integer; rsblock : Integer; skew : Integer);
var
  blocks, b : Integer;
  n, p : Integer;
  buf, ecc : TArrayOfByte;
  RSGlobals : TRSGlobals;
begin
  SetLength(buf, 256);
  SetLength(ecc, 256);
  blocks := (bytes + 2) div datablock;
  rs_init_gf($12d, RSGlobals);
  rs_init_code(rsblock, 1, RSGlobals);
  for b := 0 to blocks - 1 do
  begin
    p := 0;
    n := b;
    while n < bytes do
    begin
      buf[p] := binary[n];
      Inc(p);
      Inc(n, blocks);
    end;
    rs_encode(p, buf, ecc, RSGlobals);
    p := rsblock - 1;  // comes back reversed
    n := b;
    while n < rsblock * blocks do
    begin
      if (skew <> 0) then
      begin
        { Rotate ecc data to make 144x144 size symbols acceptable }
        { See http://groups.google.com/group/postscriptbarcode/msg/5ae8fda7757477da }
        if (b < 8) then
        begin
          binary[bytes + n + 2] := ecc[p];
          Dec(p);
        end
        else
        begin
          binary[bytes + n - 8] := ecc[p];
          Dec(p);
        end;
      end
      else
      begin
        binary[bytes + n] := ecc[p];
        Dec(p);
      end;
      Inc(n, blocks);
    end;
  end;
  rs_free(RSGlobals);
end;

function isx12(const source : Byte) : Boolean;
begin
  if (source = 13) then begin result := True; exit; end;
  if (source = 42) then begin result := True; exit; end;
  if (source = 62) then begin result := True; exit; end;
  if (source = 32) then begin result := True; exit; end;
  if ((source >= ord('0')) and (source <= ord('9'))) then begin result := True; exit; end;
  if ((source >= ord('A')) and (source <= ord('Z'))) then begin result := True; exit; end;

  result := False;
end;

procedure dminsert(var binary_string : TArrayOfChar; posn : Integer; newbit : Char);
{ Insert a character into the middle of a string at position posn }
var
  i, _end : Integer;
begin
  SetLength(binary_string, Length(binary_string) + 1);
  _end := strlen(binary_string);
  for i := _end downto posn + 1 do
    binary_string[i] := binary_string[i - 1];

  binary_string[posn] := newbit;
end;

procedure insert_value(var binary_stream : TArrayOfByte; posn : Integer; streamlen : Integer; newbit : Byte);
var
  i : Integer;
begin
  for i := streamlen downto posn + 1 do
    binary_stream[i] := binary_stream[i - 1];

  binary_stream[posn] := newbit;
end;

function p_r_6_2_1(var InputData: TArrayOfByte; const Position, Sourcelen: NativeInt): NativeInt;
var
  i: NativeInt;
  NonX12Position: NativeInt;
  SpecialX12Position: NativeInt;
begin

    {* Annex P section (r)(6)(ii)(I)
       "If one of the three X12 terminator/separator characters first
        occurs in the yet to be processed data before a non-X12 character..."
     *}

  NonX12Position := 0;
  SpecialX12Position := 0;
  Result := 0;

  for I := Position to SourceLen - 1 do begin
    if (nonX12Position = 0) and (not isX12(inputData[i])) then
      nonX12Position := i;

    if (specialX12Position = 0) and
              ((inputData[i] = 13) or
              (inputData[i] = Ord('*')) or
              (inputData[i] = Ord('>'))) then
      specialX12Position := i;

    if (nonX12Position <> 0) and (specialX12Position <> 0) and (specialX12Position < nonX12Position) then
      Result := 1;
  end;
end;

//function look_ahead_test(source : TArrayOfByte; sourcelen : Integer; position : Integer; current_mode : Integer; gs1 : Integer) : Integer;
//{ A custom version of the 'look ahead test' from Annex P }
//{ This version is deliberately very reluctant to end a data stream with EDIFACT encoding }
//var
//  ascii_count, c40_count, text_count, x12_count, edf_count, b256_count, best_count : Single;
//  sp, done, best_scheme : Integer;
//  reduced_char : Byte;
//begin
//  { step (j) }
//  if (current_mode = DM_ASCII) then
//  begin
//    ascii_count := 0.0;
//    c40_count := 1.0;
//    text_count := 1.0;
//    x12_count := 1.0;
//    edf_count := 1.0;
//    b256_count := 1.25;
//  end
//  else
//  begin
//    ascii_count := 1.0;
//    c40_count := 2.0;
//    text_count := 2.0;
//    x12_count := 2.0;
//    edf_count := 2.0;
//    b256_count := 2.25;
//  end;
//
//  case current_mode of
//    DM_C40: c40_count := 0.0;
//    DM_TEXT: text_count := 0.0;
//    DM_X12: x12_count := 0.0;
//    DM_EDIFACT: edf_count := 0.0;
//    DM_BASE256: b256_count := 0.0;
//  end;
//
//  sp := position;
//  while (sp <= sourcelen) and (sp <= (position + 8)) do
//  begin
//    if (source[sp] <= 127) then reduced_char := source[sp] else reduced_char := Ord(source[sp]) - 127;
//
//    if ((source[sp] >= ord('0')) and (source[sp] <= Ord('9'))) then ascii_count := ascii_count + 0.5 else ascii_count := ascii_count + 1.0;
//    if (source[sp] > 127) then ascii_count := ascii_count+ 1.0;
//
//    done := 0;
//    if (reduced_char = ord(' ')) then begin c40_count := c40_count+ (2.0 / 3.0); done := 1; end;
//    if ((reduced_char >= ord('0')) and (reduced_char <= ord('9'))) then begin c40_count := c40_count + (2.0 / 3.0); done := 1; end;
//    if ((reduced_char >= ord('A')) and (reduced_char <= ord('Z'))) then begin c40_count := c40_count + (2.0 / 3.0); done := 1; end;
//    if (source[sp] > 127) then c40_count := c40_count + (4.0 / 3.0);
//    if (done = 0) then c40_count := c40_count + (4.0 / 3.0);
//
//    done := 0;
//    if (reduced_char = ord(' ')) then begin text_count := text_count + (2.0 / 3.0); done := 1; end;
//    if ((reduced_char >= ord('0')) and (reduced_char <= ord('9'))) then begin text_count := text_count + (2.0 / 3.0); done := 1; end;
//    if ((reduced_char >= ord('a')) and (reduced_char <= ord('z'))) then begin text_count := text_count + (2.0 / 3.0); done := 1; end;
//    if (source[sp] > 127) then text_count := text_count + (4.0 / 3.0);
//    if (done = 0) then text_count := text_count + (4.0 / 3.0);
//
//    if (isx12(source[sp]) <> 0) then x12_count := x12_count + (2.0 / 3.0) else x12_count := x12_count + 4.0;
//
//    { step (p) }
//    done := 0;
//    if ((source[sp] >= ord(' ')) and (source[sp] <= ord('^'))) then edf_count := edf_count + (3.0 / 4.0) else edf_count := edf_count + 6.0;
//    if ((gs1 <> 0) and (source[sp] = ord('['))) then edf_count := edf_count + 6.0;
//    if (sp >= (sourcelen - 5)) then edf_count := edf_count + 6.0; { MMmmm fudge! }
//
//    { step (q) }
//    if ((gs1 <> 0) and (source[sp] = ord('['))) then b256_count := b256_count + 4.0 else b256_count := b256_count + 1.0;
//
//    Inc(sp);
//  end;
//
//  best_count := ascii_count;
//  best_scheme := DM_ASCII;
//
//  if (b256_count <= best_count) then
//  begin
//    best_count := b256_count;
//    best_scheme := DM_BASE256;
//  end;
//
//  if (edf_count <= best_count) then
//  begin
//    best_count := edf_count;
//    best_scheme := DM_EDIFACT;
//  end;
//
//  if (text_count <= best_count) then
//  begin
//    best_count := text_count;
//    best_scheme := DM_TEXT;
//  end;
//
//  if (x12_count <= best_count) then
//  begin
//    best_count := x12_count;
//    best_scheme := DM_X12;
//  end;
//
//  if (c40_count <= best_count) then
//  begin
//    best_count := c40_count;
//    best_scheme := DM_C40;
//  end;
//
//  result := best_scheme;
//end;

{ A custom version of the 'look ahead test' from Annex P }
function look_ahead_test(var InputData : TArrayOfByte; sourcelen : Integer; position : Integer; current_mode : Integer; gs1 : Integer): Integer;
var
  ascii_count,
  c40_count,
  text_count,
  x12_count,
  edf_count,
  b256_count,
  best_count: Double;
  best_scheme : integer;
  sp          : integer;
  stiction: Double;
begin
  stiction := 1.0 / 24.0 { smallest change to act on, to get around floating point inaccuracies } ;
  best_scheme := DM_NULL;
  { step (j) }
  if current_mode = DM_ASCII then begin
    ascii_count := 0.0;
    c40_count := 1.0;
    text_count := 1.0;
    x12_count := 1.0;
    edf_count := 1.0;
    b256_count := 1.25;
  end
  else begin
    ascii_count := 1.0;
    c40_count := 2.0;
    text_count := 2.0;
    x12_count := 2.0;
    edf_count := 2.0;
    b256_count := 2.25;
  end;

  case current_mode of
    DM_C40:  c40_count := 0.0;
    DM_TEXT:  text_count := 0.0;
    DM_X12:  x12_count := 0.0;
    DM_EDIFACT:  edf_count := 0.0;
    DM_BASE256:  b256_count := 0.0;
  end;

  sp := position;

  Repeat
    if sp = sourcelen then begin
        { At the end of data ... step (k) }
        ascii_count := ceil(ascii_count);
        b256_count := ceil(b256_count);
        edf_count := ceil(edf_count);
        text_count := ceil(text_count);
        x12_count := ceil(x12_count);
        c40_count := ceil(c40_count);
        best_count := c40_count;
        best_scheme := DM_C40; // (k)(7)

        if x12_count < (best_count - stiction) then begin
            best_count := x12_count;
            best_scheme := DM_X12; // (k)(6)
        end;
        if text_count < (best_count - stiction) then begin
            best_count := text_count;
            best_scheme := DM_TEXT; // (k)(5)
        end;
        if edf_count < (best_count - stiction) then begin
            best_count := edf_count;
            best_scheme := DM_EDIFACT; // (k)(4)
        end;
        if b256_count < (best_count - stiction) then begin
            best_count := b256_count;
            best_scheme := DM_BASE256; // (k)(3)
        end;
        if ascii_count <= (best_count + stiction) then begin
            best_scheme := DM_ASCII; // (k)(2)
        end;
    end
    else begin
        { ascii ... step (l) }
        if (inputData[sp] >= Ord('0')) and (inputData[sp] <= Ord('9')) then
            ascii_count  := ascii_count + 0.5
        else begin
          if inputData[sp] > 127 then
            ascii_count := ceil(ascii_count) + 2.0 // (l)(2)
          else
            ascii_count := ceil(ascii_count) + 1.0; // (l)(3)
        end;

        { c40 ... step (m) }
        if (inputData[sp] = 32 {' '}) or
                (((inputData[sp] >= Ord('0')) and (inputData[sp] <= Ord('9')))  or
                ((inputData[sp] >= Ord('A'))  and  (inputData[sp] <= Ord('Z')))) then
            c40_count  := c40_count + (2.0 / 3.0)
        else begin
            if inputData[sp] > 127 then
                c40_count  := c40_count + (8.0 / 3.0)
            else
                c40_count  := c40_count + (4.0 / 3.0)
        end;

        { text ... step (n) }
        if (inputData[sp] = 32 {' '}) or
                (((inputData[sp] >= Ord('0'))  and  (inputData[sp] <= Ord('9')))  or
                ((inputData[sp] >= Ord('a'))  and  (inputData[sp] <= Ord('z')))) then
            text_count  := text_count + (2.0 / 3.0)
         else begin
            if inputData[sp] > 127 then
                text_count  := text_count + (8.0 / 3.0)
            else
                text_count  := text_count + (4.0 / 3.0);
        end;

        { x12 ... step (o) }
        if isX12(inputData[sp]) then
            x12_count  := x12_count + (2.0 / 3.0)
        else begin
            if inputData[sp] > 127 then
                x12_count  := x12_count + (13.0 / 3.0)
            else
                x12_count  := x12_count + (10.0 / 3.0);
        end;

        { edifact ... step (p) }
        if (inputData[sp] >= 32 {' '}) and (inputData[sp] <= Ord('^')) then
            edf_count  := edf_count + (3.0 / 4.0)
        else begin
            if inputData[sp] > 127 then
                edf_count  := edf_count + 17.0  // (p)(2)  > Value changed from ISO
            else
                edf_count  := edf_count + 13.0; // (p)(3)  > Value changed from ISO
        end;

        if (gs1 = 1) and (inputData[sp] = Ord('[')) then
            edf_count  := edf_count + 13.0;  // > Value changed from ISO

        { base 256 ... step (q) }
        if (gs1 = 1) and (inputData[sp] = Ord('[')) then
            b256_count  := b256_count + 4.0   // (q)(1)
        else
            b256_count  := b256_count + 1.0;  // (q)(2)
    end;

    if sp > (position + 3) then begin
        { 4 data characters processed ... step (r) }
        { step (r)(6) }
        if (c40_count + 1.0 < ascii_count - stiction) and
                (c40_count + 1.0 < b256_count - stiction) and
                (c40_count + 1.0 < edf_count - stiction) and
                (c40_count + 1.0 < text_count - stiction) then begin

            if c40_count < (x12_count - stiction) then
                best_scheme := DM_C40;

            if (c40_count >= x12_count - stiction)
                     and  (c40_count <= x12_count + stiction) then begin
                if p_r_6_2_1(inputData, sp, sourcelen) = 1 then
                    // Test (r)(6)(ii)(i)
                  best_scheme := DM_X12
                else
                  best_scheme := DM_C40;
            end;
        end;

        { step (r)(5) }
        if (x12_count + 1.0 < ascii_count - stiction) and
                (x12_count + 1.0 < b256_count - stiction) and
                (x12_count + 1.0 < edf_count - stiction) and
                (x12_count + 1.0 < text_count - stiction) and
                (x12_count + 1.0 < c40_count - stiction) then
            best_scheme := DM_X12;

        { step (r)(4) }
        if (text_count + 1.0 < ascii_count - stiction) and
                (text_count + 1.0 < b256_count - stiction) and
                (text_count + 1.0 < edf_count - stiction) and
                (text_count + 1.0 < x12_count - stiction) and
                (text_count + 1.0 < c40_count - stiction) then
            best_scheme := DM_TEXT;

        { step (r)(3) }
        if (edf_count + 1.0 < ascii_count - stiction) and
                (edf_count + 1.0 < b256_count - stiction) and
                (edf_count + 1.0 < text_count - stiction) and
                (edf_count + 1.0 < x12_count - stiction) and
                (edf_count + 1.0 < c40_count - stiction) then
            best_scheme := DM_EDIFACT;

        { step (r)(2) }
        if (b256_count + 1.0 <= ascii_count + stiction) or
                (b256_count + 1.0 < edf_count - stiction) and
                (b256_count + 1.0 < text_count - stiction) and
                (b256_count + 1.0 < x12_count - stiction) and
                (b256_count + 1.0 < c40_count - stiction) then
            best_scheme := DM_BASE256;

        { step (r)(1) }
        if (ascii_count + 1.0 <= b256_count + stiction) and
                (ascii_count + 1.0 <= edf_count + stiction) and
                (ascii_count + 1.0 <= text_count + stiction) and
                (ascii_count + 1.0 <= x12_count + stiction) and
                (ascii_count + 1.0 <= c40_count + stiction) then
            best_scheme := DM_ASCII;
    end;
    Inc(sp);

  Until (best_scheme <> DM_NULL); // step (s)
  Result := best_scheme;
end;


function dm200encode(symbol : zint_symbol; source : TArrayOfByte; var target : TArrayOfByte; var last_mode : Integer;
                    var _length : Integer; process_Buffer: TArrayOfInteger; var process_p: integer; var Binlen_p: Integer) : Integer;
{ Encodes data using ASCII, C40, Text, X12, EDIFACT or Base 256 modes as appropriate }
{ Supports encoding FNC1 in supporting systems }
var
  sp, tp, i, gs1 : Integer;
  current_mode, next_mode : Integer;
  inputlen : Integer;
  binary : TArrayOfChar;
  shift_set, value : Integer;
  iv : Integer;
  binary_count : Integer;
  prn, temp : Integer;
begin
  inputlen := _length;
  SetLength(binary, Max(4, inputlen * 2) {Needs a minimum length !!});

  sp := 0;
  tp := 0;
  FillChar(process_Buffer[0], SizeOf(process_Buffer), 0);
  process_p := 0;

  strcpy(binary, '');

  { step (a) }
  current_mode := DM_ASCII;
  next_mode := DM_ASCII;

  if ((symbol.input_mode and 7) = GS1_MODE) then begin
    if (symbol.output_options and GS1_GS_SEPARATOR <> 0) then
      gs1 := 2
    else
      gs1 := 1;
  end
  else
    gs1 := 0;

  // manual GS1 encoding with raw data
  if (gs1=0) and (source[0]=232 { FNC1 }) then begin
    Inc(sp);
    gs1 := 1;
  end;

  if (gs1 <> 0) then
  begin
    target[tp] := 232; Inc(tp);
    concat(binary, ' ');
  end; { FNC1 }

  if (symbol.output_options and READER_INIT) <> 0 then
  begin
    if (gs1 <> 0) then
    begin
      strcpy(symbol.errtxt, 'Cannot encode in GS1 mode and Reader Initialisation at the same time');
      exit(ZERROR_INVALID_OPTION);
    end
    else
    begin
      target[tp] := 234; Inc(tp); { Reader Programming }
      concat(binary, ' ');
    end;
  end;

  if symbol.eci > 0 then begin
      { Encode ECI numbers according to Table 6 }
      target[tp] := 241;   // ECI Character
      Inc(tp);
      if symbol.eci <= 126 then begin
          target[tp] := symbol.eci + 1;
          Inc(tp);
          concat(binary, '  ');
      end;
      if (symbol.eci >= 127) and (symbol.eci <= 16382) then  begin
          target[tp] := ((symbol.eci - 127) div 254) + 128;
          Inc(tp);
          target[tp] := ((symbol.eci - 127) mod 254) + 1;
          Inc(tp);
          concat(binary, '   ');
      end;
      if symbol.eci >= 16383 then begin
          target[tp] := ((symbol.eci - 16383) div 64516) + 192;
          Inc(tp);
          target[tp] := (((symbol.eci - 16383) div 254) mod 254) + 1;
          Inc(tp);
          target[tp] := ((symbol.eci - 16383) mod 254) + 1;
          Inc(tp);
          concat(binary, '    ');
      end;
  end;

  // Check for Macro05/Macro06
  //     "[)>[RS]05[GS]...[RS][EOT]"  -> CW 236
  //     "[)>[RS]06[GS]...[RS][EOT]"  -> CW 237
  if (tp = 0) and (sp = 0) and (inputlen >= 9)
           and (source[0] = Ord('[')) and (source[1] = Ord(')')) and (source[2] = Ord('>'))
           and (source[3] = $1e {RS}) and (source[4] = Ord('0'))
           and ((source[5] = Ord('5')) or (source[5] = Ord('6')))
           and (source[6] = $1d {GS})
           and (source[inputlen - 2] = $1e {RS}) and (source[inputlen - 1] = $4 {EOT}) then begin

        // Output macro Codeword
        if (source[5] = Ord('5')) then
          target[tp] := 236
        else
          target[tp] := 237;

      inc(tp);
      concat(binary, ' ');
      {* Remove macro characters from input string *}
      sp := 7;
      Dec(inputlen, 2);
      Dec(_length, 2);
  end;


  while (sp < inputlen) do
  begin

    current_mode := next_mode;

    { step (b) - ASCII encodation }
    if (current_mode = DM_ASCII) then
    begin
      next_mode := DM_ASCII;

      if (((sp + 1) <{=} inputlen) and istwodigits(source, sp)) then
      begin
        target[tp] := (10 * StrToInt(Chr(source[sp]))) + StrToInt(Chr(source[sp + 1])) + 130;
        Inc(tp); concat(binary, ' ');
        Inc(sp, 2);
      end
      else
      begin
        next_mode := look_ahead_test(source, inputlen, sp, current_mode, gs1);

        if (next_mode <> DM_ASCII) then
        begin
          case next_mode of
            DM_C40: begin target[tp] := 230; Inc(tp); concat(binary, ' '); end;
            DM_TEXT: begin target[tp] := 239; Inc(tp); concat(binary, ' '); end;
            DM_X12: begin target[tp] := 238; Inc(tp); concat(binary, ' '); end;
            DM_EDIFACT: begin target[tp] := 240; Inc(tp); concat(binary, ' '); end;
            DM_BASE256: begin target[tp] := 231; Inc(tp); concat(binary, ' '); end;
          end;
        end
        else
        begin
          if (source[sp] > 127) then
          begin
            target[tp] := 235; { FNC4 }
            Inc(tp);
            target[tp] := (Ord(source[sp]) - 128) + 1;
            Inc(tp); concat(binary, '  ');
          end
          else
          begin
            if ((gs1 <> 0) and (source[sp] = ord('['))) then begin
              if gs1 = 2 then
                target[tp] := 29+1 { GS }
              else
                target[tp] := 232; { FNC1 }
            end
            else
              target[tp] := Ord(source[sp]) + 1;

            Inc(tp);
            concat(binary, ' ');
          end;
          Inc(sp);
        end;
      end;

    end;

    { step (c) C40 encodation }
    if (current_mode = DM_C40) then
    begin
      next_mode := DM_C40;
      if (process_p = 0) then
        next_mode := look_ahead_test(source, inputlen, sp, current_mode, gs1);

      if (next_mode <> DM_C40) then
      begin
        target[tp] := 254; Inc(tp); concat(binary, ' ');{ Unlatch }
        next_mode := DM_ASCII;
      end
      else
      begin
        if (source[sp] > 127) then
        begin
          process_buffer[process_p] := 1; Inc(process_p);
          process_buffer[process_p] := 30; Inc(process_p); { Upper Shift }
          shift_set := c40_shift[Ord(source[sp]) - 128];
          value := c40_value[Ord(source[sp]) - 128];
        end
        else
        begin
          shift_set := c40_shift[Ord(source[sp])];
          value := c40_value[Ord(source[sp])];
        end;

        if ((gs1 <> 0{1}) and (source[sp] = ord('['))) then
          if gs1 = 2 then begin
            shift_set := c40_shift[29];
            value := c40_value[29];  {* GS *}
          end
          else begin
            shift_set := 2;
            value := 27; { FNC1 }
          end;

        if (shift_set <> 0) then
        begin
          process_buffer[process_p] := shift_set - 1;
          Inc(process_p);
        end;
        process_buffer[process_p] := value;
        Inc(process_p);

        while (process_p >= 3) do begin
          iv := (1600 * process_buffer[0]) + (40 * process_buffer[1]) + (process_buffer[2]) + 1;
          target[tp] := iv div 256; Inc(tp);
          target[tp] := iv mod 256; Inc(tp);
          concat(binary, '  ');

          process_buffer[0] := process_buffer[3];
          process_buffer[1] := process_buffer[4];
          process_buffer[2] := process_buffer[5];
          process_buffer[3] := 0;
          process_buffer[4] := 0;
          process_buffer[5] := 0;
          Dec(process_p, 3);
        end;
        Inc(sp);
      end;
    end;

    { step (d) Text encodation }
    if (current_mode = DM_TEXT) then
    begin
      next_mode := DM_TEXT;
      if (process_p = 0) then
        next_mode := look_ahead_test(source, inputlen, sp, current_mode, gs1);

      if (next_mode <> DM_TEXT) then
      begin
        target[tp] := 254; Inc(tp); concat(binary, ' ');{ Unlatch }
        next_mode := DM_ASCII;
      end
      else
      begin
        if (source[sp] > 127) then
        begin
          process_buffer[process_p] := 1; Inc(process_p);
          process_buffer[process_p] := 30; Inc(process_p); { Upper Shift }
          shift_set := text_shift[Ord(source[sp]) - 128];
          value := text_value[Ord(source[sp]) - 128];
        end
        else
        begin
          shift_set := text_shift[Ord(source[sp])];
          value := text_value[Ord(source[sp])];
        end;

        if ((gs1 <> 0) and (source[sp] = ord('['))) then
          if gs1 = 2 then begin
            shift_set := text_shift[29];
            value := text_value[29];  {* GS *}
          end
          else begin
            shift_set := 2;
            value := 27; { FNC1 }
          end;

        if (shift_set <> 0) then
        begin
          process_buffer[process_p] := shift_set - 1; Inc(process_p);
        end;
        process_buffer[process_p] := value; Inc(process_p);

        while (process_p >= 3) do begin
          iv := (1600 * process_buffer[0]) + (40 * process_buffer[1]) + (process_buffer[2]) + 1;
          target[tp] := iv div 256; Inc(tp);
          target[tp] := iv mod 256; Inc(tp);
          concat(binary, '  ');

          process_buffer[0] := process_buffer[3];
          process_buffer[1] := process_buffer[4];
          process_buffer[2] := process_buffer[5];
          process_buffer[3] := 0;
          process_buffer[4] := 0;
          process_buffer[5] := 0;
          Dec(process_p, 3);
        end;
        Inc(sp);
      end;
    end;

    { step (e) X12 encodation }
    if (current_mode = DM_X12) then
    begin
      value := 0;

      next_mode := DM_X12;
      if (process_p {fs 31/08/2018 text_p???} = 0) then
        next_mode := look_ahead_test(source, inputlen, sp, current_mode, gs1);

      if (next_mode <> DM_X12) then
      begin
        target[tp] := 254; Inc(tp); concat(binary, ' ');{ Unlatch }
        next_mode := DM_ASCII;
      end
      else
      begin
        if (source[sp] = 13) then value := 0;
        if (source[sp] = ord('*')) then value := 1;
        if (source[sp] = ord('>')) then value := 2;
        if (source[sp] = ord(' ')) then value := 3;
        if ((source[sp] >= ord('0')) and (source[sp] <= ord('9'))) then value := (Ord(source[sp]) - Ord('0')) + 4;
        if ((source[sp] >= ord('A')) and (source[sp] <= ord('Z'))) then value := (Ord(source[sp]) - Ord('A')) + 14;

        process_buffer[process_p] := value; Inc(process_p);

        while (process_p >= 3) do begin
          iv := (1600 * process_buffer[0]) + (40 * process_buffer[1]) + (process_buffer[2]) + 1;
          target[tp] := iv div 256; Inc(tp);
          target[tp] := iv mod 256; Inc(tp);
          concat(binary, '  ');

          process_buffer[0] := process_buffer[3];
          process_buffer[1] := process_buffer[4];
          process_buffer[2] := process_buffer[5];
          process_buffer[3] := 0;
          process_buffer[4] := 0;
          process_buffer[5] := 0;
          Dec(process_p, 3);
        end;
        Inc(sp);
      end;
    end;

    { step (f) EDIFACT encodation }
    if (current_mode = DM_EDIFACT) then
    begin
      next_mode := DM_EDIFACT;
      if (process_p = 3) then
        next_mode := look_ahead_test(source, inputlen, sp, current_mode, gs1);

      if (next_mode <> DM_EDIFACT) then
      begin
        process_buffer[process_p] := 31;
        Inc(process_p);
        next_mode := DM_ASCII;
      end
      else
      begin
        {fs 02/06/2020}
        value := source[sp];
        //if ((source[sp] >= ord('@')) and (source[sp] <= ord('^'))) then value := Ord(source[sp]) - Ord('@');
        //if ((source[sp] >= ord(' ')) and (source[sp] <= ord('?'))) then value := Ord(source[sp]);

        if source[sp] >= ord('@') then
          value := Ord(source[sp]) - Ord('@');

        process_buffer[process_p] := value; Inc(process_p);
        Inc(sp);
      end;

      while (process_p >= 4) do begin
        target[tp] := (process_buffer[0] shl 2) + ((process_buffer[1] and $30) shr 4); Inc(tp);
        target[tp] := ((process_buffer[1] and $0f) shl 4) + ((process_buffer[2] and $3c) shr 2); Inc(tp);
        target[tp] := ((process_buffer[2] and $03) shl 6) + process_buffer[3]; Inc(tp);
        concat(binary, '   ');

        process_buffer[0] := process_buffer[4];
        process_buffer[1] := process_buffer[5];
        process_buffer[2] := process_buffer[6];
        process_buffer[3] := process_buffer[7];
        process_buffer[4] := 0;
        process_buffer[5] := 0;
        process_buffer[6] := 0;
        process_buffer[7] := 0;
        Dec(process_p, 4);
      end;
    end;

    { step (g) Base 256 encodation }
    if (current_mode = DM_BASE256) then
    begin
      next_mode := look_ahead_test(source, inputlen, sp, current_mode, gs1);

      if (next_mode = DM_BASE256) then
      begin
        target[tp] := source[sp];
        Inc(tp);
        Inc(sp);
        concat(binary, 'b');
      end
      else
        next_mode := DM_ASCII;
    end;

    if (tp > 1558) then
    begin
      strcpy(symbol.errtxt, 'Data too long to fit in symbol');
      exit(ZERROR_TOO_LONG);
    end;

  end; { while }


  { Add length and randomising algorithm to b256 }
  i := 0;
  while (i < tp) do
  begin
    if (binary[i] = 'b') then
    begin
      if ((i = 0) or ((i <> 0) and (binary[i - 1] <> 'b'))) then
      begin
        { start of binary data }
        binary_count := 0;
        while (binary_count + i < tp) and (binary[binary_count + i] = 'b') do
          Inc(binary_count);

        if (binary_count <= 249) then
        begin
          dminsert(binary, i, 'b');
          insert_value(target, i, tp, binary_count); Inc(tp);
        end
        else
        begin
          dminsert(binary, i, 'b');
          dminsert(binary, i + 1, 'b');
          insert_value(target, i, tp, (binary_count div 250) + 249); Inc(tp);
          insert_value(target, i + 1, tp, binary_count mod 250); Inc(tp);
        end;
      end;
    end;
    Inc(i);
  end;

  for i := 0 to tp - 1 do
  begin
    if (binary[i] = 'b') then
    begin
      prn := ((149 * (i + 1)) mod 255) + 1;
      temp := target[i] + prn;
      if (temp <= 255) then
        target[i] := temp
      else
        target[i] := temp - 256;
    end;
  end;

  last_mode := current_mode;
  Binlen_p := tp;
  result := 0;
end;

procedure add_tail(var target : TArrayOfByte; tp : Integer; tail_length : Integer {; last_mode : Integer});
{ adds unlatch and pad bits }
var
  i, prn, temp : NativeInt;
begin
  for i := tail_length downto 1 do
  begin
    if (i = tail_length) then
    begin
      target[tp] := 129;
      Inc(tp); { Pad }
    end
    else begin
      prn := ((149 * (tp + 1)) mod 253) + 1;
      temp := 129 + prn;
      if (temp <= 254) then begin
        target[tp] := temp;
        Inc(tp);
      end
      else begin
        target[tp] := temp - 254;
        Inc(tp);
      end;
    end;
  end;
end;

function dm200encode_remainder(var target: TArrayOfByte; target_length: integer; const source: TArrayOfByte; const inputlen: integer;
                        const last_mode: integer; const process_Buffer: TArrayOfInteger; const process_p: integer; const symbols_left: integer): Integer;
var
  intValue : NativeInt;
begin
    case last_mode of
        DM_C40,
        DM_TEXT: begin
            if process_p = 1 then // 1 data character left to encode.
            begin
                if symbols_left > 1 then  begin
                    target[target_length] := 254;
                    Inc(target_length); // Unlatch and encode remaining data in ascii.
                end;
                target[target_length] := source[inputlen - 1] + 1;
                Inc(target_length);
            end
            else if (process_p = 2) then begin   // 2 data characters left to encode.
                // Pad with shift 1 value (0) and encode as double.
                intValue := (1600 * process_buffer[0]) + (40 * process_buffer[1]) + 1;
                target[target_length] := intValue div 256;
                Inc(target_length);
                target[target_length] := intValue Mod 256;
                Inc(target_length);
                if symbols_left > 2 then begin
                    target[target_length] := 254;
                    Inc(target_length);
                end;
            end
            else begin
                if symbols_left > 0 then begin
                    target[target_length] := 254;
                    Inc(target_length);
                end;
            end;
        end;

        DM_X12: begin
            if (symbols_left = process_p) and (process_p = 1) then  begin
                // Unlatch not required!
                target[target_length] := source[inputlen - 1] + 1;
                Inc(target_length);
            end
            else begin
                target[target_length] := 254;
                Inc(target_length); // Unlatch.

                if process_p = 1 then begin
                    target[target_length] := source[inputlen - 1] + 1;
                    Inc(target_length);
                end;

                if process_p = 2 then begin
                    target[target_length] := source[inputlen - 2] + 1;
                    Inc(target_length);
                    target[target_length] := source[inputlen - 1] + 1;
                    Inc(target_length);
                end;
            end;
        end;

        DM_EDIFACT: begin
          if symbols_left <= 2 then // Unlatch not required!
          begin
              if process_p = 1 then  begin
                  target[target_length] := source[inputlen - 1] + 1;
                  Inc(target_length);
              end;
              if process_p = 2 then begin
                  target[target_length] := source[inputlen - 2] + 1;
                  Inc(target_length);
                  target[target_length] := source[inputlen - 1] + 1;
                  Inc(target_length);
              end;
          end
          else begin
              // Append edifact unlatch value (31) and empty buffer
              if process_p = 0 then begin
                  target[target_length] := (31 shl 2);
                  Inc(target_length);
              end;
              if process_p = 1 then begin
                  target[target_length] := (process_buffer[0] shl 2) + ((31 and $30) shr 4);
                  Inc(target_length);
                  target[target_length] := (31 and $0f) shl 4;
                  Inc(target_length);
              end;
              if process_p = 2 then begin
                  target[target_length] := (process_buffer[0] shl 2) + ((process_buffer[1] and $30) shr  4);
                  Inc(target_length);
                  target[target_length] := ((process_buffer[1] and $0f)  shl  4) + ((31 and $3c) shr 2);
                  Inc(target_length);
                  target[target_length] := (31 and $03) shl 6;
                  Inc(target_length);
              end;
              if process_p = 3 then begin
                  target[target_length] := (process_buffer[0] shl 2) + ((process_buffer[1] and $30) shr 4);
                  Inc(target_length);
                  target[target_length] := ((process_buffer[1] and $0f) shl 4) + ((process_buffer[2] and $3c) shr 2);
                  Inc(target_length);
                  target[target_length] := ((process_buffer[2] and $03) shl 6) + 31;
                  Inc(target_length);
              end;
          end;
        end;
    end;
    Result := target_length;
end;

function data_matrix_200(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  skew : Integer;
  binary : TArrayOfByte;
  binlen : Integer;
//  inputlen: Integer;

  process_buffer: TArrayOfInteger;    // holds remaining data to finalised
  process_p: integer;  // number of characters left to finalise
  symbolsize, optionsize, calcsize : Integer;
  taillength, error_number : Integer;
  symbols_left: integer;
  H, W, FH, FW, datablock, bytes, rsblock : Integer;
  last_mode : Integer;
  grid : TArrayOfByte;
  i : Integer;
  x, y, NC, NR : Integer;
  places : TArrayOfInteger;
  v : Integer;
begin
  skew := 0;

//  inputlen := _length;
  SetLength(binary, 2200);
  SetLength(process_buffer, 8);
  binlen := 0;
  error_number := dm200encode(symbol, source, binary, last_mode, _length, process_buffer, process_p, binlen);

  if (error_number <> 0) then
    exit(error_number);

  if ((symbol.option_2 >= 1) and (symbol.option_2 <= NbOfSymbols)) then
    optionsize := intsymbol[symbol.option_2 - 1]
  else
    optionsize := -1;

  calcsize := NbOfSymbols - 1;
  for i := NbOfSymbols - 1 downto 0 do
    if (matrixbytes[i] >= binlen + process_p) then
      calcsize := i;

  if (symbol.option_3 = DM_SQUARE) then
  begin
    { fs 30/08/2018  Skip rectangular symbols in square only mode }
    while (calcsize < NbOfSymbols) and (matrixH[calcsize] <> matrixW[calcsize]) do Inc(calcsize);

    if (optionsize <> -1) then begin
      strcpy(symbol.errtxt, '521: Can not force square symbols when symbol size is selected');
      error_number := ZWARN_INVALID_OPTION;
    end;
  end
  {fs 02/04/2018 added DMRE}
  else if symbol.option_3 = DM_RECT then
    { fs 30/08/2018  force the use of rectangular symbols }
    while (calcsize < NbOfSymbols) and (not isRectangle[calcsize]) do Inc(calcsize)
  else if symbol.option_3 <> DM_DMRE then
    { Skip DMRE symbols }
    while (calcsize < NbOfSymbols) and isDMRE[calcsize] do
      Inc(calcsize);

  symbolsize := optionsize;
  if (calcsize > optionsize) then
  begin
    symbolsize := calcsize;
    if (optionsize <> -1) then
    begin
      { flag an error }
      strcpy(symbol.errtxt, 'Data does not fit in selected symbol size');
      Exit(ZERROR_TOO_LONG);
    end;
  end;


  // Now we know the symbol size we can handle the remaining data in the process buffer.
  symbols_left := matrixbytes[symbolsize] - binlen;
  binlen := dm200encode_remainder(binary, binlen, source, _length, last_mode, process_buffer, process_p, symbols_left);

  if (binlen > matrixbytes[symbolsize]) then begin
      strcpy(symbol.errtxt, 'Data too long to fit in symbol');
      Exit(ZERROR_TOO_LONG);
  end;

  H := matrixH[symbolsize];
  W := matrixW[symbolsize];
  FH := matrixFH[symbolsize];
  FW := matrixFW[symbolsize];
  bytes := matrixbytes[symbolsize];
  datablock := matrixdatablock[symbolsize];
  rsblock := matrixrsblock[symbolsize];

  taillength := bytes - binlen;

  if (taillength <> 0) then
    add_tail(TArrayOfByte(binary), binlen, taillength{, last_mode});

  // ecc code
  if (symbolsize = NbOfSymbols - 1) then skew := 1;
  ecc200(binary, bytes, datablock, rsblock, skew);

//  begin      // placement
    NC := W - 2 * (W div FW);
    NR := H - 2 * (H div FH);
    SetLength(places, NC * NR);
    ecc200placement(places, NR, NC);
    SetLength(grid, W * H);
    FillChar(grid[0], SizeOf(grid), 0);
    y := 0;
    while y < H do
    begin
      for x := 0 to W - 1 do
        grid[y * W + x] := 1;
      x := 0;
      while x < W do
      begin
        grid[(y + FH - 1) * W + x] := 1;
        Inc(x, 2)
      end;
      Inc(y, FH);
    end;
    x := 0;
    while x < W do
    begin
      for y := 0 to H - 1 do
        grid[y * W + x] := 1;
      y := 0;
      while y < H do
      begin
        grid[y * W + x + FW - 1] := 1;
        Inc(y, 2);
      end;
      Inc(x, FW);
    end;
    for y := 0 to NR - 1 do
    begin
      for x := 0 to NC - 1 do
      begin
        v := places[(NR - y - 1) * NC + x];
        if ((v = 1) or ((v > 7) and ((binary[(v shr 3) - 1] and (1 shl (v and 7))) <> 0))) then
          grid[(1 + y + 2 * (y div (FH - 2))) * W + 1 + x + 2 * (x div (FW - 2))] := 1;
      end;
    end;
    for y := H - 1 downto 0 do
    begin
      for x := 0 to W - 1 do
      begin
        if (grid[W * y + x] <> 0) then
          set_module(symbol, (H - y) - 1, x);
      end;
      symbol.row_height[(H - y) - 1] := 1;
    end;
    SetLength(grid, 0);
    SetLength(places, 0);
//  end;

  symbol.rows := H;
  symbol.width := W;

  result := error_number;
end;

function dmatrix(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number : Integer;
begin
  if (symbol.option_1 <= 1) then
    { ECC 200 }
    error_number := data_matrix_200(symbol, source, _length)
  else
  begin
    { ECC 000 - 140 }
    strcpy(symbol.errtxt, 'Older Data Matrix standards are no longer supported');
    error_number := ZERROR_INVALID_OPTION;
  end;

  result := error_number;
end;


end.

