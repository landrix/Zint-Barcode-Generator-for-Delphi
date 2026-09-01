unit zint_qr;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: DWYWBDBU (do what you want, but dont blame us)

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b work in progress
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  zint;


function qr_code(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;
function microqr(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;
function rmqr(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;

implementation

uses
  SysUtils, zint_reedsol, zint_common, zint_sjis, zint_helper;

const
  LEVEL_L	= 1;
  LEVEL_M	= 2;
  LEVEL_Q	= 3;
  LEVEL_H	= 4;

  // Indexes into qr_mode_types (and into the cost arrays)
  QR_N = 0; // Numeric
  QR_A = 1; // Alphanumeric
  QR_B = 2; // Byte
  QR_K = 3; // Kanji

  QR_NUM_MODES = 4;

  qr_mode_types : array [0..QR_NUM_MODES - 1] of Char = ('N', 'A', 'B', 'K'); // Same order as QR_N etc

  // Bits are multiplied by this for costs, so as to be whole integers divisible by 2 and 3
  QR_MULT = 6;

type
  TQrCosts = array [0..QR_NUM_MODES - 1] of NativeInt;

const
  RHODIUM = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:';

  qr_data_codewords_L: array [0..39] of FixedInt = (
    19, 34, 55, 80, 108, 136, 156, 194, 232, 274, 324, 370, 428, 461, 523, 589, 647,
    721, 795, 861, 932, 1006, 1094, 1174, 1276, 1370, 1468, 1531, 1631,
    1735, 1843, 1955, 2071, 2191, 2306, 2434, 2566, 2702, 2812, 2956);

  qr_data_codewords_M: array [0..39] of FixedInt = (
    16, 28, 44, 64, 86, 108, 124, 154, 182, 216, 254, 290, 334, 365, 415, 453, 507,
    563, 627, 669, 714, 782, 860, 914, 1000, 1062, 1128, 1193, 1267,
    1373, 1455, 1541, 1631, 1725, 1812, 1914, 1992, 2102, 2216, 2334);

  qr_data_codewords_Q: array [0..39] of FixedInt = (
    13, 22, 34, 48, 62, 76, 88, 110, 132, 154, 180, 206, 244, 261, 295, 325, 367,
    397, 445, 485, 512, 568, 614, 664, 718, 754, 808, 871, 911,
    985, 1033, 1115, 1171, 1231, 1286, 1354, 1426, 1502, 1582, 1666);

  qr_data_codewords_H: array [0..39] of FixedInt = (
    9, 16, 26, 36, 46, 60, 66, 86, 100, 122, 140, 158, 180, 197, 223, 253, 283,
    313, 341, 385, 406, 442, 464, 514, 538, 596, 628, 661, 701,
    745, 793, 845, 901, 961, 986, 1054, 1096, 1142, 1222, 1276);

  qr_total_codewords: array [0..39] of FixedInt = (
    26, 44, 70, 100, 134, 172, 196, 242, 292, 346, 404, 466, 532, 581, 655, 733, 815,
    901, 991, 1085, 1156, 1258, 1364, 1474, 1588, 1706, 1828, 1921, 2051,
    2185, 2323, 2465, 2611, 2761, 2876, 3034, 3196, 3362, 3532, 3706);

  qr_blocks_L: array [0..39] of FixedInt = (
    1, 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 4, 6, 6, 6, 6, 7, 8, 8, 9, 9, 10, 12, 12,
    12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25);

  qr_blocks_M: array [0..39] of FixedInt = (
    1, 1, 1, 2, 2, 4, 4, 4, 5, 5, 5, 8, 9, 9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20,
    21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49);

  qr_blocks_Q: array [0..39] of FixedInt = (
    1, 1, 2, 2, 4, 4, 6, 6, 8, 8, 8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25,
    27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68);

  qr_blocks_H: array [0..39] of FixedInt = (
    1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30,
    32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81);

  qr_sizes: array [0..39] of FixedInt = (
    21, 25, 29, 33, 37, 41, 45, 49, 53, 57, 61, 65, 69, 73, 77, 81, 85, 89, 93, 97,
    101, 105, 109, 113, 117, 121, 125, 129, 133, 137, 141, 145, 149, 153, 157, 161, 165, 169, 173, 177);

  micro_qr_sizes: array [0..3] of FixedInt = (
    11, 13, 15, 17);

  qr_align_loopsize : array [0..39] of FixedInt = (
  	0, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7);

  qr_table_e1 : array [0..272] of FixedInt = (
    6, 18, 0, 0, 0, 0, 0,
    6, 22, 0, 0, 0, 0, 0,
    6, 26, 0, 0, 0, 0, 0,
    6, 30, 0, 0, 0, 0, 0,
    6, 34, 0, 0, 0, 0, 0,
    6, 22, 38, 0, 0, 0, 0,
    6, 24, 42, 0, 0, 0, 0,
    6, 26, 46, 0, 0, 0, 0,
    6, 28, 50, 0, 0, 0, 0,
    6, 30, 54, 0, 0, 0, 0,
    6, 32, 58, 0, 0, 0, 0,
    6, 34, 62, 0, 0, 0, 0,
    6, 26, 46, 66, 0, 0, 0,
    6, 26, 48, 70, 0, 0, 0,
    6, 26, 50, 74, 0, 0, 0,
    6, 30, 54, 78, 0, 0, 0,
    6, 30, 56, 82, 0, 0, 0,
    6, 30, 58, 86, 0, 0, 0,
    6, 34, 62, 90, 0, 0, 0,
    6, 28, 50, 72, 94, 0, 0,
    6, 26, 50, 74, 98, 0, 0,
    6, 30, 54, 78, 102, 0, 0,
    6, 28, 54, 80, 106, 0, 0,
    6, 32, 58, 84, 110, 0, 0,
    6, 30, 58, 86, 114, 0, 0,
    6, 34, 62, 90, 118, 0, 0,
    6, 26, 50, 74, 98, 122, 0,
    6, 30, 54, 78, 102, 126, 0,
    6, 26, 52, 78, 104, 130, 0,
    6, 30, 56, 82, 108, 134, 0,
    6, 34, 60, 86, 112, 138, 0,
    6, 30, 58, 86, 114, 142, 0,
    6, 34, 62, 90, 118, 146, 0,
    6, 30, 54, 78, 102, 126, 150,
    6, 24, 50, 76, 102, 128, 154,
    6, 28, 54, 80, 106, 132, 158,
    6, 32, 58, 84, 110, 136, 162,
    6, 26, 54, 82, 110, 138, 166,
    6, 30, 58, 86, 114, 142, 170);


  qr_annex_c : array [0..31] of FixedUInt = (
    // Format information bit sequences
    $5412, $5125, $5e7c, $5b4b, $45f9, $40ce, $4f97, $4aa0, $77c4, $72f3, $7daa, $789d,
    $662f, $6318, $6c41, $6976, $1689, $13be, $1ce7, $19d0, $0762, $0255, $0d0c, $083b,
    $355f, $3068, $3f31, $3a06, $24b4, $2183, $2eda, $2bed
    );

  qr_annex_d : array [0..33] of FixedInt = (
    // Version information bit sequences
    $07c94, $085bc, $09a99, $0a4d3, $0bbf6, $0c762, $0d847, $0e60d, $0f928, $10b78,
    $1145d, $12a17, $13532, $149a6, $15683, $168c9, $177ec, $18ec4, $191e1, $1afab,
    $1b08e, $1cc1a, $1d33f, $1ed75, $1f250, $209d5, $216f0, $228ba, $2379f, $24b0b,
    $2542e, $26a64, $27541, $28c69);

  qr_annex_c1 : array [0..31] of FixedInt = (
    // Micro QR Code format information
    $4445, $4172, $4e2b, $4b1c, $55ae, $5099, $5fc0, $5af7, $6793, $62a4, $6dfd, $68ca, $7678, $734f,
    $7c16, $7921, $06de, $03e9, $0cb0, $0987, $1735, $1202, $1d5b, $186c, $2508, $203f, $2f66, $2a51, $34e3,
    $31d4, $3e8d, $3bba);

const
  // Rectangular Micro QR Code (rMQR) - ISO/IEC 23941:2022
  //
  // The 32 rMQR symbol sizes are passed around as version numbers RMQR_VERSION + 0..31,
  // which keeps them distinguishable from the QR Code versions 1..40 sharing the same
  // helper routines (same convention as the C original).
  RMQR_VERSION = 41;

  // Table 1 - Codeword capacity of all versions of rMQR symbols
  rmqr_height : array [0..31] of FixedInt = (
     7,  7,  7,  7,  7,
     9,  9,  9,  9,  9,
    11, 11, 11, 11, 11, 11,
    13, 13, 13, 13, 13, 13,
    15, 15, 15, 15, 15,
    17, 17, 17, 17, 17);

  rmqr_width : array [0..31] of FixedInt = (
    43, 59, 77, 99, 139,
    43, 59, 77, 99, 139,
    27, 43, 59, 77,  99, 139,
    27, 43, 59, 77,  99, 139,
    43, 59, 77, 99, 139,
    43, 59, 77, 99, 139);

  rmqr_total_codewords : array [0..31] of FixedInt = (
    13, 21,  32,  44,  68,      // R7x
    21, 33,  49,  66,  99,      // R9x
    15, 31,  47,  67,  89, 132, // R11x
    21, 41,  60,  85, 113, 166, // R13x
    51, 74, 103, 136, 199,      // R15x
    61, 88, 122, 160, 232);     // R17x

  // Table 6 - Number of data codewords and input data capacity for rMQR
  rmqr_data_codewords_M : array [0..31] of FixedInt = (
     6,  12,  20,  28,  44,      // R7x
    12,  21,  31,  42,  63,      // R9x
     7,  19,  31,  43,  57,  84, // R11x
    12,  27,  38,  53,  73, 106, // R13x
    33,  48,  67,  88, 127,      // R15x
    39,  56,  78, 100, 152);     // R17x

  rmqr_data_codewords_H : array [0..31] of FixedInt = (
     3,   7,  10,  14,  24,      // R7x
     7,  11,  17,  22,  33,      // R9x
     5,  11,  15,  23,  29,  42, // R11x
     7,  13,  20,  29,  35,  54, // R13x
    15,  26,  31,  48,  69,      // R15x
    21,  28,  38,  56,  76);     // R17x

  // Table 8 - Error correction characteristics for rMQR
  rmqr_blocks_M : array [0..31] of FixedInt = (
    1, 1, 1, 1, 1,    // R7x
    1, 1, 1, 1, 2,    // R9x
    1, 1, 1, 1, 2, 2, // R11x
    1, 1, 1, 2, 2, 3, // R13x
    1, 1, 2, 2, 3,    // R15x
    1, 2, 2, 3, 4);   // R17x

  rmqr_blocks_H : array [0..31] of FixedInt = (
    1, 1, 1, 1, 2,    // R7x
    1, 1, 2, 2, 3,    // R9x
    1, 1, 2, 2, 2, 3, // R11x
    1, 1, 2, 2, 3, 4, // R13x
    2, 2, 3, 4, 5,    // R15x
    2, 2, 3, 4, 6);   // R17x

  // Highest index in rmqr_total_codewords for each given row (R7x, R9x etc)
  rmqr_fixed_height_upper_bound : array [0..6] of FixedInt = (
    -1, 4, 9, 15, 21, 26, 31);

  // Table 3 - Number of bits of character count indicator
  rmqr_numeric_cci : array [0..31] of FixedInt = (
    4, 5, 6, 7, 7,
    5, 6, 7, 7, 8,
    4, 6, 7, 7, 8, 8,
    5, 6, 7, 7, 8, 8,
    7, 7, 8, 8, 9,
    7, 8, 8, 8, 9);

  rmqr_alphanum_cci : array [0..31] of FixedInt = (
    3, 5, 5, 6, 6,
    5, 5, 6, 6, 7,
    4, 5, 6, 6, 7, 7,
    5, 6, 6, 7, 7, 8,
    6, 7, 7, 7, 8,
    6, 7, 7, 8, 8);

  rmqr_byte_cci : array [0..31] of FixedInt = (
    3, 4, 5, 5, 6,
    4, 5, 5, 6, 6,
    3, 5, 5, 6, 6, 7,
    4, 5, 6, 6, 7, 7,
    6, 6, 7, 7, 7,
    6, 6, 7, 7, 8);

  rmqr_kanji_cci : array [0..31] of FixedInt = (
    2, 3, 4, 5, 5,
    3, 4, 5, 5, 6,
    2, 4, 5, 5, 6, 6,
    3, 5, 5, 6, 6, 7,
    5, 5, 6, 6, 7,
    5, 6, 6, 6, 7);

  // Table D.1 - Column coordinates of centre module of alignment patterns
  rmqr_table_d1 : array [0..19] of FixedInt = (
    21,  0,  0,   0,
    19, 39,  0,   0,
    25, 51,  0,   0,
    23, 49, 75,   0,
    27, 55, 83, 111);

  // rMQR format information for finder pattern side
  rmqr_format_info_left : array [0..63] of FixedUInt = (
    $1FAB2, $1E597, $1DBDD, $1C4F8, $1B86C, $1A749, $19903, $18626, $17F0E, $1602B,
    $15E61, $14144, $13DD0, $122F5, $11CBF, $1039A, $0F1CA, $0EEEF, $0D0A5, $0CF80,
    $0B314, $0AC31, $0927B, $08D5E, $07476, $06B53, $05519, $04A3C, $036A8, $0298D,
    $017C7, $008E2, $3F367, $3EC42, $3D208, $3CD2D, $3B1B9, $3AE9C, $390D6, $38FF3,
    $376DB, $369FE, $357B4, $34891, $33405, $32B20, $3156A, $30A4F, $2F81F, $2E73A,
    $2D970, $2C655, $2BAC1, $2A5E4, $29BAE, $2848B, $27DA3, $26286, $25CCC, $243E9,
    $23F7D, $22058, $21E12, $20137);

  // rMQR format information for subfinder pattern side
  rmqr_format_info_right : array [0..63] of FixedUInt = (
    $20A7B, $2155E, $22B14, $23431, $248A5, $25780, $269CA, $276EF, $28FC7, $290E2,
    $2AEA8, $2B18D, $2CD19, $2D23C, $2EC76, $2F353, $30103, $31E26, $3206C, $33F49,
    $343DD, $35CF8, $362B2, $37D97, $384BF, $39B9A, $3A5D0, $3BAF5, $3C661, $3D944,
    $3E70E, $3F82B, $003AE, $01C8B, $022C1, $03DE4, $04170, $05E55, $0601F, $07F3A,
    $08612, $09937, $0A77D, $0B858, $0C4CC, $0DBE9, $0E5A3, $0FA86, $108D6, $117F3,
    $129B9, $1369C, $14A08, $1552D, $16B67, $17442, $18D6A, $1924F, $1AC05, $1B320,
    $1CFB4, $1D091, $1EEDB, $1F1FE);

function in_alpha(glyph : Byte) : NativeInt;
begin
  // Returns true if input glyph is in the Alphanumeric set
  var cglyph := Chr(glyph);

  if ((cglyph >= '0') and (cglyph <= '9')) or ((cglyph >= 'A') and (cglyph <= 'Z')) then
    Exit(1);

  case cglyph of
    ' ', '$', '%', '*', '+', '-', '.', '/', ':' : Result := 1;
  else
    Result := 0;
  end;
end;

function qr_mode_bits(version : NativeInt) : NativeInt;
begin
  // Number of bits of the mode indicator, based on version
  if (version < RMQR_VERSION) then
    Result := 4  // QR Code
  else
    Result := 3; // rMQR
end;

function qr_mode_indicator(version : NativeInt; mode : Char) : NativeInt;
begin
  // Mode indicator value, based on version
  Result := 0;

  if (version < RMQR_VERSION) then
  begin
    case mode of // QR Code
      'N': Result := 1;
      'A': Result := 2;
      'B': Result := 4;
      'K': Result := 8;
    end;
  end
  else
  begin
    case mode of // rMQR
      'N': Result := 1;
      'A': Result := 2;
      'B': Result := 3;
      'K': Result := 4;
    end;
  end;
end;

function qr_cci_bits(version : NativeInt; mode : Char) : NativeInt;
begin
  // Number of bits of the character count indicator, based on version and mode
  Result := 0;

  if (version < RMQR_VERSION) then
  begin
    if (version < 10) then
    begin
      case mode of
        'N': Result := 10;
        'A': Result := 9;
        'B': Result := 8;
        'K': Result := 8;
      end;
    end
    else
    if (version < 27) then
    begin
      case mode of
        'N': Result := 12;
        'A': Result := 11;
        'B': Result := 16;
        'K': Result := 10;
      end;
    end
    else
    begin
      case mode of
        'N': Result := 14;
        'A': Result := 13;
        'B': Result := 16;
        'K': Result := 12;
      end;
    end;
  end
  else
  begin
    case mode of
      'N': Result := rmqr_numeric_cci[version - RMQR_VERSION];
      'A': Result := rmqr_alphanum_cci[version - RMQR_VERSION];
      'B': Result := rmqr_byte_cci[version - RMQR_VERSION];
      'K': Result := rmqr_kanji_cci[version - RMQR_VERSION];
    end;
  end;
end;

function qr_terminator_bits(version : NativeInt) : NativeInt;
begin
  // Number of terminator bits, based on version
  if (version < RMQR_VERSION) then
    Result := 4  // QR Code
  else
    Result := 3; // rMQR
end;

function qr_data_codewords(ecc_level : NativeInt; version_index : NativeInt) : NativeInt;
begin
  // Number of data codewords of a QR Code version (0-based index), by error correction level
  case ecc_level of
    LEVEL_M: Result := qr_data_codewords_M[version_index];
    LEVEL_Q: Result := qr_data_codewords_Q[version_index];
    LEVEL_H: Result := qr_data_codewords_H[version_index];
  else
    Result := qr_data_codewords_L[version_index];
  end;
end;

function qr_mode_index(mode : Char) : NativeInt;
begin
  // Index of the mode in qr_mode_types
  for var j := 0 to QR_NUM_MODES - 1 do
    if qr_mode_types[j] = mode then
      Exit(j);

  Result := 0;
end;

function qr_is_alpha(glyph : NativeInt; gs1 : NativeInt) : Boolean;
begin
  // Returns true if the input glyph is in the Alphanumeric set or is the GS1 FNC1 marker
  if glyph > $ff then
    Result := False
  else
    Result := (in_alpha(Byte(glyph)) <> 0) or ((gs1 <> 0) and (glyph = Ord('[')));
end;

function qr_in_numeric(jisdata : TArrayOfInteger; _length : NativeInt; in_posn : NativeInt;
  var p_end : NativeInt; var p_cost : NativeInt) : Boolean;
begin
  // Whether in numeric or not. If in numeric, p_end is set to the position after the numeric
  // run, and p_cost to the per-numeric cost
  if (in_posn < p_end) then
    Exit(True);

  // Attempt to calculate the average 'cost' of using numeric mode in number of bits (times QR_MULT)
  var i := in_posn;
  while (i < _length) and (i < in_posn + 3) and (jisdata[i] >= Ord('0')) and (jisdata[i] <= Ord('9')) do
    inc(i);

  var digit_cnt := i - in_posn;

  if (digit_cnt = 0) then
  begin
    p_end := 0;
    Exit(False);
  end;

  p_end := i;
  case digit_cnt of
    1: p_cost := 24;  // 4 * QR_MULT
    2: p_cost := 21;  // (7 / 2) * QR_MULT
  else
    p_cost := 20;     // (10 / 3) * QR_MULT
  end;

  Result := True;
end;

function qr_in_alpha(jisdata : TArrayOfInteger; _length : NativeInt; in_posn : NativeInt;
  var p_end : NativeInt; var p_cost : NativeInt; var p_pcent : NativeInt; var p_pccnt : NativeInt;
  gs1 : NativeInt) : Boolean;
begin
  // Whether in alpha or not. If in alpha, p_end is set to the position after the alpha run, and
  // p_cost to the per-alpha cost. For GS1, p_pcent is set if the 2nd char is a percent
  var last := (in_posn + 1) = _length;
  var two_alphas := (not last) and qr_is_alpha(jisdata[in_posn + 1], gs1);

  // Attempt to calculate the average 'cost' of using alphanumeric mode in number of bits (times QR_MULT)
  if (in_posn < p_end) then
  begin
    if (gs1 <> 0) then
    begin
      if (p_pcent <> 0) then
      begin
        // The previous 2nd char was a percent, so allow for the second half of the doubled-up percent here
        // Uneven percents means this will fit evenly in an alpha pair
        if two_alphas or ((p_pccnt and 1) = 0) then
          p_cost := 66  // 11 * QR_MULT
        else
          p_cost := 69; // (11 / 2 + 6) * QR_MULT
        p_pcent := 0;
      end
      else
      begin
        // As above, uneven percents means this will fit in an alpha pair
        if (not last) or ((p_pccnt and 1) = 0) then
          p_cost := 33  // (11 / 2) * QR_MULT
        else
          p_cost := 36; // 6 * QR_MULT
      end;
    end
    else
    begin
      if not last then
        p_cost := 33
      else
        p_cost := 36;
    end;

    Exit(True);
  end;

  if not qr_is_alpha(jisdata[in_posn], gs1) then
  begin
    p_end := 0;
    p_pcent := 0;
    p_pccnt := 0;
    Exit(False);
  end;

  if (gs1 <> 0) and (jisdata[in_posn] = Ord('%')) then
  begin
    // Must double-up so counts as 2 chars
    p_end := in_posn + 1;
    p_pcent := 0;
    inc(p_pccnt);

    if two_alphas or ((p_pccnt and 1) = 0) then
      p_cost := 66
    else
      p_cost := 69;

    Exit(True);
  end;

  if two_alphas then
    p_end := in_posn + 2
  else
    p_end := in_posn + 1;

  if (gs1 <> 0) then
  begin
    if two_alphas and (jisdata[in_posn + 1] = Ord('%')) then // 2nd char is a percent
      p_pcent := 1
    else
      p_pcent := 0;
    inc(p_pccnt, p_pcent);

    if two_alphas or ((p_pccnt and 1) = 0) then
      p_cost := 33
    else
      p_cost := 36;
  end
  else
  begin
    if two_alphas then
      p_cost := 33
    else
      p_cost := 36;
  end;

  Result := True;
end;

procedure qr_head_costs(version : NativeInt; var costs : TQrCosts);
begin
  // Initial mode costs - the mode indicator plus the character count indicator.
  // Switching to a mode costs the same as heading with it.
  for var j := 0 to QR_NUM_MODES - 1 do
    costs[j] := (qr_cci_bits(version, qr_mode_types[j]) + qr_mode_bits(version)) * QR_MULT;
end;

procedure qr_define_modes(var mode : TArrayOfChar; jisdata : TArrayOfInteger; _length : NativeInt;
  gs1 : NativeInt; version : NativeInt);
{
  Calculate optimized encoding modes. Adapted from Project Nayuki.

  Copyright (c) 2019 Project Nayuki. (MIT License)
  https://www.nayuki.io/page/qr-code-generator-library

  Permission is hereby granted, free of charge, to any person obtaining a copy of
  this software and associated documentation files (the "Software"), to deal in
  the Software without restriction, including without limitation the rights to
  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
  the Software, and to permit persons to whom the Software is furnished to do so,
  subject to the following conditions:
  - The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.
}
var
  head, prev_costs, cur_costs : TQrCosts;
  char_modes : TArrayOfChar;
  numeric_end, numeric_cost : NativeInt;
  alpha_end, alpha_cost, alpha_pcent, alpha_pccnt : NativeInt;
begin
  if (_length <= 0) then
    Exit;

  numeric_end := 0;
  numeric_cost := 0;
  alpha_end := 0;
  alpha_cost := 0;
  alpha_pcent := 0;
  alpha_pccnt := 0;

  // char_modes[(i * QR_NUM_MODES) + j] is the mode to encode the code point at index i such that
  // the final segment ends in qr_mode_types[j] and the total number of bits is minimized over all
  // possible choices. SetLength zero-fills, which is the "not reachable" marker.
  SetLength(char_modes, QR_NUM_MODES * _length);

  qr_head_costs(version, head);

  // At the beginning of each iteration of the loop below, prev_costs[j] is the minimum number of
  // 1/QR_MULT bits needed to encode the entire string prefix of length i, and end in qr_mode_types[j]
  prev_costs := head;

  // Calculate costs using dynamic programming
  for var i := 0 to _length - 1 do
  begin
    FillChar(cur_costs, SizeOf(cur_costs), 0);

    if (jisdata[i] > $ff) then
    begin
      cur_costs[QR_B] := prev_costs[QR_B] + 96; // 16 * QR_MULT
      char_modes[(i * QR_NUM_MODES) + QR_B] := 'B';
      cur_costs[QR_K] := prev_costs[QR_K] + 78; // 13 * QR_MULT
      char_modes[(i * QR_NUM_MODES) + QR_K] := 'K';
    end
    else
    begin
      if qr_in_numeric(jisdata, _length, i, numeric_end, numeric_cost) then
      begin
        cur_costs[QR_N] := prev_costs[QR_N] + numeric_cost;
        char_modes[(i * QR_NUM_MODES) + QR_N] := 'N';
      end;

      if qr_in_alpha(jisdata, _length, i, alpha_end, alpha_cost, alpha_pcent, alpha_pccnt, gs1) then
      begin
        cur_costs[QR_A] := prev_costs[QR_A] + alpha_cost;
        char_modes[(i * QR_NUM_MODES) + QR_A] := 'A';
      end;

      cur_costs[QR_B] := prev_costs[QR_B] + 48; // 8 * QR_MULT
      char_modes[(i * QR_NUM_MODES) + QR_B] := 'B';
    end;

    // Start a new segment at the end to switch modes
    for var j := 0 to QR_NUM_MODES - 1 do     // To mode
      for var k := 0 to QR_NUM_MODES - 1 do   // From mode
        if (j <> k) and (char_modes[(i * QR_NUM_MODES) + k] <> #0) then
        begin
          var new_cost := cur_costs[k] + head[j]; // Switch costs are the same as head costs
          if (char_modes[(i * QR_NUM_MODES) + j] = #0) or (new_cost < cur_costs[j]) then
          begin
            cur_costs[j] := new_cost;
            char_modes[(i * QR_NUM_MODES) + j] := qr_mode_types[k];
          end;
        end;

    prev_costs := cur_costs;
  end;

  // Find the optimal ending mode
  var min_cost := prev_costs[0];
  var cur_mode := qr_mode_types[0];
  for var i := 1 to QR_NUM_MODES - 1 do
    if (prev_costs[i] < min_cost) then
    begin
      min_cost := prev_costs[i];
      cur_mode := qr_mode_types[i];
    end;

  // Get the optimal mode for each code point by tracing backwards
  for var i := _length - 1 downto 0 do
  begin
    cur_mode := char_modes[(i * QR_NUM_MODES) + qr_mode_index(cur_mode)];
    mode[i] := cur_mode;
  end;
end;

function qr_blockLength(start : NativeInt; mode : TArrayOfChar; _length : NativeInt) : NativeInt;
begin
  // Length of the run of equal modes starting at start
  var start_mode := mode[start];
  var count : NativeInt := 0;

  repeat
    inc(count);
  until not ((start + count < _length) and (mode[start + count] = start_mode));

  Result := count;
end;

function qr_calc_binlen(version : NativeInt; var mode : TArrayOfChar; jisdata : TArrayOfInteger; _length : NativeInt; gs1 : NativeInt) : NativeInt;
begin
  // Optimise the modes for this version, then calculate the actual bitlength of the
  // resulting binary string. mode[] is left holding the modes for this version.
  qr_define_modes(mode, jisdata, _length, gs1, version);

  var count : NativeInt := 0;
  var currentMode : Char := #0;

  if (gs1 <> 0) then
    inc(count, qr_mode_bits(version)); // FNC1

  for var i := 0 to _length - 1 do
  begin
    if (mode[i] <> currentMode) then
    begin
      inc(count, qr_mode_bits(version) + qr_cci_bits(version, mode[i]));
      var blocklength := qr_blockLength(i, mode, _length);

      case mode[i] of
        'K': inc(count, blocklength * 13);
        'B': begin
               // A double-byte value takes two bytes in byte mode
               for var j := i to i + blocklength - 1 do
                 if (jisdata[j] > $ff) then
                   inc(count, 16)
                 else
                   inc(count, 8);
             end;
        'A': begin
               var alphalength := blocklength;
               if (gs1 <> 0) then
               begin
                 // In alphanumeric mode % becomes %%
                 for var j := i to i + blocklength - 1 do
                   if (jisdata[j] = Ord('%')) then
                     inc(alphalength);
               end;

               if ((alphalength mod 2) = 0) then
                 inc(count, (alphalength div 2) * 11)
               else
                 inc(count, ((alphalength - 1) div 2) * 11 + 6);
             end;
        'N': begin
               case (blocklength mod 3) of
                 0: inc(count, (blocklength div 3) * 10);
                 1: inc(count, ((blocklength - 1) div 3) * 10 + 4);
                 2: inc(count, ((blocklength - 2) div 3) * 10 + 7);
               end;
             end;
      end;

      currentMode := mode[i];
    end;
  end;

  Result := count;
end;

procedure microqr_define_mode(var mode: TArrayOfChar; jisdata : TArrayOfInteger; _length : NativeInt; gs1 : NativeInt);
begin
  // The original run-length heuristic. QR Code and rMQR use the version-aware, cost-optimal
  // qr_define_modes instead; Micro QR keeps this one because its encoder is not parameterised
  // by version and so cannot re-optimise per candidate symbol.
  // Values placed into mode[] are: K = Kanji, B = Binary, A = Alphanumeric, N = Numeric
	for var i := 0 to _length-1 do
  begin
		if(jisdata[i] > $ff) then
			mode[i] := 'K'
		else
    begin
			mode[i] := 'B';
			if (in_alpha(jisdata[i])<>0) then mode[i] := 'A';
			if ((gs1<>0) and (Chr(jisdata[i]) = '[')) then  mode[i] := 'A';
			if ((Chr(jisdata[i]) >= '0') and (Chr(jisdata[i]) <= '9')) then  mode[i] := 'N';
    end;
  end;

	// If less than 6 numeric digits together then don't use numeric mode
	for var i := 0 to _length-1 do
  begin
		if (mode[i] = 'N') then
    begin
			if(((i <> 0) and (mode[i - 1] <> 'N')) or (i = 0)) then
      begin
				var mlen : NativeInt := 0;
				while (((mlen + i) < _length) and (mode[mlen + i] = 'N')) do
					inc(mlen);

				if(mlen < 6) then
        begin
					for var j := 0 to mlen-1 do
						mode[i + j] := 'A';
				end;
			end;
		end;
	end;

	// If less than 4 alphanumeric characters together then don't use alphanumeric mode
	for var i := 0 to _length-1 do
  begin
		if mode[i] = 'A' then
    begin
			if (((i <> 0) and (mode[i - 1] <> 'A')) or (i = 0)) then
      begin
				var mlen : NativeInt := 0;
				while (((mlen + i) < _length) and (mode[mlen + i] = 'A')) do
					inc(mlen);

				if(mlen < 6) then
        begin
					for var j := 0 to mlen-1 do
						mode[i + j] := 'B';
				end
			end
		end
	end
end;

procedure qr_binary(var datastream: TArrayOfInteger; version: NativeInt; target_binlen : NativeInt; mode : TArrayOfChar; jisdata : TArrayOfInteger; _length : NativeInt; gs1 : NativeInt; est_binlen : NativeInt);
var
  position, short_data_block_length, modebits : NativeInt;
  data_block : Char;
  percent : NativeInt;
  binary : TArrayOfChar;
begin
  // Convert input data to a binary stream and add padding
	position := 0;
  SetLength(binary, est_binlen + 12);
	strcpy(binary, '');

  modebits := qr_mode_bits(version);

	if (gs1<>0) then
		bscan(binary, 5, 1 shl (modebits - 1)); // FNC1

  {$IFDEF DEBUG_ZINT}
	for var i := 0 to _length-1 do
    write(Format('%s', [mode[i]]));

	writeln;
  {$ENDIF}
	percent := 0;

	repeat
		data_block := mode[position];
		short_data_block_length := 0;
		repeat
			inc(short_data_block_length);
    until not (((short_data_block_length + position) < _length) and (mode[position + short_data_block_length] = data_block));

    // Mode indicator
    bscan(binary, qr_mode_indicator(version, data_block), 1 shl (modebits - 1));

		case (data_block) of
			'K': begin
            // Kanji mode
            // Character count indicator
            bscan(binary, short_data_block_length, 1 shl (qr_cci_bits(version, 'K') - 1));

            {$IFDEF DEBUG_ZINT}writeln(Format('Kanji block (length %d)', [short_data_block_length]));{$ENDIF}

            // Character representation
            for var i := 0 to short_data_block_length-1 do
            begin
              var jis := jisdata[position + i];

              if (jis > $9fff) then dec(jis, $c140);
              var msb := (jis and $ff00) shr 4;
              var lsb := (jis and $ff);
              var prod := (msb * $c0) + lsb;

              bscan(binary, prod, $1000);

              {$IFDEF DEBUG_ZINT}write(Format('$%4X ', [prod]));{$ENDIF}
            end;

            {$IFDEF DEBUG_ZINT}writeln;{$ENDIF}
  				end;
			'B': begin
              // Byte mode
              // A double-byte value occupies two bytes, so counts twice in the count indicator
              var double_byte : NativeInt := 0;
              for var i := 0 to short_data_block_length - 1 do
                if (jisdata[position + i] > $ff) then
                  inc(double_byte);

              // Character count indicator
              bscan(binary, short_data_block_length + double_byte, 1 shl (qr_cci_bits(version, 'B') - 1));

              {$IFDEF DEBUG_ZINT}writeln(Format('Byte block (length %d)', [short_data_block_length + double_byte]));{$ENDIF}

              // Character representation
              for var i := 0 to short_data_block_length - 1 do
              begin
                var _byte := jisdata[position + i];

                if (gs1<>0) and (_byte = Ord('[')) then
                  _byte := $1d; // FNC1

                if (_byte > $ff) then
                  bscan(binary, _byte, $8000)
                else
                  bscan(binary, _byte, $80);

                {$IFDEF DEBUG_ZINT}write(Format('$%2X(%d) ', [_byte, _byte]));{$ENDIF}
              end;

              {$IFDEF DEBUG_ZINT}writeln;{$ENDIF}
           end;
			'A': begin
              // Alphanumeric mode
              // In GS1 mode a literal % is doubled up, so counts twice in the count indicator
              var percent_count : NativeInt := 0;
              if (gs1 <> 0) then
                for var i := 0 to short_data_block_length - 1 do
                  if (jisdata[position + i] = Ord('%')) then
                    inc(percent_count);

              // Character count indicator
              bscan(binary, short_data_block_length + percent_count, 1 shl (qr_cci_bits(version, 'A') - 1));

              {$IFDEF DEBUG_ZINT}Writeln(Format('Alpha block (length %d)', [short_data_block_length]));{$ENDIF}

              // Character representation
              var first, second, count, prod : NativeInt;
              var i : NativeInt := 0;
              while ( i < short_data_block_length ) do
              begin
                //first := 0;
                //second := 0;

                if(percent = 0) then
                begin
                  if(gs1<>0) and (jisdata[position + i] = ord('%')) then
                  begin
                    first := posn(RHODIUM, '%');
                    second := posn(RHODIUM, '%');
                    count := 2;
                    prod := (first * 45) + second;
                    inc(i);
                  end
                  else
                  begin
                    if(gs1<>0) and (jisdata[position + i] = ord('[')) then
                    begin
                      first := posn(RHODIUM, '%'); // FNC1
                    end
                    else
                    begin
                      first := posn(RHODIUM, Chr(jisdata[position + i]));
                    end;
                    count := 1;
                    inc(i);
                    prod := first;

                    if(mode[position + i] = 'A') then
                    begin
                      if (gs1<>0)  and (jisdata[position + i] = ord('%')) then
                      begin
                        second := posn(RHODIUM, '%');
                        count := 2;
                        prod := (first * 45) + second;
                        percent := 1;
                      end
                      else
                      begin
                        if(gs1<>0) and (jisdata[position + i] = ord('[')) then
                        begin
                          second := posn(RHODIUM, '%'); // FNC1
                        end
                        else
                        begin
                          second := posn(RHODIUM, Chr(jisdata[position + i]));
                        end;
                        count := 2;
                        inc(i);
                        prod := (first * 45) + second;
                      end;
                    end;
                  end;
                end
                else
                begin
                  first := posn(RHODIUM, '%');
                  count := 1;
                  inc(i);
                  prod := first;
                  percent := 0;

                  if(mode[position + i] = 'A') then
                  begin
                    if((gs1<>0) and (jisdata[position + i] = ord('%'))) then
                    begin
                      second := posn(RHODIUM, '%');
                      count := 2;
                      prod := (first * 45) + second;
                      percent := 1;
                    end
                    else
                    begin
                      if(gs1<>0) and (jisdata[position + i] = ord('[')) then
                      begin
                        second := posn(RHODIUM, '%'); // FNC1
                      end
                      else
                      begin
                        second := posn(RHODIUM, Chr(jisdata[position + i]));
                      end;
                      count := 2;
                      inc(i);
                      prod := (first * 45) + second;
                    end;
                  end;
                end;

                if count=2 then
                  bscan(binary, prod, $400)
                else
                  bscan(binary, prod, $20);

                {$IFDEF DEBUG_ZINT}write(Format('$%4X ', [prod]));{$ENDIF}
              end;;

              {$IFDEF DEBUG_ZINT}writeln;{$ENDIF}
				end;
			'N': begin
              // Numeric mode
              // Character count indicator
              bscan(binary, short_data_block_length, 1 shl (qr_cci_bits(version, 'N') - 1));

              {$IFDEF DEBUG_ZINT}writeln(Format('Number block (length %d)', [short_data_block_length]));{$ENDIF}

              // Character representation
              var first, second, third, count, prod : NativeInt;
              var i : NativeInt := 0;
              while ( i < short_data_block_length ) do
              begin
                //first := 0;
                //second := 0;
                //third := 0;

                first := posn(NEON, Chr(jisdata[position + i]));
                count := 1;
                prod := first;

                if(mode[position + i + 1] = 'N') then
                begin
                  second := posn(NEON, Chr(jisdata[position + i + 1]));
                  count := 2;
                  prod := (prod * 10) + second;

                  if(mode[position + i + 2] = 'N') then
                  begin
                    third := posn(NEON, Chr(jisdata[position + i + 2]));
                    count := 3;
                    prod := (prod * 10) + third;
                  end;
                end;

                bscan(binary, prod, 1 shl (3 * count)); // count = 1..3

                {$IFDEF DEBUG_ZINT}write(Format('$%4X (%d)', [prod, prod]));{$ENDIF}

                inc(i, count);
              end;

              {$IFDEF DEBUG_ZINT}writeln;{$ENDIF}
				end;
		end;

		inc(position, short_data_block_length);
	until not (position < _length) ;

	// Terminator
	var current_binlen := strlen(binary);

	var termbits := 8 - (current_binlen mod 8);
	if (termbits = 8) then termbits := 0;
	var current_bytes := (current_binlen + termbits) div 8;

	if (termbits <> 0) or (current_bytes < target_binlen) then
	begin
		// A short terminator is only allowed when the symbol is exactly filled
		var max_termbits := qr_terminator_bits(version);
		if not ((termbits < max_termbits) and (current_bytes = target_binlen)) then
			termbits := max_termbits;

		for var i := 0 to termbits - 1 do
			concat(binary, '0');
	end;

	current_binlen := strlen(binary);

	var padbits := 8 - (current_binlen mod 8);
	if(padbits = 8) then padbits := 0;
	current_bytes := (current_binlen + padbits) div 8;

	// Padding bits
	for var i := 0 to padbits-1 do
		concat(binary, '0');

	// Put data into 8-bit codewords
	for var i := 0 to current_bytes-1 do
  begin
		datastream[i] := $00;
		if(binary[i * 8] = '1') then inc(datastream[i], $80);
		if(binary[i * 8 + 1] = '1') then inc(datastream[i], $40);
		if(binary[i * 8 + 2] = '1') then inc(datastream[i], $20);
		if(binary[i * 8 + 3] = '1') then inc(datastream[i], $10);
		if(binary[i * 8 + 4] = '1') then inc(datastream[i], $08);
		if(binary[i * 8 + 5] = '1') then inc(datastream[i], $04);
		if(binary[i * 8 + 6] = '1') then inc(datastream[i], $02);
		if(binary[i * 8 + 7] = '1') then inc(datastream[i], $01);
	end;

	// Add pad codewords
	var toggle : NativeInt := 0;
	for var i := current_bytes to target_binlen - 1 do
  begin
		if(toggle = 0) then
    begin
			datastream[i] := $ec;
			toggle := 1;
		end
    else
    begin
			datastream[i] := $11;
			toggle := 0;
		end;
	end;

  {$IFDEF DEBUG_ZINT}
	writeln('Resulting codewords:');
	for i := 0 to target_binlen-1 do
  begin
		write(Format('$%2X ', [datastream[i]]));
	end;
	writeln;
  {$ENDIF}
end;

procedure add_ecc(var fullstream: TArrayOfInteger; datastream: TArrayOfInteger; total_cw : NativeInt; data_cw : NativeInt; blocks : NativeInt);
var
  data_block, ecc_block : TArrayOfByte;
  interleaved_data, interleaved_ecc : TArrayOfInteger;
  RSGlobals : TRSGlobals;
begin
	// Split data into blocks, add error correction and then interleave the blocks and error correction data
	var ecc_cw := total_cw - data_cw;
	var short_data_block_length := data_cw div blocks;
	var qty_long_blocks := data_cw mod blocks;
	var qty_short_blocks := blocks - qty_long_blocks;
	var ecc_block_length := ecc_cw div blocks;
	
  SetLength(data_block, short_data_block_length + 2);
	SetLength(ecc_block, ecc_block_length + 2);
	SetLength(interleaved_data, data_cw + 2);
	SetLength(interleaved_ecc, ecc_cw + 2);

	var posn : NativeInt := 0;

	for var i := 0 to blocks-1 do
  begin
		var length_this_block : NativeInt;
		if(i < qty_short_blocks) then
      length_this_block := short_data_block_length
    else
      length_this_block := short_data_block_length + 1;

		for var j := 0 to ecc_block_length-1 do
			ecc_block[j] := 0;

		for var j := 0 to length_this_block-1 do
      data_block[j] := datastream[posn + j];

		rs_init_gf($11d, RSGlobals);
		rs_init_code(ecc_block_length, 0, RSGlobals);
		rs_encode(length_this_block, data_block, ecc_block, RSGlobals);
		rs_free(RSGlobals);

    {$IFDEF DEBUG_ZINT}
		write(Format('Block %d: ', [i + 1]));
		for var j := 0 to length_this_block-1 do
			write(Format('%2X ', [data_block[j]]));

		if(i < qty_short_blocks) then
			write('   ');

		write(' // ');

		for var j := 0 to ecc_block_length-1 do
			write(Format('%2X ', [ecc_block[ecc_block_length - j - 1]]));

		writeln;
    {$ENDIF}

		for var j := 0 to short_data_block_length-1 do
    	interleaved_data[(j * blocks) + i] := data_block[j];

		if i >= qty_short_blocks then
			interleaved_data[(short_data_block_length * blocks) + (i - qty_short_blocks)] := data_block[short_data_block_length];

		for var j := 0 to ecc_block_length-1 do
			interleaved_ecc[(j * blocks) + i] := ecc_block[ecc_block_length - j - 1];

		inc(posn, length_this_block);
	end;

	for var j := 0 to data_cw - 1 do
		fullstream[j] := interleaved_data[j];

	for var j := 0 to ecc_cw - 1 do
		fullstream[j + data_cw] := interleaved_ecc[j];

	{$IFDEF DEBUG_ZINT}
  writeln;
	writeln('Data Stream: ');
	for var j := 0 to (data_cw + ecc_cw)-1 do
    write(Format('%2X ', [fullstream[j]]));
	writeln;
  {$ENDIF}
end;

procedure place_finder(var grid : TArrayOfByte; size : NativeInt; x : NativeInt; y : NativeInt);
const
  finder : array [0..48] of byte = (
		1, 1, 1, 1, 1, 1, 1,
		1, 0, 0, 0, 0, 0, 1,
		1, 0, 1, 1, 1, 0, 1,
		1, 0, 1, 1, 1, 0, 1,
		1, 0, 1, 1, 1, 0, 1,
		1, 0, 0, 0, 0, 0, 1,
		1, 1, 1, 1, 1, 1, 1
	);
begin

	for var xp := 0 to 6 do
  begin
		for var yp := 0 to 6 do
    begin
			if (finder[xp + (7 * yp)] = 1) then
      	grid[((yp + y) * size) + (xp + x)] := $11
      else
				grid[((yp + y) * size) + (xp + x)] := $10;
    end
  end;
end;

procedure place_align(var grid : TArrayOfByte; size : NativeInt; x : NativeInt; y : NativeInt);
const
  alignment : array [0..24] of byte = (
		1, 1, 1, 1, 1,
		1, 0, 0, 0, 1,
		1, 0, 1, 0, 1,
		1, 0, 0, 0, 1,
		1, 1, 1, 1, 1
	);
begin

	dec(x, 2);
	dec(y, 2); // Input values represent centre of pattern

	for var xp := 0 to 4 do
  begin
		for var yp := 0 to 4 do
    begin
			if (alignment[xp + (5 * yp)] = 1) then
				grid[((yp + y) * size) + (xp + x)] := $11
			else
				grid[((yp + y) * size) + (xp + x)] := $10;
    end;
  end;
end;

procedure setup_grid(var grid : TArrayOfByte; size : NativeInt; version : NativeInt);
var
  toggle : NativeInt;
  loopsize, xcoord, ycoord : NativeInt;
begin
	toggle := 1;

	//Add timing patterns
	for var i := 0 to size-1 do
  begin
		if(toggle = 1) then
     begin
			grid[(6 * size) + i] := $21;
			grid[(i * size) + 6] := $21;
			toggle := 0;
		end
    else
    begin
			grid[(6 * size) + i] := $20;
			grid[(i * size) + 6] := $20;
			toggle := 1;
		end;
	end;

	// Add finder patterns
	place_finder(grid, size, 0, 0);
	place_finder(grid, size, 0, size - 7);
	place_finder(grid, size, size - 7, 0);

	// Add separators
	for var i := 0 to 6 do
  begin
		grid[(7 * size) + i] := $10;
		grid[(i * size) + 7] := $10;
		grid[(7 * size) + (size - 1 - i)] := $10;
		grid[(i * size) + (size - 8)] := $10;
		grid[((size - 8) * size) + i] := $10;
		grid[((size - 1 - i) * size) + 7] := $10;
	end;
	grid[(7 * size) + 7] := $10;
	grid[(7 * size) + (size - 8)] := $10;
	grid[((size - 8) * size) + 7] := $10;

	// Add alignment patterns
	if(version <> 1) then
  begin
		// Version 1 does not have alignment patterns

		loopsize := qr_align_loopsize[version - 1];
		for var x := 0 to loopsize - 1 do
    begin
			for var y := 0 to loopsize - 1 do
      begin
				xcoord := qr_table_e1[((version - 2) * 7) + x];
				ycoord := qr_table_e1[((version - 2) * 7) + y];

				if not ((grid[(ycoord * size) + xcoord] and $10) <> 0) then
        begin
					place_align(grid, size, xcoord, ycoord);
				end;
			end;
		end;
	end;

	// Reserve space for format information
	for var i := 0 to 7 do
  begin
		inc(grid[(8 * size) + i], $20);
		inc(grid[(i * size) + 8], $20);
		grid[(8 * size) + (size - 1 - i)] := $20;
		grid[((size - 1 - i) * size) + 8] := $20;
	end;
	inc(grid[(8 * size) + 8], 20);
	grid[((size - 1 - 7) * size) + 8] := $21; // Dark Module from Figure 25

	// Reserve space for version information
	if (version >= 7) then
  begin
		for var i := 0 to 5 do
    begin
			grid[((size - 9) * size) + i] := $20;
			grid[((size - 10) * size) + i] := $20;
			grid[((size - 11) * size) + i] := $20;
			grid[(i * size) + (size - 9)] := $20;
			grid[(i * size) + (size - 10)] := $20;
			grid[(i * size) + (size - 11)] := $20;
		end;
	end;
end;

function cwbit(datastream : TArrayOfInteger; i : NativeInt) : NativeInt;
var
  _word, _bit : NativeInt;
begin
	_word := i shr 3;
	_bit := 7 - (i and 7);

	Result:=(datastream[_word] shr _bit) and 1;
end;

procedure populate_grid(var grid : TArrayOfByte; h_size : NativeInt; v_size : NativeInt; datastream : TArrayOfInteger; cw : NativeInt);
var
  i, n, y : NativeInt;
begin
  var not_rmqr := (v_size = h_size);
  // For rMQR allow for the righthand vertical timing pattern
  var x_start : NativeInt;
  if not_rmqr then
    x_start := h_size - 2
  else
    x_start := h_size - 3;

	var direction : NativeInt := 1; // up
	var row : NativeInt := 0; // right hand side

	n := cw * 8;
	y := v_size - 1;
	i := 0;
	repeat
		var x := x_start - (row * 2);
    var r := y * h_size;

		if (x < 6) and not_rmqr then
			dec(x); // skip over vertical timing pattern

		if not ((grid[r + (x + 1)] and $f0) <> 0) then
    begin
			if (cwbit(datastream, i)<>0) then
      	grid[r + (x + 1)] := $01
			else
				grid[r + (x + 1)] := $00;

			inc(i);
		end;

		if(i < n) then
    begin
			if not ((grid[r + x] and $f0) <> 0) then
      begin
				if (cwbit(datastream, i)<>0) then
					grid[r + x] := $01
				else
					grid[r + x] := $00;

				inc(i);
			end;
		end;

		if(direction<>0) then dec(y) else inc(y);
		if(y = -1) then
    begin
			// reached the top
			inc(row);
			y := 0;
			direction := 0;
		end;
		if(y = v_size) then
    begin
			// reached the bottom
			inc(row);
			y := v_size - 1;
			direction := 1;
		end;
	until not (i < n);
end;

function evaluate(var grid : TArrayOfByte; size : NativeInt; pattern : NativeInt) : NativeInt;
var
  block : NativeInt;
  _result : NativeInt;
  state : Char;
  p : NativeInt;
  dark_mods : NativeInt;
  percentage, k : NativeInt;
  local : TArrayOfChar;
begin
	_result := 0;

	SetLength(local, size * size);

	for var x := 0 to size - 1 do
  begin
		for var y := 0 to size - 1 do
    begin
			case pattern of
				0: if (grid[(y * size) + x] and $01)<>0 then local[(y * size) + x] := '1' else local[(y * size) + x] := '0';
				1: if (grid[(y * size) + x] and $02)<>0 then local[(y * size) + x] := '1' else local[(y * size) + x] := '0';
				2: if (grid[(y * size) + x] and $04)<>0 then local[(y * size) + x] := '1' else local[(y * size) + x] := '0';
				3: if (grid[(y * size) + x] and $08)<>0 then local[(y * size) + x] := '1' else local[(y * size) + x] := '0';
				4: if (grid[(y * size) + x] and $10)<>0 then local[(y * size) + x] := '1' else local[(y * size) + x] := '0';
				5: if (grid[(y * size) + x] and $20)<>0 then local[(y * size) + x] := '1' else local[(y * size) + x] := '0';
				6: if (grid[(y * size) + x] and $40)<>0 then local[(y * size) + x] := '1' else local[(y * size) + x] := '0';
				7: if (grid[(y * size) + x] and $80)<>0 then local[(y * size) + x] := '1' else local[(y * size) + x] := '0';
			end;
		end;
	end;

	// Test 1: Adjacent modules in row/column in same colour
	// Vertical
	for var x := 0 to size - 1 do
  begin
		state := local[x];
		block := 0;
		for var y := 0 to size - 1 do
    begin
			if (local[(y * size) + x] = state) then
        inc(block)
			else
      begin
				if(block > 5) then
					inc(_result, (3 + block));

				block := 0;
				state := local[(y * size) + x];
			end;
		end;
		if(block > 5) then
			inc(_result, (3 + block));
	end;

	// Horizontal
	for var y := 0 to size - 1 do
  begin
		state := local[y * size];
		block := 0;
		for var x := 0 to size - 1 do
    begin
			if(local[(y * size) + x] = state) then
				inc(block)
      else
      begin
				if(block > 5) then
					inc(_result,  (3 + block));

				block := 0;
				state := local[(y * size) + x];
			end;
		end;
		if(block > 5) then
			inc(_result, (3 + block));
	end;

	// Test 2 is not implimented

	// Test 3: 1:1:3:1:1 ratio pattern in row/column
	// Vertical
	for var x := 0 to size - 1 do
  begin
		for var y := 0 to (size - 7) - 1 do
    begin
			p := 0;
			if(local[(y * size) + x] = '1') then inc(p, $40);
			if(local[((y + 1) * size) + x] = '1') then inc(p, $20);
			if(local[((y + 2) * size) + x] = '1') then inc(p, $10);
			if(local[((y + 3) * size) + x] = '1') then inc(p, $08);
			if(local[((y + 4) * size) + x] = '1') then inc(p, $04);
			if(local[((y + 5) * size) + x] = '1') then inc(p, $02);
			if(local[((y + 6) * size) + x] = '1') then inc(p, $01);
			if(p = $5d) then
				inc(_result, 40);
		end;
	end;

	// Horizontal
	for var y := 0 to size - 1 do
  begin
		for var x := 0 to (size - 7) - 1 do
    begin
			p := 0;
			if(local[(y * size) + x] = '1') then inc(p, $40);
			if(local[(y * size) + x + 1] = '1') then inc(p, $20);
			if(local[(y * size) + x + 2] = '1') then inc(p, $10);
			if(local[(y * size) + x + 3] = '1') then inc(p, $08);
			if(local[(y * size) + x + 4] = '1') then inc(p, $04);
			if(local[(y * size) + x + 5] = '1') then inc(p, $02);
			if(local[(y * size) + x + 6] = '1') then inc(p, $01);
			if(p = $5d) then
				inc(_result, 40);
		end;
	end;

	// Test 4: Proportion of dark modules in entire symbol
	dark_mods := 0;
	for var x := 0 to size - 1 do
  begin
		for var y := 0 to size - 1 do
    begin
			if (local[(y * size) + x] = '1') then
				inc(dark_mods);
		end;
	end;
	percentage := 100 * (dark_mods div (size * size));
	if(percentage <= 50) then
  	k := ((100 - percentage) - 50) div 5
	else
		k := (percentage - 50) div 5;

	inc(_result, 10 * k);

	Result:= _result;
end;

function apply_bitmask(var grid : TArrayOfByte; size : NativeInt) : NativeInt;
var
  p : Byte;
  penalty : TArrayOfInteger;
  best_val, best_pattern : NativeInt;
  bit : NativeInt;
  mask, eval : TArrayOfByte;
begin
  SetLength(penalty, 8);;
  SetLength(mask, size * size);
	SetLength(eval, size * size);

	// Perform data masking
	for var x := 0 to size-1 do
  begin
		for var y := 0 to size - 1 do
    begin
			mask[(y * size) + x] := $00;

			if not ((grid[(y * size) + x] and $f0) <> 0) then
      begin
				if(((y + x) and 1) = 0) then inc(mask[(y * size) + x], $01);
				if((y and 1) = 0) then inc(mask[(y * size) + x], $02);
				if((x mod 3) = 0) then inc(mask[(y * size) + x], $04);
				if(((y + x) mod 3) = 0) then inc(mask[(y * size) + x], $08);
				if((((y div 2) + (x div 3)) and 1) = 0) then inc(mask[(y * size) + x], $10);
				if((((y * x) and 1) + ((y * x) mod 3)) = 0) then inc(mask[(y * size) + x], $20);
				if(((((y * x) and 1) + ((y * x) mod 3)) and 1) = 0) then inc(mask[(y * size) + x], $40);
				if(((((y + x) and 1) + ((y * x) mod 3)) and 1) = 0) then inc(mask[(y * size) + x], $80);
			end;
		end;
	end;

	for var x := 0 to size - 1 do
  begin
		for var y := 0 to size - 1 do
    begin
			if(grid[(y * size) + x] and $01)<>0 then p := $ff else p := $00;

			eval[(y * size) + x] := mask[(y * size) + x] xor p;
		end;
	end;


	// Evaluate result
	for var pattern := 0 to 7 do
  	penalty[pattern] := evaluate(eval, size, pattern);

	best_pattern := 0;
	best_val := penalty[0];
	for var pattern := 1 to 7 do
  begin
		if(penalty[pattern] < best_val) then
    begin
			best_pattern := pattern;
			best_val := penalty[pattern];
		end;
	end;

	// Apply mask
	for var x := 0 to size - 1 do
  begin
		for var y := 0 to size - 1 do
    begin
			bit := 0;
			case (best_pattern) of
				0: if(mask[(y * size) + x] and $01)<>0 then bit := 1;
				1: if(mask[(y * size) + x] and $02)<>0 then bit := 1;
				2: if(mask[(y * size) + x] and $04)<>0 then bit := 1;
				3: if(mask[(y * size) + x] and $08)<>0 then bit := 1;
				4: if(mask[(y * size) + x] and $10)<>0 then bit := 1;
				5: if(mask[(y * size) + x] and $20)<>0 then bit := 1;
				6: if(mask[(y * size) + x] and $40)<>0 then bit := 1;
				7: if(mask[(y * size) + x] and $80)<>0 then bit := 1;
			end;
			if(bit = 1) then
      begin
				if(grid[(y * size) + x] and $01)<>0 then
        	grid[(y * size) + x] := $00
				else
					grid[(y * size) + x] := $01;
			end;
		end;
	end;

	Result:=best_pattern;
end;

procedure add_format_info(var grid : TArrayOfByte; size : NativeInt; ecc_level: NativeInt; pattern : NativeInt);
var
  format : NativeInt;
  seq : Cardinal;
begin
	// Add format information to grid
	format := pattern;

	case(ecc_level) of
		LEVEL_L: inc(format, $08);
		LEVEL_Q: inc(format, $18);
		LEVEL_H: inc(format, $10);
	end;

	seq := qr_annex_c[format];

	for var i := 0 to 5 do
		inc(grid[(i * size) + 8], (seq shr i) and $01);

	for var i := 0 to 7 do
		inc(grid[(8 * size) + (size - i - 1)], (seq shr i) and $01);

	for var i := 0 to 5 do
  	inc(grid[(8 * size) + (5 - i)], (seq shr (i + 9)) and $01);

	for var i := 0 to 6 do
		inc(grid[(((size - 7) + i) * size) + 8], (seq shr (i + 8)) and $01);

	inc(grid[(7 * size) + 8], (seq shr 6) and $01);
	inc(grid[(8 * size) + 8], (seq shr 7) and $01);
	inc(grid[(8 * size) + 7], (seq shr 8) and $01);
end;

procedure add_version_info(var grid : TArrayOfByte; size : NativeInt; version: NativeInt);
var
  version_data : NativeInt;
begin
	// Add version information

	version_data := qr_annex_d[version - 7];
	for var i := 0 to 5 do
  begin
		inc(grid[((size - 11) * size) + i], (version_data shr (i * 3)) and $01);
		inc(grid[((size - 10) * size) + i], (version_data shr ((i * 3) + 1)) and $01);
		inc(grid[((size - 9) * size) + i], (version_data shr ((i * 3) + 2)) and $01);
		inc(grid[(i * size) + (size - 11)], (version_data shr (i * 3)) and$01);
		inc(grid[(i * size) + (size - 10)], (version_data shr ((i * 3) + 1)) and $01);
		inc(grid[(i * size) + (size - 9)], (version_data shr ((i * 3) + 2)) and $01);
	end;
end;

function qr_prep_data(symbol : zint_symbol; source : TArrayOfByte; var _length : FixedInt; var jisdata : TArrayOfInteger) : NativeInt;
var
  i, j, glyph, error_number : NativeInt;
  utfdata : TArrayOfInteger;
begin
  // Process the source data into the Shift-JIS values expected by the QR family
	SetLength(utfdata, _length + 1);

	case(symbol.input_mode) of
		DATA_MODE: begin
			for i := 0 to _length-1 do
      	jisdata[i] := source[i];
			end;
    else
		  // Convert Unicode input to Shift-JIS
			error_number := utf8toutf16(symbol, source, utfdata, _length);
			if (error_number <> 0) then begin
        Result:=error_number;
        exit;
      end;

			for  i := 0 to _length-1 do
      begin
				if(utfdata[i] <= $ff) then
					jisdata[i] := utfdata[i]
        else
        begin
					j := 0;
					glyph := 0;
					repeat
						if(sjis_lookup[j * 2] = utfdata[i]) then
							glyph := sjis_lookup[(j * 2) + 1];

						inc(j);
					until not ((j < 6843) and (glyph = 0));
					if (glyph = 0) then
          begin
						strcpy(symbol.errtxt, 'Invalid character in input data');
						Result:=ZERROR_INVALID_DATA;
            Exit;
					end;
					jisdata[i] := glyph;
				end;
			end;
  end;

  Result := 0;
end;

function qr_code(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;
var
  error_number : NativeInt;
  est_binlen, prev_est_binlen : NativeInt;
  ecc_level, autosize, version, max_cw, target_binlen, blocks, size : NativeInt;
  bitmask, gs1 : NativeInt;
  canShrink : Boolean;
  jisdata : TArrayOfInteger;
  mode, prev_mode : TArrayOfChar;
  datastream, fullstream : TArrayOfInteger;
  grid : TArrayOfByte;
begin
	SetLength(jisdata, _length + 1);
	SetLength(mode, _length + 1);

  if symbol.input_mode = GS1_MODE then
  	gs1 := 1
  else
    gs1 := 0;

  error_number := qr_prep_data(symbol, source, _length, jisdata);
  if (error_number <> 0) then
  begin
    Result := error_number;
    Exit;
  end;

	est_binlen := qr_calc_binlen(40, mode, jisdata, _length, gs1);

	ecc_level := LEVEL_L;
	max_cw := 2956;
	if((symbol.option_1 >= 1) and (symbol.option_1 <= 4)) then
  begin
		case (symbol.option_1) of
			1: begin ecc_level := LEVEL_L; max_cw := 2956; end;
			2: begin ecc_level := LEVEL_M; max_cw := 2334; end;
			3: begin ecc_level := LEVEL_Q; max_cw := 1666; end;
			4: begin ecc_level := LEVEL_H; max_cw := 1276; end;
		end;
	end;

	if(est_binlen > (8 * max_cw)) then
  begin
		strcpy(symbol.errtxt, 'Input too long for selected error correction level');
		Result:=ZERROR_TOO_LONG;
    Exit;
	end;

	autosize := 40;
	for var i := 39 downto 0 do
		if ((8 * qr_data_codewords(ecc_level, i)) >= est_binlen) then
			autosize := i + 1;

	if (autosize <> 40) then
	begin
		// Keep the version 40 estimate in case re-optimising for the lower version comes out worse
		prev_est_binlen := est_binlen;
		est_binlen := qr_calc_binlen(autosize, mode, jisdata, _length, gs1);
		if (prev_est_binlen < est_binlen) then
			est_binlen := qr_calc_binlen(40, mode, jisdata, _length, gs1);
	end;

	// Now see if the optimised binary will fit in a smaller symbol
	SetLength(prev_mode, _length + 1);
	canShrink := True;
	while canShrink do
	begin
		if (autosize = 1) then
			canShrink := False
		else
		begin
			prev_est_binlen := est_binlen;
			for var i := 0 to _length - 1 do
				prev_mode[i] := mode[i];

			est_binlen := qr_calc_binlen(autosize - 1, mode, jisdata, _length, gs1);

			if ((8 * qr_data_codewords(ecc_level, autosize - 2)) < est_binlen) then
				canShrink := False;

			if canShrink then
				// The optimisation worked - the data will fit in a smaller symbol
				dec(autosize)
			else
			begin
				// The data did not fit in the smaller symbol, revert to the original size
				est_binlen := prev_est_binlen;
				for var i := 0 to _length - 1 do
					mode[i] := prev_mode[i];
			end;
		end;
	end;

	version := autosize;

	if((symbol.option_2 >= 1) and (symbol.option_2 <= 40) and (symbol.option_2 > version)) then
	begin
		// The user selected a larger symbol than the smallest available, so re-optimise for it
		version := symbol.option_2;
		est_binlen := qr_calc_binlen(version, mode, jisdata, _length, gs1);
	end;

	// Ensure maxium error correction capacity
	if(est_binlen <= qr_data_codewords_M[version - 1]) then ecc_level := LEVEL_M;
	if(est_binlen <= qr_data_codewords_Q[version - 1]) then ecc_level := LEVEL_Q;
	if(est_binlen <= qr_data_codewords_H[version - 1]) then ecc_level := LEVEL_H;

	target_binlen := qr_data_codewords_L[version - 1]; blocks := qr_blocks_L[version - 1];
	case(ecc_level) of
		LEVEL_M: begin target_binlen := qr_data_codewords_M[version - 1]; blocks := qr_blocks_M[version - 1]; end;
		LEVEL_Q: begin target_binlen := qr_data_codewords_Q[version - 1]; blocks := qr_blocks_Q[version - 1]; end;
		LEVEL_H: begin target_binlen := qr_data_codewords_H[version - 1]; blocks := qr_blocks_H[version - 1]; end;
	end;

	SetLength(datastream, target_binlen + 1);
	SetLength(fullstream, qr_total_codewords[version - 1] + 1);

	qr_binary(datastream, version, target_binlen, mode, jisdata, _length, gs1, est_binlen);
	add_ecc(fullstream, datastream, qr_total_codewords[version - 1], target_binlen, blocks);

	size := qr_sizes[version - 1];
  SetLength(grid, size * size);

	for var i := 0 to size - 1 do
		for var j := 0 to size - 1 do
			grid[(i * size) + j] := 0;

	setup_grid(grid, size, version);
	populate_grid(grid, size, size, fullstream, qr_total_codewords[version - 1]);
	bitmask := apply_bitmask(grid, size);
	add_format_info(grid, size, ecc_level, bitmask);
	if (version >= 7) then
		add_version_info(grid, size, version);

	symbol.width := size;
	symbol.rows := size;

	for var i := 0 to size - 1 do
  begin
		for var j := 0 to size - 1 do
			if (grid[(i * size) + j] and $01)<>0 then
				set_module(symbol, i, j);


		symbol.row_height[i] := 1;
	end;

	Result:=0;
end;

// NOTE: From this point forward concerns Micro QR Code only

function micro_qr_intermediate(var binary : TArrayOfChar; jisdata : TArrayOfInteger; mode : TArrayOfChar; _length : NativeInt; var kanji_used : NativeInt; var alphanum_used : NativeInt; var byte_used : NativeInt) : NativeInt;
var
  position : NativeInt;
  short_data_block_length, i : NativeInt;
  data_block : Char;
  buffer : TArrayOfChar;
  jis, _byte : NativeInt;
  msb, lsb, prod : NativeInt;
  count, first, second, third : NativeInt;
begin
	// Convert input data to an 'intermediate stage' where data is binary encoded but control information is not
	position := 0;
  SetLength(buffer, 2);

	strcpy(binary, '');

	{$IFDEF DEBUG_ZINT}
	for i := 0 to _length - 1 do
    write(Format('%s', [mode[i]]));
	writeln;
  {$ENDIF}

	repeat
		if(strlen(binary) > 128) then
    begin
			Result:=ZERROR_TOO_LONG;
      Exit;
		end;

		data_block := mode[position];
		short_data_block_length := 0;
		repeat
			inc(short_data_block_length);
		until not (((short_data_block_length + position) < _length) and (mode[position + short_data_block_length] = data_block));

		case (data_block) of
			'K': begin
            // Kanji mode
            // Mode indicator
            concat(binary, 'K');
            kanji_used := 1;

            // Character count indicator
            buffer[0] := Chr(short_data_block_length);
            buffer[1] := #0;
            concat(binary, buffer);

            {$IFDEF DEBUG_ZINT}writeln(Format('Kanji block (length %d)', [short_data_block_length]));{$ENDIF}

            // Character representation
            for i := 0 to short_data_block_length - 1 do
            begin
              jis := jisdata[position + i];

              if(jis > $9fff) then dec(jis, $c140);
              msb := (jis and $ff00) shr 4;
              lsb := (jis and $ff);
              prod := (msb * $c0) + lsb;

              bscan(binary, prod, $1000);

              {$IFDEF DEBUG_ZINT}write(Format('$%4X ', [prod]));{$ENDIF}

              if(strlen(binary) > 128) then
              begin
                Result:= ZERROR_TOO_LONG;
                Exit;
              end;
            end;

            {$IFDEF DEBUG_ZINT}writeln;{$ENDIF}
				end;
			'B': begin
				// Byte mode
				// Mode indicator
				concat(binary, 'B');
				byte_used := 1;

				// Character count indicator
				buffer[0] := Chr(short_data_block_length);
				buffer[1] := #0;
				concat(binary, buffer);

				{$IFDEF DEBUG_ZINT}writeln(Format('Byte block (length %d)', [short_data_block_length]));{$ENDIF}

				// Character representation
				for i := 0 to short_data_block_length - 1 do
        begin
					_byte := jisdata[position + i];

					bscan(binary, _byte, $80);

					{$IFDEF DEBUG_ZINT}write(Format('$%4X ', [_byte]));{$ENDIF}

					if(strlen(binary) > 128) then
          begin
						Result := ZERROR_TOO_LONG;
            Exit;
					end;
				end;

				{$IFDEF DEBUG_ZINT}writeln;{$ENDIF}

				end;
			'A': begin
            // Alphanumeric mode
            // Mode indicator
            concat(binary, 'A');
            alphanum_used := 1;

            // Character count indicator
            buffer[0] := Chr(short_data_block_length);
            buffer[1] := #0;
            concat(binary, buffer);

            {$IFDEF DEBUG_ZINT}writeln(Format('Alpha block (length %d)', [short_data_block_length]));{$ENDIF}

            // Character representation
            i := 0;
            while ( i < short_data_block_length ) do
            begin
              //first := 0;
              //second := 0;

              first := posn(RHODIUM, Chr(jisdata[position + i]));
              count := 1;
              prod := first;

              if(mode[position + i + 1] = 'A') then
              begin
                second := posn(RHODIUM, Chr(jisdata[position + i + 1]));
                count := 2;
                prod := (first * 45) + second;
              end;

              bscan(binary, prod, 1 shl (5 * count)); // count := 1..2

              {$IFDEF DEBUG_ZINT}write(Format('$%4X ', [prod]));{$ENDIF}

              if(strlen(binary) > 128) then
              begin
                Result:=ZERROR_TOO_LONG;
                Exit;
              end;

              inc(i, 2);
            end;

            {$IFDEF DEBUG_ZINT}writeln;{$ENDIF}
				end;
			'N': begin
            // Numeric mode
            // Mode indicator
            concat(binary, 'N');

            // Character count indicator
            buffer[0] := Chr(short_data_block_length);
            buffer[1] := #0;
            concat(binary, buffer);

            {$IFDEF DEBUG_ZINT}writeln(Format('Number block (length %d)', [short_data_block_length]));{$ENDIF}

            // Character representation
            i := 0;
            while ( i < short_data_block_length ) do
            begin
               //first := 0;
               //second := 0;
               //third := 0;

              first := posn(NEON, Chr(jisdata[position + i]));
              count := 1;
              prod := first;

              if(mode[position + i + 1] = 'N') then
              begin
                second := posn(NEON, Chr(jisdata[position + i + 1]));
                count := 2;
                prod := (prod * 10) + second;
              end;

              if(mode[position + i + 2] = 'N') then
              begin
                third := posn(NEON, Chr(jisdata[position + i + 2]));
                count := 3;
                prod := (prod * 10) + third;
              end;

              bscan(binary, prod, 1 shl (3 * count)); // count := 1..3

              {$IFDEF DEBUG_ZINT}write(Format('$%4X (%d)', [prod, prod]));{$ENDIF}

              if(strlen(binary) > 128) then
              begin
                Result:=ZERROR_TOO_LONG;
                Exit;
              end;

              inc(i, 3);
            end;

            {$IFDEF DEBUG_ZINT}writeln;{$ENDIF}
				end;
		end;
		inc(position, short_data_block_length);
	until not (position < _length - 1) ;

	Result:=0;
end;

procedure get_bitlength(var count : TArrayOfInteger; stream : TArrayOfChar);
var
  _length, i : NativeInt;
begin
	_length := strlen(stream);

	for i := 0 to 3 do
  begin
		count[i] := 0;
	end;

	i := 0;
	repeat
		if((stream[i] = '0') or (stream[i] = '1')) then
    begin
			inc(count[0]);
			inc(count[1]);
			inc(count[2]);
			inc(count[3]);
			inc(i);
		end
    else
    begin
			case(stream[i]) of
				'K': begin
					inc(count[2], 5);
					inc(count[3], 7);
					inc(i, 2);
					end;
				'B': begin
					inc(count[2], 6);
					inc(count[3], 8);
					inc(i, 2);
					end;
				'A': begin
					inc(count[1], 4);
					inc(count[2], 6);
					inc(count[3], 8);
					inc(i, 2);
					end;
				'N': begin
					inc(count[0], 3);
					inc(count[1], 5);
					inc(count[2], 7);
					inc(count[3], 9);
					inc(i, 2);
					end;
			end;
		end;
	until not (i < _length);
end;

procedure microqr_expand_binary(var binary_stream : TArrayOfChar; var full_stream : TArrayOfChar; version : NativeInt);
var
  i, _length : NativeInt;
begin
	_length := strlen(binary_stream);

	i := 0;
	repeat
		case(binary_stream[i]) of
			 '1': begin concat(full_stream, '1'); inc(i); end;
			 '0': begin concat(full_stream, '0'); inc(i); end;
			 'N': begin
            // Numeric Mode
            // Mode indicator
            case(version) of
               1: concat(full_stream, '0');
               2: concat(full_stream, '00');
               3: concat(full_stream, '000');
            end;

            // Character count indicator
            bscan(full_stream, ord(binary_stream[i + 1]), 4 shl version); // version := 0..3

            inc(i, 2);
				end;
			 'A': begin
            // Alphanumeric Mode
            // Mode indicator
            case(version) of
               1: concat(full_stream, '1');
               2: concat(full_stream, '01');
               3: concat(full_stream, '001');
            end;

            // Character count indicator
            bscan(full_stream, Ord(binary_stream[i + 1]), 2 shl version); // version := 1..3

            inc(i, 2);
				end;
			 'B': begin
          // Byte Mode
          // Mode indicator
          case(version) of
             2: concat(full_stream, '10');
             3: concat(full_stream, '010');
          end;

          // Character count indicator
          bscan(full_stream, Ord(binary_stream[i + 1]), 2 shl version); // version := 2..3

          inc(i, 2);
          end;
         'K': begin
          // Kanji Mode
          // Mode indicator
          case(version) of
             2: concat(full_stream, '11');
             3: concat(full_stream, '011');
          end;

          // Character count indicator
          bscan(full_stream, Ord(binary_stream[i + 1]), 1 shl version); // version := 2..3

          inc(i, 2);
				end;
		end;
	until not (i < _length);
end;

procedure micro_qr_m1(var binary_data : TArrayOfChar);
var
	latch : NativeInt;
	bits_total, bits_left, remainder : NativeInt;
	data_codewords, ecc_codewords : NativeInt;
	data_blocks, ecc_blocks : TArrayOfByte;
  RSGlobals : TRSGlobals;
begin
	SetLength(data_blocks, 4);
  SetLength(ecc_blocks, 3);

	bits_total := 20;
	latch := 0;

	// Add terminator
	bits_left := bits_total - strlen(binary_data);
	if(bits_left <= 3) then
  begin
		for var i := 0 to bits_left - 1 do
    	concat(binary_data, '0');
		latch := 1;
	end
  else
		concat(binary_data, '000');

	if(latch = 0) then
  begin
		// Manage last (4-bit) block
		bits_left := bits_total - strlen(binary_data);
		if(bits_left <= 4) then
    begin
			for var i := 0 to bits_left - 1 do
      	concat(binary_data, '0');
			latch := 1;
		end;
	end;

	if(latch = 0) then
  begin
		// Complete current byte
		remainder := 8 - (strlen(binary_data) mod 8);
		if (remainder = 8) then remainder := 0;
		for var i := 0 to remainder - 1 do
			concat(binary_data, '0');

		// Add padding
		bits_left := bits_total - strlen(binary_data);
		if (bits_left > 4) then
    begin
			remainder := (bits_left - 4) div 8;
			for var i := 0 to remainder - 1 do
        if (i and 1)<>0 then
				  concat(binary_data, '00010001')
        else
          concat(binary_data, '11101100');
		end;
		concat(binary_data, '0000');
	end;

	data_codewords := 3;
	ecc_codewords := 2;

	// Copy data into codewords
	for var i := 0 to (data_codewords - 1) - 1 do
  begin
		data_blocks[i] := 0;
		if(binary_data[i * 8] = '1') then inc( data_blocks[i], $80);
		if(binary_data[(i * 8) + 1] = '1') then inc( data_blocks[i], $40);
		if(binary_data[(i * 8) + 2] = '1') then inc( data_blocks[i], $20);
		if(binary_data[(i * 8) + 3] = '1') then inc( data_blocks[i], $10);
		if(binary_data[(i * 8) + 4] = '1') then inc( data_blocks[i], $08);
		if(binary_data[(i * 8) + 5] = '1') then inc( data_blocks[i], $04);
		if(binary_data[(i * 8) + 6] = '1') then inc( data_blocks[i], $02);
		if(binary_data[(i * 8) + 7] = '1') then inc( data_blocks[i], $01);
	end;
	data_blocks[2] := 0;
	if(binary_data[16] = '1') then inc( data_blocks[2], $08);
	if(binary_data[17] = '1') then inc( data_blocks[2], $04);
	if(binary_data[18] = '1') then inc( data_blocks[2], $02);
	if(binary_data[19] = '1') then inc( data_blocks[2], $01);

	// Calculate Reed-Solomon error codewords
	rs_init_gf($11d, RSGlobals);
	rs_init_code(ecc_codewords, 0, RSGlobals);
	rs_encode(data_codewords,data_blocks,ecc_blocks, RSGlobals);
	rs_free(RSGlobals);

	// Add Reed-Solomon codewords to binary data
	for var i := 0 to ecc_codewords - 1 do
		bscan(binary_data, ecc_blocks[ecc_codewords - i - 1], $80);
end;

procedure micro_qr_m2(var binary_data: TArrayOfChar; ecc_mode : NativeInt);
var
	latch : NativeInt;
	bits_total, bits_left, remainder : NativeInt;
	data_codewords, ecc_codewords : NativeInt;
	data_blocks, ecc_blocks : TArrayOfByte;
  RSGlobals : TRSGlobals;
begin
	SetLength(data_blocks, 6);
  SetLength(ecc_blocks, 7);

	latch := 0;

	case ecc_mode of
    LEVEL_L : bits_total := 40;
	  LEVEL_M : bits_total := 32;
    else exit; //should not happen
  end;

	// Add terminator
	bits_left := bits_total - strlen(binary_data);
	if(bits_left <= 5) then
  begin
		for var i := 0 to bits_left - 1 do
			concat(binary_data, '0');
		latch := 1;
	end
  else
		concat(binary_data, '00000');

	if(latch = 0) then
  begin
		// Complete current byte
		remainder := 8 - (strlen(binary_data) mod 8);
		if(remainder = 8) then remainder := 0;
		for var i := 0 to remainder - 1 do
			concat(binary_data, '0');

		// Add padding
		bits_left := bits_total - strlen(binary_data);
		remainder := bits_left div 8;
		for var i := 0 to remainder - 1 do
      if (i and 1)<>0 then
      	concat(binary_data, '00010001' )
      else
        concat(binary_data, '11101100');
	end;

	case ecc_mode of
    LEVEL_L : begin data_codewords := 5; ecc_codewords := 5; end;
	  LEVEL_M : begin data_codewords := 4; ecc_codewords := 6; end;
    else exit; //should not happen
  end;

	// Copy data into codewords
	for var i := 0 to data_codewords - 1 do
  begin
		data_blocks[i] := 0;
		if(binary_data[i * 8] = '1') then inc(data_blocks[i], $80);
		if(binary_data[(i * 8) + 1] = '1') then inc(data_blocks[i], $40);
		if(binary_data[(i * 8) + 2] = '1') then inc(data_blocks[i], $20);
		if(binary_data[(i * 8) + 3] = '1') then inc(data_blocks[i], $10);
		if(binary_data[(i * 8) + 4] = '1') then inc(data_blocks[i], $08);
		if(binary_data[(i * 8) + 5] = '1') then inc(data_blocks[i], $04);
		if(binary_data[(i * 8) + 6] = '1') then inc(data_blocks[i], $02);
		if(binary_data[(i * 8) + 7] = '1') then inc(data_blocks[i], $01);
	end;

	// Calculate Reed-Solomon error codewords
	rs_init_gf($11d, RSGlobals);
	rs_init_code(ecc_codewords, 0, RSGlobals);
	rs_encode(data_codewords,data_blocks,ecc_blocks, RSGlobals);
	rs_free(RSGlobals);

	// Add Reed-Solomon codewords to binary data
  for var i := 0 to ecc_codewords - 1 do
   	bscan(binary_data, ecc_blocks[ecc_codewords - i - 1], $80);
end;

procedure micro_qr_m3(var binary_data : TArrayOfChar; ecc_mode : NativeInt);
var
	latch : NativeInt;
	bits_total, bits_left, remainder : NativeInt;
	data_codewords, ecc_codewords : NativeInt;
	data_blocks, ecc_blocks : TArrayOfByte;
  RSGlobals : TRSGlobals;
begin
	SetLength(data_blocks, 12);
  SetLength(ecc_blocks, 9);

	latch := 0;

	case ecc_mode of
    LEVEL_L : bits_total := 84;
	  LEVEL_M : bits_total := 68;
    else exit; //should not happen
  end;

	// Add terminator
	bits_left := bits_total - strlen(binary_data);
	if(bits_left <= 7) then
  begin
		for var i := 0 to bits_left-1 do
      concat(binary_data, '0');
		latch := 1;
	end
  else
		concat(binary_data, '0000000');

	if(latch = 0) then
  begin
		// Manage last (4-bit) block
		bits_left := bits_total - strlen(binary_data);
		if(bits_left <= 4) then
    begin
			for var i := 0 to bits_left - 1 do
				concat(binary_data, '0');
			latch := 1;
		end;
	end;

	if(latch = 0) then
  begin
		// Complete current byte
		remainder := 8 - (strlen(binary_data) mod 8);
		if (remainder = 8) then remainder := 0;
		for var i := 0 to remainder - 1 do
			concat(binary_data, '0');

		// Add padding
		bits_left := bits_total - strlen(binary_data);
		if(bits_left > 4) then
    begin
			remainder := (bits_left - 4) div 8;
			for var i := 0 to remainder - 1 do
        if (i and 1) <> 0  then
          concat(binary_data, '00010001')
        else
          concat(binary_data, '11101100');
		end;
		concat(binary_data, '0000');
	end;

	case ecc_mode of
    LEVEL_L : begin data_codewords := 11; ecc_codewords := 6; end;
	  LEVEL_M : begin data_codewords := 9; ecc_codewords := 8; end;
    else exit; //should not happen
  end;

	// Copy data into codewords
	for var i := 0 to (data_codewords - 1) - 1 do
  begin
		data_blocks[i] := 0;
		if(binary_data[i * 8] = '1') then inc(data_blocks[i], $80);
		if(binary_data[(i * 8) + 1] = '1') then inc(data_blocks[i], $40);
		if(binary_data[(i * 8) + 2] = '1') then inc(data_blocks[i], $20);
		if(binary_data[(i * 8) + 3] = '1') then inc(data_blocks[i], $10);
		if(binary_data[(i * 8) + 4] = '1') then inc(data_blocks[i], $08);
		if(binary_data[(i * 8) + 5] = '1') then inc(data_blocks[i], $04);
		if(binary_data[(i * 8) + 6] = '1') then inc(data_blocks[i], $02);
		if(binary_data[(i * 8) + 7] = '1') then inc(data_blocks[i], $01);
	end;

	if(ecc_mode = LEVEL_L) then
  begin
		data_blocks[11] := 0;
		if(binary_data[80] = '1') then inc(data_blocks[2], $08);
		if(binary_data[81] = '1') then inc(data_blocks[2], $04);
		if(binary_data[82] = '1') then inc(data_blocks[2], $02);
		if(binary_data[83] = '1') then inc(data_blocks[2], $01);
	end;

	if(ecc_mode = LEVEL_M) then
  begin
		data_blocks[9] := 0;
		if(binary_data[64] = '1') then inc(data_blocks[2], $08);
		if(binary_data[65] = '1') then inc(data_blocks[2], $04);
		if(binary_data[66] = '1') then inc(data_blocks[2], $02);
		if(binary_data[67] = '1') then inc(data_blocks[2], $01);
	end;

	// Calculate Reed-Solomon error codewords
	rs_init_gf($11d, RSGlobals);
	rs_init_code(ecc_codewords, 0, RSGlobals);
	rs_encode(data_codewords,data_blocks,ecc_blocks, RSGlobals);
	rs_free(RSGlobals);

	// Add Reed-Solomon codewords to binary data
	for var i := 0 to ecc_codewords - 1 do
  begin
		bscan(binary_data, ecc_blocks[ecc_codewords - i - 1], $80);
	end;
end;

procedure micro_qr_m4(var binary_data : TArrayOfChar; ecc_mode : NativeInt);
var
	latch : NativeInt;
	bits_total, bits_left, remainder : NativeInt;
	data_codewords, ecc_codewords : NativeInt;
	data_blocks, ecc_blocks : TArrayOfByte;
  RSGlobals : TRSGlobals;
begin
  SetLength(data_blocks,17);
  SetLength(ecc_blocks,15);

	latch := 0;

	case ecc_mode of
    LEVEL_L : bits_total := 128;
	  LEVEL_M : bits_total := 112;
	  LEVEL_Q : bits_total := 80;
    else exit; //shoud not happend
  end;

	// Add terminator
	bits_left := bits_total - strlen(binary_data);
	if (bits_left <= 9) then
  begin
		for var i := 0 to bits_left - 1 do
    	concat(binary_data, '0');
		latch := 1;
	end
  else
  	concat(binary_data, '000000000');

	if(latch = 0) then
  begin
		// Complete current byte
		remainder := 8 - (strlen(binary_data) mod 8);
		if(remainder = 8) then remainder := 0;
		for var i := 0 to remainder - 1 do
    	concat(binary_data, '0');

		// Add padding
		bits_left := bits_total - strlen(binary_data);
		remainder := bits_left div 8;
		for var i := 0 to remainder - 1 do
    begin
      if (i and 1)<>0 then
        concat(binary_data, '00010001')
      else
        concat(binary_data, '11101100');
		end;
  end;

	case ecc_mode of
    LEVEL_L : begin data_codewords := 16; ecc_codewords := 8; end;
	  LEVEL_M : begin data_codewords := 14; ecc_codewords := 10; end;
	  LEVEL_Q : begin data_codewords := 10; ecc_codewords := 14; end;
    else exit; //shoud not happend
  end;

	// Copy data into codewords
	for var i := 0 to data_codewords - 1 do
  begin
		data_blocks[i] := 0;
		if(binary_data[i * 8] = '1') then inc(data_blocks[i], $80);
		if(binary_data[(i * 8) + 1] = '1') then inc( data_blocks[i], $40);
		if(binary_data[(i * 8) + 2] = '1') then inc( data_blocks[i], $20);
		if(binary_data[(i * 8) + 3] = '1') then inc( data_blocks[i], $10);
		if(binary_data[(i * 8) + 4] = '1') then inc( data_blocks[i], $08);
		if(binary_data[(i * 8) + 5] = '1') then inc( data_blocks[i], $04);
		if(binary_data[(i * 8) + 6] = '1') then inc( data_blocks[i], $02);
		if(binary_data[(i * 8) + 7] = '1') then inc( data_blocks[i], $01);
	end;

	// Calculate Reed-Solomon error codewords
	rs_init_gf($11d, RSGlobals);
	rs_init_code(ecc_codewords, 0, RSGlobals);
	rs_encode(data_codewords,data_blocks,ecc_blocks, RSGlobals);
	rs_free(RSGlobals);

	// Add Reed-Solomon codewords to binary data
	for var i := 0 to ecc_codewords - 1 do
		bscan(binary_data, ecc_blocks[ecc_codewords - i - 1], $80);
end;

procedure micro_setup_grid(var grid : TArrayOfByte; size : NativeInt);
var
  toggle : NativeInt;
begin
	toggle := 1;

	// Add timing patterns
	for var i := 0 to size - 1 do
  begin
		if(toggle = 1) then
    begin
			grid[i] := $21;
			grid[(i * size)] := $21;
			toggle := 0;
		end
    else
    begin
			grid[i] := $20;
			grid[(i * size)] := $20;
			toggle := 1;
		end;
	end;

	// Add finder patterns
	place_finder(grid, size, 0, 0);

	// Add separators
	for var i := 0 to 6 do
  begin
		grid[(7 * size) + i] := $10;
		grid[(i * size) + 7] := $10;
	end;
	grid[(7 * size) + 7] := $10;


	// Reserve space for format information
	for var i := 0 to 7 do
  begin
		inc(grid[(8 * size) + i], $20);
		inc(grid[(i * size) + 8], $20);
	end;
	inc(grid[(8 * size) + 8], 20);
end;

procedure micro_populate_grid(var grid : TArrayOfByte; size : NativeInt; full_stream: TArrayOfChar);
var
  direction : NativeInt;
  row : NativeInt;
  i, n, x, y : NativeInt;
begin
	direction := 1; // up
	row := 0; // right hand side

	n := strlen(full_stream);
	y := size - 1;
	i := 0;
	repeat
    x := (size - 2) - (row * 2);

		if not((grid[(y * size) + (x + 1)] and $f0)<>0) then
    begin
			if (full_stream[i] = '1') then
      	grid[(y * size) + (x + 1)] := $01
			else
      	grid[(y * size) + (x + 1)] := $00;
			inc(i);
		end;

		if(i < n) then
    begin
			if not((grid[(y * size) + x] and $f0)<>0) then
      begin
				if (full_stream[i] = '1') then
					grid[(y * size) + x] := $01
				else
					grid[(y * size) + x] := $00;
				inc(i);
			end;
		end;

		if(direction<>0) then dec(y) else inc(y);
		if(y = 0) then
    begin
			// reached the top
			inc(row);
			y := 1;
			direction := 0;
		end;
		if(y = size) then
    begin
			// reached the bottom
			inc(row);
			y := size - 1;
			direction := 1;
		end;
	until not (i < n);
end;

function micro_evaluate(var grid : TArrayOfByte; size : NativeInt; pattern : NativeInt): NativeInt;
var
  sum1, sum2, filter, retval : NativeInt;
begin
	filter := 0;

	case(pattern) of
		 0: filter := $01;
		 1: filter := $02;
		 2: filter := $04;
		 3: filter := $08;
	end;

	sum1 := 0;
	sum2 := 0;
	for var i := 1 to size - 1 do
  begin
		if(grid[(i * size) + size - 1] and filter)<>0 then inc(sum1);
		if(grid[((size - 1) * size) + i] and filter)<>0 then inc(sum2);
	end;

	if(sum1 <= sum2) then
    retval := (sum1 * 16) + sum2
  else
    retval := (sum2 * 16) + sum1;

	Result:=retval;
end;

function micro_apply_bitmask(var grid : TArrayOfByte; size: NativeInt): NativeInt;
var
	p : Byte;
  value : TArrayOfInteger;
	best_val, best_pattern : NativeInt;
	bit : NativeInt;
	mask : TArrayOfByte;
	eval : TArrayOfByte;
begin
  SetLength(value,8);
	SetLength(mask, size * size);
	SetLength(eval, size * size);

	// Perform data masking
	for var x := 0 to size -1 do
  begin
		for var y := 0 to size - 1 do
    begin
			mask[(y * size) + x] := $00;
			if not((grid[(y * size) + x] and $f0)<>0) then
      begin
				if((y and 1) = 0) then
					inc(mask[(y * size) + x], $01);

				if((((y div 2) + (x div 3)) and 1) = 0) then
				  inc(mask[(y * size) + x], $02);

				if(((((y * x) and 1) + ((y * x) mod 3)) and 1) = 0) then
					inc(mask[(y * size) + x], $04);

				if(((((y + x) and 1) + ((y * x) mod 3)) and 1) = 0) then
					inc(mask[(y * size) + x], $08);
			end;
		end;
	end;

	for var x := 0 to size - 1 do
  begin
		for var y := 0 to size - 1 do
    begin
			if(grid[(y * size) + x] and $01)<>0 then p := $ff else p := $00;

			eval[(y * size) + x] := mask[(y * size) + x] xor p;
		end;
	end;

	// Evaluate result
	for var pattern := 0 to 7 do
		value[pattern] := micro_evaluate(eval, size, pattern);

	best_pattern := 0;
	best_val := value[0];
	for var pattern := 1 to 3 do
  begin
		if(value[pattern] > best_val) then
    begin
			best_pattern := pattern;
			best_val := value[pattern];
		end;
	end;

	// Apply mask
	for var x := 0 to size - 1 do
  begin
		for var y := 0 to size - 1 do
    begin
			bit := 0;
			case(best_pattern) of
				 0: if(mask[(y * size) + x] and $01)<>0 then bit := 1;
				 1: if(mask[(y * size) + x] and $02)<>0 then bit := 1;
				 2: if(mask[(y * size) + x] and $04)<>0 then bit := 1;
				 3: if(mask[(y * size) + x] and $08)<>0 then bit := 1;
			end;
			if(bit = 1) then
      begin
				if(grid[(y * size) + x] and $01)<>0 then
        	grid[(y * size) + x] := $00
				else
					grid[(y * size) + x] := $01;
			end;
		end;
	end;

	Result:=best_pattern;
end;

function microqr(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;
var
	j, glyph, size : NativeInt;
	binary_stream : TArrayOfChar;
	full_stream : TArrayOfChar;
	utfdata : TArrayOfInteger;
	jisdata : TArrayOfInteger;
	mode : TArrayOfChar;
	error_number, kanji_used, alphanum_used, byte_used : NativeInt;
	version_valid : TArrayOfInteger;
	binary_count : TArrayOfInteger;
	ecc_level, autoversion, version : NativeInt;
	n_count, a_count, bitmask, format, format_full : NativeInt;
  grid : TArrayOfByte;
begin
	SetLength(binary_stream, 200);
	SetLength(full_stream, 200);
	SetLength(utfdata, 40);
	SetLength(jisdata, 40);
	SetLength(mode, 40);
	kanji_used := 0;
  alphanum_used := 0;
  byte_used := 0;
	SetLength(version_valid,4);
	SetLength(binary_count,4);

	if(_length > 35) then
  begin
		strcpy(symbol.errtxt, 'Input data too long');
		Result:=ZERROR_TOO_LONG;
    exit;
	end;

	for var i := 0 to 3 do
  	version_valid[i] := 1;

	case(symbol.input_mode) of
		 DATA_MODE:
			for var i := 0 to _length - 1 do
      	jisdata[i] := source[i];
     else
      begin
			// Convert Unicode input to Shift-JIS
			error_number := utf8toutf16(symbol, source, utfdata, _length);
			if(error_number <> 0) then begin Result:=error_number; Exit end;

			for var i := 0 to _length - 1 do
      begin
				if(utfdata[i] <= $ff) then
					jisdata[i] := utfdata[i]
				else
        begin
					j := 0;
					glyph := 0;
					repeat
						if(sjis_lookup[j * 2] = utfdata[i]) then
							glyph := sjis_lookup[(j * 2) + 1];

						inc(j);
					until not ((j < 6843) and (glyph = 0));
					if(glyph = 0) then
          begin
						strcpy(symbol.errtxt, 'Invalid character in input data');
						Result:=ZERROR_INVALID_DATA;
            Exit;
					end;
					jisdata[i] := glyph;
				end;
			end;
      end;
	end;

	microqr_define_mode(mode, jisdata, _length, 0);

	n_count := 0;
	a_count := 0;
	for var i := 0 to _length - 1 do
  begin
		if((jisdata[i] >= Ord('0')) and (jisdata[i] <= Ord('9'))) then inc(n_count);
		if(in_alpha(jisdata[i])<>0) then Inc(a_count);
	end;

	if (a_count = _length) then  	// All data can be encoded in Alphanumeric mode
		for var i := 0 to _length - 1 do
			mode[i] := 'A';

	if (n_count = _length) then   // All data can be encoded in Numeric mode
		for var i := 0 to _length - 1 do
			mode[i] := 'N';

	error_number := micro_qr_intermediate(binary_stream, jisdata, mode, _length, kanji_used, alphanum_used, byte_used);
	if (error_number <> 0) then
  begin
		strcpy(symbol.errtxt, 'Input data too long');
		Result:=error_number;
    Exit;
	end;

	get_bitlength(binary_count, binary_stream);

	// Eliminate possivle versions depending on type of content
	if (byte_used<>0) then
  begin
		version_valid[0] := 0;
		version_valid[1] := 0;
	end;

	if (alphanum_used<>0) then
  begin
		version_valid[0] := 0;
	end;

	if (kanji_used<>0) then
  begin
		version_valid[0] := 0;
		version_valid[1] := 0;
	end;

	// Eliminate possible versions depending on _length of binary data
	if(binary_count[0] > 20) then version_valid[0] := 0;
	if(binary_count[1] > 40) then version_valid[1] := 0;
	if(binary_count[2] > 84) then version_valid[2] := 0;
	if(binary_count[3] > 128) then
  begin
		strcpy(symbol.errtxt, 'Input data too long');
		Result:=ZERROR_TOO_LONG;
    Exit;
	end;

	// Eliminate possible versions depending on error correction level specified
	ecc_level := LEVEL_L;
	if((symbol.option_1 >= 1) and (symbol.option_1 <= 4)) then
  	ecc_level := symbol.option_1;

	if(ecc_level = LEVEL_H) then
  begin
		strcpy(symbol.errtxt, 'Error correction level H not available');
		Result:= ZERROR_INVALID_OPTION;
    Exit;
	end;

	if(ecc_level = LEVEL_Q) then
  begin
		version_valid[0] := 0;
		version_valid[1] := 0;
		version_valid[2] := 0;
		if(binary_count[3] > 80) then
    begin
			strcpy(symbol.errtxt, 'Input data too long');
			Result:= ZERROR_TOO_LONG;
      Exit;
		end;
	end;

	if(ecc_level = LEVEL_M) then
  begin
		version_valid[0] := 0;
		if(binary_count[1] > 32) then version_valid[1] := 0;
		if(binary_count[2] > 68) then version_valid[2] := 0;
		if(binary_count[3] > 112) then
    begin
			strcpy(symbol.errtxt, 'Input data too long');
			Result:=ZERROR_TOO_LONG;
      Exit;
		end;
	end;

	autoversion := 3;
	if(version_valid[2]<>0) then autoversion := 2;
	if(version_valid[1]<>0) then autoversion := 1;
	if(version_valid[0]<>0) then autoversion := 0;

	version := autoversion;
	// Get version from user
	if((symbol.option_2 >= 1) and (symbol.option_2 <= 4)) then
  begin
		if(symbol.option_2 >= autoversion) then
			version := symbol.option_2 - 1; //decrement, because we work internal with 0..3
	end;

	// If there is enough unused space then increase the error correction level
	if(version = 3) then
  begin
		if(binary_count[3] <= 112) then ecc_level := LEVEL_M;
		if(binary_count[3] <= 80) then ecc_level := LEVEL_Q;
	end;

	if(version = 2) then
		if(binary_count[2] <= 68) then ecc_level := LEVEL_M;

	if(version = 1) then
		if(binary_count[1] <= 32) then ecc_level := LEVEL_M;

	strcpy(full_stream, '');
	microqr_expand_binary(binary_stream, full_stream, version);

	case(version) of
		 0: micro_qr_m1(full_stream);
		 1: micro_qr_m2(full_stream, ecc_level);
		 2: micro_qr_m3(full_stream, ecc_level);
		 3: micro_qr_m4(full_stream, ecc_level);
	end;

	size := micro_qr_sizes[version];
	SetLength(grid, size * size);

	for var i := 0 to size - 1 do
		for j := 0 to size - 1 do
			grid[(i * size) + j] := 0;

	micro_setup_grid(grid, size);
	micro_populate_grid(grid, size, full_stream);
	bitmask := micro_apply_bitmask(grid, size);

	// Add format data
	format := 0;
	case(version) of
		 1: case(ecc_level) of
				 1: format := 1;
				 2: format := 2;
			end;
		 2: case(ecc_level) of
				 1: format := 3;
				 2: format := 4;
			end;
		 3: case(ecc_level) of
				 1: format := 5;
				 2: format := 6;
				 3: format := 7;
			end;
	end;

	format_full := qr_annex_c1[(format shl 2) + bitmask];

	if(format_full and $4000)<>0 then inc(grid[(8 * size) + 1], $01);
	if(format_full and $2000)<>0 then inc(grid[(8 * size) + 2], $01);
	if(format_full and $1000)<>0 then inc(grid[(8 * size) + 3], $01);
	if(format_full and $800)<>0 then inc(grid[(8 * size) + 4], $01);
	if(format_full and $400)<>0 then inc(grid[(8 * size) + 5], $01);
	if(format_full and $200)<>0 then inc(grid[(8 * size) + 6], $01);
	if(format_full and $100)<>0 then inc(grid[(8 * size) + 7], $01);
	if(format_full and $80)<>0 then inc(grid[(8 * size) + 8], $01);
	if(format_full and $40)<>0 then inc(grid[(7 * size) + 8], $01);
	if(format_full and $20)<>0 then inc(grid[(6 * size) + 8], $01);
	if(format_full and $10)<>0 then inc(grid[(5 * size) + 8], $01);
	if(format_full and $08)<>0 then inc(grid[(4 * size) + 8], $01);
	if(format_full and $04)<>0 then inc(grid[(3 * size) + 8], $01);
	if(format_full and $02)<>0 then inc(grid[(2 * size) + 8], $01);
	if(format_full and $01)<>0 then inc(grid[(1 * size) + 8], $01);

	symbol.width := size;
	symbol.rows := size;

	for var i := 0 to size - 1 do
  begin
		for j := 0 to size - 1 do
    begin
			if (grid[(i * size) + j] and $01)<>0 then
				set_module(symbol, i, j);
		end;
		symbol.row_height[i] := 1;
	end;

	Result:=0;
end;

// NOTE: From this point forward concerns Rectangular Micro QR Code (rMQR) only

procedure rmqr_setup_grid(var grid : TArrayOfByte; h_size : NativeInt; v_size : NativeInt);
const
  alignment : array [0..4] of Byte = ($1F, $11, $15, $11, $1F);
var
  h_version, finder_position : NativeInt;
begin
  // Add timing patterns - top and bottom
  for var i := 0 to h_size - 1 do
  begin
    grid[i] := $20 + Ord((i and 1) = 0);
    grid[((v_size - 1) * h_size) + i] := $20 + Ord((i and 1) = 0);
  end;

  // Add timing patterns - left and right
  for var i := 0 to v_size - 1 do
  begin
    grid[i * h_size] := $20 + Ord((i and 1) = 0);
    grid[(i * h_size) + (h_size - 1)] := $20 + Ord((i and 1) = 0);
  end;

  // Add finder pattern
  place_finder(grid, h_size, 0, 0); // This works because the finder is always top left

  // Add finder sub-pattern to bottom right
  for var i := 0 to 4 do
  begin
    for var j := 0 to 4 do
    begin
      if (alignment[j] and ($10 shr i)) <> 0 then
        grid[((i + v_size - 5) * h_size) + (h_size - 5) + j] := $11
      else
        grid[((i + v_size - 5) * h_size) + (h_size - 5) + j] := $10;
    end;
  end;

  // Add corner finder pattern - bottom left
  grid[(v_size - 2) * h_size] := $11;
  grid[((v_size - 2) * h_size) + 1] := $10;
  grid[((v_size - 1) * h_size) + 1] := $11;

  // Add corner finder pattern - top right
  grid[h_size - 2] := $11;
  grid[(h_size * 2) - 2] := $10;
  grid[(h_size * 2) - 1] := $11;

  // Add separator
  for var i := 0 to 6 do
    grid[(i * h_size) + 7] := $20;

  if (v_size > 7) then
  begin
    // Note for v_size = 9 this overrides the bottom right corner finder pattern
    for var i := 0 to 7 do
      grid[(7 * h_size) + i] := $20;
  end;

  // Add alignment patterns
  if (h_size > 27) then
  begin
    h_version := 0;
    for var i := 0 to 4 do
    begin
      if (h_size = rmqr_width[i]) then
      begin
        h_version := i;
        break;
      end;
    end;

    for var i := 0 to 3 do
    begin
      finder_position := rmqr_table_d1[(h_version * 4) + i];

      if (finder_position <> 0) then
      begin
        for var j := 0 to v_size - 1 do
          grid[(j * h_size) + finder_position] := $10 + Ord((j and 1) = 0);

        // Top square
        grid[h_size + finder_position - 1] := $11;
        grid[(h_size * 2) + finder_position - 1] := $11;
        grid[h_size + finder_position + 1] := $11;
        grid[(h_size * 2) + finder_position + 1] := $11;

        // Bottom square
        grid[(h_size * (v_size - 3)) + finder_position - 1] := $11;
        grid[(h_size * (v_size - 2)) + finder_position - 1] := $11;
        grid[(h_size * (v_size - 3)) + finder_position + 1] := $11;
        grid[(h_size * (v_size - 2)) + finder_position + 1] := $11;
      end;
    end;
  end;

  // Reserve space for format information
  for var i := 0 to 4 do
  begin
    for var j := 0 to 2 do
    begin
      grid[(h_size * (i + 1)) + j + 8] := $20;
      grid[(h_size * (v_size - 6)) + (h_size * i) + j + (h_size - 8)] := $20;
    end;
  end;
  grid[(h_size * 1) + 11] := $20;
  grid[(h_size * 2) + 11] := $20;
  grid[(h_size * 3) + 11] := $20;
  grid[(h_size * (v_size - 6)) + (h_size - 5)] := $20;
  grid[(h_size * (v_size - 6)) + (h_size - 4)] := $20;
  grid[(h_size * (v_size - 6)) + (h_size - 3)] := $20;
end;

function rmqr(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;
var
  error_number, est_binlen : NativeInt;
  ecc_level, autosize, version, max_cw, target_codewords, blocks : NativeInt;
  h_size, v_size, footprint, best_footprint, format_data : NativeInt;
  left_format_info, right_format_info : FixedUInt;
  gs1 : NativeInt;
  jisdata : TArrayOfInteger;
  mode : TArrayOfChar;
  datastream, fullstream : TArrayOfInteger;
  grid : TArrayOfByte;
begin
	SetLength(jisdata, _length + 1);
	SetLength(mode, _length + 1);

  if symbol.input_mode = GS1_MODE then
  	gs1 := 1
  else
    gs1 := 0;

	if (symbol.option_1 = LEVEL_L) or (symbol.option_1 = LEVEL_Q) then
	begin
		strcpy(symbol.errtxt, 'Error correction levels L and Q are not available in rMQR');
		Result := ZERROR_INVALID_OPTION;
		Exit;
	end;

	if (symbol.option_2 < 0) or (symbol.option_2 > 38) then
	begin
		strcpy(symbol.errtxt, 'Invalid rMQR symbol size');
		Result := ZERROR_INVALID_OPTION;
		Exit;
	end;

  error_number := qr_prep_data(symbol, source, _length, jisdata);
  if (error_number <> 0) then
  begin
    Result := error_number;
    Exit;
  end;

	// Only M and H are defined for rMQR
	if (symbol.option_1 = LEVEL_H) then
		ecc_level := LEVEL_H
	else
		ecc_level := LEVEL_M;

	est_binlen := qr_calc_binlen(RMQR_VERSION + 31, mode, jisdata, _length, gs1);

	if (ecc_level = LEVEL_H) then
		max_cw := rmqr_data_codewords_H[31]
	else
		max_cw := rmqr_data_codewords_M[31];

	if (est_binlen > (8 * max_cw)) then
	begin
		strcpy(symbol.errtxt, 'Input too long for selected error correction level');
		Result := ZERROR_TOO_LONG;
		Exit;
	end;

	version := 31;

	if (symbol.option_2 = 0) then
	begin
		// Automatic symbol size - pick the smallest footprint that fits
		autosize := 31;
		best_footprint := rmqr_height[31] * rmqr_width[31];

		for var i := 30 downto 0 do
		begin
			est_binlen := qr_calc_binlen(RMQR_VERSION + i, mode, jisdata, _length, gs1);
			footprint := rmqr_height[i] * rmqr_width[i];

			if (ecc_level = LEVEL_H) then
				max_cw := rmqr_data_codewords_H[i]
			else
				max_cw := rmqr_data_codewords_M[i];

			if ((8 * max_cw) >= est_binlen) and (footprint < best_footprint) then
			begin
				autosize := i;
				best_footprint := footprint;
			end;
		end;

		version := autosize;
	end;

	if (symbol.option_2 >= 1) and (symbol.option_2 <= 32) then
		// User specified symbol size
		version := symbol.option_2 - 1;

	if (symbol.option_2 >= 33) then
	begin
		// User has specified symbol height only
		version := rmqr_fixed_height_upper_bound[symbol.option_2 - 32];

		for var i := version - 1 downto rmqr_fixed_height_upper_bound[symbol.option_2 - 33] + 1 do
		begin
			est_binlen := qr_calc_binlen(RMQR_VERSION + i, mode, jisdata, _length, gs1);

			if (ecc_level = LEVEL_H) then
				max_cw := rmqr_data_codewords_H[i]
			else
				max_cw := rmqr_data_codewords_M[i];

			if ((8 * max_cw) >= est_binlen) then
				version := i;
		end;
	end;

	est_binlen := qr_calc_binlen(RMQR_VERSION + version, mode, jisdata, _length, gs1);

	if (symbol.option_1 < LEVEL_L) or (symbol.option_1 > LEVEL_H) then
	begin
		// Detect if there is enough free space to increase the error correction level
		if (est_binlen < (rmqr_data_codewords_H[version] * 8)) then
			ecc_level := LEVEL_H;
	end;

	if (ecc_level = LEVEL_H) then
	begin
		target_codewords := rmqr_data_codewords_H[version];
		blocks := rmqr_blocks_H[version];
	end
	else
	begin
		target_codewords := rmqr_data_codewords_M[version];
		blocks := rmqr_blocks_M[version];
	end;

	if (est_binlen > (target_codewords * 8)) then
	begin
		// User has selected a symbol too small for the data
		strcpy(symbol.errtxt, 'Input too long for selected rMQR symbol size');
		Result := ZERROR_TOO_LONG;
		Exit;
	end;

  {$IFDEF DEBUG_ZINT}
	writeln(Format('Minimum codewords: %d', [(est_binlen + 7) div 8]));
	writeln(Format('Selected version: %d = R%dx%d', [version + 1, rmqr_height[version], rmqr_width[version]]));
	writeln(Format('Number of data codewords in symbol: %d', [target_codewords]));
	writeln(Format('Number of ECC blocks: %d', [blocks]));
  {$ENDIF}

	SetLength(datastream, target_codewords + 1);
	SetLength(fullstream, rmqr_total_codewords[version] + 1);

	qr_binary(datastream, RMQR_VERSION + version, target_codewords, mode, jisdata, _length, gs1, est_binlen);
	add_ecc(fullstream, datastream, rmqr_total_codewords[version], target_codewords, blocks);

	h_size := rmqr_width[version];
	v_size := rmqr_height[version];

	SetLength(grid, h_size * v_size);
	for var i := 0 to (h_size * v_size) - 1 do
		grid[i] := 0;

	rmqr_setup_grid(grid, h_size, v_size);
	populate_grid(grid, h_size, v_size, fullstream, rmqr_total_codewords[version]);

	// Apply the data mask - rMQR defines only the one of section 7.8.2
	for var i := 0 to v_size - 1 do
	begin
		for var j := 0 to h_size - 1 do
		begin
			if (grid[(i * h_size) + j] and $f0) = 0 then
			begin
				// This is a data module
				if ((((i div 2) + (j div 3)) mod 2) = 0) then
					grid[(i * h_size) + j] := grid[(i * h_size) + j] xor $01;
			end;
		end;
	end;

	// Add format information
	format_data := version;
	if (ecc_level = LEVEL_H) then
		inc(format_data, 32);

	left_format_info := rmqr_format_info_left[format_data];
	right_format_info := rmqr_format_info_right[format_data];

	for var i := 0 to 4 do
	begin
		for var j := 0 to 2 do
		begin
			grid[(h_size * (i + 1)) + j + 8] := Byte((left_format_info shr ((j * 5) + i)) and $01);
			grid[(h_size * (i + v_size - 6)) + j + (h_size - 8)] := Byte((right_format_info shr ((j * 5) + i)) and $01);
		end;
	end;
	grid[(h_size * 1) + 11] := Byte((left_format_info shr 15) and $01);
	grid[(h_size * 2) + 11] := Byte((left_format_info shr 16) and $01);
	grid[(h_size * 3) + 11] := Byte((left_format_info shr 17) and $01);
	grid[(h_size * (v_size - 6)) + (h_size - 5)] := Byte((right_format_info shr 15) and $01);
	grid[(h_size * (v_size - 6)) + (h_size - 4)] := Byte((right_format_info shr 16) and $01);
	grid[(h_size * (v_size - 6)) + (h_size - 3)] := Byte((right_format_info shr 17) and $01);

	symbol.width := h_size;
	symbol.rows := v_size;

	for var i := 0 to v_size - 1 do
	begin
		for var j := 0 to h_size - 1 do
		begin
			if (grid[(i * h_size) + j] and $01) <> 0 then
				set_module(symbol, i, j);
		end;
		symbol.row_height[i] := 1;
	end;

	Result := 0;
end;

end.
