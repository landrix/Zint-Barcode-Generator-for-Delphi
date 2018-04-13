unit zint_gridmtx;

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

function grid_matrix(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses
  zint_common, zint_reedsol, zint_gb2312;

const GM_NUMBER	= 1;
const GM_LOWER = 2;
const GM_UPPER = 3;
const GM_MIXED = 4;
const GM_CONTROL = 5;
const GM_BYTE	= 6;
const GM_CHINESE = 7;

const EUROPIUM = '0123456789ABCDEFGHIJKLMOPRSTUVWXYZabcdefghijklmnopqrstuvwxyz ';

const shift_set : array[0..63] of Char = (
	{ From Table 7 - Encoding of control characters }
	#$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$09, #$0a, #$0b, #$0c, #$0d, #$0e, #$0f, { NULL -> SI }
	#$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1a, #$1b, #$1c, #$1d, #$1e, #$1f, { DLE -> US }
	'!', '"', '#', '$', '%', '&', '''', '(', ')', '*', '+', ',', '-', '.', '/', ':',
	';', '<', '=', '>', '?', '@', '[', '\', ']', '^', '_', '`', '(', '|', ')', '~'
);

const gm_recommend_cw : array[0..12] of Integer = ( 9, 30, 59, 114, 170, 237, 315, 405, 506, 618, 741, 875, 1021 );
const gm_max_cw : array[0..12] of Integer = ( 11, 40, 79, 146, 218, 305, 405, 521, 650, 794, 953, 1125, 1313 );

const gm_data_codewords : array[0..64] of Integer = (
	0, 15, 13, 11, 9,
	45, 40, 35, 30, 25,
	89, 79, 69, 59, 49,
	146, 130, 114, 98, 81,
	218, 194, 170, 146, 121,
	305, 271, 237, 203, 169,
	405, 360, 315, 270, 225,
	521, 463, 405, 347, 289,
	650, 578, 506, 434, 361,
	794, 706, 618, 530, 441,
	953, 847, 741, 635, 529,
	1125, 1000, 875, 750, 625,
	1313, 1167, 1021, 875, 729
);

const gm_n1 : array[0..12] of Integer = ( 18, 50, 98, 81, 121, 113, 113, 116, 121, 126, 118, 125, 122 );
const gm_b1 : array[0..12] of Integer = ( 1, 1, 1, 2, 2, 2, 2, 3, 2, 7, 5, 10, 6 );
const gm_b2 : array[0..12] of Integer = ( 0, 0, 0, 0, 0, 1, 2, 2, 4, 0, 4, 0, 6 );

const gm_ebeb : array[0..259] of Integer = (
	{ E1 B3 E2 B4 }
	0, 0, 0, 0, // version 1
	3, 1, 0, 0,
	5, 1, 0, 0,
	7, 1, 0, 0,
	9, 1, 0, 0,
	5, 1, 0, 0, // version 2
	10, 1, 0, 0,
	15, 1, 0, 0,
	20, 1, 0, 0,
	25, 1, 0, 0,
	9, 1, 0, 0, // version 3
	19, 1, 0, 0,
	29, 1, 0, 0,
	39, 1, 0, 0,
	49, 1, 0, 0,
	8, 2, 0, 0, // version 4
	16, 2, 0, 0,
	24, 2, 0, 0,
	32, 2, 0, 0,
	41, 1, 10, 1,
	12, 2, 0, 0, // version 5
	24, 2, 0, 0,
	36, 2, 0, 0,
	48, 2, 0, 0,
	61, 1, 60, 1,
	11, 3, 0, 0, // version 6
	23, 1, 22, 2,
	34, 2, 33, 1,
	45, 3, 0, 0,
	57, 1, 56, 2,
	12, 1, 11, 3, // version 7
	23, 2, 22, 2,
	34, 3, 33, 1,
	45, 4, 0, 0,
	57, 1, 56, 3,
	12, 2, 11, 3, // version 8
	23, 5, 0, 0,
	35, 3, 34, 2,
	47, 1, 46, 4,
	58, 4, 57, 1,
	12, 6, 0, 0, // version 9
	24, 6, 0, 0,
	36, 6, 0, 0,
	48, 6, 0, 0,
	61, 1, 60, 5,
	13, 4, 12, 3, // version 10
	26, 1, 25, 6,
	38, 5, 37, 2,
	51, 2, 50, 5,
	63, 7, 0, 0,
	12, 6, 11, 3, // version 11
	24, 4, 23, 5,
	36, 2, 35, 7,
	47, 9, 0, 0,
	59, 7, 58, 2,
	13, 5, 12, 5, // version 12
	25, 10, 0, 0,
	38, 5, 37, 5,
	50, 10, 0, 0,
	63, 5, 62, 5,
	13, 1, 12, 11, //version 13
	25, 3, 24, 9,
	37, 5, 36, 7,
	49, 7, 48, 5,
	61, 9, 60, 3
);

const gm_macro_matrix : array[0..728] of Integer = (
	728,625,626,627,628,629,630,631,632,633,634,635,636,637,638,639,640,641,642,643,644,645,646,647,648,649,650,
	727,624,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,651,
	726,623,528,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,553,652,
	725,622,527,440,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,463,554,653,
	724,621,526,439,360,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,381,464,555,654,
	723,620,525,438,359,288,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,307,382,465,556,655,
	722,619,524,437,358,287,224,169,170,171,172,173,174,175,176,177,178,179,180,181,182,241,308,383,466,557,656,
	721,618,523,436,357,286,223,168,121,122,123,124,125,126,127,128,129,130,131,132,183,242,309,384,467,558,657,
	720,617,522,435,356,285,222,167,120,81,82,83,84,85,86,87,88,89,90,133,184,243,310,385,468,559,658,
	719,616,521,434,355,284,221,166,119,80,49,50,51,52,53,54,55,56,91,134,185,244,311,386,469,560,659,
	718,615,520,433,354,283,220,165,118,79,48,25,26,27,28,29,30,57,92,135,186,245,312,387,470,561,660,
	717,614,519,432,353,282,219,164,117,78,47,24,9,10,11,12,31,58,93,136,187,246,313,388,471,562,661,
	716,613,518,431,352,281,218,163,116,77,46,23,8,1,2,13,32,59,94,137,188,247,314,389,472,563,662,
	715,612,517,430,351,280,217,162,115,76,45,22,7,0,3,14,33,60,95,138,189,248,315,390,473,564,663,
	714,611,516,429,350,279,216,161,114,75,44,21,6,5,4,15,34,61,96,139,190,249,316,391,474,565,664,
	713,610,515,428,349,278,215,160,113,74,43,20,19,18,17,16,35,62,97,140,191,250,317,392,475,566,665,
	712,609,514,427,348,277,214,159,112,73,42,41,40,39,38,37,36,63,98,141,192,251,318,393,476,567,666,
	711,608,513,426,347,276,213,158,111,72,71,70,69,68,67,66,65,64,99,142,193,252,319,394,477,568,667,
	710,607,512,425,346,275,212,157,110,109,108,107,106,105,104,103,102,101,100,143,194,253,320,395,478,569,668,
	709,606,511,424,345,274,211,156,155,154,153,152,151,150,149,148,147,146,145,144,195,254,321,396,479,570,669,
	708,605,510,423,344,273,210,209,208,207,206,205,204,203,202,201,200,199,198,197,196,255,322,397,480,571,670,
	707,604,509,422,343,272,271,270,269,268,267,266,265,264,263,262,261,260,259,258,257,256,323,398,481,572,671,
	706,603,508,421,342,341,340,339,338,337,336,335,334,333,332,331,330,329,328,327,326,325,324,399,482,573,672,
	705,602,507,420,419,418,417,416,415,414,413,412,411,410,409,408,407,406,405,404,403,402,401,400,483,574,673,
	704,601,506,505,504,503,502,501,500,499,498,497,496,495,494,493,492,491,490,489,488,487,486,485,484,575,674,
	703,600,599,598,597,596,595,594,593,592,591,590,589,588,587,586,585,584,583,582,581,580,579,578,577,576,675,
	702,701,700,699,698,697,696,695,694,693,692,691,690,689,688,687,686,685,684,683,682,681,680,679,678,677,676
);

{ Attempt to calculate the 'cost' of using numeric mode from a given position in number of bits }
{ Also ensures that numeric mode is not selected when it cannot be used: for example in
   a string which has '2.2.0' (cannot have more than one non-numeric character for each
   block of three numeric characters) }
function number_lat(const gbdata : TArrayOfInteger; _length : Integer; position : Integer) : Integer;
var
  sp : Integer;
  numb, nonum, done : Integer;
  tally : Integer;
begin
  numb := 0; nonum := 0;
  tally := 0;
  sp := position;

  repeat
    done := 0;

    if ((gbdata[sp] >= Ord('0')) and (gbdata[sp] <= Ord('9'))) then begin Inc(numb); done := 1; end;
    case Chr(gbdata[sp]) of
      ' ',
      '+',
      '-',
      '.',
      ',':
      begin
        Inc(nonum);
        done := 1;
      end;
    end;
    if ((sp + 1) < _length) then
    begin
      if ((gbdata[sp] = $13) and (gbdata[sp + 1] = $10)) then
      begin
        Inc(nonum);
        done := 1;
        Inc(sp);
      end;
    end;

    if (done = 0) then
    begin
      Inc(tally, 80)
    end
    else
    begin
      if (numb = 3) then
      begin
        if (nonum = 0) then
        begin
          Inc(tally, 10);
        end;
        if (nonum = 1) then
        begin
          Inc(tally, 20);
        end;
        if (nonum > 1) then
        begin
          Inc(tally, 80);
        end;
        numb := 0;
        nonum := 0;
      end;
    end;

    Inc(sp);
  until not ((sp < _length) and (sp <= (position + 8)));

  if (numb = 0) then
    Inc(tally, 80);

  if (numb > 1) then
  begin
    if (nonum = 0) then
      Inc(tally, 10);

    if (nonum = 1) then
      Inc(tally, 20);

    if (nonum > 1) then
      Inc(tally, 80);
  end;

  Result := tally; exit;
end;

{ In complete contrast to the method recommended in Annex D of the ANSI standard this
     code uses a look-ahead test in the same manner as Data Matrix. This decision was made
     because the 'official' algorithm does not provide clear methods for dealing with all
     possible combinations of input data }

function seek_forward(const gbdata : TArrayOfInteger; _length : Integer; position : Integer; current_mode : Integer) : Integer;
var
  number_count, byte_count, mixed_count, upper_count, lower_count, chinese_count : Integer;
  sp, best_mode, done : Integer;
  best_count, last : Integer;
begin
  last := -1;

  if (gbdata[position] > $ff) then begin result := GM_CHINESE; exit; end;

  case current_mode of
    GM_CHINESE:
    begin
      number_count := 13;
      byte_count := 13;
      mixed_count := 13;
      upper_count := 13;
      lower_count := 13;
      chinese_count := 0;
    end;
    GM_NUMBER:
    begin
      number_count := 0;
      byte_count := 10;
      mixed_count := 10;
      upper_count := 10;
      lower_count := 10;
      chinese_count := 10;
    end;
    GM_LOWER:
    begin
      number_count := 5;
      byte_count := 7;
      mixed_count := 7;
      upper_count := 5;
      lower_count := 0;
      chinese_count := 5;
    end;
    GM_UPPER:
    begin
      number_count := 5;
      byte_count := 7;
      mixed_count := 7;
      upper_count := 0;
      lower_count := 5;
      chinese_count := 5;
    end;
    GM_MIXED:
    begin
      number_count := 10;
      byte_count := 10;
      mixed_count := 0;
      upper_count := 10;
      lower_count := 10;
      chinese_count := 10;
    end;
    GM_BYTE:
    begin
      number_count := 4;
      byte_count := 0;
      mixed_count := 4;
      upper_count := 4;
      lower_count := 4;
      chinese_count := 4;
    end;
    else  { Start of symbol }
      number_count := 4;
      byte_count := 4;
      mixed_count := 4;
      upper_count := 4;
      lower_count := 4;
      chinese_count := 4;
  end;

  sp := position;
  while (sp < _length) and (sp <= (position + 8)) do
  begin
    done := 0;

    if (gbdata[sp] >= $ff) then
    begin
      Inc(byte_count, 17);
      Inc(mixed_count, 23);
      Inc(upper_count, 18);
      Inc(lower_count, 18);
      Inc(chinese_count, 13);
      done := 1;
    end;

    if ((gbdata[sp] >= Ord('a')) and (gbdata[sp] <= Ord('z'))) then
    begin
      Inc(byte_count, 8);
      Inc(mixed_count, 6);
      Inc(upper_count, 10);
      Inc(lower_count, 5);
      Inc(chinese_count, 13);
      done := 1;
    end;

    if ((gbdata[sp] >= Ord('A')) and (gbdata[sp] <= Ord('Z'))) then
    begin
      Inc(byte_count, 8);
      Inc(mixed_count, 6);
      Inc(upper_count, 5);
      Inc(lower_count, 10);
      Inc(chinese_count, 13);
      done := 1;
    end;

    if ((gbdata[sp] >= Ord('0')) and (gbdata[sp] <= Ord('9'))) then
    begin
      Inc(byte_count, 8);
      Inc(mixed_count, 6);
      Inc(upper_count, 8);
      Inc(lower_count, 8);
      Inc(chinese_count, 13);
      done := 1;
    end;

    if (gbdata[sp] = Ord(' ')) then
    begin
      Inc(byte_count, 8);
      Inc(mixed_count, 6);
      Inc(upper_count, 5);
      Inc(lower_count, 5);
      Inc(chinese_count, 13);
      done := 1;
    end;

    if (done = 0) then
    begin
      { Control character }
      Inc(byte_count, 8);
      Inc(mixed_count, 16);
      Inc(upper_count, 13);
      Inc(lower_count, 13);
      Inc(chinese_count, 13);
    end;

    if (gbdata[sp] >= $7f) then
    begin
      Inc(mixed_count, 20);
      Inc(upper_count, 20);
      Inc(lower_count, 20);
    end;

    Inc(sp);
  end;

  { Adjust for <end of line> }
  sp := position;
  while (sp < (_length - 1)) and (sp <= (position + 7)) do
  begin
    if ((gbdata[sp] = $13) and (gbdata[sp+1] = $10)) then
      Dec(chinese_count, 13);
    Inc(sp);
  end;

  { Adjust for double digits }
  sp := position;
  while (sp < (_length - 1)) and (sp <= (position + 7)) do
  begin
    if (sp <> last) then
    begin
      if (((gbdata[sp] >= Ord('0')) and (gbdata[sp] <= Ord('9'))) and ((gbdata[sp + 1] >= Ord('0')) and (gbdata[sp + 1] <= Ord('9')))) then
      begin
        Dec(chinese_count, 13);
        last := sp + 1;
      end;
    end;
    Inc(sp);
  end;

  { Numeric mode is more complex }
  Inc(number_count, number_lat(gbdata, _length, position));

  {$IFDEF DEBUG_ZINT}WriteLn(Format('C %d / B %d / M %d / U %d / L %d / N %d', [chinese_count, byte_count, mixed_count, upper_count, lower_count, number_count]));{$ENDIF}

  best_count := chinese_count;
  best_mode := GM_CHINESE;

  if (byte_count <= best_count) then
  begin
    best_count := byte_count;
    best_mode := GM_BYTE;
  end;

  if (mixed_count <= best_count) then
  begin
    best_count := mixed_count;
    best_mode := GM_MIXED;
  end;

  if (upper_count <= best_count) then
  begin
    best_count := upper_count;
    best_mode := GM_UPPER;
  end;

  if (lower_count <= best_count) then
  begin
    best_count := lower_count;
    best_mode := GM_LOWER;
  end;

  if (number_count <= best_count) then
  begin
    //best_count := number_count;
    best_mode := GM_NUMBER;
  end;

  result := best_mode; exit;
end;

{ Add the _length indicator for byte encoded blocks }
procedure add_byte_count(var binary : TArrayOfChar; byte_count_posn : Integer; byte_count : Integer);
var
  v : Integer;
begin
  v := $100;
  while v <> 0 do
  begin
    if (byte_count and v) <> 0 then
      binary[byte_count_posn] := '0'
    else
      binary[byte_count_posn] := '1';
    Inc(byte_count_posn);
    v := v shr 1;
  end;
end;

{ Add a control character to the data stream }
procedure add_shift_char(var binary : TArrayOfChar; shifty : Integer);
var
  i : Integer;
  glyph : Integer;
begin
  glyph := 0;
  for i := 0 to 63 do
  begin
    if (shift_set[i] = Chr(shifty)) then
      glyph := i;
  end;

  {$IFDEF DEBUG_ZINT}Write(Format('SHIFT [%d] ', [glyph]));{$ENDIF}

  bscan(binary, glyph, $20);
end;

{ Create a binary stream representation of the input data.
   7 sets are defined - Chinese characters, Numerals, Lower letters, Upper letters,
   Mixed numerals and latters, Control characters and 8-bit binary data }
function gm_encode(const gbdata : TArrayOfInteger; _length : Integer; var binary : TArrayOfChar; reader : Integer) : Integer;
var
  sp, current_mode, next_mode, last_mode, glyph : Integer;
  c1, c2, done : Integer;
  p, ppos : Integer;
  numbuf : TArrayOfInteger;
  punt : Integer;
  number_pad_posn : Integer;
  byte_count_posn, byte_count : Integer;
  shift, i : Integer;
begin
  glyph := 0;
  p := 0;
  SetLength(numbuf, 3);
  punt := 0;
  byte_count_posn := 0; byte_count := 0;

  strcpy(binary, '');

  sp := 0;
  current_mode := 0;
  //last_mode := 0;
  number_pad_posn := 0;

  if (reader <> 0) then
    concat(binary, '1010'); { FNC3 - Reader Initialisation }

  repeat
    next_mode := seek_forward(gbdata, _length, sp, current_mode);

    if (next_mode <> current_mode) then
    begin
      case current_mode of
        0:
        begin
          case next_mode of
            GM_CHINESE: concat(binary, '0001');
            GM_NUMBER: concat(binary, '0010');
            GM_LOWER: concat(binary, '0011');
            GM_UPPER: concat(binary, '0100');
            GM_MIXED: concat(binary, '0101');
            GM_BYTE: concat(binary, '0111');
          end;
        end;
        GM_CHINESE:
        begin
          case next_mode of
            GM_NUMBER: concat(binary, '1111111100001');
            GM_LOWER: concat(binary, '1111111100010');
            GM_UPPER: concat(binary, '1111111100011');
            GM_MIXED: concat(binary, '1111111100100');
            GM_BYTE: concat(binary, '1111111100101');
          end;
        end;
        GM_NUMBER:
        begin
          { add numeric block padding value }
          case p of
            1: begin binary[number_pad_posn] := '1'; binary[number_pad_posn + 1] := '0'; end; // 2 pad digits
            2: begin binary[number_pad_posn] := '0'; binary[number_pad_posn + 1] := '1'; end; // 1 pad digit
            3: begin binary[number_pad_posn] := '0'; binary[number_pad_posn + 1] := '0'; end; // 0 pad digits
          end;
          case next_mode of
            GM_CHINESE: concat(binary, '1111111011'); // 1019
            GM_LOWER: concat(binary, '1111111100'); // 1020
            GM_UPPER: concat(binary, '1111111101'); // 1021
            GM_MIXED: concat(binary, '1111111110');  // 1022
            GM_BYTE: concat(binary, '1111111111');  // 1023
          end;
        end;
        GM_LOWER,
        GM_UPPER:
        begin
          case next_mode of
            GM_CHINESE: concat(binary, '11100'); // 28
            GM_NUMBER: concat(binary, '11101'); // 29
            GM_LOWER,
            GM_UPPER: concat(binary, '11110'); // 30
            GM_MIXED: concat(binary, '1111100'); // 124
            GM_BYTE: concat(binary, '1111110'); // 126
          end;
        end;
        GM_MIXED:
        begin
          case next_mode of
            GM_CHINESE: concat(binary, '1111110001');  // 1009
            GM_NUMBER: concat(binary, '1111110010');  // 1010
            GM_LOWER: concat(binary, '1111110011');  // 1011
            GM_UPPER: concat(binary, '1111110100');  // 1012
            GM_BYTE: concat(binary, '1111110111');  // 1015
          end;
        end;
        GM_BYTE:
        begin
          { add byte block _length indicator }
          add_byte_count(binary, byte_count_posn, byte_count);
          byte_count := 0;
          case next_mode of
            GM_CHINESE: concat(binary, '0001');  // 1
            GM_NUMBER: concat(binary, '0010');  // 2
            GM_LOWER: concat(binary, '0011');  // 3
            GM_UPPER: concat(binary, '0100');  // 4
            GM_MIXED: concat(binary, '0101');  // 5
          end;
        end;
      end;

      {$IFDEF DEBUG_ZINT}
      case next_mode of
        GM_CHINESE: Write('CHIN ');
        GM_NUMBER: Write('NUMB ');
        GM_LOWER: Write('LOWR ');
        GM_UPPER: Write('UPPR ');
        GM_MIXED: Write('MIXD ');
        GM_BYTE: Write('BYTE ');
      end;
      {$ENDIF}
    end;
    last_mode := current_mode;
    current_mode := next_mode;

    case current_mode of
      GM_CHINESE:
      begin
        done := 0;
        if (gbdata[sp] > $ff) then
        begin
          { GB2312 character }
          c1 := (gbdata[sp] shr 8) and $ff;
          c2 := gbdata[sp] and $ff;

          if ((c1 >= $a0) and (c1 <= $a9)) then
            glyph := ($60 * (c1 - $a1)) + (c2 - $a0);

          if ((c1 >= $b0) and (c1 <= $f7)) then
            glyph := ($60 * (c1 - $b0 + 9)) + (c2  - $a0);

          done := 1;
        end;
        if (not (done <> 0)) then
        begin
          if (sp <> (_length - 1)) then
          begin
            if ((gbdata[sp] = $13) and (gbdata[sp + 1] = $10)) then
            begin
              { End of Line }
              glyph := 7776;
              Inc(sp);
            end;
            done := 1;
          end;
        end;
        if (not (done <> 0)) then
        begin
          if (sp <> (_length - 1)) then
          begin
            if (((gbdata[sp] >= Ord('0')) and (gbdata[sp] <= Ord('9'))) and
              ((gbdata[sp + 1] >= Ord('0')) and (gbdata[sp + 1] <= Ord('9')))) then
            begin
              { Two digits }
              glyph := 8033 + (10 * (gbdata[sp] - Ord('0'))) + (gbdata[sp + 1] - Ord('0'));
              Inc(sp);
            end;
          end;
        end;
        if (not (done <> 0)) then
          { Byte value }
          glyph := 7777 + gbdata[sp];

        {$IFDEF DEBUG_ZINT}Write(Format('[%d] ', [glyph]));{$ENDIF}

        bscan(binary, glyph, $1000);
        Inc(sp);
      end;

      GM_NUMBER:
      begin
        if (last_mode <> current_mode) then
        begin
          { Reserve a space for numeric digit padding value (2 bits) }
          number_pad_posn := strlen(binary);
          concat(binary, 'XX');
        end;
        p := 0;
        ppos := -1;

        { Numeric compression can also include certain combinations of
           non-numeric character }

        numbuf[0] := Ord('0');
        numbuf[1] := Ord('0');
        numbuf[2] := Ord('0');
        repeat
          if ((gbdata[sp] >= Ord('0')) and (gbdata[sp] <= Ord('9'))) then
          begin
            numbuf[p] := gbdata[sp];
            Inc(sp);
            Inc(p);
          end;
          case Chr(gbdata[sp]) of
            ' ',
            '+',
            '-',
            '.',
            ',':
            begin
              punt := gbdata[sp];
              Inc(sp);
              ppos := p;
            end;
          end;
          if (sp < (_length - 1)) then
          begin
            if ((gbdata[sp] = $13) and (gbdata[sp + 1] = $10)) then
            begin
              { <end of line> }
              punt := gbdata[sp];
              Inc(sp,  2);
              ppos := p;
            end;
          end;
        until not ((p < 3) and (sp < _length));

        if (ppos <> -1) then
        begin
          case Chr(punt) of
            ' ': glyph := 0;
            '+': glyph := 3;
            '-': glyph := 6;
            '.': glyph := 9;
            ',': glyph := 12;
            #$13: glyph := 15;
          end;
          Inc(glyph,  ppos);
          Inc(glyph,  1000);

          {$IFDEF DEBUG_ZINT}Write(Format('[%d] ', [glyph]));{$ENDIF}

          bscan(binary, glyph, $200);
        end;

        glyph := (100 * (numbuf[0] - Ord('0'))) + (10 * (numbuf[1] - Ord('0'))) + (numbuf[2] - Ord('0'));
        {$IFDEF DEBUG_ZINT}Write(Format('[%d] ', [glyph]));{$ENDIF}

        bscan(binary, glyph, $200);
      end;

      GM_BYTE:
      begin
        if (last_mode <> current_mode) then
        begin
          { Reserve space for byte block _length indicator (9 bits) }
          byte_count_posn := strlen(binary);
          concat(binary, 'LLLLLLLLL');
        end;
        if (byte_count = 512) then
        begin
          { Maximum byte block size is 512 bytes. If longer is needed then start a new block }
          add_byte_count(binary, byte_count_posn, byte_count);
          concat(binary, '0111');
          byte_count_posn := strlen(binary);
          concat(binary, 'LLLLLLLLL');
          byte_count := 0;
        end;

        glyph := gbdata[sp];
        {$IFDEF DEBUG_ZINT}Write(Format('[%d] ', [glyph]));{$ENDIF}
        bscan(binary, glyph, $80);
        Inc(sp);
        Inc(byte_count);
      end;

      GM_MIXED:
      begin
        shift := 1;
        if ((gbdata[sp] >= Ord('0')) and (gbdata[sp] <= Ord('9'))) then shift := 0;
        if ((gbdata[sp] >= Ord('A')) and (gbdata[sp] <= Ord('Z'))) then shift := 0;
        if ((gbdata[sp] >= Ord('a')) and (gbdata[sp] <= Ord('z'))) then shift := 0;
        if (gbdata[sp] = Ord(' ')) then shift := 0;

        if (shift = 0) then
        begin
          { Mixed Mode character }
          glyph := posn(EUROPIUM, gbdata[sp]);
          {$IFDEF DEBUG_ZINT}Write(Format('[%d] ', [glyph]));{$ENDIF}

          bscan(binary, glyph, $20);
        end
        else
        begin
          { Shift Mode character }
          concat(binary, '1111110110'); { 1014 - shift indicator }
          add_shift_char(binary, gbdata[sp]);
        end;

        Inc(sp);
      end;

      GM_UPPER:
      begin
        shift := 1;
        if ((gbdata[sp] >= Ord('A')) and (gbdata[sp] <= Ord('Z'))) then  shift := 0;
        if (gbdata[sp] = Ord(' ')) then shift := 0;

        if (shift = 0) then
        begin
          { Upper character }
          glyph := posn('ABCDEFGHIJKLMNOPQRSTUVWXYZ ', gbdata[sp]);
          {$IFDEF DEBUG_ZINT}Write(Format('[%d] ', [glyph]));{$ENDIF}

          bscan(binary, glyph, $10);
        end
        else
        begin
          { Shift Mode character }
          concat(binary, '1111101'); { 127 - shift indicator }
          add_shift_char(binary, gbdata[sp]);
        end;

        Inc(sp);
      end;

      GM_LOWER:
      begin
        shift := 1;
        if ((gbdata[sp] >= Ord('a')) and (gbdata[sp] <= Ord('z'))) then shift := 0;
        if (gbdata[sp] = Ord(' ')) then shift := 0;

        if (shift = 0) then
        begin
          { Lower character }
          glyph := posn('abcdefghijklmnopqrstuvwxyz ', gbdata[sp]);
          {$IFDEF DEBUG_ZINT}Write(Format('[%d] ', [glyph]));{$ENDIF}

          bscan(binary, glyph, $10);
        end
        else
        begin
          { Shift Mode character }
          concat(binary, '1111101'); { 127 - shift indicator }
          add_shift_char(binary, gbdata[sp]);
        end;

        Inc(sp);
      end;
    end;
    if (strlen(binary) > 9191) then
    begin
      result := ZERROR_TOO_LONG; exit;
    end;

  until not (sp < _length);

  if (current_mode = GM_NUMBER) then
  begin
    { add numeric block padding value }
    case p of
      1: begin binary[number_pad_posn] := '1'; binary[number_pad_posn + 1] := '0'; end; // 2 pad digits
      2: begin binary[number_pad_posn] := '0'; binary[number_pad_posn + 1] := '1'; end; // 1 pad digit
      3: begin binary[number_pad_posn] := '0'; binary[number_pad_posn + 1] := '0'; end; // 0 pad digits
    end;
  end;

  if (current_mode = GM_BYTE) then
    { Add byte block _length indicator }
    add_byte_count(binary, byte_count_posn, byte_count);

  { Add 'end of data' character }
  case current_mode of
    GM_CHINESE: concat(binary, '1111111100000');  // 8160
    GM_NUMBER: concat(binary, '1111111010');  // 1018
    GM_LOWER,
    GM_UPPER: concat(binary, '11011');  // 27
    GM_MIXED: concat(binary, '1111110000');  // 1008
    GM_BYTE: concat(binary, '0000');  // 0
  end;

  { Add padding bits if required }
  p := 7 - (strlen(binary) mod 7);
  if (p = 7) then p := 0;
  for i := 0 to p - 1 do
    concat(binary, '0');

  if (strlen(binary) > 9191) then
  begin
    result := ZERROR_TOO_LONG; exit;
  end;

  result := 0; exit;
end;

procedure gm_add_ecc(var binary : TArrayOfChar; data_posn : Integer; layers : Integer; ecc_level : Integer; word : TArrayOfInteger);
var
  data_cw, i, j, wp : Integer;
  n1, b1, n2, b2, e1, b3, e2 : Integer;
  block_size, data_size, ecc_size : Integer;
  data, block : TArrayOfInteger;
  data_block, ecc_block : TArrayOfByte;
  RSGlobals : TRSGlobals;
begin
  SetLength(data, 1320); SetLength(block, 130);
  SetLength(data_block, 115); SetLength(ecc_block, 70);

  data_cw := gm_data_codewords[((layers - 1) * 5) + (ecc_level - 1)];

  for i := 0 to 1319 do
    data[i] := 0;

  { Convert from binary sream to 7-bit codewords }
  for i := 0 to data_posn - 1 do
  begin
    if (binary[i * 7] = '1') then Inc(data[i], $40);
    if (binary[(i * 7) + 1] = '1') then Inc(data[i], $20);
    if (binary[(i * 7) + 2] = '1') then Inc(data[i], $10);
    if (binary[(i * 7) + 3] = '1') then Inc(data[i], $08);
    if (binary[(i * 7) + 4] = '1') then Inc(data[i], $04);
    if (binary[(i * 7) + 5] = '1') then Inc(data[i], $02);
    if (binary[(i * 7) + 6] = '1') then Inc(data[i], $01);
  end;

  { Add padding codewords }
  data[data_posn] := $00;
  for i := (data_posn + 1) to data_cw - 1 do
  begin
    if (i and 1) <> 0 then
      data[i] := $7e
    else
      data[i] := $00;
  end;

  { Get block sizes }
  n1 := gm_n1[(layers - 1)];
  b1 := gm_b1[(layers - 1)];
  n2 := n1 - 1;
  b2 := gm_b2[(layers - 1)];
  e1 := gm_ebeb[((layers - 1) * 20) + ((ecc_level - 1) * 4)];
  b3 := gm_ebeb[((layers - 1) * 20) + ((ecc_level - 1) * 4) + 1];
  e2 := gm_ebeb[((layers - 1) * 20) + ((ecc_level - 1) * 4) + 2];

  { Split the data into blocks }
  wp := 0;
  for i := 0 to (b1 + b2) - 1 do
  begin
    if (i < b1) then block_size := n1 else block_size := n2;
    if (i < b3) then ecc_size := e1 else ecc_size := e2;
    data_size := block_size - ecc_size;

    { WriteLn(Format('block %d/%d: data %d / ecc %d\n', i + 1, (b1 + b2), data_size, ecc_size);}

    for j := 0 to data_size - 1 do
    begin
      data_block[j] := data[wp];
      Inc(wp);
    end;

    { Calculate ECC data for this block }
    rs_init_gf($89, RSGlobals);
    rs_init_code(ecc_size, 1, RSGlobals);
    rs_encode(data_size, data_block, ecc_block, RSGlobals);
    rs_free(RSGlobals);

    { Correct error correction data but in reverse order }
    for j := 0 to data_size - 1 do
      block[j] := data_block[j];

    for j := 0 to ecc_size - 1 do
      block[(j + data_size)] := ecc_block[ecc_size - j - 1];

    for j := 0 to n2 - 1 do
      word[((b1 + b2) * j) + i] := block[j];

    if (block_size = n1) then
      word[((b1 + b2) * (n1 - 1)) + i] := block[(n1 - 1)];
  end;
end;

procedure place_macromodule(var grid : TArrayOfChar; x : Integer; y : Integer; word1 : Integer; word2 : Integer; size : Integer);
var
  i, j : Integer;
begin
  i := (x * 6) + 1;
  j := (y * 6) + 1;

  if (word2 and $40) <> 0 then grid[(j * size) + i + 2] := '1';
  if (word2 and $20) <> 0 then grid[(j * size) + i + 3] := '1';
  if (word2 and $10) <> 0 then grid[((j + 1) * size) + i] := '1';
  if (word2 and $08) <> 0 then grid[((j + 1) * size) + i + 1] := '1';
  if (word2 and $04) <> 0 then grid[((j + 1) * size) + i + 2] := '1';
  if (word2 and $02) <> 0 then grid[((j + 1) * size) + i + 3] := '1';
  if (word2 and $01) <> 0 then grid[((j + 2) * size) + i] := '1';
  if (word1 and $40) <> 0 then grid[((j + 2) * size) + i + 1] := '1';
  if (word1 and $20) <> 0 then grid[((j + 2) * size) + i + 2] := '1';
  if (word1 and $10) <> 0 then grid[((j + 2) * size) + i + 3] := '1';
  if (word1 and $08) <> 0 then grid[((j + 3) * size) + i] := '1';
  if (word1 and $04) <> 0 then grid[((j + 3) * size) + i + 1] := '1';
  if (word1 and $02) <> 0 then grid[((j + 3) * size) + i + 2] := '1';
  if (word1 and $01) <> 0 then grid[((j + 3) * size) + i + 3] := '1';
end;

procedure place_data_in_grid(const word : TArrayOfInteger; var grid : TArrayOfChar; modules : Integer; size : Integer);
var
  x, y, macromodule, offset : Integer;
begin
  offset := 13 - ((modules - 1) div 2);
  for y := 0 to modules - 1 do
  begin
    for x := 0 to modules - 1 do
    begin
      macromodule := gm_macro_matrix[((y + offset) * 27) + (x + offset)];
      place_macromodule(grid, x, y, word[macromodule * 2], word[(macromodule * 2) + 1], size);
    end;
  end;
end;

{ Place the layer ID into each macromodule }
procedure place_layer_id(var grid : TArrayOfChar; size : Integer; layers : Integer; modules : Integer; ecc_level : Integer);
var
  i, j, layer, start, stop : Integer;
  layerid : TArrayOfInteger;
  id : TArrayOfInteger;
begin
  SetLength(layerid, layers + 1);
  SetLength(id, modules * modules);

  { Calculate Layer IDs }
  for i := 0 to layers do
  begin
    if (ecc_level = 1) then
      layerid[i] := 3 - (i mod 4)
    else
      layerid[i] := (i + 5 - ecc_level) mod 4;
  end;

  for i := 0 to modules - 1 do
    for j := 0 to modules - 1 do
      id[(i * modules) + j] := 0;

  { Calculate which value goes in each macromodule }
  start := modules div 2;
  stop := modules div 2;
  for layer := 0 to layers do
  begin
    for i := start to stop do
    begin
      id[(start * modules) + i] := layerid[layer];
      id[(i * modules) + start] := layerid[layer];
      id[((modules - start - 1) * modules) + i] := layerid[layer];
      id[(i * modules) + (modules - start - 1)] := layerid[layer];
    end;
    Dec(start);
    Inc(stop);
  end;

  { Place the data in the grid }
  for i := 0 to modules - 1 do
  begin
    for j := 0 to modules - 1 do
    begin
      if (id[(i * modules) + j] and $02) <> 0 then
        grid[(((i * 6) + 1) * size) + (j * 6) + 1] := '1';

      if (id[(i * modules) + j] and $01) <> 0 then
        grid[(((i * 6) + 1) * size) + (j * 6) + 2] := '1';
    end;
  end;
end;

function grid_matrix(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  size, modules, dark, error_number : Integer;
  auto_layers, min_layers, layers, auto_ecc_level, min_ecc_level, ecc_level : Integer;
  x, y, i, j, glyph : Integer;
  binary : TArrayOfChar;
  data_cw, input_latch : Integer;
  word : TArrayOfInteger;
  data_max, reader : Integer;
  utfdata : TArrayOfInteger;
  gbdata : TArrayOfInteger;
  grid : TArrayOfChar;
begin
  SetLength(binary, 9300);
  input_latch := 0;
  SetLength(word, 1460);
  reader := 0;
  SetLength(utfdata, _length + 1);
  SetLength(gbdata, _length + 1);

  for i := 0 to 1459 do
    word[i] := 0;

  case symbol.input_mode of
    DATA_MODE:
    begin
      for i := 0 to _length - 1 do
        gbdata[i] := source[i];
    end;
    else
      { Convert Unicode input to GB-2312 }
      error_number := utf8toutf16(symbol, source, utfdata, _length);
      if (error_number <> 0) then begin result := error_number; exit; end;

      for i := 0 to _length - 1 do
      begin
        if (utfdata[i] <= $ff) then
          gbdata[i] := utfdata[i]
        else
        begin
          j := 0;
          glyph := 0;
          repeat
            if (gb2312_lookup[j * 2] = utfdata[i]) then
              glyph := gb2312_lookup[(j * 2) + 1];
            Inc(j);
          until not ((j < 7445) and (glyph = 0));
          if (glyph = 0) then
          begin
            strcpy(symbol.errtxt, 'Invalid character in input data');
            result := ZERROR_INVALID_DATA; exit;
          end;
          gbdata[i] := glyph;
        end;
      end;
  end;

  if (symbol.output_options and READER_INIT) <> 0 then reader := 1;

  error_number := gm_encode(gbdata, _length, binary, reader);
  if (error_number <> 0) then
  begin
    strcpy(symbol.errtxt, 'Input data too long');
    result := error_number; exit;
  end;

  { Determine the size of the symbol }
  data_cw := strlen(binary) div 7;

  auto_layers := 13;
  for i := 12 downto 1 do
  begin
    if (gm_recommend_cw[(i - 1)] >= data_cw) then auto_layers := i;
  end;
  min_layers := 13;
  for i := 12 downto 0 do
  begin
    if (gm_max_cw[(i - 1)] >= data_cw) then min_layers := i;
  end;
  layers := auto_layers;
  auto_ecc_level := 3;
  if (layers = 1) then auto_ecc_level := 5;
  if ((layers = 2) or (layers = 3)) then auto_ecc_level := 4;
  min_ecc_level := 1;
  if (layers = 1) then min_ecc_level := 4;
  if ((layers = 2) or (layers = 3)) then min_ecc_level := 2;
  ecc_level := auto_ecc_level;

  if ((symbol.option_2 >= 1) and (symbol.option_2 <= 13)) then
  begin
    input_latch := 1;
    if (symbol.option_2 > min_layers) then
      layers := symbol.option_2
    else
      layers := min_layers;
  end;

  if (input_latch = 1) then
  begin
    auto_ecc_level := 3;
    if (layers = 1) then auto_ecc_level := 5;
    if ((layers = 2) or (layers = 3)) then auto_ecc_level := 4;
    ecc_level := auto_ecc_level;
    if (data_cw > gm_data_codewords[(5 * (layers - 1)) + (ecc_level - 1)]) then
      Inc(layers);
  end;

  if (input_latch = 0) then
  begin
    if ((symbol.option_1 >= 1) and (symbol.option_1 <= 5)) then
    begin
      if (symbol.option_1 > min_ecc_level) then
        ecc_level := symbol.option_1
      else
        ecc_level := min_ecc_level;
    end;
    if (data_cw > gm_data_codewords[(5 * (layers - 1)) + (ecc_level - 1)]) then
    begin
      repeat
        Inc(layers);
      until not ((data_cw > gm_data_codewords[(5 * (layers - 1)) + (ecc_level - 1)]) and (layers <= 13));
    end;
  end;

  data_max := 1313;
  case ecc_level of
    2: data_max := 1167;
    3: data_max := 1021;
    4: data_max := 875;
    5: data_max := 729;
  end;

  if (data_cw > data_max) then
  begin
    strcpy(symbol.errtxt, 'Input data too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  gm_add_ecc(binary, data_cw, layers, ecc_level, word);
  size := 6 + (layers * 12);
  modules := 1 + (layers * 2);

  SetLength(grid, size * size);

  for x := 0 to size - 1 do
    for y := 0 to size - 1 do
      grid[(y * size) + x] := '0';

  place_data_in_grid(word, grid, modules, size);
  place_layer_id(grid, size, layers, modules, ecc_level);

  { Add macromodule frames }
  for x := 0 to modules - 1 do
  begin
    dark := 1 - (x and 1);
    for y := 0 to modules - 1 do
    begin
      if (dark = 1) then
      begin
        for i := 0 to 4 do
        begin
          grid[((y * 6) * size) + (x * 6) + i] := '1';
          grid[(((y * 6) + 5) * size) + (x * 6) + i] := '1';
          grid[(((y * 6) + i) * size) + (x * 6)] := '1';
          grid[(((y * 6) + i) * size) + (x * 6) + 5] := '1';
        end;
        grid[(((y * 6) + 5) * size) + (x * 6) + 5] := '1';
        dark := 0;
      end
      else
        dark := 1;
    end;
  end;

  { Copy values to symbol }
  symbol.width := size;
  symbol.rows := size;

  for x := 0 to size - 1 do
  begin
    for y := 0 to size - 1 do
    begin
      if (grid[(y * size) + x] = '1') then
        set_module(symbol, y, x);
    end;
    symbol.row_height[x] := 1;
  end;

  result := 0; exit;
end;

end.

