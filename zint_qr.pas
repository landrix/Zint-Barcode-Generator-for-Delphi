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
function qr_code_segs(symbol : zint_symbol; const segs : TZintSegments): Integer;
function microqr(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;
function upnqr(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;
function rmqr(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;

implementation

uses
  SysUtils, zint_reedsol, zint_common, zint_sjis, zint_helper;

const
  LEVEL_L	= 1;
  LEVEL_M	= 2;
  LEVEL_Q	= 3;
  LEVEL_H	= 4;

const
  RHODIUM = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:';

  qr_data_codewords_L: array [0..39] of Integer = (
    19, 34, 55, 80, 108, 136, 156, 194, 232, 274, 324, 370, 428, 461, 523, 589, 647,
    721, 795, 861, 932, 1006, 1094, 1174, 1276, 1370, 1468, 1531, 1631,
    1735, 1843, 1955, 2071, 2191, 2306, 2434, 2566, 2702, 2812, 2956);

  qr_data_codewords_M: array [0..39] of Integer = (
    16, 28, 44, 64, 86, 108, 124, 154, 182, 216, 254, 290, 334, 365, 415, 453, 507,
    563, 627, 669, 714, 782, 860, 914, 1000, 1062, 1128, 1193, 1267,
    1373, 1455, 1541, 1631, 1725, 1812, 1914, 1992, 2102, 2216, 2334);

  qr_data_codewords_Q: array [0..39] of Integer = (
    13, 22, 34, 48, 62, 76, 88, 110, 132, 154, 180, 206, 244, 261, 295, 325, 367,
    397, 445, 485, 512, 568, 614, 664, 718, 754, 808, 871, 911,
    985, 1033, 1115, 1171, 1231, 1286, 1354, 1426, 1502, 1582, 1666);

  qr_data_codewords_H: array [0..39] of Integer = (
    9, 16, 26, 36, 46, 60, 66, 86, 100, 122, 140, 158, 180, 197, 223, 253, 283,
    313, 341, 385, 406, 442, 464, 514, 538, 596, 628, 661, 701,
    745, 793, 845, 901, 961, 986, 1054, 1096, 1142, 1222, 1276);

  qr_total_codewords: array [0..39] of Integer = (
    26, 44, 70, 100, 134, 172, 196, 242, 292, 346, 404, 466, 532, 581, 655, 733, 815,
    901, 991, 1085, 1156, 1258, 1364, 1474, 1588, 1706, 1828, 1921, 2051,
    2185, 2323, 2465, 2611, 2761, 2876, 3034, 3196, 3362, 3532, 3706);

  qr_blocks_L: array [0..39] of Integer = (
    1, 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 4, 6, 6, 6, 6, 7, 8, 8, 9, 9, 10, 12, 12,
    12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25);

  qr_blocks_M: array [0..39] of Integer = (
    1, 1, 1, 2, 2, 4, 4, 4, 5, 5, 5, 8, 9, 9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20,
    21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49);

  qr_blocks_Q: array [0..39] of Integer = (
    1, 1, 2, 2, 4, 4, 6, 6, 8, 8, 8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25,
    27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68);

  qr_blocks_H: array [0..39] of Integer = (
    1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30,
    32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81);

  qr_sizes: array [0..39] of Integer = (
    21, 25, 29, 33, 37, 41, 45, 49, 53, 57, 61, 65, 69, 73, 77, 81, 85, 89, 93, 97,
    101, 105, 109, 113, 117, 121, 125, 129, 133, 137, 141, 145, 149, 153, 157, 161, 165, 169, 173, 177);

  micro_qr_sizes: array [0..3] of Integer = (
    11, 13, 15, 17);

	rmqr_height: array [0..31] of Integer = (
		 7,  7,  7,  7,  7,
		 9,  9,  9,  9,  9,
		11, 11, 11, 11, 11, 11,
		13, 13, 13, 13, 13, 13,
		15, 15, 15, 15, 15,
		17, 17, 17, 17, 17
	);

	rmqr_width: array [0..31] of Integer = (
		43, 59, 77, 99, 139,
		43, 59, 77, 99, 139,
		27, 43, 59, 77,  99, 139,
		27, 43, 59, 77,  99, 139,
		43, 59, 77, 99, 139,
		43, 59, 77, 99, 139
	);

	rmqr_data_codewords: array [0..1, 0..31] of Integer = (
		(
			 6, 12, 20, 28, 44,
			12, 21, 31, 42, 63,
			 7, 19, 31, 43, 57, 84,
			12, 27, 38, 53, 73, 106,
			33, 48, 67, 88, 127,
			39, 56, 78, 100, 152
		),
		(
			 3,  7, 10, 14, 24,
			 7, 11, 17, 22, 33,
			 5, 11, 15, 23, 29, 42,
			 7, 13, 20, 29, 35, 54,
			15, 26, 31, 48, 69,
			21, 28, 38, 56, 76
		)
	);

	// Highest version index per fixed-height selector (option_2 33..38).
	rmqr_fixed_height_upper_bound: array [0..6] of Integer = (
		-1, 4, 9, 15, 21, 26, 31
	);

	rmqr_total_codewords: array [0..31] of Integer = (
		13, 21,  32,  44,  68,
		21, 33,  49,  66,  99,
		15, 31,  47,  67,  89, 132,
		21, 41,  60,  85, 113, 166,
		51, 74, 103, 136, 199,
		61, 88, 122, 160, 232
	);

	rmqr_numeric_cci: array [0..31] of Integer = (
		4, 5, 6, 7, 7,
		5, 6, 7, 7, 8,
		4, 6, 7, 7, 8, 8,
		5, 6, 7, 7, 8, 8,
		7, 7, 8, 8, 9,
		7, 8, 8, 8, 9
	);

	rmqr_alphanum_cci: array [0..31] of Integer = (
		3, 5, 5, 6, 6,
		5, 5, 6, 6, 7,
		4, 5, 6, 6, 7, 7,
		5, 6, 6, 7, 7, 8,
		6, 7, 7, 7, 8,
		6, 7, 7, 8, 8
	);

	rmqr_byte_cci: array [0..31] of Integer = (
		3, 4, 5, 5, 6,
		4, 5, 5, 6, 6,
		3, 5, 5, 6, 6, 7,
		4, 5, 6, 6, 7, 7,
		6, 6, 7, 7, 7,
		6, 6, 7, 7, 8
	);

	rmqr_kanji_cci: array [0..31] of Integer = (
		2, 3, 4, 5, 5,
		3, 4, 5, 5, 6,
		2, 4, 5, 5, 6, 6,
		3, 5, 5, 6, 6, 7,
		5, 5, 6, 6, 7,
		5, 6, 6, 6, 7
	);

	rmqr_blocks: array [0..1, 0..31] of Integer = (
		(
			1, 1, 1, 1, 1,
			1, 1, 1, 1, 2,
			1, 1, 1, 1, 2, 2,
			1, 1, 1, 2, 2, 3,
			1, 1, 2, 2, 3,
			1, 2, 2, 3, 4
		),
		(
			1, 1, 1, 1, 2,
			1, 1, 2, 2, 3,
			1, 1, 2, 2, 2, 3,
			1, 1, 2, 2, 3, 4,
			2, 2, 3, 4, 5,
			2, 2, 3, 4, 6
		)
	);

	rmqr_table_d1: array [0..19] of Integer = (
		21,  0,  0,   0,
		19, 39,  0,   0,
		25, 51,  0,   0,
		23, 49, 75,   0,
		27, 55, 83, 111
	);

	rmqr_format_info_left: array [0..63] of Cardinal = (
		$1FAB2, $1E597, $1DBDD, $1C4F8, $1B86C, $1A749, $19903, $18626, $17F0E, $1602B,
		$15E61, $14144, $13DD0, $122F5, $11CBF, $1039A, $0F1CA, $0EEEF, $0D0A5, $0CF80,
		$0B314, $0AC31, $0927B, $08D5E, $07476, $06B53, $05519, $04A3C, $036A8, $0298D,
		$017C7, $008E2, $3F367, $3EC42, $3D208, $3CD2D, $3B1B9, $3AE9C, $390D6, $38FF3,
		$376DB, $369FE, $357B4, $34891, $33405, $32B20, $3156A, $30A4F, $2F81F, $2E73A,
		$2D970, $2C655, $2BAC1, $2A5E4, $29BAE, $2848B, $27DA3, $26286, $25CCC, $243E9,
		$23F7D, $22058, $21E12, $20137
	);

	rmqr_format_info_right: array [0..63] of Cardinal = (
		$20A7B, $2155E, $22B14, $23431, $248A5, $25780, $269CA, $276EF, $28FC7, $290E2,
		$2AEA8, $2B18D, $2CD19, $2D23C, $2EC76, $2F353, $30103, $31E26, $3206C, $33F49,
		$343DD, $35CF8, $362B2, $37D97, $384BF, $39B9A, $3A5D0, $3BAF5, $3C661, $3D944,
		$3E70E, $3F82B, $003AE, $01C8B, $022C1, $03DE4, $04170, $05E55, $0601F, $07F3A,
		$08612, $09937, $0A77D, $0B858, $0C4CC, $0DBE9, $0E5A3, $0FA86, $108D6, $117F3,
		$129B9, $1369C, $14A08, $1552D, $16B67, $17442, $18D6A, $1924F, $1AC05, $1B320,
		$1CFB4, $1D091, $1EEDB, $1F1FE
	);

	rmqr_version_names: array [0..37] of string = (
		'R7x43', 'R7x59', 'R7x77', 'R7x99', 'R7x139', 'R9x43', 'R9x59', 'R9x77',
		'R9x99', 'R9x139', 'R11x27', 'R11x43', 'R11x59', 'R11x77', 'R11x99', 'R11x139',
		'R13x27', 'R13x43', 'R13x59', 'R13x77', 'R13x99', 'R13x139', 'R15x43', 'R15x59',
		'R15x77', 'R15x99', 'R15x139', 'R17x43', 'R17x59', 'R17x77', 'R17x99', 'R17x139',
		'R7xW', 'R9xW', 'R11xW', 'R13xW', 'R15xW', 'R17xW'
	);

  qr_align_loopsize : array [0..39] of integer = (
  	0, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7);

  qr_table_e1 : array [0..272] of integer = (
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


  qr_annex_c : array [0..31] of Cardinal = (
    // Format information bit sequences
    $5412, $5125, $5e7c, $5b4b, $45f9, $40ce, $4f97, $4aa0, $77c4, $72f3, $7daa, $789d,
    $662f, $6318, $6c41, $6976, $1689, $13be, $1ce7, $19d0, $0762, $0255, $0d0c, $083b,
    $355f, $3068, $3f31, $3a06, $24b4, $2183, $2eda, $2bed
    );

  qr_annex_d : array [0..33] of Integer= (
    // Version information bit sequences
    $07c94, $085bc, $09a99, $0a4d3, $0bbf6, $0c762, $0d847, $0e60d, $0f928, $10b78,
    $1145d, $12a17, $13532, $149a6, $15683, $168c9, $177ec, $18ec4, $191e1, $1afab,
    $1b08e, $1cc1a, $1d33f, $1ed75, $1f250, $209d5, $216f0, $228ba, $2379f, $24b0b,
    $2542e, $26a64, $27541, $28c69);

  qr_annex_c1 : array [0..31] of Integer = (
    // Micro QR Code format information
    $4445, $4172, $4e2b, $4b1c, $55ae, $5099, $5fc0, $5af7, $6793, $62a4, $6dfd, $68ca, $7678, $734f,
    $7c16, $7921, $06de, $03e9, $0cb0, $0987, $1735, $1202, $1d5b, $186c, $2508, $203f, $2f66, $2a51, $34e3,
    $31d4, $3e8d, $3bba);

function in_alpha(glyph : Byte) : integer;
var
  retval : integer;
  cglyph : char;
begin
	// Returns true if input glyph is in the Alphanumeric set
	retval := 0;
	cglyph := Chr(glyph);

	if((cglyph >= '0') and (cglyph <= '9')) then
  begin
		retval := 1;
	end;

	if((cglyph >= 'A') and (cglyph <= 'Z')) then
	begin
    retval := 1;
	end;

	case (cglyph) of
		' ',
		'$',
    '%',
		'*',
		'+',
		'-',
		'.',
		'/',
		':' : retval := 1;
  end;

	Result := retval;
end;

procedure define_mode(var mode: TArrayOfChar; jisdata : TArrayOfInteger; _length : Integer; gs1 : Integer);
var
  i, mlen, j : Integer;
begin
  // Values placed into mode[] are: K = Kanji, B = Binary, A = Alphanumeric, N = Numeric
	for i := 0 to _length-1 do
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
	for i := 0 to _length-1 do
  begin
		if (mode[i] = 'N') then
    begin
			if(((i <> 0) and (mode[i - 1] <> 'N')) or (i = 0)) then
      begin
				mlen := 0;
				while (((mlen + i) < _length) and (mode[mlen + i] = 'N')) do
					inc(mlen);

				if(mlen < 6) then
        begin
					for j := 0 to mlen-1 do
          begin
						mode[i + j] := 'A';
					end;
				end;
			end;
		end;
	end;

	// If less than 4 alphanumeric characters together then don't use alphanumeric mode
	for i := 0 to _length-1 do
  begin
		if mode[i] = 'A' then
    begin
			if (((i <> 0) and (mode[i - 1] <> 'A')) or (i = 0)) then
      begin
				mlen := 0;
				while (((mlen + i) < _length) and (mode[mlen + i] = 'A')) do
					inc(mlen);

				if(mlen < 6) then
        begin
					for j := 0 to mlen-1 do
          begin
						mode[i + j] := 'B';
					end;
				end
			end
		end
	end
end;

function estimate_binary_length(mode : TArrayOfChar; _length : Integer; gs1 : Integer; version : Integer) : Integer;
var
  i, count : Integer;
  current  : Char;
  a_count  : Integer;
  n_count  : Integer;
  scheme: Integer;
  numeric_cci, alpha_cci, byte_cci, kanji_cci: Integer;
begin
  count := 0;
  current := #0;
  a_count := 0;
  n_count := 0;

	if(version <= 9) then
		scheme := 1
	else if((version >= 10) and (version <= 26)) then
		scheme := 2
	else
		scheme := 3;

	case scheme of
		1: begin numeric_cci := 10; alpha_cci := 9; byte_cci := 8; kanji_cci := 8; end;
		2: begin numeric_cci := 12; alpha_cci := 11; byte_cci := 16; kanji_cci := 10; end;
	else
		begin numeric_cci := 14; alpha_cci := 13; byte_cci := 16; kanji_cci := 12; end;
	end;

	// Make an estimate (worst case scenario) of how long the binary string will be


	if (gs1<>0) then inc(count, 4);

	for i := 0 to _length - 1 do
  begin
		if(mode[i] <> current) then
    begin
			case mode[i] of
				'K': begin inc(count, kanji_cci + 4); current := 'K'; end;
				'B': begin inc(count, byte_cci + 4); current := 'B'; end;
				'A': begin inc(count, alpha_cci + 4); current := 'A'; a_count := 0; end;
				'N': begin inc(count, numeric_cci + 4); current := 'N'; n_count := 0; end;
      end;
    end;

		case (mode[i]) of
		  'K': inc(count, 13);
		  'B': inc(count, 8);
		  'A': begin
			       inc(a_count);

             if((a_count and 1) = 0) then
             begin
               inc(count, 5);        // 11 in total
               a_count := 0;
             end
             else
               inc(count, 6);
           end;
		  'N': begin
             inc(n_count);

             if ((n_count mod 3) = 0) then
             begin
               inc(count, 3);     // 10 in total
               n_count := 0;
             end
             else
             if ((n_count and 1) = 0) then
               inc(count, 3) // 7 in total
             else
               inc(count, 4);
          end;
		end;
	end;

	Result:=count;
end;

function qr_eci_length_bits(const eci : Integer) : Integer;
begin
	if eci <= 127 then
		Result := 8
	else if eci <= 16383 then
		Result := 16
	else
		Result := 24;
end;

function estimate_binary_length_segs(mode : TArrayOfChar; _length : Integer; gs1 : Integer; version : Integer;
	const seg_ends : TArrayOfInteger; const seg_ecis : TArrayOfInteger; seg_count : Integer;
	structapp_count : Integer) : Integer;
var
	i, seg_start, seg_end, seg_len, count : Integer;
	seg_mode : TArrayOfChar;
begin
	if seg_count <= 0 then
	begin
		Result := estimate_binary_length(mode, _length, gs1, version);
		if structapp_count <> 0 then
			Inc(Result, 20);
		Exit;
	end;

	count := 0;
	if structapp_count <> 0 then
		Inc(count, 20);

	if gs1 <> 0 then
		Inc(count, 4);

	seg_start := 0;
	for i := 0 to seg_count - 1 do
	begin
		if (Length(seg_ecis) > i) and (seg_ecis[i] <> 0) then
			Inc(count, 4 + qr_eci_length_bits(seg_ecis[i]));

		seg_end := seg_ends[i];
		if seg_end > _length then
			seg_end := _length;
		if seg_end < seg_start then
			seg_end := seg_start;

		seg_len := seg_end - seg_start;
		if seg_len > 0 then
		begin
			SetLength(seg_mode, seg_len + 1);
			Move(mode[seg_start], seg_mode[0], seg_len * SizeOf(Char));
			seg_mode[seg_len] := #0;
			Inc(count, estimate_binary_length(seg_mode, seg_len, 0, version));
		end;

		seg_start := seg_end;
	end;

	if seg_start < _length then
	begin
		seg_len := _length - seg_start;
		SetLength(seg_mode, seg_len + 1);
		Move(mode[seg_start], seg_mode[0], seg_len * SizeOf(Char));
		seg_mode[seg_len] := #0;
		Inc(count, estimate_binary_length(seg_mode, seg_len, 0, version));
	end;

	Result := count;
end;

procedure qr_binary(var datastream: TArrayOfInteger; version: Integer; target_binlen : Integer; mode : TArrayOfChar;
	jisdata : TArrayOfInteger; _length : Integer; gs1 : Integer; est_binlen : Integer;
	const seg_ends : TArrayOfInteger; const seg_ecis : TArrayOfInteger; seg_count : Integer;
	structapp_count, structapp_index, structapp_id_value : Integer);
var
  position: Integer;
  short_data_block_length, i, scheme : Integer;
  data_block : Char;
  padbits : Integer;
  current_binlen, current_bytes : Integer;
  toggle, percent : Integer;
  binary : TArrayOfChar;
  jis: Integer;
  msb, lsb, prod : Integer;
  _byte : Integer;
  count : Integer;
  first, second, third : Integer;
	seg_idx, next_seg_end : Integer;
	seg_start : Integer;
	seg_eci : Integer;
begin
  // Convert input data to a binary stream and add padding
	position := 0;
  scheme := 1;
  SetLength(binary, est_binlen + 12);
	strcpy(binary, '');

	if structapp_count <> 0 then
	begin
		concat(binary, '0011');
		bscan(binary, structapp_index - 1, $8);
		bscan(binary, structapp_count - 1, $8);
		bscan(binary, structapp_id_value, $80);
	end;

	if (gs1<>0) then
		concat(binary, '0101'); // FNC1

	if(version <= 9) then
		scheme := 1
	else
  if((version >= 10) and (version <= 26)) then
		scheme := 2
	else
  if(version >= 27) then
		scheme := 3;

  {$IFDEF DEBUG_ZINT}
	for i := 0 to _length-1 do
    write(Format('%s', [mode[i]]));

	writeln;
  {$ENDIF}
	percent := 0;
	seg_idx := 0;
	seg_start := 0;
	if seg_count > 0 then
		next_seg_end := seg_ends[0]
	else
		next_seg_end := _length;

	repeat
		if (seg_count > 0) and (position = seg_start) and (seg_idx < Length(seg_ecis)) then
		begin
			seg_eci := seg_ecis[seg_idx];
			if seg_eci <> 0 then
			begin
				concat(binary, '0111');
				if seg_eci <= 127 then
					bscan(binary, seg_eci, $80)
				else if seg_eci <= 16383 then
					bscan(binary, $8000 + seg_eci, $8000)
				else
					bscan(binary, $C00000 + seg_eci, $800000);
			end;
		end;

		data_block := mode[position];
		short_data_block_length := 0;
		repeat
			inc(short_data_block_length);
			if (position + short_data_block_length) >= next_seg_end then
				Break;
    until not (((short_data_block_length + position) < _length) and (mode[position + short_data_block_length] = data_block));

		case (data_block) of
			'K': begin
            // Kanji mode
            // Mode indicator
            concat(binary, '1000');

            // Character count indicator
            bscan(binary, short_data_block_length, $20 shl (scheme*2)); // scheme = 1..3

            {$IFDEF DEBUG_ZINT}writeln(Format('Kanji block (length %d)', [short_data_block_length]));{$ENDIF}

            // Character representation
            for i := 0 to short_data_block_length-1 do
            begin
              jis := jisdata[position + i];

							 if (jis >= $8140) and (jis <= $9ffc) then
								dec(jis, $8140)
							 else if (jis >= $e040) and (jis <= $ebbf) then
								dec(jis, $c140);
              msb := (jis and $ff00) shr 4;
              lsb := (jis and $ff);
              prod := (msb * $c0) + lsb;

              bscan(binary, prod, $1000);

              {$IFDEF DEBUG_ZINT}write(Format('$%4X ', [prod]));{$ENDIF}
            end;

            {$IFDEF DEBUG_ZINT}writeln;{$ENDIF}
  				end;
			'B': begin
              // Byte mode
              // Mode indicator
               concat(binary, '0100');

              // Character count indicator
              if scheme > 1 then
                bscan(binary, short_data_block_length, $8000)
              else
                bscan(binary, short_data_block_length, $80); // scheme = 1

              {$IFDEF DEBUG_ZINT}writeln(Format('Byte block (length %d)', [short_data_block_length]));{$ENDIF}

              // Character representation
              for i := 0 to short_data_block_length - 1 do
              begin
                _byte := jisdata[position + i];

                if (gs1<>0) and (_byte = Ord('[')) then
                  _byte := $1d; // FNC1

                bscan(binary, _byte, $80);

                {$IFDEF DEBUG_ZINT}write(Format('$%2X(%d) ', [_byte, _byte]));{$ENDIF}
              end;

              {$IFDEF DEBUG_ZINT}writeln;{$ENDIF}
           end;
			'A': begin
              // Alphanumeric mode
              // Mode indicator
              concat(binary, '0010');

              // Character count indicator
              bscan(binary, short_data_block_length, $40 shl (2 * scheme)); // scheme = 1..3

              {$IFDEF DEBUG_ZINT}Writeln(Format('Alpha block (length %d)', [short_data_block_length]));{$ENDIF}

              // Character representation
              i := 0;
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
              // Mode indicator
              concat(binary, '0001');

              // Character count indicator
              bscan(binary, short_data_block_length, $80 shl (2 * scheme)); // scheme = 1..3

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
		while (seg_idx < seg_count) and (position >= seg_ends[seg_idx]) do
			Inc(seg_idx);
		if seg_idx < seg_count then
			next_seg_end := seg_ends[seg_idx]
		else
			next_seg_end := _length;
		seg_start := position;
	until not (position < _length) ;

	// Terminator
	concat(binary, '0000');

	current_binlen := strlen(binary);

	padbits := 8 - (current_binlen mod 8);
	if(padbits = 8) then padbits := 0;
	current_bytes := (current_binlen + padbits) div 8;

	// Padding bits
	for i := 0 to padbits-1 do
		concat(binary, '0');

	// Put data into 8-bit codewords
	for i := 0 to current_bytes-1 do
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
	toggle := 0;
	for i := current_bytes to target_binlen - 1 do
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

procedure add_ecc(var fullstream: TArrayOfInteger; datastream: TArrayOfInteger; version : Integer; data_cw : Integer; blocks : Integer);
var
  ecc_cw : Integer;
  short_data_block_length : Integer;
  qty_long_blocks : Integer;
  qty_short_blocks : Integer;
  ecc_block_length : Integer;
  i, j, length_this_block, posn : Integer;
  data_block : TArrayOfByte;
  ecc_block : TArrayOfByte;
  interleaved_data :  TArrayOfInteger;
  interleaved_ecc : TArrayOfInteger;
  RSGlobals : TRSGlobals;
begin
	// Split data into blocks, add error correction and then interleave the blocks and error correction data
	ecc_cw := qr_total_codewords[version - 1] - data_cw;
	short_data_block_length := data_cw div blocks;
	qty_long_blocks := data_cw mod blocks;
	qty_short_blocks := blocks - qty_long_blocks;
	ecc_block_length := ecc_cw div blocks;
	
  SetLength(data_block, short_data_block_length + 2);
	SetLength(ecc_block, ecc_block_length + 2);
	SetLength(interleaved_data, data_cw + 2);
	SetLength(interleaved_ecc, ecc_cw + 2);

	posn := 0;

	for i := 0 to blocks-1 do
  begin
		if(i < qty_short_blocks) then
      length_this_block := short_data_block_length
    else
      length_this_block := short_data_block_length + 1;

		for j := 0 to ecc_block_length-1 do
			ecc_block[j] := 0;

		for j := 0 to length_this_block-1 do
      data_block[j] := datastream[posn + j];

		rs_init_gf($11d, RSGlobals);
		rs_init_code(ecc_block_length, 0, RSGlobals);
		rs_encode(length_this_block, data_block, ecc_block, RSGlobals);
		rs_free(RSGlobals);

    {$IFDEF DEBUG_ZINT}
		write(Format('Block %d: ', [i + 1]));
		for j := 0 to length_this_block-1 do
			write(Format('%2X ', [data_block[j]]));

		if(i < qty_short_blocks) then
			write('   ');

		write(' // ');

		for j := 0 to ecc_block_length-1 do
			write(Format('%2X ', [ecc_block[ecc_block_length - j - 1]]));

		writeln;
    {$ENDIF}

		for j := 0 to short_data_block_length-1 do
    	interleaved_data[(j * blocks) + i] := data_block[j];

		if i >= qty_short_blocks then
			interleaved_data[(short_data_block_length * blocks) + (i - qty_short_blocks)] := data_block[short_data_block_length];

		for j := 0 to ecc_block_length-1 do
			interleaved_ecc[(j * blocks) + i] := ecc_block[j];

		inc(posn, length_this_block);
	end;

	for j := 0 to data_cw - 1 do
		fullstream[j] := interleaved_data[j];

	for j := 0 to ecc_cw - 1 do
		fullstream[j + data_cw] := interleaved_ecc[j];

	{$IFDEF DEBUG_ZINT}
  writeln;
	writeln('Data Stream: ');
	for j := 0 to (data_cw + ecc_cw)-1 do
    write(Format('%2X ', [fullstream[j]]));
	writeln;
  {$ENDIF}
end;

function cwbit(datastream : TArrayOfInteger; i : Integer) : Integer; forward;
procedure place_finder(var grid : TArrayOfByte; size : Integer; x : Integer; y : Integer); forward;

function rmqr_try_shift_jis(const utfdata: TArrayOfInteger; const _length: Integer;
	var jisdata: TArrayOfInteger): Boolean;
var
	i, j, glyph: Integer;
begin
	for i := 0 to _length - 1 do
	begin
		if utfdata[i] <= $FF then
		begin
			if utfdata[i] = $A5 then
				jisdata[i] := $5C
			else if (utfdata[i] <= $7F) and (utfdata[i] <> $7E) then
				jisdata[i] := utfdata[i]
			else
				Exit(False);
		end
		else if (utfdata[i] >= $FF61) and (utfdata[i] <= $FF9F) then
			jisdata[i] := (utfdata[i] - $FF61) + $A1
		else if utfdata[i] = $203E then
			jisdata[i] := $7E
		else
		begin
			j := 0;
			glyph := 0;
			repeat
				if sjis_lookup[j * 2] = utfdata[i] then
					glyph := sjis_lookup[(j * 2) + 1];
				Inc(j);
			until not ((j < 6843) and (glyph = 0));

			if glyph = 0 then
				Exit(False);

			jisdata[i] := glyph;
		end;
	end;

	Result := True;
end;

function rmqr_best_eci(const utfdata: TArrayOfInteger; const _length: Integer;
	var eci_data: TArrayOfInteger): Integer;
var
	eci: Integer;
begin
	for eci := 3 to 24 do
	begin
		if (eci = 14) or (eci = 19) or (eci = 20) then
			Continue;
		if try_single_byte_eci(utfdata, _length, eci, eci_data) then
		begin
			Result := eci;
			Exit;
		end;
	end;

	Result := 26;
end;

function rmqr_block_length(const mode: TArrayOfChar; const start_pos, _length: Integer): Integer;
begin
	Result := 1;
	while (start_pos + Result < _length) and (mode[start_pos + Result] = mode[start_pos]) do
		Inc(Result);
end;

function estimate_binary_length_rmqr(mode : TArrayOfChar; _length : Integer; gs1 : Integer; version_idx : Integer;
	eci : Integer = 0) : Integer; forward;

procedure rmqr_refine_modes(var mode: TArrayOfChar; const jisdata: TArrayOfInteger; const _length, gs1, eci: Integer);
var
	i: Integer;
	best_est, candidate_est: Integer;
	candidate_mode: Char;
	can_numeric, can_alpha, can_byte, can_kanji: Boolean;
	candidate: TArrayOfChar;
begin
	if _length = 0 then
		Exit;

	best_est := estimate_binary_length_rmqr(mode, _length, gs1, 31, eci);
	candidate_mode := #0;
	can_numeric := True;
	can_alpha := True;
	can_byte := True;
	can_kanji := True;

	for i := 0 to _length - 1 do
	begin
		if (jisdata[i] < Ord('0')) or (jisdata[i] > Ord('9')) then
			can_numeric := False;
		if in_alpha(jisdata[i]) = 0 then
			can_alpha := False;
		if jisdata[i] > $FF then
			can_byte := False;
		if jisdata[i] <= $FF then
			can_kanji := False;
	end;

	SetLength(candidate, _length + 1);

	if can_numeric then
	begin
		for i := 0 to _length - 1 do
			candidate[i] := 'N';
		candidate_est := estimate_binary_length_rmqr(candidate, _length, gs1, 31, eci);
		if candidate_est < best_est then
		begin
			best_est := candidate_est;
			candidate_mode := 'N';
		end;
	end;

	if can_alpha then
	begin
		for i := 0 to _length - 1 do
			candidate[i] := 'A';
		candidate_est := estimate_binary_length_rmqr(candidate, _length, gs1, 31, eci);
		if candidate_est < best_est then
		begin
			best_est := candidate_est;
			candidate_mode := 'A';
		end;
	end;

	if can_byte then
	begin
		for i := 0 to _length - 1 do
			candidate[i] := 'B';
		candidate_est := estimate_binary_length_rmqr(candidate, _length, gs1, 31, eci);
		if candidate_est < best_est then
		begin
			best_est := candidate_est;
			candidate_mode := 'B';
		end;
	end;

	if can_kanji then
	begin
		for i := 0 to _length - 1 do
			candidate[i] := 'K';
		candidate_est := estimate_binary_length_rmqr(candidate, _length, gs1, 31, eci);
		if candidate_est < best_est then
		begin
			candidate_mode := 'K';
		end;
	end;

	if candidate_mode <> #0 then
		for i := 0 to _length - 1 do
			mode[i] := candidate_mode;
end;

function estimate_binary_length_rmqr(mode : TArrayOfChar; _length : Integer; gs1 : Integer; version_idx : Integer;
	eci : Integer = 0) : Integer;
var
	i, count, block_length : Integer;
	current  : Char;
	numeric_cci, alpha_cci, byte_cci, kanji_cci: Integer;
begin
	count := 0;
	current := #0;

	numeric_cci := rmqr_numeric_cci[version_idx];
	alpha_cci := rmqr_alphanum_cci[version_idx];
	byte_cci := rmqr_byte_cci[version_idx];
	kanji_cci := rmqr_kanji_cci[version_idx];

	if eci <> 0 then
	begin
		Inc(count, 3);
		if eci <= 127 then
			Inc(count, 8)
		else if eci <= 16383 then
			Inc(count, 16)
		else
			Inc(count, 24);
	end;

	if (gs1<>0) then
		inc(count, 3); // FNC1 mode indicator is 3 bits in rMQR

	i := 0;
	while i < _length do
	begin
		if mode[i] <> current then
		begin
			block_length := rmqr_block_length(mode, i, _length);
			case mode[i] of
				'K': begin inc(count, kanji_cci + 3 + (block_length * 13)); current := 'K'; end;
				'B': begin inc(count, byte_cci + 3 + (block_length * 8)); current := 'B'; end;
				'A': begin
					inc(count, alpha_cci + 3);
					inc(count, (block_length div 2) * 11);
					if (block_length and 1) <> 0 then
						inc(count, 6);
					current := 'A';
				end;
				'N': begin
					inc(count, numeric_cci + 3);
					inc(count, (block_length div 3) * 10);
					case (block_length mod 3) of
						1: inc(count, 4);
						2: inc(count, 7);
					end;
					current := 'N';
				end;
			end;
		end;
		Inc(i);
	end;

	Result := count;
end;

procedure rmqr_binary(var datastream: TArrayOfInteger; version_idx: Integer; target_binlen : Integer; mode : TArrayOfChar; jisdata : TArrayOfInteger; _length : Integer; gs1 : Integer; est_binlen : Integer; eci : Integer);
var
	position: Integer;
	short_data_block_length, i : Integer;
	data_block : Char;
	padbits : Integer;
	current_binlen, current_bytes : Integer;
	toggle, percent : Integer;
	binary : TArrayOfChar;
	jis: Integer;
	msb, lsb, prod : Integer;
	_byte : Integer;
	count : Integer;
	first, second, third : Integer;
begin
	position := 0;
	SetLength(binary, est_binlen + 12);
	strcpy(binary, '');

	if eci <> 0 then
	begin
		bscan(binary, 7, $04);
		if eci <= 127 then
			bscan(binary, eci, $80)
		else if eci <= 16383 then
			bscan(binary, $8000 + eci, $8000)
		else
			bscan(binary, $C00000 + eci, $800000);
	end;

	if (gs1<>0) then
		concat(binary, '101'); // FNC1 mode indicator for rMQR

	percent := 0;

	repeat
		data_block := mode[position];
		short_data_block_length := 0;
		repeat
			inc(short_data_block_length);
		until not (((short_data_block_length + position) < _length) and (mode[position + short_data_block_length] = data_block));

		case (data_block) of
			'K': begin
						 concat(binary, '100');
						 bscan(binary, short_data_block_length, (1 shl rmqr_kanji_cci[version_idx]) shr 1);
						 for i := 0 to short_data_block_length - 1 do
						 begin
							 jis := jisdata[position + i];

							 if (jis >= $8140) and (jis <= $9ffc) then
								dec(jis, $8140)
							 else if (jis >= $e040) and (jis <= $ebbf) then
								dec(jis, $c140);
							 msb := (jis and $ff00) shr 8;
							 lsb := (jis and $ff);
							 prod := (msb * $c0) + lsb;

							 bscan(binary, prod, $1000);
						 end;
					 end;
			'B': begin
						 concat(binary, '011');
						 bscan(binary, short_data_block_length, (1 shl rmqr_byte_cci[version_idx]) shr 1);

						 for i := 0 to short_data_block_length - 1 do
						 begin
							 _byte := jisdata[position + i];

							 if (gs1<>0) and (_byte = Ord('[')) then
								 _byte := $1d;

							 bscan(binary, _byte, $80);
						 end;
					 end;
			'A': begin
						 concat(binary, '010');
						 bscan(binary, short_data_block_length, (1 shl rmqr_alphanum_cci[version_idx]) shr 1);

						 i := 0;
						 while ( i < short_data_block_length ) do
						 begin
							 if(percent = 0) then
							 begin
								 if(gs1<>0) and (jisdata[position + i] = Ord('%')) then
								 begin
									 first := posn(RHODIUM, '%');
									 second := posn(RHODIUM, '%');
									 count := 2;
									 prod := (first * 45) + second;
									 inc(i);
								 end
								 else
								 begin
									 if(gs1<>0) and (jisdata[position + i] = Ord('[')) then
										 first := posn(RHODIUM, '%')
									 else
										 first := posn(RHODIUM, Chr(jisdata[position + i]));

									 count := 1;
									 inc(i);
									 prod := first;

									 if i < short_data_block_length then
									 begin
										 if (gs1<>0) and (jisdata[position + i] = Ord('%')) then
										 begin
											 second := posn(RHODIUM, '%');
											 count := 2;
											 prod := (first * 45) + second;
											 percent := 1;
										 end
										 else
										 begin
											 if(gs1<>0) and (jisdata[position + i] = Ord('[')) then
												 second := posn(RHODIUM, '%')
											 else
												 second := posn(RHODIUM, Chr(jisdata[position + i]));

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

								 if i < short_data_block_length then
								 begin
									 if((gs1<>0) and (jisdata[position + i] = Ord('%'))) then
									 begin
										 second := posn(RHODIUM, '%');
										 count := 2;
										 prod := (first * 45) + second;
										 percent := 1;
									 end
									 else
									 begin
										 if(gs1<>0) and (jisdata[position + i] = Ord('[')) then
											 second := posn(RHODIUM, '%')
										 else
											 second := posn(RHODIUM, Chr(jisdata[position + i]));

										 count := 2;
										 inc(i);
										 prod := (first * 45) + second;
									 end;
								 end;
							 end;

							 if count = 2 then
								 bscan(binary, prod, $400)
							 else
								 bscan(binary, prod, $20);
						 end;
					 end;
			'N': begin
						 concat(binary, '001');
						 bscan(binary, short_data_block_length, (1 shl rmqr_numeric_cci[version_idx]) shr 1);

						 i := 0;
						 while ( i < short_data_block_length ) do
						 begin
							 first := posn(NEON, Chr(jisdata[position + i]));
							 count := 1;
							 prod := first;

							 if (i + 1 < short_data_block_length) then
							 begin
								 second := posn(NEON, Chr(jisdata[position + i + 1]));
								 count := 2;
								 prod := (prod * 10) + second;

								 if (i + 2 < short_data_block_length) then
								 begin
									 third := posn(NEON, Chr(jisdata[position + i + 2]));
									 count := 3;
									 prod := (prod * 10) + third;
								 end;
							 end;

							 bscan(binary, prod, 1 shl (3 * count));
							 inc(i, count);
						 end;
					 end;
		end;

		inc(position, short_data_block_length);
	until not (position < _length);

	concat(binary, '000'); // rMQR terminator is 3 bits

	current_binlen := strlen(binary);
	padbits := 8 - (current_binlen mod 8);
	if(padbits = 8) then
		padbits := 0;
	current_bytes := (current_binlen + padbits) div 8;

	for i := 0 to padbits - 1 do
		concat(binary, '0');

	for i := 0 to current_bytes - 1 do
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

	toggle := 0;
	for i := current_bytes to target_binlen - 1 do
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
end;

procedure add_ecc_rmqr(var fullstream: TArrayOfInteger; datastream: TArrayOfInteger; version_idx : Integer; data_cw : Integer; blocks : Integer);
var
	ecc_cw : Integer;
	short_data_block_length : Integer;
	qty_long_blocks : Integer;
	qty_short_blocks : Integer;
	ecc_block_length : Integer;
	i, j, length_this_block, posn : Integer;
	data_block : TArrayOfByte;
	ecc_block : TArrayOfByte;
	interleaved_data :  TArrayOfInteger;
	interleaved_ecc : TArrayOfInteger;
	RSGlobals : TRSGlobals;
begin
	ecc_cw := rmqr_total_codewords[version_idx] - data_cw;
	short_data_block_length := data_cw div blocks;
	qty_long_blocks := data_cw mod blocks;
	qty_short_blocks := blocks - qty_long_blocks;
	ecc_block_length := ecc_cw div blocks;

	SetLength(data_block, short_data_block_length + 2);
	SetLength(ecc_block, ecc_block_length + 2);
	SetLength(interleaved_data, data_cw + 2);
	SetLength(interleaved_ecc, ecc_cw + 2);

	posn := 0;
	for i := 0 to blocks - 1 do
	begin
		if(i < qty_short_blocks) then
			length_this_block := short_data_block_length
		else
			length_this_block := short_data_block_length + 1;

		for j := 0 to ecc_block_length - 1 do
			ecc_block[j] := 0;

		for j := 0 to length_this_block - 1 do
			data_block[j] := datastream[posn + j];

		rs_init_gf($11d, RSGlobals);
		rs_init_code(ecc_block_length, 0, RSGlobals);
		rs_encode(length_this_block, data_block, ecc_block, RSGlobals);
		rs_free(RSGlobals);

		for j := 0 to short_data_block_length - 1 do
			interleaved_data[(j * blocks) + i] := data_block[j];

		if i >= qty_short_blocks then
			interleaved_data[(short_data_block_length * blocks) + (i - qty_short_blocks)] := data_block[short_data_block_length];

		for j := 0 to ecc_block_length - 1 do
			interleaved_ecc[(j * blocks) + i] := ecc_block[ecc_block_length - j - 1];

		inc(posn, length_this_block);
	end;

	for j := 0 to data_cw - 1 do
		fullstream[j] := interleaved_data[j];

	for j := 0 to ecc_cw - 1 do
		fullstream[j + data_cw] := interleaved_ecc[j];
end;

procedure populate_grid_rmqr(var grid : TArrayOfByte; h_size : Integer; v_size : Integer; datastream : TArrayOfInteger; cw : Integer);
var
	direction : Integer;
	row : Integer;
	i, n, x, y, r, x_start : Integer;
begin
	direction := 1;
	row := 0;
	x_start := h_size - 3;

	n := cw * 8;
	y := v_size - 1;
	i := 0;
	while i < n do
	begin
		x := x_start - (row * 2);
		r := y * h_size;

		if not ((grid[r + (x + 1)] and $f0) <> 0) then
		begin
			grid[r + (x + 1)] := cwbit(datastream, i);
			inc(i);
		end;

		if i < n then
		begin
			if not ((grid[r + x] and $f0) <> 0) then
			begin
				grid[r + x] := cwbit(datastream, i);
				inc(i);
			end;
		end;

		if direction <> 0 then
		begin
			dec(y);
			if y = -1 then
			begin
				inc(row);
				y := 0;
				direction := 0;
			end;
		end
		else
		begin
			inc(y);
			if y = v_size then
			begin
				inc(row);
				y := v_size - 1;
				direction := 1;
			end;
		end;
	end;
end;

procedure rmqr_setup_grid(var grid : TArrayOfByte; h_size : Integer; v_size : Integer);
const
	alignment: array[0..4] of Byte = ($1F, $11, $15, $11, $1F);
var
	i, j, h_version, finder_position: Integer;
begin
	for i := 0 to h_size - 1 do
	begin
		grid[i] := $20 + Ord((i and 1) = 0);
		grid[((v_size - 1) * h_size) + i] := $20 + Ord((i and 1) = 0);
	end;

	for i := 0 to v_size - 1 do
	begin
		grid[i * h_size] := $20 + Ord((i and 1) = 0);
		grid[(i * h_size) + (h_size - 1)] := $20 + Ord((i and 1) = 0);
	end;

	place_finder(grid, h_size, 0, 0);

	for i := 0 to 4 do
		for j := 0 to 4 do
			grid[((i + v_size - 5) * h_size) + (h_size - 5) + j] := $10 + Ord((alignment[j] and (Byte($10) shr i)) <> 0);

	grid[(v_size - 2) * h_size] := $11;
	grid[((v_size - 2) * h_size) + 1] := $10;
	grid[((v_size - 1) * h_size) + 1] := $11;

	grid[h_size - 2] := $11;
	grid[(h_size * 2) - 2] := $10;
	grid[(h_size * 2) - 1] := $11;

	for i := 0 to 6 do
		grid[(i * h_size) + 7] := $20;

	if v_size > 7 then
		for i := 0 to 7 do
			grid[(7 * h_size) + i] := $20;

	if h_size > 27 then
	begin
		h_version := 0;
		for i := 0 to 4 do
			if h_size = rmqr_width[i] then
			begin
				h_version := i;
				Break;
			end;

		for i := 0 to 3 do
		begin
			finder_position := rmqr_table_d1[(h_version * 4) + i];
			if finder_position <> 0 then
			begin
				for j := 0 to v_size - 1 do
					grid[(j * h_size) + finder_position] := $10 + Ord((j and 1) = 0);

				grid[h_size + finder_position - 1] := $11;
				grid[(h_size * 2) + finder_position - 1] := $11;
				grid[h_size + finder_position + 1] := $11;
				grid[(h_size * 2) + finder_position + 1] := $11;

				grid[(h_size * (v_size - 3)) + finder_position - 1] := $11;
				grid[(h_size * (v_size - 2)) + finder_position - 1] := $11;
				grid[(h_size * (v_size - 3)) + finder_position + 1] := $11;
				grid[(h_size * (v_size - 2)) + finder_position + 1] := $11;
			end;
		end;
	end;

	for i := 0 to 4 do
		for j := 0 to 2 do
		begin
			grid[(h_size * (i + 1)) + j + 8] := $20;
			grid[(h_size * (i + v_size - 6)) + j + (h_size - 8)] := $20;
		end;

	grid[(h_size * 1) + 11] := $20;
	grid[(h_size * 2) + 11] := $20;
	grid[(h_size * 3) + 11] := $20;
	grid[(h_size * (v_size - 6)) + (h_size - 5)] := $20;
	grid[(h_size * (v_size - 6)) + (h_size - 4)] := $20;
	grid[(h_size * (v_size - 6)) + (h_size - 3)] := $20;
end;

procedure rmqr_add_format_info(var grid : TArrayOfByte; h_size : Integer; v_size : Integer; version_idx : Integer; ecc_level : Integer);
var
	i, j, format_data: Integer;
	left_format_info, right_format_info: Cardinal;
begin
	format_data := version_idx;
	if ecc_level = LEVEL_H then
		inc(format_data, 32);

	left_format_info := rmqr_format_info_left[format_data];
	right_format_info := rmqr_format_info_right[format_data];

	for i := 0 to 4 do
		for j := 0 to 2 do
		begin
			grid[(h_size * (i + 1)) + j + 8] := (left_format_info shr ((j * 5) + i)) and $01;
			grid[(h_size * (i + v_size - 6)) + j + (h_size - 8)] := (right_format_info shr ((j * 5) + i)) and $01;
		end;

	grid[(h_size * 1) + 11] := (left_format_info shr 15) and $01;
	grid[(h_size * 2) + 11] := (left_format_info shr 16) and $01;
	grid[(h_size * 3) + 11] := (left_format_info shr 17) and $01;
	grid[(h_size * (v_size - 6)) + (h_size - 5)] := (right_format_info shr 15) and $01;
	grid[(h_size * (v_size - 6)) + (h_size - 4)] := (right_format_info shr 16) and $01;
	grid[(h_size * (v_size - 6)) + (h_size - 3)] := (right_format_info shr 17) and $01;
end;

procedure place_finder(var grid : TArrayOfByte; size : Integer; x : Integer; y : Integer);
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
var
  xp, yp : Integer;
begin

	for xp := 0 to 6 do
  begin
		for yp := 0 to 6 do
    begin
			if (finder[xp + (7 * yp)] = 1) then
      	grid[((yp + y) * size) + (xp + x)] := $11
      else
				grid[((yp + y) * size) + (xp + x)] := $10;
    end
  end;
end;

procedure place_align(var grid : TArrayOfByte; size : Integer; x : Integer; y : Integer);
const
  alignment : array [0..24] of byte = (
		1, 1, 1, 1, 1,
		1, 0, 0, 0, 1,
		1, 0, 1, 0, 1,
		1, 0, 0, 0, 1,
		1, 1, 1, 1, 1
	);
var
  xp, yp : Integer;
begin

	dec(x, 2);
	dec(y, 2); // Input values represent centre of pattern

	for xp := 0 to 4 do
  begin
		for yp := 0 to 4 do
    begin
			if (alignment[xp + (5 * yp)] = 1) then
				grid[((yp + y) * size) + (xp + x)] := $11
			else
				grid[((yp + y) * size) + (xp + x)] := $10;
    end;
  end;
end;

procedure setup_grid(var grid : TArrayOfByte; size : Integer; version : Integer);
var
  i : Integer;
  toggle : Integer;
  loopsize, x, y, xcoord, ycoord : Integer;
begin
	toggle := 1;

	//Add timing patterns
	for i := 0 to size-1 do
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
	for i := 0 to 6 do
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
		for x := 0 to loopsize - 1 do
    begin
			for y := 0 to loopsize - 1 do
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
	for i := 0 to 7 do
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
		for i := 0 to 5 do
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

function cwbit(datastream : TArrayOfInteger; i : Integer) : Integer;
var
  _word, _bit : Integer;
begin
	_word := i shr 3;
	_bit := 7 - (i and 7);

	Result:=(datastream[_word] shr _bit) and 1;
end;

procedure populate_grid(var grid : TArrayOfByte; size : Integer; datastream : TArrayOfInteger; cw : Integer);
var
  direction : Integer;
  row : Integer;
  i, n, x, y : Integer;
begin
	direction := 1; // up
	row := 0; // right hand side

	n := cw * 8;
	y := size - 1;
	i := 0;
	repeat
		x := (size - 2) - (row * 2);
		if(x < 6) then
			dec(x); // skip over vertical timing pattern

		if not ((grid[(y * size) + (x + 1)] and $f0) <> 0) then
    begin
			if (cwbit(datastream, i)<>0) then
      	grid[(y * size) + (x + 1)] := $01
			else
				grid[(y * size) + (x + 1)] := $00;

			inc(i);
		end;

		if(i < n) then
    begin
			if not ((grid[(y * size) + x] and $f0) <> 0) then
      begin
				if (cwbit(datastream, i)<>0) then
					grid[(y * size) + x] := $01
				else
					grid[(y * size) + x] := $00;

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
		if(y = size) then
    begin
			// reached the bottom
			inc(row);
			y := size - 1;
			direction := 1;
		end;
	until not (i < n);
end;

function evaluate(var grid : TArrayOfByte; size : Integer; pattern : Integer) : Integer;
var
	x, y, r, k, block : Integer;
  _result : Integer;
	state : Byte;
  dark_mods : Integer;
	percentage : Double;
	beforeCount, afterCount, a, b : Integer;
	local : TArrayOfByte;
begin
	_result := 0;

	SetLength(local, size * size);

	for x := 0 to size - 1 do
  begin
		for y := 0 to size - 1 do
    begin
			case pattern of
				0: if (grid[(y * size) + x] and $01)<>0 then local[(y * size) + x] := 1 else local[(y * size) + x] := 0;
				1: if (grid[(y * size) + x] and $02)<>0 then local[(y * size) + x] := 1 else local[(y * size) + x] := 0;
				2: if (grid[(y * size) + x] and $04)<>0 then local[(y * size) + x] := 1 else local[(y * size) + x] := 0;
				3: if (grid[(y * size) + x] and $08)<>0 then local[(y * size) + x] := 1 else local[(y * size) + x] := 0;
				4: if (grid[(y * size) + x] and $10)<>0 then local[(y * size) + x] := 1 else local[(y * size) + x] := 0;
				5: if (grid[(y * size) + x] and $20)<>0 then local[(y * size) + x] := 1 else local[(y * size) + x] := 0;
				6: if (grid[(y * size) + x] and $40)<>0 then local[(y * size) + x] := 1 else local[(y * size) + x] := 0;
				7: if (grid[(y * size) + x] and $80)<>0 then local[(y * size) + x] := 1 else local[(y * size) + x] := 0;
			end;
		end;
	end;

	// Test 1: Adjacent modules in row/column in same colour
	// Vertical
	for x := 0 to size - 1 do
  begin
		state := 0;
		block := 0;
		for y := 0 to size - 1 do
    begin
			if (local[(y * size) + x] = state) then
        inc(block)
			else
      begin
				if(block >= 5) then
					inc(_result, block - 2);

				block := 1;
				state := local[(y * size) + x];
			end;
		end;
		if(block >= 5) then
			inc(_result, block - 2);
	end;

	// Horizontal
	dark_mods := 0;
	for y := 0 to size - 1 do
  begin
		r := y * size;
		state := 0;
		block := 0;
		for x := 0 to size - 1 do
    begin
			if(local[r + x] = state) then
				inc(block)
      else
      begin
				if(block >= 5) then
					inc(_result, block - 2);

				block := 1;
				state := local[r + x];
			end;
			if state <> 0 then
				inc(dark_mods);
		end;
		if(block >= 5) then
			inc(_result, block - 2);
	end;

	// Test 2: Block of modules in same color
	for x := 0 to size - 2 do
	begin
		for y := 0 to size - 2 do
		begin
			k := local[(y * size) + x];
			if (k = local[((y + 1) * size) + x]) and (k = local[(y * size) + (x + 1)])
				and (k = local[((y + 1) * size) + (x + 1)]) then
				Inc(_result, 3);
		end;
	end;

	// Test 3: 1:1:3:1:1 ratio pattern in row/column
	// Vertical
	for x := 0 to size - 1 do
  begin
		y := 0;
		while y <= (size - 7) do
    begin
			if (local[(y * size) + x] <> 0) and (local[((y + 1) * size) + x] = 0)
				and (local[((y + 2) * size) + x] <> 0) and (local[((y + 3) * size) + x] <> 0)
				and (local[((y + 4) * size) + x] <> 0) and (local[((y + 5) * size) + x] = 0)
				and (local[((y + 6) * size) + x] <> 0) then
			begin
				beforeCount := 0;
				for b := y - 1 downto y - 4 do
				begin
					if b < 0 then
					begin
						beforeCount := 4;
						Break;
					end;
					if local[(b * size) + x] <> 0 then
						Break;
					Inc(beforeCount);
				end;
				if beforeCount = 4 then
					Inc(_result, 40)
				else
				begin
					afterCount := 0;
					for a := y + 7 to y + 10 do
					begin
						if a >= size then
						begin
							afterCount := 4;
							Break;
						end;
						if local[(a * size) + x] <> 0 then
							Break;
						Inc(afterCount);
					end;
					if afterCount = 4 then
						Inc(_result, 40);
				end;
				Inc(y, 4);
			end
			else
				Inc(y);
		end;
	end;

	// Horizontal
	for y := 0 to size - 1 do
  begin
		x := 0;
		r := y * size;
		while x <= (size - 7) do
    begin
			if (local[r + x] <> 0) and (local[r + x + 1] = 0)
				and (local[r + x + 2] <> 0) and (local[r + x + 3] <> 0)
				and (local[r + x + 4] <> 0) and (local[r + x + 5] = 0)
				and (local[r + x + 6] <> 0) then
			begin
				beforeCount := 0;
				for b := x - 1 downto x - 4 do
				begin
					if b < 0 then
					begin
						beforeCount := 4;
						Break;
					end;
					if local[r + b] <> 0 then
						Break;
					Inc(beforeCount);
				end;
				if beforeCount = 4 then
					Inc(_result, 40)
				else
				begin
					afterCount := 0;
					for a := x + 7 to x + 10 do
					begin
						if a >= size then
						begin
							afterCount := 4;
							Break;
						end;
						if local[r + a] <> 0 then
							Break;
						Inc(afterCount);
					end;
					if afterCount = 4 then
						Inc(_result, 40);
				end;
				Inc(x, 4);
			end
			else
				Inc(x);
		end;
	end;

	// Test 4: Proportion of dark modules in entire symbol
	percentage := (100.0 * dark_mods) / (size * size);
	k := Trunc(Abs(percentage - 50.0) / 5.0);

	inc(_result, 10 * k);

	Result:= _result;
end;

function apply_bitmask(var grid : TArrayOfByte; size : Integer) : Integer;
var
  x, y : Integer;
  p : Byte;
  pattern : Integer;
  penalty : TArrayOfInteger;
  best_val, best_pattern : Integer;
  bit : Integer;
  mask, eval : TArrayOfByte;
begin
  SetLength(penalty, 8);;
  SetLength(mask, size * size);
	SetLength(eval, size * size);

	// Perform data masking
	for x := 0 to size-1 do
  begin
		for y := 0 to size - 1 do
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

	for x := 0 to size - 1 do
  begin
		for y := 0 to size - 1 do
    begin
			if(grid[(y * size) + x] and $01)<>0 then p := $ff else p := $00;

			eval[(y * size) + x] := mask[(y * size) + x] xor p;
		end;
	end;


	// Evaluate result
	for pattern := 0 to 7 do
  	penalty[pattern] := evaluate(eval, size, pattern);

	best_pattern := 0;
	best_val := penalty[0];
	for pattern := 1 to 7 do
  begin
		if(penalty[pattern] < best_val) then
    begin
			best_pattern := pattern;
			best_val := penalty[pattern];
		end;
	end;

	// Apply mask
	for x := 0 to size - 1 do
  begin
		for y := 0 to size - 1 do
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

procedure add_format_info(var grid : TArrayOfByte; size : Integer; ecc_level: Integer; pattern : Integer);
var
  format : Integer;
  seq : Cardinal;
  i : Integer;
begin
	// Add format information to grid
	format := pattern;

	case(ecc_level) of
		LEVEL_L: inc(format, $08);
		LEVEL_Q: inc(format, $18);
		LEVEL_H: inc(format, $10);
	end;

	seq := qr_annex_c[format];

	for i := 0 to 5 do
		inc(grid[(i * size) + 8], (seq shr i) and $01);

	for i := 0 to 7 do
		inc(grid[(8 * size) + (size - i - 1)], (seq shr i) and $01);

	for i := 0 to 5 do
  	inc(grid[(8 * size) + (5 - i)], (seq shr (i + 9)) and $01);

	for i := 0 to 6 do
		inc(grid[(((size - 7) + i) * size) + 8], (seq shr (i + 8)) and $01);

	inc(grid[(7 * size) + 8], (seq shr 6) and $01);
	inc(grid[(8 * size) + 8], (seq shr 7) and $01);
	inc(grid[(8 * size) + 7], (seq shr 8) and $01);
end;

procedure add_version_info(var grid : TArrayOfByte; size : Integer; version: Integer);
var
  i : Integer;
  version_data : Integer;
begin
	// Add version information

	version_data := qr_annex_d[version - 7];
	for i := 0 to 5 do
  begin
		inc(grid[((size - 11) * size) + i], (version_data shr (i * 3)) and $01);
		inc(grid[((size - 10) * size) + i], (version_data shr ((i * 3) + 1)) and $01);
		inc(grid[((size - 9) * size) + i], (version_data shr ((i * 3) + 2)) and $01);
		inc(grid[(i * size) + (size - 11)], (version_data shr (i * 3)) and$01);
		inc(grid[(i * size) + (size - 10)], (version_data shr ((i * 3) + 1)) and $01);
		inc(grid[(i * size) + (size - 9)], (version_data shr ((i * 3) + 2)) and $01);
	end;
end;

function qr_code_with_seg_ends(symbol : zint_symbol; source : TArrayOfByte; _length : Integer;
	const seg_ends : TArrayOfInteger; const seg_ecis : TArrayOfInteger; seg_count : Integer): Integer;
var
	i, j : Integer;
	error_number, glyph, est_binlen, prev_est_binlen : Integer;
	required_cw, max_cw_version : Integer;
	ecc_name : Char;
	ecc_level, autosize, version, max_cw, target_binlen, blocks, size : Integer;
	canShrink: Integer;
  bitmask, gs1 : Integer;
	source_length, auto_eci_warning, auto_eci_fallback, auto_eci_mode : Integer;
	structapp_id_len, structapp_id_value : Integer;
	structapp_ch : Char;
  utfdata : TArrayOfInteger;
  jisdata : TArrayOfInteger;
  mode : TArrayOfChar;
  datastream, fullstream : TArrayOfInteger;
  grid : TArrayOfByte;
begin
	SetLength(utfdata, _length + 1);
	SetLength(jisdata, _length + 1);
	SetLength(mode, _length + 1);
	source_length := _length;
	auto_eci_warning := 0;

  if symbol.input_mode = GS1_MODE then
  	gs1 := 1
  else
    gs1 := 0;

	case(symbol.input_mode) of
		DATA_MODE: begin
			for i := 0 to _length-1 do
      	jisdata[i] := source[i];
			end;
    else
			if symbol.eci = 26 then
			begin
				for i := 0 to _length - 1 do
					jisdata[i] := source[i];
			end
			else
		  // Convert Unicode input to Shift-JIS
			error_number := utf8toutf16(symbol, source, utfdata, _length);
			if (error_number <> 0) then begin
        Result:=error_number;
        exit;
      end;

			if symbol.eci = 0 then
			begin
				symbol.eci := 20;
				auto_eci_mode := 1;
			end
			else
				auto_eci_mode := 0;

			auto_eci_fallback := 0;

			for  i := 0 to _length-1 do
      begin
				if (symbol.eci = 3) and (utfdata[i] > $FF) then
          begin
					strcpy(symbol.errtxt, 'Error 575: Invalid character in input for ECI ''3''');
					Result := ZERROR_INVALID_DATA;
            Exit;
          end;

				if(utfdata[i] <= $ff) then
				begin
					if symbol.eci = 20 then
					begin
						if utfdata[i] = $A5 then
							jisdata[i] := $5C
						else if (utfdata[i] <= $7F) and (utfdata[i] <> $7E) then
							jisdata[i] := utfdata[i]
						else
              begin
							if (symbol.eci = 20) and (auto_eci_mode <> 0) then
							begin
								auto_eci_fallback := 1;
								Break;
							end;
							strcpy(symbol.errtxt, 'Error 800: Invalid character in input');
							Result := ZERROR_INVALID_DATA;
                Exit;
              end;
					end
            else
						jisdata[i] := utfdata[i];
				end
        else if (symbol.eci = 20) and (utfdata[i] >= $FF61) and (utfdata[i] <= $FF9F) then
          jisdata[i] := (utfdata[i] - $FF61) + $A1
				else if (symbol.eci = 20) and (utfdata[i] = $203E) then
					jisdata[i] := $7E
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
							if (symbol.eci = 20) and (auto_eci_mode <> 0) then
							begin
								auto_eci_fallback := 1;
								Break;
							end;
							if symbol.eci = 3 then
								strcpy(symbol.errtxt, 'Error 575: Invalid character in input for ECI ''3''')
							else
								strcpy(symbol.errtxt, 'Error 800: Invalid character in input');
						Result:=ZERROR_INVALID_DATA;
            Exit;
					end;
					jisdata[i] := glyph;
				end;
			end;

			if auto_eci_fallback <> 0 then
			begin
				symbol.eci := 26;
				auto_eci_warning := ZWARN_USES_ECI;
				_length := source_length;
				for i := 0 to _length - 1 do
					jisdata[i] := source[i];
			end;
  end;

	define_mode(mode, jisdata, _length, gs1);

	if symbol.structapp.count <> 0 then
	begin
		if (symbol.structapp.count < 2) or (symbol.structapp.count > 16) then
		begin
			strcpy(symbol.errtxt, PChar(Format('Error 750: Structured Append count ''%d'' out of range (2 to 16)',
				[symbol.structapp.count])));
			Result := ZERROR_INVALID_OPTION;
			Exit;
		end;
		if (symbol.structapp.index < 1) or (symbol.structapp.index > symbol.structapp.count) then
		begin
			strcpy(symbol.errtxt, PChar(Format('Error 751: Structured Append index ''%d'' out of range (1 to count %d)',
				[symbol.structapp.index, symbol.structapp.count])));
			Result := ZERROR_INVALID_OPTION;
			Exit;
		end;

		structapp_id_len := Length(symbol.structapp.id);
		if structapp_id_len > 3 then
		begin
			strcpy(symbol.errtxt, PChar(Format('Error 752: Structured Append ID length %d too long (3 digit maximum)',
				[structapp_id_len])));
			Result := ZERROR_INVALID_OPTION;
			Exit;
		end;

		structapp_id_value := 0;
		for i := 1 to structapp_id_len do
		begin
			structapp_ch := symbol.structapp.id[i];
			if (structapp_ch < '0') or (structapp_ch > '9') then
			begin
				strcpy(symbol.errtxt, 'Error 753: Invalid Structured Append ID (digits only)');
				Result := ZERROR_INVALID_OPTION;
				Exit;
			end;
			structapp_id_value := (structapp_id_value * 10) + Ord(structapp_ch) - Ord('0');
		end;
		if structapp_id_value > 255 then
		begin
			strcpy(symbol.errtxt, PChar(Format('Error 754: Structured Append ID value ''%d'' out of range (0 to 255)',
				[structapp_id_value])));
			Result := ZERROR_INVALID_OPTION;
			Exit;
		end;

	end
	else
		structapp_id_value := 0;

	// GS1 QR does not support ECI or Structured Append; ECI warning takes precedence.
	if (gs1 <> 0) and (auto_eci_warning = 0) then
	begin
		if symbol.eci <> 0 then
		begin
			auto_eci_warning := ZWARN_NONCOMPLIANT;
			strcpy(symbol.errtxt, 'Warning 755: Using ECI in GS1 mode not supported by GS1 standards');
		end
		else
		begin
			for i := 0 to seg_count - 1 do
			begin
				if seg_ecis[i] <> 0 then
				begin
					auto_eci_warning := ZWARN_NONCOMPLIANT;
					strcpy(symbol.errtxt, 'Warning 755: Using ECI in GS1 mode not supported by GS1 standards');
					Break;
				end;
			end;
		end;

		if (auto_eci_warning = 0) and (symbol.structapp.count <> 0) then
		begin
			auto_eci_warning := ZWARN_NONCOMPLIANT;
			strcpy(symbol.errtxt, 'Warning 756: Using Structured Append in GS1 mode not supported by GS1 standards');
		end;
	end;

	est_binlen := estimate_binary_length_segs(mode, _length, gs1, 40, seg_ends, seg_ecis, seg_count,
		symbol.structapp.count);

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
		required_cw := (est_binlen + 7) div 8;
		if (ecc_level = LEVEL_L) then
			strcpy(symbol.errtxt, PChar(Format('Error 567: Input too long, requires %d codewords (maximum %d)',
				[required_cw, max_cw])))
		else
		begin
			case ecc_level of
				LEVEL_M: ecc_name := 'M';
				LEVEL_Q: ecc_name := 'Q';
			else
				ecc_name := 'H';
			end;
			strcpy(symbol.errtxt, PChar(Format('Error 561: Input too long for ECC level %s, requires %d codewords (maximum %d)',
				[String(ecc_name), required_cw, max_cw])));
		end;
		Result:=ZERROR_TOO_LONG;
    Exit;
	end;

	autosize := 40;
	for i := 39 downto 0 do
  begin
		case(ecc_level) of
			LEVEL_L:
				if ((8 * qr_data_codewords_L[i]) >= est_binlen) then
					autosize := i + 1;
			LEVEL_M:
				if ((8 * qr_data_codewords_M[i]) >= est_binlen) then
					autosize := i + 1;
			LEVEL_Q:
				if ((8 * qr_data_codewords_Q[i]) >= est_binlen) then
					autosize := i + 1;
			LEVEL_H:
				if ((8 * qr_data_codewords_H[i]) >= est_binlen) then
					autosize := i + 1;
		end;
	end;

	if autosize <> 40 then
		est_binlen := estimate_binary_length_segs(mode, _length, gs1, autosize, seg_ends, seg_ecis, seg_count,
			symbol.structapp.count);

	canShrink := 1;
	while canShrink <> 0 do
	begin
		if autosize = 1 then
			canShrink := 0
		else
		begin
			prev_est_binlen := est_binlen;
			est_binlen := estimate_binary_length_segs(mode, _length, gs1, autosize - 1, seg_ends, seg_ecis, seg_count,
				symbol.structapp.count);
			case(ecc_level) of
				LEVEL_L: if (8 * qr_data_codewords_L[autosize - 2]) < est_binlen then canShrink := 0;
				LEVEL_M: if (8 * qr_data_codewords_M[autosize - 2]) < est_binlen then canShrink := 0;
				LEVEL_Q: if (8 * qr_data_codewords_Q[autosize - 2]) < est_binlen then canShrink := 0;
				LEVEL_H: if (8 * qr_data_codewords_H[autosize - 2]) < est_binlen then canShrink := 0;
			end;
			if canShrink <> 0 then
				Dec(autosize)
			else
				est_binlen := prev_est_binlen;
		end;
	end;

	if((symbol.option_2 >= 1) and (symbol.option_2 <= 40)) then
	begin
		if (symbol.option_2 > autosize) then
		begin
			version := symbol.option_2;
			est_binlen := estimate_binary_length_segs(mode, _length, gs1, version, seg_ends, seg_ecis, seg_count,
				symbol.structapp.count);
		end
		else if (symbol.option_2 < autosize) then
		begin
			required_cw := (est_binlen + 7) div 8;
			case ecc_level of
				LEVEL_L: begin ecc_name := 'L'; max_cw_version := qr_data_codewords_L[symbol.option_2 - 1]; end;
				LEVEL_M: begin ecc_name := 'M'; max_cw_version := qr_data_codewords_M[symbol.option_2 - 1]; end;
				LEVEL_Q: begin ecc_name := 'Q'; max_cw_version := qr_data_codewords_Q[symbol.option_2 - 1]; end;
			else
				begin ecc_name := 'H'; max_cw_version := qr_data_codewords_H[symbol.option_2 - 1]; end;
			end;
			strcpy(symbol.errtxt, PChar(Format('Error 569: Input too long for Version %d-%s, requires %d codewords (maximum %d)',
				[symbol.option_2, String(ecc_name), required_cw, max_cw_version])));
			Result := ZERROR_TOO_LONG;
			Exit;
		end
		else
			version := autosize;
	end
	else
		version := autosize;

	// Ensure maximum error correction capacity unless user-specified
	if not ((symbol.option_1 >= 1) and (symbol.option_1 <= 4)) then
	begin
		if(est_binlen <= (8 * qr_data_codewords_M[version - 1])) then ecc_level := LEVEL_M;
		if(est_binlen <= (8 * qr_data_codewords_Q[version - 1])) then ecc_level := LEVEL_Q;
		if(est_binlen <= (8 * qr_data_codewords_H[version - 1])) then ecc_level := LEVEL_H;
	end;

	target_binlen := qr_data_codewords_L[version - 1]; blocks := qr_blocks_L[version - 1];
	case(ecc_level) of
		LEVEL_M: begin target_binlen := qr_data_codewords_M[version - 1]; blocks := qr_blocks_M[version - 1]; end;
		LEVEL_Q: begin target_binlen := qr_data_codewords_Q[version - 1]; blocks := qr_blocks_Q[version - 1]; end;
		LEVEL_H: begin target_binlen := qr_data_codewords_H[version - 1]; blocks := qr_blocks_H[version - 1]; end;
	end;

	SetLength(datastream, target_binlen + 1);
	SetLength(fullstream, qr_total_codewords[version - 1] + 1);

	qr_binary(datastream, version, target_binlen, mode, jisdata, _length, gs1, est_binlen, seg_ends, seg_ecis,
		seg_count, symbol.structapp.count, symbol.structapp.index, structapp_id_value);
	add_ecc(fullstream, datastream, version, target_binlen, blocks);

	size := qr_sizes[version - 1];
  SetLength(grid, size * size);

	for i := 0 to size - 1 do
		for j := 0 to size - 1 do
			grid[(i * size) + j] := 0;

	setup_grid(grid, size, version);
	populate_grid(grid, size, fullstream, qr_total_codewords[version - 1]);
	bitmask := apply_bitmask(grid, size);
	symbol.option_1 := ecc_level;
	symbol.option_2 := version;
	symbol.option_3 := (symbol.option_3 and $FF) or ((bitmask + 1) shl 8);
	add_format_info(grid, size, ecc_level, bitmask);
	if (version >= 7) then
		add_version_info(grid, size, version);

	symbol.width := size;
	symbol.rows := size;
	symbol.height := size;

	for i := 0 to size - 1 do
  begin
		for j := 0 to size - 1 do
			if (grid[(i * size) + j] and $01)<>0 then
				set_module(symbol, i, j);


		symbol.row_height[i] := 1;
	end;

	Result:=auto_eci_warning;
end;

function qr_code(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;
var
	seg_ends : TArrayOfInteger;
  seg_ecis : TArrayOfInteger;
begin
	SetLength(seg_ends, 0);
	SetLength(seg_ecis, 0);
	Result := qr_code_with_seg_ends(symbol, source, _length, seg_ends, seg_ecis, 0);
end;

function qr_code_segs(symbol : zint_symbol; const segs : TZintSegments): Integer;
var
	i, seg_len, total_len, posn : Integer;
	merged : TArrayOfByte;
	seg_ends : TArrayOfInteger;
  seg_ecis : TArrayOfInteger;
begin
	if Length(segs) = 0 then
	begin
		strcpy(symbol.errtxt, 'No input data');
		Result := ZERROR_INVALID_DATA;
		Exit;
	end;

	total_len := 0;
	SetLength(seg_ends, Length(segs));
	SetLength(seg_ecis, Length(segs));
	for i := 0 to High(segs) do
	begin
		seg_len := segs[i].Length;
		if (seg_len = 0) and (Length(segs[i].Source) > 0) then
			seg_len := ustrlen(segs[i].Source);
		if seg_len <= 0 then
		begin
			strcpy(symbol.errtxt, PChar(Format('Error 773: Input segment %d empty', [i])));
			Result := ZERROR_INVALID_DATA;
			Exit;
		end;
		if Length(segs[i].Source) < seg_len then
		begin
			strcpy(symbol.errtxt, 'Error 799: Segment length out of bounds');
			Result := ZERROR_INVALID_DATA;
			Exit;
		end;
		Inc(total_len, seg_len);
		seg_ends[i] := total_len;
		if segs[i].ECI >= 0 then
			seg_ecis[i] := segs[i].ECI
		else
			seg_ecis[i] := 0;
	end;

	SetLength(merged, total_len + 1);
	posn := 0;
	for i := 0 to High(segs) do
	begin
		seg_len := segs[i].Length;
		if (seg_len = 0) and (Length(segs[i].Source) > 0) then
			seg_len := ustrlen(segs[i].Source);
		Move(segs[i].Source[0], merged[posn], seg_len);
		Inc(posn, seg_len);
	end;
	merged[total_len] := 0;

	Result := qr_code_with_seg_ends(symbol, merged, total_len, seg_ends, seg_ecis, Length(seg_ends));
end;

function upnqr(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;
var
	saved_option_1, saved_option_2, saved_input_mode, saved_eci: Integer;
begin
	if symbol.input_mode = GS1_MODE then
	begin
		strcpy(symbol.errtxt, 'Error 220: Selected symbology does not support GS1 mode');
		Result := ZERROR_INVALID_OPTION;
		Exit;
	end;

	saved_option_1 := symbol.option_1;
	saved_option_2 := symbol.option_2;
	saved_input_mode := symbol.input_mode;
	saved_eci := symbol.eci;

	// UPNQR is fixed to QR Version 15 with ECC level M.
	symbol.option_1 := LEVEL_M;
	symbol.option_2 := 15;
	symbol.eci := 0;

	Result := qr_code(symbol, source, _length);

	// Keep UPNQR fixed parameters visible to callers after encode.
	if Result < ZERROR_TOO_LONG then
	begin
		symbol.option_1 := LEVEL_M;
		symbol.option_2 := 15;
	end
	else
	begin
		symbol.option_1 := saved_option_1;
		symbol.option_2 := saved_option_2;
	end;

	symbol.input_mode := saved_input_mode;
	symbol.eci := saved_eci;
end;

function rmqr(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;
var
	i, j, best_eci: Integer;
	est_binlen, prev_est_binlen, required_cw, max_cw: Integer;
	ecc_level, version_idx, autosize, blocks, target_binlen: Integer;
	ecc_idx, canShrink, footprint, best_footprint: Integer;
	h_size, v_size: Integer;
	ecc_name: Char;
	gs1, source_length, auto_eci_warning, eci_for_encoding: Integer;
	utfdata, jisdata, datastream, fullstream: TArrayOfInteger;
	mode: TArrayOfChar;
	grid: TArrayOfByte;
begin
	SetLength(utfdata, _length + 1);
	SetLength(jisdata, _length + 1);
	SetLength(mode, _length + 1);
	source_length := _length;
	auto_eci_warning := 0;
	eci_for_encoding := symbol.eci;

	if symbol.input_mode = GS1_MODE then
	begin
		strcpy(symbol.errtxt, 'Error 220: Selected symbology does not support GS1 mode');
		Result := ZERROR_INVALID_OPTION;
		Exit;
	end;
	gs1 := 0;

	if symbol.option_1 = 1 then
	begin
		strcpy(symbol.errtxt, 'Error correction level L not available in rMQR');
		Result := ZERROR_INVALID_OPTION;
		Exit;
	end;

	if symbol.option_1 = 3 then
	begin
		strcpy(symbol.errtxt, 'Error correction level Q not available in rMQR');
		Result := ZERROR_INVALID_OPTION;
		Exit;
	end;

	if (symbol.option_2 < 0) or (symbol.option_2 > 38) then
	begin
		strcpy(symbol.errtxt, PChar(Format('Version ''%d'' out of range (1 to 38)', [symbol.option_2])));
		Result := ZERROR_INVALID_OPTION;
		Exit;
	end;

	case(symbol.input_mode) of
		DATA_MODE:
			for i := 0 to _length - 1 do
				jisdata[i] := source[i];
	else
		if symbol.eci = 26 then
		begin
			for i := 0 to _length - 1 do
				jisdata[i] := source[i];
		end
	else
		begin
			Result := utf8toutf16(symbol, source, utfdata, _length);
			if (Result <> 0) then
				Exit;

			if symbol.eci = 0 then
			begin
				best_eci := rmqr_best_eci(utfdata, _length, jisdata);
				if best_eci = 3 then
				begin
					// ECI 3 is the implicit default and does not need signaling.
					symbol.eci := 0;
					eci_for_encoding := 0;
					auto_eci_warning := 0;
				end
				else if best_eci <> 26 then
				begin
					symbol.eci := best_eci;
					eci_for_encoding := best_eci;
					auto_eci_warning := ZWARN_USES_ECI;
				end
				else if rmqr_try_shift_jis(utfdata, _length, jisdata) then
				begin
					symbol.eci := 20;
					eci_for_encoding := 0;
					auto_eci_warning := ZWARN_NONCOMPLIANT;
				end
				else
				begin
					symbol.eci := 26;
					eci_for_encoding := 26;
					auto_eci_warning := ZWARN_USES_ECI;
					_length := source_length;
					for i := 0 to _length - 1 do
						jisdata[i] := source[i];
				end;
			end
			else if symbol.eci = 20 then
			begin
				eci_for_encoding := 20;
				if not rmqr_try_shift_jis(utfdata, _length, jisdata) then
				begin
					strcpy(symbol.errtxt, 'Error 800: Invalid character in input');
					Result := ZERROR_INVALID_DATA;
					Exit;
				end;
			end
			else if not try_single_byte_eci(utfdata, _length, symbol.eci, jisdata) then
			begin
				if symbol.eci = 3 then
					strcpy(symbol.errtxt, 'Error 575: Invalid character in input for ECI ''3''')
				else
					strcpy(symbol.errtxt, 'Error 800: Invalid character in input');
				Result := ZERROR_INVALID_DATA;
				Exit;
			end;
		end;
	end;

	define_mode(mode, jisdata, _length, gs1);
	rmqr_refine_modes(mode, jisdata, _length, gs1, eci_for_encoding);

	ecc_level := LEVEL_M;
	if symbol.option_1 = 4 then
		ecc_level := LEVEL_H;
	if ecc_level = LEVEL_H then
		ecc_idx := 1
	else
		ecc_idx := 0;

	est_binlen := estimate_binary_length_rmqr(mode, _length, gs1, 31, eci_for_encoding);
	max_cw := rmqr_data_codewords[ecc_idx, 31];
	if est_binlen > (8 * max_cw) then
	begin
		if ecc_level = LEVEL_H then
			ecc_name := 'H'
		else
			ecc_name := 'M';
		required_cw := (est_binlen + 7) div 8;
		strcpy(symbol.errtxt, PChar(Format('Error 578: Input too long for ECC level %s, requires %d codewords (maximum %d)',
			[String(ecc_name), required_cw, max_cw])));
		Result := ZERROR_TOO_LONG;
		Exit;
	end;

	version_idx := 31;
	if symbol.option_2 = 0 then
	begin
		autosize := 31;
		best_footprint := rmqr_height[31] * rmqr_width[31];
		for i := 30 downto 0 do
		begin
			est_binlen := estimate_binary_length_rmqr(mode, _length, gs1, i, eci_for_encoding);
			footprint := rmqr_height[i] * rmqr_width[i];
			if (8 * rmqr_data_codewords[ecc_idx, i] >= est_binlen) and (footprint < best_footprint) then
			begin
				autosize := i;
				best_footprint := footprint;
			end;
		end;
		version_idx := autosize;
		est_binlen := estimate_binary_length_rmqr(mode, _length, gs1, version_idx, eci_for_encoding);
	end
	else if (symbol.option_2 >= 1) and (symbol.option_2 <= 32) then
	begin
		version_idx := symbol.option_2 - 1;
		est_binlen := estimate_binary_length_rmqr(mode, _length, gs1, version_idx, eci_for_encoding);
	end
	else if symbol.option_2 >= 33 then
	begin
		version_idx := rmqr_fixed_height_upper_bound[symbol.option_2 - 32];
		for i := version_idx - 1 downto (rmqr_fixed_height_upper_bound[symbol.option_2 - 33] + 1) do
		begin
			est_binlen := estimate_binary_length_rmqr(mode, _length, gs1, i, eci_for_encoding);
			if (8 * rmqr_data_codewords[ecc_idx, i] >= est_binlen) then
				version_idx := i;
		end;
		est_binlen := estimate_binary_length_rmqr(mode, _length, gs1, version_idx, eci_for_encoding);
	end;

	if not ((symbol.option_1 = 2) or (symbol.option_1 = 4)) then
	begin
		canShrink := estimate_binary_length_rmqr(mode, _length, gs1, version_idx, eci_for_encoding);
		if canShrink < (rmqr_data_codewords[1, version_idx] * 8) then
		begin
			ecc_level := LEVEL_H;
			ecc_idx := 1;
		end;
	end;

	target_binlen := rmqr_data_codewords[ecc_idx, version_idx];
	blocks := rmqr_blocks[ecc_idx, version_idx];
	if est_binlen > (target_binlen * 8) then
	begin
		required_cw := (est_binlen + 7) div 8;
		if ecc_level = LEVEL_H then
			ecc_name := 'H'
		else
			ecc_name := 'M';
		strcpy(symbol.errtxt, PChar(Format('Error 560: Input too long for Version %d %s-%s, requires %d codewords (maximum %d)',
			[symbol.option_2, rmqr_version_names[symbol.option_2 - 1], String(ecc_name), required_cw, target_binlen])));
		Result := ZERROR_TOO_LONG;
		Exit;
	end;

	SetLength(datastream, target_binlen + 1);
	SetLength(fullstream, rmqr_total_codewords[version_idx] + 1);

	rmqr_binary(datastream, version_idx, target_binlen, mode, jisdata, _length, gs1, est_binlen, eci_for_encoding);
	add_ecc_rmqr(fullstream, datastream, version_idx, target_binlen, blocks);

	h_size := rmqr_width[version_idx];
	v_size := rmqr_height[version_idx];
	SetLength(grid, h_size * v_size);
	for i := 0 to v_size - 1 do
		for j := 0 to h_size - 1 do
			grid[(i * h_size) + j] := 0;

	rmqr_setup_grid(grid, h_size, v_size);
	populate_grid_rmqr(grid, h_size, v_size, fullstream, rmqr_total_codewords[version_idx]);

	for i := 0 to v_size - 1 do
		for j := 0 to h_size - 1 do
			if ((grid[(i * h_size) + j] and $F0) = 0) and (((i div 2) + (j div 3)) mod 2 = 0) then
				grid[(i * h_size) + j] := grid[(i * h_size) + j] xor $01;

	rmqr_add_format_info(grid, h_size, v_size, version_idx, ecc_level);

	symbol.option_1 := ecc_level;
	symbol.option_2 := version_idx + 1;
	symbol.width := h_size;
	symbol.rows := v_size;
	symbol.height := v_size;

	for i := 0 to v_size - 1 do
	begin
		for j := 0 to h_size - 1 do
			if (grid[(i * h_size) + j] and $01) <> 0 then
				set_module(symbol, i, j);
		symbol.row_height[i] := 1;
	end;

	if auto_eci_warning = ZWARN_NONCOMPLIANT then
		strcpy(symbol.errtxt, 'Warning 760: Converted to Shift JIS but no ECI specified');

	Result := auto_eci_warning;
end;

// NOTE: From this point forward concerns Micro QR Code only

function micro_qr_intermediate(var binary : TArrayOfChar; jisdata : TArrayOfInteger; mode : TArrayOfChar; _length : Integer; var kanji_used : integer; var alphanum_used : Integer; var byte_used : Integer) : Integer;
var
  position : Integer;
  short_data_block_length, i : Integer;
  data_block : Char;
  buffer : TArrayOfChar;
  jis, _byte : Integer;
  msb, lsb, prod : Integer;
  count, first, second, third : Integer;
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
							 msb := (jis and $ff00) shr 8;
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
  _length, i : Integer;
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

procedure microqr_expand_binary(var binary_stream : TArrayOfChar; var full_stream : TArrayOfChar; version : Integer);
var
   i, _length : Integer;
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
	i, latch : Integer;
	bits_total, bits_left, remainder : Integer;
	data_codewords, ecc_codewords : Integer;
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
		for i := 0 to bits_left - 1 do
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
			for i := 0 to bits_left - 1 do
      	concat(binary_data, '0');
			latch := 1;
		end;
	end;

	if(latch = 0) then
  begin
		// Complete current byte
		remainder := 8 - (strlen(binary_data) mod 8);
		if (remainder = 8) then remainder := 0;
		for i := 0 to remainder - 1 do
			concat(binary_data, '0');

		// Add padding
		bits_left := bits_total - strlen(binary_data);
		if (bits_left > 4) then
    begin
			remainder := (bits_left - 4) div 8;
			for i := 0 to remainder - 1 do
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
	for i := 0 to (data_codewords - 1) - 1 do
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
	for i := 0 to ecc_codewords - 1 do
		bscan(binary_data, ecc_blocks[ecc_codewords - i - 1], $80);
end;

procedure micro_qr_m2(var binary_data: TArrayOfChar; ecc_mode : Integer);
var
	i, latch : Integer;
	bits_total, bits_left, remainder : Integer;
	data_codewords, ecc_codewords : Integer;
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
		for i := 0 to bits_left - 1 do
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
		for i := 0 to remainder - 1 do
			concat(binary_data, '0');

		// Add padding
		bits_left := bits_total - strlen(binary_data);
		remainder := bits_left div 8;
		for i := 0 to remainder - 1 do
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
	for i := 0 to data_codewords - 1 do
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
  for i := 0 to ecc_codewords - 1 do
   	bscan(binary_data, ecc_blocks[ecc_codewords - i - 1], $80);
end;

procedure micro_qr_m3(var binary_data : TArrayOfChar; ecc_mode : Integer);
var
	i, latch : Integer;
	bits_total, bits_left, remainder : Integer;
	data_codewords, ecc_codewords : Integer;
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
		for i := 0 to bits_left-1 do
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
			for i := 0 to bits_left - 1 do
				concat(binary_data, '0');
			latch := 1;
		end;
	end;

	if(latch = 0) then
  begin
		// Complete current byte
		remainder := 8 - (strlen(binary_data) mod 8);
		if (remainder = 8) then remainder := 0;
		for i := 0 to remainder - 1 do
			concat(binary_data, '0');

		// Add padding
		bits_left := bits_total - strlen(binary_data);
		if(bits_left > 4) then
    begin
			remainder := (bits_left - 4) div 8;
			for i := 0 to remainder - 1 do
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
	for i := 0 to (data_codewords - 1) - 1 do
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
	for i := 0 to ecc_codewords - 1 do
  begin
		bscan(binary_data, ecc_blocks[ecc_codewords - i - 1], $80);
	end;
end;

procedure micro_qr_m4(var binary_data : TArrayOfChar; ecc_mode : Integer);
var
	i, latch : Integer;
	bits_total, bits_left, remainder : Integer;
	data_codewords, ecc_codewords : Integer;
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
		for i := 0 to bits_left - 1 do
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
		for i := 0 to remainder - 1 do
    	concat(binary_data, '0');

		// Add padding
		bits_left := bits_total - strlen(binary_data);
		remainder := bits_left div 8;
		for i := 0 to remainder - 1 do
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
	for i := 0 to data_codewords - 1 do
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
	for i := 0 to ecc_codewords - 1 do
		bscan(binary_data, ecc_blocks[ecc_codewords - i - 1], $80);
end;

procedure micro_setup_grid(var grid : TArrayOfByte; size : Integer);
var
  i : Integer;
  toggle: Integer;
begin
	toggle := 1;

	// Add timing patterns
	for i := 0 to size - 1 do
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
	for i := 0 to 6 do
  begin
		grid[(7 * size) + i] := $10;
		grid[(i * size) + 7] := $10;
	end;
	grid[(7 * size) + 7] := $10;


	// Reserve space for format information
	for i := 0 to 7 do
  begin
		inc(grid[(8 * size) + i], $20);
		inc(grid[(i * size) + 8], $20);
	end;
	inc(grid[(8 * size) + 8], 20);
end;

procedure micro_populate_grid(var grid : TArrayOfByte; size : Integer; full_stream: TArrayOfChar);
var
  direction : Integer;
  row : Integer;
  i, n, x, y : Integer;
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

function micro_evaluate(var grid : TArrayOfByte; size : Integer; pattern : Integer): Integer;
var
  sum1, sum2, i, filter, retval: Integer;
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
	for i := 1 to size - 1 do
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

function micro_apply_bitmask(var grid : TArrayOfByte; size: Integer): Integer;
var
	x, y: Integer;
	p : Byte;
	pattern : Integer;
  value : TArrayOfInteger;
	best_val, best_pattern: Integer;
	bit : Integer;
	mask : TArrayOfByte;
	eval : TArrayOfByte;
begin
  SetLength(value,8);
	SetLength(mask, size * size);
	SetLength(eval, size * size);

	// Perform data masking
	for x := 0 to size -1 do
  begin
		for y := 0 to size - 1 do
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

	for x := 0 to size - 1 do
  begin
		for y := 0 to size - 1 do
    begin
			if(grid[(y * size) + x] and $01)<>0 then p := $ff else p := $00;

			eval[(y * size) + x] := mask[(y * size) + x] xor p;
		end;
	end;

	// Evaluate result
	for pattern := 0 to 7 do
		value[pattern] := micro_evaluate(eval, size, pattern);

	best_pattern := 0;
	best_val := value[0];
	for pattern := 1 to 3 do
  begin
		if(value[pattern] > best_val) then
    begin
			best_pattern := pattern;
			best_val := value[pattern];
		end;
	end;

	// Apply mask
	for x := 0 to size - 1 do
  begin
		for y := 0 to size - 1 do
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
	i, j, glyph, size: Integer;
	binary_stream: TArrayOfChar;
	full_stream: TArrayOfChar;
	utfdata: TArrayOfInteger;
	jisdata: TArrayOfInteger;
	mode: TArrayOfChar;
	error_number, kanji_used, alphanum_used, byte_used: Integer;
	version_valid: TArrayOfInteger;
	binary_count: TArrayOfInteger;
	ecc_level, autoversion, version: Integer;
	n_count, a_count, bitmask, format, format_full: Integer;
  source_length, auto_eci_warning, auto_eci_fallback, auto_eci_mode: Integer;
  grid: TArrayOfByte;
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
	source_length := _length;
	auto_eci_warning := 0;

	if(_length > 35) then
  begin
		strcpy(symbol.errtxt, 'Input data too long');
		Result:=ZERROR_TOO_LONG;
    exit;
	end;

	for i := 0 to 3 do
  	version_valid[i] := 1;

	case(symbol.input_mode) of
		 DATA_MODE:
			for i := 0 to _length - 1 do
      	jisdata[i] := source[i];
     else
			if symbol.eci = 26 then
			begin
				for i := 0 to _length - 1 do
					jisdata[i] := source[i];
			end
			else
      begin
			// Convert Unicode input to Shift-JIS
			error_number := utf8toutf16(symbol, source, utfdata, _length);
			if(error_number <> 0) then begin Result:=error_number; Exit end;

			if symbol.eci = 0 then
			begin
				symbol.eci := 20;
				auto_eci_mode := 1;
			end
			else
				auto_eci_mode := 0;

			auto_eci_fallback := 0;

			for i := 0 to _length - 1 do
      begin
				if (symbol.eci = 3) and (utfdata[i] > $FF) then
          begin
					strcpy(symbol.errtxt, 'Error 575: Invalid character in input for ECI ''3''');
					Result := ZERROR_INVALID_DATA;
            Exit;
          end;

				if(utfdata[i] <= $ff) then
				begin
					if symbol.eci = 20 then
					begin
						if utfdata[i] = $A5 then
							jisdata[i] := $5C
						else if (utfdata[i] <= $7F) and (utfdata[i] <> $7E) then
							jisdata[i] := utfdata[i]
						else
              begin
							if (symbol.eci = 20) and (auto_eci_mode <> 0) then
							begin
								auto_eci_fallback := 1;
								Break;
							end;
							strcpy(symbol.errtxt, 'Error 800: Invalid character in input');
							Result := ZERROR_INVALID_DATA;
                Exit;
              end;
					end
					else
						jisdata[i] := utfdata[i];
				end
				else if (symbol.eci = 20) and (utfdata[i] >= $FF61) and (utfdata[i] <= $FF9F) then
					jisdata[i] := (utfdata[i] - $FF61) + $A1
				else if (symbol.eci = 20) and (utfdata[i] = $203E) then
					jisdata[i] := $7E
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
							if (symbol.eci = 20) and (auto_eci_mode <> 0) then
							begin
								auto_eci_fallback := 1;
								Break;
							end;
							if symbol.eci = 3 then
								strcpy(symbol.errtxt, 'Error 575: Invalid character in input for ECI ''3''')
							else
								strcpy(symbol.errtxt, 'Error 800: Invalid character in input');
						Result:=ZERROR_INVALID_DATA;
            Exit;
					end;
					jisdata[i] := glyph;
				end;
			end;

			if auto_eci_fallback <> 0 then
			begin
				symbol.eci := 26;
				auto_eci_warning := ZWARN_USES_ECI;
				_length := source_length;
				for i := 0 to _length - 1 do
					jisdata[i] := source[i];
			end;
	end;

	define_mode(mode, jisdata, _length, 0);

	n_count := 0;
	a_count := 0;
	for i := 0 to _length - 1 do
  begin
		if((jisdata[i] >= Ord('0')) and (jisdata[i] <= Ord('9'))) then inc(n_count);
		if(in_alpha(jisdata[i])<>0) then Inc(a_count);
	end;

	if (a_count = _length) then  	// All data can be encoded in Alphanumeric mode
		for i := 0 to _length - 1 do
			mode[i] := 'A';

	if (n_count = _length) then   // All data can be encoded in Numeric mode
		for i := 0 to _length - 1 do
			mode[i] := 'N';
	end;

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

	for i := 0 to size - 1 do
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

	for i := 0 to size - 1 do
  begin
		for j := 0 to size - 1 do
    begin
			if (grid[(i * size) + j] and $01)<>0 then
				set_module(symbol, i, j);
		end;
		symbol.row_height[i] := 1;
	end;

	Result:=auto_eci_warning;
end;

end.
