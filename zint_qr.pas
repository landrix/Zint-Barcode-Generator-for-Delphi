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

function estimate_binary_length(mode : TArrayOfChar; _length : Integer; gs1 : Integer) : Integer;
var
  i, count : Integer;
  current  : Char;
  a_count  : Integer;
  n_count  : Integer;
begin
  count := 0;
  current := #0;
  a_count := 0;
  n_count := 0;

	// Make an estimate (worst case scenario) of how long the binary string will be


	if (gs1<>0) then inc(count, 4);

	for i := 0 to _length - 1 do
  begin
		if(mode[i] <> current) then
    begin
			case mode[i] of
				'K': begin inc(count, 12 + 4); current := 'K'; end;
				'B': begin inc(count, 16 + 4); current := 'B'; end;
				'A': begin inc(count, 13 + 4); current := 'A'; a_count := 0; end;
				'N': begin inc(count, 14 + 4); current := 'N'; n_count := 0; end;
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

procedure qr_binary(var datastream: TArrayOfInteger; version: Integer; target_binlen : Integer; mode : TArrayOfChar; jisdata : TArrayOfInteger; _length : Integer; gs1 : Integer; est_binlen : Integer);
var
  position: Integer;
  short_data_block_length, i, scheme : Integer;
  data_block : Char;
  padbits : Integer;
  current_binlen, current_bytes : Integer;
  toggle, percent : Integer;
  binary : TArrayOfChar;
  jis , j: Integer;
  msb, lsb, prod : Integer;
  _byte : Integer;
  count : Integer;
  first, second, third : Integer;
begin
  // Convert input data to a binary stream and add padding
	position := 0;
  scheme := 1;
  SetLength(binary, est_binlen + 12);
	strcpy(binary, '');

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

	repeat
		data_block := mode[position];
		short_data_block_length := 0;
		repeat
			inc(short_data_block_length);
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

              if (jis > $9fff) then dec(jis, $c140);
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
                first := 0;
                second := 0;

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
                first := 0;
                second := 0;
                third := 0;

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
			interleaved_ecc[(j * blocks) + i] := ecc_block[ecc_block_length - j - 1];

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
  x, y, block : Integer;
  _result : Integer;
  state : Char;
  p : Integer;
  dark_mods : Integer;
  percentage, k : Integer;
  local : TArrayOfChar;
begin
	_result := 0;

	SetLength(local, size * size);

	for x := 0 to size - 1 do
  begin
		for y := 0 to size - 1 do
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
	for x := 0 to size - 1 do
  begin
		state := local[x];
		block := 0;
		for y := 0 to size - 1 do
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
	for y := 0 to size - 1 do
  begin
		state := local[y * size];
		block := 0;
		for x := 0 to size - 1 do
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
	for x := 0 to size - 1 do
  begin
		for y := 0 to (size - 7) - 1 do
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
	for y := 0 to size - 1 do
  begin
		for x := 0 to (size - 7) - 1 do
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
	for x := 0 to size - 1 do
  begin
		for y := 0 to size - 1 do
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

function qr_code(symbol : zint_symbol; source : TArrayOfByte; _length : Integer): Integer;
var
  i, j : Integer;
  error_number, glyph, est_binlen : Integer;
  ecc_level, autosize, version, max_cw, target_binlen, blocks, size : Integer;
  bitmask, gs1 : Integer;
  utfdata : TArrayOfInteger;
  jisdata : TArrayOfInteger;
  mode : TArrayOfChar;
  datastream, fullstream : TArrayOfInteger;
  grid : TArrayOfByte;
begin
	SetLength(utfdata, _length + 1);
	SetLength(jisdata, _length + 1);
	SetLength(mode, _length + 1);

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

	define_mode(mode, jisdata, _length, gs1);
	est_binlen := estimate_binary_length(mode, _length, gs1);

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

	if((symbol.option_2 >= 1) and (symbol.option_2 <= 40)) then
  begin
		if (symbol.option_2 > autosize) then
			version := symbol.option_2
		else
			version := autosize;
	end
  else
		version := autosize;

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
	add_ecc(fullstream, datastream, version, target_binlen, blocks);

	size := qr_sizes[version - 1];
  SetLength(grid, size * size);

	for i := 0 to size - 1 do
		for j := 0 to size - 1 do
			grid[(i * size) + j] := 0;

	setup_grid(grid, size, version);
	populate_grid(grid, size, fullstream, qr_total_codewords[version - 1]);
	bitmask := apply_bitmask(grid, size);
	add_format_info(grid, size, ecc_level, bitmask);
	if (version >= 7) then
		add_version_info(grid, size, version);

	symbol.width := size;
	symbol.rows := size;

	for i := 0 to size - 1 do
  begin
		for j := 0 to size - 1 do
			if (grid[(i * size) + j] and $01)<>0 then
				set_module(symbol, i, j);


		symbol.row_height[i] := 1;
	end;

	Result:=0;
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
              first := 0;
              second := 0;

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
               first := 0;
               second := 0;
               third := 0;

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

	if(ecc_mode = LEVEL_L) then bits_total := 40;
	if(ecc_mode = LEVEL_M) then bits_total := 32;

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

	if(ecc_mode = LEVEL_L) then begin data_codewords := 5; ecc_codewords := 5; end;
	if(ecc_mode = LEVEL_M) then begin data_codewords := 4; ecc_codewords := 6; end;

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

	if(ecc_mode = LEVEL_L) then bits_total := 84;
	if(ecc_mode = LEVEL_M) then bits_total := 68;

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

	if(ecc_mode = LEVEL_L) then begin data_codewords := 11; ecc_codewords := 6; end;
	if(ecc_mode = LEVEL_M) then begin data_codewords := 9; ecc_codewords := 8; end;

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

	if(ecc_mode = LEVEL_L) then bits_total := 128;
	if(ecc_mode = LEVEL_M) then bits_total := 112;
	if(ecc_mode = LEVEL_Q) then bits_total := 80;

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

	if(ecc_mode = LEVEL_L) then begin data_codewords := 16; ecc_codewords := 8; end;
	if(ecc_mode = LEVEL_M) then begin data_codewords := 14; ecc_codewords := 10; end;
	if(ecc_mode = LEVEL_Q) then begin data_codewords := 10; ecc_codewords := 14; end;

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
      begin
			// Convert Unicode input to Shift-JIS
			error_number := utf8toutf16(symbol, source, utfdata, _length);
			if(error_number <> 0) then begin Result:=error_number; Exit end;

			for i := 0 to _length - 1 do
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

	Result:=0;
end;

end.
