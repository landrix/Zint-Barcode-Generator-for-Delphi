unit zint_code128;

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

function code_128(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function ean_128(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function nve_18(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function ean_14(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses
  SysUtils, zint_common, zint_gs1, zint_helper;

const
  DPDSET = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*';

type
  TGlobalList = array[0..1] of array[0..169] of Integer;

const
  {Code 128 tables checked against ISO/IEC 15417:2007 }

  C128Table : array[0..106] of String = ('212222', '222122', '222221', '121223', '121322', '131222', '122213',
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
  	'2331112');
  { Code 128 character encodation - Table 1 }

 {
 * bring together same type blocks
 }
procedure grwp(var indexliste : Integer; var list : TGlobalList);
var
  i, j : Integer;
begin
	if (indexliste <= 1) then
    exit;

  //because i is modified inside the loop, we have to use "while"
  i := 1;
	while i < indexliste do
  begin
		if (list[1][i - 1] = list[1][i]) then
    begin
			{ bring together }
			list[0][i - 1] := list[0][i - 1] + list[0][i];

			{ decreace the list }
			for j := i + 1 to indexliste - 1 do
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

 {
 * Implements rules from ISO 15417 Annex E
 }
procedure dxsmooth(var indexliste : Integer; var list : TGlobalList);
var
	i, current, _length, last, next : Integer;
begin
	for i := 0  to indexliste - 1 do
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

		if(i = 0) then
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
          list[1][i] := AORB; current := AORB;
        end;
			end;

			if (current = SHIFTA) then
        { Rule 1c }
        list[1][i] := LATCHA;
			if ((current = AORB) and (next = SHIFTA)) then
      begin
        { Rule 1c }
        list[1][i] := LATCHA; current := LATCHA;
      end;
			if (current = AORB) then
        { Rule 1d }
        list[1][i] := LATCHB;
		end
    else
    begin
			if ((current = ABORC) and (_length >= 4)) then
      begin
        { Rule 3 }
        list[1][i] := LATCHC; current := LATCHC;
      end;
			if (current = ABORC) then
      begin
        list[1][i] := AORB; current := AORB;
      end;
			if ((current = AORB) and (last = LATCHA)) then
      begin
        list[1][i] := LATCHA; current := LATCHA;
      end;
			if ((current = AORB) and (last = LATCHB)) then
      begin
        list[1][i] := LATCHB; current := LATCHB;
      end;
			if ((current = AORB) and (next = SHIFTA)) then
      begin
        list[1][i] := LATCHA; current := LATCHA;
      end;
			if ((current = AORB) and (next = SHIFTB)) then
      begin
        list[1][i] := LATCHB; current := LATCHB;
      end;
			if (current = AORB) then
      begin
        list[1][i] := LATCHB; current := LATCHB;
      end;
			if ((current = SHIFTA) and (_length > 1)) then
      begin
        { Rule 4 }
        list[1][i] := LATCHA; current := LATCHA;
      end;
			if ((current = SHIFTB) and (_length > 1)) then
      begin
        { Rule 5 }
        list[1][i] := LATCHB; current := LATCHB;
      end;
			if ((current = SHIFTA) and (last = LATCHA)) then
      begin
        list[1][i] := LATCHA; current := LATCHA;
      end;
			if ((current = SHIFTB) and (last = LATCHB)) then
      begin
        list[1][i] := LATCHB; current := LATCHB;
      end;
			if ((current = SHIFTA) and (last = LATCHC)) then
      begin
        list[1][i] := LATCHA; current := LATCHA;
      end;
			if ((current = SHIFTB) and (last = LATCHC)) then
      begin
        list[1][i] := LATCHB; //current := LATCHB;
      end;
		end; { Rule 2 is implimented elsewhere, Rule 6 is implied }
	end;
	grwp(indexliste, list);
end;

 {
 * Translate Code 128 Set A characters into barcodes.
 * This set handles all control characters NULL to US.
 }
procedure c128_set_a(source : Byte; var dest : TArrayOfChar; var values : TArrayOfInteger; var bar_chars : Integer);
begin
  { limit the range to 0-127 }
  source := source and 127;

	if (source < 32) then
		source := source + 64
	else
		source := source - 32;

	concat(dest, C128Table[source]);
	values[bar_chars] := source;
  Inc(bar_chars);
end;

 {
 * Translate Code 128 Set B characters into barcodes.
 * This set handles all characters which are not part of long numbers and not
 * control characters.
 }
procedure c128_set_b(source : Byte; var dest : TArrayOfChar; var values : TArrayOfInteger; var bar_chars : Integer);
begin
	{ limit the range to 0-127 }
	source := source and 127;
	source := source - 32;

	concat(dest, C128Table[source]);
	values[bar_chars] := source;
  Inc(bar_chars);
end;

 {
 * Translate Code 128 Set C characters into barcodes.
 * This set handles numbers in a compressed form.
 }
procedure c128_set_c(source_a : Byte; source_b : Byte; var dest : TArrayOfChar; var values : TArrayOfInteger; var bar_chars : Integer);
var
  weight : Integer;
 begin
	weight := (10 * StrToInt(Chr(source_a))) + StrToInt(Chr(source_b));
	concat(dest, C128Table[weight]);
	values[bar_chars] := weight;
	Inc(bar_chars);
end;

 {
 * Handle Code 128 and NVE-18.
 }
function code_128(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  i, j, k, bar_characters, read, total_sum : Integer;
  values : TArrayOfInteger;
  error_number, indexchaine, indexliste, sourcelen, f_state : Integer;
  _set, fset : TArrayOfChar;
  last_set, current_set : Char;
  mode : Integer;
  glyph_count : Single;
  dest : TArrayOfChar;
  list : TGlobalList;
begin
  SetLength(_set, 170);
  Fill(_set, Length(_set), ' ');
  SetLength(fset, 170);
  Fill(fset, Length(fset), ' ');
  SetLength(dest, 1000);
  SetLength(values, 170);
  FillChar(values[0], Length(values), 0);
  current_set := ' ';
	error_number := 0;
	strcpy(dest, '');

	sourcelen := _length;

	bar_characters := 0;
	f_state := 0;

	if (sourcelen > 160) then
  begin
		{ This only blocks rediculously long input - the actual length of the
		   resulting barcode depends on the type of data, so this is trapped later }
		strcpy(symbol.errtxt, 'Input too long');
		result := ZERROR_TOO_LONG; exit;
	end;

	{ Detect extended ASCII characters }
	for i := 0 to sourcelen - 1 do
  begin
		if (source[i] >= 128) then
			fset[i] := 'f';
	end;
  fset[sourcelen] := #0;

	{ Decide when to latch to extended mode - Annex E note 3 }
	j := 0;
	for i := 0 to sourcelen - 1 do
  begin
		if (fset[i] = 'f') then
			Inc(j)
		else
			j := 0;

		if (j >= 5) then
    begin
			for k := i downto (i - 4) do
				fset[k] := 'F';
		end;

		if ((j >= 3) and (i = sourcelen - 1)) then
    begin
			for k := i downto i - 2 do
				fset[k] := 'F';
		end;
	end;

	{ Decide if it is worth reverting to 646 encodation for a few
	   characters as described in 4.3.4.2 (d) }
	for i := 1 to sourcelen - 1 do
  begin
		if ((fset[i - 1] = 'F') and (fset[i] = ' ')) then
    begin
			{ Detected a change from 8859-1 to 646 - count how long for }
      j := 0;
      while (fset[i + j] = ' ') and ((i + j) < sourcelen) do
        Inc(j);

			if ((j < 5) or ((j < 3) and ((i + j) = sourcelen - 1))) then
      begin
				{ Uses the same figures recommended by Annex E note 3 }
				{ Change to shifting back rather than latching back }
				for k := 0 to j - 1 do
					fset[i + k] := 'n';
			end;
		end;
	end;

	{ Decide on mode using same system as PDF417 and rules of ISO 15417 Annex E }
	indexliste := 0;
	indexchaine := 0;

	mode := parunmodd(source[indexchaine]);
	if((symbol.symbology = BARCODE_CODE128B) and (mode = ABORC)) then
		mode := AORB;

  FillChar(list[0], 170, 0);

	repeat
		list[1][indexliste] := mode;
		while ((list[1][indexliste] = mode) and (indexchaine < sourcelen)) do
    begin
			Inc(list[0][indexliste]);
			Inc(indexchaine);
			mode := parunmodd(source[indexchaine]);
			if ((symbol.symbology = BARCODE_CODE128B) and (mode = ABORC)) then
				mode := AORB;
		end;
		Inc(indexliste);
	until not (indexchaine < sourcelen);

	dxsmooth(indexliste, list);

	{ Resolve odd length LATCHC blocks }
	if ((list[1][0] = LATCHC) and ((list[0][0] and 1) <> 0)) then
  begin
		{ Rule 2 }
		Inc(list[0][1]);
		Dec(list[0][0]);
		if (indexliste = 1) then
    begin
			list[0][1] := 1;
			list[1][1] := LATCHB;
			indexliste := 2;
		end;
	end;
	if (indexliste > 1) then
  begin
		for i := 1 to indexliste - 1 do
    begin
			if ((list[1][i] = LATCHC) and ((list[0][i] and 1) <> 0)) then
      begin
				{ Rule 3b }
				Inc(list[0][i - 1]);
				Dec(list[0][i]);
			end;
		end;
	end;

	{ Put set data into set[] }

	read := 0;
	for i := 0 to indexliste - 1 do
  begin
		for j := 0 to list[0][i] - 1 do
    begin
			case(list[1][i]) of
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
  i := 0;
  while _set[i] = 'a' do
  begin
    _set[i] := 'A';
    Inc(i);
  end;

  i := 0;
  while _set[i] = 'b' do
  begin
    _set[i] := 'B';
    Inc(i);
  end;

	{ Now we can calculate how long the barcode is going to be - and stop it from
	   being too long }
	last_set := ' ';
	glyph_count := 0.0;
	for i := 0 to sourcelen - 1 do
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

		if(_set[i] = 'C') then
			glyph_count := glyph_count + 0.5
		else
			glyph_count := glyph_count + 1.0;
	end;
	if (glyph_count > 80.0) then
  begin
		strcpy(symbol.errtxt, 'Input too long');
		result := ZERROR_TOO_LONG; exit;
	end;

	{ So now we know what start character to use - we can get on with it! }
	if (symbol.output_options and READER_INIT) <> 0 then
  begin
		{ Reader Initialisation mode }
		case _set[0] of
			'A': { Start A }
      begin
			  concat(dest, C128Table[103]);
				values[0] := 103;
				current_set := 'A';
				concat(dest, C128Table[96]); { FNC3 }
				values[1] := 96;
				Inc(bar_characters);
      end;
      'B': { Start B }
      begin
				concat(dest, C128Table[104]);
				values[0] := 104;
				current_set := 'B';
				concat(dest, C128Table[96]);{ FNC3 }
				values[1] := 96;
				Inc(bar_characters);
      end;
      'C': { Start C }
      begin
				concat(dest, C128Table[104]); { Start B }
				values[0] := 105;
				concat(dest, C128Table[96]); { FNC3 }
				values[1] := 96;
				concat(dest, C128Table[99]); { Code C }
				values[2] := 99;
				Inc(bar_characters, 2);
				current_set := 'C';
      end;
    end;
  end
	else
  begin
		{ Normal mode }
		case _set[0] of
			'A': { Start A }
      begin
				concat(dest, C128Table[103]);
				values[0] := 103;
				current_set := 'A';
      end;
			'B': { Start B }
      begin
				concat(dest, C128Table[104]);
				values[0] := 104;
				current_set := 'B';
			end;
			'C': { Start C }
      begin
				concat(dest, C128Table[105]);
				values[0] := 105;
				current_set := 'C';
			end;
		end;
	end;
	Inc(bar_characters);
	//last_set := _set[0];

	if(fset[0] = 'F') then
  begin
		case current_set of
			'A':
      begin
				concat(dest, C128Table[101]);
				concat(dest, C128Table[101]);
				values[bar_characters] := 101;
				values[bar_characters + 1] := 101;
			end;
			'B':
      begin
				concat(dest, C128Table[100]);
				concat(dest, C128Table[100]);
				values[bar_characters] := 100;
				values[bar_characters + 1] := 100;
			end;
		end;
		Inc(bar_characters, 2);
		f_state := 1;
	end;

	{ Encode the data }
	read := 0;
	repeat
		if ((read <> 0) and (_set[read] <> current_set)) then
		begin { Latch different code set }
			case _set[read] of
				'A':
        begin
          concat(dest, C128Table[101]);
					values[bar_characters] := 101;
					Inc(bar_characters);
					current_set := 'A';
				end;
				'B':
        begin
          concat(dest, C128Table[100]);
					values[bar_characters] := 100;
					Inc(bar_characters);
					current_set := 'B';
				end;
				'C':
        begin
          concat(dest, C128Table[99]);
					values[bar_characters] := 99;
					Inc(bar_characters);
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
						concat(dest, C128Table[101]);
						concat(dest, C128Table[101]);
						values[bar_characters] := 101;
						values[bar_characters + 1] := 101;
					end;
					'B':
          begin
						concat(dest, C128Table[100]);
						concat(dest, C128Table[100]);
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
						concat(dest, C128Table[101]);
						concat(dest, C128Table[101]);
						values[bar_characters] := 101;
						values[bar_characters + 1] := 101;
          end;
          'B':
          begin
						concat(dest, C128Table[100]);
						concat(dest, C128Table[100]);
						values[bar_characters] := 100;
						values[bar_characters + 1] := 100;
          end;
        end;
				Inc(bar_characters, 2);
				f_state := 0;
			end;
		end;

		if ((fset[read] = 'f') or (fset[read] = 'n')) then
    begin
			{ Shift to or from extended mode }
			case current_set of
				'A':
        begin
					concat(dest, C128Table[101]); { FNC 4 }
					values[bar_characters] := 101;
        end;
        'B':
        begin
					concat(dest, C128Table[100]); { FNC 4 }
					values[bar_characters] := 100;
        end;
      end;
			Inc(bar_characters);
    end;

		if ((_set[read] = 'a') or (_set[read] = 'b')) then
    begin
			{ Insert shift character }
			concat(dest, C128Table[98]);
			values[bar_characters] := 98;
			Inc(bar_characters);
    end;

		case _set[read] of
		{ Encode data characters }
		  'a',
			'A':
      begin
        c128_set_a(source[read], dest, values, bar_characters);
				Inc(read);
			end;
			'b',
			'B':
      begin
        c128_set_b(source[read], dest, values, bar_characters);
				Inc(read);
			end;
			'C':
      begin
        c128_set_c(source[read], source[read + 1], dest, values, bar_characters);
				Inc(read, 2);
      end;
    end;
	until not (read < sourcelen);

	{ check digit calculation }
	total_sum := 0;

	for i := 0 to bar_characters - 1 do
  begin
		if (i > 0) then
			values[i] := values[i] * i;
		Inc(total_sum, values[i]);
  end;
	concat(dest, C128Table[total_sum mod 103]);

	{ Stop character }
	concat(dest, C128Table[106]);
	expand(symbol, dest);
	result := error_number; exit;
end;

{ Handle EAN-128 (Now known as GS1-128) }
function ean_128(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  values : TArrayOfInteger;
  bar_characters, read, total_sum : Integer;
  error_number, indexchaine, indexliste : Integer;
  _set : TArrayOfChar;
  mode : Integer;
  last_set : Char;
  glyph_count : Single;
  dest : TArrayOfChar;
  separator_row, linkage_flag, c_count : Integer;
  reduced : TArrayOfChar;
  i, j : Integer;
  list : TGlobalList;
begin
  SetLength(dest, 1000);
  SetLength(values, 170);
  SetLength(_set, 170);
  SetLength(reduced, _length + 1);
	error_number := 0;
  strcpy(dest, '');
	linkage_flag := 0;

	bar_characters := 0;
	separator_row := 0;

  FillChar(values[0], Length(Values), 0);
  Fill(_set, Length(_set), ' ');

	if(_length > 160) then
  begin
		{ This only blocks rediculously long input - the actual Length(source) of the
		resulting barcode depends on the type of data, so this is trapped later }
		strcpy(symbol.errtxt, 'Input too long');
		result := ZERROR_TOO_LONG; exit;
	end;

	for i := 0 to _length - 1 do
  begin
		if (source[i] = 0) then
    begin
			{ Null characters not allowed! }
			strcpy(symbol.errtxt, 'NULL character in input data');
			result := ZERROR_INVALID_DATA; exit;
		end;
	end;

	{ if part of a composite symbol make room for the separator pattern }
	if (symbol.symbology = BARCODE_EAN128_CC) then
  begin
		separator_row := symbol.rows;
		symbol.row_height[symbol.rows] := 1;
		Inc(symbol.rows);
	end;

	if(symbol.input_mode <> GS1_MODE) then
  begin
		{ GS1 data has not been checked yet }
		error_number := gs1_verify(symbol, source, _length, reduced);
		if (error_number <> 0) then begin result := error_number; exit; end;
	end;

	{ Decide on mode using same system as PDF417 and rules of ISO 15417 Annex E }
	indexliste := 0;
	indexchaine := 0;

	mode := parunmodd(Ord(reduced[indexchaine]));
	if(reduced[indexchaine] = '[') then
  begin
		mode := ABORC;
	end;

  FillChar(list[0], Length(list[0]), 0);

	repeat
		list[1][indexliste] := mode;
		while ((list[1][indexliste] = mode) and (indexchaine < zint_common.strlen(reduced))) do
    begin
			Inc(list[0][indexliste]);
			Inc(indexchaine);
			mode := parunmodd(reduced[indexchaine]);
			if (reduced[indexchaine] = '[') then mode := ABORC;
		end;
		Inc(indexliste);
	until not (indexchaine < zint_common.strlen(reduced));

	dxsmooth(indexliste, list);

	{ Put set data into _set[] }
	read := 0;
	for i := 0 to indexliste - 1 do
  begin
		for j := 0 to list[0][i] - 1 do
    begin
			case list[1][i] of
			  SHIFTA:
				  _set[read] := 'a';
			  LATCHA:
				  _set[read] := 'A';
			  SHIFTB:
				  _set[read] := 'b';
			  LATCHB:
				  _set[read] := 'B';
			  LATCHC:
				  _set[read] := 'C';
			end;
			Inc(read);
		end;
	end;

	{ Watch out for odd-Length(source) Mode C blocks }
	c_count := 0;
	for i := 0 to read - 1 do
  begin
		if (_set[i] = 'C') then
    begin
			if (reduced[i] = '[') then
      begin
				if (c_count and 1) <> 0 then
        begin
					if ((i - c_count) <> 0) then
						_set[i - c_count] := 'B'
          else
						_set[i - 1] := 'B';
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
  end;

	if (c_count and 1) <> 0 then
  begin
		if (read - c_count <> 0) then
			_set[read - c_count] := 'B'
    else
			_set[read - 1] := 'B';
	end;

	for i := 1 to read - 2 do
  begin
		if ((_set[i] = 'C') and ((_set[i - 1] = 'B') and (_set[i + 1] = 'B'))) then
			_set[i] := 'B';
	end;

	{ Now we can calculate how long the barcode is going to be - and stop it from
	   being too long }
	last_set := ' ';
	glyph_count := 0.0;
	for i := 0 to strlen(reduced) - 1 do
  begin
		if ((_set[i] = 'a') or (_set[i] = 'b')) then
			glyph_count := glyph_count + 1.0;

		if (((_set[i] = 'A') or (_set[i] = 'B')) or (_set[i] = 'C')) then
    begin
			if (_set[i] <> last_set) then
      begin
				last_set := _set[i];
				glyph_count := glyph_count + 1.0;
			end;
		end;

		if ((_set[i] = 'C') and (reduced[i] <> '[')) then
			glyph_count := glyph_count + 0.5
    else
			glyph_count := glyph_count + 1.0;
	end;
	if(glyph_count > 80.0) then
  begin
		strcpy(symbol.errtxt, 'Input too long');
		result := ZERROR_TOO_LONG; exit;
	end;

	{ So now we know what start character to use - we can get on with it! }
	case _set[1] of
		'A': { Start A }
    begin
			concat(dest, C128Table[103]);
			values[0] := 103;
    end;
		'B': { Start B }
    begin
			concat(dest, C128Table[104]);
			values[0] := 104;
    end;
		'C': { Start C }
    begin
			concat(dest, C128Table[105]);
			values[0] := 105;
    end;
	end;
	Inc(bar_characters);

	concat(dest, C128Table[102]);
	values[1] := 102;
	Inc(bar_characters);

	{ Encode the data }
	read := 0;
	repeat
		if ((read <> 0) and (_set[read] <> _set[read - 1])) then
		begin { Latch different code set }
			case (_set[read]) of
			  'A':
        begin
				  concat(dest, C128Table[101]);
				  values[bar_characters] := 101;
				  Inc(bar_characters);
				end;
			  'B':
        begin
				  concat(dest, C128Table[100]);
				  values[bar_characters] := 100;
				  Inc(bar_characters);
				end;
			  'C':
        begin
				  concat(dest, C128Table[99]);
				  values[bar_characters] := 99;
				  Inc(bar_characters);
				end
			end;
		end;

		if ((_set[read] = 'a') or (_set[read] = 'b')) then
    begin
			{ Insert shift character }
			concat(dest, C128Table[98]);
			values[bar_characters] := 98;
			Inc(bar_characters);
		end;

		if (reduced[read] <> '[') then
    begin
			{ Encode data characters }
			case _set[read] of
				'A',
				'a':
        begin
					c128_set_a(Ord(reduced[read]), dest, values, bar_characters);
					Inc(read);
				end;
				'B',
				'b':
        begin
					c128_set_b(Ord(reduced[read]), dest, values, bar_characters);
					Inc(read);
				end;
				'C':
        begin
					c128_set_c(Ord(reduced[read]), Ord(reduced[read + 1]), dest, values, bar_characters);
					Inc(read, 2);
				end;
			end;
		end
    else
    begin
			concat(dest, C128Table[102]);
			values[bar_characters] := 102;
			Inc(bar_characters);
			Inc(read);
		end;
  until not (read < strlen(reduced));

	{ "...note that the linkage flag is an extra code set character between
	   the last data character and the Symbol Check Character"
	   (GS1 Specification) }

	{ Linkage flags in GS1-128 are determined by ISO/IEC 24723 section 7.4 }

	case symbol.option_1 of
		1,
		2:
    begin
			{ CC-A or CC-B 2D component }
			case _set[strlen(reduced )- 1] of
				'A': linkage_flag := 100;
				'B': linkage_flag := 99;
				'C': linkage_flag := 101;
			end;
    end;
		3:
    begin
			{ CC-C 2D component }
			case _set[strlen(reduced) - 1] of
				'A': linkage_flag := 99;
				'B': linkage_flag := 101;
				'C': linkage_flag := 100;
			end;
    end;
	end;

	if (linkage_flag <> 0) then
  begin
		concat(dest, C128Table[linkage_flag]);
		values[bar_characters] := linkage_flag;
		Inc(bar_characters);
	end;

	{ check digit calculation }
	total_sum := 0;
	for i := 0 to bar_characters do
  begin
		if(i > 0) then
			values[i] := values[i] * i;
		Inc(total_sum, values[i]);
	end;
	concat(dest, C128Table[total_sum mod 103]);
	values[bar_characters] := total_sum mod 103;
	Inc(bar_characters);

	{ Stop character }
	concat(dest, C128Table[106]);
	values[bar_characters] := 106;
	Inc(bar_characters);
	expand(symbol, dest);

	{ Add the separator pattern for composite symbols }
	if (symbol.symbology = BARCODE_EAN128_CC) then
  begin
		for i := 0 to symbol.width - 1 do
    begin
			if ((module_is_set(symbol, separator_row + 1, i)) = 0) then
				set_module(symbol, separator_row, i);
		end;
	end;

  symbol.text[0] := 0;
	for i := 0 to _length - 1 do
  begin
		if ((source[i] <> Ord('[')) and (source[i] <> Ord(']'))) then
			symbol.text[i] := source[i];

		if (source[i] = Ord('[')) then
			symbol.text[i] := Ord('(');

		if (source[i] = Ord(']')) then
			symbol.text[i] := Ord(')');
	end;

	result := error_number; exit;
end;

function nve_18(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
{ Add check digit if encoding an NVE18 symbol }
var
  error_number, zeroes, nve_check, total_sum, sourcelen : Integer;
  ean128_equiv : TArrayOfByte;
  i : Integer;
begin
  SetLength(ean128_equiv, 25);
  FillChar(ean128_equiv[0], 25, 0);
	sourcelen := _length;

	if (sourcelen > 17) then
  begin
		strcpy(symbol.errtxt, 'Input too long');
		result := ZERROR_TOO_LONG; exit;
  end;

	error_number := is_sane(NEON, source, _length);
	if (error_number = ZERROR_INVALID_DATA) then
  begin
		strcpy(symbol.errtxt, 'Invalid characters in data');
		result := error_number; exit;
	end;

	zeroes := 17 - sourcelen;
	ustrcpy(ean128_equiv, '[00]');
  FillChar(ean128_equiv[4], zeroes, Ord('0'));
  ean128_equiv[4 + zeroes] := 0;
  uconcat(ean128_equiv, source);

	total_sum := 0;
	for i := sourcelen - 1 downto 0 do
  begin
		Inc(total_sum, ctoi(Chr(source[i])));

		if(((sourcelen - 1 - i) and 1) = 0) then
			Inc(total_sum, 2 * ctoi(Chr(source[i])));
	end;
	nve_check := 10 - total_sum mod 10;

  if (nve_check = 10) then nve_check := 0;
  ean128_equiv[21] := Ord(itoc(nve_check));
  ean128_equiv[22] := 0;

	error_number := ean_128(symbol, ean128_equiv, ustrlen(ean128_equiv));

	result := error_number; exit;
end;

function ean_14(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
{ EAN-14 - A version of EAN-128 }
var
  count, check_digit : Integer;
  error_number, zeroes : Integer;
  ean128_equiv : TArrayOfByte;
  i : Integer;
begin
  SetLength(ean128_equiv, 20);

	if (_length > 13) then
  begin
		strcpy(symbol.errtxt, 'Input wrong length');
		result := ZERROR_TOO_LONG; exit;
	end;

	error_number := is_sane(NEON, source, _length);
	if (error_number = ZERROR_INVALID_DATA) then
  begin
		strcpy(symbol.errtxt, 'Invalid character in data');
		result := error_number; exit;
	end;

	zeroes := 13 - _length;
	ustrcpy(ean128_equiv, '[01]');
  FillChar(ean128_equiv[4], zeroes, '0');
  ean128_equiv[4 + zeroes] := 0;
  uconcat(ean128_equiv, source);

	count := 0;
	for i := _length - 1 downto 0 do
  begin
		Inc(count, ctoi(Chr(source[i])));

		if (((_length - 1 - i) and 1) = 0) then
			Inc(count, 2 * ctoi(Chr(source[i])));
	end;
	check_digit := 10 - (count mod 10);
  if (check_digit = 10) then check_digit := 0;
	ean128_equiv[17] := Ord(itoc(check_digit));
  ean128_equiv[18] := 0;

	error_number := ean_128(symbol, ean128_equiv, ustrlen(ean128_equiv));

	result := error_number; exit;
end;

end.

