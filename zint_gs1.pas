unit zint_gs1;

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
  SysUtils, zint_common, zint;

function gs1_verify(symbol : zint_symbol; source : TArrayOfByte; const src_len : Integer; var reduced : TArrayOfChar) : Integer;
function ugs1_verify(symbol : zint_symbol; source : TArrayOfByte; src_len : Integer; var reduced : TArrayOfByte) : Integer;

implementation

uses zint_helper;

{ This code does some checks on the integrity of GS1 data. It is not intended
   to be bulletproof, nor does it report very accurately what problem was found
   or where, but should prevent some of the more common encoding errors }

procedure itostr(var ai_string : TArrayOfChar; ai_value : Integer);
var
  thou, hund, ten, _unit : Integer;
  temp : TArrayOfChar;
begin
  SetLength(temp, 2);
	strcpy(ai_string, '(');
	thou := ai_value div 1000;
	hund := (ai_value - (1000 * thou)) div 100;
	ten := (ai_value - ((1000 * thou) + (100 * hund))) div 10;
	_unit := ai_value - ((1000 * thou) + (100 * hund) + (10 * ten));

  temp[1] := #0;
	if (ai_value >= 1000) then begin temp[0] := itoc(thou); concat(ai_string, temp); end;
  if (ai_value >= 100) then begin temp[0] := itoc(hund); concat(ai_string, temp); end;
  temp[0] := itoc(ten);
  concat(ai_string, temp);
  temp[0] := itoc(_unit);
  concat(ai_string, temp);
  concat(ai_string, ')');
end;

function gs1_verify(symbol : zint_symbol; source : TArrayOfByte; const src_len : Integer; var reduced : TArrayOfChar) : Integer;
var
  i, j, last_ai, ai_latch : Integer;
  ai_string : TArrayOfChar;
  bracket_level, max_bracket_level, ai_length, max_ai_length, min_ai_length : Integer;
  ai_value, ai_location, data_location, data_length : array[0..99] of Integer;
  ai_count : Integer;
  error_latch : Integer;
begin
  SetLength(ai_string, 6);
	{ Detect extended ASCII characters }
	for i := 0 to src_len - 1 do
  begin
		if Ord(source[i]) >= 128 then
    begin
      strcpy(symbol.errtxt, 'Extended ASCII characters are not supported by GS1');
			result := ZERROR_INVALID_DATA; exit;
		end;
		if Ord(source[i]) < 32 then
    begin
			strcpy(symbol.errtxt, 'Control characters are not supported by GS1');
			result := ZERROR_INVALID_DATA; exit;
		end;
	end;

	if source[0] <> Ord('[') then
  begin
		strcpy(symbol.errtxt, 'Data does not start with an AI');
		result := ZERROR_INVALID_DATA; exit;
	end;

	{ Check the position of the brackets }
	bracket_level := 0;
	max_bracket_level := 0;
	ai_length := 0;
	max_ai_length := 0;
	min_ai_length := 5;
	j := 0;
	ai_latch := 0;
	for i := 0 to src_len - 1 do
  begin
		Inc(ai_length, j);
		if (((j = 1) and (source[i] <> Ord(']'))) and ((source[i] < Ord('0')) or (source[i] > Ord('9')))) then ai_latch := 1;
		if (source[i] = Ord('[')) then begin Inc(bracket_level); j := 1; end;
		if (source[i] = Ord(']')) then
    begin
			Dec(bracket_level);
			if (ai_length < min_ai_length) then min_ai_length := ai_length;
			j := 0;
			ai_length := 0;
		end;
		if (bracket_level > max_bracket_level) then max_bracket_level := bracket_level;
		if (ai_length > max_ai_length) then max_ai_length := ai_length;
	end;
	Dec(min_ai_length);

	if (bracket_level <> 0) then
  begin
		{ Not all brackets are closed }
		strcpy(symbol.errtxt, 'Malformed AI in input data (brackets don\''t match)');
		result := ZERROR_INVALID_DATA; exit;
	end;

	if (max_bracket_level > 1) then
  begin
		{ Nested brackets }
		strcpy(symbol.errtxt, 'Found nested brackets in input data');
		result := ZERROR_INVALID_DATA; exit;
	end;

	if(max_ai_length > 4) then
  begin
		{ AI is too long }
		strcpy(symbol.errtxt, 'Invalid AI in input data (AI too long)');
		result := ZERROR_INVALID_DATA; exit;
	end;

	if(min_ai_length <= 1) then
  begin
		{ AI is too short }
		strcpy(symbol.errtxt, 'Invalid AI in input data (AI too short)');
		result := ZERROR_INVALID_DATA; exit;
	end;

	if(ai_latch = 1) then
  begin
		{ Non-numeric data in AI }
		strcpy(symbol.errtxt, 'Invalid AI in input data (non-numeric characters in AI)');
		result := ZERROR_INVALID_DATA; exit;
	end;

	ai_count := 0;
	for i := 1 to src_len - 1 do
  begin
		if (source[i - 1] = Ord('[')) then
    begin
			ai_location[ai_count] := i;
			j := 0;
			repeat
				ai_string[j] := Chr(source[i + j]);
        Inc(j)
			until not (ai_string[j - 1] <> ']');
      ai_string[j - 1] := #0;
			ai_value[ai_count] := StrToInt(ArrayOfCharToString(ai_string));
			Inc(ai_count);
		end;
	end;

	for i := 0 to ai_count - 1 do
  begin
		data_location[i] := ai_location[i] + 3;
		if (ai_value[i] >= 100) then Inc(data_location[i]);
		if (ai_value[i] >= 1000) then Inc(data_location[i]);
		data_length[i] := 0;
		repeat
			Inc(data_length[i]);
		until not ((source[data_location[i] + data_length[i] - 1] <> Ord('[')) and (source[data_location[i] + data_length[i] - 1] <> 0));
		Dec(data_length[i]);
	end;

	for i := 0 to ai_count - 1 do
  begin
		if(data_length[i] = 0) then
    begin
			{ No data for given AI }
			strcpy(symbol.errtxt, 'Empty data field in input data');
			result := ZERROR_INVALID_DATA; exit;
		end;
	end;

	error_latch := 0;
  strcpy(ai_string, '');
	for i := 0 to ai_count - 1 do
  begin
		case ai_value[i] of
			0: if(data_length[i] <> 18) then error_latch := 1;
			1,
			2,
			3: if(data_length[i] <> 14) then error_latch := 1;
			4: if(data_length[i] <> 16) then error_latch := 1;
			11,
			12,
			13,
			14,
			15,
			16,
			17,
			18,
			19: if(data_length[i] <> 6) then error_latch := 1;
			20: if(data_length[i] <> 2) then error_latch := 1;
			23,
			24,
			25,
			39,
			40,
			41,
			42,
			70,
			80,
			81: error_latch := 2;
		end;
		if (
			 ((ai_value[i] >= 100) and (ai_value[i] <= 179))
			 or ((ai_value[i] >= 1000) and (ai_value[i] <= 1799))
			 or ((ai_value[i] >= 200) and (ai_value[i] <= 229))
			 or ((ai_value[i] >= 2000) and (ai_value[i] <= 2299))
			 or ((ai_value[i] >= 300) and (ai_value[i] <= 309))
			 or ((ai_value[i] >= 3000) and (ai_value[i] <= 3099))
			 or ((ai_value[i] >= 31) and (ai_value[i] <= 36))
			 or ((ai_value[i] >= 310) and (ai_value[i] <= 369))
		) then
			error_latch := 2;
		if((ai_value[i] >= 3100) and (ai_value[i] <= 3699)) then
    begin
			if (data_length[i] <> 6) then
				error_latch := 1;
		end;
		if (
			 ((ai_value[i] >= 370) and (ai_value[i] <= 379))
			 or ((ai_value[i] >= 3700) and (ai_value[i] <= 3799))
		) then
			error_latch := 2;
		if ((ai_value[i] >= 410) and (ai_value[i] <= 415)) then
    begin
			if(data_length[i] <> 13) then
				error_latch := 1;
		end;
		if (
			 ((ai_value[i] >= 4100) and (ai_value[i] <= 4199))
			 or ((ai_value[i] >= 700) and (ai_value[i] <= 703))
			 or ((ai_value[i] >= 800) and (ai_value[i] <= 810))
			 or ((ai_value[i] >= 900) and (ai_value[i] <= 999))
			 or ((ai_value[i] >= 9000) and (ai_value[i] <= 9999))
		) then
			error_latch := 2;
		if((error_latch < 4) and (error_latch > 0)) then
    begin
			{ error has just been detected: capture AI }
			itostr(ai_string, ai_value[i]);
			Inc(error_latch, 4);
		end;
	end;

	if(error_latch = 5) then
  begin
    strcpy(symbol.errtxt, 'Invalid data _length for AI ');
    concat(symbol.errtxt, ai_string);
		result := ZERROR_INVALID_DATA; exit;
	end;

	if(error_latch = 6) then
  begin
    strcpy(symbol.errtxt, 'Invalid AI value ');
    concat(symbol.errtxt, ai_string);
		result := ZERROR_INVALID_DATA; exit;
	end;

	{ Resolve AI data - put resulting string in 'reduced' }
  j := 0;
	last_ai := 0;
	ai_latch := 1;
	for i := 0 to src_len - 1 do
  begin
		if ((source[i] <> Ord('[')) and (source[i] <> Ord(']'))) then
    begin
			reduced[j] := Chr(source[i]);
      Inc(j);
    end;
    if (source[i] = Ord('[')) then
    begin
			{ Start of an AI string }
			if(ai_latch = 0) then
      begin
				reduced[j] := '[';
        Inc(j);
      end;
			ai_string[0] := Chr(source[i + 1]);
			ai_string[1] := Chr(source[i + 2]);
      ai_string[2] := #0;
			last_ai := StrToInt(ArrayOfCharToString(ai_string));
			ai_latch := 0;
			{ The following values from "GS-1 General Specification version 8.0 issue 2, May 2008"
			figure 5.4.8.2.1 - 1 "Element Strings with Pre-Defined Length Using Application Identifiers" }
			if(
				((last_ai >= 0) and (last_ai <= 4))
				or ((last_ai >= 11) and (last_ai <= 20))
				or (last_ai = 23) { legacy support - see 5.3.8.2.2 }
				or ((last_ai >= 31) and (last_ai <= 36))
				or (last_ai = 41)
			) then
				ai_latch := 1;
		end;
		{ The ']' character is simply dropped from the input }
	end;
    reduced[j] := #0;

	{ the character '[' in the reduced string refers to the FNC1 character }
	result := 0; exit;
end;

function ugs1_verify(symbol : zint_symbol; source : TArrayOfByte; src_len : Integer; var reduced : TArrayOfByte) : Integer;
var
  temp : TArrayOfChar;
  error_number : Integer;
begin
  SetLength(temp, src_len + 5);
	error_number := gs1_verify(symbol, source, src_len, temp);
	if (error_number <> 0) then begin Result := error_number; exit; end;

	if (strlen(temp) < src_len + 5) then
  begin
    ustrcpy(reduced, ArrayOfCharToArrayOfByte(temp));
		Result := 0; exit;
  end;
	strcpy(symbol.errtxt, 'ugs1_verify overflow');
	result := ZERROR_INVALID_DATA; exit;
end;

end.

