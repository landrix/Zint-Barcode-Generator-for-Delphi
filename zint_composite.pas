unit zint_composite;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b work in progress
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, zint;

function composite(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses
  zint_common, zint_pdf417, zint_rss, zint_helper, zint_gs1, zint_code128,
  zint_upcean;

const _NUMERIC = #110;
const _ALPHA = #97;
const ISOIEC = #105;
const INVALID_CHAR = #100;
const ANY_ENC	= #120;
const _ALPHA_OR_ISO = #121;

{ CC-A component coefficients from ISO/IEC 24728:2006 Annex F }
const ccaCoeffs : array[0..29] of Integer = (
	{ k = 4 }
	522, 568, 723, 809,

	{ k = 5 }
	427, 919, 460, 155, 566,

	{ k = 6 }
	861, 285, 19, 803, 17, 766,

	{ k = 7 }
	76, 925, 537, 597, 784, 691, 437,

	{ k = 8 }
	237, 308, 436, 284, 646, 653, 428, 379
);

{ rows, error codewords, k-offset of valid CC-A sizes from ISO/IEC 24723:2006 Table 9 }
const ccaVariants : array[0..50] of Integer = (
	5, 6, 7, 8, 9, 10, 12, 4, 5, 6, 7, 8, 3, 4, 5, 6, 7,
	4, 4, 5, 5, 6, 6, 7, 4, 5, 6, 7, 7, 4, 5, 6, 7, 8,
	0, 0, 4, 4, 9, 9, 15, 0, 4, 9, 15, 15, 0, 4, 9, 15, 22
);

{ following is Left RAP, Centre RAP, Right RAP and Start Cluster from ISO/IEC 24723:2006 tables 10 and 11 }
const aRAPTable : array[0..67] of Integer = (
	39, 1, 32, 8, 14, 43, 20, 11, 1, 5, 15, 21, 40, 43, 46, 34, 29,
	0, 0, 0, 0, 0, 0, 0, 43, 33, 37, 47, 1, 20, 23, 26, 14, 9,
	19, 33, 12, 40, 46, 23, 52, 23, 13, 17, 27, 33, 52, 3, 6, 46, 41,
	6, 0, 3, 3, 3, 0, 3, 3, 0, 3, 6, 6, 0, 0, 0, 0, 3
);

type
  TGlobalpwr928 = array[0..68] of array[0..6] of Word;

function _min(first : Integer; second : Integer) : Integer;
begin
  if (first <= second) then
    result := first
  else
    result := second;
end;

{ gets bit in bitString at bitPos }
function getBit(var bitStr : TArrayOfWord; bitPos : Integer) : Integer;
begin
  if (bitStr[bitPos shr 4] and ($8000 shr (bitPos and 15))) <> 0 then
    result := 1
  else
    result := 0;
end;

{ initialize pwr928 encoding table }
procedure init928(var pwr928 : TGlobalpwr928);
var
  i, j, v : Integer;
  cw : array[0..6] of Integer;
begin
  cw[6] := 1;
  for i := 5 downto 0 do
    cw[i] := 0;

  for i := 0 to 6  do
    pwr928[0][i] := cw[i];
  for j := 1 to 68 do
  begin
    v := 0;
    for i := 6 downto 1 do
    begin
      v := (2 * cw[i]) + (v div 928);
      pwr928[j][i] := v mod 928;
      cw[i] := v mod 928;
    end;
    pwr928[j][0] := (2 * cw[0]) + (v div 928);
    cw[0] := (2 * cw[0]) + (v div 928);
  end;
end;

{ converts bit string to base 928 values, codeWords[0] is highest order }
function encode928(bitString : TArrayOfWord; var codeWords : TArrayOfWord; bitLng : Integer; var pwr928 : TGlobalpwr928) : Integer;
var
  i, j, b, bitCnt, cwNdx, cwCnt, cwLng : Integer;
begin
  cwNdx := 0; cwLng := 0; b := 0;
  while b < bitLng do
  begin
    bitCnt := _min(bitLng-b, 69);
    cwCnt := bitCnt div 10 + 1;
    Inc(cwLng, cwCnt);
    for i := 0 to cwCnt - 1 do
      codeWords[cwNdx + i] := 0; { init 0 }
    for i := 0 to bitCnt - 1 do
    begin
      if (getBit(bitString, b + bitCnt - i - 1) <> 0) then
      begin
        for j := 0 to cwCnt - 1 do
          Inc(codeWords[cwNdx + j], pwr928[i][j + 7 - cwCnt]);
      end;
    end;
    for i := cwCnt - 1 downto 1 do
    begin
      { add 'carries' }
      Inc(codeWords[cwNdx + i - 1], codeWords[cwNdx + i] div 928);
      codeWords[cwNdx + i] := codeWords[cwNdx + i] mod 928;
    end;
    Inc(b, 69);
    Inc(cwNdx, 7);
  end;
  result :=cwLng; exit;
end;

{ CC-A 2D component }
function cc_a(symbol : zint_symbol; source : TArrayOfChar; cc_width : Integer) : Integer;
var
  i, strpos, segment, bitlen, cwCnt, variant, rows : Integer;
  k, offset, j, total : Integer;
  rsCodeWords : array[0..7] of Integer;
  LeftRAPStart, RightRAPStart, CentreRAPStart, StartCluster : Integer;
  LeftRAP, RightRAP, CentreRAP, Cluster : Integer;
  dummy : array[0..4] of Integer;
  writer, flip, loop : Integer;
  codeWords, bitStr : TArrayOfWord;
  codebarre, pattern : TArrayOfChar;
  local_source : TArrayOfChar; { A copy of source but with padding zeroes to make 208 bits }
  pwr928 : TGlobalpwr928;
begin
  SetLength(codebarre, 100);
  SetLength(pattern, 580);
  SetLength(local_source, 210);
  SetLength(codeWords, 28);
  SetLength(bitStr, 13);
  variant := 0;

  for i := 0 to 12 do bitStr[i] := 0;
  for i := 0 to 27 do codeWords[i] := 0;

  bitlen := strlen(source);

  for i := 0 to 207 do local_source[i] := '0';
  for i := 0 to bitlen - 1 do local_source[i] := source[i];
  local_source[208] := #0;

  for segment := 0 to 12 do
  begin
    strpos := segment * 16;
    if (local_source[strpos] = '1') then Inc(bitStr[segment], $8000);
    if (local_source[strpos + 1] = '1') then Inc(bitStr[segment], $4000);
    if (local_source[strpos + 2] = '1') then Inc(bitStr[segment], $2000);
    if (local_source[strpos + 3] = '1') then Inc(bitStr[segment], $1000);
    if (local_source[strpos + 4] = '1') then Inc(bitStr[segment], $800);
    if (local_source[strpos + 5] = '1') then Inc(bitStr[segment], $400);
    if (local_source[strpos + 6] = '1') then Inc(bitStr[segment], $200);
    if (local_source[strpos + 7] = '1') then Inc(bitStr[segment], $100);
    if (local_source[strpos + 8] = '1') then Inc(bitStr[segment], $80);
    if (local_source[strpos + 9] = '1') then Inc(bitStr[segment], $40);
    if (local_source[strpos + 10] = '1') then Inc(bitStr[segment], $20);
    if (local_source[strpos + 11] = '1') then Inc(bitStr[segment], $10);
    if (local_source[strpos + 12] = '1') then Inc(bitStr[segment], $08);
    if (local_source[strpos + 13] = '1') then Inc(bitStr[segment], $04);
    if (local_source[strpos + 14] = '1') then Inc(bitStr[segment], $02);
    if (local_source[strpos + 15] = '1') then Inc(bitStr[segment], $01);
  end;

  init928(pwr928);
  { encode codeWords from bitStr }
  cwCnt := encode928(bitStr, codeWords, bitlen, pwr928);

  case cc_width of
    2:
      case cwCnt of
        6: variant := 0;
        8: variant := 1;
        9: variant := 2;
        11: variant := 3;
        12: variant := 4;
        14: variant := 5;
        17: variant := 6;
      end;
    3:
      case cwCnt of
        8: variant := 7;
        10: variant := 8;
        12: variant := 9;
        14: variant := 10;
        17: variant := 11;
      end;
    4:
      case cwCnt of
        8: variant := 12;
        11: variant := 13;
        14: variant := 14;
        17: variant := 15;
        20: variant := 16;
      end;
  end;

  rows := ccaVariants[variant];
  k := ccaVariants[17 + variant];
  offset := ccaVariants[34 + variant];

  { Reed-Solomon error correction }

  for i := 0 to 7 do
    rsCodeWords[i] := 0;
  total := 0;
  for i := 0 to cwCnt - 1 do
  begin
    total := (codeWords[i] + rsCodeWords[k - 1]) mod 929;
    for j := k - 1 downto 0 do
    begin
      if (j = 0) then
        rsCodeWords[j] := (929 - (total * ccaCoeffs[offset + j]) mod 929) mod 929
      else
        rsCodeWords[j] := (rsCodeWords[j - 1] + 929 - (total * ccaCoeffs[offset + j]) mod 929) mod 929;
    end;
  end;

  for j := 0 to k - 1 do
    if (rsCodeWords[j] <> 0) then rsCodeWords[j] := 929 - rsCodeWords[j];

  for i := k - 1 downto 0 do
  begin
    codeWords[cwCnt] := rsCodeWords[i];
    Inc(cwCnt);
  end;

  { Place data into table }
  LeftRAPStart := aRAPTable[variant];
  CentreRAPStart := aRAPTable[variant + 17];
  RightRAPStart := aRAPTable[variant + 34];
  StartCluster := aRAPTable[variant + 51] div 3;

  LeftRAP := LeftRAPStart;
  CentreRAP := CentreRAPStart;
  RightRAP := RightRAPStart;
  Cluster := StartCluster; { Cluster can be 0, 1 or 2 for Cluster(0), Cluster(3) and Cluster(6) }

  for i := 0 to rows - 1 do
  begin
    strcpy(codebarre, '');
    offset := 929 * Cluster;
    for j := 0 to 4 do
      dummy[j] := 0;
    for j := 0 to cc_width - 1 do
      dummy[j + 1] := codeWords[i * cc_width + j];

    { Copy the data into codebarre }
    concat(codebarre, RAPLR[LeftRAP]);
    concat(codebarre, '1');
    concat(codebarre, codagemc[offset + dummy[1]]);
    concat(codebarre, '1');
    if (cc_width = 3) then
      concat(codebarre, RAPC[CentreRAP]);

    if (cc_width >= 2) then
    begin
      concat(codebarre, '1');
      concat(codebarre, codagemc[offset + dummy[2]]);
      concat(codebarre, '1');
    end;
    if (cc_width = 4) then
      concat(codebarre, RAPC[CentreRAP]);

    if (cc_width >= 3) then
    begin
      concat(codebarre, '1');
      concat(codebarre, codagemc[offset + dummy[3]]);
      concat(codebarre, '1');
    end;
    if (cc_width = 4) then
    begin
      concat(codebarre, '1');
      concat(codebarre, codagemc[offset + dummy[4]]);
      concat(codebarre, '1');
    end;
    concat(codebarre, RAPLR[RightRAP]);
    concat(codebarre, '1'); { stop }

    { Now codebarre is a mixture of letters and numbers }

    writer := 0;
    flip := 1;
    strcpy(pattern, '');
    for loop := 0 to strlen(codebarre) - 1 do
    begin
      if ((codebarre[loop] >= '0') and (codebarre[loop] <= '9')) then
      begin
        for k := 0 to ctoi(codebarre[loop]) - 1 do
        begin
          if (flip = 0) then
            pattern[writer] := '0'
          else
            pattern[writer] := '1';
          Inc(writer);
        end;
        pattern[writer] := #0;
        if (flip = 0) then
          flip := 1
        else
          flip := 0;
      end
      else
      begin
        lookup(BRSET, PDFttf, codebarre[loop], pattern);
        Inc(writer, 5);
      end;
    end;
    symbol.width := writer;

    { so now pattern[] holds the string of '1's and '0's. - copy this to the symbol }
    for loop := 0 to strlen(pattern) - 1 do
      if (pattern[loop] = '1') then set_module(symbol, i, loop);
    symbol.row_height[i] := 2;
    Inc(symbol.rows);

    { Set up RAPs and Cluster for next row }
    Inc(LeftRAP);
    Inc(CentreRAP);
    Inc(RightRAP);
    Inc(Cluster);

    if (LeftRAP = 53) then
      LeftRAP := 1;

    if (CentreRAP = 53) then
      CentreRAP := 1;

    if (RightRAP = 53) then
      RightRAP := 1;

    if (Cluster = 3) then
      Cluster := 0;
  end;

  Result := 0; exit;
end;

{ CC-B 2D component }
function cc_b(symbol : zint_symbol; source : TArrayOfChar; cc_width : Integer) : Integer;
var
  _length, i, binloc : Integer;
  data_string : TArrayOfByte;
  chainemc : TArrayOfInteger;
  mc_length : Integer;
  k, j, longueur, offset : Integer;
  mccorrection : array[0..49] of Integer;
  total : Integer;
  dummy : array[0..4] of Integer;
  codebarre, pattern : TArrayOfChar;
  variant, LeftRAPStart, CentreRAPStart, RightRAPStart, StartCluster : Integer;
  LeftRAP, CentreRAP, RightRAP, Cluster, writer, flip, loop : Integer;
begin
  SetLength(data_string, (strlen(source) div 8) + 3);
  SetLength(chainemc, 180);
  SetLength(codebarre, 100);
  SetLength(pattern, 580);
  _length := strlen(source) div 8;

  for i := 0 to _length - 1 do
  begin
    binloc := i * 8;

    data_string[i] := 0;
    if (source[binloc] = '1') then Inc(data_string[i], $80);
    if (source[binloc + 1] = '1') then Inc(data_string[i], $40);
    if (source[binloc + 2] = '1') then Inc(data_string[i], $20);
    if (source[binloc + 3] = '1') then Inc(data_string[i], $10);
    if (source[binloc + 4] = '1') then Inc(data_string[i], $08);
    if (source[binloc + 5] = '1') then Inc(data_string[i], $04);
    if (source[binloc + 6] = '1') then Inc(data_string[i], $02);
    if (source[binloc + 7] = '1') then Inc(data_string[i], $01);
  end;


  mc_length := 0;

  { 'the CC-B component shall have codeword 920 in the first symbol character position' (section 9a) }
  chainemc[mc_length] := 920;
  Inc(mc_length);

  byteprocess(chainemc, mc_length, data_string, 0, _length, 0);

  { Now figure out which variant of the symbol to use and load values accordingly }

  variant := 0;

  if (cc_width = 2) then
  begin
    variant := 13;
    if (mc_length <= 33) then variant := 12;
    if (mc_length <= 29) then variant := 11;
    if (mc_length <= 24) then variant := 10;
    if (mc_length <= 19) then variant := 9;
    if (mc_length <= 13) then variant := 8;
    if (mc_length <= 8) then variant := 7;
  end;

  if (cc_width = 3) then
  begin
    variant := 23;
    if (mc_length <= 70) then variant := 22;
    if (mc_length <= 58) then variant := 21;
    if (mc_length <= 46) then variant := 20;
    if (mc_length <= 34) then variant := 19;
    if (mc_length <= 24) then variant := 18;
    if (mc_length <= 18) then variant := 17;
    if (mc_length <= 14) then variant := 16;
    if (mc_length <= 10) then variant := 15;
    if (mc_length <= 6) then variant := 14;
  end;

  if (cc_width = 4) then
  begin
    variant := 34;
    if (mc_length <= 108) then variant := 33;
    if (mc_length <= 90) then variant := 32;
    if (mc_length <= 72) then variant := 31;
    if (mc_length <= 54) then variant := 30;
    if (mc_length <= 39) then variant := 29;
    if (mc_length <= 30) then variant := 28;
    if (mc_length <= 24) then variant := 27;
    if (mc_length <= 18) then variant := 26;
    if (mc_length <= 12) then variant := 25;
    if (mc_length <= 8) then variant := 24;
  end;

  { Now we have the variant we can load the data - from here on the same as MicroPDF417 code }
  Dec(variant);
  symbol.option_2 := MicroVariants[variant]; { columns }
  symbol.rows := MicroVariants[variant + 34]; { rows }
  k := MicroVariants[variant + 68]; { number of EC CWs }
  longueur := (symbol.option_2 * symbol.rows) - k; { number of non-EC CWs }
  i := longueur - mc_length; { amount of padding required }
  offset := MicroVariants[variant + 102]; { coefficient offset }

  { We add the padding }
  while (i > 0) do
  begin
    chainemc[mc_length] := 900;
    Inc(mc_length);
    Dec(i);
  end;

  { Reed-Solomon error correction }
  longueur := mc_length;
  for loop := 0 to 49 do
    mccorrection[loop] := 0;

  total := 0;
  for i := 0 to longueur - 1 do
  begin
    total := (chainemc[i] + mccorrection[k - 1]) mod 929;
    for j := k - 1 downto 0 do
    begin
      if (j = 0) then
        mccorrection[j] := (929 - (total * Microcoeffs[offset + j]) mod 929) mod 929
      else
        mccorrection[j] := (mccorrection[j - 1] + 929 - (total * Microcoeffs[offset + j]) mod 929) mod 929;
    end;
  end;

  for j := 0 to k - 1 do
    if (mccorrection[j] <> 0) then mccorrection[j] := 929 - mccorrection[j];

  { we add these codes to the string }
  for i := k - 1 downto 0 do
  begin
    chainemc[mc_length] := mccorrection[i];
    Inc(mc_length);
  end;

  { Now get the RAP (Row Address Pattern) start values }
  LeftRAPStart := RAPTable[variant];
  CentreRAPStart := RAPTable[variant + 34];
  RightRAPStart := RAPTable[variant + 68];
  StartCluster := RAPTable[variant + 102] div 3;

  { That's all values loaded, get on with the encoding }

  LeftRAP := LeftRAPStart;
  CentreRAP := CentreRAPStart;
  RightRAP := RightRAPStart;
  Cluster := StartCluster; { Cluster can be 0, 1 or 2 for Cluster(0), Cluster(3) and Cluster(6) }

  for i := 0 to symbol.rows - 1 do
  begin
    strcpy(codebarre, '');
    offset := 929 * Cluster;
    for j := 0 to 4 do
      dummy[j] := 0;

    for j := 0 to symbol.option_2 - 1 do
      dummy[j + 1] := chainemc[i * symbol.option_2 + j];

    { Copy the data into codebarre }
    concat(codebarre, RAPLR[LeftRAP]);
    concat(codebarre, '1');
    concat(codebarre, codagemc[offset + dummy[1]]);
    concat(codebarre, '1');
    if (cc_width = 3) then
      concat(codebarre, RAPC[CentreRAP]);

    if (cc_width >= 2) then
    begin
      concat(codebarre, '1');
      concat(codebarre, codagemc[offset + dummy[2]]);
      concat(codebarre, '1');
    end;
    if (cc_width = 4) then
      concat(codebarre, RAPC[CentreRAP]);

    if (cc_width >= 3) then
    begin
      concat(codebarre, '1');
      concat(codebarre, codagemc[offset + dummy[3]]);
      concat(codebarre, '1');
    end;
    if (cc_width = 4) then
    begin
      concat(codebarre, '1');
      concat(codebarre, codagemc[offset + dummy[4]]);
      concat(codebarre, '1');
    end;
    concat(codebarre, RAPLR[RightRAP]);
    concat(codebarre, '1'); { stop }

    { Now codebarre is a mixture of letters and numbers }

    writer := 0;
    flip := 1;
    strcpy(pattern, '');
    for loop := 0 to strlen(codebarre) - 1 do
    begin
      if ((codebarre[loop] >= '0') and (codebarre[loop] <= '9')) then
      begin
        for k := 0 to ctoi(codebarre[loop]) - 1 do
        begin
          if (flip = 0) then
            pattern[writer] := '0'
          else
            pattern[writer] := '1';
          Inc(writer);
        end;
        pattern[writer] := #0;
        if (flip = 0) then
          flip := 1
        else
          flip := 0;

      end
      else
      begin
        lookup(BRSET, PDFttf, codebarre[loop], pattern);
        Inc(writer, 5);
      end;
    end;
    symbol.width := writer;

    { so now pattern[] holds the string of '1's and '0's. - copy this to the symbol }
    for loop := 0 to strlen(pattern) - 1 do
      if (pattern[loop] = '1') then set_module(symbol, i, loop);

    symbol.row_height[i] := 2;

    { Set up RAPs and Cluster for next row }
    Inc(LeftRAP);
    Inc(CentreRAP);
    Inc(RightRAP);
    Inc(Cluster);

    if (LeftRAP = 53) then
      LeftRAP := 1;

    if (CentreRAP = 53) then
      CentreRAP := 1;

    if (RightRAP = 53) then
      RightRAP := 1;

    if (Cluster = 3) then
      Cluster := 0;

  end;

  result := 0; exit;
end;

{ CC-C 2D component - byte compressed PDF417 }
function cc_c(symbol : zint_symbol; source : TArrayOfChar; cc_width : Integer; ecc_level : Integer) : Integer;
var
  _length, i, binloc : Integer;
  data_string : TArrayOfByte;
  chainemc : TArrayOfInteger;
  mc_length, k : Integer;
  offset, longueur, loop, total, j : Integer;
  mccorrection : array[0..519] of Integer;
  c1, c2, c3 : Integer;
  dummy : array[0..34] of Integer;
  codebarre, pattern : TArrayOfChar;
begin
  SetLength(data_string, (strlen(source) div 8) + 4);
  SetLength(chainemc, 1000);
  SetLength(codebarre, 100);
  SetLength(pattern, 580);
  _length := strlen(source) div 8;

  for i := 0 to _length - 1 do
  begin
    binloc := i * 8;

    data_string[i] := 0;
    if (source[binloc] = '1') then Inc(data_string[i], $80);
    if (source[binloc + 1] = '1') then Inc(data_string[i], $40);
    if (source[binloc + 2] = '1') then Inc(data_string[i], $20);
    if (source[binloc + 3] = '1') then Inc(data_string[i], $10);
    if (source[binloc + 4] = '1') then Inc(data_string[i], $08);
    if (source[binloc + 5] = '1') then Inc(data_string[i], $04);
    if (source[binloc + 6] = '1') then Inc(data_string[i], $02);
    if (source[binloc + 7] = '1') then Inc(data_string[i], $01);
  end;

  mc_length := 0;

  chainemc[mc_length] := 0; { space for _length descriptor }
  Inc(mc_length);
  chainemc[mc_length] := 920; { CC-C identifier }
  Inc(mc_length);

  byteprocess(chainemc, mc_length, data_string, 0, _length, 0);

  chainemc[0] := mc_length;

  k := 1;
  for i := 1 to (ecc_level + 1) do
    k := k * 2;

  { 796 - we now take care of the Reed Solomon codes }
  case ecc_level of
    1: offset := 2;
    2: offset := 6;
    3: offset := 14;
    4: offset := 30;
    5: offset := 62;
    6: offset := 126;
    7: offset := 254;
    8: offset := 510;
    else offset := 0;
  end;

  longueur := mc_length;
  for loop := 0 to 519 do
    mccorrection[loop] := 0;

  total := 0;
  for i := 0 to longueur - 1 do
  begin
    total := (chainemc[i] + mccorrection[k - 1]) mod 929;
    for j := k - 1 downto 0 do
    begin
      if (j = 0) then
        mccorrection[j] := (929 - (total * coefrs[offset + j]) mod 929) mod 929
      else
        mccorrection[j] := (mccorrection[j - 1] + 929 - (total * coefrs[offset + j]) mod 929) mod 929;
    end;
  end;

  for j := 0 to k - 1 do
    if (mccorrection[j] <> 0) then mccorrection[j] := 929 - mccorrection[j];

  { we add these codes to the string }
  for i := k - 1 downto 0 do
  begin
    chainemc[mc_length] := mccorrection[i];
    Inc(mc_length);
  end;

  { 818 - The CW string is finished }
  c1 := (mc_length div cc_width - 1) div 3;
  c2 := ecc_level * 3 + (mc_length div cc_width - 1) mod 3;
  c3 := cc_width - 1;

  { we now encode each row }
  for i := 0 to (mc_length div cc_width) - 1 do
  begin
    for j := 0 to cc_width - 1 do
      dummy[j + 1] := chainemc[i * cc_width + j];

    k := (i div 3) * 30;
    case i mod 3 of
      { follows this pattern from US Patent 5,243,655:
      Row 0: L0 (row #, # of rows)         R0 (row #, # of columns)
      Row 1: L1 (row #, security level)    R1 (row #, # of rows)
      Row 2: L2 (row #, # of columns)      R2 (row #, security level)
      Row 3: L3 (row #, # of rows)         R3 (row #, # of columns)
      etc. }
      0:
      begin
        dummy[0] := k + c1;
        dummy[cc_width + 1] := k + c3;
      end;
      1:
      begin
        dummy[0] := k + c2;
        dummy[cc_width + 1] := k + c1;
      end;
      2:
      begin
        dummy[0] := k + c3;
        dummy[cc_width + 1] := k + c2;
      end;
    end;
    strcpy(codebarre, '+*'); { Start with a start char and a separator }

    for j := 0 to cc_width + 1 do
    begin
      case i mod 3 of
        1: offset := 929; { cluster(3) }
        2: offset := 1858; { cluster(6) }
        else
          offset := 0; { cluster(0) }
      end;
      concat(codebarre, codagemc[offset + dummy[j]]);
      concat(codebarre, '*');
    end;
    concat(codebarre, '-');

    strcpy(pattern, '');
    for loop := 0 to strlen(codebarre) - 1 do
      lookup(BRSET, PDFttf, codebarre[loop], pattern);

    for loop := 0 to strlen(pattern) - 1 do
      if (pattern[loop] = '1') then set_module(symbol, i, loop);

    symbol.row_height[i] := 3;
  end;
  symbol.rows := (mc_length div cc_width);
  symbol.width := strlen(pattern);

  result := 0; exit;
end;

{ Handles all data encodation from section 5 of ISO/IEC 24723 }
function cc_binary_string(symbol : zint_symbol; const source : TArrayOfChar; var binary_string : TArrayOfChar; cc_mode : Integer; var cc_width : Integer; var ecc : Integer; lin_width : Integer) : Integer;
var
  encoding_method, read_posn, d1, d2, value, alpha_pad : Integer;
  i, j, mask, ai_crop, fnc1_latch : Integer;
  group_val : Integer;
  ai90_mode, latch, remainder, binary_length : Integer;
  date_str : String;
  general_field, general_field_type : TArrayOfChar;
  target_bitsize : Integer;
  ninety : TArrayOfChar;
  numeric_part : String;
  alpha, alphanum, numeric, test1, test2, test3, next_ai_posn : Integer;
  numeric_value, table3_letter : Integer;
  byte_length, codewords_used, ecc_level, ecc_codewords, rows : Integer;
  codewords_total, target_codewords, target_bytesize : Integer;
begin
  SetLength(general_field, strlen(source) + 1);
  SetLength(general_field_type, strlen(source) + 1);
  SetLength(ninety, strlen(source) + 1);
  encoding_method := 1;
  read_posn := 0;
  ai_crop := 0;
  fnc1_latch := 0;
  alpha_pad := 0;
  ai90_mode := 0;
  ecc := 0;
  value := 0;
  target_bitsize := 0;

  if ((source[0] = '1') and ((source[1] = '0') or (source[1] = '1') or (source[1] = '7')) and (strlen(source) > 8)) then
    { Source starts (10), (11) or (17) }
    encoding_method := 2;

  if ((source[0] = '9') and (source[1] = '0')) then
    { Source starts (90) }
    encoding_method := 3;

  if (encoding_method = 1) then
    concat(binary_string, '0');


  if (encoding_method = 2) then
  begin
    { Encoding Method field '10' - date and lot number }

    concat(binary_string, '10');

    if (source[1] = '0') then
    begin
      { No date data }
      concat(binary_string, '11');
      read_posn := 2;
    end
    else
    begin
      { Production Date (11) or Expiration Date (17) }
      date_str := source[2] + source[3];
      group_val := StrToInt(date_str) * 384;

      date_str := source[4] + source[5];
      Inc(group_val, (StrToInt(date_str) - 1) * 32);

      date_str := source[6] + source[7];
      Inc(group_val, StrToInt(date_str));

      mask := $8000;
      for j := 0 to 15 do
      begin
        if ((group_val and mask) = $00) then
          concat(binary_string, '0')
        else
          concat(binary_string, '1');

        mask := mask shr 1;
      end;

      if (source[1] = '1') then
        { Production Date AI 11 }
        concat(binary_string, '0')
      else
        { Expiration Date AI 17 }
        concat(binary_string, '1');

      read_posn := 8;
    end;

    if ((source[read_posn] = '1') and (source[read_posn + 1] = '0')) then
      { Followed by AI 10 - strip this from general field }
      Inc(read_posn, 2)
    else
      { An FNC1 character needs to be inserted in the general field }
      fnc1_latch := 1;
  end;

  if (encoding_method = 3) then
  begin
    { Encodation Method field of '11' - AI 90 }

    { 'This encodation method may be used if an element string with an AI
    90 occurs at the start of the data message, and if the data field
    following the two-digit AI 90 starts with an alphanumeric string which
    complies with a specific format.' (para 5.2.2) }

    i := 0;
    repeat
      ninety[i] := source[i + 2];
      Inc(i);
    until not ((strlen(source) > i + 2 ) and ('[' <> source[i + 2]));
    ninety[i] := #0;

    { Find out if the AI 90 data is alphabetic or numeric or both }

    alpha := 0;
    alphanum := 0;
    numeric := 0;

    for i := 0 to strlen(ninety) - 1 do
    begin
      if ((ninety[i] >= 'A') and (ninety[i] <= 'Z')) then
        { Character is alphabetic }
        Inc(alpha);

      if ((ninety[i] >= '0') and (ninety[i] <= '9')) then
        { Character is numeric }
        Inc(numeric);

      case ninety[i] of
        '*',
        ',',
        '-',
        '.',
        '/': Inc(alphanum);
      end;

      if (not (((ninety[i] >= '0') and (ninety[i] <= '9')) or ((ninety[i] >= 'A') and (ninety[i] <= 'Z')))) then
      begin
        if ((ninety[i] <> '*') and (ninety[i] <> ',') and (ninety[i] <> '-') and (ninety[i] <> '.') and (ninety[i] <> '/')) then
        begin
          { An Invalid AI 90 character }
          strcpy(symbol.errtxt, 'Invalid AI 90 data');
          result := ZERROR_INVALID_DATA; exit;
        end;
      end;
    end;

    { must start with 0, 1, 2 or 3 digits followed by an uppercharacter }
    test1 := -1;
    for i := 3 downto 0 do
    begin
      if ((ninety[i] >= 'A') and (ninety[i] <= 'Z')) then
        test1 := i;
    end;

    test2 := 0;
    for i := 0 to test1 - 1 do
    begin
      if (not ((ninety[i] >= '0') and (ninety[i] <= '9'))) then
        test2 := 1;
    end;

    { leading zeros are not permitted }
    test3 := 0;
    if ((test1 >= 1) and (ninety[0] = '0')) then test3 := 1;

    if ((test1 <> -1) and (test2 <> 1) and (test3 = 0)) then
    begin
      { Encodation method '11' can be used }
      concat(binary_string, '11');

      Dec(numeric, test1);
      Dec(alpha);

      { Decide on numeric, alpha or alphanumeric mode }
      { Alpha mode is a special mode for AI 90 }

      if (alphanum > 0) then
      begin
        { Alphanumeric mode }
        concat(binary_string, '0');
        ai90_mode := 1;
      end
      else
      begin
        if (alpha > numeric) then
        begin
          { Alphabetic mode }
          concat(binary_string, '11');
          ai90_mode := 2;
        end
        else
        begin
          { Numeric mode }
          concat(binary_string, '10');
          ai90_mode := 3;
        end;
      end;

      next_ai_posn := 2 + strlen(ninety);

      if (source[next_ai_posn + 1] = '[') then
      begin
        { There are more AIs afterwords }
        if ((source[next_ai_posn + 1] = '2') and (source[next_ai_posn + 2] = '1')) then
          { AI 21 follows }
          ai_crop := 1;

        if ((source[next_ai_posn + 1] = '8') and (source[next_ai_posn + 2] = '0') and (source[next_ai_posn + 3] = '0') and (source[next_ai_posn + 4] = '4')) then
          { AI 8004 follows }
          ai_crop := 2;
      end;

      case ai_crop of
        0: concat(binary_string, '0');
        1: concat(binary_string, '10');
        2: concat(binary_string, '11');
      end;

      if (test1 = 0) then
        numeric_part := '0'
      else
      begin
        for i := 0 to test1 - 1 do
          numeric_part := numeric_part + ninety[i];
      end;

      numeric_value := StrToInt(numeric_part);

      table3_letter := -1;
      if (numeric_value < 31) then
      begin
        case ninety[test1] of
          'B': table3_letter := 0;
          'D': table3_letter := 1;
          'H': table3_letter := 2;
          'I': table3_letter := 3;
          'J': table3_letter := 4;
          'K': table3_letter := 5;
          'L': table3_letter := 6;
          'N': table3_letter := 7;
          'P': table3_letter := 8;
          'Q': table3_letter := 9;
          'R': table3_letter := 10;
          'S': table3_letter := 11;
          'T': table3_letter := 12;
          'V': table3_letter := 13;
          'W': table3_letter := 14;
          'Z': table3_letter := 15;
        end;
      end;

      if (table3_letter <> -1) then
      begin
        { Encoding can be done according to 5.2.2 c) 2) }
        { five bit binary string representing value before letter }
        mask := $10;
        for j := 0 to 4 do
        begin
          if ((numeric_value and mask) = $00) then
            concat(binary_string, '0')
          else
            concat(binary_string, '1');

          mask := mask shr 1;
        end;

        { followed by four bit representation of letter from Table 3 }
        mask := $08;
        for j := 0 to 3 do
        begin
          if ((table3_letter and mask) = $00) then
            concat(binary_string, '0')
          else
            concat(binary_string, '1');

          mask := mask shr 1;
        end;
      end
      else
      begin
        { Encoding is done according to 5.2.2 c) 3) }
        concat(binary_string, '11111');
        { ten bit representation of number }
        mask := $200;
        for j := 0 to 9 do
        begin
          if ((numeric_value and mask) = $00) then
            concat(binary_string, '0')
          else
            concat(binary_string, '1');

          mask := mask shr 1;
        end;

        { five bit representation of ASCII character }
        mask := $10;
        for j := 0 to 4 do
        begin
          if (((Ord(ninety[test1]) - 65) and mask) = $00) then
            concat(binary_string, '0')
          else
            concat(binary_string, '1');

          mask := mask shr 1;
        end;
      end;

      read_posn := test1 + 3;
    end
    else
    begin
      { Use general field encodation instead }
      concat(binary_string, '0');
      read_posn := 0;
    end;
  end;

  { Now encode the rest of the AI 90 data field }
  if (ai90_mode = 2) then
  begin
    { Alpha encodation (section 5.2.3) }
    repeat
      if ((source[read_posn] >= '0') and (source[read_posn] <= '9')) then
      begin
        mask := $10;
        for j := 0 to 4 do
        begin
          if (((Ord(source[read_posn + 1]) + 4) and mask) = $00) then
            concat(binary_string, '0')
          else
            concat(binary_string, '1');
          mask := mask shr 1;
        end;
      end;

      if ((source[read_posn] >= 'A') and (source[read_posn] <= 'Z')) then
      begin
        mask := $20;
        for j := 0 to 5 do
        begin
          if (((Ord(source[read_posn]) - 65) and mask) = $00) then
            concat(binary_string, '0')
          else
            concat(binary_string, '1');

          mask := mask shr 1;
        end;
      end;

      if (source[read_posn] = '[') then
        concat(binary_string, '11111');

      Inc(read_posn);
    until not ((source[read_posn - 1] <> '[') and (source[read_posn - 1] <> #0));
    alpha_pad := 1; { This is overwritten if a general field is encoded }
  end;

  if (ai90_mode = 1) then
  begin
    { Alphanumeric mode }
    repeat
      if ((source[read_posn] >= '0') and (source[read_posn] <= '9')) then
      begin
        mask := $10;
        for j := 0 to 4 do
        begin
          if (((Ord(source[read_posn]) - 43) and mask) = $00) then
            concat(binary_string, '0')
          else
            concat(binary_string, '1');

          mask := mask shr 1;
        end;
      end;

      if ((source[read_posn] >= 'A') and (source[read_posn] <= 'Z')) then
      begin
        mask := $20;
        for j := 0 to 5 do
        begin
          if (((Ord(source[read_posn]) - 33) and mask) = $00) then
            concat(binary_string, '0')
          else
            concat(binary_string, '1');

          mask := mask shr 1;
        end;
      end;

      case source[read_posn] of
        '[': concat(binary_string, '01111');
        '*': concat(binary_string, '111010');
        ',': concat(binary_string, '111011');
        '-': concat(binary_string, '111100');
        '.': concat(binary_string, '111101');
        '/': concat(binary_string, '111110');
      end;

      Inc(read_posn);
    until not ((source[read_posn - 1] <> '[') and (source[read_posn - 1] <> #0));
  end;

  Inc(read_posn, (2 * ai_crop));

  { The compressed data field has been processed if appropriate - the
  rest of the data (if any) goes into a general-purpose data compaction field }

  j := 0;
  if (fnc1_latch = 1) then
  begin
    { Encodation method '10' has been used but it is not followed by
       AI 10, so a FNC1 character needs to be added }
    general_field[j] := '[';
    Inc(j);
  end;

  for i := read_posn to strlen(source) - 1 do
  begin
    general_field[j] := source[i];
    Inc(j);
  end;
  general_field[j] := #0;

  if (strlen(general_field) <> 0) then alpha_pad := 0;

  latch := 0;
  for i := 0 to strlen(general_field) - 1 do
  begin
    { Table 13 - ISO/IEC 646 encodation }
    if ((general_field[i] < ' ') or (general_field[i] > 'z')) then
    begin
      general_field_type[i] := INVALID_CHAR; latch := 1;
    end
    else
      general_field_type[i] := ISOIEC;

    if (general_field[i] = '#') then
    begin
      general_field_type[i] := INVALID_CHAR; latch := 1;
    end;
    if (general_field[i] = '$') then
    begin
      general_field_type[i] := INVALID_CHAR; latch := 1;
    end;
    if (general_field[i] = '@') then
    begin
      general_field_type[i] := INVALID_CHAR; latch := 1;
    end;
    if (general_field[i] = #92) then
    begin
      general_field_type[i] := INVALID_CHAR; latch := 1;
    end;
    if (general_field[i] = '^') then
    begin
      general_field_type[i] := INVALID_CHAR; latch := 1;
    end;
    if (general_field[i] = #96) then
    begin
      general_field_type[i] := INVALID_CHAR; latch := 1;
    end;

    { Table 12 - Alphanumeric encodation }
    if ((general_field[i] >= 'A') and (general_field[i] <= 'Z')) then
      general_field_type[i] := _ALPHA_OR_ISO;

    if (general_field[i] = '*') then
      general_field_type[i] := _ALPHA_OR_ISO;

    if (general_field[i] = ',') then
      general_field_type[i] := _ALPHA_OR_ISO;

    if (general_field[i] = '-') then
      general_field_type[i] := _ALPHA_OR_ISO;

    if (general_field[i] = '.') then
      general_field_type[i] := _ALPHA_OR_ISO;

    if (general_field[i] = '/') then
      general_field_type[i] := _ALPHA_OR_ISO;

    { Numeric encodation }
    if ((general_field[i] >= '0') and (general_field[i] <= '9')) then
      general_field_type[i] := ANY_ENC;

    if (general_field[i] = '[') then
      { FNC1 can be encoded in any system }
      general_field_type[i] := ANY_ENC;
  end;

  general_field_type[strlen(general_field)] := #0;

  if (latch = 1) then
  begin
    { Invalid characters in input data }
    strcpy(symbol.errtxt, 'Invalid characters in input data');
    result := ZERROR_INVALID_DATA; exit;
  end;

  for i := 0 to strlen(general_field) - 1 do
  begin
    if ((general_field_type[i] = ISOIEC) and (general_field[i + 1] = '[')) then
      general_field_type[i + 1] := ISOIEC;
  end;

  for i := 0 to strlen(general_field) - 1 do
  begin
    if ((general_field_type[i] = _ALPHA_OR_ISO) and (general_field[i + 1] = '[')) then
      general_field_type[i + 1] := _ALPHA_OR_ISO;
  end;

  latch := general_rules(general_field, general_field_type);

  i := 0;
  repeat
    case general_field_type[i] of
      _NUMERIC:
      begin
        if (i <> 0) then
        begin
          if ((general_field_type[i - 1] <> _NUMERIC) and (general_field[i - 1] <> '[')) then
            concat(binary_string, '000'); { Numeric latch }
        end;

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
          if ((value and mask) = $00) then
            concat(binary_string, '0')
          else
            concat(binary_string, '1');

          mask := mask shr 1;
        end;

        Inc(i, 2);
      end;
      _ALPHA:
      begin
        if (i <> 0) then
        begin
          if ((general_field_type[i - 1] = _NUMERIC) or (general_field[i - 1] = '[')) then
            concat(binary_string, '0000'); { Alphanumeric latch }
          if (general_field_type[i - 1] = ISOIEC) then
            concat(binary_string, '00100'); { ISO/IEC 646 latch }
        end;

        if ((general_field[i] >= '0') and (general_field[i] <= '9')) then
        begin
          value := Ord(general_field[i]) - 43;

          mask := $10;
          for j := 0 to 4 do
          begin
            if ((value and mask) = $00) then
              concat(binary_string, '0')
            else
              concat(binary_string, '1');
            mask := mask shr 1;
          end;
        end;

        if ((general_field[i] >= 'A') and (general_field[i] <= 'Z')) then
        begin
          value := Ord(general_field[i]) - 33;

          mask := $20;
          for j := 0 to 5 do
          begin
            if ((value and mask) = $00) then
              concat(binary_string, '0')
            else
              concat(binary_string, '1');
            mask := mask shr 1;
          end;
        end;

        if (general_field[i] = '[') then concat(binary_string, '01111'); { FNC1/Numeric latch }
        if (general_field[i] = '*') then concat(binary_string, '111010'); { asterisk }
        if (general_field[i] = ',') then concat(binary_string, '111011'); { comma }
        if (general_field[i] = '-') then concat(binary_string, '111100'); { minus or hyphen }
        if (general_field[i] = '.') then concat(binary_string, '111101'); { period or full stop }
        if (general_field[i] = '/') then concat(binary_string, '111110'); { slash or solidus }

        Inc(i);
      end;
      ISOIEC:
      begin
        if (i <> 0) then
        begin
          if ((general_field_type[i - 1] = _NUMERIC) or (general_field[i - 1] = '[')) then
          begin
            concat(binary_string, '0000'); { Alphanumeric latch }
            concat(binary_string, '00100'); { ISO/IEC 646 latch }
          end;
          if (general_field_type[i - 1] = _ALPHA) then
            concat(binary_string, '00100'); { ISO/IEC 646 latch }
        end;

        if ((general_field[i] >= '0') and (general_field[i] <= '9')) then
        begin
          value := Ord(general_field[i]) - 43;

          mask := $10;
          for j := 0 to 4 do
          begin
            if ((value and mask) = $00) then
              concat(binary_string, '0')
            else
              concat(binary_string, '1');
            mask := mask shr 1;
          end;
        end;

        if ((general_field[i] >= 'A') and (general_field[i] <= 'Z')) then
        begin
          value := Ord(general_field[i]) - 1;

          mask := $40;
          for j := 0 to 6 do
          begin
            if ((value and mask) = $00) then
              concat(binary_string, '0')
            else
              concat(binary_string, '1');
            mask := mask shr 1;
          end;
        end;

        if ((general_field[i] >= 'a') and (general_field[i] <= 'z')) then
        begin
          value := Ord(general_field[i]) - 7;

          mask := $40;
          for j := 0 to 6 do
          begin
            if ((value and mask) = $00) then
              concat(binary_string, '0')
            else
              concat(binary_string, '1');

            mask := mask shr 1;
          end;
        end;

        if (general_field[i] = '[') then concat(binary_string, '01111'); { FNC1/Numeric latch }
        if (general_field[i] = '!') then concat(binary_string, '11101000'); { exclamation mark }
        if (general_field[i] = #34) then concat(binary_string, '11101001'); { quotation mark }
        if (general_field[i] = #37) then concat(binary_string, '11101010'); { percent sign }
        if (general_field[i] = '&') then concat(binary_string, '11101011'); { ampersand }
        if (general_field[i] = #39) then concat(binary_string, '11101100'); { apostrophe }
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

  binary_length := strlen(binary_string);
  if (cc_mode = 1) then
  begin
    { CC-A 2D component - calculate remaining space }
    case cc_width of
      2:
      begin
        if (binary_length > 167) then begin result := ZERROR_TOO_LONG; exit; end;
        if (binary_length <= 167) then target_bitsize := 167;
        if (binary_length <= 138) then target_bitsize := 138;
        if (binary_length <= 118) then target_bitsize := 118;
        if (binary_length <= 108) then target_bitsize := 108;
        if (binary_length <= 88) then target_bitsize := 88;
        if (binary_length <= 78) then target_bitsize := 78;
        if (binary_length <= 59) then target_bitsize := 59;
      end;
      3:
      begin
        if (binary_length > 167) then begin result := ZERROR_TOO_LONG; exit; end;
        if (binary_length <= 167) then target_bitsize := 167;
        if (binary_length <= 138) then target_bitsize := 138;
        if (binary_length <= 118) then target_bitsize := 118;
        if (binary_length <= 98) then target_bitsize := 98;
        if (binary_length <= 78) then target_bitsize := 78;
      end;
      4:
      begin
        if (binary_length > 197) then begin result := ZERROR_TOO_LONG; exit; end;
        if (binary_length <= 197) then target_bitsize := 197;
        if (binary_length <= 167) then target_bitsize := 167;
        if (binary_length <= 138) then target_bitsize := 138;
        if (binary_length <= 108) then target_bitsize := 108;
        if (binary_length <= 78) then target_bitsize := 78;
      end;
    end;
  end;

  if (cc_mode = 2) then
  begin
    { CC-B 2D component - calculated from ISO/IEC 24728 Table 1  }
    case (cc_width) of
      2:
      begin
        if (binary_length > 336) then begin result := ZERROR_TOO_LONG; exit; end;
        if (binary_length <= 336) then target_bitsize := 336;
        if (binary_length <= 296) then target_bitsize := 296;
        if (binary_length <= 256) then target_bitsize := 256;
        if (binary_length <= 208) then target_bitsize := 208;
        if (binary_length <= 160) then target_bitsize := 160;
        if (binary_length <= 104) then target_bitsize := 104;
        if (binary_length <= 56) then target_bitsize := 56;
      end;
      3:
      begin
        if (binary_length > 768) then begin result := ZERROR_TOO_LONG; exit; end;
        if (binary_length <= 768) then target_bitsize := 768;
        if (binary_length <= 648) then target_bitsize := 648;
        if (binary_length <= 536) then target_bitsize := 536;
        if (binary_length <= 416) then target_bitsize := 416;
        if (binary_length <= 304) then target_bitsize := 304;
        if (binary_length <= 208) then target_bitsize := 208;
        if (binary_length <= 152) then target_bitsize := 152;
        if (binary_length <= 112) then target_bitsize := 112;
        if (binary_length <= 72) then target_bitsize := 72;
        if (binary_length <= 32) then target_bitsize := 32;
      end;
      4:
      begin
        if (binary_length > 1184) then begin result := ZERROR_TOO_LONG; exit; end;
        if (binary_length <= 1184) then target_bitsize := 1184;
        if (binary_length <= 1016) then target_bitsize := 1016;
        if (binary_length <= 840) then target_bitsize := 840;
        if (binary_length <= 672) then target_bitsize := 672;
        if (binary_length <= 496) then target_bitsize := 496;
        if (binary_length <= 352) then target_bitsize := 352;
        if (binary_length <= 264) then target_bitsize := 264;
        if (binary_length <= 208) then target_bitsize := 208;
        if (binary_length <= 152) then target_bitsize := 152;
        if (binary_length <= 96) then target_bitsize := 96;
        if (binary_length <= 56) then target_bitsize := 56;
      end;
    end;
  end;

  if (cc_mode = 3) then
  begin
    { CC-C 2D Component is a bit more complex! }
    byte_length := binary_length div 8;
    if (binary_length mod 8 <> 0) then Inc(byte_length);

    codewords_used := (byte_length div 6) * 5;
    Inc(codewords_used, byte_length mod 6);

    ecc_level := 7;
    if (codewords_used <= 1280) then ecc_level := 6;
    if (codewords_used <= 640) then ecc_level := 5;
    if (codewords_used <= 320) then ecc_level := 4;
    if (codewords_used <= 160) then ecc_level := 3;
    if (codewords_used <= 40) then ecc_level := 2;
    ecc := ecc_level;
    ecc_codewords := 1;
    for i := 1 to ecc_level + 1 do
      ecc_codewords := ecc_codewords * 2;


    Inc(codewords_used, ecc_codewords);
    Inc(codewords_used, 3);

    if (codewords_used > symbol.option_3) then
    begin
      result := ZERROR_TOO_LONG; exit;
    end;
    { *(cc_width) := 0.5 + sqrt((codewords_used) / 3); }
    cc_width := (lin_width - 62) div 17;
    if ((codewords_used div cc_width) > 90) then
    begin
      { stop the symbol from becoming too high }
      cc_width := cc_width + 1;
    end;

    rows := codewords_used div cc_width;
    if (codewords_used mod cc_width <> 0) then
      Inc(rows);

    codewords_total := cc_width * rows;

    target_codewords := codewords_total - ecc_codewords;
    Dec(target_codewords, 3);

    target_bytesize := 6 * (target_codewords div 5);
    Inc(target_bytesize, target_codewords mod 5);

    target_bitsize := 8 * target_bytesize;
  end;

  remainder := binary_length - target_bitsize;

  if (latch = 1) then
  begin
    i := 0;
    { There is still one more numeric digit to encode }

    if ((remainder >= 4) and (remainder <= 6)) then
    begin
      d1 := ctoi(general_field[i]);
      Inc(d1);

      mask := $08;
      for j := 0 to 3 do
      begin
        if ((value and mask) = $00) then
          concat(binary_string, '0')
        else
          concat(binary_string, '1');
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
        if ((value and mask) = $00) then
          concat(binary_string, '0')
        else
          concat(binary_string, '1');
        mask := mask shr 1;
      end;
      { This may push the symbol up to the next size }
    end;
  end;

  if (strlen(binary_string) > 11805) then
  begin { (2361 * 5) }
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  { all the code below is repeated from above - it needs to be calculated again because the
     size of the symbol may have changed when adding data in the above sequence }

  binary_length := strlen(binary_string);
  if (cc_mode = 1) then
  begin
    { CC-A 2D component - calculate padding required }
    case cc_width of
      2:
      begin
        if (binary_length > 167) then begin result := ZERROR_TOO_LONG; exit; end;
        if (binary_length <= 167) then target_bitsize := 167;
        if (binary_length <= 138) then target_bitsize := 138;
        if (binary_length <= 118) then target_bitsize := 118;
        if (binary_length <= 108) then target_bitsize := 108;
        if (binary_length <= 88) then target_bitsize := 88;
        if (binary_length <= 78) then target_bitsize := 78;
        if (binary_length <= 59) then target_bitsize := 59;
      end;
      3:
      begin
        if (binary_length > 167) then begin result := ZERROR_TOO_LONG; exit; end;
        if (binary_length <= 167) then target_bitsize := 167;
        if (binary_length <= 138) then target_bitsize := 138;
        if (binary_length <= 118) then target_bitsize := 118;
        if (binary_length <= 98) then target_bitsize := 98;
        if (binary_length <= 78) then target_bitsize := 78;
      end;
      4:
      begin
        if (binary_length > 197) then begin result := ZERROR_TOO_LONG; exit; end;
        if (binary_length <= 197) then target_bitsize := 197;
        if (binary_length <= 167) then target_bitsize := 167;
        if (binary_length <= 138) then target_bitsize := 138;
        if (binary_length <= 108) then target_bitsize := 108;
        if (binary_length <= 78) then target_bitsize := 78;
      end;
    end;
  end;

  if (cc_mode = 2) then
  begin
    { CC-B 2D component }
    case cc_width of
      2:
      begin
        if (binary_length > 336) then begin result := ZERROR_TOO_LONG; exit; end;
        if (binary_length <= 336) then target_bitsize := 336;
        if (binary_length <= 296) then target_bitsize := 296;
        if (binary_length <= 256) then target_bitsize := 256;
        if (binary_length <= 208) then target_bitsize := 208;
        if (binary_length <= 160) then target_bitsize := 160;
        if (binary_length <= 104) then target_bitsize := 104;
        if (binary_length <= 56) then target_bitsize := 56;
      end;
      3:
      begin
        if (binary_length > 768) then begin result := ZERROR_TOO_LONG; exit; end;
        if (binary_length <= 768) then target_bitsize := 768;
        if (binary_length <= 648) then target_bitsize := 648;
        if (binary_length <= 536) then target_bitsize := 536;
        if (binary_length <= 416) then target_bitsize := 416;
        if (binary_length <= 304) then target_bitsize := 304;
        if (binary_length <= 208) then target_bitsize := 208;
        if (binary_length <= 152) then target_bitsize := 152;
        if (binary_length <= 112) then target_bitsize := 112;
        if (binary_length <= 72) then target_bitsize := 72;
        if (binary_length <= 32) then target_bitsize := 32;
      end;
      4:
      begin
        if (binary_length > 1184) then begin result := ZERROR_TOO_LONG; exit; end;
        if (binary_length <= 1184) then target_bitsize := 1184;
        if (binary_length <= 1016) then target_bitsize := 1016;
        if (binary_length <= 840) then target_bitsize := 840;
        if (binary_length <= 672) then target_bitsize := 672;
        if (binary_length <= 496) then target_bitsize := 496;
        if (binary_length <= 352) then target_bitsize := 352;
        if (binary_length <= 264) then target_bitsize := 264;
        if (binary_length <= 208) then target_bitsize := 208;
        if (binary_length <= 152) then target_bitsize := 152;
        if (binary_length <= 96) then target_bitsize := 96;
        if (binary_length <= 56) then target_bitsize := 56;
      end;
    end;
  end;

  if (cc_mode = 3) then
  begin
    { CC-C 2D Component is a bit more complex! }

    byte_length := binary_length div 8;
    if (binary_length mod 8 <> 0) then Inc(byte_length);

    codewords_used := (byte_length div 6) * 5;
    Inc(codewords_used, byte_length mod 6);

    ecc_level := 7;
    if (codewords_used <= 1280) then ecc_level := 6;
    if (codewords_used <= 640) then ecc_level := 5;
    if (codewords_used <= 320) then ecc_level := 4;
    if (codewords_used <= 160) then ecc_level := 3;
    if (codewords_used <= 40) then ecc_level := 2;
    ecc := ecc_level;
    ecc_codewords := 1;
    for i := 1 to ecc_level + 1 do
      ecc_codewords := ecc_codewords * 2;

    Inc(codewords_used, ecc_codewords);
    Inc(codewords_used, 3);

    if (codewords_used > symbol.option_3) then
    begin
      result := ZERROR_TOO_LONG; exit;
    end;
    { *(cc_width) := 0.5 + sqrt((codewords_used) / 3); }
    cc_width := (lin_width - 62) div 17;
    if ((codewords_used / cc_width) > 90) then
      { stop the symbol from becoming too high }
      cc_width := cc_width + 1;

    rows := codewords_used div cc_width;
    if (codewords_used mod cc_width <> 0) then
      Inc(rows);

    codewords_total := cc_width * rows;

    target_codewords := codewords_total - ecc_codewords;
    Dec(target_codewords, 3);

    target_bytesize := 6 * (target_codewords div 5);
    Inc(target_bytesize, target_codewords mod 5);

    target_bitsize := 8 * target_bytesize;
  end;

  if (binary_length < target_bitsize) then
  begin
    { Now add padding to binary string }
    if (alpha_pad = 1) then
    begin
      concat(binary_string, '11111');
      alpha_pad := 0;
      { Extra FNC1 character required after Alpha encodation (section 5.2.3) }
    end;

    if ((strlen(general_field) <> 0) and (general_field_type[strlen(general_field) - 1] = _NUMERIC)) then
      concat(binary_string, '0000');

    while (strlen(binary_string) < target_bitsize) do
      concat(binary_string, '00100');

    if (strlen(binary_string) > target_bitsize) then
      binary_string[target_bitsize] := #0;
  end;

  result := 0; exit;
end;

procedure add_leading_zeroes(symbol : zint_symbol);
var
  with_addon : Integer;
  first_len, second_len, zfirst_len, zsecond_len, i, h, n : Integer;
  temp : TArrayOfChar;
begin
  with_addon := 0;
  first_len := 0; second_len := 0; zfirst_len := 0; zsecond_len := 0; n := 0;

  h  := strlen(symbol.primary);
  for i := 0 to h - 1 do
  begin
    if (symbol.primary[i] = '+') then
      with_addon := 1
    else
    begin
      if (with_addon = 0) then
        Inc(first_len)
      else
        Inc(second_len);
    end;
  end;

  { Calculate target _lengths }
  if (first_len <= 12) then zfirst_len := 12;
  if (first_len <= 7) then zfirst_len := 7;
  if (second_len <= 5) then zsecond_len := 5;
  if (second_len <= 2) then zsecond_len := 2;
  if (second_len = 0) then zsecond_len := 0;

  { Add leading zeroes }
  n := zfirst_len - first_len + zsecond_len;
  if (n > 0) then
  begin
    SetLength(temp, strlen(symbol.primary) + 1);
    ArrayCopy(temp, symbol.primary);
    Fill(symbol.primary, n , '0');
    symbol.primary[n] := #0;
    concat(symbol.primary, temp);
  end;
end;

function composite(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number, cc_mode, cc_width, ecc_level : Integer;
  j, i, k : Integer;
  rs : Cardinal;
  bs : Cardinal;
  pri_len : Cardinal;
  reduced : TArrayOfChar;
  binary_string : TArrayOfChar;
  linear : zint_symbol;
  top_shift, bottom_shift : Integer;
begin
  rs := _length + 1;
  bs := 20 * rs;
  SetLength(reduced, rs);
  SetLength(binary_string, bs);

  error_number := 0;
  pri_len := strlen(symbol.primary);
  if (pri_len = 0) then
  begin
    strcpy(symbol.errtxt, 'No primary (linear) message in 2D composite');
    result := ZERROR_INVALID_OPTION; exit;
  end;

  if (_length > 2990) then
  begin
    strcpy(symbol.errtxt, '2D component input data too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  linear := TZintSymbol.Create(nil); { Symbol contains the 2D component and Linear contains the rest }
  try
    error_number := gs1_verify(symbol, source, _length, reduced);
    if (error_number <> 0) then begin result := error_number; exit; end;

    cc_mode := symbol.option_1;

    if ((cc_mode = 3) and (symbol.symbology <> BARCODE_EAN128_CC)) then
    begin
      { CC-C can only be used with a GS1-128 linear part }
      strcpy(symbol.errtxt, 'Invalid mode (CC-C only valid with GS1-128 linear component)');
      result := ZERROR_INVALID_OPTION; exit;
    end;

    linear.symbology := symbol.symbology;

    if (linear.symbology <> BARCODE_EAN128_CC) then
      { Set the 'component linkage' flag in the linear component }
      linear.option_1 := 2
    else
      { GS1-128 needs to know which type of 2D component is used }
      linear.option_1 := cc_mode;

    case symbol.symbology of
      BARCODE_EANX_CC:    error_number := eanx(linear, ArrayOfCharToArrayOfByte(symbol.primary), pri_len);
      BARCODE_EAN128_CC:    error_number := ean_128(linear, ArrayOfCharToArrayOfByte(symbol.primary), pri_len);
      BARCODE_RSS14_CC:    error_number := rss14(linear, ArrayOfCharToArrayOfByte(symbol.primary), pri_len);
      BARCODE_RSS_LTD_CC:  error_number := rsslimited(linear, ArrayOfCharToArrayOfByte(symbol.primary), pri_len);
      BARCODE_RSS_EXP_CC:  error_number := rssexpanded(linear, ArrayOfCharToArrayOfByte(symbol.primary), pri_len);
      BARCODE_UPCA_CC:    error_number := eanx(linear, ArrayOfCharToArrayOfByte(symbol.primary), pri_len);
      BARCODE_UPCE_CC:    error_number := eanx(linear, ArrayOfCharToArrayOfByte(symbol.primary), pri_len);
      BARCODE_RSS14STACK_CC:  error_number := rss14(linear, ArrayOfCharToArrayOfByte(symbol.primary), pri_len);
      BARCODE_RSS14_OMNI_CC:  error_number := rss14(linear, ArrayOfCharToArrayOfByte(symbol.primary), pri_len);
      BARCODE_RSS_EXPSTACK_CC:  error_number := rssexpanded(linear, ArrayOfCharToArrayOfByte(symbol.primary), pri_len);
    end;

    if (error_number <> 0) then
    begin
      strcpy(symbol.errtxt, linear.errtxt);
      concat(symbol.errtxt, ' in linear component');
      result := error_number; exit;
    end;

    case symbol.symbology of
      { Determine width of 2D component according to ISO/IEC 24723 Table 1 }
      BARCODE_EANX_CC:
      begin
        case pri_len of
          7, { EAN-8 }
          10, { EAN-8 + 2 }
          13: { EAN-8 + 5 }
            cc_width := 3;
          12, { EAN-13 }
          15, { EAN-13 + 2 }
          18: { EAN-13 + 5 }
            cc_width := 4;
          else //added for the case that the primary data is invalid
          begin
            concat(symbol.errtxt, 'Invalid primary data');
            result := ZERROR_INVALID_DATA; exit;
          end;
        end;
      end;
      BARCODE_EAN128_CC:    cc_width := 4;
      BARCODE_RSS14_CC:    cc_width := 4;
      BARCODE_RSS_LTD_CC:  cc_width := 3;
      BARCODE_RSS_EXP_CC:  cc_width := 4;
      BARCODE_UPCA_CC:    cc_width := 4;
      BARCODE_UPCE_CC:    cc_width := 2;
      BARCODE_RSS14STACK_CC:  cc_width := 2;
      BARCODE_RSS14_OMNI_CC:  cc_width := 2;
      BARCODE_RSS_EXPSTACK_CC:  cc_width := 4;
    end;

    Fill(binary_string, bs, #0);

    if (cc_mode < 1) or (cc_mode > 3) then cc_mode := 1;

    if (cc_mode = 1) then
    begin
      i := cc_binary_string(symbol, reduced, binary_string, cc_mode, cc_width, ecc_level, linear.width);
      if (i = ZERROR_TOO_LONG) then
        cc_mode := 2;
    end;

    if (cc_mode = 2) then
    begin { If the data didn't fit into CC-A it is recalculated for CC-B }
      i := cc_binary_string(symbol, reduced, binary_string, cc_mode, cc_width, ecc_level, linear.width);
      if (i = ZERROR_TOO_LONG) then
      begin
        if (symbol.symbology <> BARCODE_EAN128_CC) then
        begin
          result := ZERROR_TOO_LONG; exit;
        end
        else
          cc_mode := 3;
      end;
    end;

    if (cc_mode = 3) then
    begin { If the data didn't fit in CC-B (and linear part is GS1-128) it is recalculated for CC-C }
      i := cc_binary_string(symbol, reduced, binary_string, cc_mode, cc_width, ecc_level, linear.width);
      if (i = ZERROR_TOO_LONG) then
      begin
        result := ZERROR_TOO_LONG; exit;
      end;
    end;

    case cc_mode of { Note that ecc_level is only relevant to CC-C }
      1: error_number := cc_a(symbol, binary_string, cc_width);
      2: error_number := cc_b(symbol, binary_string, cc_width);
      3: error_number := cc_c(symbol, binary_string, cc_width, ecc_level);
    end;

    if (error_number <> 0) then
    begin
      result := ZERROR_ENCODING_PROBLEM; exit;
    end;

    { Merge the linear component with the 2D component }

    top_shift := 0;
    bottom_shift := 0;

    case symbol.symbology of
      { Determine horizontal alignment (according to section 12.3) }
      BARCODE_EANX_CC:
      begin
        case pri_len of
          7, { EAN-8 }
          10, { EAN-8 + 2 }
          13: { EAN-8 + 5 }
            bottom_shift := 13;
          12, { EAN-13 }
          15, { EAN-13 + 2 }
          18: { EAN-13 + 5 }
            bottom_shift := 2;
        end;
      end;
      BARCODE_EAN128_CC:
        if (cc_mode = 3) then
          bottom_shift := 7;
      BARCODE_RSS14_CC: bottom_shift := 4;
      BARCODE_RSS_LTD_CC: bottom_shift := 9;
      BARCODE_RSS_EXP_CC:
      begin
        k := 1;
        while((not (module_is_set(linear, 1, k - 1) <> 0)) and (module_is_set(linear, 1, k) <> 0)) do
        { while((linear.encoded_data[1][k - 1] <> '1') and (linear.encoded_data[1][k] <> '0')) begin }
          Inc(k);
        top_shift := k;
      end;
      BARCODE_UPCA_CC: bottom_shift := 2;
      BARCODE_UPCE_CC: bottom_shift := 2;
      BARCODE_RSS14STACK_CC: top_shift := 1;
      BARCODE_RSS14_OMNI_CC: top_shift := 1;
      BARCODE_RSS_EXPSTACK_CC:
      begin
        k := 1;
        while((not (module_is_set(linear, 1, k - 1) <> 0)) and (module_is_set(linear, 1, k) <> 0)) do
        { while((linear.encoded_data[1][k - 1] <> '1') and (linear.encoded_data[1][k] <> '0')) begin }
          Inc(k);
        top_shift := k;
      end;
    end;

    if (top_shift <> 0) then
    begin
      { Move the 2d component of the symbol horizontally }
      for i := 0 to symbol.rows do
      begin
        for j := (symbol.width + top_shift) downto top_shift do
        begin
          if (module_is_set(symbol, i, j - top_shift) <> 0) then set_module(symbol, i, j) else unset_module(symbol, i, j);
        end;
        for j := 0 to top_shift - 1 do
          unset_module(symbol, i, j);
      end;
    end;

    { Merge linear and 2D components into one structure }
    for i := 0 to linear.rows do
    begin
      symbol.row_height[symbol.rows + i] := linear.row_height[i];
      for j := 0 to linear.width do
      begin
        if (module_is_set(linear, i, j) <> 0) then
          set_module(symbol, i + symbol.rows, j + bottom_shift)
        else
          unset_module(symbol, i + symbol.rows, j + bottom_shift);
      end;
    end;
    if ((linear.width + bottom_shift) > symbol.width) then
      symbol.width := linear.width + bottom_shift;

    if ((symbol.width + top_shift) > symbol.width) then
      Inc(symbol.width, top_shift);

    Inc(symbol.rows, linear.rows);
    ustrcpy(symbol.text, linear.text);
  finally
    linear.Free;
  end;

  result := error_number; exit;
end;

end.

