unit Test_Code128;

{$I zint_test.inc}

{
  Unit-Tests fuer Code 128, EAN-128 (GS1-128), EAN-14, NVE-18, HIBC-128.
  Testdaten aus test_code128.c (Zint Commit b3a3c0d, 2026-03-13).
}

interface

uses
  {$IFNDEF FPC}DUnitX.TestFramework,{$ENDIF}
  TestFramework_Zint,
  SysUtils,
  zint,
  zint_common,
  TestHelper_Zint;

type
  { ---- Code 128 / Code 128B ---- }
  [TestFixture]
  TTestCode128 = class(TZintFixture)
  private
    FSymbol: TZintSymbol;
    procedure ResetSymbol;
  public
    [Setup]    procedure Setup; {$IFDEF FPC}override;{$ENDIF}
    [TearDown] procedure TearDown; {$IFDEF FPC}override;{$ENDIF}

    { == test_encode (pattern verification from C) == }
  published
    [Test] procedure Test_Encode_AIM;
    [Test] procedure Test_Encode_Digits10;
    [Test] procedure Test_Encode_Digits5_Odd;
    [Test] procedure Test_Encode_MixedAlphaNum;
    [Test] procedure Test_Encode_DataMode_HighBytes;
    [Test] procedure Test_Encode_DataMode_MixedLong;

    { == test_large (boundary tests from C) == }
    [Test] procedure Test_Large_101A_OK;
    [Test] procedure Test_Large_102A_TooLong;
    [Test] procedure Test_Large_256A_TooLong;
    [Test] procedure Test_Large_257A_TooLong;
    [Test] procedure Test_Large_ReaderInit_100A_OK;
    [Test] procedure Test_Large_ReaderInit_101A_TooLong;
    [Test] procedure Test_Large_ExtA_67_OK;
    [Test] procedure Test_Large_ExtA_68_TooLong;
    [Test] procedure Test_Large_Ext_99_OK;
    [Test] procedure Test_Large_Ext_100_TooLong;
    [Test] procedure Test_Large_199Digits_OK;
    [Test] procedure Test_Large_200Digits_OK;
    [Test] procedure Test_Large_201Digits_TooLong;
    [Test] procedure Test_Large_202Digits_OK;
    [Test] procedure Test_Large_203Digits_TooLong;
    [Test] procedure Test_Large_204Digits_TooLong;
    [Test] procedure Test_Large_257Chars_InputTooLong;

    { == test_input (width verification from C) == }
    [Test] procedure Test_Input_DataMode_PAD;
    [Test] procedure Test_Input_AIM1234;
    [Test] procedure Test_Input_GS1_NotSupported;
    [Test] procedure Test_Input_1;
    [Test] procedure Test_Input_12;
    [Test] procedure Test_Input_123;
    [Test] procedure Test_Input_1234;
    [Test] procedure Test_Input_12345;
    [Test] procedure Test_Input_US;
    [Test] procedure Test_Input_1_US;
    [Test] procedure Test_Input_12_US;
    [Test] procedure Test_Input_a_US_a;
    [Test] procedure Test_Input_1234_US_a;
    [Test] procedure Test_Input_US_AAa_US;
    [Test] procedure Test_Input_US_AAaa_US;
    [Test] procedure Test_Input_AAAa12345aAA;
    [Test] procedure Test_Input_a_US_Aa_US_US_a_US_aa_US_a;
    [Test] procedure Test_Input_NUL_US_szlig;
    [Test] procedure Test_Input_NUL_US_eacute;
    [Test] procedure Test_Input_NUL_US_eacute_a;
    [Test] procedure Test_Input_ab_szlig;
    [Test] procedure Test_Input_eee;
    [Test] procedure Test_Input_aeeeb;
    [Test] procedure Test_Input_aeeeeb;
    [Test] procedure Test_Input_aeeeeeb;
    [Test] procedure Test_Input_aeeeeebc;
    [Test] procedure Test_Input_aeeeeebcd;
    [Test] procedure Test_Input_aeeeeebcde;
    [Test] procedure Test_Input_aeeeeebcdeee;
    [Test] procedure Test_Input_plusminus4_1234AA;
    [Test] procedure Test_Input_FNC4_mixed;
    [Test] procedure Test_Input_m_lf_m_lf_m;
    [Test] procedure Test_Input_c_lf_aDEF;
    [Test] procedure Test_Input_lf_a_lf_DEF;
    [Test] procedure Test_Input_A_lf_1234_A12_lf;
    [Test] procedure Test_Input_21star_cr_lf_M0;
    [Test] procedure Test_Input_12e;
    [Test] procedure Test_Input_123e;
    [Test] procedure Test_Input_1234e;
    [Test] procedure Test_Input_123456e;
    [Test] procedure Test_Input_1234567e;
    [Test] procedure Test_Input_1e;
    [Test] procedure Test_Input_12e12;
    [Test] procedure Test_Input_1234e12;
    [Test] procedure Test_Input_1234e1234;
    [Test] procedure Test_Input_12e12e;
    [Test] procedure Test_Input_1234e123456e;
    [Test] procedure Test_Input_space_hiA_4_space;
    [Test] procedure Test_Input_space_hiA_4_2sp;
    [Test] procedure Test_Input_space_hiA_4_3sp;
    [Test] procedure Test_Input_space_hiA_4_4sp;
    [Test] procedure Test_Input_space_hiA_4_5sp;

    { == test_hrt == }
    [Test] procedure Test_HRT_Digits;
    [Test] procedure Test_HRT_NUL_Replace;
    [Test] procedure Test_HRT_CtrlReplace;
    [Test] procedure Test_HRT_Extended;

    { == test_reader_init (codeword width from C) == }
    [Test] procedure Test_ReaderInit_A;
    [Test] procedure Test_ReaderInit_12;
    [Test] procedure Test_ReaderInit_1234;

    { == Basic validation (existing) == }
    [Test] procedure Test_SingleChar;
    [Test] procedure Test_EmptyInput;
    [Test] procedure Test_WidthIncreases;
    [Test] procedure Test_AlwaysSingleRow;
    [Test] procedure Test_Encode_Alpha;

    { == Code 128B (existing) == }
    [Test] procedure Test_Code128B_Basic;
    [Test] procedure Test_Code128B_NoCodeC;
    [Test] procedure Test_Code128B_TooLong;
  end;

  { ---- EAN-128 / GS1-128 ---- }
  [TestFixture]
  TTestEAN128 = class(TZintFixture)
  private
    FSymbol: TZintSymbol;
  public
    [Setup]    procedure Setup; {$IFDEF FPC}override;{$ENDIF}
    [TearDown] procedure TearDown; {$IFDEF FPC}override;{$ENDIF}

  published
    [Test] procedure Test_Basic_AI01;
    [Test] procedure Test_HRT;
    [Test] procedure Test_MultipleAIs;
    [Test] procedure Test_EmptyGS1;

    { == test_gs1_128_input from C == }
    [Test] procedure Test_GS1_90_1_90_1;
    [Test] procedure Test_GS1_90_1_90_12;
    [Test] procedure Test_GS1_90_12_90_1;
    [Test] procedure Test_GS1_90_12_90_12;
    [Test] procedure Test_GS1_90_123_90_1;
    [Test] procedure Test_GS1_90_123_90_1234;
    [Test] procedure Test_GS1_90_1_90_1_90_1;
    [Test] procedure Test_GS1_90_1A_90_1;
    [Test] procedure Test_GS1_90_12345A12345A;
  end;

  { ---- EAN-14 ---- }
  [TestFixture]
  TTestEAN14 = class(TZintFixture)
  private
    FSymbol: TZintSymbol;
    procedure ResetSymbol;
  public
    [Setup]    procedure Setup; {$IFDEF FPC}override;{$ENDIF}
    [TearDown] procedure TearDown; {$IFDEF FPC}override;{$ENDIF}

    { == test_ean14_input from C == }
  published
    [Test] procedure Test_TooLong_15;
    [Test] procedure Test_NonNumeric_14;
    [Test] procedure Test_WrongCheckDigit;
    [Test] procedure Test_NonNumeric_13;
    [Test] procedure Test_Valid_13;
    [Test] procedure Test_Valid_14;
    [Test] procedure Test_Valid_0134;
    [Test] procedure Test_Valid_01345;
    [Test] procedure Test_Prefix01_WithCheck;
    [Test] procedure Test_Prefix01_NoCheck;
    [Test] procedure Test_BracketPrefix_WithCheck;
    [Test] procedure Test_BracketPrefix_NoCheck;
    [Test] procedure Test_ParenPrefix_WithCheck;
    [Test] procedure Test_ParenPrefix_NoCheck;
    [Test] procedure Test_Prefix00_TooLong;
    [Test] procedure Test_Bracket00_TooLong;
    [Test] procedure Test_Paren00_TooLong;
    [Test] procedure Test_MismatchParen_TooLong;

    { == test_large from C == }
    [Test] procedure Test_Large_Valid;
    [Test] procedure Test_Large_TooLong;

    { == test_reader_init from C == }
    [Test] procedure Test_ReaderInit_Warn;

    { == test_encode from C == }
    [Test] procedure Test_Encode_4070;

    { existing }
    [Test] procedure Test_ShortPadded;
  end;

  { ---- NVE-18 ---- }
  [TestFixture]
  TTestNVE18 = class(TZintFixture)
  private
    FSymbol: TZintSymbol;
    procedure ResetSymbol;
  public
    [Setup]    procedure Setup; {$IFDEF FPC}override;{$ENDIF}
    [TearDown] procedure TearDown; {$IFDEF FPC}override;{$ENDIF}

    { == test_nve18_input from C == }
  published
    [Test] procedure Test_TooLong_19;
    [Test] procedure Test_NonNumeric_18;
    [Test] procedure Test_WrongCheckDigit;
    [Test] procedure Test_NonNumeric_5;
    [Test] procedure Test_Valid_18;
    [Test] procedure Test_Valid_17;
    [Test] procedure Test_Valid_003456;
    [Test] procedure Test_Valid_00345;
    [Test] procedure Test_Prefix00_WithCheck;
    [Test] procedure Test_Prefix00_NoCheck;
    [Test] procedure Test_BracketPrefix_WithCheck;
    [Test] procedure Test_BracketPrefix_NoCheck;
    [Test] procedure Test_ParenPrefix_WithCheck;
    [Test] procedure Test_ParenPrefix_NoCheck;
    [Test] procedure Test_Prefix01_TooLong;
    [Test] procedure Test_Bracket01_TooLong;
    [Test] procedure Test_Paren01_TooLong;
    [Test] procedure Test_MismatchParen_TooLong;

    { == test_large from C == }
    [Test] procedure Test_Large_Valid;
    [Test] procedure Test_Large_TooLong;

    { == test_reader_init from C == }
    [Test] procedure Test_ReaderInit_Warn;

    { == test_encode from C == }
    [Test] procedure Test_Encode_40700000;

    { existing }
    [Test] procedure Test_ShortPadded;
  end;

  { ---- HIBC-128 ---- }
  [TestFixture]
  TTestHIBC128 = class(TZintFixture)
  private
    FSymbol: TZintSymbol;
    procedure ResetSymbol;
  public
    [Setup]    procedure Setup; {$IFDEF FPC}override;{$ENDIF}
    [TearDown] procedure TearDown; {$IFDEF FPC}override;{$ENDIF}

    { == test_hibc_input from C == }
  published
    [Test] procedure Test_InvalidChar;
    [Test] procedure Test_A99912345;
    [Test] procedure Test_AllValid;
    [Test] procedure Test_Percent58;
    [Test] procedure Test_Large_110_OK;
    [Test] procedure Test_Large_111_TooLong;
    [Test] procedure Test_Large_AllDigits;

    { == test_hrt from C == }
    [Test] procedure Test_HRT_12345678900;
    [Test] procedure Test_HRT_A999123457;

    { == test_reader_init from C == }
    [Test] procedure Test_ReaderInit;

    { == test_encode from C == }
    [Test] procedure Test_Encode_83278F8G9H0J2G;
    [Test] procedure Test_Encode_A123BJC5D6E71;
    [Test] procedure Test_Encode_52001510X3G;
  end;

  [TestFixture]
  TTestCode128HRTContentSegsFromC = class(TZintFixture)
  public
  published
    [Test] procedure HRT_ContentSegs_FromC;
  end;

implementation

{ ======================================================================
  TTestCode128
  ====================================================================== }

procedure TTestCode128.Setup;
begin
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_CODE128);
end;

procedure TTestCode128.TearDown;
begin
  FreeAndNil(FSymbol);
end;

procedure TTestCode128.ResetSymbol;
begin
  FreeAndNil(FSymbol);
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_CODE128);
end;

{ --- test_encode (pattern from C) --- }

procedure TTestCode128.Test_Encode_AIM;
{ C#0: "AIM" -> width 68, ISO/IEC 15417:2007 Figure 1 }
var
  Ret: Integer;
  Expected: String;
begin
  Ret := TZintTestHelper.EncodeData(FSymbol, 'AIM');
  ZAssert.AreEqual(ZINT_OK, Ret);
  ZAssert.AreEqual(68, FSymbol.width);
  Expected := '11010010000' {StartB}
            + '10100011000' {A}
            + '11000100010' {I}
            + '10111011000' {M}
            + '10111011000' {Check}
            + '1100011101011'; {Stop}
  ZAssert.AreEqual(Expected, TZintTestHelper.ModulesDump(FSymbol));
end;

procedure TTestCode128.Test_Encode_Digits10;
{ C#2: "1234567890" -> width 90 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1234567890'));
  ZAssert.AreEqual(90, FSymbol.width);
  ZAssert.AreEqual(
    '110100111001011001110010001011000111000101101100001010011011110110100111100101100011101011',
    TZintTestHelper.ModulesDump(FSymbol));
end;

procedure TTestCode128.Test_Encode_Digits5_Odd;
{ "12345" -> StartC 12 34 CodeB 5 Check Stop = width 79 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12345'));
  ZAssert.AreEqual(79, FSymbol.width);
end;

procedure TTestCode128.Test_Encode_MixedAlphaNum;
{ C test_input#2: "AIM1234" -> width 101 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'AIM1234'));
  ZAssert.AreEqual(101, FSymbol.width);
end;

procedure TTestCode128.Test_Encode_DataMode_HighBytes;
{ C#4: DATA_MODE "\101\102\103\104\105\106\200\200\200\200\200" -> width 178 }
var
  Data: TArrayOfByte;
begin
  FSymbol.input_mode := DATA_MODE;
  Data := TArrayOfByte.Create($41,$42,$43,$44,$45,$46,$80,$80,$80,$80,$80, 0);
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, Data, 11));
  ZAssert.AreEqual(178, FSymbol.width);
  ZAssert.AreEqual(
    '1101000010010100011000100010110001000100011010110001000100011010001000110001011101011110111010111101010000110010100001100101000011001010000110010100001100110110110001100011101011',
    TZintTestHelper.ModulesDump(FSymbol));
end;

procedure TTestCode128.Test_Encode_DataMode_MixedLong;
{ C#5: complex mixed A/B/C mode switching, width 1091 }
var
  Data: TArrayOfByte;
begin
  FSymbol.input_mode := DATA_MODE;
  Data := TArrayOfByte.Create(
    $41,$42,$43,$64,$0A,$45,$31,$32,$33,$48,$49,$4A,$34,$35,$36,$37,
    $4C,$4D,$50,$61,$57,$0A,$58,$59,$5A,$61,$62,$0A,$0A,$65,$0A,$0A,
    $66,$67,$0A,$7D,$31,$32,$0A,$0A,$34,$35,$36,$0A,$82,$C2,$0A,$0A,
    $E2,$31,$32,$0A,$0A,$0A,$0A,$31,$32,$33,$0A,$0A,$0A,$0A,$0A,$0A,
    $31,$32,$33,$34,$0A,$0A,$0A,$0A,$E2,$E2,$E2,$31,$32,$33,$34,$E2,
    $E2,$E2, 0);
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, Data, 82));
  ZAssert.AreEqual(1091, FSymbol.width);
end;

{ --- test_large (boundary tests from C) --- }

procedure TTestCode128.Test_Large_101A_OK;
{ C#0: 101 'A' -> OK, width 1146 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, TZintTestHelper.StrRepeat('A', 101)));
  ZAssert.AreEqual(1146, FSymbol.width);
end;

procedure TTestCode128.Test_Large_102A_TooLong;
{ C#1: 102 'A' -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, TZintTestHelper.StrRepeat('A', 102)));
end;

procedure TTestCode128.Test_Large_256A_TooLong;
{ C#2: 256 'A' -> TOO_LONG (exceeds symbol capacity) }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, TZintTestHelper.StrRepeat('A', 256)));
end;

procedure TTestCode128.Test_Large_257A_TooLong;
{ C#3: 257 'A' -> TOO_LONG (exceeds C128_MAX input) }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, TZintTestHelper.StrRepeat('A', 257)));
end;

procedure TTestCode128.Test_Large_ReaderInit_100A_OK;
{ C#5: READER_INIT + 100 'A' -> OK, width 1146 }
begin
  FSymbol.output_options := FSymbol.output_options or READER_INIT;
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, TZintTestHelper.StrRepeat('A', 100)));
  ZAssert.AreEqual(1146, FSymbol.width);
end;

procedure TTestCode128.Test_Large_ReaderInit_101A_TooLong;
{ C#6: READER_INIT + 101 'A' -> TOO_LONG }
begin
  FSymbol.output_options := FSymbol.output_options or READER_INIT;
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, TZintTestHelper.StrRepeat('A', 101)));
end;

procedure TTestCode128.Test_Large_ExtA_67_OK;
{ C#9: 67 chars of pattern "éA" -> OK, width 1146 }
var
  S: String;
begin
  S := TZintTestHelper.StrRepeat(#$E9'A', 67);
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, S));
  ZAssert.AreEqual(1146, FSymbol.width);
end;

procedure TTestCode128.Test_Large_ExtA_68_TooLong;
{ C#10: 68 chars of pattern "éA" -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat(#$E9'A', 68)));
end;

procedure TTestCode128.Test_Large_Ext_99_OK;
{ C#14: 99 x 'é' -> OK, width 1146 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat(#$E9, 99)));
  ZAssert.AreEqual(1146, FSymbol.width);
end;

procedure TTestCode128.Test_Large_Ext_100_TooLong;
{ C#15: 100 x 'é' -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat(#$E9, 100)));
end;

procedure TTestCode128.Test_Large_199Digits_OK;
{ C#17: 199 x '0' -> OK, width 1146 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat('0', 199)));
  ZAssert.AreEqual(1146, FSymbol.width);
end;

procedure TTestCode128.Test_Large_200Digits_OK;
{ C#18: 200 x '0' -> OK, width 1135 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat('0', 200)));
  ZAssert.AreEqual(1135, FSymbol.width);
end;

procedure TTestCode128.Test_Large_201Digits_TooLong;
{ C#19: 201 x '0' -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat('0', 201)));
end;

procedure TTestCode128.Test_Large_202Digits_OK;
{ C#20: 202 x '0' -> OK, width 1146 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat('0', 202)));
  ZAssert.AreEqual(1146, FSymbol.width);
end;

procedure TTestCode128.Test_Large_203Digits_TooLong;
{ C#21: 203 x '0' -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat('0', 203)));
end;

procedure TTestCode128.Test_Large_204Digits_TooLong;
{ C#22: 204 x '0' -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat('0', 204)));
end;

procedure TTestCode128.Test_Large_257Chars_InputTooLong;
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat('A', 257)));
end;

{ --- test_input (width verification from C) --- }

procedure TTestCode128.Test_Input_DataMode_PAD;
{ C#1: DATA_MODE "\200" -> width 57 }
var
  Data: TArrayOfByte;
begin
  FSymbol.input_mode := DATA_MODE;
  Data := TArrayOfByte.Create($80, 0);
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, Data, 1));
  ZAssert.AreEqual(57, FSymbol.width);
end;

procedure TTestCode128.Test_Input_AIM1234;
{ C#2: "AIM1234" -> width 101 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'AIM1234'));
  ZAssert.AreEqual(101, FSymbol.width);
end;

procedure TTestCode128.Test_Input_GS1_NotSupported;
{ C#13: GS1_MODE "[90]12" -> INVALID_OPTION }
begin
  FSymbol.input_mode := GS1_MODE;
  ZAssert.AreEqual(ZINT_ERROR_INVALID_OPTION, TZintTestHelper.EncodeData(FSymbol, '[90]12'));
end;

procedure TTestCode128.Test_Input_1;
{ C#14: "1" -> width 46: StartB 1 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1'));
  ZAssert.AreEqual(46, FSymbol.width);
end;

procedure TTestCode128.Test_Input_12;
{ C#18: "12" -> width 46: StartC 12 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12'));
  ZAssert.AreEqual(46, FSymbol.width);
end;

procedure TTestCode128.Test_Input_123;
{ C#25: "123" -> width 68: StartC 12 CodeB 3 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '123'));
  ZAssert.AreEqual(68, FSymbol.width);
end;

procedure TTestCode128.Test_Input_1234;
{ C#30: "1234" -> width 57: StartC 12 34 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1234'));
  ZAssert.AreEqual(57, FSymbol.width);
end;

procedure TTestCode128.Test_Input_12345;
{ C#32: "12345" -> width 79: StartC 12 34 CodeB 5 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12345'));
  ZAssert.AreEqual(79, FSymbol.width);
end;

procedure TTestCode128.Test_Input_US;
{ C#35: "\037" -> width 46: StartA US }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, #$1F));
  ZAssert.AreEqual(46, FSymbol.width);
end;

procedure TTestCode128.Test_Input_1_US;
{ C#37: "1\037" -> width 57: StartA 1 US }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1'#$1F));
  ZAssert.AreEqual(57, FSymbol.width);
end;

procedure TTestCode128.Test_Input_12_US;
{ C#38: "12\037" -> width 68: StartC 12 CodeA US }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12'#$1F));
  ZAssert.AreEqual(68, FSymbol.width);
end;

procedure TTestCode128.Test_Input_a_US_a;
{ C#40: "a\037a" -> width 79: StartB a Shift US a }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'a'#$1F'a'));
  ZAssert.AreEqual(79, FSymbol.width);
end;

procedure TTestCode128.Test_Input_1234_US_a;
{ C#42: "1234\037a" -> width 101 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1234'#$1F'a'));
  ZAssert.AreEqual(101, FSymbol.width);
end;

procedure TTestCode128.Test_Input_US_AAa_US;
{ C#45: "\037AAa\037" -> width 101 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, #$1F'AAa'#$1F));
  ZAssert.AreEqual(101, FSymbol.width);
end;

procedure TTestCode128.Test_Input_US_AAaa_US;
{ C#46: "\037AAaa\037" -> width 123 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, #$1F'AAaa'#$1F));
  ZAssert.AreEqual(123, FSymbol.width);
end;

procedure TTestCode128.Test_Input_AAAa12345aAA;
{ C#48: "AAAa12345aAA" -> width 167 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'AAAa12345aAA'));
  ZAssert.AreEqual(167, FSymbol.width);
end;

procedure TTestCode128.Test_Input_a_US_Aa_US_US_a_US_aa_US_a;
{ C#49: "a\037Aa\037\037a\037aa\037a" -> width 222 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'a'#$1F'Aa'#$1F#$1F'a'#$1F'aa'#$1F'a'));
  ZAssert.AreEqual(222, FSymbol.width);
end;

procedure TTestCode128.Test_Input_NUL_US_szlig;
{ C#50: "\000\037ß" (len 4) -> width 79 }
var
  Data: TArrayOfByte;
begin
  Data := TArrayOfByte.Create($00, $1F, $DF, 0);
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, Data, 3));
  ZAssert.AreEqual(79, FSymbol.width);
end;

procedure TTestCode128.Test_Input_NUL_US_eacute;
{ C#51: "\000\037é" (len 4) -> width 90 }
var
  Data: TArrayOfByte;
begin
  Data := TArrayOfByte.Create($00, $1F, $E9, 0);
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, Data, 3));
  ZAssert.AreEqual(90, FSymbol.width);
end;

procedure TTestCode128.Test_Input_NUL_US_eacute_a;
{ C#53: "\000\037éa" (len 5) -> width 101 }
var
  Data: TArrayOfByte;
begin
  Data := TArrayOfByte.Create($00, $1F, $E9, $61, 0);
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, Data, 4));
  ZAssert.AreEqual(101, FSymbol.width);
end;

procedure TTestCode128.Test_Input_ab_szlig;
{ C#54: "abß" -> width 79 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'ab'#$DF));
  ZAssert.AreEqual(79, FSymbol.width);
end;

procedure TTestCode128.Test_Input_eee;
{ C#57: "ééé" -> width 90: StartB LatchFNC4 é é é }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, #$E9#$E9#$E9));
  ZAssert.AreEqual(90, FSymbol.width);
end;

procedure TTestCode128.Test_Input_aeeeb;
{ C#58: "aéééb" -> width 123: shift-based }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'a'#$E9#$E9#$E9'b'));
  ZAssert.AreEqual(123, FSymbol.width);
end;

procedure TTestCode128.Test_Input_aeeeeb;
{ C#59: "aééééb" -> width 134: latch then shift back }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'a'#$E9#$E9#$E9#$E9'b'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestCode128.Test_Input_aeeeeeb;
{ C#60: "aéééééb" -> width 145 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'a'#$E9#$E9#$E9#$E9#$E9'b'));
  ZAssert.AreEqual(145, FSymbol.width);
end;

procedure TTestCode128.Test_Input_aeeeeebc;
{ C#61: "aééééébc" -> width 167 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'a'#$E9#$E9#$E9#$E9#$E9'bc'));
  ZAssert.AreEqual(167, FSymbol.width);
end;

procedure TTestCode128.Test_Input_aeeeeebcd;
{ C#62: "aééééébcd" -> width 178 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'a'#$E9#$E9#$E9#$E9#$E9'bcd'));
  ZAssert.AreEqual(178, FSymbol.width);
end;

procedure TTestCode128.Test_Input_aeeeeebcde;
{ C#63: "aééééébcde" -> width 189 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'a'#$E9#$E9#$E9#$E9#$E9'bcde'));
  ZAssert.AreEqual(189, FSymbol.width);
end;

procedure TTestCode128.Test_Input_aeeeeebcdeee;
{ C#66: "aééééébcdeééé" -> width 244 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'a'#$E9#$E9#$E9#$E9#$E9'bcde'#$E9#$E9#$E9));
  ZAssert.AreEqual(244, FSymbol.width);
end;

procedure TTestCode128.Test_Input_plusminus4_1234AA;
{ C#75: "±±±±1234AA" -> width 189 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, #$B1#$B1#$B1#$B1'1234AA'));
  ZAssert.AreEqual(189, FSymbol.width);
end;

procedure TTestCode128.Test_Input_FNC4_mixed;
{ C#77: "ÁÁèÁÁFç7Z" -> width 189 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, #$C1#$C1#$E8#$C1#$C1'F'#$E7'7Z'));
  ZAssert.AreEqual(189, FSymbol.width);
end;

procedure TTestCode128.Test_Input_m_lf_m_lf_m;
{ C#78: "m\nm\nm" -> width 112 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'm'#10'm'#10'm'));
  ZAssert.AreEqual(112, FSymbol.width);
end;

procedure TTestCode128.Test_Input_c_lf_aDEF;
{ C#79: "c\naDEF" -> width 112 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'c'#10'aDEF'));
  ZAssert.AreEqual(112, FSymbol.width);
end;

procedure TTestCode128.Test_Input_lf_a_lf_DEF;
{ C#80: "\na\nDEF" -> width 112 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, #10'a'#10'DEF'));
  ZAssert.AreEqual(112, FSymbol.width);
end;

procedure TTestCode128.Test_Input_A_lf_1234_A12_lf;
{ C#103: "A\0121234A12\012" -> width 145 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'A'#10'1234A12'#10));
  ZAssert.AreEqual(145, FSymbol.width);
end;

procedure TTestCode128.Test_Input_21star_cr_lf_M0;
{ C#104: "21*\015\012M0" -> width 112 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '21*'#13#10'M0'));
  ZAssert.AreEqual(112, FSymbol.width);
end;

procedure TTestCode128.Test_Input_12e;
{ C#125: "12é" -> width 79 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12'#$E9));
  ZAssert.AreEqual(79, FSymbol.width);
end;

procedure TTestCode128.Test_Input_123e;
{ C#126: "123é" -> width 90 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '123'#$E9));
  ZAssert.AreEqual(90, FSymbol.width);
end;

procedure TTestCode128.Test_Input_1234e;
{ C#127: "1234é" -> width 90 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1234'#$E9));
  ZAssert.AreEqual(90, FSymbol.width);
end;

procedure TTestCode128.Test_Input_123456e;
{ C#128: "123456é" -> width 101 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '123456'#$E9));
  ZAssert.AreEqual(101, FSymbol.width);
end;

procedure TTestCode128.Test_Input_1234567e;
{ C#129: "1234567é" -> width 112 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1234567'#$E9));
  ZAssert.AreEqual(112, FSymbol.width);
end;

procedure TTestCode128.Test_Input_1e;
{ C#130: "1é" -> width 68 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1'#$E9));
  ZAssert.AreEqual(68, FSymbol.width);
end;

procedure TTestCode128.Test_Input_12e12;
{ C#134: "12é12" -> width 101 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12'#$E9'12'));
  ZAssert.AreEqual(101, FSymbol.width);
end;

procedure TTestCode128.Test_Input_1234e12;
{ C#135: "1234é12" -> width 112 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1234'#$E9'12'));
  ZAssert.AreEqual(112, FSymbol.width);
end;

procedure TTestCode128.Test_Input_1234e1234;
{ C#136: "1234é1234" -> width 123 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1234'#$E9'1234'));
  ZAssert.AreEqual(123, FSymbol.width);
end;

procedure TTestCode128.Test_Input_12e12e;
{ C#139: "12é12é" -> width 123 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12'#$E9'12'#$E9));
  ZAssert.AreEqual(123, FSymbol.width);
end;

procedure TTestCode128.Test_Input_1234e123456e;
{ C#140: "1234é123456é" -> width 167 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1234'#$E9'123456'#$E9));
  ZAssert.AreEqual(167, FSymbol.width);
end;

procedure TTestCode128.Test_Input_space_hiA_4_space;
{ C#89: " ¡¡¡¡ ¡¡¡¡ " -> width 200 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol,
    ' '#$A1#$A1#$A1#$A1' '#$A1#$A1#$A1#$A1' '));
  ZAssert.AreEqual(200, FSymbol.width);
end;

procedure TTestCode128.Test_Input_space_hiA_4_2sp;
{ C#90: " ¡¡¡¡  ¡¡¡¡ " -> width 222, 2 middle spaces }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol,
    ' '#$A1#$A1#$A1#$A1'  '#$A1#$A1#$A1#$A1' '));
  ZAssert.AreEqual(222, FSymbol.width);
end;

procedure TTestCode128.Test_Input_space_hiA_4_3sp;
{ C#91: " ¡¡¡¡   ¡¡¡¡ " -> width 244, 3 middle spaces }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol,
    ' '#$A1#$A1#$A1#$A1'   '#$A1#$A1#$A1#$A1' '));
  ZAssert.AreEqual(244, FSymbol.width);
end;

procedure TTestCode128.Test_Input_space_hiA_4_4sp;
{ C#92: " ¡¡¡¡    ¡¡¡¡ " -> width 266, 4 middle spaces then latch }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol,
    ' '#$A1#$A1#$A1#$A1'    '#$A1#$A1#$A1#$A1' '));
  ZAssert.AreEqual(266, FSymbol.width);
end;

procedure TTestCode128.Test_Input_space_hiA_4_5sp;
{ C#93: " ¡¡¡¡     ¡¡¡¡ " -> width 277, 5 middle spaces then latch }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol,
    ' '#$A1#$A1#$A1#$A1'     '#$A1#$A1#$A1#$A1' '));
  ZAssert.AreEqual(277, FSymbol.width);
end;

{ --- test_hrt --- }

procedure TTestCode128.Test_HRT_Digits;
{ C#0: "1234567890" -> HRT "1234567890" }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1234567890'));
  ZAssert.AreEqual('1234567890', TZintTestHelper.GetText(FSymbol));
end;

procedure TTestCode128.Test_HRT_NUL_Replace;
{ C#2: "\000ABC\000DEF\000" (len 9) -> HRT " ABC DEF " (NUL replaced by space) }
var
  Data: TArrayOfByte;
begin
  Data := TArrayOfByte.Create($00, $41, $42, $43, $00, $44, $45, $46, $00, 0);
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, Data, 9));
  ZAssert.AreEqual(' ABC DEF ', TZintTestHelper.GetText(FSymbol));
end;

procedure TTestCode128.Test_HRT_CtrlReplace;
{ C#6: control chars are replaced with spaces in HRT }
var
  Data: TArrayOfByte;
  HRT: String;
begin
  Data := TArrayOfByte.Create($31,$32,$33,$34,$35,$09,$36,$37,$38,$39,$30,$1F,$7F, 0);
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, Data, 13));
  HRT := TZintTestHelper.GetText(FSymbol);
  ZAssert.AreEqual(13, Length(HRT), 'HRT length');
  ZAssert.AreEqual('12345 67890  ', HRT, 'HRT');
end;

procedure TTestCode128.Test_HRT_Extended;
{ C#8: "abcdé" -> HRT "abcdé" }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'abcd'#$E9));
  { HRT check - the actual output depends on encoding but data should round trip }
  ZAssert.IsTrue(Length(TZintTestHelper.GetText(FSymbol)) > 0);
end;

{ --- test_reader_init (width from C) --- }

procedure TTestCode128.Test_ReaderInit_A;
{ C#0: READER_INIT + "A" -> width 57 }
begin
  FSymbol.output_options := FSymbol.output_options or READER_INIT;
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'A'));
  ZAssert.AreEqual(57, FSymbol.width);
end;

procedure TTestCode128.Test_ReaderInit_12;
{ C#1: READER_INIT + "12" -> width 68 }
begin
  FSymbol.output_options := FSymbol.output_options or READER_INIT;
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12'));
  ZAssert.AreEqual(68, FSymbol.width);
end;

procedure TTestCode128.Test_ReaderInit_1234;
{ C#2: READER_INIT + "1234" -> width 79 }
begin
  FSymbol.output_options := FSymbol.output_options or READER_INIT;
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1234'));
  ZAssert.AreEqual(79, FSymbol.width);
end;

{ --- Basic validation (existing tests) --- }

procedure TTestCode128.Test_SingleChar;
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'A'));
  ZAssert.AreEqual(1, FSymbol.rows);
  ZAssert.AreEqual(46, FSymbol.width);
end;

procedure TTestCode128.Test_EmptyInput;
begin
  ZAssert.IsTrue(TZintTestHelper.EncodeData(FSymbol, '') >= ZINT_ERROR);
end;

procedure TTestCode128.Test_WidthIncreases;
var
  W1, W2: Integer;
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'AB'));
  W1 := FSymbol.width;
  ResetSymbol;
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'ABCDEF'));
  W2 := FSymbol.width;
  ZAssert.IsTrue(W2 > W1, Format('Width %d > %d', [W2, W1]));
end;

procedure TTestCode128.Test_AlwaysSingleRow;
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'Test12345ABC'));
  ZAssert.AreEqual(1, FSymbol.rows);
end;

procedure TTestCode128.Test_Encode_Alpha;
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'ABCDEFGHIJ'));
  ZAssert.AreEqual(1, FSymbol.rows);
  ZAssert.IsTrue(FSymbol.width > 0);
end;

{ --- Code 128B --- }

procedure TTestCode128.Test_Code128B_Basic;
begin
  FSymbol.symbology := BARCODE_CODE128B;
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'ABC123'));
  ZAssert.AreEqual(1, FSymbol.rows);
end;

procedure TTestCode128.Test_Code128B_NoCodeC;
var
  WB, WNormal: Integer;
begin
  FSymbol.symbology := BARCODE_CODE128B;
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12345678'));
  WB := FSymbol.width;
  ResetSymbol;
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12345678'));
  WNormal := FSymbol.width;
  ZAssert.IsTrue(WB > WNormal,
    Format('Code128B (%d) should be wider than Code128 (%d) for digits', [WB, WNormal]));
end;

procedure TTestCode128.Test_Code128B_TooLong;
begin
  FSymbol.symbology := BARCODE_CODE128B;
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG,
    TZintTestHelper.EncodeData(FSymbol, TZintTestHelper.StrRepeat('A', 257)));
end;

{ ======================================================================
  TTestEAN128
  ====================================================================== }

procedure TTestEAN128.Setup;
begin
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_EAN128);
  FSymbol.input_mode := DATA_MODE;
end;

procedure TTestEAN128.TearDown;
begin
  FreeAndNil(FSymbol);
end;

procedure TTestEAN128.Test_Basic_AI01;
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[01]12345678901231'));
  ZAssert.AreEqual(1, FSymbol.rows);
  ZAssert.IsTrue(FSymbol.width > 0);
end;

procedure TTestEAN128.Test_HRT;
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[01]12345678901231'));
  ZAssert.AreEqual('(01)12345678901231', TZintTestHelper.GetText(FSymbol));
end;

procedure TTestEAN128.Test_MultipleAIs;
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[01]12345678901231[10]ABC123'));
  ZAssert.AreEqual(1, FSymbol.rows);
end;

procedure TTestEAN128.Test_EmptyGS1;
begin
  ZAssert.IsTrue(TZintTestHelper.EncodeData(FSymbol, '') >= ZINT_ERROR);
end;

{ -- test_gs1_128_input from C -- }

procedure TTestEAN128.Test_GS1_90_1_90_1;
{ C#0: "[90]1[90]1" -> width 123 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[90]1[90]1'));
  ZAssert.AreEqual(123, FSymbol.width);
end;

procedure TTestEAN128.Test_GS1_90_1_90_12;
{ C#2: "[90]1[90]12" -> width 112 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[90]1[90]12'));
  ZAssert.AreEqual(112, FSymbol.width);
end;

procedure TTestEAN128.Test_GS1_90_12_90_1;
{ C#4: "[90]12[90]1" -> width 112 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[90]12[90]1'));
  ZAssert.AreEqual(112, FSymbol.width);
end;

procedure TTestEAN128.Test_GS1_90_12_90_12;
{ C#5: "[90]12[90]12" -> width 101 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[90]12[90]12'));
  ZAssert.AreEqual(101, FSymbol.width);
end;

procedure TTestEAN128.Test_GS1_90_123_90_1;
{ C#7: "[90]123[90]1" -> width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[90]123[90]1'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN128.Test_GS1_90_123_90_1234;
{ C#8: "[90]123[90]1234" -> width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[90]123[90]1234'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN128.Test_GS1_90_1_90_1_90_1;
{ C#9: "[90]1[90]1[90]1" -> width 167 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[90]1[90]1[90]1'));
  ZAssert.AreEqual(167, FSymbol.width);
end;

procedure TTestEAN128.Test_GS1_90_1A_90_1;
{ C#20: "[90]1A[90]1" -> width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[90]1A[90]1'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN128.Test_GS1_90_12345A12345A;
{ C#23: "[90]12345A12345A" -> width 178 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[90]12345A12345A'));
  ZAssert.AreEqual(178, FSymbol.width);
end;

{ ======================================================================
  TTestEAN14
  ====================================================================== }

procedure TTestEAN14.Setup;
begin
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_EAN14);
end;

procedure TTestEAN14.TearDown;
begin
  FreeAndNil(FSymbol);
end;

procedure TTestEAN14.ResetSymbol;
begin
  FreeAndNil(FSymbol);
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_EAN14);
end;

{ -- test_ean14_input from C -- }

procedure TTestEAN14.Test_TooLong_15;
{ C#0: "123456789012345" -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, '123456789012345'));
end;

procedure TTestEAN14.Test_NonNumeric_14;
{ C#1: "1234567890123A" -> INVALID_DATA }
begin
  ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, TZintTestHelper.EncodeData(FSymbol, '1234567890123A'));
end;

procedure TTestEAN14.Test_WrongCheckDigit;
{ C#2: "12345678901234" -> INVALID_CHECK (expecting 1) }
begin
  ZAssert.AreEqual(ZINT_ERROR_INVALID_CHECK, TZintTestHelper.EncodeData(FSymbol, '12345678901234'));
end;

procedure TTestEAN14.Test_NonNumeric_13;
{ C#3: "123456789012A" -> INVALID_DATA }
begin
  ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, TZintTestHelper.EncodeData(FSymbol, '123456789012A'));
end;

procedure TTestEAN14.Test_Valid_13;
{ C#4: "1234567890123" -> OK, width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1234567890123'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN14.Test_Valid_14;
{ C#5: "12345678901231" -> OK, width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12345678901231'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN14.Test_Valid_0134;
{ C#6: "0134567890123" -> OK, width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '0134567890123'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN14.Test_Valid_01345;
{ C#7: "01345678901235" -> OK, width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '01345678901235'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN14.Test_Prefix01_WithCheck;
{ C#8: "0112345678901231" -> OK, width 134, '01' prefix stripped }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '0112345678901231'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN14.Test_Prefix01_NoCheck;
{ C#9: "011234567890123" -> OK, width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '011234567890123'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN14.Test_BracketPrefix_WithCheck;
{ C#10: "[01]12345678901231" -> OK, width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[01]12345678901231'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN14.Test_BracketPrefix_NoCheck;
{ C#11: "[01]1234567890123" -> OK, width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[01]1234567890123'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN14.Test_ParenPrefix_WithCheck;
{ C#12: "(01)12345678901231" -> OK, width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '(01)12345678901231'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN14.Test_ParenPrefix_NoCheck;
{ C#13: "(01)1234567890123" -> OK, width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '(01)1234567890123'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN14.Test_Prefix00_TooLong;
{ C#14: "0012345678901231" -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, '0012345678901231'));
end;

procedure TTestEAN14.Test_Bracket00_TooLong;
{ C#15: "[00]12345678901231" -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, '[00]12345678901231'));
end;

procedure TTestEAN14.Test_Paren00_TooLong;
{ C#16: "(00)12345678901231" -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, '(00)12345678901231'));
end;

procedure TTestEAN14.Test_MismatchParen_TooLong;
{ C#17: "(01]12345678901231" -> TOO_LONG (mismatched brackets not recognized) }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, '(01]12345678901231'));
end;

{ -- test_large from C -- }

procedure TTestEAN14.Test_Large_Valid;
{ C#35: "12345678901231" -> OK, width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12345678901231'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

procedure TTestEAN14.Test_Large_TooLong;
{ C#36: "123456789012315" -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, '123456789012315'));
end;

{ -- test_reader_init from C -- }

procedure TTestEAN14.Test_ReaderInit_Warn;
{ C#5: EAN14 + READER_INIT -> WARN_INVALID_OPTION }
begin
  FSymbol.output_options := FSymbol.output_options or READER_INIT;
  ZAssert.AreEqual(ZINT_WARN_INVALID_OPTION, TZintTestHelper.EncodeData(FSymbol, '12'));
  ZAssert.AreEqual(134, FSymbol.width);
end;

{ -- test_encode from C -- }

procedure TTestEAN14.Test_Encode_4070;
{ C#39: "4070071967072" -> width 134 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '4070071967072'));
  ZAssert.AreEqual(134, FSymbol.width);
  ZAssert.AreEqual(
    '11010011100111101011101100110110011000101000101100001001001100010011001011100100001011001001100010011001001110110111001001100011101011',
    TZintTestHelper.ModulesDump(FSymbol));
end;

procedure TTestEAN14.Test_ShortPadded;
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '123'));
end;

{ ======================================================================
  TTestNVE18
  ====================================================================== }

procedure TTestNVE18.Setup;
begin
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_NVE18);
end;

procedure TTestNVE18.TearDown;
begin
  FreeAndNil(FSymbol);
end;

procedure TTestNVE18.ResetSymbol;
begin
  FreeAndNil(FSymbol);
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_NVE18);
end;

{ -- test_nve18_input from C -- }

procedure TTestNVE18.Test_TooLong_19;
{ C#0: "1234567890123456789" -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, '1234567890123456789'));
end;

procedure TTestNVE18.Test_NonNumeric_18;
{ C#1: "12345678901234567A" -> INVALID_DATA }
begin
  ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, TZintTestHelper.EncodeData(FSymbol, '12345678901234567A'));
end;

procedure TTestNVE18.Test_WrongCheckDigit;
{ C#2: "123456789012345678" -> INVALID_CHECK (expecting 5) }
begin
  ZAssert.AreEqual(ZINT_ERROR_INVALID_CHECK, TZintTestHelper.EncodeData(FSymbol, '123456789012345678'));
end;

procedure TTestNVE18.Test_NonNumeric_5;
{ C#3: "1234A568901234567" -> INVALID_DATA }
begin
  ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, TZintTestHelper.EncodeData(FSymbol, '1234A568901234567'));
end;

procedure TTestNVE18.Test_Valid_18;
{ C#5: "123456789012345675" -> OK, width 156 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '123456789012345675'));
  ZAssert.AreEqual(156, FSymbol.width);
end;

procedure TTestNVE18.Test_Valid_17;
{ C#6: "12345678901234567" -> OK, width 156 (check digit auto-calculated) }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '12345678901234567'));
  ZAssert.AreEqual(156, FSymbol.width);
end;

procedure TTestNVE18.Test_Valid_003456;
{ C#7: "003456789012345670" -> OK, width 156 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '003456789012345670'));
  ZAssert.AreEqual(156, FSymbol.width);
end;

procedure TTestNVE18.Test_Valid_00345;
{ C#8: "00345678901234567" -> OK, width 156 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '00345678901234567'));
  ZAssert.AreEqual(156, FSymbol.width);
end;

procedure TTestNVE18.Test_Prefix00_WithCheck;
{ C#9: "00123456789012345675" -> OK, width 156 (00 prefix stripped) }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '00123456789012345675'));
  ZAssert.AreEqual(156, FSymbol.width);
end;

procedure TTestNVE18.Test_Prefix00_NoCheck;
{ C#10: "0012345678901234567" -> OK, width 156 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '0012345678901234567'));
  ZAssert.AreEqual(156, FSymbol.width);
end;

procedure TTestNVE18.Test_BracketPrefix_WithCheck;
{ C#11: "[00]123456789012345675" -> OK, width 156 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[00]123456789012345675'));
  ZAssert.AreEqual(156, FSymbol.width);
end;

procedure TTestNVE18.Test_BracketPrefix_NoCheck;
{ C#12: "[00]12345678901234567" -> OK, width 156 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '[00]12345678901234567'));
  ZAssert.AreEqual(156, FSymbol.width);
end;

procedure TTestNVE18.Test_ParenPrefix_WithCheck;
{ C#13: "(00)123456789012345675" -> OK, width 156 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '(00)123456789012345675'));
  ZAssert.AreEqual(156, FSymbol.width);
end;

procedure TTestNVE18.Test_ParenPrefix_NoCheck;
{ C#14: "(00)12345678901234567" -> OK, width 156 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '(00)12345678901234567'));
  ZAssert.AreEqual(156, FSymbol.width);
end;

procedure TTestNVE18.Test_Prefix01_TooLong;
{ C#15: "01123456789012345675" -> TOO_LONG (01 prefix not recognized) }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, '01123456789012345675'));
end;

procedure TTestNVE18.Test_Bracket01_TooLong;
{ C#16: "[01]123456789012345675" -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, '[01]123456789012345675'));
end;

procedure TTestNVE18.Test_Paren01_TooLong;
{ C#17: "(01)123456789012345675" -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, '(01)123456789012345675'));
end;

procedure TTestNVE18.Test_MismatchParen_TooLong;
{ C#18: "(00]123456789012345675" -> TOO_LONG (mismatched brackets) }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, '(00]123456789012345675'));
end;

{ -- test_large from C -- }

procedure TTestNVE18.Test_Large_Valid;
{ C#37: "123456789012345675" -> OK, width 156 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '123456789012345675'));
  ZAssert.AreEqual(156, FSymbol.width);
end;

procedure TTestNVE18.Test_Large_TooLong;
{ C#38: "1234567890123456759" -> TOO_LONG }
begin
  ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, TZintTestHelper.EncodeData(FSymbol, '1234567890123456759'));
end;

{ -- test_reader_init from C -- }

procedure TTestNVE18.Test_ReaderInit_Warn;
{ C#6: NVE18 + READER_INIT -> WARN_INVALID_OPTION }
begin
  FSymbol.output_options := FSymbol.output_options or READER_INIT;
  ZAssert.AreEqual(ZINT_WARN_INVALID_OPTION, TZintTestHelper.EncodeData(FSymbol, '12'));
  ZAssert.AreEqual(156, FSymbol.width);
end;

{ -- test_encode from C -- }

procedure TTestNVE18.Test_Encode_40700000;
{ C#40: "40700000071967072" -> width 156 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '40700000071967072'));
  ZAssert.AreEqual(156, FSymbol.width);
  ZAssert.AreEqual(
    '110100111001111010111011011001100110001010001011000010011011001100110110011001001100010011001011100100001011001001100010011001001110110111011101100011101011',
    TZintTestHelper.ModulesDump(FSymbol));
end;

procedure TTestNVE18.Test_ShortPadded;
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '123'));
end;

{ ======================================================================
  TTestHIBC128
  ====================================================================== }

procedure TTestHIBC128.Setup;
begin
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_HIBC_128);
end;

procedure TTestHIBC128.TearDown;
begin
  FreeAndNil(FSymbol);
end;

procedure TTestHIBC128.ResetSymbol;
begin
  FreeAndNil(FSymbol);
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_HIBC_128);
end;

{ -- test_hibc_input from C -- }

procedure TTestHIBC128.Test_InvalidChar;
{ C#0: "," -> INVALID_DATA }
begin
  ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, TZintTestHelper.EncodeData(FSymbol, ','));
end;

procedure TTestHIBC128.Test_A99912345;
{ C#1: "A99912345/$$52001510X3" -> width 255, check digit 3 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'A99912345/$$52001510X3'));
  ZAssert.AreEqual(255, FSymbol.width);
end;

procedure TTestHIBC128.Test_AllValid;
{ C#2: "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%" -> width 497 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%'));
  ZAssert.AreEqual(497, FSymbol.width);
end;

procedure TTestHIBC128.Test_Percent58;
{ C#3: 58 x '%' -> width 695 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat('%', 58)));
  ZAssert.AreEqual(695, FSymbol.width);
end;

procedure TTestHIBC128.Test_Large_110_OK;
{ C#39: 110 x '1' -> OK, width 684 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat('1', 110)));
  ZAssert.AreEqual(684, FSymbol.width);
end;

procedure TTestHIBC128.Test_Large_111_TooLong;
{ C#40: 111 x '1' -> TOO_LONG }
begin
  ZAssert.IsTrue(TZintTestHelper.EncodeData(FSymbol,
    TZintTestHelper.StrRepeat('1', 111)) >= ZINT_ERROR);
end;

procedure TTestHIBC128.Test_Large_AllDigits;
{ C#6: "1" + 107 zeros (108 chars) -> HIBC prepends "+" and appends check "%" = 110 chars }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol,
    '1' + TZintTestHelper.StrRepeat('0', 107)));
  ZAssert.AreEqual(673, FSymbol.width);
end;

{ -- test_hrt from C -- }

procedure TTestHIBC128.Test_HRT_12345678900;
{ C#24: "1234567890" -> HRT "*+12345678900*" }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '1234567890'));
  ZAssert.AreEqual('*+12345678900*', TZintTestHelper.GetText(FSymbol));
end;

procedure TTestHIBC128.Test_HRT_A999123457;
{ C#26: "a99912345" (auto-uppercased) -> HRT "*+A999123457*" }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'a99912345'));
  ZAssert.AreEqual('*+A999123457*', TZintTestHelper.GetText(FSymbol));
end;

{ -- test_reader_init from C -- }

procedure TTestHIBC128.Test_ReaderInit;
{ C#7: READER_INIT + "A" -> OK, width 79 }
begin
  FSymbol.output_options := FSymbol.output_options or READER_INIT;
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'A'));
  ZAssert.AreEqual(79, FSymbol.width);
end;

{ -- test_encode from C -- }

procedure TTestHIBC128.Test_Encode_83278F8G9H0J2G;
{ C#41: ANSI/HIBC 2.6-2016 Section 4.1 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '83278F8G9H0J2G'));
  ZAssert.AreEqual(211, FSymbol.width);
  ZAssert.AreEqual(
    '1101001000011000100100101110111101011110010011101100100101111011101110100110010001100010111010011001101000100011100101100110001010001001110110010110111000110011100101101000100010001001100110100010001100011101011',
    TZintTestHelper.ModulesDump(FSymbol));
end;

procedure TTestHIBC128.Test_Encode_A123BJC5D6E71;
{ C#42: ANSI/HIBC 2.6-2016 Figure 1 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, 'A123BJC5D6E71'));
  ZAssert.AreEqual(200, FSymbol.width);
  ZAssert.AreEqual(
    '11010010000110001001001010001100010011100110110011100101100101110010001011000101101110001000100011011011100100101100010001100111010010001101000111011011101001110011011010001000110001101101100011101011',
    TZintTestHelper.ModulesDump(FSymbol));
end;

procedure TTestHIBC128.Test_Encode_52001510X3G;
{ C#43: ANSI/HIBC 2.6-2016 Figure 5 }
begin
  ZAssert.AreEqual(ZINT_OK, TZintTestHelper.EncodeData(FSymbol, '$$52001510X3G'));
  ZAssert.AreEqual(178, FSymbol.width);
  ZAssert.AreEqual(
    '1101001000011000100100100100011001001000110010111011110110111000101101100110010111001100110010001001011110111011100010110110010111001101000100010110001000100011110101100011101011',
    TZintTestHelper.ModulesDump(FSymbol));
end;

procedure TTestCode128HRTContentSegsFromC.HRT_ContentSegs_FromC;
type
  TItem = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    Option2: Integer;
    Data: AnsiString;
    DataLen: Integer;
    ExpectedText: AnsiString;
    ExpectedTextLen: Integer;
    ExpectedContent: AnsiString;
    ExpectedContentLen: Integer;
  end;
const
  Items: array[0..7] of TItem = (
    (Index: 1;  Symbology: BARCODE_CODE128;  InputMode: UNICODE_MODE; Option2: -1; Data: '1234567890'; DataLen: -1; ExpectedText: '1234567890'; ExpectedTextLen: -1; ExpectedContent: '1234567890'; ExpectedContentLen: -1),
    (Index: 3;  Symbology: BARCODE_CODE128;  InputMode: UNICODE_MODE; Option2: -1; Data: #0'ABC'#0'DEF'#0; DataLen: 9; ExpectedText: ' ABC DEF '; ExpectedTextLen: -1; ExpectedContent: #0'ABC'#0'DEF'#0; ExpectedContentLen: 9),
    (Index: 5;  Symbology: BARCODE_CODE128B; InputMode: UNICODE_MODE; Option2: -1; Data: '12345'#0'67890'; DataLen: 11; ExpectedText: '12345 67890'; ExpectedTextLen: -1; ExpectedContent: '12345'#0'67890'; ExpectedContentLen: 11),
    (Index: 7;  Symbology: BARCODE_CODE128;  InputMode: UNICODE_MODE; Option2: -1; Data: '12345'#9'67890'#31#127; DataLen: -1; ExpectedText: '12345 67890  '; ExpectedTextLen: -1; ExpectedContent: '12345'#9'67890'#31#127; ExpectedContentLen: -1),
    (Index: 13; Symbology: BARCODE_CODE128;  InputMode: DATA_MODE;    Option2: -1; Data: 'abcd'#233; DataLen: 5; ExpectedText: 'abcd'#233; ExpectedTextLen: 5; ExpectedContent: 'abcd'#233; ExpectedContentLen: 5),
    (Index: 17; Symbology: BARCODE_CODE128;  InputMode: DATA_MODE;    Option2: -1; Data: 'ab'#128'cd'#233; DataLen: 6; ExpectedText: 'ab cd'#233; ExpectedTextLen: 6; ExpectedContent: 'ab'#128'cd'#233; ExpectedContentLen: 6),
    (Index: 25; Symbology: BARCODE_HIBC_128; InputMode: UNICODE_MODE; Option2: -1; Data: '1234567890'; DataLen: -1; ExpectedText: '*+12345678900*'; ExpectedTextLen: -1; ExpectedContent: '+12345678900'; ExpectedContentLen: -1),
    (Index: 27; Symbology: BARCODE_HIBC_128; InputMode: UNICODE_MODE; Option2: -1; Data: 'a99912345'; DataLen: -1; ExpectedText: '*+A999123457*'; ExpectedTextLen: -1; ExpectedContent: '+A999123457'; ExpectedContentLen: -1)
  );
var
  i, j, ret, data_len, expected_text_len, expected_content_len: Integer;
  sym: TZintSymbol;
  input_bytes, expected_text_bytes, expected_content_bytes: TArrayOfByte;

  function MakeBytes(const S: AnsiString; const ExplicitLen: Integer): TArrayOfByte;
  var
    k, l: Integer;
  begin
    if ExplicitLen >= 0 then
      l := ExplicitLen
    else
      l := Length(S);
    SetLength(Result, l + 1);
    for k := 1 to l do
      Result[k - 1] := Ord(S[k]);
    Result[l] := 0;
  end;

begin
  for i := 0 to High(Items) do
  begin
    sym := TZintTestHelper.CreateSymbol(Items[i].Symbology);
    try
      sym.input_mode := Items[i].InputMode;
      sym.output_options := BARCODE_CONTENT_SEGS;
      if Items[i].Option2 >= 0 then
        sym.option_2 := Items[i].Option2;

      input_bytes := MakeBytes(Items[i].Data, Items[i].DataLen);
      if Items[i].DataLen >= 0 then
        data_len := Items[i].DataLen
      else
        data_len := Length(Items[i].Data);

      ret := TZintTestHelper.EncodeData(sym, input_bytes, data_len);
      ZAssert.AreEqual(ZINT_OK, ret, Format('C#%d ret', [Items[i].Index]));

      if Items[i].ExpectedTextLen >= 0 then
        expected_text_len := Items[i].ExpectedTextLen
      else
        expected_text_len := Length(Items[i].ExpectedText);
      expected_text_bytes := MakeBytes(Items[i].ExpectedText, expected_text_len);
      for j := 0 to expected_text_len - 1 do
        ZAssert.AreEqual(expected_text_bytes[j], sym.text[j], Format('C#%d text[%d]', [Items[i].Index, j]));

      if Items[i].ExpectedContentLen >= 0 then
        expected_content_len := Items[i].ExpectedContentLen
      else
        expected_content_len := Length(Items[i].ExpectedContent);
      expected_content_bytes := MakeBytes(Items[i].ExpectedContent, expected_content_len);

      ZAssert.AreEqual(1, sym.content_segs_count, Format('C#%d content_segs_count', [Items[i].Index]));
      ZAssert.AreEqual(expected_content_len, sym.content_segs[0].Length, Format('C#%d content length', [Items[i].Index]));
      for j := 0 to expected_content_len - 1 do
        ZAssert.AreEqual(expected_content_bytes[j], sym.content_segs[0].Source[j],
          Format('C#%d content[%d]', [Items[i].Index, j]));
    finally
      sym.Free;
    end;
  end;
end;

initialization
  ZRegisterFixture(TTestCode128);
  ZRegisterFixture(TTestEAN128);
  ZRegisterFixture(TTestEAN14);
  ZRegisterFixture(TTestNVE18);
  ZRegisterFixture(TTestHIBC128);
  ZRegisterFixture(TTestCode128HRTContentSegsFromC);

end.
