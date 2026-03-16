unit Test_Plessey;

{
  DUnitX-Tests fuer zint_plessey.pas
  Testdaten aus test_plessey.c (Zint commit b3a3c0d, 2026-03-13)
  Testet: Plessey (UK), MSI Plessey (alle Check-Optionen 0-6)
}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TestHelper_Zint,
  zint;

type
  [TestFixture]
  TTestPlessey = class
  public
    { test_large }
    [Test] procedure Large_Max67_OK;
    [Test] procedure Large_68_TooLong;

    { test_input }
    [Test] procedure Input_HexA_OK;
    [Test] procedure Input_G_InvalidChar;

    { test_encode }
    [Test] procedure Encode_0123456789ABCDEF;

    { test_hrt }
    [Test] procedure HRT_Default_ABCDEF;
    [Test] procedure HRT_ShowCheck_ABCDEF;
    [Test] procedure HRT_Default_1;
    [Test] procedure HRT_ShowCheck_1;
    [Test] procedure HRT_Default_7;
    [Test] procedure HRT_ShowCheck_7;
    [Test] procedure HRT_Default_75;
    [Test] procedure HRT_ShowCheck_75;
    [Test] procedure HRT_Default_993;
    [Test] procedure HRT_ShowCheck_993;
  end;

  [TestFixture]
  TTestMSIPlessey = class
  public
    { test_large }
    [Test] procedure Large_Max92_NoMod_OK;
    [Test] procedure Large_93_TooLong;

    { test_input }
    [Test] procedure Input_Digit_OK;
    [Test] procedure Input_Alpha_InvalidChar;
    [Test] procedure Input_Option2_Negative_Ignored;
    [Test] procedure Input_Option2_7_Ignored;

    { test_encode - option 0 (no check) }
    [Test] procedure Encode_NoMod_1234567890;

    { test_encode - option 1 (mod 10) }
    [Test] procedure Encode_Mod10_1234567890;

    { test_encode - option 2 (mod 10+10) }
    [Test] procedure Encode_Mod1010_1234567890;

    { test_encode - option 3 (mod 11 IBM) }
    [Test] procedure Encode_Mod11_1234567890;
    [Test] procedure Encode_Mod11_2211;    { check digit = 10 }

    { test_encode - option 4 (mod 11+10 IBM) }
    [Test] procedure Encode_Mod1110_1234567890;
    [Test] procedure Encode_Mod1110_2211;   { check digit = 10 }

    { test_encode - option 5 (mod 11 NCR) }
    [Test] procedure Encode_Mod11NCR_1234567890;

    { test_encode - option 6 (mod 11+10 NCR) }
    [Test] procedure Encode_Mod1110NCR_1234567890;

    { test_hrt - check digit values }
    [Test] procedure HRT_NoMod_1234567;
    [Test] procedure HRT_Mod10_1234567;
    [Test] procedure HRT_Mod1010_1234567;
    [Test] procedure HRT_Mod11_1234567;
    [Test] procedure HRT_Mod1110_1234567;
    [Test] procedure HRT_Mod11NCR_1234567;
    [Test] procedure HRT_Mod1110NCR_1234567;
    [Test] procedure HRT_Mod11_2211;  { check = 10 }

    { test_large - max92 with various options }
    [Test] procedure Large_Max92_Mod10_OK;
    [Test] procedure Large_93_Mod10_TooLong;
    [Test] procedure Large_Max92_Mod1010_OK;
    [Test] procedure Large_93_Mod1010_TooLong;
    [Test] procedure Large_92_Mod11_OK;
    [Test] procedure Large_93_Mod11_TooLong;
    [Test] procedure Large_92_Mod11_4_OK;
    [Test] procedure Large_93_Mod11_4_TooLong;
    [Test] procedure Large_92_Mod1110_OK;
    [Test] procedure Large_93_Mod1110_TooLong;
    [Test] procedure Large_92_Mod1110_4_OK;
    [Test] procedure Large_93_Mod1110_4_TooLong;
    [Test] procedure Large_92_Mod11NCR_OK;
    [Test] procedure Large_93_Mod11NCR_TooLong;
    [Test] procedure Large_92_Mod1110NCR_OK;
    [Test] procedure Large_93_Mod1110NCR_TooLong;
    [Test] procedure Large_Max92_Mod1110_Max;

    { test_hrt - additional }
    [Test] procedure HRT_NoMod_Opt0_1234567;
    [Test] procedure HRT_Mod10_NoShow_1234567;
    [Test] procedure HRT_Mod10_9999999999;
    [Test] procedure HRT_Mod1010_NoShow_1234567;
    [Test] procedure HRT_Mod1010_9999999999;
    [Test] procedure HRT_Mod11_NoShow_1234567;
    [Test] procedure HRT_Mod11_9999999999;
    [Test] procedure HRT_Mod1110_NoShow_1234567;
    [Test] procedure HRT_Mod1110_9999999999;
    [Test] procedure HRT_Mod11NCR_NoShow_1234567;
    [Test] procedure HRT_Mod11NCR_9999999999;
    [Test] procedure HRT_Mod1110NCR_NoShow_1234567;
    [Test] procedure HRT_Mod1110NCR_9999999999;
    [Test] procedure HRT_Mod10_123456;
    [Test] procedure HRT_Mod1010_123456;
    [Test] procedure HRT_Mod11_123456;
    [Test] procedure HRT_Mod1110_123456;
    [Test] procedure HRT_Mod11_NoShow_2211;
    [Test] procedure HRT_Mod1110_2211;
    [Test] procedure HRT_Mod1110_NoShow_2211;
  end;

  [TestFixture]
  TTestPlesseyHRTContentSegsFromC = class
  public
    [Test] procedure HRT_ContentSegs_FromC;
  end;

implementation

{ ---------- TTestPlessey ---------- }

procedure TTestPlessey.Large_Max67_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('A', 67));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1139, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestPlessey.Large_68_TooLong;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('A', 68));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 370: Input length 68 too long (maximum 67)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPlessey.Input_HexA_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(83, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestPlessey.Input_G_InvalidChar;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    ret := TZintTestHelper.EncodeData(sym, 'G');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 371: Invalid character at position 1 in input (digits and "ABCDEF" only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPlessey.Encode_0123456789ABCDEF;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_encode[9] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    ret := TZintTestHelper.EncodeData(sym, '0123456789ABCDEF');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(323, sym.width, 'width');
    Assert.AreEqual(
      '11101110100011101000100010001000111010001000100010001110100010001110111010001000100010001110100011101000111010001000111011101000111011101110100010001000100011101110100010001110100011101000111011101110100011101000100011101110111010001110111010001110111011101110111011101110111010001000111010001000100010001110001000101110111',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

{ Plessey HRT tests }

procedure TTestPlessey.HRT_Default_ABCDEF;
var sym: TZintSymbol;
begin
  { test_hrt[56]: default -> HRT = "0123456789ABCDEF" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    TZintTestHelper.EncodeData(sym, '0123456789ABCDEF');
    Assert.AreEqual('0123456789ABCDEF', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPlessey.HRT_ShowCheck_ABCDEF;
var sym: TZintSymbol;
begin
  { test_hrt[58]: option_2=1 -> HRT = "0123456789ABCDEF90" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '0123456789ABCDEF');
    Assert.AreEqual('0123456789ABCDEF90', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPlessey.HRT_Default_1;
var sym: TZintSymbol;
begin
  { test_hrt[60]: "1" default -> HRT = "1" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    TZintTestHelper.EncodeData(sym, '1');
    Assert.AreEqual('1', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPlessey.HRT_ShowCheck_1;
var sym: TZintSymbol;
begin
  { test_hrt[62]: "1" option_2=1 -> HRT = "173" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '1');
    Assert.AreEqual('173', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPlessey.HRT_Default_7;
var sym: TZintSymbol;
begin
  { test_hrt[64]: "7" default -> HRT = "7" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    TZintTestHelper.EncodeData(sym, '7');
    Assert.AreEqual('7', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPlessey.HRT_ShowCheck_7;
var sym: TZintSymbol;
begin
  { test_hrt[66]: "7" option_2=1 -> HRT = "758" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '7');
    Assert.AreEqual('758', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPlessey.HRT_Default_75;
var sym: TZintSymbol;
begin
  { test_hrt[68]: "75" default -> HRT = "75" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    TZintTestHelper.EncodeData(sym, '75');
    Assert.AreEqual('75', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPlessey.HRT_ShowCheck_75;
var sym: TZintSymbol;
begin
  { test_hrt[70]: "75" option_2=1 -> HRT = "7580" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '75');
    Assert.AreEqual('7580', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPlessey.HRT_Default_993;
var sym: TZintSymbol;
begin
  { test_hrt[72]: "993" default -> HRT = "993" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    TZintTestHelper.EncodeData(sym, '993');
    Assert.AreEqual('993', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPlessey.HRT_ShowCheck_993;
var sym: TZintSymbol;
begin
  { test_hrt[74]: "993" option_2=1 -> HRT = "993AA" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLESSEY);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '993');
    Assert.AreEqual('993AA', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

{ ---------- TTestMSIPlessey ---------- }

procedure TTestMSIPlessey.Large_Max92_NoMod_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_large[0]: 92 * "9" -> OK, width 1111 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 92));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1111, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Large_93_TooLong;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 93));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 372: Input length 93 too long (maximum 92)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Input_Digit_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    ret := TZintTestHelper.EncodeData(sym, '1');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(19, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Input_Alpha_InvalidChar;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 377: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Input_Option2_Negative_Ignored;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_input[2]: option_2 = -2 -> ignored, treated as 0 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := -2;
    ret := TZintTestHelper.EncodeData(sym, '1');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(19, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Input_Option2_7_Ignored;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_input[3]: option_2 = 7 -> ignored, treated as 0 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 7;
    ret := TZintTestHelper.EncodeData(sym, '1');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(19, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Encode_NoMod_1234567890;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_encode[0]: option_2=0 (no check) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567890');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(127, sym.width, 'width');
    Assert.AreEqual(
      '1101001001001101001001101001001001101101001101001001001101001101001101101001001101101101101001001001101001001101001001001001001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Encode_Mod10_1234567890;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_encode[1]: option_2=1 (mod 10) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '1234567890');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(139, sym.width, 'width');
    Assert.AreEqual(
      '1101001001001101001001101001001001101101001101001001001101001101001101101001001101101101101001001001101001001101001001001001001001101101001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Encode_Mod1010_1234567890;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_encode[2]: option_2=2 (mod 10+10) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 2;
    ret := TZintTestHelper.EncodeData(sym, '1234567890');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(151, sym.width, 'width');
    Assert.AreEqual(
      '1101001001001101001001101001001001101101001101001001001101001101001101101001001101101101101001001001101001001101001001001001001001101101001001001101001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Encode_Mod11_1234567890;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_encode[3]: option_2=3 (mod 11 IBM) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 3;
    ret := TZintTestHelper.EncodeData(sym, '1234567890');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(139, sym.width, 'width');
    Assert.AreEqual(
      '1101001001001101001001101001001001101101001101001001001101001101001101101001001101101101101001001001101001001101001001001001001001101101001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Encode_Mod11_2211;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_encode[7]: "2211" mod-11 produces check digit "10" (two chars) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 3;
    ret := TZintTestHelper.EncodeData(sym, '2211');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(79, sym.width, 'width');
    Assert.AreEqual(
      '1101001001101001001001101001001001001101001001001101001001001101001001001001001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Encode_Mod1110_1234567890;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_encode[4]: option_2=4 (mod 11+10 IBM) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 4;
    ret := TZintTestHelper.EncodeData(sym, '1234567890');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(151, sym.width, 'width');
    Assert.AreEqual(
      '1101001001001101001001101001001001101101001101001001001101001101001101101001001101101101101001001001101001001101001001001001001001101101001001001101001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Encode_Mod1110_2211;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_encode[8]: "2211" mod-11+10, check "10" + mod10 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 4;
    ret := TZintTestHelper.EncodeData(sym, '2211');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(91, sym.width, 'width');
    Assert.AreEqual(
      '1101001001101001001001101001001001001101001001001101001001001101001001001001001001001001001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Encode_Mod11NCR_1234567890;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_encode[5]: option_2=5 (mod 11 NCR wrap=9) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 5;
    ret := TZintTestHelper.EncodeData(sym, '1234567890');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(139, sym.width, 'width');
    Assert.AreEqual(
      '1101001001001101001001101001001001101101001101001001001101001101001101101001001101101101101001001001101001001101001001001001001001001001001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Encode_Mod1110NCR_1234567890;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_encode[6]: option_2=6 (mod 11+10 NCR wrap=9) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 6;
    ret := TZintTestHelper.EncodeData(sym, '1234567890');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(151, sym.width, 'width');
    Assert.AreEqual(
      '1101001001001101001001101001001001101101001101001001001101001101001101101001001101101101101001001001101001001101001001001001001001001001001101101101001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

{ HRT tests - verify check digit values via symbol.text }

procedure TTestMSIPlessey.HRT_NoMod_1234567;
var
  sym: TZintSymbol;
begin
  { test_hrt[0]: no check -> HRT = "1234567" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('1234567', TZintTestHelper.GetText(sym), 'text');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.HRT_Mod10_1234567;
var
  sym: TZintSymbol;
begin
  { test_hrt[4]: mod10 -> HRT = "12345674" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('12345674', TZintTestHelper.GetText(sym), 'text');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.HRT_Mod1010_1234567;
var
  sym: TZintSymbol;
begin
  { test_hrt[10]: mod10+10 -> HRT = "123456741" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('123456741', TZintTestHelper.GetText(sym), 'text');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.HRT_Mod11_1234567;
var
  sym: TZintSymbol;
begin
  { test_hrt[16]: mod11 IBM -> HRT = "12345674" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 3;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('12345674', TZintTestHelper.GetText(sym), 'text');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.HRT_Mod1110_1234567;
var
  sym: TZintSymbol;
begin
  { test_hrt[22]: mod11+10 IBM -> HRT = "123456741" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 4;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('123456741', TZintTestHelper.GetText(sym), 'text');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.HRT_Mod11NCR_1234567;
var
  sym: TZintSymbol;
begin
  { test_hrt[28]: mod11 NCR -> HRT = "12345679" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 5;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('12345679', TZintTestHelper.GetText(sym), 'text');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.HRT_Mod1110NCR_1234567;
var
  sym: TZintSymbol;
begin
  { test_hrt[34]: mod11+10 NCR -> HRT = "123456790" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 6;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('123456790', TZintTestHelper.GetText(sym), 'text');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.HRT_Mod11_2211;
var
  sym: TZintSymbol;
begin
  { test_hrt[48]: mod11 "2211" -> check=10, HRT = "221110" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 3;
    TZintTestHelper.EncodeData(sym, '2211');
    Assert.AreEqual('221110', TZintTestHelper.GetText(sym), 'text');
  finally
    sym.Free;
  end;
end;

{ Additional HRT tests from C test_hrt }

procedure TTestMSIPlessey.HRT_NoMod_Opt0_1234567;
var sym: TZintSymbol;
begin
  { test_hrt[2]: opt2=0 explicit, "1234567" -> "1234567" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 0;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('1234567', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod10_NoShow_1234567;
var sym: TZintSymbol;
begin
  { test_hrt[6]: opt2=1+10, "1234567" -> "1234567" (check hidden) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 11;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('1234567', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod10_9999999999;
var sym: TZintSymbol;
begin
  { test_hrt[8]: opt2=1, "9999999999" -> "99999999990" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '9999999999');
    Assert.AreEqual('99999999990', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod1010_NoShow_1234567;
var sym: TZintSymbol;
begin
  { test_hrt[12]: opt2=2+10, "1234567" -> "1234567" (check hidden) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 12;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('1234567', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod1010_9999999999;
var sym: TZintSymbol;
begin
  { test_hrt[14]: opt2=2, "9999999999" -> "999999999900" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '9999999999');
    Assert.AreEqual('999999999900', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod11_NoShow_1234567;
var sym: TZintSymbol;
begin
  { test_hrt[18]: opt2=3+10, "1234567" -> "1234567" (check hidden) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 13;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('1234567', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod11_9999999999;
var sym: TZintSymbol;
begin
  { test_hrt[20]: opt2=3, "9999999999" -> "99999999995" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 3;
    TZintTestHelper.EncodeData(sym, '9999999999');
    Assert.AreEqual('99999999995', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod1110_NoShow_1234567;
var sym: TZintSymbol;
begin
  { test_hrt[24]: opt2=4+10, "1234567" -> "1234567" (check hidden) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 14;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('1234567', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod1110_9999999999;
var sym: TZintSymbol;
begin
  { test_hrt[26]: opt2=4, "9999999999" -> "999999999959" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 4;
    TZintTestHelper.EncodeData(sym, '9999999999');
    Assert.AreEqual('999999999959', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod11NCR_NoShow_1234567;
var sym: TZintSymbol;
begin
  { test_hrt[30]: opt2=5+10, "1234567" -> "1234567" (check hidden) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 15;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('1234567', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod11NCR_9999999999;
var sym: TZintSymbol;
begin
  { test_hrt[32]: opt2=5, "9999999999" -> "999999999910" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 5;
    TZintTestHelper.EncodeData(sym, '9999999999');
    Assert.AreEqual('999999999910', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod1110NCR_NoShow_1234567;
var sym: TZintSymbol;
begin
  { test_hrt[36]: opt2=6+10, "1234567" -> "1234567" (check hidden) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 16;
    TZintTestHelper.EncodeData(sym, '1234567');
    Assert.AreEqual('1234567', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod1110NCR_9999999999;
var sym: TZintSymbol;
begin
  { test_hrt[38]: opt2=6, "9999999999" -> "9999999999109" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 6;
    TZintTestHelper.EncodeData(sym, '9999999999');
    Assert.AreEqual('9999999999109', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod10_123456;
var sym: TZintSymbol;
begin
  { test_hrt[40]: opt2=1, "123456" -> "1234566" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '123456');
    Assert.AreEqual('1234566', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod1010_123456;
var sym: TZintSymbol;
begin
  { test_hrt[42]: opt2=2, "123456" -> "12345666" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '123456');
    Assert.AreEqual('12345666', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod11_123456;
var sym: TZintSymbol;
begin
  { test_hrt[44]: opt2=3, "123456" -> "1234560" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 3;
    TZintTestHelper.EncodeData(sym, '123456');
    Assert.AreEqual('1234560', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod1110_123456;
var sym: TZintSymbol;
begin
  { test_hrt[46]: opt2=4, "123456" -> "12345609" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 4;
    TZintTestHelper.EncodeData(sym, '123456');
    Assert.AreEqual('12345609', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod11_NoShow_2211;
var sym: TZintSymbol;
begin
  { test_hrt[50]: opt2=3+10, "2211" -> "2211" (check "10" hidden) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 13;
    TZintTestHelper.EncodeData(sym, '2211');
    Assert.AreEqual('2211', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod1110_2211;
var sym: TZintSymbol;
begin
  { test_hrt[52]: opt2=4, "2211" -> "2211100" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 4;
    TZintTestHelper.EncodeData(sym, '2211');
    Assert.AreEqual('2211100', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.HRT_Mod1110_NoShow_2211;
var sym: TZintSymbol;
begin
  { test_hrt[54]: opt2=4+10, "2211" -> "2211" (check hidden) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 14;
    TZintTestHelper.EncodeData(sym, '2211');
    Assert.AreEqual('2211', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_Max92_Mod10_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[2]: 92 * "9" mod10 -> OK, width 1123 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 92));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1123, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_93_Mod10_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[3]: mod10 93x"9" -> TOO_LONG }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 93));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_Max92_Mod1010_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[4]: 92 * "9" mod10+10 -> OK, width 1135 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 2;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 92));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_93_Mod1010_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[5]: mod10+10 93x"9" -> TOO_LONG }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 2;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 93));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_92_Mod11_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[6]: mod11 92x"9" -> OK, width 1123 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 3;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 92));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1123, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_93_Mod11_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[7]: mod11 93x"9" -> TOO_LONG }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 3;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 93));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_92_Mod11_4_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[8]: mod11 92x"4" -> OK, width 1135 (check digit "10" = 2 chars) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 3;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('4', 92));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_93_Mod11_4_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[9]: mod11 93x"4" -> TOO_LONG }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 3;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('4', 93));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_92_Mod1110_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[10]: mod11+10 92x"9" -> OK, width 1135 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 4;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 92));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_93_Mod1110_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[11]: mod11+10 93x"9" -> TOO_LONG }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 4;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 93));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_92_Mod1110_4_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[12]: mod11+10 92x"4" -> OK, width 1147 (check "10" + mod10) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 4;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('4', 92));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1147, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_93_Mod1110_4_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[13]: mod11+10 93x"4" -> TOO_LONG }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 4;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('4', 93));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_92_Mod11NCR_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[14]: mod11 NCR 92x"9" -> OK, width 1123 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 5;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 92));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1123, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_93_Mod11NCR_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[15]: mod11 NCR 93x"9" -> TOO_LONG }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 5;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 93));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_92_Mod1110NCR_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[16]: mod11+10 NCR 92x"9" -> OK, width 1135 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 6;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 92));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_93_Mod1110NCR_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  { test_large[17]: mod11+10 NCR 93x"9" -> TOO_LONG }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 6;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 93));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestMSIPlessey.Large_Max92_Mod1110_Max;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_encode[10]: 18 * "9" mod11+10 -> width 247, max value previously }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 4;
    ret := TZintTestHelper.EncodeData(sym, '999999999999999999');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(247, sym.width, 'width');
    Assert.AreEqual(
      '1101101001001101101001001101101001001101101001001101101001001101101001001101101001001101101001001101101001001101101001001101101001001101101001001101101001001101101001001101101001001101101001001101101001001101101001001101101001001001001001001101001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestPlesseyHRTContentSegsFromC.HRT_ContentSegs_FromC;
type
  TItem = record
    Index: Integer;
    Symbology: Integer;
    Option2: Integer;
    Data: String;
    ExpectedText: String;
    ExpectedContent: String;
  end;
const
  Items: array[0..13] of TItem = (
    (Index: 1;  Symbology: BARCODE_MSI_PLESSEY; Option2: -1; Data: '1234567';          ExpectedText: '1234567';          ExpectedContent: '1234567'),
    (Index: 5;  Symbology: BARCODE_MSI_PLESSEY; Option2: 1;  Data: '1234567';          ExpectedText: '12345674';         ExpectedContent: '12345674'),
    (Index: 7;  Symbology: BARCODE_MSI_PLESSEY; Option2: 11; Data: '1234567';          ExpectedText: '1234567';          ExpectedContent: '12345674'),
    (Index: 11; Symbology: BARCODE_MSI_PLESSEY; Option2: 2;  Data: '1234567';          ExpectedText: '123456741';        ExpectedContent: '123456741'),
    (Index: 19; Symbology: BARCODE_MSI_PLESSEY; Option2: 13; Data: '1234567';          ExpectedText: '1234567';          ExpectedContent: '12345674'),
    (Index: 27; Symbology: BARCODE_MSI_PLESSEY; Option2: 4;  Data: '9999999999';       ExpectedText: '999999999959';     ExpectedContent: '999999999959'),
    (Index: 37; Symbology: BARCODE_MSI_PLESSEY; Option2: 16; Data: '1234567';          ExpectedText: '1234567';          ExpectedContent: '123456790'),
    (Index: 49; Symbology: BARCODE_MSI_PLESSEY; Option2: 3;  Data: '2211';             ExpectedText: '221110';           ExpectedContent: '221110'),
    (Index: 51; Symbology: BARCODE_MSI_PLESSEY; Option2: 13; Data: '2211';             ExpectedText: '2211';             ExpectedContent: '221110'),
    (Index: 55; Symbology: BARCODE_MSI_PLESSEY; Option2: 14; Data: '2211';             ExpectedText: '2211';             ExpectedContent: '2211100'),
    (Index: 57; Symbology: BARCODE_PLESSEY;     Option2: -1; Data: '0123456789ABCDEF'; ExpectedText: '0123456789ABCDEF'; ExpectedContent: '0123456789ABCDEF90'),
    (Index: 63; Symbology: BARCODE_PLESSEY;     Option2: 1;  Data: '1';                ExpectedText: '173';              ExpectedContent: '173'),
    (Index: 69; Symbology: BARCODE_PLESSEY;     Option2: -1; Data: '75';               ExpectedText: '75';               ExpectedContent: '7580'),
    (Index: 75; Symbology: BARCODE_PLESSEY;     Option2: 1;  Data: '993';              ExpectedText: '993AA';            ExpectedContent: '993AA')
  );
var
  i, j, ret, expected_content_len: Integer;
  sym: TZintSymbol;
begin
  for i := 0 to High(Items) do
  begin
    sym := TZintTestHelper.CreateSymbol(Items[i].Symbology);
    try
      sym.output_options := BARCODE_CONTENT_SEGS;
      if Items[i].Option2 >= 0 then
        sym.option_2 := Items[i].Option2;

      ret := TZintTestHelper.EncodeData(sym, Items[i].Data);
      Assert.AreEqual(ZINT_OK, ret, Format('C#%d ret', [Items[i].Index]));
      Assert.AreEqual(Items[i].ExpectedText, TZintTestHelper.GetText(sym), Format('C#%d text', [Items[i].Index]));

      expected_content_len := Length(Items[i].ExpectedContent);
      Assert.AreEqual(1, Integer(sym.content_segs_count), Format('C#%d content_segs_count', [Items[i].Index]));
      Assert.AreEqual(expected_content_len, Integer(sym.content_segs[0].Length), Format('C#%d content length', [Items[i].Index]));
      for j := 1 to expected_content_len do
        Assert.AreEqual<Integer>(Ord(Items[i].ExpectedContent[j]), Integer(sym.content_segs[0].Source[j - 1]),
          Format('C#%d content[%d]', [Items[i].Index, j - 1]));
    finally
      sym.Free;
    end;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestPlessey);
  TDUnitX.RegisterTestFixture(TTestMSIPlessey);
  TDUnitX.RegisterTestFixture(TTestPlesseyHRTContentSegsFromC);

end.
