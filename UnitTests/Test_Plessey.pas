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
    [Test] procedure Large_Max92_Mod1010_OK;
    [Test] procedure Large_Max92_Mod1110_Max;
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

{ Large tests with various check options }

procedure TTestMSIPlessey.Large_Max92_Mod10_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_large[2]: 92 * "9" mod10 -> OK, width 1123 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 92));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1123, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestMSIPlessey.Large_Max92_Mod1010_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_large[4]: 92 * "9" mod10+10 -> OK, width 1135 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_MSI_PLESSEY);
  try
    sym.option_2 := 2;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('9', 92));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1135, sym.width, 'width');
  finally
    sym.Free;
  end;
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

initialization
  TDUnitX.RegisterTestFixture(TTestPlessey);
  TDUnitX.RegisterTestFixture(TTestMSIPlessey);

end.
