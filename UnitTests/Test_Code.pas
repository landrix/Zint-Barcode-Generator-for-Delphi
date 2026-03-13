unit Test_Code;

{
  DUnitX-Tests fuer zint_code.pas
  Testdaten aus test_code.c und test_code11.c (Zint commit b3a3c0d, 2026-03-13)
}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TestHelper_Zint,
  zint;

type
  { ====================== Code 11 ====================== }
  [TestFixture]
  TTestCode11 = class
  public
    { test_large }
    [Test] procedure Large_Max140_OK;
    [Test] procedure Large_141_TooLong;

    { test_input }
    [Test] procedure Input_Dash_OK;
    [Test] procedure Input_FullSet_OK;
    [Test] procedure Input_Alpha_Rejected;
    [Test] procedure Input_Plus_Rejected;
    [Test] procedure Input_Dot_Rejected;
    [Test] procedure Input_Bang_Rejected;
    [Test] procedure Input_Space_Rejected;
    [Test] procedure Input_InvalidOption2;

    { test_hrt }
    [Test] procedure HRT_Default_2Check;
    [Test] procedure HRT_Option2_1_1Check;
    [Test] procedure HRT_Option2_2_NoCheck;
    [Test] procedure HRT_Long_Dash_2Check;

    { test_encode }
    [Test] procedure Encode_123_45_Default;
    [Test] procedure Encode_93_Default;
    [Test] procedure Encode_123_45_1Check;
    [Test] procedure Encode_123_45_NoCheck;
  end;

  { ====================== Code 39 ====================== }
  [TestFixture]
  TTestCode39 = class
  public
    { test_large }
    [Test] procedure Large_Max86_OK;
    [Test] procedure Large_87_TooLong;

    { test_input }
    [Test] procedure Input_LowerConverted_OK;
    [Test] procedure Input_FullSet_OK;
    [Test] procedure Input_Bang_Rejected;
    [Test] procedure Input_Option2_0_NoCheck;
    [Test] procedure Input_Option2_1_Check;
    [Test] procedure Input_Option2_2_HiddenCheck;

    { test_hrt }
    [Test] procedure HRT_Default_Stars;
    [Test] procedure HRT_LowerToUpper;
    [Test] procedure HRT_WithCheck;
    [Test] procedure HRT_HiddenCheck;
    [Test] procedure HRT_CheckDigitDash;

    { test_encode }
    [Test] procedure Encode_1A;
    [Test] procedure Encode_1A_WithCheck;
    [Test] procedure Encode_Z4_CheckDollar;
    [Test] procedure Encode_FullSet;
  end;

  { ====================== Extended Code 39 ====================== }
  [TestFixture]
  TTestExCode39 = class
  public
    { test_large }
    [Test] procedure Large_Max86_OK;
    [Test] procedure Large_87_TooLong;
    [Test] procedure Large_LowerMax43_OK;
    [Test] procedure Large_Lower44_TooLong;

    { test_input }
    [Test] procedure Input_Alpha_OK;
    [Test] procedure Input_Lower_OK;
    [Test] procedure Input_Comma_OK;
    [Test] procedure Input_HighASCII_Rejected;

    { test_hrt }
    [Test] procedure HRT_Default;
    [Test] procedure HRT_WithCheck;
    [Test] procedure HRT_Lower_WithCheck;
    [Test] procedure HRT_HiddenCheck;

    { test_encode }
    [Test] procedure Encode_1A;
  end;

  { ====================== LOGMARS ====================== }
  [TestFixture]
  TTestLogmars = class
  public
    { test_large }
    [Test] procedure Large_Max30_OK;
    [Test] procedure Large_31_TooLong;

    { test_hrt }
    [Test] procedure HRT_LowerToUpper;
    [Test] procedure HRT_WithCheck;
    [Test] procedure HRT_HiddenCheck;

    { test_encode }
    [Test] procedure Encode_1A;
    [Test] procedure Encode_1A_WithCheck;
    [Test] procedure Encode_ABC;
    [Test] procedure Encode_SAMPLE1;
    [Test] procedure Encode_12345_ABCDE_WithCheck;
  end;

  { ====================== Code 93 ====================== }
  [TestFixture]
  TTestCode93 = class
  public
    { test_large }
    [Test] procedure Large_Max123_OK;
    [Test] procedure Large_124_TooLong;
    [Test] procedure Large_LowerMax61_OK;
    [Test] procedure Large_Lower62_TooLong;

    { test_hrt }
    [Test] procedure HRT_Default_NoCheckShown;
    [Test] procedure HRT_Option2_1_ShowCheck;
    [Test] procedure HRT_Lower_NoCheckShown;
    [Test] procedure HRT_Lower_ShowCheck;

    { test_encode }
    [Test] procedure Encode_C93;
    [Test] procedure Encode_1A;
    [Test] procedure Encode_TEST93;
  end;

  { ====================== VIN ====================== }
  [TestFixture]
  TTestVIN = class
  public
    { test_large }
    [Test] procedure Large_17_OK;
    [Test] procedure Large_18_Wrong;
    [Test] procedure Large_16_Wrong;
    [Test] procedure Large_Import_17_OK;

    { test_input }
    [Test] procedure Input_Valid_OK;
    [Test] procedure Input_BadCheckDigit;

    { test_hrt }
    [Test] procedure HRT_Default;
    [Test] procedure HRT_Import;

    { test_encode }
    [Test] procedure Encode_1FTCR;
    [Test] procedure Encode_Import_2FTPX;
  end;

  { ====================== HIBC 39 ====================== }
  [TestFixture]
  TTestHIBC39 = class
  public
    { test_large }
    [Test] procedure Large_Max68_OK;
    [Test] procedure Large_69_TooLong;

    { test_hrt }
    [Test] procedure HRT_Default;
    [Test] procedure HRT_LowerToUpper;
    [Test] procedure HRT_Numeric;

    { test_encode }
    [Test] procedure Encode_A123BJC5D6E71;
    [Test] procedure Encode_DollarDollar52001510X3G;
  end;

implementation

{ ==================== TTestCode11 ==================== }

procedure TTestCode11.Large_Max140_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('13', 140));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1151, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestCode11.Large_141_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('13', 141));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 320: Input length 141 too long (maximum 140)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestCode11.Input_Dash_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    ret := TZintTestHelper.EncodeData(sym, '-');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(37, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestCode11.Input_FullSet_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    ret := TZintTestHelper.EncodeData(sym, '0123456789-');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(115, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestCode11.Input_Alpha_Rejected;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 321: Invalid character at position 1 in input (digits and "-" only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestCode11.Input_Plus_Rejected;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    ret := TZintTestHelper.EncodeData(sym, '12+');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 321: Invalid character at position 3 in input (digits and "-" only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestCode11.Input_Dot_Rejected;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    ret := TZintTestHelper.EncodeData(sym, '1.2');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 321: Invalid character at position 2 in input (digits and "-" only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestCode11.Input_Bang_Rejected;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    ret := TZintTestHelper.EncodeData(sym, '12!');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 321: Invalid character at position 3 in input (digits and "-" only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestCode11.Input_Space_Rejected;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    ret := TZintTestHelper.EncodeData(sym, ' ');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 321: Invalid character at position 1 in input (digits and "-" only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestCode11.Input_InvalidOption2;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    sym.option_2 := 3;
    ret := TZintTestHelper.EncodeData(sym, '1');
    Assert.AreEqual(ZINT_ERROR_INVALID_OPTION, ret, 'ret');
    Assert.AreEqual('Error 339: Invalid check digit version ''3'' (1 or 2 only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestCode11.HRT_Default_2Check;
var sym: TZintSymbol;
begin
  { test_hrt[0]: "123-45" with 2 check digits -> "123-4552" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    TZintTestHelper.EncodeData(sym, '123-45');
    Assert.AreEqual('123-4552', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode11.HRT_Option2_1_1Check;
var sym: TZintSymbol;
begin
  { test_hrt[2]: "123-45" with 1 check digit -> "123-455" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '123-45');
    Assert.AreEqual('123-455', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode11.HRT_Option2_2_NoCheck;
var sym: TZintSymbol;
begin
  { test_hrt[4]: "123-45" with 0 check digits -> "123-45" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '123-45');
    Assert.AreEqual('123-45', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode11.HRT_Long_Dash_2Check;
var sym: TZintSymbol;
begin
  { test_hrt[6]: "123456789012" -> "123456789012-8" (check digits '-' and '8') }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    TZintTestHelper.EncodeData(sym, '123456789012');
    Assert.AreEqual('123456789012-8', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode11.Encode_123_45_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[0]: "123-45" with 2 check digits (52) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    ret := TZintTestHelper.EncodeData(sym, '123-45');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(78, sym.width, 'width');
    Assert.AreEqual(
      '101100101101011010010110110010101011010101101101101101011011010100101101011001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestCode11.Encode_93_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[1]: "93" with 2 check digits (--) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    ret := TZintTestHelper.EncodeData(sym, '93');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(44, sym.width, 'width');
    Assert.AreEqual(
      '10110010110101011001010101101010110101011001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestCode11.Encode_123_45_1Check;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[4]: "123-45" with 1 check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '123-45');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(70, sym.width, 'width');
    Assert.AreEqual(
      '1011001011010110100101101100101010110101011011011011010110110101011001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestCode11.Encode_123_45_NoCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[5]: "123-45" with 0 check digits }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE11);
  try
    sym.option_2 := 2;
    ret := TZintTestHelper.EncodeData(sym, '123-45');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(62, sym.width, 'width');
    Assert.AreEqual(
      '10110010110101101001011011001010101101010110110110110101011001',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

{ ==================== TTestCode39 ==================== }

procedure TTestCode39.Large_Max86_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 86));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1143, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestCode39.Large_87_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 87));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 323: Input length 87 too long (maximum 86)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestCode39.Input_LowerConverted_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[0]: lowercase auto-converted to upper }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, 'a');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(38, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestCode39.Input_FullSet_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[1]: full Code 39 character set }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(584, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestCode39.Input_Bang_Rejected;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[2]: '!' is invalid }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, 'AB!');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 324: Invalid character at position 3 in input (alphanumerics, space and "-.$/+%" only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestCode39.Input_Option2_0_NoCheck;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    sym.option_2 := 0;
    ret := TZintTestHelper.EncodeData(sym, '1');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(38, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestCode39.Input_Option2_1_Check;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '1');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(51, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestCode39.Input_Option2_2_HiddenCheck;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    sym.option_2 := 2;
    ret := TZintTestHelper.EncodeData(sym, '1');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(51, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestCode39.HRT_Default_Stars;
var sym: TZintSymbol;
begin
  { test_hrt[0]: "ABC1234" -> "*ABC1234*" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    TZintTestHelper.EncodeData(sym, 'ABC1234');
    Assert.AreEqual('*ABC1234*', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode39.HRT_LowerToUpper;
var sym: TZintSymbol;
begin
  { test_hrt[4]: "abc1234" -> "*ABC1234*" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    TZintTestHelper.EncodeData(sym, 'abc1234');
    Assert.AreEqual('*ABC1234*', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode39.HRT_WithCheck;
var sym: TZintSymbol;
begin
  { test_hrt[2]: "ABC1234" with check -> "*ABC12340*" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, 'ABC1234');
    Assert.AreEqual('*ABC12340*', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode39.HRT_HiddenCheck;
var sym: TZintSymbol;
begin
  { test_hrt[14-15]: "123456789" option_2=2 -> HRT="*123456789*" (check hidden) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('*123456789*', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode39.HRT_CheckDigitDash;
var sym: TZintSymbol;
begin
  { test_encode[2]: "Z1" with check digit -> check digit is '-' }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, 'Z1');
    Assert.AreEqual('*Z1-*', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode39.Encode_1A;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[0]: ISO/IEC 16388:2007 Figure 1 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, '1A');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(51, sym.width, 'width');
    Assert.AreEqual(
      '100101101101011010010101101101010010110100101101101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestCode39.Encode_1A_WithCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[1]: With check digit (B) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '1A');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(64, sym.width, 'width');
    Assert.AreEqual(
      '1001011011010110100101011011010100101101011010010110100101101101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestCode39.Encode_Z4_CheckDollar;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[5]: "Z4" check digit '$' }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, 'Z4');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(64, sym.width, 'width');
    Assert.AreEqual(
      '1001011011010100110110101010100110101101001001001010100101101101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestCode39.Encode_FullSet;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[10]: Full CODE39 set }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(584, sym.width, 'width');
    Assert.AreEqual(
      '10010110110101010011011010110100101011010110010101101101100101010101001101011011010011010101011001101010101001011011011010010110101011001011010110101001011010110100101101101101001010101011001011011010110010101011011001010101010011011011010100110101011010011010101011001101011010101001101011010100110110110101001010101101001101101011010010101101101001010101011001101101010110010101101011001010101101100101100101010110100110101011011001101010101001011010110110010110101010011011010101001010110110110010101101010011010110101001001001010100100101001010010100100101010010010010100101101101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

{ ==================== TTestExCode39 ==================== }

procedure TTestExCode39.Large_Max86_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 86));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1143, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestExCode39.Large_87_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 87));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 328: Input length 87 too long (maximum 86)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestExCode39.Large_LowerMax43_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { Lowercase 'a' expands to 2 symbol chars, so max = 43 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('a', 43));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1143, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestExCode39.Large_Lower44_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('a', 44));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 317: Input too long, requires 88 symbol characters (maximum 86)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestExCode39.Input_Alpha_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(38, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestExCode39.Input_Lower_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, 'a');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(51, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestExCode39.Input_Comma_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, ',');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(51, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestExCode39.Input_HighASCII_Rejected;
var sym: TZintSymbol; ret: Integer;
  b: TArrayOfByte;
begin
  { test_input[27]: byte 192 (0xC0) -> extended ASCII not allowed }
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    SetLength(b, 2);
    b[0] := 192;
    b[1] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 1);
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 329: Invalid character at position 1 in input, extended ASCII not allowed',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestExCode39.HRT_Default;
var sym: TZintSymbol;
begin
  { test_hrt[16]: "ABC1234" -> "ABC1234" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    TZintTestHelper.EncodeData(sym, 'ABC1234');
    Assert.AreEqual('ABC1234', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestExCode39.HRT_WithCheck;
var sym: TZintSymbol;
begin
  { test_hrt[18]: "ABC1234" with check -> "ABC12340" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, 'ABC1234');
    Assert.AreEqual('ABC12340', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestExCode39.HRT_Lower_WithCheck;
var sym: TZintSymbol;
begin
  { test_hrt[22]: "abc1234" with check -> "abc1234." }
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, 'abc1234');
    Assert.AreEqual('abc1234.', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestExCode39.HRT_HiddenCheck;
var sym: TZintSymbol;
begin
  { test_hrt[24]: "abc1234" hidden check -> "abc1234" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, 'abc1234');
    Assert.AreEqual('abc1234', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestExCode39.Encode_1A;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[11]: ISO/IEC 16388:2007 Figure 1 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_EXCODE39);
  try
    ret := TZintTestHelper.EncodeData(sym, '1A');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(51, sym.width, 'width');
    Assert.AreEqual(
      '100101101101011010010101101101010010110100101101101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

{ ==================== TTestLogmars ==================== }

procedure TTestLogmars.Large_Max30_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_LOGMARS);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 30));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(511, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestLogmars.Large_31_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_LOGMARS);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 31));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 322: Input length 31 too long (maximum 30)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestLogmars.HRT_LowerToUpper;
var sym: TZintSymbol;
begin
  { test_hrt[34]: "abc1234" -> "ABC1234" (no stars for LOGMARS) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_LOGMARS);
  try
    TZintTestHelper.EncodeData(sym, 'abc1234');
    Assert.AreEqual('ABC1234', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestLogmars.HRT_WithCheck;
var sym: TZintSymbol;
begin
  { test_hrt[36]: "abc1234" with check -> "ABC12340" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_LOGMARS);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, 'abc1234');
    Assert.AreEqual('ABC12340', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestLogmars.HRT_HiddenCheck;
var sym: TZintSymbol;
begin
  { test_hrt[40]: "12345/ABCDE" hidden check -> "12345/ABCDE" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_LOGMARS);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '12345/ABCDE');
    Assert.AreEqual('12345/ABCDE', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestLogmars.Encode_1A;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[18]: Verified against TEC-IT }
  sym := TZintTestHelper.CreateSymbol(BARCODE_LOGMARS);
  try
    ret := TZintTestHelper.EncodeData(sym, '1A');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(63, sym.width, 'width');
    Assert.AreEqual(
      '100010111011101011101000101011101110101000101110100010111011101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestLogmars.Encode_1A_WithCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[19]: With check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_LOGMARS);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '1A');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(79, sym.width, 'width');
    Assert.AreEqual(
      '1000101110111010111010001010111011101010001011101011101000101110100010111011101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestLogmars.Encode_ABC;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[20]: MIL-STD-1189 Rev. B Figure 1 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_LOGMARS);
  try
    ret := TZintTestHelper.EncodeData(sym, 'ABC');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(79, sym.width, 'width');
    Assert.AreEqual(
      '1000101110111010111010100010111010111010001011101110111010001010100010111011101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestLogmars.Encode_SAMPLE1;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[21]: MIL-STD-1189 Rev. B Figure 2 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_LOGMARS);
  try
    ret := TZintTestHelper.EncodeData(sym, 'SAMPLE 1');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(159, sym.width, 'width');
    Assert.AreEqual(
      '100010111011101010111010111000101110101000101110111011101010001010111011101000101011101010001110111010111000101010001110101110101110100010101110100010111011101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestLogmars.Encode_12345_ABCDE_WithCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[22]: MIL-STD-1189 Rev. B Section 6.2.1 check char example }
  sym := TZintTestHelper.CreateSymbol(BARCODE_LOGMARS);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '12345/ABCDE');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(223, sym.width, 'width');
    Assert.AreEqual(
      '1000101110111010111010001010111010111000101011101110111000101010101000111010111011101000111010101000100010100010111010100010111010111010001011101110111010001010101011100010111011101011100010101010111011100010100010111011101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

{ ==================== TTestCode93 ==================== }

procedure TTestCode93.Large_Max123_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE93);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 123));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1144, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestCode93.Large_124_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE93);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 124));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 330: Input length 124 too long (maximum 123)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestCode93.Large_LowerMax61_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { Lowercase 'a' expands to 2 symbol chars, so max = 61 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE93);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('a', 61));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestCode93.Large_Lower62_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE93);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('a', 62));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 332: Input too long, requires 124 symbol characters (maximum 123)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestCode93.HRT_Default_NoCheckShown;
var sym: TZintSymbol;
begin
  { test_hrt[42]: "ABC1234" -> "ABC1234" (check digits not shown by default) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE93);
  try
    TZintTestHelper.EncodeData(sym, 'ABC1234');
    Assert.AreEqual('ABC1234', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode93.HRT_Option2_1_ShowCheck;
var sym: TZintSymbol;
begin
  { test_hrt[44]: "ABC1234" with option_2=1 -> "ABC1234S5" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE93);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, 'ABC1234');
    Assert.AreEqual('ABC1234S5', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode93.HRT_Lower_NoCheckShown;
var sym: TZintSymbol;
begin
  { test_hrt[46]: "abc1234" -> "abc1234" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE93);
  try
    TZintTestHelper.EncodeData(sym, 'abc1234');
    Assert.AreEqual('abc1234', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode93.HRT_Lower_ShowCheck;
var sym: TZintSymbol;
begin
  { test_hrt[48]: "abc1234" with option_2=1 -> "abc1234ZG" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE93);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, 'abc1234');
    Assert.AreEqual('abc1234ZG', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestCode93.Encode_C93;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[23]: ANSI/AIM BC5-1995 Figure 1 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE93);
  try
    ret := TZintTestHelper.EncodeData(sym, 'C93');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(64, sym.width, 'width');
    Assert.AreEqual(
      '1010111101101000101000010101010000101101010001110110101010111101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestCode93.Encode_1A;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[25]: Verified TEC-IT }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE93);
  try
    ret := TZintTestHelper.EncodeData(sym, '1A');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(55, sym.width, 'width');
    Assert.AreEqual(
      '1010111101010010001101010001101000101001110101010111101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestCode93.Encode_TEST93;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[26]: Verified TEC-IT }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE93);
  try
    ret := TZintTestHelper.EncodeData(sym, 'TEST93');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(91, sym.width, 'width');
    Assert.AreEqual(
      '1010111101101001101100100101101011001101001101000010101010000101011101101001000101010111101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

{ ==================== TTestVIN ==================== }

procedure TTestVIN.Large_17_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_VIN);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 17));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(246, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestVIN.Large_18_Wrong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_VIN);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 18));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 336: Input length 18 wrong (17 characters required)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestVIN.Large_16_Wrong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_VIN);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 16));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 336: Input length 16 wrong (17 characters required)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestVIN.Large_Import_17_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_VIN);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 17));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(259, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestVIN.Input_Valid_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[41]: valid VIN }
  sym := TZintTestHelper.CreateSymbol(BARCODE_VIN);
  try
    ret := TZintTestHelper.EncodeData(sym, '5GZCZ43D13S812715');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(246, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestVIN.Input_BadCheckDigit;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[42]: bad check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_VIN);
  try
    ret := TZintTestHelper.EncodeData(sym, '5GZCZ43D23S812715');
    Assert.AreEqual(ZINT_ERROR_INVALID_CHECK, ret, 'ret');
    Assert.AreEqual('Error 338: Invalid check digit ''2'' (position 9), expecting ''1''',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestVIN.HRT_Default;
var sym: TZintSymbol;
begin
  { test_hrt[54]: "1FTCR10UXTPA78180" -> same as HRT }
  sym := TZintTestHelper.CreateSymbol(BARCODE_VIN);
  try
    TZintTestHelper.EncodeData(sym, '1FTCR10UXTPA78180');
    Assert.AreEqual('1FTCR10UXTPA78180', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestVIN.HRT_Import;
var sym: TZintSymbol;
begin
  { test_hrt[56]: With 'I' prefix for import }
  sym := TZintTestHelper.CreateSymbol(BARCODE_VIN);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '2FTPX28L0XCA15511');
    Assert.AreEqual('2FTPX28L0XCA15511', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestVIN.Encode_1FTCR;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[30]: vinquery.com example }
  sym := TZintTestHelper.CreateSymbol(BARCODE_VIN);
  try
    ret := TZintTestHelper.EncodeData(sym, '1FTCR10UXTPA78180');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(246, sym.width, 'width');
    Assert.AreEqual(
      '100101101101011010010101101011011001010101011011001011011010010101101010110010110100101011010100110110101100101010110100101101011010101101100101011011010010110101001011010100101101101101001011010110100101011011010010110101010011011010100101101101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestVIN.Encode_Import_2FTPX;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[31]: With Import 'I' prefix }
  sym := TZintTestHelper.CreateSymbol(BARCODE_VIN);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '2FTPX28L0XCA15511');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(259, sym.width, 'width');
    Assert.AreEqual(
      '1001011011010101101001101010110010101101011011001010101011011001010110110100101001011010110101100101011011010010110101011010100110101001101101010010110101101101101001010110101001011011010010101101101001101010110100110101011010010101101101001010110100101101101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

{ ==================== TTestHIBC39 ==================== }

procedure TTestHIBC39.Large_Max68_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_HIBC_39);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 68));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1151, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestHIBC39.Large_69_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_HIBC_39);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 69));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 319: Input length 69 too long (maximum 68)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestHIBC39.HRT_Default;
var sym: TZintSymbol;
begin
  { test_hrt[58]: "ABC1234" -> "*+ABC1234+*" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_HIBC_39);
  try
    TZintTestHelper.EncodeData(sym, 'ABC1234');
    Assert.AreEqual('*+ABC1234+*', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestHIBC39.HRT_LowerToUpper;
var sym: TZintSymbol;
begin
  { test_hrt[60]: "abc1234" -> "*+ABC1234+*" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_HIBC_39);
  try
    TZintTestHelper.EncodeData(sym, 'abc1234');
    Assert.AreEqual('*+ABC1234+*', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestHIBC39.HRT_Numeric;
var sym: TZintSymbol;
begin
  { test_hrt[62]: "123456789" -> "*+1234567890*" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_HIBC_39);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('*+1234567890*', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestHIBC39.Encode_A123BJC5D6E71;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[32]: ANSI/HIBC 2.6 Figure 2 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_HIBC_39);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A123BJC5D6E71');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(271, sym.width, 'width');
    Assert.AreEqual(
      '1000101110111010100010100010001011101010001011101110100010101110101110001010111011101110001010101011101000101110101011100011101011101110100010101110100011101010101011100010111010111000111010101110101110001010101000101110111011101000101011101010100011101110100010111011101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

procedure TTestHIBC39.Encode_DollarDollar52001510X3G;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[33]: ANSI/HIBC 2.6 Figure 6 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_HIBC_39);
  try
    ret := TZintTestHelper.EncodeData(sym, '$$52001510X3G');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(271, sym.width, 'width');
    Assert.AreEqual(
      '1000101110111010100010100010001010001000100010101000100010001010111010001110101010111000101011101010001110111010101000111011101011101000101011101110100011101010111010001010111010100011101110101000101110101110111011100010101010101000111011101010111000101110100010111011101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally sym.Free; end;
end;

end.
