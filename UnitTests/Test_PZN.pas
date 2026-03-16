unit Test_PZN;

{
  DUnitX-Tests fuer PZN (Pharmazentralnummer) in zint_medical.pas
  Testdaten aus test_medical.c (Zint commit b3a3c0d, 2026-03-13)
  Testet: PZN8 (default) und PZN7 (option_2=1)
}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TestHelper_Zint,
  zint;

type
  [TestFixture]
  TTestPZN = class
  public
    { test_input / test_large }
    [Test] procedure Input_PZN8_1Digit_OK;
    [Test] procedure Input_PZN8_TooLong_9;
    [Test] procedure Input_PZN7_1Digit_OK;
    [Test] procedure Input_PZN7_TooLong_8;
    [Test] procedure Input_InvalidChar_Rejected;
    [Test] procedure Input_CheckDigit10_Rejected;
    [Test] procedure Input_BadCheckDigit_Rejected;
    [Test] procedure Large_PZN8_7Digits_OK;
    [Test] procedure Large_PZN8_TooLong_9;
    [Test] procedure Large_PZN7_6Digits_OK;
    [Test] procedure Large_PZN7_TooLong_8;
    [Test] procedure Input_InvalidChar_A;
    [Test] procedure Input_CheckDigit10_1000006;
    [Test] procedure Input_BadCheck_00000011;
    [Test] procedure Input_PZN7_CheckDigit10;
    [Test] procedure Input_PZN7_BadCheck;

    { test_hrt - PZN8 }
    [Test] procedure HRT_PZN8_12345;
    [Test] procedure HRT_PZN8_123456;
    [Test] procedure HRT_PZN8_1234567;
    [Test] procedure HRT_PZN8_12345678;

    { test_hrt - PZN7 }
    [Test] procedure HRT_PZN7_1234;
    [Test] procedure HRT_PZN7_12345;
    [Test] procedure HRT_PZN7_123456;
    [Test] procedure HRT_PZN7_1234562;

    { test_hrt - option_2=2 ignored for check }
    [Test] procedure HRT_PZN8_Option2_12345;

    { test_encode }
    [Test] procedure Encode_PZN8_1234567;
    [Test] procedure Encode_PZN8_2758089;
    [Test] procedure Encode_PZN7_123456;
    [Test] procedure Encode_PZN8_Option2_1234567;

    { test_hrt BARCODE_CONTENT_SEGS from C }
    [Test] procedure HRT_ContentSegs_FromC;
  end;

implementation

{ --- Input / Large Tests --- }

procedure TTestPZN.Input_PZN8_1Digit_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    { 1 digit => ok, padded to 7 + check = 8 }
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(142, sym.width);
  finally sym.Free; end;
end;

procedure TTestPZN.Input_PZN8_TooLong_9;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, '123456789'));
    Assert.AreEqual('Error 325: Input length 9 too long (maximum 8)',
      TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Input_PZN7_1Digit_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 1; { PZN7 }
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(129, sym.width);
  finally sym.Free; end;
end;

procedure TTestPZN.Input_PZN7_TooLong_8;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 1; { PZN7 }
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, '12345678'));
    Assert.AreEqual('Error 325: Input length 8 too long (maximum 7)',
      TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Input_InvalidChar_Rejected;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, '12345A'));
    Assert.AreEqual('Error 326: Invalid character at position 6 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Input_CheckDigit10_Rejected;
var sym: TZintSymbol;
begin
  { "0000003" => weights 1*0+2*0+3*0+4*0+5*0+6*0+7*3 = 21, 21 mod 11 = 10 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, '0000003'));
    Assert.AreEqual('Error 327: Invalid PZN, check digit is ''10''',
      TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Input_BadCheckDigit_Rejected;
var sym: TZintSymbol;
begin
  { "12345678" - correct check is 8, provide "12345670" with bad check 0 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(ZERROR_INVALID_CHECK, TZintTestHelper.EncodeData(sym, '12345670'));
    Assert.AreEqual('Error 890: Invalid check digit ''0'', expecting ''8''',
      TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

{ --- HRT Tests - PZN8 --- }

procedure TTestPZN.HRT_PZN8_12345;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345'));
    Assert.AreEqual('PZN - 00123458', TZintTestHelper.GetText(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.HRT_PZN8_123456;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '123456'));
    Assert.AreEqual('PZN - 01234562', TZintTestHelper.GetText(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.HRT_PZN8_1234567;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567'));
    Assert.AreEqual('PZN - 12345678', TZintTestHelper.GetText(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.HRT_PZN8_12345678;
var sym: TZintSymbol;
begin
  { Full 8 digits including correct check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345678'));
    Assert.AreEqual('PZN - 12345678', TZintTestHelper.GetText(sym));
  finally sym.Free; end;
end;

{ --- HRT Tests - PZN7 --- }

procedure TTestPZN.HRT_PZN7_1234;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 1;
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234'));
    Assert.AreEqual('PZN - 0012345', TZintTestHelper.GetText(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.HRT_PZN7_12345;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 1;
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345'));
    Assert.AreEqual('PZN - 0123458', TZintTestHelper.GetText(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.HRT_PZN7_123456;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 1;
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '123456'));
    Assert.AreEqual('PZN - 1234562', TZintTestHelper.GetText(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.HRT_PZN7_1234562;
var sym: TZintSymbol;
begin
  { Full 7 digits including correct check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 1;
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234562'));
    Assert.AreEqual('PZN - 1234562', TZintTestHelper.GetText(sym));
  finally sym.Free; end;
end;

{ --- HRT Test - option_2=2 (ignored for check digits, treated like default PZN8) --- }

procedure TTestPZN.HRT_PZN8_Option2_12345;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 2;
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345'));
    Assert.AreEqual('PZN - 00123458', TZintTestHelper.GetText(sym));
  finally sym.Free; end;
end;

{ --- Encode Tests --- }

procedure TTestPZN.Encode_PZN8_1234567;
var sym: TZintSymbol;
begin
  { Example from IFA Info Code 39 EN V2.1 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(142, sym.width);
    Assert.AreEqual(
      '1001011011010100101011011011010010101101011001010110110110010101010100110101101101001101010101100110101010100101101101101001011010100101101101',
      TZintTestHelper.ModulesDump(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Encode_PZN8_2758089;
var sym: TZintSymbol;
begin
  { Example from IFA Info Check Digit Calculations EN 15 July 2019 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '2758089'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(142, sym.width);
    Assert.AreEqual(
      '1001011011010100101011011010110010101101010010110110110100110101011010010110101010011011010110100101101010110010110101011001011010100101101101',
      TZintTestHelper.ModulesDump(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Encode_PZN7_123456;
var sym: TZintSymbol;
begin
  { Example from BWIPP }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 1;
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '123456'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(129, sym.width);
    Assert.AreEqual(
      '100101101101010010101101101101001010110101100101011011011001010101010011010110110100110101010110011010101011001010110100101101101',
      TZintTestHelper.ModulesDump(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Encode_PZN8_Option2_1234567;
var sym: TZintSymbol;
begin
  { option_2=2 should be ignored for check digit calc, same as default PZN8 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 2;
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(142, sym.width);
    Assert.AreEqual(
      '1001011011010100101011011011010010101101011001010110110110010101010100110101101101001101010101100110101010100101101101101001011010100101101101',
      TZintTestHelper.ModulesDump(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Large_PZN8_7Digits_OK;
var sym: TZintSymbol;
begin
  { test_large[6]: "1" * 7 -> OK, 1 row, width 142 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 7)));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(142, sym.width);
  finally sym.Free; end;
end;

procedure TTestPZN.Large_PZN8_TooLong_9;
var sym: TZintSymbol;
begin
  { test_large[7]: "1" * 9 -> ERROR_TOO_LONG }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 9)));
    Assert.AreEqual('Error 325: Input length 9 too long (maximum 8)',
      TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Large_PZN7_6Digits_OK;
var sym: TZintSymbol;
begin
  { test_large[8]: opt2=1, "1" * 6 -> OK, 1 row, width 129 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 1;
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 6)));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(129, sym.width);
  finally sym.Free; end;
end;

procedure TTestPZN.Large_PZN7_TooLong_8;
var sym: TZintSymbol;
begin
  { test_large[9]: opt2=1, "1" * 8 -> ERROR_TOO_LONG }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 1;
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 8)));
    Assert.AreEqual('Error 325: Input length 8 too long (maximum 7)',
      TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Input_InvalidChar_A;
var sym: TZintSymbol;
begin
  { test_input[22]: "A" -> invalid char at position 1 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, 'A'));
    Assert.AreEqual('Error 326: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Input_CheckDigit10_1000006;
var sym: TZintSymbol;
begin
  { test_input[23]: "1000006" -> check digit is 10 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, '1000006'));
    Assert.AreEqual('Error 327: Invalid PZN, check digit is ''10''',
      TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Input_BadCheck_00000011;
var sym: TZintSymbol;
begin
  { test_input[24]: "00000011" -> bad check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    Assert.AreEqual(ZERROR_INVALID_CHECK, TZintTestHelper.EncodeData(sym, '00000011'));
    Assert.AreEqual('Error 890: Invalid check digit ''1'', expecting ''7''',
      TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Input_PZN7_CheckDigit10;
var sym: TZintSymbol;
begin
  { test_input[25]: opt2=1, "100009" -> check digit is 10 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 1;
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, '100009'));
    Assert.AreEqual('Error 327: Invalid PZN, check digit is ''10''',
      TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.Input_PZN7_BadCheck;
var sym: TZintSymbol;
begin
  { test_input[26]: opt2=1, "0000011" -> bad check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
  try
    sym.option_2 := 1;
    Assert.AreEqual(ZERROR_INVALID_CHECK, TZintTestHelper.EncodeData(sym, '0000011'));
    Assert.AreEqual('Error 890: Invalid check digit ''1'', expecting ''7''',
      TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPZN.HRT_ContentSegs_FromC;
type
  TItem = record
    Index: Integer;
    Option2: Integer;
    Data: String;
    ExpectedText: String;
    ExpectedContent: String;
  end;
const
  Items: array[0..8] of TItem = (
    (Index: 13; Option2: -1; Data: '12345';   ExpectedText: 'PZN - 00123458'; ExpectedContent: '-00123458'),
    (Index: 15; Option2: -1; Data: '123456';  ExpectedText: 'PZN - 01234562'; ExpectedContent: '-01234562'),
    (Index: 17; Option2: -1; Data: '1234567'; ExpectedText: 'PZN - 12345678'; ExpectedContent: '-12345678'),
    (Index: 19; Option2: -1; Data: '12345678';ExpectedText: 'PZN - 12345678'; ExpectedContent: '-12345678'),
    (Index: 21; Option2: 1;  Data: '1234';    ExpectedText: 'PZN - 0012345';  ExpectedContent: '-0012345'),
    (Index: 23; Option2: 1;  Data: '12345';   ExpectedText: 'PZN - 0123458';  ExpectedContent: '-0123458'),
    (Index: 25; Option2: 1;  Data: '123456';  ExpectedText: 'PZN - 1234562';  ExpectedContent: '-1234562'),
    (Index: 27; Option2: 1;  Data: '1234562'; ExpectedText: 'PZN - 1234562';  ExpectedContent: '-1234562'),
    (Index: 29; Option2: 2;  Data: '12345';   ExpectedText: 'PZN - 00123458'; ExpectedContent: '-00123458')
  );
var
  i, j, ret, expected_content_len: Integer;
  sym: TZintSymbol;
begin
  for i := 0 to High(Items) do
  begin
    sym := TZintTestHelper.CreateSymbol(BARCODE_PZN);
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
  TDUnitX.RegisterTestFixture(TTestPZN);

end.
