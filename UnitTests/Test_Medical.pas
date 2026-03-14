unit Test_Medical;

{
  DUnitX-Tests fuer zint_medical.pas
  Testdaten aus test_medical.c (Zint commit b3a3c0d, 2026-03-13)
  Testet: Pharmacode One-Track, Pharmacode Two-Track, Code 32 (Italian Pharmacode)
  PZN ist in zint_code.pas und wird separat getestet.
}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TestHelper_Zint,
  zint;

type
  [TestFixture]
  TTestPharmaOne = class
  public
    { test_large }
    [Test] procedure Large_Max_OK;
    [Test] procedure Large_TooLong;

    { test_input }
    [Test] procedure Input_MaxValue_OK;
    [Test] procedure Input_MaxValuePlus1_Rejected;
    [Test] procedure Input_MinValue_OK;
    [Test] procedure Input_BelowMin_Rejected;
    [Test] procedure Input_TooLong_7Digits;
    [Test] procedure Input_InvalidChar_Rejected;
    [Test] procedure Input_Value1_Rejected;

    { test_hrt }
    [Test] procedure HRT_Empty;

    { test_encode }
    [Test] procedure Encode_131070;
    [Test] procedure Encode_123456;
  end;

  [TestFixture]
  TTestPharmaTwo = class
  public
    { test_large }
    [Test] procedure Large_Max_OK;
    [Test] procedure Large_TooLong;

    { test_input }
    [Test] procedure Input_MaxValue_OK;
    [Test] procedure Input_MaxValuePlus1_Rejected;
    [Test] procedure Input_TooLong_9Digits;
    [Test] procedure Input_MinValue_OK;
    [Test] procedure Input_BelowMin_Rejected;
    [Test] procedure Input_Value2_Rejected;
    [Test] procedure Input_Value1_Rejected;
    [Test] procedure Input_InvalidChar_Rejected;

    { test_hrt }
    [Test] procedure HRT_Empty;

    { test_encode }
    [Test] procedure Encode_64570080;
    [Test] procedure Encode_29876543;
  end;

  [TestFixture]
  TTestCode32 = class
  public
    { test_large }
    [Test] procedure Large_Max_OK;
    [Test] procedure Large_TooLong;

    { test_input }
    [Test] procedure Input_Valid_8Digits;
    [Test] procedure Input_SingleDigit9_OK;
    [Test] procedure Input_SingleDigit0_OK;
    [Test] procedure Input_TooLong_9Digits;
    [Test] procedure Input_InvalidChar_Rejected;
    [Test] procedure Input_99999999_OK;

    { test_hrt }
    [Test] procedure HRT_123456;
    [Test] procedure HRT_12345678;
    [Test] procedure HRT_12345678_Option2_1;
    [Test] procedure HRT_12345678_Option2_2;

    { test_encode }
    [Test] procedure Encode_34567890;
    [Test] procedure Encode_34567890_Option2_1;
    [Test] procedure Encode_34567890_Option2_2;
  end;

implementation

{ ---------- TTestPharmaOne ---------- }

procedure TTestPharmaOne.Large_Max_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_large[0]: "131070" (6 digits) -> OK, 1 row, width 78
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA);
  try
    ret := TZintTestHelper.EncodeData(sym, '131070');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(78, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaOne.Large_TooLong;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_large[1]: 7 digits -> ERROR_TOO_LONG
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 7));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 350: Input length 7 too long (maximum 6)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaOne.Input_MaxValue_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[0]: "131070" max value -> OK
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA);
  try
    ret := TZintTestHelper.EncodeData(sym, '131070');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(78, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaOne.Input_MaxValuePlus1_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[2]: "131071" -> out of range
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA);
  try
    ret := TZintTestHelper.EncodeData(sym, '131071');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 352: Input value ''131071'' out of range (3 to 131070)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaOne.Input_MinValue_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[3]: "3" min value -> OK, width 4
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA);
  try
    ret := TZintTestHelper.EncodeData(sym, '3');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(4, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaOne.Input_BelowMin_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[4]: "2" -> out of range
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA);
  try
    ret := TZintTestHelper.EncodeData(sym, '2');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 352: Input value ''2'' out of range (3 to 131070)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaOne.Input_InvalidChar_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[6]: "12A" -> invalid char at position 3
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA);
  try
    ret := TZintTestHelper.EncodeData(sym, '12A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 351: Invalid character at position 3 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaOne.Input_TooLong_7Digits;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[1]: "1310700" (7 digits) -> ERROR_TOO_LONG
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA);
  try
    ret := TZintTestHelper.EncodeData(sym, '1310700');
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 350: Input length 7 too long (maximum 6)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaOne.Input_Value1_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[5]: "1" -> out of range
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA);
  try
    ret := TZintTestHelper.EncodeData(sym, '1');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 352: Input value ''1'' out of range (3 to 131070)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaOne.HRT_Empty;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_hrt[0]: Pharmacode One has no HRT
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('', TZintTestHelper.GetText(sym), 'hrt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaOne.Encode_131070;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[0]: "131070"
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA);
  try
    ret := TZintTestHelper.EncodeData(sym, '131070');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(78, sym.width, 'width');
    Assert.AreEqual(
      '111001110011100111001110011100111001110011100111001110011100111001110011100111',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaOne.Encode_123456;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[1]: "123456"
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(58, sym.width, 'width');
    Assert.AreEqual(
      '1110011100111001001001001110010010011100100100100100100111',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

{ ---------- TTestPharmaTwo ---------- }

procedure TTestPharmaTwo.Large_Max_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_large[2]: "64570080" (8 digits) -> OK, 2 rows, width 31
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, '64570080');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(2, sym.rows, 'rows');
    Assert.AreEqual(31, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaTwo.Large_TooLong;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_large[3]: 9 digits -> ERROR_TOO_LONG
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 9));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 354: Input length 9 too long (maximum 8)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaTwo.Input_MaxValue_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[7]: "64570080" -> OK
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, '64570080');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(2, sym.rows, 'rows');
    Assert.AreEqual(31, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaTwo.Input_MaxValuePlus1_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[8]: "64570081" -> out of range
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, '64570081');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 353: Input value ''64570081'' out of range (4 to 64570080)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaTwo.Input_TooLong_9Digits;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[9]: "064570080" (9 digits) -> ERROR_TOO_LONG
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, '064570080');
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 354: Input length 9 too long (maximum 8)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaTwo.Input_MinValue_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[10]: "4" min value -> OK, 2 rows, width 3
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, '4');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(2, sym.rows, 'rows');
    Assert.AreEqual(3, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaTwo.Input_BelowMin_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[11]: "3" -> out of range
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, '3');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 353: Input value ''3'' out of range (4 to 64570080)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaTwo.Input_Value2_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[12]: "2" -> out of range
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, '2');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 353: Input value ''2'' out of range (4 to 64570080)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaTwo.Input_Value1_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[13]: "1" -> out of range
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, '1');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 353: Input value ''1'' out of range (4 to 64570080)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaTwo.Input_InvalidChar_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[14]: "123A" -> invalid char at position 4
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, '123A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 355: Invalid character at position 4 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaTwo.HRT_Empty;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_hrt[2]: Pharmacode Two has no HRT
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('', TZintTestHelper.GetText(sym), 'hrt');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaTwo.Encode_64570080;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[2]: "64570080" - 2 rows
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, '64570080');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(2, sym.rows, 'rows');
    Assert.AreEqual(31, sym.width, 'width');
    Assert.AreEqual(
      '1010101010101010101010101010101' + #10 +
      '1010101010101010101010101010101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestPharmaTwo.Encode_29876543;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[3]: "29876543"
  sym := TZintTestHelper.CreateSymbol(BARCODE_PHARMA_TWO);
  try
    ret := TZintTestHelper.EncodeData(sym, '29876543');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(2, sym.rows, 'rows');
    Assert.AreEqual(31, sym.width, 'width');
    Assert.AreEqual(
      '0010100010001010001010001000101' + #10 +
      '1000101010100000100000101010000',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

{ ---------- TTestCode32 ---------- }

procedure TTestCode32.Large_Max_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_large[4]: "1" * 8 -> OK, 1 row, width 103
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 8));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(103, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.Large_TooLong;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_large[5]: 9 digits -> ERROR_TOO_LONG
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 9));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 360: Input length 9 too long (maximum 8)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.Input_Valid_8Digits;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[15]: "12345678" -> OK, width 103
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(103, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.Input_SingleDigit9_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[16]: "9" -> OK, width 103
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    ret := TZintTestHelper.EncodeData(sym, '9');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(103, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.Input_SingleDigit0_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[17]: "0" -> OK, width 103
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    ret := TZintTestHelper.EncodeData(sym, '0');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(103, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.Input_TooLong_9Digits;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[18]: "123456789" -> ERROR_TOO_LONG
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 360: Input length 9 too long (maximum 8)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.Input_InvalidChar_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[19]: "A" -> invalid char at position 1
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 361: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.Input_99999999_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[20]: "99999999" -> OK, width 103
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    ret := TZintTestHelper.EncodeData(sym, '99999999');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(103, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.HRT_123456;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_hrt[4]: "123456" -> HRT "A001234564"
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('A001234564', TZintTestHelper.GetText(sym), 'hrt');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.HRT_12345678;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_hrt[6]: "12345678" -> HRT "A123456788"
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('A123456788', TZintTestHelper.GetText(sym), 'hrt');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.HRT_12345678_Option2_1;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_hrt[8]: option_2=1 "12345678" -> HRT "A123456788" (ignore option_2)
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '12345678');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('A123456788', TZintTestHelper.GetText(sym), 'hrt');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.HRT_12345678_Option2_2;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_hrt[10]: option_2=2 "12345678" -> HRT "A123456788" (ignore option_2)
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    sym.option_2 := 2;
    ret := TZintTestHelper.EncodeData(sym, '12345678');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('A123456788', TZintTestHelper.GetText(sym), 'hrt');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.Encode_34567890;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[4]: "34567890"
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    ret := TZintTestHelper.EncodeData(sym, '34567890');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(103, sym.width, 'width');
    Assert.AreEqual(
      '1001011011010101101001011010110010110101011011010010101100101101011010010101101010101100110100101101101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.Encode_34567890_Option2_1;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[5]: "34567890" with option_2=1 -> same pattern (option_2 shouldn't add extra check)
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '34567890');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(103, sym.width, 'width');
    Assert.AreEqual(1, sym.option_2, 'option_2 restored');
    Assert.AreEqual(
      '1001011011010101101001011010110010110101011011010010101100101101011010010101101010101100110100101101101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestCode32.Encode_34567890_Option2_2;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[6]: "34567890" with option_2=2 -> same pattern
  sym := TZintTestHelper.CreateSymbol(BARCODE_CODE32);
  try
    sym.option_2 := 2;
    ret := TZintTestHelper.EncodeData(sym, '34567890');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(103, sym.width, 'width');
    Assert.AreEqual(2, sym.option_2, 'option_2 restored');
    Assert.AreEqual(
      '1001011011010101101001011010110010110101011011010010101100101101011010010101101010101100110100101101101',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestPharmaOne);
  TDUnitX.RegisterTestFixture(TTestPharmaTwo);
  TDUnitX.RegisterTestFixture(TTestCode32);

end.
