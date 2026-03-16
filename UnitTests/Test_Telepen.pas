unit Test_Telepen;

{
  DUnitX-Tests fuer zint_telepen.pas
  Testdaten aus test_telepen.c (Zint commit b3a3c0d, 2026-03-13)
}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TestHelper_Zint,
  zint_helper,
  zint;

type
  [TestFixture]
  TTestTelepen = class
  public
    { test_large: Grenzwerte fuer maximale Eingabelaenge }
    [Test] procedure Large_MaxAlpha_OK;
    [Test] procedure Large_MaxAlpha_Plus1_TooLong;

    { test_input: Zeichenvalidierung }
    [Test] procedure Input_PrintableASCII;
    [Test] procedure Input_HighASCII_And_Controls;
    [Test] procedure Input_NUL_And_DEL;
    [Test] procedure Input_ExtendedASCII_Rejected;

    { test_hrt: Human Readable Text }
    [Test] procedure HRT_UpperAlpha;
    [Test] procedure HRT_ContentSegs_UpperAlpha;
    [Test] procedure HRT_LowerAlpha;
    [Test] procedure HRT_ContentSegs_LowerAlpha;
    [Test] procedure HRT_CtrlChar_Space;
    [Test] procedure HRT_ContentSegs_CtrlChar;
    [Test] procedure HRT_NUL_Space;
    [Test] procedure HRT_ContentSegs_NUL;
    [Test] procedure HRT_ABK0;
    [Test] procedure HRT_ContentSegs_ABK0;

    { test_encode: Korrekte Barcode-Muster }
    [Test] procedure Encode_1A;
    [Test] procedure Encode_ABC;
    [Test] procedure Encode_RST;
    [Test] procedure Encode_QuestionAt;
    [Test] procedure Encode_NUL;

    { test_fuzz }
    [Test] procedure Fuzz_69Nuls_OK;
    [Test] procedure Fuzz_70Nuls_TooLong;
  end;

  [TestFixture]
  TTestTelepenNum = class
  public
    { test_large: Grenzwerte fuer maximale Eingabelaenge }
    [Test] procedure Large_MaxNum_OK;
    [Test] procedure Large_MaxNum_Plus1_TooLong;

    { test_input: Zeichenvalidierung }
    [Test] procedure Input_Digits_OK;
    [Test] procedure Input_InvalidChar_Rejected;
    [Test] procedure Input_DigitX_OK;
    [Test] procedure Input_XDigit_Rejected;
    [Test] procedure Input_MultipleX_OK;

    { test_hrt: Human Readable Text }
    [Test] procedure HRT_Digits;
    [Test] procedure HRT_ContentSegs_Digits;
    [Test] procedure HRT_DigitX;
    [Test] procedure HRT_ContentSegs_DigitX;
    [Test] procedure HRT_LowerX_UpperHRT;
    [Test] procedure HRT_ContentSegs_LowerX;
    [Test] procedure HRT_OddLength_LeadZero;
    [Test] procedure HRT_ContentSegs_OddLength;

    { test_encode: Korrekte Barcode-Muster }
    [Test] procedure Encode_1234567890;
    [Test] procedure Encode_123456789_OddPadded;
    [Test] procedure Encode_123X;
    [Test] procedure Encode_1X3X;
    [Test] procedure Encode_3637;

    { test_fuzz }
    [Test] procedure Fuzz_70Nuls_InvalidData;
    [Test] procedure Fuzz_136x0404_OK;
    [Test] procedure Fuzz_137Digits_TooLong;
    [Test] procedure Fuzz_136ZerosX_OK;
    [Test] procedure Fuzz_Length4_OverlongBuffer_OK;
  end;

implementation

procedure AssertContentSegEquals(const Sym: TZintSymbol; const Expected: array of Byte; const Msg: String);
var
  i: Integer;
begin
  Assert.AreEqual(1, Sym.content_segs_count, Msg + ' count');
  Assert.AreEqual(Length(Expected), Sym.content_segs[0].Length, Msg + ' length');
  for i := 0 to High(Expected) do
    Assert.AreEqual(Integer(Expected[i]), Integer(Sym.content_segs[0].Source[i]), Msg + ' byte ' + IntToStr(i));
end;

{ ---------- TTestTelepen ---------- }

procedure TTestTelepen.Large_MaxAlpha_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_large[0]: "\177" * 69 -> OK, 1 row, width 1152
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat(#127, 69));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1152, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepen.Large_MaxAlpha_Plus1_TooLong;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_large[1]: "\177" * 70 -> ERROR_TOO_LONG
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat(#127, 70));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 390: Input length 70 too long (maximum 69)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepen.Input_PrintableASCII;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[0]: printable ASCII range
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    ret := TZintTestHelper.EncodeData(sym, ' !"#$%&''()*+,-./0123456789:;<');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(512, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepen.Input_HighASCII_And_Controls;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[1]: AZaz~SOH
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    ret := TZintTestHelper.EncodeData(sym, 'AZaz' + #126 + #1);
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(144, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepen.Input_NUL_And_DEL;
var
  sym: TZintSymbol;
  ret: Integer;
  b: TArrayOfByte;
begin
  // test_input[2]: NUL + DEL (bytes 0 and 127), length 2
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    SetLength(b, 3);
    b[0] := 0;    // NUL
    b[1] := 127;  // DEL
    b[2] := 0;    // terminator
    ret := TZintTestHelper.EncodeData(sym, b, 2);
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(80, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepen.Input_ExtendedASCII_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
  b: TArrayOfByte;
begin
  // test_input[3]: byte 233 (e-acute) -> INVALID_DATA
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    SetLength(b, 2);
    b[0] := 233; // e-acute
    b[1] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 1);
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual(
      'Error 391: Invalid character at position 1 in input, extended ASCII not allowed',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepen.Encode_1A;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[0]: "1A" -> BSiH Example
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    ret := TZintTestHelper.EncodeData(sym, '1A');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(80, sym.width, 'width');
    Assert.AreEqual(
      '10101010101110001011101000100010101110111011100010100010001110101110001010101010',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepen.Encode_ABC;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[1]: "ABC" -> E2326U Example
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    ret := TZintTestHelper.EncodeData(sym, 'ABC');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(96, sym.width, 'width');
    Assert.AreEqual(
      '101010101011100010111011101110001110001110111000101011101110101011101000101000101110001010101010',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepen.Encode_RST;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[2]: "RST"
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    ret := TZintTestHelper.EncodeData(sym, 'RST');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(96, sym.width, 'width');
    Assert.AreEqual(
      '101010101011100011100011100010101010111010111000111010111000101010111000111011101110001010101010',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepen.Encode_QuestionAt;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[3]: "?@" - ASCII count 127, check 0
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    ret := TZintTestHelper.EncodeData(sym, '?@');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(80, sym.width, 'width');
    Assert.AreEqual(
      '10101010101110001010101010101110111011101110101011101110111011101110001010101010',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepen.Encode_NUL;
var
  sym: TZintSymbol;
  ret: Integer;
  b: TArrayOfByte;
begin
  // test_encode[4]: "\000" length 1
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    SetLength(b, 2);
    b[0] := 0;
    b[1] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 1);
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(64, sym.width, 'width');
    Assert.AreEqual(
      '1010101010111000111011101110111011101110111011101110001010101010',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepen.HRT_UpperAlpha;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[0]: "ABC1234.;$" -> HRT "ABC1234.;$" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    ret := TZintTestHelper.EncodeData(sym, 'ABC1234.;$');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('ABC1234.;$', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestTelepen.HRT_LowerAlpha;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[2]: "abc1234.;$" -> HRT "abc1234.;$" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    ret := TZintTestHelper.EncodeData(sym, 'abc1234.;$');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('abc1234.;$', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestTelepen.HRT_CtrlChar_Space;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[4]: "ABC1234\001" -> HRT "ABC1234 " (ctrl char replaced by space) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    ret := TZintTestHelper.EncodeData(sym, 'ABC1234' + #1);
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('ABC1234 ', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestTelepen.HRT_NUL_Space;
var sym: TZintSymbol; ret: Integer;
  b: TArrayOfByte;
begin
  { test_hrt[6]: "ABC\0001234" (len 8) -> HRT "ABC 1234" (NUL replaced by space) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    SetLength(b, 9);
    b[0] := Ord('A'); b[1] := Ord('B'); b[2] := Ord('C'); b[3] := 0;
    b[4] := Ord('1'); b[5] := Ord('2'); b[6] := Ord('3'); b[7] := Ord('4');
    b[8] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 8);
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('ABC 1234', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestTelepen.HRT_ABK0;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[8]: "ABK0" -> HRT "ABK0" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    ret := TZintTestHelper.EncodeData(sym, 'ABK0');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('ABK0', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

{ ---------- TTestTelepenNum ---------- }

procedure TTestTelepenNum.Large_MaxNum_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_large[2]: "1" * 136 -> OK, 1 row, width 1136
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 136));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1136, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepenNum.Large_MaxNum_Plus1_TooLong;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_large[3]: "1" * 137 -> ERROR_TOO_LONG
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 137));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 392: Input length 137 too long (maximum 136)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepenNum.Input_Digits_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[4]: "1234567890"
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567890');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(128, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepenNum.Input_InvalidChar_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[5]: "123456789A" -> INVALID_DATA
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456789A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual(
      'Error 393: Invalid character at position 10 in input (digits and "X" only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepenNum.Input_DigitX_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[6]: "123456789X" -> [0-9]X allowed
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456789X');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(128, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepenNum.Input_XDigit_Rejected;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[7]: "12345678X9" -> X[0-9] not allowed (X at odd position)
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678X9');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual(
      'Error 394: Invalid odd position 9 of "X" in Telepen data',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepenNum.Input_MultipleX_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_input[8]: "1X34567X9X" -> [0-9]X allowed multiple times
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '1X34567X9X');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(128, sym.width, 'width');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepenNum.HRT_Digits;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[10]: "1234" -> HRT "1234" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('1234', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.HRT_DigitX;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[12]: "123X" -> HRT "123X" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '123X');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('123X', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.HRT_LowerX_UpperHRT;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[14]: "123x" -> HRT "123X" (lowercase x -> uppercase in HRT) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '123x');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('123X', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.HRT_OddLength_LeadZero;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[16]: "12345" -> HRT "012345" (leading zero added for odd length) }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('012345', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.Encode_1234567890;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[5]: "1234567890" even-length
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567890');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(128, sym.width, 'width');
    Assert.AreEqual(
      '10101010101110001010101110101110101000101010001010101110101110001011101010001000101110001010101010101011101010101110001010101010',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepenNum.Encode_123456789_OddPadded;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[6]: "123456789" odd -> zero-padded to "012345679"
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(128, sym.width, 'width');
    Assert.AreEqual(
      '10101010101110001110101010111010111000100010001011101110001110001000101010001010111010100010100010111000101110101110001010101010',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepenNum.Encode_123X;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[7]: "123X"
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '123X');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(80, sym.width, 'width');
    Assert.AreEqual(
      '10101010101110001010101110101110111010111000111011101011101110001110001010101010',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepenNum.Encode_1X3X;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[8]: "1X3X"
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '1X3X');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(80, sym.width, 'width');
    Assert.AreEqual(
      '10101010101110001110001110001110111010111000111010111010101110001110001010101010',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepenNum.Encode_3637;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  // test_encode[9]: "3637" - glyph count 127, check 0
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, '3637');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(80, sym.width, 'width');
    Assert.AreEqual(
      '10101010101110001010101010101110111011101110101011101110111011101110001010101010',
      TZintTestHelper.ModulesDump(sym), 'modules');
  finally
    sym.Free;
  end;
end;

procedure TTestTelepen.HRT_ContentSegs_UpperAlpha;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[1] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, 'ABC1234.;$');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('ABC1234.;$', TZintTestHelper.GetText(sym), 'text');
    AssertContentSegEquals(sym, [65, 66, 67, 49, 50, 51, 52, 46, 59, 36, 94], 'content');
  finally sym.Free; end;
end;

procedure TTestTelepen.HRT_ContentSegs_LowerAlpha;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[3] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, 'abc1234.;$');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('abc1234.;$', TZintTestHelper.GetText(sym), 'text');
    AssertContentSegEquals(sym, [97, 98, 99, 49, 50, 51, 52, 46, 59, 36, 125], 'content');
  finally sym.Free; end;
end;

procedure TTestTelepen.HRT_ContentSegs_CtrlChar;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[5] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, 'ABC1234' + #1);
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('ABC1234 ', TZintTestHelper.GetText(sym), 'text');
    AssertContentSegEquals(sym, [65, 66, 67, 49, 50, 51, 52, 1, 107], 'content');
  finally sym.Free; end;
end;

procedure TTestTelepen.HRT_ContentSegs_NUL;
var sym: TZintSymbol; ret: Integer;
  b: TArrayOfByte;
begin
  { test_hrt[7] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    SetLength(b, 9);
    b[0] := Ord('A'); b[1] := Ord('B'); b[2] := Ord('C'); b[3] := 0;
    b[4] := Ord('1'); b[5] := Ord('2'); b[6] := Ord('3'); b[7] := Ord('4');
    b[8] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 8);
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('ABC 1234', TZintTestHelper.GetText(sym), 'text');
    AssertContentSegEquals(sym, [65, 66, 67, 0, 49, 50, 51, 52, 108], 'content');
  finally sym.Free; end;
end;

procedure TTestTelepen.HRT_ContentSegs_ABK0;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[9] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, 'ABK0');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('ABK0', TZintTestHelper.GetText(sym), 'text');
    AssertContentSegEquals(sym, [65, 66, 75, 48, 0], 'content');
  finally sym.Free; end;
end;

procedure TTestTelepen.Fuzz_69Nuls_OK;
var
  sym: TZintSymbol;
  ret, i: Integer;
  b: TArrayOfByte;
begin
  { test_fuzz[0] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    SetLength(b, 70);
    for i := 0 to 68 do b[i] := 0;
    b[69] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 69);
    Assert.AreEqual(ZINT_OK, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestTelepen.Fuzz_70Nuls_TooLong;
var
  sym: TZintSymbol;
  ret, i: Integer;
  b: TArrayOfByte;
begin
  { test_fuzz[1] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN);
  try
    SetLength(b, 71);
    for i := 0 to 69 do b[i] := 0;
    b[70] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 70);
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.HRT_ContentSegs_Digits;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[11] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '1234');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('1234', TZintTestHelper.GetText(sym), 'text');
    AssertContentSegEquals(sym, [49, 50, 51, 52, 27], 'content');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.HRT_ContentSegs_DigitX;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[13] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '123X');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('123X', TZintTestHelper.GetText(sym), 'text');
    AssertContentSegEquals(sym, [49, 50, 51, 88, 68], 'content');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.HRT_ContentSegs_LowerX;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[15] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '123x');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('123X', TZintTestHelper.GetText(sym), 'text');
    AssertContentSegEquals(sym, [49, 50, 51, 88, 68], 'content');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.HRT_ContentSegs_OddLength;
var sym: TZintSymbol; ret: Integer;
begin
  { test_hrt[17] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '12345');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual('012345', TZintTestHelper.GetText(sym), 'text');
    AssertContentSegEquals(sym, [48, 49, 50, 51, 52, 53, 104], 'content');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.Fuzz_70Nuls_InvalidData;
var
  sym: TZintSymbol;
  ret, i: Integer;
  b: TArrayOfByte;
begin
  { test_fuzz[2] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    SetLength(b, 71);
    for i := 0 to 69 do b[i] := 0;
    b[70] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 70);
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.Fuzz_136x0404_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_fuzz[3] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('0404', 34));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.Fuzz_137Digits_TooLong;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_fuzz[4] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 137));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.Fuzz_136ZerosX_OK;
var
  sym: TZintSymbol;
  ret: Integer;
begin
  { test_fuzz[5] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('0', 135) + 'X');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestTelepenNum.Fuzz_Length4_OverlongBuffer_OK;
var
  sym: TZintSymbol;
  ret: Integer;
  b: TArrayOfByte;
begin
  { test_fuzz[7]: logical length 4, backing buffer much larger }
  sym := TZintTestHelper.CreateSymbol(BARCODE_TELEPEN_NUM);
  try
    b := StrToArrayOfByte('12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890');
    SetLength(b, Length(b) + 1);
    b[High(b)] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 4);
    Assert.AreEqual(ZINT_OK, ret, 'ret');
  finally sym.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestTelepen);
  TDUnitX.RegisterTestFixture(TTestTelepenNum);

end.
