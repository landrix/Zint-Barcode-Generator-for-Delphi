unit Test_2of5;

interface

uses
  DUnitX.TestFramework, zint;

type
  {--- C25 Standard (Matrix) ---}
  [TestFixture] TTestC25Standard = class
  public
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Large_CheckDigit_OK;
    [Test] procedure Input_InvalidChar;
    [Test] procedure Input_InvalidCharPos5;
    [Test] procedure HRT_Default;
    [Test] procedure HRT_CheckDigit;
    [Test] procedure HRT_CheckDigitHidden;
    [Test] procedure Encode_Default;
    [Test] procedure Encode_WithCheck;
  end;

  {--- C25 Interleaved ---}
  [TestFixture] TTestC25Inter = class
  public
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Large_CheckDigit_OK;
    [Test] procedure Input_InvalidChar;
    [Test] procedure HRT_OddLeadZero;
    [Test] procedure HRT_CheckDigit;
    [Test] procedure HRT_CheckDigitHidden;
    [Test] procedure HRT_EvenNoLeadZero;
    [Test] procedure HRT_EvenCheckDigitLeadZero;
    [Test] procedure HRT_EvenCheckDigitHidden;
    [Test] procedure Encode_Even;
    [Test] procedure Encode_WithCheck;
    [Test] procedure Encode_Odd;
    [Test] procedure Encode_OddWithCheck;
  end;

  {--- C25 IATA ---}
  [TestFixture] TTestC25IATA = class
  public
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure HRT_Default;
    [Test] procedure HRT_CheckDigit;
    [Test] procedure HRT_CheckDigitHidden;
    [Test] procedure Encode_Default;
    [Test] procedure Encode_WithCheck;
  end;

  {--- C25 Data Logic ---}
  [TestFixture] TTestC25Logic = class
  public
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure HRT_Default;
    [Test] procedure HRT_CheckDigit;
    [Test] procedure HRT_CheckDigitHidden;
    [Test] procedure Encode_Default;
    [Test] procedure Encode_WithCheck;
  end;

  {--- C25 Industrial ---}
  [TestFixture] TTestC25Ind = class
  public
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure HRT_Default;
    [Test] procedure HRT_CheckDigit;
    [Test] procedure HRT_CheckDigitHidden;
    [Test] procedure Encode_Default;
    [Test] procedure Encode_WithCheck;
  end;

  {--- DPLEIT ---}
  [TestFixture] TTestDPLeit = class
  public
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure HRT_Short;
    [Test] procedure HRT_Full;
    [Test] procedure Encode_Zeros;
    [Test] procedure Encode_Schwer;
    [Test] procedure Encode_Wiki;
  end;

  {--- DPIDENT ---}
  [TestFixture] TTestDPIdent = class
  public
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure Input_InvalidCharPos11;
    [Test] procedure HRT_Short;
    [Test] procedure HRT_Full;
    [Test] procedure Encode_Zeros;
    [Test] procedure Encode_Schwer;
    [Test] procedure Encode_Wiki;
  end;

  {--- ITF14 ---}
  [TestFixture] TTestITF14 = class
  public
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure Input_13Digits;
    [Test] procedure Input_14Digits_OK;
    [Test] procedure Input_14Digits_BadCheck;
    [Test] procedure Input_AI_Prefix_01;
    [Test] procedure Input_Paren_Prefix;
    [Test] procedure Input_Bare_01_Prefix;
    [Test] procedure HRT_Short;
    [Test] procedure HRT_Full;
    [Test] procedure Encode_Default;
    [Test] procedure Encode_GS1_1;
    [Test] procedure Encode_GS1_2;
  end;

implementation

uses TestHelper_Zint;

{ ========== C25 Standard ========== }

procedure TTestC25Standard.Large_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 112));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1137, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Standard.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 113));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 301: Input length 113 too long (maximum 112)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Standard.Large_CheckDigit_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 112));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1147, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Standard.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 302: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Standard.Input_InvalidCharPos5;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234A6');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 302: Invalid character at position 5 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Standard.HRT_Default;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Standard.HRT_CheckDigit;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('1234567895', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Standard.HRT_CheckDigitHidden;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Standard.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[0]: Standard, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(97, sym.width, 'width');
    Assert.AreEqual('1111010101110100010101000111010001110101110111010101110111011100010101000101110111010111011110101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestC25Standard.Encode_WithCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[1]: Standard with check, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(107, sym.width, 'width');
    Assert.AreEqual('11110101011101000101010001110100011101011101110101011101110111000101010001011101110101110100010111011110101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== C25 Interleaved ========== }

procedure TTestC25Inter.Large_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 125));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1143, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Inter.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 126));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 309: Input length 126 too long (maximum 125)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Inter.Large_CheckDigit_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 125));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1143, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Inter.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 310: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Inter.HRT_OddLeadZero;
var sym: TZintSymbol;
begin
  { test_hrt[6]: odd input gets leading zero }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('0123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Inter.HRT_CheckDigit;
var sym: TZintSymbol;
begin
  { test_hrt[8]: odd input + check digit = even, no leading zero }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('1234567895', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Inter.HRT_CheckDigitHidden;
var sym: TZintSymbol;
begin
  { test_hrt[10]: hidden check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Inter.HRT_EvenNoLeadZero;
var sym: TZintSymbol;
begin
  { test_hrt[12]: even input, no leading zero }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    TZintTestHelper.EncodeData(sym, '1234567890');
    Assert.AreEqual('1234567890', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Inter.HRT_EvenCheckDigitLeadZero;
var sym: TZintSymbol;
begin
  { test_hrt[14]: even input + check digit = odd, so leading zero added }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '1234567890');
    Assert.AreEqual('012345678905', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Inter.HRT_EvenCheckDigitHidden;
var sym: TZintSymbol;
begin
  { test_hrt[16]: even input + hidden check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '1234567890');
    Assert.AreEqual('01234567890', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Inter.Encode_Even;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[3]: even, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(81, sym.width, 'width');
    Assert.AreEqual('101011101010111000100010001110111000101010001000111010111010001110101011100011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestC25Inter.Encode_WithCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[4]: with check digit, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(99, sym.width, 'width');
    Assert.AreEqual('101010001011101110001010100010001110111011101011100010100011101110001010100011101000101011100011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestC25Inter.Encode_Odd;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[5]: odd, "7654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    ret := TZintTestHelper.EncodeData(sym, '7654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(81, sym.width, 'width');
    Assert.AreEqual('101010101110111000100010001110111000101010001000111010111010001110101011100011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestC25Inter.Encode_OddWithCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[6]: odd with check, "7654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '7654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(81, sym.width, 'width');
    Assert.AreEqual('101010100010001110111011101011100010100011101110001010100011101010001000111011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== C25 IATA ========== }

procedure TTestC25IATA.Large_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 80));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1129, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25IATA.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 81));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 305: Input length 81 too long (maximum 80)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25IATA.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 306: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25IATA.HRT_Default;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25IATA.HRT_CheckDigit;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('1234567895', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25IATA.HRT_CheckDigitHidden;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25IATA.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[8]: IATA, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(121, sym.width, 'width');
    Assert.AreEqual('1010111010101110101010101110111010111011101010111010111010101010111010111011101110101010101110101011101110101010111011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestC25IATA.Encode_WithCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[9]: IATA with check, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
    Assert.AreEqual('101011101010111010101010111011101011101110101011101011101010101011101011101110111010101010111010101110111010101011101011101010111011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== C25 Data Logic ========== }

procedure TTestC25Logic.Large_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 113));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1139, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Logic.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 114));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 307: Input length 114 too long (maximum 113)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Logic.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 308: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Logic.HRT_Default;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Logic.HRT_CheckDigit;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('1234567895', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Logic.HRT_CheckDigitHidden;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Logic.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[10]: Data Logic, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(89, sym.width, 'width');
    Assert.AreEqual('10101110100010101000111010001110101110111010101110111011100010101000101110111010111011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestC25Logic.Encode_WithCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[11]: Data Logic with check, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(99, sym.width, 'width');
    Assert.AreEqual('101011101000101010001110100011101011101110101011101110111000101010001011101110101110100010111011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== C25 Industrial ========== }

procedure TTestC25Ind.Large_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 79));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(1125, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Ind.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 80));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 303: Input length 80 too long (maximum 79)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Ind.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 304: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Ind.HRT_Default;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Ind.HRT_CheckDigit;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('1234567895', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Ind.HRT_CheckDigitHidden;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Ind.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[12]: Industrial, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(131, sym.width, 'width');
    Assert.AreEqual('11101110101110101011101010101011101110101110111010101110101110101010101110101110111011101010101011101010111011101010101110111010111',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestC25Ind.Encode_WithCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[13]: Industrial with check, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(145, sym.width, 'width');
    Assert.AreEqual('1110111010111010101110101010101110111010111011101010111010111010101010111010111011101110101010101110101011101110101010111010111010101110111010111',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== DPLEIT ========== }

procedure TTestDPLeit.Large_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 13));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestDPLeit.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 14));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 313: Input length 14 too long (maximum 13)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestDPLeit.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 314: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestDPLeit.HRT_Short;
var sym: TZintSymbol;
begin
  { test_hrt[36]: "123456789" -> "00001.234.567.890" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('00001.234.567.890', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestDPLeit.HRT_Full;
var sym: TZintSymbol;
begin
  { test_hrt[38]: "1234567890123" -> "12345.678.901.236" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    TZintTestHelper.EncodeData(sym, '1234567890123');
    Assert.AreEqual('12345.678.901.236', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestDPLeit.Encode_Zeros;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[15] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    ret := TZintTestHelper.EncodeData(sym, '0000087654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
    Assert.AreEqual('101010101110001110001010101110001110001010001011101110001010100010001110111011101011100010100011101110001010100011101000100010111011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestDPLeit.Encode_Schwer;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[17]: DIALOGPOST SCHWER brochure 3.1 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    ret := TZintTestHelper.EncodeData(sym, '2045703000360');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
    Assert.AreEqual('101010111010001000111010001011100010111010101000111000111011101110100010001010101110001110001011101110001000101010001011100011101011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestDPLeit.Encode_Wiki;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[18]: Wikipedia Leitcode example }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    ret := TZintTestHelper.EncodeData(sym, '5082300702800');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
    Assert.AreEqual('101011101011100010001011101000101110100011101110100010001010101110111000100010100011101110100011101010001110001010001011100011101011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== DPIDENT ========== }

procedure TTestDPIdent.Large_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 11));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(117, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestDPIdent.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 12));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 315: Input length 12 too long (maximum 11)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestDPIdent.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 316: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestDPIdent.Input_InvalidCharPos11;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[9]: invalid char at position 11 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567890A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 316: Invalid character at position 11 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestDPIdent.HRT_Short;
var sym: TZintSymbol;
begin
  { test_hrt[40]: "123456789" -> "00.12 3.456.789 0" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('00.12 3.456.789 0', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestDPIdent.HRT_Full;
var sym: TZintSymbol;
begin
  { test_hrt[42]: "12345678901" -> "12.34 5.678.901 6" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    TZintTestHelper.EncodeData(sym, '12345678901');
    Assert.AreEqual('12.34 5.678.901 6', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestDPIdent.Encode_Zeros;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[19] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    ret := TZintTestHelper.EncodeData(sym, '00087654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(117, sym.width, 'width');
    Assert.AreEqual('101010101110001110001010001011101110001010100010001110111011101011100010100011101110001010100011101000100010111011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestDPIdent.Encode_Schwer;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[21]: DIALOGPOST SCHWER brochure 3.1 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    ret := TZintTestHelper.EncodeData(sym, '80420000001');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(117, sym.width, 'width');
    Assert.AreEqual('101011101010001110001010100011101011100010101110001110001010101110001110001010101110001110001011101010001000111011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestDPIdent.Encode_Wiki;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[22]: Wikipedia Identcode example }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    ret := TZintTestHelper.EncodeData(sym, '39601313414');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(117, sym.width, 'width');
    Assert.AreEqual('101011101110001010001010111011100010001011100010001010111011100010001010111010001011101011100010101110001000111011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== ITF14 ========== }

procedure TTestITF14.Large_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('0', 14));
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('0', 15));
    Assert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    Assert.AreEqual('Error 311: Input length 15 too long (maximum 14)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    Assert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    Assert.AreEqual('Error 312: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_13Digits;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[11]: 13 digits, check digit auto-calculated }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567890123');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_14Digits_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[12]: 14 digits with correct check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678901231');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_14Digits_BadCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[13]: 14 digits with bad check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678901234');
    Assert.AreEqual(ZINT_ERROR_INVALID_CHECK, ret, 'ret');
    Assert.AreEqual('Error 850: Invalid check digit ''4'', expecting ''1''',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_AI_Prefix_01;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[19]: [01] prefix with check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '[01]12345678901231');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_Paren_Prefix;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[21]: (01) prefix with check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '(01)12345678901231');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_Bare_01_Prefix;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[17]: bare 01 prefix with check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '0112345678901231');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.HRT_Short;
var sym: TZintSymbol;
begin
  { test_hrt[44]: "123456789" -> "00001234567895" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    Assert.AreEqual('00001234567895', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestITF14.HRT_Full;
var sym: TZintSymbol;
begin
  { test_hrt[46]: "1234567890123" -> "12345678901231" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    TZintTestHelper.EncodeData(sym, '1234567890123');
    Assert.AreEqual('12345678901231', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestITF14.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[23] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '0000087654321');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
    Assert.AreEqual('101010101110001110001010101110001110001010001011101110001010100010001110111011101011100010100011101110001010100011101000101011100011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestITF14.Encode_GS1_1;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[25]: GS1 Figure 5.1-2 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '0950110153000');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
    Assert.AreEqual('101010100011101110001011101011100010001011100010101011100010001011101110100011100010001110101010101110001110001010001000111011101011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestITF14.Encode_GS1_2;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[26]: GS1 Figure 5.3.2.4-1 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '1540014128876');
    Assert.AreEqual(ZINT_OK, ret, 'ret');
    Assert.AreEqual(1, sym.rows, 'rows');
    Assert.AreEqual(135, sym.width, 'width');
    Assert.AreEqual('101011100010100010111010101110001000111010001011101110100010001011101011100010001110101000111011101010111000100010001110001110101011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestC25Standard);
  TDUnitX.RegisterTestFixture(TTestC25Inter);
  TDUnitX.RegisterTestFixture(TTestC25IATA);
  TDUnitX.RegisterTestFixture(TTestC25Logic);
  TDUnitX.RegisterTestFixture(TTestC25Ind);
  TDUnitX.RegisterTestFixture(TTestDPLeit);
  TDUnitX.RegisterTestFixture(TTestDPIdent);
  TDUnitX.RegisterTestFixture(TTestITF14);

end.
