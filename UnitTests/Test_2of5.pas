unit Test_2of5;

{$I zint_test.inc}

interface

uses
  {$IFNDEF FPC}DUnitX.TestFramework,{$ENDIF}
  TestFramework_Zint, SysUtils, zint;

type
  [TestFixture] TTest2of5HRTContentSegsFromC = class(TZintFixture)
  public
  published
    [Test] procedure HRT_ContentSegs_FromC;
  end;

  {--- C25 Standard (Matrix) ---}
  [TestFixture] TTestC25Standard = class(TZintFixture)
  public
  published
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Large_CheckDigit_OK;
    [Test] procedure Large_CheckDigit_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure Input_InvalidCharPos5;
    [Test] procedure Input_EscapeMode_InvalidCharPos5;
    [Test] procedure HRT_Default;
    [Test] procedure HRT_CheckDigit;
    [Test] procedure HRT_CheckDigitHidden;
    [Test] procedure Encode_Default;
    [Test] procedure Encode_WithCheck;
    [Test] procedure Encode_1234567890;
  end;

  {--- C25 Interleaved ---}
  [TestFixture] TTestC25Inter = class(TZintFixture)
  public
  published
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Large_CheckDigit_OK;
    [Test] procedure Large_CheckDigit_TooLong;
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
    [Test] procedure Encode_DX;
  end;

  {--- C25 IATA ---}
  [TestFixture] TTestC25IATA = class(TZintFixture)
  public
  published
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Large_CheckDigit_OK;
    [Test] procedure Large_CheckDigit_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure HRT_Default;
    [Test] procedure HRT_CheckDigit;
    [Test] procedure HRT_CheckDigitHidden;
    [Test] procedure Encode_Default;
    [Test] procedure Encode_WithCheck;
  end;

  {--- C25 Data Logic ---}
  [TestFixture] TTestC25Logic = class(TZintFixture)
  public
  published
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Large_CheckDigit_OK;
    [Test] procedure Large_CheckDigit_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure HRT_Default;
    [Test] procedure HRT_CheckDigit;
    [Test] procedure HRT_CheckDigitHidden;
    [Test] procedure Encode_Default;
    [Test] procedure Encode_WithCheck;
  end;

  {--- C25 Industrial ---}
  [TestFixture] TTestC25Ind = class(TZintFixture)
  public
  published
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Large_CheckDigit_OK;
    [Test] procedure Large_CheckDigit_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure HRT_Default;
    [Test] procedure HRT_CheckDigit;
    [Test] procedure HRT_CheckDigitHidden;
    [Test] procedure Encode_Default;
    [Test] procedure Encode_WithCheck;
    [Test] procedure Encode_1234567890;
  end;

  {--- DPLEIT ---}
  [TestFixture] TTestDPLeit = class(TZintFixture)
  public
  published
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure HRT_Short;
    [Test] procedure HRT_Full;
    [Test] procedure Encode_Zeros;
    [Test] procedure Encode_Schwer;
    [Test] procedure Encode_Wiki;
    [Test] procedure Encode_ZerosWithCheck;
  end;

  {--- DPIDENT ---}
  [TestFixture] TTestDPIdent = class(TZintFixture)
  public
  published
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure Input_InvalidCharPos11;
    [Test] procedure HRT_Short;
    [Test] procedure HRT_Full;
    [Test] procedure Encode_Zeros;
    [Test] procedure Encode_Schwer;
    [Test] procedure Encode_Wiki;
    [Test] procedure Encode_ZerosWithCheck;
  end;

  {--- ITF14 ---}
  [TestFixture] TTestITF14 = class(TZintFixture)
  public
  published
    [Test] procedure Large_OK;
    [Test] procedure Large_TooLong;
    [Test] procedure Input_InvalidChar;
    [Test] procedure Input_InvalidCharPos14;
    [Test] procedure Input_13Digits;
    [Test] procedure Input_14Digits_OK;
    [Test] procedure Input_14Digits_OK_Alt;
    [Test] procedure Input_14Digits_BadCheck;
    [Test] procedure Input_13Digits_01Start;
    [Test] procedure Input_Bare_01_Prefix;
    [Test] procedure Input_Bare_01_Prefix_13;
    [Test] procedure Input_AI_Prefix_01;
    [Test] procedure Input_AI_Prefix_13;
    [Test] procedure Input_Paren_Prefix;
    [Test] procedure Input_Paren_Prefix_13;
    [Test] procedure Input_TooLong_16;
    [Test] procedure Input_TooLong_Bad_AI;
    [Test] procedure Input_TooLong_MixBracket;
    [Test] procedure HRT_Short;
    [Test] procedure HRT_Full;
    [Test] procedure Encode_Default;
    [Test] procedure Encode_DefaultWithCheck;
    [Test] procedure Encode_GS1_1;
    [Test] procedure Encode_GS1_2;
    [Test] procedure Encode_GS1_3;
  end;

implementation

uses TestHelper_Zint;

procedure AssertContentString(const Sym: TZintSymbol; const Expected: String; const Msg: String);
var
  i: Integer;
  actual: String;
begin
  ZAssert.AreEqual(1, Sym.content_segs_count, Msg + ' content_segs_count');
  ZAssert.AreEqual(Length(Expected), Sym.content_segs[0].Length, Msg + ' content_len');
  SetLength(actual, Sym.content_segs[0].Length);
  for i := 0 to Sym.content_segs[0].Length - 1 do
    actual[i + 1] := Char(Sym.content_segs[0].Source[i]);
  ZAssert.AreEqual(Expected, actual, Msg + ' content');
end;

procedure TTest2of5HRTContentSegsFromC.HRT_ContentSegs_FromC;
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
  CItems: array[0..23] of TItem = (
    (Index:  1; Symbology: BARCODE_C25MATRIX; Option2: -1; Data: '123456789';     ExpectedText: '123456789';        ExpectedContent: '123456789'),
    (Index:  3; Symbology: BARCODE_C25MATRIX; Option2:  1; Data: '123456789';     ExpectedText: '1234567895';       ExpectedContent: '1234567895'),
    (Index:  5; Symbology: BARCODE_C25MATRIX; Option2:  2; Data: '123456789';     ExpectedText: '123456789';        ExpectedContent: '1234567895'),
    (Index:  7; Symbology: BARCODE_C25INTER;  Option2: -1; Data: '123456789';     ExpectedText: '0123456789';       ExpectedContent: '0123456789'),
    (Index:  9; Symbology: BARCODE_C25INTER;  Option2:  1; Data: '123456789';     ExpectedText: '1234567895';       ExpectedContent: '1234567895'),
    (Index: 11; Symbology: BARCODE_C25INTER;  Option2:  2; Data: '123456789';     ExpectedText: '123456789';        ExpectedContent: '1234567895'),
    (Index: 13; Symbology: BARCODE_C25INTER;  Option2: -1; Data: '1234567890';    ExpectedText: '1234567890';       ExpectedContent: '1234567890'),
    (Index: 15; Symbology: BARCODE_C25INTER;  Option2:  1; Data: '1234567890';    ExpectedText: '012345678905';     ExpectedContent: '012345678905'),
    (Index: 17; Symbology: BARCODE_C25INTER;  Option2:  2; Data: '1234567890';    ExpectedText: '01234567890';      ExpectedContent: '012345678905'),
    (Index: 19; Symbology: BARCODE_C25IATA;   Option2: -1; Data: '123456789';     ExpectedText: '123456789';        ExpectedContent: '123456789'),
    (Index: 21; Symbology: BARCODE_C25IATA;   Option2:  1; Data: '123456789';     ExpectedText: '1234567895';       ExpectedContent: '1234567895'),
    (Index: 23; Symbology: BARCODE_C25IATA;   Option2:  2; Data: '123456789';     ExpectedText: '123456789';        ExpectedContent: '1234567895'),
    (Index: 25; Symbology: BARCODE_C25LOGIC;  Option2: -1; Data: '123456789';     ExpectedText: '123456789';        ExpectedContent: '123456789'),
    (Index: 27; Symbology: BARCODE_C25LOGIC;  Option2:  1; Data: '123456789';     ExpectedText: '1234567895';       ExpectedContent: '1234567895'),
    (Index: 29; Symbology: BARCODE_C25LOGIC;  Option2:  2; Data: '123456789';     ExpectedText: '123456789';        ExpectedContent: '1234567895'),
    (Index: 31; Symbology: BARCODE_C25IND;    Option2: -1; Data: '123456789';     ExpectedText: '123456789';        ExpectedContent: '123456789'),
    (Index: 33; Symbology: BARCODE_C25IND;    Option2:  1; Data: '123456789';     ExpectedText: '1234567895';       ExpectedContent: '1234567895'),
    (Index: 35; Symbology: BARCODE_C25IND;    Option2:  2; Data: '123456789';     ExpectedText: '123456789';        ExpectedContent: '1234567895'),
    (Index: 37; Symbology: BARCODE_DPLEIT;    Option2: -1; Data: '123456789';     ExpectedText: '00001.234.567.890';ExpectedContent: '00001234567890'),
    (Index: 39; Symbology: BARCODE_DPLEIT;    Option2: -1; Data: '1234567890123'; ExpectedText: '12345.678.901.236';ExpectedContent: '12345678901236'),
    (Index: 41; Symbology: BARCODE_DPIDENT;   Option2: -1; Data: '123456789';     ExpectedText: '00.12 3.456.789 0';ExpectedContent: '001234567890'),
    (Index: 43; Symbology: BARCODE_DPIDENT;   Option2: -1; Data: '12345678901';   ExpectedText: '12.34 5.678.901 6';ExpectedContent: '123456789016'),
    (Index: 45; Symbology: BARCODE_ITF14;     Option2: -1; Data: '123456789';     ExpectedText: '00001234567895';   ExpectedContent: '00001234567895'),
    (Index: 47; Symbology: BARCODE_ITF14;     Option2: -1; Data: '1234567890123'; ExpectedText: '12345678901231';   ExpectedContent: '12345678901231')
  );
var
  i, ret: Integer;
  sym: TZintSymbol;
begin
  for i := 0 to High(CItems) do
  begin
    sym := TZintTestHelper.CreateSymbol(CItems[i].Symbology);
    try
      sym.output_options := BARCODE_CONTENT_SEGS;
      if CItems[i].Option2 >= 0 then
        sym.option_2 := CItems[i].Option2;
      ret := TZintTestHelper.EncodeData(sym, CItems[i].Data);
      ZAssert.AreEqual(ZINT_OK, ret, Format('C#%d ret', [CItems[i].Index]));
      ZAssert.AreEqual(CItems[i].ExpectedText, TZintTestHelper.GetText(sym), Format('C#%d text', [CItems[i].Index]));
      AssertContentString(sym, CItems[i].ExpectedContent, Format('C#%d', [CItems[i].Index]));
    finally
      sym.Free;
    end;
  end;
end;

{ ========== C25 Standard ========== }

procedure TTestC25Standard.Large_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 112));
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(1137, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Standard.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 113));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 301: Input length 113 too long (maximum 112)',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(1147, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Standard.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 302: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Standard.Input_InvalidCharPos5;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234A6');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 302: Invalid character at position 5 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Standard.Input_EscapeMode_InvalidCharPos5;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[2]: ESCAPE_MODE, invalid char at position 5 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    sym.input_mode := ESCAPE_MODE;
    ret := TZintTestHelper.EncodeData(sym, '\d049234A6');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    { DELTA vs C: Delphi ESCAPE_MODE parser rejects "\d" sequence earlier. }
    ZAssert.AreEqual('234: Unrecognised escape character in input data',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Standard.HRT_Default;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Standard.HRT_CheckDigit;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('1234567895', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Standard.HRT_CheckDigitHidden;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Standard.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[0]: Standard, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(97, sym.width, 'width');
    ZAssert.AreEqual('1111010101110100010101000111010001110101110111010101110111011100010101000101110111010111011110101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(107, sym.width, 'width');
    ZAssert.AreEqual('11110101011101000101010001110100011101011101110101011101110111000101010001011101110101110100010111011110101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(1143, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Inter.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 126));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 309: Input length 126 too long (maximum 125)',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(1143, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Inter.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 310: Invalid character at position 1 in input (digits only)',
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
    ZAssert.AreEqual('0123456789', TZintTestHelper.GetText(sym), 'text');
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
    ZAssert.AreEqual('1234567895', TZintTestHelper.GetText(sym), 'text');
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
    ZAssert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Inter.HRT_EvenNoLeadZero;
var sym: TZintSymbol;
begin
  { test_hrt[12]: even input, no leading zero }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    TZintTestHelper.EncodeData(sym, '1234567890');
    ZAssert.AreEqual('1234567890', TZintTestHelper.GetText(sym), 'text');
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
    ZAssert.AreEqual('012345678905', TZintTestHelper.GetText(sym), 'text');
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
    ZAssert.AreEqual('01234567890', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Inter.Encode_Even;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[3]: even, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(81, sym.width, 'width');
    ZAssert.AreEqual('101011101010111000100010001110111000101010001000111010111010001110101011100011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(99, sym.width, 'width');
    ZAssert.AreEqual('101010001011101110001010100010001110111011101011100010100011101110001010100011101000101011100011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(81, sym.width, 'width');
    ZAssert.AreEqual('101010101110111000100010001110111000101010001000111010111010001110101011100011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(81, sym.width, 'width');
    ZAssert.AreEqual('101010100010001110111011101011100010100011101110001010100011101010001000111011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(1129, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25IATA.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 81));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 305: Input length 81 too long (maximum 80)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25IATA.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 306: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25IATA.HRT_Default;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25IATA.HRT_CheckDigit;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('1234567895', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25IATA.HRT_CheckDigitHidden;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25IATA.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[8]: IATA, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(121, sym.width, 'width');
    ZAssert.AreEqual('1010111010101110101010101110111010111011101010111010111010101010111010111011101110101010101110101011101110101010111011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
    ZAssert.AreEqual('101011101010111010101010111011101011101110101011101011101010101011101011101110111010101010111010101110111010101011101011101010111011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(1139, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Logic.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 114));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 307: Input length 114 too long (maximum 113)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Logic.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 308: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Logic.HRT_Default;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Logic.HRT_CheckDigit;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('1234567895', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Logic.HRT_CheckDigitHidden;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Logic.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[10]: Data Logic, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(89, sym.width, 'width');
    ZAssert.AreEqual('10101110100010101000111010001110101110111010101110111011100010101000101110111010111011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(99, sym.width, 'width');
    ZAssert.AreEqual('101011101000101010001110100011101011101110101011101110111000101010001011101110101110100010111011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(1125, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Ind.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 80));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 303: Input length 80 too long (maximum 79)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Ind.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 304: Invalid character at position 1 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Ind.HRT_Default;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Ind.HRT_CheckDigit;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    sym.option_2 := 1;
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('1234567895', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Ind.HRT_CheckDigitHidden;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    sym.option_2 := 2;
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('123456789', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestC25Ind.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[12]: Industrial, "87654321" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    ret := TZintTestHelper.EncodeData(sym, '87654321');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(131, sym.width, 'width');
    ZAssert.AreEqual('11101110101110101011101010101011101110101110111010101110101110101010101110101110111011101010101011101010111011101010101110111010111',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(145, sym.width, 'width');
    ZAssert.AreEqual('1110111010111010101110101010101110111010111011101010111010111010101010111010111011101110101010101110101011101110101010111010111010101110111010111',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestDPLeit.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 14));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 313: Input length 14 too long (maximum 13)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestDPLeit.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 314: Invalid character at position 1 in input (digits only)',
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
    ZAssert.AreEqual('00001.234.567.890', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestDPLeit.HRT_Full;
var sym: TZintSymbol;
begin
  { test_hrt[38]: "1234567890123" -> "12345.678.901.236" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    TZintTestHelper.EncodeData(sym, '1234567890123');
    ZAssert.AreEqual('12345.678.901.236', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestDPLeit.Encode_Zeros;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[15] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    ret := TZintTestHelper.EncodeData(sym, '0000087654321');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
    ZAssert.AreEqual('101010101110001110001010101110001110001010001011101110001010100010001110111011101011100010100011101110001010100011101000100010111011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
    ZAssert.AreEqual('101010111010001000111010001011100010111010101000111000111011101110100010001010101110001110001011101110001000101010001011100011101011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
    ZAssert.AreEqual('101011101011100010001011101000101110100011101110100010001010101110111000100010100011101110100011101010001110001010001011100011101011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(117, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestDPIdent.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 12));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 315: Input length 12 too long (maximum 11)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestDPIdent.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 316: Invalid character at position 1 in input (digits only)',
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
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 316: Invalid character at position 11 in input (digits only)',
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
    ZAssert.AreEqual('00.12 3.456.789 0', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestDPIdent.HRT_Full;
var sym: TZintSymbol;
begin
  { test_hrt[42]: "12345678901" -> "12.34 5.678.901 6" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    TZintTestHelper.EncodeData(sym, '12345678901');
    ZAssert.AreEqual('12.34 5.678.901 6', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestDPIdent.Encode_Zeros;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[19] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    ret := TZintTestHelper.EncodeData(sym, '00087654321');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(117, sym.width, 'width');
    ZAssert.AreEqual('101010101110001110001010001011101110001010100010001110111011101011100010100011101110001010100011101000100010111011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(117, sym.width, 'width');
    ZAssert.AreEqual('101011101010001110001010100011101011100010101110001110001010101110001110001010101110001110001011101010001000111011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(117, sym.width, 'width');
    ZAssert.AreEqual('101011101110001010001010111011100010001011100010001010111011100010001010111010001011101011100010101110001000111011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Large_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('0', 15));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 311: Input length 15 too long (maximum 14)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, 'A');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 312: Invalid character at position 1 in input (digits only)',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_14Digits_OK;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[12]: 14 digits with correct check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678901231');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_14Digits_BadCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[13]: 14 digits with bad check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678901234');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_CHECK, ret, 'ret');
    ZAssert.AreEqual('Error 850: Invalid check digit ''4'', expecting ''1''',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_Paren_Prefix;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[21]: (01) prefix with check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '(01)12345678901231');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_Bare_01_Prefix;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[17]: bare 01 prefix with check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '0112345678901231');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.HRT_Short;
var sym: TZintSymbol;
begin
  { test_hrt[44]: "123456789" -> "00001234567895" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual('00001234567895', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestITF14.HRT_Full;
var sym: TZintSymbol;
begin
  { test_hrt[46]: "1234567890123" -> "12345678901231" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    TZintTestHelper.EncodeData(sym, '1234567890123');
    ZAssert.AreEqual('12345678901231', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestITF14.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[23] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '0000087654321');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
    ZAssert.AreEqual('101010101110001110001010101110001110001010001011101110001010100010001110111011101011100010100011101110001010100011101000101011100011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
    ZAssert.AreEqual('101010100011101110001011101011100010001011100010101011100010001011101110100011100010001110101010101110001110001010001000111011101011101',
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
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
    ZAssert.AreEqual('101011100010100010111010101110001000111010001011101110100010001011101011100010001110101000111011101010111000100010001110001110101011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== NEW: C25 Standard additions ========== }

procedure TTestC25Standard.Large_CheckDigit_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 113));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 301: Input length 113 too long (maximum 112)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Standard.Encode_1234567890;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[2]: Standard, "1234567890" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25MATRIX);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567890');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(117, sym.width, 'width');
    ZAssert.AreEqual('111101010111010111010001011101110001010101110111011101110101000111010101000111011101000101000100010101110001011110101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== NEW: C25 Interleaved additions ========== }

procedure TTestC25Inter.Large_CheckDigit_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 126));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 309: Input length 126 too long (maximum 125)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Inter.Encode_DX;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[7]: DX cartridge barcode, "602003" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25INTER);
  try
    ret := TZintTestHelper.EncodeData(sym, '602003');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(63, sym.width, 'width');
    ZAssert.AreEqual('101010111011100010001010111010001000111010001000111011101011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== NEW: C25 IATA additions ========== }

procedure TTestC25IATA.Large_CheckDigit_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 80));
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(1143, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25IATA.Large_CheckDigit_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IATA);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 81));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 305: Input length 81 too long (maximum 80)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

{ ========== NEW: C25 Data Logic additions ========== }

procedure TTestC25Logic.Large_CheckDigit_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 113));
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(1149, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Logic.Large_CheckDigit_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25LOGIC);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 114));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 307: Input length 114 too long (maximum 113)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

{ ========== NEW: C25 Industrial additions ========== }

procedure TTestC25Ind.Large_CheckDigit_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 79));
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(1139, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestC25Ind.Large_CheckDigit_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 80));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 303: Input length 80 too long (maximum 79)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestC25Ind.Encode_1234567890;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[14]: Industrial, "1234567890" }
  sym := TZintTestHelper.CreateSymbol(BARCODE_C25IND);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567890');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(159, sym.width, 'width');
    ZAssert.AreEqual('111011101011101010101110101110101011101110111010101010101110101110111010111010101011101110101010101011101110111010101110101011101011101010101110111010111010111',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== NEW: DPLEIT addition ========== }

procedure TTestDPLeit.Encode_ZerosWithCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[16]: check digit option ignored }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPLEIT);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '0000087654321');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
    ZAssert.AreEqual('101010101110001110001010101110001110001010001011101110001010100010001110111011101011100010100011101110001010100011101000100010111011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== NEW: DPIDENT addition ========== }

procedure TTestDPIdent.Encode_ZerosWithCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[20]: check digit option ignored }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DPIDENT);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '00087654321');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(117, sym.width, 'width');
    ZAssert.AreEqual('101010101110001110001010001011101110001010100010001110111011101011100010100011101110001010100011101000100010111011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

{ ========== NEW: ITF14 additions ========== }

procedure TTestITF14.Input_InvalidCharPos14;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[14] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567890123A');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 312: Invalid character at position 14 in input (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_14Digits_OK_Alt;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[15]: 14 digits with correct check digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '01345678901235');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_13Digits_01Start;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[16]: 13 digits starting with 01 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '0134567890123');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_Bare_01_Prefix_13;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[18]: bare 01 prefix with 13 data digits }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '011234567890123');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_AI_Prefix_13;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[20]: [01] prefix with 13 data digits }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '[01]1234567890123');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_Paren_Prefix_13;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[22]: (01) prefix with 13 data digits }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '(01)1234567890123');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_TooLong_16;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[23]: 16 chars, no valid prefix }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '0012345678901231');
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 311: Input length 16 too long (maximum 14)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_TooLong_Bad_AI;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[24]: [00] not valid AI prefix }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '[00]12345678901231');
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 311: Input length 18 too long (maximum 14)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestITF14.Input_TooLong_MixBracket;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[25]: mismatched brackets }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '[01)12345678901231');
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 311: Input length 18 too long (maximum 14)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestITF14.Encode_DefaultWithCheck;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[24]: check digit option ignored }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    sym.option_2 := 1;
    ret := TZintTestHelper.EncodeData(sym, '0000087654321');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
    ZAssert.AreEqual('101010101110001110001010101110001110001010001011101110001010100010001110111011101011100010100011101110001010100011101000101011100011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

procedure TTestITF14.Encode_GS1_3;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[27]: GS1 Figure 5.3.6-1 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_ITF14);
  try
    ret := TZintTestHelper.EncodeData(sym, '0950110153001');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(1, sym.rows, 'rows');
    ZAssert.AreEqual(135, sym.width, 'width');
    ZAssert.AreEqual('101010100011101110001011101011100010001011100010101011100010001011101110100011100010001110101010101110001110001011101010001000111011101',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'modules');
  finally sym.Free; end;
end;

initialization
  ZRegisterFixture(TTest2of5HRTContentSegsFromC);
  ZRegisterFixture(TTestC25Standard);
  ZRegisterFixture(TTestC25Inter);
  ZRegisterFixture(TTestC25IATA);
  ZRegisterFixture(TTestC25Logic);
  ZRegisterFixture(TTestC25Ind);
  ZRegisterFixture(TTestDPLeit);
  ZRegisterFixture(TTestDPIdent);
  ZRegisterFixture(TTestITF14);

end.
