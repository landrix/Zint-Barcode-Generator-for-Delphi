unit Test_Auspost;

{$I zint_test.inc}

interface

uses
  {$IFNDEF FPC}DUnitX.TestFramework,{$ENDIF}
  TestFramework_Zint, zint;

type
  {--- AUSPOST ---}
  [TestFixture] TTestAusPost = class(TZintFixture)
  public
    { test_large }
  published
    [Test] procedure Large_8_OK;
    [Test] procedure Large_9_TooLong;
    [Test] procedure Large_13_OK;
    [Test] procedure Large_14_TooLong;
    [Test] procedure Large_16_OK;
    [Test] procedure Large_17_TooLong;
    [Test] procedure Large_18_OK;
    [Test] procedure Large_19_TooLong;
    [Test] procedure Large_23_OK;
    [Test] procedure Large_24_TooLong;
    { test_hrt }
    [Test] procedure HRT_Empty_8;
    [Test] procedure HRT_ContentSegs_8;
    [Test] procedure HRT_Empty_13;
    [Test] procedure HRT_ContentSegs_13;
    [Test] procedure HRT_Empty_16;
    [Test] procedure HRT_ContentSegs_16;
    [Test] procedure HRT_Empty_18;
    [Test] procedure HRT_ContentSegs_18;
    [Test] procedure HRT_Empty_23;
    [Test] procedure HRT_ContentSegs_23;
    { test_input }
    [Test] procedure Input_8Digits_OK;
    [Test] procedure Input_DPID_InvalidChar;
    [Test] procedure Input_13Mixed_OK;
    [Test] procedure Input_13_InvalidChar;
    [Test] procedure Input_16Digits_OK;
    [Test] procedure Input_16_InvalidChar;
    [Test] procedure Input_18Mixed_OK;
    [Test] procedure Input_23Digits_OK;
    [Test] procedure Input_23_InvalidChar;
    [Test] procedure Input_WrongLength;
    { test_encode }
    [Test] procedure Encode_FCC11;
    [Test] procedure Encode_FCC11_Alt;
    [Test] procedure Encode_FCC59_AlphaNum;
    [Test] procedure Encode_FCC59_Numeric;
    [Test] procedure Encode_FCC59_Alpha;
    [Test] procedure Encode_FCC62_Numeric;
    [Test] procedure Encode_FCC62_Alpha;
    [Test] procedure Encode_FCC62_GDSET1;
    [Test] procedure Encode_FCC62_GDSET2;
    [Test] procedure Encode_FCC62_GDSET3;
    [Test] procedure Encode_FCC62_GDSET4;
    [Test] procedure Encode_FCC59_GDSET5;
  end;

  {--- AUSREPLY ---}
  [TestFixture] TTestAusReply = class(TZintFixture)
  public
  published
    [Test] procedure Large_8_OK;
    [Test] procedure Large_9_TooLong;
    [Test] procedure HRT_Empty;
    [Test] procedure HRT_Empty_7;
    [Test] procedure HRT_ContentSegs_7;
    [Test] procedure HRT_ContentSegs_8;
    [Test] procedure Input_8_OK;
    [Test] procedure Input_7_LeadZero_OK;
    [Test] procedure Input_9_TooLong;
    [Test] procedure Encode_Default;
    [Test] procedure Fuzz_NullA;
    [Test] procedure Fuzz_Null1;
  end;

  {--- AUSROUTE ---}
  [TestFixture] TTestAusRoute = class(TZintFixture)
  public
  published
    [Test] procedure Large_8_OK;
    [Test] procedure Large_9_TooLong;
    [Test] procedure HRT_Empty;
    [Test] procedure HRT_Empty_6;
    [Test] procedure HRT_Empty_5;
    [Test] procedure HRT_ContentSegs_6;
    [Test] procedure HRT_ContentSegs_8;
    [Test] procedure HRT_ContentSegs_5;
    [Test] procedure Input_6_OK;
    [Test] procedure Input_5_LeadZero_OK;
    [Test] procedure Input_9_TooLong;
    [Test] procedure Encode_Default;
    [Test] procedure Fuzz_NullA;
    [Test] procedure Fuzz_Null1;
  end;

  {--- AUSREDIRECT ---}
  [TestFixture] TTestAusRedirect = class(TZintFixture)
  public
  published
    [Test] procedure Large_8_OK;
    [Test] procedure Large_9_TooLong;
    [Test] procedure HRT_Empty;
    [Test] procedure HRT_Empty_4;
    [Test] procedure HRT_ContentSegs_8;
    [Test] procedure HRT_ContentSegs_4;
    [Test] procedure Input_4_OK;
    [Test] procedure Input_3_LeadZero_OK;
    [Test] procedure Input_1_LeadZero_OK;
    [Test] procedure Input_9_TooLong;
    [Test] procedure Encode_Default;
    [Test] procedure Fuzz_NullA;
    [Test] procedure Fuzz_Null1;
  end;

implementation

uses TestHelper_Zint;

function BytesToString(const ABytes: TArrayOfByte; const ALength: Integer): String;
var
  i, L: Integer;
begin
  if ALength <= 0 then
    Exit('');

  L := ALength;
  if Length(ABytes) < L then
    L := Length(ABytes);

  SetLength(Result, L);
  for i := 0 to L - 1 do
    Result[i + 1] := Char(ABytes[i]);
end;

{ ========== AUSPOST ========== }

{ test_large }

procedure TTestAusPost.Large_8_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 8));
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusPost.Large_9_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 9));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestAusPost.Large_13_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 13));
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(103, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusPost.Large_14_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 14));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestAusPost.Large_16_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 16));
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(103, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusPost.Large_17_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 17));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestAusPost.Large_18_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 18));
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(133, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusPost.Large_19_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 19));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestAusPost.Large_23_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 23));
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(133, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusPost.Large_24_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 24));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

{ test_hrt: AusPost produces no HRT }

procedure TTestAusPost.HRT_Empty_8;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    TZintTestHelper.EncodeData(sym, '12345678');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestAusPost.HRT_ContentSegs_8;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '12345678');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
    ZAssert.AreEqual(1, sym.content_segs_count, 'content_segs_count');
    ZAssert.AreEqual(10, sym.content_segs[0].Length, 'content_segs[0].Length');
    ZAssert.AreEqual('1112345678', BytesToString(sym.content_segs[0].Source, sym.content_segs[0].Length), 'content');
  finally sym.Free; end;
end;

procedure TTestAusPost.HRT_Empty_13;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    TZintTestHelper.EncodeData(sym, '1234567890123');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestAusPost.HRT_ContentSegs_13;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '1234567890123');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
    ZAssert.AreEqual(1, sym.content_segs_count, 'content_segs_count');
    ZAssert.AreEqual(15, sym.content_segs[0].Length, 'content_segs[0].Length');
    ZAssert.AreEqual('591234567890123', BytesToString(sym.content_segs[0].Source, sym.content_segs[0].Length), 'content');
  finally sym.Free; end;
end;

procedure TTestAusPost.HRT_Empty_16;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    TZintTestHelper.EncodeData(sym, '1234567890123456');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestAusPost.HRT_ContentSegs_16;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '1234567890123456');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
    ZAssert.AreEqual(1, sym.content_segs_count, 'content_segs_count');
    ZAssert.AreEqual(18, sym.content_segs[0].Length, 'content_segs[0].Length');
    ZAssert.AreEqual('591234567890123456', BytesToString(sym.content_segs[0].Source, sym.content_segs[0].Length), 'content');
  finally sym.Free; end;
end;

procedure TTestAusPost.HRT_Empty_18;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    TZintTestHelper.EncodeData(sym, '123456789012345678');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestAusPost.HRT_ContentSegs_18;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '123456789012345678');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
    ZAssert.AreEqual(1, sym.content_segs_count, 'content_segs_count');
    ZAssert.AreEqual(20, sym.content_segs[0].Length, 'content_segs[0].Length');
    ZAssert.AreEqual('62123456789012345678', BytesToString(sym.content_segs[0].Source, sym.content_segs[0].Length), 'content');
  finally sym.Free; end;
end;

procedure TTestAusPost.HRT_Empty_23;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    TZintTestHelper.EncodeData(sym, '12345678901234567890123');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestAusPost.HRT_ContentSegs_23;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '12345678901234567890123');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
    ZAssert.AreEqual(1, sym.content_segs_count, 'content_segs_count');
    ZAssert.AreEqual(25, sym.content_segs[0].Length, 'content_segs[0].Length');
    ZAssert.AreEqual('6212345678901234567890123', BytesToString(sym.content_segs[0].Source, sym.content_segs[0].Length), 'content');
  finally sym.Free; end;
end;

{ test_input }

procedure TTestAusPost.Input_8Digits_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusPost.Input_DPID_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[1]: DPID invalid char }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567A');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 405: Invalid character at position 8 in DPID (first 8 characters) (digits only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestAusPost.Input_13Mixed_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678ABcd#');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(103, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusPost.Input_13_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[3] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678ABcd!');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 404: Invalid character at position 13 in input (alphanumerics, space and "#" only)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestAusPost.Input_16Digits_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567890123456');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(103, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusPost.Input_16_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[6]: FCC 59 length 16 requires digits only }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456789012345A');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 402: Invalid character at position 16 in input (digits only for FCC 59 length 16)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestAusPost.Input_18Mixed_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678ABCDefgh #');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(133, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusPost.Input_23Digits_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678901234567890123');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(133, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusPost.Input_23_InvalidChar;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[9]: FCC 62 length 23 requires digits only }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567890123456789012A');
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
    ZAssert.AreEqual('Error 406: Invalid character at position 23 in input (digits only for FCC 62 length 23)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestAusPost.Input_WrongLength;
var sym: TZintSymbol; ret: Integer;
begin
  { test_input[10]: wrong length }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567');
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 401: Input length 7 wrong (8, 13, 16, 18 or 23 characters required)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

{ test_encode }

procedure TTestAusPost.Encode_FCC11;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[0]: AusPost Tech Specs Diagram 1 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '96184209');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
    ZAssert.AreEqual('1000101010100010001010100000101010001010001000001010000010001000001000100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000100010000010101010001010000010101010001000101010001000100010000010000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusPost.Encode_FCC11_Alt;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[1]: AusPost Guide Figure 3 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '39549554');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
    ZAssert.AreEqual('1000101010101010001010001010001010001000101000001000101010001010000000100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000100010000010001000100000001000100010000000000010001000000000001010000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusPost.Encode_FCC59_AlphaNum;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[2]: FCC 59 C encoding }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '56439111ABA 9');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(103, sym.width, 'width');
    ZAssert.AreEqual('1000100000101000001010101010001010101010101010101010101010101010100000000000001010100010101010000010100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000001000100010101000000010001010001000100010101010100010101010100000101000000010001000101010000000000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusPost.Encode_FCC59_Numeric;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[3]: FCC 59 N encoding }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '3221132412345678');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(103, sym.width, 'width');
    ZAssert.AreEqual('1000100000101010100010001010101010101000101010101000101010101000001000100000101000000000001000000000100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000001000100010101010101000100000101010000010001010001000000010101010001010000010001010101000100000000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusPost.Encode_FCC59_Alpha;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[4]: FCC 59 C encoding }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '32211324Ab #2');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(103, sym.width, 'width');
    ZAssert.AreEqual('1000100000101010100010001010101010101000101010101010001010100010100000101000100000000010100000100010100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000001000100010101010101000100000101010000010101010001010100010000000100000001000101010000010000000000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusPost.Encode_FCC62_Numeric;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[5]: FCC 62 N encoding }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '32211324123456789012345');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(133, sym.width, 'width');
    ZAssert.AreEqual('1000001010001010100010001010101010101000101010101000101010101000001000100000001010101010100010101010100000100000100010101010100010100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000101010100010101010101000100000101010000010001010001000000010101010001010001010101000101000100000001000001010000010001010100010000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusPost.Encode_FCC62_Alpha;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[6]: FCC 62 C encoding }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '32211324aBCd#F hIz');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(133, sym.width, 'width');
    ZAssert.AreEqual('1000001010001010100010001010101010101000101010000010101010100010000010100010100010100010000010000000000000100010100010101010000000100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000101010100010101010101000100000101010000010100010100010101010001010000010001010100000100010101000000000101000001010100000000010000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusPost.Encode_FCC62_GDSET1;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[7]: GDSET 1st part }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678DEGHJKLMNO');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(133, sym.width, 'width');
    ZAssert.AreEqual('1000001010001010100010101010100000100010000010101010101010001010001010101010101010100010101010101010100000001010000010000000000010100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000101010101000101000100000001010101000101010001010000010101010100000101000100000101000001000000000001000001010000010001010001010000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusPost.Encode_FCC62_GDSET2;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[8]: GDSET 2nd part }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '23456789PQRSTUVWXY');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(133, sym.width, 'width');
    ZAssert.AreEqual('1000001010001000101010101000001000100000001010001010001010000000101000101000100000101000101000100000001000101000101010101000101010100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000101010101010001000000010101010001010001000101000100000101010101010100010101010001010000010001010101000000010001000001010101000000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusPost.Encode_FCC62_GDSET3;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[9]: GDSET 3rd part }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '34567890Zcefgijklm');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(133, sym.width, 'width');
    ZAssert.AreEqual('1000001010001010101010000010001000000010101000001010001010000010100010100010001010001010000010000000100000101000100000001010001010100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000101010100010000000101010100010100010101010100010000010000000100000000000001000000000001000000010100000101000000010101010100010000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusPost.Encode_FCC62_GDSET4;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[10]: GDSET 4th part }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678lnopqrstuv');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(133, sym.width, 'width');
    ZAssert.AreEqual('1000001010001010100010101010100000100010000010000000100000000000001000001000000000000000100000100000000000001010001010101000000010100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000101010101000101000100000001010101000101000000010000010100010001010000010001010000000100000000000100000100000001010001000100000000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusPost.Encode_FCC59_GDSET5;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[11]: FCC 59 C encoding GDSET 5th part }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSPOST);
  try
    ret := TZintTestHelper.EncodeData(sym, '09876543wxy# ');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(103, sym.width, 'width');
    ZAssert.AreEqual('1000100000101010001000000010001010001010101000001000001000000010100010100000100010000000000010100010100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000001000101010001010101000101000100000001000001000000000001010000010100000001010001000001000100000000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

{ ========== AUSREPLY ========== }

procedure TTestAusReply.Large_8_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREPLY);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 8));
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusReply.Large_9_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREPLY);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 9));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestAusReply.HRT_Empty;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREPLY);
  try
    TZintTestHelper.EncodeData(sym, '12345678');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestAusReply.HRT_Empty_7;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREPLY);
  try
    TZintTestHelper.EncodeData(sym, '1234567');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestAusReply.HRT_ContentSegs_7;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREPLY);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '1234567');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
    ZAssert.AreEqual(1, sym.content_segs_count, 'content_segs_count');
    ZAssert.AreEqual(10, sym.content_segs[0].Length, 'content_segs[0].Length');
    ZAssert.AreEqual('4501234567', BytesToString(sym.content_segs[0].Source, sym.content_segs[0].Length), 'content');
  finally sym.Free; end;
end;

procedure TTestAusReply.HRT_ContentSegs_8;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREPLY);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '12345678');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
    ZAssert.AreEqual(1, sym.content_segs_count, 'content_segs_count');
    ZAssert.AreEqual(10, sym.content_segs[0].Length, 'content_segs[0].Length');
    ZAssert.AreEqual('4512345678', BytesToString(sym.content_segs[0].Source, sym.content_segs[0].Length), 'content');
  finally sym.Free; end;
end;

procedure TTestAusReply.Input_8_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREPLY);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusReply.Input_7_LeadZero_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREPLY);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234567');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusReply.Input_9_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREPLY);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 403: Input length 9 too long (maximum 8)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestAusReply.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[12] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREPLY);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345678');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
    ZAssert.AreEqual('1000101010001010100010101010100000100010000000001000001000000000100010100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000000000101000101000100000001010101000101000000000100010101000101000000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusReply.Fuzz_NullA;
var sym: TZintSymbol; ret: Integer;
    b: TArrayOfByte;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREPLY);
  try
    SetLength(b, 4);
    b[0] := Ord('A'); b[1] := 0; b[2] := 0; b[3] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 4);
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestAusReply.Fuzz_Null1;
var sym: TZintSymbol; ret: Integer;
    b: TArrayOfByte;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREPLY);
  try
    SetLength(b, 4);
    b[0] := Ord('1'); b[1] := 0; b[2] := 0; b[3] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 4);
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
  finally sym.Free; end;
end;

{ ========== AUSROUTE ========== }

procedure TTestAusRoute.Large_8_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 8));
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusRoute.Large_9_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 9));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestAusRoute.HRT_Empty;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    TZintTestHelper.EncodeData(sym, '12345678');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestAusRoute.HRT_Empty_6;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    TZintTestHelper.EncodeData(sym, '123456');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestAusRoute.HRT_Empty_5;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    TZintTestHelper.EncodeData(sym, '12345');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestAusRoute.HRT_ContentSegs_6;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '123456');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
    ZAssert.AreEqual(1, sym.content_segs_count, 'content_segs_count');
    ZAssert.AreEqual(10, sym.content_segs[0].Length, 'content_segs[0].Length');
    ZAssert.AreEqual('8700123456', BytesToString(sym.content_segs[0].Source, sym.content_segs[0].Length), 'content');
  finally sym.Free; end;
end;

procedure TTestAusRoute.HRT_ContentSegs_8;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '12345678');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
    ZAssert.AreEqual(1, sym.content_segs_count, 'content_segs_count');
    ZAssert.AreEqual(10, sym.content_segs[0].Length, 'content_segs[0].Length');
    ZAssert.AreEqual('8712345678', BytesToString(sym.content_segs[0].Source, sym.content_segs[0].Length), 'content');
  finally sym.Free; end;
end;

procedure TTestAusRoute.HRT_ContentSegs_5;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '12345');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
    ZAssert.AreEqual(1, sym.content_segs_count, 'content_segs_count');
    ZAssert.AreEqual(10, sym.content_segs[0].Length, 'content_segs[0].Length');
    ZAssert.AreEqual('8700012345', BytesToString(sym.content_segs[0].Source, sym.content_segs[0].Length), 'content');
  finally sym.Free; end;
end;

procedure TTestAusRoute.Input_6_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusRoute.Input_5_LeadZero_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    ret := TZintTestHelper.EncodeData(sym, '12345');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusRoute.Input_9_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 403: Input length 9 too long (maximum 8)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestAusRoute.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[13] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    ret := TZintTestHelper.EncodeData(sym, '34567890');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
    ZAssert.AreEqual('1000000000101010101010000010001000000010101000100010101010000000101000100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000101010000010000000101010100010100010101000100010101010001010001000000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusRoute.Fuzz_NullA;
var sym: TZintSymbol; ret: Integer;
    b: TArrayOfByte;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    SetLength(b, 4);
    b[0] := Ord('A'); b[1] := 0; b[2] := 0; b[3] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 4);
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestAusRoute.Fuzz_Null1;
var sym: TZintSymbol; ret: Integer;
    b: TArrayOfByte;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSROUTE);
  try
    SetLength(b, 4);
    b[0] := Ord('1'); b[1] := 0; b[2] := 0; b[3] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 4);
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
  finally sym.Free; end;
end;

{ ========== AUSREDIRECT ========== }

procedure TTestAusRedirect.Large_8_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 8));
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusRedirect.Large_9_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    ret := TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 9));
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestAusRedirect.HRT_Empty;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    TZintTestHelper.EncodeData(sym, '12345678');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestAusRedirect.HRT_Empty_4;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    TZintTestHelper.EncodeData(sym, '1234');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestAusRedirect.HRT_ContentSegs_8;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '12345678');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
    ZAssert.AreEqual(1, sym.content_segs_count, 'content_segs_count');
    ZAssert.AreEqual(10, sym.content_segs[0].Length, 'content_segs[0].Length');
    ZAssert.AreEqual('9212345678', BytesToString(sym.content_segs[0].Source, sym.content_segs[0].Length), 'content');
  finally sym.Free; end;
end;

procedure TTestAusRedirect.HRT_ContentSegs_4;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    sym.output_options := BARCODE_CONTENT_SEGS;
    ret := TZintTestHelper.EncodeData(sym, '1234');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
    ZAssert.AreEqual(1, sym.content_segs_count, 'content_segs_count');
    ZAssert.AreEqual(10, sym.content_segs[0].Length, 'content_segs[0].Length');
    ZAssert.AreEqual('9200001234', BytesToString(sym.content_segs[0].Source, sym.content_segs[0].Length), 'content');
  finally sym.Free; end;
end;

procedure TTestAusRedirect.Input_4_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    ret := TZintTestHelper.EncodeData(sym, '1234');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusRedirect.Input_3_LeadZero_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    ret := TZintTestHelper.EncodeData(sym, '123');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusRedirect.Input_1_LeadZero_OK;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    ret := TZintTestHelper.EncodeData(sym, '0');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
  finally sym.Free; end;
end;

procedure TTestAusRedirect.Input_9_TooLong;
var sym: TZintSymbol; ret: Integer;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    ret := TZintTestHelper.EncodeData(sym, '123456789');
    ZAssert.AreEqual(ZINT_ERROR_TOO_LONG, ret, 'ret');
    ZAssert.AreEqual('Error 403: Input length 9 too long (maximum 8)',
      TZintTestHelper.GetErrTxt(sym), 'errtxt');
  finally sym.Free; end;
end;

procedure TTestAusRedirect.Encode_Default;
var sym: TZintSymbol; ret: Integer;
begin
  { test_encode[14] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    ret := TZintTestHelper.EncodeData(sym, '98765432');
    ZAssert.AreEqual(ZINT_OK, ret, 'ret');
    ZAssert.AreEqual(3, sym.rows, 'rows');
    ZAssert.AreEqual(73, sym.width, 'width');
    ZAssert.AreEqual('1000001010000010000000100010100010101010100000101010101000100010100010100',
      TZintTestHelper.ModulesDumpRow(sym, 0), 'row0');
    ZAssert.AreEqual('1010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1), 'row1');
    ZAssert.AreEqual('0000001010100010101010001010001000000010101000000000001010101000001010000',
      TZintTestHelper.ModulesDumpRow(sym, 2), 'row2');
  finally sym.Free; end;
end;

procedure TTestAusRedirect.Fuzz_NullA;
var sym: TZintSymbol; ret: Integer;
    b: TArrayOfByte;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    SetLength(b, 4);
    b[0] := Ord('A'); b[1] := 0; b[2] := 0; b[3] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 4);
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
  finally sym.Free; end;
end;

procedure TTestAusRedirect.Fuzz_Null1;
var sym: TZintSymbol; ret: Integer;
    b: TArrayOfByte;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_AUSREDIRECT);
  try
    SetLength(b, 4);
    b[0] := Ord('1'); b[1] := 0; b[2] := 0; b[3] := 0;
    ret := TZintTestHelper.EncodeData(sym, b, 4);
    ZAssert.AreEqual(ZINT_ERROR_INVALID_DATA, ret, 'ret');
  finally sym.Free; end;
end;

initialization
  ZRegisterFixture(TTestAusPost);
  ZRegisterFixture(TTestAusReply);
  ZRegisterFixture(TTestAusRoute);
  ZRegisterFixture(TTestAusRedirect);

end.
