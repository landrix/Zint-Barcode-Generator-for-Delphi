unit Test_Postal;

{
  DUnitX-Tests fuer zint_postal.pas
  Testdaten aus test_postal.c (Zint commit b3a3c0d, 2026-03-13)
  Testet: FLAT, POSTNET, CEPNET, PLANET, FIM, RM4SCC, KIX, DAFT,
          KOREAPOST, JAPANPOST, Flattermarken
}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TestHelper_Zint,
  zint;

type
  [TestFixture]
  TTestPostal = class
  public
    { === test_large === }
    [Test] procedure Large_Flat_128_OK;
    [Test] procedure Large_Flat_129_TooLong;
    [Test] procedure Large_PostNet_11_OK;
    [Test] procedure Large_PostNet_12_Warn;
    [Test] procedure Large_PostNet_39_TooLong;
    [Test] procedure Large_CEPNet_8_OK;
    [Test] procedure Large_CEPNet_7_Warn;
    [Test] procedure Large_CEPNet_39_TooLong;
    [Test] procedure Large_FIM_1_OK;
    [Test] procedure Large_FIM_2_TooLong;
    [Test] procedure Large_RM4SCC_50_OK;
    [Test] procedure Large_RM4SCC_51_TooLong;
    [Test] procedure Large_JapanPost_20_OK;
    [Test] procedure Large_JapanPost_21_TooLong;
    [Test] procedure Large_JapanPost_A10_OK;
    [Test] procedure Large_JapanPost_A11_TooLong;
    [Test] procedure Large_KoreaPost_6_OK;
    [Test] procedure Large_KoreaPost_7_TooLong;
    [Test] procedure Large_Planet_13_OK;
    [Test] procedure Large_Planet_14_Warn;
    [Test] procedure Large_Planet_39_TooLong;
    [Test] procedure Large_KIX_18_OK;
    [Test] procedure Large_KIX_19_TooLong;
    [Test] procedure Large_DAFT_576_OK;
    [Test] procedure Large_DAFT_577_TooLong;

    { === test_input === }
    [Test] procedure Input_Flat_Digits_OK;
    [Test] procedure Input_Flat_InvalidChar;
    [Test] procedure Input_PostNet_5_OK;
    [Test] procedure Input_PostNet_9_OK;
    [Test] procedure Input_PostNet_11_OK;
    [Test] procedure Input_PostNet_1_Warn;
    [Test] procedure Input_PostNet_InvalidChar;
    [Test] procedure Input_FIM_a_OK;
    [Test] procedure Input_FIM_b_OK;
    [Test] procedure Input_FIM_c_OK;
    [Test] procedure Input_FIM_d_OK;
    [Test] procedure Input_FIM_e_OK;
    [Test] procedure Input_FIM_f_Invalid;
    [Test] procedure Input_CEPNet_InvalidChar;
    [Test] procedure Input_RM4SCC_AlphaNum_OK;
    [Test] procedure Input_RM4SCC_Lower_OK;
    [Test] procedure Input_RM4SCC_Invalid;
    [Test] procedure Input_JapanPost_AlphaNum_OK;
    [Test] procedure Input_JapanPost_Overflow;
    [Test] procedure Input_JapanPost_Lower_OK;
    [Test] procedure Input_JapanPost_Invalid;
    [Test] procedure Input_KoreaPost_OK;
    [Test] procedure Input_KoreaPost_Invalid;
    [Test] procedure Input_Planet_11_OK;
    [Test] procedure Input_Planet_13_OK;
    [Test] procedure Input_Planet_1_Warn;
    [Test] procedure Input_Planet_Invalid;
    [Test] procedure Input_KIX_AlphaNum_OK;
    [Test] procedure Input_KIX_Lower_OK;
    [Test] procedure Input_KIX_Invalid;
    [Test] procedure Input_DAFT_OK;
    [Test] procedure Input_DAFT_Lower_OK;
    [Test] procedure Input_DAFT_Invalid;

    { === test_hrt === }
    [Test] procedure HRT_KoreaPost_123456;

    { === test_encode === }
    [Test] procedure Encode_Flat_1304056;
    [Test] procedure Encode_PostNet_12345678901;
    [Test] procedure Encode_PostNet_555551237;
    [Test] procedure Encode_FIM_C;
    [Test] procedure Encode_FIM_E;
    [Test] procedure Encode_CEPNet_12345678;
    [Test] procedure Encode_CEPNet_36400000;
    [Test] procedure Encode_RM4SCC_BX11LT1A;
    [Test] procedure Encode_RM4SCC_W1J0TR01;
    [Test] procedure Encode_JapanPost_15400233_16_4_205;
    [Test] procedure Encode_JapanPost_350110622_1A308;
    [Test] procedure Encode_JapanPost_12345671_2_3;
    [Test] procedure Encode_KoreaPost_010230;
    [Test] procedure Encode_KoreaPost_923457;
    [Test] procedure Encode_Planet_4012345235636;
    [Test] procedure Encode_Planet_40123452356;
    [Test] procedure Encode_KIX_2500GG30250;
    [Test] procedure Encode_KIX_2130VA80430;
    [Test] procedure Encode_DAFT_DAFTTFADFATDTATFT;
  end;

implementation

{ ===== test_large ===== }

procedure TTestPostal.Large_Flat_128_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FLAT);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 128)));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(1152, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_Flat_129_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FLAT);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 129)));
    Assert.AreEqual('Error 494: Input length 129 too long (maximum 128)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_PostNet_11_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 11)));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(123, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_PostNet_12_Warn;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    Assert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 12)));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(133, sym.width);
    Assert.AreEqual('Warning 479: Input length 12 is not standard (should be 5, 9 or 11 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_PostNet_39_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 39)));
    Assert.AreEqual('Error 480: Input length 39 too long (maximum 38)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_CEPNet_8_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 8)));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(93, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_CEPNet_7_Warn;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    Assert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 7)));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(83, sym.width);
    Assert.AreEqual('Warning 780: Input length 7 wrong (should be 8 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_CEPNet_39_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 39)));
    Assert.AreEqual('Error 480: Input length 39 too long (maximum 38)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_FIM_1_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'D'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(17, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_FIM_2_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, 'DD'));
    Assert.AreEqual('Error 486: Input length 2 too long (maximum 1)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_RM4SCC_50_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 50)));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(411, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_RM4SCC_51_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 51)));
    Assert.AreEqual('Error 488: Input length 51 too long (maximum 50)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_JapanPost_20_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 20)));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_JapanPost_21_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 21)));
    Assert.AreEqual('Error 496: Input length 21 too long (maximum 20)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_JapanPost_A10_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('A', 10)));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_JapanPost_A11_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('A', 11)));
    Assert.AreEqual('Error 477: Input too long, requires too many symbol characters (maximum 20)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_KoreaPost_6_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 6)));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(162, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_KoreaPost_7_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 7)));
    Assert.AreEqual('Error 484: Input length 7 too long (maximum 6)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_Planet_13_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 13)));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(143, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_Planet_14_Warn;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    Assert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 14)));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(153, sym.width);
    Assert.AreEqual('Warning 478: Input length 14 is not standard (should be 11 or 13 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_Planet_39_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 39)));
    Assert.AreEqual('Error 482: Input length 39 too long (maximum 38)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_KIX_18_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 18)));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(143, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_KIX_19_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 19)));
    Assert.AreEqual('Error 490: Input length 19 too long (maximum 18)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_DAFT_576_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('D', 576)));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(1151, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_DAFT_577_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('D', 577)));
    Assert.AreEqual('Error 492: Input length 577 too long (maximum 576)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

{ ===== test_input ===== }

procedure TTestPostal.Input_Flat_Digits_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FLAT);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567890'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(90, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Flat_InvalidChar;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FLAT);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, 'A'));
    Assert.AreEqual('Error 495: Invalid character at position 1 in input (digits only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_5_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(63, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_9_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '123457689'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(103, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_11_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345768901'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(123, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_1_Warn;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    Assert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, '0'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(23, sym.width);
    Assert.AreEqual('Warning 479: Input length 1 is not standard (should be 5, 9 or 11 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_InvalidChar;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, '1234A'));
    Assert.AreEqual('Error 481: Invalid character at position 5 in input (digits only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_a_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'a'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(17, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_b_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'b'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(17, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_c_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'c'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(17, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_d_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'd'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(17, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_e_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'e'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(17, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_f_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, 'f'));
    Assert.AreEqual('Error 487: Invalid character in input ("A", "B", "C", "D" or "E" only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_CEPNet_InvalidChar;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, '1234567A'));
    Assert.AreEqual('Error 481: Invalid character at position 8 in input (digits only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_RM4SCC_AlphaNum_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(299, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_RM4SCC_Lower_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'a'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(19, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_RM4SCC_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, ','));
    Assert.AreEqual('Error 489: Invalid character at position 1 in input (alphanumerics only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_JapanPost_AlphaNum_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567890-ABCD'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_JapanPost_Overflow;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    Assert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, '1234567890-ABCDE'));
    Assert.AreEqual('Error 477: Input too long, requires too many symbol characters (maximum 20)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_JapanPost_Lower_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'a'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_JapanPost_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, ','));
    Assert.AreEqual('Error 497: Invalid character at position 1 in input (alphanumerics and "-" only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_KoreaPost_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '123456'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(167, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_KoreaPost_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, 'A'));
    Assert.AreEqual('Error 485: Invalid character at position 1 in input (digits only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Planet_11_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345678901'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(123, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Planet_13_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567890123'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(143, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Planet_1_Warn;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    Assert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, '0'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(23, sym.width);
    Assert.AreEqual('Warning 478: Input length 1 is not standard (should be 11 or 13 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Planet_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, '1234567890A'));
    Assert.AreEqual('Error 483: Invalid character at position 11 in input (digits only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_KIX_AlphaNum_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '0123456789ABCDEFGH'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(143, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_KIX_Lower_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'a'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(7, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_KIX_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, ','));
    Assert.AreEqual('Error 491: Invalid character at position 1 in input (alphanumerics only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_DAFT_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'DAFT'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(7, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_DAFT_Lower_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'a'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(1, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_DAFT_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    Assert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, 'B'));
    Assert.AreEqual('Error 493: Invalid character at position 1 in input ("D", "A", "F" and "T" only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

{ ===== test_hrt ===== }

procedure TTestPostal.HRT_KoreaPost_123456;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '123456'));
    Assert.AreEqual('1234569', TZintTestHelper.GetText(sym));
  finally sym.Free; end;
end;

{ ===== test_encode ===== }

procedure TTestPostal.Encode_Flat_1304056;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FLAT);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1304056'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(63, sym.width);
    Assert.AreEqual(
      '100000000001000000000000000000100000000000000000010000000001000',
      TZintTestHelper.ModulesDumpRow(sym, 0));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_PostNet_12345678901;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345678901'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(123, sym.width);
    Assert.AreEqual(
      '100000001010000010001000001010000010000010001000100000101000001000000010100000100010001000001010000000000000101000100000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_PostNet_555551237;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '555551237'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(103, sym.width);
    Assert.AreEqual(
      '1000100010000010001000001000100000100010000010001000000000101000001000100000101000100000001000001000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_FIM_C;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'C'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(17, sym.width);
    Assert.AreEqual('10100010001000101', TZintTestHelper.ModulesDumpRow(sym, 0));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_FIM_E;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'E'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(17, sym.width);
    Assert.AreEqual('10001000000010001', TZintTestHelper.ModulesDumpRow(sym, 0));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_CEPNet_12345678;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345678'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(93, sym.width);
    Assert.AreEqual(
      '100000001010000010001000001010000010000010001000100000101000001000000010100000100000100000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_CEPNet_36400000;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '36400000'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(93, sym.width);
    Assert.AreEqual(
      '100000101000001010000000100000101010000000101000000010100000001010000000101000000010000000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_RM4SCC_BX11LT1A;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'BX11LT1A'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(75, sym.width);
    Assert.AreEqual(
      '100010001010100000000010100000101010000010100010000000101000100010100000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    Assert.AreEqual(
      '001010000010000010001000100010001010000010101000000010001010001000000010101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_RM4SCC_W1J0TR01;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'W1J0TR01'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(75, sym.width);
    Assert.AreEqual(
      '101010000000001010100000100000101010001000100010000000101000001010101000001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    Assert.AreEqual(
      '000010100000100010001000100000101010100000100000100000101000100010001010001',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_JapanPost_15400233_16_4_205;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '15400233-16-4-205'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(133, sym.width);
    Assert.AreEqual(
      '1000101000100010101000100000100000100010001010001010001000101000001010001000101000001000100010100000100010000010000010000010001010001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    Assert.AreEqual(
      '1010101000100010100010100000100000101000101000101000001000101000100010001000100010001000101000100000100010001000001000001000100010101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_JapanPost_350110622_1A308;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '350110622-1A308'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(133, sym.width);
    Assert.AreEqual(
      '1000001010100010100000101000101000100000001010100010100010001000101000001000100000001010100000100010000010000010000010000010100010001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    Assert.AreEqual(
      '1010101000100010100000101000101000100000100010101000101000001000101000100000100000101000100000001010001000001000001000001000100010101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_JapanPost_12345671_2_3;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345671-2-3'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(133, sym.width);
    Assert.AreEqual(
      '1000101000100010001010101000100010001010101000101000001000100010001000001010000010000010000010000010000010000010000010000010100010001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    Assert.AreEqual(
      '1010101000101000101000100010100010100010001010101000001000101000001000101000001000001000001000001000001000001000001000001000100010101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_KoreaPost_010230;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '010230'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(167, sym.width);
    Assert.AreEqual(
      '10001000100000000000100010000000000010001000100000001000000010001000100010001000100000000000100000000001000100010001000100010001000000000001000000010001000000010001000',
      TZintTestHelper.ModulesDumpRow(sym, 0));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_KoreaPost_923457;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '923457'));
    Assert.AreEqual(1, sym.rows);
    Assert.AreEqual(168, sym.width);
    Assert.AreEqual(
      '000010001000100000001000100000001000000010001000000010001000000010001000100000000000100010001000000010000000100010001000100010000000100000001000100010001000000000001000',
      TZintTestHelper.ModulesDumpRow(sym, 0));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_Planet_4012345235636;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '4012345235636'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(143, sym.width);
    Assert.AreEqual(
      '10100010100000001010101010100000101000100010100000101000101000100010001010100010001010000010100010001010000010101010000010100000101010000010101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '10101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_Planet_40123452356;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '40123452356'));
    Assert.AreEqual(2, sym.rows);
    Assert.AreEqual(123, sym.width);
    Assert.AreEqual(
      '101000101000000010101010101000001010001000101000001010001010001000100010101000100010100000101000100010100000101010001000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_KIX_2500GG30250;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '2500GG30250'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(87, sym.width);
    Assert.AreEqual(
      '000010100000101000001010000010100010100000101000000010100000101000001010000010100000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    Assert.AreEqual(
      '001010001010000000001010000010101000100010001000100000100000101000101000101000000000101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_KIX_2130VA80430;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, '2130VA80430'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(87, sym.width);
    Assert.AreEqual(
      '000010100000101000001010000010101010000000100010001000100000101000001010000010100000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    Assert.AreEqual(
      '001010000010001010000010000010100010001010001000001010000000101010001000100000100000101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_DAFT_DAFTTFADFATDTATFT;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    Assert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'DAFTTFADFATDTATFT'));
    Assert.AreEqual(3, sym.rows);
    Assert.AreEqual(33, sym.width);
    Assert.AreEqual(
      '001010000010100010100000001000100',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    Assert.AreEqual(
      '101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    Assert.AreEqual(
      '100010000010001010000010000000100',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

end.
