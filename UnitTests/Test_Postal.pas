unit Test_Postal;

{$I zint_test.inc}

{
  DUnitX-Tests fuer zint_postal.pas
  Testdaten aus test_postal.c (Zint commit b3a3c0d, 2026-03-13)
  Testet: FLAT, POSTNET, CEPNET, PLANET, FIM, RM4SCC, KIX, DAFT,
          KOREAPOST, JAPANPOST, Flattermarken
}

interface

uses
  {$IFNDEF FPC}DUnitX.TestFramework,{$ENDIF}
  TestFramework_Zint,
  SysUtils,
  TestHelper_Zint,
  zint;

type
  [TestFixture]
  TTestPostal = class(TZintFixture)
  public
    { === test_large === }
  published
    [Test] procedure Large_Flat_128_OK;
    [Test] procedure Large_Flat_129_TooLong;
    [Test] procedure Large_PostNet_11_OK;
    [Test] procedure Large_PostNet_12_Warn;
    { Hoehe: test_postal.c test_input, symbol->height wird dort mitgeprueft.
      Die Assertion fehlte im Port, weil z_set_height nicht portiert war. }
    [Test] procedure Input_PostNet_Height_Default;
    [Test] procedure Input_PostNet_Height_Given;
    [Test] procedure Input_PostNet_Height_Compliant;
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
    [Test] procedure Large_PostNet_38_Warn;
    [Test] procedure Large_CEPNet_9_Warn;
    [Test] procedure Large_Planet_38_Warn;

    { === test_japanpost === }
    [Test] procedure JapanPost_123;
    [Test] procedure JapanPost_123456_AB;
    [Test] procedure JapanPost_123456;
    [Test] procedure JapanPost_999980_KZ;
    [Test] procedure JapanPost_987654_TU;

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
    [Test] procedure Input_PostNet_4_Warn;
    [Test] procedure Input_PostNet_6_Warn;
    [Test] procedure Input_PostNet_12_Warn;
    [Test] procedure Input_FIM_ad_TooLong;
    [Test] procedure Input_JapanPost_20SymChars_OK;
    [Test] procedure Input_JapanPost_Overflow2;
    [Test] procedure Input_JapanPost_NoHyphen_OK;
    [Test] procedure Input_Planet_10_Warn;
    [Test] procedure Input_Planet_12_Warn;
    [Test] procedure Input_Planet_14_Warn;

    { === test_hrt === }
    [Test] procedure HRT_KoreaPost_123456;
    [Test] procedure HRT_Flat_Empty;
    [Test] procedure HRT_PostNet_Empty;
    [Test] procedure HRT_FIM_Empty;
    [Test] procedure HRT_CEPNet_Empty;
    [Test] procedure HRT_RM4SCC_Empty;
    [Test] procedure HRT_JapanPost_Empty;
    [Test] procedure HRT_Planet_Empty;
    [Test] procedure HRT_KIX_Empty;
    [Test] procedure HRT_DAFT_Empty;

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
    [Test] procedure Encode_RM4SCC_FullAlpha;
    [Test] procedure Encode_JapanPost_1234567BCDEFG;
    [Test] procedure Encode_JapanPost_8901234HIJKLM;
    [Test] procedure Encode_JapanPost_0987654NOPQRS;
    [Test] procedure Encode_JapanPost_3210987TUVWXY;
    [Test] procedure Encode_Planet_5020140235635;
    [Test] procedure Encode_KIX_1231GF156X2;
    [Test] procedure Encode_KIX_1231FZ13Xhs;
    [Test] procedure Encode_KIX_1234567890ABCDEFGH;
    [Test] procedure Encode_KIX_IJKLMNOPQRSTUVWXYZ;
  end;

  [TestFixture]
  TTestPostalHRTContentSegsFromC = class(TZintFixture)
  public
  published
    [Test] procedure HRT_ContentSegs_FromC;
  end;

implementation

{ ===== test_large ===== }

procedure TTestPostal.Large_Flat_128_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FLAT);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 128)));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(1152, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_Flat_129_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FLAT);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 129)));
    ZAssert.AreEqual('Error 494: Input length 129 too long (maximum 128)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_PostNet_11_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 11)));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(123, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_PostNet_12_Warn;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 12)));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(133, sym.width);
    ZAssert.AreEqual('Warning 479: Input length 12 is not standard (should be 5, 9 or 11 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_Height_Default;
{ C: test_input C#2 - POSTNET "12345", ohne Hoehenvorgabe -> height 12 }
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(63, sym.width);
    ZAssert.AreEqual(Single(12), sym.height, 'height');
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_Height_Given;
{ C: test_input C#10 - POSTNET "12345" mit height 0.9 -> height 1.
  0.9 * 0.5 = 0.45 liegt unter dem absoluten Minimum 0.5, deshalb wird die
  halbe Balkenhoehe auf 0.5 gesetzt und die volle daraus zurueckgerechnet. }
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    sym.height := 0.9;
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(63, sym.width);
    ZAssert.AreEqual(Single(1), sym.height, 'height');
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_Height_Compliant;
{ Nicht aus einem C-Fall, sondern aus usps_set_height (postal.c:104) selbst:
  mit COMPLIANT_HEIGHT setzt C row_height auf 3.2249999 / 2.1500001, die
  Summe 5.375 liegt im konformen Bereich 4.6 bis 9.0, also keine Warnung.
  Ohne diesen Fall wuerde der COMPLIANT_HEIGHT-Zweig von keinem Test
  beruehrt. }
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    sym.output_options := sym.output_options or COMPLIANT_HEIGHT;
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345'));
    ZAssert.AreEqual(Single(3.2249999), sym.row_height[0], 'row_height[0]');
    ZAssert.AreEqual(Single(2.1500001), sym.row_height[1], 'row_height[1]');
    ZAssert.AreEqual(Single(5.375), sym.height, 'height');
  finally sym.Free; end;
end;

procedure TTestPostal.Large_PostNet_39_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 39)));
    ZAssert.AreEqual('Error 480: Input length 39 too long (maximum 38)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_CEPNet_8_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 8)));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(93, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_CEPNet_7_Warn;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 7)));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(83, sym.width);
    ZAssert.AreEqual('Warning 780: Input length 7 wrong (should be 8 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_CEPNet_39_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 39)));
    ZAssert.AreEqual('Error 480: Input length 39 too long (maximum 38)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_FIM_1_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'D'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(17, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_FIM_2_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, 'DD'));
    ZAssert.AreEqual('Error 486: Input length 2 too long (maximum 1)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_RM4SCC_50_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 50)));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(411, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_RM4SCC_51_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 51)));
    ZAssert.AreEqual('Error 488: Input length 51 too long (maximum 50)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_JapanPost_20_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 20)));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_JapanPost_21_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 21)));
    ZAssert.AreEqual('Error 496: Input length 21 too long (maximum 20)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_JapanPost_A10_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('A', 10)));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_JapanPost_A11_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('A', 11)));
    ZAssert.AreEqual('Error 477: Input too long, requires too many symbol characters (maximum 20)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_KoreaPost_6_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 6)));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(162, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_KoreaPost_7_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 7)));
    ZAssert.AreEqual('Error 484: Input length 7 too long (maximum 6)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_Planet_13_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 13)));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(143, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_Planet_14_Warn;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 14)));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(153, sym.width);
    ZAssert.AreEqual('Warning 478: Input length 14 is not standard (should be 11 or 13 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_Planet_39_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 39)));
    ZAssert.AreEqual('Error 482: Input length 39 too long (maximum 38)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_KIX_18_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 18)));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(143, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_KIX_19_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 19)));
    ZAssert.AreEqual('Error 490: Input length 19 too long (maximum 18)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_DAFT_576_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('D', 576)));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(1151, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_DAFT_577_TooLong;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('D', 577)));
    ZAssert.AreEqual('Error 492: Input length 577 too long (maximum 576)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

{ ===== test_input ===== }

procedure TTestPostal.Input_Flat_Digits_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FLAT);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567890'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(90, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Flat_InvalidChar;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FLAT);
  try
    ZAssert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, 'A'));
    ZAssert.AreEqual('Error 495: Invalid character at position 1 in input (digits only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_5_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(63, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_9_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '123457689'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(103, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_11_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345768901'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(123, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_1_Warn;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, '0'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(23, sym.width);
    ZAssert.AreEqual('Warning 479: Input length 1 is not standard (should be 5, 9 or 11 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_InvalidChar;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, '1234A'));
    ZAssert.AreEqual('Error 481: Invalid character at position 5 in input (digits only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_a_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'a'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(17, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_b_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'b'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(17, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_c_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'c'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(17, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_d_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'd'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(17, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_e_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'e'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(17, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_f_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    ZAssert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, 'f'));
    ZAssert.AreEqual('Error 487: Invalid character in input ("A", "B", "C", "D" or "E" only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_CEPNet_InvalidChar;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    ZAssert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, '1234567A'));
    ZAssert.AreEqual('Error 481: Invalid character at position 8 in input (digits only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_RM4SCC_AlphaNum_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(299, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_RM4SCC_Lower_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'a'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(19, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_RM4SCC_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    ZAssert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, ','));
    ZAssert.AreEqual('Error 489: Invalid character at position 1 in input (alphanumerics only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_JapanPost_AlphaNum_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567890-ABCD'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_JapanPost_Overflow;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, '1234567890-ABCDE'));
    ZAssert.AreEqual('Error 477: Input too long, requires too many symbol characters (maximum 20)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_JapanPost_Lower_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'a'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_JapanPost_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, ','));
    ZAssert.AreEqual('Error 497: Invalid character at position 1 in input (alphanumerics and "-" only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_KoreaPost_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '123456'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(167, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_KoreaPost_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    ZAssert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, 'A'));
    ZAssert.AreEqual('Error 485: Invalid character at position 1 in input (digits only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Planet_11_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345678901'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(123, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Planet_13_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567890123'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(143, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Planet_1_Warn;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, '0'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(23, sym.width);
    ZAssert.AreEqual('Warning 478: Input length 1 is not standard (should be 11 or 13 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Planet_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, '1234567890A'));
    ZAssert.AreEqual('Error 483: Invalid character at position 11 in input (digits only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_KIX_AlphaNum_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '0123456789ABCDEFGH'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(143, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_KIX_Lower_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'a'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(7, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_KIX_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    ZAssert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, ','));
    ZAssert.AreEqual('Error 491: Invalid character at position 1 in input (alphanumerics only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_DAFT_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'DAFT'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(7, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_DAFT_Lower_OK;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'a'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(1, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_DAFT_Invalid;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    ZAssert.AreEqual(ZERROR_INVALID_DATA, TZintTestHelper.EncodeData(sym, 'B'));
    ZAssert.AreEqual('Error 493: Invalid character at position 1 in input ("D", "A", "F" and "T" only)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

{ ===== test_hrt ===== }

procedure TTestPostal.HRT_KoreaPost_123456;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '123456'));
    ZAssert.AreEqual('1234569', TZintTestHelper.GetText(sym));
  finally sym.Free; end;
end;

{ ===== test_encode ===== }

procedure TTestPostal.Encode_Flat_1304056;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FLAT);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1304056'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(63, sym.width);
    ZAssert.AreEqual(
      '100000000001000000000000000000100000000000000000010000000001000',
      TZintTestHelper.ModulesDumpRow(sym, 0));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_PostNet_12345678901;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345678901'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(123, sym.width);
    ZAssert.AreEqual(
      '100000001010000010001000001010000010000010001000100000101000001000000010100000100010001000001010000000000000101000100000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_PostNet_555551237;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '555551237'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(103, sym.width);
    ZAssert.AreEqual(
      '1000100010000010001000001000100000100010000010001000000000101000001000100000101000100000001000001000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_FIM_C;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'C'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(17, sym.width);
    ZAssert.AreEqual('10100010001000101', TZintTestHelper.ModulesDumpRow(sym, 0));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_FIM_E;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'E'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(17, sym.width);
    ZAssert.AreEqual('10001000000010001', TZintTestHelper.ModulesDumpRow(sym, 0));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_CEPNet_12345678;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345678'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(93, sym.width);
    ZAssert.AreEqual(
      '100000001010000010001000001010000010000010001000100000101000001000000010100000100000100000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_CEPNet_36400000;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '36400000'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(93, sym.width);
    ZAssert.AreEqual(
      '100000101000001010000000100000101010000000101000000010100000001010000000101000000010000000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_RM4SCC_BX11LT1A;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'BX11LT1A'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(75, sym.width);
    ZAssert.AreEqual(
      '100010001010100000000010100000101010000010100010000000101000100010100000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '001010000010000010001000100010001010000010101000000010001010001000000010101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_RM4SCC_W1J0TR01;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'W1J0TR01'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(75, sym.width);
    ZAssert.AreEqual(
      '101010000000001010100000100000101010001000100010000000101000001010101000001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '000010100000100010001000100000101010100000100000100000101000100010001010001',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_JapanPost_15400233_16_4_205;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '15400233-16-4-205'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
    ZAssert.AreEqual(
      '1000101000100010101000100000100000100010001010001010001000101000001010001000101000001000100010100000100010000010000010000010001010001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '1010101000100010100010100000100000101000101000101000001000101000100010001000100010001000101000100000100010001000001000001000100010101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_JapanPost_350110622_1A308;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '350110622-1A308'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
    ZAssert.AreEqual(
      '1000001010100010100000101000101000100000001010100010100010001000101000001000100000001010100000100010000010000010000010000010100010001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '1010101000100010100000101000101000100000100010101000101000001000101000100000100000101000100000001010001000001000001000001000100010101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_JapanPost_12345671_2_3;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345671-2-3'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
    ZAssert.AreEqual(
      '1000101000100010001010101000100010001010101000101000001000100010001000001010000010000010000010000010000010000010000010000010100010001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '1010101000101000101000100010100010100010001010101000001000101000001000101000001000001000001000001000001000001000001000001000100010101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_KoreaPost_010230;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '010230'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(167, sym.width);
    ZAssert.AreEqual(
      '10001000100000000000100010000000000010001000100000001000000010001000100010001000100000000000100000000001000100010001000100010001000000000001000000010001000000010001000',
      TZintTestHelper.ModulesDumpRow(sym, 0));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_KoreaPost_923457;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KOREAPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '923457'));
    ZAssert.AreEqual(1, sym.rows);
    ZAssert.AreEqual(168, sym.width);
    ZAssert.AreEqual(
      '000010001000100000001000100000001000000010001000000010001000000010001000100000000000100010001000000010000000100010001000100010000000100000001000100010001000000000001000',
      TZintTestHelper.ModulesDumpRow(sym, 0));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_Planet_4012345235636;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '4012345235636'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(143, sym.width);
    ZAssert.AreEqual(
      '10100010100000001010101010100000101000100010100000101000101000100010001010100010001010000010100010001010000010101010000010100000101010000010101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '10101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_Planet_40123452356;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '40123452356'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(123, sym.width);
    ZAssert.AreEqual(
      '101000101000000010101010101000001010001000101000001010001010001000100010101000100010100000101000100010100000101010001000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_KIX_2500GG30250;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '2500GG30250'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(87, sym.width);
    ZAssert.AreEqual(
      '000010100000101000001010000010100010100000101000000010100000101000001010000010100000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '001010001010000000001010000010101000100010001000100000100000101000101000101000000000101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_KIX_2130VA80430;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '2130VA80430'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(87, sym.width);
    ZAssert.AreEqual(
      '000010100000101000001010000010101010000000100010001000100000101000001010000010100000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '001010000010001010000010000010100010001010001000001010000000101010001000100000100000101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_DAFT_DAFTTFADFATDTATFT;
var sym: TZintSymbol;
begin
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'DAFTTFADFATDTATFT'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(33, sym.width);
    ZAssert.AreEqual(
      '001010000010100010100000001000100',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '100010000010001010000010000000100',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

{ ===== additional test_large ===== }

procedure TTestPostal.Large_PostNet_38_Warn;
var sym: TZintSymbol;
begin
  { test_large[4] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 38)));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(393, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Large_CEPNet_9_Warn;
var sym: TZintSymbol;
begin
  { test_large[10] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 9)));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(103, sym.width);
    ZAssert.AreEqual('Warning 780: Input length 9 wrong (should be 8 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Large_Planet_38_Warn;
var sym: TZintSymbol;
begin
  { test_large[22] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, TZintTestHelper.StrRepeat('1', 38)));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(393, sym.width);
  finally sym.Free; end;
end;

{ ===== test_japanpost ===== }

procedure TTestPostal.JapanPost_123;
var sym: TZintSymbol;
begin
  { test_japanpost[0]: check 3 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '123'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.JapanPost_123456_AB;
var sym: TZintSymbol;
begin
  { test_japanpost[1]: check 10 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '123456-AB'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.JapanPost_123456;
var sym: TZintSymbol;
begin
  { test_japanpost[2]: check 11 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '123456'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.JapanPost_999980_KZ;
var sym: TZintSymbol;
begin
  { test_japanpost[3]: check 18 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '999980-KZ'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.JapanPost_987654_TU;
var sym: TZintSymbol;
begin
  { test_japanpost[4]: check 0 }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '987654-TU'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

{ ===== additional test_input ===== }

procedure TTestPostal.Input_PostNet_4_Warn;
var sym: TZintSymbol;
begin
  { test_input[6] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, '1234'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(53, sym.width);
    ZAssert.AreEqual('Warning 479: Input length 4 is not standard (should be 5, 9 or 11 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_6_Warn;
var sym: TZintSymbol;
begin
  { test_input[7] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, '123456'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(73, sym.width);
    ZAssert.AreEqual('Warning 479: Input length 6 is not standard (should be 5, 9 or 11 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_PostNet_12_Warn;
var sym: TZintSymbol;
begin
  { test_input[8] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, '123456789012'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(133, sym.width);
    ZAssert.AreEqual('Warning 479: Input length 12 is not standard (should be 5, 9 or 11 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_FIM_ad_TooLong;
var sym: TZintSymbol;
begin
  { test_input[15] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, 'ad'));
    ZAssert.AreEqual('Error 486: Input length 2 too long (maximum 1)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_JapanPost_20SymChars_OK;
var sym: TZintSymbol;
begin
  { test_input[23]: 20 symbol chars }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567890-ABCD1'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_JapanPost_Overflow2;
var sym: TZintSymbol;
begin
  { test_input[25]: 21 symbol chars with extra digit }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(ZERROR_TOO_LONG, TZintTestHelper.EncodeData(sym, '1234567890-ABCD12'));
    ZAssert.AreEqual('Error 477: Input too long, requires too many symbol characters (maximum 20)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_JapanPost_NoHyphen_OK;
var sym: TZintSymbol;
begin
  { test_input[26]: 20 symbol chars without hyphen }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567890ABCDE'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Planet_10_Warn;
var sym: TZintSymbol;
begin
  { test_input[34] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, '1234567890'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(113, sym.width);
    ZAssert.AreEqual('Warning 478: Input length 10 is not standard (should be 11 or 13 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Planet_12_Warn;
var sym: TZintSymbol;
begin
  { test_input[35] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, '123456789012'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(133, sym.width);
    ZAssert.AreEqual('Warning 478: Input length 12 is not standard (should be 11 or 13 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

procedure TTestPostal.Input_Planet_14_Warn;
var sym: TZintSymbol;
begin
  { test_input[36] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(ZWARN_NONCOMPLIANT, TZintTestHelper.EncodeData(sym, '12345678901234'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(153, sym.width);
    ZAssert.AreEqual('Warning 478: Input length 14 is not standard (should be 11 or 13 digits)', TZintTestHelper.GetErrTxt(sym));
  finally sym.Free; end;
end;

{ ===== additional test_hrt ===== }

procedure TTestPostal.HRT_Flat_Empty;
var sym: TZintSymbol;
begin
  { test_hrt[0]: FLAT has no HRT }
  sym := TZintTestHelper.CreateSymbol(BARCODE_FLAT);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345'));
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPostal.HRT_PostNet_Empty;
var sym: TZintSymbol;
begin
  { test_hrt[2] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_POSTNET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345'));
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPostal.HRT_FIM_Empty;
var sym: TZintSymbol;
begin
  { test_hrt[4] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_FIM);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'e'));
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPostal.HRT_CEPNet_Empty;
var sym: TZintSymbol;
begin
  { test_hrt[6] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_CEPNET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345678'));
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPostal.HRT_RM4SCC_Empty;
var sym: TZintSymbol;
begin
  { test_hrt[8] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'BX11LT1A'));
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPostal.HRT_JapanPost_Empty;
var sym: TZintSymbol;
begin
  { test_hrt[10] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234'));
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPostal.HRT_Planet_Empty;
var sym: TZintSymbol;
begin
  { test_hrt[15] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '12345678901'));
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPostal.HRT_KIX_Empty;
var sym: TZintSymbol;
begin
  { test_hrt[17] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '0123456789ABCDEFGH'));
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPostal.HRT_DAFT_Empty;
var sym: TZintSymbol;
begin
  { test_hrt[19] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_DAFT);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'DAFT'));
    ZAssert.AreEqual('', TZintTestHelper.GetText(sym), 'text');
  finally sym.Free; end;
end;

procedure TTestPostalHRTContentSegsFromC.HRT_ContentSegs_FromC;
type
  TItem = record
    Index: Integer;
    Symbology: Integer;
    Data: String;
    ExpectedText: String;
    ExpectedContent: String;
  end;
const
  Items: array[0..10] of TItem = (
    (Index: 1;  Symbology: BARCODE_FLAT;       Data: '12345';               ExpectedText: '';        ExpectedContent: '12345'),
    (Index: 3;  Symbology: BARCODE_POSTNET;    Data: '12345';               ExpectedText: '';        ExpectedContent: '123455'),
    (Index: 5;  Symbology: BARCODE_FIM;        Data: 'e';                   ExpectedText: '';        ExpectedContent: 'E'),
    (Index: 7;  Symbology: BARCODE_CEPNET;     Data: '12345678';            ExpectedText: '';        ExpectedContent: '123456784'),
    (Index: 9;  Symbology: BARCODE_RM4SCC;     Data: 'BX11LT1A';            ExpectedText: '';        ExpectedContent: 'BX11LT1AI'),
    (Index: 11; Symbology: BARCODE_JAPANPOST;  Data: '1234';               ExpectedText: '';        ExpectedContent: '1234'),
    (Index: 12; Symbology: BARCODE_JAPANPOST;  Data: '123456-AB';          ExpectedText: '';        ExpectedContent: '123456-AB'),
    (Index: 14; Symbology: BARCODE_KOREAPOST;  Data: '123456';             ExpectedText: '1234569'; ExpectedContent: '1234569'),
    (Index: 16; Symbology: BARCODE_PLANET;     Data: '12345678901';        ExpectedText: '';        ExpectedContent: '123456789014'),
    (Index: 18; Symbology: BARCODE_KIX;        Data: '0123456789ABCDEFGH'; ExpectedText: '';        ExpectedContent: '0123456789ABCDEFGH'),
    (Index: 20; Symbology: BARCODE_DAFT;       Data: 'DAFT';               ExpectedText: '';        ExpectedContent: 'DAFT')
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
      ret := TZintTestHelper.EncodeData(sym, Items[i].Data);
      ZAssert.AreEqual(ZINT_OK, ret, Format('C#%d ret', [Items[i].Index]));
      ZAssert.AreEqual(Items[i].ExpectedText, TZintTestHelper.GetText(sym), Format('C#%d text', [Items[i].Index]));

      expected_content_len := Length(Items[i].ExpectedContent);
      ZAssert.AreEqual(1, Integer(sym.content_segs_count), Format('C#%d content_segs_count', [Items[i].Index]));
      ZAssert.AreEqual(expected_content_len, Integer(sym.content_segs[0].Length), Format('C#%d content length', [Items[i].Index]));
      for j := 1 to expected_content_len do
        ZAssert.AreEqual(Ord(Items[i].ExpectedContent[j]), Integer(sym.content_segs[0].Source[j - 1]),
          Format('C#%d content[%d]', [Items[i].Index, j - 1]));
    finally
      sym.Free;
    end;
  end;
end;

{ ===== additional test_encode ===== }

procedure TTestPostal.Encode_RM4SCC_FullAlpha;
var sym: TZintSymbol;
begin
  { test_encode[9] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_RM4SCC);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(299, sym.width);
    ZAssert.AreEqual(
      '10000010100000101000001010000010100000101000001010001000100010001000100010001000100010001000100010001010000010100000101000001010000010100000101000100000101000001010000010100000101000001010000010100010001000100010001000100010001000100010001000101000001010000010100000101000001010000010100000101000001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '10101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '00000010100010001000101000100000101000100010100000000010100010001000101000100000101000100010100000000010100010001000101000100000101000100010100000000010100010001000101000100000101000100010100000000010100010001000101000100000101000100010100000000010100010001000101000100000101000100010100000101000001',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_JapanPost_1234567BCDEFG;
var sym: TZintSymbol;
begin
  { test_encode[13] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567BCDEFG'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
    ZAssert.AreEqual(
      '1000101000100010001010101000100010001010101000001000101000001000100010001000001010001000101000001000100010001000001010000010101000001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '1010101000101000101000100010100010100010001010100000101000100000101000100000101000100000100010100000100010100000100010001000100010101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_JapanPost_8901234HIJKLM;
var sym: TZintSymbol;
begin
  { test_encode[14] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '8901234HIJKLM'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
    ZAssert.AreEqual(
      '1000100010001010100000101000100010001010101000001000101000001000100010001000001010000010100000000010101000000010100010000010100000001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '1010001010001010100000101000101000101000100010100000001010100000001010100000001010100000100000100000101000100000101000001000000010101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_JapanPost_0987654NOPQRS;
var sym: TZintSymbol;
begin
  { test_encode[15] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '0987654NOPQRS'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
    ZAssert.AreEqual(
      '1000100000001010100010101000001010100010101000000010001010000010101000000010100010000010001010000010101000000010100010000010100000001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '1010100000001010001010001010100010100010100010100000101000100000100010100000100010100000100010100000001010100000001010001000001000101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_JapanPost_3210987TUVWXY;
var sym: TZintSymbol;
begin
  { test_encode[16] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_JAPANPOST);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '3210987TUVWXY'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(133, sym.width);
    ZAssert.AreEqual(
      '1000001010100010101000100000001010100010101000000010001010100000100000100000101000100000100010100000001010100000101000000010000010001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '1010101000101000101000100000001010001010001010100000001010001000100000001000101000001000101000001000101000001000100010001000100000101',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_Planet_5020140235635;
var sym: TZintSymbol;
begin
  { test_encode[21] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_PLANET);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '5020140235635'));
    ZAssert.AreEqual(2, sym.rows);
    ZAssert.AreEqual(143, sym.width);
    ZAssert.AreEqual(
      '10100010001000001010101010001000000010101010101000001000101000000010101010100010001010000010100010001010000010101010000010100010001010001010001',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '10101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_KIX_1231GF156X2;
var sym: TZintSymbol;
begin
  { test_encode[24] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1231GF156X2'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(87, sym.width);
    ZAssert.AreEqual(
      '000010100000101000001010000010100010100000101000000010100000101000100010101000000000101',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '001000100010100010000010001000101000100010000010001000101010000000001010100000100010100',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_KIX_1231FZ13Xhs;
var sym: TZintSymbol;
begin
  { test_encode[25] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1231FZ13Xhs'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(87, sym.width);
    ZAssert.AreEqual(
      '000010100000101000001010000010100010100010100000000010100000101010100000001010001000100',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '001000100010100010000010001000101000001010100000001000101000001010000010101000001000100',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_KIX_1234567890ABCDEFGH;
var sym: TZintSymbol;
begin
  { test_encode[26] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, '1234567890ABCDEFGH'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(143, sym.width);
    ZAssert.AreEqual(
      '00001010000010100000101000001010000010100010001000100010001000100010001000001010001000100010001000101000001010000010100000101000001010000010100',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '10101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '00100010001010001000001010001000101000000000101000100010001010001000001000001010100010001010000000001010001000100010100010000010100010001010000',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

procedure TTestPostal.Encode_KIX_IJKLMNOPQRSTUVWXYZ;
var sym: TZintSymbol;
begin
  { test_encode[27] }
  sym := TZintTestHelper.CreateSymbol(BARCODE_KIX);
  try
    ZAssert.AreEqual(0, TZintTestHelper.EncodeData(sym, 'IJKLMNOPQRSTUVWXYZ'));
    ZAssert.AreEqual(3, sym.rows);
    ZAssert.AreEqual(143, sym.width);
    ZAssert.AreEqual(
      '10000010100000101000001010000010100000101000001010001000100010001000100010001000100010001000100010100000101000001010000010100000101000001010000',
      TZintTestHelper.ModulesDumpRow(sym, 0));
    ZAssert.AreEqual(
      '10101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101',
      TZintTestHelper.ModulesDumpRow(sym, 1));
    ZAssert.AreEqual(
      '00001010001000100010100010000010100010001010000000001010001000100010100010000010100010001010000000001010001000100010100010000010100010001010000',
      TZintTestHelper.ModulesDumpRow(sym, 2));
  finally sym.Free; end;
end;

initialization
  ZRegisterFixture(TTestPostal);
  ZRegisterFixture(TTestPostalHRTContentSegsFromC);
end.
