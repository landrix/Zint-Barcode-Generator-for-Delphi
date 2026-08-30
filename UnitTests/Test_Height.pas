unit Test_Height;

{$I zint_test.inc}

{
  DUnitX-Tests fuer die Hoehenlogik (z_set_height und die modulspezifischen
  Varianten).

  Testdaten aus test_height und test_height_per_row in test_vector.c
  (Zint commit b3a3c0d, 2026-03-13). Das ist upstream die einzige Stelle,
  an der die Hoehenlogik je Symbologie geprueft wird - die Modul-Testdateien
  tun es fast gar nicht.

  Uebernommen sind symbology, output_options, input_mode, height, data, ret,
  height, rows und width. Die beiden Vektor-Spalten der C-Tabellen bleiben
  weg, weil der Port keinen Vektor-Renderer prueft; show_hrt setzt C auf 0,
  der Port kennt das Feld nicht und auf die Hoehe wirkt es nicht.

  ERZEUGT von scripts/gen-height-tests.ps1 - Aenderungen von Hand gehen beim
  naechsten Lauf verloren.
}

interface

uses
  {$IFNDEF FPC}DUnitX.TestFramework,{$ENDIF}
  TestFramework_Zint,
  SysUtils,
  TestHelper_Zint,
  zint_helper,
  zint;

type
  [TestFixture]
  TTestHeight = class(TZintFixture)
  private
    procedure CheckCase(ACIndex, ASymbology, AOutputOptions, AInputMode: Integer;
      const AHeight: Single; const AData: String; ARet: Integer;
      const AExpHeight: Single; AExpRows, AExpWidth: Integer);
  published
    [Test] procedure Height_C25INTER;
    [Test] procedure Height_CODE39;
    [Test] procedure Height_EXCODE39;
    [Test] procedure Height_EAN128;
    [Test] procedure Height_DPLEIT;
    [Test] procedure Height_DPIDENT;
    [Test] procedure Height_CODE93;
    [Test] procedure Height_TELEPEN;
    [Test] procedure Height_POSTNET;
    [Test] procedure Height_FIM;
    [Test] procedure Height_LOGMARS;
    [Test] procedure Height_PHARMA;
    [Test] procedure Height_PZN;
    [Test] procedure Height_PHARMA_TWO;
    [Test] procedure Height_CEPNET;
    [Test] procedure Height_AUSPOST;
    [Test] procedure Height_AUSREPLY;
    [Test] procedure Height_AUSROUTE;
    [Test] procedure Height_AUSREDIRECT;
    [Test] procedure Height_RM4SCC;
    [Test] procedure Height_EAN14;
    [Test] procedure Height_NVE18;
    [Test] procedure Height_JAPANPOST;
    [Test] procedure Height_PLANET;
    [Test] procedure Height_TELEPEN_NUM;
    [Test] procedure Height_ITF14;
    [Test] procedure Height_KIX;
    [Test] procedure Height_DAFT;
    [Test] procedure Height_HIBC_39;
    [Test] procedure Height_CODE32;

    { test_height_per_row: HEIGHTPERROW_MODE }
    [Test] procedure HeightPerRow_PHARMA_TWO;
  end;

implementation

procedure TTestHeight.CheckCase(ACIndex, ASymbology, AOutputOptions, AInputMode: Integer;
  const AHeight: Single; const AData: String; ARet: Integer;
  const AExpHeight: Single; AExpRows, AExpWidth: Integer);
var
  sym: TZintSymbol;
  ctx: String;
begin
  ctx := Format('C#%d', [ACIndex]);
  sym := TZintTestHelper.CreateSymbol(ASymbology);
  try
    sym.input_mode := UNICODE_MODE;
    if AInputMode >= 0 then
      sym.input_mode := sym.input_mode or AInputMode;
    if AOutputOptions >= 0 then
      sym.output_options := AOutputOptions;
    if AHeight >= 0 then
      sym.height := AHeight;
    ZAssert.AreEqual(ARet, TZintTestHelper.EncodeData(sym, AData),
      ctx + ' ret (' + TZintTestHelper.GetErrTxt(sym) + ')');
    ZAssert.AreEqual(AExpHeight, sym.height, ctx + ' height');
    ZAssert.AreEqual(AExpRows, sym.rows, ctx + ' rows');
    ZAssert.AreEqual(AExpWidth, sym.width, ctx + ' width');
  finally
    sym.Free;
  end;
end;
procedure TTestHeight.Height_C25INTER;
begin
  CheckCase(8, BARCODE_C25INTER, -1, -1, 1.0, '1234567890', 0, 1.0, 1, 99);
  CheckCase(9, BARCODE_C25INTER, -1, -1, 15.0, '1234567890', 0, 15.0, 1, 99);
  CheckCase(10, BARCODE_C25INTER, COMPLIANT_HEIGHT, -1, 15.0, '1234567890', ZWARN_NONCOMPLIANT, 15.0, 1, 99);
  CheckCase(11, BARCODE_C25INTER, COMPLIANT_HEIGHT, -1, 15.5, '1234567890', 0, 15.5, 1, 99);
  CheckCase(12, BARCODE_C25INTER, COMPLIANT_HEIGHT, -1, 17.5, '12345678901', ZWARN_NONCOMPLIANT, 17.5, 1, 117);
  CheckCase(13, BARCODE_C25INTER, COMPLIANT_HEIGHT, -1, 17.75, '12345678901', 0, 17.75, 1, 117);
end;

procedure TTestHeight.Height_CODE39;
begin
  CheckCase(23, BARCODE_CODE39, -1, -1, 1.0, '1234567890', 0, 1.0, 1, 155);
  CheckCase(24, BARCODE_CODE39, -1, -1, 4.0, '1', 0, 4.0, 1, 38);
  CheckCase(25, BARCODE_CODE39, COMPLIANT_HEIGHT, -1, 4.0, '1', ZWARN_NONCOMPLIANT, 4.0, 1, 38);
  CheckCase(26, BARCODE_CODE39, COMPLIANT_HEIGHT, -1, 4.4, '1', 0, 4.4000001, 1, 38);
  CheckCase(27, BARCODE_CODE39, -1, -1, 17.0, '1234567890', 0, 17.0, 1, 155);
  CheckCase(28, BARCODE_CODE39, COMPLIANT_HEIGHT, -1, 17.0, '1234567890', ZWARN_NONCOMPLIANT, 17.0, 1, 155);
  CheckCase(29, BARCODE_CODE39, COMPLIANT_HEIGHT, -1, 17.85, '1234567890', 0, 17.85, 1, 155);
end;

procedure TTestHeight.Height_EXCODE39;
begin
  CheckCase(30, BARCODE_EXCODE39, -1, -1, 1.0, '1234567890', 0, 1.0, 1, 155);
  CheckCase(31, BARCODE_EXCODE39, -1, -1, 17.8, '1234567890', 0, 17.799999, 1, 155);
  CheckCase(32, BARCODE_EXCODE39, COMPLIANT_HEIGHT, -1, 17.8, '1234567890', ZWARN_NONCOMPLIANT, 17.799999, 1, 155);
  CheckCase(33, BARCODE_EXCODE39, COMPLIANT_HEIGHT, -1, 17.9, '1234567890', 0, 17.9, 1, 155);
end;

procedure TTestHeight.Height_EAN128;
begin
  CheckCase(52, BARCODE_EAN128, -1, -1, 1.0, '[01]12345678901231', 0, 1.0, 1, 134);
  CheckCase(53, BARCODE_EAN128, -1, -1, 5.7, '[01]12345678901231', 0, 5.6999998, 1, 134);
  CheckCase(54, BARCODE_EAN128, COMPLIANT_HEIGHT, -1, 5.7, '[01]12345678901231', ZWARN_NONCOMPLIANT, 5.6999998, 1, 134);
  CheckCase(55, BARCODE_EAN128, COMPLIANT_HEIGHT, -1, 5.725, '[01]12345678901231', 0, 5.7249999, 1, 134);
  CheckCase(56, BARCODE_EAN128, -1, -1, 50.0, '[01]12345678901231', 0, 50.0, 1, 134);
end;

procedure TTestHeight.Height_DPLEIT;
begin
  CheckCase(66, BARCODE_DPLEIT, -1, -1, 1.0, '1234567890123', 0, 1.0, 1, 135);
  CheckCase(67, BARCODE_DPLEIT, COMPLIANT_HEIGHT, -1, 1.0, '1234567890123', 0, 1.0, 1, 135);
  CheckCase(68, BARCODE_DPLEIT, -1, -1, 4.0, '1234567890123', 0, 4.0, 1, 135);
end;

procedure TTestHeight.Height_DPIDENT;
begin
  CheckCase(69, BARCODE_DPIDENT, -1, -1, 1.0, '12345678901', 0, 1.0, 1, 117);
  CheckCase(70, BARCODE_DPIDENT, COMPLIANT_HEIGHT, -1, 1.0, '12345678901', 0, 1.0, 1, 117);
  CheckCase(71, BARCODE_DPIDENT, -1, -1, 4.0, '12345678901', 0, 4.0, 1, 117);
end;

procedure TTestHeight.Height_CODE93;
begin
  CheckCase(91, BARCODE_CODE93, -1, -1, 1.0, '1234567890', 0, 1.0, 1, 127);
  CheckCase(92, BARCODE_CODE93, -1, -1, 9.9, '1', 0, 9.8999996, 1, 46);
  CheckCase(93, BARCODE_CODE93, COMPLIANT_HEIGHT, -1, 9.89, '1', ZWARN_NONCOMPLIANT, 9.89000034, 1, 46);
  CheckCase(94, BARCODE_CODE93, COMPLIANT_HEIGHT, -1, 10.0, '1', 0, 10.0, 1, 46);
  CheckCase(95, BARCODE_CODE93, COMPLIANT_HEIGHT, -1, 22.0, '1234567890', ZWARN_NONCOMPLIANT, 22.0, 1, 127);
  CheckCase(96, BARCODE_CODE93, COMPLIANT_HEIGHT, -1, 22.1, '1234567890', 0, 22.1, 1, 127);
end;

procedure TTestHeight.Height_TELEPEN;
begin
  CheckCase(112, BARCODE_TELEPEN, -1, -1, 1.0, '1234567890', 0, 1.0, 1, 208);
  CheckCase(113, BARCODE_TELEPEN, COMPLIANT_HEIGHT, -1, 1.0, '1234567890', 0, 1.0, 1, 208);
  CheckCase(114, BARCODE_TELEPEN, -1, -1, 4.0, '1234567890', 0, 4.0, 1, 208);
end;

procedure TTestHeight.Height_POSTNET;
begin
  CheckCase(129, BARCODE_POSTNET, -1, -1, -1.0, '12345678901', 0, 12.0, 2, 123);
  CheckCase(130, BARCODE_POSTNET, -1, -1, 1.0, '12345678901', 0, 1.0, 2, 123);
  CheckCase(131, BARCODE_POSTNET, -1, -1, 4.5, '12345678901', 0, 4.5, 2, 123);
  CheckCase(132, BARCODE_POSTNET, COMPLIANT_HEIGHT, -1, 4.5, '12345678901', ZWARN_NONCOMPLIANT, 4.5, 2, 123);
  CheckCase(133, BARCODE_POSTNET, COMPLIANT_HEIGHT, -1, 4.6, '12345678901', 0, 4.5999999, 2, 123);
  CheckCase(134, BARCODE_POSTNET, -1, -1, 9.0, '12345678901', 0, 9.0, 2, 123);
  CheckCase(135, BARCODE_POSTNET, COMPLIANT_HEIGHT, -1, 9.0, '12345678901', 0, 9.0, 2, 123);
  CheckCase(136, BARCODE_POSTNET, COMPLIANT_HEIGHT, -1, 9.1, '12345678901', ZWARN_NONCOMPLIANT, 9.1000004, 2, 123);
  CheckCase(137, BARCODE_POSTNET, -1, -1, 20.0, '12345678901', 0, 20.0, 2, 123);
  CheckCase(138, BARCODE_POSTNET, COMPLIANT_HEIGHT, -1, 20.0, '12345678901', ZWARN_NONCOMPLIANT, 20.0, 2, 123);
end;

procedure TTestHeight.Height_FIM;
begin
  CheckCase(142, BARCODE_FIM, -1, -1, 1.0, 'A', 0, 1.0, 1, 17);
  CheckCase(143, BARCODE_FIM, -1, -1, 12.7, 'A', 0, 12.7, 1, 17);
  CheckCase(144, BARCODE_FIM, COMPLIANT_HEIGHT, -1, 12.7, 'A', ZWARN_NONCOMPLIANT, 12.7, 1, 17);
  CheckCase(145, BARCODE_FIM, COMPLIANT_HEIGHT, -1, 12.8, 'A', 0, 12.8, 1, 17);
end;

procedure TTestHeight.Height_LOGMARS;
begin
  CheckCase(146, BARCODE_LOGMARS, -1, -1, 1.0, '1234567890', 0, 1.0, 1, 191);
  CheckCase(147, BARCODE_LOGMARS, -1, -1, 6.0, '1234567890', 0, 6.0, 1, 191);
  CheckCase(148, BARCODE_LOGMARS, COMPLIANT_HEIGHT, -1, 6.0, '1234567890', ZWARN_NONCOMPLIANT, 6.0, 1, 191);
  CheckCase(149, BARCODE_LOGMARS, -1, -1, 6.25, '1234567890', 0, 6.25, 1, 191);
  CheckCase(150, BARCODE_LOGMARS, COMPLIANT_HEIGHT, -1, 6.25, '1234567890', 0, 6.25, 1, 191);
  CheckCase(151, BARCODE_LOGMARS, COMPLIANT_HEIGHT, -1, 116.0, '1234567890', 0, 116.0, 1, 191);
  CheckCase(152, BARCODE_LOGMARS, COMPLIANT_HEIGHT, -1, 117.0, '1234567890', ZWARN_NONCOMPLIANT, 117.0, 1, 191);
end;

procedure TTestHeight.Height_PHARMA;
begin
  CheckCase(153, BARCODE_PHARMA, -1, -1, 1.0, '123456', 0, 1.0, 1, 58);
  CheckCase(154, BARCODE_PHARMA, -1, -1, 15.0, '123456', 0, 15.0, 1, 58);
  CheckCase(155, BARCODE_PHARMA, COMPLIANT_HEIGHT, -1, 15.0, '123456', ZWARN_NONCOMPLIANT, 15.0, 1, 58);
  CheckCase(156, BARCODE_PHARMA, COMPLIANT_HEIGHT, -1, 16.0, '123456', 0, 16.0, 1, 58);
end;

procedure TTestHeight.Height_PZN;
begin
  CheckCase(157, BARCODE_PZN, -1, -1, 1.0, '123456', 0, 1.0, 1, 142);
  CheckCase(158, BARCODE_PZN, -1, -1, 17.7, '123456', 0, 17.700001, 1, 142);
  CheckCase(159, BARCODE_PZN, COMPLIANT_HEIGHT, -1, 17.7, '123456', ZWARN_NONCOMPLIANT, 17.700001, 1, 142);
  CheckCase(160, BARCODE_PZN, COMPLIANT_HEIGHT, -1, 17.8, '123456', 0, 17.799999, 1, 142);
end;

procedure TTestHeight.Height_PHARMA_TWO;
begin
  CheckCase(161, BARCODE_PHARMA_TWO, -1, -1, -1.0, '12345678', 0, 10.0, 2, 29);
  CheckCase(162, BARCODE_PHARMA_TWO, -1, -1, 1.0, '12345678', 0, 1.0, 2, 29);
  CheckCase(163, BARCODE_PHARMA_TWO, -1, -1, 3.9, '12345678', 0, 3.9000001, 2, 29);
  CheckCase(164, BARCODE_PHARMA_TWO, COMPLIANT_HEIGHT, -1, 3.9, '12345678', ZWARN_NONCOMPLIANT, 3.9000001, 2, 29);
  CheckCase(165, BARCODE_PHARMA_TWO, COMPLIANT_HEIGHT, -1, 4.0, '12345678', 0, 4.0, 2, 29);
  CheckCase(166, BARCODE_PHARMA_TWO, -1, -1, 15.0, '12345678', 0, 15.0, 2, 29);
  CheckCase(167, BARCODE_PHARMA_TWO, COMPLIANT_HEIGHT, -1, 15.0, '12345678', 0, 15.0, 2, 29);
  CheckCase(168, BARCODE_PHARMA_TWO, COMPLIANT_HEIGHT, -1, 15.1, '12345678', ZWARN_NONCOMPLIANT, 15.1, 2, 29);
end;

procedure TTestHeight.Height_CEPNET;
begin
  CheckCase(169, BARCODE_CEPNET, -1, -1, -1.0, '12345678', 0, 5.375, 2, 93);
  CheckCase(170, BARCODE_CEPNET, -1, -1, 1.0, '12345678', 0, 1.25, 2, 93);
  CheckCase(171, BARCODE_CEPNET, -1, -1, 4.5, '12345678', 0, 4.5, 2, 93);
  CheckCase(172, BARCODE_CEPNET, COMPLIANT_HEIGHT, -1, 4.5, '12345678', ZWARN_NONCOMPLIANT, 4.5, 2, 93);
  CheckCase(173, BARCODE_CEPNET, COMPLIANT_HEIGHT, -1, 4.6, '12345678', 0, 4.5999999, 2, 93);
  CheckCase(174, BARCODE_CEPNET, -1, -1, 9.0, '12345678', 0, 9.0, 2, 93);
  CheckCase(175, BARCODE_CEPNET, COMPLIANT_HEIGHT, -1, 9.0, '12345678', 0, 9.0, 2, 93);
  CheckCase(176, BARCODE_CEPNET, COMPLIANT_HEIGHT, -1, 9.1, '12345678', ZWARN_NONCOMPLIANT, 9.1000004, 2, 93);
  CheckCase(177, BARCODE_CEPNET, -1, -1, 20.0, '12345678', 0, 20.0, 2, 93);
  CheckCase(178, BARCODE_CEPNET, COMPLIANT_HEIGHT, -1, 20.0, '12345678', ZWARN_NONCOMPLIANT, 20.0, 2, 93);
end;

procedure TTestHeight.Height_AUSPOST;
begin
  CheckCase(206, BARCODE_AUSPOST, -1, -1, -1.0, '12345678901234567890123', 0, 8.0, 3, 133);
  CheckCase(207, BARCODE_AUSPOST, -1, -1, 1.0, '12345678901234567890123', 0, 2.0, 3, 133);
  CheckCase(208, BARCODE_AUSPOST, COMPLIANT_HEIGHT, -1, 1.0, '12345678901234567890123', ZWARN_NONCOMPLIANT, 1.9230771, 3, 133);
  CheckCase(209, BARCODE_AUSPOST, -1, -1, 6.9, '12345678901234567890123', 0, 6.9000001, 3, 133);
  CheckCase(210, BARCODE_AUSPOST, COMPLIANT_HEIGHT, -1, 6.9, '12345678901234567890123', ZWARN_NONCOMPLIANT, 6.9000001, 3, 133);
  CheckCase(211, BARCODE_AUSPOST, COMPLIANT_HEIGHT, -1, 7.0, '12345678901234567890123', 0, 7.0, 3, 133);
  CheckCase(212, BARCODE_AUSPOST, -1, -1, 14.0, '12345678901234567890123', 0, 14.0, 3, 133);
  CheckCase(213, BARCODE_AUSPOST, COMPLIANT_HEIGHT, -1, 14.0, '12345678901234567890123', 0, 14.0, 3, 133);
  CheckCase(214, BARCODE_AUSPOST, COMPLIANT_HEIGHT, -1, 14.1, '12345678901234567890123', ZWARN_NONCOMPLIANT, 14.099999, 3, 133);
end;

procedure TTestHeight.Height_AUSREPLY;
begin
  CheckCase(215, BARCODE_AUSREPLY, -1, -1, 14.0, '12345678', 0, 14.0, 3, 73);
  CheckCase(216, BARCODE_AUSREPLY, COMPLIANT_HEIGHT, -1, 14.0, '12345678', 0, 14.0, 3, 73);
  CheckCase(217, BARCODE_AUSREPLY, COMPLIANT_HEIGHT, -1, 14.25, '12345678', ZWARN_NONCOMPLIANT, 14.25, 3, 73);
end;

procedure TTestHeight.Height_AUSROUTE;
begin
  CheckCase(218, BARCODE_AUSROUTE, -1, -1, 7.0, '12345678', 0, 7.0, 3, 73);
  CheckCase(219, BARCODE_AUSROUTE, COMPLIANT_HEIGHT, -1, 7.0, '12345678', 0, 7.0, 3, 73);
end;

procedure TTestHeight.Height_AUSREDIRECT;
begin
  CheckCase(220, BARCODE_AUSREDIRECT, -1, -1, 14.0, '12345678', 0, 14.0, 3, 73);
  CheckCase(221, BARCODE_AUSREDIRECT, COMPLIANT_HEIGHT, -1, 14.0, '12345678', 0, 14.0, 3, 73);
end;

procedure TTestHeight.Height_RM4SCC;
begin
  CheckCase(226, BARCODE_RM4SCC, -1, -1, -1.0, '1234567890', 0, 8.0, 3, 91);
  CheckCase(227, BARCODE_RM4SCC, -1, -1, 1.0, '1234567890', 0, 2.0, 3, 91);
  CheckCase(228, BARCODE_RM4SCC, COMPLIANT_HEIGHT, -1, 1.0, '1234567890', ZWARN_NONCOMPLIANT, 1.9615386, 3, 91);
  CheckCase(229, BARCODE_RM4SCC, -1, -1, 4.0, '1234567890', 0, 4.0, 3, 91);
  CheckCase(230, BARCODE_RM4SCC, COMPLIANT_HEIGHT, -1, 4.0, '1234567890', ZWARN_NONCOMPLIANT, 4.0, 3, 91);
  CheckCase(231, BARCODE_RM4SCC, -1, -1, 6.0, '1234567890', 0, 6.0, 3, 91);
  CheckCase(232, BARCODE_RM4SCC, COMPLIANT_HEIGHT, -1, 6.0, '1234567890', ZWARN_NONCOMPLIANT, 6.0, 3, 91);
  CheckCase(233, BARCODE_RM4SCC, COMPLIANT_HEIGHT, -1, 6.5, '1234567890', 0, 6.5, 3, 91);
  CheckCase(234, BARCODE_RM4SCC, -1, -1, 10.8, '1234567890', 0, 10.8, 3, 91);
  CheckCase(235, BARCODE_RM4SCC, COMPLIANT_HEIGHT, -1, 10.8, '1234567890', 0, 10.8, 3, 91);
  CheckCase(236, BARCODE_RM4SCC, COMPLIANT_HEIGHT, -1, 11.0, '1234567890', ZWARN_NONCOMPLIANT, 11.0, 3, 91);
  CheckCase(237, BARCODE_RM4SCC, -1, -1, 16.0, '1234567890', 0, 16.0, 3, 91);
  CheckCase(238, BARCODE_RM4SCC, COMPLIANT_HEIGHT, -1, 16.0, '1234567890', ZWARN_NONCOMPLIANT, 16.0, 3, 91);
end;

procedure TTestHeight.Height_EAN14;
begin
  CheckCase(240, BARCODE_EAN14, -1, -1, 1.0, '1234567890123', 0, 1.0, 1, 134);
  CheckCase(241, BARCODE_EAN14, -1, -1, 5.7, '1234567890123', 0, 5.6999998, 1, 134);
  CheckCase(242, BARCODE_EAN14, COMPLIANT_HEIGHT, -1, 5.7, '1234567890123', ZWARN_NONCOMPLIANT, 5.6999998, 1, 134);
  CheckCase(243, BARCODE_EAN14, COMPLIANT_HEIGHT, -1, 5.8, '1234567890123', 0, 5.8000002, 1, 134);
end;

procedure TTestHeight.Height_NVE18;
begin
  CheckCase(263, BARCODE_NVE18, -1, -1, 1.0, '12345678901234567', 0, 1.0, 1, 156);
  CheckCase(264, BARCODE_NVE18, -1, -1, 5.7, '12345678901234567', 0, 5.6999998, 1, 156);
  CheckCase(265, BARCODE_NVE18, COMPLIANT_HEIGHT, -1, 5.7, '12345678901234567', ZWARN_NONCOMPLIANT, 5.6999998, 1, 156);
  CheckCase(266, BARCODE_NVE18, COMPLIANT_HEIGHT, -1, 5.8, '12345678901234567', 0, 5.8000002, 1, 156);
end;

procedure TTestHeight.Height_JAPANPOST;
begin
  CheckCase(267, BARCODE_JAPANPOST, -1, -1, -1.0, '1234567890', 0, 8.0, 3, 133);
  CheckCase(268, BARCODE_JAPANPOST, -1, -1, 1.0, '1234567890', 0, 2.0, 3, 133);
  CheckCase(269, BARCODE_JAPANPOST, COMPLIANT_HEIGHT, -1, 1.0, '1234567890', ZWARN_NONCOMPLIANT, 1.5, 3, 133);
  CheckCase(270, BARCODE_JAPANPOST, -1, -1, 4.8, '1234567890', 0, 4.8000002, 3, 133);
  CheckCase(271, BARCODE_JAPANPOST, COMPLIANT_HEIGHT, -1, 4.8, '1234567890', ZWARN_NONCOMPLIANT, 4.8000002, 3, 133);
  CheckCase(272, BARCODE_JAPANPOST, COMPLIANT_HEIGHT, -1, 4.9, '1234567890', 0, 4.9000001, 3, 133);
  CheckCase(273, BARCODE_JAPANPOST, -1, -1, 7.0, '1234567890', 0, 7.0, 3, 133);
  CheckCase(274, BARCODE_JAPANPOST, COMPLIANT_HEIGHT, -1, 7.0, '1234567890', 0, 7.0, 3, 133);
  CheckCase(275, BARCODE_JAPANPOST, COMPLIANT_HEIGHT, -1, 7.5, '1234567890', ZWARN_NONCOMPLIANT, 7.5, 3, 133);
  CheckCase(276, BARCODE_JAPANPOST, -1, -1, 16.0, '1234567890', 0, 16.0, 3, 133);
  CheckCase(277, BARCODE_JAPANPOST, COMPLIANT_HEIGHT, -1, 16.0, '1234567890', ZWARN_NONCOMPLIANT, 15.999999, 3, 133);
end;

procedure TTestHeight.Height_PLANET;
begin
  CheckCase(301, BARCODE_PLANET, -1, -1, -1.0, '12345678901', 0, 12.0, 2, 123);
  CheckCase(302, BARCODE_PLANET, -1, -1, 1.0, '12345678901', 0, 1.0, 2, 123);
  CheckCase(303, BARCODE_PLANET, COMPLIANT_HEIGHT, -1, 1.0, '12345678901', ZWARN_NONCOMPLIANT, 1.25, 2, 123);
  CheckCase(304, BARCODE_PLANET, -1, -1, 4.5, '12345678901', 0, 4.5, 2, 123);
  CheckCase(305, BARCODE_PLANET, COMPLIANT_HEIGHT, -1, 4.5, '12345678901', ZWARN_NONCOMPLIANT, 4.5, 2, 123);
  CheckCase(306, BARCODE_PLANET, COMPLIANT_HEIGHT, -1, 4.6, '12345678901', 0, 4.5999999, 2, 123);
  CheckCase(307, BARCODE_PLANET, -1, -1, 9.0, '12345678901', 0, 9.0, 2, 123);
  CheckCase(308, BARCODE_PLANET, COMPLIANT_HEIGHT, -1, 9.0, '12345678901', 0, 9.0, 2, 123);
  CheckCase(309, BARCODE_PLANET, COMPLIANT_HEIGHT, -1, 9.1, '12345678901', ZWARN_NONCOMPLIANT, 9.1000004, 2, 123);
  CheckCase(310, BARCODE_PLANET, -1, -1, 24.0, '12345678901', 0, 24.0, 2, 123);
  CheckCase(311, BARCODE_PLANET, COMPLIANT_HEIGHT, -1, 24.0, '12345678901', ZWARN_NONCOMPLIANT, 24.0, 2, 123);
end;

procedure TTestHeight.Height_TELEPEN_NUM;
begin
  CheckCase(331, BARCODE_TELEPEN_NUM, -1, -1, 1.0, '1234567890', 0, 1.0, 1, 128);
  CheckCase(332, BARCODE_TELEPEN_NUM, COMPLIANT_HEIGHT, -1, 1.0, '1234567890', 0, 1.0, 1, 128);
  CheckCase(333, BARCODE_TELEPEN_NUM, -1, -1, 4.0, '1234567890', 0, 4.0, 1, 128);
end;

procedure TTestHeight.Height_ITF14;
begin
  CheckCase(334, BARCODE_ITF14, -1, -1, 1.0, '1234567890', 0, 1.0, 1, 135);
  CheckCase(335, BARCODE_ITF14, -1, -1, 5.7, '1234567890', 0, 5.6999998, 1, 135);
  CheckCase(336, BARCODE_ITF14, COMPLIANT_HEIGHT, -1, 5.7, '1234567890', ZWARN_NONCOMPLIANT, 5.6999998, 1, 135);
  CheckCase(337, BARCODE_ITF14, COMPLIANT_HEIGHT, -1, 5.8, '1234567890', 0, 5.8000002, 1, 135);
end;

procedure TTestHeight.Height_KIX;
begin
  CheckCase(338, BARCODE_KIX, -1, -1, -1.0, '1234567890', 0, 8.0, 3, 79);
  CheckCase(339, BARCODE_KIX, -1, -1, 1.0, '1234567890', 0, 2.0, 3, 79);
  CheckCase(340, BARCODE_KIX, COMPLIANT_HEIGHT, -1, 1.0, '1234567890', ZWARN_NONCOMPLIANT, 1.9615386, 3, 79);
  CheckCase(341, BARCODE_KIX, -1, -1, 6.4, '1234567890', 0, 6.4000001, 3, 79);
  CheckCase(342, BARCODE_KIX, COMPLIANT_HEIGHT, -1, 6.4, '1234567890', ZWARN_NONCOMPLIANT, 6.3999996, 3, 79);
  CheckCase(343, BARCODE_KIX, COMPLIANT_HEIGHT, -1, 6.5, '1234567890', 0, 6.5, 3, 79);
  CheckCase(344, BARCODE_KIX, -1, -1, 10.8, '1234567890', 0, 10.8, 3, 79);
  CheckCase(345, BARCODE_KIX, COMPLIANT_HEIGHT, -1, 10.8, '1234567890', 0, 10.8, 3, 79);
  CheckCase(346, BARCODE_KIX, COMPLIANT_HEIGHT, -1, 10.9, '1234567890', ZWARN_NONCOMPLIANT, 10.9, 3, 79);
  CheckCase(347, BARCODE_KIX, -1, -1, 16.0, '1234567890', 0, 16.0, 3, 79);
  CheckCase(348, BARCODE_KIX, COMPLIANT_HEIGHT, -1, 16.0, '1234567890', ZWARN_NONCOMPLIANT, 16.0, 3, 79);
end;

procedure TTestHeight.Height_DAFT;
begin
  CheckCase(350, BARCODE_DAFT, -1, -1, -1.0, 'DAFTDAFTDAFTDAFT', 0, 8.0, 3, 31);
  CheckCase(351, BARCODE_DAFT, -1, -1, 1.0, 'DAFTDAFTDAFTDAFT', 0, 2.0, 3, 31);
  CheckCase(352, BARCODE_DAFT, COMPLIANT_HEIGHT, -1, 1.0, 'DAFTDAFTDAFTDAFT', 0, 2.0, 3, 31);
  CheckCase(353, BARCODE_DAFT, -1, -1, 4.0, 'DAFTDAFTDAFTDAFT', 0, 4.0, 3, 31);
  CheckCase(354, BARCODE_DAFT, -1, -1, 6.0, 'DAFTDAFTDAFTDAFT', 0, 6.0, 3, 31);
  CheckCase(355, BARCODE_DAFT, -1, -1, 12.0, 'DAFTDAFTDAFTDAFT', 0, 12.0, 3, 31);
  CheckCase(356, BARCODE_DAFT, -1, -1, 16.0, 'DAFTDAFTDAFTDAFT', 0, 16.0, 3, 31);
  CheckCase(357, BARCODE_DAFT, COMPLIANT_HEIGHT, -1, 16.0, 'DAFTDAFTDAFTDAFT', 0, 16.0, 3, 31);
end;

procedure TTestHeight.Height_HIBC_39;
begin
  CheckCase(366, BARCODE_HIBC_39, -1, -1, 1.0, '1234567890', 0, 1.0, 1, 223);
  CheckCase(367, BARCODE_HIBC_39, COMPLIANT_HEIGHT, -1, 1.0, '1234567890', ZWARN_NONCOMPLIANT, 1.0, 1, 223);
  CheckCase(368, BARCODE_HIBC_39, -1, -1, 4.0, '1234567890', 0, 4.0, 1, 223);
end;

procedure TTestHeight.Height_CODE32;
begin
  CheckCase(413, BARCODE_CODE32, -1, -1, 1.0, '12345678', 0, 1.0, 1, 103);
  CheckCase(414, BARCODE_CODE32, COMPLIANT_HEIGHT, -1, 1.0, '12345678', ZWARN_NONCOMPLIANT, 1.0, 1, 103);
  CheckCase(415, BARCODE_CODE32, -1, -1, 19.0, '12345678', 0, 19.0, 1, 103);
  CheckCase(416, BARCODE_CODE32, COMPLIANT_HEIGHT, -1, 19.0, '12345678', ZWARN_NONCOMPLIANT, 19.0, 1, 103);
  CheckCase(417, BARCODE_CODE32, COMPLIANT_HEIGHT, -1, 20.0, '12345678', 0, 20.0, 1, 103);
end;

procedure TTestHeight.HeightPerRow_PHARMA_TWO;
begin
  CheckCase(62, BARCODE_PHARMA_TWO, -1, -1, -1.0, '1234', 0, 10.0, 2, 13);
  CheckCase(63, BARCODE_PHARMA_TWO, -1, HEIGHTPERROW_MODE, 0.5, '1234', 0, 1.0, 2, 13);
  CheckCase(64, BARCODE_PHARMA_TWO, -1, HEIGHTPERROW_MODE, 2.1, '1234', 0, 4.1999998, 2, 13);
  CheckCase(65, BARCODE_PHARMA_TWO, -1, HEIGHTPERROW_MODE, 2.2, '1234', 0, 4.4000001, 2, 13);
  CheckCase(66, BARCODE_PHARMA_TWO, -1, HEIGHTPERROW_MODE, 2.25, '1234', 0, 4.5, 2, 13);
end;

initialization
  ZRegisterFixture(TTestHeight);

end.
