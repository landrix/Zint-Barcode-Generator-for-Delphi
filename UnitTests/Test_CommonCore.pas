unit Test_CommonCore;

{$I zint_test.inc}

interface

uses
  {$IFNDEF FPC}DUnitX.TestFramework,{$ENDIF}
  TestFramework_Zint,
  TestHelper_Zint,
  SysUtils,
  zint,
  zint_common;

type
  [TestFixture]
  TTestCommonCore = class(TZintFixture)
  published
    [Test]
    procedure TestCtoiItocHex;
    [Test]
    procedure TestPosnSemantics;
    [Test]
    procedure TestUstrlenStopsAtNull;
    [Test]
    procedure TestGs1AndEciSupport;
    [Test]
    procedure TestSymbologyRoundTrip;
    [Test]
    procedure TestStackableAndExtendable;
    [Test]
    procedure TestIsTwoDigits;
    [Test]
    procedure TestFroundupThreshold;
    [Test]
    procedure TestSetHeight;
  private
    procedure SetHeightCase(ACIndex, ARows: Integer; const ARowHeights: array of Single;
      const AHeight, AMinRowHeight, ADefaultHeight, AMaxHeight: Single;
      ANoErrtxt, ARet: Integer; const AExpHeight: Single; const AExpErrtxt: String);
  end;

implementation

procedure TTestCommonCore.TestCtoiItocHex;
begin
  ZAssert.AreEqual(0, ctoi('0'));
  ZAssert.AreEqual(9, ctoi('9'));
  ZAssert.AreEqual(10, ctoi('A'));
  ZAssert.AreEqual(15, ctoi('f'));
  ZAssert.AreEqual(-1, ctoi('Z'));

  ZAssert.AreEqual('0', itoc(0));
  ZAssert.AreEqual('9', itoc(9));
  ZAssert.AreEqual('A', itoc(10));
  ZAssert.AreEqual('F', itoc(15));
end;

procedure TTestCommonCore.TestPosnSemantics;
begin
  ZAssert.AreEqual(0, posn('ABC', 'A'));
  ZAssert.AreEqual(2, posn('ABC', 'C'));
  ZAssert.AreEqual(-1, posn('ABC', 'Z'));
  ZAssert.AreEqual(1, posn('ABC', Byte(Ord('B'))));
end;

procedure TTestCommonCore.TestUstrlenStopsAtNull;
var
  Data: TArrayOfByte;
begin
  SetLength(Data, 5);
  Data[0] := Ord('A');
  Data[1] := Ord('B');
  Data[2] := 0;
  Data[3] := Ord('C');
  Data[4] := 0;

  ZAssert.AreEqual(2, ustrlen(Data));
end;

procedure TTestCommonCore.TestGs1AndEciSupport;
begin
  ZAssert.IsTrue(gs1_compliant(BARCODE_CODEONE));
  ZAssert.IsFalse(gs1_compliant(BARCODE_CODE39));

  ZAssert.IsTrue(supports_eci(BARCODE_QRCODE));
  ZAssert.IsFalse(supports_eci(BARCODE_CODE39));
end;

procedure TTestCommonCore.TestSymbologyRoundTrip;
var
  Raw: Integer;
  Sym: TZintSymbology;
begin
  Raw := SymbologyToInt(zsCODEONE);
  ZAssert.AreEqual(BARCODE_CODEONE, Raw);
  Sym := IntToSymbology(Raw);
  ZAssert.AreEqual(Ord(zsCODEONE), Ord(Sym));

  Raw := SymbologyToInt(zsQRCODE);
  ZAssert.AreEqual(BARCODE_QRCODE, Raw);
  Sym := IntToSymbology(Raw);
  ZAssert.AreEqual(Ord(zsQRCODE), Ord(Sym));
end;

procedure TTestCommonCore.TestStackableAndExtendable;
begin
  ZAssert.IsTrue(is_stackable(BARCODE_CODE128B));
  ZAssert.IsFalse(is_stackable(BARCODE_QRCODE));

  ZAssert.IsTrue(is_extendable(BARCODE_EANX));
  ZAssert.IsFalse(is_extendable(BARCODE_CODE39));
end;

procedure TTestCommonCore.TestIsTwoDigits;
var
  Data: TArrayOfByte;
begin
  SetLength(Data, 4);
  Data[0] := Ord('1');
  Data[1] := Ord('2');
  Data[2] := Ord('A');
  Data[3] := 0;

  ZAssert.IsTrue(istwodigits(Data, 0));
  ZAssert.IsFalse(istwodigits(Data, 1));
end;

procedure TTestCommonCore.TestFroundupThreshold;
begin
  ZAssert.AreEqual(2.0, froundup(1.5));
  ZAssert.AreEqual(1.009, froundup(1.009));
  ZAssert.AreEqual(2.01, froundup(2.01));
end;

{ C: test_set_height, test_common.c:1370. Prueft z_set_height direkt, ohne
  Umweg ueber eine Symbologie - die einzige Stelle upstream, die die
  Zweige der Funktion einzeln durchgeht.

  Abweichung beim errtxt: C schreibt in z_set_height nur "247: ..." und
  setzt das Wort "Warning" erst in error_tag davor. Der Port schreibt den
  vollen Text im Modul, wie alle anderen portierten Meldungen auch. }
procedure TTestCommonCore.SetHeightCase(ACIndex, ARows: Integer;
  const ARowHeights: array of Single;
  const AHeight, AMinRowHeight, ADefaultHeight, AMaxHeight: Single;
  ANoErrtxt, ARet: Integer; const AExpHeight: Single; const AExpErrtxt: String);
var
  sym: TZintSymbol;
  ctx: String;
  i: Integer;
begin
  ctx := Format('C#%d', [ACIndex]);
  sym := TZintSymbol.Create(nil);
  try
    sym.rows := ARows;
    for i := 0 to High(sym.row_height) do
      sym.row_height[i] := 0;
    for i := 0 to High(ARowHeights) do
      sym.row_height[i] := ARowHeights[i];
    sym.height := AHeight;
    ZAssert.AreEqual(ARet,
      set_height(sym, AMinRowHeight, ADefaultHeight, AMaxHeight, ANoErrtxt), ctx + ' ret');
    ZAssert.AreEqual(AExpHeight, sym.height, ctx + ' height');
    ZAssert.AreEqual(AExpErrtxt, TZintTestHelper.GetErrTxt(sym), ctx + ' errtxt');
  finally
    sym.Free;
  end;
end;

procedure TTestCommonCore.TestSetHeight;
const
  W = 'Warning ';
begin
  SetHeightCase( 0,  0, [],           0,  0,          0,   0,   0, 0,                  0.5, '');
  { zero_count = 0, nur feste Hoehen }
  SetHeightCase( 1,  2, [1, 1],       2,  0,          0,   0,   0, 0,                  2,   '');
  SetHeightCase( 2,  2, [1, 1],       2,  0,          0,   1,   1, ZWARN_NONCOMPLIANT, 2,   '');
  SetHeightCase( 3,  2, [1, 1],       2,  0,          0,   1,   0, ZWARN_NONCOMPLIANT, 2,
                 W + '248: Height not compliant with standards (maximum 1)');
  { zero_count <> 0 }
  SetHeightCase( 4,  2, [2, 0],       2,  0,          0,   0,   0, 0,                  2.5, '');
  SetHeightCase( 5,  2, [2, 0],       2,  1,          0,   0,   1, ZWARN_NONCOMPLIANT, 2.5, '');
  SetHeightCase( 6,  2, [2, 0],       2,  1,          0,   0,   0, ZWARN_NONCOMPLIANT, 2.5,
                 W + '247: Height not compliant with standards (too small)');
  SetHeightCase( 7,  2, [2, 0],       0,  0,          20,  0,   0, 0,                  22,  '');
  SetHeightCase( 8,  2, [2, 0],       20, 0,          20,  0,   0, 0,                  20,  '');
  SetHeightCase( 9,  2, [2, 0],       0,  2,          0,   0,   0, 0,                  4,   '');
  { War vor der Epsilon-Behandlung nicht konform (CODABLOCKF) }
  SetHeightCase(10, 20, [],           0,  12.9000006, 258, 0,   0, 0,                  258, '');
  { dito (CODE93) }
  SetHeightCase(11,  1, [],           9.9,  9.90000057, 40, 0,  0, 0,                  9.9, '');
  SetHeightCase(12,  1, [],           9.89, 9.90000057, 40, 0,  0, ZWARN_NONCOMPLIANT, 9.89,
                 W + '247: Height not compliant with standards (too small)');
  SetHeightCase(13,  1, [],           40.02, 10,       40, 40.01, 0, ZWARN_NONCOMPLIANT, 40.02,
                 W + '248: Height not compliant with standards (maximum 40.01)');
end;


initialization
  ZRegisterFixture(TTestCommonCore);

end.
