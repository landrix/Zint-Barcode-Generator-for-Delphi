unit Test_CommonCore;

{$I zint_test.inc}

interface

uses
  {$IFNDEF FPC}DUnitX.TestFramework,{$ENDIF}
  TestFramework_Zint,
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

initialization
  ZRegisterFixture(TTestCommonCore);

end.
