unit Test_CommonCore;

interface

uses
  DUnitX.TestFramework,
  zint,
  zint_common;

type
  [TestFixture]
  TTestCommonCore = class
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
  Assert.AreEqual<Integer>(0, ctoi('0'));
  Assert.AreEqual<Integer>(9, ctoi('9'));
  Assert.AreEqual<Integer>(10, ctoi('A'));
  Assert.AreEqual<Integer>(15, ctoi('f'));
  Assert.AreEqual<Integer>(-1, ctoi('Z'));

  Assert.AreEqual<Char>('0', itoc(0));
  Assert.AreEqual<Char>('9', itoc(9));
  Assert.AreEqual<Char>('A', itoc(10));
  Assert.AreEqual<Char>('F', itoc(15));
end;

procedure TTestCommonCore.TestPosnSemantics;
begin
  Assert.AreEqual<Integer>(0, posn('ABC', 'A'));
  Assert.AreEqual<Integer>(2, posn('ABC', 'C'));
  Assert.AreEqual<Integer>(-1, posn('ABC', 'Z'));
  Assert.AreEqual<Integer>(1, posn('ABC', Byte(Ord('B'))));
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

  Assert.AreEqual<Integer>(2, ustrlen(Data));
end;

procedure TTestCommonCore.TestGs1AndEciSupport;
begin
  Assert.IsTrue(gs1_compliant(BARCODE_CODEONE));
  Assert.IsFalse(gs1_compliant(BARCODE_CODE39));

  Assert.IsTrue(supports_eci(BARCODE_QRCODE));
  Assert.IsFalse(supports_eci(BARCODE_CODE39));
end;

procedure TTestCommonCore.TestSymbologyRoundTrip;
var
  Raw: Integer;
  Sym: TZintSymbology;
begin
  Raw := SymbologyToInt(zsCODEONE);
  Assert.AreEqual<Integer>(BARCODE_CODEONE, Raw);
  Sym := IntToSymbology(Raw);
  Assert.AreEqual<TZintSymbology>(zsCODEONE, Sym);

  Raw := SymbologyToInt(zsQRCODE);
  Assert.AreEqual<Integer>(BARCODE_QRCODE, Raw);
  Sym := IntToSymbology(Raw);
  Assert.AreEqual<TZintSymbology>(zsQRCODE, Sym);
end;

procedure TTestCommonCore.TestStackableAndExtendable;
begin
  Assert.IsTrue(is_stackable(BARCODE_CODE128B));
  Assert.IsFalse(is_stackable(BARCODE_QRCODE));

  Assert.IsTrue(is_extendable(BARCODE_EANX));
  Assert.IsFalse(is_extendable(BARCODE_CODE39));
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

  Assert.IsTrue(istwodigits(Data, 0));
  Assert.IsFalse(istwodigits(Data, 1));
end;

procedure TTestCommonCore.TestFroundupThreshold;
begin
  Assert.AreEqual<Single>(2.0, froundup(1.5));
  Assert.AreEqual<Single>(1.009, froundup(1.009));
  Assert.AreEqual<Single>(2.01, froundup(2.01));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCommonCore);

end.
