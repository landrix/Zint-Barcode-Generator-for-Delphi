unit Test_Code128;

{
  Unit-Tests fuer Code 128, EAN-128 (GS1-128), EAN-14, NVE-18, HIBC-128.
  Testdaten abgeleitet aus dem C-Testframework (backend/tests/test_code128.c)
  und angepasst an den Delphi-Port basierend auf Commit 3432bc9.
}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  zint,
  zint_common,
  TestHelper_Zint;

type
  [TestFixture]
  TTestCode128 = class
  private
    FSymbol: TZintSymbol;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_Encode_SingleChar;
    [Test]
    procedure Test_Encode_Numeric;
    [Test]
    procedure Test_Encode_Alpha;
    [Test]
    procedure Test_Encode_Mixed;
    [Test]
    procedure Test_Encode_TooLong;
    [Test]
    procedure Test_Encode_EmptyInput;
    [Test]
    procedure Test_Encode_Width;
    [Test]
    procedure Test_Dimensions_SingleRow;
    [Test]
    procedure Test_Code128B_Basic;
    [Test]
    procedure Test_Code128B_TooLong;
  end;

  [TestFixture]
  TTestEAN128 = class
  private
    FSymbol: TZintSymbol;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_EAN128_Basic;
    [Test]
    procedure Test_EAN128_InvalidData;
  end;

  [TestFixture]
  TTestEAN14 = class
  private
    FSymbol: TZintSymbol;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_EAN14_Valid;
    [Test]
    procedure Test_EAN14_TooLong;
    [Test]
    procedure Test_EAN14_NonNumeric;
  end;

  [TestFixture]
  TTestNVE18 = class
  private
    FSymbol: TZintSymbol;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_NVE18_Valid;
    [Test]
    procedure Test_NVE18_TooLong;
  end;

  [TestFixture]
  TTestHIBC128 = class
  private
    FSymbol: TZintSymbol;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_HIBC128_Basic;
    [Test]
    procedure Test_HIBC128_TooLong;
  end;

implementation

{ TTestCode128 }

procedure TTestCode128.Setup;
begin
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_CODE128);
end;

procedure TTestCode128.TearDown;
begin
  FreeAndNil(FSymbol);
end;

procedure TTestCode128.Test_Encode_SingleChar;
var
  Ret: Integer;
begin
  Ret := TZintTestHelper.EncodeData(FSymbol, 'A');
  Assert.AreEqual(ZINT_OK, Ret, 'Single char A should encode OK');
  Assert.AreEqual(1, FSymbol.rows, 'Code128 is always 1 row');
  Assert.IsTrue(FSymbol.width > 0, 'Width should be > 0');
end;

procedure TTestCode128.Test_Encode_Numeric;
var
  Ret: Integer;
begin
  // Rein numerische Daten -> Code C Modus (kompakter)
  Ret := TZintTestHelper.EncodeData(FSymbol, '1234567890');
  Assert.AreEqual(ZINT_OK, Ret, '10 digits should encode OK');
  Assert.AreEqual(1, FSymbol.rows);
end;

procedure TTestCode128.Test_Encode_Alpha;
var
  Ret: Integer;
begin
  Ret := TZintTestHelper.EncodeData(FSymbol, 'ABCDEFGHIJ');
  Assert.AreEqual(ZINT_OK, Ret, '10 alpha chars should encode OK');
  Assert.AreEqual(1, FSymbol.rows);
  Assert.IsTrue(FSymbol.width > 0, 'Width should be > 0');
end;

procedure TTestCode128.Test_Encode_Mixed;
var
  Ret: Integer;
begin
  // Gemischter Inhalt: Alpha + Numerisch
  Ret := TZintTestHelper.EncodeData(FSymbol, 'ABC12345');
  Assert.AreEqual(ZINT_OK, Ret, 'Mixed alphanumeric should encode OK');
  Assert.AreEqual(1, FSymbol.rows);
end;

procedure TTestCode128.Test_Encode_TooLong;
var
  Ret: Integer;
  LongStr: String;
  ErrTxt: String;
begin
  // Der alte Port begrenzt auf 160 Zeichen vor der Glyph-Berechnung
  LongStr := TZintTestHelper.StrRepeat('A', 161);
  Ret := TZintTestHelper.EncodeData(FSymbol, LongStr);
  Assert.AreEqual(ZINT_ERROR_TOO_LONG, Ret, '161 chars should be too long');

  ErrTxt := TZintTestHelper.GetErrTxt(FSymbol);
  Assert.IsTrue(Pos('too long', LowerCase(ErrTxt)) > 0,
    'Error text should mention "too long", got: ' + ErrTxt);
end;

procedure TTestCode128.Test_Encode_EmptyInput;
var
  Ret: Integer;
begin
  Ret := TZintTestHelper.EncodeData(FSymbol, '');
  Assert.IsTrue(Ret >= ZINT_ERROR, 'Empty input should produce an error');
end;

procedure TTestCode128.Test_Encode_Width;
var
  Ret: Integer;
  Width5, Width10: Integer;
begin
  // Mehr Daten -> breiterer Barcode
  Ret := TZintTestHelper.EncodeData(FSymbol, '12345');
  Assert.AreEqual(ZINT_OK, Ret);
  Width5 := FSymbol.width;

  // Neues Symbol fuer zweiten Encode
  FreeAndNil(FSymbol);
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_CODE128);

  Ret := TZintTestHelper.EncodeData(FSymbol, '1234567890');
  Assert.AreEqual(ZINT_OK, Ret);
  Width10 := FSymbol.width;

  Assert.IsTrue(Width10 > Width5,
    Format('Width for 10 digits (%d) should be > width for 5 digits (%d)', [Width10, Width5]));
end;

procedure TTestCode128.Test_Dimensions_SingleRow;
var
  Ret: Integer;
begin
  // Code 128 erzeugt immer genau eine Zeile
  Ret := TZintTestHelper.EncodeData(FSymbol, 'Test12345');
  Assert.AreEqual(ZINT_OK, Ret);
  Assert.AreEqual(1, FSymbol.rows, 'Code128 must have exactly 1 row');
  // Mindestbreite: Start(11) + mindestens 1 Datencodeword(11) + Checksum(11) + Stop(13) = 46
  Assert.IsTrue(FSymbol.width >= 46,
    Format('Width %d should be >= 46 (minimum Code128)', [FSymbol.width]));
end;

procedure TTestCode128.Test_Code128B_Basic;
var
  Ret: Integer;
begin
  FSymbol.symbology := BARCODE_CODE128B;
  Ret := TZintTestHelper.EncodeData(FSymbol, 'ABC123');
  Assert.AreEqual(ZINT_OK, Ret, 'Code128B basic encode should succeed');
  Assert.AreEqual(1, FSymbol.rows);
end;

procedure TTestCode128.Test_Code128B_TooLong;
var
  Ret: Integer;
  LongStr: String;
begin
  FSymbol.symbology := BARCODE_CODE128B;
  LongStr := TZintTestHelper.StrRepeat('A', 161);
  Ret := TZintTestHelper.EncodeData(FSymbol, LongStr);
  Assert.AreEqual(ZINT_ERROR_TOO_LONG, Ret, '161 chars should be too long for Code128B');
end;

{ TTestEAN128 }

procedure TTestEAN128.Setup;
begin
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_EAN128);
  // Der alte Port fuehrt gs1_verify intern in ean_128() aus,
  // daher DATA_MODE (Default) verwenden, nicht GS1_MODE.
  FSymbol.input_mode := DATA_MODE;
end;

procedure TTestEAN128.TearDown;
begin
  FreeAndNil(FSymbol);
end;

procedure TTestEAN128.Test_EAN128_Basic;
var
  Ret: Integer;
begin
  Ret := TZintTestHelper.EncodeData(FSymbol, '[01]12345678901231');
  Assert.AreEqual(ZINT_OK, Ret, 'Valid EAN-128 with AI 01 should encode OK');
  Assert.AreEqual(1, FSymbol.rows);
  Assert.IsTrue(FSymbol.width > 0);
end;

procedure TTestEAN128.Test_EAN128_InvalidData;
var
  Ret: Integer;
begin
  // Leere GS1-Daten
  Ret := TZintTestHelper.EncodeData(FSymbol, '');
  Assert.IsTrue(Ret >= ZINT_ERROR, 'Empty GS1 data should produce error');
end;

{ TTestEAN14 }

procedure TTestEAN14.Setup;
begin
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_EAN14);
end;

procedure TTestEAN14.TearDown;
begin
  FreeAndNil(FSymbol);
end;

procedure TTestEAN14.Test_EAN14_Valid;
var
  Ret: Integer;
begin
  Ret := TZintTestHelper.EncodeData(FSymbol, '1234567890123');
  Assert.AreEqual(ZINT_OK, Ret, '13-digit EAN-14 (check digit auto) should encode OK');
  Assert.AreEqual(1, FSymbol.rows);
end;

procedure TTestEAN14.Test_EAN14_TooLong;
var
  Ret: Integer;
begin
  Ret := TZintTestHelper.EncodeData(FSymbol, '123456789012345');
  Assert.IsTrue(Ret >= ZINT_ERROR, '15 digits should be too long for EAN-14');
end;

procedure TTestEAN14.Test_EAN14_NonNumeric;
var
  Ret: Integer;
begin
  Ret := TZintTestHelper.EncodeData(FSymbol, '123456789012A');
  Assert.IsTrue(Ret >= ZINT_ERROR, 'Non-numeric data should cause error for EAN-14');
end;

{ TTestNVE18 }

procedure TTestNVE18.Setup;
begin
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_NVE18);
end;

procedure TTestNVE18.TearDown;
begin
  FreeAndNil(FSymbol);
end;

procedure TTestNVE18.Test_NVE18_Valid;
var
  Ret: Integer;
begin
  Ret := TZintTestHelper.EncodeData(FSymbol, '12345678901234567');
  Assert.AreEqual(ZINT_OK, Ret, '17-digit NVE-18 should encode OK');
  Assert.AreEqual(1, FSymbol.rows);
end;

procedure TTestNVE18.Test_NVE18_TooLong;
var
  Ret: Integer;
begin
  Ret := TZintTestHelper.EncodeData(FSymbol, '1234567890123456789');
  Assert.IsTrue(Ret >= ZINT_ERROR, '19 digits should be too long for NVE-18');
end;

{ TTestHIBC128 }

procedure TTestHIBC128.Setup;
begin
  FSymbol := TZintTestHelper.CreateSymbol(BARCODE_HIBC_128);
end;

procedure TTestHIBC128.TearDown;
begin
  FreeAndNil(FSymbol);
end;

procedure TTestHIBC128.Test_HIBC128_Basic;
var
  Ret: Integer;
begin
  Ret := TZintTestHelper.EncodeData(FSymbol, '1234567890');
  Assert.AreEqual(ZINT_OK, Ret, 'HIBC 128 basic encode should succeed');
  Assert.AreEqual(1, FSymbol.rows);
end;

procedure TTestHIBC128.Test_HIBC128_TooLong;
var
  Ret: Integer;
  LongStr: String;
begin
  // HIBC fuegt Praefix und Check-Zeichen hinzu, daher kuerzere Grenze
  LongStr := TZintTestHelper.StrRepeat('1', 111);
  Ret := TZintTestHelper.EncodeData(FSymbol, LongStr);
  Assert.IsTrue(Ret >= ZINT_ERROR,
    '111 chars should be too long for HIBC 128');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCode128);
  TDUnitX.RegisterTestFixture(TTestEAN128);
  TDUnitX.RegisterTestFixture(TTestEAN14);
  TDUnitX.RegisterTestFixture(TTestNVE18);
  TDUnitX.RegisterTestFixture(TTestHIBC128);

end.
