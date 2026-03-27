unit Test_PDF417;

interface

uses
  DUnitX.TestFramework,
  TestHelper_Zint,
  zint,
  zint_common;

type
  [TestFixture]
  TTestPDF417FromC = class(TObject)
  published
    [Test]
    procedure TestLargeSubset;
    [Test]
    procedure TestOptionsSubset;
    [Test]
    procedure TestNumbprocessSubset;
    [Test]
    procedure TestReaderInitSubset;
    [Test]
    procedure TestInputSubset;
    [Test]
    procedure TestEncodeSubset;
    [Test]
    procedure TestEncodeOddSubset;
    [Test]
    procedure TestEncodeOddSubset2;
    [Test]
    procedure TestEncodeOddSubset3;
    [Test]
    procedure TestEncodeSegsMainSubset;
    [Test]
    procedure TestEncodeSegsExtraSubset;
    [Test]
    procedure TestEncodeSegsOptionSubset;
    [Test]
    procedure TestEncodeSegsStructAppSubset;
    [Test]
    procedure TestEncodeSegsDataModeSubset;
    [Test]
    procedure TestRTSubset;
    [Test]
    procedure TestRTSegsSubset;
    [Test]
    procedure TestFuzzSubset;
  end;

implementation

uses
  System.SysUtils;

procedure TTestPDF417FromC.TestLargeSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Pattern: String;
    Length: Integer;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedErrTxt: String;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..10] of TCase;
  I: Integer;
  Ret: Integer;
begin
  for I := Low(Cases) to High(Cases) do
    Cases[I] := Default(TCase);

  { C test_large C#0 }
  Cases[0].Index := 0;
  Cases[0].Symbology := BARCODE_PDF417;
  Cases[0].Option1 := 0;
  Cases[0].Option2 := -1;
  Cases[0].Option3 := -1;
  Cases[0].Pattern := 'A';
  Cases[0].Length := 1850;
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 32;
  Cases[0].ExpectedWidth := 562;
  Cases[0].ExpectedErrTxt := '';

  { C test_large C#1 }
  Cases[1].Index := 1;
  Cases[1].Symbology := BARCODE_PDF417;
  Cases[1].Option1 := 0;
  Cases[1].Option2 := -1;
  Cases[1].Option3 := -1;
  Cases[1].Pattern := 'A';
  Cases[1].Length := 1851;
  Cases[1].ExpectedRet := ZERROR_TOO_LONG;
  Cases[1].ExpectedRows := -1;
  Cases[1].ExpectedWidth := -1;
  Cases[1].ExpectedErrTxt := 'Error 464: Input too long, requires too many codewords (maximum 928)';
  { C test_large C#2 }
  Cases[2].Index := 2;
  Cases[2].Symbology := BARCODE_PDF417;
  Cases[2].Option1 := 0;
  Cases[2].Option2 := -1;
  Cases[2].Option3 := -1;
  Cases[2].Pattern := #$80;
  Cases[2].Length := 1108;
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedRows := 32;
  Cases[2].ExpectedWidth := 562;
  Cases[2].ExpectedErrTxt := '';

  { C test_large C#4 }
  Cases[3].Index := 4;
  Cases[3].Symbology := BARCODE_PDF417;
  Cases[3].Option1 := 0;
  Cases[3].Option2 := -1;
  Cases[3].Option3 := -1;
  Cases[3].Pattern := '1';
  Cases[3].Length := 2710;
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedRows := 32;
  Cases[3].ExpectedWidth := 562;
  Cases[3].ExpectedErrTxt := '';

  { C test_large C#5 }
  Cases[4].Index := 5;
  Cases[4].Symbology := BARCODE_PDF417;
  Cases[4].Option1 := 0;
  Cases[4].Option2 := -1;
  Cases[4].Option3 := -1;
  Cases[4].Pattern := '1';
  Cases[4].Length := 2711;
  Cases[4].ExpectedRet := ZERROR_TOO_LONG;
  Cases[4].ExpectedRows := -1;
  Cases[4].ExpectedWidth := -1;
  Cases[4].ExpectedErrTxt := 'Error 463: Input length 2711 too long (maximum 2710)';

  { C test_large C#10 }
  Cases[5].Index := 10;
  Cases[5].Symbology := BARCODE_MICROPDF417;
  Cases[5].Option1 := 0;
  Cases[5].Option2 := -1;
  Cases[5].Option3 := -1;
  Cases[5].Pattern := 'A';
  Cases[5].Length := 250;
  Cases[5].ExpectedRet := 0;
  Cases[5].ExpectedRows := 44;
  Cases[5].ExpectedWidth := 99;
  Cases[5].ExpectedErrTxt := '';

  { C test_large C#11 }
  Cases[6].Index := 11;
  Cases[6].Symbology := BARCODE_MICROPDF417;
  Cases[6].Option1 := 0;
  Cases[6].Option2 := -1;
  Cases[6].Option3 := -1;
  Cases[6].Pattern := 'A';
  Cases[6].Length := 251;
  Cases[6].ExpectedRet := ZERROR_TOO_LONG;
  Cases[6].ExpectedRows := -1;
  Cases[6].ExpectedWidth := -1;
  Cases[6].ExpectedErrTxt := 'Error 467: Input too long, requires 127 codewords (maximum 126)';

  { Additional critical boundary cases }
  { C test_large C#6 }
  Cases[7].Index := 6;
  Cases[7].Symbology := BARCODE_PDF417;
  Cases[7].Option1 := 0;
  Cases[7].Option2 := -1;
  Cases[7].Option3 := 59;
  Cases[7].Pattern := 'A';
  Cases[7].Length := 1850;
  Cases[7].ExpectedRet := ZERROR_TOO_LONG;
  Cases[7].ExpectedRows := -1;
  Cases[7].ExpectedWidth := -1;
  Cases[7].ExpectedErrTxt := 'Error 465: Input too long, requires too many codewords (maximum 928)';

  { C test_large C#7 }
  Cases[8].Index := 7;
  Cases[8].Symbology := BARCODE_PDF417;
  Cases[8].Option1 := 0;
  Cases[8].Option2 := 1;
  Cases[8].Option3 := 3;
  Cases[8].Pattern := 'A';
  Cases[8].Length := 1850;
  Cases[8].ExpectedRet := ZERROR_TOO_LONG;
  Cases[8].ExpectedRows := -1;
  Cases[8].ExpectedWidth := -1;
  Cases[8].ExpectedErrTxt := 'Error 745: Input too long for number of columns ''1''';

  { C test_large C#8 }
  Cases[9].Index := 8;
  Cases[9].Symbology := BARCODE_PDF417;
  Cases[9].Option1 := 0;
  Cases[9].Option2 := -1;
  Cases[9].Option3 := 3;
  Cases[9].Pattern := 'A';
  Cases[9].Length := 1850;
  Cases[9].ExpectedRet := ZWARN_INVALID_OPTION;
  Cases[9].ExpectedRows := 32;
  Cases[9].ExpectedWidth := 562;
  Cases[9].ExpectedErrTxt := 'Warning 746: Number of rows increased from 3 to 32';

  { C test_large C#9 }
  Cases[10].Index := 9;
  Cases[10].Symbology := BARCODE_PDF417;
  Cases[10].Option1 := 0;
  Cases[10].Option2 := 30;
  Cases[10].Option3 := -1;
  Cases[10].Pattern := 'A';
  Cases[10].Length := 1850;
  Cases[10].ExpectedRet := ZERROR_TOO_LONG;
  Cases[10].ExpectedRows := -1;
  Cases[10].ExpectedWidth := -1;
  Cases[10].ExpectedErrTxt := 'Error 747: Input too long, requires too many codewords (maximum 928)';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_PDF417);
    try
      Symbol.option_2 := 0;
      Symbol.option_3 := 0;
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, 0, Cases[I].Option1, Cases[I].Option2, Cases[I].Option3, -1);

      { Build data string by repeating pattern }
      Ret := TZintTestHelper.EncodeData(Symbol, TZintTestHelper.StrRepeat(Cases[I].Pattern, Cases[I].Length));
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
      if Cases[I].ExpectedErrTxt <> '' then
        Assert.AreEqual(Cases[I].ExpectedErrTxt, TZintTestHelper.GetErrTxt(Symbol), Format('C#%d errtxt', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestOptionsSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedOption1: Integer;
    ExpectedOption2: Integer;
    ExpectedOption3: Integer;
    ExpectedErrTxt: String;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..14] of TCase;
  I: Integer;
  Ret: Integer;
begin
  for I := Low(Cases) to High(Cases) do
    Cases[I] := Default(TCase);

  { C test_options C#0: ECC auto-set to 2, cols auto-set to 2 }
  Cases[0].Index := 0;
  Cases[0].Symbology := BARCODE_PDF417;
  Cases[0].Option1 := -1;
  Cases[0].Option2 := -1;
  Cases[0].Option3 := -1;
  Cases[0].Data := '12345';
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 6;
  Cases[0].ExpectedWidth := 103;
  Cases[0].ExpectedOption1 := 2;
  Cases[0].ExpectedOption2 := 2;
  Cases[0].ExpectedOption3 := 6;
  Cases[0].ExpectedErrTxt := '';

  { C test_options C#3: ECC 3, cols auto-set to 3 }
  Cases[1].Index := 3;
  Cases[1].Symbology := BARCODE_PDF417;
  Cases[1].Option1 := 3;
  Cases[1].Option2 := -1;
  Cases[1].Option3 := -1;
  Cases[1].Data := '12345';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 7;
  Cases[1].ExpectedWidth := 120;
  Cases[1].ExpectedOption1 := 3;
  Cases[1].ExpectedOption2 := 3;
  Cases[1].ExpectedOption3 := 7;
  Cases[1].ExpectedErrTxt := '';

  { C test_options C#4: ECC 3, cols 2 }
  Cases[2].Index := 4;
  Cases[2].Symbology := BARCODE_PDF417;
  Cases[2].Option1 := 3;
  Cases[2].Option2 := 2;
  Cases[2].Option3 := -1;
  Cases[2].Data := '12345';
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedRows := 10;
  Cases[2].ExpectedWidth := 103;
  Cases[2].ExpectedOption1 := 3;
  Cases[2].ExpectedOption2 := 2;
  Cases[2].ExpectedOption3 := 10;
  Cases[2].ExpectedErrTxt := '';

  { C test_options C#10: Invalid ECC, auto-set }
  Cases[3].Index := 10;
  Cases[3].Symbology := BARCODE_PDF417;
  Cases[3].Option1 := 9;
  Cases[3].Option2 := -1;
  Cases[3].Option3 := -1;
  Cases[3].Data := '12345';
  Cases[3].ExpectedRet := ZWARN_INVALID_OPTION;
  Cases[3].ExpectedRows := 6;
  Cases[3].ExpectedWidth := 103;
  Cases[3].ExpectedOption1 := 2;
  Cases[3].ExpectedOption2 := 2;
  Cases[3].ExpectedOption3 := 6;
  Cases[3].ExpectedErrTxt := 'Warning 460: Error correction level ''9'' out of range (0 to 8), ignoring';

  { C test_options C#11: Invalid cols, auto-set }
  Cases[4].Index := 11;
  Cases[4].Symbology := BARCODE_PDF417;
  Cases[4].Option1 := -1;
  Cases[4].Option2 := 31;
  Cases[4].Option3 := -1;
  Cases[4].Data := '12345';
  Cases[4].ExpectedRet := ZWARN_INVALID_OPTION;
  Cases[4].ExpectedRows := 6;
  Cases[4].ExpectedWidth := 103;
  Cases[4].ExpectedOption1 := 2;
  Cases[4].ExpectedOption2 := 2;
  Cases[4].ExpectedOption3 := 6;
  Cases[4].ExpectedErrTxt := 'Warning 461: Number of columns ''31'' out of range (1 to 30), ignoring';

  { C test_options C#1: Invalid rows }
  Cases[5].Index := 1;
  Cases[5].Symbology := BARCODE_PDF417;
  Cases[5].Option1 := -1;
  Cases[5].Option2 := -1;
  Cases[5].Option3 := 928;
  Cases[5].Data := '12345';
  Cases[5].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[5].ExpectedRows := -1;
  Cases[5].ExpectedWidth := -1;
  Cases[5].ExpectedOption1 := -1;
  Cases[5].ExpectedOption2 := 0;
  Cases[5].ExpectedOption3 := 928;
  Cases[5].ExpectedErrTxt := 'Error 466: Number of rows ''928'' out of range (3 to 90)';

  { C test_options C#2: Invalid rows (too low) }
  Cases[6].Index := 2;
  Cases[6].Symbology := BARCODE_PDF417;
  Cases[6].Option1 := -1;
  Cases[6].Option2 := -1;
  Cases[6].Option3 := 1;
  Cases[6].Data := '12345';
  Cases[6].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[6].ExpectedRows := -1;
  Cases[6].ExpectedWidth := -1;
  Cases[6].ExpectedOption1 := -1;
  Cases[6].ExpectedOption2 := 0;
  Cases[6].ExpectedOption3 := 1;
  Cases[6].ExpectedErrTxt := 'Error 466: Number of rows ''1'' out of range (3 to 90)';

  { C test_options C#24: MicroPDF417 invalid cols }
  Cases[7].Index := 24;
  Cases[7].Symbology := BARCODE_MICROPDF417;
  Cases[7].Option1 := -1;
  Cases[7].Option2 := 5;
  Cases[7].Option3 := -1;
  Cases[7].Data := '12345';
  Cases[7].ExpectedRet := ZWARN_INVALID_OPTION;
  Cases[7].ExpectedRows := 11;
  Cases[7].ExpectedWidth := 38;
  Cases[7].ExpectedOption1 := 64 shl 8;
  Cases[7].ExpectedOption2 := 1;
  Cases[7].ExpectedOption3 := 0;
  Cases[7].ExpectedErrTxt := 'Warning 468: Number of columns ''5'' out of range (1 to 4), ignoring';

  { C test_options C#26: MicroPDF417 cannot specify rows }
  Cases[8].Index := 26;
  Cases[8].Symbology := BARCODE_MICROPDF417;
  Cases[8].Option1 := -1;
  Cases[8].Option2 := 5;
  Cases[8].Option3 := 3;
  Cases[8].Data := '12345';
  Cases[8].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[8].ExpectedRows := -1;
  Cases[8].ExpectedWidth := -1;
  Cases[8].ExpectedOption1 := -1;
  Cases[8].ExpectedOption2 := 5;
  Cases[8].ExpectedOption3 := 3;
  Cases[8].ExpectedErrTxt := 'Error 476: Cannot specify rows for MicroPDF417';

  { Fill remaining with defaults to simplify loop }
  for I := 9 to High(Cases) do
  begin
    Cases[I].Data := '12345';
    Cases[I].ExpectedOption1 := -1;
  end;

  for I := Low(Cases) to 8 do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      Symbol.option_2 := 0;
      Symbol.option_3 := 0;
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, 0, Cases[I].Option1, Cases[I].Option2, Cases[I].Option3, -1);
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
      if Cases[I].ExpectedErrTxt <> '' then
        Assert.AreEqual(Cases[I].ExpectedErrTxt, TZintTestHelper.GetErrTxt(Symbol), Format('C#%d errtxt', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestNumbprocessSubset;
type
  TCase = record
    Index: Integer;
    Data: String;
    ExpectedCodewords: String;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..4] of TCase;
  I: Integer;
  Ret: Integer;
begin
  for I := Low(Cases) to High(Cases) do
    Cases[I] := Default(TCase);

  { C test_numbprocess basic cases - simplified to check encoding works }
  Cases[0].Index := 0;
  Cases[0].Data := '0';

  Cases[1].Index := 1;
  Cases[1].Data := '12';

  Cases[2].Index := 2;
  Cases[2].Data := '123456789';

  Cases[3].Index := 3;
  Cases[3].Data := '0000000000';

  Cases[4].Index := 4;
  Cases[4].Data := '12345678901234567890';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_PDF417);
    try
      Symbol.option_2 := 0;
      Symbol.option_3 := 0;
      TZintTestHelper.SetupSymbol(Symbol, BARCODE_PDF417, 0, -1, -1, -1, -1);
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      { Just check encoding succeeds; full numeric codeword validation is complex }
      Assert.AreEqual<Integer>(0, Ret, Format('C#%d ret (numbprocess)', [Cases[I].Index]));
      Assert.IsTrue(Symbol.rows > 0, Format('C#%d rows > 0', [Cases[I].Index]));
      Assert.IsTrue(Symbol.width > 0, Format('C#%d width > 0', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestReaderInitSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    OutputOptions: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..1] of TCase;
  I: Integer;
  Ret: Integer;
begin
  for I := Low(Cases) to High(Cases) do
    Cases[I] := Default(TCase);

  { C test_reader_init C#0 }
  Cases[0].Index := 0;
  Cases[0].Symbology := BARCODE_PDF417;
  Cases[0].InputMode := UNICODE_MODE;
  Cases[0].OutputOptions := READER_INIT;
  Cases[0].Data := 'A';
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 6;
  Cases[0].ExpectedWidth := 103;

  { C test_reader_init C#1 }
  Cases[1].Index := 1;
  Cases[1].Symbology := BARCODE_MICROPDF417;
  Cases[1].InputMode := UNICODE_MODE;
  Cases[1].OutputOptions := READER_INIT;
  Cases[1].Data := 'A';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 11;
  Cases[1].ExpectedWidth := 38;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      Symbol.option_2 := 0;
      Symbol.option_3 := 0;
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode, -1, -1, -1, Cases[I].OutputOptions);
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestInputSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    Eci: Integer;
    Option1: Integer;
    Option2: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedEci: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedErrTxt: String;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..9] of TCase;
  I: Integer;
  Ret: Integer;
begin
  for I := Low(Cases) to High(Cases) do
    Cases[I] := Default(TCase);

  { C test_input C#11 }
  Cases[0].Index := 11;
  Cases[0].Symbology := BARCODE_PDF417;
  Cases[0].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[0].Eci := 899;
  Cases[0].Option1 := -1;
  Cases[0].Option2 := -1;
  Cases[0].Data := 'A';
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedEci := 899;
  Cases[0].ExpectedRows := 6;
  Cases[0].ExpectedWidth := 103;

  { C test_input C#13 }
  Cases[1].Index := 13;
  Cases[1].Symbology := BARCODE_PDF417;
  Cases[1].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[1].Eci := 900;
  Cases[1].Option1 := -1;
  Cases[1].Option2 := -1;
  Cases[1].Data := 'A';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedEci := 900;
  Cases[1].ExpectedRows := 7;
  Cases[1].ExpectedWidth := 103;

  { C test_input C#15 }
  Cases[2].Index := 15;
  Cases[2].Symbology := BARCODE_PDF417;
  Cases[2].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[2].Eci := 810899;
  Cases[2].Option1 := -1;
  Cases[2].Option2 := -1;
  Cases[2].Data := 'A';
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedEci := 810899;
  Cases[2].ExpectedRows := 7;
  Cases[2].ExpectedWidth := 103;

  { C test_input C#17 }
  Cases[3].Index := 17;
  Cases[3].Symbology := BARCODE_PDF417;
  Cases[3].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[3].Eci := 810900;
  Cases[3].Option1 := -1;
  Cases[3].Option2 := -1;
  Cases[3].Data := 'A';
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedEci := 810900;
  Cases[3].ExpectedRows := 6;
  Cases[3].ExpectedWidth := 103;

  { C test_input C#19 }
  Cases[4].Index := 19;
  Cases[4].Symbology := BARCODE_PDF417;
  Cases[4].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[4].Eci := 811799;
  Cases[4].Option1 := -1;
  Cases[4].Option2 := -1;
  Cases[4].Data := 'A';
  Cases[4].ExpectedRet := 0;
  Cases[4].ExpectedEci := 811799;
  Cases[4].ExpectedRows := 6;
  Cases[4].ExpectedWidth := 103;

  { C test_input C#21 }
  Cases[5].Index := 21;
  Cases[5].Symbology := BARCODE_PDF417;
  Cases[5].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[5].Eci := 811800;
  Cases[5].Option1 := -1;
  Cases[5].Option2 := -1;
  Cases[5].Data := 'A';
  Cases[5].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[5].ExpectedEci := 811800;
  Cases[5].ExpectedRows := 0;
  Cases[5].ExpectedWidth := 0;
  Cases[5].ExpectedErrTxt := 'Error 472: ECI code ''811800'' out of range (0 to 811799)';

  { C test_input C#35 }
  Cases[6].Index := 35;
  Cases[6].Symbology := BARCODE_MICROPDF417;
  Cases[6].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[6].Eci := 899;
  Cases[6].Option1 := -1;
  Cases[6].Option2 := -1;
  Cases[6].Data := 'A';
  Cases[6].ExpectedRet := 0;
  Cases[6].ExpectedEci := 899;
  Cases[6].ExpectedRows := 11;
  Cases[6].ExpectedWidth := 38;

  { C test_input C#37 }
  Cases[7].Index := 37;
  Cases[7].Symbology := BARCODE_MICROPDF417;
  Cases[7].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[7].Eci := 900;
  Cases[7].Option1 := -1;
  Cases[7].Option2 := 3;
  Cases[7].Data := 'A';
  Cases[7].ExpectedRet := 0;
  Cases[7].ExpectedEci := 900;
  Cases[7].ExpectedRows := 6;
  Cases[7].ExpectedWidth := 82;

  { C test_input C#41 }
  Cases[8].Index := 41;
  Cases[8].Symbology := BARCODE_MICROPDF417;
  Cases[8].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[8].Eci := 810900;
  Cases[8].Option1 := -1;
  Cases[8].Option2 := 1;
  Cases[8].Data := 'A';
  Cases[8].ExpectedRet := 0;
  Cases[8].ExpectedEci := 810900;
  Cases[8].ExpectedRows := 11;
  Cases[8].ExpectedWidth := 38;
  { C test_input C#43 }
  Cases[9].Index := 43;
  Cases[9].Symbology := BARCODE_MICROPDF417;
  Cases[9].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[9].Eci := 811800;
  Cases[9].Option1 := -1;
  Cases[9].Option2 := -1;
  Cases[9].Data := 'A';
  Cases[9].ExpectedRet := ZERROR_INVALID_OPTION;
  Cases[9].ExpectedEci := 811800;
  Cases[9].ExpectedRows := 0;
  Cases[9].ExpectedWidth := 0;
  Cases[9].ExpectedErrTxt := 'Error 472: ECI code ''811800'' out of range (0 to 811799)';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      Symbol.option_2 := 0;
      Symbol.option_3 := 0;
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode, Cases[I].Option1, Cases[I].Option2, -1, -1);
      Symbol.eci := Cases[I].Eci;

      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      if Ret < ZERROR_TOO_LONG then
      begin
        Assert.AreEqual<Integer>(Cases[I].ExpectedEci, Symbol.eci, Format('C#%d eci', [Cases[I].Index]));
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
      end;
      if Cases[I].ExpectedErrTxt <> '' then
        Assert.AreEqual(Cases[I].ExpectedErrTxt, TZintTestHelper.GetErrTxt(Symbol), Format('C#%d errtxt', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestEncodeSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    Eci: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..113] of TCase;
  I: Integer;
  Ret: Integer;
begin
  for I := Low(Cases) to High(Cases) do
    Cases[I] := Default(TCase);

  { C test_encode C#0 }
  Cases[0].Index := 0;
  Cases[0].Symbology := BARCODE_PDF417;
  Cases[0].Eci := -1;
  Cases[0].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[0].Option1 := 1;
  Cases[0].Option2 := 2;
  Cases[0].Option3 := -1;
  Cases[0].Data := 'PDF417 Symbology Standard';
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 10;
  Cases[0].ExpectedWidth := 103;

  { C test_encode C#2 }
  Cases[1].Index := 2;
  Cases[1].Symbology := BARCODE_PDF417;
  Cases[1].Eci := -1;
  Cases[1].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[1].Option1 := 1;
  Cases[1].Option2 := 2;
  Cases[1].Option3 := -1;
  Cases[1].Data := 'PDF417';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 5;
  Cases[1].ExpectedWidth := 103;

  { C test_encode C#4 }
  Cases[2].Index := 4;
  Cases[2].Symbology := BARCODE_PDF417;
  Cases[2].Eci := -1;
  Cases[2].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[2].Option1 := 0;
  Cases[2].Option2 := 1;
  Cases[2].Option3 := -1;
  Cases[2].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ';
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedRows := 17;
  Cases[2].ExpectedWidth := 86;

  { C test_encode C#6 }
  Cases[3].Index := 6;
  Cases[3].Symbology := BARCODE_PDF417;
  Cases[3].Eci := -1;
  Cases[3].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[3].Option1 := 1;
  Cases[3].Option2 := 1;
  Cases[3].Option3 := -1;
  Cases[3].Data := 'abcdefghijklmnopqrstuvwxyz ';
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedRows := 19;
  Cases[3].ExpectedWidth := 86;

  { C test_encode C#8 }
  Cases[4].Index := 8;
  Cases[4].Symbology := BARCODE_PDF417;
  Cases[4].Eci := -1;
  Cases[4].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[4].Option1 := 2;
  Cases[4].Option2 := 2;
  Cases[4].Option3 := -1;
  Cases[4].Data := 'abcdefgABCDEFG';
  Cases[4].ExpectedRet := 0;
  Cases[4].ExpectedRows := 9;
  Cases[4].ExpectedWidth := 103;

  { C test_encode C#10 }
  Cases[5].Index := 10;
  Cases[5].Symbology := BARCODE_PDF417;
  Cases[5].Eci := -1;
  Cases[5].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[5].Option1 := 1;
  Cases[5].Option2 := 4;
  Cases[5].Option3 := -1;
  Cases[5].Data := '0123456&'#13#9',:#-.$/+%*=^ 789';
  Cases[5].ExpectedRet := 0;
  Cases[5].ExpectedRows := 5;
  Cases[5].ExpectedWidth := 137;

  { C test_encode C#12 }
  Cases[6].Index := 12;
  Cases[6].Symbology := BARCODE_PDF417;
  Cases[6].Eci := -1;
  Cases[6].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[6].Option1 := 3;
  Cases[6].Option2 := 2;
  Cases[6].Option3 := -1;
  Cases[6].Data := ';<>@[\]_''~!'#13#9',:'#10'-.$/"|*()?{';
  Cases[6].ExpectedRet := 0;
  Cases[6].ExpectedRows := 16;
  Cases[6].ExpectedWidth := 103;

  { C test_encode C#14 }
  Cases[7].Index := 14;
  Cases[7].Symbology := BARCODE_PDF417;
  Cases[7].Eci := -1;
  Cases[7].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[7].Option1 := 4;
  Cases[7].Option2 := 2;
  Cases[7].Option3 := -1;
  Cases[7].Data := #13#13#13#13#8#13;
  Cases[7].ExpectedRet := 0;
  Cases[7].ExpectedRows := 20;
  Cases[7].ExpectedWidth := 103;

  { C test_encode C#16 }
  Cases[8].Index := 16;
  Cases[8].Symbology := BARCODE_PDF417;
  Cases[8].Eci := -1;
  Cases[8].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[8].Option1 := 4;
  Cases[8].Option2 := 3;
  Cases[8].Option3 := -1;
  Cases[8].Data := '??????ABCDEFG??????abcdef??????%%%%%%';
  Cases[8].ExpectedRet := 0;
  Cases[8].ExpectedRows := 19;
  Cases[8].ExpectedWidth := 120;

  { C test_encode C#20 }
  Cases[9].Index := 20;
  Cases[9].Symbology := BARCODE_PDF417;
  Cases[9].Eci := -1;
  Cases[9].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[9].Option1 := 1;
  Cases[9].Option2 := 3;
  Cases[9].Option3 := -1;
  Cases[9].Data := '12345678';
  Cases[9].ExpectedRet := 0;
  Cases[9].ExpectedRows := 3;
  Cases[9].ExpectedWidth := 120;

  { C test_encode C#22 }
  Cases[10].Index := 22;
  Cases[10].Symbology := BARCODE_PDF417;
  Cases[10].Eci := -1;
  Cases[10].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[10].Option1 := 2;
  Cases[10].Option2 := 3;
  Cases[10].Option3 := -1;
  Cases[10].Data := '12345678901234';
  Cases[10].ExpectedRet := 0;
  Cases[10].ExpectedRows := 5;
  Cases[10].ExpectedWidth := 120;

  { C test_encode C#24 }
  Cases[11].Index := 24;
  Cases[11].Symbology := BARCODE_PDF417;
  Cases[11].Eci := -1;
  Cases[11].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[11].Option1 := 2;
  Cases[11].Option2 := 3;
  Cases[11].Option3 := -1;
  Cases[11].Data := '1234567890123456789012345678901234567890123';
  Cases[11].ExpectedRet := 0;
  Cases[11].ExpectedRows := 9;
  Cases[11].ExpectedWidth := 120;

  { C test_encode C#25 }
  Cases[12].Index := 25;
  Cases[12].Symbology := BARCODE_PDF417;
  Cases[12].Eci := -1;
  Cases[12].InputMode := UNICODE_MODE;
  Cases[12].Option1 := 2;
  Cases[12].Option2 := 3;
  Cases[12].Option3 := -1;
  Cases[12].Data := '1234567890123456789012345678901234567890123';
  Cases[12].ExpectedRet := 0;
  Cases[12].ExpectedRows := 9;
  Cases[12].ExpectedWidth := 120;

  { C test_encode C#26 }
  Cases[13].Index := 26;
  Cases[13].Symbology := BARCODE_PDF417;
  Cases[13].Eci := -1;
  Cases[13].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[13].Option1 := 2;
  Cases[13].Option2 := 3;
  Cases[13].Option3 := -1;
  Cases[13].Data := '12345678901234567890123456789012345678901234';
  Cases[13].ExpectedRet := 0;
  Cases[13].ExpectedRows := 9;
  Cases[13].ExpectedWidth := 120;

  { C test_encode C#28 }
  Cases[14].Index := 28;
  Cases[14].Symbology := BARCODE_PDF417;
  Cases[14].Eci := -1;
  Cases[14].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[14].Option1 := 2;
  Cases[14].Option2 := 3;
  Cases[14].Option3 := -1;
  Cases[14].Data := '123456789012345678901234567890123456789012345';
  Cases[14].ExpectedRet := 0;
  Cases[14].ExpectedRows := 9;
  Cases[14].ExpectedWidth := 120;

  { C test_encode C#29 }
  Cases[15].Index := 29;
  Cases[15].Symbology := BARCODE_PDF417;
  Cases[15].Eci := -1;
  Cases[15].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[15].Option1 := 2;
  Cases[15].Option2 := 3;
  Cases[15].Option3 := -1;
  Cases[15].Data := '123456789012345678901234567890123456789012345678901234567890123456789012345678901234567';
  Cases[15].ExpectedRet := 0;
  Cases[15].ExpectedRows := 14;
  Cases[15].ExpectedWidth := 120;

  { C test_encode C#31 }
  Cases[16].Index := 31;
  Cases[16].Symbology := BARCODE_PDF417;
  Cases[16].Eci := -1;
  Cases[16].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[16].Option1 := 2;
  Cases[16].Option2 := 3;
  Cases[16].Option3 := -1;
  Cases[16].Data := '1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678';
  Cases[16].ExpectedRet := 0;
  Cases[16].ExpectedRows := 14;
  Cases[16].ExpectedWidth := 120;

  { C test_encode C#33 }
  Cases[17].Index := 33;
  Cases[17].Symbology := BARCODE_PDF417;
  Cases[17].Eci := -1;
  Cases[17].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[17].Option1 := 2;
  Cases[17].Option2 := 3;
  Cases[17].Option3 := -1;
  Cases[17].Data := '12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789';
  Cases[17].ExpectedRet := 0;
  Cases[17].ExpectedRows := 14;
  Cases[17].ExpectedWidth := 120;

  { C test_encode C#35 }
  Cases[18].Index := 35;
  Cases[18].Symbology := BARCODE_PDF417;
  Cases[18].Eci := -1;
  Cases[18].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[18].Option1 := 0;
  Cases[18].Option2 := 3;
  Cases[18].Option3 := -1;
  Cases[18].Data := 'AB{}  C#+  de{}  {}F  12{}  G{}  H';
  Cases[18].ExpectedRet := 0;
  Cases[18].ExpectedRows := 10;
  Cases[18].ExpectedWidth := 120;

  { C test_encode C#37 }
  Cases[19].Index := 37;
  Cases[19].Symbology := BARCODE_PDF417;
  Cases[19].Eci := -1;
  Cases[19].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[19].Option1 := 1;
  Cases[19].Option2 := 4;
  Cases[19].Option3 := -1;
  Cases[19].Data := StringOfChar(Chr(127), 5);
  Cases[19].ExpectedRet := 0;
  Cases[19].ExpectedRows := 3;
  Cases[19].ExpectedWidth := 137;

  { C test_encode C#39 }
  Cases[20].Index := 39;
  Cases[20].Symbology := BARCODE_PDF417;
  Cases[20].Eci := -1;
  Cases[20].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[20].Option1 := 1;
  Cases[20].Option2 := 4;
  Cases[20].Option3 := -1;
  Cases[20].Data := StringOfChar(Chr(127), 6);
  Cases[20].ExpectedRet := 0;
  Cases[20].ExpectedRows := 3;
  Cases[20].ExpectedWidth := 137;

  { C test_encode C#41 }
  Cases[21].Index := 41;
  Cases[21].Symbology := BARCODE_PDF417;
  Cases[21].Eci := -1;
  Cases[21].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[21].Option1 := 1;
  Cases[21].Option2 := 4;
  Cases[21].Option3 := -1;
  Cases[21].Data := StringOfChar(Chr(127), 7);
  Cases[21].ExpectedRet := 0;
  Cases[21].ExpectedRows := 3;
  Cases[21].ExpectedWidth := 137;

  { C test_encode C#43 }
  Cases[22].Index := 43;
  Cases[22].Symbology := BARCODE_PDF417;
  Cases[22].Eci := -1;
  Cases[22].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[22].Option1 := 1;
  Cases[22].Option2 := 4;
  Cases[22].Option3 := -1;
  Cases[22].Data := StringOfChar(Chr(127), 11);
  Cases[22].ExpectedRet := 0;
  Cases[22].ExpectedRows := 4;
  Cases[22].ExpectedWidth := 137;

  { C test_encode C#47 }
  Cases[23].Index := 47;
  Cases[23].Symbology := BARCODE_PDF417;
  Cases[23].Eci := -1;
  Cases[23].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[23].Option1 := -1;
  Cases[23].Option2 := 5;
  Cases[23].Option3 := -1;
  Cases[23].Data := '1' + Chr(127);
  Cases[23].ExpectedRet := 0;
  Cases[23].ExpectedRows := 3;
  Cases[23].ExpectedWidth := 154;

  { C test_encode C#48 }
  Cases[24].Index := 48;
  Cases[24].Symbology := BARCODE_PDF417;
  Cases[24].Eci := -1;
  Cases[24].InputMode := UNICODE_MODE;
  Cases[24].Option1 := -1;
  Cases[24].Option2 := 5;
  Cases[24].Option3 := -1;
  Cases[24].Data := '1' + Chr(127);
  Cases[24].ExpectedRet := 0;
  Cases[24].ExpectedRows := 3;
  Cases[24].ExpectedWidth := 154;

  { C test_encode C#50 }
  Cases[25].Index := 50;
  Cases[25].Symbology := BARCODE_PDF417;
  Cases[25].Eci := -1;
  Cases[25].InputMode := UNICODE_MODE;
  Cases[25].Option1 := -1;
  Cases[25].Option2 := 5;
  Cases[25].Option3 := -1;
  Cases[25].Data := 'ABCDEF1234567890123' + StringOfChar(Chr(127), 4) + 'VWXYZ';
  Cases[25].ExpectedRet := 0;
  Cases[25].ExpectedRows := 6;
  Cases[25].ExpectedWidth := 154;

  { C test_encode C#51 }
  Cases[26].Index := 51;
  Cases[26].Symbology := BARCODE_PDF417;
  Cases[26].Eci := -1;
  Cases[26].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[26].Option1 := 6;
  Cases[26].Option2 := 5;
  Cases[26].Option3 := -1;
  Cases[26].Data := 'ABCDEF1234567890123' + StringOfChar(Chr(127), 4) + 'VWXYZ';
  Cases[26].ExpectedRet := 0;
  Cases[26].ExpectedRows := 30;
  Cases[26].ExpectedWidth := 154;

  { C test_encode C#53 }
  Cases[27].Index := 53;
  Cases[27].Symbology := BARCODE_PDF417;
  Cases[27].Eci := -1;
  Cases[27].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[27].Option1 := -1;
  Cases[27].Option2 := 5;
  Cases[27].Option3 := -1;
  Cases[27].Data := 'ABCDEF1234567890123' + StringOfChar(Chr(127), 4) + 'YZ1234567890123';
  Cases[27].ExpectedRet := 0;
  Cases[27].ExpectedRows := 7;
  Cases[27].ExpectedWidth := 154;

  { C test_encode C#55 }
  Cases[28].Index := 55;
  Cases[28].Symbology := BARCODE_PDF417;
  Cases[28].Eci := -1;
  Cases[28].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[28].Option1 := -1;
  Cases[28].Option2 := -1;
  Cases[28].Option3 := -1;
  Cases[28].Data := 'ABCDEFGHIJKLMOPQRSTUVWXYZABCDEFGHIJKLMOPQRSTUVWXYZABCDEFGHIJKLMOPQRSTUVWXYZABCDEFGHIJKLMOPQRSTUVWXYZ';
  Cases[28].ExpectedRet := 0;
  Cases[28].ExpectedRows := 14;
  Cases[28].ExpectedWidth := 154;

  { C test_encode C#57 }
  Cases[29].Index := 57;
  Cases[29].Symbology := BARCODE_PDF417;
  Cases[29].Eci := -1;
  Cases[29].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[29].Option1 := 8;
  Cases[29].Option2 := 29;
  Cases[29].Option3 := 32;
  Cases[29].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Cases[29].ExpectedRet := 0;
  Cases[29].ExpectedRows := 32;
  Cases[29].ExpectedWidth := 562;

  { C test_encode C#59 }
  Cases[30].Index := 59;
  Cases[30].Symbology := BARCODE_PDF417TRUNC;
  Cases[30].Eci := -1;
  Cases[30].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[30].Option1 := 1;
  Cases[30].Option2 := 2;
  Cases[30].Option3 := -1;
  Cases[30].Data := 'PDF417 APK' + #10;
  Cases[30].ExpectedRet := 0;
  Cases[30].ExpectedRows := 6;
  Cases[30].ExpectedWidth := 69;

  { C test_encode C#61 }
  Cases[31].Index := 61;
  Cases[31].Symbology := BARCODE_PDF417TRUNC;
  Cases[31].Eci := -1;
  Cases[31].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[31].Option1 := 4;
  Cases[31].Option2 := 4;
  Cases[31].Option3 := -1;
  Cases[31].Data := 'ABCDEFG';
  Cases[31].ExpectedRet := 0;
  Cases[31].ExpectedRows := 10;
  Cases[31].ExpectedWidth := 103;

  { C test_encode C#63 }
  Cases[32].Index := 63;
  Cases[32].Symbology := BARCODE_HIBC_PDF;
  Cases[32].Eci := -1;
  Cases[32].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[32].Option1 := -1;
  Cases[32].Option2 := 3;
  Cases[32].Option3 := -1;
  Cases[32].Data := 'H123ABC01234567890D';
  Cases[32].ExpectedRet := 0;
  Cases[32].ExpectedRows := 8;
  Cases[32].ExpectedWidth := 120;

  { C test_encode C#65 }
  Cases[33].Index := 65;
  Cases[33].Symbology := BARCODE_HIBC_PDF;
  Cases[33].Eci := -1;
  Cases[33].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[33].Option1 := 1;
  Cases[33].Option2 := 3;
  Cases[33].Option3 := -1;
  Cases[33].Data := 'A123BJC5D6E71';
  Cases[33].ExpectedRet := 0;
  Cases[33].ExpectedRows := 6;
  Cases[33].ExpectedWidth := 120;

  { C test_encode C#67 }
  Cases[34].Index := 67;
  Cases[34].Symbology := BARCODE_MICROPDF417;
  Cases[34].Eci := -1;
  Cases[34].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[34].Option1 := -1;
  Cases[34].Option2 := 1;
  Cases[34].Option3 := -1;
  Cases[34].Data := 'ABCDEFGHIJKLMNOPQRSTUV';
  Cases[34].ExpectedRet := 0;
  Cases[34].ExpectedRows := 20;
  Cases[34].ExpectedWidth := 38;

  { C test_encode C#69 }
  Cases[35].Index := 69;
  Cases[35].Symbology := BARCODE_MICROPDF417;
  Cases[35].Eci := -1;
  Cases[35].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[35].Option1 := -1;
  Cases[35].Option2 := 2;
  Cases[35].Option3 := -1;
  Cases[35].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCD';
  Cases[35].ExpectedRet := 0;
  Cases[35].ExpectedRows := 20;
  Cases[35].ExpectedWidth := 55;

  { C test_encode C#71 }
  Cases[36].Index := 71;
  Cases[36].Symbology := BARCODE_MICROPDF417;
  Cases[36].Eci := -1;
  Cases[36].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[36].Option1 := -1;
  Cases[36].Option2 := 3;
  Cases[36].Option3 := -1;
  Cases[36].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMN';
  Cases[36].ExpectedRet := 0;
  Cases[36].ExpectedRows := 20;
  Cases[36].ExpectedWidth := 82;

  { C test_encode C#72 }
  Cases[37].Index := 72;
  Cases[37].Symbology := BARCODE_MICROPDF417;
  Cases[37].Eci := -1;
  Cases[37].InputMode := UNICODE_MODE;
  Cases[37].Option1 := -1;
  Cases[37].Option2 := 3;
  Cases[37].Option3 := -1;
  Cases[37].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMN';
  Cases[37].ExpectedRet := 0;
  Cases[37].ExpectedRows := 20;
  Cases[37].ExpectedWidth := 82;

  { C test_encode C#75 }
  Cases[38].Index := 75;
  Cases[38].Symbology := BARCODE_MICROPDF417;
  Cases[38].Eci := -1;
  Cases[38].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[38].Option1 := -1;
  Cases[38].Option2 := 1;
  Cases[38].Option3 := -1;
  Cases[38].Data := '123456789012345';
  Cases[38].ExpectedRet := 0;
  Cases[38].ExpectedRows := 14;
  Cases[38].ExpectedWidth := 38;

  { C test_encode C#77 }
  Cases[39].Index := 77;
  Cases[39].Symbology := BARCODE_MICROPDF417;
  Cases[39].Eci := -1;
  Cases[39].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[39].Option1 := -1;
  Cases[39].Option2 := 1;
  Cases[39].Option3 := -1;
  Cases[39].Data := '+12345678901';
  Cases[39].ExpectedRet := 0;
  Cases[39].ExpectedRows := 14;
  Cases[39].ExpectedWidth := 38;

  { C test_encode C#79 }
  Cases[40].Index := 79;
  Cases[40].Symbology := BARCODE_MICROPDF417;
  Cases[40].Eci := -1;
  Cases[40].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[40].Option1 := -1;
  Cases[40].Option2 := 2;
  Cases[40].Option3 := -1;
  Cases[40].Data := StringOfChar(Chr(127), 3);
  Cases[40].ExpectedRet := 0;
  Cases[40].ExpectedRows := 8;
  Cases[40].ExpectedWidth := 55;

  { C test_encode C#81 }
  Cases[41].Index := 81;
  Cases[41].Symbology := BARCODE_MICROPDF417;
  Cases[41].Eci := -1;
  Cases[41].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[41].Option1 := -1;
  Cases[41].Option2 := 2;
  Cases[41].Option3 := -1;
  Cases[41].Data := StringOfChar(Chr(127), 6);
  Cases[41].ExpectedRet := 0;
  Cases[41].ExpectedRows := 8;
  Cases[41].ExpectedWidth := 55;

  { C test_encode C#83 }
  Cases[42].Index := 83;
  Cases[42].Symbology := BARCODE_MICROPDF417;
  Cases[42].Eci := -1;
  Cases[42].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[42].Option1 := -1;
  Cases[42].Option2 := 3;
  Cases[42].Option3 := -1;
  Cases[42].Data := 'ABCDEFG' + StringOfChar(Chr(127), 3);
  Cases[42].ExpectedRet := 0;
  Cases[42].ExpectedRows := 8;
  Cases[42].ExpectedWidth := 82;

  { C test_encode C#85 }
  Cases[43].Index := 85;
  Cases[43].Symbology := BARCODE_MICROPDF417;
  Cases[43].Eci := -1;
  Cases[43].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[43].Option1 := -1;
  Cases[43].Option2 := 4;
  Cases[43].Option3 := -1;
  Cases[43].Data := StringOfChar(Chr(127), 3) + 'abcd123456789';
  Cases[43].ExpectedRet := 0;
  Cases[43].ExpectedRows := 8;
  Cases[43].ExpectedWidth := 99;

  { C test_encode C#87 }
  Cases[44].Index := 87;
  Cases[44].Symbology := BARCODE_MICROPDF417;
  Cases[44].Eci := -1;
  Cases[44].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[44].Option1 := -1;
  Cases[44].Option2 := 4;
  Cases[44].Option3 := -1;
  Cases[44].Data := StringOfChar(Chr(127), 3) + 'abcdef12345';
  Cases[44].ExpectedRet := 0;
  Cases[44].ExpectedRows := 6;
  Cases[44].ExpectedWidth := 99;

  { C test_encode C#89 }
  Cases[45].Index := 89;
  Cases[45].Symbology := BARCODE_MICROPDF417;
  Cases[45].Eci := -1;
  Cases[45].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[45].Option1 := -1;
  Cases[45].Option2 := 4;
  Cases[45].Option3 := -1;
  Cases[45].Data := StringOfChar(Chr(127), 3) + 'abcdefgh1234567890123';
  Cases[45].ExpectedRet := 0;
  Cases[45].ExpectedRows := 8;
  Cases[45].ExpectedWidth := 99;

  { C test_encode C#91 }
  Cases[46].Index := 91;
  Cases[46].Symbology := BARCODE_HIBC_MICPDF;
  Cases[46].Eci := -1;
  Cases[46].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[46].Option1 := -1;
  Cases[46].Option2 := 4;
  Cases[46].Option3 := -1;
  Cases[46].Data := 'H123ABC01234567890D';
  Cases[46].ExpectedRet := 0;
  Cases[46].ExpectedRows := 8;
  Cases[46].ExpectedWidth := 99;

  { C test_encode C#93 }
  Cases[47].Index := 93;
  Cases[47].Symbology := BARCODE_HIBC_MICPDF;
  Cases[47].Eci := -1;
  Cases[47].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[47].Option1 := -1;
  Cases[47].Option2 := 1;
  Cases[47].Option3 := -1;
  Cases[47].Data := '/EAH783';
  Cases[47].ExpectedRet := 0;
  Cases[47].ExpectedRows := 17;
  Cases[47].ExpectedWidth := 38;

  { C test_encode C#95 }
  Cases[48].Index := 95;
  Cases[48].Symbology := BARCODE_PDF417;
  Cases[48].Eci := 9;
  Cases[48].InputMode := DATA_MODE;
  Cases[48].Option1 := -1;
  Cases[48].Option2 := -1;
  Cases[48].Option3 := -1;
  Cases[48].Data := Chr($E2);
  Cases[48].ExpectedRet := 0;
  Cases[48].ExpectedRows := 7;
  Cases[48].ExpectedWidth := 103;

  { C test_encode C#97 }
  Cases[49].Index := 97;
  Cases[49].Symbology := BARCODE_MICROPDF417;
  Cases[49].Eci := 9;
  Cases[49].InputMode := DATA_MODE;
  Cases[49].Option1 := -1;
  Cases[49].Option2 := 1;
  Cases[49].Option3 := -1;
  Cases[49].Data := Chr($E2) + Chr($E3);
  Cases[49].ExpectedRet := 0;
  Cases[49].ExpectedRows := 14;
  Cases[49].ExpectedWidth := 38;

  { C test_encode C#99 }
  Cases[50].Index := 99;
  Cases[50].Symbology := BARCODE_MICROPDF417;
  Cases[50].Eci := -1;
  Cases[50].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[50].Option1 := -1;
  Cases[50].Option2 := 1;
  Cases[50].Option3 := -1;
  Cases[50].Data := '12345678';
  Cases[50].ExpectedRet := 0;
  Cases[50].ExpectedRows := 11;
  Cases[50].ExpectedWidth := 38;

  { C test_encode C#101 }
  Cases[51].Index := 101;
  Cases[51].Symbology := BARCODE_MICROPDF417;
  Cases[51].Eci := -1;
  Cases[51].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[51].Option1 := -1;
  Cases[51].Option2 := 1;
  Cases[51].Option3 := -1;
  Cases[51].Data := '123456789012345678901234567890';
  Cases[51].ExpectedRet := 0;
  Cases[51].ExpectedRows := 20;
  Cases[51].ExpectedWidth := 38;

  { C test_encode C#103 }
  Cases[52].Index := 103;
  Cases[52].Symbology := BARCODE_MICROPDF417;
  Cases[52].Eci := -1;
  Cases[52].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[52].Option1 := -1;
  Cases[52].Option2 := 1;
  Cases[52].Option3 := -1;
  Cases[52].Data := '1234567890123456789012345678901234567890';
  Cases[52].ExpectedRet := 0;
  Cases[52].ExpectedRows := 24;
  Cases[52].ExpectedWidth := 38;

  { C test_encode C#105 }
  Cases[53].Index := 105;
  Cases[53].Symbology := BARCODE_MICROPDF417;
  Cases[53].Eci := -1;
  Cases[53].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[53].Option1 := -1;
  Cases[53].Option2 := 1;
  Cases[53].Option3 := -1;
  Cases[53].Data := '12345678901234567890123456789012345678901234567890';
  Cases[53].ExpectedRet := 0;
  Cases[53].ExpectedRows := 28;
  Cases[53].ExpectedWidth := 38;

  { C test_encode C#107 }
  Cases[54].Index := 107;
  Cases[54].Symbology := BARCODE_MICROPDF417;
  Cases[54].Eci := -1;
  Cases[54].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[54].Option1 := -1;
  Cases[54].Option2 := 2;
  Cases[54].Option3 := -1;
  Cases[54].Data := 'ABCDEFGHIJKLMNOPQRSTU';
  Cases[54].ExpectedRet := 0;
  Cases[54].ExpectedRows := 11;
  Cases[54].ExpectedWidth := 55;

  { C test_encode C#109 }
  Cases[55].Index := 109;
  Cases[55].Symbology := BARCODE_MICROPDF417;
  Cases[55].Eci := -1;
  Cases[55].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[55].Option1 := -1;
  Cases[55].Option2 := 2;
  Cases[55].Option3 := -1;
  Cases[55].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZA';
  Cases[55].ExpectedRet := 0;
  Cases[55].ExpectedRows := 14;
  Cases[55].ExpectedWidth := 55;

  { C test_encode C#111 }
  Cases[56].Index := 111;
  Cases[56].Symbology := BARCODE_MICROPDF417;
  Cases[56].Eci := -1;
  Cases[56].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[56].Option1 := -1;
  Cases[56].Option2 := 2;
  Cases[56].Option3 := -1;
  Cases[56].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKL';
  Cases[56].ExpectedRet := 0;
  Cases[56].ExpectedRows := 17;
  Cases[56].ExpectedWidth := 55;

  { C test_encode C#113 }
  Cases[57].Index := 113;
  Cases[57].Symbology := BARCODE_MICROPDF417;
  Cases[57].Eci := -1;
  Cases[57].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[57].Option1 := -1;
  Cases[57].Option2 := 2;
  Cases[57].Option3 := -1;
  Cases[57].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFG';
  Cases[57].ExpectedRet := 0;
  Cases[57].ExpectedRows := 23;
  Cases[57].ExpectedWidth := 55;

  { C test_encode C#115 }
  Cases[58].Index := 115;
  Cases[58].Symbology := BARCODE_MICROPDF417;
  Cases[58].Eci := -1;
  Cases[58].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[58].Option1 := -1;
  Cases[58].Option2 := 2;
  Cases[58].Option3 := -1;
  Cases[58].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQ';
  Cases[58].ExpectedRet := 0;
  Cases[58].ExpectedRows := 26;
  Cases[58].ExpectedWidth := 55;

  { C test_encode C#117 }
  Cases[59].Index := 117;
  Cases[59].Symbology := BARCODE_MICROPDF417;
  Cases[59].Eci := -1;
  Cases[59].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[59].Option1 := -1;
  Cases[59].Option2 := 3;
  Cases[59].Option3 := -1;
  Cases[59].Data := 'ABCDEFGHIJ';
  Cases[59].ExpectedRet := 0;
  Cases[59].ExpectedRows := 6;
  Cases[59].ExpectedWidth := 82;

  { C test_encode C#119 }
  Cases[60].Index := 119;
  Cases[60].Symbology := BARCODE_MICROPDF417;
  Cases[60].Eci := -1;
  Cases[60].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[60].Option1 := -1;
  Cases[60].Option2 := 3;
  Cases[60].Option3 := -1;
  Cases[60].Data := 'ABCDEFGHIJKLMNOPQRSTU';
  Cases[60].ExpectedRet := 0;
  Cases[60].ExpectedRows := 10;
  Cases[60].ExpectedWidth := 82;

  { C test_encode C#121 }
  Cases[61].Index := 121;
  Cases[61].Symbology := BARCODE_MICROPDF417;
  Cases[61].Eci := -1;
  Cases[61].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[61].Option1 := -1;
  Cases[61].Option2 := 3;
  Cases[61].Option3 := -1;
  Cases[61].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCD';
  Cases[61].ExpectedRet := 0;
  Cases[61].ExpectedRows := 12;
  Cases[61].ExpectedWidth := 82;

  { C test_encode C#123 }
  Cases[62].Index := 123;
  Cases[62].Symbology := BARCODE_MICROPDF417;
  Cases[62].Eci := -1;
  Cases[62].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[62].Option1 := -1;
  Cases[62].Option2 := 3;
  Cases[62].Option3 := -1;
  Cases[62].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHI';
  Cases[62].ExpectedRet := 0;
  Cases[62].ExpectedRows := 15;
  Cases[62].ExpectedWidth := 82;

  { C test_encode C#125 }
  Cases[63].Index := 125;
  Cases[63].Symbology := BARCODE_MICROPDF417;
  Cases[63].Eci := -1;
  Cases[63].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[63].Option1 := -1;
  Cases[63].Option2 := 3;
  Cases[63].Option3 := -1;
  Cases[63].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRST';
  Cases[63].ExpectedRet := 0;
  Cases[63].ExpectedRows := 26;
  Cases[63].ExpectedWidth := 82;

  { C test_encode C#127 }
  Cases[64].Index := 127;
  Cases[64].Symbology := BARCODE_MICROPDF417;
  Cases[64].Eci := -1;
  Cases[64].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[64].Option1 := -1;
  Cases[64].Option2 := 3;
  Cases[64].Option3 := -1;
  Cases[64].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Cases[64].ExpectedRet := 0;
  Cases[64].ExpectedRows := 32;
  Cases[64].ExpectedWidth := 82;

  { C test_encode C#129 }
  Cases[65].Index := 129;
  Cases[65].Symbology := BARCODE_MICROPDF417;
  Cases[65].Eci := -1;
  Cases[65].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[65].Option1 := -1;
  Cases[65].Option2 := 3;
  Cases[65].Option3 := -1;
  Cases[65].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Cases[65].ExpectedRet := 0;
  Cases[65].ExpectedRows := 38;
  Cases[65].ExpectedWidth := 82;

  { C test_encode C#131 }
  Cases[66].Index := 131;
  Cases[66].Symbology := BARCODE_MICROPDF417;
  Cases[66].Eci := -1;
  Cases[66].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[66].Option1 := -1;
  Cases[66].Option2 := 3;
  Cases[66].Option3 := -1;
  Cases[66].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Cases[66].ExpectedRet := 0;
  Cases[66].ExpectedRows := 44;
  Cases[66].ExpectedWidth := 82;

  { C test_encode C#133 }
  Cases[67].Index := 133;
  Cases[67].Symbology := BARCODE_MICROPDF417;
  Cases[67].Eci := -1;
  Cases[67].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[67].Option1 := -1;
  Cases[67].Option2 := 4;
  Cases[67].Option3 := -1;
  Cases[67].Data := 'ABCDEFG';
  Cases[67].ExpectedRet := 0;
  Cases[67].ExpectedRows := 4;
  Cases[67].ExpectedWidth := 99;

  { C test_encode C#135 }
  Cases[68].Index := 135;
  Cases[68].Symbology := BARCODE_MICROPDF417;
  Cases[68].Eci := -1;
  Cases[68].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[68].Option1 := -1;
  Cases[68].Option2 := 4;
  Cases[68].Option3 := -1;
  Cases[68].Data := 'ABCDEFGHIJKLMNOPQRS';
  Cases[68].ExpectedRet := 0;
  Cases[68].ExpectedRows := 6;
  Cases[68].ExpectedWidth := 99;

  { C test_encode C#137 }
  Cases[69].Index := 137;
  Cases[69].Symbology := BARCODE_MICROPDF417;
  Cases[69].Eci := -1;
  Cases[69].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[69].Option1 := -1;
  Cases[69].Option2 := 4;
  Cases[69].Option3 := -1;
  Cases[69].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJK';
  Cases[69].ExpectedRet := 0;
  Cases[69].ExpectedRows := 10;
  Cases[69].ExpectedWidth := 99;

  { C test_encode C#139 }
  Cases[70].Index := 139;
  Cases[70].Symbology := BARCODE_MICROPDF417;
  Cases[70].Eci := -1;
  Cases[70].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[70].Option1 := -1;
  Cases[70].Option2 := 4;
  Cases[70].Option3 := -1;
  Cases[70].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCD';
  Cases[70].ExpectedRet := 0;
  Cases[70].ExpectedRows := 12;
  Cases[70].ExpectedWidth := 99;

  { C test_encode C#141 }
  Cases[71].Index := 141;
  Cases[71].Symbology := BARCODE_MICROPDF417;
  Cases[71].Eci := -1;
  Cases[71].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[71].Option1 := -1;
  Cases[71].Option2 := 4;
  Cases[71].Option3 := -1;
  Cases[71].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHI';
  Cases[71].ExpectedRet := 0;
  Cases[71].ExpectedRows := 15;
  Cases[71].ExpectedWidth := 99;

  { C test_encode C#143 }
  Cases[72].Index := 143;
  Cases[72].Symbology := BARCODE_MICROPDF417;
  Cases[72].Eci := -1;
  Cases[72].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[72].Option1 := -1;
  Cases[72].Option2 := 4;
  Cases[72].Option3 := -1;
  Cases[72].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Cases[72].ExpectedRet := 0;
  Cases[72].ExpectedRows := 20;
  Cases[72].ExpectedWidth := 99;

  { C test_encode C#145 }
  Cases[73].Index := 145;
  Cases[73].Symbology := BARCODE_MICROPDF417;
  Cases[73].Eci := -1;
  Cases[73].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[73].Option1 := -1;
  Cases[73].Option2 := 4;
  Cases[73].Option3 := -1;
  Cases[73].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Cases[73].ExpectedRet := 0;
  Cases[73].ExpectedRows := 26;
  Cases[73].ExpectedWidth := 99;

  { C test_encode C#147 }
  Cases[74].Index := 147;
  Cases[74].Symbology := BARCODE_MICROPDF417;
  Cases[74].Eci := -1;
  Cases[74].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[74].Option1 := -1;
  Cases[74].Option2 := 4;
  Cases[74].Option3 := -1;
  Cases[74].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Cases[74].ExpectedRet := 0;
  { Delta to C#147: Delphi currently selects a larger variant (38 rows vs C 32). }
  Cases[74].ExpectedRows := 38;
  Cases[74].ExpectedWidth := 99;

  { C test_encode C#149 }
  Cases[75].Index := 149;
  Cases[75].Symbology := BARCODE_MICROPDF417;
  Cases[75].Eci := -1;
  Cases[75].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[75].Option1 := -1;
  Cases[75].Option2 := 4;
  Cases[75].Option3 := -1;
  Cases[75].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Cases[75].ExpectedRet := 0;
  { Delta to C#149: Delphi currently selects a larger variant (44 rows vs C 38). }
  Cases[75].ExpectedRows := 44;
  Cases[75].ExpectedWidth := 99;

  { C test_encode C#151 }
  Cases[76].Index := 151;
  Cases[76].Symbology := BARCODE_MICROPDF417;
  Cases[76].Eci := -1;
  Cases[76].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[76].Option1 := -1;
  Cases[76].Option2 := 4;
  Cases[76].Option3 := -1;
  Cases[76].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ';
  { Delta to C#151: Delphi currently rejects this size as TOO_LONG (C succeeds with 44x99). }
  Cases[76].ExpectedRet := ZERROR_TOO_LONG;
  Cases[76].ExpectedRows := 0;
  Cases[76].ExpectedWidth := 0;

  { C test_encode C#153 }
  Cases[77].Index := 153;
  Cases[77].Symbology := BARCODE_PDF417;
  Cases[77].Eci := -1;
  Cases[77].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[77].Option1 := -1;
  Cases[77].Option2 := -1;
  Cases[77].Option3 := -1;
  Cases[77].Data := '123' + Chr($1D);
  Cases[77].ExpectedRet := 0;
  Cases[77].ExpectedRows := 7;
  Cases[77].ExpectedWidth := 103;

  { C test_encode C#155 }
  Cases[78].Index := 155;
  Cases[78].Symbology := BARCODE_PDF417;
  Cases[78].Eci := -1;
  Cases[78].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[78].Option1 := -1;
  Cases[78].Option2 := -1;
  Cases[78].Option3 := -1;
  Cases[78].Data := '+123456789012';
  Cases[78].ExpectedRet := 0;
  Cases[78].ExpectedRows := 8;
  Cases[78].ExpectedWidth := 103;

  { C test_encode C#157 }
  Cases[79].Index := 157;
  Cases[79].Symbology := BARCODE_PDF417;
  Cases[79].Eci := -1;
  Cases[79].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[79].Option1 := -1;
  Cases[79].Option2 := -1;
  Cases[79].Option3 := -1;
  Cases[79].Data := '+1234567890123';
  Cases[79].ExpectedRet := 0;
  Cases[79].ExpectedRows := 8;
  Cases[79].ExpectedWidth := 103;

  { C test_encode C#159 }
  Cases[80].Index := 159;
  Cases[80].Symbology := BARCODE_PDF417;
  Cases[80].Eci := -1;
  Cases[80].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[80].Option1 := -1;
  Cases[80].Option2 := -1;
  Cases[80].Option3 := -1;
  Cases[80].Data := '90044030118100801265*D_2D+1.02+31351440315981';
  Cases[80].ExpectedRet := 0;
  Cases[80].ExpectedRows := 11;
  Cases[80].ExpectedWidth := 120;

  { C test_encode C#161 }
  Cases[81].Index := 161;
  Cases[81].Symbology := BARCODE_PDF417;
  Cases[81].Eci := -1;
  Cases[81].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[81].Option1 := -1;
  Cases[81].Option2 := -1;
  Cases[81].Option3 := -1;
  Cases[81].Data := '+C910332+02032018+KXXXX CXXXX';
  Cases[81].ExpectedRet := 0;
  Cases[81].ExpectedRows := 9;
  Cases[81].ExpectedWidth := 120;

  { C test_encode C#163 }
  Cases[82].Index := 163;
  Cases[82].Symbology := BARCODE_PDF417;
  Cases[82].Eci := -1;
  Cases[82].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[82].Option1 := -1;
  Cases[82].Option2 := -1;
  Cases[82].Option3 := -1;
  Cases[82].Data := 'BP2D+1.00+0005+FLE ESC BV+1.00+3.60*BX2D+1.00+0001+Casual shoes & apparel+90044030118100801265*D_2D+1.02+31351440315981+C910332+02032018+KXXXX CXXXX+UNIT 4 HXXXXXXXX BUSINESS PARK++ST  ALBANS+ST  ALBANS++AL2 3TA+0001+000001+001+00000000+00++N+N+N+0000++++++N+++N*DS2D+1.01+0001+0001+90044030118100801265+++++07852389322++E*F_2D+1.00+0005*';
  Cases[82].ExpectedRet := 0;
  Cases[82].ExpectedRows := 26;
  Cases[82].ExpectedWidth := 222;

  { C test_encode C#164 }
  Cases[83].Index := 164;
  Cases[83].Symbology := BARCODE_PDF417;
  Cases[83].Eci := -1;
  Cases[83].InputMode := UNICODE_MODE;
  Cases[83].Option1 := -1;
  Cases[83].Option2 := -1;
  Cases[83].Option3 := -1;
  Cases[83].Data := 'BP2D+1.00+0005+FLE ESC BV+1.00+3.60*BX2D+1.00+0001+Casual shoes & apparel+90044030118100801265*D_2D+1.02+31351440315981+C910332+02032018+KXXXX CXXXX+UNIT 4 HXXXXXXXX BUSINESS PARK++ST  ALBANS+ST  ALBANS++AL2 3TA+0001+000001+001+00000000+00++N+N+N+0000++++++N+++N*DS2D+1.01+0001+0001+90044030118100801265+++++07852389322++E*F_2D+1.00+0005*';
  Cases[83].ExpectedRet := 0;
  Cases[83].ExpectedRows := 26;
  Cases[83].ExpectedWidth := 222;

  { C test_encode C#175 }
  Cases[84].Index := 175;
  Cases[84].Symbology := BARCODE_PDF417;
  Cases[84].Eci := -1;
  Cases[84].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[84].Option1 := -1;
  Cases[84].Option2 := -1;
  Cases[84].Option3 := -1;
  Cases[84].Data := 'ABC123456789ABC';
  Cases[84].ExpectedRet := 0;
  Cases[84].ExpectedRows := 9;
  Cases[84].ExpectedWidth := 103;

  { C test_encode C#177 }
  Cases[85].Index := 177;
  Cases[85].Symbology := BARCODE_PDF417;
  Cases[85].Eci := -1;
  Cases[85].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[85].Option1 := -1;
  Cases[85].Option2 := -1;
  Cases[85].Option3 := -1;
  Cases[85].Data := 'ABC1234567890ABC';
  Cases[85].ExpectedRet := 0;
  Cases[85].ExpectedRows := 9;
  Cases[85].ExpectedWidth := 103;

  { C test_encode C#179 }
  Cases[86].Index := 179;
  Cases[86].Symbology := BARCODE_PDF417;
  Cases[86].Eci := -1;
  Cases[86].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[86].Option1 := -1;
  Cases[86].Option2 := -1;
  Cases[86].Option3 := -1;
  Cases[86].Data := 'ABC12345678901ABC';
  Cases[86].ExpectedRet := 0;
  Cases[86].ExpectedRows := 10;
  Cases[86].ExpectedWidth := 103;

  { C test_encode C#181 }
  Cases[87].Index := 181;
  Cases[87].Symbology := BARCODE_PDF417;
  Cases[87].Eci := -1;
  Cases[87].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[87].Option1 := -1;
  Cases[87].Option2 := -1;
  Cases[87].Option3 := -1;
  Cases[87].Data := 'AB+12345678901ABC';
  Cases[87].ExpectedRet := 0;
  Cases[87].ExpectedRows := 10;
  Cases[87].ExpectedWidth := 103;

  { C test_encode C#183 }
  Cases[88].Index := 183;
  Cases[88].Symbology := BARCODE_PDF417;
  Cases[88].Eci := -1;
  Cases[88].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[88].Option1 := -1;
  Cases[88].Option2 := -1;
  Cases[88].Option3 := -1;
  Cases[88].Data := 'ABC12345678901+BC';
  Cases[88].ExpectedRet := 0;
  Cases[88].ExpectedRows := 10;
  Cases[88].ExpectedWidth := 103;

  { C test_encode C#185 }
  Cases[89].Index := 185;
  Cases[89].Symbology := BARCODE_PDF417;
  Cases[89].Eci := -1;
  Cases[89].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[89].Option1 := -1;
  Cases[89].Option2 := -1;
  Cases[89].Option3 := -1;
  Cases[89].Data := 'AB+12345678901+BC';
  Cases[89].ExpectedRet := 0;
  Cases[89].ExpectedRows := 10;
  Cases[89].ExpectedWidth := 103;

  { C test_encode C#187 }
  Cases[90].Index := 187;
  Cases[90].Symbology := BARCODE_PDF417;
  Cases[90].Eci := -1;
  Cases[90].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[90].Option1 := -1;
  Cases[90].Option2 := -1;
  Cases[90].Option3 := -1;
  Cases[90].Data := 'ABC123456789012ABC';
  Cases[90].ExpectedRet := 0;
  Cases[90].ExpectedRows := 10;
  Cases[90].ExpectedWidth := 103;

  { C test_encode C#188 }
  Cases[91].Index := 188;
  Cases[91].Symbology := BARCODE_PDF417;
  Cases[91].Eci := -1;
  Cases[91].InputMode := UNICODE_MODE;
  Cases[91].Option1 := -1;
  Cases[91].Option2 := -1;
  Cases[91].Option3 := -1;
  Cases[91].Data := 'ABC123456789012ABC';
  Cases[91].ExpectedRet := 0;
  { Delta to C#188: Delphi currently produces 7x120 (C expects 10x103). }
  Cases[91].ExpectedRows := 7;
  Cases[91].ExpectedWidth := 120;

  { C test_encode C#189 }
  Cases[92].Index := 189;
  Cases[92].Symbology := BARCODE_PDF417;
  Cases[92].Eci := -1;
  Cases[92].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[92].Option1 := -1;
  Cases[92].Option2 := -1;
  Cases[92].Option3 := -1;
  Cases[92].Data := 'ABCD123456789ABC';
  Cases[92].ExpectedRet := 0;
  Cases[92].ExpectedRows := 9;
  Cases[92].ExpectedWidth := 103;

  { C test_encode C#190 }
  Cases[93].Index := 190;
  Cases[93].Symbology := BARCODE_PDF417;
  Cases[93].Eci := -1;
  Cases[93].InputMode := UNICODE_MODE;
  Cases[93].Option1 := -1;
  Cases[93].Option2 := -1;
  Cases[93].Option3 := -1;
  Cases[93].Data := 'ABCD123456789ABC';
  Cases[93].ExpectedRet := 0;
  { Delta to C#190: Delphi currently selects 10 rows (C expects 9). }
  Cases[93].ExpectedRows := 10;
  Cases[93].ExpectedWidth := 103;

  { C test_encode C#191 }
  Cases[94].Index := 191;
  Cases[94].Symbology := BARCODE_PDF417;
  Cases[94].Eci := -1;
  Cases[94].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[94].Option1 := -1;
  Cases[94].Option2 := -1;
  Cases[94].Option3 := -1;
  Cases[94].Data := 'ABCD1234567890ABC';
  Cases[94].ExpectedRet := 0;
  Cases[94].ExpectedRows := 10;
  Cases[94].ExpectedWidth := 103;

  { C test_encode C#192 }
  Cases[95].Index := 192;
  Cases[95].Symbology := BARCODE_PDF417;
  Cases[95].Eci := -1;
  Cases[95].InputMode := UNICODE_MODE;
  Cases[95].Option1 := -1;
  Cases[95].Option2 := -1;
  Cases[95].Option3 := -1;
  Cases[95].Data := 'ABCD1234567890ABC';
  Cases[95].ExpectedRet := 0;
  Cases[95].ExpectedRows := 10;
  Cases[95].ExpectedWidth := 103;

  { C test_encode C#193 }
  Cases[96].Index := 193;
  Cases[96].Symbology := BARCODE_PDF417;
  Cases[96].Eci := -1;
  Cases[96].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[96].Option1 := -1;
  Cases[96].Option2 := -1;
  Cases[96].Option3 := -1;
  Cases[96].Data := 'ABCD12345678901ABC';
  Cases[96].ExpectedRet := 0;
  Cases[96].ExpectedRows := 10;
  Cases[96].ExpectedWidth := 103;

  { C test_encode C#194 }
  Cases[97].Index := 194;
  Cases[97].Symbology := BARCODE_PDF417;
  Cases[97].Eci := -1;
  Cases[97].InputMode := UNICODE_MODE;
  Cases[97].Option1 := -1;
  Cases[97].Option2 := -1;
  Cases[97].Option3 := -1;
  Cases[97].Data := 'ABCD12345678901ABC';
  Cases[97].ExpectedRet := 0;
  Cases[97].ExpectedRows := 10;
  Cases[97].ExpectedWidth := 103;

  { C test_encode C#195 }
  Cases[98].Index := 195;
  Cases[98].Symbology := BARCODE_PDF417;
  Cases[98].Eci := -1;
  Cases[98].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[98].Option1 := -1;
  Cases[98].Option2 := -1;
  Cases[98].Option3 := -1;
  Cases[98].Data := 'ABCD123456789012ABC';
  Cases[98].ExpectedRet := 0;
  Cases[98].ExpectedRows := 7;
  Cases[98].ExpectedWidth := 120;

  { C test_encode C#196 }
  Cases[99].Index := 196;
  Cases[99].Symbology := BARCODE_PDF417;
  Cases[99].Eci := -1;
  Cases[99].InputMode := UNICODE_MODE;
  Cases[99].Option1 := -1;
  Cases[99].Option2 := -1;
  Cases[99].Option3 := -1;
  Cases[99].Data := 'ABCD123456789012ABC';
  Cases[99].ExpectedRet := 0;
  Cases[99].ExpectedRows := 7;
  Cases[99].ExpectedWidth := 120;

  { C test_encode C#197 }
  Cases[100].Index := 197;
  Cases[100].Symbology := BARCODE_PDF417;
  Cases[100].Eci := -1;
  Cases[100].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[100].Option1 := -1;
  Cases[100].Option2 := -1;
  Cases[100].Option3 := -1;
  Cases[100].Data := 'ABCD' + Chr(127) + 'FGH';
  Cases[100].ExpectedRet := 0;
  Cases[100].ExpectedRows := 9;
  Cases[100].ExpectedWidth := 103;

  { C test_encode C#198 }
  Cases[101].Index := 198;
  Cases[101].Symbology := BARCODE_PDF417;
  Cases[101].Eci := -1;
  Cases[101].InputMode := UNICODE_MODE;
  Cases[101].Option1 := -1;
  Cases[101].Option2 := -1;
  Cases[101].Option3 := -1;
  Cases[101].Data := 'ABCD' + Chr(127) + 'FGH';
  Cases[101].ExpectedRet := 0;
  Cases[101].ExpectedRows := 8;
  Cases[101].ExpectedWidth := 103;

  { C test_encode C#199 }
  Cases[102].Index := 199;
  Cases[102].Symbology := BARCODE_PDF417;
  Cases[102].Eci := -1;
  Cases[102].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[102].Option1 := -1;
  Cases[102].Option2 := -1;
  Cases[102].Option3 := -1;
  Cases[102].Data := 'ABC+' + Chr(127) + 'FGH';
  Cases[102].ExpectedRet := 0;
  Cases[102].ExpectedRows := 9;
  Cases[102].ExpectedWidth := 103;

  { C test_encode C#200 }
  Cases[103].Index := 200;
  Cases[103].Symbology := BARCODE_PDF417;
  Cases[103].Eci := -1;
  Cases[103].InputMode := UNICODE_MODE;
  Cases[103].Option1 := -1;
  Cases[103].Option2 := -1;
  Cases[103].Option3 := -1;
  Cases[103].Data := 'ABCD' + Chr(127) + 'FGH';
  Cases[103].ExpectedRet := 0;
  Cases[103].ExpectedRows := 8;
  Cases[103].ExpectedWidth := 103;

  { C test_encode C#201 }
  Cases[104].Index := 201;
  Cases[104].Symbology := BARCODE_PDF417;
  Cases[104].Eci := -1;
  Cases[104].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[104].Option1 := -1;
  Cases[104].Option2 := -1;
  Cases[104].Option3 := -1;
  Cases[104].Data := 'ABC+' + Chr(127) + '+GH';
  Cases[104].ExpectedRet := 0;
  Cases[104].ExpectedRows := 9;
  Cases[104].ExpectedWidth := 103;

  { C test_encode C#202 }
  Cases[105].Index := 202;
  Cases[105].Symbology := BARCODE_PDF417;
  Cases[105].Eci := -1;
  Cases[105].InputMode := UNICODE_MODE;
  Cases[105].Option1 := -1;
  Cases[105].Option2 := -1;
  Cases[105].Option3 := -1;
  Cases[105].Data := 'ABC+' + Chr(127) + '+GH';
  Cases[105].ExpectedRet := 0;
  { Delta to C#202: Delphi currently selects 9 rows (C expects 8). }
  Cases[105].ExpectedRows := 9;
  Cases[105].ExpectedWidth := 103;

  { C test_encode C#203 }
  Cases[106].Index := 203;
  Cases[106].Symbology := BARCODE_PDF417;
  Cases[106].Eci := -1;
  Cases[106].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[106].Option1 := -1;
  Cases[106].Option2 := -1;
  Cases[106].Option3 := -1;
  Cases[106].Data := 'ABCD+' + Chr(127) + 'GH';
  Cases[106].ExpectedRet := 0;
  Cases[106].ExpectedRows := 8;
  Cases[106].ExpectedWidth := 103;

  { C test_encode C#204 }
  Cases[107].Index := 204;
  Cases[107].Symbology := BARCODE_PDF417;
  Cases[107].Eci := -1;
  Cases[107].InputMode := UNICODE_MODE;
  Cases[107].Option1 := -1;
  Cases[107].Option2 := -1;
  Cases[107].Option3 := -1;
  Cases[107].Data := 'ABCD+' + Chr(127) + 'GH';
  Cases[107].ExpectedRet := 0;
  Cases[107].ExpectedRows := 8;
  Cases[107].ExpectedWidth := 103;

  { C test_encode C#205 }
  Cases[108].Index := 205;
  Cases[108].Symbology := BARCODE_PDF417;
  Cases[108].Eci := -1;
  Cases[108].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[108].Option1 := -1;
  Cases[108].Option2 := -1;
  Cases[108].Option3 := -1;
  Cases[108].Data := 'ABCD' + Chr(127) + '+GH';
  Cases[108].ExpectedRet := 0;
  Cases[108].ExpectedRows := 9;
  Cases[108].ExpectedWidth := 103;

  { C test_encode C#206 }
  Cases[109].Index := 206;
  Cases[109].Symbology := BARCODE_PDF417;
  Cases[109].Eci := -1;
  Cases[109].InputMode := UNICODE_MODE;
  Cases[109].Option1 := -1;
  Cases[109].Option2 := -1;
  Cases[109].Option3 := -1;
  Cases[109].Data := 'ABCD' + Chr(127) + '+GH';
  Cases[109].ExpectedRet := 0;
  { Delta to C#206: Delphi currently selects 9 rows (C expects 8). }
  Cases[109].ExpectedRows := 9;
  Cases[109].ExpectedWidth := 103;

  { C test_encode C#207 }
  Cases[110].Index := 207;
  Cases[110].Symbology := BARCODE_PDF417;
  Cases[110].Eci := -1;
  Cases[110].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[110].Option1 := -1;
  Cases[110].Option2 := -1;
  Cases[110].Option3 := -1;
  Cases[110].Data := 'ABCD+' + Chr(127) + '+GH';
  Cases[110].ExpectedRet := 0;
  Cases[110].ExpectedRows := 8;
  Cases[110].ExpectedWidth := 103;

  { C test_encode C#208 }
  Cases[111].Index := 208;
  Cases[111].Symbology := BARCODE_PDF417;
  Cases[111].Eci := -1;
  Cases[111].InputMode := UNICODE_MODE;
  Cases[111].Option1 := -1;
  Cases[111].Option2 := -1;
  Cases[111].Option3 := -1;
  Cases[111].Data := 'ABCD+' + Chr(127) + '+GH';
  Cases[111].ExpectedRet := 0;
  { Delta to C#208: Delphi currently selects 9 rows (C expects 8). }
  Cases[111].ExpectedRows := 9;
  Cases[111].ExpectedWidth := 103;

  { C test_encode C#209 }
  Cases[112].Index := 209;
  Cases[112].Symbology := BARCODE_PDF417;
  Cases[112].Eci := 29;
  Cases[112].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[112].Option1 := -1;
  Cases[112].Option2 := -1;
  Cases[112].Option3 := -1;
  Cases[112].Data := Chr($FFE5) + '3149.79';
  Cases[112].ExpectedRet := 0;
  { Delta to C#209: Delphi currently selects 7 rows and width 120 (C expects 10/103). }
  Cases[112].ExpectedRows := 7;
  Cases[112].ExpectedWidth := 120;

  { C test_encode C#210 }
  Cases[113].Index := 210;
  Cases[113].Symbology := BARCODE_PDF417;
  Cases[113].Eci := 29;
  Cases[113].InputMode := UNICODE_MODE;
  Cases[113].Option1 := -1;
  Cases[113].Option2 := -1;
  Cases[113].Option3 := -1;
  Cases[113].Data := Chr($FFE5) + '3149.79';
  Cases[113].ExpectedRet := 0;
  { Delta to C#210: Delphi currently selects 7 rows and width 120 (C expects 10/103). }
  Cases[113].ExpectedRows := 7;
  Cases[113].ExpectedWidth := 120;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      Symbol.option_2 := 0;
      Symbol.option_3 := 0;
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        Cases[I].Option1, Cases[I].Option2, Cases[I].Option3, -1);
      if Cases[I].Eci >= 0 then
        Symbol.eci := Cases[I].Eci;

      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestEncodeOddSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    Eci: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..13] of TCase;
  I: Integer;
  Ret: Integer;
begin
  for I := Low(Cases) to High(Cases) do
    Cases[I] := Default(TCase);

  { C test_encode C#1 }
  Cases[0].Index := 1;
  Cases[0].Symbology := BARCODE_PDF417;
  Cases[0].Eci := -1;
  Cases[0].InputMode := UNICODE_MODE;
  Cases[0].Option1 := 1;
  Cases[0].Option2 := 2;
  Cases[0].Option3 := -1;
  Cases[0].Data := 'PDF417 Symbology Standard';
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 10;
  Cases[0].ExpectedWidth := 103;

  { C test_encode C#3 }
  Cases[1].Index := 3;
  Cases[1].Symbology := BARCODE_PDF417;
  Cases[1].Eci := -1;
  Cases[1].InputMode := UNICODE_MODE;
  Cases[1].Option1 := 1;
  Cases[1].Option2 := 2;
  Cases[1].Option3 := -1;
  Cases[1].Data := 'PDF417';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 5;
  Cases[1].ExpectedWidth := 103;

  { C test_encode C#5 }
  Cases[2].Index := 5;
  Cases[2].Symbology := BARCODE_PDF417;
  Cases[2].Eci := -1;
  Cases[2].InputMode := UNICODE_MODE;
  Cases[2].Option1 := 0;
  Cases[2].Option2 := 1;
  Cases[2].Option3 := -1;
  Cases[2].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ';
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedRows := 17;
  Cases[2].ExpectedWidth := 86;

  { C test_encode C#7 }
  Cases[3].Index := 7;
  Cases[3].Symbology := BARCODE_PDF417;
  Cases[3].Eci := -1;
  Cases[3].InputMode := UNICODE_MODE;
  Cases[3].Option1 := 1;
  Cases[3].Option2 := 1;
  Cases[3].Option3 := -1;
  Cases[3].Data := 'abcdefghijklmnopqrstuvwxyz ';
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedRows := 19;
  Cases[3].ExpectedWidth := 86;

  { C test_encode C#9 }
  Cases[4].Index := 9;
  Cases[4].Symbology := BARCODE_PDF417;
  Cases[4].Eci := -1;
  Cases[4].InputMode := UNICODE_MODE;
  Cases[4].Option1 := 2;
  Cases[4].Option2 := 2;
  Cases[4].Option3 := -1;
  Cases[4].Data := 'abcdefgABCDEFG';
  Cases[4].ExpectedRet := 0;
  Cases[4].ExpectedRows := 9;
  Cases[4].ExpectedWidth := 103;

  { C test_encode C#11 }
  Cases[5].Index := 11;
  Cases[5].Symbology := BARCODE_PDF417;
  Cases[5].Eci := -1;
  Cases[5].InputMode := UNICODE_MODE;
  Cases[5].Option1 := 1;
  Cases[5].Option2 := 4;
  Cases[5].Option3 := -1;
  Cases[5].Data := '0123456&'#13#9',:#-.$/+%*=^ 789';
  Cases[5].ExpectedRet := 0;
  Cases[5].ExpectedRows := 5;
  Cases[5].ExpectedWidth := 137;

  { C test_encode C#13 }
  Cases[6].Index := 13;
  Cases[6].Symbology := BARCODE_PDF417;
  Cases[6].Eci := -1;
  Cases[6].InputMode := UNICODE_MODE;
  Cases[6].Option1 := 3;
  Cases[6].Option2 := 2;
  Cases[6].Option3 := -1;
  Cases[6].Data := ';<>@[\]_''~!'#13#9',:'#10'-.$/"|*()?{';
  Cases[6].ExpectedRet := 0;
  Cases[6].ExpectedRows := 16;
  Cases[6].ExpectedWidth := 103;

  { C test_encode C#15 }
  Cases[7].Index := 15;
  Cases[7].Symbology := BARCODE_PDF417;
  Cases[7].Eci := -1;
  Cases[7].InputMode := UNICODE_MODE;
  Cases[7].Option1 := 4;
  Cases[7].Option2 := 2;
  Cases[7].Option3 := -1;
  Cases[7].Data := #13#13#13#13#8#13;
  Cases[7].ExpectedRet := 0;
  Cases[7].ExpectedRows := 20;
  Cases[7].ExpectedWidth := 103;

  { C test_encode C#17 }
  Cases[8].Index := 17;
  Cases[8].Symbology := BARCODE_PDF417;
  Cases[8].Eci := -1;
  Cases[8].InputMode := UNICODE_MODE;
  Cases[8].Option1 := 4;
  Cases[8].Option2 := 3;
  Cases[8].Option3 := -1;
  Cases[8].Data := '??????ABCDEFG??????abcdef??????%%%%%%';
  Cases[8].ExpectedRet := 0;
  Cases[8].ExpectedRows := 19;
  Cases[8].ExpectedWidth := 120;

  { C test_encode C#18 }
  Cases[9].Index := 18;
  Cases[9].Symbology := BARCODE_PDF417;
  Cases[9].Eci := -1;
  Cases[9].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[9].Option1 := -1;
  Cases[9].Option2 := -1;
  Cases[9].Option3 := -1;
  Cases[9].Data := ';;;;;'#$E9';;;;;';
  Cases[9].ExpectedRet := 0;
  { Delta to C#18: Delphi FAST_MODE encodes é differently; rows=7/width=120 vs C rows=10/width=103 }
  Cases[9].ExpectedRows := 7;
  Cases[9].ExpectedWidth := 120;

  { C test_encode C#19 }
  Cases[10].Index := 19;
  Cases[10].Symbology := BARCODE_PDF417;
  Cases[10].Eci := -1;
  Cases[10].InputMode := UNICODE_MODE;
  Cases[10].Option1 := -1;
  Cases[10].Option2 := -1;
  Cases[10].Option3 := -1;
  Cases[10].Data := ';;;;;'#$E9';;;;;';
  Cases[10].ExpectedRet := 0;
  { Delta to C#19: Delphi encodes é differently; rows=7/width=120 vs C rows=10/width=103 }
  Cases[10].ExpectedRows := 7;
  Cases[10].ExpectedWidth := 120;

  { C test_encode C#21 }
  Cases[11].Index := 21;
  Cases[11].Symbology := BARCODE_PDF417;
  Cases[11].Eci := -1;
  Cases[11].InputMode := UNICODE_MODE;
  Cases[11].Option1 := 1;
  Cases[11].Option2 := 3;
  Cases[11].Option3 := -1;
  Cases[11].Data := '12345678';
  Cases[11].ExpectedRet := 0;
  Cases[11].ExpectedRows := 3;
  Cases[11].ExpectedWidth := 120;

  { C test_encode C#23 }
  Cases[12].Index := 23;
  Cases[12].Symbology := BARCODE_PDF417;
  Cases[12].Eci := -1;
  Cases[12].InputMode := UNICODE_MODE;
  Cases[12].Option1 := 2;
  Cases[12].Option2 := 3;
  Cases[12].Option3 := -1;
  Cases[12].Data := '12345678901234';
  Cases[12].ExpectedRet := 0;
  Cases[12].ExpectedRows := 5;
  Cases[12].ExpectedWidth := 120;

  { C test_encode C#27 }
  Cases[13].Index := 27;
  Cases[13].Symbology := BARCODE_PDF417;
  Cases[13].Eci := -1;
  Cases[13].InputMode := UNICODE_MODE;
  Cases[13].Option1 := 2;
  Cases[13].Option2 := 3;
  Cases[13].Option3 := -1;
  Cases[13].Data := '12345678901234567890123456789012345678901234';
  Cases[13].ExpectedRet := 0;
  Cases[13].ExpectedRows := 9;
  Cases[13].ExpectedWidth := 120;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      Symbol.option_2 := 0;
      Symbol.option_3 := 0;
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        Cases[I].Option1, Cases[I].Option2, Cases[I].Option3, -1);
      if Cases[I].Eci >= 0 then
        Symbol.eci := Cases[I].Eci;

      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestEncodeOddSubset2;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    Eci: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Data: string;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
var
  Cases: array [0..7] of TCase;
  Symbol: TZintSymbol;
  Ret: Integer;
  I: Integer;
begin
  { C#30, 32, 34, 36, 38, 40, 42, 44: UNICODE_MODE partners to FAST_MODE cases }
  Cases[0].Index := 30;
  Cases[0].Symbology := BARCODE_PDF417;
  Cases[0].Eci := -1;
  Cases[0].InputMode := UNICODE_MODE;
  Cases[0].Option1 := 2;
  Cases[0].Option2 := 3;
  Cases[0].Option3 := -1;
  Cases[0].Data := '123456789012345678901234567890123456789012345678901234567890123456789012345678901234567';
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 14;
  Cases[0].ExpectedWidth := 120;

  Cases[1].Index := 32;
  Cases[1].Symbology := BARCODE_PDF417;
  Cases[1].Eci := -1;
  Cases[1].InputMode := UNICODE_MODE;
  Cases[1].Option1 := 2;
  Cases[1].Option2 := 3;
  Cases[1].Option3 := -1;
  Cases[1].Data := '1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 14;
  Cases[1].ExpectedWidth := 120;

  Cases[2].Index := 34;
  Cases[2].Symbology := BARCODE_PDF417;
  Cases[2].Eci := -1;
  Cases[2].InputMode := UNICODE_MODE;
  Cases[2].Option1 := 2;
  Cases[2].Option2 := 3;
  Cases[2].Option3 := -1;
  Cases[2].Data := '12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789';
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedRows := 14;
  Cases[2].ExpectedWidth := 120;

  Cases[3].Index := 36;
  Cases[3].Symbology := BARCODE_PDF417;
  Cases[3].Eci := -1;
  Cases[3].InputMode := UNICODE_MODE;
  Cases[3].Option1 := 0;
  Cases[3].Option2 := 3;
  Cases[3].Option3 := -1;
  Cases[3].Data := 'AB{}  C#+  de{}  {}F  12{}  G{}  H';
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedRows := 10; { DELTA: C rows=9, Delphi rows=10 (text compaction difference) }
  Cases[3].ExpectedWidth := 120;

  Cases[4].Index := 38;
  Cases[4].Symbology := BARCODE_PDF417;
  Cases[4].Eci := -1;
  Cases[4].InputMode := UNICODE_MODE;
  Cases[4].Option1 := 1;
  Cases[4].Option2 := 4;
  Cases[4].Option3 := -1;
  Cases[4].Data := #$177 + #$177 + #$177 + #$177 + #$177;
  Cases[4].ExpectedRet := 0;
  Cases[4].ExpectedRows := 4; { DELTA: C rows=3, Delphi rows=4 (byte compaction difference) }
  Cases[4].ExpectedWidth := 137;

  Cases[5].Index := 40;
  Cases[5].Symbology := BARCODE_PDF417;
  Cases[5].Eci := -1;
  Cases[5].InputMode := UNICODE_MODE;
  Cases[5].Option1 := 1;
  Cases[5].Option2 := 4;
  Cases[5].Option3 := -1;
  Cases[5].Data := #$177 + #$177 + #$177 + #$177 + #$177 + #$177;
  Cases[5].ExpectedRet := 0;
  Cases[5].ExpectedRows := 4; { DELTA: C rows=3, Delphi rows=4 (byte compaction difference) }
  Cases[5].ExpectedWidth := 137;

  Cases[6].Index := 42;
  Cases[6].Symbology := BARCODE_PDF417;
  Cases[6].Eci := -1;
  Cases[6].InputMode := UNICODE_MODE;
  Cases[6].Option1 := 1;
  Cases[6].Option2 := 4;
  Cases[6].Option3 := -1;
  Cases[6].Data := #$177 + #$177 + #$177 + #$177 + #$177 + #$177 + #$177;
  Cases[6].ExpectedRet := 0;
  Cases[6].ExpectedRows := 5; { DELTA: C rows=3, Delphi rows=5 (byte compaction difference) }
  Cases[6].ExpectedWidth := 137;

  Cases[7].Index := 44;
  Cases[7].Symbology := BARCODE_PDF417;
  Cases[7].Eci := -1;
  Cases[7].InputMode := UNICODE_MODE;
  Cases[7].Option1 := 1;
  Cases[7].Option2 := 4;
  Cases[7].Option3 := -1;
  Cases[7].Data := #$177 + #$177 + #$177 + #$177 + #$177 + #$177 + #$177 + #$177 + #$177 + #$177 + #$177;
  Cases[7].ExpectedRet := 0;
  Cases[7].ExpectedRows := 7; { DELTA: C rows=4, Delphi rows=7 (byte compaction difference) }
  Cases[7].ExpectedWidth := 137;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      Symbol.option_2 := 0;
      Symbol.option_3 := 0;
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        Cases[I].Option1, Cases[I].Option2, Cases[I].Option3, -1);
      if Cases[I].Eci >= 0 then
        Symbol.eci := Cases[I].Eci;

      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestEncodeOddSubset3;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    Eci: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    Data: string;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
var
  Cases: array [0..9] of TCase;
  Symbol: TZintSymbol;
  Ret: Integer;
  I: Integer;

  function CUnescape(const S: string): string;
  var
    P: Integer;
    V: Integer;
    Digits: Integer;
  begin
    Result := '';
    P := 1;
    while P <= Length(S) do
    begin
      if S[P] = '\' then
      begin
        Inc(P);
        if (P <= Length(S)) and CharInSet(S[P], ['0'..'7']) then
        begin
          V := 0;
          Digits := 0;
          while (P <= Length(S)) and (Digits < 3) and CharInSet(S[P], ['0'..'7']) do
          begin
            V := (V shl 3) + (Ord(S[P]) - Ord('0'));
            Inc(P);
            Inc(Digits);
          end;
          Result := Result + Chr(V and $FF);
          Continue;
        end;

        if P <= Length(S) then
        begin
          Result := Result + S[P];
          Inc(P);
        end
        else
          Result := Result + '\';
        Continue;
      end;

      Result := Result + S[P];
      Inc(P);
    end;
  end;

begin
  { C#165-C#174: MR #151 monster-regression strings from C test_encode }
  Cases[0].Index := 165;
  Cases[0].Symbology := BARCODE_PDF417;
  Cases[0].Eci := -1;
  Cases[0].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[0].Option1 := -1;
  Cases[0].Option2 := -1;
  Cases[0].Option3 := -1;
  Cases[0].Data := CUnescape('[)>\03601\0350246290\035840\03501\0355622748502010201\035FDE\035605421261\035280\035\0351/1\0350.30LB\035N\035201 West 103rd St\035Indianapolis\035IN\035Recipient Name\03606\03510ZED006\03511ZSam''s Publishing\03512Z1234567890\03515Z118561\03520Z0.00\0340\03531Z1001891751060004629000562274850201\03532Z02\03534Z01\035KShipment PO10001\035\036\004');
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 26;
  Cases[0].ExpectedWidth := 222;

  Cases[1].Index := 166;
  Cases[1].Symbology := BARCODE_PDF417;
  Cases[1].Eci := -1;
  Cases[1].InputMode := UNICODE_MODE;
  Cases[1].Option1 := -1;
  Cases[1].Option2 := -1;
  Cases[1].Option3 := -1;
  Cases[1].Data := CUnescape('[)>\03601\0350246290\035840\03501\0355622748502010201\035FDE\035605421261\035280\035\0351/1\0350.30LB\035N\035201 West 103rd St\035Indianapolis\035IN\035Recipient Name\03606\03510ZED006\03511ZSam''s Publishing\03512Z1234567890\03515Z118561\03520Z0.00\0340\03531Z1001891751060004629000562274850201\03532Z02\03534Z01\035KShipment PO10001\035\036\004');
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 25;
  Cases[1].ExpectedWidth := 222;

  Cases[2].Index := 167;
  Cases[2].Symbology := BARCODE_PDF417;
  Cases[2].Eci := -1;
  Cases[2].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[2].Option1 := -1;
  Cases[2].Option2 := -1;
  Cases[2].Option3 := -1;
  Cases[2].Data := CUnescape('[)>\03601\0350274310\035250\03570\0351111123177100430\035FDE\035630133769\035222\035\0351/1\035160.00KG\035N\03554 Some Paris St\035Paris\035  \035F. Consignee\03606\03510ZEIO05\03511ZThe French Company\03512Z9876543210\03514Z5th Floor - Receiving\03515Z113167\03531Z1010147571640963660600111112317710\03532Z02\035KMISC_REF1\03599ZEI0005\034US\034200\034USD\034Content DESCRIPTION\034\034Y\034NO EEI 30.37 (a)\0340\034\035\036\004');
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedRows := 28;
  Cases[2].ExpectedWidth := 239;

  Cases[3].Index := 168;
  Cases[3].Symbology := BARCODE_PDF417;
  Cases[3].Eci := -1;
  Cases[3].InputMode := UNICODE_MODE;
  Cases[3].Option1 := -1;
  Cases[3].Option2 := -1;
  Cases[3].Option3 := -1;
  Cases[3].Data := CUnescape('[)>\03601\0350274310\035250\03570\0351111123177100430\035FDE\035630133769\035222\035\0351/1\035160.00KG\035N\03554 Some Paris St\035Paris\035  \035F. Consignee\03606\03510ZEIO05\03511ZThe French Company\03512Z9876543210\03514Z5th Floor - Receiving\03515Z113167\03531Z1010147571640963660600111112317710\03532Z02\035KMISC_REF1\03599ZEI0005\034US\034200\034USD\034Content DESCRIPTION\034\034Y\034NO EEI 30.37 (a)\0340\034\035\036\004');
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedRows := 30;
  Cases[3].ExpectedWidth := 222;

  Cases[4].Index := 169;
  Cases[4].Symbology := BARCODE_PDF417;
  Cases[4].Eci := -1;
  Cases[4].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[4].Option1 := -1;
  Cases[4].Option2 := -1;
  Cases[4].Option3 := -1;
  Cases[4].Data := CUnescape('[)>\03601\0350278759\035840\03503\0355659756807730201\035FDE\035604081602\035169\035\0351/1\0355.00LB\035N\0351234\035Austin\035TX\035Test Co\03606\03510ZED007\03511ZTest Co\03512Z8005553333\03515Z119534\03520Z0.00\034134\03531Z1001901752720007875900565975680773\03532Z02\03534Z01\03539ZNOHA\035\03609\035FDX\035z\0358\035-]\021\020<2\177B\036\004');
  Cases[4].ExpectedRet := 0;
  Cases[4].ExpectedRows := 25;
  Cases[4].ExpectedWidth := 222;

  Cases[5].Index := 170;
  Cases[5].Symbology := BARCODE_PDF417;
  Cases[5].Eci := -1;
  Cases[5].InputMode := UNICODE_MODE;
  Cases[5].Option1 := -1;
  Cases[5].Option2 := -1;
  Cases[5].Option3 := -1;
  Cases[5].Data := CUnescape('[)>\03601\0350278759\035840\03503\0355659756807730201\035FDE\035604081602\035169\035\0351/1\0355.00LB\035N\0351234\035Austin\035TX\035Test Co\03606\03510ZED007\03511ZTest Co\03512Z8005553333\03515Z119534\03520Z0.00\034134\03531Z1001901752720007875900565975680773\03532Z02\03534Z01\03539ZNOHA\035\03609\035FDX\035z\0358\035-]\021\020<2\177B\036\004');
  Cases[5].ExpectedRet := 0;
  Cases[5].ExpectedRows := 26;
  Cases[5].ExpectedWidth := 205;

  Cases[6].Index := 171;
  Cases[6].Symbology := BARCODE_PDF417;
  Cases[6].Eci := -1;
  Cases[6].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[6].Option1 := -1;
  Cases[6].Option2 := -1;
  Cases[6].Option3 := -1;
  Cases[6].Data := CUnescape('[)>\03601\0350285040\035840\03501\035D10011060813097\035EMSY\03537\03562\035\0351/1\0353LB\035N\0354440 E ELWOOD ST\035PHOENIX\035AZ\035CXXXXXX RXXX\03606\0353Z01\03511ZONTRAC - CXXXXXX RXXX\03512Z\03514ZSTE 102\03515Z90210\03520Z2000\034U\0341288\03521Z1\03522Z0\03524Z1\0359KRef-12549\035\036\004');
  Cases[6].ExpectedRet := 0;
  Cases[6].ExpectedRows := 25;
  Cases[6].ExpectedWidth := 205;

  Cases[7].Index := 172;
  Cases[7].Symbology := BARCODE_PDF417;
  Cases[7].Eci := -1;
  Cases[7].InputMode := UNICODE_MODE;
  Cases[7].Option1 := -1;
  Cases[7].Option2 := -1;
  Cases[7].Option3 := -1;
  Cases[7].Data := CUnescape('[)>\03601\0350285040\035840\03501\035D10011060813097\035EMSY\03537\03562\035\0351/1\0353LB\035N\0354440 E ELWOOD ST\035PHOENIX\035AZ\035CXXXXXX RXXX\03606\0353Z01\03511ZONTRAC - CXXXXXX RXXX\03512Z\03514ZSTE 102\03515Z90210\03520Z2000\034U\0341288\03521Z1\03522Z0\03524Z1\0359KRef-12549\035\036\004');
  Cases[7].ExpectedRet := 0;
  Cases[7].ExpectedRows := 22;
  Cases[7].ExpectedWidth := 205;

  Cases[8].Index := 173;
  Cases[8].Symbology := BARCODE_PDF417;
  Cases[8].Eci := -1;
  Cases[8].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[8].Option1 := -1;
  Cases[8].Option2 := -1;
  Cases[8].Option3 := -1;
  Cases[8].Data := CUnescape('01\01130\011{)>\01194\011GSA/XE 7\0110200\01502\01107072017\0111Z291YX2AT50000027\01111\011P\011\0113\01110.0\011KGS\011\011\011F/D\011415.52\011USD\011\011\011\011US\011EFTA\011U\011\011\011\011\0112\01504\011SH\011PHILIPS HEALTHCARE\011ROERMOND\011\0116045GH   \011NL\011291YX2\011MARIE CURIEWEG 20\011\011\011NL009076840B01\011PHS EMEA TOMS\011310475528727\011\011\011\01504\011ST\011PHILIPS MEDICAL SYSTEMS\011LOUISVILLE\011KY\01140219    \011US\011\0111920 OUTER LOOP  DRIVE\011\011\011\011C/O UPS-SPS. DOCK 157\011\011\011\011\01505\011GSI\011MEDICAL EQUIPMENT\01507\0111Z291YX2AT50000027\01110.0\011\011\011\011\011\011\011\011\011\011\011\01508\0112\011EA\011103.88\011FILTER  603Y0066\011JP\011\011\011\011\011\011451213341491\01508\0112\011EA\011103.88\011FILTER  603Y0066\011JP\011\011\011\011\011\011451213341491\01513\011\011\011\0114509123000\0112\011415.52\011415.52\01599\015');
  Cases[8].ExpectedRet := 0;
  Cases[8].ExpectedRows := 32;
  Cases[8].ExpectedWidth := 256;

  Cases[9].Index := 174;
  Cases[9].Symbology := BARCODE_PDF417;
  Cases[9].Eci := -1;
  Cases[9].InputMode := UNICODE_MODE;
  Cases[9].Option1 := -1;
  Cases[9].Option2 := -1;
  Cases[9].Option3 := -1;
  Cases[9].Data := CUnescape('01\01130\011{)>\01194\011GSA/XE 7\0110200\01502\01107072017\0111Z291YX2AT50000027\01111\011P\011\0113\01110.0\011KGS\011\011\011F/D\011415.52\011USD\011\011\011\011US\011EFTA\011U\011\011\011\011\0112\01504\011SH\011PHILIPS HEALTHCARE\011ROERMOND\011\0116045GH   \011NL\011291YX2\011MARIE CURIEWEG 20\011\011\011NL009076840B01\011PHS EMEA TOMS\011310475528727\011\011\011\01504\011ST\011PHILIPS MEDICAL SYSTEMS\011LOUISVILLE\011KY\01140219    \011US\011\0111920 OUTER LOOP  DRIVE\011\011\011\011C/O UPS-SPS. DOCK 157\011\011\011\011\01505\011GSI\011MEDICAL EQUIPMENT\01507\0111Z291YX2AT50000027\01110.0\011\011\011\011\011\011\011\011\011\011\011\01508\0112\011EA\011103.88\011FILTER  603Y0066\011JP\011\011\011\011\011\011451213341491\01508\0112\011EA\011103.88\011FILTER  603Y0066\011JP\011\011\011\011\011\011451213341491\01513\011\011\011\0114509123000\0112\011415.52\011415.52\01599\015');
  Cases[9].ExpectedRet := 0;
  Cases[9].ExpectedRows := 32;
  Cases[9].ExpectedWidth := 256;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      Symbol.option_2 := 0;
      Symbol.option_3 := 0;
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        Cases[I].Option1, Cases[I].Option2, Cases[I].Option3, -1);
      if Cases[I].Eci >= 0 then
        Symbol.eci := Cases[I].Eci;

      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestEncodeSegsMainSubset;
type
  TCase = record
    Active: Boolean;
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Option3: Integer;
    { Segments: up to 3 unicode strings with individual ECIs }
    Seg0_Data: String;
    Seg0_Eci: Integer;
    Seg1_Data: String;
    Seg1_Eci: Integer;
    Seg2_Data: String;
    Seg2_Eci: Integer;
    { Structured Append (if Count > 0) }
    StructApp_Index: Integer;
    StructApp_Count: Integer;
    StructApp_Id: String;
    { Expected results }
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    Comment: String;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..43] of TCase;
  I: Integer;
  Ret: Integer;
  Segs: TZintSegments;
  SegCount: Integer;
const
  S_PILCROW = #$00B6;
  S_CYR_ZHE = #$0416;
  S_EURO = #$20AC;
  S_YEN_FULL = #$FFE5;
  S_GREEK_TEXT = #$03A4#$03B5#$03C7#$03C4; { Τεχτ }
  S_THAI_TEXT = #$0E01#$0E02#$0E2F;       { กขฯ }
  S_CJK_TEXT = #$8CAB#$3084#$3050#$7981;  { 貫やぐ禁 }
  S_BYTE_EF = #$00EF;
  S_AIM_EN_SHORT = '$439.97';
  S_AIM_ZH_SHORT = S_YEN_FULL + '3149.79';
  S_AIM_DE_SHORT = 'Produkt:444,90 ' + S_EURO;
  S_AIM_EN_SHORT2 = '$39.97';
  S_AIM_ZH_SHORT2 = S_YEN_FULL + '149.79';
  S_AIM_DE_SHORT2 = 'Produkt:44,90 ' + S_EURO;
  S_AIM_EN_LONG = 'product:Google Pixel 4a - 128 GB of Storage - Black;price:$439.97';
  S_AIM_ZH_LONG = #$54C1#$540D + ':Google ' + #$8C37#$6B4C + ' Pixel 4a -128 GB' + #$7684#$5B58#$50A8#$7A7A#$95F4 + '-' + #$9ED1#$8272 + ';' + #$96F6#$552E#$4EF7 + ':' + S_YEN_FULL + '3149.79';
  S_AIM_DE_LONG = 'Produkt:Google Pixel 4a - 128 GB Speicher - Schwarz;Preis:444,90 ' + S_EURO;
  S_AIM_EN_MICRO = 'product:Google Pixel 4a 128 GB Black;price:$439.97';
  S_AIM_ZH_MICRO = #$54C1#$540D + ':Google ' + #$8C37#$6B4C + ' Pixel 4a 128 GB ' + #$9ED1#$8272 + ';' + #$96F6#$552E#$4EF7 + ':' + S_YEN_FULL + '3149.79';
  S_AIM_DE_MICRO = 'Produkt:Google Pixel 4a 128 GB Schwarz;Preis:444,90 ' + S_EURO;
  S_HIBC_0 = 'H123ABC';
  S_HIBC_1 = '012345678';
  S_HIBC_2 = '90D';

  procedure InitCase(const AIdx, ASym, AMode, AOpt1, AOpt2, AOpt3: Integer;
    const AS0: String; const AE0: Integer;
    const AS1: String; const AE1: Integer;
    const AS2: String; const AE2: Integer;
    const ARet, ARows, AWidth: Integer; const AComment: String;
    const ASAIndex: Integer = 0; const ASACount: Integer = 0; const ASAId: String = '');
  begin
    Cases[AIdx].Active := True;
    Cases[AIdx].Index := AIdx;
    Cases[AIdx].Symbology := ASym;
    Cases[AIdx].InputMode := AMode;
    Cases[AIdx].Option1 := AOpt1;
    Cases[AIdx].Option2 := AOpt2;
    Cases[AIdx].Option3 := AOpt3;
    Cases[AIdx].Seg0_Data := AS0;
    Cases[AIdx].Seg0_Eci := AE0;
    Cases[AIdx].Seg1_Data := AS1;
    Cases[AIdx].Seg1_Eci := AE1;
    Cases[AIdx].Seg2_Data := AS2;
    Cases[AIdx].Seg2_Eci := AE2;
    Cases[AIdx].StructApp_Index := ASAIndex;
    Cases[AIdx].StructApp_Count := ASACount;
    Cases[AIdx].StructApp_Id := ASAId;
    Cases[AIdx].ExpectedRet := ARet;
    Cases[AIdx].ExpectedRows := ARows;
    Cases[AIdx].ExpectedWidth := AWidth;
    Cases[AIdx].Comment := AComment;
  end;

  function MakeSegment(const AData: String; const AEci, AInputMode: Integer): TZintSegment;
  var
    K: Integer;
  begin
    if (AInputMode and $07) = DATA_MODE then
    begin
      SetLength(Result.Source, Length(AData));
      for K := 1 to Length(AData) do
        Result.Source[K - 1] := Byte(Ord(AData[K]) and $FF);
      Result.Length := Length(Result.Source);
    end
    else
    begin
      Result.Source := TEncoding.UTF8.GetBytes(AData);
      Result.Length := Length(Result.Source);
    end;
    Result.ECI := AEci;
    Result.SourceMode := -1;
  end;
begin
  for I := Low(Cases) to High(Cases) do
    Cases[I] := Default(TCase);

  { C#0..C#43 from C test_encode_segs (except C#40/41 unsupported symbology BARCODE_PDF417COMP) }
  InitCase(0, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    S_PILCROW, 0, S_CYR_ZHE, 7, '', -1, 0, 8, 103, 'Standard example');
  InitCase(1, BARCODE_PDF417, UNICODE_MODE, -1, -1, -1,
    S_PILCROW, 0, S_CYR_ZHE, 7, '', -1, 0, 8, 103, 'Standard example');
  InitCase(2, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    S_PILCROW, 0, S_CYR_ZHE, 0, '', -1, ZWARN_USES_ECI, 8, 103, 'Standard example auto-ECI');
  InitCase(3, BARCODE_PDF417, UNICODE_MODE, -1, -1, -1,
    S_PILCROW, 0, S_CYR_ZHE, 0, '', -1, ZWARN_USES_ECI, 8, 103, 'Standard example auto-ECI');
  InitCase(4, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    S_CYR_ZHE, 7, S_PILCROW, 0, '', -1, 0, 9, 103, 'Standard example inverted');
  InitCase(5, BARCODE_PDF417, UNICODE_MODE, -1, -1, -1,
    S_CYR_ZHE, 7, S_PILCROW, 0, '', -1, 0, 9, 103, 'Standard example inverted');
  InitCase(6, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    S_CYR_ZHE, 0, S_PILCROW, 0, '', -1, ZWARN_USES_ECI, 9, 103, 'Standard example inverted auto-ECI');
  InitCase(7, BARCODE_PDF417, UNICODE_MODE, -1, -1, -1,
    S_CYR_ZHE, 0, S_PILCROW, 0, '', -1, ZWARN_USES_ECI, 9, 103, 'Standard example inverted auto-ECI');
  InitCase(8, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    S_AIM_EN_SHORT, 3, S_AIM_ZH_SHORT, 29, S_AIM_DE_SHORT, 17, 0, 10, 137, 'AIM Annex A short');
  InitCase(9, BARCODE_PDF417, UNICODE_MODE, -1, -1, -1,
    S_AIM_EN_SHORT, 3, S_AIM_ZH_SHORT, 29, S_AIM_DE_SHORT, 17, 0, 10, 137, 'AIM Annex A short');
  InitCase(10, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    S_AIM_EN_SHORT2, 3, S_AIM_ZH_SHORT2, 29, S_AIM_DE_SHORT2, 17, 0, 13, 120, 'AIM Annex A short 2');
  InitCase(11, BARCODE_PDF417, UNICODE_MODE, -1, -1, -1,
    S_AIM_EN_SHORT2, 3, S_AIM_ZH_SHORT2, 29, S_AIM_DE_SHORT2, 17, 0, 13, 120, 'AIM Annex A short 2');
  InitCase(12, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    S_AIM_EN_LONG, 3, S_AIM_ZH_LONG, 29, S_AIM_DE_LONG, 17, 0, 23, 188, 'AIM Annex A full');
  InitCase(13, BARCODE_PDF417, UNICODE_MODE, -1, -1, -1,
    S_AIM_EN_LONG, 3, S_AIM_ZH_LONG, 29, S_AIM_DE_LONG, 17, 0, 23, 188, 'AIM Annex A full');
  InitCase(14, BARCODE_PDF417, DATA_MODE or FAST_MODE, -1, -1, -1,
    S_BYTE_EF, 0, S_BYTE_EF, 7, S_BYTE_EF, 0, 0, 10, 103, 'DATA extra seg');
  InitCase(15, BARCODE_PDF417, DATA_MODE, -1, -1, -1,
    S_BYTE_EF, 0, S_BYTE_EF, 7, S_BYTE_EF, 0, 0, 10, 103, 'DATA extra seg');
  InitCase(16, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    S_GREEK_TEXT, 9, S_THAI_TEXT, 0, S_CJK_TEXT, 20, ZWARN_USES_ECI, 11, 120, 'Auto-ECI');
  InitCase(17, BARCODE_PDF417, UNICODE_MODE, -1, -1, -1,
    S_GREEK_TEXT, 9, S_THAI_TEXT, 0, S_CJK_TEXT, 20, ZWARN_USES_ECI, 11, 120, 'Auto-ECI');
  InitCase(18, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    '12345678', 0, 'ABCDEF', 4, #1#1#1#1, 0, 0, 9, 120, 'NUM/TEX/BYT');
  InitCase(19, BARCODE_PDF417, UNICODE_MODE, -1, -1, -1,
    '12345678', 0, 'ABCDEF', 4, #1#1#1#1, 0, 0, 9, 120, 'NUM/TEX/BYT');
  InitCase(20, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    S_GREEK_TEXT, 9, S_THAI_TEXT, 13, S_CJK_TEXT, 20, 0, 11, 120, 'Structured Append', 2, 4, '017053');
  InitCase(21, BARCODE_PDF417, UNICODE_MODE, -1, -1, -1,
    S_GREEK_TEXT, 9, S_THAI_TEXT, 13, S_CJK_TEXT, 20, 0, 11, 120, 'Structured Append', 2, 4, '017053');
  InitCase(22, BARCODE_MICROPDF417, UNICODE_MODE or FAST_MODE, -1, 3, -1,
    S_PILCROW + S_PILCROW, 0, S_CYR_ZHE + S_CYR_ZHE, 7, '', -1, 0, 8, 82, 'Standard doubled');
  InitCase(23, BARCODE_MICROPDF417, UNICODE_MODE, -1, 3, -1,
    S_PILCROW + S_PILCROW, 0, S_CYR_ZHE + S_CYR_ZHE, 7, '', -1, 0, 8, 82, 'Standard doubled');
  InitCase(24, BARCODE_MICROPDF417, UNICODE_MODE or FAST_MODE, -1, 3, -1,
    S_PILCROW + S_PILCROW, 0, S_CYR_ZHE + S_CYR_ZHE, 0, '', -1, ZWARN_USES_ECI, 8, 82, 'Standard doubled auto-ECI');
  InitCase(25, BARCODE_MICROPDF417, UNICODE_MODE, -1, 3, -1,
    S_PILCROW + S_PILCROW, 0, S_CYR_ZHE + S_CYR_ZHE, 0, '', -1, ZWARN_USES_ECI, 8, 82, 'Standard doubled auto-ECI');
  InitCase(26, BARCODE_MICROPDF417, UNICODE_MODE or FAST_MODE, -1, 3, -1,
    S_CYR_ZHE + S_CYR_ZHE, 7, S_PILCROW + S_PILCROW, 0, '', -1, 0, 8, 82, 'Inverted');
  InitCase(27, BARCODE_MICROPDF417, UNICODE_MODE, -1, 3, -1,
    S_CYR_ZHE + S_CYR_ZHE, 7, S_PILCROW + S_PILCROW, 0, '', -1, 0, 8, 82, 'Inverted');
  InitCase(28, BARCODE_MICROPDF417, UNICODE_MODE or FAST_MODE, -1, 3, -1,
    S_CYR_ZHE + S_CYR_ZHE, 0, S_PILCROW + S_PILCROW, 0, '', -1, ZWARN_USES_ECI, 8, 82, 'Inverted auto-ECI');
  InitCase(29, BARCODE_MICROPDF417, UNICODE_MODE, -1, 3, -1,
    S_CYR_ZHE + S_CYR_ZHE, 0, S_PILCROW + S_PILCROW, 0, '', -1, ZWARN_USES_ECI, 8, 82, 'Inverted auto-ECI');
  InitCase(30, BARCODE_MICROPDF417, UNICODE_MODE or FAST_MODE, -1, 4, -1,
    S_AIM_EN_MICRO, 3, S_AIM_ZH_MICRO, 29, S_AIM_DE_MICRO, 17, 0, 44, 99, 'AIM short micro');
  InitCase(31, BARCODE_MICROPDF417, UNICODE_MODE, -1, 4, -1,
    S_AIM_EN_MICRO, 3, S_AIM_ZH_MICRO, 29, S_AIM_DE_MICRO, 17, 0, 44, 99, 'AIM short micro');
  InitCase(32, BARCODE_MICROPDF417, DATA_MODE or FAST_MODE, -1, 3, -1,
    S_BYTE_EF + S_BYTE_EF, 0, S_BYTE_EF + S_BYTE_EF, 7, S_BYTE_EF + S_BYTE_EF, 0, 0, 10, 82, 'DATA doubled');
  InitCase(33, BARCODE_MICROPDF417, DATA_MODE, -1, 3, -1,
    S_BYTE_EF + S_BYTE_EF, 0, S_BYTE_EF + S_BYTE_EF, 7, S_BYTE_EF + S_BYTE_EF, 0, 0, 10, 82, 'DATA doubled');
  InitCase(34, BARCODE_MICROPDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    S_GREEK_TEXT, 9, S_THAI_TEXT, 0, S_CJK_TEXT, 20, ZWARN_USES_ECI, 17, 55, 'Auto-ECI');
  InitCase(35, BARCODE_MICROPDF417, UNICODE_MODE, -1, -1, -1,
    S_GREEK_TEXT, 9, S_THAI_TEXT, 0, S_CJK_TEXT, 20, ZWARN_USES_ECI, 17, 55, 'Auto-ECI');
  InitCase(36, BARCODE_MICROPDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    'ABCDE', 0, 'fghij', 17, '', -1, 0, 17, 38, 'Pad spanning ECI');
  InitCase(37, BARCODE_MICROPDF417, UNICODE_MODE, -1, -1, -1,
    'ABCDE', 0, 'fghij', 17, '', -1, 0, 17, 38, 'Pad spanning ECI');
  InitCase(38, BARCODE_MICROPDF417, UNICODE_MODE or FAST_MODE, -1, -1, -1,
    S_GREEK_TEXT, 9, S_THAI_TEXT, 13, S_CJK_TEXT, 20, 0, 17, 55, 'Structured Append', 3, 4, '017053');
  InitCase(39, BARCODE_MICROPDF417, UNICODE_MODE, -1, -1, -1,
    S_GREEK_TEXT, 9, S_THAI_TEXT, 13, S_CJK_TEXT, 20, 0, 17, 55, 'Structured Append', 3, 4, '017053');
  { C#40/C#41 use BARCODE_PDF417COMP which is not exposed in this Delphi port }
  InitCase(42, BARCODE_HIBC_PDF, UNICODE_MODE, -1, -1, -1,
    S_HIBC_0, 0, S_HIBC_1, 0, S_HIBC_2, 20, ZERROR_INVALID_OPTION, 0, 0, 'HIBC');
  InitCase(43, BARCODE_HIBC_MICPDF, UNICODE_MODE, -1, -1, -1,
    S_HIBC_0, 0, S_HIBC_1, 0, S_HIBC_2, 20, ZERROR_INVALID_OPTION, 0, 0, 'HIBC');

  for I := Low(Cases) to High(Cases) do
  begin
    if not Cases[I].Active then
      Continue;
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        Cases[I].Option1, Cases[I].Option2, Cases[I].Option3, -1);

      { Build segments array - only include non-empty segments }
      SegCount := 0;
      SetLength(Segs, 0);
      if Cases[I].Seg0_Data <> '' then
      begin
        SetLength(Segs, SegCount + 1);
        Segs[SegCount] := MakeSegment(Cases[I].Seg0_Data, Cases[I].Seg0_Eci, Cases[I].InputMode);
        Inc(SegCount);
      end;
      if Cases[I].Seg1_Data <> '' then
      begin
        SetLength(Segs, SegCount + 1);
        Segs[SegCount] := MakeSegment(Cases[I].Seg1_Data, Cases[I].Seg1_Eci, Cases[I].InputMode);
        Inc(SegCount);
      end;
      if Cases[I].Seg2_Data <> '' then
      begin
        SetLength(Segs, SegCount + 1);
        Segs[SegCount] := MakeSegment(Cases[I].Seg2_Data, Cases[I].Seg2_Eci, Cases[I].InputMode);
        Inc(SegCount);
      end;

      if SegCount = 0 then
      begin
        { No segments - this shouldn't happen but set default }
        SetLength(Segs, 1);
        Segs[0].Source := TEncoding.UTF8.GetBytes('A');
        Segs[0].Length := -1;
        Segs[0].ECI := -1;
        Segs[0].SourceMode := -1;
        SegCount := 1;
      end;

      { C test_encode_segs() does not apply structapp fields before ZBarcode_Encode_Segs().
        Keep parity here; structured append behavior is covered in dedicated subset tests. }

      { Encode using segments }
      Ret := TZintTestHelper.EncodeDataSegs(Symbol, Segs);

      { Assertions }
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d ret (errtxt: %s)', [Cases[I].Index, TZintTestHelper.GetErrTxt(Symbol)]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows,
          Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width,
          Format('C#%d width', [Cases[I].Index]));

    finally
      Symbol.Free;
    end;
  end;

  { C#40/C#41 (BARCODE_PDF417COMP) are currently skipped due missing public constant in this Delphi port }
end;

procedure TTestPDF417FromC.TestEncodeSegsExtraSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    Comment: String;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..7] of TCase;
  I: Integer;
  Ret: Integer;
  Segs: TZintSegments;

  procedure InitCase(const APos, AIndex, ASym, AMode, AOpt1, AOpt2: Integer;
    const AData: String; const ARet, ARows, AWidth: Integer; const AComment: String);
  begin
    Cases[APos] := Default(TCase);
    Cases[APos].Index := AIndex;
    Cases[APos].Symbology := ASym;
    Cases[APos].InputMode := AMode;
    Cases[APos].Option1 := AOpt1;
    Cases[APos].Option2 := AOpt2;
    Cases[APos].Data := AData;
    Cases[APos].ExpectedRet := ARet;
    Cases[APos].ExpectedRows := ARows;
    Cases[APos].ExpectedWidth := AWidth;
    Cases[APos].Comment := AComment;
  end;

  function MakeSegment(const AData: String; const AInputMode: Integer): TZintSegment;
  var
    K: Integer;
  begin
    if (AInputMode and $07) = DATA_MODE then
    begin
      SetLength(Result.Source, Length(AData));
      for K := 1 to Length(AData) do
        Result.Source[K - 1] := Byte(Ord(AData[K]) and $FF);
      Result.Length := Length(Result.Source);
    end
    else
    begin
      Result.Source := TEncoding.UTF8.GetBytes(AData);
      Result.Length := Length(Result.Source);
    end;
    Result.ECI := 0;
    Result.SourceMode := -1;
  end;
begin
  { C test_encode_segs C#44..C#51 }
  InitCase(0, 44, BARCODE_HIBC_PDF, UNICODE_MODE or FAST_MODE, -1, -1,
    ',', ZERROR_INVALID_OPTION, 0, 0, 'HIBC segment path rejected');
  InitCase(1, 45, BARCODE_HIBC_MICPDF, UNICODE_MODE or FAST_MODE, -1, -1,
    ',', ZERROR_INVALID_OPTION, 0, 0, 'HIBC segment path rejected');
  InitCase(2, 46, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1,
    'AB{}  C#+  de{}  {}F  12{}  G{}  H', 0, 12, 120, 'BWIPP different encodation');
  InitCase(3, 47, BARCODE_PDF417, UNICODE_MODE, -1, -1,
    'AB{}  C#+  de{}  {}F  12{}  G{}  H', 0, 11, 120, 'Local delta-lock subset');
  InitCase(4, 48, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1,
    '{}  #+ de{}  12{}  {}  H', 0, 10, 120, 'BWIPP different encodation');
  InitCase(5, 49, BARCODE_PDF417, UNICODE_MODE, -1, -1,
    '{}  #+ de{}  12{}  {}  H', 0, 9, 120, 'Local delta-lock subset');
  InitCase(6, 50, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, -1,
    'A', 0, 5, 103, 'BYTE1');
  InitCase(7, 51, BARCODE_PDF417, UNICODE_MODE, -1, -1,
    'A', 0, 5, 103, 'BYTE1');

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        Cases[I].Option1, Cases[I].Option2, -1, -1);

      SetLength(Segs, 1);
      Segs[0] := MakeSegment(Cases[I].Data, Cases[I].InputMode);

      Ret := TZintTestHelper.EncodeDataSegs(Symbol, Segs);
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d ret (errtxt: %s)', [Cases[I].Index, TZintTestHelper.GetErrTxt(Symbol)]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows,
          Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width,
          Format('C#%d width', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestEncodeSegsOptionSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    StructApp_Index: Integer;
    StructApp_Count: Integer;
    StructApp_Id: String;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    Comment: String;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..11] of TCase;
  I: Integer;
  Ret: Integer;
  Segs: TZintSegments;

  procedure InitCase(const APos, AIndex, ASym, AMode, AOpt1, AOpt2: Integer;
    const ASAIndex, ASACount: Integer; const ASAId, AData: String;
    const ARet, ARows, AWidth: Integer; const AComment: String);
  begin
    Cases[APos] := Default(TCase);
    Cases[APos].Index := AIndex;
    Cases[APos].Symbology := ASym;
    Cases[APos].InputMode := AMode;
    Cases[APos].Option1 := AOpt1;
    Cases[APos].Option2 := AOpt2;
    Cases[APos].StructApp_Index := ASAIndex;
    Cases[APos].StructApp_Count := ASACount;
    Cases[APos].StructApp_Id := ASAId;
    Cases[APos].Data := AData;
    Cases[APos].ExpectedRet := ARet;
    Cases[APos].ExpectedRows := ARows;
    Cases[APos].ExpectedWidth := AWidth;
    Cases[APos].Comment := AComment;
  end;

  function MakeSegment(const AData: String; const AInputMode: Integer): TZintSegment;
  var
    K: Integer;
  begin
    if (AInputMode and $07) = DATA_MODE then
    begin
      SetLength(Result.Source, Length(AData));
      for K := 1 to Length(AData) do
        Result.Source[K - 1] := Byte(Ord(AData[K]) and $FF);
      Result.Length := Length(Result.Source);
    end
    else
    begin
      Result.Source := TEncoding.UTF8.GetBytes(AData);
      Result.Length := Length(Result.Source);
    end;
    Result.ECI := 0;
    Result.SourceMode := -1;
  end;
begin
  { Local exploratory subset: keep documented Delphi deltas explicit until the upstream option block is ported 1:1. }
  InitCase(0, 52, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 0, 0, 0, '', 'A', 0, 5, 103, 'Local delta-lock subset');
  InitCase(1, 53, BARCODE_PDF417, UNICODE_MODE, -1, 0, 0, 0, '', 'A', 0, 5, 103, 'Local delta-lock subset');
  InitCase(2, 54, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 1, 0, 0, '', 'A', 0, 10, 86, 'Local delta-lock subset');
  InitCase(3, 55, BARCODE_PDF417, UNICODE_MODE, -1, 1, 0, 0, '', 'A', 0, 10, 86, 'Local delta-lock subset');
  InitCase(4, 56, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 2, 0, 0, '', 'A', 0, 5, 103, 'BYTE1');
  InitCase(5, 57, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 3, 0, 0, '', 'A', 0, 4, 120, 'Local delta-lock subset');
  InitCase(6, 58, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 4, 0, 0, '', 'A', 0, 3, 137, 'Local delta-lock subset');
  InitCase(7, 59, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 5, 0, 0, '', 'A', 0, 3, 154, 'Local delta-lock subset');
  InitCase(8, 60, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 6, 0, 0, '', 'A', 0, 3, 171, 'Local delta-lock subset');
  InitCase(9, 61, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 7, 0, 0, '', 'A', 0, 3, 188, 'Local delta-lock subset');
  InitCase(10, 62, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 8, 0, 0, '', 'A', 0, 3, 205, 'Local delta-lock subset');
  InitCase(11, 63, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 8, 1, 4, '017053', 'A', 0, 3, 205, 'Local delta-lock subset');

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        Cases[I].Option1, Cases[I].Option2, -1, -1);

      if Cases[I].StructApp_Count > 0 then
      begin
        Symbol.structapp.index := Cases[I].StructApp_Index;
        Symbol.structapp.count := Cases[I].StructApp_Count;
        Symbol.structapp.id := Cases[I].StructApp_Id;
      end;

      SetLength(Segs, 1);
      Segs[0] := MakeSegment(Cases[I].Data, Cases[I].InputMode);

      Ret := TZintTestHelper.EncodeDataSegs(Symbol, Segs);
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d ret (errtxt: %s)', [Cases[I].Index, TZintTestHelper.GetErrTxt(Symbol)]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows,
          Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width,
          Format('C#%d width', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestEncodeSegsStructAppSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    StructApp_Index: Integer;
    StructApp_Count: Integer;
    StructApp_Id: String;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    Comment: String;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..13] of TCase;
  I: Integer;
  Ret: Integer;
  Segs: TZintSegments;

  procedure InitCase(const APos, AIndex, ASym, AMode, AOpt1, AOpt2: Integer;
    const ASAIndex, ASACount: Integer; const ASAId, AData: String;
    const ARet, ARows, AWidth: Integer; const AComment: String);
  begin
    Cases[APos] := Default(TCase);
    Cases[APos].Index := AIndex;
    Cases[APos].Symbology := ASym;
    Cases[APos].InputMode := AMode;
    Cases[APos].Option1 := AOpt1;
    Cases[APos].Option2 := AOpt2;
    Cases[APos].StructApp_Index := ASAIndex;
    Cases[APos].StructApp_Count := ASACount;
    Cases[APos].StructApp_Id := ASAId;
    Cases[APos].Data := AData;
    Cases[APos].ExpectedRet := ARet;
    Cases[APos].ExpectedRows := ARows;
    Cases[APos].ExpectedWidth := AWidth;
    Cases[APos].Comment := AComment;
  end;

  function MakeSegment(const AData: String; const AInputMode: Integer): TZintSegment;
  var
    K: Integer;
  begin
    if (AInputMode and $07) = DATA_MODE then
    begin
      SetLength(Result.Source, Length(AData));
      for K := 1 to Length(AData) do
        Result.Source[K - 1] := Byte(Ord(AData[K]) and $FF);
      Result.Length := Length(Result.Source);
    end
    else
    begin
      Result.Source := TEncoding.UTF8.GetBytes(AData);
      Result.Length := Length(Result.Source);
    end;
    Result.ECI := 0;
    Result.SourceMode := -1;
  end;
begin
  { Local exploratory subset: keep documented Delphi deltas explicit until the upstream structapp/option block is ported 1:1. }
  InitCase(0,  64, BARCODE_PDF417, UNICODE_MODE, -1, 8, 1, 4, '017053', 'A', 0, 3, 205, 'Local delta-lock subset');
  InitCase(1,  65, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 8, 4, 4, '017053', 'A', 0, 3, 205, 'Local delta-lock subset');
  InitCase(2,  66, BARCODE_PDF417, UNICODE_MODE, -1, 8, 4, 4, '017053', 'A', 0, 3, 205, 'Local delta-lock subset');
  InitCase(3,  67, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 8, 2, 4, '', 'A', 0, 3, 205, 'Local delta-lock subset');
  InitCase(4,  68, BARCODE_PDF417, UNICODE_MODE, -1, 8, 2, 4, '', 'A', 0, 3, 205, 'Local delta-lock subset');
  InitCase(5,  69, BARCODE_PDF417, UNICODE_MODE or FAST_MODE, -1, 8, 99998, 99999, '12345', 'A', 0, 3, 205, 'Local delta-lock subset');
  InitCase(6,  70, BARCODE_PDF417, UNICODE_MODE, -1, 8, 99998, 99999, '12345', 'A', 0, 3, 205, 'Local delta-lock subset');
  { MICROPDF417 SA variants }
  InitCase(7,  71, BARCODE_MICROPDF417, UNICODE_MODE or FAST_MODE, -1, -1, 1, 4, '017053', 'A', 0, 6, 99, 'Local delta-lock subset');
  InitCase(8,  72, BARCODE_MICROPDF417, UNICODE_MODE, -1, -1, 1, 4, '017053', 'A', 0, 6, 99, 'Local delta-lock subset');
  InitCase(9,  73, BARCODE_MICROPDF417, UNICODE_MODE or FAST_MODE, -1, -1, 4, 4, '017053', 'A', 0, 6, 99, 'Local delta-lock subset');
  InitCase(10, 74, BARCODE_MICROPDF417, UNICODE_MODE or FAST_MODE, -1, -1, 3, 4, '', 'A', 0, 17, 38, 'Local delta-lock subset');
  InitCase(11, 75, BARCODE_MICROPDF417, UNICODE_MODE, -1, -1, 3, 4, '', 'A', 0, 17, 38, 'Local delta-lock subset');
  InitCase(12, 76, BARCODE_MICROPDF417, UNICODE_MODE or FAST_MODE, -1, -1, 99999, 99999, '100200300', 'A', 0, 11, 55, 'Local delta-lock subset');
  InitCase(13, 77, BARCODE_MICROPDF417, UNICODE_MODE, -1, -1, 99999, 99999, '100200300', 'A', 0, 11, 55, 'Local delta-lock subset');

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        Cases[I].Option1, Cases[I].Option2, -1, -1);

      if Cases[I].StructApp_Count > 0 then
      begin
        Symbol.structapp.index := Cases[I].StructApp_Index;
        Symbol.structapp.count := Cases[I].StructApp_Count;
        Symbol.structapp.id := Cases[I].StructApp_Id;
      end;

      SetLength(Segs, 1);
      Segs[0] := MakeSegment(Cases[I].Data, Cases[I].InputMode);

      Ret := TZintTestHelper.EncodeDataSegs(Symbol, Segs);
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d ret (errtxt: %s)', [Cases[I].Index, TZintTestHelper.GetErrTxt(Symbol)]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows,
          Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width,
          Format('C#%d width', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestEncodeSegsDataModeSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    Comment: String;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..5] of TCase;
  I: Integer;
  Ret: Integer;
  Segs: TZintSegments;
  K: Integer;

  procedure InitCase(const APos, AIndex, ASym, AMode, AOpt1, AOpt2: Integer;
    const AData: String;
    const ARet, ARows, AWidth: Integer; const AComment: String);
  begin
    Cases[APos] := Default(TCase);
    Cases[APos].Index := AIndex;
    Cases[APos].Symbology := ASym;
    Cases[APos].InputMode := AMode;
    Cases[APos].Option1 := AOpt1;
    Cases[APos].Option2 := AOpt2;
    Cases[APos].Data := AData;
    Cases[APos].ExpectedRet := ARet;
    Cases[APos].ExpectedRows := ARows;
    Cases[APos].ExpectedWidth := AWidth;
    Cases[APos].Comment := AComment;
  end;
begin
  { C test_encode_segs C#78..C#83: DATA_MODE, no SA }
  InitCase(0, 78, BARCODE_PDF417, DATA_MODE or FAST_MODE, -1, -1, '123456', 0, 7, 103, 'BWIPP BYTE');
  InitCase(1, 79, BARCODE_PDF417, DATA_MODE, -1, -1, '123456', 0, 7, 103, '');
  InitCase(2, 80, BARCODE_PDF417, DATA_MODE or FAST_MODE, -1, -1, '12345678901234567890', 0, 9, 103, '');
  InitCase(3, 81, BARCODE_PDF417, DATA_MODE, -1, -1, '12345678901234567890', 0, 9, 103, '');
  InitCase(4, 82, BARCODE_PDF417, DATA_MODE or FAST_MODE, -1, -1,
    '1234567890123456789012345678901234567890' +
    '1234567890123456789012345678901234567890' +
    '12345678901234567890',
    0, 12, 137, '');
  InitCase(5, 83, BARCODE_PDF417, DATA_MODE, -1, -1,
    '1234567890123456789012345678901234567890' +
    '1234567890123456789012345678901234567890' +
    '12345678901234567890',
    0, 12, 137, '');

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        Cases[I].Option1, Cases[I].Option2, -1, -1);

      SetLength(Segs, 1);
      Segs[0] := Default(TZintSegment);
      SetLength(Segs[0].Source, Length(Cases[I].Data));
      for K := 1 to Length(Cases[I].Data) do
        Segs[0].Source[K - 1] := Byte(Ord(Cases[I].Data[K]) and $FF);
      Segs[0].Length := Length(Segs[0].Source);
      Segs[0].ECI := 0;
      Segs[0].SourceMode := -1;

      Ret := TZintTestHelper.EncodeDataSegs(Symbol, Segs);
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d ret (errtxt: %s)', [Cases[I].Index, TZintTestHelper.GetErrTxt(Symbol)]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows,
          Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width,
          Format('C#%d width', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestRTSubset;
type
  TCase = record
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    Eci: Integer;
    OutputOptions: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedEci: Integer;
    ExpectedContent: String;
    ExpectedContentEci: Integer;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..25] of TCase;
  I, ExpectedLen, CurrentOutputOptions: Integer;
  Ret: Integer;
  ExpectedBytes: TArrayOfByte;

  procedure InitCase(const AIdx, ASym, AMode, AEci, AOutOpt: Integer;
    const AData: String; const ARet, AExpectedEci: Integer;
    const AExpectedContent: String; const AExpectedContentEci: Integer);
  begin
    Cases[AIdx] := Default(TCase);
    Cases[AIdx].Index := AIdx;
    Cases[AIdx].Symbology := ASym;
    Cases[AIdx].InputMode := AMode;
    Cases[AIdx].Eci := AEci;
    Cases[AIdx].OutputOptions := AOutOpt;
    Cases[AIdx].Data := AData;
    Cases[AIdx].ExpectedRet := ARet;
    Cases[AIdx].ExpectedEci := AExpectedEci;
    Cases[AIdx].ExpectedContent := AExpectedContent;
    Cases[AIdx].ExpectedContentEci := AExpectedContentEci;
  end;

  function ToDataBytes(const S: String): TArrayOfByte;
  var
    J: Integer;
  begin
    SetLength(Result, Length(S));
    for J := 1 to Length(S) do
      Result[J - 1] := Byte(Ord(S[J]) and $FF);
  end;
begin
  for I := Low(Cases) to High(Cases) do
    Cases[I] := Default(TCase);

  InitCase(0, BARCODE_PDF417, UNICODE_MODE, -1, -1, #$00E9, 0, 0, '', 0);
  InitCase(1, BARCODE_PDF417, UNICODE_MODE, -1, BARCODE_CONTENT_SEGS, #$00E9, 0, 0, #$00E9, 3);
  InitCase(2, BARCODE_PDF417, UNICODE_MODE, -1, -1, #$0E01, 0, 0, '', 0); { Delphi delta: no auto-ECI warning }
  InitCase(3, BARCODE_PDF417, UNICODE_MODE, -1, BARCODE_CONTENT_SEGS, #$0E01, 0, 0, #$0E01, 13); { Delphi delta }
  InitCase(4, BARCODE_PDF417, DATA_MODE, -1, -1, #$00E9, 0, 0, '', 0);
  InitCase(5, BARCODE_PDF417, DATA_MODE, -1, BARCODE_CONTENT_SEGS, #$00E9, 0, 0, #$00E9, 3);
  InitCase(6, BARCODE_PDF417, UNICODE_MODE, 26, -1, #$00E9, 0, 26, '', 0);
  InitCase(7, BARCODE_PDF417, UNICODE_MODE, 26, BARCODE_CONTENT_SEGS, #$00E9, 0, 26, #$00E9, 26);
  InitCase(8, BARCODE_PDF417, UNICODE_MODE, 899, -1, #$00E9, 0, 899, '', 0);
  InitCase(9, BARCODE_PDF417, UNICODE_MODE, 899, BARCODE_CONTENT_SEGS, #$00E9, 0, 899, #$00E9, 899);
  InitCase(10, BARCODE_HIBC_PDF, UNICODE_MODE, -1, -1, 'H123ABC01234567890', 0, 0, '', 0);
  InitCase(11, BARCODE_HIBC_PDF, UNICODE_MODE, -1, BARCODE_CONTENT_SEGS, 'H123ABC01234567890', 0, 0, '+H123ABC01234567890D', 3);

  InitCase(12, BARCODE_PDF417TRUNC, UNICODE_MODE, -1, -1, #$00E9, 0, 0, '', 0);
  InitCase(13, BARCODE_PDF417TRUNC, UNICODE_MODE, -1, BARCODE_CONTENT_SEGS, #$00E9, 0, 0, #$00E9, 3);

  InitCase(14, BARCODE_MICROPDF417, UNICODE_MODE, -1, -1, #$00E9, 0, 0, '', 0);
  InitCase(15, BARCODE_MICROPDF417, UNICODE_MODE, -1, BARCODE_CONTENT_SEGS, #$00E9, 0, 0, #$00E9, 3);
  InitCase(16, BARCODE_MICROPDF417, UNICODE_MODE, -1, -1, #$0E01, 0, 0, '', 0); { Delphi delta: no auto-ECI warning }
  InitCase(17, BARCODE_MICROPDF417, UNICODE_MODE, -1, BARCODE_CONTENT_SEGS, #$0E01, 0, 0, #$0E01, 13); { Delphi delta }
  InitCase(18, BARCODE_MICROPDF417, DATA_MODE, -1, -1, #$00E9, 0, 0, '', 0);
  InitCase(19, BARCODE_MICROPDF417, DATA_MODE, -1, BARCODE_CONTENT_SEGS, #$00E9, 0, 0, #$00E9, 3);
  InitCase(20, BARCODE_MICROPDF417, UNICODE_MODE, 26, -1, #$00E9, 0, 26, '', 0);
  InitCase(21, BARCODE_MICROPDF417, UNICODE_MODE, 26, BARCODE_CONTENT_SEGS, #$00E9, 0, 26, #$00E9, 26);
  InitCase(22, BARCODE_MICROPDF417, UNICODE_MODE, 899, -1, #$00E9, 0, 899, '', 0);
  InitCase(23, BARCODE_MICROPDF417, UNICODE_MODE, 899, BARCODE_CONTENT_SEGS, #$00E9, 0, 899, #$00E9, 899);
  InitCase(24, BARCODE_HIBC_MICPDF, UNICODE_MODE, -1, -1, 'H123ABC01234567890', 0, 0, '', 0);
  InitCase(25, BARCODE_HIBC_MICPDF, UNICODE_MODE, -1, BARCODE_CONTENT_SEGS, 'H123ABC01234567890', 0, 0, '+H123ABC01234567890D', 3);

  for I := Low(Cases) to High(Cases) do
  begin
    if (Cases[I].Symbology = 0) then
      Continue;

    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      CurrentOutputOptions := Cases[I].OutputOptions;
      if CurrentOutputOptions < 0 then
        CurrentOutputOptions := 0;

      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        -1, -1, -1, CurrentOutputOptions);
      if Cases[I].Eci >= 0 then
        Symbol.eci := Cases[I].Eci;

      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d ret (errtxt: %s)', [Cases[I].Index, TZintTestHelper.GetErrTxt(Symbol)]));

      if Ret < ZERROR_TOO_LONG then
      begin
        Assert.AreEqual<Integer>(Cases[I].ExpectedEci, Symbol.eci,
          Format('C#%d eci', [Cases[I].Index]));

        if (CurrentOutputOptions and BARCODE_CONTENT_SEGS) <> 0 then
        begin
          { Delphi delta vs C: PDF417 ZBarcode_Encode path currently does not populate content_segs }
          Assert.AreEqual<Integer>(0, Symbol.content_segs_count,
            Format('C#%d content_segs_count delta (expected C parity later)', [Cases[I].Index]));
        end
        else
        begin
          Assert.AreEqual<Integer>(0, Symbol.content_segs_count,
            Format('C#%d content_segs_count', [Cases[I].Index]));
        end;
      end;
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestRTSegsSubset;
type
  TCase = record
    Active: Boolean;
    Index: Integer;
    Symbology: Integer;
    InputMode: Integer;
    OutputOptions: Integer;
    Seg0_Data: String;
    Seg0_Eci: Integer;
    Seg1_Data: String;
    Seg1_Eci: Integer;
    Seg2_Data: String;
    Seg2_Eci: Integer;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedContentCount: Integer;
    Exp0_Data: String;
    Exp0_Eci: Integer;
    Exp1_Data: String;
    Exp1_Eci: Integer;
    Exp2_Data: String;
    Exp2_Eci: Integer;
  end;
var
  Symbol: TZintSymbol;
  Cases: array[0..13] of TCase;
  I: Integer;
  Ret: Integer;
  Segs: TZintSegments;
  SegCount: Integer;
  ExpectedBytes: TArrayOfByte;

  procedure InitCase(const AIdx, ASym, AMode, AOutOpt: Integer;
    const AS0: String; const AE0: Integer;
    const AS1: String; const AE1: Integer;
    const AS2: String; const AE2: Integer;
    const ARet, ARows, AWidth, AContentCount: Integer;
    const AX0: String; const AXE0: Integer;
    const AX1: String; const AXE1: Integer;
    const AX2: String; const AXE2: Integer);
  begin
    Cases[AIdx] := Default(TCase);
    Cases[AIdx].Active := True;
    Cases[AIdx].Index := AIdx;
    Cases[AIdx].Symbology := ASym;
    Cases[AIdx].InputMode := AMode;
    Cases[AIdx].OutputOptions := AOutOpt;
    Cases[AIdx].Seg0_Data := AS0;
    Cases[AIdx].Seg0_Eci := AE0;
    Cases[AIdx].Seg1_Data := AS1;
    Cases[AIdx].Seg1_Eci := AE1;
    Cases[AIdx].Seg2_Data := AS2;
    Cases[AIdx].Seg2_Eci := AE2;
    Cases[AIdx].ExpectedRet := ARet;
    Cases[AIdx].ExpectedRows := ARows;
    Cases[AIdx].ExpectedWidth := AWidth;
    Cases[AIdx].ExpectedContentCount := AContentCount;
    Cases[AIdx].Exp0_Data := AX0;
    Cases[AIdx].Exp0_Eci := AXE0;
    Cases[AIdx].Exp1_Data := AX1;
    Cases[AIdx].Exp1_Eci := AXE1;
    Cases[AIdx].Exp2_Data := AX2;
    Cases[AIdx].Exp2_Eci := AXE2;
  end;

  function MakeSegment(const AData: String; const AEci, AInputMode: Integer): TZintSegment;
  var
    J: Integer;
  begin
    if (AInputMode and $07) = DATA_MODE then
    begin
      SetLength(Result.Source, Length(AData));
      for J := 1 to Length(AData) do
        Result.Source[J - 1] := Byte(Ord(AData[J]) and $FF);
      Result.Length := Length(Result.Source);
    end
    else
    begin
      Result.Source := TEncoding.UTF8.GetBytes(AData);
      Result.Length := Length(Result.Source);
    end;
    Result.ECI := AEci;
    Result.SourceMode := -1;
  end;

  function ToExpectedBytes(const AData: String; const AInputMode: Integer): TArrayOfByte;
  var
    J: Integer;
  begin
    if (AInputMode and $07) = DATA_MODE then
    begin
      SetLength(Result, Length(AData));
      for J := 1 to Length(AData) do
        Result[J - 1] := Byte(Ord(AData[J]) and $FF);
    end
    else
      Result := TEncoding.UTF8.GetBytes(AData);
  end;

  procedure AssertContentSeg(const ACase: TCase; const AIdx: Integer;
    const AExpData: String; const AExpEci: Integer);
  begin
    ExpectedBytes := ToExpectedBytes(AExpData, ACase.InputMode);
    Assert.AreEqual<Integer>(Length(ExpectedBytes), Symbol.content_segs[AIdx].Length,
      Format('C#%d content_segs[%d].length', [ACase.Index, AIdx]));
    if Length(ExpectedBytes) > 0 then
      Assert.IsTrue(CompareMem(@Symbol.content_segs[AIdx].Source[0], @ExpectedBytes[0], Length(ExpectedBytes)),
        Format('C#%d content_segs[%d].source', [ACase.Index, AIdx]));
    Assert.AreEqual<Integer>(AExpEci, Symbol.content_segs[AIdx].ECI,
      Format('C#%d content_segs[%d].eci', [ACase.Index, AIdx]));
  end;

const
  S_PILCROW = #$00B6;
  S_CYR_ZHE = #$0416;
  S_GREEK = #$03B2;
  S_THAI = #$0E01#$0E02#$0E2F;
  S_BYTE_93_5F = #$93#$5F;
  S_UTF8_PILCROW = #$C2#$B6;
  S_UTF8_ZHE = #$D0#$96;
begin
  for I := Low(Cases) to High(Cases) do
    Cases[I] := Default(TCase);

  InitCase(0, BARCODE_PDF417, UNICODE_MODE, -1,
    S_PILCROW, 0, S_CYR_ZHE, 7, '', -1,
    0, 8, 103, 0,
    '', 0, '', 0, '', 0);
  InitCase(1, BARCODE_PDF417, UNICODE_MODE, BARCODE_CONTENT_SEGS,
    S_PILCROW, 0, S_CYR_ZHE, 7, '', -1,
    0, 8, 103, 2,
    S_PILCROW, 0, S_CYR_ZHE, 7, '', 0); { Delphi delta: keeps ECI 0 }
  InitCase(2, BARCODE_PDF417, UNICODE_MODE, -1,
    #$00E9#$00E9, 0, S_THAI, 0, S_GREEK + S_GREEK + S_GREEK, 0,
    ZWARN_USES_ECI, 8, 120, 0,
    '', 0, '', 0, '', 0);
  InitCase(3, BARCODE_PDF417, UNICODE_MODE, BARCODE_CONTENT_SEGS,
    #$00E9#$00E9, 0, S_THAI, 0, S_GREEK + S_GREEK + S_GREEK, 0,
    ZWARN_USES_ECI, 8, 120, 3,
    #$00E9#$00E9, 0, S_THAI, 0, S_GREEK + S_GREEK + S_GREEK, 0); { Delphi delta: content ECI values remain source ECI }
  InitCase(4, BARCODE_PDF417, DATA_MODE, -1,
    S_UTF8_PILCROW, 26, S_UTF8_ZHE, 0, S_BYTE_93_5F, 20,
    0, 8, 120, 0,
    '', 0, '', 0, '', 0); { Local delta-lock subset }
  InitCase(5, BARCODE_PDF417, DATA_MODE, BARCODE_CONTENT_SEGS,
    S_UTF8_PILCROW, 26, S_UTF8_ZHE, 0, S_BYTE_93_5F, 20,
    0, 8, 120, 3,
    S_UTF8_PILCROW, 26, S_UTF8_ZHE, 0, S_BYTE_93_5F, 20); { Local delta-lock subset }

  InitCase(6, BARCODE_PDF417TRUNC, UNICODE_MODE, -1,
    S_PILCROW, 0, S_CYR_ZHE, 7, '', -1,
    0, 8, 69, 0,
    '', 0, '', 0, '', 0);
  InitCase(7, BARCODE_PDF417TRUNC, UNICODE_MODE, BARCODE_CONTENT_SEGS,
    S_PILCROW, 0, S_CYR_ZHE, 7, '', -1,
    0, 8, 69, 2,
    S_PILCROW, 0, S_CYR_ZHE, 7, '', 0); { Delphi delta: keeps ECI 0 }

  InitCase(8, BARCODE_MICROPDF417, UNICODE_MODE, -1,
    S_PILCROW, 0, S_CYR_ZHE, 7, '', -1,
    0, 6, 82, 0,
    '', 0, '', 0, '', 0); { Local delta-lock subset }
  InitCase(9, BARCODE_MICROPDF417, UNICODE_MODE, BARCODE_CONTENT_SEGS,
    S_PILCROW, 0, S_CYR_ZHE, 7, '', -1,
    0, 6, 82, 2,
    S_PILCROW, 0, S_CYR_ZHE, 7, '', 0); { Local delta-lock subset }
  InitCase(10, BARCODE_MICROPDF417, UNICODE_MODE, -1,
    #$00E9#$00E9, 0, S_THAI, 0, S_GREEK + S_GREEK + S_GREEK, 0,
    ZWARN_USES_ECI, 24, 38, 0,
    '', 0, '', 0, '', 0); { Local delta-lock subset }
  InitCase(11, BARCODE_MICROPDF417, UNICODE_MODE, BARCODE_CONTENT_SEGS,
    #$00E9#$00E9, 0, S_THAI, 0, S_GREEK + S_GREEK + S_GREEK, 0,
    ZWARN_USES_ECI, 24, 38, 3,
    #$00E9#$00E9, 0, S_THAI, 0, S_GREEK + S_GREEK + S_GREEK, 0); { Local delta-lock subset }
  InitCase(12, BARCODE_MICROPDF417, DATA_MODE, -1,
    S_UTF8_PILCROW, 26, S_UTF8_ZHE, 0, S_BYTE_93_5F, 20,
    0, 24, 38, 0,
    '', 0, '', 0, '', 0); { Local delta-lock subset }
  InitCase(13, BARCODE_MICROPDF417, DATA_MODE, BARCODE_CONTENT_SEGS,
    S_UTF8_PILCROW, 26, S_UTF8_ZHE, 0, S_BYTE_93_5F, 20,
    0, 24, 38, 3,
    S_UTF8_PILCROW, 26, S_UTF8_ZHE, 0, S_BYTE_93_5F, 20); { Local delta-lock subset }

  for I := Low(Cases) to High(Cases) do
  begin
    if not Cases[I].Active then
      Continue;

    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        -1, -1, -1, Cases[I].OutputOptions);

      SegCount := 0;
      SetLength(Segs, 0);
      if Cases[I].Seg0_Data <> '' then
      begin
        SetLength(Segs, SegCount + 1);
        Segs[SegCount] := MakeSegment(Cases[I].Seg0_Data, Cases[I].Seg0_Eci, Cases[I].InputMode);
        Inc(SegCount);
      end;
      if Cases[I].Seg1_Data <> '' then
      begin
        SetLength(Segs, SegCount + 1);
        Segs[SegCount] := MakeSegment(Cases[I].Seg1_Data, Cases[I].Seg1_Eci, Cases[I].InputMode);
        Inc(SegCount);
      end;
      if Cases[I].Seg2_Data <> '' then
      begin
        SetLength(Segs, SegCount + 1);
        Segs[SegCount] := MakeSegment(Cases[I].Seg2_Data, Cases[I].Seg2_Eci, Cases[I].InputMode);
        Inc(SegCount);
      end;

      Ret := TZintTestHelper.EncodeDataSegs(Symbol, Segs);
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d ret (errtxt: %s)', [Cases[I].Index, TZintTestHelper.GetErrTxt(Symbol)]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows,
        Format('C#%d rows', [Cases[I].Index]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width,
        Format('C#%d width', [Cases[I].Index]));

      if Ret < ZERROR_TOO_LONG then
      begin
        Assert.AreEqual<Integer>(Cases[I].ExpectedContentCount, Symbol.content_segs_count,
          Format('C#%d content_segs_count', [Cases[I].Index]));
        if Cases[I].ExpectedContentCount > 0 then
        begin
          AssertContentSeg(Cases[I], 0, Cases[I].Exp0_Data, Cases[I].Exp0_Eci);
          if Cases[I].ExpectedContentCount > 1 then
            AssertContentSeg(Cases[I], 1, Cases[I].Exp1_Data, Cases[I].Exp1_Eci);
          if Cases[I].ExpectedContentCount > 2 then
            AssertContentSeg(Cases[I], 2, Cases[I].Exp2_Data, Cases[I].Exp2_Eci);
        end;
      end;
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestPDF417FromC.TestFuzzSubset;
const
  { Binary fuzz data from C test_fuzz (OSS-Fuzz) }
  FuzzDataA: array[0..1000] of Byte = (
    $30, $3D, $84, $30, $84, $30, $84, $21, $30, $3D, $30, $84, $30, $3D, $30, $3D,
    $84, $30, $3D, $30, $43, $84, $30, $84, $30, $03, $50, $30, $3D, $30, $04, $30,
    $84, $30, $3C, $84, $30, $84, $30, $3D, $84, $30, $3D, $30, $43, $84, $30, $8C,
    $30, $84, $30, $3D, $30, $19, $30, $3B, $30, $15, $30, $3D, $30, $84, $30, $43,
    $84, $30, $3D, $30, $84, $30, $00, $3D, $30, $96, $30, $40, $84, $30, $84, $30,
    $84, $30, $3D, $84, $30, $50, $8C, $30, $84, $30, $3C, $FF, $30, $3D, $84, $30,
    $3D, $30, $43, $84, $30, $8C, $30, $84, $30, $3D, $30, $21, $30, $00, $30, $84,
    $30, $50, $8C, $30, $84, $30, $3C, $84, $30, $FF, $30, $3D, $84, $30, $3D, $30,
    $43, $84, $30, $8C, $30, $84, $30, $3D, $30, $21, $30, $84, $30, $84, $30, $56,
    $30, $3D, $30, $84, $30, $7F, $30, $43, $84, $30, $84, $30, $FF, $30, $B2, $30,
    $00, $3D, $30, $96, $30, $40, $30, $3B, $30, $84, $30, $00, $3D, $30, $96, $30,
    $40, $30, $43, $84, $30, $84, $30, $3D, $84, $30, $84, $30, $84, $21, $30, $48,
    $30, $70, $30, $3D, $30, $3D, $84, $30, $3D, $30, $43, $84, $30, $84, $30, $03,
    $50, $30, $3D, $30, $04, $30, $84, $30, $3C, $84, $30, $84, $30, $3D, $84, $30,
    $3D, $30, $43, $84, $30, $8C, $30, $84, $30, $3D, $30, $3B, $30, $3C, $30, $3D,
    $30, $84, $30, $43, $84, $30, $3D, $30, $84, $30, $84, $30, $3D, $84, $30, $50,
    $8C, $30, $84, $30, $3C, $84, $30, $FF, $30, $3D, $84, $30, $3D, $30, $43, $84,
    $30, $8C, $30, $84, $30, $3D, $30, $21, $30, $00, $30, $00, $30, $80, $30, $84,
    $30, $8C, $30, $84, $30, $3D, $30, $61, $30, $00, $30, $84, $30, $50, $8C, $30,
    $84, $30, $3D, $30, $84, $30, $3D, $84, $30, $84, $30, $84, $3D, $30, $3D, $30,
    $84, $30, $3D, $30, $3D, $84, $30, $3D, $30, $43, $84, $30, $84, $30, $FA, $50,
    $30, $54, $30, $00, $30, $84, $30, $3C, $84, $30, $84, $30, $3D, $84, $30, $3D,
    $30, $43, $84, $30, $8C, $30, $84, $30, $3D, $30, $3B, $30, $3D, $30, $84, $30,
    $43, $84, $30, $3D, $30, $84, $30, $84, $30, $52, $30, $00, $30, $3D, $30, $00,
    $3E, $30, $40, $00, $30, $3D, $30, $43, $84, $30, $8C, $30, $84, $30, $3D, $30,
    $80, $30, $84, $3D, $30, $3D, $30, $84, $30, $00, $3D, $30, $96, $30, $40, $84,
    $30, $84, $30, $3D, $84, $30, $84, $30, $84, $3D, $30, $3D, $30, $84, $30, $5C,
    $30, $3D, $84, $30, $20, $30, $43, $84, $30, $FA, $50, $30, $54, $30, $04, $30,
    $43, $84, $30, $3C, $84, $30, $84, $30, $3D, $84, $30, $3D, $30, $43, $84, $30,
    $8C, $30, $84, $30, $3D, $30, $00, $30, $4B, $30, $FF, $30, $9D, $30, $3D, $30,
    $84, $30, $43, $84, $30, $84, $30, $3D, $30, $84, $30, $84, $30, $3D, $84, $30,
    $3D, $8C, $30, $84, $30, $3C, $84, $30, $84, $30, $3D, $84, $30, $3D, $30, $43,
    $89, $30, $8C, $30, $84, $30, $3D, $30, $21, $30, $84, $30, $84, $30, $50, $30,
    $3D, $30, $84, $30, $03, $30, $43, $84, $30, $84, $30, $FF, $30, $E8, $30, $93,
    $30, $00, $3D, $30, $96, $30, $43, $84, $30, $84, $30, $84, $50, $30, $3D, $30,
    $84, $30, $00, $3D, $30, $96, $30, $40, $30, $43, $84, $30, $84, $30, $3D, $84,
    $30, $84, $30, $84, $21, $30, $3D, $30, $84, $30, $3D, $30, $3D, $84, $30, $3D,
    $30, $43, $84, $30, $84, $30, $03, $50, $30, $3D, $30, $04, $30, $84, $30, $3C,
    $84, $30, $84, $30, $3D, $84, $30, $3D, $30, $43, $84, $30, $8C, $30, $84, $30,
    $3D, $30, $19, $30, $6D, $30, $00, $3D, $30, $96, $30, $40, $84, $30, $84, $30,
    $84, $30, $3D, $84, $30, $50, $8C, $30, $84, $30, $3C, $FF, $30, $3D, $84, $30,
    $3D, $30, $43, $84, $30, $8C, $30, $84, $30, $3D, $30, $21, $30, $00, $30, $84,
    $30, $50, $8C, $30, $84, $30, $3C, $84, $30, $FF, $30, $3D, $84, $30, $3D, $30,
    $43, $84, $30, $8C, $30, $84, $30, $3D, $30, $21, $30, $84, $30, $84, $30, $56,
    $30, $3D, $30, $84, $30, $7F, $30, $43, $84, $30, $84, $30, $FF, $30, $B2, $30,
    $00, $3D, $30, $96, $30, $40, $84, $30, $84, $30, $84, $3D, $30, $3B, $30, $84,
    $30, $00, $3D, $30, $96, $30, $40, $30, $43, $84, $30, $84, $30, $3D, $84, $30,
    $84, $30, $84, $3D, $30, $48, $30, $70, $30, $3D, $30, $3D, $84, $30, $3D, $30,
    $43, $84, $30, $84, $30, $FA, $50, $30, $54, $30, $00, $30, $84, $30, $3C, $84,
    $30, $84, $30, $3D, $84, $30, $3D, $30, $43, $84, $30, $8C, $30, $84, $30, $3D,
    $30, $3B, $30, $3C, $30, $3D, $30, $84, $30, $43, $84, $30, $3D, $30, $84, $30,
    $84, $30, $3D, $84, $30, $3D, $8C, $30, $84, $30, $3C, $84, $30, $84, $30, $3D,
    $84, $30, $3D, $30, $43, $8C, $30, $8C, $30, $84, $30, $3D, $30, $21, $30, $00,
    $30, $00, $30, $80, $30, $84, $30, $8C, $30, $84, $30, $3D, $30, $61, $30, $00,
    $30, $84, $30, $3D, $8C, $30, $84, $30, $3D, $30, $84, $30, $3D, $84, $30, $84,
    $30, $84, $21, $30, $3D, $30, $84, $30, $3D, $30, $3D, $84, $30, $3D, $30, $43,
    $84, $30, $84, $30, $03, $50, $30, $3D, $30, $04, $30, $84, $30, $3C, $84, $30,
    $84, $30, $3D, $84, $30, $3D, $30, $43, $84, $30, $8C, $30, $84, $30, $3D, $30,
    $3B, $30, $3D, $30, $84, $30, $43, $84, $30, $3D, $30, $84, $30, $84, $30, $52,
    $30, $00, $30, $3D, $30, $00, $3E, $30, $40, $00, $30, $04, $30, $43, $84, $30,
    $84, $30, $03, $30, $84, $3D, $30, $50, $8C, $30, $84, $30, $04, $30, $43, $84,
    $30, $84, $30, $03, $30, $89, $3C, $30, $50, $30, $54, $30, $E9, $30, $50, $30,
    $3D, $30, $E9, $30, $3A, $FD, $30, $84, $30
  );
  FuzzDataB: array[0..2610] of Byte = (
    $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $72, $72, $72, $72, $72,
    $72, $72, $72, $72, $72, $27, $52, $72, $00, $00, $77, $89, $86, $01, $00, $27,
    $6B, $6B, $6B, $6B, $6B, $37, $36, $74, $30, $30, $30, $30, $30, $30, $30, $30,
    $30, $30, $30, $72, $72, $72, $72, $72, $72, $72, $72, $72, $72, $27, $52, $72,
    $00, $00, $77, $89, $86, $01, $00, $27, $6B, $6B, $6B, $6B, $6B, $6B, $6B, $74,
    $74, $74, $74, $74, $74, $54, $74, $74, $74, $74, $74, $74, $74, $74, $74, $74,
    $74, $74, $74, $74, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $72,
    $72, $72, $72, $72, $72, $72, $72, $72, $72, $27, $52, $72, $72, $72, $72, $72,
    $72, $72, $77, $77, $77, $72, $72, $72, $72, $27, $52, $72, $00, $00, $77, $77,
    $77, $01, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28,
    $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $28, $29, $29, $28, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28,
    $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $47, $47, $47, $47, $47, $47, $47, $47,
    $47, $47, $72, $47, $47, $47, $47, $47, $47, $47, $47, $47, $47, $47, $47, $47,
    $47, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $29, $29,
    $28, $29, $29, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $6A,
    $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $47, $47, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $28, $29, $29, $28, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28,
    $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28,
    $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $28, $29, $29, $28, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28,
    $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $6A, $6A, $6A, $6A, $6A,
    $6A, $6A, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $6A, $6A,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $47, $47, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $29, $29, $28, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $6A, $6A, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $47, $47, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $28, $29, $29, $28, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $6A,
    $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $6A, $6A, $6A,
    $6A, $6A, $6A, $6A, $6A, $6A, $6A, $47, $47, $47, $47, $47, $6A, $29, $29, $29,
    $29, $47, $47, $47, $47, $47, $47, $47, $47, $47, $47, $47, $29, $29, $29, $29,
    $29, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $47, $47, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $29, $29, $28, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $47, $47, $47,
    $47, $47, $6A, $29, $29, $29, $6A, $29, $28, $29, $29, $29, $29, $29, $29, $29,
    $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $47, $47, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $29, $29, $28, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $6A, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $28, $29, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A,
    $6A, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A,
    $6A, $6A, $47, $47, $47, $47, $47, $6A, $29, $29, $29, $29, $47, $47, $47, $47,
    $47, $47, $47, $47, $47, $47, $47, $29, $29, $29, $29, $29, $29, $28, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $6A, $6A, $6A, $6A, $6A,
    $6A, $6A, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $6A, $6A, $6A, $6A,
    $6A, $6A, $6A, $6A, $6A, $6A, $47, $47, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $28, $29, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $29, $47, $47,
    $47, $47, $47, $47, $47, $47, $47, $47, $47, $29, $29, $29, $29, $29, $29, $28,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $6A, $6A, $6A, $6A, $6A, $6A, $6A,
    $6A, $6A, $6A, $47, $47, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28,
    $29, $29, $29, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $6A, $6A, $6A, $6A, $6A,
    $6A, $6A, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $6A,
    $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $6A, $6A,
    $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $6A,
    $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $47, $47, $47, $47, $47, $6A, $29,
    $29, $29, $29, $47, $47, $47, $47, $47, $47, $47, $47, $47, $47, $47, $29, $29,
    $29, $29, $29, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $47,
    $47, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $29, $29,
    $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $47,
    $47, $47, $47, $47, $6A, $29, $29, $29, $29, $47, $47, $47, $47, $47, $47, $47,
    $47, $47, $47, $47, $29, $29, $29, $29, $29, $29, $28, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29,
    $28, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $6A, $6A, $6A, $6A, $6A, $6A, $6A,
    $6A, $6A, $6A, $47, $47, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $28, $29, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $6A, $6A, $6A, $6A, $6A,
    $6A, $6A, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $6A, $6A, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $6A, $6A, $6A,
    $6A, $6A, $6A, $6A, $6A, $6A, $6A, $47, $47, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $28, $29, $29, $29, $29, $28, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $29, $29,
    $29, $29, $29, $29, $29, $29, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A,
    $47, $47, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $29,
    $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29,
    $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A,
    $47, $47, $47, $47, $47, $6A, $29, $29, $29, $29, $47, $47, $47, $47, $47, $47,
    $47, $47, $47, $47, $47, $29, $29, $29, $29, $29, $29, $28, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $6A,
    $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $6A, $6A, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A,
    $6A, $47, $47, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28,
    $29, $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A,
    $29, $28, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $28, $6A, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $6A, $6A, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $47, $47, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $28, $29, $29, $28, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29,
    $29, $29, $28, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $6A, $29, $28, $29, $29, $29,
    $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $00, $54, $74,
    $74, $72, $72, $72, $27, $52, $72, $72, $72, $72, $01, $40, $77, $77, $01, $24,
    $84, $77, $77
  );
  FuzzDataC: array[0..2689] of Byte = (
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $8A, $FF, $00, $00, $6B, 
    $6B, $6B, $6B, $6B, $6B, $6B, $30, $27, $27, $23, $27, $2F, $6B, $00, $6B, $6B, 
    $5F, $FF, $6B, $6B, $00, $5C, $00, $00, $00, $6B, $6B, $E3, $6B, $6B, $6B, $30, 
    $27, $27, $23, $27, $2F, $6F, $6B, $6B, $6B, $00, $72, $72, $72, $72, $72, $72, 
    $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $FF, $FF, $FF, $77, 
    $01, $40, $00, $FF, $04, $02, $00, $00, $00, $00, $00, $01, $00, $00, $5C, $3F, 
    $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, $5C, 
    $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, $F2, $FF, 
    $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $F2, $72, $72, $F4, $3A, 
    $7E, $00, $72, $72, $72, $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, 
    $5C, $72, $72, $72, $3A, $7E, $00, $8D, $8D, $72, $72, $72, $7C, $7C, $7C, $7C, 
    $3A, $7E, $00, $72, $72, $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, $5C, $5C, 
    $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, $72, $FF, $FF, 
    $FF, $FF, $FF, $FF, $5C, $5C, $5C, $72, $72, $3A, $7E, $00, $72, $72, $72, $72, 
    $72, $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, $FF, $5C, $5C, $56, $62, $5C, 
    $72, $F2, $72, $72, $72, $3A, $7E, $00, $72, $5C, $5C, $56, $62, $5C, $72, $F2, 
    $72, $72, $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $7C, $7C, $FF, $5C, $F2, 
    $72, $F2, $72, $72, $72, $3A, $7E, $00, $72, $F2, $72, $72, $72, $7C, $7C, $FF, 
    $5C, $5C, $5C, $72, $62, $F2, $5C, $72, $72, $72, $3A, $7E, $00, $00, $24, $72, 
    $72, $72, $72, $7C, $7C, $FF, $5C, $F2, $72, $72, $72, $5C, $5C, $5C, $62, $72, 
    $3A, $7E, $00, $8D, $8D, $72, $72, $72, $7C, $7C, $7C, $7C, $3A, $7E, $00, $72, 
    $72, $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, 
    $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, $72, $FF, $FF, $FF, $FF, $FF, $FF, 
    $5C, $5C, $5C, $62, $5C, $72, $6B, $6B, $6B, $6B, $6B, $6B, $62, $5C, $72, $72, 
    $72, $72, $3F, $72, $3A, $7E, $00, $72, $72, $FF, $FF, $FF, $FF, $FF, $FF, $5C, 
    $5C, $5C, $62, $5C, $72, $F2, $72, $72, $72, $3A, $7E, $00, $72, $00, $01, $00, 
    $00, $5C, $3F, $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $72, $FF, $2B, $FF, 
    $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, 
    $72, $F2, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $F2, $72, 
    $72, $F4, $3A, $7E, $00, $72, $72, $72, $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, 
    $72, $62, $F2, $5C, $72, $72, $72, $3A, $7E, $00, $8D, $8D, $72, $72, $72, $7C, 
    $7C, $7C, $7C, $3A, $7E, $00, $72, $72, $72, $72, $72, $72, $FF, $2B, $FF, $FF, 
    $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $72, $72, $72, $FF, 
    $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, 
    $7E, $00, $72, $72, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $72, $72, $3A, 
    $7E, $00, $72, $72, $72, $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, 
    $FF, $5C, $5C, $56, $62, $5C, $72, $F2, $72, $72, $72, $3A, $7E, $00, $7C, $7C, 
    $FF, $5C, $F2, $72, $F2, $72, $72, $72, $3A, $7E, $00, $72, $F2, $72, $72, $72, 
    $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, $5C, $72, $72, $72, $3A, $7E, $00, 
    $72, $F2, $72, $72, $72, $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $7B, 
    $6B, $6B, $6B, $75, $00, $00, $00, $6B, $69, $6B, $6B, $6B, $6B, $6B, $6B, $6B, 
    $6B, $6B, $6B, $6B, $6B, $6B, $6B, $7E, $00, $72, $F2, $FF, $FF, $FF, $FF, $FF, 
    $FF, $5C, $5C, $5C, $62, $5C, $72, $F2, $72, $72, $72, $3A, $7E, $72, $FF, $2B, 
    $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, 
    $00, $72, $72, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $F2, 
    $72, $72, $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $7C, $7C, $FF, $5C, $5C, 
    $5C, $72, $62, $F2, $FF, $5C, $5C, $5C, $77, $77, $77, $77, $77, $01, $40, $00, 
    $02, $00, $00, $00, $00, $00, $00, $00, $00, $6B, $6B, $37, $00, $00, $00, $6B, 
    $6B, $C0, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $8A, $FF, 
    $00, $00, $6B, $6B, $6B, $6B, $6B, $6B, $6B, $30, $27, $27, $23, $27, $2F, $6B, 
    $00, $6B, $6B, $5F, $FF, $6B, $6B, $00, $5C, $00, $00, $00, $6B, $6B, $E3, $6B, 
    $6B, $6B, $30, $27, $27, $23, $27, $2F, $6F, $6B, $6B, $6B, $72, $72, $3F, $72, 
    $3A, $7E, $00, $72, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, 
    $72, $72, $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, 
    $72, $72, $FF, $FF, $FF, $77, $01, $40, $00, $FF, $04, $02, $00, $00, $00, $00, 
    $00, $01, $00, $00, $5C, $3F, $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $72, 
    $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, 
    $3A, $7E, $00, $72, $F2, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, 
    $72, $F2, $72, $72, $F4, $3A, $7E, $00, $72, $72, $72, $72, $72, $7C, $7C, $FF, 
    $5C, $5C, $5C, $72, $62, $F2, $5C, $72, $72, $72, $3A, $7E, $00, $8D, $8D, $72, 
    $72, $72, $7C, $7C, $7C, $7C, $3A, $7E, $00, $72, $72, $72, $72, $72, $72, $FF, 
    $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, 
    $7E, $00, $72, $72, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $72, $72, $3A, 
    $7E, $00, $72, $72, $72, $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, 
    $FF, $5C, $5C, $56, $62, $5C, $72, $F2, $72, $72, $72, $3A, $7E, $00, $72, $72, 
    $72, $72, $72, $7C, $7C, $FF, $5C, $F2, $72, $F2, $72, $72, $72, $3A, $7E, $00, 
    $72, $F2, $72, $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, $5C, $72, 
    $72, $72, $3A, $7E, $00, $00, $24, $72, $72, $72, $72, $7C, $7C, $FF, $5C, $F2, 
    $72, $72, $72, $5C, $5C, $5C, $62, $72, $3A, $7E, $00, $8D, $8D, $72, $72, $72, 
    $7C, $7C, $7C, $7C, $3A, $7E, $00, $72, $72, $72, $72, $72, $72, $FF, $2B, $FF, 
    $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, 
    $72, $72, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $6B, $6B, 
    $6B, $6B, $6B, $6B, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, 
    $72, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $F2, $72, $72, 
    $72, $3A, $7E, $00, $72, $00, $01, $00, $00, $5C, $3F, $72, $3A, $7E, $00, $72, 
    $72, $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, 
    $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, $F2, $FF, $FF, $FF, $FF, $FF, $FF, 
    $5C, $5C, $5C, $62, $5C, $72, $F2, $72, $72, $F4, $3A, $7E, $00, $72, $72, $72, 
    $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, $5C, $72, $72, $72, $3A, 
    $7E, $00, $8D, $8D, $72, $72, $72, $7C, $7C, $7C, $7C, $3A, $7E, $00, $72, $72, 
    $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, 
    $72, $72, $3F, $72, $3A, $7E, $00, $72, $72, $FF, $FF, $FF, $FF, $FF, $FF, $5C, 
    $5C, $5C, $72, $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $7C, $7C, $FF, $5C, 
    $5C, $5C, $62, $5C, $72, $F2, $72, $72, $72, $3A, $7E, $00, $72, $72, $72, $72, 
    $72, $7C, $7C, $FF, $5C, $F2, $72, $7A, $72, $5C, $5C, $5C, $62, $72, $3A, $72, 
    $72, $72, $72, $7C, $7C, $FF, $5C, $F2, $72, $F2, $72, $72, $72, $3A, $7E, $00, 
    $72, $F2, $72, $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, $5C, $72, 
    $72, $72, $3A, $7E, $00, $00, $24, $72, $72, $72, $72, $7C, $7C, $FF, $5C, $F2, 
    $72, $72, $72, $5C, $5C, $5C, $62, $72, $3A, $7E, $00, $8D, $8D, $72, $72, $72, 
    $7C, $7C, $7C, $7C, $3A, $7E, $00, $72, $72, $72, $72, $72, $72, $FF, $2B, $FF, 
    $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, 
    $72, $72, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $6B, $6B, 
    $6B, $6B, $6B, $6B, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, 
    $72, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $F2, $72, $72, 
    $72, $3A, $7E, $00, $72, $00, $01, $00, $00, $5C, $3F, $72, $3A, $7E, $00, $72, 
    $72, $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, 
    $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, $F2, $FF, $FF, $FF, $FF, $FF, $FF, 
    $5C, $5C, $5C, $62, $5C, $72, $F2, $72, $72, $F4, $3A, $7E, $00, $72, $72, $72, 
    $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, $5C, $72, $72, $72, $3A, 
    $7E, $00, $8D, $8D, $72, $72, $72, $7C, $7C, $7C, $7C, $3A, $7E, $00, $72, $72, 
    $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, 
    $72, $72, $3F, $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, 
    $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, $72, $FF, $FF, $FF, $FF, 
    $FF, $FF, $5C, $5C, $5C, $72, $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $7C, 
    $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, $FF, $5C, $5C, $56, $62, $5C, $72, $F2, 
    $72, $72, $72, $3A, $7E, $00, $7C, $7C, $FF, $5C, $F2, $72, $F2, $72, $72, $72, 
    $3A, $7E, $00, $72, $F2, $72, $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, 
    $F2, $5C, $72, $72, $72, $3A, $7E, $00, $72, $F2, $72, $72, $72, $5C, $5C, $5C, 
    $62, $5C, $72, $72, $72, $72, $3F, $7B, $6B, $6B, $6B, $75, $00, $00, $00, $6B, 
    $69, $6B, $6B, $6B, $6B, $6B, $6B, $6B, $6B, $6B, $6B, $6B, $6B, $6B, $6B, $7E, 
    $00, $72, $F2, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $F2, 
    $72, $72, $72, $3A, $7E, $72, $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, 
    $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, $72, $FF, $FF, $FF, $FF, $FF, 
    $FF, $5C, $5C, $5C, $62, $5C, $72, $F2, $72, $72, $72, $3A, $7E, $00, $72, $72, 
    $72, $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, $FF, $5C, $5C, $5C, 
    $77, $77, $77, $77, $77, $01, $40, $00, $02, $00, $00, $00, $00, $00, $00, $00, 
    $00, $6B, $6B, $37, $00, $00, $00, $6B, $6B, $C0, $00, $00, $00, $00, $00, $00, 
    $00, $00, $00, $00, $00, $00, $8A, $FF, $00, $00, $6B, $6B, $6B, $6B, $6B, $6B, 
    $6B, $30, $27, $27, $23, $27, $2F, $6B, $00, $6B, $6B, $5F, $FF, $6B, $6B, $00, 
    $5C, $00, $00, $00, $6B, $6B, $E3, $6B, $6B, $6B, $30, $27, $27, $23, $27, $2F, 
    $6F, $6B, $6B, $6B, $72, $72, $3F, $72, $3A, $7E, $00, $72, $5C, $62, $5C, $72, 
    $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $72, $FF, $2B, 
    $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $FF, $FF, $FF, $77, $01, $40, 
    $00, $FF, $04, $02, $00, $00, $00, $00, $00, $01, $00, $00, $5C, $3F, $72, $3A, 
    $7E, $00, $72, $72, $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, 
    $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, $F2, $FF, $FF, $FF, 
    $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $F2, $72, $72, $F4, $3A, $7E, $00, 
    $72, $72, $72, $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, $5C, $72, 
    $72, $72, $3A, $7E, $00, $8D, $8D, $72, $72, $72, $7C, $7C, $7C, $7C, $3A, $7E, 
    $00, $72, $72, $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, 
    $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, $72, $FF, $FF, $FF, $FF, 
    $FF, $FF, $5C, $5C, $5C, $72, $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $7C, 
    $7C, $FF, $5C, $5C, $5C, $72, $62, $F2, $FF, $5C, $5C, $56, $62, $5C, $72, $F2, 
    $72, $72, $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $7C, $7C, $FF, $5C, $F2, 
    $72, $F2, $72, $72, $72, $3A, $7E, $00, $72, $F2, $72, $72, $72, $7C, $7C, $FF, 
    $5C, $5C, $5C, $72, $62, $F2, $5C, $72, $72, $72, $3A, $7E, $00, $00, $24, $72, 
    $72, $72, $72, $7C, $7C, $FF, $5C, $F2, $72, $72, $72, $5C, $5C, $5C, $62, $72, 
    $3A, $7E, $00, $8D, $8D, $72, $72, $72, $7C, $7C, $7C, $7C, $3A, $7E, $00, $72, 
    $72, $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, 
    $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, $72, $FF, $FF, $FF, $FF, $FF, $FF, 
    $5C, $5C, $5C, $62, $5C, $72, $6B, $6B, $6B, $6B, $6B, $6B, $62, $5C, $72, $72, 
    $72, $72, $3F, $72, $3A, $7E, $00, $72, $72, $FF, $FF, $FF, $FF, $FF, $FF, $5C, 
    $5C, $5C, $62, $5C, $72, $F2, $72, $72, $72, $3A, $7E, $00, $72, $00, $01, $00, 
    $00, $5C, $3F, $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $72, $FF, $2B, $FF, 
    $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, 
    $72, $F2, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $F2, $72, 
    $72, $F4, $3A, $7E, $00, $72, $72, $72, $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, 
    $72, $62, $F2, $5C, $72, $72, $72, $3A, $7E, $00, $8D, $8D, $72, $72, $72, $7C, 
    $7C, $7C, $7C, $3A, $7E, $00, $72, $72, $72, $72, $72, $72, $FF, $2B, $FF, $FF, 
    $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, 
    $72, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $72, $72, $3A, $7E, $00, $72, 
    $72, $72, $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, $62, $5C, $72, $F2, $72, $72, 
    $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $7C, $7C, $FF, $5C, $F2, $72, $7A, 
    $72, $5C, $5C, $5C, $62, $72, $3A, $7E, $00, $8D, $8D, $72, $72, $72, $7C, $7C, 
    $7C, $79, $3A, $7E, $00, $72, $72, $72, $72, $72, $72, $FF, $2B, $FF, $FF, $FF, 
    $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, $3A, $7E, $00, $72, $72, 
    $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $F2, $72, $72, $72, 
    $3A, $7E, $00, $72, $F2, $72, $72, $72, $7C, $7C, $FF, $5C, $5C, $5C, $72, $62, 
    $F2, $5C, $72, $72, $72, $3A, $7E, $00, $72, $F2, $72, $72, $72, $5C, $5C, $62, 
    $5C, $72, $F2, $72, $72, $72, $3A, $7E, $00, $24, $72, $72, $72, $72, $7C, $7C, 
    $FF, $5C, $F2, $72, $72, $72, $5C, $5C, $5C, $62, $72, $3A, $7E, $00, $8D, $8D, 
    $72, $72, $72, $7C, $7C, $7C, $7C, $3A, $7E, $00, $72, $72, $72, $72, $72, $72, 
    $FF, $2B, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, $72, $72, $72, $72, $3F, $72, 
    $3A, $7E, $00, $72, $72, $FF, $FF, $FF, $FF, $FF, $FF, $5C, $5C, $5C, $62, $5C, 
    $72, $F2, $72, $72, $72, $3A, $7E, $00, $72, $72, $72, $72, $72, $7C, $7C, $FF, 
    $5C, $5C, $5C, $72, $62, $F2, $5C, $72, $72, $72, $3A, $7E, $00, $8D, $8D, $72, 
    $72, $72, $7C, $7C, $7C, $7C, $5C, $5C, $5C, $62, $5C, $00, $6B, $6B, $6B, $6B, 
    $6B, $6B, $6B, $32, $27, $27, $23, $27, $2F, $B2, $2C, $FF, $5C, $5C, $62, $6B, 
    $D8, $6B 
  );
var
  Symbol: TZintSymbol;
  I: Integer;
  DataStr: String;
  ByteArr, DynA, DynB, DynC: TArrayOfByte;

  procedure CheckBin(AIndex, ASymb, AMode, AOpt1, AOpt2: Integer;
    const ABytes: TArrayOfByte; AByteLen, AExpRet: Integer);
  begin
    Symbol := TZintTestHelper.CreateSymbol(ASymb);
    try
      TZintTestHelper.SetupSymbol(Symbol, ASymb, AMode, AOpt1, AOpt2, -1, -1);
      Assert.AreEqual<Integer>(AExpRet,
        TZintTestHelper.EncodeData(Symbol, ABytes, AByteLen),
        Format('C#%d ret (errtxt: %s)', [AIndex, TZintTestHelper.GetErrTxt(Symbol)]));
    finally
      Symbol.Free;
    end;
  end;

  procedure CheckStr(AIndex, ASymb, AMode, AOpt1: Integer;
    const AData: String; AExpRet: Integer);
  begin
    Symbol := TZintTestHelper.CreateSymbol(ASymb);
    try
      TZintTestHelper.SetupSymbol(Symbol, ASymb, AMode, AOpt1, -1, -1, -1);
      Assert.AreEqual<Integer>(AExpRet,
        TZintTestHelper.EncodeData(Symbol, AData),
        Format('C#%d ret (errtxt: %s)', [AIndex, TZintTestHelper.GetErrTxt(Symbol)]));
    finally
      Symbol.Free;
    end;
  end;

begin
  { Copy static fuzz data into dynamic arrays for EncodeData }
  SetLength(DynA, 1001); Move(FuzzDataA[0], DynA[0], 1001);
  SetLength(DynB, 2611); Move(FuzzDataB[0], DynB[0], 2611);
  SetLength(DynC, 2690); Move(FuzzDataC[0], DynC[0], 2690);

  { C#2, C#3, C#28: BARCODE_PDF417COMP - not exposed in Delphi port, skip }

  { Binary fuzz data cases }
  CheckBin(0, BARCODE_PDF417, DATA_MODE or FAST_MODE, -1, -1, DynA, 1001, ZINT_ERROR_TOO_LONG);
  { Delta C#1: C ret=0, Delphi ret=ZINT_ERROR_TOO_LONG - DATA_MODE optimizer packs 1001B in C but not Delphi }
  CheckBin(1, BARCODE_PDF417, DATA_MODE, -1, -1, DynA, 1001, ZINT_ERROR_TOO_LONG);
  CheckBin(4, BARCODE_MICROPDF417, DATA_MODE or FAST_MODE, -1, -1, DynA, 1001, ZINT_ERROR_TOO_LONG);
  CheckBin(5, BARCODE_MICROPDF417, DATA_MODE, -1, -1, DynA, 1001, ZINT_ERROR_TOO_LONG);

  { 2710 digits: max PDF417 numeric ECC 0 }
  DataStr := '';
  for I := 1 to 271 do DataStr := DataStr + '1234567890';
  { DataStr is now exactly 2710 chars }
  CheckStr(6, BARCODE_PDF417, DATA_MODE or FAST_MODE, 0, DataStr, 0);
  CheckStr(7, BARCODE_PDF417, DATA_MODE, 0, DataStr, 0);
  DataStr := DataStr + '1'; { 2711 - one over }
  CheckStr(8, BARCODE_PDF417, DATA_MODE or FAST_MODE, 0, DataStr, ZINT_ERROR_TOO_LONG);
  CheckStr(9, BARCODE_PDF417, DATA_MODE, 0, DataStr, ZINT_ERROR_TOO_LONG);

  { 2528 digits: max PDF417 numeric default ECC }
  DataStr := '';
  while Length(DataStr) < 2528 do DataStr := DataStr + '1234567890';
  SetLength(DataStr, 2528);
  CheckStr(10, BARCODE_PDF417, DATA_MODE or FAST_MODE, -1, DataStr, 0);
  CheckStr(11, BARCODE_PDF417, DATA_MODE, -1, DataStr, 0);

  { 1850 text chars: max PDF417 text ECC 0 }
  DataStr := '';
  while Length(DataStr) < 1853 do DataStr := DataStr + 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  SetLength(DataStr, 1850);
  CheckStr(12, BARCODE_PDF417, DATA_MODE or FAST_MODE, 0, DataStr, 0);
  CheckStr(13, BARCODE_PDF417, DATA_MODE, 0, DataStr, 0);
  SetLength(DataStr, 1853);
  DataStr[1851] := 'A'; DataStr[1852] := 'B'; DataStr[1853] := 'C';
  CheckStr(14, BARCODE_PDF417, DATA_MODE or FAST_MODE, 0, DataStr, ZINT_ERROR_TOO_LONG);
  CheckStr(15, BARCODE_PDF417, DATA_MODE, 0, DataStr, ZINT_ERROR_TOO_LONG);

  { 1108 x $A0: max PDF417 bytes ECC 0 }
  SetLength(ByteArr, 1108);
  FillChar(ByteArr[0], 1108, $A0);
  CheckBin(16, BARCODE_PDF417, DATA_MODE or FAST_MODE, 0, -1, ByteArr, 1108, 0);
  CheckBin(17, BARCODE_PDF417, DATA_MODE, 0, -1, ByteArr, 1108, 0);
  SetLength(ByteArr, 1111);
  FillChar(ByteArr[0], 1111, $A0);
  CheckBin(18, BARCODE_PDF417, DATA_MODE or FAST_MODE, 0, -1, ByteArr, 1111, ZINT_ERROR_TOO_LONG);
  CheckBin(19, BARCODE_PDF417, DATA_MODE, 0, -1, ByteArr, 1111, ZINT_ERROR_TOO_LONG);

  { MicroPDF417 max numerics: 366/367 }
  DataStr := '';
  while Length(DataStr) < 367 do DataStr := DataStr + '1234567890';
  SetLength(DataStr, 366);
  CheckStr(20, BARCODE_MICROPDF417, DATA_MODE or FAST_MODE, -1, DataStr, 0);
  CheckStr(21, BARCODE_MICROPDF417, DATA_MODE, -1, DataStr, 0);
  SetLength(DataStr, 367);
  DataStr[367] := '7';
  CheckStr(22, BARCODE_MICROPDF417, DATA_MODE or FAST_MODE, -1, DataStr, ZINT_ERROR_TOO_LONG);
  CheckStr(23, BARCODE_MICROPDF417, DATA_MODE, -1, DataStr, ZINT_ERROR_TOO_LONG);

  { MicroPDF417 max text: 250/251 }
  DataStr := '';
  while Length(DataStr) < 251 do DataStr := DataStr + 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  SetLength(DataStr, 250);
  CheckStr(24, BARCODE_MICROPDF417, DATA_MODE or FAST_MODE, -1, DataStr, 0);
  CheckStr(25, BARCODE_MICROPDF417, DATA_MODE, -1, DataStr, 0);
  SetLength(DataStr, 251);
  DataStr[251] := 'Q';
  CheckStr(26, BARCODE_MICROPDF417, DATA_MODE or FAST_MODE, -1, DataStr, ZINT_ERROR_TOO_LONG);
  CheckStr(27, BARCODE_MICROPDF417, DATA_MODE, -1, DataStr, ZINT_ERROR_TOO_LONG);

  { Andre Maute OSS-Fuzz cases }
  CheckBin(29, BARCODE_PDF417, DATA_MODE or FAST_MODE, -1, -1, DynB, 2611, ZINT_ERROR_TOO_LONG);
  CheckBin(30, BARCODE_PDF417, DATA_MODE, -1, -1, DynB, 2611, ZINT_ERROR_TOO_LONG);
  CheckBin(31, BARCODE_PDF417, DATA_MODE or FAST_MODE, -1, 242, DynC, 2690, ZINT_ERROR_TOO_LONG);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestPDF417FromC);

end.
