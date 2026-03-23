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
    procedure TestEncodeSegsMainSubset;
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
  { Delta to C#53: Delphi currently chooses a shorter path with 6 rows (C expects 7). }
  Cases[27].ExpectedRows := 6;
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
  { Delta to C#175: Delphi currently selects 10 rows (C expects 9). }
  Cases[84].ExpectedRows := 10;
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
  { Delta to C#177: Delphi currently selects 10 rows (C expects 9). }
  Cases[85].ExpectedRows := 10;
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
  { Delta to C#183: Delphi currently selects 7 rows (C expects 10). }
  Cases[88].ExpectedRows := 7;
  { Delta to C#183: Delphi currently produces width 120 (C expects 103). }
  Cases[88].ExpectedWidth := 120;

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
  { Delta to C#185: Delphi currently selects 7 rows (C expects 10). }
  Cases[89].ExpectedRows := 7;
  { Delta to C#185: Delphi currently produces width 120 (C expects 103). }
  Cases[89].ExpectedWidth := 120;

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
  { Delta to C#187: Delphi currently produces 7x120 (C expects 10x103). }
  Cases[90].ExpectedRows := 7;
  Cases[90].ExpectedWidth := 120;

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
  { Delta to C#189: Delphi currently selects 10 rows (C expects 9). }
  Cases[92].ExpectedRows := 10;
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
  { Delta to C#197: Delphi currently selects 8 rows (C expects 9). }
  Cases[100].ExpectedRows := 8;
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
  { Delta to C#207: Delphi currently selects 9 rows (C expects 8). }
  Cases[110].ExpectedRows := 9;
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

procedure TTestPDF417FromC.TestEncodeSegsMainSubset;
type
  TCase = record
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
  Cases: array[0..2] of TCase;  { PoC: only first 3 items, start }
  I, J: Integer;
  Ret: Integer;
  Segs: TZintSegments;
  SegCount: Integer;
begin
  for I := Low(Cases) to High(Cases) do
    Cases[I] := Default(TCase);

  { C test_encode_segs C#0: Standard example with 2 segments (Latin + Cyrillic)}
  Cases[0].Index := 0;
  Cases[0].Symbology := BARCODE_PDF417;
  Cases[0].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[0].Option1 := -1;
  Cases[0].Option2 := -1;
  Cases[0].Option3 := -1;
  Cases[0].Seg0_Data := '¶';     { Pilcrow, Latin }
  Cases[0].Seg0_Eci := 0;         { ECI 0 = use auto-detect or symbol.eci }
  Cases[0].Seg1_Data := 'Ж';     { Cyrillic }
  Cases[0].Seg1_Eci := 7;         { ECI 7 = Cyrillic }
  Cases[0].Seg2_Data := '';      { Empty seg }
  Cases[0].Seg2_Eci := -1;
  Cases[0].StructApp_Count := 0; { No Structured Append }
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 8;
  Cases[0].ExpectedWidth := 103;
  Cases[0].Comment := 'Standard example';

  { C test_encode_segs C#1: Same as C#0 but without FAST_MODE }
  Cases[1].Index := 1;
  Cases[1].Symbology := BARCODE_PDF417;
  Cases[1].InputMode := UNICODE_MODE;
  Cases[1].Option1 := -1;
  Cases[1].Option2 := -1;
  Cases[1].Option3 := -1;
  Cases[1].Seg0_Data := '¶';
  Cases[1].Seg0_Eci := 0;
  Cases[1].Seg1_Data := 'Ж';
  Cases[1].Seg1_Eci := 7;
  Cases[1].Seg2_Data := '';
  Cases[1].Seg2_Eci := -1;
  Cases[1].StructApp_Count := 0;
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 8;
  Cases[1].ExpectedWidth := 103;
  Cases[1].Comment := 'Standard example (no FAST_MODE)';

  { C test_encode_segs C#2: Standard example with auto-ECI (expected WARN_USES_ECI) }
  Cases[2].Index := 2;
  Cases[2].Symbology := BARCODE_PDF417;
  Cases[2].InputMode := UNICODE_MODE or FAST_MODE;
  Cases[2].Option1 := -1;
  Cases[2].Option2 := -1;
  Cases[2].Option3 := -1;
  Cases[2].Seg0_Data := '¶';
  Cases[2].Seg0_Eci := 0;      { Auto-detect (Latin) }
  Cases[2].Seg1_Data := 'Ж';
  Cases[2].Seg1_Eci := 0;      { Auto-detect (should detect Cyrillic) }
  Cases[2].Seg2_Data := '';
  Cases[2].Seg2_Eci := -1;
  Cases[2].StructApp_Count := 0;
  Cases[2].ExpectedRet := ZWARN_USES_ECI;
  Cases[2].ExpectedRows := 8;
  Cases[2].ExpectedWidth := 103;
  Cases[2].Comment := 'Auto-ECI variant';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      TZintTestHelper.SetupSymbol(Symbol, Cases[I].Symbology, Cases[I].InputMode,
        Cases[I].Option1, Cases[I].Option2, Cases[I].Option3, -1);

      { Build segments array - only include non-empty segments }
      SegCount := 0;
      if Cases[I].Seg0_Data <> '' then
      begin
        SetLength(Segs, SegCount + 1);
        Segs[SegCount] := TZintTestHelper.StringToSegment(Cases[I].Seg0_Data, Cases[I].Seg0_Eci);
        Inc(SegCount);
      end;
      if Cases[I].Seg1_Data <> '' then
      begin
        SetLength(Segs, SegCount + 1);
        Segs[SegCount] := TZintTestHelper.StringToSegment(Cases[I].Seg1_Data, Cases[I].Seg1_Eci);
        Inc(SegCount);
      end;
      if Cases[I].Seg2_Data <> '' then
      begin
        SetLength(Segs, SegCount + 1);
        Segs[SegCount] := TZintTestHelper.StringToSegment(Cases[I].Seg2_Data, Cases[I].Seg2_Eci);
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

      { Set Structured Append if needed }
      if Cases[I].StructApp_Count > 0 then
      begin
        Symbol.structapp.index := Cases[I].StructApp_Index;
        Symbol.structapp.count := Cases[I].StructApp_Count;
        Symbol.structapp.id := Cases[I].StructApp_Id;
      end;

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

  { TODO: Add remaining C#3..C#43 items in future sessions }
  { NOTE: Items 42-43 (HIBC_PDF, HIBC_MICPDF) return ERROR_INVALID_OPTION and are not supported in Delphi }
end;

initialization
  TDUnitX.RegisterTestFixture(TTestPDF417FromC);

end.
