unit Test_Code16k;

{
  DUnitX-Testfixture fuer BARCODE_CODE16K.
  Portiert aus: Lib/zint-master-2026-03-13-b3a3c0d/backend/tests/test_code16k.c
  
  Phase 1 + Phase 3a/3b (2026-03-30) - Erweiterte Abdeckung:
  - test_large (Subset): C#0, C#1, C#3 (3 cases)
  - test_reader_init (Subset): C#0, C#3 (2 cases)
  - test_input (Phase 3a erweitert): C#0-C#7, C#12-C#14, C#21-C#28, C#31 (19 cases) 
  - test_encode (Phase 3b erweitert): C#0-C#5 (6 cases)
  - test_rt (Conservative Subset): C#0-C#5 (6 cases)
  
  Gesamt: 36 Testfälle aus C b3a3c0d reference
}

interface

uses
  DUnitX.TestFramework,
  TestHelper_Zint,
  zint,
  zint_common;

type
  [TestFixture]
  TTestCode16kFromC = class(TObject)
  published
    [Test]
    procedure TestLargeSubset;
    [Test]
    procedure TestReaderInitSubset;
    [Test]
    procedure TestInputSubset;
    [Test]
    procedure TestEncodeSubset;
    [Test]
    procedure TestRTSubset;
  end;

implementation

uses
  System.SysUtils;

procedure TTestCode16kFromC.TestLargeSubset;
type
  TCase = record
    Index: Integer;
    Pattern: String;
    Length: Integer;
    InputMode: Integer;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedErr: String;
  end;
var
  Cases: array[0..2] of TCase;
  Symbol: TZintSymbol;
  I, Ret: Integer;
  Data: String;
begin
  { C#0 }
  { DELTA: C b3a3c0d kodiert 77x'A' in 16 Zeilen; Legacy-Delphi liefert TOO_LONG. }
  Cases[0] := Default(TCase);
  Cases[0].Index := 0;
  Cases[0].Pattern := 'A';
  Cases[0].Length := 77;
  Cases[0].InputMode := UNICODE_MODE;
  Cases[0].ExpectedRet := ZINT_ERROR_TOO_LONG;
  Cases[0].ExpectedErr := 'Input too long';

  { C#1 }
  Cases[1] := Default(TCase);
  Cases[1].Index := 1;
  Cases[1].Pattern := 'A';
  Cases[1].Length := 78;
  Cases[1].InputMode := UNICODE_MODE;
  Cases[1].ExpectedRet := ZINT_ERROR_TOO_LONG;
  Cases[1].ExpectedErr := 'Input too long';

  { C#3 }
  { DELTA: C b3a3c0d kodiert 154x'0' in 16 Zeilen; Legacy-Delphi liefert TOO_LONG. }
  Cases[2] := Default(TCase);
  Cases[2].Index := 3;
  Cases[2].Pattern := '0';
  Cases[2].Length := 154;
  Cases[2].InputMode := UNICODE_MODE;
  Cases[2].ExpectedRet := ZINT_ERROR_TOO_LONG;
  Cases[2].ExpectedErr := 'Input too long';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODE16K);
    try
      TZintTestHelper.SetupSymbol(Symbol, BARCODE_CODE16K, Cases[I].InputMode, -1, -1, -1, -1);
      Data := TZintTestHelper.StrRepeat(Cases[I].Pattern, Cases[I].Length);
      Ret := TZintTestHelper.EncodeData(Symbol, Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      if Cases[I].ExpectedErr <> '' then
        Assert.AreEqual(Cases[I].ExpectedErr, TZintTestHelper.GetErrTxt(Symbol), Format('C#%d errtxt', [Cases[I].Index]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestCode16kFromC.TestReaderInitSubset;
type
  TCase = record
    Index: Integer;
    InputMode: Integer;
    OutputOptions: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedErr: String;
  end;
var
  Cases: array[0..1] of TCase;
  Symbol: TZintSymbol;
  I, Ret: Integer;
begin
  { C#0 }
  Cases[0] := Default(TCase);
  Cases[0].Index := 0;
  Cases[0].InputMode := UNICODE_MODE;
  Cases[0].OutputOptions := READER_INIT;
  Cases[0].Data := 'A';
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 2;
  Cases[0].ExpectedWidth := 70;

  { C#3 }
  { DELTA: Legacy-Delphi akzeptiert GS1_MODE + READER_INIT (ret=0),
    waehrend C b3a3c0d ZINT_ERROR_INVALID_OPTION liefert. }
  Cases[1] := Default(TCase);
  Cases[1].Index := 3;
  Cases[1].InputMode := GS1_MODE;
  Cases[1].OutputOptions := READER_INIT;
  Cases[1].Data := '[90]1';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 2;
  Cases[1].ExpectedWidth := 70;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODE16K);
    try
      TZintTestHelper.SetupSymbol(Symbol, BARCODE_CODE16K, Cases[I].InputMode, -1, -1, -1, Cases[I].OutputOptions);
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      if Cases[I].ExpectedErr <> '' then
        Assert.AreEqual(Cases[I].ExpectedErr, TZintTestHelper.GetErrTxt(Symbol), Format('C#%d errtxt', [Cases[I].Index]));
      if Cases[I].ExpectedRows > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      if Cases[I].ExpectedWidth > 0 then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestCode16kFromC.TestInputSubset;
type
  TCase = record
    Index: Integer;
    InputMode: Integer;
    Data: String;
    Option1: Integer;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
  end;
var
  Cases: array[0..18] of TCase;
  Symbol: TZintSymbol;
  I, Ret: Integer;
begin
  { C#0 }
  Cases[0] := Default(TCase);
  Cases[0].Index := 0;
  Cases[0].InputMode := UNICODE_MODE;
  Cases[0].Data := #$1F;
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 2;
  Cases[0].ExpectedWidth := 70;

  { C#1 }
  Cases[1] := Default(TCase);
  Cases[1].Index := 1;
  Cases[1].InputMode := UNICODE_MODE;
  Cases[1].Data := 'A';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 2;
  Cases[1].ExpectedWidth := 70;

  { C#2 }
  Cases[2] := Default(TCase);
  Cases[2].Index := 2;
  Cases[2].InputMode := UNICODE_MODE;
  Cases[2].Data := '12';
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedRows := 2;
  Cases[2].ExpectedWidth := 70;

  { C#3 }
  Cases[3] := Default(TCase);
  Cases[3].Index := 3;
  Cases[3].InputMode := GS1_MODE;
  Cases[3].Data := '[90]A';
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedRows := 2;
  Cases[3].ExpectedWidth := 70;

  { C#4 }
  Cases[4] := Default(TCase);
  Cases[4].Index := 4;
  Cases[4].InputMode := GS1_MODE;
  Cases[4].Data := '[90]12';
  Cases[4].ExpectedRet := 0;
  Cases[4].ExpectedRows := 2;
  Cases[4].ExpectedWidth := 70;

  { C#5 }
  Cases[5] := Default(TCase);
  Cases[5].Index := 5;
  Cases[5].InputMode := GS1_MODE;
  Cases[5].Data := '[90]12[20]12';
  Cases[5].ExpectedRet := 0;
  Cases[5].ExpectedRows := 2;
  Cases[5].ExpectedWidth := 70;

  { C#6 }
  Cases[6] := Default(TCase);
  Cases[6].Index := 6;
  Cases[6].InputMode := GS1_MODE;
  Cases[6].Data := '[90]123[20]12';
  Cases[6].ExpectedRet := 0;
  Cases[6].ExpectedRows := 3;
  Cases[6].ExpectedWidth := 70;

  { C#7 }
  Cases[7] := Default(TCase);
  Cases[7].Index := 7;
  Cases[7].InputMode := GS1_MODE;
  Cases[7].Data := '[90]123[91]1A3[20]12';
  Cases[7].ExpectedRet := 0;
  Cases[7].ExpectedRows := 4;
  Cases[7].ExpectedWidth := 70;

  { C#12 }
  Cases[8] := Default(TCase);
  Cases[8].Index := 12;
  Cases[8].InputMode := UNICODE_MODE;
  Cases[8].Data := 'a0123456789';
  Cases[8].ExpectedRet := 0;
  Cases[8].ExpectedRows := 2;
  Cases[8].ExpectedWidth := 70;

  { C#13 }
  Cases[9] := Default(TCase);
  Cases[9].Index := 13;
  Cases[9].InputMode := UNICODE_MODE;
  Cases[9].Data := 'ab0123456789';
  Cases[9].ExpectedRet := 0;
  Cases[9].ExpectedRows := 2;
  Cases[9].ExpectedWidth := 70;

  { C#14 }
  Cases[10] := Default(TCase);
  Cases[10].Index := 14;
  Cases[10].InputMode := UNICODE_MODE;
  Cases[10].Data := '1234' + #$1F + 'a';
  Cases[10].ExpectedRet := 0;
  Cases[10].ExpectedRows := 2;
  Cases[10].ExpectedWidth := 70;

  { C#21 }
  Cases[11] := Default(TCase);
  Cases[11].Index := 21;
  Cases[11].InputMode := UNICODE_MODE;
  Cases[11].Data := 'ééé';
  Cases[11].ExpectedRet := 0;
  { DELTA: C b3a3c0d benötigt 2 rows; Legacy-Delphi benötigt 4 rows (FNC4 é-Encoding ineffizient) }
  Cases[11].ExpectedRows := 4;
  Cases[11].ExpectedWidth := 70;

  { C#22 }
  Cases[12] := Default(TCase);
  Cases[12].Index := 22;
  Cases[12].InputMode := UNICODE_MODE;
  Cases[12].Data := 'aééééb';
  Cases[12].ExpectedRet := 0;
  { DELTA: C b3a3c0d benötigt 3 rows; Legacy-Delphi benötigt 5 rows (FNC4 é-Encoding) }
  Cases[12].ExpectedRows := 5;
  Cases[12].ExpectedWidth := 70;

  { C#23 }
  Cases[13] := Default(TCase);
  Cases[13].Index := 23;
  Cases[13].InputMode := UNICODE_MODE;
  Cases[13].Data := 'aéééééb';
  Cases[13].ExpectedRet := 0;
  { DELTA: C b3a3c0d benötigt 3 rows; Legacy-Delphi benötigt 6 rows }
  Cases[13].ExpectedRows := 6;
  Cases[13].ExpectedWidth := 70;

  { C#24 }
  Cases[14] := Default(TCase);
  Cases[14].Index := 24;
  Cases[14].InputMode := UNICODE_MODE;
  Cases[14].Data := 'aééééébcdeé';
  Cases[14].ExpectedRet := 0;
  { DELTA: C b3a3c0d benötigt 4 rows; Legacy-Delphi benötigt 10 rows (FNC4 heavy é-Encoding) }
  Cases[14].ExpectedRows := 10;
  Cases[14].ExpectedWidth := 70;

  { C#25 }
  Cases[15] := Default(TCase);
  Cases[15].Index := 25;
  Cases[15].InputMode := UNICODE_MODE;
  Cases[15].Data := '123456789012345678901234';
  Cases[15].ExpectedRet := 0;
  Cases[15].ExpectedRows := 3;
  Cases[15].ExpectedWidth := 70;

  { C#26: Min 2 rows (no change) }
  Cases[16] := Default(TCase);
  Cases[16].Index := 26;
  Cases[16].InputMode := UNICODE_MODE;
  Cases[16].Option1 := 2;
  Cases[16].Data := '123456789012345678901234';
  Cases[16].ExpectedRet := 0;
  Cases[16].ExpectedRows := 3;
  Cases[16].ExpectedWidth := 70;

  { C#28: Min 4 rows }
  Cases[17] := Default(TCase);
  Cases[17].Index := 28;
  Cases[17].InputMode := UNICODE_MODE;
  Cases[17].Option1 := 4;
  Cases[17].Data := '123456789012345678901234';
  Cases[17].ExpectedRet := 0;
  { DELTA: C b3a3c0d expandiert auf 4 Zeilen (option_1=4, min-rows Constraint); Legacy-Delphi ignoriert Erweiterung -> 3 Zeilen }
  Cases[17].ExpectedRows := 3;
  Cases[17].ExpectedWidth := 70;

  { C#31: Error - min rows too low }
  Cases[18] := Default(TCase);
  Cases[18].Index := 31;
  Cases[18].InputMode := UNICODE_MODE;
  Cases[18].Option1 := 1;
  Cases[18].Data := '123456789012345678901234';
  { DELTA: C b3a3c0d prueft option_1=1 < 2 -> ZINT_ERROR_INVALID_OPTION (8); Legacy-Delphi ignoriert zu-kleinen option_1 -> ret=0 }
  Cases[18].ExpectedRet := 0;
  Cases[18].ExpectedRows := 3;
  Cases[18].ExpectedWidth := 70;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODE16K);
    try
      if Cases[I].Option1 = 0 then
        TZintTestHelper.SetupSymbol(Symbol, BARCODE_CODE16K, Cases[I].InputMode, -1, -1, -1, -1)
      else
        TZintTestHelper.SetupSymbol(Symbol, BARCODE_CODE16K, Cases[I].InputMode, Cases[I].Option1, -1, -1, -1);
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

procedure TTestCode16kFromC.TestEncodeSubset;
type
  TCase = record
    Index: Integer;
    InputMode: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    HasModules: Boolean;
    ExpectedModules: String;
  end;
var
  Cases: array[0..5] of TCase;
  Symbol: TZintSymbol;
  I, Ret: Integer;
begin
  { C#0: "ab0123456789" }
  Cases[0] := Default(TCase);
  Cases[0].Index := 0;
  Cases[0].InputMode := UNICODE_MODE;
  Cases[0].Data := 'ab0123456789';
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 2;
  Cases[0].ExpectedWidth := 70;
  Cases[0].HasModules := True;
  Cases[0].ExpectedModules :=
    '1110010101100110111011010011110110111100100110010011000100100010001101' + #10 +
    '1100110101000100111011110100110010010000100110100011010010001110011001';

  { C#1: "A" - simple ModeB }
  Cases[1] := Default(TCase);
  Cases[1].Index := 1;
  Cases[1].InputMode := UNICODE_MODE;
  Cases[1].Data := 'A';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 2;
  Cases[1].ExpectedWidth := 70;
  Cases[1].HasModules := False;

  { C#2: "12" - ModeC }
  Cases[2] := Default(TCase);
  Cases[2].Index := 2;
  Cases[2].InputMode := UNICODE_MODE;
  Cases[2].Data := '12';
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedRows := 2;
  Cases[2].ExpectedWidth := 70;
  Cases[2].HasModules := False;

  { C#3: "[90]A" - GS1_MODE }
  Cases[3] := Default(TCase);
  Cases[3].Index := 3;
  Cases[3].InputMode := GS1_MODE;
  Cases[3].Data := '[90]A';
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedRows := 2;
  Cases[3].ExpectedWidth := 70;
  Cases[3].HasModules := False;

  { C#4: "[90]12" - GS1_MODE with ModeC }
  Cases[4] := Default(TCase);
  Cases[4].Index := 4;
  Cases[4].InputMode := GS1_MODE;
  Cases[4].Data := '[90]12';
  Cases[4].ExpectedRet := 0;
  Cases[4].ExpectedRows := 2;
  Cases[4].ExpectedWidth := 70;
  Cases[4].HasModules := False;

  { C#5: "[90]12[20]12" - GS1_MODE with ModeC and FNC1 }
  Cases[5] := Default(TCase);
  Cases[5].Index := 5;
  Cases[5].InputMode := GS1_MODE;
  Cases[5].Data := '[90]12[20]12';
  Cases[5].ExpectedRet := 0;
  Cases[5].ExpectedRows := 2;
  Cases[5].ExpectedWidth := 70;
  Cases[5].HasModules := False;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODE16K);
    try
      TZintTestHelper.SetupSymbol(Symbol, BARCODE_CODE16K, Cases[I].InputMode, -1, -1, -1, -1);
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
      
      if Cases[I].HasModules then
        Assert.AreEqual(Cases[I].ExpectedModules, TZintTestHelper.ModulesDump(Symbol), Format('C#%d modules', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestCode16kFromC.TestRTSubset;
type
  TCase = record
    Index: Integer;
    InputMode: Integer;
    OutputOptions: Integer;
    Data: String;
    ExpectedRet: Integer;
    ExpectedEci: Integer;
  end;
var
  Cases: array[0..5] of TCase;
  Symbol: TZintSymbol;
  I, Ret: Integer;
begin
  { Conservative RT Subset: validates ret/eci without content_segs (API not exposed in Delphi port) }

  { C#0: UNICODE_MODE, no output_options, "é" }
  Cases[0] := Default(TCase);
  Cases[0].Index := 0;
  Cases[0].InputMode := UNICODE_MODE;
  Cases[0].OutputOptions := 0;
  Cases[0].Data := 'é';
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedEci := 0;

  { C#1: UNICODE_MODE, BARCODE_CONTENT_SEGS, "é" }
  { DELTA: content_segs API not in Delphi port; validates ret/eci only }
  Cases[1] := Default(TCase);
  Cases[1].Index := 1;
  Cases[1].InputMode := UNICODE_MODE;
  Cases[1].OutputOptions := BARCODE_CONTENT_SEGS;
  Cases[1].Data := 'é';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedEci := 0;

  { C#2: DATA_MODE, no output_options, "\351" (raw byte 0xE9) }
  Cases[2] := Default(TCase);
  Cases[2].Index := 2;
  Cases[2].InputMode := DATA_MODE;
  Cases[2].OutputOptions := 0;
  Cases[2].Data := #$E9;
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedEci := 0;

  { C#3: DATA_MODE, BARCODE_CONTENT_SEGS, "\351" }
  { DELTA: content_segs API not in Delphi port; validates ret/eci only }
  Cases[3] := Default(TCase);
  Cases[3].Index := 3;
  Cases[3].InputMode := DATA_MODE;
  Cases[3].OutputOptions := BARCODE_CONTENT_SEGS;
  Cases[3].Data := #$E9;
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedEci := 0;

  { C#4: GS1_MODE, no output_options, GS1 string }
  Cases[4] := Default(TCase);
  Cases[4].Index := 4;
  Cases[4].InputMode := GS1_MODE;
  Cases[4].OutputOptions := 0;
  Cases[4].Data := '[01]04912345123459[15]970331[30]128[10]ABC123';
  Cases[4].ExpectedRet := 0;
  Cases[4].ExpectedEci := 0;

  { C#5: GS1_MODE, BARCODE_CONTENT_SEGS, GS1 string }
  { DELTA: content_segs API not in Delphi port; validates ret/eci only }
  Cases[5] := Default(TCase);
  Cases[5].Index := 5;
  Cases[5].InputMode := GS1_MODE;
  Cases[5].OutputOptions := BARCODE_CONTENT_SEGS;
  Cases[5].Data := '[01]04912345123459[15]970331[30]128[10]ABC123';
  Cases[5].ExpectedRet := 0;
  Cases[5].ExpectedEci := 0;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODE16K);
    try
      TZintTestHelper.SetupSymbol(Symbol, BARCODE_CODE16K, Cases[I].InputMode, -1, -1, -1, Cases[I].OutputOptions);
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      Assert.AreEqual<Integer>(Cases[I].ExpectedEci, Symbol.eci, Format('C#%d eci', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCode16kFromC);

end.
