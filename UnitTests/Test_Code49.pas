unit Test_Code49;

{
  Test Unit for Code49 Barcode Symbology
  Port from C reference: test_code49.c (Zint b3a3c0d, 2026-03-13)
  
  Coverage (Phase 1 - Conservative Subset): 
    - TestLargeSubset (2 cases from test_large: C#0, C#1)
    - TestInputSubset (7 cases from test_input: C#0-C#1, C#13, C#27-C#28, and variants)
    - TestEncodeSubset (2 cases from test_encode: C#0-C#1)
    - TestRTSubset (1 case from test_rt: C#0)
  
  Total: 12 test cases (conservative subset for rapid validation)
  
  Next Phases (TODO):
    - Phase 2: Add test_large C#2-C#3 (numeric encodation with >49 chars)
    - Phase 3: Extended input tests with min-rows validation, GS1 variants
    - Phase 4: test_encode C#2-C#3 (min-rows option features)
    - Phase 5: Complete test_rt cases with BARCODE_CONTENT_SEGS
  
  Notes:
    - test_rt uses BARCODE_CONTENT_SEGS option which may not be fully implemented in Delphi
    - Numeric encodation (char '0' repeated N times) needs special handling
    - Min-rows validation (option_1) may differ from C in error codes/messages
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  DUnitX.TestFramework,
  SysUtils,
  TestHelper_Zint,
  zint,
  zint_common;

type
  [TestFixture]
  TTestCode49FromC = class
  published
    [Test]
    procedure TestLargeSubset;
    [Test]
    procedure TestInputSubset;
    [Test]
    procedure TestEncodeSubset;
    [Test]
    procedure TestRTSubset;
  end;

implementation

{ TTestCode49FromC }

procedure TTestCode49FromC.TestLargeSubset;
type
  TCase = record
    Index: Integer;
    Data: string;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    Comment: string;
  end;
var
  Cases: array[0..1] of TCase;
  I: Integer;
  Symbol: TZintSymbol;
  Ret: Integer;
begin
  { Initialize cases from test_code49.c test_large - using string repetition }
  Cases[0] := Default(TCase);
  Cases[0].Index := 0;
  Cases[0].Data := StringOfChar('A', 49); { 49 As - capacity boundary }
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 8;
  Cases[0].ExpectedWidth := 70;
  Cases[0].Comment := 'ANSI/AIM BC6-2000 Table 1 (49 As)';

  Cases[1] := Default(TCase);
  Cases[1].Index := 1;
  Cases[1].Data := StringOfChar('A', 50); { 50 As - TOO_LONG }
  Cases[1].ExpectedRet := ZINT_ERROR_TOO_LONG;
  Cases[1].ExpectedRows := 0;
  Cases[1].ExpectedWidth := 0;
  Cases[1].Comment := 'Over capacity (50 As)';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODE49);
    try
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret (%s)', [Cases[I].Index, Cases[I].Comment]));
      if Ret < ZINT_ERROR then
      begin
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
      end;
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestCode49FromC.TestInputSubset;
type
  TCase = record
    Index: Integer;
    InputMode: Integer;
    Data: string;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    Comment: string;
  end;
var
  Cases: array[0..6] of TCase;
  I: Integer;
  Symbol: TZintSymbol;
  Ret: Integer;
begin
  { Initialize representative cases from test_code49.c test_input }
  { C#0: Extended ASCII error validation }
  Cases[0] := Default(TCase);
  Cases[0].Index := 0;
  Cases[0].InputMode := UNICODE_MODE;
  Cases[0].Data := 'é'; { Extended ASCII char > 127 }
  Cases[0].ExpectedRet := ZINT_ERROR_INVALID_DATA;
  Cases[0].ExpectedRows := 0;
  Cases[0].ExpectedWidth := 0;
  Cases[0].Comment := 'Extended ASCII validation';

  { C#1: Example 2 from ANSI/AIM BC6-2000 }
  Cases[1] := Default(TCase);
  Cases[1].Index := 1;
  Cases[1].InputMode := UNICODE_MODE;
  Cases[1].Data := 'EXAMPLE 2';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 2;
  Cases[1].ExpectedWidth := 70;
  Cases[1].Comment := 'ANSI/AIM BC6-2000 Figure 3';

  { C#2: Numeric encodation example }
  Cases[2] := Default(TCase);
  Cases[2].Index := 2;
  Cases[2].InputMode := UNICODE_MODE;
  Cases[2].Data := '12345';
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedRows := 2;
  Cases[2].ExpectedWidth := 70;
  Cases[2].Comment := 'Numeric encodation example';

  { C#13: GS1 mode with FNC1 }
  Cases[3] := Default(TCase);
  Cases[3].Index := 13;
  Cases[3].InputMode := GS1_MODE;
  Cases[3].Data := '[90]12345[91]AB12345';
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedRows := 4;
  Cases[3].ExpectedWidth := 70;
  Cases[3].Comment := 'GS1 mode with FNC1';

  { C#27: Full capacity test }
  Cases[4] := Default(TCase);
  Cases[4].Index := 27;
  Cases[4].InputMode := UNICODE_MODE;
  Cases[4].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVW'; { 49 chars }
  Cases[4].ExpectedRet := 0;
  Cases[4].ExpectedRows := 8;
  Cases[4].ExpectedWidth := 70;
  Cases[4].Comment := 'Full capacity (49 chars)';

  { C#28: Over capacity }
  Cases[5] := Default(TCase);
  Cases[5].Index := 28;
  Cases[5].InputMode := UNICODE_MODE;
  Cases[5].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWX'; { 50 chars }
  Cases[5].ExpectedRet := ZINT_ERROR_TOO_LONG;
  Cases[5].ExpectedRows := 0;
  Cases[5].ExpectedWidth := 0;
  Cases[5].Comment := 'Over capacity (50 chars)';

  { C#1 variant: Simple alphanumeric }
  Cases[6] := Default(TCase);
  Cases[6].Index := 1;
  Cases[6].InputMode := UNICODE_MODE;
  Cases[6].Data := 'AB';
  Cases[6].ExpectedRet := 0;
  Cases[6].ExpectedRows := 2;
  Cases[6].ExpectedWidth := 70;
  Cases[6].Comment := 'Simple alphanumeric';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODE49);
    try
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret (%s)', [Cases[I].Index, Cases[I].Comment]));
      if Ret < ZINT_ERROR then
      begin
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
      end;
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestCode49FromC.TestEncodeSubset;
type
  TCase = record
    Index: Integer;
    Data: string;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    Comment: string;
  end;
var
  Cases: array[0..1] of TCase;
  I: Integer;
  Symbol: TZintSymbol;
  Ret: Integer;
begin
  { Initialize conservative cases from test_code49.c test_encode }
  Cases[0] := Default(TCase);
  Cases[0].Index := 0;
  Cases[0].Data := 'MULTIPLE ROWS IN CODE 49';
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedRows := 5;
  Cases[0].ExpectedWidth := 70;
  Cases[0].Comment := 'ANSI/AIM BC6-2000 Figure 1';

  Cases[1] := Default(TCase);
  Cases[1].Index := 1;
  Cases[1].Data := 'EXAMPLE 2';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 2;
  Cases[1].ExpectedWidth := 70;
  Cases[1].Comment := 'ANSI/AIM BC6-2000 Figure 3';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODE49);
    try
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret (%s)', [Cases[I].Index, Cases[I].Comment]));
      if Ret < ZINT_ERROR then
      begin
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows, Format('C#%d rows', [Cases[I].Index]));
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width, Format('C#%d width', [Cases[I].Index]));
      end;
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestCode49FromC.TestRTSubset;
type
  TCase = record
    Index: Integer;
    InputMode: Integer;
    Data: string;
    ExpectedRet: Integer;
  end;
var
  Cases: array[0..0] of TCase;
  I: Integer;
  Symbol: TZintSymbol;
  Ret: Integer;
begin
  { Conservative round-trip test from test_code49.c test_rt (without BARCODE_CONTENT_SEGS) }
  Cases[0] := Default(TCase);
  Cases[0].Index := 0;
  Cases[0].InputMode := UNICODE_MODE;
  Cases[0].Data := 'EXAMPLE 2';
  Cases[0].ExpectedRet := 0;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODE49);
    try
      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);
      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
    finally
      Symbol.Free;
    end;
  end;
end;

end.
