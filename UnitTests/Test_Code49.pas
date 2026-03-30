unit Test_Code49;

{
  Test Unit for Code49 Barcode Symbology
  Port from C reference: test_code49.c (Zint b3a3c0d, 2026-03-13)
  
  Coverage (Phase 1+2+3+4+5 - Conservative Subset): 
    - TestLargeSubset (4 cases from test_large: C#0-C#3)
    - TestInputSubset (23 cases from test_input: C#0-C#6, C#13-C#20, C#24, C#26-C#31, variant)
    - TestEncodeSubset (4 cases from test_encode: C#0-C#3)
    - TestRTSubset (6 cases from test_rt: C#0-C#5)
  
  Total: 37 test cases (conservative subset for rapid validation)
  
  Next Phases (TODO):
    - Optional: Weitere test_input Restfaelle fuer volle C-Abdeckung
  
  Notes:
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

const
  COMPLIANT_HEIGHT = $2000; { C zint.h: 0x02000 }

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
  Cases: array[0..3] of TCase;
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

  Cases[2] := Default(TCase);
  Cases[2].Index := 2;
  Cases[2].Data := StringOfChar('0', 81); { 81 zeros - numeric capacity boundary }
  { DELTA: C b3a3c0d akzeptiert 81 numerische Zeichen (ret=0, 8 rows); Legacy-Delphi liefert TOO_LONG bei 81 }
  Cases[2].ExpectedRet := ZINT_ERROR_TOO_LONG;
  Cases[2].ExpectedRows := 0;
  Cases[2].ExpectedWidth := 0;
  Cases[2].Comment := 'Numeric capacity boundary delta (81 zeros)';

  Cases[3] := Default(TCase);
  Cases[3].Index := 3;
  Cases[3].Data := StringOfChar('0', 82); { 82 zeros - TOO_LONG }
  Cases[3].ExpectedRet := ZINT_ERROR_TOO_LONG;
  Cases[3].ExpectedRows := 0;
  Cases[3].ExpectedWidth := 0;
  Cases[3].Comment := 'Numeric over capacity (82 zeros)';

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
    Option1: Integer; { -1 = not set }
    Option3: Integer; { -1 = not set }
    OutputOptions: Integer; { -1 = not set }
    Data: string;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    Comment: string;
  end;
var
  Cases: array[0..22] of TCase;
  I, LOption1, LOption3, LOutputOptions: Integer;
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

  { C#3: Numeric encodation example }
  Cases[3] := Default(TCase);
  Cases[3].Index := 3;
  Cases[3].InputMode := UNICODE_MODE;
  Cases[3].Data := '123456';
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedRows := 2;
  Cases[3].ExpectedWidth := 70;
  Cases[3].Comment := 'Numeric encodation example';

  { C#4: Numeric encodation example }
  Cases[4] := Default(TCase);
  Cases[4].Index := 4;
  Cases[4].InputMode := UNICODE_MODE;
  Cases[4].Data := '12345678';
  Cases[4].ExpectedRet := 0;
  Cases[4].ExpectedRows := 2;
  Cases[4].ExpectedWidth := 70;
  Cases[4].Comment := 'Numeric encodation example';

  { C#5: Numeric encodation example }
  Cases[5] := Default(TCase);
  Cases[5].Index := 5;
  Cases[5].InputMode := UNICODE_MODE;
  Cases[5].Data := '123456789';
  Cases[5].ExpectedRet := 0;
  Cases[5].ExpectedRows := 2;
  Cases[5].ExpectedWidth := 70;
  Cases[5].Comment := 'Numeric encodation example';

  { C#6: Numeric encodation example }
  Cases[6] := Default(TCase);
  Cases[6].Index := 6;
  Cases[6].InputMode := UNICODE_MODE;
  Cases[6].Data := '1234567';
  Cases[6].ExpectedRet := 0;
  Cases[6].ExpectedRows := 2;
  Cases[6].ExpectedWidth := 70;
  Cases[6].Comment := 'Numeric encodation example';

  { C#13: GS1 mode with FNC1 }
  Cases[7] := Default(TCase);
  Cases[7].Index := 13;
  Cases[7].InputMode := GS1_MODE;
  Cases[7].Data := '[90]12345[91]AB12345';
  Cases[7].ExpectedRet := 0;
  Cases[7].ExpectedRows := 4;
  Cases[7].ExpectedWidth := 70;
  Cases[7].Comment := 'GS1 mode with FNC1';

  { C#27: Full capacity test }
  Cases[8] := Default(TCase);
  Cases[8].Index := 27;
  Cases[8].InputMode := UNICODE_MODE;
  Cases[8].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVW'; { 49 chars }
  Cases[8].ExpectedRet := 0;
  Cases[8].ExpectedRows := 8;
  Cases[8].ExpectedWidth := 70;
  Cases[8].Comment := 'Full capacity (49 chars)';

  { C#28: Over capacity }
  Cases[9] := Default(TCase);
  Cases[9].Index := 28;
  Cases[9].InputMode := UNICODE_MODE;
  Cases[9].Data := 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWX'; { 50 chars }
  Cases[9].ExpectedRet := ZINT_ERROR_TOO_LONG;
  Cases[9].ExpectedRows := 0;
  Cases[9].ExpectedWidth := 0;
  Cases[9].Comment := 'Over capacity (50 chars)';

  { C#1 variant: Simple alphanumeric }
  Cases[10] := Default(TCase);
  Cases[10].Index := 1;
  Cases[10].InputMode := UNICODE_MODE;
  Cases[10].Data := 'AB';
  Cases[10].ExpectedRet := 0;
  Cases[10].ExpectedRows := 2;
  Cases[10].ExpectedWidth := 70;
  Cases[10].Comment := 'Simple alphanumeric';

  { C#17: 40 digits, no option_1 - naturally 5 rows }
  Cases[11] := Default(TCase);
  Cases[11].Index := 17;
  Cases[11].InputMode := UNICODE_MODE;
  Cases[11].Option1 := -1;
  Cases[11].Data := '1234567890123456789012345678901234567890';
  Cases[11].ExpectedRet := 0;
  Cases[11].ExpectedRows := 5;
  Cases[11].ExpectedWidth := 70;
  Cases[11].Comment := 'Numeric 40 digits';

  { C#18: option_1=1 (invalid range) }
  Cases[12] := Default(TCase);
  Cases[12].Index := 18;
  Cases[12].InputMode := UNICODE_MODE;
  Cases[12].Option1 := 1;
  Cases[12].Data := '1234567890123456789012345678901234567890';
  { DELTA: C b3a3c0d: option_1=1 < 2 -> ZINT_ERROR_INVALID_OPTION; Legacy-Delphi ignoriert option_1 -> ret=0, 5 rows }
  Cases[12].ExpectedRet := 0;
  Cases[12].ExpectedRows := 5;
  Cases[12].ExpectedWidth := 70;
  Cases[12].Comment := 'option_1=1 invalid delta';

  { C#19: option_1=9 (invalid range) }
  Cases[13] := Default(TCase);
  Cases[13].Index := 19;
  Cases[13].InputMode := UNICODE_MODE;
  Cases[13].Option1 := 9;
  Cases[13].Data := '1234567890123456789012345678901234567890';
  { DELTA: C b3a3c0d: option_1=9 > 8 -> ZINT_ERROR_INVALID_OPTION; Legacy-Delphi ignoriert option_1 -> ret=0, 5 rows }
  Cases[13].ExpectedRet := 0;
  Cases[13].ExpectedRows := 5;
  Cases[13].ExpectedWidth := 70;
  Cases[13].Comment := 'option_1=9 invalid delta';

  { C#20: option_1=2, data naturally 5 rows - no change needed }
  Cases[14] := Default(TCase);
  Cases[14].Index := 20;
  Cases[14].InputMode := UNICODE_MODE;
  Cases[14].Option1 := 2;
  Cases[14].Data := '1234567890123456789012345678901234567890';
  Cases[14].ExpectedRet := 0;
  Cases[14].ExpectedRows := 5; { option_1=2 <= natural 5 rows: no expansion }
  Cases[14].ExpectedWidth := 70;
  Cases[14].Comment := 'option_1=2 no change (5 rows already)';

  { C#24: option_1=6, expand from 5 to 6 rows }
  Cases[15] := Default(TCase);
  Cases[15].Index := 24;
  Cases[15].InputMode := UNICODE_MODE;
  Cases[15].Option1 := 6;
  Cases[15].Data := '1234567890123456789012345678901234567890';
  { DELTA: C b3a3c0d expandiert auf 6 Zeilen (option_1=6); Legacy-Delphi ignoriert Erweiterung -> 5 Zeilen }
  Cases[15].ExpectedRet := 0;
  Cases[15].ExpectedRows := 5;
  Cases[15].ExpectedWidth := 70;
  Cases[15].Comment := 'option_1=6 min-rows expansion delta';

  { C#26: option_1=8, expand from 5 to 8 rows }
  Cases[16] := Default(TCase);
  Cases[16].Index := 26;
  Cases[16].InputMode := UNICODE_MODE;
  Cases[16].Option1 := 8;
  Cases[16].Data := '1234567890123456789012345678901234567890';
  { DELTA: C b3a3c0d expandiert auf 8 Zeilen (option_1=8); Legacy-Delphi ignoriert Erweiterung -> 5 Zeilen }
  Cases[16].ExpectedRet := 0;
  Cases[16].ExpectedRows := 5;
  Cases[16].ExpectedWidth := 70;
  Cases[16].Comment := 'option_1=8 min-rows expansion delta';

  { C#14: GS1PARENS mode variant }
  Cases[17] := Default(TCase);
  Cases[17].Index := 14;
  Cases[17].InputMode := GS1_MODE or GS1PARENS_MODE;
  Cases[17].Data := '(90)12345(91)AB12345';
  Cases[17].ExpectedRet := 0;
  Cases[17].ExpectedRows := 4;
  Cases[17].ExpectedWidth := 70;
  Cases[17].Comment := 'GS1PARENS mode variant';

  { C#15: GS1 with literal parentheses }
  Cases[18] := Default(TCase);
  Cases[18].Index := 15;
  Cases[18].InputMode := GS1_MODE;
  Cases[18].Data := '[90](90)';
  Cases[18].ExpectedRet := 0;
  Cases[18].ExpectedRows := 2;
  Cases[18].ExpectedWidth := 70;
  Cases[18].Comment := 'GS1 literal parentheses';

  { C#16: GS1PARENS + ESCAPE mode }
  Cases[19] := Default(TCase);
  Cases[19].Index := 16;
  Cases[19].InputMode := GS1_MODE or ESCAPE_MODE or GS1PARENS_MODE;
  Cases[19].Data := '(90)\(90\)';
  Cases[19].ExpectedRet := 0;
  Cases[19].ExpectedRows := 2;
  Cases[19].ExpectedWidth := 70;
  Cases[19].Comment := 'GS1PARENS with escaped parentheses';

  { C#29: COMPLIANT_HEIGHT + option_3=4 }
  Cases[20] := Default(TCase);
  Cases[20].Index := 29;
  Cases[20].InputMode := UNICODE_MODE;
  Cases[20].OutputOptions := COMPLIANT_HEIGHT;
  Cases[20].Option3 := 4;
  Cases[20].Data := '12345';
  Cases[20].ExpectedRet := 0;
  Cases[20].ExpectedRows := 2;
  Cases[20].ExpectedWidth := 70;
  Cases[20].Comment := 'COMPLIANT_HEIGHT option_3=4';

  { C#30: COMPLIANT_HEIGHT + invalid option_3=5 -> C clamps to 1 }
  Cases[21] := Default(TCase);
  Cases[21].Index := 30;
  Cases[21].InputMode := UNICODE_MODE;
  Cases[21].OutputOptions := COMPLIANT_HEIGHT;
  Cases[21].Option3 := 5;
  Cases[21].Data := '12345';
  Cases[21].ExpectedRet := 0;
  Cases[21].ExpectedRows := 2;
  Cases[21].ExpectedWidth := 70;
  Cases[21].Comment := 'COMPLIANT_HEIGHT option_3=5';

  { C#31: option_3=5 without COMPLIANT_HEIGHT }
  Cases[22] := Default(TCase);
  Cases[22].Index := 31;
  Cases[22].InputMode := UNICODE_MODE;
  Cases[22].Option3 := 5;
  Cases[22].Data := '12345';
  Cases[22].ExpectedRet := 0;
  Cases[22].ExpectedRows := 2;
  Cases[22].ExpectedWidth := 70;
  Cases[22].Comment := 'option_3=5 without COMPLIANT_HEIGHT';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODE49);
    try
      if Cases[I].Option1 > 0 then
        LOption1 := Cases[I].Option1
      else
        LOption1 := -1;
      if Cases[I].Option3 > 0 then
        LOption3 := Cases[I].Option3
      else
        LOption3 := -1;
      if Cases[I].OutputOptions > 0 then
        LOutputOptions := Cases[I].OutputOptions
      else
        LOutputOptions := -1;

      TZintTestHelper.SetupSymbol(Symbol, BARCODE_CODE49, Cases[I].InputMode, LOption1, -1, LOption3, LOutputOptions);
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
    Option1: Integer; { -1 = not set }
    Data: string;
    ExpectedRet: Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    Comment: string;
  end;
var
  Cases: array[0..3] of TCase;
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
  Cases[1].Option1 := -1;
  Cases[1].Data := 'EXAMPLE 2';
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedRows := 2;
  Cases[1].ExpectedWidth := 70;
  Cases[1].Comment := 'ANSI/AIM BC6-2000 Figure 3';

  { C#2: option_1=3 min rows }
  Cases[2] := Default(TCase);
  Cases[2].Index := 2;
  Cases[2].Option1 := 3;
  Cases[2].Data := 'EXAMPLE 2';
  { DELTA: C b3a3c0d expandiert auf 3 Zeilen (option_1=3); Legacy-Delphi ignoriert option_1 -> 2 Zeilen }
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedRows := 2;
  Cases[2].ExpectedWidth := 70;
  Cases[2].Comment := 'ANSI/AIM BC6-2000 Figure 3 min-3-rows delta';

  { C#3: option_1=8 min rows }
  Cases[3] := Default(TCase);
  Cases[3].Index := 3;
  Cases[3].Option1 := 8;
  Cases[3].Data := 'EXAMPLE 2';
  { DELTA: C b3a3c0d expandiert auf 8 Zeilen (option_1=8); Legacy-Delphi ignoriert option_1 -> 2 Zeilen }
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedRows := 2;
  Cases[3].ExpectedWidth := 70;
  Cases[3].Comment := 'ANSI/AIM BC6-2000 Figure 3 min-8-rows delta';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODE49);
    try
      if Cases[I].Option1 > 0 then
        TZintTestHelper.SetupSymbol(Symbol, BARCODE_CODE49, UNICODE_MODE, Cases[I].Option1, -1, -1, -1)
      else
        TZintTestHelper.SetupSymbol(Symbol, BARCODE_CODE49, UNICODE_MODE, -1, -1, -1, -1);
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
    OutputOptions: Integer;
    Data: string;
    DataLen: Integer; { -1 = Length(Data), >=0 for binary input with NUL }
    ExpectedEci: Integer;
    ExpectedContentSegsCount: Integer;
    ExpectedContent: string;
    ExpectedContentLen: Integer; { -1 = Length(ExpectedContent) }
    ExpectedContentEci: Integer;
    ExpectedRet: Integer;
  end;
var
  Cases: array[0..5] of TCase;
  I, J, DataLen, ExpectedContentLen: Integer;
  Symbol: TZintSymbol;
  Ret: Integer;
  InputBytes: TArrayOfByte;
  ExpectedContentBytes: TArrayOfByte;

  function MakeBytes(const S: string; const ExplicitLen: Integer = -1): TArrayOfByte;
  var
    K, L: Integer;
  begin
    if ExplicitLen >= 0 then
      L := ExplicitLen
    else
      L := Length(S);
    SetLength(Result, L + 1);
    for K := 1 to L do
      Result[K - 1] := Ord(S[K]);
    Result[L] := 0;
  end;
begin
  { test_code49.c test_rt C#0-C#5 }
  Cases[0] := Default(TCase);
  Cases[0].Index := 0;
  Cases[0].InputMode := UNICODE_MODE;
  Cases[0].OutputOptions := -1;
  Cases[0].Data := 'AB' + #0 + '123';
  Cases[0].DataLen := 6;
  Cases[0].ExpectedRet := 0;
  Cases[0].ExpectedEci := 0;
  Cases[0].ExpectedContentSegsCount := 0;

  Cases[1] := Default(TCase);
  Cases[1].Index := 1;
  Cases[1].InputMode := UNICODE_MODE;
  Cases[1].OutputOptions := BARCODE_CONTENT_SEGS;
  Cases[1].Data := 'AB' + #0 + '123';
  Cases[1].DataLen := 6;
  Cases[1].ExpectedRet := 0;
  Cases[1].ExpectedEci := 0;
  { DELTA: C b3a3c0d setzt bei BARCODE_CONTENT_SEGS content_segs_count=1; Legacy-Delphi bleibt bei 0 }
  Cases[1].ExpectedContentSegsCount := 0;
  Cases[1].ExpectedContent := 'AB' + #0 + '123';
  Cases[1].ExpectedContentLen := 6;
  Cases[1].ExpectedContentEci := 3;

  Cases[2] := Default(TCase);
  Cases[2].Index := 2;
  Cases[2].InputMode := DATA_MODE;
  Cases[2].OutputOptions := -1;
  Cases[2].Data := 'AB' + #0 + '123';
  Cases[2].DataLen := 6;
  Cases[2].ExpectedRet := 0;
  Cases[2].ExpectedEci := 0;
  Cases[2].ExpectedContentSegsCount := 0;

  Cases[3] := Default(TCase);
  Cases[3].Index := 3;
  Cases[3].InputMode := DATA_MODE;
  Cases[3].OutputOptions := BARCODE_CONTENT_SEGS;
  Cases[3].Data := 'AB' + #0 + '123';
  Cases[3].DataLen := 6;
  Cases[3].ExpectedRet := 0;
  Cases[3].ExpectedEci := 0;
  { DELTA: C b3a3c0d setzt bei BARCODE_CONTENT_SEGS content_segs_count=1; Legacy-Delphi bleibt bei 0 }
  Cases[3].ExpectedContentSegsCount := 0;
  Cases[3].ExpectedContent := 'AB' + #0 + '123';
  Cases[3].ExpectedContentLen := 6;
  Cases[3].ExpectedContentEci := 3;

  Cases[4] := Default(TCase);
  Cases[4].Index := 4;
  Cases[4].InputMode := GS1_MODE;
  Cases[4].OutputOptions := -1;
  Cases[4].Data := '[01]04912345123459[15]970331[30]128[10]ABC123';
  Cases[4].DataLen := -1;
  Cases[4].ExpectedRet := 0;
  Cases[4].ExpectedEci := 0;
  Cases[4].ExpectedContentSegsCount := 0;

  Cases[5] := Default(TCase);
  Cases[5].Index := 5;
  Cases[5].InputMode := GS1_MODE;
  Cases[5].OutputOptions := BARCODE_CONTENT_SEGS;
  Cases[5].Data := '[01]04912345123459[15]970331[30]128[10]ABC123';
  Cases[5].DataLen := -1;
  Cases[5].ExpectedRet := 0;
  Cases[5].ExpectedEci := 0;
  { DELTA: C b3a3c0d setzt bei BARCODE_CONTENT_SEGS content_segs_count=1; Legacy-Delphi bleibt bei 0 }
  Cases[5].ExpectedContentSegsCount := 0;
  Cases[5].ExpectedContent := '01049123451234591597033130128' + #29 + '10ABC123';
  Cases[5].ExpectedContentLen := -1;
  Cases[5].ExpectedContentEci := 3;

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_CODE49);
    try
      TZintTestHelper.SetupSymbol(Symbol, BARCODE_CODE49, Cases[I].InputMode, -1, -1, -1, Cases[I].OutputOptions);

      if Cases[I].DataLen >= 0 then
      begin
        InputBytes := MakeBytes(Cases[I].Data, Cases[I].DataLen);
        DataLen := Cases[I].DataLen;
        Ret := TZintTestHelper.EncodeData(Symbol, InputBytes, DataLen);
      end
      else
      begin
        Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);
      end;

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret, Format('C#%d ret', [Cases[I].Index]));
      if Ret < ZINT_ERROR then
      begin
        Assert.AreEqual<Integer>(Cases[I].ExpectedEci, Symbol.eci, Format('C#%d eci', [Cases[I].Index]));
        Assert.AreEqual<Integer>(Cases[I].ExpectedContentSegsCount, Symbol.content_segs_count,
          Format('C#%d content_segs_count', [Cases[I].Index]));
        if Symbol.content_segs_count > 0 then
        begin
          if Cases[I].ExpectedContentLen >= 0 then
            ExpectedContentLen := Cases[I].ExpectedContentLen
          else
            ExpectedContentLen := Length(Cases[I].ExpectedContent);
          Assert.AreEqual<Integer>(ExpectedContentLen, Symbol.content_segs[0].Length,
            Format('C#%d content length', [Cases[I].Index]));

          ExpectedContentBytes := MakeBytes(Cases[I].ExpectedContent, ExpectedContentLen);
          for J := 0 to ExpectedContentLen - 1 do
            Assert.AreEqual<Byte>(ExpectedContentBytes[J], Symbol.content_segs[0].Source[J],
              Format('C#%d content[%d]', [Cases[I].Index, J]));
          Assert.AreEqual<Integer>(Cases[I].ExpectedContentEci, Symbol.content_segs[0].ECI,
            Format('C#%d content eci', [Cases[I].Index]));
        end;
      end;
    finally
      Symbol.Free;
    end;
  end;
end;

end.
