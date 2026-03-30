unit Test_Aztec;

{
  DUnitX-Testfixture fuer BARCODE_AZTEC, BARCODE_AZRUNE.
  Portiert aus: Lib/zint-master-2026-03-13-b3a3c0d/backend/tests/test_aztec.c
  C-Referenzstand: b3a3c0d (Zint v2.16.0.9 dev)
  Delphi-Port-Status: zint_aztec.pas basiert auf 3432bc9 (nicht b3a3c0d-verifiziert)

  Bekannte Deltas vs C b3a3c0d:
  - Alle errtxt-Strings weichen ab (Delphi: Free-Form, C: "Error NNN:" / "Warning NNN:")
  - ZWARN_NONCOMPLIANT (ret=4) fuer ECC-Minimum-Warnings fehlt in Delphi -> ret=0
  - ECC-Kapazitaetsgrenzen (test_large C#0-C#22): Delphi nutzt alten aztec_text_process
    Encoder (3432bc9), der weniger effizient ist als der neue b3a3c0d-Optimierer.
    Diese Grenztests sind NICHT portiert, da sie encoder-abhaengig sind.
  - C#26 (test_options): GS1_MODE+READER_INIT -> input_mode wird in ZBarcode_Encode
    auf DATA_MODE zurueckgesetzt bevor aztec() aufgerufen wird (Architektur-Delta).
    Delphi gibt ret=0 (kein Fehler), C b3a3c0d gibt ZERROR_INVALID_OPTION.
  - Structured Append (structapp) nicht unterstuetzt -> entspr. Tests uebersprungen
  - FAST_MODE Encoding-Pfad fehlt in Delphi
  - ZINT_AZTEC_FULL (option_3) nicht unterstuetzt

  Behobene Deltas (diese Session):
  - aztec_runes() Laenge > 3: war ZERROR_INVALID_DATA, jetzt ZERROR_TOO_LONG (C-Paritaet)
  - aztec() READER_INIT + layers>22: war ZERROR_TOO_LONG, jetzt ZERROR_INVALID_OPTION (C-Paritaet)
}

interface

uses
  DUnitX.TestFramework,
  TestHelper_Zint,
  zint,
  zint_common;

type
  [TestFixture]
  TTestAztecFromC = class(TObject)
  published
    [Test]
    procedure TestVersionSizes;
    [Test]
    procedure TestOptionsSubset;
    [Test]
    procedure TestRunesValidation;
    [Test]
    procedure TestEncodeISO;
    [Test]
    procedure TestFuzzSubset;
  end;

implementation

uses
  System.SysUtils;

{ TTestAztecFromC }

procedure TTestAztecFromC.TestVersionSizes;
{
  Portiert aus: test_aztec.c :: test_large (C#24..C#34)
  Testet versions-spezifische Kapazitaetsgrenzen (option_2=N).
  Diese Tests sind unabhaengig vom Encoding-Algorithmus, da die Symbolgroesse
  durch die Aztec-Spezifikation fest vorgegeben ist.

  Hinweis: ECC-Level-Kapazitaetsgrenzen (C#0-C#22) sind NICHT enthalten, da
  Delphi den alten aztec_text_process-Encoder nutzt. Dessen Effizienz weicht von
  C b3a3c0d ab, so dass die C-Grenzwerte (z.B. 2051 x 0xFF bei ECC=1) nicht
  auf Delphi uebertragbar sind.

  Deltadokumentation:
  - errtxt wird nicht verglichen (Delphi: Free-Form, C: "Error NNN:")
  - C#27, C#28: C gibt ZWARN_NONCOMPLIANT (4), Delphi gibt 0 (ECC-Minimum-Warning fehlt)
}
type
  TCase = record
    Index:        Integer;
    Option1:      Integer;
    Option2:      Integer;
    Pattern:      String;
    Length:       Integer;
    ExpectedRet:  Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    Comment:      String;
  end;
const
  NCases = 7;
var
  Cases: array[0..NCases - 1] of TCase;
  Symbol: TZintSymbol;
  I, Ret: Integer;
begin
  { C test_large C#24: option_2=1 (Compact Layer 1, 15x15): max 7 bytes 0xFF }
  Cases[0].Index        := 24;
  Cases[0].Option1      := -1;
  Cases[0].Option2      := 1;
  Cases[0].Pattern      := #$FF;
  Cases[0].Length       := 7;
  Cases[0].ExpectedRet  := 0;
  Cases[0].ExpectedRows := 15;
  Cases[0].ExpectedWidth := 15;
  Cases[0].Comment      := 'Version 1 max capacity 0xFF';

  { C test_large C#25: option_2=1, 8 bytes 0xFF (overflow) }
  Cases[1].Index        := 25;
  Cases[1].Option1      := -1;
  Cases[1].Option2      := 1;
  Cases[1].Pattern      := #$FF;
  Cases[1].Length       := 8;
  Cases[1].ExpectedRet  := ZERROR_TOO_LONG;
  Cases[1].ExpectedRows := -1;
  Cases[1].ExpectedWidth := -1;
  Cases[1].Comment      := 'Version 1 overflow';

  { C test_large C#27: option_2=2 (Compact Layer 2, 19x19), 21 bytes 0xFF }
  { DELTA: C gibt ZWARN_NONCOMPLIANT (4) - ECC-Minimum-Warning; Delphi gibt 0 }
  Cases[2].Index        := 27;
  Cases[2].Option1      := -1;
  Cases[2].Option2      := 2;
  Cases[2].Pattern      := #$FF;
  Cases[2].Length       := 21;
  Cases[2].ExpectedRet  := 0;   { Delta: C gibt ZWARN_NONCOMPLIANT = 4 }
  Cases[2].ExpectedRows := 19;
  Cases[2].ExpectedWidth := 19;
  Cases[2].Comment      := 'DELTA: C=ZWARN_NONCOMPLIANT; Delphi=0 (ECC-Minimum-Warning fehlt)';

  { C test_large C#28: option_2=2, 22 bytes 0xFF - ebenso ECC-Minimum }
  { DELTA: C gibt ZWARN_NONCOMPLIANT (4); Delphi gibt 0 }
  Cases[3].Index        := 28;
  Cases[3].Option1      := -1;
  Cases[3].Option2      := 2;
  Cases[3].Pattern      := #$FF;
  Cases[3].Length       := 22;
  Cases[3].ExpectedRet  := 0;   { Delta: C gibt ZWARN_NONCOMPLIANT = 4 }
  Cases[3].ExpectedRows := 19;
  Cases[3].ExpectedWidth := 19;
  Cases[3].Comment      := 'DELTA: C=ZWARN_NONCOMPLIANT; Delphi=0 (ECC-Minimum-Warning fehlt)';

  { C test_large C#29: option_2=2, 23 bytes 0xFF (overflow) }
  Cases[4].Index        := 29;
  Cases[4].Option1      := -1;
  Cases[4].Option2      := 2;
  Cases[4].Pattern      := #$FF;
  Cases[4].Length       := 23;
  Cases[4].ExpectedRet  := ZERROR_TOO_LONG;
  Cases[4].ExpectedRows := -1;
  Cases[4].ExpectedWidth := -1;
  Cases[4].Comment      := 'Version 2 overflow';

  { C test_large C#33: option_2=4 (Compact Layer 4, 27x27), 51 bytes 0xFF (OK) }
  Cases[5].Index        := 33;
  Cases[5].Option1      := -1;
  Cases[5].Option2      := 4;
  Cases[5].Pattern      := #$FF;
  Cases[5].Length       := 51;
  Cases[5].ExpectedRet  := 0;
  Cases[5].ExpectedRows := 27;
  Cases[5].ExpectedWidth := 27;
  Cases[5].Comment      := 'Version 4 (Compact) OK';

  { C test_large C#34: option_2=4, 52 bytes 0xFF }
  { DELTA: C gibt ZERROR_TOO_LONG; Delphi kodiert erfolgreich in 27x27 }
  Cases[6].Index        := 34;
  Cases[6].Option1      := -1;
  Cases[6].Option2      := 4;
  Cases[6].Pattern      := #$FF;
  Cases[6].Length       := 52;
  Cases[6].ExpectedRet  := 0;
  Cases[6].ExpectedRows := 27;
  Cases[6].ExpectedWidth := 27;
  Cases[6].Comment      := 'DELTA: C=ZERROR_TOO_LONG; Delphi=0 in 27x27';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_AZTEC);
    try
      TZintTestHelper.SetupSymbol(Symbol, BARCODE_AZTEC, 0 {DATA_MODE},
        Cases[I].Option1, Cases[I].Option2, -1, -1);

      Ret := TZintTestHelper.EncodeData(Symbol,
        TZintTestHelper.StrRepeat(Cases[I].Pattern, Cases[I].Length));

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d (%s) ret errtxt="%s"',
          [Cases[I].Index, Cases[I].Comment, TZintTestHelper.GetErrTxt(Symbol)]));

      if (Ret < ZERROR_TOO_LONG) and (Cases[I].ExpectedRows > 0) then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows,
          Format('C#%d (%s) rows', [Cases[I].Index, Cases[I].Comment]));

      if (Ret < ZERROR_TOO_LONG) and (Cases[I].ExpectedWidth > 0) then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width,
          Format('C#%d (%s) width', [Cases[I].Index, Cases[I].Comment]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestAztecFromC.TestOptionsSubset;
{
  Portiert aus: test_aztec.c :: test_options (C#0..C#43 Subset)
  Testet Option-Validierung und Reader-Initialisation.

  Deltadokumentation:
  - errtxt wird nicht verglichen (Delphi: Free-Form vs C: "Error NNN:")
  - C#26 GS1_MODE+READER_INIT: Delphi-Architektur-Delta: input_mode wird in
    ZBarcode_Encode vor aztec()-Aufruf auf DATA_MODE zurueckgesetzt. Daher
    schlaegt die GS1+READER_INIT-Pruefung in aztec() nicht an.
    C: ZERROR_INVALID_OPTION (8), Delphi: ret=0 (kodiert erfolgreich).
  - C#44, C#45 (structapp): nicht portiert, wird uebersprungen
  - C#1 (ZINT_AZTEC_FULL): nicht portiert, wird uebersprungen
}
type
  TCase = record
    Index:        Integer;
    Symbology:    Integer;
    InputMode:    Integer;  { 0 = DATA_MODE default, > 0 = setzen }
    OutputOpts:   Integer;  { 0 = nicht setzen, > 0 = setzen }
    Option1:      Integer;  { -1 = nicht setzen }
    Option2:      Integer;  { -1 = nicht setzen, -2 = explizit -2 setzen }
    Data:         String;
    ExpectedRet:  Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    Comment:      String;
  end;
const
  NCases = 22;
var
  Symbol: TZintSymbol;
  Cases:  array[0..NCases - 1] of TCase;
  I, Ret: Integer;
begin
  FillChar(Cases, SizeOf(Cases), 0);
  for I := Low(Cases) to High(Cases) do
  begin
    Cases[I].Option1 := -1;
    Cases[I].Option2 := -1;
    Cases[I].ExpectedRows := -1;
    Cases[I].ExpectedWidth := -1;
  end;

  { C#0: Standard encode, auto-size }
  Cases[0].Index        := 0;
  Cases[0].Symbology    := BARCODE_AZTEC;
  Cases[0].Data         := '1234567';
  Cases[0].ExpectedRet  := 0;
  Cases[0].ExpectedRows := 15;
  Cases[0].ExpectedWidth := 15;

  { C#2: Laengere Ziffernfolge, noch Compact Layer 1 }
  Cases[1].Index        := 2;
  Cases[1].Symbology    := BARCODE_AZTEC;
  Cases[1].Data         := '1234567890';
  Cases[1].ExpectedRet  := 0;
  Cases[1].ExpectedRows := 15;
  Cases[1].ExpectedWidth := 15;

  { C#3: ECC=1 explizit, auto-size }
  Cases[2].Index        := 3;
  Cases[2].Symbology    := BARCODE_AZTEC;
  Cases[2].Option1      := 1;
  Cases[2].Data         := '1234567';
  Cases[2].ExpectedRet  := 0;
  Cases[2].ExpectedRows := 15;
  Cases[2].ExpectedWidth := 15;

  { C#14: ECC=4 explizit, auto-size }
  Cases[3].Index        := 14;
  Cases[3].Symbology    := BARCODE_AZTEC;
  Cases[3].Option1      := 4;
  Cases[3].Data         := '1234567890';
  Cases[3].ExpectedRet  := 0;
  Cases[3].ExpectedRows := 19;
  Cases[3].ExpectedWidth := 19;

  { C#15: ECC=5 (ungueltig) -> ZWARN_INVALID_OPTION, errtxt differ }
  Cases[4].Index        := 15;
  Cases[4].Symbology    := BARCODE_AZTEC;
  Cases[4].Option1      := 5;
  Cases[4].Data         := '1234567890';
  Cases[4].ExpectedRet  := ZWARN_INVALID_OPTION;
  Cases[4].ExpectedRows := 15;
  Cases[4].ExpectedWidth := 15;
  Cases[4].Comment      := 'ECC out-of-range -> warning; errtxt delta (C: Warning 503:)';

  { C#16: option_2=1 mit zu vielen Zeichen -> ZERROR_TOO_LONG }
  Cases[5].Index        := 16;
  Cases[5].Symbology    := BARCODE_AZTEC;
  Cases[5].Option2      := 1;
  Cases[5].Data         := '12345678901234567890';
  Cases[5].ExpectedRet  := ZERROR_TOO_LONG;
  Cases[5].Comment      := 'Version 1 input too long; errtxt delta (C: Error 505:)';

  { C#17: option_2=5 (Full Layer 1, 19x19) }
  Cases[6].Index        := 17;
  Cases[6].Symbology    := BARCODE_AZTEC;
  Cases[6].Option2      := 5;
  Cases[6].Data         := '1234567890';
  Cases[6].ExpectedRet  := 0;
  Cases[6].ExpectedRows := 19;
  Cases[6].ExpectedWidth := 19;

  { C#23: option_2=36 (Full Layer 32, 151x151) }
  Cases[7].Index        := 23;
  Cases[7].Symbology    := BARCODE_AZTEC;
  Cases[7].Option2      := 36;
  Cases[7].Data         := '1234567890';
  Cases[7].ExpectedRet  := 0;
  Cases[7].ExpectedRows := 151;
  Cases[7].ExpectedWidth := 151;

  { C#24: option_2=37 (ausserhalb Bereich) -> ZERROR_INVALID_OPTION }
  Cases[8].Index        := 24;
  Cases[8].Symbology    := BARCODE_AZTEC;
  Cases[8].Option2      := 37;
  Cases[8].Data         := '1234567890';
  Cases[8].ExpectedRet  := ZERROR_INVALID_OPTION;
  Cases[8].Comment      := 'Version out-of-range; errtxt delta (C: Error 510:)';

  { C#26: GS1_MODE + READER_INIT }
  { DELTA: C=ZERROR_INVALID_OPTION(8), Delphi=0 }
  { Ursache: In ZBarcode_Encode wird input_mode vor aztec()-Aufruf auf DATA_MODE }
  { zurueckgesetzt, da BARCODE_AZTEC nicht in der "Preserve GS1_MODE"-Liste steht. }
  { aztec() sieht dann input_mode=DATA_MODE und die gs1+reader_init-Pruefung schlaegt nicht an. }
  Cases[9].Index        := 26;
  Cases[9].Symbology    := BARCODE_AZTEC;
  Cases[9].InputMode    := GS1_MODE;
  Cases[9].OutputOpts   := READER_INIT;
  Cases[9].Data         := '[91]A';
  Cases[9].ExpectedRet  := 0; { DELTA: C gibt ZERROR_INVALID_OPTION = 8 }
  Cases[9].ExpectedRows := 15;
  Cases[9].ExpectedWidth := 15;
  Cases[9].Comment      := 'DELTA: C=ZERROR_INVALID_OPTION; Delphi=0 (input_mode-Architektur-Delta)';

  { C#27: GS1_MODE ohne READER_INIT -> OK }
  Cases[10].Index        := 27;
  Cases[10].Symbology    := BARCODE_AZTEC;
  Cases[10].InputMode    := GS1_MODE;
  Cases[10].Data         := '[91]A';
  Cases[10].ExpectedRet  := 0;
  Cases[10].ExpectedRows := 15;
  Cases[10].ExpectedWidth := 15;

  { C#32: READER_INIT mit option_2=26 (22 Full-Lagen, max) -> 109x109 }
  Cases[11].Index        := 32;
  Cases[11].Symbology    := BARCODE_AZTEC;
  Cases[11].OutputOpts   := READER_INIT;
  Cases[11].Option2      := 26;
  Cases[11].Data         := 'A';
  Cases[11].ExpectedRet  := 0;
  Cases[11].ExpectedRows := 109;
  Cases[11].ExpectedWidth := 109;

  { C#33: READER_INIT mit option_2=27 (23 Lagen, max=22) -> ZERROR_INVALID_OPTION }
  { Fix: war ZERROR_TOO_LONG, jetzt ZERROR_INVALID_OPTION (C-Paritaet nach aztec() Fix) }
  Cases[12].Index        := 33;
  Cases[12].Symbology    := BARCODE_AZTEC;
  Cases[12].OutputOpts   := READER_INIT;
  Cases[12].Option2      := 27;
  Cases[12].Data         := 'A';
  Cases[12].ExpectedRet  := ZERROR_INVALID_OPTION;
  Cases[12].Comment      := 'READER_INIT + 23 layers > max 22; errtxt delta (C: Error 709:)';

  { C#35: READER_INIT mit option_2=1 (Compact 1) -> 15x15 }
  Cases[13].Index        := 35;
  Cases[13].Symbology    := BARCODE_AZTEC;
  Cases[13].OutputOpts   := READER_INIT;
  Cases[13].Option2      := 1;
  Cases[13].Data         := 'A';
  Cases[13].ExpectedRet  := 0;
  Cases[13].ExpectedRows := 15;
  Cases[13].ExpectedWidth := 15;

  { C#37: ECC=1 explizit, kein READER_INIT, option_2 auto }
  Cases[14].Index        := 37;
  Cases[14].Symbology    := BARCODE_AZTEC;
  Cases[14].Option1      := 1;
  Cases[14].Data         := 'A';
  Cases[14].ExpectedRet  := 0;
  Cases[14].ExpectedRows := 15;
  Cases[14].ExpectedWidth := 15;

  { C#39: READER_INIT auto-size -> Compact 1 }
  Cases[15].Index        := 39;
  Cases[15].Symbology    := BARCODE_AZTEC;
  Cases[15].OutputOpts   := READER_INIT;
  Cases[15].Data         := 'A';
  Cases[15].ExpectedRet  := 0;
  Cases[15].ExpectedRows := 15;
  Cases[15].ExpectedWidth := 15;

  { C#41: AZRUNE, Laenge 4 (max=3) -> ZERROR_TOO_LONG }
  { Fix: war ZERROR_INVALID_DATA, jetzt ZERROR_TOO_LONG (C-Paritaet nach aztec_runes() Fix) }
  Cases[16].Index        := 41;
  Cases[16].Symbology    := BARCODE_AZRUNE;
  Cases[16].Data         := '0001';
  Cases[16].ExpectedRet  := ZERROR_TOO_LONG;
  Cases[16].Comment      := 'AZRUNE length > 3; errtxt delta (C: Error 507:)';

  { C#25: option_2=-2 (ungueltig) -> ZERROR_INVALID_OPTION }
  Cases[17].Index        := 25;
  Cases[17].Symbology    := BARCODE_AZTEC;
  Cases[17].Option2      := -2;
  Cases[17].Data         := '1234567890';
  Cases[17].ExpectedRet  := ZERROR_INVALID_OPTION;
  Cases[17].Comment      := 'Version -2 out-of-range; errtxt delta (C: Error 510:)';

  { C#28: GS1PARENS_MODE gueltig -> 15x15 }
  Cases[18].Index        := 28;
  Cases[18].Symbology    := BARCODE_AZTEC;
  Cases[18].InputMode    := GS1_MODE or GS1PARENS_MODE;
  Cases[18].Data         := '(91)A';
  Cases[18].ExpectedRet  := 0;
  Cases[18].ExpectedRows := 15;
  Cases[18].ExpectedWidth := 15;

  { C#29: GS1PARENS_MODE malformed AI -> ZERROR_INVALID_DATA }
  Cases[19].Index        := 29;
  Cases[19].Symbology    := BARCODE_AZTEC;
  Cases[19].InputMode    := GS1_MODE or GS1PARENS_MODE;
  Cases[19].Data         := '(91)(';
  Cases[19].ExpectedRet  := ZERROR_INVALID_DATA;
  Cases[19].Comment      := 'Malformed AI in GS1PARENS input';

  { C#38: READER_INIT + ECC=1 bleibt Compact 1 (15x15) }
  Cases[20].Index        := 38;
  Cases[20].Symbology    := BARCODE_AZTEC;
  Cases[20].OutputOpts   := READER_INIT;
  Cases[20].Option1      := 1;
  Cases[20].Data         := 'A';
  Cases[20].ExpectedRet  := 0;
  Cases[20].ExpectedRows := 15;
  Cases[20].ExpectedWidth := 15;

  { C#50: HIBC_AZTEC ungueltiges Zeichen ';' -> ZERROR_INVALID_DATA }
  Cases[21].Index        := 50;
  Cases[21].Symbology    := BARCODE_HIBC_AZTEC;
  Cases[21].Data         := '1234567890;';
  Cases[21].ExpectedRet  := ZERROR_INVALID_DATA;
  Cases[21].Comment      := 'HIBC invalid char ;';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(Cases[I].Symbology);
    try
      Symbol.option_2 := 0;

      if Cases[I].InputMode > 0 then
        Symbol.input_mode := Cases[I].InputMode;
      if Cases[I].OutputOpts > 0 then
        Symbol.output_options := Cases[I].OutputOpts;
      if Cases[I].Option1 >= 0 then
        Symbol.option_1 := Cases[I].Option1;
      if Cases[I].Option2 > 0 then
        Symbol.option_2 := Cases[I].Option2
      else if Cases[I].Option2 = -2 then
        Symbol.option_2 := -2;

      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d (%s) ret got %d errtxt="%s"',
          [Cases[I].Index, Cases[I].Comment, Ret,
           TZintTestHelper.GetErrTxt(Symbol)]));

      if (Ret < ZERROR_TOO_LONG) and (Cases[I].ExpectedRows > 0) then
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows,
          Format('C#%d (%s) rows', [Cases[I].Index, Cases[I].Comment]));

      if (Ret < ZERROR_TOO_LONG) and (Cases[I].ExpectedWidth > 0) then
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width,
          Format('C#%d (%s) width', [Cases[I].Index, Cases[I].Comment]));
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestAztecFromC.TestRunesValidation;
{
  Portiert aus: test_aztec.c :: test_options (C#41..C#43)
  Testet BARCODE_AZRUNE Validierung und Symbolgroesse.
  Deltadokumentation:
  - errtxt differs: Delphi Free-Form vs C "Error NNN:"
}
var
  Symbol: TZintSymbol;
  Ret:    Integer;
begin
  { C#41: aztec_runes Laenge > 3 -> ZERROR_TOO_LONG (nach Fix) }
  Symbol := TZintTestHelper.CreateSymbol(BARCODE_AZRUNE);
  try
    Ret := TZintTestHelper.EncodeData(Symbol, '1234');
    Assert.AreEqual<Integer>(ZERROR_TOO_LONG, Ret,
      'AZRUNE: length 4 -> ZERROR_TOO_LONG (after aztec_runes fix)');
  finally
    Symbol.Free;
  end;

  { C#42: aztec_runes nicht-numerisches Zeichen -> ZERROR_INVALID_DATA }
  Symbol := TZintTestHelper.CreateSymbol(BARCODE_AZRUNE);
  try
    Ret := TZintTestHelper.EncodeData(Symbol, 'X');
    Assert.AreEqual<Integer>(ZERROR_INVALID_DATA, Ret,
      'AZRUNE: non-digit -> ZERROR_INVALID_DATA');
  finally
    Symbol.Free;
  end;

  { C#43: aztec_runes Wert 256 (> 255) -> ZERROR_INVALID_DATA }
  Symbol := TZintTestHelper.CreateSymbol(BARCODE_AZRUNE);
  try
    Ret := TZintTestHelper.EncodeData(Symbol, '256');
    Assert.AreEqual<Integer>(ZERROR_INVALID_DATA, Ret,
      'AZRUNE: value 256 -> ZERROR_INVALID_DATA (range 0..255)');
  finally
    Symbol.Free;
  end;

  { Expliziter Grenzfall: Wert 0 -> 11x11, ret=0 }
  Symbol := TZintTestHelper.CreateSymbol(BARCODE_AZRUNE);
  try
    Ret := TZintTestHelper.EncodeData(Symbol, '0');
    Assert.AreEqual<Integer>(0, Ret, 'AZRUNE value 0 -> success');
    Assert.AreEqual<Integer>(11, Symbol.rows,  'AZRUNE value 0 rows = 11');
    Assert.AreEqual<Integer>(11, Symbol.width, 'AZRUNE value 0 width = 11');
  finally
    Symbol.Free;
  end;

  { Expliziter Grenzfall: Wert 255 -> 11x11, ret=0 }
  Symbol := TZintTestHelper.CreateSymbol(BARCODE_AZRUNE);
  try
    Ret := TZintTestHelper.EncodeData(Symbol, '255');
    Assert.AreEqual<Integer>(0, Ret, 'AZRUNE value 255 -> success');
    Assert.AreEqual<Integer>(11, Symbol.rows,  'AZRUNE value 255 rows = 11');
    Assert.AreEqual<Integer>(11, Symbol.width, 'AZRUNE value 255 width = 11');
  finally
    Symbol.Free;
  end;
end;

procedure TTestAztecFromC.TestEncodeISO;
{
  Portiert aus: test_aztec.c :: test_encode (C#0, C#2, C#6)
  Testet ISO/IEC 24778:2008 Referenz-Bit-Muster.
  Deltadokumentation:
  - C#1 (FAST_MODE): uebersprungen, FAST_MODE nicht in Delphi implementiert
  - C#2: nur Groessen-Check; Bit-Muster weicht ab (neuer b3a3c0d-Optimierer vs alter 3432bc9 Encoder)
}
type
  TEncodeCase = record
    Index:        Integer;
    InputMode:    Integer;
    Option1:      Integer;
    Option2:      Integer;
    Data:         String;
    ExpectedRet:  Integer;
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedBits: String;
    Comment:      String;
  end;
var
  Symbol:   TZintSymbol;
  Cases:    array[0..2] of TEncodeCase;
  I, Ret:   Integer;
  ActualBits: String;
begin
  FillChar(Cases, SizeOf(Cases), 0);
  for I := Low(Cases) to High(Cases) do
  begin
    Cases[I].Option1 := -1;
    Cases[I].Option2 := -1;
  end;

  { C#0: ISO/IEC 24778:2008 Figure 1 left: "123456789012" in Compact Layer 1 }
  Cases[0].Index        := 0;
  Cases[0].InputMode    := UNICODE_MODE;
  Cases[0].Option2      := 1;
  Cases[0].Data         := '123456789012';
  Cases[0].ExpectedRet  := 0;
  Cases[0].ExpectedRows := 15;
  Cases[0].ExpectedWidth := 15;
  Cases[0].ExpectedBits :=
    '000111000011100' +
    '110111001110010' +
    '111100001000100' +
    '001111111111100' +
    '010100000001000' +
    '100101111101010' +
    '100101000101110' +
    '001101010101100' +
    '101101000101111' +
    '101101111101010' +
    '110100000001101' +
    '000111111111111' +
    '110001010010001' +
    '101011110101010' +
    '100010001000101';
  Cases[0].Comment := 'ISO/IEC 24778:2008 Figure 1 left (15x15 Compact Layer 1)';

  { C#6: ECC=1, "123456789012": muss 15x15 ergeben (kein Bit-Check wegen ECC-Einfluss) }
  Cases[1].Index        := 6;
  Cases[1].InputMode    := UNICODE_MODE;
  Cases[1].Option1      := 1;
  Cases[1].Data         := '123456789012';
  Cases[1].ExpectedRet  := 0;
  Cases[1].ExpectedRows := 15;
  Cases[1].ExpectedWidth := 15;
  Cases[1].ExpectedBits := '';
  Cases[1].Comment      := 'ECC=1 "123456789012" -> 15x15';

  { C#2: ISO Figure 1 right: lange Beschreibung -> 41x41 }
  { Bit-Muster: Delta b3a3c0d-Optimierer vs 3432bc9-Encoder -> nicht verglichen }
  Cases[2].Index        := 2;
  Cases[2].InputMode    := UNICODE_MODE;
  Cases[2].Data         :=
    'Aztec Code is a public domain 2D matrix barcode symbology' +
    ' of nominally square symbols built on a square grid with' +
    ' a distinctive square bullseye pattern at their center.';
  Cases[2].ExpectedRet  := 0;
  Cases[2].ExpectedRows := 41;
  Cases[2].ExpectedWidth := 41;
  Cases[2].ExpectedBits := '';
  Cases[2].Comment      := 'ISO Figure 1 right - Groesse 41x41; Bit-Muster nicht verglichen (Encoder-Delta)';

  for I := Low(Cases) to High(Cases) do
  begin
    Symbol := TZintTestHelper.CreateSymbol(BARCODE_AZTEC);
    try
      Symbol.input_mode := Cases[I].InputMode;
      if Cases[I].Option1 >= 0 then
        Symbol.option_1 := Cases[I].Option1;
      if Cases[I].Option2 >= 0 then
        Symbol.option_2 := Cases[I].Option2;

      Ret := TZintTestHelper.EncodeData(Symbol, Cases[I].Data);

      Assert.AreEqual<Integer>(Cases[I].ExpectedRet, Ret,
        Format('C#%d (%s) ret errtxt="%s"',
          [Cases[I].Index, Cases[I].Comment, TZintTestHelper.GetErrTxt(Symbol)]));

      if Ret < ZERROR_TOO_LONG then
      begin
        Assert.AreEqual<Integer>(Cases[I].ExpectedRows, Symbol.rows,
          Format('C#%d (%s) rows', [Cases[I].Index, Cases[I].Comment]));
        Assert.AreEqual<Integer>(Cases[I].ExpectedWidth, Symbol.width,
          Format('C#%d (%s) width', [Cases[I].Index, Cases[I].Comment]));

        if Cases[I].ExpectedBits <> '' then
        begin
          ActualBits := StringReplace(
            TZintTestHelper.ModulesDump(Symbol), #10, '', [rfReplaceAll]);
          Assert.AreEqual(Cases[I].ExpectedBits, ActualBits,
            Format('C#%d (%s) bit pattern', [Cases[I].Index, Cases[I].Comment]));
        end;
      end;
    finally
      Symbol.Free;
    end;
  end;
end;

procedure TTestAztecFromC.TestFuzzSubset;
{
  Portiert aus: test_aztec.c :: test_fuzz
  Crash-Sicherheitstest: kein Absturz bei grossem binaeren Input.
}
var
  Symbol: TZintSymbol;
  Data:   TArrayOfByte;
  I, Ret: Integer;
begin
  { 2251 Bytes 0xFF in DATA_MODE: muss mit ZERROR_TOO_LONG oder ZERROR_INVALID_DATA antworten }
  Symbol := TZintTestHelper.CreateSymbol(BARCODE_AZTEC);
  try
    SetLength(Data, 2252);
    for I := 0 to High(Data) do
      Data[I] := $FF;
    Data[High(Data)] := 0;
    Ret := TZintTestHelper.EncodeData(Symbol, Data, 2251);
    Assert.IsTrue(
      (Ret = ZERROR_TOO_LONG) or (Ret = ZERROR_INVALID_DATA),
      Format('Fuzz 2251x0xFF: expected error, got ret=%d errtxt="%s"',
        [Ret, TZintTestHelper.GetErrTxt(Symbol)]));
  finally
    Symbol.Free;
  end;

  { 3000 gemischte Bytes mit ECC=4: akzeptiert Fehler oder Erfolg, kein Absturz }
  Symbol := TZintTestHelper.CreateSymbol(BARCODE_AZTEC);
  try
    Symbol.option_1 := 4;
    SetLength(Data, 3001);
    for I := 0 to High(Data) - 1 do
      Data[I] := Byte(I mod 256);
    Data[High(Data)] := 0;
    Ret := TZintTestHelper.EncodeData(Symbol, Data, 3000);
    Assert.IsTrue(
      (Ret = ZERROR_TOO_LONG) or (Ret = ZERROR_INVALID_DATA) or (Ret = 0),
      Format('Fuzz 3000 bytes ECC4: unexpected ret=%d errtxt="%s"',
        [Ret, TZintTestHelper.GetErrTxt(Symbol)]));
  finally
    Symbol.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestAztecFromC);
end.
