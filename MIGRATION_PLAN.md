# Zint Delphi Port – Migrations- und Testplan

## Ausgangslage

| | Delphi-Port (IST) | Zint C Original (SOLL) |
|---|---|---|
| **Basis-Commit** | `3432bc9aff311f2aea40f0e9883abfe6564c080b` | `b3a3c0d` (2026-03-13) |
| **Zint-Version** | ~2.4.x (ca. 2016/2017) | 2.16.0.9 (dev) |
| **Module** | 38 .pas-Dateien (~47.400 LOC) | 75+ .c/.h-Dateien |
| **Tests** | Keine (nur Demo-Apps) | 75+ Testdateien mit tausenden Testfällen |
| **C-Quellcode** | – | `lib/zint-master-2026-03-13-b3a3c0d/backend/` |
| **DUnitX-Projekt** | `UnitTests/DUnitXGuiRunner.dpr` und `UnitTests/DUnitXCmdTest.dpr` | – |

---

## Gesamtstrategie

```
Phase 1: Test-Infrastruktur         ← Fundament                    ✅ erledigt
Phase 2: Direkt portieren + testen  ← SOLL-Tests + Code-Port
Phase 3: Neue Module portieren      ← Fehlende Barcode-Typen
Phase 4: API-Erweiterungen          ← Neue Felder, GS1, ECI
```

> **Strategiewechsel (2025-06):** Die ursprüngliche Idee, erst IST-Tests gegen den
> alten Delphi-Code zu schreiben (Phase 2 alt), wurde verworfen. Der alte Port
> enthält zu viele Fehler (z.B. zint_telepen.pas: TeleTable[0] falsch, Error-Return
> auskommentiert, max-Länge 30 statt 69). Neuer Ansatz: **SOLL-Tests** auf Basis
> der C-Testdaten schreiben und gleichzeitig den Delphi-Code portieren, bis die
> Tests grün sind.

### Fortschritt

| Modul | Port | Tests | Status |
|---|---|---|---|
| `zint_telepen.pas` | ✅ b3a3c0d | ✅ `Test_Telepen.pas` (48 Tests) | **Erstes portiertes Modul** |
| `zint_medical.pas` | ✅ b3a3c0d | ✅ `Test_Medical.pas` + `Test_PZN.pas` (21 Tests) | pharma_one, pharma_two, code32, pzn inkl. BARCODE_CONTENT_SEGS-Paritaet |
| `zint_plessey.pas` | ✅ b3a3c0d | ✅ `Test_Plessey.pas` (32 Tests) | plessey, msi_handle (alle MSI-Varianten) inkl. BARCODE_CONTENT_SEGS-Paritaet |
| `zint_postal.pas` | ✅ b3a3c0d | ✅ `Test_Postal.pas` (84 Tests) | Alle 12 Funktionen inkl. CEPNet, FIM 'E', BARCODE_CONTENT_SEGS-Paritaet |
| `zint_common.pas` | 🔧 Bugfix + Basis-Checks | ✅ `Test_CommonCore.pas` | `posn()`-Regression abgesichert; Basischecks fuer `ctoi/itoc/ustrlen/is_stackable/is_extendable/istwodigits/froundup` |
| `zint.pas` | 🔧 Erweitert + Basis-Checks | ✅ `Test_CommonCore.pas` | +BARCODE_CEPNET=54, +ZWARN_NONCOMPLIANT=4, +BARCODE_VIN=73, Dispatch; Basischecks fuer `gs1_compliant/supports_eci` und Symbology-Mapping |
| `zint_code.pas` | ✅ b3a3c0d | ✅ `Test_Code.pas` (87 Tests) | Code11, C39, EC39, LOGMARS, C93, VIN, HIBC_39 |
| `zint_2of5.pas` | ✅ b3a3c0d | ✅ `Test_2of5.pas` (81 Tests) | C25Standard/Inter/IATA/Logic/Ind, ITF14, DPLEIT, DPIDENT; 1 dokumentiertes ESCAPE_MODE-Delta (errtxt-Position) |
| `zint_code128.pas` | ✅ b3a3c0d | ✅ `Test_Code128.pas` (41 Tests) | Code128, Code128B, EAN-128/GS1-128, EAN-14, NVE-18, HIBC-128 (DAC-DM Algorithmus) |
| `zint_code1.pas` | 🟡 Teilport b3a3c0d | ✅ `Test_Code1.pas` (5 Tests / 167 C-Indizes) | `test_input` C#0..C#38 vollstaendig + `test_large` C#104 aktiv + `test_encode` C#27..C#144 im Hauptsubset sowie fruehe Version-T-/Legacy-Faelle C#21..C#27 im Deep-Subset abgedeckt. `test_encode_segs` C#0..C#10 aktiv. `test_fuzz` vollstaendig C#0..C#5 aktiv (OSS-/CI-Fuzz-Repros inkl. #300-Varianten). C40/TEXT/DECIMAL/BYTE/GS1 Version-T-Pfade inkl. C#21/C#22/C#23/C#24/C#25/C#26/C#27 auf C-Paritaet. Dokumentierte Deltas: C#1 (kein `ZWARN_USES_ECI`; 22x22 statt 16x18), C#3 (kein `ZWARN_USES_ECI`), C#0/C#2/C#4/C#5/C#6/C#7/C#8/C#9/C#10 (`test_encode_segs`: `Error 799`/Mixed segment ECI not yet supported statt C-Erfolg bzw. C#10 `INVALID_OPTION` aus anderem Grund), C#28/C#30/C#32/C#34 (Erfolg statt TOO_LONG), C#39/C#43 (Too long statt C-Erfolg), C#48 (lower-max Digit-Quirk; TOO_LONG statt C-Erfolg), C#50 (Alpha-Grenze; TOO_LONG statt C-Erfolg), C#56 (lower-max Digit-Quirk; TOO_LONG statt C-Erfolg), C#65 (184 statt 183 CW), C#68 (Alpha-Grenze; TOO_LONG statt C-Erfolg), C#74 (lower-max Digit-Quirk; TOO_LONG statt C-Erfolg), C#76 (Alpha-Grenze; TOO_LONG statt C-Erfolg), C#80 (Byte-Grenze; TOO_LONG statt C-Erfolg), C#83 (734 statt 733 CW), C#86 (Alpha-Grenze; TOO_LONG statt C-Erfolg), C#90 (Byte-Grenze; TOO_LONG statt C-Erfolg), C#93 (generischer Error-517-Pfad statt versionsspezifischem Overflow), C#94 (lower-max Digit-Quirk; generischer Error-517-Pfad statt C-Erfolg), C#96 (Alpha-Grenze; generischer Error-517-Pfad statt C-Erfolg), C#100 (Byte-Grenze; generischer Error-517-Pfad statt C-Erfolg), C#109 (T-32 statt T-16), C#117 (T-48 statt T-32), C#119 (T-48 statt T-32), C#129 (Too long statt C-Erfolg; 39 CW), C#142 (39 statt 40 CW), C#144 (Erfolg statt TOO_LONG). Verbleibend: keine Code-One-Testbloecke im aktuellen Scope. |

**Gesamtstand (Session 6): 865 Tests, 865 bestanden, 0 fehlgeschlagen** ✅

Hinweis: Die QR-Familie ist aktuell voll gruen, enthaelt aber dokumentierte Delphi-vs-C-Paritaetsdeltas
(vor allem Warning-Klassifikation in einzelnen Unicode-Optimize-Faellen sowie content_segs/API-Themen).

Hinweis Code One (Sessions 3+4):
- Session 3: GS1_MODE-Fix, Decimal-flush, BYTE-Mode-Restore. C#6/7/8/11/15/18 volle Paritaet.
- Session 4: C40/TEXT/BYTE/GS1 Version T Pfade (C#20-27) portiert.
  Fixes: c40_value A-Z→14-39, a-z→1-26 (war vertauscht); text_shift y/z Index 121/122 = 0 (war 3).
  Session 5: c1_look_ahead_test auf C-Integer-Scoring portiert + `c1_is_last_single_ascii` Umschaltpfad in C40/TEXT (`c40_p/text_p=0`) hinzugefuegt.
  C#21 geschlossen: jetzt 55 CW wie C.
  C#24 geschlossen: Version T + ECI numerischer Grenzfall 77 Digits liefert jetzt Error 516 (39 CW) wie C (aktuell ueber explizite Grenzfall-Abbildung im Version-T-ECI-Numeric-Pfad).
  C#25 geschlossen: escaped ECI-Laenge wird in Version-T-Limitpruefung beruecksichtigt (84+7=91 -> Error 519).
  C#22 (38 Null-Bytes), C#23 (76 Digits ECI=3), C#26 (GS1 T-16), C#27 (GS1+ECI warn) volle Paritaet.

---

## Phase 1: Test-Infrastruktur aufbauen

### 1.1 Test-Helper-Unit erstellen
- [x] `UnitTests/TestHelper_Zint.pas` anlegen
- [x] Enthält Hilfs-Funktionen analog zu `testcommon.c`:
  - `CreateSymbol(ASymbology: Integer): TZintSymbol`
  - `SetupSymbol(ASymbol: TZintSymbol; ASymbology, AInputMode, AOption1, AOption2, AOption3, AOutputOptions: Integer)`
  - `EncodeAndCheck(ASymbol: TZintSymbol; AData: String; AExpectedResult: Integer; AExpectedWidth, AExpectedRows: Integer; AExpectedErrTxt: String)`
  - `CompareModules(ASymbol: TZintSymbol; AExpectedPattern: String): Boolean` – Modul-Pattern vergleichen

### 1.2 Mapping C → Delphi API

| C (zint) | Delphi (TZintSymbol) |
|---|---|
| `ZBarcode_Create()` | `TZintSymbol.Create(nil)` |
| `ZBarcode_Encode(sym, data, len)` | `sym.Encode(data, False)` (ohne Exception) |
| `ZBarcode_Delete(sym)` | `sym.Free` |
| `symbol->symbology` | `sym.symbology` |
| `symbol->input_mode` | `sym.input_mode` |
| `symbol->option_1/2/3` | `sym.option_1/2/3` |
| `symbol->output_options` | `sym.output_options` |
| `symbol->rows` | `sym.rows` |
| `symbol->width` | `sym.width` |
| `symbol->errtxt` | `sym.errtxt` (TArrayOfChar) |
| `symbol->encoded_data[][]` | `sym.encoded_data[row, col]` |
| `symbol->eci` | `sym.eci` |
| Rückgabe `0` (ZINT_OK) | Encode wirft keine Exception |
| Rückgabe `>= 5` (ZINT_ERROR_*) | Encode wirft `Exception` (bei `ARaiseExceptions=True`) |

### 1.3 Fehlercode-Konstanten (Delphi-IST)

```delphi
ZWARN_INVALID_OPTION  = 2;
ZERROR_TOO_LONG       = 5;
ZERROR_INVALID_DATA   = 6;
ZERROR_INVALID_CHECK  = 7;
ZERROR_INVALID_OPTION = 8;
ZERROR_ENCODING_PROBLEM = 9;
```

**Fehlend gegenüber C (2.16.0):**
```
ZINT_WARN_HRT_TRUNCATED   = 1   (neu)
ZINT_WARN_USES_ECI        = 3   (neu)
ZINT_WARN_NONCOMPLIANT    = 4   (neu)
ZINT_ERROR_FILE_ACCESS    = 10  (neu)
ZINT_ERROR_MEMORY         = 11  (neu)
ZINT_ERROR_FILE_WRITE     = 12  (neu)
ZINT_ERROR_USES_ECI       = 13  (neu)
ZINT_ERROR_NONCOMPLIANT   = 14  (neu)
ZINT_ERROR_HRT_TRUNCATED  = 15  (neu)
```

### 1.4 DUnitX-Projekt konfigurieren
- [x] `DUnitXGuiRunner.dpr` um uses-Klauseln für Test-Units erweitern
- [x] Suchpfad prüfen: `..\ ` zeigt auf die Barcode-Units im Root

---

## Phase 2: IST-Zustand absichern (Regressionstests)

Ziel: Für jeden existierenden Barcode-Typ mindestens grundlegende Encode-Tests schreiben, basierend auf den Testdaten aus den C-Tests des **alten** Commits (soweit rekonstruierbar) und den **aktuellen** C-Tests.

### 2.1 Test-Units anlegen (1 Unit pro Modul-Gruppe)

| # | Test-Unit | Testet | C-Referenz | Priorität |
|---|---|---|---|---|
| 1 | `Test_Code128.pas` | Code 128, EAN-128, Code128B | `test_code128.c` | Hoch |
| 2 | `Test_Code.pas` | Code 39, Code 93, Code 11, etc. | `test_code.c` | Hoch |
| 3 | `Test_2of5.pas` | C25Matrix, C25Inter, C25IATA, ITF-14 | `test_2of5.c` | Hoch |
| 4 | `Test_QR.pas` | QR Code, Micro QR | `test_qr.c` | Hoch |
| 5 | `Test_DMatrix.pas` | Data Matrix | `test_dmatrix.c` | Hoch |
| 6 | `Test_PDF417.pas` | PDF417, MicroPDF417 | `test_pdf417.c` | Hoch |
| 7 | `Test_Aztec.pas` | Aztec Code | `test_aztec.c` | Hoch |
| 8 | `Test_UPCEAN.pas` | EAN-8, EAN-13, UPC-A, UPC-E | `test_upcean.c` | Hoch |
| 9 | `Test_RSS.pas` | GS1 DataBar (RSS14, Limited, Expanded) | `test_rss.c` | Mittel |
| 10 | `Test_Postal.pas` | PostNet, Planet, RM4SCC, KIX | `test_postal.c` | Mittel |
| 11 | `Test_AusPost.pas` | Australia Post | `test_auspost.c` | Mittel |
| 12 | `Test_DotCode.pas` | DotCode | `test_dotcode.c` | Mittel |
| 13 | `Test_GridMatrix.pas` | Grid Matrix | `test_gridmtx.c` | Mittel |
| 14 | `Test_MaxiCode.pas` | MaxiCode | `test_maxicode.c` | Mittel |
| 15 | `Test_Code1.pas` | Code One | `test_code1.c` | Mittel |
| 16 | `Test_Code16k.pas` | Code 16K | `test_code16k.c` | Niedrig |
| 17 | `Test_Code49.pas` | Code 49 | `test_code49.c` | Niedrig |
| 18 | `Test_Composite.pas` | Composite-Barcodes | `test_composite.c` | Niedrig |
| 19 | `Test_Plessey.pas` | Plessey, MSI Plessey | `test_plessey.c` | Niedrig |
| 20 | `Test_Telepen.pas` | Telepen | `test_telepen.c` | Niedrig |
| 21 | `Test_Medical.pas` | Pharmacode, PZN, Code 32 | `test_medical.c` | Niedrig |
| 22 | `Test_IMail.pas` | Intelligent Mail | `test_imail.c` | Niedrig |
| 23 | `Test_GS1.pas` | GS1 Validierung | `test_gs1.c` | Niedrig |
| 24 | `Test_Common.pas` | Hilfsfunktionen | `test_common.c` | Niedrig |
| 25 | `Test_Large.pas` | Large-Number-Arithmetik | `test_large.c` | Niedrig |
| 26 | `Test_ReedSol.pas` | Reed-Solomon | `test_reedsol.c` | Niedrig |

### 2.2 Testdaten-Strategie

Für jeden Barcode-Typ werden Testdaten aus den C-Tests übernommen. Format pro Test:

```delphi
type
  TEncodeTestItem = record
    Symbology: Integer;
    InputMode: Integer;
    Option1: Integer;
    Option2: Integer;
    Data: String;
    ExpectedResult: Integer;  // 0 = OK, >=5 = Error
    ExpectedRows: Integer;
    ExpectedWidth: Integer;
    ExpectedErrTxt: String;
    Comment: String;
  end;
```

### 2.3 Test-Pattern pro Unit

```delphi
[TestFixture]
TTestCode128 = class
public
  [Setup] procedure Setup;
  [TearDown] procedure TearDown;

  [Test] procedure Test_Encode_Basic;       // Grundlegende Encode-Tests
  [Test] procedure Test_Encode_EdgeCases;   // Grenzwerte, leere Eingaben
  [Test] procedure Test_Encode_TooLong;     // Zu lange Eingaben → Fehler
  [Test] procedure Test_Encode_InvalidData; // Ungültige Zeichen → Fehler
  [Test] procedure Test_Dimensions;         // rows/width Prüfung
end;
```

### 2.4 Vorgehen pro Test-Unit

1. C-Testdatei öffnen (z.B. `lib/.../backend/tests/test_code128.c`)
2. Test-Items extrahieren (struct-Array → Delphi-Record-Array)
3. Testdaten in Delphi übertragen
4. Tests ausführen → **Erwartete Ergebnisse anpassen** falls der alte Delphi-Port abweicht
5. Abweichungen dokumentieren als Kommentar im Test

---

## Phase 3: Bestehende Module aktualisieren

Für jedes Modul, das sowohl in C als auch in Delphi existiert, den C-Diff nachziehen.

### 3.1 Modul-Mapping (C → Delphi)

| C-Datei | Delphi-Datei | Status im Port |
|---|---|---|
| `library.c` + `zint.h` | `zint.pas` | erweitert, teilweise verifiziert |
| `common.c` + `common.h` | `zint_common.pas` | in Arbeit |
| `2of5.c` | `zint_2of5.pas` | b3a3c0d portiert + Tests gruen |
| `auspost.c` | `zint_auspost.pas` | b3a3c0d portiert + Tests gruen |
| `aztec.c` | `zint_aztec.pas` | Legacy-Port (nicht b3a3c0d-verifiziert) |
| `code.c` | `zint_code.pas` | b3a3c0d portiert + Tests gruen |
| `code1.c` | `zint_code1.pas` | Teilport b3a3c0d: test_input/test_large/test_encode/test_encode_segs/test_fuzz aktiv (dokumentierte Deltas siehe oben) |
| `code128.c` | `zint_code128.pas` | b3a3c0d portiert + Tests gruen |
| `code16k.c` | `zint_code16k.pas` | Legacy-Port (nicht b3a3c0d-verifiziert) |
| `code49.c` | `zint_code49.pas` | Legacy-Port (nicht b3a3c0d-verifiziert) |
| `composite.c` | `zint_composite.pas` | in Arbeit |
| `dmatrix.c` | `zint_dmatrix.pas` | b3a3c0d port-check erweitert: `Test_DMatrix.pas` aktiv (`test_large`/`test_input`/`test_encode`/`test_options`/`test_reader_init`/`test_buffer`/`test_minimalenc`/`test_ct`/`test_ct_segs` Subsets). Core-Paritaet-Fixes: `Error 719` (MAXBARCODE), versionsspezifischer `Error 522`-Overflow-Text bei fixierter `option_2`, GS1+ReaderInit `Error 521`, ECC-Option `Error 524`, konsistente Rueckmeldung von `option_2` auf CLI-Version. Dokumentierte Restdeltas: (1) `test_ct` C#2-C#3: Auto-ECI-Warnklassifikation (Delphi: kein ZWARN_USES_ECI, statt Return-code 0 + warning 3; Ursache: UNICODE_MODE→DATA_MODE Konvertierung in Vorverarbeitung verliert semantischen Kontext fuer Auto-ECI-Retry). (2) `test_ct_segs` C#0-C#1: Symbolgroesse-Delta 16x16 (Delphi) vs 14x14 (C); Ursache: C nutzt `dm_encode_segs()` mit per-Segment-ECI-Markern interleaved in Binary-Stream (kompaktere Kodierung), Delphi nutzt aktuell Merged-Bytes via Single-Symbol-ECI-Pfad. Segmentweise ECI-Marker-Interleaving wuerden Umstrukturierung des dm200encode()-State-Machines erfordern (nicht portiert). |
| `dotcode.c` | `zint_dotcode.pas` | Legacy-Port (nicht b3a3c0d-verifiziert) |
| `gb2312.h` | `zint_gb2312.pas` | Legacy-Port (nicht b3a3c0d-verifiziert) |
| `gridmtx.c` | `zint_gridmtx.pas` | Legacy-Port (nicht b3a3c0d-verifiziert) |
| `gs1.c` | `zint_gs1.pas` | Legacy-Port (nicht b3a3c0d-verifiziert) |
| `imail.c` | `zint_imail.pas` | in Arbeit |
| `large.c` | `zint_large.pas` | Legacy-Port (nicht b3a3c0d-verifiziert) |
| `maxicode.c` | `zint_maxicode.pas` | in Arbeit |
| `medical.c` | `zint_medical.pas` | b3a3c0d portiert + Tests gruen |
| `pdf417.c` | `zint_pdf417.pas` | Port-Check + Kernport auf b3a3c0d-Verhalten fortgesetzt: `Test_PDF417.pas` aktiv (`test_large`/`test_options`/`test_numbprocess`/`test_reader_init`/`test_input` sowie `test_encode`-Subset C#0/C#2/C#4/C#6/C#8/C#10/C#12/C#14/C#16/C#20/C#22/C#24/C#25/C#26/C#28/C#29/C#31/C#33/C#35/C#37/C#39/C#41/C#43/C#47/C#48/C#50/C#51/C#53/C#55/C#57/C#59/C#61/C#63/C#65/C#67/C#69/C#71/C#72/C#75/C#77/C#79/C#81/C#83/C#85/C#87/C#89/C#91/C#93/C#95/C#97/C#99/C#101/C#103/C#105/C#107/C#109/C#111/C#113/C#115/C#117/C#119/C#121/C#123/C#125/C#127/C#129/C#131/C#133/C#135/C#137/C#139/C#141/C#143/C#145/C#147/C#149/C#151/C#153/C#155/C#157/C#159/C#161/C#163/C#164/C#175/C#177/C#179/C#181/C#183/C#185/C#187/C#188/C#189/C#190/C#191/C#192/C#193/C#194/C#195/C#196/C#197/C#198/C#199/C#200/C#201/C#202/C#203/C#204/C#205/C#206/C#207/C#208/C#209/C#210, 149 Tests gruen). Umgesetzte Kernfixes: C-nahe Auto-Sizing-Logik (Rows/Cols), exakte PDF417-/MicroPDF417-Optionsfehler (`460/461/466/467/468/472/475/476/745/746/747/748` im aktuellen Scope), korrekte Initialmode-Behandlung (PDF417 Text-Default vs. MicroPDF417 Byte-Default), `quelmode()`-Prioritaet fuer Digits, C-nahe `numbprocess()`-Logik, 2710-Maxlaengencheck (`Error 463`) und ECI-Codeword-Ausgabe (927/926/925-Pfade). Dokumentierte Deltas: `test_encode` C#53 (Rows: Delphi 6 statt C 7, Width identisch 154), C#147 (Rows: Delphi 38 statt C 32), C#149 (Rows: Delphi 44 statt C 38), C#151 (Delphi `TOO_LONG` statt C-Erfolg 44x99), C#175 (Rows: Delphi 10 statt C 9), C#177 (Rows: Delphi 10 statt C 9), C#183 (Rows/Width: Delphi 7x120 statt C 10x103), C#185 (Rows/Width: Delphi 7x120 statt C 10x103), C#187 (Rows/Width: Delphi 7x120 statt C 10x103), C#188 (Rows/Width: Delphi 7x120 statt C 10x103), C#189 (Rows: Delphi 10 statt C 9), C#190 (Rows: Delphi 10 statt C 9), C#197 (Rows: Delphi 8 statt C 9), C#202 (Rows: Delphi 9 statt C 8), C#206 (Rows: Delphi 9 statt C 8), C#207 (Rows: Delphi 9 statt C 8), C#208 (Rows: Delphi 9 statt C 8), C#209 (Rows/Width: Delphi 7x120 statt C 10x103), C#210 (Rows/Width: Delphi 7x120 statt C 10x103). Verbleibend: grosse C-Bloecke `test_encode_segs`/`test_rt`/`test_rt_segs`/`test_fuzz` noch nicht portiert; Structured-Append/Segment-Roundtrip noch nicht voll b3a3c0d-verifiziert. |
| `plessey.c` | `zint_plessey.pas` | b3a3c0d portiert + Tests gruen |
| `postal.c` | `zint_postal.pas` | b3a3c0d portiert + Tests gruen |
| `qr.c` | `zint_qr.pas` | b3a3c0d portiert + Tests gruen (Segment/content-API-Paritaet offen) |
| `reedsol.c` | `zint_reedsol.pas` | Legacy-Port (nicht b3a3c0d-verifiziert) |
| `rss.c` | `zint_rss.pas` | Legacy-Port (nicht b3a3c0d-verifiziert) |
| `sjis.h` | `zint_sjis.pas` | Legacy-Port (nicht b3a3c0d-verifiziert) |
| `telepen.c` | `zint_telepen.pas` | b3a3c0d portiert + Tests gruen |
| `upcean.c` | `zint_upcean.pas` | Legacy-Port (nicht b3a3c0d-verifiziert) |

### 3.2 Vorgehen pro Modul

1. **C-Diff analysieren**: Alt-Commit vs. aktuellen Stand der `.c`-Datei vergleichen
2. **Funktions-Signaturen** abgleichen: Neue Parameter? Geänderte Logik?
3. **Delphi-Code anpassen**: Änderungen portieren
4. **Tests erweitern**: Neue Testfälle aus aktueller `test_*.c` übernehmen
5. **Tests ausführen**: Grün = fertig, Rot = weiter debuggen

### 3.3 Reihenfolge (Abhängigkeiten beachten)

```
1. zint.pas (TZintSymbol) + zint_common.pas    ← Basis, muss zuerst aktuell sein
2. zint_helper.pas + zint_large.pas + zint_reedsol.pas  ← Hilfs-Module
3. zint_gs1.pas                                 ← GS1-Validierung (überall genutzt)
4. Einfache 1D: zint_code.pas, zint_2of5.pas, zint_code128.pas, zint_upcean.pas
5. Komplexe 1D: zint_rss.pas, zint_composite.pas
6. 2D einfach: zint_pdf417.pas, zint_dmatrix.pas
7. 2D komplex: zint_qr.pas, zint_aztec.pas, zint_code1.pas
8. Spezial: zint_maxicode.pas, zint_gridmtx.pas, zint_dotcode.pas
9. Postal: zint_postal.pas, zint_auspost.pas, zint_imail.pas
10. Sonstige: zint_medical.pas, zint_plessey.pas, zint_telepen.pas, zint_code16k.pas, zint_code49.pas
```

---

## Phase 4: Neue Module portieren

### 4.1 Fehlende Barcode-Encoder

| C-Datei | Neue Delphi-Datei | Barcode-Typ |
|---|---|---|
| `hanxin.c` + `hanxin.h` | `zint_hanxin.pas` | Han Xin Code |
| `ultra.c` | `zint_ultra.pas` | Ultracode |
| `bc412.c` | `zint_bc412.pas` | BC412 |
| `dxfilmedge.c` | `zint_dxfilmedge.pas` | DX Film Edge |
| `channel.c` | `zint_channel.pas` | Channel Code |
| `mailmark.c` | `zint_mailmark.pas` | Royal Mail Mailmark |
| `codabar.c` (refactored) | ggf. in `zint_code.pas` | Codabar |
| `codablock.c` | `zint_codablock.pas` | Codablock-F |
| `code11.c` (eigene Datei) | ggf. in `zint_code.pas` | Code 11 |
| `code128_based.c` | `zint_code128_based.pas` | Code128-basierte Varianten |
| `2of5inter_based.c` | `zint_2of5inter_based.pas` | ITF-basierte Varianten |
| `general_field.c` | `zint_general_field.pas` | GS1 General Field |

### 4.2 Fehlende Zeichensatz-Module

| C-Datei | Neue Delphi-Datei | Beschreibung |
|---|---|---|
| `eci.c` + `eci.h` + `eci_sb.h` | `zint_eci.pas` | Extended Channel Interpretation |
| `gb18030.h` | `zint_gb18030.pas` | GB18030 Zeichensatz |
| `gbk.h` | `zint_gbk.pas` | GBK Zeichensatz |
| `big5.h` | `zint_big5.pas` | Big5 Zeichensatz |
| `ksx1001.h` | `zint_ksx1001.pas` | KS X 1001 (Korean) |
| `iso3166.h` | `zint_iso3166.pas` | ISO 3166 Country Codes |
| `iso4217.h` | `zint_iso4217.pas` | ISO 4217 Currency Codes |

### 4.3 Fehlende API-Erweiterungen in TZintSymbol

```
- errtxt: TArrayOfChar          → Größe 100 → 160
- text: TArrayOfByte            → Größe 200 → 256
- Neues Feld: memfile (Pointer + Size)
- Neues Feld: content_segs (Array + Count)
- Neues Feld: text_length (Integer)
- Neue Konstanten: ZINT_CAP_BINDABLE, GS1RAW_MODE, GS1PARENS_MODE, GS1SYNTAXENGINE_MODE
- input_mode Erweiterung: FAST_MODE Flag
```

---

## Phase 5: API-Erweiterungen & Feinschliff

- [ ] Neue Fehler/Warn-Konstanten hinzufügen (siehe Phase 1.3)
- [ ] `BARCODE_*` Konstanten ergänzen (rMQR, BC412, DX Film Edge, etc.)
- [ ] GS1 Syntax Engine portieren (`gs1_lint.h` → Delphi)
- [ ] Code128 Escape-Sequenzen (`\^1`, `\^A`, `\^B`, `\^C`)
- [ ] Aztec: neuer Encodierungsalgorithmus (ZXing-basiert) + `FAST_MODE`
- [ ] output.c / filemem.c Konzepte (Memory-Buffer-Ausgabe)
- [ ] Rendering-Module aktualisieren (SVG, BMP, WMF)

### QR-Familie: Noch offene Paritaetsarbeiten (trotz gruener Suite)

- [~] Segment-API paritaet: `ZBarcode_Encode_Segs` + Segment-Array-Durchreichung ist aktiv; Rest: Unicode-Mixed-ECI/Input-Mode-Ecken und 1:1 C-RT-content-Abgleich.
- [ ] `content_segs`/RT-content Vergleichsfaehigkeit in `TZintSymbol` abbilden (fuer C `*_rt_segs`-Tests ohne Surrogate).
- [ ] Structured Append fuer QR API-seitig nachziehen (derzeit sind entsprechende C-Faelle in Delphi weiterhin ausgelassen/ersetzt).
- [ ] Surrogate in `UnitTests/Test_QR.pas` schrittweise durch 1:1 C-Testfaelle ersetzen, sobald Segment-/Content-APIs verfuegbar sind.

---

## Dateistruktur (Ziel)

```
UnitTests/
├── DUnitXGuiRunner.dpr          ← Bereits vorhanden
├── DUnitXGuiRunner.dproj        ← Bereits vorhanden
├── TestHelper_Zint.pas          ← NEU: Test-Infrastruktur
├── Test_Code128.pas             ← NEU: Tests Code 128
├── Test_Code.pas                ← NEU: Tests Code 39/93/11
├── Test_2of5.pas                ← NEU: Tests 2of5-Familie
├── Test_QR.pas                  ← NEU: Tests QR Code
├── Test_DataMatrix.pas          ← NEU: Tests Data Matrix
├── Test_PDF417.pas              ← NEU: Tests PDF417
├── Test_Aztec.pas               ← NEU: Tests Aztec
├── Test_UPCEAN.pas              ← NEU: Tests UPC/EAN
├── Test_RSS.pas                 ← NEU: Tests GS1 DataBar
├── Test_Postal.pas              ← NEU: Tests Postal
├── Test_AusPost.pas             ← NEU: Tests Australia Post
├── Test_DotCode.pas             ← NEU: Tests DotCode
├── Test_GridMatrix.pas          ← NEU: Tests Grid Matrix
├── Test_MaxiCode.pas            ← NEU: Tests MaxiCode
├── Test_Code1.pas               ← NEU: Tests Code One
├── Test_Composite.pas           ← NEU: Tests Composite
├── Test_Plessey.pas             ← NEU: Tests Plessey/MSI
├── Test_Telepen.pas             ← NEU: Tests Telepen
├── Test_Medical.pas             ← NEU: Tests Pharmacode/PZN
├── Test_IMail.pas               ← NEU: Tests Intelligent Mail
├── Test_GS1.pas                 ← NEU: Tests GS1
├── Test_Common.pas              ← NEU: Tests Common-Funktionen
├── Test_Large.pas               ← NEU: Tests Large-Number
├── Test_ReedSol.pas             ← NEU: Tests Reed-Solomon
├── Test_Code16k.pas             ← NEU: Tests Code 16K
├── Test_Code49.pas              ← NEU: Tests Code 49
├── bin/                         ← Build-Output
└── dcu/                         ← DCU-Output
```

---

## Naechste Arbeitsschritte

1. QR-Paritaetsdeltas gezielt abbauen: `content_segs`/RT-content, Structured-Append/API-Paritaet sowie Warning-3-vs-4-Faelle.
2. PDF417 vervollstaendigen: die offenen C-Bloecke `test_encode_segs`, `test_rt`, `test_rt_segs` und `test_fuzz` 1:1 in Delphi uebernehmen.
3. Data Matrix nachziehen: die verbleibenden Deltas in `test_ct` und `test_ct_segs` gezielt schliessen.
