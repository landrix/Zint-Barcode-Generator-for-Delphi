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
| `zint_telepen.pas` | ✅ b3a3c0d | ✅ `Test_Telepen.pas` | **Erster portierter Modul** |

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
| 5 | `Test_DataMatrix.pas` | Data Matrix | `test_dmatrix.c` | Hoch |
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
| `library.c` + `zint.h` | `zint.pas` | complete |
| `common.c` + `common.h` | `zint_common.pas` | work in progress |
| `2of5.c` | `zint_2of5.pas` | complete |
| `auspost.c` | `zint_auspost.pas` | complete |
| `aztec.c` | `zint_aztec.pas` | complete |
| `code.c` | `zint_code.pas` | complete |
| `code1.c` | `zint_code1.pas` | complete |
| `code128.c` | `zint_code128.pas` | complete |
| `code16k.c` | `zint_code16k.pas` | complete |
| `code49.c` | `zint_code49.pas` | complete |
| `composite.c` | `zint_composite.pas` | work in progress |
| `dmatrix.c` | `zint_dmatrix.pas` | complete |
| `dotcode.c` | `zint_dotcode.pas` | complete |
| `gb2312.h` | `zint_gb2312.pas` | complete |
| `gridmtx.c` | `zint_gridmtx.pas` | complete |
| `gs1.c` | `zint_gs1.pas` | complete |
| `imail.c` | `zint_imail.pas` | work in progress |
| `large.c` | `zint_large.pas` | complete |
| `maxicode.c` | `zint_maxicode.pas` | work in progress |
| `medical.c` | `zint_medical.pas` | complete |
| `pdf417.c` | `zint_pdf417.pas` | complete |
| `plessey.c` | `zint_plessey.pas` | complete |
| `postal.c` | `zint_postal.pas` | complete |
| `qr.c` | `zint_qr.pas` | work in progress |
| `reedsol.c` | `zint_reedsol.pas` | complete |
| `rss.c` | `zint_rss.pas` | complete |
| `sjis.h` | `zint_sjis.pas` | complete |
| `telepen.c` | `zint_telepen.pas` | **b3a3c0d ported** |
| `upcean.c` | `zint_upcean.pas` | complete |

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

## Nächster Schritt

**→ Tests für `zint_telepen.pas` kompilieren und ausführen, dann nächstes einfaches Modul portieren (z.B. `zint_medical.pas` oder `zint_plessey.pas`).**
