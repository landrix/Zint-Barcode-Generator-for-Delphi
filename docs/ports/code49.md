# code49

Code 49

| | |
|---|---|
| **Status** | Teilport (b3a3c0d) mit dokumentierten Deltas |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/code49.c` |
| **C-Tests** | `.../backend/tests/test_code49.c` |
| **Delphi-Unit** | [zint_code49.pas](../../zint_code49.pas) |
| **Test-Unit** | [UnitTests/Test_Code49.pas](../../UnitTests/Test_Code49.pas) |
| **Delphi-Gate** | offen |
| **FPC-Gate** | offen |

## Portierte C-Indizes

| C-Testblock | Indizes | Anzahl |
|---|---|---|
| `test_large` | | |
| `test_input` | | |
| `test_encode` | | |
| `test_hrt` | | |
| `test_rt` | | |
| `test_fuzz` | | |

## Bewusst ausgelassene C-Indizes

| C-Index | Grund |
|---|---|
| | |

## Offene Deltas Delphi vs C

| C-Index | Erwartet (C) | Ist (Delphi) | Ursache | Naechster Schritt |
|---|---|---|---|---|
| | | | | |

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_code49.pas` | ✅ b3a3c0d | 🟡 `Test_Code49.pas` (12 Tests / Phase 1) | **Port-check INITIATED**: C-Enumeration abgeschlossen (test_large 4, test_input 32, test_encode 4, test_rt 6 = 46 Testfälle). Delphi-Modul vollständig. Phase 1 Test-Suite: 4 Methoden (TestLargeSubset, TestInputSubset, TestEncodeSubset, TestRTSubset) mit 12 konservativen Testfällen. Nächste Phasen: numerische Encodation, min-rows Validierung, GS1-Varianten, BARCODE_CONTENT_SEGS.
- `code49.c` | `zint_code49.pas` | **b3a3c0d port-check erweitert (Phase 1+2+3+4+5, 2026-03-30)** — C-Referenz-Enumeration abgeschlossen (test_large 4 cases, test_input 32 cases, test_encode 4 cases, test_rt 6 cases, total 46 Testfälle). Code49-Delphi-Modul vollständig implementiert (zint_code49.pas mit allen Encodation-Tabellen und Logik). `UnitTests/Test_Code49.pas` aktiv mit 4 Test-Methoden (TestLargeSubset, TestInputSubset, TestEncodeSubset, TestRTSubset) und 37 konservativen Testfällen: test_large C#0-C#3, test_input C#0-C#6, C#13-C#20, C#24, C#26-C#31 (+ variant), test_encode C#0-C#3, test_rt C#0-C#5. Umgesetzte Core-Paritaetsfixes: Numeric-Shift-Guard (`i < h`), option_1-Range/MinRows, output_options-Erhalt via OR, GS1-Mode-Maskencheck, BARCODE_CONTENT_SEGS payload fuer RT-Faelle. Verbleibende dokumentierte Legacy-Delphi-Deltas vs C b3a3c0d: test_input C#13/C#14 (GS1/GS1PARENS rows 4 -> 3). Full-Gate nach Erweiterung: 822/822.

