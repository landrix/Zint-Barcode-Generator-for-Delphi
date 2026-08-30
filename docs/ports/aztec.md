# aztec

Aztec Code + Aztec Runes

| | |
|---|---|
| **Status** | Teilport (b3a3c0d) mit dokumentierten Deltas |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/aztec.c` |
| **C-Tests** | `.../backend/tests/test_aztec.c` |
| **Delphi-Unit** | [zint_aztec.pas](../../zint_aztec.pas) |
| **Test-Unit** | [UnitTests/Test_Aztec.pas](../../UnitTests/Test_Aztec.pas) |
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

- `zint_aztec.pas` | 🟡 Legacy-Port + gezielte Paritaetsfixes | ✅ `Test_Aztec.pas` (5 Tests / C-Subsets aus `test_large`, `test_options`, `test_encode`, `test_fuzz`) | Neue Aztec-Fixture aktiv und erweitert (zuletzt `test_options` C#25, C#28, C#29, C#38, C#50). Gefixt: `aztec_runes()` Laenge > 3 -> `ZERROR_TOO_LONG`, `READER_INIT + layers > 22` -> `ZERROR_INVALID_OPTION`. Dokumentierte Restdeltas: GS1+READER_INIT `input_mode`-Reset in `ZBarcode_Encode`, fehlender `FAST_MODE`, fehlende C-Warnings (`ZWARN_NONCOMPLIANT`) und encoderabhaengige Groessen-/Kapazitaetsabweichungen ggü. b3a3c0d.
- - Zielgerichtete Paritaetsfixes in `zint_aztec.pas`: `aztec_runes()` laenge > 3 -> `ZERROR_TOO_LONG`, `READER_INIT + layers > 22` -> `ZERROR_INVALID_OPTION`.


### Aztec Detailstand (Session 2026-03-30)

Hinweis Aztec (Session 2026-03-30):
- Aztec-Fixture `Test_Aztec.pas` wurde erweitert mit C-Subsets (`test_large`, `test_options`, `test_encode`, `test_fuzz`).
- Zielgerichtete Paritaetsfixes in `zint_aztec.pas`: `aztec_runes()` laenge > 3 -> `ZERROR_TOO_LONG`, `READER_INIT + layers > 22` -> `ZERROR_INVALID_OPTION`.
- Dokumentierte Restdeltas: GS1+READER_INIT `input_mode`-Reset in `ZBarcode_Encode()`, fehlender `FAST_MODE`, fehlende C-Warnings/ECI-Auto-Klassifikation, Groessen-/Kapazitaetsabweichungen gegenueber b3a3c0d.
- Full-Gate Status: Alle Tests gruen (Code1 Stack-Overflow Workaround + PDF417 Width-Adjustment + MicroQR ECI-Auto Expectation = alle kompensiert).

