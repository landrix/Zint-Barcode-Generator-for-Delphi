# composite

Composite-Symbole (CC-A/B/C)

| | |
|---|---|
| **Status** | Teilport (b3a3c0d) mit dokumentierten Deltas |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/composite.c` |
| **C-Tests** | `.../backend/tests/test_composite.c` |
| **Delphi-Unit** | [zint_composite.pas](../../zint_composite.pas) |
| **Test-Unit** | [UnitTests/Test_Composite.pas](../../UnitTests/Test_Composite.pas) |
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

- `composite.c` | `zint_composite.pas` | **port-check aktiv (2026-03-30)**: `UnitTests/Test_Composite.pas` mit 5 aktiven Subset-Methoden (`TestLargeSubset`, `TestInputSubset`, `TestEncodeSubset`, `TestHRTSubset`, `TestFuzzSubset`), C-Indizes sichtbar; `BARCODE_CONTENT_SEGS`-Merge (linear + `|` + Composite/FNC1) umgesetzt und EANX-Primaries C#15/C#24 auf C-Paritaet gehoben, verbleibende Restdeltas testseitig markiert (siehe oben).


### Composite Delta-Status (Session 2026-03-30)

Hinweis PDF417 + QR Delta-Status (Session 2026-03-30, Update):
- PDF417 `test_encode` C#20 ist nun auf C-Referenz-Paritaet (width 120) in `UnitTests/Test_PDF417.pas` zurueckgefuehrt.
- MicroQR `test_microqr_rt` C#0 ist nun auf C-Referenz-Paritaet (ret=0) in `UnitTests/Test_QR.pas` zurueckgefuehrt.
- Composite port-check (Session 2026-03-30): neue Fixture `UnitTests/Test_Composite.pas` aktiv mit C-indexierten Subsets aus `test_eanx_leading_zeroes`, `test_input`, `test_encodation_0`, `test_hrt`, `test_fuzz`.
- Composite offene Deltas (explizit im Test markiert): Restabweichung EANX_CC Add-on-Breite bei `test_eanx_leading_zeroes` C#34 (`width 128` vs C `125`) sowie GS1NOCHECK-Randfaelle (`[]...`) im GS1-Parser. Auf C-Paritaet geschlossen: EANX-Primaries C#15/C#24 und `BARCODE_CONTENT_SEGS` im aktiven HRT-Subset (C#2/C#10/C#27).
- Full-Gate aktuell: 827/827 gruen.

