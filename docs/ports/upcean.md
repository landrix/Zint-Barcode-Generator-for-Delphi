# upcean

EAN-8, EAN-13, UPC-A, UPC-E

| | |
|---|---|
| **Status** | Legacy-Port, nicht gegen b3a3c0d verifiziert |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/upcean.c` |
| **C-Tests** | `.../backend/tests/test_upcean.c` |
| **Delphi-Unit** | [zint_upcean.pas](../../zint_upcean.pas) |
| **Test-Unit** | _(keine)_ |
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
| `BARCODE_EANX`, Eingabe `4012345678901` (13 Ziffern) | nicht gemessen | ret=5, errtxt `error: Invalid length input` | Noch nicht untersucht; ob Delphi hier abweicht, ist offen. Aufgefallen im FPC-Smoke-Test von `chore/fpc-gate` (2026-08-30). Modul ist Legacy-Port ohne Testabdeckung. |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `upcean.c` | `zint_upcean.pas` | Legacy-Port (nicht b3a3c0d-verifiziert)

