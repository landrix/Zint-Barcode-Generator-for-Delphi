# library

Kern-API, Dispatch, TZintSymbol

| | |
|---|---|
| **Status** | Teilport (b3a3c0d) mit dokumentierten Deltas |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/library.c` |
| **C-Tests** | `.../backend/tests/test_library.c` |
| **Delphi-Unit** | [zint.pas](../../zint.pas) |
| **Test-Unit** | [UnitTests/Test_CommonCore.pas](../../UnitTests/Test_CommonCore.pas) |
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

- `zint.pas` | 🔧 Erweitert + Basis-Checks | ✅ `Test_CommonCore.pas` | +BARCODE_CEPNET=54, +ZWARN_NONCOMPLIANT=4, +BARCODE_VIN=73, Dispatch; Basischecks fuer `gs1_compliant/supports_eci` und Symbology-Mapping
- `library.c` + `zint.h` | `zint.pas` | erweitert, teilweise verifiziert

