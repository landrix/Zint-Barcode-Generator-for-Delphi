# code

Code11, C39, EC39, LOGMARS, C93, VIN, HIBC_39

| | |
|---|---|
| **Status** | Portiert (b3a3c0d) + Tests gruen |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/code.c` |
| **C-Tests** | `.../backend/tests/test_code.c` |
| **Delphi-Unit** | [zint_code.pas](../../zint_code.pas) |
| **Test-Unit** | [UnitTests/Test_Code.pas](../../UnitTests/Test_Code.pas) |
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

- `zint_code.pas` | ✅ b3a3c0d | ✅ `Test_Code.pas` (87 Tests) | Code11, C39, EC39, LOGMARS, C93, VIN, HIBC_39
- `code.c` | `zint_code.pas` | b3a3c0d portiert + Tests gruen

