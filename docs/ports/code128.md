# code128

Code128, Code128B, GS1-128, EAN-14, NVE-18, HIBC-128

| | |
|---|---|
| **Status** | Portiert (b3a3c0d) + Tests gruen |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/code128.c` |
| **C-Tests** | `.../backend/tests/test_code128.c` |
| **Delphi-Unit** | [zint_code128.pas](../../zint_code128.pas) |
| **Test-Unit** | [UnitTests/Test_Code128.pas](../../UnitTests/Test_Code128.pas) |
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

- `zint_code128.pas` | ✅ b3a3c0d | ✅ `Test_Code128.pas` (41 Tests) | Code128, Code128B, EAN-128/GS1-128, EAN-14, NVE-18, HIBC-128 (DAC-DM Algorithmus)
- `code128.c` | `zint_code128.pas` | b3a3c0d portiert + Tests gruen

