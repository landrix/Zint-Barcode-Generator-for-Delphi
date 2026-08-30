# telepen

Telepen, Telepen Numeric

| | |
|---|---|
| **Status** | Portiert (b3a3c0d) + Tests gruen |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/telepen.c` |
| **C-Tests** | `.../backend/tests/test_telepen.c` |
| **Delphi-Unit** | [zint_telepen.pas](../../zint_telepen.pas) |
| **Test-Unit** | [UnitTests/Test_Telepen.pas](../../UnitTests/Test_Telepen.pas) |
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

- `zint_telepen.pas` | ✅ b3a3c0d | ✅ `Test_Telepen.pas` (48 Tests) | **Erstes portiertes Modul**
- `telepen.c` | `zint_telepen.pas` | b3a3c0d portiert + Tests gruen

