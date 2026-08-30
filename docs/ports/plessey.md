# plessey

Plessey, MSI Plessey

| | |
|---|---|
| **Status** | Portiert (b3a3c0d) + Tests gruen |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/plessey.c` |
| **C-Tests** | `.../backend/tests/test_plessey.c` |
| **Delphi-Unit** | [zint_plessey.pas](../../zint_plessey.pas) |
| **Test-Unit** | [UnitTests/Test_Plessey.pas](../../UnitTests/Test_Plessey.pas) |
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

- `zint_plessey.pas` | ✅ b3a3c0d | ✅ `Test_Plessey.pas` (32 Tests) | plessey, msi_handle (alle MSI-Varianten) inkl. BARCODE_CONTENT_SEGS-Paritaet
- `plessey.c` | `zint_plessey.pas` | b3a3c0d portiert + Tests gruen

