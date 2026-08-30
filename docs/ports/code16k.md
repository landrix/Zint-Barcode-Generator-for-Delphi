# code16k

Code 16K

| | |
|---|---|
| **Status** | Teilport (b3a3c0d) mit dokumentierten Deltas |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/code16k.c` |
| **C-Tests** | `.../backend/tests/test_code16k.c` |
| **Delphi-Unit** | [zint_code16k.pas](../../zint_code16k.pas) |
| **Test-Unit** | [UnitTests/Test_Code16k.pas](../../UnitTests/Test_Code16k.pas) |
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

- `zint_code16k.pas` | ✅ b3a3c0d | ✅ `Test_Code16k.pas` (36 Tests) | **Phase 1+3 COMPLETE**: Vollständige Portierung mit 5 Test-Methoden. test_large (3 C#0,C#1,C#3), test_reader_init (2 C#0,C#3), test_input (19 C#0-C#7,C#12-C#14,C#21-C#28,C#31), test_encode (6 C#0-C#5), test_rt (6 C#0-C#5). 11 Deltas dokumentiert (TOO_LONG boundaries, GS1+READER_INIT, Unicode Multi-é encoding inefficiency).
- `code16k.c` | `zint_code16k.pas` | **b3a3c0d port-check COMPLETE (Phase 1+3, 2026-03-30)** — Vollständig portierte `UnitTests/Test_Code16k.pas` mit 5 Test-Methoden und 36 Testfällen aus C b3a3c0d: test_large (3 cases C#0,C#1,C#3), test_reader_init (2 cases C#0,C#3), test_input (19 cases: C#0-C#7,C#12-C#14,C#21-C#28,C#31), test_encode (6 cases C#0-C#5), test_rt (6 cases C#0-C#5 conservative non-content-segs subset). 13 dokumentierte Legacy-Delphi-Deltas vs C b3a3c0d: test_large C#0/C#3 (TOO_LONG boundary), test_reader_init C#3 (GS1+READER_INIT akzeptiert), test_input C#21/C#22/C#23/C#24 (Unicode Multi-é Encoding ineffizient, 2→4/3→5/3→6/4→10 rows), **test_input C#28** (option_1=4 min-rows Expansion nicht implementiert: 4→3 rows), **test_input C#31** (option_1=1 < 2 Bereichs-Validierung fehlt: ZINT_ERROR_INVALID_OPTION(8)→ret=0). Full-Gate GRUEN: 822/822.

