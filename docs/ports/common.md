# common

Gemeinsame Hilfsfunktionen

| | |
|---|---|
| **Status** | Teilport (b3a3c0d) mit dokumentierten Deltas |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/common.c` |
| **C-Tests** | `.../backend/tests/test_common.c` |
| **Delphi-Unit** | [zint_common.pas](../../zint_common.pas) |
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
| n/a (`z_set_height`) | Jeder Encoder setzt am Ende `symbol.height`, je nach `COMPLIANT_HEIGHT` unterschiedlich | Funktion nicht portiert, `symbol.height` bleibt 0 | Der Port kennt kein `z_set_height`; Hoehe entsteht erst beim Rendern aus `row_height` | Als eigenen Vorgang portieren, siehe [library.md](library.md), Abschnitt Querschnittsdeltas |
| | | | | |

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_common.pas` | 🔧 Bugfix + Basis-Checks | ✅ `Test_CommonCore.pas` | `posn()`-Regression abgesichert; Basischecks fuer `ctoi/itoc/ustrlen/is_stackable/is_extendable/istwodigits/froundup`
- `common.c` + `common.h` | `zint_common.pas` | in Arbeit

