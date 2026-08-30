# rss

GS1 DataBar (RSS14, Limited, Expanded)

| | |
|---|---|
| **Status** | Legacy-Port, nicht gegen b3a3c0d verifiziert |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/rss.c` |
| **C-Tests** | `.../backend/tests/test_rss.c` |
| **Delphi-Unit** | [zint_rss.pas](../../zint_rss.pas) |
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
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `rss.c` | `zint_rss.pas` | Legacy-Port (nicht b3a3c0d-verifiziert)

