# dmatrix

Data Matrix inkl. DMRE

| | |
|---|---|
| **Status** | Teilport (b3a3c0d) mit dokumentierten Deltas |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/dmatrix.c` |
| **C-Tests** | `.../backend/tests/test_dmatrix.c` |
| **Delphi-Unit** | [zint_dmatrix.pas](../../zint_dmatrix.pas) |
| **Test-Unit** | [UnitTests/Test_DMatrix.pas](../../UnitTests/Test_DMatrix.pas) |
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
| `test_input` C#20, `UNICODE_MODE or FAST_MODE`, Daten `ABCDEF` | rows 12 (= C) | rows 14 | Nicht die Datenuebergabe: unter FPC nachgemessen kommen 6 Bytes an, `ustrlen` = 6. Die Abweichung entsteht im UNICODE_MODE-Pfad des Encoders. Der Fall wird im FPC-Lauf uebersprungen (`{$IFDEF FPC}` in `Test_DMatrix.pas`), damit die C-Erwartung sichtbar bleibt. |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `dmatrix.c` | `zint_dmatrix.pas` | b3a3c0d port-check erweitert: `Test_DMatrix.pas` aktiv (`test_large`/`test_input`/`test_encode`/`test_options`/`test_reader_init`/`test_buffer`/`test_minimalenc`/`test_ct`/`test_ct_segs` Subsets). Core-Paritaet-Fixes: `Error 719` (MAXBARCODE), versionsspezifischer `Error 522`-Overflow-Text bei fixierter `option_2`, GS1+ReaderInit `Error 521`, ECC-Option `Error 524`, konsistente Rueckmeldung von `option_2` auf CLI-Version, `test_ct_segs` C#0-C#1 auf 14x14-C-Paritaet sowie `test_ct` C#2-C#3 Auto-ECI-Warnklassifikation auf C-Paritaet (Thai/UNICODE, `ZWARN_USES_ECI`, ECI 13).

