# auspost

Australia Post

| | |
|---|---|
| **Status** | Portiert (b3a3c0d) + Tests gruen |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/auspost.c` |
| **C-Tests** | `.../backend/tests/test_auspost.c` |
| **Delphi-Unit** | [zint_auspost.pas](../../zint_auspost.pas) |
| **Test-Unit** | [UnitTests/Test_Auspost.pas](../../UnitTests/Test_Auspost.pas) |
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

## Portierungsluecken (Funktionsinventar)

Alle Eintraege stammen aus dem Funktionsinventar
(`docs/ports/_functions.tsv`), nicht aus einem Testlauf: C-Funktionen, zu denen
es im Port keine oder nur eine unvollstaendige Entsprechung gibt. Kein Test
faellt darueber, weil die portierten C-Testsuiten die Hoehe nicht pruefen.

| C-Funktion | Status | Fehlt im Port |
|---|---|---|
| `zint_auspost` (`auspost.c:263-280`) | partial | C setzt bei `COMPLIANT_HEIGHT` `row_height` 3.7/2.6 und ruft `zint_daft_set_height(7.0, 14.0)`, was auch eine Grenzwarnung erzeugen kann. `zint_auspost.pas:265-267` setzt fest 3/2/3 |

Portweite Beschreibung in [library.md](library.md), Abschnitt
*Querschnittsdeltas*; `zint_daft_set_height` selbst siehe [postal.md](postal.md).

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `auspost.c` | `zint_auspost.pas` | b3a3c0d portiert + Tests gruen

