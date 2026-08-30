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
faellt darueber: die zugehoerige C-Testsuite prueft `symbol->height` nirgends,
der blinde Fleck ist also aus C geerbt. (Fuer postal und code128 gilt das
nicht - dort wurden vorhandene C-Assertions beim Portieren weggelassen.)

_(keine)_

Bis zum 2026-08-30 setzte `australia_post` `row_height` fest auf 3/2/3, statt
wie C bei `COMPLIANT_HEIGHT` 3.7/2.6 zu setzen und `zint_daft_set_height(7.0,
14.0)` zu rufen. Die Hoehenlogik ist portiert (Zweig `chore/set-height`). Geprueft wird sie
ueber `UnitTests/Test_Height.pas`, erzeugt aus den Tabellen `test_height` und
`test_height_per_row` in `test_vector.c` - upstream die einzige Stelle, an der
`symbol->height` je Symbologie geprueft wird.

`daft_set_height` steht im Port in `zint_postal.pas` und ist dort ueber die
Interface-Sektion erreichbar - wie in C, wo `auspost.c` den Helfer aus
`postal.c` deklariert.

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `auspost.c` | `zint_auspost.pas` | b3a3c0d portiert + Tests gruen

