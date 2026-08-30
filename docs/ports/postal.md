# postal

PostNet, Planet, RM4SCC, KIX, CEPNet, FIM

| | |
|---|---|
| **Status** | Portiert (b3a3c0d) + Tests gruen |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/postal.c` |
| **C-Tests** | `.../backend/tests/test_postal.c` |
| **Delphi-Unit** | [zint_postal.pas](../../zint_postal.pas) |
| **Test-Unit** | [UnitTests/Test_Postal.pas](../../UnitTests/Test_Postal.pas) |
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

Die Eintraege stammen nicht aus einem Testlauf, sondern aus dem
Funktionsinventar (`docs/ports/_functions.tsv`): C-Funktionen, zu denen es im
Port keine oder nur eine unvollstaendige Entsprechung gibt.

**Hier war es kein geerbter blinder Fleck.** `test_postal.c` prueft
`symbol->height` sehr wohl - in drei Testbloecken (Zeilen 150, 203 und 396 der
Testdatei), jeweils einmal je Testfall der Schleife. `Test_Postal.pas` hatte
bei 116 Testmethoden keine einzige Hoehen-Assertion: die Faelle waren portiert,
die Assertion dabei weggelassen. Waere sie mitportiert worden, waere die Luecke
beim ersten Lauf aufgefallen.

Seit dem 2026-08-30 ist die Hoehenlogik portiert und geprueft: drei Faelle aus
`test_postal.c` in `Test_Postal.pas` sowie die POSTNET-, PLANET-, CEPNET-,
RM4SCC-, KIX-, DAFT-, JAPANPOST- und FIM-Zeilen aus `test_height`
(`test_vector.c`) in `Test_Height.pas`.

_(keine)_

Mitportiert wurde dabei auch das Trackerverhaeltnis ueber `option_2` in
`zint_daft` (`postal.c:626`): C erlaubt Werte von 50 bis 900 in Tausendsteln
und setzt `symbol.height` auf 8, wenn keine Hoehe vorgegeben ist. Der Port
wertete `option_2` fuer DAFT bis dahin gar nicht aus.

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_postal.pas` | ✅ b3a3c0d | ✅ `Test_Postal.pas` (84 Tests) | Alle 12 Funktionen inkl. CEPNet, FIM 'E', BARCODE_CONTENT_SEGS-Paritaet
- `postal.c` | `zint_postal.pas` | b3a3c0d portiert + Tests gruen

