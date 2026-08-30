# medical

Pharmacode, Code32, PZN

| | |
|---|---|
| **Status** | Portiert (b3a3c0d) + Tests gruen |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/medical.c` |
| **C-Tests** | `.../backend/tests/test_medical.c` |
| **Delphi-Unit** | [zint_medical.pas](../../zint_medical.pas) |
| **Test-Unit** | [UnitTests/Test_Medical.pas](../../UnitTests/Test_Medical.pas) |
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

Bis zum 2026-08-30 fehlte in `zint_pharma`, `zint_pharma_two`, `zint_code32`
und `zint_pzn` die Hoehenlogik. Die Hoehenlogik ist portiert (Zweig `chore/set-height`). Geprueft wird sie
ueber `UnitTests/Test_Height.pas`, erzeugt aus den Tabellen `test_height` und
`test_height_per_row` in `test_vector.c` - upstream die einzige Stelle, an der
`symbol->height` je Symbologie geprueft wird.

`codabar` liegt im Port in `zint_medical.pas`, stammt aber aus einem aelteren
Zint-Stand und ist gegen `b3a3c0d` nicht portiert (Modul `codabar`, Status
`missing`). Seine Hoehenlogik aus `codabar.c:124-135` fehlt entsprechend und
gehoert in den Portierungsvorgang fuer Codabar, nicht hierher.

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_medical.pas` | ✅ b3a3c0d | ✅ `Test_Medical.pas` + `Test_PZN.pas` (21 Tests) | pharma_one, pharma_two, code32, pzn inkl. BARCODE_CONTENT_SEGS-Paritaet
- `medical.c` | `zint_medical.pas` | b3a3c0d portiert + Tests gruen

