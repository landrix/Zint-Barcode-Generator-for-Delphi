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

| C-Funktion | Status | Fehlt im Port |
|---|---|---|
| `zint_pharma` (`medical.c:97-99`) | partial | Hoehenlogik |
| `zint_pharma_two` (`medical.c:188-190`) | partial | Hoehenlogik |
| `zint_code32` (`medical.c:280-282`) | partial | Hoehenlogik |
| `zint_pzn` (`medical.c:365-369`) | partial | Hoehenlogik |

Gemeinsame Ursache: der Port kennt kein `z_set_height`. Portweite Beschreibung
in [library.md](library.md), Abschnitt *Querschnittsdeltas*.

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_medical.pas` | ✅ b3a3c0d | ✅ `Test_Medical.pas` + `Test_PZN.pas` (21 Tests) | pharma_one, pharma_two, code32, pzn inkl. BARCODE_CONTENT_SEGS-Paritaet
- `medical.c` | `zint_medical.pas` | b3a3c0d portiert + Tests gruen

