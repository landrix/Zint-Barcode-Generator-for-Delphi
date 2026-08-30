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

Beide Eintraege stammen nicht aus einem Testlauf, sondern aus dem
Funktionsinventar (`docs/ports/_functions.tsv`): C-Funktionen, zu denen es im
Port keine oder nur eine unvollstaendige Entsprechung gibt. Kein Test faellt
darueber, weil `test_postal.c` die Hoehe nicht prueft.

| C-Funktion | Erwartet (C) | Ist (Delphi) | Ursache | Naechster Schritt |
|---|---|---|---|---|
| `usps_set_height` (`postal.c:92`) | `row_height[0]/[1]` = 3.225/2.15 bei `COMPLIANT_HEIGHT` oder `BARCODE_CEPNET`, sonst 6.0/6.0; zusaetzlich Halbbalken-Verhaeltnis, wenn `symbol.height` gesetzt ist | `zint_postal.pas:157` setzt fest 6.0/6.0 - nur der Nicht-Compliant-Zweig, kein Verhaeltnis | Der Port kennt keine `COMPLIANT_HEIGHT`-Behandlung fuer POSTNET/PLANET | Zusammen mit dem port-weiten `z_set_height` loesen, siehe [library.md](library.md) |
| `zint_daft_set_height` (`postal.c:400`) | Setzt die Hoehe fuer die DAFT-artigen Symbologien | keine Entsprechung | Eigenstaendige Luecke, **nicht** dieselbe wie `usps_set_height`: der Helfer wird von `zint_rm4scc`, `zint_kix`, `zint_daft`, `zint_japanpost` und zusaetzlich von `zint_auspost` (`auspost.c:275`) verwendet | Zusammen mit den vier Symbologien unten loesen |
| `zint_rm4scc`, `zint_kix`, `zint_daft`, `zint_japanpost` | `row_height` je nach `COMPLIANT_HEIGHT`, dazu `zint_daft_set_height` mit Grenzwarnung | `royal_plot`, `kix_code`, `daft_code`, `japan_post` setzen fest 3/2/3 | siehe Zeile darueber | dito |
| `zint_fim` (`postal.c:386-388`) | Hoehenlogik vorhanden | `fim` setzt keine Hoehe | Port kennt kein `z_set_height` | siehe [library.md](library.md) |

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_postal.pas` | ✅ b3a3c0d | ✅ `Test_Postal.pas` (84 Tests) | Alle 12 Funktionen inkl. CEPNet, FIM 'E', BARCODE_CONTENT_SEGS-Paritaet
- `postal.c` | `zint_postal.pas` | b3a3c0d portiert + Tests gruen

