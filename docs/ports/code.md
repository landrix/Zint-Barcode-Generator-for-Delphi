# code

Code11, C39, EC39, LOGMARS, C93, VIN, HIBC_39

| | |
|---|---|
| **Status** | Portiert (b3a3c0d) + Tests gruen |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/code.c` |
| **C-Tests** | `.../backend/tests/test_code.c` |
| **Delphi-Unit** | [zint_code.pas](../../zint_code.pas) |
| **Test-Unit** | [UnitTests/Test_Code.pas](../../UnitTests/Test_Code.pas) |
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
| `zint_code39` (`code.c:198-211`) | partial | Hoehenlogik |
| `zint_code93` (`code.c:404-407`) | partial | Hoehenlogik |
| `zint_excode39` (`code.c:270-284`) | partial | Zwei Dinge: die Hoehenlogik ueber `zint_code39`, und `option_2` wird nicht zurueckgesetzt. C merkt sich den Wert, setzt ihn fuer den Aufruf auf 1 und stellt danach 2 wieder her (`code.c:282-283`); `ec39` merkt sich den Wert in `have_option_2`, nutzt ihn fuer die HRT-Entscheidung, setzt `symbol.option_2` aber nie zurueck. Wer dasselbe Symbol erneut kodiert, bekommt die Pruefziffer sichtbar statt versteckt |

Gemeinsame Ursache: der Port kennt kein `z_set_height`. Portweite Beschreibung
in [library.md](library.md), Abschnitt *Querschnittsdeltas*.

`zint_code11` (`code11.c`) und `zint_vin` sind vollstaendig portiert und
enthalten in C keine Hoehenlogik.

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_code.pas` | ✅ b3a3c0d | ✅ `Test_Code.pas` (87 Tests) | Code11, C39, EC39, LOGMARS, C93, VIN, HIBC_39
- `code.c` | `zint_code.pas` | b3a3c0d portiert + Tests gruen

