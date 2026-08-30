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
faellt darueber, weil die portierten C-Testsuiten die Hoehe nicht pruefen.

| C-Funktion | Status | Fehlt im Port |
|---|---|---|
| `zint_code39` (`code.c:198-211`) | partial | Hoehenlogik |
| `zint_code93` (`code.c:404-407`) | partial | Hoehenlogik |

Gemeinsame Ursache: der Port kennt kein `z_set_height`. Portweite Beschreibung
in [library.md](library.md), Abschnitt *Querschnittsdeltas*.

`zint_code11` (`code11.c`) und `zint_excode39`/`zint_vin` sind vollstaendig
portiert und enthalten in C keine Hoehenlogik.

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_code.pas` | ✅ b3a3c0d | ✅ `Test_Code.pas` (87 Tests) | Code11, C39, EC39, LOGMARS, C93, VIN, HIBC_39
- `code.c` | `zint_code.pas` | b3a3c0d portiert + Tests gruen

