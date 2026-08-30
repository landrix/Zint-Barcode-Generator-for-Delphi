# code128

Code128, Code128B, GS1-128, EAN-14, NVE-18, HIBC-128

| | |
|---|---|
| **Status** | Portiert (b3a3c0d) + Tests gruen |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/code128.c` |
| **C-Tests** | `.../backend/tests/test_code128.c` |
| **Delphi-Unit** | [zint_code128.pas](../../zint_code128.pas) |
| **Test-Unit** | [UnitTests/Test_Code128.pas](../../UnitTests/Test_Code128.pas) |
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
| `zint_code128` (`code128.c:396`) | partial | `EXTRA_ESCAPE_MODE`. C deutet `\^C` als manuellen Code-Set-Wechsel; der Port kennt das Flag nicht (`EXTRA_ESCAPE_MODE` kommt in `zint_code128.pas` nicht vor) und verarbeitet die Zeichen als normale Daten |
| `zint_gs1_128_cc` (`code128.c:622-645`) | partial | Hoehenlogik, siehe [library.md](library.md) |
| `zint_dpd` (`code128_based.c`) | **missing** | kein Pascal-Gegenstueck. DPD-Symbologie ist nicht portiert |
| `zint_upu_s10` (`code128_based.c`) | **missing** | kein Pascal-Gegenstueck. UPU S10 ist nicht portiert |

`zint_dpd` und `zint_upu_s10` waren bis zum 2026-08-30 unsichtbar, weil
`code128_based.c` keinem Modul zugeordnet war. Das Inventar prueft seither, dass
jede C-Datei einem Modul gehoert oder ausdruecklich ausserhalb des Umfangs
steht.

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_code128.pas` | ✅ b3a3c0d | ✅ `Test_Code128.pas` (41 Tests) | Code128, Code128B, EAN-128/GS1-128, EAN-14, NVE-18, HIBC-128 (DAC-DM Algorithmus)
- `code128.c` | `zint_code128.pas` | b3a3c0d portiert + Tests gruen

