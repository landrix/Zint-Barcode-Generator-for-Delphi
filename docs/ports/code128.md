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

## Portierungsstand: von done auf partial zurueckgestuft (2026-08-30)

Der Modulstatus stand auf `done`. Das ist nicht haltbar, gemessen gegen
`test_code128.c`:

| | |
|---|---|
| C-Testfaelle | 462 |
| Delphi-Testmethoden | 163 (35 %) |
| Testmethoden mit Fehler- oder Warnungserwartung | 33 |
| Faelle mit `errtxt`-Pruefung | **0** |

Zum Vergleich: telepen 100 %, auspost 99 %, postal 89 %, 2of5 82 %, code 74 %,
plessey 67 %, medical 53 %. code128 ist das einzige der acht Module, das
`symbol.errtxt` **nirgends** prueft, obwohl `test_code128.c` es an vier Stellen
assertiert (`:137`, `:139`, `:451`, `:704`) - gefunden von
`scripts/check-c-assertions.ps1`.

Dazu kommen zwei ganze C-Bloecke fuer Symbologien, die es im Port nicht gibt:
`test_dpd_input` (32 Faelle) und `test_upu_s10_input` (30), siehe die
Inventar-Tabelle unten.

Offen fuer einen kuenftigen `port/code128`-Branch:

- `errtxt`-Assertionen fuer die 33 Fehlerfaelle nachziehen, mit den C-Texten
- die fehlenden Faelle aus `test_input` (153), `test_hrt` (70), `test_large` (41)
  und `test_encode` (53 - davon 7 portiert) durchzaehlen und portieren
- `EXTRA_ESCAPE_MODE`, `GS1PARENS_MODE`, DPD und UPU S10 (Inventar-Tabelle)

## Portierungsluecken (Funktionsinventar)

Alle Eintraege stammen aus dem Funktionsinventar
(`docs/ports/_functions.tsv`), nicht aus einem Testlauf: C-Funktionen, zu denen
es im Port keine oder nur eine unvollstaendige Entsprechung gibt. Kein Test
faellt darueber - wobei `test_code128.c` `symbol->height` durchaus prueft
(`test_encode`, eine Assertion): auch hier wurde der Fall portiert und die
Assertion weggelassen.

| C-Funktion | Status | Fehlt im Port |
|---|---|---|
| `zint_code128` (`code128.c:396`) | partial | `EXTRA_ESCAPE_MODE`. C deutet `\^C` als manuellen Code-Set-Wechsel; der Port kennt das Flag nicht (`EXTRA_ESCAPE_MODE` kommt in `zint_code128.pas` nicht vor) und verarbeitet die Zeichen als normale Daten |
| `zint_gs1_128_cc` (`code128.c:622-645`) | partial | Hoehenlogik, siehe [library.md](library.md) |
| `zint_dpd` (`code128_based.c`) | **missing** | kein Pascal-Gegenstueck. DPD-Symbologie ist nicht portiert |
| `zint_upu_s10` (`code128_based.c`) | **missing** | kein Pascal-Gegenstueck. UPU S10 ist nicht portiert |
| `nve18_or_ean14` (`code128_based.c:83`) | partial | `GS1PARENS_MODE` wird nicht ausgewertet. C waehlt den Praefix modusabhaengig (`prefix[idx][!(input_mode & GS1PARENS_MODE)]`): `(01)`/`(00)` mit Flag, sonst `[01]`/`[00]`. `zint_code128.pas:739-744` baut immer die Klammerform. Das ist nicht kosmetisch - `gs1_verify` wertet `GS1PARENS_MODE` aus (`zint_gs1.pas:71`), im Parens-Modus ist `[` kein AI-Trenner mehr. Die Konstante kommt im Port ueberhaupt nur in Testunits vor |

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

