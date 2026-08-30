# library

Kern-API, Dispatch, TZintSymbol

| | |
|---|---|
| **Status** | Teilport (b3a3c0d) mit dokumentierten Deltas |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/library.c` |
| **C-Tests** | `.../backend/tests/test_library.c` |
| **Delphi-Unit** | [zint.pas](../../zint.pas) |
| **Test-Unit** | [UnitTests/Test_CommonCore.pas](../../UnitTests/Test_CommonCore.pas) |
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

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Querschnittsdeltas (betreffen jedes Modul)

Beide gefunden im Codex-Review von `port/telepen` am 2026-08-30. Sie stehen
hier und nicht bei einem einzelnen Barcode, weil sie den gesamten Port
betreffen.

### `symbol.height` wird nie gesetzt

C setzt am Ende jedes Encoders eine Symbolhoehe, ueber `z_set_height()` aus
`common.c` - mit und ohne `COMPLIANT_HEIGHT` je einen anderen Wert. Beispiel
`telepen.c:133-138`: 32 mit `COMPLIANT_HEIGHT`, sonst 50.

Der Delphi-Port hat kein Gegenstueck zu `z_set_height`. Nach `ZBarcode_Encode`
bleibt `symbol.height` auf seinem Ausgangswert; einzige Ausnahme ist
`zint_qr.pas:2653`/`:3006`, wo die Modulzahl eingetragen wird.

`zint.pas` enthaelt zwar eine Summierung aus `row_height` (`:2229`, `:2240`),
aber sie laeuft nie: sie steht in `check_row_heights` (`:2209`), dessen Rumpf
mit einem unbedingten `exit` beginnt (`:2216`) - der Rest ist toter Code. Die
Prozedur wird ohnehin nur aus `ZBarcode_Encode` bei Warnungen 1..5 gerufen
(`:3057`). Wer hier nach der Hoehenlogik sucht, findet also Code, der aussieht,
als taete er etwas.

Warum es bisher niemand gemerkt hat: **keine der portierten C-Testsuiten prueft
`height`** - `test_telepen.c` enthaelt das Wort nicht ein einziges Mal. Das
Delta ist damit real, aber von keinem Gate erfasst.

Naechster Schritt: `z_set_height` als eigenen Vorgang portieren, nicht
nebenbei in einem Barcode-Branch. Bis dahin gilt fuer jedes Modul: eine leere
Delta-Tabelle heisst "keine getesteten Deltas", nicht "keine Deltas".

### `TZintSymbol` hat kein `text_length`

Cs `zint_symbol` fuehrt `text` **und** `text_length`; die HRT kann eingebettete
NULs enthalten, weshalb die C-Tests durchweg `symbol->text_length` gegen einen
erwarteten Wert pruefen und `memcmp` statt `strcmp` verwenden.

`TZintSymbol.text` ist ein `TArrayOfByte` ohne Laengenfeld (`zint.pas:388`);
die Testhilfe liest bis zum ersten NUL (`ustrlen`). Alle HRT-Vergleiche im Port
sind daher nullterminiert.

Praktische Folge heute: keine. In allen bisher portierten HRT-Faellen setzt C
`expected_length` auf `-1`, also `strlen(expected)`, und keine erwartete HRT
enthaelt ein NUL - `telepen.c` ersetzt NUL in der HRT durch ein Leerzeichen.
Der nullterminierte Vergleich ist dort gleichwertig.

Sobald ein Modul portiert wird, dessen HRT ein NUL enthalten kann, ist das
Delta echt: Der Port kann die Laenge dann weder liefern noch pruefen.
`content_segs[].Length` gibt es dagegen bereits, dort wird die Laenge korrekt
verglichen (siehe `telepen.md`, HRT-Faelle mit `BARCODE_CONTENT_SEGS`).

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint.pas` | 🔧 Erweitert + Basis-Checks | ✅ `Test_CommonCore.pas` | +BARCODE_CEPNET=54, +ZWARN_NONCOMPLIANT=4, +BARCODE_VIN=73, Dispatch; Basischecks fuer `gs1_compliant/supports_eci` und Symbology-Mapping
- `library.c` + `zint.h` | `zint.pas` | erweitert, teilweise verifiziert

