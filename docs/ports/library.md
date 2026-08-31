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

## Behoben im Review von chore/cdiff-harness (2026-08-31)

Zwei Stellen, die der Differenztest selbst nicht traf, sondern beide Reviewer
am Quelltext fanden:

- `ZBarcode_Encode_Segs` meldete bei leerer Segmentliste den symbologie-
  abhaengigen Text (778, 228 oder 779). C unterscheidet: diese Nummern gelten
  fuer ein vorhandenes, aber leeres erstes Segment; eine leere Liste ist
  `Error 205: No input data` (`library.c:1043`).
- `supports_eci` wich von C ab (`library.c:329`): `BARCODE_UPNQR` stand darin,
  `BARCODE_CODEONE` fehlte. Das entscheidet unter anderem zwischen Fehler 228
  und 778 bei leerer Eingabe. Jetzt deckungsgleich, soweit der Port die
  Symbologien kennt - `BARCODE_HANXIN` und `BARCODE_ULTRA` fehlen ihm ganz.

## Querschnittsdeltas (betreffen jedes Modul)

Sie stehen hier und nicht bei einem einzelnen Barcode, weil sie den gesamten
Port betreffen. Die ersten beiden stammen aus dem Codex-Review von
`port/telepen` am 2026-08-30, die beiden mittleren aus dem Review von
`chore/set-height` am selben Tag. Erledigt ist bisher nur der erste.

### `symbol.height` wurde nie gesetzt (erledigt 2026-08-30)

**Befund.** C setzt am Ende jedes Encoders eine Symbolhoehe, ueber
`z_set_height()` aus `common.c` - mit und ohne `COMPLIANT_HEIGHT` je einen
anderen Wert. Beispiel `telepen.c:133-138`: 32 mit `COMPLIANT_HEIGHT`, sonst
50. Der Port hatte kein Gegenstueck; nach `ZBarcode_Encode` blieb
`symbol.height` auf seinem Ausgangswert, einzige Ausnahme `zint_qr.pas`
(`:2653`, `:3006`), wo die Modulzahl eingetragen wird.

`zint.pas` enthielt zwar eine Summierung aus `row_height`, aber sie lief nie:
sie stand in `check_row_heights`, dessen Rumpf mit einem unbedingten `exit`
begann - der Rest war toter Code. Wer dort nach der Hoehenlogik suchte, fand
Code, der aussah, als taete er etwas. Die Prozedur ist entfernt.

**Warum es niemand gemerkt hat.** Die Modul-Testdateien pruefen `height` fast
nirgends - `test_telepen.c` enthaelt das Wort nicht ein einziges Mal. Die
eigentliche Pruefung steht upstream woanders: in `test_height` und
`test_height_per_row` in `test_vector.c`, einer Tabelle ueber alle
Symbologien, sowie in `test_set_height` in `test_common.c`, das die Funktion
selbst durchgeht. Beim Portieren der Modul-Testdateien kommt man an keiner der
drei vorbei.

**Behoben in `chore/set-height`.** Portiert sind `set_height` und `stripf` in
`zint_common.pas`, `usps_set_height` und `daft_set_height` in
`zint_postal.pas`, der Auffangzweig aus `library.c:1276` am Ende von
`ZBarcode_Encode` sowie die Aufrufstellen in 2of5, auspost, code, code128,
medical, postal und telepen.

Der Auffangzweig ist leicht zu uebersehen und deshalb eigens erwaehnt: einige
Symbologien rufen in C ueberhaupt kein `z_set_height` - `plessey.c` und die
C25-Varianten in `2of5.c` -, ihre Hoehe von 50 entsteht allein dort. `height` und `row_height` sind dabei von `Integer`
auf `Single` gewechselt - die konformen Postal-Hoehen 3.225/2.15 sind als
Integer nicht darstellbar.

Geprueft wird es durch `UnitTests/Test_Height.pas` (182 Faelle, erzeugt von
`scripts/gen-height-tests.ps1` aus `test_vector.c`) und
`TTestCommonCore.TestSetHeight` (14 Faelle aus `test_common.c`). Drei
Abweichungen sind dabei aufgefallen und behoben worden; die beiden
Genauigkeitsfallen sind in [common.md](common.md) beschrieben.

Nicht portiert ist die Hoehenlogik der Module, die insgesamt nicht portiert
sind (bc412, channel, codabar, codablock, dxfilmedge, mailmark, ultra) sowie
der Module mit Status `legacy` oder `partial`, deren Encoder noch nicht auf
`b3a3c0d` stehen: code16k, code49, composite, imail, maxicode, pdf417, rss,
upcean und `zint_dpd`/`zint_upu_s10` in `code128_based.c`. Sie steht dort
jeweils im Portierungsvorgang des Moduls an, nicht mehr als Querschnittsthema.

**Was bleibt.** Fuer jedes Modul gilt weiter: eine leere Delta-Tabelle heisst
"keine getesteten Deltas", nicht "keine Deltas".

### errtxt bekommt zwei Praefixe (offen)

C baut den Meldungstext in zwei Schritten: der Encoder schreibt `247: Height
not compliant with standards (too small)`, und `error_tag` (`library.c:251`)
stellt genau einmal `Warning ` bzw. `Error ` voran. Oeffentlich steht am Ende
`Warning 247: ...`.

Der Port macht beides doppelt. Die Module schreiben den vollen Text
(`'Warning 247: ...'`, `'Error 486: ...'`), und `error_tag` (`zint.pas`)
stellt zusaetzlich das aeltere `warning: ` bzw. `error: ` voran. Oeffentlich
steht damit:

```
warning: Warning 247: Height not compliant with standards (too small)
```

Das betrifft **jede** Meldung des Ports, nicht nur die Hoehenwarnungen, und
stammt aus dem Legacy-Stand. In den Tests ist es unsichtbar, weil
`TZintTestHelper.GetErrTxt` das aeussere Praefix abschneidet - ausdruecklich,
damit die portierten C-Erwartungswerte passen.

Die Aufloesung ist mechanisch, aber breit: die Module duerften nur noch
`247: ...` schreiben, und `error_tag` muesste `Warning `/`Error ` voranstellen
statt `warning: `/`error: `. Das beruehrt jeden Encoder und jede Testunit und
gehoert deshalb in einen eigenen Vorgang, nicht in einen Barcode-Branch.
Gefunden im Codex-Review von `chore/set-height` am 2026-08-30.

### `input_mode` verliert die oberen Bits (teilweise offen)

`ZBarcode_Encode` weist `symbol.input_mode` an vier Stellen vollstaendig neu
zu - einmal `base_mode` vor der Kodierung, dann in den Wiederholungspfaden
fuer PDF417, Data Matrix und ECI. Jede dieser Zuweisungen loescht alles
oberhalb der 3-Bit-Basis. C tut das nie; nur ein ungueltiger Basismodus wird
komplett auf `DATA_MODE` gesetzt (`library.c:977`).

Der Port faengt einzelne Flags danach wieder ein: `FAST_MODE` und
`GS1NOCHECK_MODE` seit laengerem, `HEIGHTPERROW_MODE` seit
`chore/set-height` an allen vier Stellen. Nicht wiederhergestellt werden
`ESCAPE_MODE`, `GS1PARENS_MODE`, `EXTRA_ESCAPE_MODE` und
`GS1SYNTAXENGINE_MODE`; `FAST_MODE` fehlt in den letzten drei Zuweisungen.

Praktische Folge heute: keine bekannte - die betroffenen Flags werden vor
diesen Stellen ausgewertet oder gehoeren zu Modulen, die ohnehin nicht auf
`b3a3c0d` stehen. Die saubere Loesung ist, die oberen Bits gar nicht erst zu
verwerfen, statt sie einzeln nachzureichen. Gefunden im Codex-Review von
`chore/set-height` am 2026-08-30.

### `symbol.text` ist Latin-1, in C UTF-8 (offen)

`zint.h:135` beschreibt `text` als UTF-8. Der Port schreibt die Quellbytes
unveraendert hinein: fuer das Eingabebyte `0xFF` steht in C `C3 BF`, im Port
`FF`.

Aufgefallen im Differenztest (`CODE128.highbyte`, `CODE128B.highbyte`), dort
als Ausnahme gefuehrt. Es betrifft jedes Modul, das eine HRT schreibt, und die
Aufloesung gehoert zur HRT-Behandlung des gesamten Ports - zusammen mit dem
fehlenden `text_length` unten.

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

