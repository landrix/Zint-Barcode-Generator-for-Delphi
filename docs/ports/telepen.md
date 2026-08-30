# telepen

Telepen, Telepen Numeric

| | |
|---|---|
| **Status** | Portiert (b3a3c0d) + Tests gruen |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/telepen.c` |
| **C-Tests** | `.../backend/tests/test_telepen.c` |
| **Delphi-Unit** | [zint_telepen.pas](../../zint_telepen.pas) |
| **Test-Unit** | [UnitTests/Test_Telepen.pas](../../UnitTests/Test_Telepen.pas) |
| **Delphi-Gate** | gruen, 49 Tests |
| **FPC-Gate** | gruen, 49 Tests |

## Portierte C-Indizes

Vollstaendige Abdeckung: alle 49 Faelle aus `test_telepen.c` sind portiert.
Zu lesen als "jeder C-Fall hat eine Delphi-Entsprechung mit denselben Eingaben
und Erwartungswerten" - nicht als "jede einzelne C-Assertion ist nachgebildet".
Zwei Assertionsarten kann der Port strukturell nicht abbilden, beide ohne
praktische Folge fuer Telepen; sie stehen unter *Offene Deltas Delphi vs C*.

| C-Testblock | Indizes | Anzahl |
|---|---|---|
| `test_large` | C#0-C#3 | 4 |
| `test_hrt` | C#0-C#17 | 18 |
| `test_input` | C#0-C#8 | 9 |
| `test_encode` | C#0-C#9 | 10 |
| `test_fuzz` | C#0-C#7 | 8 |
| **Summe** | | **49** |

`test_generate_lens` ist ein Generator-Helfer der C-Testsuite und kein Testblock;
er wird nicht portiert.

Aufteilung auf die beiden Fixtures:

| Fixture | Faelle |
|---|---|
| `TTestTelepen` | `test_large` C#0-C#1, `test_hrt` C#0-C#9, `test_input` C#0-C#3, `test_encode` C#0-C#4, `test_fuzz` C#0-C#1 |
| `TTestTelepenNum` | `test_large` C#2-C#3, `test_hrt` C#10-C#17, `test_input` C#4-C#8, `test_encode` C#5-C#9, `test_fuzz` C#2-C#7 |

## Bewusst ausgelassene C-Indizes

| C-Index | Grund |
|---|---|
| _(keine)_ | |

## Offene Deltas Delphi vs C

Beide Eintraege sind Querschnittsthemen des Ports, nicht Telepen-spezifisch.
Gefunden im Codex-Review am 2026-08-30, ausfuehrlich in
[library.md](library.md), Abschnitt *Querschnittsdeltas*.

| C-Index | Erwartet (C) | Ist (Delphi) | Ursache | Naechster Schritt |
|---|---|---|---|---|
| alle (ungetestet) | `telepen.c:133-138` und `:215-218` setzen `symbol.height` auf 32 mit `COMPLIANT_HEIGHT`, sonst 50 | `height` bleibt 0; `zint_telepen.pas` hat keine Entsprechung | Der Port kennt kein `z_set_height`. `test_telepen.c` prueft `height` nirgends, deshalb faellt es in keinem Gate auf | Port-weit loesen, siehe [common.md](common.md) und [library.md](library.md) |
| `test_hrt` C#0-C#17 | C prueft `symbol->text_length` und vergleicht per `memcmp` | Vergleich ueber `GetText()`, also nullterminiert; `TZintSymbol` hat kein `text_length` | Strukturunterschied von `TZintSymbol` | Ohne Folge fuer Telepen: alle 18 Faelle setzen `expected_length = -1` (= `strlen`), und die Telepen-HRT ersetzt NUL durch ein Leerzeichen. Wird erst relevant, wenn ein Modul NUL in der HRT fuehrt |

## Portierungsluecken (Funktionsinventar)

Alle Eintraege stammen aus dem Funktionsinventar
(`docs/ports/_functions.tsv`), nicht aus einem Testlauf: C-Funktionen, zu denen
es im Port keine oder nur eine unvollstaendige Entsprechung gibt. Kein Test
faellt darueber: die zugehoerige C-Testsuite prueft `symbol->height` nirgends,
der blinde Fleck ist also aus C geerbt. (Fuer postal und code128 gilt das
nicht - dort wurden vorhandene C-Assertions beim Portieren weggelassen.)

| C-Funktion | Status | Fehlt im Port |
|---|---|---|
| `zint_telepen` (`telepen.c:133-138`) | partial | Hoehenlogik: 32 mit `COMPLIANT_HEIGHT`, sonst 50 |
| `zint_telepen_num` (`telepen.c:215-218`) | partial | dieselbe Hoehenlogik |

Das ist derselbe Sachverhalt wie in der Delta-Tabelle oben, hier nur auf
Funktionsebene. Portweite Beschreibung in [library.md](library.md).

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| _(keine)_ | | | |

## Notizen

### 2026-08-30 (Branch `port/telepen`)

**Die Suite lief unter Delphi ueberhaupt nicht** - und zwar nicht wegen eines
Fehlers in Telepen. `TTestQR` brach den Testlauf ab und riss alles mit, was in
der DUnitX-Reihenfolge danach kam: `TTestRMQR` (9 Tests), `TTestUPNQR` (3) und
die gesamte Test_Telepen-Suite (48). Anschliessend `EAccessViolation` und
`Runtime error 216`, den das Delphi-Gate als "known post-run AV caveat"
abgetan hat.

Eingegrenzt per binaerer Suche ueber die Testunits:

| Zusammenstellung | Ergebnis |
|---|---|
| nur `Test_Telepen` | laeuft, 48 Tests |
| `Test_Telepen` + `Test_Code128` | laeuft, 211 Tests |
| erste Haelfte aller Units (33 Fixtures) | laeuft, 563 Tests - **keine Kapazitaetsgrenze** |
| sobald `Test_QR` dabei ist | Telepen fehlt, `Test_QR` liefert nur 9 von 30 |
| ohne `TTestQR` | 66 statt 9 Tests, Telepen vollstaendig |

`TTestQR` und `TTestUPNQR` sind seither stillgelegt (nur die Registrierung;
Klassen, Testdaten und C-Erwartungen bleiben unveraendert). Beides ist
QR-Encoderarbeit, siehe [qr.md](qr.md). Seitdem endet der Testrunner mit
Exitcode 0, und das Delphi-Gate behandelt einen Nonzero-Exitcode wieder als
Fehler statt als Warnung.

**Ergaenzt:** `test_fuzz` C#6 (`BARCODE_TELEPEN_NUM`, 136 Neunen) war als
einziger C-Fall nicht portiert. Nachgetragen als
`TTestTelepenNum.Fuzz_136Nines_OK`; damit ist die Abdeckung vollstaendig.

### Review-Befunde (Fable 5, 2026-08-30)

Der Review hat die urspruengliche Behauptung "alle 49 Faelle portiert"
widerlegt und weitere Luecken gefunden. Alle sind behoben:

| Befund | Korrektur |
|---|---|
| `test_fuzz` C#3 prueft nur **34 statt 136 Zeichen**. `StrRepeat` fuellt auf Ziellaenge, wiederholt das Muster nicht N-mal. Der Test war damit trivial gruen und ein Pufferueberlauf bei maximaler gerader Eingabelaenge waere unentdeckt geblieben. | `StrRepeat('0404', 136)` |
| `test_input` C#3 uebergab ein Byte 233 statt der zwei UTF-8-Bytes `C3 A9` aus der C-Referenz. | Auf die C-Daten umgestellt |
| `test_fuzz` C#4 nutzte 137x `'1'` statt des C-Musters `1234567890...` und duplizierte damit `Large_MaxNum_Plus1_TooLong`. | Auf das C-Muster umgestellt |
| Die neun HRT-Faelle ohne `BARCODE_CONTENT_SEGS` liessen Cs `assert_null(content_segs)` weg. | `content_segs_count = 0` ergaenzt |
| In `Test_QR.pas` war versehentlich eine UTF-8-BOM entstanden (keine andere Testunit hat eine). Latente Falle, sobald `port/qr` die Unit ins FPC-Gate holt. | Entfernt |

Geprueft und ohne Befund: die zehn `test_encode`-Modulmuster sind byteidentisch
zu C, ebenso alle `content_segs`-Erwartungswerte inklusive Checkzeichen und
Laengen bei eingebetteten NULs.

Die uebrigen `StrRepeat`-Aufrufe im Projekt wurden gegengeprueft - die
Ziellaengen-Semantik entspricht der C-Konvention `{ "muster", laenge }`, der
Fehler war auf diesen einen Fall beschraenkt. Hinweis dazu jetzt in
`docs/PORTING_WORKFLOW.md` Abschnitt 4.3a.

### Rueckstand gegenueber Upstream (Stand 2026-08-30)

Nachgesehen nach dem Merge, weil die Frage aufkam, ob die C-Referenz mitgezogen
werden soll. `zint/zint` `master` ist 43 Commits und 223 Dateien vor unserem Pin
`b3a3c0d`; neuester Commit dort 2026-07-30. Derselbe Entwicklungszyklus, beide
Staende tragen `ZINT_VERSION_BUILD 9` (2.16.0 erschien 2025-12-19).

Telepen hat sich in diesen 4,5 Monaten erheblich veraendert. Zahlen direkt aus
`master/backend/tests/test_telepen.c` ausgezaehlt, nicht geschaetzt:

| Testblock | `b3a3c0d` (unser Pin) | `master` |
|---|---|---|
| `test_large` | 4 | 34 |
| `test_hrt` | 18 | 42 |
| `test_input` | 9 | 18 |
| `test_encode` | 10 | 15 |
| `test_fuzz` | 8 | 8 |
| **Summe** | **49** | **117** |

Inhaltlich in `master/backend/telepen.c`:

- **AIM-Modus ist neu** (22 Fundstellen `asc_comp_num`/`DLE`): DLE-Umschaltung
  fuer den Compressed-Numeric-Tail und alternative Start/Stop-Zeichen
  `0x82`/`0x83` statt `0x5F`/`0x7A`. Im Pin existiert davon nichts.
- **Die Hoehenlogik ist eine andere.** `master` rechnet
  `z_set_height(symbol, min_height, min_height > 32.0f ? min_height : 32.0f, 0, 0)`
  - dynamisches Minimum aus der Symbollaenge, und `no_errtxt = 0`, also mit
  moeglicher Warnung. Der oben dokumentierte Hoehendelta ("C setzt 32 bzw. 50")
  gilt weiterhin gegen `b3a3c0d`, beschreibt aber nicht mehr den aktuellen
  Upstream-Stand.

**Was das fuer die Indizes heisst:** vier der fuenf Bloecke haben zusaetzliche
Faelle bekommen. Jeder `C#<n>`-Verweis in `Test_Telepen.pas` ausser denen in
`test_fuzz` zeigt gegen `master` auf einen anderen Fall als gemeint - lautlos,
ohne dass ein Test rot wird. Das ist der Beleg fuer die Entscheidung in
PORTING_WORKFLOW Abschnitt 2b, den Pin nicht mitzuziehen.

Fuer den spaeteren `chore/c-reference-*`-Branch ist damit bekannt, was hier
ansteht: 68 zusaetzliche Faelle, der AIM-Modus und die neue Hoehenlogik.

### Review-Befunde (Codex, 2026-08-30)

Der zweite Reviewer hat 14 Befunde geliefert, davon vier zu Telepen selbst.
Alle nachgeprueft; die Telepen-Befunde sind umgesetzt, die uebrigen betreffen
`qr.md`, die Gate-Skripte und das neue `check-english.ps1`.

| Befund | Ergebnis |
|---|---|
| `height` wird nicht gesetzt (siehe Delta-Tabelle) | Bestaetigt am C-Quelltext. Nicht in diesem Branch behoben: der Port hat kein `z_set_height`, das ist Querschnittsarbeit. Als Delta dokumentiert - die Aussage "keine Deltas" war falsch |
| `text_length` wird nicht geprueft | Bestaetigt, aber ohne Wirkung fuer die 18 HRT-Faelle: C setzt dort durchweg `expected_length = -1`. Als Delta dokumentiert |
| `content_segs_count = 0` ist schwaecher als Cs `assert_null(content_segs)` | Umgesetzt. Der Zaehler und die Array-Laenge sind zwei Aussagen; `Length(sym.content_segs) = 0` steht jetzt in allen neun Faellen daneben |
| Bei Erfolg fehlte die Pruefung `errtxt` leer (Cs `assert_equal(errtxt[0] == '\0', ret == 0)`) | Umgesetzt in den acht erfolgreichen `test_large`- und `test_input`-Faellen |

Die Erweiterungen sind ohne Anpassung des Encoders gruen - der Port erfuellt
die schaerferen Assertionen bereits, sie waren nur nie geprueft.

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_telepen.pas` | ✅ b3a3c0d | ✅ `Test_Telepen.pas` (48 Tests) | **Erstes portiertes Modul**
- `telepen.c` | `zint_telepen.pas` | b3a3c0d portiert + Tests gruen
