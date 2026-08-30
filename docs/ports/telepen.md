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

| C-Index | Erwartet (C) | Ist (Delphi) | Ursache | Naechster Schritt |
|---|---|---|---|---|
| _(keine)_ | | | | |

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

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_telepen.pas` | ✅ b3a3c0d | ✅ `Test_Telepen.pas` (48 Tests) | **Erstes portiertes Modul**
- `telepen.c` | `zint_telepen.pas` | b3a3c0d portiert + Tests gruen
