# Portierungs-Workflow

Verbindlicher Ablauf fuer die Portierung von Zint C (`b3a3c0d`, 2026-03-13) nach Delphi **und** Free Pascal.

Fachliche Regeln (C-Referenz lesen, Deltas dokumentieren, Commit-Disziplin) stehen in
[.github/copilot-instructions.md](../.github/copilot-instructions.md). Dieses Dokument regelt
**Branches, Gates und Merges**.

---

## 1) Branch-Modell

```
main                 Release-Linie. Legacy-Stand + README. Bekommt develop erst
                     bei einem bewussten Release-Schnitt, nicht laufend.
                     Alles, was nach main geht, ist englisch (Abschnitt 2a).

develop              Integrationsbranch der Portierung. Immer gruen in beiden
                     Gates. Wird nach origin gepusht.

port/<barcode>       Arbeitsbranch pro Barcode. Zweigt von develop ab, bleibt LOKAL.
chore/<thema>        Arbeitsbranch fuer Infrastruktur (Gates, Skripte, Doku).

origin/legacy-stable Unangetastet. Rueckfallpfad fuer Altnutzer.
```

**Sequenziell arbeiten: es ist immer nur ein Arbeitsbranch offen.**
Grund sind vier Dateien, die jeder Barcode-Port zwangslaeufig anfasst und die bei
paralleler Arbeit garantiert kollidieren:

| Datei | Warum geteilt |
|---|---|
| `zint.pas` | `BARCODE_*`-Konstante, Dispatch in `ZBarcode_Encode`, `gs1_compliant`/`supports_eci` |
| `UnitTests/DUnitXCmdTest.dpr` | `uses`-Liste |
| `UnitTests/DUnitXGuiRunner.dpr` + `.dproj` | dito, `.dproj` ist XML |
| `UnitTests/fpc/ZintTests.lpr` | `uses`-Liste des FPC-Runners |

Feature-Branches werden **nicht** gepusht. Review laeuft lokal am Diff, nur
`develop` und `main` gehen nach origin.

---

## 2) Ablauf pro Barcode

```powershell
# 0) Start immer von aktuellem develop
git checkout develop
git pull
git checkout -b port/<barcode>
```

### Schritt 1 - C-Referenz erfassen

- `Lib/zint-master-2026-03-13-b3a3c0d/backend/<modul>.c` lesen
- `.../backend/tests/test_<modul>.c` durchzaehlen: `test_large`, `test_input`,
  `test_encode`, `test_hrt`, `test_rt`, `test_fuzz`
- Ergebnis als C-Index-Liste in `docs/ports/<modul>.md` eintragen

### Schritt 2 - Tests zuerst (failing)

- Testfaelle strukturgleich uebernehmen, C-Index als `{ C#<n> }` sichtbar halten
- Assertions in dieser Reihenfolge: `ret` -> `errtxt` -> `rows`/`width` -> Zusatzfelder
- Testunit muss unter **beiden** Compilern uebersetzen (siehe Abschnitt 4)

### Schritt 3 - Delphi-Code portieren

- Minimal und logiknah. Keine Misch-Refactorings.
- Kein Fix ohne C-Referenz-Begruendung.

### Schritt 4 - Beide Gates gruen

```powershell
scripts\gate-all.ps1
```

### Schritt 5 - Registrierung als letzter Einzelcommit

Alle Aenderungen an den vier geteilten Dateien aus Abschnitt 1 kommen in **einen
separaten, minimalen Commit am Branch-Ende**. Das macht einen Merge-Konflikt trivial
aufloesbar, falls doch einmal parallel gearbeitet wurde.

### Schritt 6 - Review (Pflicht)

> **Kein Merge nach `develop` ohne Review durch Codex UND Fable 5.**
> Das gilt fuer jeden Branch, auch fuer reine Infrastruktur- und Doku-Branches.
> Beide Reviewer bekommen denselben Auftrag, damit ihre Ergebnisse vergleichbar
> sind; sie finden erfahrungsgemaess Unterschiedliches.

Auftrag aus [REVIEW_PROMPT.md](REVIEW_PROMPT.md) erstellen, den Abschnitt
"Schwerpunkte dieses Branches" ausfuellen, dann beide starten:

```powershell
# Codex (CLI), read-only:
Get-Content docs\REVIEW_PROMPT.md -Raw | codex exec --sandbox read-only -
```

Fable 5 laeuft als Agent mit demselben Auftragstext und dem Hinweis, keine
Dateien zu aendern.

Danach:

1. Jeden Befund am Quelltext **nachpruefen** - beide Reviewer irren gelegentlich
   und widersprechen sich mitunter.
2. Befunde umsetzen oder mit Begruendung verwerfen. Bewusst verworfene Befunde
   gehoeren in den Commit-Text.
3. Beide Gates erneut laufen lassen.

Review-Checkliste siehe Abschnitt 5.

### Schritt 7 - Merge nach develop

Voraussetzung: Schritt 6 ist erledigt und die Befunde sind abgearbeitet.

```powershell
git checkout develop
git merge --no-ff port/<barcode>
scripts\gate-all.ps1          # develop muss NACH dem Merge gruen sein
git push
git branch -d port/<barcode>
```

Im Merge-Commit festhalten: Gate-Zahlen, offene Deltas und dass beide Reviews
gelaufen sind.

Der Statuseintrag in `docs/ports/<modul>.md` wird im Feature-Branch gepflegt,
die Uebersicht in `docs/ports/README.md` **erst beim Merge** auf develop.

---

## 2a) Release: Merge develop nach main

`main` ist die oeffentliche Release-Linie. Waehrend der Portierung wird auf
`develop` bewusst deutsch dokumentiert - das ist die Arbeitssprache. Was nach
`main` geht, ist dagegen fuer Fremde bestimmt.

> **Vor jedem Merge nach `main` werden alle deutschsprachigen Inhalte ins
> Englische uebersetzt.** Kein Release-Schnitt mit deutschem Text im Baum.

Betroffen ist alles Versionierte ausser der C-Referenz und dem Submodul:

| Was | Beispiele |
|---|---|
| Markdown | `README.md`, `AGENTS.md`, `docs/**/*.md`, `.github/*.md` |
| Quelltextkommentare | `*.pas`, `*.inc`, `*.dpr` - auch Alt-Kommentare, die nicht aus der Portierung stammen (z.B. `zint_qr_epc.pas`) |
| Skripte | `scripts/*.ps1`, Kommentare **und** ausgegebene Meldungen |
| Testtexte | Assertion-Meldungen, die in einem Fehlerreport landen |

Ausgenommen: `Lib/` (C-Referenz und DUnitX-Submodul, fremder Code) sowie die
Commit-Historie - Messages sind seit Abschnitt 6 ohnehin englisch und werden
nicht rueckwirkend angefasst.

**Nicht jeder Treffer ist eine Uebersetzungsaufgabe.** Deutsche Zeichen koennen
Nutzdaten sein und muessen dann Byte fuer Byte unveraendert bleiben. Der Fall im
Projekt ist `zint_qr_epc.pas:247`:

```pascal
if CharInSet(AValue[i],['Ä', 'ä', 'Ö', 'ö', 'Ü', 'ü', 'ß', '&']) then
```

Das sind die sieben Zeichen, die EPC-QR zusaetzlich erlaubt, in cp1252 kodiert -
keine Sprache, sondern eine Zeichenmenge. Wer sie beim Uebersetzen durch `ae`,
`oe`, `ue` ersetzt, aendert die Eingabevalidierung. Dasselbe gilt fuer
Testeingaben, deren deutscher Text Teil des geprueften C-Falls ist.

Solche Stellen kommen mit Begruendung nach `scripts/check-english-ignore.txt`,
sie werden nicht uebersetzt.

Das Pruefskript besteht aus drei Dateien: `check-english.ps1` (Logik),
`check-english-words.txt` (die gesuchten deutschen Woerter) und
`check-english-ignore.txt` (begruendete Ausnahmen). Es prueft sich selbst mit;
ausgenommen ist nur die Wortliste, die naturgemaess aus deutschen Woertern
besteht. Ein gruener Lauf belegt nicht, dass alles englisch ist - die Wortliste
kann nicht vollstaendig sein. Er belegt, dass keine ganze Datei vergessen wurde.

Ablauf:

```powershell
scripts\check-english.ps1     # meldet verbliebene deutsche Stellen
# ... uebersetzen, bis das Skript nichts mehr meldet ...
scripts\gate-all.ps1          # Uebersetzen darf nichts kaputtgemacht haben
git checkout main
git merge --no-ff develop
scripts\gate-all.ps1          # main muss NACH dem Merge gruen sein
git push
```

Das Uebersetzen ist eine eigene, reviewpflichtige Aenderung auf `develop` (oder
einem `chore/`-Branch), kein Nebenschauplatz des Merges - es fasst praktisch
jede Datei im Projekt an. Dabei gilt dasselbe wie beim Portieren: der Inhalt
bleibt, insbesondere Zahlenwerte, C-Indizes und Dateiverweise. Eine Uebersetzung
ist kein Anlass, eine Aussage zu glaetten, zu kuerzen oder zu beschoenigen.

---

## 3) Gates

Eine Aenderung ist erst fertig, wenn **beide** Gates gruen sind.

| Gate | Kommando | Compiler | Plattform |
|---|---|---|---|
| Delphi-Vollgate | `scripts\build-delphi-tests.ps1` | Delphi Studio 37.0 | Win32 Debug |
| FPC-Vollgate | `scripts\build-fpc-tests.ps1` | FPC 3.3.1 | aarch64-win64 |
| FPC-Compile-Gate (Linux) | `scripts\build-fpc-tests.ps1 -Wsl` | FPC 3.2.2 | aarch64-linux |
| Beides | `scripts\gate-all.ps1` | | |
| QR-Schnellcheck | `UnitTests\run_qr_regression.bat` | Delphi | Win32 |

### Mindest-Testzahl

Beide Vollgates kennen einen Parameter `-MinTests` (Delphi 881, FPC 849) und
schlagen fehl, wenn weniger Tests laufen - auch dann, wenn kein einzelner Test
rot ist. Ohne diese Schranke bleibt ein Gate gruen, wenn eine Fixture still aus
der Registrierung faellt; genau so blieb der `TTestQR`-Absturz unten lange unsichtbar.

**Wer Tests hinzufuegt, setzt die Zahl hoch.** Wer sie bewusst verringert,
begruendet das im Commit und setzt sie herunter.

Das Linux-Gate ist **nur ein Compile-Gate**: FPC 3.2.2 kennt den Modeswitch
`prefixedattributes` nicht und kann die Testfixtures daher nicht uebersetzen.
Es sichert Plattformunabhaengigkeit des Cores ab, nicht das Testverhalten.

### Bekannte Einschraenkungen der Gates

Stand 2026-08-30. Diese Punkte sind offen und sollen im jeweils passenden
Barcode-Branch abgearbeitet werden, nicht in der Infrastruktur.

| Punkt | Stand |
|---|---|
| `Test_QR` und `Test_PDF417` laufen nicht im FPC-Gate | 304 bzw. 48 Zeichenliterale > `#$00FF` lassen sich unter FPC nicht in `AnsiString` legen. Siehe `docs/ports/qr.md` und `docs/ports/pdf417.md`. |
| `Test_DMatrix` C#20 im FPC-Lauf uebersprungen | FPC liefert 14 statt 12 rows. Siehe `docs/ports/dmatrix.md`. |
| `TTestQR` und `TTestUPNQR` im Delphi-Gate stillgelegt | Geklaert, siehe naechster Abschnitt. `TTestQR` riss den Lauf ab und verschluckte rund 60 Tests; Ursache vermutlich Speicherkorruption im QR-Pfad, zustaendig ist `port/qr`. |

### GEKLAERT: DUnitX uebersprang vier Fixtures (2026-08-30)

Der frueher hier beschriebene Befund - vier registrierte Fixtures wurden nicht
ausgefuehrt - ist aufgeklaert. Es war **kein DUnitX-Problem**.

`TTestQR` bricht den Testlauf nach drei bis vier Methoden ab und reisst alles
mit, was in der DUnitX-Reihenfolge danach kommt: `TTestRMQR` (9 Tests),
`TTestUPNQR` (3) und die gesamte `Test_Telepen`-Suite (48). Danach
`EAccessViolation` und `Runtime error 216` - genau der Exitcode, den das
Delphi-Gate als "known post-run AV caveat" abgetan hat.

Eingegrenzt per binaerer Suche ueber die Testunits; die erste Haelfte aller
Units laeuft mit 563 Tests und 33 Fixtures problemlos, es ist also keine
Kapazitaetsgrenze. Ohne `TTestQR` laufen 66 statt 9 Tests.

Umgesetzt in `port/telepen`:

- `TTestQR` und `TTestUPNQR` stillgelegt (nur die Registrierung; Klassen,
  Testdaten und C-Erwartungen bleiben unveraendert). Details und
  Wiederinbetriebnahme: `docs/ports/qr.md`.
- Das Delphi-Gate behandelt einen Nonzero-Exitcode des Runners wieder als
  Fehler statt als Warnung. Seit der Stilllegung endet der Runner mit 0.

Ergebnis: **881 statt 827 Tests** unter Delphi, ohne verdeckten Absturz.

Offen bleibt die eigentliche Ursache in `TTestQR` - das Bild passt zu einer
Speicherkorruption im QR-Pfad, nicht zu einem Testfehler. Zustaendig ist
`port/qr`, siehe `docs/ports/qr.md`.

**Die Lehre daraus gilt weiter:** Eine gruene Suite belegt nichts, solange die
Zahl der gelaufenen Tests nicht mitgeprueft wird. Genau dafuer gibt es die
Mindest-Testzahl oben.

---

## 4) Dual-Compiler-Regeln (Delphi + FPC)

Diese Punkte sind der haeufigste Grund, warum Delphi-Code unter FPC bricht.

### 4.1 Keine Delphi-Unit-Namespaces

FPC kennt `System.SysUtils` nicht. Immer unqualifiziert schreiben:

```pascal
uses SysUtils, Math, Classes;          // richtig, beide Compiler
uses System.SysUtils, System.Math;     // FALSCH, bricht unter FPC
```

Windows-spezifisches immer kapseln:

```pascal
{$IFDEF MSWINDOWS}{$IFNDEF FPC}Winapi.Windows,{$ENDIF}{$ENDIF}
```

### 4.2 `Char` ist NICHT gleich breit

| | Delphi | FPC (`objfpc` / `H+`) |
|---|---|---|
| `SizeOf(Char)` | 2 (WideChar) | **1** (AnsiChar) |
| `String` | UnicodeString | AnsiString |

Konsequenz: `TArrayOfChar`, `errtxt` und `text` verhalten sich unterschiedlich.

- `text` in `TZintSymbol` **immer** als Byte-Daten behandeln, nie als Zeichenkette
- `errtxt` als Char-Array mit Nullterminierung, nie per `Length()` messen
- Bei Vergleichen und Konvertierungen `zint_helper` benutzen, nicht selbst casten

### 4.3 Vorzeichenlose Zaehlschleifen

```pascal
var n: Cardinal;
n := strlen(data);
for reader := 0 to n - 1 do ...   // FALSCH: n=0 ergibt 4294967295 Iterationen
```

Auf x86 laeuft das still ueber den Puffer, auf ARM64 gibt es sofort `EBusError`.
Zaehlvariablen fuer Schleifen **immer** `Integer`, oder vorher auf `n = 0` pruefen.

### 4.3a StrRepeat fuellt auf Laenge, es wiederholt nicht

`TZintTestHelper.StrRepeat(APattern, ATargetLen)` erzeugt einen String **der
Laenge ATargetLen**, indem es das Muster zyklisch einsetzt. Es wiederholt das
Muster nicht ATargetLen-mal.

```pascal
StrRepeat('0404', 34)    // 34 Zeichen  - NICHT 136
StrRepeat('0404', 136)   // 136 Zeichen - das ist gemeint
```

Das passt genau zur C-Konvention: die Testarrays dort fuehren `{ ..., "muster",
laenge, ... }`, wobei `laenge` ebenfalls die Ziellaenge ist. Bei einstelligen
Mustern faellt der Unterschied nicht auf, bei mehrstelligen schon - ein solcher
Fall lieferte in `Test_Telepen` einen Test, der statt 136 nur 34 Zeichen prueft
und dadurch trivial gruen war.

**Beim Uebernehmen eines C-Falls immer die Laenge aus der C-Zeile uebernehmen,
nicht die Zahl der Musterwiederholungen ausrechnen.**

### 4.4 Testunits

```pascal
uses
  {$IFDEF FPC}fpcunit, testregistry,{$ELSE}DUnitX.TestFramework,{$ENDIF}
  SysUtils, TestAssert_Zint, TestFixture_Zint, TestHelper_Zint, zint;

type
  [TestFixture]
  TTestFoo = class(TZintFixture)
  {$IFDEF FPC}published{$ELSE}public{$ENDIF}
    [Test] procedure Input_Something;
  end;
```

- `TZintFixture` (aus `TestFixture_Zint.pas`) erbt unter FPC von `TTestCase`,
  unter Delphi von `TObject`.
- Die `[Test]`-Attribute bleiben stehen. Unter FPC liefern Dummy-Attributklassen
  die Typen; FPC 3.3.1 parst und ignoriert sie.
- `published` sorgt dafuer, dass fpcunit die Methoden per RTTI findet - keine
  manuelle Registrierung noetig.
- Assertions **immer** ueber `ZAssert`, nie direkt ueber `Assert.` oder `AssertEquals`:

```pascal
ZAssert.AreEqual(expected, actual, 'C#12 ret');
ZAssert.IsTrue(cond, 'msg');
ZAssert.IsFalse(cond, 'msg');
```

---

## 5) Review-Checkliste (Codex / Fable 5)

Vor dem Merge nach develop pruefen:

- [ ] **Review durch Codex gelaufen** (Auftrag aus `docs/REVIEW_PROMPT.md`)
- [ ] **Review durch Fable 5 gelaufen** (derselbe Auftrag)
- [ ] Jeder Befund nachgeprueft, umgesetzt oder mit Begruendung verworfen
- [ ] Jeder Delphi-Fix hat eine benannte C-Referenz (Datei + Funktion + Testblock)
- [ ] C-Indizes im Test sichtbar (`{ C#<n> }`)
- [ ] Keine Assertion entfernt, nur um gruen zu werden
- [ ] Keine stillen Aenderungen an Fehlercodes oder Warnklassifikation
- [ ] Bewusst ausgelassene C-Indizes sind mit Grund markiert
- [ ] Verbleibende Deltas stehen in `docs/ports/<modul>.md`
- [ ] Keine `System.*`-Namespaces, keine `Cardinal`-Zaehlschleifen (Abschnitt 4)
- [ ] Beide Gates gruen, Zahlen im Commit genannt
- [ ] Registrierungs-Aenderungen liegen im letzten Einzelcommit

---

## 6) Commit-Konvention

**Commit-Messages sind auf Englisch.** Betreff und Rumpf, ausnahmslos - auch bei
Merge-, Doku- und Review-Commits. Der portierte Code und die C-Referenz sind
englisch, die Historie ist es damit auch. Die Projektdokumentation
(`docs/`, `AGENTS.md`, Quelltextkommentare) bleibt **auf `develop`** deutsch;
vor dem Merge nach `main` wird auch sie uebersetzt (Abschnitt 2a). Diese Regel
hier gilt unabhaengig davon fuer jede Commit-Message, auf jedem Branch.

```
<module>: <what was ported or fixed>

C reference: backend/<module>.c <function>, backend/tests/test_<module>.c <block>
Tests:       Delphi <n>/<n> green, FPC <n>/<n> green
Deltas:      <remaining differences, or "none">
```

Vor `git commit` pruefen: ist der Betreff englisch? Deutsche Messages werden nur
noch korrigiert, solange der Commit nicht gepusht ist.

---

## 7) Einmalige Einrichtung

```powershell
git clone --recurse-submodules https://github.com/landrix/Zint-Barcode-Generator-for-Delphi.git
# oder in bestehendem Clone:
git submodule update --init --recursive
```

Erwartete Werkzeuge:

| Werkzeug | Pfad / Version |
|---|---|
| Delphi Studio | `C:\Program Files (x86)\Embarcadero\Studio\37.0` |
| FPC (Windows) | `D:\bin\fpc\fpcupdeluxe\fpc\bin\aarch64-win64` - 3.3.1 |
| Lazarus | `D:\bin\fpc\fpcupdeluxe\lazarus` - 4.99 |
| FPC (WSL) | `/usr/bin/fpc` - 3.2.2, nur Compile-Gate |
| C-Referenz | `Lib/zint-master-2026-03-13-b3a3c0d/` (nicht versioniert) |
| DUnitX | `Lib/dunitx/` (Submodul) |
