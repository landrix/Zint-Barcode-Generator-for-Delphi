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

### Schritt 6 - Review

Codex- und Fable-Review am lokalen Diff:

```powershell
git diff develop...HEAD
```

Review-Checkliste siehe Abschnitt 5.

### Schritt 7 - Merge nach develop

```powershell
git checkout develop
git merge --no-ff port/<barcode>
scripts\gate-all.ps1
git push
git branch -d port/<barcode>
```

Der Statuseintrag in `docs/ports/<modul>.md` wird im Feature-Branch gepflegt,
die Uebersicht in `docs/ports/README.md` **erst beim Merge** auf develop.

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
| Delphi fuehrt weniger Tests aus als FPC | Im Bestand stehen 895 `[Test]`-Methoden. FPC fuehrt 848 aus, also alle ausser den 47 in QR/PDF417. DUnitX meldet dagegen nur 827 gefundene Tests. Die Differenz von 68 Methoden ist **ungeklaert**: entweder zaehlt DUnitX anders, oder es fuehrt 68 vorhandene Tests stillschweigend nicht aus. Das ist vor der naechsten Portierungsrunde zu klaeren, weil davon abhaengt, ob der Delphi-Stand so gruen ist wie er aussieht. |

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

## 5) Review-Checkliste (Codex / Fable)

Vor dem Merge nach develop pruefen:

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

```
<modul>: <was wurde portiert/gefixt>

C-Referenz: backend/<modul>.c <funktion>, backend/tests/test_<modul>.c <block>
Tests:      Delphi <n>/<n> gruen, FPC <n>/<n> gruen
Deltas:     <offene Abweichungen, oder "keine">
```

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
