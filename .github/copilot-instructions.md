# Copilot Instructions - C nach Delphi Portierung (Zint)

Diese Datei definiert die **fachlichen** Arbeitsregeln fuer AI-Agents im Repository.
Branch-Modell, Gates und Merge-Regeln stehen in [../docs/PORTING_WORKFLOW.md](../docs/PORTING_WORKFLOW.md)
und sind ebenso verbindlich.
Ziel ist eine reproduzierbare, sichere und nachvollziehbare Portierung von Zint C nach Delphi inklusive vollstaendiger Testportierung.

## 1) Zielbild

- Delphi-Implementierung soll funktional moeglichst 1:1 zum C-Referenzstand passen.
- Der Code muss unter **Delphi Studio 37.0 und Free Pascal 3.3.1** uebersetzen und
  in beiden Faellen dieselben Testergebnisse liefern.
- Testbasis kommt primaer aus den offiziellen C-Testdateien.
- Aenderungen werden nur dann im Delphi-Code gemacht, wenn sie durch C-Referenz, Spezifikation oder Tests begruendet sind.
- Die Suite muss nach jeder inhaltlichen Aenderung in einem verifizierbaren Zustand bleiben.

## 2) Prioritaeten

1. Korrektheit und Spezifikationskonformitaet.
2. Paritaet zu C-Verhalten (Return-Codes, errtxt, Symbolgroesse, ECI/GS1-Verhalten).
3. Testabdeckung aus C-Daten.
4. Kleine, reviewbare Commits/Edits.
5. Dokumentation von bewusst verbleibenden Deltas.

## 3) Portierungsprinzipien (Code)

- Portiere logiknah, nicht stilnah: Delphi darf idiomatisch sein, Verhalten muss gleich bleiben.
- Keine stillen API-Aenderungen an oeffentlichen Typen/Feldern ohne Not.
- Keine pauschalen Refactorings waehrend fachlicher Portierungen.
- Keine "Fixes" nur fuer einzelne Testfaelle ohne C-Referenzbegruendung.
- Bei Unsicherheit erst C-Quelle lesen, dann Delphi aendern.

## 4) Daten- und Typkonventionen (kritisch)

- Delphi `Char` ist WideChar; C `char` ist 1 Byte.
- `text` in `TZintSymbol` als Byte-Daten behandeln, nicht als WideChar-String.
- `errtxt` als Char-Array behandeln, Nullterminierung beachten.
- UTF-8/ECI/Shift-JIS-Pfade immer explizit testen (Unicode, DATA_MODE, GS1_MODE).
- Grenzwerte nie raten: aus C-Defines oder C-Tests uebernehmen.
- **FPC:** `Char` ist dort `AnsiChar` (1 Byte), unter Delphi `WideChar` (2 Byte).
  `TArrayOfChar`, `errtxt` und `text` verhalten sich deshalb unterschiedlich.
- **FPC:** keine Delphi-Unit-Namespaces (`System.SysUtils` bricht). Immer `SysUtils`.
- **FPC:** keine `Cardinal`-Zaehlschleifen (`for i := 0 to n-1` mit `n: Cardinal` und
  `n = 0` laeuft auf 4294967295 Iterationen; auf ARM64 sofort `EBusError`).
  Details siehe [../docs/PORTING_WORKFLOW.md](../docs/PORTING_WORKFLOW.md) Abschnitt 4.

## 5) Testportierung aus C (verbindlicher Ablauf)

1. Relevante C-Testfunktion in `backend/tests/test_*.c` identifizieren.
2. C-Item-Array strukturgleich in Delphi-Record-Array uebernehmen.
3. Pro Delphi-Testfall den C-Index als `{ C#<Index> }` oder `Index` Feld nachvollziehbar abbilden.
4. Assertions in dieser Reihenfolge:
	 - Return-Code
	 - Fehlermeldung (`errtxt`) falls relevant
	 - Symbolgroesse (`rows`, `width`)
	 - Zusatzfelder (`eci`, `content_segs`, etc.)
5. Bei nicht portierter Core-API den Fall markieren und sauber begrenzen (keine stillen Weglassungen).

## 6) Erwartungswerte und Deltas

- Standardfall: Erwartung = C-Referenz.
- Falls Delphi bewusst abweicht (fehlende API/Feature), dann:
	- Delta im Test direkt kommentieren (kurz, praezise).
	- Delta zusaetzlich in `docs/ports/<modul>.md` festhalten (nicht im MIGRATION_PLAN).
	- Keine irrefuehrenden "gruenen" Werte ohne Delta-Hinweis.

Beispiele fuer typische Delta-Gruende:
- `content_segs` noch nicht C-paritaet.
- Warning-Klassifikation 3 vs 4 in einzelnen Unicode/ECI-Pfaden.
- Segment-ECI-Mischfaelle noch nicht vollstaendig portiert.

## 7) Strukturierte Arbeitsschritte pro Thema

Fuer jedes Portierungsthema in dieser Reihenfolge arbeiten:

1. C-Verhalten lokalisieren (Code + Tests).
2. Delphi-Ist-Verhalten reproduzierbar machen (Failing Test).
3. Minimalen Delphi-Fix implementieren.
4. Relevante Tests laufen lassen.
5. Full Suite laufen lassen.
6. Delta-Doku aktualisieren, falls Restabweichungen bleiben.

## 8) Was nicht gemacht werden soll

- Keine grossen Misch-PRs (fachlicher Fix + Stilumbau + Umbenennungen).
- Keine Aenderung von Testdaten ohne klare Begruendung.
- Keine Entfernung bestehender Assertions nur um Tests gruen zu machen.
- Keine stillen Verhaltensaenderungen an Fehlercodes oder Warnungen.
- Keine Vermischung von "surrogate tests" mit vollwertiger C-Paritaet ohne Kennzeichnung.

## 9) Build- und Testregeln

- Nach jeder relevanten Codeaenderung mindestens den betroffenen Testblock ausfuehren.
- Vor Abschluss **beide** Vollgates laufen lassen: Delphi (Win32) und FPC.
- Empfohlener Standardlauf:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\gate-all.ps1
```

- Einzeln:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\build-delphi-tests.ps1
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\build-fpc-tests.ps1
```

- QR-spezifisch zusaetzlich schnelle Gate-Ausfuehrung nutzen:

```bat
UnitTests\run_qr_regression.bat
```

## 10) Qualitaetskriterien fuer fertig portierte Module

Ein Modul gilt als "ported + tests gruen", wenn:

- Kernfunktionen gegen C-Testdaten abgedeckt sind.
- Return-Codes/errtxt/Dimensionen stabil sind.
- Keine bekannten Abstuerze/Heap-Probleme mehr offen sind.
- Modul und Tests unter Delphi **und** FPC gruen sind.
- Verbleibende Deltas explizit dokumentiert sind.

## 10a) Review-Pflicht vor dem Merge

Kein Merge nach `develop` ohne Review durch **Codex UND Fable 5**. Das gilt fuer
jeden Branch, auch fuer reine Infrastruktur- und Doku-Branches.

- Auftragsvorlage: [../docs/REVIEW_PROMPT.md](../docs/REVIEW_PROMPT.md)
- Ablauf und Aufrufe: [../docs/PORTING_WORKFLOW.md](../docs/PORTING_WORKFLOW.md), Schritt 6
- Beide Reviewer bekommen denselben Auftrag. Jeder Befund wird am Quelltext
  nachgeprueft, dann umgesetzt oder mit Begruendung verworfen; verworfene
  Befunde gehoeren in den Commit-Text.

## 10b) Funktionsinventar

- Tests aus C pruefen nur, was C prueft. Fehlender Port-Code faellt ihnen nicht
  auf - `z_set_height` fehlt vollstaendig und keine 881 Tests merken es.
- Deshalb: je C-Funktion eine Zeile in `docs/ports/_functions.tsv`, geprueft von
  `scripts/check-c-inventory.ps1` (laeuft in `gate-all.ps1` mit).
- `missing` und `partial` brauchen zusaetzlich einen Delta-Eintrag in
  `docs/ports/<modul>.md`. Ein Modul geht erst auf Status `done`, wenn sein
  Inventar vollstaendig ist.
- Details: [../docs/PORTING_WORKFLOW.md](../docs/PORTING_WORKFLOW.md),
  Abschnitt 3a.

## 11) Commit- und Aenderungsdisziplin

- Kleine, thematisch saubere Aenderungen.
- **Commit-Messages sind auf Englisch** - Betreff und Rumpf, ausnahmslos, auch
  bei Merge-, Doku- und Review-Commits. Details:
  [../docs/PORTING_WORKFLOW.md](../docs/PORTING_WORKFLOW.md), Abschnitt 6.
- Doku und Quelltextkommentare werden auf `develop` deutsch geschrieben - das
  ist die Arbeitssprache. **Vor jedem Merge nach `main` wird alles davon ins
  Englische uebersetzt**, siehe
  [../docs/PORTING_WORKFLOW.md](../docs/PORTING_WORKFLOW.md), Abschnitt 2a.
  Neuen Text daher so schreiben, dass er sich uebersetzen laesst: keine
  Wortspiele, keine Abkuerzungen, die nur auf Deutsch aufgehen.
- Commit/Change-Message soll enthalten:
	- Was wurde portiert/gefixt?
	- Welche C-Referenz (Datei/Funktion/Testblock)?
	- Welche Tests wurden ausgefuehrt?
	- Welche Deltas bleiben offen?

## 12) Kurzcheckliste fuer Agents

Vor dem Edit:
- C-Referenz gelesen?
- Betroffene Delphi-Stellen identifiziert?

Nach dem Edit:
- Relevante Tests gruen?
- Full Suite gruen?
- Deltas dokumentiert?

Wenn etwas unklar ist:
- Nicht raten.
- Erst C-Code und C-Tests nachlesen, dann entscheiden.

## 13) Prompt-Shortcuts (konventionell)

Hinweis:
- Dies sind keine technischen Slash-Commands, sondern feste Trigger-Phrasen.
- Wenn der User eine Trigger-Phrase verwendet, wird der zugehoerige Ablauf strikt befolgt.

### Shortcut: `port-check <modul>`

Ziel:
- C-vs-Delphi-Paritaet fuer ein Modul pruefen.

Ablauf:
1. C-Referenzdatei und C-Testdatei lokalisieren (`backend/*.c`, `backend/tests/test_*.c`).
2. Delphi-Modul und Delphi-Testdatei lokalisieren.
3. Gap-Analyse erstellen: `test_large`, `test_input`, `test_encode`, `test_hrt`, `test_fuzz`.
4. Fehlende Delphi-Tests ergaenzen (zuerst failend, dann fixen).
5. Relevanten Testblock + Full Win32 DUnitX laufen lassen.
6. Deltas in Tests und `docs/ports/<modul>.md` dokumentieren.

Pflicht-Output:
- "Gefundene Luecken"
- "Umgesetzte Aenderungen"
- "Testergebnis (relevant + full suite)"
- "Offene Deltas"

### Shortcut: `port-test <testdatei>`

Ziel:
- C-Testdaten strukturgleich in Delphi-Tests uebernehmen.

Ablauf:
1. C-Item-Array 1:1 als Delphi-Record-Array uebertragen.
2. C-Indizes sichtbar halten (`Index` oder `{ C#<Index> }`).
3. Assertions in Reihenfolge: `ret` -> `errtxt` -> `rows/width` -> Zusatzfelder.
4. Nicht-portierbare Faelle explizit markieren (mit Grund).

Pflicht-Output:
- "Portierte C-Indizes"
- "Bewusst ausgelassene C-Indizes + Grund"
- "Testergebnis"

### Shortcut: `fix-failing <fixture|testname>`

Ziel:
- Konkreten roten Test reproduzieren, Ursache finden, minimal fixen.

Ablauf:
1. Zieltest isoliert ausfuehren.
2. Fehlerbild mit C-Referenz gegenpruefen.
3. Minimalen Codefix implementieren (keine Misch-Refactorings).
4. Zieltest erneut, dann Full Suite.

Pflicht-Output:
- "Root Cause"
- "Minimal-Fix"
- "Regression-Status"

### Shortcut: `delta-doc <modul>`

Ziel:
- Bekannte Delphi-vs-C-Abweichungen sauber dokumentieren.

Ablauf:
1. Delta in Test direkt kommentieren (kurz, praezise).
2. Delta in `docs/ports/<modul>.md` nachziehen.
3. Formulierung ohne Beschoenigung: was fehlt, was ist bewusst anders, was ist naechster Schritt.

Pflicht-Output:
- "Dokumentierte Deltas"
- "Verbleibendes Risiko"

### Shortcut: `qr-gate`

Ziel:
- Schneller QR-Regressionscheck.

Ablauf:
1. `UnitTests\\run_qr_regression.bat` ausfuehren.
2. Bei Fehlern: erst QR-spezifische Ursache isolieren, dann Fix.
3. Danach Full Win32 DUnitX bestaetigen.

Pflicht-Output:
- "QR-Gate Ergebnis"
- "Falls rot: betroffene Tests + Fix"

### Shortcut: `full-gate`

Ziel:
- Vollstaendige Abnahme vor Abschluss.

Ablauf:
1. `powershell -NoProfile -ExecutionPolicy Bypass -File .\\scripts\\gate-all.ps1`
2. Ergebnis exakt berichten, **je Compiler getrennt**: Found/Passed/Failed/Errored.
3. Bei Fehlschlag kein Abschluss ohne transparente Restpunkte.

Pflicht-Output:
- "Full-Gate Ergebnis"
- "Blocker (falls vorhanden)"

### Shortcut: `ship-note <modul>`

Ziel:
- Standardisierte Change-Zusammenfassung fuer Commit/PR vorbereiten.

Pflichtinhalt:
1. Was wurde portiert/gefixt?
2. Welche C-Referenz (Datei/Funktion/Testblock)?
3. Welche Tests wurden ausgefuehrt?
4. Welche Deltas bleiben offen?
