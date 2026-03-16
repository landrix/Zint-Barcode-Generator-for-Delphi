# Copilot Instructions - C nach Delphi Portierung (Zint)

Diese Datei definiert verbindliche Arbeitsregeln fuer AI-Agents im Repository.
Ziel ist eine reproduzierbare, sichere und nachvollziehbare Portierung von Zint C nach Delphi inklusive vollstaendiger Testportierung.

## 1) Zielbild

- Delphi-Implementierung soll funktional moeglichst 1:1 zum C-Referenzstand passen.
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
	- Delta zusaetzlich in `MIGRATION_PLAN.md` oder README festhalten.
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
- Vor Abschluss immer Full Win32 DUnitX laufen lassen.
- Empfohlener Standardlauf:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\build-delphi-tests.ps1
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
- Verbleibende Deltas explizit dokumentiert sind.

## 11) Commit- und Aenderungsdisziplin

- Kleine, thematisch saubere Aenderungen.
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
