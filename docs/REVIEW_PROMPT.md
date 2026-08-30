# Review-Auftrag (Vorlage)

Diese Vorlage geht **unveraendert an beide Reviewer** (Codex und Fable 5), damit
ihre Ergebnisse vergleichbar sind. Nur den Abschnitt "Schwerpunkte dieses Branches"
je Branch anpassen.

Aufrufe siehe [PORTING_WORKFLOW.md](PORTING_WORKFLOW.md), Schritt 6.

---

Führe einen Code-Review des Branches `<BRANCH>` gegen `develop` durch.

Repo: `d:\Projekte\src-Zint-Barcode-Generator-for-Delphi`
Diff: `git diff develop...HEAD`
Commits: `git log --oneline develop..HEAD`

## Kontext

Das Repo ist ein Delphi-Port der C-Bibliothek Zint (Barcode-Generator). Code und
Testsuite müssen unter **Delphi Studio 37.0 und Free Pascal 3.3.1** übersetzen und
dieselben Ergebnisse liefern. Verbindliche Regeln:
`.github/copilot-instructions.md` und `docs/PORTING_WORKFLOW.md` (Abschnitte 4 und 5).

C-Referenz: `Lib/zint-master-2026-03-13-b3a3c0d/backend/`.

Der Stand beider Gates ist `<GATE-ZAHLEN>`. **Grüne Gates sind kein Beleg für
Korrektheit** — im Projekt ist dokumentiert, dass DUnitX vier registrierte Fixtures
still überspringt (docs/PORTING_WORKFLOW.md, Abschnitt 3).

## Immer zu prüfen

1. **C-Parität.** Hat jeder Delphi-Fix eine benannte C-Referenz? Weicht das
   Verhalten unbegründet von `backend/<modul>.c` ab? Sind C-Indizes im Test
   sichtbar (`{ C#<n> }`)?
2. **Stille Verhaltensänderungen.** Wurden Fehlercodes, `errtxt` oder die
   Warnklassifikation geändert, ohne dass es dokumentiert ist? Wurde eine
   Assertion entfernt oder abgeschwächt, nur damit ein Test grün wird?
3. **Dual-Compiler-Fallen** (docs/PORTING_WORKFLOW.md, Abschnitt 4):
   `System.*`-Namespaces, `Char`-Breite (WideChar vs. AnsiChar), `Cardinal`-
   Zählschleifen, Delphi-`TStringHelper` in gemeinsam genutztem Code.
4. **Testabdeckung.** Deckt der Test wirklich ab, was er behauptet? Laufen alle
   deklarierten Testmethoden auch tatsächlich?
5. **Gates.** Kann eines der Gate-Skripte fälschlich grün melden?

## Schwerpunkte dieses Branches

<HIER JE BRANCH EINTRAGEN>

## Ausgabeformat

Pro Befund:
- `Datei:Zeile`
- Was konkret falsch ist (ein Satz)
- Konkretes Fehlerszenario: welche Eingabe/welcher Zustand führt zu welchem
  falschen Ergebnis
- Schweregrad: kritisch / wichtig / gering

Verifiziere jeden Befund am tatsächlichen Dateiinhalt — erfinde keine
Zeilennummern. **Keine Stilkritik, keine Umbenennungsvorschläge, kein Lob.**
Wenn du nichts Belastbares findest, sage das klar. Ändere keine Dateien.
