## Hinweis für AI‑Agents

Verbindliche Projektregeln, in dieser Reihenfolge lesen:

1. **[docs/PORTING_WORKFLOW.md](docs/PORTING_WORKFLOW.md)** – Branch-Modell, Gates,
   Dual-Compiler-Regeln (Delphi **und** Free Pascal), Review- und Merge-Ablauf.
2. **[.github/copilot-instructions.md](.github/copilot-instructions.md)** – fachliche
   Arbeitsregeln der C→Pascal-Portierung. Wird von Copilot/Agenten automatisch erkannt.
3. **[docs/ports/README.md](docs/ports/README.md)** – aktueller Portierungsstand je Modul.
4. **[docs/MIGRATION_PLAN.md](docs/MIGRATION_PLAN.md)** – Gesamtstrategie und Phasen.

Die wichtigsten Regeln vorab:

- Es ist **immer nur ein Arbeitsbranch offen**. Feature-Branches zweigen von `develop`
  ab, bleiben lokal und werden nach dem Review mit `--no-ff` zurückgemergt.
- Eine Änderung ist erst fertig, wenn **beide** Gates grün sind:
  `scripts\gate-all.ps1` (Delphi Win32 + FPC).
- **Kein Merge nach `develop` ohne Review durch Codex UND Fable 5** – auch nicht
  für Infrastruktur- oder Doku-Branches. Auftragsvorlage:
  [docs/REVIEW_PROMPT.md](docs/REVIEW_PROMPT.md).
- **Commit-Messages sind auf Englisch** – ausnahmslos, auch Merge- und Doku-Commits.
- Doku und Quelltextkommentare sind auf `develop` deutsch. **Vor jedem Merge nach
  `main` wird alles Deutsche ins Englische übersetzt** – `main` ist die
  öffentliche Release-Linie und soll keinen deutschen Text tragen. Der heutige
  Stand von `main` erfüllt das noch nicht (Altbestand, u.a. `zint_qr_epc.pas`
  und die `.dproj`); die Regel greift beim ersten Release-Schnitt.
- Die **C-Referenz bleibt auf `b3a3c0d` festgenagelt**, sie wird waehrend der
  Portierung nicht auf einen neueren Upstream-Stand gezogen – Begruendung und
  Ablauf fuer den spaeteren Wechsel in PORTING_WORKFLOW Abschnitt 2b.
- Modulspezifischer Status und Deltas gehören nach `docs/ports/<modul>.md`,
  **nicht** in den MIGRATION_PLAN.

Allgemeine Agenten‑Doku (extern): https://agents.md/
