# gs1

GS1-Validierung

| | |
|---|---|
| **Status** | Legacy-Port, nicht gegen b3a3c0d verifiziert |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/gs1.c` |
| **C-Tests** | `.../backend/tests/test_gs1.c` |
| **Delphi-Unit** | [zint_gs1.pas](../../zint_gs1.pas) |
| **Test-Unit** | _(keine)_ |
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

### Fehlernummern fehlen (offen)

`zint_gs1.pas` schreibt bei den meisten Meldungen keine C-Fehlernummer; von
den 14 Stellen tragen bisher drei eine. Der Differenztest fuehrt dafuer eine
Ausnahme (`EAN128.highbyte:errtxt` in `UnitTests/data/cdiff-ignore.txt`) und
wird weitere melden, sobald der Korpus mehr GS1-Faelle enthaelt.

Nachgetragen wurde nur 252 (`gs1.c:2204`), weil der Differenztest ihn traf.
C meldet an derselben Stelle 855 fuer Symbologien mit festem Seitenverhaeltnis
und laesst dort eine Digital-Link-URI zu; beides kennt der Port nicht.

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `gs1.c` | `zint_gs1.pas` | Legacy-Port (nicht b3a3c0d-verifiziert)

