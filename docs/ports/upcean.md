# upcean

EAN-8, EAN-13, UPC-A, UPC-E

| | |
|---|---|
| **Status** | Legacy-Port, nicht gegen b3a3c0d verifiziert |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/upcean.c` |
| **C-Tests** | `.../backend/tests/test_upcean.c` |
| **Delphi-Unit** | [zint_upcean.pas](../../zint_upcean.pas) |
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

## Bewusste Verhaltensaenderung: isbn_check (2026-08-30)

`isbn_check()` und `isbn13_check()` benutzten `Cardinal` als Typ fuer Laufindex
und Summe. Im Zuge der FPC-Tauglichkeit wurden sie auf `Integer` umgestellt.
Das entspricht der C-Referenz (`backend/upcean.c`, `isbnx_check`: `int i, weight,
sum, check`) und behebt einen Ueberlauf, **aendert dabei aber das bisherige
Delphi-Verhalten in einem Randfall**:

- `is_sane('0123456789X', ...)` (Zeile 419) laesst `X` an **jeder** Position zu,
  `to_upper` macht auch `x` erreichbar. `ctoi('X')` liefert `-1`.
- Steht ein `X` an nicht-letzter Position, war `sum` vorher als `Cardinal`
  uebergelaufen. Beispiel `BARCODE_ISBNX` mit `X000000003`:
  alt `sum = $FFFFFFFF`, `check = 4294967295 mod 11 = 3`, Pruefziffer `'3'`
  stimmt mit dem letzten Zeichen ueberein -> **Eingabe wurde akzeptiert**.
- Neu: `sum = -1`, `check = -1`, `itoc(-1) = '6'` -> `ZERROR_INVALID_CHECK`
  ("Incorrect ISBN check").

Das neue Verhalten ist das richtige und C-konforme; das alte akzeptierte eine
ungueltige ISBN. Die Aenderung ist hier festgehalten, weil sie nicht still
bleiben darf (siehe copilot-instructions, Abschnitt 8).

Offen: In `isbn13_check` ist dieselbe Konstellation nicht erreichbar, weil das
978/979-Praefix die Summe positiv haelt. Ein Testfall fuer den ISBN-X-Randfall
fehlt noch - nachzuholen im Branch `port/upcean`.

Gefunden im Review von `chore/fpc-gate`.

## Offene Deltas Delphi vs C

| C-Index | Erwartet (C) | Ist (Delphi) | Ursache | Naechster Schritt |
|---|---|---|---|---|
| | | | | |

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| `BARCODE_EANX`, Eingabe `4012345678901` (13 Ziffern) | nicht gemessen | ret=5, errtxt `error: Invalid length input` | Noch nicht untersucht; ob Delphi hier abweicht, ist offen. Aufgefallen im FPC-Smoke-Test von `chore/fpc-gate` (2026-08-30). Modul ist Legacy-Port ohne Testabdeckung. |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `upcean.c` | `zint_upcean.pas` | Legacy-Port (nicht b3a3c0d-verifiziert)

