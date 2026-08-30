# qr

QR Code, MicroQR, rMQR, UPNQR

| | |
|---|---|
| **Status** | Teilport (b3a3c0d) mit dokumentierten Deltas |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/qr.c` |
| **C-Tests** | `.../backend/tests/test_qr.c` |
| **Delphi-Unit** | [zint_qr.pas](../../zint_qr.pas) |
| **Test-Unit** | [UnitTests/Test_QR.pas](../../UnitTests/Test_QR.pas) |
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

## Offene Deltas FPC vs Delphi

| Fall | Delphi | FPC | Ursache |
|---|---|---|---|
| `TTestRMQR` (9 Tests), `TTestUPNQR` (3), sowie 9 von 12 Methoden in `TTestQR` | im Gesamtlauf **nicht ausgefuehrt** | n/a (Unit noch nicht im FPC-Gate) | DUnitX ueberspringt sie stillschweigend trotz Registrierung und vollstaendiger RTTI. Von 30 Testmethoden der Unit laufen nur 9. Analyse: docs/PORTING_WORKFLOW.md, Abschnitt "DUnitX fuehrt vier registrierte Fixtures nicht aus". |
| **Gesamte Unit laeuft nicht im FPC-Gate** | 30 Tests gruen | nicht uebersetzbar | `Test_QR.pas` enthaelt 304 Zeichenliterale > `#$00FF` (z.B. `#$0416`) in typisierten Konstanten-Arrays. Unter FPC ist `String` eine AnsiString: *Unicodechar/string constants cannot be converted to ansi/shortstring at compile-time*. Loesung: betroffene Record-Felder von `String` auf `UnicodeString` umstellen (unter Delphi verhaltensgleich) samt Helper-Signaturen. Danach in `UnitTests/fpc/ZintTests.lpr` wieder eintragen. |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `qr.c` | `zint_qr.pas` | b3a3c0d portiert + Tests gruen (aktive RT-/Segment-API-Pfade auf 1:1-C-Paritaet; optional nur weitere Subset-Erweiterungen)
- - [x] symbol.eci Thai (ECI 13): QR-Auto-ECI in `zint_qr.pas` nutzt jetzt Single-Byte-ECI-Priorisierung (C#2/C#3 auf C-Paritaet).


### QR-Familie Paritaetsdeltas

Hinweis: Die QR-Familie ist aktuell voll gruen, enthaelt aber dokumentierte Delphi-vs-C-Paritaetsdeltas
(vor allem Warning-Klassifikation in einzelnen Unicode-Optimize-Faellen sowie symbol.eci-Faelle).

### QR RT-content / content_segs

Hinweis QR RT-content/content_segs:
- QR-RT/content_segs sind im aktiven Scope auf C-Paritaet (inkl. ECI-Guess, GS1-FNC1-Substitution und HIBC_QR-Pfad).
- `Test_QR_RT_FromC` C#0..C#20 ist vollstaendig gruen.
- In diesem Block bestehen aktuell keine offenen Deltas.


### QR-Familie: offene Paritaetsarbeiten

### QR-Familie: Noch offene Paritaetsarbeiten (trotz gruener Suite)

- [x] `content_segs`/RT-content fuer C#0..C#20 abgeschlossen: ECI-Guess via TEncoding Round-Trip, GS1 FNC1-Bytes, HIBC_QR Single-Population. (Session 7)
- [x] Segment-API paritaet: `ZBarcode_Encode_Segs` + Segment-Array-Durchreichung ist aktiv; Unicode-Mixed-ECI/Structured-Append-Fall (C#7), `test_qr_rt_segs` C#0..9, `test_qr_encode_segs` C#0..7, `test_microqr_rt` C#0..7 und `test_upnqr_rt` C#0..5 sind auf 1:1-C-Paritaet.
- [x] symbol.eci Thai (ECI 13): QR-Auto-ECI in `zint_qr.pas` nutzt jetzt Single-Byte-ECI-Priorisierung (C#2/C#3 auf C-Paritaet).
- [x] Warning ZWARN_NONCOMPLIANT: Kanji-Optimierung (C#4/C#5) und GS1+ECI170 (C#18) auf C-Paritaet.
- [x] Structured Append fuer QR API-seitig nachziehen: Basis-Validierung/GS1-Warnpfade und Segment-/Mixed-ECI-Fall in `Encode_Segs` sind auf C-Paritaet; die aktiv genutzten RT-/Segment-Tests sind 1:1 an C ausgerichtet.
- [x] Surrogate in `UnitTests/Test_QR.pas` fuer die aktiven RT-/Segmentpfade durch 1:1 C-Testfaelle ersetzt.
- [x] MicroQR/UPNQR-Input-/Encode-Subsets weiter auf breitere 1:1-C-Abdeckung ausgebaut (inkl. zusaetzlicher DATA_MODE- und Byte-Input-Faelle aus den C-Indizes); aktuell kein offener Kern-/API-Blocker in der QR-Familie.

