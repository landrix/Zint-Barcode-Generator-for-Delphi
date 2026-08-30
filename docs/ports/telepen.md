# telepen

Telepen, Telepen Numeric

| | |
|---|---|
| **Status** | Portiert (b3a3c0d) + Tests gruen |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/telepen.c` |
| **C-Tests** | `.../backend/tests/test_telepen.c` |
| **Delphi-Unit** | [zint_telepen.pas](../../zint_telepen.pas) |
| **Test-Unit** | [UnitTests/Test_Telepen.pas](../../UnitTests/Test_Telepen.pas) |
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
| **Alle 48 Tests laufen im Delphi-Gesamtlauf nicht** | 0 von 48 ausgefuehrt | 48 von 48 gruen | DUnitX ueberspringt die Fixtures `TTestTelepen` und `TTestTelepenNum` stillschweigend, obwohl sie registriert sind und vollstaendige RTTI haben. Isoliert (`scripts\isolate-win32-av.ps1 -Units Test_Telepen`) laufen alle 48 gruen. Vorbestehend, nicht durch die Dual-Gate-Umstellung verursacht. Analyse: docs/PORTING_WORKFLOW.md, Abschnitt "DUnitX fuehrt vier registrierte Fixtures nicht aus". |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_telepen.pas` | ✅ b3a3c0d | ✅ `Test_Telepen.pas` (48 Tests) | **Erstes portiertes Modul**
- `telepen.c` | `zint_telepen.pas` | b3a3c0d portiert + Tests gruen

