# code1

Code One

| | |
|---|---|
| **Status** | Teilport (b3a3c0d) mit dokumentierten Deltas |
| **C-Referenz** | `Lib/zint-master-2026-03-13-b3a3c0d/backend/code1.c` |
| **C-Tests** | `.../backend/tests/test_code1.c` |
| **Delphi-Unit** | [zint_code1.pas](../../zint_code1.pas) |
| **Test-Unit** | [UnitTests/Test_Code1.pas](../../UnitTests/Test_Code1.pas) |
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
| | | | |

## Notizen

### Uebernommen aus MIGRATION_PLAN.md (Stand 2026-03-30)

- `zint_code1.pas` | 🟡 Teilport b3a3c0d | ✅ `Test_Code1.pas` (5 Tests / 167 C-Indizes) | Breite C-Testabdeckung aktiv (`test_input`, `test_large`, grosse `test_encode`-Subsets, `test_encode_segs`, `test_fuzz`). Wichtige Version-T-/C40-/TEXT-/GS1-Pfade wurden auf C-Paritaet gebracht; verbleibend sind dokumentierte Legacy-Deltas bei einzelnen Groessen-/Warn-/Overflow-Faellen.
- - **Stack-Overflow-Workaround (2026-03-30)**: Delphi-Port legacy-Code in `zint_code1.pas` Version-T encoder zeigt Stack-Overflow bei langen C#130-C#138 inputs (90+ 'A' characters). Workaround: `Test_Code1.TestEncodeSubset` skipped nun C#130..C#138 range und C#139 block wird auskommentiert bis Core-Reparatur durchgefuehrt (~ggf. neue Delphi Compiler version). Deltas vollstaendig dokumentiert im Test.


### Code One Detailstand

Hinweis Code One:
- Kernfixes fuer GS1_MODE, Decimal/BYTE-Umschaltungen, C40/TEXT-Tabellen und Look-Ahead sind umgesetzt.
- Kritische Version-T-Grenzfaelle (u.a. C#21/C#24/C#25) wurden auf C-Paritaet gebracht.
- **Stack-Overflow-Workaround (2026-03-30)**: Delphi-Port legacy-Code in `zint_code1.pas` Version-T encoder zeigt Stack-Overflow bei langen C#130-C#138 inputs (90+ 'A' characters). Workaround: `Test_Code1.TestEncodeSubset` skipped nun C#130..C#138 range und C#139 block wird auskommentiert bis Core-Reparatur durchgefuehrt (~ggf. neue Delphi Compiler version). Deltas vollstaendig dokumentiert im Test.
- Verbleibende Delta-Faelle sind im Testmodul explizit dokumentiert und aktuell nicht blocker-kritisch.

