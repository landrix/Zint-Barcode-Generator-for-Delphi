# Portierungsstand pro Modul

Eine Datei je Modul. So kollidieren parallele Aenderungen nicht mehr in einer
gemeinsamen Statusdatei. Ablauf und Regeln: [PORTING_WORKFLOW.md](../PORTING_WORKFLOW.md).

Diese Uebersicht wird **beim Merge auf develop** gepflegt, nicht im Feature-Branch.
Neu erzeugen mit `scripts\gen-port-index.ps1`.

Legende: OK = portiert (b3a3c0d) + Tests gruen | TEIL = Teilport mit dokumentierten
Deltas | ALT = Legacy-Port, nicht gegen b3a3c0d verifiziert | FEHLT = nicht portiert

## OK - portiert und getestet

| Modul | Beschreibung | Delphi-Unit | Tests |
|---|---|---|---|
| [2of5](2of5.md) | C25Standard/Inter/IATA/Logic/Ind, ITF14, DPLEIT, DPIDENT | `zint_2of5.pas` | `Test_2of5.pas` |
| [auspost](auspost.md) | Australia Post | `zint_auspost.pas` | `Test_Auspost.pas` |
| [code](code.md) | Code11, C39, EC39, LOGMARS, C93, VIN, HIBC_39 | `zint_code.pas` | `Test_Code.pas` |
| [code128](code128.md) | Code128, Code128B, GS1-128, EAN-14, NVE-18, HIBC-128 | `zint_code128.pas` | `Test_Code128.pas` |
| [medical](medical.md) | Pharmacode, Code32, PZN | `zint_medical.pas` | `Test_Medical.pas` |
| [plessey](plessey.md) | Plessey, MSI Plessey | `zint_plessey.pas` | `Test_Plessey.pas` |
| [postal](postal.md) | PostNet, Planet, RM4SCC, KIX, CEPNet, FIM | `zint_postal.pas` | `Test_Postal.pas` |
| [telepen](telepen.md) | Telepen, Telepen Numeric | `zint_telepen.pas` | `Test_Telepen.pas` |

## TEIL - Teilport mit dokumentierten Deltas

| Modul | Beschreibung | Delphi-Unit | Tests |
|---|---|---|---|
| [aztec](aztec.md) | Aztec Code + Aztec Runes | `zint_aztec.pas` | `Test_Aztec.pas` |
| [code1](code1.md) | Code One | `zint_code1.pas` | `Test_Code1.pas` |
| [code16k](code16k.md) | Code 16K | `zint_code16k.pas` | `Test_Code16k.pas` |
| [code49](code49.md) | Code 49 | `zint_code49.pas` | `Test_Code49.pas` |
| [common](common.md) | Gemeinsame Hilfsfunktionen | `zint_common.pas` | `Test_CommonCore.pas` |
| [composite](composite.md) | Composite-Symbole (CC-A/B/C) | `zint_composite.pas` | `Test_Composite.pas` |
| [dmatrix](dmatrix.md) | Data Matrix inkl. DMRE | `zint_dmatrix.pas` | `Test_DMatrix.pas` |
| [library](library.md) | Kern-API, Dispatch, TZintSymbol | `zint.pas` | `Test_CommonCore.pas` |
| [pdf417](pdf417.md) | PDF417, MicroPDF417 | `zint_pdf417.pas` | `Test_PDF417.pas` |
| [qr](qr.md) | QR Code, MicroQR, rMQR, UPNQR | `zint_qr.pas` | `Test_QR.pas` |

## ALT - Legacy-Port ohne b3a3c0d-Verifikation

| Modul | Beschreibung | Delphi-Unit | Tests |
|---|---|---|---|
| [dotcode](dotcode.md) | DotCode | `zint_dotcode.pas` | - |
| [gb2312](gb2312.md) | GB2312 Zeichensatz | `zint_gb2312.pas` | - |
| [gridmtx](gridmtx.md) | Grid Matrix | `zint_gridmtx.pas` | - |
| [gs1](gs1.md) | GS1-Validierung | `zint_gs1.pas` | - |
| [imail](imail.md) | Intelligent Mail | `zint_imail.pas` | - |
| [large](large.md) | Large-Number-Arithmetik | `zint_large.pas` | - |
| [maxicode](maxicode.md) | MaxiCode | `zint_maxicode.pas` | - |
| [reedsol](reedsol.md) | Reed-Solomon | `zint_reedsol.pas` | - |
| [rss](rss.md) | GS1 DataBar (RSS14, Limited, Expanded) | `zint_rss.pas` | - |
| [sjis](sjis.md) | Shift-JIS Zeichensatz | `zint_sjis.pas` | - |
| [upcean](upcean.md) | EAN-8, EAN-13, UPC-A, UPC-E | `zint_upcean.pas` | - |

## FEHLT - noch nicht portiert

| Modul | Beschreibung | Delphi-Unit | Tests |
|---|---|---|---|
| [bc412](bc412.md) | BC412 | - | - |
| [channel](channel.md) | Channel Code | - | - |
| [codabar](codabar.md) | Codabar (in C eigene Datei) | - | - |
| [codablock](codablock.md) | Codablock-F | - | - |
| [dxfilmedge](dxfilmedge.md) | DX Film Edge | - | - |
| [eci](eci.md) | Extended Channel Interpretation | - | - |
| [general_field](general_field.md) | GS1 General Field | - | - |
| [hanxin](hanxin.md) | Han Xin Code | - | - |
| [mailmark](mailmark.md) | Royal Mail Mailmark | - | - |
| [ultra](ultra.md) | Ultracode | - | - |

