(*
  Gemeinsame Compilerschalter fuer alle Testunits.

  Unter Free Pascal:
    - objfpc/H+ entspricht dem Modus der Barcode-Units.
    - prefixedattributes aktiviert Custom Attributes in Klammer-Schreibweise.
      Ohne diesen Schalter scheitert FPC schon am ersten [TestFixture].
      Der Schalter existiert erst ab FPC 3.3.1; FPC 3.2.2 kann die Testunits
      daher nicht uebersetzen und dient nur als Core-Compile-Gate.

  Unter Delphi hat die Datei keine Wirkung.

  Einbinden mit der Include-Direktive direkt nach der unit-Zeile.

  Hinweis: Dieser Kommentar nutzt bewusst die Klammer-Stern-Form. Eine
  Include-Direktive innerhalb eines geschweiften Kommentars wuerde diesen
  vorzeitig beenden - Delphi bricht dann mit E2038 ab.
*)

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
  {$modeswitch prefixedattributes}
{$ENDIF}
