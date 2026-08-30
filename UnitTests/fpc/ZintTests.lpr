program ZintTests;

{
  Free-Pascal-Testrunner fuer den Zint-Delphi-Port.

  Gegenstueck zu UnitTests/DUnitXCmdTest.dpr. Beide Runner fuehren dieselben
  Testunits aus; die Framework-Unterschiede kapselt TestFramework_Zint.

  Bauen und ausfuehren: scripts\build-fpc-tests.ps1
  Regeln: docs/PORTING_WORKFLOW.md Abschnitt 4.4

  Beim Hinzufuegen einer Testunit muss sie hier UND in DUnitXCmdTest.dpr
  eingetragen werden (siehe PORTING_WORKFLOW.md Abschnitt 1, geteilte Dateien).
}

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, consoletestrunner,
  TestFramework_Zint,
  TestHelper_Zint,
  Test_2of5,
  Test_Auspost,
  Test_Aztec,
  Test_Code,
  Test_Code1,
  Test_Code128,
  Test_Code16k,
  Test_Code49,
  Test_CommonCore,
  Test_Composite,
  Test_DMatrix,
  Test_Height,
  Test_Medical,
  Test_PZN,
  Test_Plessey,
  Test_Postal,
  Test_Telepen;

{
  Noch nicht im FPC-Lauf:

    Test_QR      304 Zeichenliterale > #$00FF
    Test_PDF417   48 Zeichenliterale > #$00FF

  Beide Units tragen Testdaten wie #$0416 (kyrillisch) direkt als Zeichen-
  literal in typisierten Konstanten-Arrays. Unter Delphi ist String eine
  UnicodeString, unter FPC (objfpc/H+) eine AnsiString; FPC lehnt die
  Konvertierung zur Uebersetzungszeit ab:

    Error: Unicodechar/string constants cannot be converted to
           ansi/shortstring at compile-time

  Das ist kein Randfall, sondern der Kern des Char-Breiten-Unterschieds
  (docs/PORTING_WORKFLOW.md Abschnitt 4.2). Die Loesung gehoert in die
  jeweiligen Barcode-Branches port/qr und port/pdf417, nicht in dieses
  Infrastruktur-Ticket: die betroffenen Record-Felder muessen von String auf
  UnicodeString umgestellt werden (unter Delphi verhaltensgleich), samt der
  Helper-Signaturen, die sie entgegennehmen.

  Stand vermerkt in docs/ports/qr.md und docs/ports/pdf417.md.
}

type
  TZintTestRunner = class(TTestRunner)
  end;

var
  App: TZintTestRunner;

begin
  App := TZintTestRunner.Create(nil);
  try
    App.Initialize;
    App.Title := 'Zint Delphi Port - FPC Test Runner';
    App.Run;
  finally
    App.Free;
  end;
end.
