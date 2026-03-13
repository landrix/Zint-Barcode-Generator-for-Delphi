program DUnitXGuiRunner;

{$APPTYPE GUI}

{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Vcl.Forms,
  DUnitX.TestFramework,
  DUnitX.Loggers.GUI.VCL,
  TestHelper_Zint,
  //Test_Code128,
  Test_Telepen,
  Test_Medical
  ;

begin
  // GUI-Runner fuer DUnitX-Tests
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
end.
