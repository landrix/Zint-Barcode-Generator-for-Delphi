program DUnitXGuiRunner;

{$APPTYPE GUI}

{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Vcl.Forms,
  DUnitX.TestFramework,
  DUnitX.Loggers.GUI.VCL,
  TestHelper_Zint,
  Test_CommonCore,
  Test_Code16k,
  Test_Code49,
  Test_Code1,
  Test_DMatrix,
  //Test_Code128,
  Test_Telepen,
  Test_Medical,
  Test_Plessey,
  Test_PZN,
  Test_PDF417
  ,
  Test_Aztec
  ;

begin
  // GUI-Runner fuer DUnitX-Tests
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
end.
