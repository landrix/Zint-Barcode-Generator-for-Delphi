program ZintTest;

uses
  Forms,
  uFormZintTest in 'uFormZintTest.pas' {Form46},
  uFrameBorderOptions in 'uFrameBorderOptions.pas' {FrameBorderOptions: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  {$IFDEF declared(TApplication.MainFormOnTaskbar)}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.CreateForm(TForm46, Form46);
  Application.Run;
end.
