program ZintTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uFormZintTest, zint_helper, zint_2of5, zint_gs1,
  zint_auspost, zint_code, zint_code128, zint_code16k,
  zint_code49, zint_pdf417, zint, zint_composite, zint_gridmtx, zint_reedsol,
  zint_gb2312, zint_large, zint_plessey, zint_common, zint_upcean, zint_aztec,
  zint_maxicode, zint_dmatrix, zint_qr, zint_medical, zint_code1, zint_render_canvas, zint_render_bmp, zint_telepen, 
zint_postal, zint_imail, zint_rss;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TformZintTest, formZintTest);
  Application.Run;
end.

