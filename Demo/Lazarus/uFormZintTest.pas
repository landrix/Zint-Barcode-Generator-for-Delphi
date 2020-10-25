unit uFormZintTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TformZintTest }

  TformZintTest = class(TForm)
    comType: TComboBox;
    edData: TEdit;
    edPrimary: TEdit;
    imgResult: TImage;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    procedure comTypeChange(Sender: TObject);
    procedure edDataChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure GenBarcode;
  public
    { public declarations }
  end;

var
  formZintTest: TformZintTest;

implementation

{$R *.lfm}

{$DEFINE RenderBMP}

uses zint, zint_lmf, {$IFDEF RenderBMP} zint_render_bmp, {$ELSE}zint_render_lmf, {$ENDIF}zint_helper;

{ TformZintTest }

procedure TformZintTest.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := Low(ZintSymbologyInfos) to High(ZintSymbologyInfos) do
    comType.Items.Add(ZintSymbologyInfos[i].DisplayName);

  comType.ItemIndex := 0;
end;

procedure TformZintTest.FormShow(Sender: TObject);
begin
  GenBarcode;
end;

procedure TformZintTest.comTypeChange(Sender: TObject);
begin
  GenBarcode;
end;

procedure TformZintTest.edDataChange(Sender: TObject);
begin
  GenBarcode;
end;

procedure TformZintTest.GenBarcode;
var
  symbol: TZintSymbol;
  {$IFDEF RenderBMP}
  rt: TZintRenderTargetBMP;
  img: TBitmap;
  {$ELSE}
  rt: TZintRenderTargetLMF;
  img: TlmfImage;
  {$ENDIF}
begin
  if comType.ItemIndex < 0 then
    exit;
  imgResult.Picture.Graphic := nil;
  Caption := '';

  symbol := TZintSymbol.Create(nil);
  symbol.SymbolType := ZintSymbologyInfos[comType.ItemIndex].Symbology;
  symbol.input_mode := UNICODE_MODE;
  symbol.primary := StrToArrayOfChar(edPrimary.Text);
  try
    symbol.Encode(edData.Text, True);
    {$IFDEF RenderBMP}
    img := TBitmap.Create;
    img.PixelFormat := pf8bit;
    img.SetSize(imgResult.Width, imgResult.Height);
    rt := TZintRenderTargetBMP.Create(nil);
    rt.Bitmap := img;
    {$ELSE}
    img := TlmfImage.Create;
    img.Width := imgResult.Width;
    img.Height := imgResult.Height;
    rt := TZintRenderTargetLMF.Create(nil);
    rt.Metafile := img;
    {$ENDIF}
    rt.Font.Height := 23;
    rt.Font.Name := 'Arial';
    rt.TextSpacing.left.TargetUnits := 10;
    rt.TextSpacing.Right.TargetUnits := 10;
    rt.Whitespace.Modules := 1;
    rt.Border.Modules := 0;
    //rt.MaxModuleWidth := 3;
    rt.HAlign := haCenter;
    rt.VAlign := vaCenter;
    try
      symbol.Render(rt);
      imgResult.Picture.Graphic := img;
    finally
      img.Free;
      rt.Free;
    end;
  except
    on E: Exception do
      Caption := e.Message;
  end;

  symbol.Free;
end;

end.
