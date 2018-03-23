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
    lblError: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
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

uses zint, zint_lmf, zint_render_lmf, zint_render_bmp, zint_helper;

{ TformZintTest }

procedure TformZintTest.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := Low(ZintSymbologyInfos) to High(ZintSymbologyInfos) do
    comType.Items.AddObject(ZintSymbologyInfos[i].DisplayName, TObject(ZintSymbologyInfos[i].Symbology));

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

{$DEFINE RenderBMP}

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
  imgResult.Picture.Graphic := nil;
  lblError.Caption := '';

  symbol := TZintSymbol.Create(nil);
  symbol.SymbolType := zsEAN14; // TZintSymbology(comType.Items.Objects[comType.ItemIndex]);
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
      lblError.Caption := e.Message;
  end;

  symbol.Free;
end;

end.
