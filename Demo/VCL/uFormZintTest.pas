unit uFormZintTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, zint, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, uRTTIHelper, ComCtrls, TypInfo,
  uFrameBorderOptions, Vcl.Menus;

type
  TForm46 = class(TForm)
    imgResult: TImage;
    Panel1: TPanel;
    edData: TEdit;
    comType: TComboBox;
    lblError: TLabel;
    btPrint: TButton;
    cbHRT: TCheckBox;
    cbRAM: TComboBox;
    Label3: TLabel;
    edMHS: TEdit;
    Label4: TLabel;
    FontDialog1: TFontDialog;
    ButtonFont: TButton;
    btSVG: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Splitter1: TSplitter;
    SaveDialog1: TSaveDialog;
    PageControl2: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    fboMargin: TFrameBorderOptions;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    fboPadding: TFrameBorderOptions;
    fboTextSpacing: TFrameBorderOptions;
    fboBorder: TFrameBorderOptions;
    fboWhitespace: TFrameBorderOptions;
    comHAlign: TComboBox;
    Label1: TLabel;
    comVAlign: TComboBox;
    Label2: TLabel;
    pumPrint: TPopupMenu;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edMiMW: TEdit;
    Label8: TLabel;
    edMaMW: TEdit;
    edPrimary: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure edDataChange(Sender: TObject);
    procedure comTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btPrintClick(Sender: TObject);
    procedure ButtonFontClick(Sender: TObject);
    procedure btSVGClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSymbol : TZintSymbol; 
    procedure GenBarcode;
    function GenSymbol: TZintSymbol;
    procedure ProcessSymbol(ASymbol: TZintSymbol);
    procedure InitRenderTarget(ARenderTarget: TZintCustomRenderTarget);
  public
    { Public-Deklarationen }
  end;

var
  Form46: TForm46;

implementation

{$R *.dfm}

uses zint_render_wmf, zint_render_canvas, Printers, zint_render_svg,
  zint_helper, uFrameOptions;

procedure TForm46.btPrintClick(Sender: TObject);
var
  symbol : TZintSymbol;
  rt  : TZintRenderTargetCanvas;
begin
  lblError.Caption := '';

  symbol:=GenSymbol;
  try
    symbol.Encode(edData.Text);

    if Sender is TMenuItem then
      Printer.PrinterIndex:=TMenuItem(Sender).Tag;
    Printer.BeginDoc;
    rt:=TZintRenderTargetCanvas.Create(nil);
    rt.Canvas:=Printer.Canvas;
    rt.Font.Assign(ButtonFont.Font);
    rt.Font.Height:=Abs(rt.Font.Height);
    rt.XDesired:=(Printer.Canvas.ClipRect.Right - Printer.Canvas.ClipRect.Left) / 3;
    rt.YDesired:=(Printer.Canvas.ClipRect.Bottom - Printer.Canvas.ClipRect.Top) / 3;
    rt.WidthDesired:=(Printer.Canvas.ClipRect.Right - Printer.Canvas.ClipRect.Left) / 3;
    rt.HeightDesired:=(Printer.Canvas.ClipRect.Right - Printer.Canvas.ClipRect.Left) / 3;
    InitRenderTarget(rt);
    try
      Symbol.Render(rt);
    finally
      rt.Free;
      Printer.EndDoc;
    end;

  except
    on E : Exception do
      lblError.Caption := e.Message;
  end;
end;

procedure TForm46.btSVGClick(Sender: TObject);
var
  sl : TStringList;
  symbol : TZintSymbol;
  rt  : TZintRenderTargetSVG;
begin
  if SaveDialog1.Execute then
  begin
    sl:=TStringList.Create;
    try
      lblError.Caption := '';

      symbol:=GenSymbol;
      try
        symbol.Encode(edData.Text);

        rt:=TZintRenderTargetSVG.Create(sl);
        rt.ForegroundColor:='black';
        rt.BackgroundColor:='white';
        rt.Transparent:=false;
        rt.Font:=ButtonFont.Font.Name;
        rt.HexagonScale:=StrToFloatDef(edMHS.Text, 1);
        rt.FontHeight:=ButtonFont.Font.Height;
        rt.XDesired:=0;
        rt.YDesired:=0;
        rt.WidthDesired:=300;
        rt.HeightDesired:=300;
        InitRenderTarget(rt);
        try
          Symbol.Render(rt);
        finally
          rt.Free;
        end;

      except
        on E : Exception do
          lblError.Caption := e.Message;
      end;

      sl.SaveToFile(SaveDialog1.FileName);
    finally
      sl.free;
    end;
  end;
end;

procedure TForm46.ButtonFontClick(Sender: TObject);
begin
  if FontDialog1.Execute() then
    ButtonFont.Font.Assign(FontDialog1.Font);
  //ButtonFont.Font.Size:=14;
  GenBarcode;
end;

procedure TForm46.comTypeChange(Sender: TObject);
begin
  GenBarcode;
end;

procedure TForm46.edDataChange(Sender: TObject);
begin
  GenBarcode;
end;

procedure TForm46.FormCreate(Sender: TObject);
var
  i : Integer;
  mi : TMenuItem;
begin
  //ReportMemoryLeaksOnShutdown := true;

  for i := Low(ZintSymbologyInfos) to High(ZintSymbologyInfos) do
    comType.Items.AddObject(ZintSymbologyInfos[i].DisplayName, TObject(ZintSymbologyInfos[i].Symbology));

  comType.ItemIndex := 0;

  for i := 0 to Printer.Printers.Count - 1 do
  begin
    mi := TMenuItem.Create(pumPrint);
    mi.Caption := Printer.Printers[i];
    mi.OnClick := btPrintClick;
    mi.Tag := i;
    pumPrint.Items.Add(mi);
  end;

  FSymbol:=TZintSymbol.Create(nil);

  ProcessSymbol(FSymbol);

  fboMargin.OnChange:=edDataChange;
  fboPadding.OnChange:=edDataChange;
  fboTextSpacing.OnChange:=edDataChange;
  fboBorder.OnChange:=edDataChange;
  fboWhitespace.OnChange:=edDataChange;
end;

procedure TForm46.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
 // for i := PageControl1.PageCount-1 downto 0 do
 //   PageControl1.Pages[i].Free;
    
  FSymbol.Free;
end;

procedure TForm46.FormShow(Sender: TObject);
begin
  GenBarcode;
end;

function TForm46.GenSymbol: TZintSymbol;
begin
  Result := FSymbol;
  Result.Clear;
  Result.SymbolType := TZintSymbology(comType.Items.Objects[comType.ItemIndex]);
  Result.input_mode := UNICODE_MODE;
end;

procedure TForm46.ProcessSymbol(ASymbol: TZintSymbol);
var
  i: Integer;
  Properties : TStringList;
  Property_Value: Variant;
  ts : TTabSheet;
  f : TFrameOptions;
begin
  Properties:=TStringList.Create;
  try
    rttihGetPropertiesList(ASymbol, Properties, false, [tkClass]);

    for i := 0 to Properties.Count - 1 do
    begin
      Property_Value:=rttihGetPropertyValue(ASymbol, Properties[i]);
      
      if TObject({$IF declared(NativeInt)}NativeInt{$ELSE}Integer{$IFEND}(Property_Value)).InheritsFrom(TCustomZintSymbolOptions) then
      begin
        ts:=TTabSheet.Create(PageControl1);
        ts.PageControl:=PageControl1;
        ts.Caption:=Properties[i];
        f := TFrameOptions.Create(ts);
        f.Parent:=ts;
        f.Init(ASymbol, Properties[i]);
        ts.OnShow:=f.RefreshFrame;
        f.OnChange:=edDataChange;
      end;
    end;
  finally
    Properties.Free;
  end;
end;

procedure TForm46.InitRenderTarget(ARenderTarget: TZintCustomRenderTarget);
begin
  ARenderTarget.HexagonScale:=StrToFloatDef(edMHS.Text, 1);
  ARenderTarget.RenderAdjustMode:=TZintRenderAdjustMode(cbRAM.ItemIndex);
  ARenderTarget.ShowText:=cbHRT.Checked;
  fboMargin.UpdateRenderBox(ARenderTarget.Margin);
  fboPadding.UpdateRenderBox(ARenderTarget.Padding);
  fboTextSpacing.UpdateRenderBox(ARenderTarget.TextSpacing);
  fboBorder.UpdateRenderBox(ARenderTarget.Border);
  fboWhitespace.UpdateRenderBox(ARenderTarget.Whitespace);
  ARenderTarget.HAlign := TZintHAlign(comHAlign.ItemIndex);
  ARenderTarget.VAlign := TZintVAlign(comVAlign.ItemIndex);
  ARenderTarget.MinModuleWidth := StrToFloatDef(edMiMW.Text, 0);
  ARenderTarget.MaxModuleWidth := StrToFloatDef(edMaMW.Text, 0);
end;

procedure TForm46.GenBarcode;
var
  symbol : TZintSymbol;
  wmf : TMetafile;
  rt  : TZintRenderTargetWMF;
begin
  imgResult.Picture.Graphic := nil;
  lblError.Caption := '';

  symbol:=GenSymbol;
  try
    symbol.primary := StrToArrayOfChar(edPrimary.Text);
    symbol.Encode(edData.Text, true);

    wmf:=TMetafile.Create;
    wmf.SetSize(imgResult.Width, imgResult.Height);
    rt:=TZintRenderTargetWMF.Create(nil);
    rt.Metafile:=wmf;
    rt.ForegroundColor:=ColorBox1.Selected;
    rt.BackgroundColor:=ColorBox2.Selected;

    rt.Font.Assign(ButtonFont.Font);
    InitRenderTarget(rt);
    try
      Symbol.Render(rt);
      imgResult.Picture.Graphic:=wmf;
    finally
      rt.Free;
      wmf.Free;
    end;

  except
    on E : Exception do
      lblError.Caption := e.Message;
  end;
end;

end.
