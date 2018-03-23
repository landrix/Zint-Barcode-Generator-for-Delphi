unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.ListBox, zint,
  FMX.Objects;

type
  TFormMain = class(TForm)
    imgResult: TImage;
    Panel1: TPanel;
    comType: TComboBox;
    edData: TEdit;
    comPrinter: TComboBox;
    btPrint: TButton;

    procedure comTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GenBarcode;
    procedure btPrintClick(Sender: TObject);
    procedure edDataChange(Sender: TObject);
    function  GenSymbol: TZintSymbol;
  public
    { Public-Deklarationen }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}


uses zint_render_fmx_bmp, zint_render_fmx_canvas, fmx.printer;

procedure TFormMain.btPrintClick(Sender: TObject);
var
  symbol : TZintSymbol;
  rt  : TZintCanvasRenderTarget;
begin

  symbol := GenSymbol;

  try
    symbol.Encode(UTF8Encode(edData.Text), true);

    Printer.ActivePrinter:=TPrinterDevice(comPrinter.Items.Objects[comPrinter.ItemIndex]);
    Printer.ActivePrinter.SelectDPI(1200, 1200);
    Printer.BeginDoc;
    rt:=TZintCanvasRenderTarget.Create(nil);
    rt.Canvas:=Printer.Canvas;
    rt.RenderAdjustMode:=ramScale;
    rt.XDesired:=Printer.Canvas.Width/3;
    rt.YDesired:=Printer.Canvas.Height/3;
    rt.WidthDesired:=Printer.Canvas.Width/3;
    rt.HeightDesired:=Printer.Canvas.Height/3;
    rt.ShowText:=True;
    try
      Symbol.Render(rt);
    finally
      rt.Free;
      Printer.EndDoc;
    end;

  except
  end;
  symbol.Free;
end;

procedure TFormMain.comTypeChange(Sender: TObject);
begin
  GenBarcode;
end;

procedure TFormMain.edDataChange(Sender: TObject);
begin
  genBarcode
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  for i := Low(ZintSymbologyInfos) to High(ZintSymbologyInfos) do
    comType.Items.AddObject(ZintSymbologyInfos[i].DisplayName, TObject(ZintSymbologyInfos[i].Symbology));

  comType.ItemIndex := 0;

  try
    for i := 0 to Printer.Count-1 do
      comPrinter.Items.AddObject(Printer.Printers[i].Title, Printer.Printers[i]);
    if comPrinter.Items.Count>0 then
      comPrinter.ItemIndex:=0;
  except
    on E: Exception do
      ShowMessage('Error at getting Printer list: '+E.Message);
  end;
end;

function  TFormMain.GenSymbol: TZintSymbol;
begin
  Result := TZintSymbol.Create(nil);
  Result.SymbolType := TZintSymbology(comType.Items.Objects[comType.ItemIndex]);
  Result.input_mode := UNICODE_MODE;
  Result.output_options:=Result.output_options or BARCODE_BOX;
  Result.border_width:=1;
  Result.whitespace_width:=10;
  Result.Encode(edData.Text, true);
end;

procedure TFormMain.GenBarcode;
var
  symbol : TZintSymbol;
  bmp : TBitmap;
  rt  : TZintBMPRenderTarget;
begin
  symbol := GenSymbol;

   bmp:=TBitmap.Create(round(imgResult.Width), round(imgResult.Height));
   rt:=TZintBMPRenderTarget.Create(nil);
   rt.Bitmap:=bmp;
   rt.RenderAdjustMode:=ramScale;
   rt.Font.Family:='Courier New';
   rt.ShowText:=True;
   Symbol.Render(rt);
   rt.Free;
   imgResult.Bitmap:=bmp;
   bmp.Free;

   symbol.Free;
end;

end.
