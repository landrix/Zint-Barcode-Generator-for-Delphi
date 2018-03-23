unit zint_render_lmf;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0
}

{$mode objfpc}{$H+}

interface

uses
  zint, zint_render_canvas, Graphics, zint_lmf;

type

  { TZintRenderTargetLMF }

  TZintRenderTargetLMF = class(TZintRenderTargetCanvas)
  private
    procedure SetMetafile(AValue: TlmfImage);
  protected
    FMetafile : TlmfImage;
    procedure Inflate(const ANewWidth, ANewHeight : Single); override;
    procedure CreateCanvas;
    procedure DrawText(const AParams: TZintDrawTextParams); override;
  public
    procedure Render(ASymbol : TZintSymbol); override;

    property Metafile : TlmfImage read FMetafile write SetMetafile;
  end;

implementation

uses
  Types, Classes;

{ TZintRenderTargetLMF }

procedure TZintRenderTargetLMF.SetMetafile(AValue: TlmfImage);
begin
  if Assigned(AValue) then
  begin
    WidthDesired := AValue.Width;
    HeightDesired := AValue.Height;
  end;
  FMetafile := AValue;
end;

procedure TZintRenderTargetLMF.Inflate(const ANewWidth, ANewHeight: Single);
begin
  if Assigned(FCanvas) then
    FCanvas.Free;

  FMetafile.Width := Round(ANewWidth);
  FMetafile.Height := Round(ANewHeight);

  CreateCanvas;
end;

procedure TZintRenderTargetLMF.CreateCanvas;
begin
  FCanvas := TlmfCanvas.Create(FMetafile);
  FCanvas.Font.Assign(FFont);
end;

procedure TZintRenderTargetLMF.DrawText(const AParams: TZintDrawTextParams);
var
  r : TRect;
  ts : TTextStyle;
  w : Integer;
begin
  //because the lmf ignores the Alignment, we have to align it manually
  r.Left := Round(AParams.X);
  r.Top := Round(AParams.Y);
  r.Right := Round(AParams.X + AParams.Width);
  r.Bottom := Round(AParams.Y + AParams.Height);
  FCanvas.Brush.Style := bsClear;
  ts.Layout := tlCenter;
  ts.SystemFont := False;
  w := FCanvas.TextWidth(AParams.Text);
  FCanvas.TextRect(r, r.Left + Round((AParams.Width - w) / 2), r.Top, AParams.Text, ts);
end;

procedure TZintRenderTargetLMF.Render(ASymbol: TZintSymbol);
begin
  CreateCanvas;
  try
    inherited;
  finally
    FCanvas.Free;
  end;
end;

end.
