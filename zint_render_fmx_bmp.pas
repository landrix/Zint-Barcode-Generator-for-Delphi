unit zint_render_fmx_bmp;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0
}

interface

uses
  zint, zint_render_fmx_canvas, FMX.Types, FMX.Graphics;

type
  TZintBMPRenderTarget = class(TZintCanvasRenderTarget)
  private
    FBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
  protected
    FBMP : TBitmap;

    procedure DrawStop; override;
    procedure Inflate(const ANewWidth, ANewHeight : Single); override;
  public
    property Bitmap: TBitmap read FBitmap write SetBitmap;
  end;

implementation

{ TZintBMPRenderTarget }


procedure TZintBMPRenderTarget.DrawStop;
begin
  inherited;
end;

procedure TZintBMPRenderTarget.Inflate(const ANewWidth, ANewHeight : Single);
begin
  FBMP.Height:=Round(ANewHeight);
  FBMP.Width:=Round(ANewWidth);
end;

procedure TZintBMPRenderTarget.SetBitmap(const Value: TBitmap);
begin
  FBitmap := Value;
  if Assigned(Value) then
  begin
    FWidthDesired:=Value.Width;
    FHeightDesired:=Value.Height;
    Canvas:=Value.Canvas;
  end;
end;

end.
