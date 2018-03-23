unit zint_render_bmp;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  zint, zint_render_canvas, Graphics;

type

  { TZintRenderTargetBMP }

  TZintRenderTargetBMP = class(TZintRenderTargetCanvas)
  protected
    FBitmap : TBitmap;
    procedure SetBitmap(const Value: TBitmap); virtual;
    procedure Inflate(const ANewWidth, ANewHeight : Single); override;
  public
    procedure Render(ASymbol : TZintSymbol); override;

    property Bitmap : TBitmap read FBitmap write SetBitmap;
  end;

implementation

{ TZintRenderTargetBMP }

procedure TZintRenderTargetBMP.Inflate(const ANewWidth, ANewHeight: Single);
begin
  FBitmap.SetSize(Round(ANewWidth), Round(ANewHeight));
end;

procedure TZintRenderTargetBMP.Render(ASymbol: TZintSymbol);
begin
  FCanvas := FBitmap.Canvas;
  inherited;
end;

procedure TZintRenderTargetBMP.SetBitmap(const Value: TBitmap);
begin
  if Assigned(Value) then
  begin
    FWidthDesired := Value.Width;
    FHeightDesired := Value.Height
  end;
  FBitmap := Value;
end;

end.

