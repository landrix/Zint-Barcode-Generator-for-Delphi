unit zint_render_canvas;

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
  Classes, zint, Graphics;

type

  { TZintRenderTargetCanvas }

  TZintRenderTargetCanvas = class(TZintCustomRenderTarget)
  protected
    FCanvas : TCanvas;
    FFGColor: TColor;
    FBGColor: TColor;
    FFont: TFont;

    procedure OnFontChange(Sender : TObject);
    procedure SetCanvas(const Value: TCanvas); virtual;
    procedure SetFont(const Value: TFont); virtual;
    procedure ClearBackground(const AParams : TZintClearBackgroundParams); override;
    procedure DrawRect(const AParams : TZintDrawRectParams); override;
    procedure DrawHexagon(const AParams : TZintDrawHexagonParams); override;
    procedure DrawRing(const AParams : TZintDrawRingParams); override;
    procedure DrawText(const AParams: TZintDrawTextParams); override;
    function CalcTextHeight(const AParams : TZintCalcTextHeightParams) : Single; override;
    function CalcTextWidth(const AParams : TZintCalcTextWidthParams) : Single; override;
    procedure Inflate(const ANewWidth, ANewHeight : Single); override;
    procedure RenderStart; override;
    procedure SetColor(const Index: Integer; const Value: TColor); virtual;
  public
    constructor Create(AOwner : TPersistent); override;
    destructor Destroy; override;

    property Canvas : TCanvas read FCanvas write SetCanvas;
  published
    property ForegroundColor : TColor index 0 read FFGColor write SetColor default clBlack;
    property BackgroundColor : TColor index 1 read FBGColor write SetColor default clWhite;
    property Font: TFont read FFont write SetFont;
  end;

implementation

uses
  Types;

{ TZintRenderTargetCanvas }

procedure TZintRenderTargetCanvas.SetCanvas(const Value: TCanvas);
begin
  if Assigned(Value) then
  begin
    FXDesired := Value.ClipRect.Left;
    FYDesired := Value.ClipRect.Top;
    FWidthDesired:=Value.ClipRect.Right-Value.ClipRect.Left;
    FHeightDesired:=Value.ClipRect.Right-Value.ClipRect.Left;
  end;
  FCanvas := Value;
end;

procedure TZintRenderTargetCanvas.SetColor(const Index: Integer;
  const Value: TColor);
begin
  case Index of
    0 : FFGColor := Value;
    1 : FBGColor := Value;
  end;
  Changed;
end;

procedure TZintRenderTargetCanvas.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TZintRenderTargetCanvas.ClearBackground(
  const AParams: TZintClearBackgroundParams);
begin
  FCanvas.Brush.Color:=FBGColor;
  FCanvas.Brush.Style:=bsSolid;
  FCanvas.FillRect(Rect(Round(AParams.X),
                        Round(AParams.Y),
                        Round(AParams.X + AParams.Width),
                        Round(AParams.Y + AParams.Height)));
end;

procedure TZintRenderTargetCanvas.DrawRect(const AParams: TZintDrawRectParams);
begin
  FCanvas.Brush.Color:=FFGColor;
  FCanvas.Brush.Style:=bsSolid;
  FCanvas.FillRect(Rect(Round(AParams.X),
                        Round(AParams.Y),
                        Round(AParams.X + AParams.Width),
                        Round(AParams.Y + AParams.Height)));

end;

procedure TZintRenderTargetCanvas.DrawHexagon(const AParams: TZintDrawHexagonParams);
var
  hexagon_width, hexagon_height : Single;
  Points : array[0..5] of TPoint;
begin
  FCanvas.Pen.Style:=psClear;
  FCanvas.Brush.Style:=bsSolid;
  FCanvas.Brush.Color:=ffgcolor;

  hexagon_width:=AParams.Width*FHexagonScale;
  hexagon_height:=AParams.Height*FHexagonScale;

  Points[0] := Point(Round(AParams.X-(hexagon_width/2)), Round(AParams.Y - hexagon_height/4));
  Points[1] := Point(Round(AParams.X-(hexagon_width/2)), Round(AParams.Y + hexagon_height/4));
  Points[2] := Point(Round(AParams.X -(hexagon_width/2) + sqrt(3) * hexagon_height / 4), Round(AParams.Y + hexagon_height / 2));
  Points[3] := Point(Round(AParams.X -(hexagon_width/2) + sqrt(3) * hexagon_height / 2), Round(AParams.Y + hexagon_height / 4));
  Points[4] := Point(Round(AParams.X -(hexagon_width/2) + sqrt(3) * hexagon_height / 2), Round(AParams.Y - hexagon_height / 4));
  Points[5] := Point(Round(AParams.X -(hexagon_width/2) + sqrt(3) * hexagon_height / 4), Round(AParams.Y - hexagon_height / 2));

  {$IFDEF FPC}
  FCanvas.Polygon(@Points[0], Length(Points), true);
  {$ELSE}
  FCanvas.Polygon(Points);
  {$ENDIF}
end;

procedure TZintRenderTargetCanvas.DrawRing(const AParams: TZintDrawRingParams);
var
  LineWidth, HalfLineWidth : Integer;
begin
  LineWidth := Round(AParams.OuterRadius - AParams.InnerRadius);
  HalfLineWidth := Round((AParams.OuterRadius - AParams.InnerRadius) / 2);
  FCanvas.Brush.Style := bsClear;
  FCanvas.Pen.Width := LineWidth;
  FCanvas.Pen.Color := FFGColor;
  FCanvas.Pen.Style := psSolid;

  FCanvas.Ellipse(Round(AParams.x - AParams.OuterRadius + HalfLineWidth),
                  Round(AParams.y - AParams.OuterRadius + HalfLineWidth),
                  Round(AParams.x + AParams.OuterRadius - HalfLineWidth),
                  Round(AParams.y + AParams.OuterRadius - HalfLineWidth));
end;

procedure TZintRenderTargetCanvas.DrawText(const AParams: TZintDrawTextParams);
var
  r : TRect;
  txt : String;
  {$IFDEF FPC}
    ts : TTextStyle;
  {$ENDIF}
begin
  r.Left := Round(AParams.X);
  r.Top := Round(AParams.Y);
  r.Right := Round(AParams.X + AParams.Width);
  r.Bottom := Round(AParams.Y + AParams.Height);
  FCanvas.Brush.Style := bsClear;
  txt:=AParams.Text;
  {$IFDEF FPC}
    ts.Alignment := taCenter;
    ts.Layout := tlCenter;
    ts.SystemFont := False;
    FCanvas.TextRect(r, r.Left, r.Top, txt, ts);
  {$ELSE}
    FCanvas.TextRect(r, txt, [tfCenter, tfVerticalCenter]);
  {$ENDIF}
end;

procedure TZintRenderTargetCanvas.Inflate(const ANewWidth, ANewHeight: Single);
begin
  //a canvas can't inflate, but we have to prevent the abstract-error
end;

procedure TZintRenderTargetCanvas.OnFontChange(Sender: TObject);
begin
  Changed;
end;

function TZintRenderTargetCanvas.CalcTextHeight(const AParams : TZintCalcTextHeightParams): Single;
begin
  Result := FCanvas.TextHeight(AParams.Text);
end;

function TZintRenderTargetCanvas.CalcTextWidth(const AParams : TZintCalcTextWidthParams): Single;
begin
  Result := FCanvas.TextWidth(AParams.Text);
end;

procedure TZintRenderTargetCanvas.RenderStart;
begin
  FCanvas.Font.Assign(Font);
end;

constructor TZintRenderTargetCanvas.Create(AOwner : TPersistent);
begin
  FFont:=TFont.Create;
  FFont.Color:=clBlack;
  FFont.OnChange := {$IFDEF FPC}@{$ENDIF}OnFontChange;
  FFGColor:=clBlack;
  FBGColor:=clWhite;

  inherited;
end;

destructor TZintRenderTargetCanvas.Destroy;
begin
  FFont.Free;
  inherited;
end;


end.
